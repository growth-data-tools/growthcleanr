testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Wide output (output_format = "wide")
#
# Wide output returns the user's own subject rows -- every submitted column
# untouched, one row per input row -- with the requested growthcleanr columns
# slotted in immediately right of the value block column they describe
# (exclude_wt_1 after wt_1). Direct long -> wide: the input table is echoed and
# verdicts are scattered onto it by subject row, so rows cannot be lost,
# reordered, or rewritten and ragged blocks come back exactly as submitted.
#
# The keystone is the same equivalence chain used for semi-wide output:
#
#   r1 = cleangrowth(long)                              long -> long
#   r2 = cleangrowth(wide, output_format = "long")      wide -> long
#   r3 = cleangrowth(wide)                              wide -> wide (mirror)
#
#   exclude(r1) == exclude(r2) == melt_back(r3)
#
# .melt_back() is written here rather than reusing package code, so a bug in the
# output adapter cannot cancel itself out.
#
# Echo-don't-rebuild; wide is the direct long -> wide analogue.
# =============================================================================

# Hand-built wide fixture: 2 subjects, 3 blocks, ragged (A has 3 observations,
# B has 2), with a patient-level carry-through column.
.mk_wide <- function() {
  data.table(
    subjid    = c("A", "B"),
    sex       = c(0L, 1L),
    agedays_1 = c(100L, 50L),
    wt_1      = c(6.0, 4.0),
    ht_1      = c(60.0, 55.0),
    agedays_2 = c(200L, 120L),
    wt_2      = c(8.0, 5.5),
    ht_2      = c(68.0, 60.0),
    agedays_3 = c(300L, NA_integer_),
    wt_3      = c(9.5, NA_real_),
    ht_3      = c(74.0, NA_real_),
    clinic    = c("north", "south")
  )
}

.wide_pmap <- list(WEIGHTKG = "wt", HEIGHTCM = "ht")
.wt_blocks <- paste0("wt_", 1:3)
.ht_blocks <- paste0("ht_", 1:3)
.val_blocks <- c(.wt_blocks, .ht_blocks)

# Melt a wide table to canonical long in the adapter's documented emission order
# (subject first-appearance, then patient row, then block number, then weight ->
# height), minting id = 1:N so same-day tiebreaks match.
.wide_to_long <- function(w, pmap = .wide_pmap) {
  w <- as.data.table(copy(w))
  w[, .pt_row := .I]
  blocks <- 1:3
  pieces <- list()
  for (b in blocks) {
    for (i in seq_along(pmap)) {
      p   <- names(pmap)[i]
      col <- paste0(pmap[[i]], "_", b)
      if (!col %in% names(w)) next
      pieces[[length(pieces) + 1L]] <- data.table(
        .pt_row = w$.pt_row, subjid = w$subjid, sex = w$sex,
        agedays = as.integer(w[[paste0("agedays_", b)]]),
        param = p, measurement = as.numeric(w[[col]]),
        .block = b, .prank = i)
    }
  }
  lng <- rbindlist(pieces)
  lng <- lng[!is.na(measurement)]
  lng[, .subj_order := match(subjid, unique(w$subjid))]
  setorder(lng, .subj_order, .pt_row, .block, .prank)
  lng[, id := seq_len(.N)]
  lng[, c(".pt_row", ".block", ".prank", ".subj_order") := NULL]
  setcolorder(lng, c("subjid", "param", "agedays", "sex", "measurement", "id"))
  lng[]
}

# Wide output -> long exclude codes, keyed by subject row + value block column.
.melt_back <- function(w_out, value_cols) {
  d <- as.data.table(copy(w_out))
  d[, .pt_row := .I]
  out <- rbindlist(lapply(value_cols, function(v) {
    data.table(.pt_row = d$.pt_row, user_col = v,
               measurement = as.numeric(d[[v]]),
               exclude = as.character(d[[paste0("exclude_", v)]]))
  }))
  out <- out[!is.na(measurement)]
  setorder(out, .pt_row, user_col)
  out[]
}

.align <- function(res) {
  d <- as.data.table(res)
  d <- d[, .(subjid = as.character(subjid), param = as.character(param),
             agedays = as.integer(agedays), measurement = as.numeric(measurement),
             exclude = as.character(exclude))]
  setkey(d, subjid, param, agedays, measurement)
  d[]
}

# ---------------------------------------------------------------------------
# Test 1: KEYSTONE -- long -> long, wide -> long, and wide -> wide all agree
# ---------------------------------------------------------------------------
test_that("the equivalence chain closes: long == wide->long == melt_back(wide->wide)", {
  w   <- .mk_wide()
  lng <- .wide_to_long(w)

  r1 <- cleangrowth(lng, quietly = TRUE)
  r2 <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                     output_format = "long",
                                     wide_cols = .wide_pmap, quietly = TRUE))
  r3 <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                     wide_cols = .wide_pmap, quietly = TRUE))

  a <- .align(r1); b <- .align(r2)
  m <- merge(a, b, by = c("subjid", "param", "agedays", "measurement"),
             all = TRUE, suffixes = c("_1", "_2"))
  expect_equal(nrow(m), nrow(a))
  expect_identical(m$exclude_1, m$exclude_2)

  # wide -> wide melted back must reproduce wide -> long, row for row.
  mb <- .melt_back(r3, .val_blocks)
  b2 <- as.data.table(r2)[, .(param = as.character(param),
                              measurement = as.numeric(measurement),
                              exclude = as.character(exclude))]
  setkey(b2, param, measurement)
  # Map value block column -> gc parameter for comparison.
  mb[, param := ifelse(user_col %in% .wt_blocks, "WEIGHTKG", "HEIGHTCM")]
  mb2 <- mb[, .(param, measurement, exclude)]
  setkey(mb2, param, measurement)
  expect_identical(nrow(mb2), nrow(b2))
  expect_identical(mb2[order(param, measurement)]$exclude,
                   b2[order(param, measurement)]$exclude)
})

# ---------------------------------------------------------------------------
# Test 2: KEYSTONE on a real dataset. Blocks are built from distinct visit days
# (one block per (subjid, agedays)); same-day duplicates of a parameter can't
# share a wt/ht block, so they are deduped here and the SDE-as-blocks case is
# covered at scale by the weight-only test below and by test 7.
# ---------------------------------------------------------------------------
test_that("equivalence holds on syngrowth (WT+HT, one block per visit day)", {
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  sg <- sg[agedays < 20 * 365.25]
  sg <- sg[param %in% c("WEIGHTKG", "HEIGHTCM")]
  sg <- sg[subjid %in% unique(sg$subjid)[1:30]]

  sg <- sg[!duplicated(sg, by = c("subjid", "agedays", "param"))]
  vis <- unique(sg[, .(subjid, agedays)])
  setorder(vis, subjid, agedays)
  vis[, block := seq_len(.N), by = subjid]
  sg <- merge(sg, vis, by = c("subjid", "agedays"))

  ages <- dcast(vis, subjid ~ block, value.var = "agedays")
  setnames(ages, setdiff(names(ages), "subjid"),
           paste0("agedays_", setdiff(names(ages), "subjid")))
  sg[, .pcol := ifelse(param == "WEIGHTKG", "wt", "ht")]
  vals <- dcast(sg, subjid ~ .pcol + block, value.var = "measurement", sep = "_")
  sexes <- unique(sg[, .(subjid, sex)])
  w <- merge(merge(sexes, ages, by = "subjid"), vals, by = "subjid")

  r2 <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                     wide_cols = .wide_pmap, max_blocks = 1000, quietly = TRUE))
  r3 <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                     wide_cols = .wide_pmap, max_blocks = 1000, quietly = TRUE))

  vblocks <- grep("^(wt|ht)_[0-9]+$", names(w), value = TRUE)
  mb <- .melt_back(r3, vblocks)
  mb[, param := ifelse(grepl("^wt_", user_col), "WEIGHTKG", "HEIGHTCM")]
  ref <- as.data.table(r2)[, .(param = as.character(param),
                               measurement = as.numeric(measurement),
                               exclude = as.character(exclude))]
  cmp <- mb[, .(param, measurement = as.numeric(measurement), exclude)]
  setkey(cmp, param, measurement, exclude)
  setkey(ref, param, measurement, exclude)
  expect_identical(cmp[order(param, measurement, exclude)],
                   ref[order(param, measurement, exclude)])
})

# ---------------------------------------------------------------------------
# Test 2b: SDEs at scale -- weight-only, so same-day duplicate weights become
# consecutive blocks sharing an agedays (no wt/ht packing ambiguity). This is
# the case test 2 has to dedupe away, exercised here on real data at volume.
# ---------------------------------------------------------------------------
test_that("same-day duplicate weights round-trip through wide output as SDEs", {
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  sg <- sg[agedays < 20 * 365.25 & param == "WEIGHTKG"]
  sg <- sg[subjid %in% unique(sg$subjid)[1:40]]

  # Inject a same-day duplicate weight for a handful of subjects so there are
  # genuine SDEs to resolve (syngrowth alone is largely one-per-day).
  dup_subj <- unique(sg$subjid)[1:8]
  dups <- sg[subjid %in% dup_subj][, .SD[1], by = subjid]
  dups[, measurement := measurement * 1.4]   # implausible second same-day weight
  sg <- rbind(sg, dups)

  # One block per weight occurrence, ordered by (agedays, then the injected dup
  # last within a day) -> same-day duplicates are consecutive blocks, same age.
  setorder(sg, subjid, agedays, measurement)
  sg[, block := seq_len(.N), by = subjid]
  ages <- dcast(sg, subjid ~ block, value.var = "agedays")
  setnames(ages, setdiff(names(ages), "subjid"),
           paste0("agedays_", setdiff(names(ages), "subjid")))
  wts <- dcast(sg, subjid ~ block, value.var = "measurement")
  setnames(wts, setdiff(names(wts), "subjid"),
           paste0("wt_", setdiff(names(wts), "subjid")))
  sexes <- unique(sg[, .(subjid, sex)])
  w <- merge(merge(sexes, ages, by = "subjid"), wts, by = "subjid")

  r2 <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                     wide_cols = list(WEIGHTKG = "wt"),
                                     max_blocks = 1000, quietly = TRUE))
  r3 <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                     wide_cols = list(WEIGHTKG = "wt"),
                                     max_blocks = 1000, quietly = TRUE))

  # There really are SDEs to resolve.
  expect_true(sum(as.character(as.data.table(r2)$exclude) == "Exclude-Extraneous") >= 8L)

  # wide -> wide melted back reproduces wide -> long exactly.
  vblocks <- grep("^wt_[0-9]+$", names(w), value = TRUE)
  mb  <- .melt_back(r3, vblocks)
  cmp <- mb[, .(measurement = as.numeric(measurement), exclude)]
  ref <- as.data.table(r2)[, .(measurement = as.numeric(measurement),
                               exclude = as.character(exclude))]
  setkey(cmp, measurement, exclude); setkey(ref, measurement, exclude)
  expect_identical(cmp[order(measurement, exclude)], ref[order(measurement, exclude)])
})

# ---------------------------------------------------------------------------
# Test 3: structure / echo fidelity
# ---------------------------------------------------------------------------
test_that("wide output echoes the input table with verdicts slotted in", {
  w   <- .mk_wide()
  pre <- copy(w)
  r   <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                      wide_cols = .wide_pmap, quietly = TRUE))
  d   <- as.data.table(r)

  # One row per subject, in input order.
  expect_equal(nrow(d), nrow(w))
  expect_identical(as.character(d$subjid), c("A", "B"))
  # Every input column returns unchanged (values and type).
  for (cl in names(w)) expect_identical(d[[cl]], w[[cl]], info = cl)
  # A verdict column sits immediately right of each value block column.
  for (v in .val_blocks) {
    expect_true(paste0("exclude_", v) %in% names(d))
    expect_equal(which(names(d) == paste0("exclude_", v)),
                 which(names(d) == v) + 1L, info = v)
  }
  # Dropping the generated columns reproduces the input exactly.
  gen <- setdiff(names(d), names(w))
  expect_identical(d[, names(w), with = FALSE], w)
  # Input table not mutated.
  expect_identical(w, pre)
})

test_that("an empty ragged block gets an NA verdict", {
  w <- .mk_wide()
  r <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                    wide_cols = .wide_pmap, quietly = TRUE))
  d <- as.data.table(r)
  # Subject B has no block 3, so its block-3 verdicts are NA; subject A's are not.
  expect_true(is.na(d[subjid == "B"]$exclude_wt_3))
  expect_true(is.na(d[subjid == "B"]$exclude_ht_3))
  expect_false(is.na(d[subjid == "A"]$exclude_wt_3))
})

# ---------------------------------------------------------------------------
# Test 4: emit control -- default, added sets, per-block agedays, switch errors
# ---------------------------------------------------------------------------
test_that("default wide output emits exclude only", {
  w <- .mk_wide()
  r <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                    wide_cols = .wide_pmap, quietly = TRUE))
  nm <- names(r)
  expect_true("exclude_wt_1" %in% nm)
  # Long-format defaults (bin_exclude, unit_error_range) are NOT widened by default.
  expect_false(any(grepl("^bin_exclude_", nm)))
  expect_false(any(grepl("^unit_error_range_", nm)))
})

test_that("widened_emit widens the requested columns for wide output", {
  w <- .mk_wide()
  r <- suppressWarnings(cleangrowth(
    w, input_format = "wide", wide_cols = .wide_pmap,
    widened_emit = c("exclude", "bin_exclude"), quietly = TRUE))
  nm <- names(r)
  expect_true(all(c("exclude_wt_1", "bin_exclude_wt_1",
                    "exclude_ht_2", "bin_exclude_ht_2") %in% nm))
})

test_that("agedays widens per block, not per measurement", {
  w <- .mk_wide()
  r <- suppressWarnings(cleangrowth(
    w, input_format = "wide", wide_cols = .wide_pmap,
    widened_emit = c("exclude", "agedays"), quietly = TRUE))
  d <- as.data.table(r)
  # One gc_internal_agedays column per block present, not per value column.
  expect_true(all(c("gc_internal_agedays_1", "gc_internal_agedays_2",
                    "gc_internal_agedays_3") %in% names(d)))
  expect_false(any(grepl("gc_internal_agedays_wt", names(d))))
  # Values match what growthcleanr used.
  expect_equal(d[subjid == "A"]$gc_internal_agedays_1, 100L)
  expect_equal(d[subjid == "A"]$gc_internal_agedays_3, 300L)
  # Subject B has no block 3 -> NA.
  expect_true(is.na(d[subjid == "B"]$gc_internal_agedays_3))
})

test_that("a switch-gated emit column errors when its switch is off", {
  w <- .mk_wide()
  expect_error(
    suppressWarnings(cleangrowth(w, input_format = "wide", wide_cols = .wide_pmap,
                                 widened_emit = "exclude_detailed", quietly = TRUE)),
    "exclude_detail")
  # With the switch on it appears and is placed per measurement.
  r <- suppressWarnings(cleangrowth(w, input_format = "wide", wide_cols = .wide_pmap,
                                    widened_emit = c("exclude", "exclude_detailed"),
                                    exclude_detail = TRUE, quietly = TRUE))
  expect_true("exclude_detailed_wt_1" %in% names(r))
})

# ---------------------------------------------------------------------------
# Test 5: imperial / length values reported through gc_internal_meas
# ---------------------------------------------------------------------------
test_that("wide imperial values are echoed verbatim and reported in metric on request", {
  w <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wlb_1 = 6.0 * 2.2046226,
    agedays_2 = 200L, wlb_2 = 8.0 * 2.2046226,
    agedays_3 = 300L, wlb_3 = 9.5 * 2.2046226)
  r <- suppressWarnings(cleangrowth(
    w, input_format = "wide", wide_cols = list(WEIGHTLBS = "wlb"),
    widened_emit = c("exclude", "measurement"), quietly = TRUE))
  d <- as.data.table(r)
  # User's pounds column untouched.
  expect_identical(d$wlb_1, w$wlb_1)
  # gc's value is the converted metric one; param says so (units in the data).
  expect_equal(d$gc_internal_meas_wlb_1, w$wlb_1 / 2.2046226, tolerance = 1e-8)
  expect_identical(as.character(unique(d$gc_internal_param_wlb_1)), "WEIGHTKG")
  # param rides along automatically with measurement.
  expect_true("gc_internal_param_wlb_2" %in% names(d))
})

# ---------------------------------------------------------------------------
# Test 6: carry_through in mirror mode
# ---------------------------------------------------------------------------
test_that("carry_through = FALSE drops only the user's extra columns", {
  w <- .mk_wide()
  r_keep <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                         wide_cols = .wide_pmap, quietly = TRUE))
  r_drop <- suppressWarnings(cleangrowth(w, input_format = "wide", wide_cols = .wide_pmap,
                                         carry_through = FALSE, quietly = TRUE))
  expect_true("clinic" %in% names(r_keep))
  expect_false("clinic" %in% names(r_drop))
  # Structural, value, and verdict columns all survive carry_through = FALSE.
  expect_true(all(c("subjid", "sex", "agedays_1", "wt_1", "exclude_wt_1",
                    "ht_3", "exclude_ht_3") %in% names(r_drop)))
})

# ---------------------------------------------------------------------------
# Test 7: same-day blocks resolved as SDEs, verdicts land on the right blocks
# ---------------------------------------------------------------------------
test_that("same-day blocks are resolved and verdicts land on their own columns", {
  w <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wt_1 = 6.0, ht_1 = 60.0,
    agedays_2 = 100L, wt_2 = 6.2, ht_2 = 60.0,   # same day as block 1
    agedays_3 = 200L, wt_3 = 8.0, ht_3 = 68.0)
  r <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                    wide_cols = .wide_pmap, quietly = TRUE))
  d <- as.data.table(r)
  # The two same-day weights get one Include and one Exclude-Extraneous, each on
  # its own block column.
  verdicts <- sort(c(as.character(d$exclude_wt_1), as.character(d$exclude_wt_2)))
  expect_equal(verdicts, c("Exclude-Extraneous", "Include"))
})

# ---------------------------------------------------------------------------
# Test 8: scope + collision errors
# ---------------------------------------------------------------------------
test_that("wide output is refused for non-wide input and named collisions", {
  # long -> wide is not supported.
  lng <- .wide_to_long(.mk_wide())
  expect_error(cleangrowth(lng, output_format = "wide"),
               "requires input_format = 'wide'")
  # semi-wide -> wide is not supported.
  sw <- data.table(subjid = "A", agedays = 100L, sex = 0L, WEIGHTKG = 6.0)
  expect_error(cleangrowth(sw, input_format = "semi_wide", output_format = "wide"),
               "requires input_format = 'wide'")
  # A generated name colliding with an input column errors before cleaning.
  w <- .mk_wide(); w[, exclude_wt_1 := 1L]
  expect_error(suppressWarnings(cleangrowth(w, input_format = "wide",
                                            wide_cols = .wide_pmap, quietly = TRUE)),
               "already contain column")
})

# ---------------------------------------------------------------------------
# Test 9: determinism
# ---------------------------------------------------------------------------
test_that("wide output is deterministic", {
  w  <- .mk_wide()
  r1 <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                     wide_cols = .wide_pmap, quietly = TRUE))
  r2 <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                     wide_cols = .wide_pmap, quietly = TRUE))
  expect_identical(as.data.table(r1), as.data.table(r2))
})
