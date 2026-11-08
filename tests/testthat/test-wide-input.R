testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Wide input (input_format = "wide", output_format = "long")
#
# Verifies the wide -> long reshape adapter: one row per subject, repeated
# observation blocks marked by a `_N` column suffix, reshaped to canonical long
# form BEFORE cleaning so results are identical to submitting the same
# measurements in long format. Covers the keystone long/wide equivalence, block
# discovery, ragged subjects, prefix remapping, the date route, the strictness
# warnings (gaps, block cap, non-monotonic ages, repeated subjects), same-day
# extraneous values, determinism, and the error catalog.
#
# Scope of this file: wide INPUT -> long OUTPUT. (Wide OUTPUT is covered in
# test-wide-output.R.)
# =============================================================================

# Small hand-built wide fixture: 2 subjects, 3 blocks, ragged (A has 3
# observations, B has 2) and with parameters missing from individual blocks.
.mk_wide <- function() {
  data.table(
    subjid    = c("A", "B"),
    sex       = c(0L, 1L),
    agedays_1 = c(100L, 50L),
    wt_1      = c(6.0, 4.0),
    ht_1      = c(60.0, 55.0),
    hc_1      = c(42.0, 38.0),
    agedays_2 = c(200L, 120L),
    wt_2      = c(8.0, 5.5),
    ht_2      = c(68.0, 60.0),
    hc_2      = c(NA_real_, NA_real_),
    agedays_3 = c(300L, NA_integer_),
    wt_3      = c(9.5, NA_real_),
    ht_3      = c(NA_real_, NA_real_),
    hc_3      = c(NA_real_, NA_real_)
  )
}

.wide_pmap <- list(WEIGHTKG = "wt", HEIGHTCM = "ht", HEADCM = "hc")
# Weight + height only: a mapped prefix with no columns is an error, so
# fixtures without head circumference map only what they carry.
.wide_pmap_wh <- list(WEIGHTKG = "wt", HEIGHTCM = "ht")

# The equivalent long form of .mk_wide(), derived rather than hand-transcribed.
.mk_wide_long_equiv <- function() {
  w <- .mk_wide()
  out <- rbindlist(lapply(1:3, function(b) {
    rbindlist(lapply(names(.wide_pmap), function(p) {
      data.table(subjid = w$subjid, param = p,
                 agedays = as.integer(w[[paste0("agedays_", b)]]),
                 sex = w$sex,
                 measurement = as.numeric(w[[paste0(.wide_pmap[[p]], "_", b)]]),
                 .block = b)
    }))
  }))
  out <- out[!is.na(measurement)]
  # Emission order: subject, then block, then weight -> height -> HC.
  out[, .prank := match(param, c("WEIGHTKG", "HEIGHTCM", "HEADCM"))]
  setorder(out, subjid, .block, .prank)
  out[, c(".block", ".prank") := NULL]
  out[, id := seq_len(.N)]
  out[]
}

# Turn a canonical long table into wide: one row per subject, one block per
# distinct agedays, blocks numbered in age order.
.long_to_wide <- function(lng, pmap = .wide_pmap) {
  d <- as.data.table(copy(lng))
  vis <- unique(d[, .(subjid, agedays)])
  setorder(vis, subjid, agedays)
  vis[, block := seq_len(.N), by = subjid]
  d <- merge(d, vis, by = c("subjid", "agedays"))

  ages <- dcast(vis, subjid ~ block, value.var = "agedays")
  setnames(ages, setdiff(names(ages), "subjid"),
           paste0("agedays_", setdiff(names(ages), "subjid")))

  d[, .pcol := unlist(pmap)[match(param, names(pmap))]]
  vals <- dcast(d, subjid ~ .pcol + block, value.var = "measurement", sep = "_")

  sexes <- unique(d[, .(subjid, sex)])
  w <- merge(merge(sexes, ages, by = "subjid"), vals, by = "subjid")
  w[]
}

.align_exclude <- function(res) {
  d <- as.data.table(res)
  d <- d[, .(subjid = as.character(subjid), param = as.character(param),
             agedays = as.integer(agedays), measurement = as.numeric(measurement),
             exclude = as.character(exclude))]
  setkey(d, subjid, param, agedays, measurement)
  d
}

# ---------------------------------------------------------------------------
# Test 1: KEYSTONE -- wide reshape produces identical results to long
# (real dataset). Dedupe same-day/same-param rows so the block layout is
# unambiguous; SDE ordering is tested separately below.
# ---------------------------------------------------------------------------
test_that("wide input matches long input on a real dataset (keystone)", {
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  sg <- sg[agedays < 20 * 365.25]
  sg <- sg[subjid %in% unique(sg$subjid)[1:40]]
  setkey(sg, subjid, param, agedays)
  sg <- sg[!duplicated(sg, by = c("subjid", "param", "agedays"))]
  sg[, id := seq_len(.N)]

  r_long <- cleangrowth(sg, quietly = TRUE)
  w      <- .long_to_wide(sg, .wide_pmap_wh)
  r_wide <- suppressWarnings(cleangrowth(
    w, input_format = "wide", output_format = "long",
    wide_cols = .wide_pmap_wh, max_blocks = 1000, quietly = TRUE))

  a <- .align_exclude(r_long)
  b <- .align_exclude(r_wide)
  m <- merge(a, b, by = c("subjid", "param", "agedays", "measurement"),
             all = TRUE, suffixes = c("_long", "_wide"))
  expect_equal(nrow(m), nrow(a))
  expect_false(any(is.na(m$exclude_long)) || any(is.na(m$exclude_wide)))
  expect_identical(m$exclude_long, m$exclude_wide)
})

# ---------------------------------------------------------------------------
# Test 2: small hand-built equivalence (ragged blocks, empty cells dropped)
# ---------------------------------------------------------------------------
test_that("wide reshape matches hand-built long (ragged blocks)", {
  w   <- .mk_wide()
  lng <- .mk_wide_long_equiv()

  r_wide <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                         wide_cols = .wide_pmap, quietly = TRUE))
  r_lng  <- cleangrowth(lng, quietly = TRUE)

  # Same set of measurements survives the reshape (no extra/dropped rows).
  expect_equal(nrow(as.data.table(r_wide)), nrow(lng))
  a <- .align_exclude(r_lng)
  b <- .align_exclude(r_wide)
  m <- merge(a, b, by = c("subjid", "param", "agedays", "measurement"),
             all = TRUE, suffixes = c("_long", "_wide"))
  expect_equal(nrow(m), nrow(a))
  expect_identical(m$exclude_long, m$exclude_wide)

  # Minted ids follow the documented emission order (subject, block, then
  # weight -> height -> HC), so they match the hand-built long fixture.
  rw <- as.data.table(r_wide)
  setorder(rw, gc_internal_id)
  expect_identical(as.character(rw$param), as.character(lng$param))
  expect_equal(as.numeric(rw$measurement), as.numeric(lng$measurement))
})

# ---------------------------------------------------------------------------
# Test 3: default prefixes are the parameter names themselves
# ---------------------------------------------------------------------------
test_that("wide input defaults to parameter-named prefixes", {
  w <- .mk_wide()
  setnames(w,
           c(paste0("wt_", 1:3), paste0("ht_", 1:3), paste0("hc_", 1:3)),
           c(paste0("WEIGHTKG_", 1:3), paste0("HEIGHTCM_", 1:3), paste0("HEADCM_", 1:3)))

  r     <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long", quietly = TRUE))
  r_ref <- suppressWarnings(cleangrowth(.mk_wide(), input_format = "wide", output_format = "long",
                                        wide_cols = .wide_pmap, quietly = TRUE))
  expect_identical(.align_exclude(r), .align_exclude(r_ref))
})

# ---------------------------------------------------------------------------
# Test 4: structural columns and the age prefix can be remapped
# ---------------------------------------------------------------------------
test_that("wide structural columns and age prefix can be remapped", {
  w <- .mk_wide()
  setnames(w, c("subjid", "sex"), c("patid", "male"))
  setnames(w, paste0("agedays_", 1:3), paste0("age_", 1:3))

  r <- suppressWarnings(cleangrowth(
    w, input_format = "wide", output_format = "long", wide_cols = .wide_pmap,
    subjid = "patid", sex = "male", agedays = "age", quietly = TRUE))

  r_ref <- suppressWarnings(cleangrowth(.mk_wide(), input_format = "wide", output_format = "long",
                                        wide_cols = .wide_pmap, quietly = TRUE))
  expect_equal(nrow(as.data.table(r)), 11L)
  # structural columns come back under the user's names
  r <- as.data.table(r)
  expect_true(all(c("patid", "male", "age") %in% names(r)))
  setnames(r, c("patid", "male", "age"), c("subjid", "sex", "agedays"))
  expect_identical(.align_exclude(r)$exclude, .align_exclude(r_ref)$exclude)
})

# ---------------------------------------------------------------------------
# Test 5: blocks are discovered, not assumed -- a subject may use any subset
# ---------------------------------------------------------------------------
test_that("only occupied cells become measurements", {
  w <- .mk_wide()
  r <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                    wide_cols = .wide_pmap, quietly = TRUE))
  d <- as.data.table(r)
  expect_equal(nrow(d[subjid == "A"]), 6L)   # 3 WT, 2 HT, 1 HC
  expect_equal(nrow(d[subjid == "B"]), 5L)   # 2 WT, 2 HT, 1 HC
  expect_equal(sort(unique(d[subjid == "B"]$agedays)), c(50L, 120L))
})

# ---------------------------------------------------------------------------
# Test 6: two blocks on the same day are resolved as SDEs
# ---------------------------------------------------------------------------
test_that("same-day duplicate blocks are resolved as SDEs", {
  w <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wt_1 = 6.0,  ht_1 = 60.0,
    agedays_2 = 100L, wt_2 = 6.2,  ht_2 = 60.0,   # same day as block 1
    agedays_3 = 200L, wt_3 = 8.0,  ht_3 = 68.0,
    agedays_4 = 300L, wt_4 = 9.5,  ht_4 = 74.0)
  r  <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                     wide_cols = .wide_pmap_wh, quietly = TRUE))
  wt <- as.data.table(r)[param == "WEIGHTKG" & agedays == 100L]
  expect_equal(nrow(wt), 2L)
  expect_equal(sort(as.character(wt$exclude)), c("Exclude-Extraneous", "Include"))
})

# ---------------------------------------------------------------------------
# Test 7: date route (dob + visit_date_N) equals the agedays route
# ---------------------------------------------------------------------------
test_that("wide date route matches the agedays route", {
  dob <- as.Date("2020-01-01")
  wd <- data.table(
    subjid = "A", sex = 0L, dob = "2020-01-01",
    visit_date_1 = as.character(dob + 100L), wt_1 = 6.0,  ht_1 = 60.0,
    visit_date_2 = as.character(dob + 200L), wt_2 = 8.0,  ht_2 = 68.0,
    visit_date_3 = as.character(dob + 300L), wt_3 = 9.5,  ht_3 = 74.0)
  wa <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wt_1 = 6.0, ht_1 = 60.0,
    agedays_2 = 200L, wt_2 = 8.0, ht_2 = 68.0,
    agedays_3 = 300L, wt_3 = 9.5, ht_3 = 74.0)

  rd <- suppressWarnings(cleangrowth(wd, input_format = "wide", output_format = "long",
                                     wide_cols = .wide_pmap_wh,
                                     dob = "dob", visit_date = "visit_date", quietly = TRUE))
  ra <- suppressWarnings(cleangrowth(wa, input_format = "wide", output_format = "long",
                                     wide_cols = .wide_pmap_wh, quietly = TRUE))
  expect_equal(sort(unique(rd$agedays)), c(100L, 200L, 300L))
  expect_identical(.align_exclude(rd)$exclude, .align_exclude(ra)$exclude)
})

# ---------------------------------------------------------------------------
# Test 8: a value with no age is handled exactly as in long format
# ---------------------------------------------------------------------------
test_that("a value in a block with no age is flagged as missing information", {
  w <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wt_1 = 6.0,  ht_1 = 60.0,
    agedays_2 = 200L, wt_2 = 8.0,  ht_2 = 68.0,
    agedays_3 = NA_integer_, wt_3 = 9.5, ht_3 = 74.0)   # values, no age
  r <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                    wide_cols = .wide_pmap_wh, quietly = TRUE))
  d <- as.data.table(r)
  noage <- d[is.na(agedays)]
  expect_equal(nrow(noage), 2L)
  expect_true(all(as.character(noage$exclude) == "Exclude-Missing-Info"))
})

# ---------------------------------------------------------------------------
# Test 9: strictness warnings (gaps, block cap, non-monotonic ages, repeats)
# ---------------------------------------------------------------------------
test_that("a gap in the block sequence warns and both sides survive", {
  w <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wt_1 = 6.0, ht_1 = 60.0,
    agedays_3 = 300L, wt_3 = 9.5, ht_3 = 74.0)     # no block 2
  expect_warning(
    r <- cleangrowth(w, input_format = "wide", output_format = "long",
                     wide_cols = .wide_pmap_wh, quietly = TRUE),
    "block")
  expect_equal(sort(unique(suppressWarnings(as.data.table(r))$agedays)), c(100L, 300L))
})

test_that("a block numbered _0 is accepted (baseline) and ordered first", {
  # Some exports use _0 for a baseline observation; it is a valid block and
  # sorts ahead of _1 (blocks are emitted in ascending numeric order).
  w <- data.table(
    subjid = "A", sex = 0L,
    agedays_0 = 40L,  wt_0 = 5.0, ht_0 = 54.0,   # baseline
    agedays_1 = 100L, wt_1 = 6.0, ht_1 = 60.0,
    agedays_2 = 200L, wt_2 = 8.0, ht_2 = 68.0)
  r <- as.data.table(cleangrowth(w, input_format = "wide", output_format = "long",
                                 wide_cols = .wide_pmap_wh, quietly = TRUE))
  expect_true(40L %in% r$agedays)                       # baseline block cleaned
  expect_equal(nrow(r), 6L)                             # 3 blocks x 2 params
  # The baseline weight gets the first minted id within the subject.
  expect_equal(r[param == "WEIGHTKG"][order(gc_internal_id)][1]$agedays, 40L)
})

test_that("exceeding max_blocks warns but processes every block", {
  w <- .mk_wide()
  expect_warning(
    r <- cleangrowth(w, input_format = "wide", output_format = "long",
                     wide_cols = .wide_pmap, max_blocks = 2, quietly = TRUE),
    "max_blocks")
  expect_true(300L %in% as.data.table(r)$agedays)   # block 3 still cleaned
})

test_that("non-monotonic block ages warn but are processed", {
  w <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 300L, wt_1 = 9.5, ht_1 = 74.0,
    agedays_2 = 100L, wt_2 = 6.0, ht_2 = 60.0,      # out of order
    agedays_3 = 200L, wt_3 = 8.0, ht_3 = 68.0)
  expect_warning(
    r <- cleangrowth(w, input_format = "wide", output_format = "long",
                     wide_cols = .wide_pmap_wh, quietly = TRUE),
    "do not increase")
  expect_equal(nrow(as.data.table(r)), 6L)
})

test_that("a subject on more than one row warns and keeps both rows", {
  w <- data.table(
    subjid = c("A", "A"), sex = c(0L, 0L),
    agedays_1 = c(100L, 300L), wt_1 = c(6.0, 9.5), ht_1 = c(60.0, 74.0),
    agedays_2 = c(200L, 400L), wt_2 = c(8.0, 11.0), ht_2 = c(68.0, 80.0))
  expect_warning(
    r <- cleangrowth(w, input_format = "wide", output_format = "long",
                     wide_cols = .wide_pmap_wh, quietly = TRUE),
    "more than one row")
  expect_equal(sort(unique(as.data.table(r)$agedays)), c(100L, 200L, 300L, 400L))
})

# ---------------------------------------------------------------------------
# Test 10: missing-relevant-parameter warnings
# ---------------------------------------------------------------------------
test_that("missing weight columns warn", {
  w <- data.table(subjid = "A", sex = 0L,
                  agedays_1 = 100L, ht_1 = 60.0,
                  agedays_2 = 200L, ht_2 = 68.0)
  expect_warning(cleangrowth(w, input_format = "wide", output_format = "long",
                             wide_cols = list(HEIGHTCM = "ht"), quietly = TRUE),
                 "No weight measurements")
})

test_that("no HC column is silent for an under-2 dataset", {
  # An under-2 dataset with no HC column is silent (there is no HC-absent warning).
  w_young <- data.table(subjid = "A", sex = 0L,
                        agedays_1 = 100L, wt_1 = 6.0, ht_1 = 60.0,
                        agedays_2 = 200L, wt_2 = 8.0, ht_2 = 68.0)
  expect_silent(suppressMessages(
    cleangrowth(w_young, input_format = "wide", output_format = "long",
                wide_cols = .wide_pmap_wh, quietly = TRUE)))
})

# ---------------------------------------------------------------------------
# Test 11: determinism / row-order independence
# ---------------------------------------------------------------------------
test_that("wide result is deterministic and subject-order independent", {
  w  <- .mk_wide()
  r1 <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                     wide_cols = .wide_pmap, quietly = TRUE))
  r2 <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                     wide_cols = .wide_pmap, quietly = TRUE))
  expect_identical(.align_exclude(r1), .align_exclude(r2))

  r3 <- suppressWarnings(cleangrowth(w[c(2, 1)], input_format = "wide", output_format = "long",
                                     wide_cols = .wide_pmap, quietly = TRUE))
  expect_identical(.align_exclude(r1)$exclude, .align_exclude(r3)$exclude)
})

# ---------------------------------------------------------------------------
# Test 12: wide and semi-wide input agree on the same measurements
# ---------------------------------------------------------------------------
test_that("wide and semi-wide input produce the same results", {
  lng <- .mk_wide_long_equiv()
  sw  <- dcast(lng, subjid + agedays + sex ~ param, value.var = "measurement")
  w   <- .long_to_wide(lng)

  r_sw <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long",
                                       quietly = TRUE))
  r_w  <- suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                       wide_cols = .wide_pmap, quietly = TRUE))
  expect_identical(.align_exclude(r_sw)$exclude, .align_exclude(r_w)$exclude)
})

# ---------------------------------------------------------------------------
# Test 12b: imperial / length prefixes map through and convert
#
# The adapter only emits the parameter string; the funnel does the conversion.
# So a wide table of pounds and inches must clean identically to the same
# measurements submitted in kilograms and centimeters, and the returned
# measurement must still be the user's verbatim submission.
# ---------------------------------------------------------------------------
test_that("wide imperial and length prefixes convert in the funnel", {
  w_imp <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wlb_1 = 6.0 * 2.2046226, hin_1 = 60.0 / 2.54,
    agedays_2 = 200L, wlb_2 = 8.0 * 2.2046226, hin_2 = 68.0 / 2.54,
    agedays_3 = 300L, wlb_3 = 9.5 * 2.2046226, hin_3 = 74.0 / 2.54)
  w_met <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wt_1 = 6.0, ht_1 = 60.0,
    agedays_2 = 200L, wt_2 = 8.0, ht_2 = 68.0,
    agedays_3 = 300L, wt_3 = 9.5, ht_3 = 74.0)

  r_imp <- suppressWarnings(cleangrowth(
    w_imp, input_format = "wide", output_format = "long",
    wide_cols = list(WEIGHTLBS = "wlb", HEIGHTIN = "hin"), quietly = TRUE))
  r_met <- suppressWarnings(cleangrowth(
    w_met, input_format = "wide", output_format = "long",
    wide_cols = .wide_pmap_wh, quietly = TRUE))

  di <- as.data.table(r_imp); setorder(di, gc_internal_id)
  dm <- as.data.table(r_met); setorder(dm, gc_internal_id)
  expect_identical(as.character(di$exclude), as.character(dm$exclude))
  # Output is the verbatim submission, not the converted value.
  expect_identical(as.character(unique(di$param)), c("WEIGHTLBS", "HEIGHTIN"))
  expect_equal(di[param == "WEIGHTLBS"]$measurement,
               c(6.0, 8.0, 9.5) * 2.2046226)

  # LENGTHCM relabels to HEIGHTCM internally but is returned as submitted.
  w_len <- copy(w_met); setnames(w_len, paste0("ht_", 1:3), paste0("len_", 1:3))
  r_len <- suppressWarnings(cleangrowth(
    w_len, input_format = "wide", output_format = "long",
    wide_cols = list(WEIGHTKG = "wt", LENGTHCM = "len"), quietly = TRUE))
  dl <- as.data.table(r_len); setorder(dl, gc_internal_id)
  expect_identical(as.character(dl$exclude), as.character(dm$exclude))
  expect_true("LENGTHCM" %in% as.character(dl$param))
})

# ---------------------------------------------------------------------------
# Test 13: the input table is not modified by the call
# ---------------------------------------------------------------------------
test_that("wide input data.table is left untouched", {
  w    <- .mk_wide()
  pre  <- copy(w)
  invisible(suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
                                         wide_cols = .wide_pmap, quietly = TRUE)))
  expect_identical(w, pre)
})

# ---------------------------------------------------------------------------
# Test 14: error catalog
# ---------------------------------------------------------------------------
test_that("wide input errors on bad input", {
  w <- .mk_wide()

  # No data.
  expect_error(cleangrowth(input_format = "wide", output_format = "long"),
               "requires a data.frame")

  # No block columns at all.
  bad <- data.table(subjid = "A", sex = 0L, agedays = 100L, WEIGHTKG = 6.0)
  expect_error(cleangrowth(bad, input_format = "wide", output_format = "long"),
               "No recognized parameter columns")

  # wide_cols maps to a prefix with no _N columns.
  expect_error(
    cleangrowth(w, input_format = "wide", output_format = "long",
                wide_cols = list(WEIGHTKG = "weight")),
    "no `<prefix>_N` columns")

  # wide_cols with an unrecognized parameter name.
  expect_error(
    cleangrowth(w, input_format = "wide", output_format = "long",
                wide_cols = list(WEIGHT = "wt")),
    "growthcleanr parameters")

  # Two parameters sharing one prefix.
  expect_error(
    cleangrowth(w, input_format = "wide", output_format = "long",
                wide_cols = list(WEIGHTKG = "wt", WEIGHTLBS = "wt")),
    "more than one parameter to the same prefix")

  # Missing subjid column.
  nosub <- copy(w); setnames(nosub, "subjid", "pid")
  expect_error(cleangrowth(nosub, input_format = "wide", output_format = "long",
                           wide_cols = .wide_pmap),
               "subjid column")

  # No age source.
  noage <- copy(w); setnames(noage, paste0("agedays_", 1:3), paste0("visitage_", 1:3))
  expect_error(cleangrowth(noage, input_format = "wide", output_format = "long",
                           wide_cols = .wide_pmap),
               "No age information found")

  # A user-supplied row id is accepted (it does not affect cleaning): long
  # output carries it under its own name, one value per input row, with
  # growthcleanr's per-measurement number in gc_internal_id.
  wi <- copy(w)[, rowid := paste0("r", .I)]
  ri <- suppressWarnings(cleangrowth(wi, input_format = "wide", output_format = "long",
                                     wide_cols = .wide_pmap, id = "rowid", quietly = TRUE))
  expect_true(all(c("rowid", "gc_internal_id") %in% names(ri)))
  expect_false("id" %in% names(ri))
  expect_true(all(ri$rowid %in% wi$rowid))
  expect_equal(ri$gc_internal_id, seq_len(nrow(ri)))

  # Leading-zero suffixes collapse two columns onto one block.
  dup <- copy(w); dup[, wt_01 := 7.0]
  expect_error(cleangrowth(dup, input_format = "wide", output_format = "long",
                           wide_cols = .wide_pmap),
               "more than one column for block")

  # (The mirror default returns wide output; see test-wide-output.R.)
})

# ---------------------------------------------------------------------------
# Test 15: the round-trip spec the re-widened output layer consumes
# ---------------------------------------------------------------------------
test_that("wide adapter builds a round-trip spec on request", {
  w  <- .mk_wide()
  ad <- suppressWarnings(growthcleanr:::.gc_wide_to_long(
    w, wide_cols = .wide_pmap, col_subjid = "subjid", col_sex = "sex",
    col_agedays = "agedays", col_dob = NULL, col_visit_date = NULL,
    age_source = NULL, want_spec = TRUE))
  sp <- ad$spec

  expect_identical(sp$format, "wide")
  # The output base is the submitted table, untouched.
  expect_identical(sp$base, w)
  # One cell-map row per emitted measurement, keyed by the minted id.
  expect_equal(nrow(sp$cell_map), nrow(ad$long))
  expect_identical(sort(sp$cell_map$id), sort(ad$long$id))
  # Generated output columns are named after the individual block columns, so a
  # wide run yields exclude_wt_1, exclude_wt_2, ... rather than exclude_wt.
  expect_true(all(c("wt_1", "wt_2", "wt_3", "ht_1", "hc_1") %in% sp$value_cols))
  expect_true(all(sp$cell_map$user_col %in% sp$value_cols))
  expect_true(all(c("exclude_wt_1", "exclude_hc_3") %in%
                    growthcleanr:::.gc_emit_names("exclude", sp$value_cols)))
})

# ---------------------------------------------------------------------------
# Test 16: length vs height is a long-format-only feature
#
# Wide input always uses default handling of linear measurements: length.adjust
# is rejected, and a LENGTHCM/LENGTHIN value at any age gets default handling
# (the same result as mapping the prefix to HEIGHTCM).
# ---------------------------------------------------------------------------
test_that("wide rejects length.adjust; length at >= 2 years gets default handling", {
  w <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wt_1 = 6.0,  len_1 = 55.0,
    agedays_2 = 800L, wt_2 = 13.0, len_2 = 88.0)   # block 2 is >= 2y
  cols <- list(WEIGHTKG = "wt", LENGTHCM = "len")

  # length.adjust = TRUE is refused for wide (long-only feature).
  expect_error(
    cleangrowth(w, input_format = "wide", wide_cols = cols,
                length.adjust = TRUE, quietly = TRUE),
    "length.adjust = TRUE .*requires long input")

  # A LENGTHCM value at a pediatric age >= 2 years gets default handling: the
  # same result as mapping the prefix to HEIGHTCM.
  r_len <- suppressWarnings(cleangrowth(w, input_format = "wide", wide_cols = cols,
                                        output_format = "long", quietly = TRUE))
  r_ht  <- suppressWarnings(cleangrowth(w, input_format = "wide",
                                        wide_cols = list(WEIGHTKG = "wt", HEIGHTCM = "len"),
                                        output_format = "long", quietly = TRUE))
  expect_equal(as.character(r_len$exclude), as.character(r_ht$exclude))

  # All length values < 2 years -> allowed in wide.
  w_young <- data.table(
    subjid = "A", sex = 0L,
    agedays_1 = 100L, wt_1 = 6.0,  len_1 = 55.0,
    agedays_2 = 400L, wt_2 = 10.0, len_2 = 70.0)
  expect_error(
    suppressWarnings(cleangrowth(w_young, input_format = "wide", wide_cols = cols,
                                 quietly = TRUE)),
    NA)
})

# ---------------------------------------------------------------------------
# Long output from wide input: the user's other subject-level columns are
# carried (repeated on each measurement); block columns are not.
# ---------------------------------------------------------------------------
test_that("long output from wide input carries the user's other columns", {
  w <- .mk_wide()
  w[, `:=`(pt = c("P1", "P2"), site = c("x", "y"), measurement = "m")]
  r <- expect_warning(cleangrowth(w, input_format = "wide", output_format = "long",
                                  wide_cols = .wide_pmap, id = "pt", quietly = TRUE),
                      regexp = "'userinput_measurement'")
  r <- as.data.table(r)
  expect_equal(names(r)[1:2], c("pt", "gc_internal_id"))
  expect_equal(tail(names(r), 2), c("site", "userinput_measurement"))
  expect_equal(r$site, unname(c(A = "x", B = "y")[r$subjid]))
  expect_false(any(grepl("_[0-9]+$", names(r))))
  expect_true(is.numeric(r$measurement))
  # the date route carries the single dob column, not the per-block visit dates
  wd <- .mk_wide()
  for (b in 1:3) wd[, (paste0("visit_date_", b)) := as.Date("2020-01-01") + get(paste0("agedays_", b))]
  wd[, paste0("agedays_", 1:3) := NULL][, dob := as.Date("2020-01-01")]
  rd <- as.data.table(suppressWarnings(cleangrowth(wd, input_format = "wide", output_format = "long",
          wide_cols = .wide_pmap, dob = "dob", visit_date = "visit_date", quietly = TRUE)))
  expect_true("dob" %in% names(rd))
  expect_false(any(grepl("^visit_date", names(rd))))
})

test_that("long output from wide input uses the user's subjid/sex/age names", {
  w <- .mk_wide(); setnames(w, c("subjid", "sex"), c("pid", "gender"))
  setnames(w, paste0("agedays_", 1:3), paste0("age_", 1:3))
  r <- as.data.table(suppressWarnings(cleangrowth(w, input_format = "wide", output_format = "long",
         wide_cols = .wide_pmap, subjid = "pid", sex = "gender", agedays = "age", quietly = TRUE)))
  expect_true(all(c("pid", "gender", "age") %in% names(r)))
  expect_false(any(c("subjid", "sex", "agedays") %in% names(r)))
  expect_warning(cleangrowth(copy(.mk_wide())[, wt_1 := factor(wt_1)], input_format = "wide",
                             output_format = "long", wide_cols = .wide_pmap, quietly = TRUE),
                 regexp = "'wt_1' are factors")
})
