testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Semi-wide output (output_format = "semi_wide")
#
# Semi-wide output returns the user's own visit rows -- every submitted column
# untouched, one row per input row -- with the requested growthcleanr columns
# slotted in immediately right of the value column they describe.
#
# The keystone is an equivalence chain closed in both directions:
#
#   r1 = cleangrowth(long)                                  long -> long
#   r2 = cleangrowth(semi_wide, output_format = "long")      semi -> long
#   r3 = cleangrowth(semi_wide, output_format = "semi_wide") semi -> semi
#
#   exclude(r1) == exclude(r2) == melt_back(r3)
#
# .melt_back() is written here rather than reusing package code, so a bug in
# the output adapter cannot cancel itself out.
# =============================================================================

.mk_semiwide <- function() {
  data.table(
    subjid   = c("A", "A", "A", "B", "B"),
    agedays  = c(100L, 200L, 300L, 50L, 120L),
    sex      = c(0L, 0L, 0L, 1L, 1L),
    WEIGHTKG = c(6.0, 8.0, 9.5, 4.0, 5.5),
    HEIGHTCM = c(60.0, 68.0, NA, 55.0, 60.0),
    HEADCM   = c(42.0, NA, NA, 38.0, NA),
    site     = c("c1", "c1", "c2", "c3", "c3")
  )
}

# Melt semi-wide input to canonical long in the adapter's documented emission
# order (subject first-appearance, then visit row, then weight -> height ->
# head circumference), minting id = 1:N so same-day tiebreaks match.
.sw_to_long <- function(sw, pmap = c(WEIGHTKG = "WEIGHTKG", HEIGHTCM = "HEIGHTCM",
                                     HEADCM = "HEADCM"),
                        extra_id = c("subjid", "agedays", "sex")) {
  sw <- as.data.table(copy(sw))
  sw[, .visit_id := .I]
  pieces <- lapply(seq_along(pmap), function(i) {
    p <- names(pmap)[i]
    if (!pmap[[i]] %in% names(sw)) return(NULL)
    data.table(.visit_id = sw$.visit_id,
               subjid = sw[[extra_id[1]]], agedays = sw[[extra_id[2]]],
               sex = sw[[extra_id[3]]], param = p,
               measurement = sw[[pmap[[i]]]],
               .prank = i)
  })
  lng <- rbindlist(Filter(Negate(is.null), pieces))
  lng <- lng[!is.na(measurement)]
  lng[, .subj_order := match(subjid, unique(sw$subjid))]
  setorder(lng, .subj_order, .visit_id, .prank)
  lng[, id := seq_len(.N)]
  lng[, c(".visit_id", ".prank", ".subj_order") := NULL]
  setcolorder(lng, c("subjid", "param", "agedays", "sex", "measurement", "id"))
  lng[]
}

# Semi-wide output -> long exclude codes, keyed by visit row.
.melt_back <- function(sw_out, value_cols) {
  d <- as.data.table(copy(sw_out))
  d[, .visit_id := .I]
  out <- rbindlist(lapply(value_cols, function(v) {
    data.table(.visit_id = d$.visit_id,
               user_col  = v,
               measurement = d[[v]],
               exclude   = as.character(d[[paste0("exclude_", v)]]))
  }))
  out <- out[!is.na(measurement)]
  setorder(out, .visit_id, user_col)
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
# Test 1: KEYSTONE -- long -> long, semi -> long, and semi -> semi all agree
# ---------------------------------------------------------------------------
test_that("the equivalence chain closes: long == semi->long == melt_back(semi->semi)", {
  sw  <- .mk_semiwide()
  lng <- .sw_to_long(sw)

  r1 <- cleangrowth(lng, quietly = TRUE)
  r2 <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide",
                                     output_format = "long", quietly = TRUE))
  r3 <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE))

  a <- .align(r1); b <- .align(r2)
  m <- merge(a, b, by = c("subjid", "param", "agedays", "measurement"),
             all = TRUE, suffixes = c("_1", "_2"))
  expect_equal(nrow(m), nrow(a))
  expect_identical(m$exclude_1, m$exclude_2)

  # semi -> semi melted back must reproduce semi -> long, row for row.
  mb <- .melt_back(r3, c("WEIGHTKG", "HEIGHTCM", "HEADCM"))
  b2 <- as.data.table(r2)[, .(param = as.character(param),
                              measurement = as.numeric(measurement),
                              exclude = as.character(exclude))]
  setkey(b2, param, measurement)
  mb2 <- mb[, .(param = user_col, measurement = as.numeric(measurement),
                exclude = exclude)]
  setkey(mb2, param, measurement)
  expect_identical(nrow(mb2), nrow(b2))
  expect_identical(mb2$exclude, b2$exclude)
})

# ---------------------------------------------------------------------------
# Test 2: KEYSTONE on a real dataset, WITH same-day duplicates preserved
# (each same-day repeat of a parameter becomes its own visit row)
# ---------------------------------------------------------------------------
test_that("equivalence holds on syngrowth with same-day duplicates as visit rows", {
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  sg <- sg[agedays < 20 * 365.25]
  sg <- sg[subjid %in% unique(sg$subjid)[1:40]]

  # Occurrence index within (subjid, agedays, param) -> visit row k.
  setorder(sg, subjid, agedays, param)
  sg[, k := seq_len(.N), by = .(subjid, agedays, param)]
  sw <- dcast(sg, subjid + agedays + sex + k ~ param, value.var = "measurement")
  setorder(sw, subjid, agedays, k)
  sw[, k := NULL]

  lng <- .sw_to_long(sw, pmap = setNames(intersect(c("WEIGHTKG", "HEIGHTCM", "HEADCM"),
                                                   names(sw)),
                                         intersect(c("WEIGHTKG", "HEIGHTCM", "HEADCM"),
                                                   names(sw))))
  r_long <- cleangrowth(lng, quietly = TRUE)
  r_sw   <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE))

  vcols <- intersect(c("WEIGHTKG", "HEIGHTCM", "HEADCM"), names(sw))
  mb <- .melt_back(r_sw, vcols)
  expect_equal(nrow(mb), nrow(lng))

  # Compare per measurement: same visit order, same parameter order.
  lng2 <- copy(lng)[, .row := .I]
  cmp <- data.table(param = mb$user_col, measurement = as.numeric(mb$measurement),
                    exclude = mb$exclude)
  ref <- as.data.table(r_long)[, .(param = as.character(param),
                                   measurement = as.numeric(measurement),
                                   exclude = as.character(exclude))]
  setkey(cmp, param, measurement, exclude)
  setkey(ref, param, measurement, exclude)
  expect_identical(nrow(cmp), nrow(ref))
  # Same multiset of (param, measurement, exclude) triples.
  expect_identical(cmp[order(param, measurement, exclude)],
                   ref[order(param, measurement, exclude)])
})

# ---------------------------------------------------------------------------
# Test 3: structure -- rows, columns, placement, input untouched
# ---------------------------------------------------------------------------
test_that("semi-wide output echoes the input table exactly, with verdicts slotted in", {
  sw   <- .mk_semiwide()
  orig <- copy(sw)
  r    <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE))

  expect_identical(sw, orig, info = "input data.table is not modified")
  expect_equal(nrow(r), nrow(orig))
  for (cl in names(orig)) {
    expect_identical(r[[cl]], orig[[cl]],
                     info = paste("column", cl, "returned untouched"))
  }
  # exclude_<COL> immediately right of <COL>
  nm <- names(r)
  for (v in c("WEIGHTKG", "HEIGHTCM", "HEADCM")) {
    expect_identical(nm[match(v, nm) + 1L], paste0("exclude_", v))
  }
  # Dropping the generated columns reproduces the input.
  expect_identical(as.data.table(r)[, names(orig), with = FALSE], orig)
  # Empty cell -> NA verdict; present cell -> a code.
  expect_true(is.na(r$exclude_HEADCM[2]))
  expect_false(is.na(r$exclude_HEADCM[1]))
})

# ---------------------------------------------------------------------------
# Test 4: default emits exclude only
# ---------------------------------------------------------------------------
test_that("default semi-wide output emits exclude only", {
  r <- suppressWarnings(cleangrowth(.mk_semiwide(), input_format = "semi_wide",
                                    quietly = TRUE))
  nm <- names(r)
  expect_true(all(paste0("exclude_", c("WEIGHTKG", "HEIGHTCM", "HEADCM")) %in% nm))
  expect_false(any(grepl("^bin_exclude_", nm)))
  expect_false(any(grepl("^unit_error_range_", nm)))
  expect_false(any(grepl("^gc_internal_", nm)))
})

# ---------------------------------------------------------------------------
# Test 5: widened_emit adds columns; switch-gated ones error when not enabled
# ---------------------------------------------------------------------------
test_that("widened_emit controls which growthcleanr columns are added", {
  sw <- .mk_semiwide()

  r <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE,
                                    widened_emit = c("exclude", "bin_exclude")))
  expect_true("bin_exclude_WEIGHTKG" %in% names(r))
  nm <- names(r)
  expect_identical(nm[match("WEIGHTKG", nm) + 1:2],
                   c("exclude_WEIGHTKG", "bin_exclude_WEIGHTKG"))

  expect_error(
    suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE,
                                 widened_emit = "exclude_detailed")),
    "exclude_detail")

  r2 <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE,
                                     exclude_detail = TRUE,
                                     widened_emit = c("exclude", "exclude_detailed")))
  expect_true("exclude_detailed_WEIGHTKG" %in% names(r2))

  expect_error(
    suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE,
                                 widened_emit = "not_a_column")),
    "not_a_column")
})

# ---------------------------------------------------------------------------
# Test 6: imperial -- user's column echoed in submitted units; gc's value and
# parameter label available on request
# ---------------------------------------------------------------------------
test_that("imperial input echoes submitted units and reports gc's value separately", {
  sw <- data.table(
    subjid    = c("A", "A", "A"),
    agedays   = c(100L, 200L, 300L),
    sex       = c(0L, 0L, 0L),
    wt_lbs    = c(13.2, 17.6, 20.9),
    ht_cm     = c(60.0, 68.0, 72.0))

  r <- suppressWarnings(cleangrowth(
    sw, input_format = "semi_wide",
    semiwide_cols = list(WEIGHTLBS = "wt_lbs", HEIGHTCM = "ht_cm"),
    widened_emit = c("exclude", "measurement"), quietly = TRUE))

  expect_identical(r$wt_lbs, sw$wt_lbs, info = "user's lbs column untouched")
  # gc's value is the converted metric one, and the param column says so.
  expect_equal(r$gc_internal_meas_wt_lbs, sw$wt_lbs / 2.2046226, tolerance = 1e-8)
  expect_identical(unique(r$gc_internal_param_wt_lbs), "WEIGHTKG")
  # `param` rides along automatically with `measurement`.
  expect_true("gc_internal_param_ht_cm" %in% names(r))
})

# ---------------------------------------------------------------------------
# Test 7: carry_through in mirror mode
# ---------------------------------------------------------------------------
test_that("carry_through = FALSE drops only the user's extra columns", {
  sw <- .mk_semiwide()
  r  <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE,
                                     carry_through = FALSE))
  expect_false("site" %in% names(r))
  expect_true(all(c("subjid", "agedays", "sex", "WEIGHTKG", "HEIGHTCM", "HEADCM",
                    "exclude_WEIGHTKG") %in% names(r)))
  expect_equal(nrow(r), nrow(sw))
})

# ---------------------------------------------------------------------------
# Test 8: scope errors
# ---------------------------------------------------------------------------
test_that("semi-wide output is refused where it cannot be defined", {
  lng <- .sw_to_long(.mk_semiwide())
  expect_error(cleangrowth(lng, output_format = "semi_wide", quietly = TRUE),
               "requires input_format")

  # Generated name collides with an existing column -> error before cleaning.
  sw <- .mk_semiwide()
  sw[, exclude_WEIGHTKG := "mine"]
  expect_error(suppressWarnings(cleangrowth(sw, input_format = "semi_wide",
                                            quietly = TRUE)),
               "already contain column")

  # Two parameters mapped to one value column.
  sw2 <- .mk_semiwide()
  expect_error(suppressWarnings(cleangrowth(
    sw2, input_format = "semi_wide", quietly = TRUE,
    semiwide_cols = list(HEIGHTCM = "HEIGHTCM", LENGTHCM = "HEIGHTCM"))),
    "more than one parameter")
})

# ---------------------------------------------------------------------------
# Test 9: determinism
# ---------------------------------------------------------------------------
test_that("semi-wide output is deterministic", {
  sw <- .mk_semiwide()
  r1 <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE))
  r2 <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", quietly = TRUE))
  expect_identical(r1, r2)
})

# ---------------------------------------------------------------------------
# length vs height is a long-format-only feature: length.adjust is rejected for
# semi-wide/wide and a LENGTHCM/LENGTHIN value at >= 2 years is an error (see
# test-semiwide-input.R / test-wide-input.R). Reconciliation math for long input
# is covered by Test 11b below.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Test 11b: metric conversion happens BEFORE the linear offset, and the two
# directions meet cleanly at the 2-year boundary. The length.adjust offset is
# applied only where a value is z-scored (sd.orig etc.); gc_internal_meas and
# the raw-cm checks keep the entered (metric) value.
# ---------------------------------------------------------------------------
test_that("inches convert to cm before the offset, and the boundary partitions", {
  # 29 in at 21 months: 29 x 2.54 = 73.66 cm, scored as +0.7 (entered as
  # height, under 2 years) = 74.36. Offsetting before converting would give a
  # different number, so this pins the order.
  d <- data.table(
    subjid = "A", param = "HEIGHTIN",
    agedays = as.integer(round(21 * 30.4375)), sex = 0L,
    measurement = 29.0, id = 1L)
  r <- cleangrowth(d, quietly = TRUE, internal_detail = TRUE, debug = TRUE,
                   length.adjust = TRUE)
  expect_equal(r$gc_internal_meas, 29 * 2.54, tolerance = 1e-9)
  expect_identical(r$gc_internal_param, "HEIGHTCM")
  r0 <- cleangrowth(copy(d)[, `:=`(param = "HEIGHTCM", measurement = 29 * 2.54 + 0.7)],
                    quietly = TRUE, debug = TRUE)
  expect_equal(r$sd.orig, r0$sd.orig, tolerance = 1e-9)

  # Boundary: height starts at day 730 (.gc_ht_start_day, where the reference
  # files switch from length to height), so 729 days is under 2 (height offset,
  # length not) and 730 is 2 or older (length offset, height not). Every row is
  # offset exactly once or not at all.
  b <- data.table(
    subjid = c("B", "B", "C", "C"),
    param  = c("HEIGHTCM", "LENGTHCM", "HEIGHTCM", "LENGTHCM"),
    agedays = c(729L, 729L, 730L, 730L), sex = 0L,
    measurement = c(80.0, 80.0, 80.0, 80.0), id = 1:4)
  rb <- cleangrowth(b, quietly = TRUE, internal_detail = TRUE, debug = TRUE,
                    length.adjust = TRUE)
  expect_equal(rb$gc_internal_meas, c(80.0, 80.0, 80.0, 80.0), tolerance = 1e-9)
  # Scored as 80.7, 80.0, 80.0, 79.3: the same z as default runs at those values.
  b0 <- copy(b)[, `:=`(param = "HEIGHTCM", measurement = c(80.7, 80.0, 80.0, 79.3))]
  rb0 <- cleangrowth(b0, quietly = TRUE, debug = TRUE)
  expect_equal(rb$sd.orig, rb0$sd.orig, tolerance = 1e-9)

  # Adult rows: an adult LENGTHCM is relabeled HEIGHTCM (adult length is
  # cleaned as height), but length.adjust is pediatric-only, so its value is
  # not adjusted. (Imperial conversion still applies: cleanadult uses the same
  # factors.) Rows with a missing age fall through both splits and are left
  # alone.
  ad <- data.table(
    subjid = c("E", "E", "E", "E"),
    param  = c("LENGTHCM", "HEIGHTCM", "HEIGHTIN", "WEIGHTLBS"),
    agedays = as.integer(30 * 365.25), sex = 0L,
    measurement = c(170.0, 170.0, 67.0, 150.0), id = 1:4)
  ra <- cleangrowth(ad, quietly = TRUE, internal_detail = TRUE,
                    length.adjust = TRUE)
  expect_identical(ra$gc_internal_param,
                   c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "WEIGHTKG"))
  expect_equal(ra$gc_internal_meas,
               c(170.0, 170.0, 67 * 2.54, 150 / 2.2046226), tolerance = 1e-9)

  # A missing age is left alone in either direction. (A valid-age anchor row
  # keeps agedays from being entirely NA, which is itself an error.)
  n <- data.table(
    subjid = "D", param = c("HEIGHTCM", "LENGTHCM", "WEIGHTKG"),
    agedays = c(NA_integer_, NA_integer_, 3000L), sex = 0L,
    measurement = c(80.0, 80.0, 12.0), id = 1:3)
  rn <- suppressWarnings(cleangrowth(n, quietly = TRUE, internal_detail = TRUE,
                                     length.adjust = TRUE))
  expect_equal(rn[id %in% 1:2][order(id)]$gc_internal_meas, c(80.0, 80.0),
               tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Test 13: date route -- gc_internal_agedays exposes the computed age
# ---------------------------------------------------------------------------
test_that("date route returns submitted dates and can report the computed age", {
  sw <- data.table(
    subjid     = c("A", "A", "A"),
    dob        = c("2020-01-01", "2020-01-01", "2020-01-01"),
    visit_date = c("2020-04-10", "2020-07-19", "2020-10-27"),
    sex        = c(0L, 0L, 0L),
    WEIGHTKG   = c(6.0, 8.0, 9.5),
    HEIGHTCM   = c(60.0, 68.0, 72.0))

  r <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide",
                                    dob = "dob", visit_date = "visit_date",
                                    widened_emit = c("exclude", "agedays"),
                                    quietly = TRUE))
  expect_identical(r$dob, sw$dob)
  expect_identical(r$visit_date, sw$visit_date)
  expect_equal(r$gc_internal_agedays,
               as.integer(as.Date(sw$visit_date) - as.Date(sw$dob)))
})
