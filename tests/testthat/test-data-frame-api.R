testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Data-frame style cleangrowth() API
#
# Verifies the new `cleangrowth(data = ...)` entry point produces identical
# results to the legacy vector-style API, handles column-name remapping,
# auto-generates `id` with a warning when absent / NA / non-unique (preserving
# the user's original `id` in the output and surfacing the gc-used id as
# `gc_internal_id` when `display_gc_id = TRUE`), and errors only on irrecoverable
# input (missing required columns, no data at all, etc.).
# =============================================================================

# Shared small dataset -- same shape as user's typical input
.mk_df <- function() {
  data.table(
    id          = 1:7,
    subjid      = "subj001",
    sex         = 0L,
    agedays     = c(183L, 190L, 222L, 270L, 292L, 355L, 367L),
    param       = "HEIGHTCM",
    measurement = c(68.77, 64.05, 68.13, 76.00, 69.98, 71.23, 76.13)
  )
}

# A multi-subject slice from syngrowth for the cross-style equivalence check
data("syngrowth", package = "growthcleanr", envir = environment())
.sg <- as.data.table(syngrowth)
setkey(.sg, subjid, param, agedays)
.sg_peds <- .sg[agedays < 20 * 365.25]
.sg_small <- .sg_peds[subjid %in% unique(.sg_peds$subjid)[1:5]]
if (!"id" %in% names(.sg_small)) .sg_small[, id := seq_len(.N)]

# ---------------------------------------------------------------------------
# Test 1: data.frame input with all-default column names
# ---------------------------------------------------------------------------
test_that("cleangrowth(data = df) works with canonical column names", {
  d <- .mk_df()
  res <- cleangrowth(d, quietly = TRUE)
  expect_s3_class(res, "data.table")
  expect_true(all(c("id", "subjid", "param", "agedays", "sex",
                    "measurement", "exclude", "bin_exclude") %in% names(res)))
  expect_equal(nrow(res), nrow(d))
})

# ---------------------------------------------------------------------------
# Test 2: data.frame input with plain data.frame (not data.table)
# ---------------------------------------------------------------------------
test_that("cleangrowth(data = df) works with a plain data.frame", {
  d <- as.data.frame(.mk_df())
  res <- cleangrowth(d, quietly = TRUE)
  expect_equal(nrow(res), nrow(d))
})

# ---------------------------------------------------------------------------
# Test 3: column-name remapping via string arguments
# ---------------------------------------------------------------------------
test_that("cleangrowth() accepts remapped column names as strings", {
  d <- .mk_df()
  d2 <- copy(d)
  setnames(d2,
           c("subjid", "id", "param", "agedays", "sex", "measurement"),
           c("patid", "row_id", "ptype", "age_d", "gender", "value"))
  res_default  <- cleangrowth(d, quietly = TRUE)
  res_remapped <- cleangrowth(d2,
                              subjid      = "patid",
                              id          = "row_id",
                              param       = "ptype",
                              agedays     = "age_d",
                              sex         = "gender",
                              measurement = "value",
                              quietly     = TRUE)
  # exclusion decisions should match
  expect_identical(as.character(res_remapped$exclude),
                   as.character(res_default$exclude))
  # output columns are renamed to the user's column names
  expect_true(all(c("patid", "row_id", "ptype", "age_d",
                    "gender", "value") %in% names(res_remapped)))
})

# ---------------------------------------------------------------------------
# Test 4: data-frame path matches vector path on multi-subject data
# ---------------------------------------------------------------------------
test_that("data-frame and vector styles produce identical results", {
  d <- copy(.sg_small)
  res_df  <- cleangrowth(d, quietly = TRUE)
  res_vec <- cleangrowth(subjid      = d$subjid,
                         param       = d$param,
                         agedays     = d$agedays,
                         sex         = d$sex,
                         measurement = d$measurement,
                         id          = d$id,
                         quietly     = TRUE)
  expect_identical(as.character(res_df$exclude),
                   as.character(res_vec$exclude))
})

# ---------------------------------------------------------------------------
# Test 5: missing `id` column warns and auto-generates sequential ids
# ---------------------------------------------------------------------------
test_that("missing id column triggers warning + auto-generates", {
  d <- .mk_df()
  d[, id := NULL]
  growthcleanr:::.gc_reset_session_notices()  # ids notice is once per session
  expect_warning(res <- cleangrowth(d, quietly = TRUE),
                 regexp = "Generated ids")
  expect_equal(res$id, seq_len(nrow(d)))
})

# ---------------------------------------------------------------------------
# Test 6: missing required columns produce a clean stop()
# ---------------------------------------------------------------------------
test_that("missing required columns error clearly", {
  d <- .mk_df()
  d2 <- copy(d); d2[, sex := NULL]
  expect_error(cleangrowth(d2, quietly = TRUE),
               regexp = "required column.*sex")

  d3 <- copy(d); d3[, measurement := NULL]
  expect_error(cleangrowth(d3, quietly = TRUE),
               regexp = "required column.*measurement")
})

# ---------------------------------------------------------------------------
# Test 7: non-data.frame `data` errors clearly
# ---------------------------------------------------------------------------
test_that("non-data.frame `data` argument errors clearly", {
  expect_error(cleangrowth(data = 1:5),
               regexp = "data.frame or data.table")
  expect_error(cleangrowth(data = list(a = 1)),
               regexp = "data.frame or data.table")
})

# ---------------------------------------------------------------------------
# Test 8: column-name argument must be a single string
# ---------------------------------------------------------------------------
test_that("column-name args reject vectors and non-strings", {
  d <- .mk_df()
  expect_error(cleangrowth(d, subjid = c("a", "b"), quietly = TRUE),
               regexp = "single column name")
  expect_error(cleangrowth(d, subjid = 1, quietly = TRUE),
               regexp = "single column name")
})

# ---------------------------------------------------------------------------
# Test 9: vector-style call without id auto-generates with a warning
# ---------------------------------------------------------------------------
test_that("vector style without id warns and auto-generates", {
  d <- .mk_df()
  growthcleanr:::.gc_reset_session_notices()  # ids notice is once per session
  expect_warning(
    res <- cleangrowth(subjid      = d$subjid,
                       param       = d$param,
                       agedays     = d$agedays,
                       sex         = d$sex,
                       measurement = d$measurement,
                       quietly     = TRUE),
    regexp = "Generated ids"
  )
  expect_equal(res$id, seq_len(nrow(d)))
})

# ---------------------------------------------------------------------------
# Test 10: no `data` and no vectors errors clearly
# ---------------------------------------------------------------------------
test_that("calling with neither `data` nor vectors errors clearly", {
  expect_error(cleangrowth(),
               regexp = "Must provide either")
})

# ---------------------------------------------------------------------------
# Test 11: non-unique `id` warns + auto-regenerates, preserves user input
# ---------------------------------------------------------------------------
test_that("duplicate id values warn and auto-regenerate, preserving user input", {
  d <- .mk_df()
  d[2, id := 1L]  # collide id 2 onto id 1
  user_id <- d$id

  # Without display_gc_id: warning fires, output `id` preserves user's bad ids,
  # no `gc_internal_id` column.
  expect_warning(res <- cleangrowth(d, quietly = TRUE),
                 regexp = "duplicated value")
  expect_equal(res$id, user_id,
               info = "output `id` preserves user's input (incl. duplicates)")
  expect_false("gc_internal_id" %in% names(res),
               info = "gc_internal_id absent without display_gc_id")

  # With display_gc_id: gc_internal_id is the regen 1:N (in input row order); output `id`
  # still preserves the user's bad ids.
  expect_warning(res2 <- cleangrowth(d, display_gc_id = TRUE, quietly = TRUE),
                 regexp = "duplicated value")
  expect_equal(res2$id, user_id,
               info = "output `id` preserves user's input even with display_gc_id")
  expect_equal(res2$gc_internal_id, seq_len(nrow(d)),
               info = "gc_internal_id is the auto-regen 1:N in input row order")

  # full_detail = TRUE also enables gc_internal_id
  expect_warning(res3 <- cleangrowth(d, full_detail = TRUE, quietly = TRUE),
                 regexp = "duplicated value")
  expect_true("gc_internal_id" %in% names(res3),
              info = "full_detail enables gc_internal_id")
  expect_equal(res3$gc_internal_id, seq_len(nrow(d)))

  # full_detail does NOT enable the qm_detail eligibility flags: they are for
  # downstream quality-measure tooling, must be requested explicitly, and do
  # not affect cleaning.
  expect_false("qm_elig_c_cf" %in% names(res3),
               info = "full_detail does NOT enable qm_detail eligibility flags")
  expect_warning(res_qm <- cleangrowth(d, qm_detail = TRUE, quietly = TRUE),
                 regexp = "duplicated value")
  expect_true("qm_elig_c_cf" %in% names(res_qm),
              info = "qm_detail = TRUE enables the qm_elig eligibility flags")

  # Vector style behaves the same
  expect_warning(
    cleangrowth(subjid = d$subjid, param = d$param, agedays = d$agedays,
                sex = d$sex, measurement = d$measurement, id = d$id,
                quietly = TRUE),
    regexp = "duplicated value")
})

# ---------------------------------------------------------------------------
# Test 12: NA in `id` warns + auto-regenerates, preserves user input
# ---------------------------------------------------------------------------
test_that("NA id values warn and auto-regenerate, preserving user input", {
  d <- .mk_df()
  d[3, id := NA_integer_]
  user_id <- d$id

  expect_warning(res <- cleangrowth(d, quietly = TRUE),
                 regexp = "missing \\(NA\\) value")
  expect_equal(res$id, user_id,
               info = "output `id` preserves NA exactly as supplied")
  expect_false("gc_internal_id" %in% names(res))

  expect_warning(res2 <- cleangrowth(d, display_gc_id = TRUE, quietly = TRUE),
                 regexp = "missing \\(NA\\) value")
  expect_equal(res2$id, user_id,
               info = "output `id` preserves NA even with display_gc_id")
  expect_equal(res2$gc_internal_id, seq_len(nrow(d)),
               info = "gc_internal_id is the auto-regen 1:N in input row order")
})

# ---------------------------------------------------------------------------
# Test 13: valid `id` + display_gc_id -> gc_internal_id mirrors id (no regen happened)
# ---------------------------------------------------------------------------
test_that("valid id with display_gc_id sets gc_internal_id == id (no warning)", {
  d <- .mk_df()  # has valid unique non-NA ids 1:7
  expect_silent(res <- suppressMessages(cleangrowth(d, display_gc_id = TRUE, quietly = TRUE)))
  expect_true("gc_internal_id" %in% names(res))
  expect_equal(res$gc_internal_id, res$id,
               info = "valid id case: gc_internal_id mirrors the (untouched) user id")
  expect_equal(res$id, d$id)
})
