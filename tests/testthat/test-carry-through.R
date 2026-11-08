testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Long-input carry-through
#
# In data-frame style, any column that cleangrowth() does not consume as a role
# column (subjid/param/agedays/sex/measurement/id) is carried through by default
# (carry_through = TRUE) and returned appended after the gc output columns, in
# original input order and values. dob/visit_date are carried this way (so
# submitted dates are echoed back). A carried column whose name matches a
# column growthcleanr returns in this run is returned as userinput_<name>, with a
# warning; names of growthcleanr columns not returned in this run are left
# alone. carry_through = FALSE restores the prior
# drop-everything behavior. Long -> long only here (wide/semi-wide mirror-mode
# carry comes with the reshaper).
# =============================================================================

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

# Default cleangrowth() output columns: the 8 base columns plus unit_error_range
# (default ON). Carried-through columns are appended after these.
.gc_default_out <- c("id", "subjid", "param", "agedays", "sex", "measurement",
                     "exclude", "bin_exclude", "unit_error_range")

# Multi-subject pediatric slice for a more realistic carry test
data("syngrowth", package = "growthcleanr", envir = environment())
.sg <- as.data.table(syngrowth)
setkey(.sg, subjid, param, agedays)
.sg_peds <- .sg[agedays < 20 * 365.25]
.sg_small <- .sg_peds[subjid %in% unique(.sg_peds$subjid)[1:5]]
if (!"id" %in% names(.sg_small)) .sg_small[, id := seq_len(.N)]

# ---------------------------------------------------------------------------
# Test 1: extra columns are preserved, appended after the gc columns
# ---------------------------------------------------------------------------
test_that("extra columns are carried through, appended, values intact", {
  d <- .mk_df()
  d[, site   := c("A", "A", "B", "B", "A", "B", "A")]
  d[, weight_note := 1:7 * 1.5]
  res <- cleangrowth(d, quietly = TRUE)

  # gc's default columns, then the carried columns last
  expect_equal(names(res), c(.gc_default_out, "site", "weight_note"))
  # values reattach to the correct rows (input order preserved)
  expect_equal(res$site, d$site)
  expect_equal(res$weight_note, d$weight_note)
})

# ---------------------------------------------------------------------------
# Test 2: carry_through = FALSE reproduces today's 8-column output exactly
# ---------------------------------------------------------------------------
test_that("carry_through = FALSE drops extras (prior behavior)", {
  d <- .mk_df()
  d[, site := "A"]
  res_off <- cleangrowth(d, carry_through = FALSE, quietly = TRUE)
  expect_equal(names(res_off), .gc_default_out)
  # identical exclude decisions to the carry-on run
  res_on <- cleangrowth(d, quietly = TRUE)
  expect_identical(as.character(res_off$exclude), as.character(res_on$exclude))
})

# ---------------------------------------------------------------------------
# Test 3: no extra columns -> byte-identical to before (8 default columns)
# ---------------------------------------------------------------------------
test_that("no extra columns leaves the default output unchanged", {
  d <- .mk_df()
  res <- cleangrowth(d, quietly = TRUE)  # carry_through defaults TRUE
  expect_equal(names(res), .gc_default_out)
})

# ---------------------------------------------------------------------------
# Test 4: carry-through does not change exclusion decisions
# ---------------------------------------------------------------------------
test_that("carried columns do not affect cleaning results", {
  d <- copy(.sg_small)
  res_plain <- cleangrowth(d, quietly = TRUE)
  d2 <- copy(d)
  d2[, clinic := rep_len(c("north", "south"), .N)]
  d2[, note   := seq_len(.N)]
  res_carry <- cleangrowth(d2, quietly = TRUE)
  expect_identical(as.character(res_carry$exclude),
                   as.character(res_plain$exclude))
  expect_equal(res_carry$clinic, d2$clinic)
  expect_equal(res_carry$note,   d2$note)
})

# ---------------------------------------------------------------------------
# Test 5: dob / visit_date are echoed back via carry-through
# ---------------------------------------------------------------------------
test_that("dob and visit_date are echoed back in output", {
  d <- .mk_df()
  d[, dob := as.Date("2015-01-01")]
  d[, visit_date := dob + agedays]
  d[, agedays := NULL]  # date route
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_true(all(c("dob", "visit_date") %in% names(res)))
  expect_equal(res$dob, d$dob)
  expect_equal(res$visit_date, d$visit_date)
  # computed agedays present too
  expect_equal(res$agedays, c(183L, 190L, 222L, 270L, 292L, 355L, 367L))
})

# ---------------------------------------------------------------------------
# Test 6: a column named like a gc output column is returned as userinput_<name>
# ---------------------------------------------------------------------------
test_that("carry column colliding with a gc output name is renamed userinput_<name>", {
  d <- .mk_df()
  d[, exclude := "keep"]
  d[, bin_exclude := 1L]
  expect_warning(res <- cleangrowth(d, quietly = TRUE),
                 regexp = "returned unchanged under the name\\(s\\) 'userinput_exclude', 'userinput_bin_exclude'")
  expect_true(all(c("userinput_exclude", "userinput_bin_exclude") %in% names(res)))
  expect_equal(res$userinput_exclude, rep("keep", nrow(d)))
  expect_equal(res$userinput_bin_exclude, rep(1L, nrow(d)))
  # growthcleanr's own columns are unaffected
  expect_false(any(as.character(res$exclude) == "keep"))

  # the renamed name already in the data is still an error
  d2 <- .mk_df()
  d2[, exclude := "keep"]
  d2[, userinput_exclude := "x"]
  expect_error(cleangrowth(d2, quietly = TRUE), regexp = "userinput_exclude")
})

# ---------------------------------------------------------------------------
# Test 7: a user column named `line` (an internal key name) is carried as-is
# ---------------------------------------------------------------------------
test_that("carry column named like a growthcleanr column NOT returned in this run keeps its name", {
  d <- .mk_df()
  d[, `:=`(cf_status = "a", gc_internal_id = 101:107, internal_id = 1L, mean_ht = 2)]
  expect_no_warning(res <- cleangrowth(d, quietly = TRUE))
  expect_equal(names(res), c(.gc_default_out, "cf_status", "gc_internal_id",
                             "internal_id", "mean_ht"))
  expect_equal(res$gc_internal_id, 101:107)
  # once the switch returns that column, the user's column is renamed
  expect_warning(res2 <- cleangrowth(d, quietly = TRUE, display_gc_id = TRUE,
                                     cf_detail = TRUE, mean_ht = TRUE),
                 regexp = "'userinput_cf_status', 'userinput_gc_internal_id', 'userinput_mean_ht'")
  expect_equal(res2$userinput_gc_internal_id, 101:107)
  expect_true(all(c("cf_status", "gc_internal_id", "mean_ht", "internal_id") %in% names(res2)))
  expect_false("userinput_internal_id" %in% names(res2))
})

test_that("carry column named line is carried through unchanged", {
  d <- .mk_df()
  d[, line := seq_len(.N) + 100L]
  res <- cleangrowth(d, quietly = TRUE)
  expect_equal(res$line, d$line)
})

# ---------------------------------------------------------------------------
# Test 7b: carry-through does NOT require id (keyed on row position, not id)
# ---------------------------------------------------------------------------
test_that("carry-through works without a user-supplied id", {
  d <- .mk_df()
  d[, id := NULL]  # no id column at all
  d[, site := c("A", "A", "B", "B", "A", "B", "A")]
  d[, note := 1:7]
  growthcleanr:::.gc_reset_session_notices()  # ids notice is once per session
  expect_warning(
    res <- cleangrowth(d, quietly = TRUE),
    regexp = "Generated ids"
  )
  # extras reattached to the correct rows despite id being auto-generated
  expect_equal(res$site, d$site)
  expect_equal(res$note, d$note)
  expect_equal(res$id, seq_len(nrow(d)))
})

# ---------------------------------------------------------------------------
# Test 8: carried columns keep original input order and are placed last
# ---------------------------------------------------------------------------
test_that("multiple carried columns keep their original relative order", {
  d <- .mk_df()
  d[, zeta  := 1:7]
  d[, alpha := 8:14]
  d[, mid   := letters[1:7]]
  # reorder input columns so carry cols are interleaved with role columns
  setcolorder(d, c("zeta", "id", "subjid", "alpha", "sex", "agedays",
                   "param", "mid", "measurement"))
  res <- cleangrowth(d, quietly = TRUE)
  carried <- setdiff(names(res), .gc_default_out)
  # appended last, in original input column order among themselves
  expect_equal(carried, c("zeta", "alpha", "mid"))
  expect_equal(tail(names(res), 3), c("zeta", "alpha", "mid"))
})

# ---------------------------------------------------------------------------
# Test 9: carry-through interacts correctly with column remapping
# ---------------------------------------------------------------------------
test_that("remapped role columns are not treated as carry columns", {
  d <- .mk_df()
  setnames(d, c("subjid", "measurement"), c("patid", "value"))
  d[, extra := 1:7]
  res <- cleangrowth(d, subjid = "patid", measurement = "value", quietly = TRUE)
  # patid/value are role columns (renamed output), NOT carried; only `extra` is
  expect_true(all(c("patid", "value", "extra") %in% names(res)))
  expect_equal(res$extra, d$extra)
  # `extra` is last; patid/value sit in their role positions
  expect_equal(tail(names(res), 1), "extra")
})

# ---------------------------------------------------------------------------
# Test 10: carry-through preserves column types (Date, factor, numeric)
# ---------------------------------------------------------------------------
test_that("carried column types are preserved", {
  d <- .mk_df()
  d[, when := as.Date("2020-01-01") + 0:6]
  d[, grp  := factor(rep_len(c("x", "y"), 7))]
  d[, num  := as.numeric(1:7)]
  res <- cleangrowth(d, quietly = TRUE)
  expect_s3_class(res$when, "Date")
  expect_s3_class(res$grp, "factor")
  expect_type(res$num, "double")
  expect_equal(res$when, d$when)
})

# ---------------------------------------------------------------------------
# Test 11: carrying does not mutate the caller's input data.table
# ---------------------------------------------------------------------------
test_that("input data.table is not mutated (no stray `line` column added)", {
  d <- .mk_df()
  d[, site := "A"]
  before <- copy(names(d))
  invisible(cleangrowth(d, quietly = TRUE))
  expect_equal(names(d), before)
  expect_false("line" %in% names(d))
})

# ---------------------------------------------------------------------------
# Test 12: carried columns named like a remapped role column (e.g. a column
# `subjid` when subjid = "patid") come back with the user's values, in single
# and checkpointed runs alike
# ---------------------------------------------------------------------------
test_that("carried columns named like remapped role columns are returned intact", {
  d <- copy(.sg_peds[subjid %in% unique(subjid)[1:4]])
  setnames(d, c("subjid", "id", "param", "agedays", "sex", "measurement"),
           c("patid", "row_id", "type", "age_d", "male", "value"))
  for (nm in c("subjid", "id", "param", "agedays", "sex", "measurement", "line"))
    d[, (nm) := paste0(nm, "-", .I)]
  args <- list(d, subjid = "patid", id = "row_id", param = "type",
               agedays = "age_d", sex = "male", measurement = "value",
               quietly = TRUE)
  r1 <- as.data.table(do.call(cleangrowth, args))
  r2 <- as.data.table(do.call(cleangrowth, c(args, list(
    checkpoint_dir = tempfile(), checkpoint_size = 2))))
  for (nm in c("patid", "row_id", "value", "age_d", "subjid", "id", "param",
               "agedays", "sex", "measurement", "line")) {
    expect_identical(r1[[nm]], d[[nm]], info = nm)
  }
  expect_false(any(duplicated(names(r1))))
  expect_equal(r2, r1, ignore_attr = TRUE)
})

# ---------------------------------------------------------------------------
# Test 13: a role column named `line` (growthcleanr's internal row-index name)
# is returned intact alongside carried columns
# ---------------------------------------------------------------------------
test_that("a role column named `line` is returned intact with carried columns", {
  base <- copy(.sg_peds[subjid %in% unique(subjid)[1:4]])
  for (role in c("subjid", "param", "agedays", "measurement")) {
    d <- copy(base)
    setnames(d, role, "line")
    d[, extra := .I]
    args <- list(d, quietly = TRUE)
    args[[role]] <- "line"
    r <- as.data.table(do.call(cleangrowth, args))
    expect_identical(r$line, d$line, info = role)
    expect_identical(r$extra, d$extra, info = role)
  }
})

# ---------------------------------------------------------------------------
# mean_ht: returned by default only when the data has adult rows
# ---------------------------------------------------------------------------
test_that("default mean_ht follows the data; explicit values override it", {
  kid <- .mk_df()
  expect_false("mean_ht" %in% names(cleangrowth(kid, quietly = TRUE)))
  res <- cleangrowth(kid, quietly = TRUE, mean_ht = TRUE)
  expect_true("mean_ht" %in% names(res))
  expect_true(all(is.na(res$mean_ht)))

  adult <- data.table::data.table(subjid = 1L, sex = 0L, param = rep(c("WEIGHTKG", "HEIGHTCM"), each = 2),
                                  agedays = rep(c(9000L, 9400L), 2), measurement = c(70, 71, 175, 175))
  expect_true("mean_ht" %in% names(cleangrowth(data.table::copy(adult), quietly = TRUE)))
  expect_false("mean_ht" %in% names(cleangrowth(data.table::copy(adult), quietly = TRUE, mean_ht = FALSE)))

  # A user column named mean_ht is renamed only when growthcleanr returns its own.
  adult[, mean_ht := 99]
  expect_warning(r <- cleangrowth(data.table::copy(adult), quietly = TRUE), "userinput_mean_ht")
  expect_true(all(r$userinput_mean_ht == 99))
  expect_equal(unique(r[param == "HEIGHTCM"]$mean_ht), 175)
  expect_no_warning(r2 <- cleangrowth(data.table::copy(adult), quietly = TRUE, mean_ht = FALSE))
  expect_true(all(r2$mean_ht == 99))
})

test_that("mean_ht reaches long output from semi-wide input and can be requested for semi-wide output", {
  sw <- data.table::data.table(subjid = rep(1:2, each = 2), sex = c("M", "M", "F", "F"),
                               agedays = rep(c(9000L, 9400L), 2), id = 1:4,
                               WEIGHTKG = c(70, 71, 60, 61), HEIGHTCM = c(175, 175, 160, 160))
  cols <- list(WEIGHTKG = "WEIGHTKG", HEIGHTCM = "HEIGHTCM")
  lg <- cleangrowth(data.table::copy(sw), input_format = "semi_wide", semiwide_cols = cols,
                    output_format = "long", quietly = TRUE)
  expect_true("mean_ht" %in% names(lg))
  wd <- cleangrowth(data.table::copy(sw), input_format = "semi_wide", semiwide_cols = cols,
                    widened_emit = c("exclude", "mean_ht"), quietly = TRUE)
  expect_true("mean_ht_HEIGHTCM" %in% names(wd))
  expect_equal(wd[subjid == 1]$mean_ht_HEIGHTCM, c(175, 175))
})
