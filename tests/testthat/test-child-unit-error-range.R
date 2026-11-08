testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# unit_error_range output column (child path)
#
# unit_error_range is an informational, direction-agnostic flag: TRUE when a
# value's magnitude is consistent with a metric/imperial unit-conversion error
# AND the value is not itself within the plausible [zn3, zp3] band. It never
# affects `exclude` and is not in exclude.levels. Default ON; omitted when
# unit_error_range = FALSE. Exclude-Missing / Exclude-Not-Cleaned rows are FALSE.
#
# Two layers of coverage:
#   - cleangrowth() contract: column presence/type, suppression, excluded rows.
#   - the pure helpers (.unit_error_range_flag, .compute_unit_error_range):
#     deterministic band logic with synthetic zn3/zp3, independent of the
#     reference tables.
# =============================================================================

data("syngrowth", package = "growthcleanr", envir = environment())
.sg <- as.data.table(syngrowth)
setkey(.sg, subjid, param, agedays)
.sg_peds <- .sg[agedays < 20 * 365.25]
.d1 <- .sg_peds[subjid == unique(.sg_peds$subjid)[1]]

# ---------------------------------------------------------------------------
# Test 1: column present and logical by default
# ---------------------------------------------------------------------------
test_that("unit_error_range column is present and logical by default", {

  res <- cleangrowth(
    subjid = .d1$subjid, param = .d1$param, agedays = .d1$agedays,
    sex = .d1$sex, measurement = .d1$measurement, id = .d1$id,
    quietly = TRUE
  )

  expect_true("unit_error_range" %in% names(res))
  expect_true(is.logical(res$unit_error_range))
  expect_false(any(is.na(res$unit_error_range)))
})

# ---------------------------------------------------------------------------
# Test 2: unit_error_range = FALSE omits the column
# ---------------------------------------------------------------------------
test_that("unit_error_range = FALSE omits the column", {

  res <- cleangrowth(
    subjid = .d1$subjid, param = .d1$param, agedays = .d1$agedays,
    sex = .d1$sex, measurement = .d1$measurement, id = .d1$id,
    unit_error_range = FALSE, quietly = TRUE
  )

  expect_false("unit_error_range" %in% names(res))
})

# ---------------------------------------------------------------------------
# Test 3: Exclude-Missing and Exclude-Not-Cleaned rows are FALSE
# ---------------------------------------------------------------------------
test_that("excluded-from-cleaning rows are FALSE (never flagged)", {

  # HC trajectory spanning the 5y cutoff (5y and over -> Exclude-Not-Cleaned),
  # plus one NA weight (-> Exclude-Missing). WT rows provide a DOP.
  hc <- data.table(
    id = 1L:6L, subjid = "uer_excl", sex = 0L, param = "HEADCM",
    agedays = c(90L, 365L, 1095L, 1460L, 2000L, 3000L),
    measurement = c(42, 47, 50, 51, 53, 55)
  )
  wt <- data.table(
    id = 101L:106L, subjid = "uer_excl", sex = 0L, param = "WEIGHTKG",
    agedays = hc$agedays,
    measurement = c(6, 10, 14, 16, NA, 24)   # NA -> Exclude-Missing
  )
  combined <- rbind(hc, wt, fill = TRUE)

  res <- suppressWarnings(cleangrowth(
    subjid = combined$subjid, param = combined$param, agedays = combined$agedays,
    sex = combined$sex, measurement = combined$measurement, id = combined$id,
    quietly = TRUE
  ))

  expect_false(any(is.na(res$unit_error_range)))

  not_cleaned <- res[exclude == "Exclude-HC-Out-of-Range"]
  missing <- res[exclude == "Exclude-Missing-Info"]
  expect_true(nrow(not_cleaned) > 0)
  expect_true(nrow(missing) > 0)
  expect_false(any(not_cleaned$unit_error_range))
  expect_false(any(missing$unit_error_range))
})

# ---------------------------------------------------------------------------
# Test 4: .unit_error_range_flag band logic -- WEIGHTKG
# WT mis-record inflates/deflates by the lb<->kg factor (2.2046).
# zn3 = 10, zp3 = 100  ->  imperial band ~[22.0, 220.5], metric band ~[4.5, 45.4],
# plausible carve-out [10, 100] inclusive.
# ---------------------------------------------------------------------------
test_that(".unit_error_range_flag flags WEIGHTKG unit-error magnitudes", {

  f <- growthcleanr:::.unit_error_range_flag

  zn3 <- rep(10, 3); zp3 <- rep(100, 3)
  vals <- c(150,   # in imperial band, outside carve-out -> TRUE
            50,    # inside carve-out [10,100]           -> FALSE
            300)   # outside every band                   -> FALSE
  expect_equal(f(rep("WEIGHTKG", 3), vals, zn3, zp3),
               c(TRUE, FALSE, FALSE))

  # NA endpoints (no reference lookup) -> FALSE
  expect_false(f("WEIGHTKG", 150, NA_real_, NA_real_))
})

# ---------------------------------------------------------------------------
# Test 5: .unit_error_range_flag band logic -- HEIGHTCM
# HT mis-record divides/multiplies by the in<->cm factor (2.54).
# zn3 = 50, zp3 = 200  ->  imperial band ~[19.7, 78.7], metric band ~[127, 508],
# plausible carve-out [50, 200] inclusive.
# ---------------------------------------------------------------------------
test_that(".unit_error_range_flag flags HEIGHTCM unit-error magnitudes", {

  f <- growthcleanr:::.unit_error_range_flag

  zn3 <- rep(50, 4); zp3 <- rep(200, 4)
  vals <- c(250,   # in metric band, outside carve-out  -> TRUE
            100,   # inside carve-out [50,200]           -> FALSE
            600,   # outside every band                  -> FALSE
            30)    # in imperial band, below carve-out   -> TRUE
  expect_equal(f(rep("HEIGHTCM", 4), vals, zn3, zp3),
               c(TRUE, FALSE, FALSE, TRUE))
})

# ---------------------------------------------------------------------------
# Test 6: .compute_unit_error_range honors the exclude carve-out
# ---------------------------------------------------------------------------
test_that(".compute_unit_error_range returns FALSE for excluded/NA-age rows", {

  dt <- data.table(
    param   = c("WEIGHTKG", "HEIGHTCM", "HEADCM",    "WEIGHTKG"),
    sex     = c(0L, 0L, 0L, 0L),
    agedays = c(365L, 365L, 2000L, NA_integer_),
    v       = c(50, 100, 50, 50),
    exclude = c("Include", "Include", "Exclude-Not-Cleaned", "Exclude-Missing")
  )

  out <- growthcleanr:::.compute_unit_error_range(dt, adult_cutpoint = 20)

  expect_length(out, 4L)
  expect_true(is.logical(out))
  expect_false(any(is.na(out)))
  expect_false(out[3])   # Exclude-Not-Cleaned
  expect_false(out[4])   # Exclude-Missing / NA agedays
})

test_that("HEADCM is evaluated through the 5y cleaning range", {

  # Inch-valued HC in a cm field is flagged at 3-5 years as well as under 3
  # (the z3 table covers HC to 1826 days); a plausible HC is not flagged.
  dt <- data.table(
    param   = c("HEADCM", "HEADCM", "HEADCM", "HEADCM"),
    sex     = c(0L, 0L, 0L, 0L),
    agedays = c(700L, 1500L, 1826L, 1500L),
    v       = c(18.5, 19.7, 20, 50),
    exclude = c("Include", "Include", "Include", "Include")
  )

  out <- growthcleanr:::.compute_unit_error_range(dt, adult_cutpoint = 20)

  expect_false(any(is.na(out)))
  expect_true(out[1])
  expect_true(out[2])
  expect_true(out[3])
  expect_false(out[4])
})
