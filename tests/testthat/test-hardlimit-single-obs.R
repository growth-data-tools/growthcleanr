testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Single-observation hard-limit exclusion (child + adult).
#
# GUARANTEE UNDER TEST: when a subject has exactly ONE observation for a
# parameter and that lone value is beyond the hard limit for its age, the value
# is excluded at the hard-limit stage (Child Step 7 PIV / Adult BIV) and nothing
# is left to pass on to the trajectory steps. With n == 1 there is no trajectory
# context, so this isolates the absolute/standardized hard limit acting on a
# single value -- the degenerate case a real dataset hits when a patient has a
# lone typo (e.g. one 300 cm height).
#
# Reported Summary code (default `exclude` column) for both child PIV
# (internal literal Exclude-C-PIV) and adult BIV (Exclude-A-PIV) is
# "Exclude-Hard-Limit". These tests assert the Summary column via cleangrowth(),
# i.e. the user-facing guarantee end to end. (The lower-level adult tests in
# test-adult-clean.R assert the internal `result`/Exclude-A-PIV directly; this
# file is the cleangrowth() complement and the child coverage.)
#
# Every value below was run through cleangrowth() and its code confirmed before
# being frozen here.
# =============================================================================

# One-row-per-param helper: a subject with a single observation for `param`.
single_obs <- function(param, agedays, meas, sex = 0L) {
  data.table(subjid = "s1", param = param, sex = sex,
             agedays = as.integer(agedays), measurement = meas, id = 1L)
}

excl1 <- function(param, agedays, meas, sex = 0L, ...) {
  d <- single_obs(param, agedays, meas, sex)
  res <- as.data.table(suppressWarnings(cleangrowth(d, quietly = TRUE, ...)))
  # The lone row must survive to output exactly once, with its own verdict.
  testthat::expect_equal(nrow(res), 1L)
  as.character(res$exclude)
}

# ---------------------------------------------------------------------------
# CHILD -- a lone value beyond the hard limit is excluded (per param, per age
# group). Ages chosen to exercise both the birth cap (agedays == 0) and a
# non-birth cap, since the child hard limits differ by age.
# ---------------------------------------------------------------------------
test_that("child WEIGHTKG: lone hard-limit value is excluded (birth + non-birth)", {
  expect_equal(excl1("WEIGHTKG", 0L,    12.0),  "Exclude-Hard-Limit")   # birth cap  (>10.5)
  expect_equal(excl1("WEIGHTKG", 700L,  41.0),  "Exclude-Hard-Limit")   # <2y cap    (>40; z ~19, below the z cap)
  expect_equal(excl1("WEIGHTKG", 1460L, 200.0), "Exclude-Hard-Limit")   # 4y, extreme
})

test_that("child HEIGHTCM: lone hard-limit value is excluded (birth + non-birth)", {
  expect_equal(excl1("HEIGHTCM", 0L,    70.0),  "Exclude-Hard-Limit")   # birth cap  (>65)
  expect_equal(excl1("HEIGHTCM", 1095L, 300.0), "Exclude-Hard-Limit")   # 3y, > absolute cap (244)
})

test_that("child HEADCM: lone hard-limit value is excluded (birth + non-birth)", {
  expect_equal(excl1("HEADCM", 0L,   52.0), "Exclude-Hard-Limit")       # birth cap  (>50)
  expect_equal(excl1("HEADCM", 730L, 80.0), "Exclude-Hard-Limit")       # 2y, > absolute cap (75)
})

test_that("child: a lone in-range value for each param is Included (control)", {
  expect_equal(excl1("WEIGHTKG", 1460L, 16.0), "Include")
  expect_equal(excl1("HEIGHTCM", 1095L, 95.0), "Include")
  expect_equal(excl1("HEADCM",   730L,  48.0), "Include")
})

# ---------------------------------------------------------------------------
# ADULT -- same guarantee end to end through cleangrowth() (agedays >= 20y).
# 10000 days ~= 27.4 y. Adult has WT + HT only.
# ---------------------------------------------------------------------------
test_that("adult WEIGHTKG: lone hard-limit value is excluded (high + low)", {
  expect_equal(excl1("WEIGHTKG", 10000L, 550.0), "Exclude-Hard-Limit")  # > high cap
  expect_equal(excl1("WEIGHTKG", 10000L, 15.0),  "Exclude-Hard-Limit")  # < low cap
})

test_that("adult HEIGHTCM: lone hard-limit value is excluded (high + low)", {
  expect_equal(excl1("HEIGHTCM", 10000L, 260.0), "Exclude-Hard-Limit")  # > high cap (244)
  expect_equal(excl1("HEIGHTCM", 10000L, 100.0), "Exclude-Hard-Limit")  # far-low lone height
})

test_that("adult: a lone in-range value for each param is Included (control)", {
  expect_equal(excl1("WEIGHTKG", 10000L, 70.0),  "Include")
  expect_equal(excl1("HEIGHTCM", 10000L, 170.0), "Include")
})
