testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Child Step 7 PIV constructed tests.
#
# Reported Summary code for any PIV is "Exclude-Hard-Limit" (population implausible
# value; internal literal "Exclude-C-PIV", Detailed "Exclude-C-Hard-Limit"). These
# tests assert the default `exclude` (Summary) column.
#
# PIV uses the unrecentered WHO/CDC-blended CSD z-score (sd.orig_uncorr). Every
# constructed subject was run through cleangrowth() and the z-scores/codes
# confirmed before freezing here.
# =============================================================================

run_excl <- function(d, ...) {
  res <- as.data.table(suppressWarnings(
    cleangrowth(d, quietly = TRUE, ...)))
  as.character(res$exclude)
}

# ---------------------------------------------------------------------------
# Standardized-PIV low-side cutoffs + the 1-year split.
#
# Tested on HEIGHTCM. (Weight has no standardized low cutoff, because the low-WT
# z asymptotes near -10 and can never reach a useful threshold; HT/HC low z's
# ARE reachable just above their absolute floors, so HT exercises the split on
# the SHIPPED defaults.)
#
# A stably-low HT trajectory holds every value at z ~ -19 (flat in z, so
# trajectory steps never fire), which sits strictly between the default
# cutoffs (piv.z.ht.low.old = -15, piv.z.ht.low.young = -25):
#   ages 200, 300  -> age < 1y  -> governed by piv.z.ht.low.young (-25): Include
#   ages 450, 550  -> age >= 1y -> governed by piv.z.ht.low.old  (-15): PIV
# ---------------------------------------------------------------------------
piv_lo <- data.table(
  subjid = "lo", param = "HEIGHTCM", sex = 0L,
  agedays = c(200L, 300L, 450L, 550L),
  measurement = c(27.0, 29.0, 30.5, 30.0),   # all z ~ -19 (confirmed)
  id = 1:4
)

test_that("default HT low cutoffs (-25/-15) split a z~-19 trajectory at 1y", {
  # The >=1y values are excluded as PIV though they are NOT more extreme than
  # the <1y values (all z ~ -19) -- purely the age split acting on the defaults.
  expect_equal(run_excl(piv_lo),
               c("Include", "Include", "Exclude-Hard-Limit", "Exclude-Hard-Limit"))
})

test_that("piv.z.ht.low.old is plumbed (loosening it rescues the >=1y values)", {
  expect_equal(run_excl(piv_lo, piv.z.ht.low.old = -25), rep("Include", 4))
})

test_that("piv.z.ht.low.young is plumbed (tightening it excludes the <1y values)", {
  expect_equal(run_excl(piv_lo, piv.z.ht.low.young = -15), rep("Exclude-Hard-Limit", 4))
})

# ---------------------------------------------------------------------------
# Absolute PIV birth-specific backstops.
#
# At agedays == 0 the absolute caps tighten: WT > 10.5, HT > 65, HC > 50 are
# each PIV. The same magnitude at a non-birth age is NOT subject to the birth
# cap, isolating the `agedays == 0` guard.
# ---------------------------------------------------------------------------
test_that("birth WT > 10.5 is PIV; the same 12 kg at a non-birth age is Include", {
  d <- data.table(
    subjid = "bw", param = "WEIGHTKG", sex = 0L,
    agedays = c(0L, 180L, 365L, 730L),
    measurement = c(12.0, 8.0, 10.0, 12.0),  # 12 at birth and again at 730d
    id = 1:4
  )
  expect_equal(run_excl(d),
               c("Exclude-Hard-Limit", "Include", "Include", "Include"))
})

test_that("birth HT > 65 is PIV; non-birth heights > 65 are Include", {
  d <- data.table(
    subjid = "bh", param = "HEIGHTCM", sex = 0L,
    agedays = c(0L, 180L, 365L, 730L),
    measurement = c(70.0, 67.0, 75.0, 85.0),  # 75 and 85 (>65) are non-birth
    id = 1:4
  )
  expect_equal(run_excl(d),
               c("Exclude-Hard-Limit", "Include", "Include", "Include"))
})

test_that("birth HC > 50 is PIV; the same 52 cm at a non-birth age is Include", {
  # HC needs a HT companion (its DOP) and stays within the 5y HC cleaning range.
  d <- rbind(
    data.table(subjid = "bc", param = "HEADCM", sex = 0L,
               agedays = c(0L, 180L, 365L, 730L),
               measurement = c(52.0, 45.0, 48.0, 52.0), id = 1:4),
    data.table(subjid = "bc", param = "HEIGHTCM", sex = 0L,
               agedays = c(0L, 180L, 365L, 730L),
               measurement = c(50.0, 67.0, 77.0, 87.0), id = 5:8)
  )
  res <- as.data.table(suppressWarnings(cleangrowth(d, quietly = TRUE)))
  hc <- res[param == "HEADCM"][order(agedays)]
  expect_equal(as.character(hc$exclude),
               c("Exclude-Hard-Limit", "Include", "Include", "Include"))
})

test_that("WT > 40 under 2y is PIV; the same 41 kg at >2y is not PIV", {
  # Offender: 41 kg at 700 days (<2y), surrounded by normal weights. Near 2y the
  # z of 41 kg is ~19, below piv.z.wt.high (22), so only the absolute cap fires.
  offender <- data.table(
    subjid = "wa", param = "WEIGHTKG", sex = 0L,
    agedays = c(500L, 700L, 720L), measurement = c(11.0, 41.0, 12.5), id = 1:3
  )
  expect_equal(run_excl(offender), c("Include", "Exclude-Hard-Limit", "Include"))

  # Strict boundary: exactly 40 kg at 700 days is not a hard limit (it is
  # still implausible, so a later step may exclude it).
  expect_false(run_excl(data.table(subjid = "wc", param = "WEIGHTKG", sex = 0L,
                                   agedays = 700L, measurement = 40.0, id = 1L)) ==
                 "Exclude-Hard-Limit")

  # Same magnitude at >2y: the <2y cap no longer applies, so a stable ~41 kg
  # trajectory is never flagged PIV (no Exclude-Hard-Limit anywhere).
  control <- data.table(
    subjid = "wb", param = "WEIGHTKG", sex = 0L,
    agedays = c(800L, 900L, 1000L, 1100L),
    measurement = c(41.0, 41.5, 42.0, 42.5), id = 1:4
  )
  out <- run_excl(control)
  expect_false(any(out == "Exclude-Hard-Limit"))
  expect_equal(out[1], "Include")
})

# ---------------------------------------------------------------------------
# BMI hard limits (same-day WT x HT pairs), run after the individual limits.
#
# Out of range: BMI < 3 (any age), BMI > 50 (<2y), BMI > 150 (>=2y). A value
# is excluded only if none of its same-day pairings is in range; when neither
# value of an out-of-range pair has an in-range pairing, the one with the
# larger |sd.orig| is excluded. Every z below was confirmed with debug = TRUE.
# ---------------------------------------------------------------------------
bmi_pair <- function(sid, ad, wts, hts) {
  d <- rbind(
    data.table(subjid = sid, param = "WEIGHTKG", sex = 0L,
               agedays = as.integer(ad), measurement = wts),
    data.table(subjid = sid, param = "HEIGHTCM", sex = 0L,
               agedays = as.integer(ad), measurement = hts))
  d[, id := .I]
  res <- as.data.table(suppressWarnings(cleangrowth(d, quietly = TRUE)))
  res[order(param, measurement),
      .(param, measurement, exclude = as.character(exclude))]
}

test_that("BMI < 3: the weight (more extreme z) is excluded, height kept", {
  # 10y: WT 5 kg (z ~ -6.6), HT 138 cm (z ~ -0.1) -> BMI 2.6
  r <- bmi_pair("bf", 3652L, 5.0, 138.0)
  expect_equal(r[param == "WEIGHTKG", exclude], "Exclude-Hard-Limit")
  expect_equal(r[param == "HEIGHTCM", exclude], "Include")
})

test_that("BMI > 50 under 2y: the height (more extreme z) is excluded", {
  # 1y: WT 10 kg (z ~ 0.3), HT 44 cm (z ~ -13.4, inside the -15 HT cutoff) -> BMI 51.7
  r <- bmi_pair("bu", 365L, 10.0, 44.0)
  expect_equal(r[param == "HEIGHTCM", exclude], "Exclude-Hard-Limit")
  expect_equal(r[param == "WEIGHTKG", exclude], "Include")
})

test_that("BMI 50-150 at >=2y is not a BMI hard limit", {
  # 3y: WT 14 kg, HT 48 cm (z ~ -12.9) -> BMI 60.8; the <2y ceiling does not
  # apply, so no hard limit (the height may still be excluded by a later step).
  r <- bmi_pair("bo", 1096L, 14.0, 48.0)
  expect_false(any(r$exclude == "Exclude-Hard-Limit"))
})

test_that("BMI > 150 at >=2y: the height (more extreme z) is excluded", {
  # 10y: WT 40 kg (z ~ 0.9), HT 50 cm (z ~ -13.8) -> BMI 160
  r <- bmi_pair("bh", 3652L, 40.0, 50.0)
  expect_equal(r[param == "HEIGHTCM", exclude], "Exclude-Hard-Limit")
  expect_equal(r[param == "WEIGHTKG", exclude], "Include")
})

test_that("multiple same-day pairings: exclude the value with no in-range pairing", {
  # 1y, one HT 44 cm (z ~ -13.4) and two weights. 10 kg -> BMI 51.7 (out);
  # 9 kg -> BMI 46.5 (in). The height has an in-range pairing, so the 10 kg
  # weight is excluded even though the height has the more extreme z.
  r <- bmi_pair("bm", 365L, c(10.0, 9.0), 44.0)
  expect_equal(r[param == "WEIGHTKG" & measurement == 10.0, exclude], "Exclude-Hard-Limit")
  expect_false(r[param == "WEIGHTKG" & measurement == 9.0, exclude] == "Exclude-Hard-Limit")
  expect_false(r[param == "HEIGHTCM", exclude] == "Exclude-Hard-Limit")
})

test_that("an in-range BMI pair is untouched", {
  r <- bmi_pair("bk", 3652L, 32.0, 138.0)
  expect_equal(r$exclude, c("Include", "Include"))
})
