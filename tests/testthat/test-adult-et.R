testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Adult Evil Twins (Adult Step 9Wa) -- anchor rule
#
# The anchor rule excludes an inner member of an over-the-limit (OTL) weight
# pair (inner B-C jump > etcap) only when both outer pairs (A-B, C-D) anchor
# within 60% of their cap. Single spikes (interior + end) cede to the EWMA
# steps. Needs >= 4 eligible weights. Selection mirrors the child (windowed
# median + UER3 + median band), with adult CSD z (sex-specific, NA-safe via an
# M/F-averaged fallback) for the |z| tiebreak and the UER3 plausible group.
#
# These tests assert BEHAVIOR (does ET fire / cede), not exact selection among
# multiple candidates, so they are independent of the median-band-width
# constant. All ages are unambiguously adult (> 20 y = 7305 days).
# =============================================================================

# All-adult single-subject weight series (kg).
mk <- function(meas, sex = 0L, base = 8000L) {
  data.table(
    id = seq_along(meas), subjid = "s", sex = sex, param = "WEIGHTKG",
    agedays = base + 365L * (seq_along(meas) - 1L), measurement = meas
  )
}
# Assert on the Detailed code (keeps the step-level Exclude-A-Evil-Twins); the
# default `exclude` carries the consolidated Summary code (Exclude-Extreme).
codes <- function(d, ...) cleangrowth(d, quietly = TRUE, exclude_detail = TRUE, ...)$exclude_detailed

# ---------------------------------------------------------------------------
# Test 1: flanked over-the-limit pair triggers Evil Twins
# ---------------------------------------------------------------------------
test_that("flanked over-the-limit weight pair triggers Evil Twins", {
  # 50,52 | [165,166] | 54,53 : a sustained high run flanked by stable lows.
  ex <- codes(mk(c(50, 52, 165, 166, 54, 53)))
  expect_true(any(ex == "Exclude-A-Evil-Twins"))
  # both extreme highs end up excluded (ET + the EWMA cascade)
  expect_true(all(ex[3:4] != "Include"))
  # the stable lows are kept
  expect_equal(as.character(ex[c(1, 2, 5, 6)]), rep("Include", 4))
})

# ---------------------------------------------------------------------------
# Test 2: single INTERIOR spike is ceded to EWMA (no Evil Twins)
# ---------------------------------------------------------------------------
test_that("single interior weight spike is ceded to EWMA, not Evil Twins", {
  ex <- codes(mk(c(50, 52, 165, 54, 53, 51)))
  expect_false(any(ex == "Exclude-A-Evil-Twins"))
  # the spike is still removed, just by an EWMA step
  expect_true(ex[3] != "Include")
})

# ---------------------------------------------------------------------------
# Test 3: single END spike is ceded (no A before B)
# ---------------------------------------------------------------------------
test_that("single end weight spike is ceded (no anchor before it)", {
  ex <- codes(mk(c(165, 50, 52, 53, 54, 51)))
  expect_false(any(ex == "Exclude-A-Evil-Twins"))
  expect_true(ex[1] != "Include")
})

# ---------------------------------------------------------------------------
# Test 4: count gate -- fewer than 4 eligible weights cannot trigger ET
# ---------------------------------------------------------------------------
test_that("Evil Twins does not fire with fewer than 4 eligible weights", {
  ex <- codes(mk(c(50, 165, 52)))
  expect_false(any(ex == "Exclude-A-Evil-Twins"))
})

# ---------------------------------------------------------------------------
# Test 5: exactly 4 eligible weights CAN trigger ET (A,B,C,D present)
# ---------------------------------------------------------------------------
test_that("Evil Twins can fire with exactly 4 eligible weights", {
  ex <- codes(mk(c(50, 52, 165, 166)))
  expect_true(any(ex == "Exclude-A-Evil-Twins"))
})

# ---------------------------------------------------------------------------
# Test 6: sex = NA still triggers ET via the averaged CSD fallback row
# ---------------------------------------------------------------------------
test_that("Evil Twins fires for sex = NA adults (averaged CSD fallback)", {
  ex <- suppressWarnings(codes(mk(c(50, 52, 165, 166, 54, 53), sex = NA)))
  expect_true(any(ex == "Exclude-A-Evil-Twins"))
})

# ---------------------------------------------------------------------------
# Test 7: adult CSD reference + helpers (sex = NA fallback)
# ---------------------------------------------------------------------------
test_that("adult CSD reference includes an M/F-averaged sex = NA row", {
  ref <- growthcleanr:::.adult_csd_ref()
  expect_true(any(is.na(ref$sex)))
  # WT sex=NA M = mean(74.0, 85.2) = 79.6
  wt_na <- ref[param == "WEIGHTKG" & is.na(sex)]
  expect_equal(wt_na$M, 79.6)
  expect_equal(wt_na$SD_neg, mean(c(13.75, 14.65)))
  expect_equal(wt_na$SD_pos, mean(c(29.9, 29.6)))

  # z at the sex-specific median is 0; NA-sex z defined (not NA)
  expect_equal(growthcleanr:::.adult_csd_z("WEIGHTKG", 0L, 85.2), 0)
  expect_equal(growthcleanr:::.adult_csd_z("WEIGHTKG", 1L, 74.0), 0)
  expect_equal(growthcleanr:::.adult_csd_z("WEIGHTKG", NA, 79.6), 0)
  expect_false(is.na(growthcleanr:::.adult_csd_z("WEIGHTKG", NA, 120)))

  # zn3/zp3 for sex=NA WT = 79.6 -/+ 3*SD
  z3 <- growthcleanr:::.adult_z3("WEIGHTKG", NA)
  expect_equal(z3$zn3, 79.6 - 3 * mean(c(13.75, 14.65)))
  expect_equal(z3$zp3, 79.6 + 3 * mean(c(29.9, 29.6)))
})
