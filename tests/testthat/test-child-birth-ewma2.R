testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# Regression coverage for two birth-measurement behaviors that the broader
# regression tests do not exercise:
#
#   Step 15 birth HT/HC. For an HT/HC subject-param with exactly
#   3 valid values where one is a birth (agedays 0), the birth is kept as a
#   normal Step 15 participant (in the EWMA, exclusion-eligible) instead of being
#   stripped -- which previously dropped the whole SP below the processing count
#   and let an extreme non-birth value survive to Include. The classic miss was a
#   wildly high HC a few days after a normal birth HC (the value was the last
#   Include, so Step 11 protected it; Step 16 only scores the birth; HC velocity
#   has no short-interval ceiling) -- so only Step 15, now reachable, catches it.
#
#   Isolated low uncorrected birth-WT guard. A birth weight that is
#   uncorrected, low (tbc.sd < -2), and isolated (next weight >= 2y away) is only
#   excludable if |tbc.sd| + (tbc.sd[next] - tbc.sd) > 7, so a fairly-plausible
#   low birth weight is not dragged out by a distant string of high weights.

# Build all probe subjects in one frame; cleangrowth processes per subject.
birth_probe_data <- function() {
  mk <- function(subjid, param, agedays, meas, sex = 0L) {
    data.table(subjid = subjid, param = param,
               agedays = as.integer(agedays), sex = sex, measurement = meas)
  }
  d <- rbindlist(list(
    # -- Step 15 birth HT/HC --
    # Exactly-3 HC with a birth; extreme high last non-birth (tbc ~ +12).
    mk("C4_hc_extreme", "HEADCM",   c(0, 5, 9),   c(34.5, 35.0, 50.0)),
    # Exactly-3 HC with a birth, all plausible -> nothing excluded.
    mk("C4_hc_ok",      "HEADCM",   c(0, 90, 180), c(34.5, 40.0, 43.5)),
    # Exactly-3 HT with a birth; extreme low last non-birth (tbc ~ -8.8).
    mk("C4_ht_extreme", "HEIGHTCM", c(0, 7, 14),  c(50.0, 51.0, 35.0)),
    # -- Isolated low birth-WT guard --
    # Same high string; shallower birth -> sum <= 7 -> birth kept.
    mk("C2_A", "WEIGHTKG", c(0, 1004, 1095, 1277, 1460), c(2.3, 18.5, 19.5, 20.5, 22.0)),
    # Deeper birth -> sum > 7 -> birth excluded.
    mk("C2_B", "WEIGHTKG", c(0, 1004, 1095, 1277, 1460), c(2.1, 18.5, 19.5, 20.5, 22.0))
  ), use.names = TRUE)
  d[, id := .I]
  d[]
}

run_birth_probe <- function() {
  d <- birth_probe_data()
  res <- as.data.table(cleangrowth(d, quietly = TRUE, exclude_detail = TRUE))
  # Assert on the Detailed code (keeps step-level Exclude-C-Traj); the default
  # `exclude` carries the consolidated Summary code (Exclude-Pattern).
  code <- function(s, p, a) {
    rid <- d[subjid == s & param == p & agedays == a]$id[1]
    as.character(res$exclude_detailed[match(rid, res$id)])
  }
  list(res = res, code = code)
}

test_that("Step 15 birth HT/HC: extreme non-birth value in a 3-value HT/HC SP with a birth is caught", {
  pb <- run_birth_probe(); code <- pb$code

  # The extreme non-birth values are excluded (Step 15 EWMA2). Before the fix,
  # these survived to Include because the SP never entered Step 15.
  expect_equal(code("C4_hc_extreme", "HEADCM", 9), "Exclude-C-Traj")
  expect_equal(code("C4_ht_extreme", "HEIGHTCM", 14), "Exclude-C-Traj")

  # The birth itself is not collateral-excluded by Step 15 (Step 16 owns births).
  expect_equal(code("C4_hc_extreme", "HEADCM", 0), "Include")
  expect_equal(code("C4_ht_extreme", "HEIGHTCM", 0), "Include")
})

test_that("Step 15 birth HT/HC: a plausible 3-value HT/HC SP with a birth is left fully Include", {
  pb <- run_birth_probe(); code <- pb$code
  expect_equal(code("C4_hc_ok", "HEADCM", 0), "Include")
  expect_equal(code("C4_hc_ok", "HEADCM", 90), "Include")
  expect_equal(code("C4_hc_ok", "HEADCM", 180), "Include")
})

test_that("isolated low uncorrected birth WT kept when |z|+rise <= 7, excluded when > 7", {
  pb <- run_birth_probe(); code <- pb$code

  # Subject A (birth tbc ~ -2.17, sum ~ 6.6): guard keeps the birth.
  expect_equal(code("C2_A", "WEIGHTKG", 0), "Include")
  # Subject B (birth tbc ~ -2.81, sum ~ 7.9): same string, deeper birth -> excluded.
  expect_equal(code("C2_B", "WEIGHTKG", 0), "Exclude-C-Traj")

  # The high string itself is plausible and retained in both subjects.
  expect_equal(code("C2_A", "WEIGHTKG", 1004), "Include")
  expect_equal(code("C2_B", "WEIGHTKG", 1004), "Include")
})
