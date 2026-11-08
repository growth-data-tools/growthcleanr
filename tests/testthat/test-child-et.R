# Child Evil Twins (Step 9) anchor-rule regression fixture.
#
# Locks the anchor-rule behavior on a deterministic, single-parameter synthetic
# fixture (inst/testdata/child-et-fixture.csv, built by
# data-raw/build_child_et_fixture.R). Each subject is a TERM trajectory
# (ctbc.sd == tbc.sd) generated to hit chosen recentered z-scores at young ages.
#
# The fixture's `expected_et` column is which rows the ET step marks
# Exclude-C-Evil-Twins (spec-derived). Rows not marked ET may still be excluded by later (EWMA/velocity)
# steps, so we assert ET MEMBERSHIP only (== / != "Exclude-C-Evil-Twins"), never
# the full exclusion code of the non-ET rows.
#
# Patterns covered (see fixture `pattern` column):
#   interior_step / interior_step_hc -> exactly 1 ET (the cascade excludes one
#       high inner value; EWMA gets the rest)
#   sustained_3_plateau              -> exactly 2 ET (cascade peels two edges)
#   single_end_spike                 -> 0 ET (no D anchor after the spike)
#   embedded_single                  -> 0 ET (one outer anchor fails)
#   transient_plateau                -> 0 ET (no inner jump; not handled by Evil Twins by design)
#   gate_3_eligible                  -> 0 ET (< 4 eligible -> count gate)

skip_on_cran()
library(growthcleanr)
library(data.table)

et_fixture <- function() {
  f <- system.file("testdata", "child-et-fixture.csv", package = "growthcleanr")
  testthat::skip_if(!nzchar(f), "Child ET fixture not installed")
  data.table::fread(f)
}

run_et <- function(fx) {
  res <- cleangrowth(
    fx[, .(id, subjid, param, agedays, sex, measurement)],
    quietly = TRUE, exclude_detail = TRUE
  )
  # Assert on the Detailed code (keeps the step-level Exclude-C-Evil-Twins);
  # the default `exclude` carries the consolidated Summary code (Exclude-Extreme).
  as.character(res$exclude_detailed)[match(fx$id, res$id)]
}

test_that("Child ET fixture: Evil-Twins membership matches expected", {
  fx <- et_fixture()
  got <- run_et(fx)

  is_et_got <- got == "Exclude-C-Evil-Twins"
  is_et_exp <- fx$expected_et == "Exclude-C-Evil-Twins"

  # Main lock: ET membership matches the spec-derived expectation per row.
  expect_equal(is_et_got, is_et_exp,
               info = paste0("ET membership mismatch at rows: ",
                             paste(fx$id[is_et_got != is_et_exp], collapse = ", ")))
})

test_that("Child ET fixture: per-pattern invariants", {
  fx <- et_fixture()
  got <- run_et(fx)
  fx[, got_exclude := got]

  n_et <- function(pat) fx[pattern == pat, sum(got_exclude == "Exclude-C-Evil-Twins")]

  # Caught patterns
  expect_equal(n_et("interior_step"),       1L, info = "WT interior step: 1 ET")
  expect_equal(n_et("interior_step_hc"),    1L, info = "HC interior step: 1 ET")
  expect_equal(n_et("sustained_3_plateau"), 2L, info = "3-plateau: 2 ET (cascade)")

  # Ceded / gated patterns: zero ET
  for (pat in c("single_end_spike", "embedded_single",
                "transient_plateau", "gate_3_eligible")) {
    expect_equal(n_et(pat), 0L, info = paste0(pat, ": 0 ET"))
  }
})

test_that("Child ET fixture: HC interior step is cleaned, not out-of-range", {
  # All HC fixture rows are at agedays <= 480 (well within the 5y HC cleaning
  # window), so none should be Exclude-HC-Out-of-Range; the step's high values
  # are ET/EWMA, not age-excluded.
  fx <- et_fixture()
  got <- run_et(fx)
  hc_idx <- which(fx$param == "HEADCM")
  expect_false(any(got[hc_idx] == "Exclude-HC-Out-of-Range"),
               info = "HC fixture rows are all <= 3y and should be cleaned")
})
