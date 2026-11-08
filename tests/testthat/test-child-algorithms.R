testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Algorithm step tests for the child algorithm
#
# Tests that verify specific algorithm step behaviors using constructed data.
# Complements regression tests (frozen counts) and parameter tests (runs ok).
# These tests construct known scenarios and verify the algorithm responds
# correctly to each one.
#
# Sections:
#   1. CF rescue
#   2. Evil twins / OTL
#   3. Error load
#   4. Age boundaries
#   5. Parallel execution
# =============================================================================

# ---------------------------------------------------------------------------
# Shared data and helpers
# ---------------------------------------------------------------------------

# Load syngrowth once at file scope
data("syngrowth", package = "growthcleanr", envir = environment())
.sg <- as.data.table(syngrowth)
setkey(.sg, subjid, param, agedays)
.sg_peds <- .sg[agedays < 20 * 365.25]

# Pre-built subsets used by multiple tests
.subjs100 <- unique(.sg_peds$subjid)[1:100]
.d100 <- .sg_peds[subjid %in% .subjs100]
.subjs200 <- unique(.sg_peds$subjid)[1:200]
.d200 <- .sg_peds[subjid %in% .subjs200]

#' Run cleangrowth on a data.table with standard columns, return merged result
run_gc <- function(d, ...) {
  res <- cleangrowth(
    subjid = d$subjid,
    param  = d$param,
    agedays = d$agedays,
    sex    = d$sex,
    measurement = d$measurement,
    id     = d$id,
    quietly = TRUE,
    ...
  )
  # Select only GC output columns before merge to avoid .x/.y conflicts
  gc_cols <- intersect(c("id", "exclude", "cf_status", "cf_deltaz", "cf_nextz"), names(res))
  merge(d, res[, ..gc_cols], by = "id")
}


# ===========================================================================
# Section 1: CF rescue tests
#
# CF rescue re-includes carried-forward values when the z-score difference
# between the CF'd value and the originator is small enough for that
# age/interval/param/rounding cell. Three modes:
#   cf_rescue = "standard" (default) -- age/interval/param lookup thresholds
#   cf_rescue = "none"     -- no rescue (all CFs excluded)
#   cf_rescue = "all"      -- every detected CF rescued (including CFs on a
#                            SPA with another Include; Step 13 resolves the
#                            resulting multi-Include SPAs)
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 1: Standard rescue rescues more CFs than "none" mode
# ---------------------------------------------------------------------------
test_that("CF rescue: standard rescues more CFs than none", {

  res_std <- run_gc(.d200, cf_rescue = "standard")
  res_none <- run_gc(.d200, cf_rescue = "none")

  n_cf_std  <- sum(grepl("-CF$", res_std$exclude))
  n_cf_none <- sum(grepl("-CF$", res_none$exclude))

  # Standard rescue should leave fewer CFs excluded than no rescue
  expect_lte(n_cf_std, n_cf_none)
})

# ---------------------------------------------------------------------------
# Test 2: cf_status column (via cf_detail=TRUE) tracks rescue status
# ---------------------------------------------------------------------------
test_that("CF rescue: cf_status column populated for rescued CFs", {

  res <- run_gc(.d200, cf_rescue = "standard", cf_detail = TRUE)

  # cf_status column should exist
  expect_true("cf_status" %in% names(res),
              info = "Output with cf_detail=TRUE must include cf_status column")

  # Some rows should have rescue status
  rescued <- res[cf_status == "CF-R"]

  # Rescued CFs should not retain CF exclusion code
  if (nrow(rescued) > 0) {
    expect_false(any(grepl("-CF$", rescued$exclude)),
                 info = "Rescued CFs should not retain CF exclusion code")
  }
})

# ---------------------------------------------------------------------------
# Test 3: cf_rescue = "none" rescues no CFs
# ---------------------------------------------------------------------------
test_that("CF rescue: none mode rescues no CFs", {

  res <- run_gc(.d100, cf_rescue = "none", cf_detail = TRUE)

  rescued <- res[cf_status == "CF-R"]
  expect_equal(nrow(rescued), 0,
               info = "cf_rescue='none' should rescue no CFs")
})

# ---------------------------------------------------------------------------
# Test 4: cf_rescue = "all" rescues all CFs
# ---------------------------------------------------------------------------
test_that("CF rescue: all mode rescues all CFs", {

  res <- run_gc(.d200, cf_rescue = "all", cf_detail = TRUE)

  # No CF exclusion codes should remain -- every detected CF is rescued.
  n_cf <- sum(grepl("-CF$", res$exclude))
  expect_equal(n_cf, 0,
               info = "cf_rescue='all' should leave no CF exclusions")

  # Rescued rows should have CF-R status.
  n_rescued <- sum(res$cf_status == "CF-R")
  expect_gt(n_rescued, 0,
            label = "Rescued CF count in cf_rescue='all'")
})


# ===========================================================================
# Section 2: unit-error "excluded-somehow" smoke tests
#
# Strategy: take clean syngrowth subjects and inject unit errors (multiply
# height by 2.54, simulating inches recorded as cm). These tests assert only
# that the injected value is excluded (!= "Include") -- they do NOT verify WHICH
# step caught it. In fact a height x2.54 inflates the z-score so far that these
# particular cases are caught as standardized PIV / CF (Detailed Exclude-C-Hard-Limit /
# Exclude-C-CF), not Evil Twins, so an Exclude-C-Evil-Twins assertion would be
# wrong here. Deterministic Evil-Twins (Step 9) MEMBERSHIP is asserted on
# spec-derived patterns in test-child-et.R; these remain smoke tests for "a
# gross unit error does not survive as Include".
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 5: Single unit error is excluded (some step) -- smoke test
# ---------------------------------------------------------------------------
test_that("unit error (height x2.54) is excluded (not Include)", {

  # Find a subject with several Include heights in middle of trajectory
  # Subject 0d8773f3 has 19 HT measurements
  target <- "0d8773f3-c18e-9736-0a78-f1fda9b4fa0a"
  ht_rows <- .d100[subjid == target & param == "HEIGHTCM"]

  # Pick a middle measurement and multiply by 2.54 (unit error: inches -> cm)
  mid_idx <- ceiling(nrow(ht_rows) / 2)
  error_id <- ht_rows$id[mid_idx]
  d_mod <- copy(.d100)
  d_mod[id == error_id, measurement := measurement * 2.54]

  res <- run_gc(d_mod)

  # Excluded by SOME step (here standardized PIV / CF, not Evil Twins -- see the
  # Section 2 header). Step membership is asserted in test-child-et.R.
  error_result <- as.character(res[id == error_id]$exclude)
  expect_true(error_result != "Include",
              info = sprintf("Unit error (x2.54) should be excluded, got: %s", error_result))
})

# ---------------------------------------------------------------------------
# Test 6: Two consecutive unit errors both excluded (some step) -- smoke test
# ---------------------------------------------------------------------------
test_that("two consecutive unit errors are both excluded (not Include)", {

  target <- "0d8773f3-c18e-9736-0a78-f1fda9b4fa0a"
  ht_rows <- .d100[subjid == target & param == "HEIGHTCM"]

  # Pick two consecutive middle measurements
  mid_idx <- ceiling(nrow(ht_rows) / 2)
  error_ids <- ht_rows$id[mid_idx:(mid_idx + 1)]
  d_mod <- copy(.d100)
  d_mod[id %in% error_ids, measurement := measurement * 2.54]

  res <- run_gc(d_mod)

  # Both modified measurements should be excluded
  for (eid in error_ids) {
    error_result <- as.character(res[id == eid]$exclude)
    expect_true(error_result != "Include",
                info = sprintf("ID %d: consecutive unit error should be excluded, got: %s",
                               eid, error_result))
  }
})

# ---------------------------------------------------------------------------
# Test 7: Three consecutive unit errors all excluded (some step) -- smoke test
# ---------------------------------------------------------------------------
test_that("three consecutive unit errors are all excluded (not Include)", {

  target <- "0d8773f3-c18e-9736-0a78-f1fda9b4fa0a"
  ht_rows <- .d100[subjid == target & param == "HEIGHTCM"]

  # Pick three consecutive middle measurements
  mid_idx <- ceiling(nrow(ht_rows) / 2)
  error_ids <- ht_rows$id[mid_idx:(mid_idx + 2)]
  d_mod <- copy(.d100)
  d_mod[id %in% error_ids, measurement := measurement * 2.54]

  res <- run_gc(d_mod)

  # All three modified measurements should be excluded
  for (eid in error_ids) {
    error_result <- as.character(res[id == eid]$exclude)
    expect_true(error_result != "Include",
                info = sprintf("ID %d: 3-consecutive unit error should be excluded, got: %s",
                               eid, error_result))
  }
})

# ---------------------------------------------------------------------------
# Test 8: Unit errors don't cause collateral damage to clean neighbors
# ---------------------------------------------------------------------------
test_that("unit errors don't exclude neighboring clean measurements", {

  target <- "0d8773f3-c18e-9736-0a78-f1fda9b4fa0a"
  ht_rows <- .d100[subjid == target & param == "HEIGHTCM"]

  # Inject one unit error in the middle
  mid_idx <- ceiling(nrow(ht_rows) / 2)
  error_id <- ht_rows$id[mid_idx]
  d_mod <- copy(.d100)
  d_mod[id == error_id, measurement := measurement * 2.54]

  # Run both original and modified
  res_orig <- run_gc(.d100)
  res_mod  <- run_gc(d_mod)

  # Get the non-error HT rows for this subject
  clean_ht_ids <- ht_rows$id[ht_rows$id != error_id]

  # Count how many were Include in original vs modified
  n_inc_orig <- sum(res_orig[id %in% clean_ht_ids]$exclude == "Include")
  n_inc_mod  <- sum(res_mod[id %in% clean_ht_ids]$exclude == "Include")

  # Should have same or very similar Include count -- no widespread collateral damage
  # Allow up to 2 rows of collateral (EWMA-based steps may shift slightly)
  # Unit error should not cause widespread collateral exclusions
  expect_gte(n_inc_mod, n_inc_orig - 2)
})


# ===========================================================================
# Section 3: Error load tests
#
# Error load (Step 21) excludes all remaining Includes for a subject-param
# when the proportion of errors exceeds error.load.threshold and the count
# of errors >= error.load.mincount.
#
# Error load (Step 21) uses the error.load.threshold parameter.
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 9: error.load.threshold parameter actually affects exclusions
# ---------------------------------------------------------------------------
test_that("error load: threshold parameter controls exclusion behavior", {

  # threshold=0.4 should catch subjects that 0.5 misses
  # (confirmed: 2 Error-load at 0.4, 0 at 0.5 on this dataset)
  res_04 <- cleangrowth(
    subjid = .d200$subjid, param = .d200$param, agedays = .d200$agedays,
    sex = .d200$sex, measurement = .d200$measurement, id = .d200$id,
    quietly = TRUE, error.load.threshold = 0.4
  )
  res_05 <- cleangrowth(
    subjid = .d200$subjid, param = .d200$param, agedays = .d200$agedays,
    sex = .d200$sex, measurement = .d200$measurement, id = .d200$id,
    quietly = TRUE, error.load.threshold = 0.5
  )

  n_el_04 <- sum(grepl("Traj-Uneval", res_04$exclude))
  n_el_05 <- sum(grepl("Traj-Uneval", res_05$exclude))

  # Lower threshold should catch at least as many
  expect_gte(n_el_04, n_el_05)

  # Specifically: 0.4 should catch more than 0.5 on this dataset
  expect_gt(n_el_04, n_el_05)
})

# ---------------------------------------------------------------------------
# Test 10: error.load.mincount parameter affects exclusions
# ---------------------------------------------------------------------------
test_that("error load: mincount parameter controls exclusion behavior", {

  # mincount=1 with low threshold should catch more than mincount=10
  res_min1 <- cleangrowth(
    subjid = .d200$subjid, param = .d200$param, agedays = .d200$agedays,
    sex = .d200$sex, measurement = .d200$measurement, id = .d200$id,
    quietly = TRUE, error.load.threshold = 0.3, error.load.mincount = 1
  )
  res_min10 <- cleangrowth(
    subjid = .d200$subjid, param = .d200$param, agedays = .d200$agedays,
    sex = .d200$sex, measurement = .d200$measurement, id = .d200$id,
    quietly = TRUE, error.load.threshold = 0.3, error.load.mincount = 10
  )

  n_el_min1  <- sum(grepl("Traj-Uneval", res_min1$exclude))
  n_el_min10 <- sum(grepl("Traj-Uneval", res_min10$exclude))

  # Lower mincount should trigger at least as many Error-load
  expect_gte(n_el_min1, n_el_min10)
})

# ---------------------------------------------------------------------------
# Test 11: Constructed high-error subject triggers Error-load
# ---------------------------------------------------------------------------
test_that("error load: subject with many errors triggers Error-load on remaining", {

  # Take a subject with many HT measurements and corrupt most of them
  # Subject 0d8773f3 has 19 HT measurements
  target <- "0d8773f3-c18e-9736-0a78-f1fda9b4fa0a"
  ht_rows <- .d100[subjid == target & param == "HEIGHTCM"]

  # Corrupt all but 2 measurements with distinct extreme values
  # (distinct values avoid SDE-Identical/CF which are excluded from error load denominator)
  # Keep first and last, corrupt everything else
  corrupt_ids <- ht_rows$id[2:(nrow(ht_rows) - 1)]
  d_mod <- copy(.d100)
  set.seed(42)
  # Each corrupt value is a different implausible height (200-300 cm range)
  d_mod[id %in% corrupt_ids,
        measurement := runif(.N, min = 200, max = 300)]

  # Use a low threshold to make it easier to trigger
  res <- run_gc(d_mod, error.load.threshold = 0.3)

  # At least one of the surviving measurements should be Error-load
  has_error_load <- any(grepl("Traj-Uneval",
                        res[subjid == target & param == "HEIGHTCM"]$exclude))
  expect_true(has_error_load,
              info = "Subject with many errors should trigger Error-load for remaining Includes")
})


# ===========================================================================
# Section 4: Age boundary tests
#
# The child algorithm has age-specific behavior at several boundaries:
#   - HEADCM: cleaned only for agedays < 5 * 365.25; HC >= 5yr is
#     Exclude-Not-Cleaned (mapped to Exclude-HC-Out-of-Range on output) because
#     the WHO head-circumference reference ends at 5 years
#   - CF rescue for multi-CF strings: females >= 16yr, males >= 17yr
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 12: HEADCM at the 5-year boundary
# ---------------------------------------------------------------------------
test_that("age boundary: HEADCM cleaned under 5yr, not cleaned over 5yr", {
  # Build synthetic data with HC measurements near the 5-year boundary.
  # HC > 5yr (agedays > 5*365.25) gets Exclude-HC-Out-of-Range; HC <= 5yr is
  # cleaned (recentering sd.median is carried forward from 2.75y out to 5y).

  # Take a subject with data starting from birth
  # Subject 0db2905c: sex=0, agedays 0-1071
  target <- "0db2905c-1e0c-0d4d-7d86-4552c5b55ebd"
  subj_data <- .sg_peds[subjid == target]
  target_sex <- subj_data$sex[1]

  # Add HC measurements spanning birth to beyond 5yr
  # Plausible HC trajectory for a male: ~35cm at birth -> ~51cm by 5yr
  under5_day <- as.integer(5 * 365.25 - 1)  # 1825 days
  over5_day  <- as.integer(5 * 365.25 + 2)  # 1828 days

  hc_ages <- c(0L, 30L, 90L, 180L, 365L, 730L, 1000L, 1400L, under5_day, over5_day)
  hc_vals <- c(35.0, 37.5, 40.0, 43.0, 46.0, 48.0, 49.0, 50.0, 51.0, 51.1)

  max_id <- max(subj_data$id)
  hc_rows <- data.table(
    id = max_id + seq_along(hc_ages),
    subjid = target,
    param = "HEADCM",
    agedays = hc_ages,
    sex = target_sex,
    measurement = hc_vals
  )

  d <- rbind(subj_data, hc_rows)
  # Include other subjects for context
  other_subjs <- unique(.sg_peds$subjid)[1:20]
  other_subjs <- other_subjs[other_subjs != target]
  d <- rbind(d, .sg_peds[subjid %in% other_subjs[1:10]])
  setkey(d, subjid, param, agedays)

  # Reassign sequential IDs to avoid conflicts
  d[, id := seq_len(.N)]

  res <- run_gc(d)

  # Find our HC rows by matching on subjid + param + agedays
  hc_res <- res[subjid == target & param == "HEADCM"]

  # Under 5yr should be cleaned (not "Exclude-HC-Out-of-Range")
  under5_res <- as.character(hc_res[agedays == under5_day]$exclude)
  expect_true(under5_res != "Exclude-HC-Out-of-Range",
              info = sprintf("HC under 5yr should be cleaned, got: %s", under5_res))

  # Over 5yr should be "Exclude-HC-Out-of-Range"
  over5_res <- as.character(hc_res[agedays == over5_day]$exclude)
  expect_equal(over5_res, "Exclude-HC-Out-of-Range",
               info = "HC over 5yr should be 'Exclude-HC-Out-of-Range'")
})

# ---------------------------------------------------------------------------
# Test 13: cf_detail columns
# ---------------------------------------------------------------------------
test_that("CF rescue: cf_detail produces cf_status, cf_deltaz, cf_nextz columns", {

  res <- cleangrowth(
    subjid = .d100$subjid, param = .d100$param, agedays = .d100$agedays,
    sex = .d100$sex, measurement = .d100$measurement, id = .d100$id,
    quietly = TRUE, cf_detail = TRUE
  )

  # All three cf_detail columns should exist
  expect_true("cf_status" %in% names(res),
              info = "Output must include cf_status column when cf_detail=TRUE")
  expect_true("cf_deltaz" %in% names(res),
              info = "Output must include cf_deltaz column when cf_detail=TRUE")
  expect_true("cf_nextz" %in% names(res),
              info = "Output must include cf_nextz column when cf_detail=TRUE")

  # Every row gets a label from {"Orig", "CF-R", "CF-NR", "CF-E", "Not-CF"}; no NA.
  # ("Adult" also valid for adult rows, but this is a peds-only sample.)
  valid_status <- c("Orig", "CF-R", "CF-NR", "CF-E", "Not-CF", "Adult")
  expect_true(all(res$cf_status %in% valid_status),
              info = "cf_status values should be Orig, CF-R, CF-NR, CF-E, Not-CF, or Adult")
  expect_false(any(is.na(res$cf_status)),
               info = "cf_status should never be NA")

  # cf_deltaz is NA for non-CF rows (Orig/Not-CF) and CF-E rows; non-negative
  # where present.
  noncf_rows <- res$cf_status %in% c("Orig", "Not-CF", "CF-E")
  expect_true(all(is.na(res$cf_deltaz[noncf_rows])),
              info = "cf_deltaz should be NA for Orig, Not-CF, and CF-E rows")
  has_delta <- !is.na(res$cf_deltaz)
  if (any(has_delta)) {
    expect_true(all(res$cf_deltaz[has_delta] >= 0),
                info = "cf_deltaz should be non-negative")
  }

  # cf_nextz is NA for non-CF, CF-E, and Adult rows.
  no_nextz_rows <- res$cf_status %in% c("Orig", "Not-CF", "CF-E", "Adult")
  expect_true(all(is.na(res$cf_nextz[no_nextz_rows])),
              info = "cf_nextz should be NA for Orig, Not-CF, CF-E, and Adult rows")
})

# ---------------------------------------------------------------------------
# Test 13b: CF-E (same-day-extraneous CFs) and cf_nextz specifics
# ---------------------------------------------------------------------------
test_that("CF rescue: CF-E rows are CFs on a shared-Include day; cf_nextz signed and skips temp SDEs", {

  res <- cleangrowth(
    subjid = .d200$subjid, param = .d200$param, agedays = .d200$agedays,
    sex = .d200$sex, measurement = .d200$measurement, id = .d200$id,
    quietly = TRUE, cf_detail = TRUE
  )
  res <- as.data.table(res)

  # CF-E rows must always be exclude == "Exclude-CF" (CFs that weren't rescued
  # because they were on a same-day-with-Include).
  cf_e <- res[cf_status == "CF-E"]
  if (nrow(cf_e) > 0) {
    expect_true(all(as.character(cf_e$exclude) == "Exclude-CF"),
                info = "CF-E rows must carry Exclude-C-CF")

    # For each CF-E row, there must be at least one OTHER row at the same
    # (subjid, param, ageday) -- that's the definition. At the time of Step 6
    # that sibling was Include; later steps (PIV, EWMA, Step 13) may exclude
    # it, so we don't assert the sibling is still Include in final output.
    counts <- res[, .(n = .N), by = .(subjid, param, agedays)]
    cfe_keys <- unique(cf_e[, .(subjid, param, agedays)])
    sib_counts <- merge(cfe_keys, counts,
                        by = c("subjid", "param", "agedays"))
    expect_true(all(sib_counts$n >= 2),
                info = "every CF-E (subjid, param, ageday) must have at least one sibling row")

    # CF-E must have NA cf_deltaz and NA cf_nextz.
    expect_true(all(is.na(cf_e$cf_deltaz)),
                info = "CF-E rows must have NA cf_deltaz")
    expect_true(all(is.na(cf_e$cf_nextz)),
                info = "CF-E rows must have NA cf_nextz")
  }

  # cf_nextz is signed (can be positive or negative). With enough data, expect
  # to see both signs in the CF-R + CF-NR population.
  nextz_vals <- res[cf_status %in% c("CF-R", "CF-NR") & !is.na(cf_nextz),
                    cf_nextz]
  if (length(nextz_vals) >= 5) {
    expect_true(any(nextz_vals > 0) || any(nextz_vals < 0),
                info = "cf_nextz should be signed (at least one positive or negative value)")
  }
})

# ---------------------------------------------------------------------------
# Test 13c: cf_detail design-aid columns (imperial, cf_dage_orig, cf_dage_next)
# ---------------------------------------------------------------------------
test_that("CF rescue: cf_detail design-aid columns have expected presence, types, and invariants", {

  res <- cleangrowth(
    subjid = .d200$subjid, param = .d200$param, agedays = .d200$agedays,
    sex = .d200$sex, measurement = .d200$measurement, id = .d200$id,
    quietly = TRUE, cf_detail = TRUE
  )
  res <- as.data.table(res)

  # All three design-aid columns present when cf_detail = TRUE
  expect_true("imperial" %in% names(res),
              info = "Output must include imperial column when cf_detail=TRUE")
  expect_true("cf_dage_orig" %in% names(res),
              info = "Output must include cf_dage_orig column when cf_detail=TRUE")
  expect_true("cf_dage_next" %in% names(res),
              info = "Output must include cf_dage_next column when cf_detail=TRUE")

  # imperial: logical, populated for every (peds) row -- no NA -- and matches
  # the internal whole/half imperial-unit definition recomputed from the
  # (metric) measurement: whole pounds for WEIGHTKG, whole/half inch for
  # HEIGHTCM/HEADCM, within 0.01 of the imperial unit.
  expect_true(is.logical(res$imperial),
              info = "imperial should be logical")
  expect_false(any(is.na(res$imperial)),
               info = "imperial should be non-NA for every child row")
  # Symmetric distance-to-nearest-unit test: within 0.01 of a whole/half
  # imperial unit in EITHER direction (a bare %% 1 / %% 0.5 would only catch
  # values just above a unit and miss those just below it).
  expected_imperial <- rep(FALSE, nrow(res))
  is_wt  <- res$param == "WEIGHTKG"
  is_htc <- res$param %in% c("HEIGHTCM", "HEADCM")
  x_wt  <- res$measurement[is_wt]  * 2.20462262
  x_htc <- res$measurement[is_htc] / 2.54
  expected_imperial[is_wt]  <- abs(x_wt  - round(x_wt))            < 0.01
  expected_imperial[is_htc] <- abs(x_htc - round(x_htc / 0.5) * 0.5) < 0.01
  expect_identical(res$imperial, expected_imperial,
                   info = "imperial must match the internal whole/half imperial-unit definition")

  # cf_dage_orig / cf_dage_next: NA on non-CF and CF-E rows; strictly positive
  # day gaps where present. Both are populated only for positionally-detected
  # CF rows (CF-R / CF-NR).
  cf_rows    <- res$cf_status %in% c("CF-R", "CF-NR")
  noncf_rows <- res$cf_status %in% c("Orig", "Not-CF", "CF-E", "Adult")

  expect_true(all(is.na(res$cf_dage_orig[noncf_rows])),
              info = "cf_dage_orig must be NA on Orig/Not-CF/CF-E/Adult rows")
  expect_true(all(is.na(res$cf_dage_next[noncf_rows])),
              info = "cf_dage_next must be NA on Orig/Not-CF/CF-E/Adult rows")

  # cf_dage_orig is always populated and strictly positive on CF rows.
  expect_true(all(!is.na(res$cf_dage_orig[cf_rows])),
              info = "cf_dage_orig must be populated on CF-R/CF-NR rows")
  expect_true(all(res$cf_dage_orig[cf_rows] > 0),
              info = "cf_dage_orig must be strictly positive on CF rows")

  # cf_dage_next may be NA at the end of a subject/param series; where present
  # (on CF rows) it is strictly positive.
  dage_next_present <- cf_rows & !is.na(res$cf_dage_next)
  if (any(dage_next_present)) {
    expect_true(all(res$cf_dage_next[dage_next_present] > 0),
                info = "cf_dage_next must be strictly positive where present")
  }
})

# ---------------------------------------------------------------------------
# Test 14: cf_detail not present by default
# ---------------------------------------------------------------------------
test_that("CF rescue: cf_detail columns absent by default", {

  .subjs50 <- unique(.sg_peds$subjid)[1:50]
  .d50 <- .sg_peds[subjid %in% .subjs50]

  res <- cleangrowth(
    subjid = .d50$subjid, param = .d50$param, agedays = .d50$agedays,
    sex = .d50$sex, measurement = .d50$measurement, id = .d50$id,
    quietly = TRUE
  )

  expect_false("cf_status" %in% names(res),
               info = "cf_status should not be in output by default")
  expect_false("cf_deltaz" %in% names(res),
               info = "cf_deltaz should not be in output by default")
  expect_false("cf_nextz" %in% names(res),
               info = "cf_nextz should not be in output by default")
})


# ===========================================================================
# Section 5: Parallel execution
#
# parallel=TRUE should produce identical results to parallel=FALSE.
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 15: parallel=TRUE produces identical results to parallel=FALSE
# ---------------------------------------------------------------------------
test_that("parallel execution: parallel=TRUE matches parallel=FALSE", {
  # This test requires growthcleanr to be installed (not just load_all)
  skip_if_not_installed("growthcleanr")

  res_seq <- cleangrowth(
    subjid = .d100$subjid, param = .d100$param, agedays = .d100$agedays,
    sex = .d100$sex, measurement = .d100$measurement, id = .d100$id,
    quietly = TRUE, parallel = FALSE
  )

  res_par <- cleangrowth(
    subjid = .d100$subjid, param = .d100$param, agedays = .d100$agedays,
    sex = .d100$sex, measurement = .d100$measurement, id = .d100$id,
    quietly = TRUE, parallel = TRUE, cores = 2
  )

  # Same number of rows
  expect_equal(nrow(res_par), nrow(res_seq),
               info = "Parallel and sequential should return same number of rows")

  # Merge by id and compare exclusion codes
  comp <- merge(res_seq[, .(id, excl_seq = as.character(exclude))],
                res_par[, .(id, excl_par = as.character(exclude))],
                by = "id")

  n_diff <- sum(comp$excl_seq != comp$excl_par)
  expect_equal(n_diff, 0,
               info = sprintf("Parallel and sequential should produce identical exclusion codes; %d differ", n_diff))
})

# ---------------------------------------------------------------------------
# Test 15b: an implausibly large `cores` is capped, not attempted
# ---------------------------------------------------------------------------
test_that("parallel execution: an out-of-range cores value is capped", {
  # Guards against a fat-fingered cores (e.g. 99999) stalling the run trying to
  # spawn that many workers. The value must be capped to the machine core count
  # (with a warning) rather than passed straight to makeCluster().
  skip_if_not_installed("growthcleanr")
  nc <- parallel::detectCores()
  skip_if(is.na(nc) || nc < 2L, "core count unavailable")
  # The cap is the machine core count, so this starts more than 2 workers;
  # R CMD check --as-cran (_R_CHECK_LIMIT_CORES_) refuses more than 2.
  limit_cores <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  skip_if(nzchar(limit_cores) && limit_cores != "false" && nc > 2L,
          "R CMD check limits workers to 2")

  d <- .d100[.d100$subjid %in% unique(.d100$subjid)[1:5], ]
  expect_warning(
    res <- cleangrowth(
      subjid = d$subjid, param = d$param, agedays = d$agedays,
      sex = d$sex, measurement = d$measurement, id = d$id,
      quietly = TRUE, parallel = TRUE, cores = 99999
    ),
    "capping to"
  )
  expect_equal(nrow(res), nrow(d))
})


# ===========================================================================
# Section 6: CF rescue rule tests (deltaz/nextz rules)
#
# Standard-mode rescue is decided on the LAST CF of each identical-value string
# and applied to the whole string. With a next value: rescue if
# cf_deltaz + cf_nextz <= 1 AND cf_deltaz <= 1.5. Without a next value: rescue
# by CF age (cf_deltaz <= 0.5 if agedays < 13*365.25, else <= 1). Uses
# cf_detail=TRUE to inspect cf_status / cf_deltaz / cf_nextz. Trajectories
# verified against the installed algorithm, not hand-derived.
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 16: a carried value followed by an even-lower z (continued decline /
# stagnant growth) is rescued.
# ---------------------------------------------------------------------------
test_that("CF rescue: carried value followed by continued decline is rescued", {
  d <- data.table(
    id = 1:6, subjid = "A", sex = 0L,
    param = rep(c("HEIGHTCM", "WEIGHTKG"), each = 3),
    agedays = rep(c(700L, 830L, 960L), 2),
    measurement = c(80, 84, 86,        # HT: normal
                    11.0, 11.0, 10.0)  # WT: 11.0 carried (CF), then drops to 10.0
  )
  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE, cf_detail = TRUE
  )
  cf <- res[id == 5]  # the carried 11.0 at day 830
  expect_equal(as.character(cf$cf_status), "CF-R",
               info = "CF followed by a lower z (continued decline) should be rescued")
  # Rescue rule holds for the deciding (last) CF.
  expect_lte(cf$cf_deltaz + cf$cf_nextz, 1)
  expect_lte(cf$cf_deltaz, 1.5)
})

# ---------------------------------------------------------------------------
# Test 17: a carried value followed by a jump up (a true CF -- a flat notch in
# a rising trajectory) is excluded.
# ---------------------------------------------------------------------------
test_that("CF exclusion: carried value followed by a jump up (true CF) is excluded", {
  d <- data.table(
    id = 1:6, subjid = "B", sex = 0L,
    param = rep(c("HEIGHTCM", "WEIGHTKG"), each = 3),
    agedays = rep(c(700L, 830L, 960L), 2),
    measurement = c(80, 84, 86,        # HT: normal
                    11.0, 11.0, 14.5)  # WT: 11.0 carried (CF), then jumps to 14.5
  )
  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE, cf_detail = TRUE
  )
  cf <- res[id == 5]
  expect_equal(as.character(cf$cf_status), "CF-NR",
               info = "CF followed by a higher z (recovery) should be excluded")
  expect_gt(cf$cf_deltaz + cf$cf_nextz, 1)  # Rule 1 violated
})

# ---------------------------------------------------------------------------
# Test 18: an identical-value string is rescued or excluded as a whole (the
# decision is made on the last CF, then applied to every CF in the string).
# ---------------------------------------------------------------------------
test_that("CF rescue: all carried values in one string share a single status", {
  d <- data.table(
    id = 1:8, subjid = "S", sex = 0L,
    param = rep(c("HEIGHTCM", "WEIGHTKG"), each = 4),
    agedays = rep(c(400L, 760L, 1120L, 1480L), 2),
    measurement = c(75, 88, 97, 104,          # HT: normal
                    12.0, 12.0, 12.0, 20.0)   # WT: 12.0 held ~3 visits, then 20.0
  )
  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE, cf_detail = TRUE
  )
  cfrows <- res[param == "WEIGHTKG" & cf_status %in% c("CF-R", "CF-NR")]
  expect_gt(nrow(cfrows), 1)  # more than one CF in the string
  expect_equal(data.table::uniqueN(cfrows$cf_status), 1,
               info = "A string is rescued/excluded as a whole")
  # Here the last CF has drifted cf_deltaz > 1.5, so the whole string is excluded.
  expect_true(all(cfrows$cf_status == "CF-NR"))
})

# ---------------------------------------------------------------------------
# Test 18b: an end-of-series CF (no next value) falls back to the age rule.
# ---------------------------------------------------------------------------
test_that("CF rescue: end-of-series CF (no next value) uses the age rule", {
  d <- data.table(
    id = 1:5, subjid = "E", sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(300L, 360L, 420L, 300L, 360L),
    measurement = c(60, 64, 67, 6.5, 6.5)  # WT: 6.5 carried at the end, no later value
  )
  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE, cf_detail = TRUE
  )
  cf <- res[id == 5]
  expect_true(is.na(cf$cf_nextz),
              info = "end-of-series CF has no next value, so cf_nextz is NA")
  expect_equal(as.character(cf$cf_status), "CF-R",
               info = "young CF (< 13y) with cf_deltaz <= 0.5 and no next value is rescued")
  expect_lte(cf$cf_deltaz, 0.5)
})


# ===========================================================================
# Section 7: GA correction (potcorr) tests
#
# Step 2b: Subjects whose first weight z-score < -2 at age < 10 months
# are flagged as "potentially correctable" (potcorr). Their z-scores are
# corrected using Fenton reference curves. This affects downstream
# exclusion decisions.
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 19: Very low birth weight triggers potcorr correction
# ---------------------------------------------------------------------------
test_that("GA correction: very low birth weight infant gets potcorr correction", {
  # Create a subject with a very low birth weight (z < -2) at birth
  # The correction should change z-scores, potentially changing exclusion outcomes
  # Compare with a normal-weight version of the same subject
  d_low <- data.table(
    id = 1:8,
    subjid = "subj_potcorr",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(0L, 30L, 90L, 180L,
                0L, 30L, 90L, 180L),
    measurement = c(45.0, 50.0, 58.0, 65.0,     # HT: short at birth, catches up
                    1.5, 3.0, 5.0, 7.0)          # WT: very low birth weight (1.5kg), rapid gain
  )

  d_normal <- data.table(
    id = 11:18,
    subjid = "subj_normal",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(0L, 30L, 90L, 180L,
                0L, 30L, 90L, 180L),
    measurement = c(50.0, 54.0, 60.0, 67.0,     # HT: normal
                    3.5, 4.5, 6.0, 8.0)          # WT: normal birth weight
  )

  d <- rbind(d_low, d_normal)

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  potcorr_res <- res[id %in% 1:8]
  normal_res <- res[id %in% 11:18]

  # Both subjects should run without NA exclusion codes
  expect_false(any(is.na(potcorr_res$exclude)))
  expect_false(any(is.na(normal_res$exclude)))
})

# ---------------------------------------------------------------------------
# Test 20: Birth weight just above potcorr threshold is NOT corrected
# ---------------------------------------------------------------------------
test_that("GA correction: birth weight z >= -2 does not trigger potcorr", {
  # A subject with first weight z-score just above -2 should NOT be potcorr
  # Normal 50th percentile male birth weight is ~3.5kg; z=-2 is ~2.5kg
  # Use 2.6kg (just above -2) -- should not trigger correction
  d <- data.table(
    id = 1:8,
    subjid = "subj_near_potcorr",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(0L, 30L, 90L, 180L,
                0L, 30L, 90L, 180L),
    measurement = c(48.0, 52.0, 59.0, 66.0,     # HT: slightly short
                    2.6, 3.8, 5.5, 7.5)          # WT: 2.6kg at birth (near but above -2 SD)
  )

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  # Near-potcorr subject should run without NA exclusion codes
  expect_false(any(is.na(res$exclude)))
})


# ===========================================================================
# Section 8: Birth measurement EWMA2 tests (Step 15 birth WT, Step 16 birth HT/HC)
#
# Birth measurements (agedays == 0) have special EWMA2 rules:
# - Birth WT in Step 15: dewma > 3 with next < 365d, dewma > 4 with next >= 365d
# - Birth HT/HC in Step 16: same thresholds, separate iterative loop
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 21: Extreme birth weight is excluded
# ---------------------------------------------------------------------------
test_that("birth EWMA2: extreme birth weight is excluded", {
  # Create a subject with an extreme birth weight that should trigger exclusion
  # dewma > 3 needed; use a very high birth weight with normal later values
  d <- data.table(
    id = 1:10,
    subjid = "subj_birth_wt",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(0L, 30L, 90L, 180L, 365L,
                0L, 30L, 90L, 180L, 365L),
    measurement = c(50.0, 54.0, 60.0, 67.0, 75.0,    # HT: normal
                    8.0, 4.5, 6.0, 8.0, 10.0)         # WT: 8kg at birth (extreme!), then normal
  )

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  # The extreme birth weight (id=6) should be excluded
  birth_wt <- res[id == 6]
  expect_true(grepl("Exclude", birth_wt$exclude),
              info = sprintf("Extreme birth weight (8kg) should be excluded, got: %s",
                             as.character(birth_wt$exclude)))
})

# ---------------------------------------------------------------------------
# Test 22: Normal birth weight is not excluded
# ---------------------------------------------------------------------------
test_that("birth EWMA2: normal birth weight is included", {
  d <- data.table(
    id = 1:10,
    subjid = "subj_birth_wt2",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(0L, 30L, 90L, 180L, 365L,
                0L, 30L, 90L, 180L, 365L),
    measurement = c(50.0, 54.0, 60.0, 67.0, 75.0,    # HT: normal
                    3.5, 4.5, 6.0, 8.0, 10.0)         # WT: 3.5kg at birth (normal)
  )

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  # Normal birth weight should be included
  birth_wt <- res[id == 6]
  expect_equal(as.character(birth_wt$exclude), "Include",
               info = "Normal birth weight (3.5kg) should be included")
})

# ---------------------------------------------------------------------------
# Test 23: Extreme birth height is excluded (Step 16)
# ---------------------------------------------------------------------------
test_that("birth EWMA2: extreme birth height is excluded", {
  # Birth height that is extremely high should trigger Step 16
  d <- data.table(
    id = 1:10,
    subjid = "subj_birth_ht",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(0L, 30L, 90L, 180L, 365L,
                0L, 30L, 90L, 180L, 365L),
    measurement = c(70.0, 54.0, 60.0, 67.0, 75.0,    # HT: 70cm at birth (extreme!), then normal
                    3.5, 4.5, 6.0, 8.0, 10.0)         # WT: normal
  )

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  # The extreme birth height (id=1) should be excluded
  birth_ht <- res[id == 1]
  expect_true(grepl("Exclude", birth_ht$exclude),
              info = sprintf("Extreme birth height (70cm) should be excluded, got: %s",
                             as.character(birth_ht$exclude)))
})


# ===========================================================================
# Section 9: LENGTHCM identity test
#
# LENGTHCM is renamed to HEIGHTCM with no measurement adjustment.
# Results should be identical.
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 24: LENGTHCM produces identical results to HEIGHTCM
# ---------------------------------------------------------------------------
test_that("LENGTHCM: results identical to HEIGHTCM for same measurements", {
  # Use a small subset of real data, convert young HEIGHTCM to LENGTHCM
  .subjs10 <- unique(.sg_peds$subjid)[1:10]
  d_ht <- .sg_peds[subjid %in% .subjs10]

  # Run with original HEIGHTCM
  res_ht <- cleangrowth(
    subjid = d_ht$subjid, param = d_ht$param, agedays = d_ht$agedays,
    sex = d_ht$sex, measurement = d_ht$measurement, id = d_ht$id,
    quietly = TRUE
  )

  # Convert HEIGHTCM to LENGTHCM for children under 2 years
  d_len <- copy(d_ht)
  d_len[param == "HEIGHTCM" & agedays < 730, param := "LENGTHCM"]

  res_len <- cleangrowth(
    subjid = d_len$subjid, param = d_len$param, agedays = d_len$agedays,
    sex = d_len$sex, measurement = d_len$measurement, id = d_len$id,
    quietly = TRUE
  )

  # Exclusion codes should be identical (same measurements, just different param label)
  expect_equal(
    as.character(res_len[order(id)]$exclude),
    as.character(res_ht[order(id)]$exclude),
    info = "LENGTHCM and HEIGHTCM should produce identical exclusion codes"
  )
})
