testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Layer 3: Edge case tests for child algorithm
#
# Tests unusual inputs that could cause crashes or incorrect results.
# =============================================================================

# ---------------------------------------------------------------------------
# Shared data: load syngrowth once at file scope
# ---------------------------------------------------------------------------
data("syngrowth", package = "growthcleanr", envir = environment())
.sg <- as.data.table(syngrowth)
setkey(.sg, subjid, param, agedays)
.sg_peds <- .sg[agedays < 20 * 365.25]

# ---------------------------------------------------------------------------
# Test 1: Single subject
# ---------------------------------------------------------------------------
test_that("child algorithm handles single subject", {

  d1 <- .sg_peds[subjid == unique(.sg_peds$subjid)[1]]

  res <- cleangrowth(
    subjid = d1$subjid,
    param = d1$param,
    agedays = d1$agedays,
    sex = d1$sex,
    measurement = d1$measurement,
    id = d1$id,
    quietly = TRUE
  )

  expect_equal(nrow(res), nrow(d1))
  expect_false(any(is.na(res$exclude)))
})

# ---------------------------------------------------------------------------
# Test 2: Subject with only 1 measurement per parameter
# ---------------------------------------------------------------------------
test_that("child algorithm handles subject with single measurement per param", {

  d <- data.table(
    id = 1:2,
    subjid = "subj001",
    sex = 0L,
    param = c("HEIGHTCM", "WEIGHTKG"),
    agedays = c(365, 365),
    measurement = c(75.0, 10.0)
  )

  res <- cleangrowth(
    subjid = d$subjid,
    param = d$param,
    agedays = d$agedays,
    sex = d$sex,
    measurement = d$measurement,
    id = d$id,
    quietly = TRUE
  )

  expect_equal(nrow(res), 2)
  expect_false(any(is.na(res$exclude)))
})

# ---------------------------------------------------------------------------
# Test 3: Subject with only 2 measurements (triggers Step 19 singles/pairs)
# ---------------------------------------------------------------------------
test_that("child algorithm handles subject with exactly 2 measurements per param", {

  d <- data.table(
    id = 1:4,
    subjid = "subj001",
    sex = 1L,
    param = c("HEIGHTCM", "HEIGHTCM", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(365, 730, 365, 730),
    measurement = c(75.0, 85.0, 10.0, 12.0)
  )

  res <- cleangrowth(
    subjid = d$subjid,
    param = d$param,
    agedays = d$agedays,
    sex = d$sex,
    measurement = d$measurement,
    id = d$id,
    quietly = TRUE
  )

  expect_equal(nrow(res), 4)
  expect_false(any(is.na(res$exclude)))
  # Plausible values should be Include
  expect_true(all(res$exclude == "Include"))
})

# ---------------------------------------------------------------------------
# Test 4: All measurements are NA (all Missing)
# ---------------------------------------------------------------------------
test_that("child algorithm errors when a required column is entirely NA", {

  # An entirely-NA required column (here measurement) is a clean early error:
  # there is nothing to clean in that field.
  d <- data.table(
    id = 1:4,
    subjid = "subj001",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(365, 730, 365, 730),
    measurement = NA_real_
  )

  expect_error(
    suppressWarnings(cleangrowth(
      subjid = d$subjid, param = d$param, agedays = d$agedays,
      sex = d$sex, measurement = d$measurement, id = d$id, quietly = TRUE
    )),
    "no non-missing values"
  )
})

# ---------------------------------------------------------------------------
# Test 5: Mixed NA and valid measurements (same subject)
# ---------------------------------------------------------------------------
test_that("child algorithm handles mix of NA and valid measurements", {

  d <- .sg_peds[subjid %in% unique(.sg_peds$subjid)[1:5]]

  # Set half the measurements to NA
  d_half <- copy(d)
  set.seed(123)
  na_rows <- sample(seq_len(nrow(d_half)), nrow(d_half) %/% 2)
  na_ids <- d_half$id[na_rows]
  d_half[na_rows, measurement := NA_real_]

  res <- suppressWarnings(cleangrowth(
    subjid = d_half$subjid,
    param = d_half$param,
    agedays = d_half$agedays,
    sex = d_half$sex,
    measurement = d_half$measurement,
    id = d_half$id,
    quietly = TRUE
  ))

  expect_equal(nrow(res), nrow(d_half))
  # All NA rows should be Missing
  expect_true(all(res[id %in% na_ids]$exclude == "Exclude-Missing-Info"))
  # Non-NA rows should NOT be Missing
  non_na_ids <- d_half$id[!seq_len(nrow(d_half)) %in% na_rows]
  expect_false(any(res[id %in% non_na_ids]$exclude == "Exclude-Missing-Info"))
})

# ---------------------------------------------------------------------------
# Test 6: Same-day identical values (SDE-Identical)
# ---------------------------------------------------------------------------
test_that("child algorithm marks same-day identical measurements", {

  d <- data.table(
    id = 1:6,
    subjid = "subj001",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(365, 365, 730, 365, 365, 730),
    measurement = c(75.0, 75.0, 85.0, 10.0, 10.0, 12.0)
  )

  res <- cleangrowth(
    subjid = d$subjid,
    param = d$param,
    agedays = d$agedays,
    sex = d$sex,
    measurement = d$measurement,
    id = d$id,
    quietly = TRUE
  )

  # Should have SDE-Identical exclusions for the duplicates
  n_sde_identical <- sum(grepl("Identical", res$exclude))
  expect_gt(n_sde_identical, 0,
            label = "Same-day identical values should get SDE-Identical")
})

# ---------------------------------------------------------------------------
# Test 7: Negative agedays marked as Missing
# ---------------------------------------------------------------------------
test_that("child algorithm marks negative agedays as Missing", {

  d <- data.table(
    id = 1:4,
    subjid = "subj001",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(-10, 365, -10, 365),
    measurement = c(50.0, 75.0, 3.5, 10.0)
  )

  res <- cleangrowth(
    subjid = d$subjid,
    param = d$param,
    agedays = d$agedays,
    sex = d$sex,
    measurement = d$measurement,
    id = d$id,
    quietly = TRUE
  )

  expect_equal(nrow(res), 4)
  # Negative agedays should be Missing
  neg_rows <- res[agedays < 0]
  expect_true(all(neg_rows$exclude == "Exclude-Missing-Info"))
})

# ---------------------------------------------------------------------------
# Test 8: HEADCM > 5 years excluded from cleaning; HEADCM <= 5 years cleaned.
# HC is not cleaned past 5 years because the WHO head-circumference reference
# ends at 5 years.
# ---------------------------------------------------------------------------
test_that("child algorithm excludes HEADCM > 5 years; cleans HEADCM <= 5 years", {

  # Build synthetic single-subject dataset with HC spanning the 5y cutoff.
  # Using one subject with plausible HC trajectory to avoid EWMA-based
  # exclusions confounding the age-cutoff check.
  subj <- data.table(
    id       = 1L:12L,
    subjid   = "hc_age_test",
    sex      = 0L,
    param    = "HEADCM",
    # Plausible measurements: ~39 cm at 1mo, growing gradually
    agedays  = c(30L, 90L, 180L, 365L,   # under 5 years
                 730L, 1095L,             # 2y, 3y (<= 5*365.25 = 1826.25, cleaned)
                 1460L, 1826L,            # 4y, ~5y (<= 5 years, cleaned)
                 1827L,                   # just over 5 years (not cleaned)
                 2000L, 2500L, 3000L),    # over 5 years
    measurement = c(39, 42, 44, 47,
                    49, 50,
                    51, 52, 52.5,
                    53, 54, 55)
  )

  # Add WT rows so the subject is not all-HC (needed for DOP checks)
  wt <- data.table(
    id       = 101L:112L,
    subjid   = "hc_age_test",
    sex      = 0L,
    param    = "WEIGHTKG",
    agedays  = subj$agedays,
    measurement = c(4, 6, 8, 10, 12, 14, 16, 18, 18.2, 19, 21, 24)
  )

  combined <- rbind(subj, wt, fill = TRUE)

  res <- suppressWarnings(cleangrowth(
    subjid = combined$subjid,
    param = combined$param,
    agedays = combined$agedays,
    sex = combined$sex,
    measurement = combined$measurement,
    id = combined$id,
    quietly = TRUE
  ))

  expect_equal(nrow(res), nrow(combined))

  hc_res <- res[id %in% subj$id]

  # HC at or under 5 years (agedays <= 5*365.25 = 1826.25): should not be Exclude-HC-Out-of-Range
  hc_under5 <- hc_res[id %in% subj[agedays <= 5 * 365.25]$id]
  expect_false(any(hc_under5$exclude == "Exclude-HC-Out-of-Range"),
               info = "HC <= 5 years should not be Exclude-HC-Out-of-Range")

  # HC over 5 years (agedays > 5*365.25 = 1826.25): should be Exclude-HC-Out-of-Range
  hc_over5 <- hc_res[id %in% subj[agedays > 5 * 365.25]$id]
  if (nrow(hc_over5) > 0) {
    expect_true(all(hc_over5$exclude == "Exclude-HC-Out-of-Range"),
                info = "HC over 5 years should be Exclude-HC-Out-of-Range (WHO HC reference ends at 5y)")
  }
})

# ---------------------------------------------------------------------------
# Test 9: Extreme values get excluded
# ---------------------------------------------------------------------------
test_that("child PIV: single extreme weight excluded as PIV", {

  # 200kg at 4 years is well above the absolute PIV threshold (>35kg for <2y, >600kg for all)
  # but we want to trigger standardized PIV, so use a value that's implausible
  # but below the absolute cap. 200kg at 4y should trigger standardized PIV.
  d <- data.table(
    id = 1:8,
    subjid = rep("subj001", 8),
    sex = rep(0L, 8),
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(365L, 730L, 1095L, 1460L,
                365L, 730L, 1095L, 1460L),
    measurement = c(75.0, 85.0, 95.0, 100.0,       # HT: normal
                    10.0, 12.0, 14.0, 200.0)        # WT: 200kg at 4y = PIV
  )

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  piv_wt <- res[id == 8]
  expect_equal(as.character(piv_wt$exclude), "Exclude-Hard-Limit",
               info = "200kg weight at 4 years should be Exclude-Hard-Limit")

  # Normal values should not be PIV
  expect_false(any(grepl("PIV", res[id != 8]$exclude)),
               info = "Normal values should not be excluded as PIV")
})

test_that("child PIV: single extreme height excluded as PIV", {

  # 300cm exceeds absolute PIV threshold (>244cm)
  d <- data.table(
    id = 1:8,
    subjid = rep("subj001", 8),
    sex = rep(0L, 8),
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(365L, 730L, 1095L, 1460L,
                365L, 730L, 1095L, 1460L),
    measurement = c(75.0, 85.0, 300.0, 100.0,      # HT: 300cm at 3y = PIV
                    10.0, 12.0, 14.0, 16.0)         # WT: normal
  )

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  piv_ht <- res[id == 3]
  expect_equal(as.character(piv_ht$exclude), "Exclude-Hard-Limit",
               info = "300cm height at 3 years should be Exclude-Hard-Limit")

  # Normal values should not be PIV
  expect_false(any(grepl("PIV", res[id != 3]$exclude)),
               info = "Normal values should not be excluded as PIV")
})

test_that("child PIV: single extreme head circumference excluded as PIV", {

  # 80cm HC exceeds absolute PIV threshold (>75cm)
  # HC only cleaned for agedays < 5*365.25 (1826.25)
  d <- data.table(
    id = 1:10,
    subjid = rep("subj001", 10),
    sex = rep(0L, 10),
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG",
              "HEADCM", "HEADCM", "HEADCM", "HEADCM"),
    agedays = c(0L, 365L, 730L,
                0L, 365L, 730L,
                0L, 90L, 365L, 730L),
    measurement = c(50.0, 75.0, 85.0,              # HT: normal
                    3.5, 10.0, 12.0,               # WT: normal
                    35.0, 40.0, 46.0, 80.0)        # HC: 80cm at 2y = PIV
  )

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  piv_hc <- res[id == 10]
  expect_equal(as.character(piv_hc$exclude), "Exclude-Hard-Limit",
               info = "80cm HC at 2 years should be Exclude-Hard-Limit")

  # Normal HC values should not be PIV
  hc_normal <- res[param == "HEADCM" & id != 10]
  expect_false(any(grepl("PIV", hc_normal$exclude)),
               info = "Normal HC values should not be excluded as PIV")
})

# ---------------------------------------------------------------------------
# Test 10: Multiple subjects with varying data density
# ---------------------------------------------------------------------------
test_that("child algorithm handles mix of data-rich and data-sparse subjects", {

  # Get one data-rich subject and create a data-sparse one
  rich_subj <- unique(.sg_peds$subjid)[1]
  d_rich <- .sg_peds[subjid == rich_subj]

  d_sparse <- data.table(
    id = max(.sg_peds$id) + 1:2,
    subjid = "sparse_subj",
    sex = 0L,
    param = c("HEIGHTCM", "WEIGHTKG"),
    agedays = c(365, 365),
    measurement = c(75.0, 10.0)
  )

  combined <- rbind(d_rich, d_sparse, fill = TRUE)

  res <- cleangrowth(
    subjid = combined$subjid,
    param = combined$param,
    agedays = combined$agedays,
    sex = combined$sex,
    measurement = combined$measurement,
    id = combined$id,
    quietly = TRUE
  )

  expect_equal(nrow(res), nrow(combined))
  expect_false(any(is.na(res$exclude)))
})

# ---------------------------------------------------------------------------
# Test 11: Carried-forward values detected
# ---------------------------------------------------------------------------
test_that("child algorithm detects carried-forward values", {

  # Create a subject with obvious carry-forward (exact repeated weight)
  d <- data.table(
    id = 1:8,
    subjid = "subj001",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(365, 730, 1095, 1460,
                365, 730, 1095, 1460),
    measurement = c(75.0, 85.0, 93.0, 100.0,  # height grows normally
                    10.0, 10.0, 10.0, 10.0)     # weight carried forward
  )

  res <- cleangrowth(
    subjid = d$subjid,
    param = d$param,
    agedays = d$agedays,
    sex = d$sex,
    measurement = d$measurement,
    id = d$id,
    quietly = TRUE
  )

  # Some of the repeated weights should be flagged as CF
  n_cf <- sum(grepl("-CF$|-CF-deltaZ", res$exclude))
  expect_gt(n_cf, 0, label = "Obvious carry-forwards should be detected")
})

# ---------------------------------------------------------------------------
# Test 12: Deterministic results (same input -> same output)
# ---------------------------------------------------------------------------
test_that("child algorithm produces deterministic results", {

  d <- .sg_peds[subjid %in% unique(.sg_peds$subjid)[1:20]]

  res1 <- cleangrowth(
    subjid = d$subjid, param = d$param,
    agedays = d$agedays, sex = d$sex,
    measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  res2 <- cleangrowth(
    subjid = d$subjid, param = d$param,
    agedays = d$agedays, sex = d$sex,
    measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  expect_identical(as.character(res1$exclude), as.character(res2$exclude))
})
