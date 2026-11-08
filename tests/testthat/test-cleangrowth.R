testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Tests for cleangrowth() API: adult algorithm, edge cases, integration.
#
# cleangrowth() returns a data.table.
# =============================================================================

# Helper: merge cleangrowth results back to input by id
run_and_merge <- function(d, ...) {
  res <- cleangrowth(
    subjid = d$subjid,
    param = d$param,
    agedays = d$agedays,
    sex = d$sex,
    measurement = d$measurement,
    id = d$id,
    quietly = TRUE,
    exclude_detail = TRUE,
    ...
  )
  d_out <- copy(d)
  d_out[, gcr_result := res[match(d$id, res$id)]$exclude_detailed]
  return(d_out)
}

test_that("growthcleanr works as expected on adult synthetic data", {

  data("syngrowth", package = "growthcleanr", envir = environment())
  data <- as.data.table(syngrowth)
  expect_equal(77721, data[, .N])
  setkey(data, subjid, param, agedays)

  data_adult <- copy(data[agedays >= 18 * 365.25, ])

  d100 <- copy(data_adult[subjid %in% unique(data_adult[, subjid])[1:100]])
  expect_equal(2023, d100[, .N])

  # Clean sample at cutpoint = 20 (passed explicitly: d100 is all-adult (>= 18y);
  # cutpoint 20 keeps the 18-to-20-year-olds on the child path, to contrast with
  # cd100cp below at cutpoint 18. growthcleanr does not auto-change the cutpoint,
  # so 20 here is the default behavior made explicit -- and it silences the
  # all-adult advisory).
  cd100 <- run_and_merge(d100, adult_cutpoint = 20)

  # Clean with lower cutpoint = 18
  cd100cp <- run_and_merge(copy(d100), adult_cutpoint = 18)

  gcr_result <- function(dt, rowid) {
    as.character(dt[id == rowid]$gcr_result)
  }

  # These results should not change with cutpoint
  expect_equal("Include", gcr_result(cd100, 18695))
  expect_equal("Include", gcr_result(cd100cp, 18695))

  expect_equal("Exclude-Identical", gcr_result(cd100, 167))
  expect_equal("Exclude-Identical", gcr_result(cd100cp, 167))

  expect_equal("Exclude-A-Hard-Limit", gcr_result(cd100, 22009))
  expect_equal("Exclude-A-Hard-Limit", gcr_result(cd100cp, 22009))

  # Results should change due to younger cutpoint
  # With default cutpoint=20, records 18-20 yrs go through child algorithm
  expect_equal("Exclude-Extraneous", gcr_result(cd100, 69740))
  expect_equal("Include", gcr_result(cd100cp, 69740))

  expect_equal("Include", gcr_result(cd100cp, 55171))

  expect_equal("Exclude-C-Hard-Limit", gcr_result(cd100, 25259))
  expect_equal("Exclude-A-Hard-Limit", gcr_result(cd100cp, 25259))

  # Check counts (codes are no longer param-specific; use exact matching)
  catcount <- function(dt, category) {
    dt[gcr_result == category, .N]
  }

  expect_equal(1595, catcount(cd100, "Include"))
  # Exclude-Extraneous is a shared "for all" code (no -A-/-C- prefix even at the
  # Detailed level), so adult extraneous (335) merges with the 17 child-path
  # extraneous rows from the 18-20yr window (cutpoint=20). At cutpoint=18 those
  # rows are adult, so cd100cp stays 357.
  expect_equal(352, catcount(cd100, "Exclude-Extraneous"))
  expect_equal(5, catcount(cd100, "Exclude-C-CF"))
  expect_equal(16, catcount(cd100, "Exclude-A-Hard-Limit"))

  expect_equal(1595, catcount(cd100cp, "Include"))
  expect_equal(357, catcount(cd100cp, "Exclude-Extraneous"))
  expect_equal(0, catcount(cd100cp, "Exclude-C-CF"))
  expect_equal(19, catcount(cd100cp, "Exclude-A-Hard-Limit"))

  # Missing data test for adults
  d5_miss <- copy(data_adult[subjid %in% unique(data[, subjid])[1:5]])
  set.seed(10)
  d5_miss$measurement[sample(seq_len(nrow(d5_miss)), 5)] <- NA

  # adult_cutpoint = 18: this is adult data (>= 18y), so clean the 18-to-20y rows
  # with the adult algorithm (growthcleanr does not auto-route by age; without this
  # the default 20 would child-path them and emit the all-adult advisory). The
  # Missing-Info count below is set at preprocessing and is cutpoint-independent.
  res_miss <- cleangrowth(
    subjid = d5_miss$subjid,
    param = d5_miss$param,
    agedays = d5_miss$agedays,
    sex = d5_miss$sex,
    measurement = d5_miss$measurement,
    id = d5_miss$id,
    adult_cutpoint = 18,
    quietly = TRUE
  )

  expect_equal(sum(res_miss$exclude == "Exclude-Missing-Info"), 5)
})

test_that("growthcleanr works without either adult or pediatric data", {

  data("syngrowth", package = "growthcleanr", envir = environment())
  dt <- as.data.table(syngrowth)

  only_peds <- dt[agedays < 20 * 365.25][1:20]
  only_adult <- dt[agedays >= 20 * 365.25][1:20]
  nobody <- dt[agedays > 120 * 365.25]

  # Testing cleangrowth works without adult data
  peds_res <- cleangrowth(
    subjid = only_peds$subjid,
    param = only_peds$param,
    agedays = only_peds$agedays,
    sex = only_peds$sex,
    measurement = only_peds$measurement,
    id = only_peds$id,
    quietly = TRUE
  )
  expect_equal(nrow(peds_res), nrow(only_peds))

  # Testing cleangrowth works without pediatric data
  adult_res <- cleangrowth(
    subjid = only_adult$subjid,
    param = only_adult$param,
    agedays = only_adult$agedays,
    sex = only_adult$sex,
    measurement = only_adult$measurement,
    id = only_adult$id,
    quietly = TRUE
  )
  expect_equal(nrow(adult_res), nrow(only_adult))

  # Testing cleangrowth works with no data
  if (nrow(nobody) > 0) {
    no_res <- cleangrowth(
      subjid = nobody$subjid,
      param = nobody$param,
      agedays = nobody$agedays,
      sex = nobody$sex,
      measurement = nobody$measurement,
      id = nobody$id,
      quietly = TRUE
    )
    expect_equal(nrow(no_res), nrow(nobody))
  } else {
    # no data at all -- skip
    expect_equal(0, nrow(nobody))
  }
})

# ===========================================================================
# Section: Child-adult spanning subjects
#
# Subjects with measurements on both sides of the adult_cutpoint (default 20y)
# have their data split: pediatric rows go through cleanchild(), adult rows
# through cleanadult(), then results are merged. These tests verify:
#   - No rows lost in the split/merge
#   - Child rows get child codes, adult rows get adult codes
#   - Constructed spanning subjects work correctly
# ===========================================================================

test_that("spanning subject: all rows preserved through child/adult split", {

  # Find a subject in syngrowth that spans the 20-year cutpoint
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  setkey(sg, subjid, param, agedays)
  cutday <- 20 * 365.25

  # Find subjects with both child and adult data
  span_info <- sg[, .(n_child = sum(agedays < cutday),
                       n_adult = sum(agedays >= cutday)), by = subjid]
  span_subjs <- span_info[n_child > 0 & n_adult > 0]$subjid

  expect_true(length(span_subjs) > 0,
              info = "syngrowth should contain spanning subjects")

  # Take a spanning subject with substantial data on both sides
  # Use a571b120 which has 30 child + 23 adult rows
  target <- "a571b120-4ca7-86be-2ee3-a59d52edae59"
  d <- sg[subjid == target]

  n_child <- sum(d$agedays < cutday)
  n_adult <- sum(d$agedays >= cutday)

  expect_true(n_child > 0 & n_adult > 0,
              info = "Target subject should have both child and adult measurements")

  # Add some context subjects for GC to work with
  context_subjs <- unique(sg$subjid)[1:20]
  context_subjs <- context_subjs[context_subjs != target]
  d <- rbind(d, sg[subjid %in% context_subjs[1:10]])

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  # All rows should be present in output
  expect_equal(nrow(res), nrow(d),
               info = "Output should have same number of rows as input")

  # All of the target subject's rows should be present
  target_res <- res[subjid == target]
  target_input <- d[subjid == target]
  expect_equal(nrow(target_res), nrow(target_input),
               info = "Spanning subject should have all rows in output")

  # No NA exclusion codes
  expect_false(any(is.na(target_res$exclude)),
               info = "No NA exclusion codes for spanning subject")
})

test_that("spanning subject: child rows get child codes, adult rows get adult codes", {

  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  setkey(sg, subjid, param, agedays)
  cutday <- 20 * 365.25

  # Use subject with substantial data on both sides
  target <- "a571b120-4ca7-86be-2ee3-a59d52edae59"
  d <- sg[subjid == target]

  # Add context subjects
  context_subjs <- unique(sg$subjid)[1:20]
  context_subjs <- context_subjs[context_subjs != target]
  d <- rbind(d, sg[subjid %in% context_subjs[1:10]])

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE, exclude_detail = TRUE
  )

  target_res <- res[subjid == target]

  # Split by age
  child_res <- target_res[agedays < cutday]
  adult_res <- target_res[agedays >= cutday]

  expect_true(nrow(child_res) > 0 & nrow(adult_res) > 0,
              info = "Should have results on both sides of cutpoint")

  # Child rows (Detailed level): Include, shared codes, or Exclude-C-*
  child_codes <- as.character(child_res$exclude_detailed)
  child_ok <- grepl("^(Include|Exclude-Missing-Info|Exclude-HC-Out-of-Range|Exclude-Identical|Exclude-Extraneous|Exclude-C-)", child_codes)
  expect_true(all(child_ok),
              info = sprintf("Child rows should have child codes, got: %s",
                             paste(unique(child_codes[!child_ok]), collapse = ", ")))

  # Adult rows (Detailed level): Include, shared codes, or Exclude-A-*
  adult_codes <- as.character(adult_res$exclude_detailed)
  adult_ok <- grepl("^(Include|Exclude-Missing-Info|Exclude-Identical|Exclude-Extraneous|Exclude-A-)", adult_codes)
  expect_true(all(adult_ok),
              info = sprintf("Adult rows should have adult codes, got: %s",
                             paste(unique(adult_codes[!adult_ok]), collapse = ", ")))
})

test_that("spanning subject: constructed subject processed correctly across boundary", {

  # Construct a subject with clear growth trajectory across child/adult boundary
  # Male: normal growth from age 18y to 22y (crossing 20y cutpoint at 7305 days)
  d_span <- data.table(
    id = 1:12,
    subjid = "subj_span",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(6570L, 6935L, 7200L,    # HT: child (~18y, ~19y, ~19.7y)
                7400L, 7670L, 8000L,    # HT: adult (~20.3y, ~21y, ~21.9y)
                6570L, 6935L, 7200L,    # WT: child
                7400L, 7670L, 8000L),   # WT: adult
    measurement = c(175.0, 176.0, 176.5,        # HT: adolescent boy near adult height
                    177.0, 177.0, 177.0,        # HT: adult (stable)
                    70.0, 72.0, 73.0,           # WT: normal
                    74.0, 75.0, 76.0)           # WT: normal
  )

  res <- cleangrowth(
    subjid = d_span$subjid, param = d_span$param, agedays = d_span$agedays,
    sex = d_span$sex, measurement = d_span$measurement, id = d_span$id,
    quietly = TRUE, exclude_detail = TRUE
  )

  # All 12 rows should be present
  expect_equal(nrow(res), 12)

  # No NA exclusion codes
  expect_false(any(is.na(res$exclude)))

  cutday <- 20 * 365.25

  # Verify child/adult code assignment
  child_ids <- d_span[agedays < cutday]$id
  adult_ids <- d_span[agedays >= cutday]$id

  child_codes <- as.character(res[id %in% child_ids]$exclude_detailed)
  adult_codes <- as.character(res[id %in% adult_ids]$exclude_detailed)

  # Child codes should not have adult prefix
  expect_false(any(grepl("^Exclude-A-", child_codes)),
               info = "Child-age rows should not get adult exclusion codes")

  # Adult codes should not have child prefix
  expect_false(any(grepl("^Exclude-C-", adult_codes)),
               info = "Adult-age rows should not get child exclusion codes")

  # Normal values on both sides should mostly be Include
  # (small dataset may cause some exclusions, but not all)
  n_include <- sum(res$exclude == "Include")
  expect_true(n_include >= 6,
              info = "Normal spanning subject should have at least half rows included")
})


# ===========================================================================
# Section: Adult integration tests (cleangrowth -> cleanadult)
#
# These tests verify that cleangrowth() correctly preprocesses data,
# passes parameters, and assembles output for adult-age rows. The adult
# unit tests (test-adult-clean.R) call cleanadult() directly -- these
# tests exercise the full pipeline path.
# ===========================================================================

test_that("adult integration: output columns present and correctly typed", {

  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  d_adult <- sg[agedays >= 20 * 365.25]
  d10 <- d_adult[subjid %in% unique(d_adult$subjid)[1:10]]

  res <- cleangrowth(
    subjid = d10$subjid, param = d10$param, agedays = d10$agedays,
    sex = d10$sex, measurement = d10$measurement, id = d10$id,
    quietly = TRUE
  )

  # All rows returned
  expect_equal(nrow(res), nrow(d10))

  # bin_exclude should be present
  expect_true("bin_exclude" %in% names(res),
              info = "bin_exclude column should be present")

  # exclude should be a factor with no NAs
  expect_true(is.factor(res$exclude))
  expect_false(any(is.na(res$exclude)))
})

test_that("adult integration: imperial units processed correctly", {

  # HEIGHTIN and WEIGHTLBS should be converted internally by cleanadult()
  d <- data.table(
    id = 1:6,
    subjid = "imp_adult",
    sex = 0L,
    param = c("HEIGHTIN", "HEIGHTIN", "HEIGHTIN",
              "WEIGHTLBS", "WEIGHTLBS", "WEIGHTLBS"),
    agedays = c(8000L, 8400L, 8800L,
                8000L, 8400L, 8800L),
    measurement = c(70.0, 70.0, 70.5,        # ~178cm
                    170.0, 175.0, 180.0)      # ~77-82kg
  )

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  # All rows returned
  expect_equal(nrow(res), 6)

  # No NA exclusion codes
  expect_false(any(is.na(res$exclude)))

  # Param should be preserved as imperial in output
  expect_true(all(res[id %in% 1:3]$param == "HEIGHTIN"),
              info = "HEIGHTIN param should be preserved in output")
  expect_true(all(res[id %in% 4:6]$param == "WEIGHTLBS"),
              info = "WEIGHTLBS param should be preserved in output")

  # Normal values should be included
  expect_true(all(res$exclude == "Include"),
              info = "Normal imperial adult values should all be included")
})

test_that("adult integration: adult_permissiveness parameter passes through", {

  # 115cm height: PIV at looser (min 120cm) but passes PIV at loosest (min 50cm)
  d <- data.table(
    id = 1:6,
    subjid = "perm_test",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(8000L, 8400L, 8800L,
                8000L, 8400L, 8800L),
    measurement = c(115.0, 170.0, 170.0,
                    70.0, 72.0, 74.0)
  )

  res_looser <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE, adult_permissiveness = "looser", exclude_detail = TRUE
  )
  res_loosest <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE, adult_permissiveness = "loosest", exclude_detail = TRUE
  )

  # 115cm should be PIV at looser (outside 120-230 range)
  expect_equal(as.character(res_looser[id == 1]$exclude_detailed), "Exclude-A-Hard-Limit",
               info = "115cm height should be PIV at looser permissiveness")

  # 115cm should NOT be PIV at loosest (inside 50-244 range)
  expect_true(as.character(res_loosest[id == 1]$exclude_detailed) != "Exclude-A-Hard-Limit",
              info = "115cm height should pass PIV at loosest permissiveness")
})

test_that("adult integration: adult_scale_max_lbs passes through", {

  # 136.078 kg = 300 lbs -- should be flagged with scale_max_lbs=300
  d <- data.table(
    id = 1:6,
    subjid = "cap_test",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(8000L, 8400L, 8800L,
                8000L, 8400L, 8800L),
    measurement = c(170.0, 170.0, 170.5,
                    70.0, 72.0, 136.078)   # 300 lbs
  )

  res_cap <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE, adult_scale_max_lbs = 300, exclude_detail = TRUE
  )
  res_nocap <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE, exclude_detail = TRUE
  )

  # With cap: scale max weight should be flagged
  expect_equal(as.character(res_cap[id == 6]$exclude_detailed), "Exclude-A-Scale-Max",
               info = "136kg (=300lbs) should be flagged with scale_max_lbs=300")

  # Without cap: same weight should not be scale-max flagged
  expect_true(as.character(res_nocap[id == 6]$exclude_detailed) != "Exclude-A-Scale-Max",
              info = "136kg should not be scale-max flagged without cap")
})

test_that("adult integration: adult_overrides forwards an individual sub-parameter", {
  # Raising the adult PIV lower height limit (overall_ht_min) above these heights
  # via adult_overrides should PIV-exclude them; the default looser preset (min 120)
  # keeps them. Confirms adult_overrides reaches cleanadult() through cleangrowth().
  d <- data.table(
    id = 1:6, subjid = "ovr_test", sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(8000L, 8400L, 8800L, 8000L, 8400L, 8800L),
    measurement = c(170.0, 170.0, 170.5, 70.0, 72.0, 71.0))

  res_ovr  <- cleangrowth(d, quietly = TRUE, exclude_detail = TRUE,
                          adult_overrides = list(overall_ht_min = 175))
  res_base <- cleangrowth(d, quietly = TRUE, exclude_detail = TRUE)

  # exclude_detailed is the OUTPUT-mapped code: internal Exclude-A-PIV maps to the
  # Detailed code Exclude-A-Hard-Limit (vignette("exclusion-codes"); map in
  # child_clean.R). (test-adult-clean.R checks the raw internal Exclude-A-PIV via a
  # direct cleanadult() call, which is pre-mapping -- a different level.)
  expect_equal(as.character(res_ovr[id == 1]$exclude_detailed), "Exclude-A-Hard-Limit",
               info = "170cm should hit the adult hard limit (PIV) with overall_ht_min = 175")
  expect_equal(as.character(res_base[id == 1]$exclude), "Include",
               info = "170cm should be kept under the default looser preset (min 120)")
})

test_that("adult_overrides validates its container, names, and values", {
  d <- data.table(id = 1:2, subjid = "v", sex = 0L,
                  param = c("HEIGHTCM", "WEIGHTKG"),
                  agedays = 8000L, measurement = c(170, 70))
  # container: must be a non-empty named list
  expect_error(cleangrowth(d, quietly = TRUE, adult_overrides = 5), "named list")
  expect_error(cleangrowth(d, quietly = TRUE, adult_overrides = list(170)), "named list")
  # names: typo and dedicated-arg names are rejected (with a hint for the latter)
  expect_error(cleangrowth(d, quietly = TRUE, adult_overrides = list(overall_ht_mn = 120)),
               "unrecognized name")
  expect_error(cleangrowth(d, quietly = TRUE, adult_overrides = list(ewma_window = 5)),
               "their own cleangrowth")
  # duplicated name is rejected before it can hit do.call's multiple-match error
  expect_error(cleangrowth(d, quietly = TRUE,
                           adult_overrides = list(overall_ht_min = 120, overall_ht_min = 130)),
               "duplicated name")
  # value type: caught downstream in resolve_permissiveness (adult rows present)
  expect_error(cleangrowth(d, quietly = TRUE, adult_overrides = list(overall_ht_min = NA)),
               "overall_ht_min must be")
})

test_that("adult integration: character/UUID ids preserved", {

  d <- data.table(
    id = c("uuid-a1", "uuid-a2", "uuid-a3", "uuid-b1", "uuid-b2", "uuid-b3"),
    subjid = "subj1",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(8000L, 8400L, 8800L,
                8000L, 8400L, 8800L),
    measurement = c(170.0, 171.0, 170.5,
                    70.0, 72.0, 74.0)
  )

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  # All original ids should be present in output
  expect_true(all(d$id %in% res$id),
              info = "All input ids should be present in output")
  expect_true(all(res$id %in% d$id),
              info = "No extra ids should appear in output")

  # Id type should be preserved as character
  expect_true(is.character(res$id),
              info = "Character ids should remain character in output")
})

test_that("adult integration: spanning subject has correct output structure", {

  # Subject with data on both sides of cutpoint
  d <- data.table(
    id = 1:12,
    subjid = "span_out",
    sex = 0L,
    param = c("HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM", "HEIGHTCM",
              "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(6570L, 6935L, 7200L,    # HT: child (~18y, ~19y, ~19.7y)
                7400L, 7670L, 8000L,    # HT: adult (~20.3y, ~21y, ~21.9y)
                6570L, 6935L, 7200L,    # WT: child
                7400L, 7670L, 8000L),   # WT: adult
    measurement = c(175.0, 176.0, 176.5,
                    177.0, 177.0, 177.0,
                    70.0, 72.0, 73.0,
                    74.0, 75.0, 76.0)
  )

  cutday <- 20 * 365.25

  res <- cleangrowth(
    subjid = d$subjid, param = d$param, agedays = d$agedays,
    sex = d$sex, measurement = d$measurement, id = d$id,
    quietly = TRUE
  )

  child_res <- res[agedays < cutday]
  adult_res <- res[agedays >= cutday]

  # bin_exclude should be present for all rows (derived in postprocessing)
  expect_true(all(!is.na(res$bin_exclude)),
              info = "bin_exclude should be non-NA for all rows")
})



# =============================================================================
# Requested optional columns are always emitted, even when no row reaches the
# child algorithm (adult-only input). Adult rows get the values they get in
# mixed output: cf_status = "Adult", everything else NA.
# =============================================================================

test_that("optional columns are emitted for adult-only input, matching mixed output", {
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  d  <- sg[subjid %in% unique(subjid)[1:60]]
  cut <- 20 * 365.25
  expect_true(d[agedays >= cut, .N] > 0 && d[agedays < cut, .N] > 0)

  opts <- list(full_detail = TRUE, debug = TRUE, recenter_detail = TRUE,
               qm_detail = TRUE, quietly = TRUE)
  mixed <- do.call(cleangrowth, c(list(d), opts))
  adult <- do.call(cleangrowth, c(list(d[agedays >= cut]), opts))
  child <- do.call(cleangrowth, c(list(d[agedays < cut]), opts))

  # same columns, same order, same types
  expect_identical(names(adult), names(mixed))
  expect_identical(names(child), names(mixed))
  expect_identical(vapply(adult, function(x) class(x)[1], ""),
                   vapply(mixed, function(x) class(x)[1], ""))

  # the adult rows carry the same values as in the mixed run
  m_ad <- mixed[agedays >= cut]
  setkey(m_ad, id); setkey(adult, id)
  for (nm in names(mixed)) {
    expect_equal(adult[[nm]], m_ad[[nm]], ignore_attr = TRUE, info = nm)
  }
  expect_true(all(adult$cf_status == "Adult"))
  expect_true(all(is.na(adult$cf_deltaz)))
  expect_true(all(is.na(adult$tbc.sd)))
  expect_true(all(is.na(adult$qm_elig_c_cf)))
})

test_that("adult rows in an adult-only internal batch are labeled cf_status 'Adult'", {
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  d  <- sg[subjid %in% unique(subjid)[1:60]]
  cut <- 20 * 365.25
  ch <- d[agedays < cut]
  ad <- d[agedays >= cut][, subjid := paste0("zz", subjid)]  # sorts last -> own batch
  r <- cleangrowth(rbind(ch, ad), cf_detail = TRUE, batch_size = 30, quietly = TRUE)
  expect_true(all(r[agedays >= cut, cf_status] == "Adult"))
})
