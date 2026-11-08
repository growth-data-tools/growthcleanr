library(growthcleanr)
library(data.table)

# CRAN smoke test (intentionally NOT skip_on_cran): a fast, deterministic
# end-to-end run of cleangrowth() for both the child and adult algorithms on
# tiny inline fixtures. Most of the algorithm test suite is skip_on_cran'd for
# runtime, so few other tests run on CRAN's platforms (test-adult-clean.R,
# test-child-sde-charid.R, test-consolidated-input-warning.R, test-gc-runtrial.R,
# and parts of test-cli.R and test-growthcleanr-app.R do; about 22 seconds in
# all). This guards against platform-specific breakage
# (build/load failure, output-assembly regressions, gross numeric divergence)
# while staying robust: every assertion is on a definitional or unambiguous
# case (missing/not-a-param preprocessing codes, exact same-day duplicates,
# a clearly-normal Include, an absurd population-limit value), never on a
# near-threshold trajectory judgment that could differ across platforms.
# Runs in well under a second.

test_that("child algorithm runs end to end and assigns the definitional codes", {
  child <- data.table(
    id = 1:14,
    subjid = "c1",
    param = c(rep(c("WEIGHTKG", "HEIGHTCM"), 5),  # 5 normal visits (WT+HT)
              "WEIGHTKG",              # measurement 0  -> Missing-Info
              "BMI",                   # not a gc param -> Not-GC-Param
              "WEIGHTKG", "WEIGHTKG"), # same-day duplicate pair
    agedays = c(rep(c(730, 1095, 1460, 1825, 2190), each = 2),
                1200, 1200, 2555, 2555),
    sex = 0,
    measurement = c(12.5, 87, 14.0, 96, 15.5, 103, 17.0, 109, 18.5, 114,
                    0, 16.0, 20.0, 20.0)
  )

  res <- suppressWarnings(cleangrowth(child, quietly = TRUE))

  # Structure: data.table, no rows dropped, default columns present, valid factor.
  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), nrow(child))
  expect_true(all(c("id", "subjid", "param", "agedays", "sex", "measurement",
                    "exclude", "bin_exclude") %in% names(res)))
  expect_true(is.factor(res$exclude))
  expect_false(any(is.na(res$exclude)))

  setkey(res, id)
  # Definitional preprocessing codes.
  expect_equal(as.character(res[.(11)]$exclude), "Exclude-Missing-Info")  # measurement 0
  expect_equal(as.character(res[.(12)]$exclude), "Exclude-Not-GC-Param")  # BMI
  # Output fidelity: the 0 is returned verbatim (not NaN), still flagged.
  expect_equal(res[.(11)]$measurement, 0)

  # The 10 clearly-normal measurements are kept.
  expect_true(all(as.character(res[.(1:10)]$exclude) == "Include"))

  # Same-day exact duplicate: exactly one Identical, one Include.
  dup <- as.character(res[.(13:14)]$exclude)
  expect_equal(sort(dup), c("Exclude-Identical", "Include"))

  # bin_exclude collapses to the two-value view.
  expect_equal(as.character(res[.(1)]$bin_exclude), "Include")
  expect_equal(as.character(res[.(11)]$bin_exclude), "Exclude")

  # mean_ht is returned by default only when the data has adult rows.
  expect_false("mean_ht" %in% names(res))
})

test_that("adult algorithm runs end to end and excludes a population-limit value", {
  adult <- data.table(
    id = 1:8,
    subjid = "a1",
    param = rep(c("WEIGHTKG", "HEIGHTCM"), 4),
    agedays = rep(c(9131, 9497, 9862, 10227), each = 2),  # ~25-28y, all adult
    sex = 1,
    measurement = c(65, 165, 66, 165, 900, 165, 67, 165)  # 900 kg = absurd
  )

  # adult_cutpoint set explicitly: routes every row to the adult algorithm and
  # silences the all-adult advisory.
  res <- suppressWarnings(cleangrowth(adult, adult_cutpoint = 20, quietly = TRUE))

  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), nrow(adult))
  expect_false(any(is.na(res$exclude)))

  setkey(res, id)
  expect_equal(as.character(res[.(5)]$exclude), "Exclude-Hard-Limit")     # 900 kg
  expect_true(all(as.character(res[.(c(1, 3, 7))]$exclude) == "Include"))  # normal weights

  # With adult rows, mean_ht is returned by default: populated for height rows, NA for weight
  # rows. (ids 2,4,6,8 = HEIGHTCM; ids 1,3,5,7 = WEIGHTKG.)
  expect_true("mean_ht" %in% names(res))
  expect_true(all(!is.na(res[.(c(2, 4, 6, 8))]$mean_ht)))  # heights populated
  expect_equal(unique(res[.(c(2, 4, 6, 8))]$mean_ht), 165) # all heights 165 cm
  expect_true(all(is.na(res[.(c(1, 3, 5, 7))]$mean_ht)))   # weights NA

  # mean_ht = FALSE omits it.
  res_no <- suppressWarnings(
    cleangrowth(adult, adult_cutpoint = 20, mean_ht = FALSE, quietly = TRUE))
  expect_false("mean_ht" %in% names(res_no))
})
