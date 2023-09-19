testthat::skip_on_cran()

test_that("growthcleanr works as expected on pediatric synthetic data", {

  # Run cleangrowth() on syngrowth data
  data <- as.data.table(syngrowth)

  # syngrowth hasn't changed in length
  expect_equal(77721, data[, .N])
  setkey(data, subjid, param, agedays)

  # subset to pediatric data
  data_peds <- copy(data[agedays < 20 * 365.25, ])

  # Create small samples; one for NHANES recentering, one for derive. Note that
  # we are not auto-detecting large sizes because running cleangrowth is too
  # long for CRAN test suite.
  #
  # Note that we're creating distinct data tables to avoid accidentally
  # reusing the same by reference.
  d100_nhanes <- as.data.table(data_peds)[subjid %in% unique(data[, subjid])[1:100], ]
  expect_equal(832, d100_nhanes[, .N])

  # And for overriding NHANES/derive option
  d100_derive <- as.data.table(data_peds)[subjid %in% unique(data[, subjid])[1:100], ]
  expect_equal(832, d100_derive[, .N])

  # Clean samples: specify sd.recenter should use NHANES
  cd100_nhanes <-
    d100_nhanes[, gcr_result := cleangrowth(
      subjid,
      param,
      agedays,
      sex,
      measurement,
      sd.recenter = "NHANES"
    )]

  # Specifying "derive" from data instead of NHANES
  cd100_derived <-
    d100_derive[, gcr_result := cleangrowth(
      subjid,
      param,
      agedays,
      sex,
      measurement,
      sd.recenter = "derive"
    )]


  # Spot check individual results
  gcr_result <- function (dt, rowid) {
    return(as.character(dt[id == rowid]$gcr_result))
  }

  # Results for these records should not change w/sample size (NHANES vs. derived)
  expect_equal("Exclude-EWMA-8", gcr_result(cd100_nhanes, 35119))
  expect_equal("Exclude-EWMA-8", gcr_result(cd100_derived, 35119))

  expect_equal("Exclude-Min-Height-Change", gcr_result(cd100_nhanes, 38718))
  expect_equal("Exclude-Min-Height-Change", gcr_result(cd100_derived, 38718))

  expect_equal("Include", gcr_result(cd100_nhanes, 23766))
  expect_equal("Include", gcr_result(cd100_derived, 23766))

  # Results for these records can change w/NHANES vs. derived
  expect_equal("Exclude-Extraneous-Same-Day", gcr_result(cd100_nhanes, 25))
  expect_equal("Include", gcr_result(cd100_derived, 25))

  expect_equal("Include", gcr_result(cd100_nhanes, 40094))
  expect_equal("Exclude-Carried-Forward", gcr_result(cd100_derived, 40094))

  expect_equal("Include", gcr_result(cd100_nhanes, 62606))
  expect_equal("Exclude-Extraneous-Same-Day", gcr_result(cd100_derived, 62606))

  # Check counts of exclusions by category
  catcount <- function (df, category) {
    return(as.numeric(df %>% filter(gcr_result == category) %>% select(n)))
  }

  d100_exclusions <-
    cd100_nhanes %>% group_by(gcr_result) %>% tally(sort = TRUE)
  expect_equal(562, catcount(d100_exclusions, "Include"))
  expect_equal(112, catcount(d100_exclusions, "Exclude-Carried-Forward"))
  expect_equal(3, catcount(d100_exclusions, "Exclude-EWMA-8"))

  d100_derived_exclusions <-
    cd100_derived %>% group_by(gcr_result) %>% tally(sort = TRUE)
  expect_equal(563, catcount(d100_derived_exclusions, "Include"))
  expect_equal(113, catcount(d100_derived_exclusions, "Exclude-Carried-Forward"))
  expect_equal(3, catcount(d100_derived_exclusions, "Exclude-EWMA-8"))

  # also run a test to make sure missing data returns as "missing"
  d5_miss <- as.data.table(data_peds)[subjid %in% unique(data[, subjid])[1:5], ]

  # add missing data randomly for 5 values
  set.seed(10)
  d5_miss$measurement[sample(1:nrow(d5_miss), 5)] <- NA

  d5_miss <-
    d5_miss[, gcr_result := cleangrowth(
      subjid,
      param,
      agedays,
      sex,
      measurement
    )]

  expect_equal(sum(d5_miss$gcr_result == "Missing"), 5)

})

test_that("growthcleanr works as expected on adult synthetic data", {

  # Run cleangrowth() on syngrowth data
  data <- as.data.table(syngrowth)

  # syngrowth hasn't changed in length
  expect_equal(77721, data[, .N])
  setkey(data, subjid, param, agedays)

  # subset to adult data
  data_adult <- copy(data[agedays >= 18 * 365.25, ])

  # Create small sample
  d100 <- as.data.table(data_adult)[subjid %in% unique(data_adult[, subjid])[1:100], ]
  expect_equal(2023, d100[, .N])

  # Clean sample
  cd100 <-
    d100[, gcr_result := cleangrowth(
      subjid,
      param,
      agedays,
      sex,
      measurement
    )]

  # Clean again with lower cutpoint
  cd100cp <-
    copy(d100)[, gcr_result := cleangrowth(
      subjid,
      param,
      agedays,
      sex,
      measurement,
      adult_cutpoint = 18
    )]


  # Spot check individual results
  gcr_result <- function (dt, rowid) {
    return(as.character(dt[id == rowid]$gcr_result))
  }

  # These results should not change with cutpoint
  expect_equal("Include", gcr_result(cd100, 18695))
  expect_equal("Include", gcr_result(cd100cp, 18695))

  expect_equal("Exclude-Adult-Identical-Same-Day", gcr_result(cd100, 167))
  expect_equal("Exclude-Adult-Identical-Same-Day", gcr_result(cd100cp, 167))

  expect_equal("Exclude-Adult-BIV", gcr_result(cd100, 22009))
  expect_equal("Exclude-Adult-BIV", gcr_result(cd100cp, 22009))

  # Results for these records should change due to younger cutpoint
  expect_equal("Exclude-Extraneous-Same-Day", gcr_result(cd100, 69740))
  expect_equal("Exclude-Adult-Extraneous-Same-Day", gcr_result(cd100cp, 69740))

  expect_equal("Exclude-Carried-Forward", gcr_result(cd100, 55171))
  expect_equal("Include", gcr_result(cd100cp, 55171))

  expect_equal("Exclude-Extraneous-Same-Day", gcr_result(cd100, 25259))
  expect_equal("Exclude-Adult-Distinct-3-Or-More", gcr_result(cd100cp, 25259))

  # Check counts of exclusions by category
  catcount <- function (df, category) {
    return(as.numeric(df %>% filter(gcr_result == category) %>% select(n)))
  }

  d100_exclusions <-
    cd100 %>% group_by(gcr_result) %>% tally(sort = TRUE)
  expect_equal(1570, catcount(d100_exclusions, "Include"))
  expect_equal(347, catcount(d100_exclusions, "Exclude-Adult-Extraneous-Same-Day"))
  expect_equal(6, catcount(d100_exclusions, "Exclude-Adult-Distinct-3-Or-More"))
  expect_equal(13, catcount(d100_exclusions, "Exclude-Carried-Forward"))
  expect_equal(13, catcount(d100_exclusions, "Exclude-Adult-BIV"))

  d100cp_exclusions <-
    cd100cp %>% group_by(gcr_result) %>% tally(sort = TRUE)
  expect_equal(1580, catcount(d100cp_exclusions, "Include"))
  expect_equal(368, catcount(d100cp_exclusions, "Exclude-Adult-Extraneous-Same-Day"))
  expect_equal(7, catcount(d100cp_exclusions, "Exclude-Adult-Distinct-3-Or-More"))
  expect_true(is.na(catcount(d100cp_exclusions, "Exclude-Carried-Forward")))
  expect_equal(14, catcount(d100cp_exclusions, "Exclude-Adult-BIV"))

  # also run a test to make sure missing data returns as "missing"
  d5_miss <- as.data.table(data_adult)[subjid %in% unique(data[, subjid])[1:5], ]

  # add missing data randomly for 5 values
  set.seed(10)
  d5_miss$measurement[sample(1:nrow(d5_miss), 5)] <- NA

  d5_miss <-
    d5_miss[, gcr_result := cleangrowth(
      subjid,
      param,
      agedays,
      sex,
      measurement
    )]

  expect_equal(sum(d5_miss$gcr_result == "Missing"), 5)

})

test_that("growthcleanr works without either adult or pediatric data", {
  # creating small only adult and only pediatric data
  # using default cutpoint -- 20
  only_peds <- syngrowth[syngrowth$agedays < 20*365.25,][1:20,]
  only_adult <- syngrowth[syngrowth$agedays >= 20*365.25,][1:20,]
  nobody <- syngrowth[syngrowth$agedays > 120*365.25,]

  # testing cleangrowth works without adult data
  peds_res <- cleangrowth(
    only_peds$subjid,
    only_peds$param,
    only_peds$agedays,
    only_peds$sex,
    only_peds$measurement,
    quietly = TRUE
  )

  expect_equal(length(peds_res), nrow(only_peds))

  # testing cleangrowth works without pediatric data
  adult_res <- cleangrowth(
    only_adult$subjid,
    only_adult$param,
    only_adult$agedays,
    only_adult$sex,
    only_adult$measurement,
    quietly = TRUE
  )

  expect_equal(length(adult_res), nrow(only_adult))

  # testing cleangrowth works with no data
  no_res <- cleangrowth(
    nobody$subjid,
    nobody$param,
    nobody$agedays,
    nobody$sex,
    nobody$measurement,
    quietly = TRUE
  )

  expect_equal(length(no_res), nrow(nobody))

})

test_that("growthcleanr runs preliminary infants algorithm", {

  # Run cleangrowth() on syngrowth data
  data <- as.data.table(syngrowth)

  # syngrowth hasn't changed in length
  expect_equal(77721, data[, .N])
  setkey(data, subjid, param, agedays)

  # subset to pediatric data
  data_peds <- copy(data[agedays < 20 * 365.25, ])

  # Create very small sample; just to test no errors in running
  d5_nhanes <- as.data.table(data_peds)[subjid %in% unique(data[, subjid])[1:5], ]
  expect_equal(42, d5_nhanes[, .N])

  # Clean samples with infants algorithm
  cd5_nhanes <-
    d5_nhanes[, gcr_result := cleangrowth(
      subjid,
      param,
      agedays,
      sex,
      measurement,
      prelim_infants = TRUE
    )]

  expect_equal(length(cd5_nhanes$gcr_result), 42)

  # test that there are a correct number of levels
  expect_equal(length(levels(cd5_nhanes$gcr_result)), 80)
})
