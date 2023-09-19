testthat::skip_on_cran()

test_that("ext_bmiz produces comparable output to CDC SAS implementation", {
  # run ext_bmiz on input data
  mydatapath <-
    system.file(file.path("extdata", "test_syngrowth_wide.csv.gz"), package = "growthcleanr")
  mydata <- read.csv(gzfile(mydatapath))

  # mydata hasn't changed in dimension
  expect_equal(nrow(mydata), 17191)
  expect_equal(ncol(mydata), 6)

  # run the data through ext_bmiz
  myd_bmi <- ext_bmiz(
    mydata,
    age = "agemos",
    wt = "weight",
    ht = "height",
    bmi = "bmi",
    adjust.integer.age = F
  )

  # load the SAS output
  cdcdatapath <-
    system.file(file.path("extdata", "test_syngrowth_sas_output_compare.csv.gz"),
                package = "growthcleanr")
  cdcdata <- read.csv(gzfile(cdcdatapath))

  # compare dimensions to the output of ext_bmiz
  expect_equal(nrow(myd_bmi), nrow(cdcdata))

  # check that all ids are accounted for
  expect_equal(myd_bmi$id, cdcdata$id)

  # reorder for ease of comparison
  myd_bmi <- myd_bmi[order(myd_bmi$id), ]
  cdcdata <- cdcdata[order(cdcdata$id), ]

  # map of column names between ext_bmiz to cdc
  map_to_cdc <- c(
    "bmiz" = "bmiz",
    "bmip" = "bmipct",
    "waz" = "waz",
    "wp" = "wapct",
    "haz" = "haz",
    "hp" = "hapct",
    "p95" = "bmi95",
    "bmip95" = "bmip95",
    "mod_bmiz" = "mod_bmiz",
    "mod_waz" = "mod_waz",
    "mod_haz" = "mod_haz"
  )

  # check all values within a tolerance of 1e-4
  err_df <- abs(as.data.frame(myd_bmi)[, names(map_to_cdc)] -
                  cdcdata[, unname(map_to_cdc)])
  # 99.99% of values should be within tolerance of 1e-4
  expect_gte(sum(err_df <= 1e-4) / nrow(cdcdata) / length(map_to_cdc) *
               100,
             99.99)

})
