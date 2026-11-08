testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Adult-cutpoint advisory
#
# growthcleanr does NOT auto-lower adult_cutpoint for all-adult data -- doing so
# would make a subject's routing depend on the rest of the dataset. adult_cutpoint
# always stays at the caller's (clamped) value. When it is left at the default and
# every measurement with a known age is at age >= 18y, a WARNING advises setting
# adult_cutpoint = 18 to clean the 18-to-20y rows with the adult algorithm.
# Setting adult_cutpoint explicitly (18 or 20) silences the advisory.
#
# The observable lever is the pediatric sex requirement: at the default cutpoint
# (20) a sex-less 18-20y row is pediatric and hits the "no usable sex" stop; only
# an explicit adult_cutpoint = 18 makes those rows adult (adult cleaning needs no
# sex). Because there is no auto-route, the default now stops on such data.
# =============================================================================

# All measurements at age 18.5-19.5y: adult under an 18 cutpoint, pediatric under
# the default 20. sexval defaults to integer NA (a logical NA column would coerce
# a later 0L to FALSE, which is not a recognized sex code).
.adult_1820 <- function(sexval = NA_integer_) data.table(
  subjid = "A", param = rep(c("WEIGHTKG", "HEIGHTCM"), each = 3),
  agedays = as.integer(rep(c(18.5, 19, 19.5) * 365.25, 2)), sex = sexval,
  measurement = c(80, 82, 81, 178, 178, 179), id = 1:6)

# Did the adult-cutpoint advisory warning fire? Collects warnings (even if the
# call also errors) and checks for the advisory's text.
.advised <- function(expr) {
  w <- character(0)
  withCallingHandlers(
    tryCatch(force(expr), error = function(e) e),
    warning = function(cw) { w <<- c(w, conditionMessage(cw)); invokeRestart("muffleWarning") })
  any(grepl("adult_cutpoint = 18", w))
}

test_that("all-adult (>=18y) sex-less data is NOT auto-routed: default cutpoint stops on sex", {
  # No auto-lower: 18-20y rows stay pediatric at the default cutpoint (20), and
  # being sex-less they hit the pediatric sex stop. The advisory fires first.
  d <- .adult_1820(NA_integer_)
  expect_true(.advised(cleangrowth(d, quietly = TRUE)))
  expect_error(suppressWarnings(cleangrowth(d, quietly = TRUE)), "usable value in the sex column")
})

test_that("adult_cutpoint = 18 routes all-adult sex-less data to the adult path (advisory silent)", {
  # The fix the advisory points to: adults need no sex, so the data cleans.
  d <- .adult_1820(NA_integer_)
  expect_false(.advised(cleangrowth(d, adult_cutpoint = 18, quietly = TRUE)))
  r <- suppressWarnings(cleangrowth(d, adult_cutpoint = 18, quietly = TRUE))
  expect_equal(nrow(as.data.table(r)), 6L)
})

test_that("advisory fires for all-adult data left at the default; data cleans via child path when sex present", {
  d <- .adult_1820(0L)
  expect_true(.advised(cleangrowth(d, quietly = TRUE)))
  r <- suppressWarnings(cleangrowth(d, quietly = TRUE))
  expect_equal(nrow(as.data.table(r)), 6L)   # cleaned by the child algorithm, not set aside
})

test_that("an explicit adult_cutpoint silences the advisory (18 or 20)", {
  expect_false(.advised(cleangrowth(.adult_1820(0L), adult_cutpoint = 20, quietly = TRUE)))
  expect_false(.advised(cleangrowth(.adult_1820(0L), adult_cutpoint = 18, quietly = TRUE)))
})

test_that("a single sub-18y measurement suppresses the advisory (data not all-adult)", {
  d <- .adult_1820(0L); d[1, agedays := as.integer(10 * 365.25)]
  expect_false(.advised(cleangrowth(d[], quietly = TRUE)))
})

test_that("NA-age rows are ignored when deciding whether to advise", {
  # One row has no age; every row that DOES have an age is >= 18y, so the advisory
  # still fires (the NA-age row is set aside as missing).
  d <- .adult_1820(0L); d[1, agedays := NA_integer_]
  expect_true(.advised(cleangrowth(d[], quietly = TRUE)))
})

test_that("a subject's routing is independent of the rest of the dataset", {
  # The old auto-default broke this: an 18-20y subject's routing depended on
  # whether EVERY subject was adult (all-adult -> cutpoint 18 -> adult path; mixed
  # -> cutpoint 20 -> child path). Now the cutpoint never changes, so the 18-20y
  # subject is cleaned by the child path either way, with identical per-row results.
  adult <- .adult_1820(0L)
  young <- data.table(subjid = "Y", param = rep(c("WEIGHTKG", "HEIGHTCM"), each = 3),
                      agedays = as.integer(rep(c(5, 6, 7) * 365.25, 2)), sex = 0L,
                      measurement = c(18, 20, 22, 110, 116, 122), id = 7:12)
  r_alone <- suppressWarnings(as.data.table(cleangrowth(copy(adult), quietly = TRUE)))
  r_mixed <- suppressWarnings(as.data.table(cleangrowth(rbind(adult, young), quietly = TRUE)))
  setkey(r_alone, id); setkey(r_mixed, id)
  expect_equal(as.character(r_alone[id %in% 1:6]$exclude),
               as.character(r_mixed[id %in% 1:6]$exclude))
})

test_that("adult HEADCM is Exclude-HC-Out-of-Range and adult LENGTHCM is cleaned as height", {
  yrs <- c(30, 30.5, 31, 31.5, 32, 30, 31, 32, 33)
  a <- data.table::data.table(
    id = 1:9, subjid = "a", sex = 0, agedays = round(yrs * 365.25),
    param = c(rep("WEIGHTKG", 5), "LENGTHCM", "HEIGHTCM", "HEIGHTCM", "HEADCM"),
    measurement = c(80, 81, 80, 82, 81, 175, 120, 175.5, 5))
  r <- cleangrowth(a, quietly = TRUE, internal_detail = TRUE)
  # adult HC is not cleaned: flagged, not returned Include
  expect_equal(as.character(r$exclude[r$param == "HEADCM"]), "Exclude-HC-Out-of-Range")
  expect_false(r$unit_error_range[r$param == "HEADCM"])
  # adult LENGTHCM participates as height: submitted label returned, gc used HEIGHTCM
  expect_equal(r$param[6], "LENGTHCM")
  expect_equal(r$gc_internal_param[6], "HEIGHTCM")
  expect_equal(as.character(r$exclude[6]), "Include")
  # and it takes part in height cleaning (the 120 cm height is excluded against it)
  expect_equal(as.character(r$exclude[7]), "Exclude-Pattern")
  # weights unaffected by the HC row
  expect_true(all(r$exclude[1:5] == "Include"))
})

test_that("missing-column error names the missing role; sex note for adults", {
  d <- data.frame(subjid = 1, param = "WEIGHTKG", agedays = 8000,
                  measurement = 70)
  expect_error(cleangrowth(d, quietly = TRUE),
               "e.g. sex = \"your_sex_column\".*may be blank or NA for adults")
  d2 <- data.frame(subjid = 1, param = "WEIGHTKG", sex = 0, agedays = 8000)
  err <- tryCatch(cleangrowth(d2, quietly = TRUE), error = conditionMessage)
  expect_match(err, "e.g. measurement = \"your_measurement_column\"", fixed = TRUE)
  expect_false(grepl("sex column is required", err))
})

test_that("sex-less pediatric rows suggest adult_cutpoint = 18 only when all pediatric rows are >= 18 y", {
  # Adult rows present: the sex-less pediatric rows are set aside (no stop) and
  # the set-aside warning carries the hint.
  .warns <- function(d) { w <- character(0)
    withCallingHandlers(suppressMessages(cleangrowth(d, quietly = TRUE)),
      warning = function(cw) { w <<- c(w, conditionMessage(cw)); invokeRestart("muffleWarning") })
    w }
  d <- data.frame(subjid = 1:2, param = "WEIGHTKG", sex = NA,
                  agedays = round(c(18.5, 25) * 365.25), measurement = 70)
  w <- .warns(d)
  expect_true(any(grepl("Unrecognized sex.*set adult_cutpoint = 18", w)))
  d$agedays[1] <- round(15 * 365.25)
  w <- .warns(d)
  expect_true(any(grepl("Unrecognized sex", w)))
  expect_false(any(grepl("adult_cutpoint = 18", w)))
  # No adult rows at all: still a stop, with the hint.
  d1 <- d[1, ]; d1$agedays <- round(18.5 * 365.25)
  expect_error(suppressWarnings(cleangrowth(d1, quietly = TRUE)), "set adult_cutpoint = 18")
})
