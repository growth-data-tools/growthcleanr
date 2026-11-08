# Tests for the consolidated input warning (Option B) and the three Class-2
# defect warnings it introduced (zero / negative measurement, negative age).
# The funnel buffers per-check bullets and emits ONE warning with up to two
# capped sections ("Unusable data" / "Generated ids"); the lead line counts the
# union of distinct set-aside rows. See R/child_clean.R.

library(growthcleanr)
library(data.table)

# A clean 2-subject dataset with ids, so nothing warns by default. Pediatric
# ages, valid sex, plausible weights/heights.
clean_df <- function() {
  data.table(
    subjid = rep(c("A", "B"), each = 4L),
    param  = rep(c("WEIGHTKG", "WEIGHTKG", "HEIGHTCM", "HEIGHTCM"), 2L),
    agedays = rep(c(365L, 730L), 4L),
    sex    = 0L,
    measurement = c(9, 11, 74, 82, 10, 12, 76, 85),
    id = 1:8
  )
}

wmsg <- function(expr) {
  m <- NULL
  withCallingHandlers(
    suppressMessages(force(expr)),
    warning = function(w) { m <<- c(m, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  paste(m, collapse = "\n")
}

test_that("clean data with ids emits no warning", {
  expect_warning(suppressMessages(cleangrowth(clean_df(), quietly = TRUE)), regexp = NA)
})

test_that("zero measurement warns with per-parameter N/% and is set aside", {
  d <- clean_df()
  d[1, measurement := 0]                       # one WEIGHTKG value = 0
  msg <- wmsg(cleangrowth(d, quietly = TRUE))
  expect_match(msg, "Zero measurements")
  expect_match(msg, "of WEIGHTKG")
  expect_match(msg, "set aside as Exclude-Missing-Info")
  out <- as.data.table(suppressWarnings(suppressMessages(cleangrowth(d, quietly = TRUE))))
  expect_equal(as.character(out[id == 1]$exclude), "Exclude-Missing-Info")
})

test_that("negative measurement warns and is set aside", {
  d <- clean_df()
  d[2, measurement := -5]
  msg <- wmsg(cleangrowth(d, quietly = TRUE))
  expect_match(msg, "Negative measurements")
  expect_match(msg, "below 0")
  out <- as.data.table(suppressWarnings(suppressMessages(cleangrowth(d, quietly = TRUE))))
  expect_equal(as.character(out[id == 2]$exclude), "Exclude-Missing-Info")
})

test_that("negative agedays warns (all-rows denominator) and is set aside", {
  d <- clean_df()
  d[3, agedays := -10L]
  msg <- wmsg(cleangrowth(d, quietly = TRUE))
  expect_match(msg, "Negative age")
  expect_match(msg, "before birth")
  out <- as.data.table(suppressWarnings(suppressMessages(cleangrowth(d, quietly = TRUE))))
  expect_false(as.character(out[id == 3]$exclude) == "Include")
})

test_that("multiple issues collapse into ONE warning with a union lead-line count", {
  d <- clean_df()
  d[1, measurement := 0]                        # zero weight
  d[2, measurement := -3]                       # negative weight
  d[, id := NULL]                               # also drops ids -> Adjusted section
  growthcleanr:::.gc_reset_session_notices()  # ids notice is once per session
  # Exactly one warning is emitted (the consolidated block), not three.
  w <- tryCatch(cleangrowth(d, quietly = TRUE),
                warning = function(w) conditionMessage(w))
  expect_match(w, "2 of 8 measurement\\(s\\).*could not be used")   # union: 2 distinct rows
  expect_match(w, "Unusable data")
  expect_match(w, "Zero measurements")
  expect_match(w, "Negative measurements")
  expect_match(w, "Generated ids")
  expect_match(w, "break ties deterministically")             # the shared id explanation
})

test_that("only a missing id -> generated-id notice only, no scary lead line", {
  d <- clean_df()
  d[, id := NULL]
  growthcleanr:::.gc_reset_session_notices()  # ids notice is once per session
  w <- tryCatch(cleangrowth(d, quietly = TRUE),
                warning = function(w) conditionMessage(w))
  expect_match(w, "Generated ids")
  expect_match(w, "break ties deterministically")
  expect_false(grepl("Unusable data", w))       # no set-aside section
  expect_false(grepl("could not be used", w))   # no set-aside lead line
})

test_that("unrecognized sex bullet leads with the sex_coding fix", {
  d <- clean_df()
  d[, sex := as.character(sex)]                 # avoid data.table int->chr coercion note
  d[subjid == "A", sex := "wombat"]             # unrecognized pediatric sex
  w <- tryCatch(cleangrowth(d, quietly = TRUE),
                warning = function(w) conditionMessage(w))
  expect_match(w, "Unrecognized sex")
  expect_match(w, "sex_coding = list\\(male = ..., female = ...\\)")
})
