testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Child same-day-identical tiebreak.
#
# When a (subjid, param, agedays) group holds identical values, one is kept
# (Include) and the rest are "Exclude-Identical" (the shared Summary code).
# The keeper is chosen by an AGE-DEPENDENT internal_id rule:
#   agedays == 0 (birth): keep the LOWEST id (the earliest reading, before
#     postnatal fluid shifts);
#   agedays  > 0:          keep the HIGHEST id (the later, more careful reading).
# A regression flipping this would net to a zero count delta and pass every
# frozen-count test, so it is asserted here on exact ids.
#
# The codes/ids below were confirmed by running cleangrowth() on this subject
# before freezing.
# =============================================================================

test_that("birth identical pair keeps the lowest id; non-birth keeps the highest", {
  d <- data.table(
    subjid = "s", param = "WEIGHTKG", sex = 0L,
    agedays     = c(0L,   0L,  180L, 365L, 365L, 730L),
    measurement = c(3.5,  3.5, 7.0,  9.0,  9.0,  11.0),
    id          = c(1L,   2L,  3L,   4L,   5L,   6L)
  )
  res <- as.data.table(suppressWarnings(cleangrowth(d, quietly = TRUE)))
  setkey(res, agedays, id)
  ex <- function(this_id) as.character(res[id == this_id]$exclude)

  # Birth pair (agedays 0): lowest id (1) survives, id 2 is the identical loser.
  expect_equal(ex(1L), "Include")
  expect_equal(ex(2L), "Exclude-Identical")

  # Non-birth pair (agedays 365): highest id (5) survives, id 4 is the loser.
  expect_equal(ex(5L), "Include")
  expect_equal(ex(4L), "Exclude-Identical")

  # Context rows are untouched.
  expect_equal(ex(3L), "Include")
  expect_equal(ex(6L), "Include")
})
