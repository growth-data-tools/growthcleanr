# Regression: non-numeric character ids must not break same-day-extraneous (SDE)
# resolution. The Step 13 SDE keeper columns (keep_id_oneday / keep_id_ewma) are
# by-group assignments whose no-eligible branch returns NA_integer_. That typed
# the column as integer; a non-numeric character id (e.g. a site-prefixed
# "S3-1329-2") was then coerced to integer NA, so neither the keeper nor the
# losers on that same-day group got resolved -- two Includes survived to the
# Step 19 DOP comparison and crashed it ("Supplied 2 items to be assigned to 1
# items of column 'comp_diff'"). Fixed by keying the keeper on internal_id
# (guaranteed-unique integer). See child_clean.R Phase B1/B2.
#
# This fixture (4 subjects, 65 rows, several same-day multiples) crashed pre-fix
# with site-prefixed character ids and cleaned fine with numeric ids.

library(growthcleanr)
library(data.table)

.charid_fixture <- function() {
  subjid <- c("S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S1","S2","S2","S2","S2","S2","S2","S2","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S3","S4","S4","S4","S4","S4","S4","S4","S4","S4","S4","S4")
  param <- c("HEIGHTCM","HEIGHTCM","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","HEIGHTCM","WEIGHTKG","WEIGHTKG","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","HEIGHTCM","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","HEIGHTCM","HEIGHTCM","WEIGHTKG","WEIGHTKG","HEIGHTCM","WEIGHTKG","HEIGHTCM","HEIGHTCM","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","WEIGHTKG","HEIGHTCM","HEIGHTCM","HEIGHTCM","WEIGHTKG","WEIGHTKG","WEIGHTKG","HEIGHTCM","HEIGHTCM","HEIGHTCM","WEIGHTKG","HEIGHTCM","WEIGHTKG","WEIGHTKG","HEIGHTCM","HEIGHTCM","HEIGHTCM","WEIGHTKG")
  agedays <- c(858,858,858,858,1329,1329,2408,2408,2843,2843,2843,2843,2843,2930,2930,3011,3011,3190,3190,4454,4454,2187,2187,2187,2365,2365,2375,2375,411,411,411,411,1571,1571,2323,2323,2323,2323,2323,2463,2463,2636,2636,2636,2636,3344,3344,3344,3697,3697,3697,3697,3697,3697,565,565,565,565,2266,2266,2266,4076,4076,4076,4076)
  measurement <- c(75.6282,75.3225,74.6042,10.144,97.9524,9.4486,100.2041,15.1187,105.6715,0,15.6723,0,15.1822,108.097,14.7688,105.174,17.4304,143.1486,17.1658,133.8269,10.684,96.2492,96.5038,13.6415,100.5275,12.3168,100.6242,0,66.9969,65.52,65.7083,9.3315,86.2363,3.3738,98.5459,98.4496,99.6349,13.5729,14.4806,100.1027,15.1304,0,99.7878,104.5033,28.6748,114.8928,18.3706,16.4133,121.3536,121.4813,132.8187,19.0262,17.5676,20.61,70.0283,68.7645,62.1967,8.0788,96.4141,0,12.6883,127.4776,128.6736,128.031,18.0478)
  data.table(subjid = subjid, param = param, agedays = agedays,
             sex = 0L, measurement = measurement)
}

test_that("non-numeric character ids do not break SDE resolution (Step 19 DOP crash)", {
  d <- .charid_fixture()

  # Site-prefixed, non-numeric character id (the format that triggered the Step 19 crash).
  # suppressWarnings: the fixture carries incidental zero-measurement / unit-error
  # -range informational warnings that are not what this test checks.
  d_char <- copy(d)[, id := paste0(subjid, "-", agedays, "-", seq_len(.N))]
  expect_no_error(out_char <- suppressWarnings(cleangrowth(d_char, quietly = TRUE)))

  # No (subjid, param, agedays) group may retain more than one Include.
  dupes <- out_char[exclude == "Include", .N, by = .(subjid, param, agedays)][N > 1]
  expect_equal(nrow(dupes), 0L)
})

test_that("character-id and numeric-id runs produce identical exclusions", {
  d <- .charid_fixture()
  d_char <- copy(d)[, id := paste0(subjid, "-", agedays, "-", seq_len(.N))]
  d_num  <- copy(d)[, id := seq_len(.N)]

  out_char <- suppressWarnings(cleangrowth(d_char, quietly = TRUE))
  out_num  <- suppressWarnings(cleangrowth(d_num,  quietly = TRUE))

  setorder(out_char, subjid, param, agedays, measurement)
  setorder(out_num,  subjid, param, agedays, measurement)
  expect_identical(as.character(out_char$exclude), as.character(out_num$exclude))
})
