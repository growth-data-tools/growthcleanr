testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# visit_date + dob -> agedays
#
# cleangrowth() can compute `agedays` from a date of birth (`dob`) + a visit
# date (`visit_date`) instead of a precomputed agedays column. This is a
# standalone preprocessing step on the long table (before the funnel / setkey),
# so everything downstream sees a normal numeric agedays.
#
# Route rule: an `agedays` column takes precedence whenever present; dates are
# used only when `agedays` is absent, or when age_source = "dates" forces them.
# Dates may be R Date objects or text in any accepted format (year-first numeric,
# English spelled-month, or numeric day/month with date_order -- see the
# expanded-format tests below).
# A subject with >1 distinct dob is set aside (Exclude-Missing-Info, with a
# warning); other subjects clean. NA date -> NA agedays ->
# Exclude-Missing-Info; visit_date < dob -> negative agedays (handled as today).
# =============================================================================

# Single-subject HEIGHTCM slice (same shape as test-data-frame-api.R)
.mk_df <- function() {
  data.table(
    id          = 1:7,
    subjid      = "subj001",
    sex         = 0L,
    agedays     = c(183L, 190L, 222L, 270L, 292L, 355L, 367L),
    param       = "HEIGHTCM",
    measurement = c(68.77, 64.05, 68.13, 76.00, 69.98, 71.23, 76.13)
  )
}

# Add Date columns that encode the SAME agedays as the numeric column:
# a fixed dob per (all) subjects, visit_date = dob + agedays.
.add_dates <- function(d, dob = as.Date("2015-01-01")) {
  d <- copy(d)
  d[, dob := dob]
  d[, visit_date := dob + agedays]
  d[]
}

# Multi-subject pediatric slice from syngrowth for the equivalence keystone
data("syngrowth", package = "growthcleanr", envir = environment())
.sg <- as.data.table(syngrowth)
setkey(.sg, subjid, param, agedays)
.sg_peds <- .sg[agedays < 20 * 365.25]
.sg_small <- .sg_peds[subjid %in% unique(.sg_peds$subjid)[1:5]]
if (!"id" %in% names(.sg_small)) .sg_small[, id := seq_len(.N)]

# ---------------------------------------------------------------------------
# Test 1 (KEYSTONE): dates route produces identical exclude to the agedays route
# ---------------------------------------------------------------------------
test_that("dob + visit_date yields identical results to precomputed agedays", {
  d_age  <- copy(.sg_small)
  d_date <- .add_dates(d_age)
  d_date[, agedays := NULL]  # force the date route (no agedays column present)

  res_age  <- cleangrowth(d_age, quietly = TRUE)
  res_date <- cleangrowth(d_date, dob = "dob", visit_date = "visit_date",
                          quietly = TRUE)

  # same input row order both ways -> compare exclude directly
  expect_identical(as.character(res_date$exclude),
                   as.character(res_age$exclude))
  # computed agedays matches the original numeric agedays
  expect_equal(res_date$agedays, d_age$agedays)
})

# ---------------------------------------------------------------------------
# Test 2: agedays = floor(visit_date - dob) in whole days
# ---------------------------------------------------------------------------
test_that("agedays is computed as whole days between dob and visit_date", {
  d <- .add_dates(.mk_df())
  d[, agedays := NULL]
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_equal(res$agedays, c(183L, 190L, 222L, 270L, 292L, 355L, 367L))
  expect_type(res$agedays, "integer")
})

# ---------------------------------------------------------------------------
# Test 3: auto-resolve prefers an agedays column; ignoring dates is silent
# ---------------------------------------------------------------------------
test_that("agedays column takes precedence over dates (auto), no warning", {
  d <- .add_dates(.mk_df())
  # Corrupt the dates so the date route would give different ages; agedays wins.
  d[, visit_date := dob + agedays + 1000L]
  # Supplying both an agedays column and dates is common and expected;
  # growthcleanr silently uses agedays and emits no age-source warning.
  expect_warning(
    res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = NA
  )
  expect_equal(res$agedays, .mk_df()$agedays)
})

# ---------------------------------------------------------------------------
# Test 4: age_source = "dates" forces the date route even when agedays present
# ---------------------------------------------------------------------------
test_that("age_source = 'dates' forces date-based ages", {
  d <- .add_dates(.mk_df())  # has BOTH an agedays column and dates
  d[, visit_date := dob + agedays + 10L]  # dates now imply agedays + 10
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date",
                     age_source = "dates", quietly = TRUE)
  # The user's own age column comes back as submitted; the ages growthcleanr
  # computed from the dates (and cleaned on) are in gc_internal_agedays.
  expect_equal(res$agedays, .mk_df()$agedays)
  expect_equal(res$gc_internal_agedays, .mk_df()$agedays + 10L)
  ref <- cleangrowth(copy(d)[, agedays := NULL], dob = "dob",
                     visit_date = "visit_date", quietly = TRUE)
  expect_equal(as.character(res$exclude), as.character(ref$exclude))
  # same with a remapped age column, and under checkpointing
  d2 <- copy(d); setnames(d2, "agedays", "age_d")
  res2 <- cleangrowth(d2, agedays = "age_d", dob = "dob", visit_date = "visit_date",
                      age_source = "dates", quietly = TRUE)
  expect_equal(res2$age_d, .mk_df()$agedays)
  expect_equal(res2$gc_internal_agedays, .mk_df()$agedays + 10L)
  res3 <- suppressWarnings(cleangrowth(d, dob = "dob", visit_date = "visit_date",
                      age_source = "dates", quietly = TRUE,
                      checkpoint_dir = file.path(tempdir(), "gc-age-src"),
                      checkpoint_size = 1L))
  expect_equal(res3, res, ignore_attr = TRUE)
  # without an age column, the computed ages are the agedays column itself
  expect_false("gc_internal_agedays" %in% names(ref))
})

# ---------------------------------------------------------------------------
# Test 5: a subject with >1 distinct dob is set aside, with a warning naming it
# ---------------------------------------------------------------------------
test_that("inconsistent dob within a subject sets that subject aside and names it", {
  d1 <- .mk_df()
  d2 <- copy(d1)[, `:=`(subjid = "subj002", id = id + 7L)]
  d <- .add_dates(rbind(d1, d2))
  d[, agedays := NULL]
  d[4, dob := dob + 5L]  # one row with a different dob for subj001
  expect_warning(
    res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "Conflicting dates of birth.*subjid subj001"
  )
  bad <- res$subjid == "subj001"
  expect_true(all(as.character(res$exclude[bad]) == "Exclude-Missing-Info"))
  expect_true(all(is.na(res$agedays[bad])))
  # the other subject cleans normally
  expect_false(any(as.character(res$exclude[!bad]) == "Exclude-Missing-Info"))

  # every subject conflicting: nothing to clean -> stop that names the cause
  d1c <- .add_dates(.mk_df()); d1c[, agedays := NULL]; d1c[4, dob := dob + 5L]
  expect_error(
    suppressWarnings(cleangrowth(d1c, dob = "dob", visit_date = "visit_date", quietly = TRUE)),
    regexp = "more than one distinct dob"
  )
})

# ---------------------------------------------------------------------------
# Test 6: ISO 8601 (YYYY-MM-DD) text columns give identical results to Date
# ---------------------------------------------------------------------------
test_that("ISO YYYY-MM-DD text columns match Date columns", {
  d_date <- copy(.sg_small)
  d_date <- .add_dates(d_date)
  d_date[, agedays := NULL]
  d_chr  <- copy(d_date)
  d_chr[, dob := as.character(dob)]                 # as.character(Date) -> ISO
  d_chr[, visit_date := as.character(visit_date)]
  expect_type(d_chr$dob, "character")

  res_date <- cleangrowth(d_date, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  res_chr  <- cleangrowth(d_chr,  dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_identical(as.character(res_chr$exclude), as.character(res_date$exclude))
  expect_equal(res_chr$agedays, res_date$agedays)
})

# ---------------------------------------------------------------------------
# Test 7: a lone date column WITHOUT an agedays column cannot give an age
# ---------------------------------------------------------------------------
test_that("a lone dob / visit_date with no agedays column errors", {
  d <- .add_dates(.mk_df())
  d[, agedays := NULL]  # no agedays -> a single date column cannot compute age
  expect_error(
    cleangrowth(d, dob = "dob", quietly = TRUE),
    regexp = "both `dob` and `visit_date`.*or an `agedays` column"
  )
  expect_error(
    cleangrowth(d, visit_date = "visit_date", quietly = TRUE),
    regexp = "both `dob` and `visit_date`.*or an `agedays` column"
  )
})

# ---------------------------------------------------------------------------
# Test 7b: a lone visit_date ALONGSIDE agedays is parse-only -- ages come from
# the agedays column, and gc_internal_visit_date surfaces the parsed date.
# ---------------------------------------------------------------------------
test_that("visit_date alongside agedays is parsed but does not drive ages", {
  d <- .add_dates(.mk_df())          # keeps the agedays column
  d[, dob := NULL]                   # visit_date only, no dob
  d[, visit_date := as.character(visit_date)]  # ISO text, as from a CSV

  res_plain <- cleangrowth(.mk_df(), quietly = TRUE)
  res <- cleangrowth(d, visit_date = "visit_date", internal_detail = TRUE,
                     quietly = TRUE)

  # agedays column drove the run: identical exclude + agedays to the plain run
  expect_identical(as.character(res$exclude), as.character(res_plain$exclude))
  expect_equal(res$agedays, .mk_df()$agedays)
  # parsed visit_date surfaced as a Date, dob absent
  expect_true("gc_internal_visit_date" %in% names(res))
  expect_false("gc_internal_dob" %in% names(res))
  expect_s3_class(res$gc_internal_visit_date, "Date")
  # matches the encoded dates (dob 2015-01-01 + agedays), in input row order
  expect_equal(res$gc_internal_visit_date,
               as.Date("2015-01-01") + .mk_df()$agedays)
})

# ---------------------------------------------------------------------------
# Test 7c: gc_internal_visit_date is gated behind internal_detail
# ---------------------------------------------------------------------------
test_that("gc_internal_visit_date only appears under internal_detail", {
  d <- .add_dates(.mk_df())
  d[, dob := NULL]
  res <- cleangrowth(d, visit_date = "visit_date", quietly = TRUE)
  expect_false("gc_internal_visit_date" %in% names(res))
  # the raw visit_date still rides through verbatim via carry_through
  expect_true("visit_date" %in% names(res))
})

# ---------------------------------------------------------------------------
# Test 7d: dob + visit_date + agedays (auto) uses agedays yet parses both dates
# ---------------------------------------------------------------------------
test_that("both dates alongside agedays parse to both gc_internal_ columns", {
  d <- .add_dates(.mk_df())  # agedays column present
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date",
                     internal_detail = TRUE, quietly = TRUE)
  expect_equal(res$agedays, .mk_df()$agedays)  # agedays column won
  expect_true(all(c("gc_internal_dob", "gc_internal_visit_date") %in% names(res)))
  expect_s3_class(res$gc_internal_dob, "Date")
  expect_equal(unique(res$gc_internal_dob), as.Date("2015-01-01"))
})

# ---------------------------------------------------------------------------
# Test 8: date args are data-frame style only
# ---------------------------------------------------------------------------
test_that("dob / visit_date / age_source error in vector style", {
  d <- .mk_df()
  expect_error(
    cleangrowth(subjid = d$subjid, param = d$param, agedays = d$agedays,
                sex = d$sex, measurement = d$measurement, id = d$id,
                dob = "dob", quietly = TRUE),
    regexp = "only supported in data-frame style"
  )
})

# ---------------------------------------------------------------------------
# Test 9: NA visit_date -> NA agedays -> Exclude-Missing-Info
# ---------------------------------------------------------------------------
test_that("a missing date becomes Exclude-Missing-Info", {
  d <- .add_dates(.mk_df())
  d[, agedays := NULL]
  d[3, visit_date := NA]
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_true(is.na(res$agedays[3]))
  expect_equal(as.character(res$exclude[3]), "Exclude-Missing-Info")
})

# ---------------------------------------------------------------------------
# Test 10: no age information at all errors clearly
# ---------------------------------------------------------------------------
test_that("no agedays column and no dates errors clearly", {
  d <- .mk_df()
  d[, agedays := NULL]
  expect_error(cleangrowth(d, quietly = TRUE),
               regexp = "No age information")
})

# ===========================================================================
# Date-arithmetic edge cases
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 11: visit_date == dob -> agedays 0 (birth), a real age (not missing)
# ---------------------------------------------------------------------------
test_that("visit_date equal to dob gives agedays 0, not Missing", {
  d <- .add_dates(.mk_df())
  d[, agedays := NULL]
  d[1, visit_date := dob]  # measured on the day of birth
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_equal(res$agedays[1], 0L)
  expect_false(as.character(res$exclude[1]) == "Exclude-Missing-Info")
})

# ---------------------------------------------------------------------------
# Test 12: visit_date before dob -> negative agedays -> Exclude-Missing-Info
# ---------------------------------------------------------------------------
test_that("visit_date before dob yields a negative age treated as Missing", {
  d <- .add_dates(.mk_df())
  d[, agedays := NULL]
  d[2, visit_date := dob - 5L]  # visit 5 days before birth
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_equal(res$agedays[2], -5L)
  expect_equal(as.character(res$exclude[2]), "Exclude-Missing-Info")
})

# ---------------------------------------------------------------------------
# Test 13: day counts are exact across a leap-year boundary (whole days)
# ---------------------------------------------------------------------------
test_that("agedays counts whole days exactly, including over a leap year", {
  d <- data.table(
    id          = 1:3,
    subjid      = "s1",
    sex         = 0L,
    param       = "HEIGHTCM",
    measurement = c(50, 65, 75),
    dob         = as.Date("2020-01-01"),                 # 2020 is a leap year
    visit_date  = as.Date(c("2020-01-01", "2020-07-01", "2021-01-01"))
  )
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  # 0 days; Jan 1 -> Jul 1 2020 = 182 (Feb has 29 days); Jan 1 2020 -> Jan 1 2021 = 366
  expect_equal(res$agedays, c(0L, 182L, 366L))
})

# ---------------------------------------------------------------------------
# Test 14: NA dob -> that row is Missing; NA does not trip the consistency check
# ---------------------------------------------------------------------------
test_that("NA dob yields Missing for that row and passes the consistency check", {
  d <- .add_dates(.mk_df())
  d[, agedays := NULL]
  d[3, dob := NA]  # subject still has a single distinct non-NA dob elsewhere
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_true(is.na(res$agedays[3]))
  expect_equal(as.character(res$exclude[3]), "Exclude-Missing-Info")
  # other rows still computed normally
  expect_equal(res$agedays[-3], c(183L, 190L, 270L, 292L, 355L, 367L))
})

# ---------------------------------------------------------------------------
# Test 15: distinct dob per subject is fine (no cross-subject bleed)
# ---------------------------------------------------------------------------
test_that("different dob across subjects computes each subject's ages correctly", {
  d <- data.table(
    id          = 1:4,
    subjid      = c("a", "a", "b", "b"),
    sex         = 0L,
    param       = "HEIGHTCM",
    measurement = c(60, 70, 62, 72),
    dob         = as.Date(c("2018-01-01", "2018-01-01",
                            "2019-06-15", "2019-06-15")),
    visit_date  = as.Date(c("2018-07-01", "2019-01-01",
                            "2019-12-15", "2020-06-15"))
  )
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  # a: 181, 365 (2018 non-leap); b: 183, 366 (spans 2020 Feb 29)
  expect_equal(res$agedays, c(181L, 365L, 183L, 366L))
})

# ---------------------------------------------------------------------------
# Test 16: POSIXct (neither Date nor ISO text) is rejected
# ---------------------------------------------------------------------------
test_that("POSIXct date columns are rejected", {
  d <- .add_dates(.mk_df())
  d[, agedays := NULL]
  d[, dob := as.POSIXct(dob)]
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "`dob`.*must be an R Date or a text"
  )
})

# ===========================================================================
# ISO-text parsing (dates as read from a CSV)
# ===========================================================================

# Helper: mk_df with ISO character date columns (dob + visit_date), no agedays
.mk_iso <- function() {
  d <- .add_dates(.mk_df())
  d[, agedays := NULL]
  d[, dob := as.character(dob)]
  d[, visit_date := as.character(visit_date)]
  d[]
}

# ---------------------------------------------------------------------------
# Test 17: an unrecognized-format date string errors, naming an example value.
# (Slash-separated year-first, e.g. "2024/03/15", is ACCEPTED -- see the
# expanded-format tests below -- so a malformed value must be genuinely
# unrecognized, e.g. an unsupported separator.)
# ---------------------------------------------------------------------------
test_that("unrecognized date text errors with an example", {
  d <- .mk_iso()
  d[3, visit_date := "2024_03_15"]  # underscore separator, not accepted
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "unrecognized format.*2024_03_15"
  )
})

# ---------------------------------------------------------------------------
# Test 18: numeric day/month text is rejected without date_order (ambiguous
# day-vs-month order -- gc never guesses).
# ---------------------------------------------------------------------------
test_that("numeric day/month text is rejected without date_order", {
  d <- .mk_iso()
  # Whole column as numeric day/month so the class is unambiguous num_dm.
  d[, visit_date := format(as.Date(visit_date), "%d/%m/%Y")]
  d[, dob        := format(as.Date(dob),        "%d/%m/%Y")]
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "numeric day/month dates.*date_order"
  )
})

# ---------------------------------------------------------------------------
# Test 19: numeric (software date integer) is rejected with an epoch message
# ---------------------------------------------------------------------------
test_that("numeric date columns are rejected (ambiguous epoch)", {
  d <- .mk_iso()
  d[, visit_date := as.integer(as.Date(visit_date) - as.Date("1960-01-01"))]  # integer day count
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "`visit_date` column .* is numeric"
  )
})

# ---------------------------------------------------------------------------
# Test 20: impossible (but ISO-shaped) dates: a few are set aside, many stop
# ---------------------------------------------------------------------------
test_that("a few impossible ISO-shaped dates are set aside; many stop the run", {
  d <- .mk_iso()
  d[4, visit_date := "2024-13-45"]  # matches the shape but is not a real date
  expect_warning(
    res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "Unreadable dates -- 1 measurement.*2024-13-45"
  )
  expect_equal(as.character(res$exclude[4]), "Exclude-Missing-Info")
  expect_true(is.na(res$agedays[4]))
  # more than a few (here 3 of 7) points to a format problem -> stop
  d[c(2, 5), visit_date := "2024-02-30"]
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "could not be read as dates.*2024-13-45"
  )
})

# ---------------------------------------------------------------------------
# Test 20b: date values with no digits ("N/A", "NULL", ".", "unknown") are blank
# ---------------------------------------------------------------------------
test_that("date values with no digits are treated as blank and reported", {
  for (tok in c("NA", "N/A", "NULL", ".", "unknown")) {
    d <- .mk_iso()
    d[3, visit_date := tok]
    expect_warning(
      res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
      regexp = "Dates with no digits -- 1 measurement", info = tok
    )
    expect_equal(as.character(res$exclude[3]), "Exclude-Missing-Info", info = tok)
    expect_equal(sum(as.character(res$exclude) == "Exclude-Missing-Info"), 1L, info = tok)
  }
  # in the dob column too, even when most values are tokens
  d <- .mk_iso()
  d[1:6, dob := "unknown"]
  expect_warning(
    res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "Dates with no digits -- 6 measurement"
  )
  # an agedays-driven run is not affected by junk in a parse-only date column
  d2 <- .add_dates(.mk_df()); d2[, dob := as.character(dob)]; d2[2, dob := "N/A"]
  expect_no_warning(res2 <- cleangrowth(d2, quietly = TRUE))
  expect_false(any(as.character(res2$exclude) == "Exclude-Missing-Info"))
})

# ---------------------------------------------------------------------------
# Test 21: a 2-digit year is rejected (guards against loose strptime parsing)
# ---------------------------------------------------------------------------
test_that("2-digit years are rejected", {
  d <- .mk_iso()
  d[1, visit_date := "24-03-15"]
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "without a four-digit year.*24-03-15"
  )
})

# ---------------------------------------------------------------------------
# Test 22: a blank date string is missing (not an error)
# ---------------------------------------------------------------------------
test_that("blank ISO text is treated as missing, not malformed", {
  d <- .mk_iso()
  d[3, visit_date := ""]
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_true(is.na(res$agedays[3]))
  expect_equal(as.character(res$exclude[3]), "Exclude-Missing-Info")
  # the other rows still compute correctly
  expect_equal(res$agedays[-3], c(183L, 190L, 270L, 292L, 355L, 367L))
})

# ---------------------------------------------------------------------------
# Test 23: factor date columns are accepted (parsed as their labels)
# ---------------------------------------------------------------------------
test_that("factor ISO columns are accepted", {
  d <- .mk_iso()
  d[, dob := as.factor(dob)]
  d[, visit_date := as.factor(visit_date)]
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_equal(res$agedays, c(183L, 190L, 222L, 270L, 292L, 355L, 367L))
})

# ===========================================================================
# Expanded date-input formats
# dob + visit_date parse under year-first numeric, English spelled-month, or numeric day/month
# (the last requires date_order). One format per run; four-digit 19xx/20xx years
# only; ambiguous numeric never guessed.
# ===========================================================================

.expected_agedays <- c(183L, 190L, 222L, 270L, 292L, 355L, 367L)

# Reformat the ISO dob/visit_date columns with a strftime pattern (numeric only).
.as_fmt <- function(d, fmt) {
  d <- copy(d)
  d[, dob := format(as.Date(dob), fmt)]
  d[, visit_date := format(as.Date(visit_date), fmt)]
  d[]
}

# Build spelled-month strings from English month.abb / month.name (base R
# constants, English on every locale), so these tests do not depend on LC_TIME.
.spell <- function(dates, style) {
  dd <- as.Date(dates)
  day <- as.integer(format(dd, "%d")); mo <- as.integer(format(dd, "%m"))
  yr  <- format(dd, "%Y")
  switch(style,
    ddmonyyyy = sprintf("%02d%s%s", day, month.abb[mo], yr), # 01Jan2015
    dmon  = sprintf("%d-%s-%s", day, month.abb[mo], yr),   # 1-Jan-2015
    dMonY = sprintf("%d %s %s", day, month.name[mo], yr),  # 1 January 2015
    MonDY = sprintf("%s %d, %s", month.name[mo], day, yr)) # January 1, 2015
}
.as_spell <- function(d, style) {
  d <- copy(d)
  d[, dob := .spell(dob, style)]
  d[, visit_date := .spell(visit_date, style)]
  d[]
}

test_that("year-first numeric formats parse (dash / slash / dot / compact)", {
  for (fmt in c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d")) {
    d <- .as_fmt(.mk_iso(), fmt)
    res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
    expect_equal(res$agedays, .expected_agedays, info = fmt)
  }
})

test_that("English spelled-month formats parse (ddMonYYYY, d-Mon-Y, d Month Y, Month d, Y)", {
  for (style in c("ddmonyyyy", "dmon", "dMonY", "MonDY")) {
    d <- .as_spell(.mk_iso(), style)
    res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
    expect_equal(res$agedays, .expected_agedays, info = style)
  }
})

test_that("year-first and spelled-month may mix separators within their class", {
  d <- .mk_iso()
  d[, dob := gsub("-", "/", dob)]          # slashes
  # visit_date left as dashes -> both are year-first (ymd), separators differ.
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_equal(res$agedays, .expected_agedays)
})

test_that("numeric day/month parses with date_order = dmy and mdy", {
  d_dmy <- .as_fmt(.mk_iso(), "%d/%m/%Y")
  res_dmy <- cleangrowth(d_dmy, dob = "dob", visit_date = "visit_date",
                         date_order = "dmy", quietly = TRUE)
  expect_equal(res_dmy$agedays, .expected_agedays)

  d_mdy <- .as_fmt(.mk_iso(), "%m/%d/%Y")
  res_mdy <- cleangrowth(d_mdy, dob = "dob", visit_date = "visit_date",
                         date_order = "mdy", quietly = TRUE)
  expect_equal(res_mdy$agedays, .expected_agedays)
})

test_that("content-unambiguous numeric (day > 12) still requires date_order", {
  d <- .mk_iso()
  # 25th of the month makes it unambiguous by content, but we classify by
  # pattern -> still an error without date_order.
  d[, dob := "25/12/2014"]
  d[, visit_date := "26/12/2014"]
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "numeric day/month dates.*date_order")
})

test_that("two-digit years are rejected in numeric day/month too", {
  d <- .as_fmt(.mk_iso(), "%d/%m/%y")   # 2-digit year
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date",
                date_order = "dmy", quietly = TRUE),
    regexp = "without a four-digit year")
})

test_that("years outside 1900-2099 are rejected", {
  d <- .mk_iso()
  d[1, visit_date := "1899-12-31"]
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "without a four-digit year")
})

test_that("mixing date formats across dob and visit_date is rejected", {
  d <- .mk_iso()                                        # dob is year-first ISO
  d[, visit_date := format(as.Date(visit_date), "%d/%m/%Y")]  # visit_date is num_dm
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date",
                date_order = "dmy", quietly = TRUE),
    regexp = "mixes more than one date format")
})

test_that("date_order is rejected in vector style", {
  d <- .mk_df()
  expect_error(
    cleangrowth(subjid = d$subjid, param = d$param, agedays = d$agedays,
                sex = d$sex, measurement = d$measurement, date_order = "dmy",
                quietly = TRUE),
    regexp = "date_order.*only supported in data-frame style")
})

test_that("date_order must be dmy or mdy", {
  d <- .as_fmt(.mk_iso(), "%d/%m/%Y")
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date",
                date_order = "ymd", quietly = TRUE),
    regexp = 'date_order.*must be NULL, "dmy", or "mdy"')
})

test_that("full_detail surfaces gc_internal_dob / gc_internal_visit_date as Dates", {
  d <- .mk_iso()
  res <- cleangrowth(d, dob = "dob", visit_date = "visit_date",
                     full_detail = TRUE, quietly = TRUE)
  expect_true(all(c("gc_internal_dob", "gc_internal_visit_date") %in% names(res)))
  expect_s3_class(res$gc_internal_dob, "Date")
  expect_equal(res$gc_internal_dob, rep(as.Date("2015-01-01"), 7L))
  # Absent without the flag.
  res2 <- cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE)
  expect_false("gc_internal_dob" %in% names(res2))
})

test_that("non-English (French) spelled months are rejected (locale pinned to English)", {
  d <- .mk_iso()
  d[, dob := "4 mars 2015"]          # French: March
  d[, visit_date := "4 septembre 2015"]  # French: September
  expect_error(
    cleangrowth(d, dob = "dob", visit_date = "visit_date", quietly = TRUE),
    regexp = "could not be read as dates")
})

# -----------------------------------------------------------------------------
# Fractional agedays: cleaned on whole days, returned verbatim
# -----------------------------------------------------------------------------
test_that("fractional agedays are cleaned as whole days but returned as submitted", {
  d  <- .mk_df()
  df <- copy(d)[, agedays := agedays + 0.4]
  r_int  <- cleangrowth(d,  quietly = TRUE)
  r_frac <- cleangrowth(df, quietly = TRUE, internal_detail = TRUE)
  # Same cleaning result as the whole-day input
  expect_identical(as.character(r_frac$exclude), as.character(r_int$exclude))
  # Submitted agedays come back unchanged (not floored, not coerced to integer)
  expect_equal(r_frac$agedays, df$agedays)
  expect_type(r_frac$agedays, "double")
  # The whole-day value used for cleaning is available under internal_detail
  expect_identical(r_frac$gc_internal_agedays, d$agedays)
  # Not emitted by default
  expect_false("gc_internal_agedays" %in% names(r_int))
})

test_that("a negative fractional agedays is rounded down (not toward zero) and set aside", {
  d <- data.table(subjid = "a", param = "WEIGHTKG", sex = 0L, id = 1:3,
                  agedays = c(-0.5, 400.9, 800), measurement = c(3.4, 10, 12.5))
  r <- suppressWarnings(cleangrowth(d, quietly = TRUE, internal_detail = TRUE))
  expect_identical(r$gc_internal_agedays, c(-1L, 400L, 800L))
  expect_equal(r$agedays, d$agedays)
  expect_false(as.character(r$exclude[1]) == "Include")
  expect_identical(as.character(r$exclude[2:3]), c("Include", "Include"))
})
