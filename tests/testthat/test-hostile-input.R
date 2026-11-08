testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Hostile / awkward input at the wrapper edge (long format).
#
# Characterization + guard tests: awkward-but-real things datasets do. The bar
# is a clear error or a documented, tested behavior -- never a silent wrong
# answer. Every expectation here reflects cleangrowth()'s actual behavior on the
# input.
#
# Long format is the bulk of this file; the semi-wide / wide reshape adapters
# are at the bottom.
#
# Related coverage (not duplicated here):
#   - flexible `sex`/`param` codings -> test-flexible-input-coding.R
#   - semi-wide / wide happy-path + warnings -> test-semiwide-input.R,
#     test-wide-input.R
# =============================================================================

base_ok <- function() data.table(
  subjid = c("a", "a"), param = c("HEIGHTCM", "WEIGHTKG"), sex = 0L,
  agedays = c(365L, 365L), measurement = c(75, 10), id = 1:2
)
excl <- function(d, ...) as.character(
  as.data.table(suppressWarnings(suppressMessages(cleangrowth(d, quietly = TRUE, ...))))$exclude)

# ---------------------------------------------------------------------------
# Clean errors (guards) -- must stop, not silently mis-handle.
# ---------------------------------------------------------------------------
test_that("zero-row input is a clean error", {
  expect_error(cleangrowth(base_ok()[0], quietly = TRUE), "zero rows")
})

test_that("an entirely-NA required column is a clean error (subjid/param/measurement/agedays)", {
  for (col in c("measurement", "subjid", "param", "agedays")) {
    d <- base_ok(); d[[col]] <- NA
    expect_error(suppressWarnings(cleangrowth(d, quietly = TRUE)), "no non-missing values",
                 info = col)
  }
})

.cap_warn <- function(expr) { ws <- character(0)
  val <- withCallingHandlers(expr, warning = function(w) {
    ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(value = val, warnings = ws) }

test_that("a factor measurement is converted by its labels (not level codes), with a warning", {
  d <- base_ok(); d[, measurement := factor(as.character(measurement))]
  r <- .cap_warn(suppressMessages(cleangrowth(d, quietly = TRUE)))
  expect_true(any(grepl("is a factor", r$warnings)))
  # returned as submitted (the factor); growthcleanr cleaned the label values
  expect_identical(r$value$measurement, d$measurement)
  ri <- suppressWarnings(suppressMessages(cleangrowth(d, quietly = TRUE, internal_detail = TRUE)))
  expect_equal(ri$gc_internal_meas, base_ok()$measurement)
  # a non-numeric label is set aside, not a stop
  d2 <- base_ok(); d2[, measurement := factor(c("75", "N/A"))]
  r2 <- .cap_warn(suppressMessages(cleangrowth(d2, quietly = TRUE)))
  expect_equal(as.character(r2$value$exclude), c("Include", "Exclude-Missing-Info"))
})

test_that("agedays as a time difference is converted to days in its own units", {
  b <- data.table(subjid = "a", param = "WEIGHTKG", sex = 0L,
                  agedays = c(400L, 800L), measurement = c(10, 12), id = 1:2)
  ref <- suppressMessages(cleangrowth(b, quietly = TRUE))
  # difftime in days: no warning, same result
  d1 <- copy(b)[, agedays := as.difftime(agedays, units = "days")]
  r1 <- .cap_warn(suppressMessages(cleangrowth(d1, quietly = TRUE)))
  expect_false(any(grepl("time difference", r1$warnings)))
  expect_equal(as.character(r1$value$exclude), as.character(ref$exclude))
  # difftime in hours: converted (not read as days), with a warning
  d2 <- copy(b)[, agedays := as.difftime(agedays * 24, units = "hours")]
  r2 <- .cap_warn(suppressMessages(cleangrowth(d2, quietly = TRUE, internal_detail = TRUE)))
  expect_true(any(grepl("time difference in hours", r2$warnings)))
  expect_equal(r2$value$gc_internal_agedays, c(400L, 800L))
  expect_equal(as.character(r2$value$exclude), as.character(ref$exclude))
})

test_that("a factor agedays is converted by its labels (not level codes), with a warning", {
  b <- data.table(subjid = "a", param = "WEIGHTKG", sex = 0L,
                  agedays = factor(c("400", "800")), measurement = c(10, 12), id = 1:2)
  r <- .cap_warn(suppressMessages(cleangrowth(b, quietly = TRUE, internal_detail = TRUE)))
  expect_true(any(grepl("`agedays` is a factor", r$warnings)))
  expect_equal(r$value$gc_internal_agedays, c(400L, 800L))
})

test_that("a non-positive / non-numeric batch_size is a clean error", {
  for (bs in list(0, -1, NA, "abc", c(1, 2))) {
    expect_error(cleangrowth(base_ok(), quietly = TRUE, batch_size = bs),
                 "batch_size must be a single positive whole number",
                 info = paste("batch_size =", paste(bs, collapse = ",")))
  }
  # a valid positive size still cleans (sanity)
  expect_silent(as.character(suppressMessages(
    cleangrowth(base_ok(), quietly = TRUE, batch_size = 1))$exclude))
})

test_that("a non-scalar / non-numeric adult_scale_max_lbs is a clean error", {
  # Before the length guard, a length-2 value crashed with the cryptic
  # "'length = 2' in coercion to 'logical(1)'" from `x < 0`. Inf (the default) and
  # 0 must still pass.
  for (v in list(NA, "big", c(100, 200), -5)) {
    expect_error(cleangrowth(base_ok(), quietly = TRUE, adult_scale_max_lbs = v),
                 "adult_scale_max_lbs must be a single positive number",
                 info = paste("adult_scale_max_lbs =", paste(v, collapse = ",")))
  }
})

test_that("a hand-built ref_tables with missing/misnamed/non-function elements is a clean error", {
  refs <- gc_preload_refs()
  # Missing an element (only one of the two closures supplied).
  expect_error(
    cleangrowth(base_ok(), quietly = TRUE, ref_tables = refs["mtz_cdc_prelim"]),
    "missing required element", info = "one key missing")
  # Misnamed element (right closures, wrong names).
  bad_names <- setNames(refs, c("cdc", "who"))
  expect_error(
    cleangrowth(base_ok(), quietly = TRUE, ref_tables = bad_names),
    "missing required element", info = "keys renamed")
  # Right names but an element that is not a function.
  not_fun <- refs; not_fun$mtz_who_prelim <- 42
  expect_error(
    cleangrowth(base_ok(), quietly = TRUE, ref_tables = not_fun),
    "must be the function", info = "element not a function")
  # Not a list at all.
  expect_error(
    cleangrowth(base_ok(), quietly = TRUE, ref_tables = "not a list"),
    "must be a list", info = "scalar instead of list")
  # The genuine gc_preload_refs() output still cleans (sanity).
  expect_silent(as.character(suppressMessages(
    cleangrowth(base_ok(), quietly = TRUE, ref_tables = refs))$exclude))
})

test_that("an all-empty-string measurement column is a clean error (coerces to all-NA)", {
  d <- base_ok(); d[, measurement := c("", "")]
  expect_error(suppressWarnings(suppressMessages(cleangrowth(d, quietly = TRUE))),
               "no non-missing values")
})

# ---------------------------------------------------------------------------
# Empty core: every row set aside (all non-gc param, or every row missing a
# structural key). Must return the set-aside rows, not crash at setorder(line).
# ---------------------------------------------------------------------------
test_that("all rows non-gc param -> all Exclude-Not-GC-Param (no crash)", {
  d <- data.table(subjid = "a", param = "BMI", sex = 0L,
                  agedays = c(365L, 730L), measurement = c(20, 22), id = 1:2)
  out <- as.data.table(suppressWarnings(cleangrowth(d, quietly = TRUE)))
  expect_equal(nrow(out), 2L)
  expect_true(all(out$exclude == "Exclude-Not-GC-Param"))
})

test_that("every row missing subjid or param -> all Missing-Info (no crash)", {
  # No single column is all-NA, but every row lacks a structural key, so the
  # core is empty after set-aside.
  d <- data.table(subjid = c(NA, "a"), param = c("WEIGHTKG", NA), sex = 0L,
                  agedays = c(365L, 730L), measurement = c(10, 12), id = 1:2)
  out <- as.data.table(suppressWarnings(cleangrowth(d, quietly = TRUE)))
  expect_equal(nrow(out), 2L)
  expect_true(all(out$exclude == "Exclude-Missing-Info"))
})

# ---------------------------------------------------------------------------
# Sex is required to clean PEDIATRIC rows (child needs it for z-scores); adults
# clean without it (M/F-averaged CSD reference). The rule is about the pediatric
# rows only, and runs in the shared funnel so it holds across input shapes:
#   - EVERY pediatric row lacks usable sex -> clean stop.
#   - SOME pediatric rows lack usable sex -> those -> Missing-Info + warning.
# ---------------------------------------------------------------------------
test_that("every pediatric row lacking a usable sex is a clean stop", {
  d <- base_ok(); d[, sex := NA]           # both rows pediatric (365d), all sex NA
  expect_error(suppressWarnings(cleangrowth(d, quietly = TRUE)), "usable value in the sex column")
})

test_that("every pediatric row lacking sex is set aside when adults are present; adults clean", {
  # Adults have valid sex and clean; ALL pediatric rows lack sex, so they are
  # set aside as Exclude-Missing-Info with a warning (no stop).
  d <- data.table(
    subjid = c("ped", "ped", "adt", "adt"), param = "WEIGHTKG",
    sex = c(NA, NA, 0L, 0L), agedays = c(365L, 730L, 10000L, 10400L),
    measurement = c(10, 12, 70, 71), id = 1:4)
  r <- .cap_warn(suppressMessages(cleangrowth(d, quietly = TRUE)))
  expect_equal(as.character(r$value$exclude),
               c("Exclude-Missing-Info", "Exclude-Missing-Info", "Include", "Include"))
  expect_true(any(grepl("This is every pediatric measurement", r$warnings)))
})

test_that("some (not all) pediatric rows lacking sex -> Missing-Info + warning, others clean", {
  # Two pediatric subjects: one with sex, one without.
  d <- data.table(
    subjid = c("hassex", "hassex", "nosex", "nosex"), param = "WEIGHTKG",
    sex = c(0L, 0L, NA, NA), agedays = c(365L, 730L, 365L, 730L),
    measurement = c(9, 11, 9, 11), id = 1:4)
  expect_warning(cleangrowth(d, quietly = TRUE), "Unrecognized sex")
  out <- as.data.table(suppressWarnings(cleangrowth(d, quietly = TRUE)))
  expect_true(all(out[subjid == "nosex"]$exclude == "Exclude-Missing-Info"))
  expect_false(any(out[subjid == "hassex"]$exclude == "Exclude-Missing-Info"))
})

test_that("all-NA sex is allowed when every measurement is adult (no pediatric rows)", {
  d <- data.table(subjid = "a", param = c("HEIGHTCM", "WEIGHTKG"), sex = NA,
                  agedays = 10000L, measurement = c(170, 70), id = 1:2)
  out <- as.data.table(suppressWarnings(cleangrowth(d, quietly = TRUE)))
  expect_equal(nrow(out), 2L)
  expect_false(any(out$exclude == "Exclude-Missing-Info"))
})

test_that("a duplicated required column is a clean error (not silently first-wins)", {
  d <- as.data.frame(base_ok())
  d$m2 <- d$measurement
  names(d)[names(d) == "m2"] <- "measurement"
  expect_error(cleangrowth(d, quietly = TRUE), "duplicate column")
})

test_that("a mis-cased / whitespace-padded required column errors with a near-match hint", {
  d <- base_ok(); setnames(d, "measurement", "Measurement")
  expect_error(cleangrowth(d, quietly = TRUE), "did you mean 'Measurement'")
  d <- base_ok(); setnames(d, "measurement", "measurement ")
  expect_error(cleangrowth(d, quietly = TRUE), "did you mean 'measurement '")
})

# ---------------------------------------------------------------------------
# NA in a key column -> row set aside as Missing-Info, plus a warning.
# ---------------------------------------------------------------------------
test_that("NA subjid on a row -> Exclude-Missing-Info (not silently Included), with a warning", {
  d <- base_ok(); d[1, subjid := NA]
  expect_warning(cleangrowth(d, quietly = TRUE), "Blank required fields")
  expect_equal(excl(d), c("Exclude-Missing-Info", "Include"))
})

test_that("NA param on a row -> Exclude-Missing-Info (reported missing, not Not-GC-Param)", {
  d <- base_ok(); d[1, param := NA]
  expect_equal(excl(d), c("Exclude-Missing-Info", "Include"))
})

test_that("NA agedays / sex / measurement on a row -> Exclude-Missing-Info", {
  d <- base_ok(); d[1, agedays := NA];     expect_equal(excl(d)[1], "Exclude-Missing-Info")
  d <- base_ok(); d[1, sex := NA];         expect_equal(excl(d)[1], "Exclude-Missing-Info")
  d <- base_ok(); d[1, measurement := NA]; expect_equal(excl(d)[1], "Exclude-Missing-Info")
})

# ---------------------------------------------------------------------------
# Numeric edge values.
# ---------------------------------------------------------------------------
test_that("Inf measurement -> Hard-Limit; NaN measurement -> Missing-Info", {
  d <- base_ok(); d[1, measurement := Inf]; expect_equal(excl(d)[1], "Exclude-Hard-Limit")
  d <- base_ok(); d[1, measurement := NaN]; expect_equal(excl(d)[1], "Exclude-Missing-Info")
})

test_that("enormous agedays -> Hard-Limit (no crash)", {
  d <- base_ok(); d[1, agedays := 3650000L]
  expect_equal(excl(d)[1], "Exclude-Hard-Limit")
})

test_that("negative and empty-string measurements -> Missing-Info", {
  d <- base_ok(); d[1, measurement := -10]
  expect_equal(excl(d)[1], "Exclude-Missing-Info")
  d <- base_ok(); d[, measurement := as.character(measurement)][1, measurement := ""]
  expect_equal(excl(d)[1], "Exclude-Missing-Info")
})

# ---------------------------------------------------------------------------
# Type / structure the algorithm should tolerate.
# ---------------------------------------------------------------------------
test_that("character measurement is coerced (with a one-time message) and cleans", {
  d <- base_ok(); d[, measurement := as.character(measurement)]
  expect_message(suppressWarnings(cleangrowth(d, quietly = TRUE)), "supplied as character")
  expect_equal(excl(d), c("Include", "Include"))
})

test_that("a character measurement is returned as submitted (child, adult, checkpointed)", {
  m <- data.table(subjid = c(rep("k", 4), rep("ad", 4)),
                  param = c("WEIGHTKG", "HEIGHTCM", "WEIGHTKG", "HEIGHTCM",
                            "WEIGHTLBS", "HEIGHTCM", "WEIGHTLBS", "HEIGHTCM"),
                  agedays = c(400, 400, 800, 800, 9000, 9000, 9400, 9400),
                  sex = c(0, 0, 0, 0, 1, 1, 1, 1),
                  measurement = c("10.0", "76", " 12.5", "abc", "150.0", "165", "152", "160"))
  r <- suppressWarnings(suppressMessages(cleangrowth(m, quietly = TRUE, internal_detail = TRUE)))
  expect_identical(r$measurement, m$measurement)
  expect_equal(r$gc_internal_meas[1:3], c(10, 76, 12.5))
  expect_equal(as.character(r$exclude[4]), "Exclude-Missing-Info")
  rn <- suppressWarnings(suppressMessages(cleangrowth(
    copy(m)[, measurement := suppressWarnings(as.numeric(measurement))], quietly = TRUE)))
  expect_equal(as.character(r$exclude), as.character(rn$exclude))
  rc <- suppressWarnings(suppressMessages(cleangrowth(m, quietly = TRUE, internal_detail = TRUE,
          checkpoint_dir = file.path(tempdir(), "gc-char-meas"), checkpoint_size = 1L)))
  expect_equal(rc, r, ignore_attr = TRUE)
})

test_that("a character measurement value carrying unit text -> that row is Missing-Info", {
  # A single unparseable value becomes NA -> Missing-Info; the parseable value
  # still cleans. (An ENTIRELY unparseable column coerces to all-NA and is the
  # clean all-NA error instead -- see the all-empty-string test above.)
  d <- base_ok(); d[, measurement := c("75 cm", "10")]
  expect_equal(excl(d)[1], "Exclude-Missing-Info")
  expect_equal(excl(d)[2], "Include")
})

test_that("factor subjid with an unused level cleans normally", {
  d <- base_ok(); d[, subjid := factor(subjid, levels = c("a", "zzz_unused"))]
  expect_equal(excl(d), c("Include", "Include"))
})

test_that("plain data.frame, data.frame with row names, and a pre-keyed data.table all clean", {
  expect_equal(excl(as.data.frame(base_ok())), c("Include", "Include"))
  df <- as.data.frame(base_ok()); rownames(df) <- c("r1", "r2")
  expect_equal(excl(df), c("Include", "Include"))
  d <- base_ok(); setkey(d, subjid, agedays)
  expect_equal(excl(d), c("Include", "Include"))
})

test_that("an extra column named T is carried through without breaking", {
  d <- base_ok(); d[, T := 1L]
  expect_equal(excl(d), c("Include", "Include"))
})

test_that("an unknown (non-gc) param -> Exclude-Not-GC-Param", {
  d <- base_ok(); d[1, param := "BMI"]
  expect_equal(excl(d)[1], "Exclude-Not-GC-Param")
})

# ---------------------------------------------------------------------------
# Cross-width: the all-NA-agedays error and the all-NA-sex/pediatric error also
# fire for semi-wide and wide input (the guards live in the shared funnel, which
# every shape reaches after reshaping to long).
# ---------------------------------------------------------------------------
test_that("all-NA agedays errors for semi-wide and wide input too", {
  sw <- data.table(subjid = "a", sex = 0L, agedays = NA_integer_,
                   wt = c(10, 12), ht = c(75, 85))
  expect_error(
    suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long",
      semiwide_cols = list(WEIGHTKG = "wt", HEIGHTCM = "ht"), quietly = TRUE)),
    "no non-missing values")

  wd <- data.table(subjid = "a", sex = 0L,
                   agedays_1 = NA_integer_, wt_1 = 10, ht_1 = 75,
                   agedays_2 = NA_integer_, wt_2 = 12, ht_2 = 85)
  expect_error(
    suppressWarnings(cleangrowth(wd, input_format = "wide", output_format = "long",
      wide_cols = list(WEIGHTKG = "wt", HEIGHTCM = "ht"), quietly = TRUE)),
    "no non-missing values")
})

test_that("all pediatric rows lacking sex errors for semi-wide input too", {
  sw <- data.table(subjid = "a", sex = NA, agedays = c(365L, 730L),
                   wt = c(10, 12), ht = c(75, 85))
  expect_error(
    suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long",
      semiwide_cols = list(WEIGHTKG = "wt", HEIGHTCM = "ht"), quietly = TRUE)),
    "usable value in the sex column")
})

# ===========================================================================
# Semi-wide / wide reshape sweep
#
# These awkward inputs run through the reshape adapters rather than the long
# funnel. The genuine silent-wrong is duplicate column names (value or
# structural), which silently first-win; the adapters stop on these (mirrors the
# long-path duplicate-required-column guard). The rest are characterization
# tests, several pinning where reshape *intentionally* differs from long because
# the parameter is carried by the column, not a value.
# ===========================================================================

SWx <- function(dt, ...) as.character(as.data.table(suppressWarnings(suppressMessages(
  cleangrowth(dt, input_format = "semi_wide", output_format = "long", quietly = TRUE, ...))))$exclude)
WDx <- function(dt, ...) as.character(as.data.table(suppressWarnings(suppressMessages(
  cleangrowth(dt, input_format = "wide", output_format = "long", quietly = TRUE, ...))))$exclude)

# --- Guards: duplicate column names must stop, not silently first-win. -------

test_that("semi-wide duplicate parameter column stops (not silent first-wins)", {
  # Genuine duplicate value column: without the guard, gc used the first
  # WEIGHTKG (6, 8) and silently dropped the second (600, 800).
  d <- data.frame(subjid = c("A", "A"), agedays = c(100L, 200L), sex = c(0L, 0L),
                  WEIGHTKG = c(6, 8), HEIGHTCM = c(60, 68), w2 = c(600, 800))
  names(d)[6] <- "WEIGHTKG"
  expect_error(SWx(d), "appear more than once")
})

test_that("semi-wide duplicate structural column stops", {
  d <- data.frame(subjid = c("A", "A"), agedays = c(100L, 200L), sex = c(0L, 0L),
                  WEIGHTKG = c(6, 8), HEIGHTCM = c(60, 68), s2 = c("Z", "Z"))
  names(d)[6] <- "subjid"
  expect_error(SWx(d), "appear more than once")
})

test_that("semi-wide duplicate mapped value column stops", {
  d <- data.frame(subjid = c("A", "A"), agedays = c(100L, 200L), sex = c(0L, 0L),
                  wt = c(6, 8), ht = c(60, 68), wt2 = c(600, 800))
  names(d)[6] <- "wt"
  expect_error(SWx(d, semiwide_cols = list(WEIGHTKG = "wt", HEIGHTCM = "ht")),
               "appear more than once")
})

test_that("wide duplicate structural column stops", {
  d <- data.frame(subjid = c("A", "B"), sex = c(0L, 1L), agedays_1 = c(100L, 50L),
                  WEIGHTKG_1 = c(6, 4), HEIGHTCM_1 = c(60, 55), s2 = c("Z", "Z"))
  names(d)[6] <- "subjid"
  expect_error(WDx(d, wide_cols = list(WEIGHTKG = "WEIGHTKG", HEIGHTCM = "HEIGHTCM")),
               "appear more than once")
})

test_that("wide duplicate / leading-zero block columns stop with one message", {
  # Exact duplicate value block.
  dv <- data.frame(subjid = c("A", "B"), sex = c(0L, 1L), agedays_1 = c(100L, 50L),
                   WEIGHTKG_1 = c(6, 4), HEIGHTCM_1 = c(60, 55), x = c(600, 400))
  names(dv)[6] <- "WEIGHTKG_1"
  expect_error(WDx(dv, wide_cols = list(WEIGHTKG = "WEIGHTKG", HEIGHTCM = "HEIGHTCM")),
               "one column per block")
  # Leading-zero padding (ht_01 vs ht_1) -> same block.
  dz <- data.frame(subjid = c("A", "B"), sex = c(0L, 1L), agedays_1 = c(100L, 50L),
                   ht_1 = c(60, 55), ht_01 = c(61, 56))
  expect_error(WDx(dz, wide_cols = list(HEIGHTCM = "ht")), "one column per block")
  # Duplicate age block.
  da <- data.frame(subjid = c("A", "B"), sex = c(0L, 1L), agedays_1 = c(100L, 50L),
                   WEIGHTKG_1 = c(6, 4), a2 = c(101L, 51L))
  names(da)[5] <- "agedays_1"
  expect_error(WDx(da, wide_cols = list(WEIGHTKG = "WEIGHTKG")), "one column per block")
})

# --- Reshape shares the long funnel's row-level guards (post-melt). ----------

test_that("a factor value column is converted by its labels in semi-wide and wide too", {
  sw <- data.table(subjid = c("A", "A"), agedays = c(100L, 200L), sex = 0L,
                   WEIGHTKG = factor(c(6, 8)), HEIGHTCM = c(60, 68))
  expect_warning(suppressMessages(cleangrowth(sw, input_format = "semi_wide",
         output_format = "long", quietly = TRUE)),
         regexp = "'WEIGHTKG' are factors")
  r <- suppressWarnings(suppressMessages(cleangrowth(sw, input_format = "semi_wide",
         output_format = "long", quietly = TRUE)))
  # values from a factor column are taken by their labels; a factor and a
  # numeric value column together come back as text
  expect_equal(r[param == "WEIGHTKG", measurement], c("6", "8"))
  expect_equal(as.character(r$exclude), rep("Include", 4))
  wd <- data.table(subjid = c("A", "B"), sex = c(0L, 1L), agedays_1 = c(100L, 50L),
                   WEIGHTKG_1 = factor(c(6, 4)), HEIGHTCM_1 = c(60, 55))
  r <- suppressWarnings(suppressMessages(cleangrowth(wd, input_format = "wide",
         output_format = "long", quietly = TRUE,
         wide_cols = list(WEIGHTKG = "WEIGHTKG", HEIGHTCM = "HEIGHTCM"))))
  expect_equal(r[param == "WEIGHTKG", measurement], c("6", "4"))
})

test_that("a character value cell with unit text -> that cell is Missing-Info", {
  sw <- data.frame(subjid = c("A", "A"), agedays = c(100L, 200L), sex = c(0L, 0L),
                   WEIGHTKG = c("6 kg", "8"), HEIGHTCM = c(60, 68), stringsAsFactors = FALSE)
  e <- SWx(sw)
  expect_true("Exclude-Missing-Info" %in% e)
})

test_that("NA subjid on a visit row -> that visit's measurements are Missing-Info", {
  sw <- data.table(subjid = c(NA, "B"), agedays = c(100L, 200L), sex = c(0L, 1L),
                   WEIGHTKG = c(6, 8), HEIGHTCM = c(60, 68))
  e <- SWx(sw)
  expect_equal(sum(e == "Exclude-Missing-Info"), 2L)  # the NA visit's wt + ht
})

# --- Where reshape intentionally differs from long format. -------------------

test_that("a NaN value cell is dropped as empty (no Missing-Info row)", {
  # In long, NaN measurement -> Exclude-Missing-Info. In semi-wide/wide a NaN
  # cell reads as "no measurement here" (is.na(NaN) is TRUE) and is dropped, the
  # same as an empty cell -- so no row is emitted for it.
  sw <- data.table(subjid = c("A", "A"), agedays = c(100L, 200L), sex = 0L,
                   WEIGHTKG = c(NaN, 8), HEIGHTCM = c(60, 68))
  e <- SWx(sw)
  expect_equal(length(e), 3L)                 # 1 wt + 2 ht, not 4
  expect_false("Exclude-Missing-Info" %in% e)
})

test_that("an unmapped extra column is ignored (not Not-GC-Param)", {
  # In long, param = "BMI" -> Exclude-Not-GC-Param. In semi-wide the parameter
  # is the column identity, so an unmapped column is simply not a measurement.
  sw <- data.table(subjid = c("A", "A"), agedays = c(100L, 200L), sex = 0L,
                   WEIGHTKG = c(6, 8), HEIGHTCM = c(60, 68), BMI = c(20, 21))
  e <- SWx(sw)
  expect_equal(length(e), 4L)                 # only WT + HT melted
  expect_false("Exclude-Not-GC-Param" %in% e)
})

test_that("an all-empty parameter column warns (not errors) and the rest cleans", {
  # In long, an all-missing measurement column errors. In semi-wide an empty
  # parameter column just means that parameter was not measured -- a legitimate
  # ragged shape -- so it warns and the other parameters clean.
  sw <- data.table(subjid = c("A", "A"), agedays = c(100L, 200L), sex = 0L,
                   WEIGHTKG = c(NA_real_, NA_real_), HEIGHTCM = c(60, 68))
  expect_warning(
    cleangrowth(sw, input_format = "semi_wide", output_format = "long", quietly = TRUE),
    "No weight measurements")
  e <- SWx(sw)
  expect_equal(length(e), 2L)                 # the two HT rows
  expect_true(all(e == "Include"))
})

# ---------------------------------------------------------------------------
# Adult rows with a missing (NA/NaN) measurement -- a param row present but no
# value. These leak into the adult algorithm's derived-quantity steps (BMI in
# eval_1d, EWMA, threshold comparisons) as NA and previously crashed the if()
# guards that consume them ("missing value where TRUE/FALSE needed"). The fix
# sets such rows aside as Exclude-Missing up front inside cleanadult() so they
# never participate. The missing row must report Missing-Info; the real row must
# still clean. Runs at every permissiveness level (the 1D BMI check and the
# loosest-only 1B BMI check are both on the crash path).
# ---------------------------------------------------------------------------
adult_pair <- function(ht, wt, age = 10958L) data.table(
  subjid = c("a", "a"), param = c("HEIGHTCM", "WEIGHTKG"), sex = 0L,
  agedays = c(age, age), measurement = c(ht, wt), id = 1:2)

test_that("adult same-day pair with one NA/NaN measurement does not crash (all levels)", {
  for (lvl in c("loosest", "looser", "tighter", "tightest")) {
    e <- excl(adult_pair(175, NA_real_), adult_permissiveness = lvl)
    expect_equal(e, c("Include", "Exclude-Missing-Info"), info = paste("WT NA,", lvl))
    e <- excl(adult_pair(NA_real_, 80), adult_permissiveness = lvl)
    expect_equal(e, c("Exclude-Missing-Info", "Include"), info = paste("HT NA,", lvl))
    # 0 -> NaN upstream reaches the adult path the same way
    e <- excl(adult_pair(175, 0), adult_permissiveness = lvl)
    expect_equal(e[1], "Include", info = paste("WT 0,", lvl))
    expect_false(e[2] == "Include", info = paste("WT 0,", lvl))
  }
})

test_that("adult longitudinal series with one NA mid-series cleans (no crash)", {
  ages <- as.integer((30:36) * 365.25)
  d <- rbindlist(list(
    data.table(subjid = "a", param = "HEIGHTCM", sex = 0L, agedays = ages,
               measurement = c(175, 175, 176, 175, 176, 175, 176)),
    data.table(subjid = "a", param = "WEIGHTKG", sex = 0L, agedays = ages,
               measurement = c(80, NA_real_, 82, 81, 83, 82, 84))))
  d[, id := seq_len(.N)]
  for (lvl in c("loosest", "looser", "tighter", "tightest")) {
    e <- excl(d, adult_permissiveness = lvl)
    expect_equal(sum(e == "Exclude-Missing-Info"), 1L, info = lvl)
    expect_true(sum(e == "Include") >= 12L, info = lvl)   # the 13 real values
  }
})

test_that("adult missing-value fuzz over the regression fixture never crashes", {
  # Belt-and-suspenders breadth check for the missing-measurement class: take the
  # full adult regression fixture (256 subjects, real trajectories exercising
  # every adult step) and blank one measurement per subject, then confirm the run
  # completes and every blanked row reports Exclude-Missing-Info. A missing value
  # left in play propagates NA into a derived-quantity comparison (BMI, EWMA,
  # median) and crashes an if() guard; injecting a hole into each subject at a
  # different position sweeps many step combinations at once.
  fx <- as.data.table(fread(
    system.file("testdata", "adult-gc-test-ALL-PHASES.csv", package = "growthcleanr")))
  base <- fx[, .(id = seq_len(.N), subjid, param, agedays, sex, measurement)]

  inject_at <- function(pick) {
    d <- copy(base)
    # deterministic per-subject position: a mid (embedded) hole and a last (edge) hole
    idx <- d[, .(row = if (pick == "mid") .I[ceiling(.N / 2)] else .I[.N]), by = subjid]$row
    d[idx, measurement := NA_real_]
    list(d = d, injected = d$id[idx])
  }

  for (pick in c("mid", "last")) {
    ij <- inject_at(pick)
    for (lvl in c("looser", "loosest")) {   # loosest also exercises the 1B BMI check
      r <- as.data.table(suppressWarnings(suppressMessages(
        cleangrowth(ij$d, adult_permissiveness = lvl, quietly = TRUE))))
      inj <- r[id %in% ij$injected]
      expect_equal(nrow(inj), length(ij$injected), info = paste(pick, lvl))
      expect_true(all(inj$exclude == "Exclude-Missing-Info"),
                  info = paste(pick, lvl,
                               "- non-missing:", paste(unique(inj$exclude), collapse = ",")))
    }
  }
})
