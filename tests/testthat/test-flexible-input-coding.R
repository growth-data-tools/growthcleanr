testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Flexible sex / param input coding.
#
# gc accepts a wider range of sex and param codings (case-insensitive word
# forms, user-supplied maps) while guarding against silent miscoding:
#   - sex: default codes 0/1, m/f, b/g, male/female, boy/girl; user sex_coding
#     replaces the defaults; present-but-unrecognized -> Exclude-Missing-Info +
#     soft warning; a 1=male/2=female column is a HARD STOP (gc reads 1 as
#     female) unless the user declares it via sex_coding.
#   - param: the spelled-out canonical names are matched case-insensitively;
#     there are NO built-in abbreviations (a bare "wt"/"ht"/"hc" is NOT assumed
#     to be a gc parameter -- it may be a sample weight, etc. -- because of the
#     abbreviation-collision risk). Any
#     non-canonical label must be declared via param_coding (which defaults to
#     a metric target, or routes to an imperial canonical); new HEADIN parameter
#     (inches -> HEADCM); unrecognized -> Not-GC-Param + soft warning.
#   - a >20% share of a parameter's cleaned values in the unit-error range emits
#     a soft warning (whole column may be in the wrong unit).
# =============================================================================

# One child subject with a plausible WT+HT trajectory; sex/param codings vary.
mk <- function(sexval = 0, param_wt = "WEIGHTKG", param_ht = "HEIGHTCM",
               subjid = 1L, wt = c(7, 9, 11, 13), ht = c(65, 75, 82, 88)) {
  ages <- c(90L, 365L, 730L, 1095L)
  d <- rbind(
    data.table(subjid = subjid, param = param_wt, agedays = ages, sex = sexval, measurement = wt),
    data.table(subjid = subjid, param = param_ht, agedays = ages, sex = sexval, measurement = ht)
  )
  d[, id := seq_len(.N)]
  d[]
}
run <- function(d, ...) as.data.table(
  suppressWarnings(suppressMessages(cleangrowth(d, quietly = TRUE, ...))))

# ---------------------------------------------------------------------------
# Sex: recognized codings
# ---------------------------------------------------------------------------
test_that("male sex codes 0/m/M/b/B/male/Male/BOY all resolve to gc_internal_sex 0", {
  for (s in list(0, "m", "M", "b", "B", "male", "Male", " boy ", "BOY")) {
    r <- run(mk(s), full_detail = TRUE)
    expect_equal(unique(r$gc_internal_sex), 0L, info = as.character(s)[1])
  }
})

test_that("female sex codes 1/f/g/G/female/girl all resolve to gc_internal_sex 1", {
  for (s in list(1, "f", "F", "g", "G", "female", "GIRL")) {
    r <- run(mk(s), full_detail = TRUE)
    expect_equal(unique(r$gc_internal_sex), 1L, info = as.character(s)[1])
  }
})

test_that("output sex column echoes the user's verbatim submission (not 0/1)", {
  r <- run(mk("Male"), full_detail = TRUE)
  expect_true(all(r$sex == "Male"))
  expect_equal(unique(r$gc_internal_sex), 0L)
})

# ---------------------------------------------------------------------------
# Sex: the 1=male/2=female hard stop
# ---------------------------------------------------------------------------
test_that("an all-1/2 sex column is a hard stop under default coding", {
  d <- rbind(mk(1, subjid = 1L), mk(2, subjid = 2L))
  d[, id := seq_len(.N)]
  expect_error(cleangrowth(d, quietly = TRUE), "1=male/2=female")
})

test_that("declaring sex_coding = list(male=1, female=2) suppresses the stop and maps correctly", {
  d <- rbind(mk(1, subjid = 1L), mk(2, subjid = 2L))
  d[, id := seq_len(.N)]
  r <- run(d, sex_coding = list(male = 1, female = 2), full_detail = TRUE)
  expect_equal(unique(r[subjid == 1]$gc_internal_sex), 0L)  # 1 -> male
  expect_equal(unique(r[subjid == 2]$gc_internal_sex), 1L)  # 2 -> female
})

test_that("the 1/2 stop does NOT fire when 0/1 coding is present", {
  d <- rbind(mk(0, subjid = 1L), mk(1, subjid = 2L))
  d[, id := seq_len(.N)]
  expect_silent(invisible(suppressMessages(cleangrowth(d, quietly = TRUE))))
})

test_that("an all-1 pediatric sex column warns loudly, cleans as female, and sex_coding removes the warning", {
  d <- rbind(mk(1, subjid = 1L), mk(1, subjid = 2L))
  d[, id := seq_len(.N)]
  expect_warning(r <- suppressMessages(cleangrowth(d, full_detail = TRUE, quietly = TRUE)),
                 "POSSIBLE SEX CODING ERROR.*every value in `sex` is 1")
  expect_equal(unique(r$gc_internal_sex), 1L)
  # one subject says nothing about the coding, so there is no warning
  d_one <- mk(1, subjid = 1L); d_one[, id := seq_len(.N)]
  expect_silent(invisible(suppressMessages(cleangrowth(d_one, quietly = TRUE))))
  # a 0 on an adult row reveals 0/1 coding, so there is no warning
  d_adult0 <- rbind(mk(1, subjid = 1L), data.table::copy(mk(0, subjid = 2L))[, agedays := agedays + 30 * 365L])
  d_adult0[, id := seq_len(.N)]
  w <- testthat::capture_warnings(suppressMessages(cleangrowth(d_adult0, quietly = TRUE)))
  expect_false(any(grepl("POSSIBLE SEX CODING ERROR", w, fixed = TRUE)))
  # explicit coding, in either direction, removes it; 1 = male then maps to male
  expect_silent(invisible(suppressMessages(
    cleangrowth(d, sex_coding = list(male = 0, female = 1), quietly = TRUE))))
  expect_silent(invisible(suppressMessages(
    r2 <- cleangrowth(d, sex_coding = list(male = 1, female = 2), full_detail = TRUE, quietly = TRUE))))
  expect_equal(unique(r2$gc_internal_sex), 0L)
  # text codes and all-0 do not trigger it
  d_f <- mk("F", subjid = 1L); d_f[, id := seq_len(.N)]
  expect_silent(invisible(suppressMessages(cleangrowth(d_f, quietly = TRUE))))
  d_0 <- mk(0, subjid = 1L); d_0[, id := seq_len(.N)]
  expect_silent(invisible(suppressMessages(cleangrowth(d_0, quietly = TRUE))))
})

# ---------------------------------------------------------------------------
# Sex: unrecognized codings
# ---------------------------------------------------------------------------
test_that("present-but-unrecognized pediatric sex warns (naming the code) and rows -> Exclude-Missing-Info", {
  d <- rbind(mk(0, subjid = 1L), mk("U", subjid = 2L))
  d[, id := seq_len(.N)]
  expect_warning(cleangrowth(d, quietly = TRUE), "Unrecognized sex.*values")
  r <- run(d)
  expect_true(all(r[subjid == 2]$exclude == "Exclude-Missing-Info"))
  expect_false(any(r[subjid == 1]$exclude == "Exclude-Missing-Info"))
})

test_that("an all-adult dataset with 1/2 sex coding cleans (sex not used for adults; no 1/2 stop)", {
  ages <- c(21L, 30L, 40L, 55L) * 365L
  d <- rbind(
    data.table(subjid = 1L, param = "WEIGHTKG", agedays = ages, sex = 1, measurement = c(80, 82, 81, 83)),
    data.table(subjid = 2L, param = "WEIGHTKG", agedays = ages, sex = 2, measurement = c(65, 66, 64, 67))
  )
  d[, id := seq_len(.N)]
  expect_error(suppressWarnings(cleangrowth(d, quietly = TRUE)), NA)  # no 1/2 stop
})

test_that("when NO pediatric row has a usable sex it is a clean stop", {
  expect_error(suppressWarnings(cleangrowth(mk("U"), quietly = TRUE)),
               "no pediatric measurement|No pediatric measurement")
})

# ---------------------------------------------------------------------------
# Sex: custom sex_coding
# ---------------------------------------------------------------------------
test_that("a fully custom sex_coding maps site-specific codes", {
  r <- run(mk("H"), sex_coding = list(male = c("h", "male"), female = c("d", "female")),
           full_detail = TRUE)
  expect_equal(unique(r$gc_internal_sex), 0L)
})

test_that("a code listed under both sexes is an error", {
  expect_error(
    cleangrowth(mk(0), sex_coding = list(male = c(0, "x"), female = c(1, "x")), quietly = TRUE),
    "both male and female")
})

# ---------------------------------------------------------------------------
# Param: no built-in abbreviations; only canonical names auto-recognized
# ---------------------------------------------------------------------------
test_that("bare WT/HT abbreviations are NOT recognized (need param_coding)", {
  # "wt" might be a sample weight, not body weight -- gc will not guess.
  expect_warning(cleangrowth(mk(0, param_wt = "WT", param_ht = "HT"), quietly = TRUE),
                 "Unrecognized parameters")
  r <- run(mk(0, param_wt = "WT", param_ht = "HT"))
  expect_true(all(r$exclude == "Exclude-Not-GC-Param"))
  # ... but param_coding rescues them, defaulting to the metric target.
  r2 <- run(mk(0, param_wt = "WT", param_ht = "HT"),
            param_coding = list(WEIGHTKG = "WT", HEIGHTCM = "HT"), full_detail = TRUE)
  expect_setequal(unique(r2$param), c("WT", "HT"))                      # verbatim
  expect_setequal(unique(r2$gc_internal_param), c("WEIGHTKG", "HEIGHTCM"))  # canonical
  expect_false(any(r2$exclude == "Exclude-Not-GC-Param"))
})

test_that("bare HC/HEAD/OFC are NOT recognized without param_coding", {
  for (tok in c("HC", "head", "OFC")) {
    d <- data.table(subjid = 1L, param = tok, agedays = c(30L, 120L, 300L, 600L),
                    sex = 0L, measurement = c(35, 40, 44, 47), id = 1:4)
    r <- run(d)
    expect_true(all(r$exclude == "Exclude-Not-GC-Param"), info = tok)
    r2 <- run(d, param_coding = list(HEADCM = tok), full_detail = TRUE)
    expect_equal(unique(r2$gc_internal_param), "HEADCM", info = tok)
  }
})

test_that("canonical param names still match case-insensitively", {
  r <- run(mk(0, param_wt = "weightkg", param_ht = "HeightCM"), full_detail = TRUE)
  expect_setequal(unique(r$gc_internal_param), c("WEIGHTKG", "HEIGHTCM"))
  expect_false(any(r$exclude == "Exclude-Not-GC-Param"))
})

# ---------------------------------------------------------------------------
# Param: param_coding (override + validation)
# ---------------------------------------------------------------------------
test_that("param_coding routes a bare column to an imperial parameter (verbatim preserved, converted)", {
  d <- mk(0, param_wt = "WT", param_ht = "HT")
  d[param == "WT", measurement := measurement * 2.20462]  # lbs
  d[param == "HT", measurement := measurement / 2.54]     # inches
  r <- run(d, param_coding = list(WEIGHTLBS = "WT", HEIGHTIN = "HT"), full_detail = TRUE)
  expect_setequal(unique(r$param), c("WT", "HT"))                          # verbatim
  expect_setequal(unique(r$gc_internal_param), c("WEIGHTKG", "HEIGHTCM"))  # converted target
  # converted internal weight is back near the plausible 7-13 kg
  expect_true(all(r[param == "WT"]$gc_internal_meas < 15))
})

test_that("param_coding with a non-canonical name is an error", {
  expect_error(cleangrowth(mk(0), param_coding = list(WGT = "WT"), quietly = TRUE),
               "canonical")
})

test_that("param_coding mapping one token to two parameters is an error", {
  expect_error(
    cleangrowth(mk(0), param_coding = list(WEIGHTKG = "x", WEIGHTLBS = "x"), quietly = TRUE),
    "both")
})

# ---------------------------------------------------------------------------
# Param: unrecognized -> Not-GC-Param + warning
# ---------------------------------------------------------------------------
test_that("an unrecognized param warns and is set aside as Exclude-Not-GC-Param", {
  d <- rbind(mk(0), data.table(subjid = 1L, param = "SBP", agedays = 365L, sex = 0,
                               measurement = 110, id = 99L))
  expect_warning(cleangrowth(d, quietly = TRUE), "Unrecognized parameters")
  r <- run(d)
  expect_equal(as.character(r[param == "SBP"]$exclude), "Exclude-Not-GC-Param")
})

# ---------------------------------------------------------------------------
# Param: HEADIN conversion
# ---------------------------------------------------------------------------
test_that("HEADIN is recognized and converts to HEADCM at 2.54 cm/in", {
  cm <- c(35, 40, 44, 47)
  d <- data.table(subjid = 1L, param = "HEADIN", agedays = c(30L, 120L, 300L, 600L),
                  sex = 0L, measurement = cm / 2.54, id = 1:4)
  r <- run(d, full_detail = TRUE)
  expect_equal(unique(r$gc_internal_param), "HEADCM")
  expect_equal(sort(round(r$gc_internal_meas, 4)), sort(cm))
  expect_true(all(r$param == "HEADIN"))  # verbatim echoed
})

# ---------------------------------------------------------------------------
# Param: LENGTHIN conversion (parallel to HEIGHTIN; converts to LENGTHCM, then
# treated as recumbent length like LENGTHCM)
# ---------------------------------------------------------------------------
test_that("LENGTHIN is recognized and converts to LENGTHCM at 2.54 cm/in", {
  cm <- c(52, 60, 66, 72)  # infant lengths (all under 2y ages)
  d <- data.table(subjid = 1L, param = "LENGTHIN", agedays = c(30L, 120L, 300L, 600L),
                  sex = 0L, measurement = cm / 2.54, id = 1:4)
  r <- run(d, full_detail = TRUE)
  # LENGTHIN -> LENGTHCM (imperial) -> HEIGHTCM (length relabel), same as LENGTHCM.
  expect_equal(unique(r$gc_internal_param), "HEIGHTCM")
  expect_equal(sort(round(r$gc_internal_meas, 4)), sort(cm))
  expect_true(all(r$param == "LENGTHIN"))            # verbatim echoed
  expect_false(any(r$exclude == "Exclude-Not-GC-Param"))
})

test_that("LENGTHIN cleans identically to the same values submitted as LENGTHCM", {
  cm <- c(52, 60, 66, 72)
  base <- function(p, m) data.table(subjid = 1L, param = p,
                                    agedays = c(30L, 120L, 300L, 600L),
                                    sex = 0L, measurement = m, id = 1:4)
  r_in <- run(base("LENGTHIN", cm / 2.54))
  r_cm <- run(base("LENGTHCM", cm))
  expect_equal(as.character(r_in$exclude), as.character(r_cm$exclude))
})

# ---------------------------------------------------------------------------
# Systematic unit-error warning
# ---------------------------------------------------------------------------
test_that(">20% of a param's cleaned values in the unit-error range warns", {
  d <- mk(0)
  d[param == "WEIGHTKG", measurement := measurement * 2.20462]  # lbs magnitudes labeled kg
  expect_warning(cleangrowth(d, quietly = TRUE), "unit-error range")
})

test_that("ordinary metric data does not emit the unit-error warning", {
  expect_warning(cleangrowth(mk(0), quietly = TRUE), NA)
})

test_that("the unit-error denominator excludes HC>5y (Exclude-HC-Out-of-Range) rows", {
  # HC cleaned past 5y is Exclude-HC-Out-of-Range and always
  # FALSE for unit_error_range; those rows must not dilute the hc denominator.
  # Here 1 of 3 young HC is in the unit-error range (33% > 20%), so the hc
  # warning fires and names "1 of 3"; if the 12 HC>5y rows were counted the share
  # would be 1/15 (7%) and no warning would fire. The "1 of 3" text proves the
  # >5y rows are excluded from the denominator.
  d <- data.table(
    subjid = "H", param = "HEADCM",
    agedays = c(180L, 240L, 300L, as.integer(seq(6, 12, length.out = 12) * 365.25)),
    sex = 0L, measurement = c(18.5, 44, 45, rep(45, 12)), id = seq_len(15))
  expect_warning(cleangrowth(d, unit_error_range = TRUE, quietly = TRUE),
                 "1 of 3 cleaned")
})
