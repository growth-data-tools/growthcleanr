testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Semi-wide input (input_format = "semi_wide", output_format = "long")
#
# Verifies the semi-wide -> long reshape adapter: one row per visit, one column
# per parameter, reshaped to canonical long form BEFORE cleaning so results are
# identical to submitting the same measurements in long format. Covers the
# keystone long/semi-wide equivalence, column remapping, empty cells, same-day
# extraneous values, the missing-relevant-parameter warnings, the date route,
# determinism, and the error catalog.
#
# Scope: semi-wide INPUT -> long OUTPUT. Re-widened output is covered in
# test-semiwide-output.R.
# =============================================================================

# Small hand-built semi-wide fixture: 2 subjects, WT/HT/HC.
.mk_semiwide <- function() {
  data.table(
    subjid   = c("A", "A", "A", "B", "B"),
    agedays  = c(100L, 200L, 300L, 50L, 120L),
    sex      = c(0L, 0L, 0L, 1L, 1L),
    WEIGHTKG = c(6.0, 8.0, 9.5, 4.0, 5.5),
    HEIGHTCM = c(60.0, 68.0, NA, 55.0, 60.0),
    HEADCM   = c(42.0, NA, NA, 38.0, NA)
  )
}

# The equivalent long form of .mk_semiwide(): melt the parameter columns and
# drop empty cells (derived, not hand-transcribed).
.mk_long_equiv <- function() {
  m <- melt(.mk_semiwide(),
            id.vars = c("subjid", "agedays", "sex"),
            measure.vars = c("WEIGHTKG", "HEIGHTCM", "HEADCM"),
            variable.name = "param", value.name = "measurement")
  m <- m[!is.na(measurement)]
  m[, param := as.character(param)]
  setcolorder(m, c("subjid", "param", "agedays", "sex", "measurement"))
  m[]
}

.align_exclude <- function(res) {
  d <- as.data.table(res)
  d <- d[, .(subjid = as.character(subjid), param = as.character(param),
             agedays = as.integer(agedays), measurement = as.numeric(measurement),
             exclude = as.character(exclude))]
  setkey(d, subjid, param, agedays, measurement)
  d
}

# ---------------------------------------------------------------------------
# Test 1: KEYSTONE -- semi-wide reshape produces identical results to long
# (real dataset). Dedupe same-day/same-param rows so the cast is unambiguous;
# SDE ordering is tested separately below.
# ---------------------------------------------------------------------------
test_that("semi-wide input matches long input on a real dataset (keystone)", {
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  sg <- sg[agedays < 20 * 365.25]
  sg <- sg[subjid %in% unique(sg$subjid)[1:60]]
  setkey(sg, subjid, param, agedays)
  sg <- sg[!duplicated(sg, by = c("subjid", "param", "agedays"))]
  sg[, id := seq_len(.N)]

  r_long <- cleangrowth(sg, quietly = TRUE)
  sw     <- dcast(sg, subjid + agedays + sex ~ param, value.var = "measurement")
  r_sw   <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long", quietly = TRUE))

  a <- .align_exclude(r_long)
  b <- .align_exclude(r_sw)
  m <- merge(a, b, by = c("subjid", "param", "agedays", "measurement"),
             all = TRUE, suffixes = c("_long", "_sw"))
  expect_equal(nrow(m), nrow(a))
  expect_false(any(is.na(m$exclude_long)) || any(is.na(m$exclude_sw)))
  expect_identical(m$exclude_long, m$exclude_sw)
})

# ---------------------------------------------------------------------------
# Test 2: small hand-built equivalence (WT/HT/HC, with empty cells)
# ---------------------------------------------------------------------------
test_that("semi-wide reshape matches hand-built long (empty cells dropped)", {
  sw  <- .mk_semiwide()
  lng <- .mk_long_equiv()
  lng[, id := seq_len(.N)]

  r_sw  <- suppressWarnings(cleangrowth(sw,  input_format = "semi_wide", output_format = "long", quietly = TRUE))
  r_lng <- cleangrowth(lng, quietly = TRUE)

  # Same set of measurements survives the reshape (no extra/dropped rows).
  expect_equal(nrow(as.data.table(r_sw)), nrow(lng))
  a <- .align_exclude(r_lng)
  b <- .align_exclude(r_sw)
  m <- merge(a, b, by = c("subjid", "param", "agedays", "measurement"),
             all = TRUE, suffixes = c("_long", "_sw"))
  expect_equal(nrow(m), nrow(a))
  expect_identical(m$exclude_long, m$exclude_sw)
})

# ---------------------------------------------------------------------------
# Test 3: column remapping via semiwide_cols
# ---------------------------------------------------------------------------
test_that("semiwide_cols remaps parameter value columns", {
  sw <- .mk_semiwide()
  setnames(sw, c("WEIGHTKG", "HEIGHTCM", "HEADCM"), c("wt_kg", "ht_cm", "head_cm"))

  r <- suppressWarnings(cleangrowth(
    sw, input_format = "semi_wide", output_format = "long",
    semiwide_cols = list(WEIGHTKG = "wt_kg", HEIGHTCM = "ht_cm", HEADCM = "head_cm"),
    quietly = TRUE))

  # Same reshaped result as the canonical-named version.
  r_ref <- suppressWarnings(cleangrowth(.mk_semiwide(), input_format = "semi_wide", output_format = "long", quietly = TRUE))
  expect_identical(.align_exclude(r)$exclude, .align_exclude(r_ref)$exclude)
  expect_true(all(as.character(unique(r$param)) %in% c("WEIGHTKG", "HEIGHTCM", "HEADCM")))
})

# ---------------------------------------------------------------------------
# Test 4: structural columns can be renamed (subjid/sex/agedays)
# ---------------------------------------------------------------------------
test_that("semi-wide structural columns can be remapped", {
  sw <- .mk_semiwide()
  setnames(sw, c("subjid", "sex", "agedays"), c("patid", "male", "age_d"))

  r <- suppressWarnings(cleangrowth(
    sw, input_format = "semi_wide", output_format = "long",
    subjid = "patid", sex = "male", agedays = "age_d", quietly = TRUE))
  expect_equal(nrow(as.data.table(r)), 11L)
  # structural columns come back under the user's own names
  expect_true(all(c("patid", "param", "age_d", "male", "measurement", "gc_internal_id", "exclude")
                  %in% names(r)))
  expect_false(any(c("subjid", "agedays", "sex") %in% names(r)))
  expect_equal(sort(unique(r$patid)), c("A", "B"))
  # under checkpointing too
  rc <- suppressWarnings(cleangrowth(
    sw, input_format = "semi_wide", output_format = "long",
    subjid = "patid", sex = "male", agedays = "age_d", quietly = TRUE,
    checkpoint_dir = file.path(tempdir(), "gc-sw-names"), checkpoint_size = 1L))
  expect_equal(as.data.table(rc), as.data.table(r), ignore_attr = TRUE)
  # no user id column -> no `id` column (growthcleanr's number is gc_internal_id)
  expect_false("id" %in% names(r))
})

test_that("long output from semi-wide input carries the user's other columns", {
  sw <- .mk_semiwide()
  sw[, `:=`(visit = paste0("v", 1:5), site = c("x", "x", "x", "y", "y"),
            exclude = "old", param = "p")]
  r <- expect_warning(cleangrowth(sw, input_format = "semi_wide", output_format = "long",
                                  id = "visit", quietly = TRUE),
                      regexp = "'userinput_exclude', 'userinput_param'")
  r <- as.data.table(r)
  expect_equal(nrow(r), 11L)
  # user id first, carried columns appended, repeated on each measurement of a row
  expect_equal(names(r)[1:2], c("visit", "gc_internal_id"))
  expect_equal(tail(names(r), 3), c("site", "userinput_exclude", "userinput_param"))
  vrow <- match(r$visit, sw$visit)
  expect_equal(r$site, sw$site[vrow])
  expect_true(all(r$userinput_exclude == "old"))
  # growthcleanr's own columns are untouched
  expect_true(all(r$param %in% c("WEIGHTKG", "HEIGHTCM", "HEADCM")))
  # carry_through = FALSE drops them
  r0 <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long",
                                     id = "visit", quietly = TRUE, carry_through = FALSE))
  expect_false(any(c("site", "userinput_exclude", "userinput_param") %in% names(r0)))
  # checkpointed run gives the same table
  rc <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long",
                                     id = "visit", quietly = TRUE,
                                     checkpoint_dir = file.path(tempdir(), "gc-sw-carry"),
                                     checkpoint_size = 1L))
  expect_equal(as.data.table(rc), r, ignore_attr = TRUE)
})

test_that("a semi-wide id column named like a growthcleanr column is renamed, not overwritten", {
  sw <- .mk_semiwide()
  sw[, exclude := paste0("v", 1:5)]
  r <- expect_warning(cleangrowth(sw, input_format = "semi_wide", output_format = "long",
                                  id = "exclude", quietly = TRUE),
                      regexp = "'userinput_exclude'")
  r <- as.data.table(r)
  expect_equal(names(r)[1], "userinput_exclude")
  expect_true(all(grepl("^(Include|Exclude)", as.character(r$exclude))))
  expect_false(any(as.character(r$exclude) %in% sw$exclude))
  sw2 <- .mk_semiwide(); sw2[, gc_internal_id := 101:105]
  r2 <- as.data.table(suppressWarnings(cleangrowth(sw2, input_format = "semi_wide",
          output_format = "long", id = "gc_internal_id", quietly = TRUE)))
  expect_equal(r2$gc_internal_id, seq_len(11L))
  expect_true(all(r2$userinput_gc_internal_id %in% 101:105))
})

test_that("semi-wide date route carries dob and visit_date into long output", {
  sw <- .mk_semiwide()
  sw[, dob := as.Date("2020-01-01")][, visit_date := dob + agedays][, agedays := NULL]
  r <- as.data.table(suppressWarnings(cleangrowth(sw, input_format = "semi_wide",
         output_format = "long", dob = "dob", visit_date = "visit_date", quietly = TRUE)))
  expect_true(all(c("dob", "visit_date") %in% names(r)))
  expect_equal(as.numeric(r$visit_date - r$dob), r$agedays)
})

# ---------------------------------------------------------------------------
# Test 5: same-day extraneous values (two visit rows, same subjid+agedays)
# ---------------------------------------------------------------------------
test_that("same-day duplicate visits are resolved as SDEs", {
  sw <- data.table(
    subjid   = c("A", "A", "A", "A"),
    agedays  = c(100L, 100L, 200L, 300L),   # two visit rows share agedays 100
    sex      = 0L,
    WEIGHTKG = c(6.0, 6.2, 8.0, 9.5),
    HEIGHTCM = c(60.0, 60.0, 68.0, 74.0)
  )
  r  <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long", quietly = TRUE))
  wt <- as.data.table(r)[param == "WEIGHTKG" & agedays == 100L]
  expect_equal(nrow(wt), 2L)
  expect_equal(sort(as.character(wt$exclude)), c("Exclude-Extraneous", "Include"))
})

# ---------------------------------------------------------------------------
# Test 6: date route (dob + visit_date) equals agedays route
# ---------------------------------------------------------------------------
test_that("semi-wide date route matches the agedays route", {
  dob <- as.Date("2020-01-01")
  swd <- data.table(
    subjid = "A", dob = "2020-01-01",
    visit_date = as.character(dob + c(100L, 200L, 300L)),
    sex = 0L, WEIGHTKG = c(6, 8, 9.5), HEIGHTCM = c(60, 68, 74))
  swa <- data.table(
    subjid = "A", agedays = c(100L, 200L, 300L),
    sex = 0L, WEIGHTKG = c(6, 8, 9.5), HEIGHTCM = c(60, 68, 74))

  rd <- suppressWarnings(cleangrowth(swd, input_format = "semi_wide", output_format = "long",
                                     dob = "dob", visit_date = "visit_date", quietly = TRUE))
  ra <- suppressWarnings(cleangrowth(swa, input_format = "semi_wide", output_format = "long", quietly = TRUE))
  expect_equal(sort(unique(rd$agedays)), c(100L, 200L, 300L))
  expect_identical(.align_exclude(rd)$exclude, .align_exclude(ra)$exclude)
})

# ---------------------------------------------------------------------------
# Test 7: missing-relevant-parameter warnings
# ---------------------------------------------------------------------------
test_that("missing weight column warns", {
  sw <- data.table(subjid = "A", agedays = c(100L, 200L), sex = 0L,
                   HEIGHTCM = c(60, 68))
  expect_warning(cleangrowth(sw, input_format = "semi_wide", output_format = "long", quietly = TRUE),
                 "No weight measurements")
})

test_that("missing HC column does not warn", {
  # Under-2 rows present with no HC column is silent (no HC-absent warning).
  sw_young <- data.table(subjid = "A", agedays = c(100L, 200L), sex = 0L,
                         WEIGHTKG = c(6, 8), HEIGHTCM = c(60, 68))
  expect_silent(suppressMessages(
    cleangrowth(sw_young, input_format = "semi_wide", output_format = "long", quietly = TRUE)))
})

# ---------------------------------------------------------------------------
# Test 8: determinism / order independence
# ---------------------------------------------------------------------------
test_that("semi-wide result is deterministic and subject-order independent", {
  sw <- .mk_semiwide()
  r1 <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long", quietly = TRUE))
  r2 <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long", quietly = TRUE))
  expect_identical(.align_exclude(r1), .align_exclude(r2))

  # Shuffle the visit-row order of subjects -> identical per-subject results.
  sw_shuf <- sw[c(4, 5, 1, 2, 3)]
  r3 <- suppressWarnings(cleangrowth(sw_shuf, input_format = "semi_wide", output_format = "long", quietly = TRUE))
  expect_identical(.align_exclude(r1)$exclude, .align_exclude(r3)$exclude)
})

# ---------------------------------------------------------------------------
# Test 9: error catalog
# ---------------------------------------------------------------------------
test_that("semi-wide errors on bad input", {
  sw <- .mk_semiwide()

  # No data.
  expect_error(cleangrowth(input_format = "semi_wide", output_format = "long"),
               "requires a data.frame")

  # No recognized parameter column.
  bad <- data.table(subjid = "A", agedays = 100L, sex = 0L, foo = 1)
  expect_error(cleangrowth(bad, input_format = "semi_wide", output_format = "long"),
               "No recognized parameter column")

  # semiwide_cols maps to a missing column.
  expect_error(
    cleangrowth(sw, input_format = "semi_wide", output_format = "long",
                semiwide_cols = list(WEIGHTKG = "not_a_col")),
    "not found")

  # semiwide_cols with an unrecognized param name.
  expect_error(
    cleangrowth(sw, input_format = "semi_wide", output_format = "long",
                semiwide_cols = list(WEIGHT = "WEIGHTKG")),
    "growthcleanr parameters")

  # Missing subjid column.
  nosub <- copy(sw); setnames(nosub, "subjid", "pid")
  expect_error(cleangrowth(nosub, input_format = "semi_wide", output_format = "long"),
               "subjid column")

  # Wide output requires wide input (semi-wide -> wide is not supported;
  # growthcleanr cannot infer observation blocks from visit rows).
  expect_error(cleangrowth(sw, input_format = "semi_wide", output_format = "wide"),
               "requires input_format = 'wide'")
})

# ---------------------------------------------------------------------------
# Test 10: long input unaffected (default path byte-identical)
# ---------------------------------------------------------------------------
test_that("input_format = 'long' (default) is unchanged", {
  lng <- .mk_long_equiv()
  lng[, id := seq_len(.N)]
  r_default  <- cleangrowth(lng, quietly = TRUE)
  r_explicit <- cleangrowth(lng, input_format = "long", quietly = TRUE)
  expect_identical(as.data.table(r_default), as.data.table(r_explicit))
})

# ---------------------------------------------------------------------------
# Test 11: length vs height is a long-format-only feature
#
# Semi-wide input always uses default handling of linear measurements, so
# length.adjust is rejected; a LENGTHCM/LENGTHIN value at any age gets default
# handling (the same result as mapping the column to HEIGHTCM).
# ---------------------------------------------------------------------------
test_that("semi-wide rejects length.adjust; length at >= 2 years gets default handling", {
  sw <- data.table(
    subjid  = "A",
    agedays = c(100L, 400L, 800L, 1200L),   # 800, 1200 are >= 2y (day 730 on)
    sex     = 0L,
    wt      = c(6.0, 10.0, 13.0, 15.0),
    len     = c(55.0, 70.0, 88.0, 95.0))
  cols <- list(WEIGHTKG = "wt", LENGTHCM = "len")

  # length.adjust = TRUE is refused for semi-wide (long-only feature).
  expect_error(
    cleangrowth(sw, input_format = "semi_wide", semiwide_cols = cols,
                length.adjust = TRUE, quietly = TRUE),
    "length.adjust = TRUE .*requires long input")

  # A LENGTHCM value at a pediatric age >= 2 years gets default handling: the
  # same result as mapping the column to HEIGHTCM.
  r_len <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide",
                                        semiwide_cols = cols, output_format = "long",
                                        quietly = TRUE))
  r_ht  <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide",
                                        semiwide_cols = list(WEIGHTKG = "wt", HEIGHTCM = "len"),
                                        output_format = "long", quietly = TRUE))
  expect_equal(as.character(r_len$exclude), as.character(r_ht$exclude))

  # LENGTHIN (imperial) also gets default handling (no error).
  swin <- copy(sw); setnames(swin, "len", "len_in")
  swin[, len_in := len_in / 2.54]
  expect_no_error(
    suppressWarnings(cleangrowth(
      swin, input_format = "semi_wide",
      semiwide_cols = list(WEIGHTKG = "wt", LENGTHIN = "len_in"), quietly = TRUE)))
})

test_that("semi-wide allows length under 2 years and length mapped to height", {
  # Every length value is < 2 years -> allowed (default handling = recumbent).
  sw_young <- data.table(
    subjid  = "A",
    agedays = c(100L, 400L, 700L, 720L),
    sex     = 0L,
    wt      = c(6.0, 10.0, 12.0, 12.5),
    len     = c(55.0, 70.0, 80.0, 82.0))
  expect_error(
    suppressWarnings(cleangrowth(
      sw_young, input_format = "semi_wide",
      semiwide_cols = list(WEIGHTKG = "wt", LENGTHCM = "len"), quietly = TRUE)),
    NA)

  # Length column mapped to HEIGHTCM -> allowed at any age (default handling).
  sw_old <- data.table(
    subjid  = "A",
    agedays = c(800L, 1200L),
    sex     = 0L,
    wt      = c(13.0, 15.0),
    len     = c(88.0, 95.0))
  expect_error(
    suppressWarnings(cleangrowth(
      sw_old, input_format = "semi_wide",
      semiwide_cols = list(WEIGHTKG = "wt", HEIGHTCM = "len"), quietly = TRUE)),
    NA)
})

test_that("semi-wide length restriction exempts adult rows", {
  # Adult rows (age >= adult_cutpoint) ignore the length/height label, so a
  # LENGTHCM column at adult ages is accepted as submitted.
  sw_adult <- data.table(
    subjid  = "A",
    agedays = as.integer(c(30, 40) * 365.25),
    sex     = 0L,
    wt      = c(80.0, 82.0),
    len     = c(170.0, 171.0))
  expect_error(
    suppressWarnings(cleangrowth(
      sw_adult, input_format = "semi_wide",
      semiwide_cols = list(WEIGHTKG = "wt", LENGTHCM = "len"), quietly = TRUE)),
    NA)

  # A 19-year-old length value runs at either cutpoint (default handling when
  # pediatric, ignored label when adult).
  sw_19 <- data.table(
    subjid  = "A",
    agedays = as.integer(c(1, 19) * 365.25),
    sex     = 0L,
    wt      = c(10.0, 60.0),
    len     = c(76.0, 175.0))
  expect_error(
    suppressWarnings(cleangrowth(
      sw_19, input_format = "semi_wide",
      semiwide_cols = list(WEIGHTKG = "wt", LENGTHCM = "len"), quietly = TRUE)),
    NA)
  expect_error(
    suppressWarnings(cleangrowth(
      sw_19, input_format = "semi_wide",
      semiwide_cols = list(WEIGHTKG = "wt", LENGTHCM = "len"),
      adult_cutpoint = 18, quietly = TRUE)),
    NA)
})

test_that("semi-wide LENGTHCM at day 729 and day 730 both run (default handling)", {
  mk <- function(a) data.table(subjid = "A", agedays = c(100L, 400L, a), sex = 0L,
                               wt = c(6.0, 10.0, 12.0), len = c(55.0, 70.0, 84.0))
  cols <- list(WEIGHTKG = "wt", LENGTHCM = "len")
  expect_no_error(
    suppressWarnings(cleangrowth(mk(730L), input_format = "semi_wide",
                                 semiwide_cols = cols, quietly = TRUE)))
  expect_no_error(
    suppressWarnings(cleangrowth(mk(729L), input_format = "semi_wide",
                                 semiwide_cols = cols, quietly = TRUE)))
})

# ---------------------------------------------------------------------------
# Long output from semi-wide input: a column with the user's name holds only the
# user's values; growthcleanr's per-measurement number is gc_internal_id.
# ---------------------------------------------------------------------------
test_that("semi-wide -> long returns the user's id under its own name, not growthcleanr's numbers", {
  sw <- data.table(id = c("v1", "v2"), subjid = "A", sex = 0L,
                   agedays = c(100L, 400L), WEIGHTKG = c(6, 10), HEIGHTCM = c(60, 75))
  # a column named `id` is used without passing id =
  r <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide", output_format = "long",
                                    quietly = TRUE))
  expect_equal(r$id, c("v1", "v1", "v2", "v2"))
  expect_equal(r$gc_internal_id, 1:4)
  # a differently named id column via id =
  sw2 <- copy(sw); setnames(sw2, "id", "visit")
  r2 <- suppressWarnings(cleangrowth(sw2, input_format = "semi_wide", output_format = "long",
                                     id = "visit", quietly = TRUE))
  expect_equal(r2$visit, c("v1", "v1", "v2", "v2"))
  expect_false("id" %in% names(r2))
  # semi-wide output: the id column comes back unchanged
  r3 <- suppressWarnings(cleangrowth(sw2, input_format = "semi_wide", id = "visit", quietly = TRUE))
  expect_equal(r3$visit, c("v1", "v2"))
})
