library(growthcleanr)

# Minimal, CRAN-safe checks for the bundled Shiny app (growthcleanr_app()).
# These run on CRAN (near-zero cost): they confirm the launcher exists, the app
# directory ships, and app.R still parses -- guarding the historical package/path
# mismatch that broke the launcher, plus any syntax breakage in the app.
# Fuller testServer scenario cases (long / semi-wide / wide mapping) are added
# separately and gated with skip_on_cran().

test_that("growthcleanr_app() is exported with a stable signature", {
  expect_true(is.function(growthcleanr_app))
  expect_identical(names(formals(growthcleanr_app)), "...")
})

test_that("the Shiny app directory ships and app.R parses", {
  app_dir <- system.file("app", package = "growthcleanr")
  expect_true(nzchar(app_dir))

  app_file <- file.path(app_dir, "app.R")
  expect_true(file.exists(app_file))

  # Parses without error (guards syntax breakage in the shipped app).
  expect_error(parse(app_file), NA)
})

# ---------------------------------------------------------------------------
# Fuller server-logic tests (skip_on_cran): drive the app's read -> map path
# with shiny::testServer on in-memory fixtures. Covers the wide age-prefix
# validation (regression for the "prefix left at 'agedays' while columns are
# age_1/age_2" bug) and the sex-coding auto-detect / value tables.
# ---------------------------------------------------------------------------

.app_obj <- function() shiny::shinyAppDir(system.file("app", package = "growthcleanr"))

.write_csv <- function(d) {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(d, f, row.names = FALSE)
  f
}

test_that("wide: a wrong age prefix is rejected at mapping, not deferred to cleaning", {
  skip_on_cran()
  d <- data.frame(
    subjid = c("a", "b"), sex = c(0L, 1L),
    age_1 = c(365L, 400L), age_2 = c(730L, 800L),
    WEIGHTKG_1 = c(7, 8), WEIGHTKG_2 = c(9, 10),
    HEIGHTCM_1 = c(65, 66), HEIGHTCM_2 = c(75, 76),
    check.names = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f, name = basename(f), size = file.size(f)))
    session$setInputs(demo_choice = "wide")
    session$setInputs(w_subjid = "subjid", w_sex = "sex", w_dob = "",
                      w_agedays_prefix = "agedays", w_visit_date = "",
                      w_weight_prefix = "WEIGHTKG", w_height_prefix = "HEIGHTCM",
                      w_head_prefix = "", w_keep_only = FALSE)
    session$setInputs(map_btn = 1)
    expect_null(rv$mapped_df)             # wrong age prefix -> mapping fails at Step 2
    session$setInputs(w_agedays_prefix = "age", map_btn = 2)
    expect_false(is.null(rv$mapped_df))   # correct prefix -> mapping succeeds
    expect_true(all(c("subjid", "sex") %in% names(rv$mapped_df)))
  })
})

test_that("sex-coding auto-detect recognizes default pairs and skips 1/2", {
  skip_on_cran()
  # .detect_sex_coding() is a top-level helper in the shipped app.R; source the
  # app to reach it (updateTextInput does not reflect into input$ under
  # testServer, so the reactive wrapper is exercised in the app, but the coding
  # logic is verified here directly).
  app_env <- new.env()
  suppressWarnings(suppressMessages(
    source(file.path(system.file("app", package = "growthcleanr"), "app.R"), local = app_env)))
  detect <- app_env$.detect_sex_coding
  expect_true(is.function(detect))

  expect_equal(detect(c("M", "M", "F", "F")),   c(male = "M",   female = "F"))
  expect_equal(detect(c("boy", "girl", "boy")), c(male = "boy", female = "girl"))
  expect_equal(detect(c(0, 0, 1, 1)),           c(male = "0",   female = "1"))
  expect_equal(detect(c("b", "g")),             c(male = "b",   female = "g"))
  # 1/2 is not a gc default -> returns NULL so gc's 1=male/2=female guard fires.
  expect_null(detect(c("1", "1", "2", "2")))
  # Unrecognized values -> NULL.
  expect_null(detect(c("X", "Y", "Z")))
  # The > 99% rule: a small stray does not block detection.
  expect_equal(detect(c(rep("M", 200), rep("F", 190), "U")), c(male = "M", female = "F"))
})

test_that("value_freq / mapped column reactives track the sex and param selection", {
  skip_on_cran()
  d <- data.frame(subjid = c("a", "b"), sex = c("M", "F"),
                  param = c("WEIGHTKG", "HEIGHTCM"), agedays = c(365L, 400L),
                  measurement = c(7, 65))
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f, name = basename(f), size = file.size(f)))
    session$setInputs(demo_choice = "long", long_sex = "sex", long_param = "param")
    expect_equal(mapped_sex_col(), "sex")
    expect_equal(mapped_param_col(), "param")
    vf <- value_freq("sex")
    expect_true(all(c("M", "F") %in% vf$Value))
  })
})

test_that("semi-wide: custom columns map, measurement columns keep their names", {
  skip_on_cran()
  # Custom column names throughout, to prove every column keeps the user's name
  # and the mapping is recorded separately (measurement columns are routed by
  # semiwide_cols in Step 3, so gc can convert imperial units).
  d <- data.frame(pid = c("a", "b"), gender = c(0L, 1L), age_days = c(365L, 400L),
                  wt = c(7, 8), ht = c(65, 66), check.names = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f, name = basename(f), size = file.size(f)))
    session$setInputs(demo_choice = "semiwide",
                      sw_subjid = "pid", sw_sex = "gender", sw_agedays = "age_days",
                      sw_weight = "wt", sw_unit_weight = "kg",
                      sw_height = "ht", sw_unit_height = "cm",
                      sw_head = "", sw_unit_head = "cm",
                      sw_id = "", sw_dob = "", sw_visit_date = "",
                      sw_keep_only = FALSE)
    session$setInputs(map_btn = 1)
    expect_false(is.null(rv$mapped_df))
    expect_identical(names(rv$mapped_df), names(d))
    expect_identical(rv$mapped_roles$subjid, "pid")
    expect_identical(rv$mapped_roles$sex, "gender")
    expect_identical(rv$mapped_roles$agedays, "age_days")
  })
})

test_that("semi-wide: an all-miss measurement mapping fails at Step 2, not at cleaning", {
  skip_on_cran()
  d <- data.frame(pid = c("a", "b"), gender = c(0L, 1L), age_days = c(365L, 400L),
                  wt = c(7, 8), ht = c(65, 66), check.names = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f, name = basename(f), size = file.size(f)))
    # Leave the measurement fields at defaults that name no real column.
    session$setInputs(demo_choice = "semiwide",
                      sw_subjid = "pid", sw_sex = "gender", sw_agedays = "age_days",
                      sw_weight = "WEIGHTKG", sw_unit_weight = "kg",
                      sw_height = "HEIGHTCM", sw_unit_height = "cm",
                      sw_head = "", sw_unit_head = "cm",
                      sw_id = "", sw_dob = "", sw_visit_date = "",
                      sw_keep_only = FALSE)
    session$setInputs(map_btn = 1)
    expect_null(rv$mapped_df)  # no measurement column found -> mapping fails at Step 2
  })
})

# ---------------------------------------------------------------------------
# Hostile-input guards (non-numeric measurement column, same source column
# mapped to two fields, duplicate column names). Pure top-level helpers in
# app.R, unit-tested directly (CRAN-safe), plus a few testServer end-to-end
# refusals (skip_on_cran).
# ---------------------------------------------------------------------------

test_that("hostile-input guard helpers catch the target problems and pass clean input", {
  app_env <- new.env()
  suppressWarnings(suppressMessages(
    source(file.path(system.file("app", package = "growthcleanr"), "app.R"), local = app_env)))
  meas_err <- app_env$.gc_app_measurement_error
  dup_role <- app_env$.gc_app_dup_role_error
  dup_name <- app_env$.gc_app_dup_colname_note
  blk      <- app_env$.gc_app_block_cols
  expect_true(all(vapply(list(meas_err, dup_role, dup_name, blk), is.function, logical(1))))

  # (1) mostly non-numeric measurement column (likely the wrong column); a few
  # non-numbers are left for growthcleanr to set aside
  num_df <- data.frame(subjid = 1:2, w = c(10, 20), stringsAsFactors = FALSE)
  few_df <- data.frame(subjid = 1:3, w = c("N/A", "20", "21"), stringsAsFactors = FALSE)
  txt_df <- data.frame(subjid = 1:3, w = c("heavy", "light", "20"), stringsAsFactors = FALSE)
  expect_null(meas_err(num_df, "w"))          # numeric -> ok
  expect_null(meas_err(num_df, ""))           # blank field ignored
  expect_null(meas_err(num_df, "notacol"))    # absent field ignored
  expect_null(meas_err(few_df, "w"))          # a few non-numbers -> ok
  m <- meas_err(txt_df, "w")
  expect_true(is.character(m) && grepl("aren't numbers", m) && grepl("'w'", m) && grepl("heavy", m))

  # (2) same source column mapped to two fields
  expect_null(dup_role(c(A = "x", B = "y")))
  expect_null(dup_role(c(A = "", B = "")))    # all blank ignored
  d <- dup_role(c("Subject ID" = "id", "Sex" = "id", "Age (days)" = "age"))
  expect_true(grepl("more than one field", d) && grepl("Subject ID", d) &&
              grepl("Sex", d) && grepl("'id'", d))

  # (3) duplicate column names
  expect_null(dup_name(c("a", "b", "c")))
  n <- dup_name(c("subjid", "subjid", "agedays"))
  expect_true(grepl("same name", n) && grepl("subjid", n))

  # block-column finder used by the wide numeric check
  nms <- c("subjid", "WEIGHTKG_1", "WEIGHTKG_2", "WEIGHTKG_x", "HEIGHTCM_1")
  expect_equal(blk(nms, "WEIGHTKG"), c("WEIGHTKG_1", "WEIGHTKG_2"))
  expect_equal(blk(nms, ""), character(0))
})

.long_inputs <- function(f, ...) {
  base <- list(
    file_upload = list(datapath = f, name = basename(f), size = file.size(f)),
    demo_choice = "long", long_subjid = "subjid", long_sex = "sex",
    long_param = "param", long_measurement = "measurement", long_agedays = "agedays",
    long_id = "", long_dob = "", long_visit_date = "", long_date_order = "auto",
    val_weight = "WEIGHT", unit_weight = "kg", val_height = "HEIGHT", unit_height = "cm",
    val_head = "", unit_head = "cm", male_val = "0", female_val = "1",
    long_length_adjust = FALSE, val_length = "", long_keep_only = FALSE)
  utils::modifyList(base, list(...))
}

test_that("long: a non-numeric measurement column is rejected at mapping", {
  skip_on_cran()
  d <- data.frame(subjid = c("a", "a"), sex = c(0L, 0L), agedays = c(365L, 400L),
                  param = c("WEIGHT", "HEIGHT"), measurement = c("heavy", "tall"),
                  stringsAsFactors = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    do.call(session$setInputs, .long_inputs(f))
    session$setInputs(map_btn = 1)
    expect_null(rv$mapped_df)   # text in the measurement column -> refused at Step 2
  })
})

test_that("long: mapping the same column to two fields is rejected at mapping", {
  skip_on_cran()
  d <- data.frame(subjid = c("a", "a"), sex = c(0L, 0L), agedays = c(365L, 400L),
                  param = c("WEIGHT", "HEIGHT"), measurement = c(10, 100),
                  stringsAsFactors = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    do.call(session$setInputs, .long_inputs(f, long_sex = "subjid"))  # subjid mapped twice
    session$setInputs(map_btn = 1)
    expect_null(rv$mapped_df)
  })
})

test_that("long: a valid numeric mapping still succeeds (guards do not over-reject)", {
  skip_on_cran()
  d <- data.frame(subjid = c("a", "a"), sex = c(0L, 0L), agedays = c(365L, 400L),
                  param = c("WEIGHT", "HEIGHT"), measurement = c(10, 100),
                  stringsAsFactors = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    do.call(session$setInputs, .long_inputs(f))
    session$setInputs(map_btn = 1)
    expect_false(is.null(rv$mapped_df))
    expect_true(all(c("subjid", "sex", "param", "measurement") %in% names(rv$mapped_df)))
  })
})

# ---------------------------------------------------------------------------
# Results keep the user's column names (the app passes them to cleangrowth()
# rather than renaming the mapped columns).
# ---------------------------------------------------------------------------

# The app's full run uses parallel = TRUE, which R CMD check --as-cran
# (_R_CHECK_LIMIT_CORES_) refuses when more than 2 workers are detected. These
# tests are skip_on_cran; the limit is lifted for this file and restored below.
.core_limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", NA_character_)
Sys.unsetenv("_R_CHECK_LIMIT_CORES_")

test_that("long: results keep the user's names, including role-like unmapped columns", {
  skip_on_cran()
  d <- data.frame(patid = rep(c("a", "b"), each = 4), gender = rep(c("M", "F"), each = 4),
                  age_d = rep(c(365L, 730L), 4), kind = rep(c("WEIGHTKG", "WEIGHTKG", "HEIGHTCM", "HEIGHTCM"), 2),
                  value = c(9, 12, 74, 86, 8.5, 11.5, 73, 85), row_no = 1:8,
                  sex = "unmapped", site = "S1", stringsAsFactors = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    do.call(session$setInputs, .long_inputs(f, long_subjid = "patid", long_sex = "gender",
                                            long_agedays = "age_d", long_param = "kind",
                                            long_measurement = "value", long_id = "row_no",
                                            val_weight = "WEIGHTKG", val_height = "HEIGHTCM",
                                            male_val = "M", female_val = "F"))
    session$setInputs(map_btn = 1)
    expect_identical(names(rv$mapped_df), names(d))
    session$setInputs(run_btn = 1)
    out <- rv$output_df
    expect_true(all(c("patid", "gender", "age_d", "kind", "value", "row_no", "sex", "site",
                      "exclude") %in% names(out)))
    expect_false(any(c("subjid", "param", "measurement", "agedays", "id") %in% names(out)))
    expect_equal(out$value, d$value)
    expect_true(all(out$sex == "unmapped"))
  })
})

test_that("semi-wide: results keep the user's names", {
  skip_on_cran()
  d <- data.frame(pid = rep(c("a", "b"), each = 2), gender = rep(c("M", "F"), each = 2),
                  age_days = rep(c(365L, 730L), 2), wt = c(9, 12, 8.5, 11.5), ht = c(74, 86, 73, 85),
                  site = "S1", stringsAsFactors = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f, name = basename(f), size = file.size(f)))
    session$setInputs(demo_choice = "semiwide",
                      sw_subjid = "pid", sw_sex = "gender", sw_agedays = "age_days",
                      sw_weight = "wt", sw_unit_weight = "kg",
                      sw_height = "ht", sw_unit_height = "cm",
                      sw_head = "", sw_unit_head = "cm",
                      sw_id = "", sw_dob = "", sw_visit_date = "",
                      sw_keep_only = FALSE, sw_output_long = FALSE,
                      male_val = "M", female_val = "F")
    session$setInputs(map_btn = 1)
    session$setInputs(run_btn = 1)
    out <- rv$output_df
    expect_true(all(names(d) %in% names(out)))
    expect_false("subjid" %in% names(out))
    expect_true(all(c("exclude_wt", "exclude_ht") %in% names(out)))
  })
})

test_that("wide: results keep the user's names", {
  skip_on_cran()
  d <- data.frame(pid = c("a", "b"), gender = c("M", "F"), age_1 = c(365L, 365L), age_2 = c(730L, 730L),
                  wt_1 = c(9, 8.5), wt_2 = c(12, 11.5), ht_1 = c(74, 73), ht_2 = c(86, 85),
                  site = "S1", stringsAsFactors = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f, name = basename(f), size = file.size(f)))
    session$setInputs(demo_choice = "wide")
    session$setInputs(w_subjid = "pid", w_sex = "gender", w_dob = "",
                      w_agedays_prefix = "age", w_visit_date = "",
                      w_weight_prefix = "wt", w_unit_weight = "kg",
                      w_height_prefix = "ht", w_unit_height = "cm",
                      w_head_prefix = "", w_unit_head = "cm",
                      w_keep_only = FALSE, w_output_long = FALSE,
                      male_val = "M", female_val = "F")
    session$setInputs(map_btn = 1)
    session$setInputs(run_btn = 1)
    out <- rv$output_df
    expect_true(all(names(d) %in% names(out)))
    expect_false("subjid" %in% names(out))
  })
})

test_that("keep-only trim keeps the mapped columns under the user's names", {
  skip_on_cran()
  d <- data.frame(patid = c("a", "a"), gender = c(0L, 0L), age_d = c(365L, 400L),
                  kind = c("WEIGHT", "HEIGHT"), value = c(10, 100), extra = "x",
                  stringsAsFactors = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    do.call(session$setInputs, .long_inputs(f, long_subjid = "patid", long_sex = "gender",
                                            long_agedays = "age_d", long_param = "kind",
                                            long_measurement = "value", long_keep_only = TRUE))
    session$setInputs(map_btn = 1)
    expect_identical(names(rv$mapped_df), c("patid", "gender", "age_d", "kind", "value"))
  })
})

test_that("the exclusion summary does not count the user's own exclude_ columns", {
  skip_on_cran()
  # Long
  d <- data.frame(patid = rep(c("a", "b"), each = 4), gender = rep(c("M", "F"), each = 4),
                  age_d = rep(c(365L, 730L), 4), kind = rep(c("WEIGHTKG", "WEIGHTKG", "HEIGHTCM", "HEIGHTCM"), 2),
                  value = c(9, 12, 74, 86, 8.5, 11.5, 73, 85), exclude_reason = "Include",
                  stringsAsFactors = FALSE)
  f <- .write_csv(d)
  shiny::testServer(.app_obj(), {
    do.call(session$setInputs, .long_inputs(f, long_subjid = "patid", long_sex = "gender",
                                            long_agedays = "age_d", long_param = "kind",
                                            long_measurement = "value",
                                            val_weight = "WEIGHTKG", val_height = "HEIGHTCM",
                                            male_val = "M", female_val = "F"))
    session$setInputs(map_btn = 1)
    session$setInputs(run_btn = 1)
    expect_equal(sum(rv$exclude_tab$count), 8L)
  })
  # Semi-wide
  d2 <- data.frame(pid = rep(c("a", "b"), each = 2), gender = rep(c("M", "F"), each = 2),
                   age_days = rep(c(365L, 730L), 2), wt = c(9, 12, 8.5, 11.5), ht = c(74, 86, 73, 85),
                   exclude_reason = "Include", stringsAsFactors = FALSE)
  f2 <- .write_csv(d2)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f2, name = basename(f2), size = file.size(f2)))
    session$setInputs(demo_choice = "semiwide",
                      sw_subjid = "pid", sw_sex = "gender", sw_agedays = "age_days",
                      sw_weight = "wt", sw_unit_weight = "kg",
                      sw_height = "ht", sw_unit_height = "cm",
                      sw_head = "", sw_unit_head = "cm",
                      sw_id = "", sw_dob = "", sw_visit_date = "",
                      sw_keep_only = FALSE, sw_output_long = FALSE,
                      male_val = "M", female_val = "F")
    session$setInputs(map_btn = 1)
    session$setInputs(run_btn = 1)
    expect_true("exclude_reason" %in% names(rv$output_df))
    expect_equal(sum(rv$exclude_tab$count), 8L)
  })
  # Wide
  d3 <- data.frame(pid = c("a", "b"), gender = c("M", "F"), age_1 = c(365L, 365L), age_2 = c(730L, 730L),
                   wt_1 = c(9, 8.5), wt_2 = c(12, 11.5), ht_1 = c(74, 73), ht_2 = c(86, 85),
                   exclude_reason = "Include", stringsAsFactors = FALSE)
  f3 <- .write_csv(d3)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f3, name = basename(f3), size = file.size(f3)))
    session$setInputs(demo_choice = "wide")
    session$setInputs(w_subjid = "pid", w_sex = "gender", w_dob = "",
                      w_agedays_prefix = "age", w_visit_date = "",
                      w_weight_prefix = "wt", w_unit_weight = "kg",
                      w_height_prefix = "ht", w_unit_height = "cm",
                      w_head_prefix = "", w_unit_head = "cm",
                      w_keep_only = FALSE, w_output_long = FALSE,
                      male_val = "M", female_val = "F")
    session$setInputs(map_btn = 1)
    session$setInputs(run_btn = 1)
    expect_equal(sum(rv$exclude_tab$count), 8L)
  })
})

test_that("the format can be chosen before or after the file is loaded", {
  skip_on_cran()
  d <- data.frame(subjid = c("a", "a"), sex = c(0L, 0L), agedays = c(365L, 400L),
                  param = c("WEIGHT", "HEIGHT"), measurement = c(10, 100),
                  stringsAsFactors = FALSE)
  f <- .write_csv(d)
  inp <- .long_inputs(f)
  up  <- inp["file_upload"]
  rest <- inp[setdiff(names(inp), "file_upload")]
  shiny::testServer(.app_obj(), {
    session$setInputs(demo_choice = "long")   # format first, as the app now lays it out
    do.call(session$setInputs, up)
    do.call(session$setInputs, rest)
    session$setInputs(map_btn = 1)
    expect_false(is.null(rv$mapped_df))
  })
  shiny::testServer(.app_obj(), {
    do.call(session$setInputs, up)            # file first still works
    do.call(session$setInputs, rest)
    session$setInputs(map_btn = 1)
    expect_false(is.null(rv$mapped_df))
  })
})

test_that("date samples skip blank and no-digit values and use a wide block's first usable column", {
  skip_on_cran()
  app_env <- new.env()
  suppressWarnings(suppressMessages(
    source(file.path(system.file("app", package = "growthcleanr"), "app.R"), local = app_env)))
  sample_of <- app_env$.gc_app_date_sample
  d <- data.frame(dob = c(NA, "", "unknown", "1/22/2015"),
                  v_1 = c(NA, NA, NA, NA), v_2 = c("", "2015-02-21", "", ""),
                  stringsAsFactors = FALSE)
  expect_identical(sample_of(d, "dob"), "dob: 1/22/2015")
  expect_identical(sample_of(d, c("v_1", "v_2")), "v_2: 2015-02-21")
  expect_null(sample_of(d, "v_1"))
  expect_null(sample_of(d, ""))
  expect_null(sample_of(d, NULL))
})

test_that("exact standard measurement names pre-select the column and its unit", {
  skip_on_cran()
  sw <- data.frame(subjid = "a", sex = 0L, agedays = 365L,
                   WEIGHTLBS = 20, HEIGHTIN = 30, HEADCM = 45)
  f <- .write_csv(sw)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f, name = basename(f), size = file.size(f)),
                      demo_choice = "semiwide")
    html <- as.character(output$mapping_ui$html)
    for (nm in c("WEIGHTLBS", "HEIGHTIN", "HEADCM"))
      expect_match(html, sprintf('<option value="%s" selected>', nm), fixed = TRUE)
    expect_match(html, '<select id="sw_unit_weight"[^>]*>\\s*<option value="kg">kg</option>\\s*<option value="lbs" selected>')
    expect_match(html, '<select id="sw_unit_height"[^>]*>\\s*<option value="cm">cm</option>\\s*<option value="in" selected>')
    expect_match(html, '<select id="sw_unit_head"[^>]*>\\s*<option value="cm" selected>')
  })
  w <- data.frame(subjid = "a", sex = 0L, agedays_1 = 365L, agedays_2 = 730L,
                  HEADIN_1 = 18, HEADIN_2 = 19, wt_1 = 9, wt_2 = 12)
  f2 <- .write_csv(w)
  shiny::testServer(.app_obj(), {
    session$setInputs(file_upload = list(datapath = f2, name = basename(f2), size = file.size(f2)),
                      demo_choice = "wide")
    html <- as.character(output$mapping_ui$html)
    expect_match(html, '<option value="HEADIN" selected>', fixed = TRUE)
    expect_match(html, '<select id="w_unit_head"[^>]*>\\s*<option value="cm">cm</option>\\s*<option value="in" selected>')
    # Custom names are not guessed.
    expect_no_match(html, '<option value="wt" selected>', fixed = TRUE)
  })
})

test_that("a new file, format or mapping clears results, notes and the stale mapping", {
  skip_on_cran()
  d <- data.frame(subjid = rep("a", 4), sex = 0L, agedays = c(365L, 365L, 730L, 730L),
                  param = rep(c("WEIGHT", "HEIGHT"), 2), measurement = c(10, 75, 12, 85),
                  stringsAsFactors = FALSE)
  f <- .write_csv(d)
  inp <- .long_inputs(f)
  shiny::testServer(.app_obj(), {
    run_once <- function(n) {
      session$setInputs(map_btn = n)
      session$setInputs(run_btn = n)
      expect_false(is.null(rv$output_df))
      rv$run_warnings <- "old note"
    }
    do.call(session$setInputs, inp)

    # Re-applying the mapping keeps the mapping but clears the results.
    run_once(1)
    session$setInputs(map_btn = 2)
    expect_false(is.null(rv$mapped_df))
    expect_null(rv$output_df); expect_null(rv$exclude_tab); expect_null(rv$run_warnings)

    # A new file voids the mapping too.
    run_once(3)
    f2 <- .write_csv(d)
    session$setInputs(file_upload = list(datapath = f2, name = basename(f2), size = file.size(f2)))
    expect_null(rv$mapped_df); expect_null(rv$output_df); expect_null(rv$run_warnings)

    # So does changing the format.
    run_once(4)
    session$setInputs(demo_choice = "semiwide")
    expect_null(rv$mapped_df); expect_null(rv$output_df); expect_null(rv$run_warnings)
  })
})

test_that("a failed run does not leave the prior run's results available", {
  skip_on_cran()
  d <- data.frame(subjid = rep("a", 4), sex = 0L, dob = "2015-01-22",
                  visit = c("2016-01-22", "2016-01-22", "2017-01-22", "2017-01-22"),
                  param = rep(c("WEIGHT", "HEIGHT"), 2), measurement = c(10, 75, 12, 85),
                  stringsAsFactors = FALSE)
  f <- .write_csv(d)
  bad <- d; bad$dob <- "01/22/2015"; bad$visit <- "01/22/2016"
  f2 <- .write_csv(bad)
  inp <- .long_inputs(f, long_agedays = "", long_dob = "dob", long_visit_date = "visit")
  shiny::testServer(.app_obj(), {
    do.call(session$setInputs, inp)
    session$setInputs(map_btn = 1)
    session$setInputs(run_btn = 1)
    expect_false(is.null(rv$output_df))
    # Numeric day/month dates with no stated order stop the run.
    session$setInputs(file_upload = list(datapath = f2, name = basename(f2), size = file.size(f2)))
    session$setInputs(map_btn = 2)
    session$setInputs(run_btn = 2)
    expect_null(rv$output_df)
    expect_null(rv$exclude_tab)
    # Stating the order lets the same file run.
    session$setInputs(long_date_order = "mdy", run_btn = 3)
    expect_false(is.null(rv$output_df))
  })
})

if (!is.na(.core_limit)) Sys.setenv("_R_CHECK_LIMIT_CORES_" = .core_limit)
