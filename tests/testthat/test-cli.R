# Tests for the command-line driver core (R/cli.R): the string->typed options
# builder (.gc_cli_opts), the exported run_gc_cli() I/O core, and a skip_on_cran
# shell-out through exec/gcdriver.R.

library(growthcleanr)
library(data.table)

# Small deterministic fixture (HT + WT, a few subjects), written to a temp CSV.
.cli_fixture <- function() {
  data.table(
    subjid      = rep(1:3, each = 4),
    param       = rep(c("WEIGHTKG", "HEIGHTCM"), 6),
    agedays     = rep(c(400L, 400L, 800L, 800L), 3),
    sex         = 0,
    measurement = rep(c(12, 80, 14, 88), 3) + rep(0:2, each = 4)
  )
}

# Semi-wide fixture (one row per visit; value columns named wt/ht, not canonically).
.cli_semiwide_fixture <- function() {
  data.table(
    subjid  = rep(1:3, each = 2),
    agedays = rep(c(400L, 800L), 3),
    sex     = 0,
    wt      = rep(c(12, 14), 3) + rep(0:2, each = 2),
    ht      = rep(c(80, 88), 3) + rep(0:2, each = 2)
  )
}

opts <- function(argv) growthcleanr:::.gc_cli_opts(argv)

# ---------------------------------------------------------------------------
# .gc_cli_opts: coercion + assembly
# ---------------------------------------------------------------------------
test_that("scalar flags coerce to the right type and cleangrowth arg", {
  o <- opts(list(adult_cutpoint = "18", ewma_window = "21",
                 batch_size = "1000", adult_scale_max_lbs = "Inf"))
  expect_identical(o$adult_cutpoint, 18)          # numeric
  expect_identical(o$ewma_window, 21L)            # integer
  expect_identical(o$batch_size, 1000L)
  expect_true(is.infinite(o$adult_scale_max_lbs)) # Inf accepted
  # dotted cleangrowth arg names are produced
  expect_identical(opts(list(piv_z_ht_high = "9"))$piv.z.ht.high, 9)
  expect_identical(opts(list(log_path = "/tmp/x"))$log.path, "/tmp/x")
})

test_that("unset flags are omitted (NA / empty / absent)", {
  expect_length(opts(list(adult_cutpoint = NA, cf_rescue = "", input_format = NULL)), 0L)
})

test_that("boolean and inverted flags map correctly", {
  o <- opts(list(parallel = TRUE, verbose = TRUE, full_detail = TRUE,
                 no_unit_error_range = TRUE, no_keep_checkpoints = TRUE,
                 no_carry_through = TRUE, quiet = TRUE))
  expect_true(o$parallel); expect_true(o$verbose); expect_true(o$full_detail)
  expect_identical(o$unit_error_range, FALSE)   # inverted
  expect_identical(o$keep_checkpoints, FALSE)
  expect_identical(o$carry_through, FALSE)
  expect_identical(o$quietly, TRUE)             # --quiet -> quietly = TRUE
  # a boolean flag left FALSE is not forwarded
  expect_null(opts(list(parallel = FALSE))$parallel)
})

test_that("Tier-2 param_coding / sex_coding / column maps assemble (metric default)", {
  o <- opts(list(param_weight = "WT,body_wt", param_height = "HT",
                 sex_male = "0,m,boy", sex_female = "1,f,girl",
                 semiwide_col_weight = "wt_kg", wide_weight = "wt"))
  # No --*-units flag -> metric canonical keys.
  expect_identical(o$param_coding, list(WEIGHTKG = c("WT", "body_wt"), HEIGHTCM = "HT"))
  expect_identical(o$sex_coding, list(male = c("0", "m", "boy"),
                                      female = c("1", "f", "girl")))
  expect_identical(o$semiwide_cols, list(WEIGHTKG = "wt_kg"))
  expect_identical(o$wide_cols, list(WEIGHTKG = "wt"))
})

test_that("units flags select the imperial vs metric canonical key", {
  # imperial units route the same column name to the imperial canonical key,
  # across all four base params
  o <- opts(list(param_weight = "WT", weight_units = "lbs",
                 param_height = "HT", height_units = "in",
                 param_length = "LEN", length_units = "in",
                 param_head   = "HC", head_units   = "in"))
  expect_identical(o$param_coding,
                   list(WEIGHTLBS = "WT", HEIGHTIN = "HT",
                        LENGTHIN = "LEN", HEADIN = "HC"))

  # explicit metric units == the default (metric)
  expect_identical(opts(list(param_weight = "WT", weight_units = "kg"))$param_coding,
                   list(WEIGHTKG = "WT"))
  expect_identical(opts(list(param_weight = "WT"))$param_coding,
                   list(WEIGHTKG = "WT"))

  # the same --*-units flag qualifies semiwide and wide maps too
  expect_identical(opts(list(semiwide_col_weight = "wt", weight_units = "lbs"))$semiwide_cols,
                   list(WEIGHTLBS = "wt"))
  expect_identical(opts(list(wide_height = "ht", height_units = "in"))$wide_cols,
                   list(HEIGHTIN = "ht"))

  # a units flag with no matching name flag is simply unused (no key, no error)
  expect_null(opts(list(weight_units = "lbs"))$param_coding)

  # a bad units value errors clearly and up front
  expect_error(opts(list(param_weight = "WT", weight_units = "stone")),
               "--weight-units: expected one of")
  expect_error(opts(list(param_height = "HT", height_units = "feet")),
               "--height-units: expected one of")
})

test_that("coercion + validation errors are clear and up front", {
  expect_error(opts(list(cf_rescue = "bogus")), "--cf-rescue: expected one of")
  expect_error(opts(list(input_format = "sideways")), "--input-format: expected one of")
  expect_error(opts(list(adult_cutpoint = "abc")), "--adult-cutpoint: expected a number")
  expect_error(opts(list(batch_size = "0")), "positive whole number")
  expect_error(opts(list(batch_size = "2.5")), "positive whole number")
  expect_error(opts(list(sex_male = "0,m")), "must be supplied together")  # one side only
})

# ---------------------------------------------------------------------------
# run_gc_cli(): I/O core + round-trip equivalence
# ---------------------------------------------------------------------------
test_that("run_gc_cli output equals an in-R cleangrowth() call", {
  dt <- .cli_fixture()
  fin <- tempfile(fileext = ".csv"); fout <- tempfile(fileext = ".csv")
  fwrite(dt, fin)
  suppressWarnings(run_gc_cli(fin, fout, opts = list(adult_cutpoint = 18, quietly = TRUE)))
  cli <- fread(fout)
  inr <- suppressWarnings(cleangrowth(dt, adult_cutpoint = 18, quietly = TRUE))
  expect_identical(as.character(cli$exclude), as.character(inr$exclude))
  expect_equal(nrow(cli), nrow(dt))
  expect_true(all(c("subjid", "param", "agedays", "exclude", "bin_exclude") %in% names(cli)))
})

test_that("run_gc_cli errors on missing input and empty input", {
  expect_error(run_gc_cli(tempfile(fileext = ".csv"), tempfile()), "input file not found")
  empty <- tempfile(fileext = ".csv"); fwrite(.cli_fixture()[0], empty)  # header only
  expect_error(run_gc_cli(empty, tempfile()), "no data rows")
})

test_that("run_gc_cli guards output I/O before cleaning (same-path, bad dir)", {
  fin <- tempfile(fileext = ".csv"); fwrite(.cli_fixture(), fin)
  before <- readLines(fin)
  # in == out must error and leave the input file untouched (no silent overwrite).
  expect_error(run_gc_cli(fin, fin, opts = list(quietly = TRUE)),
               "same file")
  expect_identical(readLines(fin), before)
  # a non-existent output directory fails fast with a clear message.
  expect_error(
    run_gc_cli(fin, file.path(tempdir(), "no_such_dir_zzz", "out.csv"),
               opts = list(quietly = TRUE)),
    "output directory does not exist")
})

test_that("checkpointed run through run_gc_cli equals a single-pass run", {
  dt <- .cli_fixture()
  fin <- tempfile(fileext = ".csv"); fout <- tempfile(fileext = ".csv")
  fwrite(dt, fin)
  ckdir <- file.path(tempdir(), "cli_ck"); unlink(ckdir, recursive = TRUE)
  suppressWarnings(run_gc_cli(fin, fout, opts = list(
    checkpoint_dir = ckdir, checkpoint_size = 2, quietly = TRUE)))
  ck  <- fread(fout)
  inr <- suppressWarnings(cleangrowth(dt, quietly = TRUE))
  expect_identical(as.character(ck$exclude), as.character(inr$exclude))
  # files are named checkpoint_NNNN.rds
  expect_true(any(grepl("^checkpoint_\\d{4}\\.rds$", list.files(ckdir))))
})

# ---------------------------------------------------------------------------
# Shell-out through exec/gcdriver.R (source-tree only; skipped on CRAN)
# ---------------------------------------------------------------------------
test_that("exec/gcdriver.R runs end to end and sets exit codes", {
  skip_on_cran()
  drv <- gc_cli_path()
  skip_if_not(nzchar(drv) && file.exists(drv), "gcdriver.R not present in installed package")
  rscript <- file.path(R.home("bin"), "Rscript")

  dt <- .cli_fixture()
  fin <- tempfile(fileext = ".csv"); fout <- tempfile(fileext = ".csv")
  fwrite(dt, fin)

  st <- suppressWarnings(system2(rscript, c(drv, fin, fout, "--adult-cutpoint", "18", "--quiet"),
                                 stdout = FALSE, stderr = FALSE))
  expect_identical(st, 0L)
  expect_true(file.exists(fout))
  out <- fread(fout)
  inr <- suppressWarnings(cleangrowth(dt, adult_cutpoint = 18, quietly = TRUE))
  expect_identical(as.character(out$exclude), as.character(inr$exclude))

  # a bad enum value exits non-zero
  bad <- suppressWarnings(system2(rscript, c(drv, fin, tempfile(fileext = ".csv"),
                                             "--cf-rescue", "bogus", "--quiet"),
                                  stdout = FALSE, stderr = FALSE))
  expect_gt(bad, 0L)

  # a missing input file exits non-zero
  miss <- suppressWarnings(system2(rscript, c(drv, tempfile(fileext = ".csv"),
                                              tempfile(fileext = ".csv"), "--quiet"),
                                   stdout = FALSE, stderr = FALSE))
  expect_gt(miss, 0L)
})

test_that("exec/gcdriver.R: --semiwide-col-* name flags and --*-units route end to end", {
  skip_on_cran()
  drv <- gc_cli_path()
  skip_if_not(nzchar(drv) && file.exists(drv), "gcdriver.R not present in installed package")
  rscript <- file.path(R.home("bin"), "Rscript")

  sw <- .cli_semiwide_fixture()
  fin <- tempfile(fileext = ".csv"); fwrite(sw, fin)

  # (1) metric name flags: --semiwide-col-weight/-height (no units) == in-R semiwide_cols metric keys
  fout <- tempfile(fileext = ".csv")
  st <- suppressWarnings(system2(rscript,
    c(drv, fin, fout, "--input-format", "semi_wide",
      "--semiwide-col-weight", "wt", "--semiwide-col-height", "ht", "--quiet"),
    stdout = FALSE, stderr = FALSE))
  expect_identical(st, 0L)
  cli <- fread(fout)
  inr <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide",
    semiwide_cols = list(WEIGHTKG = "wt", HEIGHTCM = "ht"), quietly = TRUE))
  expect_identical(as.character(cli$exclude), as.character(inr$exclude))

  # (2) --weight-units lbs routes the same wt column to the imperial key: the run
  #     matches an in-R call keyed on WEIGHTLBS (equivalence proves the routing).
  fout_lbs <- tempfile(fileext = ".csv")
  st2 <- suppressWarnings(system2(rscript,
    c(drv, fin, fout_lbs, "--input-format", "semi_wide",
      "--semiwide-col-weight", "wt", "--weight-units", "lbs",
      "--semiwide-col-height", "ht", "--quiet"),
    stdout = FALSE, stderr = FALSE))
  expect_identical(st2, 0L)
  cli_lbs <- fread(fout_lbs)
  inr_lbs <- suppressWarnings(cleangrowth(sw, input_format = "semi_wide",
    semiwide_cols = list(WEIGHTLBS = "wt", HEIGHTCM = "ht"), quietly = TRUE))
  expect_identical(as.character(cli_lbs$exclude), as.character(inr_lbs$exclude))
  expect_equal(cli_lbs$measurement, inr_lbs$measurement)

  # (3) a bad units value exits non-zero
  bad <- suppressWarnings(system2(rscript,
    c(drv, fin, tempfile(fileext = ".csv"), "--input-format", "semi_wide",
      "--semiwide-col-weight", "wt", "--weight-units", "stone", "--quiet"),
    stdout = FALSE, stderr = FALSE))
  expect_gt(bad, 0L)
})
