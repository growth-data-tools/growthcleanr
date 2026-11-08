# sd.recenter wiring for cleangrowth(), including checkpointed runs
# (cleangrowth(checkpoint_dir = ...)).
#
# Tests the PLUMBING of sd.recenter (NA = built-in reference, "none", or a
# supplied table), its validation, and the recenter_detail output contract.

testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# Small independent-subject dataset with plausible HT/WT so z-scores compute.
make_small <- function(n_subj = 12) {
  ages <- c(0L, 180L, 365L, 730L, 1460L)
  rbindlist(lapply(seq_len(n_subj), function(s) {
    off <- (s %% 5) * 0.1
    data.table(
      subjid      = s,
      param       = rep(c("WEIGHTKG", "HEIGHTCM"), each = length(ages)),
      agedays     = rep(ages, 2),
      sex         = s %% 2L,
      measurement = c(3.3 + off + c(0, 3.5, 6.5, 9.0, 12.5),   # WEIGHTKG
                      50  + off + c(0, 15,  25,  33,  50)),     # HEIGHTCM
      id          = NA_integer_
    )
  }))[, id := .I][]
}

# ---------------------------------------------------------------------------
# cleangrowth(): sd.recenter = "none" disables recentering
# ---------------------------------------------------------------------------

test_that('sd.recenter = "none" disables recentering (tbc.sd == sd.orig)', {
  d <- make_small()
  r <- suppressMessages(cleangrowth(d, sd.recenter = "none", quietly = TRUE, debug = TRUE))
  expect_equal(nrow(r), nrow(d))
  # child rows: the recentered z equals the reference z (no median subtracted)
  ok <- !is.na(r$tbc.sd) & !is.na(r$sd.orig)
  expect_true(any(ok))
  expect_equal(r$tbc.sd[ok], r$sd.orig[ok])
  # and it differs from the recentered default somewhere (recentering does something)
  r_def <- cleangrowth(d, quietly = TRUE, debug = TRUE)
  expect_false(isTRUE(all.equal(r$tbc.sd, r_def$tbc.sd)))
})

test_that('sd.recenter = "none" ignores case and surrounding spaces', {
  d <- make_small()
  r_a <- suppressMessages(cleangrowth(d, sd.recenter = "none", quietly = TRUE, debug = TRUE))
  r_b <- suppressMessages(cleangrowth(d, sd.recenter = " None ", quietly = TRUE, debug = TRUE))
  expect_identical(as.character(r_a$exclude), as.character(r_b$exclude))
  expect_equal(r_a$tbc.sd, r_b$tbc.sd)
})

test_that("recenter_source is not an argument", {
  d <- make_small()
  expect_error(cleangrowth(d, recenter_source = "none", quietly = TRUE),
               "unused argument")
})

# ---------------------------------------------------------------------------
# checkpointed runs: sd.recenter forwarded to each checkpoint unchanged
# (cleangrowth(checkpoint_dir = ...), the folded interface)
# ---------------------------------------------------------------------------

test_that('checkpoint sd.recenter = "none": checkpointed == single-pass', {
  d   <- make_small()
  dir <- file.path(tempdir(), "gc_ckpt_none")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  res <- suppressMessages(cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 4, quietly = TRUE,
                                      sd.recenter = "none"))
  ref <- suppressMessages(cleangrowth(d, sd.recenter = "none", quietly = TRUE))
  m <- merge(res[, .(id, exclude)], ref[, .(id, exclude_ref = exclude)], by = "id")
  expect_equal(as.character(m$exclude), as.character(m$exclude_ref))
})

test_that("checkpoint invalid sd.recenter value errors", {
  d   <- make_small()
  dir <- file.path(tempdir(), "gc_ckpt_bad")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  expect_error(cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 4, quietly = TRUE,
                           sd.recenter = "bogus"),
               "sd.recenter must be omitted")
})

# ---------------------------------------------------------------------------
# recenter_detail: stable, named output contract for the recentered CSD z
# ---------------------------------------------------------------------------

rc_detail_cols <- c("tbc.sd", "ctbc.sd", "rcmedian.sd")

test_that("recenter_detail is off by default (columns absent)", {
  d <- make_small()
  r <- cleangrowth(d, quietly = TRUE)
  expect_false(any(rc_detail_cols %in% names(r)))
})

test_that("recenter_detail = TRUE exposes tbc.sd / ctbc.sd / rcmedian.sd", {
  d <- make_small()
  r <- cleangrowth(d, recenter_detail = TRUE, quietly = TRUE)
  expect_true(all(rc_detail_cols %in% names(r)))
  # child rows carry real recentered z-scores (not all NA)
  expect_false(all(is.na(r$tbc.sd)))
  expect_false(all(is.na(r$rcmedian.sd)))
  # the contract columns are byte-identical to what debug surfaces
  r_dbg <- cleangrowth(d, debug = TRUE, quietly = TRUE)
  expect_equal(r$tbc.sd,  r_dbg$tbc.sd)
  expect_equal(r$ctbc.sd, r_dbg$ctbc.sd)
  # recentering identity: tbc.sd == sd.orig - rcmedian.sd (sd.orig from debug)
  expect_equal(r$tbc.sd, r_dbg$sd.orig - r$rcmedian.sd)
})

test_that("full_detail turns on recenter_detail", {
  d <- make_small()
  r <- cleangrowth(d, full_detail = TRUE, quietly = TRUE)
  expect_true(all(rc_detail_cols %in% names(r)))
})

test_that("recenter_detail + debug together produce no duplicate columns", {
  d <- make_small()
  r <- cleangrowth(d, recenter_detail = TRUE, debug = TRUE, quietly = TRUE)
  expect_false(any(duplicated(names(r))))
  # each contract column appears exactly once
  for (cc in rc_detail_cols) expect_equal(sum(names(r) == cc), 1L)
})

test_that("checkpoint forwards recenter_detail to each checkpoint", {
  d   <- make_small()
  dir <- file.path(tempdir(), "gc_ckpt_rcdetail")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  res <- cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 4, quietly = TRUE,
                     recenter_detail = TRUE)
  expect_true(all(rc_detail_cols %in% names(res)))
  expect_false(all(is.na(res$tbc.sd)))
})

# ---------------------------------------------------------------------------
# Scale regression guard: the built-in table supplied explicitly is
# byte-identical to the default on real data
# ---------------------------------------------------------------------------

test_that('the built-in table passed as sd.recenter is byte-identical to the default on syngrowth', {
  # Realistic subset (HT/WT/HC, potcorr, adult rows) so the guard exercises the
  # full pipeline, not just constructed data.
  data("syngrowth", package = "growthcleanr", envir = environment())
  sg <- as.data.table(syngrowth)
  subjs <- unique(sg$subjid)[1:300]
  d <- sg[subjid %in% subjs]

  r_def <- cleangrowth(d, quietly = TRUE, recenter_detail = TRUE)
  r_ref <- cleangrowth(d, quietly = TRUE, recenter_detail = TRUE,
                       sd.recenter = fread(system.file("extdata", "rcfile-resmoothed.csv.gz",
                                                       package = "growthcleanr")))
  expect_identical(as.character(r_def$exclude), as.character(r_ref$exclude))
  expect_identical(r_def$tbc.sd,      r_ref$tbc.sd)
  expect_identical(r_def$ctbc.sd,     r_ref$ctbc.sd)
  expect_identical(r_def$rcmedian.sd, r_ref$rcmedian.sd)
})

# ---------------------------------------------------------------------------
# sd.recenter validation: values other than NA / "none" / a table stop;
# data.frames are accepted;
# a user table must cover every evaluable child row
# ---------------------------------------------------------------------------

rc_builtin <- function() {
  fread(system.file("extdata", "rcfile-resmoothed.csv.gz", package = "growthcleanr"))
}

test_that("sd.recenter removed 2.x options and other values stop", {
  d <- make_small()
  for (v in c("nhanes", "NHANES", "derive")) {
    expect_error(cleangrowth(d, sd.recenter = v, quietly = TRUE),
                 sprintf("sd.recenter = \"%s\" was removed in growthcleanr version 3", v), fixed = TRUE)
  }
  expect_error(cleangrowth(d, sd.recenter = 5, quietly = TRUE),
               "sd.recenter must be omitted (built-in reference), \"none\" (no recentering), or a table", fixed = TRUE)
  expect_error(cleangrowth(d, sd.recenter = "reference", quietly = TRUE),
               "sd.recenter must be omitted", fixed = TRUE)
  # also stopped under checkpointing (normalized before the checkpoint split)
  dir <- file.path(tempdir(), "gc_ckpt_rcstring")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  expect_error(cleangrowth(d, sd.recenter = "NHANES", checkpoint_dir = dir,
                           checkpoint_size = 4, quietly = TRUE),
               "sd.recenter = \"NHANES\" was removed", fixed = TRUE)
})

test_that("sd.recenter NA / NULL use the built-in reference", {
  d <- make_small()
  r_def <- cleangrowth(d, quietly = TRUE)
  expect_identical(as.character(cleangrowth(d, sd.recenter = NULL, quietly = TRUE)$exclude),
                   as.character(r_def$exclude))
  expect_identical(as.character(cleangrowth(d, sd.recenter = NA, quietly = TRUE)$exclude),
                   as.character(r_def$exclude))
})

test_that("sd.recenter data.frame is used (not silently ignored)", {
  d <- make_small()
  rc <- rc_builtin()
  r_def <- cleangrowth(d, quietly = TRUE, recenter_detail = TRUE)
  r_df  <- cleangrowth(d, sd.recenter = as.data.frame(rc), quietly = TRUE,
                       recenter_detail = TRUE)
  expect_identical(as.character(r_df$exclude), as.character(r_def$exclude))
  # a shifted data.frame table must change the medians actually applied
  rc_shift <- as.data.frame(copy(rc)[, rcmedian.sd := rcmedian.sd + 1])
  r_sh <- cleangrowth(d, sd.recenter = rc_shift, quietly = TRUE, recenter_detail = TRUE)
  ok <- !is.na(r_sh$rcmedian.sd)
  expect_true(any(ok))
  expect_equal(r_sh$rcmedian.sd[ok], r_def$rcmedian.sd[ok] + 1)
})

test_that("a supplied sd.recenter table is not modified by reference", {
  d <- make_small()
  rc <- rc_builtin()
  setnames(rc, "rcmedian.sd", "sd.median")   # legacy column name
  r <- cleangrowth(d, sd.recenter = rc, quietly = TRUE)
  expect_true("sd.median" %in% names(rc))    # caller's column not renamed
  expect_null(key(rc))                       # caller's table not keyed
  expect_identical(as.character(r$exclude),
                   as.character(cleangrowth(d, quietly = TRUE)$exclude))
})

test_that("sd.recenter missing a required column stops", {
  d <- make_small()
  expect_error(cleangrowth(d, sd.recenter = rc_builtin()[, .(param, sex, agedays)],
                           quietly = TRUE),
               "missing: rcmedian.sd \\(or sd.median\\)")
})

test_that("sd.recenter with duplicate param/sex/agedays rows stops", {
  d <- make_small()
  rc <- rc_builtin()
  dup <- rbind(rc[param == "WEIGHTKG" & sex == 0L & agedays %in% c(180L, 365L)], rc)
  expect_error(cleangrowth(d, sd.recenter = dup, quietly = TRUE),
               "more than one row for 2 param/sex/agedays combination\\(s\\) \\(e\\.g\\. WEIGHTKG, sex 0, agedays 180; WEIGHTKG, sex 0, agedays 365\\)")
})

test_that("partial-coverage sd.recenter stops with a count and examples", {
  d <- make_small()   # child ages 0-1460 days, HT/WT, both sexes
  part <- rc_builtin()[agedays < 1000]
  # 12 subjects x 2 params at agedays 1460 are uncovered
  expect_error(cleangrowth(d, sd.recenter = part, quietly = TRUE),
               "no median for 24 measurement\\(s\\) to be cleaned \\(e\\.g\\. HEIGHTCM, sex 0, agedays 1460")
  # dataset-wide count under checkpointing too (not one checkpoint's count)
  dir <- file.path(tempdir(), "gc_ckpt_rcpartial")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  expect_error(cleangrowth(d, sd.recenter = part, checkpoint_dir = dir,
                           checkpoint_size = 4, quietly = TRUE),
               "no median for 24 measurement")
  # an NA median counts as uncovered
  na_rc <- rc_builtin()[param == "WEIGHTKG" & sex == 1L & agedays == 365L, rcmedian.sd := NA]
  expect_error(cleangrowth(d, sd.recenter = na_rc, quietly = TRUE),
               "no median for 6 measurement\\(s\\).*WEIGHTKG, sex 1, agedays 365")
})

test_that("coverage is required only for evaluable child rows", {
  # Adult rows, HC at 5y+, missing measurements, and unusable-sex child rows
  # need no median: a table covering only the evaluable child rows cleans.
  d <- make_small(4)
  extra <- data.table(
    subjid      = c(1, 1, 2, 3, 4),
    param       = c("WEIGHTKG", "HEADCM", "WEIGHTKG", "HEIGHTCM", "WEIGHTKG"),
    agedays     = c(8000L, 2000L, 1460L, 7400L, 900L),
    sex         = c(1L, 1L, 0L, 1L, NA),
    measurement = c(70, 52, NA, 170, 12))
  d2 <- rbind(d[, !"id"], extra)[, id := .I][]
  rc <- rc_builtin()
  keep <- unique(d[, .(param, sex, agedays)])
  cover <- rc[keep, on = c("param", "sex", "agedays")]
  r_cov <- suppressWarnings(cleangrowth(d2, sd.recenter = cover, quietly = TRUE))
  r_def <- suppressWarnings(cleangrowth(d2, quietly = TRUE))
  expect_identical(as.character(r_cov$exclude), as.character(r_def$exclude))
})
