testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# gc_partial_run(): incremental re-clean of changed subjects, reusing cached
# results for the rest.
#
# Core guarantee: because gc cleans each subject independently after
# reference recentering, a partial run must equal a full run of the same data
# under the same options, for every subject.
# =============================================================================

data("syngrowth", package = "growthcleanr", envir = environment())
.sg <- data.table::as.data.table(syngrowth)
.sg_peds <- .sg[agedays < 20 * 365.25]
.subjs <- unique(.sg_peds$subjid)

# 40-subject baseline dataset
.base_subjs <- .subjs[seq_len(40)]
.base <- .sg_peds[subjid %in% .base_subjs]

# "new" dataset: same as baseline but one subject's weights are perturbed
.changed_subj <- as.character(.base_subjs[1])
.new <- copy(.base)
.new[as.character(subjid) == .changed_subj & param == "WEIGHTKG",
     measurement := measurement * 1.4]

test_that("partial run equals a full run for every subject (exclude + tbc.sd)", {
  cached <- cleangrowth(.base, quietly = TRUE, debug = TRUE)
  full   <- cleangrowth(.new,  quietly = TRUE, debug = TRUE)

  partial <- suppressMessages(
    gc_partial_run(.new, cached, changed_subjids = .changed_subj,
                   quietly = TRUE, debug = TRUE)
  )

  expect_equal(nrow(partial), nrow(full))
  expect_setequal(partial$id, full$id)
  expect_equal(partial[order(id)]$exclude, full[order(id)]$exclude,
               info = "exclude must match a full run")
  expect_equal(partial[order(id)]$tbc.sd, full[order(id)]$tbc.sd,
               info = "recentered z-scores must match a full run")
})

test_that("partial run equals a full run under checkpointing", {
  cached <- cleangrowth(.base, quietly = TRUE)
  full   <- cleangrowth(.new,  quietly = TRUE)

  ckpt <- file.path(tempdir(), "gc_partial_ckpt")
  unlink(ckpt, recursive = TRUE)
  partial <- suppressMessages(
    gc_partial_run(.new, cached, changed_subjids = .changed_subj,
                   quietly = TRUE, checkpoint_dir = ckpt,
                   checkpoint_size = 5, keep_checkpoints = FALSE)
  )

  expect_equal(partial[order(id)]$exclude, full[order(id)]$exclude)
  # keep_checkpoints = FALSE cleans up the scratch checkpoint files
  expect_false(dir.exists(ckpt) && length(list.files(ckpt, pattern = "checkpoint_")) > 0)
})

test_that("no changed subjects returns the cached result unchanged", {
  cached <- cleangrowth(.base, quietly = TRUE)
  partial <- suppressMessages(
    gc_partial_run(.base, cached, changed_subjids = character(0), quietly = TRUE)
  )
  expect_equal(partial[order(id)]$exclude, cached[order(id)]$exclude)
  expect_setequal(partial$id, cached$id)
})

test_that("all subjects changed equals a plain full run", {
  cached <- cleangrowth(.base, quietly = TRUE)
  full   <- cleangrowth(.new,  quietly = TRUE)
  partial <- suppressMessages(
    gc_partial_run(.new, cached,
                   changed_subjids = as.character(.base_subjs), quietly = TRUE)
  )
  expect_equal(partial[order(id)]$exclude, full[order(id)]$exclude)
})

test_that("an added subject is cleaned automatically", {
  # cache built on 39 subjects; new data has a 40th (added) subject
  base39 <- .sg_peds[subjid %in% .base_subjs[seq_len(39)]]
  cached <- cleangrowth(base39, quietly = TRUE)
  full   <- cleangrowth(.base,  quietly = TRUE)  # all 40

  partial <- suppressMessages(
    gc_partial_run(.base, cached, changed_subjids = character(0), quietly = TRUE)
  )
  expect_setequal(partial$id, full$id)
  expect_equal(partial[order(id)]$exclude, full[order(id)]$exclude)
})

test_that("a removed subject is dropped", {
  cached <- cleangrowth(.base, quietly = TRUE)          # 40 subjects
  new39  <- .sg_peds[subjid %in% .base_subjs[seq_len(39)]]
  full   <- cleangrowth(new39, quietly = TRUE)          # 39 subjects

  partial <- suppressMessages(
    gc_partial_run(new39, cached, changed_subjids = character(0), quietly = TRUE)
  )
  expect_setequal(partial$id, full$id)
  expect_false(as.character(.base_subjs[40]) %in% as.character(partial$subjid))
  expect_equal(partial[order(id)]$exclude, full[order(id)]$exclude)
})

test_that("mismatched output options between cache and fresh run error", {
  cached <- cleangrowth(.base, quietly = TRUE)  # default output columns
  # request debug columns now -> fresh clean has extra columns cached lacks
  expect_error(
    suppressMessages(
      gc_partial_run(.new, cached, changed_subjids = .changed_subj,
                     quietly = TRUE, debug = TRUE)
    ),
    regexp = "different columns"
  )
})

test_that("missing id and missing changed_subjids are rejected", {
  cached <- cleangrowth(.base, quietly = TRUE)

  # changed_subjids not supplied
  expect_error(gc_partial_run(.new, cached), regexp = "changed_subjids must be supplied")

  # new_data lacking an id column
  no_id <- copy(.new)
  no_id[, id := NULL]
  expect_error(
    gc_partial_run(no_id, cached, changed_subjids = .changed_subj),
    regexp = "must have a 'id' column|must have an 'id'|'id' column"
  )
})

test_that("default mean_ht: cache and changed subset may differ, and the result matches a full run", {
  kid <- function(s) data.table::data.table(
    subjid = s, sex = 0L, param = rep(c("WEIGHTKG", "HEIGHTCM"), each = 2),
    agedays = rep(c(365L, 730L), 2), measurement = c(10, 12.5, 75, 87))
  adult <- function(s) data.table::data.table(
    subjid = s, sex = 0L, param = rep(c("WEIGHTKG", "HEIGHTCM"), each = 2),
    agedays = rep(c(9000L, 9400L), 2), measurement = c(70, 71, 175, 175))
  mk <- function(...) { d <- data.table::rbindlist(list(...)); d[, id := .I]; d[] }

  # The cache has the column; the changed subject is a child, so the subset does not.
  d1 <- mk(kid(1), kid(2), adult(3))
  cached1 <- suppressWarnings(cleangrowth(data.table::copy(d1), quietly = TRUE))
  d1b <- data.table::copy(d1); d1b[subjid == 1 & param == "WEIGHTKG", measurement := measurement * 1.02]
  p1 <- suppressWarnings(gc_partial_run(d1b, cached1, changed_subjids = "1", quietly = TRUE))
  f1 <- suppressWarnings(cleangrowth(data.table::copy(d1b), quietly = TRUE))
  data.table::setorder(p1, id); data.table::setorder(f1, id)
  expect_setequal(names(p1), names(f1))
  expect_equal(p1$mean_ht, f1$mean_ht)

  # The cache lacks the column; the changed subject is now an adult.
  d2 <- mk(kid(1), kid(2), kid(3))
  cached2 <- suppressWarnings(cleangrowth(data.table::copy(d2), quietly = TRUE))
  d2b <- mk(kid(1), kid(2), adult(3))
  p2 <- suppressWarnings(gc_partial_run(d2b, cached2, changed_subjids = "3", quietly = TRUE))
  f2 <- suppressWarnings(cleangrowth(data.table::copy(d2b), quietly = TRUE))
  data.table::setorder(p2, id); data.table::setorder(f2, id)
  expect_setequal(names(p2), names(f2))
  expect_equal(p2$mean_ht, f2$mean_ht)
})
