# Checkpointing via cleangrowth(checkpoint_dir = ...): the folded interface.
#
# Checkpointing is built into cleangrowth() and driven by checkpoint_dir. The
# core guarantee is that a checkpointed run is identical to a single-pass one
# (every subject's rows land in one checkpoint, so checkpointing cannot change
# any result). These tests cover that equivalence, fresh-vs-resume, the
# checkpoint cap, knob independence, re-widen under checkpoint, carry-through of
# extra columns, and the deprecated wrapper shim.

testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

make_mini <- function() {
  ages <- c(0L, 180L, 365L)
  d <- data.table::rbindlist(lapply(1:6, function(s) {
    data.table::data.table(
      subjid      = s,
      param       = rep(c("WEIGHTKG", "HEIGHTCM"), each = length(ages)),
      agedays     = rep(ages, 2),
      sex         = s %% 2L,
      measurement = c(3.3 + 0.01 * s + c(0, 5.5, 7.0),
                      50  + 0.10 * s + c(0, 18, 25))
    )
  }))
  d[, id := .I]
  d[]
}

test_that("cleangrowth(checkpoint_dir=) is identical to a single-pass run", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_equiv")
  unlink(dir, recursive = TRUE)

  ref <- cleangrowth(d, quietly = TRUE, debug = TRUE)
  ck  <- cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3,
                     quietly = TRUE, debug = TRUE)

  data.table::setkey(ref, id); data.table::setkey(ck, id)
  # exclude, the recentered z (tbc.sd), and every shared column must match.
  expect_equal(as.character(ck$exclude), as.character(ref$exclude))
  expect_equal(ck$tbc.sd, ref$tbc.sd)
  for (cn in intersect(names(ref), names(ck)))
    expect_equal(ck[[cn]], ref[[cn]], info = cn)
})

test_that("temporary checkpoint files are removed when a run stops with an error", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "growthcleanr_checkpoints")
  unlink(dir, recursive = TRUE)

  # Fail after every checkpoint file has been written, and record what was there.
  n_written <- NA_integer_
  testthat::local_mocked_bindings(
    .combine_checkpoints = function(ckpt_files, ...) {
      n_written <<- sum(file.exists(ckpt_files))
      stop("simulated failure")
    },
    .package = "growthcleanr")

  expect_error(suppressWarnings(cleangrowth(d, checkpoint_size = 3, quietly = TRUE)),
               "simulated failure")
  expect_gt(n_written, 0L)
  expect_false(dir.exists(dir))
})

test_that("a kept checkpoint folder is not removed when a run stops with an error", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_kept_on_error")
  unlink(dir, recursive = TRUE)

  testthat::local_mocked_bindings(
    .combine_checkpoints = function(...) stop("simulated failure"),
    .package = "growthcleanr")

  expect_error(suppressWarnings(cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3,
                                            quietly = TRUE)),
               "simulated failure")
  expect_gt(length(list.files(dir, pattern = "^checkpoint_")), 0L)
})

test_that("default run starts fresh: stale checkpoints removed, result correct", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_fresh")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  # leftovers from a prior run: a same-numbered bogus file + a higher stale one
  saveRDS(data.frame(bogus = 1), file.path(dir, "checkpoint_0001.rds"))
  saveRDS(data.frame(bogus = 1), file.path(dir, "checkpoint_0099.rds"))

  res <- cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3, quietly = TRUE)

  # stale higher-numbered file is gone (cannot leak into the combine)
  expect_false(file.exists(file.path(dir, "checkpoint_0099.rds")))
  expect_equal(nrow(res), nrow(d))
  ref <- cleangrowth(d, quietly = TRUE)
  m <- merge(res[, .(id, exclude)], ref[, .(id, exclude_ref = exclude)], by = "id")
  expect_equal(as.character(m$exclude), as.character(m$exclude_ref))
})

test_that("checkpoint_resume = TRUE reuses prior checkpoints", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_resume")
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3, quietly = TRUE)  # establish

  # tamper checkpoint_0001 with a recognizable value so reuse is detectable
  c1 <- readRDS(file.path(dir, "checkpoint_0001.rds"))
  c1$measurement[1] <- 999.0
  saveRDS(c1, file.path(dir, "checkpoint_0001.rds"))

  res_reuse <- cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3,
                           checkpoint_resume = TRUE, quietly = TRUE)
  expect_true(999.0 %in% res_reuse$measurement)   # tamper survived -> reused

  res_fresh <- cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3, quietly = TRUE)
  expect_false(999.0 %in% res_fresh$measurement)  # default overwrote it
})

test_that("resuming with changed data warns in user terms and the summary adds up", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_resume_changed")
  unlink(dir, recursive = TRUE)
  cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3, quietly = TRUE)
  d2 <- data.table::copy(d)
  d2[1:3, id := 100:102]   # rows the reused checkpoint files do not hold

  msgs <- character(0)
  expect_warning(
    res <- withCallingHandlers(
      cleangrowth(d2, checkpoint_dir = dir, checkpoint_size = 3, checkpoint_resume = TRUE),
      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }),
    "3 of 36 measurement(s) were not found in the checkpoint files in checkpoint_dir", fixed = TRUE)
  expect_equal(sum(is.na(res$exclude)), 3L)
  # the unmatched rows keep their submitted values; only the results are NA
  nr <- res[is.na(exclude)]
  expect_identical(nr$id, 100:102)
  expect_identical(nr$subjid, d2$subjid[1:3])
  expect_identical(nr$measurement, d2$measurement[1:3])
  expect_false(anyNA(res[, .(id, subjid, param, agedays, sex, measurement)]))
  summ <- paste(msgs, collapse = "\n")
  expect_match(summ, "No result   : 3 (8.3%)", fixed = TRUE)
  expect_false(grepl(".combine_checkpoints", summ, fixed = TRUE))
})

test_that("resuming with changed data keeps input values with character ids and extra columns", {
  d   <- make_mini()
  d[, id := paste0("r", id)]
  d[, site := paste0("s", subjid)]
  dir <- file.path(tempdir(), "gc_ckpt_resume_changed_chr")
  unlink(dir, recursive = TRUE)
  cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3, quietly = TRUE)
  d2 <- data.table::copy(d)
  d2[1:2, id := c("new1", "new2")]
  res <- suppressWarnings(cleangrowth(d2, checkpoint_dir = dir, checkpoint_size = 3,
                                      checkpoint_resume = TRUE, quietly = TRUE))
  expect_equal(nrow(res), nrow(d2))
  expect_identical(res$id, d2$id)
  expect_identical(res$site, d2$site)
  expect_identical(res$measurement, d2$measurement)
  expect_equal(sum(is.na(res$exclude)), 2L)
})

test_that("explicit checkpoint_size over the checkpoint cap is increased with a warning", {
  # The real cap is 9999 (four-digit checkpoint file numbers); lower it here so
  # the test stays small.
  local_mocked_bindings(.gc_max_checkpoints = function() 10L, .package = "growthcleanr")
  big <- data.table::data.table(
    subjid = 1:21, param = "WEIGHTKG", agedays = 365L,
    sex = 0L, measurement = 12.0
  )
  big[, id := .I]
  dir <- file.path(tempdir(), "gc_ckpt_cap")
  unlink(dir, recursive = TRUE)
  ws <- character(0)
  res <- withCallingHandlers(
    cleangrowth(big, checkpoint_dir = dir, checkpoint_size = 1, quietly = TRUE),
    warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("increased checkpoint_size to 3", ws)))
  expect_equal(nrow(res), 21L)
})

test_that("checkpoint_size and batch_size are independent knobs", {
  # batch_size < checkpoint_size must still equal the single-pass run: checkpoint_size is the
  # persist unit, batch_size the in-memory unit within a checkpoint.
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_knobs")
  unlink(dir, recursive = TRUE)
  ref <- cleangrowth(d, quietly = TRUE)
  ck  <- cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 4, batch_size = 2,
                     quietly = TRUE)
  data.table::setkey(ref, id); data.table::setkey(ck, id)
  expect_equal(as.character(ck$exclude), as.character(ref$exclude))
})

test_that("re-widen (semi-wide) output under checkpoint equals non-checkpoint", {
  sw <- data.table::data.table(
    subjid   = c("A", "A", "A", "B", "B"),
    agedays  = c(100L, 200L, 300L, 50L, 120L),
    sex      = c(0L, 0L, 0L, 1L, 1L),
    WEIGHTKG = c(6.0, 8.0, 9.5, 4.0, 5.5),
    HEIGHTCM = c(60.0, 68.0, NA, 55.0, 60.0),
    HEADCM   = c(42.0, NA, NA, 38.0, NA),
    site     = c("c1", "c1", "c2", "c3", "c3"))
  dir <- file.path(tempdir(), "gc_ckpt_semiwide")
  unlink(dir, recursive = TRUE)
  r_no <- cleangrowth(sw, input_format = "semi_wide", output_format = "semi_wide",
                      quietly = TRUE)
  r_ck <- cleangrowth(sw, input_format = "semi_wide", output_format = "semi_wide",
                      checkpoint_dir = dir, checkpoint_size = 1, quietly = TRUE)
  expect_equal(r_ck, r_no)
})

test_that("carry-through (extra long columns) under checkpoint equals non-checkpoint", {
  d <- make_mini()
  # Two non-growthcleanr columns that must ride through to the output unchanged,
  # appended after the gc columns, keyed to their original rows. Use per-row
  # distinct values so a misaligned reattach would show up.
  d[, extra_note := paste0("n", .I)]
  d[, site       := ifelse(subjid %% 2L == 0L, "east", "west")]
  dir <- file.path(tempdir(), "gc_ckpt_carry")
  unlink(dir, recursive = TRUE)

  r_no <- cleangrowth(d, quietly = TRUE)
  r_ck <- cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3, quietly = TRUE)

  # Carried columns are present, appended after the gc columns.
  expect_true(all(c("extra_note", "site") %in% names(r_ck)))
  # Row-for-row identical to the non-checkpoint run (compare on id).
  data.table::setkey(r_no, id); data.table::setkey(r_ck, id)
  expect_equal(r_ck, r_no)
})

test_that("all-adult data (explicit cutpoint 18) cleans identically checkpoint vs non-checkpoint", {
  # The adult_cutpoint the run uses is finalized dataset-wide and forwarded to each
  # checkpoint, so a checkpointed run applies the same cutpoint a single run would. Here all
  # rows are 18.5y with adult_cutpoint = 18, so they are cleaned by the adult
  # algorithm; some subjects have no sex (adults need none), so both runs Include
  # every row and nothing errors -- identical checkpoint vs non-checkpoint.
  # (growthcleanr does not auto-lower the cutpoint, so it must be set explicitly;
  # without it, the default 20 would send these 18-20y rows to the child path and
  # the sex-less ones would be set aside -- identically under both runs.)
  ad <- 6752L   # ~18.5y
  d  <- data.table::rbindlist(lapply(1:4, function(s) data.table::data.table(
    subjid = s, param = c("WEIGHTKG", "HEIGHTCM"), agedays = ad,
    sex = if (s <= 2) s %% 2L else NA_integer_, measurement = c(70 + s, 170 + s))))
  d[, id := .I]
  dir <- file.path(tempdir(), "gc_ckpt_alladult")
  unlink(dir, recursive = TRUE)

  ref <- suppressMessages(cleangrowth(data.table::copy(d), adult_cutpoint = 18, quietly = TRUE))
  ck  <- suppressMessages(cleangrowth(data.table::copy(d), adult_cutpoint = 18,
                                      checkpoint_dir = dir, checkpoint_size = 1, quietly = TRUE))
  data.table::setkey(ref, id); data.table::setkey(ck, id)
  expect_equal(as.character(ck$exclude), as.character(ref$exclude))
  expect_true(all(ck$exclude == "Include"))   # cleaned as adults, not set aside
})

test_that("consolidated input warning is identical checkpoint vs non-checkpoint", {
  # A dataset that trips several set-aside detections plus id auto-regen. The
  # warning must be one dataset-wide message, byte-identical to the non-checkpoint
  # run -- not fragmented per checkpoint with per-checkpoint denominators.
  d <- make_mini()                 # 6 subjects x 6 rows
  d[1, measurement := 0]           # a zero weight
  d <- rbind(d, data.table(subjid = 99, param = "SYSTOLIC", agedays = 365L,
                           sex = 0L, measurement = 120, id = 999L))  # non-gc param
  d[, id := c(1L, seq_len(.N - 1L))]  # duplicated id -> auto-regen notice

  grab <- function(expr) {
    w <- character(0)
    withCallingHandlers(expr,
      warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
    w
  }
  dir <- file.path(tempdir(), "gc_ckpt_warn")
  unlink(dir, recursive = TRUE)
  growthcleanr:::.gc_reset_session_notices()  # ids notice is once per session
  w_no <- grab(cleangrowth(data.table::copy(d), quietly = TRUE))
  growthcleanr:::.gc_reset_session_notices()  # ids notice is once per session
  w_ck <- grab(cleangrowth(data.table::copy(d), checkpoint_dir = dir,
                           checkpoint_size = 2, quietly = TRUE))

  expect_length(w_ck, 1L)                 # one consolidated warning, not per-checkpoint
  expect_equal(w_ck, w_no)                # byte-identical text (dataset-wide)
  expect_true(grepl("2 of 37 measurement.*could not be used", w_ck))  # dataset-wide count
  expect_true(grepl("Generated ids", w_ck))   # id notice not lost
})

test_that("deprecated cleangrowth_checkpoint() shim warns and forwards", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_shim")
  unlink(dir, recursive = TRUE)
  expect_warning(
    res <- cleangrowth_checkpoint(d, dir, checkpoint_size = 3, quietly = TRUE),
    "deprecated"
  )
  # shim result equals the folded interface
  ref <- cleangrowth(d, checkpoint_dir = file.path(tempdir(), "gc_ckpt_shim_ref"),
                     checkpoint_size = 3, quietly = TRUE)
  data.table::setkey(res, id); data.table::setkey(ref, id)
  expect_equal(as.character(res$exclude), as.character(ref$exclude))
})

# --- Standalone re-combine: cleangrowth_checkpoint_combine() ------------------
# The recovery path for a run whose checkpoints all completed but whose final combine
# failed: re-combine the persisted checkpoint_NNNN.rds files without re-cleaning.
# Exercised here directly, including id ordering, the missing-id warning,
# verbose/log-file output, and the two hard stops.

test_that("cleangrowth_checkpoint_combine() re-combines a completed run in id order", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_recombine")
  unlink(dir, recursive = TRUE)
  # A completed checkpointed run leaves checkpoint_NNNN.rds behind (keep_checkpoints).
  ref <- cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3,
                     keep_checkpoints = TRUE, quietly = TRUE)

  comb <- cleangrowth_checkpoint_combine(dir, id_order = d$id, quietly = TRUE)

  expect_s3_class(comb, "data.table")
  expect_equal(nrow(comb), nrow(d))
  # id_order = d$id -> rows come back in original input order
  expect_equal(as.character(comb$id), as.character(d$id))
  # checkpointing never changes a result: exclude matches the finalized run per id
  m <- merge(comb[, .(id, exclude)],
             ref[,  .(id, exclude_ref = exclude)], by = "id")
  expect_equal(as.character(m$exclude), as.character(m$exclude_ref))
})

test_that("cleangrowth_checkpoint_combine() with id_order = NULL returns all rows in checkpoint order", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_recombine_null")
  unlink(dir, recursive = TRUE)
  cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 2,
              keep_checkpoints = TRUE, quietly = TRUE)

  comb <- cleangrowth_checkpoint_combine(dir, quietly = TRUE)
  expect_equal(nrow(comb), nrow(d))
  # no reorder requested, but every input id must still be present exactly once
  expect_setequal(as.character(comb$id), as.character(d$id))
})

test_that("cleangrowth_checkpoint_combine() warns on id_order ids absent from the files", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_recombine_warn")
  unlink(dir, recursive = TRUE)
  cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3,
              keep_checkpoints = TRUE, quietly = TRUE)

  bogus <- max(d$id) + 1L
  expect_warning(
    comb <- cleangrowth_checkpoint_combine(dir, id_order = c(d$id, bogus),
                                           quietly = TRUE),
    "1 of .* id\\(s\\) in id_order were not found in the checkpoint files"
  )
  # the unmatched id produces one extra NA-filled row
  expect_equal(nrow(comb), nrow(d) + 1L)
  expect_true(anyNA(comb$id))
})

test_that("cleangrowth_checkpoint_combine() errors on a missing directory", {
  expect_error(
    cleangrowth_checkpoint_combine(file.path(tempdir(), "gc_ckpt_no_such_dir_xyz")),
    "does not exist"
  )
})

test_that("cleangrowth_checkpoint_combine() errors when no checkpoint files are present", {
  empty <- file.path(tempdir(), "gc_ckpt_recombine_empty")
  unlink(empty, recursive = TRUE); dir.create(empty, recursive = TRUE)
  expect_error(
    cleangrowth_checkpoint_combine(empty),
    "No checkpoint_.*rds files found"
  )
})

test_that("cleangrowth_checkpoint_combine() honors quietly / verbose / log_file", {
  d   <- make_mini()
  dir <- file.path(tempdir(), "gc_ckpt_recombine_log")
  unlink(dir, recursive = TRUE)
  cleangrowth(d, checkpoint_dir = dir, checkpoint_size = 3,
              keep_checkpoints = TRUE, quietly = TRUE)

  # quietly = TRUE is silent on the console
  expect_no_message(cleangrowth_checkpoint_combine(dir, quietly = TRUE))
  # default (quietly = FALSE) prints the top-level combine/done lines
  expect_message(cleangrowth_checkpoint_combine(dir), "combining")
  # verbose = TRUE additionally prints the per-file read progress
  expect_message(cleangrowth_checkpoint_combine(dir, verbose = TRUE),
                 "Reading and combining")

  # log_file receives the log even when the console is quiet
  lf <- file.path(tempdir(), "gc_ckpt_recombine.log")
  if (file.exists(lf)) unlink(lf)
  cleangrowth_checkpoint_combine(dir, log_file = lf, quietly = TRUE)
  expect_true(file.exists(lf))
  expect_true(any(grepl("combining", readLines(lf))))
})

test_that("default mean_ht: same result under checkpointing, with and without adult rows", {
  mk <- function(subj, ages) data.table::data.table(
    subjid = subj, sex = 0L,
    param = rep(c("WEIGHTKG", "HEIGHTCM"), each = length(ages)),
    agedays = rep(ages, 2),
    measurement = c(seq(70, by = 0.5, length.out = length(ages)), rep(175, length(ages))))
  # Child-only: no column, single run or checkpointed.
  kid <- data.table::rbindlist(lapply(1:6, function(s) data.table::data.table(
    subjid = s, sex = 0L, param = rep(c("WEIGHTKG", "HEIGHTCM"), each = 2),
    agedays = rep(c(365L, 730L), 2), measurement = c(10, 12.5, 75, 87))))
  ck_kid <- suppressWarnings(cleangrowth(data.table::copy(kid), quietly = TRUE, checkpoint_size = 2))
  expect_false("mean_ht" %in% names(ck_kid))
  # Mixed: the first checkpoints hold only children and the last only adults, yet
  # every row has the column and it matches a single run.
  mix <- data.table::rbindlist(list(kid, mk(7, c(9000L, 9400L)), mk(8, c(9000L, 9400L))))
  ref <- suppressWarnings(cleangrowth(data.table::copy(mix), quietly = TRUE, checkpoint = FALSE))
  ck  <- suppressWarnings(cleangrowth(data.table::copy(mix), quietly = TRUE, checkpoint_size = 2))
  expect_true("mean_ht" %in% names(ck))
  data.table::setorder(ref, subjid, param, agedays); data.table::setorder(ck, subjid, param, agedays)
  expect_equal(ck$mean_ht, ref$mean_ht)
  expect_equal(unique(ck[subjid %in% 7:8 & param == "HEIGHTCM"]$mean_ht), 175)
  expect_true(all(is.na(ck[subjid %in% 1:6]$mean_ht)))
})

test_that("mean_ht = TRUE is returned under checkpointing, matching a single run", {
  # The mean_ht switch must reach each checkpoint's
  # cleangrowth() call, or the column is silently dropped.
  d <- data.table::rbindlist(lapply(1:3, function(s) data.table::data.table(
    subjid = s, sex = s %% 2L,
    param = rep(c("WEIGHTKG", "HEIGHTCM"), each = 3),
    agedays = rep(c(9000L, 9400L, 9800L), 2),
    measurement = c(70 + s, 71 + s, 72 + s, 170 + s, 170.5 + s, 170 + s))))
  d[, id := .I]
  dir <- file.path(tempdir(), "gc_ckpt_meanht")
  unlink(dir, recursive = TRUE)
  ref <- suppressMessages(cleangrowth(data.table::copy(d), mean_ht = TRUE, quietly = TRUE))
  ck  <- suppressMessages(cleangrowth(data.table::copy(d), mean_ht = TRUE, quietly = TRUE,
                                      checkpoint_dir = dir, checkpoint_size = 1))
  expect_true("mean_ht" %in% names(ck))
  data.table::setkey(ref, id); data.table::setkey(ck, id)
  expect_equal(ck$mean_ht, ref$mean_ht)
  expect_true(all(!is.na(ck[param == "HEIGHTCM", mean_ht])))
})

test_that("every cleangrowth() argument is forwarded to checkpoints or deliberately not", {
  # Guard against the next omission from .ckpt_clean_args (mean_ht was once
  # missing, which silently dropped the column under checkpointing). Find the
  # names in the `.ckpt_clean_args <- list(...)` call inside cleangrowth().
  find_args <- function(e) {
    if (missing(e)) return(NULL)   # empty argument, e.g. the blank in x[, j]
    if (is.call(e)) {
      if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".ckpt_clean_args")))
        return(names(as.list(e[[3]]))[-1])
      for (x in as.list(e)) { r <- find_args(x); if (!is.null(r)) return(r) }
    }
    NULL
  }
  forwarded <- find_args(body(cleangrowth))
  expect_true(length(forwarded) > 20)
  # Not forwarded on purpose: the data and column mapping (the runner passes
  # the canonical long checkpoint), date handling (agedays is already numeric),
  # input/output shape and re-widen settings (applied once outside the loop),
  # progress settings (passed by the runner itself), and the checkpoint
  # settings (each checkpoint is a plain, non-checkpointed call).
  not_forwarded <- c(
    "data", "subjid", "param", "agedays", "sex", "measurement", "id",
    "dob", "visit_date", "age_source", "date_order",
    "input_format", "output_format", "semiwide_cols", "widened_emit", "wide_cols",
    "quietly", "verbose",
    "checkpoint", "checkpoint_dir", "checkpoint_size", "checkpoint_resume",
    "keep_checkpoints", "log_file", ".checkpoint_driven", "...")
  missing <- setdiff(names(formals(cleangrowth)), c(forwarded, not_forwarded))
  expect_identical(missing, character(0),
                   info = paste("not forwarded to checkpoints:", paste(missing, collapse = ", ")))
})

test_that("the 1/2 sex stop fires before any checkpoint is cleaned, however the data are sorted", {
  base <- as.data.table(syngrowth)[agedays < 7000]
  s <- unique(base$subjid)[1:12]
  d <- base[subjid %in% s]
  o <- match(d$subjid, s)
  d[, subjid := sprintf("S%02d", o)]
  d[, id := .I]
  # The first checkpoint holds only 1s, the second only 2s.
  d[, sex := ifelse(o <= 6, 1L, 2L)]
  ckdir <- tempfile("ckpt12_")
  expect_error(suppressWarnings(suppressMessages(
    cleangrowth(d, quietly = TRUE, checkpoint_dir = ckdir, checkpoint_size = 6L))),
    "1=male/2=female")
  expect_length(list.files(ckdir, pattern = "^checkpoint_.*rds$"), 0L)
  # 0 = unknown alongside 1 and 2 is not 1/2 coding, even if one checkpoint has
  # only 1s and 2s: a checkpointed run behaves like a single run.
  d[, sex := ifelse(o <= 3, 1L, ifelse(o <= 6, 2L, ifelse(o <= 9, 0L, 1L)))]
  expect_error(suppressWarnings(suppressMessages(
    cleangrowth(d, quietly = TRUE, checkpoint_dir = tempfile("ckpt012_"), checkpoint_size = 6L))),
    NA)
})
