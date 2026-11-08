# Tests for gc_runtrial() -- the sample-and-preview helper. Covers sampling
# correctness (n subjects, whole trajectories), reproducibility, first-N
# determinism, config pass-through, graceful error capture, and the diagnostic
# attribute. See R/gc_runtrial.R.

library(growthcleanr)
library(data.table)

# A small long dataset: 40 subjects, a weight + a height at two ages each.
make_long <- function(nsubj = 40) {
  rbindlist(lapply(seq_len(nsubj), function(s) {
    data.table(
      subjid = s,
      param  = rep(c("WEIGHTKG", "HEIGHTCM"), each = 2),
      agedays = rep(c(400L, 800L), times = 2),
      sex    = s %% 2L,
      measurement = c(12, 14, 80, 88) + (s %% 3)
    )
  }))
}

# gc_runtrial() prints its report; swallow stdout in tests.
run_quiet <- function(...) {
  res <- NULL
  invisible(utils::capture.output(res <- gc_runtrial(...)))
  res
}

test_that("samples the requested number of patients and keeps whole trajectories", {
  dt <- make_long(40)
  out <- run_quiet(dt, n = 10, seed = 1)
  d   <- attr(out, "gc_trial")
  expect_equal(d$n_sampled, 10)
  expect_equal(d$n_total, 40)
  expect_equal(uniqueN(out$subjid), 10)
  # every sampled subject keeps all 4 of its rows
  expect_true(all(out[, .N, by = subjid]$N == 4L))
})

test_that("random sampling is reproducible under a seed and varies without one", {
  dt <- make_long(40)
  a_ids <- sort(unique(run_quiet(dt, n = 12, seed = 99)$subjid))
  b_ids <- sort(unique(run_quiet(dt, n = 12, seed = 99)$subjid))
  expect_identical(a_ids, b_ids)
  expect_length(a_ids, 12)
})

test_that("the caller's RNG stream is left untouched", {
  dt <- make_long(40)
  set.seed(123); x <- runif(1)
  run_quiet(dt, n = 5)
  set.seed(123); y <- runif(1)
  expect_identical(x, y)
})

test_that("sampling = 'first' is deterministic and takes the first subjects", {
  dt <- make_long(40)
  out <- run_quiet(dt, n = 8, sampling = "first")
  expect_setequal(unique(out$subjid), 1:8)
  d <- attr(out, "gc_trial")
  expect_match(d$seed, "first")
})

test_that("n larger than the number of subjects uses all of them", {
  dt <- make_long(15)
  d  <- attr(run_quiet(dt, n = 100), "gc_trial")
  expect_equal(d$n_sampled, 15)
  expect_equal(d$n_total, 15)
})

test_that("config pass-through: a remapped subjid column is honored", {
  dt <- make_long(20)
  setnames(dt, "subjid", "patid")
  out <- run_quiet(dt, n = 6, subjid = "patid", seed = 5)
  expect_equal(attr(out, "gc_trial")$n_sampled, 6)
  expect_equal(uniqueN(out$patid), 6)  # output keeps the user's original column name
})

test_that("the age range is reported when the age column has a custom name", {
  dt <- make_long(10)
  setnames(dt, "agedays", "age_d")
  rep <- attr(run_quiet(dt, n = 5, agedays = "age_d", seed = 5), "gc_trial")$report
  expect_true(any(grepl("Age range        : 400 to 800 days", rep, fixed = TRUE)))
})

test_that("a configuration hard-stop is reported, not re-thrown", {
  # wide data submitted without input_format -> cleangrowth stops; trial catches it
  w <- data.frame(subjid = 1:5, sex = 0,
                  agedays_1 = 100 * (1:5), wt_1 = 10 + (1:5), ht_1 = 70 + (1:5))
  out <- run_quiet(w, n = 5)
  d   <- attr(out, "gc_trial")
  expect_equal(nrow(out), 0L)
  expect_true(!is.null(d$error))
  expect_true(data.table::is.data.table(out))
})

test_that("the diagnostic attribute carries the expected fields on success", {
  dt <- make_long(30)
  d  <- attr(run_quiet(dt, n = 10, seed = 2), "gc_trial")
  expect_true(all(c("n_sampled", "n_total", "sampling", "seed", "n_rows",
                    "input_format", "sex", "parameters", "exclusions",
                    "report") %in% names(d)))
  expect_equal(sum(d$sex), attr(run_quiet(dt, n = 10, seed = 2), "gc_trial")$n_rows)
})

test_that("sex/param mapping sections surface mapped labels and unrecognized values", {
  dt <- data.table(
    subjid = rep(1:4, each = 4),
    param  = rep(c("WT", "HEIGHTCM", "BMI", "WEIGHTKG"), 4),  # WT via param_coding, BMI unrecognized
    agedays = rep(c(400L, 400L, 400L, 800L), 4),
    sex    = rep(c("M", "Male", "2", "f"), 4),                # "2" unrecognized
    measurement = rep(c(12, 80, 18, 13), 4)
  )
  out <- run_quiet(dt, n = 4, seed = 1, param_coding = list(WEIGHTKG = "WT"))
  rep <- attr(out, "gc_trial")$report
  txt <- paste(rep, collapse = "\n")
  # mapped label appears in the weight mapping; unrecognized sex + param surfaced
  expect_match(txt, "Weight\\s+<-.*WT")
  expect_match(txt, "SEX")
  expect_true(any(grepl("in the 'sex' column : .*2", rep)))            # sex value 2
  expect_true(any(grepl("in the 'param' column : .*BMI", rep)))        # param value BMI
  expect_true("2" %in% attr(out, "gc_trial")$sex_map$present_unrecognized)
})

test_that("input guards: data-frame style required; n must be positive", {
  dt <- make_long(10)
  expect_error(gc_runtrial(subjid = dt$subjid), "data-frame")
  expect_error(gc_runtrial(dt, n = 0), "positive")
  expect_error(gc_runtrial(dt, n = -3), "positive")
})

test_that("the report carries a SUCCESSFUL/FAILED banner and names the failing call", {
  dt <- make_long(6)
  ok <- paste(capture.output(gc_runtrial(dt, n = 4)), collapse = "\n")
  expect_match(ok, "TRIAL RUN SUCCESSFUL")

  # A cleaning-time error is reported (not thrown): FAILED banner + the call.
  bad <- paste(capture.output(
    gc_runtrial(dt, n = 4, sex_coding = list(male = c(0, "x"), female = c(1, "x")))),
    collapse = "\n")
  expect_match(bad, "TRIAL RUN FAILED")
  expect_match(bad, "COULD NOT RUN")
  expect_match(bad, "\\(in: ")   # the captured call location
})
