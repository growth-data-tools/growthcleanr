# Messages that help users coming from growthcleanr 2.x: removed-argument stops,
# the one-time vector-style note, once-per-session generated-ids notice,
# adult_cutpoint clamp warning, case-insensitive adult_permissiveness, summary
# wording, longwide() unknown inclusion_types, and command-line short flags.

library(growthcleanr)
library(data.table)
skip_on_cran()

.mk <- function(n = 3) {
  d <- as.data.table(syngrowth)
  d[subjid %in% unique(subjid)[seq_len(n)]]
}

.grab_warnings <- function(expr) {
  w <- character(0)
  withCallingHandlers(expr, warning = function(c) {
    w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning")
  })
  w
}

test_that("removed 2.x arguments stop with the replacement", {
  d <- .mk()
  expect_error(cleangrowth(d, num.batches = 4), "`num.batches` was removed.*use cores")
  expect_error(cleangrowth(d, weight_cap = 150), "adult_scale_max_lbs instead \\(in pounds, not kg\\)")
  expect_error(cleangrowth(d, include.carryforward = TRUE), "use cf_rescue")
  expect_error(cleangrowth(d, prelim_infants = TRUE), "Removed and renamed arguments")
  # Every 2.2.3 argument that no longer exists is on the list.
  removed <- c("recover.unit.error", "sd.extreme", "z.extreme", "lt3.exclude.mode",
               "height.tolerance.cm", "sdmedian.filename", "sdrecentered.filename",
               "include.carryforward", "ewma.exp", "num.batches", "weight_cap",
               "adult_columns_filename", "prelim_infants")
  expect_setequal(names(growthcleanr:::.gc_removed_args), removed)
  expect_length(intersect(removed, names(formals(cleangrowth))), 0L)
})

test_that("other unknown arguments still stop, without evaluating them", {
  d <- .mk()
  expect_error(cleangrowth(d, adult_cutpont = 18), "unused argument in cleangrowth\\(\\): adult_cutpont = 18")
  expect_error(cleangrowth(d, foo = 1, bar = 2), "unused arguments in cleangrowth\\(\\): foo = 1, bar = 2")
  # An undefined object in the unknown argument is not evaluated.
  expect_error(cleangrowth(d, foo = no_such_object), "unused argument.*foo = no_such_object")
  # The deprecated checkpoint wrapper forwards `...`, so it gets the same stop.
  expect_error(suppressWarnings(cleangrowth_checkpoint(d, tempfile(), num.batches = 2)),
               "`num.batches` was removed")
})

test_that("vector style prints a one-time note, even when quiet", {
  d <- .mk()
  growthcleanr:::.gc_reset_session_notices()
  vec <- function() cleangrowth(subjid = d$subjid, param = d$param, agedays = d$agedays,
                                sex = d$sex, measurement = d$measurement, id = d$id,
                                quietly = TRUE)
  expect_message(vec(), "returns a data table.*Shown once per R session")
  expect_silent(vec())
  # Data-frame style never prints it.
  growthcleanr:::.gc_reset_session_notices()
  expect_silent(cleangrowth(d, quietly = TRUE))
})

test_that("generated-ids notice is shown once per session", {
  d <- .mk(); d[, id := NULL]
  growthcleanr:::.gc_reset_session_notices()
  w1 <- .grab_warnings(cleangrowth(d, quietly = TRUE))
  w2 <- .grab_warnings(cleangrowth(d, quietly = TRUE))
  expect_true(any(grepl("Generated ids", w1)))
  expect_false(any(grepl("Generated ids", w2)))
  # The "Unusable data" section still appears on every run.
  d[1, measurement := 0]
  w3 <- .grab_warnings(cleangrowth(d, quietly = TRUE))
  expect_true(any(grepl("Unusable data", w3)))
  expect_false(any(grepl("Generated ids", w3)))
  # gc_runtrial() reports the notice but does not use it up for the full run.
  growthcleanr:::.gc_reset_session_notices()
  tr <- utils::capture.output(gc_runtrial(d, n = 3, seed = 1))
  w4 <- .grab_warnings(cleangrowth(d, quietly = TRUE))
  expect_true(any(grepl("Generated ids", w4)))
  # Duplicated (or NA) ids describe the dataset, so they are reported every run.
  dd <- .mk(); dd[, id := 1L]
  expect_true(any(grepl("duplicated value", .grab_warnings(cleangrowth(dd, quietly = TRUE)))))
  expect_true(any(grepl("duplicated value", .grab_warnings(cleangrowth(dd, quietly = TRUE)))))
})

test_that("adult_cutpoint outside 18-20 warns when clamped", {
  d <- .mk()
  w <- .grab_warnings(cleangrowth(d, adult_cutpoint = 18 * 365.25, quietly = TRUE))
  expect_true(any(grepl("6574.5 was entered and set to 20. adult_cutpoint is in years, not days", w, fixed = TRUE)))
  w <- .grab_warnings(cleangrowth(d, adult_cutpoint = 16, quietly = TRUE))
  expect_true(any(grepl("16 was entered and set to 18.", w, fixed = TRUE)))
  expect_false(any(grepl("in years, not days", w)))
  w <- .grab_warnings(cleangrowth(d, adult_cutpoint = 19, quietly = TRUE))
  expect_false(any(grepl("adult_cutpoint must be between", w)))
  # Checkpointed run: one warning, not one per checkpoint.
  w <- .grab_warnings(cleangrowth(d, adult_cutpoint = 25, quietly = TRUE,
                                  checkpoint_dir = file.path(tempdir(), "gc_v66"),
                                  checkpoint_size = 1))
  expect_equal(sum(grepl("adult_cutpoint must be between", w)), 1L)
})

test_that("adult_permissiveness is case- and space-insensitive", {
  d <- .mk(); d[, agedays := agedays + 25 * 365]
  a <- cleangrowth(d, adult_permissiveness = "tighter", quietly = TRUE)
  b <- cleangrowth(d, adult_permissiveness = " Tighter ", quietly = TRUE)
  expect_identical(a$exclude, b$exclude)
  e <- tryCatch(cleangrowth(d, adult_permissiveness = "medium", quietly = TRUE),
                error = function(e) e)
  expect_match(conditionMessage(e), "must be one of: loosest, looser, tighter, tightest")
  expect_null(conditionCall(e))
})

test_that("cleaning summary says included / Set aside", {
  lines <- growthcleanr:::.gc_cleaning_summary_lines(
    group = c("weight", "weight", "height", NA), excl_chr = c("Include", "Exclude-CF", "Include", "Exclude-Missing-Info"),
    notcleaned = c(FALSE, FALSE, FALSE, TRUE), n_input = 4L)
  expect_true(any(grepl("Weight      : 1 included \\(50.0%\\), 1 excluded \\(50.0%\\)", lines)))
  expect_true(any(grepl("Set aside   : 1 \\(25.0%\\)", lines)))
  expect_true(any(grepl("unusable as submitted", lines)))
  expect_false(any(grepl("cleaned \\(", lines)))
})

test_that("longwide() warns on codes that are not growthcleanr codes", {
  d <- .mk()
  d$exclude <- cleangrowth(d, quietly = TRUE)$exclude
  expect_warning(longwide(d, inclusion_types = c("Include", "Exclude-Carried-Forward")),
                 "'Exclude-Carried-Forward' is not a growthcleanr version 3 exclusion code")
  # Summary, Detailed and bin_exclude values are all known.
  expect_no_warning(longwide(d, inclusion_types = c("Include", "Exclude-CF", "Exclude-C-CF", "Exclude")))
  # include_all ignores inclusion_types, so no warning.
  expect_no_warning(longwide(d, include_all = TRUE, inclusion_types = "junk"))
  d$exclude <- NULL
  expect_error(longwide(d), "2.x scripts used 'gcr_result'")
})

test_that("positional 2.x call points to the version 3 interface", {
  d <- .mk()
  e <- tryCatch(cleangrowth(d$subjid, d$param, d$agedays, d$sex, d$measurement),
                error = conditionMessage)
  expect_match(e, "`data` must be a data.frame or data.table, but a character of length")
  expect_match(e, "If this is code written for growthcleanr 2.x", fixed = TRUE)
  expect_match(e, "Updating code written for growthcleanr 2.2.3", fixed = TRUE)
})

test_that("cf_rescue = TRUE gets an informative error", {
  d <- .mk()
  msg <- "cf_rescue must be one of \"standard\", \"none\", or \"all\". The default is \"standard\", and TRUE/FALSE is not accepted. For 2.x include.carryforward = FALSE (the 2.2.3 default), the closest is \"none\"; for TRUE, \"all\"."
  expect_error(cleangrowth(d, cf_rescue = TRUE), msg, fixed = TRUE)
  expect_error(cleangrowth(d, cf_rescue = "sometimes"), msg, fixed = TRUE)
  expect_error(cleangrowth(d, cf_rescue = c("none", "all")), msg, fixed = TRUE)
  # Unambiguous abbreviations still work, as with match.arg().
  expect_identical(cleangrowth(d, cf_rescue = "stand", quietly = TRUE)$exclude,
                   cleangrowth(d, quietly = TRUE)$exclude)
  # Case-insensitive (and whitespace-tolerant), like adult_permissiveness.
  expect_identical(cleangrowth(d, cf_rescue = " None", quietly = TRUE)$exclude,
                   cleangrowth(d, cf_rescue = "none", quietly = TRUE)$exclude)
})

test_that("include.carryforward removal message gives both 2.x mappings", {
  d <- .mk()
  e <- tryCatch(cleangrowth(d, include.carryforward = FALSE), error = conditionMessage)
  expect_match(e, "\"none\" is closest to include.carryforward = FALSE, the 2.2.3 default", fixed = TRUE)
  expect_match(e, "\"all\" is closest to TRUE", fixed = TRUE)
})

test_that("a 2.x sd.recenter option is reported with the removed arguments", {
  d <- .mk()
  e <- tryCatch(cleangrowth(d, num.batches = 4, sd.recenter = "derive"), error = conditionMessage)
  expect_match(e, "`num.batches` was removed", fixed = TRUE)
  expect_match(e, "`sd.recenter = \"derive\"` was removed", fixed = TRUE)
  # A missing character value is valid (built-in table), so it is not reported.
  e_na <- tryCatch(cleangrowth(d, num.batches = 4, sd.recenter = NA_character_), error = conditionMessage)
  expect_match(e_na, "`num.batches` was removed", fixed = TRUE)
  expect_no_match(e_na, "sd.recenter", fixed = TRUE)
  # "none" is valid (no recentering), so it is not reported either.
  e_none <- tryCatch(cleangrowth(d, num.batches = 4, sd.recenter = "none"), error = conditionMessage)
  expect_match(e_none, "`num.batches` was removed", fixed = TRUE)
  expect_no_match(e_none, "sd.recenter", fixed = TRUE)
  # On its own it still stops, via the sd.recenter check.
  expect_error(cleangrowth(d, sd.recenter = "nhanes"), "sd.recenter = \"nhanes\" was removed", fixed = TRUE)
})

test_that("log.path is deprecated with a warning pointing to log_file", {
  d <- .mk()
  expect_warning(cleangrowth(d, log.path = tempdir(), quietly = TRUE),
                 "log.path is deprecated", fixed = TRUE)
  expect_no_warning(cleangrowth(d, quietly = TRUE))
})

test_that("percentages below 0.05% show as <0.1%, not 0.0%", {
  expect_identical(growthcleanr:::.gc_pct(1, 2414), "<0.1")
  expect_identical(growthcleanr:::.gc_pct(0, 2414), "0.0")
  expect_identical(growthcleanr:::.gc_pct(5, 100), "5.0")
  expect_identical(growthcleanr:::.gc_pct(1, 0), "")
})

test_that("unit-error-range warning mentions atypical growth", {
  d <- .mk(20)
  d[param == "WEIGHTKG", measurement := measurement * 2.2046]   # pounds labeled kg
  w <- .grab_warnings(cleangrowth(d, quietly = TRUE))
  w <- w[grepl("unit-error range", w)]
  expect_length(w, 1L)
  expect_match(w, "This may reflect frequent unit errors or may reflect measurements of people with atypical growth (for example, extremely premature infants, severe obesity, some medical conditions). If this parameter was recorded in imperial units, label it WEIGHTLBS",
               fixed = TRUE)
})

test_that("command-line driver rejects short flags and flags useless --widened-emit", {
  chk <- growthcleanr:::.gc_cli_check_short
  expect_null(chk(c("in.csv", "out.csv", "--adult-cutpoint", "18")))
  expect_null(chk(c("in.csv", "out.csv", "--piv-z-ht-low-young", "-25", "-h")))
  expect_match(chk(c("in.csv", "out.csv", "-w", "181.4")), "'-w' is not accepted")
  expect_match(chk(c("-s", "nhanes", "-n", "4")), "'-s', '-n' are not accepted")
  expect_match(growthcleanr:::.gc_cli_unknown_flag_hint("Undefined argument labels supplied"),
               "some flags were renamed or removed; see vignette\\(\"command-line\"\\)")
  # Choice values ignore case and surrounding spaces, like cleangrowth().
  o <- growthcleanr:::.gc_cli_opts(list(adult_permissiveness = " Tighter ",
                                        cf_rescue = "ALL"))
  expect_identical(o$adult_permissiveness, "tighter")
  expect_identical(o$cf_rescue, "all")
  expect_error(growthcleanr:::.gc_cli_opts(list(adult_permissiveness = "strict")),
               "expected one of loosest/looser/tighter/tightest")
  expect_warning(growthcleanr:::.gc_cli_opts(list(widened_emit = "exclude")),
                 "--widened-emit has no effect with long output")
  expect_no_warning(growthcleanr:::.gc_cli_opts(list(widened_emit = "exclude", input_format = "wide")))
  # With long output an invalid value still stops, as it would with wide output.
  expect_error(growthcleanr:::.gc_cli_opts(list(widened_emit = "181.4")),
               "'181.4' are not growthcleanr per-measurement output columns")
})

test_that("adjustcarryforward() stops and points to cf_rescue", {
  expect_error(adjustcarryforward(subjid = 1, exclude_opt = 0), "use the cf_rescue argument", ignore.case = TRUE)
})
