#' Trial run of growthcleanr on a sample of your data
#'
#' \code{gc_runtrial()} helps you confirm that your data are formatted correctly
#' and that growthcleanr is reading your columns, sex coding, and parameters the
#' way you intend -- before you commit to a full \code{\link{cleangrowth}} run. It
#' draws a small sample of patients, runs \code{cleangrowth()} on them with the
#' exact configuration you pass, and prints a short diagnostic report: how the
#' input was interpreted, how sex and parameters were mapped (the two highest-risk
#' settings, shown prominently), anything growthcleanr flagged while reading, and a
#' summary of the cleaning result. The cleaned sample is returned invisibly for
#' inspection. For a ready-to-run script, see "Check your setup first" in
#' \code{vignette("example-scripts")}.
#'
#' Sampling is by \emph{patient}: all of a sampled subject's rows are kept, so
#' trajectories stay intact. Random sampling (the default) is effectively free --
#' the only real cost is running growthcleanr on the sampled patients, which is
#' the same whether they were chosen at random or as the first \code{n}. Pass a
#' \code{seed} for a reproducible random sample, or \code{sampling = "first"} for
#' a deterministic peek that needs no seed.
#'
#' Only the data-frame calling style is supported (pass your table as
#' \code{data}); vector-style input is not. The trial always analyses and returns
#' results in long format so the diagnostic is well defined, regardless of any
#' \code{output_format} you set.
#'
#' If your configuration makes \code{cleangrowth()} stop (for example a forgotten
#' \code{input_format}, or a sex coding it will not guess), the trial reports that
#' stop as the headline finding instead of re-throwing it -- turning a hard error
#' into setup guidance. The sex and parameter mapping sections still print, since
#' they are derived from your configuration and data rather than the run.
#'
#' @param data A data.frame/data.table of growth measurements, in the same shape
#'   you would pass to \code{cleangrowth()} (long, semi-wide, or wide -- set
#'   \code{input_format} via \code{...} for the latter two).
#' @param n Number of patients (unique subjects) to sample. Default 100. If the
#'   data hold fewer subjects, all are used.
#' @param sampling \code{"random"} (default) or \code{"first"} (the first \code{n}
#'   subjects in order of appearance).
#' @param seed Optional integer seed for a reproducible random sample. The seed
#'   used is reported; your session's random-number state is left unchanged.
#' @param ... Any other \code{\link{cleangrowth}} argument (\code{subjid},
#'   \code{param}, \code{agedays}, \code{sex}, \code{measurement},
#'   \code{input_format}, \code{sex_coding}, \code{param_coding},
#'   \code{semiwide_cols}, \code{wide_cols}, \code{dob}, \code{visit_date}, ...),
#'   passed straight through so the trial tests your real configuration.
#'
#' @return Invisibly, a data.table: the cleaned sample in long format on success
#'   (empty on a configuration error), with the structured diagnostic attached as
#'   \code{attr(x, "gc_trial")}. Prints the report as a side effect.
#'
#' @examples
#' \donttest{
#' # quick check that a dataset is set up right
#' gc_runtrial(syngrowth)
#' }
#'
#' \dontrun{
#' # semi-wide data, custom sex coding, reproducible 50-patient sample
#' # (my_visits stands in for your own semi-wide dataset)
#' gc_runtrial(my_visits, n = 50, seed = 1,
#'             input_format = "semi_wide",
#'             sex_coding = list(male = "M", female = "F"))
#' }
#' @export
gc_runtrial <- function(data = NULL, n = 100,
                        sampling = c("random", "first"),
                        seed = NULL, ...) {
  sampling <- match.arg(sampling)

  if (is.null(data) || !is.data.frame(data)) {
    stop("gc_runtrial() needs a data.frame/data.table as `data` (the data-frame calling style). Vector-style input is not supported here; pass your table as `data`.")
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1) {
    stop(sprintf("`n` must be a single positive number of patients to sample; %s was entered.", .gc_desc_val(n)))
  }
  n <- as.integer(floor(n))
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || is.na(seed))) {
    stop("`seed` must be a single number, or NULL for an unseeded random sample.")
  }

  dots <- list(...)

  # --- Resolve the subject-id column (respect a `subjid` remap passed in ...).
  col_subjid <- if (is.null(dots[["subjid"]])) "subjid" else dots[["subjid"]]
  if (!is.character(col_subjid) || length(col_subjid) != 1L || is.na(col_subjid) ||
      !col_subjid %in% names(data)) {
    stop(sprintf("gc_runtrial() cannot find the subject-id column '%s' in `data`. Set `subjid` to the name of your subject-id column.",
                 if (is.character(col_subjid) && length(col_subjid) == 1L && !is.na(col_subjid)) col_subjid else "subjid"))
  }

  subj_all <- unique(data[[col_subjid]])
  M <- length(subj_all)
  if (M == 0L) {
    stop("`data` has no rows / no subjects to sample.")
  }
  n_use <- min(n, M)

  # --- Choose the sampled subjects, leaving the caller's RNG state untouched.
  if (identical(sampling, "random")) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit(suppressWarnings(rm(".Random.seed", envir = .GlobalEnv)), add = TRUE)
    }
    if (!is.null(seed)) set.seed(as.integer(seed))
    chosen      <- sample(subj_all, n_use)
    seed_report <- if (is.null(seed)) "none (results vary run to run; pass seed = to reproduce)" else as.character(as.integer(seed))
  } else {
    chosen      <- subj_all[seq_len(n_use)]
    seed_report <- "n/a (sampling = \"first\")"
  }

  keep        <- data[[col_subjid]] %in% chosen
  sample_data <- if (data.table::is.data.table(data)) data[keep] else data[keep, , drop = FALSE]
  n_rows      <- sum(keep)

  # --- Small display helpers.
  big <- function(x) formatC(as.integer(x), big.mark = ",", format = "d")
  pct <- function(k, tot) if (tot > 0) sprintf(" (%s%%)", .gc_pct(k, tot)) else ""
  cap_list <- function(x, k = 12L) {
    x <- unique(x[!is.na(x)])
    if (length(x) == 0L) return("(none)")
    if (length(x) > k) return(paste0(paste(x[seq_len(k)], collapse = ", "), sprintf(", ... (%d more)", length(x) - k)))
    paste(x, collapse = ", ")
  }

  # --- Config echo (available even if the run errors below).
  input_format <- if (is.null(dots[["input_format"]])) "long" else dots[["input_format"]]
  col_sex      <- if (is.null(dots[["sex"]]))     "sex"     else dots[["sex"]]
  col_agedays  <- if (is.null(dots[["agedays"]])) "agedays" else dots[["agedays"]]
  col_param    <- if (is.null(dots[["param"]]))   "param"   else dots[["param"]]
  has_dates    <- !is.null(dots[["dob"]]) && !is.null(dots[["visit_date"]]) &&
                  !identical(dots[["age_source"]], "agedays")
  is_wide      <- identical(input_format, "wide")
  age_source   <- if (has_dates) {
    if (is_wide) sprintf("date columns (dob = '%s_N', visit_date = '%s_N')", dots[["dob"]], dots[["visit_date"]])
    else         sprintf("dates (dob = '%s', visit_date = '%s')", dots[["dob"]], dots[["visit_date"]])
  } else if (is_wide) {
    sprintf("agedays_N columns (e.g. '%s_1')", col_agedays)
  } else {
    sprintf("agedays column ('%s')", col_agedays)
  }
  if (identical(input_format, "long")) {
    col_measurement <- if (is.null(dots[["measurement"]])) "measurement" else dots[["measurement"]]
    cols_line <- sprintf("subjid = '%s', param = '%s', sex = '%s', measurement = '%s'",
                         col_subjid, col_param, col_sex, col_measurement)
  } else {
    cols_line <- sprintf("subjid = '%s', sex = '%s'", col_subjid, col_sex)
  }

  # --- SEX mapping block (key column; derived from config + data, so it prints in
  #     both the success and error paths). Rows: what maps to male/female, then any
  #     present-but-unrecognized raw values, then (on success) the counts.
  eff_sex_coding <- if (is.null(dots[["sex_coding"]])) .gc_default_sex_coding else dots[["sex_coding"]]
  male_tok <- female_tok <- character(0)
  male_lbl <- female_lbl <- "(could not read sex_coding)"
  tryCatch({
    male_tok   <- .gc_norm(eff_sex_coding$male)
    female_tok <- .gc_norm(eff_sex_coding$female)
    male_lbl   <- paste(as.character(eff_sex_coding$male),   collapse = ", ")
    female_lbl <- paste(as.character(eff_sex_coding$female), collapse = ", ")
  }, error = function(e) NULL)
  sex_present_bad <- character(0)
  if (col_sex %in% names(sample_data)) {
    rs  <- sample_data[[col_sex]]
    ns  <- .gc_norm(rs)
    bad <- !is.na(ns) & nzchar(ns) & !(ns %in% c(male_tok, female_tok))
    sex_present_bad <- unique(as.character(rs)[bad])
  }
  sex_sec <- c(
    "",
    "SEX  [Key column: recommend reviewing this mapping to ensure it is correct]",
    sprintf("  Male   <- %s", male_lbl),
    sprintf("  Female <- %s", female_lbl),
    sprintf("  Unrecognized values in the '%s' column : %s", col_sex, cap_list(sex_present_bad))
  )

  # --- PARAMETER mapping block (key column).
  canon  <- .gc_recognized_params
  grp_of <- vapply(canon, .gc_param_group, character(1))
  wt_disp <- canon[grp_of == "weight"]
  ht_disp <- canon[grp_of == "height"]
  hc_disp <- canon[grp_of == "hc"]
  al <- dots[["param_coding"]]
  if (!is.null(al) && is.list(al)) {
    for (t in names(al)) {
      g <- .gc_param_group(t)
      if      (identical(g, "weight")) wt_disp <- c(wt_disp, as.character(al[[t]]))
      else if (identical(g, "height")) ht_disp <- c(ht_disp, as.character(al[[t]]))
      else if (identical(g, "hc"))     hc_disp <- c(hc_disp, as.character(al[[t]]))
    }
  }
  if (identical(input_format, "long")) {
    param_present_bad <- character(0)
    if (col_param %in% names(sample_data)) {
      rp        <- sample_data[[col_param]]
      npv       <- .gc_norm(rp)
      recog_tok <- tryCatch(names(.gc_param_coding_map(al)), error = function(e) .gc_norm(canon))
      bad       <- !is.na(npv) & nzchar(npv) & !(npv %in% recog_tok)
      param_present_bad <- unique(as.character(rp)[bad])
    }
    param_sec <- c(
      "",
      "PARAMETERS  [Key column: recommend reviewing this mapping to ensure it is correct]",
      sprintf("  Weight             <- %s", paste(wt_disp, collapse = ", ")),
      sprintf("  Height             <- %s", paste(ht_disp, collapse = ", ")),
      sprintf("  Head circumference <- %s", paste(hc_disp, collapse = ", ")),
      sprintf("  Unrecognized values in the '%s' column : %s", col_param, cap_list(param_present_bad))
    )
  } else {
    map_line <- if (identical(input_format, "semi_wide")) {
      if (is.null(dots[["semiwide_cols"]])) "auto (value columns named as growthcleanr parameters)"
      else paste(sprintf("%s <- column '%s'", names(dots[["semiwide_cols"]]), unlist(dots[["semiwide_cols"]])), collapse = "; ")
    } else {  # wide
      if (is.null(dots[["wide_cols"]])) "auto (columns named <PARAMETER>_N, e.g. WEIGHTKG_1)"
      else paste(sprintf("%s <- columns '%s_N' (e.g. '%s_1')", names(dots[["wide_cols"]]), unlist(dots[["wide_cols"]]), unlist(dots[["wide_cols"]])), collapse = "; ")
    }
    param_sec <- c(
      "",
      "PARAMETERS  [Key columns: recommend reviewing this mapping to ensure it is correct]",
      sprintf("  Parameter <- column : %s", map_line),
      "  (Parameter identity comes from columns in this format, not from a param column)"
    )
  }

  # --- Run cleangrowth() on the sample. Force long output + internal detail so
  #     the report is well defined; capture warnings; turn a hard stop into a
  #     reported finding rather than an error.
  run_dots <- dots
  run_dots$data            <- NULL
  run_dots$output_format   <- "long"
  run_dots$internal_detail <- TRUE
  run_dots$quietly         <- TRUE

  # A trial must not use up cleangrowth()'s once-per-session notices (e.g. "no
  # id supplied"): the trial reports them, and the full run that follows should
  # show them too. Snapshot the session flags and restore them after the call.
  .sess_before <- as.list(.gc_session, all.names = TRUE)
  on.exit({
    .gc_reset_session_notices()
    list2env(.sess_before, envir = .gc_session)
  }, add = TRUE)

  warns <- character(0)
  run <- tryCatch(
    withCallingHandlers(
      do.call(cleangrowth, c(list(data = sample_data), run_dots)),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      # cleangrowth() prints an always-on cleaning summary via message(); muffle
      # it here so it does not interleave with this function's own report (which
      # already reports the cleaning result in "CLEANING RESULT").
      message = function(m) invokeRestart("muffleMessage")
    ),
    error = function(e) structure(list(
      message = conditionMessage(e),
      # Report only the function name: cleangrowth() is called via do.call(),
      # so deparsing the whole call would inline the function and the data.
      call    = tryCatch({
        fn <- conditionCall(e)[[1L]]
        if (is.name(fn)) as.character(fn) else NA_character_
      }, error = function(.) NA_character_)
    ), class = "gc_trial_error")
  )
  run_error      <- if (inherits(run, "gc_trial_error")) run$message else NULL
  run_error_call <- if (inherits(run, "gc_trial_error")) run$call    else NULL

  # --- Header + "how the input was read".
  L <- c(
    "=== growthcleanr trial run ===",
    "",
    if (is.null(run_error))
      "  *** TRIAL RUN SUCCESSFUL *** -- growthcleanr cleaned the sample; review the details below."
    else
      "  *** TRIAL RUN FAILED *** -- growthcleanr could not clean the sample; see COULD NOT RUN below.",
    "",
    "SAMPLE",
    sprintf("  Patients sampled : %s of %s", big(n_use), big(M)),
    sprintf("  Sampling         : %s%s", sampling,
            if (identical(sampling, "random")) sprintf(" (seed: %s)", seed_report) else ""),
    sprintf("  Rows in sample   : %s", big(n_rows)),
    "",
    "HOW YOUR INPUT WAS READ",
    sprintf("  Input format     : %s", input_format),
    sprintf("  Columns          : %s", cols_line),
    sprintf("  Age source       : %s", age_source)
  )

  diag <- list(
    n_sampled = n_use, n_total = M, sampling = sampling, seed = seed_report,
    n_rows = n_rows, input_format = input_format, age_source = age_source,
    sex_map = list(male = male_lbl, female = female_lbl, present_unrecognized = sex_present_bad),
    warnings = warns, error = run_error
  )

  if (is.null(run_error)) {
    res <- run
    gis <- if ("gc_internal_sex" %in% names(res)) res$gc_internal_sex else rep(NA, nrow(res))
    gip <- if ("gc_internal_param" %in% names(res)) res$gc_internal_param else rep(NA_character_, nrow(res))
    n_male   <- sum(gis == 0, na.rm = TRUE)
    n_female <- sum(gis == 1, na.rm = TRUE)
    n_sexbad <- sum(is.na(gis))
    n_wt <- sum(gip == "WEIGHTKG", na.rm = TRUE)
    n_ht <- sum(gip == "HEIGHTCM", na.rm = TRUE)
    n_hc <- sum(gip == "HEADCM",   na.rm = TRUE)
    n_parbad <- sum(!(gip %in% c("WEIGHTKG", "HEIGHTCM", "HEADCM")) | is.na(gip))
    # Age comes back under the user's column name (or prefix, for wide input);
    # ages computed from dates come back as agedays.
    age_col <- intersect(c("gc_internal_agedays", col_agedays, "agedays"), names(res))[1L]
    ar  <- suppressWarnings(range(as.numeric(res[[age_col]]), na.rm = TRUE))
    exc <- sort(table(as.character(res$exclude)), decreasing = TRUE)
    tot <- nrow(res)

    sex_sec   <- c(sex_sec,   sprintf("  Count of measurements : %s male, %s female, %s unrecognized",
                                      big(n_male), big(n_female), big(n_sexbad)))
    param_sec <- c(param_sec, sprintf("  Count of measurements : %s weight, %s height, %s head circumference; %s unrecognized",
                                      big(n_wt), big(n_ht), big(n_hc), big(n_parbad)))

    age_line <- sprintf("  Age range        : %s to %s days", big(ar[1]), big(ar[2]))
    tail_sec <- c(
      "",
      "FLAGGED WHILE READING",
      if (length(warns) == 0L) "  (Nothing flagged)" else paste0("  - ", warns),
      "",
      sprintf("CLEANING RESULT (%s measurements in sample)", big(tot))
    )
    for (nm in names(exc)) {
      tail_sec <- c(tail_sec, sprintf("  %-28s %s%s", nm, big(exc[[nm]]), pct(exc[[nm]], tot)))
    }

    diag$sex        <- c(male = n_male, female = n_female, unrecognized = n_sexbad)
    diag$parameters <- c(weight = n_wt, height = n_ht, head_circumference = n_hc, unrecognized = n_parbad)
    diag$age_range  <- ar
    diag$exclusions <- exc
    out <- data.table::as.data.table(res)

    # Age range belongs with "how the input was read"; then the sex/param blocks.
    L <- c(L, age_line, sex_sec, param_sec, tail_sec)
  } else {
    tail_sec <- c(
      "",
      "FLAGGED WHILE READING",
      if (length(warns) == 0L) "  (Nothing flagged before the stop)" else paste0("  - ", warns),
      "",
      "COULD NOT RUN",
      "  growthcleanr stopped with this error:",
      paste0("    ", run_error),
      if (!is.null(run_error_call) && !is.na(run_error_call))
        paste0("    (in: ", run_error_call, ")") else NULL,
      "  Often this is a format or a column/parameter setting to fix above, then retry;",
      "  if it does not look like a settings problem, it may be a growthcleanr bug worth",
      "  reporting (include the error message and location above)."
    )
    out <- data.table::data.table()
    L <- c(L, sex_sec, param_sec, tail_sec)
  }

  L <- c(L,
    "",
    if (is.null(run_error)) "  *** TRIAL RUN SUCCESSFUL ***"
    else                    "  *** TRIAL RUN FAILED -- see COULD NOT RUN above ***",
    "")
  diag$report <- L
  cat(paste(L, collapse = "\n"), "\n", sep = "")

  data.table::setattr(out, "gc_trial", diag)
  invisible(out)
}
