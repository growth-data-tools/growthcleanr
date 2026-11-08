################################################################################
# GROWTHCLEANR CHILD ALGORITHM - R IMPLEMENTATION
################################################################################
#
# PURPOSE:
#   Identifies and flags implausible pediatric growth measurements (weight,
#   height, head circumference) from electronic health records for children
#   ages 0-20 years.
#
#

################################################################################
# CITATION
################################################################################
#
# If using this algorithm, please cite:
#   Daymont C, et al. (2017). Automated identification of implausible values
#   in growth data from pediatric electronic health records. JAMIA, 24(6):1080-1087.
#
################################################################################

################################################################################
# ALGORITHM STRUCTURE
################################################################################
#
# Preprocessing (handled in cleangrowth() before dispatch to cleanchild()):
#   - Input validation, internal_id assignment, imperial->metric conversion
#   - Z-score calculation (WHO/CDC blend via CSD method)
#   - Step 2b: Gestational age correction for potcorr (preterm) subjects
#   - Recentering -> tbc.sd and ctbc.sd
#   - Exclude-Missing and Exclude-Not-Cleaned assignment
#
# Algorithm steps (within cleanchild()):
#   Early 13: SDE-Identicals (pre-CF identical-value removal)
#   STEP 5:   Temporary same-day extraneous (SDE) flagging
#   STEP 6:   Carried-forward identification + optional rescue
#   STEP 7:   Population implausible values (PIV)
#   STEP 9:   Evil Twins (adjacent extreme values)
#   STEP 11:  EWMA1 - extreme EWMA outliers
#   STEP 13:  Final SDE resolution
#   STEP 15:  EWMA2 - moderate EWMA outliers
#   STEP 16:  Birth HT/HC EWMA2 variant
#   STEP 17:  Height/HC velocity checks
#   STEP 19:  Pairs and singles evaluation
#   STEP 21:  Error load assessment
#   STEP 22:  Output assembly
#
# Note: Step numbers are not consecutive. Steps 1-4, 8, 10, 12, 14, 18, 20 do
# not exist as top-level steps in the algorithm.
#
# KEY CONCEPTS:
#   - SDE (Same-Day Extraneous): Multiple measurements of same parameter on
#     same day. Algorithm selects most plausible value.
#   - Carried Forward: Identical consecutive values, likely copied from prior
#     visit rather than independently measured.
#   - EWMA (Exponentially Weighted Moving Average): Uses weighted average of
#     surrounding measurements to identify outliers, giving more weight to
#     temporally closer values.
#   - tbc.sd (To-Be-Cleaned SD score): Recentered z-score used throughout
#   - DOP (Designated Other Parameter): For weight->height, for height->weight, for HC->height
#   - Evil Twins: Multiple consecutive extreme values that fooled earlier steps
#
################################################################################
# DATA STRUCTURE NOTES
################################################################################
#
# R IMPLEMENTATION SPECIFICS:
#   - Uses data.table for efficient processing of large datasets
#   - Parallel processing via foreach/doParallel when parallel=TRUE
#   - Subjects are split into batches for parallel computation
#   - Each batch processes independently, then results are combined
#
# SORT ORDER:
#   Critical for deterministic results. Primary sort:
#     data.table::setkey(data.df, subjid, param, agedays, internal_id)
#
#   The 'id' field is required and should be unique per row. For datasets
#   with multiple measurements on the same day, 'id' serves as a tiebreaker
#   to ensure consistent ordering across runs. If you know the timing of values
#   on individual days, you may wish to assign higher ids to later values.
#
################################################################################
# SUPPORTING FUNCTIONS
################################################################################
#
# This file contains the main cleanchild() function and supporting utilities:
#   - identify_temp_sde(): Temporary SDE resolution - winner selection on each
#       (subjid, param, agedays) group (Child Steps 5, 6, 7, 9, 11, 13)
#   - calc_otl_evil_twins(): Identifies Evil Twins (Step 9)
#   - calc_and_recenter_z_scores(): Recomputes recentered z-scores for the
#       p_plus / p_minus perturbation columns (Child Step 15/16 pre-loop)
#   - ewma(): EWMA computation (Child Steps 11, 13, 17)
#   - ewma_cache_init() / ewma_cache_update(): incremental EWMA cache for the
#       iterative moderate-EWMA loops (Child Steps 15, 16)
#   - .child_valid(): Identifies included/partially-included rows for each step
#   - .child_exc(): Generates Exclude-C-{Reason} codes
#   - get_dop(): Maps a parameter to its Designated Other Parameter
#   - read_anthro(): Builds z-score closures from reference tables
#   - gc_preload_refs(): Pre-loads reference closures for repeated calls
#
################################################################################
# USAGE NOTES
################################################################################
#
# 1. PARALLEL PROCESSING: Set parallel=TRUE (optionally cores=N) for large datasets.
#    Each batch processes independently, so results are deterministic.
#
# 2. MEMORY: Large datasets may require increasing R memory limits.
#    Use data.table operations for efficiency.
#
# 3. DEBUGGING: Results from intermediate steps can be captured by inserting
#    saveRDS() or write.csv() calls at key points.
#
# 4. DATA.TABLE MODIFICATION: Many operations modify data.df by reference
#    (using := operator) for efficiency. Be aware of this when debugging.
#

#
#
################################################################################
# INFORMATION ABOUT IDENTIFIERS AND PRECISION HANDLING
################################################################################
#
# A substantial amount of effort is made to handle floating point precision
# and to order by row id. This ensures consistent handling of a dataset
# across multiple runs.
#
# Row IDENTIFIERS are discussed further above.
#


#' Generate a child-algorithm exclusion code.
#'
#' Convenience wrapper that returns `paste0("Exclude-C-", suffix)`. The
#' `param_val` argument is accepted but ignored - child exclusion codes
#' are not param-specific (the row's `param` column carries that
#' information); it is accepted for call-site compatibility.
#' The function is vectorised over `suffix` (and `param_val`), so it
#' works inside data.table `j` expressions.
#'
#' @param param_val ignored. Kept for call-site compatibility.
#' @param suffix reason code (e.g. `"PIV"`, `"CF"`, `"Traj"`). Must
#'   correspond to a level in `exclude.levels` or downstream factor
#'   assignment will silently produce NA.
#'
#' @return Character scalar or vector: `"Exclude-C-"` prepended to
#'   `suffix`. E.g. `.child_exc("WEIGHTKG", "PIV")` returns
#'   `"Exclude-C-PIV"`; the first arg's value is irrelevant.
#'
#' @keywords internal
#' @noRd
.child_exc <- function(param_val, suffix) {
  paste0("Exclude-C-", suffix)
}

# --- Once-per-session notices -------------------------------------------------
# Package-internal state (reset when R restarts). Used for notices that help the
# first time but are noise when repeated in a loop: the "no id supplied" notice in
# the input warning and the vector-style note for code written for growthcleanr 2.x.
.gc_session <- new.env(parent = emptyenv())

#' TRUE the first time `key` is seen in this R session, FALSE afterwards
#' @noRd
.gc_first_in_session <- function(key) {
  if (isTRUE(.gc_session[[key]])) return(FALSE)
  assign(key, TRUE, envir = .gc_session)
  TRUE
}

#' Reset the once-per-session notices (used by tests)
#' @noRd
.gc_reset_session_notices <- function() {
  rm(list = ls(.gc_session, all.names = TRUE), envir = .gc_session)
  invisible(NULL)
}

#' One-time note for vector-style calls (code possibly written for 2.x)
#'
#' Emitted after the first vector-style cleangrowth() call in an R session. Not
#' gated by `quietly`: 2.2.3 defaulted to quietly = TRUE, so old scripts often
#' pass it, and they are the ones this note is for. suppressMessages() silences it.
#' @noRd
.gc_vector_style_note <- function() {
  if (!.gc_first_in_session("vector_style_note")) return(invisible(NULL))
  message(paste0(
    "Note: since growthcleanr version 3, cleangrowth() returns a data table (one row per input row), ",
    "not a vector of exclusion codes. Code written for 2.x that treats the result as a vector ",
    "(for example sum(result == \"Include\") or df$gcr_result <- result) can run without an error and ",
    "give wrong answers; use result$exclude instead. Exclusion codes were also renamed. See ",
    "'Updating code written for growthcleanr 2.2.3' in vignette(\"experienced-start-here\"). ",
    "(Shown once per R session.)"))
  invisible(NULL)
}

# --- Output exclusion-code maps ------------------------------------------------
#' Summary / Detailed exclusion-code crosswalks and factor levels
#' @return list(map_detailed, map_summary, levels_summary, levels_detailed)
#' @noRd
.gc_exclude_maps <- function() {
  # --- Output-level crosswalks (internal step codes -> Detailed / Summary) ---
  # The algorithm assigns the granular internal codes above. Output is reported
  # at one of two user-facing levels, applied as a final mapping in the output
  # assembly: Summary (default `exclude`) and Detailed (`exclude_detailed`,
  # opt-in via exclude_detail / full_detail). A third, fully granular "Internal"
  # level (the raw codes above, incl. RV/firstRV/allRV passes, -All variants,
  # and round iteration) is deferred and not exposed. Single source of truth:
  # vignettes/exclusion-codes.Rmd (vignette("exclusion-codes")).
  #
  # Detailed: keeps the child (-C-) / adult (-A-) prefix and step granularity;
  # folds away the repeated-value (RV/firstRV/allRV), -All, and round-iteration
  # variants. Shared "for all" codes carry no -C-/-A- prefix.
  exclude.map.detailed <- c(
    'Include'                                      = 'Include',
    'Exclude-Missing'                              = 'Exclude-Missing-Info',
    'Exclude-Not-Cleaned'                          = 'Exclude-HC-Out-of-Range',
    'Exclude-Not-GC-Param'                         = 'Exclude-Not-GC-Param',
    'Exclude-C-Temp-Same-Day'                      = 'Exclude-Extraneous',
    'Exclude-C-CF'                                 = 'Exclude-C-CF',
    'Exclude-C-PIV'                                = 'Exclude-C-Hard-Limit',
    'Exclude-C-Evil-Twins'                         = 'Exclude-C-Evil-Twins',
    'Exclude-C-Traj-Extreme'                       = 'Exclude-C-Traj-Extreme',
    'Exclude-C-Traj'                               = 'Exclude-C-Traj',
    'Exclude-C-Abs-Diff'                           = 'Exclude-C-Abs-Diff',
    'Exclude-C-Pair'                               = 'Exclude-C-Pair',
    'Exclude-C-Single'                             = 'Exclude-C-Single',
    'Exclude-C-Unevaluable-Trajectory'             = 'Exclude-C-Unevaluable-Trajectory',
    'Exclude-C-Identical'                          = 'Exclude-Identical',
    'Exclude-C-Extraneous'                         = 'Exclude-Extraneous',
    'Exclude-A-PIV'                                = 'Exclude-A-Hard-Limit',
    'Exclude-A-Scale-Max'                          = 'Exclude-A-Scale-Max',
    'Exclude-A-Scale-Max-Identical'                = 'Exclude-A-Scale-Max',
    'Exclude-A-Scale-Max-RV-Propagated'            = 'Exclude-A-Scale-Max',
    'Exclude-A-Evil-Twins'                         = 'Exclude-A-Evil-Twins',
    'Exclude-A-Identical'                          = 'Exclude-Identical',
    'Exclude-A-Extraneous'                         = 'Exclude-Extraneous',
    'Exclude-A-Traj-Extreme'                       = 'Exclude-A-Traj-Extreme',
    'Exclude-A-Traj-Extreme-firstRV'               = 'Exclude-A-Traj-Extreme',
    'Exclude-A-Traj-Extreme-allRV'                 = 'Exclude-A-Traj-Extreme',
    'Exclude-A-Traj-Extreme-firstRV-RV-Propagated' = 'Exclude-A-Traj-Extreme',
    'Exclude-A-Ord-Pair'                           = 'Exclude-A-Ord-Pair',
    'Exclude-A-Ord-Pair-All'                       = 'Exclude-A-Ord-Pair',
    'Exclude-A-Window'                             = 'Exclude-A-Window',
    'Exclude-A-Window-All'                         = 'Exclude-A-Window',
    'Exclude-A-2D-Ordered'                         = 'Exclude-A-2D-Ordered',
    'Exclude-A-2D-Non-Ordered'                     = 'Exclude-A-2D-Non-Ordered',
    'Exclude-A-Traj-Moderate'                      = 'Exclude-A-Traj-Moderate',
    'Exclude-A-Traj-Moderate-allRV'                = 'Exclude-A-Traj-Moderate',
    'Exclude-A-Traj-Moderate-RV-Propagated'        = 'Exclude-A-Traj-Moderate',
    'Exclude-A-Traj-Moderate-Error-Load'           = 'Exclude-A-Traj-Moderate-Error-Load',
    'Exclude-A-Traj-Moderate-Error-Load-RV'        = 'Exclude-A-Traj-Moderate-Error-Load',
    'Exclude-A-Single'                             = 'Exclude-A-Single',
    'Exclude-A-Unevaluable-Trajectory'             = 'Exclude-A-Unevaluable-Trajectory'
  )
  # Summary: consolidated across steps AND across child/adult. Default `exclude`.
  exclude.map.summary <- c(
    'Include'                                      = 'Include',
    'Exclude-Missing'                              = 'Exclude-Missing-Info',
    'Exclude-Not-Cleaned'                          = 'Exclude-HC-Out-of-Range',
    'Exclude-Not-GC-Param'                         = 'Exclude-Not-GC-Param',
    'Exclude-C-Temp-Same-Day'                      = 'Exclude-Extraneous',
    'Exclude-C-CF'                                 = 'Exclude-CF',
    'Exclude-C-PIV'                                = 'Exclude-Hard-Limit',
    'Exclude-C-Evil-Twins'                         = 'Exclude-Extreme',
    'Exclude-C-Traj-Extreme'                       = 'Exclude-Extreme',
    'Exclude-C-Traj'                               = 'Exclude-Pattern',
    'Exclude-C-Abs-Diff'                           = 'Exclude-Pattern',
    'Exclude-C-Pair'                               = 'Exclude-Pair',
    'Exclude-C-Single'                             = 'Exclude-Single',
    'Exclude-C-Unevaluable-Trajectory'             = 'Exclude-Traj-Uneval',
    'Exclude-C-Identical'                          = 'Exclude-Identical',
    'Exclude-C-Extraneous'                         = 'Exclude-Extraneous',
    'Exclude-A-PIV'                                = 'Exclude-Hard-Limit',
    'Exclude-A-Scale-Max'                          = 'Exclude-Scale-Max',
    'Exclude-A-Scale-Max-Identical'                = 'Exclude-Scale-Max',
    'Exclude-A-Scale-Max-RV-Propagated'            = 'Exclude-Scale-Max',
    'Exclude-A-Evil-Twins'                         = 'Exclude-Extreme',
    'Exclude-A-Identical'                          = 'Exclude-Identical',
    'Exclude-A-Extraneous'                         = 'Exclude-Extraneous',
    'Exclude-A-Traj-Extreme'                       = 'Exclude-Extreme',
    'Exclude-A-Traj-Extreme-firstRV'               = 'Exclude-Extreme',
    'Exclude-A-Traj-Extreme-allRV'                 = 'Exclude-Extreme',
    'Exclude-A-Traj-Extreme-firstRV-RV-Propagated' = 'Exclude-Extreme',
    'Exclude-A-Ord-Pair'                           = 'Exclude-Pattern',
    'Exclude-A-Ord-Pair-All'                       = 'Exclude-Pattern',
    'Exclude-A-Window'                             = 'Exclude-Pattern',
    'Exclude-A-Window-All'                         = 'Exclude-Pattern',
    'Exclude-A-2D-Ordered'                         = 'Exclude-Pattern',
    'Exclude-A-2D-Non-Ordered'                     = 'Exclude-Pattern',
    'Exclude-A-Traj-Moderate'                      = 'Exclude-Pattern',
    'Exclude-A-Traj-Moderate-allRV'                = 'Exclude-Pattern',
    'Exclude-A-Traj-Moderate-RV-Propagated'        = 'Exclude-Pattern',
    'Exclude-A-Traj-Moderate-Error-Load'           = 'Exclude-Pattern',
    'Exclude-A-Traj-Moderate-Error-Load-RV'        = 'Exclude-Pattern',
    'Exclude-A-Single'                             = 'Exclude-Single',
    'Exclude-A-Unevaluable-Trajectory'             = 'Exclude-Traj-Uneval'
  )
  # Factor levels for each output level (order: Include, shared, then exclusions).
  exclude.levels.summary <- c(
    'Include', 'Exclude-Not-GC-Param', 'Exclude-Missing-Info', 'Exclude-HC-Out-of-Range',
    'Exclude-Identical', 'Exclude-Extraneous',
    'Exclude-Hard-Limit', 'Exclude-CF', 'Exclude-Scale-Max', 'Exclude-Extreme', 'Exclude-Pattern',
    'Exclude-Pair', 'Exclude-Single', 'Exclude-Traj-Uneval'
  )
  exclude.levels.detailed <- unique(unname(exclude.map.detailed))
  list(map_detailed = exclude.map.detailed, map_summary = exclude.map.summary,
       levels_summary = exclude.levels.summary, levels_detailed = exclude.levels.detailed)
}

# --- Removed 2.x arguments -----------------------------------------------------
# cleangrowth() arguments present in growthcleanr 2.2.3 and removed in version 3,
# each with what to do instead. Used by .gc_check_dots() so an old call stops with
# a pointer rather than R's bare "unused argument".
.gc_removed_args <- c(
  recover.unit.error     = "unit-error recovery was removed from the algorithm; no option is needed.",
  sd.extreme             = "the extreme z-score cutoff was replaced by the hard-limit step (Child Step 7); its limits are set with the piv.z.* arguments.",
  z.extreme              = "the extreme z-score cutoff was replaced by the hard-limit step (Child Step 7); its limits are set with the piv.z.* arguments.",
  lt3.exclude.mode       = "handling of subjects with few measurements is built in; no option is needed.",
  height.tolerance.cm    = "the height-decrease tolerance is built in; no option is needed.",
  sdmedian.filename      = "file output was removed; use recenter_detail = TRUE to return the recentering values as columns.",
  sdrecentered.filename  = "file output was removed; use recenter_detail = TRUE to return the recentering values as columns.",
  include.carryforward   = "use cf_rescue instead (\"none\" is closest to include.carryforward = FALSE, the 2.2.3 default; \"all\" is closest to TRUE; the default is now \"standard\").",
  ewma.exp               = "the EWMA exponent is fixed internally; no option is needed.",
  num.batches            = "use cores instead (with parallel = TRUE).",
  weight_cap             = "use adult_scale_max_lbs instead (in pounds, not kg).",
  adult_columns_filename = "file output was removed; the adult mean height is returned in the mean_ht column.",
  prelim_infants         = "infant handling is now built in; no option is needed."
)

#' Stop on arguments cleangrowth() does not have
#'
#' Removed 2.x argument names stop with what to use instead; any other unknown
#' argument stops with R's usual "unused argument" wording, so typos are still
#' caught even though cleangrowth() has `...`.
#' @param dots List of unevaluated argument expressions from `...`.
#' @param sd.recenter The `sd.recenter` value, so a removed 2.x character option
#'   ("nhanes"/"derive", any case) is reported in the same message as removed
#'   arguments rather than only on the next run.
#' @noRd
.gc_check_dots <- function(dots, sd.recenter = NA) {
  nms <- names(dots)
  if (is.null(nms)) nms <- rep("", length(dots))
  removed <- nms[nms %in% names(.gc_removed_args)]
  if (length(removed) > 0L) {
    rc_line <- if (is.character(sd.recenter) && length(sd.recenter) == 1L &&
                   !is.na(sd.recenter) &&
                   tolower(trimws(sd.recenter)) %in% c("nhanes", "derive"))
      sprintf("\n`sd.recenter = \"%s\"` was removed in growthcleanr version 3; the built-in reference is now the default, so omit sd.recenter to use it, or set sd.recenter = \"none\" for no recentering.", sd.recenter)
    else ""
    stop(paste0(
      paste(sprintf("`%s` was removed in growthcleanr version 3; %s", removed,
                    .gc_removed_args[removed]), collapse = "\n"),
      rc_line,
      "\nSee 'Removed and renamed arguments' in vignette(\"experienced-start-here\")."),
      call. = FALSE)
  }
  if (length(dots) == 0L) return(invisible(NULL))
  shown <- vapply(seq_along(dots), function(i) {
    val <- paste(deparse(dots[[i]], width.cutoff = 60L, nlines = 1L), collapse = " ")
    if (nchar(val) > 60L) val <- paste0(substr(val, 1L, 57L), "...")
    if (nzchar(nms[i])) paste(nms[i], "=", val) else val
  }, character(1))
  stop(sprintf("unused argument%s in cleangrowth(): %s",
               if (length(dots) > 1L) "s" else "", paste(shown, collapse = ", ")),
       call. = FALSE)
}

# --- Consolidated input-warning helpers (Option B) --------------------------
# Detection, id-notice text, and warning assembly live here as one implementation
# so both the standalone (non-checkpoint) preprocessing path and the checkpoint
# outer branch produce the identical consolidated warning. The detector runs once
# on the whole dataset (raw input vectors), so denominators are dataset-wide by
# construction -- there is no per-checkpoint fragmentation. See child_clean.R's
# preprocessing flush and the checkpoint branch for the two call sites.

#' Text of the "Generated ids" warning section
#' @noRd
.gc_generated_ids_section <- function(adjusted) {
  c("Generated ids",
    adjusted,
    paste0("growthcleanr uses an `id` to break ties deterministically when measurements are equally ",
           "plausible (e.g. some extraneous-value selection): it prefers the higher `id` (a same-day ",
           "remeasurement may be more accurate), except at birth (agedays 0), where it prefers the ",
           "lowest. Supply your own `id` for exact reproducibility across datasets and control over ",
           "these selections."))
}

#' Format a percentage for user-facing messages: one decimal, but a nonzero
#' share that would round to 0.0 is shown as "<0.1" rather than "0.0".
#' @noRd
.gc_pct <- function(k, d) {
  if (d <= 0) return("")
  v <- 100 * k / d
  if (k > 0 && v < 0.05) "<0.1" else sprintf("%.1f", v)
}

#' Assemble the consolidated input warning (returns character(0) if nothing)
#'
#' @param setaside Character vector of set-aside bullets (from
#'   \code{.gc_detect_setaside}).
#' @param setaside_lines Integer input-row indices set aside (union across
#'   bullets); the lead line counts the distinct rows.
#' @param adjusted Character vector of "adjusted automatically" notes (id
#'   auto-regen), if any.
#' @param n_input Total measurement count (dataset-wide denominator).
#' @return Character vector of message lines (the caller \code{paste(collapse="\\n")}s
#'   and \code{warning()}s it), or \code{character(0)} when there is nothing to say.
#' @noRd
.gc_format_input_warning <- function(setaside, setaside_lines, adjusted, n_input) {
  sep  <- strrep("*", 60)
  wmsg <- character(0)
  if (length(setaside) > 0L) {
    n_sa <- length(unique(setaside_lines))
    wmsg <- c(wmsg,
      "Unusable data",
      sprintf("%s of %s measurement(s) (%s%%) could not be used as submitted and were set aside. This is often expected; the details below explain each problem and suggest a fix if you want one:",
              format(n_sa, big.mark = ",", trim = TRUE),
              format(n_input, big.mark = ",", trim = TRUE), .gc_pct(n_sa, n_input)),
      paste0("  - ", setaside))
  }
  # The "no id supplied" notice is shown once per R session: it repeats
  # identically for every call in a loop. Notices about NA or duplicated ids
  # describe this dataset and are shown every run, as is "Unusable data".
  no_id <- grepl("^No `id` was supplied", adjusted)
  if (any(no_id) && !.gc_first_in_session("no_id_notice"))
    adjusted <- adjusted[!no_id]
  if (length(adjusted) > 0L) {
    if (length(wmsg) > 0L) wmsg <- c(wmsg, sep)   # separator between sections
    wmsg <- c(wmsg, .gc_generated_ids_section(adjusted))
  }
  wmsg
}

#' Build the always-on cleaning summary block (character vector of lines)
#'
#' A short headline of what growthcleanr did: per metric parameter group
#' (weight, then height, then head circumference) how many measurements were
#' included (kept as \code{Include}) vs. excluded (flagged implausible by the
#' algorithm), then an overall count of measurements set aside as unusable
#' ("Set aside"; never cleaned). Per-parameter percentages are of that
#' parameter's included+excluded total (the rows that were cleaned); the "Set
#' aside" percentage is of all submitted measurements. Bounded by \code{****}
#' separators.
#'
#' @param group Character vector, one per output row: "weight"/"height"/"hc"/NA
#'   (from \code{.gc_param_group} on the internal param).
#' @param excl_chr Character vector of final Summary exclude codes ("Include" or
#'   an \code{Exclude-*} code), one per output row.
#' @param notcleaned Logical vector, TRUE where the row was set aside as unusable
#'   input (the union of \code{.gc_detect_setaside} rows).
#' @param n_input Total submitted measurement count.
#' @return Character vector of message lines (caller \code{paste(collapse="\\n")}s
#'   and \code{message()}s it).
#' @noRd
.gc_cleaning_summary_lines <- function(group, excl_chr, notcleaned, n_input) {
  sep  <- strrep("*", 60)
  big  <- function(x) format(x, big.mark = ",", trim = TRUE)
  pctp <- function(k, d) if (d > 0L) sprintf(" (%s%%)", .gc_pct(k, d)) else ""
  # A row with no result (exclude NA -- e.g. a resumed checkpoint run whose input
  # changed) is counted on its own line so the per-param counts still add up.
  noresult <- !notcleaned & is.na(excl_chr)
  entered <- !notcleaned & !noresult
  lines <- c(sep, sprintf("growthcleanr cleaning summary  (%s measurement%s)",
                          big(n_input), if (identical(n_input, 1L)) "" else "s"))
  labs <- c(weight = "Weight", height = "Height", hc = "Head circ.")
  for (g in c("weight", "height", "hc")) {
    gi   <- entered & !is.na(group) & group == g
    totg <- sum(gi)
    if (totg == 0L) next
    ncl <- sum(gi & excl_chr == "Include")
    nex <- totg - ncl
    lines <- c(lines, sprintf("  %-11s : %s included%s, %s excluded%s",
                              labs[[g]], big(ncl), pctp(ncl, totg),
                              big(nex), pctp(nex, totg)))
  }
  n_nc <- sum(notcleaned)
  lines <- c(lines,
             sprintf("  %-11s : %s%s", "Set aside", big(n_nc), pctp(n_nc, n_input)))
  if (n_nc > 0L)
    lines <- c(lines, "    (unusable as submitted -- see the warning below for details)")
  n_nr <- sum(noresult)
  if (n_nr > 0L)
    lines <- c(lines,
               sprintf("  %-11s : %s%s", "No result", big(n_nr), pctp(n_nr, n_input)),
               "    (not found in the checkpoint files -- see the warning below)")
  c(lines, sep)
}

#' Detect input rows growthcleanr sets aside, dataset-wide
#'
#' Reproduces the staged input triage the preprocessing pipeline performs --
#' blank required fields, then (dropping missing-key rows) unrecognized params,
#' then (dropping non-gc-param rows) unrecognized pediatric sex, zero/negative
#' measurements, and negative age -- but as a pure detector: it removes no rows,
#' throws no stops, and prints no messages; it only builds the warning bullets.
#' Runs on the raw input vectors so it can be called once on the whole dataset
#' from either the standalone flush or the checkpoint outer branch, giving
#' dataset-wide denominators. The example "subjid" in each bullet is the smallest
#' factor-level subjid among the affected rows, which equals the first row's
#' subjid after the algorithm's \code{setkey(subjid, param, agedays, id)} sort --
#' reproduced here without a full sort.
#'
#' @param subjid,param,agedays,sex,measurement Raw input vectors (as submitted;
#'   \code{measurement} already coerced to numeric upstream).
#' @param param_coding,sex_coding Alias/coding tables, as passed to
#'   \code{cleangrowth}.
#' @param adult_cutpoint The adult cutpoint (already clamped to 18-20 by the
#'   caller in the non-checkpoint path; the raw value in the checkpoint branch).
#'   Used only to place the pediatric/adult boundary for set-aside detection;
#'   growthcleanr does not auto-lower it based on the data.
#' @param n_input Total measurement count (dataset-wide denominator).
#' @param dob_conflict Optional logical vector (input-row order) marking rows of
#'   subjects with more than one distinct dob; reported in their own bullet.
#' @param date_setaside Optional list (token, token_vals, bad, bad_vals) of date
#'   values treated as blank (no digits) or unreadable; reported in their own
#'   bullets.
#' @return \code{list(bullets = character, lines = integer, cutpoint = numeric)}
#'   where \code{cutpoint} is the clamped adult_cutpoint this detection used --
#'   the checkpoint branch forwards it to each checkpoint so a checkpointed run applies the
#'   same cutpoint a single run would.
#' @noRd
.gc_detect_setaside <- function(subjid, param, agedays, sex, measurement,
                                param_coding, sex_coding, adult_cutpoint,
                                n_input, dob_conflict = NULL,
                                date_setaside = NULL) {
  bullets <- character(0)
  lines   <- integer(0)

  subj_fac <- as.factor(subjid)
  # Example subjid = smallest factor level present among masked rows; equals the
  # first row's subjid under the setkey(subjid, param, agedays, id) sort.
  ex_subj <- function(mask) levels(subj_fac)[min(as.integer(subj_fac)[mask])]

  # Stage 0a: subjects with more than one distinct dob (date route). Their
  # agedays were set to NA upstream; report them here rather than as blank ages.
  dobc <- if (is.null(dob_conflict)) rep(FALSE, length(subjid)) else dob_conflict
  if (any(dobc)) {
    n_subj_dobc <- length(unique(as.integer(subj_fac)[dobc]))
    bullets <- c(bullets, sprintf(
      "Conflicting dates of birth -- %d measurement(s) (%s%%) from %d subject(s) with more than one distinct dob value (e.g. subjid %s). Ages cannot be computed reliably (the rows may combine more than one person), so all of these subjects' rows were set aside as Exclude-Missing-Info. Check the dob values for these subjects.",
      sum(dobc), .gc_pct(sum(dobc), n_input), n_subj_dobc, ex_subj(dobc)))
    lines <- union(lines, which(dobc))
  }

  # Stage 0b: date values treated as blank (no digits) or unreadable, which left
  # the row with no age. Reported here rather than as blank ages.
  datesa <- rep(FALSE, length(subjid))
  if (!is.null(date_setaside)) {
    .egv <- function(v) paste0(paste0('"', utils::head(v, 3L), '"', collapse = ", "),
                               if (length(v) > 3L) ", ..." else "")
    tokm <- date_setaside$token & is.na(agedays) & !dobc
    badm <- date_setaside$bad & is.na(agedays) & !dobc & !tokm
    if (any(tokm)) {
      bullets <- c(bullets, sprintf(
        "Dates with no digits -- %d measurement(s) (%s%%) have a dob or visit_date value with no digits (e.g. %s), which growthcleanr treated as blank, so these rows have no age and were set aside as Exclude-Missing-Info. Supply the dates if available.",
        sum(tokm), .gc_pct(sum(tokm), n_input), .egv(date_setaside$token_vals)))
    }
    if (any(badm)) {
      bullets <- c(bullets, sprintf(
        "Unreadable dates -- %d measurement(s) (%s%%) have a dob or visit_date value that is not a real date (e.g. %s), so these rows have no age and were set aside as Exclude-Missing-Info. Correct the dates if possible.",
        sum(badm), .gc_pct(sum(badm), n_input), .egv(date_setaside$bad_vals)))
    }
    datesa <- tokm | badm
    lines <- union(lines, which(datesa))
  }

  # Stage 0: blank required fields (raw NA), denominator = whole input.
  # (Ages set to NA for a dob conflict are reported in Stage 0a, not here.)
  warn_cols <- c("subjid", "param", "agedays", "measurement")
  age_blank <- is.na(agedays) & !dobc & !datesa
  n_na <- c(subjid      = sum(is.na(subjid)),  param       = sum(is.na(param)),
            agedays     = sum(age_blank), measurement = sum(is.na(measurement)))
  if (any(n_na > 0L)) {
    miss_raw <- is.na(subjid) | is.na(param) | age_blank | is.na(measurement)
    bullets <- c(bullets, sprintf(
      "Blank required fields -- %d measurement(s) (%s%%): %s. These cells are empty (NA), so the rows cannot be cleaned and were set aside. Supply the values if available.",
      sum(miss_raw), .gc_pct(sum(miss_raw), n_input),
      paste(sprintf("%s (%d)", warn_cols[n_na > 0L], n_na[n_na > 0L]), collapse = ", ")))
    lines <- union(lines, which(miss_raw))
  }

  param_norm <- .gc_recode_param(param, param_coding)
  sex_int    <- .gc_recode_sex(sex, sex_coding)

  # `avail` tracks the rows the algorithm still holds after each up-front set-aside
  # (missing-key, then non-gc-param), so later denominators/examples match.
  avail <- !(is.na(subjid) | is.na(param_norm))   # drop missing-key rows

  # Stage 1: unrecognized params (resolved param not one growthcleanr handles).
  gc_params <- .gc_recognized_params
  ng <- avail & !(param_norm %in% gc_params)
  if (any(ng)) {
    ng_tab   <- sort(table(as.character(param)[ng]), decreasing = TRUE)
    ng_show  <- utils::head(ng_tab, 8L)
    ng_nsubj <- length(unique(as.integer(subj_fac)[ng]))
    bullets <- c(bullets, sprintf(
      "Unrecognized parameters -- %d measurement(s) (%s%%) across %d subject(s) have a param growthcleanr does not recognize (e.g. subjid %s, values %s%s); set aside as Exclude-Not-GC-Param, measurement preserved. If any are mislabeled height/weight, map them instead of relabeling your data: param_coding = list(WEIGHTKG = ..., HEIGHTCM = ..., ...).",
      sum(ng), .gc_pct(sum(ng), n_input), ng_nsubj, ex_subj(ng),
      paste(sprintf("%s (%d)", names(ng_show), as.integer(ng_show)), collapse = ", "),
      if (length(ng_tab) > length(ng_show)) ", ..." else ""))
    lines <- union(lines, which(ng))
  }
  avail <- avail & !ng   # drop non-gc-param rows

  # Cutpoint finalization (clamp only) on the available rows, so the pediatric-sex
  # split uses the same cutpoint the cleaning will. growthcleanr does not
  # auto-lower the cutpoint for all-adult data: adult_cutpoint is never changed
  # from the caller's value, keeping each subject's routing independent of the
  # rest of the dataset. An advisory (.gc_adult_cutpoint_advisory(), emitted by
  # the callers) suggests adult_cutpoint = 18 when the data looks adult-only.
  if (adult_cutpoint < 18) adult_cutpoint <- 18 else if (adult_cutpoint > 20) adult_cutpoint <- 20

  # Stage 2: unrecognized pediatric sex (pediatric rows with NA resolved sex).
  ped        <- avail & !is.na(agedays) & agedays < adult_cutpoint * 365.25
  nosex_mask <- ped & is.na(sex_int)
  n_ped_nosex <- sum(nosex_mask)
  if (n_ped_nosex > 0L) {
    unrec <- nosex_mask & !is.na(sex) & .gc_norm(sex) != ""
    if (any(unrec)) {
      uv <- sort(unique(as.character(sex)[unrec]))
      uv_show <- uv[seq_len(min(5L, length(uv)))]
      sex_vals <- paste0(paste(uv_show, collapse = ", "), if (length(uv) > 5L) ", ..." else "")
    } else {
      sex_vals <- "blank/NA"
    }
    all_ped_nosex <- n_ped_nosex == sum(ped)
    bullets <- c(bullets, sprintf(
      "Unrecognized sex -- %d pediatric measurement(s) (%s%%) have a sex value growthcleanr does not recognize (e.g. subjid %s, values %s); set aside as Exclude-Missing-Info.%s If your data uses these codes, tell growthcleanr instead of recoding: sex_coding = list(male = ..., female = ...). Recognized by default: 0/1, m/M, f/F, b/g, male/female, boy/girl.",
      n_ped_nosex, .gc_pct(n_ped_nosex, n_input), ex_subj(nosex_mask), sex_vals,
      if (!all_ped_nosex) "" else paste0(
        " This is every pediatric measurement, so no pediatric data were cleaned; check that the sex column is mapped correctly.",
        if (adult_cutpoint > 18 && all(agedays[nosex_mask] >= 18 * 365.25))
          " All of these measurements are at ages 18 y or older; to clean them with the adult algorithm, which does not need a value in the sex column, set adult_cutpoint = 18." else "")))
    lines <- union(lines, which(nosex_mask))
  }

  # Stage 3: zero / negative measurements, per metric parameter group.
  grp <- ifelse(param_norm %in% c("WEIGHTKG", "WEIGHTLBS"), "WEIGHTKG",
         ifelse(param_norm %in% c("HEIGHTCM", "HEIGHTIN", "LENGTHCM", "LENGTHIN"), "HEIGHTCM",
         ifelse(param_norm %in% c("HEADCM", "HEADIN"), "HEADCM", NA_character_)))
  for (zp in c("WEIGHTKG", "HEIGHTCM", "HEADCM")) {
    pmask <- avail & !is.na(grp) & grp == zp
    pden  <- sum(pmask)
    if (pden == 0L) next
    zmask <- pmask & !is.na(measurement) & measurement == 0
    if (any(zmask)) {
      bullets <- c(bullets, sprintf(
        "Zero measurements -- %d %s measurement(s) (%s%% of %s) are 0, which is not a usable value; these are set aside as Exclude-Missing-Info (e.g. subjid %s). Check your data if the proportion is unexpected.",
        sum(zmask), zp, .gc_pct(sum(zmask), pden), zp, ex_subj(zmask)))
      lines <- union(lines, which(zmask))
    }
    nmask <- pmask & !is.na(measurement) & measurement < 0
    if (any(nmask)) {
      bullets <- c(bullets, sprintf(
        "Negative measurements -- %d %s measurement(s) (%s%% of %s) are below 0, which is not a usable value; these are set aside as Exclude-Missing-Info (e.g. subjid %s). Check your data if the proportion is unexpected.",
        sum(nmask), zp, .gc_pct(sum(nmask), pden), zp, ex_subj(nmask)))
      lines <- union(lines, which(nmask))
    }
  }

  # Stage 4: negative age (visit dated before birth).
  agemask <- avail & !is.na(agedays) & agedays < 0
  if (any(agemask)) {
    bullets <- c(bullets, sprintf(
      "Negative age -- %d measurement(s) (%s%%) have agedays below 0, a visit dated before birth; these are set aside (e.g. subjid %s). Check your date-of-birth / visit-date fields if the proportion is unexpected.",
      sum(agemask), .gc_pct(sum(agemask), n_input), ex_subj(agemask)))
    lines <- union(lines, which(agemask))
  }

  # `cutpoint` is the clamped adult_cutpoint this detection used. The checkpoint
  # outer branch forwards it to every checkpoint so a checkpointed run applies the same
  # cutpoint a single run would, keeping checkpointed == single-pass.
  list(bullets = bullets, lines = lines, cutpoint = adult_cutpoint)
}

# --- Adult-cutpoint clamp warning --------------------------------------------
# adult_cutpoint outside [18, 20] is moved to the nearest bound; say so, and when
# the value looks like days (> 365) say that it is in years. Called once per run
# (checkpoint outer branch, or the non-checkpoint body); the checkpoints receive the
# already-clamped value, so they never repeat it. Non-numeric / NA values are left
# to the numeric validation.
.gc_warn_cutpoint_clamp <- function(adult_cutpoint) {
  if (!(is.numeric(adult_cutpoint) && length(adult_cutpoint) == 1L &&
        !is.na(adult_cutpoint))) return(invisible(NULL))
  if (adult_cutpoint >= 18 && adult_cutpoint <= 20) return(invisible(NULL))
  set_to <- if (adult_cutpoint < 18) 18 else 20
  warning(sprintf(
    "adult_cutpoint must be between 18 and 20 (years); %s was entered and set to %d.%s",
    format(adult_cutpoint), set_to,
    if (adult_cutpoint > 365) " adult_cutpoint is in years, not days." else ""),
    call. = FALSE)
  invisible(NULL)
}

# --- Adult-cutpoint advisory (informational; never changes adult_cutpoint) ----
# When the caller left adult_cutpoint at its default and every usable-age
# measurement is adult (>= 18y) AND at least one falls in the 18-to-20y range
# (which the default cutpoint sends to the child algorithm), suggest
# adult_cutpoint = 18. The "some row in [18, 20)" guard avoids firing on data that
# is entirely >= 20y -- already all adult-cleaned, so there is nothing to advise.
# Emitted as a warning so it surfaces even under quietly = TRUE (and is captured
# by the Shiny app's warning collector). growthcleanr does NOT auto-lower the
# cutpoint -- keeping it fixed means each subject's routing stays independent of
# the rest of the dataset. Setting adult_cutpoint explicitly (18 or 20) makes
# missing() FALSE and silences this. Called once per run (checkpoint outer branch,
# or the non-checkpoint body).
.gc_adult_cutpoint_advisory <- function(agedays, cutpoint_unset) {
  if (!isTRUE(cutpoint_unset)) return(invisible(NULL))
  known_age <- agedays[!is.na(agedays)]
  if (length(known_age) > 0L && all(known_age >= 18 * 365.25) &&
      any(known_age < 20 * 365.25))
    warning(
      "Every measurement is at age >= 18y, so this looks like adult-only data. ",
      "growthcleanr is cleaning ages 18-20 with the child algorithm (the default). ",
      "If these are adults, set adult_cutpoint = 18 to clean them with the adult ",
      "algorithm instead. Set adult_cutpoint explicitly to silence this warning.",
      call. = FALSE)
  invisible(NULL)
}

# --- 1/2 sex hard stop --------------------------------------------------------
# A 1=male/2=female scheme is common, but gc reads 1 as female, so the pediatric
# 1s would be silently misclassified as female while only the 2s drop out -- a
# plausible-looking wrong answer. When the coding is left at default, there is at
# least one pediatric row, and every present (non-blank) sex value is 1 or 2 with
# at least one 2, hard-stop with guidance. Overridable by declaring an explicit
# sex_coding (which suppresses this). Called once per run on the whole dataset
# (checkpoint outer branch, before any checkpoint is cleaned, or the
# non-checkpoint body), so sorted data cannot stop a checkpointed run midway.
.gc_stop_if_one_two_sex <- function(sex, agedays, adult_cutpoint, sex_coding) {
  if (!identical(sex_coding, .gc_default_sex_coding)) return(invisible(NULL))
  if (!any(!is.na(agedays) & agedays < adult_cutpoint * 365.25)) return(invisible(NULL))
  sex_vals <- .gc_norm(sex)
  sex_vals <- unique(sex_vals[!is.na(sex_vals) & sex_vals != ""])
  if (length(sex_vals) > 0L && all(sex_vals %in% c("1", "2")) && "2" %in% sex_vals) {
    stop("`sex` looks like a 1=male/2=female coding (values are only 1 and 2). ",
         "growthcleanr treats 1 as female by default, so this would misclassify your data. ",
         "If your data uses 1=male/2=female, pass sex_coding = list(male = 1, female = 2); ",
         "otherwise recode sex to 0/1 or m/f.")
  }
  invisible(NULL)
}

# --- All-1 sex warning (prominent; never a stop, because single-sex data are legitimate)
# Under the default coding, a sex column that is entirely 1 is read as all
# female, but could be all-male data coded 1 = male, 2 = female; that case has no
# 2 to trip the 1/2 hard stop. The whole column is checked, because a 0 or 2 on
# an adult row also reveals the coding; at least one pediatric row is required,
# because it is the child algorithm that depends on sex; and more than one
# subject is required, because a single subject being all 1 says nothing about
# the coding. Passing any explicit sex_coding silences this. Called once per run
# (checkpoint outer branch, or the non-checkpoint body).
.gc_warn_all_one_sex <- function(sex, agedays, subjid, adult_cutpoint, sex_coding,
                                 caller = "cleangrowth") {
  if (!identical(sex_coding, .gc_default_sex_coding)) return(invisible(NULL))
  if (length(unique(subjid)) < 2L) return(invisible(NULL))
  if (!any(!is.na(agedays) & agedays < adult_cutpoint * 365.25)) return(invisible(NULL))
  sex_vals <- .gc_norm(sex)
  sex_vals <- unique(sex_vals[!is.na(sex_vals) & sex_vals != ""])
  if (identical(sex_vals, "1"))
    warning(
      "POSSIBLE SEX CODING ERROR in ", caller, "(): every value in `sex` is 1, ",
      "which growthcleanr reads as female. If these subjects are male with ",
      "1 = male, 2 = female coding, they will be read as the wrong sex: ",
      "pass sex_coding = list(male = 1, female = 2). If they are female, pass ",
      "sex_coding = list(male = 0, female = 1) or code sex as F to remove this warning.",
      call. = FALSE, immediate. = TRUE)
  invisible(NULL)
}

# --- sd.recenter: validate and normalize the recentering choice -------------
# sd.recenter is NA/NULL (use the built-in reference), "none" (no recentering;
# any case, surrounding spaces ignored), or a table of recentering medians.
# Anything else -- e.g. the 2.x option names "nhanes" / "derive" -- stops with
# a pointer to the current options. A data.frame is converted to a data.table.
# The table is copied so the setnames/setkey calls downstream never modify the
# caller's object by reference. Returns NA, "none", or a data.table with columns
# param, sex, agedays, rcmedian.sd (a legacy sd.median column is renamed).
.gc_normalize_sd_recenter <- function(sd.recenter) {
  param <- n <- NULL  # data.table columns (avoid "no visible binding")
  if (is.null(sd.recenter) ||
      (is.atomic(sd.recenter) && length(sd.recenter) == 1L && is.na(sd.recenter)))
    return(NA)
  if (is.character(sd.recenter) && length(sd.recenter) == 1L) {
    rc_val <- tolower(trimws(sd.recenter))
    if (rc_val == "none") return("none")
    if (rc_val %in% c("nhanes", "derive"))
      stop(sprintf("sd.recenter = \"%s\" was removed in growthcleanr version 3. ", sd.recenter),
           "The built-in reference is now the default, so omit sd.recenter to use it, ",
           "or set sd.recenter = \"none\" for no recentering.", call. = FALSE)
  }
  if (!is.data.frame(sd.recenter))
    stop(sprintf("sd.recenter must be omitted (built-in reference), \"none\" (no recentering), or a table of recentering medians (columns param, sex, agedays, rcmedian.sd (or sd.median)); %s was entered.",
                 .gc_desc_val(sd.recenter)), call. = FALSE)
  rc <- data.table::copy(data.table::as.data.table(sd.recenter))
  if ("sd.median" %in% names(rc) && !"rcmedian.sd" %in% names(rc))
    data.table::setnames(rc, "sd.median", "rcmedian.sd")
  miss <- setdiff(c("param", "sex", "agedays", "rcmedian.sd"), names(rc))
  if (length(miss) > 0L)
    stop(sprintf("sd.recenter must have columns param, sex, agedays, and rcmedian.sd (or sd.median); it is missing: %s.",
                 paste(sub("rcmedian.sd", "rcmedian.sd (or sd.median)", miss, fixed = TRUE), collapse = ", ")),
         call. = FALSE)
  if (!is.numeric(rc$sex) || !is.numeric(rc$agedays))
    stop("sd.recenter columns sex and agedays must be numeric (sex 0 = male, 1 = female; agedays in whole days).",
         call. = FALSE)
  rc[, param := as.character(param)]
  # One median per (param, sex, agedays): the recentering join is exact, so a
  # duplicated key would duplicate every measurement at that age and break the
  # algorithm downstream.
  dup <- rc[, list(n = .N), by = c("param", "sex", "agedays")][n > 1L]
  if (nrow(dup) > 0L) {
    data.table::setorderv(dup, c("param", "sex", "agedays"))
    ex <- utils::head(dup, 5L)
    stop(sprintf(
      "The sd.recenter table has more than one row for %d param/sex/agedays combination(s) (e.g. %s%s). Keep one median per combination.",
      nrow(dup),
      paste(sprintf("%s, sex %s, agedays %s", ex$param, ex$sex, ex$agedays), collapse = "; "),
      if (nrow(dup) > 5L) "; ..." else ""),
      call. = FALSE)
  }
  rc
}

# --- sd.recenter coverage: every evaluable child row needs a median ----------
# The recentering merge is exact on (param, sex, agedays), so a row the table
# does not cover would otherwise get no recentered z-score and be set aside as
# Exclude-Missing-Info without explanation. Checked dataset-wide from the raw
# input vectors (before checkpoint/batch splitting) with the same recode helpers
# the algorithm uses. "Evaluable" = a row the child algorithm would clean:
# recognized param, known non-negative whole-day age below the cutpoint, usable
# sex, measurement > 0, and not head circumference at 5 years or older. Adult
# rows are not recentered. Only called for a user-supplied table.
.gc_check_recenter_coverage <- function(rc, subjid, param, agedays, sex,
                                        measurement, param_coding, sex_coding,
                                        adult_cutpoint) {
  rcmedian.sd <- covered <- NULL  # data.table columns
  p <- .gc_to_metric(.gc_recode_param(param, param_coding), measurement)$param
  s <- .gc_recode_sex(sex, sex_coding)
  a <- suppressWarnings(as.integer(floor(as.numeric(agedays))))  # as in cleaning
  m <- suppressWarnings(as.numeric(measurement))
  ev <- !is.na(subjid) & p %in% c("WEIGHTKG", "HEIGHTCM", "HEADCM") &
    !is.na(a) & a >= 0L & a < adult_cutpoint * 365.25 &
    !is.na(s) & !is.na(m) & m > 0 & !(p == "HEADCM" & a >= 5 * 365.25)
  if (!any(ev)) return(invisible(NULL))
  need <- data.table::data.table(param = p[ev], sex = s[ev], agedays = a[ev])[
    , list(n = .N), by = c("param", "sex", "agedays")]
  have <- unique(rc[!is.na(rcmedian.sd), c("param", "sex", "agedays")])
  have[, covered := TRUE]
  need <- merge(need, have, by = c("param", "sex", "agedays"), all.x = TRUE)
  gap <- need[is.na(covered)]
  if (nrow(gap) == 0L) return(invisible(NULL))
  data.table::setorderv(gap, c("param", "sex", "agedays"))
  ex <- utils::head(gap, 5L)
  stop(sprintf(
    "The sd.recenter table has no median for %d measurement(s) to be cleaned (e.g. %s%s). Provide a median for every param, sex and agedays in your data, or omit sd.recenter to use the built-in reference.",
    sum(gap$n),
    paste(sprintf("%s, sex %d, agedays %d", ex$param, ex$sex, ex$agedays), collapse = "; "),
    if (nrow(gap) > 5L) "; ..." else ""),
    call. = FALSE)
}

# The columns growthcleanr adds to long output for this run's options, in output
# order (the six role columns come first and are not listed). Single source for
# the output trim in cleangrowth() and for deciding which of the user's own
# columns must be renamed userinput_<name> because a returned growthcleanr
# column has the same name. `emit_cols` (semi-wide re-widen sources) are placed
# where the trim needs them. The debug set is an upper bound: the trim keeps
# only those present.
.gc_added_output_cols <- function(exclude_detail = FALSE, tri_exclude = FALSE,
                                  mean_ht = FALSE, cf_detail = FALSE,
                                  qm_detail = FALSE, corr_detail = FALSE,
                                  recenter_detail = FALSE,
                                  unit_error_range = FALSE,
                                  display_gc_id = FALSE, internal_detail = FALSE,
                                  debug = FALSE, has_dob = FALSE,
                                  has_visit_date = FALSE,
                                  show_internal_agedays = FALSE,
                                  emit_cols = NULL) {
  out <- c("exclude", "bin_exclude")
  if (isTRUE(exclude_detail)) out <- c(out, "exclude_detailed")
  if (isTRUE(tri_exclude)) out <- c(out, "tri_exclude")
  if (isTRUE(cf_detail)) out <- c(out, "cf_status", "cf_deltaz", "cf_nextz",
                                  "imperial", "cf_dage_orig", "cf_dage_next")
  if (isTRUE(qm_detail)) out <- c(out, "qm_elig_c_cf", "qm_elig_c_hardlimit",
                                  "qm_elig_c_eviltwins", "qm_elig_c_trajextreme",
                                  "qm_elig_extraneous", "qm_elig_c_traj",
                                  "qm_elig_c_absdiff", "qm_elig_c_pair",
                                  "qm_elig_c_single", "qm_elig_c_trajuneval")
  if (isTRUE(corr_detail)) out <- c(out, "corr_val", "corr_pt")
  if (isTRUE(recenter_detail)) out <- c(out, "tbc.sd", "ctbc.sd", "rcmedian.sd")
  if (isTRUE(unit_error_range)) out <- c(out, "unit_error_range")
  if (isTRUE(mean_ht)) out <- c(out, "mean_ht")
  if (isTRUE(display_gc_id)) out <- c(out, "gc_internal_id")
  if (isTRUE(internal_detail)) {
    out <- c(out, "gc_internal_meas", "gc_internal_param", "gc_internal_sex",
             "gc_internal_id", "gc_internal_agedays",
             if (isTRUE(has_dob)) "gc_internal_dob",
             if (isTRUE(has_visit_date)) "gc_internal_visit_date")
  }
  # Ages computed from dates while the user's own age column is returned as
  # submitted: the computed ages are shown even without internal_detail.
  if (isTRUE(show_internal_agedays)) out <- c(out, "gc_internal_agedays")
  out <- c(out, emit_cols)
  if (isTRUE(debug)) out <- c(out, "gc_internal_sex", "sd.orig", "sd.corr",
                              "tbc.sd", "ctbc.sd", "tbc.p_plus", "tbc.p_minus",
                              "ctbc.p_plus", "ctbc.p_minus")
  unique(out)
}

# Rename the user's columns whose names match a column growthcleanr returns
# (`taken`) to userinput_<name>, with one warning. Returns the new names (same
# order as `cols`). Stops if a userinput_<name> is itself already used (by
# `taken` or by `existing`, the user's other column names).
.gc_userinput_names <- function(cols, taken, existing = character(0),
                                hint = " or set carry_through = FALSE") {
  collide <- intersect(cols, taken)
  if (length(collide) == 0L) return(cols)
  new_nm <- paste0("userinput_", collide)
  clash  <- intersect(new_nm, c(taken, existing))
  if (length(clash) > 0L) {
    stop(sprintf(
      "your data has column(s) %s, which growthcleanr uses for its own output, and also column(s) %s, the names growthcleanr would give your columns instead. Rename or remove one of each pair,%s.",
      paste0("'", collide, "'", collapse = ", "),
      paste0("'", clash, "'", collapse = ", "), hint), call. = FALSE)
  }
  warning(sprintf(
    "Your data has column(s) named %s, which growthcleanr uses for its own output. Your column(s) are returned unchanged under the name(s) %s.",
    paste0("'", collide, "'", collapse = ", "),
    paste0("'", new_nm, "'", collapse = ", ")), call. = FALSE)
  cols[match(collide, cols)] <- new_nm
  cols
}

  #### R-Oxygen Markup (Hidden)


# Main growthcleanr algorithm and other exported functions
# Main growthcleanr algorithm (cleangrowth): adult and pediatric are in *_clean.R
#(main bulk of algorithm) and *_support.R (supporting functions)

#' Clean growth measurements (main growthcleanr function)
#'
#' @description
#' growthcleanr flags implausible values in longitudinal anthropometric
#' measurements -- weight, height/length, and head circumference -- for children
#' and adults. growthcleanr is specifically designed to evaluate electronic
#' health record data for outpatients.
#' It does not delete or edit your data. It returns your data with an added
#' \code{exclude} column labeling each row either \code{"Include"} or an
#' exclusion code naming why it was set aside. Each subject is cleaned
#' independently, using that subject's own measurement trajectory over time, so
#' splitting, subsetting, or batching the data does not change any subject's
#' result.
#'
#' The default input is one row per measurement in \strong{long} format with
#' columns \code{subjid}, \code{param}, \code{agedays}, \code{sex}, and
#' \code{measurement} (plus an optional per-row \code{id}); semi-wide and wide
#' layouts are also accepted (see \code{input_format}). You can use date of birth
#' and visit date instead of \code{agedays}. Most runs need only the
#' input-column arguments; see "Arguments at a glance" under Details for how the
#' rest are grouped.
#'
#' New to growthcleanr? Start with \code{vignette("new-start-here")}. To check
#' that growthcleanr is reading your columns, sex coding, and parameters
#' correctly before a full run, use \code{\link{gc_runtrial}} on a sample of
#' your data. See \code{vignette("output")} for the returned columns and
#' \code{vignette("configuration")} for the full argument index. For
#' ready-to-run scripts covering common setups, see \code{vignette("example-scripts")}.
#'
#' @details
#' \strong{Calling styles:}
#'
#' \enumerate{
#'   \item \strong{Data-frame style (recommended).} Pass a single data.frame
#'     or data.table as \code{data}. The \code{subjid}, \code{param},
#'     \code{agedays}, \code{sex}, \code{measurement}, and \code{id}
#'     arguments are then column-name strings identifying which columns in
#'     \code{data} to use. Each defaults to the canonical name (e.g.
#'     \code{"subjid"}), so if your columns already use those names you can
#'     just call \code{cleangrowth(data = my_df)}. Use the column-name
#'     arguments only to remap (e.g. \code{subjid = "patid"}).
#'   \item \strong{Vector style (legacy).} Omit \code{data} and pass each
#'     of \code{subjid}, \code{param}, \code{agedays}, \code{sex}, and
#'     \code{measurement} as a vector. Retained for backward compatibility
#'     with existing code; new code should use the data-frame style.
#' }
#'
#' All vectors (or the columns referenced in \code{data}) must have the
#' same length and be aligned row-for-row. Pulling values from different
#' datasets is not supported and not checked for.
#'
#' \strong{Arguments at a glance.} The many arguments fall into groups; most runs
#' touch only the first one or two:
#' \itemize{
#'   \item \strong{Core input column names} -- \code{data}, \code{subjid}, \code{sex},
#'     \code{agedays}, \code{param}, \code{measurement}, \code{id}.
#'   \item \strong{Age from dates} (instead of \code{agedays}) -- \code{dob},
#'     \code{visit_date}, \code{age_source}, \code{date_order}.
#'   \item \strong{Sex and parameter coding} -- \code{sex_coding}, \code{param_coding}.
#'   \item \strong{Input/output shape} -- \code{input_format}, \code{output_format},
#'     \code{semiwide_cols}, \code{wide_cols}, \code{max_blocks}, \code{widened_emit},
#'     \code{carry_through}.
#'   \item \strong{Run control and performance} -- \code{parallel}, \code{cores},
#'     \code{batch_size}, \code{quietly}, \code{verbose}, \code{ref_tables},
#'     \code{ref.data.path}, \code{log.path}.
#'   \item \strong{Checkpointed (resumable) runs} -- \code{checkpoint},
#'     \code{checkpoint_dir}, \code{checkpoint_size}, \code{checkpoint_resume},
#'     \code{keep_checkpoints}, \code{log_file}.
#'   \item \strong{Adult algorithm} -- \code{adult_cutpoint},
#'     \code{adult_permissiveness}, \code{adult_scale_max_lbs},
#'     \code{adult_overrides}.
#'   \item \strong{EWMA window (child and adult)} -- \code{ewma_window}.
#'   \item \strong{Length vs. height handling} -- \code{length.adjust}.
#'   \item \strong{Recentering} -- \code{sd.recenter}.
#'   \item \strong{Carried-forward rescue} -- \code{cf_rescue}.
#'   \item \strong{Optional output columns} -- \code{exclude_detail},
#'     \code{unit_error_range}, \code{tri_exclude}, \code{cf_detail},
#'     \code{corr_detail}, \code{recenter_detail}, \code{internal_detail},
#'     \code{display_gc_id}, \code{mean_ht}, \code{full_detail}, \code{qm_detail},
#'     \code{debug}.
#'   \item \strong{Advanced threshold overrides} (rarely changed) --
#'     \code{piv.z.*}, \code{error.load.*}. See \code{vignette("algorithm-functions")}
#'     for the CSD z-scores (\code{sd.orig}) the \code{piv.z.*} cutoffs apply to.
#' }
#'
#' @param data Optional data.frame or data.table containing the input
#'   measurements. When supplied, \code{subjid}, \code{param}, \code{agedays},
#'   \code{sex}, \code{measurement}, and \code{id} are interpreted as
#'   column-name strings (defaulting to the canonical names). When \code{NULL}
#'   (the default), the function uses the legacy vector-style API.
#' @param subjid Subject identifier. In data-frame style, a column name
#'   (default \code{"subjid"}). In vector style, a vector of subject
#'   identifiers, one entry per measurement row (repeated across a subject's
#'   rows).
#' @param id Row identifier. Optional but recommended. In data-frame style, a column
#'   name (default \code{"id"}); in vector style, a vector of row ids.
#'   If \code{id} is absent / \code{NULL} / contains \code{NA} / is not
#'   unique, a warning is issued and sequential integer ids (1:N in input
#'   row order) are auto-generated for internal use. The user's original
#'   \code{id} (whatever was supplied - even if \code{NA} or duplicated) is
#'   preserved in the output \code{id} column so callers can map results
#'   back to their data. To see the id that growthcleanr actually used
#'   internally, set \code{display_gc_id = TRUE} (or \code{full_detail =
#'   TRUE}); the output then includes a \code{gc_internal_id} column equal to the
#'   user's id when no regen was needed, or the auto-generated 1:N when a
#'   regen occurred. For stable row identifiers, supply a unique per-row
#'   \code{id}; a per-subject or per-encounter identifier is not a valid
#'   \code{id} for this purpose.
#'   For semi-wide/wide input with long output, the subject, sex and age
#'   columns are returned under the user's own names (for wide input, the age
#'   prefix), and the id identifies an input row,
#'   not a measurement: it is returned under its own column name (repeated on
#'   each measurement from that row), and growthcleanr's per-measurement number
#'   is returned as \code{gc_internal_id}; with no id column there is no
#'   \code{id} column in that output. An id column named like a column
#'   growthcleanr returns (e.g. \code{exclude}) comes back as
#'   \code{userinput_<name>}, with a warning. In semi-wide/wide input the id need not
#'   be unique; in semi-wide input it orders a subject's visit rows when the
#'   per-measurement ids are assigned (see \code{vignette("wrapper")}), and in
#'   wide input it does not affect cleaning.
#' @param param Measurement parameter. In data-frame style, a column name
#'   (default \code{"param"}); in vector style, a vector. Recognized values are
#'   \code{"WEIGHTKG"}, \code{"WEIGHTLBS"}, \code{"HEIGHTCM"}, \code{"HEIGHTIN"},
#'   \code{"LENGTHCM"}, \code{"LENGTHIN"}, \code{"HEADCM"}, and \code{"HEADIN"}.
#'   Matching is case-insensitive and whitespace-trimmed, but the names must be
#'   spelled out: there are deliberately no built-in abbreviations, so a short
#'   label such as \code{"wt"}, \code{"ht"}, or \code{"hc"} is not assumed to be a
#'   growthcleanr parameter because it might be a statistical sample weight or something similar.
#'   Declare any non-canonical label with \code{param_coding}. Unrecognized
#'   params are set aside as \code{Exclude-Not-GC-Param} (with a warning); the
#'   value is preserved.
#'
#'   Units and length/height handling: imperial parameters (\code{"WEIGHTLBS"},
#'   \code{"HEIGHTIN"}, \code{"LENGTHIN"}, \code{"HEADIN"}) are converted to metric
#'   during calculation. By default, linear measurements under 2 years are
#'   evaluated as length and at 2 years (day 730) and older as height,
#'   regardless of whether they are input using a LENGTH or HEIGHT parameter;
#'   in children whose data are consistent with prematurity or
#'   small-for-gestational age (SGA) and who receive an assigned gestational
#'   age for cleaning purposes, that gestational age is used to determine an
#'   estimated corrected age, and this corrected age is used to determine default linear growth handling. This
#'   default behavior can be changed with \code{length.adjust = TRUE}. Full
#'   detail is in \code{vignette("wrapper")}, section "Length vs. height
#'   handling".
#' @param agedays Age in days at each measurement. In data-frame style, a
#'   column name (default \code{"agedays"}). In vector style, a numeric
#'   vector. May be omitted if age is supplied instead as \code{dob} +
#'   \code{visit_date} (see \code{age_source}); when an \code{agedays} column
#'   is present it takes precedence over dates by default.
#'   Cleaning uses whole days: a fractional age is rounded down (1435.4 is
#'   day 1435; -0.5 is day -1) for all cleaning, but the submitted value is returned in
#'   the output \code{agedays} column (the whole-day value is available as
#'   \code{gc_internal_agedays} under \code{internal_detail}).
#' @param dob Date of birth (data-frame style only). A column name; the column is
#'   class \code{Date} or text. Supplied with \code{visit_date} to compute
#'   \code{agedays} as \code{floor(visit_date - dob)} in whole days. A subject
#'   with more than one distinct dob value is set aside as
#'   \code{Exclude-Missing-Info}, with a warning. Text dates are accepted in any order-free
#'   format -- year-first numeric (\code{"2013-03-04"}, \code{"2013/03/04"},
#'   \code{"20130304"}) or English spelled-month (\code{"03mar2013"},
#'   \code{"15 Mar 2013"}, \code{"March 15, 2024"}); ambiguous numeric day/month
#'   dates (\code{"3/4/2013"}) require \code{date_order}. All \code{dob} and
#'   \code{visit_date} values must share one format, the year must be four digits
#'   (1900-2099; two-digit years are rejected), and blank/\code{NA} yields a
#'   missing \code{agedays}. A value with no digits (e.g. \code{"N/A"},
#'   \code{"NULL"}) is treated as blank, and a few values that are not real
#'   dates (e.g. \code{"2013-02-30"}; at most 1, or 5\% of a column) are set
#'   aside; both are reported in the input warning. More unreadable dates than
#'   that stop the run. Numeric date columns (e.g. software date integers from Stata, Excel, etc.)
#'   are rejected as ambiguous -- convert to text or an R
#'   \code{Date} first. A lone \code{dob} or \code{visit_date} supplied alongside
#'   an \code{agedays} column is parsed and returned only (as
#'   \code{gc_internal_dob}/\code{gc_internal_visit_date} under
#'   \code{internal_detail}); ages still come from the \code{agedays} column unless
#'   you set \code{age_source = "dates"} to force date-based ages. Default
#'   \code{NULL}. See "Use dates instead of age in days" in \code{vignette("example-scripts")}.
#' @param visit_date Visit (measurement) date (data-frame style only). A column
#'   name; class \code{Date} or text in the same formats as \code{dob}. Supplied
#'   with \code{dob} to compute \code{agedays}, or on its own alongside an
#'   \code{agedays} column purely to be parsed and returned as
#'   \code{gc_internal_visit_date} (under \code{internal_detail}). See \code{dob}.
#'   Default \code{NULL}.
#' @param carry_through Logical (data-frame style only). If \code{TRUE}
#'   (default), any column in \code{data} that growthcleanr does not consume
#'   as a role column (i.e. anything other than the resolved \code{subjid},
#'   \code{param}, \code{agedays}, \code{sex}, \code{measurement}, and
#'   \code{id} columns) is carried through and returned appended after the
#'   growthcleanr output columns, in its original input order and values.
#'   \code{dob}/\code{visit_date} are carried through this way (so submitted
#'   dates are echoed back). A carried column whose name matches a column
#'   growthcleanr returns in this run (for example \code{exclude}, when
#'   re-cleaning growthcleanr output) is returned under the name
#'   \code{userinput_<name>}, with a warning; names of growthcleanr columns
#'   not returned in the run are left alone. \code{FALSE} restores the prior behavior of
#'   dropping all non-role columns. With semi-wide or wide output the user's
#'   columns return structurally (the submitted table is the output base), so
#'   \code{TRUE} keeps every column and \code{FALSE} keeps only the role, id,
#'   and parameter value columns. With semi-wide or wide input and long output,
#'   the other columns (including \code{dob}/\code{visit_date}; in wide input,
#'   not the per-block columns) are repeated on each measurement from their row.
#' @param input_format Shape of \code{data}: \code{"long"} (default; one row per
#'   measurement), \code{"semi_wide"} (one row per visit: a \code{subjid}, an age
#'   source, a \code{sex}, and one column per parameter), or \code{"wide"} (one
#'   row per subject: a \code{subjid}, a \code{sex}, and repeated observation
#'   blocks marked by a \code{_N} suffix -- \code{agedays_1}, \code{wt_1},
#'   \code{ht_1}, \code{agedays_2}, ...). Semi-wide and wide are reshaped to long
#'   before cleaning, so results match submitting the same measurements in long
#'   format; both require the data-frame style. Map your own column names with
#'   \code{semiwide_cols}/\code{wide_cols}. For ready-to-run scripts, see
#'   "Semi-wide data (one row per visit)" and "Wide data (one row per subject)"
#'   in \code{vignette("example-scripts")}.
#'
#'   \strong{Length/height in non-long formats.} Customized recumbent-length vs
#'   standing-height handling is a long-format-only feature: semi-wide and wide
#'   always use default handling (values under 2 years treated as recumbent
#'   length, 2 years and over as standing height, using estimated corrected age
#'   for children whose data are consistent with prematurity or
#'   small-for-gestational age (SGA) and who receive an assigned gestational
#'   age for cleaning purposes). So \code{length.adjust = TRUE}
#'   is rejected for these formats, and a \code{LENGTHCM}/\code{LENGTHIN} value
#'   at any age gets default handling, the same as in long input with
#'   \code{length.adjust = FALSE}. See \code{length.adjust} and \code{vignette("wrapper")}, section
#'   "Length vs. height handling".
#' @param output_format Shape of the returned results: \code{"long"},
#'   \code{"semi_wide"}, or \code{"wide"}. Default \code{NULL} mirrors
#'   \code{input_format}, so semi-wide input returns semi-wide output and wide
#'   input returns wide output. Re-widened output returns the submitted table
#'   itself -- every column untouched, one row per input row -- with the
#'   growthcleanr columns named in \code{widened_emit} inserted immediately
#'   right of the value column they describe (\code{exclude_wt_kg} for
#'   semi-wide, \code{exclude_wt_1} for wide). It requires the matching input
#'   format (growthcleanr cannot infer visit rows or observation blocks from a
#'   different shape); checkpointed runs are supported. Choose
#'   \code{output_format = "long"} for the full per-measurement result table,
#'   which can be merged back onto the source data by
#'   \code{subjid}/\code{param}/\code{agedays}.
#' @param internal_detail Logical. If TRUE, adds growthcleanr's own working
#'   values for the key inputs the user supplied: \code{gc_internal_meas} (the
#'   measurement gc used -- imperial converted to metric and LENGTHCM
#'   relabeled; the \code{length.adjust} offset is applied only to z-scores,
#'   not to this value), \code{gc_internal_param}
#'   (the parameter label that goes with it), \code{gc_internal_sex} (the
#'   coerced 0/1 sex), \code{gc_internal_id}, and \code{gc_internal_agedays}
#'   (the whole-day age used for cleaning). The primary \code{measurement},
#'   \code{param}, \code{agedays}, \code{sex}, and \code{id} columns always return the user's
#'   verbatim submission; these report what the algorithm worked with. The
#'   imperial conversion is reflected for every row (the adult algorithm applies
#'   the same factors), and \code{LENGTHCM} is relabeled \code{HEIGHTCM} at
#'   every age (adult length is cleaned as height); the \code{length.adjust}
#'   reconciliation is pediatric-only. If a \code{dob} and/or \code{visit_date}
#'   column was supplied, the parsed \code{Date} values are also added as
#'   \code{gc_internal_dob} / \code{gc_internal_visit_date} (whether or not the
#'   dates drove \code{agedays}), showing the interpretation growthcleanr used.
#'   Default FALSE. Enabled by \code{full_detail}.
#' @param widened_emit Character vector naming the growthcleanr output columns
#'   to add to re-widened output (semi-wide or wide); default \code{"exclude"}.
#'   Any per-measurement output column may be named (e.g. \code{"bin_exclude"},
#'   \code{"exclude_detailed"}, \code{"unit_error_range"}, \code{"measurement"},
#'   \code{"agedays"}), and a column whose own switch was not enabled is an
#'   error. Used when \code{output_format} is \code{"semi_wide"} or
#'   \code{"wide"}. How the added columns are named (e.g. \code{exclude_wt_kg})
#'   is described in \code{vignette("wrapper")}, sections "Output format:
#'   semi-wide" and "Output format: wide".
#' @param semiwide_cols Named list/vector mapping growthcleanr parameter names to
#'   the columns holding their values in semi-wide input, e.g.
#'   \code{list(WEIGHTKG = "wt_kg", HEIGHTCM = "ht_cm", HEADCM = "head_cm")}.
#'   Recognized names are \code{WEIGHTKG}, \code{HEIGHTCM}, \code{LENGTHCM},
#'   \code{HEADCM}, \code{WEIGHTLBS}, \code{HEIGHTIN}, \code{LENGTHIN}, and
#'   \code{HEADIN} (imperial/length variants are converted downstream). If
#'   \code{NULL} (default), columns literally named as recognized parameters are
#'   used; at least one recognized parameter column must be present. The
#'   restriction rejecting \code{LENGTHCM}/\code{LENGTHIN} for children 2 years and
#'   older (described under \code{input_format}) applies.
#'   Only used when \code{input_format = "semi_wide"}.
#' @param wide_cols Named list/vector mapping growthcleanr parameter names to the
#'   column-name \emph{prefixes} holding their values in wide input, e.g.
#'   \code{list(WEIGHTKG = "wt", HEIGHTCM = "ht", HEADCM = "hc")} for columns
#'   \code{wt_1}, \code{wt_2}, \code{ht_1}, .... You choose the prefix;
#'   growthcleanr requires the \code{_N} block suffix and discovers the blocks
#'   from it. Recognized names are as for \code{semiwide_cols}, and the same
#'   length-at-2-years restriction (see \code{input_format}) applies. If
#'   \code{NULL} (default), prefixes literally named as recognized parameters are
#'   used (\code{WEIGHTKG_1}, ...). The age source is a prefix too: \code{agedays}
#'   names \code{agedays_1}, \code{agedays_2}, ... (or, on the date route,
#'   \code{visit_date} names \code{visit_date_1}, ... alongside a single
#'   patient-level \code{dob} column). \code{subjid} and \code{sex} are
#'   patient-level and unsuffixed. Subjects with fewer observations leave their
#'   trailing blocks empty; empty cells are dropped. Only used when
#'   \code{input_format = "wide"}.
#' @param max_blocks Highest number of wide observation blocks expected;
#'   default 25. Finding more warns (with guidance to raise \code{max_blocks}
#'   or reshape to long or semi-wide) but still processes every block; an
#'   implausible count errors, since it usually means the \code{_N} suffixes
#'   parsed wrong. Only used when \code{input_format = "wide"}.
#' @param age_source Which age source to use when resolving \code{agedays}:
#'   \code{"agedays"} (use the \code{agedays} column) or \code{"dates"}
#'   (compute from \code{dob} + \code{visit_date}). Default \code{NULL}
#'   auto-resolves: an \code{agedays} column is used whenever present,
#'   otherwise \code{dob} + \code{visit_date} are used. Set \code{"dates"} to
#'   force date-based ages even when an \code{agedays} column is also present;
#'   that column is then returned as submitted, and the ages computed from the
#'   dates are returned as \code{gc_internal_agedays}.
#' @param date_order Day-vs-month order for ambiguous numeric \code{dob} /
#'   \code{visit_date} values, e.g. \code{"3/4/2013"} (data-frame style only).
#'   \code{"dmy"} reads day-month-year, \code{"mdy"} reads month-day-year;
#'   applied uniformly to the whole run. Default \code{NULL} accepts only the
#'   order-free formats (year-first numeric and English spelled-month) and
#'   errors on numeric day/month dates, so an ambiguous order is never guessed.
#'   growthcleanr never auto-detects the order.
#' @param sex Subject sex. In data-frame style, a column name (default
#'   \code{"sex"}). In vector style, a vector. By default \code{0}, \code{"m"},
#'   \code{"b"}, \code{"male"}, or \code{"boy"} are read as male and \code{1},
#'   \code{"f"}, \code{"g"}, \code{"female"}, or \code{"girl"} as female (case-insensitive,
#'   whitespace-trimmed); override with \code{sex_coding}. On pediatric rows,
#'   values matching neither list (and blank/\code{NA}) are set aside as
#'   \code{Exclude-Missing-Info} (with a warning). A sex column is always
#'   required, but the sex column value may be blank or \code{NA} for adults
#'   (ages at or above \code{adult_cutpoint}): the adult algorithm does not
#'   require a valid value in the \code{sex} column, but when available, it is
#'   used in the Evil Twins step and in calculating whether a value falls in the
#'   unit error range. A \code{1}=male/\code{2}=female
#'   coding is detected and rejected with an informative error (growthcleanr
#'   reads \code{1} as female), so it cannot silently misclassify data; pass
#'   \code{sex_coding = list(male = 1, female = 2)} if that is your coding.
#'   If there is more than one subject and every value is \code{1}, which could
#'   be all-female 0/1 data or all-male 1/2 data, it is read as female with a
#'   prominent warning (the run continues); pass an
#'   explicit \code{sex_coding} or code sex as F to remove the warning.
#' @param sex_coding Named list with elements \code{male} and \code{female},
#'   each a vector of the codes (numbers or strings) that denote that sex in
#'   your data. Defaults to
#'   \code{list(male = c(0, "m", "b", "male", "boy"), female = c(1, "f", "g", "female", "girl"))}.
#'   Matching is case-insensitive and whitespace-trimmed. Supplying your own
#'   table replaces the defaults wholesale (include any standard codes you also
#'   use) and suppresses the \code{1}/\code{2} guard. Errors if a code is listed
#'   under both sexes. See "Set your sex coding (including 1 = male, 2 =
#'   female)" in \code{vignette("example-scripts")}.
#' @param param_coding Optional named list mapping your parameter labels to
#'   growthcleanr's canonical parameters, e.g.
#'   \code{list(WEIGHTKG = c("WT", "Weight"), HEIGHTIN = "HT")}. Names must be
#'   canonical parameters (\code{WEIGHTKG}, \code{WEIGHTLBS}, \code{HEIGHTCM},
#'   \code{HEIGHTIN}, \code{LENGTHCM}, \code{LENGTHIN}, \code{HEADCM},
#'   \code{HEADIN}); values are
#'   the labels to remap (case-insensitive, whitespace-trimmed). This is the
#'   only way to have a non-canonical label recognized (there are no built-in
#'   abbreviations), and also how you declare that a remapped column is imperial
#'   (map it to \code{WEIGHTLBS}/\code{HEIGHTIN}/\code{LENGTHIN}/\code{HEADIN}); otherwise a
#'   remapped label defaults to its metric target. Errors if a name is not
#'   canonical or a label maps to two parameters. Default \code{NULL}. See "Use
#'   your own measurement-type labels (WT / HT / HC)" in \code{vignette("example-scripts")}.
#' @param measurement Recorded measurement. In data-frame style, a column
#'   name (default \code{"measurement"}). In vector style, a numeric vector.
#'   Weight must be in kilograms (kg), and linear measurements in
#'   centimeters (cm), unless \code{param} is \code{"WEIGHTLBS"},
#'   \code{"HEIGHTIN"}, \code{"LENGTHIN"}, or \code{"HEADIN"}. Text or factor
#'   values are converted to numbers for cleaning (values that are not numbers
#'   are set aside as \code{Exclude-Missing-Info}) and are returned as
#'   submitted.
#' @param piv.z.wt.high Upper unrecentered CSD z-score cutoff for
#' WEIGHTKG (all ages). Weights with `sd.orig > piv.z.wt.high` are
#' excluded as hard limits (\code{Exclude-Hard-Limit}). Defaults to 22.
#' @param piv.z.ht.low.young Lower unrecentered CSD z-score cutoff for
#' HEIGHTCM under one year of age. Heights with
#' `sd.orig < piv.z.ht.low.young` are excluded as hard limits (\code{Exclude-Hard-Limit}).
#' Defaults to -25.
#' @param piv.z.ht.low.old Lower unrecentered CSD z-score cutoff for
#' HEIGHTCM at or above one year. Heights with
#' `sd.orig < piv.z.ht.low.old` are excluded as hard limits (\code{Exclude-Hard-Limit}).
#' Defaults to -15.
#' @param piv.z.ht.high Upper unrecentered CSD z-score cutoff for HEIGHTCM
#' (all ages). Heights with `sd.orig > piv.z.ht.high` are excluded
#' as hard limits (\code{Exclude-Hard-Limit}). Defaults to 8 (tighter than the
#' weight upper limit).
#' @param piv.z.hc.low Lower unrecentered CSD z-score cutoff for HEADCM
#' (all ages). Head-circumference values with
#' `sd.orig < piv.z.hc.low` are excluded as hard limits (\code{Exclude-Hard-Limit}).
#' Defaults to -15.
#' @param piv.z.hc.high Upper unrecentered CSD z-score cutoff for HEADCM
#' (all ages). Head-circumference values with
#' `sd.orig > piv.z.hc.high` are excluded as hard limits (\code{Exclude-Hard-Limit}).
#' Defaults to 15.
#' @param error.load.mincount Minimum number of real errors (exclusions other
#' than \code{Exclude-Identical}, \code{Exclude-Extraneous},
#' \code{Exclude-CF}, \code{Exclude-Missing-Info}, and
#' \code{Exclude-HC-Out-of-Range}) required per (subject, param) group before
#' Child Step 21 Error Load is evaluated for that group. Defaults to 2. Unlike
#' 2.2.3, a high error load for one parameter can no longer result in the
#' exclusion of the other parameter's values.
#' @param error.load.threshold Error ratio threshold for Child Step 21. The
#' ratio is \code{n_errors / (n_errors + n_includes)} computed per
#' (subject, param) group, where \code{n_errors} and \code{n_includes}
#' leave out the codes listed under \code{error.load.mincount}. If the ratio strictly exceeds this threshold (and at least
#' \code{error.load.mincount} real errors are present), all remaining
#' Include rows for that (subject, param) group are excluded with
#' \code{Exclude-Traj-Uneval}. Defaults to 0.5.
#' @param sd.recenter Recentering medians. One of:
#'   \describe{
#'     \item{\code{NA} (default)}{Use the built-in reference file
#'       (\code{rcfile-resmoothed.csv.gz}). (In 2.x, \code{NA} derived medians
#'       from the input data for larger datasets; that option was removed.)}
#'     \item{\code{"none"}}{No recentering: the recentered scores equal the
#'       reference scores (\code{tbc.sd = sd.orig}).}
#'     \item{A data.table or data.frame}{Your own medians, with columns
#'       \code{param}, \code{sex}, \code{agedays}, and \code{rcmedian.sd} (a
#'       median column named \code{sd.median} is also accepted). The table must
#'       have exactly one median per \code{param}, \code{sex}, and
#'       \code{agedays}, and a median for every \code{param}, \code{sex}, and
#'       \code{agedays} of the child measurements to be cleaned; otherwise
#'       growthcleanr stops and reports the problem.}
#'   }
#'   The 2.x options \code{"nhanes"} and \code{"derive"} were removed and stop
#'   with an error. See \code{vignette("recentering")}.
#' @param cf_rescue CF rescue mode for the child algorithm. One of:
#'   \describe{
#'     \item{"standard"}{(Default) Decide each identical-value string on its
#'       last copy in the string; the whole string is
#'       rescued or excluded together. When the last CF has a next included
#'       value, rescue if \code{cf_deltaz + cf_nextz <= 1} and
#'       \code{cf_deltaz <= 1.5}; when it has no next value, rescue by age
#'       (\code{cf_deltaz <= 0.5} if \code{agedays < 13*365.25}, else
#'       \code{cf_deltaz <= 1}). \code{cf_deltaz} is the absolute difference
#'       between the value's z-score before recentering and the z-score of the
#'       value that was copied. See \code{vignette("cf-rescue")}.}
#'     \item{"none"}{All potential carried-forward values are excluded. A value
#'       is detected as carried forward when it exactly equals the measurement on
#'       the subject's previous measurement day and that day has only one
#'       measurement for the parameter (see \code{vignette("cf-rescue")}).}
#'     \item{"all"}{No potential carried-forward values are excluded at this step;
#'       they are evaluated by the later steps like any other value.}
#'   }
#' @param cf_detail Logical. If TRUE, add six carried-forward (CF) columns to
#'   the output: \code{cf_status} (each row's CF role, e.g. "Orig", "CF-R"
#'   (rescued), "CF-NR" (not rescued), "Not-CF", or "Adult"), \code{cf_deltaz}
#'   and \code{cf_nextz} (the z-score differences used in the rescue
#'   decision), and three design aids for CF-logic development
#'   (\code{imperial}, \code{cf_dage_orig}, \code{cf_dage_next}). Each column
#'   and value is defined in \code{vignette("output")}, section 4, and the
#'   rescue rule in \code{vignette("cf-rescue")}. Defaults to FALSE.
#' @param corr_detail Logical. If TRUE, include \code{corr_val} and
#'   \code{corr_pt} columns in the output. \code{corr_val} labels each value
#'   "Corrected" (its gestational-age-corrected z-score differs from the
#'   uncorrected one), "Uncorrected", or "Adult" (adult row). \code{corr_pt}
#'   labels each subject "Corrected" (the subject has at least one corrected
#'   value), "Uncorrected", or "Adult". Defaults to FALSE.
#' @param recenter_detail Logical. If TRUE, include the recentered CSD z-score
#'   columns in the output: \code{tbc.sd} (the recentered z used throughout the
#'   child algorithm), \code{ctbc.sd} (its gestational-age-corrected
#'   counterpart), and \code{rcmedian.sd} (the per-row recentering median that
#'   was subtracted). This is specialized output for a related tool in
#'   development (see \code{vignette("output")}, section 5b). The three
#'   columns are a subset of what \code{debug} surfaces. Child rows only; adult rows receive
#'   \code{NA}. Defaults to FALSE. Also turned on by \code{full_detail = TRUE}.
#' @param ref.data.path Path to reference data. If not supplied, the reference
#' files included with growthcleanr are used.
#' @param log.path Deprecated; supplying it gives a warning. Use \code{log_file}
#' instead. (It still writes per-batch log files in parallel, non-quiet runs.)
#' @param parallel Determines if function runs in parallel.  Defaults to FALSE.
#' See \code{vignette("large-datasets")} and "Large datasets: run in parallel
#' and save progress" in \code{vignette("example-scripts")}.
#' @param cores Number of parallel worker processes to use when
#' \code{parallel = TRUE}. \code{NULL} (the default) auto-detects
#' \code{min(8, parallel::detectCores() - 1)} -- capped at 8 because growthcleanr's
#' per-worker setup overhead makes more workers than that counterproductive.
#' Pass a positive whole number to override; a value above the machine's core
#' count is reduced to it (extra workers only oversubscribe, and an extreme
#' value would stall the run spawning workers). Ignored when
#' \code{parallel = FALSE}. (Replaces the former \code{num.batches} argument.)
#' @param quietly Controls progress output. With the default
#' \code{quietly = FALSE}, growthcleanr prints a minimal progress view: a
#' "cleaning in progress" line, an X/Y batch (or checkpoint) counter, and
#' the end-of-run cleaning summary. Set \code{quietly = TRUE} to turn off the
#' progress lines and the summary. Warnings still appear, as does the one-time
#' note shown when \code{cleangrowth()} is called with vectors (the 2.x style);
#' that note is a message, so \code{suppressMessages()} will silence it. For the
#' detailed per-step view, use
#' \code{verbose} instead. Defaults to FALSE.
#' @param verbose When TRUE, prints the detailed view: the milestone lines
#' (preprocessing, per-batch cleaning child/adult) plus granular per-step
#' diagnostics (pre-filter counts, EWMA/SDE iteration counts, etc.). Implies
#' \code{quietly = FALSE}. Intended for development and debugging. Defaults to
#' FALSE.
#' @param adult_cutpoint Age in years, between 18 and 20, describing ages when the
#'  pediatric algorithm should not be applied (< adult_cutpoint), and the adult
#'   algorithm should apply (>= adult_cutpoint). Numbers outside this range are
#'   changed to the closest number within the range, with a warning. Defaults to 20. growthcleanr
#'   does not change this value based on your data (so each subject is cleaned
#'   independently of the rest of the dataset); if it is left at the default and
#'   every measurement with a known age is at age 18 years or older, a warning
#'   suggests setting \code{adult_cutpoint = 18} to clean those 18-to-20-year-olds
#'   with the adult algorithm instead of the pediatric one.
#' @param adult_permissiveness Permissiveness level for the adult algorithm:
#'   "loosest", "looser" (default), "tighter", or "tightest" (case and
#'   surrounding spaces are ignored). Sets defaults
#'   for all adult PIV limits, EWMA caps, height bands, etc. To change one or a
#'   few of those settings without switching preset, use
#'   \code{adult_overrides}. For the intended use of each level and its preset
#'   values, see Section 1D of \code{vignette("configuration")}. For a
#'   ready-to-run script, see "Data that includes adults" in
#'   \code{vignette("example-scripts")}.
#' @param adult_scale_max_lbs Physical scale upper limit in pounds for adult
#'   weight data (replaces 2.2.3's `weight_cap`, which was in kg). Weights within
#'   +/-0.1 kg of the cap are excluded if they are more than 50 kg different from
#'   their expected value, or if all of the subject's weights (at least two) are
#'   the same value at the cap. Defaults to Inf.
#' @param ewma_window Used by both the child and adult algorithms. Number of
#'   measurements on each side included in the
#'   exponentially weighted moving average (EWMA) weighting window. Defaults to 15.
#' @param adult_overrides Optional named list overriding individual adult
#'   permissiveness settings (those set by \code{adult_permissiveness}),
#'   e.g. \code{list(overall_ht_min = 120, allow_ht_gain = FALSE)}. Each named
#'   value replaces the preset value for that setting; unnamed or
#'   unrecognized names are an error. Applies to adult rows only. The recognized
#'   names are the adult settings listed in \code{vignette("configuration")}
#'   (Section 1D). Defaults to NULL (use the preset for every setting).
#' @param ref_tables Optional. Pre-loaded reference tables from
#'   \code{\link{gc_preload_refs}}. When provided, growthcleanr skips reading
#'   the reference files, saving about a second per call. Recommended for
#'   repeated calls (e.g., simulation loops). Defaults to NULL (reads files
#'   each call).
#' @param length.adjust Logical (default FALSE). If TRUE, reconciles how each
#'   linear growth measurement was entered with what the reference at that age
#'   expects, in \strong{both directions}: a \code{LENGTHCM} value at 2 years or
#'   older is z-scored 0.7 cm lower (recumbent to standing) and a \code{HEIGHTCM}
#'   value under 2 years 0.7 cm higher (standing to recumbent), where 2 years
#'   means day 730, the day the reference files switch from length to height; values already in
#'   the expected method, and rows with a missing age, are unchanged. The offset
#'   is applied only where a value is z-scored: the checks that work in raw
#'   centimeters (such as carried-forward detection) and \code{gc_internal_meas}
#'   use the entered value, and the returned \code{measurement} is always the
#'   user's verbatim submission. It is pediatric-only (rows at or above \code{adult_cutpoint} are
#'   not adjusted; adult \code{LENGTHCM}/\code{LENGTHIN} rows are relabeled as
#'   height, so adult length is cleaned as height). \strong{Long input only:} rejected for \code{input_format}
#'   \code{"semi_wide"}/\code{"wide"}, which always use default handling. For
#'   gestational-age-corrected children with chronological age 2 years or
#'   older but corrected age under 2 years, the default presumes recumbent
#'   length (the value is z-scored 0.7 cm lower against the standing-height
#'   reference), while \code{length.adjust = TRUE} uses the
#'   label. Full detail (the both-directions table, the exact cutoff, the
#'   gestational-age window, and where it runs in preprocessing) is in
#'   \code{vignette("wrapper")}, section "Length vs. height handling".
#' @param tri_exclude Logical. If TRUE, include a \code{tri_exclude} column in the output
#'   that classifies each row as \code{"Include"}, \code{"Same-Day"} (same-day extraneous
#'   values), or \code{"Exclude"} (all other exclusions). Defaults to FALSE.
#' @param debug Logical. If TRUE, retain intermediate diagnostic columns in the
#'   output instead of trimming to the public columns. Intended for threshold
#'   refinement and development, not routine use. Currently surfaces the
#'   raw and recentered CSD z-scores captured before cleaning
#'   (\code{sd.orig}, \code{sd.corr}, \code{tbc.sd}, \code{ctbc.sd}) and
#'   the recentered plus/minus perturbation
#'   z-scores (\code{tbc.p_plus}, \code{tbc.p_minus}, \code{ctbc.p_plus},
#'   \code{ctbc.p_minus}), plus \code{gc_internal_sex} (growthcleanr's
#'   interpreted 0/1 sex). Adult rows receive \code{NA} for the child-only
#'   z-score columns. Defaults to FALSE.
#' @param exclude_detail Logical. If TRUE, add an \code{exclude_detailed} column
#'   reporting each value's exclusion at the Detailed level: step-level codes
#'   that keep the child (\code{-C-}) / adult (\code{-A-}) algorithm prefix.
#'   The default \code{exclude} column always reports the consolidated Summary
#'   level. Preprocessing codes such as \code{Exclude-Missing-Info} are the
#'   same in both columns. Defaults to FALSE.
#' @param qm_detail Logical. If TRUE, emit the child-algorithm per-step
#'   eligibility flags (\code{qm_elig_c_cf}, \code{qm_elig_c_hardlimit},
#'   \code{qm_elig_c_eviltwins}, \code{qm_elig_c_trajextreme},
#'   \code{qm_elig_extraneous}, \code{qm_elig_c_traj}, \code{qm_elig_c_absdiff},
#'   \code{qm_elig_c_pair}, \code{qm_elig_c_single},
#'   \code{qm_elig_c_trajuneval}): one logical column per reported cleaning step
#'   marking the rows that were eligible for (in scope of) that step -- the
#'   denominator against which that step's exclusions are measured. Child rows
#'   only; NA on adult rows. Used for calculations in a related package
#'   currently in development. Not used in cleaning; typically paired with \code{exclude_detail = TRUE} so downstream
#'   code can derive which step excluded each row. Additive, write-only (does
#'   not affect any exclusion decision), and off by default. Must be requested
#'   explicitly -- it is NOT turned on by \code{full_detail}. Defaults to FALSE.
#' @param full_detail Logical. Convenience switch: if TRUE, turns on all optional
#'   output groups at once (equivalent to \code{cf_detail = TRUE},
#'   \code{corr_detail = TRUE}, \code{debug = TRUE},
#'   \code{tri_exclude = TRUE}, \code{display_gc_id = TRUE},
#'   \code{recenter_detail = TRUE}, \code{internal_detail = TRUE},
#'   \code{exclude_detail = TRUE}, and \code{mean_ht = TRUE}, which is already
#'   on by default when the data has adult rows). Defaults to
#'   FALSE. Does NOT turn on \code{qm_detail} (the quality-measure eligibility
#'   flags must be requested explicitly), and does not affect
#'   \code{unit_error_range}, which is on by default independently.
#' @param display_gc_id Logical. If TRUE, add a \code{gc_internal_id} column to the
#'   output containing the id that growthcleanr actually used internally:
#'   the user's id when it was valid (no regen needed), or the
#'   auto-generated 1:N (in input row order) when the user's id was absent /
#'   \code{NA} / non-unique and growthcleanr had to regenerate. Lets callers
#'   recover a reliable per-row key when their input id had problems
#'   (the output \code{id} column always preserves whatever the user
#'   supplied, even if invalid). Defaults to FALSE. Set via
#'   \code{full_detail = TRUE} alongside the other detail switches. For
#'   semi-wide/wide input with long output, \code{gc_internal_id} is always
#'   returned.
#' @param mean_ht Logical. Include a \code{mean_ht} column: the adult
#'   algorithm's calculated mean height (cm), computed only from the subject's finally-included heights but
#'   populated on all of the subject's adult height rows (an excluded row takes
#'   the value of the included height nearest in age). \code{NA} for child rows,
#'   adult weight rows, and subjects with no included adult height. The column can be
#'   used by researchers who want a consistent height for evaluation of body
#'   mass index. By default the column is returned when the data has adult rows
#'   and omitted otherwise; set TRUE to always return it or FALSE to omit it. For
#'   semi-wide or wide output, name it in \code{widened_emit} to add it beside the
#'   height values.
#' @param unit_error_range Logical. If TRUE (default), include a
#'   \code{unit_error_range} column: a direction-agnostic flag that is
#'   \code{TRUE} when a value's magnitude is consistent with a metric/imperial
#'   unit-conversion error and the value is not itself within the plausible
#'   z = -3 to +3 band. Purely informational -- it never affects the
#'   \code{exclude} decision and is not an exclusion code. Computed only for
#'   evaluable rows (\code{Exclude-Missing-Info}, \code{Exclude-Not-GC-Param},
#'   and \code{Exclude-HC-Out-of-Range} rows are \code{FALSE}). Real values
#'   from people with atypical growth (for example, extremely premature
#'   infants, severe obesity, some medical conditions) can fall in the
#'   unit-error range. Set FALSE to omit the column.
#' @param batch_size Number of subjects per processing batch. Subjects are never split
#'   across batches. Must be a single positive whole number. Defaults to 5000.
#'   This is the \emph{in-memory} granularity (how many subjects are held in RAM
#'   at once within a run); it is independent of \code{checkpoint_size}, the persist
#'   granularity used under checkpointing.
#' @param checkpoint Logical (default TRUE). Master switch for checkpointing --
#'   cleaning the subjects in checkpoints and writing each finished checkpoint to
#'   disk instead of holding the whole dataset in memory at once. Checkpointing
#'   substantially speeds up large datasets and prevents the out-of-memory
#'   crashes that large in-memory runs can hit (the memory-heavy steps scale
#'   super-linearly with the in-memory working set). When TRUE and no
#'   \code{checkpoint_dir} is given, checkpoints go to an ephemeral per-session
#'   temporary directory and are deleted automatically when the run ends,
#'   including after an error or interrupt, though not if R itself crashes or
#'   is killed (\code{keep_checkpoints} is forced FALSE); this only engages
#'   when the data exceeds one checkpoint (more than \code{checkpoint_size}
#'   subjects), since a single checkpoint is the whole dataset in memory and
#'   would give no benefit -- so small/typical data runs fully in memory with
#'   nothing written to disk. Set \code{checkpoint = FALSE} to disable
#'   checkpointing entirely and never write intermediate data to disk (the
#'   escape hatch for environments that forbid any data on disk); if a
#'   \code{checkpoint_dir} was also supplied it is ignored (with a warning).
#'   Supplying \code{checkpoint_dir} keeps the files at that location (see below).
#'   Very large datasets on a Linux system whose temporary directory is a RAM
#'   disk (tmpfs) should pass a \code{checkpoint_dir} on a real disk volume;
#'   macOS and Windows use disk-backed temporary directories by default.
#' @param checkpoint_dir Optional directory for \emph{persistent} checkpointed
#'   runs. When set, \code{cleangrowth} splits subjects into checkpoints of
#'   \code{checkpoint_size}, cleans each checkpoint, saves its result to a
#'   \code{checkpoint_NNNN.rds} file in this directory, and combines the
#'   checkpoints at the end. Unlike the ephemeral \code{checkpoint = TRUE}
#'   default, these files persist after the run (subject to
#'   \code{keep_checkpoints}) and can be resumed: if a run is interrupted,
#'   re-running with \code{checkpoint_resume = TRUE} skips checkpoints that
#'   already have a checkpoint file. The directory is created if it does not
#'   exist; use a dedicated directory per dataset/run. When \code{NULL}
#'   (default), checkpointing -- if it engages at all -- writes to the ephemeral
#'   temporary directory described under \code{checkpoint}. Checkpointed results
#'   are byte-identical to a single-pass run (each subject's rows stay together
#'   in one checkpoint).
#' @param checkpoint_size Number of subjects per checkpoint (one
#'   \code{checkpoint_NNNN.rds} file per checkpoint). Default 5000. Also the
#'   subject count above which the default \code{checkpoint = TRUE} engages
#'   (temporary folder) when \code{checkpoint_dir} is not set. This is the \emph{persist} granularity and is
#'   independent of \code{batch_size} (the in-memory granularity within a checkpoint);
#'   \code{batch_size < checkpoint_size} is allowed. The number of checkpoints is capped at
#'   9999: a \code{checkpoint_size} that would exceed the cap is increased to stay
#'   within it (with a warning when you set the size; results are not affected). When resuming, use the same
#'   \code{checkpoint_size} and dataset as the original run.
#' @param checkpoint_resume Logical (default FALSE). Only used when
#'   \code{checkpoint_dir} is set. When FALSE, the run starts fresh: any
#'   checkpoint files already in \code{checkpoint_dir} from a prior run are
#'   deleted first. When TRUE, existing checkpoint files are reused -- checkpoints
#'   that already have one are skipped -- to resume an interrupted run. When
#'   TRUE, you are responsible for using the same dataset, \code{checkpoint_size},
#'   and other cleaning arguments as the run that wrote those files; these are
#'   not checked, and a mismatch silently mixes results from different settings.
#' @param keep_checkpoints Logical (default TRUE). Only used when
#'   \code{checkpoint_dir} is set. Controls what happens to the per-checkpoint
#'   checkpoint files \emph{after} a successful run: if TRUE they are left in
#'   \code{checkpoint_dir}; if FALSE they are deleted once the combined result is
#'   returned. Independent of \code{checkpoint_resume}.
#' @param log_file Optional path to a log file. Used whenever checkpointing
#'   engages: when \code{checkpoint_dir} is set, or under the default
#'   \code{checkpoint = TRUE} when the data has more than
#'   \code{checkpoint_size} subjects. Progress messages are always written to the
#'   console (unless \code{quietly}); if provided, they are also appended here so
#'   a colleague can monitor a long run remotely. The log file is safe to read
#'   while the run is in progress.
#' @param .checkpoint_driven Internal. Set to TRUE by
#'   the internal checkpoint runner so that the per-checkpoint header, banner,
#'   and elapsed-time framing are printed by the checkpoint wrapper instead of
#'   here (this call then prints only its milestone lines). Not for direct use.
#' @param ... Not used. Present so that an argument removed since growthcleanr
#'   2.x (for example \code{num.batches} or \code{weight_cap}) stops with what to
#'   use instead; any other unrecognized argument stops as usual. The full
#'   list is in "Removed and renamed arguments" in
#'   \code{vignette("experienced-start-here")}.
#'
#' @return By default, a \code{data.table} in long format (one row per input
#'   measurement, in the same order as the input); the exclusion codes are in
#'   its \code{exclude} column. (Prior versions returned a character vector of
#'   exclusion codes.) Eight columns are always present, plus \code{unit_error_range}
#'   unless you turn it off:
#'   \describe{
#'     \item{\code{id}}{User-provided row identifier (or the auto-generated 1:N
#'       if none was usable).}
#'     \item{\code{subjid}}{Subject identifier, as submitted.}
#'     \item{\code{param}}{Measurement parameter, as submitted.}
#'     \item{\code{agedays}}{Age in days, as submitted.}
#'     \item{\code{sex}}{Sex, as submitted.}
#'     \item{\code{measurement}}{Measured value, as submitted.}
#'     \item{\code{exclude}}{The result (a factor): \code{"Include"} or a
#'       Summary-level exclusion code naming why the value was set aside.}
#'     \item{\code{bin_exclude}}{A two-way collapse of \code{exclude}
#'       (character):
#'       \code{"Include"} or \code{"Exclude"}.}
#'     \item{\code{unit_error_range}}{\strong{On by default} (set
#'       \code{unit_error_range = FALSE} to omit). A logical informational flag,
#'       \code{TRUE} for a value whose magnitude looks like a unit-conversion
#'       error; never affects \code{exclude}.}
#'     \item{\code{mean_ht}}{\strong{Returned by default when the data has
#'       adult rows} (set \code{mean_ht = FALSE} to omit). The adult algorithm's calculated
#'       mean height (cm), computed from
#'       included heights only and populated on all of a subject's adult height
#'       rows; \code{NA} for child rows, adult weight rows, and subjects with no
#'       included adult height.}
#'   }
#'
#'   Exclusion codes changed in version 3; for the mapping from 2.x codes, see
#'   "Mapping from prior exclusion codes" in
#'   \code{vignette("experienced-start-here")}.
#'
#'   \strong{Output fidelity.} The five echoed input columns (\code{subjid},
#'   \code{param}, \code{agedays}, \code{sex}, \code{measurement}) carry your
#'   \emph{verbatim submitted values} -- growthcleanr never silently alters or
#'   drops an input value. For example, imperial values are returned as submitted (not converted to metric), even though internally, growthcleanr converts them to metric.
#'   A value growthcleanr cannot use -- an uninterpretable \code{sex}; a
#'   \code{0}, negative, or missing \code{measurement}; a missing \code{param}; or
#'   a negative/\code{NA} \code{agedays} -- is flagged \code{Exclude-Missing-Info};
#'   a \code{param} that is not one growthcleanr recognizes is flagged
#'   \code{Exclude-Not-GC-Param}; and a head-circumference value at 5 years or
#'   older (beyond the reference) is flagged \code{Exclude-HC-Out-of-Range}. In
#'   every case the value is still returned as submitted. The internal working
#'   values are available via \code{internal_detail}/\code{debug} (see below).
#'
#'   \strong{Column naming.} The six data columns (\code{id}, \code{subjid},
#'   \code{param}, \code{agedays}, \code{sex}, \code{measurement}) are renamed to
#'   match your original argument expressions when those resolve to a simple name
#'   -- e.g. \code{cleangrowth(subjid = dt$patid, ...)} returns a column named
#'   \code{patid}. Complex expressions fall back to the canonical name. In
#'   data-frame style, output columns keep your column names (e.g.
#'   \code{subjid = "patid"} returns \code{patid}).
#'
#'   \strong{Optional columns.} Each of these switches adds columns; all default
#'   to off unless noted:
#'   \describe{
#'     \item{\code{exclude_detail}}{\code{exclude_detailed} -- the step-level
#'       (Detailed) exclusion code behind each Summary \code{exclude} value.}
#'     \item{\code{tri_exclude}}{\code{tri_exclude} -- \code{"Include"},
#'       \code{"Same-Day"}, or \code{"Exclude"}.}
#'     \item{\code{cf_detail}}{\code{cf_status}, \code{cf_deltaz}, \code{cf_nextz},
#'       and three carried-forward design aids (\code{imperial},
#'       \code{cf_dage_orig}, \code{cf_dage_next}).}
#'     \item{\code{corr_detail}}{\code{corr_val} (per value) and \code{corr_pt}
#'       (per subject) gestational-age correction status.}
#'     \item{\code{recenter_detail}}{The recentered CSD z-score columns
#'       \code{tbc.sd}, \code{ctbc.sd}, and \code{rcmedian.sd} (child rows; NA for
#'       adults) -- specialized output for a related tool in development.}
#'     \item{\code{internal_detail}}{growthcleanr's working values for your
#'       inputs: \code{gc_internal_meas}, \code{gc_internal_param},
#'       \code{gc_internal_sex}, \code{gc_internal_id},
#'       \code{gc_internal_agedays} (the whole-day age used for cleaning) (and parsed
#'       \code{gc_internal_dob}/\code{gc_internal_visit_date} if dates were
#'       supplied).}
#'     \item{\code{display_gc_id}}{\code{gc_internal_id} -- the id growthcleanr
#'       actually used internally.}
#'     \item{\code{debug}}{Intermediate z-score diagnostics for child rows
#'       (\code{sd.orig}, \code{sd.corr}, \code{tbc.sd}, \code{ctbc.sd}, and the
#'       plus/minus perturbation z-scores) plus \code{gc_internal_sex}; for
#'       development, not routine use.}
#'     \item{\code{qm_detail}}{Ten per-step child eligibility flags
#'       (\code{qm_elig_*}); for eligibility-normalized quality measures used for a related package in development. Must be
#'       requested explicitly -- \strong{not} turned on by \code{full_detail}.}
#'     \item{\code{full_detail}}{Convenience switch turning on all of the above
#'       \emph{except} \code{qm_detail} (and independent of
#'       \code{unit_error_range}).}
#'   }
#'
#'   \strong{Non-long output.} With \code{output_format = "semi_wide"} or
#'   \code{"wide"} the returned object is your submitted table with the columns
#'   named in \code{widened_emit} inserted beside the value they describe (see
#'   \code{output_format}).
#'
#'   The full catalog of exclusion codes (child + adult, Summary and Detailed) is
#'   in \code{vignette("exclusion-codes")}.
#' @md
#'
#' @export
#' @import data.table
#' @import foreach
#' @import doParallel
#' @import parallel
#' @importFrom utils write.csv
#' @rawNamespace import(R.utils, except = c(extract))
#' @examples
#' \donttest{
#' # Check your setup on a sample of your data first: gc_runtrial(syngrowth)
#'
#' # Data-frame style (recommended): canonical column names, just pass the
#' # data.frame.
#' # syngrowth is a small example dataset bundled with growthcleanr.
#' df_stats <- as.data.frame(syngrowth)
#' # Use 5 subjects so the example runs quickly
#' df_stats <- df_stats[df_stats$subjid %in% unique(df_stats[, "subjid"])[1:5], ]
#' # We recommend including a unique per-row `id` column (syngrowth already
#' # does; this shows how to add one)
#' df_stats$id <- seq_len(nrow(df_stats))
#' clean_stats <- cleangrowth(data = df_stats)
#'
#' # How many values were included, and why the others were excluded:
#' table(clean_stats$exclude)
#'
#' # The result is your data with an added `exclude` column; keep only the
#' # measurements growthcleanr says to include:
#' clean_only <- clean_stats[clean_stats$exclude == "Include", ]
#'
#' # Data-frame style with remapped column names
#' my_df <- df_stats
#' names(my_df)[names(my_df) == "subjid"] <- "patid"
#' clean_stats <- cleangrowth(data = my_df, subjid = "patid")
#'
#' # Vector style (legacy)
#' clean_stats <- cleangrowth(subjid = df_stats$subjid,
#'                            param = df_stats$param,
#'                            agedays = df_stats$agedays,
#'                            sex = df_stats$sex,
#'                            measurement = df_stats$measurement,
#'                            id = df_stats$id)
#'
#' # Parallel processing: run using 2 cores
#' clean_stats <- cleangrowth(data = df_stats,
#'                            parallel = TRUE,
#'                            cores = 2)
#' }

cleangrowth <- function(data = NULL,
                        subjid = NULL,
                        param = NULL,
                        agedays = NULL,
                        sex = NULL,
                        measurement = NULL,
                        piv.z.wt.high = 22,
                        piv.z.ht.low.young = -25,
                        piv.z.ht.low.old = -15,
                        piv.z.ht.high = 8,
                        piv.z.hc.low = -15,
                        piv.z.hc.high = 15,
                        error.load.mincount = 2,
                        error.load.threshold = 0.5,
                        sd.recenter = NA,
                        cf_rescue = "standard",
                        cf_detail = FALSE,
                        corr_detail = FALSE,
                        recenter_detail = FALSE,
                        qm_detail = FALSE,
                        ref.data.path = "",
                        log.path = NA,
                        parallel = FALSE,
                        cores = NULL,
                        quietly = FALSE,
                        verbose = FALSE,
                        adult_cutpoint = 20,
                        adult_permissiveness = "looser",
                        adult_scale_max_lbs = Inf,
                        ewma_window = 15,
                        adult_overrides = NULL,
                        id = NULL,
                        dob = NULL,
                        visit_date = NULL,
                        age_source = NULL,
                        date_order = NULL,
                        sex_coding = .gc_default_sex_coding,
                        param_coding = NULL,
                        carry_through = TRUE,
                        input_format = "long",
                        output_format = NULL,
                        semiwide_cols = NULL,
                        widened_emit = "exclude",
                        wide_cols = NULL,
                        max_blocks = 25,
                        internal_detail = FALSE,
                        ref_tables = NULL,
                        tri_exclude = FALSE,
                        debug = FALSE,
                        full_detail = FALSE,
                        unit_error_range = TRUE,
                        display_gc_id = FALSE,
                        exclude_detail = FALSE,
                        mean_ht = TRUE,
                        batch_size = 5000,
                        length.adjust = FALSE,
                        checkpoint = TRUE,
                        checkpoint_dir = NULL,
                        checkpoint_size = 5000,
                        checkpoint_resume = FALSE,
                        keep_checkpoints = TRUE,
                        log_file = NULL,
                        .checkpoint_driven = FALSE,
                        ...)
                        {
  # `...` exists only so that arguments removed since growthcleanr 2.x give an
  # informative error (and any other unknown argument still errors as R would).
  # substitute() captures the argument expressions without evaluating them, as
  # R's own "unused argument" check does.
  .gc_check_dots(as.list(substitute(list(...)))[-1L], sd.recenter = sd.recenter)

  # Vector style (no `data`): may be code written for growthcleanr 2.x, which
  # returned a vector. Recorded now (before `data` can be reassigned) for the
  # one-time note emitted after the run.
  .gc_vector_style <- is.null(data)

  # adult_permissiveness is matched case- and space-insensitively ("Tighter",
  # " looser"). Done at entry so checkpoints receive the normalized value; other
  # values are validated in resolve_permissiveness() on the adult path.
  if (is.character(adult_permissiveness) && length(adult_permissiveness) == 1L &&
      !is.na(adult_permissiveness))
    adult_permissiveness <- tolower(trimws(adult_permissiveness))

  # ewma_window: number of neighbors on each side for EWMA weighting.

  # Three progress levels, expressed via the two flags:
  #   quietly = TRUE          -> silent: no progress lines and no end summary
  #   default (quietly=FALSE) -> minimal: a "cleaning in progress" line, an X/Y
  #                              batch (or checkpoint) counter, end summary
  #   verbose = TRUE          -> detailed: milestone lines + granular per-step
  #                              diagnostics (implies quietly = FALSE)
  # The wrapper milestone lines below gate the minimal set on `!quietly` and the
  # detailed set on `verbose`; cleanchild()/cleanadult() locally remap
  # `quietly <- !verbose` so their per-step messages are verbose-only.
  if (isTRUE(verbose)) quietly <- FALSE

  # --- Progress-output helpers (used only when !quietly) ---
  # .ts(): short timestamp prefix for milestone lines.
  # .fmt_dur(): compact elapsed string ("32s" / "18m04s" / "1h02m").
  .ts <- function() format(Sys.time(), "[%H:%M:%S] ")
  .fmt_dur <- function(secs) {
    secs <- as.numeric(secs)
    if (secs < 60) return(sprintf("%.0fs", secs))
    if (secs < 3600) return(sprintf("%dm%02ds", secs %/% 60, round(secs %% 60)))
    sprintf("%dh%02dm", secs %/% 3600, (secs %% 3600) %/% 60)
  }

  # full_detail is a convenience switch that turns on every optional output
  # group at once (cf_detail + corr_detail + recenter_detail + debug +
  # tri_exclude + display_gc_id + exclude_detail + internal_detail + mean_ht).
  # NOTE: qm_detail is deliberately NOT included -- the qm_elig_* quality-measure
  # eligibility flags are for downstream quality-measure tooling and must be
  # requested explicitly (qm_detail = TRUE). They are computed only when asked
  # and do not affect cleaning.
  if (isTRUE(full_detail)) {
    cf_detail <- TRUE
    corr_detail <- TRUE
    recenter_detail <- TRUE
    debug <- TRUE
    tri_exclude <- TRUE
    display_gc_id <- TRUE
    exclude_detail <- TRUE
    internal_detail <- TRUE
    mean_ht <- TRUE
  }
  # Left at its default, mean_ht is decided once the ages are known (see below).
  .mean_ht_auto <- missing(mean_ht) && !isTRUE(full_detail) &&
    !("mean_ht" %in% widened_emit)
  if (.mean_ht_auto) mean_ht <- FALSE

  # --- Input-format dispatch (Hook A) ---------------------------------------
  # Reshape non-long input to the canonical long form BEFORE the data-frame
  # funnel below, so the entire cleaning algorithm runs unchanged. The reshape
  # emits a canonical-column long data.table and resets the role arguments so
  # the existing data-frame path handles it with all-default column mappings.
  # See R/reshape_adapters.R.
  input_format <- match.arg(input_format, c("long", "semi_wide", "wide"))
  # [length-semiwide-block] length.adjust applies NON-default handling of linear
  # growth measurements (the recumbent-length vs standing-height 0.7 cm
  # reconciliation), which must be labeled per measurement -- only long input can
  # express that. Semi-wide/wide always use default handling (the reference's
  # built-in recumbent-<2y / standing->=2y transition), so refuse the flag there
  # rather than silently ignore it. (A LENGTH label in semi-wide/wide input
  # simply gets default handling; see the note after age resolution below.)
  if (isTRUE(length.adjust) && input_format %in% c("semi_wide", "wide")) {
    stop("length.adjust = TRUE applies non-default handling of linear growth measurements and requires long input (input_format = \"long\"), so each value can be labeled recumbent length or standing height individually. Reshape to long, or remove the length.adjust = TRUE argument.")
  }
  # date_order declares day-vs-month order for ambiguous numeric dates (see the
  # date-parsing helpers in R/reshape_adapters.R). NULL = accept only order-free
  # formats (year-first numeric + English spelled-month).
  if (!is.null(date_order)) {
    if (!(length(date_order) == 1L && date_order %in% c("dmy", "mdy"))) {
      stop(sprintf('`date_order` must be NULL, "dmy", or "mdy"; %s was entered.', .gc_desc_val(date_order)))
    }
  }

  # ref_tables, when supplied, must carry the reference closures that
  # gc_preload_refs() produces (mtz_cdc_prelim, mtz_who_prelim). A hand-built
  # list with a missing or misnamed element yields NULL where a closure is
  # expected, which otherwise fails far downstream in z-score calculation with a
  # cryptic "attempt to apply non-function". Validate up front and point at the
  # intended constructor. Only reachable when a caller builds ref_tables by hand
  # instead of using gc_preload_refs().
  if (!is.null(ref_tables)) {
    ref_required_keys <- c("mtz_cdc_prelim", "mtz_who_prelim")
    if (!is.list(ref_tables)) {
      stop(sprintf("`ref_tables` must be a list from gc_preload_refs(); %s was entered.",
                   .gc_desc_val(ref_tables)))
    }
    ref_missing_keys <- setdiff(ref_required_keys, names(ref_tables))
    if (length(ref_missing_keys) > 0L) {
      stop(sprintf("`ref_tables` is missing required element(s): %s. Build it with gc_preload_refs() rather than by hand.",
                   paste(ref_missing_keys, collapse = ", ")))
    }
    ref_not_fun <- ref_required_keys[!vapply(ref_tables[ref_required_keys], is.function, logical(1))]
    if (length(ref_not_fun) > 0L) {
      stop(sprintf("`ref_tables` element(s) %s must be the function(s) from gc_preload_refs(), not other values.",
                   paste(ref_not_fun, collapse = ", ")))
    }
  }
  # Parsed dob/visit_date (input-row order), surfaced as gc_internal_dob /
  # gc_internal_visit_date under internal_detail/full_detail. Populated whenever
  # the corresponding column is supplied -- on the date route AND when an
  # agedays column drives the algorithm (parse-only); NULL when the column is
  # absent. Each is independent (a lone visit_date populates only .gc_internal_vdate).
  .gc_internal_dob   <- NULL
  .gc_internal_vdate <- NULL
  # Default output shape mirrors the input shape.
  output_format <- if (is.null(output_format)) input_format else
    match.arg(output_format, c("long", "semi_wide", "wide"))
  # Re-widened output (semi-wide/wide) only makes sense when the input arrived in
  # that shape: growthcleanr cannot infer visit rows or observation blocks from
  # long data (same-day measurements are ambiguous).
  if (output_format != "long" && output_format != input_format) {
    reshape_it <- if (output_format == "semi_wide") "visit rows" else "observation blocks"
    stop(sprintf(
      "output_format = '%s' requires input_format = '%s': growthcleanr cannot infer %s from %s input. Reshape the long output yourself with data.table::dcast().",
      output_format, output_format, reshape_it, input_format))
  }
  if (output_format != "long" && isTRUE(.checkpoint_driven)) {
    stop(sprintf("Re-widened (%s) output is not available for checkpointed runs; use output_format = 'long'.",
                 output_format))
  }
  .gc_reshape_spec <- NULL   # round-trip spec for re-widened output (Hook B)
  .gc_emit         <- NULL
  .gc_crossformat  <- FALSE  # semi-wide/wide input returned as long output
  .gc_user_id <- .gc_user_id_name <- NULL  # user id per measurement (cross-format)
  .gc_xf_extra <- NULL  # user's other columns per measurement (cross-format)
  .gc_xf_roles <- NULL  # user's subjid/sex/age names (cross-format)
  .gc_xf_factor_warned <- FALSE  # factor value columns already reported (reshape)
  if (input_format != "long") {
    if (is.null(data) || !is.data.frame(data)) {
      stop(sprintf("input_format = '%s' requires a data.frame/data.table in `data`; %s was entered. Pass your table as `data`.",
                   input_format, .gc_desc_val(data)))
    }
    # The spec is built only when the results have to be reshaped back into the
    # user's layout (mirror-mode output).
    want_spec <- identical(output_format, input_format)
    if (want_spec) .gc_emit <- .gc_normalize_emit(widened_emit)

    # The user's id column: the `id` argument, or a column named `id` if there
    # is one (as in long input).
    .gc_rs_id <- if (!is.null(id)) id else if ("id" %in% names(data)) "id" else NULL
    # Both adapters return list(long, route, spec, user_id, user_id_name) so
    # everything below is shared.
    .rs <- if (input_format == "semi_wide") {
      .gc_semiwide_to_long(
        data,
        semiwide_cols  = semiwide_cols,
        col_subjid     = if (is.null(subjid))  "subjid"  else subjid,
        col_sex        = if (is.null(sex))     "sex"     else sex,
        col_agedays    = if (is.null(agedays)) "agedays" else agedays,
        col_dob        = if (is.null(dob))        NULL else dob,
        col_visit_date = if (is.null(visit_date)) NULL else visit_date,
        age_source     = age_source,
        col_id         = .gc_rs_id,
        quietly        = quietly,
        want_spec      = want_spec)
    } else {
      .gc_wide_to_long(
        data,
        wide_cols      = wide_cols,
        col_subjid     = if (is.null(subjid))  "subjid"  else subjid,
        col_sex        = if (is.null(sex))     "sex"     else sex,
        col_agedays    = if (is.null(agedays)) "agedays" else agedays,
        col_dob        = if (is.null(dob))        NULL else dob,
        col_visit_date = if (is.null(visit_date)) NULL else visit_date,
        age_source     = age_source,
        col_id         = .gc_rs_id,
        max_blocks     = max_blocks,
        date_order     = date_order,
        quietly        = quietly,
        want_spec      = want_spec)
    }
    if (want_spec) {
      # Fail fast, before cleaning: a generated column name must not collide
      # with a column the user already has.
      gen_names <- .gc_rewiden_emit_names(.gc_emit, .rs$spec)
      collide   <- intersect(gen_names, names(data))
      if (length(collide) > 0L) {
        stop(sprintf(
          "growthcleanr would add column(s) %s to your data, but your data already contain column(s) with those name(s). Rename them, or map a different value column with `%s`.",
          paste0("'", collide, "'", collapse = ", "),
          if (input_format == "wide") "wide_cols" else "semiwide_cols"))
      }
      .gc_reshape_spec <- .rs$spec
      .gc_reshape_spec$carry_through <- isTRUE(carry_through)
    }
    if (!want_spec) {
      # Long output from semi-wide/wide input. A user column (the id, or a
      # carried column) whose name matches a column growthcleanr returns is
      # returned as userinput_<name>, with a warning -- checked now, before
      # cleaning.
      .gc_crossformat  <- TRUE
      .gc_user_id      <- .rs$user_id
      .gc_user_id_name <- .rs$user_id_name
      .gc_xf_roles <- .rs$role_names
      taken <- c("gc_internal_id", .gc_xf_roles$subjid, "param",
                 .gc_xf_roles$agedays, .gc_xf_roles$sex, "measurement",
                 .gc_added_output_cols(
                   exclude_detail = exclude_detail, tri_exclude = tri_exclude,
                   mean_ht = mean_ht, cf_detail = cf_detail,
                   qm_detail = qm_detail, corr_detail = corr_detail,
                   recenter_detail = recenter_detail,
                   unit_error_range = unit_error_range,
                   display_gc_id = display_gc_id,
                   internal_detail = internal_detail, debug = debug,
                   has_dob = identical(.rs$route, "date"),
                   has_visit_date = identical(.rs$route, "date")))
      extra_cols <- if (isTRUE(carry_through))
        setdiff(names(data), .rs$used_cols) else character(0)
      if (!is.null(.gc_user_id_name)) {
        .gc_user_id_name <- .gc_userinput_names(
          .gc_user_id_name, taken, setdiff(names(data), .rs$user_id_name),
          hint = " or name a different column with `id`")
      }
      if (length(extra_cols) > 0L) {
        .gc_xf_extra <- data.table::as.data.table(data)[.rs$src_row, extra_cols, with = FALSE]
        data.table::setnames(.gc_xf_extra, extra_cols,
          .gc_userinput_names(extra_cols, c(taken, .gc_user_id_name),
                              names(data)))
      }
    }
    # Factor value columns were taken by their labels during the reshape; say
    # so here (the long path then sees text, and stays quiet about it).
    if (length(.rs$factor_cols) > 0L) {
      warning(sprintf(
        "Measurement column(s) %s are factors; growthcleanr converted them to numbers using their labels (as.numeric(as.character(x))), not their level codes. Values that are not numbers are set aside as Exclude-Missing-Info. A factor measurement column usually means some values in the file were not numbers; check those columns.",
        paste0("'", .rs$factor_cols, "'", collapse = ", ")), call. = FALSE)
      .gc_xf_factor_warned <- TRUE
    }
    data <- .rs$long
    # From here the existing long path runs on canonical column names with a
    # valid unique minted `id`. Reset the role args so .col() uses its defaults.
    subjid <- param <- agedays <- sex <- measurement <- id <- NULL
    if (identical(.rs$route, "date")) {
      dob <- "dob"; visit_date <- "visit_date"; age_source <- "dates"
    } else {
      dob <- NULL; visit_date <- NULL; age_source <- NULL
    }
    # The long path never carries extra columns for reshaped input: on
    # cross-format output they are reattached per measurement from .gc_xf_extra
    # (by minted id), and in mirror mode the user's columns come back via the
    # output base (spec$base), not here.
    carry_through <- FALSE
  }

  # Dispatch: data-frame style vs vector style.
  # Data-frame style: `data` is a data.frame/data.table; subjid/param/etc.
  # are column-name strings (defaulting to the canonical names). Extract
  # vectors from `data`.
  # Vector style (legacy): `data` is NULL; subjid/param/etc. are vectors
  # passed in directly.
  .carry_spec <- NULL   # parked carry-through columns (data-frame style only)
  .gc_dob_conflict <- NULL  # rows of subjects with >1 distinct dob (date route only)
  .gc_date_setaside <- NULL # dates treated as blank / unreadable (date route only)
  .gc_show_int_age <- FALSE # ages from dates while the user's age column is returned
  .gc_user_agedays <- NULL  # that user age column, returned as submitted
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop(sprintf(
        "`data` must be a data.frame or data.table, but %s was entered. If this is code written for growthcleanr 2.x: in version 3 the first argument is `data`, and `cleangrowth()` returns a data table rather than a vector. Usually `cleangrowth(your_data)` is all you need. See 'Updating code written for growthcleanr 2.2.3' in vignette(\"experienced-start-here\").",
        .gc_desc_val(data)))
    }
    .col <- function(x, default) {
      if (is.null(x)) return(default)
      if (!is.character(x) || length(x) != 1L || is.na(x)) {
        stop(sprintf(
          "When `data` is provided, `%s` must be a single column name (a length-1 string); %s was entered. Pass the name of the column holding that field.",
          default, .gc_desc_val(x)))
      }
      x
    }
    col_subjid      <- .col(subjid,      "subjid")
    col_param       <- .col(param,       "param")
    col_agedays     <- .col(agedays,     "agedays")
    col_sex         <- .col(sex,         "sex")
    col_measurement <- .col(measurement, "measurement")
    col_id          <- .col(id,          "id")
    col_dob         <- if (is.null(dob))        NULL else .col(dob,        "dob")
    col_visit_date  <- if (is.null(visit_date)) NULL else .col(visit_date, "visit_date")

    # --- Resolve the age source: an `agedays` column, or `dob` + `visit_date`.
    # Rule: an `agedays` column takes precedence whenever present; dates are
    # used only when `agedays` is absent, or when age_source = "dates" forces
    # them. Date-based ages are computed here (before the funnel / setkey), so
    # everything downstream sees a normal numeric `agedays`.
    if (!is.null(age_source)) {
      age_source <- match.arg(age_source, c("agedays", "dates"))
    }
    # dob and visit_date can each be supplied on their own. A lone date column
    # is PARSED for its gc_internal_ audit column (gc_internal_dob /
    # gc_internal_visit_date, surfaced under internal_detail) even when an
    # agedays column drives the algorithm -- e.g. supplying agedays + visit_date
    # so growthcleanr hands back the parsed visit date. Computing agedays FROM
    # dates still needs BOTH; that requirement is enforced per age source below.
    has_dob     <- !is.null(col_dob)
    has_vdate   <- !is.null(col_visit_date)
    has_dates   <- has_dob && has_vdate     # both present -> dates can give agedays
    has_agedays <- col_agedays %in% names(data)
    # Any named date column must actually exist in the data.
    named_date_cols <- c(if (has_dob) col_dob, if (has_vdate) col_visit_date)
    if (length(named_date_cols) > 0L) {
      date_missing <- setdiff(named_date_cols, names(data))
      if (length(date_missing) > 0L) {
        stop(sprintf("growthcleanr does not find the date column(s): %s. Tell growthcleanr which columns hold your dates, e.g. dob = \"your_dob_column\", visit_date = \"your_visit_date_column\".",
                     paste(date_missing, collapse = ", ")))
      }
    }
    # Semi-wide and wide inputs have no `param`/`measurement` column and no single
    # `agedays` column, so they hard-stop somewhere in this validation block when
    # `input_format` was left at the default "long". When the present columns look
    # reshaped, the real fix is `input_format`, not renaming -- build a hint to
    # append to whichever stop fires first (the age check below or the
    # required-column check further down).
    fmt_hint <- ""
    if (input_format == "long") {
      present_nm <- names(data)
      # Wide signal: repeated `<name>_N` block columns (e.g. wt_1, ht_1, agedays_2).
      n_block    <- sum(grepl("_[0-9]+$", present_nm))
      # Semi-wide signal: columns named like a parameter (canonical or informal).
      param_tok  <- c(tolower(.gc_recognized_params),
                      "wt", "ht", "hc", "weight", "height", "length", "head")
      n_paramcol <- sum(tolower(trimws(present_nm)) %in% param_tok)
      if (n_block >= 2L) {
        fmt_hint <- " Some columns are named `<name>_N` (e.g. wt_1, ht_1), which looks like wide input -- if so, set input_format = \"wide\"."
      } else if (n_paramcol >= 1L) {
        fmt_hint <- " Some columns are named like measurements (e.g. weight, height), which looks like semi-wide input -- if so, set input_format = \"semi_wide\"."
      }
    }

    if (identical(age_source, "dates")) {
      if (!has_dates) {
        stop("age_source = 'dates' requires both `dob` and `visit_date` column arguments.")
      }
      use_dates <- TRUE
    } else if (identical(age_source, "agedays")) {
      if (!has_agedays) {
        stop(sprintf("age_source = 'agedays' requires an `agedays` column ('%s').", col_agedays))
      }
      use_dates <- FALSE
    } else {
      # auto-resolve
      if (has_agedays) {
        # Supplying both an agedays column and dob/visit_date is common and
        # expected; growthcleanr silently uses agedays (documented in the
        # vignette). No warning -- set age_source = 'dates' to use the dates.
        # A lone dob or visit_date alongside agedays is fine here too: it is
        # parsed for its gc_internal_ column below but does not drive ages.
        use_dates <- FALSE
      } else if (has_dates) {
        use_dates <- TRUE
      } else if (has_dob || has_vdate) {
        # Only one of dob/visit_date, and no agedays column: growthcleanr cannot
        # compute an age. (A lone date column is a valid parse-only input only
        # when an agedays column supplies the ages.)
        stop("growthcleanr needs both `dob` and `visit_date` to compute agedays from dates, or an `agedays` column to supply ages. Only one date column was supplied and there is no `agedays` column.")
      } else {
        stop(sprintf("No age information found: supply an `agedays` column, or both `dob` and `visit_date` columns.%s", fmt_hint))
      }
    }

    # Ages from dates while the data also has an age column (age_source =
    # "dates"): the user's column is returned as submitted under its own name,
    # and the ages growthcleanr computed are returned as gc_internal_agedays.
    if (use_dates && has_agedays) {
      .gc_show_int_age <- TRUE
      .gc_user_agedays <- data[[col_agedays]]
    }

    required_cols <- c(col_subjid, col_param, col_sex, col_measurement)
    if (!use_dates) required_cols <- c(required_cols, col_agedays)
    missing_cols  <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0L) {
      # A stray space or a capital letter is a common cause; offer a
      # case-/whitespace-insensitive near-match hint for each missing column.
      .norm  <- function(x) tolower(trimws(x))
      hints  <- vapply(missing_cols, function(mc) {
        cand <- names(data)[.norm(names(data)) == .norm(mc)]
        if (length(cand)) sprintf("'%s' (did you mean '%s'?)", mc, cand[1]) else sprintf("'%s'", mc)
      }, character(1))
      # Name the missing ROLE in the example (e.g. sex = "your_sex_column"), not
      # a fixed subjid example. A missing sex column gets a note that the column
      # is required even for adult data, whose sex values may be blank.
      .roles <- c(subjid = col_subjid, param = col_param, sex = col_sex,
                  measurement = col_measurement, agedays = col_agedays)
      .miss_roles <- names(.roles)[match(missing_cols, .roles)]
      .eg <- paste(sprintf("%s = \"your_%s_column\"", .miss_roles, .miss_roles),
                   collapse = ", ")
      .sex_note <- if ("sex" %in% .miss_roles) .gc_sex_col_note else ""
      # `fmt_hint` (built above) points at `input_format` when the present columns
      # look semi-wide/wide -- the likely fix here rather than renaming.
      stop(sprintf(
        "data does not contain required column(s): %s. You can either rename the column, or point growthcleanr at yours, e.g. %s.%s%s",
        paste(hints, collapse = ", "), .eg, .sex_note, fmt_hint
      ))
    }
    # A required column name appearing more than once is silently ambiguous
    # (data[[name]] returns only the first match); stop rather than guess which.
    dup_required <- unique(required_cols[vapply(required_cols,
      function(cn) sum(names(data) == cn) > 1L, logical(1))])
    if (length(dup_required) > 0L) {
      stop(sprintf(
        "data has duplicate column(s) for required field(s): %s. Each required column must appear exactly once.",
        paste(dup_required, collapse = ", ")
      ))
    }

    user_col_names <- list(
      id          = col_id,
      subjid      = col_subjid,
      param       = col_param,
      agedays     = col_agedays,
      sex         = col_sex,
      measurement = col_measurement
    )

    subjid      <- data[[col_subjid]]
    param       <- data[[col_param]]
    sex         <- data[[col_sex]]
    measurement <- data[[col_measurement]]
    id <- if (col_id %in% names(data)) data[[col_id]] else NULL

    # --- Carry-through: park any column gc does NOT consume as a role column,
    # to reattach (appended) on output. Recognized = the six role column names
    # (route-independent), so dob/visit_date are NOT recognized and ride back
    # out here (echoing the submitted dates), while an agedays column is always
    # recognized and never carried (so it cannot collide with output agedays).
    # Parked by input row index (`line`, == seq_along(measurement)); reattached
    # by a keyed merge on `line` before the output trim. Disabled by
    # carry_through = FALSE (restores the prior drop-everything behavior).
    if (isTRUE(carry_through)) {
      recognized <- c(col_subjid, col_param, col_agedays, col_sex,
                      col_measurement, col_id)
      carry_cols <- setdiff(names(data), recognized)
      if (length(carry_cols) > 0L) {
        # A carried column whose name matches a column growthcleanr returns in
        # this run (e.g. `exclude`, when re-cleaning growthcleanr output) is kept
        # and returned under a userinput_ prefix, with a warning. Names of
        # growthcleanr columns this run does not return are left alone.
        .carry_spec <- as.data.table(data)[, carry_cols, with = FALSE]
        taken <- .gc_added_output_cols(
          exclude_detail = exclude_detail, tri_exclude = tri_exclude,
          mean_ht = mean_ht, cf_detail = cf_detail, qm_detail = qm_detail,
          corr_detail = corr_detail, recenter_detail = recenter_detail,
          unit_error_range = unit_error_range, display_gc_id = display_gc_id,
          internal_detail = internal_detail, debug = debug,
          has_dob = has_dob, has_visit_date = has_vdate,
          show_internal_agedays = .gc_show_int_age)
        setnames(.carry_spec, carry_cols,
                 .gc_userinput_names(carry_cols, taken, names(data)))
        # Kept in input row order with no key column: reattached by input row
        # index, so no user column name can collide with a key.
      }
    }

    # Parse any supplied dob/visit_date into Date vectors. This runs whenever a
    # date column is present -- INDEPENDENT of whether dates or an agedays column
    # drive the algorithm -- so a lone visit_date (or dob) supplied alongside an
    # agedays column is still parsed and surfaced as gc_internal_visit_date /
    # gc_internal_dob (under internal_detail). When both are present and the date
    # route is in use, the same parsed vectors also compute agedays below.
    # dob/visit_date may be R Date objects, or text in any accepted format
    # (year-first numeric, English spelled-month, or numeric day/month with
    # date_order).
    #
    # Date parsing lives in the shared internal helpers (R/reshape_adapters.R)
    # so the semi-wide date route reuses them. .gc_date_class() resolves ONE
    # format for the whole run, jointly across whichever of dob/visit_date are
    # present; .gc_parse_date_col() then parses each column under that class.
    # Accepts a Date column as-is; blank (incl. no-digit text) -> NA; a few
    # unreadable dates -> NA, more than a few or a numeric
    # column errors.
    # Reject a numeric (epoch-ambiguous) date column with its targeted message
    # before classification, so the epoch guidance wins over "unrecognized
    # format". .gc_parse_date_col() throws on a numeric column immediately.
    dob_vec <- vdate_vec <- NULL
    if (has_dob || has_vdate) {
      if (has_dob && is.numeric(data[[col_dob]]))
        invisible(.gc_parse_date_col(data[[col_dob]], "dob", col_dob))
      if (has_vdate && is.numeric(data[[col_visit_date]]))
        invisible(.gc_parse_date_col(data[[col_visit_date]], "visit_date", col_visit_date))
      date_cls <- .gc_date_class(
        c(if (has_dob)   as.character(data[[col_dob]]),
          if (has_vdate) as.character(data[[col_visit_date]])),
        date_order, "dob/visit_date")
      # The parser tags each vector with the values it treated as blank or set
      # aside (attribute "gc_date_setaside"); keep those notes and strip the tag
      # so it does not ride into the output columns.
      .date_sa_notes <- list()
      if (has_dob) {
        dob_vec <- .gc_parse_date_col(data[[col_dob]], "dob", col_dob, date_cls, date_order)
        .date_sa_notes$dob <- attr(dob_vec, "gc_date_setaside")
        attr(dob_vec, "gc_date_setaside") <- NULL
        .gc_internal_dob <- dob_vec
      }
      if (has_vdate) {
        vdate_vec <- .gc_parse_date_col(data[[col_visit_date]], "visit_date", col_visit_date, date_cls, date_order)
        .date_sa_notes$vdate <- attr(vdate_vec, "gc_date_setaside")
        attr(vdate_vec, "gc_date_setaside") <- NULL
        .gc_internal_vdate <- vdate_vec
      }
    }

    if (use_dates) {
      # Compute agedays = floor(visit_date - dob) in whole days. NA/blank dates
      # yield NA agedays, which downstream becomes Exclude-Missing(-Info); a
      # visit_date before dob yields a negative agedays, handled as today.
      # A subject with more than one distinct dob (possibly two people under one
      # subjid) cannot be aged reliably: all of that subject's rows get NA agedays
      # (-> Exclude-Missing-Info) and are reported in the consolidated input
      # warning; the rest of the data cleans normally.
      n_dob <- tapply(dob_vec, subjid, function(d) length(unique(d[!is.na(d)])))
      bad_dob <- names(n_dob)[!is.na(n_dob) & n_dob > 1L]
      agedays <- as.integer(floor(as.numeric(vdate_vec - dob_vec, units = "days")))
      if (length(bad_dob) > 0L) {
        .gc_dob_conflict <- as.character(subjid) %in% bad_dob
        agedays[.gc_dob_conflict] <- NA_integer_
      }
      # Date values treated as blank (no digits, e.g. "N/A") or set aside as
      # unreadable (e.g. "2013-02-30") by .gc_parse_date_col(); reported in the
      # consolidated input warning.
      .n_rows <- length(agedays)
      .tok <- .badd <- rep(FALSE, .n_rows)
      .tok_vals <- .bad_vals <- character(0)
      for (.a in .date_sa_notes) {
        if (is.null(.a)) next
        .tok[.a$token] <- TRUE; .badd[.a$bad] <- TRUE
        .tok_vals <- union(.tok_vals, .a$token_vals)
        .bad_vals <- union(.bad_vals, .a$bad_vals)
      }
      if (any(.tok | .badd))
        .gc_date_setaside <- list(token = .tok, token_vals = .tok_vals,
                                  bad = .badd, bad_vals = .bad_vals)
    } else {
      agedays <- data[[col_agedays]]
    }

    # [length-semiwide-block] Recumbent length vs standing height is a
    # long-format-only distinction. In semi-wide/wide input a LENGTHCM/LENGTHIN
    # value at any age gets default handling (relabeled HEIGHTCM/HEIGHTIN below,
    # exactly as in long input with length.adjust = FALSE); only
    # length.adjust = TRUE is refused for semi-wide/wide (above).
  } else {
    if (!is.null(dob) || !is.null(visit_date) || !is.null(age_source) || !is.null(date_order)) {
      stop("`dob`, `visit_date`, `age_source`, and `date_order` are only supported in data-frame style (pass a `data` argument). In vector style, supply `agedays` directly.")
    }
    if (is.null(subjid) || is.null(param) || is.null(agedays) ||
        is.null(sex) || is.null(measurement)) {
      stop("Must provide either `data` (data.frame), or all of: subjid, param, agedays, sex, measurement.")
    }
    # Capture user's original argument names for output column renaming.
    # e.g., cleangrowth(subjid = dt$patid, ...) -> output column named "patid".
    # Handles: dt$col -> "col"; myvar -> "myvar"; anything else -> keep canonical.
    mc <- match.call()
    .extract_argname <- function(expr) {
      if (is.null(expr)) return(NULL)
      if (is.call(expr) && identical(expr[[1L]], as.name("$")))
        return(as.character(expr[[3L]]))
      if (is.call(expr) && identical(expr[[1L]], as.name("[["))) {
        idx <- expr[[3L]]; if (is.character(idx)) return(idx); return(NULL)
      }
      if (is.name(expr)) return(as.character(expr))
      NULL
    }
    user_col_names <- list(
      id          = .extract_argname(mc[["id"]]),
      subjid      = .extract_argname(mc[["subjid"]]),
      param       = .extract_argname(mc[["param"]]),
      agedays     = .extract_argname(mc[["agedays"]]),
      sex         = .extract_argname(mc[["sex"]]),
      measurement = .extract_argname(mc[["measurement"]])
    )
  }

  # --- Structural input audit (both calling styles). Runs on the assembled
  # required-field vectors, before id handling / assembly.
  #   * zero rows            -> clean stop (was an internal "[line]" crash)
  #   * factor `measurement` -> converted through its labels, with a warning
  #     (as.numeric(factor) would give level codes)
  #   * character `measurement` -> one-time note + numeric coercion (kept lenient;
  #     non-numeric / empty strings become NA -> Exclude-Missing-Info). Coercion
  #     runs BEFORE the all-NA check so an all-empty-string column is caught.
  #   * a required column all-NA -> clean stop (no data to clean in that field)
  #   * any NA in a key column -> warning; those rows become Exclude-Missing-Info
  #     (structural keys subjid/param are set aside below; NA agedays/measurement
  #     are resolved to Exclude-Missing-Info by the algorithm)
  # NB: uses literal NA only, on the verbatim vectors, so an unrecognized-but-
  # non-NA sex coding (e.g. "Male") is NOT treated as all-NA here -- broadening
  # accepted sex/param codings is tracked separately.
  if (length(measurement) == 0L) {
    stop("Input has zero rows: there is nothing to clean.")
  }
  # A text or factor measurement is returned as submitted (.meas_verbatim);
  # cleaning uses the numbers converted below.
  .meas_verbatim <- if (is.factor(measurement) || is.character(measurement)) measurement else NULL
  .meas_was_factor <- is.factor(measurement)
  if (.meas_was_factor) {
    # Convert a factor through its labels: as.numeric() on a factor returns the
    # level CODES (1,2,3...), not the labels, which would be a silent wrong
    # answer. as.character() first, then the character path below.
    warning("`measurement` is a factor; growthcleanr converted it to numbers using its labels (as.numeric(as.character(x))), not its level codes. Values that are not numbers are set aside as Exclude-Missing-Info. A factor measurement column usually means some values in the file were not numbers; check that column.",
            call. = FALSE)
    measurement <- as.character(measurement)
  }
  if (is.character(measurement)) {
    if (!.meas_was_factor && !.gc_xf_factor_warned)
      message("growthcleanr: `measurement` supplied as character; coercing to numeric ",
              "(non-numeric strings become NA -> Exclude-Missing-Info).")
    measurement <- suppressWarnings(as.numeric(measurement))
  }
  # agedays types other than numeric. The submitted value is kept verbatim for
  # output (.agedays_verbatim); cleaning uses the converted number of days.
  #   - difftime (e.g. from subtracting dates or date-times): converted to days
  #     in its own units, so a difference in hours or weeks is not read as days.
  #   - factor: converted through its labels, never its level codes (warning).
  #   - character: converted to numbers; non-numbers -> NA -> Exclude-Missing-Info.
  .agedays_verbatim <- if (!is.null(.gc_user_agedays)) .gc_user_agedays else agedays
  if (inherits(agedays, "difftime")) {
    .age_units <- units(agedays)
    agedays <- as.numeric(agedays, units = "days")
    if (!identical(.age_units, "days"))
      warning(sprintf("`agedays` is a time difference in %s; growthcleanr converted it to days for cleaning (for example, 48 hours is 2 days). Ages are returned as submitted.",
                      .age_units), call. = FALSE)
  }
  if (is.factor(agedays)) {
    warning("`agedays` is a factor; growthcleanr converted it to numbers using its labels (as.numeric(as.character(x))), not its level codes. Values that are not numbers are set aside as Exclude-Missing-Info. A factor agedays column usually means some values in the file were not numbers; check that column.",
            call. = FALSE)
    agedays <- as.character(agedays)
  }
  if (is.character(agedays)) agedays <- suppressWarnings(as.numeric(agedays))
  # Default mean_ht: returned only when the data has adult rows, so child-only
  # output has no empty column. Decided here, once, on the whole dataset, so every
  # checkpoint gets the same setting. A user column of that name has kept its name
  # so far; rename it now that the column is known to be returned.
  if (.mean_ht_auto && is.numeric(agedays) && is.numeric(adult_cutpoint) &&
      length(adult_cutpoint) == 1L && !is.na(adult_cutpoint) &&
      any(!is.na(agedays) &
            agedays >= min(max(adult_cutpoint, 18), 20) * 365.25)) {
    mean_ht <- TRUE
    if (!is.null(.carry_spec) && "mean_ht" %in% names(.carry_spec))
      setnames(.carry_spec, names(.carry_spec),
               .gc_userinput_names(names(.carry_spec), "mean_ht", names(data)))
    if (!is.null(.gc_user_id_name) && identical(.gc_user_id_name, "mean_ht"))
      .gc_user_id_name <- .gc_userinput_names(.gc_user_id_name, "mean_ht", names(data),
                                              hint = " or name a different column with `id`")
    if (!is.null(.gc_xf_extra) && "mean_ht" %in% names(.gc_xf_extra))
      setnames(.gc_xf_extra, names(.gc_xf_extra),
               .gc_userinput_names(names(.gc_xf_extra), "mean_ht", names(data)))
  }
  .key_cols <- list(subjid = subjid, param = param, agedays = agedays,
                    sex = sex, measurement = measurement)
  # All-NA is a hard error for subjid, param, measurement, and agedays -- an
  # entirely-missing one of these means there is nothing gc can clean. (An all-NA
  # `sex` is handled separately below: pediatric rows need a value in the sex
  # column, adult rows do not use it.)
  # Runs after the character coercion above, so an all-empty-string measurement
  # column (coerced to all-NA) is caught here too.
  .req_nonempty <- c("subjid", "param", "measurement", "agedays")
  .all_na <- vapply(.key_cols[.req_nonempty], function(x) all(is.na(x)), logical(1))
  if (any(.all_na)) {
    stop(sprintf(
      "Required column(s) contain no non-missing values: %s. Every required field must have at least one non-NA value.%s",
      paste(.req_nonempty[.all_na], collapse = ", "),
      if (.all_na[["agedays"]] && !is.null(.gc_dob_conflict) && all(.gc_dob_conflict))
        " Every subject has more than one distinct dob value, so no ages could be computed; check the dob column." else ""
    ))
  }

  # --- Consolidated input warning (Option B): one warning after all input
  # checks, with up to two labeled sections ("Unusable data" set-aside bullets +
  # "Generated ids"). Hard stops stay immediate; these warnings
  # are never quietly-gated. The set-aside bullets (blank required fields,
  # unrecognized params/sex, zero/negative measurements, negative age) are
  # detected once, dataset-wide, by .gc_detect_setaside() just before the flush
  # below (and by the checkpoint outer branch for a checkpointed run), so
  # denominators are dataset-wide and the lead line counts the union of distinct
  # set-aside rows (no double-count when a row trips two checks). Only
  # `.gc_adjusted` (the id auto-regen notice) is accumulated inline, next to the
  # id regen itself.
  .gc_adjusted <- character(0)
  .n_input     <- length(measurement)

  # id is fully optional. If missing/NULL/NA/non-unique, warn and auto-generate
  # sequential ids (1:N in input row order). The user's original `id` (whatever
  # they supplied - even if NA/duplicate) is preserved in the output `id` column
  # so they can map results back to their data; the auto-generated id that gc
  # actually used internally is surfaced as `gc_internal_id` when `display_gc_id = TRUE`
  # or `full_detail = TRUE`. user_id_orig is the snapshot for restoration.
  #
  # This must run before the partial-run logic (which slices id alongside other
  # vectors) and before the data.table assembly below.
  user_id_orig   <- id
  id_was_regened <- FALSE  # TRUE only when we replaced a user-supplied id (NA/dup)
                           # with the auto-regen - used at output trim to decide
                           # whether to restore user_id_orig. NOT set in the
                           # NULL branch (nothing to restore) or the valid-id
                           # branch (no regen happened).
  if (is.null(id) || length(id) == 0L) {
    .gc_adjusted <- c(.gc_adjusted,
      "No `id` was supplied; growthcleanr assigned sequential ids 1:N in input row order.")
    id <- seq_along(measurement)
  } else if (anyNA(id)) {
    .gc_adjusted <- c(.gc_adjusted, sprintf(
      "`id` has %d missing (NA) value(s); growthcleanr assigned sequential ids 1:N in input row order. Your original id is kept in the output `id` column; the id growthcleanr used is in `gc_internal_id` (set display_gc_id = TRUE or full_detail = TRUE).",
      sum(is.na(id))))
    id <- seq_along(measurement)
    id_was_regened <- TRUE
  } else if (anyDuplicated(id)) {
    dup_ids <- unique(id[duplicated(id)])
    ex <- dup_ids[seq_len(min(5L, length(dup_ids)))]
    .gc_adjusted <- c(.gc_adjusted, sprintf(
      paste0("`id` has %d duplicated value(s) (%s%s); growthcleanr assigned sequential ids 1:N in input row order. ",
             "A common cause is passing a per-subject/encounter id instead of a per-row id. ",
             "Your original id is kept in `id`; growthcleanr's id is in `gc_internal_id` (display_gc_id = TRUE / full_detail = TRUE)."),
      length(dup_ids),
      paste(ex, collapse = ", "),
      if (length(dup_ids) > 5L) ", ..." else ""
    ))
    id <- seq_along(measurement)
    id_was_regened <- TRUE
  }

  # Validate cf_rescue. match.arg() alone gives "'arg' must be NULL or a character
  # vector" for cf_rescue = TRUE (a 2.x-style include.carryforward habit), so check
  # first and say what is accepted; unambiguous abbreviations still match.
  # Case-insensitive, like adult_permissiveness.
  .cf_modes <- c("standard", "none", "all")
  if (is.character(cf_rescue)) cf_rescue <- tolower(trimws(cf_rescue))
  if (!(is.character(cf_rescue) && length(cf_rescue) == 1L && !is.na(cf_rescue) &&
        !is.na(pmatch(cf_rescue, .cf_modes)))) {
    stop("cf_rescue must be one of \"standard\", \"none\", or \"all\". The default is \"standard\", and TRUE/FALSE is not accepted. For 2.x include.carryforward = FALSE (the 2.2.3 default), the closest is \"none\"; for TRUE, \"all\".",
         call. = FALSE)
  }
  cf_rescue <- match.arg(cf_rescue, .cf_modes)

  # log.path is deprecated (it wrote per-batch logs only in parallel, non-quiet
  # runs); still honored, but warn once at the top-level call.
  if (!isTRUE(.checkpoint_driven) && !(length(log.path) == 1L && is.na(log.path)))
    warning("log.path is deprecated and no longer used in most setups; use log_file instead (written whenever checkpointing engages).",
            call. = FALSE)

  # --- Checkpointed run (folded from cleangrowth_checkpoint) ----------------
  # ---- Resolve the checkpoint target (the `checkpoint` switch + `checkpoint_dir`) ----
  # Turns the user-facing `checkpoint` / `checkpoint_dir` pair into a concrete
  # checkpoint_dir (or NULL for an in-memory run) plus the keep/resume flags the
  # dispatch below reads. Rules, in priority order:
  #   * Nested per-checkpoint call (.checkpoint_driven): never re-checkpoint.
  #   * checkpoint = FALSE: off, full stop (the "nothing on disk" escape hatch).
  #     A checkpoint_dir passed alongside it is ignored, with a warning.
  #   * checkpoint_dir supplied: honor it -- persistent, resumable, keep_checkpoints
  #     as given.
  #   * checkpoint = TRUE (default), no dir: ephemeral. Write to a per-session temp
  #     directory and delete the files after combining (keep_checkpoints = FALSE).
  #     Engage ONLY when the data exceeds one checkpoint (> checkpoint_size subjects):
  #     a single checkpoint is the whole dataset in memory and gives no benefit, so
  #     small/typical data runs fully in memory with nothing written to disk.
  if (!(length(checkpoint) == 1L && is.logical(checkpoint) && !is.na(checkpoint)))
    stop(sprintf("checkpoint must be TRUE or FALSE; %s was entered.",
                 .gc_desc_val(checkpoint)))
  if (isTRUE(.checkpoint_driven)) {
    checkpoint_dir <- NULL
  } else if (!checkpoint) {
    if (!is.null(checkpoint_dir))
      warning("checkpoint = FALSE overrides checkpoint_dir; no checkpoint files will be written.",
              call. = FALSE)
    checkpoint_dir <- NULL
  } else if (is.null(checkpoint_dir)) {
    # Ephemeral default. Engage only if checkpointing would actually split the data.
    # If checkpoint_size is invalid, engage so the dispatch below emits the proper error.
    .ckpt_size_ok <- is.numeric(checkpoint_size) && length(checkpoint_size) == 1L &&
      !is.na(checkpoint_size) && checkpoint_size > 0
    if (!.ckpt_size_ok || data.table::uniqueN(subjid) > checkpoint_size) {
      checkpoint_dir    <- file.path(tempdir(), "growthcleanr_checkpoints")
      keep_checkpoints  <- FALSE
      checkpoint_resume <- FALSE
      # Remove the temporary files however the run ends, including an error or
      # an interrupt. A crash or kill cannot run this.
      .ephemeral_ckpt_dir <- checkpoint_dir
      on.exit(unlink(.ephemeral_ckpt_dir, recursive = TRUE), add = TRUE)
    }
  }

  # sd.recenter: accept NA, "none", or a table (a data.frame is converted and
  # copied); stop on anything else (e.g. the removed 2.x strings). Done before
  # the checkpoint split so the checkpoint runner forwards the normalized value.
  sd.recenter <- .gc_normalize_sd_recenter(sd.recenter)

  # When checkpoint_dir is set, split subjects into checkpoints, clean each checkpoint with
  # a nested cleangrowth(checkpoint_dir = NULL) call in long mode, persist each
  # checkpoint to a checkpoint_NNNN.rds file, skip completed checkpoints on resume, then
  # combine. Checkpointed results are byte-identical to a single-pass run: every
  # subject's rows land in one checkpoint (split by subjid) and internal_id
  # tie-breaking is within-subject, so per-checkpoint renumbering changes nothing. The
  # input funnel, date parsing, and id regen above have already run once, so each
  # checkpoint receives canonical long data with a valid unique id and does no
  # re-regen. Re-widen (Hook B) and the user-name rename run once here on the
  # combined long result.
  if (!is.null(checkpoint_dir)) {
    checkpoint_size_provided <- !missing(checkpoint_size)

    if (!is.numeric(checkpoint_size) || length(checkpoint_size) != 1L || checkpoint_size <= 0)
      stop(sprintf("checkpoint_size must be a single positive number; %s was entered.",
                   .gc_desc_val(checkpoint_size)))
    if (!is.null(log_file) && !(is.character(log_file) && length(log_file) == 1L))
      stop(sprintf("log_file must be a single file path (a length-1 string) or NULL; %s was entered.",
                   .gc_desc_val(log_file)))
    if (!(length(checkpoint_resume) == 1L && is.logical(checkpoint_resume) && !is.na(checkpoint_resume)))
      stop(sprintf("checkpoint_resume must be TRUE or FALSE; %s was entered.",
                   .gc_desc_val(checkpoint_resume)))

    # Run the dataset-wide input triage ONCE here (on the full raw input). It does
    # two jobs for the checkpointed run:
    #   1. Emits the consolidated input warning once. The per-checkpoint calls detect
    #      the same issues but are gated silent (.checkpoint_driven), so without
    #      this the whole warning -- set-aside bullets AND the id auto-regen notice
    #      -- would be lost or fragmented per checkpoint. Denominators are dataset-wide
    #      and the text is identical to a non-checkpointed run. .gc_adjusted (id
    #      regen) was computed once, above.
    #   2. Clamps adult_cutpoint dataset-wide (.ckpt_det$cutpoint) and forwards it
    #      to every checkpoint below, so the checkpoints apply the same cutpoint a single
    #      run would. (growthcleanr does not auto-lower the cutpoint, so this is
    #      just the clamp -- a dataset-independent value.)
    .gc_warn_cutpoint_clamp(adult_cutpoint)
    .ckpt_det <- .gc_detect_setaside(
      subjid, param, agedays, sex, measurement,
      param_coding, sex_coding, adult_cutpoint, n_input = .n_input,
      dob_conflict = .gc_dob_conflict, date_setaside = .gc_date_setaside)
    .gc_stop_if_one_two_sex(sex, agedays, .ckpt_det$cutpoint, sex_coding)
    .ckpt_wmsg <- .gc_format_input_warning(
      .ckpt_det$bullets, .ckpt_det$lines, .gc_adjusted, .n_input)
    if (length(.ckpt_wmsg) > 0L)
      warning(paste(.ckpt_wmsg, collapse = "\n"), call. = FALSE)
    # Advisory (never changes the cutpoint): suggest adult_cutpoint = 18 when the
    # data looks adult-only. Same emission as the non-checkpoint body.
    .gc_adult_cutpoint_advisory(agedays, missing(adult_cutpoint))
    .gc_warn_all_one_sex(sex, agedays, subjid, .ckpt_det$cutpoint, sex_coding)
    # A user-supplied sd.recenter must cover every evaluable child row; checked
    # once here on the whole dataset so the count is dataset-wide.
    if (is.data.table(sd.recenter))
      .gc_check_recenter_coverage(sd.recenter, subjid, param, agedays, sex,
                                  measurement, param_coding, sex_coding,
                                  .ckpt_det$cutpoint)

    # Carry-through of extra (non-growthcleanr) columns: the per-checkpoint calls run
    # on canonical columns only (`.carry_spec` is parked here and reattached to
    # the combined long result below, after .gc_checkpoint_run returns). Nothing
    # to guard against here -- the reattach happens once, outside the checkpoint loop.

    # Canonical long table for checkpointing; raw values (each checkpoint re-normalizes via
    # the forwarded sex_coding / param_coding). id is the valid unique id from
    # the regen block above.
    .ckpt_long <- data.table::data.table(
      id          = id,
      subjid      = subjid,
      param       = param,
      agedays     = agedays,
      sex         = sex,
      measurement = measurement)

    # Every algorithm-affecting + output-detail argument, forwarded to each checkpoint
    # so per-checkpoint output matches a single-pass run column-for-column. Date args
    # are NOT forwarded (agedays is already numeric); carry_through is FALSE
    # (canonical columns only, guarded above); input/output_format and
    # checkpoint_dir are set by the runner.
    .ckpt_clean_args <- list(
      piv.z.wt.high        = piv.z.wt.high,
      piv.z.ht.low.young   = piv.z.ht.low.young,
      piv.z.ht.low.old     = piv.z.ht.low.old,
      piv.z.ht.high        = piv.z.ht.high,
      piv.z.hc.low         = piv.z.hc.low,
      piv.z.hc.high        = piv.z.hc.high,
      error.load.mincount  = error.load.mincount,
      error.load.threshold = error.load.threshold,
      sd.recenter          = sd.recenter,
      cf_rescue            = cf_rescue,
      cf_detail            = cf_detail,
      corr_detail          = corr_detail,
      recenter_detail      = recenter_detail,
      qm_detail            = qm_detail,
      ref.data.path        = ref.data.path,
      log.path             = log.path,
      parallel             = parallel,
      cores                = cores,
      # Forward the dataset-wide FINALIZED cutpoint (not the raw arg): the checkpoints
      # receive it explicitly, so they neither re-clamp nor re-auto-default, and a
      # checkpointed run cleans 18-to-20-year-olds exactly as a single run would.
      adult_cutpoint       = .ckpt_det$cutpoint,
      adult_permissiveness = adult_permissiveness,
      adult_scale_max_lbs  = adult_scale_max_lbs,
      ewma_window          = ewma_window,
      adult_overrides      = adult_overrides,
      sex_coding           = sex_coding,
      param_coding        = param_coding,
      carry_through        = FALSE,
      max_blocks           = max_blocks,
      internal_detail      = internal_detail,
      tri_exclude          = tri_exclude,
      debug                = debug,
      full_detail          = full_detail,
      unit_error_range     = unit_error_range,
      display_gc_id        = display_gc_id,
      exclude_detail       = exclude_detail,
      mean_ht              = mean_ht,
      batch_size           = batch_size,
      length.adjust        = length.adjust,
      ref_tables           = ref_tables)

    combined <- .gc_checkpoint_run(
      long_dt             = .ckpt_long,
      clean_args          = .ckpt_clean_args,
      checkpoint_dir      = checkpoint_dir,
      checkpoint_size          = checkpoint_size,
      checkpoint_size_provided = checkpoint_size_provided,
      checkpoint_resume   = checkpoint_resume,
      keep_checkpoints    = keep_checkpoints,
      log_file            = log_file,
      quietly             = quietly,
      verbose             = verbose)

    # The checkpoints cleaned the converted values; return the submitted ones
    # (combined is in input row order, so positional assigns align): a text or
    # factor measurement, the age column as submitted (e.g. difftime, or the
    # user's own age column when ages came from dates), and in that last case
    # the computed ages as gc_internal_agedays -- as a single run does.
    if (nrow(combined) == length(measurement)) {
      if (!is.null(.meas_verbatim)) combined[, measurement := .meas_verbatim]
      if (.gc_show_int_age && !"gc_internal_agedays" %in% names(combined))
        combined[, gc_internal_agedays := as.integer(floor(agedays))]
      combined[, agedays := .agedays_verbatim]
    }

    # --- Outer finalization (once, on the combined long result) --------------
    # Restore the user's original id (combined is in input row order, so a
    # positional assign aligns). gc_internal_id already holds the id gc used.
    if (id_was_regened && !is.null(user_id_orig)) {
      if (length(user_id_orig) == nrow(combined)) {
        combined[, id := user_id_orig]
      } else {
        warning("Could not restore the user-supplied `id` under checkpointing: length mismatch. The auto-generated id is retained in `id`.")
      }
    }
    # Always-on cleaning summary (checkpoint outer branch; the per-checkpoint calls
    # are .checkpoint_driven and stay silent). `combined` is the full long result
    # in input-row order, so .ckpt_det$lines (dataset-wide set-aside indices) map
    # positionally. gc_internal_param is present only under internal_detail; fall
    # back to the (verbatim) `param` column otherwise. Emitted here while column
    # names are still canonical and before re-widen. Same rationale as the
    # standalone emit -- shown at minimal/verbose, gated on `!quietly` so a
    # fully quiet run is silent.
    .ckpt_grp_src <- if ("gc_internal_param" %in% names(combined))
      as.character(combined$gc_internal_param) else as.character(combined$param)
    .ckpt_grp <- vapply(.ckpt_grp_src, .gc_param_group, character(1), USE.NAMES = FALSE)
    if (!quietly)
      message(paste(.gc_cleaning_summary_lines(
        .ckpt_grp, as.character(combined$exclude),
        seq_len(nrow(combined)) %in% .ckpt_det$lines, .n_input), collapse = "\n"))
    if (.gc_vector_style) .gc_vector_style_note()

    # Rename canonical output columns back to the user's names (no-op when they
    # already match, e.g. reshaped input where the names are canonical).
    for (canon_nm in c("id", "subjid", "param", "agedays", "sex", "measurement")) {
      user_nm <- user_col_names[[canon_nm]]
      if (!is.null(user_nm) && user_nm != canon_nm && canon_nm %in% names(combined)) {
        setnames(combined, canon_nm, user_nm)
      }
    }
    # Reattach carry-through (non-growthcleanr) columns, appended after the
    # growthcleanr columns -- AFTER the rename, as in a single run, so a carried
    # column whose name is a canonical role name the user remapped (e.g. a
    # column `subjid` when subjid = "patid") is not confused with growthcleanr's
    # column of that name. `combined` and `.carry_spec` are both in input row
    # order with one row per input row, so the columns align positionally.
    if (!is.null(.carry_spec) && ncol(.carry_spec) > 0L) {
      if (nrow(.carry_spec) != nrow(combined))
        stop("Internal error: carried columns do not align with the checkpointed result.", call. = FALSE)
      for (nm in names(.carry_spec)) combined[, (nm) := .carry_spec[[nm]]]
    }
    # Long output from semi-wide/wide input: user id under its own name, minted
    # id as gc_internal_id.
    if (.gc_crossformat)
      combined <- .gc_crossformat_id_cols(combined, .gc_user_id, .gc_user_id_name,
                                          .gc_xf_extra, .gc_xf_roles)
    # Re-widen (Hook B) once, if the input arrived wide / semi-wide.
    if (!is.null(.gc_reshape_spec)) {
      refn <- if (identical(.gc_reshape_spec$format, "wide")) .gc_wide_output else .gc_semiwide_output
      combined <- refn(combined, .gc_reshape_spec, emit = .gc_emit,
                       carry_through = isTRUE(.gc_reshape_spec$carry_through))
    }
    return(combined)
  }

  # avoid "no visible binding" warnings
  N <- age_years <- batch <- exclude <- index <- line <- NULL
  newbatch <- rcmedian.sd <- sd.orig <- tanner.months <- tbc.sd <- NULL
  v <- v_adult <- whoagegrp.ht <- whoagegrp_ht <- NULL
  sd.orig_cdc <- sd.orig_who <- NULL
  result <- cf_status <- NULL
  imperial <- cf_dage_orig <- cf_dage_next <- NULL
  gc_internal_dob <- gc_internal_visit_date <- NULL

  agemonths <- ageyears_2b <- intwt <- fengadays <- pmagedays <- cagedays <-
    unmod_zscore <- fen_param <- M <- S_upper <- S_lower <- cwho_cv <- ccdc_cv <-
    sd.c_cdc <- sd.c_who <- sd.c <- sd.corr <- seq_win <- sd.corr_abssumdiff <-
    fw_agedays <- ht_z <- potcorr_wt <- potcorr <-
    sd.orig_abssumdiff <- orig_colnames <- ctbc.sd <- sum_sde <- no_sde <-
    p_plus <- p_minus <- sd.corr_plus <- sd.corr_minus <-
    tbc.p_plus <- tbc.p_minus <- ctbc.p_plus <- ctbc.p_minus <-
    corr_val <- corr_pt <- tmp_is_adult <- tmp_corr_val <- tmp_subj_corr <-
    sex_input <- gc_internal_sex <- gc_internal_id <-
    param_input <- gc_internal_param <- subjid_input <-
    agedays_input <- gc_internal_agedays <-
    NULL

  # preprocessing ----

  # Flexible sex coding. Recode the submitted sex to
  # gc's integer sex (0/1/NA) via the (default or user) sex_coding table.
  # .gc_recode_sex validates sex_coding (errors if a token is under both sexes).
  # Helper + defaults live in R/reshape_adapters.R; the verbatim sex is preserved
  # as sex_input below. The sex-coding GUARDS (1/2 hard stop, unrecognized-value
  # warning) are pediatric-aware and run after the age cutpoint is known -- see
  # the pediatric-sex block below -- because adult rows clean without sex, so
  # neither a 1/2 misclassification nor a missing sex matters for them.
  sex_int <- .gc_recode_sex(sex, sex_coding)

  # organize data into a dataframe along with a line "index" so the original data order can be recovered
  # Output fidelity: keep the user's verbatim submitted values alongside gc's
  # interpreted working values, so the user always gets their data back unaltered.
  #   v_adult     = the verbatim measurement (incl. 0 and original imperial units);
  #                 v is gc's internal working value (0 -> NaN for missingness).
  #   sex_input   = the verbatim sex as submitted ("M"/"U"/9/0/1/...); sex is gc's
  #                 coerced integer (0/1/NA) used by the algorithm.
  #   param_input = the verbatim param as submitted ("WT"/"HEIGHTIN"/...); param
  #                 is gc's resolved canonical token (mapped labels resolved to one of the
  #                 recognized params) used by the algorithm.
  #   subjid_input = the verbatim subjid as submitted (original type preserved);
  #                 subjid is factored internally for grouping/keying.
  #   agedays_input = the verbatim agedays as submitted (e.g. fractional 1435.4);
  #                 agedays is gc's whole-day integer, used for all cleaning.
  # The verbatim columns are restored as the primary output columns at the output
  # trim/rename stage; the interpreted values move to the optional/debug set.
  data.all.ages <- data.table(
    line = seq_along(measurement),
    subjid = as.factor(subjid),
    subjid_input = subjid,
    param = .gc_recode_param(param, param_coding),
    param_input = param,
    # Whole days by floor() (as the date route does), so a fractional
    # negative age such as -0.5 is day -1, not day 0.
    agedays = as.integer(floor(agedays)),
    agedays_input = .agedays_verbatim,
    v = ifelse(measurement == 0, NaN, measurement),
    v_adult = measurement,
    sex = sex_int,
    sex_input = sex
  )
  #   meas_input = the measurement as submitted, kept only when it was text or a
  #                factor (v_adult holds its numeric conversion).
  if (!is.null(.meas_verbatim)) data.all.ages[, meas_input := .meas_verbatim]

  # Surface gc's parsed dob/visit_date (Date) as gc_internal_ columns whenever the
  # corresponding column was supplied (date route or an agedays-driven run).
  # Attached now in input-row order (aligned with `line`) so they ride through the
  # setkey/notgc-split below with the rest of the row; emitted only under
  # internal_detail/full_detail at the output trim. Independent: a lone visit_date
  # adds gc_internal_visit_date without gc_internal_dob.
  if (!is.null(.gc_internal_dob) && length(.gc_internal_dob) == nrow(data.all.ages)) {
    data.all.ages[, gc_internal_dob := .gc_internal_dob]
  }
  if (!is.null(.gc_internal_vdate) && length(.gc_internal_vdate) == nrow(data.all.ages)) {
    data.all.ages[, gc_internal_visit_date := .gc_internal_vdate]
  }

  # Keep user's id as 'id', create 'internal_id' for R processing.
  # internal_id is a sequential integer assigned AFTER sorting by the canonical key
  # (subjid, param, agedays, id). This ensures internal_id reflects id-sorted order,
  # so results are deterministic regardless of input row order.
  # id auto-generation (when omitted) is handled at the top of cleangrowth();
  # only a length-mismatch check remains here.
  if (length(id) != length(measurement)) {
    stop(sprintf("You supplied %d id value(s) but %d measurement(s); there must be exactly one id for each measurement (one per row). Supply one id per measurement, or leave `id` out and growthcleanr will number the rows for you.",
                 length(id), length(measurement)))
  }
  data.all.ages[, id := id]
  setkey(data.all.ages, subjid, param, agedays, id)
  data.all.ages[, internal_id := seq_len(.N)]

  # quality checks
  if (!is.numeric(adult_cutpoint)){
    stop(sprintf("adult_cutpoint must be a number between 18 and 20; %s was entered. (A number below 18 or above 20 is moved to the nearest of 18 or 20.)",
                 .gc_desc_val(adult_cutpoint)))
  }
  if (!(is.numeric(adult_scale_max_lbs) && length(adult_scale_max_lbs) == 1L && !is.na(adult_scale_max_lbs) && adult_scale_max_lbs > 0)){
    stop(sprintf("adult_scale_max_lbs must be a single positive number, or Inf for no scale cap; %s was entered. Pass a positive value (default Inf).",
                 .gc_desc_val(adult_scale_max_lbs)))
  }
  if (!(is.numeric(batch_size) && length(batch_size) == 1L && !is.na(batch_size) && batch_size >= 1)){
    stop(sprintf("batch_size must be a single positive whole number; %s was entered. Pass a positive value (default 5000).",
                 .gc_desc_val(batch_size)))
  }
  # ewma_window feeds the EWMA windowing directly (delta[pos_dist > ewma_window] <- 0)
  # as a vectorized comparison, so a hostile value silently corrupts the window
  # rather than erroring -- validate it here like the other numeric knobs. Must be
  # a single finite number >= 1 (measurements on each side of the point).
  if (!(is.numeric(ewma_window) && length(ewma_window) == 1L && !is.na(ewma_window) && is.finite(ewma_window) && ewma_window >= 1)){
    stop(sprintf("ewma_window must be a single whole number >= 1 (the count of measurements on each side included in the exponentially weighted moving average); %s was entered. Pass a positive value (default 15).",
                 .gc_desc_val(ewma_window)))
  }
  # adult_overrides: optional named list overriding individual adult preset
  # sub-parameters, spliced into the cleanadult() call below. Validate the
  # container + names here (a typo'd name would otherwise be silently dropped or
  # hit cleanadult's "unused argument"); each value's type is validated downstream
  # in resolve_permissiveness() against the preset it replaces.
  if (!is.null(adult_overrides)) {
    if (!is.list(adult_overrides) || length(adult_overrides) == 0L ||
        is.null(names(adult_overrides)) || any(names(adult_overrides) == "")) {
      stop(sprintf("adult_overrides must be NULL or a non-empty named list (e.g. list(overall_ht_min = 120)); %s was entered.",
                   .gc_desc_val(adult_overrides)))
    }
    if (anyDuplicated(names(adult_overrides))) {
      .dup_ov <- unique(names(adult_overrides)[duplicated(names(adult_overrides))])
      stop(sprintf("adult_overrides has duplicated name(s): %s. Give each adult setting at most once.",
                   paste(.dup_ov, collapse = ", ")))
    }
    .valid_adult_overrides <- names(permissiveness_presets()[["looser"]])
    .bad_ov <- setdiff(names(adult_overrides), .valid_adult_overrides)
    if (length(.bad_ov) > 0L) {
      .dedicated <- intersect(.bad_ov, c("permissiveness", "scale_max_lbs", "ewma_window"))
      stop(sprintf("adult_overrides has unrecognized name(s): %s. Recognized adult settings: %s.%s",
                   paste(.bad_ov, collapse = ", "),
                   paste(.valid_adult_overrides, collapse = ", "),
                   if (length(.dedicated) > 0L)
                     " Set adult_permissiveness / adult_scale_max_lbs / ewma_window through their own cleangrowth() arguments, not adult_overrides." else ""))
    }
  }
  # Rows lacking a structural key (subjid or param) cannot be grouped or cleaned;
  # set them aside up front as Exclude-Missing (-> Exclude-Missing-Info) so they
  # never enter batching or z-score calculation, then rebind after the algorithm
  # (mirrors the notgc set-aside below). Done BEFORE the notgc split so an NA
  # param is reported as missing, not as a non-gc param. (NA agedays/sex/
  # measurement are resolved to Exclude-Missing by the algorithm itself.)
  misskey_rows <- NULL
  .miss_key <- is.na(data.all.ages$subjid) | is.na(data.all.ages$param)
  if (any(.miss_key)) {
    misskey_rows  <- data.all.ages[.miss_key]
    data.all.ages <- data.all.ages[!.miss_key]
  }

  # Rows whose `param` (after param_coding resolution) is not one growthcleanr handles
  # are set aside up front (with their original measurement preserved - gc must
  # not mutate the user's data), assigned Exclude-Not-GC-Param, and rebound to
  # the results after the algorithm runs. They never enter z-score calculation
  # or cleaning. Unrecognized params are legitimately common (EHR extracts carry
  # BP, temp, labs), so this is a soft warning, never a hard stop -- but a
  # warning (not a quietly-gated message) so a large silent drop of possibly-
  # mislabeled height/weight cannot pass unnoticed. Reports the verbatim
  # (param_input) tokens the user submitted, with row + subject counts.
  # Row removal only; the "Unrecognized parameters" warning bullet is built
  # dataset-wide by .gc_detect_setaside() below.
  notgc_rows <- NULL
  gc_params <- .gc_recognized_params
  if (any(!data.all.ages$param %in% gc_params)) {
    .ng <- !data.all.ages$param %in% gc_params
    notgc_rows <- data.all.ages[.ng]
    data.all.ages <- data.all.ages[!.ng]
  }

  # Whether the caller left adult_cutpoint at its default. Captured BEFORE the
  # clamp below, because assigning to the parameter makes missing() return FALSE.
  .cutpoint_unset <- missing(adult_cutpoint)

  # rate limit cutpoint -- min 18, max 20 (warn when it moves; the checkpoints get
  # the already-clamped value, so this never repeats under checkpointing)
  .gc_warn_cutpoint_clamp(adult_cutpoint)
  if (adult_cutpoint < 18) {
    adult_cutpoint <- 18
  } else if (adult_cutpoint > 20) {
    adult_cutpoint <- 20
  }

  # Adult-cutpoint advisory: growthcleanr does NOT auto-lower the cutpoint for
  # all-adult data (that would make a subject's routing depend on the rest of the
  # dataset). adult_cutpoint stays at the caller's clamped value; when it was left
  # at the default and every usable-age measurement is adult (>= 18y), a warning
  # suggests adult_cutpoint = 18. Consequence (intended): an all-adult, sex-less
  # dataset is set aside / stops on the pediatric sex requirement unless the caller
  # sets adult_cutpoint = 18 -- exactly what the advisory tells them.
  .gc_adult_cutpoint_advisory(data.all.ages$agedays, .cutpoint_unset)

  # Sex is required to clean PEDIATRIC rows (the child algorithm needs it for
  # z-scores); adult rows clean without it (M/F-averaged CSD reference). So the
  # rule is about the pediatric rows only, keyed on gc's coerced sex (0/1/NA, so
  # NA covers both literal NA and unrecognized codings):
  #   - EVERY pediatric row lacks a usable sex and there are no adult rows ->
  #     nothing can be cleaned -> clean stop.
  #   - Otherwise, pediatric rows without a usable sex become
  #     Exclude-Missing-Info via the algorithm; warn so the loss is visible.
  # (data.all.ages here excludes the set-aside missing-key and non-gc-param rows.)
  # All sex-coding guards are pediatric-gated: adult rows clean without sex, so
  # neither a 1/2 misclassification nor a missing/unrecognized sex matters for
  # an all-adult dataset (it cleans without complaint).
  .ped         <- !is.na(data.all.ages$agedays) &
                  data.all.ages$agedays < adult_cutpoint * 365.25
  .n_ped       <- sum(.ped)
  .n_ped_nosex <- sum(.ped & is.na(data.all.ages$sex))

  # 1/2 hard stop. Runs before the all-missing stop so the message is specific.
  # Under checkpointing the outer branch already checked the whole dataset; a
  # single checkpoint's values say nothing about the coding of the whole.
  if (!.checkpoint_driven)
    .gc_stop_if_one_two_sex(data.all.ages$sex_input, data.all.ages$agedays,
                            adult_cutpoint, sex_coding)

  # Stop only when there are no adult rows either (nothing at all could be
  # cleaned). With adult rows present, the pediatric rows are set aside as
  # Exclude-Missing-Info and reported in the consolidated input warning.
  .n_adult <- sum(!is.na(data.all.ages$agedays) & !.ped)
  if (.n_ped > 0L && .n_ped_nosex == .n_ped && .n_adult == 0L) {
    # When every pediatric-age row is 18 y or older, the data are likely adults
    # caught below the default cutpoint of 20 y: point at adult_cutpoint = 18.
    .ped_all18 <- adult_cutpoint > 18 &&
      all(data.all.ages$agedays[.ped] >= 18 * 365.25)
    stop(sprintf(
      "No pediatric measurement (age < adult_cutpoint = %g y) has a usable value in the sex column. Pediatric measurements are cleaned only with a recognized sex value (0/1, m/M, f/F, b/g, male/female, boy/girl); provide one (or map your coding with sex_coding).%s",
      adult_cutpoint,
      if (.ped_all18) " All of these measurements are at ages 18 y or older; to allow all values to be cleaned with the same algorithm, set adult_cutpoint = 18 (adults do not need a value in the sex column)." else ""))
  }

  # --- Detect the rows growthcleanr set aside and flush the consolidated input
  # warning (Option B): one warning, up to two labeled sections
  # ("Unusable data" set-aside bullets + "Generated ids").
  # .gc_detect_setaside() builds the set-aside bullets dataset-wide from the full
  # raw input (so denominators are dataset-wide and the lead line counts the union
  # of distinct set-aside rows, no double-count); `.gc_adjusted` (the id auto-regen
  # notice) was accumulated inline next to the regen. Gated on !.checkpoint_driven:
  # under checkpointing the per-checkpoint calls stay silent and the outer branch emits
  # one dataset-wide warning instead. call. = FALSE keeps the block readable (no
  # "In cleangrowth(...) :" prefix). Never quietly-gated.
  if (!.checkpoint_driven) {
    .det <- .gc_detect_setaside(subjid, param, agedays, sex, measurement,
                                param_coding, sex_coding, adult_cutpoint,
                                n_input = .n_input,
                                dob_conflict = .gc_dob_conflict,
                                date_setaside = .gc_date_setaside)
    .wmsg <- .gc_format_input_warning(.det$bullets, .det$lines, .gc_adjusted, .n_input)
    if (length(.wmsg) > 0L) warning(paste(.wmsg, collapse = "\n"), call. = FALSE)
    .gc_warn_all_one_sex(sex, agedays, subjid, adult_cutpoint, sex_coding)
  }
  # A user-supplied sd.recenter must cover every evaluable child row (see
  # .gc_check_recenter_coverage). Under checkpointing the outer branch already
  # checked the whole dataset.
  if (!.checkpoint_driven && is.data.table(sd.recenter))
    .gc_check_recenter_coverage(sd.recenter, subjid, param, agedays, sex,
                                measurement, param_coding, sex_coding,
                                adult_cutpoint)

  # gc's internal (working) param + measurement, alongside the verbatim ones.
  # Built from the same .gc_to_metric() rule the algorithm runs, so these report
  # what growthcleanr worked with rather than re-deriving it. Surfaced only on
  # request (internal_detail / full_detail, or named in widened_emit); trimmed
  # away otherwise.
  #
  # Two stages, matching what each path actually does. The imperial conversion
  # applies everywhere (the pediatric preprocessing converts, and cleanadult
  # applies the same factors to its own meas_m). The linear stage: the
  # LENGTHCM -> HEIGHTCM relabel applies to pediatric AND adult rows (adult
  # length is cleaned as height -- see the adult dispatch below), but the
  # length.adjust reconciliation is PEDIATRIC ONLY. Rows with a missing agedays
  # fall through both splits, are never cleaned, and keep their submitted
  # linear label and value. Must run after the cutpoint clamp above.
  .intern <- .gc_to_metric(data.all.ages$param, data.all.ages$v,
                           stage = "imperial")
  .ped_rows <- !is.na(data.all.ages$agedays) &
    data.all.ages$agedays < adult_cutpoint * 365.25
  if (any(.ped_rows)) {
    .ped_lin <- .gc_to_metric(.intern$param[.ped_rows], .intern$v[.ped_rows],
                              length.adjust = length.adjust,
                              agedays = data.all.ages$agedays[.ped_rows],
                              stage = "linear")
    .intern$param[.ped_rows] <- .ped_lin$param
    .intern$v[.ped_rows]     <- .ped_lin$v
  }
  .adult_rows <- !is.na(data.all.ages$agedays) & !.ped_rows
  if (any(.adult_rows)) {
    .intern$param[.adult_rows] <- .gc_to_metric(
      .intern$param[.adult_rows], .intern$v[.adult_rows],
      stage = "linear")$param
  }
  data.all.ages[, gc_internal_param := .intern$param]
  data.all.ages[, gc_internal_meas  := .intern$v]
  # No recentering (sd.recenter = "none"): supply a zero-median table so the
  # recentered scores equal the reference scores (tbc.sd = sd.orig). We load the
  # built-in reference file only for its (param, sex, agedays) grid and zero the
  # medians, which guarantees every measurement still has a recentering value.
  if (identical(sd.recenter, "none")) {
    rc_path <- ifelse(
      ref.data.path == "",
      system.file(file.path("extdata", "rcfile-resmoothed.csv.gz"), package = "growthcleanr"),
      file.path(ref.data.path, "rcfile-resmoothed.csv.gz")
    )
    sd.recenter <- fread(rc_path)
    sd.recenter[, rcmedian.sd := 0]
    if (verbose)
      message(.ts(), "recentering disabled (sd.recenter = \"none\"): tbc.sd = sd.orig")
  }

  ### BATCHING ###
  results_list <- list()
  i <- 1
  patients <- unique(data.all.ages[, .(subjid)])
  patients[, batch := (seq_len(.N) - 1L) %/% batch_size + 1L]
  batches <- patients
  # Number of outer (memory) batches, used to scope per-batch progress messages.
  n_outer <- length(unique(batches$batch))

  # --- Progress framing (standalone runs only) ---
  # When driven by cleangrowth_checkpoint(), the wrapper owns the header, the
  # per-checkpoint banner, and the elapsed-time total; this call then prints only its
  # milestone lines. When called directly, cleangrowth() owns that framing.
  t_run_start <- Sys.time()
  if (!quietly && !.checkpoint_driven) {
    message(.ts(), sprintf(
      "growthcleanr: cleaning in progress \u2014 %s subjects, %s measurements, %d batch%s of up to %s subjects",
      format(nrow(patients), big.mark = ","),
      format(nrow(data.all.ages), big.mark = ","),
      n_outer, if (n_outer == 1L) "" else "es",
      format(batch_size, big.mark = ",")
    ))
  }

  # --- Batch-invariant constants (computed once, not per batch) ---

  # .child_exc() is defined at file level (before cleangrowth) so it's
  # visible in data.table j expressions within cleanchild()

  # enumerate the different exclusion levels
  exclude.levels.peds <- c(
    'Include',
    'Exclude-Missing',
    'Exclude-Not-Cleaned',
    'Exclude-Not-GC-Param',
    'Exclude-C-Temp-Same-Day',
    # child exclusion codes (not param-specific; param is in the data)
    'Exclude-C-CF',
    'Exclude-C-Traj-Extreme',
    'Exclude-C-Identical',
    'Exclude-C-Extraneous',
    'Exclude-C-Traj',
    'Exclude-C-Abs-Diff',
    'Exclude-C-Pair',
    'Exclude-C-Single',
    'Exclude-C-Unevaluable-Trajectory',
    'Exclude-C-PIV',
    'Exclude-C-Evil-Twins'
  )

  # Adult exclusion codes used by cleanadult()
  # Not param-specific; param is in the data
  exclude.levels.adult <- c(
    "Exclude-A-PIV",
    "Exclude-A-Scale-Max",
    "Exclude-A-Scale-Max-Identical",
    "Exclude-A-Scale-Max-RV-Propagated",
    "Exclude-A-Evil-Twins",
    "Exclude-A-Identical",
    "Exclude-A-Extraneous",
    "Exclude-A-Traj-Extreme",
    "Exclude-A-Traj-Extreme-firstRV",
    "Exclude-A-Traj-Extreme-allRV",
    "Exclude-A-Traj-Extreme-firstRV-RV-Propagated",
    "Exclude-A-Ord-Pair",
    "Exclude-A-Ord-Pair-All",
    "Exclude-A-Window",
    "Exclude-A-Window-All",
    "Exclude-A-2D-Ordered",
    "Exclude-A-2D-Non-Ordered",
    "Exclude-A-Traj-Moderate",
    "Exclude-A-Traj-Moderate-allRV",
    "Exclude-A-Traj-Moderate-RV-Propagated",
    "Exclude-A-Traj-Moderate-Error-Load",
    "Exclude-A-Traj-Moderate-Error-Load-RV",
    "Exclude-A-Single",
    "Exclude-A-Unevaluable-Trajectory"
  )
  exclude.levels <- c(exclude.levels.peds, exclude.levels.adult)

  # Output-level crosswalks + factor levels (Summary / Detailed): defined once in
  # .gc_exclude_maps() (package level, so longwide() can validate codes too).
  .gc_em <- .gc_exclude_maps()
  exclude.map.detailed    <- .gc_em$map_detailed
  exclude.map.summary     <- .gc_em$map_summary
  exclude.levels.summary  <- .gc_em$levels_summary
  exclude.levels.detailed <- .gc_em$levels_detailed

  # Load velocity reference tables (same for all batches, read once here)
  # These are only used by the pediatric algorithm (Step 17). For all-adult
  # datasets they are loaded but unused - negligible cost (~0.01 sec).
  # Tanner height velocity (used by Step 17 for ages 2.5+ years)
  tanner_ht_vel_path <- ifelse(
    ref.data.path == "",
    system.file(file.path("extdata", "tanner_ht_vel.csv.gz"), package = "growthcleanr"),
    file.path(ref.data.path, "tanner_ht_vel.csv.gz")
  )
  tanner.ht.vel <- fread(tanner_ht_vel_path)
  setnames(tanner.ht.vel, colnames(tanner.ht.vel), gsub('_', '.', colnames(tanner.ht.vel)))
  setkey(tanner.ht.vel, sex, tanner.months)

  # WHO velocity tables are loaded inside cleanchild() (HT and HC separately).

  # --- Parallel setup (once, before outer batch loop) ---
  # Create a single cluster for both child and adult dispatch across all
  # outer batches. This avoids the overhead of creating/destroying a cluster
  # per batch (~2-5 sec each). All internal functions needed by both
  # cleanchild() and cleanadult() are exported to workers.
  if (parallel) {
    # Resolve the number of parallel workers ("cores"). NULL -> auto:
    # min(8, detectCores() - 1), capped at 8 because gc's per-worker setup
    # (clusterExport + data serialization, and a fresh cluster per checkpoint under
    # checkpointing) makes more workers counterproductive past ~8. A user value
    # above the machine's core count is capped to it: extra workers only
    # oversubscribe, and an extreme value (e.g. a fat-fingered cores = 99999)
    # would otherwise stall the run trying to spawn that many worker processes.
    if (is.null(cores)) {
      nc    <- parallel::detectCores()
      cores <- if (is.na(nc) || nc < 2L) 1L else min(8L, nc - 1L)
    } else {
      if (!(length(cores) == 1L && is.numeric(cores) && !is.na(cores) &&
            cores >= 1 && cores == as.integer(cores)))
        stop(sprintf("cores must be a single positive whole number, or NULL to auto-detect; %s was entered.",
                     .gc_desc_val(cores)), call. = FALSE)
      cores <- as.integer(cores)
      nc <- parallel::detectCores()
      if (!is.na(nc) && cores > nc) {
        warning(sprintf("cores = %d exceeds the %d core(s) detected; capping to %d (extra workers only oversubscribe and would slow or stall the run).",
                        cores, nc, nc), call. = FALSE)
        cores <- nc
      } else if (is.na(nc) && cores > 128L) {
        # Machine core count could not be detected: apply a runaway guard so an
        # implausible value cannot stall the run, while still allowing large
        # legitimate worker counts on big servers.
        warning(sprintf("cores = %d is implausibly large and the machine core count could not be detected; capping to 128 to avoid stalling the run.",
                        cores), call. = FALSE)
        cores <- 128L
      }
    }
    if (cores > 1L) {
    var_for_par <- c(
      # Child algorithm functions
      ".child_valid",
      "ewma", "read_anthro", "as_matrix_delta",
      "identify_temp_sde",
      "get_dop", "calc_otl_evil_twins",
      "calc_and_recenter_z_scores",
      ".child_exc",
      "ewma_cache_init", "ewma_cache_update",
      # Shared CSD / unit-error-range helpers (used by child Step 9 and adult
      # Evil Twins on workers)
      ".child_z3", ".unit_error_range_flag",
      ".adult_csd_ref", ".adult_csd_lookup", ".adult_z3", ".adult_csd_z",
      # Adult algorithm functions
      "cleanadult",
      "permissiveness_presets", "resolve_permissiveness",
      "check_between", "round_pt",
      "compute_et_limit", "compute_perc_limit", "compute_wtallow",
      "as.matrix.delta_dn", "ewma_dn",
      "adult_ewma_cache_init", "adult_ewma_cache_update",
      "remove_piv", "remove_piv_high", "remove_piv_low",
      "identify_rv", "temp_sde", "redo_identify_rv",
      "ht_allow", "ht_change_groups", "ht_3d_growth_compare",
      "detect_runs", "compute_trajectory_fails",
      "evil_twins", "propagate_to_rv",
      "remove_ewma_wt", "remove_mod_ewma_wt",
      "eval_2d_nonord", "eval_1d", "eval_error_load"
    )
    cl <- makeCluster(cores)
    clusterExport(cl = cl, varlist = var_for_par, envir = environment())
    registerDoParallel(cl)
    }
  } else {
    cores <- 1L
  }

  ### Loop Begin ###

  for (id_batch in unique(batches$batch)) {
    # Per-batch scope tag for milestone lines. Empty for a single batch (the
    # header / checkpoint banner already names the unit); "batch i/n - " when this
    # cleangrowth() call is split into multiple memory batches.
    bp <- if (n_outer > 1L) sprintf("batch %d/%d \u2014 ", i, n_outer) else ""
    # Standalone multi-batch runs get a prominent per-batch banner; under
    # checkpoint the checkpoint banner (printed by the wrapper) already marks position.
    t_batch_start <- Sys.time()
    if (verbose && !.checkpoint_driven && n_outer > 1L) {
      message("")
      message("==================================================")
      message(sprintf("  BATCH %d / %d   \u00b7   %s subjects",
                      i, n_outer, format(length(batches$subjid[batches$batch == id_batch]), big.mark = ",")))
      message("==================================================")
    }
    ids <- batches$subjid[batches$batch == id_batch]

    data.all <- copy(
      data.all.ages[
        subjid %in% ids & agedays < adult_cutpoint * 365.25
      ]
    )

    data.adult <- copy(
      data.all.ages[
        subjid %in% ids & agedays >= adult_cutpoint * 365.25
      ]
    )
  # Batch-level reference for result assembly (all ages for this batch's subjects)
  data.batch <- copy(data.all.ages[subjid %in% ids])

  # if there's no pediatric data, no need to go through this rigamarole
  if (nrow(data.all) > 0){

    # pediatric: height velocity calculations and preprocessing ----

    # for pediatric data, convert in and lbs to cm and kg (adult is done within
    # algo). .gc_to_metric() (R/reshape_adapters.R) owns the conversion
    # arithmetic so the optional gc_internal_meas / gc_internal_param output
    # columns report exactly what the algorithm used. LENGTHCM is relabeled
    # further below, after the optional length.adjust.
    .conv <- .gc_to_metric(data.all$param, data.all$v, stage = "imperial")
    data.all[, param := .conv$param]
    data.all[, v := .conv$v]

    setkey(data.all, subjid)
    subjid.unique <- data.all[j = unique(subjid)]
    batches.all <- data.table(
      subjid = subjid.unique,
      batch = (seq_along(subjid.unique) - 1L) %% cores + 1L,
      key = 'subjid'
    )
    data.all <- batches.all[data.all]

    if (verbose)
      message(.ts(), bp, "preprocessing (z-scores, recentering)")

    # Velocity reference tables loaded once before the batch loop (batch-invariant)

    # Algorithm design principles:
    # - All steps operate separately by parameter unless otherwise noted.
    # - Data sorted by subjid, param, agedays, internal_id; only non-excluded,
    #   non-missing rows participate (controlled by .child_valid()).
    # - "Next" and "previous" refer to adjacent included values for the same
    #   subject and parameter by age.
    # - Exclusions are recorded in the `exclude` factor column. Once a row is
    #   excluded it stays excluded unless explicitly rescued (e.g., CF rescue).
    # - CSD z-scores (not LMS z-scores) form the basis for the algorithm.
    #   CSD is more sensitive to extreme high values than standard LMS z-scores.

    # Linear stage: the optional recumbent/standing offset, then the
    # LENGTHCM -> HEIGHTCM relabel. Recumbent length is ~0.7 cm longer than
    # standing height for the same child, and the reference files are
    # length-based through day 729 and height-based from day 730
    # (.gc_ht_start_day), so length.adjust = TRUE reconciles the two in BOTH
    # directions: length recorded at day 730 or later is scored 0.7 cm lower,
    # height recorded before day 730 0.7 cm higher.
    # The offset is kept in lin_off and applied ONLY where a value is z-scored
    # (sd.orig, the corrected z, and the plus/minus perturbations); v stays the
    # entered (metric) value, so the raw-cm checks see what was entered. Step 2b
    # adds the GA-window offset to lin_off under the default. Runs after the
    # imperial conversion above, so an inch-entered height is already in cm.
    # .gc_to_metric() (R/reshape_adapters.R) owns the arithmetic.
    .lin <- .gc_to_metric(data.all$param, data.all$v,
                          length.adjust = length.adjust,
                          agedays = data.all$agedays, stage = "linear")
    data.all[, param := .lin$param]
    data.all[, lin_off := .lin$off]

    # Plus/minus perturbations: a +/- nudge of each value. WEIGHTKG uses
    # +/-5%; HEIGHTCM and HEADCM use +/-1 cm. Built here (on the finalized v,
    # after LENGTHCM recode) so they ride along through Step 2b correction,
    # recentering, and into cleanchild. Their recentered z-scores
    # (tbc.p_plus/minus, ctbc.p_plus/minus) are produced at the recentering step
    # below and consumed by the trajectory steps (Child Steps 11, 15/16) to
    # avoid flagging values that a +/- nudge would render plausible.
    data.all[, p_plus := NA_real_]
    data.all[, p_minus := NA_real_]
    data.all[param == "WEIGHTKG", `:=`(p_plus = 1.05 * v, p_minus = 0.95 * v)]
    # Linear values carry the z-score-only recumbent/standing offset (lin_off).
    data.all[param %in% c("HEIGHTCM", "HEADCM"),
             `:=`(p_plus = v + lin_off + 1, p_minus = v + lin_off - 1)]

    # calculate z/sd scores
      # removing z calculations, as they are not used
      # for infants, use z and who
      measurement.to.z <- if (!is.null(ref_tables)) ref_tables$mtz_cdc_prelim else
        read_anthro(ref.data.path, cdc.only = TRUE)
      measurement.to.z_who <- if (!is.null(ref_tables)) ref_tables$mtz_who_prelim else
        read_anthro(ref.data.path, cdc.only = FALSE)

      # calculate "standard deviation" scores
      # z-scores use the entered value plus the recumbent/standing offset
      # (lin_off; 0 unless length.adjust = TRUE -- see the linear stage above).
      data.all[, sd.orig_cdc := measurement.to.z(param, agedays, sex, v + lin_off, TRUE)]
      # Note: sd.orig_who contains CDC values for ages >= 5y (the WHO closure
      # falls back to CDC when WHO data is unavailable). The name is historical.
      # The blending logic below gives zero weight to WHO at age 5+, so these
      # CDC values in sd.orig_who are never incorrectly used as WHO.
      data.all[, sd.orig_who := measurement.to.z_who(param, agedays, sex, v + lin_off, TRUE)]

      # smooth z-scores/SD scores between ages 2-5yo using weighted scores
      # older uses cdc, younger uses who
      # Gradual WHO -> CDC transition: Daymont C, et al. Pediatrics. 2025;
      # 156(3):e2025070697 (see vignette("algorithm-functions"), Age blending).
      data.all$ageyears <- data.all$agedays/365.25

      who_weight <- 5 - (data.all$ageyears)
      cdc_weight <- (data.all$ageyears) - 2

      smooth_val <- data.all$ageyears >= 2 &
        data.all$ageyears <= 5 &
        data.all$param != "HEADCM"
      data.all[smooth_val,
               sd.orig := (data.all$sd.orig_cdc[smooth_val]*cdc_weight[smooth_val] +
                             data.all$sd.orig_who[smooth_val]*who_weight[smooth_val])/3]
      # <-
      # otherwise use WHO and CDC for older and younger, respectively
      who_val <- data.all$param == "HEADCM" |
        data.all$ageyears < 2

      data.all[(who_val & !smooth_val) | (smooth_val & is.na(data.all$sd.orig_cdc)),
               sd.orig := data.all$sd.orig_who[(who_val & !smooth_val)  | (smooth_val & is.na(data.all$sd.orig_cdc))]]

      cdc_val <- data.all$param != "HEADCM" &
        data.all$ageyears > 5

      data.all[(cdc_val & !smooth_val) | (smooth_val & is.na(data.all$sd.orig_who)),
               sd.orig := data.all$sd.orig_cdc[(cdc_val & !smooth_val) | (smooth_val & is.na(data.all$sd.orig_who))]]


      # 2b: corrected z scores ----

      # keep the original column names -- we're adding a ton of columns that we
      # want to filter out after correction
      orig_colnames <- copy(colnames(data.all))

      # start by reading in fenton data only if potcorr subjects exist (see below)

      # add age in months and years (ageyears_2b used throughout Step 2b)
      data.all[, agemonths := agedays / 30.4375]
      data.all[, ageyears_2b := agedays / 365.25]

      # Sort by id, age-dependent direction
      # Age-dependent ID selection for SDEs:
      # At age 0: prefer LOWEST id (earliest measurement, before fluid/interventions)
      # At age > 0: prefer HIGHEST id (consistent with other SDE handling)
      # Create sort key: for age 0 use id ascending, for age > 0 use id descending
      data.all[, id_sort := ifelse(agedays == 0, internal_id, -internal_id)]
      setorder(data.all, subjid, param, agedays, id_sort)

      # initialize the column so it's always present
      # Create row-level potcorr_wt flag
      data.all[, potcorr := FALSE]
      data.all[, potcorr_wt := FALSE]

      # Mark infants needing Fenton correction
      # potcorr_wt: row-level flag for FIRST weight per subject (earliest agedays, not necessarily birth)
      #             that qualifies: Z<-2 AND age<10 months
      # After sort by agedays + id_sort, seq_len(.N)==1L selects first weight with age-dependent ID
      # NA-safe: sd.orig is NA when measurement is NA (Missing rows)
      data.all[param == "WEIGHTKG",
               potcorr_wt := (seq_len(.N) == 1L &
                              !is.na(sd.orig) & sd.orig < -2 &
                              agemonths < 10),
               by = subjid]

      # Clean up sort key
      data.all[, id_sort := NULL]

      # propagate that flag to all rows for that subject (subject-level)
      data.all[, potcorr := any(potcorr_wt, na.rm = TRUE), by = subjid]

      # ensure deterministic order
      setkey(data.all, subjid, param, agedays, internal_id)

      # --- Potcorr optimization: skip all reference merges if no potcorr subjects ---
      has_potcorr <- any(data.all$potcorr, na.rm = TRUE)

      if (has_potcorr) {
        # Read Fenton reference data (only when potcorr subjects exist)
        fent_foraga <- fread(
          system.file(file.path("extdata", "fent_foraga.csv.gz"),
                      package = "growthcleanr"))
        # Approximated Fenton CSD values (M, S_upper, S_lower) derived from printed Fenton (2025) charts
        fenton_ref <- fread(
          system.file(file.path("extdata", "fenton2025_ms_lookup_smoothed.csv"),
                      package = "growthcleanr"))

        # Subset to potcorr subject rows only (typically <1% of data)
        pc_ids <- unique(data.all[potcorr == TRUE, subjid])
        pc <- copy(data.all[subjid %in% pc_ids])

        # integer weight is in grams, rounded to the nearest 10
        pc[potcorr == TRUE, intwt := trunc(v*100)*10]
        # Floor weights in [100, 500) g up to the Fenton table minimum of 500 g,
        # so users who configure lower PIV thresholds can still get
        # Fenton-corrected z-scores for very low weights.
        pc[intwt >= 100 & intwt < 500, intwt := 500]

        # Fenton merge 1: weight -> estimated gestational age
        pc <- merge(
          pc, fent_foraga, by = c("sex", "intwt"),
          all.x = TRUE)

        # Propagate fengadays ONLY from potcorr_wt rows
        # Only use fengadays from the qualifying first weight (potcorr_wt==TRUE), not from any row
        pc[, fengadays_subj := ifelse(any(!is.na(fengadays) & potcorr_wt),
                                             min(fengadays[potcorr_wt], na.rm = TRUE),
                                             NA_real_),
                 by = subjid]
        # Use subject-level fengadays for all calculations
        pc[!is.na(fengadays_subj), fengadays := fengadays_subj]
        pc[, fengadays_subj := NULL]  # Clean up temporary variable

        pc[fengadays < 259, pmagedays := agedays + fengadays]
        pc[fengadays >= 259 | is.na(fengadays), pmagedays := NA_real_]

        pc[fengadays < 259, cagedays := pmagedays - 280]
        # replace fengadays with pmagedays to facilitate merging
        pc[, fengadays := pmagedays]

        # Fenton merge 2: gestational age -> approximated Fenton CSD values (M, S_upper, S_lower)
        # Map gc param names to Fenton reference param names for long-format merge
        pc[, fen_param := fcase(
          param == "WEIGHTKG", "weight",
          param == "HEIGHTCM", "length",
          param == "HEADCM",   "headcirc"
        )]
        pc <- merge(
          pc, fenton_ref[, .(sex, ga_days, param, M, S_upper, S_lower)],
          by.x = c("sex", "fengadays", "fen_param"),
          by.y = c("sex", "ga_days", "param"),
          all.x = TRUE)

        # Reset potcorr_wt when Fenton merge fails
        # Only check weight rows (potcorr_wt is only TRUE on WEIGHTKG rows)
        pc[potcorr_wt == TRUE & is.na(M), potcorr_wt := FALSE]
        # Recalculate subject-level potcorr
        pc[, potcorr := any(potcorr_wt), by = subjid]

        # --- Corrected z-scores: actual + plus/minus perturbations ---
        # Route the actual measurement and its +/-5% (WT) / +/-1 cm (HT/HC)
        # perturbations through the SAME Fenton + corrected-WHO/CDC pathway via
        # .compute_corrected_z(). The GA-correction parameters (cagedays, Fenton
        # M/S) are held fixed; only the measurement being z-scored varies. The
        # helper computes the final corrected z directly (Fenton CSD, corrected
        # WHO/CDC blend, Fenton-over-WHO for <=2, and the 2-4yr smooth blend with
        # the uncorrected z).
        pc[, sd.corr :=
             .compute_corrected_z(pc, v + lin_off, measurement.to.z, measurement.to.z_who)]
        pc[, sd.corr_plus :=
             .compute_corrected_z(pc, p_plus, measurement.to.z, measurement.to.z_who)]
        pc[, sd.corr_minus :=
             .compute_corrected_z(pc, p_minus, measurement.to.z, measurement.to.z_who)]

        # --- Evaluate whether correction makes z-scores more extreme (uncorr) ---
        examine_only <- pc$param == "WEIGHTKG" & pc$potcorr
        tmp <- copy(pc[examine_only,])

        # Filter SDE-Identicals BEFORE uncorr calculation (pre-Step-13 cleanup).
        # At birth (age 0): Keep LOWEST id (earliest, before fluid/interventions)
        # At age > 0: Keep HIGHEST id (consistent with other SDE handling)

        # Identify same-day identical values
        tmp[, `:=`(
          n_on_day = .N,
          n_unique_vals = uniqueN(v)
        ), by = .(subjid, agedays)]

        tmp[, all_identical := n_on_day > 1 & n_unique_vals == 1]

        # Age-dependent ID selection for identicals.
        # length(ids) == 0L guards the empty-table case: data.table evaluates j
        # once on an empty group to determine column type when tmp has 0 rows
        # (no potcorr WEIGHTKG rows in this batch); without the guard max() warns.
        tmp[, keep_id := {
          ids <- internal_id
          if (length(ids) == 0L) NA_integer_
          else if (isTRUE(agedays[1L] == 0L)) min(ids) else max(ids)
        }, by = .(subjid, agedays)]

        # Filter out non-selected identicals
        tmp <- tmp[!(all_identical & internal_id != keep_id)]

        # Clean up temporary columns
        tmp[, `:=`(n_on_day = NULL, n_unique_vals = NULL, all_identical = NULL, keep_id = NULL)]

        # Now create seq_win on the filtered data (without SDE-Identicals)
        # Age-dependent ID sorting for consistent sequence numbering:
        # At age 0: sort by id ascending (lowest first)
        # At age > 0: sort by id descending (highest first)
        tmp[, id_sort := ifelse(agedays == 0, internal_id, -internal_id)]
        tmp <- tmp[order(subjid, agedays, id_sort),]
        tmp[, id_sort := NULL]

        # Create seq_win (sequence number per subject, after identicals removed)
        tmp[, seq_win := sequence(.N), by = subjid]
        # we're only looking at the first 4 values, and they need to be < 2 years
        tmp <- tmp[seq_win <= 4 & ageyears_2b < 2,]
        # don't look at subjects where there is only 1 score and there is no
        # value for either
        tmp <- tmp[!(is.na(sd.corr) & is.na(sd.orig)),]
        tmp <- tmp[subjid %in% names(which(table(subjid) > 1)),]

        # create differences and take the absolute value of their sum:
        #   abs(sum(sd.corr[1] - sd.corr)) == abs(sum of diffs from the first value)
        tmp[, sd.corr_abssumdiff := abs(sum(sd.corr[1] - sd.corr)), by = subjid]
        tmp[, sd.orig_abssumdiff := abs(sum(sd.orig[1] - sd.orig)), by = subjid]
        # find subjects where corrected value needs to be replaced

        # Only evaluate uncorr for the first weight per subject (earliest agedays)
        tmp[, is_first := seq_win == 1]

        # Correction is kept only if true prematurity is CORROBORATED, by EITHER:
        #   form 1 -- a consistent early weight trajectory: >= 2 weights < 2y whose
        #     corrected early spread is no worse than uncorrected (i.e. correction
        #     does not scatter the early points), OR
        #   form 2 -- a same-day height OR head circumference at the qualifying
        #     first weight that is also low on its own (uncorrected z <= -1.5) AND
        #     no more than 3.5 z above the first weight's uncorrected z: the infant
        #     is short (or small-headed) as well as light, consistent with real
        #     prematurity. The 3.5 cap prevents a merely-somewhat-low HT/HC from
        #     corroborating a far-lower (e.g. misplaced-decimal) birth weight.
        # A potcorr subject corroborated by NEITHER is reverted to uncorrected.
        # This catches a misplaced-decimal birth weight (low weight, but a normal
        # same-day length and no consistent early weight follow-up): the false
        # correction is withdrawn, so the implausible value is no longer hidden
        # from the trajectory steps and can be excluded.
        keep_wt <- unique(tmp[is_first == TRUE & !is.na(sd.orig_abssumdiff) &
                                sd.corr_abssumdiff <= sd.orig_abssumdiff, subjid])
        # length() guard against the data.table empty-group type-inference pass:
        # when no rows survive the potcorr_wt filter, j is evaluated once on an
        # empty group, and min(integer(0)) warns "no non-missing arguments to
        # min; returning Inf". Same pattern as the keep_id guard above.
        fw_age <- pc[potcorr_wt == TRUE, .(
          fw_agedays = if (length(agedays) == 0L) NA_real_ else min(agedays),
          fw_wt_z    = if (length(sd.orig) == 0L) NA_real_ else sd.orig[1L]
        ), by = subjid]
        # form 2: a same-day HT or HC at the first weight, low on its own
        # (<= -1.5) and no more than 3.5 z above the first weight's uncorrected z.
        keep_ht <- unique(merge(
          pc[param %in% c("HEIGHTCM", "HEADCM") & !is.na(sd.orig),
             .(subjid, agedays, ht_z = sd.orig)],
          fw_age, by = "subjid"
        )[agedays == fw_agedays & ht_z <= -1.5 & (ht_z - fw_wt_z) <= 3.5, subjid])
        sub_replace <- setdiff(unique(pc[potcorr == TRUE, subjid]),
                               union(keep_wt, keep_ht))

        if (verbose)
          message(sprintf("  GA correction: %d subjects reverted to uncorrected z-scores", length(sub_replace)))

        # Revert correction for flagged subjects
        pc[subjid %in% sub_replace, sd.corr := sd.orig]
        pc[, uncorr := as.integer(subjid %in% sub_replace)]

        # --- GA window: presumed-recumbent heights (default only) ---
        # Window = HEIGHTCM rows of a (still) corrected subject with
        # chronological age at or past day 730 (.gc_ht_start_day) and corrected
        # age before day 730, the day the references switch to height. The
        # uncorrected z compares against the standing-height reference, the
        # corrected z against the recumbent-length reference at corrected age.
        # Under the default (length.adjust = FALSE) the measurement method is
        # presumed to follow corrected age, so a window value is presumed
        # recumbent: it is scored .gc_len_offset lower wherever it is z-scored
        # against standing height (sd.orig / tbc.sd and the uncorrected
        # plus/minus scores). The corrected z adds the same offset back in
        # .compute_corrected_z(), so it is scored on the entered value. As
        # with length.adjust, the offset lives only in lin_off (z-scores); v
        # stays the entered value for the raw-cm checks. With length.adjust =
        # TRUE the label decides instead (lin_off from the linear stage).
        # Subjects whose correction was withdrawn above (uncorr) are not
        # corrected, so they are not adjusted. The uncorr decision uses only
        # weights and first-weight-day heights/HCs, never window rows.
        pc[, ga_win_adj := FALSE]
        if (!isTRUE(length.adjust)) {
          pc[potcorr %in% TRUE & uncorr == 0L & param == "HEIGHTCM" &
               !is.na(v) & agedays >= .gc_ht_start_day &
               cagedays < .gc_ht_start_day,
             ga_win_adj := TRUE]
          if (any(pc$ga_win_adj)) {
            pc[ga_win_adj == TRUE, `:=`(
              lin_off = lin_off - .gc_len_offset,
              p_plus  = p_plus - .gc_len_offset,
              p_minus = p_minus - .gc_len_offset
            )]
            pw <- pc[ga_win_adj == TRUE]
            .uz <- .gc_uncorrected_z(pw$param, pw$agedays, pw$sex,
                                     pw$v + pw$lin_off,
                                     measurement.to.z, measurement.to.z_who)
            pc[ga_win_adj == TRUE, `:=`(
              sd.orig_who = .uz$sd.orig_who,
              sd.orig_cdc = .uz$sd.orig_cdc,
              sd.orig     = .uz$sd.orig,
              sd.corr = .compute_corrected_z(pw, pw$v + pw$lin_off,
                                             measurement.to.z,
                                             measurement.to.z_who),
              sd.corr_plus = .compute_corrected_z(pw, pw$p_plus, measurement.to.z,
                                                  measurement.to.z_who),
              sd.corr_minus = .compute_corrected_z(pw, pw$p_minus, measurement.to.z,
                                                   measurement.to.z_who)
            )]
          }
        }

        # Clean up Fenton merge columns (retained until after the corrected-z
        # calls above, including the GA-window rescoring).
        pc[, c("fen_param", "M", "S_upper", "S_lower") := NULL]

        # --- Write results back to data.all ---
        data.all[, sd.corr := sd.orig]
        data.all[, uncorr := 0L]
        # Perturbed corrected raw z; NA for non-potcorr rows. Recentered
        # to ctbc.p_plus/minus at the recentering step below.
        data.all[, sd.corr_plus := NA_real_]
        data.all[, sd.corr_minus := NA_real_]

        # GA-window rows (default only): carry the offset, perturbations and
        # uncorrected z back to data.all (v itself is unchanged).
        # Joins key on internal_id (the user's id is output-only).
        pc_win <- pc[ga_win_adj == TRUE, .(internal_id, lin_off, p_plus, p_minus,
                                          sd.orig_who, sd.orig_cdc, sd.orig)]
        if (nrow(pc_win) > 0L) {
          data.all[pc_win, on = "internal_id", `:=`(
            lin_off = i.lin_off, p_plus = i.p_plus, p_minus = i.p_minus,
            sd.orig_who = i.sd.orig_who, sd.orig_cdc = i.sd.orig_cdc,
            sd.orig = i.sd.orig
          )]
        }

        # Update potcorr subjects from subset results
        pc_result <- pc[, .(internal_id, sd.corr, uncorr, potcorr,
                            sd.corr_plus, sd.corr_minus)]
        data.all[pc_result, on = "internal_id", `:=`(
          sd.corr = i.sd.corr,
          uncorr = i.uncorr,
          potcorr = i.potcorr,
          sd.corr_plus = i.sd.corr_plus,
          sd.corr_minus = i.sd.corr_minus
        )]

      } else {
        # No potcorr subjects -- fast path: skip all reference file I/O and merges
        data.all[, sd.corr := sd.orig]
        data.all[, uncorr := 0L]
        data.all[, sd.corr_plus := NA_real_]
        data.all[, sd.corr_minus := NA_real_]
      }


      # Add sd.corr, potcorr, uncorr (+ perturbed corrected raw z) to
      # orig_colnames so they survive column cleanup
      orig_colnames <- c(orig_colnames, "sd.corr", "potcorr", "uncorr",
                         "sd.corr_plus", "sd.corr_minus")

      # remove temporary columns (agemonths, ageyears_2b, potcorr_wt, etc.)
      data.all <- data.all[, colnames(data.all) %in% c(orig_colnames, "id"),
                           with = FALSE]


    # sort by subjid, param, agedays, internal_id for deterministic SDE order
    setkey(data.all, subjid, param, agedays, internal_id)

    # add a new convenience index for bookkeeping
    data.all[, index := seq_len(.N)]

    # Mark missing values for exclusion
    data.all[, exclude := factor(
      ifelse(is.na(v) | is.na(agedays) | agedays < 0 | v < 0, 'Exclude-Missing', 'Include'),
    levels = exclude.levels,
    ordered = TRUE)]
    # HC measurements beyond 5 years are not cleaned. The WHO HC z-score
    # reference (sd.orig) extends to exactly 5 years, so recentered z-scores
    # cannot be formed past that point. The recentering file provides HC
    # rcmedian.sd through 5 years (see the recentering vignette). HC measurements
    # 24 months-5 years are cleaned by all standard steps; the velocity check
    # (Child Step 17) applies only mindiff = -1.5 with no upper bound for these
    # ages, since the WHO HC velocity reference ends at 24 months.
    data.all[param == "HEADCM" & agedays >= (5*365.25), exclude := "Exclude-Not-Cleaned"]

    # Initialize cf_rescued column to track CF rescue status
    # Populated in Step 6; rescued CFs get set back to "Include" but this column
    # preserves which rescue category they matched
    data.all[, cf_rescued := ""]

    # define field names needed by helper functions
    ewma.fields <- c('ewma.all', 'ewma.before', 'ewma.after')

    # SD-score recentering
    #
    # The code below reads a precomputed recentering file
    # (rcfile-resmoothed.csv.gz) and subtracts its per-
    # (param, sex, agedays) median SD-score from sd.orig to produce
    # tbc.sd (and from sd.corr to produce ctbc.sd). Recentering is
    # needed because the population mean SD-score changes with age.
    #
    # How the rcfile was built (smoothed EHR-derived medians) is described
    # in vignette("recentering").
    # Users can override with a custom sd.recenter data.table if desired.
    # (Recentering is reported as part of the "preprocessing" milestone line.)

    # Recentering: use built-in reference file or user-supplied data.table
    if (!is.data.table(sd.recenter)) {
      rc_path <- ifelse(
        ref.data.path == "",
        system.file(file.path("extdata",
                              "rcfile-resmoothed.csv.gz"),
                    package = "growthcleanr"),
        file.path(ref.data.path, "rcfile-resmoothed.csv.gz")
      )
      sd.recenter <- fread(rc_path)
      if (verbose)
        message(.ts(), bp, "using built-in reference medians")
    } else {
      if (verbose)
        message(.ts(), bp, "using user-supplied re-centering medians")
    }

    # accept a legacy user-supplied table whose median column is still named
    # sd.median (the built-in rcfile now ships the column as rcmedian.sd)
    if ("sd.median" %in% names(sd.recenter) && !"rcmedian.sd" %in% names(sd.recenter))
      setnames(sd.recenter, "sd.median", "rcmedian.sd")

    # ensure recentering medians are sorted correctly
    setkey(sd.recenter, param, sex, agedays)

    # add sd.recenter to data, and recenter
    setkey(data.all, param, sex, agedays)
    data.all <- sd.recenter[data.all]

    setkey(data.all, subjid, param, agedays, internal_id)
    data.all[, tbc.sd := sd.orig - rcmedian.sd]
    data.all[, ctbc.sd := sd.corr - rcmedian.sd]

    # --- Recenter the plus/minus perturbations ---
    # Uncorrected perturbed z (recentered): same WHO/CDC blend + recentering as
    # tbc.sd, applied to p_plus / p_minus. Computed once here for all rows so
    # the trajectory steps (Child Steps 11, 15/16) can read them directly.
    data.all <- calc_and_recenter_z_scores(data.all, "p_plus", ref.data.path,
                                           measurement.to.z, measurement.to.z_who)
    data.all <- calc_and_recenter_z_scores(data.all, "p_minus", ref.data.path,
                                           measurement.to.z, measurement.to.z_who)
    # Corrected perturbed z: default to the uncorrected perturbation, then for
    # actually-corrected values (ctbc.sd != tbc.sd) recenter the corrected
    # perturbed raw z. For non-corr values ctbc.p == tbc.p by construction
    # (the skip pattern), so no per-value corrected computation is needed.
    data.all[, ctbc.p_plus := tbc.p_plus]
    data.all[, ctbc.p_minus := tbc.p_minus]
    data.all[ctbc.sd != tbc.sd & !is.na(sd.corr_plus),
             ctbc.p_plus := sd.corr_plus - rcmedian.sd]
    data.all[ctbc.sd != tbc.sd & !is.na(sd.corr_minus),
             ctbc.p_minus := sd.corr_minus - rcmedian.sd]
    # Drop intermediates: the cn.orig* scratch columns from
    # calc_and_recenter_z_scores, the raw perturbations, and the un-recentered
    # corrected perturbed z (all consumed above).
    drop_pert <- intersect(
      c("cn.orig_cdc", "cn.orig_who", "cn.orig",
        "p_plus", "p_minus", "sd.corr_plus", "sd.corr_minus"),
      names(data.all))
    if (length(drop_pert) > 0L) data.all[, (drop_pert) := NULL]

    # notification: ensure awareness of small subsets in data (verbose tier:
    # fires per batch/checkpoint, so it would repeat in the milestone view)
    if (verbose) {
      year.counts <- data.all[, .N, floor(agedays / 365.25)]
      if (year.counts[N < 100, .N] > 0) {
        message(.ts(), bp, "note: input data has at least one age-year with < 100 subjects")
      }
    }

    # safety check: treat observations where tbc.sd cannot be calculated as missing
    data.all[is.na(tbc.sd), exclude := 'Exclude-Missing']

    # HC > 5 years: the WHO HC z-score reference ends at 5 years, so sd.orig
    # (and thus tbc.sd) is NA past that point and the is.na(tbc.sd) safety check
    # just above would relabel those rows Exclude-Missing. This line re-asserts
    # Exclude-Not-Cleaned for all HC > 5y (the more informative code).
    data.all[param == "HEADCM" & agedays >= 5*365.25, exclude := 'Exclude-Not-Cleaned']

    # pediatric: cleanchild (most of steps) ----

    # NOTE: the rest of cleangrowth's steps are done through cleanchild().

    # Store off vars for later - CP
    # --- capture key checkpoint columns for later diagnostics ---
    checkpoint_cols <- c("id", "subjid", "param", "agedays",
                         "sd.orig", "sd.corr",
                         "tbc.sd", "ctbc.sd", "rcmedian.sd",
                         "tbc.p_plus", "tbc.p_minus",
                         "ctbc.p_plus", "ctbc.p_minus")
    checkpoint_cols <- intersect(checkpoint_cols, names(data.all))
    checkpoint_data <- data.all[, ..checkpoint_cols]
    # Store off vars for later - CP


    # optionally process batches in parallel
    if (verbose)
      message(.ts(), bp, sprintf("cleaning child (%s subjects)",
                                 format(data.all[, uniqueN(subjid)], big.mark = ",")))
    if (verbose && cores > 1L)
      message(.ts(), bp, sprintf("processing in %d parallel group(s)", cores))
    if (cores == 1) {
      ret.df <- cleanchild(
        data.all,
        log.path = log.path,
        quietly = quietly,
        verbose = verbose,
        parallel = parallel,
        measurement.to.z = measurement.to.z,
        ewma.fields = ewma.fields,
        cf_rescue = cf_rescue,
        cf_detail = cf_detail,
        qm_detail = qm_detail,
        piv.z.wt.high = piv.z.wt.high,
        piv.z.ht.low.young = piv.z.ht.low.young,
        piv.z.ht.low.old = piv.z.ht.low.old,
        piv.z.ht.high = piv.z.ht.high,
        piv.z.hc.low = piv.z.hc.low,
        piv.z.hc.high = piv.z.hc.high,
        exclude.levels = exclude.levels,
        tanner.ht.vel = tanner.ht.vel,
        error.load.threshold = error.load.threshold,
        error.load.mincount = error.load.mincount,
        ref.data.path = ref.data.path,
        ewma_window = ewma_window,
        ref_tables = ref_tables)
    } else {
      # create log directory if necessary
      if (!is.na(log.path)) {
        if (verbose)
          message(.ts(), sprintf("writing batch logs to '%s'", log.path))
        ifelse(!dir.exists(log.path), dir.create(log.path, recursive = TRUE), FALSE)
      }

      batch_list <- split(data.all, by = "batch")
      `%doit%` <- if (parallel) foreach::`%dopar%` else foreach::`%do%`
      results <- foreach::foreach(
        batch_df = batch_list,
        .packages = c("data.table", "growthcleanr")
      ) %doit% {
        cleanchild(
          batch_df,
          log.path = log.path,
          quietly = quietly,
          verbose = verbose,
          parallel = parallel,
          measurement.to.z = measurement.to.z,
          ewma.fields = ewma.fields,
          cf_rescue = cf_rescue,
          cf_detail = cf_detail,
          qm_detail = qm_detail,
          piv.z.wt.high = piv.z.wt.high,
          piv.z.ht.low.young = piv.z.ht.low.young,
          piv.z.ht.low.old = piv.z.ht.low.old,
          piv.z.ht.high = piv.z.ht.high,
          piv.z.hc.low = piv.z.hc.low,
          piv.z.hc.high = piv.z.hc.high,
          exclude.levels = exclude.levels,
          tanner.ht.vel = tanner.ht.vel,
          error.load.threshold = error.load.threshold,
          error.load.mincount = error.load.mincount,
          ref.data.path = ref.data.path,
          ewma_window = ewma_window,
          ref_tables = ref_tables
        )
      }
      ret.df <- rbindlist(results)
    }
  } else {
    ret.df <- data.table()
  }

  # adult: send to cleanadult to do most of the work ----

  # no need to do this if there's no data
  if (nrow(data.adult) > 0){
    if (verbose)
      message(.ts(), bp, sprintf("cleaning adult (%s subjects)",
                                 format(data.adult[, uniqueN(subjid)], big.mark = ",")))

    # this is where we do most of the adult work
    subjid.unique <- data.adult[j = unique(subjid)]
    batches.adult <- data.table(
      subjid = subjid.unique,
      newbatch = (seq_along(subjid.unique) - 1L) %% cores + 1L
    )
    data.adult <- merge(data.adult, batches.adult, by = "subjid")

    # add age in years
    data.adult[, age_years := agedays/365.25]
    # Adult length is cleaned as height, as for children (LENGTHCM -> HEIGHTCM;
    # LENGTHIN -> HEIGHTIN, which cleanadult() converts to cm itself). Relabel
    # only: length.adjust is pediatric-only. The verbatim param is restored at
    # output from param_input.
    data.adult[param == "LENGTHCM", param := "HEIGHTCM"]
    data.adult[param == "LENGTHIN", param := "HEIGHTIN"]
    # Provide measurement column expected by cleanadult(). Uses v_adult (original
    # units, pre-conversion) because cleanadult() handles imperial conversion
    # internally. The child path uses v (already converted to metric above).
    data.adult[, measurement := v_adult]

    if (cores == 1) {
      # do the cleaning
      res <- do.call(cleanadult, c(
        list(data.adult,
             permissiveness = adult_permissiveness,
             scale_max_lbs = adult_scale_max_lbs,
             ewma_window = ewma_window,
             quietly = quietly,
             verbose = verbose),
        adult_overrides))
    } else {
      batch_list_adult <- split(data.adult, by = "newbatch")
      `%doit%` <- if (parallel) foreach::`%dopar%` else foreach::`%do%`
      results_adult <- foreach::foreach(
        batch_df = batch_list_adult,
        .packages = c("data.table", "growthcleanr")
      ) %doit% {
        do.call(cleanadult, c(
          list(batch_df,
               permissiveness = adult_permissiveness,
               scale_max_lbs = adult_scale_max_lbs,
               ewma_window = ewma_window,
               quietly = quietly,
               verbose = verbose),
          adult_overrides))
      }
      res <- rbindlist(results_adult)
    }

    # Apply Exclude-Missing AFTER cleanadult() returns (adult PIV handles NAs).
    # This differs from the child path, where Exclude-Missing is set BEFORE
    # dispatch (lines ~1333, ~1337). Both are correct - the child algorithm
    # requires pre-set Missing codes for .child_valid(), while the adult
    # algorithm handles NAs internally via PIV step 1.
    res[is.na(measurement) | agedays < 0 | measurement < 0, result := "Exclude-Missing"]

    # Adult head circumference is not cleaned (the adult algorithm handles
    # height and weight only; HC is cleaned only through age 5). Flag it
    # Exclude-Not-Cleaned (reported as Exclude-HC-Out-of-Range), as for child
    # HC past 5 years, rather than returning it Include. cleanadult() never
    # reads HC rows, so this does not affect any height/weight result.
    res[param %in% c("HEADCM", "HEADIN"), result := "Exclude-Not-Cleaned"]
  } else {
    res <- data.table()
  }


  # --- FINAL RETURN (modified to return all columns) ---

  # combine pediatric and adult results if present
  if (any(nrow(data.all) > 0, nrow(data.adult) > 0)) {

    # join pediatric and adult outputs together
    # cf_rescued only exists in the child algorithm; use empty strings for legacy path
    cf_rescued_peds <- if (!is.null(ret.df$cf_rescued)) {
      as.character(ret.df$cf_rescued)
    } else {
      rep("", nrow(ret.df))
    }

    # adult-specific columns: mean_ht and bin_result (NA for child rows)
    adult_mean_ht <- if (nrow(res) > 0 && "mean_ht" %in% names(res)) {
      res$mean_ht
    } else {
      rep(NA_real_, nrow(res))
    }
    adult_bin_result <- if (nrow(res) > 0 && "bin_result" %in% names(res)) {
      res$bin_result
    } else {
      rep(NA_character_, nrow(res))
    }

    full_out <- data.table(
      line = c(ret.df$line, res$line),
      exclude = c(as.character(ret.df$exclude), res$result),
      cf_rescued = c(cf_rescued_peds, rep("", nrow(res))),
      mean_ht = c(rep(NA_real_, nrow(ret.df)), adult_mean_ht),
      bin_result = c(rep(NA_character_, nrow(ret.df)), adult_bin_result)
    )

    # Child-side values of an optional column: from ret.df when the child
    # algorithm produced it, else `fill` (a batch with no child rows has an
    # empty ret.df). Requested columns are always emitted, so adult-only input
    # returns the same columns as mixed input (adult rows get the adult values).
    child_col <- function(nm, fill) {
      if (nm %in% names(ret.df)) ret.df[[nm]] else rep(fill, nrow(ret.df))
    }

    # Add cf_detail columns
    if (cf_detail) {
      # Adult rows have no CF concept; label them "Adult" so the column has no NAs
      full_out[, cf_status := c(as.character(child_col("cf_status", NA_character_)),
                                rep("Adult", nrow(res)))]
      full_out[, cf_deltaz := c(child_col("cf_deltaz", NA_real_), rep(NA_real_, nrow(res)))]
      full_out[, cf_nextz  := c(child_col("cf_nextz",  NA_real_), rep(NA_real_, nrow(res)))]
      # Design-aid columns: NA for adult rows (no CF concept)
      full_out[, imperial     := c(child_col("imperial", NA),              rep(NA, nrow(res)))]
      full_out[, cf_dage_orig := c(child_col("cf_dage_orig", NA_integer_), rep(NA_integer_, nrow(res)))]
      full_out[, cf_dage_next := c(child_col("cf_dage_next", NA_real_),    rep(NA_real_, nrow(res)))]
    }

    # Add qm_detail eligibility flags (NA for adult rows)
    if (qm_detail) {
      for (qc in c("qm_elig_c_cf", "qm_elig_c_hardlimit", "qm_elig_c_eviltwins",
                   "qm_elig_c_trajextreme", "qm_elig_extraneous", "qm_elig_c_traj",
                   "qm_elig_c_absdiff", "qm_elig_c_pair", "qm_elig_c_single",
                   "qm_elig_c_trajuneval")) {
        full_out[, (qc) := c(child_col(qc, NA), rep(NA, nrow(res)))]
      }
    }

    # preserve original exclude levels
    full_out[, exclude := factor(exclude, levels = exclude.levels)]

    # join back to batch-level reference to preserve all columns
    all_results <- merge(
      data.batch,              # this batch's subjects (all ages)
      full_out,
      by = "line",
      all.x = TRUE,
      sort = FALSE
    )

    # Merge checkpoint diagnostics by ID (child algorithm only).
    # checkpoint_data uses the user's original id. Adult rows get NA for
    # checkpoint columns via all.x = TRUE, which is correct.
    if (exists("checkpoint_data") && nrow(checkpoint_data) > 0) {
      drop_cols <- intersect(c("subjid", "param", "agedays"), names(checkpoint_data))
      cp_merge <- checkpoint_data[, !..drop_cols]
      all_results <- merge(all_results, cp_merge, by = "id", all.x = TRUE)
    }

    # Rows with NA agedays fall through both splits (NA < cutpoint = NA;
    # NA >= cutpoint = NA) and are in data.batch but not full_out, so the
    # all.x merge above leaves their exclude as NA. Assign Exclude-Missing.
    all_results[is.na(exclude), exclude := factor("Exclude-Missing", levels = exclude.levels)]

    # Reorder to match data.batch row order (id is guaranteed unique by input contract)
    all_results <- all_results[match(data.batch$id, all_results$id)]

    # restore original row order
    setorder(all_results, line)

    # if you have an 'id' field in data.work, it will now be preserved automatically
    # (since it existed in the original input)

    results_list[[i]] <- all_results
    if (!quietly && !.checkpoint_driven && n_outer > 1L)
      message(.ts(), sprintf("batch %d/%d \u2014 done \u00b7 %s", i, n_outer,
                             .fmt_dur(difftime(Sys.time(), t_batch_start, units = "secs"))))
    i <- i + 1


  } else {
    # All rows fell through both sides of the child/adult split
    # (e.g. every row in this batch has agedays = NA). Build the result
    # from data.batch so the row structure is preserved, and assign
    # Exclude-Missing to every row.
    all_results <- copy(data.batch)
    all_results[, exclude    := factor("Exclude-Missing", levels = exclude.levels)]
    all_results[, cf_rescued := ""]
    all_results[, mean_ht    := NA_real_]
    all_results[, bin_result := NA_character_]
    setorder(all_results, line)
    results_list[[i]] <- all_results
    if (!quietly && !.checkpoint_driven && n_outer > 1L)
      message(.ts(), sprintf("batch %d/%d \u2014 done \u00b7 %s", i, n_outer,
                             .fmt_dur(difftime(Sys.time(), t_batch_start, units = "secs"))))
    i <- i + 1
  }

  }

  # --- Parallel teardown (once, after outer batch loop) ---
  if (parallel && cores > 1L) stopCluster(cl)

  all_results <- if (length(results_list) > 0L) {
    data.table::rbindlist(results_list, use.names = TRUE, fill = TRUE)
  } else {
    # Empty core: every row was set aside (missing structural key and/or a
    # non-gc param), so there is nothing to clean and the batch loop produced
    # no results. Seed an empty, line-keyed table; the set-aside rows are
    # rebound just below and become the entire output. (Without this, an
    # empty results_list yields a 0-column table and setorder(line) errors.)
    data.table(line = integer(0))
  }
  setorder(all_results, line)

  # Rebind the set-aside non-gc-param rows (separated at entry), with their
  # original measurement intact, assigning the internal Exclude-Not-GC-Param
  # code (remapped to the user-facing level below alongside everything else).
  if (!is.null(notgc_rows) && nrow(notgc_rows) > 0) {
    notgc_rows[, exclude := factor("Exclude-Not-GC-Param", levels = exclude.levels)]
    all_results <- data.table::rbindlist(list(all_results, notgc_rows),
                                         use.names = TRUE, fill = TRUE)
    setorder(all_results, line)
  }

  # Rebind the set-aside missing-structural-key rows (NA subjid / NA param),
  # assigning the internal Exclude-Missing code (remapped to Exclude-Missing-Info
  # below alongside everything else). Verbatim values are preserved.
  if (!is.null(misskey_rows) && nrow(misskey_rows) > 0) {
    misskey_rows[, exclude := factor("Exclude-Missing", levels = exclude.levels)]
    all_results <- data.table::rbindlist(list(all_results, misskey_rows),
                                         use.names = TRUE, fill = TRUE)
    setorder(all_results, line)
  }

  # Derive bin_exclude and tri_exclude from final exclude codes.
  # These are added after all processing (including partial-run merge)
  # so they are consistent regardless of code path.

  # Check for Exclude-C-Temp-Same-Day surviving to output -
  # this is an internal working code that should be resolved by Child Step 13.
  n_unresolved_sde <- sum(as.character(all_results$exclude) == "Exclude-C-Temp-Same-Day",
                          na.rm = TRUE)
  if (n_unresolved_sde > 0) {
    warning("Exclude-C-Temp-Same-Day found in final output (", n_unresolved_sde,
            " rows). This code should have been resolved during processing. ",
            "Unresolved rows reclassified as Exclude-C-Extraneous. ",
            "Please report this bug at https://github.com/growth-data-tools/growthcleanr/issues")
    all_results[as.character(exclude) == "Exclude-C-Temp-Same-Day",
                exclude := "Exclude-C-Extraneous"]
  }

  excl_char <- as.character(all_results$exclude)

  # bin_exclude: Include vs Exclude (always present)
  all_results[, bin_exclude := fifelse(excl_char == "Include", "Include", "Exclude")]

  # tri_exclude: Include vs Same-Day vs Exclude (opt-in via tri_exclude parameter)
  if (tri_exclude) {
    # Child SDE codes: Identical and Extraneous for each param
    # Adult SDE codes: same pattern with -A- prefix
    child_sde_codes <- c("Exclude-C-Identical", "Exclude-C-Extraneous")
    adult_sde_codes <- c("Exclude-A-Identical", "Exclude-A-Extraneous")
    # Summary-level names too, so cached rows (already remapped) from a
    # partial run are classified correctly.
    summary_sde_codes <- c("Exclude-Identical", "Exclude-Extraneous")
    sde_codes <- c(child_sde_codes, adult_sde_codes, summary_sde_codes)

    all_results[, tri_exclude := fifelse(
      excl_char == "Include", "Include",
      fifelse(excl_char %in% sde_codes, "Same-Day", "Exclude")
    )]
  }

  # Requested optional columns are always emitted. The per-batch assembly above
  # covers batches with cleaned rows; this backstop adds any requested column
  # still absent -- the child z-score columns when no row reached the child
  # algorithm (e.g. adult-only input), or every column when all rows fell
  # through the child/adult split -- as NA, the value adult rows get.
  .req_opt <- list()
  if (cf_detail) .req_opt <- c(.req_opt, list(
    cf_status = NA_character_, cf_deltaz = NA_real_, cf_nextz = NA_real_,
    imperial = NA, cf_dage_orig = NA_integer_, cf_dage_next = NA_real_))
  if (qm_detail) .req_opt <- c(.req_opt, sapply(
    c("qm_elig_c_cf", "qm_elig_c_hardlimit", "qm_elig_c_eviltwins",
      "qm_elig_c_trajextreme", "qm_elig_extraneous", "qm_elig_c_traj",
      "qm_elig_c_absdiff", "qm_elig_c_pair", "qm_elig_c_single",
      "qm_elig_c_trajuneval"), function(x) NA, simplify = FALSE))
  if (recenter_detail) .req_opt <- c(.req_opt, list(
    tbc.sd = NA_real_, ctbc.sd = NA_real_, rcmedian.sd = NA_real_))
  if (debug) .req_opt <- c(.req_opt, list(
    sd.orig = NA_real_, sd.corr = NA_real_, tbc.sd = NA_real_, ctbc.sd = NA_real_,
    tbc.p_plus = NA_real_, tbc.p_minus = NA_real_,
    ctbc.p_plus = NA_real_, ctbc.p_minus = NA_real_))
  for (nm in setdiff(unique(names(.req_opt)), names(all_results))) {
    all_results[, (nm) := .req_opt[[nm]]]
  }

  # Guarantee no NA cf_status reaches output: batches that fell through the
  # child/adult split (e.g. all-NA agedays) and any cache-merge gaps are filled.
  if (cf_detail && "cf_status" %in% names(all_results)) {
    all_results[is.na(cf_status), cf_status := "Not-CF"]
  }

  # corr_val / corr_pt (corr_detail): per-value and per-subject correction
  # labels. A child value is "Corrected" iff its recentered corrected z-score
  # differs from the uncorrected one (ctbc.sd != tbc.sd); a subject is
  # "Corrected" if any of its child values is. Adult rows (agedays >=
  # adult_cutpoint) are labeled "Adult", mirroring cf_status. agedays (not
  # z-score presence) determines child vs adult, so child Exclude-Missing rows
  # are correctly "Uncorrected", not "Adult".
  if (corr_detail) {
    adult_age <- adult_cutpoint * 365.25
    for (zc in c("tbc.sd", "ctbc.sd"))
      if (!zc %in% names(all_results)) all_results[, (zc) := NA_real_]
    all_results[, tmp_is_adult := !is.na(agedays) & agedays >= adult_age]
    all_results[, tmp_corr_val :=
                  !is.na(tbc.sd) & !is.na(ctbc.sd) & ctbc.sd != tbc.sd]
    all_results[, tmp_subj_corr := any(tmp_corr_val[!tmp_is_adult]), by = subjid]
    all_results[, corr_val := fcase(tmp_is_adult, "Adult",
                                    tmp_corr_val, "Corrected",
                                    default = "Uncorrected")]
    all_results[, corr_pt := fcase(tmp_is_adult, "Adult",
                                   tmp_subj_corr, "Corrected",
                                   default = "Uncorrected")]
    all_results[, c("tmp_is_adult", "tmp_corr_val", "tmp_subj_corr") := NULL]
  }

  # unit_error_range: informational flag (does NOT interact with exclude).
  # TRUE if the value's magnitude is consistent with a metric/imperial
  # unit-conversion error AND it is not itself within the plausible [zn3, zp3]
  # band. Computed only for rows gc cleans (Exclude-Missing / Exclude-Not-Cleaned
  # rows, and any row without a z3 lookup, get FALSE). On by default; set
  # unit_error_range = FALSE to suppress. NOT an exclusion code (not in
  # exclude.levels). See the ET revision spec, section 6.
  if (isTRUE(unit_error_range)) {
    all_results[, unit_error_range := .compute_unit_error_range(
      all_results, adult_cutpoint, ref.data.path)]
  }

  # gc_internal_id + id restoration:
  # gc_internal_id (when requested) is captured from the current `id` column - which is
  # the auto-regen 1:N when a regen happened, else the user's untouched id.
  #
  # The user-id restoration only fires when a regen actually replaced a
  # user-supplied id (id_was_regened == TRUE; set in the NA/dup branches at
  # entry - not in the NULL branch where there's nothing to restore, and not
  # in the valid-id branch where no regen happened). all_results rows are in
  # input row order (setorder by `line`), and user_id_orig is in input row
  # order, so the positional assignment aligns.
  all_results[, gc_internal_id := id]
  if (id_was_regened && !is.null(user_id_orig)) {
    all_results[, id := user_id_orig]
  }

  # --- Map internal step codes to user-facing output levels (final step) ---
  # `exclude` is reported at the Summary level (default); `exclude_detailed`
  # carries the step-level Detailed codes when requested. The mapping is
  # idempotent: any code already at a user-facing level (e.g. partial-run cached
  # rows) is left unchanged. Done last so all internal-code-dependent logic
  # above (Temp-Same-Day check, bin/tri_exclude, unit_error_range) sees the raw
  # codes. See vignettes/exclusion-codes.Rmd (vignette("exclusion-codes")).
  internal_excl <- as.character(all_results$exclude)
  summary_excl <- unname(exclude.map.summary[internal_excl])
  summary_excl[is.na(summary_excl)] <- internal_excl[is.na(summary_excl)]
  if (exclude_detail) {
    detailed_excl <- unname(exclude.map.detailed[internal_excl])
    detailed_excl[is.na(detailed_excl)] <- internal_excl[is.na(detailed_excl)]
    all_results[, exclude_detailed := factor(detailed_excl, levels = exclude.levels.detailed)]
  }
  all_results[, exclude := factor(summary_excl, levels = exclude.levels.summary)]

  # Systematic unit-error warning. If a large share of
  # a parameter's cleaned values fall in the unit-error range, the whole column
  # may be in the wrong unit (e.g. weight recorded in lbs but labeled WEIGHTKG,
  # or a bare column defaulted to metric that was actually imperial). Soft
  # warning per param group (weight / height / hc). Denominator = cleaned rows
  # (preprocessing set-asides excluded -- they are always FALSE); grouped by
  # gc_internal_param so imperial/length variants group with their metric target.
  if (isTRUE(unit_error_range) && "gc_internal_param" %in% names(all_results) &&
      "unit_error_range" %in% names(all_results)) {
    # Same non-evaluated set as .compute_unit_error_range (shared constant). The
    # `exclude` column is Summary-mapped by this point, so HC>5y rows are
    # `Exclude-HC-Out-of-Range`; the constant carries that mapped form so those
    # rows (always FALSE) stay out of the hc denominator.
    .uez <- all_results[!as.character(exclude) %in% .gc_unit_error_noneval & !is.na(gc_internal_param)]
    if (nrow(.uez) > 0L) {
      .grp <- vapply(as.character(.uez$gc_internal_param), .gc_param_group, character(1))
      .uer <- .uez$unit_error_range %in% TRUE
      .lab <- c(weight = "WEIGHTLBS", height = "HEIGHTIN", hc = "HEADIN")
      for (g in c("weight", "height", "hc")) {
        gi <- .grp == g & !is.na(.grp)
        n  <- sum(gi)
        if (n > 0L) {
          k <- sum(.uer[gi])
          if (k / n > 0.20) {
            warning(sprintf(
              "%.0f%% of %s values (%d of %d cleaned) fall in the unit-error range (magnitudes consistent with a metric/imperial mix-up). This may reflect frequent unit errors or may reflect measurements of people with atypical growth (for example, extremely premature infants, severe obesity, some medical conditions). If this parameter was recorded in imperial units, label it %s or map it with param_coding.",
              100 * k / n, g, k, n, .lab[[g]]))
          }
        }
      }
    }
  }

  # --- Always-on cleaning summary (standalone / non-checkpoint runs) --------
  # A short, reassuring headline of what growthcleanr did: per-parameter cleaned
  # vs. excluded counts, then an overall "not cleaned" (set aside as unusable)
  # count. Shown at the minimal (default) and verbose levels; gated on `!quietly`
  # so a fully quiet run (quietly = TRUE) is silent. Emitted here, after the Summary exclude
  # mapping (so `exclude` is user-facing) and before the verbatim param restore
  # below (so `param`/`gc_internal_param` are still canonical for grouping).
  # Skipped for checkpoint sub-calls (.checkpoint_driven -- the outer checkpoint
  # branch emits one summary).
  # `.det` (dataset-wide set-aside detection) and `.n_input` were computed above.
  if (!quietly && !.checkpoint_driven) {
    .grp_src <- if ("gc_internal_param" %in% names(all_results))
      as.character(all_results$gc_internal_param) else as.character(all_results$param)
    .grp <- vapply(.grp_src, .gc_param_group, character(1), USE.NAMES = FALSE)
    .nc  <- all_results$line %in% .det$lines
    message(paste(.gc_cleaning_summary_lines(
      .grp, as.character(all_results$exclude), .nc, .n_input), collapse = "\n"))
  }
  # One-time note for vector-style calls (not gated by quietly; see helper).
  if (.gc_vector_style && !.checkpoint_driven) .gc_vector_style_note()

  # Output fidelity: restore the user's verbatim submitted values as the primary
  # core columns, so gc never silently alters or drops an input value.
  #   measurement (v): v_adult is the verbatim measurement - restores a submitted
  #     0 (which v holds as NaN) and any original imperial value. gc's interpreted
  #     working value is internal-only and not surfaced here.
  #   sex: sex_input is the verbatim submission - restores an uninterpretable code
  #     (e.g. "U", which the coerced sex holds as NA). gc's interpreted integer
  #     sex (0/1/NA, used by the algorithm) is surfaced as gc_internal_sex under debug /
  #     full_detail. (Note: partial-run cached rows carry the user's column names,
  #     not v_adult/sex_input; their verbatim values were already restored when
  #     first produced, so the restore here is scoped to the freshly-cleaned rows.)
  #   param: param_input is the verbatim submission - restores a mapped label
  #     (e.g. "WT", "HEIGHTIN") that the algorithm resolved to a canonical param.
  #     gc's resolved/converted param is surfaced as gc_internal_param under
  #     internal_detail / full_detail.
  #   subjid: subjid_input is the verbatim submission - restores the user's
  #     original type (e.g. integer/character), which is factored internally for
  #     grouping/keying. Avoids the as.numeric(factor) trap for numeric ids.
  if ("v_adult" %in% names(all_results)) {
    all_results[!is.na(v_adult), v := v_adult]
  }
  #   A text or factor measurement is returned as submitted (whole-column
  #   replacement, so the column keeps the submitted type).
  if ("meas_input" %in% names(all_results)) {
    all_results[, v := meas_input]
  }
  if ("sex" %in% names(all_results)) {
    all_results[, gc_internal_sex := sex]
  }
  if ("sex_input" %in% names(all_results)) {
    all_results[, sex := sex_input]
  }
  if ("param_input" %in% names(all_results)) {
    all_results[, param := param_input]
  }
  if ("subjid_input" %in% names(all_results)) {
    all_results[, subjid := subjid_input]
  }
  #   agedays: agedays_input is the verbatim submission - restores a fractional
  #     age (e.g. 1435.4) that the algorithm cleaned as whole days (1435). The
  #     whole-day value gc used is surfaced as gc_internal_agedays under
  #     internal_detail / full_detail. Whole-column replacement (no `i`) so the
  #     column takes the submitted type rather than being coerced to integer.
  if ("agedays_input" %in% names(all_results)) {
    all_results[, gc_internal_agedays := agedays]
    all_results[, agedays := agedays_input]
  }

  # Carry-through columns are reattached AFTER the trim/rename below (when only
  # final output columns remain, so a carried column cannot collide with an
  # intermediate working column). The input row index (`line`) is taken here,
  # before the trim; row order is unchanged by the trim and rename.
  reattach_carry <- !is.null(.carry_spec) && ncol(.carry_spec) > 0L &&
    "line" %in% names(all_results)
  .carry_idx <- if (reattach_carry) all_results$line else NULL

  # Trim to public columns only
  # The added columns come from .gc_added_output_cols() (single source, also
  # used for the carry-through name check):
  #   mean_ht: adult algorithm's calculated mean height (cm), populated for adult
  #     height rows only (NA for child rows); by default returned only when the
  #     data has adult rows.
  #   recenter_detail: stable, named output contract for the recentered CSD
  #     z-scores (a subset of the debug set), so downstream tools can depend on
  #     tbc.sd / ctbc.sd / rcmedian.sd without relying on the diagnostic `debug`
  #     bundle; deduped against debug.
  #   internal_detail: gc's own working values for the key inputs the user
  #     supplied -- imperial converted to metric, LENGTHCM relabeled (the length
  #     offset is z-score-only and not reflected), sex coerced to 0/1, and the
  #     parsed dob/visit_date (Date) when supplied. The primary columns always
  #     carry the user's verbatim submission; these report what the algorithm used.
  #   Semi-wide output widens the emit sources after the trim, so they are kept.
  #   debug: intermediate diagnostic columns (incl. gc_internal_sex); intersect()
  #     below keeps only those actually present, so the set can grow without
  #     guarding each column here.
  keep_cols <- c("id", "subjid", "param", "agedays", "sex", "v",
                 .gc_added_output_cols(
                   exclude_detail = exclude_detail, tri_exclude = tri_exclude,
                   mean_ht = mean_ht, cf_detail = cf_detail,
                   qm_detail = qm_detail, corr_detail = corr_detail,
                   recenter_detail = recenter_detail,
                   unit_error_range = unit_error_range,
                   display_gc_id = display_gc_id,
                   internal_detail = internal_detail, debug = debug,
                   has_dob = "gc_internal_dob" %in% names(all_results),
                   has_visit_date = "gc_internal_visit_date" %in% names(all_results),
                   show_internal_agedays = .gc_show_int_age,
                   emit_cols = if (!is.null(.gc_emit))
                     vapply(.gc_emit, .gc_emit_src, character(1))))
  all_results <- all_results[, intersect(unique(keep_cols), names(all_results)), with = FALSE]

  # Rename output columns to the user's original variable names (if different).
  # "v" is always renamed: to the user's original measurement argument name if
  # resolvable, otherwise to "measurement".
  for (canon_nm in c("id", "subjid", "param", "agedays", "sex")) {
    user_nm <- user_col_names[[canon_nm]]
    if (!is.null(user_nm) && user_nm != canon_nm && canon_nm %in% names(all_results)) {
      setnames(all_results, canon_nm, user_nm)
    }
  }
  meas_out_nm <- user_col_names[["measurement"]]
  if (is.null(meas_out_nm)) meas_out_nm <- "measurement"
  if ("v" %in% names(all_results)) setnames(all_results, "v", meas_out_nm)

  # Reattach carried-through non-gc columns, appended after the gc columns in
  # their original input order. Rows are put in input order (.carry_idx, the
  # input row index taken before the trim) and each carried column is assigned
  # by that index -- no key column, so no user column name (`line` included,
  # as a role or a carried column) can collide with it. Only final output
  # columns remain, so carried names cannot shadow an intermediate working
  # column (and reserved-name collisions were renamed at input).
  if (reattach_carry) {
    o <- order(.carry_idx)
    all_results <- all_results[o]
    for (nm in names(.carry_spec)) all_results[, (nm) := .carry_spec[[nm]][.carry_idx[o]]]
  }

  # --- Output-format dispatch (Hook B) --------------------------------------
  # Re-widened output: hand back the user's own rows (visit rows for semi-wide,
  # subject rows for wide) with the requested growthcleanr columns slotted in
  # beside each value column. See R/reshape_adapters.R.
  # Long output from semi-wide/wide input: the user's id (if any) under its own
  # column name, growthcleanr's minted per-measurement id as gc_internal_id.
  if (.gc_crossformat)
    all_results <- .gc_crossformat_id_cols(all_results, .gc_user_id, .gc_user_id_name,
                                           .gc_xf_extra, .gc_xf_roles)
  if (!is.null(.gc_reshape_spec)) {
    refn <- if (identical(.gc_reshape_spec$format, "wide")) .gc_wide_output else .gc_semiwide_output
    all_results <- refn(
      all_results, .gc_reshape_spec, emit = .gc_emit,
      carry_through = isTRUE(.gc_reshape_spec$carry_through))
  }

  # Grand total (standalone runs only; under checkpoint the wrapper prints it).
  if (verbose && !.checkpoint_driven)
    message(.ts(), sprintf("done \u2014 %s subjects in %s",
                           format(nrow(patients), big.mark = ","),
                           .fmt_dur(difftime(Sys.time(), t_run_start, units = "secs"))))

  return(all_results)
}

#' Function to calculate z-scores and csd-scores based on anthro tables.
#'
#' @param path Path to supplied reference anthro data. Defaults to package anthro tables.
#' @param cdc.only Whether or not only CDC data should be used. Defaults to false.
#'
#' @return A function \code{f(param, agedays, sex, measurement, csd = FALSE)}
#'   that returns, for each measurement, its LMS z-score (default) or, when
#'   \code{csd = TRUE}, its CSD ("standard deviation") score against the
#'   selected reference (WHO below age 5 years and CDC from age 5 years, or CDC
#'   at all ages when \code{cdc.only = TRUE}; the 2-5 year WHO/CDC blending used
#'   by cleangrowth() is not applied here).
#' @export
#' @import data.table
#' @importFrom utils read.csv read.table
#' @examples
#' \donttest{
#' # Return calculating function with all defaults
#' afunc <- read_anthro()
#'
#' # Return calculating function while specifying a path and using only CDC data
#' afunc <- read_anthro(path = system.file("extdata", package = "growthcleanr"),
#'                      cdc.only = TRUE)
#' }
read_anthro <- function(path = "", cdc.only = FALSE) {
  # avoid "no visible bindings" warning
  src <- param <- sex <- age <- ret <- m <- NULL
  csdneg <- csdpos <- s <- NULL

  # Child algorithm reference tables (consolidated WHO + infants CDC)
  weianthro_path <- lenanthro_path <- bmianthro_path <-
    ifelse(
      path == "",
      system.file(file.path("extdata", "growthfile_who.csv.gz"), package = "growthcleanr"),
      file.path(path, "growthfile_who.csv.gz")
    )
  growth_cdc_ext_path <- ifelse(
    path == "",
    system.file(file.path("extdata", "growthfile_cdc_ext_infants.csv.gz"), package = "growthcleanr"),
    file.path(path, "growthfile_cdc_ext_infants.csv.gz")
  )
  growth_cdc_ext <- read.csv(gzfile(growth_cdc_ext_path))

  l <- list(
      with(
        read.csv(gzfile(weianthro_path), header = TRUE),
        data.frame(
          src = 'WHO',
          param = 'WEIGHTKG',
          sex,
          age = agedays,
          l = who_wt_l,
          m = who_wt_m,
          s = who_wt_s,
          csdpos = who_wt_csd_pos,
          csdneg =  who_wt_csd_neg
        )
      ),
      with(
        read.csv(gzfile(lenanthro_path), header = TRUE),
        data.frame(
          src = 'WHO',
          param = 'HEIGHTCM',
          sex,
          age = agedays,
          l = who_ht_l,
          m = who_ht_m,
          s = who_ht_s,
          csdpos = who_ht_csd_pos,
          csdneg =  who_ht_csd_neg
        )
      ),
      with(
        read.csv(gzfile(lenanthro_path), header = TRUE),
        data.frame(
          src = 'WHO',
          param = 'HEADCM',
          sex,
          age = agedays,
          l = who_hc_l,
          m = who_hc_m,
          s = who_hc_s,
          csdpos = who_hc_csd_pos,
          csdneg =  who_hc_csd_neg
        )
      ),
      with(
        read.csv(gzfile(bmianthro_path), header = TRUE),
        data.frame(
          src = 'WHO',
          param = 'BMI',
          sex,
          age = agedays,
          l = who_bmi_l,
          m = who_bmi_m,
          s = who_bmi_s,
          csdpos = who_bmi_csd_pos,
          csdneg =  who_bmi_csd_neg
        )
      ),
      with(
        growth_cdc_ext,
        data.frame(
          src = 'CDC',
          param = 'WEIGHTKG',
          sex,
          age = agedays,
          l = cdc_wt_l,
          m = cdc_wt_m,
          s = cdc_wt_s,
          csdpos = cdc_wt_csd_pos,
          csdneg = cdc_wt_csd_neg
        )
      ),
      with(
        growth_cdc_ext,
        data.frame(
          src = 'CDC',
          param = 'HEIGHTCM',
          sex,
          age = agedays,
          l = cdc_ht_l,
          m = cdc_ht_m,
          s = cdc_ht_s,
          csdpos = cdc_ht_csd_pos,
          csdneg = cdc_ht_csd_neg
        )
      ),
      with(
        growth_cdc_ext,
        data.frame(
          src = 'CDC',
          param = 'HEADCM',
          sex,
          age = agedays,
          l = cdc_hc_l,
          m = cdc_hc_m,
          s = cdc_hc_s,
          csdpos = cdc_hc_csd_pos,
          csdneg = cdc_hc_csd_neg
        )
      ),
      with(
        growth_cdc_ext,
        data.frame(
          src = 'CDC',
          param = 'BMI',
          sex,
          age = agedays,
          l = cdc_bmi_l,
          m = cdc_bmi_m,
          s = cdc_bmi_s,
          csdpos = cdc_bmi_csd_pos,
          csdneg = cdc_bmi_csd_neg
        )
      )
    )

  anthro <- rbindlist(l)


  setkey(anthro, src, param, sex, age)

  return(function(param, agedays, sex, measurement, csd = FALSE) {
    # WHO data available up to 5 years for height/weight, needed for 2-5y smoothing
    src <- ifelse(agedays < 5*365.25 & !cdc.only, 'WHO', 'CDC')

    # keep column sequence the same fo efficient join
    dt <- data.table(src, param, sex, agedays, measurement)
    dt <- anthro[dt]

    dt[, ret := as.double(NA)]
    if (csd) {
      dt[measurement < m, ret := (measurement - m) / csdneg]
      dt[measurement >= m, ret := (measurement - m) / csdpos]
    } else {
      dt[l == 0, ret := log(measurement / m) / s]
      dt[l != 0, ret := (((measurement / m) ^ l) - 1) / (l * s)]
    }

    return(dt$ret)
  })
}

#' Pre-load growthcleanr reference closures
#'
#' \code{gc_preload_refs} loads all reference table closures used by
#' \code{\link{cleangrowth}} once, avoiding repeated disk reads across
#' repeated calls (e.g., simulation loops). Pass the result as the
#' \code{ref_tables} argument to \code{\link{cleangrowth}}.
#'
#' @param path Path to the reference data directory. Defaults to the package
#'   extdata directory. Pass an explicit path only if using custom reference
#'   files.
#'
#' @return A named list with two \code{read_anthro} closures:
#'   \describe{
#'     \item{mtz_cdc_prelim}{CDC-only (used by child algorithm)}
#'     \item{mtz_who_prelim}{WHO+CDC (used by child algorithm)}
#'   }
#'
#' @export
#' @examples
#' \donttest{
#' refs <- gc_preload_refs()
#' result <- cleangrowth(
#'   subjid = syngrowth$subjid,
#'   param  = syngrowth$param,
#'   agedays = syngrowth$agedays,
#'   sex = syngrowth$sex,
#'   measurement = syngrowth$measurement,
#'   ref_tables = refs
#' )
#' }
gc_preload_refs <- function(path = "") {
  list(
    mtz_cdc_prelim = read_anthro(path, cdc.only = TRUE),
    mtz_who_prelim = read_anthro(path, cdc.only = FALSE)
  )
}

# Convert vector of ages (days) into pairwise absolute-difference matrix.
# Used by ewma() for age-gap weighting.
as_matrix_delta <- function(agedays) {
  n <- length(agedays)
  delta <- abs(matrix(rep(agedays, n), n, byrow = TRUE) - agedays)
  return(delta)
}

#' Exponentially Weighted Moving Average (EWMA)
#'
#' \code{ewma} calculates the exponentially weighted moving average (EWMA) for a set of numeric observations over time.
#'
#' @param agedays Vector of age in days for each z score (potentially transformed to adjust weighting).
#'
#' @param z Input vector of numeric z-score data.
#'
#' @param ewma.exp Exponent used for age-gap weighting. May be a single
#'   scalar (recycled across all observations) or a per-observation vector
#'   matched to \code{agedays} / \code{z}; the algorithm's main internal
#'   callers pass a per-observation vector that varies the exponent by the
#'   widest neighbor age gap.
#'
#' @param ewma.adjacent Specify whether EWMA values excluding adjacent measurements should be calculated.  Defaults to TRUE.
#'
#' @param window Maximum number of observations on each side for EWMA weighting.
#'   Default 15. Set to Inf to disable windowing.
#'
#' @param cache_env Optional environment for caching EWMA intermediate results.
#'   Used internally for performance. Defaults to NULL.
#'
#' @return A named list of numeric vectors, each the same length as
#'   \code{agedays}.
#' * \code{ewma.all} - EWMA at each observation's age, excluding only the
#'   observation itself.
#' * \code{ewma.before} - also excludes the immediate prior observation
#'   (sorted by \code{agedays}); equals \code{ewma.all} for the first
#'   observation and when \code{n <= 2}.
#' * \code{ewma.after} - also excludes the immediate subsequent observation;
#'   equals \code{ewma.all} for the last observation and when \code{n <= 2}.
#'
#' When \code{ewma.adjacent = FALSE}, only \code{ewma.all} is returned (still
#'   wrapped in a one-element named list).
#'@md
#'
#' @export
#' @examples
#' # Run on 1 subject, 1 type of parameter
#' df_stats <- as.data.frame(syngrowth)
#' df_stats <- df_stats[df_stats$subjid == df_stats$subjid[1] &
#'                        df_stats$param == "HEIGHTCM", ]
#'
#' # Get the uncentered z-scores
#' measurement_to_z <- read_anthro(cdc.only = TRUE)
#' sd <- measurement_to_z(df_stats$param,
#'                        df_stats$agedays,
#'                        df_stats$sex,
#'                        df_stats$measurement,
#'                        TRUE)
#'
#' # Calculate exponentially weighted moving average
#' e_df <- ewma(df_stats$agedays, sd, ewma.exp = -1.5)
ewma <- function(agedays, z, ewma.exp, ewma.adjacent = TRUE, window = 15, cache_env = NULL) {
  # For each observation i in (agedays, z), compute a weighted average of the
  # other observations' z-values using weights w_ij = (5 + |agedays_i - agedays_j|) ^ ewma.exp_i,
  # with w_ii = 0 so observation i is excluded from its own EWMA. The "+5"
  # prevents nearly-coincident agedays from blowing up the weight; the negative
  # exponent (typically -1.5 to -3.5) damps the contribution of distant ages.
  #
  # Three EWMA variants are returned (when ewma.adjacent = TRUE) so that
  # callers can decide which neighbors to suppress when checking whether an
  # observation is consistent with its trajectory:
  #   ewma.all     EWMA of all other observations
  #   ewma.before  also drops obs i-1 (immediate predecessor by agedays)
  #   ewma.after   also drops obs i+1 (immediate successor by agedays)
  # For the first/last observation, ewma.before/ewma.after equal ewma.all
  # (no predecessor/successor exists). For n <= 2, both equal ewma.all.
  #
  # Caller supplies one ewma.exp value per observation; sweep() applies the
  # exponent row-wise so each row of the weight matrix uses that row's
  # observation's exponent. A scalar ewma.exp is recycled by sweep() and
  # works for the simple single-call form shown in the example.
  #
  # window limits the EWMA at each observation to the +/-window nearest
  # positions (default 15); set window = Inf to disable.
  #
  # cache_env (optional environment) lets callers reuse the windowed weight
  # matrix across paired calls on the same agedays + exponents (e.g.,
  # tbc.sd then ctbc.sd) to avoid recomputing the matrix.

  n <- length(agedays)
  # initialize response variables
  ewma.all <- ewma.before <- ewma.after <- vector('numeric', 0)
  if (n > 0) {
    # organize into data frame and sort into order of increasing age,
    # but retain original sort order information in index
    if (!all(agedays == cummax(agedays)))
      warning("EWMA ordering is not sorted; double check") #add in a check to make sure the inputs are already sorted (they should be)
    index <- order(agedays)

    # Use cached delta matrix if available, otherwise compute and optionally cache
    if (!is.null(cache_env) && !is.null(cache_env$delta)) {
      delta <- cache_env$delta
    } else {
      # Build weight matrix: row-wise exponent application means each
      # observation's EWMA uses that observation's own exponent for all
      # weights in its row. sweep(..., 1, ...) targets margin 1 (rows).
      delta <- as_matrix_delta(agedays)
      delta <- sweep(delta + 5, 1, ewma.exp, FUN = "^")
      diag(delta) <- 0  # self-weight is zero so obs i is excluded from its own EWMA

      # Apply windowing: zero out weights beyond window positions.
      if (!is.null(window) && is.finite(window)) {
        pos_diff <- abs(outer(1:n, 1:n, "-"))  # position difference matrix
        delta[pos_diff > window] <- 0
      }

      # Cache delta if environment provided
      if (!is.null(cache_env)) cache_env$delta <- delta
    }

    # Compute main EWMA using matrix multiply
    weighted_sums <- as.vector(delta %*% z)
    row_sums <- rowSums(delta)
    ewma.all[index] <- weighted_sums / row_sums

    if (ewma.adjacent) {
      if (n > 2) {
        # Before / After: subtract predecessor's / successor's contribution
        # from each observation's weighted sum and row sum directly. This
        # is O(n) per variant - no need to rebuild a separate weight matrix
        # for each adjacent-neighbor exclusion.
        pred_weights <- c(0, delta[cbind(2:n, 1:(n-1))])  # subdiagonal: delta[i, i-1]
        pred_z <- c(0, z[1:(n-1)])
        ewma.before[index] <- (weighted_sums - pred_weights * pred_z) / (row_sums - pred_weights)

        succ_weights <- c(delta[cbind(1:(n-1), 2:n)], 0)  # superdiagonal: delta[i, i+1]
        succ_z <- c(z[2:n], 0)
        ewma.after[index] <- (weighted_sums - succ_weights * succ_z) / (row_sums - succ_weights)
      } else {
        ewma.before <- ewma.after <- ewma.all
      }
    }
  }
  # Return as a named list (not a data.frame): list is data.table :=
  # compatible and avoids data.frame construction overhead in the per-group
  # callers in Steps 11, 13, 15/16, and 17.
  return(if (ewma.adjacent)
    list(ewma.all = ewma.all, ewma.before = ewma.before, ewma.after = ewma.after)
    else
      list(ewma.all = ewma.all))
}

# --- Incremental EWMA cache functions ---
# These allow O(n) updates when observations are excluded from EWMA groups,
# instead of O(n^2) full rebuild each iteration.

#' Create initial EWMA cache for a group (O(n^2) - first iteration only)
#' @keywords internal
#' @noRd
ewma_cache_init <- function(agedays, z_tbc, z_ctbc, exp_vals, ids, window = 15) {
  n <- length(agedays)

  # Check if ctbc == tbc (skip redundant ctbc computation for most groups)
  skip_ctbc <- identical(z_tbc, z_ctbc)

  # Build delta (weight) matrix
  delta <- as_matrix_delta(agedays)
  delta <- sweep(delta + 5, 1, exp_vals, FUN = "^")
  diag(delta) <- 0

  # Apply windowing
  if (!is.null(window) && is.finite(window)) {
    pos_diff <- abs(outer(1:n, 1:n, "-"))
    delta[pos_diff > window] <- 0
  }

  # Compute weighted sums and row sums
  ws_tbc <- as.vector(delta %*% z_tbc)
  rs <- rowSums(delta)

  # Only compute ctbc weighted sums if ctbc differs from tbc
  if (skip_ctbc) {
    ws_ctbc <- ws_tbc
  } else {
    ws_ctbc <- as.vector(delta %*% z_ctbc)
  }

  # Compute EWMA values
  ewma.all <- ws_tbc / rs
  c.ewma.all <- if (skip_ctbc) ewma.all else ws_ctbc / rs

  if (n > 2) {
    pred_w <- c(0, delta[cbind(2:n, 1:(n - 1))])
    pred_z <- c(0, z_tbc[1:(n - 1)])
    ewma.before <- (ws_tbc - pred_w * pred_z) / (rs - pred_w)
    succ_w <- c(delta[cbind(1:(n - 1), 2:n)], 0)
    succ_z <- c(z_tbc[2:n], 0)
    ewma.after <- (ws_tbc - succ_w * succ_z) / (rs - succ_w)
  } else {
    ewma.before <- ewma.after <- ewma.all
  }

  list(
    delta = delta, ws_tbc = ws_tbc, ws_ctbc = ws_ctbc, rs = rs,
    ids = ids, agedays = agedays, exp_vals = exp_vals,
    z_tbc = z_tbc, z_ctbc = z_ctbc, window = window,
    skip_ctbc = skip_ctbc,
    ewma.all = ewma.all, ewma.before = ewma.before,
    ewma.after = ewma.after, c.ewma.all = c.ewma.all
  )
}

#' Update EWMA cache after excluding one observation (O(n) instead of O(n^2))
#' @keywords internal
#' @noRd
ewma_cache_update <- function(cache, excluded_id) {
  pos_j <- which(cache$ids == excluded_id)
  if (length(pos_j) != 1L) {
    # Cache mismatch - return NULL to signal caller should do full rebuild
    return(NULL)
  }

  n <- length(cache$ids)
  new_n <- n - 1L
  keep <- seq_len(n) != pos_j
  skip_ctbc <- isTRUE(cache$skip_ctbc)

  # Remove j's contribution from weighted sums and row sums - O(n)
  col_j <- cache$delta[keep, pos_j]
  ws_tbc <- cache$ws_tbc[keep] - col_j * cache$z_tbc[pos_j]
  rs <- cache$rs[keep] - col_j

  # Only update ctbc sums if ctbc differs from tbc
  if (skip_ctbc) {
    ws_ctbc <- ws_tbc
  } else {
    ws_ctbc <- cache$ws_ctbc[keep] - col_j * cache$z_ctbc[pos_j]
  }

  # Remove row/col from delta matrix
  delta <- cache$delta[keep, keep, drop = FALSE]
  agedays <- cache$agedays[keep]
  z_tbc <- cache$z_tbc[keep]
  z_ctbc <- cache$z_ctbc[keep]
  ids <- cache$ids[keep]
  exp_vals <- cache$exp_vals[keep]
  window <- cache$window

  # Check if neighbors' exponents changed after removal.
  # Must check up to 2 positions on each side in the new index:
  # removing pos_j changes the max-gap for pos_j-1 (now adjacent
  # to pos_j-2 and pos_j) AND for pos_j-2 (whose gap-after is
  # now to pos_j instead of pos_j-1). Same logic applies on the
  # other side.
  neighbors <- integer(0)
  if (pos_j > 2L)     neighbors <- c(neighbors, pos_j - 2L)
  if (pos_j > 1L)     neighbors <- c(neighbors, pos_j - 1L)
  if (pos_j <= new_n) neighbors <- c(neighbors, pos_j)
  if (pos_j < new_n)  neighbors <- c(neighbors, pos_j + 1L)
  neighbors <- unique(
    neighbors[neighbors >= 1L & neighbors <= new_n])

  for (nb in neighbors) {
    # Recompute max gap for this neighbor
    diff_before <- if (nb > 1L) abs(agedays[nb] - agedays[nb - 1L]) else NA
    diff_after <- if (nb < new_n) abs(agedays[nb + 1L] - agedays[nb]) else NA
    maxdiff <- max(diff_before, diff_after, na.rm = TRUE)
    ageyears <- maxdiff / 365.25
    new_exp <- if (ageyears <= 1) -1.5 else if (ageyears >= 3) -3.5 else -1.5 - (ageyears - 1)

    if (abs(new_exp - exp_vals[nb]) > 1e-10) {
      # Exponent changed - rebuild this row of delta and recompute its sums
      exp_vals[nb] <- new_exp
      new_row <- (abs(agedays - agedays[nb]) + 5) ^ new_exp
      new_row[nb] <- 0  # self-weight

      # Apply windowing
      if (!is.null(window) && is.finite(window)) {
        pos_diffs <- abs(seq_len(new_n) - nb)
        new_row[pos_diffs > window] <- 0
      }

      delta[nb, ] <- new_row
      ws_tbc[nb] <- sum(new_row * z_tbc)
      if (!skip_ctbc) ws_ctbc[nb] <- sum(new_row * z_ctbc)
      rs[nb] <- sum(new_row)
    }
  }

  # If skip_ctbc, keep ws_ctbc in sync after neighbor rebuilds
  if (skip_ctbc) ws_ctbc <- ws_tbc

  # Compute EWMA values from cached sums - O(n)
  ewma.all <- ws_tbc / rs
  c.ewma.all <- if (skip_ctbc) ewma.all else ws_ctbc / rs

  if (new_n > 2L) {
    pred_w <- c(0, delta[cbind(2:new_n, 1:(new_n - 1L))])
    pred_z <- c(0, z_tbc[1:(new_n - 1L)])
    ewma.before <- (ws_tbc - pred_w * pred_z) / (rs - pred_w)
    succ_w <- c(delta[cbind(1:(new_n - 1L), 2:new_n)], 0)
    succ_z <- c(z_tbc[2:new_n], 0)
    ewma.after <- (ws_tbc - succ_w * succ_z) / (rs - succ_w)
  } else {
    ewma.before <- ewma.after <- ewma.all
  }

  list(
    delta = delta, ws_tbc = ws_tbc, ws_ctbc = ws_ctbc, rs = rs,
    ids = ids, agedays = agedays, exp_vals = exp_vals,
    z_tbc = z_tbc, z_ctbc = z_ctbc, window = window,
    skip_ctbc = skip_ctbc,
    ewma.all = ewma.all, ewma.before = ewma.before,
    ewma.after = ewma.after, c.ewma.all = c.ewma.all
  )
}

#### CP ####

# Infants support

# supporting (all) ---

#' Look up the designated other parameter (DOP) for a growth parameter.
#'
#' Maps a single parameter name to its paired "other parameter" used in
#' cross-parameter plausibility checks (Child Steps 5, 6, 11, 13, 15/16,
#' 19). The DOP assignments are: WEIGHTKG -> HEIGHTCM (height is weight's
#' anchor), HEIGHTCM -> WEIGHTKG (weight is height's anchor), and
#' HEADCM -> HEIGHTCM (height is HC's anchor; there is no reverse
#' mapping).
#'
#' Scalar-only: the internal `if / else if / else` chain requires a
#' scalar logical test, so callers must pass a scalar param string. In
#' practice both call sites pass `df$param[1]` from a single-param
#' working subset.
#'
#' @param param_name character scalar: one of `"WEIGHTKG"`,
#'   `"HEIGHTCM"`, or `"HEADCM"`. No validation - anything other than
#'   `"WEIGHTKG"` or `"HEIGHTCM"` falls through to the HEADCM branch
#'   and returns `"HEIGHTCM"`.
#'
#' @return Character scalar: the DOP for the given param.
#'
#' @keywords internal
#' @noRd
get_dop <- function(param_name){
  dop <- if (param_name == "WEIGHTKG"){
    "HEIGHTCM"
  } else if (param_name == "HEIGHTCM"){
    "WEIGHTKG"
  } else { # HEADCM
    "HEIGHTCM"
  }

  return(dop)
}

# temporary extraneous ----
#' Identify temporary SDE (same-day extraneous) "losers" on each
#' (subjid, param, agedays) group.
#'
#' Identical same-day values are removed earlier (Early Step 13
#' SDE-Identicals), so by the time identify_temp_sde() runs, all
#' same-day values in a given (subjid, param, agedays) group are
#' dissimilar. For each such group with more than one valid
#' measurement, selects one value to keep temporarily and flags the
#' remainder for exclusion. Selection uses the recentered z-score
#' (tbc.sd) and two per-subject reference points:
#'   median.spz  - median of tbc.sd for the same (subjid, param),
#'                 taken over valid rows (all non-excluded
#'                 measurements).
#'   median.dopz - median of tbc.sd for the subject's designated other
#'                 parameter (DOP).
#'
#' For each SDE group, the kept row is the one whose tbc.sd is closest
#' to median.spz, provided the subject has at least one non-SDE day.
#' Otherwise the row closest to median.dopz is kept. If no DOP data
#' exists for the subject, one row is chosen at random.
#'
#' Called from Steps 5, 6, 7, 9, 11 (mid-loop and end-of-step), and 13
#' of \code{cleanchild()}. All callers except Step 13 final SDE pass
#' \code{exclude_from_dop_ids = NULL}; the Step 13 caller passes the
#' ids of current temp SDEs so those rows do not contribute to the DOP
#' median anchor used to resolve remaining same-day groups.
#'
#' @param df data.table column-subset containing id, internal_id,
#'   subjid, param, agedays, tbc.sd, and exclude. Not mutated.
#' @param exclude_from_dop_ids optional vector of \code{id} values to
#'   remove from the DOP median calculation only (they still contribute
#'   to the SP median). NULL by default; non-NULL only at Step 13.
#' @return A logical vector aligned to the caller's input row order.
#'   TRUE marks rows the caller should flag
#'   \code{'Exclude-C-Temp-Same-Day'}; FALSE means leave the exclude
#'   column unchanged.
#'
#' @keywords internal
#' @noRd

identify_temp_sde <- function(df, exclude_from_dop_ids = NULL) {
  # exclude_from_dop_ids: when non-NULL, the passed ids are removed from the
  # DOP median calculation (but still contribute to the SP median). Only the
  # Step 13 caller passes ids; all earlier callers (Step 5 initial pass and
  # the recalc callers in Steps 6, 7, 9, and 11) leave it NULL.

  # avoid "no visible binding" warnings
  agedays <- absdmedian.spz <- absdmedian.dopz <- extraneous <- NULL
  extraneous.this.day <- index <- median.spz <- median.dopz <- NULL
  param <- subjid <- tbc.sd <- internal_id <- NULL
  median_keep <- prov_med <- is_far <- n_far <- exclude <- NULL

  # Make copy before modifying to avoid data.table alloccol error
  df <- copy(df)

  # Record the caller's original row positions before any reordering, so the
  # final result can be mapped back. Row ordering is set deterministically
  # below by explicit order(subjid, param, agedays, internal_id).
  df[, orig_row := .I]

  # Narrow df to the columns we need, grouped by (subjid, param,
  # agedays) and then sorted with internal_id as the tiebreaker so
  # same-day rows have a deterministic order for the winner selection
  # below.
  df <- df[, .(tbc.sd, exclude, id, internal_id, orig_row), by = .(subjid, param, agedays)]
  df <- df[order(subjid, param, agedays, internal_id)]

  # Compute valid.rows on the sorted data.
  valid.rows <- .child_valid(df, include.temporary.extraneous = TRUE)

  # initialize fields
  df[, `:=`(
    median.spz = as.double(NA),
    median.dopz = as.double(NA),
    absdmedian.spz = as.double(NA),
    absdmedian.dopz = as.double(NA),
    extraneous.this.day = FALSE,
    extraneous = FALSE
  )]

  # determine on which days there are extraneous measurements (more than 1 valid measurement on that age day)
  df[valid.rows, extraneous.this.day := (.N > 1), by = .(subjid, param, agedays)]

  # DOP (designated other parameter) mapping
  dop_map <- c(
    "HEIGHTCM" = "WEIGHTKG",
    "WEIGHTKG" = "HEIGHTCM",
    "HEADCM" = "HEIGHTCM"
  )

  # Robust SP median: a single clearly-extreme Include value (often a value not
  # yet excluded by a later trajectory step) distorts the SDE anchor and can make
  # the wrong same-day value win. Compare each Include value to the provisional
  # group median; if EXACTLY ONE Include value is > 3 from it, drop that value
  # from the median computation. far is evaluated for Include rows only; a value
  # is still itself SDE-evaluated -- it is only barred from anchoring the median.
  df[, median_keep := valid.rows]
  df[valid.rows, prov_med := median(tbc.sd, na.rm = TRUE), by = .(subjid, param)]
  df[, is_far := valid.rows & exclude == "Include" &
       !is.na(tbc.sd) & !is.na(prov_med) & abs(tbc.sd - prov_med) > 3]
  df[valid.rows, n_far := sum(is_far), by = .(subjid, param)]
  df[is_far & n_far == 1L, median_keep := FALSE]

  # Assign each valid row its SP median: median of tbc.sd within
  # (subjid, param), computed over kept valid rows (the lone far Include, if any,
  # is excluded from the median but still receives one).
  df[valid.rows, median.spz := median(tbc.sd[median_keep], na.rm = TRUE), by = .(subjid, param)]

  # Compact (subjid, param) -> median.spz lookup table. Used below to
  # anchor each row's DOP median to the median of its other parameter.
  sp_medians <- df[valid.rows & !is.na(median.spz),
                   .(median.spz = median.spz[1]),
                   by = .(subjid, param)]

  # Assign each row its DOP median. When exclude_from_dop_ids is NULL
  # (all callers except Step 13) the DOP median is just the SP median
  # of the other parameter. When non-NULL (Step 13), recompute medians
  # for each param excluding the passed ids first, so the Step 13 final
  # SDE resolution is not biased by rows that are themselves under
  # consideration as temp SDEs.
  df[, median.dopz := as.double(NA)]

  if (is.null(exclude_from_dop_ids)) {
    # Default path (all callers except Step 13): reuse sp_medians
    # directly as each parameter's DOP median lookup.
    for (p in c("WEIGHTKG", "HEIGHTCM", "HEADCM")) {
      dop <- dop_map[p]
      dop_medians <- sp_medians[param == dop, .(subjid, dop_median = median.spz)]
      dop_lookup <- setNames(dop_medians$dop_median, dop_medians$subjid)
      df[param == p, median.dopz := dop_lookup[as.character(subjid)]]
    }
  } else {
    # Step 13 path: recompute per-(subjid, param) medians excluding the
    # passed ids, then use those as the DOP median lookup. Also drop the lone
    # far Include value (median_keep) so the DOP anchor is cleaned consistently
    # with median.spz above.
    dop_valid_rows <- valid.rows & !(df$id %in% exclude_from_dop_ids) & df$median_keep

    # Calculate DOP-specific medians excluding temp SDEs
    df[, median.dopz.calc := as.double(NA)]
    df[dop_valid_rows, median.dopz.calc := median(tbc.sd, na.rm = TRUE), by = .(subjid, param)]

    # Create lookup table for DOP medians (excludes temp SDEs)
    dop_sp_medians <- df[dop_valid_rows & !is.na(median.dopz.calc),
                         .(median.spz = median.dopz.calc[1]),
                         by = .(subjid, param)]

    for (p in c("WEIGHTKG", "HEIGHTCM", "HEADCM")) {
      dop <- dop_map[p]
      dop_medians <- dop_sp_medians[param == dop, .(subjid, dop_median = median.spz)]
      dop_lookup <- setNames(dop_medians$dop_median, dop_medians$subjid)
      df[param == p, median.dopz := dop_lookup[as.character(subjid)]]
    }

    # Clean up temporary column
    df[, median.dopz.calc := NULL]
  }

  # For rows on an SDE day, compute the absolute distance of tbc.sd
  # from the two median anchors. These distances drive the selection
  # below.
  df[valid.rows & extraneous.this.day, `:=`(
    absdmedian.spz = abs(tbc.sd - median.spz),
    absdmedian.dopz = abs(tbc.sd - median.dopz)
  )]

  # Handle cases where SP median is NA (all days are SDE days for this param)
  # Fall back to using DOP median as the primary comparison
  df[valid.rows & extraneous.this.day & is.na(absdmedian.spz) & !is.na(median.dopz),
     absdmedian.spz := abs(tbc.sd - median.dopz)]

  # Final fallback: if both medians unavailable, use abs(tbc.sd) (treat 0 as median)
  df[valid.rows & extraneous.this.day & is.na(absdmedian.spz),
     absdmedian.spz := abs(tbc.sd)]

  # Set NA DOP distances to Inf so they sort last
  df[valid.rows & extraneous.this.day & is.na(absdmedian.dopz),
     absdmedian.dopz := Inf]

  # Within each SDE group, pick the winner by sorting on absdmedian.spz
  # (asc), then absdmedian.dopz (asc), then age-dependent internal_id.
  # The first row after sorting is the keeper; all others are marked
  # extraneous.
  df[valid.rows & extraneous.this.day, extraneous := {
    # Age-dependent internal_id tiebreaker:
    #   At agedays == 0: keep lowest internal_id (earliest, before fluid shifts)
    #   At agedays  > 0: keep highest internal_id (later, more careful measurement)
    # internal_id is used (not user id) because user id may be character.
    tiebreaker <- if (agedays[1] == 0) internal_id else -internal_id
    ord <- order(absdmedian.spz,
                 absdmedian.dopz, tiebreaker)
    keep_id <- id[ord[1]]
    id != keep_id
  }, by = .(subjid, param, agedays)]

  # Map result back to the caller's original row order. df was
  # re-ordered by the by-group work above; orig_row preserves each
  # row's caller-side position. df$extraneous is only ever set TRUE by
  # the guarded `extraneous := ...` assignment above, which restricts
  # to valid rows on SDE days, so we can use it directly without
  # re-masking on valid.rows.
  result <- logical(nrow(df))
  result[df$orig_row] <- df$extraneous
  return(result)
}

# evil twins ----

#' Function to calculate over-the-limit (OTL) measurements for Evil Twins step
#'
#' @param df data table with all parameters
#'
#' @keywords internal
#' @noRd
calc_otl_evil_twins <- function(df){
  # Anchor-based over-the-limit (OTL) detection for Evil Twins (Child Step 9).
  #
  # A value is OTL only if it is an inner member (B or C) of an evaluable
  # over-the-limit pair: an adjacent eligible pair (B, C) whose jump exceeds 5
  # on BOTH tbc.sd AND ctbc.sd, flanked by outer values A (immediately before B)
  # and D (immediately after C) that each anchor their adjacent inner member to
  # within 3 on EITHER tbc.sd OR ctbc.sd. A missing A or D disqualifies the
  # pair, so single spikes (record ends, and singles embedded between normals)
  # are ceded to the EWMA steps rather than excluded here.
  #
  # Thresholds: jump is strict > 5 on BOTH scales; anchor is <= 3 on EITHER
  # scale (lenient). Both inner members of an evaluable pair are marked OTL;
  # the outer anchors A and D are not.
  #
  # Operates per subject-param: shift() with by = .(subjid, param) yields NA at
  # SP boundaries, so cross-SP comparisons never fire. Assumes df is sorted by
  # subjid, param, agedays, internal_id. Fewer than 4 rows cannot form an
  # A,B,C,D group, so otl is FALSE for all.
  if (nrow(df) < 4L) {
    df[, "otl" := FALSE]
    return(df)
  }

  # Neighbor z-scores within each SP. l = previous, r = next; 1/2 = one/two away.
  # t = tbc.sd, c = ctbc.sd.
  df[, c(".tl1", ".tl2", ".tr1", ".tr2",
         ".cl1", ".cl2", ".cr1", ".cr2") := .(
           shift(tbc.sd, 1L, type = "lag"),  shift(tbc.sd, 2L, type = "lag"),
           shift(tbc.sd, 1L, type = "lead"), shift(tbc.sd, 2L, type = "lead"),
           shift(ctbc.sd, 1L, type = "lag"),  shift(ctbc.sd, 2L, type = "lag"),
           shift(ctbc.sd, 1L, type = "lead"), shift(ctbc.sd, 2L, type = "lead")
         ), by = .(subjid, param)]

  tbc <- df$tbc.sd
  ctbc <- df$ctbc.sd

  # This row as B (inner-left): inner pair (B, C = r1), anchored by A = l1, D = r2
  otl_B <- abs(tbc - df$.tr1) > 5 & abs(ctbc - df$.cr1) > 5 &
    !is.na(df$.tl1) & !is.na(df$.tr2) &
    (abs(df$.tl1 - tbc)     <= 3 | abs(df$.cl1 - ctbc)    <= 3) &
    (abs(df$.tr1 - df$.tr2) <= 3 | abs(df$.cr1 - df$.cr2) <= 3)

  # This row as C (inner-right): inner pair (B = l1, C), anchored by A = l2, D = r1
  otl_C <- abs(tbc - df$.tl1) > 5 & abs(ctbc - df$.cl1) > 5 &
    !is.na(df$.tl2) & !is.na(df$.tr1) &
    (abs(df$.tl2 - df$.tl1) <= 3 | abs(df$.cl2 - df$.cl1) <= 3) &
    (abs(tbc - df$.tr1)     <= 3 | abs(ctbc - df$.cr1)    <= 3)

  otl <- otl_B | otl_C
  otl[is.na(otl)] <- FALSE

  df[, c(".tl1", ".tl2", ".tr1", ".tr2",
         ".cl1", ".cl2", ".cr1", ".cr2") := NULL]
  df[, "otl" := otl]

  return(df)
}

#' Uncorrected CSD z-score at chronological age (WHO/CDC age blend).
#'
#' The same blend as the main Child z-score calculation in cleangrowth():
#' WHO under 2 years (and for HEADCM at all ages), CDC over 5 years, and a
#' linear WHO -> CDC blend from 2 to 5 years for WEIGHTKG / HEIGHTCM, falling
#' back to whichever reference is available. Used by .compute_corrected_z()
#' for the 2-4 year corrected/uncorrected blend, and by the Child Step 2b
#' GA-window adjustment to rescore the adjusted rows.
#'
#' @return A list with sd.orig_who, sd.orig_cdc, and the blended sd.orig.
#' @keywords internal
#' @noRd
.gc_uncorrected_z <- function(param, agedays, sex, meas, measurement.to.z,
                              measurement.to.z_who) {
  z_cdc <- measurement.to.z(param, agedays, sex, meas, TRUE)
  z_who <- measurement.to.z_who(param, agedays, sex, meas, TRUE)
  ay_chron <- agedays / 365.25
  z <- rep(NA_real_, length(meas))
  # %in% TRUE: NA-safe masks (data.table's i treats NA as FALSE; base
  # subassignment would error).
  smooth_val <- (ay_chron >= 2 & ay_chron <= 5 & param != "HEADCM") %in% TRUE
  z[smooth_val] <- (z_cdc[smooth_val] * (ay_chron[smooth_val] - 2) +
                      z_who[smooth_val] * (5 - ay_chron[smooth_val])) / 3
  who_val <- (param == "HEADCM" | ay_chron < 2) %in% TRUE
  use_who <- (who_val & !smooth_val) | (smooth_val & is.na(z_cdc))
  z[use_who] <- z_who[use_who]
  cdc_val <- (param != "HEADCM" & ay_chron > 5) %in% TRUE
  use_cdc <- (cdc_val & !smooth_val) | (smooth_val & is.na(z_who))
  z[use_cdc] <- z_cdc[use_cdc]
  list(sd.orig_who = z_who, sd.orig_cdc = z_cdc, sd.orig = z)
}

#' Compute the GA-corrected CSD z-score for an arbitrary measurement vector.
#'
#' Encapsulates the Child Step 2b corrected-z chain so it can be evaluated on
#' the actual measurement and on the plus/minus (+/-) perturbations using
#' the SAME Fenton gestational-age correction and corrected-WHO/CDC pathway.
#' The gestational-age correction parameters (cagedays, Fenton M/S_upper/
#' S_lower) are held fixed at the values derived from the subject's actual
#' first weight; only the measurement being z-scored varies. Returns the
#' corrected, NOT-yet-recentered z-score (subtract rcmedian.sd downstream to get
#' ctbc).
#'
#' `dt` must carry: param, agedays, sex, ageyears_2b, cagedays, M, S_upper,
#' S_lower, potcorr. `meas` is the measurement vector to z-score (same length
#' and row order as `dt`). The two closures are the CDC-only and WHO-blended
#' `read_anthro()` closures.
#'
#' Computes only sd.c and the uncorrected blend: the final fcase overwrites
#' sd.corr entirely from sd.c and the uncorrected sd.orig, so no intermediate
#' sd.corr (Fenton-only fallback, pmagedays WHO override) is needed.
#'
#' @keywords internal
#' @noRd
.compute_corrected_z <- function(dt, meas, measurement.to.z,
                                 measurement.to.z_who) {
  # avoid no visible binding warnings
  param <- agedays <- sex <- ageyears_2b <- cagedays <- M <- S_upper <-
    S_lower <- potcorr <- v_m <- sd.orig_cdc <- sd.orig_who <- sd.orig_m <-
    v_fenton <- unmod_zscore <- cwho_cv <- ccdc_cv <- sd.c_cdc <- sd.c_who <-
    sd.c <- sd.corr_m <- NULL

  h <- dt[, .(param, agedays, sex, ageyears_2b, cagedays,
              M, S_upper, S_lower, potcorr)]
  h[, v_m := meas]

  # --- Uncorrected CSD z at chronological age (mirrors the main z blend) ---
  h[, sd.orig_m := .gc_uncorrected_z(param, agedays, sex, v_m, measurement.to.z,
                                     measurement.to.z_who)$sd.orig]

  # --- Fenton CSD z (gestational-age corrected) ---
  h[, v_fenton := fifelse(param == "WEIGHTKG", v_m * 1000, v_m)]
  h[, unmod_zscore := fifelse(
    v_fenton >= M,
    (v_fenton - M) / (S_upper * M),
    (v_fenton - M) / (S_lower * M)
  )]

  # --- Corrected WHO/CDC z at corrected age (cagedays) ---
  h[, cwho_cv := v_m]
  h[, ccdc_cv := v_m]
  # Standing -> recumbent offset for the GA window (chronological age at or past
  # day 730, corrected age before day 730; .gc_ht_start_day, the day the
  # references switch to height): the value is compared with the length-based
  # reference at corrected age. One offset for WHO and CDC (.gc_len_offset,
  # shared with length.adjust). Under the default (length.adjust = FALSE),
  # cleangrowth() passes window rows presumed recumbent in already lowered by
  # the same offset (lin_off), so they come back to their entered value here.
  h[param == "HEIGHTCM" & agedays >= .gc_ht_start_day &
      cagedays < .gc_ht_start_day,
    `:=`(cwho_cv = cwho_cv + .gc_len_offset, ccdc_cv = ccdc_cv + .gc_len_offset)]
  h[, sd.c_cdc := measurement.to.z(param, cagedays, sex, ccdc_cv, TRUE)]
  h[, sd.c_who := measurement.to.z_who(param, cagedays, sex, cwho_cv, TRUE)]

  # HC uses WHO at all ages; WT/HT blend WHO->CDC between ages 2-5
  h[, sd.c := fcase(
    param == "HEADCM" & !is.na(sd.c_who),
      sd.c_who,
    param != "HEADCM" & ageyears_2b <= 2 & !is.na(sd.c_who),
      sd.c_who,
    param != "HEADCM" & ageyears_2b > 2 & ageyears_2b < 5 &
      !is.na(sd.c_who) & !is.na(sd.c_cdc),
      (sd.c_who * (5 - ageyears_2b) + sd.c_cdc * (ageyears_2b - 2)) / 3,
    param != "HEADCM" & ageyears_2b >= 5 & !is.na(sd.c_cdc),
      sd.c_cdc
  )]
  # Prefer Fenton over corrected WHO for ages <= 2 (only when Fenton available)
  h[potcorr == TRUE & ageyears_2b <= 2 & !is.na(unmod_zscore),
    sd.c := unmod_zscore]

  # Final corrected z: <=2 uses sd.c; 2-4 blends sd.c with uncorrected; else
  # uncorrected. Matches the inline fcase in cleangrowth() Step 2b.
  h[, sd.corr_m := fcase(
    ageyears_2b <= 2 & potcorr & !is.na(sd.c),
      sd.c,
    ageyears_2b > 2 & ageyears_2b < 4 & !is.na(sd.c) & !is.na(sd.orig_m),
      (sd.orig_m * (4 - ageyears_2b) + sd.c * (ageyears_2b - 2)) / 2,
    default = sd.orig_m
  )]

  return(h$sd.corr_m)
}

# moderate ewma ----

#' Recalculate recentered z-scores for a modified measurement column.
#'
#' Used by Child Step 15/16 (EWMA2 / moderate trajectory outliers) to
#' compute `tbc.p_plus` and `tbc.p_minus` from pre-filled `p_plus` /
#' `p_minus` columns - the +/-5%-weight / +/-1 cm perturbations of each
#' included value. The same WHO/CDC blending window and NA-fallback
#' logic as the main `cleangrowth()` z-score calculation are applied
#' here so that the perturbed z-scores are on the same footing as
#' `tbc.sd`.
#'
#' The `rcmedian.sd` column (populated by the main recentering in
#' `cleangrowth()`) must already be present on `df`. The helper looks
#' it up per row rather than recomputing or re-merging the recentering
#' file.
#'
#' Called twice per batch from the Step 15/16 pre-loop inside
#' `cleanchild()`, once with `cn = "p_plus"` and once with
#' `cn = "p_minus"`. Both callers pass pre-built `measurement.to.z` /
#' `measurement.to.z_who` closures so the expensive reference-table
#' reads happen once per batch rather than once per call.
#'
#' @param df data.table with columns `param`, `agedays`, `sex`,
#'   `rcmedian.sd`, and the measurement column named by `cn`. Modified
#'   in place: intermediate columns `cn.orig_cdc`, `cn.orig_who`,
#'   `cn.orig`, and a new `tbc.<cn>` column are added.
#' @param cn character scalar naming the measurement column to
#'   z-score and recenter (e.g. `"p_plus"` or `"p_minus"`). The output
#'   column is named `paste0("tbc.", cn)`.
#' @param ref.data.path path to reference-table directory; `""` uses
#'   the installed package's `inst/extdata/`. Only consulted when
#'   `measurement.to.z` / `measurement.to.z_who` are NULL.
#' @param measurement.to.z optional pre-built CDC-only closure from
#'   `read_anthro(..., cdc.only = TRUE)`. If NULL, is constructed here
#'   (one disk read).
#' @param measurement.to.z_who optional pre-built WHO-blended closure
#'   from `read_anthro(..., cdc.only = FALSE)`. If NULL, is
#'   constructed here (one disk read).
#'
#' @return The input `df` with an additional `tbc.<cn>` column
#'   containing the recentered, age-blended CSD z-score of the
#'   measurement column. The intermediate columns `cn.orig_cdc`,
#'   `cn.orig_who`, and `cn.orig` are also left on `df` but are not
#'   consumed downstream.
#'
#' @keywords internal
#' @noRd
calc_and_recenter_z_scores <- function(df, cn, ref.data.path,
                                       measurement.to.z = NULL,
                                       measurement.to.z_who = NULL){
  # avoid no visible warning errors
  cn.orig_cdc <- param <- agedays <- sex <- cn.orig_who <- cn.orig <- subjid <-
    tbc.cn <- rcmedian.sd <- NULL

  # Use pre-built closures if provided (the cleangrowth() preprocessing
  # recentering callers always do). The NULL fallback builds them on the fly -
  # one disk read per closure - and exists for direct / debugging use only.
  if (is.null(measurement.to.z))
    measurement.to.z <- read_anthro(ref.data.path, cdc.only = TRUE)
  if (is.null(measurement.to.z_who))
    measurement.to.z_who <- read_anthro(ref.data.path, cdc.only = FALSE)

  # CSD z-scores against each reference, one closure call per row of df.
  df[, cn.orig_cdc := measurement.to.z(param, agedays, sex, get(cn), TRUE)]
  df[, cn.orig_who := measurement.to.z_who(param, agedays, sex, get(cn), TRUE)]

  # Age-blending weights. WHO gets more weight at younger ages, CDC at
  # older ages; window boundaries and the /3 divisor match the main
  # z-score blend in cleangrowth() exactly.
  who_weight <- 5 - (df$agedays/365.25)
  cdc_weight <- (df$agedays/365.25) - 2

  smooth_val <- df$agedays/365.25 >= 2 & df$agedays/365.25 <= 5 & df$param != "HEADCM"

  # Inside the 2-5 year smoothing window (HT/WT only), blend the two
  # z-scores. Only cdc_weight and who_weight need to be re-subset with
  # [smooth_val]; the column references cn.orig_cdc / cn.orig_who on
  # the RHS are already restricted to smooth_val rows by the outer
  # df[smooth_val, ...] subset.
  df[smooth_val,
     cn.orig := (cn.orig_cdc * cdc_weight[smooth_val] +
                   cn.orig_who * who_weight[smooth_val])/3]

  # Under 2y (and HEADCM at any age): pure WHO. who_val and smooth_val
  # are mutually exclusive (HEADCM is excluded from smooth; <2y is
  # excluded from smooth), so this cannot overwrite the blended value.
  who_val <- df$param == "HEADCM" | df$agedays/365.25 < 2
  df[who_val, cn.orig := df$cn.orig_who[who_val]]

  # Over 5y HT/WT: pure CDC. cdc_val and smooth_val are similarly
  # mutually exclusive.
  cdc_val <- df$param != "HEADCM" & df$agedays/365.25 > 5
  df[cdc_val, cn.orig := df$cn.orig_cdc[cdc_val]]

  # NA fallbacks inside the smooth zone: if one reference is unavailable
  # for a row, use the other. Matches the main z-score path.
  df[smooth_val & is.na(cn.orig_cdc), cn.orig := cn.orig_who]
  df[smooth_val & is.na(cn.orig_who), cn.orig := cn.orig_cdc]

  # Recenter against the rcmedian.sd column populated by the main
  # recentering step; df inherits rcmedian.sd from its source data.df.
  df[, tbc.cn := cn.orig - rcmedian.sd]

  # rename ending column
  setnames(df, "tbc.cn", paste0("tbc.", cn))

  return(df)
}

# Preterm catch-up protection. Given a single (subjid, param) group's
# vectors sorted by agedays, returns a logical vector (aligned to the input
# rows) marking values that should be PROTECTED from trajectory exclusion
# because they are members of an early catch-up increase.
#
# A consecutive Include pair (a -> b, a the earlier row) qualifies when:
#   1. the earlier member is younger than 3 months (the early window: a value
#      is in scope if it OR its prior value is < 3mo, which reduces to "earlier
#      member of the pair < 3mo" since rows are age-sorted),
#   2. the z-score rises from a to b (an increase; decreases keep the normal
#      stricter thresholds, as an early drop is more likely a real error), and
#   3. the rise does not exceed allowed z on BOTH the uncorrected (tbc) and
#      corrected (ctbc) z-scores, where allowed = baseline 2 z + 0.5 z per
#      full-or-partial month after the first completed month, capped at 4 z
#      (<=1 mo -> 2; (1,2] -> 2.5; (2,3] -> 3; (3,4] -> 3.5; (4,5] -> 4; >5 -> 4).
#      Mirrors the Step 19 pair "both must exceed" pattern; for an uncorrected
#      value ctbc == tbc, so the corrected term is inert by construction.
# A qualifying value is additionally protected only if its own uncorrected z is
# low (tbc.sd <= -1.5); above that gate the corrected frame is ignored, so a
# non-low value whose corrected z was raised by GA correction is not rescued.
# BOTH members of a qualifying pair are protected - protecting only the later
# (catch-up) value is pointless if the early baseline value is then dropped.
#
# Callers gate on the subject actually being corrected (ctbc != tbc somewhere),
# so non-corrected subjects never reach this. Correction itself requires
# corroboration of true prematurity (consistent early weight, or a low same-day
# height or head circumference); a misplaced-decimal birth weight is left uncorrected upstream,
# so it never reaches this protection and stays catchable by the trajectory
# steps. There is therefore no separate corroboration gate here.
.child_catchup_protect <- function(agedays, tbc.sd, ctbc.sd) {
  # Protect both members of an early (< 3 mo) rising pair from EWMA exclusion so
  # legitimate preterm catch-up is not flagged. A pair qualifies when the
  # earlier member is < 3 months old, the uncorrected z is rising, and the rise
  # stays within `allowed` on at least one frame (the corrected frame excuses a
  # large uncorrected jump). A value is only eligible for protection if its own
  # uncorrected z is itself low (tbc.sd <= low_tbc_gate); for a value above that
  # gate the corrected z is ignored, so a non-low value whose corrected z was
  # pushed upward by GA correction is not rescued.
  n <- length(agedays)
  protect <- rep(FALSE, n)
  if (n < 2L) return(protect)
  low_tbc_gate <- -1.5
  mo_per_day <- 1 / 30.4375
  for (k in seq_len(n - 1L)) {
    if (isTRUE(agedays[k] * mo_per_day < 3)) {
      interval_mo <- (agedays[k + 1L] - agedays[k]) * mo_per_day
      # Allowed rise: baseline 2 z, plus 0.5 z for each full-or-partial month
      # after the first completed month, capped at 4 z. (<=1 mo -> 2; (1,2] -> 2.5;
      # (2,3] -> 3; (3,4] -> 3.5; (4,5] -> 4; >5 -> 4.) The cap is checked against
      # the GA-corrected z (ctbc), where real catch-up stays modest.
      allowed <- min(4, 2 + 0.5 * ceiling(max(0, interval_mo - 1)))
      d_tbc <- tbc.sd[k + 1L] - tbc.sd[k]
      d_ctbc <- ctbc.sd[k + 1L] - ctbc.sd[k]
      if (isTRUE(d_tbc > 0 & !(d_tbc > allowed & d_ctbc > allowed))) {
        protect[k] <- TRUE
        protect[k + 1L] <- TRUE
      }
    }
  }
  # Value-level gate: only protect a value whose own uncorrected z is low.
  protect & !is.na(tbc.sd) & tbc.sd <= low_tbc_gate
}

# Main child growthcleanr function (cleanchild)

#' Function to clean data (optionally in batches):
#'
#' NOTE: to use multiple processes we process patients in batches using foreach.
#' Subjects are split into batches at the wrapper level; all observations for a
#' given subject stay together within one batch. This function processes one
#' batch.
#'
#' @keywords internal
#' @import data.table
#' @importFrom stats median embed
#' @noRd
cleanchild <- function(data.df,
                               log.path,
                               quietly,
                               verbose = FALSE,
                               parallel,
                               measurement.to.z,
                               ewma.fields,
                               cf_rescue = "standard",
                               cf_detail = FALSE,
                               qm_detail = FALSE,
                               piv.z.wt.high,
                               piv.z.ht.low.young,
                               piv.z.ht.low.old,
                               piv.z.ht.high,
                               piv.z.hc.low,
                               piv.z.hc.high,
                               exclude.levels,
                               tanner.ht.vel,
                               error.load.threshold,
                               error.load.mincount,
                               ref.data.path,
                               ewma_window = 15,
                               ref_tables = NULL) {
  # The granular per-step messages below are gated on `!quietly` but are
  # development-level detail. In the redesigned progress output they belong to
  # the `verbose` tier, not the standard milestone view. Remap the local
  # `quietly` so these step messages print only when verbose = TRUE. (`quietly`
  # is used nowhere else in this function, so this is a safe local override; the
  # wrapper's milestone lines are emitted in cleangrowth(), not here.)
  quietly <- !isTRUE(verbose)

  # avoid "no visible binding" warnings
  abs.2ndlast.sd <- abs.tbc.sd <- abs.tbc.sd.next <- abs.tbc.sd.prev <- abssum2 <- NULL
  aft.g.befp1 <- agedays <- agedays.other <- bef.g.aftm1 <- delta <- NULL
  delta.agedays.next <- delta.agedays.prev <- delta.next.ht <- delta.next.sd <- NULL
  delta.prev.ht <- delta.prev.sd <- dewma.after <- dewma.after.prev <- NULL
  dewma.all <- dewma.before <- dewma.before.next <- dnext.sd <- dnext.sd.minus <- NULL
  dnext.sd.plus <- dprev.sd <- dprev.sd.minus <- dprev.sd.plus <- dup <- NULL
  dup.ratio <- ewma.after <- ewma.all <- ewma.before <- exclude <- exclude.count <- NULL
  extraneous <- extraneous.this.day <- first.of.three.or.more <- ht.exp <- NULL
  include.count <- index <- last.of.three.or.more <- line <- max.ht.vel <- NULL
  max.whoinc.1.ht <- max.whoinc.2.ht <- max.whoinc.3.ht <- max.whoinc.4.ht <- NULL
  max.whoinc.6.ht <- maxdiff.next.ht <- maxdiff.prev.ht <- median.other.sd <- NULL
  min.ht.vel <- mindiff.next.ht <- mindiff.prev.ht <- pair <- pair.next <- NULL
  swap.flag.1 <- swap.flag.2 <- tanner.months <- tanner.mid.yrs <- tbc.other.sd <- tbc.sd <- NULL
  tbc.sd.d <- tbc.sd.max <- tbc.sd.min <- tbc.sd.minus <- tbc.sd.next <- tbc.sd.plus <- NULL
  tbc.sd.prev <- tbc.sd.sw <- tbc.sd.t <- temp.diff <- temp.exclude <- v <- NULL
  v.d <- v.minus <- v.next <- v.orig <- v.plus <- v.prev <- v.sw <- v.t <- NULL
  valid.interior.measurement <- who.maxdiff.next.ht <- who.mindiff.next.ht <- NULL
  whoagegrp.ht <- whoinc.1.ht <- whoinc.2.ht <- whoinc.3.ht <- whoinc.4.ht <- NULL
  whoinc.6.ht <- whoinc.age.ht <- NULL

  cf_rescued <- cf_status <- cf_deltaz <- cf_nextz <- cf_threshold <- NULL
  cf_nextz_dec <- rescue_string <- resc_flag <- NULL
  imperial <- cf_dage_orig <- cf_dage_next <- NULL
  otl <- sd_med <- med_diff <- max_diff <- sum_otl <- i.exclude <- NULL
  elig_9 <- sp_count_9 <- uer <- .pos <- NULL


  # avoid no visible warning errors
  sum_sde <- no_sde <- cf <- seq_win <- cs <- absdiff <-
    sd.orig <- ageyears <- ctbc.sd <-
    originator_row <- cf_string_id <- ageday_has_include <-
    ..col_replace <- c.ewma.all <- pot_excl <- c.dewma.all <- p_plus <-
    p_minus <- tbc_diff_next <- tbc_diff_prior <-
    tbc_diff_plus_next <- tbc.p_plus <- tbc_diff_plus_prior <-
    tbc_diff_minus_next <- tbc.p_minus <- tbc_diff_minus_prior <- addcrithigh <-
    ctbc.p_plus <- ctbc.p_minus <- ctbc_diff_next <- ctbc_diff_prior <-
    ctbc_diff_plus_next <- ctbc_diff_plus_prior <-
    ctbc_diff_minus_next <- ctbc_diff_minus_prior <-
    addcritlow <- tbc_dop <- i.tbc.sd <- rowind <- abssum <-
    whoagegrp_ht <- d_agedays <- mindiff <- maxdiff <- who_mindiff_ht <-
    who_maxdiff_ht <- mindiff_prior <- maxdiff_prior <- whoinc.age.hc <-
    who_maxdiff_hc <- who_mindiff_hc <- diff_prev <-
    diff_next <- aft.g.aftm1 <- val_excl <-
    absval <- comp_diff <- err_ratio <-
    NULL

  # qm_detail eligibility flags + Step 19 QM helpers (avoid "no visible binding" NOTE)
  qm_elig_c_absdiff <- qm_elig_c_cf <- qm_elig_c_eviltwins <- qm_elig_c_hardlimit <- NULL
  qm_elig_c_pair <- qm_elig_c_single <- qm_elig_c_traj <- qm_elig_c_trajextreme <- NULL
  qm_elig_c_trajuneval <- qm_elig_extraneous <- NULL
  .qm_pair_fired <- .qm_sp_n_19 <- .qm_valid_19 <- NULL

  # Use internal_id for deterministic SDE order. internal_id is an integer
  # assigned in id-sorted order by cleangrowth(), so results are deterministic
  # regardless of input row order.
  data.df <- data.table(data.df, key = c('subjid', 'param', 'agedays', 'internal_id'))
  # Recreate index for batch processing
  # index was created on the full dataset in cleangrowth() before batching, so
  # batches have non-contiguous indices. This breaks merge/subset operations
  # that use index, so we reassign seq_len(.N) here within the batch.
  data.df[, index := seq_len(.N)]

  if (parallel & !is.na(log.path)) {
    sink(
      sprintf(
        "%s/cleangrowth-%s-batch-%03d.log",
        log.path,
        Sys.Date(),
        data.df$batch[1]
      )
    )
  }

  if (!quietly)
    message(sprintf(
      "[%s] Processing parallel group #%s...",
      Sys.time(),
      data.df$batch[1]
    ))

  # NOTE: in each step, we redo temp SDEs

  # EARLY STEP 13: SDE-Identicals (before Steps 5/6) ----
  # Same-day identical values are excluded before CF detection so that CF logic
  # only sees distinct repeated values, not trivial same-day duplicates.
  # Tiebreaking uses age-dependent id preference:
  # At birth (age 0): Keep LOWEST internal_id (earliest, before fluid/interventions)
  # At age > 0: Keep HIGHEST internal_id (consistent with other SDE handling)

  if (!quietly)
    message(sprintf(
      "[%s] Exclude same-day identical measurements (early SDE-Identical)...",
      Sys.time()
    ))

  # SDE-Identical handles partial identicals - duplicates are marked even when
  # mixed with non-duplicates. Groups by subjid/param/agedays/v to find
  # duplicates of each specific value.
  data.df <- data.df[order(subjid, param, agedays, internal_id)]

  # Count included values with same measurement on same day
  data.df[, n_same_value := sum(exclude == "Include"), by = .(subjid, param, agedays, v)]
  data.df[, has_dup := (n_same_value > 1)]
  # Age-dependent: keep lowest internal_id at birth, highest internal_id otherwise
  # Guard against groups with no included rows (empty vector -> -Inf/Inf warning)
  data.df[, keep_id := {
    incl_ids <- internal_id[exclude == "Include"]
    if (length(incl_ids) == 0L) NA_integer_
    else if (isTRUE(agedays[1L] == 0L)) min(incl_ids)
    else max(incl_ids)
  }, by = .(subjid, param, agedays, v)]
  data.df[has_dup & exclude == "Include" & internal_id != keep_id,
          exclude := .child_exc(param, "Identical")]
  data.df[, c("n_same_value", "has_dup", "keep_id") := NULL]

  # Include internal_id for deterministic SDE order
  setkey(data.df, subjid, param, agedays, internal_id)

  # 5: temporary SDEs ----

  # save a copy of all original measurement values before any transformation
  data.df[, v.orig := v]

  if (!quietly)
    message(sprintf(
      "[%s] Preliminarily identify potential extraneous...",
      Sys.time()
    ))
  data.df[identify_temp_sde(data.df[, .(id, internal_id, subjid, param, agedays, tbc.sd, exclude)]), exclude := 'Exclude-C-Temp-Same-Day']

  # capture a list of subjects with possible extraneous for efficiency later
  subj.dup <- data.df[exclude == 'Exclude-C-Temp-Same-Day', unique(subjid)]

  # Safety check after Step 5 - warn if duplicate Includes found
  step5_dupe_check <- data.df[exclude == "Include", .N, by = .(subjid, param, agedays)][N > 1]
  if (nrow(step5_dupe_check) > 0) {
    warning("Step 5: Found ", nrow(step5_dupe_check),
            " Include duplicates after temp SDE resolution. ",
            "This may indicate a bug in identify_temp_sde().")
  }

  # 6:  carried forwards ----

  # Initialize sde_identical_rows before CF block so it exists when referenced later
  sde_identical_rows <- data.df[0]  # Empty data.table with same structure

  if (!quietly)
    message(sprintf(
    "[%s] Exclude measurements carried forward...",
      Sys.time()
  ))

  # qm_detail: eligibility for the CF step (Child Step 6). PREVALENCE
  # denominator -- all valid (Include or temp-SDE) values at Step 6 entry, per
  # the "whole denominator" definition (a 10%-CF dataset differs from a 2%-CF
  # one even at equal rescue rate). Write-only; does not affect any exclusion.
  if (qm_detail) {
    data.df[, qm_elig_c_cf := .child_valid(data.df, include.temporary.extraneous = TRUE)]
  }

  # Only run CF detection on valid values with >1 measurement per subject-param
  sp_multi <- data.df[, .(is_multi = .N > 1), by = .(subjid, param)]
  not_single <- sp_multi[data.df, on = .(subjid, param), is_multi]
  valid_set <- .child_valid(data.df, include.temporary.extraneous = TRUE) &
    not_single
  data.sub <- data.df[valid_set,]

  # Pre-filter to subject-params with potential CFs
  # CF requires duplicate values - if all values unique, skip CF processing entirely
  # This excludes SDE-Identicals from consideration (already handled in early Step 13)
  data.sub[, sp_key := paste0(subjid, "_", param)]
  sp_has_dups <- data.sub[, .(has_dup_vals = uniqueN(v.orig) < .N), by = sp_key]
  sp_with_potential_cf <- sp_has_dups[has_dup_vals == TRUE, sp_key]
  n_total_sp <- uniqueN(data.sub$sp_key)
  n_with_cf <- length(sp_with_potential_cf)
  if (!quietly)
    message(sprintf("  CF pre-filter: %d/%d subject-params have potential CFs (%.1f%%)",
                n_with_cf, n_total_sp, 100*n_with_cf/n_total_sp))

  # Initialize cf=FALSE for all, only process those with potential CFs
  data.sub[, cf := FALSE]

  # Only process subject-params with duplicate values
  if (length(sp_with_potential_cf) > 0) {
    cf_subset <- data.sub[sp_key %in% sp_with_potential_cf]
    # Add id for consistent SDE order
    cf_subset <- cf_subset[order(subjid, param, agedays, internal_id)]

    # CF detection: compare each value to the prior ageday's single value.
    # Only compare if the prior ageday has exactly ONE value - if there are
    # multiple values (SDEs) on the prior day, skip the CF comparison.

    # Step 1: Count values per (subjid, param, ageday)
    day_counts <- cf_subset[, .(n_on_day = .N), by = .(subjid, param, agedays)]

    # Step 2: Get unique agedays per subject-param, then find prior ageday for each
    unique_days <- unique(cf_subset[, .(subjid, param, agedays)])
    setorder(unique_days, subjid, param, agedays)
    unique_days[, prior_ageday := shift(agedays, type = "lag"), by = .(subjid, param)]

    # Step 3: Get the value from each unique (subjid, param, ageday) where count == 1
    # For days with >1 value, we set prior_val to NA so CF check fails
    single_val_days <- cf_subset[, .(
      single_val = if (.N == 1) v.orig[1] else NA_real_
    ), by = .(subjid, param, agedays)]

    # Step 4: Merge prior_ageday into cf_subset
    cf_subset[unique_days, prior_ageday := i.prior_ageday, on = .(subjid, param, agedays)]

    # Step 5: Look up the single value from prior ageday
    cf_subset[single_val_days, prior_single_val := i.single_val,
             on = .(subjid, param, prior_ageday = agedays)]

    # Step 6: CF = TRUE if prior day had exactly 1 value AND current matches it.
    # Uses exact equality (no numeric tolerance).
    cf_subset[, cf := !is.na(prior_single_val) & v.orig == prior_single_val]

    # Merge cf results back to data.sub by index
    data.sub[cf_subset, cf := i.cf, on = "index"]
  }
  # End of pre-filter if block - subject-params without potential CFs already have cf=FALSE

  # Cleanup temp columns
  data.sub[, sp_key := NULL]

  # Merge CF flags back to data.df
  cf_idx <- data.sub$index[data.sub$cf]
  data.df[index %in% cf_idx, exclude := .child_exc(param, "CF")]

  # Check if CFs exist before rescue processing.
  # This optimization skips rescue logic if no CFs are present.
  any_cf <- any(data.df$exclude == "Exclude-C-CF")
  if (!quietly)
    message(sprintf("  CF rescue pre-filter: CFs exist = %s", any_cf))

  # Only process CF rescue if CFs exist
  if (any_cf) {

  # Redo temporary SDEs after CF identification: temp SDEs should be
  # re-evaluated now that CFs are excluded.
  any_sde <- any(data.df$exclude == "Exclude-C-Temp-Same-Day")

  if (any_sde) {
    # Temporarily convert Temp SDEs back to Include for re-evaluation.
    data.df[exclude == "Exclude-C-Temp-Same-Day", exclude := "Include"]

    # Re-run temp SDE logic (now CFs are excluded, so SDE evaluation will differ)
    data.df[identify_temp_sde(data.df[, .(id, internal_id, subjid, param, agedays, tbc.sd, exclude)]), exclude := 'Exclude-C-Temp-Same-Day']
  }

  # SDE-Identicals break multiple parts of the CF logic (rle, originator
  # detection, cs assignment) so temporarily remove them, do CF calculations,
  # then add them back after CF rescue below.
  sde_identical_rows <- data.df[exclude == "Exclude-C-Identical"]
  data.df <- data.df[!exclude == "Exclude-C-Identical"]

  # CFs on days that also have at least one Include are NOT eligible for
  # rescue and are excluded from string detection. ageday_has_include only
  # checks for Includes (temp SDEs do not block CF rescue eligibility).
  data.df[, ageday_has_include := any(as.character(exclude) == "Include"),
          by = c("subjid", "param", "agedays")]

  # POSITIONAL STRING DETECTION
  # Originators are Includes where the NEXT value is a CF. Strings propagate
  # forward (originator, then consecutive CFs) until interrupted by a non-CF.
  # The ageday_has_include check restricts CF rescue eligibility only; it does
  # not disqualify originators.

  # Initialize variables
  data.df[, cf_binary := exclude == "Exclude-C-CF"]

  # Process by subject-param to maintain ordering
  data.df[, ':=' (
    nextcf = shift(cf_binary, type = "lead", fill = FALSE),
    priorcf = shift(cf_binary, type = "lag", fill = FALSE),
    originator = FALSE,
    cf_string_num = NA_integer_,
    originator_z = NA_real_
  ), by = c("subjid", "param")]

  # Identify originators: Include where next value is a CF
  # Any Include can be an originator - ageday_has_include only restricts CF rescue eligibility
  data.df[exclude == "Include" & nextcf == TRUE, originator := TRUE]

  # Assign sequential string numbers to originators (per subject-param)
  data.df[, originator_seq := cumsum(originator), by = c("subjid", "param")]
  data.df[originator == TRUE, cf_string_num := originator_seq]

  # Store originator z-scores (uncorrected, pre-GA-correction).
  data.df[originator == TRUE, originator_z := sd.orig]

  # Get max number of CFs to determine loop iterations
  max_cf_count <- data.df[cf_binary == TRUE, .N, by = c("subjid", "param")]
  max_iterations <- if (nrow(max_cf_count) > 0) max(max_cf_count$N) else 0

  # Propagate string numbers and originator z-scores forward to consecutive CFs
  # Only propagate to CFs that are eligible (not on days with includes)
  if (max_iterations > 0) {
    for (i in 1:max_iterations) {
      data.df[, ':=' (
        cf_string_num = ifelse(
          cf_binary == TRUE &
          (ageday_has_include == FALSE | is.na(ageday_has_include)) &
          is.na(cf_string_num),
          shift(cf_string_num, type = "lag"),
          cf_string_num
        ),
        originator_z = ifelse(
          cf_binary == TRUE &
          (ageday_has_include == FALSE | is.na(ageday_has_include)) &
          is.na(originator_z),
          shift(originator_z, type = "lag"),
          originator_z
        )
      ), by = c("subjid", "param")]
    }
  }

  # Create seq_win variable (position in string: 0 for originator, 1/2/3... for CFs)
  data.df[, seq_win := NA_integer_]
  data.df[originator == TRUE, seq_win := 0]
  data.df[!is.na(cf_string_num) & cf_binary == TRUE,
          seq_win := seq_len(.N),
          by = c("subjid", "param", "cf_string_num")]

  # Create cs variable for compatibility with rescue code logic
  data.df[, cs := cf_string_num]

  # Calculate absdiff (absolute z-score difference from originator)
  data.df[!is.na(seq_win), absdiff := abs(sd.orig - originator_z)]

  # Clean up temporary variables
  data.df[, c("cf_binary", "nextcf", "priorcf",
              "originator", "originator_seq", "cf_string_num", "originator_z") := NULL]

  # CF rescue: use age/interval/param-specific lookup thresholds
  # CFs with ageday_has_include are already excluded from cs/seq_win assignment
  # (they stay excluded regardless of rescue mode)

  # Compute interval_days for each CF: agedays - prior_ageday
  # We need the originator's ageday for each CF string
  # Reconstruct: originator is seq_win == 0, CFs are seq_win > 0
  # For each CF, the originator's ageday is the ageday of the row with seq_win == 0 in the same cs group
  data.df[, orig_ageday := NA_integer_]
  data.df[seq_win == 0, orig_ageday := agedays]
  # Propagate originator ageday forward within each string (same approach as originator_z)
  if (max_iterations > 0) {
    for (i in 1:max_iterations) {
      data.df[, orig_ageday := ifelse(
        !is.na(cs) & seq_win > 0 & is.na(orig_ageday),
        shift(orig_ageday, type = "lag"),
        orig_ageday
      ), by = c("subjid", "param")]
    }
  }

  if (cf_rescue == "none") {
    # No rescue: all CFs stay excluded
    if (!quietly) message("  CF rescue mode: none (all CFs excluded)")

  } else if (cf_rescue == "all") {
    # Rescue every detected CF, including CFs on a SPA that also has a
    # non-CF Include. This is the "ignore CFs" / legacy-compatible mode:
    # a CF that is consistent with the trajectory should not be
    # preferentially excluded just because another value landed on the
    # same day. Step 13 final SDE resolution handles any resulting
    # multi-Include SPAs on downstream.
    cf_mask <- data.df$exclude == "Exclude-C-CF"
    if (any(cf_mask)) {
      data.df[cf_mask, cf_rescued := "Rescued-All"]
      data.df[cf_mask, exclude := "Include"]
      if (!quietly)
        message(sprintf("  CF rescue mode: all (%d measurements re-included)", sum(cf_mask)))
    }

  } else {
    # Standard rescue: deltaz/nextz rules (see vignette("cf-rescue")).
    #
    # The decision is made on the LAST CF (highest seq_win) of each
    # identical-value string and applied to the whole string: if the last CF
    # is not rescued, the entire string stays excluded. Because all values in
    # a string are identical, z declines monotonically with age, so the last
    # CF has the largest cf_deltaz (and the largest cf_deltaz + cf_nextz sum) --
    # if it clears the rule, every earlier CF in the string would too.
    #
    # Rescue the last CF when:
    #   * it has an eligible next Included value (cf_nextz not NA):
    #       cf_deltaz + cf_nextz <= 1  AND  cf_deltaz <= 1.5
    #     (cf_deltaz + cf_nextz is 2x how far the CF dips below the line joining
    #      its originator and its next value; small/negative => smooth decline
    #      or stagnation => a real value; a deep positive dip => a true CF.)
    #   * it has no next value (end of series): by CF age --
    #       cf_deltaz <= 0.5  (agedays <  13 * 365.25)
    #       cf_deltaz <= 1.0  (agedays >= 13 * 365.25)
    # cf_deltaz = absdiff = |sd.orig - originator_z| (>= 0). cf_nextz is
    # the signed sd.orig change from a CF to the next Included value;
    # pre-rescue every CF candidate is "Exclude-C-CF", so keying eligibility on
    # exclude == "Include" already skips carried values (matches the output
    # cf_nextz computed later).
    cf_rows <- !is.na(data.df$seq_win) & data.df$seq_win > 0 &
               (data.df$ageday_has_include == FALSE | is.na(data.df$ageday_has_include))

    if (any(cf_rows)) {
      # Decision-time cf_nextz for every row (skips non-Include rows, i.e. all
      # CF candidates and any excluded values, via nafill "nocb").
      data.df[, cf_nextz_dec := {
        z <- sd.orig
        z[exclude != "Include"] <- NA_real_
        z_lead <- c(z[-1L], NA_real_)
        next_z <- data.table::nafill(z_lead, type = "nocb")
        next_z - sd.orig
      }, by = .(subjid, param)]

      # Last CF per string (keyed on subjid+param+cs, since cs restarts per
      # subject-parameter).
      sub <- data.df[cf_rows, .(subjid, param, cs, seq_win, agedays,
                                absdiff, cf_nextz_dec)]
      data.table::setorder(sub, subjid, param, cs, seq_win)
      last_cf <- sub[, .SD[.N], by = .(subjid, param, cs)]

      last_cf[, rescue_string := data.table::fifelse(
        !is.na(cf_nextz_dec),
        (absdiff + cf_nextz_dec <= 1) & (absdiff <= 1.5),
        absdiff <= data.table::fifelse(agedays >= 13 * 365.25, 1.0, 0.5)
      )]

      resc_keys <- last_cf[rescue_string == TRUE, .(subjid, param, cs)]
      if (nrow(resc_keys) > 0) {
        data.df[, resc_flag := FALSE]
        data.df[resc_keys, resc_flag := TRUE, on = .(subjid, param, cs)]
        rescue_mask <- cf_rows & data.df$resc_flag
        if (any(rescue_mask)) {
          data.df[rescue_mask, cf_rescued := "Rescued"]
          data.df[rescue_mask, exclude := "Include"]
          if (!quietly)
            message(sprintf("  CF rescue: %d measurements re-included (deltaz/nextz rules)",
                        sum(rescue_mask)))
        }
        data.df[, resc_flag := NULL]
      }
      data.df[, cf_nextz_dec := NULL]
    }
  }

  } # End if (any_cf)

  # Add SDE-Identical rows back after CF rescue
  if (nrow(sde_identical_rows) > 0) {
    data.df <- rbind(data.df, sde_identical_rows, fill = TRUE)
    data.df <- data.df[order(subjid, param, agedays, internal_id)]
  }

  # Populate cf_status and cf_deltaz before dropping temp columns.
  # cf_status labels every row with one of four values:
  #   "Orig"   - originator of a CF string (value others were carried forward from)
  #   "CF-R"   - carried-forward value that was rescued
  #   "CF-NR"  - carried-forward value that stayed excluded
  #   "Not-CF" - everything else (no CF relationship)
  if (cf_detail) {
    data.df[, cf_status := "Not-CF"]
    data.df[, cf_deltaz := NA_real_]

    if ("seq_win" %in% names(data.df)) {
      # deltaZ for positionally-detected CF candidates (seq_win > 0)
      if ("absdiff" %in% names(data.df)) {
        data.df[!is.na(seq_win) & seq_win > 0, cf_deltaz := absdiff]
      }
      # Originators: first value in a CF string (seq_win == 0)
      data.df[!is.na(seq_win) & seq_win == 0, cf_status := "Orig"]
    }
    # CF-R: carried-forward values that were rescued (cf_rescued non-empty)
    data.df[!is.na(cf_rescued) & cf_rescued != "", cf_status := "CF-R"]
    # CF-NR: carried-forward values still excluded as CF. Uses the exclude code
    # rather than seq_win so CFs the positional detector skips (e.g. CFs landing
    # on a day that also has an Include) are still labeled.
    data.df[exclude == "Exclude-C-CF", cf_status := "CF-NR"]
    # CF-E: same-day-extraneous CFs -- CFs on a day that also has an Include.
    # These are flagged Exclude-C-CF by the value-match check but are excluded
    # from positional string detection (so they have no seq_win/absdiff and so
    # no cf_deltaz), and are never rescue-eligible. Split them out from CF-NR
    # so cf_nextz / cf_deltaz being NA is consistent rather than ambiguous.
    if ("ageday_has_include" %in% names(data.df)) {
      data.df[cf_status == "CF-NR" & ageday_has_include == TRUE,
              cf_status := "CF-E"]
    }

    # cf_nextz: signed change in sd.orig from a CF row (CF-R or CF-NR)
    # to the next non-CF Included value (per subjid + param, sorted by
    # agedays/internal_id) at the time of Step 6. "Non-CF" = cf_status not in
    # c("CF-R","CF-NR"); "Included" = exclude == "Include" (so temp same-day
    # extraneous rows, CF-NR, and CF-E rows are skipped). NA on non-CF and
    # CF-E rows and when no eligible next value exists (end of subject/param
    # series).
    data.df[, cf_nextz := {
      is_elig <- exclude == "Include" & !(cf_status %in% c("CF-R", "CF-NR"))
      z <- sd.orig
      z[!is_elig] <- NA_real_
      # shift up by one so a row never references its own z, then carry the
      # next non-NA backward.
      z_lead <- c(z[-1L], NA_real_)
      next_z <- data.table::nafill(z_lead, type = "nocb")
      data.table::fifelse(cf_status %in% c("CF-R", "CF-NR"),
                          next_z - sd.orig,
                          NA_real_)
    }, by = .(subjid, param)]

    # --- Extra design-aid columns (added to support new CF logic) ---

    # imperial: row-level whole/half imperial-unit flag -- TRUE when the value is
    # a whole number of pounds (WEIGHTKG) or a whole/half inch (HEIGHTCM/HEADCM),
    # within 0.01 of the imperial unit. A design aid for CF-logic development;
    # computed from v.orig so it is populated for every row, including in
    # batches with no CFs.
    # Distance-to-nearest-unit test (symmetric): round(x/u)*u gives the nearest
    # unit in both directions, so a value just below a whole/half unit is caught
    # as well as one just above it.
    data.df[, imperial := FALSE]
    data.df[param == "WEIGHTKG", imperial := {
      x <- v.orig * 2.20462262
      abs(x - round(x)) < 0.01
    }]
    data.df[param == "HEIGHTCM", imperial := {
      x <- v.orig / 2.54
      abs(x - round(x / 0.5) * 0.5) < 0.01
    }]
    data.df[param == "HEADCM", imperial := {
      x <- v.orig / 2.54
      abs(x - round(x / 0.5) * 0.5) < 0.01
    }]

    # cf_dage_orig: days from a CF row's originator to the CF row
    # (agedays_CF - agedays_orig; always positive). Populated for positionally-
    # detected CF rows (CF-R / CF-NR); NA elsewhere (incl. CF-E, which is skipped
    # by string detection, and all non-CF rows).
    data.df[, cf_dage_orig := NA_integer_]
    if (all(c("seq_win", "orig_ageday") %in% names(data.df))) {
      data.df[!is.na(seq_win) & seq_win > 0,
              cf_dage_orig := as.integer(agedays - orig_ageday)]
    }

    # cf_dage_next: days from a CF row (CF-R / CF-NR) to the next eligible
    # non-CF Included value (agedays_next - agedays_CF; always positive) -- the
    # agedays analogue of cf_nextz, using the same eligibility rule. NA on
    # non-CF/CF-E rows and at the end of a subject/param series.
    data.df[, cf_dage_next := {
      is_elig <- exclude == "Include" & !(cf_status %in% c("CF-R", "CF-NR"))
      a <- as.numeric(agedays)
      a[!is_elig] <- NA_real_
      a_lead <- c(a[-1L], NA_real_)
      next_a <- data.table::nafill(a_lead, type = "nocb")
      data.table::fifelse(cf_status %in% c("CF-R", "CF-NR"),
                          next_a - as.numeric(agedays),
                          NA_real_)
    }, by = .(subjid, param)]
  }

  # Drop columns not needed after Step 6
  # v.orig: only used for SDE tiebreaking and CF detection (Steps 5-6)
  cols_to_drop_6 <- intersect(
    c("v.orig", "seq_win", "cs", "absdiff", "ageday_has_include",
      "orig_ageday"),
    names(data.df)
  )
  if (length(cols_to_drop_6) > 0L) data.df[, (cols_to_drop_6) := NULL]

  # Step 7: PIV ----


  if (!quietly)
    message(sprintf(
      "[%s] Exclude PIVs...",
      Sys.time()
    ))

  # add age in years
  data.df[, ageyears := agedays/365.25]

  valid_set <- .child_valid(data.df, include.temporary.extraneous = TRUE)

  # qm_detail: eligibility for the hard-limit / PIV step (Child Step 7).
  # PREVALENCE denominator -- every valid value can hit a hard limit.
  if (qm_detail) {
    data.df[, qm_elig_c_hardlimit := valid_set]
  }

  # identify absolute cutoffs - all use Exclude-C-PIV (not param-specific; param is in the data)
  # Min weight: <0.2 kg for first year, <1 kg after
  data.df[valid_set & param == "WEIGHTKG" & v < 0.2 & agedays <= 365,
          exclude := "Exclude-C-PIV"]
  data.df[valid_set & param == "WEIGHTKG" & v < 1 & agedays > 365,
          exclude := "Exclude-C-PIV"]
  # Max weight at birth
  data.df[valid_set & param == "WEIGHTKG" & v > 10.5 &
            agedays == 0,
          exclude := "Exclude-C-PIV"]
  # Max weight for <2y
  data.df[valid_set & param == "WEIGHTKG" & v > 40 &
            ageyears < 2,
          exclude := "Exclude-C-PIV"]
  # Max weight for all based on published data
  data.df[valid_set & param == "WEIGHTKG" & v > 600,
          exclude := "Exclude-C-PIV"]

  # Min/max HT (any age), and max HT at birth.
  data.df[valid_set & param == "HEIGHTCM" & v < 18,
          exclude := "Exclude-C-PIV"]
  data.df[valid_set & param == "HEIGHTCM" & v > 244,
          exclude := "Exclude-C-PIV"]
  data.df[valid_set & param == "HEIGHTCM" & v > 65 &
            agedays == 0,
          exclude := "Exclude-C-PIV"]

  # Min/max HC.
  data.df[valid_set & param == "HEADCM" & v < 13,
          exclude := "Exclude-C-PIV"]
  data.df[valid_set & param == "HEADCM" & v > 75,
          exclude := "Exclude-C-PIV"]
  data.df[valid_set & param == "HEADCM" & v > 50 &
            agedays == 0,
          exclude := "Exclude-C-PIV"]

  # Standardized PIV - same exclusion code as absolute PIV (Exclude-C-PIV).
  # The !grepl(piv_pattern, exclude) guard skips rows just assigned
  # Exclude-C-PIV by the absolute block above, since valid_set was
  # computed before absolute PIV ran and so still treats those rows as valid.
  piv_pattern <- "^Exclude-C-PIV$"

  # z cutoffs use unrecentered CSD z-scores (sd.orig).
  # Cutoffs are set from cleangrowth() parameters (piv.z.* family).
  # Weight. No standardized LOW cutoff for weight: with a positive weight the
  # CSD z below the median asymptotes to -M/csd_neg as the weight -> 0, which
  # peaks near -10 in infancy and is shallower at other ages, so a low-weight z
  # can never reach a useful cutoff (the absolute floors v<0.2 kg / v<1 kg do
  # that job), so there is no standardized low weight cutoff. HT/HC keep their
  # low cutoffs (those z's are reachable just above their absolute floors).
  data.df[valid_set & param == "WEIGHTKG" & sd.orig > piv.z.wt.high &
            !grepl(piv_pattern, exclude),
          exclude := "Exclude-C-PIV"]

  # Height. Default upper HT cutoff (piv.z.ht.high = 8) is tighter than the
  # weight upper cutoff based on analysis of CHOP data showing the
  # +/-15 / +/-25 range was too loose for heights.
  data.df[valid_set & param == "HEIGHTCM" & sd.orig < piv.z.ht.low.young &
            ageyears < 1 & !grepl(piv_pattern, exclude),
          exclude := "Exclude-C-PIV"]
  data.df[valid_set & param == "HEIGHTCM" & sd.orig < piv.z.ht.low.old &
            ageyears >= 1 & !grepl(piv_pattern, exclude),
          exclude := "Exclude-C-PIV"]
  data.df[valid_set & param == "HEIGHTCM" & sd.orig > piv.z.ht.high &
            !grepl(piv_pattern, exclude),
          exclude := "Exclude-C-PIV"]

  # head circumference
  data.df[valid_set & param == "HEADCM" & sd.orig < piv.z.hc.low &
            !grepl(piv_pattern, exclude),
          exclude := "Exclude-C-PIV"]
  data.df[valid_set & param == "HEADCM" & sd.orig > piv.z.hc.high &
            !grepl(piv_pattern, exclude),
          exclude := "Exclude-C-PIV"]

  # BMI hard limits. Runs after every individual WT/HT limit above, so a value
  # already excluded as PIV never feeds a BMI. Pairs are same-day WT x HT
  # combinations among the remaining valid rows (temp SDEs included, so every
  # same-day combination is checked). A pairing is out of range if
  # BMI < 3 (any age), BMI > 50 (ageyears < 2), or BMI > 150 (ageyears >= 2);
  # all strict, like the absolute limits above. A value is excluded only if
  # none of its same-day pairings is in range. For an out-of-range pairing:
  # if only one of its two values has no in-range pairing, that value is
  # excluded; if neither does, the value with the larger |sd.orig| is
  # excluded (NA z counts as 0; an exact tie excludes the weight).
  bmi_rows <- data.df[valid_set & param %in% c("WEIGHTKG", "HEIGHTCM") &
                        !grepl(piv_pattern, exclude),
                      .(internal_id, subjid, agedays, param, v,
                        absz = fifelse(is.na(sd.orig), 0, abs(sd.orig)))]
  bmi_pairs <- merge(bmi_rows[param == "WEIGHTKG"], bmi_rows[param == "HEIGHTCM"],
                     by = c("subjid", "agedays"), suffixes = c(".w", ".h"),
                     allow.cartesian = TRUE)
  if (nrow(bmi_pairs) > 0L) {
    bmi_pairs[, bmi := v.w / (v.h / 100)^2]
    bmi_pairs[, bmi_bad := bmi < 3 |
                (agedays / 365.25 < 2 & bmi > 50) |
                (agedays / 365.25 >= 2 & bmi > 150)]
    bmi_pairs[, w_has_ok := any(!bmi_bad), by = internal_id.w]
    bmi_pairs[, h_has_ok := any(!bmi_bad), by = internal_id.h]
    bmi_out <- bmi_pairs[bmi_bad == TRUE & !(w_has_ok & h_has_ok)]
    bmi_excl <- unique(bmi_out[, fifelse(
      h_has_ok | (!w_has_ok & absz.w >= absz.h), internal_id.w, internal_id.h)])
    data.df[internal_id %in% bmi_excl, exclude := "Exclude-C-PIV"]
  }

  # 7d. Re-evaluate temp SDEs after PIV exclusions: reset all Exclude-C-Temp-Same-Day
  # rows to Include, then rerun identify_temp_sde(). Rationale: absolute or
  # standardized PIV may have excluded the prior temp-SDE "keeper" on an SPA;
  # another value in that SPA should now be flagged instead.
  data.df[exclude == 'Exclude-C-Temp-Same-Day', exclude := 'Include']
  data.df[identify_temp_sde(data.df[, .(id, internal_id, subjid, param, agedays, tbc.sd, exclude)]), exclude := 'Exclude-C-Temp-Same-Day']

  # Drop column not needed after Step 7
  # ageyears: only used for PIV age-threshold checks (Step 7)
  data.df[, ageyears := NULL]

  # Step 9: Evil Twins ----
  # Evil Twins identifies pairs (or longer runs) of consecutive measurements
  # that are both extreme relative to the subject's trajectory. Such pairs
  # distort each other's EWMA enough that neither looks out of line on a
  # neighbor-weighted basis, so EWMA-based exclusions alone can miss them.
  # This step runs before EWMA processing to catch them.
  #
  # Per-(subjid, param) group processing: groups are small and independent
  # (typically 3-30 rows), so processing each group in a local copy avoids
  # repeatedly copying the full valid dataset on every while-loop iteration.
  # Same pattern is used in Steps 11, 15, and 17.

  # MUST reorder BEFORE computing valid_set.
  # valid_set is a boolean vector aligned to row positions; reordering
  # after would misalign it, causing temp SDEs to be included and
  # non-temp-SDEs to be excluded.
  # Sort key must include agedays (so calc_otl_evil_twins compares
  # temporally adjacent rows) and internal_id (so same-ageday rows have a
  # deterministic order across sequential and parallel runs).
  data.df <- data.df[order(subjid, param, agedays, internal_id),]

  # Evil Twins requires >= 4 ELIGIBLE values per subject-param. Under the anchor
  # rule both inner members of an over-limit pair need an outer flanking value,
  # so an A,B,C,D group is the floor: an SP with fewer than 4 eligible values
  # cannot produce an evaluable pair. This is a correctness GATE (deleting it
  # would change outcomes by indexing past the ends of a short SP), not an
  # optimization, and is computed over ELIGIBLE (Include, non-temp-SDE) rows
  # only -- NOT all rows.
  data.df[, elig_9 := .child_valid(data.df, include.temporary.extraneous = FALSE)]
  data.df[, sp_count_9 := sum(elig_9), by = .(subjid, param)]
  valid_set <- data.df$elig_9 & (data.df$sp_count_9 >= 4L)

  # qm_detail: eligibility for the Evil Twins step (Child Step 9). AT-RISK
  # denominator -- the anchor rule needs >= 4 eligible values per subject-param.
  if (qm_detail) {
    data.df[, qm_elig_c_eviltwins := valid_set]
  }

  # 9a. Global early-exit check: find out if any possible evil twins exist
  # at all (cheap vectorized anchor check) before entering per-group loop.
  start_df <- calc_otl_evil_twins(data.df[valid_set,])

  if (any(start_df$otl, na.rm = TRUE)) {
    if (!quietly)
      message(sprintf(
      "[%s] Exclude evil twins...",
        Sys.time()
    ))

    # Per-value unit-error-range flag, used by the Step 9 selection's UER3 check
    # below. All rows here are child (cleanchild), so use the child z=+-3
    # reference directly. Computed once for the batch, only when ET work exists.
    cz9 <- .child_z3(data.df$param, data.df$sex, data.df$agedays)
    data.df[, uer := .unit_error_range_flag(param, v, cz9$zn3, cz9$zp3)]

    # Identify which subject-params have OTL values - only process those groups
    sp_with_otl <- unique(start_df[otl == TRUE, .(subjid, param)])

    if (!quietly)
      message(sprintf("  Evil twins pre-filter: %d subject-params have OTL values",
                  nrow(sp_with_otl)))

    # 9b. Process each subject-param group independently via for loop
    # (Avoids data.table by+:= closure mechanics; groups are small, typically 3-30 rows)
    et_excl_lines <- integer(0)
    for (sp_i in seq_len(nrow(sp_with_otl))) {
      s <- sp_with_otl$subjid[sp_i]
      p <- sp_with_otl$param[sp_i]

      # Extract this group's eligible rows
      grp_idx <- which(data.df$subjid == s & data.df$param == p & valid_set)
      if (length(grp_idx) < 4L) next

      df <- data.df[grp_idx, .(line, id, internal_id, subjid, param, agedays, v, tbc.sd, ctbc.sd, uer, exclude)]
      df <- copy(df)

      # UER3 (computed once per SP, on the initial Include set): do the SP's
      # values split cleanly into a unit-error group (uer) and a plausible group
      # (|tbc.sd| < 3) whose RAW-measurement means differ by ~ the unit-conversion
      # factor (WT 1.7-2.7 ~ 2.2046; HT/HC 2.04-3.04 ~ 2.54), with the unit-error
      # group all one z sign? If so, the unit-error block is the implausible one,
      # and the median can be pulled toward it, so the OTL selection drops the
      # most extreme value rather than trusting med_diff.
      sp0 <- df[exclude == "Include"]
      ug <- sp0[uer == TRUE]
      zg <- sp0[abs(tbc.sd) < 3]
      uer3 <- FALSE
      if (nrow(ug) >= 1L && nrow(zg) >= 1L &&
          (nrow(ug) + nrow(zg)) == nrow(sp0) &&
          length(unique(sign(ug$tbc.sd))) == 1L) {
        mu <- mean(ug$v); mz <- mean(zg$v)
        rt <- max(mu, mz) / min(mu, mz)
        bnd <- if (p == "WEIGHTKG") c(1.7, 2.7) else c(2.04, 3.04)
        uer3 <- isTRUE(rt >= bnd[1L] && rt <= bnd[2L])
      }

      # calc_otl for this group
      df <- calc_otl_evil_twins(df)

      while (any(df$otl, na.rm = TRUE)) {
        # Eligible-count gate, re-checked at the top of each iteration: an SP
        # that drops below 4 eligible (Include) values mid-cascade exits the
        # loop and cedes its remaining values to the EWMA steps.
        incl <- df[exclude == "Include"]
        if (nrow(incl) < 4L) break

        incl[, .pos := .I]   # age-sorted position within the current Include set
        otl_cand <- incl[otl == TRUE]
        if (nrow(otl_cand) == 0L) break
        otl_pos <- incl[otl == TRUE, .pos]

        if (nrow(otl_cand) == 1L) {
          # One OTL candidate: exclude it.
          worst_line <- otl_cand$line[1L]
        } else {
          # med_diff = distance from a WINDOWED median: the median of Include
          # tbc.sd within +-4 positions of the candidate. Windowing keeps the
          # center local, so a long declining/varied trajectory elsewhere in the
          # SP cannot collapse the gap between a lone dip and an on-trajectory
          # plateau (a whole-SP median sits between them; the local median sits
          # at the plateau, making the dip the clear outlier).
          md <- vapply(seq_along(otl_pos), function(j) {
            lo <- max(1L, otl_pos[j] - 4L); hi <- min(nrow(incl), otl_pos[j] + 4L)
            abs(otl_cand$tbc.sd[j] - median(incl$tbc.sd[lo:hi], na.rm = TRUE))
          }, numeric(1))
          otl_cand[, med_diff := md]

          if (uer3) {
            # (1) Clean unit-error 2-group SP -> the unit-error block is the
            # implausible (more extreme) one: exclude highest |tbc.sd|.
            ord <- order(-abs(otl_cand$tbc.sd), otl_cand$internal_id)
            worst_line <- otl_cand$line[ord[1L]]
          } else {
            mx <- max(otl_cand$med_diff)
            in_band <- otl_cand$med_diff >= mx - 1
            if (sum(in_band) >= 2L) {
              # (2) >= 2 candidates bunched within 1 of the max med_diff: the
              # windowed median cannot discriminate, so exclude the highest
              # |tbc.sd| among the band members (tiebreak internal_id).
              b <- otl_cand[in_band]
              ord <- order(-abs(b$tbc.sd), b$internal_id)
              worst_line <- b$line[ord[1L]]
            } else {
              # (3) one candidate stands clearly apart: exclude highest med_diff
              # (tiebreak |tbc.sd|, then internal_id).
              ord <- order(-otl_cand$med_diff, -abs(otl_cand$tbc.sd), otl_cand$internal_id)
              worst_line <- otl_cand$line[ord[1L]]
            }
          }
        }

        # Mark exactly one exclusion per iteration
        df[line == worst_line, exclude := .child_exc(p, "Evil-Twins")]

        # Recalculate OTL on remaining Include values
        incl2 <- df[exclude == "Include"]
        if (nrow(incl2) < 4L) break
        # Must remove otl column before calling calc_otl_evil_twins, because
        # df[, "otl" := otl] inside that function self-assigns from the existing
        # column (data.table scope resolution) instead of using the local variable.
        if ("otl" %in% names(incl2)) incl2[, otl := NULL]
        incl2 <- calc_otl_evil_twins(incl2)
        # Map otl back to df (reset all, then set TRUE for OTL rows)
        df[, otl := FALSE]
        if (any(incl2$otl, na.rm = TRUE)) {
          df[incl2[otl == TRUE], otl := TRUE, on = .(line)]
        }
      }

      # Collect lines marked for exclusion
      excl <- df$line[df$exclude == "Exclude-C-Evil-Twins"]
      if (length(excl) > 0L) et_excl_lines <- c(et_excl_lines, excl)
    }

    # Apply all exclusions back to data.df
    if (length(et_excl_lines) > 0L) {
      data.df[line %in% et_excl_lines, exclude := .child_exc(param, "Evil-Twins")]
    }
  }
  data.df[, c("elig_9", "sp_count_9") := NULL]
  if ("uer" %in% names(data.df)) data.df[, uer := NULL]

  # 9d. Re-evaluate temp SDEs after Evil Twins exclusions: reset all
  # Exclude-C-Temp-Same-Day rows to Include, then rerun identify_temp_sde().
  # Rationale: an Evil Twins exclusion may have removed the prior temp-SDE
  # "keeper" on an SPA, so another value in that SPA should now be flagged.
  data.df[exclude == 'Exclude-C-Temp-Same-Day', exclude := 'Include']
  data.df[identify_temp_sde(data.df[, .(id, internal_id, subjid, param, agedays, tbc.sd, exclude)]), exclude := 'Exclude-C-Temp-Same-Day']

  # Step 11: Extreme EWMA ----
  # Flag extreme outliers using an exponentially weighted moving average
  # (EWMA). For each measurement, the EWMA predicts what the value should
  # be based on the subject's other measurements for the same parameter;
  # values that deviate extremely from the prediction are flagged
  # Exclude-C-Traj-Extreme. Only Include rows participate in the EWMA and
  # as exclusion candidates - temp SDEs do not participate. Because an
  # erroneous measurement can distort the EWMA of its neighbours, at most
  # one value is flagged per subject-param per pass; the global while
  # loop then re-processes any subject-param that produced a new exclusion
  # until no further exclusions occur.

  if (!quietly)
    message(sprintf("[%s] Exclude extreme measurements based on EWMA...", Sys.time()))

  # 11a. Pre-filter / setup -----------------------------------------------
  # Two filters narrow the work; a third captures subjects with existing
  # temp SDEs for later targeted recalculation.

  # Count filter: EWMA needs at least 3 Include values per subject-param.
  data.df[, sp_key := paste0(subjid, "_", param)]
  include_mask <- .child_valid(data.df, include.temporary.extraneous = FALSE)
  include_counts <- data.df[include_mask, .(n_include = .N), by = sp_key]
  sp_to_process <- include_counts[n_include > 2, sp_key]

  # qm_detail: eligibility for the Extreme EWMA step (Child Step 11). AT-RISK
  # denominator -- subject-params with >= 3 non-temp Include values (the EWMA
  # processing gate). Rows admitted with include.temporary.extraneous = TRUE:
  # a temp SDE cannot be flagged during a pass (pot_excl is set only on the
  # include_set, which excludes temp SDEs). But the per-pass temp-SDE recalc in
  # the 11b loop resets and re-derives temp SDEs for any subject that just
  # produced an exclusion, and can promote a set-aside temp SDE to Include;
  # because the next iteration reprocesses exactly the subject-params that just
  # excluded, a promoted row can then be excluded as Traj-Extreme in a later
  # iteration. So those temp-SDE rows are at-risk and belong in the denominator.
  # Captured before the |tbc.sd| > 3.5 pre-filter so groups that cannot fire
  # stay in the denominator (parallel to keeping range <= 1 groups in c_traj).
  if (qm_detail) {
    data.df[, qm_elig_c_trajextreme :=
              .child_valid(data.df, include.temporary.extraneous = TRUE) &
              (sp_key %in% sp_to_process)]
  }

  # Pre-filter: EWMA1 exclusion requires |tbc.sd| > 3.5.
  # Skip groups where no Include observation has |tbc.sd| > 3.5.
  extreme_sp <- data.df[include_mask & sp_key %in% sp_to_process,
                        .(max_abs_tbc = if (any(!is.na(tbc.sd)))
                            max(abs(tbc.sd), na.rm = TRUE) else 0),
                        by = sp_key]
  sp_to_process <- extreme_sp[max_abs_tbc > 3.5, sp_key]
  if (!quietly)
    message(sprintf("  EWMA1 pre-filter: %d/%d groups have |tbc.sd| > 3.5",
                length(sp_to_process), nrow(extreme_sp)))

  # Track subjects with SDEs for targeted temp SDE recalculation.
  subj_with_sde <- unique(data.df[exclude == "Exclude-C-Temp-Same-Day", subjid])

  # 11b. Iteration loop ---------------------------------------------------
  # Each pass processes one subject-param at a time via a per-group
  # closure operating on a copy of the group's rows. At most one value is
  # flagged per subject-param per iteration. After each pass, temp SDEs
  # are recalculated only for subjects that had a new exclusion and
  # already carried temp SDEs. The loop terminates when no subject-param
  # produces a new exclusion.
  iteration <- 0
  while (length(sp_to_process) > 0) {
    iteration <- iteration + 1
    if (!quietly)
      message(sprintf("  EWMA1 iteration %d: %d subject-params", iteration, length(sp_to_process)))

    # Track which rows had EWMA1 exclusions before this iteration
    data.df[sp_key %in% sp_to_process, had_ewma1_before := exclude == "Exclude-C-Traj-Extreme"]

    # Process each subject-param - ONE exclusion per subject-param per iteration
    data.df[sp_key %in% sp_to_process,
            exclude := (function(df) {
              # Only Include values participate (no temp SDEs)
              include_set <- .child_valid(df, include.temporary.extraneous = FALSE)

              if (sum(include_set) > 2) {
                # Initialize EWMA fields
                df[, (ewma.fields) := as.double(NaN)]

                # Calculate exp_vals based on age gaps (Include values only).
                # Uses linear interpolation for the exponent.
                inc_ages <- df$agedays[include_set]
                diff_before <- c(NA, diff(inc_ages))
                diff_after <- c(diff(inc_ages), NA)
                maxdiff <- pmax(abs(diff_before), abs(diff_after), na.rm = TRUE)
                ageyears <- maxdiff / 365.25
                exp_vals <- fcase(ageyears <= 1, -1.5, ageyears >= 3, -3.5,
                                  default = -1.5 - (ageyears - 1))
                df[include_set, exp_vals := exp_vals]

                # Calculate EWMA for Include values only (no temp SDEs)
                # Share delta matrix between tbc.sd and ctbc.sd calls (same agedays/exponents)
                ewma_cache <- new.env(parent = emptyenv())
                df[include_set, (ewma.fields) := ewma(agedays, tbc.sd, exp_vals, TRUE, window = ewma_window, cache_env = ewma_cache)]
                # Skip ctbc EWMA if ctbc.sd == tbc.sd for this group (vast majority)
                if (all(df$ctbc.sd[include_set] == df$tbc.sd[include_set])) {
                  df[include_set, paste0("c.",ewma.fields) := .SD, .SDcols = ewma.fields]
                } else {
                  df[include_set, paste0("c.",ewma.fields) := ewma(agedays, ctbc.sd, exp_vals, TRUE, window = ewma_window, cache_env = ewma_cache)]
                }

                # Calculate dewma for Include values only
                df[include_set, `:=`(
                  dewma.all = tbc.sd - ewma.all,
                  dewma.before = tbc.sd - ewma.before,
                  dewma.after = tbc.sd - ewma.after,
                  c.dewma.all = ctbc.sd - c.ewma.all
                )]

                # Identify potential exclusions - Include values only.
                # c.dewma.all is tested in the same direction as dewma.all
                # (> 3.5 for positive outliers, < -3.5 for negative).
                #
                # Plus/minus check: a value is only a candidate if it stays
                # beyond +/-3.5 even after a small measurement nudge in the
                # less-extreme direction (p_minus for a high outlier, p_plus for a low one),
                # against the EWMA. Applied to both the uncorrected (tbc) and, for
                # corrected values, the corrected (ctbc) z-scores; for non-corr
                # values ctbc.p == tbc.p so the corrected terms are inert.
                df[, pot_excl := FALSE]
                df[include_set, pot_excl :=
                     (dewma.all > 3.5 & dewma.before > 3 & dewma.after > 3 & tbc.sd > 3.5 &
                        (tbc.p_minus - ewma.all > 3.5) & tbc.p_minus > 3.5 &
                        ((!is.na(ctbc.sd) & c.dewma.all > 3.5 &
                          (ctbc.p_minus - c.ewma.all > 3.5) & ctbc.p_minus > 3.5) | is.na(ctbc.sd))
                     ) |
                     (dewma.all < -3.5 & dewma.before < -3 & dewma.after < -3 & tbc.sd < -3.5 &
                        (tbc.p_plus - ewma.all < -3.5) & tbc.p_plus < -3.5 &
                        ((!is.na(ctbc.sd) & c.dewma.all < -3.5 &
                          (ctbc.p_plus - c.ewma.all < -3.5) & ctbc.p_plus < -3.5) | is.na(ctbc.sd))
                     )
                ]

                # The LAST Include value can't be excluded in this step (its
                # EWMA is one-sided). The FIRST (birth) value is not
                # auto-protected here: an extreme birth error should be caught
                # in this extreme step, before Step 13 SDE resolution, rather
                # than surviving to the moderate steps (15/16) where it can
                # first distort SDE handling at Step 13. Birth is instead
                # protected only when it is part of a legitimate preterm
                # catch-up rise (below).
                include_indices <- which(include_set)
                if (length(include_indices) > 0) {
                  df[include_indices[length(include_indices)], pot_excl := FALSE]
                }

                # Protect early preterm catch-up rises using the SAME helper as
                # Steps 15/16 (.child_catchup_protect), so the extreme and
                # moderate steps agree. Rescue-only (it can clear a
                # candidate flag but never creates an exclusion) and fires only
                # for GA-corrected subjects. Corrected status is inferred from
                # low birth weight (generally accurate), so a genuinely low birth
                # (e.g. SGA) is flagged corrected and its catch-up rise is
                # protected here; a birth in a normal-birthweight (uncorrected)
                # subject is not protected, so an extreme birth error is caught
                # in this step rather than surviving to Step 13. Operates on
                # Include rows only (consecutive among includes), mapped back to
                # df rows via include_indices.
                if (length(include_indices) >= 2 &&
                    any(df$ctbc.sd[include_set] != df$tbc.sd[include_set], na.rm = TRUE)) {
                  protect <- .child_catchup_protect(df$agedays[include_set],
                                                    df$tbc.sd[include_set],
                                                    df$ctbc.sd[include_set])
                  if (any(protect)) df[include_indices[protect], pot_excl := FALSE]
                }

                # Exclude at most ONE value (the worst)
                num.exclude <- sum(df$pot_excl, na.rm = TRUE)
                if (num.exclude == 1) {
                  df[pot_excl == TRUE, exclude := .child_exc(param, "Traj-Extreme")]
                } else if (num.exclude > 1) {
                  # Select worst: highest abs(tbc.sd + dewma.all), lowest internal_id as tiebreaker
                  worst.row <- with(df, order(pot_excl, abs(tbc.sd + dewma.all), -internal_id, decreasing = TRUE))[1]
                  df[worst.row, exclude := .child_exc(param, "Traj-Extreme")]
                }
              }

              return(df$exclude)
            })(copy(.SD)),
            by = .(subjid, param),
            .SDcols = c('internal_id', 'param', 'agedays', 'tbc.sd', 'ctbc.sd',
                        'tbc.p_plus', 'tbc.p_minus', 'ctbc.p_plus', 'ctbc.p_minus', 'exclude')]

    # Find subject-params with NEW exclusions this iteration
    data.df[sp_key %in% sp_to_process, has_new_excl :=
              exclude == "Exclude-C-Traj-Extreme" & !had_ewma1_before]
    sp_with_new_excl <- unique(data.df[has_new_excl == TRUE, sp_key])
    subjects_with_new_excl <- unique(data.df[has_new_excl == TRUE, subjid])

    # Recalculate temp SDEs only for subjects that had exclusions AND have SDEs
    affected_sde_subj <- intersect(subjects_with_new_excl, subj_with_sde)
    if (length(affected_sde_subj) > 0) {
      # Reset temp SDEs to Include for affected subjects
      data.df[subjid %in% affected_sde_subj & exclude == 'Exclude-C-Temp-Same-Day',
              exclude := 'Include']
      # Recalculate temp SDEs for subset
      sde_subset <- data.df[subjid %in% affected_sde_subj]
      sde_result <- identify_temp_sde(sde_subset[, .(id, internal_id, subjid, param, agedays, tbc.sd, exclude)])
      # Apply results back using index
      sde_indices_to_mark <- sde_subset$index[sde_result]
      data.df[index %in% sde_indices_to_mark, exclude := 'Exclude-C-Temp-Same-Day']
    }

    # Next iteration: only subject-params with new exclusions
    sp_to_process <- sp_with_new_excl

    # Cleanup iteration tracking columns
    data.df[, c("had_ewma1_before", "has_new_excl") := NULL]
  }

  data.df[, sp_key := NULL]

  # 11c. End-of-step temp SDE refresh -------------------------------------
  # Reset all Exclude-C-Temp-Same-Day rows to Include, then rerun
  # identify_temp_sde() across the full dataset. Rationale: the per-
  # iteration targeted recalc only covers subjects that had both an
  # existing temp SDE and a new EWMA1 exclusion in that pass; this final
  # pass catches any residual drift before Step 13. Does NOT use
  # exclude_from_dop_ids - that biasing is specific to Step 13.
  data.df[exclude == 'Exclude-C-Temp-Same-Day', exclude := 'Include']
  data.df[identify_temp_sde(data.df[, .(id, internal_id, subjid, param, agedays, tbc.sd, exclude)]), exclude := 'Exclude-C-Temp-Same-Day']

  # 13: SDEs ----

  if (!quietly)
    message(sprintf(
      "[%s] Exclude same day extraneous...",
      Sys.time()
    ))

  # Pass temp SDE ids to exclude from DOP median.
  # In Step 13 the DOP median uses only fully-included values - temp SDEs are
  # still under consideration and should not bias the cross-parameter anchor.
  temp_sde_ids_step13 <- data.df[exclude == 'Exclude-C-Temp-Same-Day', id]

  # qm_detail: eligibility for the final Extraneous step (Child Step 13).
  # PREVALENCE denominator (X) -- every valid value at Step 13 entry, including
  # singleton-day values that can never be extraneous. Captured before the
  # keep_cols_sde snapshot below so it survives the SDE-block column cleanup.
  if (qm_detail) {
    data.df[, qm_elig_extraneous := .child_valid(data.df, include.temporary.extraneous = TRUE)]
  }

  # Safety check: no Include duplicates should exist before restoring temp SDEs
  pre_dupe_check <- data.df[exclude == "Include", .N, by = .(subjid, param, agedays)][N > 1]
  if (nrow(pre_dupe_check) > 0) {
    warning("Step 13: Found ", nrow(pre_dupe_check),
            " Include duplicates before restoring temp SDEs.")
  }

  data.df[exclude == 'Exclude-C-Temp-Same-Day', exclude := 'Include']
  data.df[identify_temp_sde(data.df[, .(id, internal_id, subjid, param, agedays, tbc.sd, exclude)],
                                        exclude_from_dop_ids = temp_sde_ids_step13),
          exclude := 'Exclude-C-Temp-Same-Day']

  # Safety check: no Include duplicates after temp SDE marking
  post_dupe_check <- data.df[exclude == "Include", .N, by = .(subjid, param, agedays)][N > 1]
  if (nrow(post_dupe_check) > 0) {
    warning("Step 13: Found ", nrow(post_dupe_check),
            " Include duplicates after temp SDE marking.")
  }
  keep_cols_sde <- names(data.df)

  # Pre-filter to subjects with same-day measurements (potential SDEs)
  # If a subject has no same-day measurements, SDE steps can do nothing - skip entirely
  # Exclude SDE-Identicals from count - they're already handled and shouldn't trigger SDE processing
  sde_day_counts <- data.df[exclude %in% c("Include", "Exclude-C-Temp-Same-Day"),
                            .(n_on_day = .N), by = .(subjid, param, agedays)]
  subj_with_sde_days <- unique(sde_day_counts[n_on_day > 1, subjid])
  n_total_subj <- uniqueN(data.df$subjid)
  n_with_sde <- length(subj_with_sde_days)
  if (!quietly)
    message(sprintf("  SDE pre-filter: %d/%d subjects have same-day measurements (%.1f%%)",
                n_with_sde, n_total_subj, 100*n_with_sde/n_total_subj))

  # Only process subjects with same-day measurements
  if (n_with_sde == 0) {
    # No SDEs to process - skip entire SDE section
    if (!quietly) message("  No SDEs to process, skipping SDE step")
    data.sde <- data.df[0, ]  # Empty data.table with same structure
  } else {
    # Filter to subjects with at least one same-day group before the per-row
    # data.table work below.
    data.df_sde_subset <- data.df[subjid %in% subj_with_sde_days]

    # Mark groups that have a temp SDE
    data.df_sde_subset[, sde_this := any(exclude == "Exclude-C-Temp-Same-Day"),
                        by = .(subjid, param, agedays)]

    # Filter to valid rows (Include or Temp SDE), then keep only subjects with any SDE
    data.sde <- data.df_sde_subset[.child_valid(data.df_sde_subset, include.temporary.extraneous = TRUE)]
    data.sde[, has_sde_subj := any(sde_this == TRUE), by = .(subjid)]
    data.sde <- data.sde[has_sde_subj == TRUE]
    data.sde[, has_sde_subj := NULL]

    data.sde <- data.sde[order(subjid, param, agedays, internal_id)]

    # Defensive assertion: by this point every (subjid, param, agedays) group in
    # data.sde must already have distinct values. Early Child Step 13
    # (before Steps 5/6) already resolves every same-day same-value pair
    # among Include rows; `v` is never reassigned afterwards in the child
    # path; and no row re-enters this valid set except
    # Exclude-C-Temp-Same-Day -> Include, and those rows were already
    # Include at the early pass. The assertion guards that invariant, so a
    # future invariant-breaking change fails loudly here instead of silently
    # re-deriving the same answer.
    dup_check <- data.sde[, .(n_rows = .N, n_v = uniqueN(v)),
                           by = .(subjid, param, agedays)]
    if (any(dup_check$n_v < dup_check$n_rows))
      stop("Step 13: found ", sum(dup_check$n_rows - dup_check$n_v),
           " same-day duplicate value(s) remaining in data.sde; the Early ",
           "Child Step 13 same-day-identical invariant has been violated.")

    # Re-key so downstream per-row work uses internal_id as the within-SPA
    # tiebreaker.
    setkey(data.sde, subjid, param, agedays, internal_id)

  # Track temp SDE status before restoration
  data.sde[, was_temp_sde := exclude == 'Exclude-C-Temp-Same-Day']
  data.sde[exclude == 'Exclude-C-Temp-Same-Day', exclude := 'Include']
  data.sde <- data.sde[order(subjid, param, agedays, internal_id)]

  # -------------------------------------------
  # Phase B1: One-Day SDE identification + SDE-All-Extreme
  # -------------------------------------------
  # Flag one-day SDE subject-param pairs
  data.sde[, n_days_with_data := uniqueN(agedays[exclude == "Include"]),
           by = .(subjid, param)]
  data.sde[, has_sde_day := any(was_temp_sde == TRUE), by = .(subjid, param)]
  data.sde[, one_day_sde_flag := (n_days_with_data == 1L & has_sde_day)]

  # Median tbc and distance from median (by subjid, param, agedays)
  data.sde[, median_tbc := as.numeric(median(tbc.sd[exclude == "Include"], na.rm = TRUE)),
           by = .(subjid, param, agedays)]
  data.sde[, absdiff_rel_to_median := fifelse(
    exclude == "Include", abs(tbc.sd - median_tbc), NA_real_)]
  data.sde[, min_absdiff_rel_to_median := suppressWarnings(
    min(absdiff_rel_to_median, na.rm = TRUE)),
    by = .(subjid, param, agedays)]
  data.sde[is.infinite(min_absdiff_rel_to_median),
           min_absdiff_rel_to_median := NA_real_]

  # SDE-All-Extreme: if closest value to median is still > 2 SD away (one-day only)
  data.sde[one_day_sde_flag & !is.na(absdiff_rel_to_median) &
             !is.na(min_absdiff_rel_to_median) &

               min_absdiff_rel_to_median > 2,
           exclude := .child_exc(param, "Extraneous")]

  # DOP medians (cross-parameter grouping by subjid, agedays)
  # DOP median uses FULLY included values only
  data.sde[, HT_dop_med := fifelse(
    one_day_sde_flag & param == "HEIGHTCM",
    as.numeric(median(tbc.sd[exclude == "Include" & !was_temp_sde & param == "WEIGHTKG"],
                      na.rm = TRUE)),
    NA_real_),
    by = .(subjid, agedays)]
  data.sde[, WT_dop_med := fifelse(
    one_day_sde_flag & param == "WEIGHTKG",
    as.numeric(median(tbc.sd[exclude == "Include" & !was_temp_sde & param == "HEIGHTCM"],
                      na.rm = TRUE)),
    NA_real_),
    by = .(subjid, agedays)]
  data.sde[, HC_dop_med := fifelse(
    one_day_sde_flag & param == "HEADCM",
    as.numeric(median(tbc.sd[exclude == "Include" & !was_temp_sde & param == "HEIGHTCM"],
                      na.rm = TRUE)),
    NA_real_),
    by = .(subjid, agedays)]
  data.sde[, absdiff_dop_med := fcase(
    one_day_sde_flag & exclude == "Include" & param == "HEIGHTCM", abs(tbc.sd - HT_dop_med),
    one_day_sde_flag & exclude == "Include" & param == "WEIGHTKG", abs(tbc.sd - WT_dop_med),
    one_day_sde_flag & exclude == "Include" & param == "HEADCM", abs(tbc.sd - HC_dop_med),
    default = NA_real_
  )]

  data.sde[, absdiff_dop_for_sort := fifelse(
    is.na(absdiff_dop_med), Inf, as.numeric(absdiff_dop_med))]

  # Age-dependent id tiebreaker: agedays==0 picks lowest internal_id, otherwise highest
  data.sde[, tiebreaker_oneday := fifelse(
    agedays == 0L, internal_id, -internal_id)]

  # Select one value to keep per SDE group (sort by absdiff_median, absdiff_dop, tiebreaker).
  # Keyed on internal_id (guaranteed-unique INTEGER), not the user's id: the
  # no-eligible branch returns NA_integer_, which types this by-group column as
  # integer. A character id (e.g. a non-numeric "site-123") would then be coerced
  # to integer NA ("NAs introduced by coercion"), leaving keep_id NA for the group
  # so no row is kept AND none excluded -- two same-day Includes then survive and
  # crash the Step 19 DOP comparison. internal_id is always integer, so the branch
  # types are consistent and no coercion can occur.
  data.sde[, keep_id_oneday := {
    eligible_mask <- one_day_sde_flag & exclude == "Include"
    if (sum(eligible_mask, na.rm = TRUE) < 2L) NA_integer_
    else {
      eligible_ids <- internal_id[eligible_mask]
      eligible_median <- absdiff_rel_to_median[eligible_mask]
      eligible_dop <- absdiff_dop_for_sort[eligible_mask]
      eligible_tiebreaker <- tiebreaker_oneday[eligible_mask]
      ord <- order(eligible_median, eligible_dop, eligible_tiebreaker)
      eligible_ids[ord[1L]]
    }
  }, by = .(subjid, param, agedays)]

  # Assign SDE-One-Day exclusions
  data.sde[one_day_sde_flag & exclude == "Include" & !is.na(keep_id_oneday) &
             internal_id != keep_id_oneday,
           exclude := .child_exc(param, "Extraneous")]

  # -------------------------------------------
  # Phase B2: SDE-EWMA resolution
  # -------------------------------------------

  # a. Calculate EWMA for fully Included values only (exclude temp SDEs). This
  # ensures (1) only one value per same-day contributes to EWMA, and (2) the
  # diff calculation for the exponent is correct (no same-day 0 diffs).
  #
  # Robust SDE EWMA: also drop a lone far value (> 3 from the SP median) so a
  # lingering extreme -- e.g. an end-spike the Evil Twins anchor rule cedes to
  # Traj -- cannot inflate the EWMA anchor at an adjacent SDE day and spuriously
  # trip SDE-All-Extreme on a plausible same-day pair. Mirrors the robust median
  # drop in identify_temp_sde() (same > 3 cutoff, same lone n_far == 1 gate).
  data.sde[, sp_prov_med := as.numeric(median(
    tbc.sd[exclude %in% c("Include", "Exclude-C-Temp-Same-Day")], na.rm = TRUE)),
    by = .(subjid, param)]
  data.sde[, is_far_sde := exclude == "Include" & !is.na(tbc.sd) &
    !is.na(sp_prov_med) & abs(tbc.sd - sp_prov_med) > 3]
  data.sde[, n_far_sde := sum(is_far_sde), by = .(subjid, param)]
  ewma_df <- data.sde[exclude == "Include" & !was_temp_sde &
    !(is_far_sde & n_far_sde == 1L)]
  setkey(ewma_df, subjid, param, agedays, internal_id)

  # For each subject and parameter, calculate before/after gaps and assign exponent
  ewma_df[, c("diff_before", "diff_after") :=
            .(c(NA, diff(agedays)), c(diff(agedays), NA)),
          by = .(subjid, param)]

  # Take the larger of the before/after gaps and convert to years; this drives
  # the per-row EWMA exponent below (smaller gap -> less-negative exponent ->
  # heavier weight on closer neighbors).
  ewma_df[, maxdiff := pmax(abs(diff_before), abs(diff_after), na.rm = TRUE)]
  ewma_df[, ageyears := maxdiff / 365.25]

  # Apply the exact linear exponent rule from the specification
  ewma_df[, exp_val := fcase(
    ageyears <= 1, -1.5,
    ageyears >= 3, -3.5,
    default = -1.5 - (ageyears - 1)
  )]

  # Compute EWMA using these exponents, on both the uncorrected (tbc.sd) and
  # GA-corrected (ctbc.sd) frames. For a GA-corrected preterm infant the
  # uncorrected z rises steeply during catch-up and the uncorrected EWMA lags
  # it (NICU values stay in the window), so a correct same-day value can sit
  # > 1 above the uncorrected EWMA. The corrected frame is used below only on
  # dates whose same-day values are themselves corrected (ctbc.sd != tbc.sd):
  # there SDE-All-Extreme needs both frames to be extreme (as in Steps 11/15)
  # and the keeper is chosen on the corrected frame. Every other date --
  # children without GA correction, and corrected children once their values
  # are no longer corrected -- uses the uncorrected frame only, unchanged.
  ewma.fields <- c("ewma.all", "ewma.before", "ewma.after")
  ewma_df[, c(ewma.fields, "c.ewma.all") := {
    # Share the delta matrix between the tbc.sd and ctbc.sd calls
    ewma_cache <- new.env(parent = emptyenv())
    e <- ewma(agedays, tbc.sd, exp_val, TRUE, window = ewma_window, cache_env = ewma_cache)
    # Skip the ctbc EWMA when ctbc.sd == tbc.sd for this group (vast majority)
    c_all <- if (isTRUE(all(ctbc.sd == tbc.sd))) e$ewma.all
             else ewma(agedays, ctbc.sd, exp_val, FALSE, window = ewma_window, cache_env = ewma_cache)$ewma.all
    list(e$ewma.all, e$ewma.before, e$ewma.after, c_all)
  }, by = .(subjid, param)]

  # Merge EWMA columns back to data.sde. Key on internal_id (the sequential
  # 1:N row key), not the user's id: internal_id is guaranteed unique, so this
  # join can never blow up cartesian even if a caller's id were non-unique.
  ewma_merge_cols <- c("internal_id", grep("ewma", names(ewma_df), value = TRUE))
  data.sde <- merge(data.sde, ewma_df[, ..ewma_merge_cols], by = "internal_id", all.x = TRUE)

  # b. Assign EWMAs to temp SDEs (they need EWMA for SDE-EWMA resolution)
  # Compute max ewma.all from non-temp-SDE rows per group
  data.sde[, ewma_fill := suppressWarnings(
    max(ewma.all[!was_temp_sde], na.rm = TRUE)),
    by = .(subjid, param, agedays)]
  data.sde[is.infinite(ewma_fill), ewma_fill := NA_real_]
  data.sde[is.na(ewma.all) & was_temp_sde, ewma.all := ewma_fill]
  data.sde[, ewma_fill := NULL]
  # Same fill for the corrected-frame EWMA
  data.sde[, c_ewma_fill := suppressWarnings(
    max(c.ewma.all[!was_temp_sde], na.rm = TRUE)),
    by = .(subjid, param, agedays)]
  data.sde[is.infinite(c_ewma_fill), c_ewma_fill := NA_real_]
  data.sde[is.na(c.ewma.all) & was_temp_sde, c.ewma.all := c_ewma_fill]
  data.sde[, c_ewma_fill := NULL]

  # Use max(ewma) as the SPA (subject-parameter-ageday) reference point.
  # suppressWarnings: max/min on empty subsets produce -Inf/Inf with a warning;
  # converted to NA on the next line. Could use explicit length guards instead
  # (see commit 64c186b for the pattern), but these are in by-group expressions
  # where empty groups are rare and the Inf->NA conversion is correct.
  data.sde[, spa_ewma := suppressWarnings(max(ewma.all, na.rm = TRUE)),
           by = .(subjid, param, agedays)]
  data.sde[is.infinite(spa_ewma), spa_ewma := NA_real_]
  data.sde[, absdewma := abs(tbc.sd - spa_ewma)]
  data.sde[, c.spa_ewma := suppressWarnings(max(c.ewma.all, na.rm = TRUE)),
           by = .(subjid, param, agedays)]
  data.sde[is.infinite(c.spa_ewma), c.spa_ewma := NA_real_]
  data.sde[, c.absdewma := abs(ctbc.sd - c.spa_ewma)]

  # SDE-EWMA selection using sort logic
  data.sde[, n_available := sum(
    exclude %in% c("Exclude-C-Temp-Same-Day", "Include")),
    by = .(subjid, param, agedays)]
  data.sde[, min_absdewma := {
    vals <- absdewma[exclude %in% c("Exclude-C-Temp-Same-Day", "Include")]
    m <- suppressWarnings(min(vals, na.rm = TRUE))
    if (is.infinite(m)) NA_real_
    else m
  }, by = .(subjid, param, agedays)]
  data.sde[, c.min_absdewma := {
    vals <- c.absdewma[exclude %in% c("Exclude-C-Temp-Same-Day", "Include")]
    m <- suppressWarnings(min(vals, na.rm = TRUE))
    if (is.infinite(m)) NA_real_
    else m
  }, by = .(subjid, param, agedays)]

  # Corrected date: any eligible same-day value whose corrected z differs from
  # its uncorrected z. Only these dates use the corrected frame below; once a
  # child's values are no longer corrected, the corrected EWMA can still be
  # shifted by earlier corrected values in its window, so it is not used.
  data.sde[, corr_date := any(exclude %in% c("Exclude-C-Temp-Same-Day", "Include") &
                                !is.na(tbc.sd) & !is.na(ctbc.sd) & ctbc.sd != tbc.sd),
           by = .(subjid, param, agedays)]

  # SDE-All-Extreme if min absdewma > 1. On corrected dates the corrected frame
  # must also exceed 1 (as in Steps 11/15; a missing corrected value defers to
  # the uncorrected frame).
  data.sde[n_available >= 2L &
             exclude %in% c("Exclude-C-Temp-Same-Day", "Include") &
             !is.na(min_absdewma) & min_absdewma > 1 &
             (!corr_date | is.na(c.min_absdewma) | c.min_absdewma > 1),
           exclude := .child_exc(param, "Extraneous")]

  # Age-dependent tiebreaker for EWMA selection
  data.sde[, tiebreaker_ewma := fifelse(
    agedays == 0L, internal_id, -internal_id)]

  # Find the id to keep. On corrected dates: sort eligible values by
  # corrected-frame absdewma (asc), then uncorrected absdewma (asc; also covers
  # a missing corrected value), then tiebreaker (asc) -- during preterm
  # catch-up the lagging uncorrected EWMA would favor a wrong value just below
  # it. On all other dates: uncorrected absdewma (asc), then tiebreaker (asc).
  # Keyed on internal_id (guaranteed-unique INTEGER), not the user's id -- see the
  # keep_id_oneday note above: the NA_integer_ no-eligible branch types this column
  # as integer, and a non-numeric character id would be coerced to NA there,
  # silently leaving two same-day Includes that crash the Step 19 DOP comparison.
  data.sde[, keep_id_ewma := {
    eligible_mask <- exclude %in% c("Exclude-C-Temp-Same-Day", "Include")
    if (sum(eligible_mask) == 0L) NA_integer_
    else {
      eligible_ids <- internal_id[eligible_mask]
      eligible_c_absdewma <- c.absdewma[eligible_mask]
      eligible_absdewma <- absdewma[eligible_mask]
      eligible_tiebreaker <- tiebreaker_ewma[eligible_mask]
      ord <- if (corr_date[1L]) order(eligible_c_absdewma, eligible_absdewma, eligible_tiebreaker)
             else order(eligible_absdewma, eligible_tiebreaker)
      eligible_ids[ord[1L]]
    }
  }, by = .(subjid, param, agedays)]

  # Assign SDE-Extraneous exclusions: keep_id gets Include, others get Exclude-C-Extraneous
  data.sde[exclude %in% c("Exclude-C-Temp-Same-Day", "Include") &
             internal_id == keep_id_ewma,
           exclude := "Include"]
  data.sde[exclude %in% c("Exclude-C-Temp-Same-Day", "Include") &
             internal_id != keep_id_ewma,
           exclude := .child_exc(param, "Extraneous")]

  }  # End of SDE pre-filter else block

  # -------------------------------------------
  # Phase B3: Merge SDE results back to main data
  # -------------------------------------------
  sde_results <- data.sde[, .(internal_id, sde_exclude = exclude)]
  data.df <- merge(data.df, sde_results, by = "internal_id", all.x = TRUE)
  # Only overwrite rows that were in scope for SDE processing
  # (Include or Temp-SDE). Do not overwrite rows already carrying
  # permanent exclusion codes assigned before this block (e.g., PIV,
  # Evil Twins, early SDE-Identical).
  data.df[!is.na(sde_exclude) &
            exclude %in% c("Include",
                           "Exclude-C-Temp-Same-Day"),
          exclude := sde_exclude]
  data.df[, sde_exclude := NULL]

  # Invariant: after final SDE resolution, no (subjid, param, agedays) may hold
  # more than one Include. Downstream steps (esp. the Step 19 DOP comparison)
  # assume at most one same-day Include per param. Assert here -- at the true
  # site -- so any future regression fails loudly instead of surfacing as a
  # cryptic recycle error deep in Step 19. (Historic cause: a by-group keep-id
  # column typed integer by an NA_integer_ branch silently NA'd non-numeric
  # character ids; now keyed on internal_id.)
  sde_dupe_check <- data.df[exclude == "Include", .N, by = .(subjid, param, agedays)][N > 1]
  if (nrow(sde_dupe_check) > 0)
    stop("Step 13: ", nrow(sde_dupe_check), " (subjid, param, agedays) group(s) ",
         "retain >1 Include after SDE resolution; the one-Include-per-day ",
         "invariant is violated.")

  # Keep only original columns (drop any extras from SDE processing)
  extra_cols <- setdiff(names(data.df), keep_cols_sde)
  if (length(extra_cols) > 0L) data.df[, (extra_cols) := NULL]
  setkey(data.df, subjid, param, agedays, internal_id)

  # 15-16: moderate EWMA ----
  # Iteration is global rather than per-(subjid, param): p_plus / p_minus
  # neighbor age-gaps and their z-scores are computed once for all valid rows,
  # one shared while loop runs across the whole batch, and after each
  # iteration only the subject-param groups that produced a new exclusion are
  # re-processed. This avoids re-walking groups that have already converged.

  if (!quietly)
    message(sprintf("[%s] Exclude moderate EWMA...", Sys.time()))

  # The per-group closures below walk rows in
  # (subjid, param, agedays, internal_id) order; prior/next neighbor
  # differences and first_meas depend on this sort. Rows are already in that
  # order from the setkey() at the end of Step 13 above, and nothing since
  # reorders them, so no re-sort is needed here.

  # Pre-identify subject-params that need processing: Include values only,
  # 3+ per subject-param. All temp SDEs were resolved in Main Child Step 13,
  # so the default `.child_valid()` (temp SDEs excluded) is all that is
  # needed here; a temp SDE leaking through should be treated as ineligible.
  data.df[, sp_key := paste0(subjid, "_", param)]
  include_counts <- data.df[.child_valid(data.df),
                            .(n_include = .N), by = sp_key]
  sp_to_process_15 <- include_counts[n_include > 2, sp_key]

  # The plus/minus perturbation z-scores (tbc.p_plus/minus uncorrected,
  # ctbc.p_plus/minus corrected) are built once in cleangrowth() preprocessing
  # and ride in on data.df, so no per-batch perturbation build is needed
  # here. They are consumed by the addcrit guards in the EWMA2 loop below.

  # Pre-calculate the first_meas indicator used by Step 15's "first" rules.
  # Definition is param-specific:
  #   WEIGHTKG: the first Include row whose agedays > 0 (birth WT has its
  #     own birth-WT rules, so position 1 at agedays == 0 is not first_meas).
  #   HEIGHTCM / HEADCM: the first Include row among non-birth rows (birth
  #     HT/HC is handled by Step 16, except the keep_birth_sp15 3-value
  #     exception below, where the birth participates in Step 15).
  data.df[, first_meas := FALSE]
  data.df[sp_key %in% sp_to_process_15 & exclude == "Include" & param == "WEIGHTKG",
          first_meas := (seq_len(.N) == 1 & agedays > 0),
          by = .(subjid, param)]
  data.df[sp_key %in% sp_to_process_15 & exclude == "Include" & param %in% c("HEIGHTCM", "HEADCM") & agedays > 0,
          first_meas := (seq_len(.N) == 1),
          by = .(subjid, param)]

  # Step 15: EWMA2 global iterations (excluding agedays=0 for HT/HC) ----

  # 15a. Pre-filter / setup -----------------------------------------------
  # Birth HT/HC is normally excluded from Step 15 (Step 16 handles it).
  # EXCEPTION: for an HT/HC subject-param with exactly 3 valid values where one
  # is a birth, removing the birth would leave only 2 non-birth values -- too
  # little for a stable EWMA, and it caused the whole SP to drop out of Step 15
  # entirely. For these SPs we keep the birth as a normal Step 15 participant
  # (it enters the EWMA and is exclusion-eligible, scored by the same birth-value
  # rule used in Step 16). If Step 15 excludes any value, the SP falls below
  # Step 16's 3-value gate, so the birth is not re-evaluated there. Computed once
  # from the initial valid counts and held fixed so the incremental EWMA cache
  # only ever sees removals (a group that grows would break the cache).
  hthc_birth_counts <- data.df[.child_valid(data.df, include.temporary.extraneous = FALSE) &
                                 (param == "HEIGHTCM" | param == "HEADCM"),
                               .(n_tot = .N, n_birth = sum(agedays == 0L)),
                               by = sp_key]
  keep_birth_sp15 <- hthc_birth_counts[n_tot == 3L & n_birth >= 1L, sp_key]

  # Get initial subject-params to process
  step15_filter <- data.df$sp_key %in% sp_to_process_15 &
    .child_valid(data.df, include.temporary.extraneous = FALSE) &
    !((data.df$param == "HEIGHTCM" | data.df$param == "HEADCM") & data.df$agedays == 0 &
      !(data.df$sp_key %in% keep_birth_sp15))
  sp_counts_15 <- data.df[step15_filter, .(n = .N), by = sp_key]
  sp_to_process <- sp_counts_15[n > 2, sp_key]

  # qm_detail: eligibility for the Moderate EWMA step (Child Steps 15/16),
  # phase 1 (Step 15, non-birth). AT-RISK denominator -- >= 3 Include values per
  # subject-param. Captured before the tbc.sd range <= 1 pre-filter so those
  # groups stay in the denominator (decided: keep range <= 1). Step 16 (birth
  # HT/HC) ORs its participating rows into this same column below (union).
  if (qm_detail) {
    data.df[, qm_elig_c_traj := step15_filter & (sp_key %in% sp_to_process)]
  }

  # Pre-filter: All EWMA2 rules require addcrit (dewma.before > 1 AND dewma.after > 1).
  # dewma = tbc.sd - EWMA, where EWMA is a weighted average of other obs in the group.
  # |dewma| is bounded by range(tbc.sd). If range <= 1, all |dewma| <= 1,
  # so addcrit can never be TRUE and no exclusion rule can fire.
  tbc_range_15 <- data.df[step15_filter & sp_key %in% sp_to_process,
                           .(tbc_range = if (any(!is.na(tbc.sd)))
                               max(tbc.sd, na.rm = TRUE) - min(tbc.sd, na.rm = TRUE)
                             else 0),
                           by = sp_key]
  n_before_filter <- length(sp_to_process)
  sp_to_process <- tbc_range_15[tbc_range > 1, sp_key]
  if (!quietly)
    message(sprintf("  EWMA2 pre-filter: %d/%d groups have tbc.sd range > 1",
                length(sp_to_process), n_before_filter))

  # 15b. Iteration loop ---------------------------------------------------
  # Incremental EWMA cache: persists across iterations, keyed by sp_key
  ewma2_caches <- new.env(parent = emptyenv())

  iteration <- 0
  while (length(sp_to_process) > 0) {
    iteration <- iteration + 1
    if (!quietly)
      message(sprintf("  EWMA2 iteration %d: %d subject-params", iteration, length(sp_to_process)))

    # DOP snapshot: keyed table for fast O(log n) lookups inside group function
    # Refreshed each iteration because excludes change between iterations
    dop_snap <- data.df[exclude == "Include", .(subjid, param, agedays, tbc.sd)]
    setkey(dop_snap, subjid, param)

    # Recompute step15_filter each iteration - .child_valid() depends on
    # current exclusions, which change as rows are flagged.
    step15_filter <- data.df$sp_key %in% sp_to_process &
      .child_valid(data.df, include.temporary.extraneous = FALSE) &
      !((data.df$param == "HEIGHTCM" | data.df$param == "HEADCM") & data.df$agedays == 0 &
        !(data.df$sp_key %in% keep_birth_sp15))

    # Recompute first_meas each iteration (same definitions as the
    # pre-loop calc): the prior iteration may have excluded what was
    # previously the first non-birth Include row.
    data.df[sp_key %in% sp_to_process, first_meas := FALSE]
    data.df[sp_key %in% sp_to_process & exclude == "Include" & param == "WEIGHTKG",
            first_meas := (seq_len(.N) == 1 & agedays > 0),
            by = .(subjid, param)]
    data.df[sp_key %in% sp_to_process & exclude == "Include" & param %in% c("HEIGHTCM", "HEADCM") & agedays > 0,
            first_meas := (seq_len(.N) == 1),
            by = .(subjid, param)]

    # Track exclusions before this iteration
    ewma2_codes <- "Exclude-C-Traj"
    data.df[sp_key %in% sp_to_process, had_ewma2_before := (exclude %in% ewma2_codes)]

    # Process each subject-param - ONE exclusion per subject-param per iteration
    data.df[step15_filter,
            exclude := (function(df) {
              if (nrow(df) < 3) return(df$exclude)

              sp <- paste0(df$subjid[1], "_", df$param[1])

              # Initialize EWMA fields
              df[, (ewma.fields) := as.double(NaN)]

              # Try incremental EWMA update from cache
              used_cache <- FALSE
              if (exists(sp, envir = ewma2_caches)) {
                prev_cache <- get(sp, envir = ewma2_caches)
                excluded_ids <- setdiff(prev_cache$ids, df$id)
                if (length(excluded_ids) == 1L) {
                  cache <- ewma_cache_update(prev_cache, excluded_ids[1L])
                  if (!is.null(cache)) {
                    # Map cache results to df (both in agedays order)
                    df[, `:=`(ewma.all = cache$ewma.all, ewma.before = cache$ewma.before,
                              ewma.after = cache$ewma.after, c.ewma.all = cache$c.ewma.all)]
                    df[, exp_vals := cache$exp_vals]
                    used_cache <- TRUE
                  }
                }
              }

              if (!used_cache) {
                # Full computation (first iteration or cache mismatch)
                df[, c("diff_before", "diff_after") := .(c(NA, diff(agedays)), c(diff(agedays), NA))]
                df[, maxdiff := pmax(abs(diff_before), abs(diff_after), na.rm = TRUE)]
                df[, ageyears := maxdiff / 365.25]
                df[, exp_vals := fcase(ageyears <= 1, -1.5, ageyears >= 3, -3.5, default = -1.5 - (ageyears - 1))]

                cache <- ewma_cache_init(df$agedays, df$tbc.sd, df$ctbc.sd, df$exp_vals, df$id, window = ewma_window)
                df[, `:=`(ewma.all = cache$ewma.all, ewma.before = cache$ewma.before,
                          ewma.after = cache$ewma.after, c.ewma.all = cache$c.ewma.all)]
              }

              # Save cache for next iteration
              assign(sp, cache, envir = ewma2_caches)

              # Calculate dewma
              df[, `:=`(dewma.all = tbc.sd - ewma.all,
                        dewma.before = tbc.sd - ewma.before,
                        dewma.after = tbc.sd - ewma.after,
                        c.dewma.all = ctbc.sd - c.ewma.all)]

              # Calculate prior/next differences (uncorrected z + perturbations)
              df[, `:=`(tbc_diff_next = tbc.sd - c(tbc.sd[2:.N], NA),
                        tbc_diff_prior = tbc.sd - c(NA, tbc.sd[1:(.N-1)]))]
              df[, `:=`(tbc_diff_plus_next = tbc.p_plus - c(tbc.sd[2:.N], NA),
                        tbc_diff_plus_prior = tbc.p_plus - c(NA, tbc.sd[1:(.N-1)]))]
              df[, `:=`(tbc_diff_minus_next = tbc.p_minus - c(tbc.sd[2:.N], NA),
                        tbc_diff_minus_prior = tbc.p_minus - c(NA, tbc.sd[1:(.N-1)]))]
              # Corrected-z analogues. For non-corr values ctbc == tbc
              # and ctbc.p == tbc.p by construction, so these equal the
              # uncorrected diffs and the guard below is inert; they only bind
              # for actually-corrected values.
              df[, `:=`(ctbc_diff_next = ctbc.sd - c(ctbc.sd[2:.N], NA),
                        ctbc_diff_prior = ctbc.sd - c(NA, ctbc.sd[1:(.N-1)]))]
              df[, `:=`(ctbc_diff_plus_next = ctbc.p_plus - c(ctbc.sd[2:.N], NA),
                        ctbc_diff_plus_prior = ctbc.p_plus - c(NA, ctbc.sd[1:(.N-1)]))]
              df[, `:=`(ctbc_diff_minus_next = ctbc.p_minus - c(ctbc.sd[2:.N], NA),
                        ctbc_diff_minus_prior = ctbc.p_minus - c(NA, ctbc.sd[1:(.N-1)]))]

              # Additional criteria. A value is a candidate only if it stays >1z
              # (high) / <-1z (low) from each neighbor even under the toward-
              # plausible plus/minus nudge, on BOTH the uncorrected and (for
              # corrected values) the corrected z-scores.
              df[, addcrithigh := dewma.before > 1 & dewma.after > 1 &
                   ((tbc_diff_next > 1 & tbc_diff_plus_next > 1 & tbc_diff_minus_next > 1) | is.na(tbc_diff_next)) &
                   ((tbc_diff_prior > 1 & tbc_diff_plus_prior > 1 & tbc_diff_minus_prior > 1) | is.na(tbc_diff_prior)) &
                   ((ctbc_diff_next > 1 & ctbc_diff_plus_next > 1 & ctbc_diff_minus_next > 1) | is.na(ctbc_diff_next)) &
                   ((ctbc_diff_prior > 1 & ctbc_diff_plus_prior > 1 & ctbc_diff_minus_prior > 1) | is.na(ctbc_diff_prior))]
              df[, addcritlow := dewma.before < -1 & dewma.after < -1 &
                   ((tbc_diff_next < -1 & tbc_diff_plus_next < -1 & tbc_diff_minus_next < -1) | is.na(tbc_diff_next)) &
                   ((tbc_diff_prior < -1 & tbc_diff_plus_prior < -1 & tbc_diff_minus_prior < -1) | is.na(tbc_diff_prior)) &
                   ((ctbc_diff_next < -1 & ctbc_diff_plus_next < -1 & ctbc_diff_minus_next < -1) | is.na(ctbc_diff_next)) &
                   ((ctbc_diff_prior < -1 & ctbc_diff_plus_prior < -1 & ctbc_diff_minus_prior < -1) | is.na(ctbc_diff_prior))]

              # DOP lookup (using pre-computed keyed snapshot - O(log n) vs O(n) scan)
              compare_df <- dop_snap[.(df$subjid[1], get_dop(df$param[1]))]
              compare_df <- compare_df[!is.na(tbc.sd)]
              if (nrow(compare_df) > 0) {
                df[compare_df, tbc_dop := i.tbc.sd, on = "agedays"]
                df[is.na(tbc_dop), tbc_dop := median(compare_df$tbc.sd)]
              } else {
                df[, tbc_dop := NA]
              }

              df$rowind <- seq_len(nrow(df))
              n <- nrow(df)

              # All exclusion rules (mark candidates)
              df[, pot_excl := ""]

              # Middle
              df[dewma.all > 1 & (c.dewma.all > 1 | is.na(c.dewma.all)) & addcrithigh & !(rowind %in% c(1, n)),
                 pot_excl := .child_exc(param, "Traj")]
              df[dewma.all < -1 & (c.dewma.all < -1 | is.na(c.dewma.all)) & addcritlow & !(rowind %in% c(1, n)),
                 pot_excl := .child_exc(param, "Traj")]

              # Birth WT
              df[agedays == 0 & c(agedays[2:.N], NA) < 365.25 & dewma.all > 3 & (c.dewma.all > 3 | is.na(c.dewma.all)) & addcrithigh,
                 pot_excl := .child_exc(param, "Traj")]
              df[agedays == 0 & c(agedays[2:.N], NA) < 365.25 & dewma.all < -3 & (c.dewma.all < -3 | is.na(c.dewma.all)) & addcritlow,
                 pot_excl := .child_exc(param, "Traj")]
              df[agedays == 0 & c(agedays[2:.N], NA) >= 365.25 & dewma.all > 4 & (c.dewma.all > 4 | is.na(c.dewma.all)) & addcrithigh,
                 pot_excl := .child_exc(param, "Traj")]
              df[agedays == 0 & c(agedays[2:.N], NA) >= 365.25 & dewma.all < -4 & (c.dewma.all < -4 | is.na(c.dewma.all)) & addcritlow,
                 pot_excl := .child_exc(param, "Traj")]

              # Protect a fairly-plausible isolated low uncorrected birth weight.
              # A birth WT (row 1, agedays 0) that is uncorrected (ctbc == tbc),
              # low (tbc.sd < -2), and isolated (next weight >= 2 years away, the
              # same window used for GA-correction corroboration) is only
              # excludable if it is both substantially low AND the jump to the
              # next weight is large: |tbc.sd| + (tbc.sd[next] - tbc.sd) > 7.
              # Otherwise a distant string of high weights should not drag out an
              # otherwise plausible low birth weight. (Corrected preterm catch-up
              # is handled separately by .child_catchup_protect below.)
              if (df$param[1] == "WEIGHTKG" && nrow(df) >= 2 &&
                  isTRUE(df$agedays[1] == 0) &&
                  isTRUE(df$ctbc.sd[1] == df$tbc.sd[1]) &&
                  isTRUE(df$tbc.sd[1] < -2) &&
                  isTRUE(df$agedays[2] >= 2 * 365.25)) {
                birth_catchup_sum <- abs(df$tbc.sd[1]) + (df$tbc.sd[2] - df$tbc.sd[1])
                if (!isTRUE(birth_catchup_sum > 7)) df[1, pot_excl := ""]
              }

              # First
              df[first_meas & (c(agedays[2:.N], NA) - agedays < 365.25) & dewma.all > 2 & (c.dewma.all > 2 | is.na(c.dewma.all)) & addcrithigh,
                 pot_excl := .child_exc(param, "Traj")]
              df[first_meas & (c(agedays[2:.N], NA) - agedays < 365.25) & dewma.all < -2 & (c.dewma.all < -2 | is.na(c.dewma.all)) & addcritlow,
                 pot_excl := .child_exc(param, "Traj")]

              df[first_meas & (c(agedays[2:.N], NA) - agedays >= 365.25) & dewma.all > 3 & (c.dewma.all > 3 | is.na(c.dewma.all)) & addcrithigh,
                 pot_excl := .child_exc(param, "Traj")]
              df[first_meas & (c(agedays[2:.N], NA) - agedays >= 365.25) & dewma.all < -3 & (c.dewma.all < -3 | is.na(c.dewma.all)) & addcritlow,
                 pot_excl := .child_exc(param, "Traj")]

              # Last
              if (n >= 2) {
                gap_last <- df$agedays[n] - df$agedays[n-1]
                tbc_prev <- df$tbc.sd[n-1]
                df[rowind == n & gap_last < 365.25*2 & abs(tbc_prev) < 2 & dewma.all > 2 & (c.dewma.all > 2 | is.na(c.dewma.all)) & addcrithigh,
                   pot_excl := .child_exc(param, "Traj")]
                df[rowind == n & gap_last < 365.25*2 & abs(tbc_prev) < 2 & dewma.all < -2 & (c.dewma.all < -2 | is.na(c.dewma.all)) & addcritlow,
                   pot_excl := .child_exc(param, "Traj")]
                # Corrected-z gate is the lesser of abs(tbc_prev) or 3: it must
                # never sit above the uncorrected bar (dewma.all > abs(tbc_prev)),
                # which for abs(tbc_prev) in [2,3) a fixed 3 did, silently
                # tightening the last-value rule. min() keeps the gate inert for
                # uncorrected values and no higher than the main bar for corrected.
                df[rowind == n & gap_last < 365.25*2 & abs(tbc_prev) >= 2 & dewma.all > abs(tbc_prev) & (c.dewma.all > min(abs(tbc_prev), 3) | is.na(c.dewma.all)) & addcrithigh,
                   pot_excl := .child_exc(param, "Traj")]
                df[rowind == n & gap_last < 365.25*2 & abs(tbc_prev) >= 2 & dewma.all < -abs(tbc_prev) & (c.dewma.all < -min(abs(tbc_prev), 3) | is.na(c.dewma.all)) & addcritlow,
                   pot_excl := .child_exc(param, "Traj")]
                df[rowind == n & gap_last >= 365.25*2 & abs(tbc_prev) < 2 & dewma.all > 3 & (c.dewma.all > 3 | is.na(c.dewma.all)) &
                     (tbc.sd - tbc_dop > 4 | is.na(tbc_dop)) & addcrithigh, pot_excl := .child_exc(param, "Traj")]
                df[rowind == n & gap_last >= 365.25*2 & abs(tbc_prev) < 2 & dewma.all < -3 & (c.dewma.all < -3 | is.na(c.dewma.all)) &
                     (tbc.sd - tbc_dop < -4 | is.na(tbc_dop)) & addcritlow, pot_excl := .child_exc(param, "Traj")]
                df[rowind == n & gap_last >= 365.25*2 & abs(tbc_prev) >= 2 & dewma.all > 1+abs(tbc_prev) & (c.dewma.all > 3 | is.na(c.dewma.all)) &
                     (tbc.sd - tbc_dop > 4 | is.na(tbc_dop)) & addcrithigh, pot_excl := .child_exc(param, "Traj")]
                df[rowind == n & gap_last >= 365.25*2 & abs(tbc_prev) >= 2 & dewma.all < -1-abs(tbc_prev) & (c.dewma.all < -3 | is.na(c.dewma.all)) &
                     (tbc.sd - tbc_dop < -4 | is.na(tbc_dop)) & addcritlow, pot_excl := .child_exc(param, "Traj")]
              }

              # Protect both members of an early catch-up increase from
              # exclusion (see .child_catchup_protect): preterm infants
              # legitimately rise fast in z during early infancy. Rescue-only
              # (it can protect a flagged value but never creates an exclusion),
              # and fires only for GA-corrected subjects.
              if (any(df$ctbc.sd != df$tbc.sd, na.rm = TRUE)) {
                protect <- .child_catchup_protect(df$agedays, df$tbc.sd, df$ctbc.sd)
                if (any(protect)) df[protect, pot_excl := ""]
              }

              # Exclude the worst candidate (highest abssum). Birth HT/HC rows
              # are filtered out of step15_filter upstream EXCEPT for the
              # keep_birth_sp15 exactly-3-value SPs (see the Step 15 header
              # comment), where the birth enters df and is a normal candidate
              # scored by the birth-value rules above.
              candidates <- df[pot_excl != ""]
              if (nrow(candidates) > 0) {
                candidates[, abssum := abs(tbc.sd + dewma.all)]
                # internal_id breaks ties on abssum (rare but possible - abssum
                # is continuous). order() is deterministic; which.max is not
                # when rows tie.
                ord <- order(-candidates$abssum, candidates$internal_id)
                worst_idx <- candidates$index[ord[1]]
                df[index == worst_idx, exclude := pot_excl]
              }

              return(df$exclude)
            })(copy(.SD)),
            by = .(subjid, param),
            .SDcols = c('index', 'id', 'internal_id', 'subjid', 'param', 'agedays', 'tbc.sd', 'ctbc.sd',
                        'tbc.p_plus', 'tbc.p_minus', 'ctbc.p_plus', 'ctbc.p_minus', 'first_meas', 'exclude')]

    # Find subject-params with NEW exclusions this iteration
    data.df[sp_key %in% sp_to_process, has_new_excl := (exclude %in% ewma2_codes) & !had_ewma2_before]
    sp_with_new_excl <- unique(data.df[has_new_excl == TRUE, sp_key])

    # Next iteration: only subject-params with new exclusions
    sp_to_process <- sp_with_new_excl

    # Cleanup iteration tracking
    data.df[, c("had_ewma2_before", "has_new_excl") := NULL]
  }
  rm(ewma2_caches)  # free cache memory

  # Step 16: Birth HT/HC ----

  if (!quietly)
    message(sprintf("[%s] Exclude moderate EWMA for birth height and head circumference...", Sys.time()))

  # 16a. Pre-filter / setup -----------------------------------------------
  # Filter for Step 16: only HT/HC subject-params that themselves have a
  # birth measurement (agedays == 0) with 3+ Include values. A subject can
  # have birth HT without birth HC (or vice versa); use the per-param
  # sp_key rather than subjid so the no-birth param is skipped.
  sp_with_birth <- unique(data.df[param %in% c("HEIGHTCM", "HEADCM") &
                                    .child_valid(data.df) &
                                    agedays == 0, sp_key])

  step16_filter <- data.df$param %in% c("HEIGHTCM", "HEADCM") &
    .child_valid(data.df) &
    data.df$sp_key %in% sp_with_birth
  sp_counts_16 <- data.df[step16_filter, .(n = .N), by = sp_key]
  sp_to_process <- sp_counts_16[n > 2, sp_key]

  # qm_detail: Moderate EWMA eligibility, phase 2 (Step 16, birth HT/HC). OR the
  # >= 3-Include birth-HT/HC participating rows into qm_elig_c_traj (union of the
  # two phases, decided). Captured before the range <= 1 pre-filter.
  if (qm_detail) {
    data.df[step16_filter & (sp_key %in% sp_to_process), qm_elig_c_traj := TRUE]
  }

  # Pre-filter: same logic as Step 15 - all rules require addcrit (dewma > 1),
  # and |dewma| is bounded by tbc.sd range. Skip groups with range <= 1.
  tbc_range_16 <- data.df[step16_filter & sp_key %in% sp_to_process,
                           .(tbc_range = if (any(!is.na(tbc.sd)))
                               max(tbc.sd, na.rm = TRUE) - min(tbc.sd, na.rm = TRUE)
                             else 0),
                           by = sp_key]
  n_before_filter_16 <- length(sp_to_process)
  sp_to_process <- tbc_range_16[tbc_range > 1, sp_key]
  if (!quietly)
    message(sprintf("  EWMA2-birth-HT-HC pre-filter: %d/%d groups have tbc.sd range > 1",
                length(sp_to_process), n_before_filter_16))

  # 16b. Iteration loop ---------------------------------------------------
  # Incremental EWMA cache for Step 16
  ewma2b_caches <- new.env(parent = emptyenv())

  iteration <- 0
  while (length(sp_to_process) > 0) {
    iteration <- iteration + 1
    if (!quietly)
      message(sprintf("  EWMA2-birth-HT-HC iteration %d: %d subject-params", iteration, length(sp_to_process)))

    # Recompute step16_filter each iteration - .child_valid() depends on
    # current exclusions, which change as rows are flagged. sp_to_process
    # is already the surviving per-sp_key set, so we don't need to repeat
    # the sp_with_birth check here.
    step16_filter <- data.df$sp_key %in% sp_to_process &
      data.df$param %in% c("HEIGHTCM", "HEADCM") &
      .child_valid(data.df)

    ewma2_hthc_codes <- "Exclude-C-Traj"
    data.df[sp_key %in% sp_to_process, had_ewma2_before := (exclude %in% ewma2_hthc_codes)]

    data.df[step16_filter,
            exclude := (function(df) {
              if (nrow(df) < 3) return(df$exclude)

              sp <- paste0(df$subjid[1], "_", df$param[1])

              df[, (ewma.fields) := as.double(NaN)]

              # Try incremental EWMA update from cache
              used_cache <- FALSE
              if (exists(sp, envir = ewma2b_caches)) {
                prev_cache <- get(sp, envir = ewma2b_caches)
                excluded_ids <- setdiff(prev_cache$ids, df$id)
                if (length(excluded_ids) == 1L) {
                  cache <- ewma_cache_update(prev_cache, excluded_ids[1L])
                  if (!is.null(cache)) {
                    df[, `:=`(ewma.all = cache$ewma.all, ewma.before = cache$ewma.before,
                              ewma.after = cache$ewma.after, c.ewma.all = cache$c.ewma.all)]
                    df[, exp_vals := cache$exp_vals]
                    used_cache <- TRUE
                  }
                }
              }

              if (!used_cache) {
                df[, c("diff_before", "diff_after") := .(c(NA, diff(agedays)), c(diff(agedays), NA))]
                df[, maxdiff := pmax(abs(diff_before), abs(diff_after), na.rm = TRUE)]
                df[, ageyears := maxdiff / 365.25]
                df[, exp_vals := fcase(ageyears <= 1, -1.5, ageyears >= 3, -3.5, default = -1.5 - (ageyears - 1))]

                cache <- ewma_cache_init(df$agedays, df$tbc.sd, df$ctbc.sd, df$exp_vals, df$id, window = ewma_window)
                df[, `:=`(ewma.all = cache$ewma.all, ewma.before = cache$ewma.before,
                          ewma.after = cache$ewma.after, c.ewma.all = cache$c.ewma.all)]
              }

              # Save cache for next iteration
              assign(sp, cache, envir = ewma2b_caches)

              df[, `:=`(dewma.all = tbc.sd - ewma.all, dewma.before = tbc.sd - ewma.before,
                        dewma.after = tbc.sd - ewma.after, c.dewma.all = ctbc.sd - c.ewma.all)]

              df[, `:=`(tbc_diff_next = tbc.sd - c(tbc.sd[2:.N], NA),
                        tbc_diff_prior = tbc.sd - c(NA, tbc.sd[1:(.N-1)]))]
              df[, `:=`(tbc_diff_plus_next = tbc.p_plus - c(tbc.sd[2:.N], NA),
                        tbc_diff_plus_prior = tbc.p_plus - c(NA, tbc.sd[1:(.N-1)]))]
              df[, `:=`(tbc_diff_minus_next = tbc.p_minus - c(tbc.sd[2:.N], NA),
                        tbc_diff_minus_prior = tbc.p_minus - c(NA, tbc.sd[1:(.N-1)]))]
              # Corrected-z analogues; inert for non-corr values.
              df[, `:=`(ctbc_diff_next = ctbc.sd - c(ctbc.sd[2:.N], NA),
                        ctbc_diff_prior = ctbc.sd - c(NA, ctbc.sd[1:(.N-1)]))]
              df[, `:=`(ctbc_diff_plus_next = ctbc.p_plus - c(ctbc.sd[2:.N], NA),
                        ctbc_diff_plus_prior = ctbc.p_plus - c(NA, ctbc.sd[1:(.N-1)]))]
              df[, `:=`(ctbc_diff_minus_next = ctbc.p_minus - c(ctbc.sd[2:.N], NA),
                        ctbc_diff_minus_prior = ctbc.p_minus - c(NA, ctbc.sd[1:(.N-1)]))]

              df[, addcrithigh := dewma.before > 1 & dewma.after > 1 &
                   ((tbc_diff_next > 1 & tbc_diff_plus_next > 1 & tbc_diff_minus_next > 1) | is.na(tbc_diff_next)) &
                   ((tbc_diff_prior > 1 & tbc_diff_plus_prior > 1 & tbc_diff_minus_prior > 1) | is.na(tbc_diff_prior)) &
                   ((ctbc_diff_next > 1 & ctbc_diff_plus_next > 1 & ctbc_diff_minus_next > 1) | is.na(ctbc_diff_next)) &
                   ((ctbc_diff_prior > 1 & ctbc_diff_plus_prior > 1 & ctbc_diff_minus_prior > 1) | is.na(ctbc_diff_prior))]
              df[, addcritlow := dewma.before < -1 & dewma.after < -1 &
                   ((tbc_diff_next < -1 & tbc_diff_plus_next < -1 & tbc_diff_minus_next < -1) | is.na(tbc_diff_next)) &
                   ((tbc_diff_prior < -1 & tbc_diff_plus_prior < -1 & tbc_diff_minus_prior < -1) | is.na(tbc_diff_prior)) &
                   ((ctbc_diff_next < -1 & ctbc_diff_plus_next < -1 & ctbc_diff_minus_next < -1) | is.na(ctbc_diff_next)) &
                   ((ctbc_diff_prior < -1 & ctbc_diff_plus_prior < -1 & ctbc_diff_minus_prior < -1) | is.na(ctbc_diff_prior))]

              df[, pot_excl := ""]
              next_age <- if (nrow(df) > 1) df$agedays[2] else NA

              df[agedays == 0 & !is.na(next_age) & next_age < 365.25 & dewma.all > 2 & (c.dewma.all > 2 | is.na(c.dewma.all)) & addcrithigh,
                 pot_excl := .child_exc(param, "Traj")]
              df[agedays == 0 & !is.na(next_age) & next_age < 365.25 & dewma.all < -3 & (c.dewma.all < -3 | is.na(c.dewma.all)) & addcritlow,
                 pot_excl := .child_exc(param, "Traj")]
              df[agedays == 0 & !is.na(next_age) & next_age >= 365.25 & dewma.all > 3 & (c.dewma.all > 3 | is.na(c.dewma.all)) & addcrithigh,
                 pot_excl := .child_exc(param, "Traj")]
              df[agedays == 0 & !is.na(next_age) & next_age >= 365.25 & dewma.all < -4 & (c.dewma.all < -4 | is.na(c.dewma.all)) & addcritlow,
                 pot_excl := .child_exc(param, "Traj")]

              # Protect the birth value from exclusion when birth -> next is an
              # early catch-up increase (see .child_catchup_protect): preterm
              # infants legitimately rise fast in z during early infancy.
              # Rescue-only, and fires only for GA-corrected subjects.
              if (any(df$ctbc.sd != df$tbc.sd, na.rm = TRUE)) {
                protect <- .child_catchup_protect(df$agedays, df$tbc.sd, df$ctbc.sd)
                if (any(protect)) df[protect, pot_excl := ""]
              }

              candidates <- df[pot_excl != ""]
              if (nrow(candidates) > 0) {
                candidates[, abssum := abs(tbc.sd + dewma.all)]
                # internal_id breaks ties on abssum (same convention as Step 15).
                ord <- order(-candidates$abssum, candidates$internal_id)
                worst_idx <- candidates$index[ord[1]]
                df[index == worst_idx, exclude := pot_excl]
              }

              return(df$exclude)
            })(copy(.SD)),
            by = .(subjid, param),
            .SDcols = c('index', 'id', 'internal_id', 'subjid', 'param', 'agedays', 'tbc.sd', 'ctbc.sd',
                        'tbc.p_plus', 'tbc.p_minus', 'ctbc.p_plus', 'ctbc.p_minus', 'exclude')]

    data.df[sp_key %in% sp_to_process, has_new_excl := (exclude %in% ewma2_hthc_codes) & !had_ewma2_before]
    sp_with_new_excl <- unique(data.df[has_new_excl == TRUE, sp_key])
    sp_to_process <- sp_with_new_excl
    data.df[, c("had_ewma2_before", "has_new_excl") := NULL]
  }
  rm(ewma2b_caches)  # free cache memory

  # Cleanup
  data.df[, sp_key := NULL]

  # Drop columns not needed after Steps 15-16.
  #   p_plus / p_minus / tbc.p_plus / tbc.p_minus: feed the addcrit
  #     perturbation check used by both Step 15 and Step 16.
  #   first_meas: only used by Step 15's first-row rules (Step 16 keys
  #     off agedays == 0 instead).
  cols_to_drop_15_16 <- intersect(c("p_plus", "p_minus", "tbc.p_plus", "tbc.p_minus", "first_meas"),
                                   names(data.df))
  if (length(cols_to_drop_15_16) > 0L) data.df[, (cols_to_drop_15_16) := NULL]

  # 17: raw differences ----

  if (!quietly)
    message(sprintf(
      "[%s] Exclude raw differences...",
      Sys.time()
    ))

  # tanner.ht.vel is loaded once in cleangrowth() and passed as a parameter;
  # the local alias tanner.ht.vel.rev is a leftover name kept so the merge
  # sites below do not need to be renamed.
  tanner.ht.vel.rev <- tanner.ht.vel

  # read in the who height data
  who_max_ht_vel_path <- ifelse(
    ref.data.path == "",
    system.file(file.path("extdata", "who_ht_maxvel_3sd.csv.gz"), package = "growthcleanr"),
    file.path(ref.data.path, "who_ht_maxvel_3sd.csv.gz")
  )

  who_ht_vel_3sd_path <- ifelse(
    ref.data.path == "",
    system.file(file.path("extdata", "who_ht_vel_3sd.csv.gz"), package = "growthcleanr"),
    file.path(ref.data.path, "who_ht_vel_3sd.csv.gz")
  )

  who.max.ht.vel <- fread(who_max_ht_vel_path)
  who.ht.vel <- fread(who_ht_vel_3sd_path)
  setkey(who.max.ht.vel, sex, whoagegrp_ht)
  setkey(who.ht.vel, sex, whoagegrp_ht)
  who.ht.vel <- merge(who.ht.vel, who.max.ht.vel, by = c('sex', 'whoagegrp_ht'), all = TRUE)

  setnames(who.ht.vel, colnames(who.ht.vel), gsub('_', '.', colnames(who.ht.vel)))
  setkey(who.ht.vel, sex, whoagegrp.ht)

  # read in who hc data.
  # NOTE: the WHO HC velocity tables reuse the WHO HT-velocity column schema
  # (whoagegrp_ht / whoinc_*_ht), so this HC table is keyed on `whoagegrp_ht`
  # despite being head-circumference data. Rows run 0-36 months, but Step 17
  # only reads ages <= 24 mo (whoagegrp.hc is NA-ed out beyond that).
  who_max_hc_vel_path <- ifelse(
    ref.data.path == "",
    system.file(file.path("extdata", "who_hc_maxvel_3sd_infants.csv.gz"), package = "growthcleanr"),
    file.path(ref.data.path, "who_hc_maxvel_3sd_infants.csv.gz")
  )

  who_hc_vel_3sd_path <- ifelse(
    ref.data.path == "",
    system.file(file.path("extdata", "who_hc_vel_3sd_infants.csv.gz"), package = "growthcleanr"),
    file.path(ref.data.path, "who_hc_vel_3sd_infants.csv.gz")
  )

  who.max.hc.vel <- fread(who_max_hc_vel_path)
  who.hc.vel <- fread(who_hc_vel_3sd_path)
  setkey(who.max.hc.vel, sex, whoagegrp_ht)
  setkey(who.hc.vel, sex, whoagegrp_ht)
  who.hc.vel <- merge(who.hc.vel, who.max.hc.vel, by = c('sex', 'whoagegrp_ht'), all = TRUE)

  setnames(who.hc.vel, colnames(who.hc.vel), gsub('_', '.', colnames(who.hc.vel)))
  setkey(who.hc.vel, sex, whoagegrp.ht)

  # Re-sort so the per-group shifts and merges below walk rows in
  # (subjid, param, agedays, internal_id) order; diff_prev / diff_next
  # and the per-row mindiff_prior / maxdiff_prior lags depend on this sort.
  data.df <- data.df[order(subjid, param, agedays, internal_id),]

  # valid_set is a logical vector indexed by row position, so it must be
  # computed on the sorted data (otherwise the positions would not align
  # with data.df after the sort above).
  # Step 17 runs only on HT / HC, on rows that are non-temp-SDE Include
  # and belong to a subject-param with at least 2 remaining Include rows
  # (singles are handled in Step 19, weight is excluded entirely because
  # weight can legitimately decrease). Counting only Include rows here --
  # not all rows regardless of exclusion state -- matches the Step 19
  # gate below; counting all rows let a subject-param down to a single
  # remaining Include (its other rows already excluded upstream) pass
  # this gate when it should fall through to Step 19 instead.
  include_df_17 <- data.df[exclude == "Include"]
  sp_counts_17 <- include_df_17[, .(sp_n = .N), by = .(subjid, param)]
  not_single <- sp_counts_17[data.df, on = .(subjid, param),
                              fifelse(is.na(sp_n), FALSE, sp_n > 1L)]
  valid_set <- .child_valid(data.df, include.temporary.extraneous = FALSE) &
    not_single &
    data.df$param != "WEIGHTKG"

  # qm_detail: eligibility for the Abs-Diff step (Child Step 17). AT-RISK
  # denominator -- HT/HC only, subject-params with >= 2 remaining Include
  # rows. The later raw-diff violation pre-filter is pure optimization
  # (numerator-side), not eligibility.
  if (qm_detail) {
    data.df[, qm_elig_c_absdiff := valid_set]
  }

  # ---- Step 17 Pre-filter: identify groups with raw-diff violations ----
  # Compute violation thresholds in one vectorized pass over all valid rows.
  # Only groups with at least one violation need the expensive per-group while loop.
  # Groups without violations are skipped entirely (saves copy(.SD), merges, EWMA, etc.)
  data.df[, sp_key := paste0(subjid, "_", param)]
  {
    pf <- data.df[valid_set, .(sp_key, subjid, param, agedays, id, internal_id, sex, v)]
    pf <- pf[order(subjid, param, agedays, internal_id)]

    # Compute gaps and raw value diffs within each group
    pf[, `:=`(
      d_agedays = shift(agedays, n = 1L, type = "lead") - agedays,
      diff_prev =
        v - shift(v, n = 1L, type = "lag"),
      diff_next =
        shift(v, n = 1L, type = "lead") - v
    ), by = .(subjid, param)]

    # Initialize threshold columns
    pf[, `:=`(mindiff = NA_real_, maxdiff = NA_real_)]

    # ---- HEIGHTCM thresholds ----
    ht_idx <- pf$param == "HEIGHTCM"
    if (any(ht_idx)) {
      # Tanner months (17B). Keyed on midpoint age of the adjacent-measurement
      # pair. Each Tanner row is centered on a half-year (30 = the 2-3 y
      # interval, 42 = 3-4 y, ...), so the midpoint maps to the interval that
      # contains it: 6 + 12 * floor(midpoint years). Midpoints in [1.5, 2) y
      # also use the 30-month row (the lowest entry) so those pairs keep a
      # Tanner check; midpoints below 1.5 y get NA - the same quantity used for
      # the merge, so the filter and the lookup are consistent.
      pf[ht_idx, tanner.mid.yrs :=
        .5 * (agedays + shift(agedays, n = 1L, type = "lead")) / 365.25,
        by = .(subjid)]
      pf[ht_idx, tanner.months := fifelse(
        tanner.mid.yrs >= 1.5 & tanner.mid.yrs < 2, 30L,
        6L + 12L * as.integer(floor(tanner.mid.yrs)))]
      pf[ht_idx & !is.na(tanner.months) & tanner.months < 30, tanner.months := NA_integer_]
      pf[, tanner.mid.yrs := NULL]

      # Merge tanner reference (update-on-join preserves row order)
      pf[tanner.ht.vel.rev, `:=`(min.ht.vel = i.min.ht.vel, max.ht.vel = i.max.ht.vel),
         on = .(sex, tanner.months)]

      # Apply max.ht.vel floors (17C-a)
      pf[ht_idx & !is.na(max.ht.vel) & max.ht.vel < 2.54, max.ht.vel := 2.54]
      pf[ht_idx & !is.na(d_agedays) & d_agedays > 2 * 30.4375 & !is.na(max.ht.vel) & max.ht.vel < 2 * 2.54,
         max.ht.vel := 2 * 2.54]
      pf[ht_idx & !is.na(d_agedays) & d_agedays > .5 * 365.25 & !is.na(max.ht.vel) & max.ht.vel < 4 * 2.54,
         max.ht.vel := 4 * 2.54]
      pf[ht_idx & !is.na(d_agedays) & d_agedays > 365.25 & !is.na(max.ht.vel) & max.ht.vel < 8 * 2.54,
         max.ht.vel := 8 * 2.54]

      # Tanner-based mindiff/maxdiff (17C-b). The `< 365.25` and `>= 365.25`
      # formulas are continuous at gap = 1 year, so the boundary assignment
      # matches either limit; `>=` is used so a (non-integer) d_agedays of
      # exactly 365.25 is covered.
      pf[ht_idx & !is.na(d_agedays) & d_agedays < 365.25,
         mindiff := .5 * min.ht.vel * (d_agedays / 365.25)^2 - 3]
      pf[ht_idx & !is.na(d_agedays) & d_agedays >= 365.25,
         mindiff := .5 * min.ht.vel - 3]
      pf[ht_idx & !is.na(d_agedays) & d_agedays < 365.25,
         maxdiff := 2 * max.ht.vel * (d_agedays / 365.25)^0.33 + 5.5]
      pf[ht_idx & !is.na(d_agedays) & d_agedays >= 365.25,
         maxdiff := 2 * max.ht.vel * (d_agedays / 365.25)^1.5 + 5.5]

      # WHO age groups (17D). WHO reference covers only 0-24 months; NA-out
      # whoagegrp.ht when either this row or the next row is beyond 24 months
      # so the pair does not extrapolate past the WHO range.
      # Intentionally different from the length/height boundary used elsewhere
      # (.gc_ht_start_day = day 730 is height): this velocity range runs through
      # 24 months (730.5 days), so a pair ending at day 730 is still checked
      # against the WHO increments.
      pf[ht_idx, whoagegrp.ht := NA_integer_]
      pf[ht_idx & agedays / 30.4375 <= 24, whoagegrp.ht := as.integer(round(agedays / 30.4375))]
      pf[ht_idx & !is.na(d_agedays) & (agedays + d_agedays) / 30.4375 > 24,
         whoagegrp.ht := NA_integer_]

      # WHO increment age groups (17E). The `< 46` and `>= 153` clauses each
      # cover their full range with no gap, so every d_agedays maps to a group
      # (e.g. d_agedays == 199 is included via `>= 153`).
      pf[ht_idx, whoinc.age.ht := NA_integer_]
      pf[ht_idx & !is.na(d_agedays) & d_agedays < 46, whoinc.age.ht := 1L]
      pf[ht_idx & !is.na(d_agedays) & d_agedays >= 46 & d_agedays < 76, whoinc.age.ht := 2L]
      pf[ht_idx & !is.na(d_agedays) & d_agedays >= 76 & d_agedays < 107, whoinc.age.ht := 3L]
      pf[ht_idx & !is.na(d_agedays) & d_agedays >= 107 & d_agedays < 153, whoinc.age.ht := 4L]
      pf[ht_idx & !is.na(d_agedays) & d_agedays >= 153, whoinc.age.ht := 6L]

      # Merge WHO reference data (17F) - update-on-join
      who_ht_cols <- setdiff(colnames(who.ht.vel), c("sex", "whoagegrp.ht"))
      pf[who.ht.vel, (who_ht_cols) := mget(paste0("i.", who_ht_cols)), on = .(sex, whoagegrp.ht)]

      # Extract WHO mindiff/maxdiff based on whoinc.age.ht
      pf[ht_idx & !is.na(whoinc.age.ht) & !is.na(whoagegrp.ht),
         who_mindiff_ht := fcase(
           whoinc.age.ht == 1L, whoinc.1.ht,
           whoinc.age.ht == 2L, whoinc.2.ht,
           whoinc.age.ht == 3L, whoinc.3.ht,
           whoinc.age.ht == 4L, whoinc.4.ht,
           whoinc.age.ht == 6L, whoinc.6.ht,
           default = NA_real_)]
      pf[ht_idx & !is.na(whoinc.age.ht) & !is.na(whoagegrp.ht),
         who_maxdiff_ht := fcase(
           whoinc.age.ht == 1L, max.whoinc.1.ht,
           whoinc.age.ht == 2L, max.whoinc.2.ht,
           whoinc.age.ht == 3L, max.whoinc.3.ht,
           whoinc.age.ht == 4L, max.whoinc.4.ht,
           whoinc.age.ht == 6L, max.whoinc.6.ht,
           default = NA_real_)]

      # Scale WHO values based on d_agedays (17G)
      pf[ht_idx & !is.na(d_agedays) & !is.na(whoinc.age.ht) & d_agedays < whoinc.age.ht * 30.4375,
         who_mindiff_ht := who_mindiff_ht * d_agedays / (whoinc.age.ht * 30.4375)]
      pf[ht_idx & !is.na(d_agedays) & !is.na(whoinc.age.ht) & d_agedays > whoinc.age.ht * 30.4375,
         who_maxdiff_ht := who_maxdiff_ht * d_agedays / (whoinc.age.ht * 30.4375)]
      pf[ht_idx & !is.na(d_agedays) & d_agedays < 9 * 30.4375,
         `:=`(who_mindiff_ht = who_mindiff_ht * .5 - 3,
              who_maxdiff_ht = who_maxdiff_ht * 2 + 3)]

      # Choose WHO vs Tanner (17H)
      # For gap < 9 months: use WHO (already transformed)
      pf[ht_idx & !is.na(d_agedays) & d_agedays < 9 * 30.4375 & !is.na(who_mindiff_ht),
         mindiff := who_mindiff_ht]
      pf[ht_idx & !is.na(d_agedays) & d_agedays < 9 * 30.4375 & !is.na(who_maxdiff_ht),
         maxdiff := who_maxdiff_ht]
      # For gap >= 9 months: Tanner preferred, WHO fallback with transformation
      pf[ht_idx & !is.na(d_agedays) & d_agedays >= 9 * 30.4375 & is.na(min.ht.vel) & !is.na(who_mindiff_ht),
         mindiff := who_mindiff_ht * .5 - 3]
      pf[ht_idx & !is.na(d_agedays) & d_agedays >= 9 * 30.4375 & is.na(min.ht.vel) & !is.na(who_maxdiff_ht),
         maxdiff := who_maxdiff_ht * 2 + 3]

      # Fill defaults
      pf[ht_idx & is.na(mindiff), mindiff := -3]
    }

    # ---- HEADCM thresholds ----
    # HC uses WHO velocity tables only (no Tanner); starts at 2-month intervals.
    # Tolerance: +/-1.5 cm (vs +/-3 cm for HT).
    hc_idx <- pf$param == "HEADCM"
    if (any(hc_idx)) {
      # WHO age group (same rounded start-month formula as HT)
      pf[, whoagegrp.hc := NA_integer_]
      pf[hc_idx & agedays / 30.4375 <= 24,
         whoagegrp.hc := as.integer(round(agedays / 30.4375))]

      # Interval selection: HC has no 1-month interval; smallest available is 2-month
      pf[, whoinc.age.hc := NA_integer_]
      pf[hc_idx & !is.na(d_agedays) & d_agedays >= 46  & d_agedays < 76,  whoinc.age.hc := 2L]
      pf[hc_idx & !is.na(d_agedays) & d_agedays >= 76  & d_agedays < 107, whoinc.age.hc := 3L]
      pf[hc_idx & !is.na(d_agedays) & d_agedays >= 107 & d_agedays < 153, whoinc.age.hc := 4L]
      pf[hc_idx & !is.na(d_agedays) & d_agedays >= 153 & d_agedays < 200, whoinc.age.hc := 6L]

      # Rename HC velocity columns to avoid collision with already-merged HT columns
      # (both tables use "whoinc.N.ht" naming after gsub; rename HC copies to "whoinc.N.hc")
      who.hc.vel.r <- copy(who.hc.vel)
      ht_suffix_cols <- grep("whoinc", names(who.hc.vel.r), value = TRUE)
      setnames(who.hc.vel.r,
        old = ht_suffix_cols,
        new = gsub("\\.ht$", ".hc", ht_suffix_cols))
      setnames(who.hc.vel.r, "whoagegrp.ht", "whoagegrp.hc")
      setkey(who.hc.vel.r, sex, whoagegrp.hc)

      # Merge HC reference - non-HC rows have whoagegrp.hc = NA so they are not matched
      hc_vel_cols <- setdiff(names(who.hc.vel.r), c("sex", "whoagegrp.hc"))
      pf[who.hc.vel.r, (hc_vel_cols) := mget(paste0("i.", hc_vel_cols)),
         on = .(sex, whoagegrp.hc)]

      # Extract interval-specific WHO mindiff/maxdiff for HC
      pf[hc_idx & !is.na(whoinc.age.hc) & !is.na(whoagegrp.hc),
         who_mindiff_hc := fcase(
           whoinc.age.hc == 2L, whoinc.2.hc,
           whoinc.age.hc == 3L, whoinc.3.hc,
           whoinc.age.hc == 4L, whoinc.4.hc,
           whoinc.age.hc == 6L, whoinc.6.hc,
           default = NA_real_)]
      pf[hc_idx & !is.na(whoinc.age.hc) & !is.na(whoagegrp.hc),
         who_maxdiff_hc := fcase(
           whoinc.age.hc == 2L, max.whoinc.2.hc,
           whoinc.age.hc == 3L, max.whoinc.3.hc,
           whoinc.age.hc == 4L, max.whoinc.4.hc,
           whoinc.age.hc == 6L, max.whoinc.6.hc,
           default = NA_real_)]

      # Scale by d_agedays vs interval length (same logic as HT)
      pf[hc_idx & !is.na(d_agedays) & !is.na(whoinc.age.hc) &
           d_agedays < whoinc.age.hc * 30.4375,
         who_mindiff_hc := who_mindiff_hc * d_agedays / (whoinc.age.hc * 30.4375)]
      pf[hc_idx & !is.na(d_agedays) & !is.na(whoinc.age.hc) &
           d_agedays > whoinc.age.hc * 30.4375,
         who_maxdiff_hc := who_maxdiff_hc * d_agedays / (whoinc.age.hc * 30.4375)]

      # Apply HC tolerance: +/-1.5 cm (vs +/-3 cm for HT), whenever WHO data
      # exists (not restricted by age).
      pf[hc_idx & !is.na(who_mindiff_hc),
         `:=`(who_mindiff_hc = who_mindiff_hc * .5 - 1.5,
              who_maxdiff_hc = who_maxdiff_hc * 2 + 1.5)]

      # Assign to mindiff/maxdiff (WHO only - no Tanner for HC)
      pf[hc_idx & !is.na(d_agedays) & !is.na(who_mindiff_hc), mindiff := who_mindiff_hc]
      pf[hc_idx & !is.na(d_agedays) & !is.na(who_maxdiff_hc), maxdiff := who_maxdiff_hc]

      # Fallback: HC > 24 months or no WHO data -> fixed -1.5.
      pf[hc_idx & is.na(mindiff), mindiff := -1.5]

      # Clean up temporary HC columns
      hc_tmp <- c("whoagegrp.hc", "whoinc.age.hc", "who_mindiff_hc", "who_maxdiff_hc",
                  hc_vel_cols)
      pf[, (intersect(hc_tmp, names(pf))) := NULL]
    }

    # Compute lagged thresholds (previous row's mindiff/maxdiff)
    pf[, `:=`(
      mindiff_prior = shift(mindiff, n = 1L, type = "lag"),
      maxdiff_prior = shift(maxdiff, n = 1L, type = "lag")
    ), by = .(subjid, param)]

    # Check for any raw-diff violations per group
    pf[, has_violation :=
      (!is.na(diff_prev) & !is.na(mindiff_prior) & diff_prev < mindiff_prior) |
      (!is.na(diff_next) & !is.na(mindiff) & diff_next < mindiff) |
      (!is.na(diff_prev) & !is.na(maxdiff_prior) & diff_prev > maxdiff_prior) |
      (!is.na(diff_next) & !is.na(maxdiff) & diff_next > maxdiff)]

    sp17_to_process <- pf[has_violation == TRUE, unique(sp_key)]
    if (!quietly)
      message(sprintf("  Step 17 pre-filter: %d/%d groups have violations",
                  length(sp17_to_process), length(unique(pf$sp_key))))
    rm(pf)
  }

  if (length(sp17_to_process) > 0)
  data.df[valid_set & sp_key %in% sp17_to_process, exclude := (function(df) {
    # work inside a closure to drop added column values

    # save initial exclusions and the original row indices so we can write
    # the final exclude values back to data.df in their original row order.
    ind_all <- copy(df$index)
    exclude_all <- copy(df$exclude)

    # Pre-merge WHO velocity columns BEFORE the while loop (once per group, not per iteration).
    # This matches HT's pattern and avoids column-duplication on iteration 2+ when merge
    # encounters columns already added in iteration 1 and creates .x/.y suffixes.
    if (df$param[1] == "HEIGHTCM") {
      # For HEIGHTCM: pre-compute whoagegrp.ht (static across iterations; agedays don't change)
      # WHO range through 24 months; intentionally not .gc_ht_start_day (see 17D).
      df[agedays/30.4375 <= 24, whoagegrp.ht := round(agedays/30.4375)]
      df <- merge(df, who.ht.vel, by = c("sex", "whoagegrp.ht"), all.x = TRUE, sort = FALSE)
    } else if (df$param[1] == "HEADCM") {
      # For HEADCM: pre-compute whoagegrp.hc and pre-merge WHO HC velocity columns.
      # whoinc.2.hc etc. are observation-level constants (based on agedays, not d_agedays)
      # so they only need to be merged once before the while loop.
      who.hc.vel.r <- copy(who.hc.vel)
      hc_suffix_cols <- grep("whoinc|whoagegrp", names(who.hc.vel.r), value = TRUE)
      setnames(who.hc.vel.r, old = hc_suffix_cols,
               new = gsub("\\.ht$", ".hc", hc_suffix_cols))
      df[, whoagegrp.hc := NA_real_]
      df[agedays / 30.4375 <= 24, whoagegrp.hc := round(agedays / 30.4375)]
      df <- merge(df, who.hc.vel.r, by = c("sex", "whoagegrp.hc"), all.x = TRUE, sort = FALSE)
    }

    testing <- TRUE

    while (testing & nrow(df) > 1){
      # Re-sort each iteration: the tanner / WHO merges below use
      # setkey(df, sex, ...) which reorders rows. The pairwise checks
      # rely on (agedays, internal_id) order, and internal_id is the
      # deterministic tiebreaker for same-day rows (none reach Step 17
      # in practice because SDEs are resolved upstream).
      df <- df[order(agedays, internal_id),]

      # 17A
      df[, d_agedays := shift(agedays, n = 1L, type = "lead") - agedays]

      # 17E -- only applies to height
      if (df$param[1] == "HEIGHTCM"){
        # 17B. tanner.months is keyed on the midpoint of the adjacent-measurement
        # pair: 6 + 12 * floor(midpoint years), the half-year-centered Tanner
        # row whose interval contains the midpoint. Midpoints in [1.5, 2) y also
        # use the 30-month row; NA-out when the bin falls below the lowest
        # Tanner reference entry (30 months) so the NA filter matches the merge
        # key. Must match the pre-filter computation above.
        df[, tanner.mid.yrs := .5*(agedays + shift(agedays, n = 1L, type = "lead"))/365.25]
        df[, tanner.months := fifelse(tanner.mid.yrs >= 1.5 & tanner.mid.yrs < 2, 30,
                                      6+12*floor(tanner.mid.yrs))]
        df[, tanner.mid.yrs := NULL]
        df[!is.na(tanner.months) & tanner.months < 30, tanner.months := NA]

        # merge with the tanner info
        setkey(df, sex, tanner.months)
        df <- tanner.ht.vel.rev[df]

        # 17C

        # a
        df[max.ht.vel < 2.54, max.ht.vel := 2.54]
        df[d_agedays > 2*30.4375 & max.ht.vel < 2*2.54, max.ht.vel := 2*2.54]
        df[d_agedays > .5*365.25 & max.ht.vel < 4*2.54, max.ht.vel := 4*2.54]
        df[d_agedays > 365.25 & max.ht.vel < 8*2.54, max.ht.vel := 8*2.54]

        # b. Tanner-based mindiff/maxdiff. The `< 365.25` and `>= 365.25`
        # formulas are continuous at gap = 1 year, so the boundary
        # assignment matches either limit; `>=` covers (non-integer)
        # d_agedays == 365.25. Maxdiff exponents: ^0.33 (cube root) for
        # gaps < 1 year, ^1.5 for gaps >= 1 year.
        df[d_agedays < 365.25, mindiff := .5*min.ht.vel*(d_agedays/365.25)^2-3 ]
        df[d_agedays >= 365.25, mindiff := .5*min.ht.vel-3 ]
        df[d_agedays < 365.25,
           maxdiff := 2*max.ht.vel*(d_agedays/365.25)^0.33 + 5.5 ]
        df[d_agedays >= 365.25,
           maxdiff := 2*max.ht.vel*(d_agedays/365.25)^1.5 + 5.5 ]

        # 17D: whoagegrp.ht pre-computed outside loop (static for HEIGHTCM)

        # 17E: WHO increment age groups (maps d_agedays gap to a WHO
        # reference interval). The `< 46` and `>= 153` clauses each cover
        # their full range with no gap, so every d_agedays maps to a group
        # (e.g. 199 via `>= 153`). Re-initialize to NA each iteration so a
        # boundary change that introduced a gap could not leave a stale value
        # from the prior iteration.
        df[, whoinc.age.ht := NA_integer_]
        df[d_agedays < 46, whoinc.age.ht := 1]
        df[d_agedays >= 46 & d_agedays < 76, whoinc.age.ht := 2]
        df[d_agedays >= 76 & d_agedays < 107, whoinc.age.ht := 3]
        df[d_agedays >= 107 & d_agedays < 153, whoinc.age.ht := 4]
        df[d_agedays >= 153, whoinc.age.ht := 6]

        # 17F: WHO velocity lookup (columns pre-merged outside loop; fcase selects by whoinc.age.ht)
        df[, who_mindiff_ht := fcase(
          !is.na(whoagegrp.ht) & whoinc.age.ht == 1, whoinc.1.ht,
          !is.na(whoagegrp.ht) & whoinc.age.ht == 2, whoinc.2.ht,
          !is.na(whoagegrp.ht) & whoinc.age.ht == 3, whoinc.3.ht,
          !is.na(whoagegrp.ht) & whoinc.age.ht == 4, whoinc.4.ht,
          !is.na(whoagegrp.ht) & whoinc.age.ht == 6, whoinc.6.ht,
          default = NA_real_
        )]
        df[, who_maxdiff_ht := fcase(
          !is.na(whoagegrp.ht) & whoinc.age.ht == 1, max.whoinc.1.ht,
          !is.na(whoagegrp.ht) & whoinc.age.ht == 2, max.whoinc.2.ht,
          !is.na(whoagegrp.ht) & whoinc.age.ht == 3, max.whoinc.3.ht,
          !is.na(whoagegrp.ht) & whoinc.age.ht == 4, max.whoinc.4.ht,
          !is.na(whoagegrp.ht) & whoinc.age.ht == 6, max.whoinc.6.ht,
          default = NA_real_
        )]

        # NA-out WHO values when the pair crosses the 24-month WHO boundary:
        # whoagegrp.ht is based on this row's agedays alone and was pre-merged
        # before the while loop, but d_agedays changes each iteration, so this
        # check has to be per-iteration. (In the pre-filter the equivalent check
        # is applied directly to whoagegrp.ht.)
        df[!is.na(d_agedays) & (agedays + d_agedays) / 30.4375 > 24,
           `:=`(who_mindiff_ht = NA_real_, who_maxdiff_ht = NA_real_)]

        # 17G: Scale WHO velocity limits by actual gap relative to reference interval
        df[d_agedays < whoinc.age.ht*30.4375, who_mindiff_ht :=
             who_mindiff_ht * d_agedays/(whoinc.age.ht*30.4375)]
        df[d_agedays > whoinc.age.ht*30.4375, who_maxdiff_ht :=
             who_maxdiff_ht * d_agedays/(whoinc.age.ht*30.4375)]
        # For gaps < 9 months: widen limits (allow more loss, allow more gain)
        df[d_agedays < 9*30.4375, who_mindiff_ht := who_mindiff_ht*.5-3]
        df[d_agedays < 9*30.4375, who_maxdiff_ht := who_maxdiff_ht*2+3]

        # 17H: Choose between Tanner and WHO velocity limits.
        # For gap < 9 months: use WHO (already transformed in 17G above).
        df[d_agedays < 9*30.4375 & !is.na(who_mindiff_ht), mindiff := who_mindiff_ht]
        df[d_agedays < 9*30.4375 & !is.na(who_maxdiff_ht), maxdiff := who_maxdiff_ht]
        # Apply transformation for >= 9 month gaps.
        # When gap >= 9 months, Tanner is preferred. But if Tanner is not
        # available (min.ht.vel is NA), use WHO as fallback WITH the
        # transformation (same scaling as the < 9 month path).
        df[d_agedays >= 9*30.4375 & is.na(min.ht.vel) & !is.na(who_mindiff_ht),
           mindiff := who_mindiff_ht*.5-3]
        df[d_agedays >= 9*30.4375 & is.na(min.ht.vel) & !is.na(who_maxdiff_ht),
           maxdiff := who_maxdiff_ht*2+3]

        # Fallback mindiff default
        df[is.na(mindiff), mindiff := -3]

        # 17I: sort df since the Tanner merge reorders rows by its key;
        # the shift()s below rely on (agedays, internal_id) order within
        # this per-(subjid, param) group.
        df <- df[order(agedays, internal_id),]
        df[, mindiff_prior := shift(mindiff, n = 1L, type = "lag")]
        df[, maxdiff_prior := shift(maxdiff, n = 1L, type = "lag")]
      } else { # head circumference
        # WHO HC velocity columns (whoinc.2.hc, max.whoinc.2.hc, etc.) were pre-merged
        # before the while loop. Here we compute only per-iteration logic (depends on d_agedays).

        # 17J: Interval selection - HC starts at 2-month (no 1-month interval).
        df[, whoinc.age.hc := NA_integer_]
        df[!is.na(d_agedays) & d_agedays >= 46  & d_agedays < 76,  whoinc.age.hc := 2L]
        df[!is.na(d_agedays) & d_agedays >= 76  & d_agedays < 107, whoinc.age.hc := 3L]
        df[!is.na(d_agedays) & d_agedays >= 107 & d_agedays < 153, whoinc.age.hc := 4L]
        df[!is.na(d_agedays) & d_agedays >= 153 & d_agedays < 200, whoinc.age.hc := 6L]

        # 17K: Extract interval-specific WHO mindiff/maxdiff using renamed columns
        df[, who_mindiff_hc := fcase(
          whoinc.age.hc == 2L, whoinc.2.hc,
          whoinc.age.hc == 3L, whoinc.3.hc,
          whoinc.age.hc == 4L, whoinc.4.hc,
          whoinc.age.hc == 6L, whoinc.6.hc,
          default = NA_real_)]
        df[, who_maxdiff_hc := fcase(
          whoinc.age.hc == 2L, max.whoinc.2.hc,
          whoinc.age.hc == 3L, max.whoinc.3.hc,
          whoinc.age.hc == 4L, max.whoinc.4.hc,
          whoinc.age.hc == 6L, max.whoinc.6.hc,
          default = NA_real_)]

        # 17L: Scale by actual gap vs. reference interval length (same logic as HT)
        df[!is.na(d_agedays) & !is.na(whoinc.age.hc) &
             d_agedays < whoinc.age.hc * 30.4375,
           who_mindiff_hc := who_mindiff_hc * d_agedays / (whoinc.age.hc * 30.4375)]
        df[!is.na(d_agedays) & !is.na(whoinc.age.hc) &
             d_agedays > whoinc.age.hc * 30.4375,
           who_maxdiff_hc := who_maxdiff_hc * d_agedays / (whoinc.age.hc * 30.4375)]

        # 17M: Apply HC tolerance +/-1.5 cm whenever WHO data exists.
        df[!is.na(who_mindiff_hc),
           `:=`(who_mindiff_hc = who_mindiff_hc * .5 - 1.5,
                who_maxdiff_hc = who_maxdiff_hc * 2 + 1.5)]

        df[, mindiff := who_mindiff_hc]
        df[, maxdiff := who_maxdiff_hc]
        # Fallback: HC > 24 months or no WHO data -> fixed -1.5.
        df[is.na(mindiff), mindiff := -1.5]

        # 17N: Sort and lag thresholds for pairwise violation check.
        # internal_id breaks ties for the rare same-ageday case (none
        # reach Step 17 because SDEs are resolved upstream, but the sort
        # is deterministic for free).
        df <- df[order(agedays, internal_id),]
        df[, mindiff_prior := shift(mindiff, n = 1L, type = "lag")]
        df[, maxdiff_prior := shift(maxdiff, n = 1L, type = "lag")]
      }

      # 17O: generate ewma
      df[, (ewma.fields) := as.double(NaN)]

      # Calculate exponent based on max age gap to nearest neighbor
      tmp <- data.frame(
        "before" = abs(df$agedays - c(NA, df$agedays[1:(nrow(df)-1)])),
        "after" = abs(df$agedays - c(df$agedays[2:(nrow(df))], NA))
      )
      maxdiff_e <- pmax(tmp$before, tmp$after, na.rm = TRUE)
      exp_vals <- rep(-1.5, nrow(tmp))
      maxdiff_years <- maxdiff_e / 365.25
      exp_vals[maxdiff_years > 1 & maxdiff_years < 3] <-
        -1.5 - (maxdiff_years[maxdiff_years > 1 & maxdiff_years < 3] - 1)
      exp_vals[maxdiff_years >= 3] <- -3.5
      df[, exp_vals := exp_vals]

      # calculate ewma
      df[, (ewma.fields) := ewma(agedays, tbc.sd, exp_vals, TRUE, window = ewma_window)]

      # calculate dewma
      df[, `:=`(
        dewma.all = tbc.sd - ewma.all,
        dewma.before = tbc.sd - ewma.before,
        dewma.after = tbc.sd - ewma.after
      )]

      # add differences for convenience
      df[, diff_prev := v-shift(v, n = 1L, type = "lag")]
      df[, diff_next := shift(v, n = 1L, type = "lead")-v]



      if (nrow(df) > 2){
        # 17P/R: identify pairs and calculate exclusions
        df[, pair := diff_prev < mindiff_prior |
             diff_next < mindiff |
             diff_prev > maxdiff_prior |
             diff_next > maxdiff
        ]
        df[is.na(pair), pair := FALSE]
        # Reset tie-breaker flags each iteration (they persist from prior)
        df[, bef.g.aftm1 := NA]
        df[, aft.g.aftm1 := NA]
        df[(pair & shift(pair, n = 1L, type = "lag")) & abs(dewma.before) > shift(abs(dewma.after), n = 1L, type = "lag"),
           bef.g.aftm1 := TRUE]
        df[(pair & shift(pair, n = 1L, type = "lead")) & abs(dewma.after) > shift(abs(dewma.before), n = 1L, type = "lead"),
           aft.g.aftm1 := TRUE]

        # Q
        df[, val_excl := exclude]
        df[diff_prev < mindiff_prior & bef.g.aftm1, val_excl := .child_exc(param, "Abs-Diff")]
        df[diff_next < mindiff & aft.g.aftm1, val_excl := .child_exc(param, "Abs-Diff")]
        df[diff_prev > maxdiff_prior & bef.g.aftm1, val_excl := .child_exc(param, "Abs-Diff")]
        df[diff_next > maxdiff & aft.g.aftm1, val_excl := .child_exc(param, "Abs-Diff")]
        df[diff_prev < mindiff_prior & bef.g.aftm1, val_excl_code := "1"]
        df[diff_next < mindiff & aft.g.aftm1, val_excl_code := "2"]
        df[diff_prev > maxdiff_prior & bef.g.aftm1, val_excl_code := "3"]
        df[diff_next > maxdiff & aft.g.aftm1, val_excl_code := "4"]
      }else { # only 2 values
        # 17Q/R -- exclusions for pairs. `>=` (rather than strict `>`) lets
        # tied abs(tbc.sd) flag both rows so the internal_id tiebreaker
        # below picks one; with strict `>` a tied pair would escape Step 17
        # entirely despite the raw-diff violation.
        df[, val_excl := exclude]
        df[diff_prev < mindiff_prior & abs(tbc.sd) >= shift(abs(tbc.sd), n = 1L, type = "lag"),
           val_excl := .child_exc(param, "Abs-Diff")]
        df[diff_next < mindiff & abs(tbc.sd) >= shift(abs(tbc.sd), n = 1L, type = "lead"),
           val_excl := .child_exc(param, "Abs-Diff")]
        df[diff_prev > maxdiff_prior & abs(tbc.sd) >= shift(abs(tbc.sd), n = 1L, type = "lag"),
           val_excl := .child_exc(param, "Abs-Diff")]
        df[diff_next > maxdiff & abs(tbc.sd) >= shift(abs(tbc.sd), n = 1L, type = "lead"),
           val_excl := .child_exc(param, "Abs-Diff")]
        df[diff_prev < mindiff_prior & abs(tbc.sd) >= shift(abs(tbc.sd), n = 1L, type = "lag"),
           val_excl_code := "5"]
        df[diff_next < mindiff & abs(tbc.sd) >= shift(abs(tbc.sd), n = 1L, type = "lead"),
           val_excl_code := "6"]
        df[diff_prev > maxdiff_prior & abs(tbc.sd) >= shift(abs(tbc.sd), n = 1L, type = "lag"),
           val_excl_code := "7"]
        df[diff_next > maxdiff & abs(tbc.sd) >= shift(abs(tbc.sd), n = 1L, type = "lead"),
           val_excl_code := "8"]
      }

      # figure out if any of the exclusions hit
      count_exclude <- sum(df$val_excl != "Include")

      if (count_exclude > 0){
        if (nrow(df) > 2){
          # Pick the appropriate dewma side based on which neighbor the
          # violation was against - codes 1/3 flagged the diff-from-prior
          # side, so score by dewma.before (the EWMA excluding the prior
          # neighbor); codes 2/4 flagged diff-to-next, so score by
          # dewma.after (excluding the next neighbor).
          df[, absval := NA_real_]
          df[val_excl_code %in% c("1", "3"), absval := abs(dewma.before)]
          df[val_excl_code %in% c("2", "4"), absval := abs(dewma.after)]
        } else {
          df[, absval := abs(tbc.sd)]
        }

        # internal_id breaks ties on absval; order() is deterministic where
        # which.max is not. Lowest internal_id wins among tied candidates.
        candidates <- df[val_excl != "Include"]
        ord <- order(-candidates$absval, candidates$internal_id)
        idx <- candidates$index[ord[1]]

        exclude_all[ind_all == idx] <- df[index == idx, val_excl]

        # Continue iterating - after excluding 1 candidate, new violations
        # may emerge (the new neighbors, and the new d_agedays values,
        # produce fresh pair checks).
        testing <- TRUE
        df <- df[index != idx, ]
      } else {
        testing <- FALSE
      }
    }

    return(exclude_all)
  })(copy(.SD)), by = .(subjid, param),
  .SDcols = c('index', 'id', 'internal_id', 'agedays', 'sex', 'param', 'v', 'tbc.sd', 'exclude')]

  data.df[, sp_key := NULL]

  # 19: 1 or 2 measurements ----
  if (!quietly)
    message(sprintf(
      "[%s] Exclude 1 or 2 measurements...",
      Sys.time()
    ))

  # Sort so per-group df[1] is the earlier measurement and df[2] is the
  # later measurement in the 2-row pair case. internal_id is a
  # deterministic tiebreaker against a rare same-ageday case that does
  # not normally reach Step 19 (SDEs are resolved upstream).
  data.df <- data.df[order(subjid, param, agedays, internal_id),]

  # valid_set gates Step 19 to Include rows whose (subjid, param) has
  # 1 or 2 remaining Include measurements. Counting only Include rows
  # means subject-params whose excluded measurements leave them with 1
  # or 2 Includes still flow into this step.
  include_df <- data.df[exclude == "Include"]
  sp_counts <- include_df[, .(sp_n = .N), by = .(subjid, param)]
  only_single_pairs <- sp_counts[data.df, on = .(subjid, param), fifelse(is.na(sp_n), FALSE, sp_n <= 2L)]
  valid_set <- .child_valid(data.df, include.temporary.extraneous = FALSE) &
    only_single_pairs

  # qm_detail: Step 19 eligibility setup. Pair (AT-RISK) = valid rows in
  # subject-params with exactly 2 remaining Includes (both obs in the
  # denominator). Single eligibility (entry singletons + pair-demoted survivors)
  # is finished after the Step 19 closure, so snapshot the entry Include count
  # and valid set here into temp columns.
  if (qm_detail) {
    qm_sp_n_19 <- sp_counts[data.df, on = .(subjid, param),
                            fifelse(is.na(sp_n), 0L, sp_n)]
    data.df[, qm_elig_c_pair := valid_set & (qm_sp_n_19 == 2L)]
    data.df[, .qm_valid_19 := valid_set]
    data.df[, .qm_sp_n_19 := qm_sp_n_19]
  }

  # DOP snapshot: freeze the set of Include rows (with z-scores) used for
  # cross-parameter comparisons before the per-(subjid, param) closure
  # runs. data.df is modified in place during the closure, so without
  # the snapshot the DOP lookup for the second-processed param would
  # miss rows just excluded by the first-processed param - making the
  # result depend on by-group processing order.
  dop_snapshot <- data.df[exclude == "Include", .(subjid, param, agedays, tbc.sd, ctbc.sd)]
  setkey(dop_snapshot, subjid, param, agedays)

  data.df <- data.df[valid_set, exclude := (function(df) {
    # ind_all holds the original row-order indices used to write
    # exclusions back to the correct position in the return vector;
    # exclude_all is the vector returned from the closure and refreshed
    # after each exclusion rule below.
    ind_all <- copy(df$index)
    exclude_all <- copy(df$exclude)

    # valid_set already filtered to subject-params with <= 2 remaining
    # Includes, so df has 1 or 2 rows.
    is_single <- nrow(df) == 1

    # Look up all DOP Include rows for this subject. nomatch = NULL
    # returns 0 rows when the DOP param has no Includes for this
    # subject, sending control to the else branch below.
    dop <- dop_snapshot[.(df$subjid[1], get_dop(df$param[1])), nomatch = NULL]

    # comp_diff quantifies disagreement with DOP, per row: same-day DOP
    # tbc.sd if one exists, otherwise median DOP tbc.sd for this
    # subject. NA when DOP has no Include data for this subject.
    if (nrow(dop) > 0){
      for (i in seq_len(nrow(df))){
        comp_val <-
          if (df$agedays[i] %in% dop$agedays){
            abs(dop[dop$agedays == df$agedays[i], tbc.sd] - df[i, tbc.sd])
          } else {
            abs(median(dop$tbc.sd) - df[i, tbc.sd])
          }

        df[i, comp_diff := comp_val]
      }
    } else {
      df[, comp_diff := rep(NA, nrow(df))]
    }

    # Pair case: compute z-score differences and the time gap between
    # the two measurements. For non-potcorr subjects ctbc.sd equals
    # tbc.sd, so diff_ctbc.sd equals diff_tbc.sd; the separate
    # corrected check below matters only for potcorr subjects.
    if (!is_single){
      diff_tbc.sd <- df[2, tbc.sd] - df[1, tbc.sd]
      diff_ctbc.sd <- df[2, ctbc.sd] - df[1, ctbc.sd]
      diff_agedays <- df[2, agedays] - df[1, agedays]

      # Pick which of the two rows gets excluded if the pair rule below
      # fires. Primary criterion is larger comp_diff (worse DOP
      # disagreement). Fallback when both comp_diff are NA (no DOP data
      # for this subject) is larger abs(tbc.sd). internal_id breaks
      # ties - the row with the lowest internal_id is excluded.
      max_ind <- if (!all(is.na(df$comp_diff))){
        ord <- order(-df$comp_diff, df$internal_id)
        ord[1]
      } else {
        ord <- order(-abs(df$tbc.sd), df$internal_id)
        ord[1]
      }

      # Pair exclusion rule. Gaps >= 1 year allow more natural z-score
      # change, so the threshold is higher (> 4); gaps < 1 year use a
      # stricter threshold (> 2.5) because less legitimate change is
      # expected. The corrected check (|diff_ctbc.sd|) must also exceed
      # the same threshold (or be NA, which holds for non-potcorr
      # subjects) to avoid excluding legitimate preterm catch-up.
      # isTRUE() makes the condition NA-safe when z-scores are NA.
      if (isTRUE(abs(diff_tbc.sd) > 4 &
          (abs(diff_ctbc.sd) > 4 | is.na(diff_ctbc.sd)) &
          diff_agedays >= 365.25)){
        df[max_ind, exclude := .child_exc(param, "Pair")]
      } else if (isTRUE(abs(diff_tbc.sd) > 2.5 &
                 (abs(diff_ctbc.sd) > 2.5 | is.na(diff_ctbc.sd)) &
                 diff_agedays < 365.25)){
        df[max_ind, exclude := .child_exc(param, "Pair")]
      }

      # Refresh exclude_all so the return vector reflects the pair
      # decision even if the single rule below does not fire.
      exclude_all <- df$exclude

      # If one row was excluded as Pair, the remaining Include row
      # drops into the single check below.
      df <- df[exclude == "Include",]
    }

    if (nrow(df) == 1){
      # Single exclusion rule. Two branches: with DOP data, require
      # both an extreme tbc.sd (> 3) and strong DOP disagreement
      # (comp_diff > 5); without DOP data (comp_diff NA), require a
      # more extreme tbc.sd (> 5) alone. isTRUE() makes the condition
      # NA-safe.
      one_meas_cond <- isTRUE(
        (abs(df$tbc.sd) > 3 & !is.na(df$comp_diff) & df$comp_diff > 5) |
        (abs(df$tbc.sd) > 5 & is.na(df$comp_diff))
      )
      if (one_meas_cond) {
        df[, exclude := .child_exc(param, "Single")]
        # Write the single exclusion into the correct position in
        # exclude_all (keyed by index) so any pair exclusion already
        # recorded above is preserved.
        exclude_all[ind_all == df$index] <- .child_exc(df$param, "Single")
      }
    }

    return(exclude_all)
  })(copy(.SD)), by = .(subjid, param),
  .SDcols = c('index', 'internal_id', 'agedays', 'subjid', 'param', 'tbc.sd', 'ctbc.sd', 'exclude')]

  # qm_detail: finish Single eligibility (Child Step 19). A value reached the
  # single-value rule if its subject-param entered Step 19 with 1 Include, OR
  # entered with 2 Includes and the Pair rule fired (demoting the survivor into
  # the single check). Derived post-closure from the entry snapshot + the Pair
  # exclusions the closure just wrote.
  if (qm_detail) {
    data.df[, .qm_pair_fired := any(exclude == "Exclude-C-Pair"), by = .(subjid, param)]
    data.df[, qm_elig_c_single := .qm_valid_19 &
              (.qm_sp_n_19 == 1L |
               (.qm_sp_n_19 == 2L & .qm_pair_fired & exclude != "Exclude-C-Pair"))]
    data.df[, c(".qm_valid_19", ".qm_sp_n_19", ".qm_pair_fired") := NULL]
  }

  # 21: error load ----

  if (!quietly)
    message(sprintf(
      "[%s] Exclude error load...",
      Sys.time()
    ))

  # Error-load ratio: n_errors / (n_errors + n_includes) per (subjid, param).
  # non_error_codes are the exclusion codes that do NOT count as measurement
  # errors and are removed from both the numerator and the denominator:
  #   Exclude-C-Identical / Exclude-C-Extraneous: SDE housekeeping
  #     (same-day duplicates resolved in Child Step 13, not evidence of a bad
  #     measurement).
  #   Exclude-C-CF: detected carried-forward that was not rescued; a
  #     data-entry artifact rather than a quality signal. (Rescued CFs are
  #     "Include" here, tracked via the cf_rescued column, so they count as
  #     Includes in the denominator.)
  #   Exclude-Missing: row had no measurement; cannot be a measurement error.
  #   Exclude-Not-Cleaned: HC row beyond cleaning age (> 5 * 365.25 days);
  #     out of scope for the child algorithm entirely.
  non_error_codes <- c("Exclude-C-Identical",
                       "Exclude-C-Extraneous",
                       "Exclude-C-CF",
                       "Exclude-Missing",
                       "Exclude-Not-Cleaned")

  data.df[,
          c("err_ratio", "n_errors") := {
            # Count errors (not Include and not in non_error_codes)
            n_errors <- sum(!exclude %in% c("Include", non_error_codes))
            n_includes <- sum(exclude == "Include")
            denom <- n_errors + n_includes
            err_ratio <- if (denom > 0) n_errors / denom else 0
            list(err_ratio, n_errors)
          },
          by = c("subjid", "param")]

  # qm_detail: eligibility for the Unevaluable-Trajectory / Error Load step
  # (Child Step 21). AT-RISK denominator -- Include rows in subject-params with
  # at least error.load.mincount errors (the only ones that can trip). Captured
  # before the exclusion below, using the n_errors just computed.
  if (qm_detail) {
    data.df[, qm_elig_c_trajuneval := (exclude == "Include") & (n_errors >= error.load.mincount)]
  }

  # error.load.mincount guards against tripping the step on a subject with
  # a small number of errors (default: need at least 2 errors before the
  # ratio is considered).
  data.df[err_ratio > error.load.threshold & n_errors >= error.load.mincount & exclude == "Include",
          exclude := .child_exc(param, "Unevaluable-Trajectory")]

  # Cleanup
  data.df[, c("err_ratio", "n_errors") := NULL]


  # end ----

  if (!quietly)
    message(sprintf("[%s] Completed parallel group #%s...", Sys.time(), data.df$batch[1]))
  if (parallel & !is.na(log.path))
    sink()

  # Assemble return columns: essentials + z-scores + optional cf_detail columns.
  # tbc.sd (uncorrected) and ctbc.sd (corrected) are both returned; for
  # corrected subjects both are meaningful.
  return_cols <- c("id", "line", "exclude", "param", "cf_rescued")

  # Add z-score columns if they exist
  zscore_cols <- c("sd.orig_who", "sd.orig_cdc", "sd.orig", "tbc.sd", "ctbc.sd")
  for (col in zscore_cols) {
    if (col %in% names(data.df)) {
      return_cols <- c(return_cols, col)
    }
  }

  # Add cf_detail columns if requested
  if (cf_detail) {
    for (col in c("cf_status", "cf_deltaz", "cf_nextz",
                  "imperial", "cf_dage_orig", "cf_dage_next")) {
      if (col %in% names(data.df)) {
        return_cols <- c(return_cols, col)
      }
    }
  }

  # Add qm_detail per-step eligibility flags if requested
  if (qm_detail) {
    for (col in c("qm_elig_c_cf", "qm_elig_c_hardlimit", "qm_elig_c_eviltwins",
                  "qm_elig_c_trajextreme", "qm_elig_extraneous", "qm_elig_c_traj",
                  "qm_elig_c_absdiff", "qm_elig_c_pair", "qm_elig_c_single",
                  "qm_elig_c_trajuneval")) {
      if (col %in% names(data.df)) {
        return_cols <- c(return_cols, col)
      }
    }
  }

  return(data.df[, ..return_cols])
}

# Supporting pediatric growthcleanr functions
# Supporting functions for pediatric piece of algorithm

#' Identify rows currently eligible for child-algorithm processing.
#'
#' Returns a logical vector the same length as the input, marking which
#' rows are currently "valid" for a given step of `cleanchild()`. A row
#' is valid if its exclusion code does not start with `"Exclude-"`, so
#' `"Include"` rows pass unconditionally and every `Exclude-*` code is
#' filtered out by default (including `Exclude-Missing`,
#' `Exclude-Not-Cleaned`, and all algorithm-assigned `Exclude-C-*`
#' codes).
#'
#' Three optional flags add back specific soft-excluded categories so a
#' step can opt them in without changing the default for the rest of
#' the algorithm. `include.temporary.extraneous` also admits rows
#' flagged `"Exclude-C-Temp-Same-Day"` by Child Step 5;
#' `include.extraneous` also admits rows flagged `"Exclude-C-Extraneous"`
#' by Child Step 13; `include.carryforward` also admits rows flagged
#' `"Exclude-C-CF"` by Child Step 6. The flags are additive.
#'
#' Accepts either a data.frame / data.table (in which case `df$exclude`
#' is used) or a plain character / factor vector of exclusion codes.
#' The exclusion column is coerced to character so factor inputs and
#' character inputs match identically under `^Exclude` pattern
#' matching.
#'
#' @param df data.frame / data.table containing an `exclude` column, or
#'   a character or factor vector of exclusion codes.
#' @param include.temporary.extraneous if TRUE, also keep rows with
#'   `exclude == "Exclude-C-Temp-Same-Day"`. Defaults to FALSE.
#' @param include.extraneous if TRUE, also keep rows with
#'   `exclude == "Exclude-C-Extraneous"`. Defaults to FALSE.
#' @param include.carryforward if TRUE, also keep rows with
#'   `exclude == "Exclude-C-CF"`. Defaults to FALSE.
#'
#' @return Logical vector the same length as the input, TRUE for rows
#'   that the calling step should process.
#'
#' @keywords internal
#' @noRd
.child_valid <- function(df,
                  include.temporary.extraneous = FALSE,
                  include.extraneous = FALSE,
                  include.carryforward = FALSE) {
  exclude <- if (is.data.frame(df)) df$exclude else df
  exclude <- as.character(exclude)

  # Base set: non-excluded rows. All exclusion codes start with "Exclude-"
  # (including Exclude-Missing and Exclude-Not-Cleaned), so this single
  # grepl catches everything. "Include" does not match, so included rows pass.
  keep <- !grepl("^Exclude", exclude)

  if (include.temporary.extraneous)
    keep <- keep | exclude == "Exclude-C-Temp-Same-Day"
  if (include.extraneous)
    keep <- keep | exclude == "Exclude-C-Extraneous"
  if (include.carryforward)
    keep <- keep | exclude == "Exclude-C-CF"

  return(keep)
}

# ---------------------------------------------------------------------------
# unit_error_range: informational output column (see ET revision spec, sec. 6)
# ---------------------------------------------------------------------------
# Direction-agnostic TRUE/FALSE flag: a value whose magnitude is consistent with
# a metric/imperial unit-conversion error AND that is not itself within the
# plausible [zn3, zp3] band. Purely informational -- it never interacts with the
# exclude column. Computed at the end of cleangrowth() for rows gc actually
# cleans; Exclude-Missing / Exclude-Not-Cleaned rows (and any row lacking a z3
# lookup) get FALSE.

#
# The adult CSD reference + z helpers (.adult_csd_ref, .adult_csd_lookup,
# .adult_z3, .adult_csd_z) and the shared band-logic helper
# (.unit_error_range_flag) live in adult_support.R, co-located with the adult
# Evil Twins step that also consumes them (and so the standalone adult test
# harness, which sources only the adult files, can reach them). The child z3
# lookup (.child_z3) and the child+adult assembler (.compute_unit_error_range)
# stay here; they reach the adult-file helpers via the package namespace.

#' Child zn3/zp3 lookup (measurement at recentered CSD z = -3 / +3).
#'
#' The interface the child z3 reference table plugs into: takes param, sex,
#' agedays vectors and returns the recentered (uncorrected) z = -3 / +3
#' measurements per row, looked up from `child_z3_reference.csv.gz`. Returns NA
#' where no lookup row matches (e.g. HEADCM agedays beyond the 3y cleaning
#' range). Inputs are equal-length vectors; output preserves input order.
#' @keywords internal
#' @noRd
.child_z3 <- function(param, sex, agedays, ref.data.path = "") {
  zn3 <- zp3 <- .ord <- NULL
  z3_path <- ifelse(
    ref.data.path == "",
    system.file(file.path("extdata", "child_z3_reference.csv.gz"),
                package = "growthcleanr"),
    file.path(ref.data.path, "child_z3_reference.csv.gz")
  )
  tbl <- data.table::fread(z3_path)
  q <- data.table::data.table(param = as.character(param),
                              sex = as.integer(sex),
                              agedays = as.integer(agedays),
                              .ord = seq_along(param))
  q <- merge(q, tbl, by = c("param", "sex", "agedays"),
             all.x = TRUE, sort = FALSE)
  data.table::setorder(q, .ord)
  list(zn3 = q$zn3, zp3 = q$zp3)
}

#' Assemble the unit_error_range column for the full (child + adult) output.
#'
#' Dispatches the zn3/zp3 lookup by row type: child rows (agedays <
#' adult_cutpoint*365.25) use the child z3 table; adult rows use the adult CSD
#' reference; Exclude-Missing / Exclude-Not-Cleaned rows are not evaluated
#' (FALSE). `dt` must carry param, sex, agedays, v (metric measurement), exclude.
#' @keywords internal
#' @noRd
# Exclusion codes whose rows growthcleanr does NOT evaluate for the unit-error
# range (preprocessing set-asides plus HC cleaned past 5y). Listed in both the
# internal form (Exclude-Not-Cleaned) and its Summary-mapped form
# (Exclude-HC-Out-of-Range) so the set matches whether it is applied before the
# summary mapping (inside .compute_unit_error_range) or after it (the systematic
# unit-error warning denominator). Single source of truth for both call sites --
# keep them from drifting.
.gc_unit_error_noneval <- c("Exclude-Missing", "Exclude-Not-Cleaned",
                            "Exclude-Not-GC-Param", "Exclude-Missing-Info",
                            "Exclude-HC-Out-of-Range")

.compute_unit_error_range <- function(dt, adult_cutpoint, ref.data.path = "") {
  n <- nrow(dt)
  zn3 <- rep(NA_real_, n)
  zp3 <- rep(NA_real_, n)

  excl <- as.character(dt$exclude)
  # Internal names plus their Summary-level equivalents (partial-run cached rows
  # arrive already remapped) and the set-aside non-gc-param rows.
  evaluable <- !(excl %in% .gc_unit_error_noneval) &
    !is.na(dt$agedays)
  is_adult <- evaluable & dt$agedays >= adult_cutpoint * 365.25

  cidx <- which(evaluable & !is_adult)
  if (length(cidx)) {
    cz <- .child_z3(dt$param[cidx], dt$sex[cidx], dt$agedays[cidx], ref.data.path)
    zn3[cidx] <- cz$zn3
    zp3[cidx] <- cz$zp3
  }
  aidx <- which(is_adult)
  if (length(aidx)) {
    az <- .adult_z3(dt$param[aidx], dt$sex[aidx])
    zn3[aidx] <- az$zn3
    zp3[aidx] <- az$zp3
  }

  .unit_error_range_flag(dt$param, dt$v, zn3, zp3)
}

