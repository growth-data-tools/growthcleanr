# Adult growthcleanr algorithm
# Internal function called by cleangrowth(); support functions in adult_support.R
# Supports both independent and linked repval_handling modes

#' Clean height and weight data for adults
#'
#' @param df data.table with columns: id, internal_id, subjid, sex, agedays, param, measurement
#' @param permissiveness Permissiveness level: "loosest", "looser" (default),
#'   "tighter", "tightest". Sets defaults for all settings below.
#'   Explicit setting values override the preset.
#' @param overall_ht_min,overall_ht_max PIV height limits in cm (NULL = preset).
#' @param overall_wt_min,overall_wt_max PIV weight limits in kg (NULL = preset).
#' @param overall_bmi_min,overall_bmi_max PIV BMI limits in kg/m2 (NULL = preset).
#' @param single_ht_min_bmi,single_ht_max_bmi 1D height limits when BMI available.
#' @param single_wt_min_bmi,single_wt_max_bmi 1D weight limits when BMI available.
#' @param single_ht_min_nobmi,single_ht_max_nobmi 1D height limits without BMI.
#' @param single_wt_min_nobmi,single_wt_max_nobmi 1D weight limits without BMI.
#' @param single_bmi_min,single_bmi_max 1D BMI limits.
#' @param repval_handling "independent" or "linked" (NULL = preset).
#' @param ht_band Height band in inches for Step 10H (NULL = preset).
#' @param allow_ht_loss Allow height-loss rescue in 2D/3+D steps (NULL = preset).
#' @param allow_ht_gain Allow height-gain rescue for young adults (NULL = preset).
#' @param wtallow_formula Formula for wtallow: "piecewise", "piecewise-lower",
#'   "allofus15", or path to custom CSV (NULL = preset).
#' @param error_load_threshold Error load ratio threshold (NULL = preset).
#' @param mod_ewma_f Directional factor for Moderate EWMA (NULL = preset).
#' @param perclimit_low,perclimit_mid,perclimit_high Percentage criterion limits
#'   for wt <=45, 45-80, >80 kg (NULL = preset). 0 = disabled.
#' @param scale_max_lbs Physical scale upper limit in pounds (default Inf).
#' @param ewma_window Position-based EWMA window (default 15).
#' @param include_bin_result Include binary Include/Exclude column (default TRUE).
#' @param include_extraneous Include same-day event flag column (default FALSE).
#' @param include_ht_groups Include loss_groups and gain_groups columns (default FALSE).
#' @param quietly Accepted for interface compatibility (default FALSE). The
#'   adult per-step messages are gated on \code{verbose}, not on this argument.
#' @param verbose When TRUE, prints granular per-step diagnostics. Defaults to
#'   FALSE. (When called from \code{cleangrowth()}, the standard progress
#'   milestones are emitted by the wrapper, not here.)
#'
#' @return df with additional columns: result, mean_ht, and optionally
#'   bin_result (default), extraneous, loss_groups, gain_groups
#'
#' @keywords internal
#' @noRd
cleanadult <- function(df,
                       permissiveness = "looser",
                       # PIV (overall) limits -- NULL = use permissiveness preset
                       overall_ht_min = NULL,
                       overall_ht_max = NULL,
                       overall_wt_min = NULL,
                       overall_wt_max = NULL,
                       overall_bmi_min = NULL,
                       overall_bmi_max = NULL,
                       # 1D (single) limits -- NULL = use permissiveness preset
                       single_ht_min_bmi = NULL,
                       single_ht_max_bmi = NULL,
                       single_wt_min_bmi = NULL,
                       single_wt_max_bmi = NULL,
                       single_ht_min_nobmi = NULL,
                       single_ht_max_nobmi = NULL,
                       single_wt_min_nobmi = NULL,
                       single_wt_max_nobmi = NULL,
                       single_bmi_min = NULL,
                       single_bmi_max = NULL,
                       # Algorithm parameters -- NULL = use permissiveness preset
                       repval_handling = NULL,
                       ht_band = NULL,
                       allow_ht_loss = NULL,
                       allow_ht_gain = NULL,
                       wtallow_formula = NULL,
                       error_load_threshold = NULL,
                       mod_ewma_f = NULL,
                       perclimit_low = NULL,
                       perclimit_mid = NULL,
                       perclimit_high = NULL,
                       # Non-permissiveness parameters
                       scale_max_lbs = Inf,
                       ewma_window = 15,
                       # Output options
                       include_bin_result = TRUE,
                       include_extraneous = FALSE,
                       include_ht_groups = FALSE,
                       quietly = FALSE,
                       verbose = FALSE) {

  # The per-step messages below are gated on `!quietly` but are development-level
  # detail belonging to the `verbose` tier, not the standard milestone view
  # (which is emitted by cleangrowth()). Remap the local `quietly` so they print
  # only when verbose = TRUE. `quietly` is used nowhere else in this function, so
  # this local override is safe.
  quietly <- !isTRUE(verbose)

  # avoid "no visible binding for global variable" notes in R CMD check
  ageyears <- internal_id <- result <- mean_ht <- loss_groups <- NULL
  gain_groups <- extraneous <- meas_m <- age_days <- bin_result <- NULL
  param <- measurement <- i.keep <- i.mean_ht <- i.loss_grp <- NULL
  i.gain_grp <- i.extraneous <- NULL

  # =============================================================================
  # RESOLVE PERMISSIVENESS
  # =============================================================================

  p <- resolve_permissiveness(
    permissiveness,
    overall_ht_min = overall_ht_min, overall_ht_max = overall_ht_max,
    overall_wt_min = overall_wt_min, overall_wt_max = overall_wt_max,
    overall_bmi_min = overall_bmi_min, overall_bmi_max = overall_bmi_max,
    single_ht_min_bmi = single_ht_min_bmi,
    single_ht_max_bmi = single_ht_max_bmi,
    single_wt_min_bmi = single_wt_min_bmi,
    single_wt_max_bmi = single_wt_max_bmi,
    single_ht_min_nobmi = single_ht_min_nobmi,
    single_ht_max_nobmi = single_ht_max_nobmi,
    single_wt_min_nobmi = single_wt_min_nobmi,
    single_wt_max_nobmi = single_wt_max_nobmi,
    single_bmi_min = single_bmi_min, single_bmi_max = single_bmi_max,
    repval_handling = repval_handling, ht_band = ht_band,
    allow_ht_loss = allow_ht_loss, allow_ht_gain = allow_ht_gain,
    wtallow_formula = wtallow_formula,
    error_load_threshold = error_load_threshold,
    mod_ewma_f = mod_ewma_f,
    perclimit_low = perclimit_low, perclimit_mid = perclimit_mid,
    perclimit_high = perclimit_high
  )
  # Unpack resolved values into local variables
  for (.nm in names(p)) assign(.nm, p[[.nm]])

  # =============================================================================
  # PARAMETER VALIDATION
  # =============================================================================

  # Validate wtallow_formula
  builtin_formulas <- c("piecewise", "piecewise-lower", "allofus15")
  if (!wtallow_formula %in% builtin_formulas && !file.exists(wtallow_formula)) {
    stop(paste0("wtallow_formula '", wtallow_formula, "' is not a built-in formula (",
                paste(builtin_formulas, collapse = ", "),
                ") and file not found."))
  }

  # =============================================================================
  # PARAMETERS
  # =============================================================================

  # PIV limits (Step 1): absolute population implausible value thresholds
  piv_df <- data.frame(
    "low" = c(overall_ht_min, overall_wt_min),
    "high" = c(overall_ht_max, overall_wt_max)
  )
  rownames(piv_df) <- c("height", "weight")

  # 1D limits (Step 13) -- packaged from resolved values
  params_1d <- list(
    ht_min_bmi = single_ht_min_bmi, ht_max_bmi = single_ht_max_bmi,
    wt_min_bmi = single_wt_min_bmi, wt_max_bmi = single_wt_max_bmi,
    ht_min_nobmi = single_ht_min_nobmi,
    ht_max_nobmi = single_ht_max_nobmi,
    wt_min_nobmi = single_wt_min_nobmi,
    wt_max_nobmi = single_wt_max_nobmi,
    bmi_min = single_bmi_min, bmi_max = single_bmi_max
  )

  # =============================================================================
  # DATA STRUCTURE INITIALIZATION
  # =============================================================================

  if (!is.data.table(df)) {
    df <- as.data.table(df)
  }

  # Standardize column names
  if ("age_days" %in% names(df) && !"agedays" %in% names(df)) {
    setnames(df, "age_days", "agedays")
  }

  # Ensure required columns exist
  required_cols <- c("id", "internal_id", "subjid", "param", "agedays", "sex", "measurement")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Track which columns we add so we can remove them on output
  added_cols <- character(0)

  # Add ageyears if not present (cleangrowth() provides "age_years")
  if ("age_years" %in% names(df) && !"ageyears" %in% names(df)) {
    setnames(df, "age_years", "ageyears")
  } else if (!"ageyears" %in% names(df)) {
    df[, ageyears := agedays / 365.25]
    added_cols <- c(added_cols, "ageyears")
  }

  # internal_id is a sequential integer created by cleangrowth() for
  # deterministic sorting and tiebreaking. The user's original id is
  # preserved untouched in the id column throughout.
  # Named vectors (h_subj_keep, w_subj_keep) use as.character(internal_id)
  # for names, since R named vector keys are always character.
  df[, result := "Include"]
  df[, mean_ht := as.numeric(NA)]
  df[, loss_groups := as.numeric(NA)]
  df[, gain_groups := as.numeric(NA)]
  df[, extraneous := FALSE]

  # Measurement copies
  df[, meas_m := measurement]

  # For height: meas_m in cm
  df[param == "HEIGHTIN", meas_m := measurement * 2.54]
  # For weight: meas_m in kg
  df[param == "WEIGHTLBS", meas_m := measurement / 2.2046226]

  # Rows with a missing measurement (NA/NaN -- e.g. a param row with no value,
  # or a 0 converted to NaN upstream) cannot be cleaned and must not participate
  # in any step: left in play they leak into derived quantities (BMI, EWMA,
  # threshold comparisons) as NA and crash the if() guards that consume them
  # (e.g. eval_1d's BMI check). Set them aside as Exclude-Missing up front and
  # exclude them from the per-subject height/weight frames below. The wrapper
  # also re-applies Exclude-Missing after cleanadult() returns, so this matches
  # the reported output.
  df[is.na(meas_m), result := "Exclude-Missing"]

  # Add age_days alias for functions that expect it
  if (!"age_days" %in% names(df)) {
    df[, age_days := agedays]
    added_cols <- c(added_cols, "age_days")
  }

  if (!quietly) message("Data prepared: ", nrow(df), " observations, ", length(unique(df$subjid)), " subjects")

  # =============================================================================
  # MAIN ALGORITHM LOOP (per subject)
  # =============================================================================

  if (!quietly) message("Starting algorithm loop...")

  for (i in unique(df$subjid)) {
    slog <- df$subjid == i

    # =========================================================================
    # HEIGHT STEPS (1H, 3H, 9H, 10H, 11H)
    # =========================================================================

    h_subj_df <- copy(df[param %in% c("HEIGHTCM", "HEIGHTIN") & slog & !is.na(meas_m), ])
    h_subj_df <- h_subj_df[order(ageyears, internal_id), ]

    h_subj_keep <- rep("Include", nrow(h_subj_df))
    h_extraneous <- rep(FALSE, nrow(h_subj_df))
    names(h_subj_keep) <- names(h_extraneous) <- h_subj_df$internal_id

    # --- 1H: PIV (Population Implausible Values) ---
    # Exclude heights outside PIV limits (overall_ht_min / overall_ht_max).
    # No need to check for same-day duplicates before PIV because all
    # values outside these limits are excluded regardless.
    step <- "Exclude-A-PIV"
    if (nrow(h_subj_df) > 0) {
      criteria <- remove_piv(h_subj_df, "height", piv_df)
      h_subj_keep[criteria] <- step
      h_subj_df <- h_subj_df[!criteria, ]
    }

    # --- 3H: Temp SDE (Same-Day Extraneous) ---
    # When multiple heights on the same day, temporarily flag all but the one
    # closest to the patient's median (of all included heights). Final SDE
    # resolution happens later in Step 9H.
    if (nrow(h_subj_df) > 0) {
      h_subj_df <- temp_sde(h_subj_df)
      h_extraneous[as.character(h_subj_df$internal_id[h_subj_df$extraneous])] <- TRUE
    }

    # =========================================================================
    # WEIGHT STEPS (1W, 2W, 3W, 4W)
    # =========================================================================

    w_subj_df <- copy(df[param %in% c("WEIGHTKG", "WEIGHTLBS") & slog & !is.na(meas_m), ])
    w_subj_df <- w_subj_df[order(ageyears, internal_id), ]

    w_subj_keep <- rep("Include", nrow(w_subj_df))
    w_extraneous <- rep(FALSE, nrow(w_subj_df))
    names(w_subj_keep) <- names(w_extraneous) <- w_subj_df$internal_id

    # --- 1W: PIV (Population Implausible Values) ---
    # Exclude weights outside PIV limits (overall_wt_min / overall_wt_max).
    if (nrow(w_subj_df) > 0) {
      step <- "Exclude-A-PIV"
      criteria <- remove_piv(w_subj_df, "weight", piv_df)
      w_subj_keep[criteria] <- step
      w_subj_df <- w_subj_df[!criteria, ]
    }

    # --- 1B: BMI PIV check ---
    # For same-day ht+wt pairs surviving 1H/1W, compute BMI and exclude both sides
    # if BMI is outside [overall_bmi_min, overall_bmi_max]. No rounding tolerance
    # is added to the BMI comparison: tolerance is already embedded in the
    # measurement-level PIV checks above, and BMI is a derived value.
    # Pairing: first ht and first wt on each shared ageday (by internal_id order, already sorted).
    # Guard: only run when overall_bmi limits are wider than single_bmi limits (i.e., when
    # the check provides coverage beyond what Step 13 already handles). In practice this
    # means loosest only -- at other levels overall_bmi == single_bmi, so the check would
    # apply PIV thresholds earlier than intended, disrupting later algorithm steps.
    if (nrow(h_subj_df) > 0 && nrow(w_subj_df) > 0 &&
        (overall_bmi_min < single_bmi_min || overall_bmi_max > single_bmi_max)) {
      bmi_days <- intersect(h_subj_df$age_days, w_subj_df$age_days)
      exc_ht_bmi <- character(0)
      exc_wt_bmi <- character(0)
      for (d in bmi_days) {
        ht_day <- h_subj_df[h_subj_df$age_days == d, ]
        wt_day <- w_subj_df[w_subj_df$age_days == d, ]
        ht_val <- ht_day$meas_m[1]
        wt_val <- wt_day$meas_m[1]
        bmi_val <- wt_val / ((ht_val / 100)^2)
        if (bmi_val < overall_bmi_min || bmi_val > overall_bmi_max) {
          exc_ht_bmi <- c(exc_ht_bmi, as.character(ht_day$internal_id[1]))
          exc_wt_bmi <- c(exc_wt_bmi, as.character(wt_day$internal_id[1]))
        }
      }
      if (length(exc_ht_bmi) > 0) {
        h_subj_keep[exc_ht_bmi] <- "Exclude-A-PIV"
        h_subj_df <- h_subj_df[!h_subj_df$internal_id %in% exc_ht_bmi, ]
      }
      if (length(exc_wt_bmi) > 0) {
        w_subj_keep[exc_wt_bmi] <- "Exclude-A-PIV"
        w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% exc_wt_bmi, ]
      }
    }

    # --- 2W: Repeated Values (Weight only) ---
    # Identify groups of identical weight values (by meas_m) within a subject.
    # The first occurrence (earliest age, internal_id tiebreaker) is marked is_first_rv=TRUE;
    # subsequent identical values are marked is_rv=TRUE. Unique values have both FALSE.
    # RVs are NOT excluded here -- they are flagged for differential handling in
    # later steps (e.g., EWMA). RV identification is weight-only; height does not
    # use RV tracking.
    # Note: 78.1 and 78.1 are identical; 78.1 and 78.101 are not.
    w_subj_df <- identify_rv(w_subj_df)

    # --- 3W: Temp SDE (Same-Day Extraneous) ---
    # When multiple weights on the same day, temporarily flag all but the one
    # closest to the patient's median (of non-RV weights). Final SDE resolution
    # happens later in Step 10W. After flagging, redo RV identification in case
    # a first_rv was marked extraneous.
    if (nrow(w_subj_df) > 0) {
      w_subj_df <- temp_sde(w_subj_df, ptype = "weight")
      w_subj_df <- redo_identify_rv(w_subj_df)
      w_extraneous[as.character(w_subj_df$internal_id[w_subj_df$extraneous])] <- TRUE
    }

    # --- 4W: Scale Max (Weight Cap) ---
    # Exclude weights exactly at a physical scale maximum (e.g., 400 lbs) unless
    # EWMA shows they fit the patient's pattern. Uses firstRV (non-RV) values for
    # EWMA and adjacency checks. Requires both EWMA delta > 50 kg AND adjacency
    # diff > 50 kg (all with 0.12 rounding tolerance) to exclude. Before/after
    # EWMA use a 0.9 factor (45 kg threshold) to allow slight variation when
    # excluding a value changes the EWMA reference.
    # If subject's only distinct weight is the cap value, all weights excluded.
    # RVs of excluded cap values are also excluded.
    inc_df <- if (nrow(w_subj_df) > 0) {
      copy(w_subj_df[!w_subj_df$extraneous, ])
    } else {
      w_subj_df
    }

    if (nrow(inc_df) > 1 & scale_max_lbs < Inf) {
      # scale_max_lbs is in pounds; convert to kg for comparison
      wc_kg <- scale_max_lbs / 2.2046226
      wc_low <- round(wc_kg, 1) - .1
      wc_high <- round(wc_kg, 1) + .1

      # Check all values for weight cap (including RVs)
      is_wc_all <- check_between(inc_df$meas_m, wc_low, wc_high)

      # Use firstRV (non-RV) for distinct count, EWMA, and adjacency
      nonrv_idx <- if ("is_rv" %in% names(inc_df)) !inc_df$is_rv else rep(TRUE, nrow(inc_df))
      nonrv_df <- inc_df[nonrv_idx, ]
      is_wc_nonrv <- check_between(nonrv_df$meas_m, wc_low, wc_high)

      # Count distinct non-RV weights
      n_distinct_nonrv <- length(unique(nonrv_df$meas_m))

      # All distinct non-RV weights are weight cap -> "400 only wt"
      all_wc <- n_distinct_nonrv == 1 & all(is_wc_nonrv)

      criteria_all <- rep(FALSE, nrow(inc_df))
      criteria_nonrv <- rep(FALSE, nrow(nonrv_df))

      if (all_wc) {
        criteria_all <- rep(TRUE, nrow(inc_df))
      } else if (any(is_wc_nonrv) & !all(is_wc_nonrv) & n_distinct_nonrv > 1) {
        # EWMA on firstRV values only (using cache for consistency with 9Wb/11Wb)
        cache <- adult_ewma_cache_init(nonrv_df$age_days, nonrv_df$meas_m,
                                 ewma_window = ewma_window)
        dewma_all <- nonrv_df$meas_m - cache$ewma_all
        dewma_bef <- nonrv_df$meas_m - cache$ewma_before
        dewma_aft <- nonrv_df$meas_m - cache$ewma_after

        # Adjacency diffs on firstRV values
        prev_val <- c(NA, nonrv_df$meas_m[-nrow(nonrv_df)])
        next_val <- c(nonrv_df$meas_m[-1], NA)
        d_prev <- nonrv_df$meas_m - prev_val
        d_next <- nonrv_df$meas_m - next_val

        b <- 50
        tol <- 0.12  # rounding tolerance (0.1 kg rounding + float precision)
        # Note: adult_ewma_cache_init() never produces NA for dewma_bef/dewma_aft --
        # edge cases fall back to ewma_all, so no NA-to-Inf conversion is needed.
        dewma50p <- !is.na(dewma_all) & dewma_all > b + tol &
                    dewma_bef > (b * 0.9) + tol &
                    dewma_aft > (b * 0.9) + tol
        dewma50m <- !is.na(dewma_all) & dewma_all < -(b + tol) &
                    dewma_bef < -((b * 0.9) + tol) &
                    dewma_aft < -((b * 0.9) + tol)

        # Missing-as-infinity for adjacency edge values:
        # Positive: NA -> Inf (confirms). Negative: NA -> -Inf (confirms).
        d_prev_adj <- ifelse(is.na(d_prev), Inf, d_prev)
        d_next_adj <- ifelse(is.na(d_next), Inf, d_next)
        d50p <- d_prev_adj > b + tol & d_next_adj > b + tol
        d_prev_m <- ifelse(is.na(d_prev), -Inf, d_prev)
        d_next_m <- ifelse(is.na(d_next), -Inf, d_next)
        d50m <- d_prev_m < -(b + tol) & d_next_m < -(b + tol)

        criteria_nonrv <- is_wc_nonrv &
                          ((dewma50p & d50p) | (dewma50m & d50m))
        criteria_nonrv[is.na(criteria_nonrv)] <- FALSE
      }

      # Map criteria back -- all_wc includes extraneous values too.
      # Scale-max propagation is intentionally bidirectional: if any value at
      # the cap is excluded, ALL identical values (including extraneous and RVs)
      # get the scale-max code. This differs from EWMA RV propagation which
      # only goes firstRV -> later RVs.
      if (all_wc) {
        impl_ids <- as.character(w_subj_df$internal_id)
      } else {
        nonrv_exc_ids <- as.character(nonrv_df$internal_id[criteria_nonrv])
        exc_meas <- unique(nonrv_df$meas_m[criteria_nonrv])
        rv_exc_ids <- character(0)
        if (length(nonrv_exc_ids) > 0) {
          rv_exc_ids <- as.character(w_subj_df$internal_id[
            w_subj_df$meas_m %in% exc_meas &
            !w_subj_df$internal_id %in% nonrv_exc_ids])
        }
        impl_ids <- c(nonrv_exc_ids, rv_exc_ids)
      }

      # Set exclusion codes -- RV copies get distinct code in linked mode
      if (all_wc) {
        step <- "Exclude-A-Scale-Max-Identical"
        w_subj_keep[impl_ids] <- step
      } else if (length(nonrv_exc_ids) > 0) {
        w_subj_keep[nonrv_exc_ids] <- "Exclude-A-Scale-Max"
        if (length(rv_exc_ids) > 0) {
          if (repval_handling == "linked") {
            w_subj_keep[rv_exc_ids] <- "Exclude-A-Scale-Max-RV-Propagated"
          } else {
            w_subj_keep[rv_exc_ids] <- "Exclude-A-Scale-Max"
          }
        }
      }

      w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% impl_ids, ]

      if (length(impl_ids) > 0 & nrow(w_subj_df) > 0) {
        w_subj_df <- identify_rv(w_subj_df)
        w_subj_df <- temp_sde(w_subj_df, ptype = "weight")
        w_subj_df <- redo_identify_rv(w_subj_df)
      }
    }

    # =========================================================================
    # STEP 9Wa: EVIL TWINS
    # =========================================================================
    # Exclude inner members of an over-the-limit (OTL) weight pair using the
    # anchor rule: of four age-ordered eligible weights A, B, C, D, the inner
    # B-C jump must exceed etcap(B,C) while BOTH outer pairs (A-B, C-D) anchor
    # within 60% of their own etcap. A missing A or D disqualifies the pair, so
    # single spikes (record ends and embedded singles) cede to remove_ewma_wt().
    # Thresholds are dynamic 2-tier caps with weight scaling, plus 0.12 kg
    # rounding tolerance. RVs participate as ordinary values. Anchoring needs
    # >= 4 eligible values. One exclusion per iteration; iterates and re-anchors
    # until no OTL pair remains or < 4 values. Pre-check: skip if max-min range
    # <= smallest cap (no pair could possibly exceed any interval-specific cap).

    if (nrow(w_subj_df) > 0) {
      et_df <- copy(w_subj_df[!w_subj_df$extraneous, ])

      min_et_cap <- compute_et_limit(1, formula = wtallow_formula,
                                     uw = min(et_df$meas_m))
      if (nrow(et_df) >= 4 &
          (max(et_df$meas_m) - min(et_df$meas_m)) > (min_et_cap + 0.12)) {
        et_exc_ids <- evil_twins(et_df, wtallow_formula = wtallow_formula)
        if (length(et_exc_ids) > 0) {
          w_subj_keep[et_exc_ids] <- "Exclude-A-Evil-Twins"
          w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% et_exc_ids, ]
          if (nrow(w_subj_df) > 0) {
            w_subj_df <- identify_rv(w_subj_df)
            w_subj_df <- temp_sde(w_subj_df, ptype = "weight")
            w_subj_df <- redo_identify_rv(w_subj_df)
          }
        }
      }
    }

    # =========================================================================
    # STEP 9H: FINAL SDE HEIGHT RESOLUTION
    # =========================================================================
    # Final resolution of same-day height duplicates flagged in Step 3H.
    # Part A: Exclude identical same-day values (keep lowest internal_id).
    # Part B: For remaining non-identical SDEs, categorize subject by
    #   number of distinct days and SDE prevalence, then choose keeper
    #   using category-specific median comparisons.
    # Categories: (1) one day total, (2) >=4 days & <50% SDE,
    #   (3) 2-3 days or >=50% SDE.
    # Keeper retains its original meas_m (no averaging).
    # Tiebreaker for non-identical: highest internal_id (later measurement).

    if (nrow(h_subj_df) > 0 & any(h_subj_df$extraneous)) {
      step <- "Exclude-A-Identical"

      dup_days <- unique(h_subj_df$age_days[h_subj_df$extraneous])

      ide_ids <- c()
      for (dd in dup_days) {
        s_df <- copy(h_subj_df[h_subj_df$age_days == dd, ])
        ide_tab <- table(s_df$meas_m)
        if (any(ide_tab > 1)) {
          ide_ids <- c(ide_ids, s_df$internal_id[
            as.character(s_df$meas_m) %in% names(ide_tab[ide_tab > 1])
          ][duplicated(
            s_df$meas_m[
              as.character(s_df$meas_m) %in% names(ide_tab[ide_tab > 1])
            ]
          )])
        }
      }
      criteria <- h_subj_df$internal_id %in% ide_ids
      h_subj_keep[as.character(h_subj_df$internal_id)][criteria] <- step
      h_subj_df <- h_subj_df[!criteria, ]

      if (any(criteria)) {
        h_subj_df <- temp_sde(h_subj_df)
        dup_days <- unique(h_subj_df$age_days[h_subj_df$extraneous])
      }

      step <- "Exclude-A-Extraneous"

      if (any(h_subj_df$extraneous)) {
        sde_days <- unique(h_subj_df$age_days[h_subj_df$extraneous])
        non_sde_vals <- h_subj_df$meas_m[!h_subj_df$extraneous]
        daystot <- length(unique(h_subj_df$age_days))
        n_sde_days <- length(sde_days)
        sderatio <- n_sde_days / daystot

        daymed_by_day <- tapply(h_subj_df$meas_m, h_subj_df$age_days, median)
        non_sde_meas <- h_subj_df$meas_m[!h_subj_df$age_days %in% sde_days]
        nonsdemed <- if (length(non_sde_meas) > 0) median(non_sde_meas) else NA
        medmed <- median(daymed_by_day)

        rem_ids <- c()
        for (dd in sde_days) {
          s_df <- h_subj_df[h_subj_df$age_days == dd, ]
          if (nrow(s_df) <= 1) next

          s_df$absdiff_daymed <- abs(s_df$meas_m - daymed_by_day[as.character(dd)])

          if (!is.na(nonsdemed)) {
            s_df$absdiff_nonsdemed <- abs(s_df$meas_m - nonsdemed)
          } else {
            s_df$absdiff_nonsdemed <- Inf
          }
          s_df$absdiff_medmed <- abs(s_df$meas_m - medmed)

          if (daystot == 1) {
            s_df <- s_df[order(s_df$absdiff_daymed, -s_df$internal_id), ]
          } else if (daystot >= 4 & sderatio < 0.5) {
            s_df <- s_df[order(s_df$absdiff_nonsdemed, s_df$absdiff_daymed,
                               -s_df$internal_id), ]
          } else {
            s_df <- s_df[order(s_df$absdiff_medmed, s_df$absdiff_nonsdemed,
                               s_df$absdiff_daymed, -s_df$internal_id), ]
          }

          rem_ids <- c(rem_ids, s_df$internal_id[-1])
        }

        criteria <- h_subj_df$internal_id %in% rem_ids
        h_subj_keep[as.character(h_subj_df$internal_id)][criteria] <- step
        h_subj_df <- h_subj_df[!criteria, ]
      }
    }

    # =========================================================================
    # STEP 9Wb: EXTREME EWMA
    # Exclude weight outliers where EWMA deviation exceeds dynamic threshold
    # (2-tier <6m/>=6m with weight scaling). Uses exponent e=-5.
    # Range gate: skip if max-min <= cap_short+0.12 (no value can exceed cap).
    # Iterates until no more candidates or <3 values remain (no round limit).
    # Independent mode: single pass, RVs as full participants.
    # Linked mode: firstRV pass (non-RV only) -> propagate -> allRV pass.
    # =========================================================================

    if (nrow(w_subj_df) > 0) {
      inc_df <- copy(w_subj_df[!w_subj_df$extraneous, ])

      if (nrow(inc_df) >= 3) {
        wt_range <- max(inc_df$meas_m) - min(inc_df$meas_m)

        min_et_ewma <- compute_et_limit(1, formula = wtallow_formula,
                                        uw = min(inc_df$meas_m))
        if (wt_range > (min_et_ewma + 0.12)) {
          if (repval_handling == "independent") {
            # Independent: single pass, all values (including RVs) participate
            ewma_result <- remove_ewma_wt(inc_df, wtallow_formula = wtallow_formula, ewma_window = ewma_window)
            if (length(ewma_result) > 0) {
              w_subj_keep[names(ewma_result)] <- ewma_result
              w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% names(ewma_result), ]
              if (nrow(w_subj_df) > 0) w_subj_df <- identify_rv(w_subj_df)
            }
          } else {
            # Linked: firstRV pass -- exclude using non-RV values only
            has_rv <- "is_rv" %in% names(inc_df)
            firstRV_df <- if (has_rv) inc_df[!inc_df$is_rv, ] else inc_df

            if (nrow(firstRV_df) >= 3) {
              fr_range <- max(firstRV_df$meas_m) - min(firstRV_df$meas_m)
              min_et_fr <- compute_et_limit(1, formula = wtallow_formula,
                                           uw = min(firstRV_df$meas_m))
              if (fr_range > (min_et_fr + 0.12)) {
                fr_result <- remove_ewma_wt(firstRV_df, wtallow_formula = wtallow_formula,
                                            exc_label = "Exclude-A-Traj-Extreme-firstRV",
                                            ewma_window = ewma_window)
                if (length(fr_result) > 0) {
                  w_subj_keep[names(fr_result)] <- fr_result
                  # Propagate firstRV exclusions to their RV copies
                  prop <- propagate_to_rv(fr_result, w_subj_df, w_subj_keep)
                  w_subj_keep <- prop$w_subj_keep
                  all_exc <- c(names(fr_result), prop$propagated_ids)
                  w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% all_exc, ]
                  if (nrow(w_subj_df) > 0) w_subj_df <- identify_rv(w_subj_df)
                }
              }
            }

            # Linked: allRV pass -- remaining values, RVs now participate
            inc_df_all <- if (nrow(w_subj_df) > 0) {
              copy(w_subj_df[!w_subj_df$extraneous, ])
            } else {
              w_subj_df
            }
            if (nrow(inc_df_all) >= 3) {
              ar_range <- max(inc_df_all$meas_m) - min(inc_df_all$meas_m)
              min_et_ar <- compute_et_limit(1, formula = wtallow_formula,
                                           uw = min(inc_df_all$meas_m))
              if (ar_range > (min_et_ar + 0.12)) {
                ar_result <- remove_ewma_wt(inc_df_all, wtallow_formula = wtallow_formula,
                                            exc_label = "Exclude-A-Traj-Extreme-allRV",
                                            ewma_window = ewma_window)
                if (length(ar_result) > 0) {
                  w_subj_keep[names(ar_result)] <- ar_result
                  w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% names(ar_result), ]
                  if (nrow(w_subj_df) > 0) w_subj_df <- identify_rv(w_subj_df)
                }
              }
            }
          }
        }
      }
    }

    # =========================================================================
    # STEP 10H: HEIGHT DISTINCT VALUES
    # Evaluate subjects with 2+ distinct heights.
    # 10Ha (2D): band check, loss/gain rescue, frequency rescue.
    # 10Hb (3+D): w2 window selection, loss/gain group rescue.
    # Tolerance: 0.12 cm on all height comparisons (0.1cm rounding + float).
    # =========================================================================

    num_distinct <- length(unique(h_subj_df$meas_m))
    criteria <- rep(FALSE, nrow(h_subj_df))

    pairhtloss <- pairhtgain <- FALSE
    glist_loss <- glist_gain <- list()
    origexc <- g3_g2_check <- g3_g1_check <- g2_g1_check <- TRUE

    if (num_distinct == 2) {
      # 10Ha: 2 distinct heights -- band check (ht_band*2.54+0.12 cm),
      # gain rescue (young adults, htallow+2), loss rescue (allow_ht_loss),
      # frequency rescue (4/3 ratio).
      # Codes: Height-Ordered-Pairs-All (all excluded) or
      #        Height-Ordered-Pairs (some rescued by frequency)

      ht_1 <- unique(h_subj_df$meas_m[order(h_subj_df$age_days)])[1]
      ht_2 <- unique(h_subj_df$meas_m[order(h_subj_df$age_days)])[2]
      ht_1_log <- h_subj_df$meas_m == ht_1
      ht_2_log <- h_subj_df$meas_m == ht_2
      ht_1_ageyears <- h_subj_df$ageyears[ht_1_log]
      ht_2_ageyears <- h_subj_df$ageyears[ht_2_log]

      exc_2d <- abs(ht_1 - ht_2) > (ht_band * 2.54 + 0.12)

      if (exc_2d) {
        ageyears1 <- sort(ht_1_ageyears)[1]
        ageyears2 <- sort(ht_2_ageyears)[1]

        if (allow_ht_gain && ageyears1 < 25) {
          htcompare <- ifelse(ageyears2 > 25, 25, ageyears2)

          hta <-
            if ((ageyears2 - ageyears1) < 2) {
              ht_allow(15.5, ageyears1, htcompare)
            } else if ((ageyears2 - ageyears1) <= 3) {
              ht_allow(13, ageyears1, htcompare)
            } else if ((ageyears2 - ageyears1) > 3) {
              ht_allow(12, ageyears1, htcompare)
            }

          pairhtgain <-
            (ht_2 - ht_1) <= ((hta + 2) * 2.54 + 0.12) &
            (ht_2 - ht_1) > -0.12 &
            min(ht_2_ageyears) > max(ht_1_ageyears)
        } else {
          pairhtgain <- FALSE
        }

        pairhtloss <-
          allow_ht_loss &
          (ht_1 > ht_2 - 0.12) &
          (((ht_1 - ht_2) <= (5 * 2.54 + 0.12)) |
             (((ht_1 - ht_2) <= (7 * 2.54 + 0.12)) & ageyears2 >= 50)) &
          min(ht_2_ageyears) > max(ht_1_ageyears)

        exc_pairs <- !(pairhtloss | pairhtgain)
        if (exc_pairs) {
          criteria <- rep(TRUE, nrow(h_subj_df))
        }

        keepht1 <- sum(ht_1_log) >= (sum(ht_2_log) * (4 / 3))
        keepht2 <- sum(ht_2_log) >= (sum(ht_1_log) * (4 / 3))

        if (keepht1) {
          criteria[ht_1_log] <- FALSE
        }
        if (keepht2) {
          criteria[ht_2_log] <- FALSE
        }

        # Assign code: -All if no frequency rescue, otherwise without -All
        step <- if (keepht1 | keepht2) {
          "Exclude-A-Ord-Pair"
        } else {
          "Exclude-A-Ord-Pair-All"
        }
      }

    } else if (num_distinct >= 3) {
      # 10Hb: 3+ distinct heights -- find best w2 window, then evaluate
      # loss/gain group rescue.
      # Codes: Height-Window-All (no viable w2) or Height-Window (outside w2)

      h_subj_df <- h_subj_df[order(h_subj_df$ageyears, h_subj_df$internal_id), ]

      w2_window <- ht_band * 2.54 + 0.12
      w2_groups <- lapply(unique(h_subj_df$meas_m), function(x) {
        h_subj_df$meas_m[check_between(h_subj_df$meas_m, x, x + w2_window)]
      })
      o2_groups <- lapply(unique(h_subj_df$meas_m), function(x) {
        h_subj_df$meas_m[!check_between(h_subj_df$meas_m, x, x + w2_window)]
      })
      names(w2_groups) <- names(o2_groups) <- unique(h_subj_df$meas_m)

      ratio_w2o2 <- sapply(unique(h_subj_df$meas_m), function(x) {
        length(w2_groups[[as.character(x)]]) / length(o2_groups[[as.character(x)]])
      })
      okratio <- ratio_w2o2 >= 3 / 2

      if (sum(okratio) > 1) {
        consider_w2 <- unique(h_subj_df$meas_m)[okratio]
        score <- sapply(consider_w2, function(x) {
          length(w2_groups[[as.character(x)]]) +
            .5 * length(unique(w2_groups[[as.character(x)]]))
        })
        best_scores <- which(score == max(score))
        if (length(best_scores) > 1) {
          mean_abs_dist <- sapply(consider_w2[best_scores], function(x) {
            mean(abs(mean(w2_groups[[as.character(x)]]) -
                       o2_groups[[as.character(x)]]))
          })
          mean_abs_dist[is.na(mean_abs_dist)] <- Inf
          best_w2 <- consider_w2[best_scores][which.max(mean_abs_dist)]
        } else {
          best_w2 <- consider_w2[best_scores]
        }
      } else if (sum(okratio) == 1) {
        best_w2 <- unique(h_subj_df$meas_m)[okratio]
      } else {
        best_w2 <- "none"
      }

      if (best_w2 != "none") {
        step <- "Exclude-A-Window"
        criteria[!h_subj_df$meas_m %in% w2_groups[[as.character(best_w2)]]] <- TRUE
      } else {
        step <- "Exclude-A-Window-All"
        criteria <- rep(TRUE, nrow(h_subj_df))
      }

      # 3D loss (skip rescue when allow_ht_loss = FALSE)
      if (any(criteria) & allow_ht_loss) {
        gtotal_loss <- ht_change_groups(h_subj_df, 3, type = "loss")
        glist <- glist_loss <- gtotal_loss$meas
        galist <- gtotal_loss$age

        if (length(glist) > 0 & length(glist) <= 3) {
          mean_ht_vals <- sapply(glist, function(g) { suppressWarnings(mean(g)) })
          min_age <- sapply(galist, function(ga) { suppressWarnings(min(ga)) })

          g2_g1_check <-
            !(if (!is.na(mean_ht_vals[2])) {
              (mean_ht_vals[2] - mean_ht_vals[1]) < -0.12 &
                ((min_age[2] < 50 &
                    (mean_ht_vals[2] - mean_ht_vals[1]) > ((-5 * 2.54) + 0.12)) |
                   (min_age[2] >= 50 &
                      (mean_ht_vals[2] - mean_ht_vals[1]) > ((-7 * 2.54) + 0.12)))
            } else {
              FALSE
            })

          g3_g2_check <-
            if (!is.na(mean_ht_vals[2]) & !is.na(mean_ht_vals[3])) {
              (mean_ht_vals[3] - mean_ht_vals[2]) > -0.12 |
                (min_age[3] < 50 &
                   (mean_ht_vals[3] - mean_ht_vals[2]) < ((-5 * 2.54) + 0.12)) |
                (min_age[3] >= 50 &
                   (mean_ht_vals[3] - mean_ht_vals[2]) < ((-7 * 2.54) + 0.12))
            } else {
              FALSE
            }

          g3_g1_check <-
            if (!is.na(mean_ht_vals[2]) & !is.na(mean_ht_vals[3])) {
              (min_age[3] < 50 &
                 (mean_ht_vals[3] - mean_ht_vals[1]) < ((-6 * 2.54) + 0.12)) |
                (min_age[2] < 50 & min_age[3] >= 50 &
                   (mean_ht_vals[3] - mean_ht_vals[1]) < ((-8 * 2.54) + 0.12)) |
                (min_age[2] >= 50 &
                   (mean_ht_vals[3] - mean_ht_vals[1]) < ((-9 * 2.54) + 0.12))
            } else {
              FALSE
            }

          if (all(!c(g3_g2_check, g3_g1_check, g2_g1_check))) {
            criteria <- rep(FALSE, nrow(h_subj_df))
          }
        }
      }

      # 3D gain
      if (allow_ht_gain && any(criteria) & min(h_subj_df$ageyears) < 25) {
        gtotal_gain <- ht_change_groups(h_subj_df, 6, type = "gain")
        glist <- glist_gain <- gtotal_gain$meas
        galist <- gtotal_gain$age

        if (length(glist) > 0 & length(glist) <= 6) {
          mean_ht_vals <- sapply(glist, function(g) { suppressWarnings(mean(g)) })
          min_age <- sapply(galist, function(ga) { suppressWarnings(min(ga)) })

          origexc <- FALSE
          origexc <- origexc |
            ht_3d_growth_compare(mean_ht_vals, min_age, glist, compare = "before")
          origexc <- origexc |
            ht_3d_growth_compare(mean_ht_vals, min_age, glist, compare = "first")

          if (!origexc) {
            criteria <- rep(FALSE, nrow(h_subj_df))
          }
        }
      }
    }

    # Process loss/gain groups for output
    loss_groups <- gain_groups <- rep(NA, length(h_subj_keep))
    names(loss_groups) <- names(gain_groups) <- names(h_subj_keep)

    hold_groups <-
      if (length(glist_loss) > 0) {
        unlist(lapply(seq_along(glist_loss), function(x) {
          setNames(rep(x, length(glist_loss[[x]])), names(glist_loss[[x]]))
        }))
      } else {
        loss_groups
      }
    loss_groups[names(hold_groups)] <- hold_groups

    hold_groups <-
      if (length(glist_gain) > 0) {
        unlist(lapply(seq_along(glist_gain), function(x) {
          setNames(rep(x, length(glist_gain[[x]])), names(glist_gain[[x]]))
        }))
      } else {
        c()
      }
    gain_groups[names(hold_groups)] <- hold_groups

    h_subj_keep[as.character(h_subj_df$internal_id)][criteria] <- step
    h_subj_df <- h_subj_df[!criteria, ]

    # =========================================================================
    # STEP 10W: WEIGHT SDE RESOLUTION
    # =========================================================================

    if (nrow(w_subj_df) > 0 & any(w_subj_df$extraneous)) {
      step <- "Exclude-A-Identical"

      dup_days <- unique(w_subj_df$age_days[w_subj_df$extraneous])

      ide_ids <- c()
      for (dd in dup_days) {
        s_df <- copy(w_subj_df[w_subj_df$age_days == dd, ])
        ide_tab <- table(s_df$meas_m)
        if (any(ide_tab > 1)) {
          ide_ids <- c(ide_ids, s_df$internal_id[
            as.character(s_df$meas_m) %in% names(ide_tab[ide_tab > 1])
          ][duplicated(
            s_df$meas_m[
              as.character(s_df$meas_m) %in% names(ide_tab[ide_tab > 1])
            ]
          )])
        }
      }
      criteria <- w_subj_df$internal_id %in% ide_ids
      w_subj_keep[as.character(w_subj_df$internal_id)][criteria] <- step
      w_subj_df <- w_subj_df[!criteria, ]

      if (any(criteria)) {
        w_subj_df <- temp_sde(w_subj_df, ptype = "weight")
        w_subj_df <- redo_identify_rv(w_subj_df)
        dup_days <- unique(w_subj_df$age_days[w_subj_df$extraneous])
      }

      step <- "Exclude-A-Extraneous"

      if (any(w_subj_df$extraneous)) {
        sde_days <- unique(w_subj_df$age_days[w_subj_df$extraneous])
        daystot <- length(unique(w_subj_df$age_days))
        n_sde_days <- length(sde_days)
        sderatio <- n_sde_days / daystot

        daymed_by_day <- tapply(w_subj_df$meas_m, w_subj_df$age_days, median)
        # nonsdemed: use firstRV (non-RV) values only -- repeated values should not bias the reference median
        nonrv_filter <- if ("is_rv" %in% names(w_subj_df)) !w_subj_df$is_rv else TRUE
        non_sde_meas <- w_subj_df$meas_m[!w_subj_df$age_days %in% sde_days & nonrv_filter]
        nonsdemed <- if (length(non_sde_meas) > 0) median(non_sde_meas) else NA
        medmed <- median(daymed_by_day)

        rem_ids <- c()
        for (dd in sde_days) {
          s_df <- w_subj_df[w_subj_df$age_days == dd, ]
          if (nrow(s_df) <= 1) next

          s_df$absdiff_daymed <- abs(s_df$meas_m - daymed_by_day[as.character(dd)])

          if (!is.na(nonsdemed)) {
            s_df$absdiff_nonsdemed <- abs(s_df$meas_m - nonsdemed)
          } else {
            s_df$absdiff_nonsdemed <- Inf
          }
          s_df$absdiff_medmed <- abs(s_df$meas_m - medmed)

          if (daystot == 1) {
            s_df <- s_df[order(s_df$absdiff_daymed, -s_df$internal_id), ]
          } else if (daystot >= 4 & sderatio < 0.5) {
            s_df <- s_df[order(s_df$absdiff_nonsdemed, s_df$absdiff_daymed,
                               -s_df$internal_id), ]
          } else {
            s_df <- s_df[order(s_df$absdiff_medmed, s_df$absdiff_nonsdemed,
                               s_df$absdiff_daymed, -s_df$internal_id), ]
          }

          rem_ids <- c(rem_ids, s_df$internal_id[-1])
        }

        criteria <- w_subj_df$internal_id %in% rem_ids
        w_subj_keep[as.character(w_subj_df$internal_id)][criteria] <- step
        w_subj_df <- w_subj_df[!criteria, ]

        if (any(criteria) & nrow(w_subj_df) > 0) {
          w_subj_df <- identify_rv(w_subj_df)
        }
      }
    }

    # =========================================================================
    # STEP 11H: MEAN HEIGHT
    # Calculate mean height using meas_m (metric measurement).
    # Pair loss/gain: individual values (no averaging across states).
    # Loss/gain groups: mean within each group.
    # All others: simple mean of all included heights.
    # =========================================================================

    meanht <- setNames(rep(NA, length(h_subj_keep)), names(h_subj_keep))
    if (nrow(h_subj_df) > 0) {
      hold_meanht <-
        if (pairhtloss | pairhtgain) {
          h_subj_df$meas_m
        } else if (length(glist_loss) > 0 &
                   all(!c(g3_g2_check, g3_g1_check, g2_g1_check))) {
          unlist(lapply(glist_loss, function(x) {
            rep(mean(h_subj_df$meas_m[h_subj_df$internal_id %in% names(x)]),
                length(x))
          }))
        } else if (length(glist_gain) > 0 & !origexc) {
          unlist(lapply(glist_gain, function(x) {
            rep(mean(h_subj_df$meas_m[h_subj_df$internal_id %in% names(x)]),
                length(x))
          }))
        } else {
          rep(mean(h_subj_df$meas_m), nrow(h_subj_df))
        }
      names(hold_meanht) <- h_subj_df$internal_id
      meanht[names(hold_meanht)] <- hold_meanht
    }

    # =========================================================================
    # STEP 11Wa: 2D ORDERED WEIGHT PAIRS
    # =========================================================================

    # Use firstRV (non-RV) values for numdistinct routing,
    # but allRV (all values) for non-ordered check
    w_nonrv <- if ("is_rv" %in% names(w_subj_df)) {
      w_subj_df[!w_subj_df$is_rv, ]
    } else {
      w_subj_df
    }

    # firstRV numdistinct and ordering
    pair_distinct <- if (nrow(w_nonrv) > 0 && length(unique(w_nonrv$meas_m)) == 2) {
      uvals <- unique(w_nonrv$meas_m)
      max(w_nonrv$ageyears[w_nonrv$meas_m == uvals[1]]) <
        min(w_nonrv$ageyears[w_nonrv$meas_m == uvals[2]])
    } else {
      FALSE
    }

    # allRV non-ordered check (uses ALL values, including RVs)
    is_2d_nonord <- FALSE
    if (length(unique(w_subj_df$meas_m)) == 2) {
      uvals_all <- unique(w_subj_df$meas_m)
      v1_ages <- w_subj_df$ageyears[w_subj_df$meas_m == uvals_all[1]]
      v2_ages <- w_subj_df$ageyears[w_subj_df$meas_m == uvals_all[2]]
      allrv_ordered <- max(v1_ages) < min(v2_ages) || max(v2_ages) < min(v1_ages)
      if (!allrv_ordered) {
        is_2d_nonord <- TRUE
        pair_distinct <- FALSE
      }
    }

    if (pair_distinct) {
      step <- "Exclude-A-2D-Ordered"

      wt_first <- w_nonrv$meas_m[1]
      wt_last <- w_nonrv$meas_m[nrow(w_nonrv)]
      wt_diff <- wt_last - wt_first

      uvals <- unique(w_nonrv$meas_m)
      ageyears_diff <-
        min(w_nonrv$ageyears[w_nonrv$meas_m == uvals[2]]) -
        max(w_nonrv$ageyears[w_nonrv$meas_m == uvals[1]])

      uw_pair <- max(wt_first, wt_last)
      wta <- compute_wtallow(ageyears_diff * 12, formula = wtallow_formula,
                             uw = uw_pair)

      exc_pairs <- abs(wt_diff) > wta + 0.12  # rounding tolerance

      wt_perc <-
        if (wt_last / wt_first < 1) {
          wt_last / wt_first
        } else {
          wt_first / wt_last
        }

      # Subject-level perclimit (max across observations per spec).
      # This differs from 11Wb which uses observation-level perclimit.
      # In 11Wa there are only 2 distinct values so subject-level vs
      # observation-level produces the same result in practice.
      perc_limit <- max(compute_perc_limit(w_subj_df$meas_m, perclimit_low, perclimit_mid, perclimit_high))

      exc_pairs <- exc_pairs | wt_perc < perc_limit
      criteria <- exc_pairs

      impl_ids <- as.character(w_subj_df$internal_id)[criteria]
      w_subj_keep[c(impl_ids)] <- step
      w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% c(impl_ids), ]

    } else if (is_2d_nonord) {
      # =========================================================================
      # STEP 11Wa2: 2D NON-ORDERED WEIGHT PAIRS
      # =========================================================================

      step <- "Exclude-A-2D-Non-Ordered"
      nonord_exc_ids <- eval_2d_nonord(w_subj_df, w_subj_keep,
                                       wtallow_formula = wtallow_formula)
      if (length(nonord_exc_ids) > 0) {
        w_subj_keep[nonord_exc_ids] <- step
        w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% nonord_exc_ids, ]
      }

    } else if (length(unique(w_subj_df$meas_m)) >= 3) {
      # =========================================================================
      # STEP 11Wb: 7-STEP MODERATE EWMA
      # =========================================================================

      if (repval_handling == "independent") {
        # Single pass: RVs as full participants
        inc_df <- copy(w_subj_df)
        if (nrow(inc_df) >= 3) {
          sorted_ages <- sort(inc_df$age_days)
          min_gap_months <- min(diff(sorted_ages)) / 30.4375
          min_wta <- compute_wtallow(min_gap_months, formula = wtallow_formula,
                                     uw = min(inc_df$meas_m))
          wt_range_mod <- max(inc_df$meas_m) - min(inc_df$meas_m)
          min_perc_ratio <- min(inc_df$meas_m) / max(inc_df$meas_m)
          max_perc <- max(compute_perc_limit(inc_df$meas_m, perclimit_low, perclimit_mid, perclimit_high))
          can_trigger <- wt_range_mod > min_wta |
                         (max_perc > 0 & min_perc_ratio < max_perc)

          mod_result <- if (can_trigger) {
            remove_mod_ewma_wt(inc_df, exc_label = "Exclude-A-Traj-Moderate",
                               wtallow_formula = wtallow_formula,
                               ewma_window = ewma_window,
                               mod_ewma_f = mod_ewma_f,
                               perclimit_low = perclimit_low,
                               perclimit_mid = perclimit_mid,
                               perclimit_high = perclimit_high)
          } else {
            character(0)
          }
          if (length(mod_result) > 0) {
            w_subj_keep[names(mod_result)] <- mod_result
            w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% names(mod_result), ]
          }
        }
      } else {
        # Linked: firstRV pass (non-RV only)
        has_rv <- "is_rv" %in% names(w_subj_df)
        firstRV_df <- if (has_rv) w_subj_df[!w_subj_df$is_rv, ] else copy(w_subj_df)

        if (nrow(firstRV_df) >= 3 && length(unique(firstRV_df$meas_m)) >= 3) {
          sorted_ages_fr <- sort(firstRV_df$age_days)
          min_gap_fr <- min(diff(sorted_ages_fr)) / 30.4375
          min_wta_fr <- compute_wtallow(min_gap_fr, formula = wtallow_formula,
                                        uw = min(firstRV_df$meas_m))
          wt_range_fr <- max(firstRV_df$meas_m) - min(firstRV_df$meas_m)
          min_perc_fr <- min(firstRV_df$meas_m) / max(firstRV_df$meas_m)
          max_perc_limit_fr <- max(compute_perc_limit(firstRV_df$meas_m, perclimit_low, perclimit_mid, perclimit_high))
          can_trigger_fr <- wt_range_fr > min_wta_fr |
                            (max_perc_limit_fr > 0 & min_perc_fr < max_perc_limit_fr)

          # Moderate firstRV uses the same label as independent
          fr_result <- if (can_trigger_fr) {
            remove_mod_ewma_wt(firstRV_df, exc_label = "Exclude-A-Traj-Moderate",
                               wtallow_formula = wtallow_formula,
                               ewma_window = ewma_window,
                               mod_ewma_f = mod_ewma_f,
                               perclimit_low = perclimit_low,
                               perclimit_mid = perclimit_mid,
                               perclimit_high = perclimit_high)
          } else {
            character(0)
          }

          if (length(fr_result) > 0) {
            w_subj_keep[names(fr_result)] <- fr_result

            # Error load RV escalation (linked only):
            # If any error load value has RV copies, escalate entire patient
            el_codes <- fr_result[grepl("-Error-Load", fr_result)]
            if (length(el_codes) > 0 && has_rv) {
              el_meas <- unique(w_subj_df$meas_m[w_subj_df$internal_id %in% names(el_codes)])
              rv_of_el <- as.character(w_subj_df$internal_id[
                w_subj_df$meas_m %in% el_meas &
                w_subj_df$is_rv &
                !w_subj_df$internal_id %in% names(el_codes)])
              rv_of_el <- rv_of_el[w_subj_keep[rv_of_el] == "Include"]

              if (length(rv_of_el) > 0) {
                remaining_inc <- names(w_subj_keep)[w_subj_keep == "Include"]
                w_subj_keep[remaining_inc] <- "Exclude-A-Traj-Moderate-Error-Load-RV"
              }
            }

            # Propagate non-error-load exclusions to RV copies
            non_el <- fr_result[!grepl("-Error-Load", fr_result)]
            if (length(non_el) > 0) {
              prop <- propagate_to_rv(non_el, w_subj_df, w_subj_keep)
              w_subj_keep <- prop$w_subj_keep
            }

            # Remove all newly excluded from w_subj_df
            exc_ids_all <- names(w_subj_keep)[w_subj_keep != "Include"]
            w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% exc_ids_all, ]
            if (nrow(w_subj_df) > 0) w_subj_df <- identify_rv(w_subj_df)
          }
        }

        # Linked: allRV pass (remaining values, RVs as participants)
        if (nrow(w_subj_df) >= 3 && length(unique(w_subj_df$meas_m)) >= 3) {
          inc_df_all <- copy(w_subj_df)
          sorted_ages_all <- sort(inc_df_all$age_days)
          min_gap_all <- min(diff(sorted_ages_all)) / 30.4375
          min_wta_all <- compute_wtallow(min_gap_all, formula = wtallow_formula,
                                         uw = min(inc_df_all$meas_m))
          wt_range_all <- max(inc_df_all$meas_m) - min(inc_df_all$meas_m)
          min_perc_all <- min(inc_df_all$meas_m) / max(inc_df_all$meas_m)
          max_perc_limit_all <- max(compute_perc_limit(inc_df_all$meas_m, perclimit_low, perclimit_mid, perclimit_high))
          can_trigger_all <- wt_range_all > min_wta_all |
                             (max_perc_limit_all > 0 & min_perc_all < max_perc_limit_all)

          ar_result <- if (can_trigger_all) {
            remove_mod_ewma_wt(inc_df_all, exc_label = "Exclude-A-Traj-Moderate-allRV",
                               wtallow_formula = wtallow_formula,
                               ewma_window = ewma_window,
                               mod_ewma_f = mod_ewma_f,
                               perclimit_low = perclimit_low,
                               perclimit_mid = perclimit_mid,
                               perclimit_high = perclimit_high)
          } else {
            character(0)
          }
          if (length(ar_result) > 0) {
            w_subj_keep[names(ar_result)] <- ar_result
            w_subj_df <- w_subj_df[!w_subj_df$internal_id %in% names(ar_result), ]
          }
        }
      }
    }

    # =========================================================================
    # OUTPUT: Add results to main dataframe
    # =========================================================================

    if (length(h_subj_keep) > 0) {
      h_out <- data.table(
        internal_id = as.integer(names(h_subj_keep)),
        keep = h_subj_keep,
        mean_ht = meanht,
        loss_grp = loss_groups,
        gain_grp = gain_groups,
        extraneous = h_extraneous
      )
      df[h_out, result := i.keep, on = .(internal_id)]
      df[h_out, mean_ht := i.mean_ht, on = .(internal_id)]
      df[h_out, loss_groups := i.loss_grp, on = .(internal_id)]
      df[h_out, gain_groups := i.gain_grp, on = .(internal_id)]
      df[h_out, extraneous := i.extraneous, on = .(internal_id)]
    }
    if (length(w_subj_keep) > 0) {
      w_out <- data.table(
        internal_id = as.integer(names(w_subj_keep)),
        keep = w_subj_keep,
        extraneous = w_extraneous
      )
      df[w_out, result := i.keep, on = .(internal_id)]
      df[w_out, extraneous := i.extraneous, on = .(internal_id)]
    }
  }

  if (!quietly) message("Subject loop complete.")

  # ===========================================================================
  # POST-LOOP: Steps 13 and 14 (operate across subjects)
  # ===========================================================================

  if (!quietly) message("Running Step 13 (1D evaluation)...")
  for (i in unique(df$subjid)) {
    subj_data <- df[df$subjid == i, ]
    exc_1d_ids <- eval_1d(subj_data, params_1d)
    if (length(exc_1d_ids) > 0) {
      df[df$internal_id %in% exc_1d_ids & df$result == "Include" &
           param %in% c("HEIGHTCM", "HEIGHTIN"),
         result := "Exclude-A-Single"]
      df[df$internal_id %in% exc_1d_ids & df$result == "Include" &
           param %in% c("WEIGHTKG", "WEIGHTLBS"),
         result := "Exclude-A-Single"]
    }
  }

  if (!quietly) message("Running Step 14 (Error load)...")
  for (i in unique(df$subjid)) {
    subj_data <- df[df$subjid == i, ]
    exc_el_ids <- eval_error_load(subj_data, error_threshold = error_load_threshold)
    if (length(exc_el_ids) > 0) {
      df[df$internal_id %in% exc_el_ids & df$result == "Include" &
           param %in% c("HEIGHTCM", "HEIGHTIN"),
         result := "Exclude-A-Unevaluable-Trajectory"]
      df[df$internal_id %in% exc_el_ids & df$result == "Include" &
           param %in% c("WEIGHTKG", "WEIGHTLBS"),
         result := "Exclude-A-Unevaluable-Trajectory"]
    }
  }

  # ===========================================================================
  # FINAL MEAN HEIGHT (after Steps 13/14)
  # mean_ht is computed at Step 11H from the heights included at that point.
  # Steps 13/14 can later exclude a subject's remaining heights (always all of
  # them), and rows excluded before 11H never received a value. Finalize so
  # mean_ht is computed ONLY from finally-included heights but populated on
  # EVERY height row of the subject:
  #   - no included heights left -> NA on all of the subject's height rows;
  #   - an included height keeps its Step 11H value;
  #   - any other height row (excluded, or missing measurement) takes the
  #     mean_ht of the included height nearest in age (ties -> the earlier one).
  # ===========================================================================
  ht_rows <- which(df$param %in% c("HEIGHTCM", "HEIGHTIN"))
  if (length(ht_rows) > 0) {
    for (i in unique(df$subjid[ht_rows])) {
      s_rows <- ht_rows[df$subjid[ht_rows] == i]
      inc <- s_rows[df$result[s_rows] == "Include" & !is.na(df$mean_ht[s_rows])]
      if (length(inc) == 0) {
        df[s_rows, mean_ht := NA_real_]
        next
      }
      inc <- inc[order(df$agedays[inc], df$internal_id[inc])]
      fill <- setdiff(s_rows, inc)
      if (length(fill) > 0) {
        near <- vapply(fill, function(r) {
          inc[which.min(abs(df$agedays[inc] - df$agedays[r]))]
        }, integer(1))
        df[fill, mean_ht := df$mean_ht[near]]
      }
    }
  }

  if (!quietly) {
    message("Algorithm complete.")
  }

  # =============================================================================
  # OUTPUT CLEANUP
  # =============================================================================

  # id is the user's original -- untouched throughout

  # Remove internal working columns (meas_m always internal;
  # ageyears and age_days only if we added them)
  df[, meas_m := NULL]
  for (col in added_cols) {
    if (col %in% names(df)) df[, (col) := NULL]
  }

  # Add bin_result (binary Include/Exclude)
  if (include_bin_result) {
    df[, bin_result := ifelse(result == "Include", "Include", "Exclude")]
  }

  # Remove optional columns unless requested
  if (!include_extraneous && "extraneous" %in% names(df)) {
    df[, extraneous := NULL]
  }
  if (!include_ht_groups) {
    for (col in c("loss_groups", "gain_groups")) {
      if (col %in% names(df)) df[, (col) := NULL]
    }
  }

  return(df)
}
