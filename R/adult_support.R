# Adult growthcleanr support functions
# Internal functions for the adult algorithm (adult_clean.R)
# Supports both independent and linked repval_handling modes

# permissiveness presets ----

#' Return preset setting values for each permissiveness level
#'
#' Returns a named list of four permissiveness levels (loosest, looser,
#' tighter, tightest), each containing the full set of adult algorithm
#' settings for that level. Default level is "looser".
#'
#' @return Named list of four lists, each containing setting values
#' @seealso Section 1D of \code{vignette("configuration")} for what each level
#'   is intended for and the preset values; \code{vignette("adult-algorithm")}
#'   for what each setting does.
#' @examples
#' presets <- permissiveness_presets()
#' names(presets)
#' presets$looser$overall_wt_max
#' @export
permissiveness_presets <- function() {
  list(
    loosest = list(
      # PIV (overall) limits
      overall_ht_min = 50, overall_ht_max = 244,
      overall_wt_min = 20, overall_wt_max = 500,
      overall_bmi_min = 5, overall_bmi_max = 300,
      # 1D (single) limits
      single_ht_min_bmi = 60, single_ht_max_bmi = 245,
      single_wt_min_bmi = 12, single_wt_max_bmi = 350,
      single_ht_min_nobmi = 120, single_ht_max_nobmi = 245,
      single_wt_min_nobmi = 30, single_wt_max_nobmi = 350,
      single_bmi_min = 10, single_bmi_max = 250,
      # Algorithm parameters
      wtallow_formula = "piecewise",
      perclimit_low = 0.5, perclimit_mid = 0.4, perclimit_high = 0.0,
      error_load_threshold = 0.41,
      mod_ewma_f = 0.75,
      ht_band = 3,
      allow_ht_loss = TRUE, allow_ht_gain = TRUE,
      repval_handling = "independent"
    ),
    looser = list(
      overall_ht_min = 120, overall_ht_max = 230,
      overall_wt_min = 30, overall_wt_max = 270,
      overall_bmi_min = 12, overall_bmi_max = 65,
      single_ht_min_bmi = 120, single_ht_max_bmi = 230,
      single_wt_min_bmi = 30, single_wt_max_bmi = 270,
      single_ht_min_nobmi = 120, single_ht_max_nobmi = 230,
      single_wt_min_nobmi = 30, single_wt_max_nobmi = 270,
      single_bmi_min = 12, single_bmi_max = 65,
      wtallow_formula = "piecewise",
      perclimit_low = 0.5, perclimit_mid = 0.4, perclimit_high = 0.0,
      error_load_threshold = 0.41,
      mod_ewma_f = 0.75,
      ht_band = 3,
      allow_ht_loss = FALSE, allow_ht_gain = TRUE,
      repval_handling = "independent"
    ),
    tighter = list(
      overall_ht_min = 142, overall_ht_max = 213,
      overall_wt_min = 36, overall_wt_max = 159,
      overall_bmi_min = 16, overall_bmi_max = 45,
      single_ht_min_bmi = 142, single_ht_max_bmi = 213,
      single_wt_min_bmi = 36, single_wt_max_bmi = 159,
      single_ht_min_nobmi = 142, single_ht_max_nobmi = 213,
      single_wt_min_nobmi = 36, single_wt_max_nobmi = 159,
      single_bmi_min = 16, single_bmi_max = 45,
      wtallow_formula = "piecewise-lower",
      perclimit_low = 0.7, perclimit_mid = 0.4, perclimit_high = 0.4,
      error_load_threshold = 0.29,
      mod_ewma_f = 0.60,
      ht_band = 2,
      allow_ht_loss = FALSE, allow_ht_gain = TRUE,
      repval_handling = "linked"
    ),
    tightest = list(
      overall_ht_min = 147, overall_ht_max = 208,
      overall_wt_min = 39, overall_wt_max = 136,
      overall_bmi_min = 18, overall_bmi_max = 40,
      single_ht_min_bmi = 147, single_ht_max_bmi = 208,
      single_wt_min_bmi = 39, single_wt_max_bmi = 136,
      single_ht_min_nobmi = 147, single_ht_max_nobmi = 208,
      single_wt_min_nobmi = 39, single_wt_max_nobmi = 136,
      single_bmi_min = 18, single_bmi_max = 40,
      wtallow_formula = "allofus15",
      perclimit_low = 0.7, perclimit_mid = 0.4, perclimit_high = 0.4,
      error_load_threshold = 0.29,
      mod_ewma_f = 0.60,
      ht_band = 2,
      allow_ht_loss = FALSE, allow_ht_gain = FALSE,
      repval_handling = "linked"
    )
  )
}

#' Resolve permissiveness: fill NULLs from preset, keep explicit values
#' @param permissiveness Character: "loosest", "looser", "tighter", "tightest"
#' @param ... Named parameter values (NULL = use preset, non-NULL = override)
#' @return Named list of all resolved parameter values
#' @keywords internal
resolve_permissiveness <- function(permissiveness, ...) {
  valid <- c("loosest", "looser", "tighter", "tightest")
  # Guard length/type before %in% so a non-scalar (e.g. c("looser","tighter"))
  # gives this informative message instead of a cryptic "condition has length > 1".
  # This runs only on the adult path, so a bad adult_permissiveness fails fast when
  # the data has adult rows and is (intentionally) never reached for child-only data.
  if (length(permissiveness) != 1L || !is.character(permissiveness) ||
      !permissiveness %in% valid) {
    stop(paste0("adult_permissiveness must be one of: ",
                paste(valid, collapse = ", "),
                ". Got: ", .gc_desc_val(permissiveness)),
         call. = FALSE)
  }
  preset <- permissiveness_presets()[[permissiveness]]
  user <- list(...)
  # For each preset param, use user value if non-NULL, else preset. A non-NULL
  # override is validated against the TYPE/SHAPE of the preset value it replaces:
  # these sub-parameters flow into the adult algorithm's per-subject scalar if()
  # comparisons, where an NA / non-scalar / wrong-type value would otherwise crash
  # with "missing value where TRUE/FALSE needed" or silently misbehave. Reached
  # through cleangrowth(adult_overrides = ...) and direct cleanadult() calls.
  # wtallow_formula keeps its own
  # downstream builtin-or-file validation; here we only enforce it is a single string.
  resolved <- preset
  for (nm in names(user)) {
    val <- user[[nm]]
    if (is.null(val)) next   # NULL = use preset default
    ref <- preset[[nm]]
    if (length(val) != 1L || (!is.list(val) && is.na(val))) {
      stop(sprintf("%s must be a single non-missing value; %s was entered.",
                   nm, .gc_desc_val(val)))
    }
    if (is.numeric(ref) && (!is.numeric(val) || !is.finite(val))) {
      stop(sprintf("%s must be a single finite number; %s was entered.",
                   nm, .gc_desc_val(val)))
    } else if (is.logical(ref) && !is.logical(val)) {
      stop(sprintf("%s must be TRUE or FALSE; %s was entered.",
                   nm, .gc_desc_val(val)))
    } else if (is.character(ref) && !is.character(val)) {
      stop(sprintf("%s must be a single string; %s was entered.",
                   nm, .gc_desc_val(val)))
    }
    resolved[[nm]] <- val
  }
  resolved
}

# convenience functions ----

#' convenience function -- see if numeric vector falls between two numbers
#' returns boolean vector
#' @keywords internal
#' @noRd
check_between <- function(vect, num_low, num_high, incl = TRUE) {
  return(
    if (incl) {
      vect <= num_high & vect >= num_low
    } else {
      vect < num_high & vect > num_low
    }
  )
}

#' convenience function -- round to the nearest .x
#' @keywords internal
#' @noRd
round_pt <- function(val, pt) {
  return(round(val / pt) * pt)
}

# Dynamic ET/EWMA cap and perclimit helpers ----

#' Compute the Evil Twins / Extreme EWMA cap for given intervals and weights.
#' Vectorized: interval_months and uw can be vectors.
#'
#' For built-in formulas (piecewise, piecewise-lower, allofus15), ET caps are
#' derived from the formula's wtallow caps + 20 (for PW-H/PW-L) or equal to
#' the allofus15 12m cap. For custom CSV formulas, fixed caps of 70/100 are used.
#'
#' @param interval_months Interval in months (minimum neighbor gap)
#' @param formula Character: "piecewise", "piecewise-lower", "allofus15", or
#'   path to custom CSV
#' @param uw Optional numeric vector of upper weights (higher of two adjacent
#'   weights, or max(wt, ewma)). NULL = use base (UW=120) caps.
#' @return Numeric vector of ET limits
#' @references Guide A, Sulieman L, Garbett S, Cronin RM, Spotnitz M,
#'   Natarajan K, Carroll RJ, Harris P, Chen Q. Identifying erroneous height
#'   and weight values from adult electronic health records in the All of Us
#'   research program. J Biomed Inform. 2024 Jul;155:104660.
#'   \doi{10.1016/j.jbi.2024.104660}. (Source of the adapted "allofus15"
#'   formula.)
#' @keywords internal
compute_et_limit <- function(interval_months, formula = "piecewise", uw = NULL) {
  # Base caps for each formula
  PWH_CAP_6M  <- 50
  PWH_CAP_12M <- 80
  PWL_CAP_6M  <- 100 / 3   # 33.33
  PWL_CAP_12M <- 160 / 3   # 53.33
  ET_OFFSET   <- 20

  is_builtin <- formula %in% c("piecewise", "piecewise-lower", "allofus15")

  if (!is_builtin) {
    # Custom CSV: fixed 70/100
    return(ifelse(interval_months <= 6, 70, 100))
  }

  if (formula == "piecewise") {
    if (is.null(uw)) {
      et_6m  <- PWH_CAP_6M + ET_OFFSET    # 70
      et_12m <- PWH_CAP_12M + ET_OFFSET   # 100
    } else {
      uw_eff <- pmin(uw, 180)
      # wtallow cap + 20 at each tier
      et_6m <- ifelse(uw_eff > 120,
                      PWH_CAP_6M + 0.25 * (uw_eff - 120) + ET_OFFSET,
               ifelse(uw_eff < 120,
                      (PWH_CAP_6M - 20) * (uw_eff / 120) + 20 + ET_OFFSET,
                      PWH_CAP_6M + ET_OFFSET))
      et_12m <- ifelse(uw_eff > 120,
                       PWH_CAP_12M + 0.25 * (uw_eff - 120) + ET_OFFSET,
                ifelse(uw_eff < 120,
                       (PWH_CAP_12M - 20) * (uw_eff / 120) + 20 + ET_OFFSET,
                       PWH_CAP_12M + ET_OFFSET))
    }

  } else if (formula == "piecewise-lower") {
    if (is.null(uw)) {
      et_6m  <- PWL_CAP_6M + ET_OFFSET    # 53.33
      et_12m <- PWL_CAP_12M + ET_OFFSET   # 73.33
    } else {
      uw_eff <- pmin(uw, 180)
      # PW-L: no highUW adjustment (UW >= 120 uses base caps)
      et_6m <- ifelse(uw_eff < 120,
                      (PWL_CAP_6M - 20) * (uw_eff / 120) + 20 + ET_OFFSET,
                      PWL_CAP_6M + ET_OFFSET)
      et_12m <- ifelse(uw_eff < 120,
                       (PWL_CAP_12M - 20) * (uw_eff / 120) + 20 + ET_OFFSET,
                       PWL_CAP_12M + ET_OFFSET)
    }

  } else {
    # allofus15: ET cap = allofus15-cap-12m (flat across all intervals)
    if (is.null(uw)) {
      return(rep(40, length(interval_months)))
    }
    uw_eff <- pmin(uw, 180)
    pwl_cap_12m <- (PWL_CAP_12M - 20) * (uw_eff / 120) + 20
    pwl_eff <- pmin(pwl_cap_12m, uw_eff * 2 / 3)
    cap_12m <- ifelse(uw_eff >= 120, 40, pmin(40, pwl_eff))
    return(cap_12m)
  }

  # 2-tier: <=6m uses et_6m, >6m uses et_12m
  ifelse(interval_months <= 6, et_6m, et_12m)
}

#' Compute observation-level percentage criterion limit.
#' Vectorized over meas.
#' @param meas Weight measurements in kg
#' @param perclimit_low Limit for wt <= 45 kg
#' @param perclimit_mid Limit for 45 < wt <= 80 kg
#' @param perclimit_high Limit for wt > 80 kg (0 = disabled: percewma < 0 is never TRUE)
#' @return Numeric vector of perc_limits
#' @keywords internal
compute_perc_limit <- function(meas, perclimit_low, perclimit_mid, perclimit_high) {
  ifelse(meas <= 45, perclimit_low, ifelse(meas <= 80, perclimit_mid, perclimit_high))
}

# EWMA functions ----

#' function to calculate as delta matrix for adults
#' @keywords internal
#' @noRd
as.matrix.delta_dn <- function(agedays) {
  n <- length(agedays)
  delta <- abs(matrix(rep(agedays, n), n, byrow = TRUE) - agedays)
  return(delta)
}

#' Exponentially Weighted Moving Average (EWMA)
#' Adult version uses |delta|^(-5) weighting (not pediatric (5+delta)^(-1.5))
#' @keywords internal
#' @noRd
ewma_dn <- function(agedays, meas, ewma.exp = -5, ewma.adjacent = TRUE,
                    ewma_window = 15) {
  n <- length(agedays)
  ewma.all <- ewma.before <- ewma.after <- vector('numeric', 0)
  if (n > 0) {
    if (!all(agedays == cummax(agedays)))
      warning("EWMA ordering is not sorted; double check")
    index <- order(agedays)

    delta <- as.matrix.delta_dn(agedays)
    delta <- ifelse(delta == 0, 0, (delta) ^ ewma.exp)

    # Apply position-based window: zero out entries beyond ewma_window positions
    if (!is.null(ewma_window)) {
      pos_dist <- abs(row(delta) - col(delta))
      delta[pos_dist > ewma_window] <- 0
    }

    ewma.all[index] <- delta %*% meas / apply(delta, 1, sum)

    if (ewma.adjacent) {
      if (n > 2) {
        delta2 = delta
        delta2[col(delta2) == row(delta2) - 1] = 0
        ewma.before[index] = delta2 %*% meas / apply(delta2, 1, sum)
        delta3 = delta
        delta3[col(delta3) == row(delta3) + 1] = 0
        ewma.after[index] = delta3 %*% meas / apply(delta3, 1, sum)
      } else {
        ewma.before <- ewma.after <- ewma.all
      }
    }
  }
  return(if (ewma.adjacent)
    data.frame(ewma.all, ewma.before, ewma.after)
    else
      data.frame(ewma.all))
}

# EWMA Cache functions ----
# EWMA cache: O(n) iterative updates instead of O(n^2) full rebuild

#' Initialize EWMA cache for a set of observations.
#' Builds the full weight matrix once (O(n^2)), computes EWMA values.
#' @param agedays Sorted age in days
#' @param meas Measurements (same order as agedays)
#' @param ewma_exp Exponent for distance weighting (default -5)
#' @param ewma_window Maximum number of positions on each side included in the EWMA (default 15)
#' @return Cache list with delta matrix, weighted sums, row sums, and EWMA values
#' @keywords internal
adult_ewma_cache_init <- function(agedays, meas, ewma_exp = -5, ewma_window = 15) {
  n <- length(agedays)
  if (n == 0) return(NULL)

  # Build weight matrix
  delta <- as.matrix.delta_dn(agedays)
  delta <- ifelse(delta == 0, 0, delta ^ ewma_exp)

  # Apply position-based window: zero out entries beyond ewma_window positions
  if (!is.null(ewma_window)) {
    pos_dist <- abs(row(delta) - col(delta))
    delta[pos_dist > ewma_window] <- 0
  }

  # Weighted sums and row sums
  ws <- as.vector(delta %*% meas)
  rs <- rowSums(delta)

  # EWMA values
  ewma_all <- ws / rs

  if (n > 2) {
    # Subdiagonal trick: ewma_before removes predecessor, ewma_after removes successor
    pred_w <- c(0, delta[cbind(2:n, 1:(n - 1))])
    pred_m <- c(0, meas[1:(n - 1)])
    ewma_before <- (ws - pred_w * pred_m) / (rs - pred_w)

    succ_w <- c(delta[cbind(1:(n - 1), 2:n)], 0)
    succ_m <- c(meas[2:n], 0)
    ewma_after <- (ws - succ_w * succ_m) / (rs - succ_w)
  } else {
    ewma_before <- ewma_after <- ewma_all
  }

  list(
    delta = delta,
    ws = ws,
    rs = rs,
    meas = meas,
    agedays = agedays,
    n = n,
    ewma_all = ewma_all,
    ewma_before = ewma_before,
    ewma_after = ewma_after
  )
}

#' Update EWMA cache by removing one observation. O(n) instead of O(n^2).
#' @param cache Cache list from adult_ewma_cache_init or previous update
#' @param pos_j Position (index) of the observation to remove
#' @return Updated cache list with n-1 observations, or NULL if n would be 0
#' @keywords internal
adult_ewma_cache_update <- function(cache, pos_j) {
  n <- cache$n
  if (n <= 1) return(NULL)

  keep <- seq_len(n)[-pos_j]

  # Subtract obs j's contribution from all other weighted sums and row sums
  col_j <- cache$delta[keep, pos_j]
  ws <- cache$ws[keep] - col_j * cache$meas[pos_j]
  rs <- cache$rs[keep] - col_j

  # Trim matrix and vectors
  delta <- cache$delta[keep, keep, drop = FALSE]
  meas <- cache$meas[keep]
  agedays <- cache$agedays[keep]
  n_new <- n - 1L

  # Recompute EWMA values from updated sums
  ewma_all <- ws / rs

  if (n_new > 2) {
    pred_w <- c(0, delta[cbind(2:n_new, 1:(n_new - 1))])
    pred_m <- c(0, meas[1:(n_new - 1)])
    ewma_before <- (ws - pred_w * pred_m) / (rs - pred_w)

    succ_w <- c(delta[cbind(1:(n_new - 1), 2:n_new)], 0)
    succ_m <- c(meas[2:n_new], 0)
    ewma_after <- (ws - succ_w * succ_m) / (rs - succ_w)
  } else {
    ewma_before <- ewma_after <- ewma_all
  }

  list(
    delta = delta,
    ws = ws,
    rs = rs,
    meas = meas,
    agedays = agedays,
    n = n_new,
    ewma_all = ewma_all,
    ewma_before = ewma_before,
    ewma_after = ewma_after
  )
}

# step 1w, W PIV ----

#' function to remove PIVs, based on cutoffs for the given method
#' @keywords internal
#' @noRd
remove_piv <- function(subj_df, type, piv_df) {
  too_low <- remove_piv_low(subj_df, type, piv_df)
  too_high <- remove_piv_high(subj_df, type, piv_df)
  return(too_low | too_high)
}

#' @keywords internal
#' @noRd
remove_piv_low <- function(subj_df, type, piv_df) {
  # 0.12 tolerance for rounding (0.1 cm/kg rounding + float precision)
  too_low <- subj_df$meas_m < piv_df[type, "low"] - 0.12
  return(too_low)
}

#' @keywords internal
#' @noRd
remove_piv_high <- function(subj_df, type, piv_df) {
  # 0.12 tolerance for rounding (0.1 cm/kg rounding + float precision)
  too_high <- subj_df$meas_m > piv_df[type, "high"] + 0.12
  return(too_high)
}

# step 2w, W repeated values ----

#' Identify repeated weight values within a subject.
#' Marks the first occurrence (earliest age, internal_id tiebreaker) as is_first_rv=TRUE
#' and all subsequent identical values as is_rv=TRUE. Unique values get both FALSE.
#' Relies on w_subj_df being pre-sorted by age/id. Exact numeric match only
#' (78.1 != 78.101). Does not filter by extraneous status; caller is responsible
#' for sequencing (see identify_rv -> temp_sde -> redo_identify_rv cycle).
#' @keywords internal
#' @noRd
identify_rv <- function(w_subj_df) {
  if (nrow(w_subj_df) > 0) {
    # is_rv: TRUE for all duplicates except the first occurrence
    w_subj_df$is_rv <- duplicated(w_subj_df$meas_m)
    # is_first_rv: TRUE for the first occurrence of values that have duplicates
    w_subj_df$is_first_rv <- duplicated(w_subj_df$meas_m, fromLast = TRUE) &
      !w_subj_df$is_rv
  }
  return(w_subj_df)
}

# step 3, temp extraneous ----

#' Temporarily flag same-day values that deviate most from patient median.
#' For weight (ptype="weight"): median of non-RV values (all included, not
#' filtered by same-day status). For height (ptype="height"): median of all
#' included values. On each same-day group, the value closest to median
#' survives (internal_id tiebreaker); others are marked extraneous=TRUE.
#' @keywords internal
#' @noRd
temp_sde <- function(subj_df, ptype = "height") {
  tab_days <- table(subj_df$age_days)
  dup_days <- names(tab_days)[tab_days > 1]

  if (nrow(subj_df) >= 2) {
    # Median of all included values (non-RV only for weight, all for height)
    if (ptype == "weight") {
      med_val <- median(subj_df$meas_m[!subj_df$is_rv])
    } else {
      med_val <- median(subj_df$meas_m)
    }

    subj_df$diff <- NA
    subj_df$diff[as.character(subj_df$age_days) %in% dup_days] <-
      abs(subj_df$meas_m[as.character(subj_df$age_days) %in% dup_days] -
            med_val)

    subj_df$extraneous <- FALSE
    for (dd in dup_days) {
      day_diffs <- subj_df$diff[as.character(subj_df$age_days) == dd]
      # Keep highest internal_id among ties (last position with min diff, matching
      # final SDE resolution which keeps highest internal_id)
      keeper <- max(which(day_diffs == min(day_diffs)))
      subj_df$extraneous[as.character(subj_df$age_days) == dd][-keeper] <- TRUE
    }

    subj_df$diff <- NULL
  } else if (nrow(subj_df) > 0) {
    subj_df$extraneous <- FALSE
  }

  return(subj_df)
}

#' Re-identify repeated values after temp SDE resolution.
#' Only runs if an is_first_rv value was marked extraneous by temp_sde() --
#' in that case, a different value in the RV group may need to become first_rv.
#' Subsets to non-extraneous values, re-runs identify_rv(), maps results back.
#' Note: after non-SDE exclusions (weight cap, evil twins), the calling code
#' uses identify_rv() directly on the remaining rows, then temp_sde(), then
#' this function for SDE cleanup.
#' @keywords internal
#' @noRd
redo_identify_rv <- function(w_subj_df) {
  if (nrow(w_subj_df) > 0 & any(w_subj_df$extraneous & w_subj_df$is_first_rv)) {
    inc_df <- copy(w_subj_df[!w_subj_df$extraneous, ])
    inc_df <- identify_rv(inc_df)
    w_subj_df$is_first_rv <- w_subj_df$is_rv <- FALSE
    w_subj_df$is_rv[w_subj_df$internal_id %in% inc_df$internal_id] <- inc_df$is_rv
    w_subj_df$is_first_rv[w_subj_df$internal_id %in% inc_df$internal_id] <- inc_df$is_first_rv
  }
  return(w_subj_df)
}

# step 10 hab, H distinct values ----

#' function to calculate height growth allowance
#' @keywords internal
#' @noRd
ht_allow <- function(velocity, ageyears1, ageyears2) {
  return(
    velocity * (log(ageyears2 - 16.9)) - (velocity * log(ageyears1 - 16.9))
  )
}

#' function to generate height growth/loss groups
#' @keywords internal
#' @noRd
ht_change_groups <- function(h_subj_df, cutoff, type = "loss") {
  glist <- galist <- list()
  cg <- 1
  glist[[cg]] <- setNames(h_subj_df$meas_m[1], h_subj_df$internal_id[1])
  galist[[cg]] <- h_subj_df$ageyears[1]
  for (m in 2:nrow(h_subj_df)) {
    cm <- h_subj_df$meas_m[m]
    crng <- max(c(glist[[cg]], cm)) - min(c(glist[[cg]], cm))
    temp_mindiff <- min(glist[[cg]]) - cm
    temp_maxdiff <- max(glist[[cg]]) - cm

    if (crng < (5.08 + 0.12)) {
      glist[[cg]] <- setNames(c(glist[[cg]], cm),
                              c(names(glist[[cg]]), h_subj_df$internal_id[m]))
      galist[[cg]] <- c(galist[[cg]], h_subj_df$ageyears[m])
    } else {
      cg <- cg + 1
      glist[[cg]] <- setNames(cm, h_subj_df$internal_id[m])
      galist[[cg]] <- h_subj_df$ageyears[m]
    }

    if (cg > cutoff) {
      break
    }

    if (type == "loss") {
      if (temp_mindiff < -(5.08 + 0.12)) {
        glist <- galist <- list()
        break
      }
    } else {
      if (temp_maxdiff > (5.08 + 0.12)) {
        glist <- galist <- list()
        break
      }
    }
  }

  return(list(
    "meas" = glist,
    "age" = galist
  ))
}

#' function to compare growth for 3D height groups
#' @keywords internal
#' @noRd
ht_3d_growth_compare <- function(mean_ht, min_age, glist,
                                 compare = "before") {
  origexc <- FALSE
  for (i in 2:6) {
    if (i > length(glist)) {
      next
    }
    check_num <- if (compare == "before") { i - 1 } else { 1 }
    ageyears1 <- min_age[check_num]
    ageyears2 <- min_age[i]
    mh1 <- mean_ht[check_num]
    mh2 <- mean_ht[i]

    htcompare <- ifelse(ageyears2 > 25, 25, ageyears2)

    hta <-
      if ((htcompare - ageyears1) < 1) {
        ht_allow(20, ageyears1, htcompare)
      } else if ((htcompare - ageyears1) <= 3) {
        ht_allow(15, ageyears1, htcompare)
      } else if ((htcompare - ageyears1) > 3) {
        ht_allow(12, ageyears1, htcompare)
      }

    origexc <- origexc |
      ((mh2 - mh1) < -0.12 |
       (mh2 - mh1) > hta + 0.12)
  }

  return(origexc)
}

# compute_wtallow ----

#' Compute weight allowance from interval in months (vectorized)
#'
#' Built-in formulas: "piecewise" (PW-H, default), "piecewise-lower" (PW-L),
#' "allofus15". Custom: pass a file path to a CSV with columns 'months' and
#' 'wtallow'. Values are linearly interpolated; months beyond the table are
#' clamped to the nearest row.
#'
#' UW (upper weight) scaling:
#' - PW-H: scaled up for UW > 120 (slope 0.25/kg, UW capped at 180) and
#'   down for UW < 120, with 2/3 ceiling.
#' - PW-L: NOT scaled up for UW > 120. Scaled down for UW < 120, with 2/3
#'   ceiling.
#' - allofus15: not adjusted by UW, except its 12m cap is limited to never
#'   exceed the effective PW-L at 12m for that UW.
#' - Custom CSV: no UW adjustment.
#'
#' @param months Numeric vector of interval lengths in months
#' @param formula Character: formula name or path to custom CSV
#' @param uw Optional numeric vector of upper weights (higher of two adjacent
#'   weights, or max(wt, ewma)). NULL = use base (UW=120) formula.
#' @return Numeric vector of weight allowances
#' @references Guide A, Sulieman L, Garbett S, Cronin RM, Spotnitz M,
#'   Natarajan K, Carroll RJ, Harris P, Chen Q. Identifying erroneous height
#'   and weight values from adult electronic health records in the All of Us
#'   research program. J Biomed Inform. 2024 Jul;155:104660.
#'   \doi{10.1016/j.jbi.2024.104660}. (Source of the adapted "allofus15"
#'   formula.)
#' @keywords internal
compute_wtallow <- function(months, formula = "piecewise", uw = NULL) {

  # Base caps for each formula family
  PWH_CAP_6M  <- 50
  PWH_CAP_12M <- 80
  PWL_CAP_6M  <- 100 / 3   # 33.33
  PWL_CAP_12M <- 160 / 3   # 53.33

  # wtallow at 1 day (starting point for PW-H-highUW linear segment)
  WTALLOW_1DAY <- 10 + 10 * log(1 + 5 / 30.4375) / log(6)  # ~10.85

  if (formula == "piecewise") {
    # ---------------------------------------------------------------
    # PW-H (Piecewise-Higher): used by loosest/looser
    # ---------------------------------------------------------------
    if (is.null(uw)) {
      # PW-H-Base (UW = 120, no adjustment)
      wta <- ifelse(months <= 1, 10 + 10 * log(1 + 5 * months) / log(6),
             ifelse(months <= 6, 20 + (PWH_CAP_6M - 20) / 5 * (months - 1),
             ifelse(months <= 12,
                    PWH_CAP_6M + (PWH_CAP_12M - PWH_CAP_6M) / 6 * (months - 6),
                    PWH_CAP_12M)))
    } else {
      uw_eff <- pmin(uw, 180)

      # Adjusted caps: depend on UW range
      adj_6m <- ifelse(uw_eff > 120,
                       PWH_CAP_6M + 0.25 * (uw_eff - 120),
                ifelse(uw_eff < 120,
                       (PWH_CAP_6M - 20) * (uw_eff / 120) + 20,
                       PWH_CAP_6M))
      adj_12m <- ifelse(uw_eff > 120,
                        PWH_CAP_12M + 0.25 * (uw_eff - 120),
                 ifelse(uw_eff < 120,
                        (PWH_CAP_12M - 20) * (uw_eff / 120) + 20,
                        PWH_CAP_12M))

      # Value at 1 month: 25 for highUW, 20 for base/lowUW
      val_1m <- ifelse(uw_eff > 120, 25, 20)

      # 0-1m segment: linear for highUW, log curve for base/lowUW
      val_0_1 <- ifelse(uw_eff > 120,
                        WTALLOW_1DAY + (25 - WTALLOW_1DAY) * months,
                        10 + 10 * log(1 + 5 * months) / log(6))

      # Linear segments
      val_1_6  <- val_1m + (adj_6m - val_1m) / 5 * (months - 1)
      val_6_12 <- adj_6m + (adj_12m - adj_6m) / 6 * (months - 6)

      # Assemble piecewise
      wta <- ifelse(months <= 1, val_0_1,
             ifelse(months <= 6, val_1_6,
             ifelse(months <= 12, val_6_12,
                                  adj_12m)))

      # 2/3 ceiling for lowUW
      wta <- ifelse(uw_eff < 120, pmin(wta, uw_eff * 2 / 3), wta)
    }

  } else if (formula == "piecewise-lower") {
    # ---------------------------------------------------------------
    # PW-L (Piecewise-Lower): used by tighter
    # NOT scaled up for UW > 120. Scaled down for UW < 120.
    # ---------------------------------------------------------------
    if (is.null(uw)) {
      # PW-L-Base (UW >= 120, no adjustment)
      wta <- ifelse(months <= 1, 10 + 10 * log(1 + 5 * months) / log(6),
             ifelse(months <= 6,
                    20 + (PWL_CAP_6M - 20) / 5 * (months - 1),
             ifelse(months <= 12,
                    PWL_CAP_6M + (PWL_CAP_12M - PWL_CAP_6M) / 6 * (months - 6),
                    PWL_CAP_12M)))
    } else {
      uw_eff <- pmin(uw, 180)

      # PW-L: UW >= 120 uses base caps; UW < 120 scales down
      adj_6m <- ifelse(uw_eff < 120,
                       (PWL_CAP_6M - 20) * (uw_eff / 120) + 20,
                       PWL_CAP_6M)
      adj_12m <- ifelse(uw_eff < 120,
                        (PWL_CAP_12M - 20) * (uw_eff / 120) + 20,
                        PWL_CAP_12M)

      # 0-1m: always log curve (no linear highUW variant for PW-L)
      val_0_1 <- 10 + 10 * log(1 + 5 * months) / log(6)

      val_1_6  <- 20 + (adj_6m - 20) / 5 * (months - 1)
      val_6_12 <- adj_6m + (adj_12m - adj_6m) / 6 * (months - 6)

      wta <- ifelse(months <= 1, val_0_1,
             ifelse(months <= 6, val_1_6,
             ifelse(months <= 12, val_6_12,
                                  adj_12m)))

      # 2/3 ceiling for lowUW
      wta <- ifelse(uw_eff < 120, pmin(wta, uw_eff * 2 / 3), wta)
    }

  } else if (formula == "allofus15") {
    # ---------------------------------------------------------------
    # allofus15: used by tightest. Not adjusted by UW except its
    # 12m cap is limited to never exceed effective PW-L at 12m.
    # ---------------------------------------------------------------
    if (is.null(uw)) {
      cap_12m <- 40
    } else {
      uw_eff <- pmin(uw, 180)
      pwl_cap_12m <- (PWL_CAP_12M - 20) * (uw_eff / 120) + 20
      pwl_eff <- pmin(pwl_cap_12m, uw_eff * 2 / 3)
      cap_12m <- ifelse(uw_eff >= 120, 40, pmin(40, pwl_eff))
    }

    days_2   <- 2 / 30.4375    # ~0.0657 months
    days_7   <- 7 / 30.4375    # ~0.230 months
    slope_6mo_12mo <- (cap_12m - 15) / (12 - 6)

    wta <- ifelse(months <= days_2, 5,
           ifelse(months <= days_7, 10,
           ifelse(months <= 6, 15,
           ifelse(months <= 12,
                  15 + slope_6mo_12mo * (months - 6),
                  cap_12m))))

  } else {
    # ---------------------------------------------------------------
    # Custom CSV: columns 'months' and 'wtallow', linearly interpolated.
    # No UW adjustment.
    # ---------------------------------------------------------------
    if (!exists(".wtallow_custom_cache", envir = .gc_cache) ||
        !identical(attr(get(".wtallow_custom_cache", envir = .gc_cache), "path"), formula)) {
      if (!file.exists(formula)) {
        stop(paste0("wtallow_formula '", formula, "' is not a built-in formula ",
                     "('piecewise', 'piecewise-lower', 'allofus15') ",
                     "and file not found."))
      }
      custom <- read.csv(formula, stringsAsFactors = FALSE)
      if (!all(c("months", "wtallow") %in% names(custom))) {
        stop(sprintf("Custom wtallow CSV (%s) must have columns 'months' and 'wtallow'. Columns found: %s.",
                     formula, paste(names(custom), collapse = ", ")))
      }
      custom <- custom[order(custom$months), ]
      attr(custom, "path") <- formula
      assign(".wtallow_custom_cache", custom, envir = .gc_cache)
    }
    custom <- get(".wtallow_custom_cache", envir = .gc_cache)
    wta <- approx(x = custom$months, y = custom$wtallow, xout = months, rule = 2)$y
  }

  wta
}

# detect_runs ----

#' Detect consecutive runs of TRUE in a logical vector
#' Returns list with run_id, run_len, run_pos for each element
#' @param flagged Logical vector
#' @keywords internal
detect_runs <- function(flagged) {
  n <- length(flagged)
  run_id  <- rep(NA_integer_, n)
  run_len <- rep(NA_integer_, n)
  run_pos <- rep(NA_integer_, n)
  if (n == 0 || !any(flagged)) return(list(run_id = run_id, run_len = run_len, run_pos = run_pos))

  is_start <- flagged & c(TRUE, !flagged[-n])
  cum_id <- cumsum(is_start)
  cum_id[!flagged] <- NA

  if (any(!is.na(cum_id))) {
    tbl <- table(cum_id)
    for (rid in names(tbl)) {
      idx <- which(cum_id == as.integer(rid))
      run_id[idx]  <- as.integer(rid)
      run_len[idx] <- as.integer(tbl[rid])
      run_pos[idx] <- seq_along(idx)
    }
  }
  list(run_id = run_id, run_len = run_len, run_pos = run_pos)
}

# compute_trajectory_fails ----

#' Pre-computes trajectory rescue for all observations.
#' Returns TRUE = fails all rescue (not rescued).
#' @param meas Weights (kg), age-sorted
#' @param age_days Ages in days
#' @param err Tolerance in kg (default 5)
#' @keywords internal
compute_trajectory_fails <- function(meas, age_days, err = 5) {
  n <- length(meas)
  if (n < 3) return(rep(TRUE, n))

  p1 <- c(NA, meas[-n])
  n1 <- c(meas[-1], NA)
  p2 <- c(NA, NA, meas[1:(n - 2)])
  n2 <- c(meas[3:n], NA, NA)
  ap1 <- c(NA, age_days[-n])
  an1 <- c(age_days[-1], NA)
  ap2 <- c(NA, NA, age_days[1:(n - 2)])
  an2 <- c(age_days[3:n], NA, NA)

  # METHOD 1: Interpolation between p1 and n1 (+/-err)
  lo <- pmin(p1, n1, na.rm = FALSE) - err
  hi <- pmax(p1, n1, na.rm = FALSE) + err
  rescued_interp <- !is.na(lo) & !is.na(hi) & meas >= lo & meas <= hi

  # METHOD 2: Extrapolation from prior (p2 -> p1)
  slope_p <- (p1 - p2) / (ap1 - ap2)
  lepolate_p <- p1 + slope_p * (age_days - ap1)
  lepolate_p <- round(lepolate_p / 0.2) * 0.2
  lo_p <- pmin(p2, lepolate_p, na.rm = FALSE) - err
  hi_p <- pmax(p2, lepolate_p, na.rm = FALSE) + err
  rescued_prior <- !is.na(lo_p) & !is.na(hi_p) & meas >= lo_p & meas <= hi_p
  # Distance guard: don't trust extrapolation > 2x source interval
  dist_extrap_p <- abs(age_days - ap1)
  dist_source_p <- abs(ap1 - ap2)
  rescued_prior[!is.na(dist_extrap_p) & !is.na(dist_source_p) &
                dist_extrap_p > 2 * dist_source_p] <- FALSE

  # METHOD 3: Extrapolation from next (n2 -> n1)
  slope_n <- (n1 - n2) / (an1 - an2)
  lepolate_n <- n1 + slope_n * (age_days - an1)
  lepolate_n <- round(lepolate_n / 0.2) * 0.2
  lo_n <- pmin(n2, lepolate_n, na.rm = FALSE) - err
  hi_n <- pmax(n2, lepolate_n, na.rm = FALSE) + err
  rescued_next <- !is.na(lo_n) & !is.na(hi_n) & meas >= lo_n & meas <= hi_n
  dist_extrap_n <- abs(an1 - age_days)
  dist_source_n <- abs(an2 - an1)
  rescued_next[!is.na(dist_extrap_n) & !is.na(dist_source_n) &
               dist_extrap_n > 2 * dist_source_n] <- FALSE

  rescued_interp[is.na(rescued_interp)] <- FALSE
  rescued_prior[is.na(rescued_prior)]   <- FALSE
  rescued_next[is.na(rescued_next)]     <- FALSE

  fails <- !rescued_interp & !rescued_prior & !rescued_next
  fails
}

# Adult CSD reference + unit-error-range helpers ----
# These live here (not in child_clean.R) so the adult algorithm and the
# standalone adult test harness -- which sources only the adult files -- can
# reach them. The child z3 lookup and the child+adult unit_error_range assembler
# stay in child_clean.R and reach .unit_error_range_flag / the adult CSD helpers
# via the package namespace.

#' Adult CSD reference constants (single named source).
#'
#' Sex-specific median + below-/above-median SDs for adult HEIGHTCM and WEIGHTKG,
#' from NHANES 2017-2020 + 2021-2023 (unweighted, ages 18+). Used by the adult
#' unit_error_range zn3/zp3 and the adult Evil Twins CSD z-scores (median-band
#' |z| tiebreak, UER3 unit-error check). sex: 0 = male, 1 = female. No HC
#' (adults have none).
#'
#' A sex = NA row is appended per param as the simple mean of the male and
#' female constants, so adults whose sex is missing/unrecognized (coerced to NA
#' at input) still get a sensible reference rather than NA z-scores. This is a
#' derived fallback, NOT a re-derived pooled-sex NHANES distribution; the CSV
#' remains the M/F source-of-record. Computed here (not hard-coded) so it stays
#' in sync if the empirical M/F constants ever change.
#'
#' Kept as ONE named object so the constants are never re-entered inline in a
#' z-score function.
#' @keywords internal
#' @noRd
.adult_csd_ref <- function() {
  M <- SD_neg <- SD_pos <- NULL
  ref <- data.table::data.table(
    param  = c("HEIGHTCM", "HEIGHTCM", "WEIGHTKG", "WEIGHTKG"),
    sex    = c(1L, 0L, 1L, 0L),
    M      = c(160.3, 174.1, 74.0, 85.2),
    SD_neg = c(7.1,   7.8657, 13.75, 14.65),
    SD_pos = c(7.1,   7.7,    29.9,  29.6)
  )
  # sex = NA fallback: mean of male + female per param.
  na_ref <- ref[, .(sex = NA_integer_,
                    M = mean(M), SD_neg = mean(SD_neg), SD_pos = mean(SD_pos)),
                by = param]
  data.table::rbindlist(list(ref, na_ref), use.names = TRUE)
}

#' Look up adult CSD constants (M, SD_neg, SD_pos) per (param, sex).
#'
#' NA-safe: sex = NA resolves to the averaged fallback row in `.adult_csd_ref()`.
#' Uses a -1 sentinel for NA sex in both the query and the reference so the join
#' does not depend on NA-matching join semantics. Returns NA for params not in
#' the adult reference (e.g. HEADCM). Inputs are equal-length vectors; output
#' preserves input order.
#' @keywords internal
#' @noRd
.adult_csd_lookup <- function(param, sex) {
  M <- SD_neg <- SD_pos <- .ord <- .sexkey <- NULL
  ref <- .adult_csd_ref()
  ref[, .sexkey := data.table::fifelse(is.na(sex), -1L, as.integer(sex))]
  sk <- as.integer(sex)
  q <- data.table::data.table(param = as.character(param),
                              .sexkey = data.table::fifelse(is.na(sk), -1L, sk),
                              .ord = seq_along(param))
  q <- merge(q, ref[, .(param, .sexkey, M, SD_neg, SD_pos)],
             by = c("param", ".sexkey"), all.x = TRUE, sort = FALSE)
  data.table::setorder(q, .ord)
  list(M = q$M, SD_neg = q$SD_neg, SD_pos = q$SD_pos)
}

#' Adult zn3/zp3 (measurement at CSD z = -3 / +3), per param and sex.
#'
#' zn3 = M - 3*SD_neg; zp3 = M + 3*SD_pos. NA-safe via `.adult_csd_lookup`.
#' Returns NA for params not in the adult reference (e.g. HEADCM). Inputs are
#' equal-length vectors; output preserves input order.
#' @keywords internal
#' @noRd
.adult_z3 <- function(param, sex) {
  r <- .adult_csd_lookup(param, sex)
  list(zn3 = r$M - 3 * r$SD_neg, zp3 = r$M + 3 * r$SD_pos)
}

#' Adult per-value CSD z-score, per (param, sex).
#'
#' Two-sided CSD: z = (x - M) / SD_neg if x < M, else (x - M) / SD_pos. NA-safe
#' via `.adult_csd_lookup` (sex = NA -> averaged fallback). Returns NA for params
#' not in the adult reference. Used by the adult Evil Twins median-band |z|
#' tiebreak and the UER3 plausible-group test. Inputs are equal-length vectors;
#' output preserves input order.
#' @keywords internal
#' @noRd
.adult_csd_z <- function(param, sex, value) {
  r <- .adult_csd_lookup(param, sex)
  sd_side <- data.table::fifelse(value < r$M, r$SD_neg, r$SD_pos)
  (value - r$M) / sd_side
}

#' Compute the direction-agnostic unit-error-range flag for a vector of values.
#'
#' Shared by the child (Step 9 + the wrapper assembler) and the adult Evil Twins
#' UER3 check. Given each row's param, metric measurement `value`, and
#' plausible-band endpoints zn3/zp3 (measurements at z = -3 / +3): build the two
#' unit-error bands -- range_im (measured imperial, recorded metric) and range_mi
#' (measured metric, recorded imperial), each EXCLUSIVE -- and the plausible
#' carve-out range_3 = the inclusive interval from zn3 to zp3. Returns TRUE iff
#' the value falls in range_im OR range_mi AND NOT in range_3. NA endpoints (no
#' lookup) -> FALSE.
#' @keywords internal
#' @noRd
.unit_error_range_flag <- function(param, value, zn3, zp3) {
  LB_PER_KG <- 2.2046226218   # WT: lb-number-in-kg-field inflates
  CM_PER_IN <- 2.54           # HT/HC: in-number-in-cm-field deflates
  is_wt <- as.character(param) == "WEIGHTKG"

  # im = measured imperial, recorded metric; mi = measured metric, recorded imperial
  im_zn3 <- data.table::fifelse(is_wt, zn3 * LB_PER_KG, zn3 / CM_PER_IN)
  im_zp3 <- data.table::fifelse(is_wt, zp3 * LB_PER_KG, zp3 / CM_PER_IN)
  mi_zn3 <- data.table::fifelse(is_wt, zn3 / LB_PER_KG, zn3 * CM_PER_IN)
  mi_zp3 <- data.table::fifelse(is_wt, zp3 / LB_PER_KG, zp3 * CM_PER_IN)

  in_im <- value > pmin(im_zn3, im_zp3) & value < pmax(im_zn3, im_zp3)
  in_mi <- value > pmin(mi_zn3, mi_zp3) & value < pmax(mi_zn3, mi_zp3)
  in_3  <- value >= zn3 & value <= zp3

  res <- (in_im | in_mi) & !in_3
  res[is.na(res)] <- FALSE
  res
}

# Step 9Wa: Evil Twins ----

#' Evil twins detection for one subject (anchor rule)
#'
#' Adult Step 9Wa, weight only, in kg measurement-space. Mirrors the child Step 9
#' anchor rule and one-exclusion-per-iteration selection (calc_otl_evil_twins +
#' the Step 9 driver in child_clean.R); the scale differs (raw kg + interval/UW
#' etcaps in place of z-scores).
#'
#' A weight is OTL ("over-the-limit") only if it is an inner member (B or C) of
#' an evaluable over-the-limit pair: four age-ordered eligible values A, B, C, D
#' where the inner B-C jump exceeds etcap(B,C) and BOTH outer pairs (A-B, C-D)
#' anchor within 60% of their own etcap. A missing A or D disqualifies the pair,
#' so single spikes (record ends and singles embedded between normals) cede to
#' remove_ewma_wt(), which has the better-informed directional 90% rule and
#' per-observation thresholds. All comparisons to etcap carry the 0.12 kg
#' rounding tolerance used throughout the adult algorithm.
#'
#' RVs (repeated values) participate as ordinary values: no special handling.
#'
#' Selection per iteration (one exclusion): if a single OTL candidate, exclude
#' it. Otherwise compute absd_med = |meas - windowed median| (median over +/-4
#' age-ordered positions, kg space) and route by the UER3 unit-error check and a
#' median band, identically in structure to the child:
#'  - UER3 (computed ONCE on the initial Include set): if the SP splits cleanly
#'    into a unit-error group (unit_error_range flag, all one CSD-z sign) and a
#'    plausible group (|adult CSD z| < 3) whose mean kg differ by ~2.2046 (the
#'    lb<->kg factor, band 1.7-2.7), the unit-error block is the implausible one
#'    and the median can be pulled toward it, so exclude the highest |z|.
#'  - Otherwise: band = candidates whose absd_med is within band_width of the max
#'    absd_med, band_width = 0.20 * etcap(B,C) of the max candidate's inner pair
#'    (the adult analogue of the child's fixed z-unit band). If >= 2 in band (the
#'    windowed median cannot discriminate), exclude the highest |adult CSD z|; if
#'    exactly 1 (one absd_med stands clearly apart), absd_med is decisive.
#'  - Tiebreak: lowest internal_id.
#'
#' Adult CSD z (sex-specific, NA-safe via the averaged sex=NA reference row) is
#' used for the UER3 plausible group and the |z| tiebreak; weights are looked up
#' as WEIGHTKG (meas_m is already kg).
#'
#' @param w_subj_df Weight data for one subject (data.table with meas_m, age_days,
#'   sex, internal_id). All eligible (non-extraneous) values, RVs included.
#' @param wtallow_formula Formula for wtallow/ET caps
#' @return Character vector of internal_ids to exclude (Exclude-A-Evil-Twins)
#' @keywords internal
evil_twins <- function(w_subj_df, wtallow_formula = "piecewise") {
  # Anchor rule needs >= 4 eligible weights (A, B, C, D); fewer cannot form an
  # evaluable pair. Re-checked at the top of every iteration.
  inc_df <- w_subj_df[order(w_subj_df$age_days, w_subj_df$internal_id), ]
  exc_ids <- character(0)

  # --- UER3: computed ONCE on the initial Include set (adult WT, kg space) ---
  sp_w   <- inc_df$meas_m
  sp_sex <- inc_df$sex
  sp_par <- rep("WEIGHTKG", length(sp_w))
  sp_z   <- .adult_csd_z(sp_par, sp_sex, sp_w)
  sp_b   <- .adult_z3(sp_par, sp_sex)
  sp_uer <- .unit_error_range_flag(sp_par, sp_w, sp_b$zn3, sp_b$zp3)
  uer3 <- FALSE
  ug_i <- which(sp_uer)
  zg_i <- which(abs(sp_z) < 3)
  if (length(ug_i) >= 1L && length(zg_i) >= 1L &&
      (length(ug_i) + length(zg_i)) == length(sp_w) &&
      length(unique(sign(sp_z[ug_i]))) == 1L) {
    mu <- mean(sp_w[ug_i]); mz <- mean(sp_w[zg_i])
    rt <- max(mu, mz) / min(mu, mz)
    uer3 <- isTRUE(rt >= 1.7 && rt <= 2.7)
  }

  repeat {
    # Work with currently non-excluded values (initial order preserved on subset)
    working <- inc_df[!inc_df$internal_id %in% exc_ids, ]
    n <- nrow(working)
    if (n < 4L) break

    w    <- working$meas_m
    ages <- working$age_days
    iid  <- as.character(working$internal_id)
    sx   <- working$sex

    # Adjacent-pair etcaps (uw = max of the two weights). Length n-1; pair i
    # occupies positions (i, i+1). These caps serve the inner AND both outer
    # comparisons, so they are computed once and not recalculated per pair.
    months  <- diff(ages) / 30.4375
    wt_diff <- abs(diff(w))
    uw_pair <- pmax(w[-n], w[-1])
    etcap   <- compute_et_limit(months, formula = wtallow_formula, uw = uw_pair)

    # Anchor rule: pair i (B = pos i, C = pos i+1) is OTL iff A = pos i-1 and
    # D = pos i+2 both exist, |B-C| > etcap(B,C) + 0.12, |A-B| <= 0.60*etcap(A,B)
    # + 0.12, |C-D| <= 0.60*etcap(C,D) + 0.12. Both inner members are marked OTL.
    otl       <- logical(n)
    pos_etcap <- rep(NA_real_, n)   # inner-pair etcap recorded per OTL position
    for (i in seq_len(n - 1L)) {
      if (i >= 2L && i <= (n - 2L)) {
        inner   <- wt_diff[i]     >  etcap[i]     + 0.12
        anchorA <- wt_diff[i - 1L] <= 0.60 * etcap[i - 1L] + 0.12
        anchorD <- wt_diff[i + 1L] <= 0.60 * etcap[i + 1L] + 0.12
        if (isTRUE(inner) && isTRUE(anchorA) && isTRUE(anchorD)) {
          otl[i] <- TRUE; otl[i + 1L] <- TRUE
          pos_etcap[i]      <- max(pos_etcap[i],      etcap[i], na.rm = TRUE)
          pos_etcap[i + 1L] <- max(pos_etcap[i + 1L], etcap[i], na.rm = TRUE)
        }
      }
    }
    cand <- which(otl)
    if (length(cand) == 0L) break

    if (length(cand) == 1L) {
      worst <- cand
    } else {
      z <- .adult_csd_z(rep("WEIGHTKG", n), sx, w)
      # absd_med = distance from a windowed median (+/-4 positions), kg space.
      absd_med <- vapply(cand, function(j) {
        lo <- max(1L, j - 4L); hi <- min(n, j + 4L)
        abs(w[j] - median(w[lo:hi], na.rm = TRUE))
      }, numeric(1))

      if (uer3) {
        # Clean unit-error 2-group SP -> exclude the highest |z|.
        ord <- order(-abs(z[cand]), working$internal_id[cand])
        worst <- cand[ord[1L]]
      } else {
        mx <- max(absd_med)
        # adult median-band width = 0.20 * etcap(B,C) (NB: child uses an
        # absolute z-unit band; do NOT unify the two widths). The tight band keeps
        # the cluster small so absd_med is decisive more often.
        bw <- 0.20 * pos_etcap[cand[which.max(absd_med)]]
        in_band <- absd_med >= mx - bw
        if (sum(in_band) >= 2L) {
          # >= 2 bunched within the band: windowed median cannot discriminate,
          # so exclude the highest |z| among band members (tiebreak internal_id).
          b <- cand[in_band]
          ord <- order(-abs(z[b]), working$internal_id[b])
          worst <- b[ord[1L]]
        } else {
          # one candidate stands clearly apart: exclude highest absd_med
          # (tiebreak |z|, then internal_id).
          ord <- order(-absd_med, -abs(z[cand]), working$internal_id[cand])
          worst <- cand[ord[1L]]
        }
      }
    }

    # Exclude exactly one value per iteration.
    exc_ids <- c(exc_ids, iid[worst])
  }

  exc_ids
}

# Step 9Wb/11Wb: EWMA weight outlier removal ----

#' Remove extreme EWMA weight outliers (Adult Step 9Wb)
#'
#' Iteratively excludes values whose EWMA deviation exceeds an interval-specific
#' threshold (ET cap). Each round removes the single worst outlier (largest
#' |dewma|, with age then internal_id as tiebreakers). Iterates until no candidates
#' remain or <3 values.
#'
#' Threshold: ET cap derived from wtallow formula and upper weight (UW).
#' Per-observation threshold based on min neighbor gap and max(wt, ewma).
#'
#' 90% rule: directional dewma (before/after) must exceed 90% of threshold.
#' For edge values, the missing directional EWMA falls back to the overall EWMA,
#' so the 90% rule does not block exclusion; a missing neighbor gap is treated
#' as Inf when choosing the interval.
#'
#' @param subj_df Data frame with age_days, meas_m, internal_id columns (pre-sorted)
#' @param wtallow_formula Formula for ET cap computation
#' @param exc_label Exclusion code label assigned to excluded values.
#' @param ewma_window Maximum number of positions on each side included in the EWMA (default 15)
#' @return Named character vector: internal_id -> exc_label for excluded values
#' @keywords internal
remove_ewma_wt <- function(subj_df, wtallow_formula = "piecewise",
                           exc_label = "Exclude-A-Traj-Extreme",
                           ewma_window = 15) {
  orig_subj_df <- subj_df
  rem_ids <- character(0)
  round_codes <- character(0)
  change <- TRUE
  round_num <- 1
  cache <- NULL

  while (change) {
    n <- nrow(subj_df)
    if (n < 3) break

    # Minimum neighbor gap in months (NA -> Inf for edge values)
    agedays_bef <- c(NA, subj_df$age_days[-n])
    agedays_aft <- c(subj_df$age_days[-1], NA)
    gap_bef <- subj_df$age_days - agedays_bef
    gap_aft <- agedays_aft - subj_df$age_days
    gap_bef_check <- ifelse(is.na(gap_bef), Inf, gap_bef)
    gap_aft_check <- ifelse(is.na(gap_aft), Inf, gap_aft)
    min_gap_months <- pmin(gap_bef_check, gap_aft_check) / 30.4375

    # EWMA -- rebuilt each round (full init).
    cache <- adult_ewma_cache_init(subj_df$age_days, subj_df$meas_m,
                             ewma_window = ewma_window)
    dewma_all <- subj_df$meas_m - cache$ewma_all
    dewma_bef <- subj_df$meas_m - cache$ewma_before
    dewma_aft <- subj_df$meas_m - cache$ewma_after

    # Dynamic per-observation threshold: ET cap based on formula and UW
    uw_obs <- pmax(subj_df$meas_m, cache$ewma_all)
    threshold <- compute_et_limit(min_gap_months, formula = wtallow_formula,
                                  uw = uw_obs)

    # 90% rule: directional dewma must exceed 90% of threshold
    # Edge values: ewma_before/ewma_after fall back to ewma_all via
    # adult_ewma_cache_init(), so dewma_bef/dewma_aft are never NA.

    # Positive direction
    # 0.12 kg rounding tolerance on all threshold comparisons
    criteria_pos <- !is.na(dewma_all) & dewma_all > threshold + 0.12 &
                    dewma_bef > 0.9 * threshold + 0.12 &
                    dewma_aft > 0.9 * threshold + 0.12
    # Negative direction
    criteria_neg <- !is.na(dewma_all) & dewma_all < -(threshold + 0.12) &
                    dewma_bef < -(0.9 * threshold + 0.12) &
                    dewma_aft < -(0.9 * threshold + 0.12)

    criteria_new <- criteria_pos | criteria_neg

    if (all(!criteria_new)) {
      change <- FALSE
    } else {
      absdewma <- abs(dewma_all)
      cand_idx <- which(criteria_new)
      # internal_id tiebreaker for sort determinism
      ord <- order(-absdewma[cand_idx], subj_df$age_days[cand_idx],
                   subj_df$internal_id[cand_idx])
      to_rem <- cand_idx[ord[1]]

      rem_ids <- c(rem_ids, as.character(subj_df$internal_id[to_rem]))
      round_codes <- c(round_codes, exc_label)
      subj_df <- subj_df[-to_rem, ]
      round_num <- round_num + 1

      if (nrow(subj_df) < 3) {
        change <- FALSE
      }
    }
  }

  # Return named character vector
  result <- setNames(round_codes, rem_ids)
  result
}

# Linked mode: RV propagation ----

#' Propagate firstRV exclusions forward to RV copies and extraneous values
#' (linked mode). For each excluded value, finds all other values with the
#' same measurement that are still "Include" -- including extraneous values
#' whose is_rv flag may have been cleared by redo_identify_rv(). Marks them
#' with `"<code>-RV-Propagated"`. This is forward-only propagation (firstRV ->
#' later RVs + extraneous), unlike Step 4W's bidirectional scale-max propagation.
#' @param exc_codes Named character vector: internal_id -> exclusion code (from firstRV pass)
#' @param w_subj_df Current working weight data (must have meas_m, internal_id columns)
#' @param w_subj_keep Named vector of all exclusion codes for this subject's weights
#' @return List with updated w_subj_keep and propagated_ids
#' @keywords internal
propagate_to_rv <- function(exc_codes, w_subj_df, w_subj_keep) {
  propagated_ids <- character(0)
  if (length(exc_codes) == 0) {
    return(list(w_subj_keep = w_subj_keep, propagated_ids = propagated_ids))
  }

  for (exc_id in names(exc_codes)) {
    idx <- which(as.character(w_subj_df$internal_id) == exc_id)
    if (length(idx) == 0) next
    exc_meas <- w_subj_df$meas_m[idx[1]]

    # Find all values with same meas_m (RV copies + extraneous with same value)
    match_idx <- which(w_subj_df$meas_m == exc_meas &
                       as.character(w_subj_df$internal_id) != exc_id)
    if (length(match_idx) == 0) next

    match_ids <- as.character(w_subj_df$internal_id[match_idx])
    match_ids <- match_ids[w_subj_keep[match_ids] == "Include"]

    if (length(match_ids) > 0) {
      prop_code <- paste0(exc_codes[exc_id], "-RV-Propagated")
      w_subj_keep[match_ids] <- prop_code
      propagated_ids <- c(propagated_ids, match_ids)
    }
  }

  list(w_subj_keep = w_subj_keep, propagated_ids = propagated_ids)
}

# Step 11Wb: 7-step Moderate EWMA ----

#' Remove implausible weight values using 7-step moderate EWMA flow.
#' @param full_inc_df Data frame for one subject with columns:
#'   internal_id, age_days, ageyears, meas_m (weight in kg). Must have >= 3 rows.
#' @param exc_label Character prefix for exclusion codes
#' @param max_rounds Maximum number of rounds (default 100)
#' @param wtallow_formula Formula for wtallow ("piecewise", "piecewise-lower", "allofus15")
#' @param ewma_window Maximum number of positions on each side included in the EWMA (default 15)
#' @param mod_ewma_f Directional factor: both directional (before/after) deviations must exceed mod_ewma_f x wtallow (default 0.75)
#' @param perclimit_low,perclimit_mid,perclimit_high Percentage-limit settings passed to \code{compute_perc_limit()}
#' @param endpair_gap_days Max within-pair age gap (days) for the end-pair median guard (default 90).
#' @return Named character vector: internal_id -> exclusion code for excluded values.
#' @keywords internal
remove_mod_ewma_wt <- function(full_inc_df, exc_label = "Exclude-A-Traj-Moderate",
                               max_rounds = 100, wtallow_formula = "piecewise",
                               ewma_window = 15,
                               mod_ewma_f = 0.75,
                               perclimit_low = 0.5, perclimit_mid = 0.4,
                               perclimit_high = 0.0,
                               endpair_gap_days = 90) {
  inc_df <- full_inc_df[order(full_inc_df$ageyears, full_inc_df$internal_id), ]
  exclusions <- character(0)

  for (round_num in seq_len(max_rounds)) {
    n <- nrow(inc_df)
    if (n < 3) break

    ids   <- as.character(inc_df$internal_id)
    meas  <- inc_df$meas_m
    adays <- inc_df$age_days
    ayrs  <- inc_df$ageyears

    # Age differences to neighbors (in years)
    ageyrs_bef <- c(Inf, diff(ayrs))
    ageyrs_aft <- c(diff(ayrs), Inf)
    agedays_bef <- round(ageyrs_bef * 365.25)
    agedays_aft <- round(ageyrs_aft * 365.25)

    # Minimum adjacent age diff (years), floored at 0
    minagediff <- pmin(ageyrs_bef, ageyrs_aft)
    minagediff[minagediff < 0] <- 0
    months <- minagediff * 12

    # Weight differences to neighbors
    wt_bef <- c(NA, diff(meas))
    wt_aft <- c(diff(meas), NA)

    # EWMA -- rebuilt each round (full init).
    cache <- adult_ewma_cache_init(adays, meas, ewma_window = ewma_window)
    dewma_all <- meas - cache$ewma_all
    dewma_bef <- meas - cache$ewma_before
    dewma_aft <- meas - cache$ewma_after

    # wtallow: UW-adjusted and base (for prioritization scoring)
    uw_obs <- pmax(meas, cache$ewma_all)
    wta <- compute_wtallow(months, formula = wtallow_formula, uw = uw_obs)
    wta_base <- compute_wtallow(months, formula = wtallow_formula, uw = NULL)

    # Observation-level perclimit (each observation's perclimit is based on
    # its own weight). This differs from 11Wa which uses subject-level (max).
    perc_limit <- compute_perc_limit(meas, perclimit_low, perclimit_mid, perclimit_high)

    sn <- seq_len(n)

    # Trajectory rescue
    traj_fails <- compute_trajectory_fails(meas, adays)

    # === STEP 1: Standard pathway + trajectory rescue ===
    # 0.12 kg rounding tolerance on all threshold comparisons
    f <- mod_ewma_f
    std_exc <- (dewma_all > wta + 0.12 & dewma_bef > (f * wta) + 0.12 & dewma_aft > (f * wta) + 0.12) |
               (dewma_all < -(wta + 0.12) & dewma_bef < -((f * wta) + 0.12) & dewma_aft < -((f * wta) + 0.12))
    std_exc[is.na(std_exc)] <- FALSE

    exc_stand <- std_exc & traj_fails

    # Initialize accumulators
    exc_wt_i <- rep(FALSE, n)
    error_load <- rep(FALSE, n)

    # === STEP 2: Standard run detection + pair scoring ===
    runs <- detect_runs(exc_stand)

    # 4+ runs -> error load
    is_4plus <- !is.na(runs$run_len) & runs$run_len >= 4 & exc_stand
    error_load[is_4plus] <- TRUE
    exc_wt_i[is_4plus] <- TRUE

    # Isolated (run_len == 1): directly exc_wt_i
    is_isolated <- !is.na(runs$run_len) & runs$run_len == 1 & !error_load
    exc_wt_i[is_isolated] <- TRUE

    # Pairs/trios (run_len 2-3): score
    is_pair_trio <- !is.na(runs$run_len) & runs$run_len >= 2 &
                    runs$run_len <= 3 & !error_load & exc_stand

    if (any(is_pair_trio)) {
      # wtallow_unrel: use gap to non-pair-member
      p2_age <- c(NA, NA, adays[1:(n - 2)])
      n2_age <- c(adays[3:n], NA, NA)

      # When before neighbor is a pair member, use gap to p2
      d_bef_rm <- (adays - p2_age) / 365.25
      d_bef_keep <- ageyrs_aft
      minadiff_bef_rm <- pmin(d_bef_rm, d_bef_keep, na.rm = TRUE)
      minadiff_bef_rm[is.na(d_bef_rm) & is.na(d_bef_keep)] <- NA
      minadiff_bef_rm[is.na(d_bef_rm)] <- d_bef_keep[is.na(d_bef_rm)]
      minadiff_bef_rm[is.na(d_bef_keep)] <- d_bef_rm[is.na(d_bef_keep)]
      wta_bef_unrel <- compute_wtallow(minadiff_bef_rm * 12, formula = wtallow_formula,
                                     uw = uw_obs)

      # When after neighbor is a pair member, use gap to n2
      d_aft_rm <- (n2_age - adays) / 365.25
      d_aft_keep <- ageyrs_bef
      minadiff_aft_rm <- pmin(d_aft_keep, d_aft_rm, na.rm = TRUE)
      minadiff_aft_rm[is.na(d_aft_keep) & is.na(d_aft_rm)] <- NA
      minadiff_aft_rm[is.na(d_aft_keep)] <- d_aft_rm[is.na(d_aft_keep)]
      minadiff_aft_rm[is.na(d_aft_rm)] <- d_aft_keep[is.na(d_aft_rm)]
      wta_aft_unrel <- compute_wtallow(minadiff_aft_rm * 12, formula = wtallow_formula,
                                     uw = uw_obs)

      score_std <- rep(NA_real_, n)
      is_first <- is_pair_trio & runs$run_pos == 1
      score_std[is_first] <- abs(dewma_aft[is_first] / wta_aft_unrel[is_first])
      is_last <- is_pair_trio & runs$run_pos == runs$run_len
      score_std[is_last] <- abs(dewma_bef[is_last] / wta_bef_unrel[is_last])
      is_mid <- is_pair_trio & runs$run_pos > 1 & runs$run_pos < runs$run_len
      score_std[is_mid] <- abs(dewma_bef[is_mid] / wta_bef_unrel[is_mid] +
                               dewma_aft[is_mid] / wta_aft_unrel[is_mid])

      unique_runs <- unique(runs$run_id[is_pair_trio])
      for (rid in unique_runs) {
        in_run <- which(is_pair_trio & runs$run_id == rid)
        if (length(in_run) > 0) {
          best <- in_run[which.max(score_std[in_run])]
          exc_wt_i[best] <- TRUE
        }
      }
    }

    # === STEP 3: Alternate pathway ===
    # 0.12 kg rounding tolerance on all threshold comparisons
    alt_exc <- rep(FALSE, n)
    prior_unrel <- agedays_bef <= 14 & !is.na(wt_bef) & abs(wt_bef) > wta + 0.12
    alt_exc[prior_unrel] <-
      (dewma_all[prior_unrel] > wta[prior_unrel] + 0.12 &
       dewma_aft[prior_unrel] > (f * wta[prior_unrel]) + 0.12) |
      (dewma_all[prior_unrel] < -(wta[prior_unrel] + 0.12) &
       dewma_aft[prior_unrel] < -((f * wta[prior_unrel]) + 0.12))
    next_unrel <- agedays_aft <= 14 & !is.na(wt_aft) & abs(wt_aft) > wta + 0.12
    alt_exc[next_unrel] <- alt_exc[next_unrel] |
      (dewma_all[next_unrel] > wta[next_unrel] + 0.12 &
       dewma_bef[next_unrel] > (f * wta[next_unrel]) + 0.12) |
      (dewma_all[next_unrel] < -(wta[next_unrel] + 0.12) &
       dewma_bef[next_unrel] < -((f * wta[next_unrel]) + 0.12))
    alt_exc[is.na(alt_exc)] <- FALSE

    exc_pair <- alt_exc & !exc_wt_i

    # === STEP 4: Alternate run detection + pair scoring ===
    alt_runs <- detect_runs(exc_pair)

    is_alt_isolated <- !is.na(alt_runs$run_len) & alt_runs$run_len == 1 &
                       exc_pair & !error_load
    exc_wt_i[is_alt_isolated] <- TRUE

    is_alt_pt <- !is.na(alt_runs$run_len) & alt_runs$run_len >= 2 &
                 alt_runs$run_len <= 3 & !error_load & exc_pair

    if (any(is_alt_pt)) {
      p2_age <- c(NA, NA, adays[1:(n - 2)])
      n2_age <- c(adays[3:n], NA, NA)

      d_bef_rm2 <- (adays - p2_age) / 365.25
      d_bef_keep2 <- ageyrs_aft
      minadiff_bef2 <- pmin(d_bef_rm2, d_bef_keep2, na.rm = TRUE)
      minadiff_bef2[is.na(d_bef_rm2) & is.na(d_bef_keep2)] <- NA
      minadiff_bef2[is.na(d_bef_rm2)] <- d_bef_keep2[is.na(d_bef_rm2)]
      minadiff_bef2[is.na(d_bef_keep2)] <- d_bef_rm2[is.na(d_bef_keep2)]
      wta_bef_unrel2 <- compute_wtallow(minadiff_bef2 * 12, formula = wtallow_formula,
                                      uw = uw_obs)

      d_aft_rm2 <- (n2_age - adays) / 365.25
      d_aft_keep2 <- ageyrs_bef
      minadiff_aft2 <- pmin(d_aft_keep2, d_aft_rm2, na.rm = TRUE)
      minadiff_aft2[is.na(d_aft_keep2) & is.na(d_aft_rm2)] <- NA
      minadiff_aft2[is.na(d_aft_keep2)] <- d_aft_rm2[is.na(d_aft_keep2)]
      minadiff_aft2[is.na(d_aft_rm2)] <- d_aft_keep2[is.na(d_aft_rm2)]
      wta_aft_unrel2 <- compute_wtallow(minadiff_aft2 * 12, formula = wtallow_formula,
                                      uw = uw_obs)

      score_alt <- rep(NA_real_, n)
      prior_committed <- c(FALSE, exc_wt_i[-n] & !exc_pair[-n])
      next_committed  <- c(exc_wt_i[-1] & !exc_pair[-1], FALSE)

      eff_first  <- is_alt_pt & alt_runs$run_pos == 1 & !prior_committed
      eff_last   <- is_alt_pt & alt_runs$run_pos == alt_runs$run_len & !next_committed
      eff_middle <- is_alt_pt & !eff_first & !eff_last

      score_alt[eff_first]  <- abs(dewma_aft[eff_first] / wta_aft_unrel2[eff_first])
      score_alt[eff_last]   <- abs(dewma_bef[eff_last] / wta_bef_unrel2[eff_last])
      score_alt[eff_middle] <- abs(dewma_bef[eff_middle] / wta_bef_unrel2[eff_middle] +
                                   dewma_aft[eff_middle] / wta_aft_unrel2[eff_middle])

      unique_alt_runs <- unique(alt_runs$run_id[is_alt_pt])
      for (rid in unique_alt_runs) {
        in_run <- which(is_alt_pt & alt_runs$run_id == rid)
        if (length(in_run) > 0) {
          best <- in_run[which.max(score_alt[in_run])]
          exc_wt_i[best] <- TRUE
        }
      }
    }

    # === STEP 5: Percentage criterion ===
    percewma     <- meas / cache$ewma_all
    percewma_bef <- meas / cache$ewma_before
    percewma_aft <- meas / cache$ewma_after

    perc_flag <- percewma < perc_limit & percewma_bef < perc_limit &
                 percewma_aft < perc_limit & !exc_wt_i
    perc_flag[is.na(perc_flag)] <- FALSE
    exc_wt_i[perc_flag] <- TRUE

    # === STEP 6: 4+ consecutive exc_wt_i -> error load ===
    consec_runs <- detect_runs(exc_wt_i)
    new_error_load <- !is.na(consec_runs$run_len) & consec_runs$run_len >= 4 & exc_wt_i
    error_load[new_error_load] <- TRUE

    # === STEP 7: Prioritized exclusion ===

    # Error load: all excluded immediately
    el_ids <- ids[error_load]
    if (length(el_ids) > 0) {
      el_code <- paste0(exc_label, "-Error-Load")
      exclusions[el_ids] <- el_code
    }

    # Non-error-load candidates
    candidates <- exc_wt_i & !error_load

    # === End-pair median guard ===
    # When both members of an end pair (first/second or penult/last) are
    # independently eligible for moderate-EWMA exclusion, each member's EWMA
    # reference is dominated by its divergent partner, so the run/score logic can
    # exclude the in-line member. Decide which member to keep using a clean local
    # reference -- the median of up to 3 interior-side values -- instead of the
    # partner-contaminated EWMA. The member closer to that median is protected
    # this round; the other is forced into the candidate set as the nominee
    # (overriding the pair-scoring collapse). This selects WHICH member is the
    # nominee only; the Step 7 scoring below still decides WHETHER it is excluded.
    # Trigger: n >= 4, both members eligible, the interior-side neighbor NOT
    # eligible (so a 3+ run defers to the run logic), within-pair age gap <
    # endpair_gap_days. Median window is up to 3 interior-side values; for n = 4
    # only 2 are available (last-pair: positions 1..2; first-pair: positions 3..4).
    if (n >= 4) {
      eligible <- (exc_stand | alt_exc | perc_flag) & !error_load

      # penult/last pair (indices n-1, n); window max(1, n-4)..n-2; interior nbr n-2.
      if (eligible[n - 1] && eligible[n] && !eligible[n - 2] &&
          (adays[n] - adays[n - 1]) < endpair_gap_days) {
        m_L <- median(meas[max(1L, n - 4L):(n - 2L)])
        # Closer-to-median is kept; an exact tie keeps the edge value (last).
        if (abs(meas[n - 1] - m_L) < abs(meas[n] - m_L)) {
          keep_i <- n - 1L; nom_i <- n
        } else {
          keep_i <- n;      nom_i <- n - 1L
        }
        candidates[keep_i] <- FALSE
        candidates[nom_i]  <- TRUE
      }

      # first/second pair (indices 1, 2); window 3..min(n, 5); interior nbr 3.
      if (eligible[1] && eligible[2] && !eligible[3] &&
          (adays[2] - adays[1]) < endpair_gap_days) {
        m_F <- median(meas[3L:min(n, 5L)])
        # Closer-to-median is kept; an exact tie keeps the edge value (first).
        if (abs(meas[2] - m_F) < abs(meas[1] - m_F)) {
          keep_i <- 2L; nom_i <- 1L
        } else {
          keep_i <- 1L; nom_i <- 2L
        }
        candidates[keep_i] <- FALSE
        candidates[nom_i]  <- TRUE
      }
    }

    if (!any(candidates)) {
      if (any(error_load)) {
        inc_df <- inc_df[!error_load, ]
      } else {
        break
      }
      next
    }

    # Score each candidate: (|dewma| - adj_wta) / base_wta
    # This prevents erroneous UW inflation from distorting the score.
    # adj_wta (wta) absorbs UW; base_wta (wta_base) is UW-independent.
    score_final <- rep(NA_real_, n)

    # Edge values
    is_edge_first <- candidates & sn == 1
    is_edge_last  <- candidates & sn == n
    score_final[is_edge_first] <- pmax(0, abs(dewma_all[is_edge_first]) -
                                       wta[is_edge_first]) / wta_base[is_edge_first]
    score_final[is_edge_last]  <- pmax(0, abs(dewma_all[is_edge_last]) -
                                       wta[is_edge_last]) / wta_base[is_edge_last]

    # Interior values: graduated multiplier
    is_middle <- candidates & sn > 1 & sn < n
    if (any(is_middle)) {
      excess_bef <- pmax(0, abs(dewma_bef[is_middle]) -
                         wta[is_middle]) / wta_base[is_middle]
      excess_aft <- pmax(0, abs(dewma_aft[is_middle]) -
                         wta[is_middle]) / wta_base[is_middle]
      min_excess <- pmin(excess_bef, excess_aft)
      multiplier <- pmin(1.0, 0.6 + 0.4 * min_excess)
      score_final[is_middle] <- multiplier * (excess_bef + excess_aft)
    }

    # Tiebreakers: smallest adj_wta -> closest to median age -> earliest position
    median_age <- median(adays)
    absdiff_median <- abs(adays - median_age)

    cand_idx <- which(candidates)
    ord <- order(-score_final[cand_idx], wta[cand_idx],
                 absdiff_median[cand_idx], sn[cand_idx],
                 as.numeric(ids[cand_idx]))
    best_idx <- cand_idx[ord[1]]

    exclusions[ids[best_idx]] <- exc_label

    # Remove excluded + error load
    to_remove <- error_load
    to_remove[best_idx] <- TRUE
    inc_df <- inc_df[!to_remove, ]
  }

  exclusions
}

# Step 11Wa2: 2D Non-Ordered Pairs ----

#' Evaluate 2D non-ordered weight pairs for one subject
#' @param w_subj_df Weight data with meas_m, age_days, internal_id. Already filtered to Inc.
#' @param w_subj_keep Named vector of current exclusion codes for all weight obs
#' @param wtallow_formula Formula for wtallow
#' @return Character vector of internal_ids to exclude with code Exclude-A-2D-Non-Ordered
#' @keywords internal
eval_2d_nonord <- function(w_subj_df, w_subj_keep, wtallow_formula = "piecewise") {
  exc_ids <- character(0)
  if (nrow(w_subj_df) < 2) return(exc_ids)

  vals <- w_subj_df$meas_m
  ids <- as.character(w_subj_df$internal_id)

  # Check: exactly 2 distinct values
  uvals <- unique(vals)
  if (length(uvals) != 2) return(exc_ids)

  # Check: NOT time-ordered (interleaved)
  v1_ages <- w_subj_df$age_days[vals == uvals[1]]
  v2_ages <- w_subj_df$age_days[vals == uvals[2]]
  # Time-ordered if all of one value come before all of the other
  if (max(v1_ages) < min(v2_ages) || max(v2_ages) < min(v1_ages)) {
    return(exc_ids)  # This is ordered -> handled by 2D Ord
  }

  # Compute wtallow for each adjacent pair
  w_sorted <- w_subj_df[order(w_subj_df$age_days, w_subj_df$internal_id), ]
  n <- nrow(w_sorted)
  any_outside <- FALSE

  for (i in 1:(n - 1)) {
    if (w_sorted$meas_m[i] != w_sorted$meas_m[i + 1]) {
      age_diff_months <- abs(w_sorted$age_days[i + 1] - w_sorted$age_days[i]) / 30.4375
      uw_pair <- max(w_sorted$meas_m[i], w_sorted$meas_m[i + 1])
      wta <- compute_wtallow(age_diff_months, formula = wtallow_formula,
                             uw = uw_pair)
      if (abs(w_sorted$meas_m[i + 1] - w_sorted$meas_m[i]) > wta + 0.12) {
        any_outside <- TRUE
        break
      }
    }
  }

  if (!any_outside) return(exc_ids)  # Rule 1: all within wtallow -> keep all

  # Check for prior non-SDE exclusions
  # SDE codes are Identical and Extraneous (ht or wt); all others count as non-SDE.
  # Use exact matching to avoid grepl("Identical") accidentally matching
  # "Exclude-A-Scale-Max-Identical", which is a non-SDE exclusion.
  all_wt_ids <- names(w_subj_keep)
  sde_codes <- c("Exclude-A-Identical", "Exclude-A-Extraneous")
  prior_nonSDE <- any(
    w_subj_keep[all_wt_ids] != "Include" &
    !w_subj_keep[all_wt_ids] %in% sde_codes &
    w_subj_keep[all_wt_ids] != "" &
    !is.na(w_subj_keep[all_wt_ids])
  )

  if (prior_nonSDE) {
    # Rule 2: Outside wtallow + prior non-SDE exclusions -> all excluded
    return(ids)
  }

  # Check dominance
  count_v1 <- sum(vals == uvals[1])
  count_v2 <- sum(vals == uvals[2])
  total <- count_v1 + count_v2
  dominant_pct <- max(count_v1, count_v2) / total

  if (dominant_pct > 0.65) {
    # Rule 3: dominant > 65% -> exclude minority
    minority_val <- if (count_v1 > count_v2) uvals[2] else uvals[1]
    exc_ids <- ids[vals == minority_val]
  } else {
    # Rule 4: each <= 65% -> all excluded
    exc_ids <- ids
  }

  exc_ids
}

# Step 13: 1D Evaluation ----

#' Evaluate single distinct value observations
#' @param subj_results data.table with result, meas_m, param, ageyears, internal_id columns
#'   for ONE subject -- all params included
#' @param params List of limit parameters
#' @return Character vector of internal_ids to mark Exclude-A-Single
#' @keywords internal
eval_1d <- function(subj_results, params) {
  # Two-pass: (1) evaluate with BMI, (2) re-evaluate if BMI lost due to pass 1 exclusion

  exc_ids <- character(0)

  for (pass in 1:2) {
    # Get currently included heights and weights (excluding pass-1 exclusions)
    ht_inc <- subj_results[subj_results$param %in% c("HEIGHTCM", "HEIGHTIN") &
                           subj_results$result == "Include" &
                           !subj_results$internal_id %in% exc_ids, ]
    wt_inc <- subj_results[subj_results$param %in% c("WEIGHTKG", "WEIGHTLBS") &
                           subj_results$result == "Include" &
                           !subj_results$internal_id %in% exc_ids, ]

    ht_1d <- length(unique(ht_inc$meas_m)) == 1 & nrow(ht_inc) > 0
    wt_1d <- length(unique(wt_inc$meas_m)) == 1 & nrow(wt_inc) > 0

    if (!ht_1d && !wt_1d) next

    # Check BMI availability
    ht_days <- if (nrow(ht_inc) > 0) unique(ht_inc$age_days) else integer(0)
    wt_days <- if (nrow(wt_inc) > 0) unique(wt_inc$age_days) else integer(0)
    bmi_days <- intersect(ht_days, wt_days)
    has_bmi <- length(bmi_days) > 0

    if (pass == 1 && has_bmi) {
      # Pass 1: evaluate with BMI. Form a BMI for EVERY same-day pairing of an
      # included height with an included weight; the single-value parameter is
      # protected if ANY of those BMIs is in range (not just the first pair).
      bmi_pairs <- merge(
        data.frame(age_days = ht_inc$age_days, ht_val = ht_inc$meas_m)[
          ht_inc$age_days %in% bmi_days, , drop = FALSE],
        data.frame(age_days = wt_inc$age_days, wt_val = wt_inc$meas_m)[
          wt_inc$age_days %in% bmi_days, , drop = FALSE],
        by = "age_days")
      bmi <- bmi_pairs$wt_val / ((bmi_pairs$ht_val / 100)^2)

      bmi_extreme <- !any(bmi >= params$bmi_min & bmi <= params$bmi_max)

      if (bmi_extreme) {
        if (ht_1d) exc_ids <- c(exc_ids, as.character(ht_inc$internal_id))
        if (wt_1d) exc_ids <- c(exc_ids, as.character(wt_inc$internal_id))
      } else {
        # 0.12 tolerance for rounding (0.1 cm/kg rounding + float precision)
        if (ht_1d) {
          ht_val_check <- unique(ht_inc$meas_m)
          if (ht_val_check < params$ht_min_bmi - 0.12 | ht_val_check > params$ht_max_bmi + 0.12) {
            exc_ids <- c(exc_ids, as.character(ht_inc$internal_id))
          }
        }
        if (wt_1d) {
          wt_val_check <- unique(wt_inc$meas_m)
          if (wt_val_check < params$wt_min_bmi - 0.12 | wt_val_check > params$wt_max_bmi + 0.12) {
            exc_ids <- c(exc_ids, as.character(wt_inc$internal_id))
          }
        }
      }
    } else if ((pass == 1 && !has_bmi) || (pass == 2 && !has_bmi)) {
      # No BMI: use tighter limits (0.12 tolerance)
      if (ht_1d) {
        ht_val_check <- unique(ht_inc$meas_m)
        if (ht_val_check < params$ht_min_nobmi - 0.12 | ht_val_check > params$ht_max_nobmi + 0.12) {
          exc_ids <- c(exc_ids, as.character(ht_inc$internal_id))
        }
      }
      if (wt_1d) {
        wt_val_check <- unique(wt_inc$meas_m)
        if (wt_val_check < params$wt_min_nobmi - 0.12 | wt_val_check > params$wt_max_nobmi + 0.12) {
          exc_ids <- c(exc_ids, as.character(wt_inc$internal_id))
        }
      }
    }
    # Pass 2 with BMI: nothing new to check (already evaluated with BMI in pass 1)
  }

  exc_ids
}

# Step 14: Error Load ----

#' Evaluate error load for one subject
#' @param subj_results data.table with result, param columns for ONE subject
#' @param error_threshold Threshold for error ratio (default 0.41)
#' @return Character vector of internal_ids to mark Exclude-A-Unevaluable-Trajectory
#' @keywords internal
eval_error_load <- function(subj_results, error_threshold = 0.41) {
  exc_ids <- character(0)

  # Process height and weight separately
  for (p in c("ht", "wt")) {
    if (p == "ht") {
      p_rows <- subj_results[subj_results$param %in% c("HEIGHTCM", "HEIGHTIN"), ]
    } else {
      p_rows <- subj_results[subj_results$param %in% c("WEIGHTKG", "WEIGHTLBS"), ]
    }
    if (nrow(p_rows) == 0) next

    # Exclude SDEs from denominator
    sde_codes <- c("Exclude-A-Identical", "Exclude-A-Extraneous")
    non_sde <- p_rows[!p_rows$result %in% sde_codes, ]
    if (nrow(non_sde) == 0) next

    # Count errors
    error_codes_ht <- c("Exclude-A-PIV",
                        "Exclude-A-Single",
                        "Exclude-A-Ord-Pair",
                        "Exclude-A-Ord-Pair-All",
                        "Exclude-A-Window",
                        "Exclude-A-Window-All")
    error_codes_wt <- c("Exclude-A-PIV",
                        "Exclude-A-Single",
                        "Exclude-A-2D-Ordered",
                        "Exclude-A-2D-Non-Ordered",
                        "Exclude-A-Scale-Max",
                        "Exclude-A-Scale-Max-Identical",
                        "Exclude-A-Scale-Max-RV-Propagated",
                        "Exclude-A-Evil-Twins")

    # EWMA-RV-Propagated codes don't count as errors (same underlying error as source).
    # Scale-Max-RV-Propagated DOES count.
    is_ewma_propagated <- grepl("^Exclude-A-Traj.*-RV-Propagated$", non_sde$result, ignore.case = TRUE)
    is_ewma_error <- grepl("^Exclude-A-Traj", non_sde$result) & !is_ewma_propagated
    if (p == "ht") {
      n_errors <- sum((non_sde$result %in% error_codes_ht | is_ewma_error) & !is_ewma_propagated)
    } else {
      n_errors <- sum((non_sde$result %in% error_codes_wt | is_ewma_error) & !is_ewma_propagated)
    }

    n_inc <- sum(non_sde$result == "Include")
    denom <- n_errors + n_inc
    if (denom < 3) next

    ratio <- n_errors / denom
    if (ratio > error_threshold) {
      # Exclude all remaining Include values
      inc_ids <- as.character(non_sde$internal_id[non_sde$result == "Include"])
      exc_ids <- c(exc_ids, inc_ids)
    }
  }

  exc_ids
}
