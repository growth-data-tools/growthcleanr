# Register symbols used through data.table non-standard evaluation.
#
# The names below are data.table column names referenced inside `dt[...]`
# expressions, the `.()` alias, and `..`-prefixed column selectors. They are
# not undefined globals, but R CMD check's static analysis cannot see that they
# are bound as columns at runtime. Declaring them here suppresses the spurious
# "no visible binding for global variable" / "no visible global function
# definition" NOTEs without changing any behavior.

utils::globalVariables(c(
  ".",
  "..checkpoint_cols", "..drop_cols", "..ewma_merge_cols", "..return_cols",
  "HC_dop_med", "HT_dop_med", "WT_dop_med", "N",
  "absdewma", "absdiff_dop_med", "agedays", "all_identical", "batch_df",
  "bin_exclude", "bin_result", "cf_binary", "cf_deltaz", "cf_nextz", "cf_rescued",
  "cf_string_num", "ctbc.sd", "diff_after", "diff_before", "dup_count", "ewma_fill",
  "exclude", "exclude_detailed", "exp_val", "fengadays_subj", "first_meas", "fw_wt_z",
  "ga_days", "gc_internal_id", "gc_internal_sex", "gc_internal_meas", "gc_internal_param", "gc_internal_agedays", "agedays_input", "meas_input", "had_ewma1_before", "had_ewma2_before", "has_dup", "has_dup_vals",
  "has_new_excl", "has_sde_day", "has_sde_subj", "has_violation",
  "i.cf", "i.max.ht.vel", "i.min.ht.vel", "i.potcorr", "i.prior_ageday", "i.sd.corr",
  "i.sd.corr_minus", "i.sd.corr_plus", "i.single_val", "i.uncorr", "id", "id_sort",
  "internal_id", "is_far_sde", "is_first", "is_multi", "keep_id", "keep_id_dup",
  "keep_id_ewma", "keep_id_oneday", "max.whoinc.2.hc", "max.whoinc.3.hc", "max.whoinc.4.hc",
  "max.whoinc.6.hc", "max_abs_tbc", "mean_ht", "median_tbc", "min_absdewma", "n",
  "n_available", "n_birth", "n_days_with_data", "n_far_sde", "n_include", "n_on_day",
  "n_same_value", "n_tot", "n_unique_vals", "nextcf", "one_day_sde_flag", "orig_ageday",
  "orig_row", "originator", "originator_seq", "originator_z", "param", "prior_ageday",
  "prior_single_val", "sde_exclude", "sde_this", "sex", "sp_key", "sp_n", "sp_prov_med",
  "spa_ewma", "subjid", "tbc.sd", "tbc_range", "tiebreaker_ewma", "tiebreaker_oneday",
  "uncorr", "was_temp_sde", "whoagegrp.hc", "whoinc.2.hc", "whoinc.3.hc", "whoinc.4.hc",
  "whoinc.6.hc",
  "absdiff_dop_for_sort", "absdiff_rel_to_median", "exp_vals", "median.dopz.calc",
  "min_absdiff_rel_to_median", "val_excl_code",
  # Child Step 13 corrected-frame SDE-EWMA columns
  "c.ewma.all", "c_ewma_fill", "c.spa_ewma", "c.absdewma", "c.min_absdewma", "corr_date",
  # recumbent/standing offset (z-score-only) and the Child Step 2b GA window
  "lin_off", "ga_win_adj", "i.lin_off", "i.p_plus", "i.p_minus",
  "i.sd.orig_who", "i.sd.orig_cdc", "i.sd.orig",
  # Child Step 7 BMI hard limits (same-day WT x HT pairs)
  "absz", "absz.w", "absz.h", "bmi", "bmi_bad", "w_has_ok", "h_has_ok",
  "internal_id.w", "internal_id.h", "v.w", "v.h",
  # reshape adapters (semi-wide / wide input, re-widened output)
  ".a", ".block", ".cm_block", ".cm_param", ".cm_pt_row", ".cm_user_col",
  ".cm_visit_id", ".pt_row", ".prank", ".src_col", ".subj_order", ".user_id",
  ".visit_id", "bad", "block", "dob", "measurement", "pt_row", "user_col",
  "visit_date", "visit_id"
))

#' @importFrom stats setNames
NULL

# Package-internal cache environment. Used to memoize custom wtallow formula
# files (see adult_support.R) without writing to the user's global environment.
.gc_cache <- new.env(parent = emptyenv())
