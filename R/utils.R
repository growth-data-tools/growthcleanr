#' Recode binary sex variable for compatibility
#'
#' \code{recode_sex} recodes a binary sex variable for a given source column in a data frame or data table.
#' Only exact matches of \code{sourcem}/\code{sourcef} are recoded; any other value (e.g. "M"/"F") becomes NA in \code{targetcol} without a warning. \code{ext_bmiz()} reads only the column named \code{sex}, so to prepare data for it use \code{targetcol = "sex"} (with \code{targetm = "M"}, \code{targetf = "F"} if the data might have only one sex, since \code{ext_bmiz()} accepts numeric sex only when both 1 and 2 are present). Not needed between \code{cleangrowth()} and \code{ext_bmiz()}: \code{longwide()} already outputs sex as M or F.
#'
#' @param input_data a data frame or data table to be transformed. Expects a source column containing a binary sex variable.
#' @param sourcecol name of sex descriptor column. Defaults to "sex"
#' @param sourcem variable indicating "male" sex in input data. Defaults to "0"
#' @param sourcef variable indicating "female" sex in input data. Defaults to "1"
#' @param targetcol desired name of recoded sex descriptor column. Defaults to "sex_recoded"
#' @param targetm value to use for "male" sex in \code{targetcol}. Defaults to 1
#' @param targetf value to use for "female" sex in \code{targetcol}. Defaults to 2
#'
#' @return Returns a data table with recoded sex variables.
#'
#' @export
#' @examples
#' # Run on given data
#' df <- as.data.frame(syngrowth)
#'
#' # Run with all defaults
#' df_r <- recode_sex(df)
#'
#' # Specify different targets
#' df_rt <- recode_sex(df, targetcol = "sexr", targetm = "Male", targetf = "Female")
#'
#' # Specify different inputs
#' df_ri <- recode_sex(df_rt, sourcecol = "sexr", sourcem = "Male", sourcef = "Female")
recode_sex <- function(input_data,
                       sourcecol = "sex",
                       sourcem = "0",
                       sourcef = "1",
                       targetcol = "sex_recoded",
                       targetm = 1L,
                       targetf = 2L) {
  # cast to DT for faster processing
  input_table <- data.table(input_data)
  # match against the source values before writing anything, so a target column
  # that is also the source column is not recoded twice
  source_vals <- input_table[[sourcecol]]
  recoded <- rep(c(targetm, targetf)[NA_integer_], length(source_vals))
  recoded[which(source_vals == sourcem)] <- targetm
  recoded[which(source_vals == sourcef)] <- targetf
  set(input_table, j = targetcol, value = recoded)

  #return table
  return(input_table)
}


#' Transform data in growthcleanr format into wide structure for BMI calculation
#'
#' \code{longwide} transforms data from long to wide format. Ideal for
#' transforming output from growthcleanr::cleangrowth() into a format suitable
#' for growthcleanr::ext_bmiz().
#'
#' @param long_df A data frame (or data.table) to be transformed. Expects
#' columns: id, subjid, sex, agedays, param, measurement, and an exclusion
#' column from \code{cleangrowth()} (named \code{"exclude"} by default; the
#' \code{cleangrowth()} output \code{data.table} can be passed directly).
#' @param id name of observation ID column
#' @param subjid name of subject ID column
#' @param sex name of sex descriptor column
#' @param agedays name of age (in days) descriptor column
#' @param param name of parameter column to identify each type of measurement
#' @param measurement name of measurement column containing the actual
#' measurement data
#' @param gcr_result name of column of results from growthcleanr::cleangrowth().
#' Defaults to \code{"exclude"}, the name of the exclusion column in
#' \code{cleangrowth()} output. If your exclusion column has a different name,
#' pass it explicitly. If the named column is missing and the default
#' \code{"exclude"} was used, the error says so and suggests
#' \code{gcr_result = "gcr_result"} (the column name 2.x scripts used).
#' @param include_all Determines whether the function keeps all exclusion codes.
#' If TRUE, all exclusion types are kept and the inclusion_types argument is
#' ignored. Defaults to FALSE.
#' @param inclusion_types Vector indicating which exclusion codes from the
#' cleaning algorithm should be included in the data, given that include_all is
#' FALSE. For all options, see growthcleanr::cleangrowth(). Defaults to
#' c("Include"). A value that is not a growthcleanr version 3 exclusion code (for
#' example the 2.x code \code{"Exclude-Carried-Forward"}) matches no rows, so
#' \code{longwide()} warns and points to the code mapping in
#' \code{vignette("experienced-start-here")}.
#' @param extra_cols Vector of additional columns to include in the output. If
#' a column C1 differs on agedays matched height and weight values, then include
#' separate ht_C1 and wt_C1 columns as well as a match_C1 column that gives
#' booleans indicating where ht_C1 and wt_C1 are the same. If the agedays
#' matched height and weight columns are identical, then only include a single
#' version of C1. Defaults to empty vector (not keeping any additional columns).
#' @param keep_unmatched_data boolean indicating whether to keep height/weight
#' observations that do not have a matching weight/height on that day
#' @param sex_coding Named list with elements \code{male} and \code{female}
#' giving the sex codes used in \code{long_df}, exactly as for
#' \code{cleangrowth()}. The default recognizes the same codes
#' \code{cleangrowth()} does (0/1, m/M, f/F, b/g, male/female, boy/girl; case-
#' and whitespace-insensitive). If you passed a \code{sex_coding} to
#' \code{cleangrowth()}, pass the same one here. Sex is output as M or F, which
#' \code{ext_bmiz()} reads directly. As in \code{cleangrowth()}, with the default
#' coding \code{longwide()} stops if \code{sex} has only 1s and 2s, and gives a
#' prominent warning if every value is 1 across more than one subject; passing
#' \code{sex_coding} removes both. A warning is also given if any retained row's
#' sex is missing or not recognized (its sex becomes NA, so \code{ext_bmiz()}
#' cannot compute BMI z-scores for it).
#' @param param_coding Optional named list mapping your parameter labels to
#' growthcleanr's canonical parameters, exactly as for \code{cleangrowth()}
#' (e.g. \code{list(WEIGHTKG = "wt", HEIGHTCM = "ht")}); pass the same one you
#' gave \code{cleangrowth()}. Parameters are read the same way
#' \code{cleangrowth()} reads them: imperial values (\code{HEIGHTIN},
#' \code{LENGTHIN}, \code{WEIGHTLBS}) are converted to cm/kg and
#' \code{LENGTHCM} is treated as height, so \code{ht} and \code{wt} are always
#' metric. Head circumference rows are not used; rows with an unrecognized
#' parameter are dropped with a warning.
#'
#' @return Returns a data frame transformed from long to wide. Includes only
#' values flagged with indicated inclusion types. Potentially includes
#' additional columns if arguments are passed to extra_cols. For each subject,
#' heights without corresponding weights for a given age (and vice versa) will
#' be dropped unless keep_unmatched_data is set to TRUE. The subject and age
#' columns keep the names you passed as \code{subjid} and \code{agedays} (for
#' example \code{patid}); the other columns are \code{agey}, \code{agem},
#' \code{sex} (recoded to M or F), \code{ht_id}, \code{wt_id},
#' \code{wt} (kg), and \code{ht} (cm). If no rows are kept, a data frame with
#' these columns and no rows is returned.
#'
#' @export
#' @examples
#' \donttest{
#' # Run on a small subset of given data
#' df <- syngrowth[syngrowth$subjid %in% unique(syngrowth$subjid)[1:5], ]
#' gc_out <- cleangrowth(df)
#' df$exclude <- gc_out$exclude[match(df$id, gc_out$id)]
#'
#' # Convert to wide format (default gcr_result = "exclude")
#' wide_df <- longwide(df)
#'
#' # Include all inclusion types
#' wide_df <- longwide(df, include_all = TRUE)
#'
#' # Specify which exclusion codes to retain (default `exclude` column uses
#' # the Summary codes; pass `exclude_detail = TRUE` to cleangrowth() and use
#' # `gcr_result = "exclude_detailed"` for the step-level Detailed codes)
#' wide_df <- longwide(df, inclusion_types = c("Include", "Exclude-CF"))
#' }
longwide <-
  function(long_df,
           id = "id",
           subjid = "subjid",
           sex = "sex",
           agedays = "agedays",
           param = "param",
           measurement = "measurement",
           gcr_result = "exclude",
           include_all = FALSE,
           inclusion_types = c("Include"),
           extra_cols = NULL,
           keep_unmatched_data = FALSE,
           sex_coding = .gc_default_sex_coding,
           param_coding = NULL) {
    # Accept data.table input (e.g. cleangrowth() output, which is a data.table):
    # this function uses data.frame-style `[, col]` indexing throughout, where a
    # data.table would instead read `col` as a literal column name. Coerce up front.
    long_df <- as.data.frame(long_df)

    # The six required columns are read by the names the user passed and
    # renamed to fixed internal names, so every later step refers to the same
    # columns whatever the input calls them (and an unrelated column that
    # happens to be named e.g. `subjid` is never read). The user's subjid and
    # agedays names are restored on output.
    .canon <- c("id", "subjid", "sex", "agedays", "param", "measurement")
    .src   <- c(id, subjid, sex, agedays, param, measurement)

    # check that all needed columns are present, if not, throw an error.
    if(all(c(.src, gcr_result, extra_cols) %in% colnames(long_df))) {
      .clash <- intersect(c(gcr_result, extra_cols),
                          c(.src, .canon, "agey", "agem", "ht", "wt", "ht_id", "wt_id"))
      if (length(.clash) > 0L) {
        stop(sprintf(
          "longwide(): %s cannot be used as gcr_result or in extra_cols, because longwide() already uses %s for a required or output column. Rename the column first.",
          paste0("'", .clash, "'", collapse = ", "),
          if (length(.clash) > 1L) "these names" else "this name"))
      }
      obs_df <- long_df[, c(.src, gcr_result, extra_cols), drop = FALSE]
      names(obs_df)[seq_along(.canon)] <- .canon
      # Same sex-coding checks as cleangrowth(), on the whole column before rows
      # are filtered. cleangrowth() returns sex as entered, so data coded 1 = male,
      # 2 = female reaches this point with no record of the sex_coding used. An
      # infinite cutpoint applies the checks at every age: longwide() uses sex for
      # adult rows too.
      .gc_stop_if_one_two_sex(obs_df$sex, obs_df$agedays, Inf, sex_coding)
      .gc_warn_all_one_sex(obs_df$sex, obs_df$agedays, obs_df$subjid, Inf, sex_coding,
                           caller = "longwide")
    } else {
      # catch error if any variables were not found
      .needed <- c(id, subjid, sex, agedays, param, measurement, gcr_result, extra_cols)
      .missing <- setdiff(.needed, colnames(long_df))
      stop(sprintf(
        "longwide(): input is missing required column(s): %s. Columns present: %s. Pass the matching column-name arguments (id=, subjid=, param=, ...) or add the missing columns.%s",
        paste(.missing, collapse = ", "),
        paste(colnames(long_df), collapse = ", "),
        if (identical(gcr_result, "exclude") && "exclude" %in% .missing)
          " The results column now defaults to 'exclude'. If your results are in another column (2.x scripts used 'gcr_result'), pass gcr_result = \"gcr_result\"." else ""))
    }

    # Warn on inclusion_types values that are not growthcleanr exclusion codes
    # (e.g. 2.x codes such as "Exclude-Carried-Forward"): they match nothing.
    # Known = Summary + Detailed codes, plus bin_exclude/tri_exclude values.
    if (isFALSE(include_all)) {
      .em    <- .gc_exclude_maps()
      .known <- unique(c(.em$levels_summary, .em$levels_detailed, "Exclude", "Same-Day"))
      .unk   <- setdiff(as.character(inclusion_types), .known)
      if (length(.unk) > 0L) {
        warning(sprintf(
          "longwide(): %s %s not %sgrowthcleanr version 3 exclusion code%s, so no rows match %s. Exclusion codes were renamed in version 3; see 'Mapping from prior exclusion codes' in vignette(\"experienced-start-here\").",
          paste0("'", .unk, "'", collapse = ", "),
          if (length(.unk) > 1L) "are" else "is",
          if (length(.unk) > 1L) "" else "a ",
          if (length(.unk) > 1L) "s" else "",
          if (length(.unk) > 1L) "them" else "it"),
          call. = FALSE)
      }
    }

    # extract values flagged with indicated inclusion types:
    if (include_all == TRUE) {
      obs_df <- obs_df
    } else if (include_all == FALSE) {
      obs_df <- obs_df[obs_df[,gcr_result] %in% inclusion_types,]
    } else{
      stop(paste0("longwide(): include_all must be a single TRUE or FALSE; a ",
                  typeof(include_all), " of length ", length(include_all), " was entered."))
    }

    # calculate age in years
    obs_df$agey <- round(obs_df$agedays / 365.25, 4)

    # calculate age in months
    obs_df$agem <- round((obs_df$agey * 12), 4)

    # recode sex to M/F, parsing the submitted codes exactly as cleangrowth() does
    # (same default codes, or the user's sex_coding). M/F means the same thing in
    # cleangrowth(), longwide(), and ext_bmiz(), and is valid for single-sex data.
    .sex_int <- .gc_recode_sex(obs_df$sex, sex_coding)
    .sex_na  <- is.na(.sex_int)
    if (any(.sex_na)) {
      .sub  <- as.character(obs_df$sex[.sex_na])
      .unrec <- unique(.sub[!is.na(.sub) & trimws(.sub) != ""])
      warning(sprintf(
        "longwide(): %d of %d retained measurement(s) have a missing or unrecognized sex%s; their sex is NA in the output, so ext_bmiz() cannot compute BMI z-scores for them. If your data uses other codes, pass sex_coding = list(male = ..., female = ...) (the same sex_coding given to cleangrowth()). Recognized by default: 0/1, m/M, f/F, b/g, male/female, boy/girl.",
        sum(.sex_na), length(.sex_na),
        if (length(.unrec) > 0L)
          sprintf(" (unrecognized values: %s)",
                  paste(utils::head(.unrec, 5), collapse = ", ")) else ""),
        call. = FALSE)
    }
    # Recoded in place; the output column is always named `sex` (its values
    # are recoded, so they no longer match the user's column).
    obs_df$sex <- c("M", "F")[.sex_int + 1L]

    # Resolve param and measurement the way cleangrowth() does, since its
    # output returns both verbatim: param_coding labels (e.g. "WT") resolve to
    # a canonical param, imperial values convert to metric (HEIGHTIN/LENGTHIN
    # x 2.54, WEIGHTLBS / 2.2046226), and LENGTHCM is treated as HEIGHTCM
    # (relabel only). Written to `param` / `measurement` (the names the pivot
    # below keys on). Only height and weight are used; head circumference is
    # dropped silently, and unrecognized labels are dropped with a warning.
    .p_in  <- obs_df$param
    .p_can <- .gc_recode_param(.p_in, param_coding)
    .conv  <- .gc_to_metric(.p_can, obs_df$measurement, stage = "all")
    obs_df$param <- .conv$param
    obs_df$measurement <- .conv$v
    .unrec_p <- !.conv$param %in% c(.gc_recognized_params, "HEADCM")
    if (any(.unrec_p)) {
      .vals <- unique(as.character(.p_in[.unrec_p]))
      warning(sprintf(
        "longwide(): %d of %d retained measurement(s) have a param growthcleanr does not recognize (values: %s) and were dropped. If any are height or weight, map them with param_coding = list(WEIGHTKG = ..., HEIGHTCM = ...) (the same param_coding given to cleangrowth()).",
        sum(.unrec_p), length(.unrec_p), paste(utils::head(.vals, 5), collapse = ", ")),
        call. = FALSE)
    }
    obs_df <- obs_df[obs_df$param %in% c("HEIGHTCM", "WEIGHTKG"), , drop = FALSE]

    # check for unique weight and height ids
    if (any(duplicated(obs_df$id))) {
      .dup <- unique(obs_df$id[duplicated(obs_df$id)])
      stop(sprintf(
        "longwide(): `id` must be unique per row, but %d value(s) are duplicated (e.g. %s). longwide() pairs heights and weights by row `id`; supply a unique per-row id or de-duplicate first.",
        length(.dup), paste(utils::head(.dup, 5), collapse = ", ")))
    }

    # Split into heights and weights (rows with a missing measurement are not
    # used). Columns: the join keys, any extra_cols, the value, and the row id.
    .keys <- c("subjid", "agey", "agem", "agedays", "sex")
    .one_param <- function(p, valname, idname) {
      x <- obs_df[obs_df$param == p & !is.na(obs_df$measurement), , drop = FALSE]
      out <- x[, c(.keys, extra_cols), drop = FALSE]
      out[[valname]] <- x$measurement
      out[[idname]]  <- x$id
      rownames(out) <- NULL
      out
    }
    height <- .one_param("HEIGHTCM", "HEIGHTCM", "ht_id")
    weight <- .one_param("WEIGHTKG", "WEIGHTKG", "wt_id")

    # join based on subjid, age, and sex, potentially keep unmatched values too
    wide_df <- merge(height,
                     weight,
                     by = .keys,
                     all = keep_unmatched_data)
    wide_df$wt <- wide_df$WEIGHTKG
    wide_df$ht <- wide_df$HEIGHTCM


    for(col in extra_cols) {
      ht_vals <- wide_df[,paste0(col, ".x")]
      wt_vals <- wide_df[,paste0(col, ".y")]
      match_vals <- ht_vals == wt_vals
      match_vals[is.na(ht_vals) & is.na(wt_vals)] <- TRUE

      # If the columns match exactly, then just put a single column with that
      # name in and drop the other two.
      if(sum(match_vals, na.rm=TRUE)==length(match_vals)) {
        wide_df[,col] <- ht_vals
        wide_df[,c(paste0(col, ".x"), paste0(col, ".y"))] <- list(NULL)
      } else {
        wide_df[,paste0("match_", col)] <- match_vals
        colnames(wide_df)[colnames(wide_df)==paste0(col, ".x")] <- paste0("ht_",
                                                                          col)
        colnames(wide_df)[colnames(wide_df)==paste0(col, ".y")] <- paste0("wt_",
                                                                          col)
      }
    }

    # Drop WEIGHTKG and HEIGHTCM columns since they are in ht and wt
    wide_df <- wide_df[, setdiff(names(wide_df), c("HEIGHTCM", "WEIGHTKG")), drop = FALSE]

    # Return the subject and age columns under the names the user passed
    # (sex keeps the name `sex`: its values are recoded to M/F).
    names(wide_df)[names(wide_df) == "subjid"]  <- subjid
    names(wide_df)[names(wide_df) == "agedays"] <- agedays

    return(wide_df)
}

#' Compute BMI using standard formula
#'
#' \code{simple_bmi} Computes BMI using standard formula. Assumes input compatible with
#' output from longwide().
#'
#' @param wide_df A data frame or data table containing heights and weights in
#' wide format, e.g., after transformation with longwide()
#' @param wtcol name of the weight value column (kg), default 'wt'
#' @param htcol name of the height value column (cm), default 'ht'
#'
#' @return Returns a data table with the added column "bmi"
#'
#' @export
#' @import data.table
#' @examples
#' \donttest{
#' # Simple usage
#' # Run on a small subset of given data
#' df <- as.data.frame(syngrowth)
#' df <- df[df$subjid %in% unique(df[, "subjid"])[1:2], ]
#' # cleangrowth() returns the data with an 'exclude' column
#' clean_df <- cleangrowth(df)
#' # Convert to wide format
#' wide_df <- longwide(clean_df)
#' wide_df_with_bmi <- simple_bmi(wide_df)
#'
#' # Specifying different column names; note that quotes are used
#' colnames(wide_df)[colnames(wide_df) %in% c("wt", "ht")] <-
#'   c("weight", "height")
#' wide_df_with_bmi <- simple_bmi(wide_df, wtcol = "weight", htcol = "height")
#' }
simple_bmi <- function(wide_df, wtcol = "wt", htcol = "ht") {
  # Verify the specified columns are present
  if (!all(c(wtcol, htcol) %in% colnames(wide_df))) {
    stop(sprintf(
      "simple_bmi(): weight/height column(s) not found: %s. Columns present: %s. Pass wtcol= and htcol= to match your data.",
      paste(setdiff(c(wtcol, htcol), colnames(wide_df)), collapse = ", "),
      paste(colnames(wide_df), collapse = ", ")))
  }

  # coerce to data table
  if (!is.data.table(wide_df)){
    wide_df <- as.data.table(wide_df)
  }

  # add bmi column
  wide_df[, "bmi" := get(wtcol) / ((get(htcol) * 0.01) ^ 2)]
  return(wide_df)
}

#' Removed function: adjustcarryforward()
#'
#' `adjustcarryforward()` was removed in growthcleanr 3.0.0. Carried-forward
#' handling is now part of [cleangrowth()]; use its `cf_rescue` argument. This
#' function remains only to stop with that message, so that packages that refer
#' to it do not fail their checks.
#'
#' @param ... Ignored.
#' @return None; always stops with an error.
#' @seealso `vignette("cf-rescue")`
#' @keywords internal
#' @export
adjustcarryforward <- function(...) {
  stop("adjustcarryforward() was removed in growthcleanr 3.0.0. ",
       "Use the cf_rescue argument of cleangrowth() instead; see vignette(\"cf-rescue\").",
       call. = FALSE)
}
