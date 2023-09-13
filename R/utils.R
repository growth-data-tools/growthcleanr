#' Split input data into multiple files
#'
#' \code{splitinput} Splits input based on keepcol specified, yielding csv files each with at least the mininum
#' number of rows that are written and saved separately (except for the last split file written, which may be
#' smaller). Allows splitting input data while ensuring all records for each individual subject will stay together
#' in one file. Pads split filenames with zeros out to five digits for consistency, assuming < 100,000 file count
#' result.
#'
#' @param df data frame to split
#' @param fname new name for each of the split files to start with
#' @param fdir directory to put each of the split files (use "." for  working directory). Must be changed from default (NA), which will trigger error.
#' @param min_nrow minimum number of rows for each split file (default 10000)
#' @param keepcol the column name (default "subjid") to use to keep records with the same values together in the same single split file
#'
#' @return the count number referring to the last split file written
#'
#' @export
#' @importFrom utils tail
#' @examples
#' \donttest{
#' # Run on given data
#' df <- as.data.frame(syngrowth)
#'
#' # Run with all defaults (specifying directory)
#' splitinput(df, fdir = tempdir())
#'
#' # Specifying the name, directory and minimum row size
#' splitinput(df, fname = "syngrowth", fdir = tempdir(), min_nrow = 5000)
#'
#' # Specifying a different subject ID column
#' colnames(df)[colnames(df) == "subjid"] <- "sub_id"
#' splitinput(df, fdir = tempdir(), keepcol = "sub_id")
#' }
splitinput <-
  function(df,
           fname = deparse(substitute(df)),
           fdir = NA,
           min_nrow = 10000,
           keepcol = 'subjid') {
    # first, check if the given directory exists
    if (!is.na(fdir) & fdir != "." & is.character(fdir) & !dir.exists(fdir)){
      stop("invalid directory")
    }

    fname_counter <- 0
    row_count <- 0
    split_df <- data.frame()

    # split the data frame by the grouping that user specifies
    split_sample <- split(df, df[[keepcol]])

    # grab the individual grouping names
    split_sample_names <- names(split_sample)

    for (name in split_sample_names) {
      # append the rows from name, store new total row_count for current split file
      split_df <- rbind(split_df, split_sample[[name]])
      current_nrow <- nrow(split_df)

      # check if updated row count will exceed min row count,
      # if min nrow is exceeded, then write.csv the current split file and clear the split dataframe starter (start from 0)
      if (current_nrow > min_nrow) {
        fname_counter_str <- sprintf("%05d", fname_counter) #pad 0s
        write.csv(
          split_df,
          file = file.path(fdir, paste(fname, fname_counter_str, "csv", sep = ".")),
          row.names = FALSE
        )

        split_df <- data.frame() #reset split_df
        fname_counter <- fname_counter + 1
      } else if (name == tail(split_sample_names, 1)) {
        #for last part, just write
        fname_counter_str <- sprintf("%05d", fname_counter) #pad 0s
        write.csv(
          split_df,
          file = file.path(fdir, paste(fname, fname_counter_str, "csv", sep = ".")),
          row.names = FALSE,
          na = ""
        )
      }
    }

    return(fname_counter)
  }


#' Recode binary sex variable for compatibility
#'
#' \code{recode_sex} recodes a binary sex variable for a given source column in a data frame or data table.
#' Useful in transforming output from growthcleanr::cleangrowth() into a format suitable for growthcleanr::ext_bmiz().
#'
#' @param input_data a data frame or data table to be transformed. Expects a source column containing a binary sex variable.
#' @param sourcecol name of sex descriptor column. Defaults to "sex"
#' @param sourcem variable indicating "male" sex in input data. Defaults to "0"
#' @param sourcef variable indicating "female" sex in input data. Defaults to "1"
#' @param targetcol desired name of recoded sex descriptor column. Defaults to "sex_recoded"
#' @param targetm desired name of recoded sex variable indicating "male" sex in output data. Defaults to 1
#' @param targetf desired name of recoded sex variable indicating "female" sex in output data. Defaults to 2
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
  #replace targetcol variables with targetm where sourcecol = sourcem
  input_table[input_table[[sourcecol]] == sourcem, targetcol] <-
    targetm
  #replace targetcol variables with targetf where sourcecol = sourcef
  input_table[input_table[[sourcecol]] == sourcef, targetcol] <-
    targetf

  #return table
  return(input_table)
}


#' Transform data in growthcleanr format into wide structure for BMI calculation
#'
#' \code{longwide} transforms data from long to wide format. Ideal for
#' transforming output from growthcleanr::cleangrowth() into a format suitable
#' for growthcleanr::ext_bmiz().
#'
#' @param long_df A data frame to be transformed. Expects columns: id, subjid,
#' sex, agedays, param, measurement, and gcr_result.
#' @param id name of observation ID column
#' @param subjid name of subject ID column
#' @param sex name of sex descriptor column
#' @param agedays name of age (in days) descriptor column
#' @param param name of parameter column to identify each type of measurement
#' @param measurement name of measurement column containing the actual
#' measurement data
#' @param gcr_result name of column of results from growthcleanr::cleangrowth()
#' @param include_all Determines whether the function keeps all exclusion codes.
#' If TRUE, all exclusion types are kept and the inclusion_types argument is
#' ignored. Defaults to FALSE.
#' @param inclusion_types Vector indicating which exclusion codes from the
#' cleaning algorithm should be included in the data, given that include_all is
#' FALSE. For all options, see growthcleanr::cleangrowth(). Defaults to
#' c("Include").
#' @param extra_cols Vector of additional columns to include in the output. If
#' a column C1 differs on agedays matched height and weight values, then include
#' separate ht_C1 and wt_C1 columns as well as a match_C1 column that gives
#' booleans indicating where ht_C1 and wt_C1 are the same. If the agedays
#' matched height and weight columns are identical, then only include a single
#' version of C1. Defaults to empty vector (not keeping any additional columns).
#' @param keep_unmatched_data boolean indicating whether to keep height/weight
#' observations that do not have a matching weight/height on that day
#'
#' @return Returns a data frame transformed from long to wide. Includes only
#' values flagged with indicated inclusion types. Potentially includes
#' additional columns if arguments are passed to extra_cols. For each subject,
#' heights without corresponding weights for a given age (and vice versa) will
#' be dropped unless keep_unmatched_data is set to TRUE.
#'
#' @export
#' @rawNamespace import(tidyr, except = extract)
#' @rawNamespace import(dplyr, except = c(last, first, summarize, src, between))
#' @examples
#' # Run on a small subset of given data
#' df <- as.data.frame(syngrowth)
#' df <- df[df$subjid %in% unique(df[, "subjid"])[1:5], ]
#' df <- cbind(df,
#'             "gcr_result" = cleangrowth(df$subjid,
#'                                        df$param,
#'                                        df$agedays,
#'                                        df$sex,
#'                                        df$measurement))
#' # Convert to wide format
#' wide_df <- longwide(df)
#'
#' # Include all inclusion types
#' wide_df <- longwide(df, include_all = TRUE)
#'
#' # Specify all inclusion codes
#' wide_df <- longwide(df, inclusion_types = c("Include", "Exclude-Carried-Forward"))
longwide <-
  function(long_df,
           id = "id",
           subjid = "subjid",
           sex = "sex",
           agedays = "agedays",
           param = "param",
           measurement = "measurement",
           gcr_result = "gcr_result",
           include_all = FALSE,
           inclusion_types = c("Include"),
           extra_cols = NULL,
           keep_unmatched_data = FALSE) {
    # avoid "no visible binding" warnings
    sex_recoded <- agey <- agem <- HEIGHTCM <- WEIGHTKG <- NULL
    wt <- wt_id <- ht <- ht_id <- NULL

    # check that all needed columns are present, if not, throw an error.
    if(all(c(id, subjid, sex, agedays, param, measurement, gcr_result,
             extra_cols) %in% colnames(long_df))) {
      long_df %>%
        select(id, subjid, sex, agedays, param, measurement, all_of(gcr_result),
               all_of(extra_cols)) -> obs_df
    } else {
      # catch error if any variables were not found
      stop("not all specified columns were present")
    }

    # extract values flagged with indicated inclusion types:
    if (include_all == TRUE) {
      obs_df <- obs_df
    } else if (include_all == FALSE) {
      obs_df <- obs_df[obs_df[,gcr_result] %in% inclusion_types,]
    } else{
      stop(paste0("include_all is not a logical of length 1. It is a ",
                  typeof(include_all), " of length ", length(include_all)))
    }

    # calculate age in years
    obs_df$agey <- round(obs_df$agedays / 365.25, 4)

    # calculate age in months
    obs_df$agem <- round((obs_df$agey * 12), 4)

    # recode sex to expected ext_bmiz() format
    obs_df <- recode_sex(
      input_data = obs_df,
      sourcecol = "sex",
      sourcem = "0",
      sourcef = "1",
      targetcol = "sex_recoded",
      targetm = 1L,
      targetf = 2L
    )

    obs_df %>%
      mutate(sex = sex_recoded) %>%
      mutate(param = as.character(param)) %>%
      select(subjid, id, agey, agem, agedays, sex, param, measurement,
             all_of(extra_cols)) -> clean_df


    # check for unique weight and height ids
    if (any(duplicated(clean_df$id))) {
      stop("duplicate IDs in long_df")
    }

    # separate heights and weights using unique ids
    clean_df %>%
      pivot_wider(names_from = param, values_from = measurement) -> param_separated

    # extract heights and weights attached to ids
    param_separated %>%
      filter(!is.na(HEIGHTCM)) %>%
      filter(is.na(WEIGHTKG)) %>% # why this, shouldn't the !is.na height take care of it
      mutate(ht_id = id) %>%
      select(-id) %>%
      select(-WEIGHTKG) -> height

    param_separated %>%
      filter(is.na(HEIGHTCM)) %>%
      filter(!is.na(WEIGHTKG)) %>%
      mutate(wt_id = id) %>%
      select(-id) %>%
      select(-HEIGHTCM) -> weight

    # join based on subjid, age, and sex, potentially keep unmatched values too
    wide_df <- merge(height,
                     weight,
                     by = c("subjid", "agey", "agem", "agedays", "sex"),
                     all = keep_unmatched_data) %>%
      mutate(wt = WEIGHTKG, ht = HEIGHTCM)


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
    wide_df <- wide_df %>% select(-c(HEIGHTCM, WEIGHTKG))

    return(wide_df)
}

#' Compute BMI using standard formula
#'
#' \code{simple_bmi} Computes BMI using standard formula. Assumes input compatible with
#' output from longwide().
#'
#' @param wide_df A data frame or data table containing heights and weights in
#' wide format, e.g., after transformation with longwide()
#' @param wtcol name of observation height value column, default 'wt'
#' @param htcol name of subject weight value column, default 'ht'
#'
#' @return Returns a data table with the added column "bmi"
#'
#' @export
#' @import data.table
#' @rawNamespace import(dplyr, except = c(last, first, summarize, src, between))
#' @examples
#' # Simple usage
#' # Run on a small subset of given data
#' df <- as.data.frame(syngrowth)
#' df <- df[df$subjid %in% unique(df[, "subjid"])[1:5], ]
#' df <- cbind(df,
#'             "gcr_result" = cleangrowth(df$subjid,
#'                                        df$param,
#'                                        df$agedays,
#'                                        df$sex,
#'                                        df$measurement))
#' # Convert to wide format
#' wide_df <- longwide(df)
#' wide_df_with_bmi <- simple_bmi(wide_df)
#'
#' # Specifying different column names; note that quotes are used
#' colnames(wide_df)[colnames(wide_df) %in% c("wt", "ht")] <-
#'   c("weight", "height")
#' wide_df_with_bmi <- simple_bmi(wide_df, wtcol = "weight", htcol = "height")
simple_bmi <- function(wide_df, wtcol = "wt", htcol = "ht") {
  # Verify the specified columns are present
  if (!all(c(wtcol, htcol) %in% colnames(wide_df))) {
    stop("Specified column names are not all present")
  }

  # coerce to data table
  if (!is.data.table(wide_df)){
    wide_df <- as.data.table(wide_df)
  }

  # add bmi column
  wide_df[, "bmi" := get(wtcol) / ((get(htcol) * 0.01) ^ 2)]
  return(wide_df)
}
