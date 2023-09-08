# Infants support

# supporting (all) ---

# function that takes in a parameter name and returns the designated other
# parameter (DOP)
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
#' Function for temporary extraneous (step 5):
#' 5.  Temporary extraneous: I use extraneous to refer to more than more than one recorded value for a parameter on the same day,
#'     and we need to select which one to include in our analysis. The overall strategy will be to select a measurement using a simple
#'     strategy that will be used temporarily, and select permanently in a later step after we have a somewhat cleaner dataset that
#'     can help us identify the best extraneous.
#' a.  For subjects/parameters with extraneous: Determine median_tbc*sd for both parameters: the median tbc*sd for each subject
#'     and parameter including only non-extraneous values with exc_*==0. The median of the same parameter as the extraneous will
#'     be referred to as median_tbc*sd, the median of the other parameter will be referred to as median_tbcOsd.
#' b.	For each subject/parameter with extraneous and at least one value for the subject/parameter on a day with no extraneous,
#'     select the value closest to the median_tbc*sd for temporary inclusion, and assign all other extraneous exc_*=2.
#'  i.	For each subject/parameter with extraneous and no values for the subject/parameter on a day with no extraneous, select the
#'     value closest to the median_tbcOsd for temporary inclusion, and assign all other extraneous exc_*=2.
#'     If median_tbcOsd is missing because there are no values for the other parameter, randomly choose one extraneous value for
#'     each subject/parameter/age to keep as exc_*=0 and replace exc_*=2 for all other extraneous for that subject/parameter/age.
#'
#' @keywords internal
#' @noRd
temporary_extraneous_infants <- function(df) {
  # avoid "no visible binding" warnings
  agedays <- delta.median.sd <- extraneous <- extraneous.this.day <- NULL
  index <- median.sd <- param <- subjid <- tbc.sd <- NULL

  # add subjid and param if needed (may be missing depending on where this is called from)
  if (is.null(df$subjid))
    df[, subjid := NA]
  if (is.null(df$param))
    df[, param := NA]
  # only operate on valid rows (but include rows that may have previously been flagged as a "temporary extraneous")
  # also do not calculate temporary extraneous for NNTEs
  valid.rows <- valid(df, include.temporary.extraneous = TRUE) &
    !df$nnte
  # make a small copy of df with only the fields we need for efficiency
  df <- df[j = .(tbc.sd), keyby = .(subjid, param, agedays, index)]
  # initialize some useful fields
  df[, `:=`(
    median.sd = as.double(NA),
    delta.median.sd = as.double(NA),
    extraneous.this.day = FALSE,
    extraneous = FALSE
  )]

  # determine on which days there are extraneous measurements (more than 1 valid measurement on that age day)
  df[valid.rows, extraneous.this.day := (.N > 1), by = .(subjid, param, agedays)]
  # calculate median of measurements on days where there is no extraneous
  df[valid.rows &
       !extraneous.this.day, median.sd := median(tbc.sd), by = .(subjid, param)]

  # distribute median to other valid or potential extraneous rows for same parameter for that subject
  df[valid.rows, median.sd := sort(median.sd)[1], by = .(subjid, param)]
  # take care of subject/parameters with more than one day with a valid observation
  # determine the absolute difference between the measurements sd score and the median for that parameter for each child
  df[valid.rows &
       extraneous.this.day, delta.median.sd := abs(tbc.sd - median.sd), by = .(subjid, param)]
  # identify subjects that have duplicates/extraneous on all days of observation for that parameter (i.e. delta.median.sd undefined)
  subj.all.dups <- df[valid.rows &
                        extraneous.this.day &
                        is.na(delta.median.sd), unique(subjid)]

  # add the "other map" -- now including head circumference
  other_p_map <- c(
    "HEIGHTCM" = "WEIGHTKG",
    "WEIGHTKG" = "HEIGHTCM",
    "HEADCM" = "HEIGHTCM"
  )

  df[valid.rows &
       subjid %in% subj.all.dups, delta.median.sd := (function(subj.df) {
         # iterate over parameters where delta.median.sd is not yet defined
         # pass 1: take median from other parameter(s)
         for (p in subj.df[is.na(delta.median.sd) &
                           extraneous.this.day, unique(param)]) {
           median.other.sd <- subj.df[param == other_p_map[p] &
                                        !extraneous.this.day, median(tbc.sd)]
           subj.df[param == p, delta.median.sd := abs(tbc.sd - median.other.sd)]
         }
         return(subj.df$delta.median.sd)
       })(copy(.SD)), by = .(subjid)]

  # Final pass: take median as zero (i.e. if no measurements from a different parameter)
  # NOTE -- this is not exactly the same as taking a random parameter
  df[valid.rows &
       extraneous.this.day &
       is.na(delta.median.sd), delta.median.sd := abs(tbc.sd)]

  # flag any extraneous value on the same day that is not the minimum distance from the median sd score
  # NOTE: in the case of exact ducplicates, "which.min" will pick the first
  df[valid.rows &
       extraneous.this.day, extraneous := seq_along(delta.median.sd) != which.min(delta.median.sd), by =
       .(subjid, param, agedays)]
  # return the extraneous valid rows (i.e. the ones that should be temporarily excluded)
  return(df$extraneous & valid.rows)
}

# evil twins ----

#' Function to calculate out of bounds (OOB) measurements for Evil Twins step
#'
#' @param df data table with all parameters
#'
#' @keywords internal
#' @noRd
calc_oob_evil_twins <- function(df){
  # # start by determining if a measurement is out of bounds (oob)

  # for differences for adjacent, we pad with a high number to default to true
  # we don't want to consider the comparison with the next subj/param
  oob <- ((
    (abs(c(df$tbc.sd[2:nrow(df)], Inf) - df$tbc.sd) > 5 &
       rev(duplicated(rev(paste(df$subjid, "_", df$param))))) |
      (abs(c(Inf, df$tbc.sd[1:(nrow(df)-1)]) - df$tbc.sd) > 5 &
         duplicated(paste(df$subjid, "_", df$param)))
  )) & ((
    (abs(c(df$ctbc.sd[2:nrow(df)], Inf) - df$ctbc.sd) > 5 &
       rev(duplicated(rev(paste(df$subjid, "_", df$param))))) |
      (abs(c(Inf, df$ctbc.sd[1:(nrow(df)-1)]) - df$ctbc.sd) > 5 &
         duplicated(paste(df$subjid, "_", df$param)))
  ))

  df[, "oob" := oob]

  return(df)
}

# moderate ewma ----

# function to calculate and recenter z scores for a given column
# df: data frame with parameter, agedays, sex, cn
# cn: column name to calculate, smooth, and recenter
# ref.data.path: reference data path
#
# returns df with additional column, tbc.(cn), which is the recentered z-score
# for the input
#' @keywords internal
#' @noRd
calc_and_recenter_z_scores <- function(df, cn, ref.data.path){
  # for infants, use z and who
  measurement.to.z <- read_anthro(ref.data.path, cdc.only = TRUE,
                                  infants = TRUE)
  measurement.to.z_who <- read_anthro(ref.data.path, cdc.only = FALSE,
                                      infants = TRUE)

  # calculate "standard deviation" scores
  df[, cn.orig_cdc := measurement.to.z(param, agedays, sex, get(cn), TRUE)]
  df[, cn.orig_who := measurement.to.z_who(param, agedays, sex, get(cn), TRUE)]

  # smooth z-scores/SD scores between ages 1 - 3yo using weighted scores
  # older uses cdc, younger uses who
  who_weight <- 4 - (df$agedays/365.25)
  cdc_weight <- (df$agedays/365.25) - 2

  smooth_val <- df$agedays/365.25 >= 2 &
    df$agedays/365.25 <= 4 &
    df$param != "HEADCM"
  df[smooth_val,
           cn.orig := (cn.orig_cdc[smooth_val]*cdc_weight[smooth_val] +
                         cn.orig_who[smooth_val]*who_weight[smooth_val])/2]

  # otherwise use WHO and CDC for older and younger, respectively
  who_val <- df$param == "HEADCM" |
    df$agedays/365.25 < 2
  df[who_val | (smooth_val & is.na(df$cn.orig_cdc)),
           cn.orig := df$cn.orig_who[who_val  | (smooth_val & is.na(df$cn.orig_cdc))]]

  cdc_val <- df$param != "HEADCM" |
    df$agedays/365.25 > 4
  df[cdc_val | (smooth_val & is.na(df$cn.orig_who)),
           cn.orig := df$cn.orig_cdc[cdc_val | (smooth_val & is.na(df$sd.orig_who))]]

  # now recenter -- already has the sd.median from the original recentering
  setkey(df, subjid, param, agedays)
  df[, tbc.cn := cn.orig - sd.median]

  # rename ending column
  setnames(df, "tbc.cn", paste0("tbc.", cn))

  return(df)
}
