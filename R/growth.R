# Main growthcleanr algorithm and other exported functions
# Main growthcleanr algorithm (cleangrowth): adult and pediatric are in *_clean.R
#(main bulk of algorithm) and *_support.R (supporting functions)

#' Clean growth measurements
#'
#' @param subjid Vector of unique identifiers for each subject in the database.
#' @param param Vector identifying each measurement, may be 'WEIGHTKG', 'WEIGHTLBS', 'HEIGHTCM', 'HEIGHTIN', 'LENGTHCM', or 'HEADCM'.
#'   'HEIGHTCM'/'HEIGHTIN' vs. 'LENGTHCM' only affects z-score calculations between ages 24 to 35 months (730 to 1095 days).
#'   All linear measurements below 731 days of life (age 0-23 months) are interpreted as supine length, and
#'   all linear measurements above 1095 days of life (age 36+ months) are interpreted as standing height.
#'   Note: at the moment, all LENGTHCM will be converted to HEIGHTCM. In the future, the algorithm will be updated to consider this difference.
#'   Additionally, imperial 'HEIGHTIN' and 'WEIGHTLBS' measurements are converted to
#'   metric during algorithm calculations.
#' @param agedays Numeric vector containing the age in days at each measurement.
#' @param sex Vector identifying the gender of the subject, may be 'M', 'm', or 0 for males, vs. 'F', 'f' or 1 for females.
#' @param measurement Numeric vector containing the actual measurement data.  Weight must be in
#'   kilograms (kg), and linear measurements (height vs. length) in centimeters (cm).
#' @param recover.unit.error Indicates whether the cleaning algorithm should
#' attempt to identify unit errors (I.e. inches vs. cm, lbs vs. kg). If unit
#' errors are identified, the value will be corrected and retained within the
#' cleaning algorithm as a valid measurement.  Defaults to FALSE.
#' @param sd.extreme Measurements more than sd.extreme standard deviations from
#' the mean (either above or below) will be flagged as invalid. Defaults to 25.
#' @param z.extreme Measurements with an absolute z-score greater than
#' z.extreme will be flagged as invalid. Defaults to 25.
#' @param lt3.exclude.mode Determines type of exclusion procedure to use for 1 or 2 measurements of one type without
#' matching same ageday measurements for the other parameter. Options include "default" (standard growthcleanr approach),
#' and "flag.both" (in case of two measurements of one type without matching values for the other parameter, flag both
#' for exclusion if beyond threshold)
#' @param height.tolerance.cm maximum decrease in height tolerated for sequential measurements
#' @param error.load.mincount minimum count of exclusions on parameter before
#' considering excluding all measurements. Defaults to 2.
#' @param error.load.threshold threshold of percentage of excluded measurement count to included measurement
#' count that must be exceeded before excluding all measurements of either parameter. Defaults to 0.5.
#' @param sd.recenter specifies how to recenter medians. May be a data frame or
#' table w/median SD-scores per day of life by gender and parameter, or "NHANES"
#' or "derive" as a character vector.
#' \itemize{
#'   \item If `sd.recenter` is specified as a data set, use the data set
#'   \item If `sd.recenter` is specified as "`nhanes`", use NHANES reference medians
#'   \item If `sd.recenter` is specified as "`derive`", derive from input
#'   \item If `sd.recenter` is not specified or `NA`:
#'     \itemize{
#'       \item If the input set has at least 5,000 observations, derive medians from input
#'       \item If the input set has fewer than 5,000 observations, use NHANES
#'     }
#' }
#'
#' If specifying a data set, columns must include param, sex, agedays, and sd.median
#' (referred to elsewhere as "modified Z-score"), and those medians will be used
#' for recentering. A summary of how the NHANES reference medians were derived is
#' available in README.md. Defaults to NA.
#' @param sdmedian.filename Name of file to save sd.median data calculated on the input dataset to as CSV.
#' Defaults to "", for which this data will not be saved. Use for extracting medians for parallel processing
#' scenarios other than the built-in parallel option.
#' @param sdrecentered.filename Name of file to save re-centered data to as CSV. Defaults to "", for which this
#' data will not be saved. Useful for post-processing and debugging.
#' @param include.carryforward Determines whether Carry-Forward values are kept in the output. Defaults to False.
#' @param ewma.exp Exponent to use for weighting measurements in the
#' exponentially weighted moving average calculations. Defaults to -1.5.
#' This exponent should be negative in order to weight growth measurements
#' closer to the measurement being evaluated more strongly. Exponents that are
#' further from zero (e.g. -3) will increase the relative influence of
#' measurements close in time to the measurement being evaluated compared to
#' using the default exponent.
#' @param ref.data.path Path to reference data. If not supplied, the year 2000
#' Centers for Disease Control (CDC) reference data will be used.
#' @param log.path Path to log file output when running in parallel (non-quiet mode). Default is NA. A new
#' directory will be created if necessary. Set to NA to disable log files.
#' @param parallel Determines if function runs in parallel.  Defaults to FALSE.
#' @param num.batches Specify the number of batches to run in parallel. Only
#' applies if parallel is set to TRUE. Defaults to the number of workers
#' returned by the getDoParWorkers function in the foreach package.
#' @param quietly Determines if function messages are to be displayed and if log files (parallel only) are to be generated.
#' Defaults to TRUE
#' @param adult_cutpoint Number between 18 and 20, describing ages when the
#'  pediatric algorithm should not be applied (< adult_cutpoint), and the adult
#'   algorithm should apply (>= adult_cutpoint). Numbers outside this range will be
#'   changed to the closest number within the range. Defaults to 20.
#' @param weight_cap Positive number, describing a weight cap in kg (rounded to the
#' nearest .1, +/- .1) within the adult dataset. If there is no weight cap, set
#'  to Inf. Defaults to Inf.
#' @param adult_columns_filename Name of file to save original adult data, with additional output columns to
#' as CSV. Defaults to "", for which this data will not be saved. Useful
#' for post-analysis. For more information on this output, please see README.
#' @param prelim_infants TRUE/FALSE. Run the in-development release of the infants algorithm (expands pediatric algorithm to improve performance for children 0 – 2 years). Not recommended for use in research. For more information regarding the logic of the algorithm, see the vignette 'Preliminary Infants Algorithm.' Defaults to FALSE.
#'
#' @return Vector of exclusion codes for each of the input measurements.
#'
#'    Possible values for each code are:
#'
#' * 'Include', 'Unit-Error-High', 'Unit-Error-Low', 'Swapped-Measurements', 'Missing',
#' *  'Exclude-Carried-Forward', 'Exclude-SD-Cutoff', 'Exclude-EWMA-Extreme', 'Exclude-EWMA-Extreme-Pair',
#' *  'Exclude-Extraneous-Same-Day',
#' *  'Exclude-EWMA-8', 'Exclude-EWMA-9', 'Exclude-EWMA-10', 'Exclude-EWMA-11', 'Exclude-EWMA-12', 'Exclude-EWMA-13', 'Exclude-EWMA-14',
#' *  'Exclude-Min-Height-Change', 'Exclude-Max-Height-Change',
#' *  'Exclude-Pair-Delta-17', 'Exclude-Pair-Delta-18', 'Exclude-Pair-Delta-19',
#' *  'Exclude-Single-Outlier', 'Exclude-Too-Many-Errors', 'Exclude-Too-Many-Errors-Other-Parameter'
#' @md
#'
#' @export
#' @import data.table
#' @rawNamespace import(plyr, except = c(failwith, id, summarize, count, desc, mutate, arrange, rename, is.discrete, summarise, summarize))
#' @import foreach
#' @import doParallel
#' @import parallel
#' @importFrom utils write.csv
#' @rawNamespace import(R.utils, except = c(extract))
#' @examples
#' \donttest{
#' # Run calculation using a small subset of given data
#' df_stats <- as.data.frame(syngrowth)
#' df_stats <- df_stats[df_stats$subjid %in% unique(df_stats[, "subjid"])[1:5], ]
#'
#' clean_stats <-cleangrowth(subjid = df_stats$subjid,
#'                          param = df_stats$param,
#'                          agedays = df_stats$agedays,
#'                          sex = df_stats$sex,
#'                          measurement = df_stats$measurement)
#'
#' # Once processed you can filter data based on result value
#' df_stats <- cbind(df_stats, "clean_result" = clean_stats)
#' clean_df_stats <- df_stats[df_stats$clean_result == "Include",]
#'
#' # Parallel processing: run using 2 cores and batches
#' clean_stats <- cleangrowth(subjid = df_stats$subjid,
#'                            param = df_stats$param,
#'                            agedays = df_stats$agedays,
#'                            sex = df_stats$sex,
#'                            measurement = df_stats$measurement,
#'                            parallel = TRUE,
#'                            num.batches = 2)
#' }
cleangrowth <- function(subjid,
                        param,
                        agedays,
                        sex,
                        measurement,
                        recover.unit.error = FALSE,
                        sd.extreme = 25,
                        z.extreme = 25,
                        lt3.exclude.mode = "default",
                        height.tolerance.cm = 2.5,
                        error.load.mincount = 2,
                        error.load.threshold = 0.5,
                        sd.recenter = NA,
                        sdmedian.filename = "",
                        sdrecentered.filename = "",
                        include.carryforward = FALSE,
                        ewma.exp = -1.5,
                        ref.data.path = "",
                        log.path = NA,
                        parallel = FALSE,
                        num.batches = NA,
                        quietly = TRUE,
                        adult_cutpoint = 20,
                        weight_cap = Inf,
                        adult_columns_filename = "",
                        prelim_infants = FALSE) {
  # avoid "no visible binding" warnings
  N <- age_years <- batch <- exclude <- index <- line <- NULL
  newbatch <- sd.median <- sd.orig <- tanner.months <- tbc.sd <- NULL
  v <- v_adult <- whoagegrp.ht <- whoagegrp_ht <- z.orig <- NULL
  z.orig_cdc <- z.orig_who <- sd.orig_cdc <- sd.orig_who <- NULL
  id <- result <- NULL

  sd.orig_uncorr <- agemonths <- intwt <- fengadays <- pmagedays <- cagedays <-
    unmod_zscore <- fen_wt_m <- fen_wt_l <- fen_wt_s <- cwho_cv <- ccdc_cv <-
    sd.c_cdc <- sd.c_who <- sd.c <- sd.corr <- seq_win <- sd.corr_abssumdiff <-
    sd.orig_abssumdiff <- orig_colnames <- ctbc.sd <- sum_sde <- no_sde <-
    sum_val <- no_dup_val <- no_outliers <- no_bigdiff <- nottoofar <- nnte <-
    nnte_full <- NULL

  # preprocessing ----

  # organize data into a dataframe along with a line "index" so the original data order can be recovered
  data.all.ages <- data.table(
    line = seq_along(measurement),
    subjid = as.factor(subjid),
    param,
    agedays = as.integer(agedays),
    v = ifelse(measurement == 0, NaN, measurement),
    v_adult = measurement,
    sex = as.integer(ifelse(
      sex %in% c(0, 'm', 'M'), 0, ifelse(sex %in% c(1, 'f', 'F'), 1, NA)
    ))
  )

  # quality checks
  if (!is.numeric(adult_cutpoint)){
    stop("adult_cutpoint not numeric. Please enter a number between 18 and 20.")
  }
  if (!is.numeric(weight_cap) | weight_cap < 0){
    stop("weight_cap not numeric. Please enter a positive number.")
  }
  if (any(!param %in% c("LENGTHCM", "HEIGHTCM", "WEIGHTKG", "HEIGHIN",
                        "WEIGHTLBS", "HEADCM"))){
    cat(sprintf("[%s] Parameters included that do not match 'param' specifications. Marking as missing...\n", Sys.time()))
    data.all.ages <-
      data.all.ages[
        !param %in% c("LENGTHCM", "HEIGHTCM", "WEIGHTKG", "HEIGHIN",
                      "WEIGHTLBS", "HEADCM"), v := NA]
    data.all.ages <-
      data.all.ages[
        !param %in% c("LENGTHCM", "HEIGHTCM", "WEIGHTKG", "HEIGHIN",
                      "WEIGHTLBS", "HEADCM"), v_adult := NA]
  }

  # rate limit cutpoint -- min 18, max 20
  cutpoint_update <-
    if (adult_cutpoint < 18){
      18
    } else if (adult_cutpoint > 20){
      20
    } else {
      adult_cutpoint
    }

  # split by cutpoint
  # for ease, data.all will refer to pediatric data; data.adult will refer to
  # adult data -- copy to make sure they're separate
  data.all <- copy(data.all.ages[agedays < adult_cutpoint*365.25,])
  data.adult <- copy(data.all.ages[agedays >= adult_cutpoint*365.25,])

  # TODO: ADD PARALLEL FOR ADULTS

  # constants for pediatric
  # enumerate the different exclusion levels
  if (prelim_infants){
    # different for infants
    exclude.levels.peds <- c(
      'Include',
      'Unit-Error-High',
      'Unit-Error-Low',
      'Unit-Error-Possible',
      'Swapped-Measurements',
      'Exclude',
      'Missing',
      'Not cleaned',
      'Exclude-Temporary-Extraneous-Same-Day',
      'Exclude-Carried-Forward',
      # added CF exclusions
      "Exclude-1-CF-deltaZ-<0.05",
      "Exclude-1-CF-deltaZ-<0.1-wholehalfimp",
      "Exclude-Teen-2-plus-CF-deltaZ-<0.05",
      "Exclude-Teen-2-plus-CF-deltaZ-<0.1-wholehalfimp",
      'Exclude-EWMA-Extreme',
      'Exclude-EWMA-Extreme-Pair',
      'Exclude-SDE-Identical',
      'Exclude-SDE-All-Exclude',
      'Exclude-SDE-All-Extreme',
      'Exclude-SDE-EWMA',
      'Exclude-SDE-One-Day',
      "Exclude-EWMA2-middle",
      "Exclude-EWMA2-birth-WT",
      "Exclude-EWMA2-birth-WT-ext",
      "Exclude-EWMA2-first",
      "Exclude-EWMA2-first-ext",
      "Exclude-EWMA2-last",
      "Exclude-EWMA2-last-high",
      "Exclude-EWMA2-last-ext",
      "Exclude-EWMA2-last-ext-high",
      "Exclude-EWMA2-birth-HT-HC",
      "Exclude-EWMA2-birth-HT-HC-ext",
      "Exclude-Min-diff",
      "Exclude-Max-diff",
      "Exclude-2-meas->1-year",
      "Exclude-2-meas-<1-year",
      "Exclude-1-meas",
      "Exclude-Error-load",

      # old

      'Exclude-Extraneous-Same-Day',
      'Exclude-SD-Cutoff',
      'Exclude-EWMA-8',
      'Exclude-EWMA-9',
      'Exclude-EWMA-10',
      'Exclude-EWMA-11',
      'Exclude-EWMA-12',
      'Exclude-EWMA-13',
      'Exclude-EWMA-14',
      'Exclude-Min-Height-Change',
      'Exclude-Max-Height-Change',
      'Exclude-Pair-Delta-17',
      'Exclude-Pair-Delta-18',
      'Exclude-Pair-Delta-19',
      'Exclude-Single-Outlier',
      'Exclude-Too-Many-Errors',
      'Exclude-Too-Many-Errors-Other-Parameter',

      #new
      "Exclude-Absolute-BIV",
      "Exclude-Standardized-BIV",
      "Exclude-Evil-Twins",
      "Exclude-EWMA1-Extreme"
    )
  } else {
    exclude.levels.peds <- c(
      'Include',
      'Unit-Error-High',
      'Unit-Error-Low',
      'Unit-Error-Possible',
      'Swapped-Measurements',
      'Exclude',
      'Missing',
      'Exclude-Temporary-Extraneous-Same-Day',
      'Exclude-Carried-Forward',
      'Exclude-SD-Cutoff',
      'Exclude-EWMA-Extreme',
      'Exclude-EWMA-Extreme-Pair',
      'Exclude-Extraneous-Same-Day',
      'Exclude-EWMA-8',
      'Exclude-EWMA-9',
      'Exclude-EWMA-10',
      'Exclude-EWMA-11',
      'Exclude-EWMA-12',
      'Exclude-EWMA-13',
      'Exclude-EWMA-14',
      'Exclude-Min-Height-Change',
      'Exclude-Max-Height-Change',
      'Exclude-Pair-Delta-17',
      'Exclude-Pair-Delta-18',
      'Exclude-Pair-Delta-19',
      'Exclude-Single-Outlier',
      'Exclude-Too-Many-Errors',
      'Exclude-Too-Many-Errors-Other-Parameter'
    )
  }

  exclude.levels.adult <- c(
    "Include",
    "Exclude-Adult-BIV",
    "Exclude-Adult-Hundreds",
    "Exclude-Adult-Hundreds-RV",
    "Exclude-Adult-Unit-Errors",
    "Exclude-Adult-Unit-Errors-RV",
    "Exclude-Adult-Transpositions",
    "Exclude-Adult-Transpositions-RV",
    "Exclude-Adult-Weight-Cap-Identical",
    "Exclude-Adult-Weight-Cap",
    "Exclude-Adult-Swapped-Measurements",
    "Exclude-Adult-Identical-Same-Day",
    "Exclude-Adult-Extraneous-Same-Day",
    "Exclude-Adult-Distinct-Pairs",
    "Exclude-Adult-Distinct-3-Or-More",
    "Exclude-Adult-EWMA-Extreme",
    "Exclude-Adult-EWMA-Extreme-RV",
    "Exclude-Adult-Distinct-Ordered-Pairs",
    "Exclude-Adult-EWMA-Moderate",
    "Exclude-Adult-Possibly-Impacted-By-Weight-Cap",
    "Exclude-Adult-Distinct-Single",
    "Exclude-Adult-Too-Many-Errors"
  )

  exclude.levels <- base::union(exclude.levels.peds, exclude.levels.adult)

  # if there's no pediatric data, no need to go through this rigamarole
  if (nrow(data.all) > 0){

    # pediatric: height velocity calculations and preprocessing ----

    # for pediatric data, convert in and lbs to cm and kg (adult is done within algo)
    data.all[param == "HEIGHTIN", v := v*2.54]
    data.all[param == "HEIGHTIN", param := "HEIGHTCM"]
    data.all[param == "WEIGHTLBS", v := v/2.2046226]
    data.all[param == "WEIGHTLBS", param := "WEIGHTKG"]

    # if parallel processing is desired, load additional modules
    if (parallel) {
      if (is.na(num.batches)) {
        num.batches <- getDoParWorkers()
      }
      if (prelim_infants){
        # variables needed for parallel workers
        var_for_par <- c("temporary_extraneous", "valid", "swap_parameters",
                         "na_as_false", "ewma", "read_anthro", "as_matrix_delta",
                         "sd_median",

                         "temporary_extraneous_infants",
                         "get_dop", "calc_oob_evil_twins",
                         "calc_and_recenter_z_scores")
      } else {
        var_for_par <- c("temporary_extraneous", "valid", "swap_parameters",
                         "na_as_false", "ewma", "read_anthro", "as_matrix_delta",
                         "sd_median")
      }

      cl <- makeCluster(num.batches)
      clusterExport(cl = cl, varlist = var_for_par, envir = environment())
      registerDoParallel(cl)
    } else {
      if (is.na(num.batches))
        num.batches <- 1
    }

    setkey(data.all, subjid)
    subjid.unique <- data.all[j = unique(subjid)]
    batches.all <- data.table(
      subjid = subjid.unique,
      batch = sample(num.batches, length(subjid.unique), replace = TRUE),
      key = 'subjid'
    )
    data.all <- batches.all[data.all]

    if (!quietly){
      cat(sprintf("[%s] Begin processing pediatric data...\n", Sys.time()))
    }

    # load tanner height velocity data. sex variable is defined such that 0=male and 1=female
    # recode column names to match syntactic style ("." rather than "_" in variable names)
    tanner_ht_vel_path <- ifelse(
      ref.data.path == "",
      system.file(file.path("extdata", "tanner_ht_vel.csv.gz"), package = "growthcleanr"),
      file.path(ref.data.path, "tanner_ht_vel.csv.gz")
    )

    tanner.ht.vel <- fread(tanner_ht_vel_path)

    setnames(tanner.ht.vel,
             colnames(tanner.ht.vel),
             gsub('_', '.', colnames(tanner.ht.vel)))
    setkey(tanner.ht.vel, sex, tanner.months)
    # keep track of column names in the tanner data
    tanner.fields <- colnames(tanner.ht.vel)
    tanner.fields <- tanner.fields[!tanner.fields %in% c('sex', 'tanner.months')]

    if (!prelim_infants){
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
    } else {
      who_max_ht_vel_path <- ifelse(
        ref.data.path == "",
        system.file(file.path("extdata", "who_hc_maxvel_3sd_infants.csv.gz"), package = "growthcleanr"),
        file.path(ref.data.path, "who_hc_maxvel_3sd_infants.csv.gz")
      )

      who_ht_vel_3sd_path <- ifelse(
        ref.data.path == "",
        system.file(file.path("extdata", "who_hc_vel_3sd_infants.csv.gz"), package = "growthcleanr"),
        file.path(ref.data.path, "who_hc_vel_3sd_infants.csv.gz")
      )
    }
    who.max.ht.vel <- fread(who_max_ht_vel_path)
    who.ht.vel <- fread(who_ht_vel_3sd_path)
    setkey(who.max.ht.vel, sex, whoagegrp_ht)
    setkey(who.ht.vel, sex, whoagegrp_ht)
    who.ht.vel <- as.data.table(dplyr::full_join(who.ht.vel, who.max.ht.vel, by =
                                                   c('sex', 'whoagegrp_ht')))

    setnames(who.ht.vel, colnames(who.ht.vel), gsub('_', '.', colnames(who.ht.vel)))
    setkey(who.ht.vel, sex, whoagegrp.ht)
    # keep track of column names in the who growth velocity data
    who.fields <- colnames(who.ht.vel)
    who.fields <- who.fields[!who.fields %in% c('sex', 'whoagegrp.ht')]

    # 1.  General principles
    # a.	All steps are done separately for each parameter unless otherwise noted
    # b.	All steps are done sorted by subject, parameter, and age (in days) for nonexcluded and nonmissing values only unless otherwise noted. This is very important.
    #     Sorting needs to be redone with each step to account for excluded and transformed  values.
    # c.	The next value refers to the value with the next highest age for the same parameter and the same subject, and the previous value refers to the value with the
    #     next lowest age for the same parameter and the same subject.
    # d.	You will need to set up a method for keeping track of whether a value is missing or excluded (and in what step). I use variables called exc_* that are =0
    #     if a value is to be included, =1 if missing, and =2 or higher if it is to be excluded, with each number indicating a different step. I also set up
    #     parameter-specific subjid_* variables that are = to subjid for included values and are blank if the value is missing or should be excluded. These subjid_*
    #     variables need to be updated with each step.
    # e.  All steps assume that data are sorted by subjid_*, parameter, and age (in days) for nonexcluded and nonmissing values only
    #     unless otherwise noted. Sorting needs to be redone after any transformations or exclusions to account for excluded and
    #     transformed values.
    # f.  The next value refers to the nonexcluded nonmissing value with the next highest age for the same parameter and the same
    #     subject, and the previous value refers to the nonexcluded nonmissing value with the next lowest age for the same parameter
    #     and the same subject.
    # g.  exc_* should only be replaced with a  higher value if exc_*==0 at the time of replacement, unless otherwise specified.


    # NOTE: in the R code below exclusion is documented as a series of factor levels, where all levels occuring before 'Exclude' in the sequence are considered
    # to be valid measurements.  We use the built in sorting of the data.table object and subsets rather than re-sorting at each step
    # to ensure that only valid measurements are used at the beginning of each step.
    # Also, unlike the Stata code, the measurement parameter (weight vs. height) is recorded as a factor in the data frame, rather than as a variable name

    # 2.  Data set-up
    # a.	I always code sex as 0=Male, 1=Female, so I recoded the variable sex that way and left a variable sexorigcode the way the data was sent to me (1=Female 2=Male)
    # b.	Remove rows that are extraneous for subjid, param, and measurement from further analysis
    #     NOTE: this step is not needed -- handled automatically by "temporary extraneous" step.
    # c.  I generated separate variables for weight (wt) and height (ht), as well as exc_* and subjid_* variables. Set exc_*=0 if value is not missing
    #     and exc_*=1 if value is missing. In all future steps, exc_* should only be changed if it is 0. This helps to keep track of which step excluded a value.
    #     I also kept the measurement variable there and untouched because sometimes wt and ht got transformed to something else.
    # d.	I made tables based on CDC growth curve parameters that include data for each day that I will send separately. The LMS parameters for each day are
    #     cubically interpolated from the values by month available on the CDC website. Create wt and ht z-scores for each value of each parameter (Z: WtZ and HtZ).
    # e.	There are variables in the table labelled cdc_*_csd_pos and cdc_*_csd_neg. For each age and sex, these correspond to ½ of the absolute value of the
    #     median and the value with a z-score of +2 (csd_pos) and -2 (csd_neg). These can be created to generate a score similar to the z-score but with an
    #     important difference. The z-scores created using the LMS method account for skewness in the distribution of the parameters (particularly weight), which
    #     can lead to small changes in z-score with large changes in weight in subjects with very high weight, and relatively large changes in z-score for smaller
    #     changes in weight in subjects with low weights.  The score we will create can be called an SD-score (SDorig: WtSDorig and HtSDorig that is calculated by
    #     dividing the difference between the value and the median by the SD score (use csd_pos if the value is above the median, csd_neg if the value is below the
    #     median). These SD-scores, rather than z-scores, now form the basis for the algorithm.

    # recategorize linear parameters as 'HEIGHTCM'
    # NOTE: this will be changed in future to consider this difference
    data.all[param == 'LENGTHCM', param := 'HEIGHTCM']

    # calculate z/sd scores
    if(prelim_infants){
      if (!quietly)
        cat(sprintf("[%s] Calculating z-scores...\n", Sys.time()))
      # removing z calculations, as they are not used
      # for infants, use z and who
      measurement.to.z <- read_anthro(ref.data.path, cdc.only = TRUE,
                                      prelim_infants = TRUE)
      measurement.to.z_who <- read_anthro(ref.data.path, cdc.only = FALSE,
                                          prelim_infants = TRUE)

      # calculate "standard deviation" scores
      if (!quietly)
        cat(sprintf("[%s] Calculating SD-scores...\n", Sys.time()))
      data.all[, sd.orig_cdc := measurement.to.z(param, agedays, sex, v, TRUE)]
      data.all[, sd.orig_who := measurement.to.z_who(param, agedays, sex, v, TRUE)]

      # smooth z-scores/SD scores between ages 1 - 3yo using weighted scores
      # older uses cdc, younger uses who
      data.all$ageyears <- data.all$agedays/365.25

      who_weight <- 4 - (data.all$ageyears)
      cdc_weight <- (data.all$ageyears) - 2

      smooth_val <- data.all$ageyears >= 2 &
        data.all$ageyears <= 4 &
        data.all$param != "HEADCM"
      data.all[smooth_val,
               sd.orig := (data.all$sd.orig_cdc[smooth_val]*cdc_weight[smooth_val] +
                             data.all$sd.orig_who[smooth_val]*who_weight[smooth_val])/2]

      # otherwise use WHO and CDC for older and younger, respectively
      who_val <- data.all$param == "HEADCM" |
        data.all$ageyears < 2

      data.all[(who_val & !smooth_val) | (smooth_val & is.na(data.all$sd.orig_cdc)),
               sd.orig := data.all$sd.orig_who[(who_val & !smooth_val)  | (smooth_val & is.na(data.all$sd.orig_cdc))]]

      cdc_val <- data.all$param != "HEADCM" &
        data.all$ageyears > 4

      data.all[(cdc_val & !smooth_val) | (smooth_val & is.na(data.all$sd.orig_who)),
               sd.orig := data.all$sd.orig_cdc[(cdc_val & !smooth_val) | (smooth_val & is.na(data.all$sd.orig_who))]]

      # NOTE: SD SCORES IN CODE ARE Z IN INFANT DOCS -- USE sd.orig ONLY

      # keep the original, uncorrected, unrecentered zscores
      data.all[,sd.orig_uncorr := sd.orig]

      # NOTE: MAY WANT TO SUBSET HERE

      # 2b: corrected z scores ----

      # keep the original column names -- we're adding a ton of columns that we
      # want to filter out after correction
      orig_colnames <- copy(colnames(data.all))

      # start by reading in approximated fenton reference values
      ref_for_corraga <- fread(
        system.file(file.path("extdata", "ref_for_corraga.csv.gz"),
                    package = "growthcleanr"))
      ref_for_corrz <- fread(
        system.file(file.path("extdata", "ref_for_corrz.csv.gz"),
                    package = "growthcleanr"))

      # add age in months
      data.all[, agemonths := agedays/30.4375]

      potcorr <- data.all$param == "WEIGHTKG" &
        data.all$sd.orig < -2 &
        data.all$agemonths < 10

      # integer weight is in grams, rounded to the nearest 10
      data.all[potcorr, intwt := round(v*100)*10]
      # replace to facilitate merging with approximated Fenton reference values
      data.all[intwt >= 250 & intwt <=560, intwt := 570]

      # merge with approximated Fenton reference values
      data.all <- merge(
        data.all, ref_for_corraga, by = c("sex", "intwt"),
        all.x = TRUE)

      data.all[fengadays < 259, pmagedays := agedays + fengadays]
      data.all[fengadays < 259, cagedays := pmagedays - 280]
      # replace fengadays with pmagedays to facilitate merging
      data.all[, fengadays := pmagedays]

      # merge with approximated Fenton reference values
      data.all <- merge(
        data.all, ref_for_corrz, by = c("sex", "fengadays"),
        all.x = TRUE)

      # add unmodified zscore using weight in unrounded grams
      data.all[, unmod_zscore :=
                 ((v*1000/fen_wt_m)^fen_wt_l - 1)/(fen_wt_l * fen_wt_s)]

      # read in who/cdc data
      growthfile_who <- fread(
        system.file(file.path("extdata", "growthfile_who.csv.gz"),
                    package = "growthcleanr"))
      growthfile_cdc <- fread(
        system.file(file.path("extdata", "growthfile_cdc_ext_infants.csv.gz"),
                    package = "growthcleanr"))

      # merge in the who/cdc data
      data.all <- merge(data.all, growthfile_who,
                        by.x = c("sex", "cagedays"),
                        by.y = c("sex", "agedays"),
                        all.x = TRUE)
      data.all <- merge(data.all, growthfile_cdc,
                        by.x = c("sex", "cagedays"),
                        by.y = c("sex", "agedays"),
                        all.x = TRUE)

      # adjust WHO and CDC heights based on age
      data.all[, cwho_cv := v]
      data.all[, ccdc_cv := v]
      data.all[param == "HEIGHTCM" & agedays > 730 & cagedays <= 730,
               cwho_cv := cwho_cv + 0.8]
      data.all[param == "HEIGHTCM" & agedays > 730 & cagedays <= 730,
               ccdc_cv := ccdc_cv + 0.7]

      # create the corrected z scores
      # use read anthro, but pass in different arguments
      # pass in the corrected height
      data.all[, sd.c_cdc :=
                 measurement.to.z(param, cagedays, sex, ccdc_cv, TRUE)]
      data.all[, sd.c_who :=
                 measurement.to.z_who(param, cagedays, sex, cwho_cv, TRUE)]

      # smooth using weights as in original z score creation
      who_weight <- 4 - (data.all$agedays/365.25)
      cdc_weight <- (data.all$agedays/365.25) - 2

      smooth_val <- data.all$agedays/365.25 >= 2 &
        data.all$agedays/365.25 <= 4 &
        data.all$param != "HEADCM"
      data.all[smooth_val,
               sd.c := (sd.c_cdc[smooth_val]*cdc_weight[smooth_val] +
                             sd.c_who[smooth_val]*who_weight[smooth_val])/2]

      # otherwise use WHO and CDC for older and younger, respectively
      who_val <- data.all$param == "HEADCM" |
        data.all$agedays/365.25 < 2
      data.all[who_val | (smooth_val & is.na(data.all$sd.c_cdc)),
               sd.c := data.all$sd.c_who[who_val  | (smooth_val & is.na(data.all$sd.c_cdc))]]

      cdc_val <- data.all$param != "HEADCM" |
        data.all$agedays/365.25 > 4
      data.all[cdc_val | (smooth_val & is.na(data.all$sd.c_who)),
               sd.c := data.all$sd.c_cdc[cdc_val | (smooth_val & is.na(data.all$sd.c_who))]]

      # smooth corrected and uncorrected z scores
      uncorrweight <-  4 - (data.all$agedays/365.25)
      corrweight <- (data.all$agedays/365.25) - 2
      smooth_val <- data.all$agedays/365.25 >= 2 &
        data.all$agedays/365.25 <= 4
      data.all[smooth_val,
               sd.corr := (sd.c[smooth_val]*corrweight[smooth_val] +
                             sd.orig[smooth_val]*uncorrweight[smooth_val])/2]
      # for < 2 & potential correction, use corrected score
      data.all[agedays/365.25 <= 2 & potcorr, sd.corr := sd.c]
      # for > 4, use original z score
      data.all[agedays/365.25 >= 4, sd.corr := sd.orig]
      # for not potential corrections, use the original z score
      data.all[!potcorr, sd.corr := sd.orig]
      # if the who/corrected score is not available for any reason, use the
      # original
      data.all[is.na(sd.corr), sd.corr := sd.orig]

      # check for consistent growth
      examine_only <- data.all$param == "WEIGHTKG" &
        data.all$subjid %in% data.all$subjid[potcorr]
      tmp <- copy(data.all[examine_only,])
      tmp <- tmp[order(subjid, agedays),]
      tmp[, seq_win := sequence(.N), by = subjid]
      # we're only looking at the first 4 values, and they need to be < 2 years
      tmp <- tmp[seq_win <= 4 & (agedays/365.25) < 2,]
      # don't look at subjects where there is only 1 score and there is no
      # value for either
      tmp <- tmp[!(is.na(sd.corr & sd.orig)),]
      tmp <- tmp[subjid %in% names(table(subjid) > 1),]
      # create differences, absolute sum them
      tmp[, sd.corr_abssumdiff := abs(sum(sd.corr[1] - sd.corr)), by = subjid]
      tmp[, sd.orig_abssumdiff := abs(sum(sd.orig[1] - sd.orig)), by = subjid]
      # find subjects where corrected value needs to be replaced
      sub_replace <- unique(tmp[sd.corr_abssumdiff > sd.orig_abssumdiff,
                                subjid])

      # replace accordingly in the main dataframe
      data.all[subjid %in% sub_replace, sd.corr := sd.orig]

      orig_colnames <- c(orig_colnames, "sd.corr")

      # remove many added columns
      data.all <- data.all[, colnames(data.all) %in% orig_colnames,
                           with = FALSE]
    } else {
      # calculate z scores
      if (!quietly)
        cat(sprintf("[%s] Calculating z-scores...\n", Sys.time()))
      measurement.to.z <- read_anthro(ref.data.path, cdc.only = TRUE)
      data.all[, z.orig := measurement.to.z(param, agedays, sex, v)]

      # calculate "standard deviation" scores
      if (!quietly)
        cat(sprintf("[%s] Calculating SD-scores...\n", Sys.time()))
      data.all[, sd.orig := measurement.to.z(param, agedays, sex, v, TRUE)]
    }

    # sort by subjid, param, agedays
    setkey(data.all, subjid, param, agedays)

    # add a new convenience index for bookkeeping
    data.all[, index := 1:.N]

    # Mark missing values for exclusion
    data.all[, exclude := factor(with(data.all, ifelse(
      is.na(v) |
        agedays < 0, 'Missing', 'Include'
    )),
    levels = exclude.levels,
    ordered = TRUE)]
    # also mark certain measurements to not consider
    data.all[param == "HEADCM" & agedays > (3*365.25), exclude := "Not cleaned"]

    # define field names needed by helper functions
    ewma.fields <- c('ewma.all', 'ewma.before', 'ewma.after')

    # 3.  SD-score recentering: Because the basis of the method is comparing SD-scores over time, we need to account for the fact that
    #     the mean SD-score for the population changes with age.
    # a.	Determine the median cdc*sd for each parameter by year of age (with sexes combined): median*sd.
    # b.	The median*sd should be considered to apply to midyear-age, defined as the age in days with the same value as the integer
    #     portion of (365.25*year + 365.25/2).
    # c.	Linearly interpolate median*sd for each parameter between each midyear-age, naming the interpolated values rc*sd.
    # d.	For ages below the first midyear-age, let rc*sd equal the median*sd for the earliest year.
    #     For ages above the last midyear_age, let rc*sd equal the median*sd for the last year.
    # e.	Subtract rcsd_* from SDorig to create the recentered SD-score.  This recentered SD-score, labeled tbc*sd
    #     (stands for "to be cleaned") will be used for most of the rest of the analyses.
    # f.	In future steps I will sometimes refer to measprev and measnext which refer to the previous or next wt or ht measurement
    #     for which exc_*==0 for the subject and parameter, when the data are sorted by subject, parameter, and agedays. SDprev and SDnext refer to the tbc*sd of the previous or next measurement.

    if (!quietly)
      cat(sprintf("[%s] Re-centering data...\n", Sys.time()))

    # see function definition below for explanation of the re-centering process
    # returns a data table indexed by param, sex, agedays. can use NHANES reference
    # data, derive from input, or use user-supplied data.
    if (!is.data.table(sd.recenter)) {
      # INFANTS CHANGES:
      # use recentering file derived from work, independent of sex
      if (prelim_infants){
        infants_reference_medians_path <- ifelse(
          ref.data.path == "",
          system.file(file.path("extdata",
                                "rcfile-2023-08-15_format.csv.gz"),
                      package = "growthcleanr"),
          file.path(ref.data.path, "rcfile-2023-08-15_format.csv.gz")
        )
        sd.recenter <- fread(infants_reference_medians_path)
        if (!quietly)
          cat(
            sprintf(
              "[%s] Using infants reference medians...\n",
              Sys.time()
            )
          )
      } else if ((is.character(sd.recenter) &
                  tolower(sd.recenter) == "nhanes") |
          (!(is.character(sd.recenter) &
             tolower(sd.recenter) == "derive") & (data.all[, .N] < 5000))) {

        # Use NHANES medians if the string "nhanes" is specified instead of a data.table
        # or if sd.recenter is not specified as "derive" and N < 5000.

        nhanes_reference_medians_path <- ifelse(
          ref.data.path == "",
          system.file(file.path("extdata", "nhanes-reference-medians.csv.gz"), package = "growthcleanr"),
          file.path(ref.data.path, "nhanes-reference-medians.csv.gz")
        )
        sd.recenter <- fread(nhanes_reference_medians_path)
        if (!quietly)
          cat(
            sprintf(
              "[%s] Using NHANES reference medians...\n",
              Sys.time()
            )
          )
      } else {
        # Derive medians from input data
        sd.recenter <- data.all[exclude < 'Exclude', sd_median(param, sex, agedays, sd.orig)]
        if (!quietly)
          cat(
            sprintf(
              "[%s] Using re-centering medians derived from input...\n",
              Sys.time()
            )
          )
        if (sdmedian.filename != "") {
          write.csv(sd.recenter, sdmedian.filename, row.names = FALSE)
          if (!quietly)
            cat(
              sprintf(
                "[%s] Wrote re-centering medians to %s...\n",
                Sys.time(),
                sdmedian.filename
              )
            )
        }
      }
    } else {
      # Use specified data
      if (!quietly)
        cat(
          sprintf(
            "[%s] Using specified re-centering medians...\n",
            Sys.time()
          )
        )
    }

    # ensure recentering medians are sorted correctly
    setkey(sd.recenter, param, sex, agedays)

    # add sd.recenter to data, and recenter
    setkey(data.all, param, sex, agedays)
    data.all <- sd.recenter[data.all]

    setkey(data.all, subjid, param, agedays)
    data.all[, tbc.sd := sd.orig - sd.median]
    if (prelim_infants){
      # separate out corrected and noncorrected values
      data.all[, ctbc.sd := sd.corr - sd.median]
    }

    if (sdrecentered.filename != "") {
      write.csv(data.all, sdrecentered.filename, row.names = FALSE)
      if (!quietly)
        cat(
          sprintf(
            "[%s] Wrote re-centered data to %s...\n",
            Sys.time(),
            sdrecentered.filename
          )
        )
    }

    # notification: ensure awareness of small subsets in data
    if (!quietly) {
      year.counts <- data.all[, .N, floor(agedays / 365.25)]
      if (year.counts[N < 100, .N] > 0) {
        cat(
          sprintf(
            "[%s] Note: input data has at least one age-year with < 100 subjects...\n",
            Sys.time()
          )
        )
      }
    }

    # safety check: treat observations where tbc.sd cannot be calculated as missing
    data.all[is.na(tbc.sd), exclude := 'Missing']

    if (prelim_infants){
      # 4: identify subset that don't need to be cleaned (nnte) ----

      # identify those meeting all subjects meeting these criteria as "no need
      # to ewma"
      # does the subject have sdes

      # keep the original column names -- we're adding a ton of columns that we
      # want to filter out after correction
      orig_colnames <- copy(colnames(data.all))
      # no SDEs
      data.all[, sum_sde := .N, by = c("subjid", "param", "agedays")]
      data.all[, no_sde := sum_sde == 1]
      # does the subject have identical values
      data.all[, sum_val := .N, by = c("subjid", "param", "v")]
      data.all[, no_dup_val := sum_val == 1]
      # all tbc.sd are within [-3,3] -- 0 is false
      data.all[, no_outliers := sum((tbc.sd > -3 & tbc.sd < 3) |
                                      is.na(tbc.sd)) == (.N),
               by = c("subjid", "param")]
      data.all[, no_outliers := no_outliers == 1]
      # all max - min tbd.sc < 2.5
      data.all[, no_bigdiff :=
                 rep((abs(max(tbc.sd, na.rm = TRUE) - min(tbc.sd, na.rm = TRUE)) < 2.5),
                     .N),
               by = c("subjid", "param")]
      # the previous value can't be too far from the current value
      data.all[, seq_win := sequence(.N), by = c("subjid", "param")]
      data.all[, nottoofar :=
                 (abs(tbc.sd - dplyr::lag(tbc.sd)) < 1 | seq_win == 1) &
                 (abs(tbc.sd - dplyr::lead(tbc.sd)) < 1 | seq_win == .N),
               by = c("subjid", "param")]
      data.all[is.na(nottoofar),  nottoofar :=  TRUE]

      # cumulative: no need to ewma -- needs to work for all within a subject &
      # parameter
      data.all[, nnte := no_sde & no_dup_val & no_outliers & no_bigdiff & nottoofar]
      # NNTE can be calculated by parameter -- but it's occasionally easier for
      # calculations to require all parameters to be nnte
      data.all[, nnte_full := sum(nnte) == .N, by = c("subjid", "param")]
      data.all[, nnte := sum(nnte) == .N, by = c("subjid")]


      # remove many added columns -- except for nnte
      orig_colnames <- c(orig_colnames, "nnte", "nnte_full")
      data.all <- data.all[, colnames(data.all) %in% orig_colnames,
                           with = FALSE]
    }
    # pediatric: cleanbatch (most of steps) ----

    # NOTE: the rest of cleangrowth's steps are done through cleanbatch().

    # optionally process batches in parallel
    if (!quietly)
      cat(sprintf(
        "[%s] Cleaning growth data in %d batch(es)...\n",
        Sys.time(),
        num.batches
      ))
    if (num.batches == 1) {
      if (!prelim_infants){
        ret.df <- cleanbatch(data.all,
                             log.path = log.path,
                             quietly = quietly,
                             parallel = parallel,
                             measurement.to.z = measurement.to.z,
                             ewma.fields = ewma.fields,
                             ewma.exp = ewma.exp,
                             recover.unit.error = recover.unit.error,
                             include.carryforward = include.carryforward,
                             sd.extreme = sd.extreme,
                             z.extreme = z.extreme,
                             exclude.levels = exclude.levels,
                             tanner.ht.vel = tanner.ht.vel,
                             who.ht.vel = who.ht.vel,
                             lt3.exclude.mode = lt3.exclude.mode,
                             error.load.threshold = error.load.threshold,
                             error.load.mincount = error.load.mincount)
      } else {
        ret.df <- cleanbatch_infants(
          data.all,
          log.path = log.path,
          quietly = quietly,
          parallel = parallel,
          measurement.to.z = measurement.to.z,
          ewma.fields = ewma.fields,
          recover.unit.error = recover.unit.error,
          include.carryforward = include.carryforward,
          sd.extreme = sd.extreme,
          z.extreme = z.extreme,
          exclude.levels = exclude.levels,
          tanner.ht.vel = tanner.ht.vel,
          who.ht.vel = who.ht.vel,
          lt3.exclude.mode = lt3.exclude.mode,
          error.load.threshold = error.load.threshold,
          error.load.mincount = error.load.mincount,
          ref.data.path = ref.data.path)
      }
    } else {
      # create log directory if necessary
      if (!is.na(log.path)) {
        cat(sprintf("[%s] Writing batch logs to '%s'...\n", Sys.time(), log.path))
        ifelse(!dir.exists(log.path), dir.create(log.path, recursive = TRUE), FALSE)
      }

      if (!prelim_infants){
        ret.df <- ddply(
          data.all,
          .(batch),
          cleanbatch,
          .parallel = parallel,
          .paropts = list(.packages = "data.table"),
          log.path = log.path,
          quietly = quietly,
          parallel = parallel,
          measurement.to.z = measurement.to.z,
          ewma.fields = ewma.fields,
          ewma.exp = ewma.exp,
          recover.unit.error = recover.unit.error,
          include.carryforward = include.carryforward,
          sd.extreme = sd.extreme,
          z.extreme = z.extreme,
          exclude.levels = exclude.levels,
          tanner.ht.vel = tanner.ht.vel,
          who.ht.vel = who.ht.vel,
          lt3.exclude.mode = lt3.exclude.mode,
          error.load.threshold = error.load.threshold,
          error.load.mincount = error.load.mincount
        )
      } else {
        ret.df <- ddply(
          data.all,
          .(batch),
          cleanbatch_infants,
          .parallel = parallel,
          .paropts = list(.packages = "data.table"),
          log.path = log.path,
          quietly = quietly,
          parallel = parallel,
          measurement.to.z = measurement.to.z,
          ewma.fields = ewma.fields,
          recover.unit.error = recover.unit.error,
          include.carryforward = include.carryforward,
          sd.extreme = sd.extreme,
          z.extreme = z.extreme,
          exclude.levels = exclude.levels,
          tanner.ht.vel = tanner.ht.vel,
          who.ht.vel = who.ht.vel,
          lt3.exclude.mode = lt3.exclude.mode,
          error.load.threshold = error.load.threshold,
          error.load.mincount = error.load.mincount,
          ref.data.path = ref.data.path
        )
      }
      stopCluster(cl)
    }


    if (!quietly)
      cat(sprintf("[%s] Done with pediatric data!\n", Sys.time()))
  } else {
    ret.df <- data.table()

    if (!quietly)
      cat(sprintf("[%s] No pediatric data. Moving to adult data...\n", Sys.time()))
  }

  # adult: send to cleanadult to do most of the work ----

  # no need to do this if there's no data
  if (nrow(data.adult) > 0){
    if (!quietly){
      cat(sprintf("[%s] Begin processing adult data...\n", Sys.time()))
    }

    # if parallel processing is desired, load additional modules
    if (parallel) {
      if (is.na(num.batches)) {
        num.batches <- getDoParWorkers()
      }
      # variables needed for parallel workers
      var_for_par <- c(
        "cleanadult", "check_between", "round_pt", "get_float_rem",
        "as.matrix.delta_dn", "ewma_dn", "remove_biv", "remove_biv_high",
        "remove_biv_low", "identify_rv", "temp_sde", "redo_identify_rv",
        "rem_hundreds", "rem_unit_errors", "get_num_places", "switch_tens_ones",
        "rem_transpositions", "ht_allow", "ht_change_groups",
        "ht_3d_growth_compare", "remove_ewma_wt", "remove_mod_ewma_wt"
      )

      cl <- makeCluster(num.batches)
      clusterExport(cl = cl, varlist = var_for_par, envir = environment())
      registerDoParallel(cl)
    } else {
      if (is.na(num.batches))
        num.batches <- 1
    }

    # this is where we do most of the adult work
    # is the randomness necessary here?
    subjid.unique <- data.adult[j = unique(subjid)]
    batches.adult <- data.table(
      subjid = subjid.unique,
      newbatch = sample(num.batches, length(subjid.unique), replace = TRUE)
    )
    data.adult <- merge(data.adult, batches.adult, by = "subjid")

    # add age in years
    data.adult[, age_years := agedays/365.25]
    # rename for ease of use
    data.adult[, measurement := v_adult]
    data.adult[, id := line]

    if (num.batches == 1) {
      # do the cleaning
      res <- cleanadult(data.adult, weight_cap = weight_cap)
    } else {
      res <- ddply(
        data.adult,
        .(newbatch),
        cleanadult,
        .parallel = parallel,
        .paropts = list(.packages = "data.table"),
        weight_cap = weight_cap
      )

      res <- as.data.table(res)
    }

    # replace result with missing if measurement or agedays are missing
    res[is.na(measurement) | agedays < 0, result := "Missing"]

    if (parallel){
      stopCluster(cl)
    }

    if (adult_columns_filename != "") {
      write.csv(res, adult_columns_filename, row.names = FALSE, na = "")
      if (!quietly){
        cat(
          sprintf(
            "[%s] Wrote adult data with additional columns to to %s...\n",
            Sys.time(),
            adult_columns_filename
          )
        )
      }
    }

    if (!quietly)
      cat(sprintf("[%s] Done with adult data!\n", Sys.time()))
  } else {
    res <- data.table()

    if (!quietly){
      cat(sprintf("[%s] No adult data. Moving to postprocessing...\n", Sys.time()))
    }
  }

  if (any(nrow(data.all) > 0, nrow(data.adult) > 0)) {
    # join with pediatric data
    full_out <- data.table(
      line = c(ret.df$line, res$line),
      exclude = c(as.character(ret.df$exclude), res$result),
      mean_sde = c(rep(NA, nrow(ret.df)), res$mean_sde)
    )
    full_out[, exclude := factor(exclude, levels = exclude.levels)]
    full_out <- full_out[order(line),]
    # remove column added for keeping track
    full_out[, line := NULL]

    return(full_out$exclude)
  } else {
    return(c())
  }

}

#' Function to calculate z-scores and csd-scores based on anthro tables.
#'
#' @param path Path to supplied reference anthro data. Defaults to package anthro tables.
#' @param cdc.only Whether or not only CDC data should be used. Defaults to false.
#' @param prelim_infants TRUE/FALSE. Run the in-development release of the infants algorithm (expands pediatric algorithm to improve performance for children 0 – 2 years). Not recommended for use in research. For more information regarding the logic of the algorithm, see the vignette 'Preliminary Infants Algorithm.' Defaults to FALSE.
#'
#' @return Function for calculating BMI based on measurement, age in days, sex, and measurement value.
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
read_anthro <- function(path = "", cdc.only = FALSE, prelim_infants = FALSE) {
  # avoid "no visible bindings" warning
  src <- param <- sex <- age <- ret <- m <- NULL
  csdneg <- csdpos <- s <- NULL

  # set correct path based on input reference table path (if any)
  if (!prelim_infants){
    weianthro_path <- ifelse(
      path == "",
      system.file(file.path("extdata", "weianthro.txt.gz"), package = "growthcleanr"),
      file.path(path, "weianthro.txt.gz")
    )
    lenanthro_path <- ifelse(
      path == "",
      system.file(file.path("extdata", "lenanthro.txt.gz"), package = "growthcleanr"),
      file.path(path, "lenanthro.txt.gz")
    )
    bmianthro_path <- ifelse(
      path == "",
      system.file(file.path("extdata", "bmianthro.txt.gz"), package = "growthcleanr"),
      file.path(path, "bmianthro.txt.gz")
    )
    growth_cdc_ext_path <- ifelse(
      path == "",
      system.file(file.path("extdata", "growthfile_cdc_ext.csv.gz"), package = "growthcleanr"),
      file.path(path, "growthfile_cdc_ext.csv.gz")
    )
  } else {
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
  }
  growth_cdc_ext <- read.csv(gzfile(growth_cdc_ext_path))

  l <- if (!prelim_infants){
    list(
      with(
        read.table(gzfile(weianthro_path), header = TRUE),
        data.frame(
          src = 'WHO',
          param = 'WEIGHTKG',
          sex = sex - 1,
          age,
          l,
          m,
          s,
          csdpos = as.double(NA),
          csdneg = as.double(NA)
        )
      ),
      with(
        read.table(gzfile(lenanthro_path), header = TRUE),
        data.frame(
          src = 'WHO',
          param = 'HEIGHTCM',
          sex = sex - 1,
          age,
          l,
          m,
          s,
          csdpos = as.double(NA),
          csdneg = as.double(NA)
        )
      ),
      with(
        read.table(gzfile(bmianthro_path), header = TRUE),
        data.frame(
          src = 'WHO',
          param = 'BMI',
          sex = sex - 1,
          age,
          l,
          m,
          s,
          csdpos = as.double(NA),
          csdneg = as.double(NA)
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
  } else {
    list(
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
  }

  anthro <- rbindlist(l)


  setkey(anthro, src, param, sex, age)

  return(function(param, agedays, sex, measurement, csd = FALSE) {
    # For now, we will only use CDC growth reference data, note that the cubically interpolated file
    # we are using has linear measurments derived from length data for children < 731 days, and height thereafter
    src <- ifelse(agedays < 3*365.25 & !cdc.only, 'WHO', 'CDC')

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

#' Exponentially Weighted Moving Average (EWMA)
#'
#' \code{ewma} calculates the exponentially weighted moving average (EWMA) for a set of numeric observations over time.
#'
#' @param agedays Vector of age in days for each z score (potentially transformed to adjust weighting).
#'
#' @param z Input vector of numeric z-score data.
#'
#' @param ewma.exp Exponent to use for weighting.
#'
#' @param ewma.adjacent Specify whether EWMA values excluding adjacent measurements should be calculated.  Defaults to TRUE.
#'
#' @return Data frame with 3 variables:
#' * The first variable (ewma.all) contains the EWMA at observation time
#'   excluding only the actual observation for that time point.
#' * The second variable (ewma.before) contains the EWMA for each observation excluding both the actual observation
#'   and the immediate prior observation.
#' * The third variable (ewma.after) contains the EWMA for each observation excluding both the actual observation
#'   and the subsequent observation.
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
ewma <- function(agedays, z, ewma.exp, ewma.adjacent = TRUE) {
  # 6.  EWMA calculation description: Most of the next steps will involve calculating the exponentially weighted moving average for each subject and parameter. I will
  #     describe how to calculate EWMASDs, and will describe how it needs to be varied in subsequent steps.
  # a.	The overall goal of the EWMASD calculation is to identify the difference between the SD-score and what we might predict that DS-score should be, in order to
  #     determine whether it should be excluded.
  # b.	Only nonmissing SD-scores for a parameter that are not designated for exclusion are included in the following calculations.
  # c.	For each SD-score SDi and associated agedaysi calculate the following for every other z-score (SDj…SDn) and associated agedays (agedaysj…agedaysn)  for the
  #     same subject and parameter
  #   i.	ΔAgej=agedaysj-agedaysi
  #   ii.	EWMAZ=SDi=[Σj→n(SDj*((5+ΔAgej)^-1.5))]/[ Σj→n((5+ΔAgej)^-1.5)]
  #   iii.	For most EWMASD calculations, there are 3 EWMASDs that need to be calculated. I will note if not all of these need to be done for a given step.
  #     1.	EWMASDall calculated as above
  #     2.	EWMAZbef calculated excluding the SD-score just before the SD-score of interest (sorted by agedays). For the first observation for a parameter for a
  #         subject, this should be identical to EWMASDall rather than missing.
  #     3.	EWMAZaft calculated excluding the z-score just after the SD-score of interest (sorted by agedays). For the lastobservation for a parameter for a subject,
  #         this should be identical to EWMASDall rather than missing.
  #   iv.	For each of the three EWMASDs, calculate the dewma_*=SD-EWMASD
  # d.	EWMASDs and ΔEWMASDs will change if a value is excluded or manipulated using one of the methods below, therefore EWMASDs and ΔEWMASDs be recalculated for each
  #     step where they are needed.
  # e.	For these calculations, use variables that allow for precise storage of numbers (in Stata this is called 'double') because otherwise rounding errors can cause
  #     problems in a few circumstances

  n <- length(agedays)
  # initialize response variables
  ewma.all <- ewma.before <- ewma.after <- vector('numeric', 0)
  if (n > 0) {
    # organize into data frame and sort into order of increasing age,
    # but retain original sort order information in index
    if (!all(agedays == cummax(agedays)))
      warning("EWMA ordering is not sorted; double check") #add in a check to make sure the inputs are already sorted (they should be)
    index <- order(agedays)


    # calculate matrix of differences in age, and add 5 to each delta per Daymont algorithm
    delta <- as_matrix_delta(agedays)
    delta <- ifelse(delta == 0, 0, (delta + 5) ^ ewma.exp)

    # calculate EWMAs, and return in order of original data
    ewma.all[index] <- delta %*% z / apply(delta, 1, sum)

    if (ewma.adjacent) {
      if (n > 2) {
        delta2 <- delta
        delta2[col(delta2) == row(delta2) - 1] <- 0
        ewma.before[index] <- delta2 %*% z / apply(delta2, 1, sum)
        delta3 <- delta
        delta3[col(delta3) == row(delta3) + 1] <- 0
        ewma.after[index] <- delta3 %*% z / apply(delta3, 1, sum)
      } else {
        ewma.before <- ewma.after <- ewma.all
      }
    }
  }
  # return all 3 EWMAs as a data frame
  return(if (ewma.adjacent)
    data.frame(ewma.all, ewma.before, ewma.after)
    else
      data.frame(ewma.all))
}

#' Calculate median SD score by age for each parameter.
#'
#' @param param Vector identifying each measurement, may be 'WEIGHTKG', or 'HEIGHTCM'.
#' @param agedays Numeric vector containing the age in days at each measurement.
#' @param sex Vector identifying the gender of the subject, may be 'M', 'm', or 0 for males, vs. 'F', 'f' or 1 for females.
#' @param sd.orig Vector of previously calculated standard deviation (SD) scores for each measurement before re-centering.
#'
#' @return Table of data with median SD-scores per day of life by gender and parameter.
#'
#' @export
#' @import data.table
#' @examples
#' # Run on 1 subject
#' df_stats <- as.data.frame(syngrowth)
#' df_stats <- df_stats[df_stats$subjid == df_stats$subjid[1], ]
#'
#' # Get the original standard deviations
#' measurement_to_z <- read_anthro(cdc.only = TRUE)
#' sd.orig <- measurement_to_z(df_stats$param,
#'                        df_stats$agedays,
#'                        df_stats$sex,
#'                        df_stats$measurement,
#'                        TRUE)
#'
#' # Calculate median standard deviations
#' sd.m <- sd_median(df_stats$param,
#'                   df_stats$sex,
#'                   df_stats$agedays,
#'                   sd.orig)
sd_median <- function(param, sex, agedays, sd.orig) {
  # 3.  SD-score recentering: Because the basis of the method is comparing SD-scores over time, we need to account for the fact that
  #     the mean SD-score for the population changes with age.
  # a.  Determine the median cdc*sd for each parameter by year of age (with sexes combined): median*sd.
  # b.	The median*sd should be considered to apply to midyear-age, defined as the age in days with the same value as the integer
  #     portion of (365.25*year + 365.25/2).
  # c.	Linearly interpolate median*sd for each parameter between each midyear-age, naming the interpolated values rc*sd.
  # d.	For ages below the first midyear-age, let rc*sd equal the median*sd for the earliest year.
  #     For ages above the last midyear_age, let rc*sd equal the median*sd for the last year.
  # e.	Subtract rcsd_* from SDorig to create the recentered SD-score.  This recentered SD-score, labeled tbc*sd
  #     (stands for "to be cleaned") will be used for most of the rest of the analyses.
  # f.	In future steps I will sometimes refer to measprev and measnext which refer to the previous or next wt or ht measurement
  #     for which exc_*==0 for the subject and parameter, when the data are sorted by subject, parameter, and agedays. SDprev and SDnext refer to the tbc*sd of the previous or next measurement.

  # avoid "no visible binding" warnings
  ageyears <- sd.median <- NULL

  dt <- data.table(param, sex, agedays, ageyears = floor(agedays / 365.25), sd.orig)
  setkey(dt, param, sex, agedays)
  # determine ages (in days) we need to cover from min to max age in years
  agedays.to.cover <- dt[, (floor(min(ageyears) * 365.25):floor(max(ageyears +
                                                                     1) * 365.25))]
  # group all measurements above age 19 together (copied from CD stata code e-mailed 4/3/15)
  dt[ageyears > 19, ageyears := 19]
  dt.median <- dt[!is.na(sd.orig), list(sex = c(0, 1), sd.median = rep(median(sd.orig), 2)), by =
                   .(param, ageyears)]
  dt.median <- dt.median[, {
    x_pts <- floor((ageyears + 0.5) * 365.25)
    y_pts <- sd.median
    list(
      agedays = agedays.to.cover,
      sd.median = if (length(x_pts) < 2) {
        # fewer than 2 age-years of data (e.g. adult_cutpoint near the
        # minimum age in the dataset): fall back to constant recentering
        rep(if (length(y_pts) > 0) y_pts[1] else NA_real_,
            length(agedays.to.cover))
      } else {
        approx(x_pts, y_pts, xout = agedays.to.cover, rule = 2)$y
      }
    )
  }, by = .(param, sex)]
  setkey(dt.median, param, sex, agedays)
  return(dt.median)
}
