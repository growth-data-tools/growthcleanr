# Main pediatric growthcleanr function -- infants cleanbatch
# internal supporting functions for pediatrics can be found in: infants_support.R
# note

#' Function to clean data (optionally in batches):
#' 4.  Dataset split (optional)
#' a.	In Stata, many of the rest of the steps require you to repeat steps as many times as there are observations for the subject with the highest number of
#'     observations. Therefore, in order to speed things up I separated the dataset at this point into three groups: short (subjects with <=10 total
#'     observations), medium (subjects with >10 and <=30 observations) and long (subjects with >30 observations). I performed all the rest of the steps
#'     separately for each of the 3 datasets and then re-combined them at the end. Note that all observations for a subject stay together. This step is
#'     definitely not necessary if it will not speed things up in whatever software program you're using.
#'
#' NOTE: to use mutliple processes in R we will process patients in batches using the plyr ddply function
#' set up function to process one patient.  Function to parallelize batches is below.
#'
#' @keywords internal
#' @import data.table
#' @importFrom stats median
#' @noRd
cleanbatch_infants <- function(data.df,
                               log.path,
                               quietly,
                               parallel,
                               measurement.to.z,
                               ewma.fields,
                               ewma.exp,
                               recover.unit.error,
                               include.carryforward,
                               sd.extreme,
                               z.extreme,
                               exclude.levels,
                               tanner.ht.vel,
                               who.ht.vel,
                               lt3.exclude.mode,
                               error.load.threshold,
                               error.load.mincount) {
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
  pair.prev <- param <- param.other <- prev.v <- sd.median <- sex <- subjid <- NULL
  swap.flag.1 <- swap.flag.2 <- tanner.months <- tbc.other.sd <- tbc.sd <- NULL
  tbc.sd.d <- tbc.sd.max <- tbc.sd.min <- tbc.sd.minus <- tbc.sd.next <- tbc.sd.plus <- NULL
  tbc.sd.prev <- tbc.sd.sw <- tbc.sd.t <- temp.diff <- temp.exclude <- v <- NULL
  v.d <- v.minus <- v.next <- v.orig <- v.plus <- v.prev <- v.sw <- v.t <- NULL
  valid.interior.measurement <- who.maxdiff.next.ht <- who.mindiff.next.ht <- NULL
  whoagegrp.ht <- whoinc.1.ht <- whoinc.2.ht <- whoinc.3.ht <- whoinc.4.ht <- NULL
  whoinc.6.ht <- whoinc.age.ht <- z.orig <- NULL

  oob <- sd_med <- med_diff <- max_diff <- sum_oob <- i.exclude <- NULL

  data.df <- data.table(data.df, key = c('subjid', 'param', 'agedays', 'index'))

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
    cat(sprintf(
      "[%s] Processing Batch #%s...\n",
      Sys.time(),
      data.df$batch[1]
    ))

  # NOTE: in each step, we redo temp SDEs

  # 5: temporary SDEs ----

  # save a copy of all original measurement values before any transformation
  data.df[, v.orig := v]

  if (!quietly)
    cat(sprintf(
      "[%s] Preliminarily identify potential extraneous...\n",
      Sys.time()
    ))
  data.df$exclude[temporary_extraneous_infants(data.df)] <- 'Exclude-Temporary-Extraneous-Same-Day'

  # capture a list of subjects with possible extraneous for efficiency later
  subj.dup <- data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', unique(subjid)]

  # 6:  carried forwards ----

  if (!include.carryforward) {
    if (!quietly)
      cat(sprintf(
        "[%s] Exclude measurements carried forward...\n",
        Sys.time()
      ))

    # save original column names
    orig_colnames <- colnames(data.df)

    # we only running carried forwards on valid values, non NNTE values,
    # and non single values
    tmp <- table(paste0(data.df$subjid, "_", data.df$param))
    single <- paste0(data.df$subjid, "_", data.df$param) %in% names(tmp)[tmp > 1]
    valid_set <- valid(data.df, include.temporary.extraneous = TRUE) &
      !data.df$nnte_full &
      single
    data.sub <- data.df[valid_set,]

    # for ease, order by subject, parameter, and agedays
    data.sub <- data.sub[order(subjid, param, agedays)]

    # if no sdes, exclude carried forward values
    data.sub[, sum_sde := .N, by = c("subjid", "param", "agedays")]
    data.sub[, no_sde := !paste0(data.sub$subjid, "_", data.sub$param) %in%
               paste0(data.sub$subjid, "_", data.sub$param)[data.sub$sum_sde > 1]]
    data.sub[no_sde == TRUE, cf :=  (v.orig - shift(v.orig)) == 0, by = c("subjid", "param")]
    data.sub[is.na(cf), cf := FALSE]

    # for values with sdes, compare to all values from the prior day
    data.sub[no_sde == FALSE, cf := (function(df){
      ages <- unique(agedays)
      for (i in 2:length(ages)) {
        # find the set of measurements from the previous age in days
        all.prev.v <- df[agedays == ages[i - 1], v.orig]
        # if a measurement for the current age is found in the set of measurements from the previous age, then mark it as carried forward
        df[agedays == ages[i] &
             v.orig %in% all.prev.v, cf := TRUE]
      }
    })(copy(.SD)), .SDcols = c('agedays', 'cf', 'v.orig'), by = .(subjid, param)]

    # merge in the carried forwards
    cf_idx <- data.sub$index[data.sub$cf]
    data.df[index %in% cf_idx, exclude := "Exclude-Carried-Forward"]

    # redo temp sde
    data.df$exclude[temporary_extraneous_infants(data.df)] <- 'Exclude-Temporary-Extraneous-Same-Day'

    # find out if values are whole or half imperial
    data.df[param == "WEIGHTKG",
            wholehalfimp := (v.orig * 2.20462262)%%1 < 0.01]
    data.df[param == "HEIGHTCM",
            wholehalfimp := (v.orig * 1.27)%%1 < 0.01]
    data.df[param == "HEIGHTCM",
            wholehalfimp := (v.orig * 1.27 / 2)%%1 < 0.01]
    data.df[param == "HEADCM",
            wholehalfimp := (v.orig * 1.27)%%1 < 0.01]
    data.df[param == "HEADCM",
            wholehalfimp := (v.orig * 1.27 / 2)%%1 < 0.01]
    # find string carried forward length
    data.df[, seq_win := sequence(rle(as.character(exclude))$lengths),
            by = c("subjid", "param")]
    data.df[exclude != "Exclude-Carried-Forward", seq_win := NA]
    # relabel all the initial values as "0" -- reorder
    data.df <- data.df[order(subjid, param, v.orig, agedays)]
    data.df[which(seq_win == 1)-1, seq_win :=  0]
    # reorder correctly
    data.df <- data.df[order(subjid, param, agedays)]
    # also create labels
    data.df[, cs := rep(1:length(rle(as.character(exclude))$lengths),
                        times = rle(as.character(exclude))$lengths),
            by = c("subjid", "param")]
    # fix the first one -- have to do this outside of data table
    data.df[which(seq_win == 0), "cs"] <-
      data.df[which(seq_win == 0)+1, "cs"]
    data.df[is.na(seq_win), cs := NA]

    # find th diff between initial and cf
    data.df[!is.na(seq_win), absdiff := abs(sd.orig - sd.orig[1]),
            by = c("subjid", "param", "cs")]

    # handle CFs by case
    data.df[!is.na(seq_win), exclude := (function(df){
      # only 1 cf in string
      if (max(seq_win) == 1){
        df[seq_win != 0 & absdiff < 0.05,
           exclude := "Exclude-1-CF-deltaZ-<0.05"]

        # use short circuiting to have the lower end covered (>= 0.05)
        df[seq_win != 0 & absdiff < .1 & wholehalfimp,
           exclude := "Exclude-1-CF-deltaZ-<0.1 wholehalfimp"]
      } else if (max(seq_win) > 1){
        df[seq_win != 0 & agedays/365.25 > 16 & sex == 1 & absdiff < 0.05,
           exclude := "Exclude-Teen-2-plus-CF-deltaZ-<0.05"]
        df[seq_win != 0 & agedays/365.25 > 17 & sex == 0 & absdiff < 0.05,
           exclude := "Exclude-Teen-2-plus-CF-deltaZ-<0.05"]

        df[seq_win != 0 & agedays/365.25 > 16 & sex == 1 & absdiff < .1 &
             wholehalfimp,
           exclude := "Exclude-Teen-2-plus-CF-deltaZ-<0.1-wholehalfimp"]
        df[seq_win != 0 & agedays/365.25 > 17 & sex == 0 & absdiff < .1 &
             wholehalfimp,
           exclude := "Exclude-Teen-2-plus-CF-deltaZ-<0.1-wholehalfimp"]
      }
    })(copy(.SD)),
    .SDcols = c('agedays', "seq_win", 'absdiff', "sex", "cs", "wholehalfimp",
                "exclude"),
    by = c("subjid", "param", "cs")]

  # BIV ----

  # Wt limits from do-file Oct 10 2022, in dataset, highest plausible weight=29.5kg, 65 lbs
  # HC Limits based on analysis in do-file from Oct 11 2022: in dataset, lowest plausible 18.5, z=-13, highest plausible 63.5, z=11.2  (at birth, highest=42, z=6)


  if (!quietly)
    cat(sprintf(
      "[%s] Exclude BIVs...\n",
      Sys.time()
    ))

  exc_nam <- "Exclude-Absolute-BIV"

  # identify absolute cutoffs
  # Min/max weight at birth based on published births
  data.df[valid(data.df) & param == "WEIGHTKG" & v < 0.2,
          exclude := exc_nam]
  # Min weight after birth based on rules about who can go home with alowance for significant weight loss -- this is for outpatient data only so very low weight babies would still be in NICU
  data.df[valid(data.df) & param == "WEIGHTKG" & v > 10.5 &
            agedays == 0,
          exclude := exc_nam]
  # Max weight for <2y based on eval (see do-file from Oct 10 2022), highest plaus weight 29.5kg
  data.df[valid(data.df) & param == "WEIGHTKG" & v < 1 &
            agedays == 0,
          exclude := exc_nam]
  # Max weight for <2y based on eval (see do-file from Oct 10 2022), highest plaus weight 29.5kg
  data.df[valid(data.df) & param == "WEIGHTKG" & v > 35 &
            agedays < 2,
          exclude := exc_nam]
  # Max weight for all based on published data
  data.df[valid(data.df) & param == "WEIGHTKG" & v > 600,
          exclude := exc_nam]

  # Min/max HC based on analysis in do file from
  # Also, 18 is z=-6 for 22 0/7 in Fenton and 65 is z=6 for 40 0/7
  data.df[valid(data.df) & param == "HEIGHTCM" & v < 18,
          exclude := exc_nam]
  data.df[valid(data.df) & param == "HEIGHTCM" & v > 65 &
            agedays == 0,
          exclude := exc_nam]

  # Min/max HC based on analysis in do file from Oct 11 2022
  # Also, 13 is z=-6 for 22 0/7 in Fenton and
  data.df[valid(data.df) & param == "HEADCM" & v < 13,
          exclude := exc_nam]
  data.df[valid(data.df) & param == "HEADCM" & v > 75,
          exclude := exc_nam]
  data.df[valid(data.df) & param == "HEADCM" & v > 50 &
            agedays == 0,
          exclude := exc_nam]

  exc_nam <- "Exclude-Standardized-BIV"

  # identify z cutoff
  # ***Note, using unrecentered values***
  #  *For weight only do after birth
  data.df[valid(data.df) & param == "WEIGHTKG" & z.orig < - 25 &
            agedays < 1,
          exclude := exc_nam]
  data.df[valid(data.df) & param == "WEIGHTKG" & z.orig < -15 &
            agedays >= 1,
          exclude := exc_nam]
  data.df[valid(data.df) & param == "WEIGHTKG" & z.orig > 15,
          exclude := exc_nam]

  # *Max z-score for height based on analysis of CHOP data because 15/25 too loose for upper limits
  data.df[valid(data.df) & param == "HEIGHTCM" & z.orig < - 25 &
            agedays < 1,
          exclude := exc_nam]
  data.df[valid(data.df) & param == "HEIGHTCM" & z.orig < -15 &
            agedays >= 1,
          exclude := exc_nam]
  data.df[valid(data.df) & param == "HEIGHTCM" & z.orig > 8,
          exclude := exc_nam]

  # head circumference
  data.df[valid(data.df) & param == "HEADCM" & z.orig < -15,
          exclude := exc_nam]
  data.df[valid(data.df) & param == "HEADCM" & z.orig > 15,
          exclude := exc_nam]


  # 9d.  Replace exc_*=0 if exc_*==2 & redo step 5 (temporary extraneous)
  data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
  data.df[temporary_extraneous_infants(data.df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']

  # evil twins ----
  # Evil Twins: An important weakness in the original pediatric growthcleanr algorithm was that it often failed to identify two or more implausible measurements that occurred next to each other, even if they were extremely deviant from a child’s other measurements. This step is now added to identify these multiple extreme values, although it also identifies some single values.

  exc_nam <- "Exclude-Evil-Twin"

  # first, find out of any re possible evil twins in the first place
  start_df <- calc_oob_evil_twins(data.df[valid(data.df),])

  if (any(start_df$sum_oob > 2)) {
    if (!quietly)
      cat(sprintf(
        "[%s] Exclude evil twins...\n",
        Sys.time()
      ))

    df <- copy(data.df)

    # start to evaluate and remove evil twins
    data.df[, exclude := (function(df) {
      # where we are updating results
      upd.df <- copy(df)
      upd.df <- calc_oob_evil_twins(df[valid(df),])
      # count the amount of oobs for each subject/param and distribute it out
      upd.df[, `:=` (sum_oob = sum(oob, na.rm = T)), by =.(subjid, param)]

      any_oob <- TRUE
      # while there are multiple oob, we want to remove
      while (any_oob){

        # now calculate the maximum difference from the median tbc.sd
        upd.df[, `:=` (sd_med = median(tbc.sd, na.rm = T)), by =.(subjid, param)]
        upd.df[, `:=` (med_diff = abs(tbc.sd - sd_med)), by =.(subjid, param)]
        upd.df[, `:=` (max_diff = med_diff  == max(med_diff)), by =.(subjid, param)]
        # for ones with no tbc.sd, mark as false
        upd.df[is.na(max_diff), max_diff := FALSE]

        upd.df[sum_oob > 0 & max_diff, exclude := exc_nam]

        df[upd.df[exclude == exc_nam,], exclude := i.exclude, on = .(line)]

        upd.df <- calc_oob_evil_twins(df[valid(df),])
        upd.df[, `:=` (sum_oob = sum(oob, na.rm = T)), by =.(subjid, param)]

        any_oob <- any(upd.df$sum_oob > 2)

      }

      return(df$exclude)
    })(copy(.SD))]

  }


  # 9d.  Replace exc_*=0 if exc_*==2 & redo step 5 (temporary extraneous)
  data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
  data.df[temporary_extraneous_infants(data.df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']

  # extreme ewma ----


  # 11.  Exclude extreme errors with EWMA
  # a.	Erroneous measurements can distort the EWMA for measurements around them. Therefore, if the EWMA method identifies more than one value for a subject and
  #     parameter that meets criteria for exclusion, we will only exclude the value that deviates the most from expected in any given step. Then we will repeat the
  #     entire process until no measurements are identified that meet criteria for exclusion.
  # b.	Perform a EWMA calculation
  #   i.	Only use values where exc_*==0 to determine the EWMAs. However, calculate dewma_* variables for values where exc_*==0 or exc_*==2
  if (!quietly)
    cat(sprintf(
      "[%s] Exclude extreme measurements based on EWMA...\n",
      Sys.time()
    ))
  data.df <- data.df[, exclude := (function(df) {
    num.ewma.excluded <- 0
    # optimization: determine whether this subject has any extraneous
    has.extraneous <- subjid %in% subj.dup
    while (TRUE) {
      df[, (ewma.fields) := as.double(NaN)]
      df[valid(exclude), (ewma.fields) := ewma(agedays, tbc.sd, ewma.exp, TRUE)]

      # note: at this point, only one ewma exists per param on a given day for a subject, so sort(ewma.all)[1] will returns the non-missing ewma.all
      # restrict to children with possible extraneous for efficiency
      if (has.extraneous) {
        df[, `:=`(
          ewma.all = sort(ewma.all)[1],
          ewma.before = sort(ewma.before)[1],
          ewma.after = sort(ewma.after)[1]
        ), by = .(agedays)]
      }
      df[, `:=`(
        dewma.all = tbc.sd - ewma.all,
        dewma.before = tbc.sd - ewma.before,
        dewma.after = tbc.sd - ewma.after
      )]

      # 11c.  Identify all values that meet all of the following criteria as potential exclusions:
      #   i.	There are 3 or more measurements for that subject and parameter
      #   ii.	(dewma_*>3.5 & dewma_*_bef>3 & dewma_*_aft>3 & tbc*sd>3.5) OR (dewma_*<-3.5 & dewma_*_bef<-3 & d & dewma_*_aft<-3 & tbc*sd<-3.5)
      #   iii.	exc_*==0

      # a.	For VLEZ: DEWMA > 8, DEWMA_bef > 6, DEWMA_aft > 6, and tbc`p’z > 3.5
      # b.	For non-VLEZ: DEWMA > 3.5, DEWMA_bef > 3, DEWMA_aft > 3, and tbc`p’z > 3.5
      # c.	For VLEZ and non-VLEZ: DEWMA < -3.5, DEWMA_bef < -3, DEWMA_aft <-3, tbc`p’z < -3.5

      # (z-score < -3 in the first 6 months of life )
      vlez <- df$agedays < 6*30.4375 & df$tbc.sd < -3

      num.valid <- sum(valid(df))
      rep <- na_as_false(with(
        df,
        num.valid >= 3 & valid(df)
        &
          (# for VLEZ only
          (vlez &
             dewma.all > 8 & dewma.before > 6 & dewma.after > 6 & tbc.sd > 3.5
          ) |
          # for non-VLEZ
          (!vlez &
             dewma.all > 3.5 & dewma.before > 3 & dewma.after > 3 & tbc.sd > 3.5
          ) |
          # for both
          (
            dewma.all < -3.5 &
              dewma.before < -3 & dewma.after < -3 & tbc.sd < -3.5
          ))
      ))
      num.exclude <- sum(rep)
      # 11d.  If there is only one potential exclusion identified in step 11c for a subject and parameter, replace exc_*=5 for that value
      if (num.exclude == 1)
        df[rep, exclude := 'Exclude-EWMA-Extreme']
      # 11e.  If there is more than one potential exclusion identified in step 11c for a subject and parameter, calculate abssum_*=|tbc*sd+dewma_*| for each exclusion and
      #     replace exc_*=5 for the value with the highest abssum_*
      if (num.exclude > 1) {
        # first order by decreasing abssum
        worst.row <- with(df, order(rep, abs(tbc.sd + (
          tbc.sd - ewma.all
        )), decreasing = TRUE))[1]
        df[worst.row, exclude := 'Exclude-EWMA1-Extreme']
      }
#
#       # 11f.  For subjects/parameters with only 2 values, calculate abstbc*sd=|tbc*sd|
#       # g.  Replace exc_*=6 for values that meet all of the following criteria
#       #   i.	There are 2 measurements for that subject and parameter
#       #   ii.	(dewma_*>3.5 & tbc*sd>3.5) OR (dewma_*<-3.5 & tbc*sd<-3.5)
#       #   iii.	If there are 2 measurements for a subject/parameter that meet criteria ii, only replace exc_*=6 for the value with the larger abstbc*sd
#       rep <- na_as_false(with(
#         df,
#         num.valid == 2 &
#           (
#             tbc.sd - ewma.all > 3.5 &
#               tbc.sd > 3.5 |
#               tbc.sd - ewma.all < -3.5 & tbc.sd < -3.5
#           )
#       ))
#       num.exclude <- sum(rep)
#       if (num.exclude == 1)
#         df[rep, exclude := 'Exclude-EWMA-Extreme-Pair']
#       if (num.exclude > 1) {
#         # first order by decreasing abssum
#         worst.row <- with(df, order(rep, abs(tbc.sd), decreasing = TRUE))[1]
#         df[worst.row, exclude := 'Exclude-EWMA-Extreme-Pair']
#       }

      # 11h.  Recalculate temporary extraneous as in step 5
      # optimize: only perform these steps if this subject is known to have extraneous measurements
      if (has.extraneous) {
        df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
        df[temporary_extraneous(df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']
      }

      # 11i.  If there was at least one subject who had a potential exclusion identified in step 11c, repeat steps 11b-11g. If there were no subjects with potential
      #     exclusions identified in step 11c, move on to step 12.
      newly.excluded <- sum(df$exclude %in% c('Exclude-EWMA1-Extreme', 'Exclude-EWMA-Extreme-Pair'))
      if (newly.excluded > num.ewma.excluded) {
        num.ewma.excluded <- newly.excluded
      } else {
        break
      }
    }
    return(df$exclude)
  })(copy(.SD)), by = .(subjid, param), .SDcols = c('index', 'sex', 'agedays', 'tbc.sd', 'exclude')]

  # 9d.  Replace exc_*=0 if exc_*==2 & redo step 5 (temporary extraneous)
  data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
  data.df[temporary_extraneous_infants(data.df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']

  # SDEs ----

  # STOP HERE: need to adapt to new work

  # # prepare a list of valid rows and initialize variables for convenience
  # valid.rows <- valid(data.df)
  # data.df[, `:=`(
  #   ewma.all = as.double(NaN),
  #   abssum2 = as.double(NaN),
  #   median.other.sd = as.double(NaN),
  #   extraneous = FALSE
  # )]
  #
  # # 12c.	Calculate a EWMA step for all subjects/parameters with duplicates/extraneous and at
  # #       least one non-extraneous value with the following modifications
  # #   i.	For calculating the EWMA, include only the extraneous selected in 12c
  # #  ii.	Calculate dewma_* for all values of extraneous
  # # iii.	You do not need to calculate EWMAbef or EWMAaft for this step
  #
  # # determine proportion of days with extraneous/duplication for each parameter ahead of time for efficiency
  # dup.ratio.df <- data.df[subjid %in% subj.dup &
  #                           (valid.rows |
  #                              temp.dups), list(dup = (.N > 1)), by = .(subjid, param, agedays)][j = list(dup.ratio =
  #                                                                                                           mean(dup)), keyby = .(subjid, param)]
  # # identify subject/parameters where there is duplication but at least one day with no extraneous for that parameter
  # subj.param.not.all.dups <- dup.ratio.df[dup.ratio < 1.0, list(subjid, param)]
  # # identify subject/parameters where there is duplication for all days for that parameter
  # subj.param.all.dups <- dup.ratio.df[dup.ratio == 1, list(subjid, param)]
  # subj.all.dups <- subj.param.all.dups[, unique(subjid)]
  # # perform ewma for subjects with extraneous
  # data.df[subjid %in% subj.dup &
  #           valid.rows, ewma.all := ewma(agedays, tbc.sd, ewma.exp, ewma.adjacent =
  #                                          FALSE), by = .(subjid, param)]
  # # note: at this point, only one ewma.all exists per param on a given day for a subject, so sort(ewma.all)[1] will returns the non-missing ewma.all
  # data.df[subjid %in% subj.dup, ewma.all := sort(ewma.all)[1], by = .(subjid, param, agedays)]
  #
  # # iv.  Calculate abssum2_*=|2*dewma_*|+|tbc*sd| (note that this is different from how we calculated abssum_* in step 11).
  # # NOTE: only children with more than one ageday with valid measurements will have a valid ewma from above
  # data.df[, abssum2 := 2 * abs(tbc.sd - ewma.all) + abs(tbc.sd)]
  #
  # # 12d.  For each subject/parameter/age with extraneous and at least one non-extraneous value:
  # #   i.  Replace exc_*=7 for all values except the value that has the smallest abssum2_*.
  # data.df[data.table(subj.param.not.all.dups), extraneous := seq_along(abssum2) != which.min(abssum2), by =
  #           .(subjid, param, agedays)]
  # data.df[temp.dups, exclude := 'Include']
  # data.df[(valid.rows |
  #            temp.dups) &
  #           extraneous, exclude := 'Exclude-Extraneous-Same-Day']
  # #  ii.  Determine dup_tot_* (# of days with extraneous for that subject/parameter) and nodup_tot_* (# of days with nonexlcuded
  # #       non-extraneous for that subject/parameter).
  # # iii.  If dup_tot_*/(dup_tot_*+nodup_tot_*) is greater than 1/2, replace exc_*=7 for all extraneous for that subject/parameter
  # #       for each age where the  largest measurement minus the smallest measurement for that subject/parameter/age is larger than
  # #       the maximum difference (ht 3cm; wt 0-9.999 kg 0.25kg; wt 10-29.9999 kg 0.5 kg; wt 30kg and higher 1 kg).
  #
  # data.df[data.table(dup.ratio.df[dup.ratio > 1 / 2, list(subjid, param)]), exclude := (function(df) {
  #   df[, `:=`(tbc.sd.min = as.double(NaN),
  #             tbc.sd.max = as.double(NaN))]
  #   df[valid(
  #     exclude,
  #     include.extraneous = TRUE,
  #     include.temporary.extraneous = TRUE
  #   ), `:=`(tbc.sd.min = min(tbc.sd),
  #           tbc.sd.max = max(tbc.sd))]
  #   df[tbc.sd.max - tbc.sd.min > ifelse(param == 'HEIGHTCM',
  #                                       3,
  #                                       ifelse(
  #                                         param == 'WEIGHTKG',
  #                                         ifelse(tbc.sd.min < 10, 0.25, ifelse(tbc.sd.min < 30, 0.5, 1)),
  #                                         NA
  #                                       )),
  #      exclude := 'Exclude-Extraneous-Same-Day']
  #   return(df$exclude)
  # })(copy(.SD)), .SDcols = c('exclude', 'tbc.sd'), by = .(subjid, param, agedays)]
  #
  # # 12e.	For each subject/parameter/age with extraneous and no nonextraneous values:
  # #   i.	Replace exc_*=7 for all values except the value with the smallest |tbc*sd-median_tbcOsd|. If median_tbcOsd is missing because there are no values
  # #       for the other parameter, randomly choose one extraneous value for each subject/parameter/age to keep as exc_*=0 and replace exc_*=7 for all other
  # #       extraneous for that subject/parameter/age.
  # #  ii.  If the largest measurement minus the smallest measurement for that subject/parameter/age is larger than the maximum difference
  # #       (ht 3cm; wt 0-9.999 kg 0.25kg; wt 10-29.9999 kg 0.5 kg; wt 30kg and higher 1 kg)., replace exc_*=7 for all extraneous for that
  # #       subject/parameter/age.
  #
  # # calculate median for other parameter (restrict to subjects with all duplication for at least one parameter)
  # data.df[subjid %in% subj.all.dups, exclude := (function(subj.df) {
  #   # flag days that have extraneous / potentially valid parameters
  #   subj.df[, `:=`(
  #     extraneous.this.day = FALSE,
  #     extraneous = FALSE,
  #     tbc.sd.min = as.double(NaN),
  #     tbc.sd.max = as.double(NaN)
  #   )]
  #   valid.rows = valid(
  #     subj.df,
  #     include.extraneous = TRUE,
  #     include.temporary.extraneous = TRUE
  #   )
  #   subj.df[valid.rows, extraneous.this.day := (.N > 1), by = .(param, agedays)]
  #   for (p in subj.df[j = unique(param)]) {
  #     median.sd <- subj.df[param != p &
  #                            !extraneous.this.day, median(tbc.sd)]
  #     subj.df[param == p, median.other.sd := median.sd]
  #   }
  #   # safety check -- assign median.other.sd==0 to ensure "which.min" functions correctly below
  #   subj.df[is.na(median.other.sd), median.other.sd := 0]
  #   # identify rows as extraneous where |tbc*sd-median_tbcOsd| is not at the minimum value
  #   subj.df[extraneous.this.day == TRUE, extraneous := (seq_along(median.other.sd) != which.min(abs(tbc.sd - median.other.sd))), by =
  #             .(param, agedays)]
  #   subj.df[extraneous.this.day &
  #             !extraneous, exclude := 'Include']
  #   subj.df[extraneous.this.day &
  #             extraneous, exclude := 'Exclude-Extraneous-Same-Day']
  #   subj.df[extraneous.this.day == TRUE, `:=`(tbc.sd.min = min(tbc.sd),
  #                                             tbc.sd.max = max(tbc.sd)), by = .(param, agedays)]
  #   subj.df[tbc.sd.max - tbc.sd.min > ifelse(param == 'HEIGHTCM',
  #                                            3,
  #                                            ifelse(
  #                                              param == 'WEIGHTKG',
  #                                              ifelse(tbc.sd.min < 10, 0.25, ifelse(tbc.sd.min < 30, 0.5, 1)),
  #                                              NA
  #                                            )),
  #           exclude := 'Exclude-Extraneous-Same-Day']
  #
  #   # identify kids who had an SD or EWMA extreme excluded that was a extraneous and re-label as "Exclude-Extraneous-Same-Day"
  #   subj.df[, extraneous.this.day := FALSE]
  #   # consider any non-missing measurement when determining presence of extraneous
  #   subj.df[exclude != 'Missing', extraneous.this.day := (.N > 1), by =
  #             .(param, agedays)]
  #   subj.df[extraneous.this.day &
  #             exclude %in% c('Exclude-SD-Cutoff',
  #                            'Exclude-EWMA-Extreme',
  #                            'Exclude-EWMA-Extreme-Pair'), exclude := 'Exclude-Extraneous-Same-Day']
  #
  #   return(subj.df$exclude)
  # })(copy(.SD)), .SDcols = c('param', 'agedays', 'exclude', 'tbc.sd'), by =
  #   .(subjid)]
  #
  # # 12f.  For any values that were excluded with exc_*=4, 5, or 6 that are also extraneous, replace exc_*=7.
  # data.df[subjid %in% subj.dup, exclude := (function(subj.df) {
  #   if (.N > 1) {
  #     subj.df[exclude %in% c('Exclude-SD-Cutoff',
  #                            'Exclude-EWMA-Extreme',
  #                            'Exclude-EWMA-Extreme-Pair'), exclude := 'Exclude-Extraneous-Same-Day']
  #   }
  #   return(subj.df$exclude)
  # })(copy(.SD)), .SDcols = c('exclude'), by = .(subjid, param, agedays)]


  # end ----

  if (!quietly)
    cat(sprintf("[%s] Completed Batch #%s...\n", Sys.time(), data.df$batch[1]))
  if (!quietly & parallel)
    sink()

  return(data.df[j = .(line, exclude, tbc.sd, tbc.other.sd, param)]) #debugging
}
