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
                               recover.unit.error,
                               include.carryforward,
                               sd.extreme,
                               z.extreme,
                               exclude.levels,
                               tanner.ht.vel,
                               who.ht.vel,
                               lt3.exclude.mode,
                               error.load.threshold,
                               error.load.mincount,
                               ref.data.path) {
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
    not_single <- paste0(data.df$subjid, "_", data.df$param) %in% names(tmp)[tmp > 1]
    valid_set <- valid(data.df, include.temporary.extraneous = TRUE) &
      !data.df$nnte_full &
      not_single
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
      if (length(ages) > 1){
        for (i in 2:length(ages)) {
          # find the set of measurements from the previous age in days
          all.prev.v <- df[agedays == ages[i - 1], v.orig]
          # if a measurement for the current age is found in the set of measurements from the previous age, then mark it as carried forward
          df[agedays == ages[i] &
               v.orig %in% all.prev.v, cf := TRUE]
        }
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
    data.df[!is.na(seq_win), absdiff :=
              abs(sd.orig_uncorr - sd.orig_uncorr[1]),
            by = c("subjid", "param", "cs")]

    # handle CFs by case
    data.df[!is.na(seq_win), exclude := (function(df){
      # only 1 cf in string
      if (max(seq_win) == 1){
        df[seq_win != 0 & absdiff < 0.05,
           exclude := "Exclude-1-CF-deltaZ-<0.05"]

        # use short circuiting to have the lower end covered (>= 0.05)
        df[seq_win != 0 & absdiff < .1 & wholehalfimp,
           exclude := "Exclude-1-CF-deltaZ-<0.1-wholehalfimp"]
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

      return(df$exclude)
    })(copy(.SD)),
    .SDcols = c('agedays', "seq_win", 'absdiff', "sex", "cs", "wholehalfimp",
                "exclude"),
    by = c("subjid", "param", "cs")]
  }

  # BIV ----

  # Wt limits from do-file Oct 10 2022, in dataset, highest plausible weight=29.5kg, 65 lbs
  # HC Limits based on analysis in do-file from Oct 11 2022: in dataset, lowest plausible 18.5, z=-13, highest plausible 63.5, z=11.2  (at birth, highest=42, z=6)


  if (!quietly)
    cat(sprintf(
      "[%s] Exclude BIVs...\n",
      Sys.time()
    ))

  # add age in years
  data.df[, ageyears := agedays/365.25]

  # calculate the valid step
  valid_set <- valid(data.df, include.temporary.extraneous = TRUE) &
    !data.df$nnte_full

  exc_nam <- "Exclude-Absolute-BIV"

  # identify absolute cutoffs
  # Min/max weight at birth based on published births
  data.df[valid_set & param == "WEIGHTKG" & v < 0.2 & agedays == 0,
          exclude := exc_nam]
  data.df[valid_set & param == "WEIGHTKG" & v < 1 & agedays != 0,
          exclude := exc_nam]
  # Min weight after birth based on rules about who can go home with alowance for significant weight loss -- this is for outpatient data only so very low weight babies would still be in NICU
  data.df[valid_set & param == "WEIGHTKG" & v > 10.5 &
            agedays == 0,
          exclude := exc_nam]
  # Max weight for <2y based on eval (see do-file from Oct 10 2022), highest plaus weight 29.5kg
  data.df[valid_set & param == "WEIGHTKG" & v < 1 &
            agedays == 0,
          exclude := exc_nam]
  # Max weight for <2y based on eval (see do-file from Oct 10 2022), highest plaus weight 29.5kg
  data.df[valid_set & param == "WEIGHTKG" & v > 35 &
            ageyears < 2,
          exclude := exc_nam]
  # Max weight for all based on published data
  data.df[valid_set & param == "WEIGHTKG" & v > 600,
          exclude := exc_nam]

  # Min/max HC based on analysis in do file from
  # Also, 18 is z=-6 for 22 0/7 in Fenton and 65 is z=6 for 40 0/7
  data.df[valid_set & param == "HEIGHTCM" & v < 18,
          exclude := exc_nam]
  data.df[valid_set & param == "HEIGHTCM" & v > 244,
          exclude := exc_nam]
  data.df[valid_set & param == "HEIGHTCM" & v > 65 &
            agedays == 0,
          exclude := exc_nam]

  # Min/max HC based on analysis in do file from Oct 11 2022
  # Also, 13 is z=-6 for 22 0/7 in Fenton and
  data.df[valid_set & param == "HEADCM" & v < 13,
          exclude := exc_nam]
  data.df[valid_set & param == "HEADCM" & v > 75,
          exclude := exc_nam]
  data.df[valid_set & param == "HEADCM" & v > 50 &
            agedays == 0,
          exclude := exc_nam]

  exc_nam <- "Exclude-Standardized-BIV"

  # identify z cutoff
  # ***Note, using unrecentered values***
  #  *For weight only do after birth
  data.df[valid_set & param == "WEIGHTKG" & sd.orig_uncorr < -25 &
            ageyears < 1,
          exclude := exc_nam]
  data.df[valid_set & param == "WEIGHTKG" & sd.orig_uncorr < -15 &
            ageyears >= 1,
          exclude := exc_nam]
  data.df[valid_set & param == "WEIGHTKG" & sd.orig_uncorr > 22,
          exclude := exc_nam]

  # *Max z-score for height based on analysis of CHOP data because 15/25 too loose for upper limits
  data.df[valid_set & param == "HEIGHTCM" & sd.orig_uncorr < -25 &
            ageyears < 1,
          exclude := exc_nam]
  data.df[valid_set & param == "HEIGHTCM" & sd.orig_uncorr < -15 &
            ageyears >= 1,
          exclude := exc_nam]
  data.df[valid_set & param == "HEIGHTCM" & sd.orig_uncorr > 8,
          exclude := exc_nam]

  # head circumference
  data.df[valid_set & param == "HEADCM" & sd.orig_uncorr < -15,
          exclude := exc_nam]
  data.df[valid_set & param == "HEADCM" & sd.orig_uncorr > 15,
          exclude := exc_nam]


  # 9d.  Replace exc_*=0 if exc_*==2 & redo step 5 (temporary extraneous)
  data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
  data.df[temporary_extraneous_infants(data.df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']

  # evil twins ----
  # Evil Twins: An important weakness in the original pediatric growthcleanr algorithm was that it often failed to identify two or more implausible measurements that occurred next to each other, even if they were extremely deviant from a child’s other measurements. This step is now added to identify these multiple extreme values, although it also identifies some single values.

  exc_nam <- "Exclude-Evil-Twins"

  # create the valid set
  # we only running carried forwards on valid values, non NNTE values,
  # and non single values, and non pair
  tmp <- table(paste0(data.df$subjid, "_", data.df$param))
  not_single_pairs <- paste0(data.df$subjid, "_", data.df$param) %in% names(tmp)[tmp > 2]
  valid_set <- valid(data.df, include.temporary.extraneous = FALSE) &
    !data.df$nnte_full & # does not use the "other"
    not_single_pairs

  # make sure it's ordered by subjid and parameter
  data.df <- data.df[order(subjid, param),]

  # 9A/B/C
  # first, find out of any re possible evil twins in the first place
  start_df <- calc_oob_evil_twins(data.df[valid_set,])

  if (any(start_df$oob)) {
    if (!quietly)
      cat(sprintf(
        "[%s] Exclude evil twins...\n",
        Sys.time()
      ))

    # start to evaluate and remove evil twins
    data.df[valid_set, exclude := (function(df) {
      # where we are updating results
      upd.df <- copy(df)
      upd.df <- calc_oob_evil_twins(upd.df)
      # count the amount of oobs for each subject/param and distribute it out
      upd.df[, `:=` (sum_oob = sum(oob, na.rm = T)), by =.(subjid, param)]

      any_oob <- any(upd.df$sum_oob > 2)
      # while there are multiple oob, we want to remove
      while (any_oob){

        # 9D
        # now calculate the maximum difference from the median tbc.sd
        upd.df[, `:=` (sd_med = median(tbc.sd, na.rm = T)), by =.(subjid, param)]
        upd.df[, `:=` (med_diff = abs(tbc.sd - sd_med)), by =.(subjid, param)]
        upd.df[, `:=` (max_diff = med_diff  == max(med_diff)), by =.(subjid, param)]
        # for ones with no tbc.sd, mark as false
        upd.df[is.na(max_diff), max_diff := FALSE]

        upd.df[sum_oob > 0 & max_diff, exclude := exc_nam]

        df[upd.df[exclude == exc_nam,], exclude := i.exclude, on = .(line)]

        #9E
        # reupdate valid (to recalculate OOB -- others are not included)
        upd.df <- calc_oob_evil_twins(df[valid(df),])
        upd.df[, `:=` (sum_oob = sum(oob, na.rm = T)), by =.(subjid, param)]

        any_oob <- any(upd.df$sum_oob > 2)

      }

      return(df$exclude)
    })(copy(.SD))]

  }

  # 9F.  redo temp extraneous
  data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
  data.df[temporary_extraneous_infants(data.df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']

  # Extreme EWMA ----

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

    # only enter if there's more than one valid value
    tmp <- table(paste0(df$subjid, "_", df$param))
    not_single_pairs <- paste0(df$subjid, "_", df$param) %in% names(tmp)[tmp > 2]
    valid_set <- valid(df, include.temporary.extraneous = TRUE) &
      !df$nnte_full & # does not use the "other"
      not_single_pairs

    if (sum(valid_set) > 1){
      while (TRUE) {
        tmp <- table(paste0(df$subjid, "_", df$param))
        not_single_pairs <- paste0(df$subjid, "_", df$param) %in% names(tmp)[tmp > 2]
        valid_set <- valid(df, include.temporary.extraneous = TRUE) &
          !df$nnte_full & # does not use the "other"
          not_single_pairs


        df[, (ewma.fields) := as.double(NaN)]

        # first, calculate which exponent we want to put through (pass a different
        # on for each exp)
        # subset df to only valid rows
        df_sub <- df[valid_set,]
        tmp <- data.frame(
          "before" = abs(df_sub$agedays - c(NA, df_sub$agedays[1:(nrow(df_sub)-1)])),
          "after" = abs(df_sub$agedays - c(df_sub$agedays[2:(nrow(df_sub))], NA))
        )
        maxdiff <- sapply(1:nrow(tmp), function(x){max(tmp[x,], na.rm = T)})
        exp_vals <- rep(-1.5, nrow(tmp))
        exp_vals[maxdiff > 365.25] <- -2.5
        exp_vals[maxdiff > 730.5] <- -3.5
        df[valid_set, exp_vals := exp_vals]

        # calculate ewma -- need to do it in a special way to not include
        # temp extaneous

        # first calculate ewma for rows without temp extraneous
        no_tmp_extr <- valid(df, include.temporary.extraneous = FALSE)

        df[valid_set & no_tmp_extr, (ewma.fields) := ewma(agedays, tbc.sd, exp_vals, TRUE)]
        df[valid_set & no_tmp_extr, paste0("c.",ewma.fields) := ewma(agedays, ctbc.sd, exp_vals, TRUE)]

        # now go through the temp extraneous and do the ewmas just for those
        # rows
        if (sum(df$exclude == "Exclude-Temporary-Extraneous-Same-Day") > 0){
          # go through each temp extraneous
          for (j in
               which(df$exclude == "Exclude-Temporary-Extraneous-Same-Day")){
            # subset to only the temp extraneous and valid values
            df_sub <- df[valid_set & (no_tmp_extr | (1:nrow(df) == j)),]

            # run ewma on those values
            df_sub[, (ewma.fields) := ewma(agedays, tbc.sd, exp_vals, TRUE)]
            df_sub[, paste0("c.",ewma.fields) := ewma(agedays, ctbc.sd, exp_vals, TRUE)]

            # merge the value back in
            col_replace <- c(ewma.fields, paste0("c.",ewma.fields))
            df_val <-
              df_sub[df_sub$exclude == "Exclude-Temporary-Extraneous-Same-Day",
                     ..col_replace]
            df[j, (col_replace) := df_val]
          }

        }

        # include the corrected
        df[, `:=`(
          dewma.all = tbc.sd - ewma.all,
          dewma.before = tbc.sd - ewma.before,
          dewma.after = tbc.sd - ewma.after,

          c.dewma.all = ctbc.sd - c.ewma.all
        )]


        # calculate potential exclusions
        df[valid_set, pot_excl :=
             (dewma.all > 3.5 & dewma.before > 3 & dewma.after > 3 & tbc.sd > 3.5 &
                ((!is.na(ctbc.sd) & c.dewma.all > 3.5) | is.na(ctbc.sd))
             ) |
             # for both
             (
               dewma.all < -3.5 &
                 dewma.before < -3 & dewma.after < -3 & tbc.sd < -3.5 &
                 ((!is.na(ctbc.sd) & c.dewma.all > 3.5) | is.na(ctbc.sd))
             )
        ]
        df[valid_set,][c(1, nrow(df)), pot_excl := FALSE]
        df[!valid_set, pot_excl := FALSE] # to compensate for later


        # 11c.  Identify all values that meet all of the following criteria as potential exclusions:
        #   i.	There are 3 or more measurements for that subject and parameter
        #   ii.	(dewma_*>3.5 & dewma_*_bef>3 & dewma_*_aft>3 & tbc*sd>3.5) OR (dewma_*<-3.5 & dewma_*_bef<-3 & d & dewma_*_aft<-3 & tbc*sd<-3.5)
        #   iii.	exc_*==0

        num.exclude <- sum(df$pot_excl)
        # 11d.  If there is only one potential exclusion identified in step 11c for a subject and parameter, replace exc_*=5 for that value
        if (num.exclude == 1)
          df[pot_excl, exclude := 'Exclude-EWMA1-Extreme']
        # 11e.  If there is more than one potential exclusion identified in step 11c for a subject and parameter, calculate abssum_*=|tbc*sd+dewma_*| for each exclusion and
        #     replace exc_*=5 for the value with the highest abssum_*
        if (num.exclude > 1) {
          # first order by decreasing abssum
          worst.row <- with(df, order(pot_excl, abs(tbc.sd + dewma.all),
                                      decreasing = TRUE))[1]
          df[worst.row, exclude := 'Exclude-EWMA1-Extreme']
        }

        # 11h.  Recalculate temporary extraneous as in step 5
        # optimize: only perform these steps if this subject is known to have extraneous measurements
        if (has.extraneous) {
          df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
          df[temporary_extraneous_infants(df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']
        }

        # 11i.  If there was at least one subject who had a potential exclusion identified in step 11c, repeat steps 11b-11g. If there were no subjects with potential
        #     exclusions identified in step 11c, move on to step 12.
        newly.excluded <- sum(df$exclude %in% c('Exclude-EWMA1-Extreme'))
        if (newly.excluded > num.ewma.excluded) {
          num.ewma.excluded <- newly.excluded
        } else {
          break
        }
      }
    }
    return(df$exclude)
  })(copy(.SD)), by = .(subjid, param), .SDcols = c('index', 'sex', 'agedays', 'tbc.sd', 'ctbc.sd', 'nnte_full', 'exclude')]

  # 9d.  Replace exc_*=0 if exc_*==2 & redo step 5 (temporary extraneous)
  data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
  data.df[temporary_extraneous_infants(data.df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']

  # 13: SDEs ----

  if (!quietly)
    cat(sprintf(
      "[%s] Exclude same day extraneous...\n",
      Sys.time()
    ))

  # prepare a list of valid rows and initialize variables for convenience
  valid.rows <- valid(data.df, include.temporary.extraneous = TRUE) &
    !data.df$nnte_full  # does not use the "other"

  data.df <- data.df[valid.rows, exclude := (function(subj_df) {
    # keep the original indices and exclude to overwrite
    orig_idx <- copy(subj_df$index)
    exclude_all <- copy(subj_df$exclude)
    if (nrow(subj_df) > 0 & any(subj_df$exclude == "Exclude-Temporary-Extraneous-Same-Day")){
      # for convenience
      subj_df[, extraneous := exclude == "Exclude-Temporary-Extraneous-Same-Day"]

      step <- "Exclude-SDE-Identical"

      # identify duplicate days
      dup_days <- unique(subj_df$agedays[subj_df$extraneous])

      # first, exclude identical measurements on the same day
      ide_ids <- c() # where we keep the identical ones
      for (dd in dup_days){
        # count amount of unique values
        s_df <- copy(subj_df[subj_df$agedays == dd,])
        ide_tab <- table(s_df$v)
        if (any(ide_tab > 1)){
          # for each identical, keep only the first one, by id
          # (ordered initially)
          ide_ids <- c(ide_ids, s_df$index[
            as.character(s_df$v) %in% names(ide_tab[ide_tab > 1])
          ][duplicated(
            s_df$v[
              as.character(s_df$v) %in% names(ide_tab[ide_tab > 1])
            ]
          )])
        }
      }
      criteria <- subj_df$index %in% ide_ids

      # we're going to update excludes before moving on to the rest of this
      # subject
      exclude_all[criteria] <- step

      subj_df <- subj_df[!criteria,]

      if (any(criteria)){
        # reevaluate temp same day
        subj_df[, extraneous := temporary_extraneous_infants(subj_df)]

        # identify duplicate days
        dup_days <- unique(subj_df$agedays[subj_df$extraneous])
      }

      # 13B: identify similar groups
      similar_ids <- c()
      for (dd in dup_days){
        # first, calculate if all SDEs on a single day are similar
        s_df <- copy(subj_df[subj_df$agedays == dd,])

        similar <- if (subj_df$param[1] == "WEIGHTKG"){
          max(s_df$v)/min(s_df$v) <= 1.03 & max(s_df$v) - min(s_df$v) <= 2.5
        } else if (subj_df$param[1] == "HEIGHTCM"){
          (max(s_df$v) - min(s_df$v) <  2.541 & min(s_df$v) < 127) |
            (max(s_df$v) - min(s_df$v) <  5.081 & min(s_df$v) >= 127)
        } else { # head circumference
          max(s_df$v) - min(s_df$v) <  1.271
        }

        if (similar){
          similar_ids <- c(similar_ids, s_df$index)
        }
      }

      # 13C: exclude non similar groups

      step <- "Exclude-SDE-All-Exclude"
      # now the rest!

      # next, calculate the duplicate ratio -- what proportion of days are
      # duplicated
      dup_ratio <-
        length(unique(subj_df$agedays[duplicated(subj_df$agedays)]))/
        length(unique(subj_df$agedays))
      # also check whether or not any same-days are adjacent -- need 4 at least
      # rolling windows of day differences -- we are looking for 0,x,0
      if (nrow(subj_df) > 3){
        roll <- embed(diff(subj_df$agedays), 3)
        adjacent <- sapply(1:nrow(roll), function(x){
          all(c(roll[x,1] == 0, roll[x,2] != 0, roll[x,3] == 0))
        })

        # get age days & rows that had adjacent
        idx_roll <- c(embed(1:nrow(subj_df),4)[adjacent,, drop = FALSE])
        idx_adj <- which(subj_df$agedays %in% subj_df$agedays[idx_roll])
      } else {
        adjacent <- F
        idx_adj <- idx_roll <- c()
      }

      # if dup ratio is too high, we exclude all
      # if adjacent, we exclude only those relevant sdes
      # same day extraneous -- for strict (default)
      criteria <-
          # looser criteria:
          # 1.	Exclude for ratio if ratio >1/3
          # 2.	Exclude for adjacent if 2 are adjacent AND ratio >1/4
          # 3.	Exclude for adjacent if 2 are adjacent AND those 2 are the first 2 or last 2 measurements for the subject/parameter
          #   note: if the first or last is sde and it's adjacent, by default
          #   that means it's the first or last two that are adjacent
          # 4.	Exclude for adjacent if 3 or more are adjacent
          if (dup_ratio > (1/3)){
            subj_df$extraneous
          } else if (
            (sum(adjacent) == 1 & dup_ratio > (1/4)) |
            (sum(adjacent) == 1 & c(1) %in% idx_adj) |
            (sum(adjacent) == 1 &
             c(nrow(subj_df)) %in% idx_adj) |
            (sum(adjacent) >= 2)
          ){
            # adjacent sum == 1 means 2 are adjacent
            # only exclude sdes that are adjacent
            subj_df$agedays %in% subj_df$agedays[idx_roll]
          } else {
            rep(F, nrow(subj_df))
          }
      # re-include similar groups
      criteria[subj_df$index %in% similar_ids] <- FALSE

      # we're going to update excludes before moving on to the rest of this
      # subject
      exclude_all[orig_idx %in% subj_df$index[criteria]] <- step

      subj_df <- subj_df[!criteria,]

      if (any(criteria)){
        # reevaluate temp same day
        subj_df[, extraneous := temporary_extraneous_infants(subj_df)]

        # identify duplicate days
        dup_days <- unique(subj_df$agedays[subj_df$extraneous])
      }

      step_extreme <- "Exclude-SDE-All-Extreme"
      step <- "Exclude-SDE-EWMA"
      # next, check for trivial differences for SDEs on the same day
      # this only works if there are non-sde days

      if (any(subj_df$extraneous)){
        # 13D: calculating ewmas

        # check for SDEs by EWMA -- alternate calculate excluding other SDEs
        all_sdes <- duplicated(subj_df$agedays) |
          duplicated(subj_df$agedays, fromLast = T)

        # first, calculate which exponent we want to put through (pass a different
        # on for each exp)
        tmp <- data.frame(
          "before" = abs(subj_df$agedays - c(NA, subj_df$agedays[1:(nrow(subj_df)-1)])),
          "after" = abs(subj_df$agedays - c(subj_df$agedays[2:(nrow(subj_df))], NA))
        )
        maxdiff <- sapply(1:nrow(tmp), function(x){max(tmp[x,], na.rm = T)})
        exp_vals <- rep(-1.5, nrow(tmp))
        exp_vals[maxdiff > 365.25] <- -2.5
        exp_vals[maxdiff > 730.5] <- -3.5
        subj_df[, exp_vals := exp_vals]

        # calculate the time difference for all values, as well as exponential
        delta <- as_matrix_delta(subj_df$agedays)
        delta <- ifelse(delta == 0, 0, (delta) ^ subj_df$exp_vals)

        rem_ids_extreme <- c()
        rem_ids <- c()
        for (dd in dup_days){
          # first, calculate if all SDEs on a single day are similar
          s_df <- copy(subj_df[subj_df$agedays == dd,])

          similar <- if (subj_df$param[1] == "WEIGHTKG"){
            max(s_df$v)/min(s_df$v) <= 1.03 & max(s_df$v) - min(s_df$v) <= 2.5
          } else if (subj_df$param[1] == "HEIGHTCM"){
            (max(s_df$v) - min(s_df$v) <  2.541 & min(s_df$v) < 127) |
              (max(s_df$v) - min(s_df$v) <  5.081 & min(s_df$v) >= 127)
          } else { # head circumference
            max(s_df$v) - min(s_df$v) <  1.271
          }

          if (!similar){
            # 13E

            # calculate dewma for each SDE on this day
            dewma <- sapply(1:nrow(s_df), function(x){
              ind <- subj_df$index == s_df[[x, "index"]]

              ewma_res <- sum(subj_df$tbc.sd[!all_sdes]*delta[ind,!all_sdes])/
                sum(delta[ind, !all_sdes])

              # delta ewma
              return(s_df$tbc.sd[x] - ewma_res)
            })
            absdewma <- abs(dewma)

            if (min(absdewma) > 1){
              rem_ids_extreme <- c(rem_ids_extreme, s_df$index)
            } else {
              de_day <- which.min(abs(dewma))

              # keep the value with the lowest EWMA -- do not keep rest
              rem_ids <- c(rem_ids, s_df$index[-de_day])
            }
          }
        }
        # re-include similar groups
        criteria_extreme <- subj_df$index %in% rem_ids_extreme
        criteria <- subj_df$index %in% rem_ids
        # we're going to update excludes before moving on to the rest of this
        # subject
        exclude_all[orig_idx %in% subj_df$index[criteria_extreme]] <- step_extreme
        exclude_all[orig_idx %in% subj_df$index[criteria]] <- step

        subj_df <- subj_df[!(criteria | criteria_extreme),]

        if (any(c(criteria, criteria_extreme))){
          # reevaluate temp same day
          subj_df[, extraneous := temporary_extraneous_infants(subj_df)]

          # identify duplicate days
          dup_days <- unique(subj_df$agedays[subj_df$extraneous])
        }
      }

      # all remaining are similar
      step_extreme <- "Exclude-SDE-All-Extreme"
      step <- "Exclude-SDE-One-Day"
      if (any(subj_df$extraneous)){
        # check for SDEs
        all_sdes <- duplicated(subj_df$agedays) |
          duplicated(subj_df$agedays, fromLast = T)

        rem_ids <- c()
        rem_ids_extreme <- c()
        for (dd in dup_days){
          # first, calculate if all SDEs on a single day are similar
          s_df <- copy(subj_df[subj_df$agedays == dd,])

          # find the DOP (designated other parameter)
          dop <-
            data.df[subjid == s_df$subjid[1] & param == get_dop(s_df$param[1]) &
                      agedays == s_df$agedays[1],]

          if (nrow(dop) > 0){
            # 13F
            med_diff <- abs(median(dop$tbc.sd)-s_df$tbc.sd)

            if (min(med_diff) > 2){
              rem_ids_extreme <- c(rem_ids_extreme, s_df$index)
            } else {
              de_day <- which.min(med_diff)

              # keep the value with the lowest median diff -- do not keep rest
              rem_ids <- c(rem_ids, s_df$index[-de_day])
            }
          } else if (nrow(s_df) > 3){
            #13G
            med_diff <- abs(median(s_df$tbc.sd)-s_df$tbc.sd)

            # if even and the middle two values are equidistant
            if (nrow(s_df)%%2 == 0 &
                diff(sort(med_diff)[1:2]) < 1e-8){
              # if the ageday is even, keep the lower z score; otherwise,
              # keep higher
              keep_val <-
                if (s_df$agedays[1]%%2 == 0){
                  which(s_df$v == min(s_df$v[order(med_diff)[1:2]]))
                } else {
                  which(s_df$v == max(s_df$v[order(med_diff)[1:2]]))
                }
              rem_ids <- c(rem_ids, s_df$index[-keep_val])
            }

            de_day <- which.min(med_diff)
            # keep the value with the lowest median diff -- do not keep rest
            rem_ids <- c(rem_ids, s_df$index[-de_day])
          } else {
            # 13H

            # 2 values, keep based on the age days
            keep_val <-
            if (s_df$agedays[1]%%2 == 0){
              which.min(s_df$v)
            } else {
              which.max(s_df$v)
            }
            rem_ids <- c(rem_ids, s_df$index[-keep_val])
          }
        }

        # re-include similar groups
        criteria_extreme <- subj_df$index %in% rem_ids_extreme
        criteria <- subj_df$index %in% rem_ids
        # we're going to update excludes before moving on to the rest of this
        # subject
        exclude_all[orig_idx %in% subj_df$index[criteria_extreme]] <- step_extreme
        exclude_all[orig_idx %in% subj_df$index[criteria]] <- step
      }
    }

    # replace all the "exclude temp extraneous" with includes -- other SDEs
    # were removed
    exclude_all[exclude_all == "Exclude-Temporary-Extraneous-Same-Day"] <-
      "Include"

    return(exclude_all)
  })(copy(.SD)), by = .(subjid, param), .SDcols = colnames(data.df)]

  # 15: moderate EWMA ----

  if (!quietly)
    cat(sprintf(
      "[%s] Exclude moderate EWMA...\n",
      Sys.time()
    ))

  # create the valid set
  # we only running carried forwards on valid values, non NNTE values,
  # and non single values, and non pair
  tmp <- table(paste0(data.df$subjid, "_", data.df$param))
  not_single_pairs <- paste0(data.df$subjid, "_", data.df$param) %in% names(tmp)[tmp > 2]
  valid_set <- valid(data.df, include.temporary.extraneous = FALSE) &
    !data.df$nnte_full & # does not use the "other"
    not_single_pairs

  # work in an a function order to encapsulate and not keep all the additional
  # columns

  # calculate plus/minus values

  # order just for ease later
  data.df <- data.df[order(subjid, param, agedays),]
  data.df <- data.df[valid_set, exclude := (function(df) {
    # 15A: calc plus/minus values
    df[param == "WEIGHTKG", p_plus := 1.05*v]
    df[param == "WEIGHTKG", p_minus := .95*v]

    df[param == "HEIGHTCM", p_plus := v+1]
    df[param == "HEIGHTCM", p_minus := v-1]

    df[param == "HEADCM", p_plus := v+1]
    df[param == "HEADCM", p_minus := v-1]

    #15B: smooth and recenter
    df <- calc_and_recenter_z_scores(df, "p_plus", ref.data.path)
    df <- calc_and_recenter_z_scores(df, "p_minus", ref.data.path)

    #15c: exclude agedays = 0 for ht, hc
    #15d: calculate ewma -- run within a subject/parameter
    df[(param != "HEIGHTCM" | param != "HEADCM") & agedays != 0,
             exclude := (function(df_sub) {
      # save initial exclusions to keep track
      ind_all <- copy(df_sub$index)
      exclude_all <- copy(df_sub$exclude)

      testing <- TRUE

      while (testing & nrow(df_sub) >= 3){
        df_sub[, (ewma.fields) := as.double(NaN)]

        # first, calculate which exponent we want to put through (pass a different
        # on for each exp)
        # subset df to only valid rows

        tmp <- data.frame(
          "before" = abs(df_sub$agedays - c(NA, df_sub$agedays[1:(nrow(df_sub)-1)])),
          "after" = abs(df_sub$agedays - c(df_sub$agedays[2:(nrow(df_sub))], NA))
        )
        maxdiff <- sapply(1:nrow(tmp), function(x){max(tmp[x,], na.rm = T)})
        exp_vals <- rep(-1.5, nrow(tmp))
        exp_vals[maxdiff > 365.25] <- -2.5
        exp_vals[maxdiff > 730.5] <- -3.5
        df_sub[, exp_vals := exp_vals]

        # calculate ewma
        df_sub[, (ewma.fields) := ewma(agedays, tbc.sd, exp_vals, TRUE)]
        df_sub[, paste0("c.",ewma.fields) := ewma(agedays, ctbc.sd, exp_vals, TRUE)]

        # calculate dewma
        df_sub[, `:=`(
          dewma.all = tbc.sd - ewma.all,
          dewma.before = tbc.sd - ewma.before,
          dewma.after = tbc.sd - ewma.after,

          c.dewma.all = ctbc.sd - c.ewma.all
        )]

        # 15E: calculate prior and next differences
        df_sub[, tbc_diff_next := tbc.sd - c(tbc.sd[2:nrow(df_sub)], NA)]
        df_sub[, tbc_diff_prior := tbc.sd - c(NA, tbc.sd[1:(nrow(df_sub)-1)])]

        df_sub[, tbc_diff_plus_next := tbc.p_plus - c(tbc.sd[2:nrow(df_sub)], NA)]
        df_sub[, tbc_diff_plus_prior :=
                 tbc.p_plus- c(NA, tbc.sd[1:(nrow(df_sub)-1)])]

        df_sub[, tbc_diff_minus_next := tbc.p_minus - c(tbc.sd[2:nrow(df_sub)], NA)]
        df_sub[, tbc_diff_minus_prior :=
                 tbc.p_minus- c(NA, tbc.sd[1:(nrow(df_sub)-1)])]

        # 15F: all exclusion criteria
        df_sub[,
               addcrithigh :=
                 dewma.before > 1 & dewma.after > 1 &
                 ((tbc_diff_next > 1 & tbc_diff_plus_next > 1 & tbc_diff_minus_next > 1) | is.na(tbc_diff_next)) &
                 ((tbc_diff_prior > 1 & tbc_diff_plus_prior > 1 & tbc_diff_minus_prior > 1) | is.na(tbc_diff_prior))
        ]
        df_sub[,
               addcritlow :=
                 dewma.before < -1 & dewma.after < -1 &
                 ((tbc_diff_next < -1 & tbc_diff_plus_next < -1 & tbc_diff_minus_next < -1) | is.na(tbc_diff_next)) &
                 ((tbc_diff_prior < -1 & tbc_diff_plus_prior < -1 & tbc_diff_minus_prior < -1) | is.na(tbc_diff_prior))
        ]

        #15G: find all the values for the DOP
        # find the comparison -- making sure to keep only valid rows
        compare_df <- data.df[subjid == df_sub$subjid[1] &
                                param == get_dop(df_sub$param[1]) &
                                exclude == "Include",]
        if (nrow(compare_df) > 0){
          df_sub[compare_df, tbc_dop := i.tbc.sd,
                 on = c("agedays")]
          df_sub[is.na(tbc_dop), tbc_dop := median(compare_df$tbc.sd)]
        } else {
          df_sub[, tbc_dop := NA]
        }

        #15H: all exclusions
        df_sub$rowind <- 1:nrow(df_sub)

        #a
        df_sub[-c(1, nrow(df_sub)),][
          dewma.all > 1 & (c.dewma.all > 1 | is.na(c.dewma.all)) & addcrithigh,
          exclude := "Exclude-EWMA2-middle"]
        df_sub[-c(1, nrow(df_sub)),][
          dewma.all < -1 & (c.dewma.all < -1 | is.na(c.dewma.all)) & addcritlow,
          exclude := "Exclude-EWMA2-middle"]

        #b
        df_sub[agedays == 0 & agedays[2] < 365.25 &
                 dewma.all > 3 & (c.dewma.all > 3 | is.na(c.dewma.all)) &
                 addcrithigh
               , exclude := "Exclude-EWMA2-birth-WT"]
        df_sub[agedays == 0 & agedays[2] < 365.25 &
                 dewma.all < -3 & (c.dewma.all < -3 | is.na(c.dewma.all)) &
                 addcritlow
               , exclude := "Exclude-EWMA2-birth-WT"]

        #c
        df_sub[agedays == 0 & agedays[2] >= 365.25 &
                 dewma.all > 4 & (c.dewma.all > 4 | is.na(c.dewma.all)) &
                 addcrithigh
               , exclude := "Exclude-EWMA2-birth-WT-ext"]
        df_sub[agedays == 0 & agedays[2] >= 365.25 &
                 dewma.all < -4 & (c.dewma.all < -4 | is.na(c.dewma.all)) &
                 addcritlow
               , exclude := "Exclude-EWMA2-birth-WT-ext"]

        #d
        df_sub[rowind == 1 & agedays[1] != 0 & agedays[2]-agedays[1] < 365.25 &
                 dewma.all > 2 & (c.dewma.all > 2 | is.na(c.dewma.all)) &
                 addcrithigh
               , exclude := "Exclude-EWMA2-first"]
        df_sub[rowind == 1 & agedays[1] != 0 & agedays[2]-agedays[1] < 365.25 &
                 dewma.all < -2 & (c.dewma.all < -2 | is.na(c.dewma.all)) &
                 addcritlow
               , exclude := "Exclude-EWMA2-first"]

        # e
        df_sub[rowind == 1 & agedays[1] != 0 & agedays[2]-agedays[1] >= 365.25 &
                 dewma.all > 3 & (c.dewma.all > 3 | is.na(c.dewma.all)) &
                 addcrithigh
               , exclude := "Exclude-EWMA2-first-ext"]
        df_sub[rowind == 1 & agedays[1] != 0 & agedays[2]-agedays[1] >= 365.25 &
                 dewma.all < -3 & (c.dewma.all < -3 | is.na(c.dewma.all)) &
                 addcritlow
               , exclude := "Exclude-EWMA2-first-ext"]

        # f
        df_sub[rowind == nrow(df_sub) &
                 agedays[nrow(df_sub)]-agedays[nrow(df_sub)-1] < 365.25*2 &
                 abs(tbc.sd[nrow(df_sub)-1]) < 2 &
                 dewma.all > 2 & (c.dewma.all > 2 | is.na(c.dewma.all)) &
                 addcrithigh
               , exclude := "Exclude-EWMA2-last"]
        df_sub[rowind == nrow(df_sub) &
                 agedays[nrow(df_sub)]-agedays[nrow(df_sub)-1] < 365.25*2 &
                 abs(tbc.sd[nrow(df_sub)-1]) < 2 &
                 dewma.all < -2 & (c.dewma.all < -2 | is.na(c.dewma.all)) &
                 addcritlow
               , exclude := "Exclude-EWMA2-last"]

        # g
        df_sub[rowind == nrow(df_sub) &
                 agedays[nrow(df_sub)]-agedays[nrow(df_sub)-1] < 365.25*2 &
                 abs(tbc.sd[nrow(df_sub)-1]) >= 2 &
                 dewma.all > abs(tbc.sd[nrow(df_sub)-1]) &
                 (c.dewma.all > 3 | is.na(c.dewma.all)) &
                 addcrithigh
               , exclude := "Exclude-EWMA2-last-high"]
        df_sub[rowind == nrow(df_sub) &
                 agedays[nrow(df_sub)]-agedays[nrow(df_sub)-1] < 365.25*2 &
                 abs(tbc.sd[nrow(df_sub)-1]) >= 2 &
                 dewma.all < abs(tbc.sd[nrow(df_sub)-1]) &
                 (c.dewma.all < -3 | is.na(c.dewma.all)) &
                 addcritlow
               , exclude := "Exclude-EWMA2-last-high"]

        # h
        df_sub[rowind == nrow(df_sub) &
                 agedays[nrow(df_sub)]-agedays[nrow(df_sub)-1] >= 365.25*2 &
                 abs(tbc.sd[nrow(df_sub)-1]) < 2 &
                 dewma.all > 3 &
                 (c.dewma.all > 3 | is.na(c.dewma.all)) &
                 (tbc.sd - tbc_dop > 4 | is.na(tbc_dop)) &
                 addcrithigh
               , exclude := "Exclude-EWMA2-last-ext"]
        df_sub[rowind == nrow(df_sub) &
                 agedays[nrow(df_sub)]-agedays[nrow(df_sub)-1] >= 365.25*2 &
                 abs(tbc.sd[nrow(df_sub)-1]) < 2 &
                 dewma.all < -3 &
                 (c.dewma.all <- 3 | is.na(c.dewma.all)) &
                 (tbc.sd - tbc_dop < -4 | is.na(tbc_dop)) &
                 addcritlow
               , exclude := "Exclude-EWMA2-last-ext"]

        # i
        df_sub[rowind == nrow(df_sub) &
                 agedays[nrow(df_sub)]-agedays[nrow(df_sub)-1] >= 365.25*2 &
                 abs(tbc.sd[nrow(df_sub)-1]) >= 2 &
                 dewma.all > (1+abs(tbc.sd[nrow(df_sub)-1])) &
                 (c.dewma.all > 3 | is.na(c.dewma.all)) &
                 (tbc.sd - tbc_dop > 4 | is.na(tbc_dop)) &
                 addcrithigh
               , exclude := "Exclude-EWMA2-last-ext-high"]
        df_sub[rowind == nrow(df_sub) &
                 agedays[nrow(df_sub)]-agedays[nrow(df_sub)-1] >= 365.25*2 &
                 abs(tbc.sd[nrow(df_sub)-1]) >= 2 &
                 dewma.all < (-1-abs(tbc.sd[nrow(df_sub)-1])) &
                 (c.dewma.all <- 3 | is.na(c.dewma.all)) &
                 (tbc.sd - tbc_dop < -4 | is.na(tbc_dop)) &
                 addcritlow
               , exclude := "Exclude-EWMA2-last-ext-high"]

        # figure out if any of the exclusions hit
        count_exclude <- sum(df_sub$exclude != "Include")
        if (count_exclude > 0){
          df_sub[, abssum := abs(tbc.sd + dewma.all)]

          # choose the highest abssum for exclusion
          idx <- df_sub$index[which.max(df_sub[df_sub$exclude != "Include",
                                               abssum])]

          exclude_all[ind_all == idx] <- df_sub[index == idx, exclude]

          #set up to continue on
          testing <- TRUE

          df_sub <- df_sub[index != idx, ]
        } else {
          testing <- FALSE
        }
      }

      return(exclude_all)
    })(copy(.SD)), , by = .(subjid, param), .SDcols = colnames(df)]

    return(df$exclude)
  })(copy(.SD)), .SDcols = colnames(data.df)]

  # 16: moderate EWMA for birth HT and HC ----

  if (!quietly)
    cat(sprintf(
      "[%s] Exclude moderate EWMA for birth height and head circumference...\n",
      Sys.time()
    ))

  # create the valid set
  # we only running carried forwards on valid values, non NNTE values,
  # and non single values, and non weight
  tmp <- table(paste0(data.df$subjid, "_", data.df$param))
  not_single <- paste0(data.df$subjid, "_", data.df$param) %in% names(tmp)[tmp > 1]
  valid_set <- valid(data.df, include.temporary.extraneous = FALSE) &
    !data.df$nnte_full & # does not use the "other"
    not_single &
    data.df$param != "WEIGHTKG" # do not run on weight

  # work in an a function order to encapsulate and not keep all the additional
  # columns

  # NOTE: check NNTE na's

  # calculate plus/minus values

  # order just for ease later
  data.df <- data.df[order(subjid, param, agedays),]
  data.df <- data.df[valid_set, exclude := (function(df) {
    # =calc plus/minus values
    df[param == "HEIGHTCM", p_plus := v+1]
    df[param == "HEIGHTCM", p_minus := v-1]

    df[param == "HEADCM", p_plus := v+1]
    df[param == "HEADCM", p_minus := v-1]

    #smooth and recenter
    df <- calc_and_recenter_z_scores(df, "p_plus", ref.data.path)
    df <- calc_and_recenter_z_scores(df, "p_minus", ref.data.path)

    #16a: calculate ewma -- run within a subject/parameter
    # inly evaluate on subject/parameters with a birth ageday
    df <- df[subjid %in% subjid[agedays == 0],
      exclude := (function(df_sub) {
        # save initial exclusions to keep track
        ind_all <- copy(df_sub$index)
        exclude_all <- copy(df_sub$exclude)

        testing <- TRUE

        while (testing & nrow(df_sub) >= 3){
          df_sub[, (ewma.fields) := as.double(NaN)]

          # first, calculate which exponent we want to put through (pass a different
          # on for each exp)
          # subset df to only valid rows

          tmp <- data.frame(
            "before" = abs(df_sub$agedays - c(NA, df_sub$agedays[1:(nrow(df_sub)-1)])),
            "after" = abs(df_sub$agedays - c(df_sub$agedays[2:(nrow(df_sub))], NA))
          )
          maxdiff <- sapply(1:nrow(tmp), function(x){max(tmp[x,], na.rm = T)})
          exp_vals <- rep(-1.5, nrow(tmp))
          exp_vals[maxdiff > 365.25] <- -2.5
          exp_vals[maxdiff > 730.5] <- -3.5
          df_sub[, exp_vals := exp_vals]

          # calculate ewma
          df_sub[, (ewma.fields) := ewma(agedays, tbc.sd, exp_vals, TRUE)]
          df_sub[, paste0("c.",ewma.fields) := ewma(agedays, ctbc.sd, exp_vals, TRUE)]

          # calculate dewma
          df_sub[, `:=`(
            dewma.all = tbc.sd - ewma.all,
            dewma.before = tbc.sd - ewma.before,
            dewma.after = tbc.sd - ewma.after,

            c.dewma.all = ctbc.sd - c.ewma.all
          )]

          # 16B: calculate prior and next differences
          df_sub[, tbc_diff_next := tbc.sd - c(tbc.sd[2:nrow(df_sub)], NA)]
          df_sub[, tbc_diff_prior := tbc.sd - c(NA, tbc.sd[1:(nrow(df_sub)-1)])]

          df_sub[, tbc_diff_plus_next := tbc.p_plus - c(tbc.sd[2:nrow(df_sub)], NA)]
          df_sub[, tbc_diff_plus_prior :=
                   tbc.p_plus- c(NA, tbc.sd[1:(nrow(df_sub)-1)])]

          df_sub[, tbc_diff_minus_next := tbc.p_minus - c(tbc.sd[2:nrow(df_sub)], NA)]
          df_sub[, tbc_diff_minus_prior :=
                   tbc.p_minus- c(NA, tbc.sd[1:(nrow(df_sub)-1)])]

          # 16C: all exclusion criteria
          df_sub[,
                 addcrithigh :=
                   dewma.before > 1 & dewma.after > 1 &
                   ((tbc_diff_next > 1 & tbc_diff_plus_next > 1 & tbc_diff_minus_next > 1) | is.na(tbc_diff_next)) &
                   ((tbc_diff_prior > 1 & tbc_diff_plus_prior > 1 & tbc_diff_minus_prior > 1) | is.na(tbc_diff_prior))
          ]
          df_sub[,
                 addcritlow :=
                   dewma.before < -1 & dewma.after < -1 &
                   ((tbc_diff_next < -1 & tbc_diff_plus_next < -1 & tbc_diff_minus_next < -1) | is.na(tbc_diff_next)) &
                   ((tbc_diff_prior < -1 & tbc_diff_plus_prior < -1 & tbc_diff_minus_prior < -1) | is.na(tbc_diff_prior))
          ]

          #16D: all exclusions
          df_sub$rowind <- 1:nrow(df_sub)

          #a
          df_sub[agedays == 0 & agedays[2] < 365.25 &
                   dewma.all > 3 & (c.dewma.all > 3 | is.na(c.dewma.all)) &
                   addcrithigh
                 , exclude := "Exclude-EWMA2-birth-HT-HC"]
          df_sub[agedays == 0 & agedays[2] < 365.25 &
                   dewma.all < -3 & (c.dewma.all < -3 | is.na(c.dewma.all)) &
                   addcritlow
                 , exclude := "Exclude-EWMA2-birth-HT-HC"]

          #b
          df_sub[agedays == 0 & agedays[2] >= 365.25 &
                   dewma.all > 4 & (c.dewma.all > 4 | is.na(c.dewma.all)) &
                   addcrithigh
                 , exclude := "Exclude-EWMA2-birth-HT-HC-ext"]
          df_sub[agedays == 0 & agedays[2] >= 365.25 &
                   dewma.all < -4 & (c.dewma.all < -4 | is.na(c.dewma.all)) &
                   addcritlow
                 , exclude := "Exclude-EWMA2-birth-HT-HC-ext"]

          # figure out if any of the exclusions hit
          count_exclude <- sum(df_sub$exclude != "Include")
          if (count_exclude > 0){
            df_sub[, abssum := abs(tbc.sd + dewma.all)]

            # choose the highest abssum for exclusion
            idx <- df_sub$index[which.max(df_sub[df_sub$exclude != "Include",
                                                 abssum])]

            exclude_all[ind_all == idx] <- df_sub[index == idx, exclude]

            #set up to continue on
            testing <- TRUE

            df_sub <- df_sub[index != idx, ]
          } else {
            testing <- FALSE
          }
        }
        return(exclude_all)
      })(copy(.SD)), , by = .(subjid, param), .SDcols = colnames(df)]

    return(df$exclude)
  })(copy(.SD)), .SDcols = colnames(data.df)]

  # 17: raw differences ----

  if (!quietly)
    cat(sprintf(
      "[%s] Exclude raw differences...\n",
      Sys.time()
    ))

  # read in tanner data
  tanner_ht_vel_rev_path <- ifelse(
    ref.data.path == "",
    system.file(file.path("extdata", "tanner_ht_vel.csv.gz"), package = "growthcleanr"),
    file.path(ref.data.path, "tanner_ht_vel.csv.gz")
  )

  tanner.ht.vel.rev <- fread(tanner_ht_vel_rev_path)

  setnames(tanner.ht.vel.rev,
           colnames(tanner.ht.vel.rev),
           gsub('_', '.', colnames(tanner.ht.vel.rev)))
  setkey(tanner.ht.vel.rev, sex, tanner.months)

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
  who.ht.vel <- as.data.table(dplyr::full_join(who.ht.vel, who.max.ht.vel, by =
                                                 c('sex', 'whoagegrp_ht')))

  setnames(who.ht.vel, colnames(who.ht.vel), gsub('_', '.', colnames(who.ht.vel)))
  setkey(who.ht.vel, sex, whoagegrp.ht)

  # read in who hc data
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
  who.hc.vel <- as.data.table(dplyr::full_join(who.hc.vel, who.max.hc.vel, by =
                                                 c('sex', 'whoagegrp_ht')))

  setnames(who.hc.vel, colnames(who.hc.vel), gsub('_', '.', colnames(who.hc.vel)))
  setkey(who.hc.vel, sex, whoagegrp.ht)

  # create the valid set
  # we only running carried forwards on valid values, non NNTE values,
  # and non single values, and non weight
  tmp <- table(paste0(data.df$subjid, "_", data.df$param))
  not_single <- paste0(data.df$subjid, "_", data.df$param) %in% names(tmp)[tmp > 1]
  valid_set <- valid(data.df, include.temporary.extraneous = FALSE) &
    !data.df$nnte_full & # does not use the "other"
    not_single &
    data.df$param != "WEIGHTKG" # do not run on weight

  # order just for ease later
  data.df <- data.df[order(subjid, param, agedays),]
  data.df <- data.df[valid_set, exclude := (function(df) {
    # work inside a closure to drop added column values

    # save initial exclusions to keep track
    ind_all <- copy(df$index)
    exclude_all <- copy(df$exclude)

    testing <- TRUE

    while (testing & nrow(df) > 1){
      # sort df since it got reordered with keys
      df <- df[order(agedays),]

      # 17A
      df[, d_agedays := dplyr::lead(agedays) - agedays]

      # 17E -- only applies to height
      if (df$param[1] == "HEIGHTCM"){
        # 17B
        df[, tanner.months := 6+12*(round(.5*(agedays + dplyr::lead(agedays))/365.25))]
        # set tanner to missing for smaller values
        df[(agedays/30.4375) < 30, tanner.months := NA]

        # merge with the tanner info
        setkey(df, sex, tanner.months)
        df <- tanner.ht.vel.rev[df]

        # 17C

        # a
        df[max.ht.vel < 2.54, max.ht.vel := 2.54]
        df[d_agedays > 2*30.4375 & max.ht.vel < 2*2.54, max.ht.vel := 2*2.54]
        df[d_agedays > .5*365.25 & max.ht.vel < 4*2.54, max.ht.vel := 4*2.54]
        df[d_agedays > 365.25 & max.ht.vel < 8*2.54, max.ht.vel := 8*2.54]

        # b
        df[d_agedays < 365.25, mindiff := .5*min.ht.vel*(d_agedays/365.25)-3 ]
        df[d_agedays > 365.25, mindiff := .5*min.ht.vel-3 ]
        df[d_agedays < 365.25,
           maxdiff := 2*min.ht.vel*(d_agedays/365.25)^1.5 + 5.5 ]
        df[d_agedays > 365.25,
           maxdiff := 2*min.ht.vel*(d_agedays/365.25)^0.33 + 5.5 ]

        # 17D
        # generate the who age group variable
        df[, whoagegrp.ht := round(agedays/30.4375)]
        df[whoagegrp.ht > 24 | dplyr::lead(whoagegrp.ht) > 24,
           whoagegrp.ht := NA]


        # 17E
        df[d_agedays >= 20 & d_agedays < 46, whoinc.age.ht := 1]
        df[d_agedays >= 46 & d_agedays < 76, whoinc.age.ht := 2]
        df[d_agedays >= 76 & d_agedays < 107, whoinc.age.ht := 3]
        df[d_agedays >= 107 & d_agedays < 153, whoinc.age.ht := 4]
        df[d_agedays >= 153 & d_agedays < 199, whoinc.age.ht := 6]

        # update the edge intervals
        df[d_agedays < 20, whoinc.age.ht := 1]
        df[d_agedays > 199, d_agedays := 200]
        df[d_agedays == 200, whoinc.age.ht := 6]

        # 17F
        # merge with WHO
        # add the column name we want to grab
        m_who_ht_vel <- merge(df, who.ht.vel, by = c("sex", "whoagegrp.ht"),
                               all.x = TRUE, sort = FALSE)
        for (i in unique(df$whoinc.age.ht[!is.na(df$whoinc.age.ht) &
                                          !is.na(df$whoagegrp.ht)])){
          sub_m_who_ht_vel <- m_who_ht_vel[whoinc.age.ht == i,]

          cn <- paste0("whoinc.", i, ".ht")
          df[whoinc.age.ht == i,
             who_mindiff_ht := as.numeric(sub_m_who_ht_vel[, get(cn)])]
          cn <- paste0("max.whoinc.", i, ".ht")
          df[whoinc.age.ht == i,
             who_maxdiff_ht := as.numeric(sub_m_who_ht_vel[, get(cn)])]
        }
        # if there are none, preallocate for ease
        if (length(unique(df$whoinc.age.ht[!is.na(df$whoinc.age.ht) &
                                           !is.na(df$whoagegrp.ht)])) < 1){
          df[, who_mindiff_ht := NA_real_]
          df[, who_maxdiff_ht := NA_real_]
        }
        df[, who_mindiff_ht := as.numeric(who_mindiff_ht)]
        df[, who_maxdiff_ht := as.numeric(who_maxdiff_ht)]

        # 17G
        df[d_agedays < whoinc.age.ht*30.4375, who_mindiff_ht :=
             who_mindiff_ht * d_agedays/(whoinc.age.ht*30.4375)]
        df[d_agedays > whoinc.age.ht*30.4375, who_maxdiff_ht :=
             who_maxdiff_ht * d_agedays/(whoinc.age.ht*30.4375)]

        df[d_agedays < 9*30.4375, who_mindiff_ht := who_mindiff_ht*.5-3]
        df[d_agedays < 9*30.4375, who_maxdiff_ht := who_maxdiff_ht*2+3]

        # 17H
        # tanner is implicit
        # greater than 9 months, use tanner if available, otherwise who
        df[(d_agedays < 9*30.4375 | is.na(min.ht.vel)) &
             !is.na(who_mindiff_ht), mindiff := who_mindiff_ht]
        df[(d_agedays < 9*30.4375 | is.na(min.ht.vel)) &
             !is.na(who_mindiff_ht), maxdiff := who_maxdiff_ht]
        # otherwise, fill in
        df[is.na(mindiff), mindiff := 3]
        # for birth measurements, add allowance of 1.5cm
        df[agedays == 0, mindiff := mindiff - 1.5]
        df[agedays == 0, maxdiff := maxdiff + 1.5]

        # 17I
        # sort df since it got reordered with keys
        df <- df[order(agedays),]
        df[, mindiff_prev := dplyr::lag(mindiff)]
        df[, maxdiff_prev := dplyr::lag(maxdiff)]
      } else { # head circumference
        # 17D
        # generate the who age group variable
        df[, whoagegrp.ht := round(agedays/30.4375)]
        df[whoagegrp.ht > 24 | dplyr::lead(whoagegrp.ht) > 24,
           whoagegrp.ht := NA]

        # 17J
        df[d_agedays >= 46 & d_agedays < 76, whoinc.age.hc := 2]
        df[d_agedays >= 76 & d_agedays < 107, whoinc.age.hc := 3]
        df[d_agedays >= 107 & d_agedays < 153, whoinc.age.hc := 4]
        df[d_agedays >= 153 & d_agedays < 199, whoinc.age.hc := 6]

        # update the edge intervals to missing
        df[d_agedays < 46 | d_agedays > 199, whoinc.age.hc := NA]

        # 17K
        # merge with WHO
        # add the column name we want to grab

        m_who_hc_vel <- merge(df, who.hc.vel, by = c("sex", "whoagegrp.ht"),
                              all.x = TRUE, sort = FALSE)
        for (i in unique(df$whoinc.age.hc[!is.na(df$whoinc.age.hc) &
                                          !is.na(df$whoagegrp.hc)])){
          sub_m_who_hc_vel <- m_who_hc_vel[whoinc.age.ht == i,]

          cn <- paste0("whoinc.", i, ".ht")
          df[whoinc.age.hc == i,
             who_mindiff_hc := as.numeric(sub_m_who_ht_vel[, get(cn)])]
          cn <- paste0("max.whoinc.", i, ".ht")
          df[whoinc.age.hc == i,
             who_maxdiff_hc := as.numeric(sub_m_who_ht_vel[, get(cn)])]
        }
        # if there are none, preallocate for ease
        if (length(unique(df$whoinc.age.hc[!is.na(df$whoinc.age.hc) &
                                           !is.na(df$whoagegrp.hc)])) < 1){
          df[, who_mindiff_hc := NA_real_]
          df[, who_maxdiff_hc := NA_real_]
        }
        df[, who_mindiff_hc := as.numeric(who_mindiff_hc)]
        df[, who_maxdiff_hc := as.numeric(who_maxdiff_hc)]

        # 17L
        df[d_agedays < whoinc.age.hc*30.4375, who_mindiff_hc :=
             who_mindiff_hc * d_agedays/(whoinc.age.hc*30.4375)]
        df[d_agedays > whoinc.age.hc*30.4375, who_maxdiff_hc :=
             who_maxdiff_hc * d_agedays/(whoinc.age.hc*30.4375)]

        df[d_agedays < 9*30.4375, who_mindiff_hc := who_mindiff_hc*.5-1.5]
        df[d_agedays < 9*30.4375, who_maxdiff_hc := who_maxdiff_hc*2+1.5]

        # 17M
        # use who
        df[, mindiff := who_mindiff_hc]
        df[, maxdiff := who_maxdiff_hc]
        # otherwise, fill in
        df[is.na(mindiff), mindiff := -1.5]
        # for birth measurements, add allowance of .5cm
        df[agedays == 0, mindiff := mindiff - .5]
        df[agedays == 0, maxdiff := maxdiff + .5]

        # 17N
        # sort df since it got reordered with keys
        df <- df[order(agedays),]
        df[, mindiff_prev := dplyr::lag(mindiff)]
        df[, maxdiff_prev := dplyr::lag(maxdiff)]
      }

      # 17O: generate ewma
      df[, (ewma.fields) := as.double(NaN)]

      # first, calculate which exponent we want to put through (pass a different
      # on for each exp)
      # subset df to only valid rows
      tmp <- data.frame(
        "before" = abs(df$agedays - c(NA, df$agedays[1:(nrow(df)-1)])),
        "after" = abs(df$agedays - c(df$agedays[2:(nrow(df))], NA))
      )
      maxdiff_e <- sapply(1:nrow(tmp), function(x){max(tmp[x,], na.rm = T)})
      exp_vals <- rep(-1.5, nrow(tmp))
      exp_vals[maxdiff_e > 365.25] <- -2.5
      exp_vals[maxdiff_e > 730.5] <- -3.5
      df[, exp_vals := exp_vals]

      # calculate ewma
      df[, (ewma.fields) := ewma(agedays, tbc.sd, exp_vals, TRUE)]

      # calculate dewma
      df[, `:=`(
        dewma.all = tbc.sd - ewma.all,
        dewma.before = tbc.sd - ewma.before,
        dewma.after = tbc.sd - ewma.after
      )]

      # add differences for convenience
      df[, diff_prev := (v-dplyr::lag(v))]
      df[, diff_next := (dplyr::lead(v)-v)]

      if (nrow(df) > 2){
        # 17P/R: identify pairs and calculate exclusions
        df[, pair := (v-dplyr::lag(v)) < mindiff_prev |
             (dplyr::lead(v)-v) < mindiff |
             (v-dplyr::lag(v)) > maxdiff_prev | (dplyr::lead(v)-v) > maxdiff
        ]
        df[is.na(pair), pair := FALSE]
        df[pair & abs(dewma.before) > dplyr::lag(abs(dewma.after)),
           bef.g.aftm1 := TRUE]
        df[pair & abs(dewma.after) > dplyr::lead(abs(dewma.before)),
           aft.g.aftm1 := TRUE]

        # Q
        df[, val_excl := exclude]
        df[diff_prev < mindiff_prev & bef.g.aftm1, val_excl := "Exclude-Min-diff"]
        df[diff_next < mindiff & aft.g.aftm1, val_excl := "Exclude-Min-diff"]
        df[diff_prev > maxdiff_prev & bef.g.aftm1, val_excl := "Exclude-Max-diff"]
        df[diff_next > maxdiff & aft.g.aftm1, val_excl := "Exclude-Max-diff"]



      } else { # only 2 values
        # 17Q/R -- exclusions for pairs
        df[, val_excl := exclude]
        df[diff_prev < mindiff_prev & abs(tbc.sd) > dplyr::lag(abs(tbc.sd)),
           val_excl := "Exclude-Min-diff"]
        df[diff_next < mindiff & abs(tbc.sd) > dplyr::lead(abs(tbc.sd)),
           val_excl := "Exclude-Min-diff"]
        df[diff_prev > maxdiff_prev & abs(tbc.sd) > dplyr::lag(abs(tbc.sd)),
           val_excl := "Exclude-Max-diff"]
        df[diff_next > maxdiff &  abs(tbc.sd) > dplyr::lead(abs(tbc.sd)),
           val_excl := "Exclude-Max-diff"]
      }

      # figure out if any of the exclusions hit
      count_exclude <- sum(df$val_excl != "Include")
      if (count_exclude > 0){
        if (nrow(df) > 2){
          df[, absval := abs(dewma.all)]
        } else {
          df[, absval := abs(tbc.sd)]
        }

        # choose the highest abssum for exclusion
        idx <- df$index[which.max(df[df$val_excl != "Include",
                                     absval])]

        exclude_all[ind_all == idx] <- df[index == idx, val_excl]

        #set up to continue on
        if (count_exclude > 1){
          testing <- TRUE

          df <- df[index != idx, ]
        } else {
          testing <- FALSE
        }
      } else {
        testing <- FALSE
      }
    }

    return(exclude_all)
  })(copy(.SD)), by = .(subjid, param), .SDcols = colnames(data.df)]

  # 19: 1 or 2 measurements ----

  if (!quietly)
    cat(sprintf(
      "[%s] Exclude 1 or 2 measurements...\n",
      Sys.time()
    ))

  # create the valid set
  # we only running carried forwards on valid values, non NNTE values,
  # and non single values, and non weight
  tmp <- table(paste0(data.df$subjid, "_", data.df$param))
  only_single_pairs <- paste0(data.df$subjid, "_", data.df$param) %in% names(tmp)[tmp <= 2]
  valid_set <- valid(data.df, include.temporary.extraneous = FALSE) &
    !data.df$nnte_full & # does not use the "other"
    only_single_pairs

  # order just for ease later
  data.df <- data.df[order(subjid, param, agedays),]
  data.df <- data.df[valid_set, exclude := (function(df) {
    # save initial exclusions to keep track
    ind_all <- copy(df$index)
    exclude_all <- copy(df$exclude)

    # 19A: is it a single or a pair?
    is_single <- nrow(df) == 1

    # find the DOP (designated other parameter)
    dop <-
      data.df[subjid == df$subjid[1] & param == get_dop(df$param[1]) &
                exclude == "Include",]

    # 19D: calculate the voi comparison
    if (nrow(dop) > 0){
      for (i in 1:nrow(df)){
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

    # 19B/C: for pairs, calculate info
    if (!is_single){
      diff_tbc.sd <- df[2, tbc.sd] - df[1, tbc.sd]
      diff_ctbc.sd <- df[2, ctbc.sd] - df[1, ctbc.sd]
      diff_agedays <- df[2, agedays] - df[1, agedays]

      abs_tbd.sd <- abs(df[1, tbc.sd])
      abs_ctbd.sd <- abs(df[1, ctbc.sd])

      med_dop <-
        if (nrow(dop) > 0){
          median(dop$tbc.sd)
        } else {
          NA
        }
      med_cdop <-
        if (nrow(dop) > 0){
          median(dop$ctbc.sd)
        } else {
          NA
        }

      # 19E: which is larger
      max_ind <- if (!all(is.na(df$comp_diff))){
        which.max(df$comp_diff)
      } else {
        which.max(abs(df$tbc.sd))
      }

      # 19F/G: which exclusion
      if (diff_tbc.sd > 4 & (diff_ctbc.sd > 4 | is.na(diff_ctbc.sd)) &
          diff_agedays >=365.25){
        df[max_ind, exclude := "Exclude-2-meas->1-year"]
      } else if (diff_tbc.sd > 2.5 & (diff_ctbc.sd > 2.5 | is.na(diff_ctbc.sd)) &
                 diff_agedays < 365.25){
        df[max_ind, exclude := "Exclude-2-meas-<1-year"]
      }

      # save the results
      exclude_all <- df$exclude

      # 19H
      # if one needs to get removed, we want to reevaluate as a single
      df <- df[exclude == "Include",]
    }

    if (nrow(df) == 1){
      df[(abs(tbc.sd) > 3 & !is.na(comp_diff) & comp_diff > 5) |
           (abs(tbc.sd) > 5 & is.na(comp_diff)),
         exclude := "Exclude-1-meas"]
    }

    return(exclude_all)
  })(copy(.SD)), by = .(subjid, param), .SDcols = colnames(data.df)]

  # 21: error load ----

  if (!quietly)
    cat(sprintf(
      "[%s] Exclude error load...\n",
      Sys.time()
    ))

  valid_set <- !data.df$nnte_full

  data.df[valid_set,
          err_ratio := sum(!exclude %in%
                             c("Include",
                               "Exclude-SDE-Identical",
                               "Exclude-SDE-All-Exclude",
                               "Exclude-SDE-All-Extreme",
                               "Exclude-SDE-EWMA",
                               "Exclude-SDE-All-Extreme",
                               "Exclude-SDE-One-Day",
                               "Exclude-Carried-Forward",
                               "Exclude-1-CF-deltaZ-<0.05",
                               "Exclude-1-CF-deltaZ-<0.1-wholehalfimp",
                               "Exclude-Teen-2-plus-CF-deltaZ-<0.05",
                               "Exclude-Teen-2-plus-CF-deltaZ-<0.1-wholehalfimp"
                             ))/.N,
          by = c("subjid", "param")]

  data.df[valid_set & err_ratio > .4 & exclude == "Include",
          exclude := "Exclude-Error-load"]


  # end ----

  if (!quietly)
    cat(sprintf("[%s] Completed Batch #%s...\n", Sys.time(), data.df$batch[1]))
  if (!quietly & parallel)
    sink()

  return(data.df[j = .(line, exclude, tbc.sd, param)]) #debugging
}
