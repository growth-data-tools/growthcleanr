# Main pediatric growthcleanr function -- cleanbatch
# internal supporting functions for pediatrics can be found in: pediatric_support.R

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
cleanbatch <- function(data.df,
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

  # save a copy of all original measurement values before any transformation
  data.df[, v.orig := v]

  if (!quietly)
    cat(sprintf(
      "[%s] Preliminarily identify potential extraneous...\n",
      Sys.time()
    ))
  data.df$exclude[temporary_extraneous(data.df)] <- 'Exclude-Temporary-Extraneous-Same-Day'

  # capture a list of subjects with possible extraneous for efficiency later
  subj.dup <- data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', unique(subjid)]

  # 7a.  For each day on which a subject had both a weight and a height recorded, calculate tbc*sd_sw: SD scores as if the weight had been recorded as the height
  #      and the height had been recorded as the weight, recentered using rcsd_*.  I intentionally did not allow values that were the first or last for a subject/parameter to be replaced as a switch.
  # NOTE: this additional constraint related to first and last values is implemented below via the code
  # swap.flag.1 := if(.N>2) c(FALSE, rep(TRUE, .N-2), FALSE) else FALSE, by=.(subjid,param)

  if (!quietly)
    cat(sprintf(
      "[%s] Identify potentially swapped measurements...\n",
      Sys.time()
    ))

  data.df[, v.sw := swap_parameters(field.name = 'v', df = data.df)]
  # calculate "standard deviation" score for the "swapped" parameter and recenter
  data.df[, tbc.sd.sw := measurement.to.z(param, agedays, sex, v.sw, TRUE) - sd.median]

  # 7b.  Perform a EWMA calculation.
  #   i.	In addition to the standard all/bef/aft dewma_* variables calculate all/bef/aft dewma_*_sw variables by subtracting EWMASD from tbc*sd_sw
  data.df[, (ewma.fields) := as.double(NaN)]
  data.df[valid(exclude), (ewma.fields) := ewma(agedays, tbc.sd, ewma.exp, TRUE), by =
            .(subjid, param)]


  # 7c.  Label pairs of height/weight measurements recorded on the same day as a switch if ALL of the following criteria are met for BOTH parameters:
  #   i.	For the weight value:
  #       exc_wt==0 & tbcwtsd>4 & |tbcwtsd_sw|<3 & dewma_wt>3 & dewma_wt_bef>2 & dewma_wt_aft>2
  #         & |dewma_wt_sw|<0.3 & |dewma_wt_sw_bef|<0.5 & |dewma_wt_sw_aft|<0.5
  #   ii.	For the height value:
  #       exc_ht==0 & tbchtsd<-7 & |tbchtsd_sw|<3 & dewma_ht<-6 & dewma_ht_bef<-5 & dewma_ht_aft<-5 & |dewma_ht_sw|<0.3 & |dewma_ht_sw_bef|<0.5 & |dewma_ht_sw_aft|<0.5
  #

  # initialize swap.flag.1
  data.df[, `:=`(valid.interior.measurement = FALSE,
                 swap.flag.1 = FALSE)]

  # flag interior measurements
  data.df[valid(data.df), valid.interior.measurement := if (.N > 2)
    c(FALSE, rep(TRUE, .N - 2), FALSE)
    else
      FALSE, by = .(subjid, param)]
  data.df[valid.interior.measurement &
            (
              param == 'WEIGHTKG' &
                tbc.sd > 4 &
                abs(tbc.sd.sw) < 3 &
                (tbc.sd - ewma.all) > 3 &
                (tbc.sd - ewma.before) > 2 &
                (tbc.sd - ewma.after) > 2
              &
                abs(tbc.sd.sw - ewma.all) < 0.3 &
                abs(tbc.sd.sw - ewma.before) < 0.5 &
                abs(tbc.sd.sw - ewma.after) < 0.5
              |
                param == 'HEIGHTCM' &
                tbc.sd < -7 &
                abs(tbc.sd.sw) < 3 &
                (tbc.sd - ewma.all) < -6 &
                (tbc.sd - ewma.before) < -5 &
                (tbc.sd - ewma.after) < -5
              &
                abs(tbc.sd.sw - ewma.all) < 0.3 &
                abs(tbc.sd.sw - ewma.before) < 0.5 &
                abs(tbc.sd.sw - ewma.after) < 0.5
            ), swap.flag.1 := TRUE]


  # 7d.  For pairs of measurements that meet criteria for a switch, do the following:
  #   i.	Replace wt with the value that was originally recorded as the ht, and replace ht with the value that was originally recorded as the wt
  #   ii.	Replace tbc*sd with the values for tbc*sd_sw
  data.df$swap.flag.2 <- swap_parameters(field.name = 'swap.flag.1', df = data.df)
  data.df[swap.flag.1 &
            swap.flag.2, `:=`(v = v.sw,
                              tbc.sd = tbc.sd.sw,
                              exclude = 'Swapped-Measurements')]

  # look for possible unit errors
  # calculate additional z-scores if we will be attempting to correct unit errors
  if (recover.unit.error) {
    # 8.  Identify unit errors (weight/height recorded in wrong units)
    # a.  Generate variables transformed by conversion factors for kg/lbs and inches/cm:
    #    i.	  wt_d_2=wt/2.204622
    #    ii.	wt_t_2=wt*2.204622
    #    iii.	ht_d_2=ht/2.54
    #    iv.	ht_t_2=ht*2.54
    if (!quietly)
      cat(sprintf("[%s] Identify and recover unit errors...\n", Sys.time()))
    valid.rows <- valid(data.df)
    data.df[, `:=`(
      v.d = v / ifelse(param == 'WEIGHTKG', 2.204622, 2.54),
      v.t = v * ifelse(param == 'WEIGHTKG', 2.204622, 2.54)
    )]

    # 8b.  Calculate SD scores for each transformed variable and recenter these (tbc*sd_d_2 and tbc_*sd_t_2)
    data.df$tbc.sd.d <- with(data.df,
                             measurement.to.z(param, agedays, sex, v.d, TRUE) - sd.median)
    data.df$tbc.sd.t <- with(data.df,
                             measurement.to.z(param, agedays, sex, v.t, TRUE) - sd.median)

    # 8c.  Perform a EWMA calculation
    #    i.	  In addition to the standard all/bef/aft dewma_* variables calculate all/bef/aft variables for  dewma_*_d_2 and dewma_*_t_2  by subtracting EWMASD from
    #         tbc*sd_d_2 and tbc_*_sd_t_2
    # NOTE: the additional dewma fields are calculated in the tests below
    data.df[, (ewma.fields) := as.double(NaN)]
    data.df[valid(exclude), (ewma.fields) := ewma(agedays, tbc.sd, ewma.exp, TRUE), by =
              .(subjid, param)]

    # 8d.  Also calculate d_prevsd_*=tbc*sd-tbc*sdprev and d_nextsd_*=tbc*sdi-tbc*sdnext. d_prevsd_* will be missing for the first value and d_nextsd_* should be
    #     missing for the last value. I intentionally did not allow values that were the first or last for a subject/parameter to be replaced as unit errors.
    # structure c(NA, field.name[-.N]) == get.prev
    # structure c(field.name[-1], NA) == get.next
    data.df[, `:=`(delta.prev.sd = as.double(NaN),
                   delta.next.sd = as.double(NaN))]
    data.df[valid.rows, `:=`(
      delta.prev.sd = tbc.sd - c(NA, tbc.sd[-.N]),
      delta.next.sd = tbc.sd - c(tbc.sd[-1], NA)
    ), by = .(subjid, param)]

    # 8e.	Identify a value as a unit error if one of the following sets of criteria are met:
    #    i.  	For wt_d_2: dewma_wt>3 & dewma_wt_bef>2 & dewma_wt_aft>2 & tbcwtsd>3 & d_nextsd_wt>2 & d_nextsd_wt is not missing & d_prevsd_wt>2 & d_prevsd_wt is not missing
    #        & abs(dewma_wt_d_2)<0.3 & abs(dewma_wt_d_2_bef)<0.5 & abs(dewma_wt_d_2_aft)<0.5 & abs(tbcwtsd_d_2)<3 & exc_wt==0
    #    ii.	For wt_t_2: dewma_wt<-3 & dewma_wt_bef<-2 & dewma_wt_aft<-2 & tbcwtsd<-3 & d_nextsd_wt<-2 & d_nextsd_wt is not missing & d_prevsd_wt<-2 & d_prevsd_wt is not missing
    #        & abs(dewma_wt_t_2)<0.3 & abs(dewma_wt_t_2_bef)<0.5 & abs(dewma_wt_t_2_aft)<0.5 & abs(tbcwtsd_t_2)<3 & exc_wt==0
    #    iii.	For ht_d_2: dewma_ht>5 & dewma_ht_bef>4 & dewma_ht_aft>4 & tbchtsd>7 & d_nextsd_ht>4 & d_nextsd_ht is not missing & d_prevsd_ht>4 & d_prevsd_ht is not missing
    #        & abs(dewma_ht_d_2)<0.3 & abs(dewma_ht_d_2_bef)<0.5 & abs(dewma_ht_d_2_aft)<0.5 & abs(tbchtsd_d_2)<3 & exc_ht==0
    #    iv.	For ht_t_2: dewma_ht<-5 & dewma_ht_bef<-4 & dewma_ht_aft<-4 & tbchtsd<-7 & d_nextsd_ht<-4 & d_nextsd_ht is not missing & d_prevsd_ht<-4 & d_prevsd_ht is not missing
    #        & abs(dewma_ht_t_2)<0.3 & abs(dewma_ht_t_2_bef)<0.5 & abs(dewma_ht_t_2_aft)<0.5 & abs(tbchtsd_t_2)<3 & exc_ht==0
    # f.  For values that are identified as unit errors by one of the sets of criteria above, replace wt or ht with the corresponding transformed value and
    #     replace tbc*sd with the recentered sd-score for the transformed value

    # process unit error low first (8.e.i and iii from above), re-factor slightly for efficiency
    data.df[valid(data.df) &
              abs(tbc.sd.d - ewma.all) < 0.3 &
              abs(tbc.sd.d - ewma.before) < 0.5 &
              abs(tbc.sd.d - ewma.after) < 0.5 & abs(tbc.sd.d) < 3
            &
              (
                param == 'WEIGHTKG' &
                  (tbc.sd - ewma.all) > 3 &
                  (tbc.sd - ewma.before) > 2 &
                  (tbc.sd - ewma.after) > 2 & tbc.sd > 3
                &
                  !is.na(delta.next.sd) &
                  delta.next.sd > 2 &
                  !is.na(delta.prev.sd) & delta.prev.sd > 2
                |
                  param == 'HEIGHTCM' &
                  (tbc.sd - ewma.all) > 5 &
                  (tbc.sd - ewma.before) > 4 &
                  (tbc.sd - ewma.after) > 4 & tbc.sd > 7
                &
                  !is.na(delta.next.sd) &
                  delta.next.sd > 4 &
                  !is.na(delta.prev.sd) & delta.prev.sd > 4
              ),
            `:=`(v = v.d,
                 tbc.sd = tbc.sd.d,
                 exclude = 'Unit-Error-High')]

    # process unit error high second (8.e.ii and iv from above), re-factor slightly for efficiency
    data.df[valid(data.df) &
              abs(tbc.sd.t - ewma.all) < 0.3 &
              abs(tbc.sd.t - ewma.before) < 0.5 &
              abs(tbc.sd.t - ewma.after) < 0.5 & abs(tbc.sd.t) < 3
            &
              (
                param == 'WEIGHTKG' &
                  (tbc.sd - ewma.all) < -3 &
                  (tbc.sd - ewma.before) < -2 &
                  (tbc.sd - ewma.after) < -2 & tbc.sd < -3
                &
                  !is.na(delta.next.sd) &
                  delta.next.sd < -2 &
                  !is.na(delta.prev.sd) & delta.prev.sd < -2
                |
                  param == 'HEIGHTCM' &
                  (tbc.sd - ewma.all) < -5 &
                  (tbc.sd - ewma.before) < -4 &
                  (tbc.sd - ewma.after) < -4 & tbc.sd < -7
                &
                  !is.na(delta.next.sd) &
                  delta.next.sd < -4 &
                  !is.na(delta.prev.sd) & delta.prev.sd < -4
              ),
            `:=`(v = v.t,
                 tbc.sd = tbc.sd.t,
                 exclude = 'Unit-Error-Low')]
  }

  # 9.  Exclude values that are carried forward. For the purposes of this analysis, any value that is identical to the preceding value for the same parameter
  #     and subject is considered carried forward. The chances of having identical measurements, even at an age/interval when little or no growth would be expected,
  #     is fairly small, and when this is the case the carried forward measurements provide little new information.
  # a.	Calculate d_prev_wt=wt-wtprev and d_prev_ht=ht-htprev. Use original measurements rather than transformed
  #     measurements (unit errors and switches).
  # b.	Unlike most steps, do this step for all extraneous values (exc_*==2) in addition to included values (exc_*==0), comparing all values for one day to all
  #     values from the prior day – if there are any values with a d_prev*==0, the value on the latter day should be excluded.
  # c.	Replace exc_*=3 for all values with d_prev*==0 & (exc_*==0 OR exc_*==2)
  if (!include.carryforward) {
    if (!quietly)
      cat(sprintf(
        "[%s] Exclude measurements carried forward...\n",
        Sys.time()
      ))
    # for efficiency, bring get.prev and get.next inline here (working on valid rows within a single parameter for a single subject)
    # structure c(NA, field.name[-.N]) == get.prev
    data.df[, prev.v := as.double(NaN)]
    data.df[valid(data.df), prev.v := c(NA, v.orig[-.N]), by = .(subjid, param)]

    # optimize "carry forward" for children without extraneous.
    data.df[!(subjid %in% subj.dup) &
              v.orig == prev.v, exclude := 'Exclude-Carried-Forward']

    # need to handle children with extraneous measurements on same day separately
    data.df[subjid %in% subj.dup &
              valid(data.df, include.temporary.extraneous = TRUE), exclude := (function(df) {
                setkey(df, agedays)
                ages = unique(agedays)
                # no point in looking for measurements carried forward if all measurements are from a single day of life
                if (length(ages) > 1) {
                  # iterate over each age
                  for (i in 2:length(ages)) {
                    # find the set of measurements from the previous age in days
                    all.prev.v = df[agedays == ages[i - 1], v.orig]
                    # if a measurement for the current age is found in the set of measurements from the previous age, then mark it as carried forward
                    df[agedays == ages[i] &
                         v.orig %in% all.prev.v, exclude := 'Exclude-Carried-Forward']
                  }
                }
                return(df$exclude)
              })(copy(.SD)), .SDcols = c('agedays', 'exclude', 'v.orig'), by = .(subjid, param)]
  }

  # 9d.  Replace exc_*=0 if exc_*==2 & redo step 5 (temporary extraneous)
  data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
  data.df[temporary_extraneous(data.df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']

  # 10.  Exclude extreme errors with SD cutoffs. For this, a cutoff of |SD|>25 is used. Because of differences in SD and z score, there are some very extreme values
  #      with a |z|>25 that are implausible with an |SD|<25, so both are used to exclude extreme errors. This works better than using a lower value for the limit
  #      for |SD|.
  # a.	Generally we only evaluate measurements where exc_*==0, but for this step we also need to evaluate measurements with exc_*==2
  # b.	Replace exc_*=4 if |tbc*sd|>25 & (exc_*==0 OR exc_*==2)
  # c.	Replace exc_*=4 if |*z|>25 & (exc_*==0 OR exc_*==2) & the value is not switched or transformed.
  if (!quietly)
    cat(sprintf(
      "[%s] Exclude extreme measurements based on SD...\n",
      Sys.time()
    ))
  data.df[na_as_false(
    valid(data.df, include.temporary.extraneous = TRUE) & abs(tbc.sd) > sd.extreme
    |
      exclude %in% c('Include', 'Exclude-Temporary-Extraneous-Same-Day') &
      abs(z.orig) > z.extreme
  ),
  exclude := 'Exclude-SD-Cutoff']

  # 10d. Redo temporary extraneous as in step 5.
  data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
  data.df[temporary_extraneous(data.df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']

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

      num.valid <- sum(valid(df))
      rep <- na_as_false(with(
        df,
        num.valid >= 3 & valid(df)
        &
          (
            dewma.all > 3.5 & dewma.before > 3 & dewma.after > 3 & tbc.sd > 3.5
            |
              dewma.all < -3.5 &
              dewma.before < -3 & dewma.after < -3 & tbc.sd < -3.5
          )
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
        df[worst.row, exclude := 'Exclude-EWMA-Extreme']
      }

      # 11f.  For subjects/parameters with only 2 values, calculate abstbc*sd=|tbc*sd|
      # g.  Replace exc_*=6 for values that meet all of the following criteria
      #   i.	There are 2 measurements for that subject and parameter
      #   ii.	(dewma_*>3.5 & tbc*sd>3.5) OR (dewma_*<-3.5 & tbc*sd<-3.5)
      #   iii.	If there are 2 measurements for a subject/parameter that meet criteria ii, only replace exc_*=6 for the value with the larger abstbc*sd
      rep <- na_as_false(with(
        df,
        num.valid == 2 &
          (
            tbc.sd - ewma.all > 3.5 &
              tbc.sd > 3.5 |
              tbc.sd - ewma.all < -3.5 & tbc.sd < -3.5
          )
      ))
      num.exclude <- sum(rep)
      if (num.exclude == 1)
        df[rep, exclude := 'Exclude-EWMA-Extreme-Pair']
      if (num.exclude > 1) {
        # first order by decreasing abssum
        worst.row <- with(df, order(rep, abs(tbc.sd), decreasing = TRUE))[1]
        df[worst.row, exclude := 'Exclude-EWMA-Extreme-Pair']
      }

      # 11h.  Recalculate temporary extraneous as in step 5
      # optimize: only perform these steps if this subject is known to have extraneous measurements
      if (has.extraneous) {
        df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']
        df[temporary_extraneous(df), exclude := 'Exclude-Temporary-Extraneous-Same-Day']
      }

      # 11i.  If there was at least one subject who had a potential exclusion identified in step 11c, repeat steps 11b-11g. If there were no subjects with potential
      #     exclusions identified in step 11c, move on to step 12.
      newly.excluded <- sum(df$exclude %in% c('Exclude-EWMA-Extreme', 'Exclude-EWMA-Extreme-Pair'))
      if (newly.excluded > num.ewma.excluded) {
        num.ewma.excluded <- newly.excluded
      } else {
        break
      }
    }
    return(df$exclude)
  })(copy(.SD)), by = .(subjid, param), .SDcols = c('index', 'sex', 'agedays', 'tbc.sd', 'exclude')]



  # 12. Redo extraneous using EWMA. This will be the final time extraneous are done.
  #     For some extraneous it is very difficult to tell which one is likely representative. If the extraneous are very similar to each
  #     other, we will select one. If it is very difficult to tell which one is correct and the extraneous are not very similar, we will
  #     exclude all extraneous for that subject/parameter on that day
  # a.	Replace exc_*=0 for all temporarily excluded extraneous (exc_*==0)
  if (!quietly)
    cat(sprintf("[%s] Exclude extraneous based on EWMA...\n", Sys.time()))
  data.df[exclude == 'Exclude-Temporary-Extraneous-Same-Day', exclude := 'Include']

  # 12b. Select which extraneous to include in EWMA calculations using the same criteria as in step 5. However, do not include values
  #      in these medians that were excluded in steps 9-11 (exc_*=3, 4, 5 or 6)
  #  i.  Determine median_tbc*sd and median_tbcOsd as in step 5.
  # ii.	 For each subject/parameter with extraneous and at least one non-extraneous value, select the value closest to the
  #      median_tbc*sd for inclusion in EWMA calculations.

  # This is functionally the same as re-doing the "temporary extraneous" step before doing the EWMA
  temp.dups <- temporary_extraneous(data.df)
  data.df[temp.dups, exclude := 'Exclude-Temporary-Extraneous-Same-Day']

  # prepare a list of valid rows and initialize variables for convenience
  valid.rows <- valid(data.df)
  data.df[, `:=`(
    ewma.all = as.double(NaN),
    abssum2 = as.double(NaN),
    median.other.sd = as.double(NaN),
    extraneous = FALSE
  )]

  # 12c.	Calculate a EWMA step for all subjects/parameters with duplicates/extraneous and at
  #       least one non-extraneous value with the following modifications
  #   i.	For calculating the EWMA, include only the extraneous selected in 12c
  #  ii.	Calculate dewma_* for all values of extraneous
  # iii.	You do not need to calculate EWMAbef or EWMAaft for this step

  # determine proportion of days with extraneous/duplication for each parameter ahead of time for efficiency
  dup.ratio.df <- data.df[subjid %in% subj.dup &
                            (valid.rows |
                               temp.dups), list(dup = (.N > 1)), by = .(subjid, param, agedays)][j = list(dup.ratio =
                                                                                                            mean(dup)), keyby = .(subjid, param)]
  # identify subject/parameters where there is duplication but at least one day with no extraneous for that parameter
  subj.param.not.all.dups <- dup.ratio.df[dup.ratio < 1.0, list(subjid, param)]
  # identify subject/parameters where there is duplication for all days for that parameter
  subj.param.all.dups <- dup.ratio.df[dup.ratio == 1, list(subjid, param)]
  subj.all.dups <- subj.param.all.dups[, unique(subjid)]
  # perform ewma for subjects with extraneous
  data.df[subjid %in% subj.dup &
            valid.rows, ewma.all := ewma(agedays, tbc.sd, ewma.exp, ewma.adjacent =
                                           FALSE), by = .(subjid, param)]
  # note: at this point, only one ewma.all exists per param on a given day for a subject, so sort(ewma.all)[1] will returns the non-missing ewma.all
  data.df[subjid %in% subj.dup, ewma.all := sort(ewma.all)[1], by = .(subjid, param, agedays)]

  # iv.  Calculate abssum2_*=|2*dewma_*|+|tbc*sd| (note that this is different from how we calculated abssum_* in step 11).
  # NOTE: only children with more than one ageday with valid measurements will have a valid ewma from above
  data.df[, abssum2 := 2 * abs(tbc.sd - ewma.all) + abs(tbc.sd)]

  # 12d.  For each subject/parameter/age with extraneous and at least one non-extraneous value:
  #   i.  Replace exc_*=7 for all values except the value that has the smallest abssum2_*.
  data.df[data.table(subj.param.not.all.dups), extraneous := seq_along(abssum2) != which.min(abssum2), by =
            .(subjid, param, agedays)]
  data.df[temp.dups, exclude := 'Include']
  data.df[(valid.rows |
             temp.dups) &
            extraneous, exclude := 'Exclude-Extraneous-Same-Day']
  #  ii.  Determine dup_tot_* (# of days with extraneous for that subject/parameter) and nodup_tot_* (# of days with nonexlcuded
  #       non-extraneous for that subject/parameter).
  # iii.  If dup_tot_*/(dup_tot_*+nodup_tot_*) is greater than 1/2, replace exc_*=7 for all extraneous for that subject/parameter
  #       for each age where the  largest measurement minus the smallest measurement for that subject/parameter/age is larger than
  #       the maximum difference (ht 3cm; wt 0-9.999 kg 0.25kg; wt 10-29.9999 kg 0.5 kg; wt 30kg and higher 1 kg).

  data.df[data.table(dup.ratio.df[dup.ratio > 1 / 2, list(subjid, param)]), exclude := (function(df) {
    df[, `:=`(tbc.sd.min = as.double(NaN),
              tbc.sd.max = as.double(NaN))]
    df[valid(
      exclude,
      include.extraneous = TRUE,
      include.temporary.extraneous = TRUE
    ), `:=`(tbc.sd.min = min(tbc.sd),
            tbc.sd.max = max(tbc.sd))]
    df[tbc.sd.max - tbc.sd.min > ifelse(param == 'HEIGHTCM',
                                        3,
                                        ifelse(
                                          param == 'WEIGHTKG',
                                          ifelse(tbc.sd.min < 10, 0.25, ifelse(tbc.sd.min < 30, 0.5, 1)),
                                          NA
                                        )),
       exclude := 'Exclude-Extraneous-Same-Day']
    return(df$exclude)
  })(copy(.SD)), .SDcols = c('exclude', 'tbc.sd'), by = .(subjid, param, agedays)]

  # 12e.	For each subject/parameter/age with extraneous and no nonextraneous values:
  #   i.	Replace exc_*=7 for all values except the value with the smallest |tbc*sd-median_tbcOsd|. If median_tbcOsd is missing because there are no values
  #       for the other parameter, randomly choose one extraneous value for each subject/parameter/age to keep as exc_*=0 and replace exc_*=7 for all other
  #       extraneous for that subject/parameter/age.
  #  ii.  If the largest measurement minus the smallest measurement for that subject/parameter/age is larger than the maximum difference
  #       (ht 3cm; wt 0-9.999 kg 0.25kg; wt 10-29.9999 kg 0.5 kg; wt 30kg and higher 1 kg)., replace exc_*=7 for all extraneous for that
  #       subject/parameter/age.

  # calculate median for other parameter (restrict to subjects with all duplication for at least one parameter)
  data.df[subjid %in% subj.all.dups, exclude := (function(subj.df) {
    # flag days that have extraneous / potentially valid parameters
    subj.df[, `:=`(
      extraneous.this.day = FALSE,
      extraneous = FALSE,
      tbc.sd.min = as.double(NaN),
      tbc.sd.max = as.double(NaN)
    )]
    valid.rows = valid(
      subj.df,
      include.extraneous = TRUE,
      include.temporary.extraneous = TRUE
    )
    subj.df[valid.rows, extraneous.this.day := (.N > 1), by = .(param, agedays)]
    for (p in subj.df[j = unique(param)]) {
      median.sd <- subj.df[param != p &
                             !extraneous.this.day, median(tbc.sd)]
      subj.df[param == p, median.other.sd := median.sd]
    }
    # safety check -- assign median.other.sd==0 to ensure "which.min" functions correctly below
    subj.df[is.na(median.other.sd), median.other.sd := 0]
    # identify rows as extraneous where |tbc*sd-median_tbcOsd| is not at the minimum value
    subj.df[extraneous.this.day == TRUE, extraneous := (seq_along(median.other.sd) != which.min(abs(tbc.sd - median.other.sd))), by =
              .(param, agedays)]
    subj.df[extraneous.this.day &
              !extraneous, exclude := 'Include']
    subj.df[extraneous.this.day &
              extraneous, exclude := 'Exclude-Extraneous-Same-Day']
    subj.df[extraneous.this.day == TRUE, `:=`(tbc.sd.min = min(tbc.sd),
                                              tbc.sd.max = max(tbc.sd)), by = .(param, agedays)]
    subj.df[tbc.sd.max - tbc.sd.min > ifelse(param == 'HEIGHTCM',
                                             3,
                                             ifelse(
                                               param == 'WEIGHTKG',
                                               ifelse(tbc.sd.min < 10, 0.25, ifelse(tbc.sd.min < 30, 0.5, 1)),
                                               NA
                                             )),
            exclude := 'Exclude-Extraneous-Same-Day']

    # identify kids who had an SD or EWMA extreme excluded that was a extraneous and re-label as "Exclude-Extraneous-Same-Day"
    subj.df[, extraneous.this.day := FALSE]
    # consider any non-missing measurement when determining presence of extraneous
    subj.df[exclude != 'Missing', extraneous.this.day := (.N > 1), by =
              .(param, agedays)]
    subj.df[extraneous.this.day &
              exclude %in% c('Exclude-SD-Cutoff',
                             'Exclude-EWMA-Extreme',
                             'Exclude-EWMA-Extreme-Pair'), exclude := 'Exclude-Extraneous-Same-Day']

    return(subj.df$exclude)
  })(copy(.SD)), .SDcols = c('param', 'agedays', 'exclude', 'tbc.sd'), by =
    .(subjid)]

  # 12f.  For any values that were excluded with exc_*=4, 5, or 6 that are also extraneous, replace exc_*=7.
  data.df[subjid %in% subj.dup, exclude := (function(subj.df) {
    if (.N > 1) {
      subj.df[exclude %in% c('Exclude-SD-Cutoff',
                             'Exclude-EWMA-Extreme',
                             'Exclude-EWMA-Extreme-Pair'), exclude := 'Exclude-Extraneous-Same-Day']
    }
    return(subj.df$exclude)
  })(copy(.SD)), .SDcols = c('exclude'), by = .(subjid, param, agedays)]

  # 13.  Calculate plus/minus measurements with allowable errors and corresponding recentered SD scores
  # a.	In order to help determine if a deviation from EWMA could be explained by an allowable degree of error, calculate the following:
  #    i.	wt_plus=wt+0.05*wt
  #    ii.	wt_minus=wt-0.05*wt
  #    iii.	ht_plus=ht+1
  #    iv.	ht_minus=ht-1
  # b.	Foreach of the above calculated and then recenter the SD score for the new value, generating tbc*sd_plus and tbc*sd_minus
  data.df[, delta := ifelse(param == 'WEIGHTKG', .05 * v, 1)]
  data.df[, `:=`(v.minus = v - delta, v.plus = v + delta)]
  data.df[, `:=`(
    tbc.sd.minus = measurement.to.z(param, agedays, sex, v.minus, TRUE),
    tbc.sd.plus = measurement.to.z(param, agedays, sex, v.plus, TRUE)
  )]

  # 14.  Exclude moderate errors based on EWMA
  # a.	This step is similar to step 11, with repeated exclusions of 1 value at a time, but with different criteria than step 11. There are several criteria used
  #     as checks to make sure that values that have a large dewma_* are truly not likely to be representative.
  # b.	Perform a EWMA calculation
  #   i.	In addition to standard dewma_* variables, calculate dewma_*_plus and dewma_*_minus using the tbc*sd scores generated in step 13
  if (!quietly)
    cat(sprintf("[%s] Exclude moderate errors based on EWMA...\n", Sys.time()))
  data.df[, exclude := (function(subj.df) {
    num.ewma.excluded <- 0
    while (TRUE) {
      valid.rows <- valid(subj.df)
      # initialize fields
      subj.df[, `:=`(
        ewma.all = as.double(NaN),
        ewma.before = as.double(NaN),
        ewma.after = as.double(NaN),
        tbc.sd.prev = as.double(NaN),
        tbc.sd.next = as.double(NaN),
        delta.agedays.prev = as.integer(NaN),
        delta.agedays.next = as.integer(NaN),
        abs.2ndlast.sd = as.double(NaN),
        tbc.other.sd = as.double(NaN)
      )]
      subj.df[valid.rows, (ewma.fields) := ewma(agedays, tbc.sd, ewma.exp, TRUE), by =
                param]
      subj.df[, `:=`(
        dewma.all = tbc.sd - ewma.all,
        dewma.before = tbc.sd - ewma.before,
        dewma.after = tbc.sd - ewma.after
      )]

      # 14c.	Calculate d_prevsd=tbc*sd-tbc*sdprev; d_prevsd_minus=tbc*sd_minus-tbc*sdprev; d_prevsd_plus=tbc*sd_plus-tbc*sdprev
      #     and d_nextsd=tbc*sd-tbc*sdnext; d_nextsd_minus=tbc*sd_minus-tbc*sdnext; d_nextsd_plus=tbc*sd_plus-tbc*sdnext

      # for efficiency, bring get.prev and get.next inline here (working on valid rows within a single parameter for a single subject)
      # structure c(NA, field.name[-.N]) == get.prev
      # structure c(field.name[-1], NA) == get.next
      subj.df[valid.rows, `:=`(tbc.sd.prev = c(NA, tbc.sd[-.N]),
                               tbc.sd.next = c(tbc.sd[-1], NA)), by = param]
      subj.df[, `:=`(
        dprev.sd = tbc.sd - tbc.sd.prev,
        dprev.sd.minus = tbc.sd.minus - tbc.sd.prev,
        dprev.sd.plus = tbc.sd.plus - tbc.sd.prev,
        dnext.sd = tbc.sd - tbc.sd.next,
        dnext.sd.minus = tbc.sd.minus - tbc.sd.next,
        dnext.sd.plus = tbc.sd.plus - tbc.sd.next
      )]

      # 14d.	Calculate d_agedays_prev=agedays-agedaysprev and d_agedays_next=agedaysnext-agedays
      subj.df[valid.rows, `:=`(
        delta.agedays.prev = as.integer(agedays - c(NA, agedays[-.N])),
        delta.agedays.next = as.integer(agedays - c(agedays[-1], NA))
      ), by = param]

      # 14e.	Generate abs_2ndlast_sd=|tbc*sd| for the second-to-last measurement for a subject/parameter
      # assign this value (if defined) to all agedays for that subject
      subj.df[valid.rows, abs.2ndlast.sd := ifelse(.N >= 2, abs(tbc.sd[.N -
                                                                         1]), as.double(NaN)), by = param]

      # 14f.	Calculate tbcOsd which is the tbc*sd for the OTHER parameter for the same subject and ageday  with exc_*==0 (this may be missing).
      # NOTE: move this simplified version of swap_parameters here for efficiency
      valid.other <- subj.df[valid.rows, list(
        param.other = ifelse(param == 'WEIGHTKG', 'HEIGHTCM', 'WEIGHTKG'),
        agedays.other = agedays,
        tbc.sd
      )]
      setkey(valid.other, param.other, agedays.other)
      subj.df[valid.rows, tbc.other.sd := valid.other[list(param, agedays), tbc.sd]]

      subj.df[, exclude := (function(df) {
        # 14g.  Calculate median_tbcOsd which is the median_tbc*sd for the OTHER parameter for the same subject with exc_*==0 (this may be missing)
        # also assign param variable for use in subsequent steps
        median.tbc.other.sd <- median(df$tbc.other.sd, na.rm = TRUE)
        num.valid <- sum(valid(df$exclude))

        # 14h.	Identify values for possible exclusion if they meet one of the following sets of criteria. Generate a temporary exclusion variable temp_exc_* equal
        #     to the number indicated to keep track of which set of criteria were met
        df$temp.exclude <-
          factor(NA, levels = exclude.levels, ordered = TRUE)

        # 14h.i.	Replace temp_exc_*=8 if the value is one of 3 or more measurements for a subject/parameter AND the value is neither the first nor the last measurement
        #       AND one of the following sets of criteria are met
        #     1.	dewma_*>1 & dewma_*_bef>1 & dewma_*_aft>1 & d_nextsd_*>1 & d_prevsd_*>1 & d_prevsd_plus_*>1 & d_prevsd_minus_*>1 & d_nextsd_plus_*>1 & d_nextsd_minus_*>1
        #     2.	dewma_*<-1 & dewma_*_bef<-1 & dewma_*_aft<-1 & d_nextsd_*<-1 & d_prevsd_*<-1 & d_prevsd_plus_*<-1 & d_prevsd_minus_*<-1 & d_nextsd_plus_*<-1 & d_nextsd_minus_*<-1

        # NOTE: NA as treated as false in context of data.table, which takes care of the NA values that result when
        # evaluating the boolean expression for a first or last value of a parameter for a child,
        # which ensures the flag is only set for measurements that have both a preceding and subsequent measurement
        df[dewma.all > 1 &
             dewma.before > 1 &
             dewma.after > 1 &
             dprev.sd > 1 &
             dprev.sd.plus > 1 &
             dprev.sd.minus > 1 &
             dnext.sd > 1 &
             dnext.sd.plus > 1 & dnext.sd.minus > 1
           |
             dewma.all < -1 &
             dewma.before < -1 &
             dewma.after < -1 &
             dprev.sd < -1 &
             dprev.sd.plus < -1 &
             dprev.sd.minus < -1 &
             dnext.sd < -1 &
             dnext.sd.plus < -1 & dnext.sd.minus < -1,
           temp.exclude := 'Exclude-EWMA-8']

        # 14h.ii.	Replace temp_exc_*=9 (mark for potential exclusion) if the value is the first of 3 or more measurements for a subject/parameter
        #      AND d_agedays_next< 365.25 AND one of the following sets of criteria are met
        #
        #     1.	dewma_*>2 & dewma_*_aft>1 & d_nextsd_*>1 & d_nextsd_plus_*>1 & d_nextsd_minus_*>1 & agedays>=30
        #     2.	dewma_*<-2 & dewma_*_aft<-1 & d_nextsd_*<-1 & d_nextsd_plus_*<-1 & d_nextsd_minus_*<-1 & agedays>=30
        #     3.  dewma_*>2.5 & dewma_*_aft>1 & d_nextsd_*>1 & d_nextsd_plus_*>1 & d_nextsd_minus_*>1 & agedays<30
        #     4.  dewma_*<-4 & dewma_*_aft<-1 & d_nextsd_*<-1 & d_nextsd_plus_*<-1 & d_nextsd_minus_*<-1 & agedays<30

        # take advantage of other variables we have calculated to infer that a row is the first of three or more valid measurements for a paramete
        df$first.of.three.or.more <- FALSE
        df$last.of.three.or.more <- FALSE
        df[is.na(delta.agedays.prev) &
             num.valid >= 3, first.of.three.or.more := TRUE]
        df[is.na(delta.agedays.next) &
             num.valid >= 3, last.of.three.or.more := TRUE]

        df[first.of.three.or.more & delta.agedays.next < 365.25
           &
             (
               dewma.all > 2 &
                 dewma.after > 1 &
                 dnext.sd > 1 &
                 dnext.sd.plus > 1 & dnext.sd.minus > 1 & agedays >= 30
               |
                 dewma.all < -2 &
                 dewma.after < -1 &
                 dnext.sd < -1 &
                 dnext.sd.plus < -1 & dnext.sd.minus < -1 & agedays >= 30
               |
                 dewma.all > 2.5 &
                 dewma.after > 1 &
                 dnext.sd > 1 &
                 dnext.sd.plus > 1 & dnext.sd.minus > 1 & agedays < 30
               |
                 dewma.all < -4 &
                 dewma.after < -1 &
                 dnext.sd < -1 &
                 dnext.sd.plus < -1 & dnext.sd.minus < -1 & agedays < 30
             ),
           temp.exclude := 'Exclude-EWMA-9']

        # 14h.iii.	Replace temp_exc_*=10 if the value is the first of 3 or more measurements for a subject/parameter AND d_agedays_next>365.25
        #         AND one of the following sets of criteria are met
        #     1.	dewma_*>3 & dewma_*_aft>1 & d_nextsd_*>1 & d_nextsd_plus_*>1 & d_nextsd_minus_*>1
        #     2.	dewma_*<-3 & dewma_*_aft<-1 & d_nextsd_*<-1 & d_nextsd_plus_*<-1 & d_nextsd_minus_*<-1

        # NOTE: changed age comparison to >= to ensure complete coverage of possible conditions in the event case fractional days are present in the dataset
        df[first.of.three.or.more & delta.agedays.next >= 365.25
           &
             (
               dewma.all > 3 &
                 dewma.after > 1 &
                 dnext.sd > 1 &
                 dnext.sd.plus > 1 & dnext.sd.minus > 1
               |
                 dewma.all < -3 &
                 dewma.after < -1 &
                 dnext.sd < -1 &
                 dnext.sd.plus < -1 & dnext.sd.minus < -1
             ),
           temp.exclude := 'Exclude-EWMA-10']

        # 14h.iv.	Replace temp_exc_*=11 if the value is the last of 3 or more measurements for a subject/parameter AND d_agedays_prev<730.5 AND abs_2ndlast_sd <2
        #       AND one of the following sets of criteria are met
        #     1.	dewma_*>2 & dewma_*_bef>1 & d_prevsd_*>1 & d_prevsd_plus_*>1 & d_prevsd_minus_*>1
        #     2.	dewma_*<-2 & dewma_*_bef<-1 & d_prevsd_*<-1 & d_prevsd_plus_*<-1 & d_prevsd_minus_*<-1
        df[last.of.three.or.more &
             delta.agedays.prev < 730.5 & abs.2ndlast.sd < 2
           &
             (
               dewma.all > 2 &
                 dewma.before > 1 &
                 dprev.sd > 1 &
                 dprev.sd.plus > 1 & dprev.sd.minus > 1
               |
                 dewma.all < -2 &
                 dewma.before < -1 &
                 dprev.sd < -1 &
                 dprev.sd.plus < -1 & dprev.sd.minus < -1
             ),
           temp.exclude := 'Exclude-EWMA-11']

        # 14h.v.	Replace temp_exc_*=12 if the value is the last of 3 or more measurements for a subject/parameter AND d_agedays_prev<730.5 AND abs_2ndlast_sd >=2
        #       AND one of the following sets of criteria are met
        #     1.	dewma_*>abs_2ndlast_sd & dewma_*_bef>1 & d_prevsd_*>1 & d_prevsd_plus_*>1 & d_prevsd_minus_*>1
        #     2.	dewma_*<(-1*abs_2ndlast_sd) & dewma_*_bef<-1 & d_prevsd_*<-1 & d_prevsd_plus_*<-1 & d_prevsd_minus_*<-1
        df[last.of.three.or.more &
             delta.agedays.prev < 730.5 & abs.2ndlast.sd >= 2
           &
             (
               dewma.all > abs.2ndlast.sd &
                 dewma.before > 1 &
                 dprev.sd > 1 &
                 dprev.sd.plus > 1 & dprev.sd.minus > 1
               |
                 dewma.all < -abs.2ndlast.sd &
                 dewma.before < -1 &
                 dprev.sd < -1 &
                 dprev.sd.plus < -1 & dprev.sd.minus < -1
             ),
           temp.exclude := 'Exclude-EWMA-12']

        # 14h.vi.	Replace temp_exc_*=13 if the value is the last of 3 or more measurements for a subject/parameter AND d_agedays_prev>730.5 AND  abs_2ndlast_sd <2
        #       AND one of the following sets of criteria are met
        #     1.	dewma_*>3 & dewma_*_bef>1 & d_prevsd_*>1 & d_prevsd_plus_*>1 & d_prevsd_minus_*>1 & ((tbc*sd-tbcOsd)>4
        #         OR ((tbc*sd-median_tbcOsd)>4 & tbcOsd is missing) OR median_tbcOsd is missing)
        #     2.	dewma_*<-3 & dewma_*_bef<-1 & d_prevsd_*<-1 & d_prevsd_plus_*<-1 & d_prevsd_minus_*<-1 & ((tbc*sd-tbcOsd)<-4
        #         OR ((tbc*sd-median_tbcOsd)<-4 & tbcOsd is missing) OR median_tbcOsd is missing)

        # NOTE: changed age comparison to >= to ensure complete coverage of possible conditions in the event case fractional days are present in the dataset
        df[last.of.three.or.more &
             delta.agedays.prev >= 730.5 & abs.2ndlast.sd < 2
           &
             (
               dewma.all > 3 &
                 dewma.before > 1 &
                 dprev.sd > 1 &
                 dprev.sd.plus > 1 & dprev.sd.minus > 1
               &
                 (
                   tbc.sd - tbc.other.sd > 4 |
                     is.na(tbc.other.sd) &
                     tbc.sd - median.tbc.other.sd > 4 |
                     is.na(median.tbc.other.sd)
                 )
               |
                 dewma.all < -3 &
                 dewma.before < -1 &
                 dprev.sd < -1 &
                 dprev.sd.plus < -1 & dprev.sd.minus < -1
               &
                 (
                   tbc.sd - tbc.other.sd < -4 |
                     is.na(tbc.other.sd) &
                     tbc.sd - median.tbc.other.sd < -4 |
                     is.na(median.tbc.other.sd)
                 )
             ),
           temp.exclude := 'Exclude-EWMA-13']

        # 14h.vii.	Replace temp_exc_*=14 if the value is the last of 3 or more measurements for a subject/parameter AND d_agedays_prev>730.5 AND  abs_2ndlast_sd>=2
        #         AND one of the following sets of criteria are met
        #     1.	dewma_*>(1+abs_2ndlast_sd) & dewma_*_bef>1 & d_prevsd_*>1 & d_prevsd_plus_*>1 & d_prevsd_minus_*>1 & ((tbc*sd-tbcOsd)>4
        #         OR ((tbc*sd-median_tbcOsd)>4 & tbcOsd is missing) OR median_tbcOsd is missing)
        #     2.	dewma_*<(-1-abs_2ndlast_sd) & dewma_*_bef<-1 & d_prevsd_*<-1 & d_prevsd_plus_*<-1 & d_prevsd_minus_*<-1 & ((tbc*sd-tbcOsd)<-4
        #         OR ((tbc*sd-median_tbcOsd)<-4 & tbcOsd is missing) OR median_tbcOsd is missing)
        df[last.of.three.or.more &
             delta.agedays.prev >= 730.5 & abs.2ndlast.sd >= 2
           &
             (
               dewma.all > 1 + abs.2ndlast.sd &
                 dewma.before > 1 &
                 dprev.sd > 1 &
                 dprev.sd.plus > 1 & dprev.sd.minus > 1
               &
                 (
                   tbc.sd - tbc.other.sd > 4 |
                     is.na(tbc.other.sd) &
                     tbc.sd - median.tbc.other.sd > 4 |
                     is.na(median.tbc.other.sd)
                 )
               |
                 dewma.all < -1 - abs.2ndlast.sd &
                 dewma.before < -1 &
                 dprev.sd < -1 &
                 dprev.sd.plus < -1 & dprev.sd.minus < -1
               &
                 (
                   tbc.sd - tbc.other.sd < -4 |
                     is.na(tbc.other.sd) &
                     tbc.sd - median.tbc.other.sd < -4 |
                     is.na(median.tbc.other.sd)
                 )
             ),
           temp.exclude := 'Exclude-EWMA-14']

        # 14i.	If there is only one potential exclusion identified in step 14h for a subject and parameter, replace exc_*=temp_exc_* for that value
        rep <- !is.na(df$temp.exclude)
        num.exclude <- sum(rep)
        if (num.exclude == 1)
          df[rep, exclude := temp.exclude]

        # 14j.	If there is more than one potential exclusion identified in step 14h for a subject and parameter, calculate abssum_*=|tbc*sd+dewma_*| for each exclusion
        #     and replace exc_*=temp_exc_* for the value with the highest abssum_*
        if (num.exclude > 1) {
          # first order by decreasing abssum  (where rep=TRUE)
          worst.row <- with(df, order(rep, abs(tbc.sd + dewma.all), decreasing =
                                        TRUE))[1]
          df[worst.row, exclude := temp.exclude]
        }

        return(df$exclude)
      })(copy(.SD)), by = param]

      # k.	If there was at least one subject who had a potential exclusion identified in step 14h, repeat steps 14b-14j. If there were no subjects with potential
      #     exclusions identified in step 14h, move on to step 15.
      newly.excluded <- sum(subj.df$exclude >= 'Exclude-EWMA-8' &
                              subj.df$exclude <= 'Exclude-EWMA-14')
      if (newly.excluded > num.ewma.excluded) {
        num.ewma.excluded <- newly.excluded
      } else {
        break
      }
    }

    return(subj.df$exclude)
  })(copy(.SD)), by = subjid, .SDcols = c(
    'index',
    'sex',
    'param',
    'agedays',
    'v',
    'tbc.sd',
    'v.minus',
    'v.plus',
    'tbc.sd.minus',
    'tbc.sd.plus',
    'exclude'
  )]

  # 15.  Exclude heights based on absolute differences in measurement. The key to this step is that once we identify pairs of measurements with implausible
  #      amounts of absolute difference between them, we have to determine which of the two values in the pair is less likely to be representative and should
  #      be excluded. For subjects/parameters with 3 or more measurements, this is done by looking at the dewma_* of each of the 2 values in a pair using a ewma that
  #      excludes the other value in the pair. For subjects/parameters with 2 or more measurements, this is done by looking at the absolute value of the tbc*sd.
  #      The Tanner height velocity reference is used for measurements taken at >2yo, WHO will be used for <2yo. For a few pairs of measurements either could be used;
  #      WHO will be used if difference between ages is < 9 months.
  if (!quietly)
    cat(sprintf(
      "[%s] Exclude heights based on growth velocity...\n",
      Sys.time()
    ))
  data.df[param == 'HEIGHTCM', exclude := (function(subj.df) {
    # assign some book keeping variables
    #subj.df[, `:=`(subjid = subjid, param='HEIGHTCM',index=1:.N)]
    subj.df[, index := 1:.N]

    num.height.excluded <- 0
    while (TRUE) {
      # use a closure to discard all the extra fields added to df with each iteration
      subj.df[valid(exclude), exclude := (function (df) {
        # with each iteration we are operating only on valid rows to allow simplification of code below

        # initialize fields
        df[, (ewma.fields) := as.double(NaN)]
        df[, `:=`(
          v.prev = as.double(NaN),
          v.next = as.double(NaN),
          dewma.after.prev = as.double(NaN),
          dewma.before.next = as.double(NaN),
          abs.tbc.sd.prev = as.double(NaN),
          abs.tbc.sd.next = as.double(NaN),
          agedays.next = as.integer(NaN),
          abs.2ndlast.sd = as.double(NaN),
          mindiff.prev.ht = as.double(NaN),
          mindiff.next.ht = as.double(NaN),
          maxdiff.prev.ht = as.double(NaN),
          maxdiff.next.ht = as.double(NaN),
          pair.prev = FALSE,
          pair.next = FALSE
        )]

        # ewma fields are needed later -- calculate now for efficiency
        df[, (ewma.fields) := ewma(agedays, tbc.sd, ewma.exp, TRUE)]

        # calculate some usefule values (e.g. dewma values and tbc.sd) for use in later steps
        df[, `:=`(
          dewma.all = tbc.sd - ewma.all,
          dewma.before = tbc.sd - ewma.before,
          dewma.after = tbc.sd - ewma.after,
          abs.tbc.sd = abs(tbc.sd)
        )]

        # 15a.  As with steps 11 and 14, only one value will be excluded per round, and the step will be repeated until there are no more values to exclude
        # b.  For each height, calculate the d_age=agedays of next value-agedays of current value
        # NOTE: obtain next measurement, ewma.before and abs.tbc.sd as well since they are needed later

        # for efficiency, bring get.next inline here (working on valid rows within a single parameter for a single subject)
        # structure c(field.name[-1], NA) == get.next
        df[, `:=`(
          agedays.next = c(agedays[-1], NA),
          v.next = c(v[-1], NA),
          dewma.before.next = c(dewma.before[-1], NA),
          abs.tbc.sd.next = c(abs.tbc.sd[-1], NA)
        )]
        df$delta.agedays.next <- with(df, agedays.next - agedays)

        # 15c.	For each height, calculate mid_agedays=0.5*(agedays of next value + agedays of current value)
        df$mid.agedays <- 0.5 * (df$agedays.next + df$agedays)

        # 15d.	Generate variable tanner_months= 6+12*(round(mid_agedays/365.25))
        # only calculate for rows that relate to height (may speed up subsequent processing)
        df$tanner.months <-
          with(df, 6 + 12 * (round(mid.agedays / 365.25)))

        # 15e.	Merge with dataset tanner_ht_vel using sex and tanner_months – this will give you min_ht_vel and max_ht_vel
        setkey(df, sex, tanner.months)
        df <- tanner.ht.vel[df]

        # 15f.	Calculate the following:
        #   i.	mindiff_ht=0.5*min_ht_vel*(d_agedays/365.25)^2-3 if d_agedays<365.25
        #   ii.	replace mindiff_ht=0.5*min_ht_vel-3 if d_agedays>365.25
        df[, ht.exp := ifelse(delta.agedays.next < 365.25, 2, 0)]
        df[, `:=`(maxdiff.next.ht = as.double(NA),
                  mindiff.next.ht = as.double(NaN))]
        df[, mindiff.next.ht := 0.5 * min.ht.vel * (delta.agedays.next /
                                                      365.25) ^ ht.exp - 3]

        # 15f.iii.	maxdiff_ht=2*max_ht_vel*(d_agedays/365.25)^1.5+5.5 if d_agedays>365.25
        #   iv.	replace maxdiff_ht=2*max_ht_vel*(d_agedays/365.25)^0.33+5.5 if d_agedays<365.25
        df[, ht.exp := ifelse(delta.agedays.next < 365.25, 0.33, 1.5)]
        df[, maxdiff.next.ht := 2 * max.ht.vel * (delta.agedays.next /
                                                    365.25) ^ ht.exp + 5.5]

        # 15g.	Generate variable whoagegrp_ht=agedays/30.4375 rounded to the nearest integer
        df[, whoagegrp.ht := round(agedays / 30.4375)]

        # 15h.	Generate variable whoinc_age_ht based on values of d_agedays_ht according the the following table
        #   d_agedays_ht	whoinc_age_ht
        #   20-45	        1
        #   46-75	        2
        #   76-106	      3
        #   107-152	      4
        #   153-198	      6
        #   All others	  missing
        df[, whoinc.age.ht := ifelse(delta.agedays.next < 20 ,
                                     NA,
                                     ifelse(
                                       delta.agedays.next <= 45,
                                       1,
                                       ifelse(
                                         delta.agedays.next <= 75,
                                         2,
                                         ifelse(
                                           delta.agedays.next <= 106,
                                           3,
                                           ifelse(
                                             delta.agedays.next <= 152,
                                             4,
                                             ifelse(delta.agedays.next <= 198, 6, NA)
                                           )
                                         )
                                       )
                                     ))]

        # i.	Merge using sex and whoagegrp_ht using who_ht_vel_3sd and who_ht_maxvel_3sd; this will give you varaibles whoinc_i_ht and maxwhoinc_i_ht
        #     for various intervals where i is 1,2, 3,4, 6 and corresponds to whoinc_age_ht.
        setkey(df, sex, whoagegrp.ht)
        df <- who.ht.vel[df]

        # restore original sort order (ensures valid.rows variable applies to correct rows)
        setkey(df, index)

        # 15j.	Generate variable who_mindiff_ht=whoinc_i_ht according to the value if whoinc_age_ht; make who_mindiff_ht missing if whoinc_i_ht or whoinc_age_ht is missing.
        df[, who.mindiff.next.ht := ifelse(
          delta.agedays.next < 20 ,
          NA,
          ifelse(
            delta.agedays.next <= 45,
            whoinc.1.ht,
            ifelse(
              delta.agedays.next <= 75,
              whoinc.2.ht,
              ifelse(
                delta.agedays.next <= 106,
                whoinc.3.ht,
                ifelse(
                  delta.agedays.next <= 152,
                  whoinc.4.ht,
                  ifelse(delta.agedays.next <= 198, whoinc.6.ht, NA)
                )
              )
            )
          )
        )]

        # 15k.	Generate variable who_maxdiff_ht=max_whoinc_i_ht according to the value if whoinc_age_ht; make who_maxdiff_ht missing if max_whoinc_i_ht or
        #     whoinc_age_ht is missing.
        df[, who.maxdiff.next.ht := ifelse(
          delta.agedays.next < 20 ,
          NA,
          ifelse(
            delta.agedays.next <= 45,
            max.whoinc.1.ht,
            ifelse(
              delta.agedays.next <= 75,
              max.whoinc.2.ht,
              ifelse(
                delta.agedays.next <= 106,
                max.whoinc.3.ht,
                ifelse(
                  delta.agedays.next <= 152,
                  max.whoinc.4.ht,
                  ifelse(delta.agedays.next <= 198, max.whoinc.6.ht, NA)
                )
              )
            )
          )
        )]

        # 15l.	Scale allowed value based on d_agedays_ht:
        #   1.	replace who_mindiff_`p'=who_mindiff_`p'*d_agedays_`p'/(whoinc_age_`p'*30.4375) if d_agedays_`p'<(whoinc_age_`p'*30.4375)
        #   2.	replace who_maxdiff_`p'=who_maxdiff_`p'*d_agedays_`p'/(whoinc_age_`p'*30.4375) if d_agedays_`p'>(whoinc_age_`p'*30.4375)
        df[delta.agedays.next < whoinc.age.ht * 30.4375,
           `:=`(
             who.mindiff.next.ht = who.mindiff.next.ht * delta.agedays.next / (whoinc.age.ht *
                                                                                 30.4375),
             who.maxdiff.next.ht = who.maxdiff.next.ht * delta.agedays.next /
               (whoinc.age.ht * 30.4375)
           )]

        # 15m.	Replace mindiff_ht/maxdiff_ht with adjusted WHO value if Tanner value is missing or if both are present and age difference is < 9 months:
        #   1.	replace mindiff_`p'=0.5*who_mindiff_`p'-3 if who_mindiff_`p' is not missing & d_agedays_`p'<(9*30.4375)
        #   2.	replace maxdiff_`p'=2*who_maxdiff_`p'+3 if who_maxdiff_`p' is not missing & d_agedays_`p'<(9*30.4375)
        #   3.	replace mindiff_`p'=0.5*who_mindiff_`p'-3 if mindiff_`p' is missing & who_mindiff_`p' is not missing
        #   4.	replace maxdiff_`p'=2*who_maxdiff_`p'+3 if maxdiff_`p is missing & who_maxdiff_`p' is not missing

        # refactored logic slightly for efficiency
        df[!is.na(who.mindiff.next.ht) &
             (delta.agedays.next < 9 * 30.4375 |
                is.na(mindiff.next.ht)),
           `:=`(
             mindiff.next.ht = 0.5 * who.mindiff.next.ht - 3,
             maxdiff.next.ht = 2.0 * who.maxdiff.next.ht + 3
           )]

        # 15m.5.  replace mindiff_`p'=-3 if mindiff_`p' is missing
        df[is.na(mindiff.next.ht), mindiff.next.ht := -3]

        # 15n.	Determine the min/maxdiffs for the previous age: mindiff_prev_ht, maxdiff_prev_ht
        # NOTE: obtain previous height, ewma.after value and abs.tbc.sd as well since they are needed in next steps

        # for efficiency, bring get.prev inline here (working on valid rows within a single parameter for a single subject)
        # structure c(NA, tbc.sd[-.N]) == get.prev
        df[, `:=`(
          v.prev = c(NA, v[-.N]),
          dewma.after.prev = c(NA, dewma.after[-.N]),
          abs.tbc.sd.prev = c(NA, abs.tbc.sd[-.N]),
          mindiff.prev.ht = c(NA, mindiff.next.ht[-.N]),
          maxdiff.prev.ht = c(NA, maxdiff.next.ht[-.N])
        )]

        # 15o.	Determine d_prev_ht=ht-htprev (set to missing for the first value for a subject) and d_next_ht=htnext-ht (set to missing for the last value for a subject)
        df[, `:=`(delta.prev.ht = v - v.prev,
                  delta.next.ht = v.next - v)]

        # 15p.  Perform a EWMA calculation with the following modifications:
        #  i.	  Generate a variable pair=1 if (d_prev_ht<mindiff_prev_ht OR d_ht<mindiff_ht OR d_prev_ht>maxdiff_prev_ht  OR d_ht>maxdiff_ht) AND exc_ht==0
        df[, pair := na_as_false(
          delta.prev.ht < mindiff.prev.ht |
            delta.next.ht < mindiff.next.ht |
            delta.prev.ht > maxdiff.prev.ht |
            delta.next.ht > maxdiff.next.ht
        )]

        # for efficiency, bring get.prev and get.next inline here (working on valid rows within a single parameter for a single subject)
        # structure c(NA, field.name[-.N]) == get.prev
        # structure c(field.name[-1], NA) == get.next
        df[, `:=`(pair.prev = c(FALSE, pair[-.N]),
                  pair.next = c(pair[-1], FALSE))]

        #  ii.	Generate bef_g_aftm1=1 if |Δewma_htbef| for the value of interest is greater than |Δewma_htaft| for the previous value
        #       AND the value of interest is not the first height value for that subject AND pair==1 AND pair for the previous value==1

        #  iii.	Generate aft_g_befp1=1 if |Δewma_htaft| for the value of interest is greater than |Δewma_htbef| for the next value
        #       AND the value of interest is not the last height value for that subject AND pair==1 AND pair for the next value==1
        # NOTE: pair.next will be NA last height, which will result in a FALSE value below
        df[, `:=`(
          bef.g.aftm1 = na_as_false(
            abs(dewma.before) > abs(dewma.after.prev)  & pair & pair.prev
          ),
          aft.g.befp1 = na_as_false(
            abs(dewma.after)  > abs(dewma.before.next) & pair & pair.next
          )
        )]

        #  iv.	Determine tbchtsd for each value as well as the one before prev_tbchtsd and after next_tbchtsd it
        # NOTE: done previously for efficiency

        # 15p.v.  Determine the total number of ht values for each subject (tot_ht)
        # NOTE: all rows are valid due to constraint in subj.df[...] statement
        num.valid <- .N

        # 15q.	Identify a value for possible exclusion if one of the following sets of criteria are met. For values identified by each set of criteria determine
        #       the value of temp_diff using the formula given
        #   i.	d_prev_ht<mindiff_prev_ht & bef_g_aftm1_ht==1 & exc_ht==0 & mindiff_prev_ht is not missing
        #     a.  (temp_diff=|dewma_ht_bef|)
        df[, temp.diff := as.double(NaN)]
        df$temp.exclude <-
          factor(NA, levels = exclude.levels, ordered = TRUE)
        df[delta.prev.ht < mindiff.prev.ht & bef.g.aftm1,
           `:=`(temp.diff = abs(dewma.before),
                temp.exclude = 'Exclude-Min-Height-Change')]

        #   ii.	d_ht<mindiff_ht & aft_g_befp1_ht==1 & exc_ht==0 & mindiff_ht is not missing
        #     a.	(temp_diff=|dewma_ht_aft|)
        df[delta.next.ht < mindiff.next.ht & aft.g.befp1,
           `:=`(temp.diff = abs(dewma.after),
                temp.exclude = 'Exclude-Min-Height-Change')]

        #   iii.	d_prev_ht>maxdiff_prev_ht & bef_g_aftm1_ht==1 & exc_ht==0 & mindiff_prev_ht is not missing
        #     a.  (temp_diff=|dewma_ht_bef|)
        df[delta.prev.ht > maxdiff.prev.ht & bef.g.aftm1,
           `:=`(temp.diff = abs(dewma.before),
                temp.exclude = 'Exclude-Max-Height-Change')]

        #   iv.	d_ht>maxdiff_ht & aft_g_befp1_ht==1 & exc_ht==0 & mindiff_ht is not missing
        #     a.  (temp_diff=|dewma_ht_aft|)
        df[delta.next.ht > maxdiff.next.ht & aft.g.befp1,
           `:=`(temp.diff = abs(dewma.after),
                temp.exclude = 'Exclude-Max-Height-Change')]

        #   v.	d_prev_ht<mindiff_prev_ht & tot_ht==2 & |tbchtsd|>|prev_tbchtsd|
        #     a. for v-viii temp_diff is kept as missing
        #   vi. d_ht<mindiff_ht & tot_ht==2 & |tbchtsd|>|next_tbchtsd|
        df[delta.prev.ht < mindiff.prev.ht &
             num.valid == 2 & abs.tbc.sd > abs.tbc.sd.prev
           |
             delta.next.ht < mindiff.next.ht &
             num.valid == 2 & abs.tbc.sd > abs.tbc.sd.next,
           temp.exclude := 'Exclude-Min-Height-Change']

        #   vii.	d_prev_ht>maxdiff_prev_ht & tot_ht==2 & |tbchtsd|>|prev_tbchtsd|
        #   viii. d_ht>maxdiff_ht & tot_ht==2 & |tbchtsd|>|next_tbchtsd|
        df[delta.prev.ht > maxdiff.prev.ht &
             num.valid == 2 & abs.tbc.sd > abs.tbc.sd.prev
           |
             delta.next.ht > maxdiff.next.ht &
             num.valid == 2 & abs.tbc.sd > abs.tbc.sd.next,
           temp.exclude := 'Exclude-Max-Height-Change']

        # r.  If there is only one potential exclusion identified in step 15j for a subject and parameter,
        #     replace exc_ht=15 for that value if it met criteria i, ii, v, or vi  and exc_ht=16 if it met criteria iii, iv, vii, or viii
        # NOTE: these exclusions are assigned in the code above as 'Exclude-Min-Height-Change' and 'Exclude-Max-Height-Change'
        rep <- !is.na(df$temp.exclude)
        num.exclude <- sum(rep)
        if (num.exclude == 1)
          df[rep, exclude := temp.exclude]

        # s.  If there is more than one potential exclusion identified in step 14h for a subject and parameter, determine which value has the largest temp_diff and
        #     replace exc_ht=15 for that value if it met criteria i, ii, v, or vi and exc_ht=16 for that value if it met criteria iii,  iv, vii, or viii.

        if (num.exclude > 1) {
          # first order by decreasing temp.diff (where rep=TRUE)
          worst.row <- order(rep, df$temp.diff, decreasing = TRUE)[1]
          df[worst.row, exclude := temp.exclude]
        }

        return(df$exclude)
      })(copy(.SD))]

      # t.  If there was at least one subject who had a potential exclusion identified in step 15q, repeat steps 15b-15q. If there were no subjects with potential
      #     exclusions identified in step 15q, move on to step 16.
      newly.excluded <- sum(
        subj.df$exclude %in% c(
          'Exclude-Min-Height-Change',
          'Exclude-Max-Height-Change'
        )
      )
      if (newly.excluded > num.height.excluded) {
        num.height.excluded <- newly.excluded
      } else {
        break
      }
    }

    setkey(subj.df, index)
    return(subj.df$exclude)
  })(copy(.SD)), by = .(subjid), .SDcols = c('sex', 'agedays', 'v', 'tbc.sd', 'exclude')]

  # 16.  Exclude measurements for subjects/parameters with only 1 or 2 measurements with exc_*=0. This step uses a variety of criteria,
  #      including the tbc*sd of the other parameter.
  if (!quietly)
    cat(sprintf(
      "[%s] Exclude single measurements and pairs...\n",
      Sys.time()
    ))
  data.df$tbc.other.sd <- swap_parameters(df = data.df)
  data.df[, exclude := (function(df) {
    valid.rows <- which(valid(df))
    # calculate median SD for other parameter (used in steps below)
    median.tbc.other.sd <- median(df$tbc.other.sd)
    # a.  Identify subjects/parameters with only 2 values with exc_*==0, and determine the following:
    #   1.  d_tbc*sd_other=the absolute value difference in tbc*sd
    #   2.	d_agedays_other=the difference in agedays between the 2 values
    #   3.	tbcOsd=the tbc*sd for the other parameter on the same ageday
    # NOTE: this is done as the first step below for efficiency
    #   4.	median_tbcOsd= the median tbc*sd for the other parameter
    #   5.	abs_d_*_O is the absolute value of the difference between tbc*sd and tbcOsd if tbcOsd is not missing;
    #       it is the absolute value of the difference between tbc*sd and median_tbcOsd if tbcOsd is missing but median_tbcOsd is not missing;
    #       and is missing if tbcOsd and median_tbcOsd are both missing
    if (length(valid.rows) == 2) {
      delta.tbc.sd.other <- abs(df$tbc.sd[valid.rows[1]] - df$tbc.sd[valid.rows[2]])
      delta.agedays.other <- abs(df$agedays[valid.rows[1]] - df$agedays[valid.rows[2]])
      abs.delta.other <- with(df, ifelse(
        !is.na(tbc.other.sd),
        abs(tbc.sd - tbc.other.sd),
        ifelse(
          !is.na(median.tbc.other.sd),
          abs(tbc.sd - median.tbc.other.sd),
          NA
        )
      ))

      # b.	For subjects/parameters with 2 values with exc_*==0, replace exc_*=17 if one of the following sets of criteria are met:
      #   1.	If |d_agedays_other|>365.25 and |d_tbc*sd_other|>3; replace exc_*=17 for the value of the pair that has the largest abs_d_*_O. If abs_d_*O is missing, replace exc_*=17 for the value of the pair with the higher |tbc*sd|
      #   2.	If |d_agedays_other|<365.25 and |d_tbc*sd_other|>2; replace exc_*=18 for the value of the pair that has the largest abs_d_*_O. If abs_d_*O is missing, replace exc_*=18 for the value of the pair with the higher |tbc*sd|
      #
      # MODE default
      #   1.  uses current behavior outlined in b, in else statement
      #
      # MODE flag.both
      #   1.  similar to b, except in cases where only HTs values exist or only WTs values exist, both are dropped instead of only 1 (first if)
      #
      #
      # check whether each value in double has other parameters (make sure they don't first)
      if (lt3.exclude.mode == "flag.both" &&
          (is.na(df$tbc.other.sd[valid.rows[1]])) &&
          (is.na(df$tbc.other.sd[valid.rows[2]])) &&
          is.na(abs.delta.other)) {
        # if they don't then do test
        # check default thresholds, if they fail then exclude both regardless of tbc.sd
        if (na_as_false(delta.agedays.other >= 365.25 &
                        delta.tbc.sd.other > 3)) {
          df$exclude[valid.rows[1]] = 'Exclude-Pair-Delta-19'
          df$exclude[valid.rows[2]] = 'Exclude-Pair-Delta-19'
        } else if (na_as_false(delta.agedays.other < 365.25 &
                               delta.tbc.sd.other > 2)) {
          df$exclude[valid.rows[1]] = 'Exclude-Pair-Delta-19'
          df$exclude[valid.rows[2]] = 'Exclude-Pair-Delta-19'
        }

      } else {
        # default method if other mode is not used or if other parameters exist
        worst.row <- order(valid(df),
                           abs(df$tbc.sd),
                           abs(abs.delta.other),
                           decreasing = TRUE)[1]
        if (na_as_false(delta.agedays.other >= 365.25 &
                        delta.tbc.sd.other > 3)) {
          df$exclude[worst.row] <- 'Exclude-Pair-Delta-17'
        } else if (na_as_false(delta.agedays.other < 365.25 &
                               delta.tbc.sd.other > 2)) {
          df$exclude[worst.row] <- 'Exclude-Pair-Delta-18'
        }
      }
    }
    # c.	Identify subjects/parameters with exactly 1 value for which exc_*=0. This will include subjects/parameters for which a value was excluded in step 16b.
    #     Determine tbcOsd and median_tbcOsd as described in step 16a3 and 16a4 above.
    # d.	For subjects/parameters with 1 value for which exc_*=0,  replace exc_*=19 if one of the following sets of criteria are met:
    #   1.	|tbc*sd|>3 & |tbc*sd-tbcOsd|>5 & tbcOsd is not missing
    #   2.	|tbc*sd|>3 & |tbc*sd-median_tbcOsd|>5 & tbcOsd is missing & median_tbcOsd is not missing
    #   3.	|tbc*sd|>5 & tbcOsd is missing & median_tbcOsd is missing
    valid.rows <- valid(df)
    # NOTE: valid.rows is now a boolean.  was a row number in code above
    if (sum(valid.rows) == 1) {
      #cat("param = ", df[valid.rows, param])
      df[valid.rows &
           (
             abs(tbc.sd) > 3 &
               abs(
                 tbc.sd - ifelse(
                   !is.na(tbc.other.sd),
                   tbc.other.sd,
                   median.tbc.other.sd
                 )
               ) > 5 |
               abs(tbc.sd) > 5 &
               is.na(tbc.other.sd) & is.na(median.tbc.other.sd)
           ),
         exclude := 'Exclude-Single-Outlier']
    }

    # ensure factor levels didn't accidentally get mangled
    return(factor(df$exclude, levels = exclude.levels, ordered = TRUE))

  })(copy(.SD)), by = .(subjid, param), .SDcols = c('agedays', 'tbc.sd', 'tbc.other.sd', 'exclude', 'param')] #added param here for debugging, i think it's dropped otherwise

  # 17.  Exclude measurements based on error load for the subject
  # a.	For each subject/parameter determine the following:
  #   1.	tot_exc_*=the total number of values for which exc_* is equal to 4, 5, 6, or 8-19
  #   2.	tot_inc_*=the total number of values for which exc_*=0
  # b.	For subjects/parameters where tot_exc_*>0.5 x tot_inc_*; replace all values where exc_* had been 0 to exc_*=20
  # c.	For subjects/parameters where tot_exc_*>tot_inc_*, replace all values for the OTHER parameter where exc_* had been 0 to exc_*=21
  #
  # CD e-mail 2/10/15: It looks like there are two problems. One is carried forward measurements. The other is that I forgot to include an
  # important part of the rule in the English - tot_exc_* has to be >=2. I'm forwarding an updated English version.
  # Also, because your exclusions are handled a little differently I wanted to specify that unit errors and swaps are not included
  # in tot_exc_* and are included in tot_inc_*, whereas carried forwards and extraneous are not included in either count.
  # NOTE: updated to include optional argument "include.carryforward=TRUE" in the valid() function, and added the
  # constraints "exclude.count.this.parameter >= 2" and "exclude.count.other.parameter >= 2" in the code below

  if (!quietly)
    cat(
      sprintf(
        "[%s] Exclude all measurements if maximum threshold of errors is exceeded...\n",
        Sys.time()
      )
    )
  # NOTE: restrict analysis to non-missing rows
  data.df[exclude != 'Missing', exclude := (function(subj.df) {
    inc.exc <- subj.df[, j = list(exclude.count = sum(
      !valid(
        exclude,
        include.extraneous = TRUE,
        include.carryforward = TRUE
      )
    ),
    include.count = sum(valid(exclude))), keyby = param]
    for (p in unique(subj.df$param)) {
      exclude.count.this.parameter <- inc.exc[param == p, exclude.count]
      exclude.count.other.parameter <- sum(inc.exc[param != p, exclude.count])
      if (exclude.count.this.parameter > error.load.threshold * inc.exc[param ==
                                                                        p, include.count] &
          exclude.count.this.parameter >= error.load.mincount) {
        subj.df[param == p &
                  valid(exclude), exclude := 'Exclude-Too-Many-Errors']
      } else if (exclude.count.other.parameter > sum(inc.exc[param != p, include.count]) &
                 exclude.count.other.parameter >= error.load.mincount) {
        subj.df[param == p &
                  valid(exclude), exclude := 'Exclude-Too-Many-Errors-Other-Parameter']
      }
    }
    # ensure factor levels didn't accidentally get mangled
    return(factor(subj.df$exclude, levels = exclude.levels, ordered =
                    TRUE))
  })(copy(.SD)), by = subjid, .SDcols = c('param', 'exclude')]

  if (!quietly)
    cat(sprintf("[%s] Completed Batch #%s...\n", Sys.time(), data.df$batch[1]))
  if (!quietly & parallel)
    sink()
  return(data.df[j = .(line, exclude, tbc.sd, tbc.other.sd, param)]) #debugging
}
