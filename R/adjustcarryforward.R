### Define the helper function and the carryforward adjustment function ###
# Segments labeled with ADJUSTCF EDIT in comments are areas where substantive
# changes were made to the original cleangrowth logic. There are other changes
# as well that are not specifically called out.

# helper function to treat NA values as FALSE
na.as.false = function(v) {
  v[is.na(v)] = FALSE
  v
}

# function to calculate temporary exclusion for step 15 (a - q)
calc_temp_exclusion_15 <- function(
  df,
  ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
  ewma.exp, minfactor, maxfactor, banddiff, banddiff_plus,
  min_ht.exp_under, min_ht.exp_over, max_ht.exp_under, max_ht.exp_over){

  # avoid "no visible binding" warnings
  abs.tbc.sd <- abs.tbc.sd.next <- abs.tbc.sd.prev <- aft.g.befp1 <- NULL
  agedays <- agedays.next <- bef.g.aftm1 <- delta.agedays.next <- NULL
  delta.next.ht <- delta.prev.ht <- dewma.after <- dewma.after.prev <- NULL
  dewma.before <- dewma.before.next <- ewma.after <- ewma.all <- ewma.before <- NULL
  ht.exp <- index <- max.ht.vel <- max.whoinc.1.ht <- max.whoinc.2.ht <- max.whoinc.3.ht <- NULL
  max.whoinc.4.ht <- max.whoinc.6.ht <- maxdiff.next.ht <- maxdiff.prev.ht <- NULL
  mid.agedays <- min.ht.vel <- mindiff.next.ht <- mindiff.prev.ht <- pair <- NULL
  pair.next <- pair.prev <- sex <- tanner.months <- tbc.sd <- temp.diff <- temp.exclude <- NULL
  v <- v.next <- v.prev <- who.maxdiff.next.ht <- who.mindiff.next.ht <- whoagegrp.ht <- NULL
  whoinc.1.ht <- whoinc.2.ht <- whoinc.3.ht <- whoinc.4.ht <- whoinc.6.ht <- NULL
  whoinc.age.ht <- NULL


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

  # calculate some useful values (e.g. dewma values and tbc.sd) for use in later steps
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
  df[, delta.agedays.next := agedays.next - agedays]

  # 15c.	For each height, calculate mid_agedays=0.5*(agedays of next value + agedays of current value)
  df[, mid.agedays := 0.5 * (agedays.next + agedays)]

  # 15d.	Generate variable tanner_months= 6+12*(round(mid_agedays/365.25))
  # only calculate for rows that relate to height (may speed up subsequent processing)
  df[, tanner.months := 6 + 12 * (round(mid.agedays / 365.25))]

  # 15e.	Merge with dataset tanner_ht_vel using sex and tanner_months – this will give you min_ht_vel and max_ht_vel
  setkey(df, sex, tanner.months)
  df = tanner.ht.vel[df]

  # 15f.	Calculate the following:
  #   i.	mindiff_ht=0.5*min_ht_vel*(d_agedays/365.25)^2-3 if d_agedays<365.25
  #   ii.	replace mindiff_ht=0.5*min_ht_vel-3 if d_agedays>365.25
  df[, ht.exp := ifelse(delta.agedays.next < 365.25,
                        min_ht.exp_under,
                        min_ht.exp_over)]
  df[, `:=`(maxdiff.next.ht = as.double(NA),
            mindiff.next.ht = as.double(NaN))]
  df[, mindiff.next.ht := minfactor * min.ht.vel * (delta.agedays.next /
                                                      365.25) ^ ht.exp - banddiff]

  # 15f.iii.	maxdiff_ht=2*max_ht_vel*(d_agedays/365.25)^1.5+5.5 if d_agedays>365.25
  #   iv.	replace maxdiff_ht=2*max_ht_vel*(d_agedays/365.25)^0.33+5.5 if d_agedays<365.25
  df[, ht.exp := ifelse(delta.agedays.next < 365.25,
                        max_ht.exp_under,
                        max_ht.exp_over)]
  df[, maxdiff.next.ht := maxfactor * max.ht.vel * (delta.agedays.next /
                                                      365.25) ^ ht.exp + banddiff_plus]

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
  df[, whoinc.age.ht :=
       ifelse(delta.agedays.next < 20 ,
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
  df = who.ht.vel[df]

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
       mindiff.next.ht = minfactor * who.mindiff.next.ht - banddiff,
       maxdiff.next.ht = maxfactor * who.maxdiff.next.ht + banddiff
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
  df[, pair := na.as.false(
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
    bef.g.aftm1 = na.as.false(
      abs(dewma.before) > abs(dewma.after.prev)  & pair & pair.prev
    ),
    aft.g.befp1 = na.as.false(
      abs(dewma.after)  > abs(dewma.before.next) & pair & pair.next
    )
  )]

  #  iv.	Determine tbchtsd for each value as well as the one before prev_tbchtsd and after next_tbchtsd it
  # NOTE: done previously for efficiency

  # 15p.v.  Determine the total number of ht values for each subject (tot_ht)
  # NOTE: all rows are valid due to constraint in subj.df[...] statement
  num.valid = .N

  # 15q.	Identify a value for possible exclusion if one of the following sets of criteria are met. For values identified by each set of criteria determine
  #       the value of temp_diff using the formula given
  #   i.	d_prev_ht<mindiff_prev_ht & bef_g_aftm1_ht==1 & exc_ht==0 & mindiff_prev_ht is not missing
  #     a.  (temp_diff=|dewma_ht_bef|)
  df[, temp.diff := as.double(NaN)]
  df[, temp.exclude := factor(NA, levels = exclude.levels, ordered =
                                TRUE)]
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

  return(df)
}

# function to check every carried forward in a string one by one
check_cf_string <- function(
  eval_df, wh_exclude,
  ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
  ewma.exp, minfactor, maxfactor, banddiff, banddiff_plus,
  min_ht.exp_under, min_ht.exp_over, max_ht.exp_under, max_ht.exp_over){

  # find the next include OR the row end
  next_incl <- which(
    eval_df$orig.exclude[(wh_exclude+1):nrow(eval_df)] == "Include"
  )[1]+wh_exclude
  if (is.na(next_incl)){
    next_incl <- nrow(eval_df)+1
  }

  # find the include this is based on
  first_incl <- which(
    eval_df$orig.exclude[1:wh_exclude] == "Include"
  )
  first_incl <- first_incl[length(first_incl)]

  # now we want to test all of the ones in the string
  verdict <- "Include"
  cf_ind <- first_incl+1
  while(verdict == "Include" & cf_ind < next_incl){
    # build the dataframe for evaluation
    sub_df <- copy(
      if (next_incl <= nrow(eval_df)){
        eval_df[c(first_incl, cf_ind, next_incl),]
      } else {
        eval_df[c(first_incl, cf_ind),]
      }
    )

    eval_sub <- calc_temp_exclusion_15(
      copy(sub_df),
      ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
      ewma.exp, minfactor, maxfactor, banddiff, banddiff_plus,
      min_ht.exp_under, min_ht.exp_over, max_ht.exp_under, max_ht.exp_over)

    verdict <-
      if (is.na(eval_sub$temp.exclude[2])){
        "Include"
      } else {
        "Exclude"
      }

    cf_ind <- cf_ind + 1
  }

  return(list("verdict" = verdict,
              "cf_ind" = cf_ind,
              "next_incl" = next_incl,
              "first_incl" = first_incl))
}

# function to calculate step 15 as in the original algorithm (no parameters)
# eval type: definitely "exclude" or "include"
calc_step_15_no_param <- function(
  df,
  eval_type = "exclude",
  ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
  ewma.exp){

  # avoid "no visible binding for global variable" warnings
  agedays <- tbc.sd <- ewma.all <- ewma.before <- ewma.after <- v <- dewma.before <- NULL
  abs.tbc.sd <- sex <- tanner.months <- ht.exp <- delta.agedays.next <- mindiff.next.ht <- NULL
  min.ht.vel <- maxdiff.next.ht <- max.ht.vel <- minhtvel.exp <- min.ht.vel.2sd <- NULL
  max.ht.vel.2sd <- whoagegrp.ht <- whoinc.age.ht <- index <- who.mindiff.next.ht <- NULL
  whoinc.1.ht <- whoinc.2.ht <- whoinc.3.ht <- whoinc.4.ht <- whoinc.6.ht <- NULL
  who.maxdiff.next.ht <- max.whoinc.1.ht <- max.whoinc.2.ht <- max.whoinc.3.ht <- NULL
  max.whoinc.4.ht <- max.whoinc.6.ht <- dewma.after <- v.prev <- v.next <- pair <- NULL
  delta.prev.ht <- mindiff.prev.ht <- delta.next.ht <- maxdiff.prev.ht <- NULL
  dewma.after.prev <- pair.prev <- dewma.before.next <- pair.next <- temp.diff <- NULL
  bef.g.aftm1 <- aft.g.befp1 <- abs.tbc.sd.prev <- abs.tbc.sd.next <- temp.exclude <- NULL

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

  if (eval_type == "exclude"){
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
  } else { # we're evaluating ones we want to keep
    # 15f.	Calculate the following:
    #   i.	mindiff_ht=0.5*min_ht_vel*(d_agedays/365.25)^2-3 if d_agedays<365.25
    #   ii.	replace mindiff_ht=0.5*min_ht_vel-3 if d_agedays>365.25
    df[, ht.exp := ifelse(delta.agedays.next < 365.25, 1, 0)]
    df[, minhtvel.exp := ifelse(delta.agedays.next < 365.25, 0, 1)]
    df[, `:=`(maxdiff.next.ht = as.double(NA),
              mindiff.next.ht = as.double(NaN))]
    df[, mindiff.next.ht := min.ht.vel.2sd * (min.ht.vel) ^ minhtvel.exp *
         (delta.agedays.next /365.25) ^ ht.exp]

    # 15f.iii.	maxdiff_ht=2*max_ht_vel*(d_agedays/365.25)^1.5+5.5 if d_agedays>365.25
    #   iv.	replace maxdiff_ht=2*max_ht_vel*(d_agedays/365.25)^0.33+5.5 if d_agedays<365.25
    df[, maxdiff.next.ht := max.ht.vel.2sd * (delta.agedays.next / 365.25)]
  }

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

  if (eval_type == "exclude"){
    # refactored logic slightly for efficiency
    df[!is.na(who.mindiff.next.ht) &
         (delta.agedays.next < 9 * 30.4375 |
            is.na(mindiff.next.ht)),
       `:=`(
         mindiff.next.ht = 0.5 * who.mindiff.next.ht - 3,
         maxdiff.next.ht = 2.0 * who.maxdiff.next.ht + 3
       )]
  } else { # we're evaluating if we want to keep or not
    # should only be used if age is less than 24 months
    # refactored logic slightly for efficiency
    df[!is.na(who.mindiff.next.ht) &
         agedays < 24 * 30.4375 &
         (delta.agedays.next < 9 * 30.4375 |
            is.na(mindiff.next.ht)),
       `:=`(
         mindiff.next.ht = who.mindiff.next.ht,
         maxdiff.next.ht = who.maxdiff.next.ht
       )]
  }

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

  return(df$temp.exclude)
}

#' Answers for adjustcarryforward
#'
#' Determines what should absolutely be reincluded or definitely excluded
#' for a given dataset, already run through \code{cleangrowth}.
#'
#' @param subjid Vector of unique identifiers for each subject in the database.
#' @param param Vector identifying each measurement, may be 'WEIGHTKG', 'HEIGHTCM', or 'LENGTHCM'
#'   'HEIGHTCM' vs. 'LENGTHCM' only affects z-score calculations between ages 24 to 35 months (730 to 1095 days).
#'   All linear measurements below 731 days of life (age 0-23 months) are interpreted as supine length, and
#'   all linear measurements above 1095 days of life (age 36+ months) are interpreted as standing height.
#'   Note: at the moment, all LENGTHCM will be converted to HEIGHTCM. In the future, the algorithm will be updated to consider this difference.
#' @param agedays Numeric vector containing the age in days at each measurement.
#' @param sex Vector identifying the gender of the subject, may be 'M', 'm', or 0 for males, vs. 'F',
#'  'f' or 1 for females.
#' @param measurement Numeric vector containing the actual measurement data.  Weight must be in
#'   kilograms (kg), and linear measurements (height vs. length) in centimeters (cm).
#' @param orig.exclude Vector of exclusion assessment results from cleangrowth()
#' @param sd.recenter Data frame or table with median SD-scores per day of life
#' @param ewma.exp Exponent to use for weighting measurements in the exponentially weighted moving
#'  average calculations. Defaults to -1.5. This exponent should be negative in order to weight growth
#'  measurements closer to the measurement being evaluated more strongly. Exponents that are further from
#'  zero (e.g. -3) will increase the relative influence of measurements close in time to the measurement
#'  being evaluated compared to using the default exponent.
#' @param ref.data.path Path to reference data. If not supplied, the year 2000
#' Centers for Disease Control (CDC) reference data will be used.
#' @param quietly Determines if function messages are to be displayed and if log files (parallel only)
#' are to be generated. Defaults to TRUE.
#'
#' @return A data frame, containing an index "n" of rows, corresponding to the
#' original order of the input vectors, and "acf_answers", containing the answers
#' on whether a height value should be kept or excluded (returns "Definitely
#' Exclude", "Definitely Include", or "Unknown" for height values, NA for weight
#' values).
#'
#' @export
#' @rawNamespace import(plyr, except = c(failwith, id, summarize, count, desc, mutate, arrange, rename, is.discrete, summarise, summarize))
#' @rawNamespace import(dplyr, except = c(last, first, summarize, src, between))
#' @import data.table
#' @rawNamespace import(R.utils, except = c(extract))
# NOTE: no examples, since this is a temporary function
acf_answers <- function(subjid,
                        param,
                        agedays,
                        sex,
                        measurement,
                        orig.exclude,
                        sd.recenter = NA,
                        ewma.exp = -1.5,
                        ref.data.path = "",
                        quietly = TRUE){

  # avoid "no visible binding for global variable" warnings
  tanner.months <- whoagegrp_ht <- whoagegrp.ht <- z.orig <- z.orig <- v <- sd.orig <- NULL
  index <- exclude <- tbc.sd <- sd.median <- acf_answer <- NULL

  # process ----
  # organize data into a dataframe along with a line "index" so the original data order can be recovered
  data.all <- data.table(
    line = seq_along(measurement),
    subjid = as.factor(subjid),
    param,
    agedays = as.integer(agedays),
    v = ifelse(measurement == 0, NaN, measurement),
    sex = as.integer(ifelse(
      sex %in% c(0, 'm', 'M'), 0, ifelse(sex %in% c(1, 'f', 'F'), 1, NA)
    )),
    orig.exclude = as.factor(orig.exclude)
  )

  ### ADJUSTCF EDIT
  data.all <- data.all[, n := 1:.N]
  ### ENDEDIT

  data.orig <- data.all

  setkey(data.all, subjid)

  subjid.unique <- unique(data.all$subjid)

  # tanner/who 3 SD ----

  # load tanner height velocity data. sex variable is defined such that 0=male and 1=female
  # recode column names to match syntactic style ("." rather than "_" in variable names)
  tanner_ht_vel_path <- ifelse(
    ref.data.path == "",
    system.file("extdata/tanner_ht_vel.csv.gz", package = "growthcleanr"),
    paste(ref.data.path, "tanner_ht_vel.csv.gz", sep =
            "")
  )
  tanner.ht.vel <- fread(tanner_ht_vel_path)

  setnames(tanner.ht.vel,
           colnames(tanner.ht.vel),
           gsub('_', '.', colnames(tanner.ht.vel)))
  setkey(tanner.ht.vel, sex, tanner.months)
  # keep track of column names in the tanner data
  tanner.fields <- colnames(tanner.ht.vel)
  tanner.fields <- tanner.fields[!tanner.fields %in% c('sex', 'tanner.months')]

  who_max_ht_vel_path <- ifelse(
    ref.data.path == "",
    system.file("extdata/who_ht_maxvel_3sd.csv.gz", package = "growthcleanr"),
    paste(ref.data.path, "who_ht_maxvel_3sd.csv.gz", sep =
            "")
  )

  who_ht_vel_3sd_path <- ifelse(
    ref.data.path == "",
    system.file("extdata/who_ht_vel_3sd.csv.gz", package = "growthcleanr"),
    paste(ref.data.path, "who_ht_vel_3sd.csv.gz", sep =
            "")
  )
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

  # tanner/who 2 SD ----

  # load tanner height velocity data. sex variable is defined such that 0=male and 1=female
  # recode column names to match syntactic style ("." rather than "_" in variable names)
  tanner_ht_vel_2sd_path <- ifelse(
    ref.data.path == "",
    system.file("extdata/tanner_ht_vel_with_2sd.csv.gz", package = "growthcleanr"),
    paste(ref.data.path, "tanner_ht_vel_with_2sd.csv.gz", sep = "")
  )
  tanner.ht.vel.2sd <- fread(tanner_ht_vel_2sd_path)

  setnames(tanner.ht.vel.2sd,
           colnames(tanner.ht.vel.2sd),
           gsub('_', '.', colnames(tanner.ht.vel.2sd)))
  setkey(tanner.ht.vel.2sd, sex, tanner.months)
  # keep track of column names in the tanner data
  tanner.fields.2sd <- colnames(tanner.ht.vel.2sd)
  tanner.fields.2sd <-
    tanner.fields.2sd[!tanner.fields.2sd %in% c('sex', 'tanner.months')]

  who_max_ht_vel_2sd_path <- ifelse(
    ref.data.path == "",
    system.file("extdata/who_ht_maxvel_2sd.csv.gz", package = "growthcleanr"),
    paste(ref.data.path, "who_ht_maxvel_2sd.csv.gz", sep =
            "")
  )

  who_ht_vel_2sd_path <- ifelse(
    ref.data.path == "",
    system.file("extdata/who_ht_vel_2sd.csv.gz", package = "growthcleanr"),
    paste(ref.data.path, "who_ht_vel_2sd.csv.gz", sep =
            "")
  )
  who.max.ht.vel.2sd <- fread(who_max_ht_vel_2sd_path)
  who.ht.vel.2sd <- fread(who_ht_vel_2sd_path)
  setkey(who.max.ht.vel.2sd, sex, whoagegrp_ht)
  setkey(who.ht.vel.2sd, sex, whoagegrp_ht)
  who.ht.vel.2sd <-
    as.data.table(dplyr::full_join(who.ht.vel.2sd, who.max.ht.vel.2sd, by =
                                     c('sex', 'whoagegrp_ht')))

  setnames(who.ht.vel.2sd,
           colnames(who.ht.vel.2sd),
           gsub(
             ".2sd", "", # replace 2sd with nothing, for ease of use in calculations
             gsub('_', '.', colnames(who.ht.vel.2sd))
            ))
  setkey(who.ht.vel.2sd, sex, whoagegrp.ht)
  # keep track of column names in the who growth velocity data
  who.fields.2sd <- colnames(who.ht.vel.2sd)
  who.fields.2sd <- who.fields.2sd[!who.fields.2sd %in% c('sex', 'whoagegrp.ht')]

  # getting zscores ----

  # recategorize linear parameters as 'HEIGHTCM'
  # NOTE: this will be changed in future to consider this difference
  data.all[param == 'LENGTHCM', param := 'HEIGHTCM']

  # calculate z scores
  if (!quietly)
    cat(sprintf("[%s] Calculating z-scores...\n", Sys.time()))
  measurement.to.z <- read_anthro(ref.data.path, cdc.only = TRUE)
  data.all[, z.orig := measurement.to.z(param, agedays, sex, v)]

  # calculate "standard deviation" scores
  if (!quietly)
    cat(sprintf("[%s] Calculating SD-scores...\n", Sys.time()))
  data.all[, sd.orig := measurement.to.z(param, agedays, sex, v, TRUE)]

  # sort by subjid, param, agedays
  setkey(data.all, subjid, param, agedays)

  # add a new convenience index for bookkeeping
  data.all[, index := 1:.N]

  # enumerate the different exclusion levels
  exclude.levels <- c(
    'Missing',
    'No Change',
    'Include',
    'Exclude-Min-Height-Change',
    'Exclude-Max-Height-Change'
  )

  # Mark missing values for exclusion
  data.all[, exclude := factor(with(data.all, ifelse(
    is.na(v) |
      agedays < 0, 'Missing', 'No Change'
  )),
  levels = exclude.levels,
  ordered = TRUE)] # why is this ordered??

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
  # returns a data table indexed by param, sex, agedays
  if (!is.data.table(sd.recenter)) {
    # ADJUSTCF EDIT - be explicit about levels to keep
    keep.levels <- c(
      "Include",
      "Unit-Error-High",
      "Unit-Error-Low",
      "Unit-Error-Possible",
      "Swapped-Measurements"
    )
    sd.recenter <- data.all[orig.exclude %in% keep.levels, sd_median(param, sex, agedays, sd.orig)]
    # END EDIT
  }
  # add sd.recenter to data, and recenter
  setkey(data.all, param, sex, agedays)
  data.all <- sd.recenter[data.all]
  setkey(data.all, subjid, param, agedays)
  data.all[, tbc.sd := sd.orig - sd.median]

  # safety check: treat observations where tbc.sd cannot be calculated as missing
  data.all[is.na(tbc.sd), exclude := 'Missing']

  # start evaluation ----

  if (!quietly)
    cat(sprintf("[%s] Calculating definitely exclude/include...\n", Sys.time()))

  data.all[param == 'HEIGHTCM', acf_answer := (function(subj.df) {
    # assign some book keeping variables
    #subj.df[, `:=`(subjid = subjid, param='HEIGHTCM',index=1:.N)]
    subj.df[, index := 1:.N]

    # use a closure to discard all the extra fields added to df with each iteration
    subj.df[,
      acf_answer := (function (df) {
        # calculate definitely exclude
        # do steps 15a - 15q (functionalized for ease)
        def_excl <- calc_step_15_no_param(
          copy(df),
          eval_type = "exclude",
          ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
          ewma.exp)

        def_incl <- calc_step_15_no_param(
          copy(df),
          eval_type = "include",
          ewma.fields, tanner.ht.vel.2sd, who.ht.vel.2sd, exclude.levels,
          ewma.exp)

        # calculate which should definitely be included and excluded
        verdict <- rep("Unknown", length(def_incl))
        verdict[!is.na(def_excl)] <- "Definitely Exclude"
        verdict[is.na(def_incl)] <- "Definitely Include"

        return(verdict)

      })(copy(.SD))]

    setkey(subj.df, index)
    return(subj.df$acf_answer)
  })(copy(.SD)), by = .(subjid), .SDcols = c('sex', 'agedays', 'v', 'tbc.sd', 'exclude', 'orig.exclude')]

  # formulating results
  acf_answer_df <- data.frame(n = data.all$n, acf_answer = data.all$acf_answer)
  # sort for user ease
  acf_answer_df <- acf_answer_df[order(acf_answer_df$n),]

  return(acf_answer_df)
}

#' adjustcarryforward
#' \code{adjustcarryforward} Uses absolute height velocity to identify values
#' excluded as carried forward values for reinclusion.
#' @param subjid Vector of unique identifiers for each subject in the database.
#' @param param Vector identifying each measurement, may be 'WEIGHTKG', 'HEIGHTCM', or 'LENGTHCM'
#'   'HEIGHTCM' vs. 'LENGTHCM' only affects z-score calculations between ages 24 to 35 months (730 to 1095 days).
#'   All linear measurements below 731 days of life (age 0-23 months) are interpreted as supine length, and
#'   all linear measurements above 1095 days of life (age 36+ months) are interpreted as standing height.
#'   Note: at the moment, all LENGTHCM will be converted to HEIGHTCM. In the future, the algorithm will be updated to consider this difference.
#' @param agedays Numeric vector containing the age in days at each measurement.
#' @param sex Vector identifying the gender of the subject, may be 'M', 'm', or 0 for males, vs. 'F',
#'  'f' or 1 for females.
#' @param measurement Numeric vector containing the actual measurement data.  Weight must be in
#'   kilograms (kg), and linear measurements (height vs. length) in centimeters (cm).
#' @param orig.exclude Vector of exclusion assessment results from cleangrowth()
#' @param exclude_opt Number from 0 to 3 indicating which option to use to handle strings of carried-forwards:
#'    0. no change.
#'    1. when deciding to exclude values, if we have a string of carried forwards,
#'    drop the most deviant value, and all CFs in the same string, and move on as
#'    normal.
#'    2. when deciding to exclude values, if the most deviant in a
#'    string of carried forwards is flagged, check all the CFs in that
#'    string from 1:N. Exclude all after the first that is flagged for
#'    exclusion when comparing to the Include before and after. Do not
#'    remove things designated as include.
#'    3. when deciding to exclude values, if the most deviant in a
#'    string of carried forwards is flagged, check all the CFs in that
#'    string from 1:N. Exclude all after the first that is flagged for
#'    exclusion when comparing to the Include before and after. Make sure
#'    remove things designated as include.
#' @param sd.recenter Data frame or table with median SD-scores per day of life
#' @param ewma.exp Exponent to use for weighting measurements in the exponentially weighted moving
#'  average calculations. Defaults to -1.5. This exponent should be negative in order to weight growth
#'  measurements closer to the measurement being evaluated more strongly. Exponents that are further from
#'  zero (e.g. -3) will increase the relative influence of measurements close in time to the measurement
#'  being evaluated compared to using the default exponent.
#' @param ref.data.path Path to reference data. If not supplied, the year 2000
#' Centers for Disease Control (CDC) reference data will be used.
#' @param quietly Determines if function messages are to be displayed and if log files (parallel only)
#' are to be generated. Defaults to TRUE.
#' @param minfactor Sweep variable for computing mindiff.next.ht in 15f, default 0.5
#' @param maxfactor Sweep variable for computing maxdiff.next.ht in 15f, default 2
#' @param banddiff Sweep variable for computing mindiff.next.ht in 15f, default 3
#' @param banddiff_plus Sweep variable for computing maxdiff.next.ht in 15, default 5.5
#' @param min_ht.exp_under Sweep variable for computing ht.exp in 15f, default 2
#' @param min_ht.exp_over Sweep variable for computing ht.exp in 15f, default 0
#' @param max_ht.exp_under Sweep variable for computing ht.exp in 15f, default 0.33
#' @param max_ht.exp_over Sweep variable for computing ht.exp in 15f, default 1.5
#'
#' @return Re-evaluated exclusion assessments based on height velocity.
#'
#' @export
#' @rawNamespace import(plyr, except = c(failwith, id, summarize, count, desc, mutate, arrange, rename, is.discrete, summarise, summarize))
#' @rawNamespace import(dplyr, except = c(last, first, summarize, src, between))
#' @import data.table
#' @examples
#' \donttest{
#' # Run on a small subset of given data
#' df <- as.data.frame(syngrowth)
#' df <- df[df$subjid %in% unique(df[, "subjid"])[1:2], ]
#' clean_df <- cbind(df,
#'                   "gcr_result" = cleangrowth(df$subjid,
#'                                              df$param,
#'                                              df$agedays,
#'                                              df$sex,
#'                                              df$measurement))
#'
#' # Adjust carry forward values in cleaned data
#' adj_clean <- adjustcarryforward(subjid = clean_df$subjid,
#'                                 param = clean_df$param,
#'                                 agedays = clean_df$agedays,
#'                                 sex = clean_df$sex,
#'                                 measurement = clean_df$measurement,
#'                                 orig.exclude = clean_df$gcr_result)
#' }
adjustcarryforward <- function(subjid,
                               param,
                               agedays,
                               sex,
                               measurement,
                               orig.exclude,
                               exclude_opt = 0,
                               sd.recenter = NA,
                               ewma.exp = -1.5,
                               ref.data.path = "",
                               quietly = TRUE,
                               minfactor = 0.5,
                               maxfactor = 2,
                               banddiff = 3,
                               banddiff_plus = 5.5,
                               min_ht.exp_under = 2,
                               min_ht.exp_over = 0,
                               max_ht.exp_under = 0.33,
                               max_ht.exp_over = 1.5) {

  # Avoid "undefined global functions/variables" warnings
  v <- sd.orig <- sd.median <- tbc.sd <- agedays.next <- mid.agedays <- min.ht.vel <- NULL
  delta.agedays.next <- ht.exp <- max.ht.vel <- mindiff.next.ht <- temp.exclude <- NULL
  ecf_tmp <- ewma.all <- ewma.before <- ewma.after <- abs.tbc.sd <- whoinc.1.ht <- NULL
  whoinc.2.ht <- whoinc.3.ht <- whoinc.4.ht <- whoinc.6.ht <- max.whoinc.1.ht <- NULL
  max.whoinc.2.ht <- max.whoinc.3.ht <- max.whoinc.4.ht <- max.whoinc.6.ht <- NULL
  whoinc.age.ht <- whoinc.age.ht <- who.mindiff.next.ht <- who.maxdiff.next.ht <- NULL
  dewma.after <- maxdiff.next.ht <- v.prev <- v.next <- delta.prev.ht <- NULL
  mindiff.prev.ht <- delta.next.ht <- maxdiff.prev.ht <- pair <- dewma.after.prev <- NULL
  pair.prev <- dewma.before.next <- pair.next <- bef.g.aftm1 <- aft.g.befp1 <- NULL
  abs.tbc.sd.prev <- abs.tbc.sd.next <- exclude <- n <- dewma.before <- NULL
  tanner.months <- whoagegrp_ht <- whoagegrp.ht <- z.orig <- index <- temp.diff <- NULL

  # check option is valid
  if (!exclude_opt %in% 0:3){
    stop("Invalid exclude_opt. Enter a number from 0 to 3.")
  }

  # organize data into a dataframe along with a line "index" so the original data order can be recovered
  data.all <- data.table(
    line = seq_along(measurement),
    subjid = as.factor(subjid),
    param,
    agedays = as.integer(agedays),
    v = ifelse(measurement == 0, NaN, measurement),
    sex = as.integer(ifelse(
      sex %in% c(0, 'm', 'M'), 0, ifelse(sex %in% c(1, 'f', 'F'), 1, NA)
    )),
    orig.exclude = as.factor(orig.exclude)
  )

  ### ADJUSTCF EDIT
  data.all <- data.all[, n := 1:.N]
  ### ENDEDIT

  data.orig <- data.all

  setkey(data.all, subjid)

  subjid.unique <- unique(data.all$subjid)

  #### ADJUSTCF EDIT ####
  # for this purpose, want to subset dataset down to just "Exclude-Carried-Forward" and "Include" - assume all other measurements are invalid
  # want to remove any carried forward values whose non-carried forward is also excluded
  # data.all <- data.all %>%
  #   mutate(orig.exclude.lag = lag(orig.exclude, n = 1)) %>%
  #   filter(!(
  #     orig.exclude == "Exclude-Carried-Forward" &
  #       grepl("exclude", orig.exclude.lag, ignore.case = TRUE)
  #   )) %>%
  #   filter(orig.exclude %in% c("Exclude-Carried-Forward", "Include")) %>%
  #   select(-orig.exclude.lag)

  # NEW EDIT --
  # remove all the weight measurements
  data.all <- data.all %>%
    filter(.data$param %in% c("HEIGHTCM", "LENGTHCM"))

  # filter to only subjects with possible carried forwards - n is here to merge back
  # if they have all includes, filter them out
  data.all <- data.all %>%
    filter(.data$subjid %in% data.all$subjid[data.all$orig.exclude == "Exclude-Carried-Forward"]) %>%
    as.data.table()

  # here's what we want to filter out -- anything that's not carried forward/include
  # we're also going to include strings of carried forward
  # but we also need to make sure they're not coming from an excluded value

  # start of string to remove: everything that isn't include/excl-cf
  st <- which(!data.all$orig.exclude %in% c("Exclude-Carried-Forward", "Include"))
  # end of string: include or the end of a subject
  subj_end <- length(data.all$subjid)-match(unique(data.all$subjid),rev(data.all$subjid))+1
  end <- c(which(data.all$orig.exclude == "Include"),subj_end)
  end <- unique(sort(end))

  # remove anything between start and ends (including start, not including end)
  to_rem <- unlist(
    lapply(st, function(x){
      to_rem <- c(x:(end[end >= x][1]))
      if (to_rem[length(to_rem)] %in% subj_end){
        # if it's the last value, we want to get rid of that end
        return(to_rem)
      } else {
        # if it's an include, we want to keep it (don't remove)
        return(to_rem[-length(to_rem)])
      }
    })
  )
  to_rem <- unique(to_rem)

  if (length(to_rem) > 0){
    data.all <- data.all[-to_rem,]
  }

  # filter to only subjects with possible carried forwards again
  data.all <- data.all %>%
    filter(.data$subjid %in% data.all$subjid[data.all$orig.exclude == "Exclude-Carried-Forward"]) %>%
    as.data.table()

  ### END EDIT ####

  # load tanner height velocity data. sex variable is defined such that 0=male and 1=female
  # recode column names to match syntactic style ("." rather than "_" in variable names)
  tanner_ht_vel_path <- ifelse(
    ref.data.path == "",
    system.file("extdata/tanner_ht_vel.csv.gz", package = "growthcleanr"),
    paste(ref.data.path, "tanner_ht_vel.csv.gz", sep = "")
  )

  tanner.ht.vel <- fread(tanner_ht_vel_path)

  setnames(tanner.ht.vel,
           colnames(tanner.ht.vel),
           gsub('_', '.', colnames(tanner.ht.vel)))
  setkey(tanner.ht.vel, sex, tanner.months)
  # keep track of column names in the tanner data
  tanner.fields <- colnames(tanner.ht.vel)
  tanner.fields <- tanner.fields[!tanner.fields %in% c('sex', 'tanner.months')]

  who_max_ht_vel_path <- ifelse(
    ref.data.path == "",
    system.file("extdata/who_ht_maxvel_3sd.csv.gz", package = "growthcleanr"),
    paste(ref.data.path, "who_ht_maxvel_3sd.csv.gz", sep =
            "")
  )

  who_ht_vel_3sd_path <- ifelse(
    ref.data.path == "",
    system.file("extdata/who_ht_vel_3sd.csv.gz", package = "growthcleanr"),
    paste(ref.data.path, "who_ht_vel_3sd.csv.gz", sep =
            "")
  )
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
  # b.	Remove rows that are duplicates for subjid, param, and measurement from further analysis
  #     NOTE: this step is not needed -- handled automatically by "temporary duplicate" step.
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

  # calculate z scores
  if (!quietly)
    cat(sprintf("[%s] Calculating z-scores...\n", Sys.time()))
  measurement.to.z <- read_anthro(ref.data.path, cdc.only = TRUE)
  data.all[, z.orig := measurement.to.z(param, agedays, sex, v)]

  # calculate "standard deviation" scores
  if (!quietly)
    cat(sprintf("[%s] Calculating SD-scores...\n", Sys.time()))
  data.all[, sd.orig := measurement.to.z(param, agedays, sex, v, TRUE)]

  # sort by subjid, param, agedays
  setkey(data.all, subjid, param, agedays)

  # add a new convenience index for bookkeeping
  data.all[, index := 1:.N]

  # enumerate the different exclusion levels
  exclude.levels <- c(
    'Missing',
    'No Change',
    'Include',
    'Exclude-Min-Height-Change',
    'Exclude-Max-Height-Change'
  )

  # Mark missing values for exclusion
  data.all[, exclude := factor(with(data.all, ifelse(
    is.na(v) |
      agedays < 0, 'Missing', 'No Change'
  )),
  levels = exclude.levels,
  ordered = TRUE)] # why is this ordered??

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
  # returns a data table indexed by param, sex, agedays
  if (!is.data.table(sd.recenter)) {
    # ADJUSTCF EDIT - be explicit about levels to keep
    keep.levels <- c(
      "Include",
      "Unit-Error-High",
      "Unit-Error-Low",
      "Unit-Error-Possible",
      "Swapped-Measurements"
    )
    sd.recenter <- data.all[orig.exclude %in% keep.levels, sd_median(param, sex, agedays, sd.orig)]
    # END EDIT
  }
  # add sd.recenter to data, and recenter
  setkey(data.all, param, sex, agedays)
  data.all <- sd.recenter[data.all]
  setkey(data.all, subjid, param, agedays)
  data.all[, tbc.sd := sd.orig - sd.median]

  # safety check: treat observations where tbc.sd cannot be calculated as missing
  data.all[is.na(tbc.sd), exclude := 'Missing']



  ######### END DATA PROCESING #########

  ######### START FLAGGING #########

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

  # ADJUSTCF EDIT:
  # there are several different method for handling carried forward values
  # 0. no change
  # 1. when deciding to exclude values, if we have a string of carried forwards,
  #    drop the most deviant value, and all CFs in the same string, and move on as
  #    normal
  # 2. when deciding to exclude values, if the most deviant in a
  # string of carried forwards is flagged, check all the CFs in that
  # string from 1:N. exclude all after the first that is flagged for
  # exclusion when comparing to the Include before and after. do not
  # remove things designated as include.
  # 3. when deciding to exclude values, if the most deviant in a
  # string of carried forwards is flagged, check all the CFs in that
  # string from 1:N. exclude all after the first that is flagged for
  # exclusion when comparing to the Include before and after. make sure
  # remove things designated as include.


  # for (opt in all_opts){
  data.all[param == 'HEIGHTCM', exclude := (function(subj.df) {
    # assign some book keeping variables
    #subj.df[, `:=`(subjid = subjid, param='HEIGHTCM',index=1:.N)]
    subj.df[, index := 1:.N]

    # keep the original subject dataframe, since we will always use the original
    # includes to compare to
    subj.df_orig <- copy(subj.df)
    num.height.excluded = 0
    while (TRUE) {
      # use a closure to discard all the extra fields added to df with each iteration
      # "include" protects subsets (used in option 3)
      subj.df[!grepl("Exclude", exclude) & !grepl("Include", exclude),
              exclude := (function (df) {
                # put together the dataframe with original includes for comparisons
                # get all "includes"
                subj.df_orig_incl <- copy(subj.df_orig[orig.exclude == "Include",])
                # now get all the remaining and add them on
                df <- rbind(subj.df_orig_incl,
                            df[orig.exclude != "Include",])
                # sort them by original index
                df <- df[order(index),]

                # do steps 15a - 15q (functionalized for ease)
                eval_df <- calc_temp_exclusion_15(
                  copy(df),
                  ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
                  ewma.exp, minfactor, maxfactor, banddiff, banddiff_plus,
                  min_ht.exp_under, min_ht.exp_over, max_ht.exp_under, max_ht.exp_over)

                # r.  If there is only one potential exclusion identified in step 15j for a subject and parameter,
                #     replace exc_ht=15 for that value if it met criteria i, ii, v, or vi  and exc_ht=16 if it met criteria iii, iv, vii, or viii
                # NOTE: these exclusions are assigned in the code above as 'Exclude-Min-Height-Change' and 'Exclude-Max-Height-Change'

                # remove includes from consideration of removal
                eval_df[orig.exclude == "Include", temp.diff := NaN]
                eval_df[orig.exclude == "Include", temp.exclude := NA]

                # count the amount of error codes we get
                all_exclude = !is.na(eval_df$temp.exclude)
                num.exclude = sum(all_exclude)

                # if there's only one, all of the groups agree
                # s.  If there is more than one potential exclusion identified in step 14h for a subject and parameter, determine which value has the largest temp_diff and
                #     replace exc_ht=15 for that value if it met criteria i, ii, v, or vi and exc_ht=16 for that value if it met criteria iii,  iv, vii, or viii.

                if (exclude_opt == 0){
                  # option 0: normal
                  if (num.exclude == 1){
                    eval_df[all_exclude, exclude := temp.exclude]
                  } else if (num.exclude > 1) {
                    # first order by decreasing temp.diff (where rep=TRUE)
                    worst.row = order(all_exclude, eval_df$temp.diff, decreasing = TRUE)[1]
                    eval_df[worst.row, exclude := temp.exclude]
                  }
                } else if (exclude_opt == 1){
                  # option 1: if there's a carried forward after, we want to exclude that too, until the next include
                  if (num.exclude == 1){
                    wh_exclude <- which(all_exclude)

                    # if it's in a string of carried forwards
                    if (
                      eval_df[wh_exclude, "orig.exclude"] == "Exclude-Carried-Forward" &&
                      wh_exclude != nrow(eval_df) &&
                      eval_df[wh_exclude+1, "orig.exclude"] == "Exclude-Carried-Forward"
                    ){
                      # find the next include OR the row end
                      next_incl <- which(
                        eval_df$orig.exclude[(wh_exclude+1):nrow(eval_df)] == "Include"
                      )[1]+wh_exclude
                      if (is.na(next_incl)){
                        next_incl <- nrow(eval_df)+1
                      }


                      # mark all the CFs after for exclusion
                      all_exclude[wh_exclude:(next_incl-1)] <- TRUE
                      # also copy all the temp.excludes
                      eval_df$temp.exclude[(wh_exclude+1):(next_incl-1)] <-
                        eval_df$temp.exclude[wh_exclude]
                    }

                    # exclude all the carried forwards before the next include
                    eval_df[all_exclude, exclude := temp.exclude]
                  } else if (num.exclude > 1) {
                    # first order by decreasing temp.diff (where rep=TRUE)
                    worst.row = order(all_exclude, eval_df$temp.diff, decreasing = TRUE)[1]

                    # option 1: if there's a carried forward after, we want to exclude that too, until the next include
                    wh_exclude <- worst.row
                    if (
                      eval_df[wh_exclude, "orig.exclude"] == "Exclude-Carried-Forward" &&
                      wh_exclude != nrow(eval_df) &&
                      eval_df[wh_exclude+1, "orig.exclude"] == "Exclude-Carried-Forward"
                    ){
                      # find the next include OR the row end
                      next_incl <- which(
                        eval_df$orig.exclude[(wh_exclude+1):nrow(eval_df)] == "Include"
                      )[1]+wh_exclude
                      if (is.na(next_incl)){
                        next_incl <- nrow(eval_df)+1
                      }

                      # mark all the CFs after for exclusion
                      worst.row <- c(worst.row:(next_incl-1))
                      # also copy all the temp.excludes
                      eval_df$temp.exclude[(wh_exclude+1):(next_incl-1)] <-
                        eval_df$temp.exclude[wh_exclude]
                    }

                    eval_df[worst.row, exclude := temp.exclude]
                  }
                } else if (exclude_opt == 2){
                  # 2. when deciding to exclude values, if the most deviant in a
                  # string of carried forwards is flagged, check all the CFs in that
                  # string from 1:N. exclude all after the first that is flagged for
                  # exclusion when comparing to the Include before and after. do not
                  # remove things designated as include.

                  # after you've evaluated them, do you reevaluate the ones you've kept?
                  if (num.exclude == 1){
                    wh_exclude <- which(all_exclude)

                    if (
                      eval_df[wh_exclude, "orig.exclude"] == "Exclude-Carried-Forward" &&
                      ((wh_exclude != nrow(eval_df) &&
                        eval_df[wh_exclude+1, "orig.exclude"] == "Exclude-Carried-Forward")
                       ||
                       (wh_exclude != 1 &&
                        eval_df[wh_exclude-1, "orig.exclude"] == "Exclude-Carried-Forward")
                      )
                    ){
                      # check all the carried forwards in a string
                      res <- check_cf_string(
                        copy(eval_df), wh_exclude,
                        ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
                        ewma.exp, minfactor, maxfactor, banddiff, banddiff_plus,
                        min_ht.exp_under, min_ht.exp_over, max_ht.exp_under, max_ht.exp_over
                      )

                      verdict <- res$verdict
                      cf_ind <- res$cf_ind
                      next_incl <- res$next_incl
                      first_incl <- res$first_incl

                      if (verdict == "Exclude"){
                        # mark all the CFs after for exclusion
                        all_exclude[(cf_ind-1):(next_incl-1)] <- TRUE
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(cf_ind-1):(next_incl-1)] <-
                          eval_df$temp.exclude[(cf_ind-1)]
                      } else {
                        # if the verdict is include, we need to mark them all for
                        # "exclusion" to remove -- otherwise we end up in a loop
                        all_exclude[(first_incl+1):(next_incl-1)] <- TRUE
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(first_incl+1):(next_incl-1)] <-
                          "Include"
                      }
                    }

                    # exclude all the carried forwards before the next include
                    eval_df[all_exclude, exclude := temp.exclude]
                  } else if (num.exclude > 1) {
                    # first order by decreasing temp.diff (where rep=TRUE)
                    worst.row = order(all_exclude, eval_df$temp.diff, decreasing = TRUE)[1]

                    wh_exclude <- worst.row
                    if (
                      eval_df[wh_exclude, "orig.exclude"] == "Exclude-Carried-Forward" &&
                      ((wh_exclude != nrow(eval_df) &&
                        eval_df[wh_exclude+1, "orig.exclude"] == "Exclude-Carried-Forward")
                       ||
                       (wh_exclude != 1 &&
                        eval_df[wh_exclude-1, "orig.exclude"] == "Exclude-Carried-Forward")
                      )
                    ){
                      # check all the carried forwards in a string
                      res <- check_cf_string(
                        copy(eval_df), wh_exclude,
                        ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
                        ewma.exp, minfactor, maxfactor, banddiff, banddiff_plus,
                        min_ht.exp_under, min_ht.exp_over, max_ht.exp_under, max_ht.exp_over
                      )

                      verdict <- res$verdict
                      cf_ind <- res$cf_ind
                      next_incl <- res$next_incl
                      first_incl <- res$first_incl

                      if (verdict == "Exclude"){
                        # mark all the CFs after for exclusion
                        worst.row <- c((cf_ind-1):(next_incl-1))
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(cf_ind-1):(next_incl-1)] <-
                          eval_df$temp.exclude[(cf_ind-1)]
                      } else {
                        # if the verdict is include, we need to mark them all for
                        # "exclusion" to remove -- otherwise we end up in a loop
                        worst.row <- c((first_incl+1):(next_incl-1))
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(first_incl+1):(next_incl-1)] <-
                          "Include"
                      }
                    }

                    eval_df[worst.row, exclude := temp.exclude]
                  }
                } else if (exclude_opt == 3){
                  # 3. when deciding to exclude values, if the most deviant in a
                  # string of carried forwards is flagged, check all the CFs in that
                  # string from 1:N. exclude all after the first that is flagged for
                  # exclusion when comparing to the Include before and after. make sure
                  # remove things designated as include.

                  # after you've evaluated them, do you reevaluate the ones you've kept?
                  if (num.exclude == 1){
                    wh_exclude <- which(all_exclude)
                    if (
                      eval_df[wh_exclude, "orig.exclude"] == "Exclude-Carried-Forward" &&
                      ((wh_exclude != nrow(eval_df) &&
                        eval_df[wh_exclude+1, "orig.exclude"] == "Exclude-Carried-Forward")
                       ||
                       (wh_exclude != 1 &&
                        eval_df[wh_exclude-1, "orig.exclude"] == "Exclude-Carried-Forward")
                      )
                    ){
                      # check all the carried forwards in a string
                      res <- check_cf_string(
                        copy(eval_df), wh_exclude,
                        ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
                        ewma.exp, minfactor, maxfactor, banddiff, banddiff_plus,
                        min_ht.exp_under, min_ht.exp_over, max_ht.exp_under, max_ht.exp_over
                      )

                      verdict <- res$verdict
                      cf_ind <- res$cf_ind
                      next_incl <- res$next_incl
                      first_incl <- res$first_incl

                      if (verdict == "Exclude"){
                        # mark all the CFs after for exclusion
                        all_exclude[(cf_ind-1):(next_incl-1)] <- TRUE
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(cf_ind-1):(next_incl-1)] <-
                          eval_df$temp.exclude[(cf_ind-1)]

                        # we also want to keep all the implicit excludes
                        # mark all the CFs after for "exclusion"
                        all_exclude[(first_incl+1):(cf_ind)] <- TRUE
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(first_incl+1):(cf_ind)] <-
                          "Include"
                      } else {
                        # if the verdict is include, we need to mark them all for
                        # "exclusion" to remove -- otherwise we end up in a loop
                        all_exclude[(first_incl+1):(next_incl-1)] <- TRUE
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(first_incl+1):(next_incl-1)] <-
                          "Include"
                      }
                    }

                    # exclude all the carried forwards before the next include
                    eval_df[all_exclude, exclude := temp.exclude]
                  } else if (num.exclude > 1) {
                    # first order by decreasing temp.diff (where rep=TRUE)
                    worst.row = order(all_exclude, eval_df$temp.diff, decreasing = TRUE)[1]

                    wh_exclude <- worst.row
                    if (
                      eval_df[wh_exclude, "orig.exclude"] == "Exclude-Carried-Forward" &&
                      ((wh_exclude != nrow(eval_df) &&
                        eval_df[wh_exclude+1, "orig.exclude"] == "Exclude-Carried-Forward")
                       ||
                       (wh_exclude != 1 &&
                        eval_df[wh_exclude-1, "orig.exclude"] == "Exclude-Carried-Forward")
                      )
                    ){
                      # check all the carried forwards in a string
                      res <- check_cf_string(
                        copy(eval_df), wh_exclude,
                        ewma.fields, tanner.ht.vel, who.ht.vel, exclude.levels,
                        ewma.exp, minfactor, maxfactor, banddiff, banddiff_plus,
                        min_ht.exp_under, min_ht.exp_over, max_ht.exp_under, max_ht.exp_over
                      )

                      verdict <- res$verdict
                      cf_ind <- res$cf_ind
                      next_incl <- res$next_incl
                      first_incl <- res$first_incl

                      if (verdict == "Exclude"){
                        # mark all the CFs after for exclusion
                        worst.row <- c((cf_ind-1):(next_incl-1))
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(cf_ind-1):(next_incl-1)] <-
                          eval_df$temp.exclude[(cf_ind-1)]

                        # we also want to keep all the implicit excludes
                        # mark all the CFs after for "exclusion"
                        worst.row <- c((first_incl+1):(cf_ind), worst.row)
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(first_incl+1):(cf_ind)] <-
                          "Include"
                      } else {
                        # if the verdict is include, we need to mark them all for
                        # "exclusion" to remove -- otherwise we end up in a loop
                        worst.row <- c((first_incl+1):(next_incl-1))
                        # also copy all the temp.excludes
                        eval_df$temp.exclude[(first_incl+1):(next_incl-1)] <-
                          "Include"
                      }
                    }

                    eval_df[worst.row, exclude := temp.exclude]
                  }
                }

                return(eval_df$exclude)

              })(copy(.SD))]

      # t.  If there was at least one subject who had a potential exclusion identified in step 15q, repeat steps 15b-15q. If there were no subjects with potential
      #     exclusions identified in step 15q, move on to step 16.
      newly.excluded = sum(
        subj.df$exclude %in% c(
          'Exclude-Min-Height-Change',
          'Exclude-Max-Height-Change'
        )
      )

      if (newly.excluded > num.height.excluded) {
        num.height.excluded = newly.excluded
      } else {
        break
      }
    }
    setkey(subj.df, index)
    return(subj.df$exclude)
  })(copy(.SD)), by = .(subjid), .SDcols = c('sex', 'agedays', 'v', 'tbc.sd', 'exclude', 'orig.exclude')]

  # process what came from step 15

  # if you're outside of the bands OR if you are not a carry forward then you have no change
  data.all[
    !(
      data.all[, exclude] %in% c(
        'Exclude-Min-Height-Change',
        'Exclude-Max-Height-Change'
      )
    ) &
      (data.all$orig.exclude == "Exclude-Carried-Forward"), exclude := "Include"]
  # everything with exclude should not be changed
  data.all[
    data.all[, exclude] %in% c('Exclude-Min-Height-Change','Exclude-Max-Height-Change'),
    exclude := "No Change"]
  # all original includes should not be changed
  data.all[
    data.all$orig.exclude != "Exclude-Carried-Forward",
    exclude := "No Change"
  ]

  # }

  # formulating results and options
  acf_df <- data.frame(n = data.all$n)
  acf_df <- cbind(acf_df,
                  data.all[, exclude])
  colnames(acf_df)[-1] <- "adjustcarryforward"

  # return results
  return(rbind(
    acf_df,
    data.frame(
      filter(data.orig, !.data$n %in% data.all$n) %>% mutate(adjustcarryforward = "Not Considered")  %>% select("adjustcarryforward", "n")
    )
  ))
}
