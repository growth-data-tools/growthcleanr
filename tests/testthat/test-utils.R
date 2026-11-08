testthat::skip_on_cran()
library(growthcleanr)

test_that("recode_sex works as expected with defaults", {
  # create a dataframe according to recode_sex defaults
  num_obs <- 20
  set.seed(7) # for replicability
  df <- data.frame(matrix(NA, nrow = num_obs, ncol = 1))
  colnames(df) <- "sex"
  df$sex <- sample(c("0", "1"), num_obs, replace = T)

  # run recode sex with all defaults
  r_df <- recode_sex(df)

  # check that column names are correct
  expect(all(colnames(r_df) %in% c("sex", "sex_recoded")),
         "column names incorrect")

  # all observations are accounted for
  expect_equal(df$sex, r_df$sex)

  # check that sex was recoded according to specifications
  expect_equal(r_df$sex_recoded[r_df$sex == "0"], rep(1, sum(df$sex == "0")))
  expect_equal(r_df$sex_recoded[r_df$sex == "1"], rep(2, sum(df$sex == "1")))
})

test_that("recode_sex works as expected with custom inputs", {
  # create a dataframe with customization
  num_obs <- 31
  set.seed(7) # for replicability
  df <- data.frame(matrix(NA, nrow = num_obs, ncol = 1))
  colnames(df) <- "sex_type"
  df$sex_type <- sample(c("M", "F"), num_obs, replace = T)

  # run recode sex with all defaults
  r_df <- recode_sex(
    df,
    sourcecol = "sex_type",
    sourcem = "M",
    sourcef = "F",
    targetcol = "sex_r",
    targetm = "m",
    targetf = "f"
  )

  # check that column names are correct
  expect(all(colnames(r_df) %in% c("sex_type", "sex_r")),
         "column names incorrect")

  # all observations are accounted for
  expect_equal(df$sex_type, r_df$sex_type)

  # check that sex was recoded according to specifications
  expect_equal(r_df$sex_r[r_df$sex_type == "M"],
               rep("m", sum(df$sex_type == "M")))
  expect_equal(r_df$sex_r[r_df$sex_type == "F"],
               rep("f", sum(df$sex_type == "F")))
})

test_that("longwide works as expected with default values", {
  # use synthetic data, running cleaning on a subset
  data("syngrowth")
  sub_syn <-
    syngrowth[syngrowth$subjid %in% unique(syngrowth$subjid)[1:100], ]
  # cleangrowth() returns a data.table; attach its `exclude` column
  # to sub_syn (longwide's default gcr_result = "exclude")
  gc_out <- cleangrowth(sub_syn)
  sub_syn$gcr_result <- as.character(gc_out$exclude)[match(sub_syn$id, gc_out$id)]

  # run longwide on changed data
  wide_syn <- longwide(sub_syn, gcr_result = "gcr_result")

  # check that it has the correct amount of columns
  expect_equal(ncol(wide_syn), 9)

  # check that all subjects are accounted for
  ss <- unique(sub_syn$subjid)
  ws <- unique(wide_syn$subjid)

  expect_equal(sort(ss), sort(ws))

  # check that all subjects' measurements with at least two occurrences appear;
  # works since cleangrowth() function will only identify at most one height and
  # one weight to include for given ageday
  all_obs <- sapply(unique(sub_syn$subjid), function(i) {
    sub_group <- sub_syn[sub_syn$gcr_result == "Include",]
    sum(table(sub_group$agedays[sub_group$subjid == i]) >= 2)
  })

  # it should be that the number of occurrences for each subject in "all_obs" is
  # the same as in "wide_syn", so check that but make sure sorted by subjid
  ws_subj_counts <- table(wide_syn$subjid)
  ws_subj_counts <- setNames(as.vector(ws_subj_counts), names(ws_subj_counts))

  expect_equal(all_obs[sort(names(all_obs))],
               ws_subj_counts[sort(names(ws_subj_counts))])

  # get all the observation ID's in the wide data
  obs_ids <- c(wide_syn$wt_id, wide_syn$ht_id)

  # here specified inclusion type is just "include"
  # check that it includes specified inclusion types
  expect(
    all(sub_syn$gcr_result[sub_syn$id %in% obs_ids] == "Include"),
    "longwide() includes inclusion values that were not specified"
  )

  # check that all sexes have been correctly recoded by picking out all the
  # unique subject ID and sex pairs from each data set, sorting them by ID, and
  # looking if the sex code in the new set is the M/F version of the old 0/1
  orig_sex <- sub_syn[!duplicated(sub_syn$subjid), c("subjid", "sex")]
  orig_sex <- orig_sex[order(orig_sex$subjid),]

  aft_sex <- wide_syn[!duplicated(wide_syn$subjid), c("subjid", "sex")]
  aft_sex <- aft_sex[order(aft_sex$subjid),]

  expect_equal(c("M", "F")[orig_sex$sex + 1], aft_sex$sex)

  # spot check that data is correct
  set.seed(7)
  # check height ids
  ht_sub <-
    sub_syn[sub_syn$param == "HEIGHTCM" & sub_syn$id %in% obs_ids, ]
  for (x in ht_sub$id[sample(1:nrow(ht_sub), 20)]) {
    w_idx <- wide_syn$ht_id == x
    ht_idx <- ht_sub$id == x

    # check ages
    expect_equal(wide_syn$agey[w_idx], round(ht_sub$agedays[ht_idx] / 365.25), 4)
    expect_equal(wide_syn$agem[w_idx],
                 round(round(ht_sub$agedays[ht_idx] / 365.25), 4) * 12, 4)
    expect_equal(wide_syn$agedays[w_idx], ht_sub$agedays[ht_idx])

    # check height
    expect_equal(wide_syn$ht[w_idx], ht_sub$measurement[ht_idx])

  }

  # check weight ids
  wt_sub <-
    sub_syn[sub_syn$param == "WEIGHTKG" & sub_syn$id %in% obs_ids, ]
  for (x in wt_sub$id[sample(1:nrow(wt_sub), 20)]) {
    w_idx <- wide_syn$wt_id == x
    wt_idx <- wt_sub$id == x

    # check ages
    expect_equal(wide_syn$agey[w_idx], round(wt_sub$agedays[wt_idx] / 365.25), 4)
    expect_equal(wide_syn$agem[w_idx],
                 round(round(wt_sub$agedays[wt_idx] / 365.25), 4) * 12, 4)
    expect_equal(wide_syn$agedays[w_idx], wt_sub$agedays[wt_idx])

    # check weight
    expect_equal(wide_syn$wt[w_idx], wt_sub$measurement[wt_idx])
  }
})

test_that("longwide works as expected with extra columns", {
  # use synthetic data, running cleaning on a subset
  data("syngrowth")
  sub_syn <-
    syngrowth[syngrowth$subjid %in% unique(syngrowth$subjid)[1:100], ]
  # cleangrowth() returns a data.table; attach its `exclude` column
  gc_out <- cleangrowth(sub_syn)
  sub_syn$gcr_result <- as.character(gc_out$exclude)[match(sub_syn$id, gc_out$id)]

  # add extra columns to sub_syn; one where all values match, and one where they
  # don't
  set.seed(7)
  sub_syn$r1 <- sample(c("A", "B", "C", "D"), size = nrow(sub_syn),
                       replace = TRUE)
  sub_syn$r2 <- "E"

  # run longwide on changed data
  wide_syn <- longwide(sub_syn, gcr_result = "gcr_result",
                       extra_cols = c("r1", "r2"))

  # check that it has the correct amount of columns
  expect_equal(ncol(wide_syn), ncol(sub_syn)+4)

  # check that additional columns are correctly named
  expect_equal(c("ht_r1", "wt_r1", "match_r1", "r2") %in%
                    colnames(wide_syn), rep(TRUE, 4))


  # run through same tests as default values

  # check that all subjects are accounted for
  ss <- unique(sub_syn$subjid)
  ws <- unique(wide_syn$subjid)

  expect_equal(sort(ss), sort(ws))

  # check that all subjects' measurements with at least two occurrences appear;
  # works since cleangrowth() function will only identify at most one height and
  # one weight to include for given ageday
  all_obs <- sapply(unique(sub_syn$subjid), function(i) {
    sub_group <- sub_syn[sub_syn$gcr_result == "Include",]
    sum(table(sub_group$agedays[sub_group$subjid == i]) >= 2)
  })

  # it should be that the number of occurrences for each subject in "all_obs" is
  # the same as in "wide_syn", so check that but make sure sorted by subjid
  ws_subj_counts <- table(wide_syn$subjid)
  ws_subj_counts <- setNames(as.vector(ws_subj_counts), names(ws_subj_counts))

  expect_equal(all_obs[sort(names(all_obs))],
               ws_subj_counts[sort(names(ws_subj_counts))])

  # get all the observation ID's in the wide data
  obs_ids <- c(wide_syn$wt_id, wide_syn$ht_id)

  # here specified inclusion type is just "include"
  # check that it includes specified inclusion types
  expect(
    all(sub_syn$gcr_result[sub_syn$id %in% obs_ids] == "Include"),
    "longwide() includes inclusion values that were not specified"
  )

  # check that all sexes have been correctly recoded
  orig_sex <- sub_syn[!duplicated(sub_syn$subjid), c("subjid", "sex")]
  orig_sex <- orig_sex[order(orig_sex$subjid),]

  aft_sex <- wide_syn[!duplicated(wide_syn$subjid), c("subjid", "sex")]
  aft_sex <- aft_sex[order(aft_sex$subjid),]

  expect_equal(c("M", "F")[orig_sex$sex + 1], aft_sex$sex)

  # spot check that data is correct; add in checks for additional columns
  set.seed(7)
  # check height ids
  ht_sub <-
    sub_syn[sub_syn$param == "HEIGHTCM" & sub_syn$id %in% obs_ids, ]
  for (x in ht_sub$id[sample(1:nrow(ht_sub), 20)]) {
    w_idx <- wide_syn$ht_id == x
    ht_idx <- ht_sub$id == x

    # check ages
    expect_equal(wide_syn$agey[w_idx], round(ht_sub$agedays[ht_idx] / 365.25), 4)
    expect_equal(wide_syn$agem[w_idx],
                 round(round(ht_sub$agedays[ht_idx] / 365.25), 4) * 12, 4)
    expect_equal(wide_syn$agedays[w_idx], ht_sub$agedays[ht_idx])

    # check height
    expect_equal(wide_syn$ht[w_idx], ht_sub$measurement[ht_idx])

    # check extra columns
    expect_equal(wide_syn$ht_r1[w_idx], ht_sub$r1[ht_idx])
    expect_equal(wide_syn$r2[w_idx], ht_sub$r2[ht_idx])
  }

  # check weight ids
  wt_sub <-
    sub_syn[sub_syn$param == "WEIGHTKG" & sub_syn$id %in% obs_ids, ]
  for (x in wt_sub$id[sample(1:nrow(wt_sub), 20)]) {
    w_idx <- wide_syn$wt_id == x
    wt_idx <- wt_sub$id == x

    # check ages
    expect_equal(wide_syn$agey[w_idx], round(wt_sub$agedays[wt_idx] / 365.25), 4)
    expect_equal(wide_syn$agem[w_idx],
                 round(round(wt_sub$agedays[wt_idx] / 365.25), 4) * 12, 4)
    expect_equal(wide_syn$agedays[w_idx], wt_sub$agedays[wt_idx])

    # check weight
    expect_equal(wide_syn$wt[w_idx], wt_sub$measurement[wt_idx])

    # check extra columns
    expect_equal(wide_syn$wt_r1[w_idx], wt_sub$r1[wt_idx])
    expect_equal(wide_syn$r2[w_idx], wt_sub$r2[wt_idx])
  }
})

test_that("longwide works as expected when not dropping unmatched values", {
  # use synthetic data, running cleaning on a subset
  data("syngrowth")
  sub_syn <-
    syngrowth[syngrowth$subjid %in% unique(syngrowth$subjid)[1:100], ]
  # cleangrowth() returns a data.table; attach its `exclude` column
  gc_out <- cleangrowth(sub_syn)
  sub_syn$gcr_result <- as.character(gc_out$exclude)[match(sub_syn$id, gc_out$id)]

  # to improve coverage add extra columns to sub_syn; one where all values
  # match, and one where they don't
  sub_syn$r1 <- sample(c("A", "B", "C", "D"), size = nrow(sub_syn),
                       replace = TRUE)
  sub_syn$r2 <- "E"

  # run longwide on changed data
  wide_syn <- longwide(sub_syn, gcr_result = "gcr_result",
                       extra_cols = c("r1", "r2"),
                       keep_unmatched_data = TRUE)

  # check for correct number of columns and correct naming; here r2 will not
  # be all matches since there are unmatched heights/weights
  expect_equal(ncol(wide_syn), ncol(sub_syn)+6)

  expect_equal(c("ht_r1", "wt_r1", "match_r1", "ht_r2", "wt_r2", "match_r2")
               %in% colnames(wide_syn), rep(TRUE, 6))

  # check that all measurements are in the data set by making sure all
  # observation ID's appear
  obs_ids <- c(wide_syn$wt_id, wide_syn$ht_id)

  # there will be NA's for the unmatched ID's, drop these to compare the lists
  obs_ids <- obs_ids[!is.na(obs_ids)]

  # pull the ID's from the long form data set and make sure it is the same list
  # as in the wide form (need to sort first)
  ss_ids <- sub_syn$id[sub_syn$gcr_result=="Include"]

  expect_equal(sort(obs_ids), sort(ss_ids))
})

test_that("longwide works as expected with other exclusion codes", {
  # just checking the custom-ness, so use a smaller subset for speed
  # use synthetic data, running cleaning on a subset
  data("syngrowth")
  sub_syn <-
    syngrowth[syngrowth$subjid %in% unique(syngrowth$subjid)[1:20], ]
  # cleangrowth() returns a data.table; attach its `exclude` as `cv`
  # to verify the custom gcr_result argument still works
  gc_out <- cleangrowth(sub_syn)
  sub_syn$cv <- as.character(gc_out$exclude)[match(sub_syn$id, gc_out$id)]

  # run longwide on changed data with all exclusion types included
  wide_syn <- longwide(sub_syn,
                       gcr_result = "cv",
                       include_all = TRUE)

  # check that it has the correct amount of columns
  expect_equal(ncol(wide_syn), 9)

  # check that all subjects are accounted for
  expect(all(unique(sub_syn$subjid) %in% unique(wide_syn$subjid)),
         "not all subjects appear in wide format")

  # quick test that a few particular height/weight ID's with an exclusion codes
  # besides "Include" appear in "wide_syn"
  expect(3 %in% wide_syn$ht_id, "missing height with ID 3 that should be
         included")
  expect(16 %in% wide_syn$wt_id, "missing height with ID 16 that should be
         included")
  expect(104 %in% wide_syn$ht_id, "missing height with ID 104 that should be
         included")
  expect(109 %in% wide_syn$wt_id, "missing weight with ID 109 that should be
         included")

  # run longwide on changed data with some exclusion types included
  # exclusion codes: Exclude-C-CF (carried forward), Exclude-C-Extraneous
  # (same-day extraneous)
  inc_types <- c("Include",
                 "Exclude-C-CF",
                 "Exclude-C-Extraneous")
  wide_syn <- longwide(sub_syn,
                       gcr_result = "cv",
                       inclusion_types = inc_types)

  # check that it has the correct amount of columns
  expect_equal(ncol(wide_syn), 9)

  # check that it includes specified inclusion types
  obs_ids <- c(wide_syn$wt_id, wide_syn$ht_id)
  expect(
    all(sub_syn$cv[sub_syn$id %in% obs_ids] %in% inc_types),
    "longwide() includes inclusion values that were not specified"
  )
})

test_that("longwide throws errors correctly", {
  # use synthetic data, running cleaning on a very small subset
  data("syngrowth")
  sub_syn <-
    syngrowth[syngrowth$subjid %in% unique(syngrowth$subjid)[1:5], ]
  # cleangrowth() returns a data.table; attach its `exclude` column
  gc_out <- cleangrowth(sub_syn)
  sub_syn$exclude <- as.character(gc_out$exclude)[match(sub_syn$id, gc_out$id)]

  # test with deleting a necessary column
  expect_error(longwide(sub_syn[, -2]))

  # test include_all not being correct
  expect_error(longwide(sub_syn, include_all = "hello"))

  # test duplicated ids
  sub_syn$id <- 1
  expect_error(longwide(sub_syn))
})

test_that("simple_bmi works as expected", {
  data("syngrowth")

  # Similar strategy as for longwide, create subset for speed
  sub_syn <-
    syngrowth[syngrowth$subjid %in% unique(syngrowth$subjid)[101:200],]
  # cleangrowth() returns a data.table; attach its `exclude` as `cv`
  gc_out <- cleangrowth(sub_syn)
  sub_syn$cv <- as.character(gc_out$exclude)[match(sub_syn$id, gc_out$id)]

  wide_syn <-
    longwide(sub_syn, gcr_result = "cv", include_all = TRUE)
  bmi_syn <- simple_bmi(wide_syn)
  expect_equal(TRUE, "wt" %in% names(bmi_syn))
  expect_equal(bmi_syn$bmi,
               bmi_syn$wt / ((bmi_syn$ht * .01) ^ 2))

  # Verify that invalid column names throw an error
  expect_error(simple_bmi(wide_syn, ht = "invalid_column"))
  expect_error(simple_bmi(wide_syn, wt = "invalid_wt_col", ht = "invalid_ht_col"))
})

test_that("longwide parses sex like cleangrowth (M/F, words) and warns on unrecognized", {
  mk <- function(sx) data.frame(
    id = 1:4, subjid = c("a", "a", "b", "b"), sex = sx,
    agedays = c(3000, 3000, 3100, 3100),
    param = c("HEIGHTCM", "WEIGHTKG", "HEIGHTCM", "WEIGHTKG"),
    measurement = c(130, 28, 131, 29), exclude = "Include",
    stringsAsFactors = FALSE)
  # M/F (the recommended coding) -> M/F, no warning
  expect_silent(w <- longwide(mk(c("M", "M", "f", "F"))))
  expect_equal(w$sex[order(w$subjid)], c("M", "F"))
  # 0/1 and words still work
  expect_equal(longwide(mk(c(0, 0, 1, 1)))$sex, c("M", "F"))
  expect_equal(longwide(mk(c("Male", "male", " girl", "GIRL")))$sex, c("M", "F"))
  # a user sex_coding is honored
  expect_equal(longwide(mk(c(1, 1, 2, 2)), sex_coding = list(male = 1, female = 2))$sex,
               c("M", "F"))
  # 1/2 data with the default coding stops, as in cleangrowth(), even if the
  # only 2s are on rows longwide() would filter out
  expect_error(longwide(mk(c(1, 1, 2, 2))), "looks like a 1=male/2=female coding")
  d12 <- mk(c(1, 1, 2, 2)); d12$exclude[3:4] <- "Exclude-Extraneous"
  expect_error(longwide(d12), "looks like a 1=male/2=female coding")
  # every value 1 across subjects warns, naming longwide(); a declared coding is
  # silent, and all-male data declared 1/2 comes out male
  expect_warning(w <- longwide(mk(c(1, 1, 1, 1))),
                 "POSSIBLE SEX CODING ERROR in longwide\\(\\): every value in `sex` is 1")
  expect_equal(w$sex, c("F", "F"))
  expect_silent(w <- longwide(mk(c(1, 1, 1, 1)), sex_coding = list(male = 1, female = 2)))
  expect_equal(w$sex, c("M", "M"))
  # a single subject coded 1 says nothing about the coding, so no warning
  one_subj <- mk(c(1, 1, 1, 1)); one_subj$subjid <- "a"; one_subj$agedays <- c(3000, 3000, 3100, 3100)
  expect_silent(longwide(one_subj))
  # all-male data goes through the whole chain with no sex-coding message
  expect_silent(b <- ext_bmiz(simple_bmi(longwide(mk(c("M", "M", "M", "M"))))))
  expect_false(anyNA(b$bmiz))
  # unrecognized / missing sex -> NA with a warning naming the values
  expect_warning(w <- longwide(mk(c("U", "U", "F", "F"))),
                 "2 of 4 retained measurement\\(s\\).*unrecognized values: U")
  expect_true(is.na(w$sex[w$subjid == "a"]))
  expect_warning(longwide(mk(c(NA, NA, "F", "F"))), "missing or unrecognized sex")
  # a remapped sex column is parsed too (output column is `sex`)
  d <- mk(c("M", "M", "F", "F")); names(d)[names(d) == "sex"] <- "gender"
  expect_equal(longwide(d, sex = "gender")$sex, c("M", "F"))
})

test_that("longwide reads remapped column names and returns the user's subjid/agedays names", {
  base <- data.frame(
    id = 1:6, subjid = c("a", "a", "b", "b", "c", "c"), sex = c(0, 0, 1, 1, 0, 0),
    agedays = c(3000, 3000, 3100, 3100, 3200, 3200),
    param = rep(c("HEIGHTCM", "WEIGHTKG"), 3),
    measurement = c(130, 28, 131, 29, 132, 30), exclude = "Include",
    stringsAsFactors = FALSE)
  ref <- longwide(base)
  # every required column renamed at once
  d <- base
  names(d)[1:6] <- c("row_id", "patid", "gender", "age_days", "type", "value")
  w <- longwide(d, id = "row_id", subjid = "patid", sex = "gender",
                agedays = "age_days", param = "type", measurement = "value")
  expect_equal(names(w), c("patid", "agey", "agem", "age_days", "sex",
                           "ht_id", "wt_id", "wt", "ht"))
  expect_equal(unname(as.list(w)), unname(as.list(ref)))
  # a renamed subject column alone (the common case after cleangrowth(subjid = ...))
  d2 <- base; names(d2)[names(d2) == "subjid"] <- "patid"
  expect_equal(longwide(d2, subjid = "patid")$patid, c("a", "b", "c"))
  # an unrelated column that happens to be named `subjid` is never read
  d3 <- d2; d3$subjid <- "zzz"
  w3 <- longwide(d3, subjid = "patid")
  expect_equal(nrow(w3), 3L)
  expect_equal(w3$patid, c("a", "b", "c"))
  expect_false("subjid" %in% names(w3))
  # extra_cols / gcr_result may not reuse a name longwide() needs
  expect_error(longwide(d3, subjid = "patid", extra_cols = "subjid"),
               "'subjid' cannot be used as gcr_result or in extra_cols")
  # heights with no weights, or no rows kept: 0 rows, same columns
  expect_equal(names(longwide(base[base$param == "HEIGHTCM", ])), names(ref))
  expect_equal(nrow(longwide(base[base$param == "HEIGHTCM", ])), 0L)
  expect_equal(names(suppressWarnings(longwide(base, inclusion_types = "Exclude-Pattern"))),
               names(ref))
})

test_that("longwide reads param like cleangrowth: coding, imperial, LENGTHCM, HC", {
  mk <- function(p, m) data.frame(
    id = seq_along(p), subjid = "a", sex = "M", agedays = 3000,
    param = p, measurement = m, exclude = "Include", stringsAsFactors = FALSE)
  # imperial converts to metric; LENGTHCM is height; HC dropped silently
  w <- expect_silent(longwide(mk(c("HEIGHTIN", "WEIGHTLBS", "HEADCM"),
                                 c(50, 60, 52))))
  expect_equal(w$ht, 50 * 2.54)
  expect_equal(w$wt, 60 / 2.2046226)
  expect_equal(longwide(mk(c("LENGTHCM", "WEIGHTKG"), c(127, 27)))$ht, 127)
  # param_coding labels resolve
  w2 <- longwide(mk(c("ht", "wt"), c(127, 27)),
                 param_coding = list(HEIGHTCM = "ht", WEIGHTKG = "wt"))
  expect_equal(c(w2$ht, w2$wt), c(127, 27))
  # unrecognized labels are dropped with a warning
  expect_warning(w3 <- longwide(mk(c("HEIGHTCM", "WEIGHTKG", "BMI"), c(127, 27, 16.7))),
                 "does not recognize \\(values: BMI\\)")
  expect_equal(nrow(w3), 1L)
})

test_that("ext_bmiz() accepts numeric sex only with both 1s and 2s and nothing else", {
  mk <- function(sx) data.frame(sex = sx, agem = 120.5, wt = 40, ht = 145, bmi = 40 / 1.45^2,
                                stringsAsFactors = FALSE)
  msg <- "numeric codes that cannot be read safely"
  # numeric, text, and factor versions of the same codes all stop
  for (sx in list(c(0, 1), c("0", "1"), factor(c(0, 1)), c(0, 1, NA), 0, c(1, 1), 1, c(2, 2), 2,
                  c(0, 1, 2), c(1, 2, 9), c(1, 2, 2.5), c("M", "F", "1"), c("M", "0"), c(10, 20))) {
    expect_error(ext_bmiz(mk(sx)), msg)
  }
  # both 1s and 2s, also as text or factor, and with missing values
  expect_silent(out <- ext_bmiz(mk(c(1, 2))))
  expect_silent(ext_bmiz(mk(c(1L, 2L))))
  expect_silent(out_txt <- ext_bmiz(mk(c("1", "2"))))
  expect_silent(ext_bmiz(mk(factor(c(1, 2)))))
  expect_equal(as.numeric(out_txt$bmiz), as.numeric(out$bmiz))
  # text codes give the same results, including single-sex columns
  expect_silent(out_mf <- ext_bmiz(mk(c("M", "F"))))
  expect_equal(as.numeric(out_mf$bmiz), as.numeric(out$bmiz))
  expect_equal(as.numeric(ext_bmiz(mk(c("m", "f")))$bmiz), as.numeric(out$bmiz))
  expect_equal(as.numeric(ext_bmiz(mk(c("B", "G")))$bmiz), as.numeric(out$bmiz))
  expect_equal(as.numeric(ext_bmiz(mk(c("male", "female")))$bmiz), as.numeric(out$bmiz))
  expect_equal(as.numeric(ext_bmiz(mk(c("Boy", "Girl")))$bmiz), as.numeric(out$bmiz))
  expect_silent(ext_bmiz(mk(c("M", "M"))))
  expect_silent(ext_bmiz(mk(c("F", "F"))))
})

test_that("ext_bmiz() warns with a count for missing or unrecognized sex, within the age range only", {
  mk <- function(sx, agem = 120.5) data.frame(sex = sx, agem = agem, wt = 40, ht = 145,
                                              bmi = 40 / 1.45^2, stringsAsFactors = FALSE)
  # unrecognized text and missing values are NA, with a count
  expect_warning(out <- ext_bmiz(mk(c("U", "M", "F"))), "1 of 3 row\\(s\\).*missing or unrecognized")
  expect_true(is.na(out$bmiz[1]))
  expect_false(anyNA(out$bmiz[2:3]))
  expect_warning(ext_bmiz(mk(c(1, 2, NA, NA))), "2 of 4 row\\(s\\)")
  # outside 24-240 months sex does not matter
  expect_silent(ext_bmiz(mk(c(1, 2, NA), agem = c(120.5, 120.5, 300.5))))
  # a missing age with a missing sex is simply out of range
  expect_silent(out <- ext_bmiz(mk(c(1, 2, NA), agem = c(120.5, 120.5, NA))))
  expect_equal(nrow(out), 3L)
  # logical values are not sex codes, so FALSE is not read as female
  expect_warning(out <- ext_bmiz(mk(c(TRUE, FALSE))), "2 of 2 row\\(s\\)")
  expect_true(all(is.na(out$bmiz)))
})

test_that("recode_sex() in place does not recode twice, and its output works in ext_bmiz()", {
  d <- data.frame(sex = c(0, 1, 9), agem = 120.5, wt = 40, ht = 145, bmi = 40 / 1.45^2)
  r <- recode_sex(d, targetcol = "sex")
  expect_equal(r$sex, c(1L, 2L, NA_integer_))
  ref <- ext_bmiz(data.frame(sex = c(1, 2), agem = 120.5, wt = 40, ht = 145, bmi = 40 / 1.45^2))
  out <- suppressWarnings(ext_bmiz(r))
  expect_equal(as.numeric(out$bmiz[1:2]), as.numeric(ref$bmiz))
})

test_that("ext_bmiz() warns and overwrites existing output columns, leaving the input alone", {
  df <- data.frame(sex = c(1, 2), agem = 120.5, wt = 40, ht = 145, bmi = 40 / 1.45^2)
  first <- ext_bmiz(df)
  expect_warning(second <- ext_bmiz(first), "already has column(s) named bmiz", fixed = TRUE)
  expect_equal(names(second), names(first))
  expect_equal(as.numeric(second$bmiz), as.numeric(first$bmiz))
  # a user's own column of that name: replaced in the output, not in the input
  mine <- cbind(df, bmiz = c(99, 99))
  expect_warning(out <- ext_bmiz(mine), "named bmiz")
  expect_equal(as.numeric(out$bmiz), as.numeric(first$bmiz))
  expect_equal(mine$bmiz, c(99, 99))
})

test_that("ext_bmiz() warns about rows at about 729-730 days", {
  mk <- function(days) data.frame(sex = "F", agem = days / 30.4375, wt = 12, ht = 86, bmi = 12 / 0.86^2)
  expect_warning(out <- ext_bmiz(mk(c(729, 730, 731))), "2 row\\(s\\) are at about 729-730 days")
  expect_true(all(is.na(out$bmiz[1:2])))
  expect_false(is.na(out$bmiz[3]))
  expect_silent(ext_bmiz(mk(731)))
  expect_silent(ext_bmiz(mk(700)))  # further below 24 months: no warning
  # no weight or height: nothing to calculate, so nothing to point out
  d <- mk(730); d$wt <- NA_real_; d$ht <- NA_real_
  expect_silent(ext_bmiz(d))
})
