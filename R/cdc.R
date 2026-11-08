#' Function for LMS formula with modified (m) z-scores
#'
#' @keywords internal
#' @noRd
z_score <- function(var, l, m, s) {
  ls <- l * s
  invl <- 1 / l
  z <- (((var / m) ^ l) - 1) / (ls) # z-score formula
  sdp2 <- (m * (1 + 2 * ls) ^ (invl)) - m
  # modified z-score (+2)
  sdm2 <- m - (m * (1 - 2 * ls) ^ (invl))
  mz <-
    fifelse(var < m, (var - m) / (0.5 * sdm2), (var - m) / (sdp2 * 0.5))
  return(list(z, mz))
}

#' Function to reorder columns of data table
#'
#' @keywords internal
#' @noRd
set_cols_first <- function(DT, cols, intersection = TRUE)
{
  # thanks to hutils
  if (intersection) {
    return(setcolorder(DT, c(
      intersect(cols, names(DT)),
      setdiff(names(DT), cols)
    )))
  }
  else {
    return(setcolorder(DT, c(cols, setdiff(
      names(DT), cols
    ))))
  }
}

#' Calculate extended BMI measures
#'
#' \code{ext_bmiz} Calculates the sigma (scale parameter for the half-normal
#' distribution), extended BMI percentile, extended BMIz, and the CDC LMS
#' Z-scores for weight, height, and BMI for children between 2 and 19.9 years
#' of age. Note that for BMIs <= 95th percentile of the CDC growth charts, the
#' extended values for BMI are equal to the LMS values. The extended values
#' differ only for children who have a BMI > 95th percentile.
#'
#' This function produces output equivalent to
#' [CDC software](https://www.cdc.gov/growth-chart-training/hcp/computer-programs/index.html)
#' and the
#' [cdcanthro R package](https://CRAN.R-project.org/package=cdcanthro).
#'
#' The extended BMIz is the inverse cumulative distribution function (CDF) of
#' the extended BMI percentile. If the extended percentile is very close to
#' 100, the \code{qnorm} function in R produces an infinite value. This occurs
#' only if the extended BMI percentile is > 99.99999999999999. This occurs
#' infrequently, such as a 48-month-old with a BMI > 39, and it is likely that
#' these BMIs represent data entry errors. For these cases, extended BMIz is
#' set to 8.21, a value that is slightly greater than the largest value that
#' can be calculated.
#'
#' The output columns are listed under Value below.
#'
#' \code{data} must have columns for at least age, sex, weight, height, and bmi.
#'
#' \code{age} should be coded in months, using the most precise values available.
#' To convert to months from age in years, multiply by 12. To convert to months
#' from age in days, divide by 30.4375 (365.25 / 12).
#'
#' \code{sex}: we recommend M and F, which is what \code{longwide()} returns.
#' Male can be coded as boy, Boy, b, B, male, Male, m, or M, and female can be
#' coded as girl, Girl, g, G, female, Female, f, or F. Numeric sex is accepted
#' only if \code{sex} has both 1s and 2s and no other values, read as 1 = male,
#' 2 = female; otherwise \code{ext_bmiz()} stops. This is because 0/1 is the
#' numeric coding in \code{cleangrowth()}, and a column of only 1s could be either
#' coding. The 1/2 coding matches the CDC reference data, CDC software, and the
#' cdcanthro package. Other text values, and logical values, are not recognized:
#' their z-scores are \code{NA}, with a warning.
#'
#' \code{wt} should be in kilograms.
#'
#' \code{ht} should be in centimeters.
#'
#' Specify the input data column names for \code{age}, \code{wt},
#' \code{ht}, \code{bmi} using quotation marks. See example below.
#'
#' \code{ext_bmiz()} does not change \code{data}. It returns a copy of
#' \code{data} with the new columns added; your column names and values are
#' kept as they are (the 0.5 added by \code{adjust.integer.age} is used only in
#' the calculation). If \code{data} already has a column with the name of one of
#' the columns \code{ext_bmiz()} adds (\code{bmiz}, \code{bmip}, \code{waz},
#' \code{wp}, \code{haz}, \code{hp}, \code{p95}, \code{p97}, \code{bmip95},
#' \code{mod_bmiz}, \code{mod_waz}, \code{mod_haz}, \code{sigma},
#' \code{original_bmip}, \code{original_bmiz}, \code{sev_obese},
#' \code{obese}), for example from an earlier run, it warns and the returned
#' data has the newly calculated values in that column; rename the column first
#' to keep both.
#'
#' Values are calculated for ages 24 through 240 months, as in the cdcanthro
#' package. Age 730 days is 23.98 months (730 / 30.4375), which is below 24
#' months, so a row at 730 days has \code{NA} values even though a child at that
#' age may be exactly 2 years old; values begin at 731 days. A warning gives the
#' number of rows at about 729-730 days.
#'
#' If the argument \code{adjust.integer.age} is \code{TRUE} (the default),
#' 0.5 will be added to all \code{age} values for the calculation if all input values are integers. Set to
#' \code{FALSE} to disable.
#'
#' By default, the reference data file \code{CDCref_d.csv}, made available at
#' https://www.cdc.gov/growth-chart-training/hcp/computer-programs/index.html, is included
#' in this package for convenience. If you are developing this package, use
#' \code{ref.data.path} to adjust the path to this file from your working
#' directory if necessary.
#'
#' @param data Input data frame or data table
#'
#' @param age Name of input column containing subject age in months in quotes, default "agem"
#' @param wt Name of input column containing weight (kg) value in quotes, default "wt"
#' @param ht Name of input column containing height (cm) value in quotes, default "ht"
#' @param bmi Name of input column containing calculated BMI in quotes, default "bmi"
#' @param adjust.integer.age If age inputs are all integer, add 0.5 if TRUE;
#'   default TRUE
#' @param ref.data.path Path to directory containing reference data
#'
#' @return A data.table: a copy of \code{data} (same rows, row order, column
#'   names, and values) with these columns added: \code{bmiz}, \code{bmip},
#'   \code{waz}, \code{wp}, \code{haz}, \code{hp}, \code{p95}, \code{p97},
#'   \code{bmip95}, \code{mod_bmiz}, \code{mod_waz}, \code{mod_haz},
#'   \code{sigma}, \code{original_bmip}, \code{original_bmiz},
#'   \code{sev_obese}, \code{obese}.
#'
#' @references Wei R, Ogden CL, Parsons VL, Freedman DS, Hales CM. A method
#'   for calculating BMI z-scores and percentiles above the 95th percentile of
#'   the CDC growth charts. Ann Hum Biol. 2020 Sep;47(6):514-521.
#'   \doi{10.1080/03014460.2020.1808065}.
#'
#' @export
#' @import data.table
#' @importFrom stats approx pnorm qnorm
#' @rawNamespace import(R.utils, except = c(extract))
#' @examples
#' \donttest{
#' # Run on a small subset of given data
#' df <- as.data.frame(syngrowth)
#' df <- df[df$subjid %in% unique(df[, "subjid"])[1:5], ]
#' # cleangrowth() returns the data with an 'exclude' column
#' clean_df <- cleangrowth(df)
#' df_wide <- longwide(clean_df) # convert to wide format for ext_bmiz
#' df_wide_bmi <- simple_bmi(df_wide) # compute simple BMI
#'
#' # Calling the function with default column names
#' df_bmiz <- ext_bmiz(df_wide_bmi)
#'
#' # Specifying different column names; note that quotes are used
#' dfc <- simple_bmi(df_wide)
#' colnames(dfc)[colnames(dfc) %in% c("agem", "wt", "ht")] <-
#'   c("agemos", "weightkg", "heightcm")
#' df_bmiz <- ext_bmiz(dfc, age="agemos", wt="weightkg", ht="heightcm")
#'
#' # Disabling conversion of all-integer age in months to (age + 0.5)
#' dfc <- simple_bmi(df_wide)
#' df_bmiz <- ext_bmiz(dfc, adjust.integer.age=FALSE)
#' }
ext_bmiz <- function (data,
                      age = "agem",
                      wt = "wt",
                      ht = "ht",
                      bmi = "bmi",
                      adjust.integer.age = TRUE,
                      ref.data.path = "")
{
  # avoid "no visible binding" warnings
  agemos <- agemos1 <- agemos2 <- agey <- NULL
  bmip95 <- bp <- bz <- denom <- ebp <- ebz <- haz <- l <- NULL
  lbmi1 <- lbmi2 <- lht1 <- lht2 <- lwt1 <- lwt2 <- m <- NULL
  mbmi <- mbmi1 <- mbmi2 <- mht1 <- mht2 <- mref <- mwt1 <- NULL
  mwt2 <- p95 <- s <- sbmi <- sbmi1 <- sbmi2 <- seq_ <- NULL
  sex <- sexn <- sht1 <- sht2 <- sigma <- sref <- swt1 <- NULL
  swt2 <- waz <- z1 <- `_AGEMOS1` <- NULL
  wp <- hp <- p97 <- dist1 <- adist1 <- perc1 <- aperc1 <- NULL
  obese <- sev_obese <- NULL

  # Output columns ext_bmiz() adds (in this order).
  out_cols <- c("bmiz", "bmip", "waz", "wp", "haz", "hp", "p95", "p97",
                "bmip95", "mod_bmiz", "mod_waz", "mod_haz", "sigma",
                "original_bmip", "original_bmiz", "sev_obese", "obese")

  # ext_bmiz() never modifies `data`: it reads the columns it needs into its own
  # working table and returns a copy of `data` with the new columns added. The
  # user's column names are kept as they are.
  if (!is.data.frame(data))
    stop("ext_bmiz(): `data` must be a data frame or data table.", call. = FALSE)
  in_cols <- c(age = age, wt = wt, ht = ht, bmi = bmi, sex = "sex")
  absent <- in_cols[!in_cols %in% names(data)]
  if (length(absent) > 0)
    stop(sprintf("ext_bmiz(): column(s) not found in `data`: %s. Name the columns with the age, wt, ht, and bmi arguments; the sex column must be named sex.",
                 paste(absent, collapse = ", ")), call. = FALSE)
  clash <- intersect(out_cols, names(data))
  if (length(clash) > 0)
    warning(sprintf("ext_bmiz(): `data` already has column(s) named %s (for example, from an earlier ext_bmiz() run). The returned data has the newly calculated values in these column(s); your `data` is not changed. To keep both, rename these column(s) before calling ext_bmiz().",
                    paste(clash, collapse = ", ")), call. = FALSE)

  dorig <- as.data.table(data.table::copy(data))
  data <- data.table(seq_ = seq_len(nrow(dorig)),
                     sex = dorig[["sex"]],
                     age = dorig[[age]], wt = dorig[[wt]],
                     ht = dorig[[ht]], bmi = dorig[[bmi]])
  if (adjust.integer.age) {
    if (isTRUE(all.equal(data$age, round(data$age)))) {
      data[, `:=`(age, age + 0.5)]
    }
  }

  # Numeric sex is accepted only as a column with both 1s and 2s and nothing else.
  # 0/1 is the cleangrowth() numeric coding, and a column of only 1s could be either
  # coding, so anything else numeric stops rather than giving wrong z-scores.
  sex_present <- .gc_norm(data$sex)
  sex_present <- unique(sex_present[!is.na(sex_present) & sex_present != ""])
  sex_is_num <- !is.na(suppressWarnings(as.numeric(sex_present)))
  if (any(sex_is_num) && !setequal(sex_present, c("1", "2"))) {
    stop(sprintf("ext_bmiz(): `sex` has numeric codes that cannot be read safely (values: %s). Numeric sex is accepted only if `sex` has both 1s (male) and 2s (female) and no other values. Recode sex to M/F first; longwide() output is already coded M/F. If a value such as 0 means unknown, set it to NA.",
                 paste(utils::head(sex_present, 5), collapse = ", ")), call. = FALSE)
  }

  data[, sexn := toupper(substr(sex, 1, 1))]
  data[, sexn := fcase(sexn %in% c(1, 'B', 'M'), 1L,
                       sexn %in% c(2, 'G', 'F'), 2L)]
  # Logical values are not sex codes (FALSE would otherwise read as "F").
  if (is.logical(data$sex)) data[, sexn := NA_integer_]

  in_range <- data[, between(age, 24, 240) & !(is.na(wt) & is.na(ht))]
  in_range[is.na(in_range)] <- FALSE  # missing age
  # Rows at about 729-730 days fall just under 24 months, though a child at 730
  # days may be exactly 2 years old, so they are worth pointing out.
  n_near_24 <- data[, sum(!is.na(age) & age >= 729 / 30.4375 & age < 24 &
                            !(is.na(wt) & is.na(ht)))]
  if (n_near_24 > 0L) {
    warning(sprintf("ext_bmiz(): %d row(s) are at about 729-730 days of age, just under 24 months (730 days is 23.98 months); their z-scores and percentiles are NA. ext_bmiz() covers ages 24-240 months, as does the cdcanthro package; values begin at 731 days.",
                    n_near_24), call. = FALSE)
  }
  n_no_sex <- sum(in_range & is.na(data$sexn))
  if (n_no_sex > 0L) {
    warning(sprintf("ext_bmiz(): %d of %d row(s) at ages 24-240 months have a missing or unrecognized sex; their z-scores and percentiles are NA. Recognized codes: boy, b, male, m (male) and girl, g, female, f (female), or 1 (male) and 2 (female) if `sex` has both.",
                    n_no_sex, sum(in_range)), call. = FALSE)
  }

  data <- data[in_range, .(seq_, sexn, age, wt, ht, bmi)]
  v1 <- c("seq_", "id", "sexn", "age", "wt", "ht", "bmi")

  dref_path <-
    ifelse(
      ref.data.path == "",
      system.file("extdata/CDCref_d.csv.gz", package = "growthcleanr"),
      paste(ref.data.path, "CDCref_d.csv.gz", sep = "")
    )
  # Note: referring to underscore-leading column as `_AGEMOS1`, i.e. with
  # backticks, results in a no visible binding warning, but vars can't start
  # with an "_", so we have to use backticks at assignment up above as well.
  dref <- fread(dref_path)[`_AGEMOS1` > 23 & denom == "age"]
  names(dref) <- tolower(names(dref))
  names(dref) <- gsub("^_", "", names(dref))
  setnames(dref, 'sex', 'sexn')

  d20 <- dref[agemos2 == 240, .(sexn,
                                agemos2,
                                lwt2,
                                mwt2,
                                swt2,
                                lbmi2,
                                mbmi2,
                                sbmi2,
                                lht2,
                                mht2,
                                sht2)]
  names(d20) <- gsub("2", "", names(d20))

  dref <- dref[, .(sexn,
                   agemos1,
                   lwt1,
                   mwt1,
                   swt1,
                   lbmi1,
                   mbmi1,
                   sbmi1,
                   lht1,
                   mht1,
                   sht1)]
  names(dref) <- gsub("1", "", names(dref))

  dref <- rbindlist(list(dref, d20))
  adj_bmi_met <- dref[agemos == 240, .(sexn, mbmi, sbmi)]
  setnames(adj_bmi_met, c("sexn", "mref", "sref"))
  dref <- dref[adj_bmi_met, on = "sexn"]
  v <- c("sexn",
         "age",
         "wl",
         "wm",
         "ws",
         "bl",
         "bm",
         "bs",
         "hl",
         "hm",
         "hs",
         "mref",
         "sref")
  setnames(dref, v)
  if (length(setdiff(data$age, dref$age)) > 0) {
    uages <- unique(data$age)
    fapprox <- function(i) {
      .d <- dref[sexn == i]
      fapp <- function(vars, ...)
        approx(.d$age, vars,
               xout = uages)$y
      # Note: specifying v with "..v" gives no visible binding warning, use
      # with option. lapply (not sapply) so the result always has one row per
      # unique age, even when there is only one unique age.
      as.data.table(lapply(.d[, v, with = FALSE], fapp))
    }
    dref <- rbindlist(lapply(1:2, fapprox))
  }

  setkey(data, sexn, age)
  setkey(dref, sexn, age)
  dt <- dref[data]

  dt[, `:=`(c("waz", "mwaz"), z_score(dt$wt, dt$wl, dt$wm, dt$ws))]
  dt[, `:=`(c("haz", "mhaz"), z_score(dt$ht, dt$hl, dt$hm, dt$hs))]
  dt[, `:=`(c("bz", "mbz"), z_score(dt$bmi, dt$bl, dt$bm, dt$bs))]

  setDT(dt)
  setnames(dt, c("bl", "bm", "bs"), c("l", "m", "s"))
  dt[, `:=`(c("wl", "wm", "ws", "hl", "hm", "hs"), NULL)]

  # Sequential assignments (each may use the columns created before it).
  dt[, bp := 100 * pnorm(bz)]
  dt[, p95 := m * (1 + l * s * qnorm(0.95)) ^ (1 / l)]
  dt[, p97 := m * (1 + l * s * qnorm(0.97)) ^ (1 / l)]
  dt[, bmip95 := 100 * (bmi / p95)]
  dt[, wp := 100 * pnorm(waz)]
  dt[, hp := 100 * pnorm(haz)]

  # other BMI metrics -- PMID 31439056
  dt[, z1 := ((bmi / m) - 1) / s]         # LMS formula when L=1: ((BMI/M)-1)/S
  dt[, dist1 := z1 * m * s]               # unadjusted distance from median
  dt[, adist1 := z1 * sref * mref]        # Adjusted (to age 20y) dist from median
  dt[, perc1 := z1 * 100 * s]             # unadjusted %distance from median
  dt[, aperc1 := z1 * 100 * sref]         # adj %distance from median

  dt[, obese := 1L * (bmi >= p95)]
  dt[, sev_obese := 1L * (bmip95 >= 120)]

  # now create Extended z-score for BMI >=95th P
  dt[, `:=`(ebz = bz,
            ebp = bp,
            agey = age / 12)]
  dt[, `:=`(
    sigma,
    fifelse(
      sexn == 1,
      0.3728 + 0.5196 * agey - 0.0091 * agey ^ 2,
      0.8334 + 0.3712 * agey - 0.0011 * agey ^ 2
    )
  )]
  dt[bp >= 95, `:=`(ebp, 90 + 10 * pnorm((bmi - p95) / sigma))]
  dt[bp >= 95, `:=`(ebz, qnorm(ebp / 100))]
  dt[bp > 99 & is.infinite(ebz), `:=`(ebz, 8.21)]

  x <- c("agey", "mref", "sref", "sexn", "wt", "ht", "bmi")
  dt[, `:=`((x), NULL)]
  setnames(
    dt,
    c(
      "adist1",
      "aperc1",
      "bp",
      "bz",
      "mbz",
      "mwaz",
      "mhaz",
      "ebp",
      "ebz",
      "l",
      "m",
      "s"
    ),
    c(
      "adj_dist1",
      "adj_perc1",
      "original_bmip",
      "original_bmiz",
      "mod_bmiz",
      "mod_waz",
      "mod_haz",
      "bmip",
      "bmiz",
      "bmi_l",
      "bmi_m",
      "bmi_s"
    )
  )

  v <- c(
    "seq_",
    "bmiz",
    "bmip",
    "waz",
    "wp",
    "haz",
    "hp",
    "p95",
    "p97",
    "bmip95",
    "mod_bmiz",
    "mod_waz",
    "mod_haz",
    "sigma",
    "original_bmip",
    "original_bmiz",
    "sev_obese",
    "obese"
  )
  dt <- dt[, v, with = FALSE]

  # Add the new columns to the copy of `data`, in its original row order; rows
  # outside 24-240 months (or with no weight or height) get NA.
  dtot <- dorig
  idx <- match(seq_len(nrow(dtot)), dt$seq_)
  for (col in out_cols) set(dtot, j = col, value = dt[[col]][idx])

  # Add variable labels (the "label" attribute, shown e.g. by RStudio's viewer)
  labs <- c(
      stats::setNames(c("Age (months)", "Height (cm)", "Weight (kg)", "BMI"),
                      c(age, ht, wt, bmi)),
      original_bmiz = "LMS BMI-for-sex/age z-score",
      original_bmip = "LMS BMI-for-sex/age percentile",
      waz = "LMS Weight-for-sex/age z-score",
      wp = "LMS Weight-for-sex/age percentile",
      haz = "LMS Height-for-sex/age z-score",
      hp = "LMS Height-for-sex/age percentile",
      p95 = "95th percentile of BMI in growth charts",
      p97 = "97th percentile of BMI in growth charts",
      bmip95 = "BMI as a percentage of the 95th percentile",
      mod_bmiz = "Modified BMI-for-age z-score",
      mod_waz = "Modified Weight-for-age z-score",
      mod_haz = "Modified Height-for-age z-score",
      sigma = "Scale parameter for half-normal distribution",
      bmip = "LMS / Extended BMI percentile",
      bmiz = "LMS / Extended BMI z-score",
      sev_obese = "BMI >= 120% of 95th percentile (0/1)",
      obese = "BMI >= 95th percentile (0/1)"
  )
  for (nm in intersect(names(labs), names(dtot)))
    setattr(dtot[[nm]], "label", labs[[nm]])
  return(dtot[])
}
