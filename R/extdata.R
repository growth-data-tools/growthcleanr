#' BMI Anthro
#'
#' Part of default CDC-derived tables
#'
#' Contains BMI data for calculating BMI
#'
#' @name bmianthro
#'
#' @section bmianthro.txt.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' CDC Growth Percentile Table
#'
#' Part of default CDC-derived tables
#'
#' Contains percentiles for various ages, gender, and weights, pre-calculated by CDC
#'
#' @name growth_cdc_ext
#'
#' @section growthfile_cdc_ext.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' CDC Growth Percentile Table for Infants
#'
#' Part of default CDC-derived tables
#'
#' Contains percentiles for various ages, gender, and weights, pre-calculated by CDC for infants algorithm
#'
#' @name growth_cdc_ext_infants
#'
#' @section growthfile_cdc_ext_infants.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' WHO Growth Percentile Table
#'
#' Part of default WHO-derived tables
#'
#' Contains percentiles for various ages, gender, and weights, pre-calculated by WHO
#'
#' @name growth_who_ext
#'
#' @section growthfile_who.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' Length to Age Table
#'
#' Part of default CDC-derived tables
#'
#' Contains percentiles for various ages, gender, and weights, pre-calculated by CDC
#'
#' @name lenanthro
#'
#' @section lenanthro.txt.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' Infants reference medians
#'
#' Contains reference median values for default recentering in the infants
#' algorithm
#'
#' @name rc-reference-medians
#'
#' @section rcfile-2023-08-15_format.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' NHANES reference medians
#'
#' Contains reference median values for default recentering, derived from NHANES
#' years 2009-2018
#'
#' @name nhanes-reference-medians
#'
#' @section nhanes-reference-medians.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' Tanner Growth Velocity Table
#'
#' Part of default CDC-derived tables
#'
#' Contains velocities for growth pre-calculated by CDC
#'
#' @name tanner_ht_vel
#'
#' @section tanner_ht_vel.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' Tanner Growth Velocity Table for Infants
#'
#' Part of default CDC-derived tables
#'
#' Contains velocities for growth pre-calculated by CDC, used for the infants
#' algorithm
#'
#' @name tanner_ht_vel_rev
#'
#' @section tanner_ht_vel_rev.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' Tanner Growth Velocity Table with (2\eqn{\sigma})
#'
#' Part of default CDC-derived tables
#'
#' Contains velocities for growth pre-calculated by CDC, including those 2
#' standard deviations away.
#'
#' @name tanner_ht_vel_with_2sd
#'
#' @section tanner_ht_vel_with_2sd.csv.gz:
#'
#' Used in function `acf_answers()`
#'
#'
NULL

#' Weight Anthro Table
#'
#' Part of default CDC-derived tables
#'
#' Contains median and standard deviation for weight by age and gender
#'
#' @name weianthro
#'
#' @section weianthro.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' WHO Maximum Height Velocity for (3\eqn{\sigma})
#'
#' Part of default WHO-derived tables
#'
#' Contains three standard deviations for the World Health Organization values of
#' maximum height velocities.
#'
#' @name who_ht_maxvel
#'
#' @section who_ht_maxvel_3sd.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' WHO Maximum Height Velocity for (2\eqn{\sigma})
#'
#' Part of default WHO-derived tables
#'
#' Contains two standard deviations for the World Health Organization values of
#' maximum height velocities.
#'
#' @name who_ht_maxvel_2sd
#'
#' @section who_ht_maxvel_2sd.csv.gz:
#'
#' Used in function `acf_answers()`
#'
#'
NULL

#' WHO Height Velocity for (3\eqn{\sigma})
#'
#' Part of default WHO-derived tables
#'
#' Contains three standard deviations for the World Health Organization values of
#' height velocities.
#'
#' @name who_ht_vel_3sd
#'
#' @section who_ht_vel_3sd.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' WHO Height Velocity for (2\eqn{\sigma})
#'
#' Part of default WHO-derived tables
#'
#' Contains two standard deviations for the World Health Organization values of
#' height velocities.
#'
#' @name who_ht_vel_2sd
#'
#' @section who_ht_vel_2sd.csv.gz:
#'
#' Used in function `acf_answers()`
#'
#'
NULL

#' WHO Maximum Head Circumference Velocity for (3\eqn{\sigma})
#'
#' Part of default WHO-derived tables
#'
#' Contains three standard deviations for the World Health Organization values of
#' maximum head circumference velocities.
#'
#' @name who_hc_maxvel
#'
#' @section who_hc_maxvel_3sd_infants.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' WHO Head Circumference Velocity for (3\eqn{\sigma})
#'
#' Part of default WHO-derived tables
#'
#' Contains three standard deviations for the World Health Organization values of
#' head circumference velocities.
#'
#' @name who_hc_vel_3sd
#'
#' @section who_hc_vel_3sd_infants.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' CDC SAS BMI Input
#'
#' Contains input data for CDC SAS macro for calculating BMI values.
#'
#' @name test_syngrowth_wide
#'
#' @section test_syngrowth_wide.csv.gz:
#'
#' Used to test function `ext_bmiz()`
#'
#'
NULL

#' CDC SAS BMI Output
#'
#' Contains results of CDC SAS macro for calculating BMI values.
#'
#' @name test_syngrowth_sas_output_compare
#'
#' @section test_syngrowth_sas_output_compare.csv.gz:
#'
#' Used to test function `ext_bmiz()`
#'
#'
NULL

#' CDC BMI reference data
#'
#' Used for extended BMIz computation
#'
#' @name CDCref_d
#'
#' @section CDCref_d.csv.gz:
#'
#' Used for extended BMI computation
#'
#'
NULL

#' Fenton-Based Corrected Age Reference (Weight to GA Lookup)
#'
#' Approximated reference values for premature infant corrected-age estimation, derived from
#' data extracted from printed Fenton (2025) growth charts. Provides a
#' reverse lookup from integer weight (grams, rounded to 10) to estimated
#' gestational age in days, by sex.
#'
#' @name fent_for_corraga
#'
#' @section fent_for_corraga.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' Fenton-Based Corrected Z-Score Reference
#'
#' Approximated reference values for premature infant corrected z-score calculation, derived
#' from data extracted from printed Fenton (2025) growth charts. Allows determination of approximate
#' z-scores by gestational age and sex.
#'
#' @name fent_for_corrz
#'
#' @section fent_for_corrz.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL
