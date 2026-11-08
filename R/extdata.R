#' CDC Growth Reference Table
#'
#' Part of default CDC-derived tables
#'
#' Contains CDC LMS parameters and CSD values (median plus half the distance
#' to +/-2 SD) for weight, height, head circumference, and BMI, by age in days
#' and sex.
#'
#' @name growth_cdc_ext_infants
#'
#' @section growthfile_cdc_ext_infants.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' WHO Growth Reference Table
#'
#' Part of default WHO-derived tables
#'
#' Contains WHO LMS parameters and CSD values for weight, height, head
#' circumference, and BMI, by age in days (0 to 5 years) and sex.
#'
#' @name growth_who_ext
#'
#' @section growthfile_who.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' Recentering reference medians
#'
#' Contains reference median z-scores (column rcmedian.sd) by age in days, sex,
#' and param, used for the default recentering in the child algorithm
#' (sd.recenter = NA). See vignette("recentering").
#'
#' @name rc-reference-medians
#'
#' @section rcfile-resmoothed.csv.gz:
#'
#' Used in function `cleangrowth()`
#'
#'
NULL

#' Child unit-error-range reference values
#'
#' Part of default reference tables
#'
#' Contains, for each age in days, sex, and param, the measurement values at
#' recentered CSD z-scores of -3 (column zn3) and +3 (column zp3), computed
#' with the same WHO/CDC blending and default recentering used by the child
#' algorithm. Covers weight and height from birth to 20 years and head
#' circumference from birth to 5 years (1826 days). Used to compute the informational
#' \code{unit_error_range} output column and the unit-error check in Evil Twins
#' (Child Step 9). See \code{vignette("algorithm-functions")}.
#'
#' @name child_z3_reference
#'
#' @section child_z3_reference.csv.gz:
#'
#' Used in function `cleangrowth()` (when \code{unit_error_range = TRUE}, the
#' default)
#'
#'
NULL

#' Tanner Growth Velocity Table
#'
#' Part of default reference tables
#'
#' Contains height velocities from Tanner and Davies (1985)
#'
#' @references Tanner JM, Davies PS. Clinical longitudinal standards for
#'   height and height velocity for North American children. J Pediatr. 1985
#'   Sep;107(3):317-329. \doi{10.1016/s0022-3476(85)80501-1}.
#'
#' @name tanner_ht_vel
#'
#' @section tanner_ht_vel.csv.gz:
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
#' @references World Health Organization. WHO Child Growth Standards: Growth
#'   velocity based on weight, length and head circumference: Methods and
#'   development. Geneva: World Health Organization; 2009. ISBN 9789241547635.
#'
#' @name who_ht_maxvel
#'
#' @section who_ht_maxvel_3sd.csv.gz:
#'
#' Used in function `cleangrowth()`
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
#' @references World Health Organization. WHO Child Growth Standards: Growth
#'   velocity based on weight, length and head circumference: Methods and
#'   development. Geneva: World Health Organization; 2009. ISBN 9789241547635.
#'
#' @name who_ht_vel_3sd
#'
#' @section who_ht_vel_3sd.csv.gz:
#'
#' Used in function `cleangrowth()`
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
#' @references World Health Organization. WHO Child Growth Standards: Growth
#'   velocity based on weight, length and head circumference: Methods and
#'   development. Geneva: World Health Organization; 2009. ISBN 9789241547635.
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
#' @references World Health Organization. WHO Child Growth Standards: Growth
#'   velocity based on weight, length and head circumference: Methods and
#'   development. Geneva: World Health Organization; 2009. ISBN 9789241547635.
#'
#' @name who_hc_vel_3sd
#'
#' @section who_hc_vel_3sd_infants.csv.gz:
#'
#' Used in function `cleangrowth()`
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

#' Approximated Fenton Median Weights by Gestational Age
#'
#' A table of approximated Fenton median weights by gestational age, derived
#' from printed Fenton (2025) charts. Maps sex and integer weight (grams,
#' rounded to nearest 10) to the gestational age in days at which that weight
#' is the median. These are approximations derived from the charts, not values
#' published by the Fenton authors. See \code{vignette("wrapper")},
#' "Gestational-age reference values".
#'
#' @references Fenton TR, Elmrayed S, Alshaikh BN. Fenton Third-Generation
#'   Growth Charts of Preterm Infants Without Abnormal Fetal Growth: A
#'   Systematic Review and Meta-Analysis. Paediatr Perinat Epidemiol. 2025
#'   Aug;39(6):543-555. \doi{10.1111/ppe.70035}. Epub 2025 Jun 19. PMID:
#'   40534585; PMCID: PMC12391854.
#' @name fent_foraga
#'
#' @section fent_foraga.csv.gz:
#'
#' Used in function `cleangrowth()` for GA correction (Step 2b).
#'
#'
NULL

#' Approximated Fenton CSD Values (M, S_upper, S_lower)
#'
#' Approximated Fenton CSD values: approximated reference values derived from
#' printed Fenton (2025) charts, for weight, length, and head circumference by
#' sex and gestational age in days. Contains the median (M), S_upper, and
#' S_lower, where S_upper x M is half the distance between the median and +2 SD
#' and S_lower x M is half the distance between the median and -2 SD. Used with
#' the CSD method for z-score calculation. These are approximations derived
#' from the charts, not values published by the Fenton authors. See
#' \code{vignette("wrapper")}, "Gestational-age reference values".
#'
#' @references Fenton TR, Elmrayed S, Alshaikh BN. Fenton Third-Generation
#'   Growth Charts of Preterm Infants Without Abnormal Fetal Growth: A
#'   Systematic Review and Meta-Analysis. Paediatr Perinat Epidemiol. 2025
#'   Aug;39(6):543-555. \doi{10.1111/ppe.70035}. Epub 2025 Jun 19. PMID:
#'   40534585; PMCID: PMC12391854.
#' @name fenton2025_ms_lookup_smoothed
#'
#' @section fenton2025_ms_lookup_smoothed.csv:
#'
#' Used in function `cleangrowth()` for GA correction (Step 2b).
#'
#'
NULL
