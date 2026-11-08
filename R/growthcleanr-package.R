#' growthcleanr: a data cleaner for anthropometric measurements
#'
#' growthcleanr flags implausible anthropometric (height, weight, and head
#' circumference) measurements in longitudinal datasets, such as those from
#' electronic health records. It also identifies other measurements that should
#' often be excluded from analyses.
#'
#' @section Getting started:
#' The main function is \code{\link{cleangrowth}}. Pass it a data frame with one
#' row per measurement and columns \code{subjid}, \code{param}, \code{agedays},
#' \code{sex}, and \code{measurement} (plus an optional per-row \code{id}).
#' Semi-wide and wide layouts, alternate column names, and date-of-birth /
#' visit-date ages are also supported. growthcleanr will not delete or edit
#' data: it returns your data with an added \code{exclude} column labeling each
#' row \code{"Include"} or an exclusion code naming why it recommends exclusion.
#'
#' The function \code{\link{growthcleanr_app}} opens a Shiny app that runs
#' growthcleanr without writing code. A command-line driver is documented in
#' \code{vignette("command-line")}.
#'
#' New to growthcleanr? Start with \code{vignette("new-start-here")}. See
#' \code{vignette("output")} for more information including the returned columns
#' and \code{vignette("configuration")} for the full index of arguments and thresholds.
#'
#' @name growthcleanr-package
#' @aliases growthcleanr
"_PACKAGE"
