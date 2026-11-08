#!/usr/bin/env Rscript
#
# growthcleanr command-line driver: CSV in, CSV out.
#
#   Rscript "$(Rscript -e 'cat(growthcleanr::gc_cli_path())')" input.csv output.csv [--flags]
#
# It ships in the installed package at inst/scripts/gcdriver.R; gc_cli_path()
# returns its resolved location.
#
# This is a THIN shell. It builds an argparser parser from the shared flag spec
# in R/cli.R, parses the command line, turns it into a typed options list
# (growthcleanr:::.gc_cli_opts), and hands off to growthcleanr::run_gc_cli().
# No cleaning logic lives here. See vignette("command-line").

suppressPackageStartupMessages({
  library(argparser)
  library(data.table)
  library(growthcleanr)
})

p <- arg_parser("growthcleanr command-line driver (CSV in, CSV out)")

# Positional arguments.
p <- add_argument(p, "infile",  help = "input CSV path")
p <- add_argument(p, "outfile", help = "output CSV path")

# argparser auto-generates single-letter short flags and warns when they
# collide; we rely on the long flags only, so silence that noise.
suppressWarnings({
  # Scalar / value flags (spec-driven; unset -> NA).
  scal <- growthcleanr:::.gc_cli_scalars()
  for (flag in names(scal)) {
    p <- add_argument(p, paste0("--", flag), help = scal[[flag]]$help, default = NA)
  }

  # Boolean flags (present -> TRUE).
  ft <- growthcleanr:::.gc_cli_flags_true()
  for (flag in names(ft)) {
    p <- add_argument(p, paste0("--", flag), flag = TRUE,
                      help = sprintf("Set %s = TRUE.", ft[[flag]]))
  }

  # Inverted / off-switch boolean flags.
  fi <- growthcleanr:::.gc_cli_flags_inverted()
  for (flag in names(fi)) {
    p <- add_argument(p, paste0("--", flag), flag = TRUE,
                      help = sprintf("Set %s = %s.", fi[[flag]]$arg, fi[[flag]]$value))
  }

  # Tier-2 per-key flags (sex coding, param/semiwide/wide column maps; unset -> NA).
  for (flag in growthcleanr:::.gc_cli_tier2_flags()) {
    p <- add_argument(p, paste0("--", flag),
                      help = "Column/label mapping (see the command-line vignette).",
                      default = NA)
  }

  # Units flags qualifying the per-param name flags above (choices + metric default).
  un <- growthcleanr:::.gc_cli_units()
  for (flag in names(un)) {
    p <- add_argument(p, paste0("--", flag), help = un[[flag]]$help,
                      default = un[[flag]]$default)
  }
})

# parse_args() throws on argument-parsing errors (unknown flag, missing value)
# BEFORE the main tryCatch below, so wrap it too -- otherwise those surface as a
# raw R stack trace instead of the clean "gcdriver error:" message. argparser
# still prints its own specific detail line (e.g. which flag) to stderr first.
# Long flags only: reject single-dash letter flags before argparser sees them
# (argparser auto-assigns first-letter short forms that meant other things in
# growthcleanr 2.x; see growthcleanr:::.gc_cli_check_short).
.short_err <- growthcleanr:::.gc_cli_check_short(commandArgs(trailingOnly = TRUE))
if (!is.null(.short_err)) {
  message("gcdriver error: ", .short_err)
  quit(status = 1L)
}

argv <- tryCatch(parse_args(p), error = function(e) {
  message("gcdriver error: ",
          growthcleanr:::.gc_cli_unknown_flag_hint(conditionMessage(e)))
  quit(status = 1L)
})

status <- tryCatch({
  opts <- growthcleanr:::.gc_cli_opts(argv)
  if (isTRUE(argv$verbose)) {
    message("gcdriver: ", argv$infile, " -> ", argv$outfile)
    message("gcdriver: forwarding ", length(opts), " option(s) to cleangrowth().")
  }
  run_gc_cli(argv$infile, argv$outfile, opts)
  0L
}, error = function(e) {
  message("gcdriver error: ", conditionMessage(e))
  1L
})

quit(status = status)
