# Command-line driver core for growthcleanr.
#
# Two pieces:
#   * run_gc_cli()  -- exported, testable core: CSV in -> cleangrowth() -> CSV out.
#   * a shared flag spec (.gc_cli_scalars/.gc_cli_flags_true/.gc_cli_flags_inverted/
#     .gc_cli_param_bases/.gc_cli_units) plus .gc_cli_opts(), which turn the parsed
#     command-line argument list into a typed `opts` list forwarded to cleangrowth().
# The thin argument shell lives in inst/scripts/gcdriver.R and builds its argparser
# parser FROM this spec, so the flag surface has a single source of truth.
#
# SSOT-for-defaults rule: only arguments the user actually supplied are forwarded;
# cleangrowth()'s own defaults are never re-declared here (they would drift).

#' Run growthcleanr from the command line (CSV in, CSV out)
#'
#' Testable core behind the \code{inst/scripts/gcdriver.R} command-line driver. Reads a
#' CSV, runs \code{\link{cleangrowth}} on it (data-frame style), and writes the
#' result to a CSV. Only the options in \code{opts} are forwarded, so
#' \code{cleangrowth()}'s own defaults apply to everything else.
#'
#' @param infile Path to the input CSV. Read with \code{data.table::fread}.
#'   Must use the canonical column names (\code{subjid}, \code{param},
#'   \code{agedays}, \code{sex}, \code{measurement}, optional \code{id}) unless
#'   remapped via the column-name options in \code{opts}.
#' @param outfile Path to write the cleaned result to, via
#'   \code{data.table::fwrite}. Overwritten if it exists.
#' @param opts Named list of arguments forwarded verbatim to
#'   \code{cleangrowth()} (already coerced to the right types). Typically built
#'   from command-line flags by the driver; may be supplied directly in tests or
#'   from other R code. An empty list runs \code{cleangrowth()} with all defaults.
#' @return The output path, invisibly.
#' @seealso \code{\link{cleangrowth}}; the command-line driver
#'   \code{inst/scripts/gcdriver.R}.
#' @examples
#' \dontrun{
#' run_gc_cli("visits.csv", "cleaned.csv",
#'            opts = list(adult_cutpoint = 18, quietly = TRUE))
#' }
#' @export
run_gc_cli <- function(infile, outfile, opts = list()) {
  if (!is.character(infile) || length(infile) != 1L)
    stop("`infile` must be a single file path.")
  if (!is.character(outfile) || length(outfile) != 1L)
    stop("`outfile` must be a single file path.")
  if (!file.exists(infile))
    stop(sprintf("input file not found: %s", infile))
  if (!is.list(opts))
    stop("`opts` must be a list of cleangrowth() arguments.")

  # Fail fast on I/O hazards, BEFORE the (potentially long) clean:
  #  * writing the output back over the input silently destroys the source data;
  #  * an unwritable/missing output directory would otherwise surface only after
  #    the whole run, throwing away the work.
  if (normalizePath(infile,  mustWork = FALSE) ==
      normalizePath(outfile, mustWork = FALSE))
    stop(sprintf("input and output paths are the same file (%s); choose a different output path so the input is not overwritten.",
                 outfile))
  outdir <- dirname(outfile)
  if (!dir.exists(outdir))
    stop(sprintf("output directory does not exist: %s. Create it first, or choose an existing directory.", outdir))
  if (file.access(outdir, mode = 2L) != 0L)
    stop(sprintf("output directory is not writable: %s.", outdir))

  df <- data.table::fread(infile)
  if (nrow(df) == 0L)
    stop(sprintf("input file has no data rows: %s", infile))

  res <- do.call(cleangrowth, c(list(data = df), opts))
  data.table::fwrite(res, outfile)
  invisible(outfile)
}

#' Path to the growthcleanr command-line driver script
#'
#' Returns the location of the installed \code{gcdriver.R} command-line driver,
#' so it can be invoked with \code{Rscript} without hunting for the file:
#' \preformatted{DRIVER=$(Rscript -e 'cat(growthcleanr::gc_cli_path())')
#' Rscript "$DRIVER" input.csv output.csv --quiet}
#'
#' @return The absolute path to \code{gcdriver.R} in the installed package.
#' @seealso \code{\link{run_gc_cli}}; \code{vignette("command-line")}.
#' @export
gc_cli_path <- function() {
  p <- system.file("scripts", "gcdriver.R", package = "growthcleanr")
  if (!nzchar(p))
    stop("gcdriver.R not found in the installed growthcleanr package; reinstall growthcleanr.")
  p
}

# ---- Shared flag spec ------------------------------------------------------
# Base parameter tokens the per-parameter Tier-2 flags cover, each with its two
# unit-appropriate canonical keys. The Tier-2 name flags (--param-*, --semiwide-col-*,
# --wide-*) carry only the base token (e.g. --param-weight); the unit is declared
# separately by a --*-units flag, and the driver combines the two into the
# canonical key gc expects (metric key or imperial key). This mirrors the Shiny
# app, which pairs a column/value field with a units selector per parameter.
.gc_cli_param_bases <- function() {
  list(
    weight = list(units_flag = "weight-units", keys = c(kg = "WEIGHTKG",  lbs  = "WEIGHTLBS")),
    height = list(units_flag = "height-units", keys = c(cm = "HEIGHTCM",  `in` = "HEIGHTIN")),
    length = list(units_flag = "length-units", keys = c(cm = "LENGTHCM",  `in` = "LENGTHIN")),
    head   = list(units_flag = "head-units",   keys = c(cm = "HEADCM",    `in` = "HEADIN"))
  )
}

# Units flags: flag-name -> list(choices, default (metric = first choice), help).
# These do NOT map to a cleangrowth arg; they qualify the Tier-2 name flags above,
# and .gc_cli_opts() consumes them when assembling param_coding/semiwide_cols/wide_cols.
.gc_cli_units <- function() {
  bases <- .gc_cli_param_bases()
  out <- list()
  for (b in names(bases)) {
    ch <- names(bases[[b]]$keys)  # e.g. c("kg","lbs") or c("cm","in")
    out[[bases[[b]]$units_flag]] <- list(
      choices = ch,
      default = ch[[1L]],
      help = sprintf("Units for the --param-%s / --semiwide-col-%s / --wide-%s column: %s (default %s). growthcleanr converts imperial to metric internally.",
                     b, b, b, paste(ch, collapse = "/"), ch[[1L]])
    )
  }
  out
}

# Scalar / value flags: flag-name -> list(arg = cleangrowth arg, type, help).
# `type` is one of "numeric", "integer", "character", "csv" (comma list).
.gc_cli_scalars <- function() {
  list(
    "adult-cutpoint"       = list(arg = "adult_cutpoint",       type = "numeric",   help = "Age (years) dividing pediatric/adult processing; clamped to [18,20]."),
    "adult-permissiveness" = list(arg = "adult_permissiveness", type = "character", choices = c("loosest", "looser", "tighter", "tightest"), help = "Adult preset: loosest/looser/tighter/tightest."),
    "adult-scale-max-lbs"  = list(arg = "adult_scale_max_lbs",  type = "numeric",   help = "Physical scale upper limit in pounds (accepts Inf)."),
    "cf-rescue"            = list(arg = "cf_rescue",            type = "character", choices = c("standard", "none", "all"), help = "Carried-forward rescue mode: standard/none/all."),
    "ewma-window"          = list(arg = "ewma_window",          type = "integer",   help = "EWMA window (neighbors per side)."),
    "piv-z-wt-high"        = list(arg = "piv.z.wt.high",        type = "numeric",   help = "Child upper WEIGHTKG z cutoff."),
    "piv-z-ht-low-young"   = list(arg = "piv.z.ht.low.young",   type = "numeric",   help = "Child lower HEIGHTCM z cutoff, age <1y."),
    "piv-z-ht-low-old"     = list(arg = "piv.z.ht.low.old",     type = "numeric",   help = "Child lower HEIGHTCM z cutoff, age >=1y."),
    "piv-z-ht-high"        = list(arg = "piv.z.ht.high",        type = "numeric",   help = "Child upper HEIGHTCM z cutoff."),
    "piv-z-hc-low"         = list(arg = "piv.z.hc.low",         type = "numeric",   help = "Child lower HEADCM z cutoff."),
    "piv-z-hc-high"        = list(arg = "piv.z.hc.high",        type = "numeric",   help = "Child upper HEADCM z cutoff."),
    "input-format"         = list(arg = "input_format",         type = "character", choices = c("long", "semi_wide", "wide"), help = "Input shape: long/semi_wide/wide."),
    "output-format"        = list(arg = "output_format",        type = "character", choices = c("long", "semi_wide", "wide"), help = "Output shape: long/semi_wide/wide (default mirrors input)."),
    "widened-emit"         = list(arg = "widened_emit",         type = "csv",       help = "Comma list of gc columns to add to re-widened output."),
    "dob"                  = list(arg = "dob",                  type = "character", help = "Date-of-birth column name (with --visit-date)."),
    "visit-date"           = list(arg = "visit_date",           type = "character", help = "Visit-date column name (with --dob)."),
    "age-source"           = list(arg = "age_source",           type = "character", choices = c("agedays", "dates"), help = "Age source: agedays/dates (default auto)."),
    "date-order"           = list(arg = "date_order",           type = "character", choices = c("dmy", "mdy"), help = "Ambiguous numeric date order: dmy/mdy."),
    "batch-size"           = list(arg = "batch_size",           type = "integer",   help = "Subjects per in-memory batch."),
    "cores"                = list(arg = "cores",                type = "integer",   help = "Worker cores when --parallel (default auto)."),
    "log-path"             = list(arg = "log.path",             type = "character", help = "Deprecated; use --log-file."),
    "checkpoint-dir"       = list(arg = "checkpoint_dir",       type = "character", help = "Persistent, resumable checkpoint directory (checkpointing is on by default to a temp dir; --no-checkpoint disables it)."),
    "checkpoint-size"      = list(arg = "checkpoint_size",      type = "integer",   help = "Subjects per checkpoint."),
    "log-file"             = list(arg = "log_file",             type = "character", help = "Checkpoint progress log path (optional)."),
    "subjid-col"           = list(arg = "subjid",               type = "character", help = "Column holding subject id (if not 'subjid')."),
    "param-col"            = list(arg = "param",                type = "character", help = "Column holding parameter (if not 'param')."),
    "agedays-col"          = list(arg = "agedays",              type = "character", help = "Column holding age in days (if not 'agedays')."),
    "sex-col"              = list(arg = "sex",                  type = "character", help = "Column holding sex (if not 'sex')."),
    "measurement-col"      = list(arg = "measurement",          type = "character", help = "Column holding measurement (if not 'measurement')."),
    "id-col"               = list(arg = "id",                   type = "character", help = "Column holding per-row id (if not 'id').")
  )
}

# Boolean flags that set their cleangrowth arg to TRUE when present.
.gc_cli_flags_true <- function() {
  c(
    "length-adjust"     = "length.adjust",
    "exclude-detail"    = "exclude_detail",
    "cf-detail"         = "cf_detail",
    "corr-detail"       = "corr_detail",
    "tri-exclude"       = "tri_exclude",
    "full-detail"       = "full_detail",
    "parallel"          = "parallel",
    "checkpoint-resume" = "checkpoint_resume",
    "verbose"           = "verbose"
  )
}

# Inverted boolean flags: present -> set arg to the given value (a default-TRUE
# cleangrowth arg turned off, or --quiet turning quietly on).
.gc_cli_flags_inverted <- function() {
  list(
    "no-unit-error-range" = list(arg = "unit_error_range", value = FALSE),
    "no-mean-ht"          = list(arg = "mean_ht",          value = FALSE),
    "no-checkpoint"       = list(arg = "checkpoint",       value = FALSE),
    "no-keep-checkpoints" = list(arg = "keep_checkpoints", value = FALSE),
    "no-carry-through"    = list(arg = "carry_through",    value = FALSE),
    "quiet"               = list(arg = "quietly",          value = TRUE)
  )
}

# ---- Coercion + assembly ---------------------------------------------------
# Coerce a single raw string flag value to its typed form, with a clear error.
.gc_cli_coerce <- function(x, type, flag) {
  x <- as.character(x)
  if (type == "csv") {
    v <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
    v <- v[nzchar(v)]
    if (length(v) == 0L)
      stop(sprintf("--%s: no values after splitting on ','.", flag))
    return(v)
  }
  if (type == "character") return(x)
  if (type == "numeric") {
    if (identical(x, "Inf")) return(Inf)
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n))
      stop(sprintf("--%s: expected a number, got '%s'.", flag, x))
    return(n)
  }
  if (type == "integer") {
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n) || n <= 0 || n != round(n))
      stop(sprintf("--%s: expected a positive whole number, got '%s'.", flag, x))
    return(as.integer(n))
  }
  stop(sprintf("--%s: internal error, unknown flag type '%s'.", flag, type))
}

# Turn a parsed argument list (named by underscore, as argparser exposes it)
# into the typed `opts` list for cleangrowth(). Only supplied flags are added.
.gc_cli_opts <- function(argv) {
  opts <- list()
  akey <- function(flag) gsub("-", "_", flag, fixed = TRUE)
  # A value flag is "present" when it is a non-NA, non-empty scalar.
  present <- function(flag) {
    v <- argv[[akey(flag)]]
    !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(as.character(v))
  }
  raw <- function(flag) as.character(argv[[akey(flag)]])

  # Scalars / value flags.
  scal <- .gc_cli_scalars()
  for (flag in names(scal)) {
    if (present(flag)) {
      sp <- scal[[flag]]
      val <- .gc_cli_coerce(raw(flag), sp$type, flag)
      # Choice values ignore case and surrounding spaces, as cleangrowth() does
      # for adult_permissiveness (every choice list is lowercase).
      if (!is.null(sp$choices) && is.character(val)) val <- tolower(trimws(val))
      if (!is.null(sp$choices) && !all(val %in% sp$choices))
        stop(sprintf("--%s: expected one of %s; got '%s'.",
                     flag, paste(sp$choices, collapse = "/"), raw(flag)))
      opts[[sp$arg]] <- val
    }
  }

  # Plain boolean flags (TRUE when present).
  ft <- .gc_cli_flags_true()
  for (flag in names(ft)) {
    if (isTRUE(argv[[akey(flag)]])) opts[[ft[[flag]]]] <- TRUE
  }

  # Inverted boolean flags.
  fi <- .gc_cli_flags_inverted()
  for (flag in names(fi)) {
    if (isTRUE(argv[[akey(flag)]])) opts[[fi[[flag]]$arg]] <- fi[[flag]]$value
  }

  # Tier-2: sex_coding (both keys required together).
  male   <- if (present("sex-male"))   .gc_cli_coerce(raw("sex-male"),   "csv", "sex-male")
  female <- if (present("sex-female")) .gc_cli_coerce(raw("sex-female"), "csv", "sex-female")
  if (!is.null(male) || !is.null(female)) {
    if (is.null(male) || is.null(female))
      stop("--sex-male and --sex-female must be supplied together (each replaces one side of the coding).")
    opts$sex_coding <- list(male = male, female = female)
  }

  # Tier-2: per-parameter column/value maps. Each base param (weight/height/
  # length/head) has a name flag per format group; a separate --*-units flag
  # selects the unit-appropriate canonical key. Resolve the unit first (defaulting
  # to metric), then assemble param_coding / semiwide_cols / wide_cols.
  bases <- .gc_cli_param_bases()
  unit_key <- character(0)  # base -> canonical key given the chosen unit
  for (b in names(bases)) {
    ch <- names(bases[[b]]$keys)
    uf <- bases[[b]]$units_flag
    u  <- if (present(uf)) raw(uf) else ch[[1L]]  # default = metric
    if (!u %in% ch)
      stop(sprintf("--%s: expected one of %s; got '%s'.",
                   uf, paste(ch, collapse = "/"), u))
    unit_key[[b]] <- bases[[b]]$keys[[u]]
  }
  groups <- list(
    list(prefix = "param",        arg = "param_coding",  type = "csv"),
    list(prefix = "semiwide-col", arg = "semiwide_cols", type = "character"),
    list(prefix = "wide",         arg = "wide_cols",     type = "character")
  )
  for (g in groups) {
    lst <- list()
    for (b in names(bases)) {
      flag <- paste0(g$prefix, "-", b)
      if (present(flag)) lst[[unit_key[[b]]]] <- .gc_cli_coerce(raw(flag), g$type, flag)
    }
    if (length(lst)) opts[[g$arg]] <- lst
  }

  # --widened-emit only affects re-widened (semi-wide / wide) output. The output
  # shape defaults to the input shape, which defaults to long.
  if (!is.null(opts$widened_emit)) {
    out_fmt <- if (!is.null(opts$output_format)) opts$output_format
               else if (!is.null(opts$input_format)) opts$input_format
               else "long"
    if (identical(out_fmt, "long")) {
      # Check the value anyway, so an invalid one (e.g. a 2.x `-w 181.4` rewritten
      # as --widened-emit 181.4) stops here as it would with wide output.
      .gc_normalize_emit(opts$widened_emit)
      warning("--widened-emit has no effect with long output; it applies only when the output is semi-wide or wide.",
              call. = FALSE)
    }
  }

  opts
}

# ---- Argument pre-checks (run by the driver before argparser) --------------
# argparser gives every flag an automatic one-letter short form (its first
# letter) and has no switch to turn that off. Several of those letters meant
# something else in growthcleanr 2.x (-w weight cap, -s sdrecenter, -n
# numbatches), so an old command could run with a different meaning. The driver
# therefore accepts long flags only: any single-dash letter token other than -h
# (help) is rejected. Negative numbers (e.g. -25 as a value) are allowed.
# Returns an error message, or NULL when the arguments are fine.
.gc_cli_check_short <- function(args) {
  bad <- args[grepl("^-[^-]", args) & !grepl("^-[0-9.]", args) & args != "-h"]
  if (length(bad) == 0L) return(NULL)
  sprintf("growthcleanr's command-line driver accepts only long flags (for example --adult-scale-max-lbs); %s %s not accepted. Short flags such as -w, -s and -n had different meanings in growthcleanr 2.x. See vignette(\"command-line\").",
          paste0("'", bad, "'", collapse = ", "),
          if (length(bad) > 1L) "are" else "is")
}

# Pointer appended to argparser's unknown-flag error, which is what an old 2.x
# long flag (e.g. --weightcap, --sdrecenter) produces. argparser prints "'--x' is
# not a defined optional argument or flag" to stderr itself; the condition
# message it then raises is "Undefined argument labels supplied".
.gc_cli_unknown_flag_hint <- function(msg) {
  if (grepl("Undefined argument labels|not a defined optional argument", msg))
    paste0(msg, "\nIf this command was written for growthcleanr 2.x, some flags were renamed or removed; see vignette(\"command-line\") for the current flags (or run the driver with --help).")
  else msg
}

# Build the full list of Tier-2 string-mapping flag names (long form, without "--"):
# the sex-coding pair plus the per-base-param name flags for each format group.
# The --*-units flags are NOT here -- they carry choices/defaults and are added by
# the shell from .gc_cli_units(). Kept here so inst/scripts/gcdriver.R stays a thin builder.
.gc_cli_tier2_flags <- function() {
  bases <- names(.gc_cli_param_bases())  # weight, height, length, head
  c("sex-male", "sex-female",
    paste0("param-", bases),
    paste0("semiwide-col-", bases),
    paste0("wide-", bases))
}
