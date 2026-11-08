# Internal reshape adapters: accept semi-wide and wide input and reshape it to
# the canonical long form BEFORE the cleangrowth() funnel, so the entire cleaning
# algorithm runs unchanged on long data. Data.table-only, ASCII source.
#
# Nothing here is exported; all helpers are `.`-prefixed and live only in the
# package namespace. The only public surface is the new cleangrowth() arguments
# (input_format, output_format, semiwide_cols), which call in here at "Hook A".

# Render an offending argument value for an error message: the value itself for
# a short atomic scalar/vector, otherwise a class+length description. Keeps
# messages informative ("25 was entered") without dumping large objects.
.gc_desc_val <- function(x) {
  if (length(x) == 0L) return("(empty)")
  if (length(x) <= 3L && (is.character(x) || is.numeric(x) || is.logical(x) || is.factor(x))) {
    return(paste(as.character(x), collapse = ", "))
  }
  sprintf("a %s of length %d", paste(class(x), collapse = "/"), length(x))
}

# Recognized growthcleanr parameter strings a value column may map to. Imperial
# and length variants are accepted here and converted to metric later in the
# funnel; the adapter only emits the param string, never touches units.
.gc_recognized_params <- c("WEIGHTKG", "HEIGHTCM", "LENGTHCM",
                           "WEIGHTLBS", "HEIGHTIN", "LENGTHIN", "HEADCM", "HEADIN")

# Imperial -> metric conversion and linear-parameter handling, as a single
# rule. Returns gc's internal (working) param + measurement for each row.
#
# Two stages, always in this order:
#   "imperial" -- HEIGHTIN -> HEIGHTCM (x 2.54), LENGTHIN -> LENGTHCM (x 2.54),
#                 HEADIN -> HEADCM (x 2.54), WEIGHTLBS -> WEIGHTKG
#                 (/ 2.2046226). Units first, so the linear stage below sees
#                 centimeters no matter how the value arrived (a LENGTHIN then
#                 rides the LENGTHCM path through the linear stage).
#   "linear"   -- the optional recumbent/standing offset (both directions,
#                 see below), then LENGTHCM -> HEIGHTCM relabeling.
# `stage = "all"` runs both; the pediatric preprocessing calls the stages
# separately because other work sits between them.
#
# The offset (length.adjust = TRUE) reconciles how a linear measurement was
# entered with what the reference at that age expects. Recumbent length runs
# about 0.7 cm longer than standing height for the same child, and the WHO/CDC
# reference files are length-based through day 729 and height-based from day
# 730 (.gc_ht_start_day) onward:
#   - entered as LENGTHCM at day 730 or later -> -0.7 (to standing height)
#   - entered as HEIGHTCM before day 730      -> +0.7 (to recumbent length)
# Rows with a missing age are left alone. The offset is returned separately
# (`off`) and is NOT applied to the value: it is used only where a value is
# z-scored against a reference, so the raw-cm checks (carried-forward and
# same-day identical detection, velocity, hard limits) always see the entered
# value. The 0.7 cm is .gc_len_offset, shared
# with the Child Step 2b gestational-age correction (see .compute_corrected_z()
# and the GA-window adjustment in cleangrowth()), so the two cancel (to
# floating-point precision).
#
# Single source of truth for this arithmetic: the pediatric preprocessing site
# in cleangrowth() calls this, cleanadult() applies the same imperial factors to
# its own meas_m, and the optional gc_internal_meas / gc_internal_param output
# columns are built from it.
# Recumbent-length minus standing-height offset (cm), used everywhere gc
# reconciles length and height: the length.adjust reconciliation below and the
# Child Step 2b standing/recumbent offset. One value for both WHO and CDC (WHO
# uses 0.7 cm; CDC's methods report uses 0.8 cm; gc uses 0.7 for consistency).
.gc_len_offset <- 0.7

# First age (in days) at which a linear measurement is height rather than
# length. The WHO and CDC reference files switch from length to height values
# at day 730, so every length/height decision in gc uses the same day: the
# length.adjust reconciliation below, the Child Step 2b gestational-age window,
# and the semi-wide/wide LENGTHCM-at-older-ages check. (Child Step 17's WHO
# velocity range is intentionally different; see the comment there.)
.gc_ht_start_day <- 730

.gc_to_metric <- function(param, v, length.adjust = FALSE, agedays = NULL,
                          stage = c("all", "imperial", "linear")) {
  stage <- match.arg(stage)
  p <- as.character(param)
  x <- as.numeric(v)
  off <- numeric(length(x))  # z-score-only recumbent/standing offset (cm)

  if (stage %in% c("all", "imperial")) {
    hin <- !is.na(p) & p == "HEIGHTIN"
    lin <- !is.na(p) & p == "LENGTHIN"
    din <- !is.na(p) & p == "HEADIN"
    wlb <- !is.na(p) & p == "WEIGHTLBS"
    x[hin] <- x[hin] * 2.54
    x[lin] <- x[lin] * 2.54
    x[din] <- x[din] * 2.54
    x[wlb] <- x[wlb] / 2.2046226
    p[hin] <- "HEIGHTCM"
    p[lin] <- "LENGTHCM"
    p[din] <- "HEADCM"
    p[wlb] <- "WEIGHTKG"
  }

  if (stage %in% c("all", "linear")) {
    lcm <- !is.na(p) & p == "LENGTHCM"
    hcm <- !is.na(p) & p == "HEIGHTCM"
    if (isTRUE(length.adjust) && !is.null(agedays)) {
      ht_age  <- !is.na(agedays) & agedays >= .gc_ht_start_day
      len_age <- !is.na(agedays) & !ht_age
      off[lcm & ht_age]  <- -.gc_len_offset
      off[hcm & len_age] <- .gc_len_offset
    }
    p[lcm] <- "HEIGHTCM"
  }

  list(param = p, v = x, off = off)
}

# Coarse parameter group, used only for the "relevant parameter missing"
# warnings (weight / height-or-length / head circumference).
.gc_param_group <- function(p) {
  if (p %in% c("WEIGHTKG", "WEIGHTLBS")) return("weight")
  if (p %in% c("HEIGHTCM", "HEIGHTIN", "LENGTHCM", "LENGTHIN")) return("height")
  if (p %in% c("HEADCM", "HEADIN")) return("hc")
  NA_character_
}

# Fixed within-visit slot rank for deterministic id emission: report weight
# before height before head circumference (any imperial/length variant sorts
# with its metric target).
.gc_param_rank <- function(p) {
  data.table::fifelse(
    p %in% c("WEIGHTKG", "WEIGHTLBS"), 1L,
    data.table::fifelse(p %in% c("HEIGHTCM", "HEIGHTIN", "LENGTHCM", "LENGTHIN"), 2L, 3L))
}

# Flexible sex / param input coding ----------------------------------------
#
# Both features normalize tokens the same way -- coerce to character, trim
# whitespace, tolower -- so "Male", " M ", 0 and "0" all match, and neither ever
# guesses an ambiguous code.

# Token normalizer shared by both recoders: character, trimmed, lowercased.
# Genuine NA stays NA (as.character(NA) is NA_character_); an empty/blank string
# normalizes to "" (treated as missing, never matched to a token).
.gc_norm <- function(x) tolower(trimws(as.character(x)))

# Appended to the missing-sex-column errors (long, semi-wide, wide): the column
# is always required, even for adult-only data, whose sex values may be blank/NA.
.gc_sex_col_note <- " A sex column is always required, but the sex column value may be blank or NA for adults (ages at or above adult_cutpoint)."

# Default sex coding. male = 0, female = 1 (growthcleanr's convention), plus the
# common word/letter spellings. 2 is deliberately absent so a 1=male/2=female
# scheme trips the guard instead of silently misreading the 1s as female.
.gc_default_sex_coding <- list(
  male   = c(0, "m", "b", "male", "boy"),
  female = c(1, "f", "g", "female", "girl")
)

# Recode a submitted sex vector to gc's integer sex (0 = male, 1 = female,
# NA = missing/unrecognized), using the (default or user) sex_coding table.
# Matching is normalized on both sides. Errors if a token is listed under both
# sexes. Present-but-unrecognized values become NA here (they are set aside as
# Exclude-Missing-Info downstream); the caller warns about them.
.gc_recode_sex <- function(sex, sex_coding = .gc_default_sex_coding) {
  if (!is.list(sex_coding) || !all(c("male", "female") %in% names(sex_coding))) {
    stop("`sex_coding` must be a list with `male` and `female` token vectors, ",
         "e.g. list(male = c(0, \"m\", \"male\"), female = c(1, \"f\", \"female\")).")
  }
  male_tok   <- .gc_norm(sex_coding$male)
  female_tok <- .gc_norm(sex_coding$female)
  both <- intersect(male_tok, female_tok)
  if (length(both) > 0L) {
    stop(sprintf("Token(s) mapped to both male and female in `sex_coding`: %s.",
                 paste(both, collapse = ", ")))
  }
  sx  <- .gc_norm(sex)
  out <- rep(NA_integer_, length(sx))
  out[sx %in% male_tok]   <- 0L
  out[sx %in% female_tok] <- 1L
  out
}

# Build the normalized label -> canonical lookup: each canonical parameter maps
# from its own (case-insensitive) name, then from the user's `param_coding`.
# growthcleanr recognizes ONLY the spelled-out canonical parameter names
# (WEIGHTKG, HEIGHTCM, LENGTHCM, HEADCM, and the imperial WEIGHTLBS/HEIGHTIN/
# LENGTHIN/HEADIN) -- there are deliberately NO built-in bare synonyms (no wt/ht/hc/
# weight/head/...): a short abbreviation like "wt" collides with columns that
# are not body weight (statistical sample weights, etc.), so gc will not guess.
# A non-canonical label must be declared via `param_coding` (which defaults to
# a metric target unless the user maps it to an imperial canonical). Validates
# the user list. Returned as a named character vector keyed by normalized token.
.gc_param_coding_map <- function(param_coding = NULL) {
  canon <- .gc_recognized_params
  map <- stats::setNames(canon, .gc_norm(canon))
  if (!is.null(param_coding)) {
    if (!is.list(param_coding) || is.null(names(param_coding)) ||
        any(names(param_coding) == "")) {
      stop("`param_coding` must be a named list keyed by canonical parameter, ",
           "e.g. list(WEIGHTKG = c(\"WT\", \"Weight\"), HEIGHTIN = \"HT\").")
    }
    bad <- setdiff(names(param_coding), canon)
    if (length(bad) > 0L) {
      stop(sprintf("`param_coding` names must be canonical parameters (%s); unknown: %s.",
                   paste(canon, collapse = ", "), paste(bad, collapse = ", ")))
    }
    seen <- character(0)  # normalized token -> target, to catch cross-target conflicts
    for (tgt in names(param_coding)) {
      for (a in .gc_norm(param_coding[[tgt]])) {
        if (a %in% names(seen) && seen[[a]] != tgt) {
          stop(sprintf("Label '%s' maps to both %s and %s in `param_coding`.",
                       a, seen[[a]], tgt))
        }
        seen[a] <- tgt
        map[a] <- tgt
      }
    }
  }
  map
}

# Resolve a submitted param vector to gc's canonical parameter tokens using the
# param_coding map. Recognized tokens (incl. mapped labels) become their canonical form;
# unrecognized values are returned unchanged (verbatim), so they fall through to
# the Exclude-Not-GC-Param holdout. The user's verbatim param is preserved
# separately (param_input) and restored at output.
.gc_recode_param <- function(param, param_coding = NULL) {
  map <- .gc_param_coding_map(param_coding)
  p   <- .gc_norm(param)
  out <- as.character(param)
  hit <- p %in% names(map)
  out[hit] <- unname(map[p[hit]])
  out
}

# Date-input parsing. Broadens the accepted date formats beyond strict ISO
# YYYY-MM-DD without ever silently misparsing an ambiguous date.
#
# Three format classes (a run resolves under exactly ONE, jointly across dob and
# visit_date):
#   "ymd"     year-first numeric: 2013-03-04, 2013/03/04, 2013.03.04, 20130304
#   "spelled" English spelled-month: 03mar2013, 15-Mar-2024, "March 15, 2024"
#   "num_dm"  numeric day/month: 3/4/2013, 03-04-2013 -- REQUIRES date_order
# ymd and spelled are order-free (unambiguous); num_dm needs date_order = "dmy"
# or "mdy". The year must be four digits beginning 19 or 20 -- two-digit years
# are rejected and the range is bounded to 1900-2099.

# Classify a pooled character vector of date strings into one format class,
# enforcing the year guard, single-class homogeneity, and the num_dm-needs-order
# rule. Returns the class string, or NULL when there is nothing to classify (all
# blank / no character values -- e.g. Date-typed columns). `arg_label` names the
# source for error messages (e.g. "dob/visit_date").
.gc_date_class <- function(x, date_order, arg_label = "date") {
  if (is.null(x)) return(NULL)
  trimmed <- trimws(as.character(x))
  # Values with no digits ("NA", "N/A", "NULL", ".", "unknown") are treated as
  # blank: they cannot be misread as a different date.
  vals <- trimmed[!.gc_date_is_blank(trimmed)]
  if (length(vals) == 0L) return(NULL)

  .egs <- function(v) {
    u <- unique(v)
    sprintf("%s%s", paste0('"', u[seq_len(min(5L, length(u)))], '"', collapse = ", "),
            if (length(u) > 5L) ", ..." else "")
  }

  # Year guard: every value must carry a four-digit 19xx/20xx year run. This is
  # what rejects two-digit years (lubridate would otherwise pivot them silently).
  has_year <- grepl("(19|20)[0-9]{2}", vals)
  if (!all(has_year)) {
    stop(sprintf(
      "`%s` has %d date value(s) without a four-digit year in 1900-2099 (e.g. %s). Two-digit years are not accepted; use a four-digit year, e.g. \"2013-03-04\".",
      arg_label, sum(!has_year), .egs(vals[!has_year])))
  }

  # Classify each value by pattern (not by whether it happens to parse), so a
  # content-unambiguous numeric like "25/12/2013" still requires date_order.
  ymd     <- grepl("^(19|20)[0-9]{2}([-/.][0-9]{1,2}){2}$", vals) |
             grepl("^(19|20)[0-9]{6}$", vals)
  spelled <- grepl("[A-Za-z]", vals)
  num_dm  <- grepl("^[0-9]{1,2}[-/.][0-9]{1,2}[-/.](19|20)[0-9]{2}$", vals)
  cls <- ifelse(ymd, "ymd",
                ifelse(spelled, "spelled",
                       ifelse(num_dm, "num_dm", NA_character_)))

  if (anyNA(cls)) {
    stop(sprintf(
      "`%s` has %d date value(s) in an unrecognized format (e.g. %s). Use year-first (\"2013-03-04\"), English spelled-month (\"15 Mar 2013\"), or numeric day/month with `date_order` (\"04/03/2013\").",
      arg_label, sum(is.na(cls)), .egs(vals[is.na(cls)])))
  }

  classes <- unique(cls)
  if (length(classes) > 1L) {
    egs <- vapply(classes, function(k) vals[cls == k][1L], character(1))
    stop(sprintf(
      "`%s` mixes more than one date format in the same run (%s). All dates in dob and visit_date must share one format; reformat so every date uses the same format.",
      arg_label,
      paste(sprintf("%s e.g. \"%s\"", classes, egs), collapse = "; ")))
  }

  if (identical(classes, "num_dm") && is.null(date_order)) {
    stop(sprintf(
      "`%s` uses numeric day/month dates (e.g. \"%s\") whose day-vs-month order is ambiguous. Set `date_order = \"dmy\"` or `date_order = \"mdy\"` to declare it.",
      arg_label, vals[1L]))
  }
  classes
}

# TRUE for a date value treated as blank: NA, empty, or text with no digits
# (e.g. "NA", "N/A", "NULL", ".", "unknown"). Such a value cannot be misread as a
# different date, so it is set aside like a blank rather than stopping the run.
.gc_date_is_blank <- function(trimmed) {
  is.na(trimmed) | trimmed == "" | !grepl("[0-9]", trimmed)
}

# Parse a date column to R Date, given the resolved format `class` (from
# .gc_date_class()) and the declared `date_order`. Accepts a Date column as-is;
# rejects a numeric column outright (a bare date integer is epoch-dependent:
# different software count from different origin dates). Character/factor
# values are parsed via lubridate with the C (English) locale so spelled-month
# recognition is identical on every machine. Blank (incl. text with no digits) ->
# NA. A value that matches the run's format but is not a real date (e.g.
# "2013-02-30") is set to NA when such values are few (at most 1, or 5% of the
# column's non-blank values); more than that points to a format problem (e.g. a
# wrong date_order) and errors with examples. The returned Date vector carries
# attribute "gc_date_setaside" = list(token/token_vals = indices and values
# treated as blank because they had no digits, bad/bad_vals = indices and values
# of unreadable dates set aside), used by
# cleangrowth() to report them in the consolidated input warning. When `class` is NULL but
# non-blank text is present (single-column call), the column is classified alone.
#
# Shared single source of truth: called by the cleangrowth() date funnel and the
# wide adapter's internal age check.
.gc_parse_date_col <- function(x, arg_label, colname, class = NULL, date_order = NULL) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) {
    stop(sprintf(
      "`%s` column '%s' is numeric. Date columns must be text (e.g. \"2024-03-15\"), not a number: a date integer means different dates in different software (each counts from a different origin date), so it cannot be interpreted unambiguously. Export the column as text instead.",
      arg_label, colname))
  }
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) {
    stop(sprintf("`%s` column '%s' must be an R Date or a text (character) date column, but it is %s. Convert it to character (e.g. \"2024-03-15\") or an R Date.",
                 arg_label, colname, paste(class(x), collapse = "/")))
  }
  trimmed  <- trimws(x)
  is_token <- !is.na(trimmed) & trimmed != "" & !grepl("[0-9]", trimmed)
  is_blank <- .gc_date_is_blank(trimmed)
  parsed   <- rep(as.Date(NA), length(x))
  if (any(!is_blank)) {
    if (is.null(class)) {
      class <- .gc_date_class(trimmed[!is_blank], date_order, arg_label)
    }
    nb <- trimmed[!is_blank]
    if (identical(class, "spelled")) {
      # The month is a word, so day-first (dmy: "04mar2013") and month-first
      # (mdy: "March 4, 2013") are each unambiguous per value. But
      # parse_date_time trains a single order across the whole vector and
      # mis-resolves a mix, so parse each single order and take whichever hit.
      d1 <- suppressWarnings(lubridate::parse_date_time(nb, orders = "dmy", locale = "C", quiet = TRUE))
      d2 <- suppressWarnings(lubridate::parse_date_time(nb, orders = "mdy", locale = "C", quiet = TRUE))
      d1[is.na(d1)] <- d2[is.na(d1)]
      got <- d1
    } else {
      orders <- switch(class,
        ymd    = "ymd",
        num_dm = date_order,
        stop(sprintf("Internal error: unknown date class '%s'.", class)))
      got <- suppressWarnings(lubridate::parse_date_time(nb, orders = orders, locale = "C", quiet = TRUE))
    }
    parsed[!is_blank] <- as.Date(got, tz = "UTC")
  }
  bad <- which(!is_blank & is.na(parsed))
  if (length(bad) > 0L && length(bad) > max(1L, floor(0.05 * sum(!is_blank)))) {
    ex <- unique(trimmed[bad])
    ex <- ex[seq_len(min(5L, length(ex)))]
    stop(sprintf(
      "`%s` column '%s' has %d date value(s) that could not be read as dates (e.g. %s%s). That is more than a few, which usually means the dates are not in the format growthcleanr expected; if they are numeric day/month dates, check `date_order`. A few unreadable dates are set aside instead of stopping the run. See vignette(\"wrapper\").",
      arg_label, colname, length(bad),
      paste0('"', ex, '"', collapse = ", "),
      if (length(unique(trimmed[bad])) > 5L) ", ..." else ""))
  }
  attr(parsed, "gc_date_setaside") <- list(
    token = which(is_token), token_vals = unique(trimmed[is_token]),
    bad = bad, bad_vals = unique(trimmed[bad]))
  parsed
}

# Duplicate-column guard shared by both adapters. R's data.frame/data.table
# indexing (`data[[col]]`) silently returns the FIRST of two identically-named
# columns, so a duplicated subjid / sex / age / parameter column would drop the
# shadowed column's values with no signal -- the reshape analogue of the
# long-path duplicate-required-column stop. `cols` is the set of resolved role
# column names (structural + mapped value columns).
.gc_check_dup_cols <- function(nm, cols, shape) {
  cols <- unique(cols[!is.na(cols)])
  dup  <- cols[vapply(cols, function(cc) sum(nm == cc) > 1L, logical(1))]
  if (length(dup) > 0L) {
    stop(sprintf(
      "In %s input, column name(s) appear more than once: %s. Each subjid / sex / age / parameter column must have a unique name -- a duplicate column is silently ignored (only the first is used). Rename or drop the duplicates.",
      shape, paste0("'", dup, "'", collapse = ", ")))
  }
  invisible(TRUE)
}

# Wide block-collision guard: a (col, block) table for one prefix must have at
# most one column per block number. Two columns for the same block arise from an
# exact duplicate name (wt_1 twice) or leading-zero padding (wt_01 and wt_1);
# both would silently collapse to one cell. `label` names the prefix/role.
.gc_check_block_dups <- function(blk, label) {
  if (anyDuplicated(blk$block)) {
    d <- unique(blk$block[duplicated(blk$block)])
    stop(sprintf(
      "%s has more than one column for block %s (%s). Use exactly one column per block: duplicate names or leading-zero padding (e.g. '..._01' and '..._1') both resolve to the same block.",
      label, paste(d, collapse = ", "),
      paste0("'", blk$col[blk$block %in% d], "'", collapse = ", ")))
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Semi-wide -> long
#
# Input: one row per visit, with a subjid column, a sex column, an age source
# (an `agedays` column OR `dob` + `visit_date` columns), and one column per
# anthropometric parameter (at most one value per parameter per visit).
#
# Output: list(long = <canonical long data.table>, route = "agedays"|"date",
#              spec = <round-trip spec or NULL>).
#   `long` has columns subjid, param, {agedays | dob, visit_date}, sex,
#   measurement, id -- named canonically so it falls straight into the existing
#   cleangrowth() data-frame path. `id` is a freshly minted, unique, per-
#   measurement integer (drives same-day-extraneous tiebreaking downstream).
#
# The date route emits dob + visit_date (broadcast per measurement row) rather
# than computing agedays here: the existing funnel date step converts them, so
# date -> agedays logic stays in one place.
#
# `want_spec = TRUE` (semi-wide output requested) additionally returns the
# round-trip spec: a copy of the input table to use as the output base, plus a
# cell map linking each minted id back to its visit row and source column. See
# .gc_semiwide_output().
# ---------------------------------------------------------------------------
.gc_semiwide_to_long <- function(data,
                                 semiwide_cols,
                                 col_subjid,
                                 col_sex,
                                 col_agedays,
                                 col_dob,
                                 col_visit_date,
                                 age_source,
                                 col_id,
                                 quietly = TRUE,
                                 want_spec = FALSE) {
  data <- data.table::as.data.table(data)
  nm   <- names(data)

  # --- Structural column-name args must each be a single string.
  .req1 <- function(x, argname) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      stop(sprintf("In semi-wide input, `%s` must be a single column name (string).", argname))
    }
    x
  }
  col_subjid <- .req1(col_subjid, "subjid")
  col_sex    <- .req1(col_sex,    "sex")
  if (!is.null(col_id))         col_id         <- .req1(col_id,         "id")
  if (!is.null(col_dob))        col_dob        <- .req1(col_dob,        "dob")
  if (!is.null(col_visit_date)) col_visit_date <- .req1(col_visit_date, "visit_date")

  if (!col_subjid %in% nm) stop(sprintf("Required subjid column '%s' not found in the data. Set the `subjid` argument to the name of your subject-id column, or rename that column.", col_subjid))
  if (!col_sex %in% nm)    stop(sprintf("Required sex column '%s' not found in the data. Set the `sex` argument to the name of your sex column, or rename that column.%s", col_sex, .gc_sex_col_note))

  # --- Resolve the parameter -> column mapping.
  if (is.null(semiwide_cols)) {
    # Default: any column literally named as a recognized gc parameter.
    present <- intersect(.gc_recognized_params, nm)
    if (length(present) == 0L) {
      stop("No recognized parameter column found in the semi-wide input. Provide at least one ",
           "parameter column named WEIGHTKG / HEIGHTCM / HEADCM (etc.), or map your columns with ",
           "`semiwide_cols = list(WEIGHTKG = \"wt\", HEIGHTCM = \"ht\", HEADCM = \"hc\")`.")
    }
    pmap <- stats::setNames(present, present)  # gc param -> user column name
  } else {
    if (is.null(names(semiwide_cols)) || any(!nzchar(names(semiwide_cols)))) {
      stop("`semiwide_cols` must be a named list/vector mapping growthcleanr parameter names to column names, ",
           "e.g. list(WEIGHTKG = \"wt\", HEIGHTCM = \"ht\", HEADCM = \"hc\").")
    }
    bad_param <- setdiff(names(semiwide_cols), .gc_recognized_params)
    if (length(bad_param) > 0L) {
      stop(sprintf("`semiwide_cols` names must be growthcleanr parameters (%s); unrecognized: %s.",
                   paste(.gc_recognized_params, collapse = ", "),
                   paste(bad_param, collapse = ", ")))
    }
    lens <- vapply(semiwide_cols, length, integer(1))
    vals <- unlist(semiwide_cols, use.names = TRUE)
    if (any(lens != 1L) || !is.character(vals) || any(is.na(vals))) {
      .bad <- names(semiwide_cols)[lens != 1L]
      if (!length(.bad)) .bad <- names(semiwide_cols)
      stop(sprintf("`semiwide_cols` values must each be a single column name (string); check entry(ies): %s. Give one column name per parameter, e.g. list(WEIGHTKG = \"wt\").",
                   paste(.bad, collapse = ", ")))
    }
    miss <- vals[!vals %in% nm]
    if (length(miss) > 0L) {
      stop(sprintf("`semiwide_cols` maps to column(s) not found in the data: %s.",
                   paste(sprintf("%s -> '%s'", names(miss), miss), collapse = ", ")))
    }
    pmap <- vals
  }
  # Each value column may back exactly one parameter. Two parameters mapped to
  # one column would emit the same value twice under different labels (and, on
  # semi-wide output, generate two verdict columns for one value column).
  if (anyDuplicated(pmap)) {
    dup <- unique(pmap[duplicated(pmap)])
    stop(sprintf(
      "`semiwide_cols` maps more than one parameter to the same column: %s. Map each parameter to its own column.",
      paste(sprintf("'%s' <- %s", dup,
                    vapply(dup, function(d) paste(names(pmap)[pmap == d], collapse = " + "),
                           character(1))),
            collapse = "; ")))
  }
  present_params <- names(pmap)

  # --- Resolve the age route (mirrors the data-frame path rules).
  has_agedays <- col_agedays %in% nm
  has_dates   <- !is.null(col_dob) && !is.null(col_visit_date)
  if (!is.null(col_dob) && is.null(col_visit_date)) {
    stop("`dob` was supplied without `visit_date`; both are required to compute agedays from dates.")
  }
  if (is.null(col_dob) && !is.null(col_visit_date)) {
    stop("`visit_date` was supplied without `dob`; both are required to compute agedays from dates.")
  }
  if (has_dates) {
    date_missing <- setdiff(c(col_dob, col_visit_date), nm)
    if (length(date_missing) > 0L) {
      stop(sprintf("growthcleanr does not find the date column(s): %s. Tell growthcleanr which columns hold your dates, e.g. dob = \"your_dob_column\", visit_date = \"your_visit_date_column\"; or supply an agedays column instead.", paste(date_missing, collapse = ", ")))
    }
  }
  if (!is.null(age_source)) age_source <- match.arg(age_source, c("agedays", "dates"))
  if (identical(age_source, "dates")) {
    if (!has_dates) stop("age_source = 'dates' requires both `dob` and `visit_date` column arguments.")
    route <- "date"
  } else if (identical(age_source, "agedays")) {
    if (!has_agedays) stop(sprintf("age_source = 'agedays' requires an `agedays` column ('%s').", col_agedays))
    route <- "agedays"
  } else {
    if (has_agedays) {
      # Supplying both age and dates is common and expected; use agedays
      # silently (no warning). Set age_source = 'dates' to use the dates.
      route <- "agedays"
    } else if (has_dates) {
      route <- "date"
    } else {
      stop("No age information found: supply an `agedays` column, or both `dob` and `visit_date` columns.")
    }
  }

  # --- Per-visit fields.
  visit_id <- seq_len(nrow(data))
  have_uid <- !is.null(col_id) && col_id %in% nm

  # No resolved role column may be duplicated (see .gc_check_dup_cols): a second
  # same-named subjid/sex/age/id/parameter column would be silently ignored.
  .gc_check_dup_cols(nm, c(col_subjid, col_sex,
                           if (route == "agedays") col_agedays else c(col_dob, col_visit_date),
                           if (have_uid) col_id else NULL, unname(pmap)), "semi-wide")

  subj <- data[[col_subjid]]
  sexv <- data[[col_sex]]

  # --- "Relevant parameter missing" warnings. A parameter is present only if it
  # is mapped AND has at least one non-missing cell. Weight and height/length
  # warn when absent; head circumference does not.
  .has_data <- function(p) {
    if (!p %in% present_params) return(FALSE)
    any(!is.na(data[[pmap[[p]]]]))
  }
  wt_ok <- any(vapply(c("WEIGHTKG", "WEIGHTLBS"), .has_data, logical(1)))
  ht_ok <- any(vapply(c("HEIGHTCM", "HEIGHTIN", "LENGTHCM", "LENGTHIN"), .has_data, logical(1)))
  if (!wt_ok) {
    warning("No weight measurements found in the semi-wide input (weight column absent or entirely missing); weight will not be evaluated.")
  }
  if (!ht_ok) {
    warning("No height/length measurements found in the semi-wide input (height/length column absent or entirely missing); height will not be evaluated.")
  }

  # --- Melt: one visit row -> up to length(present_params) measurement rows.
  pieces <- vector("list", length(present_params))
  for (i in seq_along(present_params)) {
    p   <- present_params[i]
    val <- data[[pmap[[p]]]]
    # A factor value column is taken by its labels: binding a factor with other
    # columns would turn every value into a factor level.
    if (is.factor(val)) val <- as.character(val)
    dt  <- data.table::data.table(
      .visit_id   = visit_id,
      subjid      = subj,
      sex         = sexv,
      param       = p,
      measurement = val)
    if (route == "agedays") {
      dt[, agedays := data[[col_agedays]]]
    } else {
      dt[, dob        := data[[col_dob]]]
      dt[, visit_date := data[[col_visit_date]]]
    }
    if (have_uid) dt[, .user_id := data[[col_id]]]
    pieces[[i]] <- dt
  }
  long <- data.table::rbindlist(pieces, use.names = TRUE)

  # Empty cell (NA / NaN measurement) = no measurement of that parameter at that
  # visit -> drop. A literal 0 is kept and handled downstream (0 -> missing) so
  # semi-wide matches long-format behavior. Users must not pad with 0.
  long <- long[!is.na(measurement)]
  if (nrow(long) == 0L) {
    stop("After reshaping, the semi-wide input has no non-missing measurements to clean: every mapped parameter cell is empty (NA/NaN). Check that your value columns hold data and are mapped correctly in `semiwide_cols` (do not pad empty visits with 0).")
  }

  # --- Mint deterministic, unique per-measurement ids (drives SDE tiebreaking).
  # Emission order: subject in first-appearance order; within subject by visit
  # (an optional user `id` breaks ties, else input row order); within a visit by
  # the fixed param rank (weight, then height/length, then HC). This is "stable
  # and documented," not a provably optimal same-day tiebreak -- users who need
  # explicit control over which same-day duplicate is flagged should reformat to
  # long and set `id` themselves (see vignette("wrapper")).
  long[, .subj_order := match(subjid, unique(subj))]
  long[, .prank := .gc_param_rank(param)]
  if (have_uid) {
    data.table::setorder(long, .subj_order, .user_id, .visit_id, .prank)
  } else {
    data.table::setorder(long, .subj_order, .visit_id, .prank)
  }
  long[, id := seq_len(.N)]

  # Round-trip spec for semi-wide output: the input table becomes the output
  # base (so every user column returns untouched at its original position) and
  # the cell map links each minted id back to (visit row, source column).
  spec <- NULL
  if (isTRUE(want_spec)) {
    spec <- list(
      format    = "semi_wide",
      base      = data.table::copy(data),
      cell_map  = long[, .(id, visit_id = .visit_id, param = as.character(param),
                           user_col = unname(pmap[as.character(param)]))],
      pmap       = pmap,
      value_cols = unname(pmap),
      col_order  = nm,
      role_cols  = c(col_subjid, col_sex,
                     if (route == "agedays") col_agedays else c(col_dob, col_visit_date),
                     if (have_uid) col_id else NULL,
                     unname(pmap))
    )
    data.table::setkey(spec$cell_map, id)
  }

  # The user's id for each measurement (its visit row's id), aligned with the
  # minted ids (long is in minted-id order here). Returned so long output can
  # carry it under the user's own column name.
  user_id <- if (have_uid) long$.user_id else NULL
  # Value columns that were factors (taken by their labels above); cleangrowth()
  # warns about them by name.
  factor_cols <- unname(pmap)[vapply(unname(pmap), function(cc) is.factor(data[[cc]]), logical(1))]
  # Each measurement's input row, and the columns the reshape used, so long
  # output can carry the user's other columns (repeated per measurement).
  src_row  <- long$.visit_id
  used_cols <- c(col_subjid, col_sex, if (route == "agedays") col_agedays,
                 if (have_uid) col_id, unname(pmap))

  drop_helpers <- intersect(c(".visit_id", ".subj_order", ".prank", ".user_id"), names(long))
  long[, (drop_helpers) := NULL]

  keep <- if (route == "date") {
    c("subjid", "param", "dob", "visit_date", "sex", "measurement", "id")
  } else {
    c("subjid", "param", "agedays", "sex", "measurement", "id")
  }
  long <- long[, keep, with = FALSE]

  list(long = long[], route = route, spec = spec,
       user_id = user_id, user_id_name = if (have_uid) col_id else NULL,
       src_row = src_row, used_cols = used_cols, factor_cols = factor_cols,
       # The user's names for the structural columns, for long output (the
       # age name only when ages come from an age column; on the date route
       # the computed ages are growthcleanr's `agedays`).
       role_names = list(subjid = col_subjid, sex = col_sex,
                         agedays = if (route == "agedays") col_agedays else "agedays"))
}

# ---------------------------------------------------------------------------
# Wide -> long
#
# Input: one row per patient, with repeated observation blocks marked by a
# required `_N` suffix -- the user picks the prefix, growthcleanr requires the
# suffix. A patient row carries a subjid, a sex, and for each block N an age
# source plus up to one value per anthropometric parameter:
#
#   subjid sex agedays_1 wt_1 ht_1 agedays_2 wt_2 ht_2 ...
#
# Output: list(long = <canonical long data.table>, route = "agedays"|"date",
#              spec = <round-trip spec or NULL>) -- the same contract as
# .gc_semiwide_to_long(), so Hook A treats the two formats identically.
#
# Blocks are discovered from the data, not declared: every column named
# <prefix>_<integer> for a mapped prefix is a block of that prefix. Ragged
# patients (fewer observations than the widest patient) simply leave their
# trailing blocks empty; empty cells are dropped on the way in.
# ---------------------------------------------------------------------------

# All <prefix>_<integer> columns for one prefix, as a (col, block) table in
# ascending block order. Matching is done by string prefix rather than regex so
# prefixes containing regex metacharacters ("wt.kg") need no escaping.
.gc_discover_blocks <- function(nms, prefix) {
  pre  <- paste0(prefix, "_")
  cand <- nms[startsWith(nms, pre)]
  suf  <- substring(cand, nchar(pre) + 1L)
  ok   <- grepl("^[0-9]+$", suf)
  out  <- data.table::data.table(col = cand[ok], block = as.integer(suf[ok]))
  data.table::setorder(out, block)
  out[]
}

.gc_wide_to_long <- function(data,
                             wide_cols,
                             col_subjid,
                             col_sex,
                             col_agedays,
                             col_dob,
                             col_visit_date,
                             age_source,
                             col_id = NULL,
                             max_blocks = 25,
                             date_order = NULL,
                             quietly = TRUE,
                             want_spec = FALSE) {
  data <- data.table::as.data.table(data)
  nm   <- names(data)

  # --- Structural column-name args must each be a single string.
  .req1 <- function(x, argname, noun = "column name") {
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      stop(sprintf("In wide input, `%s` must be a single %s (string).", argname, noun))
    }
    x
  }
  col_subjid <- .req1(col_subjid, "subjid")
  col_sex    <- .req1(col_sex,    "sex")
  col_agedays    <- .req1(col_agedays,    "agedays",    "column-name prefix")
  if (!is.null(col_dob))        col_dob        <- .req1(col_dob,        "dob")
  if (!is.null(col_visit_date)) col_visit_date <- .req1(col_visit_date, "visit_date", "column-name prefix")

  # Per-measurement ids are minted from the block layout. A user `id` column
  # identifies a row (usually a subject), not a measurement: it is kept as is
  # and returned under its own name (each measurement from a row carries that
  # row's id in long output); it does not affect cleaning.
  if (!is.null(col_id)) col_id <- .req1(col_id, "id")
  have_uid <- !is.null(col_id) && col_id %in% nm

  if (!col_subjid %in% nm) stop(sprintf("Required subjid column '%s' not found in the data. Set the `subjid` argument to the name of your subject-id column, or rename that column.", col_subjid))
  if (!col_sex %in% nm)    stop(sprintf("Required sex column '%s' not found in the data. Set the `sex` argument to the name of your sex column, or rename that column.%s", col_sex, .gc_sex_col_note))

  # Structural columns (subjid, sex, and the single dob column on the date route)
  # are exact names; a duplicate would be silently first-wins. The per-block
  # value/age columns are checked below via .gc_check_block_dups.
  .gc_check_dup_cols(nm, c(col_subjid, col_sex, if (!is.null(col_dob)) col_dob,
                           if (have_uid) col_id), "wide")

  # --- Resolve the parameter -> prefix mapping.
  if (is.null(wide_cols)) {
    # Default: prefixes literally named as recognized gc parameters
    # (WEIGHTKG_1, HEIGHTCM_1, ...).
    cand <- .gc_recognized_params
    pmap <- stats::setNames(cand, cand)
  } else {
    if (is.null(names(wide_cols)) || any(!nzchar(names(wide_cols)))) {
      stop("`wide_cols` must be a named list/vector mapping growthcleanr parameter names to column-name prefixes, ",
           "e.g. list(WEIGHTKG = \"wt\", HEIGHTCM = \"ht\", HEADCM = \"hc\").")
    }
    bad_param <- setdiff(names(wide_cols), .gc_recognized_params)
    if (length(bad_param) > 0L) {
      stop(sprintf("`wide_cols` names must be growthcleanr parameters (%s); unrecognized: %s.",
                   paste(.gc_recognized_params, collapse = ", "),
                   paste(bad_param, collapse = ", ")))
    }
    lens <- vapply(wide_cols, length, integer(1))
    vals <- unlist(wide_cols, use.names = TRUE)
    if (any(lens != 1L) || !is.character(vals) || any(is.na(vals))) {
      .bad <- names(wide_cols)[lens != 1L]
      if (!length(.bad)) .bad <- names(wide_cols)
      stop(sprintf("`wide_cols` values must each be a single column-name prefix (string); check entry(ies): %s. Give one prefix per parameter, e.g. list(WEIGHTKG = \"wt\").",
                   paste(.bad, collapse = ", ")))
    }
    pmap <- vals
  }
  # Each prefix may back exactly one parameter: two parameters sharing a prefix
  # would emit every value twice under different labels.
  if (anyDuplicated(pmap)) {
    dup <- unique(pmap[duplicated(pmap)])
    stop(sprintf(
      "`wide_cols` maps more than one parameter to the same prefix: %s. Map each parameter to its own prefix.",
      paste(sprintf("'%s' <- %s", dup,
                    vapply(dup, function(d) paste(names(pmap)[pmap == d], collapse = " + "),
                           character(1))),
            collapse = "; ")))
  }

  # --- Discover blocks for each mapped prefix.
  blocks <- lapply(pmap, function(pfx) .gc_discover_blocks(nm, pfx))
  names(blocks) <- names(pmap)
  n_found <- vapply(blocks, nrow, integer(1))
  if (is.null(wide_cols)) {
    # Defaults are candidates, not requirements: keep the ones actually present.
    if (all(n_found == 0L)) {
      stop("No recognized parameter columns found in the wide input. Wide columns must carry a `_N` block ",
           "suffix (e.g. WEIGHTKG_1, WEIGHTKG_2). Name them after a growthcleanr parameter, or map your prefixes with ",
           "`wide_cols = list(WEIGHTKG = \"wt\", HEIGHTCM = \"ht\", HEADCM = \"hc\")`.")
    }
  } else if (any(n_found == 0L)) {
    miss <- names(pmap)[n_found == 0L]
    stop(sprintf("`wide_cols` maps parameter(s) %s to prefix(es) with no `<prefix>_N` columns in the data (expected e.g. %s).",
                 paste0("'", miss, "'", collapse = ", "),
                 paste0("'", pmap[miss], "_1'", collapse = ", ")))
  }
  pmap           <- pmap[n_found > 0L]
  blocks         <- blocks[names(pmap)]
  present_params <- names(pmap)

  # Two columns for one block (an exact duplicate name, or a leading-zero suffix
  # like ht_01 alongside ht_1) would silently collapse to one cell.
  for (p in present_params) {
    .gc_check_block_dups(blocks[[p]], sprintf("Prefix '%s'", pmap[[p]]))
  }

  # --- Resolve the age route (same precedence rules as the other input paths).
  age_blk <- .gc_discover_blocks(nm, col_agedays)
  vd_blk  <- if (is.null(col_visit_date)) NULL else .gc_discover_blocks(nm, col_visit_date)
  has_agedays <- nrow(age_blk) > 0L
  has_dates   <- !is.null(col_dob) && !is.null(col_visit_date)
  # Same one-column-per-block rule for the age and visit-date prefixes.
  if (has_agedays) .gc_check_block_dups(age_blk, sprintf("Age prefix '%s'", col_agedays))
  if (!is.null(vd_blk) && nrow(vd_blk) > 0L) {
    .gc_check_block_dups(vd_blk, sprintf("Visit-date prefix '%s'", col_visit_date))
  }
  if (!is.null(col_dob) && is.null(col_visit_date)) {
    stop("`dob` was supplied without `visit_date`; both are required to compute agedays from dates.")
  }
  if (is.null(col_dob) && !is.null(col_visit_date)) {
    stop("`visit_date` was supplied without `dob`; both are required to compute agedays from dates.")
  }
  if (has_dates) {
    if (!col_dob %in% nm) {
      stop(sprintf("growthcleanr does not find the dob column: %s. Tell growthcleanr which column holds the dates, e.g. dob = \"your_dob_column\"; or supply `%s_N` age columns instead.", col_dob, col_agedays))
    }
    if (nrow(vd_blk) == 0L) {
      stop(sprintf("No `%s_N` visit-date columns found in the wide input (expected e.g. '%s_1').",
                   col_visit_date, col_visit_date))
    }
  }
  if (!is.null(age_source)) age_source <- match.arg(age_source, c("agedays", "dates"))
  if (identical(age_source, "dates")) {
    if (!has_dates) stop("age_source = 'dates' requires both `dob` and `visit_date` column arguments.")
    route <- "date"
  } else if (identical(age_source, "agedays")) {
    if (!has_agedays) stop(sprintf("age_source = 'agedays' requires `%s_N` age columns (e.g. '%s_1').",
                                   col_agedays, col_agedays))
    route <- "agedays"
  } else {
    if (has_agedays) {
      # Supplying both age and dates is common and expected; use the age
      # columns silently (no warning). Set age_source = 'dates' to use dates.
      route <- "agedays"
    } else if (has_dates) {
      route <- "date"
    } else {
      stop(sprintf("No age information found: supply `%s_N` age columns, or a `dob` column plus `%s_N` visit-date columns.",
                   col_agedays, if (is.null(col_visit_date)) "visit_date" else col_visit_date))
    }
  }

  # --- Block-count guards, over the union of value and age blocks.
  all_blocks <- sort(unique(c(unlist(lapply(blocks, `[[`, "block")),
                             if (route == "agedays") age_blk$block else vd_blk$block)))
  max_n <- max(all_blocks)
  if (max_n > 1000L) {
    stop(sprintf("Discovered %d observation blocks (highest suffix _%d). growthcleanr cannot handle more than 1000. Check the `_N` naming of your columns (a stray suffix like col_9999 could trigger this). If the data really is this wide, reshape to long or semi-wide input instead.",
                 length(all_blocks), max_n))
  }
  if (max_n > max_blocks) {
    warning(sprintf("Found %d observation blocks (highest suffix _%d), which exceeds max_blocks = %d; processing all of them. Raise `max_blocks`, or reshape to long or semi-wide input for data this wide.",
                    length(all_blocks), max_n, max_blocks))
  }
  gaps <- setdiff(seq_len(max_n), all_blocks)
  if (length(gaps) > 0L) {
    warning(sprintf("Observation block(s) %s have no columns (block numbering runs 1 to %d); processing the blocks that are present.",
                    paste0("_", gaps, collapse = ", "), max_n))
  }

  # --- Patient-level fields. One row per patient is the contract; repeated
  # subjids still work (each row's blocks become more observations for that
  # subject), but they are worth flagging because the usual cause is a
  # half-reshaped table.
  pt_row <- seq_len(nrow(data))
  subj   <- data[[col_subjid]]
  sexv   <- data[[col_sex]]
  if (anyDuplicated(subj)) {
    dup <- unique(subj[duplicated(subj)])
    warning(sprintf("Wide input has %d subject(s) on more than one row (e.g. %s); each row's observation blocks are treated as additional measurements for that subject.",
                    length(dup), paste0("'", utils::head(dup, 3L), "'", collapse = ", ")))
  }

  # --- Ages, one value per (patient row, block).
  if (route == "agedays") {
    age_long <- data.table::rbindlist(lapply(seq_len(nrow(age_blk)), function(i)
      data.table::data.table(.pt_row = pt_row, .block = age_blk$block[i],
                             agedays = data[[age_blk$col[i]]])))
    age_num <- suppressWarnings(as.numeric(age_long$agedays))
  } else {
    # Classify dob + every visit_date block jointly (one format for the whole
    # run), then parse each column under that class. This age computation only
    # feeds the block-monotonicity check below; the funnel re-parses the emitted
    # dob/visit_date for the authoritative agedays, using the same date_order.
    # Reject numeric (epoch-ambiguous) date columns with their targeted message
    # before classification.
    date_num_cols <- c(col_dob, vd_blk$col)
    for (.dc in date_num_cols) {
      if (is.numeric(data[[.dc]]))
        invisible(.gc_parse_date_col(data[[.dc]],
                                     if (identical(.dc, col_dob)) "dob" else "visit_date", .dc))
    }
    date_cls <- .gc_date_class(
      c(as.character(data[[col_dob]]),
        unlist(lapply(vd_blk$col, function(cc) as.character(data[[cc]])),
               use.names = FALSE)),
      date_order, "dob/visit_date")
    dobv <- .gc_parse_date_col(data[[col_dob]], "dob", col_dob, date_cls, date_order)
    age_long <- data.table::rbindlist(lapply(seq_len(nrow(vd_blk)), function(i)
      data.table::data.table(.pt_row = pt_row, .block = vd_blk$block[i],
                             visit_date = .gc_parse_date_col(data[[vd_blk$col[i]]],
                                                             "visit_date", vd_blk$col[i],
                                                             date_cls, date_order))))
    age_num <- as.numeric(age_long$visit_date - dobv[age_long$.pt_row], units = "days")
  }

  # Ages should rise across ascending blocks within a patient row. Out-of-order
  # blocks are processed as they are (the funnel sorts by age anyway), but they
  # usually mean the blocks were assembled in the wrong order.
  chk <- data.table::data.table(.pt_row = age_long$.pt_row, .block = age_long$.block, .a = age_num)
  data.table::setorder(chk, .pt_row, .block)
  nonmono <- chk[!is.na(.a), .(bad = any(diff(.a) < 0)), by = .pt_row][bad == TRUE]
  if (nrow(nonmono) > 0L) {
    warning(sprintf("Ages do not increase across observation blocks for %d subject(s) (e.g. %s); processing as submitted.",
                    nrow(nonmono),
                    paste0("'", utils::head(subj[nonmono$.pt_row], 3L), "'", collapse = ", ")))
  }

  # --- "Relevant parameter missing" warnings. A parameter counts as present
  # only if it is mapped AND has at least one non-missing cell in any block.
  .has_data <- function(p) {
    if (!p %in% present_params) return(FALSE)
    any(vapply(blocks[[p]]$col, function(cc) any(!is.na(data[[cc]])), logical(1)))
  }
  if (!any(vapply(c("WEIGHTKG", "WEIGHTLBS"), .has_data, logical(1)))) {
    warning("No weight measurements found in the wide input (weight columns absent or entirely missing); weight will not be evaluated.")
  }
  if (!any(vapply(c("HEIGHTCM", "HEIGHTIN", "LENGTHCM", "LENGTHIN"), .has_data, logical(1)))) {
    warning("No height/length measurements found in the wide input (height/length columns absent or entirely missing); height will not be evaluated.")
  }
  # (Head circumference is not warned on when absent.)

  # --- Melt: one patient row -> one long row per occupied (block, parameter).
  pieces <- list()
  for (p in present_params) {
    b <- blocks[[p]]
    for (i in seq_len(nrow(b))) {
      pieces[[length(pieces) + 1L]] <- data.table::data.table(
        .pt_row     = pt_row,
        .block      = b$block[i],
        .src_col    = b$col[i],
        param       = p,
        measurement = if (is.factor(data[[b$col[i]]])) as.character(data[[b$col[i]]])
                      else data[[b$col[i]]])
    }
  }
  long <- data.table::rbindlist(pieces, use.names = TRUE)

  # Empty cell (NA / NaN) = no measurement in that block -> drop. This is what
  # makes ragged patients work: a patient with 3 observations in a 6-block table
  # simply has nothing in blocks 4-6. A literal 0 is kept and handled downstream
  # (0 -> missing), matching long-format behavior; do not pad with 0.
  long <- long[!is.na(measurement)]
  if (nrow(long) == 0L) {
    stop("After reshaping, the wide input has no non-missing measurements to clean: every <prefix>_N value cell is empty (NA/NaN). Check that your block columns hold data and are mapped correctly in `wide_cols` (do not pad empty blocks with 0).")
  }

  long[, subjid := subj[.pt_row]]
  long[, sex    := sexv[.pt_row]]
  # A value in a block with no age (or no age column at all) keeps its row and
  # is handled downstream as missing information -- exactly as in long format.
  long <- merge(long, age_long, by = c(".pt_row", ".block"), all.x = TRUE, sort = FALSE)
  if (route == "date") long[, dob := dobv[.pt_row]]

  # --- Mint deterministic, unique per-measurement ids (drives SDE tiebreaking).
  # Emission order: subject in first-appearance order; within subject by patient
  # row, then block number ascending; within a block by the fixed param rank
  # (weight, then height/length, then HC). Stable and documented rather than
  # provably optimal -- users who need explicit control over which same-day
  # duplicate is flagged should reformat to long and set `id` themselves (see
  # vignette("wrapper")).
  long[, .subj_order := match(subjid, unique(subj))]
  long[, .prank := .gc_param_rank(param)]
  data.table::setorder(long, .subj_order, .pt_row, .block, .prank)
  long[, id := seq_len(.N)]

  # Round-trip spec for wide output: the input table is the output base (every
  # user column returns untouched at its original position) and the cell map
  # links each minted id back to (patient row, source column).
  spec <- NULL
  if (isTRUE(want_spec)) {
    spec <- list(
      format    = "wide",
      base      = data.table::copy(data),
      cell_map  = long[, .(id, pt_row = .pt_row, block = .block,
                           param = as.character(param), user_col = .src_col)],
      pmap       = pmap,
      blocks     = blocks,
      n_blocks   = max_n,
      # Per-measurement value columns, in parameter-then-block order: these are
      # what the emitted output columns are named after (exclude_ht_1, ...).
      value_cols = unlist(lapply(blocks, `[[`, "col"), use.names = FALSE),
      col_order  = nm,
      role_cols  = c(col_subjid, col_sex,
                     if (route == "agedays") age_blk$col else c(col_dob, vd_blk$col),
                     if (have_uid) col_id,
                     unlist(lapply(blocks, `[[`, "col"), use.names = FALSE)))
    data.table::setkey(spec$cell_map, id)
  }

  # The user's row id for each measurement, aligned with the minted ids (long is
  # in minted-id order here).
  user_id <- if (have_uid) data[[col_id]][long$.pt_row] else NULL
  # Value columns that were factors (taken by their labels above); cleangrowth()
  # warns about them by name.
  .vcols <- unlist(lapply(blocks, `[[`, "col"), use.names = FALSE)
  factor_cols <- .vcols[vapply(.vcols, function(cc) is.factor(data[[cc]]), logical(1))]
  # Each measurement's input row, and the columns the reshape used (subject
  # columns and every block column), so long output can carry the user's other
  # columns (repeated per measurement).
  src_row  <- long$.pt_row
  used_cols <- c(col_subjid, col_sex,
                 if (route == "agedays") age_blk$col else vd_blk$col,
                 if (have_uid) col_id,
                 unlist(lapply(blocks, `[[`, "col"), use.names = FALSE))

  drop_helpers <- intersect(c(".pt_row", ".block", ".src_col", ".subj_order", ".prank"),
                            names(long))
  long[, (drop_helpers) := NULL]

  keep <- if (route == "date") {
    c("subjid", "param", "dob", "visit_date", "sex", "measurement", "id")
  } else {
    c("subjid", "param", "agedays", "sex", "measurement", "id")
  }
  long <- long[, keep, with = FALSE]

  list(long = long[], route = route, spec = spec,
       user_id = user_id, user_id_name = if (have_uid) col_id else NULL,
       src_row = src_row, used_cols = used_cols, factor_cols = factor_cols,
       # The user's names for the structural columns, for long output (the
       # age name only when ages come from an age column; on the date route
       # the computed ages are growthcleanr's `agedays`).
       role_names = list(subjid = col_subjid, sex = col_sex,
                         agedays = if (route == "agedays") col_agedays else "agedays"))
}

# Long output from semi-wide/wide input: growthcleanr's minted per-measurement
# id is returned as gc_internal_id (never under a user's column name), and the
# user's id, if supplied, is returned under its own column name with the
# user's own values (repeated for each measurement from that input row). With
# no user id there is no `id` column. `dt` holds the minted ids in `id`.
.gc_crossformat_id_cols <- function(dt, user_vals, user_name, extra = NULL,
                                    role_names = NULL) {
  if (!"id" %in% names(dt)) return(dt)
  if ("gc_internal_id" %in% names(dt)) dt[, gc_internal_id := NULL]
  mint <- dt$id
  data.table::setnames(dt, "id", "gc_internal_id")
  # subjid / sex / age columns under the user's own names (as in long input).
  for (canon in names(role_names)) {
    nm <- role_names[[canon]]
    if (!is.null(nm) && nm != canon && canon %in% names(dt))
      data.table::setnames(dt, canon, nm)
  }
  if (!is.null(user_vals) && !is.null(user_name)) {
    dt[, (user_name) := user_vals[mint]]
    data.table::setcolorder(dt, user_name)
  }
  # The user's other columns (carry_through), one row per minted id, already
  # renamed where a name matches a returned growthcleanr column; appended.
  if (!is.null(extra) && ncol(extra) > 0L) {
    for (nm in names(extra)) dt[, (nm) := extra[[nm]][mint]]
  }
  dt
}

# ---------------------------------------------------------------------------
# Semi-wide output: emitted-column naming
#
# Two categories:
#   1. growthcleanr's own OUTPUT columns take no prefix, only the value-column
#      suffix: exclude_<COL>, bin_exclude_<COL>. `exclude` is the main output --
#      what the user came for -- so it needs no announcement that it is ours.
#   2. growthcleanr's INTERNAL version of a key input the user supplied takes a
#      gc_internal_ prefix: gc_internal_meas_<COL>, gc_internal_param_<COL>,
#      gc_internal_agedays. These report what gc used, not what was submitted.
#      `measurement` is abbreviated to `meas` so wide output stays inside a
#      32-character variable-name limit.
#
# Per-measurement columns take the _<COL> suffix; visit-level ones (agedays,
# sex, subjid) are emitted once, unsuffixed.
# ---------------------------------------------------------------------------
.gc_emit_internal  <- c("subjid", "id", "param", "measurement", "agedays", "sex")
.gc_emit_per_visit <- c("subjid", "agedays", "sex")

# Every per-measurement column cleangrowth() can put in its long output, with
# the argument that turns it on (NA = always available).
.gc_emit_catalog <- c(
  exclude = NA, bin_exclude = NA, measurement = NA, param = NA, id = NA,
  agedays = NA, sex = NA, subjid = NA,
  exclude_detailed = "exclude_detail", tri_exclude = "tri_exclude",
  unit_error_range = "unit_error_range", mean_ht = "mean_ht",
  cf_status = "cf_detail", cf_deltaz = "cf_detail", cf_nextz = "cf_detail",
  corr_val = "corr_detail", corr_pt = "corr_detail",
  tbc.sd = "recenter_detail", ctbc.sd = "recenter_detail",
  rcmedian.sd = "recenter_detail",
  qm_elig_c_cf = "qm_detail", qm_elig_c_hardlimit = "qm_detail",
  qm_elig_c_eviltwins = "qm_detail", qm_elig_c_trajextreme = "qm_detail",
  qm_elig_extraneous = "qm_detail", qm_elig_c_traj = "qm_detail",
  qm_elig_c_absdiff = "qm_detail", qm_elig_c_pair = "qm_detail",
  qm_elig_c_single = "qm_detail", qm_elig_c_trajuneval = "qm_detail")

# Validate + normalize the widened_emit vector. Requesting the value gc used
# (`measurement`) always also emits `param`, so the units are visible in the
# data: gc_internal_meas_wt_lbs holds kilograms, and gc_internal_param_wt_lbs
# says WEIGHTKG.
.gc_normalize_emit <- function(emit) {
  if (is.null(emit)) emit <- "exclude"
  if (!is.character(emit) || anyNA(emit) || length(emit) == 0L) {
    stop("`widened_emit` must be a character vector of growthcleanr output column names, e.g. \"exclude\".")
  }
  emit <- unique(emit)
  bad  <- setdiff(emit, names(.gc_emit_catalog))
  if (length(bad) > 0L) {
    stop(sprintf(
      "`widened_emit` value(s) %s are not growthcleanr per-measurement output columns. Available: %s.",
      paste0("'", bad, "'", collapse = ", "),
      paste(names(.gc_emit_catalog), collapse = ", ")))
  }
  if ("measurement" %in% emit && !"param" %in% emit) {
    emit <- append(emit, "param", after = match("measurement", emit))
  }
  emit
}

# Source column in cleangrowth()'s output for one emit keyword. The internal
# versions of user inputs come from gc's own columns, not the verbatim ones the
# user gets back.
.gc_emit_src <- function(g) {
  switch(g,
         measurement = "gc_internal_meas",
         param       = "gc_internal_param",
         sex         = "gc_internal_sex",
         id          = "gc_internal_id",
         agedays     = "gc_internal_agedays",
         g)
}

# Output column name for one emitted gc column (+ value column, when suffixed).
.gc_emit_name <- function(g, user_col = NULL) {
  label  <- if (identical(g, "measurement")) "meas" else g
  prefix <- if (g %in% .gc_emit_internal) "gc_internal_" else ""
  if (g %in% .gc_emit_per_visit) return(paste0(prefix, label))
  paste0(prefix, label, "_", user_col)
}

# All column names re-widened output will generate, in emission order.
# `value_cols` is the spec's list of per-measurement value columns: the mapped
# parameter columns for semi-wide, the individual `<prefix>_N` block columns for
# wide (so a wide run generates exclude_ht_1, exclude_ht_2, ...).
.gc_emit_names <- function(emit, value_cols) {
  ucols <- unname(value_cols)
  out <- character(0)
  for (g in emit) {
    out <- c(out, if (g %in% .gc_emit_per_visit) .gc_emit_name(g)
                  else vapply(ucols, function(u) .gc_emit_name(g, u), character(1)))
  }
  unname(out)
}

# ---------------------------------------------------------------------------
# Long results -> semi-wide output
#
# The user's input table is the output base: it comes back with its columns
# untouched, in their original order, one row per input row, with the emitted
# growthcleanr columns slotted in immediately right of the value column they
# describe. Nothing is reconstructed from the results, so rows cannot be lost,
# reordered, or silently rewritten.
# ---------------------------------------------------------------------------
.gc_semiwide_output <- function(res, spec, emit, carry_through = TRUE) {
  res <- data.table::as.data.table(res)
  if (!"id" %in% names(res)) {
    stop("Internal error: semi-wide output needs the `id` column to rejoin results.")
  }

  base <- spec$base
  if (!isTRUE(carry_through)) {
    keep <- intersect(spec$col_order, spec$role_cols)
    base <- base[, keep, with = FALSE]
  }
  n <- nrow(base)

  cm <- data.table::copy(spec$cell_map)
  data.table::setnames(cm, c("param", "user_col", "visit_id"),
                       c(".cm_param", ".cm_user_col", ".cm_visit_id"))
  map <- merge(cm, res, by = "id", all.x = TRUE, sort = FALSE)
  data.table::setorder(map, id)

  ucols <- unname(spec$value_cols)
  gen   <- list()   # generated column name -> vector
  for (g in emit) {
    src <- .gc_emit_src(g)
    if (!src %in% names(map)) {
      sw <- .gc_emit_catalog[[g]]
      stop(sprintf(
        "`widened_emit` includes '%s', which is not in this run's output%s.",
        g, if (is.na(sw)) "" else sprintf(" -- set `%s = TRUE` to produce it", sw)))
    }
    if (g %in% .gc_emit_per_visit) {
      # One value per visit row: take it from that visit's first measurement.
      first <- map[!duplicated(.cm_visit_id)]
      tmpl  <- first[[src]]
      vec   <- tmpl[rep(NA_integer_, n)]
      vec[first$.cm_visit_id] <- tmpl
      gen[[.gc_emit_name(g)]] <- vec
    } else {
      for (u in ucols) {
        sub  <- map[.cm_user_col == u]
        tmpl <- sub[[src]]
        # Subsetting by NA yields an all-NA vector of the same class, which
        # preserves factor levels (exclude) and Date/integer types.
        vec  <- tmpl[rep(NA_integer_, n)]
        vec[sub$.cm_visit_id] <- tmpl
        gen[[.gc_emit_name(g, u)]] <- vec
      }
    }
  }

  out <- data.table::copy(base)
  for (nm_g in names(gen)) out[, (nm_g) := gen[[nm_g]]]

  # Column order: the user's own columns as submitted, each value column
  # followed immediately by the growthcleanr columns describing it; visit-level
  # columns (gc_internal_agedays etc.) appended at the end.
  suffixed <- setdiff(emit, .gc_emit_per_visit)
  ord <- character(0)
  for (cl in names(base)) {
    ord <- c(ord, cl)
    if (cl %in% ucols) {
      ord <- c(ord, vapply(suffixed, function(g) .gc_emit_name(g, cl), character(1)))
    }
  }
  ord <- c(ord, setdiff(names(gen), ord))
  data.table::setcolorder(out, unname(ord))
  out[]
}

# ---------------------------------------------------------------------------
# Wide output: emission levels + naming
#
# Wide output has THREE emission levels, one more than semi-wide, because the
# age varies within a subject row:
#   - per-measurement (suffixed by the value block column, e.g. `_wt_1`):
#       exclude, bin_exclude, and the internal meas/param/id.
#   - per-block (suffixed by the block number, e.g. `_1`): agedays. This is the
#       wide-specific level -- `gc_internal_agedays_1`, `gc_internal_agedays_2`,
#       ... -- and is the only way to see the age growthcleanr computed on the
#       date route.
#   - per-subject (unsuffixed): subjid, sex -- one value per patient row.
#
# Naming is otherwise identical to semi-wide: gc's own output columns take the
# suffix only (exclude_wt_1); gc's internal version of a user input takes the
# gc_internal_ prefix (gc_internal_meas_wt_1); measurement abbreviates to meas
# for the 32-character variable-name limit.
# ---------------------------------------------------------------------------
.gc_emit_per_subject <- c("subjid", "sex")

# Output column name for one emitted gc column in wide output. `suffix` is the
# value block column (per-measurement), the block number (per-block agedays), or
# NULL (per-subject).
.gc_wide_emit_name <- function(g, suffix = NULL) {
  label  <- if (identical(g, "measurement")) "meas" else g
  prefix <- if (g %in% .gc_emit_internal) "gc_internal_" else ""
  if (is.null(suffix)) return(paste0(prefix, label))
  paste0(prefix, label, "_", suffix)
}

# Every output column name a re-widen will generate, in emission order, for the
# collision pre-check. Dispatches on the spec's format so the semi-wide path is
# byte-identical to before and the wide path gets the per-block agedays names.
.gc_rewiden_emit_names <- function(emit, spec) {
  if (!identical(spec$format, "wide")) return(.gc_emit_names(emit, spec$value_cols))
  vcols  <- unname(spec$value_cols)
  blocks <- sort(unique(spec$cell_map$block))
  out <- character(0)
  for (g in emit) {
    out <- c(out,
      if (g %in% .gc_emit_per_subject) .gc_wide_emit_name(g)
      else if (identical(g, "agedays")) vapply(blocks, function(b) .gc_wide_emit_name(g, b), character(1))
      else vapply(vcols, function(u) .gc_wide_emit_name(g, u), character(1)))
  }
  unname(out)
}

# ---------------------------------------------------------------------------
# Long results -> wide output
#
# Direct long -> wide (no semi-wide waystation): the user's input table is the
# output base, and each long result row already knows its destination cell via
# the cell map (`user_col` is the exact block column, e.g. "wt_1"). Verdicts are
# scattered onto the echoed base by patient row, so rows cannot be lost,
# reordered, or rewritten, and ragged blocks come back exactly as submitted.
# ---------------------------------------------------------------------------
.gc_wide_output <- function(res, spec, emit, carry_through = TRUE) {
  res <- data.table::as.data.table(res)
  if (!"id" %in% names(res)) {
    stop("Internal error: wide output needs the `id` column to rejoin results.")
  }

  base <- spec$base
  if (!isTRUE(carry_through)) {
    keep <- intersect(spec$col_order, spec$role_cols)
    base <- base[, keep, with = FALSE]
  }
  n <- nrow(base)   # one row per subject (pt_row)

  cm <- data.table::copy(spec$cell_map)
  data.table::setnames(cm, c("param", "user_col", "pt_row", "block"),
                       c(".cm_param", ".cm_user_col", ".cm_pt_row", ".cm_block"))
  map <- merge(cm, res, by = "id", all.x = TRUE, sort = FALSE)
  data.table::setorder(map, id)

  vcols <- unname(spec$value_cols)
  gen   <- list()   # generated column name -> vector
  # Subsetting a template vector by NA yields an all-NA vector of the same class,
  # preserving factor levels (exclude) and Date/integer types.
  .scatter <- function(src_vals, pt_rows) {
    vec <- src_vals[rep(NA_integer_, n)]
    vec[pt_rows] <- src_vals
    vec
  }
  for (g in emit) {
    src <- .gc_emit_src(g)
    if (!src %in% names(map)) {
      sw <- .gc_emit_catalog[[g]]
      stop(sprintf(
        "`widened_emit` includes '%s', which is not in this run's output%s.",
        g, if (is.na(sw)) "" else sprintf(" -- set `%s = TRUE` to produce it", sw)))
    }
    if (g %in% .gc_emit_per_subject) {
      # One value per subject row: take it from that subject's first measurement.
      first <- map[!duplicated(.cm_pt_row)]
      gen[[.gc_wide_emit_name(g)]] <- .scatter(first[[src]], first$.cm_pt_row)
    } else if (identical(g, "agedays")) {
      # One column per block present: all measurements in a block share its age.
      # Subset to the block *first*, then take one row per subject (deduping over
      # the whole map would keep only each subject's first block).
      for (b in sort(unique(map$.cm_block))) {
        sub <- map[.cm_block == b]
        sub <- sub[!duplicated(.cm_pt_row)]
        gen[[.gc_wide_emit_name(g, b)]] <- .scatter(sub[[src]], sub$.cm_pt_row)
      }
    } else {
      # One column per value block column (wt_1, wt_2, ...): each backs at most
      # one measurement per subject.
      for (u in vcols) {
        sub <- map[.cm_user_col == u]
        gen[[.gc_wide_emit_name(g, u)]] <- .scatter(sub[[src]], sub$.cm_pt_row)
      }
    }
  }

  out <- data.table::copy(base)
  for (nm_g in names(gen)) out[, (nm_g) := gen[[nm_g]]]

  # Column order: the user's own columns as submitted, each value block column
  # followed immediately by the per-measurement growthcleanr columns describing
  # it; per-block (agedays) and per-subject (subjid/sex) internals appended at
  # the end, matching semi-wide output.
  pm_emit <- setdiff(emit, c(.gc_emit_per_subject, "agedays"))
  ord <- character(0)
  for (cl in names(base)) {
    ord <- c(ord, cl)
    if (cl %in% vcols) {
      ord <- c(ord, vapply(pm_emit, function(g) .gc_wide_emit_name(g, cl), character(1)))
    }
  }
  ord <- c(ord, setdiff(names(gen), ord))
  data.table::setcolorder(out, unname(ord))
  out[]
}
