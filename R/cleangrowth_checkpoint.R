# Maximum number of checkpoints in one run: checkpoint files are numbered with
# four digits (checkpoint_NNNN.rds). A function so tests can lower it.
.gc_max_checkpoints <- function() 9999L

# -- Internal helper -----------------------------------------------------------

#' Combine checkpoint .rds files into a single data.table
#'
#' Internal workhorse used by both \code{cleangrowth_checkpoint()} and
#' \code{cleangrowth_checkpoint_combine()}.
#'
#' @param ckpt_files Character vector of full paths to checkpoint .rds files,
#'   in the order they should be combined.
#' @param id_order Optional vector of id values in the desired output row
#'   order. If NULL, rows are returned in checkpoint order.
#' @param .log Function for logging messages.
#' @param missing_msg Optional function(n_missing, n_total) returning the
#'   warning text used when some \code{id_order} values are not found in the
#'   checkpoint files. NULL uses a generic message.
#' @return data.table
#' @noRd
.combine_checkpoints <- function(ckpt_files, id_order = NULL, .log = message,
                                 missing_msg = NULL) {

  # Verify all expected files exist before reading any
  missing <- ckpt_files[!file.exists(ckpt_files)]
  if (length(missing) > 0)
    stop("Cannot combine: ", length(missing), " checkpoint file(s) missing:\n",
         paste(basename(missing), collapse = ", "))

  .log("Reading and combining ", length(ckpt_files), " checkpoint file(s) one at a time...")

  # Read and bind one file at a time to limit peak memory usage.
  # Loading all files into a list first would hold up to n+1 copies in RAM;
  # here at most 2 copies (final + current checkpoint) are live at once.
  first_levels <- NULL   # factor levels from first checkpoint that has them
  final        <- NULL

  for (i in seq_along(ckpt_files)) {
    checkpoint <- readRDS(ckpt_files[[i]])

    # Capture factor levels before rbindlist can coerce them
    if (is.null(first_levels) &&
        "exclude" %in% names(checkpoint) && is.factor(checkpoint[["exclude"]]))
      first_levels <- levels(checkpoint[["exclude"]])

    final <- if (is.null(final)) checkpoint
             else rbindlist(list(final, checkpoint), use.names = TRUE, fill = TRUE)
    rm(checkpoint)   # release per-checkpoint memory immediately

    if (i %% 10L == 0L || i == length(ckpt_files))
      .log("  Read ", i, " / ", length(ckpt_files),
           " file(s), ", nrow(final), " rows so far")
  }

  # Re-apply factor levels to exclude column in case rbindlist coerced it.
  if (!is.null(first_levels) && "exclude" %in% names(final))
    final[, exclude := factor(as.character(exclude), levels = first_levels)]

  # Reorder to match id_order if provided
  if (!is.null(id_order)) {
    # Use character comparison to be safe against integer/double/factor type
    # mismatches between the input data and what was saved in the .rds files.
    idx <- match(as.character(id_order), as.character(final[["id"]]))
    n_missing <- sum(is.na(idx))
    if (n_missing > 0) {
      big <- function(x) format(x, big.mark = ",", trim = TRUE)
      msg <- if (is.function(missing_msg)) missing_msg(n_missing, length(idx)) else
        paste0(big(n_missing), " of ", big(length(idx)),
               " id(s) in id_order were not found in the checkpoint files; ",
               "these rows are NA in the output.")
      warning(msg, call. = FALSE)
    }
    final <- final[idx]
  }

  .log("Combined result: ", nrow(final), " rows, ", ncol(final), " columns")
  return(final)
}


# -- Internal checkpoint runner -------------------------------------------------

#' Checkpoint / persist / resume / combine loop for a checkpointed clean
#'
#' Internal engine behind \code{cleangrowth(checkpoint_dir = ...)}. Splits the
#' canonical long table \code{long_dt} into checkpoints of \code{checkpoint_size} subjects,
#' cleans each checkpoint with a nested \code{cleangrowth(checkpoint_dir = NULL)} call
#' in long mode, persists each checkpoint to \code{checkpoint_NNNN.rds}, skips completed
#' checkpoints when \code{checkpoint_resume = TRUE}, then combines. Returns the
#' combined result in input-row order with canonical column names; the caller
#' (\code{cleangrowth}) does the outer finalization (id restore, user-name
#' rename, re-widen).
#'
#' @param long_dt Canonical long data.table: \code{id}, \code{subjid},
#'   \code{param}, \code{agedays}, \code{sex}, \code{measurement} (raw values;
#'   each checkpoint re-normalizes via the forwarded \code{sex_coding}/\code{param_coding}).
#'   \code{id} must already be valid and unique (regen happens once, upstream).
#' @param clean_args Named list of arguments forwarded to each checkpoint's
#'   \code{cleangrowth()} call.
#' @param checkpoint_dir Directory for the per-checkpoint \code{.rds} files.
#' @param checkpoint_size Subjects per checkpoint.
#' @param checkpoint_size_provided Logical; TRUE if the caller supplied
#'   \code{checkpoint_size} (an explicit value that exceeds the 9999-checkpoint cap
#'   is increased with a warning; the default auto-increases silently).
#' @param checkpoint_resume,keep_checkpoints,log_file,quietly,verbose Checkpoint
#'   controls (see \code{\link{cleangrowth}}).
#' @return data.table (combined, canonical column names, input-row order).
#' @noRd
.gc_checkpoint_run <- function(long_dt,
                               clean_args,
                               checkpoint_dir,
                               checkpoint_size          = 5000,
                               checkpoint_size_provided = FALSE,
                               checkpoint_resume   = FALSE,
                               keep_checkpoints    = TRUE,
                               log_file            = NULL,
                               quietly             = FALSE,
                               verbose             = FALSE) {

  if (isTRUE(verbose)) quietly <- FALSE

  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  t_run_start <- Sys.time()

  # id_order for the final combine is the input-row order of the id column.
  # Capture it now (long_dt is never reordered below, but this documents intent).
  input_id_order <- long_dt$id

  # .log(): timestamped status line. Console only when !quietly; always appended
  # to log_file (if set) so a colleague can monitor a quiet run remotely.
  .log <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
    if (!quietly) message(msg)
    if (!is.null(log_file)) cat(msg, "\n", file = log_file, append = TRUE)
  }
  # .banner(): prominent, un-timestamped checkpoint marker - the line the user scans
  # for to see how far the run has progressed. Tee'd to log_file as well.
  .banner <- function(title) {
    rule <- strrep("=", 50L)
    lines <- c("", rule, paste0("  ", title), rule)
    if (!quietly) for (ln in lines) message(ln)
    if (!is.null(log_file)) cat(paste(lines, collapse = "\n"), "\n", file = log_file, append = TRUE)
  }
  # Compact elapsed-time formatter ("32s" / "18m04s" / "1h02m").
  .fmt_dur <- function(secs) {
    secs <- as.numeric(secs)
    if (secs < 60) return(sprintf("%.0fs", secs))
    if (secs < 3600) return(sprintf("%dm%02ds", secs %/% 60, round(secs %% 60)))
    sprintf("%dh%02dm", secs %/% 3600, (secs %% 3600) %/% 60)
  }

  # ---- split subjects into checkpoints ----
  # Sort so checkpoint membership is deterministic for a given set of subjects and
  # checkpoint_size, independent of input row order. The final result is reordered to
  # the input id order regardless, so output order is unaffected.
  all_subjs <- sort(unique(long_dt$subjid))
  n_subjs   <- length(all_subjs)

  # Cap the number of checkpoints at 9,999 (checkpoint files are numbered with
  # four digits, checkpoint_NNNN.rds). A checkpoint_size that would exceed it is
  # increased to stay within the limit; checkpoint size does not affect results,
  # so this warns (when the user chose the size) rather than stopping.
  max_checkpoints <- .gc_max_checkpoints()
  if (ceiling(n_subjs / checkpoint_size) > max_checkpoints) {
    min_checkpoint_size <- ceiling(n_subjs / max_checkpoints)
    if (checkpoint_size_provided) {
      warning("checkpoint_size = ", checkpoint_size, " would produce ",
              ceiling(n_subjs / checkpoint_size), " checkpoints for ", n_subjs,
              " subjects, more than the ", max_checkpoints, "-checkpoint limit; ",
              "growthcleanr increased checkpoint_size to ", min_checkpoint_size,
              ". Results are not affected.", call. = FALSE)
    }
    checkpoint_size <- min_checkpoint_size
    .log("Auto-increased checkpoint_size to ", checkpoint_size,
         " to stay within ", max_checkpoints, " checkpoints for ", n_subjs, " subjects.")
  }

  checkpoints   <- split(all_subjs, ceiling(seq_along(all_subjs) / checkpoint_size))
  n_checkpoints <- length(checkpoints)

  .log("growthcleanr: cleaning in progress: ", format(n_subjs, big.mark = ","), " subjects, ",
       format(nrow(long_dt), big.mark = ","), " measurements, ",
       n_checkpoints, " checkpoint", if (n_checkpoints == 1L) "" else "s",
       " of up to ", format(checkpoint_size, big.mark = ","), " subjects",
       " | checkpoint_dir: ", normalizePath(checkpoint_dir, mustWork = FALSE))

  # ---- pre-load reference tables once across all checkpoints ----
  # Saves ~0.9 sec per cleangrowth() call. (Skipped if the caller already
  # supplied ref_tables.)
  if (is.null(clean_args$ref_tables)) clean_args$ref_tables <- gc_preload_refs()

  # ---- reset or reuse prior checkpoint files ----
  existing_ckpts <- list.files(checkpoint_dir, pattern = "^checkpoint_\\d{4}\\.rds$", full.names = TRUE)
  if (length(existing_ckpts) > 0) {
    if (checkpoint_resume) {
      .log("checkpoint_resume = TRUE: reusing ", length(existing_ckpts),
           " existing checkpoint file(s); checkpoints that already have one are skipped.")
    } else {
      file.remove(existing_ckpts)
      .log("Removed ", length(existing_ckpts),
           " checkpoint file(s) from a prior run (checkpoint_resume = FALSE).")
    }
  }

  # ---- process checkpoints ----
  for (i in seq_along(checkpoints)) {
    ckpt_file <- file.path(checkpoint_dir, sprintf("checkpoint_%04d.rds", i))
    if (checkpoint_resume && file.exists(ckpt_file)) {
      .log("checkpoint ", i, "/", n_checkpoints, ": skipped (already complete)")
      next
    }
    checkpoint_dt <- long_dt[long_dt$subjid %in% checkpoints[[i]], ]
    .banner(sprintf("CHECKPOINT %d / %d  |  %s subjects",
                    i, n_checkpoints, format(length(checkpoints[[i]]), big.mark = ",")))
    t_checkpoint_start <- Sys.time()
    # Each checkpoint is a fresh long-mode cleangrowth() with no checkpointing.
    # .checkpoint_driven = TRUE lets the runner own the per-checkpoint framing.
    result <- do.call(cleangrowth, c(
      list(data = checkpoint_dt, checkpoint = FALSE, checkpoint_dir = NULL,
           input_format = "long", output_format = "long",
           quietly = quietly, verbose = verbose, .checkpoint_driven = TRUE),
      clean_args))
    saveRDS(result, ckpt_file)
    .log("checkpoint ", i, "/", n_checkpoints, ": done | ",
         .fmt_dur(difftime(Sys.time(), t_checkpoint_start, units = "secs")),
         " | elapsed ",
         .fmt_dur(difftime(Sys.time(), t_run_start, units = "secs")))
  }

  # ---- combine all checkpoints in original input order ----
  ckpt_files <- file.path(checkpoint_dir, sprintf("checkpoint_%04d.rds", seq_len(n_checkpoints)))
  .log("postprocessing: combining ", n_checkpoints, " checkpoint", if (n_checkpoints == 1L) "" else "s")
  # A resumed run whose input changed after the checkpoint files were written
  # leaves rows that no checkpoint holds; say so in user terms (no internal names).
  .ckpt_missing_msg <- function(n_missing, n_total) {
    big <- function(x) format(x, big.mark = ",", trim = TRUE)
    if (checkpoint_resume)
      paste0(big(n_missing), " of ", big(n_total), " measurement(s) were not found in the ",
             "checkpoint files in checkpoint_dir (the input has changed since those files ",
             "were written), so their exclude is NA. If the change in data was not ",
             "intentional, rerun with checkpoint_resume = FALSE to clean every row.")
    else
      paste0(big(n_missing), " of ", big(n_total), " measurement(s) were not found in the ",
             "checkpoint files in checkpoint_dir, so their exclude is NA. Check the ",
             "checkpoint files for corruption.")
  }
  final <- .combine_checkpoints(ckpt_files, id_order = input_id_order,
                                .log = if (verbose) .log else function(...) invisible(NULL),
                                missing_msg = .ckpt_missing_msg)

  # Rows no checkpoint holds come back all-NA from the combine. Restore their
  # input values (final is in input-row order, one row per input row) so only the
  # result columns are NA -- growthcleanr never drops a submitted value.
  if (nrow(final) == nrow(long_dt)) {
    .ckpt_miss <- which(is.na(final[["id"]]) & !is.na(long_dt[["id"]]))
    if (length(.ckpt_miss) > 0L) {
      for (col in intersect(names(long_dt), names(final)))
        data.table::set(final, i = .ckpt_miss, j = col, value = long_dt[[col]][.ckpt_miss])
    }
  }

  if (nrow(final) != nrow(long_dt))
    warning("Combined result has ", nrow(final), " rows but input had ",
            nrow(long_dt), " rows. Check checkpoint files for corruption.")

  if (!keep_checkpoints) {
    file.remove(ckpt_files)
    .log("Checkpoint files removed")
  }

  .log("all ", n_checkpoints, " checkpoint", if (n_checkpoints == 1L) "" else "s",
       " complete | ", format(n_subjs, big.mark = ","), " subjects, ",
       format(nrow(final), big.mark = ","), " rows | total ",
       .fmt_dur(difftime(Sys.time(), t_run_start, units = "secs")))
  return(final)
}


# -- Main checkpoint function ---------------------------------------------------

#' Run cleangrowth() with automatic checkpointing (deprecated wrapper)
#'
#' \strong{Deprecated.} Checkpointing is built into \code{\link{cleangrowth}}
#' via its \code{checkpoint_dir} argument -- call
#' \code{cleangrowth(data, checkpoint_dir = ...)} directly. This wrapper forwards
#' to \code{cleangrowth()} and will be removed in a future version. The
#' \code{use_existing_checkpoint_results} argument maps to
#' \code{cleangrowth(checkpoint_resume = ...)}. Calling \code{cleangrowth()}
#' directly also lets a checkpointed run use wide / semi-wide input and output
#' (via its \code{input_format} / \code{output_format}).
#'
#' @param data,checkpoint_dir,checkpoint_size,log_file,keep_checkpoints,quietly,verbose
#'   See \code{\link{cleangrowth}}. \code{quietly} defaults to \code{FALSE} here
#'   (this wrapper prints by default).
#' @param use_existing_checkpoint_results Deprecated alias for
#'   \code{cleangrowth(checkpoint_resume = ...)}: reuse existing checkpoint files
#'   (resume an interrupted run) instead of starting fresh.
#' @param ... Additional arguments passed to \code{\link{cleangrowth}} (e.g.
#'   \code{cf_rescue}, \code{sd.recenter}, \code{adult_permissiveness}).
#'
#' @return A data.table -- see \code{\link{cleangrowth}} for the output columns.
#'
#' @seealso \code{\link{cleangrowth}}, \code{\link{cleangrowth_checkpoint_combine}}
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- fread("large_dataset.csv")
#'
#' # Preferred: call cleangrowth() directly with checkpoint_dir.
#' result <- cleangrowth(dt, checkpoint_dir = "results/gc_checkpoints",
#'                       checkpoint_size = 5000, log_file = "results/gc_run.log")
#' }
#'
#' @export
cleangrowth_checkpoint <- function(data,
                                   checkpoint_dir,
                                   checkpoint_size       = 5000,
                                   log_file         = NULL,
                                   keep_checkpoints = TRUE,
                                   use_existing_checkpoint_results = FALSE,
                                   quietly          = FALSE,
                                   verbose          = FALSE,
                                   ...) {

  if (isTRUE(verbose)) quietly <- FALSE

  if (!is.data.frame(data)) data <- as.data.table(data)

  .Deprecated(
    new = "cleangrowth(checkpoint_dir = ...)",
    msg = paste0(
      "cleangrowth_checkpoint() is deprecated: checkpointing is now built into ",
      "cleangrowth() -- call cleangrowth(data, checkpoint_dir = ...) directly. ",
      "use_existing_checkpoint_results maps to checkpoint_resume."))

  # Forward to the folded interface. The one renamed argument is
  # use_existing_checkpoint_results -> checkpoint_resume; everything else keeps
  # its name. quietly defaults to FALSE here, the same as cleangrowth().
  cleangrowth(data,
              checkpoint_dir    = checkpoint_dir,
              checkpoint_size        = checkpoint_size,
              checkpoint_resume = use_existing_checkpoint_results,
              keep_checkpoints  = keep_checkpoints,
              log_file          = log_file,
              quietly           = quietly,
              verbose           = verbose,
              ...)
}


# -- Standalone combine function ------------------------------------------------

#' Combine saved checkpoint files from a checkpointed cleangrowth() run
#'
#' Reads all checkpoint \code{.rds} files from a directory and combines them
#' into a single data.table. Use this when a checkpointed \code{\link{cleangrowth}}
#' run (\code{checkpoint_dir} set) completed all checkpoints but the combine
#' step failed - you can re-combine
#' without re-running the algorithm.
#'
#' @param checkpoint_dir Directory containing checkpoint \code{.rds} files
#'   named \code{checkpoint_0001.rds}, \code{checkpoint_0002.rds}, etc.
#' @param id_order Optional vector of \code{id} values specifying the desired
#'   output row order (typically \code{your_data$id}). If \code{NULL}, rows
#'   are returned in checkpoint order (subjects grouped together but not in the
#'   original row order).
#' @param log_file Optional path to append log messages.
#' @param quietly Logical (default FALSE). When FALSE, prints a short start and
#'   completion line (and writes them to \code{log_file} if set). When TRUE, the
#'   console is silent; \code{log_file} still receives the log.
#' @param verbose Logical (default FALSE). When TRUE, additionally prints the
#'   per-file read progress. Implies \code{quietly = FALSE}.
#'
#' @return A long-format data.table with growthcleanr's standard column names
#'   (\code{id}, \code{subjid}, \code{param}, \code{agedays}, \code{sex},
#'   \code{measurement}, plus result columns). Unlike \code{cleangrowth()}, it
#'   does not restore the user's column names, carried-through columns, or
#'   wide/semi-wide layout; if growthcleanr generated \code{id} values (because
#'   \code{id} was missing or not unique), \code{id} holds those generated values.
#'
#' @details
#' All \code{.rds} files matching \code{checkpoint_NNNN.rds} in
#' \code{checkpoint_dir} are read and combined in numeric order. Every matching
#' file is included, so the directory should hold checkpoints from a single run
#' only. (A checkpointed \code{cleangrowth()} run with \code{checkpoint_resume =
#' FALSE}, the default, removes earlier checkpoint files from the directory at
#' the start, so after a run only that run's files remain.)
#'
#' @seealso \code{\link{cleangrowth_checkpoint}}
#'
#' @examples
#' \dontrun{
#' # If the combine step failed at the end of a run, re-combine like this:
#' result <- cleangrowth_checkpoint_combine(
#'   checkpoint_dir = "results/gc_checkpoints",
#'   id_order       = dt$id   # original row order; needs a unique id column named id
#' )
#' saveRDS(result, "results/gc_output.rds")
#' }
#'
#' @export
cleangrowth_checkpoint_combine <- function(checkpoint_dir,
                                            id_order = NULL,
                                            log_file = NULL,
                                            quietly  = FALSE,
                                            verbose  = FALSE) {

  if (isTRUE(verbose)) quietly <- FALSE

  if (!dir.exists(checkpoint_dir))
    stop("checkpoint_dir does not exist: ", checkpoint_dir)

  t_start <- Sys.time()
  .fmt_dur <- function(secs) {
    secs <- as.numeric(secs)
    if (secs < 60) return(sprintf("%.0fs", secs))
    if (secs < 3600) return(sprintf("%dm%02ds", secs %/% 60, round(secs %% 60)))
    sprintf("%dh%02dm", secs %/% 3600, (secs %% 3600) %/% 60)
  }
  .log <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
    if (!quietly) message(msg)
    if (!is.null(log_file))
      cat(msg, "\n", file = log_file, append = TRUE)
  }

  # Auto-detect checkpoint files in numeric order
  all_files <- list.files(checkpoint_dir,
                           pattern = "^checkpoint_\\d{4}\\.rds$",
                           full.names = TRUE)
  if (length(all_files) == 0)
    stop("No checkpoint_NNNN.rds files found in: ", checkpoint_dir)

  ckpt_files <- sort(all_files)   # lexicographic == numeric for zero-padded names
  .log("combining ", length(ckpt_files),
       " checkpoint file(s) in ", normalizePath(checkpoint_dir))

  # Per-file read progress is verbose-only.
  final <- .combine_checkpoints(ckpt_files, id_order = id_order,
                                .log = if (verbose) .log else function(...) invisible(NULL))

  .log("done \u00b7 ", format(nrow(final), big.mark = ","), " rows \u00b7 ",
       .fmt_dur(difftime(Sys.time(), t_start, units = "secs")))
  return(final)
}
