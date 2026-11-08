#' Incremental (partial) growthcleanr run
#'
#' Re-clean only the subjects whose data changed since a previous
#' \code{\link{cleangrowth}} run, and reuse the earlier cleaned results for
#' everyone else. This is intended for workflows that repeatedly clean a mostly
#' unchanging dataset -- for example a simulation loop that perturbs a subset of
#' subjects and re-cleans -- where a full re-run would waste most of its work.
#'
#' \code{gc_partial_run()} is a thin wrapper: it slices \code{new_data} down to
#' the changed (and any newly added) subjects, hands that subset to a single
#' ordinary \code{cleangrowth()} call, and row-binds the freshly cleaned rows to
#' the retained rows of \code{cached_output}. All arguments in \code{...} --
#' including \code{checkpoint_dir}, \code{checkpoint_size}, \code{batch_size}, and
#' every output-column switch -- are forwarded verbatim to that inner
#' \code{cleangrowth()} call, so a large changed subset can still be
#' checkpointed. The cache itself is never passed into \code{cleangrowth()};
#' the merge happens entirely at the output level.
#'
#' \strong{Why the result is correct.} After reference-based recentering,
#' growthcleanr cleans each subject independently of every other subject, so
#' cleaning only the changed subjects yields the exact same rows those subjects
#' would receive in a full run. The combined result is therefore identical to
#' \code{cleangrowth(new_data, ...)} for a given set of options (see Examples).
#'
#' \strong{Caller responsibilities.}
#' \itemize{
#'   \item \code{...} must specify the \emph{same} options used to produce
#'     \code{cached_output} (same \code{cf_rescue}, permissiveness, output
#'     switches, etc.). If the requested output columns differ from those in
#'     \code{cached_output}, the function stops rather than silently produce
#'     \code{NA}-filled rows.
#'   \item \code{id} must be present and unique per row in both
#'     \code{new_data} and \code{cached_output}, and must be stable across runs
#'     (the same measurement keeps the same \code{id}). \code{id} is how retained
#'     cached rows and freshly cleaned rows are ordered in the output.
#'   \item Input must be long format (one measurement per row). Wide and
#'     semi-wide input are not supported by this wrapper.
#' }
#'
#' @param new_data A long-format \code{data.frame}/\code{data.table} of the
#'   current data, using growthcleanr's canonical column names (\code{subjid},
#'   \code{param}, \code{agedays}, \code{sex}, \code{measurement}) plus a
#'   per-row \code{id}.
#' @param cached_output The \code{data.table} returned by a previous
#'   \code{cleangrowth()} run on the earlier version of this dataset.
#' @param changed_subjids Required. A vector of \code{subjid} values whose data
#'   changed since \code{cached_output} was produced and must be re-cleaned. Any
#'   subject present in \code{new_data} but not in \code{cached_output} (added)
#'   is re-cleaned automatically; any subject in \code{cached_output} but not in
#'   \code{new_data} (removed) is dropped. May be length 0 (re-clean only added
#'   subjects, if any). Values not present in \code{new_data} are ignored.
#' @param ... Passed verbatim to the inner \code{\link{cleangrowth}} call on the
#'   changed subset (e.g. \code{checkpoint_dir}, \code{checkpoint_size},
#'   \code{batch_size}, \code{cf_rescue}, output switches). Must match the
#'   options used to produce \code{cached_output}.
#'
#' @return A \code{data.table} with the same columns as \code{cleangrowth()}
#'   would return under the same \code{...}, containing every subject in
#'   \code{new_data}, ordered by \code{id}.
#'
#' @seealso \code{\link{cleangrowth}}
#'
#' @examples
#' \donttest{
#' # Baseline clean of the full dataset
#' baseline <- cleangrowth(syngrowth)
#'
#' # Suppose one subject's data was corrected; re-clean only that subject
#' changed <- syngrowth$subjid[1]
#' updated <- gc_partial_run(syngrowth, baseline, changed_subjids = changed)
#'
#' # Identical to a full re-run (same options), for every subject
#' full <- cleangrowth(syngrowth)
#' identical(updated[order(id)]$exclude, full[order(id)]$exclude)
#' }
#'
#' @export
gc_partial_run <- function(new_data, cached_output, changed_subjids, ...) {

  if (missing(changed_subjids) || is.null(changed_subjids)) {
    stop("changed_subjids must be supplied: give the subjid values whose data ",
         "changed since cached_output was produced (use character(0) to re-clean ",
         "only newly added subjects).", call. = FALSE)
  }

  new_dt <- data.table::as.data.table(new_data)
  cached <- data.table::as.data.table(cached_output)

  # Required columns. id is required (not auto-regenerated here) because it is
  # the only stable key linking retained cached rows to freshly cleaned rows.
  for (col in c("subjid", "id")) {
    if (!(col %in% names(new_dt)))
      stop(sprintf("new_data must have a '%s' column (canonical long format).", col),
           call. = FALSE)
    if (!(col %in% names(cached)))
      stop(sprintf("cached_output must have a '%s' column; pass the data.table ",
                   "returned by cleangrowth().", col), call. = FALSE)
  }
  if (anyDuplicated(new_dt$id))
    stop("new_data 'id' must be unique per row for a partial run.", call. = FALSE)
  if (anyDuplicated(cached$id))
    stop("cached_output 'id' must be unique per row for a partial run.", call. = FALSE)

  # Which subjects to re-clean: explicitly changed (that are actually present in
  # new_data) plus any newly added subjects. Removed subjects (in cache, not in
  # new_data) are dropped by the keep filter below.
  new_subj    <- unique(as.character(new_dt$subjid))
  cached_subj  <- unique(as.character(cached$subjid))
  changed     <- as.character(changed_subjids)
  added       <- setdiff(new_subj, cached_subj)
  recompute   <- union(intersect(changed, new_subj), added)

  # Retained cached rows: subjects present in new_data and NOT being re-cleaned.
  keep_cached <- cached[as.character(subjid) %in% new_subj &
                          !(as.character(subjid) %in% recompute)]

  # Nothing to re-clean -> return the retained cache (added subjects, if any,
  # are always in `recompute`, so this branch means the subject set is a subset
  # of the cache with no changes).
  if (length(recompute) == 0L) {
    return(keep_cached[order(id)])
  }

  # Clean only the changed subset. Checkpointing (and every other option) rides
  # in through `...`; cached_output is deliberately NOT passed to cleangrowth().
  subset_in <- new_dt[as.character(subjid) %in% recompute]
  # The all-1 sex warning judges a whole dataset, so it says nothing useful about
  # a subset of changed subjects (a single changed girl would trigger it).
  fresh <- data.table::as.data.table(withCallingHandlers(
    cleangrowth(subset_in, ...),
    warning = function(w) {
      if (grepl("in cleangrowth(): every value in `sex` is 1", conditionMessage(w), fixed = TRUE))
        invokeRestart("muffleWarning")
    }))

  # mean_ht is returned by default only when a run has adult rows, so the cache
  # and a changed subset can differ on it. The side without it had no adult rows,
  # so NA is its correct value.
  if ("mean_ht" %in% names(fresh) && !"mean_ht" %in% names(keep_cached)) {
    keep_cached[, mean_ht := NA_real_]
    data.table::setcolorder(keep_cached, names(fresh))
  } else if ("mean_ht" %in% names(keep_cached) && !"mean_ht" %in% names(fresh)) {
    fresh[, mean_ht := NA_real_]
    data.table::setcolorder(fresh, names(keep_cached))
  }

  # Schema guard: the retained cache and the fresh rows must have the same
  # columns, or the row-bind would NA-fill. A mismatch means cached_output was
  # produced with different output options than requested here.
  if (!setequal(names(fresh), names(keep_cached))) {
    only_fresh  <- setdiff(names(fresh), names(keep_cached))
    only_cached <- setdiff(names(keep_cached), names(fresh))
    stop("cached_output and the fresh clean have different columns, so they ",
         "cannot be merged. This means cached_output was produced with different ",
         "output options than were requested here -- re-run gc_partial_run() with ",
         "the same options used to build cached_output.",
         if (length(only_fresh))  sprintf("\n  Only in the fresh clean: %s.", paste(only_fresh, collapse = ", ")) else "",
         if (length(only_cached)) sprintf("\n  Only in cached_output: %s.", paste(only_cached, collapse = ", ")) else "",
         call. = FALSE)
  }

  out <- data.table::rbindlist(list(keep_cached, fresh), use.names = TRUE)
  out[order(id)]
}
