# Combine saved checkpoint files from a checkpointed cleangrowth() run

Reads all checkpoint `.rds` files from a directory and combines them
into a single data.table. Use this when a checkpointed
[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md) run
(`checkpoint_dir` set) completed all checkpoints but the combine step
failed - you can re-combine without re-running the algorithm.

## Usage

``` r
cleangrowth_checkpoint_combine(
  checkpoint_dir,
  id_order = NULL,
  log_file = NULL,
  quietly = FALSE,
  verbose = FALSE
)
```

## Arguments

- checkpoint_dir:

  Directory containing checkpoint `.rds` files named
  `checkpoint_0001.rds`, `checkpoint_0002.rds`, etc.

- id_order:

  Optional vector of `id` values specifying the desired output row order
  (typically `your_data$id`). If `NULL`, rows are returned in checkpoint
  order (subjects grouped together but not in the original row order).

- log_file:

  Optional path to append log messages.

- quietly:

  Logical (default FALSE). When FALSE, prints a short start and
  completion line (and writes them to `log_file` if set). When TRUE, the
  console is silent; `log_file` still receives the log.

- verbose:

  Logical (default FALSE). When TRUE, additionally prints the per-file
  read progress. Implies `quietly = FALSE`.

## Value

A long-format data.table with growthcleanr's standard column names
(`id`, `subjid`, `param`, `agedays`, `sex`, `measurement`, plus result
columns). Unlike
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md), it
does not restore the user's column names, carried-through columns, or
wide/semi-wide layout; if growthcleanr generated `id` values (because
`id` was missing or not unique), `id` holds those generated values.

## Details

All `.rds` files matching `checkpoint_NNNN.rds` in `checkpoint_dir` are
read and combined in numeric order. Every matching file is included, so
the directory should hold checkpoints from a single run only. (A
checkpointed
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md) run
with `checkpoint_resume = FALSE`, the default, removes earlier
checkpoint files from the directory at the start, so after a run only
that run's files remain.)

## See also

[`cleangrowth_checkpoint`](https://growthcleanr.org/reference/cleangrowth_checkpoint.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# If the combine step failed at the end of a run, re-combine like this:
result <- cleangrowth_checkpoint_combine(
  checkpoint_dir = "results/gc_checkpoints",
  id_order       = dt$id   # original row order; needs a unique id column named id
)
saveRDS(result, "results/gc_output.rds")
} # }
```
