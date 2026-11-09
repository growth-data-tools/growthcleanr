# Run cleangrowth() with automatic checkpointing (deprecated wrapper)

**Deprecated.** Checkpointing is built into
[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md) via
its `checkpoint_dir` argument – call
`cleangrowth(data, checkpoint_dir = ...)` directly. This wrapper
forwards to
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md) and
will be removed in a future version. The
`use_existing_checkpoint_results` argument maps to
`cleangrowth(checkpoint_resume = ...)`. Calling
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
directly also lets a checkpointed run use wide / semi-wide input and
output (via its `input_format` / `output_format`).

## Usage

``` r
cleangrowth_checkpoint(
  data,
  checkpoint_dir,
  checkpoint_size = 5000,
  log_file = NULL,
  keep_checkpoints = TRUE,
  use_existing_checkpoint_results = FALSE,
  quietly = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- data, checkpoint_dir, checkpoint_size, log_file, keep_checkpoints,
  quietly, verbose:

  See
  [`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md).
  `quietly` defaults to `FALSE` here (this wrapper prints by default).

- use_existing_checkpoint_results:

  Deprecated alias for `cleangrowth(checkpoint_resume = ...)`: reuse
  existing checkpoint files (resume an interrupted run) instead of
  starting fresh.

- ...:

  Additional arguments passed to
  [`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md)
  (e.g. `cf_rescue`, `sd.recenter`, `adult_permissiveness`).

## Value

A data.table – see
[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md) for
the output columns.

## See also

[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md),
[`cleangrowth_checkpoint_combine`](https://growthcleanr.org/reference/cleangrowth_checkpoint_combine.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
dt <- fread("large_dataset.csv")

# Preferred: call cleangrowth() directly with checkpoint_dir.
result <- cleangrowth(dt, checkpoint_dir = "results/gc_checkpoints",
                      checkpoint_size = 5000, log_file = "results/gc_run.log")
} # }
```
