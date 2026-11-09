# Incremental (partial) growthcleanr run

Re-clean only the subjects whose data changed since a previous
[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md) run,
and reuse the earlier cleaned results for everyone else. This is
intended for workflows that repeatedly clean a mostly unchanging dataset
– for example a simulation loop that perturbs a subset of subjects and
re-cleans – where a full re-run would waste most of its work.

## Usage

``` r
gc_partial_run(new_data, cached_output, changed_subjids, ...)
```

## Arguments

- new_data:

  A long-format `data.frame`/`data.table` of the current data, using
  growthcleanr's canonical column names (`subjid`, `param`, `agedays`,
  `sex`, `measurement`) plus a per-row `id`.

- cached_output:

  The `data.table` returned by a previous
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  run on the earlier version of this dataset.

- changed_subjids:

  Required. A vector of `subjid` values whose data changed since
  `cached_output` was produced and must be re-cleaned. Any subject
  present in `new_data` but not in `cached_output` (added) is re-cleaned
  automatically; any subject in `cached_output` but not in `new_data`
  (removed) is dropped. May be length 0 (re-clean only added subjects,
  if any). Values not present in `new_data` are ignored.

- ...:

  Passed verbatim to the inner
  [`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md)
  call on the changed subset (e.g. `checkpoint_dir`, `checkpoint_size`,
  `batch_size`, `cf_rescue`, output switches). Must match the options
  used to produce `cached_output`.

## Value

A `data.table` with the same columns as
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
would return under the same `...`, containing every subject in
`new_data`, ordered by `id`.

## Details

`gc_partial_run()` is a thin wrapper: it slices `new_data` down to the
changed (and any newly added) subjects, hands that subset to a single
ordinary
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
call, and row-binds the freshly cleaned rows to the retained rows of
`cached_output`. All arguments in `...` – including `checkpoint_dir`,
`checkpoint_size`, `batch_size`, and every output-column switch – are
forwarded verbatim to that inner
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
call, so a large changed subset can still be checkpointed. The cache
itself is never passed into
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md);
the merge happens entirely at the output level.

**Why the result is correct.** After reference-based recentering,
growthcleanr cleans each subject independently of every other subject,
so cleaning only the changed subjects yields the exact same rows those
subjects would receive in a full run. The combined result is therefore
identical to `cleangrowth(new_data, ...)` for a given set of options
(see Examples).

**Caller responsibilities.**

- `...` must specify the *same* options used to produce `cached_output`
  (same `cf_rescue`, permissiveness, output switches, etc.). If the
  requested output columns differ from those in `cached_output`, the
  function stops rather than silently produce `NA`-filled rows.

- `id` must be present and unique per row in both `new_data` and
  `cached_output`, and must be stable across runs (the same measurement
  keeps the same `id`). `id` is how retained cached rows and freshly
  cleaned rows are ordered in the output.

- Input must be long format (one measurement per row). Wide and
  semi-wide input are not supported by this wrapper.

## See also

[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md)

## Examples

``` r
# \donttest{
# Baseline clean of the full dataset
baseline <- cleangrowth(syngrowth)
#> [14:12:10] growthcleanr: cleaning in progress — 2,697 subjects, 77,721 measurements, 1 batch of up to 5,000 subjects
#> ************************************************************
#> growthcleanr cleaning summary  (77,721 measurements)
#>   Weight      : 33,631 included (78.2%), 9,348 excluded (21.8%)
#>   Height      : 26,318 included (75.8%), 8,424 excluded (24.2%)
#>   Set aside   : 0 (0.0%)
#> ************************************************************

# Suppose one subject's data was corrected; re-clean only that subject
changed <- syngrowth$subjid[1]
updated <- gc_partial_run(syngrowth, baseline, changed_subjids = changed)
#> [14:14:08] growthcleanr: cleaning in progress — 1 subjects, 26 measurements, 1 batch of up to 5,000 subjects
#> ************************************************************
#> growthcleanr cleaning summary  (26 measurements)
#>   Weight      : 10 included (71.4%), 4 excluded (28.6%)
#>   Height      : 10 included (83.3%), 2 excluded (16.7%)
#>   Set aside   : 0 (0.0%)
#> ************************************************************

# Identical to a full re-run (same options), for every subject
full <- cleangrowth(syngrowth)
#> [14:14:09] growthcleanr: cleaning in progress — 2,697 subjects, 77,721 measurements, 1 batch of up to 5,000 subjects
#> ************************************************************
#> growthcleanr cleaning summary  (77,721 measurements)
#>   Weight      : 33,631 included (78.2%), 9,348 excluded (21.8%)
#>   Height      : 26,318 included (75.8%), 8,424 excluded (24.2%)
#>   Set aside   : 0 (0.0%)
#> ************************************************************
identical(updated[order(id)]$exclude, full[order(id)]$exclude)
#> [1] TRUE
# }
```
