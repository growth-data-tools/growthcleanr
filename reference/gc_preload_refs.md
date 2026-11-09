# Pre-load growthcleanr reference closures

`gc_preload_refs` loads all reference table closures used by
[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md) once,
avoiding repeated disk reads across repeated calls (e.g., simulation
loops). Pass the result as the `ref_tables` argument to
[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md).

## Usage

``` r
gc_preload_refs(path = "")
```

## Arguments

- path:

  Path to the reference data directory. Defaults to the package extdata
  directory. Pass an explicit path only if using custom reference files.

## Value

A named list with two `read_anthro` closures:

- mtz_cdc_prelim:

  CDC-only (used by child algorithm)

- mtz_who_prelim:

  WHO+CDC (used by child algorithm)

## Examples

``` r
# \donttest{
refs <- gc_preload_refs()
result <- cleangrowth(
  subjid = syngrowth$subjid,
  param  = syngrowth$param,
  agedays = syngrowth$agedays,
  sex = syngrowth$sex,
  measurement = syngrowth$measurement,
  ref_tables = refs
)
#> Warning: Generated ids
#> No `id` was supplied; growthcleanr assigned sequential ids 1:N in input row order.
#> growthcleanr uses an `id` to break ties deterministically when measurements are equally plausible (e.g. some extraneous-value selection): it prefers the higher `id` (a same-day remeasurement may be more accurate), except at birth (agedays 0), where it prefers the lowest. Supply your own `id` for exact reproducibility across datasets and control over these selections.
#> [14:16:09] growthcleanr: cleaning in progress — 2,697 subjects, 77,721 measurements, 1 batch of up to 5,000 subjects
#> ************************************************************
#> growthcleanr cleaning summary  (77,721 measurements)
#>   Weight      : 33,631 included (78.2%), 9,348 excluded (21.8%)
#>   Height      : 26,318 included (75.8%), 8,424 excluded (24.2%)
#>   Set aside   : 0 (0.0%)
#> ************************************************************
# }
```
