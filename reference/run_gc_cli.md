# Run growthcleanr from the command line (CSV in, CSV out)

Testable core behind the `inst/scripts/gcdriver.R` command-line driver.
Reads a CSV, runs
[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md) on it
(data-frame style), and writes the result to a CSV. Only the options in
`opts` are forwarded, so
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)'s
own defaults apply to everything else.

## Usage

``` r
run_gc_cli(infile, outfile, opts = list())
```

## Arguments

- infile:

  Path to the input CSV. Read with
  [`data.table::fread`](https://rdrr.io/pkg/data.table/man/fread.html).
  Must use the canonical column names (`subjid`, `param`, `agedays`,
  `sex`, `measurement`, optional `id`) unless remapped via the
  column-name options in `opts`.

- outfile:

  Path to write the cleaned result to, via
  [`data.table::fwrite`](https://rdrr.io/pkg/data.table/man/fwrite.html).
  Overwritten if it exists.

- opts:

  Named list of arguments forwarded verbatim to
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  (already coerced to the right types). Typically built from
  command-line flags by the driver; may be supplied directly in tests or
  from other R code. An empty list runs
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  with all defaults.

## Value

The output path, invisibly.

## See also

[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md); the
command-line driver `inst/scripts/gcdriver.R`.

## Examples

``` r
if (FALSE) { # \dontrun{
run_gc_cli("visits.csv", "cleaned.csv",
           opts = list(adult_cutpoint = 18, quietly = TRUE))
} # }
```
