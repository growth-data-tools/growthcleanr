# Trial run of growthcleanr on a sample of your data

`gc_runtrial()` helps you confirm that your data are formatted correctly
and that growthcleanr is reading your columns, sex coding, and
parameters the way you intend – before you commit to a full
[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md) run.
It draws a small sample of patients, runs
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md) on
them with the exact configuration you pass, and prints a short
diagnostic report: how the input was interpreted, how sex and parameters
were mapped (the two highest-risk settings, shown prominently), anything
growthcleanr flagged while reading, and a summary of the cleaning
result. The cleaned sample is returned invisibly for inspection. For a
ready-to-run script, see "Check your setup first" in
[`vignette("example-scripts")`](https://growthcleanr.org/articles/example-scripts.md).

## Usage

``` r
gc_runtrial(
  data = NULL,
  n = 100,
  sampling = c("random", "first"),
  seed = NULL,
  ...
)
```

## Arguments

- data:

  A data.frame/data.table of growth measurements, in the same shape you
  would pass to
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  (long, semi-wide, or wide – set `input_format` via `...` for the
  latter two).

- n:

  Number of patients (unique subjects) to sample. Default 100. If the
  data hold fewer subjects, all are used.

- sampling:

  `"random"` (default) or `"first"` (the first `n` subjects in order of
  appearance).

- seed:

  Optional integer seed for a reproducible random sample. The seed used
  is reported; your session's random-number state is left unchanged.

- ...:

  Any other
  [`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md)
  argument (`subjid`, `param`, `agedays`, `sex`, `measurement`,
  `input_format`, `sex_coding`, `param_coding`, `semiwide_cols`,
  `wide_cols`, `dob`, `visit_date`, ...), passed straight through so the
  trial tests your real configuration.

## Value

Invisibly, a data.table: the cleaned sample in long format on success
(empty on a configuration error), with the structured diagnostic
attached as `attr(x, "gc_trial")`. Prints the report as a side effect.

## Details

Sampling is by *patient*: all of a sampled subject's rows are kept, so
trajectories stay intact. Random sampling (the default) is effectively
free – the only real cost is running growthcleanr on the sampled
patients, which is the same whether they were chosen at random or as the
first `n`. Pass a `seed` for a reproducible random sample, or
`sampling = "first"` for a deterministic peek that needs no seed.

Only the data-frame calling style is supported (pass your table as
`data`); vector-style input is not. The trial always analyses and
returns results in long format so the diagnostic is well defined,
regardless of any `output_format` you set.

If your configuration makes
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
stop (for example a forgotten `input_format`, or a sex coding it will
not guess), the trial reports that stop as the headline finding instead
of re-throwing it – turning a hard error into setup guidance. The sex
and parameter mapping sections still print, since they are derived from
your configuration and data rather than the run.

## Examples

``` r
# \donttest{
# quick check that a dataset is set up right
gc_runtrial(syngrowth)
#> === growthcleanr trial run ===
#> 
#>   *** TRIAL RUN SUCCESSFUL *** -- growthcleanr cleaned the sample; review the details below.
#> 
#> SAMPLE
#>   Patients sampled : 100 of 2,697
#>   Sampling         : random (seed: none (results vary run to run; pass seed = to reproduce))
#>   Rows in sample   : 2,955
#> 
#> HOW YOUR INPUT WAS READ
#>   Input format     : long
#>   Columns          : subjid = 'subjid', param = 'param', sex = 'sex', measurement = 'measurement'
#>   Age source       : agedays column ('agedays')
#>   Age range        : 0 to 25,312 days
#> 
#> SEX  [Key column: recommend reviewing this mapping to ensure it is correct]
#>   Male   <- 0, m, b, male, boy
#>   Female <- 1, f, g, female, girl
#>   Unrecognized values in the 'sex' column : (none)
#>   Count of measurements : 1,022 male, 1,933 female, 0 unrecognized
#> 
#> PARAMETERS  [Key column: recommend reviewing this mapping to ensure it is correct]
#>   Weight             <- WEIGHTKG, WEIGHTLBS
#>   Height             <- HEIGHTCM, LENGTHCM, HEIGHTIN, LENGTHIN
#>   Head circumference <- HEADCM, HEADIN
#>   Unrecognized values in the 'param' column : (none)
#>   Count of measurements : 1,605 weight, 1,350 height, 0 head circumference; 0 unrecognized
#> 
#> FLAGGED WHILE READING
#>   (Nothing flagged)
#> 
#> CLEANING RESULT (2,955 measurements in sample)
#>   Include                      2,224 (75.3%)
#>   Exclude-Extraneous           484 (16.4%)
#>   Exclude-CF                   140 (4.7%)
#>   Exclude-Identical            64 (2.2%)
#>   Exclude-Pattern              25 (0.8%)
#>   Exclude-Hard-Limit           15 (0.5%)
#>   Exclude-Extreme              3 (0.1%)
#> 
#>   *** TRIAL RUN SUCCESSFUL ***
#> 
# }

if (FALSE) { # \dontrun{
# semi-wide data, custom sex coding, reproducible 50-patient sample
# (my_visits stands in for your own semi-wide dataset)
gc_runtrial(my_visits, n = 50, seed = 1,
            input_format = "semi_wide",
            sex_coding = list(male = "M", female = "F"))
} # }
```
