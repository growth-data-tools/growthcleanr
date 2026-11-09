# Transform data in growthcleanr format into wide structure for BMI calculation

`longwide` transforms data from long to wide format. Ideal for
transforming output from growthcleanr::cleangrowth() into a format
suitable for growthcleanr::ext_bmiz().

## Usage

``` r
longwide(
  long_df,
  id = "id",
  subjid = "subjid",
  sex = "sex",
  agedays = "agedays",
  param = "param",
  measurement = "measurement",
  gcr_result = "exclude",
  include_all = FALSE,
  inclusion_types = c("Include"),
  extra_cols = NULL,
  keep_unmatched_data = FALSE,
  sex_coding = .gc_default_sex_coding,
  param_coding = NULL
)
```

## Arguments

- long_df:

  A data frame (or data.table) to be transformed. Expects columns: id,
  subjid, sex, agedays, param, measurement, and an exclusion column from
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  (named `"exclude"` by default; the
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  output `data.table` can be passed directly).

- id:

  name of observation ID column

- subjid:

  name of subject ID column

- sex:

  name of sex descriptor column

- agedays:

  name of age (in days) descriptor column

- param:

  name of parameter column to identify each type of measurement

- measurement:

  name of measurement column containing the actual measurement data

- gcr_result:

  name of column of results from growthcleanr::cleangrowth(). Defaults
  to `"exclude"`, the name of the exclusion column in
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  output. If your exclusion column has a different name, pass it
  explicitly. If the named column is missing and the default `"exclude"`
  was used, the error says so and suggests `gcr_result = "gcr_result"`
  (the column name 2.x scripts used).

- include_all:

  Determines whether the function keeps all exclusion codes. If TRUE,
  all exclusion types are kept and the inclusion_types argument is
  ignored. Defaults to FALSE.

- inclusion_types:

  Vector indicating which exclusion codes from the cleaning algorithm
  should be included in the data, given that include_all is FALSE. For
  all options, see growthcleanr::cleangrowth(). Defaults to
  c("Include"). A value that is not a growthcleanr version 3 exclusion
  code (for example the 2.x code `"Exclude-Carried-Forward"`) matches no
  rows, so `longwide()` warns and points to the code mapping in
  [`vignette("experienced-start-here")`](https://growthcleanr.org/articles/experienced-start-here.md).

- extra_cols:

  Vector of additional columns to include in the output. If a column C1
  differs on agedays matched height and weight values, then include
  separate ht_C1 and wt_C1 columns as well as a match_C1 column that
  gives booleans indicating where ht_C1 and wt_C1 are the same. If the
  agedays matched height and weight columns are identical, then only
  include a single version of C1. Defaults to empty vector (not keeping
  any additional columns).

- keep_unmatched_data:

  boolean indicating whether to keep height/weight observations that do
  not have a matching weight/height on that day

- sex_coding:

  Named list with elements `male` and `female` giving the sex codes used
  in `long_df`, exactly as for
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md).
  The default recognizes the same codes
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  does (0/1, m/M, f/F, b/g, male/female, boy/girl; case- and
  whitespace-insensitive). If you passed a `sex_coding` to
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md),
  pass the same one here. Sex is output as M or F, which
  [`ext_bmiz()`](https://growthcleanr.org/reference/ext_bmiz.md) reads
  directly. As in
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md),
  with the default coding `longwide()` stops if `sex` has only 1s and
  2s, and gives a prominent warning if every value is 1 across more than
  one subject; passing `sex_coding` removes both. A warning is also
  given if any retained row's sex is missing or not recognized (its sex
  becomes NA, so
  [`ext_bmiz()`](https://growthcleanr.org/reference/ext_bmiz.md) cannot
  compute BMI z-scores for it).

- param_coding:

  Optional named list mapping your parameter labels to growthcleanr's
  canonical parameters, exactly as for
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  (e.g. `list(WEIGHTKG = "wt", HEIGHTCM = "ht")`); pass the same one you
  gave
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md).
  Parameters are read the same way
  [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
  reads them: imperial values (`HEIGHTIN`, `LENGTHIN`, `WEIGHTLBS`) are
  converted to cm/kg and `LENGTHCM` is treated as height, so `ht` and
  `wt` are always metric. Head circumference rows are not used; rows
  with an unrecognized parameter are dropped with a warning.

## Value

Returns a data frame transformed from long to wide. Includes only values
flagged with indicated inclusion types. Potentially includes additional
columns if arguments are passed to extra_cols. For each subject, heights
without corresponding weights for a given age (and vice versa) will be
dropped unless keep_unmatched_data is set to TRUE. The subject and age
columns keep the names you passed as `subjid` and `agedays` (for example
`patid`); the other columns are `agey`, `agem`, `sex` (recoded to M or
F), `ht_id`, `wt_id`, `wt` (kg), and `ht` (cm). If no rows are kept, a
data frame with these columns and no rows is returned.

## Examples

``` r
# \donttest{
# Run on a small subset of given data
df <- syngrowth[syngrowth$subjid %in% unique(syngrowth$subjid)[1:5], ]
gc_out <- cleangrowth(df)
#> [14:18:16] growthcleanr: cleaning in progress — 5 subjects, 93 measurements, 1 batch of up to 5,000 subjects
#> ************************************************************
#> growthcleanr cleaning summary  (93 measurements)
#>   Weight      : 45 included (80.4%), 11 excluded (19.6%)
#>   Height      : 32 included (86.5%), 5 excluded (13.5%)
#>   Set aside   : 0 (0.0%)
#> ************************************************************
df$exclude <- gc_out$exclude[match(df$id, gc_out$id)]

# Convert to wide format (default gcr_result = "exclude")
wide_df <- longwide(df)

# Include all inclusion types
wide_df <- longwide(df, include_all = TRUE)

# Specify which exclusion codes to retain (default `exclude` column uses
# the Summary codes; pass `exclude_detail = TRUE` to cleangrowth() and use
# `gcr_result = "exclude_detailed"` for the step-level Detailed codes)
wide_df <- longwide(df, inclusion_types = c("Include", "Exclude-CF"))
# }
```
