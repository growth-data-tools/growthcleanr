# Exponentially Weighted Moving Average (EWMA)

`ewma` calculates the exponentially weighted moving average (EWMA) for a
set of numeric observations over time.

## Usage

``` r
ewma(agedays, z, ewma.exp, ewma.adjacent = TRUE, window = 15, cache_env = NULL)
```

## Arguments

- agedays:

  Vector of age in days for each z score (potentially transformed to
  adjust weighting).

- z:

  Input vector of numeric z-score data.

- ewma.exp:

  Exponent used for age-gap weighting. May be a single scalar (recycled
  across all observations) or a per-observation vector matched to
  `agedays` / `z`; the algorithm's main internal callers pass a
  per-observation vector that varies the exponent by the widest neighbor
  age gap.

- ewma.adjacent:

  Specify whether EWMA values excluding adjacent measurements should be
  calculated. Defaults to TRUE.

- window:

  Maximum number of observations on each side for EWMA weighting.
  Default 15. Set to Inf to disable windowing.

- cache_env:

  Optional environment for caching EWMA intermediate results. Used
  internally for performance. Defaults to NULL.

## Value

A named list of numeric vectors, each the same length as `agedays`.

- `ewma.all` - EWMA at each observation's age, excluding only the
  observation itself.

- `ewma.before` - also excludes the immediate prior observation (sorted
  by `agedays`); equals `ewma.all` for the first observation and when
  `n <= 2`.

- `ewma.after` - also excludes the immediate subsequent observation;
  equals `ewma.all` for the last observation and when `n <= 2`.

When `ewma.adjacent = FALSE`, only `ewma.all` is returned (still wrapped
in a one-element named list).

## Examples

``` r
# Run on 1 subject, 1 type of parameter
df_stats <- as.data.frame(syngrowth)
df_stats <- df_stats[df_stats$subjid == df_stats$subjid[1] &
                       df_stats$param == "HEIGHTCM", ]

# Get the uncentered z-scores
measurement_to_z <- read_anthro(cdc.only = TRUE)
sd <- measurement_to_z(df_stats$param,
                       df_stats$agedays,
                       df_stats$sex,
                       df_stats$measurement,
                       TRUE)

# Calculate exponentially weighted moving average
e_df <- ewma(df_stats$agedays, sd, ewma.exp = -1.5)
```
