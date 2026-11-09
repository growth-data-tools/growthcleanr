# Remove implausible weight values using 7-step moderate EWMA flow.

Remove implausible weight values using 7-step moderate EWMA flow.

## Usage

``` r
remove_mod_ewma_wt(
  full_inc_df,
  exc_label = "Exclude-A-Traj-Moderate",
  max_rounds = 100,
  wtallow_formula = "piecewise",
  ewma_window = 15,
  mod_ewma_f = 0.75,
  perclimit_low = 0.5,
  perclimit_mid = 0.4,
  perclimit_high = 0,
  endpair_gap_days = 90
)
```

## Arguments

- full_inc_df:

  Data frame for one subject with columns: internal_id, age_days,
  ageyears, meas_m (weight in kg). Must have \>= 3 rows.

- exc_label:

  Character prefix for exclusion codes

- max_rounds:

  Maximum number of rounds (default 100)

- wtallow_formula:

  Formula for wtallow ("piecewise", "piecewise-lower", "allofus15")

- ewma_window:

  Maximum number of positions on each side included in the EWMA (default
  15)

- mod_ewma_f:

  Directional factor: both directional (before/after) deviations must
  exceed mod_ewma_f x wtallow (default 0.75)

- perclimit_low, perclimit_mid, perclimit_high:

  Percentage-limit settings passed to
  [`compute_perc_limit()`](https://growthcleanr.org/reference/compute_perc_limit.md)

- endpair_gap_days:

  Max within-pair age gap (days) for the end-pair median guard (default
  90).

## Value

Named character vector: internal_id -\> exclusion code for excluded
values.
