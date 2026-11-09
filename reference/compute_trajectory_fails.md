# Pre-computes trajectory rescue for all observations. Returns TRUE = fails all rescue (not rescued).

Pre-computes trajectory rescue for all observations. Returns TRUE =
fails all rescue (not rescued).

## Usage

``` r
compute_trajectory_fails(meas, age_days, err = 5)
```

## Arguments

- meas:

  Weights (kg), age-sorted

- age_days:

  Ages in days

- err:

  Tolerance in kg (default 5)
