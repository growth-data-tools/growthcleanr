# Compute observation-level percentage criterion limit. Vectorized over meas.

Compute observation-level percentage criterion limit. Vectorized over
meas.

## Usage

``` r
compute_perc_limit(meas, perclimit_low, perclimit_mid, perclimit_high)
```

## Arguments

- meas:

  Weight measurements in kg

- perclimit_low:

  Limit for wt \<= 45 kg

- perclimit_mid:

  Limit for 45 \< wt \<= 80 kg

- perclimit_high:

  Limit for wt \> 80 kg (0 = disabled: percewma \< 0 is never TRUE)

## Value

Numeric vector of perc_limits
