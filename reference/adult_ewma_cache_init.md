# Initialize EWMA cache for a set of observations. Builds the full weight matrix once (O(n^2)), computes EWMA values.

Initialize EWMA cache for a set of observations. Builds the full weight
matrix once (O(n^2)), computes EWMA values.

## Usage

``` r
adult_ewma_cache_init(agedays, meas, ewma_exp = -5, ewma_window = 15)
```

## Arguments

- agedays:

  Sorted age in days

- meas:

  Measurements (same order as agedays)

- ewma_exp:

  Exponent for distance weighting (default -5)

- ewma_window:

  Maximum number of positions on each side included in the EWMA (default
  15)

## Value

Cache list with delta matrix, weighted sums, row sums, and EWMA values
