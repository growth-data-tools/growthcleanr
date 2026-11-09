# Update EWMA cache by removing one observation. O(n) instead of O(n^2).

Update EWMA cache by removing one observation. O(n) instead of O(n^2).

## Usage

``` r
adult_ewma_cache_update(cache, pos_j)
```

## Arguments

- cache:

  Cache list from adult_ewma_cache_init or previous update

- pos_j:

  Position (index) of the observation to remove

## Value

Updated cache list with n-1 observations, or NULL if n would be 0
