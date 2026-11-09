# Compute the Evil Twins / Extreme EWMA cap for given intervals and weights. Vectorized: interval_months and uw can be vectors.

For built-in formulas (piecewise, piecewise-lower, allofus15), ET caps
are derived from the formula's wtallow caps + 20 (for PW-H/PW-L) or
equal to the allofus15 12m cap. For custom CSV formulas, fixed caps of
70/100 are used.

## Usage

``` r
compute_et_limit(interval_months, formula = "piecewise", uw = NULL)
```

## Arguments

- interval_months:

  Interval in months (minimum neighbor gap)

- formula:

  Character: "piecewise", "piecewise-lower", "allofus15", or path to
  custom CSV

- uw:

  Optional numeric vector of upper weights (higher of two adjacent
  weights, or max(wt, ewma)). NULL = use base (UW=120) caps.

## Value

Numeric vector of ET limits

## References

Guide A, Sulieman L, Garbett S, Cronin RM, Spotnitz M, Natarajan K,
Carroll RJ, Harris P, Chen Q. Identifying erroneous height and weight
values from adult electronic health records in the All of Us research
program. J Biomed Inform. 2024 Jul;155:104660.
[doi:10.1016/j.jbi.2024.104660](https://doi.org/10.1016/j.jbi.2024.104660)
. (Source of the adapted "allofus15" formula.)
