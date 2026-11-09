# Function to calculate z-scores and csd-scores based on anthro tables.

Function to calculate z-scores and csd-scores based on anthro tables.

## Usage

``` r
read_anthro(path = "", cdc.only = FALSE)
```

## Arguments

- path:

  Path to supplied reference anthro data. Defaults to package anthro
  tables.

- cdc.only:

  Whether or not only CDC data should be used. Defaults to false.

## Value

A function `f(param, agedays, sex, measurement, csd = FALSE)` that
returns, for each measurement, its LMS z-score (default) or, when
`csd = TRUE`, its CSD ("standard deviation") score against the selected
reference (WHO below age 5 years and CDC from age 5 years, or CDC at all
ages when `cdc.only = TRUE`; the 2-5 year WHO/CDC blending used by
cleangrowth() is not applied here).

## Examples

``` r
# \donttest{
# Return calculating function with all defaults
afunc <- read_anthro()

# Return calculating function while specifying a path and using only CDC data
afunc <- read_anthro(path = system.file("extdata", package = "growthcleanr"),
                     cdc.only = TRUE)
# }
```
