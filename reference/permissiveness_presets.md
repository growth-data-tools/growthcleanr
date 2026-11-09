# Return preset setting values for each permissiveness level

Returns a named list of four permissiveness levels (loosest, looser,
tighter, tightest), each containing the full set of adult algorithm
settings for that level. Default level is "looser".

## Usage

``` r
permissiveness_presets()
```

## Value

Named list of four lists, each containing setting values

## See also

Section 1D of
[`vignette("configuration")`](https://growthcleanr.org/articles/configuration.md)
for what each level is intended for and the preset values;
[`vignette("adult-algorithm")`](https://growthcleanr.org/articles/adult-algorithm.md)
for what each setting does.

## Examples

``` r
presets <- permissiveness_presets()
names(presets)
#> [1] "loosest"  "looser"   "tighter"  "tightest"
presets$looser$overall_wt_max
#> [1] 270
```
