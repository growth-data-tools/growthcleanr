# Removed function: adjustcarryforward()

`adjustcarryforward()` was removed in growthcleanr 3.0.0.
Carried-forward handling is now part of
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md);
use its `cf_rescue` argument. This function remains only to stop with
that message, so that packages that refer to it do not fail their
checks.

## Usage

``` r
adjustcarryforward(...)
```

## Arguments

- ...:

  Ignored.

## Value

None; always stops with an error.

## See also

[`vignette("cf-rescue")`](https://growthcleanr.org/articles/cf-rescue.md)
