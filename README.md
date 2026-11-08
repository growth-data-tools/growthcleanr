
<!-- README.md is generated from README.Rmd. Please edit that file -->

# growthcleanr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/growthcleanr)](https://cran.r-project.org/package=growthcleanr)
[![R build
status](https://github.com/growth-data-tools/growthcleanr/workflows/R-CMD-check/badge.svg)](https://github.com/growth-data-tools/growthcleanr/actions)

<!-- badges: end -->

R package for cleaning pediatric and adult weight, height, and head
circumference data from electronic health records.

Version 3 is a major update. If you are new to growthcleanr, start with
`vignette("new-start-here")`; if you have used growthcleanr before,
start with `vignette("experienced-start-here")`. The full documentation
is available at [growthcleanr.org](https://growthcleanr.org).

<a name="cite"></a> This package implements a modified version of the
[Daymont et
al. algorithm](https://pmc.ncbi.nlm.nih.gov/articles/PMC7651915/), as
specified in Supplemental File 3 within the [Supplementary
Material](https://pmc.ncbi.nlm.nih.gov/articles/PMC7651915/) published
with that paper. It includes significant updates that have yet to be
published in a peer-reviewed journal, but are described in detail in the
package documentation.

> Carrie Daymont, Michelle E Ross, A Russell Localio, Alexander G Fiks,
> Richard C Wasserman, Robert W Grundmeier, Automated identification of
> implausible values in growth data from pediatric electronic health
> records, Journal of the American Medical Informatics Association,
> Volume 24, Issue 6, November 2017, Pages 1080–1087,
> <https://doi.org/10.1093/jamia/ocx037>

This package also includes a function for calculating percentiles and
Z-scores of pediatric growth observations using the methods in [CDC
software](https://www.cdc.gov/growth-chart-training/hcp/computer-programs/index.html)
and the [cdcanthro R
package](https://CRAN.R-project.org/package=cdcanthro).

## Installation

### From CRAN

Install the current release from CRAN:

``` r
install.packages("growthcleanr")
```

### GitHub version

To install the current version from GitHub with `remotes`, building the
vignettes so the in-package documentation is available locally:

``` r
install.packages("remotes")
remotes::install_github("growth-data-tools/growthcleanr",
                        build_vignettes = TRUE, dependencies = TRUE)
```

`build_vignettes = TRUE` requires the `knitr` and `rmarkdown` packages,
which `dependencies = TRUE` installs for you.
(`pak::pak("growth-data-tools/growthcleanr")` also works and is faster,
but does not build the vignettes.)

## Summary

The `growthcleanr` package processes weight, height, and head
circumference data to identify implausible measurements and other values
that should be excluded from analysis. It uses a variety of techniques,
many of which rely on a comparison of a value to an expected value
derived from the patient’s other values.

Results from `growthcleanr` include a flag for each measurement
indicating a recommendation to include it or exclude it from analysis.
No values are removed. These flags can be used as-is to identify an
analytic dataset, but researchers can also examine detailed results to
customize use of growthcleanr. The exclusion flags also facilitate
reporting the reason for excluding a measurement.

To start running `growthcleanr`, an R installation is required, as is a
growth measurement dataset prepared for use in `growthcleanr`.
growthcleanr can be run from R, from the command line, or through a
point-and-click Shiny app (`growthcleanr_app()`) that doesn’t require
writing R code.

## Documentation

The full documentation is available at
[growthcleanr.org](https://growthcleanr.org): guides for getting
started, configuration and output, and technical references for the
child and adult algorithms. The vignettes also ship prebuilt with the
package; view them locally with `browseVignettes("growthcleanr")`.

## Changes

Source code is on
[GitHub](https://github.com/growth-data-tools/growthcleanr). For a
summary of what changed relative to the last CRAN release, see [Start
here for experienced
users](https://growthcleanr.org/articles/experienced-start-here.html) or
the [Changelog](https://growthcleanr.org/news/index.html) (also
`NEWS.md`). Earlier tagged releases of growthcleanr are listed [at
GitHub](https://github.com/growth-data-tools/growthcleanr/releases).

## Contributors and funding

The people and organizations who have contributed to and supported
growthcleanr are listed on the
[Contributors](https://growthcleanr.org/articles/contributors.html)
page.
