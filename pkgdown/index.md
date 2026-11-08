An open-source R package that flags implausible values in longitudinal pediatric and adult weight, height, and head circumference data.

::: {.alert .alert-info}
**We are excited to announce the release of growthcleanr 3.0.0.**

**growthcleanr 3.0.0 is now available on GitHub and is in the process of being released on CRAN**, which we expect to happen soon. Until then, `install.packages("growthcleanr")` in R will install the older version, 2.2.3. To install 3.0.0 now:

```r
install.packages("remotes")
remotes::install_github("growth-data-tools/growthcleanr",
                        build_vignettes = TRUE, dependencies = TRUE)
```
:::

## What does growthcleanr do?

- Infographic and fact sheet ([PDF](growthcleanr-fact-sheet.pdf))
- Researcher overview ([PDF](growthcleanr-researcher-overview.pdf))

## How to use

**Install.** From CRAN, in R: `install.packages("growthcleanr")`.

Source code is on [GitHub](https://github.com/growth-data-tools/growthcleanr).

**Basics**

- [Start here](articles/new-start-here.html)
- [Shiny app (graphical interface) how-to guide](articles/how-to-shiny.html)
- [R interface how-to guide](articles/how-to-r.html)
- Researcher overview ([PDF](growthcleanr-researcher-overview.pdf))

**More info**

- [Example R scripts](articles/example-scripts.html)
- [Returning users: what's new](articles/experienced-start-here.html)
- [Tips for making the input CSV](articles/csv-format.html)
- [Cleaning large datasets](articles/large-datasets.html)
- [Command-line driver](articles/command-line.html)

## Cite

Citing growthcleanr helps us to track how it is used to guide further development and obtain further support.

> Carrie Daymont, Michelle E Ross, A Russell Localio, Alexander G Fiks, Richard C Wasserman, Robert W Grundmeier, Automated identification of implausible values in growth data from pediatric electronic health records, Journal of the American Medical Informatics Association, Volume 24, Issue 6, November 2017, Pages 1080–1087, https://doi.org/10.1093/jamia/ocx037

## Documentation

Full technical documentation is in the [Articles](articles/index.html) and [Reference](reference/index.html) menus above: the child and adult algorithms, configuration and output, z-scores and methods, utilities and testing, and the complete function reference.

## About the project

**Funding and contributors.** [People and organizations](articles/contributors.html) who have contributed to and supported growthcleanr.

**Source and license.** Open source under the MIT License. Browse or fork it on [GitHub](https://github.com/growth-data-tools/growthcleanr).

**Related package.** [cdcanthro](https://CRAN.R-project.org/package=cdcanthro) calculates WHO and CDC z-scores and is commonly used alongside growthcleanr.

## Contact

Contact through [GitHub](https://github.com/growth-data-tools/growthcleanr/issues) or email: <cdaymont@pennstatehealth.psu.edu>.

**Report a bug.** Provide as much detail as possible: the version and interface (app, R, or command-line), the code you used, the exact error message, the structure of your dataset, and if possible, a sample dataset without private data.

**Suggest a feature.** We will continue improving growthcleanr as resources allow. Suggest new features or identify new issues you would like growthcleanr to address.

**Collaborate.** [Email](mailto:cdaymont@pennstatehealth.psu.edu) if you are interested in collaborating to evaluate or improve growthcleanr.
