# growthcleanr 3.0.0

growthcleanr 3.0.0 is a major update addressing usability, cleaning performance, customization, efficiency, and documentation. This changelog lists key changes. If you used an earlier version, start with `vignette("experienced-start-here")`, which explains how to update 2.2.3 code and describes all of these changes in more detail. For full documentation of current behavior, see the package vignettes (`browseVignettes("growthcleanr")`) and [growthcleanr.org](https://growthcleanr.org).

## New: Shiny app

- **growthcleanr now has a point-and-click app, so you can clean data without writing R code.** Start it with `growthcleanr_app()`. The app walks you through loading a data file, mapping your columns, and running growthcleanr, and lets you save the results. It runs entirely on your own computer; no data are uploaded. See the [Shiny app how-to guide](https://growthcleanr.org/articles/how-to-shiny.html).

## Main changes

- **Algorithms.** Multiple changes to improve performance, including: an updated child algorithm with improved handling of infants and cleaning of head circumference through age 5 years; an updated adult algorithm with four permissiveness levels; a new "evil twins" step to detect runs of consecutive erroneous values; rescue of many consecutive identical values that were previously excluded as carried forward; and built-in recentering medians based on clinical data.
- **Input.** `cleangrowth()` accepts a data frame, with the option to use alternate column names. Data can be in one of three formats: long (one measurement per row), semi-wide (one visit per row), or wide (one subject per row). Users may enter date of birth and visit date rather than `agedays`. An optional `id` column makes results fully replicable. The Shiny app also handles these formats and dates. For ready-to-run scripts, see `vignette("example-scripts")`; for full detail, see "Input format: long, semi-wide, and wide" and "Date parsing" in `vignette("wrapper")`.
- **Output.** `cleangrowth()` returns your data with results added. New outputs include redesigned exclusion codes and `bin_exclude`, `unit_error_range`, and `mean_ht` columns.
- **Efficiency.** Checkpointing, on by default for datasets with more than 5,000 subjects, reduces memory use and keeps run time roughly linear in the number of subjects. Parallel processing is simpler to use (`parallel = TRUE`). `gc_partial_run()` re-cleans only subjects whose data changed.
- **Trial runs.** `gc_runtrial()` runs growthcleanr on a sample of your data and reports how your columns, sex coding, and parameters were read, so you can check your setup before a full run.
- **Documentation.** How-to guides for the [Shiny app](https://growthcleanr.org/articles/how-to-shiny.html) and the [R interface](https://growthcleanr.org/articles/how-to-r.html), a [researcher overview](https://growthcleanr.org/growthcleanr-researcher-overview.pdf), a [fact sheet](https://growthcleanr.org/growthcleanr-fact-sheet.pdf), ready-to-run scripts (`vignette("example-scripts")`), and expanded technical documentation.

## Breaking changes

Code written for 2.2.3 will need to be updated. We apologize for any inconvenience this causes to experienced users but hope these changes make growthcleanr easier for most users. Details and replacement code are in "Updating code written for growthcleanr 2.2.3" in `vignette("experienced-start-here")`.

- Results differ from 2.2.3, and no setting reproduces 2.2.3 exactly. To reproduce an analysis done with 2.2.3, keep using 2.2.3.
- `cleangrowth()` takes a data frame as its first argument and returns a data.table, not a vector of exclusion codes. The content of the old vector is now in the `exclude` column. Old calls with unnamed arguments stop with a message, but code that names its arguments and treats the result as a vector can run without an error and give wrong results.
- Exclusion codes were renamed to be more meaningful. Filters on specific 2.2.3 codes will not match anything, without a warning. See "Mapping from prior exclusion codes" in `vignette("experienced-start-here")`.
- growthcleanr now excludes only a subset of consecutive identical values, controlled by the new `cf_rescue` argument. `testacf()` and `acf_answers()` were removed, and `adjustcarryforward()` now only stops with a message. See `vignette("cf-rescue")`.
- Several arguments were removed or renamed, and a removed argument stops with a message naming what to use instead. For example, `num.batches` is now `cores`, and `weight_cap` in kg is now `adult_scale_max_lbs` in pounds. See "Removed and renamed arguments" in `vignette("experienced-start-here")`.
- The `sd.recenter` argument still allows users to enter their own recentering table. With the default, `sd.recenter = NA`, growthcleanr now uses a built-in recentering table based on clinical data. The `"nhanes"` and `"derive"` options and `sd_median()` were removed. See `vignette("recentering")`.
- `splitinput()` was removed; checkpointing replaces it. See `vignette("large-datasets")`.
- The command-line driver moved (find it with `gc_cli_path()`) and accepts only long flags. See `vignette("command-line")`.
- `ext_bmiz()` output keeps your column names, and `longwide()` arguments changed. To avoid errors related to numeric sex coding, `longwide()` now returns sex as M or F, not numeric sex, so filters of `longwide()` output on `sex == 1` or `sex == 2` will match no rows, without a warning. `ext_bmiz()` now stops on numeric sex unless `sex` has both 1s and 2s and no other values. See `vignette("utilities")`.
- Docker support was dropped.

## Other changes

- Smaller changes, including new warnings in `cleangrowth()`, `longwide()`, and `ext_bmiz()` and a `recode_sex()` fix, are in "Other changes" in `vignette("experienced-start-here")`.
- Additional authors, contributors, and clinician scientific advisors have been added. See the [Contributors](https://growthcleanr.org/articles/contributors.html) page.

# growthcleanr 2.2.3 - 2026-05-03

## Changed

- Fixed bug affecting adult_cutpoint affecting datasets with narrow age ranges
- Corrected a problem with reference values 

# growthcleanr 2.2.2 - 2026-02-20

## Changed

- Removed outdated statement from README re bug that has now been fixed.

# growthcleanr 2.2.1 - 2026-02-18

## Changed

- Updated internal dplyr code to use `.data$` pronoun notation for CRAN compatibility with recent dplyr versions, resolving "no visible binding for global variable" notes
- Added explicit `rlang` dependency for `.data` pronoun support
- Fixed recentering table that had previously erroneously caused birth measurements to be excluded in small datasets
- Updated test expectations to reflect corrected recentering behavior
- Resolved remaining "no visible binding" note for `id` variable in data.table operations
- Updated broken CDC SAS program URL to current location

# growthcleanr 2.2.0-prelim-infants - 2023-09-13

## Added

- Added option to cleangrowth for the preliminary infants algorithm -- expands pediatric algorithm to consider 0 - 2 years old, with infants = TRUE in cleangrowth(), with all steps implemented. Note that this option is still preliminary and should not be used for research. For more information regarding the logic of the algorithm, see the vignette 'Preliminary Infants Algorithm.' 


# growthcleanr 2.1.1 - 2023-03-01

## Changed

- Fixed missing adult measurements to be labeled "Missing" in output (#119)
- Added tests for missingness in adult output
- Fixed missing "-RV" codes in adult output
- Corrected contributor names in DESCRIPTION (#120)
- Added email for Dan Chudnov in DESCRIPTION (#95)

# growthcleanr 2.1.0 - 2023-02-03

## Added

- Use dependabot to update GitHub workflow action versions (#94)
- Use GitHub action to build and publish container image (#101)

## Changed

- Updated `ext_bmiz()` to match Dec 2022 NCHS guidelines (#98)
- New options to keep dates, columns, unmatched rows in `longwide()` (#71)
- Updated CITATION to match new CRAN requirements
- Updated Dockerfile to build from repo, not CRAN

# growthcleanr 2.0.3 - 2022-11-01

## Added

- CRAN release checklist now added under Developer Guidelines vignette (#99)

## Changed

- All possible levels for `cleangrowth()` output factor now enumerated
- Updated maintainer to Carrie Daymont

# growthcleanr 2.0.2 - 2022-09-13

## Added

- Package now available on CRAN: https://cran.r-project.org/package=growthcleanr

## Changed

- Several updates for CRAN deployment: improved example/test runtimes, text
  corrections (#82); switched examples to use `donttest`, added CRAN comments
  file, updated `.Rbuildignore` (#84)
- Documentation updated with CRAN install (#86), fixed links (#85)
- Updated GitHub check workflow (#80) and pkgdown workflow

# growthcleanr 2.0.1 - 2022-08-29

## Changed

- Updated DESCRIPTION, including authors, URLS, title, description, and imports
- Compressed files in `inst/extdata` for size requirements; added `R.utils` as
  import to support `fread()` for `.gz` files
- Updated license year

# growthcleanr 2.0.0 - 2021-06-30

## Added

- Support for cleaning adult (18-65) observations with `adult_cutpoint` and
  `weightcap` options (https://github.com/growth-data-tools/growthcleanr/issues/17, others)
- Added documentation describing adult algorithm, examples, and exclusions
  (#30), next steps (#63)
- Added tests supporting adult observations (#49)

## Changed

- Removed BMI calculation from `longwide()`, added `simple_bmi()` (#47)
- Enhanced `gcdriver.R` to support adult options, parallel operation
  (https://github.com/growth-data-tools/growthcleanr/pull/23)
- Refreshed `syngrowth` synthetic test data, now includes adults (#50)
- Reorganized documentation from README, now using
  [pkgdown](https://pkgdown.r-lib.org/) (#30)
- Improved code layout to pass `CHECK` cleanly (#18, #60)

# growthcleanr 1.2.6 - 2021-06-10

## Changed

- Corrected four duplicated age-rows in NHANES reference medians (#40)
- Added missing non-newborn constraint in 14h.ii (thanks Lusha Cao)
- Removed `Hmisc` dependency (#36)
- Replaced `clean_value` result column name in docs with `gcr_result` for
  clarity (#35)

# growthcleanr 1.2.5 - 2021-02-26

## Added

- Added `inst/extdata/nhanes-reference-medians.csv`, reference medians for
  recentering derived from NHANES (described in README)

## Changed

- Updated behavior of `sd.recenter` option to include new NHANES reference
  medians and explicit specification with "NHANES" or "derive"
  (https://github.com/growth-data-tools/growthcleanr/pull/9)
- Switched `README.md` to be generated from `README.Rmd` w/knitr (thanks
  @mcanouil) (#17)
- Switched to use `file.path()` more consistently in `R/growth.R`

# growthcleanr 1.2.4 - 2021-01-14

## Changed

- Minor update to WHO HT velocity 3SD files to correct a small number of errors
  (#24). Affected files were:

  - `inst/extdata/who_ht_maxvel_3sd.csv`
  - `inst/extdata/who_ht_vel_3sd.csv`

  Although these changes were very minor, it is possible that results on data
  cleaned after this change may vary from previous results. The prior version of
  these files may be obtained by visiting the tagged release version 1.2.3 at
  <https://github.com/growth-data-tools/growthcleanr/releases/tag/1.2.3>.

  The released version of `growthcleanr` available at that link contains the
  older version of both files; that older version may be used to verify
  reproducibility.

  Alternatively, a more recent version of `growthcleanr` may be used with only
  the affected files replaced with their older versions available at the 1.2.3
  tag link above. This must be done manually.

# growthcleanr 1.2.3 - 2021-01-07

## Added

- New exclusion handling option on experimental carry forward adjustment

## Changed

- Improved experimental carry forward adjustment handling of strings of CF
  values, output handling, and documentation; renamed "Missing" values
- Updated DESCRIPTION, imports, documentation to address testing issue (#12)
- Switched to R-native argparser library to support script options
- Switched to GitHub Actions for continuous integration / testing (thanks
  @mcanouil)
- Improved Dockerfile to standardize user/path, simplify install (thanks
  @mcanouil)

# growthcleanr 1.2.2 - 2020-09-29

## Added

- CITATION file, now `citation("growthcleanr")` works as expected

## Changed

- Standardized on arrow assignment
- Moved functions previously within other functions to top level
- `@import` now preferred over `library()` for library loading
- Exported more functions
- Improved carried forward adjustment driver script, now supports line-grid
  (like original sweep), random, and grid-search search types, with
  configuration
- Added `fdir` option to `splitinput()` to specify split file directory
- Added package minimum versions to DESCRIPTION
- Fixed example code to reduce build warnings
- Improved and corrected documentation
- Re-compressed synthetic sample data (`syngrowth`) to improve compression

# growthcleanr 1.2.1 - 2020-08-14

## Added

- New tests in `tests/testthat/test-utils.R` and `tests/testthat/test-cdc.R` to
  support newly added functions

## Changed

- Improved error handling in `longwide()`; fixed missing import in DESCRIPTION

# growthcleanr 1.2 - 2020-07-24

## Added

- New CDC BMI calculation function `ext_bmiz()`, comparable to SAS program
  published at https://www.cdc.gov/growth-chart-training/hcp/computer-programs/sas.html
- Reference data file `inst/extdata/CDCref_d.csv` from CDC for use with
  `ext_bmiz()`
- New function `longwide()` for transforming `cleangrowth()` output for use with
  `ext_bmiz()`
- New function `recode_sex()` for recoding input data column values for `sex` to
  match `cleangrowth()` or `ext_bmiz()` requirements
- New `exec/gcdriver.R` command-line script for CLI execution of `cleangrowth()`
- New `Dockerfile` (and `.dockerignore`) enabling containerized use of
  `growthcleanr`
- Started test suite in `tests`
- New experimental function `adjustcarryforward()` in `R/adjustcarryforward.R`
  and driver script `exec/testadjustcf.R` (see README-adjustcarryforward.md for
  details)

## Changed

- Reorganized code from `R/growth.R` into separate files for clarity and easier
  maintenance (all utility functions not directly used by `cleangrowth()` are
  now in `R/utils.R`)
- Updated README with details and examples for added functions

# growthcleanr 1.1 - 2020-02-07

## Added

- New options to add flexibility:
  - `error.load.mincount` and `error.load.threshold`
  - `lt3.exclude.mode` with default (same as before) and `flag.both` mode for
    handling unmatched pairs
  - `sdmedian.filename` and `sdrecentered.filename`
- New `splitinput()` function
- New example synthetic data set `syngrowth` loads automatically.

## Changed

- Several updates to improve performance, including eliminating use of
  data.table in ewma function.
- Updated README with link to paper, detailed introduction, more installation
  details, examples, notes on handling large datasets, lists of parameters and
  exclusions.

# growthcleanr 1.0.0 - 2018-09-11

## Added

- Initial version posted to GitHub.
