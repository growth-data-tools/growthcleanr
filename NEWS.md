# growthcleanr 2.2.1 - 2026-02-18

## Changed

- Updated internal dplyr code to use `.data$` pronoun notation for CRAN compatibility with recent dplyr versions, resolving "no visible binding for global variable" notes
- Added explicit `rlang` dependency for `.data` pronoun support
- Fixed recentering table that had previously erroneously caused birth measurements to be excluded in small datasets
- Updated test expectations to reflect corrected recentering behavior

# growthcleanr 2.2.0-prelim-infants - 2023-09-13

## Added

- Added option to cleangrowth for the preliminary infants algorithm -- expands pediatric algorithm to consider 0 - 2 years old, with infants = TRUE in cleangrowth(), with all steps implemented. Note that this option is still preliminary and should not be used for research. For more information regarding the logic of the algorithm, see the vignette 'Preliminary Infants Algorithm.' 


# growthcleanr 3.0.0-infants-beta - 2023-03-13

## Added

- Added the infants beta release algorithm -- expands pediatric algorithm to consider 0 - 2 years old, with infants = TRUE in cleangrowth()
  - Updated velocity data for the extension (#122)

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
  `weightcap` options (https://github.com/mitre/growthcleanr/pull/17, others)
- Added documentation describing adult algorithm, examples, and exclusions
  (#30), next steps (#63)
- Added tests supporting adult observations (#49)

## Changed

- Removed BMI calculation from `longwide()`, added `simple_bmi()` (#47)
- Enhanced `gcdriver.R` to support adult options, parallel operation
  (https://github.com/mitre/growthcleanr/pull/23)
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
  (https://github.com/mitre/growthcleanr/issues/9)
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
  https://github.com/carriedaymont/growthcleanr/releases/tag/1.2.3.

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
  published at https://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm
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
