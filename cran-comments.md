# CRAN submission growthcleanr 2.2.0

## R CMD check results
There were no ERRORs or WARNINGs.

There was one note:

❯ checking installed package size ... NOTE
    installed size is  5.9Mb
    sub-directories of 1Mb or more:
      extdata   5.0Mb
      
extdata has been compressed as much as possible -- data in that folder is necessary to run the package.

## Downstream dependencies
There are currently no downstream dependencies for this package.

# Previous Submissions

# CRAN submission growthcleanr 2.1.1

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

# CRAN submission growthcleanr 2.1.0

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

## CRAN manual test

We have addressed each of the notes in the manual test as below:

* Package CITATION file contains call(s) to old-style personList() or
   as.personList().  Please use c() on person objects instead.
   Package CITATION file contains call(s) to old-style citEntry() or
   citHeader()/citFooter().  Please use bibentry() instead, possibly with
   arguments 'header' and 'footer'.


We have updated the citation accordingly.

# CRAN submission growthcleanr 2.1.0 (1)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

# CRAN submission growthcleanr 2.0.3

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

# CRAN submission growthcleanr 2.0.2 (1)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

# CRAN submission growthcleanr 2.0.1 (6)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## CRAN manual test

We have addressed each of the notes in the manual test as below:

* Possibly misspelled words in DESCRIPTION:
    Anthropometric (3:25)
    anthropometric (22:56)

These are not misspelled.

* Please write TRUE and FALSE instead of T and F. (Please don't use 'T' or 'F' as vector names.), e.g.:

This has been corrected throughout functions in package.

* Please replace \dontrun with \donttest. Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.

\dontrun has been replaced with \donttest.

* Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().

I have gone through all functions in package and affected functions have been adjusted/confirmed to not write by default to user directory. All examples with directory output have been adjusted to use tempdir().

## Downstream dependencies
There are currently no downstream dependencies for this package.

# CRAN submission growthcleanr 2.0.1 (5)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking installed package size ... NOTE
    installed size is  9.8Mb
    sub-directories of 1Mb or more:
      doc       5.7Mb
      extdata   3.3Mb

doc should not be included in build (listed in .Rbuildignore) and extdata has been compressed as much as possible -- data in that folder is necessary to run the package.

## CRAN pretest

We have addressed each of the notes in the manual test as below:

* Possibly misspelled words in DESCRIPTION:
    Anthropometric (3:25)
    anthropometric (22:56)

These are not misspelled.

* Package has a VignetteBuilder field but no prebuilt vignette index.

Vignette files are now added to Rbuildignore, VignetteBuilder removed.

*   Files in the 'vignettes' directory but no files in 'inst/doc':
    'adult-algorithm.Rmd', 'configuration.Rmd', 'installation.Rmd',
      'large-data-sets.Rmd', 'next-steps.Rmd', 'output.Rmd',
      'quickstart.Rmd', 'usage.Rmd', 'utilities.Rmd'

Vignette files are now added to Rbuildignore.

*   Directory 'inst/doc' does not exist.
  Package vignettes without corresponding single PDF/HTML:
     'adult-algorithm.Rmd'
     'configuration.Rmd'
     'installation.Rmd'
     'large-data-sets.Rmd'
     'next-steps.Rmd'
     'output.Rmd'
     'quickstart.Rmd'
     'usage.Rmd'
     'utilities.Rmd'

Vignette files are now added to Rbuildignore.

## Downstream dependencies
There are currently no downstream dependencies for this package.


# CRAN submission growthcleanr 2.0.1 (4)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking installed package size ... NOTE
    installed size is  9.8Mb
    sub-directories of 1Mb or more:
      doc       5.7Mb
      extdata   3.3Mb

doc should not be included in build (listed in .Rbuildignore) and extdata has been compressed as much as possible -- data in that folder is necessary to run the package.

## CRAN manual test

We have addressed each of the notes in the manual test as below:

* Possibly misspelled words in DESCRIPTION:
    Anthropometric (3:25)
    anthropometric (22:56)

These are not misspelled.

* License components with restrictions and base license permitting such:
     MIT + file LICENSE
   File 'LICENSE':
     MIT License

     Copyright (c) 2018-2022 Carrie Daymont

     Permission is...

License has been changed to CRAN template.

*   Size of tarball: 5826685 bytes

Vignette building has been removed, size is now around ~3.7 MB.

## Downstream dependencies
There are currently no downstream dependencies for this package.

# CRAN submission growthcleanr 2.0.1 (3)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking installed package size ... NOTE
    installed size is  9.8Mb
    sub-directories of 1Mb or more:
      doc       5.7Mb
      extdata   3.3Mb

doc should not be included in build (listed in .Rbuildignore) and extdata has been compressed as much as possible -- data in that folder is necessary to run the package.

## CRAN pretest

We have addressed each of the notes in the pretest as below:

* Possibly misspelled words in DESCRIPTION:
    Anthropometric (3:25)
    anthropometric (22:56)

These are not misspelled.

*   Examples with CPU (user + system) or elapsed time > 10s
              user system elapsed
  cleangrowth 4.01    0.3    18.2

cleangrowth() example has been changed to \donttest{}

## Downstream dependencies
There are currently no downstream dependencies for this package.

# CRAN submission growthcleanr 2.0.1 (2)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking installed package size ... NOTE
    installed size is  9.8Mb
    sub-directories of 1Mb or more:
      doc       5.7Mb
      extdata   3.3Mb

doc should not be included in build (listed in .Rbuildignore) and extdata has been compressed as much as possible -- data in that folder is necessary to run the package.

## CRAN pretest

We have addressed each of the notes in the pretest as below:

* Possibly misspelled words in DESCRIPTION:
    Anthropometric (3:25)
    anthropometric (22:56)

These are not misspelled.

*   Found the following (possibly) invalid URLs:
    URL: https://cran.r-project.org/web/packages/survey/index.html
      From: inst/doc/configuration.html
      Status: 200
      Message: OK
      CRAN URL not in canonical form
    The canonical URL of the CRAN page for a package is
      https://CRAN.R-project.org/package=pkgname

This has been fixed.

*   The Description field should start with a capital letter.

This has been fixed ('growthcleanr' removed, includes capitalized).

*   Examples with CPU (user + system) or elapsed time > 10s
              user system elapsed
  cleangrowth 4.01    0.3    18.2

cleangrowth() example has been shortened (run on fewer subjects).

* Check: Overall checktime, Result: NOTE
  Overall checktime 13 min > 10 min

Tests have been shortened (elapsed time locally ~80s).

## Downstream dependencies
There are currently no downstream dependencies for this package.

# CRAN submission grwothcleanr 2.0.1 (1)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking installed package size ... NOTE
    installed size is  9.8Mb
    sub-directories of 1Mb or more:
      doc       5.7Mb
      extdata   3.3Mb

doc should not be included in build (listed in .Rbuildignore) and extdata has been compressed as much as possible -- data in that folder is necessary to run the package.

## Downstream dependencies
There are currently no downstream dependencies for this package.
