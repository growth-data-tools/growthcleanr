# CRAN submission growthcleanr 3.0.0

This is a major update to the current CRAN release (2.2.3).

## What's new

growthcleanr 3.0.0 includes substantial updates to the child (previously referred to as pediatric) and adult algorithms. New features include evaluation of head circumference, better assessment of infant growth, more restrictive exclusion of consecutive identical ("carried-forward") values, and configurable "permissiveness" levels for the adult algorithm. Multiple usability improvements were made, including accepting a data frame and returning a data.table (rather than requiring users to bind a vector) and addition of a Shiny app. Documentation was also expanded and updated. Extensive changes were made to improve efficiency. User-visible changes are summarized in NEWS.md with further details in `vignette("experienced-start-here")`. `vignette("new-start-here")` introduces the package for new users.

## Test environments

* local: macOS 26 (aarch64), R 4.5.2
* win-builder: Windows Server 2022, R-devel (r90836)
* GitHub Actions: ubuntu-latest, R release and R oldrel-1

## R CMD check results

0 errors | 0 warnings | 1 note

The note, from `checking CRAN incoming feasibility`, has two parts:

* Possibly misspelled words in DESCRIPTION: 'Daymont', 'et', and 'al'. These are the author name and citation form in the reference to Daymont et al. 2017, which is cited by DOI (`https://doi.org/10.1093/jamia/ocx037`).
* 404s for seven growthcleanr.org addresses. These pages are part of the 3.0.0 website, which goes live when 3.0.0 is pushed to the public GitHub repository, before this submission.

## Reverse dependencies

growthcleanr is a suggested package for one package on CRAN, bsitar. It is used only in one internal helper function. The bsitar maintainer confirmed that the changes in version 3 will not affect external users of bsitar, and he is aware of the need to change his internal code. We edited growthcleanr so that the current helper code will not result in failing a CRAN check.
