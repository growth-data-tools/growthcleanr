# Calculate extended BMI measures

`ext_bmiz` Calculates the sigma (scale parameter for the half-normal
distribution), extended BMI percentile, extended BMIz, and the CDC LMS
Z-scores for weight, height, and BMI for children between 2 and 19.9
years of age. Note that for BMIs \<= 95th percentile of the CDC growth
charts, the extended values for BMI are equal to the LMS values. The
extended values differ only for children who have a BMI \> 95th
percentile.

## Usage

``` r
ext_bmiz(
  data,
  age = "agem",
  wt = "wt",
  ht = "ht",
  bmi = "bmi",
  adjust.integer.age = TRUE,
  ref.data.path = ""
)
```

## Arguments

- data:

  Input data frame or data table

- age:

  Name of input column containing subject age in months in quotes,
  default "agem"

- wt:

  Name of input column containing weight (kg) value in quotes, default
  "wt"

- ht:

  Name of input column containing height (cm) value in quotes, default
  "ht"

- bmi:

  Name of input column containing calculated BMI in quotes, default
  "bmi"

- adjust.integer.age:

  If age inputs are all integer, add 0.5 if TRUE; default TRUE

- ref.data.path:

  Path to directory containing reference data

## Value

A data.table: a copy of `data` (same rows, row order, column names, and
values) with these columns added: `bmiz`, `bmip`, `waz`, `wp`, `haz`,
`hp`, `p95`, `p97`, `bmip95`, `mod_bmiz`, `mod_waz`, `mod_haz`, `sigma`,
`original_bmip`, `original_bmiz`, `sev_obese`, `obese`.

## Details

This function produces output equivalent to [CDC
software](https://www.cdc.gov/growth-chart-training/hcp/computer-programs/index.html)
and the [cdcanthro R
package](https://CRAN.R-project.org/package=cdcanthro).

The extended BMIz is the inverse cumulative distribution function (CDF)
of the extended BMI percentile. If the extended percentile is very close
to 100, the `qnorm` function in R produces an infinite value. This
occurs only if the extended BMI percentile is \> 99.99999999999999. This
occurs infrequently, such as a 48-month-old with a BMI \> 39, and it is
likely that these BMIs represent data entry errors. For these cases,
extended BMIz is set to 8.21, a value that is slightly greater than the
largest value that can be calculated.

The output columns are listed under Value below.

`data` must have columns for at least age, sex, weight, height, and bmi.

`age` should be coded in months, using the most precise values
available. To convert to months from age in years, multiply by 12. To
convert to months from age in days, divide by 30.4375 (365.25 / 12).

`sex`: we recommend M and F, which is what
[`longwide()`](https://growthcleanr.org/reference/longwide.md) returns.
Male can be coded as boy, Boy, b, B, male, Male, m, or M, and female can
be coded as girl, Girl, g, G, female, Female, f, or F. Numeric sex is
accepted only if `sex` has both 1s and 2s and no other values, read as 1
= male, 2 = female; otherwise `ext_bmiz()` stops. This is because 0/1 is
the numeric coding in
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md),
and a column of only 1s could be either coding. The 1/2 coding matches
the CDC reference data, CDC software, and the cdcanthro package. Other
text values, and logical values, are not recognized: their z-scores are
`NA`, with a warning.

`wt` should be in kilograms.

`ht` should be in centimeters.

Specify the input data column names for `age`, `wt`, `ht`, `bmi` using
quotation marks. See example below.

`ext_bmiz()` does not change `data`. It returns a copy of `data` with
the new columns added; your column names and values are kept as they are
(the 0.5 added by `adjust.integer.age` is used only in the calculation).
If `data` already has a column with the name of one of the columns
`ext_bmiz()` adds (`bmiz`, `bmip`, `waz`, `wp`, `haz`, `hp`, `p95`,
`p97`, `bmip95`, `mod_bmiz`, `mod_waz`, `mod_haz`, `sigma`,
`original_bmip`, `original_bmiz`, `sev_obese`, `obese`), for example
from an earlier run, it warns and the returned data has the newly
calculated values in that column; rename the column first to keep both.

Values are calculated for ages 24 through 240 months, as in the
cdcanthro package. Age 730 days is 23.98 months (730 / 30.4375), which
is below 24 months, so a row at 730 days has `NA` values even though a
child at that age may be exactly 2 years old; values begin at 731 days.
A warning gives the number of rows at about 729-730 days.

If the argument `adjust.integer.age` is `TRUE` (the default), 0.5 will
be added to all `age` values for the calculation if all input values are
integers. Set to `FALSE` to disable.

By default, the reference data file `CDCref_d.csv`, made available at
https://www.cdc.gov/growth-chart-training/hcp/computer-programs/index.html,
is included in this package for convenience. If you are developing this
package, use `ref.data.path` to adjust the path to this file from your
working directory if necessary.

## References

Wei R, Ogden CL, Parsons VL, Freedman DS, Hales CM. A method for
calculating BMI z-scores and percentiles above the 95th percentile of
the CDC growth charts. Ann Hum Biol. 2020 Sep;47(6):514-521.
[doi:10.1080/03014460.2020.1808065](https://doi.org/10.1080/03014460.2020.1808065)
.

## Examples

``` r
# \donttest{
# Run on a small subset of given data
df <- as.data.frame(syngrowth)
df <- df[df$subjid %in% unique(df[, "subjid"])[1:5], ]
# cleangrowth() returns the data with an 'exclude' column
clean_df <- cleangrowth(df)
#> [14:12:07] growthcleanr: cleaning in progress — 5 subjects, 93 measurements, 1 batch of up to 5,000 subjects
#> ************************************************************
#> growthcleanr cleaning summary  (93 measurements)
#>   Weight      : 45 included (80.4%), 11 excluded (19.6%)
#>   Height      : 32 included (86.5%), 5 excluded (13.5%)
#>   Set aside   : 0 (0.0%)
#> ************************************************************
df_wide <- longwide(clean_df) # convert to wide format for ext_bmiz
df_wide_bmi <- simple_bmi(df_wide) # compute simple BMI

# Calling the function with default column names
df_bmiz <- ext_bmiz(df_wide_bmi)

# Specifying different column names; note that quotes are used
dfc <- simple_bmi(df_wide)
colnames(dfc)[colnames(dfc) %in% c("agem", "wt", "ht")] <-
  c("agemos", "weightkg", "heightcm")
df_bmiz <- ext_bmiz(dfc, age="agemos", wt="weightkg", ht="heightcm")

# Disabling conversion of all-integer age in months to (age + 0.5)
dfc <- simple_bmi(df_wide)
df_bmiz <- ext_bmiz(dfc, adjust.integer.age=FALSE)
# }
```
