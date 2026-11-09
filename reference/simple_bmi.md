# Compute BMI using standard formula

`simple_bmi` Computes BMI using standard formula. Assumes input
compatible with output from longwide().

## Usage

``` r
simple_bmi(wide_df, wtcol = "wt", htcol = "ht")
```

## Arguments

- wide_df:

  A data frame or data table containing heights and weights in wide
  format, e.g., after transformation with longwide()

- wtcol:

  name of the weight value column (kg), default 'wt'

- htcol:

  name of the height value column (cm), default 'ht'

## Value

Returns a data table with the added column "bmi"

## Examples

``` r
# \donttest{
# Simple usage
# Run on a small subset of given data
df <- as.data.frame(syngrowth)
df <- df[df$subjid %in% unique(df[, "subjid"])[1:2], ]
# cleangrowth() returns the data with an 'exclude' column
clean_df <- cleangrowth(df)
#> Warning: POSSIBLE SEX CODING ERROR in cleangrowth(): every value in `sex` is 1, which growthcleanr reads as female. If these subjects are male with 1 = male, 2 = female coding, they will be read as the wrong sex: pass sex_coding = list(male = 1, female = 2). If they are female, pass sex_coding = list(male = 0, female = 1) or code sex as F to remove this warning.
#> [14:18:20] growthcleanr: cleaning in progress — 2 subjects, 57 measurements, 1 batch of up to 5,000 subjects
#> ************************************************************
#> growthcleanr cleaning summary  (57 measurements)
#>   Weight      : 28 included (77.8%), 8 excluded (22.2%)
#>   Height      : 18 included (85.7%), 3 excluded (14.3%)
#>   Set aside   : 0 (0.0%)
#> ************************************************************
# Convert to wide format
wide_df <- longwide(clean_df)
#> Warning: POSSIBLE SEX CODING ERROR in longwide(): every value in `sex` is 1, which growthcleanr reads as female. If these subjects are male with 1 = male, 2 = female coding, they will be read as the wrong sex: pass sex_coding = list(male = 1, female = 2). If they are female, pass sex_coding = list(male = 0, female = 1) or code sex as F to remove this warning.
wide_df_with_bmi <- simple_bmi(wide_df)

# Specifying different column names; note that quotes are used
colnames(wide_df)[colnames(wide_df) %in% c("wt", "ht")] <-
  c("weight", "height")
wide_df_with_bmi <- simple_bmi(wide_df, wtcol = "weight", htcol = "height")
# }
```
