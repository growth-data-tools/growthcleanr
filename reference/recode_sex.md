# Recode binary sex variable for compatibility

`recode_sex` recodes a binary sex variable for a given source column in
a data frame or data table. Only exact matches of `sourcem`/`sourcef`
are recoded; any other value (e.g. "M"/"F") becomes NA in `targetcol`
without a warning.
[`ext_bmiz()`](https://growthcleanr.org/reference/ext_bmiz.md) reads
only the column named `sex`, so to prepare data for it use
`targetcol = "sex"` (with `targetm = "M"`, `targetf = "F"` if the data
might have only one sex, since
[`ext_bmiz()`](https://growthcleanr.org/reference/ext_bmiz.md) accepts
numeric sex only when both 1 and 2 are present). Not needed between
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md) and
[`ext_bmiz()`](https://growthcleanr.org/reference/ext_bmiz.md):
[`longwide()`](https://growthcleanr.org/reference/longwide.md) already
outputs sex as M or F.

## Usage

``` r
recode_sex(
  input_data,
  sourcecol = "sex",
  sourcem = "0",
  sourcef = "1",
  targetcol = "sex_recoded",
  targetm = 1L,
  targetf = 2L
)
```

## Arguments

- input_data:

  a data frame or data table to be transformed. Expects a source column
  containing a binary sex variable.

- sourcecol:

  name of sex descriptor column. Defaults to "sex"

- sourcem:

  variable indicating "male" sex in input data. Defaults to "0"

- sourcef:

  variable indicating "female" sex in input data. Defaults to "1"

- targetcol:

  desired name of recoded sex descriptor column. Defaults to
  "sex_recoded"

- targetm:

  value to use for "male" sex in `targetcol`. Defaults to 1

- targetf:

  value to use for "female" sex in `targetcol`. Defaults to 2

## Value

Returns a data table with recoded sex variables.

## Examples

``` r
# Run on given data
df <- as.data.frame(syngrowth)

# Run with all defaults
df_r <- recode_sex(df)

# Specify different targets
df_rt <- recode_sex(df, targetcol = "sexr", targetm = "Male", targetf = "Female")

# Specify different inputs
df_ri <- recode_sex(df_rt, sourcecol = "sexr", sourcem = "Male", sourcef = "Female")
```
