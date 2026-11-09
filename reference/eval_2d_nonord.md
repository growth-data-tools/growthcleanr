# Evaluate 2D non-ordered weight pairs for one subject

Evaluate 2D non-ordered weight pairs for one subject

## Usage

``` r
eval_2d_nonord(w_subj_df, w_subj_keep, wtallow_formula = "piecewise")
```

## Arguments

- w_subj_df:

  Weight data with meas_m, age_days, internal_id. Already filtered to
  Inc.

- w_subj_keep:

  Named vector of current exclusion codes for all weight obs

- wtallow_formula:

  Formula for wtallow

## Value

Character vector of internal_ids to exclude with code
Exclude-A-2D-Non-Ordered
