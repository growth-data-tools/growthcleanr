# Evaluate error load for one subject

Evaluate error load for one subject

## Usage

``` r
eval_error_load(subj_results, error_threshold = 0.41)
```

## Arguments

- subj_results:

  data.table with result, param columns for ONE subject

- error_threshold:

  Threshold for error ratio (default 0.41)

## Value

Character vector of internal_ids to mark
Exclude-A-Unevaluable-Trajectory
