# Propagate firstRV exclusions forward to RV copies and extraneous values (linked mode). For each excluded value, finds all other values with the same measurement that are still "Include" – including extraneous values whose is_rv flag may have been cleared by redo_identify_rv(). Marks them with `"<code>-RV-Propagated"`. This is forward-only propagation (firstRV -\> later RVs + extraneous), unlike Step 4W's bidirectional scale-max propagation.

Propagate firstRV exclusions forward to RV copies and extraneous values
(linked mode). For each excluded value, finds all other values with the
same measurement that are still "Include" – including extraneous values
whose is_rv flag may have been cleared by redo_identify_rv(). Marks them
with `"<code>-RV-Propagated"`. This is forward-only propagation (firstRV
-\> later RVs + extraneous), unlike Step 4W's bidirectional scale-max
propagation.

## Usage

``` r
propagate_to_rv(exc_codes, w_subj_df, w_subj_keep)
```

## Arguments

- exc_codes:

  Named character vector: internal_id -\> exclusion code (from firstRV
  pass)

- w_subj_df:

  Current working weight data (must have meas_m, internal_id columns)

- w_subj_keep:

  Named vector of all exclusion codes for this subject's weights

## Value

List with updated w_subj_keep and propagated_ids
