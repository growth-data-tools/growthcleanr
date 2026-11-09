# Child unit-error-range reference values

Part of default reference tables

## Details

Contains, for each age in days, sex, and param, the measurement values
at recentered CSD z-scores of -3 (column zn3) and +3 (column zp3),
computed with the same WHO/CDC blending and default recentering used by
the child algorithm. Covers weight and height from birth to 20 years and
head circumference from birth to 5 years (1826 days). Used to compute
the informational `unit_error_range` output column and the unit-error
check in Evil Twins (Child Step 9). See
[`vignette("algorithm-functions")`](https://growthcleanr.org/articles/algorithm-functions.md).

## child_z3_reference.csv.gz

Used in function
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md)
(when `unit_error_range = TRUE`, the default)
