# Package index

## Overview

Package overview

- [`growthcleanr-package`](https://growthcleanr.org/reference/growthcleanr-package.md)
  [`growthcleanr`](https://growthcleanr.org/reference/growthcleanr-package.md)
  : growthcleanr: a data cleaner for anthropometric measurements

## Main function

cleangrowth() cleans growth data. Start here.

- [`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md) :
  Clean growth measurements (main growthcleanr function)

## Other cleaning functions

Other ways to run growthcleanr

- [`gc_runtrial()`](https://growthcleanr.org/reference/gc_runtrial.md) :
  Trial run of growthcleanr on a sample of your data
- [`growthcleanr_app()`](https://growthcleanr.org/reference/growthcleanr_app.md)
  : Run the growthcleanr Shiny application
- [`run_gc_cli()`](https://growthcleanr.org/reference/run_gc_cli.md) :
  Run growthcleanr from the command line (CSV in, CSV out)

## User utilities

User functions for related tasks. ext_bmiz() computes Z-scores and
percentiles for pediatric subjects using the methods in the cdcanthro
package (<https://CRAN.R-project.org/package=cdcanthro>).

- [`permissiveness_presets()`](https://growthcleanr.org/reference/permissiveness_presets.md)
  : Return preset setting values for each permissiveness level
- [`gc_cli_path()`](https://growthcleanr.org/reference/gc_cli_path.md) :
  Path to the growthcleanr command-line driver script
- [`longwide()`](https://growthcleanr.org/reference/longwide.md) :
  Transform data in growthcleanr format into wide structure for BMI
  calculation
- [`simple_bmi()`](https://growthcleanr.org/reference/simple_bmi.md) :
  Compute BMI using standard formula
- [`ext_bmiz()`](https://growthcleanr.org/reference/ext_bmiz.md) :
  Calculate extended BMI measures
- [`recode_sex()`](https://growthcleanr.org/reference/recode_sex.md) :
  Recode binary sex variable for compatibility

## Support functions

Functions used within the cleaning process. Users can call them directly
(for example, for checkpointed, partial, or repeated runs), but most
users will not need to.

- [`cleangrowth_checkpoint()`](https://growthcleanr.org/reference/cleangrowth_checkpoint.md)
  : Run cleangrowth() with automatic checkpointing (deprecated wrapper)
- [`cleangrowth_checkpoint_combine()`](https://growthcleanr.org/reference/cleangrowth_checkpoint_combine.md)
  : Combine saved checkpoint files from a checkpointed cleangrowth() run
- [`gc_preload_refs()`](https://growthcleanr.org/reference/gc_preload_refs.md)
  : Pre-load growthcleanr reference closures
- [`gc_partial_run()`](https://growthcleanr.org/reference/gc_partial_run.md)
  : Incremental (partial) growthcleanr run
- [`ewma()`](https://growthcleanr.org/reference/ewma.md) : Exponentially
  Weighted Moving Average (EWMA)
- [`read_anthro()`](https://growthcleanr.org/reference/read_anthro.md) :
  Function to calculate z-scores and csd-scores based on anthro tables.

## Datasets

Reference data and additional sources

- [`CDCref_d`](https://growthcleanr.org/reference/CDCref_d.md) : CDC BMI
  reference data
- [`syngrowth`](https://growthcleanr.org/reference/syngrowth.md) :
  syngrowth
- [`tanner_ht_vel`](https://growthcleanr.org/reference/tanner_ht_vel.md)
  : Tanner Growth Velocity Table
- [`child_z3_reference`](https://growthcleanr.org/reference/child_z3_reference.md)
  : Child unit-error-range reference values
- [`who_ht_maxvel`](https://growthcleanr.org/reference/who_ht_maxvel.md)
  : WHO Maximum Height Velocity for (3\\\sigma\\)
- [`who_ht_vel_3sd`](https://growthcleanr.org/reference/who_ht_vel_3sd.md)
  : WHO Height Velocity for (3\\\sigma\\)
- [`fent_foraga`](https://growthcleanr.org/reference/fent_foraga.md) :
  Approximated Fenton Median Weights by Gestational Age
- [`fenton2025_ms_lookup_smoothed`](https://growthcleanr.org/reference/fenton2025_ms_lookup_smoothed.md)
  : Approximated Fenton CSD Values (M, S_upper, S_lower)
- [`growth_cdc_ext_infants`](https://growthcleanr.org/reference/growth_cdc_ext_infants.md)
  : CDC Growth Reference Table
- [`growth_who_ext`](https://growthcleanr.org/reference/growth_who_ext.md)
  : WHO Growth Reference Table
- [`rc-reference-medians`](https://growthcleanr.org/reference/rc-reference-medians.md)
  : Recentering reference medians
- [`who_hc_maxvel`](https://growthcleanr.org/reference/who_hc_maxvel.md)
  : WHO Maximum Head Circumference Velocity for (3\\\sigma\\)
- [`who_hc_vel_3sd`](https://growthcleanr.org/reference/who_hc_vel_3sd.md)
  : WHO Head Circumference Velocity for (3\\\sigma\\)
