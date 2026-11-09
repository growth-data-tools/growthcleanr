# growthcleanr: a data cleaner for anthropometric measurements

growthcleanr flags implausible anthropometric (height, weight, and head
circumference) measurements in longitudinal datasets, such as those from
electronic health records. It also identifies other measurements that
should often be excluded from analyses.

## Getting started

The main function is
[`cleangrowth`](https://growthcleanr.org/reference/cleangrowth.md). Pass
it a data frame with one row per measurement and columns `subjid`,
`param`, `agedays`, `sex`, and `measurement` (plus an optional per-row
`id`). Semi-wide and wide layouts, alternate column names, and
date-of-birth / visit-date ages are also supported. growthcleanr will
not delete or edit data: it returns your data with an added `exclude`
column labeling each row `"Include"` or an exclusion code naming why it
recommends exclusion.

The function
[`growthcleanr_app`](https://growthcleanr.org/reference/growthcleanr_app.md)
opens a Shiny app that runs growthcleanr without writing code. A
command-line driver is documented in
[`vignette("command-line")`](https://growthcleanr.org/articles/command-line.md).

New to growthcleanr? Start with
[`vignette("new-start-here")`](https://growthcleanr.org/articles/new-start-here.md).
See [`vignette("output")`](https://growthcleanr.org/articles/output.md)
for more information including the returned columns and
[`vignette("configuration")`](https://growthcleanr.org/articles/configuration.md)
for the full index of arguments and thresholds.

## See also

Useful links:

- <https://growthcleanr.org/>

- <https://github.com/growth-data-tools/growthcleanr>

- Report bugs at
  <https://github.com/growth-data-tools/growthcleanr/issues>

## Author

**Maintainer**: Carrie Daymont <cdaymont@pennstatehealth.psu.edu>
\[copyright holder\]

Authors:

- Robert W Grundmeier

- Jeffrey Miller

- Diego Campos

- Dan Chudnov

- Hannah De los Santos

- David Freedman

- Wen Hu

- Chris Penney

Other contributors:

- Jesse Anderson \[contributor\]

- Matt Boyas \[contributor\]

- Lusha Cao \[contributor\]

- Sara Couture \[contributor\]

- Prashila Dullabh \[contributor\]

- Jess Fernandez \[contributor\]

- Andrew Gregorowicz \[contributor\]

- Lyudmila Kompaniyets \[contributor\]

- A Russell Localio \[contributor\]

- Ruth Nwefo \[contributor\]

- Max Olivier \[contributor\]

- Samantha Pierce \[contributor\]

- Michelle E Ross \[contributor\]

- Steffani Silva \[contributor\]

- Mithuna Srinivasan \[contributor\]

- Marissa Sucosky \[contributor\]

- Jennifer Wiltz \[contributor\]

- Hanzhe Zhang \[contributor\]

- Alexander G Fiks \[scientific advisor\]

- Richard C (Mort) Wasserman \[scientific advisor\]

- Andreas Achilleos \[scientific advisor\]

- Nabeel Akhtar \[scientific advisor\]

- Sarah Akhtar \[scientific advisor\]

- Wilbert Beachy \[scientific advisor\]

- Colin Bridgeman \[scientific advisor\]

- Jessica Butts \[scientific advisor\]

- Emily Despeaux \[scientific advisor\]

- Christine Irvin \[scientific advisor\]

- Madiha Jamil \[scientific advisor\]

- Robert Marquis \[scientific advisor\]

- Abigail Nelson \[scientific advisor\]

- Sheila Nguyen \[scientific advisor\]

- Taraneh Soleymani \[scientific advisor\]

- Ronald Williams \[scientific advisor\]
