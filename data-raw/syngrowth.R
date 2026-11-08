# Build data/syngrowth.rda (the syngrowth package dataset) from data-raw/syngrowth.csv.
# data-raw/ is not part of the CRAN package.

syngrowth <- read.csv('data-raw/syngrowth.csv')

usethis::use_data(syngrowth, compress="xz", overwrite=T)
