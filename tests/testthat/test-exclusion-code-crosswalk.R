testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Exclusion-code crosswalk: internal literal -> Detailed -> Summary
#
# Locks that the shipped two-level mapping matches the canonical crosswalk in
# vignette("exclusion-codes"). The map vectors (exclude.map.detailed /
# exclude.map.summary) live INSIDE cleangrowth(), so they cannot be introspected
# via ::: ; instead we validate them through real cleangrowth() output.
#
# This is the single place the Detailed -> Summary folding is asserted directly.
# It also guards the silent-NA-factor class: a new internal code added without a
# crosswalk entry would surface an unmapped Detailed code (or an NA) and fail here.
# =============================================================================

# Canonical Detailed -> Summary oracle (mirror of vignette("exclusion-codes")).
# Keys: every Detailed code the algorithm can emit. Values: the Summary fold.
# (Internal-only Exclude-C-Temp-Same-Day never reaches output, so it is absent.)
detailed_to_summary <- c(
  # shared / preprocessing (no -C-/-A- prefix at any level)
  'Include'                            = 'Include',
  'Exclude-Missing-Info'               = 'Exclude-Missing-Info',
  'Exclude-HC-Out-of-Range'            = 'Exclude-HC-Out-of-Range',
  'Exclude-Not-GC-Param'               = 'Exclude-Not-GC-Param',
  'Exclude-Identical'                  = 'Exclude-Identical',
  'Exclude-Extraneous'                 = 'Exclude-Extraneous',
  # child
  'Exclude-C-CF'                       = 'Exclude-CF',
  'Exclude-C-Hard-Limit'                      = 'Exclude-Hard-Limit',
  'Exclude-C-Evil-Twins'               = 'Exclude-Extreme',
  'Exclude-C-Traj-Extreme'             = 'Exclude-Extreme',
  'Exclude-C-Traj'                     = 'Exclude-Pattern',
  'Exclude-C-Abs-Diff'                 = 'Exclude-Pattern',
  'Exclude-C-Pair'                     = 'Exclude-Pair',
  'Exclude-C-Single'                   = 'Exclude-Single',
  'Exclude-C-Unevaluable-Trajectory'   = 'Exclude-Traj-Uneval',
  # adult
  'Exclude-A-Hard-Limit'                      = 'Exclude-Hard-Limit',
  'Exclude-A-Scale-Max'                = 'Exclude-Scale-Max',
  'Exclude-A-Evil-Twins'               = 'Exclude-Extreme',
  'Exclude-A-Traj-Extreme'             = 'Exclude-Extreme',
  'Exclude-A-Ord-Pair'                 = 'Exclude-Pattern',
  'Exclude-A-Window'                   = 'Exclude-Pattern',
  'Exclude-A-2D-Ordered'               = 'Exclude-Pattern',
  'Exclude-A-2D-Non-Ordered'           = 'Exclude-Pattern',
  'Exclude-A-Traj-Moderate'            = 'Exclude-Pattern',
  'Exclude-A-Traj-Moderate-Error-Load' = 'Exclude-Pattern',
  'Exclude-A-Single'                   = 'Exclude-Single',
  'Exclude-A-Unevaluable-Trajectory'   = 'Exclude-Traj-Uneval'
)

# The 14 valid Summary factor levels (mirror of exclude.levels.summary).
summary_levels <- c(
  'Include', 'Exclude-Not-GC-Param', 'Exclude-Missing-Info', 'Exclude-HC-Out-of-Range',
  'Exclude-Identical', 'Exclude-Extraneous',
  'Exclude-Hard-Limit', 'Exclude-CF', 'Exclude-Scale-Max', 'Exclude-Extreme', 'Exclude-Pattern',
  'Exclude-Pair', 'Exclude-Single', 'Exclude-Traj-Uneval'
)

run_codes <- function(d) {
  res <- as.data.table(suppressWarnings(
    cleangrowth(d, quietly = TRUE, exclude_detail = TRUE)))
  res[, .(detailed = as.character(exclude_detailed),
          summary  = as.character(exclude))]
}

# Breadth: a syngrowth subset spans child + adult ages and exercises many codes.
data("syngrowth", package = "growthcleanr", envir = environment())
sg <- as.data.table(syngrowth)
sg_subs <- sort(unique(as.character(sg$subjid)))[1:250]
sg <- sg[as.character(subjid) %in% sg_subs]

# Guaranteed coverage for codes the subset may not hit: Missing-Info (NA),
# HC-Out-of-Range (HC > 5y), Not-GC-Param (param gc does not handle), and an
# adult PIV (5 kg adult weight, below the looser PIV floor of 30 kg).
constructed <- data.table(
  id      = paste0("X", 1:8),
  subjid  = c(rep("Xc", 4), rep("Xa", 4)),
  sex     = 0L,
  param   = c("WEIGHTKG", "HEADCM", "BLOODPRESSURE", "WEIGHTKG",
              "HEIGHTCM", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
  agedays = c(365L, 2000L, 365L, 365L,
              8000L, 8000L, 8030L, 8060L),
  measurement = c(NA, 50, 120, 12,
                  170, 80, 5, 80)
)

codes <- rbind(run_codes(sg), run_codes(constructed))

test_that("every output row has non-NA Detailed and Summary codes", {
  expect_false(any(is.na(codes$detailed)))
  expect_false(any(is.na(codes$summary)))
})

test_that("every observed Detailed code is in the canonical crosswalk", {
  obs <- unique(codes$detailed)
  unmapped <- setdiff(obs, names(detailed_to_summary))
  expect_equal(unmapped, character(0),
    info = paste("Detailed codes with no crosswalk entry:",
                 paste(unmapped, collapse = ", ")))
})

test_that("Summary equals the canonical fold of the Detailed code, every row", {
  expected <- unname(detailed_to_summary[codes$detailed])
  mism <- which(expected != codes$summary)
  expect_equal(codes$summary, expected,
    info = if (length(mism))
      paste("First mismatch: Detailed", codes$detailed[mism[1]],
            "gave Summary", codes$summary[mism[1]],
            "but crosswalk expects", expected[mism[1]])
    else "")
})

test_that("every observed Summary code is a valid Summary factor level", {
  expect_true(all(unique(codes$summary) %in% summary_levels))
})

test_that("crosswalk coverage: shared, child, and adult folds are all exercised", {
  obs <- unique(codes$detailed)
  # deterministic shared/preprocessing codes from the constructed rows
  for (code in c("Include", "Exclude-Missing-Info", "Exclude-HC-Out-of-Range",
                 "Exclude-Not-GC-Param")) {
    expect_true(code %in% obs, info = paste("expected", code, "in output"))
  }
  # at least one child and one adult exclusion fold exercised
  expect_true(any(grepl("^Exclude-C-", obs)), info = "no child exclusion exercised")
  expect_true(any(grepl("^Exclude-A-", obs)), info = "no adult exclusion exercised")
})
