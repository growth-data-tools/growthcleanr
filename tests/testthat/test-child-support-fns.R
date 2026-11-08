testthat::skip_on_cran()
library(growthcleanr)
library(data.table)

# =============================================================================
# Child support-function unit tests. These reach the package-internal helpers directly via ::: and
# assert their behavior in isolation, so a silent logic flip in a helper that
# nets to a zero count delta in the frozen regression suites still fails here.
#
# Every expected value below was confirmed by running the helper once before
# being frozen here (not hand-derived).
# =============================================================================

# ---------------------------------------------------------------------------
# get_dop() mapping (Designated Other Parameter)
# WEIGHTKG <-> HEIGHTCM; HEADCM -> HEIGHTCM (no reverse); the documented
# fall-through sends anything non-WT/non-HT (e.g. LENGTHCM) to HEIGHTCM.
# ---------------------------------------------------------------------------
test_that("get_dop() maps each param to its designated other parameter", {
  get_dop <- growthcleanr:::get_dop
  expect_equal(get_dop("WEIGHTKG"), "HEIGHTCM")
  expect_equal(get_dop("HEIGHTCM"), "WEIGHTKG")
  expect_equal(get_dop("HEADCM"),   "HEIGHTCM")
  # fall-through (no validation): LENGTHCM lands in the HEADCM branch
  expect_equal(get_dop("LENGTHCM"), "HEIGHTCM")
})

# ---------------------------------------------------------------------------
# .child_valid() include-flag matrix
# Base set = rows not starting with "Exclude"; the three include.* flags each
# re-admit exactly one temporarily-excluded category. Missing / Not-Cleaned /
# PIV never come back. Works on the text of the exclude column.
# ---------------------------------------------------------------------------
test_that(".child_valid() include flags re-admit only their own category", {
  cv <- growthcleanr:::.child_valid
  codes <- c("Include", "Exclude-Missing", "Exclude-Not-Cleaned",
             "Exclude-C-Temp-Same-Day", "Exclude-C-Extraneous",
             "Exclude-C-CF", "Exclude-C-PIV")
  df <- data.frame(exclude = codes, stringsAsFactors = FALSE)
  names(codes) <- codes  # for readable indexing

  # default: only Include is valid
  expect_equal(cv(df), c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))

  # each flag adds exactly one category
  expect_equal(cv(df, include.temporary.extraneous = TRUE),
               c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(cv(df, include.extraneous = TRUE),
               c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(cv(df, include.carryforward = TRUE),
               c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))

  # all three together; Missing / Not-Cleaned / PIV stay FALSE
  expect_equal(cv(df, TRUE, TRUE, TRUE),
               c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))

  # vector-input path equals data.frame path
  expect_identical(cv(codes), cv(df))
})

# ---------------------------------------------------------------------------
# .child_z3() boundary + row alignment
# Looks up the recentered z = -3 / +3 measurements from child_z3_reference.
# (a) a known cell returns finite zn3 < zp3; (b) HEADCM beyond the 5y cleaning
# range returns NA endpoints; (c) a reverse-age query preserves input order.
# ---------------------------------------------------------------------------
test_that(".child_z3() returns finite ordered endpoints in range and NA beyond", {
  z3 <- growthcleanr:::.child_z3

  r <- z3("WEIGHTKG", 0L, 365L)
  expect_true(is.finite(r$zn3) && is.finite(r$zp3))
  expect_lt(r$zn3, r$zp3)

  # HEADCM is cleaned under 5y; the z3 table covers HC to 1826 days, and
  # beyond that there is no z3 row
  rh <- z3("HEADCM", 0L, 2000L)
  expect_true(is.na(rh$zn3) && is.na(rh$zp3))
  rh3 <- z3("HEADCM", 0L, 1300L)
  expect_true(is.finite(rh3$zn3) && is.finite(rh3$zp3))

  # within-range HEADCM does resolve (guards against an over-broad NA)
  rh2 <- z3("HEADCM", 0L, 365L)
  expect_true(is.finite(rh2$zn3) && is.finite(rh2$zp3))
})

test_that(".child_z3() preserves input row order (the .ord restore)", {
  z3 <- growthcleanr:::.child_z3
  ages_rev <- c(700L, 400L, 100L)
  rev_out  <- z3(rep("WEIGHTKG", 3), rep(0L, 3), ages_rev)
  sort_out <- z3(rep("WEIGHTKG", 3), rep(0L, 3), sort(ages_rev))
  # reverse query reversed equals the sorted query
  expect_equal(rev(rev_out$zp3), sort_out$zp3)
  # zp3 increases with age, so the reverse query is monotone decreasing
  expect_true(all(diff(rev_out$zp3) < 0))
})

# ---------------------------------------------------------------------------
# ewma() cache_env reuses the delta (weight) matrix across z vectors
# The Step 11 optimization passes one cache_env to the tbc call then the ctbc
# call. The cache holds only the age/exponent-derived weight matrix, so the
# second call on a DIFFERENT z must equal a fresh, uncached EWMA of that z.
# ---------------------------------------------------------------------------
test_that("ewma() with a shared cache_env matches uncached EWMA per z vector", {
  ewma <- growthcleanr:::ewma
  ages  <- c(100, 200, 400, 800, 1600)
  exp   <- rep(-2, 5)
  z_tbc  <- c(0.1, 0.5, -0.3, 1.2, -0.8)
  z_ctbc <- c(0.2, -0.4, 0.9, -1.1, 0.3)

  e <- new.env()
  r_tbc  <- ewma(ages, z_tbc,  exp, cache_env = e)  # populates e$delta
  r_ctbc <- ewma(ages, z_ctbc, exp, cache_env = e)  # reuses e$delta

  expect_equal(r_tbc,  ewma(ages, z_tbc,  exp, cache_env = NULL))
  expect_equal(r_ctbc, ewma(ages, z_ctbc, exp, cache_env = NULL))
})

# ---------------------------------------------------------------------------
# ewma_cache_update() neighbor-exponent rebuild
# Removing an observation can change a neighbor's widest age gap and thus its
# EWMA exponent. The O(n) incremental update must reproduce a full O(n^2)
# rebuild on the reduced set, and must return NULL for an unknown id.
# ---------------------------------------------------------------------------
test_that("ewma_cache_update() matches a fresh rebuild and rejects unknown ids", {
  ci <- growthcleanr:::ewma_cache_init
  cu <- growthcleanr:::ewma_cache_update

  # per-observation exponent from the widest neighbor age gap (the rule the
  # cache uses internally: <=1y -> -1.5, >=3y -> -3.5, linear between)
  exp_from_gaps <- function(ages) {
    n <- length(ages); ev <- numeric(n)
    for (i in seq_len(n)) {
      db <- if (i > 1) abs(ages[i] - ages[i - 1]) else NA
      da <- if (i < n) abs(ages[i + 1] - ages[i]) else NA
      ay <- max(db, da, na.rm = TRUE) / 365.25
      ev[i] <- if (ay <= 1) -1.5 else if (ay >= 3) -3.5 else -1.5 - (ay - 1)
    }
    ev
  }

  ages <- c(0, 300, 360, 800, 2000)  # removing id 3 (age 360) widens a neighbor gap
  ids  <- 1:5
  z    <- c(0, 0.5, -0.5, 1, -1)

  cache <- ci(ages, z, z, exp_from_gaps(ages), ids)
  upd   <- cu(cache, 3L)

  red_ages <- ages[-3]; red_ids <- ids[-3]; red_z <- z[-3]
  fresh <- ci(red_ages, red_z, red_z, exp_from_gaps(red_ages), red_ids)

  expect_equal(upd$ewma.all,    fresh$ewma.all)
  expect_equal(upd$ewma.before, fresh$ewma.before)
  expect_equal(upd$ewma.after,  fresh$ewma.after)
  expect_equal(upd$exp_vals,    exp_from_gaps(red_ages))

  # an id not present in the cache signals "do a full rebuild" via NULL
  expect_null(cu(cache, 99L))
})
