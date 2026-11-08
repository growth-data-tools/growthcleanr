skip_on_cran()
library(growthcleanr)
library(data.table)

# Corroboration of the GA correction and the UER3 unit-error
# selection in Evil Twins. Small constructed subjects; measurements chosen so
# the relevant z-scores and unit-error ratios are unambiguous.
#
# Correction corroboration: a subject's low first weight only earns GA
# correction if true prematurity is corroborated by EITHER
#   form 1 -- a consistent early weight trajectory (>= 2 weights < 2y whose
#             corrected early spread is no worse than uncorrected), OR
#   form 2 -- a same-day HEIGHTCM *or* HEADCM at the first weight that is itself
#             low (uncorrected z <= -1.5) AND no more than 3.5 z above the first
#             weight's uncorrected z (so a merely-somewhat-low HT/HC cannot
#             corroborate a far-lower, e.g. misplaced-decimal, birth weight).
# A misplaced-decimal birth weight (low weight, normal same-day length, no
# consistent early follow-up) is corroborated by neither, so its correction is
# withdrawn and the implausible value is caught by the trajectory steps.
# Because form 1 / HT / HC are ORed, HC only decides the outcome when there is
# no weight-trajectory corroboration and no confirming HT.

# Assert on the Detailed code (keeps the step-level Exclude-C-* literals);
# the default `exclude` carries the consolidated Summary code.
ccode <- function(res, s, p, a) {
  r <- res[res$subjid == s & res$param == p & res$agedays == a, ]
  as.character(r$exclude_detailed[1])
}

test_that("GA correction is gated on corroboration of prematurity", {
  # A: implausibly low birth weight (1.0 kg, same as B's), a NORMAL same-day
  #    birth length, and no weight follow-up until > 2y. Neither corroboration
  #    form holds, so correction is withheld -> the birth is uncorrected (very
  #    low z) and excluded as an extreme trajectory outlier. (1.0 kg keeps the
  #    same-day BMI >= 3 so the Child Step 7 BMI hard limit does not pre-empt
  #    the corroboration logic; a misplaced-decimal 0.34 kg would now be caught
  #    there instead.)
  A <- data.table(
    id = c("A1", "A2", "A3", "A4"), subjid = "A", sex = 0L,
    param = c("WEIGHTKG", "HEIGHTCM", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(0L, 0L, 1000L, 1100L),
    measurement = c(1.0, 50, 13, 13.5))
  # B: real preterm -- low birth weight AND a low same-day birth length
  #    (z < -2). Form 2 corroborates -> correction kept -> birth Include.
  B <- data.table(
    id = c("B1", "B2", "B3", "B4"), subjid = "B", sex = 0L,
    param = c("WEIGHTKG", "HEIGHTCM", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(0L, 0L, 1000L, 1100L),
    measurement = c(1.0, 38, 13, 13.5))
  # C: real preterm with a consistent early weight trajectory (form 1) -> kept.
  C <- data.table(
    id = paste0("C", 1:4), subjid = "C", sex = 0L, param = "WEIGHTKG",
    agedays = c(0L, 60L, 120L, 1000L), measurement = c(1.0, 3.5, 5.5, 12))

  res <- as.data.table(cleangrowth(rbind(A, B, C), quietly = TRUE, exclude_detail = TRUE))

  # low birth weight with normal length, no early follow-up -> excluded
  expect_equal(ccode(res, "A", "WEIGHTKG", 0), "Exclude-C-Traj-Extreme")
  # real preterm corroborated by a low birth length (form 2) -> kept
  expect_equal(ccode(res, "B", "WEIGHTKG", 0), "Include")
  # real preterm corroborated by consistent early weights (form 1) -> kept
  expect_equal(ccode(res, "C", "WEIGHTKG", 0), "Include")
})

test_that("HT/HC confirm-correction: HC corroborates only as last resort, capped at 3.5 z", {
  # corr_pt captures the rule's direct output (correction kept vs reverted),
  # which is more stable than the downstream trajectory label.
  cpt <- function(res, s) as.character(res[subjid == s & param == "WEIGHTKG" &
                                             agedays == 20L, corr_pt][1])

  # All four: low first weight (z < -2, < 10 mo) with only ONE weight before 2y,
  # so form 1 (>= 2 early weights) cannot fire. The birth day's HT/HC decides.

  # D -- no HT; a low HC (z ~ -2.9, within 3.5 of the WT z ~ -4.3) corroborates
  #      -> correction KEPT (old HT-only rule would have reverted this).
  D <- data.table(id = paste0("D", 1:5), subjid = "D", sex = 0L,
    param = c("WEIGHTKG", "HEADCM", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(20L, 20L, 900L, 1100L, 1300L), measurement = c(1.8, 33, 12.5, 14, 15.5))
  # E -- HC high (z ~ +7, fails the <= -1.5 gate) -> reverts -> birth WT excluded.
  E <- copy(D); E[, subjid := "E"]; E[, id := paste0("E", 1:5)]; E[param == "HEADCM", measurement := 45]
  # F -- a confirming HT (z ~ -4.8, within 3.5) keeps the correction EVEN WITH a
  #      bad high HC present: HT takes precedence, HC does not override.
  F <- data.table(id = paste0("F", 1:6), subjid = "F", sex = 0L,
    param = c("WEIGHTKG", "HEIGHTCM", "HEADCM", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(20L, 20L, 20L, 900L, 1100L, 1300L), measurement = c(2.2, 44, 45, 12.5, 14, 15.5))
  # G -- HC passes the <= -1.5 gate (z ~ -1.7) but sits ~4 z above a very low WT
  #      (z ~ -5.6): the 3.5 cap blocks it -> reverts (isolates the cap vs D).
  G <- data.table(id = paste0("G", 1:5), subjid = "G", sex = 0L,
    param = c("WEIGHTKG", "HEADCM", "WEIGHTKG", "WEIGHTKG", "WEIGHTKG"),
    agedays = c(20L, 20L, 900L, 1100L, 1300L), measurement = c(1.2, 34.5, 12.5, 14, 15.5))

  res <- as.data.table(cleangrowth(rbind(D, E, F, G), quietly = TRUE,
                                   exclude_detail = TRUE, corr_detail = TRUE))

  expect_equal(cpt(res, "D"), "Corrected")     # HC corroborates
  expect_equal(cpt(res, "E"), "Uncorrected")   # HC too high, fails the gate
  expect_equal(cpt(res, "F"), "Corrected")     # confirming HT overrides bad HC
  expect_equal(cpt(res, "G"), "Uncorrected")   # 3.5 cap blocks an otherwise-low HC

  # Kept corrections leave the birth weight Included; reverted ones expose it.
  expect_equal(ccode(res, "D", "WEIGHTKG", 20), "Include")
  expect_equal(ccode(res, "F", "WEIGHTKG", 20), "Include")
  expect_true(ccode(res, "E", "WEIGHTKG", 20) != "Include")
  expect_true(ccode(res, "G", "WEIGHTKG", 20) != "Include")
})

test_that("UER3: a clean unit-error 2-group SP drops the unit-error block", {
  # Weights split into a plausible cluster (~7-9 kg) and a ~2.2x unit-error
  # block (~17-20 kg). The group mean ratio (~2.4) is in the WT unit-error band
  # and the high block is all one z sign, so UER3 fires and the Evil Twins
  # selection drops the high (unit-error) values rather than the baseline.
  U <- data.table(
    id = paste0("U", 1:5), subjid = "U", sex = 0L, param = "WEIGHTKG",
    agedays = c(120L, 240L, 360L, 480L, 600L),
    measurement = c(6.87, 8.69, 16.94, 18.59, 20.21))
  res <- as.data.table(cleangrowth(U, quietly = TRUE, exclude_detail = TRUE))
  expect_equal(ccode(res, "U", "WEIGHTKG", 360), "Exclude-C-Evil-Twins")
  expect_equal(ccode(res, "U", "WEIGHTKG", 480), "Exclude-C-Evil-Twins")
})
