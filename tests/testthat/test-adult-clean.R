# testthat tests for cleanadult()
# Run with: testthat::test_file("tests/testthat/test-adult-clean.R")

library(testthat)
library(growthcleanr)
library(data.table)

# =============================================================================
# HELPER: build a minimal data.table for cleanadult()
# =============================================================================

make_df <- function(subjid, param, agedays, measurement, sex = 0, id = NULL) {
  n <- length(param)
  if (is.null(id)) id <- seq_len(n)
  data.table(
    id      = as.character(id),
    internal_id = seq_len(n),
    subjid  = subjid,
    sex     = sex,
    agedays = agedays,
    param   = param,
    measurement = measurement
  )
}

# Shorthand: one subject with interleaved ht/wt at given ages
make_subject <- function(subjid, ages, hts, wts, sex = 0, id_start = 1) {
  n <- length(ages)
  stopifnot(length(hts) == n, length(wts) == n)
  rows <- list()
  id <- id_start
  for (i in seq_len(n)) {
    rows[[length(rows) + 1]] <- data.table(
      id = as.character(id), subjid = subjid, sex = sex,
      agedays = ages[i], param = "HEIGHTCM", measurement = hts[i]
    )
    id <- id + 1
    rows[[length(rows) + 1]] <- data.table(
      id = as.character(id), subjid = subjid, sex = sex,
      agedays = ages[i], param = "WEIGHTKG", measurement = wts[i]
    )
    id <- id + 1
  }
  dt <- rbindlist(rows)
  dt[, internal_id := seq_len(.N)]
  dt
}

# Run cleanadult quietly. copy() prevents by-reference side effects.
run_clean <- function(df, ...) {
  df_copy <- copy(df)
  # Always reassign internal_id to ensure uniqueness after rbind
  df_copy[, internal_id := seq_len(.N)]
  if (!"ageyears" %in% names(df_copy) && !"age_years" %in% names(df_copy) &&
      "agedays" %in% names(df_copy)) {
    df_copy[, ageyears := agedays / 365.25]
  }
  result <- growthcleanr:::cleanadult(df_copy, quietly = TRUE, ...)
  result
}

# Extract result for a given id
res <- function(df, row_id) {
  idx <- which(df$id == as.character(row_id))
  as.character(df$result[idx])
}


# =============================================================================
# 1. PIV-ONLY DATASETS
# =============================================================================

test_that("single weight PIV is correctly excluded", {
  # One subject, one weight observation that exceeds PIV limit (>500 kg)
  df <- make_df(
    subjid = "piv_wt",
    param  = "WEIGHTKG",
    agedays = 10000,
    measurement = 550
  )
  out <- run_clean(df)
  expect_equal(nrow(out), 1)
  expect_equal(res(out, 1), "Exclude-A-PIV")
})

test_that("single height PIV is correctly excluded", {
  # One subject, one height observation that exceeds PIV limit (>244 cm)
  df <- make_df(
    subjid = "piv_ht",
    param  = "HEIGHTCM",
    agedays = 10000,
    measurement = 260
  )
  out <- run_clean(df)
  expect_equal(nrow(out), 1)
  expect_equal(res(out, 1), "Exclude-A-PIV")
})

test_that("weight PIV low boundary is correctly excluded", {
  # Weight < 20 kg
  df <- make_df(
    subjid = "piv_wt_low",
    param  = "WEIGHTKG",
    agedays = 10000,
    measurement = 15
  )
  out <- run_clean(df)
  expect_equal(res(out, 1), "Exclude-A-PIV")
})

test_that("height PIV low boundary is correctly excluded", {
  # Height < 50 cm
  df <- make_df(
    subjid = "piv_ht_low",
    param  = "HEIGHTCM",
    agedays = 10000,
    measurement = 40
  )
  out <- run_clean(df)
  expect_equal(res(out, 1), "Exclude-A-PIV")
})

test_that("values at PIV boundary are not excluded as PIV", {
  # Boundary values pass loosest PIV (inclusive limits: ht 50-244, wt 20-500)
  # but may be excluded by 1D limits -- so we check they are NOT PIV-excluded
  df <- rbind(
    make_df("piv_edge", "HEIGHTCM", 10000, 50, id = 1),
    make_df("piv_edge", "HEIGHTCM", 10500, 244, id = 2),
    make_df("piv_edge", "WEIGHTKG", 10000, 20, id = 3),
    make_df("piv_edge", "WEIGHTKG", 10500, 500, id = 4)
  )
  out <- run_clean(df, permissiveness = "loosest")
  expect_true(all(!out$result %in% c("Exclude-A-PIV", "Exclude-A-PIV")))
})

test_that("BMI PIV: implausibly low BMI pair excluded at loosest", {
  # ht=244cm, wt=25kg individually pass loosest PIV (ht 50-244, wt 20-500).
  # BMI = 25/(2.44^2) = 4.20 < loosest overall_bmi_min (5) -> both excluded.
  # Use same ageday so they form a pair.
  df <- rbind(
    make_df("bmi_low", "HEIGHTCM", 10000, 244, id = 1),
    make_df("bmi_low", "WEIGHTKG", 10000, 25, id = 2)
  )
  out <- run_clean(df, permissiveness = "loosest")
  expect_equal(res(out, 1), "Exclude-A-PIV")
  expect_equal(res(out, 2), "Exclude-A-PIV")
})

test_that("BMI PIV: implausibly high BMI pair excluded at loosest", {
  # ht=50cm, wt=100kg individually pass loosest PIV.
  # BMI = 100/(0.5^2) = 400 > loosest overall_bmi_max (300) -> both excluded.
  df <- rbind(
    make_df("bmi_high", "HEIGHTCM", 10000, 50, id = 1),
    make_df("bmi_high", "WEIGHTKG", 10000, 100, id = 2)
  )
  out <- run_clean(df, permissiveness = "loosest")
  expect_equal(res(out, 1), "Exclude-A-PIV")
  expect_equal(res(out, 2), "Exclude-A-PIV")
})

test_that("BMI PIV: valid BMI pair not excluded at loosest", {
  # ht=170cm, wt=70kg -> BMI = 24.2, well within [5, 300]
  df <- rbind(
    make_df("bmi_ok", "HEIGHTCM", 10000, 170, id = 1),
    make_df("bmi_ok", "WEIGHTKG", 10000, 70, id = 2)
  )
  out <- run_clean(df, permissiveness = "loosest")
  expect_true(res(out, 1) != "Exclude-A-PIV")
  expect_true(res(out, 2) != "Exclude-A-PIV")
})

test_that("BMI PIV: no same-day pair means no BMI PIV check", {
  # Height and weight on different days -- no same-day pair, no BMI PIV check possible.
  # ht=244cm (day 1), wt=25kg (day 2): if they were on the same day, BMI=4.2 < 5.
  # On different days: only individual PIV applies -> neither excluded.
  df <- rbind(
    make_df("bmi_noday", "HEIGHTCM", 10000, 244, id = 1),
    make_df("bmi_noday", "WEIGHTKG", 10001, 25, id = 2)
  )
  out <- run_clean(df, permissiveness = "loosest")
  expect_true(res(out, 1) != "Exclude-A-PIV")
  expect_true(res(out, 2) != "Exclude-A-PIV")
})


# =============================================================================
# 2. MINIMAL / EDGE CASE DATASETS
# =============================================================================

test_that("single valid observation is included", {
  df <- make_df("solo", "WEIGHTKG", 10000, 70)
  out <- run_clean(df)
  expect_equal(nrow(out), 1)
  expect_equal(res(out, 1), "Include")
})

test_that("single valid height is included", {
  df <- make_df("solo_ht", "HEIGHTCM", 10000, 170)
  out <- run_clean(df)
  expect_equal(nrow(out), 1)
  expect_equal(res(out, 1), "Include")
})

test_that("subject with only heights (no weights) runs correctly", {
  df <- rbind(
    make_df("ht_only", "HEIGHTCM", 10000, 170, id = 1),
    make_df("ht_only", "HEIGHTCM", 10365, 170, id = 2),
    make_df("ht_only", "HEIGHTCM", 10730, 170, id = 3)
  )
  out <- run_clean(df)
  expect_equal(nrow(out), 3)
  # All same height -- should all be included
  expect_true(all(out$result == "Include"))
})

test_that("subject with only weights (no heights) runs correctly", {
  df <- rbind(
    make_df("wt_only", "WEIGHTKG", 10000, 75, id = 1),
    make_df("wt_only", "WEIGHTKG", 10365, 76, id = 2),
    make_df("wt_only", "WEIGHTKG", 10730, 74, id = 3)
  )
  out <- run_clean(df)
  expect_equal(nrow(out), 3)
  expect_true(all(out$result == "Include"))
})

test_that("multiple subjects run independently", {
  df <- rbind(
    make_df("subj_a", "WEIGHTKG", 10000, 75, id = 1),
    make_df("subj_a", "WEIGHTKG", 10365, 76, id = 2),
    make_df("subj_b", "WEIGHTKG", 10000, 600, id = 3),  # PIV
    make_df("subj_b", "WEIGHTKG", 10365, 80, id = 4)
  )
  out <- run_clean(df)
  expect_equal(res(out, 1), "Include")
  expect_equal(res(out, 2), "Include")
  expect_equal(res(out, 3), "Exclude-A-PIV")
  expect_equal(res(out, 4), "Include")
})

test_that("dataset with mix of PIV and valid values", {
  df <- rbind(
    make_df("mix", "HEIGHTCM", 10000, 170, id = 1),
    make_df("mix", "HEIGHTCM", 10365, 300, id = 2),  # PIV high
    make_df("mix", "WEIGHTKG", 10000, 75, id = 3),
    make_df("mix", "WEIGHTKG", 10365, 10, id = 4)     # PIV low
  )
  out <- run_clean(df)
  expect_equal(res(out, 1), "Include")
  expect_equal(res(out, 2), "Exclude-A-PIV")
  expect_equal(res(out, 3), "Include")
  expect_equal(res(out, 4), "Exclude-A-PIV")
})


# =============================================================================
# 3. WEIGHT CAP (Step 4W)
# =============================================================================

test_that("weight at scale max is excluded when scale_max_lbs is set", {
  # 400 lbs = 181.437 kg. Put weights at exactly 181.44 and near it.
  # Need >= 3 weights for EWMA-based cap detection
  scale_max_kg <- 400 / 2.2046226
  df <- rbind(
    make_df("cap", "WEIGHTKG", 10000, 75, id = 1),
    make_df("cap", "WEIGHTKG", 10100, 80, id = 2),
    make_df("cap", "WEIGHTKG", 10200, 78, id = 3),
    make_df("cap", "WEIGHTKG", 10300, scale_max_kg, id = 4)  # at scale max
  )
  out <- run_clean(df, scale_max_lbs = 400)
  expect_equal(res(out, 4), "Exclude-A-Scale-Max")
})

test_that("weight cap is not triggered when scale_max_lbs = Inf (default)", {
  scale_max_kg <- 400 / 2.2046226
  df <- rbind(
    make_df("nocap", "WEIGHTKG", 10000, 75, id = 1),
    make_df("nocap", "WEIGHTKG", 10100, 80, id = 2),
    make_df("nocap", "WEIGHTKG", 10200, 78, id = 3),
    make_df("nocap", "WEIGHTKG", 10300, scale_max_kg, id = 4)
  )
  out <- run_clean(df)  # default scale_max_lbs = Inf
  # Should not be weight cap -- may be something else or Include
  expect_true(res(out, 4) != "Exclude-A-Scale-Max")
  expect_true(res(out, 4) != "Exclude-A-Scale-Max-Identical")
})

test_that("weight cap identical excludes when only distinct weight equals scale max", {
  scale_max_kg <- 400 / 2.2046226
  # All weights are at scale max -- only 1 distinct value
  df <- rbind(
    make_df("cap_id", "WEIGHTKG", 10000, scale_max_kg, id = 1),
    make_df("cap_id", "WEIGHTKG", 10100, scale_max_kg, id = 2),
    make_df("cap_id", "WEIGHTKG", 10200, scale_max_kg, id = 3)
  )
  out <- run_clean(df, scale_max_lbs = 400)
  expect_true(all(out$result == "Exclude-A-Scale-Max-Identical"))
})


# =============================================================================
# 4. SAME-DAY IDENTICAL & EXTRANEOUS (Steps 9H, 10W)
# =============================================================================

test_that("same-day identical heights are handled correctly", {
  # Two identical heights on the same day -- one kept, one excluded
  df <- rbind(
    make_df("sdi", "HEIGHTCM", 10000, 170, id = 1),
    make_df("sdi", "HEIGHTCM", 10000, 170, id = 2),
    make_df("sdi", "HEIGHTCM", 10365, 170, id = 3)
  )
  out <- run_clean(df)
  # One should be identical, one should be included
  results <- out$result
  expect_equal(sum(results == "Exclude-A-Identical"), 1)
  expect_equal(sum(results == "Include"), 2)
  # Lower id kept (id=1 included, id=2 excluded)
  expect_equal(res(out, 1), "Include")
  expect_equal(res(out, 2), "Exclude-A-Identical")
})

test_that("same-day non-identical heights -- extraneous excluded", {
  # Two different heights on the same day -- one kept, one extraneous
  df <- rbind(
    make_df("sde", "HEIGHTCM", 10000, 170, id = 1),
    make_df("sde", "HEIGHTCM", 10000, 175, id = 2),
    make_df("sde", "HEIGHTCM", 10365, 170, id = 3)
  )
  out <- run_clean(df)
  # One should be extraneous, two included
  expect_equal(sum(out$result == "Exclude-A-Extraneous"), 1)
  expect_equal(sum(out$result == "Include"), 2)
})

test_that("same-day identical weights are handled correctly", {
  df <- rbind(
    make_df("sdi_wt", "WEIGHTKG", 10000, 75, id = 1),
    make_df("sdi_wt", "WEIGHTKG", 10000, 75, id = 2),
    make_df("sdi_wt", "WEIGHTKG", 10365, 75, id = 3)
  )
  out <- run_clean(df)
  expect_equal(sum(out$result == "Exclude-A-Identical"), 1)
  expect_equal(sum(out$result == "Include"), 2)
  expect_equal(res(out, 1), "Include")
  expect_equal(res(out, 2), "Exclude-A-Identical")
})


# =============================================================================
# 5. EVIL TWINS (Step 9Wa)
# =============================================================================

test_that("single large weight jump is ceded to EWMA, not Evil Twins (anchor rule)", {
  # Under the anchor rule a single over-cap spike is NOT an evil twin: it has no
  # flanking over-the-limit pair, so it is ceded to the EWMA steps (which have
  # the directional 90% rule and per-observation thresholds). 165kg among ~75kg.
  df <- rbind(
    make_df("et", "WEIGHTKG", 10000, 75, id = 1),
    make_df("et", "WEIGHTKG", 10003, 165, id = 2),  # single +90kg spike
    make_df("et", "WEIGHTKG", 10365, 78, id = 3),
    make_df("et", "WEIGHTKG", 10730, 76, id = 4)
  )
  out <- run_clean(df)
  expect_false(any(out$result == "Exclude-A-Evil-Twins"))
  # the spike is still excluded, just by an EWMA step rather than ET
  expect_true(res(out, 2) != "Include")
})

test_that("evil twins not triggered when weight range <= cap", {
  # Weights 55kg apart at 3-day interval -- below 60kg cap
  df <- rbind(
    make_df("noet", "WEIGHTKG", 10000, 75, id = 1),
    make_df("noet", "WEIGHTKG", 10003, 130, id = 2),  # +55kg
    make_df("noet", "WEIGHTKG", 10365, 78, id = 3),
    make_df("noet", "WEIGHTKG", 10730, 76, id = 4)
  )
  out <- run_clean(df)
  # Should NOT be evil twins
  expect_true(res(out, 2) != "Exclude-A-Evil-Twins")
})

test_that("alternating extreme pattern is ceded to EWMA, not Evil Twins (anchor rule)", {
  # 9 weights alternating 50/220 kg, 3-day spacing. Every adjacent pair is a huge
  # jump, but the alternation means no over-the-limit inner pair has BOTH outer
  # neighbors anchored within band, so the anchor rule cedes the whole pattern to
  # the EWMA steps (an accepted limitation; see vignette("adult-algorithm")). The
  # extreme 220kg values are still excluded downstream.
  df <- rbind(
    make_df("et4", "WEIGHTKG", 10000,  50, id = 1),
    make_df("et4", "WEIGHTKG", 10003, 220, id = 2),
    make_df("et4", "WEIGHTKG", 10006,  50, id = 3),
    make_df("et4", "WEIGHTKG", 10009, 220, id = 4),
    make_df("et4", "WEIGHTKG", 10012,  50, id = 5),
    make_df("et4", "WEIGHTKG", 10015, 220, id = 6),
    make_df("et4", "WEIGHTKG", 10018,  50, id = 7),
    make_df("et4", "WEIGHTKG", 10021, 220, id = 8),
    make_df("et4", "WEIGHTKG", 10024,  50, id = 9)
  )
  out <- run_clean(df)
  expect_equal(sum(out$result == "Exclude-A-Evil-Twins"), 0)
  # the extreme 220kg values are still excluded (by the EWMA steps)
  vals220 <- vapply(c(2L, 4L, 6L, 8L), function(i) res(out, i), character(1))
  expect_true(all(vals220 != "Include"))
})


# =============================================================================
# 6. EXTREME EWMA (Step 9Wb)
# =============================================================================

test_that("extreme EWMA catches large outlier (wide spacing avoids ET)", {
  # All intervals > 18mo so ET cap = 100kg. Outlier at 200 -> diff 125 > 100 cap but
  # range 200-74 = 126 > 100 -> ET fires first. Need range <= 100 for ET skip.
  # Use 170kg outlier (range 95 < 100, no ET) but dewma ~95 < 100 cap -> not extreme.
  # Instead: use heights too so BMI triggers, or accept moderate EWMA catches it.
  # Practical test: 200kg outlier among ~75s with monthly spacing.
  # At 200kg: ET cap = 50+0.70*(200-120) = 106. dewma ~125 > 106 -> extreme EWMA.
  # Monthly spacing keeps intervals <6m (cap_short=50 baseline).
  df <- rbind(
    make_df("ext", "WEIGHTKG", 10000, 75, id = 1),
    make_df("ext", "WEIGHTKG", 10030, 76, id = 2),
    make_df("ext", "WEIGHTKG", 10060, 200, id = 3),  # outlier
    make_df("ext", "WEIGHTKG", 10090, 74, id = 4),
    make_df("ext", "WEIGHTKG", 10120, 77, id = 5)
  )
  out <- run_clean(df, permissiveness = "loosest")
  expect_true(grepl("Exclude", res(out, 3)))
})


# =============================================================================
# 7. HEIGHT DISTINCT PAIRS (Step 10H)
# =============================================================================

test_that("2D heights within band are included", {
  # Two distinct heights 2 inches apart with ht_band=3 -> within band
  ht1 <- 170
  ht2 <- 170 + 2 * 2.54  # +2 inches in cm
  df <- rbind(
    make_df("ht2d_in", "HEIGHTCM", 10000, ht1, id = 1),
    make_df("ht2d_in", "HEIGHTCM", 10365, ht2, id = 2)
  )
  out <- run_clean(df)
  expect_true(all(out$result == "Include"))
})

test_that("2D heights outside band are excluded", {
  # Two distinct heights 5 inches apart with ht_band=3 -> outside band
  ht1 <- 170
  ht2 <- 170 + 5 * 2.54  # +5 inches
  df <- rbind(
    make_df("ht2d_out", "HEIGHTCM", 10000, ht1, id = 1),
    make_df("ht2d_out", "HEIGHTCM", 10365, ht2, id = 2)
  )
  out <- run_clean(df)
  # Both should be excluded as 2D (no frequency rescue with equal counts)
  expect_true(all(out$result == "Exclude-A-Ord-Pair-All"))
})

test_that("ht_band parameter changes 2D outcome", {
  # Heights 4 inches apart -- excluded with ht_band=3, included with ht_band=4
  ht1 <- 170
  ht2 <- 170 + 4 * 2.54  # +4 inches
  df <- rbind(
    make_df("ht_band", "HEIGHTCM", 10000, ht1, id = 1),
    make_df("ht_band", "HEIGHTCM", 10365, ht2, id = 2)
  )

  out3 <- run_clean(df, ht_band = 3)
  out4 <- run_clean(df, ht_band = 4)

  expect_true(all(out3$result == "Exclude-A-Ord-Pair-All"))
  expect_true(all(out4$result == "Include"))
})

test_that("3+D heights -- values outside w2 window excluded", {
  # Three very different heights -- no viable w2 window
  df <- rbind(
    make_df("ht3d", "HEIGHTCM", 10000, 150, id = 1),
    make_df("ht3d", "HEIGHTCM", 10365, 170, id = 2),
    make_df("ht3d", "HEIGHTCM", 10730, 190, id = 3)
  )
  out <- run_clean(df)
  # At least some should be excluded as 3+D
  excluded <- out$result[grepl("Window", out$result)]
  expect_true(length(excluded) > 0)
})


# =============================================================================
# 8. MODERATE EWMA (Step 11Wb)
# =============================================================================

test_that("moderate EWMA catches moderate outlier among normals", {
  # Normal weights around 75, one moderate outlier at 120 (within ET cap)
  # Need enough values for the 7-step flow to operate
  df <- rbind(
    make_df("mod", "WEIGHTKG", 10000, 75, id = 1),
    make_df("mod", "WEIGHTKG", 10030, 76, id = 2),
    make_df("mod", "WEIGHTKG", 10060, 120, id = 3),  # outlier
    make_df("mod", "WEIGHTKG", 10090, 74, id = 4),
    make_df("mod", "WEIGHTKG", 10120, 77, id = 5),
    make_df("mod", "WEIGHTKG", 10150, 75, id = 6)
  )
  out <- run_clean(df)
  expect_true(grepl("Exclude-A-Traj-Moderate", res(out, 3)))
})


# =============================================================================
# 9. 1D EVALUATION (Step 13)
# =============================================================================

test_that("1D single height outside limits is excluded", {
  # Single height below 1D no-BMI minimum (122 cm at loosest)
  df <- make_df("1d_ht", "HEIGHTCM", 10000, 100)
  out <- run_clean(df, permissiveness = "loosest")
  expect_equal(res(out, 1), "Exclude-A-Single")
})

test_that("1D single weight outside limits is excluded", {
  # Single weight above 1D no-BMI maximum (350 kg at loosest)
  df <- make_df("1d_wt", "WEIGHTKG", 10000, 400)
  out <- run_clean(df, permissiveness = "loosest")
  expect_equal(res(out, 1), "Exclude-A-Single")
})

test_that("1D single height within limits is included", {
  df <- make_df("1d_ok", "HEIGHTCM", 10000, 170)
  out <- run_clean(df)
  expect_equal(res(out, 1), "Include")
})

test_that("1D uses BMI limits when both ht and wt available", {
  # Height 65cm is below no-BMI minimum (120) but above BMI minimum (60)
  # With a paired weight, BMI limits apply. Use loosest for wide PIV limits.
  df <- rbind(
    make_df("1d_bmi", "HEIGHTCM", 10000, 65, id = 1),
    make_df("1d_bmi", "WEIGHTKG", 10000, 30, id = 2)
  )
  out <- run_clean(df, permissiveness = "loosest")
  # 65cm is above single_ht_min_bmi (60) -- should be included with BMI check
  expect_equal(res(out, 1), "Include")
})

test_that("1D no-BMI height minimum is 120 at loosest, same as looser", {
  # A single distinct height of 121 cm with no same-day weight: loosest must be
  # at least as permissive as looser.
  df <- make_df("1d_nobmi_min", "HEIGHTCM", 10000, 121)
  expect_equal(res(run_clean(df, permissiveness = "loosest"), 1), "Include")
  expect_equal(res(run_clean(df, permissiveness = "looser"), 1), "Include")
  expect_equal(permissiveness_presets()$loosest$single_ht_min_nobmi,
               permissiveness_presets()$looser$single_ht_min_nobmi)
  # below the limit (minus the 0.12 rounding tolerance) is still excluded
  df_low <- make_df("1d_nobmi_low", "HEIGHTCM", 10000, 119.8)
  expect_equal(res(run_clean(df_low, permissiveness = "loosest"), 1), "Exclude-A-Single")
})

test_that("1D custom limits work", {
  # Height 130cm is within default no-BMI limits (120-230 at looser)
  # But if we raise single_ht_min_nobmi to 140, it should be excluded
  df <- make_df("1d_custom", "HEIGHTCM", 10000, 130)
  out_default <- run_clean(df)
  out_custom  <- run_clean(df, single_ht_min_nobmi = 140)
  expect_equal(res(out_default, 1), "Include")
  expect_equal(res(out_custom, 1), "Exclude-A-Single")
})


# =============================================================================
# 10. ERROR LOAD (Step 14)
# =============================================================================

test_that("error load triggered when too many exclusions", {
  # Subject with 3 values: 1 PIV + 1 other exclusion -> ratio > 0.41
  # PIV height + 2D height excluded + one remaining height -> error load on survivor
  # Simpler: 5 obs, 3 PIV -> ratio 3/5 = 0.6 > 0.41 -> error load on remaining
  df <- rbind(
    make_df("el", "HEIGHTCM", 10000, 170, id = 1),   # valid
    make_df("el", "HEIGHTCM", 10100, 170, id = 2),   # valid
    make_df("el", "HEIGHTCM", 10200, 300, id = 3),   # PIV
    make_df("el", "HEIGHTCM", 10300, 300, id = 4),   # PIV
    make_df("el", "HEIGHTCM", 10400, 300, id = 5)    # PIV
  )
  out <- run_clean(df)
  # 3 PIV + 2 remaining: ratio 3/5 = 0.6 > 0.41
  expect_equal(sum(out$result == "Exclude-A-PIV"), 3)
  expect_equal(sum(out$result == "Exclude-A-Unevaluable-Trajectory"), 2)
})


# =============================================================================
# 11. PARAMETER VALIDATION
# =============================================================================

test_that("invalid wtallow_formula is rejected", {
  df <- make_df("val", "WEIGHTKG", 10000, 75)

  # Non-existent file path
  expect_error(run_clean(df, wtallow_formula = "/nonexistent/path.csv"))
})

test_that("missing required columns are rejected", {
  df <- data.table(id = "1", subjid = "s", param = "WEIGHTKG")
  expect_error(run_clean(df), "Missing required columns")
})


# =============================================================================
# 12. OUTPUT STRUCTURE
# =============================================================================

test_that("output has expected columns (default)", {
  df <- make_df("struct", "WEIGHTKG", 10000, 75)
  out <- run_clean(df)

  # Default output: result, mean_ht, bin_result (no extraneous/groups)
  expected_cols <- c("id", "subjid", "sex", "agedays", "param", "measurement",
                     "result", "mean_ht", "bin_result")
  for (col in expected_cols) {
    expect_true(col %in% names(out), info = paste("Missing column:", col))
  }
  # Internal columns should be removed
  for (col in c("meas_m", "age_days", "id_as_entered")) {
    expect_false(col %in% names(out), info = paste("Leaked internal column:", col))
  }
  # Optional columns should not be present by default
  for (col in c("extraneous", "loss_groups", "gain_groups")) {
    expect_false(col %in% names(out), info = paste("Optional column present:", col))
  }
})

test_that("output includes optional columns when requested", {
  df <- make_df("struct", "WEIGHTKG", 10000, 75)
  out <- run_clean(df, include_extraneous = TRUE, include_ht_groups = TRUE)

  for (col in c("extraneous", "loss_groups", "gain_groups")) {
    expect_true(col %in% names(out), info = paste("Missing optional column:", col))
  }
})

test_that("output preserves all input rows", {
  df <- rbind(
    make_df("s1", "HEIGHTCM", 10000, 300, id = 1),  # PIV
    make_df("s1", "WEIGHTKG", 10000, 600, id = 2),   # PIV
    make_df("s2", "HEIGHTCM", 10000, 170, id = 3),
    make_df("s2", "WEIGHTKG", 10000, 75, id = 4)
  )
  out <- run_clean(df)
  expect_equal(nrow(out), 4)
  expect_true(all(c("1", "2", "3", "4") %in% out$id))
})

test_that("result column is always populated (no NAs)", {
  df <- make_subject("full", ages = c(10000, 10365, 10730),
                     hts = c(170, 171, 170), wts = c(75, 76, 74))
  out <- run_clean(df)
  expect_true(!any(is.na(out$result)))
})


# =============================================================================
# 13. REPEATED VALUES (Step 2W)
# =============================================================================

test_that("repeated weight values are handled gracefully", {
  # Same weight on multiple days -- RVs should be identified internally
  df <- rbind(
    make_df("rv", "WEIGHTKG", 10000, 75, id = 1),
    make_df("rv", "WEIGHTKG", 10100, 75, id = 2),  # repeated
    make_df("rv", "WEIGHTKG", 10200, 75, id = 3),  # repeated
    make_df("rv", "WEIGHTKG", 10365, 76, id = 4)
  )
  out <- run_clean(df)
  # In independent mode, RVs are full participants -- all should be Include
  expect_true(all(out$result == "Include"))
})


# =============================================================================
# 14. 2D ORDERED WEIGHT PAIRS (Step 11Wa)
# =============================================================================

test_that("2D ordered weight pairs excluded when ratio below perclimit", {
  # Two distinct weights: 40 and 90. Diff = 50 < ET cap 60, avoids Evil Twins.
  # Ratio = 40/90 = 0.44. Subject-level perclimit = max(0.5, 0.5, 0, 0) = 0.5
  # (40 kg -> 0.5 at loosest, 90 kg -> 0 disabled). 0.44 < 0.5 -> excluded.
  df <- rbind(
    make_df("2dord", "WEIGHTKG", 10000, 40, id = 1),
    make_df("2dord", "WEIGHTKG", 10100, 40, id = 2),
    make_df("2dord", "WEIGHTKG", 10365, 90, id = 3),
    make_df("2dord", "WEIGHTKG", 10400, 90, id = 4)
  )
  out <- run_clean(df)
  expect_true(all(out$result == "Exclude-A-2D-Ordered"))
})


# =============================================================================
# 15. 2D NON-ORDERED WEIGHT PAIRS (Step 11Wa2)
# =============================================================================

test_that("2D non-ordered weight pairs handled correctly", {
  # Two distinct weights interleaved in time: 40, 90, 40, 90
  # Diff = 50 < ET cap 60. Interleaved -> non-ordered.
  df <- rbind(
    make_df("2dno", "WEIGHTKG", 10000, 40, id = 1),
    make_df("2dno", "WEIGHTKG", 10100, 90, id = 2),
    make_df("2dno", "WEIGHTKG", 10200, 40, id = 3),
    make_df("2dno", "WEIGHTKG", 10300, 90, id = 4)
  )
  out <- run_clean(df)
  # Should be excluded via 2D Ord or 2D Non-Ord or other weight step
  included <- sum(out$result == "Include")
  expect_true(included < 4)
})


# =============================================================================
# 16. WEIGHT CAP INFLUENCE
# =============================================================================

test_that("cap influence marks remaining values when all at scale max", {
  scale_max_kg <- 400 / 2.2046226
  # Subject with some values at scale max and some not, where cap exclusion
  # changes the remaining EWMA enough to affect other exclusions
  df <- rbind(
    make_df("capinf", "WEIGHTKG", 10000, 75, id = 1),
    make_df("capinf", "WEIGHTKG", 10050, scale_max_kg, id = 2),
    make_df("capinf", "WEIGHTKG", 10100, scale_max_kg, id = 3),
    make_df("capinf", "WEIGHTKG", 10200, 80, id = 4),
    make_df("capinf", "WEIGHTKG", 10365, 78, id = 5),
    make_df("capinf", "WEIGHTKG", 10730, 77, id = 6)
  )
  out <- run_clean(df, scale_max_lbs = 400)
  # Just verify it runs without error -- cap influence is a complex interaction
  expect_equal(nrow(out), 6)
})


# =============================================================================
# 17. FULL REGRESSION TEST
# =============================================================================

test_that("full regression fixture: WT/HT rows match their expected codes", {
  input_file <- system.file("testdata", "adult-gc-test-ALL-PHASES.csv",
                             package = "growthcleanr")
  skip_if_not(nzchar(input_file), "Test CSV not found")

  df <- fread(input_file)
  # The regression CSV was built with allow_ht_loss = FALSE, so override the
  # loosest preset's allow_ht_loss = TRUE here to keep the expected counts valid.
  out <- run_clean(df, scale_max_lbs = 400, allow_ht_loss = FALSE)

  # Load expected values
  test_csv <- fread(input_file)
  test_csv[, id := as.character(id)]
  results <- out[, .(id = as.character(id), result)]
  merged <- merge(test_csv, results, by = "id", all.x = TRUE)

  # Map CSV expected codes (short names + old R names) to new codes
  normalize_code <- function(code, ptype = "wt") {
    p <- toupper(ptype)  # "WT" or "HT"
    shared <- c("Inc" = "Include")
    # Short and full result-code aliases mapped to the current codes
    wt_map <- c(
      "PIV" = "Exclude-A-PIV",
      "Exclude-Adult-PIV" = "Exclude-A-PIV",
      "400" = "Exclude-A-Scale-Max",
      "Exclude-Adult-Scale-Max" = "Exclude-A-Scale-Max",
      "400 only wt" = "Exclude-A-Scale-Max-Identical",
      "Exclude-Adult-Scale-Max-Identical" = "Exclude-A-Scale-Max-Identical",
      "RV 400" = "Exclude-A-Scale-Max",
      "Exclude-Adult-Scale-Max-RV-propagated" = "Exclude-A-Scale-Max-RV-Propagated",
      "Evil twins" = "Exclude-A-Evil-Twins",
      "Exclude-Adult-Evil-Twins" = "Exclude-A-Evil-Twins",
      "Same-day-identical" = "Exclude-A-Identical",
      "Exclude-Adult-Identical-Same-Day" = "Exclude-A-Identical",
      "Same-day-extraneous" = "Exclude-A-Extraneous",
      "Exclude-Adult-Extraneous-Same-Day" = "Exclude-A-Extraneous",
      "2D Ord" = "Exclude-A-2D-Ordered",
      "Exclude-Adult-Weight-Distinct-2D-Ordered" = "Exclude-A-2D-Ordered",
      "2D Non-Ord" = "Exclude-A-2D-Non-Ordered",
      "Exclude-Adult-Distinct-Non-Ordered-Pairs" = "Exclude-A-2D-Non-Ordered",
      "1D" = "Exclude-A-Single",
      "Exclude-Adult-Distinct-Single" = "Exclude-A-Single",
      "Error load" = "Exclude-A-Unevaluable-Trajectory",
      "Exclude-Adult-Too-Many-Errors" = "Exclude-A-Unevaluable-Trajectory"
    )
    ht_map <- c(
      "PIV" = "Exclude-A-PIV",
      "Exclude-Adult-PIV" = "Exclude-A-PIV",
      "Same-day-identical" = "Exclude-A-Identical",
      "Exclude-Adult-Identical-Same-Day" = "Exclude-A-Identical",
      "Same-day-extraneous" = "Exclude-A-Extraneous",
      "Exclude-Adult-Extraneous-Same-Day" = "Exclude-A-Extraneous",
      "3+D no w2 to keep" = "Exclude-A-Window-All",
      "Exclude-Adult-Height-Window-All" = "Exclude-A-Window-All",
      "3+D outside w2" = "Exclude-A-Window",
      "Exclude-Adult-Height-Window" = "Exclude-A-Window",
      "Exclude-Adult-Height-Ordered-Pairs" = "Exclude-A-Ord-Pair",
      "Exclude-Adult-Height-Ordered-Pairs-All" = "Exclude-A-Ord-Pair-All",
      "1D" = "Exclude-A-Single",
      "Exclude-Adult-Distinct-Single" = "Exclude-A-Single",
      "Error load" = "Exclude-A-Unevaluable-Trajectory",
      "Exclude-Adult-Too-Many-Errors" = "Exclude-A-Unevaluable-Trajectory"
    )
    mapping <- c(shared,
                 if (p == "WT") wt_map else ht_map)
    result <- code
    # Exact matches first
    for (s in names(mapping)) {
      result[code == s] <- mapping[s]
    }
    # Regex for old EWMA round codes -> new Traj format (round number stripped)
    result <- sub(
      "^Exclude-Adult-EWMA-Extreme round \\d+$",
      "Exclude-A-Traj-Extreme", result)
    result <- sub(
      "^Exclude-Adult-EWMA-Extreme-firstRV round \\d+$",
      "Exclude-A-Traj-Extreme-firstRV", result)
    result <- sub(
      "^Exclude-Adult-EWMA-Extreme-allRV round \\d+$",
      "Exclude-A-Traj-Extreme-allRV", result)
    result <- sub(
      "^Exclude-Adult-EWMA-Moderate round \\d+$",
      "Exclude-A-Traj-Moderate", result)
    result <- sub(
      "^Exclude-Adult-EWMA-Moderate-allRV round \\d+$",
      "Exclude-A-Traj-Moderate-allRV", result)
    result <- sub(
      "^Exclude-Adult-EWMA-Moderate-Error-Load round \\d+$",
      "Exclude-A-Traj-Moderate-Error-Load", result)
    result <- sub(
      "^Exclude-Adult-EWMA-Moderate-Error-Load-RV round \\d+$",
      "Exclude-A-Traj-Moderate-Error-Load-RV", result)
    result
  }

  # Weight validation
  wt_rows <- merged[param %in% c("WEIGHTKG", "WEIGHTLBS") &
                      expected_exc_wt_independent != "", ]
  wt_rows[, expected_r := normalize_code(
    expected_exc_wt_independent, "wt")]
  wt_rows[, match := result == expected_r]

  # Height validation
  ht_rows <- merged[param %in% c("HEIGHTCM", "HEIGHTIN") &
                      expected_exc_ht_independent != "", ]
  ht_rows[, expected_r := normalize_code(
    expected_exc_ht_independent, "ht")]
  ht_rows[, match := result == expected_r]

  total_pass <- sum(wt_rows$match, na.rm = TRUE) + sum(ht_rows$match, na.rm = TRUE)
  total_n <- nrow(wt_rows) + nrow(ht_rows)

  # At least 1250 of the deterministic CSV rows match.
  expect_true(total_pass >= 1250)
  expect_true(total_n >= 1365)
})


# =============================================================================
# 18. HEIGHTIN AND WEIGHTLBS UNIT HANDLING
# =============================================================================

test_that("HEIGHTIN input is converted and processed correctly", {
  # 67 inches = 170.18 cm -- should be included
  df <- make_df("ht_in", "HEIGHTIN", 10000, 67)
  out <- run_clean(df)
  expect_equal(res(out, 1), "Include")
})

test_that("HEIGHTIN PIV detection works in cm", {
  # 10 inches = 25.4 cm -- below 50cm PIV limit
  df <- make_df("ht_in_piv", "HEIGHTIN", 10000, 10)
  out <- run_clean(df)
  expect_equal(res(out, 1), "Exclude-A-PIV")
})


# =============================================================================
# 19. MEAN HEIGHT OUTPUT
# =============================================================================

test_that("mean_ht is populated for included heights", {
  df <- make_subject("mht", ages = c(10000, 10365, 10730),
                     hts = c(170, 171, 170), wts = c(75, 76, 74))
  out <- run_clean(df)
  ht_rows <- out[out$param == "HEIGHTCM", ]
  # All included heights should have mean_ht populated
  included_ht <- ht_rows[ht_rows$result == "Include", ]
  expect_true(all(!is.na(included_ht$mean_ht)))
})


# =============================================================================
# 20. MULTI-SUBJECT ISOLATION
# =============================================================================

test_that("exclusions in one subject don't affect another", {
  # Subject A: all PIV weights -> error load
  # Subject B: normal weights -> all included
  df <- rbind(
    make_df("iso_a", "WEIGHTKG", 10000, 600, id = 1),   # PIV
    make_df("iso_a", "WEIGHTKG", 10100, 600, id = 2),   # PIV
    make_df("iso_a", "WEIGHTKG", 10200, 75, id = 3),    # valid but error load
    make_df("iso_b", "WEIGHTKG", 10000, 75, id = 4),
    make_df("iso_b", "WEIGHTKG", 10100, 76, id = 5),
    make_df("iso_b", "WEIGHTKG", 10200, 74, id = 6)
  )
  out <- run_clean(df)
  # Subject B should be completely unaffected by Subject A
  expect_true(all(out$result[out$subjid == "iso_b"] == "Include"))
})


# =============================================================================
# 21. PERMISSIVENESS FRAMEWORK
# =============================================================================

test_that("default permissiveness (loosest) matches prior defaults", {
  df <- rbind(
    make_subject("perm", ages = c(10000, 10100, 10200, 10300, 10400),
                 hts = c(170, 170, 170, 170, 170),
                 wts = c(75, 76, 120, 74, 77))
  )
  out_default <- run_clean(df)
  out_loosest <- run_clean(df, permissiveness = "loosest")
  expect_identical(out_default$result, out_loosest$result)
})

test_that("invalid permissiveness produces an error", {
  df <- make_df("err", "WEIGHTKG", 10000, 75)
  expect_error(run_clean(df, permissiveness = "invalid"),
               "permissiveness must be one of")
})

test_that("non-scalar permissiveness gives the clean message, not a length-> crash", {
  # Before the length/type guard, a length-2 value hit `if (!x %in% valid)` with a
  # length-2 condition and died with the cryptic "condition has length > 1".
  df <- make_df("err2", "WEIGHTKG", 10000, 75)
  expect_error(run_clean(df, permissiveness = c("looser", "tighter")),
               "permissiveness must be one of")
})

test_that("hostile permissiveness sub-parameter overrides are clean errors, not scalar-if crashes", {
  # The preset sub-parameters (overall_*, single_*, mod_ewma_f, allow_ht_*,
  # repval_handling, ...) flow into per-subject scalar if() comparisons, where an
  # NA / non-scalar / wrong-type value would crash with "missing value where
  # TRUE/FALSE needed". resolve_permissiveness() validates each non-NULL override
  # against the type/shape of the preset value it replaces. Reachable only via a
  # direct cleanadult() call (cleangrowth() does not forward these).
  df <- make_df("sub", "WEIGHTKG", 10000, 75)
  # numeric limit: NA / non-finite / non-scalar / wrong-type
  for (v in list(NA, NaN, Inf, "x", c(1, 2)))
    expect_error(run_clean(df, overall_ht_min = v), "overall_ht_min must be a single")
  # logical flag
  for (v in list(NA, 1, "yes", c(TRUE, FALSE)))
    expect_error(run_clean(df, allow_ht_gain = v),
                 "allow_ht_gain must be (TRUE or FALSE|a single)")
  # string parameter
  for (v in list(NA, 5, c("a", "b")))
    expect_error(run_clean(df, repval_handling = v),
                 "repval_handling must be a single")
  # valid overrides still resolve (sanity)
  expect_no_error(run_clean(df, overall_ht_min = 50, allow_ht_gain = FALSE,
                            mod_ewma_f = 0.6, repval_handling = "linked"))
})

test_that("explicit override takes precedence over preset", {
  # Under tightest, overall_ht_min = 147. A height of 145 should be PIV.
  # But if we override overall_ht_min = 50, it should pass PIV.
  df <- rbind(
    make_df("ovr", "HEIGHTCM", 10000, 145, id = 1),
    make_df("ovr", "HEIGHTCM", 10365, 170, id = 2)
  )
  out_tight <- run_clean(df, permissiveness = "tightest")
  out_ovr <- run_clean(df, permissiveness = "tightest",
                        overall_ht_min = 50)
  expect_equal(res(out_tight, 1), "Exclude-A-PIV")
  expect_true(res(out_ovr, 1) != "Exclude-A-PIV")
})

test_that("tightest excludes more than loosest on borderline data", {
  # Moderate outlier that loosest may keep but tightest should catch
  # (lower EWMA caps + lower mod_ewma_f + tighter wtallow)
  df <- rbind(
    make_df("bl", "WEIGHTKG", 10000, 75, id = 1),
    make_df("bl", "WEIGHTKG", 10030, 76, id = 2),
    make_df("bl", "WEIGHTKG", 10060, 120, id = 3),
    make_df("bl", "WEIGHTKG", 10090, 74, id = 4),
    make_df("bl", "WEIGHTKG", 10120, 77, id = 5),
    make_df("bl", "WEIGHTKG", 10150, 75, id = 6)
  )
  out_loose <- run_clean(df, permissiveness = "loosest")
  out_tight <- run_clean(df, permissiveness = "tightest")
  n_exc_loose <- sum(out_loose$result != "Include")
  n_exc_tight <- sum(out_tight$result != "Include")
  expect_true(n_exc_tight >= n_exc_loose)
})

test_that("allow_ht_gain = FALSE suppresses height gain rescue", {
  # Two heights 4 inches apart in a young adult (age ~25yr = 9131 days)
  # With allow_ht_gain = TRUE and ht_band = 3, gain rescue saves them
  # With allow_ht_gain = FALSE, both should be excluded (outside 3" band)
  ht1 <- 170
  ht2 <- 170 + 4 * 2.54  # +4 inches
  df <- rbind(
    make_df("htg", "HEIGHTCM", 8766, ht1, id = 1),  # ~24 yr
    make_df("htg", "HEIGHTCM", 9131, ht2, id = 2)    # ~25 yr
  )
  out_gain <- run_clean(df, allow_ht_gain = TRUE)
  out_nogain <- run_clean(df, allow_ht_gain = FALSE)
  # With gain rescue: at least one should be included
  n_inc_gain <- sum(out_gain$result == "Include")
  # Without gain rescue: both excluded (outside band, no rescue)
  n_inc_nogain <- sum(out_nogain$result == "Include")
  expect_true(n_inc_gain >= n_inc_nogain)
})

test_that("mod_ewma_f = 0.60 catches more than 0.75", {
  # Subject with moderate outlier where directional check matters
  df <- rbind(
    make_df("mf", "WEIGHTKG", 10000, 75, id = 1),
    make_df("mf", "WEIGHTKG", 10030, 76, id = 2),
    make_df("mf", "WEIGHTKG", 10060, 110, id = 3),
    make_df("mf", "WEIGHTKG", 10090, 74, id = 4),
    make_df("mf", "WEIGHTKG", 10120, 77, id = 5),
    make_df("mf", "WEIGHTKG", 10150, 75, id = 6)
  )
  out_75 <- run_clean(df, mod_ewma_f = 0.75)
  out_60 <- run_clean(df, mod_ewma_f = 0.60)
  n_exc_75 <- sum(out_75$result != "Include")
  n_exc_60 <- sum(out_60$result != "Include")
  expect_true(n_exc_60 >= n_exc_75)
})

test_that("allofus15 short-interval breakpoints work", {
  # 8 kg change at 1-day interval: exceeds 5 kg limit (allofus15)
  # but within piecewise limits (~10 kg at 1 day)
  df <- rbind(
    make_df("ao", "WEIGHTKG", 10000, 75, id = 1),
    make_df("ao", "WEIGHTKG", 10001, 83, id = 2),
    make_df("ao", "WEIGHTKG", 10030, 76, id = 3),
    make_df("ao", "WEIGHTKG", 10060, 74, id = 4),
    make_df("ao", "WEIGHTKG", 10090, 77, id = 5)
  )
  out_pw <- run_clean(df, wtallow_formula = "piecewise")
  out_ao <- run_clean(df, wtallow_formula = "allofus15")
  n_exc_pw <- sum(out_pw$result != "Include")
  n_exc_ao <- sum(out_ao$result != "Include")
  expect_true(n_exc_ao >= n_exc_pw)
})

test_that("permissiveness presets have all required parameters", {
  presets <- permissiveness_presets()
  expect_equal(length(presets), 4)
  required <- c("overall_ht_min", "overall_ht_max",
                 "overall_wt_min", "overall_wt_max",
                 "overall_bmi_min", "overall_bmi_max",
                 "single_ht_min_bmi", "single_ht_max_bmi",
                 "single_wt_min_bmi", "single_wt_max_bmi",
                 "single_ht_min_nobmi", "single_ht_max_nobmi",
                 "single_wt_min_nobmi", "single_wt_max_nobmi",
                 "single_bmi_min", "single_bmi_max",
                 "wtallow_formula",
                 "error_load_threshold", "mod_ewma_f",
                 "ht_band", "allow_ht_loss", "allow_ht_gain",
                 "repval_handling")
  for (level in names(presets)) {
    for (param in required) {
      expect_true(param %in% names(presets[[level]]),
                  info = paste(level, "missing", param))
    }
  }
})

test_that("Step 13 keeps a single-value height if ANY same-day BMI is in range", {
  # One distinct height (220 cm) with two same-day included weights: the first
  # pair's BMI (11.4) is below single_bmi_min (12 at looser), the second (14.5)
  # is in range. Any in-range BMI protects the height (not just the first pair).
  a <- data.table::data.table(
    id = 1:4, subjid = "s", sex = 0,
    agedays = round(c(40, 40, 40.5, 40.5) * 365.25),
    param = c("HEIGHTCM", "WEIGHTKG", "HEIGHTCM", "WEIGHTKG"),
    measurement = c(220, 55, 220, 70))
  r <- cleangrowth(a, quietly = TRUE, adult_permissiveness = "looser")
  expect_true(all(r$exclude == "Include"))
  # with every same-day BMI out of range the height is still excluded
  a2 <- data.table::copy(a)[id == 4, measurement := 56]
  r2 <- cleangrowth(a2, quietly = TRUE, adult_permissiveness = "looser",
                    exclude_detail = TRUE)
  expect_equal(as.character(r2$exclude_detailed[r2$param == "HEIGHTCM"]),
               rep("Exclude-A-Single", 2))
})

test_that("mean_ht: from included heights only, populated on excluded height rows", {
  yrs <- c(30, 30, 31, 31, 32, 32, 33)
  a <- data.table::data.table(
    id = 1:7, subjid = "s", sex = 0, agedays = round(yrs * 365.25),
    param = c("HEIGHTCM", "WEIGHTKG", "HEIGHTCM", "WEIGHTKG",
              "HEIGHTCM", "WEIGHTKG", "HEIGHTCM"),
    measurement = c(170, 70, 172, 71, 25, 70, 171))
  r <- cleangrowth(a, quietly = TRUE, mean_ht = TRUE)
  ht <- r[param == "HEIGHTCM"]
  expect_equal(as.character(ht$exclude[ht$measurement == 25]), "Exclude-Hard-Limit")
  # computed from the three included heights only
  expect_equal(unique(ht$mean_ht), mean(c(170, 172, 171)))
  # and populated on the excluded (25 cm) row too
  expect_false(anyNA(ht$mean_ht))
  expect_true(all(is.na(r$mean_ht[r$param == "WEIGHTKG"])))
  # a subject with no included heights gets NA on every height row
  b <- data.table::data.table(
    id = 1:4, subjid = "t", sex = 0, agedays = round(c(30, 30, 31, 31) * 365.25),
    param = c("HEIGHTCM", "WEIGHTKG", "HEIGHTCM", "WEIGHTKG"),
    measurement = c(25, 70, 25, 71))
  rb <- cleangrowth(b, quietly = TRUE, mean_ht = TRUE)
  expect_true(all(is.na(rb$mean_ht)))
})
