# build_child_et_fixture.R
# Builds a deterministic Child Evil Twins (Step 9) regression fixture.
#
# Each subject is a single-parameter, TERM trajectory (so ctbc.sd == tbc.sd)
# whose measurements are generated to hit chosen RECENTERED z-scores (tbc.sd)
# at young ages in the pure-WHO region (agedays < 730, and <= 3y for HC), so the
# CSD inversion is direct (no WHO/CDC blend) and well clear of PIV limits.
#
# Patterns exercise the anchor rule:
#   - interior step  [0,0,6,6]   -> caught (inner jump flanked by stable A and D)
#   - single end spike [0,0,0,6] -> ceded to EWMA (no D after the spike)
#   - embedded single  [0,6,0,0] -> ceded to EWMA (one outer anchor fails)
#   - transient plateau [0,6,6,0]-> ceded (no inner jump; not handled by Evil Twins by design)
#   - 3 eligible only            -> gated out (< 4 eligible)
#   - sustained 3-plateau        -> two ET exclusions across cascade iterations
#
# HT uses the LOW direction (z = -6) to stay clear of the ht-high PIV cutoff (8);
# WT and HC use the HIGH direction (PIV high 22 / 15, ample margin).
#
# The `expected_et` column is the SPEC-DERIVED prediction of which rows the ET
# step marks Exclude-C-Evil-Twins. It was confirmed against cleangrowth() output
# and frozen. Rows not marked ET may still be excluded by later (EWMA)
# steps; the companion test asserts ET membership only.
#
# Usage:  Rscript data-raw/build_child_et_fixture.R   (run from the package root)
# Output: inst/testdata/child-et-fixture.csv
#
# Daymont 2026-05-22

library(data.table)

pkg_dir <- if (file.exists("inst/extdata")) "." else
  stop("Run from the package root")

who <- fread(file.path(pkg_dir, "inst/extdata/growthfile_who.csv.gz"))
rc  <- fread(file.path(pkg_dir, "inst/extdata/rcfile-resmoothed.csv.gz"))
setkey(rc, param, sex, agedays)

# WHO long: (agedays, sex, param, m, csdpos, csdneg)
param_map <- list(WEIGHTKG = "wt", HEIGHTCM = "ht", HEADCM = "hc")
who_long <- rbindlist(lapply(names(param_map), function(p) {
  a <- param_map[[p]]
  data.table(agedays = who$agedays, sex = who$sex, param = p,
             m = who[[paste0("who_", a, "_m")]],
             csdpos = who[[paste0("who_", a, "_csd_pos")]],
             csdneg = who[[paste0("who_", a, "_csd_neg")]])
}))
setkey(who_long, param, sex, agedays)

# Recentered-z -> measurement (pure WHO region). raw z = tbc + sd.median.
# Order-preserving merge (avoids DT[.()] join surprises).
to_meas <- function(param, sex, agedays, tbc_target) {
  q <- data.table(param = as.character(param), sex = as.integer(sex),
                  agedays = as.integer(agedays), tbc = tbc_target,
                  .ord = seq_along(agedays))
  q <- merge(q, who_long, by = c("param", "sex", "agedays"),
             all.x = TRUE, sort = FALSE)
  q <- merge(q, rc[, .(param, sex, agedays, sd.median)],
             by = c("param", "sex", "agedays"), all.x = TRUE, sort = FALSE)
  setorder(q, .ord)
  raw <- q$tbc + q$sd.median
  round(ifelse(raw < 0, q$m + raw * q$csdneg, q$m + raw * q$csdpos), 2)
}

# ---- Subject/pattern definitions (sex = 0 male throughout) ----
# Each: list(subjid, param, ages, tbc, expected_et logical per row, pattern, note)
defs <- list(
  list(subjid = "et_wt_step",   param = "WEIGHTKG", ages = c(120,240,360,480),
       tbc = c(0,0,6,6),      et = c(F,F,T,F), pattern = "interior_step",
       note = "step up that stays up; ET excludes one high inner value, EWMA gets the rest"),
  list(subjid = "et_wt_endspk", param = "WEIGHTKG", ages = c(120,240,360,480),
       tbc = c(0,0,0,6),      et = c(F,F,F,F), pattern = "single_end_spike",
       note = "no value after the spike -> no D anchor -> ceded to EWMA"),
  list(subjid = "et_ht_embed",  param = "HEIGHTCM", ages = c(120,240,360,480,600),
       tbc = c(0,-6,0,0,0),   et = c(F,F,F,F,F), pattern = "embedded_single",
       note = "single interior spike (low); one outer anchor fails -> ceded to EWMA"),
  list(subjid = "et_ht_plateau",param = "HEIGHTCM", ages = c(120,240,360,480),
       tbc = c(0,-6,-6,0),    et = c(F,F,F,F), pattern = "transient_plateau",
       note = "flat plateau flanked by normals; no inner jump -> deferred flat-pattern, ceded"),
  list(subjid = "et_hc_step",   param = "HEADCM",   ages = c(120,240,360,480),
       tbc = c(0,0,6,6),      et = c(F,F,T,F), pattern = "interior_step_hc",
       note = "HC step (<=3y); same as WT step"),
  list(subjid = "et_wt_gate3",  param = "WEIGHTKG", ages = c(120,240,360),
       tbc = c(0,6,6),        et = c(F,F,F), pattern = "gate_3_eligible",
       note = "only 3 eligible -> ET count gate (>=4) blocks evaluation entirely"),
  list(subjid = "et_wt_plat3",  param = "WEIGHTKG", ages = c(120,240,360,480,600),
       tbc = c(0,0,6,6,6),    et = c(F,F,T,T,F), pattern = "sustained_3_plateau",
       note = "3-long high plateau; cascade peels two edges as ET, last high left to EWMA")
)

rows <- rbindlist(lapply(defs, function(d) {
  data.table(
    subjid = d$subjid, param = d$param, agedays = as.integer(d$ages), sex = 0L,
    measurement = to_meas(d$param, 0L, as.integer(d$ages), d$tbc),
    target_tbc = d$tbc,
    expected_et = ifelse(d$et, "Exclude-C-Evil-Twins", "(not Evil-Twins)"),
    pattern = d$pattern, note = d$note
  )
}))
rows[, id := .I]
setcolorder(rows, c("id","subjid","param","agedays","sex","measurement",
                    "target_tbc","expected_et","pattern","note"))

outfile <- file.path(pkg_dir, "inst/testdata/child-et-fixture.csv")
fwrite(rows, outfile)
cat(sprintf("Wrote %d rows (%d subjects) to %s\n",
            nrow(rows), uniqueN(rows$subjid), outfile))
print(rows[, .(id, subjid, param, agedays, measurement, target_tbc, expected_et)])
