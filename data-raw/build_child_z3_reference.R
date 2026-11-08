# build_child_z3_reference.R
# Builds the child zn3/zp3 lookup table for unit_error_range.
#
# For each (agedays, sex, param) combination, computes the measurement values
# at recentered CSD z = -3 and z = +3, faithfully following gc's z-score
# pipeline: WHO <2y, CDC >5y, blended 2-5y (HT/WT), WHO-only for HC,
# then recentered by subtracting rcmedian.sd (the recentering median).
#
# Output: long-format CSV with columns (agedays, sex, param, zn3, zp3)
# matching the recentering file convention.
#
# Usage:
#   Rscript data-raw/build_child_z3_reference.R
#   (run from the package root)
#
# Dependencies: data.table
# Reads: inst/extdata/growthfile_who.csv.gz
#         inst/extdata/growthfile_cdc_ext_infants.csv.gz
#         inst/extdata/rcfile-resmoothed.csv.gz
#
# Daymont 2026-05-22

library(data.table)

# ---- 1. Load reference files ----

pkg_dir <- if (file.exists("inst/extdata")) "." else
  stop("Run from the package root directory")

who <- fread(file.path(pkg_dir, "inst/extdata/growthfile_who.csv.gz"))
cdc <- fread(file.path(pkg_dir, "inst/extdata/growthfile_cdc_ext_infants.csv.gz"))
rc  <- fread(file.path(pkg_dir, "inst/extdata/rcfile-resmoothed.csv.gz"))

# ---- 2. Reshape references to long format ----
# Extract (agedays, sex, param, m, csdpos, csdneg) for each source.

reshape_ref <- function(dt, src_prefix, params) {
  # params: named list, e.g. list(WEIGHTKG = "wt", HEIGHTCM = "ht", HEADCM = "hc")
  out <- rbindlist(lapply(names(params), function(pname) {
    abbr <- params[[pname]]
    m_col      <- paste0(src_prefix, "_", abbr, "_m")
    csdpos_col <- paste0(src_prefix, "_", abbr, "_csd_pos")
    csdneg_col <- paste0(src_prefix, "_", abbr, "_csd_neg")
    # Skip if columns don't exist (e.g. CDC HC may be absent at some ages)
    if (!all(c(m_col, csdpos_col, csdneg_col) %in% names(dt))) return(NULL)
    data.table(
      agedays = dt$agedays,
      sex     = dt$sex,
      param   = pname,
      m       = dt[[m_col]],
      csdpos  = dt[[csdpos_col]],
      csdneg  = dt[[csdneg_col]]
    )
  }))
  out
}

param_map <- list(WEIGHTKG = "wt", HEIGHTCM = "ht", HEADCM = "hc")

who_long <- reshape_ref(who, "who", param_map)
cdc_long <- reshape_ref(cdc, "cdc", param_map)

setkey(who_long, agedays, sex, param)
setkey(cdc_long, agedays, sex, param)

# ---- 3. Helper: invert CSD z-score to measurement ----
# Given target z-score and reference (m, csdpos, csdneg), return measurement.
# CSD forward: z = (x - m) / csdneg if x < m; z = (x - m) / csdpos if x >= m
# CSD inverse: x = m + z * csdneg if z < 0; x = m + z * csdpos if z >= 0

csd_inverse <- function(target_z, m, csdpos, csdneg) {
  ifelse(target_z < 0,
         m + target_z * csdneg,
         m + target_z * csdpos)
}

# Forward CSD: measurement -> z
csd_forward <- function(x, m, csdpos, csdneg) {
  ifelse(x < m,
         (x - m) / csdneg,
         (x - m) / csdpos)
}

# ---- 4. Build the lookup grid ----
# Age ranges by param:
#   HC:    0 to 1826 (WHO only; HC is cleaned under 5 years, and the WHO HC
#          reference ends at 1826 days)
#   HT/WT: 0 to 7304 (matching recentering file max)

# Get the actual agedays present in each reference
who_ages <- sort(unique(who_long$agedays))  # 0..1826
cdc_ages <- sort(unique(cdc_long$agedays))  # 0..7671
rc_ages  <- sort(unique(rc$agedays))        # 0..7304

# Max age for each param. HC: the end of the WHO HC reference (1826 days),
# which covers the algorithm's HC cleaning range (agedays < 5 * 365.25).
max_age <- list(WEIGHTKG = max(rc_ages), HEIGHTCM = max(rc_ages),
                HEADCM = max(who_ages))

# Build the grid
grid <- rbindlist(lapply(names(max_age), function(p) {
  ages <- rc_ages[rc_ages <= max_age[[p]]]
  CJ(agedays = ages, sex = c(0L, 1L), param = p)
}))

# Merge recentering medians
setkey(rc, agedays, sex, param)
setkey(grid, agedays, sex, param)
grid <- rc[grid]

# Check for missing recentering values
if (!"rcmedian.sd" %in% names(grid)) stop("recentering file has no rcmedian.sd column")
if (any(is.na(grid$rcmedian.sd))) {
  n_miss <- sum(is.na(grid$rcmedian.sd))
  stop(sprintf("%d rows missing rcmedian.sd -- check recentering coverage", n_miss))
}

# ---- 5. Compute zn3 and zp3 (measurement at recentered z = -3 and +3) ----
# target raw z = +/-3 + rcmedian.sd (to account for recentering)

grid[, target_zn := -3 + rcmedian.sd]
grid[, target_zp :=  3 + rcmedian.sd]

# Classify each row into blending regime
grid[, ageyears := agedays / 365.25]
grid[, regime := fifelse(
  param == "HEADCM", "who",
  fifelse(ageyears < 2, "who",
          fifelse(ageyears > 5, "cdc", "blend"))
)]

# Merge WHO and CDC reference parameters
grid <- merge(grid, who_long, by = c("agedays", "sex", "param"), all.x = TRUE,
              suffixes = c("", "_who"))
setnames(grid, c("m", "csdpos", "csdneg"), c("who_m", "who_csdpos", "who_csdneg"))

grid <- merge(grid, cdc_long, by = c("agedays", "sex", "param"), all.x = TRUE)
setnames(grid, c("m", "csdpos", "csdneg"), c("cdc_m", "cdc_csdpos", "cdc_csdneg"))

# --- 5a. Pure WHO or CDC: direct inversion ---

grid[regime == "who", zn3 := csd_inverse(target_zn, who_m, who_csdpos, who_csdneg)]
grid[regime == "who", zp3 := csd_inverse(target_zp, who_m, who_csdpos, who_csdneg)]
grid[regime == "cdc", zn3 := csd_inverse(target_zn, cdc_m, cdc_csdpos, cdc_csdneg)]
grid[regime == "cdc", zp3 := csd_inverse(target_zp, cdc_m, cdc_csdpos, cdc_csdneg)]

# --- 5b. Blended region (2-5y, HT/WT only): numerical inversion ---
# blended_z(x) = (cdc_z(x) * (ageyears - 2) + who_z(x) * (5 - ageyears)) / 3
# Solve blended_z(x) = target using uniroot.

blend_rows <- which(grid$regime == "blend")

if (length(blend_rows) > 0) {
  message(sprintf("Solving %d blended-region rows via uniroot...", length(blend_rows)))

  # Pre-extract columns for speed (avoid repeated data.table access in loop)
  b_ageyears   <- grid$ageyears[blend_rows]
  b_who_m      <- grid$who_m[blend_rows]
  b_who_csdpos <- grid$who_csdpos[blend_rows]
  b_who_csdneg <- grid$who_csdneg[blend_rows]
  b_cdc_m      <- grid$cdc_m[blend_rows]
  b_cdc_csdpos <- grid$cdc_csdpos[blend_rows]
  b_cdc_csdneg <- grid$cdc_csdneg[blend_rows]
  b_target_zn  <- grid$target_zn[blend_rows]
  b_target_zp  <- grid$target_zp[blend_rows]

  zn3_vals <- numeric(length(blend_rows))
  zp3_vals <- numeric(length(blend_rows))

  for (i in seq_along(blend_rows)) {
    ay   <- b_ageyears[i]
    w_cdc <- ay - 2
    w_who <- 5 - ay

    blended_z_fn <- function(x) {
      z_who <- csd_forward(x, b_who_m[i], b_who_csdpos[i], b_who_csdneg[i])
      z_cdc <- csd_forward(x, b_cdc_m[i], b_cdc_csdpos[i], b_cdc_csdneg[i])
      (z_cdc * w_cdc + z_who * w_who) / 3
    }

    # Search interval: use the wider of WHO and CDC +/-3 inversions as bounds,
    # with generous padding
    who_lo <- csd_inverse(-10, b_who_m[i], b_who_csdpos[i], b_who_csdneg[i])
    who_hi <- csd_inverse(10,  b_who_m[i], b_who_csdpos[i], b_who_csdneg[i])
    cdc_lo <- csd_inverse(-10, b_cdc_m[i], b_cdc_csdpos[i], b_cdc_csdneg[i])
    cdc_hi <- csd_inverse(10,  b_cdc_m[i], b_cdc_csdpos[i], b_cdc_csdneg[i])
    lo <- min(who_lo, cdc_lo, na.rm = TRUE)
    hi <- max(who_hi, cdc_hi, na.rm = TRUE)

    # zn3
    tryCatch({
      res <- uniroot(function(x) blended_z_fn(x) - b_target_zn[i],
                      lower = lo, upper = hi, tol = 1e-6)
      zn3_vals[i] <- res$root
    }, error = function(e) {
      zn3_vals[i] <<- NA_real_
      warning(sprintf("uniroot failed for zn3 at row %d (agedays=%d, sex=%d, param=%s): %s",
                      blend_rows[i], grid$agedays[blend_rows[i]],
                      grid$sex[blend_rows[i]], grid$param[blend_rows[i]], e$message))
    })

    # zp3
    tryCatch({
      res <- uniroot(function(x) blended_z_fn(x) - b_target_zp[i],
                      lower = lo, upper = hi, tol = 1e-6)
      zp3_vals[i] <- res$root
    }, error = function(e) {
      zp3_vals[i] <<- NA_real_
      warning(sprintf("uniroot failed for zp3 at row %d (agedays=%d, sex=%d, param=%s): %s",
                      blend_rows[i], grid$agedays[blend_rows[i]],
                      grid$sex[blend_rows[i]], grid$param[blend_rows[i]], e$message))
    })
  }

  grid[blend_rows, zn3 := zn3_vals]
  grid[blend_rows, zp3 := zp3_vals]
  message("Blended-region solve complete.")
}

# ---- 6. Sanity checks ----

# zn3 should always be < zp3
bad <- grid[!is.na(zn3) & !is.na(zp3) & zn3 >= zp3]
if (nrow(bad) > 0) {
  warning(sprintf("%d rows where zn3 >= zp3 -- investigate!", nrow(bad)))
  print(bad[, .(agedays, sex, param, zn3, zp3, rcmedian.sd)])
}

# All values should be positive (anthropometric measurements)
neg <- grid[(!is.na(zn3) & zn3 <= 0) | (!is.na(zp3) & zp3 <= 0)]
if (nrow(neg) > 0) {
  warning(sprintf("%d rows with non-positive zn3 or zp3 -- investigate!", nrow(neg)))
  print(neg[, .(agedays, sex, param, zn3, zp3, rcmedian.sd, target_zn)])
}

# No NAs in the output (except possibly at boundary ages)
na_count <- sum(is.na(grid$zn3)) + sum(is.na(grid$zp3))
if (na_count > 0) {
  warning(sprintf("%d NA values in zn3/zp3 -- check reference coverage", na_count))
}

# Quick spot checks: print a few rows
message("\n--- Spot checks ---")
message("Birth (agedays=0), male, WEIGHTKG:")
print(grid[agedays == 0 & sex == 0 & param == "WEIGHTKG",
           .(agedays, sex, param, rcmedian.sd, zn3, zp3)])
message("Age 3y (agedays=1096), female, HEIGHTCM (blended region):")
print(grid[agedays == 1096 & sex == 1 & param == "HEIGHTCM",
           .(agedays, sex, param, rcmedian.sd, regime, zn3, zp3)])
message("Age 10y (agedays=3653), male, WEIGHTKG (CDC only):")
print(grid[agedays == 3653 & sex == 0 & param == "WEIGHTKG",
           .(agedays, sex, param, rcmedian.sd, regime, zn3, zp3)])
message("Age 2y (agedays=730), male, HEADCM (WHO only):")
print(grid[agedays == 730 & sex == 0 & param == "HEADCM",
           .(agedays, sex, param, rcmedian.sd, regime, zn3, zp3)])

# ---- 7. Output ----

# Keep only the output columns, matching recentering file convention
out <- grid[, .(agedays, sex, param, zn3, zp3)]
setkey(out, agedays, sex, param)

# Round to 5 decimal places (well below measurement precision for all params).
# zn3/zp3 are the recentered UNCORRECTED (tbc.sd) z = -3/+3 measurements only;
# no GA-corrected (ctbc.sd) variant is produced -- unit_error_range uses the
# population reference, not patient-specific GA correction.
out[, zn3 := round(zn3, 5)]
out[, zp3 := round(zp3, 5)]

outfile <- file.path(pkg_dir, "inst/extdata/child_z3_reference.csv.gz")
fwrite(out, outfile)
message(sprintf("\nWrote %d rows to %s", nrow(out), outfile))
message(sprintf("  Params: %s", paste(unique(out$param), collapse = ", ")))
message(sprintf("  Age range: %d-%d days", min(out$agedays), max(out$agedays)))
message(sprintf("  Sexes: %s", paste(sort(unique(out$sex)), collapse = ", ")))
