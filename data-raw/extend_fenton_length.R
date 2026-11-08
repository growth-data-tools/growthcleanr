# Extend the approximated Fenton CSD values for length below their lower boundary.
#
# Background: in fenton2025_ms_lookup_smoothed.csv the weight and headcirc
# curves start at ga_days = 157 (22+3 weeks), but the length curve starts at
# ga_days = 164 (23+3 weeks). The weight->gestational-age lookup
# (fent_foraga, after the 500 g floor) can produce GA estimates as low as
# 157 days. So for the most premature infants, the earliest LENGTH row has a
# post-menstrual age in [157, 163] that falls below the length reference and
# therefore receives no GA correction (sd.corr falls back to the uncorrected
# z), while weight/HC at the same age are corrected. That leaves a single
# uncorrected, extreme-negative length z sitting in an otherwise-corrected
# trajectory.
#
# Fix: linearly extrapolate the length M, S_upper, S_lower below the 23+3
# boundary down to 22+3 (ga_days 157), matching weight/HC. The slope is taken
# over the lowest one-week span available in the table (23+3 -> 24+3, i.e.
# ga_days 164 -> 171); the low end of the length curve is essentially linear,
# so this is a faithful continuation. For a target ga_days g (157..163):
#   value(g) = value(164) - (164 - g) * (value(171) - value(164)) / 7
# applied independently to M, S_upper, and S_lower, per sex.
#
# Idempotent: if any length row below ga_days 164 already exists, do nothing.
# Appends the new rows to the CSV (existing rows untouched) for a minimal diff.

suppressMessages(library(data.table))

csv_path <- file.path("inst", "extdata", "fenton2025_ms_lookup_smoothed.csv")
stopifnot(file.exists(csv_path))
fr <- fread(csv_path)

if (nrow(fr[param == "length" & ga_days < 164]) > 0) {
  message("Length rows below ga_days 164 already present; nothing to do.")
} else {
  new_rows <- rbindlist(lapply(c(0L, 1L), function(sx) {
    lo <- fr[param == "length" & sex == sx & ga_days == 164]
    hi <- fr[param == "length" & sex == sx & ga_days == 171]
    stopifnot(nrow(lo) == 1L, nrow(hi) == 1L)
    g  <- 157:163
    slope <- function(col) (hi[[col]] - lo[[col]]) / 7
    data.table(
      sex      = sx,
      param    = "length",
      ga_days  = g,
      ga_weeks = round(g / 7, 4),
      agedays  = as.integer(g - 280),
      M        = round(lo$M       - (164 - g) * slope("M"),       4),
      S_upper  = round(lo$S_upper - (164 - g) * slope("S_upper"), 6),
      S_lower  = round(lo$S_lower - (164 - g) * slope("S_lower"), 6)
    )
  }))

  # Append only the new rows (existing rows untouched -> minimal diff).
  fwrite(new_rows, csv_path, append = TRUE, col.names = FALSE)
  message(sprintf("Appended %d extrapolated length rows (ga_days 157-163, both sexes).", nrow(new_rows)))
  print(new_rows)
}
