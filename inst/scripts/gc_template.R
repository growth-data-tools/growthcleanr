#!/usr/bin/env Rscript

# =============================================================================
# growthcleanr -- template script
# =============================================================================
#
# WHAT THIS DOES
#   Reads a CSV of growth measurements, runs growthcleanr on it, and writes a
#   new CSV that is your original data plus growthcleanr's results.
#
# WHO THIS IS FOR
#   People who want to run growthcleanr without learning R. You install R once,
#   change two file names below, and run this file from a command line. You do
#   not need to open R or write any R code.
#
# -----------------------------------------------------------------------------
# ONE-TIME SETUP (do this once, then never again)
# -----------------------------------------------------------------------------
#   1. Install R (free): https://cran.r-project.org/
#   2. Install growthcleanr. Open a command line / terminal and run:
#
#        Rscript -e "install.packages('growthcleanr')"
#
#      (You can also do this inside R with install.packages("growthcleanr").)
#
# -----------------------------------------------------------------------------
# EACH TIME YOU WANT TO CLEAN A FILE
# -----------------------------------------------------------------------------
#   1. Edit the two file names in the "EDIT THESE" section below.
#   2. Save this file.
#   3. From a command line / terminal, run:
#
#        Rscript gc_template.R
#
#      (Use the full path to this file if you are not in the same folder,
#       e.g. Rscript /path/to/gc_template.R)
#
# -----------------------------------------------------------------------------
# WHAT YOUR INPUT FILE MUST LOOK LIKE
# -----------------------------------------------------------------------------
#   A CSV in "long" format: one measurement per row, with these columns.
#   (Column names must match, OR see "Different column names?" below.)
#
#     subjid       Person identifier (numbers or text). Repeats across a
#                  person's rows.
#     param        What was measured. One of:
#                    WEIGHTKG   weight in kilograms
#                    HEIGHTCM   standing height in centimeters
#                    LENGTHCM   recumbent length in centimeters
#                    HEADCM     head circumference in centimeters
#                    WEIGHTLBS  weight in pounds      (converted internally)
#                    HEIGHTIN   height in inches      (converted internally)
#     agedays      Age in DAYS at the measurement (a number).
#     sex          0 = male, 1 = female (also accepts M/F, male/female, B/G, and
#                  boy/girl, not case-sensitive).
#     measurement  The measured value (a number), in the units named by param.
#     id           OPTIONAL. A unique row identifier. If you do not supply one,
#                  growthcleanr creates one for you.
#
#   Example rows:
#     subjid,param,agedays,sex,measurement
#     101,WEIGHTKG,90,0,5.6
#     101,HEIGHTCM,90,0,59.1
#     101,WEIGHTKG,280,0,9.2
#
# -----------------------------------------------------------------------------
# WHAT THE OUTPUT FILE CONTAINS
# -----------------------------------------------------------------------------
#   Every column from your input file, unchanged, PLUS:
#
#     exclude           growthcleanr's verdict for that row: "Include" if the
#                       value looks usable, or a code naming why it was set
#                       aside (e.g. "Exclude-CF").
#     bin_exclude       A simple two-way summary: "Include" or "Exclude".
#     unit_error_range  TRUE if the value's magnitude looks like a metric/
#                       imperial unit mix-up (a hint to check units).
#     mean_ht           Only if your data has adults: the mean height (cm)
#                       growthcleanr calculated, on adult height rows.
#     id                Added ONLY if you did not supply an id column: the
#                       sequential row id growthcleanr generated (you will see
#                       a harmless "Generated ids" note when this happens).
#
#   growthcleanr never deletes your rows and never changes your measured
#   values -- it only adds these result columns. To keep only the values it
#   considers usable, filter to rows where exclude == "Include" afterward.
#
# =============================================================================


# ---------------------------------------------------------------------------
# EDIT THESE  <-- change the two file names, then save and run.
# ---------------------------------------------------------------------------

input_file  <- "my_data.csv"       # the CSV to read in
output_file <- "my_data_cleaned.csv"  # the CSV to write out


# ---------------------------------------------------------------------------
# You normally do NOT need to change anything below this line.
# ---------------------------------------------------------------------------

library(growthcleanr)
library(data.table)

# Read the input CSV.
df <- fread(input_file)

# Run growthcleanr. This is the one line that does the work.
result <- cleangrowth(df)

# --- OPTIONS (advanced, all optional) --------------------------------------
# To change how growthcleanr runs, delete the line above and use one of these
# instead. Everything not listed keeps its default.
#
#   # Your columns are named differently? Map them (left = growthcleanr's name,
#   # right = your column's name):
#   result <- cleangrowth(df, subjid = "patient_id", measurement = "value")
#
#   # Change the age (in years) that separates children from adults (default 20):
#   result <- cleangrowth(df, adult_cutpoint = 18)
#
#   # Choose how strict the ADULT cleaning is:
#   #   "loosest", "looser" (default), "tighter", "tightest"
#   result <- cleangrowth(df, adult_permissiveness = "tighter")
#
#   # Add a column explaining each exclusion in more detail:
#   result <- cleangrowth(df, exclude_detail = TRUE)
#
#   # Large file? Run in parallel across processor cores:
#   result <- cleangrowth(df, parallel = TRUE, cores = 4)
# ---------------------------------------------------------------------------

# Write the results to the output CSV.
fwrite(result, output_file)

cat("Done. Wrote", nrow(result), "rows to", output_file, "\n")
