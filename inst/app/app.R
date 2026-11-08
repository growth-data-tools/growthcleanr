# app.R — growthcleanr Shiny App (local, save-results version)
# Runs entirely on the user's own computer: data is loaded into local R and never
# uploaded anywhere; results are saved to the user's own machine (no download).
library(shiny)
library(DT)
library(tools)
library(shinyjs)   # closes the results modal on save (shinyjs::onclick)
library(shinyFiles)   # "file already on this computer" chooser (reads a local file in place, no copy)


library(growthcleanr)
options(shiny.maxRequestSize = 5000 * 1024^2)  # 5 GB max file size (local file picker)

# Date-format selector, shared by all three formats. Only matters when age is
# taken from date-of-birth + visit-date columns. Year-first ISO (2013-03-04)
# and spelled-month (11 Aug 2013) dates need no order, so "auto" covers those;
# all-numeric day/month dates (04/03/2013) are ambiguous and need the order
# stated (growthcleanr never guesses it). Routed to gc's `date_order` argument via date_order_arg().
date_order_input <- function(id) {
  selectInput(id,
    "Date format (only needed if you map date-of-birth / visit-date columns and not in default accepted format)",
    choices = c(
      "Year-first (2013-03-04) or spelled month (11 Aug 2013) — do not need to enter format" = "auto",
      "Day / Month / Year — e.g. 04/03/2013" = "dmy",
      "Month / Day / Year — e.g. 03/04/2013" = "mdy"),
    selected = "auto", width = "100%")
}

# One sample value for a mapped date role, as "column: value". `cols` is one
# column, or a wide visit-date block (the first column with a usable value is
# used). Values with no digits ("NA", "unknown") are skipped because growthcleanr
# treats them as blank. Returns NULL when there is nothing to show.
.gc_app_date_sample <- function(df, cols) {
  for (cn in intersect(cols, names(df))) {
    v <- trimws(as.character(df[[cn]]))
    v <- v[!is.na(v) & grepl("[0-9]", v)]
    if (length(v)) return(paste0(cn, ": ", v[1]))
  }
  NULL
}

# Caption under the unit selectors. The unit is auto-set from a recognizable
# name (see the *_unit auto-detect observers), so this tells the user we guessed
# and to confirm. `noun` = "column name" / "prefix" / "parameter value".
# Auto-detect a sex coding from a column's values: if the top-2 non-missing
# values cover > 99% and match a gc default pair (case-insensitive,
# order-independent), return c(male = ..., female = ...) preserving the data's
# own casing; otherwise NULL. 1/2 is deliberately excluded (it is not a gc
# default -- gc's 1=male/2=female guard must still fire), so a 1/2 column
# returns NULL. Pure function (no reactivity) so it is unit-testable.
.gc_app_sex_pairs <- list(c("0", "1"), c("m", "f"), c("male", "female"),
                          c("boy", "girl"), c("b", "g"))
.detect_sex_coding <- function(values) {
  v <- trimws(as.character(values))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(NULL)
  tab <- sort(table(v), decreasing = TRUE)
  if (length(tab) < 2 || sum(tab[1:2]) / sum(tab) <= 0.99) return(NULL)
  top2 <- names(tab)[1:2]; lt <- tolower(top2)
  for (pr in .gc_app_sex_pairs) {
    if (setequal(lt, pr)) return(c(male = top2[lt == pr[1]][1], female = top2[lt == pr[2]][1]))
  }
  NULL
}

# ---- Hostile-input guards (pure, unit-testable; used by the server) ---------
# Each returns a user-facing message string when the problem is present, else
# NULL, so the server can `stop()` (mapping guards) or warn (load note) with the
# message in one place. Kept top-level so tests can call them directly.

# The `<prefix>_<integer>` block columns for a wide measurement prefix.
.gc_app_block_cols <- function(nms, prefix) {
  if (!nzchar(prefix)) return(character(0))
  pre  <- paste0(prefix, "_")
  cand <- nms[startsWith(nms, pre)]
  cand[grepl("^[0-9]+$", substring(cand, nchar(pre) + 1L))]
}

# Mostly non-numeric measurement values. `cols` are the user's real measurement
# column names. A few non-numbers (e.g. "N/A", "<5") are fine: growthcleanr sets
# them aside and reports them in the run notes, as the package does. An error is
# raised only when more than half of a column's non-blank values are not numbers,
# which usually means the wrong column was chosen in Step 2. Reports a count + one
# example value and row.
.gc_app_measurement_error <- function(df, cols) {
  cols <- cols[nzchar(cols) & cols %in% names(df)]
  for (cn in cols) {
    v <- df[[cn]]
    if (is.numeric(v)) next
    ch  <- trimws(as.character(v))
    nonblank <- !is.na(ch) & nzchar(ch)
    bad <- which(nonblank & is.na(suppressWarnings(as.numeric(ch))))
    if (length(bad) > 0.5 * sum(nonblank)) {
      return(sprintf(
        "Most values in your measurement column '%s' aren't numbers (%d of %d, for example '%s' in row %d). Check that you chose the right column in Step 2. Measurements must be numbers only (for example 15.2); a few values that aren't numbers are fine and are set aside.",
        cn, length(bad), sum(nonblank), ch[bad[1]], bad[1]))
    }
  }
  NULL
}

# The same source column mapped to more than one field. `pairs` is a named
# character vector field_label = column; blank entries are ignored.
.gc_app_dup_role_error <- function(pairs) {
  pairs <- pairs[nzchar(pairs)]
  if (!length(pairs)) return(NULL)
  dups <- unique(pairs[duplicated(pairs)])
  if (!length(dups)) return(NULL)
  col   <- dups[1]
  roles <- names(pairs)[pairs == col]
  sprintf("The column '%s' is mapped to more than one field (%s). Each column can be used for only one field. In Step 2, change all but one of those to a different column (or to (None) if the field is optional).",
          col, paste(roles, collapse = ", "))
}

# Duplicate column names in the loaded file (growthcleanr maps by name, so only
# the first column of a repeated name is usable).
.gc_app_dup_colname_note <- function(nms) {
  dups <- unique(nms[duplicated(nms)])
  if (!length(dups)) return(NULL)
  sprintf("Your file has more than one column with the same name (%s). growthcleanr can only use the first column of each name; rename the duplicates so each column has a unique name if you need them kept apart.",
          paste(dups, collapse = ", "))
}

unit_caption <- function(noun) {
  p(style = "font-size: 12px; color: #666;",
    tags$b(sprintf("The unit is set automatically from your %s when it's recognizable", noun)),
    sprintf(" (e.g. HEIGHTIN sets inches). Please confirm it matches your actual values — if a %s is named WEIGHTKG but the numbers are pounds, switch the unit to lbs.", noun))
}

# Collapsed "length vs. height" advanced panel (native <details>, so it starts
# closed and needs no JS). Explains the default, offers the length.adjust
# reconciliation, and — when enabled — an optional field to map a recumbent-
# length measurement that is separate from the standing-height field above.
# `noun` = "column" / "prefix" / "parameter value"; ids differ per format.
# Long-format-only panel. When the box is checked, the user assigns length vs
# height by parameter value (instead of gc's age-based default), and both share
# one unit. See the server `long_param_coding()` for how the fields are used.
# Links to the hosted "wrapper" vignette section. The pkgdown anchor is derived
# deterministically from the heading; if that heading is reworded, pin an
# explicit {#phase-8-lengthcm-heightcm-relabel} on it.
gc_length_ref <- tags$a(
  href = "https://growthcleanr.org/articles/wrapper.html#phase-8-lengthcm-heightcm-relabel",
  target = "_blank", rel = "noopener",
  "the growthcleanr “wrapper” vignette, section “Phase 8: Length vs. height handling”")
length_height_panel <- function() {
  tags$details(
    style = "margin: 10px 0; border: 1px solid #ddd; border-radius: 6px; padding: 6px 10px; background: #fafafa;",
    tags$summary(style = "cursor: pointer; font-weight: bold;",
                 "Optional: control length vs. height assignment"),
    div(style = "padding-top: 8px;",
      p(style = "font-size: 12px; color: #555;",
        "By default, growthcleanr assumes that linear growth measurements for ages <2 years are taken as recumbent length and those ≥2 years are taken as standing height (for infants whose data are consistent with prematurity or small-for-gestational age (SGA) and who receive an assigned gestational age for cleaning purposes, by corrected age), regardless of whether the column name or parameter value says length or height. Read more at ",
        gc_length_ref,
        ". This default can be overridden only for data in long format."),
      p(style = "font-size: 12px; color: #555;",
        "Check the box to use each row's length or height label (the parameter value) to reconcile recumbent and standing measurements. See ",
        gc_length_ref, " for how this affects cleaning."),
      checkboxInput("long_length_adjust",
        "Use each value's length/height label (length.adjust)", FALSE),
      conditionalPanel("input.long_length_adjust == true",
        fluidRow(
          column(6, textInput("val_length", "Length parameter value:", ""))
        ),
        p(style = "font-size: 12px; color: #666;",
          "Your Height parameter value above is treated as standing height, and the length value here uses the same “Length/height unit” selected above.")
      )
    )
  )
}

# Optional memory/output trim, offered in every format's mapping panel. Default
# OFF, so leaving it reproduces the current behavior (all columns carried
# through). When on, the mapping step keeps only the columns cleaning needs and
# drops the rest before running -- helpful for wide EHR exports with many extra
# columns. `id` differs per format. See the server `needed_cols()`.
keep_only_checkbox <- function(id) {
  tagList(
    checkboxInput(id,
      "Keep only the columns growthcleanr needs (drop the rest to save memory)",
      FALSE),
    p(style = "font-size: 12px; color: #666; margin-top: -6px;",
      "Off by default: every other column is normally carried through to the results file untouched (when semi-wide or wide data is returned in long format, they are repeated on each measurement from their row; for wide data, only subject-level columns are returned). Check this only if you don't need the extra columns in the cleaned data — it lowers memory use on files with many columns, but the dropped columns won't appear in the results file.")
  )
}

# ==========================================================================
ui <- fluidPage(
  useShinyjs(),  # required for the shinyjs::onclick() handler used below
  tags$head(
    tags$style(HTML("
      .title-panel {
        background-color: #FFDAB9;   /* light orange (peach) */
        padding: 25px;
        border-radius: 8px;
        margin-bottom: 25px;
      }
      .title-panel h2 {
        margin-top: 0;
        color: #333;
      }
      .title-panel p {
        font-size: 14px;
        line-height: 1.6;
        margin-bottom: 10px;
      }
      .warn-box {
        background-color: #FFF3CD;
        border: 1px solid #FFECB5;
        padding: 12px 15px;
        border-radius: 6px;
        margin-bottom: 15px;
        font-size: 13px;
      }
      .footer-panel {
        text-align: center;
        padding: 15px;
        color: #666;
        font-size: 90%;
        margin-top: 25px;
        border-top: 1px solid #ddd;
      }
      .diagnostic-output {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        border: 1px solid #dee2e6;
        overflow-x: auto;
        font-family: 'Courier New', monospace;
        font-size: 12px;
        white-space: pre-wrap;
        max-height: 600px;
        overflow-y: auto;
      }
      .upload-box {
        background-color: #e7f3ff;
        border: 2px dashed #4a90e2;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        text-align: center;
      }
      .next-step {
        background-color: #D6ECD6;
        border-left: 6px solid #2E8B57;
        padding: 16px 20px;
        border-radius: 6px;
        margin-top: 25px;
        margin-bottom: 10px;
        font-size: 18px;
        font-weight: bold;
        color: #1E5631;
      }
    "))
  ),

  # Title Panel with light orange background
  div(class = "title-panel",
      h2("growthcleanr Shiny app"),
      p("This Shiny application runs growthcleanr in R on your own computer. Load your height, weight, and head circumference data (CSV, TXT, or TSV) — from your own computer, into R on your own computer — to process growth measurements in ",
        strong("standard long"), ", ", strong("semi-wide"), ", or ", strong("wide"), " formats."),
      p("Your data remains entirely within your secure environment and is never transmitted externally (even though it is displayed in a browser). More information is in the ",
        a("how-to guide",
          href = "https://growthcleanr.org/articles/how-to-shiny.html",
          target = "_blank"), ".")
  ),

  # ---- Troubleshooting: a collapsed panel shown on every step, aimed at
  # non-technical users. Covers the problems that stop people BEFORE they reach
  # a specific error message (app won't open, package not installed, file won't
  # load, chooser can't see a drive, run looks frozen, nothing included). Install
  # specifics are intentionally deferred to the how-to guide (linked in the title
  # above) so this app never carries an install recipe that could drift. Native
  # <details>, so it starts closed and needs no JavaScript.
  tags$details(
    style = "margin: 0 0 20px 0; border: 1px solid #ddd; border-radius: 6px; padding: 8px 14px; background: #fafafa;",
    tags$summary(style = "cursor: pointer; font-weight: bold; font-size: 15px;",
                 "Having trouble? Open this for troubleshooting tips"),
    div(style = "font-size: 13px; color: #444; padding-top: 8px;",
      p(tags$b("The app won't open, or opens and then shows a blank or red-error page."),
        " This almost always means growthcleanr (or one of the packages it uses) isn't installed yet, or R was not restarted after installing. Follow the install steps in the ",
        a("how-to guide", href = "https://growthcleanr.org/articles/how-to-shiny.html", target = "_blank"),
        ", then fully restart R and launch the app again. If a red message appears in the R console when you launch, that message usually names the missing piece."),
      p(tags$b("Step 3 says growthcleanr isn't installed, or won't load."),
        " Install growthcleanr using the how-to guide, restart R, and click ",
        tags$i("Verify growthcleanr version"), " again."),
      p(tags$b("Your data file won't load in Step 1.")),
      tags$ul(
        tags$li("Supported file types are CSV, TXT, and TSV (comma-, tab-, or semicolon-separated). If your data is in Excel, use File > Save As and choose CSV, then load that file."),
        tags$li("The very first row of the file must be the column names, with no title or notes rows above it. Delete any such rows and re-save."),
        tags$li("Every row should have the same number of columns."),
        tags$li("If the preview shows everything crammed into a single column, the app didn't recognize how your file separates values — re-save it as a CSV.")
      ),
      p(tags$b("The \"Read a file already on this computer\" window doesn't show my drive or folder."),
        " It starts at your home folder and lists the drives your computer has connected. For a file on an external or network drive, make sure that drive is plugged in or connected, then look for it in the list."),
      p(tags$b("Cleaning is taking a long time, or looks frozen."),
        " Large files are cleaned in checkpoints of 5,000 subjects, and each checkpoint typically takes 2-10 minutes. Watch the progress bar in Step 3 — the app is still working even when it looks still. Bigger files, and subjects with more measurements, take longer."),
      p(tags$b("After cleaning, nothing (or almost nothing) was included."),
        " This is almost always a mapping problem in Step 2 — the parameter values or columns weren't matched to what growthcleanr expected. Go back to Step 2, recheck your choices against the \"Check your key columns\" tables, and read the notes shown after the run.")
    )
  ),

  tabsetPanel(

    # =============== STEP 1: Load & Preview Data ===============
    tabPanel("Step 1: Load & Preview Data",
             h3("First, select data format"),
             radioButtons("demo_choice", "Select the format your dataset is in:",
                          choices = c("Long format (one row per measurement)" = "long",
                                      "Semi-wide format (one row per visit)" = "semiwide",
                                      "Wide format (one row per subject)" = "wide"),
                          selected = character(0)),

             div(class = "warn-box",
                 tags$b("Data format descriptions:"), br(),
                 tags$ul(
                   tags$li(tags$b("Long format:"), " One row per measurement. Required columns are a subject identifier, sex, age (age in days, or date of birth and visit date), the parameter type (weight/height/head circumference), and measurement value (e.g. 15.2, 120.4)."),
                   tags$li(tags$b("Semi-wide format:"), " One row per visit. Each measurement type is in its own column on the same row (e.g., WEIGHTKG, HEIGHTCM)."),
                   tags$li(tags$b("Wide format:"), " One row per subject. Multiple visits stored as numbered columns (e.g., agedays_1, WEIGHTKG_1, HEIGHTCM_1, agedays_2, WEIGHTKG_2, HEIGHTCM_2).")
                 )
             ),

             h3("Then, load your data file", style = "margin-top: 35px;"),
             p(style = "font-size: 13px; color: #555;",
               "There are two ways to load a file below based on file size. Supported file types: CSV (comma-, tab-, or semicolon-separated), TXT, TSV."),
             div(class = "upload-box",
                 fileInput("file_upload",
                           "Upload a file through your browser (familiar file picker, best for files <500 MB)",
                           accept = c(".csv", ".txt", ".tsv",
                                      "text/csv",
                                      "text/comma-separated-values",
                                      "text/tab-separated-values",
                                      "text/plain",
                                      ".csv"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected")
             ),

             div(style = "text-align: left; font-weight: bold; font-size: 24px; margin: 6px 0;", "OR"),

             # Read-in-place input (shinyFiles): pick a file already on this
             # computer and read it directly (no copy, no 5 GB cap). Recommended for
             # large files. See the server `sf_roots()` / `read_and_store()` wiring.
             div(class = "upload-box", style = "text-align: left;",
                 tags$b("Read a file already on this computer (recommended for files >500MB, required for >5GB)"), br(),
                 # btn-default (not btn-primary) so this matches the white/gray
                 # "Browse..." button of the upload box above.
                 shinyFilesButton("file_path_btn", "Choose a file on this computer",
                                  title = "Select a data file (CSV / TXT / TSV) on this computer",
                                  multiple = FALSE, class = "btn-default"),
                 p(style = "font-size: 12px; color: #666; margin-top: 10px;", textOutput("chosen_path"))
             ),

             hr(),
             h4("Then, preview first 10 rows"),
             DTOutput("preview_table"),
             div(class = "next-step",
                 "Next: When you have finished previewing your data, click on the tab above for Step 2."),

             # Large-file guidance lives at the bottom (plain text, no color box) so
             # it does not interrupt the format -> load -> preview workflow; a
             # pointer near the top links here.
             hr(),
             h4("Notes on large files"),
             div(style = "font-size: 13px; color: #555;",
                 p("Several things shape how the growthcleanr Shiny app handles large files, including how the file is loaded, whether the dataset is split into checkpoints, and whether extra columns are removed."),
                 p(tags$b("The two loading methods.")),
                 tags$ul(
                   tags$li(tags$b("Upload a file through your browser"), " (the first blue box) works well for files under approximately 500 MB. It does a little extra work because it first copies the file to a temporary location and then reads it, but it has a more familiar, slightly easier to use interface."),
                   tags$li(tags$b("Read a file already on this computer"), " (the second blue box) is recommended for files >500 MB and is required for files over 5 GB. It reads the file without making a copy.")
                 ),
                 p(tags$b("Checkpointing")),
                 p("The cleaning algorithm uses a lot of working memory. Without special handling, runtime grows much faster than the number of subjects. To avoid this, growthcleanr automatically uses checkpointing for datasets with more than 5,000 subjects. With checkpointing, growthcleanr splits the data into checkpoints of 5,000 subjects and evaluates them separately. With checkpointing, runtime increases relatively linearly. Checkpointing creates a folder of intermediate files on your computer (in R's temporary directory). You do not need to manage it: it holds nothing you need to keep, and its files are removed automatically when the run finishes."),
                 p(tags$b("Removing extra columns")),
                 p("growthcleanr uses a subject ID, sex, age (an age-in-days column, or date-of-birth + visit-date), the measurement column(s), and, in long format, the parameter column. Extra columns are carried through untouched (when semi-wide or wide data is returned in long format, they are repeated on each measurement from their row; for wide data, only subject-level columns are returned) and never affect cleaning, but they add to load time, memory, and the size of the results file. In Step 2 (the next tab) you can click the 'keep only the columns growthcleanr needs' option to remove extra columns.")
             )
    ),

    # =============== STEP 2: Map Columns ===============
    tabPanel("Step 2: Map Columns",
             h3("Map your data to growthcleanr format"),
             div(class = "warn-box",
                 "Make sure growthcleanr knows which columns and values to use."
             ),
             uiOutput("mapping_ui"),
             hr(),
             h4("Then, check your key columns"),
             p(style = "font-size: 12px; color: #666;",
               "These update as you choose columns above. Use them to confirm the sex and parameter columns and to set the value codings correctly (the sex coding auto-fills when the values are recognized)."),
             fluidRow(
               column(6, uiOutput("sex_values_ui")),
               column(6, uiOutput("param_values_ui"))
             ),
             hr(),
             h4("After checking data, click button below"),
             p(style = "font-size: 12px; color: #666;",
               "This sends information to growthcleanr about how to handle your data."),
             actionButton("map_btn", "Apply Mapping", class = "btn-primary"),
             hr(),
             h4("Mapped Data Preview"),
             DTOutput("mapped_preview"),
             div(class = "next-step",
                 "Next: When you have finished checking and mapping your data, click on the tab above for Step 3.")
    ),

    # =============== STEP 3: Run growthcleanr ===============
    tabPanel("Step 3: Run growthcleanr",
             h4("First, verify growthcleanr version"),
             actionButton("install_btn", "Verify growthcleanr version", class = "btn-info"),
             verbatimTextOutput("install_status"),
             br(),

             h4("Then, perform a quick test (optional)"),
             p("Run a quick test on a random sample of up to 100 subjects (whole trajectories) to review data quality before full processing."),
             actionButton("trial_btn", "Run quick test", class = "btn-warning"),
             br(), br(),
             div(class = "diagnostic-output",
                 verbatimTextOutput("trial_report")
             ),
             br(),

             h4("Now, clean your data"),
             p("Each checkpoint of 5,000 subjects will typically take 2-10 minutes depending on your computing environment and how many measurements the subjects have. See the progress bar indicating how many checkpoints have been cleaned."),
             actionButton("run_btn", "Clean my data", class = "btn-success btn-lg"),
             br(), br(),
             uiOutput("save_results_ui"),
             uiOutput("run_warnings"),
             h4("Exclusion Summary Statistics (exclude column)"),
             verbatimTextOutput("exclude_stats_text"),
             DTOutput("exclude_summary"),
             br(),
             h4("Sample Output — First 20 rows with exclude & bin_exclude"),
             DTOutput("sample_output"),
             br()
    )
  ),

  # ---- Footer: OS-PCORTF funding acknowledgment & link ----
  hr(),
  div(class = "footer-panel",
      p("The Shiny app for growthcleanr was funded by the HHS Office of the Secretary Patient-Centered Outcomes Research Trust Fund (OS-PCORTF)."),
      p(
        "Learn more at the ",
        a("OS-PCORTF site",
          href = "https://aspe.hhs.gov/collaborations-committees-advisory-groups/os-pcortf/explore-portfolio",
          target = "_blank"),
        "."
      )
  )
)

# ==========================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(
    raw_df          = NULL,
    mapped_df       = NULL,
    mapped_roles    = NULL,
    output_df       = NULL,
    install_msg     = "Not yet run.",
    exclude_tab     = NULL,
    trial_report    = NULL,
    run_warnings    = NULL,
    chosen_path     = NULL
  )

  # Pop-ups that stay until closed. Their ids are kept so they can be removed when
  # they no longer apply; a plain variable, so reading it adds no reactive dependency.
  sticky_ids <- character(0)
  sticky_note <- function(msg, type) {
    sticky_ids <<- c(sticky_ids, showNotification(msg, type = type, duration = NULL))
  }
  clear_sticky <- function() {
    for (id in sticky_ids) removeNotification(id)
    sticky_ids <<- character(0)
  }
  # Results, notes and pop-ups describe one file + mapping, so clear them when
  # either changes. Otherwise a failed run would leave the prior file's results
  # on screen and available to save.
  clear_results <- function() {
    clear_sticky()
    rv$run_warnings <- NULL
    rv$trial_report <- NULL
    rv$output_df    <- NULL
    rv$exclude_tab  <- NULL
  }
  # A new file or format also voids the mapping, so Step 3 cannot run on the old data.
  clear_mapping <- function() {
    clear_results()
    rv$mapped_df    <- NULL
    rv$mapped_roles <- NULL
  }
  observeEvent(input$demo_choice, {
    # Nothing to void the first time a format is picked; keeps file-load notes.
    if (!is.null(rv$mapped_df) || !is.null(rv$output_df)) clear_mapping()
  })

  # App-specific pointer added to growthcleanr's date-format errors, which name
  # the `date_order` argument that app users set through a Step 2 dropdown.
  date_order_hint <- "In this app, choose the matching option in the Step 2 'Date format' field and run again"

  fmt <- reactive({
    req(input$demo_choice)
    input$demo_choice
  })

  # Build the sex_coding argument to splice into cleangrowth()/gc_runtrial()
  # calls via do.call(). When the user leaves the app's default 0/1 fields we
  # OMIT sex_coding entirely (return an empty list) so growthcleanr uses its own
  # default coding -- which both recognizes 0/1/m/f/male/female/boy/girl AND
  # keeps its 1=male/2=female safety guard active (that guard fires only when
  # sex_coding is identical to gc's default, i.e. when none is supplied). We pass
  # an explicit sex_coding (which suppresses the guard) only when the user
  # actually enters their own male/female codes.
  sex_coding_arg <- function() {
    if (identical(input$male_val, "0") && identical(input$female_val, "1")) {
      list()
    } else {
      list(sex_coding = list(male = input$male_val, female = input$female_val))
    }
  }

  # Build the semiwide_cols list mapping the unit-appropriate canonical parameter
  # to the user's real (unrenamed) measurement column name. Imperial keys
  # (WEIGHTLBS/HEIGHTIN/HEADIN) tell gc to convert lbs->kg / in->cm; metric keys
  # pass the value through. `df` is the mapped data (measurement columns keep
  # their original names); a mapped column absent from `df` is skipped so gc
  # never receives a mapping to a missing column.
  semiwide_cols_arg <- function(df) {
    cols <- list()
    if (nzchar(input$sw_weight) && input$sw_weight %in% names(df)) {
      cols[[if (input$sw_unit_weight == "lbs") "WEIGHTLBS" else "WEIGHTKG"]] <- input$sw_weight
    }
    if (nzchar(input$sw_height) && input$sw_height %in% names(df)) {
      cols[[if (input$sw_unit_height == "in") "HEIGHTIN" else "HEIGHTCM"]] <- input$sw_height
    }
    if (nzchar(input$sw_head) && input$sw_head %in% names(df)) {
      cols[[if (input$sw_unit_head == "in") "HEADIN" else "HEADCM"]] <- input$sw_head
    }
    cols
  }

  # Build the wide_cols list mapping the unit-appropriate canonical parameter to
  # the user's column-name prefix. Imperial keys trigger gc's unit conversion.
  # Prefixes are not renamed in Step 2 (gc discovers the `<prefix>_N` blocks), so
  # there is no data frame to check against here.
  wide_cols_arg <- function() {
    cols <- list()
    if (nzchar(input$w_weight_prefix)) {
      cols[[if (input$w_unit_weight == "lbs") "WEIGHTLBS" else "WEIGHTKG"]] <- input$w_weight_prefix
    }
    if (nzchar(input$w_height_prefix)) {
      cols[[if (input$w_unit_height == "in") "HEIGHTIN" else "HEIGHTCM"]] <- input$w_height_prefix
    }
    if (nzchar(input$w_head_prefix)) {
      cols[[if (input$w_unit_head == "in") "HEADIN" else "HEADCM"]] <- input$w_head_prefix
    }
    cols
  }

  # param_coding for long format: map each unit-appropriate canonical parameter
  # to the user's parameter *value* (WEIGHT/HT/... as it appears in the param
  # column). Length is the optional recumbent-length value from the advanced
  # panel; it maps to LENGTHCM or LENGTHIN.
  long_param_coding <- function() {
    pa <- list()
    if (nzchar(input$val_weight)) pa[[if (input$unit_weight == "lbs") "WEIGHTLBS" else "WEIGHTKG"]] <- input$val_weight
    if (nzchar(input$val_head))   pa[[if (input$unit_head == "in") "HEADIN" else "HEADCM"]] <- input$val_head
    # One linear unit (the "Length/height unit") governs both height and length.
    lin_in <- identical(input$unit_height, "in")
    if (nzchar(input$val_height)) pa[[if (lin_in) "HEIGHTIN" else "HEIGHTCM"]] <- input$val_height
    # Assign-by-parameter-value mode: add a length parameter value under the
    # same unit; length.adjust is passed alongside (see length_adjust_arg).
    if (isTRUE(input$long_length_adjust) && isTruthy(input$val_length)) {
      pa[[if (lin_in) "LENGTHIN" else "LENGTHCM"]] <- input$val_length
    }
    pa
  }

  # length.adjust to splice into cleangrowth()/gc_runtrial() via do.call().
  # Passed only when the advanced-panel checkbox `id` is on; gc's default is
  # FALSE, so an unchecked box reproduces the default behavior exactly.
  length_adjust_arg <- function(id) {
    if (isTRUE(input[[id]])) list(length.adjust = TRUE) else list()
  }

  # ---- Step 2 mapping helpers ------------------------------------------------
  # A required column must be named AND present. Stop with a plain-language,
  # actionable message (which field, which name, and the columns actually
  # available) instead of dplyr's terse "Column `x` doesn't exist".
  need_col <- function(df, colname, label) {
    if (!nzchar(colname)) {
      stop(sprintf("Please choose the column for %s in Step 2 (use the drop-down for that field).", label), call. = FALSE)
    }
    if (!colname %in% names(df)) {
      stop(sprintf(
        "%s: column '%s' was not found in your data. Your columns are: %s. Choose one of them in the '%s' field in Step 2.",
        label, colname, paste(names(df), collapse = ", "), label), call. = FALSE)
    }
    invisible(TRUE)
  }

  # Optional / auto-detected column (agedays, id, dob, visit_date). Return the
  # user's column name if present, or NULL if the field is blank. If it is absent,
  # silently skip ONLY when the field still holds its pre-filled default (the
  # app's guess, e.g. "dob" on a file that has no dates) -- but if the user typed
  # a NON-default name that isn't in the data, that is almost certainly a mistake,
  # so stop with a clear message instead of ignoring it.
  role_opt <- function(df, from, default, label) {
    if (!nzchar(from)) return(NULL)
    if (from %in% names(df)) return(from)
    if (!identical(from, default)) {
      stop(sprintf(
        "%s: column '%s' was not found in your data. Your columns are: %s. Choose your column in the '%s' field in Step 2, or clear it if you don't have that column.",
        label, from, paste(names(df), collapse = ", "), label), call. = FALSE)
    }
    NULL  # equals the default and absent -> the app's guess didn't match; skip.
  }

  # date_order to splice into cleangrowth()/gc_runtrial() via do.call(). Only
  # needed for ambiguous numeric day/month dates; "auto" (ISO / spelled-month)
  # passes nothing so gc auto-detects.
  date_order_arg <- function(val) {
    if (is.null(val) || !nzchar(val) || identical(val, "auto")) list()
    else list(date_order = val)
  }

  # After mapping a long / semi-wide file, confirm there is an age source:
  # either an age-in-days column, or BOTH date of birth and visit date. Caught
  # here so the user gets a clear Step-2 message rather than a downstream gc error.
  check_age_source <- function(roles) {
    has_age   <- !is.null(roles$agedays)
    has_dob   <- !is.null(roles$dob)
    has_visit <- !is.null(roles$visit_date)
    if (has_age || (has_dob && has_visit)) return(invisible(TRUE))
    if (has_dob && !has_visit) {
      stop("Found a date-of-birth column but no visit-date column. Both are needed to compute age from dates. Choose your visit-date column in the 'Visit date' field in Step 2 (or provide an 'agedays' column).", call. = FALSE)
    }
    if (has_visit && !has_dob) {
      stop("Found a visit-date column but no date-of-birth column. Both are needed to compute age from dates. Choose your date-of-birth column in the 'Date of birth' field in Step 2 (or provide an 'agedays' column).", call. = FALSE)
    }
    stop("No age information found. growthcleanr needs either an 'agedays' column (age in days at each measurement) OR both a date-of-birth column and a visit-date column. Choose the matching column(s) in Step 2 (Age, Date of birth, Visit date).", call. = FALSE)
  }

  # Canonical growthcleanr measurement parameter names (metric + imperial), used
  # to spot columns that *look* like measurements but weren't mapped.
  gc_param_names <- c("WEIGHTKG", "WEIGHTLBS", "HEIGHTCM", "HEIGHTIN",
                      "LENGTHCM", "LENGTHIN", "HEADCM", "HEADIN")

  # TRUE if the data has any `<prefix>_<integer>` block columns for `prefix`
  # (mirrors gc's wide block discovery). Used to validate wide measurement
  # prefixes in Step 2 before gc does.
  has_blocks <- function(nms, prefix) {
    if (!nzchar(prefix)) return(FALSE)
    pre  <- paste0(prefix, "_")
    cand <- nms[startsWith(nms, pre)]
    any(grepl("^[0-9]+$", substring(cand, nchar(pre) + 1L)))
  }

  # Columns (semi-wide) or prefixes (wide) whose names are canonical gc
  # parameters but that the user did NOT map -- so they will be silently
  # ignored. Returns a character vector for an informational note. Long format
  # has no such concept (parameter identity comes from values), so returns "".
  detect_unmapped_params <- function(format_choice, raw) {
    nms <- names(raw)
    if (identical(format_choice, "semiwide")) {
      mapped <- c(input$sw_weight, input$sw_height, input$sw_head)
      intersect(gc_param_names, setdiff(nms, mapped))
    } else if (identical(format_choice, "wide")) {
      mapped <- c(input$w_weight_prefix, input$w_height_prefix, input$w_head_prefix)
      cand   <- setdiff(gc_param_names, mapped)
      cand[vapply(cand, function(p) has_blocks(nms, p), logical(1))]
    } else {
      character(0)
    }
  }

  # Columns cleaning actually needs, given the mapped data frame `df` and format
  # `fm`. Used by the optional "keep only needed columns" trim to shrink memory
  # and output size on wide EHR exports. growthcleanr otherwise parks every extra
  # column and reattaches it to the output verbatim, so dropping them changes
  # only memory/output size, not the cleaning math. Returns existing column names
  # in their original order.
  #   long     : subjid, sex, param, measurement + present agedays/id/dob/visit_date
  #   semiwide : subjid, sex, the mapped measurement columns + present age columns
  #   wide     : subjid, sex, present dob + every <prefix>_N block column for the
  #              agedays / visit_date / measurement prefixes
  needed_cols <- function(fm, df, roles) {
    nms <- names(df)
    role_cols <- unlist(roles, use.names = FALSE)
    if (fm == "long") {
      keep <- role_cols
    } else if (fm == "semiwide") {
      meas <- c(input$sw_weight, input$sw_height, input$sw_head)
      meas <- meas[nzchar(meas) & meas %in% nms]
      keep <- c(role_cols, meas)
    } else { # wide
      prefixes <- c(input$w_agedays_prefix, input$w_visit_date,
                    input$w_weight_prefix, input$w_height_prefix, input$w_head_prefix)
      prefixes <- prefixes[nzchar(prefixes)]
      block <- unlist(lapply(prefixes, function(p) {
        pre <- paste0(p, "_")
        cand <- nms[startsWith(nms, pre)]
        cand[grepl("^[0-9]+$", substring(cand, nchar(pre) + 1L))]
      }), use.names = FALSE)
      keep <- c(role_cols, block)
    }
    intersect(nms, unique(keep))  # existing columns, original order
  }

  # ---- Unit auto-detection --------------------------------------------------
  # Infer the unit selector from a recognizable measurement name/value. gc treats
  # the parameter name as unit-bearing (HEIGHTIN = inches), so when the user's
  # column name, prefix, or parameter value clearly names a unit we default the
  # toggle to match. Detection is deliberately conservative: exact canonical gc
  # names, or a unit token (kg/lbs/cm/in and spellings) at a word boundary at the
  # end of the name. LENGTH* names are intentionally not detected. Returns
  # NA when unknown, leaving the current selection untouched. The user can always
  # override; the caption tells them to confirm.
  .unit_norm <- function(x) tolower(trimws(as.character(x)))
  detect_weight_unit <- function(name) {
    n <- .unit_norm(name); if (!nzchar(n)) return(NA_character_)
    if (n == "weightlbs") return("lbs")
    if (n == "weightkg")  return("kg")
    if (grepl("(^|[^a-z])(lbs?|pounds?)$", n)) return("lbs")
    if (grepl("(^|[^a-z])(kgs?|kilograms?)$", n)) return("kg")
    NA_character_
  }
  detect_length_unit <- function(name, cm_names, in_names) {
    n <- .unit_norm(name); if (!nzchar(n)) return(NA_character_)
    if (n %in% tolower(in_names)) return("in")
    if (n %in% tolower(cm_names)) return("cm")
    if (grepl("(^|[^a-z])(ins?|inch|inches)$", n)) return("in")
    if (grepl("(^|[^a-z])(cms?|centimet(er|re)s?)$", n)) return("cm")
    NA_character_
  }
  # Wire one name field to its unit selector.
  autodetect_unit <- function(name_id, unit_id, detector) {
    force(name_id); force(unit_id); force(detector)
    observeEvent(input[[name_id]], {
      u <- detector(input[[name_id]])
      if (!is.na(u)) updateSelectInput(session, unit_id, selected = u)
    }, ignoreInit = TRUE)
  }
  # The height field recognizes LENGTH* names too, but only for the *unit*
  # (cm/in) -- the height-vs-length label is handled by the optional panel.
  .ht_cm <- c("HEIGHTCM", "LENGTHCM"); .ht_in <- c("HEIGHTIN", "LENGTHIN")
  .len_cm <- c("LENGTHCM", "HEIGHTCM"); .len_in <- c("LENGTHIN", "HEIGHTIN")
  # Long parameter values, semi-wide columns, wide prefixes.
  autodetect_unit("val_weight", "unit_weight", detect_weight_unit)
  autodetect_unit("val_height", "unit_height", function(n) detect_length_unit(n, .ht_cm, .ht_in))
  autodetect_unit("val_head",   "unit_head",   function(n) detect_length_unit(n, "HEADCM", "HEADIN"))
  # The length value feeds the one shared linear unit (unit_height), same as the
  # height value does.
  autodetect_unit("val_length", "unit_height", function(n) detect_length_unit(n, .len_cm, .len_in))

  # The linear unit governs height only in the default mode and both height and
  # length when assigning by parameter value, so its label follows the checkbox.
  observeEvent(input$long_length_adjust, {
    updateSelectInput(session, "unit_height",
      label = if (isTRUE(input$long_length_adjust)) "Length/height unit" else "Height unit")
  })
  autodetect_unit("sw_weight", "sw_unit_weight", detect_weight_unit)
  autodetect_unit("sw_height", "sw_unit_height", function(n) detect_length_unit(n, .ht_cm, .ht_in))
  autodetect_unit("sw_head",   "sw_unit_head",   function(n) detect_length_unit(n, "HEADCM", "HEADIN"))
  autodetect_unit("w_weight_prefix", "w_unit_weight", detect_weight_unit)
  autodetect_unit("w_height_prefix", "w_unit_height", function(n) detect_length_unit(n, .ht_cm, .ht_in))
  autodetect_unit("w_head_prefix",   "w_unit_head",   function(n) detect_length_unit(n, "HEADCM", "HEADIN"))

  # ---- Step 1: Read a data file and preview ----
  # Shared reader for BOTH input methods: the browser file picker (fileInput ->
  # Shiny copies the bytes to a temp file, `path` is that copy) and the "file
  # already on this computer" chooser (shinyFiles -> `path` is the ORIGINAL file,
  # read in place, no copy). `name` is the display name; `size` is the file size
  # in bytes, stored so the run step can decide whether to checkpoint large jobs.
  read_and_store <- function(path, name, size) {
    clear_mapping()
    file_ext <- tolower(tools::file_ext(name))
    df <- tryCatch({
      if (file_ext %in% c("csv", "txt", "tsv")) {
        # Read with data.table::fread: fast and memory-efficient for large files,
        # auto-detects the delimiter (comma / tab / semicolon / pipe / ...),
        # handles quoted fields, skips blank lines, and strips a leading UTF-8 BOM.
        # check.names = FALSE keeps column names exactly as in the file
        # (growthcleanr maps by the name you pass and carries names back verbatim,
        # so make.names()-style mangling would only break mapping). data.table =
        # FALSE returns a plain data.frame so the rest of the app is unchanged.
        # blank.lines.skip = TRUE so a stray empty row in the middle doesn't
        # truncate the data (fread otherwise stops at the first blank line).
        d <- data.table::fread(path, header = TRUE, check.names = FALSE,
                               data.table = FALSE, showProgress = FALSE,
                               blank.lines.skip = TRUE)
        # Belt-and-suspenders BOM strip (in case an older data.table keeps it).
        if (ncol(d) >= 1) names(d)[1] <- sub("^﻿", "", names(d)[1])
        d
      } else {
        showNotification("This file type isn't supported. The app reads CSV, TXT, or TSV files (comma-, tab-, or semicolon-separated). If your data is in Excel, use File > Save As and choose CSV, then load that file.",
                         type = "error", duration = 10)
        NULL
      }
    }, error = function(e) {
      sticky_note(
        paste0("Could not read the file: ", conditionMessage(e),
               ". Check that it has a single header row and the same number of columns in every row (e.g. remove any title or metadata rows above the header)."),
        type = "error")
      NULL
    })

    if (!is.null(df) && nrow(df) > 0) {
      rv$raw_df    <- df
      rv$file_info <- list(name = name, size = size, datapath = path)
      showNotification(paste("✓ File loaded successfully:", nrow(df), "rows,", ncol(df), "columns"),
                       type = "message", duration = 5)
      # A single column whose name still contains a delimiter usually means the
      # file uses a delimiter we didn't recognize (e.g. a pipe).
      if (ncol(df) == 1 && grepl("[,;\t|]", names(df)[1])) {
        sticky_note(
          "Your file loaded as a single column — its delimiter may not be recognized. The app reads comma-, tab-, or semicolon-separated files; re-save your data in one of those formats.",
          type = "warning")
      }
      # Duplicate column names are ambiguous (only the first of each name is used).
      dupnote <- .gc_app_dup_colname_note(names(df))
      if (!is.null(dupnote)) sticky_note(dupnote, type = "warning")
    } else {
      showNotification("The file has no data rows (or could not be read). Check that it has a header row and at least one row of data.",
                       type = "warning", duration = 10)
    }
  }

  # Method A — browser file picker. Shiny has already copied the bytes to a temp
  # file by the time this fires; `datapath` points to that copy.
  observe({
    req(input$file_upload)
    fi <- input$file_upload
    read_and_store(fi$datapath, fi$name, fi$size)
  })

  # Method B — "file already on this computer" (shinyFiles). Deliberately generic
  # roots so it works on anyone's machine, with no path tied to any one setup:
  # the user's home folder plus every volume/drive the OS has mounted (Windows
  # drive letters, macOS /Volumes, Linux mounts, external/network drives). Passed
  # as a function so newly mounted drives appear without restarting the app.
  sf_roots <- function() c(Home = path.expand("~"), shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, "file_path_btn", roots = sf_roots,
                              filetypes = c("csv", "txt", "tsv"), session = session)

  observeEvent(input$file_path_btn, {
    sel <- shinyFiles::parseFilePaths(sf_roots(), input$file_path_btn)
    if (nrow(sel) == 0) return()   # dialog opened but nothing picked yet
    path <- as.character(sel$datapath[1])
    if (!nzchar(path) || !file.exists(path)) return()
    rv$chosen_path <- path
    # `size` from the real file on disk; the original is read in place (no copy).
    read_and_store(path, basename(path), file.info(path)$size)
  })

  output$chosen_path <- renderText({
    if (is.null(rv$chosen_path)) "" else paste("Selected file:", rv$chosen_path)
  })

  output$preview_table <- renderDT({
    req(rv$raw_df)
    # Only name the format once one has been chosen; before that input$demo_choice
    # is empty, which would otherwise render "Preview of loaded  format data".
    cap <- if (isTruthy(input$demo_choice)) {
      paste("Preview of loaded", input$demo_choice, "format data (first 10 rows)")
    } else {
      "Preview of your data (first 10 rows)"
    }
    datatable(head(rv$raw_df, 10),
              options = list(scrollX = TRUE, pageLength = 10),
              caption = cap)
  })

  # ---- Step 2: Dynamic mapping UI with dob & visit_date for all formats ----
  output$mapping_ui <- renderUI({
    # NOTE: the three format branches below (long / semiwide / wide) each define
    # inputs with the SAME ids (male_val, female_val, and the parameter/unit
    # fields). That is safe ONLY because this is a single renderUI with a
    # mutually-exclusive if/else on fmt() -- exactly one branch is ever in the
    # DOM, so the shared ids never collide. Do NOT move these panels into
    # conditionalPanel() or otherwise render more than one format's panel at
    # once: duplicate ids would then bind ambiguously and input$male_val (etc.)
    # could read the wrong field.
    req(fmt(), rv$raw_df)
    df <- rv$raw_df
    validate(need(!is.null(df), "Please load a data file in Step 1 first."))

    cols <- names(df)
    # Column-name mapping input = a dropdown of the actual columns, pre-selected
    # to the canonical gc name if that column is present, else blank (blank => the
    # user must pick one; a blank required field errors on Apply Mapping).
    # `optional = TRUE` labels the blank choice "(None)" (an explicit "I don't have
    # / don't want this column" that maps to "" -> skipped downstream); required
    # fields keep "(choose column)", whose blank value errors on Apply Mapping.
    # allowEmptyOption lists "(None)" in the dropdown, so a pre-selected optional
    # column can be un-chosen; selectize otherwise treats it as placeholder text only.
    # `canon` may list several exact names; the first one present is pre-selected.
    first_present <- function(canon, avail) {
      hit <- intersect(canon, avail)
      if (length(hit)) hit[1] else ""
    }
    blank_sel <- function(id, label, choices, selected, optional) {
      if (optional) {
        selectizeInput(id, label, choices = choices, selected = selected,
                       options = list(allowEmptyOption = TRUE))
      } else {
        selectInput(id, label, choices = choices, selected = selected)
      }
    }
    col_sel <- function(id, label, canon, optional = FALSE) {
      blank <- stats::setNames("", if (optional) "(None)" else "(choose column)")
      blank_sel(id, label, c(blank, stats::setNames(cols, cols)),
                first_present(canon, cols), optional)
    }
    # Wide per-visit prefixes: a dropdown of the prefixes actually detected in the
    # data (columns like age_1/age_2 -> prefix "age"), pre-selected to the
    # canonical prefix if present, else blank. `optional` behaves as in col_sel().
    wide_prefixes <- unique(sub("_[0-9]+$", "", grep("_[0-9]+$", cols, value = TRUE)))
    pre_sel <- function(id, label, canon, optional = FALSE) {
      blank <- stats::setNames("", if (optional) "(None)" else "(choose prefix)")
      blank_sel(id, label, c(blank, stats::setNames(wide_prefixes, wide_prefixes)),
                first_present(canon, wide_prefixes), optional)
    }
    # Exact standard names pre-selected for each measurement (metric first), and
    # the matching unit. The unit is set here as well as by the auto-detect
    # observers because those only fire when the selected name changes, which it
    # may not when this panel is redrawn; a stale "kg" on a pounds column would
    # silently convert nothing.
    canon_wt <- c("WEIGHTKG", "WEIGHTLBS")
    canon_ht <- c("HEIGHTCM", "HEIGHTIN")
    canon_hc <- c("HEADCM", "HEADIN")
    unit_sel <- function(id, label, canon, avail, units) {
      selectInput(id, label, choices = units,
                  selected = if (identical(first_present(canon, avail), canon[2])) units[2] else units[1])
    }

    if (fmt() == "long") {
      wellPanel(
        h4("Long Format Column Mapping"),
        p("One row per measurement. Choose the column in your data for each field:"),
        fluidRow(
          column(6, col_sel("long_subjid", "Patient Subject ID", "subjid")),
          column(6, col_sel("long_id", "Observation ID (optional)", "id", optional = TRUE))
        ),
        hr(),
        h4("Age"),
        p(style = "font-size: 12px; color: #666;",
          "Must have age in days OR date of birth and date of visit. If you have both, growthcleanr will use age in days."),
        fluidRow(
          column(6, col_sel("long_agedays", "Age (days) column", "agedays", optional = TRUE))
        ),
        fluidRow(
          column(6, col_sel("long_dob", "Date of birth column (used if present)", "dob", optional = TRUE)),
          column(6, col_sel("long_visit_date", "Visit date column (used if present)", "visit_date", optional = TRUE))
        ),
        fluidRow(
          column(12, date_order_input("long_date_order"), uiOutput("date_samples"))
        ),
        hr(),
        h4("Sex"),
        fluidRow(
          column(6, col_sel("long_sex", "Sex", "sex"))
        ),
        h5("Sex value coding"),
        p("Specify the values in your 'sex' column that represent male and female:"),
        fluidRow(
          column(6, textInput("male_val", "Male value (e.g., 0, M, Male, B, or Boy)", "0")),
          column(6, textInput("female_val", "Female value (e.g., 1, F, Female, G, or Girl)", "1"))
        ),
        hr(),
        h4("Parameters and measurements"),
        p("Identify the parameter and measurement columns, then specify the values in your 'parameter' column and their units:"),
        fluidRow(
          column(6, col_sel("long_param", "Parameter column to indicate measurement type (height, weight, or head circumference)", "param")),
          column(6, col_sel("long_measurement", "Measurement column: numeric value of height, weight or head circumference", "measurement"))
        ),
        unit_caption("parameter value"),
        fluidRow(
          column(4, textInput("val_weight", "Weight parameter value (e.g., WEIGHT)", "WEIGHT")),
          column(4, selectInput("unit_weight", "Weight unit", choices = c("kg", "lbs"), selected = "kg"))
        ),
        # One linear parameter value + one linear unit, always shown. In the
        # default (age-based) mode this is the only linear input. When the panel
        # box is checked, a Length parameter value is added below and shares this
        # same "Length/height unit".
        fluidRow(
          column(4, textInput("val_height", "Height parameter value (e.g., HEIGHT)", "HEIGHT")),
          # Label starts "Height unit"; an observer switches it to "Length/height
          # unit" when the assign-by-parameter-value box is checked.
          column(4, selectInput("unit_height", "Height unit", choices = c("cm", "in"), selected = "cm"))
        ),
        fluidRow(
          column(4, textInput("val_head", "Head circumference parameter value (optional)", "")),
          column(4, selectInput("unit_head", "Head circumference unit", choices = c("cm", "in"), selected = "cm"))
        ),
        length_height_panel(),
        hr(),
        keep_only_checkbox("long_keep_only")
      )

    } else if (fmt() == "semiwide") {
      wellPanel(
        h4("Semi-Wide Format Column Mapping"),
        p("One row per visit. Each measurement type (weight, height, head circumference) is in its own column. Choose the column in your data for each field:"),
        fluidRow(
          column(6, col_sel("sw_subjid", "Subject ID column", "subjid")),
          column(6, col_sel("sw_id", "Observation ID (optional)", "", optional = TRUE))
        ),
        hr(),
        h4("Age"),
        p(style = "font-size: 12px; color: #666;",
          "Must have age in days OR date of birth and date of visit. If you have both, growthcleanr will use age in days."),
        fluidRow(
          column(6, col_sel("sw_agedays", "Age (days) column", "agedays", optional = TRUE))
        ),
        fluidRow(
          column(6, col_sel("sw_dob", "Date of birth column (used if present)", "dob", optional = TRUE)),
          column(6, col_sel("sw_visit_date", "Visit date column (used if present)", "visit_date", optional = TRUE))
        ),
        fluidRow(
          column(12, date_order_input("sw_date_order"), uiOutput("date_samples"))
        ),
        hr(),
        h4("Sex"),
        fluidRow(
          column(6, col_sel("sw_sex", "Sex column", "sex"))
        ),
        h5("Sex value coding"),
        fluidRow(
          column(6, textInput("male_val", "Male value (e.g., 0, M, Male, B, or Boy)", "0")),
          column(6, textInput("female_val", "Female value (e.g., 1, F, Female, G, or Girl)", "1"))
        ),
        hr(),
        h4("Parameters and measurements"),
        p("Choose each measurement's column and its units. growthcleanr converts imperial units (lbs, in) to metric internally."),
        unit_caption("column name"),
        fluidRow(
          column(4, col_sel("sw_weight", "Weight column (e.g., WEIGHTKG)", canon_wt, optional = TRUE)),
          column(4, unit_sel("sw_unit_weight", "Weight unit", canon_wt, cols, c("kg", "lbs")))
        ),
        fluidRow(
          column(4, col_sel("sw_height", "Height column (e.g., HEIGHTCM)", canon_ht, optional = TRUE)),
          column(4, unit_sel("sw_unit_height", "Height unit", canon_ht, cols, c("cm", "in")))
        ),
        fluidRow(
          column(4, col_sel("sw_head", "Head circumference column (optional)", canon_hc, optional = TRUE)),
          column(4, unit_sel("sw_unit_head", "Head circumference unit", canon_hc, cols, c("cm", "in")))
        ),
        hr(),
        h4("Output"),
        checkboxInput("sw_output_long", "Return output in long format (default: semi-wide)", FALSE),
        p(style = "font-size: 12px; color: #666;",
          "In the app, the unit_error_range and mean_ht columns are returned only with long format output to prevent very wide returned datasets."),
        hr(),
        keep_only_checkbox("sw_keep_only")
      )

    } else if (fmt() == "wide") {
      wellPanel(
        h4("Wide Format Column Mapping"),
        p("One row per subject. Multiple visits are stored as numbered columns (e.g., agedays_1, agedays_2, WEIGHTKG_1, WEIGHTKG_2). Choose the column name prefixes (without visit numbers):"),
        fluidRow(
          column(6, col_sel("w_subjid", "Subject ID column", "subjid"))
        ),
        p(style = "font-size: 12px; color: #666;",
          "Note: long and semi-wide formats allow an optional observation ID column; wide format does not."),
        hr(),
        h4("Age"),
        p(style = "font-size: 12px; color: #666;",
          "Must have age in days OR date of birth and date of visit. If you have both, growthcleanr will use age in days."),
        fluidRow(
          column(6, pre_sel("w_agedays_prefix", "Age (days) prefix (e.g., agedays for agedays_1, agedays_2)", "agedays", optional = TRUE))
        ),
        fluidRow(
          column(6, col_sel("w_dob", "Date of birth column (used if present)", "dob", optional = TRUE)),
          column(6, pre_sel("w_visit_date", "Visit date prefix (used with date of birth, e.g., visit_date_1, visit_date_2)", "visit_date", optional = TRUE))
        ),
        fluidRow(
          column(12, date_order_input("w_date_order"), uiOutput("date_samples"))
        ),
        hr(),
        h4("Sex"),
        fluidRow(
          column(6, col_sel("w_sex", "Sex column", "sex"))
        ),
        h5("Sex value coding"),
        fluidRow(
          column(6, textInput("male_val", "Male value (e.g., 0, M, Male, B, or Boy)", "0")),
          column(6, textInput("female_val", "Female value (e.g., 1, F, Female, G, or Girl)", "1"))
        ),
        hr(),
        h4("Parameters and measurements"),
        p("Choose each measurement's column-name prefix and its units. growthcleanr converts imperial units (lbs, in) to metric internally."),
        unit_caption("prefix"),
        fluidRow(
          column(4, pre_sel("w_weight_prefix", "Weight prefix (e.g., WEIGHTKG)", canon_wt, optional = TRUE)),
          column(4, unit_sel("w_unit_weight", "Weight unit", canon_wt, wide_prefixes, c("kg", "lbs")))
        ),
        fluidRow(
          column(4, pre_sel("w_height_prefix", "Height prefix (e.g., HEIGHTCM)", canon_ht, optional = TRUE)),
          column(4, unit_sel("w_unit_height", "Height unit", canon_ht, wide_prefixes, c("cm", "in")))
        ),
        fluidRow(
          column(4, pre_sel("w_head_prefix", "Head circumference prefix (optional)", canon_hc, optional = TRUE)),
          column(4, unit_sel("w_unit_head", "Head circumference unit", canon_hc, wide_prefixes, c("cm", "in")))
        ),
        hr(),
        h4("Output"),
        checkboxInput("w_output_long", "Return output in long format (default: wide)", FALSE),
        p(style = "font-size: 12px; color: #666;",
          "In the app, the unit_error_range and mean_ht columns are returned only with long format output to prevent very wide returned datasets."),
        hr(),
        keep_only_checkbox("w_keep_only")
      )
    }
  })

  # ---- Step 2: "Check your key columns" value tables + sex-coding auto-detect ----
  # Which columns the user has currently mapped to sex / param (long only),
  # tracking the Step-2 dropdowns for the active format.
  mapped_sex_col <- reactive({
    switch(fmt(), long = input$long_sex, semiwide = input$sw_sex, wide = input$w_sex, "")
  })
  mapped_param_col <- reactive({
    if (identical(fmt(), "long")) input$long_param else ""
  })

  # Frequency table (value + count) for a column: top `n` values by count with a
  # "+ N more" note. Returns a data.frame with an integer `more` attribute.
  value_freq <- function(col, n = 10) {
    if (is.null(col) || !nzchar(col) || is.null(rv$raw_df) || !col %in% names(rv$raw_df)) return(NULL)
    tab <- sort(table(rv$raw_df[[col]], useNA = "ifany"), decreasing = TRUE)
    nm  <- names(tab); nm[is.na(nm)] <- "(blank/NA)"
    d   <- data.frame(Value = nm, Count = as.integer(tab), stringsAsFactors = FALSE)
    more <- if (nrow(d) > n) nrow(d) - n else 0L
    if (more > 0) d <- d[seq_len(n), , drop = FALSE]
    attr(d, "more") <- more
    d
  }

  # Small HTML table from a data.frame (avoids nesting renderTable in renderUI).
  df_to_html_table <- function(d) {
    tags$table(class = "table table-condensed", style = "width:auto; font-size:13px; margin-top:4px;",
      tags$thead(tags$tr(lapply(names(d), function(h) tags$th(h)))),
      tags$tbody(lapply(seq_len(nrow(d)), function(i)
        tags$tr(lapply(d[i, ], function(x) tags$td(as.character(x)))))))
  }

  values_ui <- function(col, noun) {
    if (is.null(col) || !nzchar(col)) {
      return(helpText(sprintf("Map your %s column above to see its values here.", noun)))
    }
    d <- value_freq(col)
    if (is.null(d)) return(helpText(sprintf("Column '%s' was not found in your data.", col)))
    more <- attr(d, "more"); attr(d, "more") <- NULL
    tagList(
      tags$b(sprintf("Values in your %s column (%s):", noun, col)),
      df_to_html_table(d),
      if (more > 0) helpText(sprintf("+ %d more distinct value(s) not shown.", more))
    )
  }

  output$sex_values_ui <- renderUI({ values_ui(mapped_sex_col(), "sex") })
  output$param_values_ui <- renderUI({
    if (!identical(fmt(), "long")) {
      return(helpText("Parameter values are shown for long format only (semi-wide / wide identify each parameter by column, not by a value in one column)."))
    }
    values_ui(mapped_param_col(), "parameter")
  })

  # Sex-coding auto-detect: when the mapped sex column changes, prefill the
  # male/female coding fields if `.detect_sex_coding()` recognizes the column's
  # values (see that top-level helper). Fires only on a column change, so a
  # user's manual edits to the coding fields are not clobbered.
  observeEvent(mapped_sex_col(), {
    col <- mapped_sex_col()
    if (is.null(col) || !nzchar(col) || is.null(rv$raw_df) || !col %in% names(rv$raw_df)) return()
    hit <- .detect_sex_coding(rv$raw_df[[col]])
    if (!is.null(hit)) {
      updateTextInput(session, "male_val",   value = unname(hit[["male"]]))
      updateTextInput(session, "female_val", value = unname(hit[["female"]]))
    }
  })

  # One sample date per mapped date column, under the Date format field, so the
  # format can be chosen without opening the data file. Only one format's panel
  # is ever in the DOM, so all three share this output id.
  output$date_samples <- renderUI({
    req(rv$raw_df)
    df <- rv$raw_df
    picks <- switch(fmt(),
      long     = list(input$long_dob, input$long_visit_date),
      semiwide = list(input$sw_dob, input$sw_visit_date),
      wide     = list(input$w_dob, if (isTruthy(input$w_visit_date)) .gc_app_block_cols(names(df), input$w_visit_date)))
    samples <- unlist(lapply(picks, function(cols) .gc_app_date_sample(df, cols)))
    if (!length(samples)) return(NULL)
    p(style = "font-size: 12px; color: #666;",
      "Sample dates from your file: ", tags$b(paste(samples, collapse = "; "), .noWS = "outside"), ". ",
      "These show how the dates are stored in the file. Excel and other spreadsheet programs may display dates differently, so choose the format that matches these samples.")
  })

  # ---- Step 2: Apply mapping (with dob/visit_date for all formats) ----
  observeEvent(input$map_btn, {
    clear_results()
    req(rv$raw_df)
    fm <- fmt()
    df <- rv$raw_df

    map_result <- tryCatch({

      if (fm == "long") {
        # Required columns: stop with a clear message if any is missing.
        need_col(df, input$long_subjid,      "Patient Subject ID")
        need_col(df, input$long_sex,         "Sex")
        need_col(df, input$long_param,       "Parameter column")
        need_col(df, input$long_measurement, "Measurement column")
        # Reject mapping the same source column to two fields, and a measurement
        # column that isn't numeric.
        dm <- .gc_app_dup_role_error(c(
          "Patient Subject ID" = input$long_subjid, "Sex" = input$long_sex,
          "Parameter" = input$long_param, "Measurement" = input$long_measurement,
          "Age (days)" = input$long_agedays, "Observation ID" = input$long_id,
          "Date of birth" = input$long_dob, "Visit date" = input$long_visit_date))
        if (!is.null(dm)) stop(dm, call. = FALSE)
        mm <- .gc_app_measurement_error(df, input$long_measurement)
        if (!is.null(mm)) stop(mm, call. = FALSE)
        # The data keeps the user's column names; `roles` records which column
        # fills each role, and is passed to growthcleanr so results keep them too.
        # Optional / auto-detected columns: used if present; a wrong non-default
        # name errors, a non-matching default is skipped (see role_opt).
        roles <- list(
          subjid      = input$long_subjid,
          sex         = input$long_sex,
          param       = input$long_param,
          measurement = input$long_measurement,
          agedays     = role_opt(df, input$long_agedays,    "agedays",    "Age (days) column"),
          id          = role_opt(df, input$long_id,         "id",         "Observation ID column"),
          dob         = role_opt(df, input$long_dob,        "dob",        "Date of birth column"),
          visit_date  = role_opt(df, input$long_visit_date, "visit_date", "Visit date column"))
        check_age_source(roles)
        list(df = df, roles = roles)

      } else if (fm == "semiwide") {
        # The measurement columns are routed to the unit-appropriate canonical
        # parameter (WEIGHTKG/WEIGHTLBS, HEIGHTCM/HEIGHTIN, HEADCM/HEADIN) via
        # semiwide_cols in Step 3, so gc can convert imperial units.
        need_col(df, input$sw_subjid, "Subject ID column")
        need_col(df, input$sw_sex,    "Sex column")
        dm <- .gc_app_dup_role_error(c(
          "Subject ID" = input$sw_subjid, "Sex" = input$sw_sex, "Age (days)" = input$sw_agedays,
          "Observation ID" = input$sw_id, "Date of birth" = input$sw_dob, "Visit date" = input$sw_visit_date,
          "Weight" = input$sw_weight, "Height" = input$sw_height, "Head circumference" = input$sw_head))
        if (!is.null(dm)) stop(dm, call. = FALSE)
        roles <- list(
          subjid     = input$sw_subjid,
          sex        = input$sw_sex,
          agedays    = role_opt(df, input$sw_agedays,    "agedays",    "Age (days) column"),
          id         = role_opt(df, input$sw_id,         "",           "Observation ID column"),
          dob        = role_opt(df, input$sw_dob,        "dob",        "Date of birth column"),
          visit_date = role_opt(df, input$sw_visit_date, "visit_date", "Visit date column"))
        # At least one measurement column (weight/height/head) must name a real
        # column; otherwise gc later fails with a cryptic "semiwide_cols must be
        # a named list". A non-matching default (e.g. WEIGHTKG on a weight-only
        # file) is ignored -- only an all-miss is an error.
        meas_named   <- c(input$sw_weight, input$sw_height, input$sw_head)
        meas_present <- meas_named[nzchar(meas_named) & meas_named %in% names(df)]
        if (length(meas_present) == 0) {
          stop(sprintf(
            "None of the measurement columns you entered were found in your data (Weight: '%s', Height: '%s', Head: '%s'). Your columns are: %s. In Step 2, set the Weight and Height column fields to your actual column names and choose the matching units.",
            input$sw_weight, input$sw_height, input$sw_head,
            paste(names(df), collapse = ", ")), call. = FALSE)
        }
        mm <- .gc_app_measurement_error(df, meas_present)
        if (!is.null(mm)) stop(mm, call. = FALSE)
        check_age_source(roles)
        list(df = df, roles = roles)

      } else if (fm == "wide") {
        # Wide input: subjid/sex/dob are single columns; agedays and visit_date
        # are per-visit column *prefixes* and are passed to
        # cleangrowth() by prefix. growthcleanr mints ids for wide
        # input, so no id column is used. Age presence (agedays_N / visit_date_N
        # blocks) is validated by gc at run time, with clear wide-specific errors.
        need_col(df, input$w_subjid, "Subject ID column")
        need_col(df, input$w_sex,    "Sex column")
        # Single-column fields and prefix fields live in separate namespaces, so
        # check each group for a source mapped to two fields.
        dmc <- .gc_app_dup_role_error(c(
          "Subject ID" = input$w_subjid, "Sex" = input$w_sex, "Date of birth" = input$w_dob))
        if (!is.null(dmc)) stop(dmc, call. = FALSE)
        dmp <- .gc_app_dup_role_error(c(
          "Age (days) prefix" = input$w_agedays_prefix, "Visit date prefix" = input$w_visit_date,
          "Weight prefix" = input$w_weight_prefix, "Height prefix" = input$w_height_prefix,
          "Head circumference prefix" = input$w_head_prefix))
        if (!is.null(dmp)) stop(dmp, call. = FALSE)
        roles <- list(
          subjid = input$w_subjid,
          sex    = input$w_sex,
          dob    = role_opt(df, input$w_dob, "dob", "Date of birth column"))
        # At least one weight/height prefix must have numbered `<prefix>_N`
        # columns, or gc fails at run time ("no <prefix>_N columns"). Catch it
        # here so the message points at Step 2. A non-matching default is
        # ignored as long as one measurement prefix resolves.
        w_prefixes <- c(input$w_weight_prefix, input$w_height_prefix, input$w_head_prefix)
        if (!any(vapply(w_prefixes, function(p) has_blocks(names(df), p), logical(1)))) {
          stop(sprintf(
            "None of the measurement prefixes you entered have numbered per-visit columns (expected e.g. '%s_1', '%s_2'). Weight prefix: '%s', Height prefix: '%s', Head prefix: '%s'. Your columns are: %s. In wide format each measurement is split into columns named <prefix>_1, <prefix>_2, ...; set the prefix fields to match, or use long / semi-wide format if your data isn't laid out that way.",
            input$w_weight_prefix, input$w_weight_prefix,
            input$w_weight_prefix, input$w_height_prefix, input$w_head_prefix,
            paste(names(df), collapse = ", ")), call. = FALSE)
        }
        # Every per-visit measurement block column must be numeric.
        wmeas <- unlist(lapply(c(input$w_weight_prefix, input$w_height_prefix, input$w_head_prefix),
                               function(p) .gc_app_block_cols(names(df), p)), use.names = FALSE)
        mm <- .gc_app_measurement_error(df, wmeas)
        if (!is.null(mm)) stop(mm, call. = FALSE)
        # Validate the age source at mapping time, so the error points at Step 2
        # rather than the cleaning step. Wide age is valid if the age-days prefix has numbered
        # per-visit columns, OR a date-of-birth column is present together with a
        # visit-date prefix that has blocks. Catches a wrong/blank age prefix here
        # (e.g. columns named age_1/age_2 while the prefix field still says
        # "agedays") so the message points at Step 2, not the cleaning step.
        if (!(has_blocks(names(df), input$w_agedays_prefix) ||
              (!is.null(roles$dob) && has_blocks(names(df), input$w_visit_date)))) {
          stop(sprintf(
            "No age source found for wide format. The Age (days) prefix '%s' has no numbered per-visit columns (expected '%s_1', '%s_2', ...), and no date-of-birth column plus visit-date prefix were found either. Your columns are: %s. Set the Age (days) prefix to match your data (for example 'age' if your columns are age_1, age_2, ...), or provide a date-of-birth column plus a visit-date prefix.",
            input$w_agedays_prefix, input$w_agedays_prefix, input$w_agedays_prefix,
            paste(names(df), collapse = ", ")), call. = FALSE)
        }
        list(df = df, roles = roles)
      }

    }, error = function(e) {
      showNotification(paste("There's a problem with your Step 2 choices:", e$message), type = "error", duration = 15)
      NULL
    })

    # Only report success (and store the result) when the mapping actually
    # succeeded. On error `map_result` is NULL: clear any prior mapping so Step 3
    # cannot silently run on stale/absent data, and do NOT show the success
    # message.
    if (is.null(map_result)) {
      rv$mapped_df <- NULL
      rv$mapped_roles <- NULL
      return()
    }
    mapped <- map_result$df
    roles  <- map_result$roles

    # Optional "keep only needed columns" trim (default off; per-format checkbox).
    # Applied here so the mapped-data preview, the quick test, and the full run
    # all use the trimmed data. Leaving it off carries every column through.
    keep_id <- switch(fm, long = "long_keep_only", semiwide = "sw_keep_only", wide = "w_keep_only")
    dropped <- character(0)
    if (isTRUE(input[[keep_id]])) {
      keep    <- needed_cols(fm, mapped, roles)
      dropped <- setdiff(names(mapped), keep)
      mapped  <- mapped[, keep, drop = FALSE]
    }

    rv$mapped_df <- mapped
    rv$mapped_roles <- roles
    showNotification("✓ Mapping applied successfully", type = "message", duration = 3)
    if (length(dropped) > 0) {
      showNotification(
        sprintf("Kept the %d column(s) cleaning needs and dropped %d other column(s) (they won't be in the results file): %s",
                ncol(mapped), length(dropped), paste(dropped, collapse = ", ")),
        type = "message", duration = 8)
    }

    # Informational: flag columns that look like growthcleanr measurements but
    # weren't mapped (e.g. a HEADCM column with the Head field left blank) --
    # they'd be silently ignored otherwise.
    unmapped <- detect_unmapped_params(fm, rv$raw_df)
    if (length(unmapped) > 0) {
      sticky_note(
        paste0("Note: your file has column(s) that look like growthcleanr measurements but weren't mapped, so they will be ignored: ",
               paste(unmapped, collapse = ", "),
               ". If you want them cleaned, add them in Step 2 (e.g. the Head circumference field)."),
        type = "warning")
    }

    # Warn about blank Subject IDs: growthcleanr groups all blank-ID rows into a
    # single subject, which usually mis-groups data with no error.
    sj <- mapped[[roles$subjid]]
    if (!is.null(sj)) {
      nblank <- sum(is.na(sj) | !nzchar(trimws(as.character(sj))))
      if (nblank > 0) {
        sticky_note(
          sprintf("Warning: %d row(s) have a blank Subject ID. Rows with no Subject ID can't be assigned to a subject: missing IDs are set aside and not cleaned, and empty-text IDs (i.e., empty but not missing) would all be grouped as one subject. Check your Subject ID column.", nblank),
          type = "warning")
      }
    }
  })

  output$mapped_preview <- renderDT({
    req(rv$mapped_df)
    datatable(head(rv$mapped_df, 10), options = list(scrollX = TRUE, pageLength = 10))
  })

  # ---- Step 3: check growthcleanr version ----
  observeEvent(input$install_btn, {
    msg <- tryCatch({
      if (!requireNamespace("growthcleanr", quietly = TRUE)) {
        stop("growthcleanr does not appear to be installed. Open the Troubleshooting section at the top of the app for how to install it, then restart R and try again.")
      }
      library(growthcleanr)
      paste0("✓ growthcleanr loaded successfully (version ", as.character(utils::packageVersion("growthcleanr")), ").")
    }, error = function(e) paste("❌ growthcleanr could not be loaded:", conditionMessage(e),
                                 "— see the Troubleshooting section at the top of the app."))
    rv$install_msg <- msg
  })

  output$install_status <- renderText({ rv$install_msg })

  # ---- Step 3a: Run gc_runtrial() on a 100-record sample (using mapped column names) ----
  observeEvent(input$trial_btn, {
    # No stored mapping means Step 2 was never applied, or it errored. Tell the
    # user instead of halting silently (req() would just do nothing).
    if (is.null(rv$mapped_df)) {
      showNotification("❌ Please apply a valid column mapping in Step 2 first (check for a mapping error there).",
                       type = "error", duration = 6)
      return()
    }

    # Validate that mapping was applied
    fm <- fmt()
    if (fm == "long" && is.null(rv$mapped_roles$param)) {
      showNotification("❌ Please click 'Apply Mapping' in Step 2 first",
                       type = "error", duration = 5)
      return()
    }
    clear_sticky()

    withProgress(message = 'Running quick test...', value = 0, {
      df <- rv$mapped_df
      roles <- rv$mapped_roles

      # gc_runtrial() samples whole subjects internally (default 100 patients),
      # which keeps each patient's trajectory intact. Pass the full mapped data
      # and let it do the sampling -- do NOT pre-slice random rows, which would
      # scatter measurements across many patients and make the trial meaningless.
      sample_df <- df

      incProgress(0.3, detail = "Preparing data")

      trial_output <- tryCatch({
        incProgress(0.5, detail = "Running the quick test")

        if (fm == "long") {
          param_coding_list <- long_param_coding()

          has_dates <- !is.null(roles$dob) && !is.null(roles$visit_date)
          captured <- capture.output({
            do.call(gc_runtrial, c(list(
              data           = sample_df,
              subjid         = roles$subjid,
              param          = roles$param,
              agedays        = roles$agedays,
              sex            = roles$sex,
              measurement    = roles$measurement,
              id             = roles$id,
              dob            = if (has_dates) roles$dob else NULL,
              visit_date     = if (has_dates) roles$visit_date else NULL,
              param_coding  = if (length(param_coding_list) > 0) param_coding_list else NULL
            ), sex_coding_arg(), date_order_arg(input$long_date_order),
               length_adjust_arg("long_length_adjust")))
          })
        } else if (fm == "semiwide") {
          # Build semiwide_cols mapping the unit-appropriate canonical parameter
          # -> the user's real (unrenamed) measurement column name, so gc converts
          # imperial units when selected.
          semiwide_cols_list <- semiwide_cols_arg(sample_df)

          has_dates <- !is.null(roles$dob) && !is.null(roles$visit_date)
          captured <- capture.output({
            do.call(gc_runtrial, c(list(
              data           = sample_df,
              subjid         = roles$subjid,
              sex            = roles$sex,
              agedays        = if (is.null(roles$agedays)) "agedays" else roles$agedays,
              id             = roles$id,
              dob            = if (has_dates) roles$dob else NULL,
              visit_date     = if (has_dates) roles$visit_date else NULL,
              input_format   = "semi_wide",
              semiwide_cols  = semiwide_cols_list
            ), sex_coding_arg(), date_order_arg(input$sw_date_order)))
          })
        } else if (fm == "wide") {
          # wide_cols maps the unit-appropriate canonical gc parameter -> the
          # user's column-name prefix. Keys MUST be gc parameter names
          # (metric WEIGHTKG/HEIGHTCM/HEADCM or imperial WEIGHTLBS/HEIGHTIN/HEADIN),
          # not the user's prefix; gc converts imperial units internally.
          wide_cols_list <- wide_cols_arg()

          # visit_date is a per-visit prefix; pass it only alongside a
          # date-of-birth column (gc needs both to use the date route).
          w_has_dob <- !is.null(roles$dob)
          captured <- capture.output({
            do.call(gc_runtrial, c(list(
              data         = sample_df,
              subjid       = roles$subjid,
              sex          = roles$sex,
              agedays      = input$w_agedays_prefix,
              dob          = roles$dob,
              visit_date   = if (w_has_dob && nzchar(input$w_visit_date)) input$w_visit_date else NULL,
              input_format = "wide",
              wide_cols    = wide_cols_list
            ), sex_coding_arg(), date_order_arg(input$w_date_order)))
          })
        }

        incProgress(1, detail = "Complete")
        paste(captured, collapse = "\n")

      }, error = function(e) {
        paste("❌ Test trial failed:", conditionMessage(e))
      })

      if (grepl("date_order", trial_output, fixed = TRUE))
        trial_output <- paste0(trial_output, "\n\n", date_order_hint, ".")
      rv$trial_report <- trial_output
    })

    # Prominent pass/fail pop-up. gc_runtrial() reports a cleaning/config stop
    # INSIDE its own text (it does not throw), so a failed trial otherwise showed
    # a misleading "complete" message with the error buried in the report. Detect
    # failure from the report's markers (or our own thrown-error prefix) and show
    # a persistent error pop-up that points the user at the report.
    rpt <- if (is.null(rv$trial_report)) "" else rv$trial_report
    failed <- grepl("TRIAL RUN FAILED", rpt, fixed = TRUE) ||
              grepl("COULD NOT RUN",    rpt, fixed = TRUE) ||
              grepl("Test trial failed:", rpt, fixed = TRUE)
    if (failed) {
      sticky_note(
        "❌ QUICK TEST FAILED — growthcleanr could not clean the sample. See the quick test report below (look for 'TRIAL RUN FAILED' / 'COULD NOT RUN' and the error).",
        type = "error")
    } else {
      showNotification("✓ Quick test successful", type = "message", duration = 4)
    }
  })

  output$trial_report <- renderText({
    if (is.null(rv$trial_report)) {
      "Click 'Run quick test' to generate a report on a sample of up to 100 subjects."
    } else {
      rv$trial_report
    }
  })

  # ---- Step 3b: Run cleangrowth() (with dob/visit_date included) ----
  observeEvent(input$run_btn, {
    if (is.null(rv$mapped_df)) {
      showNotification("❌ Please apply a valid column mapping in Step 2 first (check for a mapping error there).",
                       type = "error", duration = 6)
      return()
    }
    # A failed run must not leave the prior run's results or notes in place.
    clear_sticky()
    rv$output_df   <- NULL
    rv$exclude_tab <- NULL
    fm <- fmt()
    df <- rv$mapped_df
    roles <- rv$mapped_roles
    # growthcleanr turns on temporary checkpointing by itself above 5,000 subjects
    # and removes the files when the run ends. Those runs print per-checkpoint
    # progress, which drives the progress bar below.
    run_quietly <- data.table::uniqueN(df[[roles$subjid]]) <= 5000
    # Use multiple cores for the full run. parallel = TRUE lets gc auto-detect the
    # worker count (min(8, detectCores() - 1)) and forward it into checkpoints
    # too, so this speeds up both small and large (checkpointed) files; a single-core
    # machine stays on one worker. (The 100-patient trial run is left sequential --
    # too small to benefit from a cluster, whose ~2-5s startup would dominate.)

    withProgress(message = 'Cleaning your data...', value = 0, {

      # Collect growthcleanr's warnings so they can be shown to the user.
      # gc writes genuinely useful guidance to warnings (e.g. "Unusable data:
      # ... map them via param_coding", "No height measurements found"), which
      # otherwise goes only to the R console the app user never sees.
      run_warnings <- character(0)
      combined <- withCallingHandlers(tryCatch({

        if (fm == "long") {
          incProgress(0.3, detail = "Processing long format")

          param_coding_list <- long_param_coding()

          has_dates <- !is.null(roles$dob) && !is.null(roles$visit_date)
          gcr_out <- do.call(growthcleanr::cleangrowth, c(list(
            data = df,
            subjid = roles$subjid,
            param = roles$param,
            agedays = roles$agedays,
            sex = roles$sex,
            measurement = roles$measurement,
            id = roles$id,
            dob = if (has_dates) roles$dob else NULL,
            visit_date = if (has_dates) roles$visit_date else NULL,
            param_coding = if (length(param_coding_list) > 0) param_coding_list else NULL,
            quietly = run_quietly, parallel = TRUE
          ), sex_coding_arg(), date_order_arg(input$long_date_order),
             length_adjust_arg("long_length_adjust")))
          incProgress(1, detail = "Complete")
          gcr_out

        } else if (fm == "semiwide") {
          incProgress(0.3, detail = "Processing semi-wide format")

          # Build semiwide_cols mapping the unit-appropriate canonical parameter
          # -> the user's real (unrenamed) measurement column name, so gc converts
          # imperial units when selected.
          semiwide_cols_list <- semiwide_cols_arg(df)

          has_dates <- !is.null(roles$dob) && !is.null(roles$visit_date)
          gcr_out <- do.call(growthcleanr::cleangrowth, c(list(
            data = df,
            subjid = roles$subjid,
            sex = roles$sex,
            agedays = if (is.null(roles$agedays)) "agedays" else roles$agedays,
            id = roles$id,
            dob = if (has_dates) roles$dob else NULL,
            visit_date = if (has_dates) roles$visit_date else NULL,
            input_format = "semi_wide",
            semiwide_cols = semiwide_cols_list,
            output_format = if (input$sw_output_long) "long" else NULL,
            widened_emit = c("exclude", "bin_exclude"),
            quietly = run_quietly, parallel = TRUE
          ), sex_coding_arg(), date_order_arg(input$sw_date_order)))
          incProgress(1, detail = "Complete")
          gcr_out

        } else { # wide
          incProgress(0.3, detail = "Processing wide format")

          # wide_cols maps the unit-appropriate canonical gc parameter -> the
          # user's column-name prefix. Keys MUST be gc parameter names
          # (metric WEIGHTKG/HEIGHTCM/HEADCM or imperial WEIGHTLBS/HEIGHTIN/HEADIN),
          # not the user's prefix; gc converts imperial units internally.
          wide_cols_list <- wide_cols_arg()

          w_has_dob <- !is.null(roles$dob)
          gcr_out <- do.call(growthcleanr::cleangrowth, c(list(
            data = df,
            subjid = roles$subjid,
            sex = roles$sex,
            agedays = input$w_agedays_prefix,
            dob = roles$dob,
            visit_date = if (w_has_dob && nzchar(input$w_visit_date)) input$w_visit_date else NULL,
            input_format = "wide",
            wide_cols = wide_cols_list,
            output_format = if (input$w_output_long) "long" else NULL,
            widened_emit = c("exclude", "bin_exclude"),
            quietly = run_quietly, parallel = TRUE
          ), sex_coding_arg(), date_order_arg(input$w_date_order)))
          incProgress(1, detail = "Complete")
          gcr_out
        }

      }, error = function(e) {
        msg <- sub("[.]\\s*$", "", conditionMessage(e))
        if (grepl("sex_coding", msg, fixed = TRUE))
          msg <- paste0(msg, ". In this app, enter your codes in the Step 2 'Male value' and 'Female value' fields (for example 1 and 2), click Apply Mapping, and run again")
        if (grepl("date_order", msg, fixed = TRUE))
          msg <- paste0(msg, ". ", date_order_hint)
        sticky_note(
          paste0("❌ Cleaning could not finish: ", msg,
                 ". Check the notes below and your Step 2 choices; the Troubleshooting section at the top of the app lists common causes."),
          type = "error"
        )
        NULL
      }),
      warning = function(w) {
        run_warnings <<- c(run_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        # On large (checkpointed) runs gc emits per-checkpoint progress via message()
        # (those run with quietly = FALSE). Drive the progress bar from the
        # "checkpoint N/M: done" log lines, mapped into the 0.3-0.95 band that follows
        # the "Processing ..." step so the bar advances instead of resetting.
        # All messages are muffled here so none leak to the console; small
        # (non-checkpointed) runs are quiet, so this handler is inert for them.
        txt <- conditionMessage(m)
        cm  <- regmatches(txt, regexec("checkpoint\\s+(\\d+)\\s*/\\s*(\\d+)\\s*:\\s*done",
                                       txt, ignore.case = TRUE))[[1]]
        if (length(cm) == 3L) {
          done  <- suppressWarnings(as.integer(cm[2]))
          total <- suppressWarnings(as.integer(cm[3]))
          if (!is.na(done) && !is.na(total) && total > 0L) {
            setProgress(value  = min(0.95, 0.3 + 0.65 * done / total),
                        detail = sprintf("%d / %d checkpoints cleaned", done, total))
          }
        }
        invokeRestart("muffleMessage")
      })

      rv$run_warnings <- run_warnings
      req(combined)

      # Store results
      rv$output_df <- combined

      # Generate exclude summary. Long output has a single `exclude` column;
      # widened (semi-wide / wide) output names it per parameter
      # (`exclude_WEIGHTKG`, `exclude_HEIGHTCM`, ...). Pool all of them so the
      # summary is populated for every format. The names come from the mapping,
      # so a user's own column (e.g. `exclude_reason`) is not counted.
      long_out <- switch(fm, long = TRUE, semiwide = isTRUE(input$sw_output_long),
                         wide = isTRUE(input$w_output_long))
      excl_cols <- if (long_out) {
        "exclude"
      } else if (fm == "semiwide") {
        paste0("exclude_", unlist(semiwide_cols_arg(df), use.names = FALSE))
      } else {
        paste0("exclude_", unlist(lapply(unlist(wide_cols_arg(), use.names = FALSE), function(p)
          .gc_app_block_cols(names(df), p)), use.names = FALSE))
      }
      excl_cols <- intersect(excl_cols, names(combined))
      if (length(excl_cols) > 0) {
        vals <- unlist(lapply(excl_cols, function(cn) as.character(combined[[cn]])),
                       use.names = FALSE)
        vals <- vals[!is.na(vals) & nzchar(vals)]
        rv$exclude_tab <- as.data.frame(table(vals), stringsAsFactors = FALSE)
        names(rv$exclude_tab) <- c("exclude_category", "count")
      }
    })

    # Completion message reflects how much data was usable, so a run where
    # everything was excluded (e.g. unmapped parameter values) does not read as
    # an unqualified success.
    tot <- if (!is.null(rv$exclude_tab)) sum(rv$exclude_tab$count) else 0
    inc <- if (!is.null(rv$exclude_tab)) rv$exclude_tab$count[rv$exclude_tab$exclude_category == "Include"] else 0
    if (length(inc) == 0) inc <- 0
    if (tot > 0 && inc == 0) {
      sticky_note(
        sprintf("⚠ Cleaning ran, but 0 of %d measurements were included. This is almost always a Step 2 mapping problem (the parameter values or columns weren't matched). Recheck your Step 2 choices and the notes and exclusion summary below.", tot),
        type = "warning")
    } else {
      showNotification(
        sprintf("✓ Cleaning finished: %d of %d measurements included (%s%%).",
                inc, tot, if (tot > 0) round(100 * inc / tot, 1) else 0),
        type = "message", duration = 6)
    }
  })

  # growthcleanr's own warnings, surfaced in the UI. These carry the most
  # actionable guidance (why data was set aside and how to fix mapping), which
  # otherwise would be invisible to a user running the app.
  output$run_warnings <- renderUI({
    w <- rv$run_warnings
    if (is.null(w) || length(w) == 0) return(NULL)
    div(class = "warn-box",
        tags$b("Notes/warnings:"),
        " Notes and warnings are common for messy/complex datasets. Review notes and warnings to ensure they make sense for your dataset.",
        tags$pre(style = "white-space: pre-wrap; margin-top: 8px; margin-bottom: 0;",
                 paste(w, collapse = "\n\n")))
  })

  output$exclude_stats_text <- renderText({
    req(rv$exclude_tab)
    total <- sum(rv$exclude_tab$count)
    included <- rv$exclude_tab$count[rv$exclude_tab$exclude_category == "Include"]
    if (length(included) == 0) included <- 0
    paste0("Total measurements: ", total, " | Included: ", included, " (",
           round(100 * included / total, 1), "%)")
  })

  output$exclude_summary <- renderDT({
    req(rv$exclude_tab)
    datatable(rv$exclude_tab,
              caption = "Breakdown of 'exclude' categories",
              options = list(pageLength = 15))
  })

  output$sample_output <- renderDT({
    req(rv$output_df)
    datatable(head(rv$output_df, 20),
              options = list(scrollX = TRUE, pageLength = 20))
  })

observeEvent(rv$output_df, {
  req(rv$output_df)
  showModal(modalDialog(
    title = "Processing complete",
    tagList(
      "Your results are ready. Save them with the button below.",
      tags$br(), tags$br(),
      tags$b("A 'Save results (CSV)' button also stays on the page"),
      " (under the Clean my data button), so you can save more than once, save to a second folder, or recover if a save is cancelled — closing this box does not lose your results."
    ),
    easyClose = TRUE,
    footer = tagList(
      downloadButton("download_btn", "Save CSV now", class = "btn-primary"),
      modalButton("Close (I'll use the button below)")
    )
  ))
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Both the modal button and the persistent page button write the same results;
# two ids so both can exist in the DOM at once. data.table::fwrite is much faster
# than write.csv on large outputs.
.gc_save_name    <- function() paste0("growthcleanr_output_", Sys.Date(), ".csv")
.gc_save_content <- function(file) { req(rv$output_df); data.table::fwrite(rv$output_df, file) }
output$download_btn     <- downloadHandler(filename = .gc_save_name, content = .gc_save_content)
output$download_persist <- downloadHandler(filename = .gc_save_name, content = .gc_save_content)

# Close the modal when its save button is clicked; the persistent button remains.
shinyjs::onclick("download_btn", { removeModal() })

# Persistent save button on the page -- appears whenever results exist, so a
# cancelled/mis-clicked save is always recoverable without re-running.
output$save_results_ui <- renderUI({
  req(rv$output_df)
  div(class = "next-step",
    sprintf("Results ready (%s rows). ", format(nrow(rv$output_df), big.mark = ",")),
    downloadButton("download_persist", "Save results (CSV)", class = "btn-primary"),
    tags$span(style = "font-size: 12px; color: #666;",
              "  Save again or to another folder anytime — this stays here until you run again, load a new file, or change the mapping.")
  )
})

}

shinyApp(ui, server)
