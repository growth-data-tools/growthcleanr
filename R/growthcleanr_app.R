#' Run the growthcleanr Shiny application
#'
#' Launches the interactive growthcleanr Shiny interface in a local web browser.
#' The app runs entirely on the user's own machine: data is read into the local
#' R session, cleaned with [cleangrowth()], and results are saved to the user's
#' computer. Nothing is uploaded anywhere.
#'
#' The app needs the `shiny`, `DT`, `shinyjs`, and `shinyFiles` packages, which
#' growthcleanr imports, so a standard growthcleanr install is enough to run it.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return This function is called for its side effect of launching the app and
#'   does not return a value; interrupt R to stop the app.
#' @importFrom shiny runApp
#' @importFrom DT datatable
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles shinyFilesButton
#' @export
#' @examples
#' \dontrun{
#' growthcleanr_app()
#' }
growthcleanr_app <- function(...) {
  # Belt-and-suspenders: these are Imports, so a normal install has them, but
  # give a clear message if a build is somehow missing one.
  needed <- c("shiny", "DT", "shinyjs", "shinyFiles")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      sprintf(
        "The growthcleanr app needs the package(s) %s. Install with install.packages(c(%s)).",
        paste(missing, collapse = ", "),
        paste(sprintf('"%s"', missing), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  app_dir <- system.file("app", package = "growthcleanr")
  if (app_dir == "") {
    stop(
      "Could not find the app directory. Try re-installing 'growthcleanr'.",
      call. = FALSE
    )
  }
  shiny::runApp(app_dir, ...)
}
