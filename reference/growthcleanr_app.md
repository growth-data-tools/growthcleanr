# Run the growthcleanr Shiny application

Launches the interactive growthcleanr Shiny interface in a local web
browser. The app runs entirely on the user's own machine: data is read
into the local R session, cleaned with
[`cleangrowth()`](https://growthcleanr.org/reference/cleangrowth.md),
and results are saved to the user's computer. Nothing is uploaded
anywhere.

## Usage

``` r
growthcleanr_app(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

This function is called for its side effect of launching the app and
does not return a value; interrupt R to stop the app.

## Details

The app needs the `shiny`, `DT`, `shinyjs`, and `shinyFiles` packages,
which growthcleanr imports, so a standard growthcleanr install is enough
to run it.

## Examples

``` r
if (FALSE) { # \dontrun{
growthcleanr_app()
} # }
```
