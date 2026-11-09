# Path to the growthcleanr command-line driver script

Returns the location of the installed `gcdriver.R` command-line driver,
so it can be invoked with `Rscript` without hunting for the file:

    DRIVER=$(Rscript -e 'cat(growthcleanr::gc_cli_path())')
    Rscript "$DRIVER" input.csv output.csv --quiet

## Usage

``` r
gc_cli_path()
```

## Value

The absolute path to `gcdriver.R` in the installed package.

## See also

[`run_gc_cli`](https://growthcleanr.org/reference/run_gc_cli.md);
[`vignette("command-line")`](https://growthcleanr.org/articles/command-line.md).
