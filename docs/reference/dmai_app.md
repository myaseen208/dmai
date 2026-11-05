# Monetary aggregates in a web browser

`dmai_app()` starts the application of dmai: load the components of a
monetary aggregate (the simulated example
[`money_sim`](https://myaseen208.com/dmai/reference/money_sim.md) or a
CSV file), choose the quantity and rate columns and the benchmark rate,
compute the Divisia or another index, compare all methods, inspect the
user costs, weights and Divisia second moments. Every result page shows
an interpretation computed from the results
([`dmai_interpret()`](https://myaseen208.com/dmai/reference/dmai_interpret.md)),
the output as R prints it, formatted tables, plots and the R code that
repeats the analysis, each with a Copy button.

## Usage

``` r
dmai_app(launch.browser = interactive(), ...)

dmai_app_object()
```

## Arguments

- launch.browser:

  passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html): `TRUE`
  (default in an interactive session) opens the application in the web
  browser.

- ...:

  further arguments to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), such
  as `port`.

## Value

`dmai_app()` is called for its side effect and returns the value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) when the
application is closed; `dmai_app_object()` returns the application
object (for
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html),
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html)
or deployment on a server).

## Details

The application needs the suggested packages shiny, bslib and DT. Plots
are sent to the browser as WebP images when magick is installed (smaller
than PNG; PNG otherwise) and can be downloaded as PNG, PDF or WebP. Text
is set in IBM Plex Sans and output in IBM Plex Mono when the browser can
reach Google Fonts, in the system fonts otherwise. A CSV file needs one
row per date: a date column (read as a date when it looks like one) and
a quantity and a rate column for every component.

## See also

[`dmai`](https://myaseen208.com/dmai/reference/dmai.md),
[`dmai_compare`](https://myaseen208.com/dmai/reference/dmai_compare.md),
[`dmai_interpret`](https://myaseen208.com/dmai/reference/dmai_interpret.md)

## Author

Muhammad Yaseen (<myaseen208@gmail.com>)

## Examples

``` r
if (interactive()) {
  dmai_app(
      launch.browser = TRUE
    )
}
```
