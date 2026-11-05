# Published values used by the tests and vignettes

Reads `inst/extdata/reference.csv`: one row per published number, with
its source, the R call that recomputes it with this package (`call`),
the expression that extracts the number from the result (`extract`, in
terms of `x`), the printed value and the digits to compare. The same
file drives the test of published values and the vignette that
reproduces the published examples. Each distinct `call` is evaluated
once.

## Usage

``` r
dmai_reference(source = NULL, evaluate = FALSE, level = 1)
```

## Arguments

- source:

  optional regular expression; keep rows whose `source` matches.

- evaluate:

  logical; evaluate the rows and add columns `computed` and `agrees`
  (default `FALSE`).

- level:

  largest `level` evaluated (1 = fast rows, 2 = slow rows; default 1);
  rows above it get `NA`.

## Value

A `data.table` with columns `id`, `source`, `quantity`, `call`,
`extract`, `value`, `digits`, `level`, `note` (and `computed`,
`agrees`).

## See also

Other dmai utilities:
[`dmai_notation()`](https://myaseen208.com/dmai/reference/dmai_notation.md),
[`dmai_save()`](https://myaseen208.com/dmai/reference/dmai_save.md),
[`dmai_theme()`](https://myaseen208.com/dmai/reference/dmai_theme.md)

## Author

Muhammad Yaseen (<myaseen208@gmail.com>)

## Examples

``` r
ref <-
  dmai_reference(
      source   = NULL
    , evaluate = FALSE
    , level    = 1
    )
ref
#>                     id                                     source
#>                 <char>                                     <char>
#>  1: cpi19_tornqvist_t2 ILO et al. (2004), CPI Manual, Table 19.11
#>  2: cpi19_tornqvist_t3 ILO et al. (2004), CPI Manual, Table 19.11
#>  3: cpi19_tornqvist_t4 ILO et al. (2004), CPI Manual, Table 19.11
#>  4: cpi19_tornqvist_t5 ILO et al. (2004), CPI Manual, Table 19.11
#>  5:    cpi19_fisher_t2 ILO et al. (2004), CPI Manual, Table 19.11
#>  6:    cpi19_fisher_t3 ILO et al. (2004), CPI Manual, Table 19.11
#>  7:    cpi19_fisher_t4 ILO et al. (2004), CPI Manual, Table 19.11
#>  8:    cpi19_fisher_t5 ILO et al. (2004), CPI Manual, Table 19.11
#>  9: cpi19_laspeyres_t2  ILO et al. (2004), CPI Manual, Table 19.5
#> 10: cpi19_laspeyres_t3  ILO et al. (2004), CPI Manual, Table 19.5
#> 11: cpi19_laspeyres_t4  ILO et al. (2004), CPI Manual, Table 19.5
#> 12: cpi19_laspeyres_t5  ILO et al. (2004), CPI Manual, Table 19.5
#> 13:   cpi19_paasche_t2  ILO et al. (2004), CPI Manual, Table 19.5
#> 14:   cpi19_paasche_t3  ILO et al. (2004), CPI Manual, Table 19.5
#> 15:   cpi19_paasche_t4  ILO et al. (2004), CPI Manual, Table 19.5
#> 16:   cpi19_paasche_t5  ILO et al. (2004), CPI Manual, Table 19.5
#>                                                                                                                     quantity
#>                                                                                                                       <char>
#>  1:       chained Tornqvist price index of period 2 (Divisia formula with prices as quantities and quantities as user costs)
#>  2:       chained Tornqvist price index of period 3 (Divisia formula with prices as quantities and quantities as user costs)
#>  3:       chained Tornqvist price index of period 4 (Divisia formula with prices as quantities and quantities as user costs)
#>  4:       chained Tornqvist price index of period 5 (Divisia formula with prices as quantities and quantities as user costs)
#>  5:                                                                                   chained Fisher price index of period 2
#>  6:                                                                                   chained Fisher price index of period 3
#>  7:                                                                                   chained Fisher price index of period 4
#>  8:                                                                                   chained Fisher price index of period 5
#>  9:                                                                                chained Laspeyres price index of period 2
#> 10:                                                                                chained Laspeyres price index of period 3
#> 11:                                                                                chained Laspeyres price index of period 4
#> 12:                                                                                chained Laspeyres price index of period 5
#> 13: chained Paasche price index of period 2 (14.10 / 10.20 = 1.38235; compared to 3 digits because the Manual prints 1.3823)
#> 14:                                                                                  chained Paasche price index of period 3
#> 15:                                                                                  chained Paasche price index of period 4
#> 16:                                                                                  chained Paasche price index of period 5
#>                                                                                                                                        call
#>                                                                                                                                      <char>
#>  1:               dmai(.data = cpi_ch19, method = 'Barnett', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1)
#>  2:               dmai(.data = cpi_ch19, method = 'Barnett', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1)
#>  3:               dmai(.data = cpi_ch19, method = 'Barnett', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1)
#>  4:               dmai(.data = cpi_ch19, method = 'Barnett', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1)
#>  5: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>  6: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>  7: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>  8: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>  9: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 10: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 11: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 12: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 13: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 14: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 15: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 16: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>                           extract  value digits level   note
#>                            <char>  <num>  <int> <int> <char>
#>  1:                    x$index[2] 1.4052      4     1       
#>  2:                    x$index[3] 1.3112      4     1       
#>  3:                    x$index[4] 1.2624      4     1       
#>  4:                    x$index[5] 1.2224      4     1       
#>  5:              x$index$index[2] 1.4011      4     1       
#>  6:              x$index$index[3] 1.3185      4     1       
#>  7:              x$index$index[4] 1.2689      4     1       
#>  8:              x$index$index[5] 1.2226      4     1       
#>  9: prod(x$fisher$laspeyres[2:2]) 1.4200      4     1       
#> 10: prod(x$fisher$laspeyres[2:3]) 1.3646      4     1       
#> 11: prod(x$fisher$laspeyres[2:4]) 1.3351      4     1       
#> 12: prod(x$fisher$laspeyres[2:5]) 1.3306      4     1       
#> 13:   prod(x$fisher$paasche[2:2]) 1.3823      3     1       
#> 14:   prod(x$fisher$paasche[2:3]) 1.2740      4     1       
#> 15:   prod(x$fisher$paasche[2:4]) 1.2060      4     1       
#> 16:   prod(x$fisher$paasche[2:5]) 1.1234      4     1       
# \donttest{
dmai_reference(
    source   = "."
  , evaluate = TRUE
  , level    = 1
  )
#>                     id                                     source
#>                 <char>                                     <char>
#>  1: cpi19_tornqvist_t2 ILO et al. (2004), CPI Manual, Table 19.11
#>  2: cpi19_tornqvist_t3 ILO et al. (2004), CPI Manual, Table 19.11
#>  3: cpi19_tornqvist_t4 ILO et al. (2004), CPI Manual, Table 19.11
#>  4: cpi19_tornqvist_t5 ILO et al. (2004), CPI Manual, Table 19.11
#>  5:    cpi19_fisher_t2 ILO et al. (2004), CPI Manual, Table 19.11
#>  6:    cpi19_fisher_t3 ILO et al. (2004), CPI Manual, Table 19.11
#>  7:    cpi19_fisher_t4 ILO et al. (2004), CPI Manual, Table 19.11
#>  8:    cpi19_fisher_t5 ILO et al. (2004), CPI Manual, Table 19.11
#>  9: cpi19_laspeyres_t2  ILO et al. (2004), CPI Manual, Table 19.5
#> 10: cpi19_laspeyres_t3  ILO et al. (2004), CPI Manual, Table 19.5
#> 11: cpi19_laspeyres_t4  ILO et al. (2004), CPI Manual, Table 19.5
#> 12: cpi19_laspeyres_t5  ILO et al. (2004), CPI Manual, Table 19.5
#> 13:   cpi19_paasche_t2  ILO et al. (2004), CPI Manual, Table 19.5
#> 14:   cpi19_paasche_t3  ILO et al. (2004), CPI Manual, Table 19.5
#> 15:   cpi19_paasche_t4  ILO et al. (2004), CPI Manual, Table 19.5
#> 16:   cpi19_paasche_t5  ILO et al. (2004), CPI Manual, Table 19.5
#>                                                                                                                     quantity
#>                                                                                                                       <char>
#>  1:       chained Tornqvist price index of period 2 (Divisia formula with prices as quantities and quantities as user costs)
#>  2:       chained Tornqvist price index of period 3 (Divisia formula with prices as quantities and quantities as user costs)
#>  3:       chained Tornqvist price index of period 4 (Divisia formula with prices as quantities and quantities as user costs)
#>  4:       chained Tornqvist price index of period 5 (Divisia formula with prices as quantities and quantities as user costs)
#>  5:                                                                                   chained Fisher price index of period 2
#>  6:                                                                                   chained Fisher price index of period 3
#>  7:                                                                                   chained Fisher price index of period 4
#>  8:                                                                                   chained Fisher price index of period 5
#>  9:                                                                                chained Laspeyres price index of period 2
#> 10:                                                                                chained Laspeyres price index of period 3
#> 11:                                                                                chained Laspeyres price index of period 4
#> 12:                                                                                chained Laspeyres price index of period 5
#> 13: chained Paasche price index of period 2 (14.10 / 10.20 = 1.38235; compared to 3 digits because the Manual prints 1.3823)
#> 14:                                                                                  chained Paasche price index of period 3
#> 15:                                                                                  chained Paasche price index of period 4
#> 16:                                                                                  chained Paasche price index of period 5
#>                                                                                                                                        call
#>                                                                                                                                      <char>
#>  1:               dmai(.data = cpi_ch19, method = 'Barnett', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1)
#>  2:               dmai(.data = cpi_ch19, method = 'Barnett', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1)
#>  3:               dmai(.data = cpi_ch19, method = 'Barnett', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1)
#>  4:               dmai(.data = cpi_ch19, method = 'Barnett', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1)
#>  5: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>  6: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>  7: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>  8: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>  9: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 10: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 11: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 12: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 13: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 14: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 15: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#> 16: dmai(.data = cpi_ch19, method = 'Fisher', date = 'period', x = paste0('p', 1:6), user_cost = paste0('q', 1:6), base = 1, as_dt = FALSE)
#>                           extract  value digits level   note computed agrees
#>                            <char>  <num>  <int> <int> <char>    <num> <lgcl>
#>  1:                    x$index[2] 1.4052      4     1        1.405162   TRUE
#>  2:                    x$index[3] 1.3112      4     1        1.311201   TRUE
#>  3:                    x$index[4] 1.2624      4     1        1.262440   TRUE
#>  4:                    x$index[5] 1.2224      4     1        1.222405   TRUE
#>  5:              x$index$index[2] 1.4011      4     1        1.401050   TRUE
#>  6:              x$index$index[3] 1.3185      4     1        1.318509   TRUE
#>  7:              x$index$index[4] 1.2689      4     1        1.268915   TRUE
#>  8:              x$index$index[5] 1.2226      4     1        1.222610   TRUE
#>  9: prod(x$fisher$laspeyres[2:2]) 1.4200      4     1        1.420000   TRUE
#> 10: prod(x$fisher$laspeyres[2:3]) 1.3646      4     1        1.364610   TRUE
#> 11: prod(x$fisher$laspeyres[2:4]) 1.3351      4     1        1.335139   TRUE
#> 12: prod(x$fisher$laspeyres[2:5]) 1.3306      4     1        1.330577   TRUE
#> 13:   prod(x$fisher$paasche[2:2]) 1.3823      3     1        1.382353   TRUE
#> 14:   prod(x$fisher$paasche[2:3]) 1.2740      4     1        1.273966   TRUE
#> 15:   prod(x$fisher$paasche[2:4]) 1.2060      4     1        1.205975   TRUE
#> 16:   prod(x$fisher$paasche[2:5]) 1.1234      4     1        1.123405   TRUE
# }
```
