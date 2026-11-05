# Notation of dmai

The symbols used in the help pages and vignettes of dmai, with their
meaning and the argument or result component that holds them. The
notation follows one rule throughout: scalars are upper-case letters in
normal face (\\P\\, \\R\\), indices lower-case italic (\\i, j\\),
vectors lower-case bold (\\\mathbf{y}\\, \\\boldsymbol{\beta}\\) and
matrices upper-case bold (\\\mathbf{X}\\, \\\boldsymbol{\Sigma}\\).

## Usage

``` r
dmai_notation(format = c("table", "path", "tex"))
```

## Arguments

- format:

  `"table"` (default: a `data.table` with columns `symbol` (LaTeX),
  `text` (plain text), `kind` (scalar, index, vector, matrix, set,
  function, operator), `meaning` and `argument`), `"path"` (the path of
  `notation.csv`) or `"tex"` (the path of `Notations.tex`, the notation
  appendix used in the articles).

## Value

See `format`.

## See also

Other dmai utilities:
[`dmai_reference()`](https://myaseen208.com/dmai/reference/dmai_reference.md),
[`dmai_save()`](https://myaseen208.com/dmai/reference/dmai_save.md),
[`dmai_theme()`](https://myaseen208.com/dmai/reference/dmai_theme.md)

## Author

Muhammad Yaseen (<myaseen208@gmail.com>)

## Examples

``` r
dmai_notation(
    format = c("table", "path", "tex")[1]
  )
#>                   symbol    text     kind
#>                   <char>  <char>   <char>
#>  1:                    n       n   scalar
#>  2:                    T       T   scalar
#>  3:                 i, j     i j    index
#>  4:                    t       t    index
#>  5:               x_{it}    x_it   scalar
#>  6:               r_{it}    r_it   scalar
#>  7:                  R_t     R_t   scalar
#>  8:            \\pi_{it}   pi_it   scalar
#>  9:               s_{it}    s_it   scalar
#> 10:        \\bar{s}_{it} sbar_it   scalar
#> 11:        \\mathbf{x}_t     x_t   vector
#> 12: \\boldsymbol{\\pi}_t    pi_t   vector
#> 13:        \\mathbf{s}_t     s_t   vector
#> 14:          \\mathbf{X}       X   matrix
#> 15:                  Q_t     Q_t   scalar
#> 16:                  V_t     V_t   scalar
#> 17:                  E_t     E_t   scalar
#> 18:                  P_t     P_t   scalar
#> 19:            \\kappa_t kappa_t   scalar
#> 20:                  K_t     K_t   scalar
#> 21:              \\Delta   Delta operator
#>                   symbol    text     kind
#>                   <char>  <char>   <char>
#>                                                                     meaning
#>                                                                      <char>
#>  1:                                                    number of components
#>  2:                                                         number of dates
#>  3:                                                              components
#>  4:                                                                   dates
#>  5:                                       quantity of component i at date t
#>  6:                                 rate of return of component i at date t
#>  7:                                                benchmark rate at date t
#>  8:                      user cost of component i: (R_t - r_it) / (1 + R_t)
#>  9:            share of component i in the expenditure on monetary services
#> 10:                                    average of the shares at t - 1 and t
#> 11:                                  quantities of the components at date t
#> 12:                                                    user costs at date t
#> 13:                                            expenditure shares at date t
#> 14:                              T x n matrix of quantities (dates in rows)
#> 15:               index of the aggregate at date t (base at the first date)
#> 16: aggregate in the units of x (equal to the simple sum at the first date)
#> 17:                      expenditure on monetary services: sum_i pi_it x_it
#> 18:                                   dual user-cost price index: E_t / V_t
#> 19:                            Divisia mean of the growth of the components
#> 20:                        Divisia variance of the growth of the components
#> 21:                                             first difference over dates
#>                                                                     meaning
#>                                                                      <char>
#>      argument
#>        <char>
#>  1:         x
#>  2:     .data
#>  3:          
#>  4:      date
#>  5:         x
#>  6:         r
#>  7: benchmark
#>  8: user_cost
#>  9:          
#> 10:          
#> 11:         x
#> 12: user_cost
#> 13:          
#> 14:         x
#> 15:      base
#> 16:          
#> 17:          
#> 18:          
#> 19:          
#> 20:          
#> 21:          
#>      argument
#>        <char>
```
