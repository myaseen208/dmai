# References of a result or of the package

`dmai_cite()` gives the references of the methods behind a result of the
package, or the references of every method in the package, as text or
BibTeX. `citation("dmai")` cites the package and its main methods.

## Usage

``` r
dmai_cite(x = NULL, style = c("text", "bibtex"))

# Default S3 method
dmai_cite(x = NULL, style = c("text", "bibtex"))
```

## Arguments

- x:

  a result of a function of the package (with `as_dt = FALSE` for
  [`dmai()`](https://myaseen208.com/dmai/reference/dmai.md)), or `NULL`
  (default) for every reference of the package.

- style:

  `"text"` (default) or `"bibtex"`.

## Value

An object of class `bibentry` (printed in the chosen style).

## Author

Muhammad Yaseen (<myaseen208@gmail.com>)

## Examples

``` r
dmai_cite(
    x     = NULL
  , style = "text"
  )
#> Barnett, W. A. (1978). The user cost of money. Economics Letters, 1(2), 145-149. doi:10.1016/0165-1765(78)90051-4
#> 
#> Barnett, W. A. (1980). Economic monetary aggregates: an application of index number and aggregation theory. Journal of Econometrics, 14(1), 11-48. doi:10.1016/0304-4076(80)90070-6
#> 
#> Hancock, D. (1985). The financial firm: production with monetary and nonmonetary goods. Journal of Political Economy, 93(5), 859-880. doi:10.1086/261339
#> 
#> Tornqvist, L. (1936). The Bank of Finland's consumption price index. Bank of Finland Monthly Bulletin, 10, 1-8.
#> 
#> Theil, H. (1967). Economics and Information Theory. North-Holland, Amsterdam.
#> 
#> Fisher, I. (1922). The Making of Index Numbers. Houghton Mifflin, Boston.
#> 
#> Diewert, W. E. (1976). Exact and superlative index numbers. Journal of Econometrics, 4(2), 115-145. doi:10.1016/0304-4076(76)90009-9
#> 
#> Rotemberg, J. J., Driscoll, J. C. and Poterba, J. M. (1995). Money, output, and prices: evidence from a new monetary aggregate. Journal of Business and Economic Statistics, 13(1), 67-83.
#> 
#> Barnett, W. A. and Serletis, A. (1990). A dispersion-dependency diagnostic test for aggregation error: with applications to monetary economics and income distribution. Journal of Econometrics, 43(1-2), 5-34. doi:10.1016/0304-4076(90)90104-2
#> 
#> ILO, IMF, OECD, UNECE, Eurostat and The World Bank (2004). Consumer Price Index Manual: Theory and Practice. International Labour Office, Geneva. Chapter 19: Price indices using an artificial data set. 
data(money_sim)
fit <-
  dmai(
      .data     = money_sim
    , method    = "Fisher"
    , logbase   = NULL
    , date      = "Date"
    , x         = NULL
    , r         = NULL
    , user_cost = NULL
    , benchmark = "rb"
    , percent   = TRUE
    , base      = 100
    , as_dt     = FALSE
    )
dmai_cite(
    x     = fit
  , style = "bibtex"
  )
#> @Article{Barnett1978,
#>   title = {The User Cost of Money},
#>   author = {William A. Barnett},
#>   journal = {Economics Letters},
#>   year = {1978},
#>   volume = {1},
#>   number = {2},
#>   pages = {145--149},
#>   doi = {10.1016/0165-1765(78)90051-4},
#> }
#> 
#> @Article{Barnett1980,
#>   title = {Economic Monetary Aggregates: An Application of Index Number and Aggregation Theory},
#>   author = {William A. Barnett},
#>   journal = {Journal of Econometrics},
#>   year = {1980},
#>   volume = {14},
#>   number = {1},
#>   pages = {11--48},
#>   doi = {10.1016/0304-4076(80)90070-6},
#> }
#> 
#> @Book{Fisher1922,
#>   title = {The Making of Index Numbers},
#>   author = {Irving Fisher},
#>   publisher = {Houghton Mifflin},
#>   address = {Boston},
#>   year = {1922},
#> }
#> 
#> @Article{Diewert1976,
#>   title = {Exact and Superlative Index Numbers},
#>   author = {W. Erwin Diewert},
#>   journal = {Journal of Econometrics},
#>   year = {1976},
#>   volume = {4},
#>   number = {2},
#>   pages = {115--145},
#>   doi = {10.1016/0304-4076(76)90009-9},
#> }
```
