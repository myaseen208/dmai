# Plain-language interpretation of a result

`dmai_interpret()` turns a result of the package into a short report: a
headline, findings, recommendations and cautions, computed from its
numbers, with the sources of the method. It covers
[`dmai()`](https://myaseen208.com/dmai/reference/dmai.md) (with
`as_dt = FALSE`),
[`dmai_compare()`](https://myaseen208.com/dmai/reference/dmai_compare.md),
[`dmai_user_cost()`](https://myaseen208.com/dmai/reference/dmai_user_cost.md)
and
[`dmai_moments()`](https://myaseen208.com/dmai/reference/dmai_moments.md).

## Usage

``` r
dmai_interpret(x, top = 3L, ...)

# Default S3 method
dmai_interpret(x, top = 3L, ...)

# S3 method for class 'dmai_interpretation'
print(x, ...)

# S3 method for class 'dmai_interpretation'
format(x, style = c("text", "markdown"), width = getOption("width", 80L), ...)
```

## Arguments

- x:

  a result of a function of the package.

- top:

  number of dates or components named in the findings (default 3).

- ...:

  unused.

- style:

  `"text"` (default) or `"markdown"`.

- width:

  line width of the text (default `getOption("width")`).

## Value

An object of class `"dmai_interpretation"`: a list with `analysis`,
`title`, `headline`, `findings`, `recommendations`, `cautions`, `facts`
(the numbers behind the sentences), `guide` (how to read the method) and
`sources`. [`format()`](https://rdrr.io/r/base/format.html) gives it as
text or Markdown.

## Details

Growth per year is the average growth between the first and last dates,
annualised with the spacing of the dates (monthly, quarterly,
half-yearly or annual) when the date column is of class `Date`;
otherwise growth is reported per period.

## See also

[`dmai_cite`](https://myaseen208.com/dmai/reference/dmai_cite.md),
[`dmai_app`](https://myaseen208.com/dmai/reference/dmai_app.md)

## Author

Muhammad Yaseen (<myaseen208@gmail.com>)

## Examples

``` r
data(money_sim)
fit <-
  dmai(
      .data     = money_sim
    , method    = "Barnett"
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
dmai_interpret(
    x   = fit
  , top = 3
  )
#> Divisia (Barnett) monetary aggregate
#> 
#> From 2005-01-01 to 2024-12-01 the Divisia (Barnett) aggregate grew 11.3% a
#> year, against 12.8% for the simple sum (1.6 points less).
#> 
#> Findings:
#> - Largest growth at 2009-03-01, 2021-12-01, 2024-01-01 (2.49, 2.24, 2.17);
#>   smallest at 2007-06-01, 2012-09-01, 2018-12-01 (-0.71, -0.67, -0.61).
#> - At the last date the largest weights are x3 (28.1%), x1 (19.5%), x2
#>   (18.8%).
#> - The weights moved most for x1 (32.3 to 19.5%), x4 (4.3 to 9.9%), x6 (3.7 to
#>   8.5%).
#> - The simple sum overstates money growth: it gives the same weight to
#>   interest-bearing deposits, which are held largely as savings, as to
#>   currency and demand deposits.
#> 
#> Recommendations:
#> - Compare the methods with dmai_compare(); check how the result depends on
#>   the benchmark rate.
#> - dmai_moments() shows when the components grow at very different rates.
#> 
#> Sources:
#> - Barnett, W. A. (1978). The user cost of money. Economics Letters, 1(2),
#>   145-149. doi:10.1016/0165-1765(78)90051-4
#> - Barnett, W. A. (1980). Economic monetary aggregates: an application of
#>   index number and aggregation theory. Journal of Econometrics, 14(1), 11-48.
#>   doi:10.1016/0304-4076(80)90070-6
#> - Tornqvist, L. (1936). The Bank of Finland's consumption price index. Bank
#>   of Finland Monthly Bulletin, 10, 1-8.
#> - Theil, H. (1967). Economics and Information Theory. North-Holland,
#>   Amsterdam.
```
