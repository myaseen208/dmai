# Artificial price and quantity data of the Consumer Price Index Manual

The artificial data set of Chapter 19 of the Consumer Price Index Manual
(ILO et al. 2004, Tables 19.1 and 19.2): prices and quantities of six
commodities in five periods. The Manual gives the chained Laspeyres,
Paasche, Fisher and Tornqvist price indexes of these data (Tables 19.5
and 19.11), which the package reproduces: a Divisia quantity index is a
Tornqvist index with the user costs as prices, so
[`dmai()`](https://myaseen208.com/dmai/reference/dmai.md) with the
prices as `x` and the quantities as `user_cost` gives the Manual's price
indexes (the two roles are symmetric in these formulas).

## Usage

``` r
cpi_ch19
```

## Format

A data frame with 5 rows and 13 columns: `period` (1 to 5), `p1`, ...,
`p6` (prices) and `q1`, ..., `q6` (quantities).

## Source

ILO, IMF, OECD, UNECE, Eurostat and The World Bank (2004). *Consumer
Price Index Manual: Theory and Practice*. International Labour Office,
Geneva. Chapter 19, Tables 19.1 and 19.2.

## See also

Other dmai data:
[`money_sim`](https://myaseen208.com/dmai/reference/money_sim.md)

## Examples

``` r
data(cpi_ch19)
# the chained Tornqvist price index of Table 19.11: 1, 1.4052, 1.3112, 1.2624, 1.2224
dmai(
    .data     = cpi_ch19
  , method    = "Barnett"
  , logbase   = NULL
  , date      = "period"
  , x         = paste0("p", 1:6)
  , r         = NULL
  , user_cost = paste0("q", 1:6)
  , benchmark = NULL
  , percent   = TRUE
  , base      = 1
  , as_dt     = TRUE
  )
#>    period      DMAI    index    value
#>     <int>     <num>    <num>    <num>
#> 1:      1        NA 1.000000 6.000000
#> 2:      2 34.015242 1.405162 8.430970
#> 3:      3 -6.920871 1.311201 7.867208
#> 4:      4 -3.789754 1.262440 7.574639
#> 5:      5 -3.222585 1.222405 7.334431
```
