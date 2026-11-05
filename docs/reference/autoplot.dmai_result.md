# Plot a dmai result with ggplot2

`autoplot()` methods for the results of dmai;
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the same
figures.

## Usage

``` r
autoplot.dmai_result(object, which = NULL, ...)
```

## Arguments

- object:

  a result of
  [`dmai`](https://myaseen208.com/dmai/reference/dmai.md)`(..., as_dt = FALSE)`,
  [`dmai_compare()`](https://myaseen208.com/dmai/reference/dmai_compare.md),
  [`dmai_user_cost()`](https://myaseen208.com/dmai/reference/dmai_user_cost.md)
  or
  [`dmai_moments()`](https://myaseen208.com/dmai/reference/dmai_moments.md).

- which:

  the figure: for
  [`dmai()`](https://myaseen208.com/dmai/reference/dmai.md) results
  `"index"` (default, with the simple sum), `"growth"`, `"shares"`,
  `"user_cost"`, `"moments"` or `"price"`; for
  [`dmai_compare()`](https://myaseen208.com/dmai/reference/dmai_compare.md)
  `"index"` or `"growth"`; for
  [`dmai_user_cost()`](https://myaseen208.com/dmai/reference/dmai_user_cost.md)
  `"shares"` or `"user_cost"`.

- ...:

  not used.

## Value

A `ggplot` object.

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
ggplot2::autoplot(
    object = fit
  , which  = "shares"
  )
```
