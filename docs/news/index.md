# Changelog

## dmai 1.0.0

### Changes visible to users

- [`dmai()`](https://myaseen208.com/dmai/reference/dmai.md) keeps its
  name, its arguments (`.data`, `method`, `logbase`) and its result (a
  table with the date and `DMAI`); new arguments are appended: `date`,
  `x`, `r` (column names; by default `Date`, `x1`, `x2`, … and `r1`,
  `r2`, … as before), `user_cost` (user costs given directly),
  `benchmark` (a benchmark rate instead of the largest component rate,
  with the upper envelope), `percent`, `base` and `as_dt`.
- The table also has the index level (`index`, equal to `base` at the
  first date) and `value` (the aggregate in the units of the
  components). It is a `data.table` (version 0.5.0 returned a grouped
  tibble).
- `as_dt = FALSE` returns the full result, of class
  `c("dmai_index", "dmai_result", "list")`: the index, the user costs
  and shares of every component, the benchmark rate, the dual user-cost
  price index, the Divisia second moments and the simple sum, with
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and `as.data.table()` methods; the printed result ends with the
  sources of the method.
- Every function that takes data or a result is an S3 generic with a
  `.default` method.
- `method` has a default (`"Barnett"`): in version 0.5.0 a call without
  `method` stopped with an error.

### Corrections

- The growth `DMAI` of the Barnett method used logarithms to base 10 by
  default (`logbase = NULL`), which gives 0.434 times the growth rate;
  Barnett’s Divisia index uses natural logarithms. The default is now
  the natural logarithm, so `DMAI` is the growth in percent;
  `logbase = 10` reproduces version 0.5.0 exactly.

### New

- Methods `"Fisher"` (Fisher ideal index), `"SimpleSum"` and `"CE"`
  (currency equivalent index of Rotemberg, Driscoll and Poterba 1995) in
  [`dmai()`](https://myaseen208.com/dmai/reference/dmai.md).
- [`dmai_compare()`](https://myaseen208.com/dmai/reference/dmai_compare.md):
  every method on the same data, with average growth per period and per
  year.
- [`dmai_user_cost()`](https://myaseen208.com/dmai/reference/dmai_user_cost.md):
  user costs and expenditure shares of the components.
- [`dmai_moments()`](https://myaseen208.com/dmai/reference/dmai_moments.md):
  Divisia means and variances of the growth of the components and of
  their user costs (Barnett and Serletis 1990).
- The dual user-cost price index by factor reversal.
- [`dmai_interpret()`](https://myaseen208.com/dmai/reference/dmai_interpret.md),
  [`dmai_cite()`](https://myaseen208.com/dmai/reference/dmai_cite.md),
  [`dmai_app()`](https://myaseen208.com/dmai/reference/dmai_app.md): a
  plain-language interpretation of every result, the references of every
  method and a built-in shiny application;
  [`dmai_theme()`](https://myaseen208.com/dmai/reference/dmai_theme.md),
  [`dmai_save()`](https://myaseen208.com/dmai/reference/dmai_save.md)
  (with WebP),
  [`dmai_notation()`](https://myaseen208.com/dmai/reference/dmai_notation.md),
  [`dmai_reference()`](https://myaseen208.com/dmai/reference/dmai_reference.md).
- Data: `money_sim` (simulated monthly monetary components, 2005
  to 2024) and `cpi_ch19` (the artificial data of the Consumer Price
  Index Manual, whose chained Laspeyres, Paasche, Fisher and Tornqvist
  indexes the package reproduces).
- Quarto vignettes in HTML and PDF; complete help pages with the
  formulas; tests against version 0.5.0, hand computations and the
  published values.

### Internal

- dplyr, magrittr, stringr, tibble and tidyr are no longer imported; the
  computations are matrix operations and `data.table`.

## dmai 0.5.0

CRAN release: 2024-03-23

- Updated to pass CRAN tests

## dmai 0.4.0

CRAN release: 2019-05-18

### Bug

- Fixed the bug in logbase argument

## dmai 0.3.0

CRAN release: 2019-04-11

### New Features

- Added logbase argument

## dmai 0.1.0

CRAN release: 2018-11-02

### New Features

- First version of dmai
