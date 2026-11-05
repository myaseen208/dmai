# Simulated monetary components and rates

SIMULATED monthly data, January 2005 to December 2024, shaped like the
components of a broad money aggregate of a central bank: quantities
(billions) and rates of return (percent a year) of eight components, and
a benchmark rate. The numbers are not real; they are drawn by
`data-raw/make-data.R` in the source package (rates as autoregressive
processes that follow a policy-type rate, quantities as trending series
that respond to the spread between their own rate and the benchmark) to
show the methods.

## Usage

``` r
money_sim
```

## Format

A data frame with 240 rows and 18 columns:

- `Date`:

  first day of the month (class `Date`).

- `x1`:

  currency in circulation.

- `x2`:

  demand deposits.

- `x3`:

  savings deposits.

- `x4`:

  time deposits up to 3 months.

- `x5`:

  time deposits of 3 to 6 months.

- `x6`:

  time deposits of 6 months to 1 year.

- `x7`:

  time deposits over 1 year.

- `x8`:

  foreign currency deposits.

- `r1`:

  the own rate of `x1` in percent (zero).

- `r2`:

  the own rate of `x2` in percent (zero).

- `r3`:

  the own rate of `x3` in percent.

- `r4`:

  the own rate of `x4` in percent.

- `r5`:

  the own rate of `x5` in percent.

- `r6`:

  the own rate of `x6` in percent.

- `r7`:

  the own rate of `x7` in percent.

- `r8`:

  the own rate of `x8` in percent.

- `rb`:

  the benchmark rate in percent (a long bond yield), above every
  component rate.

## Source

Simulated; see `data-raw/make-data.R`.

## See also

Other dmai data:
[`cpi_ch19`](https://myaseen208.com/dmai/reference/cpi_ch19.md)

## Examples

``` r
data(money_sim)
head(money_sim)
#>         Date      x1     x2      x3     x4     x5     x6     x7     x8 r1 r2
#> 1 2005-01-01 1235.89 902.17 1528.41 391.09 363.04 453.61 294.87 244.10  0  0
#> 2 2005-02-01 1227.77 929.38 1549.25 383.42 377.85 452.20 301.51 240.38  0  0
#> 3 2005-03-01 1248.90 947.40 1585.89 383.64 377.37 450.00 308.99 245.24  0  0
#> 4 2005-04-01 1243.45 947.19 1584.36 370.24 388.20 445.87 313.15 243.91  0  0
#> 5 2005-05-01 1231.78 945.22 1603.69 388.85 384.77 447.13 319.44 251.38  0  0
#> 6 2005-06-01 1221.64 963.80 1627.26 405.89 394.48 447.90 336.14 242.79  0  0
#>      r3    r4    r5    r6    r7    r8    rb
#> 1 3.500 5.500 6.000 6.500 7.200 1.500 9.500
#> 2 3.510 5.635 6.088 6.475 7.168 1.852 9.553
#> 3 3.165 5.258 5.661 5.932 6.850 1.663 9.028
#> 4 3.729 5.647 6.166 6.263 7.176 1.456 9.267
#> 5 3.070 5.075 5.469 5.883 6.858 1.642 9.127
#> 6 3.216 5.156 5.715 6.124 6.956 1.676 9.058
```
