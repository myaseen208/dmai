# dmai: Divisia monetary aggregates index

The dmai package builds monetary aggregates from their components
(currency, demand deposits, savings and time deposits, ...) by the
methods of index number theory. The Divisia index of Barnett (1980)
weights every component by its share in the expenditure on monetary
services, priced by its user cost (Barnett 1978); the package also gives
the Fisher ideal index, a variant with simple growth rates, the simple
sum and the currency equivalent index, the dual user-cost price index
and the Divisia second moments.

## Functions

- [`dmai()`](https://myaseen208.com/dmai/reference/dmai.md):

  the aggregate by one method (Barnett, Hancock, Fisher, simple sum,
  currency equivalent).

- [`dmai_compare()`](https://myaseen208.com/dmai/reference/dmai_compare.md):

  every method on the same data.

- [`dmai_user_cost()`](https://myaseen208.com/dmai/reference/dmai_user_cost.md):

  user costs and expenditure shares of the components.

- [`dmai_moments()`](https://myaseen208.com/dmai/reference/dmai_moments.md):

  Divisia means and variances of the growth of the components.

- [`dmai_interpret()`](https://myaseen208.com/dmai/reference/dmai_interpret.md),
  [`dmai_cite()`](https://myaseen208.com/dmai/reference/dmai_cite.md),
  [`dmai_app()`](https://myaseen208.com/dmai/reference/dmai_app.md):

  plain-language interpretation, references and a shiny application.

The data sets
[`money_sim`](https://myaseen208.com/dmai/reference/money_sim.md)
(simulated monetary data) and
[`cpi_ch19`](https://myaseen208.com/dmai/reference/cpi_ch19.md) (the
artificial data of the Consumer Price Index Manual, used to check the
index formulas) come with the package.

## References

Barnett, W. A. (1978). The user cost of money. *Economics Letters*,
**1**(2), 145–149.
[doi:10.1016/0165-1765(78)90051-4](https://doi.org/10.1016/0165-1765%2878%2990051-4)

Barnett, W. A. (1980). Economic monetary aggregates: an application of
index number and aggregation theory. *Journal of Econometrics*,
**14**(1), 11–48.
[doi:10.1016/0304-4076(80)90070-6](https://doi.org/10.1016/0304-4076%2880%2990070-6)

## See also

Useful links:

- <https://myaseen208.com/dmai/>

- <https://github.com/myaseen208/dmai>

- Report bugs at <https://github.com/myaseen208/dmai/issues>

## Author

Muhammad Yaseen (<myaseen208@gmail.com>) and Ahmad Nadeem
(<Ahmed.Nadeem@sbp.org.pk>)
