# Theory and notation

\\ \newcommand{\vy}{\mathbf{y}} \newcommand{\vx}{\mathbf{x}}
\newcommand{\vb}{\mathbf{b}} \newcommand{\ve}{\mathbf{e}}
\newcommand{\vone}{\mathbf{1}} \newcommand{\vbeta}{\boldsymbol{\beta}}
\newcommand{\vmu}{\boldsymbol{\mu}} \newcommand{\mX}{\mathbf{X}}
\newcommand{\mZ}{\mathbf{Z}} \newcommand{\mV}{\mathbf{V}}
\newcommand{\mI}{\mathbf{I}} \newcommand{\mJ}{\mathbf{J}}
\newcommand{\mSigma}{\boldsymbol{\Sigma}} \newcommand{\E}{\mathrm{E}}
\newcommand{\Var}{\mathrm{Var}} \newcommand{\Cov}{\mathrm{Cov}}
\newcommand{\tr}{\operatorname{tr}} \newcommand{\vr}{\mathbf{r}}
\newcommand{\vs}{\mathbf{s}} \newcommand{\vpi}{\boldsymbol{\pi}} \\

## 1 Abstract

This vignette sets out the formulas of the package: the user cost of a
monetary asset, the expenditure shares, the Divisia index in the
discrete form of Tornqvist and Theil, the Fisher ideal index, the simple
sum and the currency equivalent index, the dual user-cost price index
and the Divisia second moments, with the notation used throughout.

## 2 Notation

| symbol | meaning | argument |
|:---|:---|:---|
| \\n\\ | number of components | x |
| \\T\\ | number of dates | .data |
| \\i, j\\ | components |  |
| \\t\\ | dates | date |
| \\x\_{it}\\ | quantity of component i at date t | x |
| \\r\_{it}\\ | rate of return of component i at date t | r |
| \\R_t\\ | benchmark rate at date t | benchmark |
| \\\pi\_{it}\\ | user cost of component i: (R_t - r_it) / (1 + R_t) | user_cost |
| \\s\_{it}\\ | share of component i in the expenditure on monetary services |  |
| \\\bar{s}\_{it}\\ | average of the shares at t - 1 and t |  |
| \\\mathbf{x}\_t\\ | quantities of the components at date t | x |
| \\\boldsymbol{\pi}\_t\\ | user costs at date t | user_cost |
| \\\mathbf{s}\_t\\ | expenditure shares at date t |  |
| \\\mathbf{X}\\ | T x n matrix of quantities (dates in rows) | x |
| \\Q_t\\ | index of the aggregate at date t (base at the first date) | base |
| \\V_t\\ | aggregate in the units of x (equal to the simple sum at the first date) |  |
| \\E_t\\ | expenditure on monetary services: sum_i pi_it x_it |  |
| \\P_t\\ | dual user-cost price index: E_t / V_t |  |
| \\\kappa_t\\ | Divisia mean of the growth of the components |  |
| \\K_t\\ | Divisia variance of the growth of the components |  |
| \\\Delta\\ | first difference over dates |  |

## 3 User costs

A monetary asset gives two things: a yield and monetary services
(liquidity). Barnett (1978) prices the services by the yield given up.
With the benchmark rate \\R_t\\, the return on an asset that gives no
monetary services, and the own rate \\r\_{it}\\ of component \\i\\, the
user cost of one unit of it held over period \\t\\ is \\ \pi\_{it} =
\frac{R_t - r\_{it}}{1 + R_t}, \\ the discounted interest forgone. The
expenditure on monetary services is \\E_t = \sum_i \pi\_{it}x\_{it}\\
and the share of component \\i\\ \\ s\_{it} =
\frac{\pi\_{it}x\_{it}}{E_t}. \\ The factor \\1/(1 + R_t)\\ is the same
for every component and cancels in the shares. Rates may be in percent
(`percent = TRUE`) or decimal.

The user costs and shares at the first date of the simulated data:

``` r

data(money_sim)
uc <-
  dmai_user_cost(
      .data     = money_sim
    , date      = "Date"
    , x         = NULL
    , r         = NULL
    , benchmark = "rb"
    , percent   = TRUE
    )
uc$components[Date == min(Date)]
```

             Date component quantity  rate  user_cost      share
           <Date>    <fctr>    <num> <num>      <num>      <num>
    1: 2005-01-01        x1  1235.89   0.0 0.08675799 0.32336335
    2: 2005-01-01        x2   902.17   0.0 0.08675799 0.23604748
    3: 2005-01-01        x3  1528.41   3.5 0.05479452 0.25256810
    4: 2005-01-01        x4   391.09   5.5 0.03652968 0.04308480
    5: 2005-01-01        x5   363.04   6.0 0.03196347 0.03499531
    6: 2005-01-01        x6   453.61   6.5 0.02739726 0.03747928
    7: 2005-01-01        x7   294.87   7.2 0.02100457 0.01867866
    8: 2005-01-01        x8   244.10   1.5 0.07305936 0.05378301

**The benchmark rate.** \\R_t\\ should exceed the rate of every
component. With `benchmark = NULL` the package takes \\R_t = \max_i
r\_{it}\\, as version 0.5.0 did; the component with that rate then has a
user cost of zero. With a given benchmark the package uses \\\max(R_t,
\max_i r\_{it})\\, the upper envelope.

## 4 Indexes

All indexes are chained: each date is linked to the previous one and the
links are multiplied, with \\Q_1\\ equal to `base`. With \\\bar{s}\_{it}
= (s\_{it} + s\_{i,t-1})/2\\:

**Divisia** (`"Barnett"`). Barnett (1980) aggregates by the Divisia
index, in discrete time the index of Tornqvist (1936) and Theil (1967):
\\ \log\frac{Q_t}{Q\_{t-1}} = \sum\_{i=1}^{n}\bar{s}\_{it}\left(\log
x\_{it} - \log x\_{i,t-1}\right). \\

**Divisia with simple growth** (`"Hancock"`). The same shares with
simple growth rates, as in version 0.5.0: \\ \frac{Q_t}{Q\_{t-1}} - 1 =
\sum\_{i=1}^{n}\bar{s}\_{it}\left(\frac{x\_{it}}{x\_{i,t-1}} - 1\right).
\\

**Fisher ideal** (`"Fisher"`). The geometric mean of the Laspeyres and
Paasche quantity indexes with the user costs as prices (Fisher 1922): \\
\frac{Q_t}{Q\_{t-1}} =
\left(\frac{\boldsymbol{\pi}\_{t-1}^\top\mathbf{x}\_t}{\boldsymbol{\pi}\_{t-1}^\top\mathbf{x}\_{t-1}}
\cdot\frac{\boldsymbol{\pi}\_t^\top\mathbf{x}\_t}{\boldsymbol{\pi}\_t^\top\mathbf{x}\_{t-1}}\right)^{1/2}.
\\ The Divisia (Tornqvist) and Fisher indexes are both superlative:
exact for flexible aggregator functions (Diewert 1976), and they agree
closely in practice.

**Simple sum** (`"SimpleSum"`). \\\sum_i x\_{it}\\, the usual official
aggregate: perfect substitutes, equal weights.

**Currency equivalent** (`"CE"`). Rotemberg, Driscoll and Poterba
(1995): \\ CE_t = \sum\_{i=1}^{n}\frac{R_t - r\_{it}}{R_t}\\x\_{it}, \\
the stock of currency that would give the same services.

**Growth.** `DMAI` is \\100\log(Q_t/Q\_{t-1})\\ for every method except
`"Hancock"`, for which it is \\100(Q_t/Q\_{t-1} - 1)\\. With `logbase`
the logarithm is taken to that base; version 0.5.0 used base 10 by
default, which gives \\1/\log 10 \approx 0.434\\ times the growth rate.

**Level in money units.** \\V_t = V_1 Q_t/Q_1\\ with \\V_1 = \sum_i
x\_{i1}\\ (column `value`).

## 5 The dual price index

By factor reversal the user-cost price of the aggregate is \\ P_t =
\frac{E_t}{V_t}, \\ so that price times quantity is the expenditure
(component `price`). For the Fisher index it equals the Fisher price
index of the user costs; for the Divisia index it is the implicit price
index.

## 6 Second moments

Theil (1967) reads the shares as probabilities: the Divisia growth is
then the mean of the growth rates of the components, and their variance
\\ K_t = \sum\_{i=1}^{n}\bar{s}\_{it}\left(\Delta\log x\_{it} -
\kappa_t\right)^2, \qquad \kappa_t =
\sum\_{i=1}^{n}\bar{s}\_{it}\\\Delta\log x\_{it}, \\ measures how
differently the components grow. Barnett and Serletis (1990) use it as a
diagnostic: when it is large the weights matter and an aggregate with
the wrong weights (the simple sum) misleads.
[`dmai_moments()`](https://myaseen208.com/dmai/reference/dmai_moments.md)
gives \\100\kappa_t\\, \\10^4K_t\\ and \\100\sqrt{K_t}\\, and the same
for the user costs.

## 7 References

Barnett, W. A. (1978). The user cost of money. *Economics Letters*,
1(2), 145–149.

Barnett, W. A. (1980). Economic monetary aggregates: an application of
index number and aggregation theory. *Journal of Econometrics*, 14(1),
11–48.

Barnett, W. A. and Serletis, A. (1990). A dispersion-dependency
diagnostic test for aggregation error: with applications to monetary
economics and income distribution. *Journal of Econometrics*, 43(1–2),
5–34.

Diewert, W. E. (1976). Exact and superlative index numbers. *Journal of
Econometrics*, 4(2), 115–145.

Fisher, I. (1922). *The Making of Index Numbers*. Houghton Mifflin,
Boston.

Rotemberg, J. J., Driscoll, J. C. and Poterba, J. M. (1995). Money,
output, and prices: evidence from a new monetary aggregate. *Journal of
Business and Economic Statistics*, 13(1), 67–83.

Theil, H. (1967). *Economics and Information Theory*. North-Holland,
Amsterdam.

Tornqvist, L. (1936). The Bank of Finland’s consumption price index.
*Bank of Finland Monthly Bulletin*, 10, 1–8.
