# ggplot2 theme of dmai

The theme used by every
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method of dmai
(identical in all packages of the author):
[`theme_classic`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with a serif font, black axis lines, ticks pointing inwards and no grid,
close to the look of TikZ/pgfplots figures in LaTeX documents. The
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods also
use `scales::pretty_breaks(n = 10)` on every continuous axis. Save
figures with
[`dmai_save`](https://myaseen208.com/dmai/reference/dmai_save.md).

## Usage

``` r
dmai_theme(
  base_size = 11,
  base_family = "serif",
  ticks_inward = TRUE,
  legend_position = "bottom"
)
```

## Arguments

- base_size:

  base font size in points (default 11).

- base_family:

  font family (default `"serif"`; for example `"Latin Modern Roman"`
  when installed, to match LaTeX documents).

- ticks_inward:

  logical; ticks point into the panel, as in pgfplots (default `TRUE`).

- legend_position:

  position of the legend (default `"bottom"`).

## Value

A `ggplot2` theme.

## See also

Other dmai utilities:
[`dmai_notation()`](https://myaseen208.com/dmai/reference/dmai_notation.md),
[`dmai_reference()`](https://myaseen208.com/dmai/reference/dmai_reference.md),
[`dmai_save()`](https://myaseen208.com/dmai/reference/dmai_save.md)

## Author

Muhammad Yaseen (<myaseen208@gmail.com>)

## Examples

``` r
library(ggplot2)
p <-
  ggplot(data = data.frame(x = 1:20, y = cumsum(sin(1:20))), mapping = aes(x = x, y = y)) +
  geom_line()
p +
  dmai_theme(
      base_size       = 11
    , base_family     = "serif"
    , ticks_inward    = TRUE
    , legend_position = "bottom"
    )

p +
  dmai_theme(
      base_size       = 12
    , base_family     = "serif"
    , ticks_inward    = FALSE
    , legend_position = "right"
    ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
```
