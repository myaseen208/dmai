# dmai: progress (read this first)

Prefix `dmai`. Standard: SKILL.qmd. Released version audited in dev/AUDIT.md; legacy snapshot in dev/legacy/.

## Phase

| phase | state | note |
|---|---|---|
| P0 audit and snapshots | done | 0.5.0 (CRAN); the example with a seed in dev/legacy/examples/dmai.R |
| P1 repository and scaffold | done | common layer from mhpfilter, prefix dmai |
| P2 cores (computation) | done | R/core.R: matrix computations, no compiled code needed |
| P3 interface and results | done | S3 generics with .default; data.table table kept as the default result (as_dt = TRUE) |
| P4 help pages | done | LaTeX maths, every argument in the examples |
| P5 tests | done | legacy, hand computations, identities, CPI Manual values, application |
| P6 vignettes (HTML + PDF) | done | five vignettes |
| P7 README, NEWS, site | done | README.qmd, NEWS.qmd, _pkgdown.yml |
| P8 checks and release | done | tools/check-all.R |

## Functions -> cores -> tests -> vignette

| exported function | core(s) | test file | vignette | state |
|---|---|---|---|---|
| dmai | .dmai_input, .dmai_links, .dmai_fit | test-dmai, test-legacy, test-reference | 01, 02, 03, 05 | done |
| dmai_compare | .dmai_fit | test-dmai | 01, 03 | done |
| dmai_user_cost | .dmai_input, .dmai_components_table | test-dmai | 01, 03 | done |
| dmai_moments | .dmai_moments_core | test-dmai | 01, 02, 03 | done |
| dmai_interpret, dmai_cite | | test-dmai | 01 | done |
| dmai_app, dmai_app_object | | test-app | 04 | done |

## Corrections of the released version

| function | result | old value | new value | reason | in NEWS |
|---|---|---|---|---|---|
| dmai (Barnett) | DMAI with the default logbase | 100 x sum s (log10 x_t - log10 x_t-1) | 100 x sum s (ln x_t - ln x_t-1) | Divisia uses natural logarithms; the old default was 0.434 times the growth rate | yes |
| dmai | call without `method` | error (condition of length 2) | Barnett | R >= 4.2 | yes |

## Decisions specific to this package

* Natural logarithm as the default (decided with the author); `logbase = 10` reproduces 0.5.0.
* The default benchmark stays the largest component rate (0.5.0); `benchmark` gives a separate rate with the upper
  envelope.
* No real data could be shipped; `money_sim` is simulated and labelled as such. `cpi_ch19` (artificial data of the
  CPI Manual) validates the index formulas against published values.
* `user_cost` lets the user supply user costs directly (and makes the CPI Manual check possible).
