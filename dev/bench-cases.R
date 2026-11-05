# Cases timed by tools/bench.R: calls that run in version 0.5.0 and in this version, on simulated data with the
# columns of version 0.5.0 (Date, x1, ..., r1, ...). Each case is a quoted call evaluated in the global environment.
set.seed(2024)
mk <- function(T, n) {
  d <- data.frame(Date = seq(as.Date("2000-01-01"), by = "month", length.out = T))
  for (i in seq_len(n)) d[[paste0("x", i)]] <- exp(cumsum(rnorm(T, 0.01, 0.02))) * 100
  for (i in seq_len(n)) d[[paste0("r", i)]] <- pmax(0, 2 + i / 2 + cumsum(rnorm(T, 0, 0.1)))
  d
}
d36  <- mk(36, 13)
d240 <- mk(240, 8)
d600 <- mk(600, 20)
list(
    barnett_36x13   = quote(dmai(.data = d36, method = "Barnett", logbase = 10))
  , hancock_36x13   = quote(dmai(.data = d36, method = "Hancock"))
  , barnett_240x8   = quote(dmai(.data = d240, method = "Barnett", logbase = 10))
  , barnett_600x20  = quote(dmai(.data = d600, method = "Barnett", logbase = 10))
  )
