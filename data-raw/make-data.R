# The data sets of the package. Run from the package folder: Rscript data-raw/make-data.R

# 1. money_sim: SIMULATED monthly monetary components and rates, 2005-01 to 2024-12 (not real data)
set.seed(20240101)
T <- 240L
Date <- seq(as.Date("2005-01-01"), by = "month", length.out = T)
ar1 <- function(n, mu, phi, sd, x0 = mu) {
  x <- numeric(n); x[1] <- x0
  for (t in 2:n) x[t] <- mu + phi * (x[t - 1] - mu) + rnorm(1, sd = sd)
  x
}
policy <- pmax(ar1(T, 8, 0.97, 0.35), 1)                          # policy-type rate, percent
rb     <- policy + 1.5 + ar1(T, 0, 0.8, 0.15)                     # benchmark: a long bond yield, percent
spread <- function(m, s) pmax(policy - m + ar1(T, 0, 0.7, s), 0)   # deposit rates follow the policy rate
r <- cbind(
    r1 = 0                                                       # currency in circulation
  , r2 = 0                                                       # demand deposits
  , r3 = spread(4.5, 0.10)                                       # savings deposits
  , r4 = spread(2.5, 0.10)                                       # time deposits up to 3 months
  , r5 = spread(2.0, 0.10)                                       # time deposits 3 to 6 months
  , r6 = spread(1.5, 0.10)                                       # time deposits 6 months to 1 year
  , r7 = spread(0.8, 0.12)                                       # time deposits over 1 year
  , r8 = pmax(1.5 + ar1(T, 0, 0.9, 0.15), 0)                     # foreign currency deposits
  )
x0  <- c(1200, 900, 1500, 400, 350, 450, 300, 250)               # billions at the start
mu  <- c(0.08, 0.09, 0.11, 0.14, 0.15, 0.15, 0.16, 0.06) / 12     # trend growth per month (deposits outgrow currency)
beta <- c(-0.004, -0.003, 0.000, 0.002, 0.002, 0.003, 0.004, 0) # response to changes of the own spread
sdv <- c(0.012, 0.015, 0.010, 0.020, 0.020, 0.018, 0.022, 0.025)
own <- sweep(r, 1, rb, function(a, b) a - b)                     # own rate minus benchmark
X <- sapply(1:8, function(i) {
  d <- mu[i] + beta[i] * c(0, diff(own[, i])) + rnorm(T, sd = sdv[i])
  x0[i] * exp(cumsum(d))
})
colnames(X) <- paste0("x", 1:8)
money_sim <- data.frame(Date = Date, round(X, 2), round(r, 3), rb = round(rb, 3))
save(money_sim, file = "data/money_sim.rda", compress = "xz")

# 2. cpi_ch19: the artificial data of Chapter 19 of the Consumer Price Index Manual (ILO et al. 2004),
#    Tables 19.1 (prices) and 19.2 (quantities)
cpi_ch19 <- data.frame(
    period = 1:5
  , p1 = c(1.0, 1.2, 1.0, 0.8, 1.0), p2 = c(1.0, 3.0, 1.0, 0.5, 1.0), p3 = c(1.0, 1.3, 1.5, 1.6, 1.6)
  , p4 = c(1.0, 0.7, 0.5, 0.3, 0.1), p5 = c(1.0, 1.4, 1.7, 1.9, 2.0), p6 = c(1.0, 0.8, 0.6, 0.4, 0.2)
  , q1 = c(1.0, 0.8, 1.0, 1.2, 0.9), q2 = c(1.0, 0.9, 1.1, 1.2, 1.2), q3 = c(2.0, 1.9, 1.8, 1.9, 2.0)
  , q4 = c(1.0, 1.3, 3.0, 6.0, 12.0), q5 = c(4.5, 4.7, 5.0, 5.6, 6.5), q6 = c(0.5, 0.6, 0.8, 1.3, 2.5)
  )
save(cpi_ch19, file = "data/cpi_ch19.rda", compress = "xz")
