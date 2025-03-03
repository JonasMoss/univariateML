lambda <- 3
theta <- rexp(50000, lambda)
z <- rbinom(50000, 1, 0.5)
psi <- 0.5 * theta + z * 3 / 2 * theta
# plot(theta, psi)
# abline(a = 0, b = 1)

# x <- psi[z == 0]
# AIC(univariateML::mlexp(x))
# AIC(univariateML::mlgamma(x))
# AIC(univariateML::mlgumbel(x))



uppers <- seq(0.0, log(5), by = 0.01)
g <- function(upper) {
  indices <- psi < upper
  mean(psi * (1 - indices) + theta * (indices))
}
plot(uppers, sapply(uppers, g), type = "l")
abline(v = log(4) / lambda)

theta <- seq(0.1, 5, by = 0.01)
h <- function(theta) (log(16) - theta) / (theta * 3)
h <- function(theta) 3 / 4 * (3 * theta - 1)
hh <- function(theta) 3 / 4 * (theta)
plot(theta, sapply(theta, h), type = "l")
lines(theta, sapply(theta, hh), type = "l", col = "red")
abline(h = log(8))

# plot(dexp(psi, 0.5) / (dexp(psi, 2) + dexp(psi, 0.5)))

mean((dexp(psi, 0.5) / (dexp(psi, 2) + dexp(psi, 0.5)) - z)^2)

x <- seq(0, 3, by = 0.01)

plot(x, dexp(x, 1 / 2), type = "l", ylim = c(0, 2), col = "red")
lines(x, 2 * dexp(x, 2), type = "l")
abline(v = log(4))

plot(x, log(dexp(x, 1 / 2) / dexp(x, 2)), type = "l", col = "red")
abline(h = log(2))
abline(v = log(4))

pexp(2 / 3 * log(4))
pexp(2 / 3 * log(4))


upper <- log(4)
a <- integrate(function(x) dexp(x, 2) * (3 / 2 * dexp(x, 2) / (dexp(x, 1 / 2) + dexp(x, 2)) * x + 0.5 * x), 0, upper)$value
b <- integrate(function(x) dexp(x, 1 / 2) * (3 / 2 * dexp(x, 2) / (dexp(x, 1 / 2) + dexp(x, 2)) * x + 0.5 * x), 0, upper)$value
c <- integrate(function(x) dexp(x, 1 / 2) * x, upper, Inf)$value
d <- integrate(function(x) dexp(x, 2) * x, upper, Inf)$value
(a + b) / 2 + (c + d) / 2

upper <- log(4)
a <- integrate(function(x) dexp(x, 2) * (3 / 2 * dexp(x, 2) / (dexp(x, 1 / 2) + dexp(x, 2)) * x - 0.5 * x), 0, upper)$value
b <- integrate(function(x) dexp(x, 1 / 2) * (3 / 2 * dexp(x, 2) / (dexp(x, 1 / 2) + dexp(x, 2)) * x - 0.5 * x), 0, upper)$value
(a + b) / 2 + 5 / 4


##
g <- function(upper) {
  indices <- psi < upper
  mean(psi * (1 - indices) + theta * (indices))
}
indices <- psi < upper
mean(psi * (1 - indices) + theta * (indices))


h <- function(psi) {
  3 / 2 * dexp(psi, 2) / (dexp(psi, 1 / 2) + dexp(psi, 2)) * psi - 0.5 * psi
}

psi <- seq(0.01, 1.5, by = 0.01)
plot(psi, sapply(psi, h))

x <- seq(1, 100, by = 0.1)
plot(x, 2 * sqrt(x), type = "l")
n <- 100
i <- seq(1, n)
plot(i, 2 * (sqrt(i) - sqrt(i - 1)) / sqrt(n) / sapply(i, function(i) 1 / (i * n)^(1 / 2)), type = "l")
lines(i, sapply(i, function(i) 1 / (i * n)^(1 / 2)), col = "red")
