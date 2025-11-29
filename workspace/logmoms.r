logmom1 <- function(mu, sigma2) {
  psi1 <- digamma(1)
  psi1_1 <- trigamma(1)

  f_over_df <- function(c) {
    k <- inverse_digamma(digamma(1) - c * mu)

    psi1_k <- trigamma(k)
    psi2_k <- psigamma(k, deriv = 2)

    c_2 <- c^(-2)
    c_3 <- c^(-3)
    term1_prime <- -2 * c_3 * (psi1_1 + psi1_k)
    term2_prime <- c_2 * (psi2_k * (-mu / psi1_k))

    f <- c_2 * (psi1_1 + psi1_k) - sigma2
    df <- term1_prime + term2_prime
    f / df
  }

  c <- univariateML:::newton_raphson_1d(f_over_df, pi / sqrt(3 * sigma2))
  c(k = inverse_digamma(psi1 - c * mu), c = c)
}


logmom2 <- function(mu, sigma2) {
  psi1 <- digamma(1)
  psi_prime1 <- trigamma(1)
  eta_sq <- mu^2 / sigma2
  f_over_df <- function(k) {
    k <- exp(k)
    psik <- digamma(k)
    psi_primek <- trigamma(k)
    psi_primeprimek <- psigamma(k, deriv = 2)

    term1_g <- psi1 - psik
    g_k <- term1_g^2 - eta_sq * (psi_prime1 + psi_primek)
    g_prime_k <- -2 * psi_primek * term1_g - eta_sq * psi_primeprimek
    g_k / (g_prime_k * k)
  }

  k0 <- inverse_digamma(psi1 - pi / sqrt(3 * sigma2) * mu)
  k <- exp(univariateML:::newton_raphson_1d(f_over_df, log(k0)))
  c(k = k, c = (psi1 - digamma(k)) / mu)
}

two_mods <- \(x) {
  mu <- mean(log(x))
  sigma2 <- var(log(x))
  eta_sq <- mu^2 / sigma2

  list(
    "logmom1" = logmom1(mu, sigma2),
    "logmom2" = logmom2(mu, sigma2)
  )
}


set.seed(1)
x <- actuar::rburr(10000, 0.1, 0.1)
x <- x[is.finite(x)]

two_mods(x)
mu <- mean(log(x))
sigma2 <- var(log(x))
eta_sq <- mu^2 / sigma2

# x <- actuar::rburr(1000, 3, 10)
# x <- x[is.finite(x)]



kk <- seq(2, 5, by = 0.01)
plot(kk, f_over_df(log(kk)), type = "l")
abline(h = 0)
abline(v = 2.733974)

kk <- seq(0.001, 0.2, by = 0.01)
plot(kk, f_over_df(log(kk)), type = "l")
abline(h = 0)
abline(v = 0.1131889)


g_over_dg <- function(k) {
  psik <- digamma(k)
  psi_primek <- trigamma(k)
  psi_primeprimek <- psigamma(k, deriv = 2)

  term1_g <- psi1 - psik
  g_k <- term1_g^2 - eta_sq * (psi_prime1 + psi_primek)
  g_prime_k <- -2 * psi_primek * term1_g - eta_sq * psi_primeprimek
  g_k / g_prime_k
}

kk <- seq(0.001, 5, by = 0.01)
plot(kk, g_over_dg(kk), type = "l")
abline(h = 0)


cc <- seq(0.1, 2, by = 0.1)
plot(cc, sapply(cc, f), type = "l")
abline(h = 0)



mu <- 100
sigma <- 101
sigma2 <- sigma^2

logmom1(mu, sigma2)

f <- function(c) {
  psi1 <- digamma(1)
  psi1_1 <- trigamma(1)
  k <- inverse_digamma(digamma(1) - c * mu)
  psi1_k <- trigamma(k)
  psi2_k <- psigamma(k, deriv = 2)
  f <- c^(-2) * (psi1_1 + psi1_k) - sigma2
  f
}

mu <- 20
sigma <- sqrt(400)
sigma2 <- sigma^2

logmom1(mu, sigma2)


cc <- seq(logmom1(mu, sigma2)[2]/100, logmom1(mu, sigma2)[2]*2, length.out = 100)
plot(cc, sapply(cc, f), type = "l")
abline(h = 0)



mu = 0.763
sigma2 = 40.06
logmom1(mu, sigma2)


cc <- seq(logmom1(mu, sigma2)[2]/2, logmom1(mu, sigma2)[2]*2, length.out = 100)
plot(cc, sapply(cc, f), type = "l")
abline(h = 0)


mu = 0.694
sigma2 = 1.645

cc <- seq(logmom1(mu, sigma2)[2]/2, logmom1(mu, sigma2)[2]*2, length.out = 100)
plot(cc, sapply(cc, f), type = "l")
abline(h = 0)



mu = 0.763
sigma2 = 40.06
microbenchmark::microbenchmark(logmom1(mu, sigma2),logmom1_rcpp(mu, sigma2))


microbenchmark::microbenchmark(logmom1(mu, sigma2),logmom1_internal_c_lean(mu, sigma2))

microbenchmark::microbenchmark(inverse_digamma(5), inverse_digamma_scalar(5))

