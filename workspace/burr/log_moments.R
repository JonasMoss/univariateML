#' Solution for eta = mu^2/sigma^2 in lmom equation
#' @param k Parameter of Burr XII distribution. Is positive
#' @return The value of eta
eta <- \(k) (digamma(k) - digamma(1))^2 / (trigamma(1) + trigamma(k))

#' Solution for eta = mu^2/sigma^2 in lmom equation
#' @param k Parameter of Burr XII distribution. Is positive
#' @return The value of eta
kappa <- \(k) (psigamma(1,2) - psigamma(k,2)) / (trigamma(k) + trigamma(1))^(3/2)


#' Calculate statistics for the logmoment 2-Burr problem.
#' @param x Data.
#' @return Various statistics related to logmoment 2-Burr.
calcer <- \(x) {
  mu <- mean(log(x))
  sigma2 <- var(log(x)) * (length(x) - 1) / length(x)
  cv <- mu^2/sigma2
  c <- logmom1_c(mu, sigma2)
  c(mu = mu, c = c, cv = cv, sigma2 = sigma2, lim = exp(mu - sqrt(sigma2)))
}


#' Solution for kappa in 3-burr
#' @param u Log of k parameter of Burr XII distribution. Is positive.
#' @return The value of kappa / kappa_cdf.
lim_inf <- psigamma(1, 2) / (trigamma(1))^(3/2)
lim_neg_inf <- 2
kappa <- \(u) (psigamma(1,2) - psigamma(exp(u),2)) / (trigamma(exp(u)) + trigamma(1))^(3/2)
kappa_cdf <- \(u) (lim_neg_inf - kappa(u)) / (lim_neg_inf - lim_inf)

kappa_deriv <- function(u) {
  eu <- exp(u)
  D <- psigamma(eu, 1) + psigamma(1, 1)
  term1 <- -psigamma(eu, 3) * D
  term2 <- -1.5 * psigamma(eu, 2) * (psigamma(1, 2) - psigamma(eu, 2))
  numerator <- eu * (term1 + term2)
  denominator <- D^(5/2)
  return(numerator / denominator)
}

# --- Analytical Derivative of kappa_cdf(u) ---
# kappa_cdf'(u) = -kappa'(u) / (lim_neg_inf - lim_inf)
kappa_cdf_deriv <- function(u) {
  kd <- kappa_deriv(u)
  denom <- lim_neg_inf - lim_inf
  return(-kd / denom)
}

kappa_cdf_inv<- function(p, start_u, tol = 1e-8, max_iter = 100) {
  u <- start_u
  for (i in 1:max_iter) {
    f_u <- kappa_cdf(u) - p
    f_prime_u <- kappa_cdf_deriv(u)
    u_new <- u - f_u / f_prime_u
    if (abs(u_new - u) < tol) {
      return(u_new)
    }
    u <- u_new
  }
  print(i)
  return(u)
}

kappa_inv <- function(k, start_u, tol = 1e-8, max_iter = 100) {
  u <- start_u
  for (i in 1:max_iter) {
    f_u <- kappa(u) - k
    f_prime_u <- kappa_deriv(u)
    u_new <- u - f_u / f_prime_u
    if (abs(u_new - u) < tol) {
      print(paste0("iter: ", i))
      return(u_new)
    }
    u <- u_new
  }
  print(paste0("iter: ", i))
  return(u)
}



C <- trigamma(1)               #  ψʹ(1)
A <- psigamma(1, deriv = 2)    #  ψʺ(1)   (negative)
K <- 2 - A / C^(1.5)           #  > 0
c.left  <-  K / (3 * C)
c.right <- -3 * A / (2 * K * C^(2.5))
uu <- seq(log(1e-4),log(1e4), length.out = 1000)

plot(uu, qlogis(kappa_cdf(uu)), type = "l")
lines(uu, -log(1-kappa_cdf(uu)), col = "red")
lines(uu, log(kappa_cdf(uu)), col = "blue")
lines(uu, uu - log(c.right), col = "green")
lines(uu, 2*uu + log(6*C/K), col = "purple")




uu <- seq(log(1e-3),log(1e3), length.out = 1000)
plot(uu, kappa_cdf(uu), type = "l")

yy <- log(kappa(kk) - const)
fit <- lm(yy ~ log(kk) + kk)
summary(fit)













xi <- \(k) (psigamma(1,2) - psigamma(k,2)) / (trigamma(k) + trigamma(1))^(3/2)
kk <- exp(seq(log(1e-3),log(1e3), length.out = 1000))
const <- xi(100000)
f <- \(kk) -(xi(kk) - 2) / (2-const)
plot(kk, -(xi(kk) - 2) / (2-const), type = "l", log = "x")
plot(kk, sapply(kk, \(k) numDeriv::grad(f, k)), log = "x", type = "l")




xi <- \(k) (psigamma(1,2) - psigamma(exp(k),2)) / (trigamma(exp(k)) + trigamma(1))^(3/2)
f <- \(k) -(xi(k) - 2) / (2-const)


kk <- seq(-10,-2, length.out = 1000)
plot(kk, log(-xi(exp(kk))), type = "l")
fit <- lm(I(log(-xi(exp(kk)))) ~ kk)
lines(kk, predict(fit), col = "red")

max((predict(lm(I(log(-xi(exp(kk)))) ~ kk)) - log(-xi(exp(kk))))/abs( log(-xi(exp(kk)))))



f <- \(kk) -(xi(kk) - 2) / (2-const)
theta <- seq(-10,3, length.out = 1000)
plot(theta, log(1-f(exp(theta))), type = "l")
fit <- lm(I(log(1-f(exp(theta)))) ~ exp(theta) + theta + exp(-theta))
summary(fit)
lines(theta, predict(fit), col = "red")

max((predict(fit) - log(1-f(exp(theta))))/abs(log(1-f(exp(theta)))))





plot(kk, -(xi(kk) - 2) / (2-const), type = "l", log = "y")
lines(kk, extraDistr::pgumbel(kk, -0.7354553, 0.9195635), col = "red")

plot(kk, 1+(xi(kk) - 2) / (2-const), type = "l", log = "y")
lines(kk, extraDistr::pgumbel(kk, -0.7354553, 0.9195635, lower.tail = FALSE), col = "red")


plot(kk, sapply(kk, \(k) numDeriv::grad(f, k)), type = "l", log = "y")
lines(kk, extraDistr::dgumbel(kk, -0.7354553, 0.9195635), col = "red")

