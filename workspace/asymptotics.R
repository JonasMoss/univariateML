set.seed(3)
x <- rexp(11)
n <- length(x)

x <- extraDistr::rgompertz(10000, 1, 0.000001)

mean(x)
sd(x) * sqrt((n - 1) / n)

f_a <- function(b) {
  exp_bx <- exp(b * x)
  sum_bx <- sum(exp_bx)
  s <- sum_bx - n
  b * n / s
}

b <- 0.001
numDeriv::grad(f_a, b)
numDeriv::hessian(f_a, b)

(n * numDeriv::grad(f_a, b) + sum(x)) / (n * numDeriv::hessian(f_a, b))


da / b - a / b^2 - n * b * sum(x^2 * exp(b * x)) / s^2 + 2 * n * b * sum(x * exp(b * x))^2 / s^3 - n * sum(x * exp(b * x)) / s^2


g <- function(b) {
  a <- f_a(b)
  sum(extraDistr::dgompertz(x, a, b, log = TRUE))
}

numDeriv::grad(g, b)
numDeriv::grad(f_a, b) / f_a(b) * n
numDeriv::hessian(g, 0.003037)


n <- length(x)
x_sum <- sum(x)

f <- function(b) {
  if (FALSE) {
    a <- n / x_sum
    da <- -n / 2 * sum(x^2) / sum(x)^2
    d2a <- n * (1 / 2 * sum(x^2)^2 - 1 / 3 * (x_sum * sum(x^3))) / x_sum^3
    df <- n * (a * d2a - da^2) / a^2
  }

  exp_bx <- exp(b * x)
  sum_bx <- sum(exp_bx)
  sum_bx_x <- sum(x * exp_bx)
  sum_bx_x2 <- sum(x^2 * exp_bx)
  s <- sum_bx - n
  r <- b * sum_bx_x - s

  a <- b * n / s

  # da/b - a/b^2 - n*b*sum(x^2*exp(b*x)) / s^2 + 2*n*b*sum(x*exp(b*x))^2 / s^3 - n*sum(x*exp(b*x)) / s^2


  da <- -n * r / s^2
  # d2a <- n / s^2 * (b * (2 * sum_bx_x^2 / s - sum_bx_x2) - 2 * sum_bx_x)
  x_sum - a / b^2 * r + n * da / a - da / b * s

  # f <- x_sum - a / b^2 * r + n * da / a - da / b * s
  # df <- n * (a * d2a - da^2) / a^2
}


b <- seq(-0.005, 0.005, by = 0.00001)
plot(b, sapply(b, f), type = "l")
abline(v = 0.003037)
abline(h = 0)

b <- seq(-0.1, 0.1, by = 0.001)
plot(b, sapply(b, f), type = "l")
