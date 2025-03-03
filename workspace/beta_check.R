x <- rbeta(100, 2, 7)

s <- mean(log(x))
r <- mean(log(1 - x))
g1 <- exp(s)
g2 <- exp(r)
b <- log(0.5 + 0.5 * g2 / (1 - (g1 + g2)))
b <- 1
n <- length(x)

a <- function(b) {
  exp_b <- exp(b)
  digamma_b_r <- digamma(exp_b) - r
  trigamma_b <- trigamma(exp_b)

  # Inverts digamma(s-r+psi(exp(b)))
  y <- s + digamma_b_r
  a <- 1 / log(1 + exp(-y))
  a <- a + (y - digamma(a)) / trigamma(a)
  a <- a + (y - digamma(a)) / trigamma(a)
  a <- a + (y - digamma(a)) / trigamma(a)
  a
}

numDeriv::grad(a, exp(b))
a_ <- a(exp(b))
-trigamma(a_ + exp(b)) / (trigamma(a_) + trigamma(a_ + exp(b)))

digamma(a_) - s + r - digamma(exp(b))
