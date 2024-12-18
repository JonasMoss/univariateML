x <- rgamma(100, 100, 0.1)
#x <- rbeta(100, 2, 7)
#x <- extraDistr::rgumbel(100, 1, 1)
mlgumbel_(x)

res <- replicate(10000, {
  x <- extraDistr::rgumbel(100, 3, 2)
  mlgumbel_(x)$estimates
})



mlgumbel_2(x)

microbenchmark::microbenchmark(mlgumbel_2(x), mlgumbel_(x))

y <- seq(0.01, 0.25, 0.01)
mean(sapply(y, g))

#mu + beta*digamma(1) = mean(x)
#mu - beta*log(log(2)) = median(x)

#beta*digamma(1) + median(x) + beta * log(log(2) = mean(x)
beta <- (mean(x) -  median(x)) / (digamma(1) + log(log(2)))

# mu = qnorm(0.25) + beta*log(-log(0.25))
# qnorm(0.75) - qnorm(0.25) = beta*log(-log(0.25)) -  beta*log(-log(0.75))

g <- \(p) (quantile(x, p) - quantile(x, 1-p))/(log(-log(1-p)) - log(-log(p)))


f <- Vectorize(\(shape) {
  shape_mean <- mean(x^shape)
  scale <- shape_mean^(1 / shape)
  sum(dweibull(x, shape, scale, log = TRUE))
})

y <- seq(0.0001, 0.1, by = 0.001)
plot(y, f(y))

y <- -log(x)


beta <- sqrt(var(x) * 6) / pi
mu <- mean(x) + beta*digamma(1)


x1 <- extraDistr::rgumbel(100, 100, 0.001)
mlgumbel(x1)
mlgumbel(x1 - mean(x1))[1] + mean(x1)


x <- extraDistr::rgumbel(1000, 100, 0.001)

hist(x)
hist((x - mu)/beta)
hist(extraDistr::rgumbel(1000, 0, 1))

x <- extraDistr::rgumbel(100, 0.01, 0.001)
mlgumbel_(x)
mlgumbel_2(x)
microbenchmark::microbenchmark(mlgumbel_(x), mlgumbel_2(x), mlgumbel_3(x), times = 10000)

mu <- mean(x) + beta*digamma(1)
beta <- sqrt(var(x) * 6) / pi
mlgumbel(x)
mlgumbel((x - mu)/beta)

mu <- mean(x) + beta*digamma(1)
mlgumbel((x - mu)/beta)[2] * beta
mlgumbel((x - mu)/beta)[1]*beta + mu
mlgumbel(x)

mlgumbel_2 <- \(x, ...) {
  x_bar <- sum(x) / length(x)
  sd_x <- sd(x)
  beta <- sd_x * sqrt(6) / pi
  mu <- x_bar

  estimates <- mlgumbel_estimate2((x - mu) / sd_x)
  mu <- estimates[1] * sd_x + mu
  sigma <- estimates[2] * sd_x

  logLik <- -length(x) * (log(sigma) + (x_bar - mu) / sigma + 1)

  list(estimates = c(mu = mu, sigma = sigma), logLik = logLik)
}

mlgumbel_estimate2 <- \(x, ...) {
  f_over_df <- \(sigma0) {
    inv_sigma0 <- 1 / sigma0
    exps <- exp(-x * inv_sigma0)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps) / psi0
    psi2 <- sum(x^2 * exps) / psi0 - psi1^2
    f <- -sigma0 - psi1
    df <- -1 - inv_sigma0^2 * psi2
    f / df
  }
  sigma <- newton_raphson_1d(f_over_df, 1, ...)
  mu <- -sigma * log(sum(exp(-x / sigma)) / length(x))
  c(mu, sigma)
}


mlgumbel_3 <- \(x, ...) {
  x_bar <- sum(x) / length(x)
  estimates <- mlgumbel_estimate3(x - x_bar, ...)
  mu <- estimates[1] + x_bar
  sigma <- estimates[2]
  logLik <- -length(x) * (log(sigma) + (x_bar - mu) / sigma + 1)
  list(estimates = c(mu = mu, sigma = sigma), logLik = logLik)
}

mlgumbel_estimate3 <- \(x, ...) {
  f_over_df <- \(sigma0) {
    inv_sigma0 <- 1 / sigma0
    exps <- exp(-x * inv_sigma0)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps) / psi0
    psi2 <- sum(x^2 * exps) / psi0 - psi1^2
    f <- -sigma0 - psi1
    df <- -1 - inv_sigma0^2 * psi2
    f / df
  }

  dots <- list(...)
  sigma0 <- if (!is.null(dots$sigma0)) dots$sigma0 else sqrt(var(x) * (length(x) - 1) / (length(x) - 2.1) * 6) / pi

  sigma <- newton_raphson_1d(f_over_df, sigma0, ...)
  mu <- -sigma * log(sum(exp(-x / sigma)) / length(x))
  c(mu, sigma)
}







