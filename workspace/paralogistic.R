x <- actuar::rparalogis(1000, 10)

s <- mean(log(x))
n <- length(x)

loglik <- \(alpha) {
  2*n*log(alpha) + (alpha-1)*n*s - (alpha+1)*n*r(alpha)
}

f <- \(alpha) {
  2*n/alpha + n * s - n * r(alpha) - n * (alpha+1)*r_grad(alpha)
}

df <- \(alpha) {
  n * (-2/alpha^2 - 2*r_grad(alpha) - (alpha + 1) * r_hessian(alpha))
}

numDeriv::grad(loglik, 2)
f(2)

numDeriv::hessian(loglik, 2)
df(2)


r <- \(alpha) mean(log(1+x^alpha))
r_grad <- \(alpha) mean(x^alpha * log(x) / (x^alpha + 1))
r_hessian <- \(alpha) mean(x^alpha * log(x)^2 / (x^alpha + 1)) -
  mean((x^alpha * log(x) / (x^alpha + 1))^2)

f_over_df <- \(alpha) {
  r_grad_alpha <- r_grad(alpha)
    f <- 2*n/alpha + n * s - n * r(alpha) - n * (alpha+1)*r_grad(alpha)
    df <- n * (-2/alpha^2 - 2*r_grad(alpha) - (alpha + 1) * r_hessian(alpha))
    f/df
}

shape0 <- max(3/(1-mean(x)), 1)
newton_raphson_1d(f_over_df, 5)


alphas <- seq(4, 6, by = 0.01)
plot(alphas, sapply(alphas, loglik), type = "l")
