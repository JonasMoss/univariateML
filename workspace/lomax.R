x <- extraDistr::rlomax(100, 4, 6)
n <- length(x)

lambda0 <- 2
kappa <- \(lambda) n/sum(log(1 + lambda * x))
dkappa <- \(lambda) -kappa(lambda)^2 * r(lambda)
r <- \(lambda) mean(x/(1+lambda*x))
dr <- \(lambda) -mean(x^2 / (1 + lambda * x)^2)

f <- \(lambda) {
  n / lambda - n*kappa(lambda)*r(lambda) - n*r(lambda)
}
df <- \(lambda) {
  n * kappa(lambda)^2*r(lambda)^2 - n*(kappa(lambda) + 1) * dr(lambda) - n/lambda^2
}

loglik <- \(lambda) sum(extraDistr::dlomax(x,lambda, kappa(lambda), log = TRUE))
kappa_times <- \(lambda) n/sum(log(1 + lambda * x)) * lambda

numDeriv::grad(loglik, lambda0)
f(lambda0)

numDeriv::grad(kappa, lambda0)
dkappa(lambda0)

numDeriv::grad(r, lambda0)
dr(lambda0)

numDeriv::hessian(loglik, lambda0)

df(lambda0)





x<-rexp(10)
n <- length(x)
(1/mean(x)* mean(x^2)/2 - mean(x))
mean(x^2) < 2*mean(x)^2
var(x) * (n-1) / n > mean(x)^2
mllomax(x)
#mean(log(x))

#2*mean(x)
#1/mean(x) * mean(x^2)

f(1e-6) / n

-(-1/mean(x)* mean(x^2)/2 + mean(x)) - f(1e-5)/n
(1/mean(x)* mean(x^2)/2 - mean(x)) - f(1e-5)/n

mean(1/x^2)
mean(x)^2
mean(x^2)/2
1/exp(mean(log(x)))

mllomax(x)


lambdas <- seq(1e-6, 1e-5, by = 1e-10)
plot(lambdas, sapply(lambdas, f), type = "l")

lambdas <- seq(1e-3, 1, by = 1e-3)
plot(lambdas, sapply(lambdas, loglik), type = "l")
plot(lambdas, sapply(lambdas, f), type = "l")
plot(lambdas, sapply(lambdas, df), type = "l")
mllomax(x)

lambdas <- seq(0.001, 1, by = 0.01)
plot(lambdas, sapply(lambdas, loglik))
plot(lambdas, sapply(lambdas, kappa_times))
plot(lambdas, sapply(lambdas, r))