theta1 = theta2 = 0.00000001

x <- MASS::mvrnorm(1000000, c(0, 0), diag(2))
sqrt_p_theta <- sqrt(dnorm(x[, 1], theta1 * theta2^(1/3)) * dnorm(x[, 2], theta2))
sqrt_p_0 <- sqrt(dnorm(x[, 1]) * dnorm(x[, 2]))

mean((sqrt_p_theta / sqrt_p_0 - 1 - theta2*x[, 2])^2) / theta1^2



h <- 1
x <- seq(-3, 3, by = 0.001)
p <- \(x, n, h = 1) dnorm(x, h/sqrt(n))
p0 <- \(x) p(x, 1, 0)
q <- \(x) dnorm(x, 0, sqrt(2))

plot(x, q(x) / p(x, 1, 0))
lines(x, sqrt(1/2)*exp(1/4*x^2), col = "red")

n <- 10000
f <- \(x) n * (sqrt(p(x, n = n)) - sqrt(p0(x))) * q(x)/p0(x)
a = 2*integrate(\(x) n * (sqrt(p(x, n = n)) - sqrt(p0(x))) * sqrt(p0(x)), -Inf, Inf)$value
b = 4*integrate(\(x) n * (sqrt(p(x, n = n)) - sqrt(p0(x)))^2, -Inf, Inf)$value
a - 1/4 * b

a = 2*integrate(\(x) n * (sqrt(p(x, n = n)) - sqrt(p0(x))) * q(x) / sqrt(p0(x)), -10, 10)$value
b = 4*integrate(\(x) n * (sqrt(p(x, n = n)) - sqrt(p0(x)))^2 * q(x) / p0(x), -10, 10)$value

a - b / 4

x <- seq(-10, 10, by = 0.001)
plot(x, n * (sqrt(p(x, n = n)) - sqrt(p0(x)))^2 * q(x) / p0(x))
plot(x, n * (sqrt(p(x, n = n)) - sqrt(p0(x))) * q(x) / sqrt(p0(x)))


###

ff <- \(x, n) sqrt(n) * 2 * (sqrt(p(x, n = n)) - sqrt(p0(x))) / sqrt(p0(x))

### Score


a = 2*integrate(\(x) n * (sqrt(p(x, n = n)) - sqrt(p0(x))) * q(x), -Inf, Inf)$value
b = 4*integrate(\(x) n * (sqrt(p(x, n = n)) - sqrt(p0(x)))^2 * q(x), -Inf, Inf)$value
