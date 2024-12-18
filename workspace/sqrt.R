y <- 2

# g <- \(x) {
#   (x^3 - x*y) / (2*y)
# }

# g <- \(x) {
#   (x^3 - x*y) / (3*x^2 - y)
# }

g <- \(x) {
  (x*(x^2-y))*(x*y + 1)/(2*x^3*y+x^2+y)
}

h <- \(x) {
  (x^2 - y) / (2*x)
}

f <- \(x) {
  x* (x^2 - y) / (x^2 + y)
}

f <- \(x) {
  p <- 1/(2*x)
  f <- (x^2 - y)
  df <- 2*x
  f / (df - p * f)
  2*(x^3 - x*y) / (3*x^2 + y)
}

x0 <- 7
newton_raphson_1d(g, x0)
newton_raphson_1d(h, x0)
newton_raphson_1d(f, x0)



x <- seq(.1, 2, by = 0.000001)
y <- sqrt(x)
mod <- lm(y~x + I(x^2))
mod <- lm(y~x)
plot(x, y)
lines(x, predict(mod), col = "red")
