xi <- \(k, p) {
  top <- psigamma(1, p-1) + (-1)^p*psigamma(k, p-1)
  bottom <- (trigamma(1) + trigamma(k))^(p/2)
  top / bottom
}


uu <- seq(-4, 4, by = 0.001)
yy <- log(1-xi(exp(uu), 1))
summary(lm(yy ~ uu + I(uu^2) + exp(uu)))





uu <- seq(-5, 5, length.out = 100)
plot(uu, log(1-xi(exp(uu), 1)), type = "l", lwd = 2)
lines(uu, 2*uu + log(pi^2/3), col = "red")
lines(uu, log(uu) + (pi/sqrt(6) - digamma(1))/uu +log(sqrt(6)/pi), col = "red")

uu <- seq(-20, 100, length.out = 100)
zz <- log(1-xi(exp(uu), 1)) - (log(uu) + (pi/sqrt(6) - digamma(1))/uu +log(sqrt(6)/pi))
plot(uu, zz, type = "l")


cutoff <- 0

f <- \(x) ifelse(x > cutoff, log(1-x), -x)

#f <- \(x) log(1-x)

uu <- seq(-5, 5, length.out = 1000)
plot(uu, f(xi(exp(uu), 1)), type = "l", lwd = 2)
abline(v = 0, lty = 2, col = "grey")
lines(uu, 2*uu + log(pi^2/3), col = "red")