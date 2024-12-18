## Easy and not, perhaps, very interesting

x <- actuar::rinvexp(1000, 5)

theta <- mean(1/x)
n <- length(x)
n*log(1/theta) - n - 2*sum(log(x))

sum(actuar::dinvexp(x, theta, log = TRUE))