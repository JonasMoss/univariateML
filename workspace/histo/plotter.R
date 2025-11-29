set.seed(123)
n <- 100
x_orig <- seq(-2, 2, length.out = n)
y_orig <- sin(x_orig) + rnorm(n, 0, 0.2)

# "tuning" parameters for kernel. We want to find the BEST ones for ALL problems.
k <- 10*n

K_gram <- matrix(0, n, n)
for (p in 1:n) {
  for (q in 1:n) {
    K_gram[p,q] = (min(p,q) + k) * (n + 1 - max(p,q) + k)
  }
}
K_gram <- K_gram/(n + 2*k +1)

lambda <- 100
A_matrix <- K_gram + lambda * diag(n)
alpha <- solve(A_matrix, y_orig)
y_hat_at_data_points <- K_gram %*% alpha

plot(x_orig, y_orig, pch=19, cex=0.7, col="gray",
     main=paste("Regression with Interval-Sharing Kernel (lambda=", signif(lambda,3), ")"))
lines(x_orig, y_hat_at_data_points, type="s", col="red", lwd=2)
lines(x_orig, sin(x_orig), col="blue", lty=2, lwd=1.5) # True sine wave