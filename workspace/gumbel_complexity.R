x <- extraDistr::rgumbel(1000, 1, 0.001)

x2 <- x^2
sigma0 <- 0.43
neg_sigma_inv <- -1/sigma0
exps <- exp(x * neg_sigma_inv)
psi0 <- sum(exps)
psi1 <- sum(x*exps) / psi0
psi2 <- sum(x2*exps) / psi0 - psi1^2
f <- sigma0 + psi1
df <- 1 + neg_sigma_inv^2 * psi2
f / df

x2 <- x^2
neg_sigma_inv <- -1/sigma0
psi <- exp(-1/sigma0)
microbenchmark::microbenchmark(exp(x * neg_sigma_inv),
                               psi^x,
                               sum(exps),
                               sum(x*exps) / psi0,
                               sum(x^2*exps) / psi0 - psi1^2, times = 1000)