size <- 60
alpha <- 7
beta <- 9
p <- c(size+1, alpha-1, beta+2)

x <- extraDistr::rbnbinom(1000, size, alpha, beta)

l <- \(p) {
  size <- p[1]
  alpha <- p[2]
  beta <- p[3]
  sum(extraDistr::dbnbinom(x, size, alpha, beta, log = TRUE))
}

grad <- \(p) {
  n <- length(x)
  r <- p[1]
  a <- p[2]
  b <- p[3]

  di_bx <- sum(digamma(b+x))
  di_rx <- sum(digamma(r+x))
  di_abrx <- sum(digamma(a+b+r+x))

  di_a <- n * digamma(a)
  di_b <- n * digamma(b)
  di_r <- n * digamma(r)
  di_ab <- n * digamma(a + b)
  di_ar <- n * digamma(a + r)

  u_r <- di_rx - di_abrx + di_ar - di_r
  u_a <- di_ab - di_abrx + di_ar - di_a
  u_b <- di_ab - di_abrx + di_bx - di_b

  c(u_r, u_a, u_b)
}

numDeriv::grad(l, p)
grad(p)

hessian <- \(p) {
  n <- length(x)
  r <- p[1]
  a <- p[2]
  b <- p[3]

  tri_bx <- sum(trigamma(b+x))
  tri_rx <- sum(trigamma(r+x))
  tri_abrx <- sum(trigamma(a+b+r+x))

  tri_a <- n * trigamma(a)
  tri_b <- n * trigamma(b)
  tri_r <- n * trigamma(r)
  tri_ab <- n * trigamma(a + b)
  tri_ar <- n * trigamma(a + r)

  u_rr <- tri_rx - tri_abrx + tri_ar - tri_r
  u_aa <- tri_ab - tri_abrx + tri_ar - tri_a
  u_bb <- tri_ab - tri_abrx + tri_bx - tri_b
  u_ra <- -tri_abrx + tri_ar
  u_rb <- -tri_abrx
  u_ab <- tri_ab - tri_abrx

  out <- matrix(0, 3, 3)
  diag(out) <- c(u_rr, u_aa, u_bb)
  out[1, 2] <- out[2, 1] <- u_ra
  out[1, 3] <- out[3, 1] <- u_rb
  out[2, 3] <- out[3, 2] <- u_ab
  out
}

numDeriv::hessian(l, p)
hessian(p)

nlm(l, p)$estimate

attr(l, "hessian") = hessian
attr(l, "gradient") = grad
nlm(l, p)$estimate