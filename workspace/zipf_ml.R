N <- 10
shape <- 1.5
x <- sads::rzipf(100, N, shape)
#N <- max(x)
sum_lx <- sum(log(x))
log_seq <- log(seq(N))
log_seq2 <- log(seq(N))^2

n <- length(x)
f <- \(shape) -shape * sum_lx - n * log(sum(1/seq(1, N)^shape))

grad <-\(shape) -sum_lx + n*sum(1/seq(1, N)^shape * log_seq) / sum(1/seq(1, N)^shape)
hessian <-\(shape) {
  -n*sum(1/seq(1, N)^shape * log_seq2) / sum(1/seq(1, N)^shape) + (grad(shape) + sum_lx)^2/n
}

reltol <- .Machine$double.eps^0.25
numDeriv::grad(f, 0.4)
grad(0.4)
numDeriv::hessian(f, 0.3)
hessian(0.3)

shape0 <- 1
for (i in 1:iterlim) {
  shape <- shape - grad(shape) / hessian(shape)
  if (abs((shape - shape0) / shape0) < reltol) break
  shape0 <- shape
}


grad(2) / hessian(2)
shape = 2

sum_shape <- sum(1/seq(1, N)^shape)
sum_log_hs <- sum(1/seq(1, N)^shape * log_seq)
sum_log2_hs <- sum(1/seq(1, N)^shape * log_seq2)
top = sum_log_hs / sum_shape
bottom = -n*sum_log2_hs / sum_shape + n*top^2

(n*top - sum_lx)/bottom

shape0 <- 1
for (i in 1:iterlim) {
  sum_shape <- sum(1/seq(1, N)^shape)
  sum_log_hs <- sum(1/seq(1, N)^shape * log_seq)
  sum_log2_hs <- sum(1/seq(1, N)^shape * log_seq2)
  top = sum_log_hs / sum_shape
  bottom = -n*sum_log2_hs / sum_shape + n*top^2
  shape <- shape - (n*top - sum_lx)/bottom
  if (abs((shape - shape0) / shape0) < reltol) break
  shape0 <- shape
}
