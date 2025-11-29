set.seed(123)
k_gram <- \(n) {
  k <- n
  K_gram <- matrix(0, n, n)
  for (p in 1:n) {
    for (q in 1:n) {
      K_gram[p,q] = (min(p,q) + k) * (n + 1 - max(p,q) + k)
    }
  }
  K_gram/(3*n + 1)
}

sapply(2:15, \(i) solve(k_gram(i))[1])
sapply(2:15, \(i) (i+2)/(i+1))

set.seed(123)
k_gram <- \(n, k = n) {
  K_gram <- matrix(0, n, n)
  for (p in 1:n) {
    for (q in 1:n) {
      K_gram[p,q] = (min(p,q) + k) * (n + 1 - max(p,q) + k)
    }
  }
  K_gram / (n + 1 + 2*k)
}

solve(k_gram(3, 7))
solve(k_gram(4, 9))


sapply(2:15, \(i) solve(k_gram(i, 11))[1])
sapply(2:15, \(i) (i+2)/(i+1))

tridiag <- function(upper, lower, main){
  out <- matrix(0,length(main),length(main))
  diag(out) <- main
  indx <- seq.int(length(upper))
  out[cbind(indx+1,indx)] <- lower
  out[cbind(indx,indx+1)] <- upper
  return(out)
}

L <- tridiag(rep(-1,n-1), rep(-1,n-1), c(1,rep(2, n-2), 1))
solve(L)

