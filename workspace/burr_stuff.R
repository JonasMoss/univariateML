i <- 1
res <- t(sapply(seq(10000), function(i) {
  set.seed(i)
  x <- actuar::rburr(100, runif(1, 0.5, 100), runif(1, 0.5, 100))
  e <- mean(log(x))
  s <- mean(log(x)^2)
  c(e, s, unname(c(mlburr(x)[2])))
  # print(i)
}))
