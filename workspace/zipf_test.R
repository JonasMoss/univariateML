set.seed(313)
N <- sample(50, 1000, replace = TRUE)
s <- rexp(1000, 1/10)

fun <- \(i) {
  print(i)
  x <- sads::rzipf(1000, N[i], s[i])
  mlzipf(x)
  tryCatch(mlzipf(x), error = \(cond) conditionMessage(cond))
}

sapply(seq_along(s), fun)

mlzipf(x)

z <- seq(0.001, 2, by = 0.01)
plot(z, sapply(z, f))


obj <- do.call(paste0("ml", "unif"), list(x))