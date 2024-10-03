set.seed(313)
x <- rbinom(1000, 50, 0.8)

ind <- max(x):100
z <- 0 * ind
i = 1
for (size in ind) {
  prob <- mean(x) / size
  #z[i] <- sum(dbinom(x, size, prob, log = TRUE))
  z[i] <- sum(x * log(prob)) + sum((size - x) * log(1-prob)) + sum(lchoose(size, x))
  i = i + 1
}

plot(ind, z)


f <- \(size){
  prob <- mean(x) / size
  sum(x * log(prob)) + sum((size - x) * log(1-prob)) + sum(lchoose(size, x))
}

size <- seq(max(x), 100, length.out= 1000)

# Use newton-raphson on this.
