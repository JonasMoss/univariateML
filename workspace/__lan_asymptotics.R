shape = 2
rate = 5

m <- 10000000
x <- rgamma(m, shape, rate)

lambda <- 1/mean(x)
score <- \(lambda) 1/lambda - x

k <- var(score(lambda))
j <- 1/lambda^2

n <- 1000
n_reps <- 500000
h <- 1

results <- replicate(n_reps, {
  x <- rgamma(n, shape, rate)
  sum(2 * sqrt(dexp(x, rate = lambda + h/sqrt(n))) / sqrt(dexp(x, rate = lambda)) - 2)
})

mean(results) # -0.06967542
0.5*(0.5*k-j) - 1/sqrt(n_reps) # [1] -0.06998623
0.5*(0.5*k-j) # -0.05998623
-j / 4 # [1] -0.03999391
-k / 4 # [1] -0.02000158

var(results) # [1] 0.08093325
k # [1] 0.08000632