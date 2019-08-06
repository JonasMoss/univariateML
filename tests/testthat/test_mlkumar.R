context("mlkumar")
set.seed(313)
data1 = extraDistr::rkumar(100, 4, 4)
data2 = extraDistr::rkumar(10, 3, 7)
data3 = c(0, data2)
data3 = c(data2, 1)

mle1 = nlm(function(p) {
  -mean(extraDistr::dkumar(data1, p[1], p[2], log = TRUE))
}, p = c(4, 4))$estimate

mle2 = nlm(function(p) {
  -mean(extraDistr::dkumar(data2, p[1], p[2], log = TRUE))
}, p = c(3, 7))$estimate

expect_equal(mle1, as.numeric(mlkumar(data1)), tolerance = 1e-4)
expect_equal(mle2, as.numeric(mlkumar(data2)), tolerance = 1e-4)
expect_warning(mlkumar(data2, iterlim = 1))
expect_error(mlkumar(data3))
expect_error(mlkumar(data4))

