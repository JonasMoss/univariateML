context("mlgumbel")
set.seed(313)
data1 = extraDistr::rgumbel(100, 1, 1)
data2 = extraDistr::rgumbel(10, 3, 7)

mle1 = nlm(function(p) {
  -mean(extraDistr::dgumbel(data1, p[1], p[2], log = TRUE))
}, p = c(1,1))$estimate

mle2 = nlm(function(p) {
  -mean(extraDistr::dgumbel(data2, p[1], p[2], log = TRUE))
}, p = c(3,7))$estimate

expect_equal(mle1, as.numeric(mlgumbel(data1)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlgumbel(data2)), tolerance = 1e-5)
expect_warning(mlgumbel(data2, iterlim = 1))