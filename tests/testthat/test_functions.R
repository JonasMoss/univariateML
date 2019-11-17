context("functions")

set.seed(313)
x <- rnorm(10, 2, 3)
obj <- mlnorm(x)

expect_equal(dml(1, obj), dnorm(1, mean = obj[1], sd = obj[2]))
expect_equal(pml(1, obj, log.p = TRUE), pnorm(1, mean = obj[1], sd = obj[2], log.p = TRUE))
expect_equal(qml(0.9, obj, lower.tail = FALSE), qnorm(0.9, mean = obj[1], sd = obj[2], lower.tail = FALSE))
expect_equal((function() {
  set.seed(313)
  rml(1, obj)
})(), (function() {
  set.seed(313)
  rnorm(1, obj[1], obj[2])
})())
