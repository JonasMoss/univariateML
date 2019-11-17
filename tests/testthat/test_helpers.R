context("helpers")

set.seed(313)
x <- rnorm(10, 2, 3)
obj <- mlnorm(x)

expect_equal(univariateML_to_string(obj, "d"), "stats::dnorm")
expect_equal(univariateML_to_string(obj, "p"), "stats::pnorm")
expect_equal(univariateML_to_string(obj, "q"), "stats::qnorm")
expect_equal(univariateML_to_string(obj, "r"), "stats::rnorm")
expect_equal(univariateML_to_string(obj, "ml"), "mlnorm")

expect_equal(
  names(formals(univariateML_to_function(obj, "d"))),
  c("x", "mean", "sd", "log")
)
expect_equal(
  names(formals(univariateML_to_function(obj, "p"))),
  c("q", "mean", "sd", "lower.tail", "log.p")
)
expect_equal(
  names(formals(univariateML_to_function(obj, "q"))),
  c("p", "mean", "sd", "lower.tail", "log.p")
)
expect_equal(
  names(formals(univariateML_to_function(obj, "r"))),
  c("n", "mean", "sd")
)
expect_equal(
  names(formals(univariateML_to_function(obj, "ml"))),
  c("x", "na.rm")
)
