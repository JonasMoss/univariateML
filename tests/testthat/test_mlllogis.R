context("mlllogis")

## Data generation.
set.seed(313)
small_data <- actuar::rllogis(100, 2, 3)
tiny_data <- actuar::rllogis(10, 1, 1)

## Finds errors with na and data out of bounds.
expect_error(mlllogis(c(tiny_data, NA)))
expect_error(mlllogis(c(tiny_data, 0)))
expect_error(mlllogis(c(tiny_data, -1)))

# Check correctness
obj_1 <- mllogis(log(tiny_data))
obj_2 <- mlllogis(tiny_data)

expect_equal(
  unname(obj_1[1]),
  unname(-log(obj_2)[2])
)

expect_equal(
  unname(obj_1[2]),
  unname(1 / obj_2[1])
)

## Checks that na.rm works as intended.
expect_equal(
  coef(mlllogis(small_data)),
  coef(mlllogis(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlllogis(small_data, na.rm = TRUE)
expect_equal(
  sum(actuar::dllogis(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Loglogistic")
expect_equal(class(est), "univariateML")

# Check names.
expect_equal(names(est), c("shape", "rate"))
