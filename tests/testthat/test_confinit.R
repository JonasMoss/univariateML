context("confint")

object = mlinvgauss(airquality$Wind[1:3])
expect_equal(dim(confint(object)), c(2, 2))
expect_equal(rownames(confint(object)), c("mean", "shape"))
expect_equal(length(confint(object, "mean")), 2)
expect_equal(length(confint(object, level = 0.5, "shape")), 2)
expect_error(confint(object, "variance"))