context("confint")

object = mlinvgauss(airquality$Wind[1:3])
expect_equal(dim(confint(object)), c(2, 2))
expect_equal(rownames(confint(object)), c("mean", "shape"))
expect_equal(length(confint(object, "mean")), 2)
expect_equal(length(confint(object, level = 0.5, "shape")), 2)
expect_error(confint(object, "variance"))

set.seed(313)
I1 = confint(object)
set.seed(313)
I2 = bootstrapml(object)
expect_equal(I1, I2)