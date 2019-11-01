context("bootstrap")

# Gamma data.
set.seed(313)
x = rgamma(10, 2, 3)
obj = mlgamma(x)
obj2 = mlexp(x)

# Different reducer and map.
set.seed(313)
expect_equal(c(bootstrapml(obj, Nreps = 2,
                         map = function(x) x[1]/x[2],
                         reducer = mean)),
             0.4352018,
             tolerance = 1e-5)

# Different reducer and map.
set.seed(313)
expect_equal(object = unname(c(bootstrapml(obj, Nreps = 2, reducer = mean))),
             expected = c(4.333538, 10.581035),
             tolerance = 1e-5)

# Common reducer
set.seed(313)
expect_equal(object = unname(c(bootstrapml(obj, Nreps = 2))),
             expected = c(2.369753, 5.001551, 6.297323, 16.160518),
             tolerance = 1e-5)

# Keeping Names
set.seed(313)
expect_equal(object = colnames(bootstrapml(obj, Nreps = 2, reducer = mean)),
             expected = c("shape", "rate"))

# Common reducer, one-dim
set.seed(313)
expect_equal(object = unname(c(bootstrapml(obj2, Nreps = 2))),
             expected = c(1.967853, 3.108297),
             tolerance = 1e-5)