x <- actuar::rparalogis(1000, 1, 1)

microbenchmark::microbenchmark(mlparalogis(x))