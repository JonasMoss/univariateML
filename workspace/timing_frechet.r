shapes <- c(1,5,0.5,5,5)
theta <- c(1,5,5,10^-3, 10^-15)
rates <- (1/theta)^(1/shapes)

for (i in 1:5) {
  x <- actuar::rinvweibull(1000, shapes[i], rates[i])
  mlinvweibull(x)
}


frechetAEE::ce_frechet_cpp(x)



x <- actuar::rinvweibull(1000, 5, 2)
mlinvweibull(x)
distributionsrd::frechet.mle(x)
frechetAEE::ce_frechet_cpp(x)

x <- actuar::rinvweibull(1000, 5, 5)
microbenchmark::microbenchmark(distributionsrd::frechet.mle(x),
mlinvweibull(x),
frechetAEE::ce_frechet_cpp(x))

x <- actuar::rinvweibull(1000, 5, 20)
microbenchmark::microbenchmark(mlinvweibull(x),
                               frechetr::mlfrechet_estimate(x),
                               frechetAEE::ce_frechet_cpp(x))


x <- actuar::rinvweibull(100, 1, 1000)
microbenchmark::microbenchmark(univariateML::mlinvweibull(x),
                               frechetAEE::ce_frechet_cpp(x),
                               frechetr::mle(x),
                               frechetr::mlme(x),
                               frechetr::lhk(x),
                               times = 1000)


x <- actuar::rinvweibull(40, 5, 5)
microbenchmark::microbenchmark(distributionsrd::frechet.mle(x),
                               mlinvweibull(x),
                               frechetAEE::ce_frechet_cpp(x))


shape <- 3.3
log_x <- log(x)
microbenchmark::microbenchmark(mean(x^-shape), mean(exp(-log_x * shape)),
                               sum(exp(-log_x * shape))/length(x))




