test_that("special functions match Rmath", {
  skip_on_cran()

  set.seed(123)
  xs <- c(-5, -1, -0.5, 0.1, 0.5, 1, 2, 10, 50, 1e3)
  pos_xs <- xs[xs > 0]

  expect_equal(special_digamma_cpp(pos_xs), digamma(pos_xs), tolerance = 1e-12)
  expect_equal(special_trigamma_cpp(pos_xs), trigamma(pos_xs),
               tolerance = 1e-12, scale = 1)
  expect_equal(special_tetragamma_cpp(pos_xs), psigamma(pos_xs, deriv = 2),
               tolerance = 1e-10, scale = 1)
  expect_equal(special_psigamma_cpp(pos_xs, 0), digamma(pos_xs),
               tolerance = 1e-12)
  expect_equal(special_psigamma_cpp(pos_xs, 1), trigamma(pos_xs),
               tolerance = 1e-12)

  expect_equal(special_lgamma_cpp(pos_xs), lgamma(pos_xs), tolerance = 1e-12)

  a <- c(0.1, 0.5, 1, 2, 5)
  b <- c(0.2, 0.7, 1.5, 3, 10)
  expect_equal(special_lbeta_cpp(a, b), lbeta(a, b), tolerance = 1e-12)

  xnorm <- seq(-5, 5, length.out = 21)
  expect_equal(special_dnorm_cpp(xnorm, mean = 0.5, sd = 2, log = FALSE),
               dnorm(xnorm, mean = 0.5, sd = 2, log = FALSE),
               tolerance = 1e-12)
  expect_equal(special_dnorm_cpp(xnorm, mean = -0.5, sd = 1.5, log = TRUE),
               dnorm(xnorm, mean = -0.5, sd = 1.5, log = TRUE),
               tolerance = 1e-12)

  xpois <- 0:10
  expect_equal(special_dpois_cpp(xpois, lambda = 3, log = FALSE),
               dpois(xpois, lambda = 3, log = FALSE),
               tolerance = 1e-12)
  expect_equal(special_dpois_cpp(xpois, lambda = 7, log = TRUE),
               dpois(xpois, lambda = 7, log = TRUE),
               tolerance = 1e-12)

  xgeom <- 0:8
  expect_equal(special_dgeom_cpp(xgeom, prob = 0.2, log = FALSE),
               dgeom(xgeom, prob = 0.2, log = FALSE),
               tolerance = 1e-12)
  expect_equal(special_dgeom_cpp(xgeom, prob = 0.7, log = TRUE),
               dgeom(xgeom, prob = 0.7, log = TRUE),
               tolerance = 1e-12)
})
