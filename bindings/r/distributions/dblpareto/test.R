test_that("double Pareto d/p/q/r are available through the fitted-model API", {
  model <- mldblpareto(scale = 2, left_rate = 3, right_rate = 1)
  expect_equal(pml(2, model), 1 / 4)
  expect_equal(dml(2, model), 3 / 8)
  p <- c(1e-12, .01, .25, .4, .95, 1 - 1e-12)
  for (tail in c(TRUE, FALSE))
    expect_equal(pml(qml(log(p), model, lower.tail = tail, log.p = TRUE),
                     model, lower.tail = tail, log.p = TRUE), log(p), tolerance = 1e-12)
  set.seed(1)
  x <- rml(500, model)
  f <- mldblpareto(x)
  expect_equal(sum(dml(x, f, log = TRUE)), attr(f, "logLik"), tolerance = 1e-12)
})

test_that("double Pareto stops carry the appropriate one-sided models", {
  x <- c(1, 2, 2, 4)
  p <- tryCatch(mldblpareto(x, scale = 1), error = identity)
  expect_s3_class(p, "univariateML_limit")
  expect_match(conditionMessage(p), "mlpareto")
  expect_identical(p$model, "pareto")
  expect_equal(sum(dml(x, do.call(mlpareto, as.list(p$estimates)), log = TRUE)),
               p$limit_loglik)
  q <- tryCatch(mldblpareto(x, scale = 4, left_rate = 3), error = identity)
  expect_s3_class(q, "univariateML_limit")
  expect_match(conditionMessage(q), "mlpower")
  expect_identical(q$model, "power")
  expect_equal(q$estimates[["beta"]], 3)
  expect_equal(sum(dml(x, do.call(mlpower, as.list(q$estimates)), log = TRUE)),
               q$limit_loglik)
  expect_error(mldblpareto(2), "unbounded")
  expect_error(mldblpareto(rep(2, 3), scale = 2), "unbounded")
  expect_error(mldblpareto(c(1, 0)), class = "univariateML_invalid")
  expect_error(mldblpareto(x, scale = Inf), "positive")
})

test_that("double Pareto returns all proved maximizing kinks", {
  flat <- mldblpareto(c(1, 2, 2, 4), left_rate = 3, right_rate = 1)
  expect_false(attr(flat, "unique"))
  expect_equal(flat[["scale"]], 1)
  reflected <- mldblpareto(c(1 / 1024, .5, .5, 2, 2, 1024))
  expect_false(attr(reflected, "unique"))
  expect_true(reflected[["scale"]] %in% c(.5, 2))
})

test_that("an unseparated double Pareto comparison preserves its finite witness", {
  f <- mldblpareto(c(.5, 1, 1, 1, 1, 16))
  expect_equal(f[["scale"]], 1)
  expect_identical(attr(f, "certified"), FALSE)
  expect_identical(attr(f, "unique"), NA)
})
