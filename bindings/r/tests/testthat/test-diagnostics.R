test_that('TASK-132 invalid observations stop before conversion to core spans', {
  skip_unless_tier('dev')
  for (id in names(univariateML_metadata)) {
    meta <- univariateML_metadata[[id]]
    args <- golden_fit_args(id)
    invalid <- list(numeric(), c(NaN, 1), c(Inf, 1), c(-Inf, 1))
    if (meta$support$type == 'integer') invalid <- c(invalid, list(c(.5, 1.5)))
    for (x in invalid) {
      expect_error(do.call(get(id), c(list(x=x), args)), info=paste(id,paste(x,collapse=',')))
    }
  }
})
