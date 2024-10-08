context("functions")

# Verify that distributions with d*, r*, p*, and q* versions work correctly
seed <- 313

set.seed(seed)

# For all distributions to be tested here, follow the pattern of list entries:
# Where "dist" is the name of the distribution type function stem, enter:
# dist = rdist(10, ...) where ... are arbitrary values for the rdist parameters.
# This list must be constructed manually (rather than by automatically reading
# the list of ml*.R files) because each dist function has unique parameters.
dists <- list(
  norm = rnorm(10, 2, 3),
  pois = rpois(10, 3)
)

for (i in 1:length(dists)) {
  dist <- names(dists)[i]
  x <- dists[[i]]

  obj <- do.call(paste0("ml", dist), list(x))

  # Validate ddist
  expect_equal(
    dml(1, obj),
    do.call(
      paste0("d", dist),  # ddist()
      c(1, as.list(obj))  # 1 as input plus all dist parameters
    )
  )

  # Validate pdist
  expect_equal(
    pml(1, obj, log.p = TRUE),
    do.call(
      paste0("p", dist),  # pdist()
      # 1 as input plus all dist parameters
      c(
        1, log.p = TRUE,
        as.list(obj)
      )
    )
  )

  # Validate qdist
  expect_equal(
    qml(0.9, obj, lower.tail = TRUE),
    do.call(
      paste0("q", dist),  # qdist()
      # 0.9 as input plus all dist parameters
      c(
        0.9, lower.tail = TRUE,
        as.list(obj)
      )
    )
  )

  # Validate rdist.
  # Seeds must be reset repeatedly to ensure perfect random replication.
  expect_equal(
    (function() {
      set.seed(seed)
      rml(1, obj)
    })(),
    (function() {
      set.seed(seed)
      do.call(
        paste0("r", dist),  # rdist()
        c(1, as.list(obj))  # 1 as input plus all dist parameters
      )
    })()
  )
}
