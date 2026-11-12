# Adjacent binary64 value toward -Inf, including powers of two.
previous_double <- function(x) {
  overflow <- is.infinite(x) & x>0
  x[overflow] <- .Machine$double.xmax
  exponent <- floor(log2(abs(x)))
  exponent <- exponent - as.integer(2^exponent > abs(x))
  step <- 2^(exponent - 52)
  step <- ifelse(x > 0 & x == 2^exponent, step / 2, step)
  result <- x - step
  result[overflow] <- .Machine$double.xmax
  result
}

# Independent truth checks of the generated R shell, never the selected
# reference. Per parameter point: five evenly spaced value rows;
# three evenly spaced quantile rows in EACH tail/scale group; 32 RNG draws.
# Tolerances, zero floors and ambiguous lattice endpoints follow the C++
# consumer in univariateML-cpp/tests/test_support.hpp.
test_that("generated d/p/q/r agree with independent truth tables", {
  skip_if_not_installed("jsonlite")
  directory <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  skip_if(!nzchar(directory) || !dir.exists(directory), "distribution dossiers absent")
  ns <- asNamespace("univariateML")
  pick <- function(n, count) unique(round(seq(1L, n, length.out = min(n, count))))
  close_log <- function(got, expected) {
    same <- got == expected
    same[is.na(same)] <- FALSE
    same | (is.finite(got) & is.finite(expected) &
              abs(got - expected) <= 1e-10 * pmax(1, abs(expected)))
  }
  metadata <- get("univariateML_metadata", ns)
  checked <- 0L
  for (folder in sort(list.dirs(directory, recursive = FALSE))) {
    if (!length(umvl_filter_families(basename(folder)))) next
    id <- basename(folder)
    filename <- file.path(folder, "truth.json")
    # Stats families have no generated functions. Checking binding ownership
    # avoids accidentally picking up a function imported from another package.
    if (!exists(paste0("d", id), ns, inherits = FALSE) || !file.exists(filename)) next
    checked <- checked + 1L
    truth <- jsonlite::fromJSON(filename, simplifyVector = FALSE)
    matrix_of <- function(rows) do.call(rbind, lapply(rows, function(row)
      vapply(row, as.numeric, numeric(1))))
    parameters <- matrix_of(truth$parameters)
    values <- matrix_of(truth$values)
    quantiles <- matrix_of(truth$quantiles)
    fun <- lapply(paste0(c("d", "p", "q", "r"), id), get, envir = ns, inherits = FALSE)
    points <- if (tier_at_least("full")) seq_len(nrow(parameters)) else
      pick(nrow(parameters), if (tier_at_least("dev")) 3L else 1L)
    # TASK-189 adds d/p/q stresses. Preserve every historical sampler case
    # and its primitive RNG stream without expanding sampling into this grid.
    historical <- if (is.null(truth$historical_sampler_points)) seq_len(nrow(parameters)) else
      as.integer(unlist(truth$historical_sampler_points)) + 1L
    sampled <- if (tier_at_least("full")) historical else
      historical[pick(length(historical), if (tier_at_least("dev")) 3L else 1L)]
    points <- sort(unique(c(points, sampled)))
    for (point in points) {
      theta <- setNames(as.list(parameters[point, ]), unlist(truth$parameter_names))
      v <- values[values[, 1] == point - 1L, , drop = FALSE]
      v <- v[pick(nrow(v), if (tier_at_least("dev")) 5L else 2L), , drop = FALSE]
      info <- paste(id, "point", point - 1L)
      has_fit <- !is.null(metadata[[paste0("ml", id)]])
      if (has_fit) fit <- univariateML_construct(unlist(theta), paste0("ml", id),
                                    list(logLik = 0, n = 1L, call = NULL))
      if (has_fit) expect_equal(suppressWarnings(dml(v[, 2], fit, log = TRUE)),
                   suppressWarnings(do.call(fun[[1]], c(list(v[, 2]), theta, list(log = TRUE)))),
                   info = paste(info, "density dispatch"))
      expect_true(all(close_log(suppressWarnings(do.call(fun[[1]], c(list(v[, 2]), theta, list(log = TRUE)))), v[, 3])), info = info)
      for (lower in c(TRUE, FALSE)) {
        expected <- v[, if (lower) 4L else 5L]
        got <- do.call(fun[[2]], c(list(v[, 2]), theta, list(lower.tail = lower, log.p = TRUE)))
        expect_true(all(close_log(got, expected)), info = paste(info, "log tail", lower))
        if (has_fit) expect_equal(pml(v[, 2], fit, lower.tail = lower, log.p = TRUE), got,
                     info = paste(info, "CDF dispatch", lower))
        natural <- exp(expected)
        keep <- natural > 1e-300
        got <- do.call(fun[[2]], c(list(v[keep, 2]), theta, list(lower.tail = lower)))
        expect_true(all(is.finite(got) & abs(got - natural[keep]) <= 1e-10 * natural[keep]),
                    info = paste(info, "natural tail", lower))
        for (log_input in c(TRUE, FALSE)) {
          q <- quantiles[quantiles[, 1] == point - 1L & quantiles[, 2] == lower &
                           quantiles[, 3] == log_input, , drop = FALSE]
          if (!nrow(q)) next
          q <- q[pick(nrow(q), if (tier_at_least("dev")) 3L else 1L), , drop = FALSE]
          got <- suppressWarnings(do.call(fun[[3]], c(list(q[, 4]), theta, list(lower.tail = lower, log.p = log_input))))
          if (has_fit) expect_equal(suppressWarnings(qml(q[, 4], fit, lower.tail = lower, log.p = log_input)), got,
                       info = paste(info, "quantile dispatch", lower, log_input))
          expected <- q[, 5]
          good <- got == expected | (is.finite(got) & is.finite(expected) &
            abs(got - expected) <= pmax(1e-9 * abs(expected), q[, 9])) |
            ifelse(q[, 8] != 0, got == q[, 6] | got == q[, 7],
                   got >= q[, 6] & got <= q[, 7])
          discrete <- has_fit && metadata[[paste0("ml",id)]]$support$type == "Z"
          interior <- if (log_input) is.finite(q[,4]) & q[,4] < 0 else q[,4] > 0 & q[,4] < 1
          large <- discrete & interior & abs(expected) > 2^53
          good[is.na(good)] <- FALSE
          backward <- !discrete & !good & interior & is.finite(got) & is.finite(expected)
          if (any(backward)) {
            at <- do.call(fun[[2]], c(list(got[backward]), theta,
                                     list(lower.tail=lower, log.p=log_input)))
            target <- q[backward, 4]
            scale <- if (log_input) pmax(1, abs(target)) else abs(target)
            good[backward] <- is.finite(at) & abs(at-target) <= 1e-12 * scale
          }
          if (any(large)) {
            at <- do.call(fun[[2]], c(list(got[large]), theta, list(lower.tail=lower, log.p=log_input)))
            before <- do.call(fun[[2]], c(list(previous_double(got[large])), theta, list(lower.tail=lower, log.p=log_input)))
            target <- q[large,4]
            good[large] <- good[large] & (is.finite(got[large]) | (expected[large]==Inf & got[large]==Inf)) & if (lower) at>=target & before<target else at<=target & before>target
          }
          expect_true(all(good), info = paste(info, "quantile", lower, log_input))
        }
      }
      if (!point %in% sampled) next
      # These inverse samplers consume exactly one uniform per draw.
      # Replay it to check every draw against the direct quantile.
      inverse_rng <- id %in% c("zeta", "yulesimon", "pgw")
      draw_once <- function() do.call(fun[[4]], c(list(32L), theta))
      set.seed(20261029 + point)
      draws <- draw_once()
      set.seed(20261029 + point)
      expect_identical(draw_once(), draws, info = info)
      expect_length(draws, 32L)
      if (inverse_rng) {
        set.seed(20261029 + point)
        uniforms <- stats::runif(length(draws))
        replay <- do.call(fun[[3]], c(list(uniforms), theta))
        expect_identical(draws, replay, info = paste(info, "inverse RNG replay"))
      }
      lo <- do.call(fun[[3]], c(list(0), theta))
      hi <- do.call(fun[[3]], c(list(1), theta))
      expect_true(all(!is.na(draws) & draws >= lo & draws <= hi),
                  info = paste(info, "sample support"))
      if (identical(metadata[[paste0("ml", id)]]$support$type, "Z"))
        expect_true(all(!is.na(draws) & draws == floor(draws)), info = info)
    }
  }
  skip_if(checked == 0L, "generated-family truth tables absent")
})

# Shell policy tests use generated functions directly, including an internal
# external-reference family, so they remain useful before TASK-78.9 switches.
test_that("generated wrappers recycle and preserve missing values and warnings", {
  d <- getFromNamespace("dlomax", "univariateML")
  p <- getFromNamespace("plomax", "univariateML")
  q <- getFromNamespace("qlomax", "univariateML")
  r <- getFromNamespace("rlomax", "univariateML")
  expect_identical(d(numeric(), 1, 2), numeric())
  expect_identical(d(c(NA_real_, NaN), 1, 2), c(NA_real_, NaN))
  expect_identical(p(c(NA_real_, NaN), 1, 2), c(NA_real_, NaN))
  expect_identical(q(c(NA_real_, NaN), 1, 2), c(NA_real_, NaN))
  expect_equal(d(1:3, c(1, 2), 2), c(d(1, 1, 2), d(2, 2, 2), d(3, 1, 2)))
  expect_warning(d(1, -1, 2), "NaNs produced")
  expect_warning(q(2, 1, 2), "NaNs produced")
  expect_warning(getFromNamespace("dbbinom", "univariateML")(1.5, 5, 2, 3), "non-integer x")
  expect_identical(r(0, 1, 2), numeric())
  expect_identical(r(4, numeric(), 2), numeric())
  expect_length(r(c(2, 9, 4), 1, 2), 3L)
  expect_length(r(2.9, 1, 2), 2L)
  expect_error(r(NA, 1, 2), "invalid arguments")
  expect_identical(r(2, NA_real_, 2), rep(NA_real_, 2))
  expect_identical(r(2, NaN, 2), rep(NaN, 2))
  expect_warning(r(2, -1, 2), "NaNs produced")
  set.seed(1)
  expect_length(r(2, 1:9, 2), 2L)
})

# The frozen EMG golden replays its own R sampler. Keep the R normal/
# exponential primitive stream while using the core sampler template.
test_that("EMG retains its established R primitive stream", {
  set.seed(78)
  expected <- vapply(seq_len(20L), function(i) {
    z <- stats::rnorm(1L)
    e <- stats::rexp(1L, rate = 2)
    0.3 + 1.7 * z + e
  }, numeric(1))
  expected_seed <- .Random.seed
  set.seed(78)
  got <- getFromNamespace("remg", "univariateML")(20L, 0.3, 1.7, 2)
  expect_equal(got, expected, tolerance = 1e-14)
  expect_identical(.Random.seed, expected_seed)
})

test_that("heavy-tail inverse samplers return finite quantiles on replayed uniforms", {
  for (id in umvl_filter_families(c("zeta", "yulesimon"))) {
    r <- getFromNamespace(paste0("r",id), "univariateML")
    q <- getFromNamespace(paste0("q",id), "univariateML")
    shape <- if (id=="zeta") 1.1 else .1
    set.seed(155)
    draws <- r(4096L,shape)
    set.seed(155)
    uniforms <- runif(4096L)
    expect_true(all(is.finite(draws)))
    expect_true(any(draws>2^53))
    expect_identical(draws,q(uniforms,shape))
  }
})
