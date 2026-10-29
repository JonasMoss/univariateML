# Compatibility formals for the existing exported d/p/q/r API.
# Function bodies are generated from README parameter names.
dpqr_api <- list(
  cmb = list(d = "x, size, prob, nu, log = FALSE", p = "q, size, prob, nu, lower.tail = TRUE, log.p = FALSE", q = "p, size, prob, nu, lower.tail = TRUE, log.p = FALSE", r = "n, size, prob, nu"),
  cmp = list(d = "x, lambda, nu, log = FALSE", p = "q, lambda, nu, lower.tail = TRUE, log.p = FALSE", q = "p, lambda, nu, lower.tail = TRUE, log.p = FALSE", r = "n, lambda, nu"),
  emg = list(d = "x, mu, sigma, lambda, log = FALSE", p = "q, mu, sigma, lambda, lower.tail = TRUE, log.p = FALSE", q = "p, mu, sigma, lambda, lower.tail = TRUE, log.p = FALSE", r = "n, mu, sigma, lambda"),
  foldnorm = list(d = "x, mu = 0, sigma = 1, log = FALSE", p = "q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE", q = "p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE", r = "n, mu = 0, sigma = 1"),
  ged = list(d = "x, mean = 0, sd = 1, nu = 2, log = FALSE", p = "q, mean = 0, sd = 1, nu = 2, lower.tail = TRUE, log.p = FALSE", q = "p, mean = 0, sd = 1, nu = 2, lower.tail = TRUE, log.p = FALSE", r = "n, mean = 0, sd = 1, nu = 2"),
  logitnorm = list(d = "x, mu = 0, sigma = 1, log = FALSE, ...", p = "q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE, ...", q = "p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE, ...", r = "n, mu = 0, sigma = 1, ..."),
  naka = list(d = "x, shape, scale, log = FALSE", p = "q, shape, scale, lower.tail = TRUE, log.p = FALSE", q = "p, shape, scale, lower.tail = TRUE, log.p = FALSE", r = "n, shape, scale"),
  sged = list(d = "x, mean = 0, sd = 1, nu = 2, xi = 1.5, log = FALSE", p = "q, mean = 0, sd = 1, nu = 2, xi = 1.5, lower.tail = TRUE, log.p = FALSE", q = "p, mean = 0, sd = 1, nu = 2, xi = 1.5, lower.tail = TRUE, log.p = FALSE", r = "n, mean = 0, sd = 1, nu = 2, xi = 1.5"),
  sgomp = list(d = "x, b, eta, log = FALSE", p = "q, b, eta, lower.tail = TRUE, log.p = FALSE", q = "p, b, eta, lower.tail = TRUE, log.p = FALSE", r = "n, b, eta"),
  skellam = list(d = "x, mu1, mu2, log = FALSE", p = "q, mu1, mu2, lower.tail = TRUE, log.p = FALSE", q = "p, mu1, mu2, lower.tail = TRUE, log.p = FALSE", r = "n, mu1, mu2"),
  sn = list(d = "x, xi = 0, omega = 1, alpha = 0, log = FALSE", p = "q, xi = 0, omega = 1, alpha = 0, lower.tail = TRUE, log.p = FALSE", q = "p, xi = 0, omega = 1, alpha = 0, lower.tail = TRUE, log.p = FALSE", r = "n, xi = 0, omega = 1, alpha = 0"),
  snorm = list(d = "x, mean = 0, sd = 1, xi = 1.5, log = FALSE", p = "q, mean = 0, sd = 1, xi = 1.5, lower.tail = TRUE, log.p = FALSE", q = "p, mean = 0, sd = 1, xi = 1.5, lower.tail = TRUE, log.p = FALSE", r = "n, mean = 0, sd = 1, xi = 1.5"),
  sstd = list(d = "x, mean = 0, sd = 1, nu = 5, xi = 1.5, log = FALSE", p = "q, mean = 0, sd = 1, nu = 5, xi = 1.5, lower.tail = TRUE, log.p = FALSE", q = "p, mean = 0, sd = 1, nu = 5, xi = 1.5, lower.tail = TRUE, log.p = FALSE", r = "n, mean = 0, sd = 1, nu = 5, xi = 1.5"),
  std = list(d = "x, mean = 0, sd = 1, nu = 5, log = FALSE", p = "q, mean = 0, sd = 1, nu = 5, lower.tail = TRUE, log.p = FALSE", q = "p, mean = 0, sd = 1, nu = 5, lower.tail = TRUE, log.p = FALSE", r = "n, mean = 0, sd = 1, nu = 5"),
  tlambda = list(d = "x, mu = 0, sigma = 1, lambda = 0, log = FALSE", p = "q, mu = 0, sigma = 1, lambda = 0, lower.tail = TRUE, log.p = FALSE", q = "p, mu = 0, sigma = 1, lambda = 0, lower.tail = TRUE, log.p = FALSE", r = "n, mu = 0, sigma = 1, lambda = 0"),
  yulesimon = list(d = "x, rho, log = FALSE", p = "q, rho, lower.tail = TRUE, log.p = FALSE", q = "p, rho, lower.tail = TRUE, log.p = FALSE", r = "n, rho"),
  zeta = list(d = "x, s, log = FALSE", p = "q, s, lower.tail = TRUE, log.p = FALSE", q = "p, s, lower.tail = TRUE, log.p = FALSE", r = "n, s"),
  zipf = list(d = "x, N, s, log = FALSE", p = "q, N, s, lower.tail = TRUE, log.p = FALSE", q = "p, N, s, lower.tail = TRUE, log.p = FALSE", r = "n, N, s")
)
