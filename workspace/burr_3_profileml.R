# install.packages(c("univariateML","actuar"))
library(univariateML) # for mlburr()
library(actuar) # for dburr()

# — Full analytical 3×3 Hessian of ℓ(σ,c,k)
full_hess_3 <- function(x, c, k, sigma) {
  z <- x / sigma
  lnz <- log(z)
  zc <- z^c
  oneA <- 1 + zc

  A <- sum(zc / oneA)
  B <- sum(zc * lnz / oneA)
  C <- sum(zc * lnz^2 / oneA^2)
  D <- sum(zc / oneA^2)
  E <- sum(zc * (c * lnz + 1 + zc) / oneA^2)

  Hcc <- -length(x) / c^2 - (k + 1) * C
  Hkk <- -length(x) / k^2
  Haa <- c / sigma^2 * (length(x) - (k + 1) * A - c * (k + 1) * D)
  Hck <- -B
  Hkc <- Hck
  Hka <- c / sigma * A
  Hak <- Hka
  Hca <- -length(x) / sigma + (k + 1) / sigma * E
  Hac <- Hca

  matrix(c(
    Hcc, Hck, Hca,
    Hkc, Hkk, Hka,
    Hac, Hak, Haa
  ), 3, 3, byrow = TRUE)
}

# — Envelope gradient p'(σ)
prof_grad <- function(x, sigma) {
  shp <- mlburr(x / sigma)
  k <- shp["shape1"]
  c <- shp["shape2"]
  A <- (x / sigma)^c
  (c / sigma) * (-length(x) + (k + 1) * sum(A / (1 + A)))
}

# — Profile Hessian p''(σ) via block‐identity
prof_hess_analytic <- function(x, sigma) {
  shp <- mlburr(x / sigma)
  k <- shp["shape1"]
  c <- shp["shape2"]
  H3 <- full_hess_3(x, c, k, sigma)

  Hσσ <- H3[3, 3] # now that order is (c,k,σ)
  Hσφ <- H3[3, 1:2]
  Hφσ <- H3[1:2, 3]
  Hφφ <- H3[1:2, 1:2]

  Hσσ - as.numeric(Hσφ %*% solve(Hφφ, Hφσ))
}

f_over_df <- \(x, sigma) {
  g <- prof_grad(x, sigma)
  h <- prof_hess_analytic(x, sigma)
  g / h
}


# — One‑dimensional Newton–Raphson on σ
profile_newton_burr3 <- function(x,
                                 sigma_init = median(x),
                                 tol = 1e-8,
                                 maxit = 50,
                                 trace = FALSE) {
  sigma <- sigma_init
  shp <- mlburr(x / sigma)

  for (i in seq_len(maxit)) {
    delta <- f_over_df(x, sigma)
    sigma_new <- sigma - delta
    if (abs(delta) < tol) {
      sigma <- sigma_new
      break
    }
    sigma <- sigma_new
  }
  if (i == maxit) warning("Did not converge in ", maxit, " iterations")

  shp <- mlburr(x / sigma)
  list(
    est = stats::setNames(c(shp, sigma), c("shape1", "shape2", "scale")),
    logLik = sum(dburr(x,
      shape1 = shp["shape1"],
      shape2 = shp["shape2"],
      scale  = sigma,
      log    = TRUE
    )),
    iter = i
  )
}

# — Example
set.seed(1)
x <- rburr(10000, shape1 = 1, shape2 = 2, scale = 3)
res_nlm <- burr3_nlm(x) # your nlm‐based fit
res_profile <- profile_newton_burr3(x, trace = TRUE)

print(res_nlm)
print(res_profile)


#microbenchmark::microbenchmark(burr3_nlm(x), profile_newton_burr3(x))
