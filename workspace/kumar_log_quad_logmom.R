## kumar_lmom_improvements.R
## Improved L-moment estimators for Kumaraswamy(a,b)

# Original function for reference:
# kumar_lmom <- function(mu, sigma2) {
#   neuler <- digamma(1)    # -Euler
#   basel  <- pi^2/6        # trigamma(1)
#   f_over_df <- function(a) {
#     b       <- inverse_digamma(neuler - a * mu)
#     tri     <- trigamma(b)
#     tetra   <- psigamma(b, deriv=2)
#     f       <- a * tri * (basel - tri - a^2 * sigma2)
#     df      <- a * tetra * mu - 2 * tri * (basel + tri)
#     f/df
#   }
#   a0 <- sqrt(basel/sigma2)
#   for(i in 1:100) {
#     a1 <- a0 - f_over_df(a0)
#     if(abs(a1 - a0) < abs(a0)*.Machine$double.eps^0.5) break
#     a0 <- a1
#   }
#   a0
# }


# (a) Verify correctness:
# The root equation arises from matching sample log-moment variance:
# Var(log X) = (psi'(1) - psi'(b+1))/a^2 = sigma2.
# Let tri = psi'(b+1) so psi'(1) - tri = a^2 sigma2.
# Also psi(b+1) = psi(1) - a mu => b = inv_digamma(psi(1)-a mu) - 1.
# Differentiating yields f(a)=0 and df/da as coded above.


# (b,c) Two variants with transforms on 'a':


kumar_lmom_orig <- function(mu, sigma2) {
  neuler <- digamma(1)    # -Euler
  basel  <- pi^2/6        # trigamma(1)
  f_over_df <- function(a) {
    b       <- inverse_digamma(neuler - a * mu)
    tri     <- trigamma(b)
    tetra   <- psigamma(b, deriv=2)
    f       <- a * tri * (basel - tri - a^2 * sigma2)
    df      <- a * tetra * mu - 2 * tri * (basel + tri)
    f/df
  }
  a0 <- sqrt(basel/sigma2)
  for(i in 1:100) {
    a1 <- a0 - f_over_df(a0)
    if(abs(a1 - a0) < abs(a0)*.Machine$double.eps^0.5) break
    a0 <- a1
  }
  c(a0, i)
}

# 1. log-scale Newton on t = log(a)
kumar_lmom_log <- function(mu, sigma2,
                           reltol = sqrt(.Machine$double.eps),
                           iterlim = 100) {
  neuler <- digamma(1)
  basel  <- trigamma(1)

  # core: f(a) and df(a)
  score_f <- function(a) {
    b     <- inverse_digamma(neuler - a*mu)
    tri   <- trigamma(b+1)
    tet   <- psigamma(b+1, deriv=2)
    f     <- a * tri * (basel - tri - a^2*sigma2)
    df    <- a * tet * mu - 2*tri*(basel + tri)
    list(f=f, df=df)
  }

  # initialize in t
  a0 <- sqrt(basel/sigma2)
  t  <- log(a0)

  # Newton on t: f_t(t)=a f(a),  f'_t = a^2 df + a f
  for(i in seq_len(iterlim)) {
    a    <- exp(t)
    sc   <- score_f(a)
    U    <- a * sc$f
    H    <- a^2 * sc$df + U
    t1   <- t - U/H
    if(abs(t1 - t) < abs(t)*reltol) { t <- t1; break }
    t <- t1
  }
  c(exp(t), i)
}

# 2. quadratic-scale: θ = sqrt(a)
kumar_lmom_quad <- function(mu, sigma2,
                            reltol = sqrt(.Machine$double.eps),
                            iterlim = 100) {
  neuler <- digamma(1)
  basel  <- trigamma(1)

  score_f <- function(a) {
    b     <- inverse_digamma(neuler - a*mu)
    tri   <- trigamma(b+1)
    tet   <- psigamma(b+1, deriv=2)
    f     <- a * tri * (basel - tri - a^2*sigma2)
    df    <- a * tet * mu - 2*tri*(basel + tri)
    list(f=f, df=df)
  }

  # init in θ
  a0 <- sqrt(basel/sigma2)
  θ  <- sqrt(a0)

  # Newton on θ: f_θ = 2θ f, f'_θ = 4θ^2 df + 2f
  for(i in seq_len(iterlim)) {
    a    <- θ^2
    sc   <- score_f(a)
    U    <- 2*θ * sc$f
    H    <- 4*θ^2 * sc$df + 2*sc$f
    θ1   <- θ - U/H
    if(abs(θ1 - θ) < abs(θ)*reltol) { θ <- θ1; break }
    θ <- θ1
  }
  c(θ^2,i)
}
# End of file.  Compare iteration counts of each variant on your data.


a <- 1
b <- 20

x <- extraDistr::rkumar(100, a, b)
#mlkumar_init_fisher_then_profile(x)$iterations
mlkumar_(x)$iterations
mlkumar_quad(x)$iterations
mlkumar_log(x)$iterations

n <- length(x)
log_x <- log(x)
n_div <- 1 / n
mu <- sum(log_x) * n_div
sigma2 <- sum(log_x^2) * n_div - mu^2

kumar_lmom_orig(mu, sigma2)
kumar_lmom_log(mu, sigma2)
kumar_lmom_quad(mu, sigma2)



neuler <- digamma(1)
basel  <- trigamma(1)

score_f <- function(a) {
  b     <- inverse_digamma(neuler - a*mu)
  tri   <- trigamma(b+1)
  tet   <- psigamma(b+1, deriv=2)
  f     <- a * tri * (basel - tri - a^2*sigma2)
  df    <- a * tet * mu - 2*tri*(basel + tri)
  list(f=f, df=df)
}


aa <- seq(0.1, 1, by = 0.01)
plot(aa, sapply(aa, \(a) score_f(a)$f), type = "l")
abline(h = 0)
abline(v=sqrt(basel/sigma2))
plot(aa, sapply(aa, \(a) score_f(a)$f/score_f(a)$df), type = "l")
