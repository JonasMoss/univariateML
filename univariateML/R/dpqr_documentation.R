# Documentation retained from the former per-family dpqr.R sources.
#' Conway--Maxwell--binomial distribution
#'
#' Density, distribution, quantile and random generation functions.
#' Support is 0 to size. size is a nonnegative integer at most 1e7;
#' prob lies between zero and one (inclusive), and nu is any finite real number. The member nu = 1
#' is binomial. Normalization enumerates the finite support in log scale.
#' Tables are shared by repeated parameter tuples within each call.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param size Known number of trials.
#' @param prob Canonical probability coordinate; generally not the mean divided by size.
#' @param nu Real dispersion coordinate.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE, use P(X <= q); otherwise use P(X > q).
#' @details Numeric arguments are recycled to their maximum length (zero length
#' returns numeric(0)); logical flags use their first element. Missing values
#' propagate. Invalid parameters or probabilities produce NaN with a warning;
#' noninteger density observations produce zero with a warning. CDF arguments
#' are floored. Quantiles are the smallest support point k with F(k) >= p
#' (or P(X > k) <= p for upper tails). Tails are summed directly, including
#' on the logarithmic scale. Random draws use inversion and R's RNG.
#' @return A numeric vector of probabilities, quantiles or random draws.
#' @name cmb-distribution
NULL

#' Conway--Maxwell--Poisson distribution
#'
#' Density, distribution, quantile and random generation using the CMP normalizer.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param lambda Nonnegative rate coordinate; must be below one when nu is zero.
#' @param nu Nonnegative dispersion coordinate.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE, use P(X <= q); otherwise use P(X > q).
#' @details Numeric arguments are recycled to their maximum length (zero length
#' returns numeric(0)); logical flags use their first element. Missing values
#' propagate. Invalid parameters or probabilities produce NaN with a warning;
#' noninteger density observations produce zero with a warning. CDF arguments
#' are floored. Quantiles are the smallest support point k with F(k) >= p
#' (or P(X > k) <= p for upper tails). Tails are summed directly, including
#' on the logarithmic scale. Random draws use inversion and R's RNG.
#' @return A numeric vector of probabilities, quantiles or random draws.
#' @name cmp-distribution
NULL

#' Exponentially modified Gaussian distribution
#'
#' Density, CDF, quantiles and sampling for a normal plus independent exponential.
#' @param x,q Observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; if length exceeds one its length is used.
#' @param mu Normal component mean.
#' @param sigma Positive normal component standard deviation.
#' @param lambda Positive exponential rate.
#' @param log,log.p Return or supply log probabilities.
#' @param lower.tail Use the lower tail if TRUE, upper tail otherwise.
#' @details Numeric arguments recycle; missing values propagate. Invalid
#' parameters or probabilities give NaN with a warning. Both tails are evaluated
#' in log scale. Quantiles use safeguarded Newton and bisection. Random draws
#' add independent normal and exponential draws using R's RNG.
#' @return A numeric vector.
#' @name emg-distribution
NULL

#' Folded normal distribution
#'
#' Density, distribution, quantile and random generation for the absolute
#' value of a normal variable with nonnegative location mu and positive scale sigma.
#' The sign of the parent location is unidentifiable. mu = 0 is half-normal.
#' @param x,q Numeric observations or quantiles.
#' @param p Numeric probabilities.
#' @param n Number of observations; if length exceeds one, its length is used.
#' @param mu Nonnegative finite parent normal location.
#' @param sigma Positive finite parent normal scale.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE use P(X <= q), otherwise P(X > q).
#' @details Numeric arguments recycle to their maximum length; zero lengths
#' return numeric(0). Missing values propagate. Invalid parameters and
#' probabilities produce NaN with a warning. Logical flags use their first
#' element. Both tails are evaluated directly, including logarithmic tails.
#' Quantiles use safeguarded bisection. Random draws are abs(rnorm(n, mu, sigma))
#' and use R's RNG, controlled by set.seed().
#' @return A numeric vector of densities, probabilities, quantiles or draws.
#' @name foldnorm-distribution
NULL

#' GED distribution
#'
#' Density, distribution, quantile and random generation in the fGarch
#' mean and standard deviation parameterisation.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector uses its length.
#' @param mean Finite mean.
#' @param sd Finite positive standard deviation.
#' @param nu Finite shape, positive (including shapes below one).
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric inputs recycle, zero-length inputs return numeric(0), and
#'   missing values propagate. Invalid parameters yield NaN with a warning.
#'   Logical flags use their first element. Densities and small tails are
#'   evaluated directly on the log scale. Random generation uses R's RNG.
#' @return A numeric vector.
#' @name ged-distribution
NULL

#' logitnorm distribution
#'
#' Density, distribution, quantile and random generation functions.
#' @param x,q Numeric observations.
#' @param p Numeric probabilities.
#' @param n Number of draws; a vector supplies its length.
#' @param mu,sigma Normal location and positive scale on the logit scale.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric arguments recycle, zero length returns numeric(0), and
#' missing values propagate. Invalid parameters give NaN with a warning.
#' Parameters must be finite. Random generation transforms draws from R's RNG.
#' @param ... Additional arguments, retained for reference API compatibility.
#' @return A numeric vector.
#' @name logitnorm-distribution
NULL

#' naka distribution
#'
#' Density, distribution, quantile and random generation functions.
#' @param x,q Numeric observations.
#' @param p Numeric probabilities.
#' @param n Number of draws; a vector supplies its length.
#' @param shape,scale Positive shape and scale; scale is the mean squared amplitude.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric arguments recycle, zero length returns numeric(0), and
#' missing values propagate. Invalid parameters give NaN with a warning.
#' Parameters must be finite. Random generation transforms draws from R's RNG.
#' @return A numeric vector.
#' @name naka-distribution
NULL

#' SGED distribution
#'
#' Density, distribution, quantile and random generation in the fGarch
#' mean and standard deviation parameterisation.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector uses its length.
#' @param mean Finite mean.
#' @param sd Finite positive standard deviation.
#' @param nu Finite shape, positive (including shapes below one).
#' @param xi Finite positive Fernandez--Steel skewness.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric inputs recycle, zero-length inputs return numeric(0), and
#'   missing values propagate. Invalid parameters yield NaN with a warning.
#'   Logical flags use their first element. Densities and small tails are
#'   evaluated directly on the log scale. Random generation uses R's RNG.
#' @return A numeric vector.
#' @name sged-distribution
NULL

#' Shifted Gompertz distribution
#'
#' Density, distribution, quantile and random generation functions for the
#' shifted Gompertz family, including the exponential member eta = 0.
#' There is no location parameter; b is a rate.
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations; if length exceeds one, its length is used.
#' @param b Positive finite rate.
#' @param eta Nonnegative finite shape.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE use P(X <= q), otherwise P(X > q).
#' @details Numeric arguments recycle to their maximum length; zero length
#' returns numeric(0). Missing values propagate. Invalid parameters and
#' probabilities produce NaN with a warning. Logical flags use their first
#' element. Both tails are evaluated directly, including on the log scale.
#' Quantiles use safeguarded bisection. Random generation takes the maximum
#' of an exponential and a Gumbel variate using R's RNG; eta = 0 uses only
#' the exponential variate.
#' @return Numeric vector of densities, probabilities, quantiles or draws.
#' @name sgomp-distribution
NULL

#' Skellam distribution
#'
#' Difference of independent Poisson counts with nonnegative finite rates.
#' Zero rates give the corresponding signed Poisson member.
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of draws; a vector requests its length.
#' @param mu1,mu2 Nonnegative finite Poisson rates.
#' @param log,log.p Return or supply log probabilities.
#' @param lower.tail Use P(X <= q) if TRUE, P(X > q) otherwise.
#' @details Arguments recycle; missing values propagate. Invalid arguments
#' give NaN with a warning. Noninteger density arguments give zero with a
#' warning; CDF arguments are floored. Quantiles are the smallest integer
#' reaching the requested probability. Infinite quantiles denote unbounded
#' support endpoints. Log tails are evaluated directly. Random generation
#' subtracts independent Poisson draws using R's RNG.
#' @return A numeric vector.
#' @name skellam-distribution
NULL

#' Azzalini skew-normal distribution
#'
#' Density, distribution, quantile and random generation in direct parameters.
#' This differs from the Fernandez--Steel two-piece normal (mlsnorm).
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector supplies its length.
#' @param xi Finite location.
#' @param omega Positive finite scale.
#' @param alpha Finite real shape.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE use P(X <= q), otherwise P(X > q).
#' @details Numeric arguments recycle; zero lengths give numeric(0). Missing
#' values propagate. Invalid parameters and probabilities give NaN with a warning.
#' Tail probabilities use Owen's T or direct scaled density quadrature to avoid
#' cancellation. Quantiles use bracketed bisection. Random draws use two normal
#' variates from R's RNG, so set.seed controls reproducibility.
#' @return A numeric vector.
#' @name sn-distribution
NULL

#' SNORM distribution
#'
#' Density, distribution, quantile and random generation in the fGarch
#' mean and standard deviation parameterisation.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector uses its length.
#' @param mean Finite mean.
#' @param sd Finite positive standard deviation.
#' @param xi Finite positive Fernandez--Steel skewness.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric inputs recycle, zero-length inputs return numeric(0), and
#'   missing values propagate. Invalid parameters yield NaN with a warning.
#'   Logical flags use their first element. Densities and small tails are
#'   evaluated directly on the log scale. Random generation uses R's RNG.
#' @return A numeric vector.
#' @name snorm-distribution
NULL

#' SSTD distribution
#'
#' Density, distribution, quantile and random generation in the fGarch
#' mean and standard deviation parameterisation.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector uses its length.
#' @param mean Finite mean.
#' @param sd Finite positive standard deviation.
#' @param nu Finite shape, greater than two.
#' @param xi Finite positive Fernandez--Steel skewness.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric inputs recycle, zero-length inputs return numeric(0), and
#'   missing values propagate. Invalid parameters yield NaN with a warning.
#'   Logical flags use their first element. Densities and small tails are
#'   evaluated directly on the log scale. Random generation uses R's RNG.
#' @return A numeric vector.
#' @name sstd-distribution
NULL

#' STD distribution
#'
#' Density, distribution, quantile and random generation in the fGarch
#' mean and standard deviation parameterisation.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector uses its length.
#' @param mean Finite mean.
#' @param sd Finite positive standard deviation.
#' @param nu Finite shape, greater than two.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric inputs recycle, zero-length inputs return numeric(0), and
#'   missing values propagate. Invalid parameters yield NaN with a warning.
#'   Logical flags use their first element. Densities and small tails are
#'   evaluated directly on the log scale. Random generation uses R's RNG.
#' @return A numeric vector.
#' @name std-distribution
NULL

#' Tukey lambda distribution
#'
#' Density, distribution, quantiles and random generation for
#' Q(u) = mu + sigma * (u^lambda - (1-u)^lambda) / lambda,
#' with the analytic logistic limit at lambda = 0.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector supplies its length.
#' @param mu Finite location.
#' @param sigma Positive finite scale.
#' @param lambda Finite shape (evaluation allows all real shapes).
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE use P(X <= q), otherwise P(X > q).
#' @details Numeric vectors recycle, missing values propagate, and invalid
#' parameters give NaN with a warning. Inversion solves for the smaller tail
#' in log probability using safeguarded Newton iterations. Random generation
#' uses R's uniform RNG, so set.seed controls reproducibility.
#' @return A numeric vector.
#' @name tlambda-distribution
NULL

#' Yule--Simon distribution
#'
#' Density, distribution, quantile and random generation functions.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param rho Finite shape in (0, inf).
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE, use P(X <= q); otherwise use P(X > q).
#' @details Numeric arguments are recycled to their maximum length (zero length
#' returns numeric(0)); logical flags use their first element. Missing values
#' propagate. Invalid parameters or probabilities produce NaN with a warning;
#' noninteger density observations produce zero with a warning. CDF arguments
#' are floored. Quantiles are the smallest support point k with F(k) >= p
#' (or P(X > k) <= p for upper tails). Tails are evaluated directly, including
#' on the logarithmic scale. Random draws use inversion and R's RNG.
#' @return A numeric vector of probabilities, quantiles or random draws.
#' @details Support starts at one and has no finite upper endpoint. Quantiles
#' beyond 2^53 use the representable double grid; a result beyond the largest
#' finite double is Inf. At that scale, adjacent integers cannot be distinguished.
#' @name yulesimon-distribution
NULL

#' Zeta distribution
#'
#' Density, distribution, quantile and random generation functions.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param s Finite shape in (1, inf).
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE, use P(X <= q); otherwise use P(X > q).
#' @details Numeric arguments are recycled to their maximum length (zero length
#' returns numeric(0)); logical flags use their first element. Missing values
#' propagate. Invalid parameters or probabilities produce NaN with a warning;
#' noninteger density observations produce zero with a warning. CDF arguments
#' are floored. Quantiles are the smallest support point k with F(k) >= p
#' (or P(X > k) <= p for upper tails). Tails are evaluated directly, including
#' on the logarithmic scale. Random draws use inversion and R's RNG.
#' @return A numeric vector of probabilities, quantiles or random draws.
#' @details Support starts at one and has no finite upper endpoint. Quantiles
#' beyond 2^53 use the representable double grid; a result beyond the largest
#' finite double is Inf. At that scale, adjacent integers cannot be distinguished.
#' @name zeta-distribution
NULL

#' Finite Zipf distribution
#'
#' Density, distribution, quantile and random generation functions.
#' Support is 1 to N; N is a positive integer at most .Machine$integer.max,
#' and s is finite and nonnegative. The member s = 0 is uniform.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param N Upper support endpoint.
#' @param s Nonnegative shape.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE, use P(X <= q); otherwise use P(X > q).
#' @details Numeric arguments are recycled to their maximum length (zero length
#' returns numeric(0)); logical flags use their first element. Missing values
#' propagate. Invalid parameters or probabilities produce NaN with a warning;
#' noninteger density observations produce zero with a warning. CDF arguments
#' are floored. Quantiles are the smallest support point k with F(k) >= p
#' (or P(X > k) <= p for upper tails). Tails are summed directly, including
#' on the logarithmic scale. Random draws use inversion and R's RNG.
#' @return A numeric vector of probabilities, quantiles or random draws.
#' @name zipf-distribution
NULL

