# Free-scale Burr XII boundaries

Write k = shape1, c = shape2, b = log(scale), and Y = log(X).
For T ~ Exp(1), an exact construction is

    Y = b + log(expm1(T/k))/c.

This identifies the nondegenerate boundary distributions without treating
log-logistic (k = 1) as a boundary.

- As k tends to infinity, log(expm1(T/k)) = log(T) - log(k) + o(1).
  Holding b - log(k)/c fixed gives log Weibull, hence Weibull on X.
  Its scale is scale_Burr * k^(-1/c).
- As k tends to zero, log(expm1(T/k)) = T/k + o(1).
  Holding kc = a > 0 and b finite gives Y = b + T/a: Pareto on X
  with minimum exp(b), shape a. This requires c tending to infinity.
- For k bounded away from zero and infinity, c tending to infinity
  collapses Y to a point. A simultaneous location shift cannot preserve
  a positive interquantile distance. The same applies to k tending to
  infinity with c tending to infinity. These are point masses, or escape
  of probability, rather than further continuous limit models.
- No coupled path with c tending to zero supplies a missed model. The
  density of Y is kc sigmoid(z) exp(-k softplus(z)), z=c(Y-b).
  Maximising over k gives c sigmoid(z)/(e softplus(z)) <= c/e.
  Thus log likelihood <= n(log(c)-1) - sum(log(x)), which tends to minus
  infinity uniformly over k and scale for every fixed positive sample.
- With k tending to zero and bounded c, the maximum density of Y is
  c (k/(k+1))^(k+1), which tends to zero. In the remaining k -> 0,
  c -> infinity corner, kc tending to zero gives dispersion to infinity;
  kc tending to infinity gives a point mass. Only finite positive kc
  gives the Pareto limit above.

For a nonconstant positive sample, a point mass cannot improve the joint
likelihood: shrinking its interquantile range leaves at least one distinct
observation in a vanishing tail. Constant samples have unbounded likelihood
and are rejected as invalid input. These arguments leave Weibull and Pareto
as the two relevant continuous faces. Multistart optimisation is a numerical
search for the interior maximum, not a global stationary-point certificate.

Under reciprocation, b changes sign and log likelihood gains
-2 sum(log(x)). Weibull becomes actuar inverse Weibull: its `rate` equals
the reciprocal-data Weibull scale, and its inverse Weibull scale is 1/rate.
Pareto minimum becomes the power upper endpoint; the power shape equals
the Pareto shape. `power_mle(x, 0)` supplies the exact endpoint MLE (the
R-facing `mlpower` defaults to a small positive endpoint offset).

The independent oracle in test.R optimises the full actuar density over
three log parameters, with Nelder-Mead followed by BFGS from ordinary and
extreme shape starts, and compares the package limit fits. For rburr with
(k,c,scale)=(1,2,3), seeds 1 through 6 give 3/6 non-interior fits at n=20
(two Weibull, one Pareto); all six n=200 fits are interior. The reciprocal
samples have exactly the corresponding inverse Weibull and power faces.
`oracle-results.csv` records the initial independent run; the small-sample
likelihoods are frozen in `oracle-fixtures.inc` for C++ regression tests.
The expanded extreme-start oracle is rerun by both R test files.

Reference: [Shao (2004), CSDA 45, 675-687](https://doi.org/10.1016/S0167-9473(02)00367-5).
The accessible publisher abstract and algorithm identify the same two
faces and warn of multiple maxima; the quantile and density derivations
above are independent calculations. One web search was used.
