# Quantile-defined families: g-and-k, g-and-h, generalized lambda

**Status:** banked idea, 2026-11-08 (Jonas): possible families to add later,
as one project; not the highest priority. If g-and-k is the most visible
member, start there. No implementation authorized by this entry.

All three families are defined by an explicit quantile function, so each
density costs one root-find per observation. They share the machinery being
built for `tlambda` (TASK-151): a safeguarded Halley inverse with a seed
table, profile fitting with a concave inner fit where the density is log
concave, and an escape and existence inventory. Each has a fixed number of
coefficients, so all are in scope.

## g-and-k (look at first)

With z = Φ⁻¹(u) and the conventional c = 0.8,

$$
Q(u)=A+B\left[1+c\tanh\!\left(\frac{gz}{2}\right)\right]\left(1+z^2\right)^{k}z,
$$

g skewness and k kurtosis. Tails grow polynomially in z (like a power of
log(1/p)), so every moment exists. Q is monotone only for k above about
-1/2, with the exact limit depending on g and c: a validity region like
the original Ramberg-Schmeiser GLD.

Visibility: the standard test problem for approximate Bayesian computation,
where the likelihood is called intractable (for example Allingham, King and
Mengersen, 2009; Prangle's `gk` R package). Rayner and MacGillivray (2002)
used plain numerical ML. A solid ML fitter would show that the likelihood is
merely expensive. It would also show where the real difficulties are:
existence, the validity boundary in k, and any multiple maxima from g.

## g-and-h (Tukey)

$$
Q(u)=A+B\,\frac{e^{gz}-1}{g}\,e^{hz^2/2},\qquad h\ge0.
$$

g skewness (g=0 gives z), h tail weight: power tails with index 1/h, so
moments of order k exist only for k < 1/h. h=0 is a lognormal-type face
on the boundary, so testing for heavy tails is a boundary problem. The
member g=0 inverts in closed form, z = sign(x) sqrt(W(h x²)/h), which is
Goerg's Lambert W × Gaussian (`core/lambert_w.hpp` exists). Used for
operational-risk losses (Dutta and Perry, 2006; Degen, Embrechts and
Lambrigger, 2007).

## Generalized lambda (FKML)

$$
Q(u)=\lambda_1+\frac{1}{\lambda_2}\left[\frac{u^{\lambda_3}-1}{\lambda_3}-\frac{(1-u)^{\lambda_4}-1}{\lambda_4}\right],
$$

valid for every shape pair. With lambda3 = lambda4 = lambda and
sigma = 1/lambda2 it is `tlambda`.

* **Log concavity at fixed shapes in [0,1]^2 (proved).** 1/Q'(u) is
  proportional to pq/(p+q) with p = u^(1-lambda3) and q = (1-u)^(1-lambda4).
  Both are concave for shapes in [0,1], and (p,q) -> pq/(p+q) is concave and
  nondecreasing in each argument, so 1/Q'(u) is concave in u, which is
  equivalent to log concavity in x. A 40-digit grid agrees and fails just
  outside the square. So the location-scale fit at fixed shapes in [0,1]^2 is
  concave in (1/sigma, -mu/sigma), and the tlambda profile fitter extends
  with a 2-D outer shape search.
* **GPD face.** As lambda3 -> inf the quantile converges pointwise to a
  generalized Pareto with location lambda1, scale 1/lambda2 and shape
  -lambda4: an escape to `mlgpd`. lambda4 -> inf gives the reflected GPD;
  both together give a point mass, the analogue of the tlambda spike path.
* **Per-tail regimes.** Each tail is heavy (shape < 0), exponential (0),
  bounded with zero endpoint density (0 < shape < 1), or bounded with
  positive endpoint density (shape >= 1), and the tails mix. Endpoint
  density at fixed shapes is at most lambda2, never infinite, so the
  three-parameter-Weibull failure cannot occur. Conjecture from the tlambda
  analysis: regular inference iff each bounded side has shape < 1/2.
* **Spacings.** The maximum product of spacings objective is at most
  -(n+1)log(n+1), and the spike and collapse paths send it to -inf.
  Unlike ML it needs no fitted shape restriction for those paths. It does
  need a tie convention, and no method separates duplicated uniform members.
* **Literature.** Fitting is mostly by moments, percentiles, L-moments,
  starship or spacings. ML exists only as plain numerical optimisation: Su
  (2007), GLDEX, and `gld::fit.fkml`, whose default is Nelder-Mead from a
  shape grid. A brief search (2026-11-08) found no treatment of existence,
  escapes, uniqueness or inference regimes. Chattopadhyay, Sarkar and
  Kuchibhotla (arXiv 2511.23086) avoid the likelihood on purpose.

## Trigger to revisit

TASK-151 merged, so the fast inverse and profile fitter exist to reuse; or the
[ML-difficulties paper](2026-11-02-ml-difficulties-paper.md) wants an
"intractable likelihood" example; or a user asks for one of these families.

## Verification

Per family, following the tlambda reports: an existence, escape and
uniqueness inventory with the free-subset table; a 50-digit truth table for
d/p/q; a profile fitter prototype on the tlambda agreement harness; and
independent comparisons with `gld`/GLDEX (GLD) and `gk` (g-and-k, g-and-h).
Check full-rank information at interior points before any inference claim.
References above are from memory or a brief search; verify them before
citing.
