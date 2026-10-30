# PIG likelihood and reference audit

The parameterization is `actuar::dpoisinvgauss(x, mean=mu, shape=lambda)`;
`dispersion=1/lambda` is an alternative argument, not another coefficient.
All variances below use denominator n. Derivations here are independent of
any published likelihood-existence theorem.

## Exact mean reduction and existence

Write A=2+lambda/mu², z=sqrt(lambda*A), t=lambda/mu. Integration of the
Poisson kernel against IG(mu,lambda) gives

p_k = sqrt(2lambda/pi) exp(t) (lambda/A)^((k-1/2)/2)
      K_(k-1/2)(z) / k!.

At fixed z, lambda=(z²-t²)/2 and 0<t<z. The entire t-dependent likelihood
is n*t+S*log(lambda). For S>0 this is strictly concave, with its unique
maximum satisfying lambda=t*S/n, hence mu=S/n exactly. This also strictly
improves the t=0 Poisson–Levy face with both parameters free.
On the reduced profile, lambda down to zero gives minus infinite log
likelihood if any count is positive; lambda to infinity gives Poisson(mu).
At delta=1/lambda=0,

ell(delta)-ell(0) = n*mu*(v-mu)*delta/2 + O(delta²).

Thus v>mu guarantees a finite maximum strictly exceeding Poisson (before
the numerical tie rule). Arbitrarily weak improvements can be classified
as Poisson by the specified tolerance 1e-8*n.

## Proof: v <= mu implies Poisson wins globally, including equality

Let m_k=E[Y|X=k] at finite PIG parameters. Put
r_k=z*K_(k+1/2)(z)/K_(k-1/2)(z), so m_k=r_k/A.
For k=0, r_0=z; for k=1, r_1=z+1. For every integer k>=2,

r_k < k + sqrt(z²+k(k-1)).                                      (1)

Here is a direct proof of (1). The Bessel differential equation gives
z*r_k'=r_k²-(2k-1)*r_k-z². Set c=k(k-1), b=k+sqrt(z²+c).
At a crossing r_k=b,

z*(r_k'-b') = c/sqrt(z²+c) > 0.

But the half-integer Bessel expansion at infinity gives
r_k-b = -c/(2*z²)+O(z^-3)<0. Any crossing is from negative to positive
as z increases; a positive region would therefore contradict negativity
at infinity. This proves (1) for all z>0. The expansion follows directly
from the finite half-integer Bessel polynomial, without assumptions on
noninteger orders.

At mu=xbar, Jensen's inequality for the strictly concave square root gives

average(m_X) <= [mu + sqrt(z²+average(X*(X-1)))]/A
             <= [mu + sqrt(z²+mu²)]/A = mu,

since average(X*(X-1))=v+mu²-mu <= mu² and
z²+mu²=mu²*(A-1)². The inequality is strict whenever mu>0:
if any count >=2, (1) is strict; if all counts are 0 or 1, v<mu.
In particular, v=mu>0 necessarily includes a count >=2.

The posterior integration-by-parts identity is
A*m_k=2k-1+lambda*E[1/Y|k]. Substitution into the likelihood score gives,
on the mean-reduced profile,

ell_lambda = (1+lambda/mu²)/lambda * (n*mu - sum(m_X)) > 0.

Therefore the profile is strictly increasing at every finite lambda for
v<=mu, including equality. The Poisson supremum is global and unattained.
All ones are a special case (also p_1=E[Y*exp(-Y)]<exp(-1)).
The fitter still performs the scalar likelihood comparison instead of
relying on an unproved sample-moment shortcut.

## Every fixed subset

* Both free: exact mu=xbar reduction, scalar search over log(lambda).
* Mean fixed: scalar search over log(lambda), comparing Poisson at that
  same supplied mean, not at xbar. The dispersion theorem above does not
  apply when the fixed mean differs from xbar.
* Shape fixed: scalar search over log(mu). The Poisson face is unavailable.
  The Poisson–Levy face at mu=infinity is proper but **cannot win** for a
  finite sample: t=lambda/mu yields A=2+t²/lambda and
  ell(t)=ell(0)+n*t+O(t²). Every finite observed k has finite posterior
  moments at A=2, so the remainder is valid, including samples of zeros.
  For a positive sample, ell tends to minus infinity at mu down to zero;
  the Levy endpoint has a strict finite improvement, proving attainment.
  No uniqueness claim is needed.
* Both fixed: evaluate the log likelihood directly, including all zeros.

With a free parameter, all zeros have supremum 0 at the zero point mass:
mu down to zero if shape is fixed; lambda down to zero if mean is fixed.
These are unattained with positive finite coefficients and are reported
explicitly under D1.

## Numerical search and precision limits

The fitter aggregates counts, makes one stable log-ratio recurrence pass
per likelihood evaluation, scans a log-parameter interval of width at least 96
(center log(mu²) for shape, log(xbar) for fixed-shape mean;
for fixed mean the shape interval also includes the interval centered at
log(xbar²); centers are clipped to [-650,650]), with spacing
0.5, and golden-section refines every bracketed peak. It compares the exact
Poisson likelihood with ties going to the face at 1e-8*n. Endpoint winners
without a winning analytic face and unconverged refinements report failure.
This is a broad numerical global search, not a proof that no peak fits
between grid nodes or outside the interval. Complexity per evaluation is
O(max(x)+number of distinct counts); very large counts can be expensive.

The stable recurrence uses log p0=-2mu/(1+sqrt(1+2mu²/lambda)),
log(p1/p0)=log(mu)-log(1+2mu²/lambda)/2, and log-addition of the
positive recurrence terms. Calculations of mu² and the dispersion ratio
are done through logarithms to avoid overflow. It supports the Levy limit
internally for validation; R fitting accepts only finite fixed parameters.

## actuar defect map and options

`reference_audit.py` generates `reference-grid.csv` and `reference-region.png`
using installed actuar and independent 100-decimal mpmath half-integer Bessel-K polynomial values.
The finite positive polynomial avoids convergence failures in mpmath’s
general hypergeometric Bessel routine at extreme order/argument.
The count-zero, count-two, and rounded-mean slices cover means .01 to 10000
and shape/mean² from 1e-8 through 1e24. The CSV is a sampled map, not an
exhaustive parameter classification. The source mechanisms explain the
regions beyond those slices:

1. Cancellation: actuar computes log p0 as
   `(1-sqrt(1+2*mu²/lambda))*lambda/mu`. When 2mu²/lambda approaches machine
   precision, subtraction loses digits; once rounded sqrt is 1 it returns
   log p0=0. For tiny dispersion the entire pmf approaches exp(mu) times
   Poisson instead of Poisson. Error can be significant well before this.
2. Underflow: actuar exponentiates the initial masses even for log=TRUE.
   If both starting masses underflow, all subsequent recurrence masses
   stay zero, including central counts with substantial true probability.
   Subnormal seeds can lose substantial relative precision before both
   become zero. Conversely, cancellation can inflate the recurrence masses
   until they overflow, yielding positive infinite log probabilities.
   The exact condition depends on the rounded initial masses; stable
   log p0 and log p1 below roughly -745 predict the region. Thus means
   around 750 or larger and sufficiently large shape are vulnerable.
   Underflow can also occur in tail probabilities even when the seeds live.
3. The C recurrence uses integer `i*(i-1)`, which can overflow above 46341;
   the proposed draft uses double loop arithmetic. This extra source-level
   issue is outside the count range of the sampled map.

Reproduction examples:

```r
actuar::dpoisinvgauss(2, mean=2, shape=1e20, log=TRUE) # log(2), true -1.306852819440055
actuar::dpoisinvgauss(1000, mean=1000, shape=1e12, log=TRUE) # -Inf, true -4.372900006026047
```

Finite fitted models can reach the bad region. For example, fixed parameters
can be held at either defective point, and a free fit with mean around 1000
and modest overdispersion already has underflowed seeds. Barely overdispersed
fits have large shape; a near-Poisson likelihood gain exceeding the tie
threshold still need not make actuar's cancellation error negligible.
For example, `mlpig(c(968,1032))` gives mean 1000, shape about 41663609,
and stable log likelihood -9.7691783, while actuar gives -Inf. Fixing shape
at 1e20 for `mlpig(0:3,shape=1e20)` yields stable log likelihood -6.052116
and actuar log likelihood about -0.052116. Installed version: actuar 3.3.7.
The 990-point audit has 426 finite errors >1e-8, 110 negative infinities,
and 36 positive infinities. The standalone draft C replacement passes all
990 points at 1e-8, maximum absolute log error 3.02e-11; full actuar package
integration remains untested. The fitter itself remains stable; its logLik can disagree with
`sum(log(dml(...)))` because dml uses actuar.

Options: (a) document the defect and retain actuar dispatch (implemented);
(b) submit the accompanying upstream C patch after package-level review
(draft only, nothing sent); (c) explicitly approve a flagged replacement
before shipping a new R pmf. No R density or runtime replacement is added.
The existing reference-fix mechanism cannot repair this density using
actuar's own accurate density calls; a handwritten replacement needs the
user's decision. The CDF/quantile functions reuse broken masses and also
need upstream evaluation; no extreme-parameter p/q guarantee is made here.

Source: [actuar poisinvgauss.c](https://raw.githubusercontent.com/cran/actuar/master/src/poisinvgauss.c).
The source copy was recovered from the stopped agent's scratch; the
installed-version probes, not assumptions about future upstream releases,
are the evidence for this audit.
