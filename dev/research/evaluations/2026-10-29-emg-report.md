# EMG implementation report

Status: complete; C++, R, quality, goldens and numerical checks passed.
No commits or git write commands; no board edits; no existing goldens changed.

## Files

New `univariateML-cpp/include/cppunivariate/distributions/emg/`:
`README.md`, `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `dpqr.R`, `test.cpp`, `test.R`, `golden.json`.
Shared files: `univariateML-cpp/include/cppunivariate/cppunivariate.hpp`
(one include registration), `bindings/r/tests/testthat/test_input_checks.R`
(one interior-sample registration for the generic successful-fit contract).
This report is the other root-level addition.
Generated package files are ignored and not source changes.

## Mathematics: derived / proved

- Model: independent N(mu,sigma^2) plus Exp(lambda), both scales positive.
- Put z=(x-mu)/sigma, a=lambda*sigma, M(t)=Phi(-t)/phi(t).
  The exponential-normal term T=exp(a*(a/2-z))*Phi(z-a)
  equals phi(z)*M(a-z) when a-z>=0; density is lambda*T.
  F=Phi(z)-T; survival=Phi(-z)+T. These identities drive direct tails.
- M(t) has Laplace's continued fraction, evaluated backward for t>=12.
  For small a, M(-z)-M(a-z) is the integral of -M' over that interval;
  -M'=1-t*M is itself evaluated without cancellation using the fraction.
- Normal density and exponential density are log concave; convolution is
  strictly log concave here. Conditional location score is
  sum(a-1/M(a-z))/sigma, strictly decreasing; bracketed Newton profiles it.
- At an interior joint stationary point with free mu and lambda,
  mu+1/lambda=mean(x). This is a diagnostic, not a global profile formula.
- Normal face: lambda->infinity, conditional mu and sigma MLEs are analytic.
  Holding lambda removes this face. Holding mu/sigma is respected; mlnorm
  itself fits only the unrestricted normal, as the error message explains.
- Exponential face: sigma->0; free endpoint=min(x), free rate=n/sum(x-min).
  Recover full endpoint likelihood by mu=min(x)-c*sigma, c->infinity and
  c*sigma->0. Thus repeated minima do not require half-density penalties
  when mu is free. If mu is held exactly at a minimum, each tied minimum
  instead contributes -log(2). Holding sigma removes this face.
- Constant data have unbounded likelihood if sigma and lambda are free
  and mu is free or equals the observation. A positive held scale prevents
  this collapse. For w=max(sigma,1/lambda), exponential tilting of the
  larger-scale component bounds f(x) by C/w*exp(-abs(x-mu)/(2*w)), uniformly
  in the smaller scale. A sample range R>0 bounds the likelihood by
  (C/w)^n*exp(-R/(2*w)), which vanishes as w->0. For w->infinity the density
  maximum vanishes; bounded w and diverging mu also lose likelihood.
  Thus nonconstant data have no additional unbounded collapse or escape;
  strictly beating both permitted finite faces gives interior attainment.
- With total mean and variance held at sample m,v, the inward normal
  expansion begins tau^3*sum((x-m)^3)/(3*v^3), tau=1/lambda.
  Positive sample skew excludes a local normal maximum; negative skew
  only establishes local behaviour, not global existence or a face winner.
- If the third central sum vanishes, the next normal-face term is
  tau^4*(sum((x-m)^4)-3*n*v^2)/(4*v^4). For x=-2:2 this is negative:
  normal is locally maximal while the exponential face has larger likelihood.
- For a free endpoint, m tied minima and sigma->0, its location score gives
  m*phi(c)/Phi(c) asymptotic to n*lambda*sigma, with endpoint displacement
  -c*sigma and c of order sqrt(2*log(1/sigma)). The leading likelihood loss
  from the exponential face is -n*lambda*c*sigma. Thus that face is also a
  local maximum, including repeated minima. Boundary multimodality already
  occurs in x=-2:2; a single local optimum cannot settle the global fit.

## Mathematics: numerical checks

- Independent mpmath (400 decimal digits): 147 comparisons, rates 1e-300
  through 1e6, standardized x from -100 through 100. Maximum absolute log
  errors: density 4.47e-13, lower tail 4.50e-13, upper tail 2.24e-13.
- 60 extreme log-quantile round trips passed, including log p=-1e-100.
  Relative checks verify representable log probabilities near one.
  Additional probes cover a subnormal 1e-320 rate with a finite quantile.
- Symmetric and repeated-minimum probes selected translated exponential;
  a 40-point EMG quantile sample selected an interior maximum.
- R convolution and all eight fixed/free subsets passed; seven free subsets
  each use three independent slow-oracle starts (21 optimizations).
- One 500-point EMG golden generated; quality passed logLik (gap 1.03e-12),
  score/information (standardized score 3.70e-7), and eight-start oracle
  (likelihood gap 8e-13). No old golden changed.

## Mathematics: assumptions / limitations

- No general sample-level necessary/sufficient existence classifier or
  uniqueness theorem is claimed. In particular skewness sign and minimum
  multiplicity are not used as such a classifier.
- The outer log-scale search uses 25 starts (five for a one-scale fit),
  with unconstrained Nelder-Mead refinement and analytic face comparisons.
  It is a numerical global search, not a certificate of finding every mode.
- Forty-level tail fraction truncation and eight-point quadrature are
  numerical approximations; the independent high-precision grid checks them.
  Extreme-range arithmetic uses this workstation's extended long double;
  the full parameter range on platforms with 64-bit long double is unverified.
- Face decisions prefer a face within 1e-8*n of the best interior likelihood.
- No existing model fits an unrestricted translated exponential. Its error
  message names the face and explains mlexp(x-min(x)) for free endpoints.
  Checked mlgpd: its strictly-exceeded held threshold excludes sample minima.

## Validation

- `just cpp-test`: 5/5 targets, 312 doctest cases, 506365 assertions passed.
  The base already includes the additional CMP target (five rather than four).
- `just test`: 4978 expectations passed; zero failures, warnings or skips.
- `just quality emg`: 1/1 distribution passed, no skipped parameters.
- `UMVL_GOLDEN_IDS=emg just goldens`: exactly one new golden generated.
- 147 high-precision comparisons and 60 log-quantile round trips passed;
  sampler seeding, direct convolution, all fixed subsets, D1 faces, endpoint
  ties, plot, bootstrap, model_select, and golden regression are covered.
- `git diff --check`: clean. Logs: `/tmp/umvl-codex/emg/`.
All builds, tests and numerical probes run through `/tmp/umvl-cpu/run`.
