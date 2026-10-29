# huberfree review report

No commits or git writes. Default `mlhuber(x)` still holds epsilon=1.345;
`mlhuber(x, epsilon=NULL)` estimates it (C++: NaN requests estimation).

## Files

- `distributions/huber/mle.hpp`: free-shape profile, analytic constrained
  normal/Laplace faces, statuses, fixed solver retained unchanged apart from
  its internal name and an optional already-sorted flag.
- `distributions/huber/adapter.cpp`: NULL dispatch, fixed attributes, D1 errors.
- `distributions/huber/README.md`: API, faces and numerical-search bounds.
- `distributions/huber/test.cpp`, `test.R`: faces, scale limit, recovery,
  two-point analytic comparison, all held subsets, direct multistart oracle.
- `notes/evaluations/2026-10-29-huber-free-epsilon-validate.R`: reproducible kurtosis map/recovery/timing.
- Shared file changed: `distributions/README_SCHEMA.md` only; clarifies that
  fixable parameters can have a held adapter default and NULL frees them.
  Paths above are relative to `cppunivariate/include/cppunivariate/`.
- No aggregator, canonical names, helper lists or input-check edits.

## Mathematics: proved (from the stated density)

- Write c=epsilon and Z(c)=sqrt(2*pi) erf(c/sqrt(2))+2 exp(-c²/2)/c.
  For c→0 with sigma=bc, cZ(c)→2 and rho_c((x-mu)/(bc))→|x-mu|/b:
  the face is Laplace(mu,b), **b=sigma/epsilon**, not sigma*epsilon.
  Holding sigma excludes this face. The normal face has sigma unchanged.
- Both faces require free epsilon. `limits` uses `fixed: []` (no required
  held coordinates), with eligibility explicitly qualified in `when` and
  Details; the adapter enforces these conditions, including held mu/sigma.
- For two distinct observations normalize to (-1,1). Concavity/symmetry
  gives mu=0, sigma=min(c,1). For c>=1, ell=-2log Z(c)-1 is strictly
  below normal. For c<1, ell=-2log(cZ(c))+c²-2 increases strictly.
  To see its derivative is positive, put I=integral_0^c exp(-u²/2)du:
  it suffices that c²exp(-c²/2)>c(1-c²)I; use I<=c and
  exp(-c²/2)>1-c². Its value at 1 is below normal too.
  Normal exceeds Laplace by 1+log(2/pi)>0. Thus the joint free MLE never
  exists for distinct n=2; both faces have been compared.
- For free scale, rho_c(z)>=c|z|-c²/2 bounds the profile improvement over
  Laplace by n*c²/2; this also safely prunes below a finite incumbent. Thus c<exp(-9) cannot win at tolerance 1e-8*n.
  For held scale, log f <= log c-log(2sigma)+c²/2 supplies a lower cutoff
  below the incumbent. At the upper cutoff the normal fit lies wholly
  inside the quadratic core and solves the concave inner problem; the
  remaining normalizer is monotone. The exact faces win numerical ties.
- An interior fit strictly better than both boundary suprema implies finite
  attainment (continuity and compact profile level sets). This is a
  sufficient existence criterion, not a universal kurtosis classifier.

## Checked numerically

- Final `just cpp-test`: 4/4 ctest targets passed (28.57 s). Huber-only:
  6 cases, 1539 assertions passed; all counts include the new tests.
- Direct `extraDistr::dhuber` oracle: 18 starts per held subset (four
  subsets), optimizing free mu/log(sigma)/log(epsilon); logLik agrees
  within 1e-7. Fixed defaults also retain their existing oracle tests.
- Seeded n=100000 Huber draws recovered epsilon 0.520412, 0.791371,
  1.34619 for truths 0.5, 0.8, 1.345; fitted scales 3.12356, 2.95812,
  2.99450 for true sigma=3 (mu=2).
- Numerical map, 400 symmetric generalized-normal midpoint quantiles:
  powers 0.6/0.8/1/1.2/1.5 give empirical kurtoses
  10.683/7.091/5.382/4.428/3.617 and finite shape estimates
  0.0373/0.0484/0.0581/0.8998/1.5358. Powers 2/3/4, kurtoses
  2.942/2.400/2.180, select normal. These even samples can favor tiny
  Huber cores over Laplace; the odd sample (-10,-.1,0,.1,10) selects Laplace.
- Map/recovery R recipe: `notes/evaluations/2026-10-29-huber-free-epsilon-validate.R`; scratch logs
  under `/tmp/umvl-codex/huberfree/`.
- `just quality huber`: 1/1 passed; existing fixed-default golden unchanged.
- Final `just test`: 4264 passed, 0 failed, 0 warnings, 0 skips.
  All builds/tests/probes ran through `/tmp/umvl-cpu/run`, one thread each.
- n=1000000 timing, optimized C++17 (`g++ -O2`), generation excluded:
  held epsilon 0.114 s; free epsilon 177.064 s, fitted epsilon=0.801013
  (truth 0.8). Single thread under the shared slot lock, nice priority.
  Free profiling remains expensive at this size; no performance threshold
  was imposed. R's debug-build timing is not substituted for this core timing.

## Assumptions / open questions

- Log-grid spacing <=0.125 plus refinement of **every detected** local
  maximum is a numerical global search, not an interval certificate. It
  assumes no winning peak is narrow enough to escape this grid.
- No global unimodality or kurtosis-only existence theorem is asserted.
- Golden machinery has fit_args but stores one golden per distribution;
  adding free fit_args would replace the held-default baseline. Retain the
  existing golden unchanged and test the free fit against independent oracles.
