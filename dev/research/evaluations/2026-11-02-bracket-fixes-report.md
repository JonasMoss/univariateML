# Verified one-dimensional brackets: LST, slash, Lomax and Gompertz (R6)

Completed 2026-11-02 in `codex/bracket-fixes`, based on
`55149028ca17515418b2df50d92d4b220d822e2d`, with the uncommitted patch identified
below. All six required final commands finished successfully on the workstation.
No commits, board edits, package installs, simbox runs, or edits in other
worktrees were made. Goldens and the settled d/p/q/r, special-function, truth
and generator sources are unchanged. `slash/README.md` is unchanged; proposed
wording appears below.

## Results

| Reproducer | Corrected result |
|---|---|
| `mllst(c(0,1e-300,1),mu=0,df=1)` | sigma `1.000000000000251e-300`, log-likelihood `686.6481910601056` |
| `mlslash(c(0,1e-300,1),mu=0)` | sigma `6.308348019219305e-301`, log-likelihood `686.5298451637411` |
| `mllomax(c(1e300,2e300),kappa=1e300)` | Stops with numerical failure. The root is approximately `6.6667e-601`, outside the positive double range; no subnormal endpoint is returned. |
| `mlgompertz(c(1e-300,2e-300),a=1e-300)` | b `6.9425003677242987e302`, log-likelihood `699.69797343971527` |

Rescaling the Lomax observations to `(1,2)` makes the conditional rate
representable: lambda is approximately `6.6667e-301`, with log-likelihood
`2*(log(2/3)-1)`. This is a representation limit, not mathematical
non-existence. The other three reproductions also pass uniform-rescaling
checks with the appropriate held coefficients and likelihood Jacobians.

## Algorithm and certificate

Each changed scalar branch verifies opposite score signs before bisection.
Initial numerical ranges are seeds, followed by geometric expansion in the
log parameter, clipped to the logs of the smallest positive and largest finite
double. A missing bracket returns `IterationFailure`. A returned parameter must
be finite and positive, have a finite likelihood, and pass an absolute total
log-coordinate score gate of `1e-3`. This is a total score, not a mean score.
An unfinished refinement can survive only through that documented score gate;
there is no unchecked endpoint or last-iterate success.

The following monotonicity statements are mathematical results in exact
arithmetic, for the indicated held coefficients. They give global conditional
maxima, without relying on a grid of likelihood evaluations. Write `n` for
sample size, `k` for the number of zero residuals, and `logistic(z)` for
`1/(1+exp(-z))`.

- **LST, location and df held.** With `t=log(sigma)`, the score is
  `-n+(df+1)*sum(logistic(2*(log(abs(x-mu))-t)-log(df)))`.
  It strictly decreases from `df*(n-k)-k` to `-n`. Under the strict existing
  mass inequality there is one root and one global scale maximum. The former
  radius-minus-350 range is retained only as an initial seed, then expanded
  and checked. Bisection targets log width `1e-12`, subject to `maxit` and the
  final score gate. Residual logarithms handle both squared-ratio overflow and
  subtraction overflow between finite values; weighted probabilities are
  evaluated in log coordinates. Likelihood grouping is preserved where the
  original ratio arithmetic is finite. The binding now describes numerical
  bracket/score failures as well as EM failures.
- **Slash, location held / scale profiles.** Each score term is
  `1-2*u/expm1(u)`, where `u=(x-mu)^2/(2*sigma^2)`; its limit at a zero
  residual is `-1`. The total score strictly decreases from `n-2*k` to `-n`.
  Under `2*k<n` there is exactly one scale maximum. Log residuals and small/tail
  expansions avoid overflow and cancellation. The solver expands both ends,
  then takes at most `min(maxit,65)` bisections, stopping also at floating-point
  midpoint exhaustion. It checks the score before and after conversion back
  to original data units. This last check detects subnormal rounding of the
  returned scale. Overflowing finite residual differences use the existing
  scaled-difference helper. Failed profile points have likelihood `-Inf` for
  candidate selection and retain failure status. An entirely failed search
  cannot be relabelled as a collapse boundary. Existing free-location search
  guarantees are not extended by the conditional scale proof.
- **Lomax, kappa held.** With `t=log(lambda)`, the score is
  `n-(kappa+1)*sum(logistic(t+log(x)))`. It strictly decreases from `n` to
  `-n*kappa`, giving a unique global conditional maximum. The initial seed is
  based on the first observation and held shape; expansion verifies both
  signs within the representable rate range. Log-weighted logistic terms
  retain products of huge shapes and tiny probabilities. Bisection targets
  log width `1e-12`. The existing softplus likelihood is retained, with a
  finite-result check. The joint fitter is unchanged.
- **Gompertz, a held.** The score in `b` is
  `sum(x)-a*sum(integral_0^x u*exp(b*u) du)`.
  Its derivative is strictly negative for positive sample sum. Its zero-limit
  sign gives the existing conditional existence criterion. The solver uses
  observations divided by their maximum for the boundary comparison, avoiding
  underflow in squared tiny observations and preserving the exact constant-data
  equality case. It initializes around `b*max(x)=exp(-2)`, expands a log-b
  bracket, and targets log width `1e-12`. The old cap `b*max(x)<=710` is removed.
  Integral derivatives are evaluated relative to `sum(x)` in log coordinates;
  the final gate is on the total log-b score, `b*score_b`. A small-z polynomial
  is used below `1e-3`, and the log derivative's exponentially small remainder
  is omitted above `z=50`. Logarithmic accumulation computes `a*I(b)` without
  forming overflowing exponentials. Summing `b*x` directly avoids an overflowing
  `sum(x)` even when the total linear likelihood term is finite. The joint
  fitter is unchanged.

These are certificates in exact arithmetic conditional on the computed
comparisons, with the stated practical floating-point score margin. They are
not outward-rounded interval proofs or universal error bounds for library
transcendentals and summation. Conservatively failing at a floating-point
boundary is intentional. Logarithmic evaluations and bracket expansion add
linear passes over the sample; each score evaluation remains O(n).

The rejected ranges had no justified relationship to the smallest residual or
the conditional optimum. In particular, a scale-equivariant range can still
miss a root when residual ratios span hundreds of decimal orders. A fixed
exponential cap can also exclude a valid maximum whose likelihood is finite
because its coefficient multiplies the exponential by a very small number.

## Regression and independent validation

C++ cases were appended at the ends of the four `test.cpp` files; corresponding
R cases were appended to the four family tests. They cover the reproductions,
uniform rescaling, unchanged default goldens, iteration exhaustion, and
unrepresentable roots. Additional cases cover:

- LST and slash at the smallest positive residual scale, which fail rather than
  returning an unchecked floor.
- Slash after back-transformation rounds a valid internal root to a subnormal
  original scale whose score fails the gate.
- Overflowing finite residual subtraction, with data `(M,-M,0)` and held
  location `M`, where `M` is the largest finite double. For LST at df=1,
  `sigma/M=sqrt((sqrt(73)-5)/6)` is an independent analytical reference.
- Gompertz data `(1e308,1e308)` with held `a=1e-308`. The expected scaled rate
  is `b*1e308=1`, and the likelihood after the unit correction is
  `2-2*expm1(1)`. This exercises an overflowing sample sum on binary64-only
  platforms while the likelihood remains finite.

For the small-residual LST case, the negligible tail correction gives the
independent likelihood reference `-log(1e-300)-3*log(pi)-log(2)`.
For slash, its small residual at the maximum satisfies `expm1(u)=2*u`.
Both references are checked independently of the bisection bookkeeping.

An 80-digit Python `decimal` calculation independently solves the extreme
Gompertz score using direct high-precision exponentials. It gives

```
b      = 6.9425003677238882160103773361120525e302
logLik = 699.69797343971527627592773114059637
```

The R fit's relative b error is `5.913e-14`, and its absolute likelihood error
is `6.276e-15`. The verification asserts errors below `1e-10` and `1e-8`,
respectively. This is independent numerical evidence; the uniqueness argument
above supplies the mathematical part. The reproducible calculation is retained
in the appendix.

An initial C++ dev run passed 104/106 shards. It exposed two integration
regressions: a logarithmic Gompertz boundary comparison rounded an exact
constant-data equality into an apparent interior case; a failed slash endpoint
profile initialized candidate selection with NaN. The normalized boundary
comparison and noncompetitive failed-profile likelihood described above fixed
these. The focused four-family rerun passed 4/4. Subsequent final runs below
include the further sample-sum overflow regression and all pass. No old test
was weakened and no golden was regenerated.

## Final runs

Environment: workstation, Linux `7.0.0-38-generic`, x86_64, GNU C++ 15.2.0,
R 4.6.1 (2026-06-24). Computation used the shared slot wrappers, `nice -n 10`,
and BLAS/OpenMP threads pinned to one. The six final commands ran sequentially
under one four-slot reservation from 03:21:08 to 03:35:21 UTC; the additional
R audit and decimal check completed at 03:35:22 UTC. Final-run temporary files
used `build/tmp`; retained scratch and logs are under `build/scratch`.

| Command | Machine | Final result |
|---|---|---|
| `just cpp-test` | workstation | 106/106 dev shards passed |
| `just cpp-test-no-long-double` | workstation | 127/127 tests passed |
| `just test` | workstation | FAIL 0, WARN 0, SKIP 151, PASS 20260 |
| `just test-full` | workstation | FAIL 0, WARN 0, SKIP 0, PASS 41863 |
| `just js-build` | workstation | exit 0 |
| `just js-test` | workstation | 413/413 passed; no failures or skips |
| Nine-family R audit | workstation | completed; open finding below |
| Gompertz decimal comparison | workstation | both error assertions passed |

The 151 R dev skips are tier exclusions; full has none. There are no permitted
final test failures. Compiler/configuration warnings were emitted from unchanged
code: CMake CMP0167; special-function inline declarations; existing indentation,
unused-variable and test range-loop diagnostics; and an Emscripten binomial
self-comparison warning. These are distinct from the zero R-test warning count.
System Boost discovery uses the recipe's installed-BH fallback; no installation
was performed. Initial standalone audit compilation likewise needed the explicit
installed BH include path before succeeding.

Reproduction of the six required runs, without nested slot locks:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run4 bash -e -c '
  export CTEST_PARALLEL_LEVEL=4 EMCC_CORES=4
  just cpp-test
  just cpp-test-no-long-double
  just test
  just test-full
  just js-build
  just js-test
'
```

## Recheck of the nine earlier audit rows

The cited Cauchy report is absent at this worktree's base revision. It was read
from shared Git history without checking out or editing another worktree:

```sh
git show 6dd5ae7f2728c4b0a06ca2f28e6684f42c98b922:dev/research/evaluations/2026-11-02-cauchy-bracket-report.md
```

The following are additional R attempts, distinct from that report's inputs.
The large Zipf support and `iterlim=1` exercise its fallback. The log-logistic
and Weibull attempts push toward the small-shape side, opposite the earlier
near-tie large-shape attempt. These are adversarial numerical checks, not proofs
that all other numerical ranges are correct. None of these families was edited.

| Family | Additional call | Observation |
|---|---|---|
| logis | `mllogis(c(-1e300,0,1e-300),location=0)` | scale `3.082958703562489e299`, ll `-2074.88928294665`; absolute total log-scale score `8.127e-13` |
| gamma | `mlgamma(c(1e-300,1,1e300),scale=1e-300)` | Stops with `IterationFailure`; no successful endpoint |
| llogis | `mlllogis(c(1e-300,1,1e300),scale=1)` | shape `0.002866862243649718`, ll `-23.427781210550734` |
| weibull | `mlweibull(c(1e-300,1,1e300),scale=1)` | shape `0.0016063661683380593`, ll `-23.664281690194802` |
| ht | `mlht(c(0,1e-310,1e300),mu=0,nu=1)` | sigma approximately `1e-310`, ll `-669.79757226470156`; computed total log-scale score zero |
| sgomp | `mlsgomp(c(1e-300,1,1e300),b=1)` | **Failure established:** succeeds with eta `0`, whose right derivative is positive |
| gumbel | `mlgumbel(c(0,1e-300,1),mu=0)` | sigma `0.31885160419331571`, ll `-1.7506119570068566` |
| gpd | `mlgpd(c(1e-300,1,1e300),mu=0,xi=1,iterlim=500)` | Stops with failure to converge; no accepted edge |
| zipf | `mlzipf(c(1L,1L,2147483647L),N=2147483647,iterlim=1)` | shape `1.0863487403184955`, ll `-30.354904469538759` |

C++ screening additionally checked the corresponding log-logistic, Weibull,
Gumbel and Zipf interior solutions' analytic scores, obtaining residuals of order
`1e-12` or smaller. No new failure was established in the other eight families by these attempts. Their general unchecked-range questions remain
outside the certificate established for the four repaired branches.

## Open findings outside the authorized edits

### 1. Shifted Gompertz can overwrite the interior eta optimum with its face

For `x=c(1e-300,1,1e300)` and held `b=1`, the fitter succeeds with eta zero.
Put `A_i=1-exp(-x_i)` and `E_i=exp(-x_i)`. The eta-dependent improvement over
eta zero is exactly

```
Delta(eta) = sum(log1p(eta*A_i)-eta*E_i).
Delta'(eta) = sum(A_i/(1+eta*A_i)-E_i).
```

Its second derivative is strictly negative. The right derivative at zero is
approximately `1-2/exp(1)=0.26424111765711533>0`, so zero cannot be a
conditional maximizer. The positive-derivative argument establishes the failure;
the following reference values are numerical:

```
eta optimum       = 0.226912564691088
Delta(optimum)    = 0.0281497209864791
Delta(0.2)        = 0.027793780794203415
```

The final `free_eta` face comparison in `sgomp/mle.hpp` uses full log-likelihoods
near `-1e300`. Both values round to the same number and
`face.ll >= best.ll-1e-8*n` selects the face, despite an improvement much larger
than its intended `3e-8` margin. This is a likelihood-comparison failure, not a
new demonstration that the inner eta bracket misses its root.

**Open:** an owner-authorized change to `sgomp/mle.hpp` must preserve small
likelihood improvements in the face comparison and verify the appropriate
interior/face score conditions, with C++ and R regressions. No sgomp file is
owned by this job, so no fix was applied. The stable gain calculation below
and the R reproducer retain the evidence.

### 2. Existing Gompertz sampler overflows for the repaired extreme fit

With seed `20261102`, `rml(5,mlgompertz(c(1e-300,2e-300),a=1e-300))`
returns five `Inf` values. Its true quantiles are on the tiny observation scale.
The existing `gompertz/dpqr.hpp::rgompertz` forms `b/a`, which overflows even
though the sampled values can be finite. This can be seen directly in
`log(1-(b/a)*log(u))/b` for interior uniforms.

**Open / needed settled-source change:** replace that sampler arithmetic by a
stable inverse computation, for example through the existing stable Gompertz
quantile path, and add extreme-parameter sampling regression coverage. The brief
forbids d/p/q/r edits in this job, so the sampler, generated R sampler, special
functions, truth fixtures and generators were not changed. No other needed
settled-source change was established here.

## Proposed slash dossier wording

The following text is supplied for the parallel owner; it was not inserted into
`slash/README.md`:

> With location held, the total log-scale score is strictly decreasing from
> `n-2*k` to `-n`, where `k` observations equal the held location. Under the
> strict existence inequality it has a unique root, giving the global
> conditional scale maximum. Scale profiles expand a log-scale bracket within
> the representable positive range until opposite score signs are verified,
> then bisect subject to `iterlim`. Log residuals, small-argument series and
> tail limits stabilize the score. The returned original-unit scale must have
> absolute total log-scale score below `1e-3`, including after rounding during
> back-transformation; otherwise the result is `IterationFailure`. Failed
> profile evaluations are excluded from likelihood candidate selection, and
> an entirely failed search remains a numerical failure. These certificates
> hold in exact arithmetic conditional on computed comparisons, with a
> practical floating-point score margin. The free-location search's overall
> guarantee is unchanged.

## Reproducible independent checks

After generating/loading the package with the final sources, the stable shifted
Gompertz check needs only base R:

```r
x <- c(1e-300,1,1e300)
f <- mlsgomp(x,b=1) # returns eta=0
A <- -expm1(-x); E <- exp(-x)
score <- function(eta) sum(A/(1+eta*A)-E)
gain <- function(eta) sum(log1p(eta*A)-eta*E)
eta <- uniroot(score,c(0,1),tol=1e-14)$root
c(score.at.zero=score(0), eta=eta, gain=gain(eta), gain.at.point.two=gain(.2))
set.seed(20261102)
rml(5,mlgompertz(c(1e-300,2e-300),a=1e-300))
```

The independent decimal reference below can be saved under `build/scratch` and
run with `/tmp/umvl-cpu/run python3 <file>`. It uses no third-party package.
The final run also compared the reference with the 17-digit R results, asserting
the error margins reported above.

```python
from decimal import Decimal, getcontext
getcontext().prec = 80
D = Decimal
alpha = D('1e-600')  # held a times the observation unit 1e-300
# z = b * 1e-300; observations in these units are (1,2).
def score(z):
    return D(3) - alpha * sum((j*z-1)*(j*z).exp()+1 for j in (D(1),D(2))) / (z*z)
lo, hi = D(600), D(750)
for _ in range(220):
    mid = (lo+hi)/2
    if score(mid) > 0: lo = mid
    else: hi = mid
z = (lo+hi)/2
penalty = alpha * sum((j*z).exp()-1 for j in (D(1),D(2))) / z
ll = 2*D('1e-300').ln()+3*z-penalty
print('b =', z/D('1e-300'))
print('logLik =', ll)
print('total log-b score =', z*score(z))
```

## Tested patch identity

SHA-256 identities after all final runs are below. The report was written
afterward; fitting and test sources were unchanged during report preparation.
All started computation commands had finished before this report was written.

```
127389c1db3b4a5dd9d062fcc5c0d0d35ab7226f0d9bb1ea91b3b77a556a5525  bindings/common/distributions/lst/fit.hpp
8b98b2cc0886e6ffa69aebc2cc08a4bb6a7a6796c48dbf2dc31a443db4d3f0c7  bindings/r/distributions/gompertz/test.R
fd30b44502a46f8d6849636dc24534acccdc4076909d41bb85533aff320cbc9e  bindings/r/distributions/lomax/test.R
c471a83c6d1a733b933ad6473edb87352f7370c2bf7ab310781f9c3b3a5bd3bd  bindings/r/distributions/lst/test.R
9b6930874ce6018f796f43403a05865b150a8ab92a2cbc380718228de90bd42a  bindings/r/distributions/slash/test.R
9bb174c1ce6bd474158eb9eb82af75f63b6fcd0224d917647d3eec784b786fb8  univariateML-cpp/include/cppunivariate/distributions/gompertz/README.md
cab2eb6d11ad0e0f399dc7e9094b6f2a6b387f1821454ed33609cfba225cf21b  univariateML-cpp/include/cppunivariate/distributions/gompertz/mle.hpp
57c03e63d782496ec37db38bca37f71402c064dfda0ca77803cad33d736e103c  univariateML-cpp/include/cppunivariate/distributions/gompertz/test.cpp
6e76cfdce76cee2f81dc5d48cf32df9d8d6b56b6c2b0534ed38782e184263ccc  univariateML-cpp/include/cppunivariate/distributions/lomax/README.md
80079ffe85a12182fc016b8bcbe94cbfd090bcc36970c16e520381d9f6d2273b  univariateML-cpp/include/cppunivariate/distributions/lomax/mle.hpp
ce25a093cd8216f5dc178ffcb0f34bf57bbfaab45bda07da6cd642960ca37b0d  univariateML-cpp/include/cppunivariate/distributions/lomax/test.cpp
0be039573e5b39f153d8e549d7cf9ecd456ec43af952678a902f2e423c99839e  univariateML-cpp/include/cppunivariate/distributions/lst/README.md
8082ac00a9f113121f92a2755f4bb1b81ef9177ee94c5f89ef129a8d9e2f0827  univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp
57a1f0d517192b2d94b5ec25e0e19188629533584dfeacf152d52c4a16d870ee  univariateML-cpp/include/cppunivariate/distributions/lst/test.cpp
e69dcd035ed1a061a6767bdfb34cb617dac938b7be25904694a72ba39369ec68  univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp
dd4b05ba027f42567f4f29f966d513e024e72190661250e1f2797fb657198b93  univariateML-cpp/include/cppunivariate/distributions/slash/test.cpp
```

## Artifact delivery

The prescribed `codex-math-web/bin/show` command was attempted after writing
this report. The viewer returned HTTP 403 because this worktree is outside its
configured readable roots. No viewer settings or files outside the worktree
were changed. The report remains at the requested path.
