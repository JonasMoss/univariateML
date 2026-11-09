# TASK-163: extreme-shape t and GED d/p/q/r

Completed on 2026-11-09 in the tail-extreme worktree, based on 072822a2.
All four reported defects are fixed, the related-family audit is complete,
and every final validation command exited successfully. Changes remain
uncommitted. No merge or rebase was performed.

## Results

1. Student tails and inversion remain usable at huge degrees of freedom.
   The sstd quarter quantile at shape 1e100 and its origin CDF at DBL_MAX
   are finite. Unsupported backend calculations return NaN instead of
   supplying an invalid inversion bracket.
2. GED inversion retains the coordinate needed by the power transform.
   The skew-GED quarter quantile at shape 1e100 is approximately
   -0.8660254037844386, agreeing with -sqrt(3)/2 and its CDF, rather than
   the former positive endpoint.
3. GED sampling preserves random amplitudes when the underlying tiny-shape
   gamma variable would underflow. Seeded GED and skew-GED samples retain
   spread and the correct first two moments at shapes through DBL_MAX.
4. The portable large-gamma and Student samplers avoid the overflowing
   libc++ proposal. The WASM regression exercises every affected family
   through DBL_MAX in a separately killable process.

The audit also fixed GED evaluation at adjacent doubles around the uniform
edge, standardized-t density evaluation after squaring overflow, half-t
endpoint cancellation, and the large-shape skew-t integration kernel.
Unrepresentable GED log scales, unresolved transformed GED edges, and
overflowing skew standardizations now produce NaN.

## Mathematics and numerical safeguards

### GED probabilities and quantiles

Put a = 1/nu and lambda² = Gamma(a)/Gamma(3a). For a unit-variance GED,
the positive-side survival probability at z is

    Q(a, (z/lambda)^nu) / 2.

The stable inversion coordinate is w = log(G)/nu, so that the final radius
is exp(log(lambda) + w). It avoids first representing G, or multiplying w
by nu and subsequently dividing a rounded or infinite result by nu.

For x = exp(nu*w) <= 1, integration of the exponential series gives

    P(a,x) = exp(w - logGamma(1+a))
             * [1 + a * sum(n>=1) (-x)^n / (n! * (a+n))].

The terms alternate with decreasing magnitude in this region; the next
term bounds the exact-arithmetic truncation error. The implementation
uses logGamma(1+a), log1p, and log1mexp, preserving the small upper
probability. For x > 1 and a <= 1, a log-domain continued fraction
evaluates Q directly, including its log(a) prefactor. Larger a delegates
to the special-function seam.

P is strictly increasing in w and Q is strictly decreasing, because the
gamma density is positive. Both bracket endpoints must pass their
comparisons before bisection starts. A NaN comparison aborts. Bisection
returns only when its endpoints are adjacent in binary64; budget
exhaustion returns NaN. Bracket expansion, bisection, the series, and the
continued fraction all have finite limits and failure exits.

This certifies the inversion bracket in exact arithmetic, conditional on
the computed probability comparisons. Special-function and floating-point
accuracy are numerical claims, supported by the independent references
below; no outward-rounded enclosure is claimed.

### The GED uniform edge

The recurrence identity

    lambda² = 3 * Gamma(1+a) / Gamma(1+3a)

separates the small finite-shape correction from sqrt(3). Near that edge,
the implementation evaluates

    log(|z|/lambda)
      = 0.5 * log1p(fma(|z|, |z|, -3)/3)
        - 0.5 * [logGamma(1+a) - logGamma(1+3a)].

Subtracting two ordinary logarithms can lose the side of the edge.
At shape DBL_MAX, adjacent input doubles can have a finite plateau
density or an underflowed density, and very different log tails.
Replacing every large finite shape by a uniform law would miss these
values.

Affine and skew transforms introduce another source of uncertainty.
Their practical forward-error allowance is 32 machine epsilons times
the participating magnitudes. Interior results must resolve the edge
distance or the 1/nu tail scale to the implemented 1e-11 relative margin.
An uncertain exterior radius is accepted only when its lower bound
already puts the log gamma argument beyond log(DBL_MAX)+2. Otherwise
evaluation returns NaN. Exact unit inputs use the fused calculation
directly. These are practical floating-point safeguards, rather than
interval arithmetic.

The standardized GED tends to Uniform(-sqrt(3),sqrt(3)). A two-piece
skew-uniform has constant density over its entire interval; centering and
standardizing it gives the same limit for every fixed positive skew
parameter. This explains the skew-GED quarter-quantile reference.

### GED sampling

For independent Y ~ Gamma(a+1,1) and U ~ Uniform(0,1),

    G = Y * U^(1/a) ~ Gamma(a,1),
    log(G)/nu = a*log(Y) + log(U).

The distributional identity follows by integrating the conditional
density over Y: its density at g is proportional to
g^(a-1) times the integral of exp(-y) from g to infinity.
The resulting density is g^(a-1)*exp(-g)/Gamma(a).

Applying the GED power in this log expression preserves U even when G
is unrepresentable. As a tends to zero, the radius tends to sqrt(3)*U,
giving the required uniform limit. The augmented path starts at nu=16.
The ordinary path below that threshold retains its previous primitive
draw sequence and arithmetic. Zero, subnormal, or nonfinite gamma draws
are rejected with NaN before powering.

### Student probabilities, densities, and sampling

At degrees of freedom at least 1e8, direct Student probabilities replace
the beta-tail bridge that can round away its complementary argument.
For sufficiently larger degrees of freedom, the normal-tail bridge is
selected only when

    nu >= 1e16 and (1+|x|)^4 / nu < 1e-16.

The leading relative Student-to-normal tail correction is of order
(1+|x|)^4/nu. The bridge's numerical accuracy is checked by the
high-precision references and logarithmic-tail regressions. A lost beta
complement in the remaining backend fallback produces NaN. Large-shape
quantiles must also pass a CDF residual check of
2e-12 * max(1, abs(requested log probability)).

Density evaluation preserves the scaled squared radius. In particular,
log(z²/(nu-2)) cannot replace log1p(z²/(nu-2)) merely because z²
overflowed. For nu=DBL_MAX and z=1.3407807929942597e154, the corrected
standardized-t log density is about -6.230329639708919e307; the former
path incorrectly returned the mode density. Independent 360-digit
working-precision references cover this case and a second overflowing
square.

The half-t CDF integrates its density series near zero, avoiding
subtraction of nearly equal Student probabilities. Its small-probability
inverse uses the cubic correction to the local inverse. The skew-t wedge
kernel preserves nu*log1p(y) before y becomes subnormal; for y<1e-8,
the series for log1p(y)/y has remainder bounded by y³/4 after the retained
quadratic term.

The portable gamma sampler uses a bounded Marsaglia--Tsang path for
shape at least 1e6:

    d = shape - 1/3, c = (1/3)/sqrt(d), v = (1+c*Z)^3.

This avoids overflowing multiples of shape. Its acceptance calculation
evaluates log1p(h)-h stably, with h=v-1. After 10000 unsuccessful
proposals it returns NaN. The large Student sampler, starting at
nu=2e6, uses Z/sqrt(G/(nu/2)) with G ~ Gamma(nu/2,1), avoiding both the
standard-library large-shape path and an overflowing doubled gamma draw.
An unrepresentable halved shape is rejected before calling a primitive.

## Family audit and regressions

| Family | Extreme-shape paths covered |
|---|---|
| lst | Direct/normal Student tails, checked quantiles, log tails through x=1000, stable density kernel, portable t sampling |
| std | Shared Student tails and sampler; scaled-radius density after square overflow |
| sstd | Student-tail consumers, huge-shape origin CDF and quarter quantile, finite skew standardization |
| ht | Student-tail consumer, relative endpoint accuracy down to probability 1e-100, density and sampling |
| st | Stable Student density and wedge kernel, skew-normal limit and inversion, gamma/t sampling |
| ged | Scaled gamma inversion, tiny reciprocal shape, exact-input uniform edges, augmented sampling |
| sged | Scaled gamma inversion and augmented GED consumer, skew-uniform limit, transformed-edge failure gates |

C++ cases were appended to the GED and lst test files and ran in native
and no-long-double builds. R regressions in the corresponding family
test files exercise all seven model interfaces. JS checks exercise all
seven families at 1e8, 1e100, and Number.MAX_VALUE, along with density
overflow, edge tails, logarithmic inversion, and sampling spread.

The JS sampler regression runs in a child process with a 30-second
timeout and SIGKILL. A synchronous WASM rejection loop cannot defeat
this parent-process deadline. The earlier unbounded MAX-shape sample
call was moved into this bounded coverage. The final bounded test
passed in 168.8023 ms.

## Independent numerical evidence

The retained [evidence directory](2026-11-09-tail-extreme-data/) contains
the C++ probes, Python oracles, comparison scripts, before-fix witnesses,
and final outputs. The oracles use 360 working decimal digits in mpmath,
with the exact supplied binary64 parameters and observations.
The Student oracle integrates the independently normalized density.
Gamma tails use independent incomplete-gamma evaluation. Interior GED
references use the exponential series with an explicit negligible
remainder bound; edge references distinguish all three neighboring
doubles around sqrt(3).

| Comparison set | Checks per build mode | Largest observed error |
|---|---:|---:|
| Central GED/skew-GED/skew-t quantiles, Student density, tiny-shape gamma log tails | 27 | 3.33e-16 |
| GED edge log densities and log survival probabilities | 18 | 1.79e-16 |
| Student log densities after square overflow | 4 | 1.51e-14 |

The first two error measures divide absolute error by max(1,abs(reference));
the last uses relative error. Both build modes passed all 49 comparisons,
giving 98 final checks. The largest density error comes from the existing
log-coordinate lst evaluation and is below the 1e-12 comparison tolerance.

Reproduce the final comparisons from the worktree root with

    /tmp/umvl-cpu/run4 sh dev/research/evaluations/2026-11-09-tail-extreme-data/run-evidence.sh

The [central oracle](2026-11-09-tail-extreme-data/oracle.py),
[edge oracle](2026-11-09-tail-extreme-data/edge-oracle.py), and
[density-overflow oracle](2026-11-09-tail-extreme-data/std-overflow-oracle.py)
are independent of the library's truth tables.

## Final validation

Every command below finished with exit status zero. Local runs used
the four-slot wrapper, with its nice priority and single-thread BLAS
settings. Remote runs used simbox-run; C++ build/test parallelism was
explicitly limited to four. Each suite was a separate foreground command.
All results are from the final source, including the density-overflow fix.

| Command | Machine | Result |
|---|---|---|
| just cpp-test, dev | Workstation, GCC 15.2 | 106/106 shards |
| UMVL_TEST_TIER=full just cpp-test | Simbox, GCC 13.3 | 129/129 shards |
| just cpp-test-no-long-double | Workstation, GCC 15.2 | 129/129 shards |
| just cpp-test-no-long-double | Simbox, GCC 13.3 | 129/129 shards |
| just test | Simbox | PASS 22269; FAIL 0; WARN 0; SKIP 154 |
| just test-full | Simbox | PASS 45101; FAIL 0; WARN 0; SKIP 0 |
| just js-build | Workstation | Successful |
| just js-test | Workstation, WASM/libc++ | 557/557; no failures or skips |
| just site-check | Workstation | Successful; 99 generated C++ snippets compile/link; browser checks pass |
| run-evidence.sh | Workstation, both precision modes | 98/98 comparisons |

Exact commands, terminal summaries, and hashes of the changed tracked
files are retained in [validation.json](2026-11-09-tail-extreme-data/validation.json)
and [validation.txt](2026-11-09-tail-extreme-data/validation.txt).
The site check's unrelated regenerated screenshot was restored to its
original bytes.

No fitter files, public fitter names, defaults, code generators, truth
tables, or goldens were changed. Existing fitter golden regressions
passed in the final suites. The truth/golden diff and whitespace checks
are clean. NEWS, status files, roadmap files, the board, and other
worktrees were not edited. There are no open items for the four defects
or the requested related-family audit.
