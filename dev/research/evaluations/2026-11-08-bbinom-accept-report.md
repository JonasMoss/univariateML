# TASK-116.50: beta-binomial fixed-size coefficient acceptance

Completed in codex/bbinom-accept, based on a6ce88a30535bed8aa6ba719f20dda92b1f4021e.
Every supported fixed-size subset now either passes a coefficient and global
certificate or stops with structured numerical, comparison, limit or budget
details. The finite-profile likelihood allowance has been removed. All final
required suites finished green. The default golden coefficients are unchanged
exactly; the frozen golden file was not edited.

The family's certified flag is now the string 'yes'. Its
[fit note and dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md#fixed-size-coefficient-acceptance)
state the conditional nature of the certificate and its practical floating-point
margins. A successful general finite certificate also proves uniqueness for
that sample. A general symbolic uniqueness classification remains unknown.

| Fixed-size subset | Acceptance or stop |
|---|---|
| Both shapes free, size at least two | Algebraic saturated size-two solution, or mean-score rectangle, negative profile curvature, public-shape enclosure and strict exclusion of the complement |
| Alpha held, beta free | Scalar score/curvature enclosure in beta and strict exclusion outside its region |
| Beta held, alpha free | The preceding scalar certificate after reflecting the counts and exchanging the shapes |
| Both shapes held | Direct finite likelihood evaluation; non-finite evaluation stops |
| Size one, one shape free | The scalar certificate; the Bernoulli likelihood identifies the remaining shape |
| Size zero with a free shape; size one with both shapes free | Nonidentification stop |
| Point-mass, endpoint-only or certified binomial escapes | Existing named limit stops and witnesses |

Free-size searches retain their existing integer tail and comparison
certificates. Their fixed-size children now use the stronger acceptance, with
the same shared enumeration ledger. Estimating size together with both shapes
remains the explicitly unsupported TASK-113 subset.

## Conditional proof of the finite certificate

Write \(h\) for sample size, \(N\) for the held trial size,
\(p=\alpha/(\alpha+\beta)\), \(s=1/(\alpha+\beta)\), and \(q=1-p\).
Omitting the data-only binomial coefficients,

$$
\ell(p,s)=V(p,s)-D(s),\qquad
V=\sum_j\{A_j\log(p+js)+C_j\log(q+js)\},\qquad
D=h\sum_j\log(1+js).
$$

Here \(A_j=\#\{x_i>j\}\) and \(C_j=\#\{x_i<N-j\}\).
The numerator \(V\) is jointly concave; this does not imply global concavity
of \(\ell\). For a non-point-mass sample, \(A_0,C_0>0\), so the mean score
decreases strictly from positive infinity to negative infinity. Thus every
fixed dispersion has a unique interior profiled mean \(p(s)\).

The implementation in
[acceptance.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/acceptance.hpp)
first encloses that mean over a dispersion region \(s\in[s_l,s_h]\).
Its lower and upper score functions are

$$
g_-(p)=\sum_j\left\{\frac{A_j}{p+js_h}-\frac{C_j}{1-p+js_l}\right\},
\qquad
g_+(p)=\sum_j\left\{\frac{A_j}{p+js_l}-\frac{C_j}{1-p+js_h}\right\}.
$$

Both decrease strictly. A lower bracket for the root of \(g_-\) and an upper
bracket for the root of \(g_+\) give \(p_l\le p(s)\le p_h\) throughout
the region. In an uncertain score comparison, the slope magnitude is at least
\(A_0+C_0\); dividing the padded residual by this lower bound retains a
root bracket rather than choosing an unchecked sign.

On this rectangle define

$$
a_j^-=\frac{A_j}{(p_h+js_h)^2},\quad
a_j^+=\frac{A_j}{(p_l+js_l)^2},\quad
c_j^-=\frac{C_j}{(1-p_l+js_h)^2},\quad
c_j^+=\frac{C_j}{(1-p_h+js_l)^2}.
$$

Then the following are uniform bounds:

$$
M=\sum_j(a_j^-+c_j^-)\le-\ell_{pp},\qquad
J=\sum_jj^2\left\{\frac{h}{(1+js_l)^2}-a_j^--c_j^-\right\}\ge\ell_{ss},
$$

$$
B=\max\left(
\left|\sum_jj(c_j^--a_j^+)\right|,
\left|\sum_jj(c_j^+-a_j^-)\right|
\right)\ge|\ell_{ps}|.
$$

Consequently the profile \(L(s)=\ell(p(s),s)\) satisfies

$$
L''(s)=\ell_{ss}+\frac{\ell_{ps}^2}{-\ell_{pp}}
\le J+\frac{B^2}{M}=-K.
$$

Acceptance requires a strictly positive padded \(K\). The initial region
has radius one eighth of the proposed dispersion; successive halvings may
establish the curvature condition. Every halving and solve has the solver
iteration cap, and every scan consumes the common cell ledger.

At a trial dispersion, let the fixed-dispersion mean bracket have midpoint
\(m\) and radius \(r\). If the computed dispersion score has error allowance
\(e_s\), then

$$
\delta_s=\frac{|\ell_s(m,s)|+e_s+Br}{K}
$$

encloses its unique profile root, provided the resulting interval lies
strictly inside the curvature region. Indeed the negative derivative bound
forces opposite weak score signs at the enclosed endpoints. Continuity gives
a root, and strict negative curvature gives uniqueness. Furthermore
\(|p'(s)|\le B/M\), hence

$$
\delta_p=r+\frac{B}{M}\delta_s.
$$

The image bounds for the public coefficients are

$$
\alpha_*\in\left[\frac{m-\delta_p}{s+\delta_s},
                    \frac{m+\delta_p}{s-\delta_s}\right],\qquad
\beta_*\in\left[\frac{1-m-\delta_p}{s+\delta_s},
                   \frac{1-m+\delta_p}{s-\delta_s}\right].
$$

Both widths, including the public double-conversion reserve, must meet the
requested relative coefficient tolerance. Refinement uses the analytic
profile Newton step inside the already proved region. A small optimizer
step or a small score without this curvature conversion cannot pass.

For one held shape \(a\), the free shape \(b\) has

$$
\ell''(b)=-\sum_j\frac{C_j}{(b+j)^2}
                  +h\sum_j\frac1{(a+b+j)^2}.
$$

On \([b_l,b_h]\), a positive padded
\(\sum_j C_j/(b_h+j)^2-h\sum_j(a+b_l+j)^{-2}\) is a lower bound
on \(-\ell''\). The padded scalar score divided by this curvature bounds
the root error directly in the public free shape. The root enclosure must
lie inside the curvature region and meet the coefficient tolerance.
Reflection only permutes the public shape coordinates.

### Global comparison

Local enclosures alone would not establish global optimality. The existing
analytic tail and shape-score bounds give compact search domains. Every
part outside the selected curvature region must now have its tangent/chord
likelihood upper bound strictly below a feasible incumbent's padded lower
anchor. There is no positive likelihood allowance.

For both free shapes the cover includes dispersion zero and extends beyond
the existing \(hH_{N-1}/\#\{0<x_i<N\}\) negative-score tail threshold.
For one free shape it covers the existing analytic positive/negative score
bounds, with the log-grid endpoints extended outside them. Concavity of
\(V\) gives a tangent upper bound, and convexity of \(-D\) gives its chord
upper bound. The residual mean score contributes its existing absolute-score
bound. Subdivision covers the intervals between proposals, so the proposal
grid is never assumed to find every mode.

The enclosed root maximizes its strictly concave profile region. All other
regions are strictly worse than a feasible point in that region; therefore
the root is the unique global maximum. Any missed competing peak prevents
strict exclusion. Separated detected maxima whose values cannot be ordered
within the comparison margin return ProfileTie with both witnesses.
Unresolved covers, unavailable curvature, precision failures and exhausted
ledgers stop rather than return an estimate.

The size-two saturated solution remains algebraic. Its sign test now also
checks coefficient precision: integer products are used as exact only below
the mantissa safety threshold; otherwise the delta error divided by delta,
plus conversion rounding, must meet the tolerance. Non-finite held-shape
search bounds stop before constructing a log grid. Final non-finite
likelihood evaluations also stop.

## Floating-point scope and work

These are exact-arithmetic certificates conditional on computed comparisons,
with practical floating-point margins. They are not outward-rounded intervals
or a rigorous roundoff-error theorem.

Mean and scalar score comparisons use \(32\epsilon_L\) absolute-sum margins;
curvature uses \(128\epsilon_L\), and public coefficient conversion reserves
\(8\epsilon\). The global upper bounds retain their \(128\epsilon\) padding.
Feasible anchors additionally subtract \(256\epsilon(1+|\ell|+hN)\) for
a held-shape profile, or the existing \(1024\epsilon(1+|F|+hN)\) face-scale
margin for a two-shape profile.

Near the binomial face, directly subtracting large dispersion-score summands
can lose the coefficient accuracy. The new evaluator uses the identity

$$
\ell_s(p,s)=g_0+(p-p_0)
 \left\{-\frac{J_A}{pp_0}+\frac{J_C}{qq_0}\right\}
-s\sum_jj^2\left\{
 \frac{A_j}{p(p+js)}+\frac{C_j}{q(q+js)}-\frac{h}{1+js}
\right\},
$$

where \(J_A=\sum jA_j\), \(J_C=\sum jC_j\), and \(g_0\) is the face score
computed from the integer moment numerator in the
[face report](2026-11-07-bbinom-face-report.md).
It selects this expression only when its practical error allowance is
smaller than the direct expression's allowance. Moment arithmetic is treated
as exact only below the intermediate mantissa safety threshold.

Every new solver loop is bounded by min(iterlim, 100000), global exclusion
shares one 128-times-iterlim certificate ledger between its two sides, and
all scans charge the existing 100000-times-iterlim enumeration-cell ledger.
Free-size children receive that same ledger. No shared header changed.
The binding-neutral adapter classifies coefficient failures as numerical
stops and describes finite/face comparison ties consistently in R and JS.

## Evidence and uniqueness counterexamples

The [70-digit checker](2026-11-08-bbinom-accept-data/check.py) independently
solves the original alpha/beta scores using digamma differences. All 60
finite cases passed: the golden and its two held-shape fits, ordinary and
near-binomial samples, five rational saturated tables under all three
estimating subsets, and twelve seeded samples under those subsets.
The largest relative coefficient error was \(2.514\times10^{-12}\), against
the requested \(10^{-10}\). The default golden's two shape coefficients are
bit-for-bit equal to their frozen values. Full results are in
[oracle.json](2026-11-08-bbinom-accept-data/oracle.json).

The rational tables attain the exact multinomial upper bound, providing
independent global witnesses. For example, size-three frequencies
(10,12,9,4) have alpha=2 and beta=3; the uniform size-six table has both
shapes one. Mean and variance identify the two positive shapes for sizes
at least two. The near-binomial size-three table from the face report
retains alpha=beta=18749.5 to the requested accuracy in native and actual
double-width WASM arithmetic.

The C++ additions also check a coefficient enclosure containing the exact
size-three solution alpha=beta=4, reject an unexcluded outside region,
check identified one-held-shape Bernoulli fits, and require numerical stops
for impossible precision and non-finite shape search bounds. The existing
final-cell exhaustion test continues to retain finite and face witnesses.
R and append-only shared JS tests cover public coefficient accuracy and
structured precision stops.

Uniqueness fails in the supported nonidentified regimes. At size one with
data (0,1,1), every pair (alpha,beta)=(2t,t), t>0, has the same maximized
Bernoulli likelihood. At size zero with all-zero data, every positive shape
pair has likelihood one. These are attained continua of maxima and stop as
nonidentification. The finite certificate above proves sample-specific
uniqueness whenever it succeeds.

## Final validation

All commands were foreground runs and finished before this report was written.
Long suites used simbox; workstation computation used the slot wrappers.
BLAS/OpenMP were pinned to one thread, builds used at most four workers, and
long runs inherited nice level 10. The following results use the final
fitter, including the non-finite search-bound guard.

| Check | Machine | Result |
|---|---|---|
| C++ dev | Simbox | 106/106 |
| C++ full | Simbox | 129/129 |
| Whole no-long-double suite | Simbox; workstation | 129/129 on each |
| R dev | Simbox | 22,130 passes; 0 failures/warnings; 154 tier skips |
| R full | Simbox | 44,962 passes; 0 failures/warnings/skips |
| JS build and tests | Workstation | Build green; 552/552 tests |
| Site check | Workstation | Green, including 99 generated C++17 examples |
| Final 70-digit checker | Workstation | 60/60 finite fits; coefficient error below requested tolerance |
| Diff whitespace check | Workstation | Green |

[Simbox summaries](2026-11-08-bbinom-accept-data/simbox-suites.txt),
[workstation summaries](2026-11-08-bbinom-accept-data/workstation-suites.txt),
and [validation.json](2026-11-08-bbinom-accept-data/validation.json)
retain the results. Site validation regenerated an unrelated tracked
reference screenshot; it was restored byte-for-byte from the baseline.

## Timings

The baseline is the stated base commit. Both versions were compiled as C++17
with GCC 15.2 and -O2. Seven interleaved batches of 100 fits per case ran
on one pinned workstation CPU; compilation is excluded. These are small
sample-specific measurements, not a general speed claim.

| Case | Before, ms/fit | After, ms/fit |
|---|---:|---:|
| Finite size-ten fit, both shapes free | 0.3091 | 0.2675 |
| Negative-score size-ten binomial face | 0.06266 | 0.06275 |
| Zero-score size-two binomial face | 0.01987 | 0.01995 |
| Size ten, alpha held | 0.03216 | 0.01401 |
| Size ten, beta held | 0.02492 | 0.00955 |

[timing.json](2026-11-08-bbinom-accept-data/timing.json) retains every trial,
the compiler identity and medians;
[timing.cpp](2026-11-08-bbinom-accept-data/timing.cpp) specifies the samples.

## Reproduction and scope

From the repository root, using the installed dependencies:

    export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
    mkdir -p "$TMPDIR" build/scratch
    /tmp/umvl-cpu/simbox-run bash dev/research/evaluations/2026-11-08-bbinom-accept-data/final-suites.sh
    /tmp/umvl-cpu/run4 bash dev/research/evaluations/2026-11-08-bbinom-accept-data/workstation-suites.sh
    /tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-08-bbinom-accept-data/final-evidence.sh

The [source manifest](2026-11-08-bbinom-accept-data/source.sha256) identifies
the fitter, acceptance helper, dossier, binding and tests, unchanged golden,
and reproduction scripts. Seven implementation/documentation/test files
changed, including the new family-private acceptance header; the shared JS
test change is append-only. No shared core header, d/p/q/r, special function,
truth fixture, generator, golden, NEWS, status or roadmap file changed.
No commits, board edits, sub-agents, package installations or publication
were used. No required implementation item remains open.

Open presentation item: the prescribed artifact-pane show command returned
HTTP 403 because this worktree is outside the viewer's readable roots.
Displaying the report in that pane requires registering this worktree or
adding it to CODEX_MATH_WEB_VIEW_ROOTS. No viewer configuration or file outside
this worktree was changed. The completed report remains at the requested path.
