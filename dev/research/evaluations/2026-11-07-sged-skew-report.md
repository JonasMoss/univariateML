# SGED fixed-shape free-skew certificates — TASK-116.37

Implemented two certified routes in `sged/skew_certificate.hpp`: exact delegation
at held shape ν = 2 for all four free-skew subsets, and exhaustive knot/face
comparison for 0 < ν ≤ 1 with both moments free. The latter fixes a reproducible
missed maximum: on `(-4,-1,0,0,0,0,0,0,0.3,2)` at ν = 1/2, the old conditional
search returned log likelihood −8.75051485, whereas the certified maximum is
−5.83627325.

The family flag remains **`certified: partial`**. Its `fit.note` now identifies
the additional scopes. The default free-shape fitter and all golden files are
unchanged. No commits were made; the worktree remains based on `5c9c02d8`.

## Scope

Sets in this table name **free coefficients**, with ν held throughout.
“Open” means the existing numerical search remains reachable and is not
promoted to a certificate.

| Free subset | 0 < ν ≤ 1 | ν = 2 | ν > 1, ν ≠ 2 |
|---|---|---|---|
| {xi} | Open | Normal delegation | Open |
| {mean, xi} | Open | Normal delegation | Open |
| {sd, xi} | Open | Normal delegation | Open |
| {mean, sd, xi} | Knot/face certificate | Normal delegation | Open |

The previously certified fixed-skew subsets are preserved. Their canonical
records remain the [held-shape report](2026-11-06-sged-cert-report.md) and
[subunit scale report](2026-11-07-sged-subunit-report.md).

## Why the new routes are global

Write the fixed-shape two-half-scale density as

\[
f(x)=\frac{\nu}{\Gamma(1/\nu)(a+b)}
\begin{cases}
\exp[-((c-x)/a)^\nu],&x<c,\\
\exp[-((x-c)/b)^\nu],&x\ge c,
\end{cases}
\]

where (a,b>0). Its public coordinates are

\[
k=\frac{\Gamma(2/\nu)}{\Gamma(1/\nu)},\qquad
v=\frac{\Gamma(3/\nu)}{\Gamma(1/\nu)},\qquad
\mu=c+k(b-a),\quad
\sigma^2=v(a^2-ab+b^2)-k^2(b-a)^2,\quad
\xi=\sqrt{b/a}.
\]

**Held ν = 2.** These are exactly the public mean, standard deviation and skew
coordinates of the two-piece normal. The public `snorm_mle` entry point selects
its joint polynomial certificate when both moments are free, and its
conditional certificate otherwise. SGED transports finite results, statuses,
candidates, ties and budgets. Finite candidates acquire the held ν coordinate.
Half-normal scales are multiplied by √2, and their models become half-GEDs
with ν = 2, preserving the same density and likelihood. Conversion must retain the
scale within $10^{-10}$ in logarithmic coordinates; otherwise it stops
numerically. Tests cover a finite half-normal scale whose half-GED conversion
overflows, and a subnormal scale whose conversion loses coefficient accuracy.
There is no
near-normal approximation or delegation for shapes merely close to two.

**Both moments free, 0 < ν ≤ 1.** For a fixed join (c), let

\[
A(c)=\sum_{x_i<c}(c-x_i)^\nu,\quad
B(c)=\sum_{x_i>c}(x_i-c)^\nu,\quad
r=\frac1{1+\nu},\quad S=A^r+B^r.
\]

The negative log likelihood is strictly convex in ((\log a,\log b)) when
both sums are positive: it is a log-sum-exp term plus two strictly convex
exponentials. Solving its scores gives the unique half-scale optimum

\[
t=(\nu S/n)^{1/\nu},\qquad a=tA^r,\quad b=tB^r,\qquad
\ell(c)=n\{\log\nu-\log\Gamma(1/\nu)-\log(tS)-1/\nu\}.
\]

A zero sum gives the corresponding half-GED face by the same formula. Its
nonzero scale is ((\nu\operatorname{mean}|x-c|^\nu)^{1/\nu}).

Between successive distinct observations, (A) and (B) are nonnegative
concave functions for ν ≤ 1. The power (r) is increasing and concave, so
(S) is concave there. Maximizing the profiled likelihood is equivalent to
minimizing (S); a minimum is therefore at an observation. Outside the sample
range, moving the join towards the nearest observation decreases every
residual at fixed half-scales. Thus enumerating the distinct observations
covers every finite maximizer and both winning half-GED possibilities. No
finite skew cutoff, local-start assumption or profile-unimodality assumption
enters this argument.

These are exact-arithmetic arguments conditional on the computed comparisons,
with the practical floating-point margins below. They are not claims of
outward-rounded interval arithmetic.

## Uniqueness fails

* For the two-point sample ((-1,1)), the two reflected half-GED faces tie
  for every 0 < ν ≤ 1. The new route returns a structured tie with both faces.
* Finite maximizers can also tie. At ν = 1/2, take (-100), five copies of
  (-1), five copies of (1), and (100). There are only four distinct
  candidate joins. At either interior join (S<12), whereas at either
  endpoint (S>20). Reflection gives exact equality at the two interior
  joins. Hence there are two separated finite global MLEs, not merely a
  face ambiguity. Their log likelihood is approximately −43.23807453471509.

For the second example, the two values on the left half of the sample are

\[
S(-1)=99^{1/3}+(5\sqrt2+\sqrt{101})^{2/3}<5+7=12,
\qquad
S(-100)=(5\sqrt{99}+5\sqrt{101}+\sqrt{200})^{2/3}>109^{2/3}>20.
\]

For the first inequality, (5\sqrt2+\sqrt{101}<37/2) and
((37/2)^2<7^3). Reflection supplies the other two joins. These strict
separation bounds give an analytic proof; the decimal likelihood is
numerical. The tests retain both finite witnesses.

## Arithmetic, stops and work

The implementation normalizes by the sample range, uses compensated power
sums, restores the density Jacobian, and checks distinct normalized knots.
Candidate comparisons allow

\[
4096\epsilon(1+n+|\ell|)(1+1/\nu).
\]

Candidate likelihood bounds additionally include the discrepancy between the
ideal profile value and the density evaluated at stored coefficients. A finite
fit must reconstruct its join within (10^{-10}(1+|c|)) and its likelihood
within (10^{-7}(n+|\ell|)). Public-mean cancellation is screened with
(256\epsilon) times the absolute terms against (10^{-7}(1+|\mu|)).
Finite and face scales must recover their logarithmic ideal values within
(10^{-10}); face likelihoods are reevaluated at the stored coefficients.
Positive side-scale underflow, nonfinite moments, lost knots or failed
reconstruction produce a numerical stop. Rounded subnormal face scales are
explicitly tested.

The knot route consumes at most (128\min(\mathtt{iterlim},100000))
certificate evaluations. Sorting is (O(n\log n)); evaluating (k) knots
is (O(nk)). If the distinct-knot count already exceeds the budget, the
route stops after one (O(n)) face witness instead of spending predictable
quadratic work. The 100,000-observation regression checks this preflight.
A budget stop keeps the witness and an infinite global upper bound. A
completed comparison retains a finite global upper bound on a tie.

No new certified route falls back to an unchecked optimizer. Numerical,
budget and tie outcomes use `CertificateFailure` and `StopDetail`. The
binding-neutral SGED adapter preserves a delegated budget even when no
candidate could yet be evaluated, matching the normal adapter. R and JS use
the same core routing. No shared core header was changed.

## Evidence and timings

The [70-digit checker](2026-11-07-sged-skew-data/check.py) verifies the
half-scale formulas and public-coordinate density identity in 12 deterministic
configurations, checks interior join grids against the knot winners, and
checks five held-moment contact limits. Its [saved output](2026-11-07-sged-skew-data/high-precision.json)
is numerical evidence; exhaustiveness comes from the concavity proof above.
A C++ regression compares the ν = 1/2 finite coefficients with this reference
at relative tolerance (2\times10^{-12}). Additional C++, R and JS cases
exercise the normal delegation, finite and face ties, likelihood recovery,
budgets, cancellation, subnormal scales and normal-face conversion overflow.

The [paired benchmark](2026-11-07-sged-skew-data/benchmark.cpp) uses the
10-observation sample from the opening paragraph, both moments free,
`gradtol=1e-6`, `iterlim=200`, GCC 13.3 on simbox, and five fits per row.
“Before” invokes the unchanged original conditional search kernel directly;
its face-payload reconstruction is excluded. “After” invokes the public
SGED fitter. These shared-machine timings are illustrative, not a general
speed bound.

| Held ν | Before, ms/fit | After, ms/fit | Before log likelihood | After log likelihood |
|---|---:|---:|---:|---:|
| 0.5 | 0.93008 | 0.00690 | −8.75051485 | −5.83627325 |
| 1 | 1.02514 | 0.00605 | −13.42335960 | −13.42335726 |
| 2 | 0.38988 | 0.04811 | −17.12067393 | −17.12067393 |
| 3 | 1.62405 | 1.62674 | −17.15505921 | −17.15505921 |

The last row is an unchanged, uncertified route; its timing difference does
not establish an algorithmic speed improvement. The ν = 2 row stops at a
half-GED face.

The [held-moment benchmark](2026-11-07-sged-skew-data/benchmark-held.cpp) covers
the other three changed normal routes. It uses
`(-3,-1,-0.5,0,0.1,0.2,0.4,1,2,4)`, ν = 2 and the same controls and five-fit
averaging. All six before/after runs return finite fits, with paired total
log likelihoods agreeing within $3\times10^{-14}$.

| Held moments | Before, ms/fit | After, ms/fit | Total log likelihood |
|---|---:|---:|---:|
| mean = 0.2 | 8.31164 | 7.51371 | −19.74121899025 |
| sd = 1.4 | 0.19354 | 1.01106 | −20.24716805955 |
| mean = 0.2, sd = 1.4 | 0.07488 | 0.33173 | −20.27615105787 |

The held-sd routes are slower on this sample; their new searches close the
global comparisons instead of accepting the previous numerical search.
Raw measurements are in [timings.txt](2026-11-07-sged-skew-data/timings.txt).

The final validation commands below finished successfully:

| Check | Machine | Result |
|---|---|---|
| C++ development tier | simbox, GCC 13.3 | 106/106 |
| C++ full tier | simbox, GCC 13.3 | 129/129 |
| No-long-double full suite | simbox, GCC 13.3 | 129/129 |
| No-long-double full suite | workstation, GCC 15.2 | 129/129 |
| R development tier | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21482 |
| R full tier | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44297 |
| JS build and tests | workstation | build green; 489/489 |
| Site check | workstation | green |
| Changed Markdown links | workstation | 2 files; 15 local destinations; 0 broken |

All listed tiers include the final arithmetic and face-conversion guards.
The default golden regressions passed, and no golden, d/p/q/r, truth, GED,
special-function or code-generator source was changed. The site checker regenerated an unrelated screenshot;
it was restored byte-for-byte after checking.

The optional repository-wide Markdown checker reported four unchanged
findings outside this job: an unbuilt `boundary-points.pdf`, a LaTeX expression
misread as a link in the negative-binomial dossier, and two `/inference/`
website links treated as repository paths. Their exact locations are in
[repository-links.txt](2026-11-07-sged-skew-data/repository-links.txt).
The [job-specific checker](2026-11-07-sged-skew-data/check-links.py) restricts
the same parser to the changed SGED dossier and this report. It passed all
15 local destinations across the two files.

[Validation record](2026-11-07-sged-skew-data/validation.txt), including the
final header hash. The retained runners are
[run-cpp.sh](2026-11-07-sged-skew-data/run-cpp.sh) and
[run-final-cpp.sh](2026-11-07-sged-skew-data/run-final-cpp.sh); invoke them
through `/tmp/umvl-cpu/simbox-run bash` from this worktree.

## Open entries

The following reductions identify the missing work. They are not
certificates for the open rows in the scope table. Those rows still use the
legacy search and can return `Ok` or a numerically selected face without a
global certificate. This report does not validate those decisions globally.

**O1 — Both moments free, ν > 1 except two.** The half-scale profile above
remains valid and reduces the problem to minimizing (S(c)) on the compact
sample range; endpoint knots represent the skew faces. Its concavity argument
does not extend to these shapes. Missing: an implemented exhaustive stationary
point/interval comparison for the noninteger-power profile, including knot
joins and public-coordinate enclosures. A single scalar optimizer or a finite
skew scan would not close this entry.

**O2 — Skew free with one or both moments held, ν ≠ 2.** Let
(s=a+b), (z=(b-a)/s\in(-1,1)), (h=v-k^2>0), and (D=3v-4k^2).
The actual moment constraints are

\[
\mu=c+kzs,\qquad
\sigma^2=\frac{s^2}{4}(v+Dz^2),\qquad
\xi=\sqrt{\frac{1+z}{1-z}}.
\]

With sd held, (s(z)=2\sigma/\sqrt{v+Dz^2}). If mean is held too,
(c(z)=\mu-kz s(z)); thus both-moment fits have a compact scalar parameter.
With sd held and mean free, joins at fixed skew are still knots for ν ≤ 1,
leaving a finite collection of scalar skew profiles. With mean held and sd
free, the scale remains free and joins move with it. Fixed-skew inner
convexity for ν ≥ 1, or the existing subunit scale certificate, does not
certify any of these outer profiles. Missing: uniform profile/derivative
bounds and exhaustive comparisons on the skew intervals, with scale tails
also controlled when sd is free.

**O3 — Constrained faces and contact rates within O2.** At the right face,
(a\to0), (b>0), so (\mu=c+kb) and (\sigma^2=hb^2); reflection gives
the left face. With both moments held this fixes
(b=\sigma/\sqrt h), (c=\mu-kb). An observation on the infeasible side
forces likelihood to zero; a strict supported-side gap gives the ordinary
half-GED value. Contact is different. Along the exact held-moment ellipse,
for an observation at the limiting right endpoint,

\[
\frac{|x-c(z)|}{a(z)}\longrightarrow\frac{kv}{2h}.
\]

Each such observation therefore retains penalty ((kv/(2h))^\nu), rather
than the zero kernel penalty of an unconstrained half-GED endpoint. The
reflected face has the same effect. The checker confirms this expansion at
ν = 0.5, 1, 1.7, 2 and 4. Missing: tail envelopes uniform up to both faces,
including exact contacts, nearby supported or infeasible endpoints, and the
permitted approach rates when only one moment is held. The normal delegation
settles this at ν = 2 only. Outward-rounded arithmetic is not a prerequisite;
the missing pieces are the bounds and their implementation under the
package's conditional-comparison standard.

Free-ν uniform and half-GED corners remain outside this job. No GED file was
edited.
