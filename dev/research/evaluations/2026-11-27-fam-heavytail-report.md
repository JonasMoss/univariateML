# TASK-385 · Heavy tails and skew

Base `4a014db6`. The changes cover `lst`, `std`, `sstd`, `sged`,
`st`, and `tlambda`. The atlas and the generated site capability table
reflect the affected routes. All changes are in the job worktree.

## TASK-257 · Joint Student holds

A checked finite proposal with one nuisance coefficient held had no
general route to a certified joint result. The new cover brackets the
monotone conditional scale score over each shape interval when location
is held. With ordinary scale or standard deviation held, it covers
location and shape jointly and uses the derivatives for the actual hold.
It certifies a finite proposal only after excluding every competing cell
and eligible tail, and enclosing the root to the requested accuracy in
public units. The held-standard-deviation cover uses a small-shape bound
from the gaps between sample order statistics. A conditional positive-scale
$t_2$ face remains eligible when standard deviation is free.

For exact sample reflection about the held location, the fixed-shape
nuisance theorem puts the conditional optimum at that center. The finite
all-free certificate transfers to that hold. In `lst`, the extra interval
below df one is covered using a monotone scale tube and the uniform
$B(\nu)\le\nu/2$ bound near zero. The general held-location finite cover
is used for separated positive normal-face coefficients. Unclosed
comparisons retain converged, locally checked results with
`certified=false`. [Written cover and inclusion proof](2026-11-25-tcert-math-report.md#cover-and-inclusion)
· [Lean fixed-shape scope](2026-11-27-lean-tdist-report.md#scope-and-proof).

Regressions exercise the reflected shallow maximum (df about 539.46),
an asymmetric held-location sample (df about 3.63), and interior
location–shape fits with ordinary scale or standard deviation held
(df about 2.99 and 3.23). The [exact score replay](2026-11-27-fam-heavytail-data/check_two_modes.py)
asserts the rational signs $+,-,-,+,-$ at df $4,6,20,50,100$ using the
weighted sample from TASK-252. The signs prove two separated interior
shape maxima. [Exact sample and proof](2026-11-25-tcert-math-report.md#an-exact-sample-with-two-interior-shape-maxima).

**Changed expectations.** The native and double-only regression at
`tol=1.220703e-4` now certifies both joint `lst` holds and held mean in
`std`. The held-sd `std` regression still returns an uncertified
estimate. The R and JS held-golden assertions at their default tolerance
have the same certification pattern. The held-scale `lst` fixture 33000
now certifies its interior df near 1.0001. These corrections retain the
original coefficient, likelihood, hold, and restriction checks. No
golden file changed.

## TASK-270 · Critical collapse and SGED width

**SSTD.** At critical multiplicity with only scale free, cancellation of
the common scale power gives a piecewise rational score. A single
off-mean site allows a quadratic comparison. For several sites,
signed end collars and root enclosures compare the finite candidates
with the sharp collapse value. A critical extreme atom with shape
held, free mean and spread, and the broad side toward the remaining
observations has a separate symmetric-density bound. When mean, sd and
skew are held and shape is free, the numerical shape search must beat
the sharp $t_2$ collapse value. The conditional skew-normal remains a
competitor in that search.

On $(0,0,0,1)$ with mean zero and df three, skew 2 and skew 10 give
certified unattained collapse, while skew 0.1 gives a certified
positive-scale winner. The skew-2 pin changed from a numerical stop
to nonexistence in C++ and R. A mixed-side example with six zeros,
$-1$, and $2$ exercises stationary root comparison across the join.
With mean zero, sd 0.01, skew 0.1, and free shape, $(0,0,1)$ gives a
locally checked finite fit above the collapse value and retains the
uncertified flag. [Collapse value and selection proof](2026-11-25-sstdsged-math-report.md#which-model-wins).

**SGED.** Rounded equality previously selected the unresolved
`ScaleBoundary` guard. The actual squared width now compares exact
dyadic integers. With free mean, compare the squared range with
$12\sigma^2$. With held mean, compare the squared endpoint residuals
with $3\sigma^2$. Exact contact is unreachable for positive binary64
sd. Exterior data give zero uniform limiting likelihood. Strictly
interior data admit a finite shape above the uniform height, which
also supplies a lower bound for rejecting a worse finite or half-face
nomination. The tests cover successive floats around $\sqrt 3$,
including the formerly rounded contact, and compare subnormal and
overflowing physical ranges. A contact-count `UniformBoundary` result
would require exact contact in the public input representation.
[Width comparison and contact likelihood](2026-11-25-sstdsged-math-report.md#2-sged-held-width-and-a-free-mean)
· [Both-moment tail proof](2026-11-25-sstdsged-math-report.md#additional-signed-shape-tails).

## TASK-259 · Tukey lambda half ties

The scale lower bound is the maximum of the old cutoff and the new
disjoint-pair bound with computation margins. The compact comparison
uses the extreme-pair scale upper bound and density bounds over
location, log-scale and shape. Strict exclusion at the collapse level
gives a certified `CollapseLimit`. When exclusion fails, the existing
unresolved outcome retains its collapse and uniform witnesses.

For $(-1,0,0,1)$ the regression pins the compact log-scale width between
11 and 12, down from about 34.43. For $(0,0,10^{-20},1)$ the scale bounds
leave no eligible scale and the fitter now proves collapse instead of
returning an unresolved comparison. [Cutoff proof and sample widths](2026-11-25-tlroot-math-report.md#sample-dependent-cutoff)
· [Compact comparison](2026-11-25-tl-math-report.md#compact-comparison-and-examples).

## Certified-outcome audit

In `st`, `ScaleBoundary` uses both the proof reason and its certification
flag to declare nonattainment. Generic critical equality reports
`Unresolved`, with the exponent sign obtained by a fused comparison.
The regression uses mixed-sign residuals. The symmetric Student
delegation and the held-location, held-df one-sign reductions retain
their proved outcomes.

The SSTD generic equality paths remain distinct from the proved
scale-only, extreme-atom, and symmetric delegation paths. The SGED
dossier distinguishes its balanced free-moment uniform result and its
conditional half-GED covers from numerical comparisons. The atlas
carries those scopes along with the revised skew-t detector entries.
[Dossier proof audit](2026-11-27-dossier-proofs-report.md#runtime-follow-ups).

The reported `lst` symmetry concern was checked at both call sites.
The helper itself rejects a nonpositive largest-atom exponent, including
when the numerical search supplies a witness. At df 0.5, $(-1,0,1)$
therefore has no certified symmetry result. $(0,1,1)$ gives strict
divergence at the largest atom. Both cases have regression coverage.
[Lean report and scope](2026-11-27-lean-tdist-report.md#dossier-findings).

## Open comparisons

- The Student covers can leave near-zero normal slopes, critical
  free-shape collapse collars, degenerate stationary points, or
  overlapping maxima unresolved. The work needed for those cases is
  a signed bound on the corresponding tail or a separate identity
  for equal critical values.
- General SSTD free-shape criticality with other holds still needs
  sharp joint skew, normal, and half-face comparisons. A finite
  critical value that exactly ties collapse needs an equality proof.
  Overlapping scale-root value bounds alone leave that comparison open.
- The SGED width test decides which strict order applies. Close
  interior contacts may require shapes outside practical search
  resolution. A general finite and moving half-face cover with free
  shape and skew remains open.
- The Tukey lambda cover certifies strict exclusion. A vanishing gap
  at the collapse level or a finite winner above it still needs a
  separate certified maximizer comparison.

## Verification

All final runs used the workstation. Simbox did not respond on SSH.

| Check | Result |
| --- | --- |
| `just test-changed --no-long-double`, selected C++ tests across 14 families | 28/28 native and 28/28 double-only |
| Changed-family R run before correcting the three golden certification assertions | 5,097 passed, 33 tier skips, no warnings, and the three stale assertions |
| `UMVL_TEST_FAMILIES=lst,std just test` after those corrections | 835 passed, no failures or warnings, 13 tier skips |
| `just js-build` and `just js-test` | 601/601 |
| `just cpp-test` | 136/136 |
| `just site-check` | Passed, including 86 generated C++17 examples |
| `existence_atlas.py --check`, `existence_keys.py`, exact score replay, and `git diff --check` | Passed |

The repository-wide Markdown link scan also flagged the unchanged
`P+Q` expression on line 461 of the beta-prime math report. No shared
core, special-function, or code-generator files changed. No Git writes
or board edits were made.
