# TASK-79 p0-locscale: analysis and remaining work

**Incomplete.** All proper hold subsets are enumerated, but some tlambda,
Student, GED, Huber and Bhattacharjee rows still lack a proved exact
held-value/boundary criterion. The uniform and shape-face comparisons are
numerical in some cases; an optimizer cannot certify a global supremum or
settle equality. Therefore the candidate YAML is provisional, the R7 domain
audit is not exhaustive, and this is not yet the complete M1 oracle requested.
The note identifies the unresolved rows and conditions.

Proved from the densities: collapse exponents for lst and tlambda, including
ties at a held location; std's `nu>2` is a natural domain and its t2 face
requires the reported sd to diverge when the ordinary t scale remains finite;
GED has an unrestricted spike. For GED with sd held, the spike diverges if
k/n > 0.1379185 where k observations equal the mean: for distinct samples
with free mean this includes every n=2 through 7. Thus `while_free: [mean,
sd]` would be wrong. Boundary paths were also checked numerically.

R7 surprises: `extraDistr::dbhatt` accepts `a=0` as the normal member, which
the README and fitter reject. `stats::dnorm` accepts `sd=0` as a degenerate
case; whether its Inf/zero density values count as an attained R7 member
needs a likelihood convention. lst and GED reference densities admit shapes
below their fitting bounds; the owned tlambda density accepts all finite
shapes. Tumlinson (2015), Theorem 1, proves the free GED spike.

Numerical sweep: 840 sample/hold cases, 264 finite master fits, 490 unsupported
holds, 86 errors or reported escapes. Every finite fit matched an independent
oracle to `1e-6*n`: 262 by the reference-density search, two Bhattacharjee
cases by a separate stable CDF-difference calculation after the external
reference lost tail precision. Own std, ged and tlambda densities also matched
independent README-formula checks on the stated grids. Held values were not
exhaustively varied; numerical agreement does not prove boundary existence.

M4: all 1,200 seeded replicates completed. Free-epsilon normal-face escapes
were 60%, 72%, 64% at n=20, 100, 1000; Cauchy escapes were 4%, 10%, 62%
(normal plus Laplace). Independent optimization/face comparison agreed in
all cells after profiling ten near-Laplace cases. The high normal escape rate
argues against an unconditional free-epsilon default under D1, but R3 makes
changing that default a policy decision.

Outputs: `dev/research/evaluations/2026-10-29-p0-locscale.md` and scripts/results in
`dev/research/evaluations/2026-10-29-p0-locscale/`. No package code, READMEs, tests,
goldens, board edits, commits, or Git writes were made.
