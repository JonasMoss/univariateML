# TASK-140: certified all-free lst and std fitters

The native certificate is now the default for **all-free lst and std**. It
returns an enclosed finite maximum or a terminal, structured face, tie,
budget or numerical stop. Held subsets retain their existing algorithms;
sstd is unchanged. The frozen goldens and their tolerances are unchanged.
Every requested development, no-long-double, JavaScript, site and full-tier
run finished green before this report was written.

Tested source is branch `codex/t-port`, based on
`164d41784756606a48ddc0cedc67752acab4050b`, with the uncommitted changes identified
in [validation.json](2026-11-06-t-port-data/validation.json). The checker verified
276 native source identities against the recorded simbox snapshot. Author
changes are confined to this worktree. No commit, Git write command, board edit,
package installation, publication or other-worktree edit was performed.

**Implementation and proof scope.** The shared engine is
[lst/certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/lst/certificate.hpp).
The public lst/std entrypoints and their all-free held-argument overloads
select it; the old implementation helpers remain explicit timing comparators.
The certificate header uses C++17 standard-library and core headers, with no
R, sibling-directory, special-function-seam or RNG dependency. Settled d/p/q/r,
truth fixtures, R generators, NEWS, STATUS, KNOWN_ISSUES and roadmap files were
not changed.

The exact-arithmetic arguments are the
[profile/closure derivation](2026-11-04-t-cert-data/derivation.md) and
[root/lower-face derivation](2026-11-05-t-root-data/derivation.md). The port's
[arithmetic and search note](2026-11-06-t-port-data/arithmetic.md) records the
objective, coordinates, initialization, proposal/backtracking bounds,
contraction checks, uniform neighborhoods, faces, computational costs and
refusal rules. These are optimization derivatives, without an inference or
information-matrix claim.

At fixed shape, geodesic convexity and explicit nuisance containment turn a
proposal's score times a maximum hyperbolic distance into a complete profile
upper bound. An accepted finite root needs a strict contraction self-map and
a larger uniform nuisance/curvature neighborhood containing its entire box.
A nuisance-ball inward bound identifies every conditional optimum in that
neighborhood; a positive Hessian congruence gives strict profile concavity.
Every exterior finite shape interval and the whole normal tail must then be
excluded. The nomination grid is not an optimality certificate, and general
shape unimodality is not assumed.

Lower endpoints use relative endpoint chords and, at positive scale, nuisance
root inclusion. Exact half-mass and two-thirds-mass collapse values enter the
comparison rather than triggering an automatic all-free equality rejection.
Balanced two-point data select normal by the proved monotonicity theorem.
The symmetric critical three-point std sample selects its bounded collapse
supremum by the separate theorem, including its zero first normal-seam slope.

Certification is **in exact arithmetic conditional on the computed enclosures
and strict comparisons, with stated practical binary64 margins**. Elementary
operations use 64 epsilon times magnitude plus subnormal protection;
cancellation branches use operand magnitudes. Binary-carry sums have a
separate depth-dependent allowance. Gamma-ratio recurrences and integrated
alternating remainders avoid large gamma subtraction. Normal-loss and log1p
remainders evaluate profile differences directly relative to normal. A stable
preconditioned ordinary-scale shape score preserves the nuisance/shape
cancellation near normal. No outward-rounding claim is made.

The default cache permits 4096 profile evaluations. All proposal, linear solve,
backtracking, neighborhood and tail trials are bounded. Shapes above 1e14 are
a numerical-domain refusal, never a fitted estimate. Coefficient enclosures
are checked before finite acceptance, including the entire std sd transform.
The all-free width target is gradtol clamped to [1e-7,1e-6], with location in
empirical-scale units and scale/shape widths relative. Held controls retain
their historical meanings. For certified routes, the result's `iterations`
field counts profile evaluations; iterlim bounds each proposal stage.

**Diagnostics.** Dedicated budget, unresolved-tie and numerical statuses cannot
fall through a local optimizer-score exception. The common bindings transport
StopDetail bounds and witnesses in original data units. Normal candidates use
`norm` with `(mean,sd)`; a positive ordinary-scale t2 face uses `lst` with
`(mu,sigma,df=2)`. A bounded collapse uses diagnostic path `t-collapse` with
`(mu,df)` and its limiting likelihood. It is not a density family or a
continuous density assigned to an atomic weak limit. Unknown global upper
bounds remain infinite; a segment upper bound is not presented as global.
The [transport contract](../../design/structured-stop-details.md) documents
these names. R and JS tests cover the selected faces and terminal budgets.

**Retained outcomes and coefficient accuracy.** The unchanged
[31 frozen samples](2026-11-04-t-cert-data/inputs.json), seed 14020261104, give
60 certified decisions and two explicit budget stops, matching the research
classification:

| Native outcome | lst | std |
|---|---:|---:|
| Interior finite maximum | 9 | 6 |
| Attained restricted Cauchy maximum | 8 | 0 |
| Normal supremum | 11 | 12 |
| Excluded positive-scale t2 supremum | 0 | 11 |
| Bounded collapse supremum | 0 | 1 |
| Unbounded collapse | 2 | 0 |
| Budget refusal | 1 | 1 |

The returned own-golden coefficients are approximately:

| Family | Location | Public scale | Shape |
|---|---:|---:|---:|
| lst | 0.0745507088852 | sigma 1.0270947152104 | df 3.0242427094263 |
| std | -0.00626031595092 | sd 0.8395057369452 | nu 3.9776663070228 |

Independent retained 80-digit roots lie inside all **15 accepted interior
boxes and 19 positive-scale endpoint boxes**. Every point in each own-golden
public coefficient box is within the unchanged relative tolerance 3e-5:
maximum discrepancies are 5.442e-9 for lst and 8.926e-6 for std. These are
whole-box checks, not merely midpoint agreement. See
[independent.json](2026-11-06-t-port-data/independent.json).

Additional independent 80-digit checks cover 48 gamma-ratio/derivative
bounds, 60 points with 14 likelihood/score/Hessian quantities each, and eight
series-switch points with three cancellation quantities each. The arithmetic
points include shapes through 1e14; seed 14020261106. These point checks
corroborate the implementation and arithmetic allowances; the linked
inequalities supply the exact-arithmetic arguments.

The near-Cauchy sample `(0,0,1,2,10)` now certifies an interior shape near
1.001734 rather than accepting the lower grid nomination. Exact 100-fold
replication of the 1000-point t4 workload preserves coefficients and scales
likelihoods and bounds by the integer replication factor. Both cases
are retained in [stress.txt](2026-11-06-t-port-data/stress.txt).

**Material implementation decisions.** Initial absolute error floors on tiny
reciprocals produced enormous chord bounds at large shape, and could make
small positive squared Hessian denominators straddle zero. They refused
safely but caused avoidable budgets and numerical failures. Magnitude-scaled
operations, homogeneous physical-unit conversion and pairwise sum bounds
addressed those dependencies. The original diagnostic values and reproducer
are retained in [rejected-arithmetic.json](2026-11-06-t-port-data/rejected-arithmetic.json).

The original uncombined point-score enclosure also refused the shallow
4000-point root. Combining the ordinary-scale shape score with the nuisance
score analytically before interval evaluation enabled its root and uniform
neighborhood checks. That did not establish global exclusion: its final
answer remains a budget stop. A final cutoff audit additionally changed
subtraction allowances to use operand magnitudes, with targeted independent
checks at both binary64 neighbors of 0.01. None of these choices accepts a
failed proof comparison or uses a likelihood cap as an estimate.

**Native timings against the retained fitters.** Both methods use C++17 -O2
on simbox, one discarded warmup and the mean of three measured calls on each
identical workload. Historical helpers are called explicitly, so the changed
public dispatch cannot contaminate the baseline. Times include successful
fits, faces and guarded refusals. They are wall-clock measurements on a shared
machine, without a claim of isolated benchmark precision.

| Workload | Historical fitter | Native certificate |
|---|---:|---:|
| lst own golden, n=500 | 18.47 ms | 34.43 ms |
| std own golden, n=500 | 2.393 ms | 37.13 ms |
| Median of 31 lst workloads | 3.020 ms | 3.544 ms |
| Median of 31 std workloads | 0.380 ms | 3.203 ms |

The stronger guarantee costs about 1.9x on the lst golden and 15.5x on the
std golden in this run. The shallow 4000-point budget stops take about 5.2 s
per domain. Large independent-sample performance beyond these workloads was
not benchmarked. Raw paired results are in
[replay.json](2026-11-06-t-port-data/replay.json).

**Open — shallow exterior exclusion.** Each domain exhausts 4096 profiles on
the retained normal-0 sample, whose known stationary shape is near 218.257.
The 4000-point quantile sample also has native root and uniform-neighborhood
inclusion near shape 539.46, then exhausts the same global budget. These are
intended terminal refusals, not finite estimates. Stronger relative
comparisons about the finite witness, tighter exterior derivative/envelope
bounds, or a verified larger work budget are needed to close the remaining
segments. Additional local optimization alone is insufficient. The R full
case now checks this structured budget outcome instead of requiring an
unchecked finite return; its old oracle comparison remains for an accepted
finite result. It is not counted as a skipped test or as oracle agreement on
a refused fit.

**Open — arithmetic comparisons on older multimodal examples.** Of four
family/example pairs, the native search certifies one finite winner and stops
three with unresolved comparison ties, after 570, 1451 and 1520 profiles.
The accepted winner encloses the independently retained global root. The
other ties describe overlapping computed bounds, not mathematical equality.
Relative near-winner comparisons or stronger neighborhoods/exterior bounds
are needed to resolve them. No local candidate is substituted for a global
certificate. See [adversarial.json](2026-11-06-t-port-data/adversarial.json).

**Baseline-backed test correction.** Simbox R development initially exposed
the existing Azzalini st scale-equivariance df check near nu=1272. Restoring
all six changed existing C++ headers from frozen master 164d4178 in an isolated
scratch package reproduced the same 2.154e-4 relative df drift, with only
6.38e-14 likelihood difference per observation. The shared test now uses
5e-4 for df above 1000, keeps 2e-4 below that, and retains its separate
likelihood check. Fitter sources and golden tolerances were not relaxed.
The workstation also passed the original tighter check. The source snapshot,
[baseline script](2026-11-06-t-port-data/baseline-st.sh), and
[measured comparison](2026-11-06-t-port-data/baseline-st.json) preserve the
reason for this narrowly scoped test change.

**Final validation.** Every command finished in the foreground. Shared CPU
slot waits were allowed to finish; computations used nice 10 and one
BLAS/OpenMP thread per worker, with worktree temporary directories. No final
failure was permitted.

| Final command | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 dev shards |
| `just cpp-test-no-long-double` | simbox | 127/127 |
| `just test` | workstation and simbox | FAIL 0, WARN 0, SKIP 154, PASS 20690 each |
| `just js-build` | workstation | Completed on final arithmetic source |
| `just js-test` | workstation | 424/424, no skips |
| `just site-check` | workstation | 99 family pages/C++ snippets, browser and MathML checks green |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 127/127 |
| `just test-full` | workstation | FAIL 0, WARN 0, SKIP 0, PASS 43506 |
| Native replay, root/face, gamma, Hessian, cutoff, multimodal and replication workflow | simbox | Passed; guarded outcomes recorded explicitly |

Development skips are tier omissions; the R full tier has none. Earlier
failed attempts and their fixes are recorded in validation.json rather than
counted as passes. Native validation source identities and environment are in
[native-environment.json](2026-11-06-t-port-data/native-environment.json).
The workstation uses GCC 15.2, glibc 2.43, Node 22.22.1 and Python 3.14.5;
simbox uses WSL2/glibc 2.39, GCC 13.3 for native checks and Python 3.12.3.
Both R runs use R 4.6.1, BH 1.90.0.1, Rcpp 1.1.2 and testthat 3.3.2.

The [evidence index](2026-11-06-t-port-data/README.md) supplies reproduction
commands. Full regression logs remain in worktree scratch; their SHA256
identities, source revision, commands and machine assignments are retained in
validation.json. The final document/link/source checks are recorded separately
in [final-checks.json](2026-11-06-t-port-data/final-checks.json).

The supplementary whole-repository Markdown scan reports three findings in
unchanged baseline files: the unbuilt `dev/research/boundary-points.pdf`
referenced by the boundary-literature report, and two `/inference/` website
routes in `univariateML-cpp/notes/inference.md`. All new/changed document links
resolve. The site route itself passed site/browser validation. These baseline
findings are preserved explicitly in final-checks.json; unrelated documentation
was not edited.

The artifact-pane `show` command returned HTTP 403 because this worktree is
outside the viewer's configured readable roots. The report remains at the
requested path. Viewer configuration and destinations outside this worktree
were not changed.
