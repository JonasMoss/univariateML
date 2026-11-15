# Fit snapshot of master `a1166cbf`: baselines after step 5b (2026-11-15)

Recorded after the four step-5b merges (each family's core decides its own
outcome; reports `2026-11-14-core5b-a` to `-d`). Earlier baselines:
[2026-11-14](2026-11-14-fit-snapshot-report.md).

Files: [native](2026-11-15-fit-snapshot-data/native.jsonl.gz),
[double-only](2026-11-15-fit-snapshot-data/double.jsonl.gz),
[routes, native](2026-11-15-fit-snapshot-data/routes-native.jsonl.gz),
[routes, double-only](2026-11-15-fit-snapshot-data/routes-double.jsonl.gz),
each with its `provenance.json`; 11,905 family and 1,950 route cases per
mode. The snapshots of `a1166cbf` (workstation, GCC 15.2.0) hold the 11,820
and 1,934 cases of the previous baselines. The new class `tie3` (TASK-214.24:
85 family and 16 route cases per mode) was recorded by the strict-rules
worktree at `999f8da4` (families) and `875a2eac` (routes) with
`fit-snapshot.py --classes tie3`, and appended. No file under
`univariateML-cpp/include` or `bindings/common` differs between those commits
and `a1166cbf` except two comments in `core/outcome.hpp`. The provenance
files are those of the `a1166cbf` runs. Every case reports an outcome that
agrees with its emitted kind (`fit-snapshot.py --outcomes`, all four).

## Differences from the 2026-11-14 baselines

[classify-diff.py](2026-11-15-fit-snapshot-data/classify-diff.py), output
[classify-diff.out](2026-11-15-fit-snapshot-data/classify-diff.out). Native
and double-only modes show the same changes.

- **No estimate changed.** No case that is an estimate in both baselines
  returns different coefficients.
- **Outcome kinds** (per mode; each is a decision recorded in the 5b
  reports):
  - Failure to nonexistence, 52 cases: refusals (invalid data; pareto3 numerical) that are proved
    nonexistence. fpareto 18, gb2 16, frechet 3, kumar 2, pareto3 2,
    tlambda 2, and one each for burr, gev, invburr, invtrgamma, pareto2,
    pareto4, sn, sstd and trgamma.
  - Failure to limit, 204 cases: sstd 138, gb2 38, fpareto 28.
  - Failure to estimate: gev 3. Two free fits end at the boundary
    $\xi = -1$, certified; one has $\xi$ held, uncertified.
  - Nonexistence to failure (invalid input), 30 cases: hyper 13 and nhyper 16
    (infeasible held models, 5b-c), and rayleigh `zeros`, whose likelihood is
    identically zero.
  - Limit to failure (evaluation), 2 cases: lomax and pareto2 `subnormal`,
    whose exponential limit rate cannot be represented (5b-c). pareto2's
    message still suggests raising `iterlim`.
  - Failure reasons only, 14 cases: sstd 6 (numerical to unresolved),
    tlambda 7 and st 1 (invalid input to numerical).
- **Log likelihood.** 33 estimate cases per mode moved by more than
  $10^{-12}$ relative with unchanged coefficients. In 5b, the families
  re-evaluate the reported value at the returned coefficients instead of an
  internal profile point (gumbel 5b-a; hcauchy, weibull and others 5b-b).
  - The largest moves are on the near-constant, offset and subnormal classes:
    gamma, weibull and logis up to 4% relative, because rounding the
    coefficients costs likelihood there.
  - Three subnormal cases moved by about one unit. At 50 digits the new
    values equal the sum of the dossier log density at the returned
    coefficients; the old ones were off by 1.0 to 1.25
    ([subnormal-loglik.py](2026-11-15-fit-snapshot-data/subnormal-loglik.py),
    [output](2026-11-15-fit-snapshot-data/subnormal-loglik.out)).
  - The routes move by at most $9.3\cdot10^{-9}$ (pois zero-truncated and
    zero-modified, class `huge`).
- The rest of the 1,228 differing native cases (1,241 double-only, 264 in
  each route snapshot) are payload fields of non-estimate outcomes: limit
  estimates and likelihoods, candidates, messages, reasons and flags. These
  are the changes the 5b reports classify.

## Strict layer

On `a1166cbf` with the snapshots before `tie3` was added, `just strict`
reports no failure outside the known list of that commit (64 entries), and
none listed but passing.

On these stored files, master `3d6652fe` with its known list of 189
entries reports no failure outside the list and none listed but passing:
fit-judge 17, fit-density 0, fit-consistency 80, fit-refine 46,
cert-replay 45, strict-seam 1. The entries added since `a1166cbf` come
from these files:

- certificate replay: TASK-214.31 (40) and TASK-214.32 (5);
- the strict rules: TASK-214.33 (76) and TASK-214.17 (4).

## Suites

On `a1166cbf`, workstation, 2026-11-15:

| suite | result |
|---|---|
| C++ | 110/110 |
| C++, double only | 154/154 |
| R dev tier | 36,354 |
| R full tier | 65,493 |
| JS | 586/586 |
| relations | passed |
| site check | passed |

## Patch after the face fixes (2026-11-15)

The gompertz, pig and cmp records of the native and double-only files were
re-recorded on master `3cc23ca6` (after the face-fixes merge `e691bebe`) and replaced in place (`patched` in each
`provenance.json`). Against the replaced records only the three fixed cases
differ: pig and cmp `class/zeros` now carry `unique = false` in both modes,
and gompertz `class/huge` in double-only mode is the native estimate instead
of a certified exponential limit (TASK-214.32).

`just strict` on the patched files with master's known list of 243 entries
(after the face fixes, the integer and cover replays): fit-judge 16,
fit-density 0, fit-consistency 80, fit-refine 46, cert-replay 100,
strict-seam 1; no failure outside the list and none listed but passing
(cert-replay rerun alone after its first run was terminated by another
process; 1,520 CPU s).

## Patch after step 5c job 1 (2026-11-15)

The records of the 27 renamed families (TASK-150.1.1, merge `14876c7c`) were
replaced with snapshots of that branch (provenance `patches`; recorded on its
worktree before the commit). Against the replaced records only 64 messages per
mode differ, where a message names the control (`reltol` becomes `tol`); no
number, outcome or flag changes
([report](2026-11-15-ctrl-a-report.md)).

## Patch after the margin fixes (2026-11-15)

The binom, gompertz, pareto2 and slash records were replaced with snapshots
of the margin-fixes branch (TASK-214.27, merge `d15aef16`; provenance
`patches`). Only pareto2's 21 stored exponential limits per mode change:
`certified` becomes false and the message notes the likelihood margin
([report](2026-11-15-margin-fixes-report.md)).

## Patch after step 5c job 2a (2026-11-15)

The records of cauchy, gamma, invgamma, lgamma, llogis, logis and naka, and of
the `gamma_mle` callers betapr and genbeta, were replaced with snapshots of
the job 2a branch (TASK-150.1.2, merge `73cff084`; provenance `patches`). The
differences are those of the [job report](2026-11-15-ctrl-b1-report.md): fits
move by up to 4.6e-6 relative where the old absolute step stopped early;
llogis and naka class offset become uncertified; two betapr payloads change.
The known list has 233 entries.

## Patch after the unique-flag fixes (2026-11-15)

The records of hyper, nhyper, triang, pig, cmp and fatigue were replaced with
snapshots of the unique-flags branch (merge `b4ce191f`; provenance
`patches`): 43 cases per mode change `unique` (and hyper warnings), no
coefficient changes ([report](2026-11-15-unique-flags-report.md)). The known
list has 173 entries.
