# Strict layer: the five rules of 2026-11-14 (strict-rules)

Job strict-rules (TASK-214.24), consolidation step 6. Worktree branch
`claude/strict-rules` on master `999f8da4`, which includes step 5b (each
family's C++ core decides its own outcome, `core/outcome.hpp`). The first
draft was judged on the `7e3c9779` baselines; every verdict below was redone
on the post-5b snapshots of the `validate` worktree (commit `a1166cbf`; no
file under `univariateML-cpp/include` or `bindings/common` differs between
`a1166cbf` and `999f8da4`), plus fresh `tie3` snapshots of this worktree. It
implements the five recommendations Jonas adopted on 2026-11-14
([decision](../../roadmap/decisions.md),
[memo](../2026-11-14-strict-rule-questions.md)). Evidence:
[2026-11-14-strict-rules-data/](2026-11-14-strict-rules-data/).

No fit changed. The only core edit is two comments in `core/outcome.hpp`
(section 2). No `mle.hpp`, `certificate.hpp`, binding, golden, `dpqr.hpp` or
truth file was edited. Dossiers changed only by the keys appended at the end
of their `existence` block and rayleigh's `endpoint_log_density` declaration.

## 1. Dossier keys

Five optional keys under `existence`, documented in
[README_SCHEMA.md](../../../univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md)
("Strict-layer keys under `existence`") and validated by
[`existence_keys.py`](../../validation/existence_keys.py), which
`fit-consistency.py` uses to read them and which, run alone, checks every
dossier. Names are the memo's. Each value carries its evidence (`evidence`,
`reason` or `argument`).

| key | content |
|---|---|
| `equivariance` | `group` (`affine`: $A + BX$, $B > 0$; `power-scale`: $AX^B$, $A, B > 0$), `map` (image coefficients as expressions in `A`, `B` and coefficient names; the mapped coefficients must be free), optional `held` (holds the map needs, e.g. `{min: 0}`), `evidence` |
| `collapse` | `at` (`any` value or the sample `minimum`), `exponent` $e(n, k, \theta)$, `infimum` $e^*(n, k)$ over the fitting domain, `attained` (`false`: only a limit attains it), `evidence` |
| `two_point` | `outcome`, `unique` (estimates), `evidence`; requires `equivariance` |
| `min_n` | `value` ($\ge 2$), `reason` |
| `strict_exceptions` | list of `class`, `outcome`, `argument` |

Outcomes are written `estimate`, `limit`, `budget`, `nonexistence(<reason>)`,
`failure(<reason>)`.

**Readers.** The R loader (`load_distributions.R`) passes `existence` through
unvalidated and the generators render only `statement` and `reason`; all 85
dossiers load (checked with `Rscript`, one slot). The site reads `statement`,
`reason` and `uniqueness` only; `just site-check` passes (section 7). The R
package copies the front matter into test fixtures, so the R dev tier was run
as well. No generated R, JS or site file shows the new keys.

### Keys per family

The memo's analysis covers 22 equivariant families (F2), the collapse
families of F3 with frechet and gev (Q1, Q5), the two-point families of Q3
and the near-constant families of Q1.

| family | equivariance | collapse ($e^*$) | two_point | min_n | strict_exceptions |
|---|---|---|---|---|---|
| burr | power-scale (shape2, scale) | | | | |
| cauchy | affine | any, $n-2k$, attained | Em (proved) | | half-ties: N:unattained (proved) |
| emg | affine (rate $\lambda/B$) | | | | |
| expweibull | power-scale (shape, scale) | | | 3 | |
| fpareto | power-scale, `min` held at 0 | | | | |
| frechet | affine | minimum, $n-2k$, attained | N:unattained (proved) | | |
| gb2 | power-scale | | | | |
| gengamma | power-scale ($\mu \mapsto \log A + B\mu$) | | | 3 | |
| gev | affine | minimum, $n-2k$, attained | E, unique (proved) | | |
| huber | affine | | | | |
| invburr, invtrgamma, trgamma | power-scale | | | | |
| lst | affine | any, $n-2k$, attained ($\nu \ge 1$) | | | |
| pareto4 | power-scale, `min` held at 0 | | | | |
| slash | affine | any, $n-2k$, attained | | | |
| sn | affine | | | | |
| sstd | affine | any, $2n-3k$, limit ($\nu > 2$) | | 3 | |
| st | affine | any, $n-2k$, attained ($\nu \ge 1$) | | 3 | |
| std | affine | any, $2n-3k$, limit ($\nu > 2$) | | | |
| tlambda | affine | any, $n-2k$, attained ($\lambda = -1$) | E, unique (numerical) | | |
| triang | affine (all three coefficients) | | | | |
| kumar | | | | | near-constant: F:evaluation (proof + computation) |
| gompertz | | | | | near-constant: F:evaluation (proof + computation) |
| sgomp | | | | | near-constant: F:evaluation (numerical) |

Step 5b changed no declared key: each `two_point`, `strict_exceptions`
and `collapse` entry still names an outcome the core can or cannot reach as
before (frechet and gev now end in their declared `N:unattained` and `E`,
cauchy in `Em`; see section 5). `existence_keys.py` validates all 85 dossiers
and the R loader (`load_distributions`) reads all 85, including rayleigh's new
declaration (`endpoint_log_density$lower = -Inf`).

### Evidence

**Equivariance (all 22 checked).**
[`equivariance.py`](../../validation/equivariance.py) checks the density
identity $\log f(T(x); \varphi(\theta)) + \log T'(x) = \log f(x; \theta)$ at 24
seeded points $(\theta, A, B, x)$: on the dossier's `log_density` at 40 digits
and on `dpqr.hpp` in binary64, and symbolically where SymPy can. It also
checks that the image coefficients stay in the parameter domain and that no
mapped coefficient has a fit-domain restriction (so the map, a group action,
is a bijection of the fitting domain).
[Results](2026-11-14-strict-rules-data/equivariance.txt): all 22 pass; the
largest dossier difference is $1.6 \times 10^{-40}$ relative, the largest C++
difference $3.5 \times 10^{-15}$ (gev). SymPy proves the identity for 11
(cauchy, emg, frechet, gengamma, gev, huber, lst, slash, sn, st, std); for
burr, expweibull, fpareto, gb2, invburr, invtrgamma, pareto4, sstd, trgamma
and triang it did not simplify within its 20-second guard, also with the data
declared positive and logs of powers expanded, so their evidence is
numerical. tlambda's density is implicit in $u = F(x)$; its map is read off
the quantile function $\mu + \sigma Q_\lambda(u)$ and checked on the C++
density only. A [negative control](2026-11-14-strict-rules-data/equivariance-negative-control.txt)
perturbs one map entry per family by $10^{-3}$: all 22 are detected by both
densities. `fit-consistency.py` repeats the binary64 check on every run (rule
`equivariance`).

**Collapse.** Every exponent is stated in its dossier: cauchy and slash
($\sigma^{n-2k}$), lst, st, std and sstd ($\nu(n-k)-k$), frechet
($\lambda(n-k)-k$ at the minimum) and gev ($(n-k)/\xi - k$ at the minimum).
The infimum follows from the fitting domain: $\nu \ge 1$, $\lambda \ge 1$ and
$\xi \le 1$ attain $n-2k$; $\nu > 2$ gives $2n-3k$ only as a limit. tlambda:
its dossier bounds the likelihood near a collapsing location by a constant
times $\sigma^{n-2k}$ on the branch $-1 \le \lambda \le 1$, and at
$\lambda = -1$ the density $1/(u^{-2} + (1-u)^{-2})$ has tails of order
$x^{-2}$, so the bound is attained (the memo's sketch; elementary). Members
with $\lambda \ge 0$ have exponential or bounded tails and do not collapse on
nonconstant data.

**Two observations.** gev: the dossier proves the unique maximum $\xi = -1$,
$\sigma = pd$, $\mu = M - pd$ for two-point samples with minimum fraction
$p \le 1/2$. frechet: two observations are an exact half-minimum tie, for
which the dossier's pair bound proves a finite unattained supremum. cauchy:
the dossier proves that two values occurring equally often attain a maximum
with a continuum of maximizing pairs (so `unique = false`). tlambda
([script](2026-11-14-strict-rules-data/tlambda_two_point.py),
[output](2026-11-14-strict-rules-data/tlambda_two_point.txt)), numerical,
not a proof: with $d = x_2 - x_1$, $d^2 L = h(\lambda, u_1, u_2) =
(Q(u_2) - Q(u_1))^2 / (Q'(u_1) Q'(u_2))$ depends only on the shape and the two
quantile levels. Its supremum over placements (Nelder-Mead from 64 starts in
logit coordinates) is below one at 408 shapes in $[-1, 1)$; the largest is
$1 - 1.9 \times 10^{-7}$ at $\lambda = 1 - 10^{-8}$, approaching one like
$(1-\lambda)\log(1/(1-\lambda))$, which matches a first-order expansion. At
$\lambda = 1$, $h = (u_2 - u_1)^2$ reaches one only at the uniform on
$[x_1, x_2]$. Hence the declared outcome: a unique estimate at
$\lambda = 1$, $\mu = (x_1 + x_2)/2$, $\sigma = d/2$, attained in the closed
fitted branch.

**Near-constant maximizers outside binary64.**
[Script](2026-11-14-strict-rules-data/near_constant_maximizers.py),
[output](2026-11-14-strict-rules-data/near_constant_maximizers.txt), 80 digits
on the snapshot's exact inputs $[c, c, c^+]$:

- kumar: the dossier proves a unique MLE with $\hat b = -n/S(\hat a)$; the
  profile maximum is $\hat a = 1.633 \times 10^{16}$ with
  $\hat b \approx 10^{6.0 \times 10^{15}}$.
- gompertz: the dossier proves a unique MLE when
  $\overline{x^2} < 2\bar x^2$ (true here); $\hat b = 1.906 \times 10^{16}$ and
  $\hat a \approx 10^{-4.35 \times 10^{15}}$, below the smallest positive
  double.
- sgomp (numerical): the dossier proves existence only. The rate profile
  (the shape is unique for each held rate, by the dossier) on 220 rates over
  55 decades, refined at its peak, puts the maximum at
  $b = 1.438 \times 10^{16}$ with $\eta \approx 10^{1.08 \times 10^{16}}$; no
  other grid region comes within 12 log-likelihood units.

All three settle the memo's "kumar now; gompertz, sgomp once checked":
`failure(evaluation)` ("an unrepresentable result") is the honest outcome.

**min_n.** Declared 3 for the four equivariant families whose two-point
outcome is not settled (expweibull, gengamma, st, sstd), as the memo's Q3
rule prescribes "until declared". The reason names the open question.

## 2. Rules (`fit-consistency.py`)

The existing rules stand. Changes and additions, each keyed
`phenomenon:<rule>|mode|family|class|<label>`:

| rule | checks |
|---|---|
| `valid-data` (extended) | valid observations never end in `unsupported`, except for $2 \le n <$ `min_n` |
| `outside-support` (extended) | observations on a fixed closed bound whose endpoint log density is $-\infty$ for every free coefficient end in `invalid-input`; the declared `endpoint_log_density`, or, where none is declared, the SymPy limit of the dossier's `log_density` |
| `near-constant` | (continuous) `[c, c, c+]` never ends in `no-convergence`, `evaluation`, `invalid-input`, `invalid-argument` or `unsupported`; a refusal already listed under `valid-data` is not repeated |
| `near-constant-tie3` | (equivariance applies under the golden holds) near-constant ends in the kind and reason of its exact preimage `tie3`, or in `conditioning` |
| `two-point` | (equivariance applies) `two` ends in the declared `two_point` outcome (and uniqueness); undeclared with three or more free coefficients: `unsupported` below a declared `min_n`, otherwise never a numerical failure or budget |
| `tie-collapse` | (continuous, valid, nonconstant classes; free location and scale) with `collapse`: $e^*(n, k) < 0$ requires `nonexistence(unbounded)`, $e^* > 0$ forbids it, $e^* = 0$ forbids refusals; without it and without a spike restriction: never `nonexistence(unbounded)` |
| `equivariance` | the declared map against the C++ density (once per family and mode) |

A `strict_exceptions` entry replaces its class's default verdict: the fit must
end in exactly the declared outcome (reported under the rule of the class).
The table now has the columns `half-ties` and `tie3`.

**Q4 declaration.** rayleigh now declares
`endpoint_log_density: {lower: "-Inf"}` in its `support` line, as the memo
has it, and the rule reads the declaration. Where no endpoint density is
declared the rule still falls back to the SymPy limit of the dossier's
`log_density`; among the families with an undeclared fixed closed endpoint
(exp, expweibull, foldnorm, gompertz, sgomp) none has a limit of $-\infty$
independent of the free coefficients. rayleigh passes `outside-support` on
the post-5b snapshots (its `zeros` class now ends `F:invalid-input`).

**`core/outcome.hpp`.** Two comments widened, no code:
`InvalidInput, // Failure: empty, non-finite or out-of-support data, data with zero density under every admissible coefficient vector, or invalid held values` and
`Unsupported, // Failure: a hold subset, or a sample below the documented minimum size, the family does not implement`.

## 3. Snapshot class `tie3`

`fit-snapshot.py` gains `tie3 = [q50, q50, q75]` (the preimage of
near-constant), appended after every existing class so no earlier case
changes, and an option `--classes c,d` that records only those class cases
(its part files carry the class list in their tag; a full run never reuses
them). `half-ties` (`[q50]*3 + [q10, q25, q75]`) was already a snapshot class
and is now tabulated by `fit-consistency.py`.

**No existing case changed.** The case list is rebuilt with the edited script
and its input hashes compared with the stored post-5b snapshots of `validate`
([script](2026-11-14-strict-rules-data/hashcheck.py), no fitting): native
families 11,820 of 11,820 stored cases have an identical hash, native routes
1,934 of 1,934, and the rebuild has 85 and 16 new cases, all of class `tie3`
(101 in all; 11,905 and 1,950 in total). The double snapshots use the same
case list.

Only the `tie3` cases were fitted for this worktree, in both modes
(`--classes tie3`, one case in each of the 85 families per mode; the routes' 16 `tie3`
cases were not recorded, so route rows show `-` in the `tie3` column of the
[consistency table](2026-11-14-strict-rules-data/consistency-table.md)).
`fit-consistency.py` judges the `validate` snapshots together with the two
`tie3` directories. Double mode (`--no-long-double`) uses the same dossiers
and rules; the failures are the same in both modes: all 80 reviewed failures
occur in native and in double (40 per mode, the same 40 rule, family and class cells), none in only one.

## 4. New strict entries

`fit-consistency.py` with an empty known list reports 80 failures (40 per
mode): `near-constant` 42, `near-constant-tie3` 24, `tie-collapse` 4,
`two-point` 10. Every one was reviewed as a real phenomenon (the core ends in
an outcome the rule's evidence excludes); no rule bug was found on the
post-5b data. [`add_entries.py`](2026-11-14-strict-rules-data/add_entries.py)
adds them to `dev/validation/strict-known.json` (80 entries, no existing entry
changed; the file's `note` text differs from master's only through
`strict_common.py`'s own constant). Groups (count = entries, native and
double together):

| card | finding | entries | families | example |
|---|---|---|---|---|
| TASK-214.24 | `near-constant-equivariant-numerical-failure` | 26 | burr, emg, expweibull, fpareto, gengamma, huber, invburr, lst, pareto4, sn, std, trgamma, triang | burr: near-constant ends in `F:evaluation` |
| TASK-214.24 | `near-constant-numerical-failure-unsettled` | 10 | betapr, fatigue, genbeta, invparalogis, paralogis | betapr: `F:evaluation` |
| TASK-214.24 | `near-constant-unrepresentable-maximizer` | 6 | gompertz, kumar, sgomp | kumar: the dossier proves `F:evaluation`; the core ends `F:no-convergence` |
| TASK-214.24 | `near-constant-differs-from-preimage` | 24 | burr, emg, fpareto, gengamma, huber, invburr, lst, pareto4, sn, std, trgamma, triang | burr: near-constant `F:evaluation`, its exact preimage `tie3` ends `L` |
| TASK-214.24 | `collapse-not-reported` | 2 | lst | near-constant ($n = 3$, $k = 2$, $e^* = -1$) requires `N:unbounded`; ends `F:evaluation` |
| TASK-214.17 | `tlambda-collapse-and-two-point-evaluation-failure` | 4 | tlambda | `ties` ($n = 6$, $k = 4$, $e^* = -2$) requires `N:unbounded`; ends `F:evaluation`. `two`: declared `E`, ends `F:evaluation` |
| TASK-214.24 | `two-point-undecided-below-min-n` | 8 | expweibull, gengamma, sstd, st | `two` below the declared `min_n` 3 ends `F:no-convergence` (sstd: `Lm`), not `F:unsupported` |

Reasons of the near-constant failures are numerical (`evaluation`,
`no-convergence`); the 13 equivariant families fail on a problem equivalent
to a well-scaled three-point one (memo Q1), which is why
`near-constant-differs-from-preimage` lists the same families except
expweibull (12 of 13). Hypothesis, not checked per case: expweibull's `tie3`
ends in the same failure as its near-constant sample, so the rule is met. The `tlambda` entries keep card TASK-214.17 as in the
draft; its finding is renamed because the refusals as `invalid-input` are gone
(section 5).

## 5. Movement since the 7e3c9779 draft

The draft listed 106 entries on the `7e3c9779` baselines; the post-5b
snapshots give 80. No failure is new: 26 entries disappeared (comparison by
key; `add_entries.py` was updated, its groups for these were dropped).
The comparison of the two consistency tables (103 changed cells, the rest are
route and master changes outside these rules) shows what 5b did to the
classes the rules read:

| family | class | draft (7e3c9779) | now | entries gone |
|---|---|---|---|---|
| gev | `one`, `two` | `F:invalid-input` | `N:unbounded`, `E` (the declared `two_point`) | 2 (two-point) |
| frechet | `one`, `two` | `F:invalid-input` | `N:unbounded`, `N:unattained` (declared) | 2 (two-point) |
| cauchy | `two`, `half-ties` | `E`, `N:unbounded` | `Em` (declared, not unique), `N:unattained` (declared) | 4 (two-point, tie-collapse) |
| gb2 | `one`, `constant`, `ties`, `half-ties`, `near-constant` | `F:invalid-input` and `F:unsupported` | `N:unbounded` and `Lm` | 6 valid-data, 2 near-constant-tie3 |
| rayleigh | `zeros` | `N:unattained` | `F:invalid-input` | 2 (outside-support) |
| tlambda | `one`, `tie3`, `near-constant` | `F:invalid-input` | `N:unbounded` | 6 (tie-collapse near-constant and tie3, valid-data) |
| tlambda | `ties`, `two` | `F:invalid-input` | `F:evaluation` | none: still failing, under a new reason |
| sstd | `near-constant`, `tie3` | `F:no-convergence` | `F:unresolved` (allowed by the rule) | 2 (near-constant) |

`one` is `N:unbounded` now in burr, fpareto, frechet, gb2, gev, invburr,
invtrgamma, kumar, pareto3, pareto4, sn, sstd, tlambda and trgamma (draft:
`F:invalid-input`, pareto3 `F:evaluation`), so Q2's `one` requirement holds
everywhere; the `valid-data` and `one-constant` rules no longer list a
failure. Other changes outside these rules: `invalid-argument` became
`invalid-input` for invalid observations in about ten families, and in the
zero-modified routes `zeros` became `Em`. Unchanged: the other 42 near-constant
entries (21 families; the 5b core did not fix them), lst's
`tie-collapse`, and the expweibull, gengamma, sstd and st `two` outcomes.

## 6. Open

- The 80 listed entries are failures to fix, not accepted behaviour: 42
  near-constant numerical failures (formulation defects in the 13
  equivariant families; kumar, gompertz and sgomp should end
  `failure(evaluation)` and end `no-convergence`), lst and tlambda
  `N:unbounded` on tie data, and tlambda's two-point outcome (declared `E`,
  numerical evidence only). Not investigated why tlambda ends
  `F:evaluation` on `ties` and `two` after 5b.
- `min_n` of expweibull, gengamma, sstd and st (3) stays until their
  two-point outcomes are decided by proof or high-precision computation
  (memo Q3); sstd now ends `Lm` on `two`, a mathematical outcome the rule
  still lists because it is not declared.
- The routes' `tie3` cases were not recorded, so the `near-constant-tie3`
  rule has no input for the route families (zero-modified and truncated
  variants); they are judged by `near-constant` only.
- Step 5b's outcome mapping was not re-derived per family in the C++; the
  declared keys were checked by running the rules on the snapshots, not by
  reading each `outcome(...)`.

## 7. Runs

Workstation, GCC 15.2.0, `nice -n 10`, BLAS and OpenMP pinned to one thread,
one run at a time through `/tmp/umvl-cpu`. Wall times; the CPU time of the two
snapshot runs was not captured (`time` output lost), at most four cores each.

```
cd univariateML.wt/strict-rules
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run4 nice -n 10 python3 dev/validation/fit-snapshot.py --output build/snapshots/tie3-native --classes tie3 --jobs 4            # 191 s wall
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run4 nice -n 10 python3 dev/validation/fit-snapshot.py --output build/snapshots/tie3-double --classes tie3 --jobs 4 --no-long-double   # 166 s wall
python3 dev/research/evaluations/2026-11-14-strict-rules-data/hashcheck.py $V      # 3 s
V=../validate/build/snapshots; S=build/snapshots
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 dev/validation/fit-consistency.py --known /tmp/sr-none.json \
  $V/native,$V/routes-native,$S/tie3-native $V/double,$V/routes-double,$S/tie3-double      # 80 new, exit 1; 41.5 s user
python3 dev/research/evaluations/2026-11-14-strict-rules-data/add_entries.py <same arguments>      # adds 80 entries
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 dev/validation/fit-consistency.py <same arguments> [--table ...]   # 80 known, 0 new, exit 0; 44.5 s user
python3 dev/validation/existence_keys.py                 # all 85 dossiers
Rscript -e 'source("bindings/r/scripts/load_distributions.R"); load_distributions("univariateML-cpp/include/cppunivariate/distributions")'   # 85 loaded
python3 dev/tools/check_links.py
```

(`$V`, `$S` as written for the second and later lines. `sr-none.json` is
`{"known": {}}`. `--prune` was never used. The earlier runs of the draft, the
`equivariance.py` and negative-control checks, the R dev tier and
`just site-check`, were not repeated; only rayleigh's one-line dossier change
and `outcome.hpp` comments are new since.)

## 8. Files

- `univariateML-cpp/include/cppunivariate/core/outcome.hpp` (two comments).
- 27 family dossiers (`distributions/<id>/README.md`): burr, cauchy, emg,
  expweibull, fpareto, frechet, gb2, gengamma, gev, gompertz, huber, invburr,
  invtrgamma, kumar, lst, pareto4, sgomp, slash, sn, sstd, st, std, tlambda,
  trgamma, triang (keys under `existence`), and rayleigh (`support` line).
  `distributions/README_SCHEMA.md`.
- `dev/validation/`: `fit-consistency.py`, `fit-snapshot.py`, `README.md`,
  `strict-known.json` (80 entries added), new `equivariance.py`,
  `existence_keys.py`.
- `dev/research/evaluations/2026-11-14-strict-rules-report.md` (this file) and
  `2026-11-14-strict-rules-data/`: `add_entries.py` (updated), `hashcheck.py`
  (new), `consistency-table.md` (regenerated on the post-5b snapshots), the
  equivariance, near-constant and tlambda two-point scripts and outputs.
- Not committed: `build/snapshots/tie3-native`, `tie3-double` (gitignored).
