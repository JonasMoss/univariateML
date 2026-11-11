# TASK-185: stable binomial profile log-pmf

Completed in `codex/ztpois-pmf`, against baseline
`3b2a16fed4787c71b96bf23b1cb86569973b0a9c`. No commits, board changes,
package installs or changes to frozen goldens were made.

The binomial profile now reuses the stable density machinery already used by
d/p/q. Case 629 retains size `1000023`, probability `0.99998160042319029`
and its uncertified flag. Its log likelihood changes from
`-14.363697396130272` to `-14.363696715670791`, against the independent
reference `-14.36369671567082418865494152379055364552868918481136`.
Absolute error falls from approximately `6.805e-7` to `3.31e-14`.

## Implementation and caller inventory

The change is in [ztpois/profile.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/ztpois/profile.hpp).
Integer sizes call the existing `distributions::dbinom(..., true)`. The size
search also evaluates noninteger sizes: these use the same existing stable
`special::beta_log_density_positive` kernel through the exact identity
`binomial_mass(x; r,p) = beta_density(p; x+1,r-x+1)/(r+1)`.
For integer observations this is exactly the gamma extension of the former
falling-factorial formula. It preserves real-size interpolation without
summing a number of terms proportional to the observation.

Every original direct call is covered by the shared helper:

| Consumer | Uses |
| --- | --- |
| `ztbinom/mle.hpp` | Held-probability likelihood; future-size upper bound; free-probability size profile; final likelihood after probability refinement |
| `zib/mle.hpp` | Profiled-inflation likelihood; held-probability future-size upper bound; held-inflation likelihood; final likelihood after precision refinement |
| `zmbinom/mle.hpp` | Positive-data likelihood delegated to `ztbinom_mle` |
| `zmpois/profile.hpp`, kind 3 | Shared modified-binomial route delegated to `ztbinom_mle` |

The inventory also found `zib`'s ordinary-binomial reduction importing a
separate, less accurate likelihood from `binom_mle`. The
[zib change](../../../univariateML-cpp/include/cppunivariate/distributions/zib/mle.hpp)
keeps that delegate's search and coefficients, then recomputes its successful
likelihood with the stable shared helper. For replay case 525, double-only
mode formerly returned `-14.363696713795463`, an absolute error of `1.875e-9`
against an allowance of `1.436e-9`. It now agrees with the reference to
`3.31e-14`, with its certified flag and coefficients unchanged.

The binomial d/p/q route in `ztpois/dpqr_core.hpp` already calls `dbinom` and
needed no change. Other users of the profile header include the truncated and
modified Poisson, geometric, negative-binomial and logarithmic families.
Their Poisson, `nb_logpmf` and logarithmic evaluations concern different laws
and are unchanged. No further independent binomial mass formula was found in
the zero-family profiles. The standalone `binom_mle` implementation remains
unchanged; the adjacent double-only likelihood issue above still needs a
separate correction there. This job prevents `zib` from importing that value.

No search, certificate, work allowance, status enum, message or public fitter
signature changed. No dossier note was needed: the identity and implementation
are derivable from the code, and no new existence or certification result is
claimed. No settled `dpqr.hpp`, `special/`, truth fixture or code generator was
edited.

## Independent numerical evidence

[Provenance](2026-11-11-ztpois-pmf-data/provenance.json) records source hashes,
compiler, modes and reference precision. The
[checker](2026-11-11-ztpois-pmf-data/check.py) uses an independent mpmath
log-gamma formula at 80 digits, converting supplied and returned doubles
exactly from binary64. Every reference is recomputed at 120 digits and checked
to differ by less than `1e-60 * max(1, abs(reference))`.
These are numerical checks, not outward-rounded interval certificates or a
uniform error proof. The beta identity above is exact arithmetic; the existing
fit certificates and their floating-point qualifications are unchanged.

The [grid](2026-11-11-ztpois-pmf-data/grid.json.gz) contains 1,341 points per mode:
698 integer sizes and 643 noninteger interpolations. Integer sizes are
`10^3, 10^4, ..., 10^9`; interpolations add `0.25`. Probabilities include
`1e-12`, `1/size`, `.001`, `.1`, `.5`, `.9`, `1-1/size` and `1-1e-12`.
Observations cover zero, one, two, both sides of the kernel's 16-count seam,
the upper endpoint and its neighbours, and the mean plus/minus one and three
standard deviations. Both native and double-only modes pass. The worst error,
normalized by `max(1, abs(reference))`, is `9.843e-15`, below `1e-10`.
Maximum absolute 80/120-digit reference drift is `3.952e-71`.

The [frozen replay inputs](2026-11-11-ztpois-pmf-data/cases.json.gz) cover all 370
relevant fallback-count cases and all 13 additional relevant acc-fb-count
cases: 157 `zib`, 145 `zmbinom`, and 81 `ztbinom` cases. Data, holds, tolerance
and iteration allowance match the earlier fallback cases. Baseline headers
are rebuilt from the recorded commit, without committing source archives.

[Replay results and independent likelihood checks](2026-11-11-ztpois-pmf-data/replay-checks.json.gz)
and the [summary](2026-11-11-ztpois-pmf-data/summary.json) show, in each mode:

- All 383 outcomes are unchanged: 341 certified successes, 27 uncertified
  successes, ten limit stops and five numerical stops.
- All 341 certified coefficient vectors are unchanged. Certified likelihoods
  are unchanged within rounding or improve toward the independent reference.
  The largest increase in normalized error is `8.961e-16`; the checker allows
  a stated rounding margin of 64 double epsilons, not exact monotonic movement
  of every final bit. Exactly 132 native and 138 double-only certified
  likelihoods are bitwise unchanged.
- All 368 successful likelihoods meet the contract. Worst normalized error is
  `1.192e-12`, at case 1063, unchanged from baseline. Maximum absolute
  reference drift between 80 and 120 digits is `5.295e-70`.
- Coefficients change only in uncertified cases 1053 and 1054, both nearly
  flat held-inflation profiles. Their selected size becomes `13046636065`
  rather than `811159542` and `175100589`, respectively. Independent
  likelihoods increase by about `4.12e-21` and `8.88e-20`; these gains are
  below double resolution and supply no new global-optimality claim.
  Both flags remain uncertified. Case 629's coefficients are unchanged.

## Regressions and final validation

Tests were appended, without modifying or removing existing cases, in
[ztpois/test.cpp](../../../univariateML-cpp/include/cppunivariate/distributions/ztpois/test.cpp),
[zmbinom/test.cpp](../../../univariateML-cpp/include/cppunivariate/distributions/zmbinom/test.cpp)
and [zib/test.cpp](../../../univariateML-cpp/include/cppunivariate/distributions/zib/test.cpp).
They cover billion-size central and endpoint masses, support and probability
endpoints, real-size interpolation, case 629 and the certified binomial delegate.

The [test manifest](2026-11-11-ztpois-pmf-data/test-manifest.json) links retained
compressed logs. Every started test command has finished.

| Check | Machine | Final result |
| --- | --- | --- |
| C++ dev coverage, `just cpp-test` plus affected-family rerun | simbox, GCC 13.3 | All 108 shards covered by successful final checks; see correction below |
| Affected native C++ shards: ztpois, ztbinom, zib, zmbinom | simbox | 4/4 passed |
| `just cpp-test-no-long-double` | Workstation, GCC 15.2 | 135/135 passed |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 135/135 passed |
| `just test` | simbox, R 4.6.1 | FAIL 0, WARN 0, SKIP 154, PASS 25600 |
| `just test-full` | simbox, R 4.6.1 | FAIL 0, WARN 0, SKIP 0, PASS 52358 |
| Independent grid and replay checks | Workstation, GCC 15.2 | Both arithmetic modes passed |
| `git diff --check` | Workstation | Passed |

The first native dev run tested an intermediate implementation that called
public `dbinom` for noninteger sizes. Its integer validation disrupted the
continuous search: three family shards failed, while the other 105 passed.
The correction preserves real-size interpolation through the stable beta
identity. The final native rerun passed all four affected shards; following
the job's late-fix rule, the whole dev tier was not repeated. Both complete
135-shard double-only runs and both R tiers then passed with the final source.

The workstation run waited for its shared four-slot reservation and completed
normally. Simbox remained available; no local fallback was needed. JS and site
checks were not triggered: no shared core, binding, message, code-generator,
dossier or public fitter signature changed.

## Reproduction

From this worktree, with the existing dependencies:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-11-ztpois-pmf-data/build.sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-11-ztpois-pmf-data/check.py
bash dev/research/evaluations/2026-11-11-ztpois-pmf-data/suites.sh
/tmp/umvl-cpu/run4 just cpp-test-no-long-double
```

The suite script runs simbox double-only C++, R dev and R full sequentially,
falling back locally only on exit 75. The native dev run and four-family
correction run are recorded separately in the retained logs. No required
TASK-185 item remains open.

## Artifact pane handoff

All report links were checked and exist. The requested `bin/show` invocation
returned HTTP 403: this worktree is outside the viewer's registered readable
roots. The display helper has no root-registration option. No files were
copied outside the worktree and no shared viewer settings were changed.

Open, display only: the viewer must register
`/home/jonas/Files/programming/univariateML.wt/ztpois-pmf` as a readable root
before it can display this report in the artifact pane. The report and its
retained evidence are complete on disk; numerical and validation work is done.
