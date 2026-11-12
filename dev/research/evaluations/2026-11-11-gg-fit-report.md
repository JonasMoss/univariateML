# TASK-177: Prentice generalized gamma fitting

The fitter, R and common binding, dossier, golden, and generated JavaScript registrations are in this worktree, based on `bcea25d2`. The parameters are Prentice's $(\mu,\sigma,Q)$, with $\sigma>0$ and signed $Q$. The lognormal at $Q=0$ is an attained ordinary member. A successful fit carries a `certified` flag; a successful locally checked candidate is retained when the global certificate cannot close. Unbounded concentration and strictly winning unattained power and Pareto limits have distinct statuses and wrapper diagnostics.

## Likelihood and certificates

The reduction, held-coordinate proofs, inference calculation, and their limits are in the [theory report](2026-11-10-gg-theory-report.md). For nonconstant log data $y_i=\log x_i$, standardize to $u_i=(y_i-\bar y)/s$, let $K(t)=\log\overline{e^{tu}}$, and solve $\log a-\psi(a)=K(t)$. The two signs of $t$ cover the two transformed-gamma interiors; their common zero point is the fitted normal likelihood of $y$. The signed profile satisfies $\Delta'(0)=-\kappa_3/6$. The implementation uses the existing transformed-gamma profile and bounds, together with a smooth signed-shape score and local observed-information refinement. It does not use the parent's $10^{-8}$ likelihood-tie rule to round a nonzero $Q$ to zero.

For a strictly separated all-free finite candidate, the two parent side covers can certify a matching winner and exclude the other side. For an all-free power or Pareto stop, the certificate compares the faces and bounds both signed profiles: the parent seam and tail bounds cover the ends, while monotonicity of the inverse-digamma shape and $K'$ bounds each finite cell's derivative. A tangent residual allowance accounts for the inexact inner shape. An additional sufficient $Q=0$ certificate first proves exact multiplicative symmetry of the binary64 observations using error-free product pairs, then excludes both sides; merely small numerical skew does not trigger it. The symmetry and side exclusions are sufficient conditions, not a claim that zero skewness alone proves a maximum.

These certificates are **exact-arithmetic arguments conditional on the computed comparisons**, not outward-rounded interval arithmetic. Their practical binary64 allowance includes, for example, $256\epsilon(1+n)+16e_y/d$ in face comparisons, where $e_y$ bounds the log-input rounding allowance and $d$ is the smaller relevant log-data scale; finite-candidate side comparisons use $512\epsilon(1+n)+16e_y/d$. The individual bounds and propagated terms are in `face_certificate.hpp`, `free_certificate.hpp`, and `local.hpp`. The local acceptance check uses positive observed information and contracting Newton corrections with coefficient floors $s_Q=1$, $s_\mu=\sigma$, and relative scale accuracy. These floating-point margins are practical, not a proof of uniform rounding-error bounds.

With $Q$ held, the nonconstant-data conditional maximum is unique by strict concavity; the one-coordinate holds and direct all-held evaluation use the corresponding conditional equations. With $\sigma$ held, a finite global maximum exists; its general uniqueness and a global finite-region certificate remain open. The joint corners with $\mu$ held remain open. For those open comparisons, a locally verified candidate is returned uncertified rather than refused merely because a certificate did not close. Search or coefficient-accuracy failure remains a numerical failure. Equal positive observations with free location and scale are unbounded. The held-location concentration stop is used only when equality can be established; rounded log ties are not treated as proof.

The per-observation information matrix at $Q=0$ has determinant $1/(3\sigma^4)>0$; its principal coordinate submodels have full rank. The dossier records this regular interior classification, but numerical inference is explicitly unavailable for this family: no score/Hessian inference adapter or coverage claim is supplied here. See the theory report for the DQM argument and conditions on inference.

## Held-subset study

[`held-study.cpp`](2026-11-11-gg-fit-data/held-study.cpp), its [raw CSV](2026-11-11-gg-fit-data/held-study.csv), and [`summarize-study.py`](2026-11-11-gg-fit-data/summarize-study.py) reproduce a deterministic nine-sample census: three simulated signed-shape samples, two near-seam samples, strict power and Pareto examples, two-point data, and constant data. Each row below contains nine fits. A mask bit holds $\mu$ (1), $\sigma$ (2), or $Q$ (4); held values are 0, 1, and 0.5 respectively. Timings are elapsed workstation milliseconds from this run, not a benchmark guarantee. Boundary stops are proved strict by the implemented face comparison; `IterationFailure` means the candidate search or its accuracy check failed.

| Held | Certified Ok | Uncertified Ok | Stops | Median ms | Max ms |
|---|---:|---:|---|---:|---:|
| none | 3 | 2 | power 1, Pareto 1, numerical 1, unbounded 1 | 13.745 | 27.945 |
| $\mu$ | 0 | 5 | numerical 3, unbounded 1 | 24.680 | 32.186 |
| $\sigma$ | 1 | 8 | — | 1.497 | 4.134 |
| $\mu,\sigma$ | 1 | 8 | — | 1.237 | 4.071 |
| $Q$ | 8 | 0 | unbounded 1 | 0.502 | 0.641 |
| $\mu,Q$ | 8 | 0 | unbounded 1 | 0.479 | 0.628 |
| $\sigma,Q$ | 9 | 0 | — | 0.411 | 0.557 |
| all | 9 | 0 | — | 0.003 | 0.009 |

The four numerical failures are the free two-point sample and the held-$\mu$ power, Pareto, and two-point samples. They are search/accuracy failures in this census, not claims of proved nonexistence. In particular, this study does not settle the held-$\mu$ corner comparisons or the tied two-point free case. The constant sample's outcomes depend on the held subset, as the table records.

## Numerical evidence

The independent 50-digit golden reference for the frozen 500-observation sample reports worst relative coefficient difference $8.5\times10^{-16}$, log-likelihood difference $-5.2\times10^{-14}$, and reference score norm $1.16\times10^{-55}$. It is a local reference, not a global optimality proof. The new golden's tolerance is recorded in the C++ and R shared regression tables; existing family golden files and fits were not changed.

The reproducible [evaluation data](2026-11-11-gg-fit-data/) include 70–80 digit independent checks. Fixed-$Q$ conditional coefficient differences were at most $6.04\times10^{-12}$ across the tested positive, negative, and near-zero shapes. The signed score's worst absolute difference was $7.96\times10^{-15}$; local coefficient checks over all seven nonempty free subsets were at most $1.87\times10^{-14}$. Signed-profile root checks included perturbations down to $10^{-10}$ and had maximum tested coefficient difference $7.57\times10^{-12}$. The strict power and Pareto examples produced their respective certificates, while the tested tied face did not produce a strict face stop. The exact-symmetry zero certificate used 2,150 evaluations; a separate 70-digit grid of 91 positive profile points lay below zero, with symmetry covering their reflected points. Grid and optimizer comparisons are numerical evidence, not substitutes for the stated global bounds.

The appended C++ and R cases cover both signs near the seam, zero shape, holds, constants, two-point data, strict faces and mapped parent-family fits. The full-tier R tests also compare shape-free held subsets against independent multistart optimizations. Scaling checks use $\mu\mapsto\mu+\log b$, with $\sigma,Q$ invariant. The public C++ umbrella, generated JS family registries, shared golden tolerance tables, R family-count and scaling tests, and canonical inference availability were updated as necessary to integrate the new family; no old tests were removed or rewritten to accept new stops.

## Final validation

Workstation builds, tests, and numerical computations ran through the CPU slot lock; final suite commands used `/tmp/umvl-cpu/run4`, separately and to completion. The workstation compiler was GCC 15.2.0. The second-machine attempt used `/tmp/umvl-cpu/simbox-run` and returned 75, `simbox unreachable`; the specified local fallback was then run. Thus there is **no GCC 13/simbox pass** to claim.

| Command or check | Result | Machine |
|---|---|---|
| `just regen`, `just goldens` | passed; only the new sample was created | workstation |
| `just golden-reference --write gengamma` | passed; 50-digit local reference above | workstation |
| `just quality gengamma` | pass | workstation |
| targeted C++ gengamma | 38 cases, 233,766 assertions passed | workstation |
| targeted R gengamma, full tier | 76 passed, 0 failed/warned/skipped | workstation |
| `just cpp-test` | 108/108 shards passed | workstation |
| `just cpp-test-no-long-double` | 135/135 passed; local fallback after simbox exit 75 also 135/135 | workstation |
| `just test` | 23,433 passed; 155 tier skips; 0 failures, 0 warnings | workstation |
| `just test-full` | 46,470 passed; 0 failures, warnings, or skips | workstation |
| `just js-build`, `just js-test` | build passed; 619/619 tests passed | workstation |
| `just site-check` | passed; 100 generated C++ snippets compile and link | workstation |
| `git diff --check` | passed | workstation |

The first R dev run exposed the shared family-count, missing golden tolerance, and log-coordinate scaling assumptions; those integration checks were updated and both dev and full tiers then passed. The first JS build exposed the missing shared C++ golden tolerance, and the first site check exposed stale generated inference availability; both were regenerated or updated and the final runs passed. Package regeneration emitted two pre-existing unresolved roxygen links for `1e-7,1e-6` in other families, and the build showed existing compiler warnings; neither affected test results.

## Open items

- A global certificate for every free-$Q$ held subset, flat seam, tied face, and held-$\mu$ joint corner is not established. Successful locally checked results in such cases are marked uncertified; the study records the actual search failures separately.
- The strict-face and finite-winner arithmetic margins are practical rather than outward-rounded. A uniform floating-point proof, and resolution of comparisons inside those margins, remain open.
- Simbox was unreachable (exit 75); GCC 13 second-machine no-long-double validation remains open. The required workstation run and local fallback passed.
- The alias report assigns the unified name to `gengamma`, but `trgamma` front matter still contains the unqualified `generalized gamma` alias. This task authorized edits to that dossier's prose; resolving the duplicate metadata alias needs a separately scoped front-matter change.
- Numerical inference for `gengamma` is not implemented despite the regularity classification at $Q=0$.
