# GEV and Fréchet all-free certificates — TASK-116.51

Final validation completed 2026-11-09 UTC in `codex/gev-allfree`, based on
`ddb399172bd99091c8d1b04dbf7fe329c8adc43e`. Changes remain uncommitted.

## Delivered scope

**Open G1 and F2 are closed.** General all-free GEV now uses a global shape
cover on its fitted branch `-1 <= xi <= 1`. General all-free Fréchet uses
the positive part of that cover, with direct public-coefficient inclusion
and a certified comparison against the proper Gumbel closure. Successful
estimates have coefficient enclosures; an unresolved search stops with
budget, solver, numerical or comparison-tie details instead of returning
an unchecked numerical incumbent.

| Public free subset | Outcome |
|---|---|
| GEV `{mu,sigma,xi}` | Global certificate, including shape endpoints, Gumbel seam and existing collapse/two-point classifications |
| Fréchet `{lambda,mu,sigma}` | Global positive-shape cover, finite versus Gumbel comparison, existing collapse/two-point classifications |
| GEV shape free with one nuisance coefficient held | Compatible exact members remain certified; general G2 remains open below |
| Fréchet `{lambda,mu}`, public `sigma` held | Compatible finite two-point members remain certified; general F1 remains open below |

Both dossiers retain **`fit.certified: partial`**, because their supported
held-nuisance subsets are not all certified. Their `fit.note` fields now
identify the new all-free coverage and the remaining conditional scope.
The public shape restrictions and default `iterlim=100` are unchanged.
Neither golden was edited; both pass the frozen coefficient and likelihood
tolerances.

The previous phase's Fréchet two-point certificate and exact compatible
GEV/Fréchet holds remain wired in. Its inverse-shape convexity proof is in
[two-point-proof.md](2026-11-08-gev-allfree-data/two-point-proof.md).

## Mathematical certificate and implementation

The [GEV certificate](../../../univariateML-cpp/include/cppunivariate/distributions/gev/joint_certificate.hpp)
is called by the public all-free GEV fitter and by the all-free Fréchet
route. The old general eight-cell GEV and 32-cell Fréchet numerical scans
are replaced. The existing point-profile solver supplies proposals and
feasible likelihood lower witnesses only; its convergence never establishes
coverage or authorizes an `Ok` result.

The [proof](2026-11-08-gev-allfree-data/joint-proof.md) establishes these pieces:

1. At fixed nonzero shape, scale profiling leaves a unique monotone
   endpoint-gap root. The inner solver verifies opposite score signs and
   exposes a root bracket. Monotonicity of that gap gives nuisance boxes
   from brackets at slab endpoints. Uniform score signs, interval Newton
   and implicit-derivative tubes enclose the inner optimum through zero.
2. With normalized observations `d_i >= 0`, minimum pivot `v`, and
   `L_i=log1p(xi*d_i/v)/xi`, the exact profiled value is

   $$f=\ell/n=-\log v-\log\operatorname{mean}(e^{-L_i})
                  -(1+\xi)\operatorname{mean}(L_i)-1.$$

   All `L_i >= 0`, all exponentials are at most one, and their sum is at
   least the minimum multiplicity. Near zero, convergent power-of-radius
   series with explicit remainder bounds enclose the value and two shape
   derivatives. The large exponential Gumbel-error constant is absent.
3. Negative-shape endpoint-gap coordinates keep rectangular boxes inside
   moving support. A one-sided reverse-exponential face-value envelope
   covers a whole neighbourhood of `xi=-1`; no derivative interval is
   taken through its singular endpoint. The value-only negative-shape
   envelope also excludes pivot slabs before a feasible rectangular pivot
   box can be formed. Existing minimum-majority and half-minimum collapse
   proofs handle the remaining data-dependent faces.
4. Every outer slab is retained until its likelihood upper bound loses to
   a feasible incumbent, its score has a definite sign, its profile is
   strictly convex, or a stationary maximum is isolated. Centered Taylor
   bounds preserve the zero nuisance score. Strict negative profile
   curvature and opposite endpoint scores prove existence and uniqueness
   within an isolating slab; joint interval Newton then encloses every
   public coefficient. All surviving candidates and attained shape
   endpoints are compared globally. Separated unresolved near-equal
   candidates stop as a comparison tie with witnesses.
5. Fréchet restricts the cover to `0 <= xi <= 1`. Its public map is
   `lambda=1/xi`, `mu_F=m-v/xi`, `sigma_F=sigma_G/xi`. The stopping rule
   encloses these coefficients directly, including relative shape
   precision near zero. A strictly winning zero-shape face returns a typed
   Gumbel limit with proper Gumbel coefficients and advice to use
   `mlgumbel`. A held public Fréchet scale does not preserve this free
   profiling orbit and is not passed through this route.

Negative-shape public location bounds retain power-mean dependencies:
`partial_e s <= 1` implies that `s(e,a)-e` decreases in the endpoint gap.
Four monotone corner evaluations replace an unnecessarily wide independent
subtraction. This resolves the five-point regression
`{0,.22725282343514291,.56276665892011701,.58275143915118077,1}` without
loosening tolerance. Its independent 50-digit solution is retained in the
reference JSON. Infeasible pivot rectangles now trigger subdivision
immediately; one-sided bracket expansion respects the support floor and
stops when it encloses the proved endpoint-gap corner box.

The package certificate standard applies: **exact arithmetic conditional on
the computed comparisons, with practical floating-point margins**. This is
not outward-rounded interval arithmetic. Compensated sums, `128*epsilon`
pivot-jet margins, `256*epsilon` gap/coefficient margins and
`4096*epsilon` outer comparisons have the scale factors stated in the code.
Original-coordinate restoration has separate representability and
likelihood checks. Coefficient isolation uses the documented location scale
`max(abs(mu),sigma)`; GEV shape has unit scale and Fréchet shape uses relative
precision. Arithmetic stagnation never returns an unchecked estimate.

Uniqueness is conditional on the isolations and global comparisons, not a
theorem that every supported subset has one maximizer. A concrete nonunique
subset is `x=(4,4,4)` with public GEV `mu=4,sigma=2` held: every
`xi in [-1,1]` has likelihood `-n*(1+log(2))`. The existing test covers this
flat shape profile. The earlier independent golden-profile checks also
show positive curvature at `xi=-0.5` and negative curvature at `xi=-0.1`;
global shape concavity is not assumed.

## Work budget and timings

The shared certificate ledger allows `128*min(iterlim,100000)` evaluated
slabs, profile points and contractions. Nested solver loops have their own
`min(iterlim,100000)` iteration caps. A certificate cap gives a budget stop;
a solver cap gives non-convergence; precision stagnation gives numerical
failure. Binding-neutral routing preserves these details in R and JS,
including finite witnesses, proper Gumbel witnesses and minimum-majority
nonexistence. No shared core or stop-detail header was changed.

The default budget is **12,800**. Both goldens finish comfortably within it,
so no default increase is proposed or silently applied. The 189 seeded GEV
samples all returned certified `Ok`; the largest ledger count was **1,453**.
They cover nine generating shapes from `-0.9` to `2`, seven sample sizes
from 3 to 500, and three replicates per combination (seed 20261029).
Generating shape 2 does not change the fitter's restricted shape branch.

Paired workstation timings below use the same samples and optimized C++
binaries. “Before” is the pre-G1 worktree fitter, reconstructed from the
retained header archive; it already includes the first phase's exact
shortcuts. Golden times average three fits; large-sample times use one fit
(seed 11651). These are shared-workstation wall times, not a controlled
performance study.

| Sample | Before, seconds | Certified, seconds | Certificate evaluations |
|---|---:|---:|---:|
| GEV golden, n=500 | 0.007939 | 2.868485 | 515 |
| Fréchet golden, n=500 | 0.018220 | 1.204227 | 394 |
| GEV, n=10000 | 0.177713 | 55.552556 | 701 |
| Fréchet, n=10000 | 0.160228 | 15.373613 | 452 |

Certification has substantial runtime cost, particularly for large GEV
samples. No n=100000 runtime claim is made. All four default fits certified;
the paired check verifies golden tolerances and likelihood agreement with
the preceding numerical fits. Raw results and reproduction are
[joint-final-before.jsonl](2026-11-08-gev-allfree-data/joint-final-before.jsonl),
[joint-final-after.jsonl](2026-11-08-gev-allfree-data/joint-final-after.jsonl),
[joint-final-timing-summary.json](2026-11-08-gev-allfree-data/joint-final-timing-summary.json)
and [joint-final-benchmark.sh](2026-11-08-gev-allfree-data/joint-final-benchmark.sh).

The unchanged exact shortcuts retain the first phase's subset timings:
Fréchet two-point n=20,k=1: 365.718 to 3.798 microseconds;
GEV two-point held location n=4: 4020.611 to 0.821;
GEV two-point held scale n=4: 5336.306 to 0.234;
Fréchet compatible held scale n=16: 219.963 to 1.036.
Those comparisons use the original base, not the pre-G1 archive; their
[raw timing summary](2026-11-08-gev-allfree-data/timing-summary.json)
retains all workloads and batches.

## Independent evidence and goldens

The [50-digit checker](2026-11-08-gev-allfree-data/joint-check.py) passed
54 interval-derivative comparisons, 54 public-coefficient comparisons,
nine independent inner-root inclusions and three one-sided face checks.
These numerical checks test the computed enclosures; the proof supplies
global coverage. The independent stationary references are:

| Golden | Location | Scale | Shape | Log likelihood |
|---|---:|---:|---:|---:|
| GEV | 0.006623074223683958 | 0.9955800131732660 | xi=0.3084360154657577 | -874.76388443183023 |
| Fréchet | 0.06127683692505054 | 0.9001111496659627 | lambda=1.7802216014234668 | -608.45464812711251 |

Full digits, the five-point reference and check counts are in
[joint-independent.json](2026-11-08-gev-allfree-data/joint-independent.json);
computed boxes are in
[joint-bounds.json](2026-11-08-gev-allfree-data/joint-bounds.json).
Both frozen goldens pass `1e-10` relative coefficient and likelihood
comparisons. Golden, d/p/q/r and truth files remain byte-identical to the
base. No reference was used to edit a golden.

## Precise open entries

- **G2: general GEV with public location or scale held and shape free.**
  The free-orbit gap profile does not respect either public constraint.
  For held scale and positive shape, the location-score equation now gives
  a proved region `m+shift <= mu <= m+sigma*log(2*n/k)` and a unique inner
  root, without the old exponential-radius constant. The
  [supporting derivation](2026-11-08-gev-allfree-data/held-scale-region-proof.md)
  states the bound. Missing implementation is a uniform constrained
  inner-root enclosure, profile jets, coefficient isolation and outer
  root/value coverage, including the negative-shape moving-support face.
  The held-location/free-scale problem needs its own constrained inner
  enclosure. Compatible exact members remain the implemented exception;
  general conditional numerical searches are not promoted to certificates.
- **F1: Fréchet shape/location free with public scale held.** A constructive
  compactness proof now bounds the shape tail and small-gap superlevel face.
  At the unique fixed-shape location root,
  `e >= b(lambda)=[lambda*k/((lambda+1)*n)]^(1/lambda)` and
  `e^(-lambda) <= 2*n/k`. The increasing lower bound gives a uniform root
  rectangle on each shape slab. For a slab `[l,h]` with `h<=2*l`, every
  power exponential throughout that rectangle is at most `(2*n/k)^2`.
  These proved bounds remove both compactness and exponential growth as
  principal blockers. The remaining work is an implemented budgeted
  constrained profile-jet module, complete stationary/value cover and
  public-coefficient inclusion. On the held-scale golden, the supporting
  superlevel construction obtains `lambda<=2048` and `e>=1/8192`;
  the root bound strengthens the latter to `e>=1/1000`.
  [50-digit checks](2026-11-08-gev-allfree-data/held-scale-region-check.json)
  cover 21 face/tail inequalities and five independently bracketed roots.
  A held scale imposes `sigma_G=sigma_F*xi`, so the free positive-scale
  Gumbel face from F2 cannot be substituted. This supporting mathematics
  is not an implemented F1 certificate.

These are the remaining optional constrained subsets, not missing G1/F2
coverage. Neither open entry is attributed to a requirement for outward
rounding.

## Final validation and change boundary

Every required final command finished green. Local work used the prescribed
slot locks, nice level 10 and single-thread BLAS/OpenMP. Simbox runs used its
wrapper. No command remains running.

| Command | Machine | Final result |
|---|---|---|
| `just cpp-test` | workstation, GCC 15 | 106/106; 331.98 s |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13 | 129/129; 286.72 s |
| `just cpp-test-no-long-double` | workstation, GCC 15, Release | 129/129; 182.34 s |
| `just cpp-test-no-long-double` | simbox, GCC 13 | 129/129; 196.52 s |
| `just test` | simbox | 22190 passes, 154 expected skips, zero failures/warnings; 79.028 s |
| `just test-full` | simbox | 45022 passes, zero skips/failures/warnings; 301.867 s |
| `just js-build` | workstation | green |
| `just js-test` | workstation | 561/561; 335.917 s |
| `just site-check` | workstation | 99 family pages and C++17 snippets; MathML/browser checks green |

Final logs are the `joint-final-*.log.gz` files in this report's data folder;
[joint-final-suites.sh](2026-11-08-gev-allfree-data/joint-final-suites.sh)
reproduces the commands with simbox exit-75 fallback. Final independent
checks ran on simbox, seeded cases and paired timings on the workstation.
The earlier simbox benchmark transport dropped and was retried locally.
An obsolete pre-fix timing process was stopped after its replacement passed;
it supplies no final timing or validation claim. Pre-final checks exposed
and resolved the coefficient-dependency stall and pivot-box expansion issue
above. Nonfatal compiler/startup warnings in build logs are pre-existing;
R test warning counts are zero.

Changes are confined to GEV/Fréchet fitters, dossiers, family tests, their
binding-neutral adapters, appended JS tests and this report/evidence.
Shared JS and family C++ tests are append-only. No shared core header,
`special/`, d/p/q/r, truth source, generator, NEWS, STATUS, KNOWN_ISSUES or
roadmap file changed. No dependency was installed and no board or Git write
command was run. The unrelated site-generated screenshot was restored
byte-for-byte. The [source manifest](2026-11-08-gev-allfree-data/source-state.json)
records fixture preservation, append-only checks and final source hashes.
