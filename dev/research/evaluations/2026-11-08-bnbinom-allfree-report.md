# bnbinom all-free certificate

TASK-116.54. Completed 2026-11-09 on `codex/bnbinom-allfree`, based on
`ddb399172bd99091c8d1b04dbf7fe329c8adc43e`.
[Source fingerprints](2026-11-08-bnbinom-allfree-data/source.json).

The all-free fitter now certifies finite global maximizers through uniform
NB/Poisson exterior bounds and a complete stationary cover. It also implements
a uniform inner size/beta profile over alpha intervals, supporting-polynomial
face proofs and the equal-frequency compound-Poisson face theorem.

When these certificates cannot close an all-free decision, the fitter retains
the legacy score-checked finite result or numerical winning-face comparison,
explicitly **uncertified**. A proved comparison takes precedence over that
fallback; genuine search failures still stop. The dossier is
`certified: 'partial'` and names exactly these boundaries. The original NB-face
assertions are restored, including `c(0,0,5,5)` and
`rep(c(0,5), each=20)`.

All required final suites finished green after the review corrections. On the
270 seeded samples, 88 outcomes were certified finite, seven were proved
faces and 175 retained uncertified legacy results; no search failed. Goldens
are unchanged. The independent high-precision reference and its discrepancy
with the tighter interpretation of the frozen golden are reported below.
No commits, merges, rebases, installs or board edits were made.

## Coverage and implementation

| Free coefficients | Verified behavior |
|---|---|
| One shape | Existing scalar single-crossing certificate |
| size, beta; alpha held | Existing global stationary/root cover, retaining both labels and the diagonal witness |
| alpha, beta; size held | Existing global pair certificate, with a broader sufficient NB-face polynomial proof |
| size, alpha; beta held | The transported pair certificate, respecting the actual held beta value |
| All three | New global finite certificate; uniform inner profile when its cover closes; proved Poisson faces; otherwise explicitly uncertified legacy finite/face results, with stops for actual failed searches or proved-wrong legacy answers |
| None | Direct likelihood evaluation, excluded from fitting certification |

All-zero point-mass stops retain their exact nonattainment argument. No
structural hold exception was added; `holds: []` remains unchanged.

Three private headers are wired into
[mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp):
[face_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/face_certificate.hpp),
[alpha_interval_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/alpha_interval_certificate.hpp)
and [joint_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/joint_certificate.hpp).
The pair fitter also calls the new face certificate. These are production
gates, not unused research helpers.

The binding-neutral
[fit.hpp](../../../bindings/common/distributions/bnbinom/fit.hpp)
routes outcomes for both R and JS. `BnbinomResult::certified` follows the
existing fitter result convention, and the common binding uses the existing
`FitResult.extra` scalar channel. No shared result type or serializer changed.
Finite JS results expose numeric `certified` 0/1. The family
[R adapter](../../../bindings/r/distributions/bnbinom/adapter.cpp) exposes a
logical flag and attaches it to the estimates, so the public R fit retains
`attr(fit, "certified")`. R stop conditions also retain the logical flag and
their condition class. Limit messages in both bindings explicitly begin
`Certified global comparison:` or `Uncertified legacy comparison:`; JS error
serialization already carries the message but does not serialize extra fields.

Certified all-free results expose `global_loglik_interval` and
`global_coefficient_lower/upper`; successful additional inner covers expose
`profile_alpha_interval` and `profile_mode_lower/upper` in JS and the internal
R adapter list. Both swapped maxima and the singular-diagonal witness remain
inspectable. Coefficient and profile boxes describe the canonical
representative; reflection supplies the other labeling.

The legacy face comparison is restored with its original per-observation
`1e-8` likelihood threshold and Poisson tie preference. Finite fallback
candidates retain the original finite-difference score and parameter-range
gates. Failure of a certificate or its evaluation allowance alone does not
turn an otherwise successful legacy search into a refusal.

Proved inequalities override the fallback. A strictly better NB bound defeats
a legacy Poisson tie preference. A finite witness whose likelihood lower bound
exceeds the chosen face's upper bound defeats that face. The joint cover now
retains its finite witness and lower bound even when its full coefficient
cover fails, while leaving its unproved estimate coefficients unavailable.
A replacement finite result must pass the same legacy score/range gates and
retain the proved advantage; otherwise the proved-wrong legacy face is refused
with diagnostics. A completed global cover takes precedence over every
legacy candidate. These priorities have appended regression coverage.

For `c(0,0,5,5)` and `rep(c(0,5), each=20)`, the result is again the numerical
NB-face decision, explicitly uncertified, and R reports “Use `mlnbinom`”.
The existing C++ NegativeBinomialLimit and R expect_error assertions remain
unchanged; the previously added refusal assertions were corrected. Failed NB
delegates still stop; their two-coefficient precision-failure witnesses are
explicitly typed as NB so probability is not mislabeled as alpha.

## Global finite proof

The complete [derivations](2026-11-08-bnbinom-allfree-data/derivations.md)
and the [family dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md)
give the inequalities and their implementation correspondence.

A finite likelihood lower bound must strictly exceed upper bounds for the
globally fitted NB and Poisson faces. The NB upper bound includes delegated
size-root uncertainty: the stable mean-profile likelihood and
`|ell''(r)|/n <= 2*mean/r^2` give a Taylor height bound. A rounded NB success
probability is not treated as an exact profile optimizer.

Small-shape mass bounds first exclude size or beta approaching zero. With
`m = size*beta/(alpha+size+beta)`, a finite-count mass bound gives `m < M`.
The uniform Poisson remainder bounds the smaller interchangeable shape above.
After ordering size no greater than beta, CDF and positive-count bounds close
both extreme alpha/beta ratio corners. The uniform NB remainder
`C/(alpha+beta)` then bounds their sum above. These inequalities hold while
alpha varies. They put a nonempty likelihood superlevel in a compact positive
box, proving attainment and interior stationarity in the original parameter
space.

The complete three-dimensional cover retains boxes crossing size = beta.
It excludes boxes using signed scalar-mode bounds, coarse uniform alpha-mode
brackets, monotone likelihood upper bounds and local NB remainders. A
preconditioned interval Newton step preserves every stationary point. For
any fixed matrix M and stationary point p in a box centered at c,

$$p=c-MS(c)+(I-M\bar H)(p-c).$$

The averaged Hessian is enclosed over the whole box. This identity requires
no assumption that the computed approximate inverse M is accurate. Sequential
coordinate intersections preserve all surviving roots.

Newton only proposes a center. A box is accepted only when it lies within
the center's relative coordinate radius `min(tol,1e-8)/4`; every other box
must be excluded. Compactness supplies a global maximizer, and the closed
cover places every canonical global maximizer in that coordinate window.
This proves the returned coefficient accuracy without assuming uniqueness
or accepting a converged local search as the answer.

After the joint cover closes, the fitter additionally calls the alpha-held certificate uniformly
over a positive alpha slab. Endpoint score bounds, interval Hessians,
contraction and a complete conditional stationary cover enclose every global
inner maximizer throughout the slab, including its reflected label. The
proposal must belong to the certified branch. Endpoint fits alone would
not establish this continuum statement. The complete joint cover already
certifies the finite answer independently: an unsuccessful optional inner
cover leaves that stronger certificate intact and omits the profile fields.
In the seeded study, 53 of the 88 certified finite outcomes also completed
the uniform inner cover; the other 35 were certified by the joint cover.

On the frozen sample, the production
[joint check](2026-11-08-bnbinom-allfree-data/joint.json) closes with 40,308
certificate evaluations at iterlim 600, below its allowance of 76,800.
Its likelihood interval is
`[-1275.6474865499017, -1275.6474812378162]`.
The aggregate 280,003 solver iterations come from separately bounded nested
solves. The [uniform inner check](2026-11-08-bnbinom-allfree-data/profile.json)
closes with 3,251 covering evaluations on
`alpha in [2.1419770782757963, 2.1419813622342367]`, enclosing canonical modes in
`size in [1.0658894099767415,1.0685574685749313]` and
`beta in [5.358028378383171,5.3714402141238047]`.

The certificate uses the stipulated package standard: **certified in exact
arithmetic, conditional on computed comparisons, with stated practical
floating-point margins**. It does not claim outward rounding. Score margins
are 128 double eps times accumulated magnitudes; slab and full Hessians use
512 and 1024 eps, preconditioned products 64 eps, and Newton mappings 4096
eps. Likelihood and face comparisons have the additional magnitude-dependent
pads listed in the derivation. The retained
[root-margin probe](2026-11-08-bnbinom-allfree-data/root-probe.json)
finds a largest relative score-margin contribution of about `1.82e-10`.
Much tighter requested tolerances can therefore prevent certificate closure;
a successful legacy search then remains available as uncertified.

Each solver uses at most `min(iterlim,100000)` iterations. Each documented
certificate stage uses at most `128*min(iterlim,100000)` evaluations; the
polynomial allowance counts coefficient updates. Exhaustion, unresolved
arithmetic and unrepresentable coefficient windows cannot produce a certified
estimate. In the all-free route, they fall back to the independently
score-gated legacy result when available; actual failed solves still stop.
The legacy stop-detail upper bound zero remains a valid universal bound;
the additional global interval is tighter.

## Proved face decisions

The supporting derivative polynomial has a known root at one. Dividing by
`1-y` and proving strict positivity of the quotient proves the unique
directional maximum at the fitted face. Homogenization and bounded Bernstein
subdivision extend the old single-sign-change test. The supporting inequality
then bounds every relevant mixture; nondegenerate finite mixing makes the
likelihood strictly smaller, proving an unattained face supremum.

For frequencies `(n0,n1,n3)=(3,10,1)`, the Poisson quotient is
`(101-29*y+13*y^2)/14`; the held-size-one NB quotient is
`(231-29*y+52*y^2)/27`. Their discriminants are respectively `-4411/196`
and `-47207/729`, proving positivity independently of floating-point
subdivision. Exact rational Bernstein leaves are retained in the
[oracle output](2026-11-08-bnbinom-allfree-data/oracle.json).

Equal positive frequencies at 0,...,4 have a separate global theorem.
Every BNB is Poisson mixed by `G_size*G_beta/G_alpha`. Gamma densities are
hyperbolically completely monotone; products and ratios preserve this
property, and an HCM density is a generalized gamma convolution. These are
the results used from [Behme and Bondesson, Propositions 1 and 3](https://arxiv.org/abs/1507.04017).
The GGC Laplace representation makes the count law compound Poisson.

For this sample, removing jumps above four increases likelihood; optimizing
an exponential tilt restricts the remaining jump intensities to a mean-two
simplex. The negative radial dispersion derivative has a degree-11 numerator.
Homogenization and four degree elevations give 759 positive coefficients
and 57 zeros, with strict decrease near the Poisson endpoint along every
nonzero dispersion ray. Thus Poisson(2) uniquely maximizes likelihood over
compound-Poisson laws. Finite BNB cannot attain it, and a concentration path
approaches it. Replication preserves the argument.

The [exact rational checker](2026-11-08-bnbinom-allfree-data/compound-poisson-proof.py)
and [coefficient fingerprint](2026-11-08-bnbinom-allfree-data/compound-poisson.json)
reproduce the algebra. The runtime frequency recognizer is wired into the
all-free decision. This additional theorem is needed because the
mixed-Poisson supporting test fails here: `D(0)=exp(2)>5`. No containment
between the full compound-Poisson and mixed-Poisson classes is assumed.

## Independent values, goldens and nonuniqueness

The [independent oracle](2026-11-08-bnbinom-allfree-data/oracle.py) uses direct
mpmath gamma/digamma formulas at 80-digit precision. Its numerical stationary
root is:

| Quantity | Independent value |
|---|---:|
| size | 1.06722343927581191837420231405645 |
| alpha | 2.14197922025506372146287362523174 |
| beta | 5.36473429625378530962489118169215 |
| Log likelihood | -1275.647486369558616740192902391587 |

The production coefficients are
`(1.0672234392758453,2.1419792202550165,5.3647342962534097)`, agreeing within
`1e-12` relative error. High-precision stationarity is an independent
numerical check; the completed production cover supplies global coefficient
certification.

The frozen golden is
`(1.0672234787479615,2.1419791546447171,5.3647338351087397)`.
Its relative coefficient differences from the independent root are about
`3.70e-8`, `3.06e-8` and `8.60e-8`. These exceed `1e-8`, so the independent
values above are the tighter reference requested by the task. The existing
`2e-6` golden regression tolerance passes; the golden was not edited.
The likelihood change is below `1e-8`. All nonempty held subsets return
bit-identical status, coefficients and likelihood before and after; the new
certification metadata is additional.

The certified canonical global box is disjoint from its reflection and from
size = beta. Existence and exact swap symmetry therefore prove **at least
two distinct labeled global maximizers** on this sample. No general
identified-uniqueness theorem is asserted. The retained diagonal witness
is approximately `(2.0995783843540756,1.8339012651122904,2.0995783843540756)`,
with numerical log likelihood `-1276.2838627028506054`. The separate
[diagonal-exclusion probe](2026-11-08-bnbinom-allfree-data/diagonal-probe.json)
closes all 857 boxes. The diagonal is retained as a diagnostic, never selected
solely from symmetry.

## Timings

The [benchmark](2026-11-08-bnbinom-allfree-data/benchmark.sh),
[paired records and ranges](2026-11-08-bnbinom-allfree-data/timings.json)
and [comparison checks](2026-11-08-bnbinom-allfree-data/summarize.py) use the
same 500-count sample and held golden values. These are simbox C++17/O2
medians of three paired runs, five calls per timing, under nice 10 with
single-threaded BLAS/OpenMP. The baseline headers are frozen copies from
`ddb39917`, with [hashes](2026-11-08-bnbinom-allfree-data/baseline/source.json);
the refreshed benchmark ran through `/tmp/umvl-cpu/simbox-run`. Queue time
and compilation are excluded.

| Held coefficients | Before ms | After ms |
|---|---:|---:|
| None | 207.807 | 2883.824 |
| size | 159.560 | 159.745 |
| alpha | 355.644 | 354.878 |
| size, alpha | 1.115 | 1.100 |
| beta | 70.434 | 70.250 |
| size, beta | 1.021 | 1.017 |
| alpha, beta | 1.023 | 1.032 |
| All | 0.0333 | 0.0335 |

The new all-free proof costs about 13.9 times the previous numerical search
on this workload. Shared load produces substantial variation; no speed
improvement is claimed for unchanged routes. Their outputs are identical.

## Seeded coverage and timing

The [driver](2026-11-08-bnbinom-allfree-data/coverage.cpp) fits all three
coefficients freely at the production defaults, `tol=1e-8`, `iterlim=600`.
The fixed design has two seeds per cell and sample sizes 30, 200 and 1000:

- BNB: size in {0.5,1,3}, alpha in {0.75,1.5,3,8}, beta in {0.5,2,8}:
  216 samples, including the infinite-mean alpha=0.75 stratum.
- NB: size in {0.5,2,10}, probability in {0.2,0.7}: 36 samples.
- Poisson: mean in {0.3,2,10}: 18 samples.

Each row uses `StdRng` with seed `11654000 + row_id`; the
[raw samples](2026-11-08-bnbinom-allfree-data/coverage-samples.jsonl),
[per-fit results](2026-11-08-bnbinom-allfree-data/coverage.csv),
[summary](2026-11-08-bnbinom-allfree-data/coverage.json) and
[production-source fingerprints](2026-11-08-bnbinom-allfree-data/coverage-source.json)
are retained. The latter match the sources used by all final suites.
The [resumable launcher](2026-11-08-bnbinom-allfree-data/coverage.sh) verifies
complete sample/result pairs and source identity before resuming. The study
ran on simbox through its wrapper, one computation thread under nice 10.

| Generating family | Samples | Certified finite | Proved face | Uncertified legacy | Search failure |
|---|---:|---:|---:|---:|---:|
| BNB | 216 | 88 (40.74%) | 3 (1.39%) | 125 (57.87%) | 0 |
| NB | 36 | 0 | 0 | 36 (100%) | 0 |
| Poisson | 18 | 0 | 4 (22.22%) | 14 (77.78%) | 0 |
| Total | 270 | 88 (32.59%) | 7 (2.59%) | 175 (64.81%) | 0 |

The uncertified outcomes comprise 77 finite fits, 85 NB-face decisions and
13 Poisson-face decisions. The seven proved faces comprise six Poisson
limits and one all-zero point-mass limit. No all-free NB-face decision in
this study is certified. No sample overflow or search failure occurred.

| Generating family | Mean fit ms | Median fit ms | Maximum fit ms |
|---|---:|---:|---:|
| BNB | 2240.61 | 1138.44 | 44495.67 |
| NB | 773.62 | 165.28 | 4558.40 |
| Poisson | 371.55 | 31.44 | 2763.45 |
| Total | 1920.41 | 964.99 | 44495.67 |

Total fitting time was 518.51 seconds. Timing excludes generation, compilation,
queueing and summary calculation; it includes the legacy search and all
attempted certificate stages. These are descriptive frequencies on this
fixed grid with two seeds per cell, not estimated population frequencies.
They depend on the design, default work allowance, machine and floating-point
comparisons. Generating from a limit family does not itself prove that its
face maximizes a finite sample's likelihood.

## Open mathematical classifications and fallback behavior

1. **General winning NB faces.** The absolute remainder `C/(alpha+beta)`
   excludes an NB neighborhood when a finite witness has a strict advantage.
   It cannot establish dominance when that gap vanishes. Certification needs
   a signed uniform neighborhood inequality and a complete finite-plus-face
   comparison, including ties. `(0,0,5,5)` remains an unresolved proof case:
   its legacy NB-face decision is returned as uncertified, with witnesses.
2. **Other winning Poisson faces.** Supporting-polynomial and equal-frequency
   compound-Poisson conditions are sufficient. Samples outside them need
   another global inequality or signed face-neighborhood analysis. Failed
   sufficient tests do not establish nonexistence. Successful legacy finite
   or face outcomes remain available, explicitly uncertified.
3. **General identified uniqueness.** Labeled nonuniqueness has the proved
   counterexample above. The finite certificate needs no general uniqueness
   theorem; unresolved competitors outside its coefficient window prevent
   certification. No broader uniqueness classification is claimed.

These are precise limits of the mathematical certification. They do not
cause a new refusal when the legacy route succeeds. An actual failed search,
failed NB delegate, or a legacy face proved worse without an acceptable
replacement still has a structured stop with diagnostic witnesses. Failed
certificate candidates are never mislabeled as certified estimates.

## Final validation and scope

Every required final command completed successfully. The retained
[validation record](2026-11-08-bnbinom-allfree-data/validation.txt) records
timestamps and counts. These runs follow the review corrections and supersede earlier results;
an interrupted run is not counted as a pass. Numerical sources stayed
unchanged throughout these final suites.

| Check | Machine | Result |
|---|---|---|
| Targeted native and no-long-double bnbinom | simbox | Each: 45 cases, 206758 assertions, zero failures |
| C++ dev | simbox | 106/106 |
| C++ full | simbox | 129/129 |
| C++ no-long-double | Workstation | 129/129 |
| C++ no-long-double | simbox | 129/129 |
| R dev | simbox | FAIL 0, WARN 0, SKIP 154, PASS 22194 |
| R full | simbox | FAIL 0, WARN 0, SKIP 0, PASS 45026 |
| JS build and tests | Workstation | Build succeeds; 560/560 tests |
| Site check | Workstation | 99 MathML family pages, 99 C++17 snippets, relations, links and browser checks pass |
| Independent arithmetic, production covers and paired output checks | Workstation and simbox | Pass; retained JSON and exact-polynomial evidence |
| Additional seeded R sample, all eight subsets | Workstation | Pass; [raw adapter results](2026-11-08-bnbinom-allfree-data/r-targeted.json) |
| Frozen files, appended shared tests, source identity and whitespace | Workstation and remote source checks | Pass |

Local suites used `/tmp/umvl-cpu/run4`, one suite per foreground command;
R suites, C++ dev/full and the second no-long-double run used
`/tmp/umvl-cpu/simbox-run`.
The final simbox runs needed no exit-75 fallback. The wrappers apply nice 10,
single-threaded BLAS/OpenMP and the documented CPU reservations. Both the
workstation GCC 15 and simbox GCC 13 double modes were exercised. JS and site
checks completed after the review corrections with the same final sources.
The last required suite, site-check, completed at 2026-11-09 16:23:55 UTC.

The [single-suite launcher](2026-11-08-bnbinom-allfree-data/single-suite.sh)
reproduces each suite separately, for example:

~~~sh
/tmp/umvl-cpu/run4 sh dev/research/evaluations/2026-11-08-bnbinom-allfree-data/single-suite.sh workstation cpp-full
/tmp/umvl-cpu/run4 sh dev/research/evaluations/2026-11-08-bnbinom-allfree-data/single-suite.sh workstation cpp-no-long-double
/tmp/umvl-cpu/simbox-run sh dev/research/evaluations/2026-11-08-bnbinom-allfree-data/single-suite.sh simbox test-full
~~~

Changes are confined to bnbinom, its common binding, its R adapter and family tests,
append-only shared JS tests, and this evaluation evidence. The original C++ and R
assertions were restored byte-for-byte and checked against the base; new
C++ cases are appended. The added `(0,0,5,5)` checks assert the restored
uncertified NB face. Shared infrastructure headers are unchanged.
Settled d/p/q/r, special functions, truth files, code generators, goldens,
NEWS, status files and roadmaps are unchanged. No changes to those settled
components are needed by this implementation. The site check's unrelated
archived screenshot was restored, and obsolete scratch build products were
removed.

The repository-wide Markdown link checker retains four pre-existing
findings outside this task: the missing boundary-points PDF, an nbinom
mathematical-expression false positive and two `/inference/` links. Changed
family and evaluation links are checked separately; those findings were not
edited as part of this job.

The artifact-pane display was attempted after the report was written. The
viewer returned HTTP 403 because its readable roots exclude this worktree.
Viewer configuration and other checkouts were left unchanged; the report
remains available directly in this worktree.
