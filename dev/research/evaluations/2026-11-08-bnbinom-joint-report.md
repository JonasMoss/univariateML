# bnbinom joint certificate report

TASK-116.42, 2026-11-08. Branch `codex/bnbinom-joint`, base
`1c61c57dbe83a726adca4ebe1bc8f2dca490e213`.
[Final source fingerprints](2026-11-08-bnbinom-joint-data/source.json).

Both remaining two-free-shape routes now return certified finite global
maxima or terminal structured stops. Their negative-binomial faces have a
new sufficient winning-face certificate. The all-free route gains a proved
class of Poisson-face decisions. On the golden sample it retains both swapped
numerical candidates and a diagonal witness. Its general finite maximizer and negative-binomial
face decisions remain open, as specified below. Family-wide `fit.certified`
remains `partial`; the dossier's `fit.note` names this coverage.

All required final test runs finished green. The golden file is unchanged,
and all-free golden coefficients and likelihood are identical before and
after on the workstation. No commits were made.

## Coverage and changed behavior

| Free coefficients | Result |
|---|---|
| One shape | Existing scalar certificate, unchanged |
| size, beta; alpha held | Existing global rectangle/root certificate, unchanged |
| alpha, beta; size held | New uniform NB-neighborhood exclusion, finite rectangle cover, root isolation and global comparison; supporting-polynomial NB-face proof where applicable |
| size, alpha; beta held | The same certificate after transporting the actual held beta value to canonical size, then transporting every finite result and witness back |
| All three | New supporting-polynomial Poisson-face proof where applicable; other finite and face searches retain their numerical algorithm and coefficients |
| None | Direct likelihood evaluation |

The implementation is in
[pair_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/pair_certificate.hpp),
called by
[mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp).
Routing and stop classes remain binding-neutral in
[fit.hpp](../../../bindings/common/distributions/bnbinom/fit.hpp).
There is no R-only dispatch. Budget, arithmetic and unresolved-comparison
stops do not return estimate coefficients. Overflowing box-ratio denominators
stop numerically before any exclusion. NB delegate precision failures retain
the numerical stop class, including when a held size makes its face probability
unrepresentable; the parent does not replace that failed face with Poisson. All-zero point-mass stops and
scalar fits retain their previous behavior. No structural hold exception was
added.

For example, the golden sample with size held at 1 has the certified fit
`(size, alpha, beta) = (1, 2.2093545126014242, 6.0278846419290313)`.
With beta held at 1 the result is
`(6.0278846419290313, 2.2093545126014242, 1)`.
There is no imposed ordering in either held problem. Holding size at 5 instead
gives `(5, 2.0963969113416687, 1.1060203341395347)`; holding a different
coefficient at a different value is not a symmetry-equivalent problem.

The sample `(0,0,5,5)` with size or beta held at 1 now stops numerically,
retaining both finite and NB-face witnesses. Neither an unsuccessful global
comparison nor a BFGS endpoint is reported as an estimate. Its face does not
pass the new sufficient supporting-polynomial condition.

## Mathematical evidence

The canonical family-specific proofs are in the dossier sections
[Alpha-free pair certificates](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#alpha-free-pair-certificates)
and
[All-free face bounds](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#all-free-face-bounds).
The retained
[derivations](2026-11-08-bnbinom-joint-data/derivations.md)
give the exterior inequalities and the proposed profiled-cover construction.

With size fixed, beta-mixture probability bounds exclude small beta and both
extreme mixing ratios uniformly as alpha varies. A gamma-increment bound
then gives an absolute log-likelihood remainder `C/(alpha+beta)` against the
fixed-size NB face. A finite witness strictly above the face therefore supplies
an effective compact rectangle. Positive mixed alpha/beta score derivatives,
the existing scalar single-crossing theorem and monotone mass bounds exclude
boxes. Remaining boxes must lie inside contraction-certified, negative-definite
root neighborhoods. The entire cover closes before any estimate is returned.
All isolated competitors are compared; unresolved identified ties stop.

The narrow root enclosure has relative radius
`min(gradtol, 1e-8)/4`, independently of the optimizer's stopping rule.
Likelihood enclosures include variation across that coefficient enclosure.
The floating-point contract is the package standard: exact arithmetic
certification conditional on computed comparisons, with practical margins,
not outward rounding. Scores retain the existing `128 * epsilon` digamma-term
margin; Hessian and likelihood terms use `512 * epsilon` magnitude margins,
with the additional likelihood margin `1e-10*n*(1+abs(logLik)/n)`.

The winning-face condition uses concavity of likelihood in a mixing law.
For the NB face, its directional-likelihood derivative has coefficient signs
`(k+1)*n[k+1] - (k+r)*q*n[k]`. One sign change from positive to negative
proves a unique maximum at the fitted face. A nondegenerate beta mixture has
strictly lower likelihood. This proves nonattainment, rather than merely
comparing a finite search with the face. Positive constant and binary samples
pass; size 1 with sample `0:4` is another NB example. Unresolved coefficient
signs do not pass the condition.

For all-free fits, BNB is mixed Poisson. The analogous signs
`(k+1)*n[k+1] - mean*n[k]` prove Poisson nonattainment for samples passing
that test, including positive constant and binary samples. This is sufficient,
not necessary. Other legacy Poisson comparisons are not promoted to proofs.

Proposals, compactness expansions and covering boxes share
`128*min(iterlim,100000)` certificate evaluations. Each nested optimization,
Newton refinement and contraction-radius search uses at most
`min(iterlim,100000)` solver steps. The NB face delegate receives the caller's
allowance. Exhaustion and arithmetic failures are terminal.

No general uniqueness theorem is asserted for the new pairs. The inherited
alpha-held counterexample remains a proved failure of labeled uniqueness:
on the golden sample with alpha 2, the certified maximum approximately
`(1.1509739520206237, 2, 4.5037452914721273)` and its swap are distinct.
The earlier
[diagonal exclusion and global certificate](2026-11-07-bnbinom-alpha-report.md)
remain unchanged and pass their regression cases. All-free swapped candidates
are numerical witnesses; their global optimality is not inferred from symmetry.

## Independent numerical evidence

The [70-digit oracle](2026-11-08-bnbinom-joint-data/oracle.py) uses direct
mpmath gamma/digamma formulas. Its
[retained output](2026-11-08-bnbinom-joint-data/oracle.json)
contains four pair roots, including both held golden values, and 600 sampled
checks of the uniform NB, Poisson and finite-count upper bounds. The new
pair estimates agree within `1e-8` relative error. This is independent numerical
evidence; global coverage follows from the production certificate and proofs.

Tests exercise transported held values 1, 5 and both golden values, finite
root bounds, exact face cases, forced budget stops, unresolved faces, mass and
Hessian box bounds, R/JS stop payloads, unrepresentable NB face probabilities,
and preservation of the all-free
swapped and diagonal witnesses. The original tests remain byte-for-byte prefixes
of their files.

## Timings and goldens

[Benchmark source and launcher](2026-11-08-bnbinom-joint-data/benchmark.sh),
[paired measurements and ranges](2026-11-08-bnbinom-joint-data/timings.json)
and [comparison checks](2026-11-08-bnbinom-joint-data/summarize.py) are retained.
These are workstation C++17/O2 medians from three paired runs, five calls per
timing, using the same 500-count frozen sample and held values.

| Held coefficients | Before ms | After ms |
|---|---:|---:|
| None | 232.495 | 230.299 |
| size | 47.990 | 177.385 |
| alpha | 386.982 | 386.184 |
| size, alpha | 1.220 | 1.225 |
| beta | 45.387 | 77.173 |
| size, beta | 1.110 | 1.122 |
| alpha, beta | 1.122 | 1.130 |
| All | 0.037 | 0.037 |

Shared workstation load produced wide timing ranges. No speed improvement is
claimed for unchanged routes. The new size-held certificate costs about 3.7
and the beta-held certificate about 1.7 times their previous searches in these
medians. Unchanged routes return identical status, coefficients and likelihood
across every paired run. New pair coefficients remain within the existing
golden tolerance and agree with the independent roots. No golden was regenerated.

## Open entries

1. **General all-free finite certificate.** The uniform exterior bounds are
   proved, but a complete stationary/root cover is not implemented. On the
   golden sample a finite witness has log likelihood `-1275.6474863695587`,
   above the certified NB face `-1303.7427190771177` and Poisson face
   `-3336.7458743346015`. The derived canonical superlevel bounds have
   `size >= 1/16`, `size <= 480.5239176692118` and
   `alpha+beta <= 1.9029657619549489e25`, together with positive ratio cutoffs.
   These prove conditional compactness and attainment, not that the retained
   numerical root is the maximizer.

   The reproducible [profile probe](2026-11-08-bnbinom-joint-data/profile-probe.sh)
   and its [output](2026-11-08-bnbinom-joint-data/profile-probe.json) attempt
   to compose the existing unique conditional-alpha certificate over this
   domain. Four of six endpoint solves stop `NumericalFailure`; two succeed.
   The scalar maxima exist, but the current cancellation margin cannot supply
   the requested coefficient enclosures at the large endpoints. A viable
   adaptive cover must exclude exterior boxes before profiling, or use coarser
   signed brackets/stable differences there, then isolate all remaining roots.
   It must retain the diagonal and both swapped labels. The current multistart
   window cannot substitute for that cover. All-free finite coefficients are
   preserved, with their certification limitation explicit in the dossier.

2. **Winning faces beyond the sufficient polynomial tests.** The absolute
   uniform remainders exclude a face neighborhood when a finite witness beats
   the face. They do not prove a winning face when no such strict gap exists.
   Completing those decisions needs signed neighborhood bounds, an additional
   global mixture certificate, or another complete finite-plus-face argument,
   including ties. Alpha-free pair fits stop numerically in these unresolved
   cases. General all-free NB and remaining Poisson decisions retain their
   previous numerical comparisons and are not certified.

These are the precise remaining requirements for family-wide `certified: yes`.
The added all-free Poisson proofs and uniform exterior analysis do not close them.

## Final validation

Every command below completed successfully. The retained
[validation record](2026-11-08-bnbinom-joint-data/validation.txt)
contains commands, timestamps, counts and scope checks.

| Run | Machine | Result |
|---|---|---|
| Targeted bnbinom | Workstation and simbox | Each: 29 cases, 188381 assertions, zero failures |
| C++ dev | simbox | 106/106 shards |
| C++ full | simbox | 129/129 shards |
| C++ no-long-double | simbox | 129/129 shards |
| C++ no-long-double | Workstation | 129/129 shards |
| R dev | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21610 |
| R full | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44425 |
| JS build and tests | Workstation | Build succeeds; 505/505 tests |
| Site check | Workstation | 99 family pages with MathML, 99 C++17 snippets, links and browser checks pass |
| Independent oracle | simbox | Four roots and 600 bound comparisons pass |
| Paired timing/output checks | Workstation | All eight subsets pass |
| Profile probe | Workstation | Six outcomes recorded; expected arithmetic stops keep estimates unavailable |
| Frozen files, append-only tests, source identity, scope and whitespace | Workstation | All pass |

Standalone C++ uses GCC 13 on simbox and GCC 15 on the workstation; simbox R
uses R 4.6.1 and g++-15. Long runs used the provided slot wrappers, nice 10
and single-threaded BLAS/OpenMP. No simbox exit-75 fallback was needed.
Remote source hashes before and after the final run match the final local
fingerprints. Numerical sources and tests stayed fixed during the final suites. The
final dossier prose passed the workstation site check. That checker refreshed
an unrelated archived screenshot; its original bytes were restored.

Changes are confined to bnbinom, its binding-neutral header, appended C++/R/JS
tests and this evaluation evidence. Settled d/p/q/r and special-function code,
truth tables, code generators, NEWS, status files and roadmaps are unchanged.

To reproduce from the worktree, first create `build/tmp` and set `TMPDIR`,
`JUST_TEMPDIR` and `XDG_RUNTIME_DIR` to that path, then run:

~~~sh
/tmp/umvl-cpu/simbox-run sh dev/research/evaluations/2026-11-08-bnbinom-joint-data/final-simbox.sh
/tmp/umvl-cpu/run4 sh dev/research/evaluations/2026-11-08-bnbinom-joint-data/workstation-suites.sh
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-08-bnbinom-joint-data/targeted.sh
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-08-bnbinom-joint-data/benchmark.sh
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-08-bnbinom-joint-data/profile-probe.sh
~~~

The artifact-pane display command was attempted after the report was written.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. Viewer configuration and other worktrees were left unchanged.
