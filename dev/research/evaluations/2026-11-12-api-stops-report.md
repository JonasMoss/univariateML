# TASK-150.4: shared accuracy-stop classification

Baseline: `0019c6b3`. Worktree: `api-stops`, branch `codex/api-stops`.
No commits, pushes, board changes, package installations, or other worktree edits
were made. All commands started for this job have finished.

The core now carries `AccuracyStop::{None,Tolerance,Conditioning}`,
`attainable_tol`, and `conditioning_bound` in `StopDetail`. Common bindings
route the classification to R and JS and retain finite candidates in public
coordinates. NaN, overflow, invalid projected scales, and failed searches
without a conditioning diagnostic remain numerical failures. Existing family
status enums and side flags remain compatible; consumers use the shared kind.

Two requirements remain open: exact pointwise minimum tolerances for general
nonlinear roots, and the independent simbox/GCC 13 no-long-double run. The
former is a mathematical limitation of the generic spacing diagnostic, described
precisely below. The closed-ratio regression does report its actual minimum.

## Interface and diagnostic evidence

`bindings/common/fit.hpp` exposes error kinds `tolerance` and `conditioning`.
R conditions inherit, in order, `univariateML_tolerance` or
`univariateML_conditioning`, then `univariateML_numerical`,
`univariateML_stop`, `error`, and `condition`. Existing numerical-error catches
therefore continue to work. JS exposes the same kind strings and numeric
`attainable_tol`, including through the throwing facade and TypeScript type.
Tolerance messages include `tol must be at least ... here` at 17-digit precision.
Control names have not changed.

The shared recorder excludes held coordinates, works after conversion to public
coefficient units, and retains a finite coefficient/likelihood witness. Invalid
positive-scale projections do not become tolerance stops. Existing candidate
likelihood enclosures are preserved when the same witness is already present.

The general automatic-differentiation polish diagnoses conditioning only after
the existing refinement fails. It subtracts the Hessian's predicted score change
from symmetric small perturbations, maps the residual through positive-definite
observed information, and requires the remaining Newton correction to be covered
by four times that measured uncertainty. A large unfinished correction, a
nonpositive information matrix, or a cap by itself does not establish
conditioning. This is a practical floating-point diagnostic conditional on the
computed derivatives, not a rigorous interval bound.

The two-piece refinements propagate their existing score-noise calculations.
The Burr pair refinement separates rounding uncertainty from unresolved tail
error before assigning conditioning. Beta-binomial propagates its existing
score summation uncertainty through inverse information, with its practical
margin. Folded normal retains its quartic-transition uncertainty. None of these
changes constitutes a new global-optimality proof. Successful refinement
criteria and ordinary successful fitting arithmetic were retained.

The [native binding replay](2026-11-12-api-stops-data/replay.txt) demonstrates:

| Case | Kind | `attainable_tol` | Witness |
|---|---|---:|---|
| ZIB, observations `(0,1,1)`, size 1, inflation held at 0, requested `1e-20` | tolerance | `3.7007434154171883e-17` | probability `2/3` rounded to double |
| Folded normal, `(1,1,1,sqrt(9+4*sqrt(6)))`, default tolerance | conditioning | `1.9023582291494466e-16` | finite folded-normal candidate |

For the first row, the exact absolute representation error is
`1/(3*2^53)`: this is algebraic evidence, rather than a golden comparison. The
second row tests the dossier's quartic-transition analysis and the implemented
floating-point margin numerically. Its displayed `attainable_tol` is the
representation-resolution diagnostic; the separate conditioning bound causes
the default-tolerance failure. C++, R, and JS regressions cover both kinds and
the closed-ratio attainable value. A subnormal folded-normal scale regression
also checks that evaluation breakdown is numerical and carries no zero-scale
own-family candidate.

## Per-family site inventory and decisions

The reproducible [baseline inventory](2026-11-12-api-stops-data/inventory.json)
contains 160 source nominations, including declarations, helpers, and binding
branches; these are not 160 independent mathematical decisions. Its
[script](2026-11-12-api-stops-data/inventory.py) reads the baseline with
`git show`, rather than storing source archives. The table includes additional
accuracy-related `NumericalFailure` and delegate sites inspected beyond the
original marker list. Paths are under
`univariateML-cpp/include/cppunivariate/distributions/<id>/`, and binding routing
is under `bindings/common/distributions/<id>/fit.hpp` where applicable.

Here, **spacing** means the public-coordinate resolution classification subject
to the open pointwise-minimum limitation below. **Noise** means a propagated or
measured score/curvature diagnostic. Both retain a finite best available witness;
neither turns a certificate-only inconclusion into a new stop.

| Family | Sites and decision |
|---|---|
| bbinom | `accuracy.hpp` inverse-information noise and `mle.hpp` failed polish: spacing or noise, with size excluded and held shapes masked. |
| bnbinom | Failed final local refinement: spacing or measured noise, with shape holds masked. A refinement that contradicts the certificate stays numerical; restore the searched coefficients paired with their likelihood rather than labelling the disagreement tolerance. |
| emg | Certificate accuracy flag, failed AD polish, and final projection: spacing or noise. Nonfinite evaluations and cap failures remain numerical. |
| foldnorm | Quartic-transition ambiguity carries its existing uncertainty and becomes conditioning at the default. Root/projection failures use spacing when applicable; a failed root without a noise bound remains numerical. Restore physical best coefficients and likelihood instead of NaN placeholders. |
| gb2 | Beta-reduction input-rounding/information gate, failed two-piece polish, and public scale projection: spacing or propagated noise. Restore the best public point before recording a failed refinement. Power/scale transports record after restoring public units and Jacobian. |
| genbeta | Final local refinement and physical projection: spacing or noise. Preserve the diagnostic while rebuilding the original-unit stop payload; the structural scale hold is excluded. |
| gumbel | Accuracy-return and held-scale location projection: spacing, or numerical where the search supplied no noise bound. Append a payload without changing old aggregate positions. |
| ht | Failed AD polish and scale projection carry the fixed existing accuracy target and any noise evidence; location/held coordinates are excluded. No new control was introduced. |
| lomax | General and held-shape root/projection failures record public rate/shape candidates. Affine recursion rescales candidates and likelihoods. Spacing classifies tight requests; failures without a noise diagnostic remain numerical. |
| lst | Fixed-df and general final t-family refinements: spacing or noise. In the fixed-df profile both location and scale remain free. Restore physical units before recording. |
| pareto3 | Failed refinement and final shape/scale projection: spacing or noise. Held threshold is excluded; nonfinite scale/likelihood remains numerical. |
| pig | Final coefficient-accuracy rejection: spacing at the finite mean/shape witness, with held mean excluded. No noise bound is invented for other search failures. |
| sged | Certificate and final restricted/unrestricted local refinements: spacing or noise. A refuted restriction is numerical, independent of a tiny requested tolerance. Mask held parameters and preserve the certificate's physical candidate. |
| slash | Local polish and scale projection: spacing or noise; held location/scale excluded. Pure search failure without evidence stays numerical. |
| sn | Failed AD polish and public projection: spacing or noise, with location/scale/skew holds masked. |
| sstd | Existing local noise test now fills shared detail; full/subset polishes and public projections propagate it. Spacing excludes held coordinates. |
| st | Full/subset skew-t refinements and public projection use the same two-piece diagnostics. Restore the best physical point on failure and preserve held values. |
| std | Final t-family refinement: spacing or noise; relative df and public location/scale witnesses retained. |
| weibull | Root/projection accuracy exits now retain shape/scale and detail, with free-coordinate masks. Spacing applies to tight requests; no unmeasured default conditioning claim is added. |
| zib | Final local/representation gates use shared detail. The size-one binomial face uses the actual count-ratio rounding error, with held probabilities excluded. Integer size is excluded. |
| zmbinom | Propagates its ZIB/binomial probability diagnosis and combines it with the actual zero-mass count-ratio rounding error. Integer size is excluded. |
| ztbinom | Final probability accuracy marker records public size/probability, excluding enumerated size. No cap-only conditioning classification. |
| betapr | `fallback.hpp` and `gamma_accuracy.hpp` propagate the Burr-pair or gamma-polish noise and public coefficient projection. Preserve compatible old helper overloads. |
| burr | Pair and AD fallback helpers propagate noise separately from unresolved tails; final physical projection records public shape/scale. Successful uncertified fallback behavior is preserved. |
| invburr | Preserve the diagnostic across reciprocal payload assignment, then record the reciprocal public witness. Do not overwrite the new accuracy diagnosis with the transported certificate. |
| pareto4 | Burr delegate carries shared diagnosis after appending the held threshold to own-family candidates. |
| pareto2 | Lomax delegate carries the diagnosis after reciprocal rate-to-scale conversion and masks supplied holds. An unrepresentable reciprocal scale is numerical, including at a tight tolerance. |
| invpareto | Lomax accuracy stops now use structured routing with the delegate's retained candidates and shared kind. |
| fpareto | GB2 delegate preserves diagnostic detail through its public-coordinate transformation. |
| ged | Final AD accuracy refinement propagates noise and public coefficient representation evidence. |
| gev | Local AD refinement and final physical coefficient gate propagate diagnosis; independent certificate/search failure without it remains numerical. |
| frechet | Gumbel/GEV accuracy helpers and certificate/local refinement carry the public-coordinate diagnosis. |
| bhatt | AD and pair-variance refinements carry diagnosis. Outer certificate rejection previously dropped a diagnosed local tolerance stop; it now preserves the kind, attainable value, noise, and valid local witnesses alongside certificate witnesses. New C++/R regression and the existing tightened JS assertion verify this handoff. Certificate-only unresolved bounds remain numerical. |
| paralogis | Burr pair/AD local refinement overloads carry detail to the final public coefficients; spacing or noise as supported. |
| invparalogis | Reciprocal public candidates preserve diagnosis. Invalid reciprocal shape/scale or likelihood is numerical; a core `Ok` with infinite scale is rejected. |
| sgomp | Final shared AD polish carries diagnosis and a finite public witness; ordinary search failure remains numerical. |
| skellam | Final local AD polish carries diagnosis, respecting held means. |
| zinb | Negative-binomial local polish carries shared noise/projection detail and public probability/shape witnesses. |
| ztnbinom | `accuracy.hpp` NB-polish overloads preserve detail through conditional/profile paths and final binding routing. |
| tbinom | Existing fallback local refinement now carries detail at its existing `1e-10` target; no API/control change. |

## Two invalid reciprocal successes

Inspection of the diagnostic transports exposed two core successes with infinite
public scales for observations `(1e308,1.1e308,1.2e308)`: Pareto II with shape
held at 100 and threshold 0, and inverse paralogistic with shape held at 0.5.
These are evaluation/projection breakdowns, not accuracy stops. The
[baseline/current replay](2026-11-12-api-stops-data/transports.txt) reproduces
them from `0019c6b3` with its [script](2026-11-12-api-stops-data/transports.sh).
The baseline is reconstructed under ignored scratch using `git archive`; no
source tree is archived in the evaluation data.

Pareto II now returns the existing iteration/arithmetic failure and a numerical
binding stop. Inverse paralogistic returns its existing numerical-failure status
instead of `Ok`. Both remove invalid own-family candidates, preserve legitimate
non-success statuses, and have C++, R, and JS regressions at default and tight
tolerances. This deliberately changes invalid `Ok` outputs, not a valid default
MLE or a golden sample. Goldens were not regenerated or edited.

## Validation and late fixes

All runs used the workstation, through the required slot wrappers, with
`nice -n 10` and single-threaded BLAS/OpenMP. Simbox attempts for C++ dev, R dev,
R full, and no-long-double returned 75 and were rerun locally through `run4`.
The workstation compiler was GCC 15.2.0; JS used Emscripten. Compact log tails
are retained in [validation.txt](2026-11-12-api-stops-data/validation.txt).

| Run | Result |
|---|---|
| `just test-changed --dry-run` | Shared headers selected all transitive family dependants. |
| `just cpp-test` | 108/108 dev shards passed. |
| `just test` | 25,831 passed; 0 failures, 0 warnings; 154 skips. |
| `just test-full` | Sweep completed: 52,595 passed, 0 warnings, 0 skips; four assertions failed solely because the newly added inverse-paralogistic test used `reltol` instead of its existing `gradtol`. Corrected and rerun as below. |
| Late Pareto II / inverse-paralogistic C++ checks | 2/2 shards passed after the reciprocal-scale guards. |
| Late Pareto II / inverse-paralogistic R checks | 71 passed; 0 failures/warnings; 4 full-tier skips. Includes both corrected overflow/tolerance test calls. |
| Late Bhattacharjee C++ check | Family shard passed with the appended fallback-classification regression. |
| Late Bhattacharjee R full-tier file | 324 passed; 0 failures, warnings, or skips. |
| `just js-build` | Passed after the Bhattacharjee payload fix. |
| `just js-test` | Final run: 618/618 passed, no skips; 238.5 seconds. |
| `just cpp-test-no-long-double` | Final workstation run: 135/135 passed; 648.2 seconds test time. Includes full-labelled slow shards and all late C++ changes. |
| `just site-check` | Passed, including 99 generated C++ snippets, catalogue checks, desktop/mobile browser checks, and build-time MathML checks. |
| Native evidence replays and baseline inventory | Regenerated successfully after validation. |
| `git diff --check` | Passed. |

The first JS suite passed 617/618 tests and exposed the Bhattacharjee diagnostic
being dropped at the certificate handoff. The family-level payload fix was
followed by its C++ and full-tier R checks and a complete green JS rerun.
The two reciprocal guards and the R control-name correction were also family
changes made after the earlier dev snapshots. As requested for late family-only
fixes, the affected C++/R files were rerun instead of repeating the complete
R sweep. Thus the recorded full R sweep itself has the four historical test-call
failures; the final targeted checks resolving them are green. No shared core or
common-binding change was made after those full/dev snapshots.

Two preflight compilation issues were corrected before the final dev run: a
fixed-df LST mask used an unavailable variable, and tbinom's fallback used an
unavailable tolerance variable instead of its existing fixed target. They did
not require changing any successful-fit expectation. Existing tests were not
deleted or weakened to accommodate stops. The build logs include an existing
binomial self-comparison compiler warning and workstation shell-startup messages
about a missing Cargo environment file; R test warning counts are zero.

The checks are regression, interface, transport, and diagnostic evidence. They
are not independent high-precision MLE oracles for every affected family, and
they do not establish new global certificates.

## Open items and out-of-scope handoffs

1. **Exact pointwise minimum tolerance outside closed ratios.** The generic
   helper computes normalized adjacent-double spacing at the retained public
   candidate. That is a conservative resolution threshold used to classify an
   existing failed accuracy check; it is not the actual distance between the
   unknown real maximizer and its closest double. An exactly representable
   maximizer can have zero representation error. The ZIB/ZMB closed ratios use
   their actual rounding errors, but the generic `attainable_tol` does not meet
   the stronger “smallest attainable” interpretation for every nonlinear
   family. Closing this requires those refinements to retain root enclosures
   narrow enough to resolve rounding cells, or reliable extended-coordinate
   target-error bounds, and regressions with requests between the actual
   distance and the conservative spacing threshold. It cannot be justified
   merely by measuring a candidate's ULP. Both design documents explicitly
   distinguish the current diagnostic from that unproved impossibility claim.
2. **Independent GCC 13 run.** Simbox was unreachable (exit 75). The full
   workstation no-long-double suite passed, but it does not substitute for
   `just cpp-test-no-long-double` on simbox when that machine returns.
3. **gengamma handoff.** This worktree has no `gengamma/mle.hpp`; the other
   session's forthcoming fitter must populate the shared enum and attainable
   value at its accuracy decisions, retain public shape/scale candidates,
   propagate noise through coordinate transformations, use common structured
   routing, and add binding regressions. Evaluation breakdown remains numerical.
4. **cmb handoff, not edited.** Its log-odds-to-probability projection and the
   returned-coordinate moment-score gates in `mle.hpp` (around lines 131–138),
   plus local polish, need shared accuracy diagnostics. Distinguish a tight
   probability-representation request from score/table-noise conditioning;
   exclude integer size and held coefficients and retain the existing best
   finite public witness. Enumeration exhaustion alone is not conditioning.
5. **dgamma handoff, not edited.** The final `accuracy_detail::polish` call in
   `mle.hpp` (around line 313) currently uses the compatibility overload that
   discards diagnostic detail. Pass its result's `stop_detail`, record the
   shape/rate candidate with holds excluded, and route the kind through the
   binding. Nonfinite incomplete-gamma evaluation remains numerical.

No d/p/q/r implementation, `special/` header, truth table, code generator,
generated d/p/q/r source contract, control name, or forbidden project-status
file was changed. Family-specific dossier prose changed only for folded
normal's conditioning transition. Shared interpretation is documented in
`dev/design/structured-stop-details.md` and `dev/design/accuracy-contract.md`.
