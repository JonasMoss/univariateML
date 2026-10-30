# TASK-81: P2 generalized beta holds

Worktree: `p2-genbeta`; no commits or shared-source edits.

## Changes

Only README.md, mle.hpp, adapter.cpp, test.R and test.cpp in the thirteen
assigned distribution folders (61 tracked files), plus this report. Golden fixtures are unchanged.

| Families | Changed files in each folder |
|---|---|
| burr, invburr, paralogis, invparalogis, trgamma, invtrgamma, pareto2, pareto3, pareto4, nsbeta | README.md, mle.hpp, adapter.cpp, test.R, test.cpp |
| gb2, genbeta | README.md, mle.hpp, test.R, test.cpp |
| fpareto | README.md, test.R, test.cpp |

- burr / invburr: both shape arguments now accept holds. Fixed shape2 uses
  the exact Lomax transformation and its exponential-face criterion; fixed
  shape1 profiles log location and solves the concave power likelihood.
  Shape1 with shape2 held profiles scale; scale with shape2 held gives an
  explicit shape1 estimate. Reciprocation gives the inverse family.
- paralogis / invparalogis: held shape uses the Burr subfamily with both
  Burr shapes equal, with a unique log-scale score root or all values held.
- trgamma / invtrgamma: scale holds use the strictly concave conditional
  shape scores. With shape1 held, the unique power root is bracketed without a fixed
  numerical range; a near-scale regression covers powers above 1e8.
  With both shapes free a data-scaled log-power mesh nominates score
  roots and compares the held-endpoint power/Pareto face. Observations
  exactly at the held endpoint contribute an additional face penalty.
- pareto2: held shape or scale uses the corresponding Burr power-one
  profile on x - min, including all-held and constant samples.
- pareto3: held shape or scale uses Burr with shape1=1 on x - min;
  detects the constant sample at min + scale with shape free.
- pareto4: shape holds delegate to Burr on x - min.
- nsbeta: with bounds held, one free shape has a strictly decreasing
  digamma score; supports constant samples when a shape is held.
- gb2 / fpareto: retain existing holds and remove the overly broad
  constant-sample rejection for coercive one-parameter conditional fits.
- genbeta: retain shape holds and allow constant data when the held
  tangent prevents concentration, including the attained shape2=1 ridge.

Adapters preserve supplied held values exactly, record them in `fixed`,
and reject nonpositive or nonfinite held values. New arguments are documented
under `r.control`. Sample-dependent collapse and faces belong under `limits`.

## Structural exceptions

52 existing free-threshold entries changed from `not-implemented` to `no-mle`
under A4: genbeta 8, fpareto 16, pareto2 4, pareto3 4, pareto4 8, nsbeta 12.
These record the unsupported unrestricted threshold problem; they do not
claim that every restricted fixed-shape endpoint problem lacks an MLE.
There are no remaining `not-implemented` entries in this group.

## Evidence and limits

The monotone conditional roots, Lomax transformation, reciprocity and
constant-sample concentration mechanisms follow the P0 analytic arguments.
The held-scale transformed-gamma search and existing GB2/genbeta outer
searches are numerical; the meshes are not certified global searches.
P0's unresolved joint-limit classifications remain open mathematics.

Per-family R tests enumerate holds on n=3, 20 and 100 reference-quantile
samples, ties and limit-model samples, with additional at-scale/above-scale
samples in the unbounded-support families. Independent three-start nlminb
fits evaluate the reference density. Separate tests cover constant samples
and invalid held values. C++ cases cover golden holds and newly admitted
constant conditional fits.

## Final validation

- `just quality` on all 13 ids: 13 pass.
- `just cpp-test`: six CTest executables; five pass outright. The distribution
  executable has 430 cases (428 pass, 2 known failures) and 513,858 assertions
  (513,856 pass, 2 known failures). The failures are exactly betapr/test.cpp:33
  and std/test.cpp:40.
- `just test`: 13,801 passed expectations, 3 known failures, 0 warnings and
  0 skips. Failures are exactly frozen qsstd in test-dist-sstd.R, golden
  bnbinom, and golden std. Every assigned-family test and the conformance
  sweep passed. Assigned-family default goldens remain unchanged.

All three final commands ran through `/tmp/umvl-codex/simbox-run.sh`.
The C++ and R commands exit nonzero solely for the five declared machine
exceptions; there are no additional failures. Logs are in
`/tmp/umvl-codex/p2-genbeta/`: `cpp-verified-simbox.log`,
`r-complete-simbox.log`, and `quality-complete-simbox.log`.

## Open questions

- Certified global outer searches and exhaustive joint-face catalogues for
  GB2, genbeta and held-scale transformed gamma remain unavailable in P0.
- Estimated thresholds remain outside this task (A4).
- No shared change is required by this implementation.
