# TASK-81: p2-halfthreshold

Worktree: `codex/p2-halfthreshold`, based on master `79789d3`.
No commits, board edits, package installs, or sub-agents.

## Scope and files

Changed `README.md`, `mle.hpp`, `adapter.cpp`, `test.R`, and `test.cpp`
in each of `hnorm`, `hcauchy`, `fatigue`, and `betapr` (20 family files).
`ht` and `foldnorm` already support every existing coefficient hold;
their implementations and existing tests remain unchanged and are validated.
No shared source or golden files were edited. This report is the only
additional tracked-path file.

## Holds

- hnorm: `sigma`, alone or with `mu`. Exact endpoint/RMS profiles in C++;
  held scale allows constants and singleton samples. Both held is evaluation.
  Nonfinite likelihoods from the inherited R endpoint representation stop
  with a precision diagnostic.
- hcauchy: `sigma`. Direct likelihood evaluation through the existing
  fixed-nu half-t path; zero multiplicity causes no collapse with scale held.
- fatigue, with `mu` held: `alpha`, `beta`, and both. Held beta gives the
  exact RMS alpha solution; held alpha uses a bounded log-beta profile,
  refining all sampled maxima. Both held is evaluation. Constant samples
  collapse only with both alpha/beta free, or with alpha free and every
  shifted observation equal to held beta. New hold paths use status enums
  for invalid input, collapse and failed searches.
- betapr: `shape1`, `shape2`, both shapes, either shape plus scale, and
  all coefficients. At fixed scale, a free shape solves its monotone
  digamma score. Both shapes held gives a unique log-scale score root.
  A single held shape profiles the other shape and compares sampled
  finite scale maxima with the appropriate gamma/inverse-gamma face,
  retaining the held shape in that face. Failed roots/searches stop.
- All new coefficient arguments default to NULL; held values are returned
  exactly and included in `fixed`. New arguments and existence conditions
  are documented; implemented `not-implemented` entries were removed.

## Structural exceptions and deferred work

No newly implemented hold was mathematically classified universally no-MLE.
Fatigue `mu = NULL` remains rejected under A4, including when alpha/beta
are held; its four exact held subsets are declared `no-mle`. This is the
requested policy exclusion: P0 establishes existence for some restricted
free-threshold subsets, so the README distinguishes policy from mathematics.
Translations of hcauchy/ht remain P4 work.
The hnorm C++ endpoint is exact; its R adapter retains the existing
predecessor-of-minimum convention pending the shared P3 closure migration.
The default estimation branches and all golden files remain unchanged.

## Evidence and limits

P0 proves the hnorm endpoint/RMS solutions, the fixed-shape beta-prime
score uniqueness, and the stated held-threshold fatigue existence results.
The fatigue fixed-alpha search has analytic exterior bounds: individual
stationary beta/t ratios satisfy y=1 or (y+1)^2=alpha^2*y.
Within those bounds it is a numerical grid/refinement search.
Beta-prime free-scale closure and global profile optimality retain P0's
qualification: multistart agreement is evidence, not a global certificate.
The new profile has explicit numerical shape/scale ranges and stops for
unresolved winning edges; face comparisons use tolerance 1e-8*n.

New R tests use independent reference densities and multistart nlminb,
plus exact evaluations for fully held fits. Samples include n=20 and 100,
ties, constants, singletons/small n, normal/gamma/inverse-gamma limit data,
invalid held values, collapse, face selection, and short iteration limits.
C++ additions exercise each new subset, exact profiles and face statuses.

## Final validation

C++ `just cpp-test`: 6 CTest targets; 5 pass, and distribution_tests has
only the two explicitly allowed machine failures (betapr/test.cpp:33 and
std/test.cpp:40). Distribution suite: 421 cases, 419 passed; 513,722
assertions, 513,720 passed. All five newly added C++ cases pass.
R `just test`: 11,034 passed assertions, 3 failed, 0 warnings, 0 skips.
The only failures are the allowed frozen sstd reference and bnbinom/std
goldens. Conformance: 1,756 passed assertions. Four new R test blocks were
added. All six family files pass: betapr 316, fatigue 149, foldnorm 257,
hcauchy 64, hnorm 122, ht 73 assertions (981 total).
`just quality hnorm hcauchy ht foldnorm fatigue betapr`: 6/6 passed.
All three final commands ran on simbox through the supplied slot-lock
runner and finished. C++ and R return nonzero solely for the five allowed
machine-dependent failures; no tests were suppressed or fixtures changed.
`git diff --check` passes; all six golden.json files are unchanged.
Logs: `/tmp/umvl-codex/p2-halfthreshold/`.

## Open questions

Certified global profile optimization and beta-prime closure completeness
remain mathematical questions. The shared hnorm endpoint/density closure
is P3 work and cannot be changed within this job's file scope.
