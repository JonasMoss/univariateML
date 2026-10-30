# TASK-81: P2 location–scale holds

Completed the missing holds for norm, cauchy, logis, laplace, gumbel, lst, std,
ged, slash, tlambda and bhatt. Huber already supported every subset; its
argument documentation and existence description were completed.
Removed all 21 `not-implemented` entries. There are 46 newly supported nonempty
hold combinations, including all-held evaluations. No entry was converted to
structural `no-mle`, and no requested hold remains unimplemented.

Files changed: `README.md`, `mle.hpp`, `adapter.cpp`, `test.R`, and `test.cpp`
inside each of the eleven folders above under
`univariateML-cpp/include/cppunivariate/distributions/`; Huber changes are confined
to `huber/README.md`. This is 56 family files, plus this report. No shared source,
reference density, golden, board or roadmap file was edited. No commits or Git
writes were made.

| Family | Newly supported held sets | Method |
|---|---|---|
| norm | mean; sd; mean+sd | Exact conditional mean and residual RMS. |
| cauchy | location; scale; location+scale | Monotone held-location scale score; grid and refinement for multimodal held-scale location. |
| logis | location; scale; location+scale | Bisection of the two monotone conditional scores. |
| laplace | mu; sigma; mu+sigma | Sample median and exact mean absolute residual. |
| gumbel | mu; sigma; mu+sigma | Strictly concave inverse-scale likelihood; exact conditional location. |
| lst | mu; sigma; mu+sigma; mu+df; sigma+df; mu+sigma+df | Exact conditional scale score, numerical location/df profiles, conditional normal and collapse comparisons. |
| std | mean; sd; mean+sd; mean+nu; sd+nu; mean+sd+nu | Ordinary-t scale conversion; conditional normal, finite-scale t2, and tie-collapse faces. |
| ged | mean; sd; nu; mean+sd; mean+nu; sd+nu; mean+sd+nu | Convex fixed-shape location profile and exact scale; numerical shape profile with Laplace and conditional uniform comparisons. |
| slash | mu; mu+sigma | Existing unique scale profile, conditioned on held mu with its own tie count. |
| tlambda | mu; sigma; mu+sigma; mu+lambda; sigma+lambda; mu+sigma+lambda | Simplex restricted to free coordinates, explicit uniform members, conditional collapse and support checks. |
| bhatt | mu; mu+sigma; mu+a; mu+sigma+a | Conditional log-concave location and scale searches; conditional normal/uniform comparisons. |
| huber | None missing | Existing complete support retained and tested. |

All supplied coefficients have exact adapter names, default to NULL except the
required Huber epsilon, are returned exactly, and appear in `fixed`. New
arguments are documented in `r.control`. Sample-dependent non-existence is
under `limits:` and README prose, rather than `holds:`. Bhattacharjee includes
the proper normal member a=0 under R7; normal-boundary fits now return it.
All golden JSON files are unchanged. Existing default numerical paths are
retained, apart from that declared attained Bhattacharjee boundary.

Proved reductions and boundaries:

- Normal and Laplace conditional profiles are closed form. Logistic conditional
  scores are monotone; the held-location Gumbel likelihood is strictly concave
  in inverse scale, and its held-scale location is explicit.
- Fixed-location t scale has a unique score root below its collapse threshold;
  the collapse exponent is `df*(n-k)-k`. At equality every positive scale lies
  strictly below the fixed-location collapse supremum. Cauchy, slash, and
  negative-shape Tukey lambda use their corresponding exact collapse bounds.
- Fixed-shape GED with nu>=1 minimizes a convex residual power sum and has an
  exact scale profile. With sd held, its forced uniform-endpoint log penalty is
  `exp(-EulerGamma)` per endpoint observation; profiling a free location at both
  forced endpoints gives `2*exp(-EulerGamma)*sqrt(k*l)`. This follows from the
  small-argument log-Gamma expansion and is independently checked in R.
- Held positive ordinary scale removes scale collapse. Held-location tie
  diagnostics count only observations at that location. Bhattacharjee's normal
  boundary is attained; its sigma-to-zero uniform face remains an escape.

Numerical rather than certified: isolation of every location or shape mode in
the multistart/grid searches, global comparisons with eligible limit faces,
and equality decisions within 1e-8*n likelihood tolerance. Independent R
optim multistarts check varied n=20 and n=100 samples, ties, n=2 samples, and
normal/uniform limit samples. Tiny-width Bhattacharjee and central slash
oracles use stable formulas checked against mixture quadrature; Tukey-lambda
inversion uses a dynamic log-probability bracket. Agreement with these searches
is not a proof that every possible sample mode has been isolated.

Final validation, all through the supplied simbox lock runner:

| Command | Final result |
|---|---|
| `just cpp-test` | 461 cases: 459 pass, 2 known failures; 524,988 assertions: 524,986 pass, 2 known failures. All five other CTest targets pass. |
| `just test` | 12,239 passes, 3 known failures, 0 warnings, 0 skips; all 1,874 conformance assertions pass. |
| `just quality norm cauchy logis laplace gumbel lst std ged slash huber tlambda bhatt` | 12/12 pass. |

The distribution C++ executable accounts for 439 cases (437 pass, 2 known
failures) and 513,941 assertions (513,939 pass, 2 known failures). The full
commands therefore exit nonzero only for the explicitly accepted failures.

Added 23 C++ test cases and 33 R test blocks. The C++ golden sweep covers 53
nonempty held subsets in the eleven changed fitters. The independent R sweep
contains 318 sample/hold cases. Generic conformance covers all 72 subsets of
the twelve families, including empty and all-held sets, within the full
repository sweep. Named regressions cover conditional lower-shape t faces,
held-location half ties, held-support feasibility, constant-sample collapse,
and the held-sd GED endpoint penalty. Held-profile regressions also check
units 1e-300 and 1e300, plus fixed t degrees of freedom 1e20 against the
independent normal residual-RMS profile. Scale brackets avoid underflow and
the t scale score avoids subtraction cancellation at large held shapes.

The five accepted machine failures were left untouched: C++ betapr/test.cpp:33
and std/test.cpp:40; R frozen qsstd, golden bnbinom, and golden std. Logs are in
`/tmp/umvl-codex/p2-locscale/`.

No blocking open question or shared change is required. Certified global mode
isolation remains open. Existing shape restrictions and conservative guards
on the old default/shape-only paths were retained; restriction flags and the
other P3 changes remain outside this job.
