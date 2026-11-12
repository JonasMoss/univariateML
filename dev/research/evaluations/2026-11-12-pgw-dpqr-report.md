# TASK-182: ordinary power generalized Weibull d/p/q/r

Implemented in worktree `pgw-dpqr`, branch `codex/pgw-dpqr`, based on
`af824a98d78f2f2325c7a675c20c84c45e6bb826`. No commits, pushes, board edits,
package installs or sub-agents were used. All final numerical and site checks passed.
Simbox was unavailable; the missing second-compiler run and the explicit
quantile failure region are recorded below.

## Delivered

The new [PGW dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pgw/README.md)
contains `README.md`, `dpqr.hpp`, `test.cpp`, `truth.py` and `truth.json`.
There is no fitter or golden sample. The front matter declares `stage: dpqr`,
positive coefficients in the order `scale`, `shape`, `exponent`, support and
symbolic log density, without fit controls, fit certification or fictitious
optimizer outcomes. The schema now documents this probability-only stage
and descriptive distributional limits without likelihood-detection fields.

For scale \(a\), shape \(b\), exponent \(c\), the implementation uses

\[
H(x)=\operatorname{expm1}\!\left(c\log\{1+(x/a)^b\}\right),
\qquad S(x)=e^{-H(x)}.
\]

Weibull is recovered at \(c=1\); exponential, with rate \(1/a\), at
\(b=c=1\). The canonical directed relation is consequently added to the
Weibull dossier: `weibull -> pgw`, mapping `exponent: 1`. Exponential already
has a Weibull special-case relation, so no redundant transitive edge is
added. The PGW dossier states and links both reductions. Its two descriptive
boundary records give the proved power-transformed Gompertz and
lower-threshold conditional Weibull paths from the
[theory report](2026-11-11-pgw-theory-report.md#proved-boundary-paths).
Those proofs concern distributional limits; they do not establish that these
paths exhaust likelihood escapes or attain the largest boundary likelihood.

The R generator now includes staged families in its internal d/p/q/r layer
without manufacturing ML adapters or exported fitter APIs. Its truth consumer
checks them directly and only tests fitted-object dispatch when fit metadata
exists. JavaScript generation exposes evaluation-only types and coefficient
metadata, while keeping the fitter registry unchanged. This also makes the
existing probability-only exponentiated Weibull available to these consumers.
Dedicated PGW R and JavaScript cases cover tiny natural survival probabilities,
inverse failures, exact centers and subnormal parameters.

The current site catalogue and explorer require fitters. Site preparation
therefore uses `fitFamilies` for explorer coverage and omits canonical links
to pending probability-only family pages from its presentation data. PGW has
no fitted-family page or fitting explorer yet. Canonical relations remain
available to the independent relation checker. Site validation passes without
adding misleading fit metadata to PGW.

## Numerical implementation

The scalar C++ API supplies `dpgw`, `ppgw`, `qpgw` and a one-uniform inverse
sampler `rpgw`. It uses the existing `special/tails.hpp` interface; no shared
C++ core or special-function implementation changed.

The power is formed in logarithmic coordinates, preserving displacement near
the scale with `log1p`. Reciprocal extreme shapes and exponents are regrouped
before an intermediate multiplication can overflow. For a small power,
binary exponent decomposition and compensated quotient residuals retain a
hazard that would otherwise be lost by rounding an underflowed power. The
large-shape branch uses a corrected half power before squaring. In the branch
\(b\log(x/a)<-36\), replacing \(\log(1+t)\) by \(t\) has relative error
below \(1.2\times10^{-16}\); this is a mathematical approximation bound,
not an interval certificate for the complete floating-point computation.

Probability evaluation retains the logarithmic hazard and computes each tail
through the existing tail helpers. It does not obtain survival by subtracting
the CDF from one. The quantile uses the closed-form inverse through `log1p`
and `expm1`, with separate endpoint, extreme-tail and subnormal-input branches.
For subnormal lower-tail log inputs, binary scaling and FMA avoid subtracting
two large logarithms before division by a small coefficient.

At zero the density is infinite for \(b<1\), zero for \(b>1\), and \(c/a\)
for \(b=1\). Negative observations have zero density and lower-tail
probability; positive infinity and probability endpoints are handled
explicitly. Invalid coefficients and invalid probability inputs return NaN.

### Explicit quantile limitation

The inverse conservatively returns NaN when its computed log power satisfies
\(|\log((x/a)^b)|<0.2\) and \(b<10^{-4}\). In that region, division by
a tiny shape can amplify uncertainty in the near-unit power beyond the
quantile accuracy contract. It returns a rounded zero or infinity only when
both sides of a practical \(10^{-12}\) absolute log-power margin imply that
same endpoint, with additional distance from the representable boundary.
Upper log inputs exactly equal to \(1-2^c\), for integer \(c\le53\), retain
the exact quantile \(a\).

For example, upper log probability `-3.000000004`, scale `1e-308`, shape
`1e-12`, exponent `2` has a finite reference quantile approximately
`1.9700139198778433748958628982982474e126`, but this implementation reports
NaN. The R shell emits its standard `NaNs produced` warning. The inverse
sampler inherits this numerical failure. This behavior and region are
documented in the dossier and tested across languages; no unchecked finite
quantile is substituted.

## Independent evidence

The 80-digit mpmath truth table contains **25 parameter settings, 469 value
rows and 1,100 inverse rows**. It includes ordinary parameters, values close
to \(c=1\), extreme shapes and exponents, natural and logarithmic inputs in
both tails, log probabilities down to `-10000`, and both boundary-law regimes.
The harness checks CDF differentiation against the declarative log density
and integrates that density in cumulative-hazard coordinates. Its integration
ends at hazard 256, with omitted mass \(e^{-256}\). All 25 normalization
checks passed.

Two retained scripts provide additional evidence from exact binary64 inputs:

- [edge-references.py](2026-11-12-pgw-dpqr-data/edge-references.py) evaluates
  reciprocal extreme coefficients, subnormal exponents, and inverse examples
  at 1,000 decimal digits; its results are in the adjacent JSON file.
- [check-ranges.py](2026-11-12-pgw-dpqr-data/check-ranges.py) compares 24 natural
  upper-tail probabilities near \(10^{-304}\) at 1,000 digits. It covers scales
  near `1e-300` and `1e300`, exponents near `1e308`, huge shapes with inexact
  ratios near the scale, and the Weibull reduction. All 24 passed the
  `1e-10` relative check; maximum relative error was
  **9.859709502968935e-13**.

These are independent high-precision numerical comparisons, not exhaustive
proofs or outward-rounded bounds. Boundary convergence tests supplement the
analytic paths in the earlier theory report. Sampler tests verify uniform
replay and probability-integral-transform moments.

The [source audit](2026-11-12-pgw-dpqr-data/source-audit.json) confirms the
truth-table dependency fingerprint, the stress-check header hash and the
byte-identical generated R header. Final PGW header SHA-256:
`aa4a606c2a3f093f2e808a5696ff615410dea052fd5ae916058809dc93bc583b`.

## Final validation

Every executed build, test and numerical script used the shared CPU wrappers
on the workstation, with thread limits and `nice` supplied by those wrappers.
Heavy suites used `run4`; single-threaded evidence, relation checks and focused
checks used `run`. Simbox attempts returned 75 before execution, so the required
fallbacks ran locally. Logs and machine/exit records are retained in
[validation-summary.json](2026-11-12-pgw-dpqr-data/validation-summary.json) and
[final-logs/](2026-11-12-pgw-dpqr-data/final-logs/).

| Check | Machine | Final result |
|---|---|---|
| `just cpp-test` | Workstation, GCC 15.2 | 109/109 shards passed |
| `just cpp-test-no-long-double` | Workstation, GCC 15.2 | 136/136 shards passed |
| `just cpp-test-no-long-double` attempt | Simbox | Unavailable, exit 75 |
| `just test` | Workstation after simbox exit 75 | FAIL 0, WARN 0, SKIP 155, PASS 29119 |
| `just test-full` | Workstation after simbox exit 75 | FAIL 0, WARN 0, SKIP 0, PASS 58341 |
| `just js-build` | Workstation | Passed |
| `just js-test` | Workstation | 627/627 tests passed; no skips |
| `just site-check`, final rerun | Workstation | Passed: 100 pages, graph, MathML, links, browser checks and 100 C++ snippets |
| `just relations`, final rerun | Workstation | 158 edges; zero failed |
| Rebuild `distribution_tests`; CTest `distribution_pgw` | Workstation | Passed |
| Focused PGW/Weibull R rerun | Workstation | FAIL 0, WARN 0, SKIP 1, PASS 39 |
| Truth fingerprint, evidence hash, generated R header audit | Workstation | Passed |
| Local destinations in the four changed Markdown files | Workstation | Passed; zero broken destinations |
| `git diff --check` | Workstation | Passed |

The first C++ dev build compiled PGW before its final tail fix. The current
11-case PGW test source subsequently passed **19,857 assertions** in a
standalone normal-precision run, and the aggregate target was rebuilt and its
PGW shard rerun successfully at the end. The complete no-long-double build
and both complete R tiers used the final kernel, as did the successful WASM
build and JavaScript suite.

The initial site check exposed its assumption that every bound family has a
fitter; the initial relation check rejected two conditional edges that relied
on random draws hitting exact parameter equalities. Both were corrected as
described above. The final changes concerned dossier relations and site
presentation only; site and relation checks and focused PGW/Weibull R checks
were rerun. The numerical kernel and shared binding generators did not change
after the complete R/JavaScript suites passed. Initial diagnostic logs are
retained alongside successful final logs. Long wall times in the JSON include
CPU-lock queues; the CTest executions themselves took 372.79 seconds for dev
and 412.61 seconds for no-long-double.

No existing fitter, default fitted result, golden sample or other family's
probability implementation was changed. The existing Weibull dossier changed
only to record its PGW parent relation.

## Open entries

1. **Finite quantiles inside the documented cancellation region:** a more
   accurate inverse or a sharper error bound is needed to return them instead
   of NaN. The concrete finite example above and its 1,000-digit reference
   identify a reproducible target. Sampling inherits the same limitation.
2. **GCC 13 cross-machine comparison:** simbox returned exit 75. The mandatory
   workstation no-long-double suite passed, but no second-compiler result is
   claimed. A reachable simbox is required to close this entry.
3. **Artifact-pane display:** the requested `bin/show` invocation returned
   HTTP 403: this worktree is outside the viewer's configured readable roots.
   The report exists at the requested path and can be linked directly. No
   configuration outside this worktree was changed; adding this worktree to
   the viewer's readable roots is needed for pane display.
4. **Repository-wide Markdown link inventory:** the optional
   `python3 dev/tools/check_links.py` check reports 45 broken destinations in
   seven files unchanged from HEAD, including code-pattern false positives
   and missing local literature. No reported destination is in a file changed
   by this job. The focused check of all four changed Markdown files passes;
   the [link audit](2026-11-12-pgw-dpqr-data/link-audit.json) records the
   unchanged-file comparison and the full checker log is retained. Repair of
   those unrelated documents was not undertaken.

Fitting, inference implementation, likelihood-face classification and adding
probability-only families to the site's fitting-oriented catalogue remain
outside this d/p/q/r job.
