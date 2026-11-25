# rvinecopulib compatibility proposal

TASK-248 proposes a test-only change against rvinecopulib `main` at
[`2ac9d35c1bc25ba310f9e65f32579229b2dbcfd0`](https://github.com/vinecopulib/rvinecopulib/commit/2ac9d35c1bc25ba310f9e65f32579229b2dbcfd0).
The change is ready for review. [Patch](2026-11-25-revdep-pr-rvinecopulib/rvinecopulib.patch),
[proposed title and body](2026-11-25-revdep-pr-rvinecopulib/pr.md),
[test results](2026-11-25-revdep-pr-rvinecopulib/results.md).

## Rayleigh test

The old fixture reached rvinecopulib's fitted-margin guard by fitting
Rayleigh to a sample containing zero. The CRAN build returned
`logLik = -Inf`. The upcoming build raises `univariateML_invalid` with
`kind = "invalid"` and `reason = "invalid-input"` because the likelihood
is zero for every scale. rvinecopulib already records that error and its
message as a candidate failure.

The replacement fixture uses the existing custom-margin protocol. It
constructs a valid margin, sets its reported log-likelihood to `-Inf`,
and returns it so the candidate validator sees the invalid metadata.
The three original checks are retained. Selection errors if this is the
sole candidate, warns when it can fall back, and returns the good
candidate in that case. The changed expectations passed with both
univariateML builds. The full suite passed with CRAN 1.5.0, and the
upcoming build went from four failures to two.

Accepting either Rayleigh message would exercise only the fitter refusal
on the upcoming build. A mocked univariateML fitter would exercise the
guard and keep the test coupled to the optional adapter. The custom
fixture exercises the guard directly with real margin selection. The
nearby optimizer-error test already covers reporting a fitter exception
and continuing with another candidate.

## Discrete support and the ordered-binomial follow-up

The named family adapter reads `attr(metadata$support, "type")` and maps
`"Z"` to `"d"`. The upcoming build stores `"Z"` in the Poisson support
list's `type` element and the attribute is absent. Restoring the
attribute in memory cleared both known failures, matching the planned
univariateML shim. [Upstream support lookup](https://github.com/vinecopulib/rvinecopulib/blob/2ac9d35c1bc25ba310f9e65f32579229b2dbcfd0/R/margins.R#L790-L804)

**Open — omitted binomial size.** The same vine test then reaches an
ordered factor fitted through the named `"binom"` adapter. The adapter
calls `mlbinom(x)`, and the upcoming design-parameter rule requires
an explicit `size`. On this sample, both `size = NULL` and `size = 4`
reproduce the CRAN estimates. A diagnostic that restored the support
attribute and supplied `size = NULL` for omitted binomial sizes passed
all 1,224 expectations. The release decision is whether univariateML
preserves the omitted-size call or rvinecopulib gains a way to specify
`size` for the named adapter. The diagnostic establishes current
behavior on this suite. It leaves the scope of either compatibility
change open. [Ordered-factor fixture](https://github.com/vinecopulib/rvinecopulib/blob/2ac9d35c1bc25ba310f9e65f32579229b2dbcfd0/tests/testthat/test_vine.R#L586-L604)

The patch is a mail-formatted working-tree diff with the upstream base
commit in its footer. The no-commit instruction precluded generating it
with `git format-patch`. Forward and reverse `git apply --check` both
passed.
