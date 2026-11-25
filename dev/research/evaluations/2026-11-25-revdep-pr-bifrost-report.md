# Bifrost reverse-dependency PR proposal

TASK-247 proposes a test-only patch against bifrost `main` at
`6df4b209e7014b815db2b9455292469df6ccc4e4`.
[Upstream commit](https://github.com/jakeberv/bifrost/commit/6df4b209e7014b815db2b9455292469df6ccc4e4)

The original test used `rep(2, 8)` to exercise bifrost's finite-statistics
guard. CRAN univariateML 1.5.0 returned infinite Normal log-likelihood and a
finite Cauchy fit. For either location–scale family, setting location to
the observed constant makes the density at that point proportional to
the inverse scale. The likelihood therefore diverges as scale tends to
zero. The upcoming build refused both direct fits with
`univariateML_nomle`, `kind = "nomle"` and `reason = "unbounded"`.

The patch reuses a nonconstant sample from the test and temporarily mocks
`univariateML::model_select()` for Normal. The real Cauchy fit still goes
through model selection. All original assertions about the winner, rankings,
failure record, and all-non-finite error remain. The mock follows the
existing same-file test style.

Bifrost already records `conditionMessage()` in `failed$reason` and continues
with the next candidate. A separate probe on the constant sample with
`models = c("norm", "exp")` verified that both builds select exponential.
Bifrost recorded non-finite Normal statistics on CRAN 1.5.0 and the complete
Normal error message from `model_select()` on the upcoming build. An extra
integration test could assert that a failed Normal candidate leaves
exponential eligible across versions. The proposal keeps the original
coverage and uses the probe as evidence for leaving the handler alone.

The changed file and full installed-package testthat suite passed on the
workstation with both builds after the patch. Each failed once on the
unpatched upcoming build. The expanded source suite passed with CRAN 1.5.0
and surfaced a separate seeded-bootstrap assertion on the upcoming build.

**Open empirical fixture.** With the repository fixtures enabled, the
avian-skeleton test's Gumbel bootstrap upper bound rounds to 0.144 on the
upcoming build and 0.142 on CRAN 1.5.0. The failure reproduces on the original
test file. Replaying the legacy Gumbel sampler with the upcoming fitter
restores 0.142 at the asserted precision. Bifrost needs to decide whether
this manuscript regression should pin its sampler or accept variation
between valid bootstrap streams. The proposed patch covers the
constant-data fallback only.

**Open patch export detail.** `git format-patch --stdout HEAD` emitted zero
bytes because the proposed edit is uncommitted. The deliverable uses a
mailbox-formatted Git diff with the base commit recorded in its footer.
`git apply --check` passed on the original tree, and the reverse check
passed on the patched tree.

- [Mailbox patch](2026-11-25-revdep-pr-bifrost/bifrost.patch)
- [PR draft](2026-11-25-revdep-pr-bifrost/pr.md)
- [Before-and-after results](2026-11-25-revdep-pr-bifrost/results.md)
