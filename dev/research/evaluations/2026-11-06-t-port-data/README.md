# TASK-140 native port evidence

The [arithmetic note](arithmetic.md) records the exact identities, practical
margins, proof checks and significant rejected approaches. The final report
is `../2026-11-06-t-port-report.md`.

From the worktree root, outside any existing CPU allocation:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-06-t-port-data/native.sh
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-06-t-port-data/targeted.sh
```

The supplied simbox launcher can replace the one-slot launcher for the native
workflow. It includes compilation, so no external installed package or sibling
checkout provides the implementation. Plain Boost headers are discovered through
the existing BH installation for compiling the historical comparison fitters;
the certificate itself uses only the C++17 standard library and core headers.
No installation step is part of this workflow.

`native.sh` runs in the foreground. It writes:

- `gamma.json`: 48 independent 80-digit normalizer/derivative enclosure checks.
- `replay.json`: 31 frozen samples, both fitting domains, profiles and native
  timings. Each method has one discarded warmup and three measured calls.
  Historical helpers are called explicitly, so changing the public defaults
  cannot contaminate the baseline.
- `independent.json`: accepted finite and endpoint boxes against retained
  independent 80-digit roots, plus whole-box golden tolerances. The earlier
  research global outcomes are compared only for native accepted decisions.
- `cutoff.json`: 8 points with 3 independent remainder/loss checks around
  the cancellation-series switches.
- `arithmetic.json`: 60 deterministic points with 14 enclosed likelihood,
  score and Hessian quantities, including large shape through 1e14 and the
  stable ordinary-scale shape score. Seed 14020261106.
- `adversarial.json`: exhaustive native outcomes on the two retained multimodal
  examples. A finite winner must enclose the independently retained winner;
  guarded comparison refusals are recorded explicitly.
- `stress.txt`: the near-Cauchy finite fit, a restricted Cauchy, the shallow
  4,000-point sample, and exact 100-fold replication. Status numbers follow
  `TCertificateStatus` in the source; 6 is budget, 7 unresolved tie, 8 numerical.
- `shallow.txt`: root/neighborhood inclusion for the 4,000-point sample;
  stage 4 means both passed. This is not global exclusion or an accepted MLE.
- `native-environment.json`: compiler, R, Python and source SHA256 identities.

`diagnostic.cpp`, `diagnostic.py` and `tail-profiles.cpp` are small diagnostic
reproducers retained for the interval dependency investigations. They are not
acceptance tests and do not change production dispatch. Their original loose
arithmetic is described in `arithmetic.md`; the final header contains its
corrected version.

`local-final.sh` acquires and releases four workstation slots separately for
JS build, JS tests, site validation and the R full tier. Do not wrap that script
in another slot launcher. Final regression results and log hashes are recorded
in `validation.json`; full logs are worktree scratch output.

The unrelated near-normal Azzalini st equivariance failure on simbox is
reproduced by `baseline-st.sh`/`baseline-st.R`. They copy the already generated
package into worktree scratch, restore the six changed C++ headers from frozen
master 164d4178, delete only copied build products, and compile the scratch
package with `pkgload`. They do not install a package or alter the checkout.
`baseline-master/source.json` identifies every restored header. The resulting
`baseline-st.json` records the same nu drift around 1272 and its negligible
likelihood difference. The shared test keeps its existing 2e-4 df tolerance
below nu=1000 and uses 5e-4 above it, retaining the separate likelihood check.
The lst/std golden tolerances and all fitters' numerical behavior are unaffected
by this test-only correction.
