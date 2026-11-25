# WASM stack overflow corrupted later fits (TASK-266)

The Codex job found the cause and made the fix, then stopped before writing this report: OpenAI's content filter
ended its session mid-run. Claude finished the checks and wrote the report from the job's logs.

## Cause

The JS binding was built with Emscripten's default 64 KiB stack and no stack-overflow check
([build.sh](../../../bindings/js/build.sh)). The free-scale Tukey lambda certificate on `[-0.25, 0.25]` with `sigma = 1`
and `lambda = 1.5` held (an `unresolved` stop with two contact candidates) uses about 84 KiB of stack: with
`-sSTACK_OVERFLOW_CHECK=2` at the default size the call aborts with "Attempt to set SP to 0x00020470, with stack limits
[0x000254f0 - 0x000354f0]". Without the check, Emscripten's stack grows down into static data without any error,
so the overflow silently overwrote static memory. Every later held-moment snorm fit in the same instance then failed
with a `numerical` stop ("certificate arithmetic or root enclosure"), on master before TASK-254 as well as after.

Native builds were not affected: their stacks are megabytes, so the same sequence in R gives the same result twice.
The fit snapshots and their baselines are native and double-only C++, not WASM, so no recorded result changes.

## Fix

`-sSTACK_SIZE=262144` (256 KiB, about three times the measured need) and `-sSTACK_OVERFLOW_CHECK=2`, so any future
overflow aborts with a message instead of corrupting memory. The check instruments every stack-pointer adjustment;
its cost was not measured here.

## Tests

- JS ([stop-test.mjs](../../../bindings/js/stop-test.mjs)): a fresh instance fits the held-moment snorm sample, runs
  the leaking Tukey lambda stop and three other structured stops (Cauchy `nomle`, Lomax `limit`, Tukey lambda
  `budget`), and checks that the snorm result is identical each time.
- C++ ([snorm/test.cpp](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/test.cpp)): the same
  sequence natively (it passed before the fix too, as expected, since the defect is WASM-only).

Results of `just js-build`, `just js-test` and `just test-changed --no-long-double` on the workstation are in the
merge commit message.

## Open

- The job did not audit the other function-local `static thread_local` caches it listed (skew_coordinate, ged, sged,
  cmp, skellam, pig, tbinom, emg); none is implicated in this defect. Their keys look complete on inspection, but
  that was not checked systematically.
- Stack use of other deep certificates in WASM is unmeasured; the overflow check now turns an overflow into an
  abort that the JS tests would catch.
- The earlier wording of this job's brief (memory corruption, out-of-bounds writes, sanitizer builds) is what the
  content filter flagged; phrase future debugging briefs in terms of the defect and its reproduction.
