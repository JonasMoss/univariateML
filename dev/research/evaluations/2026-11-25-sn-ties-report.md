# TASK-254 · Exact reflected maxima in sn and snorm

Finite skewed winners on exactly symmetric samples yield a two-point
maximizer set under reflection-invariant holds. Both public fits report
`unique = FALSE` and carry the finite winner's certification. The held-moment
`snorm` example now returns a certified fit where the held certificate
previously reported an unresolved comparison.

## Implementation

The symmetry test sorts the original binary64 observations and pairs the
$i$th smallest with the $i$th largest, including the middle observation
twice for odd sample sizes. The sample is symmetric exactly when every
pair has the same real sum. FastTwoSum orders the summands by magnitude
and stores their rounded sum and its exact residual under round-to-nearest
with gradual underflow. Both words must agree. For an overflowing endpoint
sum, matching pairs must also overflow before scaling. Both summands are
then large enough that halving is exact, so the same comparison applies to
the scaled pair sums. A held location must have exactly that sum when
paired with itself. This avoids rounding a sample center that is between
binary64 numbers. No tolerance enters the data or held-location comparison.
See [sn/reflection.hpp, line 36](../../../univariateML-cpp/include/cppunivariate/distributions/sn/reflection.hpp#L36).

- `sn` attaches $(2c-\xi,\omega,-\alpha)$ after accepting the public
  coefficient vector. With $\xi$ held at $c$, strict concavity in the
  signed shape coordinate gives the normal winner for nonflat data.
  The constant-data flat shape line continues through its own route.
  [sn/mle.hpp, line 543](../../../univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp#L543)
- `snorm` attaches $(2c-\mu,\sigma,1/\xi)$ in the accepted free profile.
  The held certificate uses exact symmetry and a coefficient-accuracy
  comparison to associate a numerical contender with the reflected root.
  Its existing face, unresolved-box and coefficient-accuracy checks still
  apply. Exact reflection establishes equality of the two likelihoods,
  conditional on the existing numerical winner selection.
  [snorm/mle.hpp, line 254](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/mle.hpp#L254)
  [snorm/certificate.hpp, line 569](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/certificate.hpp#L569)
- The `snorm` outcome reads the typed set and the common binding transports
  the set and both rows. `sn` uses its existing set handling.
  [snorm/mle.hpp, line 306](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/mle.hpp#L306)
  [snorm binding, line 28](../../../bindings/common/distributions/snorm/fit.hpp#L28)

The likelihood identities follow from negating the standardized residual
and slant for `sn`, and swapping the two side scales for `snorm`.
The reference checks prove that the fixture maxima are finite and skewed.
The held-moment witness check also passes with the exact rational value of
the binary64 input used for $151/200$. Numerical density sums verify the
likelihood at both returned binary64 parameter vectors.
[Mathematical examples](2026-11-25-sn-math-report.md#exact-finite-global-tie)
[Two-piece examples](2026-11-25-sn-math-report.md#exact-profile-counterexamples)
[Rational replay](2026-11-25-sn-ties-data/rational-check.txt)

## Regression and validation

C++ and R tests pin the seven-point `sn` sample, the 22-point free `snorm`
sample, and the three-point `snorm` sample with mean zero and sd $151/200$.
They check both members, certification, the uniqueness flag and direct
density sums. The free-fit perturbations stay single. The C++ symmetry
test exercises a rounded pair-sum collision, a subnormal midpoint and
overflowing sums.
[sn C++ tests, line 418](../../../univariateML-cpp/include/cppunivariate/distributions/sn/test.cpp#L418)
[snorm C++ tests, line 455](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/test.cpp#L455)
[sn R tests, line 205](../../../bindings/r/distributions/sn/test.R#L205)
[snorm R tests, line 333](../../../bindings/r/distributions/snorm/test.R#L333)

All final runs used the workstation. Native and double-only compilation
used GCC 15.2. The changed-family runner selected ged, sged, sn, snorm,
sstd, st and std.

| Check | Result |
|---|---|
| `just test-changed --no-long-double`, native C++ | 12/12 passed |
| Same runner, double-only C++ | 12/12 passed |
| Same runner, R | 3,016 passed, zero failures, zero warnings, 23 skips |
| Same runner, JS | Build passed. Tests 260/261 passed. See Open. |
| Direct WASM reflection smoke test | All three fixtures passed, including both density sums |
| `just site-check` | Passed |
| `fit-density.py` | Zero NEW |
| `fit-consistency.py` | Zero NEW |

The stored and fresh snapshots matched record for record using
`build/scratch/diff_baseline.py`. The frozen corpus had no newly recognized
reflected pairs. The regression fixtures supply direct coverage of the
changed results.

| Mode | Records | Changed | Added | Removed |
|---|---:|---:|---:|---:|
| Native | 12,081 | 0 | 0 | 0 |
| Double-only | 12,081 | 0 | 0 | 0 |
| Modifier routes, native | 2,006 | 0 | 0 | 0 |
| Modifier routes, double-only | 2,006 | 0 | 0 | 0 |

[Snapshot diffs and provenance](2026-11-25-sn-ties-data/)
[Density check](2026-11-25-sn-ties-data/fit-density.txt)
[Consistency check](2026-11-25-sn-ties-data/fit-consistency.txt)
[WASM checker](2026-11-25-sn-ties-data/wasm-pairs.mjs)
[WASM results](2026-11-25-sn-ties-data/wasm-pairs.jsonl)

## Open

- **TASK-255.** Each base precision mode has 83 asymmetric certified
  `sn` estimates and 62 asymmetric certified `snorm` estimates currently
  labeled unique. Of those, 1 `sn` and 6 `snorm` records lack a conditional
  uniqueness proof from their hold subset. That gives 7 distinct records,
  or 14 rows across both precision modes, for a vocabulary change scoped to
  asymmetric certified fits with unproved cardinality. The modifier-route
  modes contribute zero. The exact input audit compares rational values of
  the binary64 observations.
  [Audit code](2026-11-25-sn-ties-data/audit.py)
  [Audit counts](2026-11-25-sn-ties-data/snapshot-audit.jsonl)
- **Cardinality beyond the reflected pair.** Reflection proves equality
  and distinctness for the partner of the accepted skewed winner.
  Ruling out additional exact ties would require a complete comparison
  of disjoint maximizing profile branches.
- **JavaScript suite assertion.** The shared stop test still expects an
  unresolved exception for the held-moment fixture. The direct WASM
  smoke test verifies the successful two-point result and both likelihoods.
  The shared JS test is outside the listed file ownership. The full runner's
  green result awaits authorization to update that assertion and rerun it.
  [Existing JS assertion, line 407](../../../bindings/js/stop-test.mjs#L407)
