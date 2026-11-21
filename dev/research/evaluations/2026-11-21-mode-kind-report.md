# Precision-independent fit outcomes

TASK-214.13 and the two skew-t entries of TASK-214.33. Branch `codex/mode-kind`, base `8b3b2dba`. All nine precision-mode checks and both minimum-sample checks pass. Native and double-only snapshots each cover 1,783 cases. All nine canonical golden records per mode and all records for the five dependent families match the stored baselines exactly.

## Listed cases

The references use exact binary64 inputs evaluated at 80 decimal digits. The skew-normal held-moment references independently profile location or scale over the side-scale ratio. The huge free case also has exact rational polynomial root isolation. The C++ certificates use the repository's practical floating-point margins. An outcome described as an uncertified fit passed the coefficient check, with global optimality left open. [Reference script](2026-11-21-mode-kind-data/reference.py), [reference results](2026-11-21-mode-kind-data/reference.txt), [snapshot payloads](2026-11-21-mode-kind-data/case-outcomes.txt).

| Family and case | Evidence and final outcome in both modes | Cause and fix | Strict verdict |
|---|---|---|---|
| bhatt `000364` | Finite held-scale fit, with location 500000.187023187013 and half-width 500007.677401286530. The 80-digit score root has negative definite Hessian. Both fits are uncertified. | The double-only search lost precision in distant Gaussian endpoint ratios and their curvature. The location solver evaluates the nearer endpoint's Mills hazard and derivative directly when the other endpoint is negligible. The final refinement squares the small endpoint residual directly. `bhatt/mle.hpp:36`, `bhatt/accuracy.hpp:25`. | Pass |
| bhatt `000428` | Finite location 200002.139774650095 with both scale and width held. The 80-digit score root and strict conditional log concavity establish the unique maximizer. The fitter retains its uncertified flag. | The double-only profile had the same far-tail cancellation. The hazard formula restores its location solve. `bhatt/mle.hpp:36`. | Pass |
| dgamma `001025` | Finite shape 972057.312598201494 with rate held. Independent integration and the shape score give log likelihood −39.2043904698867. | Double-only differentiation subtracted nearly equal gamma tails at shape near one million. Refinement integrates the smooth cells directly, with a centered gamma deviance and Stirling correction at the midpoint. `dgamma/accuracy.hpp:33`, `dgamma/mle.hpp:428`. | Pass |
| dgamma `wide` | Finite shape 0.0813443740298880101 and rate 3.25051624549745014 × 10⁻⁷. Independent integration gives log likelihood −31.9228051314017. | The unit cell at one million lost score precision in double-only tail subtraction. Midpoint integration of that cell restores the two-parameter refinement. `dgamma/accuracy.hpp:33`. | Pass |
| snorm `006576` | Right half-normal limit with endpoint −426123.507000533200 and scale 534066.589499234597. Its likelihood beats the left face by 0.6998452128. The independent held-mean profile approaches the right face from below, and the certificate excludes the competing cells. | The double-only uncertain-root bound relaxed away the held-mean location constraint. Applying the split's implied scale cap rules out the uncertain endpoint intervals. `snorm/certificate.hpp:453`. | Pass |
| snorm `006587` | Right half-normal with endpoint 0.99999998 and scale 1.657581062883877. The face gap is 2.7296798639 × 10⁻¹⁶ in log likelihood. The normalization bound proves that any finite shape loses to its corresponding face. | The earlier native audit retained an unresolved comparison and double-only stopped numerically. The held-standard-deviation normalization bound excludes the interior, and the factored residual-sum difference resolves the face ordering before adding the common normalizer. `snorm/certificate.hpp:335`. | Pass |
| snorm `006590` | Right half-normal with endpoint −1.1057748031821504 × 10¹⁵⁰ and scale 1.657581062883877. Its log likelihood is −7.3610385812 × 10³⁰⁰. The independent profile approaches this face and the right face beats the reflected face by 3.6727030429 × 10³⁰¹. | Double-only Bernstein multiplications and deflation overflowed before the score was normalized. A common positive divisor now scales the score contributions before forming the polynomial. `snorm/certificate.hpp:393`. | Pass |
| snorm `huge` | Right half-normal with endpoint −1.3714475655123392 × 10³⁰⁸ and scale 1.6754169181897987 × 10³⁰⁸. Exact root isolation and 80-digit comparison of seven candidates select the left sample endpoint as the mode. | The full range overflowed in double-only standardization. The profile now works with a half-range and restores physical coefficients after calculating them in normalized units. `snorm/mle.hpp:139`. | Pass |
| st `007040` | Uncertified finite location 0.0472401210858987306 and restricted degrees of freedom 1. The 80-digit profile gives log likelihood −34.1165032914879 and right df derivative −11.7168580037 at the solution. The Gaussian envelope proves the skew-normal escape loses. | The native auxiliary skew-normal fit failed an absolute likelihood guard because normalized and restored evaluations differed by 0.0001220703125. The double-only fit cleared the guard. Twice the Gaussian density bounds the skew-normal density, and the two extreme observations alone give a face log-likelihood upper bound of −2.9514377005 × 10¹¹. The fitter excludes that face before calling its optimizer. `st/mle.hpp:194`. [Auxiliary replay](2026-11-21-mode-kind-data/st-face.txt) | Pass |
| st `two` | Both fully free fits return `failure(unsupported)` for the distinct pair. The collapse exponent at df 1 is zero. The declared three-observation minimum applies to the fully free fit, and the held evaluation route still works. | Both builds previously returned a collapse-equality convergence stop below the implemented minimum. The fitter and common binding report the supported range explicitly. The dossier's per-class declaration enforces the same outcome when the collapse rule also applies. `st/mle.hpp:141`, `bindings/common/distributions/st/fit.hpp:27`, `st/README.md:111`. | Native pass, double-only pass |

The 2026-11-12 audit labels native `006587` a tie. The newer canonical native snapshot already labels its payload a limit and still includes both face candidates. The repaired fit carries a single certified right-face comparison in both modes. For `007040`, the older audit's generic native error is a numerical stop with reason `no-convergence` in the canonical baseline. [Audit inventory](2026-11-12-fit-snapshot-data/mode-kind-differences.txt), [canonical case outcomes](2026-11-21-mode-kind-data/case-outcomes.txt).

## Snapshot moves

The comparison uses the stored 2026-11-15 snapshots. Coefficient movement is measured relative to the stored coefficient and includes estimate-to-estimate cases only. Counts include changes to diagnostics and iterations. [Native comparison](2026-11-21-mode-kind-data/diff-native.txt), [double-only comparison](2026-11-21-mode-kind-data/diff-double.txt), [golden checks](2026-11-21-mode-kind-data/additional-evidence.txt).

| Family | Cases per mode | Native changed / kind moves | Double-only changed / kind moves | Largest relative move, native / double-only |
|---|---:|---:|---:|---:|
| bhatt | 173 | 2 / 0 | 2 / 2 | 0 / 0 |
| dgamma | 79 | 3 / 0 | 3 / 2 | 1.77 × 10⁻¹² / 6.41 × 10⁻¹¹ |
| ged | 173 | 0 / 0 | 0 / 0 | 0 / 0 |
| lst | 173 | 0 / 0 | 0 / 0 | 0 / 0 |
| sged | 285 | 0 / 0 | 0 / 0 | 0 / 0 |
| snorm | 157 | 7 / 3 | 10 / 7 | 0 / 0 |
| sstd | 285 | 0 / 0 | 0 / 0 | 0 / 0 |
| st | 285 | 7 / 6 | 6 / 5 | 0 / 0 |
| std | 173 | 0 / 0 | 0 / 0 | 0 / 0 |

Bhatt's two changed native records move only in iteration count. Dgamma's maxima occur in the free rate for native `wide` and in the rate for double-only `001011` with shape held. Both movements are below the default coefficient tolerance.

Beyond the listed kind changes, the snapshot comparison records the following.

- Snorm `006584`, `006585`, and `006588` move from numerical stops to right half-normal limits in both modes. The held-sd samples meet the same normalization bound as `006587`. Independently evaluated face gaps are positive even when adding them to the log normalizer erases the difference. [Face-gap and normalization checks](2026-11-21-mode-kind-data/additional-evidence.txt).
- St archive `006931` moves from a collapse-equality stop to unsupported in both modes. It is another distinct pair with all parameters free.
- St `006992`, `007054`, and `007056` move from numerical stops to checked, uncertified fits with df 1 in both modes. Each has held scale and a finite witness above the skew-normal upper bound. The finite log likelihoods are about −33.29, −8289.52, and −34.14. The bounds are about −2.95 × 10¹¹, −6.78 × 10³⁰⁰, and −2.95 × 10¹¹, respectively. [Witnesses and upper bounds](2026-11-21-mode-kind-data/case-outcomes.txt).

## Regression tests and validation

The appended Bhatt test covers `000364` and `000428`. The appended dgamma test covers `001025` and `wide`. Two appended snorm tests cover all three held-moment cases and `huge`. Two appended st tests cover `007040` and the exact distinct pair from class `two`, including the fully held evaluation. The tests compare the outcomes and use `mltest::kFitReferenceTol` with `.scale(0)` for reference coefficients.

| Check | Machine | Result |
|---|---|---|
| `just test-changed --no-long-double` | Workstation, GCC 15.2 | Native and double-only C++ each passed 9/9. R passed 3,440 with zero failures or warnings and 29 skips. JS build passed and JS tests passed 264/264. |
| Canonical snapshots | Workstation, GCC 15.2 | 1,783 cases per mode. The five header dependants `ged`, `lst`, `sged`, `sstd`, and `std` match their complete stored snapshots in both modes. |
| `fit-judge.py` | Workstation | All nine listed mode checks pass. The judge found zero failures in the selected snapshots. |
| `fit-consistency.py` | Workstation | Both st two-point checks pass. Zero new findings and two known sstd findings in the selected snapshots. |
| `just quality bhatt dgamma snorm st` | Workstation | Passed for all four families. |
| `just site-check` | Workstation | Final four-slot run passed, including 85 family pages and 286 inference rows. |

The 13:10 OOM left the final test and snapshot logs empty. The earlier references and direct probes finished before the interruption. The restarted tests and both snapshots completed through `run4`. The first site check used a single-slot wrapper and rebuilt WASM. The final site run used `run4` and included the dossier-only consistency fix. That fix changed neither the fitter nor the snapshot inputs, so the consistency check reused the completed snapshots. [Targeted tests](2026-11-21-mode-kind-data/test-changed.txt), [fit judge](2026-11-21-mode-kind-data/fit-judge.txt), [consistency check](2026-11-21-mode-kind-data/fit-consistency.txt), [quality](2026-11-21-mode-kind-data/quality.txt), [site check](2026-11-21-mode-kind-data/site-check.txt).

## Open

- The held-scale joint Bhatt fit for `000364` has an independently verified interior stationary maximum. A closed global width comparison would be needed to promote its certification flag.
- The skew-t fit for `007040` has an independently checked constrained stationary solution and a proved exclusion of the skew-normal face. A global bound over the remaining finite location and df domain would be needed to certify the fit. Attainment for the fully free distinct two-point sample remains open at the df restriction.

## Reproduction

Run `dev/validation/fit-snapshot.py --family bhatt,dgamma,ged,lst,sged,snorm,sstd,st,std --jobs 4` through `run4` with separate output directories. Add `--no-long-double` to the second run. Pass both directories to `dev/validation/fit-judge.py` and `dev/validation/fit-consistency.py`. Use [diff.py](2026-11-21-mode-kind-data/diff.py) on each directory and its [stored baseline](2026-11-15-fit-snapshot-data/). Run [reference.py](2026-11-21-mode-kind-data/reference.py) through the single-slot wrapper to reproduce the independent references. [Native provenance](2026-11-21-mode-kind-data/native.provenance.json), [double-only provenance](2026-11-21-mode-kind-data/double.provenance.json).
