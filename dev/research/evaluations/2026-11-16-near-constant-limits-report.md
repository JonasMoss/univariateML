# Near-constant limits of betapr and genbeta (TASK-214.39, part 2)

Branch `claude/near-constant-limits`, from master `821f6eda`. Not committed. Source:
[near-constant report](2026-11-15-near-constant-report.md) (sections betapr, genbeta). Scripts and outputs:
[2026-11-16-near-constant-limits-data](2026-11-16-near-constant-limits-data/).

## Summary

| family | sample $[c,c,c^+]$ | before | now | certified |
|---|---|---|---|---|
| betapr, all free | two low, one high | F:evaluation | limit `invgamma`, shape 1.3678792443002628e32, scale 8.372894375288101e31 | no (margin-decided) |
| betapr, all free | $[c,c^+,c^+]$ | F:evaluation | limit `gamma`, shape 1.3678792443002632e32, scale 4.4748665991497303e-33 | no |
| genbeta, scale 1 | $[c,c,c^+]$ | F:no-convergence | limit `power` (endpoint max $x$, rate $1.13\cdot10^{16}$) | no |
| genbeta, scale 1 | $[c,c^+,c^+]$ | F:no-convergence | limit `power` (rate $2.26\cdot10^{16}$) | no |

**Deviation from the task text: genbeta selects `power`, not `lgamma`, for the report's sample.** The report compared finite $a$
with the log-gamma face only ($a\le10^3$) and never evaluated the power face. At 100 digits, for the report's sample,
$L_{\text{power}}=108.42679703335579741$ against $L_{\text{lgamma}}=108.2097022045816969$ (power higher by 0.217), and for the reverse
sample 110.50623857503563 against 108.20970220458170 (by 2.30). The power face is a limit of genbeta densities (checked below), so
`lgamma` is not the supremum. The report's face values for `lgamma` ($\hat q=8.15\cdot10^{30}$) are correct as values of that face.
The dossier text the report proposes for genbeta's `lgamma` entry is therefore not applied as proposed.

## betapr (`betapr/near_constant.hpp`, hooked at `betapr/joint_certificate.hpp` 502-507, `betapr/mle.hpp` 36-38, 365-366)

Method. After the existing search ends in `BfgsFailure` (not budget, not accuracy stop), a sample with relative spread
$(x_{\max}-x_{\min})/x_{\min}\le10^{-6}$ and $n\ge3$ goes to `betapr_detail::near_constant_limit`. It never replaces an `Ok`.
- Offsets $e_i=(x_i-x_{\min})/x_{\min}$ are exact to rounding; $v_i=(e_i-\bar e)/(1+\bar e)$ gives $x_i=\tilde x(1+v_i)$ and
  $w_i=-v_i/(1+v_i)$ gives $1/x_i\propto 1+w_i$. $c=\log\overline{x}-\overline{\log x}=\sum_{k\ge2}(-1)^k(\overline{v^k}-\bar v^k)/k$
  (the linear terms cancel exactly), the shape is the root of $\log a-\psi(a)=c$ by the Stirling series
  (`gamma/relative.hpp`, `relative_gamma::log_ratio`, `shape_from_ratio`). No logarithm of an observation is used.
- Face choice by the sign of the third central moment $s_3$ of the offsets: $L_{\text{inv}}-L_{\text{gamma}}=ns_3/(3s_2)(1+O(\rho))$
  (derived here from $c_\gamma=s_2/2-s_3/3$, $c_{\text{inv}}=s_2/2-2s_3/3$; for $[c,c,c^+]$ it gives $\delta/3$, the report's value).
  The comparison is made only if $|s_3|>64\,\max|v_i|^2s_2$ (fourth-order terms are bounded by that order; the constant 64 is not proved).
  Symmetric samples (for example $[c,c,c^+,c^+]$) are left as failures.
- Certification: none. The leading-order sign is an analytic comparison of the two faces, but it does not exclude finite members:
  "no interior point beats the face" is numerical in the report (profile $L_g+\rho(L_i-L_g)$, 17 digits) and has no bound. So
  `margin_decided = true`, outcome `limit(certified = false)` (new field `BetaPrimeResult::margin_decided`, used in `outcome()`);
  the adapter's `render_fit` adds " No improvement above the likelihood margin was found."
- `logLik` is the log likelihood at the returned doubles, not the supremum. At shapes of $10^{32}$ a relative rounding of the scale
  by $10^{-16}$ costs $a\eta^2/2\approx1$: 107.0976992046995 against the supremum 108.2097022045817 (the first version returned the
  supremum and `fit-density.py` flagged it, 4 new failures). The loss is computed in double-double
  (`relative_gamma::DD`, `scale_loss`); it equals the 70-digit value at the returned doubles (107.09769920469951).
  `candidates` carry both faces at their own returned coefficients; `loglik_upper` is infinity (supremum over finite members not bounded).
- Reverse sample: the same analysis gives $L_\gamma-L_{\text{inv}}=-ns_3/(3s_2)>0$, gamma wins, checked below.

Evidence for the decision. 400 random near-constant samples (`sweep.cpp`: $n=3..6$, spread $2\cdot10^{-16}$ to $10^{-6}$, three
bases), 360 betapr cases with the search failing before the change: 313 now end at a face (119 gamma, 194 inverse gamma), 47 stay
failures (zero or near-zero skewness, mostly two-valued samples with equal split) and 40 are constant (`Unbounded`, unchanged).
For all 313, the face chosen and its supremum agree with an 80-digit gamma and inverse-gamma fit (`verify_sweep.py`): 0 disagreements.
At spreads $10^{-5}$ and above the existing search does not end in this way and the code is not reached.

## genbeta (`genbeta/near_constant.hpp`, hooked at `genbeta/mle.hpp` 460, 468-473)

Method. After the all-free search ends `SearchUnresolved` (not budget, not accuracy stop), a sample in $(0,\text{scale})$ with relative
spread of $x$ at most $10^{-6}$, $n\ge2$ and $\text{gap}\le10^{-6}\bar y$ ($y=\log(\text{scale}/x)$) goes to `genbeta_detail::near_constant_limit`.
$y$ in doubles is not usable here (one ulp of $y$ is 20% of the spread for the report's sample, and `gamma_mle` on it returns shape
$7.46\cdot10^{30}$ instead of $8.15\cdot10^{30}$); the code uses the exact offsets $d_i=\log(x_{\max}/x_i)$.
- Power face: $n(-\log g-1)+\sum(-\log x_i)$, $g=\overline d$; coefficients (endpoint $x_{\max}$, rate $1/g$).
- Log-gamma face: gamma fit of $y$ from the offsets as above, coefficients (shape, scale of $y$, held scale).
- Decision: the higher value, evaluated as the difference $n(-\log g-1-\text{core}(q,c)+\log\bar y)$, which is $n(\log(\sqrt{2\pi}s/g)-\tfrac12)$
  to leading order ($s$ the standard deviation of $y$): power when $s/g>0.6577$, log-gamma otherwise. Not decided when $|{\rm diff}|\le4\cdot4096\,\epsilon(\ldots)$.
  $[c,c,c^+]$: $s/g=0.707$, power by 0.217; $[c,c^+,c^+]$: 1.414, power by 2.30; $[c,c,c,c,c^+]$: 0.5, log-gamma by 1.37.
- Certification: none (`allfree_certified=false`, outcome `limit(false)`). Finite $(p,q,a)$ are not excluded by a bound. Numerical
  scan at 60 digits (`genbeta_profile_large_a.py`; valid for $a\gtrsim50$ because $q\ge e^{0.18a}$): the profile in $a$ stays at the
  log-gamma level for $a$ up to about $10^{12}$ (spread $10^{-16}$), dips by up to 0.3, and rises monotonically to the power face as
  $a\to\infty$; the same shape as a function of $a\rho$ at $\rho=10^{-8}$ and $10^{-6}$. The power face is reached by explicit finite genbeta
  densities: $a=10^{19}$, $p=\lambda/a$, $\log q=-3-a\log x_{\max}$ gives 108.369 at 80 digits, above the log-gamma face.
- The log-gamma face's `limit_loglik` is the supremum of the face, not evaluated at the rounded coefficients (the same rounding loss,
  about $5\cdot10^{-2}$ for shape $8\cdot10^{30}$, is not subtracted). `fit-density.py` skips the model.
- `fit_genbeta` already words an uncertified `UnitGammaBoundary`/`PowerBoundary`; `render_fit` adds the margin sentence.

Evidence. 355 genbeta cases of the same sweep (68 log-gamma, 287 power) agree with 80-digit values of both faces in choice and value
(0 disagreements). 5 of 400 samples remain `SearchUnresolved`: spread of $y$ above the $10^{-6}$ gate (spread of $x$ $2.7\cdot10^{-7}$ to
$7.6\cdot10^{-7}$ with $\bar y\approx0.16$), where the old search also fails. Not covered.

## Tests

Added (`near-constant` samples as hex doubles; the stored values):
- `betapr/test.cpp` line 841 "betapr near-constant samples end at the face the skewness selects, uncertified": $[c,c,c^+]$ inverse gamma,
  $[c,c^+,c^+]$ gamma, each `Limit`, `certified=false`, shape and scale within 1e-10 of 100-digit references, `logLik` within 1e-10 of the
  70-digit value at the returned doubles; $[c,c,c^+,c^+]$ stays a failure; the tie3 sample keeps its certified inverse-gamma limit.
- `genbeta/test.cpp` line 631 "genbeta near-constant samples end at the power or log-gamma face, uncertified": two power cases and one
  log-gamma case ($[c,c,c,c,c^+]$), coefficients within 1e-10 and `limit_loglik` within 1e-12 of 100-digit values.
Before the change (the new tests built against the unmodified headers, `margin_decided` checks removed): all three fail
(betapr `status` is not `InvGammaBoundary`, genbeta not `PowerBoundary`).

Changed expectation (one): `genbeta/test.cpp` "genbeta certificate: rounded logarithms do not prove constant-data nonexistence", line 275.
It looped over held $a=0.4$ and free $a$ on $\{e^{-700},\text{nextafter}\}$ and expected `SearchUnresolved`/`Arithmetic` for both. The held-$a$ half is unchanged;
the all-free half now expects `PowerBoundary`, not certified (`s/g=1`, correct by the 100-digit comparison above).

## Snapshot diff

`betapr,genbeta,gb2,fpareto`, native and `--no-long-double`, against `2026-11-15-fit-snapshot-data` (dirs kept in `build/snapshots/{native,double}`).
Both modes: 1576 cases each, 10 changed, identical lists. No `Ok` moved, no stop became worse, no certified limit became uncertified.
All 10 changed from a failure to an uncertified limit:
- betapr: `class near-constant` (failure -> limit invgamma), `class offset` (n = 6, spread $2.4\cdot10^{-15}$, failure -> invgamma), `archive`
  000172, 000173, 000175, 000176 (n = 5, spreads $5\cdot10^{-8}$ and $5\cdot10^{-12}$, failure -> invgamma).
- genbeta: `class near-constant` (failure -> power), `archive` 002491, 002493, 002494 (n = 5, failure -> log-gamma (`unavailable:unit-gamma`)).
The eight extra cases have the same cause. Face choice and value checked at 80 digits: betapr invgamma wins by $1.1\cdot10^{-8}$,
$1.1\cdot10^{-12}$, $1.3\cdot10^{-15}$; genbeta log-gamma wins by 0.340. gb2 and fpareto: no change.

## Strict layer (`--prune`, baseline snapshots overlaid with the new dirs; `CERT_REPLAY_COVERS_CACHE=build/covers-cache.json`)

- Removed from `dev/validation/strict-known.json` (4, TASK-214.33, finding `near-constant-numerical-failure-unsettled`):
  `phenomenon:near-constant|{native,double}|{betapr,genbeta}|class|near-constant`.
- `fit-judge.py`: 13 known, 0 new. `fit-density.py`: 0 new (4 new on the first version, see betapr). `fit-consistency.py`: 26 known, 0 new
  (the 4 entries above passed and were pruned). `fit-refine.py`: 12 known, 0 new. `cert-replay.py --family betapr`: no replay exists for betapr;
  `--family genbeta`: 158 cases, 0 failures.

## Dossiers

`betapr/README.md` limits entries `gamma` and `invgamma` (`detect`): the near-constant rule and the skewness formula (numerical, below
double resolution). `genbeta/README.md` limits entries `lgamma` and `power` (`detect`): the comparison $s/g$ against 0.6577 and the numerical profile.
Front matter parses; `just gen` ran without error (generated R files are gitignored).

## Open

- The unproved parts: no bound excludes finite members for either family; the constant 64 in the betapr validity test.
- genbeta: samples with spread of $y$ above $10^{-6}$ relative that the old search also fails (5 of 400 random ones) are not covered.
- Samples with zero skewness (betapr) and with $s/g$ at 0.6577 (genbeta) stay failures.
- The genbeta log-gamma `limit_loglik` is the face supremum, not the value at the rounded coefficients.
- betapr near-constant fits with held shape or scale: not examined (holds unchanged).

## Suites

One at a time through `/tmp/umvl-cpu/run4`, after the last code change:
- `just cpp-test`: 110 of 110 passed.
- `just cpp-test-no-long-double`: 154 of 154 passed.
- `just test-changed`: exit 0 for each step it ran: `cpp-test` (277 s), `test` (R tier, 582 s), `js-build`, `js-test` (586 of 586 passed).

## Files changed

`univariateML-cpp/include/cppunivariate/distributions/`: `gamma/relative.hpp` (new), `betapr/near_constant.hpp` (new),
`betapr/joint_certificate.hpp`, `betapr/mle.hpp`, `betapr/README.md`, `betapr/test.cpp`, `genbeta/near_constant.hpp` (new),
`genbeta/mle.hpp`, `genbeta/README.md`, `genbeta/test.cpp`; `dev/validation/strict-known.json`; `dev/validation/strict-refine.jsonl.gz` (reference cache, extended by `fit-refine.py` for the new cases); this report and its data directory.
