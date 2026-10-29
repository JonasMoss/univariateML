# threshold implementation report

No commits or git-writing commands. Existing distribution folders and existing
goldens were not edited. Fatigue is unchanged.

## Files changed

New `cppunivariate/include/cppunivariate/distributions/` folders:
`pareto2/`, `pareto3/`, `pareto4/`, `fpareto/`, `nsbeta/`, `frechet/`.
Each contains `README.md`, `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `test.cpp`,
`test.R`, and (after generation) `golden.json`.
The scalar C++ density seam is provided; R d/p/q/r use the requested references.

Shared files, individually:
- `cppunivariate/include/cppunivariate/cppunivariate.hpp`: six includes.
- `cppunivariate/include/cppunivariate/distributions/README_SCHEMA.md`: optional reason.
- `univariateML/CANONICAL_NAMES.md`: six coefficient registrations.
- `univariateML/scripts/load_distributions.R`: retain and validate required_reason.
- `univariateML/scripts/codegen_wrappers.R`: append reason using quoted R strings.
- `univariateML/R/code_helpers.R`: three runtime-checked reference-tail repairs.
- `univariateML/tests/testthat/test_input_checks.R`: held-argument switches.
- `univariateML/tests/testthat/test-audit-input.R`: held-argument switches.
- `codex-report.md`: this report.

## Mathematics: proved

- Pareto II: min=m-epsilon, scale=epsilon gives likelihood proportional to
  epsilon^(shape*(n-k)-k); select shape<k/(n-k). Constant data diverge for any shape.
- Pareto III/IV/Feller-Pareto: the lower-endpoint density powers are respectively
  shape-1, shape2-1, shape2*shape3-1. Select a negative power and approach the minimum.
- Nonstandard beta: choose the corresponding shape below one and approach either
  sample extreme with its endpoint while keeping the other endpoint beyond the sample.
- These bijective likelihood transformations transfer identification and sample
  existence conditions from the delegates.
- Held Pareto fits exactly translate Lomax/loglogistic/Burr/GB2; beta additionally
  transforms scale and subtracts n*log(max-min). Parameter mappings are in the READMEs.
- Fréchet maps to GEV with xi=1/lambda, GEV scale=sigma/lambda,
  GEV location=mu+sigma. Held location maps to inverse Weibull.
- Free Fréchet collapse has power lambda*(n-k)-k. With the GEV tie restriction,
  lambda>=1 excludes this path. Lambda=1 is attained; lambda=infinity is Gumbel.

## Mathematics: checked numerically

Six reference density mappings passed absolute tolerance 1e-12 on specified grids.
For Fréchet the grid starts at .1; enormous negative log densities at tiny x have
roundoff larger than absolute 1e-12 despite the exact mapping.
For x=(1,2,4,7), epsilon=(1e-3,1e-6,1e-9), collapse log likelihoods:
- Pareto II: -8.32008, -3.48136, 1.35408.
- Pareto III: -6.70059, -3.18487, .27094.
- Pareto IV: -6.89598, -3.34971, .10707.
- Feller-Pareto: -9.21090, -5.75154, -2.29766.
- Nonstandard beta: -5.69514, -2.23966, 1.21422.
- Fréchet: -9.92510, -4.40512, .77261.
Slow R likelihood oracles, fixed subsets, reference tails and seeded sampling are
in the per-family tests. Fréchet uses GEV profile internals, scans positive profile
score roots, compares attained lambda=1 and Gumbel, and prefers Gumbel within 1e-8*n.

## Assumptions and limits

Inherited delegate existence/search policies are retained rather than re-proved.
Finite numerical profile root discovery is not a proof that all stationary points
were found. Free-endpoint subsets with other parameters held remain unsupported.
Pareto IV supports held scale; Feller-Pareto supports all delegate held subsets.
Selected held-scale endpoint faces retain min+scale and explicitly name that
constraint rather than suggesting a free-endpoint Pareto/power refit.
The two-shape delegates without held-parameter APIs explicitly reject added holds.
Fréchet's documented shape domain applies to free location; held location allows
all positive shapes. Its free-location sample restrictions match GEV.

## Validation

- `just cpp-test`: 5/5 ctest targets; 356 cases, 517,336 assertions, all passing.
- `just quality`: 85/85 passing, including all six new families.
- `just test`: `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 6767 ]` (existing missing-sn skip).
- Three additional focused testthat assertions pass against the current repair
  registry after final log-scale arithmetic hardening; this avoids overflow under
  extreme rescaling. The full-suite count above predates only this hardening.
- Six new goldens; no existing goldens changed. Feller-Pareto holds shape1=.3
  and shape3=.4 because its default-seed unrestricted sample selects double Pareto.
- Generic input contracts use held beta shapes for Feller-Pareto and the existing
  Burr interior sample for Pareto IV, since ten-point seeds can select faces.
- Model selection, degrees of freedom and bootstrap retention are tested for
  required bounds; the existing golden fit_args replay remains unchanged.
- All builds, full tests and quality runs used `/tmp/umvl-cpu/run`; no commits.

Logs and probe scripts: `/tmp/umvl-codex/threshold/`. The independent reference
mapping and collapse probes also passed; three scaled-tail identities passed
absolute tolerance 1e-12. Final `git diff --check` is clean.

## Ready-to-paste KNOWN_ISSUES.md rows

| extraDistr 1.10.0.5 | `qfrechet` upper log tails round through 1-p, returning infinity for finite quantiles | `extraDistr::qfrechet(-40,2,lower.tail=FALSE,log.p=TRUE)` is Inf, should be about 4.851652e8 | runtime-checked stable log inversion for `qml` |
| extraDistr 1.10.0.5 | `pfrechet` upper log tails underflow | `extraDistr::pfrechet(1e200,2,lower.tail=FALSE,log.p=TRUE)` is -Inf, should be about -921.034 | runtime-checked log survival calculation for `pml` |
| actuar 3.3-7 | `qfpareto` upper log tails lose the complementary beta probability | `actuar::qfpareto(-40,0,.3,.7,.4,lower.tail=FALSE,log.p=TRUE)` is Inf, should be about 6.892523e81 | runtime-checked smaller-beta-tail inversion for `qml` |

## Open questions

The inherited GB2 search reports unresolved for held-scale deterministic
Pareto/power samples (also reproduced directly with mlgb2); Feller-Pareto
preserves that error. Better certification of these faces remains open.
No change to the requested fitted domains. Wider free-endpoint constrained subsets
remain separate existence studies. Review numerical global-search coverage with the
same qualifications as the existing GEV and GB2 fitters.
