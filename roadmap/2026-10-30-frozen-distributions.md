# Frozen distributions through the fitters

**Status:** landed 2026-10-30 (TASK-99, merge d0226d2). Defaults: standard
members (TASK-104, decision log).

No d/p/q/r are exported (decision 2026-10-30). Users still need to
evaluate and simulate a family in its own parametrization, and examples
need data without `univariateML:::r<id>`. Instead of exporting densities,
every fitter also builds a fully specified ("frozen") model, as in
`scipy.stats.gamma(a = 2)` or R's distributions3.

## Rule

Without data nothing is estimated; an unspecified coefficient takes its
README `simulate:` value. With data, behaviour is unchanged.

| Call | Result |
|---|---|
| `mlgamma(x)` | fit |
| `mlgamma(x, shape = 2)` | fit with `shape` held |
| `mlgamma(x, shape = 2, scale = 1)` | all held: nothing estimated, `logLik` at the given values, df = 0 (AIC and likelihood-ratio tests work) |
| `mlgamma(shape = 3)` | frozen model, `scale` at its default |
| `mlgamma()` | frozen model at all defaults |

`dml`/`pml`/`qml`/`rml`/`plot` work on frozen models. `rml(n, mlX())`
replaces `simulate_default("mlX", n)` (never released; remove it).

## Details

- Frozen models carry no data (`n = 0` and a marker attribute). `logLik`,
  `AIC`, `BIC`, `nobs`, `confint`, `bootstrapml` and `summary` stop with a
  clear message; `print` says the model is fixed, not fitted.
- Validate against the declared `domain`, not `fit_domain`: restrictions
  exist for the existence of the MLE (for example GED $\nu \ge 1$), not
  for the distribution.
- Coefficients must be named; with `x` missing, an unnamed argument is an
  error (`mlgamma(2, 1)` would otherwise read 2 as data).
- Required design and threshold coefficients (R3) are required only with
  data; without data they default like the others. Document it.
- The `simulate:` values become user-facing defaults: review them once for
  sensible, stable values and list them on each help page (generated).
- Generic: implemented once in the wrapper codegen from README front
  matter; no per-family code. One generic test over all 98 families:
  frozen-model `dml`/`pml`/`qml` equal the internal generated functions,
  and `rml` reproduces under `set.seed`.
- Examples: real data where the family is a plausible model; otherwise
  `x <- rml(n, mlX(<coefficients>))` followed by `mlX(x)`. Every README
  needs `examples:` (codegen fails otherwise; about 15 have none).
  Drop contrived pairings (`mlbinom(ChickWeight$weight, size = 400)`,
  `precip` for every positive family).

## Trigger to revisit

Start after TASK-78.14 (boundary audit), which edits many of the same
README and test files.

## Verification

Generic frozen-model test passes for 98 families; `just check` runs every
example; no `:::` and no `simulate_default` remain in examples or docs;
NAMESPACE gains nothing except possibly print methods.
