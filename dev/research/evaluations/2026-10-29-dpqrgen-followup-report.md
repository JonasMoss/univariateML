# TASK-78.8 review follow-up (base 0c3f76b)

## Changes

- Removed `bindings/r/scripts/dpqr_api.R` and `R/dpqr_documentation.R`.
  All 18 families now own their export title, numeric defaults, coefficient
  descriptions and algorithm/support notes under their README's `r.export`.
- Codegen derives all formals from README coefficient order plus defaults;
  shared description, argument/return text and evaluation policy live once
  in `codegen_dpqr.R`, with continuous/discrete p/q conventions.
- Extended the existing P1 validator in `load_distributions.R`; there is no
  separate P1 schema validator under dev/validation/quality/. Absent/false means internal;
  a mapping means exported. Unknown keys, malformed maps, non-coefficient
  names, nonnumeric/missing defaults and malformed text are rejected.
  The schema documents all keys, types and fallbacks. Developer schema tests
  cover 53 expectations, including empty mappings and rejection of true/null.
- Dropped logitnorm's `...` from d/p/q/r. These APIs were never released;
  silently swallowed misspelt arguments now raise unused-argument errors.
- No C++/RNG, fitting, numerical, resolver, truth-table or golden changes.

## Rendered documentation comparison against 0c3f76b

All 18 Rd files and their rendered text were diffed. Titles, aliases, topic
names and usage stay unchanged except the four logitnorm ellipses. Every
coefficient constraint and algorithm/special-member note is retained.

Differences common to all topics: the source-file comment points only to
ml03_dpqr.R; line wrapping/paragraph grouping change; x/q, p, n, flags and
return text are unified. The return explicitly includes densities. Shared
policy spells out zero lengths, recycling, first logical element, NA/NaN,
invalid-input warnings, log densities/tails and R RNG/set.seed. Continuous
p/q tail inversion and discrete flooring/integer-warning/quantile/infinite-
endpoint conventions are now stated consistently. These clarify previously
shorter boilerplate; family-specific description text moves into Details.

| Topic(s) | Family-specific text retained/reorganized |
|---|---|
| cmb | 0..size support, size cap, prob/nu domains, binomial member, finite log normalizer, table reuse, summed tails and inversion. |
| cmp | CMP normalizer, summed tails and inversion; moved normalizer from description to Details. |
| emg | Normal plus exponential definition, Newton/bisection quantiles and additive sampler. |
| foldnorm | Absolute normal, sign unidentifiability, half-normal member, bisection and absolute-normal sampler. |
| ged, sged, snorm, sstd, std | fGarch mean/SD parameterisation moves to Details; all coefficient text and evaluation domains retained. |
| logitnorm | Finiteness and transformed sampling retained; combined mu/sigma documentation split into location/positive-scale entries; ellipsis compatibility paragraph removed intentionally. |
| naka | Finiteness and transformed sampling retained; shape/scale text split, retaining scale's mean-squared-amplitude meaning. |
| sgomp | Exponential member, no location, b rate, bisection and maximum-of-exponential/Gumbel sampler retained. |
| skellam | Signed Poisson limits and subtractive sampler retained; combined rate entry split into equivalent singular entries. |
| sn | Direct parameters vs Fernandez--Steel, Owen's T/quadrature, cancellation avoidance, bisection and two-normal sampler retained. |
| tlambda | Full quantile formula, logistic limit, smaller-log-tail safeguarded Newton and uniform sampler retained. |
| yulesimon, zeta | Support at one, no finite upper endpoint, double grid beyond 2^53, overflow to Inf and inversion retained. |
| zipf | Finite support/cap, finite nonnegative shape, uniform member, summed tails and inversion retained. |

Other coefficient descriptions are verbatim. There are no unexplained Rd
changes; shared wording and the intentional logitnorm removal account for
all differences.

## Validation

Simbox: schema 53/53; generic truth 6,247/6,247, no warnings/skips.
`just test`: 28,971 pass, 3 known failures (sstd frozen reference; bnbinom
and std goldens), 0 warnings/skips: identical to the previous final run.
Audit: NAMESPACE identical; all 336 formals match except logitnorm ellipses;
all four misspelling checks pass; deterministic regeneration across 85 files.
`just check`: 28,958 pass, the same 3 failures, 1 CRAN-only skip and no test
warnings. Overall 1 ERROR (known tests), 1 existing Rd cross-reference WARNING,
no NOTEs; the warning text matches the previous final check exactly.
The concluding API/NAMESPACE/determinism audit passed after final regeneration.
Simbox remained available throughout; no local fallback or local tests ran.

Logs, baseline help, individual Rd diffs and before/after rendered text:
`/tmp/umvl-codex/dpqrgen/followup/`.
No commits or git writes were made.

Pane display was attempted; the viewer still rejects this worktree root (403).
