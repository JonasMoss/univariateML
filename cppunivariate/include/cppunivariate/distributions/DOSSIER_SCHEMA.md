# dossier.md schema

`dossier.md` is the human-prose source of truth for a distribution.
One file lives next to its `mle.hpp` / `dpqr.hpp` / `metadata.hpp`
in `cppunivariate/include/cppunivariate/distributions/<id>/`. It
feeds (today) the auto-generated R roxygen blocks via
`univariateML/scripts/codegen_wrappers.R` and (later) the Python
shell's docstrings.

The C++ artifact does not read `dossier.md` at build time — it is
prose, not source. Conversion to Rd / reST / HTML happens in the R
and Python codegen layers.

## Guiding principle: document only what cannot be read off the code

The code is easier to test than the docs. Anything *derivable* from
the existing C++ files — parameter names, supports, defaults, the
PDF, the algorithm, the status-enum trigger conditions — belongs in
those files, not here. Repeating it in markdown invites drift and
adds nothing a reader couldn't get by following a `\seealso` link
or reading the header.

What the dossier *adds* on top of code:

- **Semantic intent** — what the distribution is in plain English,
  what it models. (Not in any header.)
- **Mathematical relationships to other distributions** — limit
  cases, special-case reductions. (Lomax → Exponential as
  $\kappa \to \infty$; negative-binomial → Poisson; etc.)
- **Interpretation of failure modes** — when the status enum fires,
  what should the user do next. The trigger condition is in the
  code; the *action* is not.
- **References** — the papers that justify the estimator's
  properties.
- **See-also pointers** — related ML wrappers in the package.

If a candidate sentence does not fall into one of these five
buckets, it almost certainly belongs in code (or nowhere).

## Section schema

| Section          | Required | Maps to                                |
|------------------|:--------:|----------------------------------------|
| `# <Name>`       | yes      | `\title` (auto: "<Name> maximum likelihood estimation") |
| `## Description` | yes      | `\description`                         |
| `## Edge cases`  | no       | `\details`                             |
| `## See also`    | no       | `\seealso`                             |
| `## References`  | no       | `\references`                          |

`<Name>` must match `name` declared in the dossier's
`metadata.hpp` (e.g. `# Lomax`, not `# lomax distribution`).

### Required sections

- **Title.** Just `# <Name>`. The wrapper title
  ("<Name> maximum likelihood estimation") is auto-formed.

- **Description.** One or two plain-English sentences: what the
  distribution is and what it models. No math, no parameter list.
  Surfaces as `\description{}` in Rd and the docstring summary in
  Python.

### Optional sections

- **Edge cases.** Non-derivable mathematical relationships and
  status-enum *interpretation*. Limit cases ("X converges to Y as
  $\theta \to \infty$"), regimes in which the MLE does not exist,
  and what the user should do instead. Reference the status enum
  by name (e.g. `LomaxStatus::MeanVarianceCondition`) so a reader
  can map prose to API.

- **See also.** Related ML wrappers in the package, especially
  limit cases that are themselves distributions (e.g. `mlexp` as
  the $\kappa \to \infty$ limit of `mllomax`). Disambiguation
  ("not the same as `mlpareto`") does not belong here — leave it
  out or fold it into Description.

- **References.** One bibliographic entry per line. Free-form
  prose; the codegen does not parse them.

### Extending the schema

A dossier may introduce a section not in the table above
(`## Identifiability`, `## Tail behaviour`, `## Algorithm` if the
algorithm-docs question is reopened). The codegen falls through to
`\details`. Use new sections sparingly — most distributions need
only Description plus maybe Edge cases.

## Math conventions

The dossier source is toolchain-agnostic; do not write Rd-specific
or Sphinx-specific markup directly. The codegen rewrites math into
the right form per consumer.

| Math kind | Source delimiter | Rd output    | Sphinx output     |
|-----------|------------------|--------------|-------------------|
| Inline    | `$x$`            | `\eqn{x}`    | `` :math:`x` ``   |
| Display   | `$$ ... $$`      | `\deqn{...}` | `.. math::` block |

Use LaTeX inside math (`\lambda`, `\kappa`, `\overline{x}`,
`\bar x`). Parameter names should match `metadata.hpp` so prose
lines up with the auto-generated `\arguments` block.

## What the dossier does *not* contain

Already covered elsewhere — do not duplicate:

- **PDF / CDF / quantile formulas.** In `dpqr.hpp`. Users land on
  the upstream `?dlomax` / `?dburr` page via the wrapper's
  `\seealso` (configured in `univariateML/metadata/<id>.json`).
- **Algorithm description.** In `mle.hpp` — Newton path,
  reparametrisations, init formulas. Out of scope for the dossier
  for now; may be revisited.
- **Parameter tables.** Names, defaults, and support come from
  `metadata.hpp`.
- **Moments (mean, variance, …).** Wiki-page material; not what
  this package documents.
- **Status-enum *definitions*.** Trigger conditions are in the
  enum and header comment of `mle.hpp`. The dossier covers
  *interpretation*, not definition.
- **Implementation derivations** (Hessian blocks, sympy-derived
  gradients). Comments in `mle.hpp`.
- **Test notes.** Tests live in `cppunivariate/tests/` and the
  golden JSONs.
- **`\arguments` / `\examples` / `\value` / `\usage` / `\author`.**
  Wrapper-mechanical; owned by `codegen_wrappers.R`.

## Worked example

`distributions/lomax/dossier.md` is the canonical example: a tight
Description, an Edge cases section that documents the
$\kappa \to \infty$ exponential limit and the
`MeanVarianceCondition` interpretation, one See-also entry, one
reference.

## Status

This schema is **provisional**. The pipeline that consumes it
(`scripts/parse_dossier.R`, the pandoc lua filter, the
`gen_doc()` rewrite in `codegen_wrappers.R`) is the next phase;
until then, dossiers are exercised manually via markdown preview.
The schema may be revised once a second distribution has been
piloted under it.
