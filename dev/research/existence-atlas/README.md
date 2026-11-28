# Existence atlas data

The [base overview](overview.md) describes the unmodified regimes and their
proof inputs. The [modifier overview](modifier-overview.md) summarizes the
[modifier atlas](modifier-atlas.csv). Regenerate both atlases and overviews
with `python3 dev/tools/existence_atlas.py --overview`. Use `--check` to
validate the saved outputs.

## Modifier regimes

Each row combines a base family and its existing held mask with one eligible
modifier. The four `truncate:*` modifiers apply to every family. The three
`zero:*` modifiers follow the integer-support, literal-zero and construction
rule, including the declared logseries exception. For each parent mask,
`zero:modified` includes a regime with `p0` free and one with `p0` held.
`zero:inflated` likewise includes both regimes for `pi`. Generic lemmas
can be cited by individual eligible regimes.
[Modifier design](../../design/modifiers.md)

`implemented` is true when the dossier declares the route implemented for
the mask. `implementation_status` retains the scoped status and
`implementation_domain` records any declared bounded domain. The same
truncation capability applies to all four bound patterns within its domain.
For example, zeta requires the resolved window to have the form `[1,N]`.
The table describes the general truncation capability. The `[1,Inf]` alias
uses the zero-truncated route, including for negative binomial with free
size. Implementation metadata carries no proof grade.

## Modifier proof input

[modifier-proofs.csv](modifier-proofs.csv) takes reviewed claims. Its columns
match [proofs.csv](proofs.csv) with `modifier` inserted after `held`:

```text
family,held,modifier,question,claim,direction,status,evidence,frontier,origin
```

Use an empty held field or `all free` for the all-free mask, and otherwise
join the sorted public held coefficient names with `+`, including `p0`
or `pi` where applicable. The modifier
keys are `truncate:known`, `truncate:lower-estimated`,
`truncate:upper-estimated`, `truncate:both-estimated`, `zero:truncated`,
`zero:modified`, and `zero:inflated`.

Each regime takes at most one row per question. The main questions are
`criterion` (attainment), `failure` (failure modes), and `uniqueness`
(cardinality). Use `case:<name>` for an additional scoped claim. Named
cases appear in the JSON `proved_cases` column and leave the main question
counts unchanged. Directions are `iff`, `exact`, `one-sided`, and
`open`. Statuses are `written`, `cited`, and `unproved`. A written
claim needs a repository-relative or atlas-relative `file.md#anchor`
reference, with multiple references separated by a semicolon and a space.
A cited claim needs a bibliographic reference. Record outstanding proof
obligations in `frontier` and regime provenance in `origin` when relevant.
An absent claim is `unproved` with direction `open`.

To validate a candidate file without changing the saved inputs or outputs,
run `python3 dev/tools/existence_atlas.py --check-modifier-proofs PATH`.

## Modifier Lean coverage

The [Lean map fragments](modifier-lean-map.d/) use the base Lean-map
columns with `modifier` inserted after `regime`. Use the modifier keys above.
For a concrete regime, `coverage` is a JSON object keyed by question or
`case:<name>`, for example `{"criterion": "partial"}`. A family-specific
`generic` regime prefixes each key with the held mask, for example
`{"all free/criterion": "partial"}`.

Review the declaration against the written claim in the exact family,
mask and modifier before assigning a grade. Use `full` when the statement
proves the whole written claim, and `partial` for a narrower result.
Leave helper coverage blank. The generator validates the theorem, source
line and recorded Lean signature against the source, and checks that
each graded key has a corresponding proof row. A full grade promotes
the written claim to `lean` and supplies its theorem names as evidence.
Partial evidence is recorded in the `*_lean_partial` fields or the
`lean_partial` field of a named case. The overview counts full and
partial-only evidence for the three main questions.
