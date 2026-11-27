# Modifier axis for the existence atlas

TASK-444 adds 2,340 modifier regimes over the 520 base regimes at
`dc2cd865`. Each of the four truncation patterns has 520 eligible regimes.
The 12 zero-eligible families, including the logseries exception, contribute
52 zero-truncated regimes, 104 zero-modified regimes and 104 zero-inflated
regimes. Zero modification includes both free and held `p0` for every
eligible parent mask, and zero inflation does the same for `pi`. The dossier
metadata marks 124 modifier regimes implemented within their declared scope. The separate
modifier overview preserves the base overview byte for byte.
[Modifier atlas](../existence-atlas/modifier-atlas.csv) ·
[Modifier overview](../existence-atlas/modifier-overview.md) ·
[Format](../existence-atlas/README.md)

The generator records implementation scope independently of attainment,
failure and cardinality evidence. For example, the negative binomial
general truncation capability requires size held, while the
`[1,Inf]` alias uses its separately implemented zero-truncated route.
The reviewed input has only its header, so all three questions remain open
in every generated modifier regime. Scoped case claims have a dedicated
JSON field and leave the main question totals unchanged.
[Modifier proof input](../existence-atlas/modifier-proofs.csv) ·
[Modifier design](../../design/modifiers.md)

## Draft import checks

The alternate reader validated copies under `build/tmp` without changing
the reviewed input. Acceptance here means that the key, fields and
references passed validation. Both drafts remain under independent
mathematical review.

| Draft | Rows | Accepted by reader | Rejected |
| --- | ---: | ---: | ---: |
| Truncated exponential families | 207 | 198 | 9 |
| Zero modification | 249 | 249 | 0 |
| Both drafts combined | 456 | 447 | 9 |

All 42 proposed held-atom claims passed for binomial, negative binomial,
geometric and Poisson. The combined check found the same nine rejections
and no duplicate keys across the drafts.

The nine rejected rows use `generic` as the family. They are candidates
for a general theorems note on tightening windows to sample extremes,
transferring failure modes and characterizing ties with looser windows.

| Generic modifier | Rejected questions |
| --- | --- |
| `truncate:both-estimated` | `criterion`, `failure`, `uniqueness` |
| `truncate:lower-estimated` | `criterion`, `failure`, `uniqueness` |
| `truncate:upper-estimated` | `criterion`, `failure`, `uniqueness` |

[Truncation draft](2026-11-27-trunc-expfam-math-data/proofs.csv) ·
[Estimated-window results](2026-11-27-trunc-expfam-math-report.md#all-applicable-truncateestimated) ·
[Zero modification draft](2026-11-27-zero-mod-math-data/proofs.csv)

## Verification

- Two foreground workstation generations with `--overview` produced
  identical SHA-256 hashes for both atlas CSVs and both overviews.
  `git diff --exit-code -- dev/research/existence-atlas/atlas.csv dev/research/existence-atlas/overview.md`
  confirmed that both base outputs match HEAD.
- `python3 dev/tools/existence_atlas.py --check` passed on the
  workstation with the header-only proof file. A temporary smoke check
  verified complete eligible regime coverage, sorted atom-hold masks with
  inherited implementation flags, the logseries and negative
  binomial implementation scopes, and a written proof joining an
  unimplemented route. It also checked rejection of ineligible modifiers,
  bad statuses and directions, missing anchors, and duplicate keys.
- The workstation link checker reported the known `P+Q` false positive
  in the beta-prime report and 47 inherited entries. The beta-prime source
  is identical at HEAD. The known Batteries path did not appear in this
  worktree's results.
- The job used Python checks only, as specified by the no-build scope.

## Open for the reviewed import

The reviewed math rows are the missing input for proof coverage. The
generic estimated-window results can supply evidence for claims after
specialization to an eligible family and mask.
