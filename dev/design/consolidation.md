# Consolidation plan (agreed 2026-11-12)

Follows the [repository audit](../research/evaluations/2026-11-12-repo-audit-report.md).
Jonas set the order: clean up, add fuzzers, then develop against two test
layers. New breadth is held, at least partly, until step 8. Decisions are in
the [decision log](../roadmap/decisions.md) under 2026-11-12.

Everything here is a target until its step lands. Each step says what must be
true when it is done.

## Outcome model (target)

One outcome type for every family, in the core, replacing the per-family
status enums. Kinds:

| kind | meaning | flags and payload |
|---|---|---|
| estimate | a maximizer within `tol` | `certified` (global claim), `unique`; when not unique, every maximizer or a description of the set |
| limit | the supremum is a named limit model | `certified`: proved, or only "no gain above the stated margin" |
| nonexistence | proved: no maximizer and no limit model to name | reason (unbounded, unattained) |
| budget | work budget exhausted | witnesses and bounds |
| failure | invalid input, numerical or accuracy failure | reason; `attainable_tol` for accuracy stops |

Rules agreed with Jonas:

- **Limit within the margin.** A limit stop decided only by the likelihood
  margin stays a limit stop with `certified = FALSE`, and its message says that
  no improvement above the margin was found, not that none exists.
- **Non-unique maxima.** Proved non-uniqueness is an estimate with
  `unique = FALSE`. A finite set of maximizers is listed; a continuum is
  described (for example by its endpoints). Each family states its canonical
  representative for `coef()`. `vcov` and `confint` refuse; model selection
  works. Two candidates the certificate cannot separate are a different case:
  the better one with `certified = FALSE` and both attached.
- **`tol` is a ceiling** on the coefficient error. A fitter may deliver more
  accuracy than asked. The default stays `1e-10` until the strict layer shows
  how often default fits stop on tolerance or conditioning.
- **Pathological families** use this type and get no enum of their own. They
  are its acceptance test. No mixtures for now.

The regime table in [relaxed-likelihood.tex](../research/relaxed-likelihood.tex)
(R1 to R8) is the reference for the kinds and reasons.

## Steps

1. **Land the running jobs; freeze cross-cutting files.** Merge or park
   bnb-loglik, ew-fit, modifiers-2, pgw-dpqr, stats-route and seam-rerun. After
   that, no job edits `bindings/common/`, the code generators or the status
   enums until step 4 is done. Hygiene: delete merged branches, stop tracking
   generated JS metadata, correct stale counts and `STATUS.md`.
   Done when: no worktree is open and master passes the dev tier.
2. **Decisions.** Recorded 2026-11-12; `AGENTS.md` Principle 1 updated.
3. **Snapshot fuzzer.** Seeded and deterministic, every family and supported
   hold subset: the fit-audit archive plus magnitude classes (subnormal, huge,
   near-constant, wide spread, heavy ties). It records status, coefficients,
   `logLik` and flags, and judges nothing.
   Done when: two runs on the same build give identical snapshots, and the
   native and double-only builds are each recorded.
4. **Shared outcome type, no numerical change.** First a per-family mapping
   from the existing enum to the outcome, in the core, checked against the kind
   strings the bindings produce today. Then the bindings read the outcome.
   Then the family enums go. The snapshot is unchanged at every commit.
5. **Uniform controls and visible flags.** `tol` and `iterlim` only
   (TASK-150.1); `certified` and `unique` in `print`, `summary` and
   `model_select`; the flag forwarded for `st` and `tlambda`; ties become
   estimates with `unique = FALSE`. Behaviour changes, so the snapshot diff is
   reviewed and recorded.
6. **Strict layer with a ratchet.** The fuzzer starts judging: `Ok` invariants
   (finite, in the domain, `logLik` equal to the summed density, within `tol`
   of a high-precision refinement), the same status for the same phenomenon
   across families, the seam check and the archive replay. Known failures are a
   tracked list that may only shrink; a new failure blocks a merge. The current
   tiers stay the merge gate. First entries: the gamma, norm and logis defects
   of the audit.
7. **Pathological families.** The sine family (no maximizer for any sample),
   and Ferguson's example if the "`Ok` but inconsistent" case should be
   represented; its failure is statistical and belongs in the dossier, not in
   the fit outcome. Excluded from `model_select` by default.
8. **Certificate replay; breadth resumes.** Replay certificate comparisons in
   high precision on the archive and add the result to the strict layer. Pilot
   outward rounding on one or two families. Then pgw, qgauss, modifier
   retirements and the other banked families continue against both layers.
