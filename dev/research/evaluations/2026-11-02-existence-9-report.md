# Existence-9: Student and exponential-power family prose

Completed 2026-11-02 (Europe/Oslo), TASK-101.6.4.1.

## Scope and tested source

The six dossiers `std`, `sstd`, `ged`, `sged`, `lst` and `ht` now have
`existence` records, revised limit paths and detectors, coefficient
interpretations, and conditional-fitting explanations. R-specific material
is under **R notes**; dated implementation choices and their evidence are
under **Decision history**. Existing density-limit derivations, endpoint
penalties, numerical qualifications and historical evidence remain accessible.

All six joint uniqueness labels are **unknown**. Conditional proofs are
stated separately: a monotone scale equation or fixed-shape convexity does
not certify a free-shape or free-skew search.

The tested source is branch `codex/existence-9`, HEAD
`422733c85731847958aae9caed0b302e6604ff29`, plus the uncommitted six-dossier
diff. Its SHA-256, from `git diff --` followed by the six README paths in
order `std sstd ged sged lst ht`, is
`144697d33cf32fae32a3181d544d4e35b3c09fdc12f5a192f6852538e7b7bab8`.
No commit, board edit, package installation, sub-agent work or simbox run
was performed.

A parsed-YAML comparison against HEAD confirmed that removing only
`existence`, limit `when`/`detect`, hold reasons, parameter advice and
fitting-restriction reasons leaves identical metadata for every family.
No statuses, bounds, held sets, numerical/firmness flags, densities,
relations, examples, fitting metadata, inference metadata or goldens changed.
The only changed advice is the required `ht` endpoint advice. Neither it nor
the revised restriction reasons is duplicated in the six common binding
headers or matched literally by their R tests, so no binding strings or test
regexes needed edits.

## Family results

### `std`

**Statement:** A finite joint MLE is data-dependent; collapse, normal and
finite-scale t with two degrees of freedom are competing limits, while held
location and shape give a unique positive scale maximum exactly when
`nu*(n-k) > k`.

**Grade:** Joint uniqueness **unknown**; fixed-location/fixed-shape scale
existence and uniqueness **proved**. For ordinary t scale `a`, the collapse
power is `a^(nu*(n-k)-k)`. With nonzero residuals `d_i`, the log-scale score
is `sum((nu+1)*d_i^2/(nu*a^2+d_i^2))-n`. It decreases strictly from
`nu*(n-k)-k` to `-n`. This proves the conditional root criterion, including
the finite unattained equality supremum with location held. The existing
normal and t2 density-limit arguments do not prove the global comparisons.

Holding standard deviation excludes a finite-scale t2 law, but free shape
can still shrink the effective ordinary scale as `nu` decreases to two.
Holding shape removes the two shape escapes. Other held subsets retain
numerical profiles and only their reachable faces.

**Status changes:** None. **Disagreements:** The former fixed-shape limit
text put the tie-count condition in `when`; it is now in `detect`, with a
separate scale path. Free-shape equality is described in the prose and
fitter but has no separately scoped free-shape equality record in `limits`.

**Open:** A global certificate for free location/shape profiles and a full
classification of free-shape tie equality. Successful stationarity checks
or a numerical boundary comparison do not supply either result.

### `sstd`

**Statement:** Joint finite attainment can fail through collapse or the
skew-normal, half-t/half-normal and two-piece t2 faces; held coefficients
restrict their moment constraints, and fixed mean and both shapes permit a
positive scale maximum when `nu*(n-k) > k`.

**Grade:** Joint existence classification and uniqueness **unknown**;
conditional endpoint-vanishing existence argument **argued**, with the
listed density limits **proved** as in the retained derivations. At fixed
mean and shapes, a positive collapse exponent makes the likelihood vanish
at scale zero, and it also vanishes at infinite scale, so continuity gives
an interior maximum. This argument does not assert uniqueness of the
implemented scale search. With scale and shapes held, the continuous
location likelihood vanishes at both location infinities and attains a
maximum. Holding mean preserves the moment, not the two-piece join point.

**Status changes:** None. **Disagreements:** The old limit clauses mixed
collapse conditions with paths and used an undirected “half-normal corner”
for both signs. Paths, directions and detectors are now explicit. The old
prose called the skew-normal face “fitted exactly” and described simulated
face selection as non-existence; both now state the numerical qualification.
The held-standard-deviation collapse caveat from the conditional analysis
is retained.

**Open:** Exhaustiveness of simultaneous shape/skew escapes, tie-equality
classification, and global bounds for the numerical conditional and joint
searches. The existing finite grids are not global certificates.

### `ged`

**Statement:** The unrestricted joint likelihood is unbounded; estimated
shape is restricted to at least one, where constant data collapse and a
uniform limit may prevent finite attainment; fixed shape above one gives a
unique location-scale maximum for nonconstant data.

**Grade:** Restricted joint uniqueness **unknown**; fixed-shape conditional
claims **proved**. For fixed shape, location minimizes the sum of powered
absolute residuals. Above one this is strictly convex, at one its minimizers
are medians, and below one a minimum can be selected at an observation.
Free scale is an explicit positive multiple of the residual power mean.
Consequently the nonconstant fixed-shape fit above one is unique; a median
interval can give nonunique locations at shape one. With location and shape
held, free scale is unique unless every residual vanishes. A held scale
removes scale collapse. The dossier now explicitly distinguishes accepted
held shapes below one from restricted estimated shapes.

The unrestricted non-existence citation was verified against the
[publisher record](https://www.sciencedirect.com/science/article/abs/pii/S0167715215002886):
Tumlinson, Samuel E. (2015). “On the non-existence of maximum likelihood
estimates for the extended exponential power distribution and its
generalizations.” *Statistics & Probability Letters*, **107**, 111–114.
DOI: 10.1016/j.spl.2015.08.009. One web search was used for this verification.
This reference is not used to assert uniqueness in the restricted domain.

**Status changes:** None. **Disagreements:** “Shapes below one are not fitted”
was too broad: the conditional implementation accepts held positive shapes
below one. The text now distinguishes holding from estimating. The declared
constant-sample failure status `InvalidStart` is unchanged; the mathematical
reason is unbounded scale likelihood, not the name of that status.

**Open:** Global certification of the outer shape search and its uniform
comparison. The endpoint penalties with held moments remain part of that
comparison and are not replaced by an unconstrained uniform likelihood.

### `sged`

**Statement:** The unrestricted joint likelihood is unbounded; under the
estimated-shape restriction, collapse, uniform and half-GED limits can
prevent a finite maximum; held coefficients retain their reachable faces
and moment constraints.

**Grade:** Joint existence and uniqueness **unknown**; fixed-shape/skew
inverse-scale concavity **proved** by the retained convex-piece argument;
conditional scale/location attainment **argued**. With mean and both shapes
held and at least one nonzero residual, likelihood vanishes at both scale
endpoints, so a positive-scale maximum exists. With scale and both shapes
held, the location likelihood vanishes at both infinities. Neither fact
certifies the finite scans. The unrestricted non-existence result follows
also by including the symmetric GED subfamily; see the verified Tumlinson
reference above.

**Status changes:** None. **Disagreements:** The old half-face prose said a
numerically winning face establishes absence of an MLE. It now separates
nonattainment of a true half-face supremum from numerical face selection.
The unresolved constrained-uniform endpoint conditions are now data
detectors, separate from the infinite-shape paths.

**Open:** Coupled endpoint approach rates when a held limiting uniform width
equals the sample range; global mode/shape/skew bounds; and uniqueness beyond
the established conditional convexity scopes. Existing numerical
`ScaleBoundary` records remain numerical and do not assert non-existence.

### `lst`

**Statement:** Joint fitting on shape at least one can encounter collapse or
a normal supremum; held location and shape give a unique scale maximum
exactly when `df*(n-k) > k`, while holding location removes the estimated-shape
lower bound and makes any exact location tie dangerous with free scale.

**Grade:** Joint uniqueness **unknown**; conditional scale criterion
**proved** by the same strictly decreasing score as for ordinary t scale
above. At equality the score is negative at every positive scale when
location is held. Holding scale removes collapse. Neither monotone EM nor
multistart shape/location searches proves global uniqueness.

**Status changes:** None. **Disagreements:** The former restriction reason
claimed shape at least one prevents free-location collapse without the tie
qualification. The new text identifies the excluded small-shape mechanism
and retains separate tie diagnostics. Equality is not universally
non-existence: the existing balanced two-point Cauchy special case has
attained, nonunique fits. The equality detector and all-free rejection
prose now distinguish that fact from the fitter's diagnostic policy.

**Open:** Global shape/location certification and the general equality
classification. The leading collapse record uses `Unbounded`, whereas the
all-free sample guard can return `NoMLE`, including equality; a future
status-scope inventory should record that guard separately. No structural
hold exception was added for a data-dependent rejection.

### `ht`

**Statement:** A free endpoint is the sample minimum; shape estimation with
that endpoint free is restricted to at least one, and finite attainment
still depends on endpoint ties and the half-normal comparison; held shape
gives a unique positive scale root exactly when `nu*(n-k) > k`.

**Grade:** Joint shape uniqueness **unknown**; endpoint maximization,
conditional scale criterion and its equality supremum **proved**. The
density strictly decreases with distance from the endpoint, establishing
its free estimate. The log-scale score strictly decreases between the
stated limits. With held endpoint and free shape/scale, any endpoint tie
allows sufficiently small positive shape to make the collapse exponent
negative. Holding scale removes that collapse. The retained two-mode sample
explicitly rules out general profile unimodality.

**Status changes:** None. **Disagreements:** The old restriction reason
suggested that shape at least one prevents collapse without stating the
minimum-multiplicity condition. It now states the ordinary `2*k < n` scope.
The old prose incorrectly described both shape and scale held as likelihood
evaluation even with endpoint free; it now distinguishes endpoint estimation.
The equality record is scoped to held shape, although the fitter also
compares a shape-one scale-zero candidate when shape is free and `2*k = n`.
That missing free-shape scope needs a separate metadata record in a job
authorized to change it. The frozen limit `name` also contains R calls and
misleading zero-endpoint advice after arbitrary translation; the new R notes
correctly require preservation of the actual held endpoint.

**Open:** Computable upper bounds on the shape profile over every interval
and its large-shape tail, including inner-solve and floating-point errors.
The retained derivative-bound discussion identifies the missing enclosure;
a finite grid alone is insufficient.

## Generated-help follow-up outside this job's ownership

`bindings/r/scripts/codegen_wrappers.R:53–77` still expects limit clauses.
It prepends “When” and appends “as” and punctuation to the new full sentences,
and wraps a detector without dollar-delimited math as code. For example,
`mlht.Rd` contains “as The shape … scale..”. This requires a generator change
that renders sentence-valued paths and detectors directly as prose.

The same fallback unconditionally writes “the maximum likelihood estimator
does not exist” for limits without `numerical: true`. That is too strong for
an uncertified numerical comparison or unresolved equality boundary, even
when the existence statement and detector explicitly qualify the search.
The generator should distinguish a proved boundary law from a certified
sample-level decision, preserve uncertainty and avoid treating a numerical
failure as a non-existence theorem. Neither codegen nor the frozen flags
were changed here. These are concrete open output-contract issues, not
claims that the numerical searches have been certified by this prose pass.

## Validation

Every computing check ran locally on **JonasBI**, through the shared CPU
slot helpers, with at most four cores and single-thread BLAS/OpenMP.
`run4` applies `nice -n 10`. Temporary/runtime directories were
`$PWD/build/tmp`, and logs and scratch stayed in `build/scratch/`.
Environment: R 4.6.1 (2026-06-24), Python 3.14.5, Ubuntu GCC 15.2.0.
Final runs finished on 2026-11-02 local time (2026-11-01 UTC).

| Check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Exit 0; package, metadata, wrappers and Rd regenerated. |
| Requested targeted `testthat::test_local` filter | **FAIL 0, WARN 0, SKIP 19, PASS 8350**; 91.8 seconds. All 19 skips require the full tier. |
| Requested all-`ml*.Rd` `tools::checkRd` scan, excluding Non-ASCII diagnostics | **No diagnostics for any of these six families.** Existing diagnostics were confined to `mlskellam.Rd` and `mlst.Rd`; those families were not edited. Exit 0. |
| `python3 site/scripts/data.py` | Exit 0; derived 99 families. Reported symbolic-density fallbacks are informational. |
| `just relations` | **157 edges, 0 failed.** |
| `just cpp-test` | **106/106 dev shards passed**; 50.44 seconds test time. |
| Final `just test` | **FAIL 0, WARN 0, SKIP 151, PASS 20106**; 105.36 seconds test elapsed time. |
| Protected-field YAML comparison against HEAD | All six passed; no machine-readable changes outside the authorized prose fields. |
| `git diff --check` | Passed. |

After the initial regeneration, the remaining commands ran consecutively
inside `/tmp/umvl-cpu/run4 bash build/scratch/validate.sh`; there were no
nested slot locks or external waiting loops within the reservation. The
script used the requested filter
`dist-(std|sstd|ged|sged|lst|ht)|conformance|restricted|generated-dpqr|parameter`
and set `UMVL_DISTRIBUTIONS_DIR` to the worktree dossiers. Final `just test`
performed its own regeneration before the dev suite. Existing tests supplied
their own fixed seeds and tolerances; no new numerical experiment was used
to promote a claim to proof. These passes establish regression and metadata
compatibility, not global optimality of the existing fitters.

Full-tier, no-long-double and JavaScript suites were not required for this
documentation-only change: no fitting code, d/p/q/r, test infrastructure or
binding behavior changed. Full-tier oracle coverage is therefore explicitly
not claimed. All started build/test commands finished successfully before
this report was written.

The requested artifact-pane opener was attempted after writing this report.
It returned HTTP 403: this worktree is outside the viewer's configured
readable roots. The report was not copied outside the worktree and no
viewer configuration was changed.
