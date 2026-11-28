# Independent review of estimated truncation for GED and skew families

TASK-472 reviewed 621 TASK-469 proposals across seven families. The join file contains 621 rows, with 432 accepted, 189 revised in scope or wording, and zero rejected. Its grades are 105 iff, 426 exact and 90 one-sided. All joined rows are written proofs. [Reviewed CSV](2026-11-28-mod-rev12a-data/reviewed.csv)

**Several proposed failure lists include excluded faces that the holds make impossible.** In particular held-scale fixed-df LST has only the bounded uniform as a positive excluded height, and free-location LST cannot approach df zero. The two-piece Gaussian and stretched-exponential rows also require mask-specific restrictions on broad, tilted and sharp limits.

For each proposed design I matched the parent key and checked the corresponding reviewed known-window result wherever that question had a known-window row. The proofs use one unchanged parent density and coefficient domain across windows. All seven kernels are continuous and strictly positive at every finite point, including two-piece joins. An inward segment at either cut gives positive mass, so neither the zero-density endpoint nor the positive-nominal-width zero-overlap obstruction can occur here. At positive width joint maximization reduces to the tightened window, even when the coefficient optimizer escapes. Ties at the sample extreme change the product and the collapse exponent while leaving the bound constraint intact. At coincident cuts the inward one-sided expansion has mass asymptotic to density times width and yields a divergent product. An estimated cut is unique on every attained branch. These mathematical regimes are eligible for the atlas. [Modifier design](../../design/modifiers.md) · [Reviewed known windows](../existence-atlas/modifier-proofs.csv) · [Joint profiling](2026-11-28-trunc-est3-math-report.md#joint-profiling-and-boundary-conventions)

## Row verdicts

| Family | Held | Estimated cut | Question | Verdict | Reason |
| --- | --- | --- | --- | --- | --- |
| ged | all free | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| ged | all free | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | all free | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | all free | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| ged | all free | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | all free | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | all free | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| ged | all free | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | all free | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | mean | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| ged | mean | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | mean | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| ged | mean | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | mean | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| ged | mean | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | mean+nu | lower | criterion | regraded | A strictly decreasing scale score matches the residual-power moment between its limits evaluated on the tight window. |
| ged | mean+nu | lower | failure | accepted | The scalar residual-power tilt has only projection divergence at its lower moment and the bounded-window uniform at or above its upper moment. |
| ged | mean+nu | lower | uniqueness | accepted | Strict positive variance identifies the scalar tilt and all estimated cuts are strict mass minimizers. |
| ged | mean+nu | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| ged | mean+nu | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean+nu | upper | criterion | regraded | A strictly decreasing scale score matches the residual-power moment between its limits evaluated on the tight window. |
| ged | mean+nu | upper | failure | accepted | The scalar residual-power tilt has only projection divergence at its lower moment and the bounded-window uniform at or above its upper moment. |
| ged | mean+nu | upper | uniqueness | accepted | Strict positive variance identifies the scalar tilt and all estimated cuts are strict mass minimizers. |
| ged | mean+nu | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| ged | mean+nu | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean+nu | both | criterion | regraded | A strictly decreasing scale score matches the residual-power moment between its limits evaluated on the tight window. |
| ged | mean+nu | both | failure | accepted | The scalar residual-power tilt has only projection divergence at its lower moment and the bounded-window uniform at or above its upper moment. |
| ged | mean+nu | both | uniqueness | accepted | Strict positive variance identifies the scalar tilt and all estimated cuts are strict mass minimizers. |
| ged | mean+nu | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| ged | mean+nu | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean+nu+sd | lower | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| ged | mean+nu+sd | lower | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| ged | mean+nu+sd | lower | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| ged | mean+nu+sd | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean+nu+sd | upper | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| ged | mean+nu+sd | upper | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| ged | mean+nu+sd | upper | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| ged | mean+nu+sd | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean+nu+sd | both | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| ged | mean+nu+sd | both | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| ged | mean+nu+sd | both | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| ged | mean+nu+sd | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean+sd | lower | criterion | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| ged | mean+sd | lower | failure | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| ged | mean+sd | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean+sd | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | mean+sd | upper | criterion | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| ged | mean+sd | upper | failure | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| ged | mean+sd | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean+sd | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | mean+sd | both | criterion | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| ged | mean+sd | both | failure | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| ged | mean+sd | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | mean+sd | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | nu | lower | criterion | regraded | The fixed GED power controls remote slopes and a free scale supplies the flat and sharp paths on the tight window. |
| ged | nu | lower | failure | regraded | The fixed GED power controls remote slopes and a free scale supplies the flat and sharp paths on the tight window. |
| ged | nu | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| ged | nu | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | nu | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | nu | upper | criterion | regraded | The fixed GED power controls remote slopes and a free scale supplies the flat and sharp paths on the tight window. |
| ged | nu | upper | failure | regraded | The fixed GED power controls remote slopes and a free scale supplies the flat and sharp paths on the tight window. |
| ged | nu | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| ged | nu | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | nu | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | nu | both | criterion | regraded | The fixed GED power controls remote slopes and a free scale supplies the flat and sharp paths on the tight window. |
| ged | nu | both | failure | regraded | The fixed GED power controls remote slopes and a free scale supplies the flat and sharp paths on the tight window. |
| ged | nu | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| ged | nu | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | nu | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | nu+sd | lower | criterion | regraded | Holding GED scale leaves a remote slope tending to zero below power one and to infinity above power one, with exactly linear exterior tails at one on the tight window. |
| ged | nu+sd | lower | failure | regraded | Holding GED scale leaves a remote slope tending to zero below power one and to infinity above power one, with exactly linear exterior tails at one on the tight window. |
| ged | nu+sd | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | nu+sd | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | nu+sd | upper | criterion | regraded | Holding GED scale leaves a remote slope tending to zero below power one and to infinity above power one, with exactly linear exterior tails at one on the tight window. |
| ged | nu+sd | upper | failure | regraded | Holding GED scale leaves a remote slope tending to zero below power one and to infinity above power one, with exactly linear exterior tails at one on the tight window. |
| ged | nu+sd | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | nu+sd | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| ged | nu+sd | both | criterion | regraded | Holding GED scale leaves a remote slope tending to zero below power one and to infinity above power one, with exactly linear exterior tails at one on the tight window. |
| ged | nu+sd | both | failure | regraded | Holding GED scale leaves a remote slope tending to zero below power one and to infinity above power one, with exactly linear exterior tails at one on the tight window. |
| ged | nu+sd | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| ged | nu+sd | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | all free | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | all free | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | all free | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | all free | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | all free | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | all free | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | all free | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| sged | all free | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | all free | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | mean | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | mean | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| sged | mean | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+nu | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | mean+nu | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+nu | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | mean+nu | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+nu | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| sged | mean+nu | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+nu+sd | lower | criterion | regraded | The fixed GED power and the shrinking-side scale determine the join value and the exterior decay after checking overlap including equality at either tight cut. |
| sged | mean+nu+sd | lower | failure | regraded | The fixed GED power and the shrinking-side scale determine the join value and the exterior decay after checking overlap including equality at either tight cut. |
| sged | mean+nu+sd | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu+sd | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+nu+sd | upper | criterion | regraded | The fixed GED power and the shrinking-side scale determine the join value and the exterior decay after checking overlap including equality at either tight cut. |
| sged | mean+nu+sd | upper | failure | regraded | The fixed GED power and the shrinking-side scale determine the join value and the exterior decay after checking overlap including equality at either tight cut. |
| sged | mean+nu+sd | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu+sd | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+nu+sd | both | criterion | regraded | The fixed GED power and the shrinking-side scale determine the join value and the exterior decay after checking overlap including equality at either tight cut. |
| sged | mean+nu+sd | both | failure | regraded | The fixed GED power and the shrinking-side scale determine the join value and the exterior decay after checking overlap including equality at either tight cut. |
| sged | mean+nu+sd | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu+sd | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+nu+sd+xi | lower | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| sged | mean+nu+sd+xi | lower | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| sged | mean+nu+sd+xi | lower | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| sged | mean+nu+sd+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu+sd+xi | upper | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| sged | mean+nu+sd+xi | upper | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| sged | mean+nu+sd+xi | upper | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| sged | mean+nu+sd+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu+sd+xi | both | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| sged | mean+nu+sd+xi | both | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| sged | mean+nu+sd+xi | both | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| sged | mean+nu+sd+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu+xi | lower | criterion | regraded | The split point tends to the held mean at small scale and the fixed positive skew multipliers flatten the kernel at broad scale on the tight window. |
| sged | mean+nu+xi | lower | failure | regraded | The split point tends to the held mean at small scale and the fixed positive skew multipliers flatten the kernel at broad scale on the tight window. |
| sged | mean+nu+xi | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | mean+nu+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+nu+xi | upper | criterion | regraded | The split point tends to the held mean at small scale and the fixed positive skew multipliers flatten the kernel at broad scale on the tight window. |
| sged | mean+nu+xi | upper | failure | regraded | The split point tends to the held mean at small scale and the fixed positive skew multipliers flatten the kernel at broad scale on the tight window. |
| sged | mean+nu+xi | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | mean+nu+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+nu+xi | both | criterion | regraded | The split point tends to the held mean at small scale and the fixed positive skew multipliers flatten the kernel at broad scale on the tight window. |
| sged | mean+nu+xi | both | failure | regraded | The split point tends to the held mean at small scale and the fixed positive skew multipliers flatten the kernel at broad scale on the tight window. |
| sged | mean+nu+xi | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| sged | mean+nu+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+nu+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+sd+xi | lower | criterion | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| sged | mean+sd+xi | lower | failure | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| sged | mean+sd+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+sd+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+sd+xi | upper | criterion | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| sged | mean+sd+xi | upper | failure | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| sged | mean+sd+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+sd+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+sd+xi | both | criterion | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| sged | mean+sd+xi | both | failure | regraded | The shape domain includes power one and the gamma expansion fixes the infinite-shape edge factor even at an observed tight cut. |
| sged | mean+sd+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+sd+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+xi | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | mean+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+xi | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | mean+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | mean+xi | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| sged | mean+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | mean+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | nu | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | nu | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | nu | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | nu | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | nu | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | nu | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | nu | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| sged | nu | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | nu | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | nu+sd+xi | lower | criterion | regraded | Holding scale and skew fixes both side scales and the held power determines the remote slopes on their respective sides on the tight window. |
| sged | nu+sd+xi | lower | failure | regraded | Holding scale and skew fixes both side scales and the held power determines the remote slopes on their respective sides on the tight window. |
| sged | nu+sd+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | nu+sd+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | nu+sd+xi | upper | criterion | regraded | Holding scale and skew fixes both side scales and the held power determines the remote slopes on their respective sides on the tight window. |
| sged | nu+sd+xi | upper | failure | regraded | Holding scale and skew fixes both side scales and the held power determines the remote slopes on their respective sides on the tight window. |
| sged | nu+sd+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | nu+sd+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | nu+sd+xi | both | criterion | regraded | Holding scale and skew fixes both side scales and the held power determines the remote slopes on their respective sides on the tight window. |
| sged | nu+sd+xi | both | failure | regraded | Holding scale and skew fixes both side scales and the held power determines the remote slopes on their respective sides on the tight window. |
| sged | nu+sd+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | nu+sd+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | nu+xi | lower | criterion | regraded | The fixed shape and skew give two positive stretched-exponential tail constants and the free reported mean can place the split point anywhere on the tight window. |
| sged | nu+xi | lower | failure | regraded | The fixed shape and skew give two positive stretched-exponential tail constants and the free reported mean can place the split point anywhere on the tight window. |
| sged | nu+xi | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | nu+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | nu+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | nu+xi | upper | criterion | regraded | The fixed shape and skew give two positive stretched-exponential tail constants and the free reported mean can place the split point anywhere on the tight window. |
| sged | nu+xi | upper | failure | regraded | The fixed shape and skew give two positive stretched-exponential tail constants and the free reported mean can place the split point anywhere on the tight window. |
| sged | nu+xi | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | nu+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | nu+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | nu+xi | both | criterion | regraded | The fixed shape and skew give two positive stretched-exponential tail constants and the free reported mean can place the split point anywhere on the tight window. |
| sged | nu+xi | both | failure | regraded | The fixed shape and skew give two positive stretched-exponential tail constants and the free reported mean can place the split point anywhere on the tight window. |
| sged | nu+xi | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| sged | nu+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | nu+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | xi | lower | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | xi | upper | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform, with the fixed cut at the opposite observed extreme. |
| sged | xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sged | xi | both | case:endpoint-pair | accepted | The held power is at least one or the free-power domain imposes that bound, and the finite kernels satisfy the strict endpoint chord comparison with an accessible broad-scale uniform. |
| sged | xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sged | xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | all free | lower | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| sn | all free | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | all free | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | all free | upper | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| sn | all free | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | all free | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | all free | both | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform. |
| sn | all free | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | all free | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | alpha | lower | criterion | regraded | The Gaussian and inverse-Mills tail slopes are quadratic-tail slopes on both sides and location and scale are free on the tight window. |
| sn | alpha | lower | failure | regraded | The Gaussian and inverse-Mills tail slopes are quadratic-tail slopes on both sides and location and scale are free on the tight window. |
| sn | alpha | lower | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| sn | alpha | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | alpha | upper | criterion | regraded | The Gaussian and inverse-Mills tail slopes are quadratic-tail slopes on both sides and location and scale are free on the tight window. |
| sn | alpha | upper | failure | regraded | The Gaussian and inverse-Mills tail slopes are quadratic-tail slopes on both sides and location and scale are free on the tight window. |
| sn | alpha | upper | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| sn | alpha | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | alpha | both | criterion | regraded | The Gaussian and inverse-Mills tail slopes are quadratic-tail slopes on both sides and location and scale are free on the tight window. |
| sn | alpha | both | failure | regraded | The Gaussian and inverse-Mills tail slopes are quadratic-tail slopes on both sides and location and scale are free on the tight window. |
| sn | alpha | both | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform. |
| sn | alpha | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | alpha+omega | lower | criterion | regraded | Fixed scale and skew make both limiting remote slope magnitudes infinite on the tight window. |
| sn | alpha+omega | lower | failure | regraded | Fixed scale and skew make both limiting remote slope magnitudes infinite on the tight window. |
| sn | alpha+omega | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha+omega | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | alpha+omega | upper | criterion | regraded | Fixed scale and skew make both limiting remote slope magnitudes infinite on the tight window. |
| sn | alpha+omega | upper | failure | regraded | Fixed scale and skew make both limiting remote slope magnitudes infinite on the tight window. |
| sn | alpha+omega | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha+omega | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | alpha+omega | both | criterion | regraded | Fixed scale and skew make both limiting remote slope magnitudes infinite on the tight window. |
| sn | alpha+omega | both | failure | regraded | Fixed scale and skew make both limiting remote slope magnitudes infinite on the tight window. |
| sn | alpha+omega | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha+omega | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | alpha+omega+xi | lower | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| sn | alpha+omega+xi | lower | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| sn | alpha+omega+xi | lower | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| sn | alpha+omega+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha+omega+xi | upper | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| sn | alpha+omega+xi | upper | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| sn | alpha+omega+xi | upper | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| sn | alpha+omega+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha+omega+xi | both | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| sn | alpha+omega+xi | both | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| sn | alpha+omega+xi | both | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| sn | alpha+omega+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha+xi | lower | criterion | regraded | The positive Gaussian carrier and bounded standardized mode put the sharp limit at the projection of the held location on the tight window. |
| sn | alpha+xi | lower | failure | regraded | The positive Gaussian carrier and bounded standardized mode put the sharp limit at the projection of the held location on the tight window. |
| sn | alpha+xi | lower | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| sn | alpha+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | alpha+xi | upper | criterion | regraded | The positive Gaussian carrier and bounded standardized mode put the sharp limit at the projection of the held location on the tight window. |
| sn | alpha+xi | upper | failure | regraded | The positive Gaussian carrier and bounded standardized mode put the sharp limit at the projection of the held location on the tight window. |
| sn | alpha+xi | upper | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| sn | alpha+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | alpha+xi | both | criterion | regraded | The positive Gaussian carrier and bounded standardized mode put the sharp limit at the projection of the held location on the tight window. |
| sn | alpha+xi | both | failure | regraded | The positive Gaussian carrier and bounded standardized mode put the sharp limit at the projection of the held location on the tight window. |
| sn | alpha+xi | both | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform. |
| sn | alpha+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | alpha+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | omega+xi | lower | criterion | regraded | Fixed location and scale leave only the two skew infinities and a Gaussian CDF boundary layer at a join cut after checking overlap including equality at either tight cut. |
| sn | omega+xi | lower | failure | regraded | Fixed location and scale leave only the two skew infinities and a Gaussian CDF boundary layer at a join cut after checking overlap including equality at either tight cut. |
| sn | omega+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | omega+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | omega+xi | upper | criterion | regraded | Fixed location and scale leave only the two skew infinities and a Gaussian CDF boundary layer at a join cut after checking overlap including equality at either tight cut. |
| sn | omega+xi | upper | failure | regraded | Fixed location and scale leave only the two skew infinities and a Gaussian CDF boundary layer at a join cut after checking overlap including equality at either tight cut. |
| sn | omega+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | omega+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | omega+xi | both | criterion | regraded | Fixed location and scale leave only the two skew infinities and a Gaussian CDF boundary layer at a join cut after checking overlap including equality at either tight cut. |
| sn | omega+xi | both | failure | regraded | Fixed location and scale leave only the two skew infinities and a Gaussian CDF boundary layer at a join cut after checking overlap including equality at either tight cut. |
| sn | omega+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | omega+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | xi | lower | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| sn | xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | xi | upper | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| sn | xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sn | xi | both | case:endpoint-pair | accepted | The log-concave normal times Gaussian CDF is strictly nonconstant and broad free scale gives the bounded-window uniform. |
| sn | xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sn | xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | all free | lower | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| snorm | all free | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | all free | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | all free | upper | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| snorm | all free | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | all free | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | all free | both | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform. |
| snorm | all free | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | all free | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | mean | lower | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| snorm | mean | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | mean | upper | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| snorm | mean | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | mean | both | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform. |
| snorm | mean | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | mean+sd | lower | criterion | regraded | The quadratic kernel gives a moving join and a Gaussian shrinking-side cut limit after checking overlap including equality at either tight cut. |
| snorm | mean+sd | lower | failure | regraded | The quadratic kernel gives a moving join and a Gaussian shrinking-side cut limit after checking overlap including equality at either tight cut. |
| snorm | mean+sd | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean+sd | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | mean+sd | upper | criterion | regraded | The quadratic kernel gives a moving join and a Gaussian shrinking-side cut limit after checking overlap including equality at either tight cut. |
| snorm | mean+sd | upper | failure | regraded | The quadratic kernel gives a moving join and a Gaussian shrinking-side cut limit after checking overlap including equality at either tight cut. |
| snorm | mean+sd | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean+sd | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | mean+sd | both | criterion | regraded | The quadratic kernel gives a moving join and a Gaussian shrinking-side cut limit after checking overlap including equality at either tight cut. |
| snorm | mean+sd | both | failure | regraded | The quadratic kernel gives a moving join and a Gaussian shrinking-side cut limit after checking overlap including equality at either tight cut. |
| snorm | mean+sd | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean+sd | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | mean+sd+xi | lower | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| snorm | mean+sd+xi | lower | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| snorm | mean+sd+xi | lower | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| snorm | mean+sd+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean+sd+xi | upper | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| snorm | mean+sd+xi | upper | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| snorm | mean+sd+xi | upper | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| snorm | mean+sd+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean+sd+xi | both | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| snorm | mean+sd+xi | both | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| snorm | mean+sd+xi | both | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| snorm | mean+sd+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean+xi | lower | criterion | regraded | Fixed skew gives quadratic potentials on the two sides and the split point converges to the held mean at small scale on the tight window. |
| snorm | mean+xi | lower | failure | regraded | Fixed skew gives quadratic potentials on the two sides and the split point converges to the held mean at small scale on the tight window. |
| snorm | mean+xi | lower | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| snorm | mean+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | mean+xi | upper | criterion | regraded | Fixed skew gives quadratic potentials on the two sides and the split point converges to the held mean at small scale on the tight window. |
| snorm | mean+xi | upper | failure | regraded | Fixed skew gives quadratic potentials on the two sides and the split point converges to the held mean at small scale on the tight window. |
| snorm | mean+xi | upper | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| snorm | mean+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | mean+xi | both | criterion | regraded | Fixed skew gives quadratic potentials on the two sides and the split point converges to the held mean at small scale on the tight window. |
| snorm | mean+xi | both | failure | regraded | Fixed skew gives quadratic potentials on the two sides and the split point converges to the held mean at small scale on the tight window. |
| snorm | mean+xi | both | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform. |
| snorm | mean+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | mean+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | sd+xi | lower | criterion | regraded | The two fixed Gaussian scales give infinite remote slope and rule out broad-scale and finite-slope limits on the tight window. |
| snorm | sd+xi | lower | failure | regraded | The two fixed Gaussian scales give infinite remote slope and rule out broad-scale and finite-slope limits on the tight window. |
| snorm | sd+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | sd+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | sd+xi | upper | criterion | regraded | The two fixed Gaussian scales give infinite remote slope and rule out broad-scale and finite-slope limits on the tight window. |
| snorm | sd+xi | upper | failure | regraded | The two fixed Gaussian scales give infinite remote slope and rule out broad-scale and finite-slope limits on the tight window. |
| snorm | sd+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | sd+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | sd+xi | both | criterion | regraded | The two fixed Gaussian scales give infinite remote slope and rule out broad-scale and finite-slope limits on the tight window. |
| snorm | sd+xi | both | failure | regraded | The two fixed Gaussian scales give infinite remote slope and rule out broad-scale and finite-slope limits on the tight window. |
| snorm | sd+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | sd+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | xi | lower | criterion | regraded | Both Gaussian side multipliers stay fixed and positive while the split point and scale can move freely on the tight window. |
| snorm | xi | lower | failure | regraded | Both Gaussian side multipliers stay fixed and positive while the split point and scale can move freely on the tight window. |
| snorm | xi | lower | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| snorm | xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | xi | upper | criterion | regraded | Both Gaussian side multipliers stay fixed and positive while the split point and scale can move freely on the tight window. |
| snorm | xi | upper | failure | regraded | Both Gaussian side multipliers stay fixed and positive while the split point and scale can move freely on the tight window. |
| snorm | xi | upper | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform, with the fixed cut at the opposite observed extreme. |
| snorm | xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| snorm | xi | both | criterion | regraded | Both Gaussian side multipliers stay fixed and positive while the split point and scale can move freely on the tight window. |
| snorm | xi | both | failure | regraded | Both Gaussian side multipliers stay fixed and positive while the split point and scale can move freely on the tight window. |
| snorm | xi | both | case:endpoint-pair | accepted | The two positive quadratic side potentials are log-concave and broad free scale at fixed skew gives the bounded-window uniform. |
| snorm | xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| snorm | xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | all free | lower | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | all free | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | all free | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | all free | upper | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | all free | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | all free | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | all free | both | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | all free | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | all free | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean | lower | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean | upper | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean | both | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+nu+sd | lower | criterion | regraded | The fixed Student tail controls the strictly exterior side and the shrinking-side join requires the one-sided rescaling after checking overlap including equality at either tight cut. |
| sstd | mean+nu+sd | lower | failure | regraded | The fixed Student tail controls the strictly exterior side and the shrinking-side join requires the one-sided rescaling after checking overlap including equality at either tight cut. |
| sstd | mean+nu+sd | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+nu+sd | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+nu+sd | upper | criterion | regraded | The fixed Student tail controls the strictly exterior side and the shrinking-side join requires the one-sided rescaling after checking overlap including equality at either tight cut. |
| sstd | mean+nu+sd | upper | failure | regraded | The fixed Student tail controls the strictly exterior side and the shrinking-side join requires the one-sided rescaling after checking overlap including equality at either tight cut. |
| sstd | mean+nu+sd | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+nu+sd | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+nu+sd | both | criterion | regraded | The fixed Student tail controls the strictly exterior side and the shrinking-side join requires the one-sided rescaling after checking overlap including equality at either tight cut. |
| sstd | mean+nu+sd | both | failure | regraded | The fixed Student tail controls the strictly exterior side and the shrinking-side join requires the one-sided rescaling after checking overlap including equality at either tight cut. |
| sstd | mean+nu+sd | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+nu+sd | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+nu+sd+xi | lower | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| sstd | mean+nu+sd+xi | lower | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| sstd | mean+nu+sd+xi | lower | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| sstd | mean+nu+sd+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+nu+sd+xi | upper | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| sstd | mean+nu+sd+xi | upper | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| sstd | mean+nu+sd+xi | upper | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| sstd | mean+nu+sd+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+nu+sd+xi | both | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| sstd | mean+nu+sd+xi | both | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| sstd | mean+nu+sd+xi | both | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| sstd | mean+nu+sd+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+nu+xi | lower | criterion | regraded | The two-piece t tails have common exponent nu+1 and holding the mean fixes the scaled center offset on the tight window. |
| sstd | mean+nu+xi | lower | failure | regraded | The two-piece t tails have common exponent nu+1 and holding the mean fixes the scaled center offset on the tight window. |
| sstd | mean+nu+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+nu+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+nu+xi | upper | criterion | regraded | The two-piece t tails have common exponent nu+1 and holding the mean fixes the scaled center offset on the tight window. |
| sstd | mean+nu+xi | upper | failure | regraded | The two-piece t tails have common exponent nu+1 and holding the mean fixes the scaled center offset on the tight window. |
| sstd | mean+nu+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+nu+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+nu+xi | both | criterion | regraded | The two-piece t tails have common exponent nu+1 and holding the mean fixes the scaled center offset on the tight window. |
| sstd | mean+nu+xi | both | failure | regraded | The two-piece t tails have common exponent nu+1 and holding the mean fixes the scaled center offset on the tight window. |
| sstd | mean+nu+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+nu+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+sd | lower | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean+sd | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+sd | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+sd | upper | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean+sd | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+sd | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+sd | both | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean+sd | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+sd | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+sd+xi | lower | criterion | regraded | The two scalar df ends exhaust the closure and the fixed scaled shift gives the tie product at a retained mean, including when it equals a cut. |
| sstd | mean+sd+xi | lower | failure | regraded | The two scalar df ends exhaust the closure and the fixed scaled shift gives the tie product at a retained mean, including when it equals a cut. |
| sstd | mean+sd+xi | lower | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean+sd+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+sd+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+sd+xi | upper | criterion | regraded | The two scalar df ends exhaust the closure and the fixed scaled shift gives the tie product at a retained mean, including when it equals a cut. |
| sstd | mean+sd+xi | upper | failure | regraded | The two scalar df ends exhaust the closure and the fixed scaled shift gives the tie product at a retained mean, including when it equals a cut. |
| sstd | mean+sd+xi | upper | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean+sd+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+sd+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+sd+xi | both | criterion | regraded | The two scalar df ends exhaust the closure and the fixed scaled shift gives the tie product at a retained mean, including when it equals a cut. |
| sstd | mean+sd+xi | both | failure | regraded | The two scalar df ends exhaust the closure and the fixed scaled shift gives the tie product at a retained mean, including when it equals a cut. |
| sstd | mean+sd+xi | both | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean+sd+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+sd+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+xi | lower | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+xi | upper | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | mean+xi | both | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | mean+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | mean+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | nu+xi | lower | criterion | regraded | At fixed df and skew the two-piece t has positive tail constants and arbitrary physical center on the tight window. |
| sstd | nu+xi | lower | failure | regraded | At fixed df and skew the two-piece t has positive tail constants and arbitrary physical center on the tight window. |
| sstd | nu+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | nu+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | nu+xi | upper | criterion | regraded | At fixed df and skew the two-piece t has positive tail constants and arbitrary physical center on the tight window. |
| sstd | nu+xi | upper | failure | regraded | At fixed df and skew the two-piece t has positive tail constants and arbitrary physical center on the tight window. |
| sstd | nu+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | nu+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | nu+xi | both | criterion | regraded | At fixed df and skew the two-piece t has positive tail constants and arbitrary physical center on the tight window. |
| sstd | nu+xi | both | failure | regraded | At fixed df and skew the two-piece t has positive tail constants and arbitrary physical center on the tight window. |
| sstd | nu+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | nu+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | sd | lower | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | sd | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | sd | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | sd | upper | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | sd | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | sd | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | sd | both | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | sd | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | sd | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | sd+xi | lower | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | sd+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | sd+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | sd+xi | upper | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | sd+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | sd+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | sd+xi | both | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | sd+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | sd+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | xi | lower | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | xi | upper | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| sstd | xi | both | case:divergent-tie | accepted | At fixed allowed scale and skew the df-down-to-two path has a finite scaled mean offset and a density-product exponent of 3(n−j)−n, which is negative here. |
| sstd | xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| sstd | xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| st | alpha+nu | lower | criterion | regraded | The skewing t CDF tends to strictly positive constants on both tails at fixed alpha and df on the tight window. |
| st | alpha+nu | lower | failure | regraded | The skewing t CDF tends to strictly positive constants on both tails at fixed alpha and df on the tight window. |
| st | alpha+nu | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| st | alpha+nu | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| st | alpha+nu | upper | criterion | regraded | The skewing t CDF tends to strictly positive constants on both tails at fixed alpha and df on the tight window. |
| st | alpha+nu | upper | failure | regraded | The skewing t CDF tends to strictly positive constants on both tails at fixed alpha and df on the tight window. |
| st | alpha+nu | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| st | alpha+nu | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| st | alpha+nu | both | criterion | regraded | The skewing t CDF tends to strictly positive constants on both tails at fixed alpha and df on the tight window. |
| st | alpha+nu | both | failure | regraded | The skewing t CDF tends to strictly positive constants on both tails at fixed alpha and df on the tight window. |
| st | alpha+nu | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| st | alpha+nu | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| st | alpha+nu+xi | lower | criterion | regraded | The skewing t CDF preserves both power exponents and the held location forces the centered collapse direction on the tight window. |
| st | alpha+nu+xi | lower | failure | regraded | The skewing t CDF preserves both power exponents and the held location forces the centered collapse direction on the tight window. |
| st | alpha+nu+xi | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| st | alpha+nu+xi | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| st | alpha+nu+xi | upper | criterion | regraded | The skewing t CDF preserves both power exponents and the held location forces the centered collapse direction on the tight window. |
| st | alpha+nu+xi | upper | failure | regraded | The skewing t CDF preserves both power exponents and the held location forces the centered collapse direction on the tight window. |
| st | alpha+nu+xi | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| st | alpha+nu+xi | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| st | alpha+nu+xi | both | criterion | regraded | The skewing t CDF preserves both power exponents and the held location forces the centered collapse direction on the tight window. |
| st | alpha+nu+xi | both | failure | regraded | The skewing t CDF preserves both power exponents and the held location forces the centered collapse direction on the tight window. |
| st | alpha+nu+xi | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| st | alpha+nu+xi | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | all free | lower | criterion | regraded | Free center restricts df to at least one and free scale permits critical collapse at df one or divergence for strict majority ties. |
| lst | all free | lower | failure | regraded | Free center restricts df to at least one and free scale permits critical collapse at df one or divergence for strict majority ties. |
| lst | all free | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | all free | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | all free | upper | criterion | regraded | Free center restricts df to at least one and free scale permits critical collapse at df one or divergence for strict majority ties. |
| lst | all free | upper | failure | regraded | Free center restricts df to at least one and free scale permits critical collapse at df one or divergence for strict majority ties. |
| lst | all free | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | all free | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | all free | both | criterion | regraded | Free center restricts df to at least one and free scale permits critical collapse at df one or divergence for strict majority ties. |
| lst | all free | both | failure | regraded | Free center restricts df to at least one and free scale permits critical collapse at df one or divergence for strict majority ties. |
| lst | all free | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | all free | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | df | lower | criterion | regraded | Fixed positive df gives integrable power tails and both center and scale may approach a sample cut jointly on the tight window. |
| lst | df | lower | failure | regraded | Fixed positive df gives integrable power tails and both center and scale may approach a sample cut jointly on the tight window. |
| lst | df | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | df | upper | criterion | regraded | Fixed positive df gives integrable power tails and both center and scale may approach a sample cut jointly on the tight window. |
| lst | df | upper | failure | regraded | Fixed positive df gives integrable power tails and both center and scale may approach a sample cut jointly on the tight window. |
| lst | df | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | df | both | criterion | regraded | Fixed positive df gives integrable power tails and both center and scale may approach a sample cut jointly on the tight window. |
| lst | df | both | failure | regraded | Fixed positive df gives integrable power tails and both center and scale may approach a sample cut jointly on the tight window. |
| lst | df | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | df+mu | lower | criterion | regraded | Fixed df gives integrable power tails and the held center allows only centered collapse at the center on the tight window. |
| lst | df+mu | lower | failure | regraded | Fixed df gives integrable power tails and the held center allows only centered collapse at the center on the tight window. |
| lst | df+mu | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df+mu | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | df+mu | upper | criterion | regraded | Fixed df gives integrable power tails and the held center allows only centered collapse at the center on the tight window. |
| lst | df+mu | upper | failure | regraded | Fixed df gives integrable power tails and the held center allows only centered collapse at the center on the tight window. |
| lst | df+mu | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df+mu | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | df+mu | both | criterion | regraded | Fixed df gives integrable power tails and the held center allows only centered collapse at the center on the tight window. |
| lst | df+mu | both | failure | regraded | Fixed df gives integrable power tails and the held center allows only centered collapse at the center on the tight window. |
| lst | df+mu | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df+mu | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | df+mu+sigma | lower | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| lst | df+mu+sigma | lower | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| lst | df+mu+sigma | lower | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| lst | df+mu+sigma | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df+mu+sigma | upper | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| lst | df+mu+sigma | upper | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| lst | df+mu+sigma | upper | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| lst | df+mu+sigma | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df+mu+sigma | both | criterion | regraded | The held parent is strictly positive on the whole line and its unique best conditional mass is positive exactly at positive tight width. |
| lst | df+mu+sigma | both | failure | accepted | On positive tight width no parent coefficient can escape and any loosening of a cut only increases the normalizer. |
| lst | df+mu+sigma | both | uniqueness | accepted | Only the cuts can vary and every strict enlargement has positive mass. |
| lst | df+mu+sigma | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df+sigma | lower | criterion | regraded | Fixed positive scale and df leave only remote centers, so compact ratios tend to one and Fatou rules out a positive ray limit on the tight window. |
| lst | df+sigma | lower | failure | regraded | Fixed positive scale and df leave only remote centers, so compact ratios tend to one and Fatou rules out a positive ray limit on the tight window. |
| lst | df+sigma | lower | case:nonunique | regraded | Exact rational bounds beat the centered fit and uniform boundary, then continuity and reflection force two finite global maxima. |
| lst | df+sigma | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df+sigma | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | df+sigma | upper | criterion | regraded | Fixed positive scale and df leave only remote centers, so compact ratios tend to one and Fatou rules out a positive ray limit on the tight window. |
| lst | df+sigma | upper | failure | regraded | Fixed positive scale and df leave only remote centers, so compact ratios tend to one and Fatou rules out a positive ray limit on the tight window. |
| lst | df+sigma | upper | case:nonunique | regraded | Exact rational bounds beat the centered fit and uniform boundary, then continuity and reflection force two finite global maxima. |
| lst | df+sigma | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df+sigma | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | df+sigma | both | criterion | regraded | Fixed positive scale and df leave only remote centers, so compact ratios tend to one and Fatou rules out a positive ray limit on the tight window. |
| lst | df+sigma | both | failure | regraded | Fixed positive scale and df leave only remote centers, so compact ratios tend to one and Fatou rules out a positive ray limit on the tight window. |
| lst | df+sigma | both | case:nonunique | regraded | Exact rational bounds beat the centered fit and uniform boundary, then continuity and reflection force two finite global maxima. |
| lst | df+sigma | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | df+sigma | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | mu | lower | criterion | regraded | The held center admits df below one, so fixed-small-df ties and sharp large-df exterior cut constants must both be screened before comparing finite boundary heights. |
| lst | mu | lower | failure | accepted | The fixed-small-df tie witness survives tightening and a tight ray still allows the reviewed non-tight-in-law small-df path. |
| lst | mu | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | mu | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | mu | upper | criterion | regraded | The held center admits df below one, so fixed-small-df ties and sharp large-df exterior cut constants must both be screened before comparing finite boundary heights. |
| lst | mu | upper | failure | accepted | The fixed-small-df tie witness survives tightening and a tight ray still allows the reviewed non-tight-in-law small-df path. |
| lst | mu | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | mu | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | mu | both | criterion | regraded | The held center admits df below one, so fixed-small-df ties and sharp large-df exterior cut constants must both be screened before comparing finite boundary heights. |
| lst | mu | both | failure | accepted | The fixed-small-df tie witness survives tightening and a tight ray still allows the reviewed non-tight-in-law small-df path. |
| lst | mu | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | mu | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | sigma | lower | criterion | regraded | Free center restricts df to at least one and held positive scale rules out all finite-df scale-collapse and exterior-power paths. |
| lst | sigma | lower | failure | regraded | Free center restricts df to at least one and held positive scale rules out all finite-df scale-collapse and exterior-power paths. |
| lst | sigma | lower | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | sigma | lower | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | sigma | upper | criterion | regraded | Free center restricts df to at least one and held positive scale rules out all finite-df scale-collapse and exterior-power paths. |
| lst | sigma | upper | failure | regraded | Free center restricts df to at least one and held positive scale rules out all finite-df scale-collapse and exterior-power paths. |
| lst | sigma | upper | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | sigma | upper | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |
| lst | sigma | both | criterion | regraded | Free center restricts df to at least one and held positive scale rules out all finite-df scale-collapse and exterior-power paths. |
| lst | sigma | both | failure | regraded | Free center restricts df to at least one and held positive scale rules out all finite-df scale-collapse and exterior-power paths. |
| lst | sigma | both | case:collapsed-window | accepted | Positive continuous density on the whole line gives inward mass proportional to the shrinking width and an unbounded likelihood at a coincident cut. |
| lst | sigma | both | case:bound-set | accepted | Every strict extension past an estimated sample extreme adds positive mass, so an attained bound is unique. |

## Proof errors and scope corrections

### Holds change the boundary list

The proposed fixed-power failure text was shared with half-t and mentioned shrinking support. All reviewed Student kernels have support on the whole real line. For LST with df and scale held, the only escaping coefficient is the center. For example, with df 3, scale 1 and sample `(0,1)` on the tight window `[0,1]`, the kernel ratio to its value at zero tends uniformly to one as the center tends to either infinity. The same compact-ratio argument holds for every finite positive scale and df. Thus the bounded uniform is the entire positive excluded comparison on an interval. On a ray Fatou gives zero sample likelihood, so the positive finite objective attains. With scale free, shrinking toward a retained point with (j) ties gives exponent ((
u+1)(n-j)-n), with an inward half-line at a tight cut. Holding the center fixes the feasible direction. For SSTD with held mean this direction includes the mean-to-center offset, and for ST the two tail constants are positive because the skewing Student CDF is evaluated at finite arguments. The equality exponent uses the critical direction product, not a density assigned to an atom. [Fixed power argument](2026-11-27-trunc-skew-math-report.md#fixed-power-shape)

The fixed-power stretched-exponential transfer also required separating the holds. With scale held and power one, the exterior location kernel is exactly exponential on the retained side, so the remote limit of that sign and slope is already represented by a finite center at the cut. With a held GED or SGED power below one, fixed-scale remote slopes tend to zero, and above one they diverge. For example, a two-piece normal with held scale and skew has both quadratic tails, so there is no finite nonzero remote slope and no broad uniform limit. At held mean and fixed skew the split point moves with scale but tends to the mean as scale shrinks. Broad scale gives the uniform on bounded windows, and the sharp limit diverges only for a constant at the projected mean. The SN proof uses the Gaussian and inverse-Mills slopes instead of the split-kernel slopes. In particular, with both location and scale free and alpha held, the bounded uniform belongs in its failure comparison. [Fixed stretched-exponential argument](2026-11-27-trunc-skew-math-report.md#fixed-stretched-exponential-shape) · [Fixed-alpha SN argument](2026-11-27-trunc-skew-math-report.md#azzalini-skew-normal)

### Scalar edges and joins

SGED and snorm with only skew free have stretched-exponential decay on the disappearing side, so their proposed exterior-power alternative cannot occur. At the limiting join the scaled displacement tends to (H=m_1/[2(1-m_1^2)]>0) for skew tending to infinity, with reflection at zero. If the surviving side has positive overlap, an observation at the limiting join uses the numerator factor (g(-H)). If the whole window is on the disappearing side, equality between the tight cut and the join still gives a cut atom through the one-sided rescaling. SSTD instead gives an integrable exterior power at positive distance or the critical tie exponent at the contact cut. For SN with only alpha free, holding the location keeps the join fixed and its factor is (1/2) when the surviving side has positive width. [Two-piece skew-only limits](2026-11-27-trunc-skew-math-report.md#skew-only-two-piece-envelopes) · [SN scalar limits](2026-11-27-trunc-skew-math-report.md#azzalini-skew-normal)

In GED and SGED with only shape free, shape one is included. The infinite-shape limit requires the pointwise factor (exp[-exp(-gamma_E)]) at either limiting parent endpoint, including when a tightened cut retains an observation there. When the limiting support has zero overlap, the positive-width finite-parent windows still have positive mass, and the normalized kernel concentrates at the near cut. For SSTD with only df free, df two is excluded and the small-df model is an exterior inverse cube or a retained-center collapse. The scaled offset is finite since (m_1/sqrt{
u-2}	o1), so the threshold and critical contact product apply at a tight cut as well as at an interior center. The infinite-df limit is retained snorm. [Scalar shape proofs](2026-11-27-trunc-skew-math-report.md#shape-only-ged-and-standardized-student)

### LST free df and equality

With free center, df cannot tend to zero. When scale is free, strict majority ties diverge at df one, and equality at a half-sample tie has the critical angle comparison. If df drifts to one while the effective scale shrinks, its coefficient-dependent extra power of the shrinking radius is at most one, and setting df exactly to one realizes the upper envelope. At held scale and free center, a remote steep slope can still diverge for a constant on a finite cut, even though fixed-df collapse is excluded. With mean held and scale free, the criterion also needs the exterior-center cut constant branch. For example, the sample `(0)` on tight `[0,1]` with held center (-1) has no centered tie. Taking physical scale (v^{-1/2}) as df (v	oinfty) gives kernel ((1+(x+1)^2)^{-(v+1)/2}). Its inward log slope at zero is (-(v+1)/2), and on ([0,1]) the magnitude is at least (2(v+1)/5). Thus the relative integral is at most (5/[2(v+1)]), which proves divergence. The held-center failure grade remains one-sided because the known-window small-df, shrinking-scale path on tight ([0,infty)) for sample ((0,1)) can diverge with mass escaping. [Free-df exhaustion](2026-11-27-trunc-skew-math-report.md#free-degrees-of-freedom-in-lst-and-ht) · [Small-df correction](2026-11-27-mod-rev7-report.md#student-small-df)

For every retained comparison criterion with finite excluded supremum (B), a finite feasible point at (B) either wins or is beaten by a point above (B). The latter forces a finite maximizing subsequence by exhaustion of the boundary. This resolves the equality branch without assuming that stationary points are finite in number or that the best face is attained. [Likelihood comparison](2026-11-27-trunc-ls-math-report.md#likelihood-and-comparison)

### Exact reflected maxima

For LST with df 3 and scale (1/sqrt3), the trial center 4 beats the center 0 and remote uniform on the exact tight-window sample in the CSV. The normalizer at 0 exceeds (pi/4+1/2>1) and at 4 is less than (pi/2<2). The inner-pair likelihood at 4 is at least (1/(4cdot65^2)), and the outer pair at least (1/(4cdot10817^4)). The script checks the resulting rational lower bounds after raising the inner-pair ratios to the 50th power. There is a finite nonzero global maximizer by continuity and the strict gap over uniform. Reflection gives a second global maximizer, and tightening uniquely fixes the cuts. For each single-cut design the fixed cut must be the opposite end of the sample. [Exact proof](2026-11-28-trunc-est3-math-report.md#exact-reflected-maxima-for-lst-and-slash) · [Replay script](2026-11-28-trunc-est3-math-data/check.py)

## Rows not joined

None. All 621 proposed keys have a parent regime, are eligible for the modifier, and are represented in the join input. The all-held rows concern likelihood given a sample and held parent coefficients. The modifier design's separate frozen-model interface without fitted data requires known bounds.

## Open by family

Each listed mask applies to all three estimated-bound designs. The table concerns the reviewed masks. The named cases and exact cut formulas remain available within their hypotheses.

| Family | General attainment or failure still open | Exact parent maximizing set still open |
| --- | --- | --- |
| ged | All free and mean held need joint large-shape comparisons at moving uniform edges. | All reviewed masks except mean+nu and mean+nu+sd. |
| sged | All free, mean, mean+nu, mean+xi, nu, xi need joint compression or moving-uniform control with the stated holds. | All reviewed masks except mean+nu+sd+xi. |
| sn | All free and xi held need coupled skew and scale limits, including wrong-side conditioning and, with free location, moving join contacts. | All reviewed masks except alpha+omega+xi. |
| snorm | All free and mean held need coupled skew and scale limits at the moving split point. | All reviewed masks except mean+sd+xi. |
| sstd | All free, mean, mean+sd, mean+xi, sd, sd+xi, xi need joint small-df and skew or scale comparisons, including critical tie products beyond the fixed-skew path. | All reviewed masks except mean+nu+sd+xi. |
| st | The two reviewed masks have exhaustive comparisons. | Both reviewed masks need finite-root and global tie classification. |
| lst | The seven reviewed masks have attainment comparisons. At held mu with free df and scale the divergent-ray failure classification still needs all possible joint limits. | All reviewed masks except df+mu+sigma. The df+sigma counterexample establishes multiplicity for its exact sample. |

## Checks

The workstation ran the source replay through one CPU slot on a scratch copy. All 64 checks passed. The four LST inequalities are rational, and the other numerical experiments were unnecessary for the proof grades in this review. No build applied to this review.

The candidate validator and the atlas's `--check-modifier-proofs` command both passed on the workstation. The Markdown link checker exited 1 for the pre-existing `P+Q` false positive in the beta-prime report at line 461 and reported 47 inherited destinations. It reported no issue in either review artifact.
