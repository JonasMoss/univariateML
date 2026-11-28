# Independent review of estimated truncation bounds

TASK-476 reviewed 604 TASK-475 proposals for Feller–Pareto, generalized beta, GEV and discretized gamma. The [join file](2026-11-28-mod-rev13a-data/reviewed.csv) contains 604 rows, including 486 accepted as submitted and 118 regraded through a claim correction or clarification. There were 0 rejected rows. The [source report](2026-11-28-trunc-est4-math-report.md) and the [known-window proof inventory](../existence-atlas/modifier-proofs.csv) are unchanged.

**The Feller–Pareto transfer omits an eligible excluded height.** Even with both beta shapes held, scale can escape at finite power and yield a nonzero log-excess tilt. For the exact sample `(1,1,2)` this omitted face wins. The corrected criterion uses all integrable signed tilts. The endpoint corrections below also make the distinction between closed nominal cuts, open parent support at zero, and empty retained events explicit.

| Family | Accepted | Regraded | Rejected | Join rows |
| --- | ---: | ---: | ---: | ---: |
| fpareto | 300 | 10 | 0 | 310 |
| genbeta | 126 | 81 | 0 | 207 |
| gev | 27 | 9 | 0 | 36 |
| dgamma | 33 | 18 | 0 | 51 |
| **Total** | **486** | **118** | **0** | **604** |

## Row decisions

`m` and `M` are the realized sample extremes and `A` and `B` are fixed cuts. A bound-cardinality case describes the full cut set for each winning parent vector. The main uniqueness rows need the parent winning set as well.

| Family | Held | Estimated cuts | Question | Verdict | Reason |
| --- | --- | --- | --- | --- | --- |
| dgamma | all free | lower | `criterion` | regraded | On [m,B] the grouped compactness proof gives sufficiency and the exact occupancy split includes samples using three or more cells. |
| dgamma | all free | lower | `failure` | accepted | On [m,B], the occupancy split and the three integrable grouped faces exhaust the competitive parent ends. |
| dgamma | all free | lower | `case:bound-cardinality` | regraded | On [m,B] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | all free | lower | `case:adjacent-cells` | regraded | On [m,B] the monotone grouped odds and the Bernoulli upper bound prove the continuum, and the refutation now names an exact sample and fixed cuts. |
| dgamma | all free | upper | `criterion` | regraded | On [A,M] the grouped compactness proof gives sufficiency and the exact occupancy split includes samples using three or more cells. |
| dgamma | all free | upper | `failure` | accepted | On [A,M], the occupancy split and the three integrable grouped faces exhaust the competitive parent ends. |
| dgamma | all free | upper | `case:bound-cardinality` | regraded | On [A,M] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | all free | upper | `case:adjacent-cells` | regraded | On [A,M] the monotone grouped odds and the Bernoulli upper bound prove the continuum, and the refutation now names an exact sample and fixed cuts. |
| dgamma | all free | both | `criterion` | regraded | On [m,M] the grouped compactness proof gives sufficiency and the exact occupancy split includes samples using three or more cells. |
| dgamma | all free | both | `failure` | accepted | On [m,M], the occupancy split and the three integrable grouped faces exhaust the competitive parent ends. |
| dgamma | all free | both | `case:bound-cardinality` | regraded | On [m,M] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | all free | both | `case:singleton` | accepted | Both estimated cuts retain exactly the occupied cell, whose conditional mass is one for every held-compatible parent vector. |
| dgamma | all free | both | `case:adjacent-cells` | regraded | On [m,M] the monotone grouped odds and the Bernoulli upper bound prove the continuum, and the refutation now names an exact sample and fixed cuts. |
| dgamma | rate | lower | `criterion` | accepted | On [m,B], shape zero yields an atom or inverse-power exponential and infinite shape gives a last-cell atom only at finite U. |
| dgamma | rate | lower | `failure` | accepted | On [m,B], shape zero yields an atom or inverse-power exponential and infinite shape gives a last-cell atom only at finite U. |
| dgamma | rate | lower | `case:bound-cardinality` | regraded | On [m,B] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | rate | lower | `case:two-cell-uniqueness` | accepted | On [m,B] the adjacent cells strictly order their conditional means of log y, so there is at most one winning parent coefficient with both cells occupied. |
| dgamma | rate | upper | `criterion` | accepted | On [A,M], shape zero yields an atom or inverse-power exponential and infinite shape gives a last-cell atom only at finite U. |
| dgamma | rate | upper | `failure` | accepted | On [A,M], shape zero yields an atom or inverse-power exponential and infinite shape gives a last-cell atom only at finite U. |
| dgamma | rate | upper | `case:bound-cardinality` | regraded | On [A,M] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | rate | upper | `case:two-cell-uniqueness` | accepted | On [A,M] the adjacent cells strictly order their conditional means of log y, so there is at most one winning parent coefficient with both cells occupied. |
| dgamma | rate | both | `criterion` | accepted | On [m,M], shape zero yields an atom or inverse-power exponential and infinite shape gives a last-cell atom only at finite U. |
| dgamma | rate | both | `failure` | accepted | On [m,M], shape zero yields an atom or inverse-power exponential and infinite shape gives a last-cell atom only at finite U. |
| dgamma | rate | both | `case:bound-cardinality` | regraded | On [m,M] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | rate | both | `case:singleton` | accepted | Both estimated cuts retain exactly the occupied cell, whose conditional mass is one for every held-compatible parent vector. |
| dgamma | rate | both | `case:two-cell-uniqueness` | accepted | On [m,M] the adjacent cells strictly order their conditional means of log y, so there is at most one winning parent coefficient with both cells occupied. |
| dgamma | rate+shape | lower | `criterion` | accepted | On [m,B], positive mass in every count cell makes the held vector feasible whenever the cuts contain the sample. |
| dgamma | rate+shape | lower | `failure` | accepted | On [m,B], positive mass in every count cell makes the held vector feasible whenever the cuts contain the sample. |
| dgamma | rate+shape | lower | `uniqueness` | accepted | On [m,B], positive mass in every count cell makes the held vector feasible whenever the cuts contain the sample and the cut rounding intervals give the full joint fiber. |
| dgamma | rate+shape | lower | `case:bound-cardinality` | regraded | On [m,B] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | rate+shape | upper | `criterion` | accepted | On [A,M], positive mass in every count cell makes the held vector feasible whenever the cuts contain the sample. |
| dgamma | rate+shape | upper | `failure` | accepted | On [A,M], positive mass in every count cell makes the held vector feasible whenever the cuts contain the sample. |
| dgamma | rate+shape | upper | `uniqueness` | accepted | On [A,M], positive mass in every count cell makes the held vector feasible whenever the cuts contain the sample and the cut rounding intervals give the full joint fiber. |
| dgamma | rate+shape | upper | `case:bound-cardinality` | regraded | On [A,M] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | rate+shape | both | `criterion` | accepted | On [m,M], positive mass in every count cell makes the held vector feasible whenever the cuts contain the sample. |
| dgamma | rate+shape | both | `failure` | accepted | On [m,M], positive mass in every count cell makes the held vector feasible whenever the cuts contain the sample. |
| dgamma | rate+shape | both | `uniqueness` | accepted | On [m,M], positive mass in every count cell makes the held vector feasible whenever the cuts contain the sample and the cut rounding intervals give the full joint fiber. |
| dgamma | rate+shape | both | `case:bound-cardinality` | regraded | On [m,M] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | shape | lower | `criterion` | accepted | On [m,B], the zero-rate grouped power is eligible only at finite U and infinite rate collapses to the first cell. |
| dgamma | shape | lower | `failure` | accepted | On [m,B], the zero-rate grouped power is eligible only at finite U and infinite rate collapses to the first cell. |
| dgamma | shape | lower | `case:bound-cardinality` | regraded | On [m,B] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | shape | lower | `case:two-cell-uniqueness` | accepted | On [m,B] the adjacent cells strictly order their conditional means of -y, so there is at most one winning parent coefficient with both cells occupied. |
| dgamma | shape | upper | `criterion` | accepted | On [A,M], the zero-rate grouped power is eligible only at finite U and infinite rate collapses to the first cell. |
| dgamma | shape | upper | `failure` | accepted | On [A,M], the zero-rate grouped power is eligible only at finite U and infinite rate collapses to the first cell. |
| dgamma | shape | upper | `case:bound-cardinality` | regraded | On [A,M] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | shape | upper | `case:two-cell-uniqueness` | accepted | On [A,M] the adjacent cells strictly order their conditional means of -y, so there is at most one winning parent coefficient with both cells occupied. |
| dgamma | shape | both | `criterion` | accepted | On [m,M], the zero-rate grouped power is eligible only at finite U and infinite rate collapses to the first cell. |
| dgamma | shape | both | `failure` | accepted | On [m,M], the zero-rate grouped power is eligible only at finite U and infinite rate collapses to the first cell. |
| dgamma | shape | both | `case:bound-cardinality` | regraded | On [m,M] positivity at every retained cell proves the full interval of equal-mass real cuts. |
| dgamma | shape | both | `case:singleton` | accepted | Both estimated cuts retain exactly the occupied cell, whose conditional mass is one for every held-compatible parent vector. |
| dgamma | shape | both | `case:two-cell-uniqueness` | accepted | On [m,M] the adjacent cells strictly order their conditional means of -y, so there is at most one winning parent coefficient with both cells occupied. |
| fpareto | all free | lower | `criterion` | accepted | On [m,B], a free low-end factor permits a subunit threshold spike. |
| fpareto | all free | lower | `failure` | accepted | On [m,B], a free low-end factor permits a subunit threshold spike. |
| fpareto | all free | lower | `uniqueness` | accepted | On [m,B], a free low-end factor permits a subunit threshold spike, leaving an empty finite-positive maximizing set. |
| fpareto | all free | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | all free | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | all free | upper | `criterion` | accepted | On [A,M], a free low-end factor permits a subunit threshold spike. |
| fpareto | all free | upper | `failure` | accepted | On [A,M], a free low-end factor permits a subunit threshold spike. |
| fpareto | all free | upper | `uniqueness` | accepted | On [A,M], a free low-end factor permits a subunit threshold spike, leaving an empty finite-positive maximizing set. |
| fpareto | all free | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | all free | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | all free | both | `criterion` | accepted | On [m,M], a free low-end factor permits a subunit threshold spike. |
| fpareto | all free | both | `failure` | accepted | On [m,M], a free low-end factor permits a subunit threshold spike. |
| fpareto | all free | both | `uniqueness` | accepted | On [m,M], a free low-end factor permits a subunit threshold spike, leaving an empty finite-positive maximizing set. |
| fpareto | all free | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | all free | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min | lower | `criterion` | accepted | On [m,B], translation leaves the all-free GB2 faces and the integrability of the translated cuts determines eligibility. |
| fpareto | min | lower | `failure` | accepted | On [m,B], translation leaves the all-free GB2 faces and the integrability of the translated cuts determines eligibility. |
| fpareto | min | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min | upper | `criterion` | accepted | On [A,M], translation leaves the all-free GB2 faces and the integrability of the translated cuts determines eligibility. |
| fpareto | min | upper | `failure` | accepted | On [A,M], translation leaves the all-free GB2 faces and the integrability of the translated cuts determines eligibility. |
| fpareto | min | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min | both | `criterion` | accepted | On [m,M], translation leaves the all-free GB2 faces and the integrability of the translated cuts determines eligibility. |
| fpareto | min | both | `failure` | accepted | On [m,M], translation leaves the all-free GB2 faces and the integrability of the translated cuts determines eligibility. |
| fpareto | min | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+scale+shape1+shape2+shape3 | lower | `criterion` | accepted | On [m,B], the sole coefficient vector is decided by retained mass and its closed-threshold density. |
| fpareto | min+scale+shape1+shape2+shape3 | lower | `failure` | regraded | On [m,B] the closed threshold distinguishes impossible lower expansion at a fixed coincident upper cut from feasible right expansion. |
| fpareto | min+scale+shape1+shape2+shape3 | lower | `uniqueness` | accepted | On [m,B], the sole coefficient vector is decided by retained mass and its closed-threshold density, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| fpareto | min+scale+shape1+shape2+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+scale+shape1+shape2+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+scale+shape1+shape2+shape3 | upper | `criterion` | accepted | On [A,M], the sole coefficient vector is decided by retained mass and its closed-threshold density. |
| fpareto | min+scale+shape1+shape2+shape3 | upper | `failure` | regraded | On [A,M] the closed threshold distinguishes impossible lower expansion at a fixed coincident upper cut from feasible right expansion. |
| fpareto | min+scale+shape1+shape2+shape3 | upper | `uniqueness` | accepted | On [A,M], the sole coefficient vector is decided by retained mass and its closed-threshold density, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| fpareto | min+scale+shape1+shape2+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+scale+shape1+shape2+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+scale+shape1+shape2+shape3 | both | `criterion` | accepted | On [m,M], the sole coefficient vector is decided by retained mass and its closed-threshold density. |
| fpareto | min+scale+shape1+shape2+shape3 | both | `failure` | regraded | On [m,M] the closed threshold distinguishes impossible lower expansion at a fixed coincident upper cut from feasible right expansion. |
| fpareto | min+scale+shape1+shape2+shape3 | both | `uniqueness` | accepted | On [m,M], the sole coefficient vector is decided by retained mass and its closed-threshold density, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| fpareto | min+scale+shape1+shape2+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+scale+shape1+shape2+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+scale+shape2 | lower | `criterion` | accepted | On [m,B], held threshold, scale and power fix the beta coordinate, and strict concavity and inward tests settle its two free shapes. |
| fpareto | min+scale+shape2 | lower | `failure` | accepted | On [m,B], held threshold, scale and power fix the beta coordinate, and strict concavity and inward tests settle its two free shapes. |
| fpareto | min+scale+shape2 | lower | `uniqueness` | accepted | On [m,B], held threshold, scale and power fix the beta coordinate, and strict concavity and inward tests settle its two free shapes, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| fpareto | min+scale+shape2 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+scale+shape2 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+scale+shape2 | upper | `criterion` | accepted | On [A,M], held threshold, scale and power fix the beta coordinate, and strict concavity and inward tests settle its two free shapes. |
| fpareto | min+scale+shape2 | upper | `failure` | accepted | On [A,M], held threshold, scale and power fix the beta coordinate, and strict concavity and inward tests settle its two free shapes. |
| fpareto | min+scale+shape2 | upper | `uniqueness` | accepted | On [A,M], held threshold, scale and power fix the beta coordinate, and strict concavity and inward tests settle its two free shapes, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| fpareto | min+scale+shape2 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+scale+shape2 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+scale+shape2 | both | `criterion` | accepted | On [m,M], held threshold, scale and power fix the beta coordinate, and strict concavity and inward tests settle its two free shapes. |
| fpareto | min+scale+shape2 | both | `failure` | accepted | On [m,M], held threshold, scale and power fix the beta coordinate, and strict concavity and inward tests settle its two free shapes. |
| fpareto | min+scale+shape2 | both | `uniqueness` | accepted | On [m,M], held threshold, scale and power fix the beta coordinate, and strict concavity and inward tests settle its two free shapes, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| fpareto | min+scale+shape2 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+scale+shape2 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+shape1 | lower | `criterion` | accepted | On [m,B], translation leaves the held-q GB2 faces with nonnegative small-power slope and their stated integrability tests. |
| fpareto | min+shape1 | lower | `failure` | accepted | On [m,B], translation leaves the held-q GB2 faces with nonnegative small-power slope and their stated integrability tests. |
| fpareto | min+shape1 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+shape1 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+shape1 | upper | `criterion` | accepted | On [A,M], translation leaves the held-q GB2 faces with nonnegative small-power slope and their stated integrability tests. |
| fpareto | min+shape1 | upper | `failure` | accepted | On [A,M], translation leaves the held-q GB2 faces with nonnegative small-power slope and their stated integrability tests. |
| fpareto | min+shape1 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+shape1 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+shape1 | both | `criterion` | accepted | On [m,M], translation leaves the held-q GB2 faces with nonnegative small-power slope and their stated integrability tests. |
| fpareto | min+shape1 | both | `failure` | accepted | On [m,M], translation leaves the held-q GB2 faces with nonnegative small-power slope and their stated integrability tests. |
| fpareto | min+shape1 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+shape1 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+shape1+shape3 | lower | `criterion` | regraded | On [m,B] the linked fixed-shape GB2 section omits nonzero log-excess tilts as scale escapes at finite power. |
| fpareto | min+shape1+shape3 | lower | `failure` | regraded | On [m,B] the linked fixed-shape GB2 section omits nonzero log-excess tilts as scale escapes at finite power. |
| fpareto | min+shape1+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+shape1+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+shape1+shape3 | upper | `criterion` | regraded | On [A,M] the linked fixed-shape GB2 section omits nonzero log-excess tilts as scale escapes at finite power. |
| fpareto | min+shape1+shape3 | upper | `failure` | regraded | On [A,M] the linked fixed-shape GB2 section omits nonzero log-excess tilts as scale escapes at finite power. |
| fpareto | min+shape1+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+shape1+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+shape1+shape3 | both | `criterion` | regraded | On [m,M] the linked fixed-shape GB2 section omits nonzero log-excess tilts as scale escapes at finite power. |
| fpareto | min+shape1+shape3 | both | `failure` | regraded | On [m,M] the linked fixed-shape GB2 section omits nonzero log-excess tilts as scale escapes at finite power. |
| fpareto | min+shape1+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+shape1+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+shape3 | lower | `criterion` | accepted | On [m,B], translation leaves the held-p GB2 faces with nonpositive small-power slope and their stated integrability tests. |
| fpareto | min+shape3 | lower | `failure` | accepted | On [m,B], translation leaves the held-p GB2 faces with nonpositive small-power slope and their stated integrability tests. |
| fpareto | min+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+shape3 | upper | `criterion` | accepted | On [A,M], translation leaves the held-p GB2 faces with nonpositive small-power slope and their stated integrability tests. |
| fpareto | min+shape3 | upper | `failure` | accepted | On [A,M], translation leaves the held-p GB2 faces with nonpositive small-power slope and their stated integrability tests. |
| fpareto | min+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | min+shape3 | both | `criterion` | accepted | On [m,M], translation leaves the held-p GB2 faces with nonpositive small-power slope and their stated integrability tests. |
| fpareto | min+shape3 | both | `failure` | accepted | On [m,M], translation leaves the held-p GB2 faces with nonpositive small-power slope and their stated integrability tests. |
| fpareto | min+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | min+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale | lower | `criterion` | accepted | On [m,B], both low-end factors remain free at held scale. |
| fpareto | scale | lower | `failure` | accepted | On [m,B], both low-end factors remain free at held scale. |
| fpareto | scale | lower | `uniqueness` | accepted | On [m,B], both low-end factors remain free at held scale, leaving an empty finite-positive maximizing set. |
| fpareto | scale | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale | upper | `criterion` | accepted | On [A,M], both low-end factors remain free at held scale. |
| fpareto | scale | upper | `failure` | accepted | On [A,M], both low-end factors remain free at held scale. |
| fpareto | scale | upper | `uniqueness` | accepted | On [A,M], both low-end factors remain free at held scale, leaving an empty finite-positive maximizing set. |
| fpareto | scale | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale | both | `criterion` | accepted | On [m,M], both low-end factors remain free at held scale. |
| fpareto | scale | both | `failure` | accepted | On [m,M], both low-end factors remain free at held scale. |
| fpareto | scale | both | `uniqueness` | accepted | On [m,M], both low-end factors remain free at held scale, leaving an empty finite-positive maximizing set. |
| fpareto | scale | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1 | lower | `criterion` | accepted | On [m,B], the held tail shape still permits a subunit low-end product. |
| fpareto | scale+shape1 | lower | `failure` | accepted | On [m,B], the held tail shape still permits a subunit low-end product. |
| fpareto | scale+shape1 | lower | `uniqueness` | accepted | On [m,B], the held tail shape still permits a subunit low-end product, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape1 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1 | upper | `criterion` | accepted | On [A,M], the held tail shape still permits a subunit low-end product. |
| fpareto | scale+shape1 | upper | `failure` | accepted | On [A,M], the held tail shape still permits a subunit low-end product. |
| fpareto | scale+shape1 | upper | `uniqueness` | accepted | On [A,M], the held tail shape still permits a subunit low-end product, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape1 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1 | both | `criterion` | accepted | On [m,M], the held tail shape still permits a subunit low-end product. |
| fpareto | scale+shape1 | both | `failure` | accepted | On [m,M], the held tail shape still permits a subunit low-end product. |
| fpareto | scale+shape1 | both | `uniqueness` | accepted | On [m,M], the held tail shape still permits a subunit low-end product, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape1 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1+shape2 | lower | `criterion` | accepted | On [m,B], free shape3 can make the held power product subunit. |
| fpareto | scale+shape1+shape2 | lower | `failure` | accepted | On [m,B], free shape3 can make the held power product subunit. |
| fpareto | scale+shape1+shape2 | lower | `uniqueness` | accepted | On [m,B], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape1+shape2 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1+shape2 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1+shape2 | upper | `criterion` | accepted | On [A,M], free shape3 can make the held power product subunit. |
| fpareto | scale+shape1+shape2 | upper | `failure` | accepted | On [A,M], free shape3 can make the held power product subunit. |
| fpareto | scale+shape1+shape2 | upper | `uniqueness` | accepted | On [A,M], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape1+shape2 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1+shape2 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1+shape2 | both | `criterion` | accepted | On [m,M], free shape3 can make the held power product subunit. |
| fpareto | scale+shape1+shape2 | both | `failure` | accepted | On [m,M], free shape3 can make the held power product subunit. |
| fpareto | scale+shape1+shape2 | both | `uniqueness` | accepted | On [m,M], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape1+shape2 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1+shape2 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1+shape2+shape3 | lower | `criterion` | accepted | On [m,B], only the remote uniform can compete at finite positive excluded height outside the scalar spike and width branches. |
| fpareto | scale+shape1+shape2+shape3 | lower | `failure` | accepted | On [m,B], only the remote uniform can compete at finite positive excluded height outside the scalar spike and width branches. |
| fpareto | scale+shape1+shape2+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1+shape2+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1+shape2+shape3 | upper | `criterion` | accepted | On [A,M], only the remote uniform can compete at finite positive excluded height outside the scalar spike and width branches. |
| fpareto | scale+shape1+shape2+shape3 | upper | `failure` | accepted | On [A,M], only the remote uniform can compete at finite positive excluded height outside the scalar spike and width branches. |
| fpareto | scale+shape1+shape2+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1+shape2+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1+shape2+shape3 | both | `criterion` | accepted | On [m,M], only the remote uniform can compete at finite positive excluded height outside the scalar spike and width branches. |
| fpareto | scale+shape1+shape2+shape3 | both | `failure` | accepted | On [m,M], only the remote uniform can compete at finite positive excluded height outside the scalar spike and width branches. |
| fpareto | scale+shape1+shape2+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1+shape2+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1+shape3 | lower | `criterion` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape1+shape3 | lower | `failure` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape1+shape3 | lower | `uniqueness` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape1+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1+shape3 | upper | `criterion` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape1+shape3 | upper | `failure` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape1+shape3 | upper | `uniqueness` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape1+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape1+shape3 | both | `criterion` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape1+shape3 | both | `failure` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape1+shape3 | both | `uniqueness` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape1+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape1+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape2 | lower | `criterion` | accepted | On [m,B], free shape3 can make the held power product subunit. |
| fpareto | scale+shape2 | lower | `failure` | accepted | On [m,B], free shape3 can make the held power product subunit. |
| fpareto | scale+shape2 | lower | `uniqueness` | accepted | On [m,B], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape2 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape2 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape2 | upper | `criterion` | accepted | On [A,M], free shape3 can make the held power product subunit. |
| fpareto | scale+shape2 | upper | `failure` | accepted | On [A,M], free shape3 can make the held power product subunit. |
| fpareto | scale+shape2 | upper | `uniqueness` | accepted | On [A,M], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape2 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape2 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape2 | both | `criterion` | accepted | On [m,M], free shape3 can make the held power product subunit. |
| fpareto | scale+shape2 | both | `failure` | accepted | On [m,M], free shape3 can make the held power product subunit. |
| fpareto | scale+shape2 | both | `uniqueness` | accepted | On [m,M], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape2 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape2 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape2+shape3 | lower | `criterion` | accepted | On [m,B], the free tail and threshold comparison retains the eligible zero-tail kernel and remote exponential or uniform. |
| fpareto | scale+shape2+shape3 | lower | `failure` | accepted | On [m,B], the free tail and threshold comparison retains the eligible zero-tail kernel and remote exponential or uniform. |
| fpareto | scale+shape2+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape2+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape2+shape3 | upper | `criterion` | accepted | On [A,M], the free tail and threshold comparison retains the eligible zero-tail kernel and remote exponential or uniform. |
| fpareto | scale+shape2+shape3 | upper | `failure` | accepted | On [A,M], the free tail and threshold comparison retains the eligible zero-tail kernel and remote exponential or uniform. |
| fpareto | scale+shape2+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape2+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape2+shape3 | both | `criterion` | accepted | On [m,M], the free tail and threshold comparison retains the eligible zero-tail kernel and remote exponential or uniform. |
| fpareto | scale+shape2+shape3 | both | `failure` | accepted | On [m,M], the free tail and threshold comparison retains the eligible zero-tail kernel and remote exponential or uniform. |
| fpareto | scale+shape2+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape2+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape3 | lower | `criterion` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape3 | lower | `failure` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape3 | lower | `uniqueness` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape3 | upper | `criterion` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape3 | upper | `failure` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape3 | upper | `uniqueness` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | scale+shape3 | both | `criterion` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape3 | both | `failure` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | scale+shape3 | both | `uniqueness` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | scale+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | scale+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1 | lower | `criterion` | accepted | On [m,B], the held tail shape still permits a subunit low-end product. |
| fpareto | shape1 | lower | `failure` | accepted | On [m,B], the held tail shape still permits a subunit low-end product. |
| fpareto | shape1 | lower | `uniqueness` | accepted | On [m,B], the held tail shape still permits a subunit low-end product, leaving an empty finite-positive maximizing set. |
| fpareto | shape1 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1 | upper | `criterion` | accepted | On [A,M], the held tail shape still permits a subunit low-end product. |
| fpareto | shape1 | upper | `failure` | accepted | On [A,M], the held tail shape still permits a subunit low-end product. |
| fpareto | shape1 | upper | `uniqueness` | accepted | On [A,M], the held tail shape still permits a subunit low-end product, leaving an empty finite-positive maximizing set. |
| fpareto | shape1 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1 | both | `criterion` | accepted | On [m,M], the held tail shape still permits a subunit low-end product. |
| fpareto | shape1 | both | `failure` | accepted | On [m,M], the held tail shape still permits a subunit low-end product. |
| fpareto | shape1 | both | `uniqueness` | accepted | On [m,M], the held tail shape still permits a subunit low-end product, leaving an empty finite-positive maximizing set. |
| fpareto | shape1 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1+shape2 | lower | `criterion` | accepted | On [m,B], free shape3 can make the held power product subunit. |
| fpareto | shape1+shape2 | lower | `failure` | accepted | On [m,B], free shape3 can make the held power product subunit. |
| fpareto | shape1+shape2 | lower | `uniqueness` | accepted | On [m,B], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape1+shape2 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1+shape2 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1+shape2 | upper | `criterion` | accepted | On [A,M], free shape3 can make the held power product subunit. |
| fpareto | shape1+shape2 | upper | `failure` | accepted | On [A,M], free shape3 can make the held power product subunit. |
| fpareto | shape1+shape2 | upper | `uniqueness` | accepted | On [A,M], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape1+shape2 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1+shape2 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1+shape2 | both | `criterion` | accepted | On [m,M], free shape3 can make the held power product subunit. |
| fpareto | shape1+shape2 | both | `failure` | accepted | On [m,M], free shape3 can make the held power product subunit. |
| fpareto | shape1+shape2 | both | `uniqueness` | accepted | On [m,M], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape1+shape2 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1+shape2 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1+shape2+shape3 | lower | `criterion` | accepted | On [m,B], the held-shape atom envelope uses the survival denominator exactly when the tight lower cut equals m. |
| fpareto | shape1+shape2+shape3 | lower | `failure` | accepted | On [m,B], the held-shape atom envelope uses the survival denominator exactly when the tight lower cut equals m. |
| fpareto | shape1+shape2+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1+shape2+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1+shape2+shape3 | lower | `case:exact-nonunique-sample` | regraded | The exact shifted-Lomax height factors through s with its global maximum at s=1/2 and two feasible decompositions. |
| fpareto | shape1+shape2+shape3 | upper | `criterion` | accepted | On [A,M], the held-shape atom envelope uses the survival denominator exactly when the tight lower cut equals m. |
| fpareto | shape1+shape2+shape3 | upper | `failure` | accepted | On [A,M], the held-shape atom envelope uses the survival denominator exactly when the tight lower cut equals m. |
| fpareto | shape1+shape2+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1+shape2+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1+shape2+shape3 | both | `criterion` | accepted | On [m,M], the held-shape atom envelope uses the survival denominator exactly when the tight lower cut equals m. |
| fpareto | shape1+shape2+shape3 | both | `failure` | accepted | On [m,M], the held-shape atom envelope uses the survival denominator exactly when the tight lower cut equals m. |
| fpareto | shape1+shape2+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1+shape2+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1+shape3 | lower | `criterion` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape1+shape3 | lower | `failure` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape1+shape3 | lower | `uniqueness` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape1+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1+shape3 | upper | `criterion` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape1+shape3 | upper | `failure` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape1+shape3 | upper | `uniqueness` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape1+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape1+shape3 | both | `criterion` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape1+shape3 | both | `failure` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape1+shape3 | both | `uniqueness` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape1+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape1+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape2 | lower | `criterion` | accepted | On [m,B], free shape3 can make the held power product subunit. |
| fpareto | shape2 | lower | `failure` | accepted | On [m,B], free shape3 can make the held power product subunit. |
| fpareto | shape2 | lower | `uniqueness` | accepted | On [m,B], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape2 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape2 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape2 | upper | `criterion` | accepted | On [A,M], free shape3 can make the held power product subunit. |
| fpareto | shape2 | upper | `failure` | accepted | On [A,M], free shape3 can make the held power product subunit. |
| fpareto | shape2 | upper | `uniqueness` | accepted | On [A,M], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape2 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape2 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape2 | both | `criterion` | accepted | On [m,M], free shape3 can make the held power product subunit. |
| fpareto | shape2 | both | `failure` | accepted | On [m,M], free shape3 can make the held power product subunit. |
| fpareto | shape2 | both | `uniqueness` | accepted | On [m,M], free shape3 can make the held power product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape2 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape2 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape2+shape3 | lower | `criterion` | accepted | On [m,B], a held subunit product permits a spike and otherwise free tail shape and scale permit a negative collapse exponent. |
| fpareto | shape2+shape3 | lower | `failure` | accepted | On [m,B], a held subunit product permits a spike and otherwise free tail shape and scale permit a negative collapse exponent. |
| fpareto | shape2+shape3 | lower | `uniqueness` | accepted | On [m,B], a held subunit product permits a spike and otherwise free tail shape and scale permit a negative collapse exponent, leaving an empty finite-positive maximizing set. |
| fpareto | shape2+shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape2+shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape2+shape3 | upper | `criterion` | accepted | On [A,M], a held subunit product permits a spike and otherwise free tail shape and scale permit a negative collapse exponent. |
| fpareto | shape2+shape3 | upper | `failure` | accepted | On [A,M], a held subunit product permits a spike and otherwise free tail shape and scale permit a negative collapse exponent. |
| fpareto | shape2+shape3 | upper | `uniqueness` | accepted | On [A,M], a held subunit product permits a spike and otherwise free tail shape and scale permit a negative collapse exponent, leaving an empty finite-positive maximizing set. |
| fpareto | shape2+shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape2+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape2+shape3 | both | `criterion` | accepted | On [m,M], a held subunit product permits a spike and otherwise free tail shape and scale permit a negative collapse exponent. |
| fpareto | shape2+shape3 | both | `failure` | accepted | On [m,M], a held subunit product permits a spike and otherwise free tail shape and scale permit a negative collapse exponent. |
| fpareto | shape2+shape3 | both | `uniqueness` | accepted | On [m,M], a held subunit product permits a spike and otherwise free tail shape and scale permit a negative collapse exponent, leaving an empty finite-positive maximizing set. |
| fpareto | shape2+shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape2+shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape3 | lower | `criterion` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape3 | lower | `failure` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape3 | lower | `uniqueness` | accepted | On [m,B], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape3 | lower | `case:null-tight-slice` | accepted | On [m,B], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape3 | lower | `case:bound-cardinality` | accepted | On [m,B], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape3 | upper | `criterion` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape3 | upper | `failure` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape3 | upper | `uniqueness` | accepted | On [A,M], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape3 | upper | `case:null-tight-slice` | accepted | On [A,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| fpareto | shape3 | both | `criterion` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape3 | both | `failure` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit. |
| fpareto | shape3 | both | `uniqueness` | accepted | On [m,M], free shape2 can make the held lower-shape product subunit, leaving an empty finite-positive maximizing set. |
| fpareto | shape3 | both | `case:null-tight-slice` | accepted | On [m,M], threshold contact uses the inside density with exponent shape2*shape3-1, and the fixed-side geometry determines whether an expansion reaches the interior. |
| fpareto | shape3 | both | `case:bound-cardinality` | accepted | On [m,M], the parent is positive above its closed threshold and has no finite upper endpoint. |
| genbeta | all free | lower | `criterion` | accepted | On [m,B], free scale and upper shape allow a subunit spike even at the tight upper cut. |
| genbeta | all free | lower | `failure` | accepted | On [m,B], free scale and upper shape allow a subunit spike even at the tight upper cut. |
| genbeta | all free | lower | `uniqueness` | accepted | On [m,B], free scale and upper shape allow a subunit spike even at the tight upper cut, leaving an empty finite-positive maximizing set. |
| genbeta | all free | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | all free | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | all free | upper | `criterion` | accepted | On [A,M], free scale and upper shape allow a subunit spike even at the tight upper cut. |
| genbeta | all free | upper | `failure` | accepted | On [A,M], free scale and upper shape allow a subunit spike even at the tight upper cut. |
| genbeta | all free | upper | `uniqueness` | accepted | On [A,M], free scale and upper shape allow a subunit spike even at the tight upper cut, leaving an empty finite-positive maximizing set. |
| genbeta | all free | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | all free | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | all free | both | `criterion` | accepted | On [m,M], free scale and upper shape allow a subunit spike even at the tight upper cut. |
| genbeta | all free | both | `failure` | accepted | On [m,M], free scale and upper shape allow a subunit spike even at the tight upper cut. |
| genbeta | all free | both | `uniqueness` | accepted | On [m,M], free scale and upper shape allow a subunit spike even at the tight upper cut, leaving an empty finite-positive maximizing set. |
| genbeta | all free | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | all free | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale | lower | `criterion` | accepted | On [m,B], the held-scale log kernel supplies the zero-shape, log-distance and wall envelopes, with singular contact and constant concentration checked separately. |
| genbeta | scale | lower | `failure` | accepted | On [m,B], the held-scale log kernel supplies the zero-shape, log-distance and wall envelopes, with singular contact and constant concentration checked separately. |
| genbeta | scale | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale | upper | `criterion` | accepted | On [A,M], the held-scale log kernel supplies the zero-shape, log-distance and wall envelopes, with singular contact and constant concentration checked separately. |
| genbeta | scale | upper | `failure` | accepted | On [A,M], the held-scale log kernel supplies the zero-shape, log-distance and wall envelopes, with singular contact and constant concentration checked separately. |
| genbeta | scale | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | scale | both | `criterion` | accepted | On [m,M], the held-scale log kernel supplies the zero-shape, log-distance and wall envelopes, with singular contact and constant concentration checked separately. |
| genbeta | scale | both | `failure` | accepted | On [m,M], the held-scale log kernel supplies the zero-shape, log-distance and wall envelopes, with singular contact and constant concentration checked separately. |
| genbeta | scale | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape1 | lower | `criterion` | accepted | On [m,B], the held lower shape leaves the zero-upper-shape and log-distance faces on their integrable cuts. |
| genbeta | scale+shape1 | lower | `failure` | accepted | On [m,B], the held lower shape leaves the zero-upper-shape and log-distance faces on their integrable cuts. |
| genbeta | scale+shape1 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape1 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape1 | upper | `criterion` | accepted | On [A,M], the held lower shape leaves the zero-upper-shape and log-distance faces on their integrable cuts. |
| genbeta | scale+shape1 | upper | `failure` | accepted | On [A,M], the held lower shape leaves the zero-upper-shape and log-distance faces on their integrable cuts. |
| genbeta | scale+shape1 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape1 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | scale+shape1 | both | `criterion` | accepted | On [m,M], the held lower shape leaves the zero-upper-shape and log-distance faces on their integrable cuts. |
| genbeta | scale+shape1 | both | `failure` | accepted | On [m,M], the held lower shape leaves the zero-upper-shape and log-distance faces on their integrable cuts. |
| genbeta | scale+shape1 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape1 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape1+shape2+shape3 | lower | `criterion` | accepted | On [m,B], the sole coefficient vector has a finite positive fit exactly on the positive-mass and positive-density branch. |
| genbeta | scale+shape1+shape2+shape3 | lower | `failure` | regraded | On [m,B] a null tight slice can permit diverging cut sequences even when all four parent coefficients are held. |
| genbeta | scale+shape1+shape2+shape3 | lower | `uniqueness` | accepted | On [m,B], the sole coefficient vector has a finite positive fit exactly on the positive-mass and positive-density branch, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape1+shape2+shape3 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape1+shape2+shape3 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape1+shape2+shape3 | upper | `criterion` | accepted | On [A,M], the sole coefficient vector has a finite positive fit exactly on the positive-mass and positive-density branch. |
| genbeta | scale+shape1+shape2+shape3 | upper | `failure` | regraded | On [A,M] a null tight slice can permit diverging cut sequences even when all four parent coefficients are held. |
| genbeta | scale+shape1+shape2+shape3 | upper | `uniqueness` | accepted | On [A,M], the sole coefficient vector has a finite positive fit exactly on the positive-mass and positive-density branch, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape1+shape2+shape3 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape1+shape2+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | scale+shape1+shape2+shape3 | both | `criterion` | accepted | On [m,M], the sole coefficient vector has a finite positive fit exactly on the positive-mass and positive-density branch. |
| genbeta | scale+shape1+shape2+shape3 | both | `failure` | regraded | On [m,M] a null tight slice can permit diverging cut sequences even when all four parent coefficients are held. |
| genbeta | scale+shape1+shape2+shape3 | both | `uniqueness` | accepted | On [m,M], the sole coefficient vector has a finite positive fit exactly on the positive-mass and positive-density branch, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape1+shape2+shape3 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape1+shape2+shape3 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape1+shape3 | lower | `criterion` | accepted | On [m,B], the beta coordinate is fixed and strict scalar tilt monotonicity applies at interior observations. |
| genbeta | scale+shape1+shape3 | lower | `failure` | accepted | On [m,B], the beta coordinate is fixed and strict scalar tilt monotonicity applies at interior observations. |
| genbeta | scale+shape1+shape3 | lower | `uniqueness` | accepted | On [m,B], the beta coordinate is fixed and strict scalar tilt monotonicity applies at interior observations, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape1+shape3 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape1+shape3 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape1+shape3 | upper | `criterion` | accepted | On [A,M], the beta coordinate is fixed and strict scalar tilt monotonicity applies at interior observations. |
| genbeta | scale+shape1+shape3 | upper | `failure` | accepted | On [A,M], the beta coordinate is fixed and strict scalar tilt monotonicity applies at interior observations. |
| genbeta | scale+shape1+shape3 | upper | `uniqueness` | accepted | On [A,M], the beta coordinate is fixed and strict scalar tilt monotonicity applies at interior observations, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape1+shape3 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape1+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | scale+shape1+shape3 | both | `criterion` | accepted | On [m,M], the beta coordinate is fixed and strict scalar tilt monotonicity applies at interior observations. |
| genbeta | scale+shape1+shape3 | both | `failure` | accepted | On [m,M], the beta coordinate is fixed and strict scalar tilt monotonicity applies at interior observations. |
| genbeta | scale+shape1+shape3 | both | `uniqueness` | accepted | On [m,M], the beta coordinate is fixed and strict scalar tilt monotonicity applies at interior observations, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape1+shape3 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape1+shape3 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape2 | lower | `criterion` | accepted | On [m,B], the held upper shape leaves the product rule at one and the three regular held-scale log-kernel faces off that seam. |
| genbeta | scale+shape2 | lower | `failure` | accepted | On [m,B], the held upper shape leaves the product rule at one and the three regular held-scale log-kernel faces off that seam. |
| genbeta | scale+shape2 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape2 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape2 | lower | `case:exact-nonunique-sample` | regraded | On [m,B] the sample (1,2,2) gives a strictly interior log-mean test and the optimum product has infinitely many positive factorizations. |
| genbeta | scale+shape2 | upper | `criterion` | accepted | On [A,M], the held upper shape leaves the product rule at one and the three regular held-scale log-kernel faces off that seam. |
| genbeta | scale+shape2 | upper | `failure` | accepted | On [A,M], the held upper shape leaves the product rule at one and the three regular held-scale log-kernel faces off that seam. |
| genbeta | scale+shape2 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape2 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | scale+shape2 | upper | `case:exact-nonunique-sample` | regraded | On [A,M] the sample (1,2,2) gives a strictly interior log-mean test and the optimum product has infinitely many positive factorizations. |
| genbeta | scale+shape2 | both | `criterion` | accepted | On [m,M], the held upper shape leaves the product rule at one and the three regular held-scale log-kernel faces off that seam. |
| genbeta | scale+shape2 | both | `failure` | accepted | On [m,M], the held upper shape leaves the product rule at one and the three regular held-scale log-kernel faces off that seam. |
| genbeta | scale+shape2 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape2 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape2 | both | `case:exact-nonunique-sample` | regraded | On [m,M] the sample (1,2,2) gives a strictly interior log-mean test and the optimum product has infinitely many positive factorizations. |
| genbeta | scale+shape2+shape3 | lower | `criterion` | accepted | On [m,B], the beta coordinate is fixed and the positive lower-shape tilt has explicit endpoint means. |
| genbeta | scale+shape2+shape3 | lower | `failure` | accepted | On [m,B], the beta coordinate is fixed and the positive lower-shape tilt has explicit endpoint means. |
| genbeta | scale+shape2+shape3 | lower | `uniqueness` | accepted | On [m,B], the beta coordinate is fixed and the positive lower-shape tilt has explicit endpoint means, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape2+shape3 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape2+shape3 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape2+shape3 | upper | `criterion` | accepted | On [A,M], the beta coordinate is fixed and the positive lower-shape tilt has explicit endpoint means. |
| genbeta | scale+shape2+shape3 | upper | `failure` | accepted | On [A,M], the beta coordinate is fixed and the positive lower-shape tilt has explicit endpoint means. |
| genbeta | scale+shape2+shape3 | upper | `uniqueness` | accepted | On [A,M], the beta coordinate is fixed and the positive lower-shape tilt has explicit endpoint means, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape2+shape3 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape2+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | scale+shape2+shape3 | both | `criterion` | accepted | On [m,M], the beta coordinate is fixed and the positive lower-shape tilt has explicit endpoint means. |
| genbeta | scale+shape2+shape3 | both | `failure` | accepted | On [m,M], the beta coordinate is fixed and the positive lower-shape tilt has explicit endpoint means. |
| genbeta | scale+shape2+shape3 | both | `uniqueness` | accepted | On [m,M], the beta coordinate is fixed and the positive lower-shape tilt has explicit endpoint means, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape2+shape3 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape2+shape3 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape3 | lower | `criterion` | accepted | On [m,B], the beta coordinate is fixed and strict joint concavity and inward tests control the two shapes on interior data. |
| genbeta | scale+shape3 | lower | `failure` | accepted | On [m,B], the beta coordinate is fixed and strict joint concavity and inward tests control the two shapes on interior data. |
| genbeta | scale+shape3 | lower | `uniqueness` | accepted | On [m,B], the beta coordinate is fixed and strict joint concavity and inward tests control the two shapes on interior data, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape3 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape3 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | scale+shape3 | upper | `criterion` | accepted | On [A,M], the beta coordinate is fixed and strict joint concavity and inward tests control the two shapes on interior data. |
| genbeta | scale+shape3 | upper | `failure` | accepted | On [A,M], the beta coordinate is fixed and strict joint concavity and inward tests control the two shapes on interior data. |
| genbeta | scale+shape3 | upper | `uniqueness` | accepted | On [A,M], the beta coordinate is fixed and strict joint concavity and inward tests control the two shapes on interior data, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape3 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | scale+shape3 | both | `criterion` | accepted | On [m,M], the beta coordinate is fixed and strict joint concavity and inward tests control the two shapes on interior data. |
| genbeta | scale+shape3 | both | `failure` | accepted | On [m,M], the beta coordinate is fixed and strict joint concavity and inward tests control the two shapes on interior data. |
| genbeta | scale+shape3 | both | `uniqueness` | accepted | On [m,M], the beta coordinate is fixed and strict joint concavity and inward tests control the two shapes on interior data, and the mass-equality rule supplies all nominal cut fibers of the sole tight winner. |
| genbeta | scale+shape3 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | scale+shape3 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape1 | lower | `criterion` | accepted | On [m,B], the held lower shape leaves the upper-shape spike available at free scale. |
| genbeta | shape1 | lower | `failure` | accepted | On [m,B], the held lower shape leaves the upper-shape spike available at free scale. |
| genbeta | shape1 | lower | `uniqueness` | accepted | On [m,B], the held lower shape leaves the upper-shape spike available at free scale, leaving an empty finite-positive maximizing set. |
| genbeta | shape1 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape1 | upper | `criterion` | accepted | On [A,M], the held lower shape leaves the upper-shape spike available at free scale. |
| genbeta | shape1 | upper | `failure` | accepted | On [A,M], the held lower shape leaves the upper-shape spike available at free scale. |
| genbeta | shape1 | upper | `uniqueness` | accepted | On [A,M], the held lower shape leaves the upper-shape spike available at free scale, leaving an empty finite-positive maximizing set. |
| genbeta | shape1 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | shape1 | both | `criterion` | accepted | On [m,M], the held lower shape leaves the upper-shape spike available at free scale. |
| genbeta | shape1 | both | `failure` | accepted | On [m,M], the held lower shape leaves the upper-shape spike available at free scale. |
| genbeta | shape1 | both | `uniqueness` | accepted | On [m,M], the held lower shape leaves the upper-shape spike available at free scale, leaving an empty finite-positive maximizing set. |
| genbeta | shape1 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape1+shape2 | lower | `criterion` | accepted | On [m,B], the free-scale table respects the fixed lower shape, with the scalar power rule at unit upper shape. |
| genbeta | shape1+shape2 | lower | `failure` | accepted | On [m,B], the free-scale table respects the fixed lower shape, with the scalar power rule at unit upper shape. |
| genbeta | shape1+shape2 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1+shape2 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape1+shape2 | upper | `criterion` | accepted | On [A,M], the free-scale table respects the fixed lower shape, with the scalar power rule at unit upper shape. |
| genbeta | shape1+shape2 | upper | `failure` | accepted | On [A,M], the free-scale table respects the fixed lower shape, with the scalar power rule at unit upper shape. |
| genbeta | shape1+shape2 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1+shape2 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | shape1+shape2 | both | `criterion` | accepted | On [m,M], the free-scale table respects the fixed lower shape, with the scalar power rule at unit upper shape. |
| genbeta | shape1+shape2 | both | `failure` | accepted | On [m,M], the free-scale table respects the fixed lower shape, with the scalar power rule at unit upper shape. |
| genbeta | shape1+shape2 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1+shape2 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape1+shape2+shape3 | lower | `criterion` | accepted | On [m,B], above unit upper shape contact has zero value and the bounded-window power is the only positive remote-scale height. |
| genbeta | shape1+shape2+shape3 | lower | `failure` | accepted | On [m,B], above unit upper shape contact has zero value and the bounded-window power is the only positive remote-scale height. |
| genbeta | shape1+shape2+shape3 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1+shape2+shape3 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape1+shape2+shape3 | upper | `criterion` | accepted | On [A,M], above unit upper shape contact has zero value and the bounded-window power is the only positive remote-scale height. |
| genbeta | shape1+shape2+shape3 | upper | `failure` | accepted | On [A,M], above unit upper shape contact has zero value and the bounded-window power is the only positive remote-scale height. |
| genbeta | shape1+shape2+shape3 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1+shape2+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | shape1+shape2+shape3 | both | `criterion` | accepted | On [m,M], above unit upper shape contact has zero value and the bounded-window power is the only positive remote-scale height. |
| genbeta | shape1+shape2+shape3 | both | `failure` | accepted | On [m,M], above unit upper shape contact has zero value and the bounded-window power is the only positive remote-scale height. |
| genbeta | shape1+shape2+shape3 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1+shape2+shape3 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape1+shape3 | lower | `criterion` | accepted | On [m,B], the held lower shape and power leave the upper-shape spike available at free scale. |
| genbeta | shape1+shape3 | lower | `failure` | accepted | On [m,B], the held lower shape and power leave the upper-shape spike available at free scale. |
| genbeta | shape1+shape3 | lower | `uniqueness` | accepted | On [m,B], the held lower shape and power leave the upper-shape spike available at free scale, leaving an empty finite-positive maximizing set. |
| genbeta | shape1+shape3 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1+shape3 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape1+shape3 | upper | `criterion` | accepted | On [A,M], the held lower shape and power leave the upper-shape spike available at free scale. |
| genbeta | shape1+shape3 | upper | `failure` | accepted | On [A,M], the held lower shape and power leave the upper-shape spike available at free scale. |
| genbeta | shape1+shape3 | upper | `uniqueness` | accepted | On [A,M], the held lower shape and power leave the upper-shape spike available at free scale, leaving an empty finite-positive maximizing set. |
| genbeta | shape1+shape3 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | shape1+shape3 | both | `criterion` | accepted | On [m,M], the held lower shape and power leave the upper-shape spike available at free scale. |
| genbeta | shape1+shape3 | both | `failure` | accepted | On [m,M], the held lower shape and power leave the upper-shape spike available at free scale. |
| genbeta | shape1+shape3 | both | `uniqueness` | accepted | On [m,M], the held lower shape and power leave the upper-shape spike available at free scale, leaving an empty finite-positive maximizing set. |
| genbeta | shape1+shape3 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape1+shape3 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape2 | lower | `criterion` | accepted | On [m,B], the free-scale table allows both factors of the power to vary and retains only integrable faces above unit upper shape. |
| genbeta | shape2 | lower | `failure` | accepted | On [m,B], the free-scale table allows both factors of the power to vary and retains only integrable faces above unit upper shape. |
| genbeta | shape2 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape2 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape2 | upper | `criterion` | accepted | On [A,M], the free-scale table allows both factors of the power to vary and retains only integrable faces above unit upper shape. |
| genbeta | shape2 | upper | `failure` | accepted | On [A,M], the free-scale table allows both factors of the power to vary and retains only integrable faces above unit upper shape. |
| genbeta | shape2 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape2 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | shape2 | both | `criterion` | accepted | On [m,M], the free-scale table allows both factors of the power to vary and retains only integrable faces above unit upper shape. |
| genbeta | shape2 | both | `failure` | accepted | On [m,M], the free-scale table allows both factors of the power to vary and retains only integrable faces above unit upper shape. |
| genbeta | shape2 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape2 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape2+shape3 | lower | `criterion` | accepted | On [m,B], fixed power deletes the small- and large-power faces and the unit-shape seam uses the scalar product test. |
| genbeta | shape2+shape3 | lower | `failure` | accepted | On [m,B], fixed power deletes the small- and large-power faces and the unit-shape seam uses the scalar product test. |
| genbeta | shape2+shape3 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape2+shape3 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape2+shape3 | upper | `criterion` | accepted | On [A,M], fixed power deletes the small- and large-power faces and the unit-shape seam uses the scalar product test. |
| genbeta | shape2+shape3 | upper | `failure` | accepted | On [A,M], fixed power deletes the small- and large-power faces and the unit-shape seam uses the scalar product test. |
| genbeta | shape2+shape3 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape2+shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | shape2+shape3 | both | `criterion` | accepted | On [m,M], fixed power deletes the small- and large-power faces and the unit-shape seam uses the scalar product test. |
| genbeta | shape2+shape3 | both | `failure` | accepted | On [m,M], fixed power deletes the small- and large-power faces and the unit-shape seam uses the scalar product test. |
| genbeta | shape2+shape3 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape2+shape3 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape3 | lower | `criterion` | accepted | On [m,B], the held power leaves the upper-shape spike available at free scale. |
| genbeta | shape3 | lower | `failure` | accepted | On [m,B], the held power leaves the upper-shape spike available at free scale. |
| genbeta | shape3 | lower | `uniqueness` | accepted | On [m,B], the held power leaves the upper-shape spike available at free scale, leaving an empty finite-positive maximizing set. |
| genbeta | shape3 | lower | `case:null-tight-slice` | regraded | On [m,B] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape3 | lower | `case:bound-cardinality` | regraded | On [m,B] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| genbeta | shape3 | upper | `criterion` | accepted | On [A,M], the held power leaves the upper-shape spike available at free scale. |
| genbeta | shape3 | upper | `failure` | accepted | On [A,M], the held power leaves the upper-shape spike available at free scale. |
| genbeta | shape3 | upper | `uniqueness` | accepted | On [A,M], the held power leaves the upper-shape spike available at free scale, leaving an empty finite-positive maximizing set. |
| genbeta | shape3 | upper | `case:null-tight-slice` | regraded | On [A,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape3 | upper | `case:bound-cardinality` | accepted | On [A,M], positive density before the closed upper support end makes upward loosening strict unless scale=M. |
| genbeta | shape3 | both | `criterion` | accepted | On [m,M], the held power leaves the upper-shape spike available at free scale. |
| genbeta | shape3 | both | `failure` | accepted | On [m,M], the held power leaves the upper-shape spike available at free scale. |
| genbeta | shape3 | both | `uniqueness` | accepted | On [m,M], the held power leaves the upper-shape spike available at free scale, leaving an empty finite-positive maximizing set. |
| genbeta | shape3 | both | `case:null-tight-slice` | regraded | On [m,M] the expansion test is valid at compatible positive observations, and the open lower parent end needs an explicit exclusion. |
| genbeta | shape3 | both | `case:bound-cardinality` | regraded | On [m,M] genbeta's open lower support end excludes the proposed lower-contact plateau. |
| gev | all free | lower | `case:null-tight-slice` | accepted | On [m,B], endpoint contact uses the inside density, which is finite positive only at the upper end for xi=-1 on the fitted branch, and the fixed-side geometry determines whether an expansion reaches the interior. |
| gev | all free | lower | `case:bound-cardinality` | accepted | On [m,B], strict positivity in the support interior makes equal-mass loosenings exactly those outside the fitted support. |
| gev | all free | lower | `case:minimum-tie-collapse` | accepted | On [m,B], the retained mass has a positive limit on the stated branch and xi=1 gives a scale exponent n-2k at k minimum ties. |
| gev | all free | lower | `case:reverse-exponential` | accepted | On [m,B], the xi=-1 slice optimizes its effective upper endpoint at M, with a strictly monotone rate mean and a full endpoint plateau when the tight upper cut equals M. |
| gev | all free | lower | `case:upper-only-exponential` | regraded | On [m,B] the source CDF ratio proves the tilt for admissible shapes and bounds while the open global face comparison stays outside this named case. |
| gev | all free | upper | `case:null-tight-slice` | accepted | On [A,M], endpoint contact uses the inside density, which is finite positive only at the upper end for xi=-1 on the fitted branch, and the fixed-side geometry determines whether an expansion reaches the interior. |
| gev | all free | upper | `case:bound-cardinality` | accepted | On [A,M], strict positivity in the support interior makes equal-mass loosenings exactly those outside the fitted support. |
| gev | all free | upper | `case:minimum-tie-collapse` | accepted | On [A,M], the retained mass has a positive limit on the stated branch and xi=1 gives a scale exponent n-2k at k minimum ties. |
| gev | all free | upper | `case:reverse-exponential` | accepted | On [A,M], the xi=-1 slice optimizes its effective upper endpoint at M, with a strictly monotone rate mean and a full endpoint plateau when the tight upper cut equals M. |
| gev | all free | upper | `case:upper-only-exponential` | regraded | On [A,M] the source CDF ratio proves the tilt for admissible shapes and bounds while the open global face comparison stays outside this named case. |
| gev | all free | both | `case:null-tight-slice` | accepted | On [m,M], endpoint contact uses the inside density, which is finite positive only at the upper end for xi=-1 on the fitted branch, and the fixed-side geometry determines whether an expansion reaches the interior. |
| gev | all free | both | `case:bound-cardinality` | accepted | On [m,M], strict positivity in the support interior makes equal-mass loosenings exactly those outside the fitted support. |
| gev | all free | both | `case:minimum-tie-collapse` | accepted | On [m,M], the retained mass has a positive limit on the stated branch and xi=1 gives a scale exponent n-2k at k minimum ties. |
| gev | all free | both | `case:reverse-exponential` | accepted | On [m,M], the xi=-1 slice optimizes its effective upper endpoint at M, with a strictly monotone rate mean and a full endpoint plateau when the tight upper cut equals M. |
| gev | all free | both | `case:upper-only-exponential` | regraded | On [m,M] the source CDF ratio proves the tilt for admissible shapes and bounds while the open global face comparison stays outside this named case. |
| gev | mu | lower | `case:null-tight-slice` | accepted | On [m,B], endpoint contact uses the inside density, which is finite positive only at the upper end for xi=-1 on the fitted branch, and the fixed-side geometry determines whether an expansion reaches the interior. |
| gev | mu | lower | `case:bound-cardinality` | accepted | On [m,B], strict positivity in the support interior makes equal-mass loosenings exactly those outside the fitted support. |
| gev | mu | lower | `case:minimum-tie-collapse` | accepted | On [m,B], the retained mass has a positive limit on the stated branch and xi=1 gives a scale exponent n-2k at k minimum ties. |
| gev | mu | upper | `case:null-tight-slice` | accepted | On [A,M], endpoint contact uses the inside density, which is finite positive only at the upper end for xi=-1 on the fitted branch, and the fixed-side geometry determines whether an expansion reaches the interior. |
| gev | mu | upper | `case:bound-cardinality` | accepted | On [A,M], strict positivity in the support interior makes equal-mass loosenings exactly those outside the fitted support. |
| gev | mu | upper | `case:minimum-tie-collapse` | accepted | On [A,M], the retained mass has a positive limit on the stated branch and xi=1 gives a scale exponent n-2k at k minimum ties. |
| gev | mu | both | `case:null-tight-slice` | accepted | On [m,M], endpoint contact uses the inside density, which is finite positive only at the upper end for xi=-1 on the fitted branch, and the fixed-side geometry determines whether an expansion reaches the interior. |
| gev | mu | both | `case:bound-cardinality` | accepted | On [m,M], strict positivity in the support interior makes equal-mass loosenings exactly those outside the fitted support. |
| gev | mu | both | `case:minimum-tie-collapse` | accepted | On [m,M], the retained mass has a positive limit on the stated branch and xi=1 gives a scale exponent n-2k at k minimum ties. |
| gev | xi | lower | `case:null-tight-slice` | regraded | On [m,B] the moving-support expansion test applies only after rejecting inadmissible held shapes. |
| gev | xi | lower | `case:bound-cardinality` | accepted | On [m,B], strict positivity in the support interior makes equal-mass loosenings exactly those outside the fitted support. |
| gev | xi | lower | `case:reverse-exponential` | accepted | On [m,B], the xi=-1 slice optimizes its effective upper endpoint at M, with a strictly monotone rate mean and a full endpoint plateau when the tight upper cut equals M. |
| gev | xi | lower | `case:upper-only-exponential` | regraded | On [m,B] the source CDF ratio proves the tilt for admissible shapes and bounds while the open global face comparison stays outside this named case. |
| gev | xi | upper | `case:null-tight-slice` | regraded | On [A,M] the moving-support expansion test applies only after rejecting inadmissible held shapes. |
| gev | xi | upper | `case:bound-cardinality` | accepted | On [A,M], strict positivity in the support interior makes equal-mass loosenings exactly those outside the fitted support. |
| gev | xi | upper | `case:reverse-exponential` | accepted | On [A,M], the xi=-1 slice optimizes its effective upper endpoint at M, with a strictly monotone rate mean and a full endpoint plateau when the tight upper cut equals M. |
| gev | xi | upper | `case:upper-only-exponential` | regraded | On [A,M] the source CDF ratio proves the tilt for admissible shapes and bounds while the open global face comparison stays outside this named case. |
| gev | xi | both | `case:null-tight-slice` | regraded | On [m,M] the moving-support expansion test applies only after rejecting inadmissible held shapes. |
| gev | xi | both | `case:bound-cardinality` | accepted | On [m,M], strict positivity in the support interior makes equal-mass loosenings exactly those outside the fitted support. |
| gev | xi | both | `case:reverse-exponential` | accepted | On [m,M], the xi=-1 slice optimizes its effective upper endpoint at M, with a strictly monotone rate mean and a full endpoint plateau when the tight upper cut equals M. |
| gev | xi | both | `case:upper-only-exponential` | regraded | On [m,M] the source CDF ratio proves the tilt for admissible shapes and bounds while the open global face comparison stays outside this named case. |

## Proof errors and scope corrections

### Feller–Pareto can lose to a nonzero log-excess tilt

For `min+shape1+shape3`, the estimated-window sections inherit the Feller–Pareto known-window pointer to the GB2 fixed-beta-shapes section. That older section lists only log-uniform. The intervening GB2 review corrected the GB2 row, but the corresponding Feller–Pareto row still routes readers through the older list. Finite positive power with escaping scale makes the slope nonzero. Translation by the held threshold preserves this branch, and the resolved cut determines which slopes are integrable. All real slopes are integrable on bounded log windows, only negative slopes on a right ray and only positive slopes on a left ray. No pure tilt is integrable on the whole log line. For any finite positive candidate tying the maximum excluded height, equality suffices. If a still higher feasible value exists, an escaping sequence cannot reach that higher supremum. [Fixed-threshold source](2026-11-27-trunc-gb2-math-report.md#fpareto-minshape1shape3-truncateknown) · [GB2 correction and exhaustion](2026-11-27-mod-rev10-report.md#finite-power-with-escaping-scale)

Take `min=0`, `shape1=shape3=1` and sample `(1,1,2)`, with fixed upper cut 2 or fixed lower cut 1 where applicable. Every design then profiles to `[1,2]`. In the log coordinate on `[0,log 2]`, the log density of every finite parent is strictly concave. Its chord through the sample endpoints exponentiates to a kernel with the same endpoint densities and strictly smaller integral. Normalizing strictly increases the sample product. The best pure tilt has negative slope because the sample log mean is one third of the interval, strictly below its midpoint and above its lower endpoint. Taking scale down to zero at positive power equal to the magnitude of that slope reaches the tilt, with the same positive Jacobian contribution at the observations. This proves nonattainment and rules out log-uniform as the winning excluded height. The six affected criterion and failure rows now name the corrected faces. [Exact sign replay](2026-11-28-mod-rev13a-data/validate.py)

### Null windows and density at the occupied end

The genbeta sections allow a lower-cut plateau at `d=m` with `d=0`. The declared parent support is `(0,scale]`, so a sample at zero is incompatible. A closed nominal cut at zero leaves that support open. Every attained fit with an estimated lower cut has strict lower tightening up to its positive minimum. At the closed upper endpoint the inside density is singular, positive finite or zero according as the upper shape is below, equal to or above one. For example, with all parent coefficients held at one, sample `(1)` and fixed upper cut 2, the nominal tight window `[1,2]` has mass zero. Changing its lower cut to `1-epsilon` gives likelihood `1/epsilon`. With the upper shape changed to two, the same expansion has mass `epsilon^2` and leaves the occupied density zero. [Generalized-beta support](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md) · [Inside-density calculation](2026-11-27-trunc-gb2-math-report.md#moving-support-and-endpoint-conventions)

Feller–Pareto has the opposite geometry. Its closed lower threshold is compatible at the minimum precisely on the appropriate inside-density branch, and positive mass extends indefinitely to the right. With threshold 0, scale 1 and `(shape1,shape2,shape3)=(2,1,1)`, sample `(0)` and fixed upper cut 0, every lower expansion retains zero mass. With both cuts estimated, expanding the upper side to `epsilon` retains mass `1-(1+epsilon)^(-2)` at the same parent vector. The threshold density is 2 and the height diverges as the upper cut descends to zero. A positive finite threshold contact on a nonnull window allows every outward nominal lower cut. Outside that contact, each such loosening adds positive mass. [Feller–Pareto support](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md) · [Joint profiling](2026-11-28-trunc-est4-math-report.md#joint-profiling-lemma)

The exact contact replays also reproduce the triangular and uniform GPD errors from checking nominal width alone. The `[1,2]` tight event retains only the endpoint in both examples. On `[1-epsilon,2]` the triangular height is `2/(2*epsilon-epsilon^2)` and the GPD height is `1/epsilon`. With GPD sample `(0)` instead, every lower cut at or below its threshold gives height one under fixed upper cut 1. These direction-specific cases explain why coincidence of the cuts, repeated extreme observations and a fitted support end all enter the joint argument. For dgamma the closed event retains the integer cell at an equal cut, and each added eligible cell has strictly positive probability for every parent vector. The cut intervals in the CSV describe the mathematical likelihood maximizers. The API selects the sample-extreme representative. [Contact examples](2026-11-28-trunc-est4-math-report.md#corrections-and-scope) · [Truncation conventions](../../design/modifiers.md)

### GEV held domain and endpoint trace

The free-shape GEV rows use the fitted branch `[-1,1]`. In the `xi` hold mask both nuisance coefficients are free. The dossier excludes held `xi<-1` in that mask, so the reference-family singularity at the upper endpoint cannot be used to reject a fit there. At `xi=-1` the closed upper endpoint has finite positive density, and the conditional reverse-exponential rate and upper-support endpoint must be optimized together. The unique rate on attainment follows from a strictly positive variance. The sample `(1/2,1)` with tight `[0,1]` has mean `3/4`. Both-estimated sample `(0,1,1)` has mean `2/3` on the same interval. Each mean lies strictly between `1/2` and 1, so with `xi=-1` held all endpoints at or above 1 tie at the maximizing rate. If the fixed upper cut exceeds 1, only endpoint 1 works. For sample constant at an endpoint the null test and the shrinking-width or increasing-rate path settle the absence of a positive finite fit on this slice. These claims apply to the specified submodel. The named remote exponential path proves reachability where integrable, and the strict-majority path proves divergence along its stated sequence. Neither supplies a global GEV comparison. [GEV held domain](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md#details) · [Exact submodel](2026-11-28-trunc-est4-math-report.md#explicit-moving-endpoint-submodels)

### Grouped dgamma and exact refutations

On tight `[2,3]`, sample `(2,2,3)` has empirical odds `1/2` and global Bernoulli upper bound `4/27`. At shape 2 and rate zero the grouped odds are `7/5`. At positive rate the odds decrease strictly to zero, and at rate zero they increase strictly with shape by ordering the cell means of log latent value. Thus every shape at least 2 has a unique matching positive rate and all such pairs attain the global upper bound. With another retained cell the strict positivity of its probability makes that height unattainable, while a gamma concentrated at the shared boundary reaches the empirical Bernoulli asymptotically. For separated occupied cells the strictly concave latent exponent or a steep rate forces loss at one separated cell, so only the three finite integrable grouped faces can compete. A stationary comparison at equality therefore gives attainment. This includes samples occupying three or more cells, even when some occupied pairs are adjacent. [Grouped-gamma proof](2026-11-27-trunc-misc-math-report.md#discretized-gamma) · [Occupancy correction](2026-11-27-mod-rev9-report.md#discretized-gamma-occupancy-and-count-rays)

The two other nonuniqueness witnesses are analytic. For Feller–Pareto at the three-shape hold in the CSV, sample `(1,2)` gives conditional product `4s/(1+s)^3` on tight `[1,Inf]`. Its gap from `16/27` has numerator `(2s-1)^2(4s+16)` over `27(1+s)^3`. Both `(min,scale)=(3/4,1/4)` and `(1,1/2)` attain. For genbeta at `scale=2, shape2=1`, the sample `(1,2,2)` on tight `[1,2]` has a strictly interior empirical log mean. Strict concavity in the positive product gives a unique maximizing product, with every positive factorization attaining. Both proofs use finite positive density at the included support endpoint and are valid for the joint problem by tightening at each parent vector. [Source examples](2026-11-27-trunc-gb2-math-report.md#moving-support-and-endpoint-conventions) · [Exact replays](2026-11-28-mod-rev13a-data/validate.py)

## Rows not joined

No keys were omitted from the join file. All four families are eligible, and every proposed hold exists in the base atlas. The all-free GEV and dgamma fields use the proposed empty spelling, which the atlas normalizes to `all free`. The source contains no proposed estimated-bound GEV `sigma` rows. The proposals also carry no general GEV attainment, failure or uniqueness rows. The report below records the corresponding research questions.

The [modifier design](../../design/modifiers.md#availability-and-refusals) makes the four families eligible for mathematical analysis. Production routes for these fits are reserved. Unlike zeta and the positive NB size-zero alias, none of these rows uses a parent domain that changes with the resolved window. The Pareto scale endpoint and the NB size-zero positive-window exception in the earlier work illustrate why the domain check belongs before the joint profiling lemma. No zero modifier or added `p0` or `pi` enters these likelihoods. [Prior endpoint and alias proof](2026-11-27-trunc-est-math-report.md#pareto-endpoint-and-count-exceptions)

## Open by family

| Family | What remains open |
| --- | --- |
| dgamma | Bound or characterize winning parent roots on windows with at least three cells outside the settled occupancy cases, and develop a finite comparison against the grouped faces including exact ties. |
| fpareto | Exhaust divergent weak limits and escaping-mass paths at joint threshold contact, and determine all winning parent components for the general shifted GB2 comparisons, including ties with the critical lower-tight atom envelope. |
| genbeta | Exhaust divergent laws near the moving upper endpoint with free shapes, and isolate or characterize all winners of the held-scale and moving-scale comparisons beyond the unit-shape product and strict-concavity cases. |
| gev | Compare the critical minimum multiplicity height and all remote and mixed-support faces with finite fits for each mask, including held scale, and isolate the full parent winners outside the held-`xi=-1` slice. |

## Checks

The workstation ran the [review validator and exact replays](2026-11-28-mod-rev13a-data/validate.py), the atlas modifier-proof checker and the TASK-475 numerical replay through the single-slot lock. The TASK-475 output reproduced the committed JSON byte for byte. The 80-digit grids corroborate the formulas. The grades for the analytic claims use the proofs and exact comparisons.

The repository Markdown link check has the same failures as its baseline before the review. It reports 47 inherited broken destinations and 19 errors outside this review. Eighteen of the 19 come from TASK-475’s source fragments under `2026-11-28-trunc-est4-math-data/`, whose relative paths resolve correctly in the assembled report but fail when interpreted from the fragment directory. The remaining error is the known beta-prime false positive on line 461. The validator separately checks every evidence fragment in the join file.
