# Review of fixed truncation for threshold and shape families

TASK-465 reviewed all 333 TASK-462 proposals. The review file contains 205
written rows and 128 open rows. The verdicts are 293 accepted,
40 regraded and 0 rejected. The written directions are 65
`iff`, 83 `exact` and 57 `one-sided`. All rows carry origin
`TASK-462`. [Reviewed rows](2026-11-27-mod-rev9-data/reviewed.csv) · [Proposals](2026-11-27-trunc-misc-math-data/proofs.csv) · [Source proofs](2026-11-27-trunc-misc-math-report.md)

**The all-free GPD failure list misses a divergent joint path with mass
escaping to infinity.** The fixed-shape witness still proves nonattainment.
The Pareto and nonstandard-beta contact witnesses also establish divergence
without exhausting joint paths. The discretized-gamma adjacency language
needed an exact restriction on the number of occupied cells.
[Proof errors and scope corrections](#proof-errors-and-scope-corrections)

All 97 family and hold pairs occur in the base atlas and are eligible for
`truncate:known`. The review uses nonempty finite samples, valid holds and a
closed window intersected with the support. Finite continuous cut
observations use the declared endpoint density. Count windows keep the
integers from the ceiling of the lower cut through the floor of the upper
cut, intersected with the count support. Infinite cuts are unbounded. NA
cuts use different modifier keys. The zero routes, including held or free
`p0` and `pi`, use different likelihoods and do not enter these rows.
Positive finite fits and infinite reference endpoint densities are treated
separately. [Atlas conventions](../existence-atlas/README.md) · [Base overview](../existence-atlas/overview.md) · [Base proofs](../existence-atlas/proofs.csv) · [Modifier design](../../design/modifiers.md)

An open row counts as accepted when its open grade is warranted. Its frontier
may be edited to reflect the actual held coordinates or a proved subcase.
Regraded written rows carry a changed proof grade or claim. The finite
boundary comparisons use the joint sample likelihood and include equality
with an attainable value. They give attainment criteria even where a
certified root count or practical decision procedure remains open.

## Row verdicts

| Family | Held | Question | Verdict | Reason |
| --- | --- | --- | --- | --- |
| dgamma | all free | criterion | regraded | The adjacent-cell branch needs exactly two occupied cells, while every sample spanning at least two count units falls under the separated-pair bound. |
| dgamma | all free | failure | regraded | A constant selects its occupied cell atom, an exactly adjacent pair selects its empirical Bernoulli when excluded, and a separated pair forces all other joint ends to lose. |
| dgamma | all free | uniqueness | accepted | The odds argument settles windows of at most two cells, while general cardinality on larger windows remains open. |
| dgamma | all free | case:adjacent-cells | regraded | The limiting normal split gives the exact Bernoulli bound and the strict rate-odds monotonicity gives a continuum of finite maximizers on two retained cells. |
| dgamma | rate | criterion | accepted | At fixed rate, the two scalar shape ends exhaust escapes and a finite equality with a positive end height suffices. |
| dgamma | rate | failure | regraded | The large-shape atom is present only with a finite last cell, while small shape gives an atom only when zero is retained. |
| dgamma | rate | uniqueness | accepted | The odds argument settles windows of at most two cells, while general cardinality on larger windows remains open. |
| dgamma | rate+shape | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| dgamma | rate+shape | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| dgamma | rate+shape | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| dgamma | shape | criterion | accepted | At fixed shape, the rate ends are exhaustion bounds and a finite equality with the competing height suffices. |
| dgamma | shape | failure | accepted | At fixed shape the only positive excluded heights come from a finite grouped power and a rate-infinity first-cell atom. |
| dgamma | shape | uniqueness | accepted | The odds argument settles windows of at most two cells, while general cardinality on larger windows remains open. |
| expweibull | all free | criterion | accepted | Exhaust joint shape, scale and exponent escapes and handle zero observations. |
| expweibull | all free | failure | accepted | Exhaust joint shape, scale and exponent escapes and handle zero observations. |
| expweibull | all free | uniqueness | accepted | Exhaust joint shape, scale and exponent escapes and handle zero observations. |
| expweibull | all free | case:constant-collapse | accepted | Matching the scale to the observation makes the density grow linearly with shape and conditioning can only increase that density. |
| expweibull | exponent | criterion | accepted | Exhaust joint shape and scale with exponent held escapes and handle zero observations. |
| expweibull | exponent | failure | accepted | Exhaust joint shape and scale with exponent held escapes and handle zero observations. |
| expweibull | exponent | uniqueness | accepted | Exhaust joint shape and scale with exponent held escapes and handle zero observations. |
| expweibull | exponent+scale | criterion | accepted | The two shape ends exhaust escapes, with zero height or log-uniform at small shape and a support-aware concentration test at large shape. |
| expweibull | exponent+scale | failure | accepted | The large-shape estimates include both cuts outside the scale and the small-shape log-uniform is integrable only on a bounded window away from zero. |
| expweibull | exponent+scale | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| expweibull | exponent+shape | criterion | accepted | The two scale ends are exhaustive and finite equality with their larger finite height permits attainment. |
| expweibull | exponent+shape | failure | accepted | The broad-scale power is integrable at zero and small scale can win only as a constant at a strictly positive lower cut. |
| expweibull | exponent+shape | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| expweibull | scale | criterion | accepted | Exhaust joint shape and exponent with scale held escapes and handle zero observations. |
| expweibull | scale | failure | accepted | Exhaust joint shape and exponent with scale held escapes and handle zero observations. |
| expweibull | scale | uniqueness | accepted | Exhaust joint shape and exponent with scale held escapes and handle zero observations. |
| expweibull | scale+shape | criterion | accepted | The log CDF coordinate yields a positive exponential tilt with strictly increasing mean. |
| expweibull | scale+shape | failure | accepted | The finite zero-exponent carrier and the upper-cut constant exhaust the transformed exponential failure modes. |
| expweibull | scale+shape | uniqueness | accepted | The log CDF statistic varies on every positive-width retained interval, so the scalar log likelihood is strictly concave. |
| expweibull | shape | criterion | accepted | Exhaust joint scale and exponent with shape held escapes and handle zero observations. |
| expweibull | shape | failure | accepted | Exhaust joint scale and exponent with shape held escapes and handle zero observations. |
| expweibull | shape | uniqueness | accepted | Exhaust joint scale and exponent with shape held escapes and handle zero observations. |
| fatigue | all free | criterion | accepted | Exhaust joint threshold, shape and scale limits, including vanishing excess and remote thresholds. |
| fatigue | all free | failure | accepted | Exhaust joint threshold, shape and scale limits, including vanishing excess and remote thresholds. |
| fatigue | all free | uniqueness | accepted | Exhaust joint threshold, shape and scale limits, including vanishing excess and remote thresholds. |
| fatigue | all free | case:constant-collapse | accepted | One can place the normal-coordinate zero at the constant observation and shrink the free shape. |
| fatigue | alpha | criterion | accepted | Bound moving thresholds and varying scale at held shape, including contact with the sample minimum and cut concentration. |
| fatigue | alpha | failure | accepted | Bound moving thresholds and varying scale at held shape, including contact with the sample minimum and cut concentration. |
| fatigue | alpha | uniqueness | accepted | Bound moving thresholds and varying scale at held shape, including contact with the sample minimum and cut concentration. |
| fatigue | alpha | case:constant-collapse | accepted | At held shape, moving the threshold to the observation with excess equal to the vanishing scale gives density proportional to inverse scale. |
| fatigue | alpha+beta | criterion | accepted | The remote location and contact bounds exhaust the scalar ends, with the upper-cut constant handled separately and finite equality allowed. |
| fatigue | alpha+beta | failure | accepted | The remote exponential is integrable only with a finite lower cut, and the remaining competitive end is upper-cut shrinking. |
| fatigue | alpha+beta | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| fatigue | alpha+mu | criterion | accepted | Both scale ends have zero height except at the accessible constant cut, so compactness gives an attained winner elsewhere. |
| fatigue | alpha+mu | failure | accepted | The small- and large-scale tail estimates exhaust cut concentration and suppress any observation separated from the favored cut. |
| fatigue | alpha+mu | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| fatigue | beta | criterion | accepted | Bound simultaneous threshold contact and shape change at held scale, including critical ties and remote threshold limits. |
| fatigue | beta | failure | accepted | Bound simultaneous threshold contact and shape change at held scale, including critical ties and remote threshold limits. |
| fatigue | beta | uniqueness | accepted | Bound simultaneous threshold contact and shape change at held scale, including critical ties and remote threshold limits. |
| fatigue | beta+mu | criterion | accepted | In the normal coordinate, the squared statistic is nonconstant and its decreasing inverse-shape mean spans exactly the two stated bounds. |
| fatigue | beta+mu | failure | accepted | The only competitive inverse-shape ends are the integrable uniform in the transformed coordinate and the projected squared-statistic minimum. |
| fatigue | beta+mu | uniqueness | accepted | The squared normal coordinate has positive variance on a positive-width retained interval. |
| fatigue | mu | criterion | accepted | Exhaust joint shape-scale escapes at the held threshold and bound their conditional heights under vanishing retention. |
| fatigue | mu | failure | accepted | Exhaust joint shape-scale escapes at the held threshold and bound their conditional heights under vanishing retention. |
| fatigue | mu | uniqueness | accepted | Exhaust joint shape-scale escapes at the held threshold and bound their conditional heights under vanishing retention. |
| fatigue | mu | case:constant-collapse | accepted | One can place the normal-coordinate zero at the constant observation and shrink the free shape. |
| foldnorm | all free | criterion | accepted | The folded-to-normal ratio bounds all concentrating and losing paths and the equivalent cosh kernel gives the remaining finite-height branch. |
| foldnorm | all free | failure | accepted | Subsequences in scale and in location divided by variance exhaust concentration, bounded-window cosh and zero-height escapes. |
| foldnorm | all free | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| foldnorm | mu | criterion | accepted | Holding location leaves only projected small-scale collapse and the broad uniform or zero height, with finite equality allowed. |
| foldnorm | mu | failure | accepted | The Gaussian pivot controls the small-scale trace and the held location forces the broad cosh parameter to zero. |
| foldnorm | mu | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| foldnorm | mu | case:half-normal | accepted | At zero location the inverse-variance tilt statistic is nonconstant throughout the retained interval. |
| foldnorm | mu+sigma | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| foldnorm | mu+sigma | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| foldnorm | mu+sigma | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| foldnorm | sigma | criterion | accepted | With scale held, only remote location can escape, and it loses unless every observation is at the finite upper cut. |
| foldnorm | sigma | failure | accepted | The large-location Gaussian pivot concentrates at the finite upper cut and fixed observations elsewhere lose. |
| foldnorm | sigma | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| gev | all free | criterion | accepted | Compare the critical minimum-tie height and integrable remote faces to finite fits, and exhaust mixed support, shape and scale paths. |
| gev | all free | failure | accepted | Compare the critical minimum-tie height and integrable remote faces to finite fits, and exhaust mixed support, shape and scale paths. |
| gev | all free | uniqueness | accepted | Compare the critical minimum-tie height and integrable remote faces to finite fits, and exhaust mixed support, shape and scale paths. |
| gev | all free | case:upper-only-exponential | regraded | The derivation covers remote fixed nonzero shapes in the stated range, with the zero-shape exponential supplied separately by Gumbel. |
| gev | all free | case:reverse-exponential | accepted | The endpoint removes only denominator mass until it reaches the sample maximum, and the scalar rate test gives the exact endpoint plateau within this submodel. |
| gev | all free | case:minimum-tie-collapse | accepted | The sample likelihood along the shape-one path has scale exponent n minus twice the minimum multiplicity, and retained mass is at most one. |
| gev | mu | criterion | accepted | At the held location, exhaust shape and scale escapes with moving support, including critical ties when location equals the minimum. |
| gev | mu | failure | accepted | At the held location, exhaust shape and scale escapes with moving support, including critical ties when location equals the minimum. |
| gev | mu | uniqueness | accepted | At the held location, exhaust shape and scale escapes with moving support, including critical ties when location equals the minimum. |
| gev | mu | case:minimum-tie-collapse | accepted | The sample likelihood along the shape-one path has scale exponent n minus twice the minimum multiplicity, and retained mass is at most one. |
| gev | sigma | criterion | accepted | At the held scale, control joint shape/location and endpoint limits, including remote reverse exponentials at shape −1. |
| gev | sigma | failure | accepted | At the held scale, control joint shape/location and endpoint limits, including remote reverse exponentials at shape −1. |
| gev | sigma | uniqueness | accepted | At the held scale, control joint shape/location and endpoint limits, including remote reverse exponentials at shape −1. |
| gev | xi | criterion | accepted | Compare remote tilt and support-contact heights to finite fits for each held shape, including critical minimum ties for positive shape. |
| gev | xi | failure | accepted | Compare remote tilt and support-contact heights to finite fits for each held shape, including critical minimum ties for positive shape. |
| gev | xi | uniqueness | accepted | Compare remote tilt and support-contact heights to finite fits for each held shape, including critical minimum ties for positive shape. |
| gev | xi | case:upper-only-exponential | regraded | The derivation covers remote fixed nonzero shapes in the stated range, with the zero-shape exponential supplied separately by Gumbel. |
| gev | xi | case:reverse-exponential | accepted | The endpoint removes only denominator mass until it reaches the sample maximum, and the scalar rate test gives the exact endpoint plateau within this submodel. |
| gpd | all free | criterion | accepted | Placing the threshold at the minimum and fixing sufficiently large positive shape gives divergence, with width collapse covering an upper-cut constant. |
| gpd | all free | failure | regraded | Fixed-shape collapse proves divergence but misses joint escapes where the conditional mass drifts to infinity. |
| gpd | all free | uniqueness | accepted | The explicit divergent path on every compatible sample makes the set of positive finite maximizers empty. |
| gpd | mu | criterion | accepted | Exhaust the joint scale-shape envelope at a separated lower cut and the negative-shape upper-support contacts. |
| gpd | mu | failure | accepted | Exhaust the joint scale-shape envelope at a separated lower cut and the negative-shape upper-support contacts. |
| gpd | mu | uniqueness | accepted | Exhaust the joint scale-shape envelope at a separated lower cut and the negative-shape upper-support contacts. |
| gpd | mu | case:threshold-contact | accepted | At a held occupied threshold, choosing a sufficiently large positive shape makes the scale exponent negative. |
| gpd | mu+sigma | criterion | regraded | All-threshold right-ray data give an attained plateau, and the remaining scalar shape test must include the uniform boundary and its possibly discontinuous contact. |
| gpd | mu+sigma | failure | regraded | The right-ray all-threshold trace ties an attained fit, while positive separated data on a bounded window access log-uniform and the remaining winning escapes diverge. |
| gpd | mu+sigma | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| gpd | mu+sigma+xi | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| gpd | mu+sigma+xi | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| gpd | mu+sigma+xi | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| gpd | mu+xi | criterion | regraded | Holding threshold and fitted shape leaves only the scale ends typed in G1, with width minimization at the uniform member. |
| gpd | mu+xi | failure | regraded | At zero shape the rate-zero uniform and rate-infinity lower-cut collapse are required, alongside the nonzero-shape support and scale ends. |
| gpd | mu+xi | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| gpd | sigma | criterion | accepted | Exhaust threshold-shape paths at the held scale, including cut contact and large positive shape coupled to a moving threshold. |
| gpd | sigma | failure | accepted | Exhaust threshold-shape paths at the held scale, including cut contact and large positive shape coupled to a moving threshold. |
| gpd | sigma | uniqueness | accepted | Exhaust threshold-shape paths at the held scale, including cut contact and large positive shape coupled to a moving threshold. |
| gpd | xi | criterion | accepted | For general held shape, compare finite fits with remote threshold and scale escapes, including negative-shape support contacts and positive-shape critical traces. |
| gpd | xi | failure | accepted | For general held shape, compare finite fits with remote threshold and scale escapes, including negative-shape support contacts and positive-shape critical traces. |
| gpd | xi | uniqueness | accepted | For general held shape, compare finite fits with remote threshold and scale escapes, including negative-shape support contacts and positive-shape critical traces. |
| gpd | xi | case:flat-uniform | regraded | The uniform support can match the sample extrema and wider raw supports tie whenever truncation leaves the same width. |
| naka | all free | criterion | accepted | The squared-data gamma quadrant has coercive nonconstant likelihood and its finite integrable faces are decided by inward scores, including equality. |
| naka | all free | failure | accepted | Gamma coercivity removes infinity for nonconstant data, leaving only the integrable coordinate edges and corner, while a constant has a centered collapse ray. |
| naka | all free | uniqueness | accepted | The free gamma statistics have positive variance in every allowed direction and the mapping back to Nakagami coefficients is injective. |
| naka | scale | criterion | accepted | At held scale the gamma ray tilts log squared excess minus squared excess divided by scale, a nonconstant statistic with a unique projected maximum. |
| naka | scale | failure | accepted | The zero-tilt carrier is integrable only on a bounded window away from zero and the opposite end concentrates at the unique projected statistic maximum. |
| naka | scale | uniqueness | accepted | The free gamma statistics have positive variance in every allowed direction and the mapping back to Nakagami coefficients is injective. |
| naka | scale+shape | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| naka | scale+shape | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| naka | scale+shape | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| naka | shape | criterion | accepted | The decreasing rate tilt in squared observations has strictly ordered endpoint means. |
| naka | shape | failure | accepted | The shape-held zero rate gives a power only on a bounded window, while infinite rate can win at a lower-cut constant. |
| naka | shape | uniqueness | accepted | The free gamma statistics have positive variance in every allowed direction and the mapping back to Nakagami coefficients is injective. |
| nsbeta | all free | criterion | accepted | A free endpoint and its associated shape can always approach the occupied contact with a fixed negative exponent or shrink around a pinned constant. |
| nsbeta | all free | failure | regraded | The proved contact paths suffice for divergence, but other joint paths such as interior concentration are available in these masks. |
| nsbeta | all free | uniqueness | accepted | The contact or shrinking construction makes the positive finite maximizing set empty on the stated compatible domain. |
| nsbeta | all free | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | max | criterion | accepted | A free endpoint and its associated shape can always approach the occupied contact with a fixed negative exponent or shrink around a pinned constant. |
| nsbeta | max | failure | regraded | The proved contact paths suffice for divergence, but other joint paths such as interior concentration are available in these masks. |
| nsbeta | max | uniqueness | accepted | The contact or shrinking construction makes the positive finite maximizing set empty on the stated compatible domain. |
| nsbeta | max | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | max+min | criterion | regraded | The transferred beta moment test requires finite sample log statistics, with an observed held-side endpoint allowed at its unit held shape. |
| nsbeta | max+min | failure | regraded | The available beta faces depend on how many shapes are free and the test requires the indicated sample carriers. |
| nsbeta | max+min | uniqueness | regraded | Strict concavity gives exactly one maximizer whenever attained on the beta moment-test domain. |
| nsbeta | max+min+shape1 | criterion | regraded | The transferred beta moment test requires finite sample log statistics, with an observed held-side endpoint allowed at its unit held shape. |
| nsbeta | max+min+shape1 | failure | regraded | The available beta faces depend on how many shapes are free and the test requires the indicated sample carriers. |
| nsbeta | max+min+shape1 | uniqueness | regraded | Strict concavity gives exactly one maximizer whenever attained on the beta moment-test domain. |
| nsbeta | max+min+shape1+shape2 | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| nsbeta | max+min+shape1+shape2 | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| nsbeta | max+min+shape1+shape2 | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| nsbeta | max+min+shape2 | criterion | regraded | The transferred beta moment test requires finite sample log statistics, with an observed held-side endpoint allowed at its unit held shape. |
| nsbeta | max+min+shape2 | failure | regraded | The available beta faces depend on how many shapes are free and the test requires the indicated sample carriers. |
| nsbeta | max+min+shape2 | uniqueness | regraded | Strict concavity gives exactly one maximizer whenever attained on the beta moment-test domain. |
| nsbeta | max+shape1 | criterion | accepted | For a nonsingular held moving-side shape, bound simultaneous movement of the free endpoint and variation in the shape at the fixed endpoint, including cut contact and concentration. |
| nsbeta | max+shape1 | failure | accepted | For a nonsingular held moving-side shape, bound simultaneous movement of the free endpoint and variation in the shape at the fixed endpoint, including cut contact and concentration. |
| nsbeta | max+shape1 | uniqueness | accepted | For a nonsingular held moving-side shape, bound simultaneous movement of the free endpoint and variation in the shape at the fixed endpoint, including cut contact and concentration. |
| nsbeta | max+shape1 | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | max+shape1+shape2 | criterion | regraded | With one endpoint free, its associated shape controls the contact spike and the fixed side contributes a single remote power with an integrability test. |
| nsbeta | max+shape1+shape2 | failure | regraded | The moving endpoint has one finite contact and one remote end, and the latter is the fixed-side power, including uniform at unit shape. |
| nsbeta | max+shape1+shape2 | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| nsbeta | max+shape1+shape2 | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | max+shape2 | criterion | accepted | A free endpoint and its associated shape can always approach the occupied contact with a fixed negative exponent or shrink around a pinned constant. |
| nsbeta | max+shape2 | failure | regraded | The proved contact paths suffice for divergence, but other joint paths such as interior concentration are available in these masks. |
| nsbeta | max+shape2 | uniqueness | accepted | The contact or shrinking construction makes the positive finite maximizing set empty on the stated compatible domain. |
| nsbeta | max+shape2 | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | min | criterion | accepted | A free endpoint and its associated shape can always approach the occupied contact with a fixed negative exponent or shrink around a pinned constant. |
| nsbeta | min | failure | regraded | The proved contact paths suffice for divergence, but other joint paths such as interior concentration are available in these masks. |
| nsbeta | min | uniqueness | accepted | The contact or shrinking construction makes the positive finite maximizing set empty on the stated compatible domain. |
| nsbeta | min | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | min+shape1 | criterion | accepted | A free endpoint and its associated shape can always approach the occupied contact with a fixed negative exponent or shrink around a pinned constant. |
| nsbeta | min+shape1 | failure | regraded | The proved contact paths suffice for divergence, but other joint paths such as interior concentration are available in these masks. |
| nsbeta | min+shape1 | uniqueness | accepted | The contact or shrinking construction makes the positive finite maximizing set empty on the stated compatible domain. |
| nsbeta | min+shape1 | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | min+shape1+shape2 | criterion | regraded | With one endpoint free, its associated shape controls the contact spike and the fixed side contributes a single remote power with an integrability test. |
| nsbeta | min+shape1+shape2 | failure | regraded | The moving endpoint has one finite contact and one remote end, and the latter is the fixed-side power, including uniform at unit shape. |
| nsbeta | min+shape1+shape2 | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| nsbeta | min+shape1+shape2 | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | min+shape2 | criterion | accepted | For a nonsingular held moving-side shape, bound simultaneous movement of the free endpoint and variation in the shape at the fixed endpoint, including cut contact and concentration. |
| nsbeta | min+shape2 | failure | accepted | For a nonsingular held moving-side shape, bound simultaneous movement of the free endpoint and variation in the shape at the fixed endpoint, including cut contact and concentration. |
| nsbeta | min+shape2 | uniqueness | accepted | For a nonsingular held moving-side shape, bound simultaneous movement of the free endpoint and variation in the shape at the fixed endpoint, including cut contact and concentration. |
| nsbeta | min+shape2 | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | shape1 | criterion | accepted | A free endpoint and its associated shape can always approach the occupied contact with a fixed negative exponent or shrink around a pinned constant. |
| nsbeta | shape1 | failure | regraded | The proved contact paths suffice for divergence, but other joint paths such as interior concentration are available in these masks. |
| nsbeta | shape1 | uniqueness | accepted | The contact or shrinking construction makes the positive finite maximizing set empty on the stated compatible domain. |
| nsbeta | shape1 | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | shape1+shape2 | criterion | accepted | For nonnegative held powers all finite positive-width contacts are included and the extended-line endpoint sequences give an exhaustive remote comparison. |
| nsbeta | shape1+shape2 | failure | regraded | The remote list is proved for both held powers nonnegative, whereas a negative associated power gives a divergence witness without an exhaustive list on that branch. |
| nsbeta | shape1+shape2 | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| nsbeta | shape1+shape2 | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| nsbeta | shape2 | criterion | accepted | A free endpoint and its associated shape can always approach the occupied contact with a fixed negative exponent or shrink around a pinned constant. |
| nsbeta | shape2 | failure | regraded | The proved contact paths suffice for divergence, but other joint paths such as interior concentration are available in these masks. |
| nsbeta | shape2 | uniqueness | accepted | The contact or shrinking construction makes the positive finite maximizing set empty on the stated compatible domain. |
| nsbeta | shape2 | case:endpoint-spike | accepted | At positive retained width the associated negative exponent gives a contact spike, and at a pinned constant the inverse-width calculation covers collapse. |
| pareto3 | all free | criterion | accepted | Every compatible sample permits the fixed lower-power spike or upper-cut shrink, so no positive finite value maximizes. |
| pareto3 | all free | failure | regraded | The contact paths prove unboundedness, but the joint free-coordinate ends are not classified by P1. |
| pareto3 | all free | uniqueness | accepted | The set of positive finite maximizers is empty by the unconditional divergence witness. |
| pareto3 | min | criterion | accepted | Exhaust shape-scale escapes at a fixed threshold, including zero retention at cut contacts and sample ties. |
| pareto3 | min | failure | accepted | Exhaust shape-scale escapes at a fixed threshold, including zero retention at cut contacts and sample ties. |
| pareto3 | min | uniqueness | accepted | Exhaust shape-scale escapes at a fixed threshold, including zero retention at cut contacts and sample ties. |
| pareto3 | min+scale | criterion | accepted | Exhaust the two scalar shape ends at held threshold and scale, including contacts with either cut. |
| pareto3 | min+scale | failure | accepted | Exhaust the two scalar shape ends at held threshold and scale, including contacts with either cut. |
| pareto3 | min+scale | uniqueness | accepted | Exhaust the two scalar shape ends at held threshold and scale, including contacts with either cut. |
| pareto3 | min+scale+shape | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| pareto3 | min+scale+shape | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| pareto3 | min+scale+shape | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| pareto3 | min+shape | criterion | accepted | The held threshold fixes the effective cuts and P3 covers both scale ends including the exact contact exponent and finite equality. |
| pareto3 | min+shape | failure | accepted | P3's small-scale power or contact likelihood and broad bounded power exhaust positive excluded heights at fixed shapes and threshold. |
| pareto3 | min+shape | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| pareto3 | scale | criterion | accepted | Every compatible sample permits the fixed lower-power spike or upper-cut shrink, so no positive finite value maximizes. |
| pareto3 | scale | failure | regraded | The contact paths prove unboundedness, but the joint free-coordinate ends are not classified by P1. |
| pareto3 | scale | uniqueness | accepted | The set of positive finite maximizers is empty by the unconditional divergence witness. |
| pareto3 | scale+shape | criterion | accepted | With scale and shapes held, the two threshold ends and all included contacts and joins are covered, including upper-cut shrinking. |
| pareto3 | scale+shape | failure | accepted | A fixed-scale threshold has only an occupied contact, a shrinking constant at the upper cut or a remote bounded uniform as competitive ends. |
| pareto3 | scale+shape | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| pareto3 | scale+shape | case:threshold-spike | accepted | A negative lower power exponent proves divergence at positive retained width, and a constant at the upper cut admits shrinking for any positive power. |
| pareto3 | shape | criterion | accepted | For held shape at least one, bound the joint threshold-scale envelope, including scale collapse at sample ties and support-cut contact. |
| pareto3 | shape | failure | accepted | For held shape at least one, bound the joint threshold-scale envelope, including scale collapse at sample ties and support-cut contact. |
| pareto3 | shape | uniqueness | accepted | For held shape at least one, bound the joint threshold-scale envelope, including scale collapse at sample ties and support-cut contact. |
| pareto3 | shape | case:threshold-spike | accepted | A negative lower power exponent proves divergence at positive retained width, and a constant at the upper cut admits shrinking for any positive power. |
| pareto4 | all free | criterion | accepted | Every compatible sample permits the fixed lower-power spike or upper-cut shrink, so no positive finite value maximizes. |
| pareto4 | all free | failure | regraded | The contact paths prove unboundedness, but the joint free-coordinate ends are not classified by P1. |
| pareto4 | all free | uniqueness | accepted | The set of positive finite maximizers is empty by the unconditional divergence witness. |
| pareto4 | min | criterion | accepted | Exhaust scale and both shape ends at the held threshold, including the joint Weibull path and tied observations. |
| pareto4 | min | failure | accepted | Exhaust scale and both shape ends at the held threshold, including the joint Weibull path and tied observations. |
| pareto4 | min | uniqueness | accepted | Exhaust scale and both shape ends at the held threshold, including the joint Weibull path and tied observations. |
| pareto4 | min+scale | criterion | accepted | Exhaust the joint shape ends at held threshold and scale, including cut contact and vanishing retained mass. |
| pareto4 | min+scale | failure | accepted | Exhaust the joint shape ends at held threshold and scale, including cut contact and vanishing retained mass. |
| pareto4 | min+scale | uniqueness | accepted | Exhaust the joint shape ends at held threshold and scale, including cut contact and vanishing retained mass. |
| pareto4 | min+scale+shape1 | criterion | accepted | Exhaust the two scalar lower-power ends, including the contact trace when the held scale meets a window cut. |
| pareto4 | min+scale+shape1 | failure | accepted | Exhaust the two scalar lower-power ends, including the contact trace when the held scale meets a window cut. |
| pareto4 | min+scale+shape1 | uniqueness | accepted | Exhaust the two scalar lower-power ends, including the contact trace when the held scale meets a window cut. |
| pareto4 | min+scale+shape1+shape2 | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| pareto4 | min+scale+shape1+shape2 | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| pareto4 | min+scale+shape1+shape2 | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| pareto4 | min+scale+shape2 | criterion | accepted | The log-survival transformation is fixed by the holds, so a strict scalar exponential mean interval decides attainment. |
| pareto4 | min+scale+shape2 | failure | accepted | In the transformed coordinate the only competitive rate limits are collapse at its lower cut or the integrable zero-rate uniform. |
| pareto4 | min+scale+shape2 | uniqueness | accepted | Positive log-survival coordinate variance makes the scalar rate log likelihood strictly concave. |
| pareto4 | min+shape1 | criterion | accepted | Exhaust scale and lower-power escapes with threshold and tail shape held. |
| pareto4 | min+shape1 | failure | accepted | Exhaust scale and lower-power escapes with threshold and tail shape held. |
| pareto4 | min+shape1 | uniqueness | accepted | Exhaust scale and lower-power escapes with threshold and tail shape held. |
| pareto4 | min+shape1+shape2 | criterion | accepted | The held threshold fixes the effective cuts and P3 covers both scale ends including the exact contact exponent and finite equality. |
| pareto4 | min+shape1+shape2 | failure | accepted | P3's small-scale power or contact likelihood and broad bounded power exhaust positive excluded heights at fixed shapes and threshold. |
| pareto4 | min+shape1+shape2 | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| pareto4 | min+shape2 | criterion | accepted | Exhaust scale and tail-shape escapes with threshold and lower power held, including the Weibull path when shape1 divided by scale to the lower power has a finite limit. |
| pareto4 | min+shape2 | failure | accepted | Exhaust scale and tail-shape escapes with threshold and lower power held, including the Weibull path when shape1 divided by scale to the lower power has a finite limit. |
| pareto4 | min+shape2 | uniqueness | accepted | Exhaust scale and tail-shape escapes with threshold and lower power held, including the Weibull path when shape1 divided by scale to the lower power has a finite limit. |
| pareto4 | scale | criterion | accepted | Every compatible sample permits the fixed lower-power spike or upper-cut shrink, so no positive finite value maximizes. |
| pareto4 | scale | failure | regraded | The contact paths prove unboundedness, but the joint free-coordinate ends are not classified by P1. |
| pareto4 | scale | uniqueness | accepted | The set of positive finite maximizers is empty by the unconditional divergence witness. |
| pareto4 | scale+shape1 | criterion | accepted | Every compatible sample permits the fixed lower-power spike or upper-cut shrink, so no positive finite value maximizes. |
| pareto4 | scale+shape1 | failure | regraded | The contact paths prove unboundedness, but the joint free-coordinate ends are not classified by P1. |
| pareto4 | scale+shape1 | uniqueness | accepted | The set of positive finite maximizers is empty by the unconditional divergence witness. |
| pareto4 | scale+shape1+shape2 | criterion | accepted | With scale and shapes held, the two threshold ends and all included contacts and joins are covered, including upper-cut shrinking. |
| pareto4 | scale+shape1+shape2 | failure | accepted | A fixed-scale threshold has only an occupied contact, a shrinking constant at the upper cut or a remote bounded uniform as competitive ends. |
| pareto4 | scale+shape1+shape2 | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| pareto4 | scale+shape1+shape2 | case:threshold-spike | accepted | A negative lower power exponent proves divergence at positive retained width, and a constant at the upper cut admits shrinking for any positive power. |
| pareto4 | scale+shape2 | criterion | accepted | For held lower power at least one, analyze the joint threshold and tail-shape envelope with scale fixed. |
| pareto4 | scale+shape2 | failure | accepted | For held lower power at least one, analyze the joint threshold and tail-shape envelope with scale fixed. |
| pareto4 | scale+shape2 | uniqueness | accepted | For held lower power at least one, analyze the joint threshold and tail-shape envelope with scale fixed. |
| pareto4 | scale+shape2 | case:threshold-spike | accepted | A negative lower power exponent proves divergence at positive retained width, and a constant at the upper cut admits shrinking for any positive power. |
| pareto4 | shape1 | criterion | accepted | Every compatible sample permits the fixed lower-power spike or upper-cut shrink, so no positive finite value maximizes. |
| pareto4 | shape1 | failure | regraded | The contact paths prove unboundedness, but the joint free-coordinate ends are not classified by P1. |
| pareto4 | shape1 | uniqueness | accepted | The set of positive finite maximizers is empty by the unconditional divergence witness. |
| pareto4 | shape1+shape2 | criterion | accepted | For held lower power at least one, exhaust joint threshold-scale traces, including finite critical heights at sample ties. |
| pareto4 | shape1+shape2 | failure | accepted | For held lower power at least one, exhaust joint threshold-scale traces, including finite critical heights at sample ties. |
| pareto4 | shape1+shape2 | uniqueness | accepted | For held lower power at least one, exhaust joint threshold-scale traces, including finite critical heights at sample ties. |
| pareto4 | shape1+shape2 | case:threshold-spike | accepted | A negative lower power exponent proves divergence at positive retained width, and a constant at the upper cut admits shrinking for any positive power. |
| pareto4 | shape2 | criterion | accepted | Check the prior untruncated divergence proof for promotion to this modifier mask under closed-window normalization, and classify other joint escapes if an exhaustive failure row is sought. |
| pareto4 | shape2 | failure | accepted | Check the prior untruncated divergence proof for promotion to this modifier mask under closed-window normalization, and classify other joint escapes if an exhaustive failure row is sought. |
| pareto4 | shape2 | uniqueness | accepted | Check the prior untruncated divergence proof for promotion to this modifier mask under closed-window normalization, and classify other joint escapes if an exhaustive failure row is sought. |
| pareto4 | shape2 | case:threshold-spike | accepted | A negative lower power exponent proves divergence at positive retained width, and a constant at the upper cut admits shrinking for any positive power. |
| pgw | all free | criterion | accepted | Exhaust joint shape, scale and exponent escapes and handle zero observations. |
| pgw | all free | failure | accepted | Exhaust joint shape, scale and exponent escapes and handle zero observations. |
| pgw | all free | uniqueness | accepted | Exhaust joint shape, scale and exponent escapes and handle zero observations. |
| pgw | all free | case:constant-collapse | accepted | Matching the scale to the observation makes the density grow linearly with shape and conditioning can only increase that density. |
| pgw | exponent | criterion | accepted | Exhaust joint shape and scale with exponent held escapes and handle zero observations. |
| pgw | exponent | failure | accepted | Exhaust joint shape and scale with exponent held escapes and handle zero observations. |
| pgw | exponent | uniqueness | accepted | Exhaust joint shape and scale with exponent held escapes and handle zero observations. |
| pgw | exponent+scale | criterion | accepted | The two shape ends exhaust escapes, with zero height or log-uniform at small shape and a support-aware concentration test at large shape. |
| pgw | exponent+scale | failure | accepted | The large-shape estimates include both cuts outside the scale and the small-shape log-uniform is integrable only on a bounded window away from zero. |
| pgw | exponent+scale | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| pgw | exponent+scale+shape | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| pgw | exponent+scale+shape | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| pgw | exponent+scale+shape | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| pgw | exponent+shape | criterion | accepted | The two scale ends are exhaustive and finite equality with their larger finite height permits attainment. |
| pgw | exponent+shape | failure | accepted | The broad-scale power is integrable at zero and small scale can win only as a constant at a strictly positive lower cut. |
| pgw | exponent+shape | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| pgw | scale | criterion | accepted | Exhaust joint shape and exponent with scale held escapes and handle zero observations. |
| pgw | scale | failure | accepted | Exhaust joint shape and exponent with scale held escapes and handle zero observations. |
| pgw | scale | uniqueness | accepted | Exhaust joint shape and exponent with scale held escapes and handle zero observations. |
| pgw | scale+shape | criterion | accepted | Both exponent ends at held scale and shape are accounted for, and the finite comparison explicitly admits equality. |
| pgw | scale+shape | failure | accepted | The zero-exponent derivative carrier is integrable at a finite upper cut and the large-exponent collapse can win only at a positive lower-cut constant. |
| pgw | scale+shape | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| pgw | shape | criterion | accepted | Exhaust joint scale and exponent with shape held escapes and handle zero observations. |
| pgw | shape | failure | accepted | Exhaust joint scale and exponent with shape held escapes and handle zero observations. |
| pgw | shape | uniqueness | accepted | Exhaust joint scale and exponent with shape held escapes and handle zero observations. |
| sgomp | all free | criterion | accepted | The log-slope bounds exhaust joint escapes of nonconstant data and permit equality with the largest bounded onset height. |
| sgomp | all free | failure | accepted | Joint rate-shape subsequences give only the bounded onset carrier at positive finite height, and constants have explicit divergent concentration paths. |
| sgomp | all free | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| sgomp | b | criterion | accepted | At held rate there is one excluded shape end and the remaining boundary at eta zero is included. |
| sgomp | b | failure | accepted | The conditional CDF ratio makes the upper-cut constant the sole winning infinite-shape path at held rate. |
| sgomp | b | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| sgomp | b+eta | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| sgomp | b+eta | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| sgomp | b+eta | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| sgomp | eta | criterion | accepted | With shape held, the positive rate has two exhaustive ends and a finite equality with the bounded uniform suffices. |
| sgomp | eta | failure | accepted | The exponential ratio bound controls large rate and the density at vanishing rate gives only the bounded uniform at positive height. |
| sgomp | eta | uniqueness | accepted | The comparison controls attainment but leaves general conditional maximizing-set cardinality unresolved. |
| triang | all free | criterion | accepted | The rational height on each order stratum projects to semialgebraic intervals and points, with equality decided by inclusion of the highest endpoint. |
| triang | all free | failure | accepted | Algebraic elimination settles winning heights while the elementary classification by reachable limiting kernels and traces remains open. |
| triang | all free | uniqueness | accepted | The exact height fiber is an explicit semialgebraic description of all maximizers, including intervals and equality strata. |
| triang | all free | case:width-bound | accepted | The concave affine pieces have integral at least half the peak times the retained width, which is bounded below by the range of nonconstant data. |
| triang | a | criterion | accepted | The rational height on each order stratum projects to semialgebraic intervals and points, with equality decided by inclusion of the highest endpoint. |
| triang | a | failure | accepted | Algebraic elimination settles winning heights while the elementary classification by reachable limiting kernels and traces remains open. |
| triang | a | uniqueness | accepted | The exact height fiber is an explicit semialgebraic description of all maximizers, including intervals and equality strata. |
| triang | a | case:width-bound | accepted | The concave affine pieces have integral at least half the peak times the retained width, which is bounded below by the range of nonconstant data. |
| triang | a+b | criterion | accepted | The rational height on each order stratum projects to semialgebraic intervals and points, with equality decided by inclusion of the highest endpoint. |
| triang | a+b | failure | accepted | The compact mode interval has no remote parameter path, and an elementary classification can focus on the mode strata and endpoint traces. |
| triang | a+b | uniqueness | accepted | The exact height fiber is an explicit semialgebraic description of all maximizers, including intervals and equality strata. |
| triang | a+b | case:width-bound | accepted | The concave affine pieces have integral at least half the peak times the retained width, which is bounded below by the range of nonconstant data. |
| triang | a+b | case:nonunique | regraded | Exact rational integral and gap identities give strict loss at every interior mode and the same maximum on both closed plateaus. |
| triang | a+b+c | criterion | accepted | A compatible held model has positive retained mass and its sole coefficient vector attains the finite conditional likelihood. |
| triang | a+b+c | failure | accepted | Holding every parent coefficient leaves no free parameter that can approach an excluded end. |
| triang | a+b+c | uniqueness | accepted | All coefficients are held, so the compatible parameter set is a singleton. |
| triang | a+b+c | case:width-bound | accepted | The concave affine pieces have integral at least half the peak times the retained width, which is bounded below by the range of nonconstant data. |
| triang | a+c | criterion | accepted | The rational height on each order stratum projects to semialgebraic intervals and points, with equality decided by inclusion of the highest endpoint. |
| triang | a+c | failure | accepted | Algebraic elimination settles winning heights while the elementary classification by reachable limiting kernels and traces remains open. |
| triang | a+c | uniqueness | accepted | The exact height fiber is an explicit semialgebraic description of all maximizers, including intervals and equality strata. |
| triang | a+c | case:width-bound | accepted | The concave affine pieces have integral at least half the peak times the retained width, which is bounded below by the range of nonconstant data. |
| triang | b | criterion | accepted | The rational height on each order stratum projects to semialgebraic intervals and points, with equality decided by inclusion of the highest endpoint. |
| triang | b | failure | accepted | Algebraic elimination settles winning heights while the elementary classification by reachable limiting kernels and traces remains open. |
| triang | b | uniqueness | accepted | The exact height fiber is an explicit semialgebraic description of all maximizers, including intervals and equality strata. |
| triang | b | case:width-bound | accepted | The concave affine pieces have integral at least half the peak times the retained width, which is bounded below by the range of nonconstant data. |
| triang | b+c | criterion | accepted | The rational height on each order stratum projects to semialgebraic intervals and points, with equality decided by inclusion of the highest endpoint. |
| triang | b+c | failure | accepted | Algebraic elimination settles winning heights while the elementary classification by reachable limiting kernels and traces remains open. |
| triang | b+c | uniqueness | accepted | The exact height fiber is an explicit semialgebraic description of all maximizers, including intervals and equality strata. |
| triang | b+c | case:width-bound | accepted | The concave affine pieces have integral at least half the peak times the retained width, which is bounded below by the range of nonconstant data. |
| triang | c | criterion | accepted | The rational height on each order stratum projects to semialgebraic intervals and points, with equality decided by inclusion of the highest endpoint. |
| triang | c | failure | accepted | Algebraic elimination settles winning heights while the elementary classification by reachable limiting kernels and traces remains open. |
| triang | c | uniqueness | accepted | The exact height fiber is an explicit semialgebraic description of all maximizers, including intervals and equality strata. |
| triang | c | case:width-bound | accepted | The concave affine pieces have integral at least half the peak times the retained width, which is bounded below by the range of nonconstant data. |

## Proof errors and scope corrections

### GPD and Pareto IV joint escapes

GPD's all-free failure row lists fixed-positive-shape concentration and
shrinking uniform width as if they exhausted the ways to diverge. Take
sample `(1,2)` on `[1,Inf]`, set the threshold to 1, and use the Pareto II
parameters $s>0$ and $h=s^{1/4}$. The GPD parameters are $\xi=1/h$ and
$\sigma=s/h$. The retained mass equals one. The joint height is
$(h/s)^2(1+1/s)^{-h-1}\sim s^{-1/2}$. Meanwhile the survival at every
fixed positive excess $y$ tends to one, since $h\log(1+y/s)\to0$.
Thus the mass escapes every bounded interval even as the height diverges.
The attainment and empty maximizing-set claims still follow from the
original witnesses. [GPD derivation](2026-11-27-trunc-misc-math-report.md#generalized-pareto) · [Earlier Pareto II counterexample](2026-11-27-mod-rev6-report.md#pareto-ii-can-diverge-with-escaping-mass)

For Pareto IV, set $T\to\infty$, $s=m-d=e^{-T}$,
$h=1/T$, and hold the lower power $p>0$ fixed. The density at $m$ is
asymptotic to $p e^T/(2T)$, while every positive gap $g=x-m$ contributes
$p e^{-p}/(Tg)$. The raw joint likelihood diverges and conditioning
divides it by a factor at most one. The path retains positive mass for
each finite $T$ on a compatible window, even if $m$ is a cut. This also
applies to the open Pareto IV mask with only `shape2` held. Its open row
is preserved per the review rule and flags the available proof for a
separate atlas decision. For Pareto III or IV with the lower power free,
even a held scale permits concentration at an interior constant without
threshold contact: set $d=x_0-s$ and send $p$ to infinity at fixed $h$.
The density at $x_0$ is then $ph/(2^{h+1}s)$, which diverges before
conditioning. These paths establish why the exact joint failure grades
are too broad. [Prior threshold proof](2026-11-25-thresh-math-report.md#structural-spikes) · [TASK-462 Pareto proof](2026-11-27-trunc-misc-math-report.md#pareto-iii-and-iv)

Nonstandard beta also has a distinct concentration mechanism. In the
all-free mask, use fixed endpoints $-1,1$, sample `(0)`, window
`[-1,1]`, and both shapes tending to infinity together. Divide the
kernel by its value at zero to obtain $(1-x^2)^{k-1}$. Its integral
tends to zero by dominated convergence and the occupied density
diverges with both endpoints staying away from the observation. The
contact result suffices for the no-attainment rows but does not exhaust
joint failure. Where only one shape is free the report likewise gives
a contact witness without an exhaustive joint analysis.
[TASK-462 beta proof](2026-11-27-trunc-misc-math-report.md#nonstandard-beta)

### Discretized-gamma occupancy and count rays

The free criterion's adjacency branch applies only when **exactly two**
cells are occupied and their count difference is one. For example, on
`[0,Inf]` the sample `(0,1,2)` attains even though it contains
adjacent occupied cells and leaves infinitely many other retained cells.
Compare cells zero and two. When the shape grows, the strict
log-kernel separation between those cells and an intervening interval
forces one of their probabilities to zero. The argument applies to
their factors in the full sample product because all other cell
probabilities are at most one. For this window the zero-shape and
zero-rate faces are nonintegrable, so there is no positive excluded
height and compactness gives an attained maximum. The held-rate
large-shape failure wording also needs a finite upper cut. On a right
ray any fixed cell loses to a larger cell. On exactly two retained cells,
sample `(0,1)` attains its empirical Bernoulli height on a continuum
because the strictly decreasing rate odds cross one at every
sufficiently large fixed shape. [Grouped-gamma proof](2026-11-27-trunc-misc-math-report.md#discretized-gamma)

### Scalar GPD and beta scope

GPD's held-threshold and held-shape failure list needs the exponential
concentration case at shape zero. For example, with threshold 0, known
window `[1,2]` and sample `(1)`, the density is
$1/[\sigma(1-e^{-1/\sigma})]$ at the retained lower cut and diverges
as scale tends to zero. The raw support does not shrink. The free-shape
comparison at held threshold and scale also needs the all-threshold
right-ray plateau: the trace at infinite shape ties every finite
shape at height $\sigma^{-n}$. At a finite uniform support contact,
the included uniform value dominates the neighboring limsup when
retained width stays positive. If the window instead shrinks at contact,
the inverse-width trace is tested first. The scale-only rows explicitly
use the stated shape range $\xi\ge-1$. [GPD cases G1 and G2](2026-11-27-trunc-misc-math-report.md#generalized-pareto)

For fixed-support beta with both shapes free, the inherited inward-score
criterion uses finite logs of both endpoint distances. With one shape
held, a sample at its corresponding endpoint is admissible for this
test when that held shape is one and the free log statistic is finite.
The scalar comparison has one zero-shape carrier and cannot access
the two-free corner. For a single moving endpoint and held shapes,
the remote integrable face is the fixed-side power, with uniform as
its unit-shape case. Its integral remains finite at the included
opposite endpoint for any valid held positive shape when the effective
interval is bounded. Endpoint observations still require a positive
finite density for this comparison.
[Inherited beta theorem](2026-11-27-trunc-expfam-math-report.md#beta-free-truncateknown) · [Nonstandard-beta proof](2026-11-27-trunc-misc-math-report.md#nonstandard-beta)

### Triangular exact check

For held endpoints `a=-1, b=2`, window `[0,1]` and sample `(0,1)`,
the outer mode intervals retain a single affine branch. Dropping its
common factor gives endpoint heights 2 and 1 and area $3/2$, hence
joint height $8/9$. For $0<c<1$, put $D=(c+1)(2-c)=2+c(1-c)$. The
two trapezoids have total area $3(D-1)/(2D)$, with endpoint product
$1/D$. Subtracting the interior height from $8/9$ gives
$4(2D-1)(D-2)/(9(D-1)^2)>0$. Thus the entire maximizing set is
`[-1,0] ∪ [1,2]`, including the two cut modes. The check script
verifies the polynomial identities over rational numbers and the sign
holds on the full real interior mode interval.
[Source proof](2026-11-27-trunc-misc-math-report.md#triangular) · [Exact check](2026-11-27-mod-rev9-data/validate_review.py)

The general triangular height sets come from elimination over the reals,
including the strata where a cut or observation equals an endpoint or the
mode. This is an exact description of the height fiber and hence of the
full maximizing set. An executable exact comparison requires an exact
input representation, such as algebraic coordinates. The semialgebraic
argument is independent of the numerical mode grid.

## Rows not joined as proofs

No proposed key was rejected or omitted from the review file. The
128 open rows remain `unproved` and are carried as frontier
records. The coordinator can use the 205 written rows as proof
inputs, including the one-sided regrades. There were no missing parent
masks, ineligible modifier keys or claims that relied on numerical root
ranking.

## Open by family

| Family | Open rows | One-sided written rows | What remains |
| --- | ---: | ---: | --- |
| dgamma | 3 | 1 | Determine full cardinality beyond two retained cells in the attained free and scalar branches, including ties between stationary components. |
| expweibull | 14 | 1 | Handle zero observations and exhaust joint escapes, especially the threshold path above the held scale when retained mass vanishes, and determine cardinality on the scalar scale and shape branches. |
| fatigue | 14 | 3 | Exhaust joint shape, scale and threshold limits in the free and single-hold masks and rank the stationary winners on the scalar scale and location branches. |
| foldnorm | 3 | 1 | Determine cardinality for general free and held-coordinate fits, with the zero-location tilt already strictly concave. |
| gev | 12 | 6 | Compare finite models against joint shape, scale and support-contact traces, the critical minimum-tie height and the increasing exponential when accessible under the holds. |
| gpd | 11 | 3 | Classify joint divergences in the free mask and joint escapes with threshold, scale or shape held, and settle cardinality for the general scalar branches beyond the identified plateaus. |
| naka | 0 | 0 | The positive-support masks have no open attainment, failure or cardinality rows. |
| nsbeta | 9 | 20 | Complete the mixed endpoint and shape regimes without direct spikes and classify additional divergent paths, then determine cardinality for the endpoint comparisons. |
| pareto3 | 11 | 4 | Complete the held-threshold shape and scale envelope and the held-shape joint threshold and scale envelope, including critical ties, and settle scalar cardinality. |
| pareto4 | 26 | 8 | Complete the held-threshold shape and scale envelopes and joint moving-threshold limits, assess promotion of the transferred held-lower-power divergence, and determine general scalar cardinality. |
| pgw | 15 | 1 | Handle zero observations and exhaust joint escapes, especially conditional heights below the threshold face at vanishing retention, and determine general scalar cardinality. |
| sgomp | 3 | 0 | Determine the number and shape of the maximizing stationary components in the joint and scalar branches. |
| triang | 7 | 9 | Give elementary classifications of the attainable and excluded affine or two-piece limits and observation traces by hold. The algebraic attainment and full maximizing-set tests already cover these cases. |

## Checks

The workstation's single-slot wrapper ran the reviewer validation script.
It checks the parent masks and modifier eligibility, all evidence anchors
including those on open entries, and unique row keys. It also runs the
exact triangular check. The atlas's modifier-input validator passes.
The TASK-462 limit-check script was replayed from a scratch copy with the
single-slot wrapper. All 39 checks passed at 100 digits and the new CSV
matched the original byte for byte. The replay illustrates the limits
independently of the written proofs.

The repository link check reported 47 inherited destinations and exited
with status 1 on the known false positive `P+Q` in the beta-prime math
report at line 461. The vendored Batteries README produced no diagnostic
in this worktree. The review adds no broken links.
[Validator](2026-11-27-mod-rev9-data/validate_review.py)
