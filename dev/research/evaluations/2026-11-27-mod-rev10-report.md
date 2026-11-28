# Independent review of known-window truncation for the GB2 tree

TASK-466 reviewed all 255 proposals from TASK-461 across 103 eligible parent masks. The [reviewed CSV](2026-11-27-mod-rev10-data/reviewed.csv) contains 197 accepted rows and 58 regraded rows, with zero rejected or open-status input rows. It contains 103 iff criteria, 71 exact and 32 one-sided failure rows, 47 exact cardinality rows, and two one-sided nonuniqueness cases.

**Five masks omit nonzero power escapes at finite power and escaping scale.** These escapes can supply a strictly winning excluded height. Both the criterion and failure rows for those masks use the corrected comparison below. Two divergent-contact constructions also show why a likelihood-divergence witness cannot classify the limiting conditional law.

The review uses the closed imposed window intersected with the parent support. At closed parent endpoints it uses the inside density, and all finite-fit criteria require positive retained mass and finite positive joint likelihood. The comparison includes the whole parent window and both singly cut windows. Reciprocal and log transformations preserve observations on finite included cuts, with a parameter-independent Jacobian. No proposed modifier uses an estimated bound or a zero modifier. [Modifier design](../../design/modifiers.md) · [Parent atlas](../existence-atlas/atlas.csv) · [Modifier atlas](../existence-atlas/modifier-atlas.csv) · [Source report](2026-11-27-trunc-gb2-math-report.md)

## Row verdicts

| Family | Held | Question | Verdict | Reason |
| --- | --- | --- | --- | --- |
| betapr | all free | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| betapr | all free | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| betapr | scale | criterion | accepted | Strict concavity and the nonconstant-data tail bound leave only the integrable zero-shape edges and corner. |
| betapr | scale | failure | accepted | Strict concavity and the nonconstant-data tail bound leave only the integrable zero-shape edges and corner. |
| betapr | scale | uniqueness | accepted | Strict concavity and the nonconstant-data tail bound leave only the integrable zero-shape edges and corner. |
| betapr | scale+shape1 | criterion | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| betapr | scale+shape1 | failure | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| betapr | scale+shape1 | uniqueness | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| betapr | scale+shape1+shape2 | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| betapr | scale+shape1+shape2 | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| betapr | scale+shape1+shape2 | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| betapr | scale+shape2 | criterion | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| betapr | scale+shape2 | failure | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| betapr | scale+shape2 | uniqueness | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| betapr | shape1 | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| betapr | shape1 | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| betapr | shape1+shape2 | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| betapr | shape1+shape2 | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| betapr | shape2 | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| betapr | shape2 | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| burr | all free | criterion | accepted | The finite-power, small-power slope-curvature, and large-power wall analyses cover all subsequences under the given holds. |
| burr | all free | failure | regraded | A held beta shape forces zero small-power curvature and rules out the large-power two-piece face. |
| burr | scale | criterion | accepted | With scale fixed the zero-shape, nonpositive log-tilt and lower-wall branches exhaust the escapes in the forward or reflected coordinate. |
| burr | scale | failure | accepted | With scale fixed the zero-shape, nonpositive log-tilt and lower-wall branches exhaust the escapes in the forward or reflected coordinate. |
| burr | scale+shape1 | criterion | accepted | At fixed scale and beta shapes the modal projection gives the only collapse and the small-power limit is log-uniform when integrable. |
| burr | scale+shape1 | failure | accepted | At fixed scale and beta shapes the modal projection gives the only collapse and the small-power limit is log-uniform when integrable. |
| burr | scale+shape1+shape2 | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| burr | scale+shape1+shape2 | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| burr | scale+shape1+shape2 | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| burr | scale+shape2 | criterion | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| burr | scale+shape2 | failure | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| burr | scale+shape2 | uniqueness | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| burr | shape1 | criterion | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| burr | shape1 | failure | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| burr | shape1+shape2 | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| burr | shape1+shape2 | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| burr | shape2 | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| burr | shape2 | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| fpareto | all free | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | all free | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | all free | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | min | criterion | accepted | Translation preserves the corresponding GB2 holds on positive excesses and a free factor permits a subunit density at threshold contact. |
| fpareto | min | failure | regraded | The proof establishes the finite-height comparison or a divergence witness, with joint limiting laws in singular regimes still open. |
| fpareto | min+scale+shape1+shape2+shape3 | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| fpareto | min+scale+shape1+shape2+shape3 | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| fpareto | min+scale+shape1+shape2+shape3 | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| fpareto | min+scale+shape2 | criterion | accepted | Translation preserves the corresponding GB2 holds on positive excesses and a free factor permits a subunit density at threshold contact. |
| fpareto | min+scale+shape2 | failure | regraded | The proof establishes the finite-height comparison or a divergence witness, with joint limiting laws in singular regimes still open. |
| fpareto | min+scale+shape2 | uniqueness | accepted | Translation preserves the corresponding GB2 holds on positive excesses and a free factor permits a subunit density at threshold contact. |
| fpareto | min+shape1 | criterion | accepted | Translation preserves the corresponding GB2 holds on positive excesses and a free factor permits a subunit density at threshold contact. |
| fpareto | min+shape1 | failure | regraded | The proof establishes the finite-height comparison or a divergence witness, with joint limiting laws in singular regimes still open. |
| fpareto | min+shape1+shape3 | criterion | accepted | Translation preserves the corresponding GB2 holds on positive excesses and a free factor permits a subunit density at threshold contact. |
| fpareto | min+shape1+shape3 | failure | regraded | The proof establishes the finite-height comparison or a divergence witness, with joint limiting laws in singular regimes still open. |
| fpareto | min+shape3 | criterion | accepted | Translation preserves the corresponding GB2 holds on positive excesses and a free factor permits a subunit density at threshold contact. |
| fpareto | min+shape3 | failure | regraded | The proof establishes the finite-height comparison or a divergence witness, with joint limiting laws in singular regimes still open. |
| fpareto | scale | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | scale+shape1 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape1 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape1 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | scale+shape1+shape2 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape1+shape2 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape1+shape2 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | scale+shape1+shape2+shape3 | criterion | accepted | With only threshold free the bounded-window remote uniform is the sole regular excluded height and feasible closed joins are included. |
| fpareto | scale+shape1+shape2+shape3 | failure | accepted | With only threshold free the bounded-window remote uniform is the sole regular excluded height and feasible closed joins are included. |
| fpareto | scale+shape1+shape3 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape1+shape3 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape1+shape3 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | scale+shape2 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape2 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape2 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | scale+shape2+shape3 | criterion | accepted | The held scale controls small-excess behavior while remote threshold and growing tail shape jointly produce the decreasing exponential. |
| fpareto | scale+shape2+shape3 | failure | regraded | The proof establishes the finite-height comparison or a divergence witness, with joint limiting laws in singular regimes still open. |
| fpareto | scale+shape3 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape3 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | scale+shape3 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | shape1 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape1 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape1 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | shape1+shape2 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape1+shape2 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape1+shape2 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | shape1+shape2+shape3 | case:exact-nonunique-sample | regraded | An exact squared-factor likelihood gap proves global optimality at the common reduced scale for distinct feasible pairs. |
| fpareto | shape1+shape2+shape3 | criterion | regraded | The endpoint comparison includes the feasible sample-minimum contact as well as the fixed-cut join. |
| fpareto | shape1+shape2+shape3 | failure | regraded | The proof establishes the finite-height comparison or a divergence witness, with joint limiting laws in singular regimes still open. |
| fpareto | shape1+shape3 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape1+shape3 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape1+shape3 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | shape2 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape2 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape2 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | shape2+shape3 | criterion | accepted | Choosing a sufficiently small fixed positive tail shape yields a negative contact exponent and proves divergence. |
| fpareto | shape2+shape3 | failure | accepted | Choosing a sufficiently small fixed positive tail shape yields a negative contact exponent and proves divergence. |
| fpareto | shape2+shape3 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| fpareto | shape3 | criterion | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape3 | failure | accepted | A free low-end factor gives a subunit threshold spike, with width collapse at equality with the upper cut. |
| fpareto | shape3 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| gb2 | all free | criterion | accepted | The finite-power, small-power slope-curvature, and large-power wall analyses cover all subsequences under the given holds. |
| gb2 | all free | failure | accepted | The finite-power, small-power slope-curvature, and large-power wall analyses cover all subsequences under the given holds. |
| gb2 | scale+shape1+shape2 | criterion | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| gb2 | scale+shape1+shape2 | failure | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| gb2 | scale+shape1+shape2 | uniqueness | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| gb2 | scale+shape2 | criterion | accepted | Strict concavity and the nonconstant-data tail bound leave only the integrable zero-shape edges and corner. |
| gb2 | scale+shape2 | failure | accepted | Strict concavity and the nonconstant-data tail bound leave only the integrable zero-shape edges and corner. |
| gb2 | scale+shape2 | uniqueness | accepted | Strict concavity and the nonconstant-data tail bound leave only the integrable zero-shape edges and corner. |
| gb2 | scale+shape2+shape3 | criterion | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| gb2 | scale+shape2+shape3 | failure | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| gb2 | scale+shape2+shape3 | uniqueness | accepted | The fixed transform gives an identified scalar beta tilt with strictly increasing mean and explicit eligible end limits. |
| gb2 | shape1 | criterion | accepted | The finite-power, small-power slope-curvature, and large-power wall analyses cover all subsequences under the given holds. |
| gb2 | shape1 | failure | regraded | A held beta shape forces zero small-power curvature and rules out the large-power two-piece face. |
| gb2 | shape1+shape3 | criterion | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| gb2 | shape1+shape3 | failure | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| gb2 | shape3 | criterion | accepted | The finite-power, small-power slope-curvature, and large-power wall analyses cover all subsequences under the given holds. |
| gb2 | shape3 | failure | regraded | A held beta shape forces zero small-power curvature and rules out the large-power two-piece face. |
| genbeta | all free | criterion | accepted | The fixed-shape subunit spike or the lower-cut width collapse gives infinite supremum for every compatible sample. |
| genbeta | all free | failure | accepted | The fixed-shape subunit spike or the lower-cut width collapse gives infinite supremum for every compatible sample. |
| genbeta | all free | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| genbeta | scale | criterion | accepted | The held-endpoint log-kernel partitions the finite-power, log-distance and moving-wall regimes, with Fatou bounding any extra endpoint mass. |
| genbeta | scale | failure | regraded | The finite-height comparison and the singularity witness leave joint boundary laws in the singular cases unclassified. |
| genbeta | scale+shape1 | criterion | accepted | Holding the lower shape restricts the bounded-height ends to the stated zero-upper-shape and log-distance limits. |
| genbeta | scale+shape1 | failure | regraded | The finite-height comparison and the singularity witness leave joint boundary laws in the singular cases unclassified. |
| genbeta | scale+shape1+shape2+shape3 | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| genbeta | scale+shape1+shape2+shape3 | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| genbeta | scale+shape1+shape2+shape3 | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| genbeta | scale+shape1+shape3 | criterion | regraded | Closed endpoint contact at unit upper shape can enter a finite fit only when the upper shape is held there. |
| genbeta | scale+shape1+shape3 | failure | regraded | The finite-height comparison and the singularity witness leave joint boundary laws in the singular cases unclassified. |
| genbeta | scale+shape1+shape3 | uniqueness | accepted | The fixed beta transform gives strict likelihood concavity for interior data and the inside density decides closed parent contact. |
| genbeta | scale+shape2 | case:exact-nonunique-sample | regraded | Strict concavity in the product proves a global optimum and two distinct feasible factorizations realize it. |
| genbeta | scale+shape2 | criterion | accepted | Fixed upper shape gives the stated limits in power and lower slope, with the unit-shape model reducing to one product. |
| genbeta | scale+shape2 | failure | regraded | The finite-height comparison and the singularity witness leave joint boundary laws in the singular cases unclassified. |
| genbeta | scale+shape2+shape3 | criterion | regraded | Closed endpoint contact at unit upper shape can enter a finite fit only when the upper shape is held there. |
| genbeta | scale+shape2+shape3 | failure | regraded | The explicit contact split distinguishes excluded tilt ends from a held singular or zero trace. |
| genbeta | scale+shape2+shape3 | uniqueness | accepted | The fixed beta transform gives strict likelihood concavity for interior data and the inside density decides closed parent contact. |
| genbeta | scale+shape3 | criterion | regraded | Closed endpoint contact at unit upper shape can enter a finite fit only when the upper shape is held there. |
| genbeta | scale+shape3 | failure | regraded | The finite-height comparison and the singularity witness leave joint boundary laws in the singular cases unclassified. |
| genbeta | scale+shape3 | uniqueness | accepted | The fixed beta transform gives strict likelihood concavity for interior data and the inside density decides closed parent contact. |
| genbeta | shape1 | criterion | accepted | The fixed-shape subunit spike or the lower-cut width collapse gives infinite supremum for every compatible sample. |
| genbeta | shape1 | failure | accepted | The fixed-shape subunit spike or the lower-cut width collapse gives infinite supremum for every compatible sample. |
| genbeta | shape1 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| genbeta | shape1+shape2 | criterion | regraded | The row now states the subunit and constant-data outcomes as well as the finite superunit comparison. |
| genbeta | shape1+shape2 | failure | regraded | The finite-height comparison and the singularity witness leave joint boundary laws in the singular cases unclassified. |
| genbeta | shape1+shape2+shape3 | criterion | regraded | The endpoint-only test includes the subunit branch and the finite closed contact at unit shape. |
| genbeta | shape1+shape2+shape3 | failure | accepted | Only the endpoint varies, so the closed unit-shape contact or the regular pure-power end handles equality. |
| genbeta | shape1+shape3 | criterion | accepted | The fixed-shape subunit spike or the lower-cut width collapse gives infinite supremum for every compatible sample. |
| genbeta | shape1+shape3 | failure | accepted | The fixed-shape subunit spike or the lower-cut width collapse gives infinite supremum for every compatible sample. |
| genbeta | shape1+shape3 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| genbeta | shape2 | criterion | regraded | The row now states the subunit and constant-data outcomes as well as the finite superunit comparison. |
| genbeta | shape2 | failure | regraded | The finite-height comparison and the singularity witness leave joint boundary laws in the singular cases unclassified. |
| genbeta | shape2+shape3 | criterion | regraded | The row now states the subunit and constant-data outcomes as well as the finite superunit comparison. |
| genbeta | shape2+shape3 | failure | regraded | The finite-height comparison and the singularity witness leave joint boundary laws in the singular cases unclassified. |
| genbeta | shape3 | criterion | accepted | The fixed-shape subunit spike or the lower-cut width collapse gives infinite supremum for every compatible sample. |
| genbeta | shape3 | failure | accepted | The fixed-shape subunit spike or the lower-cut width collapse gives infinite supremum for every compatible sample. |
| genbeta | shape3 | uniqueness | regraded | The divergence proof establishes an empty finite-positive-likelihood maximizing set even when a closed singular parameter value has infinite likelihood. |
| gengamma | Q | criterion | accepted | Fixed nonzero Q is a signed fixed-gamma-shape problem and Q zero uses both normal natural parameters. |
| gengamma | Q | failure | regraded | The signed comparison includes Q zero, where either normal power-tilt sign is possible. |
| gengamma | Q+mu | criterion | accepted | Fixed Q and mu give a fixed log-coordinate template scaled about its mode, including the normal template at Q zero. |
| gengamma | Q+mu | failure | accepted | The fixed-Q sign and normal seam specialize to the stated conditioned log-coordinate limit. |
| gengamma | Q+sigma | criterion | accepted | Fixed nonzero Q and sigma leave a scalar gamma rate tilt and Q zero leaves the fixed-variance normal location tilt. |
| gengamma | Q+sigma | failure | accepted | Fixed nonzero Q and sigma leave a scalar gamma rate tilt and Q zero leaves the fixed-variance normal location tilt. |
| gengamma | Q+sigma | uniqueness | accepted | Fixed nonzero Q and sigma leave a scalar gamma rate tilt and Q zero leaves the fixed-variance normal location tilt. |
| gengamma | all free | criterion | accepted | Both transformed-gamma signs exhaust the off-seam escapes and the attainable normal seam is tested separately. |
| gengamma | all free | failure | accepted | Both transformed-gamma signs exhaust the off-seam escapes and the attainable normal seam is tested separately. |
| gengamma | mu | criterion | accepted | The mode hold links wall position to slope, with the normal seam and the projected-mode collapse included. |
| gengamma | mu | failure | accepted | The mode hold links wall position to slope, with the normal seam and the projected-mode collapse included. |
| gengamma | mu+sigma | criterion | accepted | Only the two signed infinite-Q ends escape and Q zero is a finite continuous seam candidate. |
| gengamma | mu+sigma | failure | accepted | Only the two signed infinite-Q ends escape and Q zero is a finite continuous seam candidate. |
| gengamma | sigma | criterion | accepted | The scale hold fixes the small-power quadratic limit on the seam and the large-power slope vanishes at the moving wall. |
| gengamma | sigma | failure | accepted | The scale hold fixes the small-power quadratic limit on the seam and the large-power slope vanishes at the moving wall. |
| invburr | all free | criterion | accepted | The finite-power, small-power slope-curvature, and large-power wall analyses cover all subsequences under the given holds. |
| invburr | all free | failure | regraded | A held beta shape forces zero small-power curvature and rules out the large-power two-piece face. |
| invburr | scale | criterion | accepted | With scale fixed the zero-shape, nonpositive log-tilt and lower-wall branches exhaust the escapes in the forward or reflected coordinate. |
| invburr | scale | failure | accepted | With scale fixed the zero-shape, nonpositive log-tilt and lower-wall branches exhaust the escapes in the forward or reflected coordinate. |
| invburr | scale+shape1 | criterion | accepted | At fixed scale and beta shapes the modal projection gives the only collapse and the small-power limit is log-uniform when integrable. |
| invburr | scale+shape1 | failure | accepted | At fixed scale and beta shapes the modal projection gives the only collapse and the small-power limit is log-uniform when integrable. |
| invburr | scale+shape1+shape2 | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invburr | scale+shape1+shape2 | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invburr | scale+shape1+shape2 | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invburr | scale+shape2 | criterion | accepted | The fixed transform gives an identified scalar reciprocal beta tilt with strictly increasing mean and explicit eligible end limits. |
| invburr | scale+shape2 | failure | accepted | The fixed transform gives an identified scalar reciprocal beta tilt with strictly increasing mean and explicit eligible end limits. |
| invburr | scale+shape2 | uniqueness | accepted | The fixed transform gives an identified scalar reciprocal beta tilt with strictly increasing mean and explicit eligible end limits. |
| invburr | shape1 | criterion | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| invburr | shape1 | failure | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| invburr | shape1+shape2 | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invburr | shape1+shape2 | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invburr | shape2 | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invburr | shape2 | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invparalogis | all free | criterion | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| invparalogis | all free | failure | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| invparalogis | scale | criterion | accepted | Tying upper shape to power leaves only broad log-uniform and projected-mode concentration, with the held-scale contact exception. |
| invparalogis | scale | failure | accepted | Tying upper shape to power leaves only broad log-uniform and projected-mode concentration, with the held-scale contact exception. |
| invparalogis | scale+shape | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invparalogis | scale+shape | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invparalogis | scale+shape | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invparalogis | shape | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invparalogis | shape | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invpareto | all free | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invpareto | all free | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invpareto | scale | criterion | accepted | The fixed transform gives an identified scalar reciprocal beta tilt with strictly increasing mean and explicit eligible end limits. |
| invpareto | scale | failure | accepted | The fixed transform gives an identified scalar reciprocal beta tilt with strictly increasing mean and explicit eligible end limits. |
| invpareto | scale | uniqueness | accepted | The fixed transform gives an identified scalar reciprocal beta tilt with strictly increasing mean and explicit eligible end limits. |
| invpareto | scale+shape | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invpareto | scale+shape | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invpareto | scale+shape | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invpareto | shape | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invpareto | shape | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| invtrgamma | all free | criterion | accepted | The free gamma log kernel separates finite-power edges, the small-power quadratic and the moving wall, with full trace approachable. |
| invtrgamma | all free | failure | accepted | The free gamma log kernel separates finite-power edges, the small-power quadratic and the moving wall, with full trace approachable. |
| invtrgamma | scale | criterion | accepted | At held scale the finite-power zero-shape, broad power and large-power wall with its contact penalty exhaust the competing heights. |
| invtrgamma | scale | failure | accepted | At held scale the finite-power zero-shape, broad power and large-power wall with its contact penalty exhaust the competing heights. |
| invtrgamma | scale+shape1 | criterion | accepted | A fixed gamma template about the held log scale has just the bounded-window broad limit and the projected-mode collapse. |
| invtrgamma | scale+shape1 | failure | accepted | A fixed gamma template about the held log scale has just the bounded-window broad limit and the projected-mode collapse. |
| invtrgamma | scale+shape1+shape2 | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invtrgamma | scale+shape1+shape2 | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invtrgamma | scale+shape1+shape2 | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| invtrgamma | scale+shape2 | criterion | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| invtrgamma | scale+shape2 | failure | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| invtrgamma | scale+shape2 | uniqueness | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| invtrgamma | shape1 | criterion | accepted | With gamma shape held the two regular scale-power ends give opposite log tilts and fixed-template modal alignment settles constants. |
| invtrgamma | shape1 | failure | accepted | With gamma shape held the two regular scale-power ends give opposite log tilts and fixed-template modal alignment settles constants. |
| invtrgamma | shape1+shape2 | criterion | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| invtrgamma | shape1+shape2 | failure | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| invtrgamma | shape1+shape2 | uniqueness | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| invtrgamma | shape2 | criterion | accepted | The gamma natural domain is the positive shape-rate quadrant and strict concavity gives the edge and corner tests. |
| invtrgamma | shape2 | failure | accepted | The gamma natural domain is the positive shape-rate quadrant and strict concavity gives the edge and corner tests. |
| invtrgamma | shape2 | uniqueness | accepted | The gamma natural domain is the positive shape-rate quadrant and strict concavity gives the edge and corner tests. |
| paralogis | all free | criterion | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| paralogis | all free | failure | regraded | Finite positive power with scale escaping to zero or infinity gives nonzero log tilts in addition to the broad log-uniform. |
| paralogis | scale | criterion | accepted | Tying upper shape to power leaves only broad log-uniform and projected-mode concentration, with the held-scale contact exception. |
| paralogis | scale | failure | accepted | Tying upper shape to power leaves only broad log-uniform and projected-mode concentration, with the held-scale contact exception. |
| paralogis | scale+shape | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| paralogis | scale+shape | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| paralogis | scale+shape | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| paralogis | shape | criterion | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| paralogis | shape | failure | accepted | The fixed-power kernel and its reciprocal exhaust the finite-scale zero-shape, gamma and zero-rate power ends subject to the held shapes. |
| trgamma | all free | criterion | accepted | The free gamma log kernel separates finite-power edges, the small-power quadratic and the moving wall, with full trace approachable. |
| trgamma | all free | failure | accepted | The free gamma log kernel separates finite-power edges, the small-power quadratic and the moving wall, with full trace approachable. |
| trgamma | scale | criterion | accepted | At held scale the finite-power zero-shape, broad power and large-power wall with its contact penalty exhaust the competing heights. |
| trgamma | scale | failure | accepted | At held scale the finite-power zero-shape, broad power and large-power wall with its contact penalty exhaust the competing heights. |
| trgamma | scale+shape1 | criterion | accepted | A fixed gamma template about the held log scale has just the bounded-window broad limit and the projected-mode collapse. |
| trgamma | scale+shape1 | failure | accepted | A fixed gamma template about the held log scale has just the bounded-window broad limit and the projected-mode collapse. |
| trgamma | scale+shape1+shape2 | criterion | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| trgamma | scale+shape1+shape2 | failure | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| trgamma | scale+shape1+shape2 | uniqueness | accepted | The singleton domain leaves only evaluation of the closed-window density and its retained mass. |
| trgamma | scale+shape2 | criterion | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| trgamma | scale+shape2 | failure | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| trgamma | scale+shape2 | uniqueness | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| trgamma | shape1 | criterion | accepted | With gamma shape held the two regular scale-power ends give opposite log tilts and fixed-template modal alignment settles constants. |
| trgamma | shape1 | failure | accepted | With gamma shape held the two regular scale-power ends give opposite log tilts and fixed-template modal alignment settles constants. |
| trgamma | shape1+shape2 | criterion | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| trgamma | shape1+shape2 | failure | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| trgamma | shape1+shape2 | uniqueness | accepted | The fixed power gives a single gamma tilt with ordered mean thresholds in the appropriate forward or reciprocal coordinate. |
| trgamma | shape2 | criterion | accepted | The gamma natural domain is the positive shape-rate quadrant and strict concavity gives the edge and corner tests. |
| trgamma | shape2 | failure | accepted | The gamma natural domain is the positive shape-rate quadrant and strict concavity gives the edge and corner tests. |
| trgamma | shape2 | uniqueness | accepted | The gamma natural domain is the positive shape-rate quadrant and strict concavity gives the edge and corner tests. |

## Proof errors and scope corrections

### Finite power with escaping scale

The fixed-shape discussion classifies small and large power, but drops the finite positive power branch when log scale is free. This affects Burr and inverse Burr with shape1 held, GB2 with shape1 and shape3 held, and all-free paralogis and inverse paralogis. The held-scale variants have no such branch. [Source varying-power argument](2026-11-27-trunc-gb2-math-report.md#varying-the-power) · [Source GB2 fixed-shape row](2026-11-27-trunc-gb2-math-report.md#gb2-shape1shape3-truncateknown)

In the forward log coordinate write the kernel as
[
e^{pd(y-m)}(1+e^{d(y-m)})^{-p-q}.
]
Take (d	o d_0>0). When (m	o+infty), the kernel centered at a fixed point tends to a positive log tilt with slope (pd_0). When (m	o-infty), factor the softplus on its other side to obtain the negative tilt with slope (-q_0d_0). Here (q_0=q) under held shapes and (q_0=d_0) under the paralogis tie. Convergence on compact intervals holds for the kernels and their log slopes. On an unbounded retained end where the tilt is integrable, log concavity and a strictly inward tangent bound give dominated convergence on that end. When the limiting integral is infinite, Fatou gives zero sample likelihood. Thus an upper-only physical window can support the positive slope and a lower-only window can support the negative slope. Both signs are available on bounded log windows and neither sign is integrable on the whole log line. Reflection exchanges the signs and cuts, with no additional trace factor. Every nonzero slope of the permitted sign is approachable by choosing (d_0), including slope (-d_0^2) for forward paralogis. The zero tilt follows from the existing small-power proof.

For an explicit strict winner in the forward families use (x=(1,1,2)) on ([1,2]). Write (L=log 2). Each feasible finite log density has a strictly concave log on ([0,L]). Interpolate its endpoint log values by a chord. The exponential of that chord integrates to strictly less than one and has the same two endpoint values before normalizing. Normalizing it raises every sampled density, so even the best finite fit would lose to the best log tilt. The empirical log mean is (L/3<L/2). The variance identity for the tilt gives a unique maximizing slope, strictly negative, since the mean increases continuously from 0 to (L) and equals (L/2) at slope zero. The reachable escaping-scale tilt therefore wins strictly over log-uniform and every finite fit. Use (x=(1,1,1/2)) on ([1/2,1]) in the inverse families. The pointwise Jacobians multiply every likelihood by the same factor.

For exhaustion, take a subsequential power limit in ([0,infty]). At finite positive power the scale is either finite and feasible or in one of the preceding two ends. At zero power the uniform slope bound tends to zero, even with scale moving. At infinite power the left and right modal slopes diverge uniformly outside each fixed neighborhood of the mode, including under (q=d). For two distinct observations an interval toward the midpoint improves over the farther observation by a diverging log difference. If the normalized peak diverges, the separated-observation log-concavity bound already kills the joint likelihood. If the peak stays bounded, strict improvement kills one factor while bounding the rest. Constants diverge by aligning the freely movable mode. Outside that branch the stationary comparison uses the supremum over the complete list of integrable tilts, with equality allowed. If a root meets that threshold, it attains or a strictly better finite point forces a maximizing subsequence to stay in the feasible parameter region. [Normalization and comparison lemma](2026-11-27-trunc-gb2-math-report.md#likelihood-formulas-and-comparison-rules)

### Held beta shapes restrict the list

The proposed failure wording for all-free Burr and inverse Burr and the two single-beta-shape GB2 holds includes a lognormal and a two-piece power. The source proof explicitly removes both in these masks. For example with (p) held, the small-power slope and curvature satisfy (Ble dp) and (0<Cle d(dp-B)). Bounded (B) forces zero limiting curvature as (d) tends to zero. At large power the slope magnitude (pd) diverges, which excludes the two-bounded-slope branch. Holding (q) exchanges signs. The revised rows retain the allowed signed tilts, fixed-power edges, and moving-wall trace. The reflected GB2 gamma face and the inverse Burr window are evaluated in their respective coordinates. [Proof with held shapes](2026-11-27-trunc-gb2-math-report.md#complete-varying-power-gb2-and-burr-boundaries)

### Divergence can occur with mass escaping the fitted region

For shifted GB2 take (p=1/2, d=q=1), the sample ((1,2)) and the window ([1,infty)). Set (S=N) and (h=1-N^{-4}). The excess CDF is ((u/(S+u))^{1/2}). The retained mass tends to one, and the conditional probability of every fixed bounded interval tends to zero as (N) grows. The density is (S/(2sqrt{u}(S+u)^{3/2})). The sample excesses are (N^{-4}) and (1+N^{-4}), giving likelihood asymptotic to (N/4). All three shapes are held. The small-scale atom and the fixed-scale subunit spike are valid witnesses, while this diverging sequence carries its mass to infinity. Mass escape is possible even with scale held when tail shape is free. For (S=d=1, p=1/2, q=epsilon, h=1-epsilon^6), the normalizer (B(1/2,epsilon)) is asymptotic to (1/epsilon), the first density is asymptotic to (epsilon^{-2}), the second to (epsilon/2^{1/2}), and the retained mass tends to one. Every fixed bounded interval again loses all its mass. These paths affect the failure classifications, while the divergence witnesses still prove the corresponding nonattainment conditions. [Shifted GB2 proof](2026-11-27-trunc-gb2-math-report.md#shifted-gb2-with-the-shapes-held) · [Pareto II precedent](2026-11-27-mod-rev6-report.md#pareto-ii-can-diverge-with-escaping-mass)

For generalized beta on ([0,2]) with sample ((1/2,1)), take (q=1/2, d=1, p=epsilon, S=1+epsilon^6). The fitted law is supported inside the window for small (epsilon). The beta mean (p/(p+q)) tends to zero, so the fitted law tends to an atom at zero. Since (1/B(p,1/2)sim p), the first density is of order (epsilon) and the second of order (epsilon^{-2}). Their product diverges. With (p=q=1/2) held and (d=epsilon, S=1+epsilon^6), the upper beta factor at the two observations is of order (epsilon^{-1/2}) and (epsilon^{-7/2}), respectively. The Jacobian supplies order (epsilon) at each observation, so the joint likelihood is of order (epsilon^{-2}). The beta coordinate has the same interior beta law at each (epsilon), and (X=S Z^{1/epsilon}) tends to zero almost surely. These examples have neither shrinking retained width nor a regular conditional beta law at the limiting sample maximum. The prior full-window atlas also identifies mixtures along divergent paths. The affected failure rows carry one-sided grades and keep the proved finite-height comparisons. [Generalized-beta limit analysis](2026-11-25-genbeta-limits-math-report.md#3-endpoint-free)

At a held generalized-beta endpoint with (q) free, the (q=1) subsection has a finite pointwise trace, but competing feasible (q<1) gives infinite likelihood. The finite contact condition applies at held (q=1), including for sample ((1,2)) with (S=2). At held (q>1) there is zero likelihood at contact. The CSV makes these cases explicit. It also includes the feasible closed (dp=1) threshold contact in the shifted fixed-shape comparison as well as the fixed-cut join. [Endpoint conventions](2026-11-27-trunc-gb2-math-report.md#moving-support-and-endpoint-conventions)

### Finite heights and equality

The parent GB2 and Burr gamma faces survive as the fixed-power gamma and reciprocal-gamma profiles, with Weibull and inverse Weibull when the surviving beta shape is one. The parent lognormal becomes the bounded-curvature small-power profile, and the double-Pareto becomes the two-piece log tilt. Conditioning adds zero-shape edges, one-sided powers and cutoff locations whose kernels need to normalize only on the retained interval. For transformed gamma the corresponding finite-power shape-zero profile, quadratic small-power profile and hard wall cover the named parent faces. The fixed-scale wall retains its (e^{-j}) factor for (j) contacts. The fixed-scale Burr wall retains (2^{-j}) in its forward coordinate. At a movable wall the full trace is approached by placing an upper wall above the contact, or a lower wall below it, by more than the inverse-power scale. A reduced trace strictly below an approachable full value cannot supply a winning boundary height. In generalized beta the small-power log-distance kernel recovers the held-endpoint log-gamma. Extra endpoint mass in the subunit branch can only lower the distinct-interior sample height relative to the approachable pure power. In shifted GB2 at critical small-scale contact the atom envelope remains a separate sample height even though the weak limiting law is a point mass. [GB2 parent proof](2026-11-27-genbeta-atlas-math-report.md#gb2-exteriors-with-one-beta-shape-held) · [Burr parent proof](2026-11-25-burr-math-report.md#face-comparisons-and-equality) · [Transformed-gamma truncation](2026-11-27-trunc-gb2-math-report.md#transformed-gamma-with-all-coefficients-free) · [Critical atom envelope](2026-11-27-trunc-gb2-math-report.md#shifted-gb2-with-the-shapes-held)

The two-shape tests use the beta or gamma transform only at fixed power and with the stated fixed scale where beta coordinates require it. On distinct data the leading natural-parameter direction has at most one maximizing observation coordinate, which supplies coercivity. The constrained concavity tests at finite edges use the appropriate cut integrals and include zero inward score. In the stationary comparisons an attained candidate tying an excluded height is feasible. Any strictly greater feasible height then confines the supremum to the model after subsequence extraction. The Prentice (Q=0) normal seam is feasible and the positive-variance normal limits reached from nonzero (Q) enter that seam. The normal zero-quadratic tilt remains excluded, and the normal tests cover the variance-held, mean-held and free cases separately. [Truncation theorem](2026-11-27-trunc-expfam-math-report.md#general-theorem) · [Normal criterion](2026-11-27-trunc-expfam-math-report.md#norm-free-truncateknown) · [Joint likelihood envelope](../ml-behaviour.tex)

### Exact sample checks

- **Generalized beta.** With (S=2, q=1), the sample ((1,2)) on the closed imposed window ([0,2]) has likelihood (r^2 2^{-r-1}) in (r=pd). Zero is outside the open parent lower support. The derivative of the log likelihood is (2/r-log 2) and its second derivative is strictly negative. The global optimum (R=2/log 2) is attained at ((p,d)=(1,R)) and ((2,R/2)). The rational check encloses the positive logarithm using its convergent series and verifies both factorizations.
- **Feller–Pareto.** With (p=d=1, q=2), the sample ((1,2)) on ([1,infty)) forces (hle1). For (s=S+1-h>0) the conditional likelihood is (4s/(1+s)^3). The numerator of its gap from (16/27), over (27(1+s)^3), is ((2s-1)^2(4s+16)). The global optimum is reached at ((h,S)=(3/4,1/4)) and ((1,1/2)), and at every feasible pair having (s=1/2). The calculation uses the closed threshold at 1 for the second pair.

Both rows give one-sided refutations under the full coefficient holds named in the CSV. [Exact replay](2026-11-27-mod-rev10-data/validate.py) · [Source examples](2026-11-27-trunc-gb2-math-report.md#moving-support-and-endpoint-conventions)

## Rows not joined

Every proposed key appears in the reviewed CSV. All 103 parent masks exist in the base and modifier atlases, and all are eligible for known-window truncation. No rows were proposed with open status. The CSV is the coordinator's join input.

## Open questions by family

| Family | What remains open |
| --- | --- |
| betapr | Determine the winning set on proper windows when scale is free, and obtain a finite comparison procedure at equality with a fitted face. |
| burr | Bound the number of winning roots with free or partly held power and scale, and resolve equality with the newly included supported tilts. |
| gb2 | Bound the stationary components on proper windows under free scale and power, including simultaneous competition between the two-piece, hard-wall and smooth faces where available. |
| genbeta | Classify normalized divergent limits at singular endpoint contact, establish cardinality on the general comparisons, and isolate finite competitors near a moving wall or fitted log-distance face. |
| gengamma | Determine off-seam and seam winner cardinality, and give a practical comparison of the signed walls and power tilts including equality. |
| trgamma | Bound winning stationary components and provide a finite boundary comparison for moving and held walls, including exact contact losses. |
| invtrgamma | Resolve the corresponding reflected winning sets and finite boundary comparison, retaining contact loss at the reflected cut. |
| invburr | Classify winning roots on the reflected comparison routes and provide effective tie tests against the reflected tilt and wall profiles. |
| invparalogis | Determine cardinality on the reflected scale and shape comparisons and decide equality with the supported tilt, including held-scale contact. |
| paralogis | Determine cardinality when scale or shape varies and provide an effective tie test against the supported tilt. |
| invpareto | Bound the conditional scale or joint reciprocal-Lomax winning set and resolve exact equality with the boundary profile. |
| fpareto | Classify divergent threshold and scale paths with escaping mass, determine the full winning set beyond the explicit Lomax continuum, and isolate candidates near the critical atom envelope. |

## Checks

All computation ran on the workstation through the one-slot lock. The [CSV validator and exact sample replay](2026-11-27-mod-rev10-data/validate.py) and the atlas modifier-proof checker passed. The source report link and fragment checker validated 107 destinations. The source likelihood grid replay reproduced all 36 entries byte for byte, with 60- and 90-digit evaluations agreeing within (10^{-54}). The grid checks formulas numerically. The row grades use the analytic proofs and exact sample reductions.

The repository link checker reports 47 inherited broken destinations and the pre-existing false positive in the beta-prime report at line 461. The baseline run before writing this review reported the same failures.
