# Independent review of known-window truncation for skew and shape-indexed families

Reviewed all **239 proposals** from TASK-458. The [join file](2026-11-27-mod-rev7-data/reviewed.csv) contains **239 rows** (224 accepted and 15 regraded). It contains 81 iff, 106 exact and 52 one-sided rows. 0 rows were rejected.

**The small-df Student failure list needs a qualification.** With location held and scale free, LST and half-t can have unbounded sample likelihood on a ray while all conditional mass escapes. The fixed-scale LST proof also omits the bounded-window tie case. The review retains the attainment comparison, corrects the fixed-scale case, and grades the two free-scale failure rows as one-sided.

The proofs use a closed known window intersected with the parent support, with positive retained mass and inward density values at included moving endpoints. This matters for Tukey support contact and half-t at the sample minimum. Logit-normal observations lie in `(0,1)` and log-gamma observations lie in `(1,∞)`. Both infinite cuts recover the parent regime when they leave its entire support intact. All proposed masks are eligible. Estimated cuts and the zero modifiers fall outside these rows. [TASK-458 proof](2026-11-27-trunc-skew-math-report.md) · [Modifier design](../../design/modifiers.md) · [Atlas conventions](../existence-atlas/README.md)

For the accepted face comparisons I checked the subsequence limits of the joint sample product, including ties at cuts, and the availability of the stated boundary heights under the holds. A finite point above the escape bound confines a maximizing sequence. A finite point equal to the bound either maximizes or provides a starting comparison for a strictly higher finite point. The exact Double Pareto maximizing-set rows use the finite knot reduction and strict concavity in the active rates. The other comparisons can leave the cardinality of the best finite fits open. [Likelihood closure lemma](../ml-behaviour.tex#lem:closure) · [Fixed-power precursor](2026-11-27-trunc-ls-math-report.md#power-tails-and-critical-collapse) · [Conditional moment and constrained-family proofs](2026-11-27-trunc-expfam-math-report.md#scalar-and-two-coordinate-checks)

## Row verdicts

| Family | Held | Question | Verdict | Reason |
| --- | --- | --- | --- | --- |
| ged | all free | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| ged | mean | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| ged | mean+nu | criterion | accepted | The scalar absolute-power moment lies strictly between its projected minimum and the broad-scale endpoint mean. |
| ged | mean+nu | failure | accepted | The scalar tilt concentrates at its unique projected minimum and can reach the broad uniform threshold only on a bounded interval. |
| ged | mean+nu | uniqueness | accepted | The strictly monotone scalar score gives at most one finite scale. |
| ged | mean+nu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| ged | mean+nu+sd | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| ged | mean+nu+sd | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| ged | mean+nu+sd | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| ged | mean+sd | criterion | accepted | The scalar shape interval has the stated boundary values and a finite candidate may attain at equality with the best excluded height. |
| ged | mean+sd | failure | accepted | The shape ends exhaust scalar escape, including contact factors and the divergent or critical endpoint products where applicable. |
| ged | nu | criterion | accepted | The scale and remote-slope bounds exhaust the hold-compatible limits, and a finite point may tie the greatest boundary height. |
| ged | nu | failure | accepted | The holds select the uniform, integrable remote tilt, or projection and cut concentration from the stretched-exponential limits. |
| ged | nu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| ged | nu+sd | criterion | accepted | The scale and remote-slope bounds exhaust the hold-compatible limits, and a finite point may tie the greatest boundary height. |
| ged | nu+sd | failure | accepted | The holds select the uniform, integrable remote tilt, or projection and cut concentration from the stretched-exponential limits. |
| sged | all free | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sged | mean | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sged | mean+nu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sged | mean+nu+sd | criterion | accepted | The two skew ends exhaust the scalar search and the moving join has an explicit sample value for the equality comparison. |
| sged | mean+nu+sd | failure | accepted | The exact moving-join asymptotic accounts for positive overlap, an exterior cut and contact without positive favored overlap. |
| sged | mean+nu+sd+xi | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| sged | mean+nu+sd+xi | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| sged | mean+nu+sd+xi | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| sged | mean+nu+xi | criterion | accepted | The scale and remote-slope bounds exhaust the hold-compatible limits, and a finite point may tie the greatest boundary height. |
| sged | mean+nu+xi | failure | accepted | The holds select the uniform, integrable remote tilt, or projection and cut concentration from the stretched-exponential limits. |
| sged | mean+nu+xi | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sged | mean+sd+xi | criterion | accepted | The scalar shape interval has the stated boundary values and a finite candidate may attain at equality with the best excluded height. |
| sged | mean+sd+xi | failure | accepted | The shape ends exhaust scalar escape, including contact factors and the divergent or critical endpoint products where applicable. |
| sged | mean+xi | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sged | nu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sged | nu+sd+xi | criterion | accepted | The scale and remote-slope bounds exhaust the hold-compatible limits, and a finite point may tie the greatest boundary height. |
| sged | nu+sd+xi | failure | accepted | The holds select the uniform, integrable remote tilt, or projection and cut concentration from the stretched-exponential limits. |
| sged | nu+xi | criterion | accepted | The scale and remote-slope bounds exhaust the hold-compatible limits, and a finite point may tie the greatest boundary height. |
| sged | nu+xi | failure | accepted | The holds select the uniform, integrable remote tilt, or projection and cut concentration from the stretched-exponential limits. |
| sged | nu+xi | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sged | xi | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sn | all free | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sn | alpha | criterion | accepted | The hold-compatible Gaussian or skew-end bounds exhaust escape and allow a finite witness at equality. |
| sn | alpha | failure | accepted | Fixed-skew curvature or the two scalar skew ends bound the joint product even at a closed cut or a half-Gaussian join. |
| sn | alpha | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sn | alpha+omega | criterion | accepted | The hold-compatible Gaussian or skew-end bounds exhaust escape and allow a finite witness at equality. |
| sn | alpha+omega | failure | accepted | Fixed-skew curvature or the two scalar skew ends bound the joint product even at a closed cut or a half-Gaussian join. |
| sn | alpha+omega+xi | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| sn | alpha+omega+xi | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| sn | alpha+omega+xi | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| sn | alpha+xi | criterion | accepted | The hold-compatible Gaussian or skew-end bounds exhaust escape and allow a finite witness at equality. |
| sn | alpha+xi | failure | accepted | Fixed-skew curvature or the two scalar skew ends bound the joint product even at a closed cut or a half-Gaussian join. |
| sn | alpha+xi | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sn | omega+xi | criterion | accepted | The hold-compatible Gaussian or skew-end bounds exhaust escape and allow a finite witness at equality. |
| sn | omega+xi | failure | accepted | Fixed-skew curvature or the two scalar skew ends bound the joint product even at a closed cut or a half-Gaussian join. |
| sn | xi | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| snorm | all free | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| snorm | mean | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| snorm | mean+sd | criterion | accepted | The two skew ends exhaust the scalar search and the moving join has an explicit sample value for the equality comparison. |
| snorm | mean+sd | failure | accepted | The exact moving-join asymptotic accounts for positive overlap, an exterior cut and contact without positive favored overlap. |
| snorm | mean+sd+xi | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| snorm | mean+sd+xi | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| snorm | mean+sd+xi | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| snorm | mean+xi | criterion | accepted | The scale and remote-slope bounds exhaust the hold-compatible limits, and a finite point may tie the greatest boundary height. |
| snorm | mean+xi | failure | accepted | The holds select the uniform, integrable remote tilt, or projection and cut concentration from the stretched-exponential limits. |
| snorm | mean+xi | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| snorm | sd+xi | criterion | accepted | The scale and remote-slope bounds exhaust the hold-compatible limits, and a finite point may tie the greatest boundary height. |
| snorm | sd+xi | failure | accepted | The holds select the uniform, integrable remote tilt, or projection and cut concentration from the stretched-exponential limits. |
| snorm | xi | criterion | accepted | The scale and remote-slope bounds exhaust the hold-compatible limits, and a finite point may tie the greatest boundary height. |
| snorm | xi | failure | accepted | The holds select the uniform, integrable remote tilt, or projection and cut concentration from the stretched-exponential limits. |
| snorm | xi | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| sstd | all free | case:divergent-tie | accepted | The fixed-sd path to df 2 has a negative joint exponent at the retained mean for the stated tie count. |
| sstd | mean | case:divergent-tie | accepted | The fixed-sd path to df 2 has a negative joint exponent at the retained mean for the stated tie count. |
| sstd | mean+nu+sd | criterion | accepted | The two skew ends exhaust the scalar search and the moving join has an explicit sample value for the equality comparison. |
| sstd | mean+nu+sd | failure | accepted | The exact moving-join asymptotic accounts for positive overlap, an exterior cut and contact without positive favored overlap. |
| sstd | mean+nu+sd+xi | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| sstd | mean+nu+sd+xi | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| sstd | mean+nu+sd+xi | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| sstd | mean+nu+xi | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| sstd | mean+nu+xi | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| sstd | mean+sd | case:divergent-tie | accepted | The fixed-sd path to df 2 has a negative joint exponent at the retained mean for the stated tie count. |
| sstd | mean+sd+xi | criterion | accepted | The scalar shape interval has the stated boundary values and a finite candidate may attain at equality with the best excluded height. |
| sstd | mean+sd+xi | failure | accepted | The shape ends exhaust scalar escape, including contact factors and the divergent or critical endpoint products where applicable. |
| sstd | mean+sd+xi | case:divergent-tie | accepted | The fixed-sd path to df 2 has a negative joint exponent at the retained mean for the stated tie count. |
| sstd | mean+xi | case:divergent-tie | accepted | The fixed-sd path to df 2 has a negative joint exponent at the retained mean for the stated tie count. |
| sstd | nu+xi | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| sstd | nu+xi | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| sstd | sd | case:divergent-tie | accepted | The fixed-sd path to df 2 has a negative joint exponent at the retained mean for the stated tie count. |
| sstd | sd+xi | case:divergent-tie | accepted | The fixed-sd path to df 2 has a negative joint exponent at the retained mean for the stated tie count. |
| sstd | xi | case:divergent-tie | accepted | The fixed-sd path to df 2 has a negative joint exponent at the retained mean for the stated tie count. |
| st | alpha+nu | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| st | alpha+nu | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| st | alpha+nu+xi | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| st | alpha+nu+xi | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| lst | all free | criterion | accepted | The finite-df, large-df and allowed small-df cases bound every escape in the finite-supremum regime and equality admits a finite witness. |
| lst | all free | failure | accepted | The df subsequences give the eligible finite faces and the scale and cut tests give the divergent cases under these holds. |
| lst | df | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| lst | df | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| lst | df+mu | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| lst | df+mu | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| lst | df+mu+sigma | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| lst | df+mu+sigma | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| lst | df+mu+sigma | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| lst | df+sigma | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| lst | df+sigma | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| lst | df+sigma | case:nonunique | regraded | Exact bounds place the value at 4 above both the centered fit and the uniform limit, so reflection supplies distinct global maximizers. |
| lst | mu | criterion | regraded | The non-tight small-df paths occur only in the already divergent branch and leave the finite-threshold criterion intact. |
| lst | mu | failure | regraded | The cited finite-height comparison survives but the joint small-df paths with tied observations can diverge without a weak collapse law. |
| lst | sigma | criterion | accepted | The finite-df, large-df and allowed small-df cases bound every escape in the finite-supremum regime and equality admits a finite witness. |
| lst | sigma | failure | accepted | The df subsequences give the eligible finite faces and the scale and cut tests give the divergent cases under these holds. |
| slash | all free | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| slash | all free | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| slash | mu | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| slash | mu | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| slash | sigma | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| slash | sigma | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| slash | sigma | case:nonunique | regraded | The exact Slash product at 4 beats the centered fit and uniform boundary, and symmetry gives distinct global maximizers. |
| huber | all free | criterion | accepted | The finite-center limits and remote slopes of the clipped derivative exhaust escape with the physical holds imposed. |
| huber | all free | failure | accepted | The two clipped-slope coordinates give the hold-compatible regular faces and the steep-slope concentration tests. |
| huber | all free | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| huber | epsilon | criterion | accepted | The finite-center limits and remote slopes of the clipped derivative exhaust escape with the physical holds imposed. |
| huber | epsilon | failure | accepted | The two clipped-slope coordinates give the hold-compatible regular faces and the steep-slope concentration tests. |
| huber | epsilon | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| huber | epsilon+mu | criterion | accepted | The finite-center limits and remote slopes of the clipped derivative exhaust escape with the physical holds imposed. |
| huber | epsilon+mu | failure | accepted | The two clipped-slope coordinates give the hold-compatible regular faces and the steep-slope concentration tests. |
| huber | epsilon+mu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| huber | epsilon+sigma | criterion | accepted | The finite-center limits and remote slopes of the clipped derivative exhaust escape with the physical holds imposed. |
| huber | epsilon+sigma | failure | accepted | The two clipped-slope coordinates give the hold-compatible regular faces and the steep-slope concentration tests. |
| huber | mu | criterion | accepted | The finite-center limits and remote slopes of the clipped derivative exhaust escape with the physical holds imposed. |
| huber | mu | failure | accepted | The two clipped-slope coordinates give the hold-compatible regular faces and the steep-slope concentration tests. |
| huber | mu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| huber | mu+sigma | criterion | accepted | The finite-center limits and remote slopes of the clipped derivative exhaust escape with the physical holds imposed. |
| huber | mu+sigma | failure | accepted | The two clipped-slope coordinates give the hold-compatible regular faces and the steep-slope concentration tests. |
| huber | mu+sigma | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| huber | sigma | criterion | accepted | The finite-center limits and remote slopes of the clipped derivative exhaust escape with the physical holds imposed. |
| huber | sigma | failure | accepted | The two clipped-slope coordinates give the hold-compatible regular faces and the steep-slope concentration tests. |
| huber | sigma | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| ht | all free | criterion | accepted | The finite-df, large-df and allowed small-df cases bound every escape in the finite-supremum regime and equality admits a finite witness. |
| ht | all free | failure | accepted | The df subsequences give the eligible finite faces and the scale and cut tests give the divergent cases under these holds. |
| ht | mu | criterion | accepted | The finite-df, large-df and allowed small-df cases bound every escape in the finite-supremum regime and equality admits a finite witness. |
| ht | mu | failure | regraded | The cited finite-height comparison survives but the joint small-df paths with tied observations can diverge without a weak collapse law. |
| ht | mu+nu | criterion | regraded | A physical left ray can give a bounded half-t interval and a winning uniform even with a subcritical endpoint tie. |
| ht | mu+nu | failure | regraded | A physical left ray can give a bounded half-t interval and a winning uniform even with a subcritical endpoint tie. |
| ht | mu+nu+sigma | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| ht | mu+nu+sigma | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| ht | mu+nu+sigma | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| ht | mu+sigma | criterion | accepted | The finite-df, large-df and allowed small-df cases bound every escape in the finite-supremum regime and equality admits a finite witness. |
| ht | mu+sigma | failure | regraded | The small-df normalizer gives distinct bounded and unbounded tie outcomes, with the same held-scale estimate for the full and half kernels. |
| ht | nu | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| ht | nu | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| ht | nu+sigma | criterion | accepted | The broad ratios, exterior power and joint contact exponent bound every hold-compatible escape and the finite witness may tie the bound. |
| ht | nu+sigma | failure | accepted | Tail domination and the rescaled joint contact product exhaust the positive fixed-power boundary heights under these holds. |
| ht | nu+sigma | case:left-ray-unique | accepted | With fixed scale and df every feasible endpoint below the sample minimum can be increased to improve the likelihood. |
| ht | sigma | criterion | accepted | The finite-df, large-df and allowed small-df cases bound every escape in the finite-supremum regime and equality admits a finite witness. |
| ht | sigma | failure | accepted | The df subsequences give the eligible finite faces and the scale and cut tests give the divergent cases under these holds. |
| tlambda | lambda | criterion | accepted | The three shape branches exhaust the eligible boundaries, and positive-density finite support contacts are included before comparing heights. |
| tlambda | lambda | failure | accepted | The power, logistic, and bounded-support branches account for the allowed exterior limits and zero-width contact. |
| tlambda | lambda+mu | criterion | accepted | The three shape branches exhaust the eligible boundaries, and positive-density finite support contacts are included before comparing heights. |
| tlambda | lambda+mu | failure | accepted | The power, logistic, and bounded-support branches account for the allowed exterior limits and zero-width contact. |
| tlambda | lambda+mu+sigma | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| tlambda | lambda+mu+sigma | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| tlambda | lambda+mu+sigma | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| tlambda | lambda+sigma | criterion | accepted | The three shape branches exhaust the eligible boundaries, and positive-density finite support contacts are included before comparing heights. |
| tlambda | lambda+sigma | failure | accepted | The power, logistic, and bounded-support branches account for the allowed exterior limits and zero-width contact. |
| tlambda | mu | case:divergent-tie | accepted | Holding location permits a sufficiently negative shape for which the tied-sample collapse exponent is strictly negative. |
| tlambda | sigma | case:cut-collapse | accepted | The included shape-one support can slide against either finite cut at fixed scale and the retained width tends to zero. |
| bhatt | all free | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| bhatt | a | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| bhatt | a+mu | criterion | regraded | At the included width zero the sharp endpoint must also allow an interior constant to collapse at the Gaussian center. |
| bhatt | a+mu | failure | regraded | At the included width zero the sharp endpoint must also allow an interior constant to collapse at the Gaussian center. |
| bhatt | a+mu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| bhatt | a+mu+sigma | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| bhatt | a+mu+sigma | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| bhatt | a+mu+sigma | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| bhatt | a+sigma | criterion | accepted | With one free coefficient the scalar ends exhaust escape, and a finite fit may tie the greatest excluded height. |
| bhatt | a+sigma | failure | accepted | The stated scalar ends account for both competing regular faces and sharp or remote concentration under these holds. |
| bhatt | mu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| bhatt | mu+sigma | criterion | accepted | With one free coefficient the scalar ends exhaust escape, and a finite fit may tie the greatest excluded height. |
| bhatt | mu+sigma | failure | accepted | The stated scalar ends account for both competing regular faces and sharp or remote concentration under these holds. |
| emg | all free | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| emg | lambda | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| emg | lambda+mu | criterion | accepted | With one free coefficient the scalar ends exhaust escape, and a finite fit may tie the greatest excluded height. |
| emg | lambda+mu | failure | regraded | The two Gaussian-scale ends exhaust the scalar search and the sharp end retains the half-density factor at the included join, with the unrelated width clause removed. |
| emg | lambda+mu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| emg | lambda+mu+sigma | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| emg | lambda+mu+sigma | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| emg | lambda+mu+sigma | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| emg | lambda+sigma | criterion | accepted | With one free coefficient the scalar ends exhaust escape, and a finite fit may tie the greatest excluded height. |
| emg | lambda+sigma | failure | regraded | The fixed-rate left tail becomes exponential and the rightward location limit drives the inward cut slope to infinity, with the unrelated width clause removed. |
| emg | mu | case:endpoint-pair | accepted | The strict endpoint chord bound applies at every admissible finite parameter in this scope and a broad scale or allowed vanishing threshold reaches the uniform height. |
| emg | mu+sigma | criterion | accepted | With one free coefficient the scalar ends exhaust escape, and a finite fit may tie the greatest excluded height. |
| emg | mu+sigma | failure | regraded | The two rate ends follow from the normal convolution limit and integrability of the CDF kernel on the specified windows, with the unrelated width clause removed. |
| logitnorm | all free | criterion | accepted | The strictly monotone transform preserves the optimizing coefficients and the cited conditional moment tests give an exact threshold. |
| logitnorm | all free | failure | accepted | The transformed carrier or normal constraints give the excluded moment faces and concentration at the applicable extremum. |
| logitnorm | all free | uniqueness | accepted | The transformed free statistics have positive definite covariance in the relevant parameter coordinates. |
| logitnorm | all free | case:uniform-logit | accepted | The two transformed cut observations have zero tilt mean and greater variance than the uniform on the logit interval. |
| logitnorm | mu | criterion | accepted | The strictly monotone transform preserves the optimizing coefficients and the cited conditional moment tests give an exact threshold. |
| logitnorm | mu | failure | accepted | The transformed carrier or normal constraints give the excluded moment faces and concentration at the applicable extremum. |
| logitnorm | mu | uniqueness | accepted | The transformed free statistics have positive definite covariance in the relevant parameter coordinates. |
| logitnorm | mu+sigma | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| logitnorm | mu+sigma | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| logitnorm | mu+sigma | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| logitnorm | sigma | criterion | accepted | The strictly monotone transform preserves the optimizing coefficients and the cited conditional moment tests give an exact threshold. |
| logitnorm | sigma | failure | accepted | The transformed carrier or normal constraints give the excluded moment faces and concentration at the applicable extremum. |
| logitnorm | sigma | uniqueness | accepted | The transformed free statistics have positive definite covariance in the relevant parameter coordinates. |
| dblpareto | all free | criterion | accepted | The held-kink rate rectangles and, where needed, the exact kink reduction yield component suprema that a finite winner may tie. |
| dblpareto | all free | failure | accepted | Zero and infinite rates and the vanishing-normalizer cases exhaust positive failure heights in the competing components. |
| dblpareto | all free | uniqueness | accepted | Strict concavity identifies each component's active rates, and the exact kink reduction or base profile recovers all winning plateaus and inactive rates. |
| dblpareto | left_rate | criterion | accepted | The held-kink rate rectangles and, where needed, the exact kink reduction yield component suprema that a finite winner may tie. |
| dblpareto | left_rate | failure | accepted | Zero and infinite rates and the vanishing-normalizer cases exhaust positive failure heights in the competing components. |
| dblpareto | left_rate | uniqueness | accepted | Strict concavity identifies each component's active rates, and the exact kink reduction or base profile recovers all winning plateaus and inactive rates. |
| dblpareto | left_rate+right_rate | criterion | accepted | The held-kink rate rectangles and, where needed, the exact kink reduction yield component suprema that a finite winner may tie. |
| dblpareto | left_rate+right_rate | failure | accepted | Zero and infinite rates and the vanishing-normalizer cases exhaust positive failure heights in the competing components. |
| dblpareto | left_rate+right_rate | uniqueness | regraded | Strict convexity inside a gap needs a finite cut, and on the whole line balancing the two held-rate slopes gives interior plateaus. |
| dblpareto | left_rate+right_rate | case:nonunique | regraded | The exact kernel-ratio inequality proves the entire maximizing plateau for the proposed sample. |
| dblpareto | left_rate+right_rate+scale | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| dblpareto | left_rate+right_rate+scale | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| dblpareto | left_rate+right_rate+scale | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| dblpareto | left_rate+scale | criterion | accepted | The held-kink rate rectangles and, where needed, the exact kink reduction yield component suprema that a finite winner may tie. |
| dblpareto | left_rate+scale | failure | accepted | Zero and infinite rates and the vanishing-normalizer cases exhaust positive failure heights in the competing components. |
| dblpareto | left_rate+scale | uniqueness | accepted | Strict concavity identifies each component's active rates, and the exact kink reduction or base profile recovers all winning plateaus and inactive rates. |
| dblpareto | right_rate | criterion | accepted | The held-kink rate rectangles and, where needed, the exact kink reduction yield component suprema that a finite winner may tie. |
| dblpareto | right_rate | failure | accepted | Zero and infinite rates and the vanishing-normalizer cases exhaust positive failure heights in the competing components. |
| dblpareto | right_rate | uniqueness | accepted | Strict concavity identifies each component's active rates, and the exact kink reduction or base profile recovers all winning plateaus and inactive rates. |
| dblpareto | right_rate+scale | criterion | accepted | The held-kink rate rectangles and, where needed, the exact kink reduction yield component suprema that a finite winner may tie. |
| dblpareto | right_rate+scale | failure | accepted | Zero and infinite rates and the vanishing-normalizer cases exhaust positive failure heights in the competing components. |
| dblpareto | right_rate+scale | uniqueness | accepted | Strict concavity identifies each component's active rates, and the exact kink reduction or base profile recovers all winning plateaus and inactive rates. |
| dblpareto | scale | criterion | accepted | The held-kink rate rectangles and, where needed, the exact kink reduction yield component suprema that a finite winner may tie. |
| dblpareto | scale | failure | accepted | Zero and infinite rates and the vanishing-normalizer cases exhaust positive failure heights in the competing components. |
| dblpareto | scale | uniqueness | accepted | Strict concavity identifies each component's active rates, and the exact kink reduction or base profile recovers all winning plateaus and inactive rates. |
| lgamma | all free | criterion | accepted | The strictly monotone transform preserves the optimizing coefficients and the cited conditional moment tests give an exact threshold. |
| lgamma | all free | failure | accepted | The transformed carrier or normal constraints give the excluded moment faces and concentration at the applicable extremum. |
| lgamma | all free | uniqueness | accepted | The transformed free statistics have positive definite covariance in the relevant parameter coordinates. |
| lgamma | all free | case:corner | accepted | On the gamma scale the data (1,4) match the corner log moment and strictly exceed the corner arithmetic mean. |
| lgamma | ratelog | criterion | accepted | The strictly monotone transform preserves the optimizing coefficients and the cited conditional moment tests give an exact threshold. |
| lgamma | ratelog | failure | accepted | The transformed carrier or normal constraints give the excluded moment faces and concentration at the applicable extremum. |
| lgamma | ratelog | uniqueness | accepted | The transformed free statistics have positive definite covariance in the relevant parameter coordinates. |
| lgamma | ratelog+shapelog | criterion | accepted | The supplied vector is the finite fit whenever the sample is compatible and the retained event has positive mass. |
| lgamma | ratelog+shapelog | failure | accepted | A compatible all-held model has no coefficient along which to escape. |
| lgamma | ratelog+shapelog | uniqueness | accepted | All parent coefficients are supplied so the compatible positive-likelihood feasible set consists of one vector. |
| lgamma | shapelog | criterion | accepted | The strictly monotone transform preserves the optimizing coefficients and the cited conditional moment tests give an exact threshold. |
| lgamma | shapelog | failure | accepted | The transformed carrier or normal constraints give the excluded moment faces and concentration at the applicable extremum. |
| lgamma | shapelog | uniqueness | accepted | The transformed free statistics have positive definite covariance in the relevant parameter coordinates. |

## Proof errors and scope corrections

### Student small df

The final small-df tie calculation is labelled HT in the source. It extends to LST with both location and scale held. Write $v$ for df, $c$ for the held center and $s=\sigma\sqrt v$. The unnormalized kernel is $(s^2+(x-c)^2)^{-(1+v)/2}$. If $c$ belongs to a positive-width bounded effective interval, each side of positive length contributes a normalizer of order $\log(1/s)$ by integrating from distance $s$ to a fixed positive distance and using $s^{-v}\to1$. The numerator at a tie is of order $1/s$ and at every other observation it has a positive finite limit. Any ties therefore give an infinite supremum with a weak atom at $c$. If an effective ray survives, at least one tail contributes order $1/v$ to the normalizer, so the centered density is of order $\sqrt v$ and the other sample densities are of order $v$. The same bounds apply to two rays. The product tends to zero. For a strictly exterior held center on a bounded interval, the inverse-distance kernel follows by dominated convergence. For retained centers without ties on bounded windows the diverging normalizer gives zero product. Together with the large-df normal limit these exhaust the held-scale and held-location scalar cases. For instance, LST with $c=0$, $\sigma=1$ and data $(0,1)$ on $[0,1]$ diverges as $v\downarrow0$ at the stated holds. [Small-df argument](2026-11-27-trunc-skew-math-report.md#free-degrees-of-freedom-in-lst-and-ht)

For free scale on $[0,\infty)$, hold $c=0$, take the exact sample $(0,1)$ and choose $s=\exp(-1/\sqrt v)$, so $\sigma=s/\sqrt v$. This works for both LST and HT. The normalizer is asymptotic to $s^{-v}/v$ since after rescaling by $s$ the integrand beyond 1 differs from $u^{-1-v}$ by a term bounded by a constant times $u^{-3-v}$. The sample product is asymptotic to $v^2 s^{v-1}$ and diverges. For fixed $B>0$ the mass in $[0,B]$ is asymptotic at the additive scale to $1-(B/s)^{-v}$, by integrating the same tail with an error of order $v$. It tends to zero. More generally $s=\exp(-A/v)$ for fixed $A>0$ gives limit $1-e^{-A}$ for the mass in each such interval and still gives a divergent product. The maximizing paths can therefore be non-tight even when the fixed-df witness for the same sample is a point-mass collapse. This gap changes the failure grade for the two held-location, free-scale rows. It leaves the iff criterion intact because the source already proves divergence for these samples at an admissible fixed low df. [Joint likelihood and weak-limit distinction](../ml-behaviour.tex#lem:closure)

### Half-t effective window

The fixed-power statement about guaranteed attainment with a subcritical tie on a ray needs the **effective** window to be unbounded. Take physical window $(-\infty,1]$, the sample $(0,1)$, held $\mu=0$ and held $\nu=3$. Then $k=1<\nu(n-k)$ but the effective interval is $[0,1]$. For the Cauchy kernel $h(x)=(s^2+x^2)^{-1}$ the interval-average to endpoint-geometric-mean ratio is $\delta/\sin\delta>1$, where $\delta=\arctan(1/s)>0$. Jensen for the square gives the strict inequality also for the df-3 kernel $h^2$. Thus every finite scale loses to the broad-scale uniform and the supremum is one. The fixed-power face derivation already includes this effective uniform. The reviewed rows make the scope and its consequence explicit. [Fixed power limits](2026-11-27-trunc-skew-math-report.md#fixed-power-shape) · [Student endpoint inequality](2026-11-27-trunc-ls-math-report.md#std-nu-truncateknown)

### Bhattacharjee at zero width

The scalar sharp-scale description in the convolution proof starts at positive width. The included width $a=0$ gives the retained normal exactly. With center held, the sufficient statistic is $(x-\mu)^2$ and the tilt is $-1/(2\sigma^2)$. The moment must lie strictly between the projected minimum and the uniform mean, with infinite upper threshold on an unbounded interval. In particular a constant at an interior held center diverges as scale decreases. For positive width the proof's edge factors and cut-collapse limits apply. The EMG failure rows had a Bhattacharjee width clause copied into them. Their corrected claims give the respective EMG limits directly. [Convolution proofs](2026-11-27-trunc-skew-math-report.md#convolution-kernels) · [Held-center normal](2026-11-27-trunc-expfam-math-report.md#norm-mean-truncateknown)

### Double Pareto on the whole line

The strict gap convexity proof with rates held needs at least one finite cut on the log scale. With no cut the normalizing constant is independent of the kink, and each gap between knots has log-likelihood slope $\beta(n-k)-\alpha k$ for $k$ data to its left. A zero slope between winning knots gives the entire gap. For example, rates 1 and 1 with sample $(1,4)$ yield all kink scales in $[1,4]$ when the physical window retains all positive reals. The base scan supplies this part of the exact claim. With a genuine cut, strict gap convexity rules out this type of interior plateau and an exterior plateau can still maximize. [Truncated knot reduction](2026-11-27-trunc-skew-math-report.md#double-pareto-on-the-log-scale) · [Base equality rule](2026-11-25-dblpareto-math-report.md#all-eight-masks)

### Edge formulas and exact examples

In the accessible GED and SGED shape path, the expression for the lower edge should read $c_\infty-\sqrt3\,s_{-,\infty}$. The shape-only endpoint factor is still correct. Expanding the gamma ratios gives $\log C_p=-(p/2)\log 3-\gamma_E+O(1/p)$, while the first-order terms cancel in the standardized first absolute moment. The moving SGED edges consequently shift by $O(p^{-2})$ at fixed skew. For skew-only two-piece paths the direction of the kink motion is essential. The shrinking-side standardized displacement tends to $m_1/[2(1-m_1^2)]$ at the nominal join, which gives the stated contact factor. The SN half-Gaussian join instead has factor $1/2$ with center and scale held. [Two-piece shape and skew paths](2026-11-27-trunc-skew-math-report.md#skew-only-two-piece-envelopes) · [The shape-only endpoints](2026-11-27-trunc-skew-math-report.md#shape-only-ged-and-standardized-student) · [SN scalar limits](2026-11-27-trunc-skew-math-report.md#azzalini-skew-normal)

For the LST and Slash examples the window is symmetric, the likelihood is even in location, and the translated normalizer decreases strictly from location 0 to 4 because each radial kernel decreases strictly with distance. At 4 the exact products beat the centered products and the broad uniform threshold. Compactness then gives a global maximizer away from zero, and reflection gives another. For Double Pareto at rates 1 and 1, the log of the kernel relative to its value at zero differs from $-y$ by $c-|y-c|+y$, where $c$ is the log kink. This is zero for $c\le0$ and equals $2\min(y,c)$ for $y\ge0$, $c>0$. Integrating over log window $[0,1]$ proves the whole maximizing plateau for the constant sample. The result at each sample is one-sided cardinality coverage. [Fixed-power witnesses](2026-11-27-trunc-skew-math-report.md#fixed-power-shape) · [Plateau witness](2026-11-27-trunc-skew-math-report.md#double-pareto-on-the-log-scale) · [Exact checks](2026-11-27-mod-rev7-data/exact_checks.py)

## Rows not joined

Every proposed key is represented in the merge input. No proposed key failed the parent, modifier or evidence checks.

## Open by family

The masks below lack the indicated coverage in this join file. Named cases remain available for their stated samples.

| Family | Attainment still open | Exhaustive failure list still open | Exact maximizing set still open |
| --- | --- | --- | --- |
| bhatt | a, all free, mu, sigma | a, all free, mu, sigma | a, a+mu, a+sigma, all free, mu, mu+sigma, sigma |
| dblpareto | None | None | None |
| emg | all free, lambda, mu, sigma | all free, lambda, mu, sigma | all free, lambda, lambda+mu, lambda+sigma, mu, mu+sigma, sigma |
| ged | all free, mean, sd | all free, mean, sd | all free, mean, mean+sd, nu, nu+sd, sd |
| ht | None | mu | all free, mu, mu+nu, mu+sigma, nu, nu+sigma, sigma |
| huber | None | None | all free, epsilon, epsilon+mu, epsilon+sigma, mu, mu+sigma, sigma |
| lgamma | None | None | None |
| logitnorm | None | None | None |
| lst | None | mu | all free, df, df+mu, df+sigma, mu, sigma |
| sged | all free, mean, mean+nu, mean+sd, mean+xi, nu, nu+sd, sd, sd+xi, xi | all free, mean, mean+nu, mean+sd, mean+xi, nu, nu+sd, sd, sd+xi, xi | all free, mean, mean+nu, mean+nu+sd, mean+nu+xi, mean+sd, mean+sd+xi, mean+xi, nu, nu+sd, nu+sd+xi, nu+xi, sd, sd+xi, xi |
| slash | None | None | all free, mu, sigma |
| sn | all free, omega, xi | all free, omega, xi | all free, alpha, alpha+omega, alpha+xi, omega, omega+xi, xi |
| snorm | all free, mean, sd | all free, mean, sd | all free, mean, mean+sd, mean+xi, sd, sd+xi, xi |
| sstd | all free, mean, mean+nu, mean+sd, mean+xi, nu, nu+sd, sd, sd+xi, xi | all free, mean, mean+nu, mean+sd, mean+xi, nu, nu+sd, sd, sd+xi, xi | all free, mean, mean+nu, mean+nu+sd, mean+nu+xi, mean+sd, mean+sd+xi, mean+xi, nu, nu+sd, nu+xi, sd, sd+xi, xi |
| st | all free, alpha, alpha+xi, nu, nu+xi, omega, xi | all free, alpha, alpha+xi, nu, nu+xi, omega, xi | all free, alpha, alpha+nu, alpha+nu+xi, alpha+xi, nu, nu+xi, omega, xi |
| tlambda | all free, mu, sigma | all free, mu, sigma | all free, lambda, lambda+mu, lambda+sigma, mu, sigma |

For the missing GED and SGED attainment and failure masks, the main gap is a uniform bound at moving support edges as shape grows, jointly with compression when skew is free. For SN and snorm the corresponding gap is coupled skew and location or scale, including wrong-side conditioning and moving joins. For sstd and ST the open cases require joint shape and skew limits, with critical tie products near df 2 for sstd and skew contact for ST. A scalar ST skew end with positive favored overlap or positive distance into the wrong side does not cover contact at the join.

For LST and HT the finite comparison leaves a finite-root and tie-classification problem. The held-location small-df divergent branch also needs a classification of every escape law and its sample product. Slash has an exhaustive attainment test and an exact reflected tie example, with the general maximizing set open. Huber requires a finite maximizing-set analysis across quadratic, linear and mixed pieces, including plateaus. For free-shape Tukey lambda the moving support and the shape transition around zero and one still need joint bounds. The fixed-shape uniform member has the explicit retained-width rule. For Bhattacharjee and EMG with two or more coefficients free, the missing limits involve Gaussian smoothing jointly with moving component edges or vanishing retained mass. Conditional scalar and transformed moment tests already cover every proposed logit-normal and log-gamma mask. The Double Pareto component proofs cover the entire maximizing set, with numerical equality decisions still a separate implementation question.

## Checks

The workstation ran the [exact checks](2026-11-27-mod-rev7-data/exact_checks.py), the [join-file validator](2026-11-27-mod-rev7-data/validate.py), and the repository modifier validator. An isolated copy of the source [numerical replay](2026-11-27-trunc-skew-math-data/check.py) passed all 68 checks at 80 digits. The replay corroborates specific paths. The proof grades depend on the analytic limits and exact example inequalities. The repository modifier validator accepted all 239 rows. The Markdown link check reported 47 inherited findings and exited 1 for the known false positive at line 461 of the beta-prime report, where it parses `P+Q` as a destination. It found no broken destinations in the review report. The Batteries README did not appear in this worktree’s output. This review involved no builds.
