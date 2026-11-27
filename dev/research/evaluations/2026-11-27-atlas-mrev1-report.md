# Independent review of Tukey lambda, skew GED and skew Student atlas rows

TASK-409 · 2026-11-27

The review covers **116 proposals**. The join file contains **114 rows**. It keeps 103 proposals at their submitted grade, regrades 11 and omits two skew-t uniqueness rows whose counterexample uses the wrong likelihood. All 114 joined rows have written evidence. [Reviewed rows](2026-11-27-atlas-mrev1-data/reviewed.csv)

| Family | Reviewed | Accepted | Regraded | Rejected | Joined `iff` | Joined `exact` | Joined `one-sided` |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| tlambda | 15 | 15 | 0 | 0 | 5 | 6 | 4 |
| sged | 36 | 36 | 0 | 0 | 12 | 12 | 12 |
| ged | 3 | 3 | 0 | 0 | 0 | 0 | 3 |
| st | 23 | 21 | 0 | 2 | 7 | 7 | 7 |
| sstd | 39 | 28 | 11 | 0 | 14 | 14 | 11 |
| **Total** | **116** | **103** | **11** | **2** | **38** | **39** | **37** |

A finite point at the largest boundary envelope is sufficient for attainment in the comparison criteria. It is either already a maximizer or has a strictly better finite competitor, in which case compactness supplies a finite maximum. This uses the joint sample envelope, including the appropriate support-contact penalties. The review kept that mathematical criterion distinct from an effective procedure for deciding exact equality. For fixed-shape skew Student, the finite root and component results suffice for one-sided cardinality rows under the requested grading rule. The exact examples of nonuniqueness have the same grade, even where the example's full maximizing set is known. [Atlas conventions](../existence-atlas/overview.md)

## Row verdicts

| Family | Held | Question | Verdict | Reason |
| --- | --- | --- | --- | --- |
| tlambda | lambda | criterion | accepted | The collapse exponents and strict collar reduce the critical case to a compact comparison that retains finite equality. |
| tlambda | lambda | failure | accepted | The tail and modal bounds give sharp zero, finite and infinite scale-zero envelopes and exclude infinite location or scale. |
| tlambda | lambda | uniqueness | accepted | The shape-five witness beats the best centered fit and reflection gives distinct global maxima, with separate strictness proofs on the singleton window. |
| tlambda | lambda+mu | criterion | accepted | The fixed-location critical face strictly dominates every finite scale and the other cases follow from the exponent or support bound. |
| tlambda | lambda+mu | failure | accepted | Fixing location leaves only the stated scale-zero envelopes, including a finite limit at zero exponent. |
| tlambda | lambda+mu | uniqueness | accepted | The inverse-scale or scale monotonicity proofs cover nonnegative shapes and the strictly decreasing scale score covers every attaining negative shape. |
| tlambda | lambda+sigma | criterion | accepted | The closed support interval gives the stated strict or weak width test and infinite location contributes zero. |
| tlambda | lambda+sigma | failure | accepted | The fixed scale excludes collapse and positive support feasibility is the only possible obstruction. |
| tlambda | lambda+sigma | uniqueness | accepted | Strict convexity on the feasible interval and reflection give exactly the two positive-density contacts. |
| tlambda | mu | criterion | accepted | Joint tail bounds give a compact superlevel set without ties and a fixed sufficiently negative shape diverges with any tie. |
| tlambda | mu | failure | accepted | The joint scale and shape bounds exhaust the corners, including infinity at the finite critical shape when shape may vary. |
| tlambda | mu | uniqueness | accepted | Uniform compactness, a strict uniform collar and opposite profile signs define a real crossing and give two finite global fits. |
| tlambda | sigma | criterion | accepted | The shape interval is closed, infinite location gives zero and a positive logistic witness ensures attainment. |
| tlambda | sigma | failure | accepted | The uniform upper bound controls infinite location and all finite contacts use the included endpoint convention. |
| tlambda | sigma | uniqueness | accepted | The uniform plateau follows from the strict mode bound and the prior far-residual bound plus local strict curvature gives the isolated reflected pair. |
| sged | all free | criterion | accepted | The uniform and optimized half-GED benchmarks exhaust the nonconstant fitted-domain boundary and the witness rule includes equality. |
| sged | all free | failure | accepted | Scale collapse is confined to constants and the joint uniform and half-GED envelopes exhaust all positive ends. |
| sged | all free | uniqueness | accepted | A strict finite perturbation beats both half orientations, the uniform and every reflection-fixed fit. |
| sged | mean | criterion | accepted | The moment-constrained half ends and centered uniform exhaust the boundary for nonzero residuals, with equality admitted. |
| sged | mean | failure | accepted | The mean constraint pins collapse and varying scale preserves the half contact without a penalty. |
| sged | mean | uniqueness | accepted | The constrained Laplace perturbation strictly beats the reflection-fixed upper bound and all faces. |
| sged | mean+nu | criterion | accepted | Fixed shape removes the uniform and the two held-mean halves have sharp joint values, with equality admitted. |
| sged | mean+nu | failure | accepted | Only all-zero-residual collapse and the two finite half ends survive and varying scale realizes full half-contact height. |
| sged | mean+nu | uniqueness | accepted | The shape-one perturbation respects the held mean and strictly beats the faces and the symmetric profile. |
| sged | mean+nu+sd | criterion | accepted | Only skew can escape and the two sharp thin-side endpoint traces give the exact finite-witness benchmark. |
| sged | mean+nu+sd | failure | accepted | The held moments force the displayed thin-side ratio at each tie and the two skew ends exhaust the boundary. |
| sged | mean+nu+sd | uniqueness | accepted | The scalar stationary equation decreases strictly and its sole interior squared-skew root gives the reciprocal pair. |
| sged | mean+sd | criterion | accepted | The fixed-width uniform and finite half ends exhaust the boundary and a finite equality witness attains. |
| sged | mean+sd | failure | accepted | Finite-shape motion recovers full half-contact height and the joint uniform contact penalty follows from the gamma expansion. |
| sged | mean+sd | uniqueness | accepted | The Laplace perturbation preserves both held moments and strictly dominates all faces and the reflection-fixed set. |
| sged | mean+sd+xi | criterion | accepted | Strict uniform containment gives a finite improvement, strict excess makes the uniform infeasible and contact uses the sharp trace. |
| sged | mean+sd+xi | failure | accepted | Only shape varies and only the exact fixed-width contact can prevent attainment, with the proved joint endpoint penalty. |
| sged | mean+sd+xi | uniqueness | accepted | At unit skew the earlier validated GED shape cover and continuity give separated global maxima on the same held domain. |
| sged | mean+xi | criterion | accepted | The centered uniform is the sole finite boundary benchmark off collapse and equality requires a finite witness. |
| sged | mean+xi | failure | accepted | Fixed skew precludes a half end and slight scale inflation realizes the unpenalized uniform trace. |
| sged | mean+xi | uniqueness | accepted | The validated GED bulk-plus-outlier crossing transfers exactly at unit skew. |
| sged | nu | criterion | accepted | Only fixed-shape half faces and constant collapse remain and moving the join to the supporting extreme sharpens each benchmark. |
| sged | nu | failure | accepted | The fixed-shape half contacts are realized at the extreme with full height and only constants can collapse. |
| sged | nu | uniqueness | accepted | The strict reflected comparison survives holding shape one. |
| sged | nu+sd | criterion | accepted | Only fixed-shape half benchmarks remain, with prescribed width and joins at the corresponding extremes. |
| sged | nu+sd | failure | accepted | Holding sd excludes collapse and the strict half-peak comparison also resolves constants. |
| sged | nu+sd | uniqueness | accepted | The feasible reflected perturbation preserves shape one and the specified sd while beating every excluded face. |
| sged | sd | criterion | accepted | The two half profiles and a sharp fixed-width uniform trace provide an exhaustive finite-witness comparison. |
| sged | sd | failure | accepted | The uniform contact calculation gives an additive log penalty and strict containment rules out a uniform winner. |
| sged | sd | uniqueness | accepted | The held-sd perturbation dominates the symmetric benchmark and the excluded faces, with uniform support infeasible. |
| sged | sd+xi | criterion | accepted | The held-weight uniform trace is the only benchmark and off-contact cases are settled by strict improvement or infeasibility. |
| sged | sd+xi | failure | accepted | Only the shape-infinite uniform can prevent attainment and the optimized two-extreme penalty uses both counts and fixed weights. |
| sged | sd+xi | uniqueness | accepted | The small-sd GED Laplace theorem gives the full median interval and transfers at unit skew. |
| sged | xi | criterion | accepted | Scale collapse and the range uniform exhaust the boundary, and the finite-witness rule includes equality. |
| sged | xi | failure | accepted | Fixed skew excludes half ends and inflation realizes full uniform contact height when scale is free. |
| sged | xi | uniqueness | accepted | The prior interval-backed GED shape comparison gives the nontrivial exact Laplace median interval at unit skew. |
| ged | mean | uniqueness | accepted | The interval inequalities separate two compact shape branches from the valley and uniform, and continuity gives an exact real tie. |
| ged | mean+sd | uniqueness | accepted | The validated shape cover excludes the intervening and outer regions and the envelopes cross on the exact radius band. |
| ged | sd | uniqueness | accepted | The small-sd Laplace theorem forces shape one and the conditional absolute loss is flat exactly on the median interval. |
| st | all free | uniqueness | accepted | The rational finite witness beats every escape and the reflection-fixed family, so any global winner has a distinct reflection. |
| st | alpha | criterion | accepted | The skew-normal and critical displacement values exhaust the bounded boundary and the score test includes the fitted df floor. |
| st | alpha | failure | accepted | Held slant removes the half faces and the collapse calculation includes strict, critical and divergent atom cases. |
| st | alpha | uniqueness | accepted | The fixed-df Student uniqueness theorem applies at zero slant and the exact tail bounds force the sample winner to df one. |
| st | alpha+nu | criterion | accepted | Fixed slant and df leave only the atom exponent and optimized displacement layer, including the equality comparison. |
| st | alpha+nu | failure | accepted | The tail expansion gives the sharp collapse envelope and all other ends vanish. |
| st | alpha+nu | uniqueness | accepted | The Cauchy product identity gives the entire upper semicircle and its endpoints tie only in the closure. |
| st | alpha+nu+xi | uniqueness | rejected | The displayed objective has an extra factor of scale and the correct likelihood strictly prefers the first branch throughout the stated band. |
| st | alpha+xi | criterion | accepted | Only normal and held-location collapse remain and the finite score test must include the fitted df floor. |
| st | alpha+xi | failure | accepted | The moment-free held-location expansion gives the exact critical factor and held slant removes the half ends. |
| st | alpha+xi | uniqueness | rejected | The fixed-df crossing used to infer the free-df tie fails for the actual likelihood. |
| st | nu | criterion | accepted | The two fixed-df half faces and sharp critical collapse values exhaust the bounded comparison and equality is included. |
| st | nu | failure | accepted | The proof controls the moving half contacts and all zero-scale slant layers at fixed df. |
| st | nu | uniqueness | accepted | Holding df one preserves the strict rational witness, boundary separation and reflection. |
| st | nu+xi | criterion | accepted | Positive exponent and mixed signs eliminate all boundary competitors, while critical scale monotonicity and one-sign slant monotonicity exclude the other cases. |
| st | nu+xi | failure | accepted | The collapse exponent decides the small-scale behavior and a compatible half-t can win at positive exponent with held contacts. |
| st | nu+xi | uniqueness | accepted | Complementary skew factors force zero slant on paired residuals and the strictly decreasing scale score gives one root. |
| st | omega | criterion | accepted | Holding scale removes collapse and the positive-scale normal and half faces exhaust the score comparison, including the fitted df floor. |
| st | omega | failure | accepted | The positive-scale contact paths attain both half-t and half-normal envelopes and infinite location is zero. |
| st | omega | uniqueness | accepted | The rational reflected witness remains feasible at the held scale and is separated from the faces and reflection-fixed set. |
| st | xi | criterion | accepted | The fixed-location faces and collapse exhaust the boundary and the finite score test includes the fitted df floor. |
| st | xi | failure | accepted | The collapse layer fixes displacement at zero and half contacts retain their held-location factors. |
| st | xi | uniqueness | accepted | Complementary skew factors force zero slant and the centered exact bounds force df one with a unique scale. |
| sstd | all free | criterion | accepted | The joint envelope reduces to the two-piece normal, t2 and half corners plus atom collapse, and a finite score-zero equality witness suffices. |
| sstd | all free | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the two-piece normal, t2 and half corners plus atom collapse. |
| sstd | all free | uniqueness | regraded | The exact t2 box cover and outward interval witness exclude all faces and the centered unit-skew subset. |
| sstd | mean | criterion | accepted | The joint envelope reduces to the held-mean normal and t2, half contacts of full height and collapse at the held mean, and a finite score-zero equality witness suffices. |
| sstd | mean | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the held-mean normal and t2, half contacts of full height and collapse at the held mean. |
| sstd | mean | uniqueness | regraded | The same strict witness has mean zero and the constrained boundary is bounded by the all-free boundary. |
| sstd | mean+nu | criterion | accepted | The joint envelope reduces to the two held-mean fixed-shape halves and collapse at the held mean, and a finite score-zero equality witness suffices. |
| sstd | mean+nu | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the two held-mean fixed-shape halves and collapse at the held mean. |
| sstd | mean+nu | uniqueness | regraded | The witness satisfies mean zero and shape 31 and remains strictly above the constrained boundary and reflection-fixed set. |
| sstd | mean+nu+sd | criterion | accepted | The joint envelope reduces to the two moment-constrained half faces with forced thin-side contact ratios, and a finite score-zero equality witness suffices. |
| sstd | mean+nu+sd | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the two moment-constrained half faces with forced thin-side contact ratios. |
| sstd | mean+nu+sd | uniqueness | regraded | The witness preserves all three holds and the strict all-free separation still applies. |
| sstd | mean+nu+xi | criterion | accepted | The joint envelope reduces to only held-mean collapse, with exponent and tied broad-side correction, and a finite score-zero equality witness suffices. |
| sstd | mean+nu+xi | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving only held-mean collapse, with exponent and tied broad-side correction. |
| sstd | mean+nu+xi | uniqueness | regraded | Clearing denominators gives finitely many scale-score roots, and ranking the roots is only a one-sided cardinality result under the review rule. |
| sstd | mean+sd | criterion | accepted | The joint envelope reduces to the held-moment normal, moving half contacts, penalized half-normal contacts and lower-shape collapse, and a finite score-zero equality witness suffices. |
| sstd | mean+sd | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the held-moment normal, moving half contacts, penalized half-normal contacts and lower-shape collapse. |
| sstd | mean+sd | uniqueness | regraded | The strict reflected witness satisfies both moment holds and the constrained faces are bounded by the all-free faces. |
| sstd | mean+sd+xi | criterion | accepted | The joint envelope reduces to the hold-compatible normal and lower-shape collapse at the held mean, and a finite score-zero equality witness suffices. |
| sstd | mean+sd+xi | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the hold-compatible normal and lower-shape collapse at the held mean. |
| sstd | mean+xi | criterion | accepted | The joint envelope reduces to the held-skew and held-mean normal and t2 profiles and collapse, and a finite score-zero equality witness suffices. |
| sstd | mean+xi | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the held-skew and held-mean normal and t2 profiles and collapse. |
| sstd | nu | criterion | accepted | The joint envelope reduces to the two held-shape halves at sample extremes and collapse at sample atoms, and a finite score-zero equality witness suffices. |
| sstd | nu | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the two held-shape halves at sample extremes and collapse at sample atoms. |
| sstd | nu | uniqueness | regraded | Holding shape 31 preserves the strict witness and both reflected maxima. |
| sstd | nu+sd | criterion | accepted | The joint envelope reduces to the two held-shape, held-sd halves at the supporting sample extremes, and a finite score-zero equality witness suffices. |
| sstd | nu+sd | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the two held-shape, held-sd halves at the supporting sample extremes. |
| sstd | nu+sd | uniqueness | regraded | Holding shape 31 and sd 9/8 preserves the strict witness and reflection. |
| sstd | nu+xi | criterion | accepted | The joint envelope reduces to only collapse at sample atoms with the two holds imposed, and a finite score-zero equality witness suffices. |
| sstd | nu+xi | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving only collapse at sample atoms with the two holds imposed. |
| sstd | nu+xi | uniqueness | regraded | The score set has finitely many semialgebraic components of constant value, and ranking them is only one-sided under the review rule. |
| sstd | sd | criterion | accepted | The joint envelope reduces to the held-sd normal and half profiles and lower-shape atom collapse, and a finite score-zero equality witness suffices. |
| sstd | sd | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the held-sd normal and half profiles and lower-shape atom collapse. |
| sstd | sd | uniqueness | regraded | Holding sd 9/8 keeps the strict witness and the all-free upper bounds remain valid. |
| sstd | sd+xi | criterion | accepted | The joint envelope reduces to the two-piece normal with held sd and skew and lower-shape atom collapse, and a finite score-zero equality witness suffices. |
| sstd | sd+xi | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the two-piece normal with held sd and skew and lower-shape atom collapse. |
| sstd | sd+xi | uniqueness | regraded | The exact shifted Student witness beats every centered fit and the normal face, and reflection yields two finite winners. |
| sstd | xi | criterion | accepted | The joint envelope reduces to the held-skew normal and t2 and collapse at sample atoms, and a finite score-zero equality witness suffices. |
| sstd | xi | failure | accepted | The moment equations and uniform tail bounds exhaust the relevant boundary, leaving the held-skew normal and t2 and collapse at sample atoms. |

## Proof errors

**The skew-t scale-crossing construction fails on its entire proposed sample band.** At df one let \(p=100\), \(A=2^{20}\), and \(R=2^{50000}\). Take \(p\) copies each of \(1\) and \(D\), and one copy of \(R\). Each observation contributes a Cauchy factor \(w/(w^2+x^2)\) and the displayed skew factor \(S_x\). Extracting \(1/w\) for the observations at \(1\) and \(w/D^2\) for those at \(D\) cancels their leading scale powers. The observation at \(R\) contributes \(w/R^2\). After normalization, the leading factor is \(w\). The displayed objective uses \(w^2\), which multiplies the correct likelihood by \(w\) and changes the relative heights of the branches. The source checker tests inequalities for that changed objective. [Source construction](2026-11-27-st-atlas-math-report.md#two-scale-maxima-with-slant-and-location-held)

Write \(G\) for the correct objective and \(D_\pm\) for the stated band endpoints. The branch comparison is uniform in \(D\).

- At \(w=A/8\), write \(S_1/2=(1+(1+y)^{-1/2})/2\), with \(y=1/64+A^{-2}\). The tangent bound and Bernoulli's inequality give \((S_1/2)^p>1-p/250\). Each of the other five dimensionless factors exceeds \(99/100\) uniformly for \(D\ge D_-\), so
  \[
  G(A/8,D)>(A/8)2^p(1-p/250)(99/100)^5>D_+/16.
  \]
- For \(8A\le w\le 2^{50}A\), \(S_1\le 9/8\) and all other dimensionless factors are at most one. Consequently
  \[
  G(w,D)\le 2^{50}A(9/8)^p<D_+/16.
  \]
- For \(w\ge 2^{50}A\), \(S_1\le 1+2^{-50}\). With \(u=w/D\), the bound \((1+u^2)^p\ge 1+pu^2\) gives \(w(1+u^2)^{-p}\le D/(2\sqrt{p})\). Hence
  \[
  G(w,D)\le (1+2^{-50})^p D_+/20<D_+/16.
  \]

These comparisons use rational bounds, with the constants checked in the review script. The branch maxima cannot tie at the source's implicitly defined \(D_*\). The held-df row fails for its proposed sample, and the free-df row fails because it requires both winning scales at df one. Whether another sample proves either general nonuniqueness claim remains open. The reflected skew-t construction uses a separately bounded likelihood. [Exact arithmetic check](2026-11-27-atlas-mrev1-data/validate.py) · [Reflected example](2026-11-27-st-atlas-math-report.md#reflected-finite-maxima-with-unrestricted-slant)

**The SGED held-sd failure claim needed a units correction.** At exact range contact, the optimized penalty is an additive loss of \(e^{-\gamma}\min(A,B)\) in log likelihood, where \(A,B\) are the extreme counts. The source row's wording could be read as a product-likelihood multiplier of \(e^{-\gamma}\min(A,B)\). The product multiplier is \(\exp[-e^{-\gamma}\min(A,B)]\). The final wording specifies the log penalty and retains the exact grade. [Uniform-end derivation](2026-11-27-sged-atlas-math-report.md#uniform-end)

## Rows not joined

- **st, alpha+nu+xi, uniqueness** and **st, alpha+xi, uniqueness** were rejected because of the scale-normalization error. Both hold masks exist in the atlas.
- Every proposed hold mask exists in the atlas. The empty CSV hold field corresponds to `all free` in `atlas.csv`.
- Two existing all-free skew-t keys and the criterion and failure keys for st with alpha+nu+xi held were outside the submitted CSV. The report's fitted-domain comparison and strict critical-scale result are relevant to those existing keys. The join file covers submitted proposals only. [Existing proof inventory](../existence-atlas/proofs.csv) · [Strict critical-scale result](2026-11-27-st-atlas-math-report.md#strict-critical-scale-loss-with-location-held)

## Open by family

- **tlambda.** General joint cardinality with held shape outside the singleton window, and cardinality on the held-location and held-scale shape profiles, remain open. The compact critical comparison is exact as a criterion. An effective comparison for arbitrary irrational held shape and an exact binary64 held-location crossing example remain open. [Source report](2026-11-27-tlambda-atlas-math-report.md)
- **sged.** General maximizing-set cardinality, practical signed comparisons at joint half contacts and exact finite/face equality tests remain open. For fixed sd and an extremely small width deficit, the existence proof supplies no practical shape bound. [Source report](2026-11-27-sged-atlas-math-report.md#open)
- **ged.** The two separated-shape ties prove multiplicity on exactly defined real samples. A held-mean binary64 shape tie, effective general root isolation and exact equality predicates remain open. [Held-mean proof](2026-11-27-sged-atlas-math-report.md#ged-held-mean) · [Held-sd proof](2026-11-25-ged-math-report.md#separated-and-tied-maximizers)
- **st.** The two rejected masks need a valid globally separated scale crossing or a different cardinality proof. Arbitrary held-slant and held-location cardinality still needs joint nuisance or df-profile comparisons. General critical-layer upper bounds when the moving-atom linear term vanishes and exact comparison of close finite and face values remain open. [Source report](2026-11-27-st-atlas-math-report.md#open)
- **sstd.** Counting all reflected orbits is open. Held-skew singletons with asymmetric data need further analysis, including fixed-shape comparison across join branches and free-shape comparison across finite roots. The semialgebraic score proof determines the form of the candidate components at fixed shape. Effective isolation and exact value comparison for arbitrary real powers remain open. [Fixed-shape comparison](2026-11-27-sstd-atlas-math-report.md#fixed-shape-component-comparison)

## Checks

All seven source checker invocations exited zero on the workstation through the one-slot wrapper. The Tukey lambda checks covered the elasticity and critical collar plus proposed keys and anchors. The SGED script replayed the prior GED interval covers and the new strict reflection and crossing inequalities. The two skew-t scripts checked the rational reflected witness and replayed the contact layers, as well as testing the scale expression discussed above. The two skew Student scripts replayed the exact 35,819-box \(t_2\) cover, the outward interval witness and the supplementary envelope checks. The review validator passed all 114 keys and evidence anchors, confirmed no duplicates and checked the corrected skew-t branch inequalities. No builds were required. [Review validator](2026-11-27-atlas-mrev1-data/validate.py)

The repository link checker reported 47 inherited destinations and one flagged destination, the known `P+Q` false positive in the beta-prime report at line 461. It found no broken destinations in this review. The batteries false positive did not appear in this worktree.
