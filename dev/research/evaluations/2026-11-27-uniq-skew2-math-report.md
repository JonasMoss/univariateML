# Generic uniqueness in two-piece Student and generalized error models

TASK-414 · 2026-11-27 · Base e5392fec

**Proved.** For skew Student, all 14 masks have a generic finite-singleton theorem on fixed-count strata with sufficiently many distinct levels. Held shape lowers the threshold to three levels. With free shape four levels suffice, and the all-free result of TASK-412 also covers ordinary three-observation samples. Held sd and unit skew give an open two-observation counterexample. In SGED, the smooth fit has the same four-level threshold, but shape one requires comparison of the composed heights at knots. In GED and in SGED with unit skew held, free mean can give the entire median interval on an open set even when shape is estimated. Thus an unqualified generic-singleton claim fails for these masks.

## Comparison on ordered level strata

**Proved.** Fix holds and positive integer counts \(k_j\) on the ordered levels \(x_1<\cdots<x_m\). All measure statements below are in this open \(m\)-dimensional chart. In the experiment with \(n\) observations, \(m=n\) almost everywhere. A count attached to a tied level does not increase the number of intersections available to separate gradients. Repeated-level strata, constants and small \(n\) are therefore specified separately.

Use the compact-comparison theorem of TASK-412. In smooth charts, the derivative at a level is the ordinary kernel sample score times that level's count. Piecewise analytic heights with matching first derivatives at the joins satisfy the theorem on compact pieces. At a tied global value, two different composed gradients imply a null set. More precisely, take the union of nondifferentiability loci of compact two-chart maxima at points where their value equals the full supremum. This is the exact set of ties with differing gradients among the compared candidates. For pairs proved to separate, it describes their exceptional samples without postulating sample symmetry. It still requires global-height comparison. At a crossing of two nondegenerate analytic stationary values with different gradients, equality lies on a regular hypersurface. Reflection establishes an orbit of size two at an off-symmetry winner of a reflection-invariant sample and holds. It does not count other winning orbits. [Compact comparison and analytic branches](2026-11-27-uniq-generic-math-report.md#the-theorem) · [Exact ties and arithmetic](2026-11-25-ties-math-report.md#classification-of-the-constructions)

## Skew Student separation

**Proved.** In denominator widths \(a,b>0\) and join \(c\), the sample score is \(-(q+1)(x-c)/(w^2+(x-c)^2)\), with \(q=\nu>2\) and \(w=a\) or \(b\). The height is piecewise analytic with matching first derivatives. On a common exterior of distinct joins, put \(r>0\) for distance to the nearer join, \(d>0\) for the join gap, and \(A,B\) for the near and far squared widths. Writing \(u,v\) for near and far shape plus one, the equality polynomial is
\[
 (v-u)r^3+d(v-2u)r^2+(vA-uB-ud^2)r+vAd.
\]
Descartes' rule gives at most one positive root if \(v\le u\) and at most two if \(v>u\). The near and far roles reverse on the other exterior. No score equality occurs between distinct joins or at either join. At a common join with different shapes there is at most one root per side in squared distance, plus the join. With equal shapes and a common join, equality on an occupied strict side pins its width. If no observations occupy the other strict side, equal heights pin its width too, because only the peak normalization \(1/(a+b)\) can differ. A nonconstant sample has at least one strict side. Thus four levels suffice with free shape and three suffice with held shape, independently of the moment holds. With a common join known in advance and fixed width ratio, two distinct nonzero standardized absolute distances suffice even with shape free. [Score intersection proof and stronger all-free low-level result](2026-11-27-uniq-generic-math-report.md#pilot-2-skew-student-all-free)

**Proved for the regular faces and critical collapse.** Use the joint envelope of the atlas. Its positive-width pieces are the two-piece normal **N**, the positive-width two-piece \(t_2\) **D**, and the half laws **H**. **C** is the critical collapse. A smooth free join or width is a chart variable. A half contact imposed by the data can be parametrized by the contacted extreme, solving for join, width or shape as in the atlas. On a strict support piece the ordinary broad-side value applies. At a moving contact of a free-mean half, set the join to the extreme. At a moving contact of a held-mean half, insert the implied width or the locally smooth shape root. Off the contact level the resulting sample derivatives are still the fixed-kernel derivatives. With both moments held, the forced contact of a fixed shape or normal end lies on a fixed-level hyperplane. Keep its penalized height for pointwise comparisons and omit that hyperplane from the generic comparison. This hyperplane exclusion is required separately on each ordered level chart. The atlas gives the actual contact penalties, including the normal corner, and excludes the remaining ends by zero likelihood or divergence. [Joint face and collapse values](2026-11-27-sstd-atlas-math-report.md#joint-envelope)

Between Student and normal score curves the count is again at most three. On the exterior where the Student join is nearer, equality gives \(A_0(r+d)(r^2+w^2)-pr=0\) with \(A_0,p>0\). The leading, quadratic and constant coefficients are positive, allowing at most two roots. Where the normal join is nearer it gives \(A_0r((r+d)^2+w^2)-p(r+d)=0\), with at most one positive root. A common join gives at most one strict root per side plus the join. Two normal curves are affine on each exterior and have at most one intersection there unless identical on that side. For positive-width faces, discard at most two contacted levels in a pairwise comparison. A shared score identity on one occupied side pins the visible shape, join and broad width. If the other side is unoccupied and there are no forced penalties, equal heights pin the total width. This also rules out an equal-height full/half identity, since the half's other width is zero. Two descriptions of the identical half kernel at a corner count as the same face point. Hence on strata of at least six levels, distinct regular positive-width winners separate, including finite/face and face/face ties. A half's active score is used only within its support.

On a critical atom stratum **C** occurs at a unique atom. Its height at fixed skew is a data-independent multiplier times \(\prod_{x_i\ne y}|x_i-y|^{-(q_*+1)}\), with the multiplier constant on the stratum. Off the contact its score is the rational Student score with zero width at \(y\). The preceding counts apply against positive-width kernels, or follow by factoring out the zero root in the cubic when its constant coefficient vanishes. Thus the same six-level condition separates critical collapse from positive-width winners. Different collapse skews at that one atom share this gradient. A held mean equal to an observed level is itself a null hyperplane in a level chart with that coordinate free. On a subchart with that coordinate pinned, finite/finite separation on the remaining levels holds when the remaining levels alone exceed the intersection count, with the same normalization argument when a strict side is absent only from all observations. General separation of collapse skew labels and of all low-level face comparisons remains open.

**Proved scalar and fixed-shape refinements.** With shape held, the stationarity equations on either side of all join equations are semialgebraic after adjoining the square root from a variance hold. Each connected component of the score-zero set has constant value. The complete pointwise answer compares representatives with the boundary benchmark and retains all top finite components. The scale-only mask has a finite stationary list. For this mask let \(U=1/T\) in ordinary widths, and suppose all residuals to the held mean are on the side of \(z\), or take \(z=0\) and allow either side. The standardized magnitude in denominator units on each side is \(r_i=\beta_i U+\delta_i\), with \(\beta_i\ge0\) and \(\delta_i\ge0\). The log inverse-scale score is \(n-(q+1)\sum r_i(r_i-\delta_i)/(1+r_i^2)\). Each nonconstant term increases strictly, since its derivative in \(r_i\) has numerator \(\delta_i r_i^2+2r_i-\delta_i>0\) for \(r_i>\delta_i\). The score starts at \(n\) and ends at \(k-q(n-k)\), where \(k\) is the held-mean count. It has exactly one zero if that end is negative, and no finite maximum otherwise. At unit skew and held shape, free location and scale instead have the all-sample Student geodesic uniqueness theorem. [Fixed-shape components and boundary ranking](2026-11-27-sstd-atlas-math-report.md#fixed-shape-component-comparison) · [Student geodesic theorem](2026-11-27-uniq-generic-math-report.md#pilot-1-student-with-df-held)

## SGED and GED separation

**Proved for the smooth kernels.** In ordinary widths \(a,b\), the SGED loss at level \(x\) is \((|x-c|/w)^p\) and the log peak is \(-\log(a+b)-\log\Gamma(1+1/p)\). GED has \(a=b\) and \(c=\mu\). Free shape is fitted on \(p\ge1\). For \(p>1\) the sample gradient is continuous at the join. The log ratio of two score magnitudes on a common exterior is a constant plus \(u\log r-v\log(r+d)\), where \(u,v\) are the near and far shapes minus one. Its derivative has numerator \(ud+(u-v)r\) over \(r(r+d)\). If \(u\ge v>0\), it has no turning point there and gives at most one equality. The other exterior has at most two. If \(u=v\) each exterior permits at most one. Opposite signs exclude points between distinct joins. A common join gives at most one equality on each side for different powers, plus the join. For equal shape and join the occupied widths and equal height identify the fit as for Student. Thus two superunit winners separate at four levels, or three with shape held. At a held GED mean, equality at two different nonzero distances forces the same power and width, including the power-one candidate. Samples without such distances are a null set for \(n\ge2\). The same conclusion holds for fixed unit skew in SGED with mean held.

**Proved for subunit shape and moving joins.** Subunit shape is allowed only when held. At a join a fixed-parameter sample germ can fail even to be locally Lipschitz. Use a chart that sets \(c=x_j\), preserving the holds. With mean free this is direct. With mean held and scale free solve \(z=(\mu-x_j)/(kS)\) with \(S=a+b\), \(z=(b-a)/S\), \(k=\Gamma(2/p)/\Gamma(1/p)\). With both moments held use \(c=\mu-2k\sigma z/W(z)\), \(W^2=v+Dz^2\), \(v=\Gamma(3/p)/\Gamma(1/p)\), \(D=3v-4k^2\). The derivative in \(z\) is \(-2k\sigma v/W^3<0\). The contact term is identically zero on each such chart and all other residuals are separated from zero locally. For the Laplace comparison at held mean and held nonunit skew with free sd, solve \(S=(\mu-x_j)/(kz)>0\) on a moving contact. At unit skew the contact is on the fixed hyperplane \(x_j=\mu\), and at held mean, sd and skew the Laplace join is also fixed and contacts are on fixed-level hyperplanes. The exact weighted-loss argument below covers free mean with both shapes held. In all comparisons away from the at most two candidate contacts, the derivatives remain the ordinary kernel scores. For a common subunit shape and different joins the log ratio is strictly monotone on each common exterior. For a common join the active widths are recovered and the normalization decides the remaining width. Five levels suffice for finite/finite generic uniqueness at fixed subunit shape with skew free. With both moments and shape held the scalar comparison below also gives the exact set on low-level samples. [Moment and joint face formulas](2026-11-27-sged-atlas-math-report.md#common-sged-proof)

**Proved for Laplace.** At fixed widths, the complete conditional location set minimizes weighted absolute loss. In the gap after level \(j\) the loss slope in \(c\) is \(K_j/a-(n-K_j)/b\), \(K_j=\sum_{i\le j}k_i\). The slope strictly increases across observations. Zero slope gives the closed gap, otherwise the unique minimizing knot is where the slopes change sign. When scale is free and skew is fixed, the scale profile preserves this ordering and gives the same scale everywhere in a winning gap. For fixed widths below shape one, the strictly concave losses on gaps instead leave only knots. The free-scale conditional width for fixed shape and width ratio is unique whenever the loss is positive. Above one, strict convexity in the join gives the unique location for each width, and profiling total width preserves the location ordering.

With free mean **and free skew**, no conditional Laplace gap can win. At a balanced slope, differentiating that slope in \(z\) is nonzero, even when \(S\) depends on \(z\) through held sd. A gap of global maxima would contain two interior joins with different skew derivatives, which cannot both be zero. This argument also excludes globally winning endpoints of a conditionally flat gap. Each finite Laplace winner then has a knot join. Its composed gradient at the knot is minus the sum of the other level gradients by translation invariance, since the mean is free. Two equal-height Laplace winners at the same knot must agree in all occupied slopes if their gradients agree, and normalization pins an unused width. At different knots, equality requires adjacent knots, since an intervening level would have opposite gradients. If there are levels on both common exteriors, the two widths agree. The conditional median inequalities at the adjacent knots then force a balanced gap, already excluded. If the lower knot is the first level and there are levels beyond the upper knot, the right width \(b\) is common. Equality of the composed derivative at the first level gives \(k_1/a'=(n-k_1)/b\), where \(a'\) is the upper winner's left width. This makes the upper knot an endpoint of a balanced gap. Reflection handles the last level. Consequently on \(m\ge3\) the free-mean, free-skew finite Laplace candidates separate. This proof does not require the skew or scale optimum to be unique in a fixed knot chart. The shape-one and superunit scores can agree at at most two noncontact levels, one per side, since on each side the shape-one score is constant and the superunit magnitude is strictly monotone. With one moving knot ignored, four levels suffice for mixed finite comparisons. When the mean is held and skew is free, no sum-to-zero identity for the composed gradient is available. The both-moment, held-shape result below resolves the exact scalar search, but general Laplace/Laplace gradient separation with a held mean remains open.

**Proved for the ordinary ends, with the following separation scope.** SGED compares finite-shape half ends and the uniform whenever the holds allow them. The uniform has height \(-n\log R\) for a free location and scale, or \(-n\log(2\rho)\) with mean held and scale free, where \(R\) is the range and \(\rho\) is the largest distance to that mean. Its derivatives vanish off the extreme levels. With sd held the unpenalized height is constant under strict containment and is beaten by finite fits. Exact support contacts, including a farthest-side tie when using the centered uniform, are proper hyperplanes for these generic statements. Use the atlas penalties there for any pointwise comparison. For free mean, a half contact can fix its supporting join at the extreme. For held mean and free sd it fixes \(S\) through the mean equation. With both moments held and free shape it can solve the monotone half endpoint equation for \(p\). These charts are feasible nearby on their open contact branches. At shape one with both moments held the contact at the fixed half endpoint is an exceptional hyperplane. The broad-side gradient applies off contact. At \(m\ge5\), a finite shape candidate or half end differs from the uniform at a nonextreme, noncontact level. Indeed at least two levels remain after discarding two extremes and one contact, and at most one of those can be at a smooth finite join. Between two power-law kernel scores (finite or half), the above same-sign exterior counts apply after discarding up to two contacts. Identity on one active side of a non-Laplace curve fixes shape, join and that width. Equal height excludes different total widths when the other side is absent and penalties are zero. Oppositely oriented half ends cannot share a nonzero gradient at any sample where both are supported. At \(m\ge6\) this proves separation for comparisons involving a non-Laplace finite or half shape, except that an infinite supremum makes comparison irrelevant. In a mixed Laplace/non-Laplace comparison use the constant-versus-strictly-monotone count instead. Two same-orientation Laplace half charts, or a finite Laplace chart versus a Laplace half, can share constant slopes on their common side and require a separate height test. Uniforms with different limiting skew labels can describe the same optimized limiting law. [Half contacts and uniform endpoint values](2026-11-27-sged-atlas-math-report.md#finite-shape-half-ends) · [Uniform traces](2026-11-27-sged-atlas-math-report.md#uniform-end)

## Open sets of median intervals

**Proved.** For GED with sd held, let \(R_1\) be the minimum mean absolute loss. The strict small-sd criterion
\[
 \log(R_1/\sigma)>9/4-\gamma-\tfrac12\log2
\]
forces \(p=1\) as the unique winning shape. \(R_1\) is continuous, so at any even sample size a sufficiently spread distinct sample gives an open set of full median intervals. The exact example is \((-1,1)\) at \(\sigma=1/10\). The result transfers to SGED with unit skew held. With general fixed skew, whenever the gap slope balances at \(p=1\), the whole weighted median gap wins if shape one wins. The balance is a count predicate and can hold on a full-dimensional ordered chamber. In particular it is equivalent to \(a/b=K_j/(n-K_j)\). If there is no such count, no location gap can occur at the Laplace shape. [Small-sd proof](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md#free-shape-bounds)

**Proved, free scale as well.** At the GED sample with one copy of each of \(\pm1\) and three copies each of \(\pm2^{-10}\), the strict shape-one score and strict global gap outside any shape-one neighborhood persist on an open neighborhood in the full eight-observation chart, including distinct samples. To see this, on compact shape ranges the power-loss minimization over the sample range is continuous. At shape one all medians have the same loss and profiled scale. The right profile slope is the maximum of the partial shape slopes over that median interval, by the uniform first-order expansion of the unprofiled loss. The strict sign established at the symmetric witness therefore bounds every median's partial shape slope strictly below zero. By compactness, near the sample and near \(p=1\) every minimizing join is close to this witness median interval and the partial profile slope remains strictly negative there. For a candidate at small \(p-1>0\), integrate that slope with its join fixed to bound its value by its own value at \(p=1\). The large-shape tail is uniform locally: with \(d=R/2>0\), at least one residual at every join is at least \(d\), yielding the upper bound \(-\log(2d)+[\log n-1-\log p]/p-\log\Gamma(1+1/p)\) on the per-observation profiled height. This tends locally uniformly to the range-uniform height, strictly below the witness. On the intervening compact shape interval the witness's strict gap persists. Thus only \(p=1\) wins throughout the neighborhood. Its complete maximizer set is the central interval with common fitted scale, also for SGED at held unit skew. For odd distinct samples the ordinary median is a singleton. In SGED or GED with mean held a median displacement is infeasible. [Strict shape comparisons and right-score proof](2026-11-25-ged-math-report.md#exact-finiteuniform-equality-and-an-all-free-median-interval)

## Regime results

In the summary, level thresholds refer to almost every ordered sample at each fixed count pattern and fixed valid holds, conditional on finite attainment. “Exact set” is an exact-real candidate and value prescription on every sample. It can require comparing real values exactly.

| Regime | Previous claim | New result |
| --- | --- | --- |
| sstd all free | reflected pair | Generic at four levels and on ordinary three-observation samples, as in TASK-412. Common face comparison also applies |
| sstd mean | reflected pair | Generic at four levels |
| sstd mean+nu | reflected pair | Exact set by stationary components, generic at three levels |
| sstd mean+nu+sd | reflected pair | Exact set by stationary components, generic at three levels |
| sstd mean+nu+xi | finite stationary scale list | Exact set, generic at three levels and pointwise on an outward-residual class |
| sstd mean+sd | reflected pair | Generic at four levels |
| sstd mean+sd+xi | open | Generic at four levels |
| sstd mean+xi | open | Generic at four levels |
| sstd nu | reflected pair | Exact set by stationary components, generic at three levels |
| sstd nu+sd | reflected pair | Exact set by stationary components, generic at three levels |
| sstd nu+xi | stationary components | Exact set, generic at three levels and pointwise at unit skew |
| sstd sd | reflected pair | Generic at four levels |
| sstd sd+xi | reflected pair at unit skew | Generic at four levels, open two-observation region of nonuniqueness |
| sstd xi | open | Generic at four levels |
| sged all free | reflected pair | Generic at four levels |
| sged mean | reflected pair | Generic at four levels for comparisons involving superunit winners. Laplace/Laplace open |
| sged mean+nu | reflected pair | Generic on superunit and subunit classes. Laplace/Laplace open |
| sged mean+nu+sd | reciprocal pair | Exact scalar set for every held shape |
| sged mean+sd | reflected pair | Generic at four levels for comparisons involving superunit winners. Laplace/Laplace open |
| sged mean+sd+xi | GED shape crossing | Generic at four levels, at unit skew already with two unequal nonzero radii |
| sged mean+xi | GED shape crossing | Generic at four levels, at unit skew already with two unequal nonzero radii |
| sged nu | reflected knots | Generic at three levels. Exact knot set for subunit and Laplace |
| sged nu+sd | reflected pair | Generic at three levels for \(p\ge1\), five for \(p<1\). Exact Laplace knot comparison |
| sged nu+sd+xi | unique above one | Exact weighted point, segment or knot set |
| sged nu+xi | strict above one | Exact weighted point, segment or knot set with fitted width |
| sged sd | reflected pair | Generic at four levels |
| sged sd+xi | Laplace median interval | Generic at four levels up to winning median gap, with open gap sets |
| sged xi | Laplace median interval | Generic at four levels up to winning median gap, with open gap sets |
| ged all free | conditional locations and ties | Generic at four levels up to winning median gap, with open gap sets |
| ged mean | separated shape crossing | Exact scale-profile root comparison, generic at two unequal nonzero radii |
| ged mean+sd | separated shape crossing | Generic at two unequal nonzero radii |
| ged sd | Laplace median interval | Generic at four levels up to winning median gap, with open gap sets |

### sstd held all free

**Proved.** TASK-412 covers all ordinary sample sizes: at four or more levels by separation, at three equally replicated levels by both width scores, and at one and two distinct observations by nonattainment. It also proves finite/face separation starting at four distinct levels for this mask. Unequal three-level count patterns remain open. With other repeated-level patterns use the general level result. The exact reflection sample used in the next section is an exception on its six-level chart as well as in the full observation space. [All-free theorem and small samples](2026-11-27-uniq-generic-math-report.md#pilot-2-skew-student-all-free)

### sstd held mean

**Proved.** Four levels suffice with mean fixed, because finite equal-height pairs still obey the same gradient bound, although location stationarity is unavailable. The common face comparison includes **N**, **D**, both compatible **H**, and **C** when critical. For this and all seven other free-skew masks in scope, the exact reflection sample is one copy each of \(\pm512\), 512 copies each of \(\pm7/4\), and 5120 copies each of \(\pm1\). Where held, use mean zero, shape 31 and sd \(9/8\). Its finite witness beats the reflection-fixed value and all faces strictly. Every such mask has at least two finite winners exchanged by reflection. These samples lie on proper reflection-symmetry subspaces of their six-level charts. [Exact witness and interval comparisons](2026-11-27-sstd-atlas-math-report.md#six-level-sample-and-exact-separation)

### sstd held mean+nu

**Proved.** Three levels suffice with equal held shape. Compare the stationary components to **H** and eligible **C**. This is an exact value prescription even on the six-level reflected sample above and on critical repeated strata. It requires comparing one value per component, and does not count other orbits of the reflected sample by symmetry alone.

### sstd held mean+nu+sd

**Proved.** Three levels suffice. With only skew free the stationary components are scalar points or intervals. Retain the full components at the winning height against the two **H** ends, with the atlas's forced penalty at contact. The six-level example uses the specified three holds. The forced-contact equalities themselves are on fixed-level hyperplanes.

### sstd held mean+nu+xi

**Proved.** Three levels suffice. The finite scale-score roots give the exact maximizing set after comparison with **C**, or the empty set on divergent or strictly boundary-winning samples. On outward residuals the inverse-scale proof gives a singleton exactly when \(q(n-k)>k\). The critical finite/collapse crossing instead has six zeros and \(-1,d_*\) at mean zero, shape three and skew ten, with \(d_*\) the attained minimum defined in the linked proof. Its residuals occupy both signs, and the held mean is a pinned critical atom, outside the generic finite/face comparison. The collapse equality does not establish another finite winner. [Critical crossing](2026-11-25-sstdsged-math-report.md#which-model-wins)

### sstd held mean+sd

**Proved.** Four levels suffice. Compare **N**, **H** with both moments preserved, and the possible critical **C**. In particular the normal half-contact penalty must survive this comparison. The six-level reflected example is attained strictly above the ends. Three-level and lower-level finite uniqueness remains open in general.

### sstd held mean+sd+xi

**Proved.** Four levels suffice for the scalar shape comparison. The only competing ends are the held **N** and possible critical **C**. At unit skew the join is fixed, so two distinct nonzero radii separate finite shapes. With asymmetric skew the join changes with shape and the general count applies. The earlier \((0,0,1)\) witness at mean zero, sd \(1/100\), skew \(1/10\) establishes strict attainment, leaving pointwise shape cardinality on that sample undecided. Global ordering of scalar roots away from the generic class remains open. [Held shape-only comparison](2026-11-27-sstd-atlas-math-report.md#sstd-held-meansdxi)

### sstd held mean+xi

**Proved.** Four levels suffice. At unit skew and two unequal nonzero radii, the common-join score argument separates the two varying coefficients already. At asymmetric skew the conditional scale can have competing stationary branches. The generic result uses the joint likelihood, so it needs no differentiability of a profiled scale. The matching-skew **N**, **D** and possible **C** determine attainment. Pointwise equality of joint finite candidates outside the separated comparison remains open.

### sstd held nu

**Proved.** Three levels suffice. At each shape the exact semialgebraic prescription compares all finite stationary components to the two supported **H** ends and possible critical **C**. The six-level reflected counterexample at shape 31 meets this description. Low-level finite cardinality and the number of its reflection orbits are open.

### sstd held nu+sd

**Proved.** Three levels suffice. The exact fixed-shape component comparison requires only the two **H** ends. The six-level reflected example uses shape 31 and sd \(9/8\). No inference about uniqueness on every two-level sample follows from the intersection bound.

### sstd held nu+xi

**Proved.** Three levels suffice. Use the exact fixed-shape component rule against **C**. At unit skew, the held-df Student theorem gives a singleton on every sample with strict collapse exponent, and divergence or finite nonattainment otherwise. At general skew the scale score is strictly monotone at a fixed join when its maximizing scale exists. This still leaves comparison across locations, for which the component rule applies.

### sstd held sd

**Proved.** Four levels suffice. Include **N**, **H** with the held scale relation and possible **C**. The six-level reflected sample at sd \(9/8\) gives an exceptional pair. The shape-two positive-width **D** is infeasible under this hold.

### sstd held sd+xi

**Proved.** Four levels suffice, but for \(n=2\) at unit skew there is an open set of finite nonuniqueness. Translate the midpoint to zero and write \(h\) for the half-gap. For any fixed \(q>2\) the product of the two density denominators, before raising to power \(-(q+1)/2\), is
\[
 [c^2+(q-2)\sigma^2-h^2]^2+4h^2(q-2)\sigma^2 .
\]
At each winning shape, the exact conditional join set is the midpoint alone if \(h^2\le(q-2)\sigma^2\), or the midpoint plus or minus \(\sqrt{h^2-(q-2)\sigma^2}\) otherwise. For \(h/\sigma\) near 100 the strict centered upper bound and the shape-three off-center witness used at \((-100,100)\), sd one, remain separated by continuity. The best **N** is centered and collapse has positive exponent. Every global winner is finite and off-center, with its mirror. The half-gap inequality is open, since every two-observation sample is symmetric about its own free midpoint. The number of winning shapes remains open here. For general skew the boundary comparison uses its own **N** and possible **C**. [Exact two-observation witness](2026-11-27-sstd-atlas-math-report.md#fixed-skew-and-held-sd)

### sstd held xi

**Proved.** Four levels suffice. At unit skew this is the ordinary Student problem with free mean, sd and shape. Its fixed-df geodesic theorem gives the unique conditional location-scale fit whenever it attains, while free-df shape ranking is separate. For general skew the compatible **N**, **D** and possible **C** decide attainment. The earlier open claim is resolved on the stated generic class, with lower-level and pointwise shape comparisons open.

### sged held all free

**Proved.** Four levels suffice by the smooth and Laplace finite-gradient comparisons. The reflected example used in the next section is an exception outside the resulting full-measure set, both in its four-level chart and in the full eight-observation space. Constant samples collapse. A balanced pair of distinct observations selects the unattained uniform. The half and uniform benchmarks are the joint values in the atlas. A simultaneous Laplace half and finite Laplace win needs the remaining same-slope half comparison. [All-free attainment and two-level case](2026-11-27-sged-atlas-math-report.md#sged-held-all-free)

### sged held mean

**Proved.** At four or more levels a generic attained set containing a superunit winner contains just that winner. At shape one the mean constraint couples the join and widths. The Laplace/Laplace comparison remains open. Compare the mean-constrained half ends and centered uniform, after the collapse test. The exact reflected sample for the free-skew SGED masks is one copy of each of \(\pm1\) and three of each of \(\pm2^{-10}\). Where held use mean zero, sd \(\sqrt2(1+3\cdot2^{-10})/4\) and shape one. The previously proved strict gap over the reflection-fixed set and both types of ends forces at least two distinct finite winners. [Reflected sample and strict comparisons](2026-11-27-sged-atlas-math-report.md#a-finite-reflected-pair-across-eight-masks)

### sged held mean+nu

**Proved.** Three levels suffice when \(p>1\) and five suffice when \(0<p<1\). For \(p\ge1\) a fixed skew reduces the scale problem to a strictly concave log likelihood in inverse width on nonzero-residual samples. Indeed the scaled residual is affine in inverse width, the asymmetric power loss is convex, and the log-normalization term is strictly concave. Pure collapse occurs at all-zero residuals. The remaining maximization over skew and the constrained half benchmarks are separate. The reflected example at shape one is valid, and generic Laplace ties with moving contacts and held mean remain open.

### sged held mean+nu+sd

**Proved, exact set.** Use the single variable \(z\in(-1,1)\), \(S=2\sigma/W(z)\), and \(c=\mu-kzS\). Insert every feasible contact \(c=x_j\). On the open cells between them, isolate all zeros of the scalar score, and compare their heights and the contact heights against both penalized half benchmarks. Keep all finite candidates at the winning value. The score has only finitely many zeros on each cell. To see this, all residual and width factors are analytic near each cell endpoint before taking powers, and nonidentically-zero factors have finite orders of vanishing. Each derivative term there is a real power of the one-sided distance times a convergent analytic series. A finite sum of such series either has a first nonzero term or vanishes identically. It therefore has no accumulating zeros unless the height is constant on a cell.

No constant cell is possible. For noninteger \(p\), a cell with a contact endpoint has a nonzero term proportional to the distance to the endpoint to power \(p\) times a positive analytic factor. Its nonanalytic term cannot cancel the analytic contributions of the other levels. With no interior contact, there must be a thin-side observation at one of the two ends. Otherwise the sample would lie inside the full swept join interval, generating a contact, or be pinned to both distinct endpoints. Even equality at a swept endpoint leaves the observation strictly on the thin side at the opposite end. The thin-side term then diverges. For integer \(p\), each cell's penalty is an algebraic function of \(z\) (signed residuals at fixed powers). Its cancellation against the log peak would make \(\log W(z)\) algebraic. This is impossible since \(D>0\) and the analytic continuation of \(\log W\) around a simple complex zero of \(W^2\) adds a nonzero multiple of \(\pi i\) per circuit. An algebraic germ has only finitely many continuation values at its base point. Thus no shape permits an interval of optimizing skews. The exact sample \((-11/10,11/10)\), at mean zero, shape one and sd one, has exactly the two reciprocal skews from the earlier monotone equation in \(z^2\). [Penalized half values and reciprocal pair](2026-11-27-sged-atlas-math-report.md#sged-held-meannusd)

### sged held mean+sd

**Proved.** At four or more levels any superunit winner is generically the only finite winner. Both half ends can move in shape at contact, so use their joint envelopes and the held-moment uniform with its contact count. The held-sd reflected example forces a finite pair. A full generic claim still needs pure Laplace separation under both moments, and the scalar rule at fixed shape one decides that slice pointwise.

### sged held mean+sd+xi

**Proved.** The finite scalar shape is generically unique on strata of four or more levels, including comparisons with the sole Laplace candidate. At unit skew two unequal nonzero distances suffice. Compare the held-moment uniform, including the possible exact contact. The previous tied sample at unit skew and sd one is eight zeros, 23 copies each of \(\pm a_*\) and 12 copies each of \(\pm b_0\), where \(b_0=\texttt{0x1.6c84e093fc0ecp+0}\) and \(a_*\) is the unique separated-envelope crossing in \((0.329,0.331)\). Its two winning shapes are an exceptional equality on the pinned-mean symmetric stratum. [Exact separated-shape crossing](2026-11-25-ged-math-report.md#separated-and-tied-maximizers)

### sged held mean+xi

**Proved.** Four levels suffice, and at unit skew two unequal nonzero distances suffice. At shape one, the inverse-scale strict concavity argument gives a single finite candidate. Compare the centered uniform, or collapse if all residuals vanish. The exact crossing at unit skew uses \(N=2^{131072}\) copies each of \(0,h_*/2,1/2\), and one copy of \(1\), with mean zero. Here \(h_*\) is the smallest zero on \([0.103,0.105]\) of the difference between the maxima on \([1,2]\) and \([2,3N+1]\). At that sample two distinct finite shapes with their respective scales beat the uniform. This equality is a crossing, and the level fixed at the held mean is outside the ordinary full level chart. [Crossing construction](2026-11-27-sged-atlas-math-report.md#ged-held-mean)

### sged held nu

**Proved.** For \(p\ge1\), three levels suffice by the smooth or Laplace gradient argument. For \(0<p<1\) with both moments free the profiled join cost is \(A(c)^{1/(p+1)}+B(c)^{1/(p+1)}\), where \(A,B\) are the left and right residual power sums. Its strict concavity on gaps (also at \(p=1\)) restricts the exact global comparison to knots, including the extreme knots as half candidates. Subunit ties between two different knots are null for \(m\ge3\). For any two candidate knots, vary a third level toward the nearer candidate without crossing either candidate. The varied level's contribution at that knot makes the derivative of the cost blow up with exponent \(p-1<0\), or \(p/(p+1)-1<0\) when there are no other terms on that side. The other candidate's derivative stays bounded. The remaining terms on the varied side have positive analytic sums or are identically zero. The analytic cost difference cannot be identically zero on the ordered chamber. For this one-variable continuation the varied level may pass other noncandidate levels because the two cost formulas depend only on sides relative to the candidate knots. Two levels give only the two excluded half candidates for \(p\le1\), with an exact tie for equal counts. The four-level subunit sample \((-100,(-1)^{[5]},1^{[5]},100)\) at \(p=1/2\) gives exactly the two finite knots \(\pm1\) with their profiled widths, and the shape-one reflected example gives distinct finite winners as well. [Knot profiles and half comparison](2026-11-07-sged-skew-report.md#why-the-new-routes-are-global)

### sged held nu+sd

**Proved.** At least three levels suffice for \(p\ge1\); at least five suffice below one, using the two moving contact charts. At or below one, the global finite search with free skew can use all observation knots, optimizing scalar skew for each and comparing the two half ends with sd preserved. For \(p=1\) the no-flat-gap proof ensures that all finite winners are knots, rather than merely finding one knot representative. Constants select a half peak. The shape-one reflected example at the given sd remains valid. Low-level subunit ties with held sd remain open in general.

### sged held nu+sd+xi

**Proved, exact set.** The only free coordinate is join and the location objective is the weighted loss. Above one the strict convex minimizer is unique, at one use exactly the weighted median point or closed gap, and below one retain every knot of least loss. For \(m\ge3,\ p<1\), the analytic two-knot cost difference is nonzero by varying a third level toward one candidate as in the profiled argument. For two distinct levels \(L,H\) with counts \(A,B\), compare \(B((H-L)/b)^p\) with \(A((H-L)/a)^p\). Equality gives exactly the two endpoints throughout the corresponding level chart. At power one \(A/a=B/b\) gives the closed interval. Thus a fixed unit skew at \((-1,1)\), sd one, gives two points at \(p=1/2\) and a segment at \(p=1\), each of which persists on the full distinct two-observation chart at its hold. Hold-compatible mean is obtained by translating the join by \(kzS\). Constants give the unique join at the common value. [Two-level formulas](2026-11-25-sstdsged-math-report.md#3-sged-held-sd-and-two-distinct-observations)

### sged held nu+xi

**Proved, exact set.** Apply precisely the same location comparison in units of \(S\) at fixed width ratio. At every positive minimum loss \(Q=\sum_i(|x_i-c|/w_i)^p\), using the side fractions \(w_i\), maximize in \(S\) at \(S^p=pQ/n\). This width is common to every maximizing join, including a full weighted median gap when balanced. Constants collapse. The low-level two-point and median conclusions and the subunit null-set result transfer to free sd by strict ordering of the profiled likelihood in \(Q\). Thus the former above-one singleton claim extends to an exact conditional classification for every held power and both shapes.

### sged held sd

**Proved.** Four levels suffice for finite uniqueness with free skew and location. Compare the half ends at the supported extremes and the fixed-width uniform. At exact width contact the best uniform log penalty is \(e^{-\gamma}\) times the smaller extreme count, as corrected in the review. The reflected sample with held sd gives two finite winners away from generic uniqueness. Constants select the half peaks, and these two half labels do not count as two finite solutions. [Units of the uniform penalty](2026-11-27-atlas-mrev1-report.md#proof-errors)

### sged held sd+xi

**Proved.** At four or more levels almost every attained fit has either a single superunit winner or exactly its shape-one weighted median set. At \(p=1\) a nontrivial gap is a single class for this comparison, since its whole set has the same height and a smooth height germ obtained by choosing an interior join. Off its level contacts the slopes are constant. An odd sample at unit skew has a median point, while even samples at unit skew give an open gap region by the small-sd theorem. The exact witness at sd \(1/10\) is \((-1,1)\). The only excluded competitor is the width-constrained uniform. Whether a balanced gap can win for each asymmetric held skew and size outside the stated examples remains open.

### sged held xi

**Proved.** At four or more levels the finite maximizing set is generically a superunit singleton or the shape-one weighted median set with its one profiled sd. The exact eight-observation Laplace witness with unit skew extends to an open neighborhood, as proved above. Constants collapse and a nonconstant sample has only the range uniform as an excluded competitor. At asymmetric skew, odd size alone does not preclude count balance. A generic singleton follows at any size in scope if no gap count satisfies the fixed-ratio balance.

### ged held all free

**Proved.** At four or more levels, almost every attained finite maximizing set is either a singleton at \(p>1\) or the full ordinary median set and its common fitted scale at \(p=1\). The open eight-observation neighborhood proves actual positive-measure nonuniqueness. For odd distinct samples in scope the median set is one point. Constant samples collapse. The range uniform is the only excluded competitor on nonconstant samples. At the earlier exact finite/uniform equality, the sample \((-1,-a,0,0,a,1)\) with \(a=3/e-1\) has one finite Laplace fit and a tied excluded uniform. It is not a finite multiplicity counterexample. [Finite/uniform equality](2026-11-25-ged-math-report.md#exact-finiteuniform-equality-and-an-all-free-median-interval)

### ged held mean

**Proved, exact set with generic refinement.** With nonzero residuals, use the finite analytic shape-score list on the established cutoff, including shape one and the cutoff endpoint, and the unique conditional scale for each shape. Compare to the centered uniform, keeping all finite winners. At all-zero residuals collapse gives the empty set. Two unequal nonzero radii separate finite winners even if one has shape one. For a constant sample away from the mean, the strictly increasing shape profile gives the uniform escape. The large-multiplicity \(h_*\) crossing specified in the SGED held-mean-and-skew section retains its two separated shapes and respective scales. [Root list and crossing](2026-11-27-sged-atlas-math-report.md#ged-held-mean)

### ged held mean+sd

**Proved.** At two unequal nonzero radii, finite candidates have separate sample gradients. Therefore almost every sample of size at least two has a unique finite fit when attained. The held-width uniform contact is compared at the known partner value, not treated as a second finite shape. The \(a_*,b_0\) construction specified in the SGED held-mean, sd and skew section gives the exact separated finite crossing. With all residuals zero the peak decreases strictly with shape and \(p=1\) alone wins. A full pointwise ordering of the shape roots on exceptional samples remains open.

### ged held sd

**Proved.** At four or more levels the generically winning class is a superunit singleton or the full median set at \(p=1\). The strict small-sd condition gives an open set of median intervals at every even size. The sample \((-1,1)\), sd \(1/10\) is an exact example even below that level threshold. The \(a_*,b_0\) crossing remains a finite shape tie with free mean, because at each fixed shape zero remains the unique conditional median or strictly convex minimizer on that sample. Compare the fixed-width uniform at exact contact and whenever feasible.

## What this changes for the fitter

An almost-everywhere assertion supplies no pointwise certificate on binary64 data. Preserve unknown cardinality when a finite value is certified but competing finite maximizers have not been excluded. A verified winning Laplace shape with balanced conditional slopes should retain the entire segment and its common scale, where eligible. At a proved reflected pair, keep both partners and claim a complete set only when other orbits have been excluded. Tied excluded ends do not add finite estimates. When the existing search succeeds but its global certificate does not close, retain its result marked uncertified under the existing rule. A proved refutation or a search failure follows the existing failure path.

## Open

- Skew Student: settle the unconstrained low-level strata left by the score counts, notably unequal three-level counts in the all-free problem, and all low-level face comparisons that do not have the stronger TASK-412 proof. Identify skew labels on a common critical-collapse envelope, isolate roots effectively with free shape, and count the winning shapes and reflection orbits in the given examples. In the fixed-shape exact rules, general effective height equality with arbitrary real powers remains open.
- SGED: establish or refute pure Laplace/Laplace generic separation with mean held and skew free. The moving-contact derivative in the pinned-mean chart has no translation sum identity, and equal constant slopes on occupied sides need not determine the join. Equal-gradient Laplace finite/half and same-orientation half comparisons need separate equality predicates. Low-level and pinned-contact strata beyond the stated formulas need joint nuisance comparisons. Determine which balanced asymmetric skew count patterns actually have open sets with a winning estimated Laplace shape.
- GED and fixed-skew SGED: resolve the remaining low-level mixed-shape comparisons and give effective exact equality tests for separated shape branches, including the question of a binary64 shape tie. The given real crossings settle existence of ties, while general orbit and shape counts need global root exclusion.
- Beyond the exact candidate prescriptions, the nondifferentiability description of the exceptional locus still uses the global optimum. Find explicit data predicates that locate and count all winning branches.

## Checks

The workstation checks used the one-slot lock. The symbolic replay checks the score intersections, scale monotonicity, two-point factorization and SGED contact derivative. The inherited interval scripts check the exact witness comparisons. The artifact check covers all 32 regime headings, relative links and atlas proposals. This research-only job changed no fitter, dossier or test, and ran no builds or package suites.

Reproduce from the worktree root.

~~~sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export PYTHONDONTWRITEBYTECODE=1
eval_dir=dev/research/evaluations
/tmp/umvl-cpu/run python3 "$eval_dir/2026-11-27-uniq-skew2-math-data/check_algebra.py"
/tmp/umvl-cpu/run python3 "$eval_dir/2026-11-27-sstd-atlas-math-data/check.py"
/tmp/umvl-cpu/run python3 "$eval_dir/2026-11-27-sged-atlas-math-data/check.py"
/tmp/umvl-cpu/run python3 "$eval_dir/2026-11-25-ged-math-data/cover.py"
/tmp/umvl-cpu/run python3 "$eval_dir/2026-11-27-uniq-skew2-math-data/validate.py"
~~~

[Proposed uniqueness rows](2026-11-27-uniq-skew2-math-data/proofs.csv)
