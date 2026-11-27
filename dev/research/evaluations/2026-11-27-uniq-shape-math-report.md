# Generic uniqueness for positive-support shape families

TASK-416 · 2026-11-27 · Base `e5392fec`

**Proved.** The scoped results below cover all 52 requested regimes. Every almost-everywhere result is conditional on finite attainment and names the minimum number of distinct levels. The smallest thresholds come from exact score intersection counts. For several other models the finite score comparison is proved only above a dimension bound. The generalized-beta unit-upper-shape ridge persists on open data domains. Triangular and held-scale generalized gamma can have multiple maxima on open sets at sample size two.

The [shared proofs](2026-11-27-uniq-shape-math-data/method.md) give the score calculations, support restrictions, face comparisons and null-set construction. Every occurrence of “generic” below means almost every sample in level measure at fixed ordered multiplicities and holds. Counts in brackets indicate replication. A finite critical-value comparison specifies the exact maximizing set when the listed candidates cover the entire regime, including all exclusions and contacts. On such a list the exceptional samples are precisely those with distinct candidates attaining the common largest feasible value. A numerical tolerance between values supplies no equality test. Prior finite ties and face ties are retained, including those on smaller strata.

## Summary

| Regime | Previous atlas claim | New result |
| --- | --- | --- |

| betapr held all free | There are exact two finite maxima tied with both faces. | generic ≥3 |
| betapr held shape1 | Conditional global uniqueness can fail. | generic ≥3 |
| betapr held shape2 | Conditional global uniqueness can fail after reciprocal transfer. | generic ≥3 |
| burr held all free | Exact samples can have two finite co-winners above both faces. | generic ≥4 |
| burr held shape2 | Exact held-power samples can tie. | generic ≥2 and two-level singleton |
| expweibull held all free | An exact real sample ties two finite global maxima. | generic ≥7 |
| expweibull held scale | At positive held scale on the nonconstant attained branch, the complete maximizing set consists of the winning finite shape roots in a compact witness superlevel with their uniquely profiled exponents. | generic ≥5 |
| fatigue held alpha | On nonconstant data the stationary set for free threshold and scale at held shape has finitely many semialgebraically connected components, each of constant likelihood. | generic ≥5 |
| fatigue held alpha+beta | At positive held scale the threshold fit is unique for every sample if the held shape squared is at most one half, and for constant data at any positive held shape. | exact root set and generic ≥3 |
| fatigue held alpha+mu | For constant samples a singleton occurs at alpha at most two and a reciprocal pair at alpha above two. | exact root set and generic ≥3 |
| fatigue held beta | The held-scale threshold profile has a finite stationary list with a unique conditional shape per root, and the exact sample (0,1,1) at held scale one has a unique fit. | exact root set and generic ≥5 |
| fpareto held min | On nonconstant positive excesses each fixed beta-shape pair has a unique conditional power and scale, and the joint stationary set has finitely many connected components. | generic ≥4 |
| fpareto held min+shape1 | On nonconstant positive excesses each free lower beta shape has a unique conditional power and scale, with finitely many connected stationary components. | generic ≥4 |
| fpareto held min+shape3 | On nonconstant positive excesses each free upper beta shape has a unique conditional power and scale, with finitely many connected stationary components. | generic ≥4 |
| fpareto held scale+shape2+shape3 | Unique at a=1. | generic ≥5 and contact singleton |
| frechet held all free | On two-level samples below half, both profile endpoints can compete and a winning finite shape one is unique. Some multi-level real samples have two finite global maxima. | generic ≥7 |
| frechet held sigma | Exact real data can tie shape one with an interior shape. | generic ≥5 |
| gb2 held all free | Guarded numerical comparisons support a unique finite winner on the all-free golden and its exact log-affine transforms. | generic ≥4 |
| gb2 held shape1 | Not unique in general: with shape1=1, the reciprocal of the replicated Burr crossing sample specified in the report has separated finite global winners. | generic ≥4 |
| gb2 held shape3 | Not unique in general: with shape3=1, the replicated Burr crossing sample specified in the report has separated finite global winners above both faces. | generic ≥4 |
| genbeta held scale | A strict finite witness above both faces gives finitely many maximizing powers with unique beta-shape fits at each power. | generic ≥4 |
| genbeta held scale+shape2 | Not unique in general: with held scale 1 and shape2 1, x=(1/2,1/4) has the maximizing ridge shape1*shape3=2/(3 log 2). | generic ≥4 off unit; exact unit ridge |
| genbeta held shape2 | Not unique in general: with held shape2 1, x=(1/2,1) has the maximizing ridge scale=1 and shape1*shape3=2/log 2. | generic ≥7 above unit; exact unit ridge |
| gengamma held all free | Not unique in general: x=(1/4,(1/2) repeated 10000 times,2 repeated 10000 times,4) with all coefficients free has distinct reflected global maximizers, with a finite signed candidate list and the lognormal seam. | exact comparison and generic for all n |
| gengamma held mu | With location held and at least one nonzero log residual, compare the two finite signed stationary lists and the normal seam with the available faces to obtain the entire maximizing set, which is a singleton for sign-symmetric log residuals. | exact comparison and generic ≥3 |
| gengamma held mu+sigma | If every log observation equals held mu, the unique shape is Q=0. | generic ≥3 |
| gengamma held sigma | Not unique in general: x=(1/4,4) with sigma=1 has distinct reflected global maximizers. | generic ≥3 |
| gev held all free | A nonconstant sample can have two distinct all-free maxima. | generic ≥7 |
| gev held mu | An exact held-location sample ties the attained endpoint and another finite fit. | generic ≥5 |
| gev held sigma | Exact samples tie the reverse exponential with another attained point. | generic ≥5 |
| gpd held mu | An exact sample ties an interior shape with the attained uniform. | generic ≥2 |
| gpd held mu+xi | Unique for positive excesses and fixed xi≥−1. | exact set |
| invburr held all free | Reciprocation transfers exact Burr nonuniqueness examples. | generic ≥4 |
| invburr held shape2 | Not unique in general: the exact reciprocal Lomax crossing x=(1/a_*,1 repeated 10 times,1/100 repeated 10 times) at shape2=1 has exactly two global maximizers. | generic ≥2 and two-level singleton |
| invparalogis held all free | Reciprocity transfers exact all-free ties. | generic ≥4 |
| invparalogis held scale | Reciprocity transfers the exact held-scale tie to inverse paralogistic. | generic ≥4 |
| invpareto held all free | Reciprocity transfers the Lomax two-level uniqueness and the three-level tie. | generic ≥2 and two-level singleton |
| invtrgamma held all free | Not unique in general: the exact replicated-outlier crossing sample specified in the report with all coefficients free has separated finite global maximizers. | exact comparison and generic for all n |
| invtrgamma held scale | When mean oriented log excess is zero, the scale-held profile has a unique maximum. | exact comparison and generic ≥3 |
| lomax held all free | Two-level samples attain uniquely when a fit exists; some three-level samples have two global winners. | generic ≥2 and two-level singleton |
| paralogis held all free | Exact all-free samples can have distinct global maxima. | generic ≥4 |
| paralogis held scale | A real two-point sample with integer multiplicities has two distinct maximizing shape bands for the fixed held scale. | generic ≥4 |
| pareto2 held min | Not unique in general: the exact Lomax crossing x=(a_*,1 repeated 10 times,100 repeated 10 times) at min=0 has exactly two global maximizers. | generic ≥2 and two-level singleton |
| pareto4 held min | Exact Burr ties transfer on strictly positive excesses. | generic ≥4 |
| pareto4 held min+shape2 | Not unique in general: the exact Lomax crossing x=(a_*,1 repeated 10 times,100 repeated 10 times) at min=0 and shape2=1 has exactly two global maximizers. | generic ≥2 and two-level singleton |
| pareto4 held scale+shape2 | Unique at a=1. | generic ≥5 and contact singleton |
| pgw held scale | On the nonconstant attained held-scale branch the full set consists of the winning stationary shapes in a compact interval with their unique conditional exponents. | exact comparison and generic ≥5 |
| pgw held shape | Finite critical-value comparison identifies the maximizing set. | exact comparison and generic ≥5 |
| sgomp held all free | One exact real sample ties the attained exponential member and a positive-shape finite maximum. | exact comparison and generic ≥5 |
| trgamma held all free | An exact finite/power equality retains the attained fit. | exact comparison and generic for all n |
| trgamma held scale | When mean oriented log excess is zero, the scale-held profile has a unique maximum. | exact comparison and generic ≥3 |
| triang held all free | On x=(0,1), two reflected all-free parameter triples attain the same global maximum. | exact set and generic ≥3 |

## Regime proofs


### betapr held all free

**Proved.** The fixed-power logistic proof separates two finite fits and either gamma face on three levels. Use the compact scalar ratio comparison against both optimized gamma heights. The exact log-symmetric sample \(512\log 2\,(-1,(-1/2)^{[10]},(1/2)^{[10]},1)\) has a reflected pair. The \(10^6(-1,(-s_*)^{[10]},s_*^{[10]},1)\) least-crossing log sample has exactly two finite winners at both gamma heights. Both belong to the level-stratum exceptional set. The six-observation wide-log singleton class remains pointwise. The symmetric four-way equality uses the unique zero of the finite-minus-face gap in the cited interval. [Prior comparison](2026-11-25-betapr-math-report.md#all-free-exact-samples)


### betapr held shape1

**Proved.** The fixed-power logistic separation applies against finite fits and the compatible gamma face. At unit lower beta shape the Lomax theorem gives pointwise uniqueness on two levels and the exact three-level Lomax crossing gives two. At unit upper shape take its reciprocal. The relevant gamma or exponential face can tie a single finite point on the two-level threshold construction. [Prior comparison](2026-11-25-betapr-math-report.md#one-shape-held)


### betapr held shape2

**Proved.** The fixed-power logistic comparison and the shape1 result transfer by reciprocation, which interchanges the beta shapes. The reciprocal unit-shape Lomax crossing still has exactly two finite winners. [Prior comparison](2026-11-25-betapr-math-report.md#held-unit-shape-and-a-real-equality)


### burr held all free

**Proved.** Use the free-power logistic bound and compare to Weibull and the minimum cutoff. Both faces separate on four levels. With \(N=2^{90}\), use the exponentials of \(N\) copies of each entry of \((0,.23826u_*,7u_*/15,8u_*/15,8u_*/15,1,1)\), and one extra \(e^{-1}\). The least crossing \(u_*\) of the low- and high-shape compact profile heights in the cited proof gives separated finite winners above both faces. [Prior comparison](2026-11-25-burr2-math-report.md#two-finite-global-maximizers-above-both-faces)


### burr held shape2

**Proved.** For held power \(c>0\), \(u=x^c\) gives Lomax with the exponential face transported to Weibull. Two levels give pointwise uniqueness when attained, and higher-level ties are null. One exact tie uses powered observations \((1,e^{5.1},e^{r_*},e^8)\), with \(r_*\) the unique low/high height crossing in \((5.99,6.02)\). The three-level Lomax crossing also transfers. [Prior comparison](2026-11-25-burr-math-report.md#held-power-has-an-exact-global-tie)


### expweibull held all free

**Proved.** Use the exponentiated-Weibull score incidence. The inverse-Weibull face separates for \(m\ge6\). The power cutoff separates with two noncontacts because the EW log-data score decreases strictly: \(h(u)=u/(e^u-1)\) has \(-1<h'<0\), and \(1-u+(c-1)h(u)\) decreases for all \(c>0\). The exact sample \((1^{[510976]},2^{[513024]},2^{v_*})\), with \(v_*\) the first equality of the two profile heights in the cited proof, is below the proved separation range. The theorem leaves its within-region count open. [Prior comparison](2026-11-25-expweibull-math-report.md#two-finite-local-maxima-and-an-exact-real-global-tie)


### expweibull held scale

**Proved.** On the attained branch the shape profile has a compact finite list, each root with the unique exponent \(c=1/\overline{-\log(1-e^{-u_i})}\). The other branches have the held-scale power or collapse outcomes. Score incidence gives a null tie set for five levels. A universal height order or a held-scale tie on fewer levels remains open. [Prior comparison](2026-11-27-uniq-atlas-math-report.md#expweibull-held-scale)


### fatigue held alpha

**Proved.** The rational score distinguishes parameters as functions and the two-free-coordinate incidence gives the stated null tie set. For nonconstants all ends vanish, so the complete set is the union of the stationary components at the largest attained height. No positive-dimensional winning component occurs off the null set in this range. Constants collapse. The proof leaves lower-level winning component dimension open. [Prior comparison](2026-11-27-lsheld-atlas-math-report.md#fatigue-held-alpha)


### fatigue held alpha+beta

**Proved.** Both threshold tails vanish for every nonempty sample. With \(d_i=x_i-x_{\min}\), take all positive roots in \(t=x_{\min}-\mu\) of \(\sum_i g(t+d_i)=0\) using the fatigue score. Clear its positive denominators. The numerator is nonzero because the log likelihood vanishes to minus infinity at the ends. Precisely the roots with highest likelihood maximize. This finite comparison covers every sample and excludes a flat interval. The one-coordinate incidence gives generic uniqueness on three levels. The prior pointwise singleton for \(\alpha^2\le1/2\), and on all constants, survives. [Prior comparison](2026-11-27-lsheld-atlas-math-report.md#fatigue-held-alphabeta)


### fatigue held alpha+mu

**Proved.** On positive excesses take exactly the highest-likelihood roots of the rational scale score. The prior turning-point criterion gives at most two local maxima and vanishing likelihood at either end. The one-coordinate incidence gives the null tie set with three levels. Log-symmetric excesses give reciprocal scale pairs about their geometric mean precisely for \(\alpha^2>4\sum_i\cosh z_i/\sum_i\operatorname{sech}^2(z_i/2)\), where \(z_i\) are centered log excesses. In particular every two-observation sample is log-symmetric, and for any fixed \(\alpha>2\) a sufficiently small log spacing gives two winners on an open set. At threshold zero, \((1/2,2)\) has critical squared shape \(45/8\), so \(\alpha=3\) gives exactly two. Constants give two for \(\alpha>2\) and one otherwise. For algebraic inputs the prior Hermite–Lindemann test compares the two outer roots by exact algebraic conditions. A support violation gives no positive fit. [Prior comparison](2026-11-25-fatigue-math-report.md#number-of-local-maxima)


### fatigue held beta

**Proved.** Set \(k\) to the minimum multiplicity. Constants and \(3k>n\) are unbounded. For \(3k\le n\), take the positive stationary roots of the rational squared profile \(W\), compare to zero or to \(W(0+)\) at equality, and retain all roots tied at the greatest height if they reach it. Shape is the unique \(\sqrt Q\) at each. This is an exact algebraic comparison on rational data. The face contact derivatives separate in measure for \(m\ge4\) and finite/finite for \(m\ge5\). The exact singleton \((0,1,1)\) at scale one is on the critical stratum. [Prior comparison](2026-11-27-lsheld-atlas-math-report.md#fatigue-held-beta)


### fpareto held min

**Proved.** Strictly positive excesses give the full GB2 logistic proof, including all six exterior types. Finite fits are unique almost everywhere for four levels. The normal, gamma, log-asymmetric-Laplace and cutoff comparisons also separate on four levels. The stationary-component comparison includes all finite winners, but the golden or any particular low-level fit still needs a pointwise proof. For zero excess with free power the endpoint closure permits a spike. [Prior comparison](2026-11-27-lsheld-atlas-math-report.md#fpareto-held-min)


### fpareto held min+shape1

**Proved.** Subtract the held threshold and use the GB2 right-shape hold. The inverse transformed gamma and upper cutoff are the only competitive positive faces. Finite and face scores separate with four levels. The exact reciprocal Burr crossing transfers at held upper beta shape one. Fixing the upper shape leaves the lower-end spike possible at an observed threshold in the density closure. [Prior comparison](2026-11-27-lsheld-atlas-math-report.md#fpareto-held-minshape1)


### fpareto held min+shape3

**Proved.** Use the GB2 lower-shape hold on positive excesses. Transformed gamma and the lower cutoff separate at four levels. The Burr crossing transfers at unit lower shape. A held positive lower shape still permits a subunit product with free power at an observed threshold in the density closure. [Prior comparison](2026-11-27-lsheld-atlas-math-report.md#fpareto-held-minshape3)


### fpareto held scale+shape2+shape3

**Proved.** With \(a=cp\), the subunit case has infinite supremum. At \(a=1\), the closed minimum and the unique fitted beta upper shape give the whole set on nonconstants; constants diverge. Above one, both free coordinates are interior on the attained branch and the Feller–Pareto incidence gives generic uniqueness. At each threshold the strictly concave beta-shape likelihood has a unique root; its existence follows since the shape score decreases from infinity to the strictly negative average of \(-\log(1+h_i)\). The profiled score is analytic and nonconstant by the established exterior bounds. Ties reduce to comparing the competing finite roots on the positive compact superlevel. [Prior comparison](2026-11-25-thresh-math-report.md#fixed-scale-and-free-upper-tail-shape)


### frechet held all free

**Proved.** The Fréchet score, Gumbel face and minimum-half collapse trace are as in the score charts. Gumbel versus finite separates for \(m\ge6\), and the minimum-half stratum has no finite maximum. The pointwise two-level attainment and singleton tests still apply. The exact \((0,1,r_F)\) uses the least crossing in \([13/5,27/10]\) of the compared shape bands; its two finite winners lie below the proved separation range. [Prior comparison](2026-11-25-frechet-math-report.md#fréchet-all-free)


### frechet held sigma

**Proved.** At held public scale the unique location root at each shape and the included shape-one branch yield the candidate comparison. There is no competing Gumbel face on this constrained route. The finite score incidence gives the null tie set. At normalized scale one the prior exact \((0,1,r_S)\) least crossing in \([39,40]\) ties the shape-one and interior branches. [Prior comparison](2026-11-25-frechet-math-report.md#fréchet-single-holds)


### gb2 held all free

**Proved.** Four levels separate finite fit scores. The two gamma faces, two cutoff faces, log-asymmetric-Laplace face and normal face also separate from finite fits on four levels. Constants concentrate. The golden comparison and its log-affine transfers are numerical, based on guarded long-double comparisons; this proof gives no pointwise conclusion for the golden, and the exact trough establishes only local nonunimodality. [Prior comparison](2026-11-27-atlas-mrev2-report.md#proof-errors-and-corrections)


### gb2 held shape1

**Proved.** The logistic score and inverse transformed-gamma and power cutoff faces separate with four levels. At unit upper shape the reciprocal of the replicated Burr crossing above is exact. Its finite ties lie in the compact-envelope exceptional set. The reciprocal finite/Weibull equality is a finite/inverse-Weibull equality. [Prior comparison](2026-11-27-genbeta-atlas-math-report.md#gb2-held-shape1)


### gb2 held shape3

**Proved.** The logistic score and transformed-gamma and Pareto cutoff faces separate with four levels. Holding the lower shape at one gives exactly Burr XII, so the displayed replicated Burr crossing supplies separated finite winners. The same reduction gives the prior finite/Weibull equality. [Prior comparison](2026-11-27-genbeta-atlas-math-report.md#gb2-held-shape3)


### genbeta held scale

**Proved.** On strict interior nonconstant data the four-rate bound separates finite candidates, including the unit-shape member. Against the fixed-endpoint gamma face use six levels; the moving power face separates at two noncontacts for nonunit shape, and strictly beats the unit-shape slice. At fixed power the beta fit is strictly concave; the prior finite stationary comparison above both faces covers strict witnesses. Ordering roots at equality and a pointwise golden proof remain open. [Prior comparison](2026-11-27-genbeta-atlas-math-report.md#genbeta-held-scale)


### genbeta held scale+shape2

**Proved.** For \(q\ne1\) use fixed-endpoint logistic-denominator separation. The gamma face has fixed shape and endpoint and one free rate, so incidence separates at \(m\ge4\). The additional fixed-endpoint power face for \(q<1\) has constant score and separates at two levels; for \(q>1\) it loses by the inward bound. At \(q=1\) every strictly interior sample has the full ridge \(pa=1/\overline{\log(b/x_i)}\). At \(b=1,x=(1/2,1/4)\) it is \(2/(3\log2)\). This multiplicity holds on an open data domain. [Prior comparison](2026-11-27-genbeta-atlas-math-report.md#genbeta-held-scaleshape2)


### genbeta held shape2

**Proved.** For \(q<1\) the endpoint spike rules out a finite maximum. For \(q=1\) on nonconstants the entire maximizing set is \(b=x_{\max},pa=1/\overline{\log(x_{\max}/x_i)}\), including the example \((1/2,1)\) with rate \(2/\log2\). This ridge exists throughout the nonconstant domain. Above unit use the free-endpoint score incidence; support contact has zero likelihood. The shifted gamma separates at six levels and the moving power at two noncontacts. Constants collapse on the \(q\ge1\) branch. [Prior comparison](2026-11-27-genbeta-atlas-math-report.md#genbeta-held-shape2)


### gengamma held all free

**Proved.** TASK-412 already proves three-level separation for finite signed candidates, the attained normal seam and both moving endpoint faces. Its two-level theorem excludes all finite fits and constants collapse. The exact signed-root comparison gives the complete set, retaining all finite candidates reaching the largest value, including an equal face height. The dyadic log-symmetric \((1/4,(1/2)^{[10000]},2^{[10000]},4)\) has a reflected pair and lies in the exceptional set. The number of reflected orbits is open. [Prior comparison](2026-11-27-uniq-generic-math-report.md#pilot-3-prentice-generalized-gamma-all-free)


### gengamma held mu

**Proved.** Use the two signed lists, the attained normal seam and the available fixed-endpoint power or Pareto traces. The inherited strict end signs give finite lists on every nonzero-residual sample and the exact height rule gives the whole set; all-zero residuals collapse. The gamma-score comparison gives three-level genericity, including finite/face. On sign-symmetric nonzero residuals the normal is the unique winner by the Stirling bound, regardless of level count. [Prior comparison](2026-11-27-uniq-atlas-math-report.md#gengamma-held-mu)


### gengamma held mu+sigma

**Proved.** Restrict the finite gamma score comparison to this one-shape chart, including the normal seam. Fixed positive scale makes the density peak vanish at infinite \(|Q|\), so the tails are irrelevant. The scaled peak is \(k^{-1/2} k^k e^{-k}/(\sigma\Gamma(k))\) with \(k=Q^{-2}\). The all-zero residual fit is uniquely normal by strict Stirling. For small nonzero samples winning heights and any degenerate seam remain open. [Prior comparison](2026-11-10-gg-theory-report.md#mu-and-sigma-held-q-free)


### gengamma held sigma

**Proved.** At each nonzero shape the log location is uniquely profiled; the attained normal seam uses the sample mean. Both infinite-shape tails vanish by the uniform peak bound. On three levels finite and seam scores separate. Every two-observation log sample is reflection symmetric about its log mean, and when a nonzero shape wins it has a distinct partner. In particular \((1/4,4)\) at scale one beats the normal, giving an open region of ties in the ordinary two-observation experiment by the strict quartic-coefficient test \(d>\sigma\), where \(d\) is half the log range. [Prior comparison](2026-11-27-uniq-atlas-math-report.md#gengamma-held-sigma)


### gev held all free

**Proved.** Separate positive and negative charts, the Gumbel seam, and the attained reverse-exponential contact. Include the minimum-half collapse trace as in the critical criterion. Smooth finite charts separate by incidence on seven levels, and reverse-exponential pairs require at most five parameters. The exact \((0,t_*,1,2-t_*)\) with the least crossing \(t_*\) in \([.29494,.29497]\) ties the closed reverse exponential with one positive shape. This is below the proved level bound. [Prior comparison](2026-11-25-gev-math-report.md#exactly-two-all-free-global-maxima)


### gev held mu

**Proved.** At held location the reverse exponential can be at the maximum contact or have slack. Use both feasible height charts and the fixed-location critical collapse trace on its tie stratum. All finite score comparisons have pair dimension at most four. The exact \((0,t_L,1,2-t_L)\) with held location \(3t_L/4\), \(t_L\) the last strict-better transition in the prior proof, has separated finite winners. Once fixed at its exact value this hold is compatible with the sample-stratum theorem. [Prior comparison](2026-11-25-gev-math-report.md#global-ties-with-public-scale-or-location-held)


### gev held sigma

**Proved.** All ordinary samples attain; the unique nuisance fit at each feasible shape is compared to the Gumbel seam and restriction endpoints, with reverse exponential at the maximum. The score incidence covers five levels. Constants have a unique reverse exponential by the density bound. The exact \((0,r_S,r_S,r_S)\) at scale one uses the first strict-better transition \(r_S\in(4,100)\) and lies on the two-level stratum. [Prior comparison](2026-11-25-gev-math-report.md#scale-held)


### gpd held mu

**Proved.** On strict positive excesses the nonuniform score is \(-(1+\xi)/(\sigma+\xi y)\). Two distinct pairs with \(\xi>-1\) coincide at at most one level, and the optimized uniform has zero gradient off its maximum contact, unlike every nonuniform. The uniform scale is the largest excess. Thus every positive two-level or higher stratum has a null tie set, containing the exact \((a_*,a_*,1)\) at threshold zero from the cited maximum-defined construction. Positive constants have only the tight uniform: optimizing scale in each nonuniform member gives its peak at scale equal to the observation with value strictly below the reciprocal excess. Zero excesses allow divergence when shape is free. The fitted scalar profile and uniform comparison remain the exact candidate rule. [Prior comparison](2026-11-05-gpd-cert-report.md)


### gpd held mu+xi

**Proved.** On positive excesses with \(-1<\xi\) the log-scale score strictly decreases and has exactly one zero. At \(\xi=-1\) the tight uniform is unique unless every excess is zero, when scale collapse is unbounded. With \(k>0\) zeros, \(\xi\le0\) and at least one positive excess the same one-root rule or tight uniform applies; for positive \(\xi\) use the exponent \((n-k)/\xi-k\), with singleton, finite unattained supremum, or divergence as the sign is positive, zero, or negative. All-zero excesses diverge. These claims concern the fitted shape domain; the reference subuniform branch has singular upper contacts. [Prior comparison](2026-11-27-lsheld-atlas-math-report.md#gpd-held-xi)


### invburr held all free

**Proved.** Reciprocation transports all finite and face comparisons of Burr, changing Weibull and lower cutoff to inverse Weibull and upper cutoff. Reciprocate the displayed replicated-log Burr crossing for exact co-winners. [Prior comparison](2026-11-25-burr-math-report.md#inverse-and-shifted-burr)


### invburr held shape2

**Proved.** Transform by \(x^{-c}\), giving Lomax and exponential faces in powered units. Two levels give pointwise uniqueness whenever attained. The exact sample \((1/a_*,1^{[10]},(1/100)^{[10]})\) with \(c=1\) has exactly two global maxima and belongs to the three-level null tie locus. [Prior comparison](2026-11-27-uniq-atlas-math-report.md#invburr-held-shape2)


### invparalogis held all free

**Proved.** Reciprocation transports the logistic score bound, all-free compactness and the exact two-level tie below. There are no positive excluded competitive faces on nonconstants; the entire set is the reciprocal image of the finite profile winners. [Prior comparison](2026-11-25-paralogis-math-report.md#reciprocal-transfer-and-verification)


### invparalogis held scale

**Proved.** Reciprocate both the sample and the held scale. The held-scale paralogistic finite-root comparison and logistic score bound apply with unchanged cardinality. The exact scale-one sample \((e^{1/1000},e^{r_*})\) gives two winners below the proved level bound. [Prior comparison](2026-11-25-paralogis-math-report.md#held-scale)


### invpareto held all free

**Proved.** Invert the data to obtain Lomax, including its exponential face. The exact reciprocal of \(X_*=(a_*,1^{[10]},100^{[10]})\) has exactly two finite winners. On two positive levels the finite fit is pointwise unique whenever attained. [Prior comparison](2026-11-25-lomax-uniq-math-report.md#transfer-and-lean)


### invtrgamma held all free

**Proved.** Reflect the power-gamma scores. Constants collapse and the all-free two-level Prentice bound excludes finite attainment. For all other samples the signed finite root list and normal/Pareto comparison give the entire set; finite ties and finite/face ties are null. The prior exact construction is \(2^{-\widetilde Y_{h_*}}\), with \(\widetilde Y_h\) the \(m\)-fold replication of the twelve-level path with one extra log observation 100, and \(h_*,m\) chosen by the prior strict tests. The separated winning roots lie in the exceptional set. Reciprocating it also gives a trgamma finite/finite example. [Prior comparison](2026-11-27-uniq-atlas-math-report.md#invtrgamma-held-all-free)


### invtrgamma held scale

**Proved.** Reciprocate both data and scale. The held-scale gamma score separates on three levels. The finite list covers every bounded case; in the one-sided branch the endpoint Pareto trace is beaten strictly by its inward expansion. At mean oriented log excess zero and nonzero variance the power score decreases strictly and gives the prior singleton. Full winning root comparison at other small sample sizes remains open. [Prior comparison](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)


### lomax held all free

**Proved.** The physical rational score separates at two levels, both from another finite fit and from the exponential. The two-level singleton is pointwise whenever attained. Write \(X_*=(a_*,1^{[10]},100^{[10]})\), where \(a_*\) is the unique low/high rate height crossing in \([2^{-512},2^{-32}]\) of the cited proof. It has exactly two finite global winners above the face, and lies on the null three-level tie set. [Prior comparison](2026-11-25-lomax-uniq-math-report.md#exact-tie-construction)


### paralogis held all free

**Proved.** The logistic score is restricted by \(p=1,q=a\), so four levels suffice. Nonconstant positive data have compact competitive sets and constants collapse. The exact \((1,\exp(K_*/\epsilon^2)^{[9999]})\), \(\epsilon=10^{-4}\), has two winners at the unique band-height crossing \(K_*\in(7/5,8/5)\). Common rescaling of both levels preserves this tie. Changing the log spacing changes the paralogistic power restriction, so the construction does not give a whole-stratum tie. [Prior comparison](2026-11-25-paralogis-math-report.md#all-free-fit)


### paralogis held scale

**Proved.** Restrict the logistic score to the held scale, with no competitive excluded face. The exact two-observation tie at scale one is \((e^{-1/1000},e^{-r_*})\), \(r_*\) the unique crossing of the two shape bands in \((3/1000,5/1000)\). The generic result leaves the smaller strata open. The cited root and height bounds also give pointwise singleton classes. [Prior comparison](2026-11-25-paralogis-math-report.md#held-scale)


### pareto2 held min

**Proved.** Subtract the held threshold on its positive-excess domain to obtain Lomax and its exponential. \(X_*\) at threshold zero transfers with exactly two winners. The two-level singleton when attained is pointwise. At a zero excess the endpoint closure is unbounded with both shapes of the Lomax fit free, by shrinking scale at small enough tail shape. [Prior comparison](2026-11-27-uniq-atlas-math-report.md#pareto2-held-min)


### pareto4 held min

**Proved.** On positive excesses the full Burr theorem transfers by threshold subtraction. Its two faces, logistic separation and the displayed replicated-log crossing all carry over. An observed held threshold is outside the positive-excess regime and allows a subunit-power spike in the density closure. [Prior comparison](2026-11-25-burr-math-report.md#inverse-and-shifted-burr)


### pareto4 held min+shape2

**Proved.** Use \(u=(x-\min)^c\) with the power held. This gives the Lomax finite and face comparisons and pointwise two-level uniqueness. At threshold zero and power one \(X_*\) gives exactly two global maxima. A held contact below unit power diverges, above unit power has zero density, and at unit power allows the zero-aware Lomax divergence with tail shape free. [Prior comparison](2026-11-27-uniq-atlas-math-report.md#pareto4-held-minshape2)


### pareto4 held scale+shape2

**Proved.** With fixed scale and power \(c<1\), the minimum contact is singular. At \(c=1\) the included minimum and the analytic conditional upper-shape root give the entire finite set on nonconstants; constants diverge. For \(c>1\) and nonconstants the earlier compact threshold bounds and unique conditional shape leave an analytic scalar finite-root comparison. The Feller–Pareto score identity and incidence show generic uniqueness on five levels. The exact \((0,0,(41/100)^{[20]})\) at scale one and power 48 has two local maxima; equality of their heights was not established. [Prior comparison](2026-11-25-pareto34-math-report.md)


### pgw held scale

**Proved.** At scale above the minimum on nonconstants take the finite critical list with the unique profiled exponent and keep every candidate at its greatest height. On the other branches the strict threshold comparison or collapse gives an empty finite set. The score incidence gives generic uniqueness for five levels. At equality of held scale and minimum the threshold trace carries the \(2^{-k}\) penalty, and strictly beats finite points. [Prior comparison](2026-11-27-uniq-atlas-math-report.md#pgw-held-scale)


### pgw held shape

**Proved.** With shape held the finite list and optimized transformed Gompertz value give the full set, including an attained finite equality. Constants select the face. The PGW score incidence separates finite fits from each other with five levels and from the face with four. The ratio \((x_{\min}/x_{\max})^b\ge1/2\) still gives strict face dominance. [Prior comparison](2026-11-25-pgw-math-report.md#gompertz-decisions-and-the-finite-critical-list)


### sgomp held all free

**Proved.** Compare the finite positive-shape roots to the eligible attained exponential. The compact rate bounds and nonconstant analytic profile give the exact set on every nonconstant sample; constants diverge. The shifted-Gompertz score gives the stated finite/finite bound, and the exponential versus positive shape separates at two levels. The exact \((1^{[9]},M_*)\), with \(M_*\) the unique height crossing in \([8.60,8.62]\), belongs to the two-level exceptional set for that comparison and has exactly two global winners. [Prior comparison](2026-11-25-sgomp-math-report.md#exact-separated-maxima-and-the-crossing)


### trgamma held all free

**Proved.** The powered-gamma scores give three-level separation against finite fits and either face, the Prentice bound excludes all two-level fits, and constants diverge. The prior signed-root comparison with normal and power heights gives the exact whole set. The finite/power equality at the first crossing \(h_*\) of the twelve-log-level path retains the finite winner or winners. Reciprocation of the inverse finite/finite example gives separated finite co-winners strictly above the faces in trgamma as well. [Prior comparison](2026-11-25-trgamma-math-report.md#exact-finitepower-equality-over-the-reals)


### trgamma held scale

**Proved.** The held-scale gamma score separates on three levels. On every bounded case the two strict end signs prove a finite list of stationary powers, each with one shape. The power endpoint trace strictly loses to an inward witness whenever available in the bounded branch. For nonzero residual variance and mean zero the profiled score is strictly decreasing and proves the singleton. Nonnegative constants diverge, and the prior finite list includes negative constants. [Prior comparison](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)


### triang held all free

**Proved.** Use the knot proof below. All nonconstant maximizing sets are finite and each maximizing knot gives exactly one endpoint pair. Constants collapse. Balanced two-level counts always have two winners, hence the exact ordinary size-two tie \((0,1)\) extends to the whole two-observation chamber. Unequal two-level counts have one winner, and the exceptional set on higher level strata is the exact tied-knot comparison and is null. [Prior comparison](2026-11-27-uniq-atlas-math-report.md#triang-held-c)


## Triangular knot proof

**Proved.** Write \(c\) for the mode, \(a,b\) for the distances from the mode to the lower and upper support endpoints, and \(d_i,e_i\) for the distances of observations strictly to its left and right. At fixed endpoints the log likelihood is strictly convex on each gap of positive likelihood, since its mode-dependent part there is a sum of negative logarithms of positive affine functions. It strictly improves toward the sample range outside that range. At an observed support endpoint positive likelihood requires the mode there. Every maximum is therefore a data knot. For a fixed knot the total log likelihood is
\[
 n\log 2-n\log(a+b)
 +\sum_{i:x_i<c}\log(1-d_i/a)
 +\sum_{i:x_i>c}\log(1-e_i/b).
\]
An empty side has optimal width zero. For occupied sides, the domain in log widths is convex, each occupied log-width term is strictly concave in its own coordinate, and \(-\log(a+b)\) is concave in log widths. An occupied width approaching its largest distance sends a factor to zero; an infinite width sends the peak to zero. Thus each knot has a unique endpoint pair with positive likelihood. Compare these finitely many heights. The exact exceptional set is the union over distinct knot indices of the equality of their fitted heights with both at least as high as every other height. The whole maximizing set consists of these winning knots with their fitted endpoints. A constant sample collapses.

For exactly two levels at distance \(d>0\), a knot with fraction \(w\) of data on its occupied side has optimal width \(d(1+w)\), and mean log height \(\log(2/d)-\log(1+w)+w\log(w/(1+w))\). Its derivative in \(w\) is \(\log(w/(1+w))<0\). Thus the heavier knot wins, or both win at equal counts. There is no segment of modes.

For three or more levels, equality of composed gradients at distinct winning knots first forces them to be adjacent: a level between the modes has opposite score signs. Write \(i<j\) for the adjacent knots themselves and \(L,I,J,R\) for the total counts before \(i\), at \(i\), at \(j\), and after \(j\). Write \(A,B\) for physical endpoints when common. Off the knots the scores are \(1/(x-A)\) on the left and \(-1/(B-x)\) on the right. A shared exterior observation on a side forces a common endpoint on that side. The own-knot derivative, for the whole log product, is \(-L_c/(c-A)+R_c/(B-c)\) with \(L_c,R_c\) the counts strictly below and above the knot. It follows by differentiating at fixed physical endpoints and recalculating the mode; when a side is empty its width remains zero and differentiating its moving endpoint gives the same rule plus a peak term. That peak term must be included at an extreme knot.

When both exterior sides are populated, gradient equality at the two knots would give
\[
 (J+R)/(B-i)=(L+I)/(i-A),\qquad
 (J+R)/(B-j)=(L+I)/(j-A).
\]
The left side increases and the right side decreases, an impossibility. If \(i\) is the minimum, then \(L=0\), \(R>0\) and the upper endpoint is still common. Only the gradient equality at the interior knot \(j\) is needed in this case. The mode at \(i\) contributes an ordinary noncontact derivative at \(j\). Hold the common upper endpoint \(B\). For the extreme-knot candidate with lower endpoint \(i\) define the product using its unique optimized upper endpoint. Put \(d=j-i\), \(b=B-j\), \(a=j-A'\). The extreme score equality at \(j\) gives \(I/a=(J+R)/b\), so \(a/b=I/(J+R)\). At these endpoints the peak-normalized likelihood ratio of the interior candidate to the extreme one, using their common upper endpoint, is
\[
 \left(\frac{b+d}{a+b}\right)^n
 \left(1-\frac{d}{a}\right)^I
 \left(1+\frac{d}{b}\right)^{J+R}.
\]
Here \(0<d<a\). The weighted product of the last two factors is at most one by the weighted arithmetic-geometric mean inequality, since \(I/a=(J+R)/b\). The first factor is strictly below one since \(d<a\). Thus equal gradients cannot occur at a tie. Reflection treats the case with no exterior to the right. The case with neither exterior has just two levels. The compact comparison theorem now proves the higher-level assertion, including fixed unequal multiplicities and boundary modes.

## What this changes for the fitter

- Use the triangular knot comparison with the included endpoint-mode convention. It certifies two exact winners at balanced two-level counts by a count check. At unit upper shape in generalized beta report the identified rate and its full maximizing ridge.
- A proved singleton slice or a complete global comparison at a specified sample can support a pointwise uniqueness flag. A null tie set in level measure cannot certify singleton cardinality on binary64 inputs. Keep unknown uniqueness when value is certified but cardinality is unresolved. An excluded face at the same height leaves any finite winner in the maximizing set.
- Keep an accuracy-checked successful search result flagged uncertified if a global sign, face comparison or budget remains unresolved. Stop when a proof rules out that result or when the search itself fails. No changes to fitters, tests, or dossiers are proposed in this job.

## Open

- **Below the score bounds.** Distinct scores as functions do not prove separation at the observations when the level count is small. Except for the listed pointwise or structural branches, what is missing is a stationary-score elimination or a global height inequality on those smaller strata. This includes three-level Burr and GB2, two-level beta prime away from the held-unit cases, three- to six-level all-free exponentiated Weibull and Fréchet/GEV, and the corresponding two- to four-level held branches. The all-free exponentiated-Weibull and Fréchet/GEV crossings already rule out an everywhere-singleton theorem. The incidence bound leaves open whether additional ties there occupy positive level measure.
- **Support and components.** Below the stated incidence bounds, a held-shape fatigue or held-threshold Feller–Pareto stationary component can require a dimension check and comparison against every competitive height. Above unit exponent in the fixed-scale threshold profiles, the conditional root is unique but ordering distinct outer roots needs more information. Above unit held shape in free-endpoint generalized beta the shifted gamma and the power trace both remain relevant. For fixed-endpoint generalized beta, compare the inner stationary roots at the winning face height and close the golden with an independent enclosure.
- **Heights on the null sets.** The compact-envelope test gives a mathematical tie locus after a global comparison. A terminating implementation still needs effective isolation and critical-height decisions. Which exact witnesses have additional co-winners, which nonsymmetric strata have ties, and binary64 realizability of the real crossings remain open wherever the cited construction only gives a pair or uses real data. GB2's numerical golden singleton needs an outward comparison.
- **Joint face comparisons.** The finite/face separation scope uses the actual sample-dependent trace. Pairwise coincidence of two excluded faces can have a different locus, including exact equality of opposite log cutoffs on balanced two-level counts. General deciding equations for all optimizing face/face comparisons are open. They are separate from the cardinality of the finite fit.

## Checks

The workstation checks, each through one CPU slot, verify the new score formulas and triangular identities, reproduce selected exact witness checks, and validate the atlas-fragment schema, regime coverage, headings and local evidence anchors. The no-build rule for this research job leaves all package suites unchanged.

- [Proposed atlas rows](2026-11-27-uniq-shape-math-data/proofs.csv)
- [Symbolic and structural checker](2026-11-27-uniq-shape-math-data/check.py)
- [Shared proofs](2026-11-27-uniq-shape-math-data/method.md)

Run the checks from the worktree root.

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export PYTHONDONTWRITEBYTECODE=1
evaluation=dev/research/evaluations
/tmp/umvl-cpu/run python3 "$evaluation/2026-11-27-uniq-shape-math-data/check.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$evaluation/2026-11-25-lomax-uniq-math-data/exact_checks.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$evaluation/2026-11-25-sgomp-math-data/certificate.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$evaluation/2026-11-27-uniq-atlas-math-data/exact_checks.py"
```
