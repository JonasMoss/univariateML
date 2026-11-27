# Independent review of discrete and shape uniqueness rows

TASK-418 · Base `765e3fe4`

The reviewed fragment contains **67 rows** across 30 families, with **51 accepted, 16 regraded and 0 rejected**. Its final grades are **4 exact and 63 one-sided**, all with written evidence. There are 15 rows from TASK-413 and 52 from TASK-416. The replacement wording retains the proved tie witnesses and applicable earlier singleton or ridge cases. Every regrade changes the direction from exact to one-sided. The accepted all-free beta-binomial row also corrects the scope of the inherited claim. [Reviewed rows](2026-11-27-atlas-urev2-data/reviewed.csv)

Generic claims condition on finite attainment and use Lebesgue measure on the ordered distinct levels with multiplicities fixed. For the logistic scores, clearing two denominators gives at most four exponential rates and hence at most three intersections between distinct scores. For the dimension-based proofs, a candidate pair with at most d free coordinates gives an equal-score incidence set of dimension at most d when each remaining observation has a finite set of score intersections. Moving contacts use composed heights or account for the omitted level in the dimension bound. A support-boundary candidate was included only at a reachable joint trace or an attained endpoint. The four exact rows describe the maximizing set pointwise, including the relevant empty, flat, singleton and tied cases. [Generic framework](2026-11-27-uniq-generic-math-report.md#the-theorem) · [Shape score proofs](2026-11-27-uniq-shape-math-data/method.md) · [Discrete proofs](2026-11-27-uniq-discrete-math-report.md)

## Row verdicts

| Family | Held | Question | Verdict | Reason |
| --- | --- | --- | --- | --- |
| binom | all free | uniqueness | accepted | Strict continuous profile branches give a pointwise singleton or adjacent pair by local integer inequalities, and prime valuations exclude prime-power lower sizes. |
| bbinom | all free | uniqueness | accepted | Fixed-size algebraic comparisons and the signed or strict-witness tail bounds give scoped decisions, while the previous cited example proves only local multiplicity. |
| bbinom | alpha | uniqueness | accepted | The rational real-size chart gives finitely many monotonicity intervals and resolves integer-hold face equality, while the profiled quadratic determines nonzero seam signs. |
| bbinom | size | uniqueness | regraded | The rational likelihood gives a finite identifiable stationary list and a quantifier-elimination procedure, with singleton status proved on the stated size and histogram classes. |
| bnbinom | all free | uniqueness | accepted | The Poisson-mixture supporting inequality is decidable at algebraic critical points, and parameter symmetry counts labels separately for each winning orbit. |
| bnbinom | alpha | uniqueness | accepted | The fixed-ratio Euler product and its strictly monotone diagonal score prove the binary singleton, and the Lean example and outward golden cover prove off-seam global winners. |
| bnbinom | beta | uniqueness | accepted | The rational integer-hold formula and the profiled expansion give the scoped comparison. |
| bnbinom | size | uniqueness | accepted | The rational integer-hold formula and the profiled expansion give the scoped comparison under the exact size-beta swap. |
| cmb | all free | uniqueness | accepted | Strict covariance identifies each interior size profile and saturation at full support excludes every larger size. |
| cmb | nu | uniqueness | regraded | The rational-hold comparison is a procedure, and exact endpoint orders and continuity prove the real-hold tie. |
| dgamma | all free | uniqueness | accepted | The analytic profile has strict score tails, and the two-cell trace bound gives strict curvature under the stated uniform inequalities. |
| hyper | all free | uniqueness | regraded | The residue-tail and growing-corner arguments make the rational comparison terminate, while mixed binary samples give explicit population rays. |
| hyper | N | uniqueness | regraded | The slice ratios and rational height comparisons give a finite procedure, with exactly verified examples of distinct optimal designs. |
| hyper | k+m | uniqueness | accepted | The strict single crossing gives a pointwise singleton or adjacent pair by local inequalities, and the zero branches give an explicit ray or an empty set. |
| skellam | all free | uniqueness | accepted | The convolution of the Poisson supporting sum gives strict exclusion of any opposite-side rate, and the analytic profiled roots have no tail accumulation. |
| betapr | all free | uniqueness | accepted | The fixed-power score difference has at most two zeros and the prior interval crossing retains both finite winners at both face heights. |
| betapr | shape1 | uniqueness | accepted | The fixed-power score bound applies, and the unit lower shape is exactly the Lomax model with its global crossing. |
| betapr | shape2 | uniqueness | accepted | Reciprocation interchanges the held beta shapes and preserves both scores up to the common Jacobian and the maximizing set. |
| burr | all free | uniqueness | accepted | The four-term exponential bound gives at most three score intersections and the outward compact cover proves the replicated-log crossing above both faces. |
| burr | shape2 | uniqueness | accepted | Taking the held power transfers the two-level theorem, exponential face and exact global ties to and from Lomax. |
| expweibull | all free | uniqueness | accepted | Score asymptotics identify the parameters, incidence gives the seven-level threshold, and the prior interval bounds put both crossing winners above the faces. |
| expweibull | scale | uniqueness | accepted | The four-coordinate score incidence gives five levels and the strict attained branch keeps the prior compact profile list. |
| fatigue | alpha | uniqueness | accepted | Distinct poles identify the two free coordinates and the semialgebraic stationary components carry constant likelihood. |
| fatigue | alpha+beta | uniqueness | regraded | The rational score gives a finite root comparison procedure and the two-coordinate incidence gives the three-level threshold. |
| fatigue | alpha+mu | uniqueness | regraded | The rational scale score gives a comparison procedure, with three-level genericity by incidence and explicit symmetric pairs by reflection. |
| fatigue | beta | uniqueness | regraded | The rational squared profile gives an exact comparison procedure that includes equality at the critical contact. |
| fpareto | min | uniqueness | accepted | Positive excesses reduce to the logistic score bound, and the linked closure handles the contact and compatible faces. |
| fpareto | min+shape1 | uniqueness | accepted | Positive excesses reduce to the logistic score bound, and the linked closure handles the contact and compatible faces. |
| fpareto | min+shape3 | uniqueness | accepted | Positive excesses reduce to the logistic score bound, and the linked closure handles the contact and compatible faces. |
| fpareto | scale+shape2+shape3 | uniqueness | accepted | Above unit lower-end exponent the pole and tail identify both free coordinates, and at unit exponent strict concavity settles the closed contact. |
| frechet | all free | uniqueness | accepted | The support singularity identifies the three finite coefficients and the Gumbel and critical-collapse comparisons use separate scores. |
| frechet | sigma | uniqueness | accepted | The held-scale finite chart gives pair dimension four and the prior outward cover gives an exact crossing of the included shape-one and interior branches. |
| gb2 | all free | uniqueness | accepted | The exponential-sum bound separates finite fits on four levels even though the golden comparison remains only numerical. |
| gb2 | shape1 | uniqueness | accepted | The fixed-hold logistic comparison applies on four levels and the unit hold transfers the outward-certified Burr example. |
| gb2 | shape3 | uniqueness | accepted | The fixed-hold logistic comparison applies on four levels and the unit hold transfers the outward-certified Burr example. |
| genbeta | scale | uniqueness | accepted | Four exponential rates suffice for nonunit or mixed upper shapes, and the moving power face strictly dominates the otherwise unidentified unit-shape slice. |
| genbeta | scale+shape2 | uniqueness | accepted | Fixed-endpoint scores separate nonunit shapes and the identified rate gives the full unit-shape ridge. |
| genbeta | shape2 | uniqueness | accepted | The moving-endpoint pole identifies nonunit above-one shapes by incidence, and the unit power contact gives the exact ridge. |
| gengamma | all free | uniqueness | regraded | Three-level separation and two-level nonattainment prove generic uniqueness at every sample size, with a procedural comparison of the signed roots and seam. |
| gengamma | mu | uniqueness | regraded | The signed score bound and Stirling inequality give genericity and a pointwise symmetric singleton, while signed-list comparison alone is one-sided. |
| gengamma | mu+sigma | uniqueness | accepted | Signed and normal scores separate on three levels and strict Stirling gives the pointwise all-zero-residual result. |
| gengamma | sigma | uniqueness | accepted | Three levels separate the finite scores, while the positive quartic term at the normal produces the exact reflected pair construction at two levels. |
| gev | all free | uniqueness | accepted | The distinct support singularities and the composed reverse-exponential contact give the seven-level bound, and the interval certificate isolates the exact pair. |
| gev | mu | uniqueness | accepted | The held-location pair dimension is at most four including contact and slack branches, and the constrained crossing preserves separated winners. |
| gev | sigma | uniqueness | accepted | The held-scale score incidence covers the competing fitted charts, and the strict endpoint transition gives an exact two-level tie. |
| gpd | mu | uniqueness | accepted | Nonuniform scores have at most one intersection and the tight uniform has a zero noncontact derivative, while Lean proves the retained interior/uniform tie. |
| gpd | mu+xi | uniqueness | accepted | Strict scale-score monotonicity and the endpoint exponent classify each fitted-domain maximizing set as empty or a singleton, including zero excesses. |
| invburr | all free | uniqueness | accepted | Reciprocation transports the Burr score bound, both faces and the exact replicated-log crossing. |
| invburr | shape2 | uniqueness | accepted | The reciprocal held-power transformation gives the Lomax score and two-level result while preserving its exact co-winners. |
| invparalogis | all free | uniqueness | accepted | Reflection of log observations preserves the logistic bound and transports the exact two-level global crossing. |
| invparalogis | scale | uniqueness | accepted | Reciprocating both the observations and held scale preserves the global crossing and score-separation bound. |
| invpareto | all free | uniqueness | accepted | Reciprocation preserves the rational Lomax separation, the two-level singleton and both winners at the exact crossing. |
| invtrgamma | all free | uniqueness | regraded | The reflected signed score and two-level nonattainment prove generic uniqueness, while selecting the highest roots after face comparison is procedural. |
| invtrgamma | scale | uniqueness | regraded | Three levels give exponential-score separation and strict end signs leave a finite list for the root-height comparison procedure. |
| lomax | all free | uniqueness | accepted | The physical score has at most one intersection for distinct fits and the cited exact interval proof settles all two-level fits and the three-level crossing. |
| paralogis | all free | uniqueness | accepted | The logistic restriction retains the four-level bound and the validated band comparison gives two winners on the exact two-level crossing. |
| paralogis | scale | uniqueness | accepted | Fixing the scale preserves the logistic bound, and the outward held-scale comparisons certify the two shape bands at the crossing. |
| pareto2 | min | uniqueness | accepted | The positive-excess transform preserves the exact Lomax crossing, the face comparison and pointwise two-level uniqueness. |
| pareto4 | min | uniqueness | accepted | Subtracting the held threshold gives Burr on positive excesses and preserves its exact crossing and score bound. |
| pareto4 | min+shape2 | uniqueness | accepted | The positive-excess transform preserves the exact Lomax crossing, the face comparison and pointwise two-level uniqueness. |
| pareto4 | scale+shape2 | uniqueness | accepted | The threshold pole and tail identify the two free parameters on the interior branch and the unit-exponent contact has an explicit optimizer. |
| pgw | scale | uniqueness | regraded | Five-level incidence proves generic uniqueness and held-scale compactness gives a finite root comparison procedure. |
| pgw | shape | uniqueness | regraded | The score incidence proves generic uniqueness and the collar gives a finite-root comparison procedure including face equality. |
| sgomp | all free | uniqueness | regraded | Incidence separates positive-shape scores, and the interval crossing accounts for the attained exponential, while ordering the roots is procedural. |
| trgamma | all free | uniqueness | regraded | Three levels give exponential-score separation and the two-level bound transfers from Prentice, while signed-root and face comparison is procedural. |
| trgamma | scale | uniqueness | regraded | Three levels give exponential-score separation and strict end signs leave a finite list for the root-height comparison procedure. |
| triang | all free | uniqueness | accepted | Every maximizer has a knot mode with a unique endpoint pair, and the two-level formula and composed-gradient comparison settle the two- and higher-level claims. |

## Proof errors and scope corrections

- **The existing all-free beta-binomial row overstates its evidence.** For (7,8,13), the size-38 binomial peak loses strictly to the feasible size-13 witness with shapes (52/21,73/83). The second example in the same section also has a strict height gap. This establishes separate profile peaks. A nonconstant cross-size global tie remains open. The replacement retains the separate peaks and the exact size-zero and mixed-binary ridges. [Exact peak comparisons](2026-11-25-bbinomfree-math-report.md#nonunimodality-and-exact-ties)
- **The cited all-free binomial row overstates the tie conclusion.** Strict branches imply that a positive-mean pair of co-winners would consist of two adjacent sizes on opposite sides of the continuous peak. An example requires a feasible histogram satisfying the valuation equations. The prime-power exclusion and exact search through lower size 500 leave that existence question open. The all-zero nonidentification is proved. [DeRiggi's profile theorem](https://www.tandfonline.com/doi/abs/10.1080/01621459.1983.10477949) · [Valuation equations](2026-11-25-binom-ties-math-report.md#which-adjacent-sizes-can-tie)
- **Fixed-endpoint generalized beta has a score identity at upper shape one.** The held-scale proof says the four-rate argument separates finite candidates including the unit-shape member. At scale one and upper shape one, \((p,a)=(1,2)\) and \((2,1)\) both give density \(2x\) on \((0,1)\). The generic result with free upper shape survives because the reachable moving power cutoff at the sample maximum strictly beats the whole unit-shape slice on strictly interior data, since moving the endpoint from \(b\) to \(M=\max x_i<b\) at fixed rate \(pa>0\) multiplies the likelihood by \((b/M)^{npa}>1\). At held unit upper shape the ridge remains part of the result. [Proposed fixed-scale proof](2026-11-27-uniq-shape-math-report.md#genbeta-held-scale) · [Reachable traces](2026-11-27-genbeta-atlas-math-report.md#genbeta-held-scale)
- For shifted Gompertz the five-level incidence bound counts variable levels. If zero is held fixed as a level, four positive levels give only four free sample coordinates. The replacement uses five variable levels and keeps the finite profile comparison on data containing zeros. [Score incidence](2026-11-27-uniq-shape-math-data/method.md#other-score-charts)
- The all-free GB2 golden comparison uses guarded arithmetic. The accepted claim follows from the logistic score and compact comparisons. Pointwise uniqueness at the golden remains open pending an outward enclosure. [Earlier review](2026-11-27-atlas-mrev2-report.md#proof-errors-and-corrections)

The incidence proof uses o-minimality to make nonidentical analytic score-equality fibers finite. Its link to Wilkie's paper has a typo in the final DOI digit. [Wilkie's theorem](https://doi.org/10.1090/S0894-0347-96-00216-0)

For binomial and for hypergeometric with both margins held, two adjacent ratios give a local membership test for each feasible size. Strict crossing reduces the positive attained branch to a singleton or adjacent pair, with the other branches explicitly classified. Cross-slice enumeration, quantifier elimination and retaining the top roots of a profile receive one-sided grades. The triangular row gives a set indexed by observed knots, each with a unique endpoint pair. For GPD with threshold and shape held, the monotone score and endpoint exponent give an empty set or a singleton throughout the fitted shape domain.

## Corrected skew-t crossing

The skew-t holds alpha+nu+xi and alpha+xi belong to the locscale batch. The corrected witness proves multiplicity for both holds. From the dossier density \(2t_\nu(z)T_{\nu+1}(h)/w\), at \(\xi=0\), \(\nu=1\), and \(x>0\), use \(z=x/w\), \(h=A z\sqrt{2/(1+z^2)}\), and \(2T_2(h)=1+h/\sqrt{2+h^2}\). Thus
\[
  \pi f(x)=\frac{w}{w^2+x^2}S_x,\qquad
  S_x=1+\frac{A}{\sqrt{1+A^2+(w/x)^2}}.
\]
Take \(A=2^{20}\), \(R=2^{50000}\), and \(D\in[D_-,D_+]=[2^{10}A,2^{150}A]\). On \(1^{[p]},D^{[p]},R\) with \(p=100\), multiplication by \(\pi^{201}D^{2p}R^2/2^{p+1}\) gives
\[
 w\left(\frac{w^2}{w^2+1}\right)^p
 \left(\frac{D^2}{w^2+D^2}\right)^p S_1^p
 \left(\frac{S_D}{2}\right)^p
 \frac{R^2}{w^2+R^2}\frac{S_R}{2}.
\]
The scale factor is \(w\). The rational check verifies the witnesses at \(A/8\) and \(D_+/16\), the gap at the split \(8A\), and the opposite strict orders of branch maxima at \(D_-\) and \(D_+\). The branch heights are continuous because the scale-end values converge uniformly to zero on the closed \(D\)-band. Their first equality gives distinct interior global scales on opposite sides of the split.

For free df, the Student normalizer obeys \(c_\nu\le1/\sqrt{2\pi}\) by Cauchy–Schwarz for the gamma ratio. For \(\nu\ge1\), \((1+z^2)t_\nu(z)\) is largest at \(|z|=1\), or constant at df one, and is bounded by one. For \(\nu\ge2\), Bernoulli's inequality gives \(t_\nu(z)\le 6(1+z^2)^{-3/2}\). The rational tail bounds exclude \(w\ge2^{1000}\) and \(\nu\ge2\) at smaller scales, including the skew-normal limit. For \(1\le\nu\le2\), the digamma integral bounds \((\log c_\nu)'\) by \(1/(4\nu^2)\), using \(1/(1+e^{-t/2})-1/2\le t/8\). The skew CDF contributes at most three to the log-density df derivative, from the fixed-argument bound of two and \(h\,t_{\nu+1}(h)/T_{\nu+1}(h)\le1\). The outlier then forces the total df derivative to be negative by the checked rational inequality \(R^2>2(2^{1000})^2 2^{20n}\). Every winner lies at df one, so the same scales win in both masks. The construction proves at least two scales, with the total number open. The real outlier exceeds the binary64 range. [Dossier density](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md#details) · [Repaired source proof](2026-11-27-uniq-locscale-math-report.md#repair-of-the-skew-t-scale-crossing) · [Independent rational check](2026-11-27-atlas-urev2-data/check_skew_t.py)

## Rows not joined

Every proposed family and hold mask exists in the atlas. All 67 proposed keys have replacements in the reviewed fragment. The two skew-t masks checked above belong to the locscale fragment.

## Open questions by family

| Family | Remaining question |
| --- | --- |
| binom | A feasible nonconstant histogram with a positive-mean tie at adjacent sizes with composite lower size having at least two prime factors, or an arithmetic exclusion of all such histograms. |
| bbinom | An exact comparison with a fitted NB face when the large-size tail loses, a higher-order test at the vanishing fourth-order Poisson coefficient, and an exclusion or witness for nonconstant cross-size ties. Held noninteger alpha at zero quadratic coefficient needs a further tail bound. Fixed sizes above eight outside the endpoint-rich class need a curvature or multiplicity result. |
| bnbinom | The number of winning canonical orbits and equality of general special-function heights. Held beta or size at noninteger values needs an exact comparison outside the established branches. |
| cmb | General free-dispersion heights, effective comparison with the uncapped CMP face, and whether rational holds or nonadjacent sizes give nonconstant ties. The exact real crossing leaves the indices of the other winners open. |
| dgamma | General degeneracy and equality of competing analytic heights outside the proved uniqueness classes. |
| hyper | A structural classification beyond the explicit saturated sets in the all-free fit and of ties across different draw-count slices at held population. With both margins held the exact local inequalities settle the set. |
| skellam | Equality of Bessel critical values, including equality with the Poisson member on integer histograms outside the proved singleton classes. |
| betapr | Two-level cardinality for general holds and effective equality tests for exceptional stationary comparisons. |
| burr | General all-free cardinality at three levels and pointwise root-height comparisons on the exceptional sets in both masks. |
| expweibull | Three to six levels all free, and fewer than five at held scale, need pointwise comparisons or sharper separation. The known all-free crossing leaves within-band cardinality open. |
| fatigue | Held alpha at small level counts needs a check of which stationary components win and whether a positive-dimensional component wins. The other masks have finite candidates. Algebraic comparisons decide their algebraic instances. Simpler pointwise branch conditions remain open. |
| fpareto | Small positive-excess strata need winning-component comparisons under fixed threshold. An exact finite tie with all four regular coefficients free is open. For the fixed-scale and fixed-power interior branch, a height order of competing threshold roots is missing. |
| frechet | Three- to six-level all-free strata and smaller held-scale strata still need pointwise shape comparison. The exact crossings leave additional co-winners open. |
| gb2 | Three-level finite cardinality and exact equality of stationary heights. The all-free golden needs an outward global enclosure. |
| genbeta | Fixed-endpoint critical-height comparison at face equality and a pointwise golden proof. With upper shape above one and endpoint free, fewer than seven levels still need finite-score separation or direct comparison. |
| gengamma | Isolation and exact height decisions for tied signed roots and faces. Held scale has a two-level multiplicity region whose boundaries and additional orbits remain open. Small nonzero-residual strata remain open with location and scale held. |
| gev | Effective pointwise ordering on smaller and exceptional strata, including the number of additional winners in the held-location and held-scale crossings. |
| gpd | With threshold held and shape free, classify exceptional finite/uniform ties by height or root order. With both threshold and shape held the fitted-domain set is settled. |
| invburr | All-free small-level cardinality and comparison of exceptional roots after the reciprocal transfer. The held-power two-level branch is settled. |
| invparalogis | Pointwise classification of two- and three-level strata outside the proved classes and exact comparisons at exceptional critical heights. |
| invpareto | Pointwise comparison of exceptional profiles with at least three levels, including additional ties beyond the exhibited reciprocal crossing. |
| invtrgamma | Effective ordering of signed critical heights and additional winners at the replicated-outlier tie. With scale held, small-level comparisons outside the centered singleton remain open. |
| lomax | Characterize exceptional ties and compare separated stationary heights on samples with at least three levels. |
| paralogis | Classify smaller strata outside the proved pointwise classes and locate any additional equal-height branches. |
| pareto2 | Beyond two positive excess levels, classify exceptional Lomax ties and handle equality of critical heights. |
| pareto4 | Held threshold inherits the Burr three-level question. Held threshold and power inherit the Lomax exceptional ties. For fixed scale and power above one, the separated threshold maxima lack an exact height-order theorem. |
| pgw | Small strata and comparison of critical heights, including equality with the transformed Gompertz face at held shape. |
| sgomp | Exact finite/exponential or finite/finite height decisions beyond the proved examples, including low-dimensional strata with a fixed zero. |
| trgamma | Pointwise order at finite/finite or finite/power equality, and scale-held comparisons outside the centered singleton and the proved finite candidate lists. |
| triang | A direct criterion for which higher-level knots have equal top height, beyond computing their individual knot fits. |

## Checks

All computation ran on the workstation through the one-slot wrapper. The discrete scripts passed the exact binomial filter through 500, the rational CMB and hypergeometric witnesses, and the symbolic beta-binomial and beta-negative-binomial expansion checks. Their 80-digit residual replays corroborated the analytic remainder arguments. The shape checker passed the score and triangular identities. [Discrete checker](2026-11-27-uniq-discrete-math-data/check.py) · [Hypergeometric replay](2026-11-27-uniq-discrete-math-data/hyper_tail.py) · [Shape checker](2026-11-27-uniq-shape-math-data/check.py)

The inherited exact and outward-interval replays passed for the beta-prime, Burr, exponentiated Weibull, Fréchet, GEV, paralogistic, Lomax, shifted-Gompertz, generalized-gamma and all-zero or swap cases cited above. The beta-binomial Bernstein checker covered all support triples up to reflection at sizes four through eight. The original corrected skew-t checker and the independent dossier-based rational check both passed. [Fixed-size curvature verifier](2026-11-25-bbinom3-math-data/bernstein.py) · [Rational checks for the inherited generalized-gamma and beta-negative-binomial cases](2026-11-27-uniq-atlas-math-data/exact_checks.py) · [Skew-t source checker](2026-11-27-uniq-locscale-math-data/check_math.py)

The fragment validator passed all 67 keys, the evidence headings, the origins and the duplicate check. The repository link scan matched the pre-review baseline. Its only current diagnostic outside the inherited list is the known beta-prime formula at line 461. This was a mathematics-only review. [Fragment validator](2026-11-27-atlas-urev2-data/validate.py)
