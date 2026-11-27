# Independent review of truncation and zero modifier proofs

TASK-442 reviewed all 207 rows proposed by TASK-439. The import file contains **156 written rows**. 136 were accepted, 20 were regraded, and 51 were rejected for the proposed import. Regraded includes a corrected claim at its original direction. 5 rows finish at one-sided. An empty held mask in the import corresponds to the atlas label `all free`.

[Reviewed CSV](2026-11-27-mod-rev1-data/reviewed.csv) · [TASK-439 report](2026-11-27-trunc-expfam-math-report.md) · [Parent atlas](../existence-atlas/atlas.csv) · [Modifier design](../../design/modifiers.md)

## Row decisions

| Family | Held | Modifier | Question | Verdict | Reason |
| --- | --- | --- | --- | --- | --- |
| beta | all free | truncate:known | criterion | accepted | The positive-quadrant inward log scores are necessary and sufficient at each integrable edge or corner, with equality included. |
| beta | all free | truncate:known | failure | accepted | An open parent endpoint makes the corresponding zero-shape integral diverge, leaving only the stated edges, corner and constant collapse. |
| beta | all free | truncate:known | uniqueness | accepted | The two log statistics are affinely independent on a positive-width interval. |
| beta | shape1 | truncate:known | criterion | accepted | At fixed positive shape one the scalar log-complement score has exactly the open range of its two limiting means. |
| beta | shape1 | truncate:known | failure | accepted | The shape-two-zero carrier requires the upper cut, and the opposite end collapses only at an observable lower cut. |
| beta | shape1 | truncate:known | uniqueness | accepted | The log complement varies on the retained positive-width interval. |
| beta | shape2 | truncate:known | criterion | accepted | At fixed positive shape two the scalar log score has exactly the open range of its two limiting means. |
| beta | shape2 | truncate:known | failure | accepted | The shape-one-zero carrier requires the lower cut, and the opposite end collapses only at an observable upper cut. |
| beta | shape2 | truncate:known | uniqueness | accepted | The log statistic varies on the retained positive-width interval. |
| binom | all free | truncate:known | criterion | accepted | The analytic size profiles converge to the Poisson face and a finite witness at face equality suffices. |
| binom | all free | truncate:known | failure | accepted | For nonconstant samples each fixed size has an interior optimum and the only remaining end is the same-window Poisson. |
| binom | all free | truncate:known | uniqueness | accepted | The signed or flat tail and exact prefix comparisons identify every winning size with its unique matched probability, while constants use the compatible atom and singleton sets. |
| binom | all free | zero:inflated | criterion | accepted | The positive-count mass bound compacts the rate at infinite size and a finite profile meeting that face attains. |
| binom | all free | zero:inflated | failure | accepted | Along any positive-height infinite-size sequence the only possible excluded law is ZIP, including its ordinary Poisson member. |
| binom | all free | zero:inflated | uniqueness | regraded | The fixed-size tilt or Bernoulli solution gives each size's full set, and the size comparison plus the all-zero atom union gives all winners. |
| binom | all free | zero:modified | criterion | accepted | The Bernoulli optimum includes both endpoints and compatibility gates the positive component, whose criterion is transported exactly. |
| binom | all free | zero:modified | failure | accepted | A positive-component Poisson escape transports with the same Bernoulli weight and no other excluded end is needed. |
| binom | all free | zero:modified | uniqueness | accepted | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice. |
| binom | all free | zero:truncated | criterion | accepted | The prior interval-certified moment envelope and transcendence argument settle the strict threshold and its equality question. |
| binom | all free | zero:truncated | failure | accepted | The moment envelope and signed analytic tail make the positive Poisson the sole excluded winner for nonconstants. |
| binom | all free | zero:truncated | uniqueness | accepted | The finite algebraic prefix and the explicit all-ones union give all finite winners. |
| binom | p0 | zero:modified | criterion | rejected | A compatible held Bernoulli term is constant in the parent parameters and transports the conditional criterion, including all zeros, and the proposed modifier-held mask has no atlas parent. |
| binom | p0 | zero:modified | failure | rejected | A positive-component Poisson escape transports with the same Bernoulli weight and no other excluded end is needed, and the proposed modifier-held mask has no atlas parent. |
| binom | p0 | zero:modified | uniqueness | rejected | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice, and the proposed modifier-held mask has no atlas parent. |
| binom | p0+prob | zero:modified | criterion | rejected | A compatible held Bernoulli term is constant in the parent parameters and transports the conditional criterion, including all zeros, and the proposed modifier-held mask has no atlas parent. |
| binom | p0+prob | zero:modified | failure | rejected | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds, and the proposed modifier-held mask has no atlas parent. |
| binom | p0+prob | zero:modified | uniqueness | rejected | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice, and the proposed modifier-held mask has no atlas parent. |
| binom | p0+size | zero:modified | criterion | rejected | A compatible held Bernoulli term is constant in the parent parameters and transports the conditional criterion, including all zeros, and the proposed modifier-held mask has no atlas parent. |
| binom | p0+size | zero:modified | failure | rejected | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds, and the proposed modifier-held mask has no atlas parent. |
| binom | p0+size | zero:modified | uniqueness | rejected | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice, and the proposed modifier-held mask has no atlas parent. |
| binom | pi | zero:inflated | criterion | rejected | Each polynomial slice attains and the only positive-height escape is the corresponding held-inflation Poisson face, and the proposed modifier-held mask has no atlas parent. |
| binom | pi | zero:inflated | failure | rejected | The positive-count mass bound gives the same ZIP compactification with inflation held, and the proposed modifier-held mask has no atlas parent. |
| binom | pi | zero:inflated | uniqueness | rejected | The roots and endpoints give a finite list per design, with ranking across designs still required, and the proposed modifier-held mask has no atlas parent. |
| binom | pi+size | zero:inflated | criterion | rejected | The compact fixed-size likelihood is a polynomial in the free probability and includes each compatible endpoint, and the proposed modifier-held mask has no atlas parent. |
| binom | pi+size | zero:inflated | failure | rejected | There is no omitted boundary of the fixed-size probability interval, and the proposed modifier-held mask has no atlas parent. |
| binom | pi+size | zero:inflated | uniqueness | rejected | Derivative roots are finite for a nonconstant polynomial, with a flat constant slice treated separately, and the proposed modifier-held mask has no atlas parent. |
| binom | prob | truncate:known | criterion | regraded | A fixed interior probability gives a finite winning prefix outside the singleton and bounded top-only cases. |
| binom | prob | truncate:known | failure | accepted | At fixed interior probability the only nonattainment is the bounded top atom, since unbounded-window fixed-count likelihood tends to zero. |
| binom | prob | truncate:known | uniqueness | regraded | On a retained singleton every compatible size wins, while the other attained cases use a finite prefix or endpoint compatibility. |
| binom | prob | zero:modified | criterion | accepted | The Bernoulli optimum includes both endpoints and compatibility gates the positive component, whose criterion is transported exactly. |
| binom | prob | zero:modified | failure | accepted | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds. |
| binom | prob | zero:modified | uniqueness | accepted | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice. |
| binom | prob | zero:truncated | criterion | accepted | The fixed interior probability profile goes to zero as size grows, and held probabilities zero and one have the stated attainable atom sets. |
| binom | prob | zero:truncated | failure | accepted | Large size at fixed interior probability loses all positive-sample likelihood, and endpoint holds reduce to admitted atoms. |
| binom | prob | zero:truncated | uniqueness | accepted | The finite prefix lists all interior-hold winners and direct compatibility gives every endpoint winner. |
| binom | size | truncate:known | criterion | regraded | A compatible singleton can attain at interior probability even when neither endpoint atom is admitted. |
| binom | size | truncate:known | failure | accepted | At fixed size the only unattained ends are retained endpoint atoms with zero parent event mass. |
| binom | size | truncate:known | uniqueness | accepted | The odds tilt is identifiable on nonsingletons and every compatible probability fits a singleton. |
| binom | size | zero:inflated | criterion | regraded | A compact fixed-size slice attains for all compatible data, including the all-zero cases omitted from the claim. |
| binom | size | zero:inflated | failure | accepted | The fixed-size probability square is compact and all its proper endpoint laws are included. |
| binom | size | zero:inflated | uniqueness | regraded | The strict fixed-size tilt, Bernoulli product and zero-atom union give the complete set when their scopes are included. |
| binom | size | zero:modified | criterion | accepted | The Bernoulli optimum includes both endpoints and compatibility gates the positive component, whose criterion is transported exactly. |
| binom | size | zero:modified | failure | accepted | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds. |
| binom | size | zero:modified | uniqueness | accepted | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice. |
| binom | size | zero:truncated | criterion | regraded | A compatible size-one conditional law is flat, while larger sizes use the mean root or admitted atoms. |
| binom | size | zero:truncated | failure | accepted | At fixed positive size both positive-support endpoint atoms are admitted. |
| binom | size | zero:truncated | uniqueness | accepted | At size one every conditional probability parameter is optimal and at larger size the monotone mean or attained atom identifies it. |
| exp | all free | truncate:known | criterion | accepted | The effective lower cut is the origin of the negative-rate tilt, whose upper finite mean threshold is half the width. |
| exp | all free | truncate:known | failure | accepted | Only the lower endpoint collapse or bounded uniform can win at the two scalar ends. |
| exp | all free | truncate:known | uniqueness | accepted | The shifted observation has strictly positive variance under every finite interior rate. |
| gamma | all free | truncate:known | criterion | accepted | Nonconstant-data coercivity and the two feasible inward scores, with equality included, settle the positive quadrant. |
| gamma | all free | truncate:known | failure | accepted | The carrier integrals at the two zero-coordinate edges and their corner are finite exactly at the cuts used in their tests. |
| gamma | all free | truncate:known | uniqueness | accepted | No nontrivial linear combination of log X and X is constant on an interval, so strict concavity covers all attained cases. |
| gamma | scale | truncate:known | criterion | accepted | At positive held scale the free log tilt has finite shape-zero mean only when the lower cut removes the singularity. |
| gamma | scale | truncate:known | failure | accepted | The only eligible finite edge is the cut shape-zero carrier, and log-mean equality at a finite top gives collapse. |
| gamma | scale | truncate:known | uniqueness | accepted | The log statistic has positive conditional variance and the shape parametrization is injective. |
| gamma | shape | truncate:known | criterion | accepted | The positive held shape gives a negative-rate tilt with the stated power-carrier mean and an infinite threshold on unbounded windows. |
| gamma | shape | truncate:known | failure | accepted | The finite lower cut can collapse when observable and the bounded carrier is the excluded rate-zero face. |
| gamma | shape | truncate:known | uniqueness | accepted | The strictly monotone mean in rate supplies a single solution. |
| generic | all free | truncate:both-estimated | criterion | rejected | The both-bound tightening proof retains all tied observations, but the generic key has no parent atlas regime. |
| generic | all free | truncate:both-estimated | failure | rejected | The both-bound reduction inherits valid tight-window failures and the feasible zero-width collapse, but the generic key has no parent atlas regime. |
| generic | all free | truncate:both-estimated | uniqueness | rejected | The both-bound equality test follows from equality of retained probability, but the generic key has no parent atlas regime. |
| generic | all free | truncate:lower-estimated | criterion | rejected | The lower-bound tightening proof retains all tied observations, but the generic key has no parent atlas regime. |
| generic | all free | truncate:lower-estimated | failure | rejected | The lower-bound reduction inherits valid tight-window failures and the feasible zero-width collapse, but the generic key has no parent atlas regime. |
| generic | all free | truncate:lower-estimated | uniqueness | rejected | The lower-bound equality test follows from equality of retained probability, but the generic key has no parent atlas regime. |
| generic | all free | truncate:upper-estimated | criterion | rejected | The upper-bound tightening proof retains all tied observations, but the generic key has no parent atlas regime. |
| generic | all free | truncate:upper-estimated | failure | rejected | The upper-bound reduction inherits valid tight-window failures and the feasible zero-width collapse, but the generic key has no parent atlas regime. |
| generic | all free | truncate:upper-estimated | uniqueness | rejected | The upper-bound equality test follows from equality of retained probability, but the generic key has no parent atlas regime. |
| geom | all free | truncate:known | criterion | regraded | The scalar mean rule needs the compatible singleton branch before testing the two thresholds. |
| geom | all free | truncate:known | failure | accepted | The upper threshold is the closed-window uniform mean and the lower escape is the excluded atom. |
| geom | all free | truncate:known | uniqueness | accepted | The geometric count mean is strictly monotone outside singleton windows. |
| geom | all free | zero:inflated | criterion | accepted | The two-tilt concavity proof gives attainment for positives and either admitted atom gives it for zeros. |
| geom | all free | zero:inflated | failure | accepted | The positive-height ends are either attainable zero atoms or ruled out by a positive observation losing mass. |
| geom | all free | zero:inflated | uniqueness | accepted | The conditional and ordinary branch roots are unique for positives, while the all-zero atom union is explicit. |
| geom | all free | zero:modified | criterion | accepted | The Bernoulli optimum includes both endpoints and compatibility gates the positive component, whose criterion is transported exactly. |
| geom | all free | zero:modified | failure | accepted | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds. |
| geom | all free | zero:modified | uniqueness | accepted | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice. |
| geom | all free | zero:truncated | criterion | accepted | The positive mean is the reciprocal of probability, including its admitted endpoint at one. |
| geom | all free | zero:truncated | failure | accepted | The positive atom is admitted and the infinite-mean end cannot maximize any fixed positive histogram. |
| geom | all free | zero:truncated | uniqueness | accepted | The shifted geometric law has exactly one matching probability. |
| geom | p0 | zero:modified | criterion | rejected | A compatible held Bernoulli term is constant in the parent parameters and transports the conditional criterion, including all zeros, and the proposed modifier-held mask has no atlas parent. |
| geom | p0 | zero:modified | failure | rejected | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds, and the proposed modifier-held mask has no atlas parent. |
| geom | p0 | zero:modified | uniqueness | rejected | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice, and the proposed modifier-held mask has no atlas parent. |
| geom | pi | zero:inflated | criterion | rejected | The held-weight objective is strictly concave in the geometric ratio for positives and the zero atom covers all zeros, and the proposed modifier-held mask has no atlas parent. |
| geom | pi | zero:inflated | failure | rejected | For positives the two excluded ends have zero likelihood, while zeros can attain at the parent atom, and the proposed modifier-held mask has no atlas parent. |
| geom | pi | zero:inflated | uniqueness | rejected | The scalar holds give a unique positive fit and the all-zero union sliced by the weight covers the rest, and the proposed modifier-held mask has no atlas parent. |
| invgamma | alpha | truncate:known | criterion | accepted | Reciprocation with positive held shape gives the fixed-shape gamma score with reversed bounds. |
| invgamma | alpha | truncate:known | failure | accepted | The reciprocal gamma lower collapse and bounded power face map to the stated original-scale limits. |
| invgamma | alpha | truncate:known | uniqueness | accepted | The reciprocal transform preserves likelihood order and its rate identifies beta. |
| invgauss | all free | truncate:known | criterion | accepted | Coercivity and the two inward moment scores apply on the closed positive quadrant, including the infinite-moment exclusion. |
| invgauss | all free | truncate:known | failure | accepted | Carrier integrability leaves exactly the two zero-coordinate edges and their corner, in addition to constant-data collapse. |
| invgauss | all free | truncate:known | uniqueness | accepted | X and reciprocal X are affinely independent on any positive-width interval, giving strict concavity. |
| invgauss | mean | truncate:known | criterion | accepted | The fixed-mean exponent is a negative scalar tilt of the displayed statistic with a unique minimum at the projected mean. |
| invgauss | mean | truncate:known | failure | accepted | Its infinite-tilt end collapses at that minimum, while an upper threshold is finite only with both parent ends cut. |
| invgauss | mean | truncate:known | uniqueness | accepted | The scalar statistic is nonconstant even when the interval straddles its minimum. |
| invgauss | shape | truncate:known | criterion | accepted | Holding positive shape leaves a negative linear tilt whose upper mean is the truncated Lévy mean, including infinity. |
| invgauss | shape | truncate:known | failure | accepted | The lower end can collapse at an observable cut and the zero-linear-tilt limit is Lévy when its mean is finite. |
| invgauss | shape | truncate:known | uniqueness | accepted | The tilted X variance is strictly positive along the held-shape slice. |
| invweibull | shape | truncate:known | criterion | accepted | The reciprocal power with positive held shape gives a negative-rate exponential tilt on the reversed interval. |
| invweibull | shape | truncate:known | failure | accepted | The exponential lower collapse and bounded uniform transform to the upper collapse and inverse power law. |
| invweibull | shape | truncate:known | uniqueness | accepted | The transformed rate is a strictly increasing power of the free scale. |
| lnorm | all free | truncate:known | criterion | accepted | The coefficient-independent Jacobian takes the full normal attainment test to the open positive support. |
| lnorm | all free | truncate:known | failure | accepted | Log coordinates send the complete normal list to collapse and eligible power or log-uniform faces. |
| lnorm | all free | truncate:known | uniqueness | accepted | The natural normal coordinates identify both parameters and the log transform preserves likelihood order. |
| lnorm | meanlog | truncate:known | criterion | accepted | Logging gives the fixed-mean squared-distance score and transforms both cuts. |
| lnorm | meanlog | truncate:known | failure | accepted | The projected log mean can collapse and the bounded log-uniform is the other end. |
| lnorm | meanlog | truncate:known | uniqueness | accepted | The logged squared-distance statistic has positive variance at any finite scale. |
| lnorm | sdlog | truncate:known | criterion | accepted | Logging gives the fixed-scale linear tilt and puts the average between the transformed cuts. |
| lnorm | sdlog | truncate:known | failure | accepted | Either observable log endpoint can be approached by location escape at fixed log scale. |
| lnorm | sdlog | truncate:known | uniqueness | accepted | The conditional log observation has strictly positive variance on every admissible interval. |
| nbinom | all free | zero:inflated | criterion | accepted | The ordinary slice and admissible positive stationary fits cover all finite optima and a candidate meeting the exhaustive Poisson face attains. |
| nbinom | all free | zero:inflated | failure | accepted | Uniform small-size and extreme-rate bounds leave only the Poisson face with its profiled inflation. |
| nbinom | all free | zero:inflated | uniqueness | regraded | The free-weight positive stationary list is finite by the positive NB profile, but ranking it with the ordinary candidate earns one-sided coverage. |
| nbinom | all free | zero:modified | criterion | accepted | The Bernoulli optimum includes both endpoints and compatibility gates the positive component, whose criterion is transported exactly. |
| nbinom | all free | zero:modified | failure | accepted | A positive-component Poisson escape transports with the same Bernoulli weight and no other excluded end is needed. |
| nbinom | all free | zero:modified | uniqueness | regraded | The Bernoulli factor preserves the proved flat cases and conditional candidate finiteness, so the stationary-value cardinality rule remains one-sided. |
| nbinom | all free | zero:truncated | criterion | accepted | Analytic profiling at size zero and the Poisson end gives a finite candidate comparison that includes equality. |
| nbinom | all free | zero:truncated | failure | accepted | The matched probability converges to the positive-Poisson face and the other profiled end is the attained logarithmic law. |
| nbinom | all free | zero:truncated | uniqueness | regraded | The analytic finite candidate list and all-ones ray are proved, so stationary-height ranking is one-sided under the review rule. |
| nbinom | p0 | zero:modified | criterion | rejected | A compatible held Bernoulli term is constant in the parent parameters and transports the conditional criterion, including all zeros, and the proposed modifier-held mask has no atlas parent. |
| nbinom | p0 | zero:modified | failure | rejected | A positive-component Poisson escape transports with the same Bernoulli weight and no other excluded end is needed, and the proposed modifier-held mask has no atlas parent. |
| nbinom | p0 | zero:modified | uniqueness | rejected | The Bernoulli factor preserves the proved flat cases and conditional candidate finiteness, so the stationary-value cardinality rule remains one-sided, and the proposed modifier-held mask has no atlas parent. |
| nbinom | p0+prob | zero:modified | criterion | rejected | A compatible held Bernoulli term is constant in the parent parameters and transports the conditional criterion, including all zeros, and the proposed modifier-held mask has no atlas parent. |
| nbinom | p0+prob | zero:modified | failure | rejected | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds, and the proposed modifier-held mask has no atlas parent. |
| nbinom | p0+prob | zero:modified | uniqueness | rejected | The Bernoulli factor preserves the proved flat cases and conditional candidate finiteness, so the stationary-value cardinality rule remains one-sided, and the proposed modifier-held mask has no atlas parent. |
| nbinom | p0+size | zero:modified | criterion | rejected | A compatible held Bernoulli term is constant in the parent parameters and transports the conditional criterion, including all zeros, and the proposed modifier-held mask has no atlas parent. |
| nbinom | p0+size | zero:modified | failure | rejected | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds, and the proposed modifier-held mask has no atlas parent. |
| nbinom | p0+size | zero:modified | uniqueness | rejected | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice, and the proposed modifier-held mask has no atlas parent. |
| nbinom | pi | zero:inflated | criterion | rejected | Interior score necessity and positive-height compactness above the exhaustive Poisson face settle both strict victory and equality, and the proposed modifier-held mask has no atlas parent. |
| nbinom | pi | zero:inflated | failure | rejected | The same-inflation Poisson face is the only positive-height end surviving the uniform PMF bounds, and the proposed modifier-held mask has no atlas parent. |
| nbinom | pi | zero:inflated | uniqueness | rejected | The joint held-weight score has no proved candidate-set structure beyond stationarity, though the all-zero atom union is exact, and the proposed modifier-held mask has no atlas parent. |
| nbinom | pi+size | zero:inflated | criterion | rejected | A positive witness and vanishing endpoint likelihood ensure an attained scalar root for compatible positives, and the proposed modifier-held mask has no atlas parent. |
| nbinom | pi+size | zero:inflated | failure | rejected | The endpoint probabilities cannot cause an escape for positives and a proper zero atom handles all zeros, and the proposed modifier-held mask has no atlas parent. |
| nbinom | pi+size | zero:inflated | uniqueness | rejected | Analyticity on a compact superlevel gives finitely many contenders, so stationary-height ranking is one-sided, and the proposed modifier-held mask has no atlas parent. |
| nbinom | prob | zero:modified | criterion | accepted | The Bernoulli optimum includes both endpoints and compatibility gates the positive component, whose criterion is transported exactly. |
| nbinom | prob | zero:modified | failure | accepted | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds. |
| nbinom | prob | zero:modified | uniqueness | regraded | The Bernoulli factor preserves the proved flat cases and conditional candidate finiteness, so the stationary-value cardinality rule remains one-sided. |
| nbinom | prob | zero:truncated | criterion | accepted | The divided normalization is analytic at size zero and the score is strictly negative eventually, so the finite list suffices. |
| nbinom | prob | zero:truncated | failure | accepted | Positive-sample likelihood decays at infinite size for an interior hold and the logarithmic and probability-one members are attained. |
| nbinom | prob | zero:truncated | uniqueness | regraded | Analyticity proves finiteness of the score candidates and the all-ones special cases, so the general ranking is one-sided. |
| nbinom | size | truncate:known | criterion | regraded | Singletons and the size-zero law must be separated from the scalar test at positive held size. |
| nbinom | size | truncate:known | failure | regraded | At positive size the two finite-window ends are the lower atom and normalized rising factorial weights, while the logarithmic alias has no failure. |
| nbinom | size | truncate:known | uniqueness | regraded | Positive count variance identifies the matched probability and the compatible singleton and size-zero exceptions can be written explicitly. |
| nbinom | size | zero:inflated | criterion | accepted | Positive-height compactness on the scalar probability slice and the zero-atom union give attainment. |
| nbinom | size | zero:inflated | failure | accepted | At positive held size both probability ends lose positive-data likelihood and the all-zero atom is attained. |
| nbinom | size | zero:inflated | uniqueness | regraded | The fixed-carrier tilt is strict for positives and the all-zero union and size-zero flat case complete the set. |
| nbinom | size | zero:modified | criterion | accepted | The Bernoulli optimum includes both endpoints and compatibility gates the positive component, whose criterion is transported exactly. |
| nbinom | size | zero:modified | failure | accepted | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds. |
| nbinom | size | zero:modified | uniqueness | accepted | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice. |
| nbinom | size | zero:truncated | criterion | accepted | The scalar mean root exists for mean above one at every eligible size, including the logarithmic member, with one attained at probability one. |
| nbinom | size | zero:truncated | failure | accepted | Neither the infinite positive mean end nor an admitted atom or logarithmic member yields an excluded winner. |
| nbinom | size | zero:truncated | uniqueness | accepted | The scalar statistic identifies probability on the held-size slice, including the two admitted extensions. |
| norm | all free | truncate:known | criterion | accepted | The matched linear edge is optimal at nonpositive inward score, including exact zero, and all constants collapse. |
| norm | all free | truncate:known | failure | accepted | The recessional quadratic check excludes nonconstant collapse and the integrable line of zero precision gives the listed faces. |
| norm | all free | truncate:known | uniqueness | accepted | The covariance of the linear and quadratic statistics is positive definite on every positive-width interval. |
| norm | mean | truncate:known | criterion | accepted | The squared-distance score spans the interval from the squared projection distance to the uniform or infinite moment. |
| norm | mean | truncate:known | failure | accepted | The scalar ends are collapse at the unique squared-distance minimizer and the bounded-window uniform. |
| norm | mean | truncate:known | uniqueness | accepted | Squared distance has strictly positive variance on every interval of positive width. |
| norm | sd | truncate:known | criterion | accepted | At positive held scale, a full real location tilt matches every mean strictly between the window ends. |
| norm | sd | truncate:known | failure | accepted | A mean at either finite cut forces the entire sample to that cut and gives unbounded likelihood there. |
| norm | sd | truncate:known | uniqueness | accepted | The fixed-scale tilt on the count-free interval has strictly positive variance of X. |
| pareto | all free | truncate:known | criterion | accepted | The fitted endpoint first tightens the cut and then the log-scale exponential score decides attainment. |
| pareto | all free | truncate:known | failure | accepted | The tight endpoint gives the scalar collapse and log-uniform ends, with zero-width tightening giving collapse too. |
| pareto | all free | truncate:known | uniqueness | accepted | The scalar root is unique and the support parameter ties exactly along preimages of the optimal cut. |
| pareto | a | truncate:known | criterion | accepted | The endpoint-dependent factor cancels and monotonicity in the effective cut yields attainment whenever the tight window has positive width. |
| pareto | a | truncate:known | failure | accepted | At a sample constant at the upper cut, lifting the support endpoint shrinks a feasible window and sends likelihood to infinity. |
| pareto | a | truncate:known | uniqueness | accepted | Strict improvement up to the tight effective cut gives exactly its preimage in the parent endpoints. |
| pareto | b | truncate:known | criterion | accepted | The closed Pareto support and log transform turn the held-endpoint regime into the scalar exponential fit. |
| pareto | b | truncate:known | failure | accepted | Only a lower-end collapse or the bounded log-uniform face remains after the log transform. |
| pareto | b | truncate:known | uniqueness | accepted | The transformed exponential rate is exactly the Pareto shape. |
| pois | all free | truncate:known | criterion | regraded | The scalar mean rule needs the compatible singleton branch as well as the interior and admitted endpoints. |
| pois | all free | truncate:known | failure | accepted | Monotone count means leave only the two endpoint atoms, subject to the zero-rate extension. |
| pois | all free | truncate:known | uniqueness | accepted | Positive count variance identifies the rate except on a compatible singleton. |
| pois | all free | zero:inflated | criterion | accepted | The fixed-carrier zero tilt has a finite optimum for any positive sample and the all-zero weight one is admitted. |
| pois | all free | zero:inflated | failure | accepted | Positive samples are compact at both rate ends and all zeros attain through inflation one. |
| pois | all free | zero:inflated | uniqueness | accepted | Strict concavity identifies the two coordinates with positives and all positive rates tie at all zeros. |
| pois | all free | zero:modified | criterion | accepted | The Bernoulli optimum includes both endpoints and compatibility gates the positive component, whose criterion is transported exactly. |
| pois | all free | zero:modified | failure | accepted | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds. |
| pois | all free | zero:modified | uniqueness | accepted | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice. |
| pois | all free | zero:truncated | criterion | accepted | The strictly monotone positive mean spans one to infinity, with one attained by the zero-rate extension. |
| pois | all free | zero:truncated | failure | accepted | The lower atom is admitted and the upper end loses all likelihood at fixed positive observations. |
| pois | all free | zero:truncated | uniqueness | accepted | One conditional mean or the admitted endpoint identifies the rate. |
| pois | p0 | zero:modified | criterion | rejected | A compatible held Bernoulli term is constant in the parent parameters and transports the conditional criterion, including all zeros, and the proposed modifier-held mask has no atlas parent. |
| pois | p0 | zero:modified | failure | rejected | The Bernoulli endpoints are admitted and the cited conditional slice has no excluded winner under these holds, and the proposed modifier-held mask has no atlas parent. |
| pois | p0 | zero:modified | uniqueness | rejected | The Bernoulli factor fixes one fitted or held weight and preserves the complete conditional maximizing set, including the flat all-zero parent slice, and the proposed modifier-held mask has no atlas parent. |
| pois | pi | zero:inflated | criterion | rejected | The held-weight rate score has a finite interior maximum for positive data and all zeros with held weight below one escape to zero rate, and the proposed modifier-held mask has no atlas parent. |
| pois | pi | zero:inflated | failure | rejected | The positive rate domain explicitly excludes the only all-zero maximizing limit when the held inflation is below one, and the proposed modifier-held mask has no atlas parent. |
| pois | pi | zero:inflated | uniqueness | rejected | The turning-level argument proves an exact two-rate tie for 99 zeros and one ten, and the proposed modifier-held mask has no atlas parent. |
| rayleigh | all free | truncate:known | criterion | accepted | Squaring gives an exponential model with a bijective rate on positive-density data. |
| rayleigh | all free | truncate:known | failure | accepted | The lower collapse requires a positive cut and the bounded zero-rate face transforms to a density proportional to X. |
| rayleigh | all free | truncate:known | uniqueness | accepted | The scalar root identifies the scale on admissible positive-density data. |
| weibull | shape | truncate:known | criterion | accepted | The power transform at held positive shape makes the free-scale problem exponential on the transformed interval. |
| weibull | shape | truncate:known | failure | accepted | Lower collapse and a finite-window power density are exactly the inverse transforms of the exponential failures. |
| weibull | shape | truncate:known | uniqueness | accepted | The exponential rate maps bijectively to free Weibull scale at fixed positive shape. |
| zeta | all free | truncate:known | criterion | accepted | The proof covers the explicitly stated finite signed window by strictly monotone log means. |
| zeta | all free | truncate:known | failure | regraded | The two infinite-exponent atoms exhaust the failures on the proved finite signed windows. |
| zeta | all free | truncate:known | uniqueness | regraded | Log-count variance gives strictness precisely when the finite signed window has more than one count. |

## Proof errors and grading corrections

- **The singleton branch is missing from the proposed Poisson, geometric and held-size binomial attainment tests.** On the closed window `[3,3]` the sample `(3,3)` has likelihood one at every positive Poisson rate, every interior geometric probability, and every interior binomial probability at size 12. Neither cutoff need be an admitted parent atom. The source's general count-window paragraph proves this branch. The revised criteria put it before the nonsingleton tests. For held-size NB the revision also states when the size-zero slice is flat and when the declared positive-window extension has a unique probability. [Count-window proof](2026-11-27-trunc-expfam-math-report.md#count-windows)
- **The held-probability binomial proof overstates its decaying-tail case.** Take `prob=1/2`, `[3,3]` and `(3,3)`. Every size at least three has positive event mass, and its conditional likelihood is identically one as the size grows. The assertion that likelihood vanishes for “all other data” needs the singleton exception. On a nonsingleton bounded window the successive ratios favor its top, and on an unbounded window the conditioning probability tends to one while each fixed observed mass tends to zero. Thus the finite prefix argument works in the remaining attained interior-hold cases. [Free-size window proof](2026-11-27-trunc-expfam-math-report.md#binom-free-truncateknown)
- **The fixed-size inflated cardinality claims omit zero-only samples.** With held positive binomial size and with held positive NB size, the set on all zeros is the union of inflation one and the parent-zero probability endpoint. At size zero the full probability/inflation slice yields the zero law. The revised free-inflation claims include these sets. For fixed-size positive binomial truncation, size one also needs its explicit singleton branch. [Fixed-size binomial inflation](2026-11-27-trunc-expfam-math-report.md#binom-size-zeroinflated) · [Fixed-size NB inflation](2026-11-27-trunc-expfam-math-report.md#nbinom-size-zeroinflated)
- **Ranking stationary likelihoods does not earn exact cardinality.** The positive NB profile is analytic at size zero and at inverse size zero, with a nonzero factorial-moment coefficient at the Poisson end. This establishes candidate finiteness. For a fixed interior probability, the scalar size score is analytic through zero and eventually negative. Free inflation adds the ordinary parent branch and filters the finite positive-profile list by the zero-mass constraint. The five joined uniqueness rows based on those rankings earn one-sided coverage. With both NB parent coordinates free and inflation held, the proposed simultaneous-score ranking has no proved finite candidate list or other joint root structure. The all-zero maximizing union is still exact, and the criterion and failure arguments have an exhaustive boundary comparison. [NB positive profile](2026-11-27-trunc-expfam-math-report.md#nbinom-free-zerotruncated) · [NB inflation comparison](2026-11-27-trunc-expfam-math-report.md#nbinom-free-zeroinflated)
- **Scope of the signed zeta proof.** The cited section proves the finite closed `[1,N]` regime with every finite real exponent. The failure and cardinality claims now state this domain explicitly. Extending that scope requires separate treatment of the allowed exponent domain and window geometry. [Zeta window proof](2026-11-27-trunc-expfam-math-report.md#zeta-free-truncateknown)

## Boundary and equality checks

The count calculations use retained integers from `ceiling(a)` through `floor(b)`, intersected with the parent support. In transferring the earlier binomial arguments, `L` is the first retained integer. The earlier half-open lower cutoff is replaced by `L - 1` on the lattice. The fixed-size normalizers are strictly log-convex in identifiable odds. The finite-prefix proof uses polynomial root and resultant isolation for bounded windows, and the nonzero leading factorial gap for positive lower-only windows. A finite candidate equal to the reachable same-window Poisson value attains. If a candidate strictly beats that value, convergence leaves a finite winning prefix. Hermite–Lindemann excludes equality between a finite profile likelihood and its positive lower-only Poisson face, and excludes a zero first-order gap there. On the untruncated positive-mean branch the ordinary binomial dispersion result uses sample variance divided by sample size and treats equality as escape. [Profile and equality proof](2026-11-25-tbinom-math-report.md#poisson-face-comparison-and-equality) · [Finite decision](2026-11-25-tbinom-math-report.md#a-finite-decision-certificate) · [Hermite–Lindemann](https://arxiv.org/abs/2306.14352) · [Ordinary binomial criterion](https://www.tandfonline.com/doi/abs/10.1080/01621459.1983.10477949)

For positive-size NB at zero probability, a bounded window admits finite rising-factorial weights, while the unbounded version has divergent mass by the binomial series. At size zero the positive-window logarithmic law is an admitted coefficient limit, and at probability one the positive-conditioned atom is at one. For the free-size positive profile, the first-order coefficient is the positive-Poisson factorial gap with the opposite sign to the binomial expansion because the NB weights have rising factors. Analytic continuation at both ends bounds the number of zeros of the profile score. For inflation, small size sends every fixed positive-count mass to zero uniformly in probability. At large size the bounds on the mass of any observed positive count force the rescaled rate into a compact positive interval. Its only limit law at positive likelihood is Poisson before the inflation step. Held or profiled inflation transfers that bound, including the ordinary face at zero inflation. This proves the finite-to-face equality rule even where it gives no terminating comparison. [NB truncation proof](2026-11-27-trunc-expfam-math-report.md#nbinom-size-truncateknown) · [NB positive profile](2026-11-27-trunc-expfam-math-report.md#nbinom-free-zerotruncated) · [NB inflation proof](2026-11-27-trunc-expfam-math-report.md#nbinom-free-zeroinflated)

For the continuous families I checked the natural domains in the dossier parametrizations. Normal uses the open negative-quadratic half-plane and its held-coordinate rays or line. Gamma, beta and inverse Gaussian use positive quadrants with identifiable statistics on each positive-width interval. Their finite faces are limited by integrability of the indicated carriers and the feasible directional scores are nonpositive at a winning face, including at equality. Infinite moments enter as one-sided derivatives, for example the inward score on the Lévy edge is positive infinite when its first moment diverges. For the scalar reductions I checked the effective endpoints, monotone statistic ranges and bijections of free coefficients. Pareto support includes the parameter-dependent endpoint. Rayleigh has zero density at zero under the repository convention. These checks use the likelihood envelope and the family-specific recession and integrability arguments. [Likelihood closure](../ml-behaviour.tex#lem:closure) · [Natural-coordinate proof](2026-11-27-trunc-expfam-math-report.md#convex-parent-constraints) · [Scalar and joint edge checks](2026-11-27-trunc-expfam-math-report.md#scalar-and-two-coordinate-checks) · [Geyer on recession](https://arxiv.org/abs/0901.0455)

For estimated bounds, tightening to an observed extreme retains every copy of a tied observation. At a positive-mass tight window, equality with a looser window requires that the difference in window mass vanish at the fitted coefficients. This includes rounding plateaus and support padding. A coincident pair of bounds can retain a compatible count atom. For admissible continuous samples at a common point, when zero-width tightening is approachable through positive-width intervals and the carrier density there is positive, the shrinking normalizer gives divergence. These generic conclusions were reviewed separately from whether a concrete family admits the fixed-window fit. [Estimated-bound argument](2026-11-27-trunc-expfam-math-report.md#all-applicable-truncateestimated)

For zero modification the Bernoulli term has its unique maximum at the observed zero fraction including zero and one. Held `p0` supplies a constant term when compatible. All zeros leave the admissible positive-component coefficients flat, and all positives force a free `p0` to zero. For inflation, the ordinary branch and the positive-conditional branch meet when the parent zero mass equals the observed zero fraction. For example, the geometric sample `(0,0,1,3)` has positive-conditional mean two and parent `prob=1/2`. Its parent zero mass is exactly the empirical one-half, giving `pi=0` at the seam. On a fixed exponential carrier the joint zero tilt supplies the global comparison, with the saturated Bernoulli slice handled separately. Free size instead requires comparing size slices. [Factorization and fixed-carrier proof](2026-11-27-trunc-expfam-math-report.md#exact-factorization-and-inflation-boundary)

**The CMB boundary shortcut is false.** For `(0,2,2,2,2,2,2,2)` hold `nu=1` and `prob=317/500`. The conditional mass at two is strictly largest at size two. That size has parent zero mass above `1/8` and clips to zero inflation. Size three has zero mass below `1/8`, so it fits positive inflation and exceeds size two by an exact rational inequality. Every larger size has a smaller conditional mass than size three and cannot improve its Bernoulli factor. I checked the CMB inflation claims and cited sections in TASK-440 against the cross-design comparison. TASK-439 has no CMB rows, and its free-size binomial and NB arguments retain the global comparisons. [CMB proof](2026-11-27-zero-mod-math-report.md#cmb-all-free-inflated) · [TASK-440 rows](2026-11-27-zero-mod-math-data/proofs.csv) · [Exact replay](2026-11-27-mod-rev1-data/exact_checks.py)

The positive-binomial sample `(1,1,2)` on closed `[1,2]` attains the multinomial bound for every size at least two with `prob=1/size`, and ties the conditional Poisson face at rate one. The saturated odds of two against one are exactly one-half in both cases. For the separate size-one inflated sample `(0,1)`, every pair with `(1-pi)*prob=1/2` attains the global Bernoulli bound. For held-inflation ZIP with 99 zeros and one ten, the exact difference between the positive mean and total mean is `99/10 > 4`. The analytic turning-point calculation gives two branches and opposite height ordering at the branch tangencies. The between-peak height difference is strictly increasing in inflation because the score in inflation at fixed rate is strictly increasing in rate after removing the positive-data term common to both peaks. Its unique zero defines the exact real hold, giving two global rates. This is a one-sided nonuniqueness witness if imported under a future held-`pi` key. [Binomial ties](2026-11-25-tbinom2-math-report.md#exact-size-ties) · [Inflated binomial](2026-11-27-trunc-expfam-math-report.md#binom-free-zeroinflated) · [Analytic ZIP tie](2026-11-27-trunc-expfam-math-report.md#pois-free-zeroinflated) · [Numerical locator](2026-11-07-zip-cert-report.md#nonuniqueness-counterexample)

The exact normal sample `±57735/100000` on `[-1,1]` is strictly on the interior side. In the symmetric quadratic tilt the uniform second moment exceeds the sample second moment by a positive rational below one millionth. On the tilt interval `[-1/100,0]` a rational lower bound for the variance of squared observations is `(99/100)(4/45)`. It brackets the root and bounds the per-observation likelihood improvement strictly below `10^-8`. The gamma corner sample `(1,4)` on `[1,4]` has exactly zero shape score and strictly negative inward rate score by strict Jensen. Both signs were replayed from the exact inequalities. [Normal comparison](2026-11-27-trunc-expfam-math-report.md#what-this-changes-for-the-fitter) · [Gamma corner](2026-11-27-trunc-expfam-math-report.md#gamma-free-truncateknown) · [Exact checks](2026-11-27-mod-rev1-data/exact_checks.py)

## Rows not joined

All four proposed zero-modifier families are eligible. Truncation is eligible for every named family in the proposal. The following keys need an explicit schema decision or concrete parent rows before import.

| Rows | Proposed modifier and masks | Follow-up |
| --- | --- | --- |
| 9 | Estimated lower, upper and both bounds, family `generic` | Store the reduction as a generic theorem or specialize it to concrete parent keys with the appropriate fixed-window and zero-width cases. |
| 24 | Zero modification with `p0` on binom (3 masks), nbinom (3), geom (1) and pois (1) | Provide a place for a modifier-only hold alongside the base mask. Retain the constant Bernoulli factor and its compatibility checks. |
| 18 | Zero inflation with `pi` on binom (2 masks), nbinom (2), geom (1) and pois (1) | Provide a place for the held inflation coefficient and apply the row-level mathematical comments before import. |

The base proof input uses an empty mask for all free and the generated atlas displays `all free`. The validator resolves that spelling only. Modifier-held masks were kept distinct because they change the likelihood and their direct atlas keys are absent. [Base proof input](../existence-atlas/proofs.csv) · [Atlas overview](../existence-atlas/overview.md)

## Open by family

| Family | Open after review |
| --- | --- |
| beta | The edge and corner tests give the mathematical criterion in the reviewed slices. Reliable numerical face-score and coefficient enclosures for the currently refused routes remain open. |
| binom | General lower-only integer unimodality and exact nonconstant lower-only size ties remain open. Mixed free-size inflation needs a finite prefix/tail certificate, including certified equalities across designs or with its face. The proposed modifier-held keys need schema support. |
| exp | The scalar known-window proof closes all three questions. A certified numerical route and concrete estimated-bound atlas keys remain open. |
| gamma | Reliable numerical face and corner score enclosures and interior coefficient certification remain open. The reviewed mathematical tests include tangent scores and exact zero inward signs. |
| geom | The reviewed mathematical slices close the three questions. A named generic-window status for the lower atom or upper uniform and import of the modifier-held rows remain open. |
| invgamma | The held-alpha proof closes the scalar questions. Free shape on general windows was outside the proposed scope and remains open here. |
| invgauss | Numerical face-score enclosures must account for divergent edge moments. The reviewed mathematical slices include the infinite-moment exclusions. |
| invweibull | Free shape changes the statistic with the coefficient. A joint analysis on general windows remains open. |
| lnorm | The three log-transformed mathematical slices are settled. Numerical mapping and certification near a transformed face remain to be implemented. |
| nbinom | A terminating comparison at multiple stationary roots or equal ordinates and a sample-level cardinality classification remain open. Held-inflation joint root coverage, effective free-size inflation comparisons and general windows at free size also remain open. |
| norm | A validated sign and coefficient enclosure near the quadratic face would resolve the exact interior example even when the likelihood gap falls inside the public comparison margin. |
| pareto | The known-window endpoint and shape mathematics is settled for the reviewed slices. The generic estimated-window import and implementation of these window routes remain open. |
| pois | The reviewed scalar and free-inflation questions are settled. Import of the held-inflation zero-rate escape and exact two-rate witness needs a modifier-held key. |
| rayleigh | The known-window scalar proof covers positive-density data. The numerical route and a concrete estimated-bound atlas specialization remain open. |
| weibull | Free shape is outside the fixed-statistic proof. Its truncated joint attainment and cardinality need a separate argument. |
| zeta | The imported scope is finite signed `[1,N]`. Broader windows need explicit atlas scopes. The current dispatcher also refuses a finite lower-estimated window when the resolved minimum is greater than one. |

## Validation

The workstation ran the following checks in foreground through the single-slot launcher.

| Check | Result |
| --- | --- |
| [Positive-binomial directed interval certificate](2026-11-25-tbinom2-math-data/face_interval.py) | All 80,000 boxes passed, with positive lower bounds in each of the three rectangles. |
| [Earlier binomial interval checks](2026-11-25-tbinom-math-data/check.py) | Passed the local-peak, finite witness and golden interval checks. |
| [TASK-440 checker](2026-11-27-zero-mod-math-data/check.py) | Passed the exact rational CMB comparison and the separate numerical probes. |
| [TASK-439 checker](2026-11-27-trunc-expfam-math-data/check.py) | Passed both precision replays. The NB size-grid comparisons are numerical evidence only. |
| [Independent exact checks](2026-11-27-mod-rev1-data/exact_checks.py) | Passed the normal near-face inequalities, gamma corner, binomial ties, CMB comparison and ZIP turning condition. |
| [CSV validator](2026-11-27-mod-rev1-data/validate.py) | Passed column, parent regime, eligibility, origin, source anchor and unique-key checks. |
| Repository link checker | No review-introduced broken links. The checker reported 47 inherited entries and the known false positive at `2026-11-25-betapr-math-report.md:461`. |

Reproduce the review-specific checks from the worktree root.

```sh
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
data=dev/research/evaluations/2026-11-27-mod-rev1-data
/tmp/umvl-cpu/run python3 -B "$data/exact_checks.py"
/tmp/umvl-cpu/run python3 -B "$data/validate.py"
/tmp/umvl-cpu/run python3 -B dev/tools/check_links.py
```
