# Independent review of fixed count-window truncation proofs

TASK-463 reviewed all 211 TASK-457 proposals across 68 hold masks and 14 families. Accepted 170 as submitted and regraded 41. The join file contains 68 iff, 125 exact and 18 one-sided claims. All 211 are written proofs at the scope stated in the exported rows. Three corrections affect how the results should be used. CMB can escape along a flat direction while preserving a winning law. The CMP two-cell fiber needs the parent-domain restriction at zero dispersion. The discrete Weibull parent has an excluded Bernoulli winner at held probability when the sample maximum is one. [Join file](2026-11-27-mod-rev8-data/reviewed.csv) · [Proposals](2026-11-27-trunc-discrete-math-data/proofs.csv) · [Source proofs](2026-11-27-trunc-discrete-math-report.md)

## Row decisions

| Family | Held | Question | Verdict | Reason |
| --- | --- | --- | --- | --- |
| bbinom | all free | `case:two-cell-tie` | regraded | The two exact shape pairs both match the empirical odds. |
| bbinom | all free | `criterion` | accepted | The exhaustive fixed- and growing-size recurrences bound every escaping subsequence, including the defective zero-cell case and finite equality. |
| bbinom | all free | `failure` | accepted | The zero-shape singular products, diverging-shape odds and growing-design normalization cover all compatible escapes. |
| bbinom | alpha | `criterion` | accepted | The fixed-alpha recurrence gives the fixed-design faces and the growing-size NB envelope, and a finite equality witness attains. |
| bbinom | alpha | `failure` | regraded | Holding alpha excludes the logarithmic and Poisson growing-size faces. |
| bbinom | alpha+size | `criterion` | accepted | The fixed-size rational score components and complete shape-end comparisons include both equality and compatible singleton slices. |
| bbinom | alpha+size | `failure` | regraded | Only one shape varies, so simultaneous zero-shape mixtures and the diverging-both binomial face are unavailable. |
| bbinom | alpha+size | `uniqueness` | regraded | Semialgebraic connected components are finite in number, but their height ranking earns one-sided cardinality coverage. |
| bbinom | beta | `criterion` | accepted | At held beta every growing-size path has zero height on positive ray samples, with finite-window power or harmonic faces and the all-zero singleton handled separately. |
| bbinom | beta | `failure` | regraded | Holding beta forces h to one at growing size and excludes the Poisson and proper NB faces. |
| bbinom | beta+size | `criterion` | accepted | The fixed-size rational score components and complete shape-end comparisons include both equality and compatible singleton slices. |
| bbinom | beta+size | `failure` | regraded | Only one shape varies, so simultaneous zero-shape mixtures and the diverging-both binomial face are unavailable. |
| bbinom | beta+size | `uniqueness` | regraded | Semialgebraic connected components are finite in number, but their height ranking earns one-sided cardinality coverage. |
| bbinom | size | `case:two-cell-tie` | regraded | The two exact shape pairs both match the empirical odds. |
| bbinom | size | `criterion` | accepted | The fixed-size rational score components and complete shape-end comparisons include both equality and compatible singleton slices. |
| bbinom | size | `failure` | accepted | The zero-shape singular products, diverging-shape odds and growing-design normalization cover all compatible escapes. |
| bbinom | size | `uniqueness` | regraded | Semialgebraic connected components are finite in number, but their height ranking earns one-sided cardinality coverage. |
| bnbinom | all free | `case:finite-rational` | accepted | At algebraic holds the finite rational likelihood and its domain give a semialgebraic global decision including ties. |
| bnbinom | all free | `case:label-tie` | regraded | The swapped numerator shapes both match the exact empirical histogram. |
| bnbinom | all free | `criterion` | accepted | The restricted recurrence exhaustion and summable ray bounds justify the finite-candidate comparison including equality. |
| bnbinom | all free | `failure` | accepted | The ordered-shape recurrence and tail bounds exhaust finite, Poisson, NB, power and defective limits under their path conditions. |
| bnbinom | alpha | `case:finite-rational` | accepted | At algebraic holds the finite rational likelihood and its domain give a semialgebraic global decision including ties. |
| bnbinom | alpha | `case:label-tie` | regraded | The swapped numerator shapes both match the exact empirical histogram. |
| bnbinom | alpha | `criterion` | accepted | The restricted recurrence exhaustion and summable ray bounds justify the finite-candidate comparison including equality. |
| bnbinom | alpha | `failure` | regraded | Restricting the ordered-shape table to the actual holds removes unavailable escape families. |
| bnbinom | alpha+beta | `case:finite-rational` | accepted | At algebraic holds the finite rational likelihood and its domain give a semialgebraic global decision including ties. |
| bnbinom | alpha+beta | `criterion` | accepted | The restricted recurrence exhaustion and summable ray bounds justify the finite-candidate comparison including equality. |
| bnbinom | alpha+beta | `failure` | regraded | Restricting the ordered-shape table to the actual holds removes unavailable escape families. |
| bnbinom | alpha+beta+size | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| bnbinom | alpha+beta+size | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| bnbinom | alpha+beta+size | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| bnbinom | alpha+size | `case:finite-rational` | accepted | At algebraic holds the finite rational likelihood and its domain give a semialgebraic global decision including ties. |
| bnbinom | alpha+size | `criterion` | accepted | The restricted recurrence exhaustion and summable ray bounds justify the finite-candidate comparison including equality. |
| bnbinom | alpha+size | `failure` | regraded | Restricting the ordered-shape table to the actual holds removes unavailable escape families. |
| bnbinom | beta | `case:finite-rational` | accepted | At algebraic holds the finite rational likelihood and its domain give a semialgebraic global decision including ties. |
| bnbinom | beta | `criterion` | accepted | The restricted recurrence exhaustion and summable ray bounds justify the finite-candidate comparison including equality. |
| bnbinom | beta | `failure` | regraded | Restricting the ordered-shape table to the actual holds removes unavailable escape families. |
| bnbinom | beta+size | `case:finite-rational` | accepted | At algebraic holds the finite rational likelihood and its domain give a semialgebraic global decision including ties. |
| bnbinom | beta+size | `criterion` | accepted | The restricted recurrence exhaustion and summable ray bounds justify the finite-candidate comparison including equality. |
| bnbinom | beta+size | `failure` | regraded | Restricting the ordered-shape table to the actual holds removes unavailable escape families. |
| bnbinom | size | `case:finite-rational` | accepted | At algebraic holds the finite rational likelihood and its domain give a semialgebraic global decision including ties. |
| bnbinom | size | `criterion` | accepted | The restricted recurrence exhaustion and summable ray bounds justify the finite-candidate comparison including equality. |
| bnbinom | size | `failure` | regraded | Restricting the ordered-shape table to the actual holds removes unavailable escape families. |
| cmb | all free | `criterion` | accepted | The bounded size comparison keeps flat slices, identified moments, exposed heights and included endpoint atoms. |
| cmb | all free | `failure` | regraded | Exposed faces and scalar extreme sets omit full-law limits along unidentified directions. |
| cmb | all free | `uniqueness` | accepted | The finite slice comparison retains the full identified-root, affine-fiber and endpoint sets at every winning size. |
| cmb | nu | `criterion` | accepted | The bounded size comparison keeps flat slices, identified moments, exposed heights and included endpoint atoms. |
| cmb | nu | `failure` | regraded | Exposed faces and scalar extreme sets omit full-law limits along unidentified directions. |
| cmb | nu | `uniqueness` | accepted | The finite slice comparison retains the full identified-root, affine-fiber and endpoint sets at every winning size. |
| cmb | nu+prob | `case:cross-size-tie` | regraded | The quadratic identity ties sizes two and three and the successor-ratio bound excludes every larger size. |
| cmb | nu+prob | `criterion` | accepted | The bounded size comparison keeps flat slices, identified moments, exposed heights and included endpoint atoms. |
| cmb | nu+prob | `failure` | regraded | With both canonical coefficients held the size cap prevents escape. |
| cmb | nu+prob | `uniqueness` | accepted | The finite slice comparison retains the full identified-root, affine-fiber and endpoint sets at every winning size. |
| cmb | nu+prob+size | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| cmb | nu+prob+size | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| cmb | nu+prob+size | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| cmb | nu+size | `criterion` | accepted | The bounded size comparison keeps flat slices, identified moments, exposed heights and included endpoint atoms. |
| cmb | nu+size | `failure` | regraded | Exposed faces and scalar extreme sets omit full-law limits along unidentified directions. |
| cmb | nu+size | `uniqueness` | accepted | The finite slice comparison retains the full identified-root, affine-fiber and endpoint sets at every winning size. |
| cmb | prob | `criterion` | accepted | The bounded size comparison keeps flat slices, identified moments, exposed heights and included endpoint atoms. |
| cmb | prob | `failure` | regraded | Exposed faces and scalar extreme sets omit full-law limits along unidentified directions. |
| cmb | prob | `uniqueness` | accepted | The finite slice comparison retains the full identified-root, affine-fiber and endpoint sets at every winning size. |
| cmb | prob+size | `criterion` | accepted | The bounded size comparison keeps flat slices, identified moments, exposed heights and included endpoint atoms. |
| cmb | prob+size | `failure` | regraded | Exposed faces and scalar extreme sets omit full-law limits along unidentified directions. |
| cmb | prob+size | `uniqueness` | accepted | The finite slice comparison retains the full identified-root, affine-fiber and endpoint sets at every winning size. |
| cmb | size | `criterion` | accepted | The bounded size comparison keeps flat slices, identified moments, exposed heights and included endpoint atoms. |
| cmb | size | `failure` | regraded | Exposed faces and scalar extreme sets omit full-law limits along unidentified directions. |
| cmb | size | `uniqueness` | accepted | The finite slice comparison retains the full identified-root, affine-fiber and endpoint sets at every winning size. |
| cmp | all free | `criterion` | accepted | The permitted hull directions and the mean-matched geometric KKT test settle equality, with mixed two-cell data attainable at positive dispersion. |
| cmp | all free | `failure` | accepted | The successive-ratio gaps force only point or adjacent laws at large dispersion, and the finite geometric edge has its rate inclusion test. |
| cmp | all free | `uniqueness` | regraded | The proposed affine fiber must exclude zero dispersion at rates at least one. |
| cmp | lambda | `criterion` | accepted | The factorial moment decreases strictly off flat sets and its upper endpoint is tested with the parent rate restriction. |
| cmp | lambda | `failure` | accepted | The minimum-statistic law and excluded finite geometric edge exhaust winning failures at positive held rate. |
| cmp | lambda | `uniqueness` | accepted | Strict scalar covariance supplies one root and explicit flat or atomic branches give the remaining fits. |
| cmp | lambda+nu | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| cmp | lambda+nu | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| cmp | lambda+nu | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| cmp | nu | `criterion` | accepted | The conditional mean increases strictly off singleton branches and the geometric upper endpoint is included only in the proper domain. |
| cmp | nu | `failure` | accepted | The lower or upper atom and the finite geometric uniform exhaust winning endpoint escapes. |
| cmp | nu | `uniqueness` | accepted | Strict scalar covariance supplies one root and explicit flat or atomic branches give the remaining fits. |
| dlaplace | all free | `criterion` | accepted | Clipping or projecting integer centers reduces the problem to finitely many distance-moment slices with exact endpoint comparisons. |
| dlaplace | all free | `failure` | regraded | Unbounded exterior centers can preserve an attained geometric plateau and should be stated explicitly. |
| dlaplace | all free | `uniqueness` | accepted | Distance scores identify the rate when free and the exact center reduction retains all median or exterior plateau ties. |
| dlaplace | location | `criterion` | accepted | The distance score is strictly decreasing off a singleton and its endpoint expectations give the exact root interval. |
| dlaplace | location | `failure` | accepted | With center held, the nearest-cell and uniform rate ends are exhaustive at positive height. |
| dlaplace | location | `uniqueness` | accepted | Distance scores identify the rate when free and the exact center reduction retains all median or exterior plateau ties. |
| dlaplace | location+scale | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| dlaplace | location+scale | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| dlaplace | location+scale | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| dlaplace | scale | `criterion` | accepted | Projecting centers at the held rate leaves finitely many representative laws and every winning plateau preimage. |
| dlaplace | scale | `failure` | regraded | Unbounded exterior centers can preserve an attained geometric plateau and should be stated explicitly. |
| dlaplace | scale | `uniqueness` | accepted | Distance scores identify the rate when free and the exact center reduction retains all median or exterior plateau ties. |
| dnorm | all free | `case:two-cell-tie` | regraded | Reflection gives two exact saturated fits and a refutation earns one-sided coverage. |
| dnorm | all free | `criterion` | accepted | The integrated cell weights yield only sharp point or pair and broad geometric competitors, with equality handled by compact superlevels. |
| dnorm | all free | `failure` | accepted | The bounded-precision tilt and infinite-precision projection exhaust the sharp and geometric faces. |
| dnorm | mean | `case:two-cell-cardinality` | accepted | Exact reflection gives the flat balanced-edge case and folding yields the strict probability interval off that edge. |
| dnorm | mean | `criterion` | accepted | The broad uniform or zero height and the sharp projected law give both endpoints of the scalar comparison. |
| dnorm | mean | `failure` | accepted | Fixing the center restricts sharp concentration to its projected cell or equal halves at a shared edge. |
| dnorm | mean+sd | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| dnorm | mean+sd | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| dnorm | mean+sd | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| dnorm | sd | `case:two-cell-cardinality` | accepted | The derivative in location has the strict sign given by the ordered within-cell means and its endpoint odds span the mixed targets. |
| dnorm | sd | `criterion` | accepted | At held scale, the only positive escaping heights are cutoff atoms and these saturate exactly on corresponding constants. |
| dnorm | sd | `failure` | accepted | Translating at fixed precision forces a favored cutoff atom or loss of fixed masses. |
| dunif | all free | `criterion` | accepted | Each free effective endpoint reaches its observed extreme under the closed-window inequalities. |
| dunif | all free | `failure` | accepted | Effective widening lowers the likelihood and clipped parameter escapes preserve an attained law. |
| dunif | all free | `uniqueness` | accepted | All parent endpoint preimages of the shortest feasible effective interval are given explicitly. |
| dunif | max | `criterion` | accepted | Each free effective endpoint reaches its observed extreme under the closed-window inequalities. |
| dunif | max | `failure` | accepted | Effective widening lowers the likelihood and clipped parameter escapes preserve an attained law. |
| dunif | max | `uniqueness` | accepted | All parent endpoint preimages of the shortest feasible effective interval are given explicitly. |
| dunif | max+min | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| dunif | max+min | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| dunif | max+min | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| dunif | min | `criterion` | accepted | Each free effective endpoint reaches its observed extreme under the closed-window inequalities. |
| dunif | min | `failure` | accepted | Effective widening lowers the likelihood and clipped parameter escapes preserve an attained law. |
| dunif | min | `uniqueness` | accepted | All parent endpoint preimages of the shortest feasible effective interval are given explicitly. |
| dweibull | all free | `case:two-cell-tie` | regraded | Exact opposite polynomial signs bracket an empirical-odds root at each of two finite shapes. |
| dweibull | all free | `criterion` | accepted | The joint rate and power limits exhaust the faces, and the attainable short-window odds are checked before saturation on larger support. |
| dweibull | all free | `failure` | accepted | The joint expansion and separating increments account for all positive-height power, Pareto, atom and adjacent paths. |
| dweibull | shape1 | `case:two-cell-comparison` | accepted | The zero-end analytic extension and eventual sign of the odds derivative prove a finite two-cell comparison including ties. |
| dweibull | shape1 | `criterion` | regraded | The face comparison remains valid, while the cited parent theorem corrects the full-window claim for samples with maximum one. |
| dweibull | shape1 | `failure` | accepted | The two held-probability endpoints follow from the displayed survival differences, including the fixed Bernoulli mass when zero is retained. |
| dweibull | shape1+shape2 | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| dweibull | shape1+shape2 | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| dweibull | shape1+shape2 | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| dweibull | shape2 | `criterion` | regraded | The proved strict signed score test needs the explicit compatible singleton branch. |
| dweibull | shape2 | `failure` | accepted | The strict rate profile has only its increment and lower-atom ends, with broad ray loss of mass. |
| dweibull | shape2 | `uniqueness` | regraded | The negative-curvature argument identifies one rate on a nonsingleton and the compatible singleton remains flat. |
| hyper | all free | `case:two-cell-comparison` | accepted | The exact rational ratio image or monotone integer bracketing with singleton checks gives the full short-window comparison. |
| hyper | all free | `criterion` | accepted | The integer-ratio subsequence classification bounds all growing populations and the finite-height equality rule includes ties. |
| hyper | all free | `failure` | accepted | The integer recurrence and the summable weight bound exhaust growing-design limits subject to the specified holds. |
| hyper | N | `criterion` | accepted | The held population gives a finite enumeration of every compatible integer design. |
| hyper | N | `failure` | accepted | A finite compatible design has positive mass and a finite enumeration leaves no population escape. |
| hyper | N | `uniqueness` | accepted | Exact rational comparison across a bounded list retains every tied integer design. |
| hyper | N+k | `criterion` | accepted | The held population gives a finite enumeration of every compatible integer design. |
| hyper | N+k | `failure` | accepted | A finite compatible design has positive mass and a finite enumeration leaves no population escape. |
| hyper | N+k | `uniqueness` | accepted | Exact rational comparison across a bounded list retains every tied integer design. |
| hyper | N+m | `criterion` | accepted | The held population gives a finite enumeration of every compatible integer design. |
| hyper | N+m | `failure` | accepted | A finite compatible design has positive mass and a finite enumeration leaves no population escape. |
| hyper | N+m | `uniqueness` | accepted | Exact rational comparison across a bounded list retains every tied integer design. |
| hyper | k | `case:two-cell-comparison` | accepted | The exact rational ratio image or monotone integer bracketing with singleton checks gives the full short-window comparison. |
| hyper | k | `criterion` | accepted | The integer-ratio subsequence classification bounds all growing populations and the finite-height equality rule includes ties. |
| hyper | k | `failure` | accepted | The integer recurrence and the summable weight bound exhaust growing-design limits subject to the specified holds. |
| hyper | k+m | `case:population-only` | accepted | Both held counts force the adjacent ratios to zero as population grows, with the eventual lower end fixed. |
| hyper | k+m | `case:two-cell-comparison` | accepted | The exact rational ratio image or monotone integer bracketing with singleton checks gives the full short-window comparison. |
| hyper | k+m | `criterion` | accepted | The integer-ratio subsequence classification bounds all growing populations and the finite-height equality rule includes ties. |
| hyper | k+m | `failure` | accepted | The integer recurrence and the summable weight bound exhaust growing-design limits subject to the specified holds. |
| hyper | m | `case:two-cell-comparison` | accepted | The exact rational ratio image or monotone integer bracketing with singleton checks gives the full short-window comparison. |
| hyper | m | `criterion` | accepted | The integer-ratio subsequence classification bounds all growing populations and the finite-height equality rule includes ties. |
| hyper | m | `failure` | accepted | The integer recurrence and the summable weight bound exhaust growing-design limits subject to the specified holds. |
| lgser | all free | `criterion` | regraded | The strict mean interval applies on nonsingletons and the general singleton lemma supplies the flat branch. |
| lgser | all free | `failure` | accepted | The endpoint ratios give the lower atom and harmonic weights, whose sums diverge on a ray. |
| lgser | all free | `uniqueness` | regraded | Strict variance gives one root on nonsingletons and the general singleton lemma gives every flat fit. |
| lgser | theta | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| lgser | theta | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| lgser | theta | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| nhyper | all free | `case:two-cell-comparison` | accepted | The exact rational ratio image or monotone integer bracketing with singleton checks gives the full short-window comparison. |
| nhyper | all free | `criterion` | accepted | The integer-ratio subsequence classification bounds all growing populations and the finite-height equality rule includes ties. |
| nhyper | all free | `failure` | accepted | The integer recurrence and the summable weight bound exhaust growing-design limits subject to the specified holds. |
| nhyper | N | `criterion` | accepted | The held population gives a finite enumeration of every compatible integer design. |
| nhyper | N | `failure` | accepted | A finite compatible design has positive mass and a finite enumeration leaves no population escape. |
| nhyper | N | `uniqueness` | accepted | Exact rational comparison across a bounded list retains every tied integer design. |
| nhyper | N+m | `criterion` | accepted | The held population gives a finite enumeration of every compatible integer design. |
| nhyper | N+m | `failure` | accepted | A finite compatible design has positive mass and a finite enumeration leaves no population escape. |
| nhyper | N+m | `uniqueness` | accepted | Exact rational comparison across a bounded list retains every tied integer design. |
| nhyper | N+r | `criterion` | accepted | The held population gives a finite enumeration of every compatible integer design. |
| nhyper | N+r | `failure` | accepted | A finite compatible design has positive mass and a finite enumeration leaves no population escape. |
| nhyper | N+r | `uniqueness` | accepted | Exact rational comparison across a bounded list retains every tied integer design. |
| nhyper | m | `case:ray-attainment` | accepted | Holding the marked count forces the polynomial tail to lose every fixed positive mass, and the zero atom is attained. |
| nhyper | m | `case:two-cell-comparison` | accepted | The exact rational ratio image or monotone integer bracketing with singleton checks gives the full short-window comparison. |
| nhyper | m | `criterion` | accepted | The integer-ratio subsequence classification bounds all growing populations and the finite-height equality rule includes ties. |
| nhyper | m | `failure` | accepted | The integer recurrence and the summable weight bound exhaust growing-design limits subject to the specified holds. |
| nhyper | m+r | `case:ray-attainment` | accepted | Holding the marked count forces the polynomial tail to lose every fixed positive mass, and the zero atom is attained. |
| nhyper | m+r | `case:two-cell-comparison` | accepted | The exact rational ratio image or monotone integer bracketing with singleton checks gives the full short-window comparison. |
| nhyper | m+r | `criterion` | accepted | The integer-ratio subsequence classification bounds all growing populations and the finite-height equality rule includes ties. |
| nhyper | m+r | `failure` | accepted | The integer recurrence and the summable weight bound exhaust growing-design limits subject to the specified holds. |
| nhyper | r | `case:two-cell-comparison` | accepted | The exact rational ratio image or monotone integer bracketing with singleton checks gives the full short-window comparison. |
| nhyper | r | `criterion` | accepted | The integer-ratio subsequence classification bounds all growing populations and the finite-height equality rule includes ties. |
| nhyper | r | `failure` | accepted | The integer recurrence and the summable weight bound exhaust growing-design limits subject to the specified holds. |
| pig | all free | `case:finite-rational` | accepted | The positive Bessel polynomial weights and the rational held charts give semialgebraic global comparisons at algebraic inputs. |
| pig | all free | `case:two-cell-tie` | regraded | The two rational Bessel-coordinate pairs both lie in the admissible domain and match the empirical odds. |
| pig | all free | `criterion` | accepted | The extended Bessel coordinates exhaust the compatible faces, and the in-domain equality rule includes the zero atom and finite ties. |
| pig | all free | `failure` | accepted | The Bessel-ratio tails and proper pgf limits exhaust the Poisson, Sibuya, Lévy and endpoint cases. |
| pig | mean | `case:finite-rational` | accepted | The positive Bessel polynomial weights and the rational held charts give semialgebraic global comparisons at algebraic inputs. |
| pig | mean | `criterion` | accepted | The extended Bessel coordinates exhaust the compatible faces, and the in-domain equality rule includes the zero atom and finite ties. |
| pig | mean | `failure` | regraded | The two endpoint paths for the held coordinate exclude several faces in the generic wording. |
| pig | mean+shape | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| pig | mean+shape | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| pig | mean+shape | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| pig | shape | `case:finite-rational` | accepted | The positive Bessel polynomial weights and the rational held charts give semialgebraic global comparisons at algebraic inputs. |
| pig | shape | `case:positive-shape-ray` | accepted | The exact inward derivative strictly improves the Lévy face and only all-l data can win at the lower atom. |
| pig | shape | `criterion` | accepted | The extended Bessel coordinates exhaust the compatible faces, and the in-domain equality rule includes the zero atom and finite ties. |
| pig | shape | `failure` | regraded | The two endpoint paths for the held coordinate exclude several faces in the generic wording. |
| skellam | all free | `case:two-cell-comparison` | accepted | Every positive product supplies an exact empirical-odds solution, with separate included-axis checks for constants. |
| skellam | all free | `criterion` | accepted | Admissible axes and stationary charts are compared against the exhaustive Bessel-ratio escapes, with equality included. |
| skellam | all free | `failure` | accepted | The order-ratio bounds separate finite-product, geometric, rare-axis and drifting limits. |
| skellam | mu1 | `case:two-cell-comparison` | accepted | Strict log-concavity of the convolution gives the held-rate odds sign, and reflected Poisson covers a held zero rate. |
| skellam | mu1 | `criterion` | accepted | Admissible axes and stationary charts are compared against the exhaustive Bessel-ratio escapes, with equality included. |
| skellam | mu1 | `failure` | regraded | Holding one rate removes the large-product finite-tilt face. |
| skellam | mu1+mu2 | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| skellam | mu1+mu2 | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| skellam | mu1+mu2 | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |
| skellam | mu2 | `case:two-cell-comparison` | accepted | Strict log-concavity of the convolution gives the held-rate odds sign, and reflected Poisson covers a held zero rate. |
| skellam | mu2 | `criterion` | accepted | Admissible axes and stationary charts are compared against the exhaustive Bessel-ratio escapes, with equality included. |
| skellam | mu2 | `failure` | regraded | Holding one rate removes the large-product finite-tilt face. |
| yulesimon | all free | `criterion` | accepted | The strict ray score and the rational finite-window comparison include endpoint equality and singleton branches. |
| yulesimon | all free | `failure` | accepted | The exact survival ratios give the only two endpoints, with the harmonic normalizer diverging on rays. |
| yulesimon | all free | `uniqueness` | regraded | The ray root is unique and the finite-window polynomial proves a finite stationary list whose ranking earns one-sided coverage. |
| yulesimon | rho | `criterion` | accepted | Positive retained mass and support at each occupied cell are sufficient to evaluate the sole law. |
| yulesimon | rho | `failure` | accepted | A fixed tuple cannot escape and zero retained or observed mass gives incompatibility. |
| yulesimon | rho | `uniqueness` | accepted | The admissible held tuple is the entire compatible maximizing set. |

## Proof errors and scope corrections

- **CMB exposed faces miss escaping flat directions.** Hold size two and retain `[1,Inf)`. On sample `(1,2)`, take log odds `eta = nu log(2)` and let dispersion diverge. The law stays `(1/2,1/2)` and the height stays `1/4`. The full retained hull survives. With fixed interior probability and size three on `[1,2]`, both cells have the same binomial coefficient, so the full odds law also persists as dispersion diverges. With fixed dispersion a singleton can remain an atom as probability tends to an endpoint where the conditioning event is undefined. Six failure rows retain one-sided exposed-law claims, consistent with the positive-truncation review. The bounded-design attainment and full-set comparisons already keep the feasible flat slices. With both canonical coefficients held, size is bounded and direct enumeration proves attainment with no parameter escape. [Fixed-design hull and fibers](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown) · [Held-probability flat statistic](2026-11-27-trunc-discrete-math-report.md#cmb-probsize-truncateknown) · [Earlier review](2026-11-27-mod-rev2-report.md#proof-errors-and-scope-corrections)
- **CMP's displayed affine fiber is too large at zero dispersion.** On `[0,1]`, sample `(0,1,1)` requires odds two, so the fiber has rate two at every positive dispersion. The displayed fiber includes `(lambda,nu)=(2,0)`, where the parent series diverges. The report's parent-domain restriction excludes that point. Keeping the intersection with the proper domain gives the full finite fiber and preserves attainment. At three or more cells the strict covariance and inward test cover the identified branches, including an exact zero inward derivative. [Joint CMP proof](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown) · [Held-law domain](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)
- **At held probability, discrete Weibull can fail to attain on the full window even with a positive observation.** Take `shape1=1/2`, window `[0,Inf)` and sample `(1)`. Its probability is `1/2 - (1/2)^(2^shape2)`, strictly below one-half at every finite shape. It increases to one-half at the excluded Bernoulli law. On the full window the correct split is all zeros (flat), maximum one (excluded Bernoulli) and maximum at least two (unique finite fit). The two-endpoint scalar comparison remains valid on proper windows. On a positive ray with lower cut one the normalizer changes, and the held-probability positive-truncation result remains valid. [Claim under review](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown) · [Parent theorem](2026-11-25-dweibull-math-report.md#attainment-and-maximizer-sets) · [Positive-ray theorem](2026-11-27-zero-mod-math-report.md#dweibull-all-free-truncated)
- **Hold-specific escape lists are narrower than several submitted failure claims.** In beta-binomial with one shape and size held, the simultaneous zero-shape mixture and diverging-both binomial paths are unavailable. With beta held and size growing, `N/(N+beta)` tends to one, ruling out a proper growing-size NB or Poisson limit. In beta-negative-binomial, the ordered recurrence must be specialized after restoring the two numerator labels. Holding alpha rules out positive-rate Poisson growth, and holding either numerator rules out the two-diverging-numerator branch. In PIG, held shape leaves the lower atom and Poisson–Lévy competitor at positive shape, with the latter strictly improvable on a ray. Held mean leaves Sibuya and Poisson. In Skellam, holding one positive rate rules out a finite-slope large-product tilt. These restrictions are reflected in the corresponding failure rows. [Beta and count-family proofs](2026-11-27-trunc-discrete-math-report.md#finite-population-and-beta-mixture-families) · [PIG and Skellam proofs](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)
- **An unbounded discrete Laplace center can preserve a winning plateau.** On `[0,1]` with held scale `1/2` and sample `(0,0,1)`, every center at or below zero gives the empirical law. Taking the center to minus infinity preserves that same law. The center reduction proves attainment and all finite preimages. The corresponding failure claims now record the tied plateau limit. [Center reduction](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)
- **Critical candidates and full maximizing sets earn different grades.** The fixed-size beta-binomial rational argument proves a finite number of critical components and exact height comparisons at algebraic holds. The finite-window Yule–Simon argument proves finitely many roots, and its ray argument proves uniqueness. The stationary rankings in those main cardinality rows earn one-sided coverage. The beta-negative-binomial and PIG finite-window cases give a separate algebraic decision: the rational likelihood, its positive denominator and the admissible domain allow quantifier elimination of the assertion that a candidate dominates every feasible parameter, retaining literal equality and flat sets. CMB and the bounded integer designs give the full sets by identified moment equations or direct enumeration. [Rational and canonical lemmas](2026-11-27-trunc-discrete-math-report.md#general-lemmas) · [Yule–Simon proof](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown) · [Prior cardinality grading](2026-11-27-mod-rev3-report.md#proof-errors-and-grading-corrections)

In the finite-to-face comparisons, a witness equal to the escaped height attains if no finite law is better. A strict finite improvement confines a maximizing subsequence to a compact level above the escaped height. The family-specific subsequence bounds justify that step. The count likelihood is a finite product of probabilities even on a ray, so lost probability at unobserved cells does not require tightness if the relevant cell limits and their normalization are controlled. Closed cuts round inward before intersection with parameter-dependent support. The singleton and two-cell checks precede the larger-window tests. For a disconnected odds image, compare the best likelihood on both sides of the empirical odds and retain all tied preimages. [Likelihood closure](../ml-behaviour.tex#lem:closure) · [Atlas overview](../existence-atlas/overview.md) · [General count-window rules](2026-11-27-trunc-discrete-math-report.md#general-lemmas) · [Modifier convention](../../design/modifiers.md)

## Agreement on the positive ray

The overlap consists of beta-binomial, beta-negative-binomial, CMB, CMP, discrete Weibull and PIG. All use the same positive cells on `[1,Inf)` and the same exclusion of parent zero atoms with no positive mass. CMB's incomplete failure grade is carried over. PIG's Lévy limit is reachable, but the positive-ray proof shows why it cannot be a winner in the all-free or positive-shape-held regime. The Weibull error above concerns the full parent window starting at zero. The held-probability criterion on the positive ray still gives nonattainment exactly on all ones. The beta-family finite-window witnesses and the CMP two-cell fiber require the stated short effective support. Their grades make no broader uniqueness claim on the ray. There is no remaining disagreement with the reviewed positive-ray criteria. Logarithmic series has a hurdle exception and no separate eligible `zero:truncated` row. [Reviewed zero rows](../existence-atlas/modifier-proofs.csv) · [Zero-modifier analysis](2026-11-27-zero-mod-math-report.md) · [Earlier reviews](2026-11-27-mod-rev1-report.md) · [CMB review](2026-11-27-mod-rev2-report.md) · [Equality review](2026-11-27-mod-rev3-report.md)

## Rows not joined

Every proposed key has an existing parent mask and is eligible for `truncate:known`. The join file contains all 211 keys with origin `TASK-457`. No rejected or unmatched keys remain. [Parent atlas](../existence-atlas/atlas.csv) · [Modifier atlas](../existence-atlas/modifier-atlas.csv)

## Open by family

| Family | Remaining proof or decision task |
| --- | --- |
| bbinom | Bound the free-size tail uniformly near the NB, Poisson, power and harmonic comparisons. Decide finite equality at that tail and give general cardinality beyond the explicit short-window fibers and fixed-design algebraic comparisons. |
| bnbinom | Isolate competitive ray roots on compact superlevels and decide equality with reachable ray faces. The swap witnesses leave possible distinct unordered winners open. |
| cmb | Complete the escape classification for rank-zero and rank-one charts, including paths that preserve the full conditional law. Efficient certification across the bounded size range is also open. |
| cmp | The exact attainment and maximizing-set rules are covered for these masks. A fitter would still need certified moment evaluations and brackets near the geometric boundary. |
| dlaplace | The finite center reduction, scalar root and plateau preimages settle attainment and the maximizing set. Certified ranking between center slices would still be needed in an implementation. |
| dnorm | Isolate all relevant scalar and joint roots on larger proper windows. The geometric inward score gives a strict-improvement test, and its nonpositive sign alone leaves finite-to-face equality and general cardinality open. |
| dunif | The shortest-window and clipping formulas settle the proposed existence and full-set questions. |
| dweibull | Control all competitive shape profiles on larger proper windows and settle exact comparison against power or Pareto laws. For held probability on positive two-cell windows the finite root argument leaves exact root isolation and a simpler count open. |
| hyper | Bound the unbounded population tail near equality against binomial and Poisson laws, and characterize all winning designs beyond the exact short-window rules. |
| lgser | The scalar mean threshold gives exact attainment and the full flat or unique set. A fitter would still need a certified bracket near the harmonic limit. |
| nhyper | Decide the sign and equality of unbounded-population comparisons against shifted NB and polynomial weights, and characterize ties beyond the short-window and finite-design rules. |
| pig | Isolate competitive scalar and joint ray roots and decide finite equality against Poisson and Sibuya faces. The finite-window Lévy derivative vanishes and needs an additional comparison for a simple decision. |
| skellam | Bound the proper-window scalar and joint root counts and decide exact equality with geometric or axis laws beyond the two-cell odds rules. |
| yulesimon | Give a simpler finite-window sample test and a uniform cardinality result beyond the ray proof and finite polynomial candidate list. |

## Checks

The workstation single-slot runs passed the independent exact witness and counterexample checks, the join-file validator and the repository's alternate-input atlas validator. The TASK-457 replay produced the same 36 numerical records at 80 digits in scratch. Those records serve as spot checks. The exact rational comparisons, symmetry and saturation arguments establish the witnessed ties. The earlier beta-negative-binomial positive-ray witness and CMB comparisons were also replayed. [Exact checks](2026-11-27-mod-rev8-data/exact_checks.py) · [Join validator](2026-11-27-mod-rev8-data/validate.py) · [Source replay](2026-11-27-trunc-discrete-math-data/check.py)

The repository Markdown link check has the same diagnostics as the pre-review baseline. The counted error is the known `P+Q` false positive in the beta-prime report at line 461. The Batteries path is absent from this checkout's scan. No build or package suite applies to this proof review.
