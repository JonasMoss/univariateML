# Independent review of estimated truncation bounds

TASK-471 reviewed 552 TASK-468 proposals across 13 families. The join file contains 552 written rows. 365 are accepted as submitted and 187 are regraded, including wording corrections that preserve the proof grade. 0 are rejected. The final directions are 156 `iff`, 337 `exact`, and 59 `one-sided`. [Reviewed CSV](2026-11-28-mod-rev11b-data/reviewed.csv) · [Proposed proofs and window reductions](2026-11-28-trunc-est2-math-report.md)

## Row decisions

`lower`, `upper` and `both` identify the estimated cut or cuts. Likelihoods use closed windows and model-specific support intersections. Parent masks and modifier keys were checked against both atlases. Each transferred result was checked against the corresponding reviewed known-window mask before applying its tight window. [Parent atlas](../existence-atlas/atlas.csv) · [Reviewed known-window proofs](../existence-atlas/modifier-proofs.csv) · [Modifier design](../../design/modifiers.md)

| Family | Held | Estimated | Question | Verdict | Reason |
| --- | --- | --- | --- | --- | --- |
| dlaplace | all free | lower | criterion | accepted | Each clipped center has a strictly concave rate profile on a nonsingleton and the finite slice comparison retains equality between attaining and excluded heights. |
| dlaplace | all free | lower | failure | accepted | Center clipping and the uniform geometric-tail bound control joint center and rate escape, including a tied plateau. |
| dlaplace | all free | lower | uniqueness | regraded | The center comparison is exhaustive, but the singleton must retain every rate and center. |
| dlaplace | all free | upper | criterion | accepted | Each clipped center has a strictly concave rate profile on a nonsingleton and the finite slice comparison retains equality between attaining and excluded heights. |
| dlaplace | all free | upper | failure | accepted | Center clipping and the uniform geometric-tail bound control joint center and rate escape, including a tied plateau. |
| dlaplace | all free | upper | uniqueness | regraded | The center comparison is exhaustive, but the singleton must retain every rate and center. |
| dlaplace | all free | both | criterion | accepted | Each clipped center has a strictly concave rate profile on a nonsingleton and the finite slice comparison retains equality between attaining and excluded heights. |
| dlaplace | all free | both | failure | accepted | Center clipping and the uniform geometric-tail bound control joint center and rate escape, including a tied plateau. |
| dlaplace | all free | both | uniqueness | regraded | The center comparison is exhaustive, but the singleton must retain every rate and center. |
| dlaplace | location | lower | criterion | accepted | The endpoint distance expectations bracket the unique finite rate on a nonsingleton and all rates fit a singleton. |
| dlaplace | location | lower | failure | regraded | Holding the center leaves only the rate ends, including the diffuse broad-rate limit on rays. |
| dlaplace | location | lower | uniqueness | regraded | The held center cannot vary, and the rate fiber is the whole domain when the retained cell is a singleton. |
| dlaplace | location | upper | criterion | accepted | The endpoint distance expectations bracket the unique finite rate on a nonsingleton and all rates fit a singleton. |
| dlaplace | location | upper | failure | regraded | Holding the center leaves only the rate ends, including the diffuse broad-rate limit on rays. |
| dlaplace | location | upper | uniqueness | regraded | The held center cannot vary, and the rate fiber is the whole domain when the retained cell is a singleton. |
| dlaplace | location | both | criterion | accepted | The endpoint distance expectations bracket the unique finite rate on a nonsingleton and all rates fit a singleton. |
| dlaplace | location | both | failure | regraded | Holding the center leaves only the rate ends, including the diffuse broad-rate limit on rays. |
| dlaplace | location | both | uniqueness | regraded | The held center cannot vary, and the rate fiber is the whole domain when the retained cell is a singleton. |
| dlaplace | location+scale | lower | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| dlaplace | location+scale | lower | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| dlaplace | location+scale | lower | uniqueness | accepted | The held parent pair is fixed, and all compatible nominal cuts that retain the same optimal cells tie. |
| dlaplace | location+scale | upper | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| dlaplace | location+scale | upper | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| dlaplace | location+scale | upper | uniqueness | accepted | The held parent pair is fixed, and all compatible nominal cuts that retain the same optimal cells tie. |
| dlaplace | location+scale | both | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| dlaplace | location+scale | both | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| dlaplace | location+scale | both | uniqueness | accepted | The held parent pair is fixed, and all compatible nominal cuts that retain the same optimal cells tie. |
| dlaplace | scale | lower | criterion | accepted | At fixed rate the finite center list and exterior preimages realize the maximum. |
| dlaplace | scale | lower | failure | accepted | A center escaping along an exterior plateau preserves its attained law, while it loses along an unbounded retained side. |
| dlaplace | scale | lower | uniqueness | regraded | At held rate only the center changes, and a singleton makes the entire center domain flat. |
| dlaplace | scale | upper | criterion | accepted | At fixed rate the finite center list and exterior preimages realize the maximum. |
| dlaplace | scale | upper | failure | accepted | A center escaping along an exterior plateau preserves its attained law, while it loses along an unbounded retained side. |
| dlaplace | scale | upper | uniqueness | regraded | At held rate only the center changes, and a singleton makes the entire center domain flat. |
| dlaplace | scale | both | criterion | accepted | At fixed rate the finite center list and exterior preimages realize the maximum. |
| dlaplace | scale | both | failure | accepted | A center escaping along an exterior plateau preserves its attained law, while it loses along an unbounded retained side. |
| dlaplace | scale | both | uniqueness | regraded | At held rate only the center changes, and a singleton makes the entire center domain flat. |
| dnorm | all free | lower | criterion | accepted | Sharp adjacent saturation dispatches first, and for larger sample range the integrated-cell scores are compared with the optimized geometric face including equality. |
| dnorm | all free | lower | failure | accepted | The precision and linear-tilt subsequences exhaust sharp cells, geometric faces and drift after replacing the carrier by the rounded tight cells. |
| dnorm | all free | lower | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dnorm | all free | lower | case:two-cell-tie | accepted | The two retained cells are reflections about mean one at both finite scales, so the identical empirical odds certify both global maxima. |
| dnorm | all free | upper | criterion | accepted | Sharp adjacent saturation dispatches first, and for larger sample range the integrated-cell scores are compared with the optimized geometric face including equality. |
| dnorm | all free | upper | failure | accepted | The precision and linear-tilt subsequences exhaust sharp cells, geometric faces and drift after replacing the carrier by the rounded tight cells. |
| dnorm | all free | upper | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dnorm | all free | upper | case:two-cell-tie | accepted | The two retained cells are reflections about mean one at both finite scales, so the identical empirical odds certify both global maxima. |
| dnorm | all free | both | criterion | accepted | Sharp adjacent saturation dispatches first, and for larger sample range the integrated-cell scores are compared with the optimized geometric face including equality. |
| dnorm | all free | both | failure | accepted | The precision and linear-tilt subsequences exhaust sharp cells, geometric faces and drift after replacing the carrier by the rounded tight cells. |
| dnorm | all free | both | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dnorm | all free | both | case:two-cell-tie | accepted | The two retained cells are reflections about mean one at both finite scales, so the identical empirical odds certify both global maxima. |
| dnorm | mean | lower | criterion | accepted | Fixing mean restricts the canonical parameters to a line, so the scalar score comparison includes the broad and projected sharp values and their equality. |
| dnorm | mean | lower | failure | accepted | A fixed mean leaves the broad limit and the unique projected sharp cell or equal adjacent pair, with drift at zero height on rays. |
| dnorm | mean | lower | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dnorm | mean | lower | case:two-cell-cardinality | accepted | Folding around the held mean makes the two-cell distance ratio monotone under precision tilting, with a flat exact edge case. |
| dnorm | mean | upper | criterion | accepted | Fixing mean restricts the canonical parameters to a line, so the scalar score comparison includes the broad and projected sharp values and their equality. |
| dnorm | mean | upper | failure | accepted | A fixed mean leaves the broad limit and the unique projected sharp cell or equal adjacent pair, with drift at zero height on rays. |
| dnorm | mean | upper | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dnorm | mean | upper | case:two-cell-cardinality | accepted | Folding around the held mean makes the two-cell distance ratio monotone under precision tilting, with a flat exact edge case. |
| dnorm | mean | both | criterion | accepted | Fixing mean restricts the canonical parameters to a line, so the scalar score comparison includes the broad and projected sharp values and their equality. |
| dnorm | mean | both | failure | accepted | A fixed mean leaves the broad limit and the unique projected sharp cell or equal adjacent pair, with drift at zero height on rays. |
| dnorm | mean | both | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dnorm | mean | both | case:two-cell-cardinality | accepted | Folding around the held mean makes the two-cell distance ratio monotone under precision tilting, with a flat exact edge case. |
| dnorm | mean+sd | lower | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| dnorm | mean+sd | lower | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| dnorm | mean+sd | lower | uniqueness | accepted | The compatible held tuple is fixed and integer cut rounding gives multiple nominal preimages. |
| dnorm | mean+sd | upper | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| dnorm | mean+sd | upper | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| dnorm | mean+sd | upper | uniqueness | accepted | The compatible held tuple is fixed and integer cut rounding gives multiple nominal preimages. |
| dnorm | mean+sd | both | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| dnorm | mean+sd | both | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| dnorm | mean+sd | both | uniqueness | accepted | The compatible held tuple is fixed and integer cut rounding gives multiple nominal preimages. |
| dnorm | sd | lower | criterion | accepted | At fixed precision a finite endpoint atom can attain the saturated height only at infinity on a nonsingleton, and otherwise compact positive superlevels give attainment. |
| dnorm | sd | lower | failure | accepted | With precision fixed only the canonical linear term escapes, giving an endpoint atom or drift. |
| dnorm | sd | lower | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dnorm | sd | lower | case:two-cell-cardinality | accepted | On two cells the derivative of log odds is the strictly positive difference of within-cell means and the odds span all positive values. |
| dnorm | sd | upper | criterion | accepted | At fixed precision a finite endpoint atom can attain the saturated height only at infinity on a nonsingleton, and otherwise compact positive superlevels give attainment. |
| dnorm | sd | upper | failure | accepted | With precision fixed only the canonical linear term escapes, giving an endpoint atom or drift. |
| dnorm | sd | upper | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dnorm | sd | upper | case:two-cell-cardinality | accepted | On two cells the derivative of log odds is the strictly positive difference of within-cell means and the odds span all positive values. |
| dnorm | sd | both | criterion | accepted | At fixed precision a finite endpoint atom can attain the saturated height only at infinity on a nonsingleton, and otherwise compact positive superlevels give attainment. |
| dnorm | sd | both | failure | accepted | With precision fixed only the canonical linear term escapes, giving an endpoint atom or drift. |
| dnorm | sd | both | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dnorm | sd | both | case:two-cell-cardinality | accepted | On two cells the derivative of log odds is the strictly positive difference of within-cell means and the odds span all positive values. |
| dunif | all free | lower | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | all free | lower | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | all free | lower | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | all free | upper | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | all free | upper | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | all free | upper | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | all free | both | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | all free | both | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | all free | both | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | max | lower | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | max | lower | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | max | lower | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | max | upper | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | max | upper | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | max | upper | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | max | both | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | max | both | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | max | both | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | max+min | lower | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | max+min | lower | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | max+min | lower | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | max+min | upper | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | max+min | upper | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | max+min | upper | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | max+min | both | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | max+min | both | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | max+min | both | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | min | lower | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | min | lower | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | min | lower | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | min | upper | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | min | upper | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | min | upper | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dunif | min | both | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| dunif | min | both | failure | regraded | The included singleton is an attained atom, and unbounded clipping preimages can preserve the same attained interval. |
| dunif | min | both | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| dweibull | all free | lower | criterion | accepted | Singleton and adjacent saturation plus the continuous odds image settle short windows, and the explicit escape envelope supports comparison on wider samples. |
| dweibull | all free | lower | failure | accepted | Joint rate and shape ends give power or logarithmic increments, Pareto differences, adjacent or singleton limits, and defective or vanishing ray heights. |
| dweibull | all free | lower | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dweibull | all free | lower | case:two-cell-tie | accepted | At shapes two and three the exact zero-rate odds exceed one and the infinite-rate odds vanish, yielding two finite saturated fits by continuity. |
| dweibull | all free | upper | criterion | accepted | Singleton and adjacent saturation plus the continuous odds image settle short windows, and the explicit escape envelope supports comparison on wider samples. |
| dweibull | all free | upper | failure | accepted | Joint rate and shape ends give power or logarithmic increments, Pareto differences, adjacent or singleton limits, and defective or vanishing ray heights. |
| dweibull | all free | upper | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dweibull | all free | upper | case:two-cell-tie | accepted | At shapes two and three the exact zero-rate odds exceed one and the infinite-rate odds vanish, yielding two finite saturated fits by continuity. |
| dweibull | all free | both | criterion | accepted | Singleton and adjacent saturation plus the continuous odds image settle short windows, and the explicit escape envelope supports comparison on wider samples. |
| dweibull | all free | both | failure | accepted | Joint rate and shape ends give power or logarithmic increments, Pareto differences, adjacent or singleton limits, and defective or vanishing ray heights. |
| dweibull | all free | both | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dweibull | all free | both | case:two-cell-tie | accepted | At shapes two and three the exact zero-rate odds exceed one and the infinite-rate odds vanish, yielding two finite saturated fits by continuity. |
| dweibull | shape1 | lower | criterion | accepted | The two endpoint shape limits and scalar critical values settle proper windows, and the parent theorem gives the corrected maximum-one Bernoulli branch. |
| dweibull | shape1 | lower | failure | regraded | The small-shape ray limit is defective if zero is retained, and its surviving zero mass can tie the whole flat parent slice. |
| dweibull | shape1 | lower | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dweibull | shape1 | lower | case:two-cell-comparison | accepted | The first cell pair has explicit monotone odds, and on positive pairs analyticity and the eventual derivative sign leave finitely many critical values. |
| dweibull | shape1 | upper | criterion | accepted | The two endpoint shape limits and scalar critical values settle proper windows, and the parent theorem gives the corrected maximum-one Bernoulli branch. |
| dweibull | shape1 | upper | failure | regraded | The small-shape ray limit is defective if zero is retained, and its surviving zero mass can tie the whole flat parent slice. |
| dweibull | shape1 | upper | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dweibull | shape1 | upper | case:two-cell-comparison | accepted | The first cell pair has explicit monotone odds, and on positive pairs analyticity and the eventual derivative sign leave finitely many critical values. |
| dweibull | shape1 | both | criterion | accepted | The two endpoint shape limits and scalar critical values settle proper windows, and the parent theorem gives the corrected maximum-one Bernoulli branch. |
| dweibull | shape1 | both | failure | regraded | The small-shape ray limit is defective if zero is retained, and its surviving zero mass can tie the whole flat parent slice. |
| dweibull | shape1 | both | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| dweibull | shape1 | both | case:two-cell-comparison | accepted | The first cell pair has explicit monotone odds, and on positive pairs analyticity and the eventual derivative sign leave finitely many critical values. |
| dweibull | shape1+shape2 | lower | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| dweibull | shape1+shape2 | lower | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| dweibull | shape1+shape2 | lower | uniqueness | accepted | The compatible held tuple is fixed and integer cut rounding gives multiple nominal preimages. |
| dweibull | shape1+shape2 | upper | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| dweibull | shape1+shape2 | upper | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| dweibull | shape1+shape2 | upper | uniqueness | accepted | The compatible held tuple is fixed and integer cut rounding gives multiple nominal preimages. |
| dweibull | shape1+shape2 | both | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| dweibull | shape1+shape2 | both | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| dweibull | shape1+shape2 | both | uniqueness | accepted | The compatible held tuple is fixed and integer cut rounding gives multiple nominal preimages. |
| dweibull | shape2 | lower | criterion | accepted | Strict rate-score decrease makes the zero-rate sign decisive whenever some observation exceeds the effective lower count. |
| dweibull | shape2 | lower | failure | accepted | At fixed shape the increment law is the finite-window broad end and increasing rate concentrates at the lower cell. |
| dweibull | shape2 | lower | uniqueness | accepted | Strict negative rate curvature gives one finite root on each nonsingleton, and every rate ties on a singleton. |
| dweibull | shape2 | upper | criterion | accepted | Strict rate-score decrease makes the zero-rate sign decisive whenever some observation exceeds the effective lower count. |
| dweibull | shape2 | upper | failure | accepted | At fixed shape the increment law is the finite-window broad end and increasing rate concentrates at the lower cell. |
| dweibull | shape2 | upper | uniqueness | accepted | Strict negative rate curvature gives one finite root on each nonsingleton, and every rate ties on a singleton. |
| dweibull | shape2 | both | criterion | accepted | Strict rate-score decrease makes the zero-rate sign decisive whenever some observation exceeds the effective lower count. |
| dweibull | shape2 | both | failure | accepted | At fixed shape the increment law is the finite-window broad end and increasing rate concentrates at the lower cell. |
| dweibull | shape2 | both | uniqueness | accepted | Strict negative rate curvature gives one finite root on each nonsingleton, and every rate ties on a singleton. |
| skellam | all free | lower | criterion | accepted | Compare the compatible axis and finite interior candidates against the applicable Bessel or Poisson boundary heights, retaining finite equality. |
| skellam | all free | lower | failure | accepted | The Bessel product and rate ratio splits yield summable geometric tilts, rare-axis atoms or drift. |
| skellam | all free | lower | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| skellam | all free | lower | case:two-cell-comparison | accepted | At fixed positive Bessel product the tilt realizes all positive target odds, and compatible included axes are evaluated separately. |
| skellam | all free | upper | criterion | accepted | Compare the compatible axis and finite interior candidates against the applicable Bessel or Poisson boundary heights, retaining finite equality. |
| skellam | all free | upper | failure | accepted | The Bessel product and rate ratio splits yield summable geometric tilts, rare-axis atoms or drift. |
| skellam | all free | upper | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| skellam | all free | upper | case:two-cell-comparison | accepted | At fixed positive Bessel product the tilt realizes all positive target odds, and compatible included axes are evaluated separately. |
| skellam | all free | both | criterion | accepted | Compare the compatible axis and finite interior candidates against the applicable Bessel or Poisson boundary heights, retaining finite equality. |
| skellam | all free | both | failure | accepted | The Bessel product and rate ratio splits yield summable geometric tilts, rare-axis atoms or drift. |
| skellam | all free | both | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| skellam | all free | both | case:two-cell-comparison | accepted | At fixed positive Bessel product the tilt realizes all positive target odds, and compatible included axes are evaluated separately. |
| skellam | mu1 | lower | criterion | accepted | Compare the compatible axis and finite interior candidates against the applicable Bessel or Poisson boundary heights, retaining finite equality. |
| skellam | mu1 | lower | failure | accepted | The held rate rules out a finite-slope large-product tilt and the held-zero slice reduces to the corresponding signed Poisson law. |
| skellam | mu1 | lower | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| skellam | mu1 | lower | case:two-cell-comparison | accepted | The strict adjacent-ratio order or reflected Poisson chart gives the attainable odds interval, including each compatible axis endpoint. |
| skellam | mu1 | upper | criterion | accepted | Compare the compatible axis and finite interior candidates against the applicable Bessel or Poisson boundary heights, retaining finite equality. |
| skellam | mu1 | upper | failure | accepted | The held rate rules out a finite-slope large-product tilt and the held-zero slice reduces to the corresponding signed Poisson law. |
| skellam | mu1 | upper | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| skellam | mu1 | upper | case:two-cell-comparison | accepted | The strict adjacent-ratio order or reflected Poisson chart gives the attainable odds interval, including each compatible axis endpoint. |
| skellam | mu1 | both | criterion | accepted | Compare the compatible axis and finite interior candidates against the applicable Bessel or Poisson boundary heights, retaining finite equality. |
| skellam | mu1 | both | failure | accepted | The held rate rules out a finite-slope large-product tilt and the held-zero slice reduces to the corresponding signed Poisson law. |
| skellam | mu1 | both | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| skellam | mu1 | both | case:two-cell-comparison | accepted | The strict adjacent-ratio order or reflected Poisson chart gives the attainable odds interval, including each compatible axis endpoint. |
| skellam | mu1+mu2 | lower | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| skellam | mu1+mu2 | lower | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| skellam | mu1+mu2 | lower | uniqueness | accepted | The compatible held tuple is fixed and integer cut rounding gives multiple nominal preimages. |
| skellam | mu1+mu2 | upper | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| skellam | mu1+mu2 | upper | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| skellam | mu1+mu2 | upper | uniqueness | accepted | The compatible held tuple is fixed and integer cut rounding gives multiple nominal preimages. |
| skellam | mu1+mu2 | both | criterion | accepted | The fixed coefficients attain exactly when the rounded window retains positive mass at every observed cell. |
| skellam | mu1+mu2 | both | failure | accepted | Only incompatibility prevents fitting the held coefficients, and a positive-mass singleton is an attained atom. |
| skellam | mu1+mu2 | both | uniqueness | accepted | The compatible held tuple is fixed and integer cut rounding gives multiple nominal preimages. |
| skellam | mu2 | lower | criterion | accepted | Compare the compatible axis and finite interior candidates against the applicable Bessel or Poisson boundary heights, retaining finite equality. |
| skellam | mu2 | lower | failure | accepted | The held rate rules out a finite-slope large-product tilt and the held-zero slice reduces to the corresponding signed Poisson law. |
| skellam | mu2 | lower | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| skellam | mu2 | lower | case:two-cell-comparison | accepted | The strict adjacent-ratio order or reflected Poisson chart gives the attainable odds interval, including each compatible axis endpoint. |
| skellam | mu2 | upper | criterion | accepted | Compare the compatible axis and finite interior candidates against the applicable Bessel or Poisson boundary heights, retaining finite equality. |
| skellam | mu2 | upper | failure | accepted | The held rate rules out a finite-slope large-product tilt and the held-zero slice reduces to the corresponding signed Poisson law. |
| skellam | mu2 | upper | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| skellam | mu2 | upper | case:two-cell-comparison | accepted | The strict adjacent-ratio order or reflected Poisson chart gives the attainable odds interval, including each compatible axis endpoint. |
| skellam | mu2 | both | criterion | accepted | Compare the compatible axis and finite interior candidates against the applicable Bessel or Poisson boundary heights, retaining finite equality. |
| skellam | mu2 | both | failure | accepted | The held rate rules out a finite-slope large-product tilt and the held-zero slice reduces to the corresponding signed Poisson law. |
| skellam | mu2 | both | case:bound-cardinality | regraded | The proof specifies the entire conditional cut fiber by cell inclusion, which is stronger than the submitted nonuniqueness wording. |
| skellam | mu2 | both | case:two-cell-comparison | accepted | The strict adjacent-ratio order or reflected Poisson chart gives the attainable odds interval, including each compatible axis endpoint. |
| laplace | all free | lower | criterion | accepted | Any positive-height finite-rate exterior face has a finite plateau representative, leaving only the uniform threshold on bounded windows for nonconstant data. |
| laplace | all free | lower | failure | regraded | The fixed-rate exterior plateau can reach the supremum along an escaping location sequence, so the exact list needs this attained-law limit. |
| laplace | all free | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| laplace | all free | lower | case:endpoint-histogram | accepted | Strict convexity inside the endpoint gap leaves only the exterior exponential plateaus, whose values are ordered by the endpoint counts. |
| laplace | all free | lower | case:nonunique | accepted | On the tight right ray the only competing sample location has strictly smaller likelihood at every rate, and the exterior plateau peaks at rate two. |
| laplace | all free | upper | criterion | accepted | Any positive-height finite-rate exterior face has a finite plateau representative, leaving only the uniform threshold on bounded windows for nonconstant data. |
| laplace | all free | upper | failure | regraded | The fixed-rate exterior plateau can reach the supremum along an escaping location sequence, so the exact list needs this attained-law limit. |
| laplace | all free | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| laplace | all free | upper | case:endpoint-histogram | accepted | Strict convexity inside the endpoint gap leaves only the exterior exponential plateaus, whose values are ordered by the endpoint counts. |
| laplace | all free | upper | case:nonunique | regraded | A right ray is unavailable with a free upper cut, but reflection gives the exact left-ray witness. |
| laplace | all free | both | criterion | accepted | Any positive-height finite-rate exterior face has a finite plateau representative, leaving only the uniform threshold on bounded windows for nonconstant data. |
| laplace | all free | both | failure | regraded | The fixed-rate exterior plateau can reach the supremum along an escaping location sequence, so the exact list needs this attained-law limit. |
| laplace | all free | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| laplace | all free | both | case:endpoint-histogram | accepted | Strict convexity inside the endpoint gap leaves only the exterior exponential plateaus, whose values are ordered by the endpoint counts. |
| laplace | all free | both | case:nonunique | regraded | Both estimated cuts are finite, and the bounded endpoint proof gives a replacement witness with an entire maximizing location plateau. |
| laplace | mu | lower | criterion | accepted | The distance mean decreases strictly from its zero-rate limit to the projected minimum, settling both excluded equalities. |
| laplace | mu | lower | failure | accepted | The distance mean decreases strictly from its zero-rate limit to the projected minimum, settling both excluded equalities. |
| laplace | mu | lower | uniqueness | accepted | Strictly negative distance-statistic curvature gives one rate, with each free cut forced to its observed extreme. |
| laplace | mu | upper | criterion | accepted | The distance mean decreases strictly from its zero-rate limit to the projected minimum, settling both excluded equalities. |
| laplace | mu | upper | failure | accepted | The distance mean decreases strictly from its zero-rate limit to the projected minimum, settling both excluded equalities. |
| laplace | mu | upper | uniqueness | accepted | Strictly negative distance-statistic curvature gives one rate, with each free cut forced to its observed extreme. |
| laplace | mu | both | criterion | accepted | The distance mean decreases strictly from its zero-rate limit to the projected minimum, settling both excluded equalities. |
| laplace | mu | both | failure | accepted | The distance mean decreases strictly from its zero-rate limit to the projected minimum, settling both excluded equalities. |
| laplace | mu | both | uniqueness | accepted | Strictly negative distance-statistic curvature gives one rate, with each free cut forced to its observed extreme. |
| laplace | mu+sigma | lower | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| laplace | mu+sigma | lower | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| laplace | mu+sigma | lower | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| laplace | mu+sigma | upper | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| laplace | mu+sigma | upper | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| laplace | mu+sigma | upper | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| laplace | mu+sigma | both | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| laplace | mu+sigma | both | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| laplace | mu+sigma | both | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| laplace | sigma | lower | criterion | accepted | Every integrable exterior law has a finite plateau preimage, and the finite knot comparison attains on positive width. |
| laplace | sigma | lower | failure | regraded | The fixed-rate exterior plateau can reach the supremum along an escaping location sequence, so the exact list needs this attained-law limit. |
| laplace | sigma | lower | uniqueness | accepted | Strict convexity between sample locations leaves a finite cut-and-knot comparison plus all integrable plateau preimages. |
| laplace | sigma | lower | case:endpoint-histogram | accepted | Strict convexity inside the endpoint gap leaves only the exterior exponential plateaus, whose values are ordered by the endpoint counts. |
| laplace | sigma | upper | criterion | accepted | Every integrable exterior law has a finite plateau preimage, and the finite knot comparison attains on positive width. |
| laplace | sigma | upper | failure | regraded | The fixed-rate exterior plateau can reach the supremum along an escaping location sequence, so the exact list needs this attained-law limit. |
| laplace | sigma | upper | uniqueness | accepted | Strict convexity between sample locations leaves a finite cut-and-knot comparison plus all integrable plateau preimages. |
| laplace | sigma | upper | case:endpoint-histogram | accepted | Strict convexity inside the endpoint gap leaves only the exterior exponential plateaus, whose values are ordered by the endpoint counts. |
| laplace | sigma | both | criterion | accepted | Every integrable exterior law has a finite plateau preimage, and the finite knot comparison attains on positive width. |
| laplace | sigma | both | failure | regraded | The fixed-rate exterior plateau can reach the supremum along an escaping location sequence, so the exact list needs this attained-law limit. |
| laplace | sigma | both | uniqueness | accepted | Strict convexity between sample locations leaves a finite cut-and-knot comparison plus all integrable plateau preimages. |
| laplace | sigma | both | case:endpoint-histogram | accepted | Strict convexity inside the endpoint gap leaves only the exterior exponential plateaus, whose values are ordered by the endpoint counts. |
| logis | all free | lower | criterion | accepted | Uniform tail bounds at both rate ends leave only constant collapse or the integrable signed exponential threshold for the finite comparison. |
| logis | all free | lower | failure | accepted | Uniform tail bounds at both rate ends leave only constant collapse or the integrable signed exponential threshold for the finite comparison. |
| logis | all free | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| logis | all free | lower | case:endpoint-histogram | accepted | The transformed-location factor is monotone for unequal endpoint counts and cancels exactly for equal counts, with the stated rate optimization. |
| logis | all free | upper | criterion | accepted | Uniform tail bounds at both rate ends leave only constant collapse or the integrable signed exponential threshold for the finite comparison. |
| logis | all free | upper | failure | accepted | Uniform tail bounds at both rate ends leave only constant collapse or the integrable signed exponential threshold for the finite comparison. |
| logis | all free | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| logis | all free | upper | case:endpoint-histogram | accepted | The transformed-location factor is monotone for unequal endpoint counts and cancels exactly for equal counts, with the stated rate optimization. |
| logis | all free | both | criterion | accepted | Uniform tail bounds at both rate ends leave only constant collapse or the integrable signed exponential threshold for the finite comparison. |
| logis | all free | both | failure | accepted | Uniform tail bounds at both rate ends leave only constant collapse or the integrable signed exponential threshold for the finite comparison. |
| logis | all free | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| logis | all free | both | case:endpoint-histogram | accepted | The transformed-location factor is monotone for unequal endpoint counts and cancels exactly for equal counts, with the stated rate optimization. |
| logis | location | lower | criterion | accepted | Holding location fixes the collapse projection and leaves bounded uniform as the sole nonzero broad-rate competitor. |
| logis | location | lower | failure | accepted | Holding location fixes the collapse projection and leaves bounded uniform as the sole nonzero broad-rate competitor. |
| logis | location | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| logis | location | upper | criterion | accepted | Holding location fixes the collapse projection and leaves bounded uniform as the sole nonzero broad-rate competitor. |
| logis | location | upper | failure | accepted | Holding location fixes the collapse projection and leaves bounded uniform as the sole nonzero broad-rate competitor. |
| logis | location | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| logis | location | both | criterion | accepted | Holding location fixes the collapse projection and leaves bounded uniform as the sole nonzero broad-rate competitor. |
| logis | location | both | failure | accepted | Holding location fixes the collapse projection and leaves bounded uniform as the sole nonzero broad-rate competitor. |
| logis | location | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| logis | location+scale | lower | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| logis | location+scale | lower | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| logis | location+scale | lower | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| logis | location+scale | upper | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| logis | location+scale | upper | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| logis | location+scale | upper | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| logis | location+scale | both | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| logis | location+scale | both | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| logis | location+scale | both | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| logis | scale | lower | criterion | accepted | The rational score or its identically zero case covers finite locations, and each available fixed-rate exponential is compared at equality. |
| logis | scale | lower | failure | accepted | The rational score or its identically zero case covers finite locations, and each available fixed-rate exponential is compared at equality. |
| logis | scale | lower | uniqueness | accepted | Clearing positive denominators gives finitely many positive roots or an identically zero score, which supports the proposed one-sided root comparison. |
| logis | scale | lower | case:endpoint-histogram | accepted | The transformed-location factor is monotone for unequal endpoint counts and cancels exactly for equal counts, with the stated rate optimization. |
| logis | scale | lower | case:nonunique | accepted | The endpoint product cancels the transformed-location terms at held scale, giving an exact continuum on the tight bounded window. |
| logis | scale | upper | criterion | accepted | The rational score or its identically zero case covers finite locations, and each available fixed-rate exponential is compared at equality. |
| logis | scale | upper | failure | accepted | The rational score or its identically zero case covers finite locations, and each available fixed-rate exponential is compared at equality. |
| logis | scale | upper | uniqueness | accepted | Clearing positive denominators gives finitely many positive roots or an identically zero score, which supports the proposed one-sided root comparison. |
| logis | scale | upper | case:endpoint-histogram | accepted | The transformed-location factor is monotone for unequal endpoint counts and cancels exactly for equal counts, with the stated rate optimization. |
| logis | scale | upper | case:nonunique | accepted | The endpoint product cancels the transformed-location terms at held scale, giving an exact continuum on the tight bounded window. |
| logis | scale | both | criterion | accepted | The rational score or its identically zero case covers finite locations, and each available fixed-rate exponential is compared at equality. |
| logis | scale | both | failure | accepted | The rational score or its identically zero case covers finite locations, and each available fixed-rate exponential is compared at equality. |
| logis | scale | both | uniqueness | accepted | Clearing positive denominators gives finitely many positive roots or an identically zero score, which supports the proposed one-sided root comparison. |
| logis | scale | both | case:endpoint-histogram | accepted | The transformed-location factor is monotone for unequal endpoint counts and cancels exactly for equal counts, with the stated rate optimization. |
| logis | scale | both | case:nonunique | accepted | The endpoint product cancels the transformed-location terms at held scale, giving an exact continuum on the tight bounded window. |
| cauchy | all free | lower | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| cauchy | all free | lower | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| cauchy | all free | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| cauchy | all free | upper | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| cauchy | all free | upper | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| cauchy | all free | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| cauchy | all free | both | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| cauchy | all free | both | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| cauchy | all free | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| cauchy | location | lower | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| cauchy | location | lower | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| cauchy | location | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| cauchy | location | upper | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| cauchy | location | upper | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| cauchy | location | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| cauchy | location | both | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| cauchy | location | both | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| cauchy | location | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| cauchy | location+scale | lower | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| cauchy | location+scale | lower | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| cauchy | location+scale | lower | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| cauchy | location+scale | upper | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| cauchy | location+scale | upper | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| cauchy | location+scale | upper | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| cauchy | location+scale | both | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| cauchy | location+scale | both | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| cauchy | location+scale | both | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| cauchy | scale | lower | criterion | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| cauchy | scale | lower | failure | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| cauchy | scale | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| cauchy | scale | upper | criterion | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| cauchy | scale | upper | failure | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| cauchy | scale | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| cauchy | scale | both | criterion | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| cauchy | scale | both | failure | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| cauchy | scale | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| gumbel | all free | lower | criterion | accepted | The transformed exponential bounds leave collapse for constants and include joint broad-scale increasing slopes at finite upper cuts. |
| gumbel | all free | lower | failure | accepted | The transformed exponential bounds leave collapse for constants and include joint broad-scale increasing slopes at finite upper cuts. |
| gumbel | all free | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| gumbel | all free | upper | criterion | accepted | The transformed exponential bounds leave collapse for constants and include joint broad-scale increasing slopes at finite upper cuts. |
| gumbel | all free | upper | failure | accepted | The transformed exponential bounds leave collapse for constants and include joint broad-scale increasing slopes at finite upper cuts. |
| gumbel | all free | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| gumbel | all free | both | criterion | accepted | The transformed exponential bounds leave collapse for constants and include joint broad-scale increasing slopes at finite upper cuts. |
| gumbel | all free | both | failure | accepted | The transformed exponential bounds leave collapse for constants and include joint broad-scale increasing slopes at finite upper cuts. |
| gumbel | all free | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| gumbel | mu | lower | criterion | accepted | Holding location fixes the sharp-scale projection and leaves bounded uniform as the broad-scale competitor. |
| gumbel | mu | lower | failure | accepted | Holding location fixes the sharp-scale projection and leaves bounded uniform as the broad-scale competitor. |
| gumbel | mu | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| gumbel | mu | upper | criterion | accepted | Holding location fixes the sharp-scale projection and leaves bounded uniform as the broad-scale competitor. |
| gumbel | mu | upper | failure | accepted | Holding location fixes the sharp-scale projection and leaves bounded uniform as the broad-scale competitor. |
| gumbel | mu | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| gumbel | mu | both | criterion | accepted | Holding location fixes the sharp-scale projection and leaves bounded uniform as the broad-scale competitor. |
| gumbel | mu | both | failure | accepted | Holding location fixes the sharp-scale projection and leaves bounded uniform as the broad-scale competitor. |
| gumbel | mu | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| gumbel | mu+sigma | lower | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| gumbel | mu+sigma | lower | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| gumbel | mu+sigma | lower | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| gumbel | mu+sigma | upper | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| gumbel | mu+sigma | upper | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| gumbel | mu+sigma | upper | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| gumbel | mu+sigma | both | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| gumbel | mu+sigma | both | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| gumbel | mu+sigma | both | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| gumbel | sigma | lower | criterion | accepted | The transformed mean must lie strictly between the lower and zero-rate limits, whose equalities give upper collapse or the excluded decreasing exponential. |
| gumbel | sigma | lower | failure | accepted | The transformed mean must lie strictly between the lower and zero-rate limits, whose equalities give upper collapse or the excluded decreasing exponential. |
| gumbel | sigma | lower | uniqueness | accepted | Strictly negative transformed exponential curvature gives a unique finite location whenever the transformed mean is in range. |
| gumbel | sigma | upper | criterion | accepted | The transformed mean must lie strictly between the lower and zero-rate limits, whose equalities give upper collapse or the excluded decreasing exponential. |
| gumbel | sigma | upper | failure | accepted | The transformed mean must lie strictly between the lower and zero-rate limits, whose equalities give upper collapse or the excluded decreasing exponential. |
| gumbel | sigma | upper | uniqueness | accepted | Strictly negative transformed exponential curvature gives a unique finite location whenever the transformed mean is in range. |
| gumbel | sigma | both | criterion | accepted | The transformed mean must lie strictly between the lower and zero-rate limits, whose equalities give upper collapse or the excluded decreasing exponential. |
| gumbel | sigma | both | failure | accepted | The transformed mean must lie strictly between the lower and zero-rate limits, whose equalities give upper collapse or the excluded decreasing exponential. |
| gumbel | sigma | both | uniqueness | accepted | Strictly negative transformed exponential curvature gives a unique finite location whenever the transformed mean is in range. |
| hnorm | all free | lower | criterion | regraded | After the null-window check, the lower-tight concave wedge gives an iff from the strict inward second-moment sign, including zero. |
| hnorm | all free | lower | failure | accepted | The negative quadratic wedge leaves a nonpositive exponential edge or uniform, together with the constant and null-window shrink paths. |
| hnorm | all free | lower | uniqueness | accepted | The lower-tight identified wedge has a strictly concave objective and the only cut plateau begins at the fitted support endpoint. |
| hnorm | all free | upper | criterion | regraded | After the null-window check, an attained winner in either of the two branches may equal the other branch’s supremum. |
| hnorm | all free | upper | failure | accepted | The negative quadratic wedge leaves a nonpositive exponential edge or uniform, together with the constant and null-window shrink paths. |
| hnorm | all free | upper | uniqueness | accepted | The upper-only branch comparison gives at most two distinct parent winners and leaves exact separated ties open. |
| hnorm | all free | upper | case:null-effective-window | regraded | The proposed fixed-parent shrink proves divergence on this sufficient branch, which leaves the general failure classification to the main row. |
| hnorm | all free | both | criterion | regraded | After the null-window check, the lower-tight concave wedge gives an iff from the strict inward second-moment sign, including zero. |
| hnorm | all free | both | failure | accepted | The negative quadratic wedge leaves a nonpositive exponential edge or uniform, together with the constant and null-window shrink paths. |
| hnorm | all free | both | uniqueness | accepted | The lower-tight identified wedge has a strictly concave objective and the only cut plateau begins at the fitted support endpoint. |
| hnorm | mu | lower | criterion | regraded | Positive mass must be checked before the scalar squared-residual mean comparison at the held endpoint. |
| hnorm | mu | lower | failure | accepted | The fixed-support squared-residual tilt has only lower collapse and the bounded broad uniform at positive likelihood. |
| hnorm | mu | lower | uniqueness | accepted | On positive width the squared-residual statistic has positive variance, so the held-location conditional scale fit is unique. |
| hnorm | mu | upper | criterion | regraded | Positive mass must be checked before the scalar squared-residual mean comparison at the held endpoint. |
| hnorm | mu | upper | failure | accepted | The fixed-support squared-residual tilt has only lower collapse and the bounded broad uniform at positive likelihood. |
| hnorm | mu | upper | uniqueness | accepted | On positive width the squared-residual statistic has positive variance, so the held-location conditional scale fit is unique. |
| hnorm | mu | upper | case:null-effective-window | regraded | The proposed fixed-parent shrink proves divergence on this sufficient branch, which leaves the general failure classification to the main row. |
| hnorm | mu | both | criterion | regraded | Positive mass must be checked before the scalar squared-residual mean comparison at the held endpoint. |
| hnorm | mu | both | failure | accepted | The fixed-support squared-residual tilt has only lower collapse and the bounded broad uniform at positive likelihood. |
| hnorm | mu | both | uniqueness | accepted | On positive width the squared-residual statistic has positive variance, so the held-location conditional scale fit is unique. |
| hnorm | mu+sigma | lower | criterion | regraded | A compatible held law attains on positive retained width, while a null tight slice can give divergence under a free upper cut. |
| hnorm | mu+sigma | lower | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| hnorm | mu+sigma | lower | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| hnorm | mu+sigma | upper | criterion | regraded | A compatible held law attains on positive retained width, while a null tight slice can give divergence under a free upper cut. |
| hnorm | mu+sigma | upper | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| hnorm | mu+sigma | upper | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| hnorm | mu+sigma | upper | case:null-effective-window | regraded | The proposed fixed-parent shrink proves divergence on this sufficient branch, which leaves the general failure classification to the main row. |
| hnorm | mu+sigma | both | criterion | regraded | A compatible held law attains on positive retained width, while a null tight slice can give divergence under a free upper cut. |
| hnorm | mu+sigma | both | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| hnorm | mu+sigma | both | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| hnorm | sigma | lower | criterion | regraded | After the null-window check, the held-scale mean equation on the lower-tight chart or the two upper-only branches gives the exact criterion. |
| hnorm | sigma | lower | failure | accepted | At held scale only the remote negative location collapse or upper-cut shrinking support can fail on compatible data. |
| hnorm | sigma | lower | uniqueness | accepted | Strict decrease of the linear tilt score identifies the location and the support endpoint determines the cut preimages. |
| hnorm | sigma | upper | criterion | regraded | After the null-window check, the held-scale mean equation on the lower-tight chart or the two upper-only branches gives the exact criterion. |
| hnorm | sigma | upper | failure | accepted | At held scale only the remote negative location collapse or upper-cut shrinking support can fail on compatible data. |
| hnorm | sigma | upper | uniqueness | accepted | Each upper-only branch gives at most one candidate and comparison retains a possible exact tie. |
| hnorm | sigma | upper | case:null-effective-window | regraded | The proposed fixed-parent shrink proves divergence on this sufficient branch, which leaves the general failure classification to the main row. |
| hnorm | sigma | both | criterion | regraded | After the null-window check, the held-scale mean equation on the lower-tight chart or the two upper-only branches gives the exact criterion. |
| hnorm | sigma | both | failure | accepted | At held scale only the remote negative location collapse or upper-cut shrinking support can fail on compatible data. |
| hnorm | sigma | both | uniqueness | accepted | Strict decrease of the linear tilt score identifies the location and the support endpoint determines the cut preimages. |
| hcauchy | all free | lower | criterion | regraded | Shrinking at a null tight slice is checked before comparing finite candidates with the power and collapse thresholds. |
| hcauchy | all free | lower | failure | accepted | The support-constrained power envelope covers critical and exterior faces, and the upper-cut constant adds the shrinking-support end. |
| hcauchy | all free | lower | case:bound-cardinality | accepted | At fixed winning coefficients only padding below a support endpoint at the sample minimum is free, since the upper side has positive density. |
| hcauchy | all free | upper | criterion | regraded | Shrinking at a null tight slice is checked before comparing finite candidates with the power and collapse thresholds. |
| hcauchy | all free | upper | failure | accepted | The support-constrained power envelope covers critical and exterior faces, and the upper-cut constant adds the shrinking-support end. |
| hcauchy | all free | upper | case:bound-cardinality | accepted | At fixed winning coefficients only padding below a support endpoint at the sample minimum is free, since the upper side has positive density. |
| hcauchy | all free | upper | case:null-effective-window | regraded | The proposed fixed-parent shrink proves divergence on this sufficient branch, which leaves the general failure classification to the main row. |
| hcauchy | all free | both | criterion | regraded | Shrinking at a null tight slice is checked before comparing finite candidates with the power and collapse thresholds. |
| hcauchy | all free | both | failure | accepted | The support-constrained power envelope covers critical and exterior faces, and the upper-cut constant adds the shrinking-support end. |
| hcauchy | all free | both | case:bound-cardinality | accepted | At fixed winning coefficients only padding below a support endpoint at the sample minimum is free, since the upper side has positive density. |
| hcauchy | mu | lower | criterion | regraded | Positive mass must be checked before the scalar power-face comparison at the held endpoint. |
| hcauchy | mu | lower | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| hcauchy | mu | lower | case:bound-cardinality | accepted | At fixed winning coefficients only padding below a support endpoint at the sample minimum is free, since the upper side has positive density. |
| hcauchy | mu | upper | criterion | regraded | Positive mass must be checked before the scalar power-face comparison at the held endpoint. |
| hcauchy | mu | upper | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| hcauchy | mu | upper | case:bound-cardinality | accepted | At fixed winning coefficients only padding below a support endpoint at the sample minimum is free, since the upper side has positive density. |
| hcauchy | mu | upper | case:null-effective-window | regraded | The proposed fixed-parent shrink proves divergence on this sufficient branch, which leaves the general failure classification to the main row. |
| hcauchy | mu | both | criterion | regraded | Positive mass must be checked before the scalar power-face comparison at the held endpoint. |
| hcauchy | mu | both | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| hcauchy | mu | both | case:bound-cardinality | accepted | At fixed winning coefficients only padding below a support endpoint at the sample minimum is free, since the upper side has positive density. |
| hcauchy | mu+sigma | lower | criterion | regraded | A compatible held law attains on positive retained width, while a null tight slice can give divergence under a free upper cut. |
| hcauchy | mu+sigma | lower | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| hcauchy | mu+sigma | lower | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| hcauchy | mu+sigma | upper | criterion | regraded | A compatible held law attains on positive retained width, while a null tight slice can give divergence under a free upper cut. |
| hcauchy | mu+sigma | upper | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| hcauchy | mu+sigma | upper | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| hcauchy | mu+sigma | upper | case:null-effective-window | regraded | The proposed fixed-parent shrink proves divergence on this sufficient branch, which leaves the general failure classification to the main row. |
| hcauchy | mu+sigma | both | criterion | regraded | A compatible held law attains on positive retained width, while a null tight slice can give divergence under a free upper cut. |
| hcauchy | mu+sigma | both | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| hcauchy | mu+sigma | both | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| hcauchy | sigma | lower | criterion | regraded | The null-tight check precedes the remote-location comparison and includes the upper-cut constant at held scale. |
| hcauchy | sigma | lower | failure | accepted | At held scale the only positive-width ends are remote-location uniform and the upper-cut shrinking support branch. |
| hcauchy | sigma | lower | case:bound-cardinality | accepted | At fixed winning coefficients only padding below a support endpoint at the sample minimum is free, since the upper side has positive density. |
| hcauchy | sigma | upper | criterion | regraded | The null-tight check precedes the remote-location comparison and includes the upper-cut constant at held scale. |
| hcauchy | sigma | upper | failure | accepted | At held scale the only positive-width ends are remote-location uniform and the upper-cut shrinking support branch. |
| hcauchy | sigma | upper | case:bound-cardinality | accepted | At fixed winning coefficients only padding below a support endpoint at the sample minimum is free, since the upper side has positive density. |
| hcauchy | sigma | upper | case:left-ray-unique | accepted | With lower cut minus infinity, the effective support starts at location and every strict move toward the minimum improves the likelihood. |
| hcauchy | sigma | upper | case:null-effective-window | regraded | The proposed fixed-parent shrink proves divergence on this sufficient branch, which leaves the general failure classification to the main row. |
| hcauchy | sigma | both | criterion | regraded | The null-tight check precedes the remote-location comparison and includes the upper-cut constant at held scale. |
| hcauchy | sigma | both | failure | accepted | At held scale the only positive-width ends are remote-location uniform and the upper-cut shrinking support branch. |
| hcauchy | sigma | both | case:bound-cardinality | accepted | At fixed winning coefficients only padding below a support endpoint at the sample minimum is free, since the upper side has positive density. |
| std | all free | lower | case:balanced-endpoints | accepted | The Cauchy angular inequality and Jensen bound every finite Student fit strictly below uniform, which is reachable by a remote free coefficient. |
| std | all free | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | all free | lower | case:divergent-tie | accepted | An admissible fixed df sufficiently close to two makes the collapse exponent strictly negative at the specified sample center. |
| std | all free | upper | case:balanced-endpoints | accepted | The Cauchy angular inequality and Jensen bound every finite Student fit strictly below uniform, which is reachable by a remote free coefficient. |
| std | all free | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | all free | upper | case:divergent-tie | accepted | An admissible fixed df sufficiently close to two makes the collapse exponent strictly negative at the specified sample center. |
| std | all free | both | case:balanced-endpoints | accepted | The Cauchy angular inequality and Jensen bound every finite Student fit strictly below uniform, which is reachable by a remote free coefficient. |
| std | all free | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | all free | both | case:divergent-tie | accepted | An admissible fixed df sufficiently close to two makes the collapse exponent strictly negative at the specified sample center. |
| std | mean | lower | case:balanced-endpoints | accepted | The Cauchy angular inequality and Jensen bound every finite Student fit strictly below uniform, which is reachable by a remote free coefficient. |
| std | mean | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | mean | lower | case:divergent-tie | accepted | An admissible fixed df sufficiently close to two makes the collapse exponent strictly negative at the specified sample center. |
| std | mean | upper | case:balanced-endpoints | accepted | The Cauchy angular inequality and Jensen bound every finite Student fit strictly below uniform, which is reachable by a remote free coefficient. |
| std | mean | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | mean | upper | case:divergent-tie | accepted | An admissible fixed df sufficiently close to two makes the collapse exponent strictly negative at the specified sample center. |
| std | mean | both | case:balanced-endpoints | accepted | The Cauchy angular inequality and Jensen bound every finite Student fit strictly below uniform, which is reachable by a remote free coefficient. |
| std | mean | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | mean | both | case:divergent-tie | accepted | An admissible fixed df sufficiently close to two makes the collapse exponent strictly negative at the specified sample center. |
| std | mean+nu | lower | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| std | mean+nu | lower | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| std | mean+nu | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | mean+nu | upper | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| std | mean+nu | upper | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| std | mean+nu | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | mean+nu | both | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| std | mean+nu | both | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| std | mean+nu | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | mean+nu+sd | lower | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| std | mean+nu+sd | lower | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| std | mean+nu+sd | lower | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| std | mean+nu+sd | upper | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| std | mean+nu+sd | upper | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| std | mean+nu+sd | upper | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| std | mean+nu+sd | both | criterion | regraded | A positive-mass tight window attains at the held law, while compatible shrinkage on a null continuous tight window diverges. |
| std | mean+nu+sd | both | failure | accepted | The held law has no coefficient escape, with zero effective width handled by compatible shrinkage or infeasibility. |
| std | mean+nu+sd | both | uniqueness | regraded | The parent tuple is fixed but the joint cut fiber still needs the zero-removed-mass equality condition. |
| std | mean+sd | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | mean+sd | lower | case:divergent-tie | accepted | Taking df down to two shrinks the effective scale even with sd held, and the strict count inequality makes the power product diverge. |
| std | mean+sd | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | mean+sd | upper | case:divergent-tie | accepted | Taking df down to two shrinks the effective scale even with sd held, and the strict count inequality makes the power product diverge. |
| std | mean+sd | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | mean+sd | both | case:divergent-tie | accepted | Taking df down to two shrinks the effective scale even with sd held, and the strict count inequality makes the power product diverge. |
| std | nu | lower | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| std | nu | lower | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| std | nu | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | nu | upper | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| std | nu | upper | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| std | nu | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | nu | both | criterion | accepted | The applicable power faces give the finite candidate threshold with equality retained, after checking the relevant divergent ties. |
| std | nu | both | failure | accepted | The joint collapse exponent and the rescaled normalization determine divergent and critical heights, and exterior power and broad uniform complete the applicable list. |
| std | nu | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | nu+sd | lower | criterion | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| std | nu+sd | lower | failure | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| std | nu+sd | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | nu+sd | upper | criterion | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| std | nu+sd | upper | failure | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| std | nu+sd | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | nu+sd | both | criterion | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| std | nu+sd | both | failure | accepted | At held positive effective scale, the remote-center density vanishes on rays and converges to uniform on bounded windows. |
| std | nu+sd | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | sd | lower | case:balanced-endpoints | accepted | The Cauchy angular inequality and Jensen bound every finite Student fit strictly below uniform, which is reachable by a remote free coefficient. |
| std | sd | lower | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | sd | lower | case:divergent-tie | accepted | Taking df down to two shrinks the effective scale even with sd held, and the strict count inequality makes the power product diverge. |
| std | sd | upper | case:balanced-endpoints | accepted | The Cauchy angular inequality and Jensen bound every finite Student fit strictly below uniform, which is reachable by a remote free coefficient. |
| std | sd | upper | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | sd | upper | case:divergent-tie | accepted | Taking df down to two shrinks the effective scale even with sd held, and the strict count inequality makes the power product diverge. |
| std | sd | both | case:balanced-endpoints | accepted | The Cauchy angular inequality and Jensen bound every finite Student fit strictly below uniform, which is reachable by a remote free coefficient. |
| std | sd | both | case:bound-cardinality | accepted | Strict positivity on the real line forces each estimated cut to its observed extreme at every attained coefficient vector. |
| std | sd | both | case:divergent-tie | accepted | Taking df down to two shrinks the effective scale even with sd held, and the strict count inequality makes the power product diverge. |
| unif | all free | lower | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | all free | lower | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | all free | lower | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | all free | upper | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | all free | upper | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | all free | upper | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | all free | both | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | all free | both | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | all free | both | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | max | lower | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | max | lower | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | max | lower | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | max | upper | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | max | upper | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | max | upper | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | max | both | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | max | both | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | max | both | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | max+min | lower | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | max+min | lower | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | max+min | lower | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | max+min | upper | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | max+min | upper | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | max+min | upper | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | max+min | both | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | max+min | both | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | max+min | both | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | min | lower | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | min | lower | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | min | lower | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | min | upper | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | min | upper | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | min | upper | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |
| unif | min | both | criterion | regraded | The explicit targets follow by independently tightening each compatible effective end under this mask. |
| unif | min | both | failure | regraded | Shrinkage and incompatibility are the only failures, and a clipping plateau can preserve an attained law along an escaping sequence. |
| unif | min | both | uniqueness | regraded | The clipping equations give the full joint preimages under this mask and retain all zero-mass padding. |

## Proof errors and scope corrections

### Two Laplace witnesses used the wrong truncation geometry

**The proposed right-ray witness is impossible for the upper-estimated and both-estimated rows.** Every sample has a finite maximum, so an estimated upper cut gives a finite tight upper endpoint. For upper-only use the reflected witness, sample `(0,1)` on the tight left ray `(-Inf,1]`, with the lower cut fixed at `-Inf`. At any inverse scale the competitor at zero loses to the right plateau by the factor `(2-exp(-r))^(-2)`. On that plateau `2 log(r)-r` peaks at `r=2`. Every location at or above one therefore maximizes. For both estimated cuts take `(0,0,1)` with the two parent coefficients free. The tight interval is `[0,1]`. At each rate the log likelihood is strictly convex between the endpoints, and the decreasing exponential plateau beats the increasing one by the factor `exp(r)`. The mean `1/3` lies strictly between zero and the uniform mean `1/2`. The decreasing exponential mean varies strictly between those limits, so there is exactly one positive rate that matches `1/3`. All locations at or below zero with that rate maximize globally. This proof specifies distinct exact maximizers through a unique scalar equation whose root existence and global optimality follow analytically. [Proposed upper-only row](2026-11-28-trunc-est2-math-report.md#laplace-all-free-truncateupper-estimated) · [Proposed both-estimated row](2026-11-28-trunc-est2-math-report.md#laplace-all-free-truncateboth-estimated) · [Endpoint argument](2026-11-28-trunc-est2-math-report.md#continuous-samples-with-just-the-two-endpoints-occupied)

**The Laplace failure list omitted escaping winning plateaus.** On the lower-estimated right ray `[0,Inf)`, sample `(0,1)` attains at every location at or below zero with scale `1/2`. Sending the location to minus infinity at that scale preserves the exact same maximizing law. The same example applies with scale held at `1/2`. Reflected and bounded plateaus follow from the same split-normalizer proof. The corrected exact rows distinguish plateaus that have finite representatives from excluded uniform and collapse limits. The uniform rows likewise retain the possibility of parameters moving without bound behind a clipping cut. [Fixed-window Laplace proof](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown) · [Held-scale Laplace proof](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown)

### Null tight windows and endpoints

For any compatible coefficient vector with positive tight mass, the ratio of loose to tight conditional likelihood is the tight-to-loose probability ratio raised to sample size. This proves the joint comparison after taking the supremum, including repeated observations at the cuts, zero-mass padding and coefficients that escape on the tight window. Equality with an excluded face permits a finite witness at that height. If a better point exists, the family-specific escape bounds confine a maximizing subsequence above the face height. For finite positive maximizing heights, the removed conditional probability tends to zero. This transfers the limiting conditional law even when the nominal bounds move. [Joint profiling proof](2026-11-28-trunc-est2-math-report.md#joint-profiling-lemma) · [Likelihood closure lemma](../ml-behaviour.tex#lem:closure)

A null tight event requires a separate feasibility check before that ratio is used. For `(1)` with half-normal or half-Cauchy location and scale held at one, the upper-estimated design with fixed lower cut zero has tight window `[0,1]`, which intersects the support only at one. Relax the upper end to `1+e`. The density at one is at least `1/e` since the kernel peaks there, so the likelihood diverges as `e` decreases to zero. With a fixed upper cut of one and a free lower cut at these same holds, every nominal window still intersects the support only at one and the design is infeasible. When the location is free, sample `(1,1)` with upper cut one also admits the distinct coefficient path `mu=1-e` at scale one, whose half-Cauchy density at one is at least `1/(2e)` for `0<e<=1`. The weak limit along each compatible shrinking path is the point mass at one. The half-law criterion rows now state the mass check. Their separate null-window cases establish sufficient divergence branches and have one-sided grades. [Null effective windows](2026-11-28-trunc-est2-math-report.md#corrections-and-scope) · [Earlier upper-cut correction](2026-11-27-mod-rev4-report.md#upper-cut-half-cauchy)

The continuous kernels in these masks are strictly positive throughout their effective intervals, including the inside limit at a finite included support endpoint. In particular the null half-law event comes from zero length even though the pointwise density is positive. Uniform uses the separate width calculation. Discrete uniform endpoints may coincide, and a compatible Skellam axis or origin atom may leave a single cell with positive probability. Discrete Laplace has a flat rate fiber on a compatible singleton, even at a held center. For a nonsingleton, the held-center mean equation gives a unique rate whenever it has a root. The revised cardinality rows describe those two cases explicitly. [Support conventions](../../design/modifiers.md) · [Uniform end formulas](2026-11-28-trunc-est2-math-report.md#uniform-endpoint-formulas) · [Discrete Laplace distance argument](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

### Count faces and the domain check

The reviewed masks use the same proper coefficient law for every compared window. Skellam includes zero-rate Poisson axes and the origin atom as parent members. Conditioning can remove those members when their event mass vanishes, and the interior approach can still give a rare-axis atom. The positive-count discrete Weibull law on a lower-tight ray uses the ordinary parent probabilities divided by survival at the lower cell. At held `q=1/2`, sample `(1)` on `[1,Inf)` has conditional likelihood `1-2^(1-2^beta)`, which tends to one only at infinite shape. At the same holds on `[0,Inf)` the excluded Bernoulli height is one-half. The small-shape limit on a ray containing zero is defective, with surviving zero mass `1-q`. It can tie the finite flat fit for all-zero data, so the exact held-probability failure rows now include it. The two-cell tie on `[1,2]` is exact because the broad-rate odds at shapes two and three are `5/3` and `19/7`, and the other rate end has odds zero. Each finite root with odds one attains the same saturated likelihood. [Discrete Weibull fixed-window proof](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown) · [Parent attainment theorem](2026-11-25-dweibull-math-report.md#attainment-and-maximizer-sets) · [Skellam axes and Bessel limits](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

The zeta domain reversal and the NB size-zero positive-window extension are outside these keys. Their exact examples explain why the ratio argument requires a common parent law for every compared window. Neither extension was used to infer compatibility in this review. [First estimated-bound review](2026-11-27-mod-rev5-report.md#proof-errors-and-scope-corrections) · [Positive-window route convention](../../design/modifiers.md#availability-and-refusals)

## Rows not joined

All proposal keys have eligible parent masks and eligible modifier keys. All 552 keys are in the reviewed CSV with origin `TASK-468`. The two out-of-geometry Laplace witnesses have exact replacements at the same keys.

## Open by family

| Family | Remaining proof work |
| --- | --- |
| std | For free df, exhaust joint shape, location and effective-scale ends under the tight conditioning, including the df-two equality count and the varying-location Gaussian end. At held df, isolate winning roots and decide critical-to-finite equality and coefficient cardinality outside the stated cases. |
| dnorm | Isolate competitive scalar and joint roots on proper windows with more than two cells and decide equality against the optimized geometric face. General coefficient cardinality is open beyond the stated short-window results. |
| skellam | Isolate competitive proper-window roots and decide exact finite-to-axis or finite-to-geometric equality beyond the two-cell odds comparison. |
| laplace | Reduce the bounded free-rate comparison to a direct data test and characterize all ties across the rate profiles and location plateaus on samples with interior observations. |
| logis | Isolate winning inverse-scale roots and determine when distinct held-scale polynomial roots tie at the largest likelihood outside the flat endpoint case. |
| dweibull | Isolate competitive roots for free shape on larger proper windows and decide exact equality with increment or Pareto faces. The held-shape rate test is exact. |
| hcauchy | Determine winning-root cardinality and simplify the comparisons with critical and exterior power faces across the effective-window branches. |
| hnorm | Determine whether distinct upper-only branch winners can tie globally when the fixed lower cut is strictly below the sample minimum. The lower-tight wedge has a unique coefficient winner whenever it attains. |
| dlaplace | The finite center comparison and scalar rate test give attainment and all maximizing preimages. An implementation would need certified ranking of the different center slices. |
| dunif | The compatibility and clipping formulas settle attainment, singleton inclusion and the full maximizing set for these masks. |
| cauchy | Give direct conditions for the exterior and critical-face comparisons and characterize finite coefficient ties after tightening, where the earlier symmetric wide-window witnesses no longer apply. |
| gumbel | Isolate winning rate roots in both free-scale masks and characterize possible ties between distinct rates. |
| unif | The target-width and clipping formulas settle attainment, divergence and the full maximizing set for these masks. |

## Validation

All computations ran locally through the workstation’s one-slot wrapper. The TASK-468 witness script passed at 70 and 100 decimal digits, and its regenerated CSV matched the source data after normalizing record endings. The TASK-468 regime generator replayed the source report and proposal file. The earlier exact witness replays and the independent arithmetic check passed. Numerical values illustrate the analytic paths and were never used to certify a global maximizer. The join file passed the review validator and the repository modifier-proof validator. No builds ran. [Independent check](2026-11-28-mod-rev11b-data/exact_checks.py) · [Review validator](2026-11-28-mod-rev11b-data/validate.py) · [TASK-468 replay](2026-11-28-trunc-est2-math-data/check.py)

The repository link checker reported 47 inherited broken destinations and exited with its one known beta-prime false positive at line 461, where it reads `P+Q` as a path. It reported no broken destinations in this review.
