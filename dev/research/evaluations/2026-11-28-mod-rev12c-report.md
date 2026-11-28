# Estimated truncation bounds for nine families

TASK-474 reviews TASK-469 at worktree revision `245d4af8`. The proposal contains **506 rows** for the nine assigned families. The brief lists 536, but filtering the checked-in file by the nine family names returns 506 across 123 regimes. The join file contains **506 rows** with 454 accepted, 52 regraded and 0 rejected. There are 120 iff, 361 exact and 25 one-sided claims. Every proposed key has an eligible parent regime. [Proposal](2026-11-28-trunc-est3-math-data/proofs.csv) · [Join file](2026-11-28-mod-rev12c-data/reviewed.csv)

**The collapsed-window entries missed positive nominal widths with zero retained mass.** The correction affects upper-estimated Gompertz and Pareto II and lower-estimated Power. It applies even when every parent coefficient is held. The row-level decisions document the corrected scope.

For every compatible coefficient vector with positive tight mass, closing the window at a sample extreme preserves the sample numerator and weakly decreases the mass. This holds after intersecting with a moving support. Taking suprema shows the comparison is joint and already accounts for an escaping coefficient MLE. All nominal cut ties come from removing zero mass. If the joint supremum is finite and positive, the tight-to-loose mass ratio tends to one on any maximizing sequence, so the total variation distance to the tightened laws tends to zero. A divergent sequence need not satisfy this conclusion. Repeating an extreme changes the powers or moments in the family comparison, but keeps the same permitted bounds. All nine parent domains are common across the compared windows, and the zeta signed-window and size-zero negative-binomial alias exceptions are outside this inventory. These are mathematical results for the joint window problem. The interface stores a canonical cut when supported. [Design](../../design/modifiers.md) · [Known-window proofs](2026-11-27-trunc-shape-math-report.md) · [Known-window review](2026-11-27-mod-rev6-report.md) · [Earlier estimated-window review](2026-11-27-mod-rev5-report.md)

## Row decisions

| Family | Held | Estimated cut | Question | Verdict | Reason |
| --- | --- | --- | --- | --- | --- |
| weibull | all free | lower-estimated | criterion | accepted | The Gumbel kernel on log cuts exhausts both free-coefficient escapes and the finite candidate comparison includes equality. |
| weibull | all free | lower-estimated | failure | accepted | The log-coordinate limits are the integrable signed power tilts and a constant admits a centered atom path. |
| weibull | all free | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| weibull | all free | lower-estimated | case:bound-set | accepted | With compatible data, the Weibull observation is strictly positive and every outward extension on an estimated side adds mass. |
| weibull | all free | upper-estimated | criterion | accepted | The Gumbel kernel on log cuts exhausts both free-coefficient escapes and the finite candidate comparison includes equality. |
| weibull | all free | upper-estimated | failure | accepted | The log-coordinate limits are the integrable signed power tilts and a constant admits a centered atom path. |
| weibull | all free | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| weibull | all free | upper-estimated | case:bound-set | accepted | With compatible data, the Weibull observation is strictly positive and every outward extension on an estimated side adds mass. |
| weibull | all free | both-estimated | criterion | accepted | The Gumbel kernel on log cuts exhausts both free-coefficient escapes and the finite candidate comparison includes equality. |
| weibull | all free | both-estimated | failure | accepted | The log-coordinate limits are the integrable signed power tilts and a constant admits a centered atom path. |
| weibull | all free | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| weibull | all free | both-estimated | case:bound-set | accepted | With compatible data, the Weibull observation is strictly positive and every outward extension on an estimated side adds mass. |
| weibull | scale | lower-estimated | criterion | accepted | Holding the log center leaves only the bounded-log-width uniform face and concentration at the projected log scale. |
| weibull | scale | lower-estimated | failure | accepted | Small shape gives the eligible log-uniform and large shape can diverge at the projected scale. |
| weibull | scale | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| weibull | scale | lower-estimated | case:bound-set | accepted | With compatible data, the Weibull observation is strictly positive and every outward extension on an estimated side adds mass. |
| weibull | scale | upper-estimated | criterion | accepted | Holding the log center leaves only the bounded-log-width uniform face and concentration at the projected log scale. |
| weibull | scale | upper-estimated | failure | accepted | Small shape gives the eligible log-uniform and large shape can diverge at the projected scale. |
| weibull | scale | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| weibull | scale | upper-estimated | case:bound-set | accepted | With compatible data, the Weibull observation is strictly positive and every outward extension on an estimated side adds mass. |
| weibull | scale | both-estimated | criterion | accepted | Holding the log center leaves only the bounded-log-width uniform face and concentration at the projected log scale. |
| weibull | scale | both-estimated | failure | accepted | Small shape gives the eligible log-uniform and large shape can diverge at the projected scale. |
| weibull | scale | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| weibull | scale | both-estimated | case:bound-set | accepted | With compatible data, the Weibull observation is strictly positive and every outward extension on an estimated side adds mass. |
| weibull | scale+shape | lower-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| weibull | scale+shape | lower-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| weibull | scale+shape | lower-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| weibull | scale+shape | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| weibull | scale+shape | upper-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| weibull | scale+shape | upper-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| weibull | scale+shape | upper-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| weibull | scale+shape | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| weibull | scale+shape | both-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| weibull | scale+shape | both-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| weibull | scale+shape | both-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| weibull | scale+shape | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| weibull | shape | lower-estimated | criterion | accepted | The strictly increasing power transform gives the negative exponential score with the zero-rate mean on the transformed window. |
| weibull | shape | lower-estimated | failure | accepted | Infinite rate collapses at the included lower cut and zero rate gives the power carrier on finite transformed width. |
| weibull | shape | lower-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| weibull | shape | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| weibull | shape | upper-estimated | criterion | accepted | The strictly increasing power transform gives the negative exponential score with the zero-rate mean on the transformed window. |
| weibull | shape | upper-estimated | failure | accepted | Infinite rate collapses at the included lower cut and zero rate gives the power carrier on finite transformed width. |
| weibull | shape | upper-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| weibull | shape | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| weibull | shape | both-estimated | criterion | accepted | The strictly increasing power transform gives the negative exponential score with the zero-rate mean on the transformed window. |
| weibull | shape | both-estimated | failure | accepted | Infinite rate collapses at the included lower cut and zero rate gives the power carrier on finite transformed width. |
| weibull | shape | both-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| weibull | shape | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| lomax | all free | lower-estimated | criterion | accepted | The pole lower bound is the effective physical lower cut, with the exponential, power and reciprocal faces exhausted by the joint power lemma. |
| lomax | all free | lower-estimated | failure | accepted | The shifted pole limits give exponential, power and reciprocal faces, and an observed positive lower-cut constant can collapse. |
| lomax | all free | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| lomax | all free | lower-estimated | case:bound-set | accepted | With compatible data, the Lomax observation is strictly positive and every outward extension on an estimated side adds mass. |
| lomax | all free | upper-estimated | criterion | accepted | The pole lower bound is the effective physical lower cut, with the exponential, power and reciprocal faces exhausted by the joint power lemma. |
| lomax | all free | upper-estimated | failure | accepted | The shifted pole limits give exponential, power and reciprocal faces, and an observed positive lower-cut constant can collapse. |
| lomax | all free | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| lomax | all free | upper-estimated | case:bound-set | accepted | With compatible data, the Lomax observation is strictly positive and every outward extension on an estimated side adds mass. |
| lomax | all free | both-estimated | criterion | accepted | The pole lower bound is the effective physical lower cut, with the exponential, power and reciprocal faces exhausted by the joint power lemma. |
| lomax | all free | both-estimated | failure | accepted | The shifted pole limits give exponential, power and reciprocal faces, and an observed positive lower-cut constant can collapse. |
| lomax | all free | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| lomax | all free | both-estimated | case:bound-set | accepted | With compatible data, the Lomax observation is strictly positive and every outward extension on an estimated side adds mass. |
| lomax | kappa | lower-estimated | criterion | accepted | At fixed shape the effective-cut pole has only its open lower edge and its infinite upper edge to compare with the finite roots. |
| lomax | kappa | lower-estimated | failure | accepted | The excluded pole ends give the integrable power or uniform, and zero is excluded as an observation. |
| lomax | kappa | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| lomax | kappa | lower-estimated | case:bound-set | accepted | With compatible data, the Lomax observation is strictly positive and every outward extension on an estimated side adds mass. |
| lomax | kappa | upper-estimated | criterion | accepted | At fixed shape the effective-cut pole has only its open lower edge and its infinite upper edge to compare with the finite roots. |
| lomax | kappa | upper-estimated | failure | accepted | The excluded pole ends give the integrable power or uniform, and zero is excluded as an observation. |
| lomax | kappa | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| lomax | kappa | upper-estimated | case:bound-set | accepted | With compatible data, the Lomax observation is strictly positive and every outward extension on an estimated side adds mass. |
| lomax | kappa | both-estimated | criterion | accepted | At fixed shape the effective-cut pole has only its open lower edge and its infinite upper edge to compare with the finite roots. |
| lomax | kappa | both-estimated | failure | accepted | The excluded pole ends give the integrable power or uniform, and zero is excluded as an observation. |
| lomax | kappa | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| lomax | kappa | both-estimated | case:bound-set | accepted | With compatible data, the Lomax observation is strictly positive and every outward extension on an estimated side adds mass. |
| lomax | kappa+lambda | lower-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| lomax | kappa+lambda | lower-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| lomax | kappa+lambda | lower-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| lomax | kappa+lambda | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| lomax | kappa+lambda | upper-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| lomax | kappa+lambda | upper-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| lomax | kappa+lambda | upper-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| lomax | kappa+lambda | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| lomax | kappa+lambda | both-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| lomax | kappa+lambda | both-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| lomax | kappa+lambda | both-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| lomax | kappa+lambda | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| lomax | lambda | lower-estimated | criterion | accepted | The fixed pole gives a negative exponential tilt of the shifted log excess with strictly monotone mean. |
| lomax | lambda | lower-estimated | failure | accepted | The lower log-cut equality collapses and the integrable zero-rate limit is the shifted reciprocal. |
| lomax | lambda | lower-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| lomax | lambda | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| lomax | lambda | upper-estimated | criterion | accepted | The fixed pole gives a negative exponential tilt of the shifted log excess with strictly monotone mean. |
| lomax | lambda | upper-estimated | failure | accepted | The lower log-cut equality collapses and the integrable zero-rate limit is the shifted reciprocal. |
| lomax | lambda | upper-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| lomax | lambda | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| lomax | lambda | both-estimated | criterion | accepted | The fixed pole gives a negative exponential tilt of the shifted log excess with strictly monotone mean. |
| lomax | lambda | both-estimated | failure | accepted | The lower log-cut equality collapses and the integrable zero-rate limit is the shifted reciprocal. |
| lomax | lambda | both-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| lomax | lambda | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| llogis | all free | lower-estimated | criterion | accepted | The logistic log-slope controls both tails at fixed shape and rules out a large-shape escape at positive height on distinct data. |
| llogis | all free | lower-estimated | failure | accepted | The feasible signed log tilts give the regular faces and constants can concentrate at their log value. |
| llogis | all free | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| llogis | all free | lower-estimated | case:bound-set | accepted | With compatible data, the log-logistic observation is strictly positive and every outward extension on an estimated side adds mass. |
| llogis | all free | upper-estimated | criterion | accepted | The logistic log-slope controls both tails at fixed shape and rules out a large-shape escape at positive height on distinct data. |
| llogis | all free | upper-estimated | failure | accepted | The feasible signed log tilts give the regular faces and constants can concentrate at their log value. |
| llogis | all free | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| llogis | all free | upper-estimated | case:bound-set | accepted | With compatible data, the log-logistic observation is strictly positive and every outward extension on an estimated side adds mass. |
| llogis | all free | both-estimated | criterion | accepted | The logistic log-slope controls both tails at fixed shape and rules out a large-shape escape at positive height on distinct data. |
| llogis | all free | both-estimated | failure | accepted | The feasible signed log tilts give the regular faces and constants can concentrate at their log value. |
| llogis | all free | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| llogis | all free | both-estimated | case:bound-set | accepted | With compatible data, the log-logistic observation is strictly positive and every outward extension on an estimated side adds mass. |
| llogis | scale | lower-estimated | criterion | accepted | The center is fixed in log coordinates, giving the projected-center collapse test and log-uniform only on bounded log width. |
| llogis | scale | lower-estimated | failure | accepted | Small shape gives the eligible log-uniform and large shape diverges for a constant at the projected scale. |
| llogis | scale | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| llogis | scale | lower-estimated | case:bound-set | accepted | With compatible data, the log-logistic observation is strictly positive and every outward extension on an estimated side adds mass. |
| llogis | scale | upper-estimated | criterion | accepted | The center is fixed in log coordinates, giving the projected-center collapse test and log-uniform only on bounded log width. |
| llogis | scale | upper-estimated | failure | accepted | Small shape gives the eligible log-uniform and large shape diverges for a constant at the projected scale. |
| llogis | scale | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| llogis | scale | upper-estimated | case:bound-set | accepted | With compatible data, the log-logistic observation is strictly positive and every outward extension on an estimated side adds mass. |
| llogis | scale | both-estimated | criterion | accepted | The center is fixed in log coordinates, giving the projected-center collapse test and log-uniform only on bounded log width. |
| llogis | scale | both-estimated | failure | accepted | Small shape gives the eligible log-uniform and large shape diverges for a constant at the projected scale. |
| llogis | scale | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| llogis | scale | both-estimated | case:bound-set | accepted | With compatible data, the log-logistic observation is strictly positive and every outward extension on an estimated side adds mass. |
| llogis | scale+shape | lower-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| llogis | scale+shape | lower-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| llogis | scale+shape | lower-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| llogis | scale+shape | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| llogis | scale+shape | upper-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| llogis | scale+shape | upper-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| llogis | scale+shape | upper-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| llogis | scale+shape | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| llogis | scale+shape | both-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| llogis | scale+shape | both-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| llogis | scale+shape | both-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| llogis | scale+shape | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| llogis | shape | lower-estimated | criterion | accepted | The power-transformed kernel has only the two open scale ends with integrable positive likelihood. |
| llogis | shape | lower-estimated | failure | accepted | At finite width the large scale gives uniform in the power coordinate and the zero-scale reciprocal square needs a positive lower cut. |
| llogis | shape | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| llogis | shape | lower-estimated | case:bound-set | accepted | With compatible data, the log-logistic observation is strictly positive and every outward extension on an estimated side adds mass. |
| llogis | shape | upper-estimated | criterion | accepted | The power-transformed kernel has only the two open scale ends with integrable positive likelihood. |
| llogis | shape | upper-estimated | failure | accepted | At finite width the large scale gives uniform in the power coordinate and the zero-scale reciprocal square needs a positive lower cut. |
| llogis | shape | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| llogis | shape | upper-estimated | case:bound-set | accepted | With compatible data, the log-logistic observation is strictly positive and every outward extension on an estimated side adds mass. |
| llogis | shape | both-estimated | criterion | accepted | The power-transformed kernel has only the two open scale ends with integrable positive likelihood. |
| llogis | shape | both-estimated | failure | accepted | At finite width the large scale gives uniform in the power coordinate and the zero-scale reciprocal square needs a positive lower cut. |
| llogis | shape | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| llogis | shape | both-estimated | case:bound-set | accepted | With compatible data, the log-logistic observation is strictly positive and every outward extension on an estimated side adds mass. |
| gompertz | all free | lower-estimated | criterion | accepted | The Gumbel kernel applies on the nonnegative physical window and the integrable signed exponential faces exhaust distinct-data escapes. |
| gompertz | all free | lower-estimated | failure | accepted | The signed exponential faces follow from the two Gumbel coefficients and constants, including zero, have an atom witness. |
| gompertz | all free | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| gompertz | all free | lower-estimated | case:bound-set | accepted | Density is positive from zero onward, so only lower padding at a zero sample minimum can tie. |
| gompertz | all free | upper-estimated | criterion | regraded | The Gumbel kernel applies on the nonnegative physical window and the integrable signed exponential faces exhaust distinct-data escapes, with the comparison explicitly restricted to positive effective tight width. |
| gompertz | all free | upper-estimated | failure | regraded | The signed exponential faces follow from the two Gumbel coefficients and constants, including zero, have an atom witness, with the comparison explicitly restricted to positive effective tight width. |
| gompertz | all free | upper-estimated | case:collapsed-window | regraded | The upper-estimated Gompertz window can retain only zero despite positive nominal width and the row now covers that null intersection. |
| gompertz | all free | upper-estimated | case:bound-set | accepted | Density is positive from zero onward, so only lower padding at a zero sample minimum can tie. |
| gompertz | all free | both-estimated | criterion | accepted | The Gumbel kernel applies on the nonnegative physical window and the integrable signed exponential faces exhaust distinct-data escapes. |
| gompertz | all free | both-estimated | failure | accepted | The signed exponential faces follow from the two Gumbel coefficients and constants, including zero, have an atom witness. |
| gompertz | all free | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| gompertz | all free | both-estimated | case:bound-set | accepted | Density is positive from zero onward, so only lower padding at a zero sample minimum can tie. |
| gompertz | a | lower-estimated | criterion | accepted | The small-shape face has rate equal to the held a and the all-zero formula settles the finite-window and ray cases. |
| gompertz | a | lower-estimated | failure | accepted | The rate-a face and positive lower-cut collapse follow from the slope and the all-zero density is decreasing on finite width and flat on the ray. |
| gompertz | a | lower-estimated | uniqueness | regraded | The all-zero density is exactly the held a on the full ray, giving an exact witness with distinct b values and a lower-bound plateau. |
| gompertz | a | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| gompertz | a | upper-estimated | criterion | regraded | The small-shape face has rate equal to the held a and the all-zero formula settles the finite-window and ray cases, with the comparison explicitly restricted to positive effective tight width. |
| gompertz | a | upper-estimated | failure | regraded | The rate-a face and positive lower-cut collapse follow from the slope and the all-zero density is decreasing on finite width and flat on the ray, with the comparison explicitly restricted to positive effective tight width. |
| gompertz | a | upper-estimated | case:collapsed-window | regraded | The upper-estimated Gompertz window can retain only zero despite positive nominal width and the row now covers that null intersection. |
| gompertz | a | upper-estimated | case:bound-set | accepted | Density is positive from zero onward, so only lower padding at a zero sample minimum can tie. |
| gompertz | a | both-estimated | criterion | accepted | The small-shape face has rate equal to the held a and the all-zero formula settles the finite-window and ray cases. |
| gompertz | a | both-estimated | failure | accepted | The rate-a face and positive lower-cut collapse follow from the slope and the all-zero density is decreasing on finite width and flat on the ray. |
| gompertz | a | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| gompertz | a | both-estimated | case:bound-set | accepted | Density is positive from zero onward, so only lower padding at a zero sample minimum can tie. |
| gompertz | a+b | lower-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| gompertz | a+b | lower-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| gompertz | a+b | lower-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| gompertz | a+b | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| gompertz | a+b | upper-estimated | criterion | regraded | Evaluation at the specified vector requires a compatible sample and positive retained mass, with the comparison explicitly restricted to positive effective tight width. |
| gompertz | a+b | upper-estimated | failure | regraded | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit, with the comparison explicitly restricted to positive effective tight width. |
| gompertz | a+b | upper-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| gompertz | a+b | upper-estimated | case:collapsed-window | regraded | The upper-estimated Gompertz window can retain only zero despite positive nominal width and the row now covers that null intersection. |
| gompertz | a+b | both-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| gompertz | a+b | both-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| gompertz | a+b | both-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| gompertz | a+b | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| gompertz | b | lower-estimated | criterion | accepted | The transform exp(bX) for held b gives the strictly monotone negative exponential score with the transformed endpoint means. |
| gompertz | b | lower-estimated | failure | accepted | Infinite rate collapses at the lower cut and the eligible zero-rate carrier has slope b in physical space. |
| gompertz | b | lower-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| gompertz | b | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| gompertz | b | upper-estimated | criterion | regraded | The transform exp(bX) for held b gives the strictly monotone negative exponential score with the transformed endpoint means, with the comparison explicitly restricted to positive effective tight width. |
| gompertz | b | upper-estimated | failure | regraded | Infinite rate collapses at the lower cut and the eligible zero-rate carrier has slope b in physical space, with the comparison explicitly restricted to positive effective tight width. |
| gompertz | b | upper-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| gompertz | b | upper-estimated | case:collapsed-window | regraded | The upper-estimated Gompertz window can retain only zero despite positive nominal width and the row now covers that null intersection. |
| gompertz | b | both-estimated | criterion | accepted | The transform exp(bX) for held b gives the strictly monotone negative exponential score with the transformed endpoint means. |
| gompertz | b | both-estimated | failure | accepted | Infinite rate collapses at the lower cut and the eligible zero-rate carrier has slope b in physical space. |
| gompertz | b | both-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| gompertz | b | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| frechet | all free | lower-estimated | case:half-at-lower-cut | accepted | The strict pairwise CDF-increment inequality is sharp at shape one as the gap to the location and scaled inverse excess go to zero. |
| frechet | all free | lower-estimated | criterion | accepted | Shape one proves the majority obstruction and the sharp half-at-cut bound applies only at a lower contact. |
| frechet | all free | lower-estimated | failure | accepted | The minimum-contact exponent at shape one gives the stated divergence witness. |
| frechet | all free | lower-estimated | case:critical-contact | accepted | At exact half, substituting shape one cancels the gap exponent and the limiting denominator is the correct one for contact or for a lower cut below the minimum. |
| frechet | all free | lower-estimated | case:gumbel | accepted | The scaling of log excess yields both density and CDF convergence on the tight window. |
| frechet | all free | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| frechet | all free | lower-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | all free | upper-estimated | case:half-at-lower-cut | accepted | The strict pairwise CDF-increment inequality is sharp at shape one as the gap to the location and scaled inverse excess go to zero. |
| frechet | all free | upper-estimated | criterion | accepted | Shape one proves the majority obstruction and the sharp half-at-cut bound applies only at a lower contact, and the profiled shape-one fiber settles the equal-count ray pair. |
| frechet | all free | upper-estimated | failure | accepted | The minimum-contact exponent at shape one gives the stated divergence witness. |
| frechet | all free | upper-estimated | case:critical-contact | accepted | At exact half, substituting shape one cancels the gap exponent and the limiting denominator is the correct one for contact or for a lower cut below the minimum. |
| frechet | all free | upper-estimated | case:gumbel | accepted | The scaling of log excess yields both density and CDF convergence on the tight window. |
| frechet | all free | upper-estimated | case:upper-ray-pair | accepted | Profiling the inverse-power rate and applying the hyperbolic-sine inequality resolves equality and the entire shape-one fiber. |
| frechet | all free | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| frechet | all free | upper-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | all free | both-estimated | case:half-at-lower-cut | accepted | The strict pairwise CDF-increment inequality is sharp at shape one as the gap to the location and scaled inverse excess go to zero. |
| frechet | all free | both-estimated | criterion | accepted | Shape one proves the majority obstruction and the sharp half-at-cut bound applies only at a lower contact. |
| frechet | all free | both-estimated | failure | accepted | The minimum-contact exponent at shape one gives the stated divergence witness. |
| frechet | all free | both-estimated | case:critical-contact | accepted | At exact half, substituting shape one cancels the gap exponent and the limiting denominator is the correct one for contact or for a lower cut below the minimum. |
| frechet | all free | both-estimated | case:gumbel | accepted | The scaling of log excess yields both density and CDF convergence on the tight window. |
| frechet | all free | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| frechet | all free | both-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | lambda | lower-estimated | criterion | accepted | The contact exponent and far-location limits exhaust finite-lower-cut escapes, with the exact rate profile supplying the upper ray. |
| frechet | lambda | lower-estimated | failure | accepted | Only the shifted power, eligible reflected exponential and critical contact have competitive finite excluded height. |
| frechet | lambda | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda | lower-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | lambda | upper-estimated | criterion | accepted | The contact exponent and far-location limits exhaust finite-lower-cut escapes, with the exact rate profile supplying the upper ray, using the new inverse-power rate profile on the physical upper ray. |
| frechet | lambda | upper-estimated | failure | accepted | Only the shifted power, eligible reflected exponential and critical contact have competitive finite excluded height, using the new inverse-power rate profile on the physical upper ray. |
| frechet | lambda | upper-estimated | case:upper-ray-pair | accepted | Profiling the inverse-power rate and applying the hyperbolic-sine inequality resolves equality and the entire shape-one fiber. |
| frechet | lambda | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda | upper-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | lambda | both-estimated | criterion | accepted | The contact exponent and far-location limits exhaust finite-lower-cut escapes, with the exact rate profile supplying the upper ray. |
| frechet | lambda | both-estimated | failure | accepted | Only the shifted power, eligible reflected exponential and critical contact have competitive finite excluded height. |
| frechet | lambda | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda | both-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | lambda+mu | lower-estimated | criterion | accepted | The inverse-power excess gives a negative exponential tilt on the reversed effective cuts. |
| frechet | lambda+mu | lower-estimated | failure | accepted | The zero-rate carrier is eligible at finite transformed width and infinite rate collapses at the upper physical cut. |
| frechet | lambda+mu | lower-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| frechet | lambda+mu | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda+mu | upper-estimated | criterion | accepted | The inverse-power excess gives a negative exponential tilt on the reversed effective cuts. |
| frechet | lambda+mu | upper-estimated | failure | accepted | The zero-rate carrier is eligible at finite transformed width and infinite rate collapses at the upper physical cut. |
| frechet | lambda+mu | upper-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| frechet | lambda+mu | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda+mu | both-estimated | criterion | accepted | The inverse-power excess gives a negative exponential tilt on the reversed effective cuts. |
| frechet | lambda+mu | both-estimated | failure | accepted | The zero-rate carrier is eligible at finite transformed width and infinite rate collapses at the upper physical cut. |
| frechet | lambda+mu | both-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| frechet | lambda+mu | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda+mu+sigma | lower-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| frechet | lambda+mu+sigma | lower-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| frechet | lambda+mu+sigma | lower-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| frechet | lambda+mu+sigma | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda+mu+sigma | upper-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| frechet | lambda+mu+sigma | upper-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| frechet | lambda+mu+sigma | upper-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| frechet | lambda+mu+sigma | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda+mu+sigma | both-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| frechet | lambda+mu+sigma | both-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| frechet | lambda+mu+sigma | both-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| frechet | lambda+mu+sigma | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda+sigma | lower-estimated | criterion | accepted | Fixed shape and scale leave only the possible finite-width uniform and upper-width collapse. |
| frechet | lambda+sigma | lower-estimated | failure | accepted | Far-left location gives a uniform only on bounded width and sample contact gives positive height only on the upper-width collapse branch. |
| frechet | lambda+sigma | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda+sigma | lower-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | lambda+sigma | upper-estimated | criterion | accepted | Fixed shape and scale leave only the possible finite-width uniform and upper-width collapse. |
| frechet | lambda+sigma | upper-estimated | failure | accepted | Far-left location gives a uniform only on bounded width and sample contact gives positive height only on the upper-width collapse branch. |
| frechet | lambda+sigma | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda+sigma | upper-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | lambda+sigma | both-estimated | criterion | accepted | Fixed shape and scale leave only the possible finite-width uniform and upper-width collapse. |
| frechet | lambda+sigma | both-estimated | failure | accepted | Far-left location gives a uniform only on bounded width and sample contact gives positive height only on the upper-width collapse branch. |
| frechet | lambda+sigma | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| frechet | lambda+sigma | both-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | mu | lower-estimated | criterion | accepted | Reversed log excess preserves the Gumbel comparison, with both signed tilt branches checked for integrability. |
| frechet | mu | lower-estimated | failure | accepted | The reversed Gumbel escape analysis gives signed excess powers and free centering gives constant collapse. |
| frechet | mu | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| frechet | mu | lower-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | mu | upper-estimated | criterion | accepted | Reversed log excess preserves the Gumbel comparison, with both signed tilt branches checked for integrability. |
| frechet | mu | upper-estimated | failure | accepted | The reversed Gumbel escape analysis gives signed excess powers and free centering gives constant collapse. |
| frechet | mu | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| frechet | mu | upper-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | mu | both-estimated | criterion | accepted | Reversed log excess preserves the Gumbel comparison, with both signed tilt branches checked for integrability. |
| frechet | mu | both-estimated | failure | accepted | The reversed Gumbel escape analysis gives signed excess powers and free centering gives constant collapse. |
| frechet | mu | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| frechet | mu | both-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | mu+sigma | lower-estimated | criterion | accepted | The fixed Gumbel center in reversed log excess gives the projected-center test and the bounded-log-width face. |
| frechet | mu+sigma | lower-estimated | failure | accepted | Only log-uniform can win at finite excluded height and a constant at the projected excess scale collapses. |
| frechet | mu+sigma | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| frechet | mu+sigma | lower-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | mu+sigma | upper-estimated | criterion | accepted | The fixed Gumbel center in reversed log excess gives the projected-center test and the bounded-log-width face. |
| frechet | mu+sigma | upper-estimated | failure | accepted | Only log-uniform can win at finite excluded height and a constant at the projected excess scale collapses. |
| frechet | mu+sigma | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| frechet | mu+sigma | upper-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | mu+sigma | both-estimated | criterion | accepted | The fixed Gumbel center in reversed log excess gives the projected-center test and the bounded-log-width face. |
| frechet | mu+sigma | both-estimated | failure | accepted | Only log-uniform can win at finite excluded height and a constant at the projected excess scale collapses. |
| frechet | mu+sigma | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| frechet | mu+sigma | both-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | sigma | lower-estimated | criterion | accepted | The held-scale estimates cover finite-location contact, large shape and remote location, including vanishing height on the physical upper ray. |
| frechet | sigma | lower-estimated | failure | accepted | Remote location can yield a nonincreasing exponential on the eligible windows and constants admit concentration. |
| frechet | sigma | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| frechet | sigma | lower-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | sigma | upper-estimated | criterion | accepted | The held-scale estimates cover finite-location contact, large shape and remote location, including vanishing height on the physical upper ray. |
| frechet | sigma | upper-estimated | failure | accepted | Remote location can yield a nonincreasing exponential on the eligible windows and constants admit concentration. |
| frechet | sigma | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| frechet | sigma | upper-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| frechet | sigma | both-estimated | criterion | accepted | The held-scale estimates cover finite-location contact, large shape and remote location, including vanishing height on the physical upper ray. |
| frechet | sigma | both-estimated | failure | accepted | Remote location can yield a nonincreasing exponential on the eligible windows and constants admit concentration. |
| frechet | sigma | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| frechet | sigma | both-estimated | case:bound-set | accepted | Finite positive likelihood requires strict excess above the location, so every estimated cut is uniquely tight. |
| invexp | all free | lower-estimated | criterion | accepted | The decreasing reciprocal transform uses the upper physical cut as its lower cut and gives the strictly monotone rate score. |
| invexp | all free | lower-estimated | failure | accepted | Infinite rate collapses at the included upper physical cut and the eligible zero-rate carrier is the inverse square. |
| invexp | all free | lower-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| invexp | all free | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| invexp | all free | upper-estimated | criterion | accepted | The decreasing reciprocal transform uses the upper physical cut as its lower cut and gives the strictly monotone rate score. |
| invexp | all free | upper-estimated | failure | accepted | Infinite rate collapses at the included upper physical cut and the eligible zero-rate carrier is the inverse square. |
| invexp | all free | upper-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| invexp | all free | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| invexp | all free | both-estimated | criterion | accepted | The decreasing reciprocal transform uses the upper physical cut as its lower cut and gives the strictly monotone rate score. |
| invexp | all free | both-estimated | failure | accepted | Infinite rate collapses at the included upper physical cut and the eligible zero-rate carrier is the inverse square. |
| invexp | all free | both-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| invexp | all free | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| invexp | scale | lower-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| invexp | scale | lower-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| invexp | scale | lower-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| invexp | scale | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| invexp | scale | upper-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| invexp | scale | upper-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| invexp | scale | upper-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| invexp | scale | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| invexp | scale | both-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| invexp | scale | both-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| invexp | scale | both-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| invexp | scale | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| power | all free | lower-estimated | criterion | regraded | Kernel mass is strictly increasing in the effective upper cut and at its minimum the log tilt has the two stated score thresholds, with the comparison explicitly restricted to positive effective tight width. |
| power | all free | lower-estimated | failure | regraded | The profiled support can shrink and the open shape ends give the log-uniform or the upper atom, with the comparison explicitly restricted to positive effective tight width. |
| power | all free | lower-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | all free | lower-estimated | case:collapsed-window | regraded | At upper support contact the lower-estimated Power window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| power | all free | upper-estimated | criterion | accepted | Kernel mass is strictly increasing in the effective upper cut and at its minimum the log tilt has the two stated score thresholds. |
| power | all free | upper-estimated | failure | accepted | The profiled support can shrink and the open shape ends give the log-uniform or the upper atom. |
| power | all free | upper-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | all free | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| power | all free | both-estimated | criterion | accepted | Kernel mass is strictly increasing in the effective upper cut and at its minimum the log tilt has the two stated score thresholds. |
| power | all free | both-estimated | failure | accepted | The profiled support can shrink and the open shape ends give the log-uniform or the upper atom. |
| power | all free | both-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | all free | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| power | alpha | lower-estimated | criterion | regraded | Fixed support leaves a positive log tilt on the effective interval with strict score thresholds, with the comparison explicitly restricted to positive effective tight width. |
| power | alpha | lower-estimated | failure | regraded | Zero shape gives the eligible log-uniform and infinite shape collapses at the included upper endpoint, with the comparison explicitly restricted to positive effective tight width. |
| power | alpha | lower-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | alpha | lower-estimated | case:collapsed-window | regraded | At upper support contact the lower-estimated Power window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| power | alpha | upper-estimated | criterion | accepted | Fixed support leaves a positive log tilt on the effective interval with strict score thresholds. |
| power | alpha | upper-estimated | failure | accepted | Zero shape gives the eligible log-uniform and infinite shape collapses at the included upper endpoint. |
| power | alpha | upper-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | alpha | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| power | alpha | both-estimated | criterion | accepted | Fixed support leaves a positive log tilt on the effective interval with strict score thresholds. |
| power | alpha | both-estimated | failure | accepted | Zero shape gives the eligible log-uniform and infinite shape collapses at the included upper endpoint. |
| power | alpha | both-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | alpha | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| power | alpha+beta | lower-estimated | criterion | regraded | Evaluation at the specified vector requires a compatible sample and positive retained mass, with the comparison explicitly restricted to positive effective tight width. |
| power | alpha+beta | lower-estimated | failure | regraded | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit, with the comparison explicitly restricted to positive effective tight width. |
| power | alpha+beta | lower-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | alpha+beta | lower-estimated | case:collapsed-window | regraded | At upper support contact the lower-estimated Power window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| power | alpha+beta | upper-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| power | alpha+beta | upper-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| power | alpha+beta | upper-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | alpha+beta | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| power | alpha+beta | both-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| power | alpha+beta | both-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| power | alpha+beta | both-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | alpha+beta | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| power | beta | lower-estimated | criterion | regraded | With shape held, minimizing the effective upper end gives a positive-width optimum exactly when h exceeds the effective lower cut, with the comparison explicitly restricted to positive effective tight width. |
| power | beta | lower-estimated | failure | regraded | If the effective upper end can shrink to the constant there is unbounded conditional likelihood at fixed shape, with the comparison explicitly restricted to positive effective tight width. |
| power | beta | lower-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | beta | lower-estimated | case:collapsed-window | regraded | At upper support contact the lower-estimated Power window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| power | beta | upper-estimated | criterion | accepted | With shape held, minimizing the effective upper end gives a positive-width optimum exactly when h exceeds the effective lower cut. |
| power | beta | upper-estimated | failure | accepted | If the effective upper end can shrink to the constant there is unbounded conditional likelihood at fixed shape. |
| power | beta | upper-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | beta | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| power | beta | both-estimated | criterion | accepted | With shape held, minimizing the effective upper end gives a positive-width optimum exactly when h exceeds the effective lower cut. |
| power | beta | both-estimated | failure | accepted | If the effective upper end can shrink to the constant there is unbounded conditional likelihood at fixed shape. |
| power | beta | both-estimated | uniqueness | accepted | The log tilt identifies a free shape and the monotone width profile gives the full inverse image of the optimal effective cut. |
| power | beta | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| kumar | all free | lower-estimated | criterion | accepted | The negative log transform preserves the endpoint observations and the joint shape analysis exhausts the log-power and reciprocal faces including their corner. |
| kumar | all free | lower-estimated | failure | accepted | The two shape-zero faces and their integrable corner follow from the kernel and the large-shape estimates leave constant collapse. |
| kumar | all free | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| kumar | all free | lower-estimated | case:bound-set | accepted | With compatible data, the Kumaraswamy observation must be strictly between zero and one and every outward extension on an estimated side adds mass. |
| kumar | all free | upper-estimated | criterion | accepted | The negative log transform preserves the endpoint observations and the joint shape analysis exhausts the log-power and reciprocal faces including their corner. |
| kumar | all free | upper-estimated | failure | accepted | The two shape-zero faces and their integrable corner follow from the kernel and the large-shape estimates leave constant collapse. |
| kumar | all free | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| kumar | all free | upper-estimated | case:bound-set | accepted | With compatible data, the Kumaraswamy observation must be strictly between zero and one and every outward extension on an estimated side adds mass. |
| kumar | all free | both-estimated | criterion | accepted | The negative log transform preserves the endpoint observations and the joint shape analysis exhausts the log-power and reciprocal faces including their corner. |
| kumar | all free | both-estimated | failure | accepted | The two shape-zero faces and their integrable corner follow from the kernel and the large-shape estimates leave constant collapse. |
| kumar | all free | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| kumar | all free | both-estimated | case:bound-set | accepted | With compatible data, the Kumaraswamy observation must be strictly between zero and one and every outward extension on an estimated side adds mass. |
| kumar | a | lower-estimated | criterion | accepted | The decreasing log transform leaves a positive exponential tilt with a strictly increasing mean. |
| kumar | a | lower-estimated | failure | accepted | The excluded zero-b carrier is reciprocal and the infinite tilt collapses at the lower physical cut. |
| kumar | a | lower-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| kumar | a | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| kumar | a | upper-estimated | criterion | accepted | The decreasing log transform leaves a positive exponential tilt with a strictly increasing mean. |
| kumar | a | upper-estimated | failure | accepted | The excluded zero-b carrier is reciprocal and the infinite tilt collapses at the lower physical cut. |
| kumar | a | upper-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| kumar | a | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| kumar | a | both-estimated | criterion | accepted | The decreasing log transform leaves a positive exponential tilt with a strictly increasing mean. |
| kumar | a | both-estimated | failure | accepted | The excluded zero-b carrier is reciprocal and the infinite tilt collapses at the lower physical cut. |
| kumar | a | both-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| kumar | a | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| kumar | b | lower-estimated | criterion | accepted | The one free shape has the log-power edge and upper-cut concentration, with vanishing height for nonconstant data at infinite shape. |
| kumar | b | lower-estimated | failure | accepted | The integrable small-a face is a power in negative log data and large a can diverge only for upper-cut constants. |
| kumar | b | lower-estimated | uniqueness | accepted | At held b=1 the transformed exponential score identifies the sole finite root on the stated subcase. |
| kumar | b | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| kumar | b | upper-estimated | criterion | accepted | The one free shape has the log-power edge and upper-cut concentration, with vanishing height for nonconstant data at infinite shape. |
| kumar | b | upper-estimated | failure | accepted | The integrable small-a face is a power in negative log data and large a can diverge only for upper-cut constants. |
| kumar | b | upper-estimated | uniqueness | accepted | At held b=1 the transformed exponential score identifies the sole finite root on the stated subcase. |
| kumar | b | upper-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on right and the joint singleton test checks that side for positive carrier and density. |
| kumar | b | both-estimated | criterion | accepted | The one free shape has the log-power edge and upper-cut concentration, with vanishing height for nonconstant data at infinite shape. |
| kumar | b | both-estimated | failure | accepted | The integrable small-a face is a power in negative log data and large a can diverge only for upper-cut constants. |
| kumar | b | both-estimated | uniqueness | accepted | At held b=1 the transformed exponential score identifies the sole finite root on the stated subcase. |
| kumar | b | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | all free | lower-estimated | criterion | accepted | Profiling the fixed pole places the threshold at the minimum where a sufficiently small shape gives divergence, with width collapse if the upper cut equals the minimum. |
| pareto2 | all free | lower-estimated | failure | accepted | The small-scale lower atom and shrinking-width constructions are witnesses, and the ray permits escaping mass. |
| pareto2 | all free | lower-estimated | uniqueness | accepted | Every compatible sample permits divergence, so the attained set is empty. |
| pareto2 | all free | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | all free | upper-estimated | criterion | regraded | Profiling the fixed pole places the threshold at the minimum where a sufficiently small shape gives divergence, with width collapse if the upper cut equals the minimum, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | all free | upper-estimated | failure | regraded | The small-scale lower atom and shrinking-width constructions are witnesses, and the ray permits escaping mass, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | all free | upper-estimated | uniqueness | accepted | Every compatible sample permits divergence, so the attained set is empty. |
| pareto2 | all free | upper-estimated | case:collapsed-window | regraded | At threshold contact the upper-estimated Pareto II window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| pareto2 | all free | both-estimated | criterion | accepted | Profiling the fixed pole places the threshold at the minimum where a sufficiently small shape gives divergence, with width collapse if the upper cut equals the minimum. |
| pareto2 | all free | both-estimated | failure | accepted | The small-scale lower atom and shrinking-width constructions are witnesses, and the ray permits escaping mass. |
| pareto2 | all free | both-estimated | uniqueness | accepted | Every compatible sample permits divergence, so the attained set is empty. |
| pareto2 | all free | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | min | lower-estimated | criterion | accepted | The effective cut shifts the lower pole bound, with the Lomax comparison away from threshold divergence and critical ties included. |
| pareto2 | min | lower-estimated | failure | accepted | The shifted power, reciprocal and exponential comparison applies away from divergence, and a ray spike can send mass to infinity. |
| pareto2 | min | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | min | lower-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | min | upper-estimated | criterion | regraded | The effective cut shifts the lower pole bound, with the Lomax comparison away from threshold divergence and critical ties included, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | min | upper-estimated | failure | regraded | The shifted power, reciprocal and exponential comparison applies away from divergence, and a ray spike can send mass to infinity, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | min | upper-estimated | case:collapsed-window | regraded | At threshold contact the upper-estimated Pareto II window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| pareto2 | min | upper-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | min | both-estimated | criterion | accepted | The effective cut shifts the lower pole bound, with the Lomax comparison away from threshold divergence and critical ties included. |
| pareto2 | min | both-estimated | failure | accepted | The shifted power, reciprocal and exponential comparison applies away from divergence, and a ray spike can send mass to infinity. |
| pareto2 | min | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | min | both-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | min+scale | lower-estimated | criterion | accepted | The fixed effective lower cut and pole give a one-to-one negative tilt in log excess with explicit endpoint means. |
| pareto2 | min+scale | lower-estimated | failure | accepted | Lower equality collapses and the finite-width zero-shape limit is the translated reciprocal. |
| pareto2 | min+scale | lower-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| pareto2 | min+scale | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | min+scale | upper-estimated | criterion | regraded | The fixed effective lower cut and pole give a one-to-one negative tilt in log excess with explicit endpoint means, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | min+scale | upper-estimated | failure | regraded | Lower equality collapses and the finite-width zero-shape limit is the translated reciprocal, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | min+scale | upper-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| pareto2 | min+scale | upper-estimated | case:collapsed-window | regraded | At threshold contact the upper-estimated Pareto II window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| pareto2 | min+scale | both-estimated | criterion | accepted | The fixed effective lower cut and pole give a one-to-one negative tilt in log excess with explicit endpoint means. |
| pareto2 | min+scale | both-estimated | failure | accepted | Lower equality collapses and the finite-width zero-shape limit is the translated reciprocal. |
| pareto2 | min+scale | both-estimated | uniqueness | accepted | Strict score monotonicity identifies the fitted scalar and equality of window mass gives the entire nominal cut set. |
| pareto2 | min+scale | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | min+scale+shape | lower-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| pareto2 | min+scale+shape | lower-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| pareto2 | min+scale+shape | lower-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| pareto2 | min+scale+shape | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | min+scale+shape | upper-estimated | criterion | regraded | Evaluation at the specified vector requires a compatible sample and positive retained mass, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | min+scale+shape | upper-estimated | failure | regraded | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | min+scale+shape | upper-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| pareto2 | min+scale+shape | upper-estimated | case:collapsed-window | regraded | At threshold contact the upper-estimated Pareto II window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| pareto2 | min+scale+shape | both-estimated | criterion | accepted | Evaluation at the specified vector requires a compatible sample and positive retained mass. |
| pareto2 | min+scale+shape | both-estimated | failure | accepted | For fixed coefficients on the positive-width evaluation branch, only invalidity or incompatibility can prevent a positive fit. |
| pareto2 | min+scale+shape | both-estimated | uniqueness | accepted | The coefficient vector is specified and the support-intersection equality test gives all nominal bound ties. |
| pareto2 | min+scale+shape | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | min+shape | lower-estimated | criterion | accepted | The open pole endpoint, critical contact and finite-width uniform exhaust the held-shape comparison. |
| pareto2 | min+shape | lower-estimated | failure | accepted | The contact exponent distinguishes collapse, the finite envelope and vanishing, with the other pole ends included. |
| pareto2 | min+shape | lower-estimated | case:lower-tight-ray | accepted | The retained pole score strictly decreases on distinct samples and the held threshold or held scale determines the excluded or included endpoint. |
| pareto2 | min+shape | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | min+shape | lower-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | min+shape | upper-estimated | criterion | regraded | The open pole endpoint, critical contact and finite-width uniform exhaust the held-shape comparison, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | min+shape | upper-estimated | failure | regraded | The contact exponent distinguishes collapse, the finite envelope and vanishing, with the other pole ends included, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | min+shape | upper-estimated | case:collapsed-window | regraded | At threshold contact the upper-estimated Pareto II window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| pareto2 | min+shape | upper-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | min+shape | both-estimated | criterion | accepted | The open pole endpoint, critical contact and finite-width uniform exhaust the held-shape comparison. |
| pareto2 | min+shape | both-estimated | failure | accepted | The contact exponent distinguishes collapse, the finite envelope and vanishing, with the other pole ends included. |
| pareto2 | min+shape | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | min+shape | both-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | scale | lower-estimated | criterion | accepted | The monotone threshold branch reduces to the minimum and the exterior branch has the reciprocal and exponential limits with the join retained. |
| pareto2 | scale | lower-estimated | failure | accepted | The held-scale bound rules out a small pole and leaves the reciprocal and exponential faces and the stated constant collapses. |
| pareto2 | scale | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | scale | lower-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | scale | upper-estimated | criterion | regraded | The monotone threshold branch reduces to the minimum and the exterior branch has the reciprocal and exponential limits with the join retained, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | scale | upper-estimated | failure | regraded | The held-scale bound rules out a small pole and leaves the reciprocal and exponential faces and the stated constant collapses, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | scale | upper-estimated | case:collapsed-window | regraded | At threshold contact the upper-estimated Pareto II window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| pareto2 | scale | upper-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | scale | both-estimated | criterion | accepted | The monotone threshold branch reduces to the minimum and the exterior branch has the reciprocal and exponential limits with the join retained. |
| pareto2 | scale | both-estimated | failure | accepted | The held-scale bound rules out a small pole and leaves the reciprocal and exponential faces and the stated constant collapses. |
| pareto2 | scale | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | scale | both-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | scale+shape | lower-estimated | criterion | accepted | The threshold comparison includes the minimum, the join and exterior score roots, with uniform eligible at finite width. |
| pareto2 | scale+shape | lower-estimated | failure | accepted | Far-left threshold yields uniform only at finite width and approaching the upper cut on constants shrinks the retained support. |
| pareto2 | scale+shape | lower-estimated | case:lower-tight-ray | accepted | The retained pole score strictly decreases on distinct samples and the held threshold or held scale determines the excluded or included endpoint. |
| pareto2 | scale+shape | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | scale+shape | lower-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | scale+shape | upper-estimated | criterion | regraded | The threshold comparison includes the minimum, the join and exterior score roots, with uniform eligible at finite width, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | scale+shape | upper-estimated | failure | regraded | Far-left threshold yields uniform only at finite width and approaching the upper cut on constants shrinks the retained support, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | scale+shape | upper-estimated | case:collapsed-window | regraded | At threshold contact the upper-estimated Pareto II window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| pareto2 | scale+shape | upper-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | scale+shape | both-estimated | criterion | accepted | The threshold comparison includes the minimum, the join and exterior score roots, with uniform eligible at finite width. |
| pareto2 | scale+shape | both-estimated | failure | accepted | Far-left threshold yields uniform only at finite width and approaching the upper cut on constants shrinks the retained support. |
| pareto2 | scale+shape | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | scale+shape | both-estimated | case:bound-set | accepted | The Pareto II kernel is positive from its closed threshold, so only lower padding at threshold contact can tie. |
| pareto2 | shape | lower-estimated | criterion | accepted | At fixed pole, increasing the threshold to the minimum removes mass, leaving the uniform and exact contact heights to compare. |
| pareto2 | shape | lower-estimated | failure | accepted | The pole ends give the uniform or critical contact envelope and negative exponent or vanishing width gives divergence. |
| pareto2 | shape | lower-estimated | uniqueness | regraded | The rational trial beats both pole-limit heights, so a finite winning pole has distinct exactly equivalent threshold-scale pairs. |
| pareto2 | shape | lower-estimated | case:lower-tight-ray | accepted | The retained pole score strictly decreases on distinct samples and the held threshold or held scale determines the excluded or included endpoint. |
| pareto2 | shape | lower-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on left and the joint singleton test checks that side for positive carrier and density. |
| pareto2 | shape | upper-estimated | criterion | regraded | At fixed pole, increasing the threshold to the minimum removes mass, leaving the uniform and exact contact heights to compare, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | shape | upper-estimated | failure | regraded | The pole ends give the uniform or critical contact envelope and negative exponent or vanishing width gives divergence, with the comparison explicitly restricted to positive effective tight width. |
| pareto2 | shape | upper-estimated | uniqueness | regraded | The rational trial beats both pole-limit heights, so a finite winning pole has distinct exactly equivalent threshold-scale pairs. |
| pareto2 | shape | upper-estimated | case:collapsed-window | regraded | At threshold contact the upper-estimated Pareto II window can retain only the sample point despite positive nominal width and the row now covers that null intersection. |
| pareto2 | shape | both-estimated | criterion | accepted | At fixed pole, increasing the threshold to the minimum removes mass, leaving the uniform and exact contact heights to compare. |
| pareto2 | shape | both-estimated | failure | accepted | The pole ends give the uniform or critical contact envelope and negative exponent or vanishing width gives divergence. |
| pareto2 | shape | both-estimated | uniqueness | regraded | The rational trial beats both pole-limit heights, so a finite winning pole has distinct exactly equivalent threshold-scale pairs. |
| pareto2 | shape | both-estimated | case:collapsed-window | accepted | The fixed-cut design permits opening on either and the joint singleton test checks that side for positive carrier and density. |

## Proof errors and scope corrections

### Positive nominal width with zero retained mass

The proposed `case:collapsed-window` rows cover nominal singletons. Its per-regime instructions therefore miss an entire branch of the source's joint profiling argument. In held-a Gompertz the regime prose observes that an estimated upper cut at zero with a fixed lower cut below zero has zero mass. The positive-mass looser windows settle the joint problem in the opposite direction from treating the tight event as an invalid design. Here are exact counterexamples to extending the positive-width comparisons to nominal width alone.

- **Gompertz.** Take sample `(0)`, hold `a=b=1` and use `[-1,NA]`. The tight window is nominally `[-1,0]` and retains just zero. On `[-1,ε]` the conditional density at zero is `1/(1−exp(−(exp(ε)−1)))`, asymptotic to `1/ε`.
- **Pareto II.** Take sample `(1)`, hold `min=scale=shape=1` and use `[0,NA]`. The tight window is `[0,1]` and retains just the threshold. On `[0,1+ε]` the density at 1 is `(1+ε)/ε`.
- **Power.** Take sample `(1)`, hold `alpha=beta=1` and use `[NA,2]`. The nominal tight window `[1,2]` retains just 1. On `[1−ε,2]` the law is uniform on `[1−ε,1]` with density `1/ε` at 1. With free alpha the same witness is available at alpha equal to the observation.

Each example gives infinite joint supremum and an atom at the observation as the weak limit. For any multiplicity, raise the one-observation likelihood to that power. The general argument fixes the coefficients, uses the positive finite inside endpoint density and lets the effective length shrink along the permitted side. A zero-density contact cannot use that argument. For example Fréchet data exactly at a held location have zero density even when a looser event has mass. The open endpoints of Weibull, Lomax, log-logistic, inverse exponential, Kumaraswamy and Power at zero also supply no positive observation density. Conversely, the constant at a held Power upper endpoint cannot expand rightward from a coincident fixed lower cut there. A Pareto II constant at a held threshold or Gompertz at zero cannot expand leftward from a coincident fixed upper cut. [Joint profiling proof](2026-11-28-trunc-est3-math-report.md#joint-profiling-and-boundary-conventions) · [Earlier zero-width correction](2026-11-28-trunc-est2-math-report.md#joint-profiling-lemma)

### Exact comparisons and refutations

- **Fréchet upper ray.** With shape held and minimum gap from the location (d>0), the transformed inverse-power excess above its value at the upper cut is exponential. Its fitted rate is (n/H(d)), where (H) sums those excesses. The two profile ends give the negative, zero or positive contact exponent at (d=0) and the reflected exponential at infinity. For two equally frequent values, removing the common exponential factor at shape one gives the exact squared reciprocal-gap product. The hyperbolic-sine inequality is strict at every shape above one. At shape one and sample `(−1,0)`, the location-scale pairs `(−2,4)` and `(−3,12)` follow directly from the rate equation. With the lower cut tight, the strict CDF increment bound gives a different supremum. [Upper-ray profile](2026-11-28-trunc-est3-math-report.md#fréchet-upper-ray) · [Lower contact](2026-11-27-trunc-shape-math-report.md#frechet-free-truncateknown)
- **Pareto II held shape.** With shape two, two copies of 1 and one each of 11/10 and 2, the lower-tight pole likelihood tends to zero at zero scale and to one at infinity. At pole distance one, dividing the kernel by its integral (3/8) gives the exact product ((8/3)^4(10/11)^3/8>1). A finite winning pole must therefore exist. Every admissible threshold and scale at that pole has the same retained kernel, giving distinct exact maximizers for each of the three designs. The revised uniqueness claims record this as a one-sided refutation. With shape and scale both free, the separate ray path with threshold one, sample `(1,2)`, and shape equal to scale to the power (1/4) has likelihood asymptotic to scale to the power (-1/2) even as mass leaves every bounded interval. The failure claims retain their one-sided scope. [Pole profile](2026-11-28-trunc-est3-math-report.md#power-and-moving-endpoints) · [Exact finite-width witness](2026-11-28-trunc-est3-math-report.md#particular-skew-family-comparisons)
- **Gompertz held a.** The sample `(0)` and lower-estimated cut with fixed upper infinity give density exactly (a) for every positive (b) and every lower cut at or below zero. On a finite positive-width window from zero, the denominator increases strictly in (b), so the excluded exponential wins. The ray example is a one-sided refutation. [Held-a proof](2026-11-27-trunc-shape-math-report.md#gompertz-a-truncateknown)
- **Power.** The endpoint density is evaluated from inside. At positive effective width a smaller upper endpoint strictly decreases mass, and a fitted shape is unique by the positive log tilt. For upper estimation the entire joint plateau has `min(alpha, upper cut)=h` with both arguments at least (h). Equality of the empirical log mean with the zero-shape mean belongs to the excluded log-uniform face. [Power profile](2026-11-28-trunc-est3-math-report.md#power-and-moving-endpoints)

## Rows not joined

All proposed rows for the nine named families are represented in the join file. The 30-row difference from the brief has no corresponding keys in the checked-in proposal.

## Open by family

| Family | Joined | Remaining |
| --- | ---: | --- |
| frechet | 107 | For free location and shape, minimum-minority data and exact half away from a lower-tight window still need an exhaustive joint escape comparison beyond the upper-ray pair. Finite-winner cardinality and effective face equality are open for the nonscalar masks outside the explicit fibers. |
| pareto2 | 99 | Bounded-window winning-pole cardinality at held shape, general finite-winner cardinality with other nonscalar holds, effective face equality, and the full set of divergent-ray weak limits with free shape and scale remain open. |
| weibull | 48 | General finite-winner cardinality and effective equality with a signed log tilt or log-uniform face remain open with shape free. |
| lomax | 48 | General finite-winner cardinality and effective equality against the shifted exponential, reciprocal or power faces remain open for free rate. |
| llogis | 48 | General finite-winner cardinality and effective equality with the eligible power faces remain open for the comparison masks. |
| gompertz | 48 | Outside the all-zero held-a ray, general finite-winner cardinality and effective equality on the comparison masks remain open. |
| power | 48 | Attainment, the failure branches and the full finite maximizing set are covered in all proposed masks under the stated parent-law convention. |
| kumar | 36 | General finite-winner cardinality beyond the scalar cases and effective comparison with the two shape-zero faces remain open. |
| invexp | 24 | The scalar criterion, the failure branches and the full finite maximizing set are covered in both proposed masks under the stated parent-law convention. |

## Checks

The workstation single-slot replay of TASK-469's checker passed all 64 rational or 80-digit checks and reproduced the saved CSV byte for byte. The 80-digit comparisons are spot checks. The load-bearing inequalities and maximizing sets above use analytic arguments and exact arithmetic. The source report verifier checked its 447 headings and local links. The join validator checks the schema, proposal membership, parent masks, modifier eligibility, evidence anchors and duplicate keys. No builds were requested.

The atlas candidate check passed. The repository link checker reported 47 inherited destinations and one additional destination, the documented `P+Q` false positive at line 461 in the beta-prime report. It reported no destination from this review. The Batteries false positive did not appear in this checkout. [Beta-prime report](2026-11-25-betapr-math-report.md)
