# Independent review of estimated truncation bounds

TASK-455 reviewed 270 TASK-448 rows for 30 family and held-mask pairs across 16 families. The submission contains 252 accepted rows and 18 regraded rows, with 88 `iff`, 176 `exact`, and six `one-sided` claims, all `written`. The material proof gap is the comparison between zeta windows with different admissible exponent domains. [Reviewed CSV](2026-11-27-mod-rev5-data/reviewed.csv) · [TASK-448 report](2026-11-27-trunc-est-math-report.md)

I matched every proposal to its parent key and its same-question reviewed known-window entry, then evaluated the known-window result on the effective closed window with the free cuts substituted. For Rayleigh and zeta I revised the wording that will travel in the atlas. The Rayleigh and fixed-lower zeta rows retain their proposed directions. The table records verdicts per key. [Base atlas](../existence-atlas/atlas.csv) · [Reviewed known-window rows](../existence-atlas/modifier-proofs.csv) · [Atlas grading](../existence-atlas/README.md) · [Modifier design](../../design/modifiers.md)

## Row decisions

| Family | Held | Estimated cut | Question | Verdict | Reason |
| --- | --- | --- | --- | --- | --- |
| beta | all free | lower | criterion | accepted | On the window cut at the lower edge of the observed range, testing the zero-shape edges and their corner at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| beta | all free | lower | failure | accepted | The single-peaked log-statistic directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| beta | all free | lower | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| beta | all free | upper | criterion | accepted | On the window cut at the upper edge of the observed range, testing the zero-shape edges and their corner at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| beta | all free | upper | failure | accepted | The single-peaked log-statistic directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| beta | all free | upper | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| beta | all free | both | criterion | accepted | On the window cut at both observed extremes, testing the zero-shape edges and their corner at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| beta | all free | both | failure | accepted | The single-peaked log-statistic directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| beta | all free | both | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| beta | shape1 | lower | criterion | accepted | Tilting log(1−X) gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| beta | shape1 | lower | failure | accepted | The eligible face is the zero second shape when the cut excludes one, with infinite-tilt collapse only at the lower observed extreme on positive width. |
| beta | shape1 | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| beta | shape1 | upper | criterion | accepted | Tilting log(1−X) gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| beta | shape1 | upper | failure | accepted | The eligible face is the zero second shape when the cut excludes one, with infinite-tilt collapse only at the fixed effective lower cut on positive width. |
| beta | shape1 | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| beta | shape1 | both | criterion | accepted | Tilting log(1−X) gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| beta | shape1 | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| beta | shape1 | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| beta | shape2 | lower | criterion | accepted | Tilting log X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| beta | shape2 | lower | failure | accepted | The eligible face is the zero first shape when the cut excludes zero, with infinite-tilt collapse only at the fixed effective upper cut on positive width. |
| beta | shape2 | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| beta | shape2 | upper | criterion | accepted | Tilting log X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| beta | shape2 | upper | failure | accepted | The eligible face is the zero first shape when the cut excludes zero, with infinite-tilt collapse only at the upper observed extreme on positive width. |
| beta | shape2 | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| beta | shape2 | both | criterion | accepted | Tilting log X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| beta | shape2 | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| beta | shape2 | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| binom | all free | lower | criterion | accepted | The tight-window matched profiles converge to their Poisson face, and a finite equality attains while constants admit a unit mass. |
| binom | all free | lower | failure | accepted | For nonconstant data all finite sizes have a proper interior optimum, so strict Poisson victory requires size diverging along the matched-mean scaling. |
| binom | all free | lower | uniqueness | accepted | Discrete finite-prefix or flat-tail comparisons give the complete size set, with saturated Bernoulli and constant-data branches explicitly described. |
| binom | all free | upper | criterion | accepted | The tight-window matched profiles converge to their Poisson face, and a finite equality attains while constants admit a unit mass. |
| binom | all free | upper | failure | accepted | For nonconstant data all finite sizes have a proper interior optimum, so strict Poisson victory requires size diverging along the matched-mean scaling. |
| binom | all free | upper | uniqueness | accepted | Discrete finite-prefix or flat-tail comparisons give the complete size set, with saturated Bernoulli and constant-data branches explicitly described. |
| binom | all free | both | criterion | accepted | The tight-window matched profiles converge to their Poisson face, and a finite equality attains while constants admit a unit mass. |
| binom | all free | both | failure | accepted | For nonconstant data all finite sizes have a proper interior optimum, so strict Poisson victory requires size diverging along the matched-mean scaling. |
| binom | all free | both | uniqueness | accepted | Discrete finite-prefix or flat-tail comparisons give the complete size set, with saturated Bernoulli and constant-data branches explicitly described. |
| binom | prob | lower | criterion | accepted | The lower cut lets size equal the constant count, while nonconstants have a finite interior-probability prefix and endpoints use atom compatibility. |
| binom | prob | lower | failure | accepted | Every compatible interior hold has a finite winner, while incompatible endpoint holds have no positive likelihood. |
| binom | prob | lower | uniqueness | accepted | Successive weight ratios bound the finite candidate prefix outside the explicit singleton and atom branches, and the mass rule supplies all nominal cuts. |
| binom | prob | upper | criterion | accepted | At interior probability, cap-at-count fits occur only when the retained window is a singleton for constant top data, while endpoint holds use atom compatibility. |
| binom | prob | upper | failure | accepted | At interior probability the upper-tight nonsingleton has an excluded top atom exactly for constant data, while incompatible endpoint holds have zero likelihood. |
| binom | prob | upper | uniqueness | accepted | Successive weight ratios bound the finite candidate prefix outside the explicit singleton and atom branches, and the mass rule supplies all nominal cuts. |
| binom | prob | both | criterion | accepted | Constants are singletons at every size at least the count for interior probability, and endpoint holds require a compatible atom. |
| binom | prob | both | failure | accepted | Every compatible interior hold has a finite winner, while incompatible endpoint holds have no positive likelihood. |
| binom | prob | both | uniqueness | accepted | Successive weight ratios bound the finite candidate prefix outside the explicit singleton and atom branches, and the mass rule supplies all nominal cuts. |
| binom | size | lower | criterion | accepted | After capping by held size, a singleton, strictly interior count mean or admitted end atom gives exactly the claimed attainment. |
| binom | size | lower | failure | accepted | On a positive-width count carrier, only a constant at the tightened lower cut can select an excluded atom. |
| binom | size | lower | uniqueness | accepted | The identifiable odds tilt on a nonsingleton and the singleton and alias sets exhaust all coefficient and window pairs. |
| binom | size | upper | criterion | accepted | After capping by held size, a singleton, strictly interior count mean or admitted end atom gives exactly the claimed attainment. |
| binom | size | upper | failure | accepted | The upper-tight constant selects the top atom, which is excluded precisely when the top count is below the held size. |
| binom | size | upper | uniqueness | accepted | The identifiable odds tilt on a nonsingleton and the singleton and alias sets exhaust all coefficient and window pairs. |
| binom | size | both | criterion | accepted | After capping by held size, a singleton, strictly interior count mean or admitted end atom gives exactly the claimed attainment. |
| binom | size | both | failure | accepted | Both cuts tight leave either a compatible singleton or a strictly interior mean. |
| binom | size | both | uniqueness | accepted | The identifiable odds tilt on a nonsingleton and the singleton and alias sets exhaust all coefficient and window pairs. |
| exp | all free | lower | criterion | accepted | Tilting X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| exp | all free | lower | failure | accepted | The eligible face is the zero-rate uniform on finite windows, with infinite-tilt collapse only at the lower observed extreme on positive width. |
| exp | all free | lower | uniqueness | accepted | The rate is identifiable and the only estimated-cut plateau can come from including padding below zero. |
| exp | all free | upper | criterion | accepted | Tilting X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| exp | all free | upper | failure | accepted | The eligible face is the zero-rate uniform on finite windows, with infinite-tilt collapse only at the fixed effective lower cut on positive width. |
| exp | all free | upper | uniqueness | accepted | The rate is identifiable and the only estimated-cut plateau can come from including padding below zero. |
| exp | all free | both | criterion | accepted | Tilting X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| exp | all free | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| exp | all free | both | uniqueness | accepted | The rate is identifiable and the only estimated-cut plateau can come from including padding below zero. |
| gamma | all free | lower | criterion | accepted | On the window cut at the lower edge of the observed range, testing the zero-shape and zero-rate edges and their corner at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| gamma | all free | lower | failure | accepted | The single-peaked log-linear directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| gamma | all free | lower | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| gamma | all free | upper | criterion | accepted | On the window cut at the upper edge of the observed range, testing the zero-shape and zero-rate edges and their corner at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| gamma | all free | upper | failure | accepted | The single-peaked log-linear directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| gamma | all free | upper | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| gamma | all free | both | criterion | accepted | On the window cut at both observed extremes, testing the zero-shape and zero-rate edges and their corner at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| gamma | all free | both | failure | accepted | The single-peaked log-linear directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| gamma | all free | both | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| gamma | scale | lower | criterion | accepted | Tilting log X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| gamma | scale | lower | failure | accepted | The eligible face is the zero-shape carrier when the lower cut is positive, with infinite-tilt collapse only at the fixed effective upper cut on positive width. |
| gamma | scale | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| gamma | scale | upper | criterion | accepted | Tilting log X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| gamma | scale | upper | failure | accepted | The eligible face is the zero-shape carrier when the lower cut is positive, with infinite-tilt collapse only at the upper observed extreme on positive width. |
| gamma | scale | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| gamma | scale | both | criterion | accepted | Tilting log X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| gamma | scale | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| gamma | scale | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| gamma | shape | lower | criterion | accepted | Tilting X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| gamma | shape | lower | failure | accepted | The eligible face is the zero-rate held-shape power law on finite windows, with infinite-tilt collapse only at the lower observed extreme on positive width. |
| gamma | shape | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| gamma | shape | upper | criterion | accepted | Tilting X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| gamma | shape | upper | failure | accepted | The eligible face is the zero-rate held-shape power law on finite windows, with infinite-tilt collapse only at the fixed effective lower cut on positive width. |
| gamma | shape | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| gamma | shape | both | criterion | accepted | Tilting X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| gamma | shape | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| gamma | shape | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| geom | all free | lower | criterion | accepted | The interior geometric tilt ends at the finite discrete-uniform mean or infinity, with singleton and admitted lower-atom exceptions. |
| geom | all free | lower | failure | accepted | At the tightened lower count, an incompatible ordinary atom is excluded, and the finite uniform is the opposite endpoint where eligible. |
| geom | all free | lower | uniqueness | accepted | Strict mean matching gives at most one root on a nonsingleton, and the compatible singleton, alias and zero-mass cut rules give the complete joint set. |
| geom | all free | upper | criterion | accepted | The interior geometric tilt ends at the finite discrete-uniform mean or infinity, with singleton and admitted lower-atom exceptions. |
| geom | all free | upper | failure | accepted | The uniform is the only excluded face on a compatible nonsingleton with a tightened upper cut. |
| geom | all free | upper | uniqueness | accepted | Strict mean matching gives at most one root on a nonsingleton, and the compatible singleton, alias and zero-mass cut rules give the complete joint set. |
| geom | all free | both | criterion | accepted | The interior geometric tilt ends at the finite discrete-uniform mean or infinity, with singleton and admitted lower-atom exceptions. |
| geom | all free | both | failure | accepted | The uniform is the only excluded face on a compatible nonsingleton with a tightened upper cut. |
| geom | all free | both | uniqueness | accepted | Strict mean matching gives at most one root on a nonsingleton, and the compatible singleton, alias and zero-mass cut rules give the complete joint set. |
| invgamma | alpha | lower | criterion | accepted | Tilting 1/X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invgamma | alpha | lower | failure | accepted | The eligible face is the zero-rate power law on finite reciprocal windows, with infinite-tilt collapse only at the fixed effective upper cut on positive width. |
| invgamma | alpha | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invgamma | alpha | upper | criterion | accepted | Tilting 1/X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invgamma | alpha | upper | failure | accepted | The eligible face is the zero-rate power law on finite reciprocal windows, with infinite-tilt collapse only at the upper observed extreme on positive width. |
| invgamma | alpha | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invgamma | alpha | both | criterion | accepted | Tilting 1/X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invgamma | alpha | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| invgamma | alpha | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invgauss | all free | lower | criterion | accepted | On the window cut at the lower edge of the observed range, testing the Lévy and exponential-power edges and their corner at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| invgauss | all free | lower | failure | accepted | The single-peaked reciprocal-linear directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| invgauss | all free | lower | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| invgauss | all free | upper | criterion | accepted | On the window cut at the upper edge of the observed range, testing the Lévy and exponential-power edges and their corner at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| invgauss | all free | upper | failure | accepted | The single-peaked reciprocal-linear directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| invgauss | all free | upper | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| invgauss | all free | both | criterion | accepted | On the window cut at both observed extremes, testing the Lévy and exponential-power edges and their corner at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| invgauss | all free | both | failure | accepted | The single-peaked reciprocal-linear directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| invgauss | all free | both | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| invgauss | mean | lower | criterion | accepted | Tilting X/mean² + 1/X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invgauss | mean | lower | failure | accepted | The eligible face is the zero-shape carrier with finite mean statistic when both support ends are cut, with infinite-tilt collapse only at the projection of the held centre on positive width. |
| invgauss | mean | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invgauss | mean | upper | criterion | accepted | Tilting X/mean² + 1/X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invgauss | mean | upper | failure | accepted | The eligible face is the zero-shape carrier with finite mean statistic when both support ends are cut, with infinite-tilt collapse only at the projection of the held centre on positive width. |
| invgauss | mean | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invgauss | mean | both | criterion | accepted | Tilting X/mean² + 1/X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invgauss | mean | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| invgauss | mean | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invgauss | shape | lower | criterion | accepted | Tilting X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invgauss | shape | lower | failure | accepted | The eligible face is the zero-u Lévy law when the upper cut is finite, with infinite-tilt collapse only at the lower observed extreme on positive width. |
| invgauss | shape | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invgauss | shape | upper | criterion | accepted | Tilting X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invgauss | shape | upper | failure | accepted | The eligible face is the zero-u Lévy law when the upper cut is finite, with infinite-tilt collapse only at the fixed effective lower cut on positive width. |
| invgauss | shape | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invgauss | shape | both | criterion | accepted | Tilting X gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invgauss | shape | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| invgauss | shape | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invweibull | shape | lower | criterion | accepted | Tilting X to the negative held shape gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invweibull | shape | lower | failure | accepted | The eligible face is the zero-rate inverse-power law on finite transformed windows, with infinite-tilt collapse only at the fixed effective upper cut on positive width. |
| invweibull | shape | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invweibull | shape | upper | criterion | accepted | Tilting X to the negative held shape gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invweibull | shape | upper | failure | accepted | The eligible face is the zero-rate inverse-power law on finite transformed windows, with infinite-tilt collapse only at the upper observed extreme on positive width. |
| invweibull | shape | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| invweibull | shape | both | criterion | accepted | Tilting X to the negative held shape gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| invweibull | shape | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| invweibull | shape | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| lnorm | all free | lower | criterion | accepted | On the window cut at the lower edge of the observed range, testing the power face matched in log coordinates at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| lnorm | all free | lower | failure | accepted | The single-peaked quadratic directions in log coordinates leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| lnorm | all free | lower | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| lnorm | all free | upper | criterion | accepted | On the window cut at the upper edge of the observed range, testing the power face matched in log coordinates at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| lnorm | all free | upper | failure | accepted | The single-peaked quadratic directions in log coordinates leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| lnorm | all free | upper | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| lnorm | all free | both | criterion | accepted | On the window cut at both observed extremes, testing the power face matched in log coordinates at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| lnorm | all free | both | failure | accepted | The single-peaked quadratic directions in log coordinates leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| lnorm | all free | both | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| lnorm | meanlog | lower | criterion | accepted | Tilting squared log distance from meanlog gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| lnorm | meanlog | lower | failure | accepted | The eligible face is the zero quadratic tilt giving log-uniform on finite log windows, with infinite-tilt collapse only at the projection of the held centre on positive width. |
| lnorm | meanlog | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| lnorm | meanlog | upper | criterion | accepted | Tilting squared log distance from meanlog gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| lnorm | meanlog | upper | failure | accepted | The eligible face is the zero quadratic tilt giving log-uniform on finite log windows, with infinite-tilt collapse only at the projection of the held centre on positive width. |
| lnorm | meanlog | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| lnorm | meanlog | both | criterion | accepted | Tilting squared log distance from meanlog gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| lnorm | meanlog | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| lnorm | meanlog | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| lnorm | sdlog | lower | criterion | accepted | The full location tilt matches the mean of log X strictly inside the profiled interval, which requires nonconstant data. |
| lnorm | sdlog | lower | failure | accepted | A constant sample at the estimated cut is a zero-carrier support face and the location tilt collapses there. |
| lnorm | sdlog | lower | uniqueness | accepted | The full scalar location tilt is identifiable and positive carrier density makes every estimated cut strict. |
| lnorm | sdlog | upper | criterion | accepted | The full location tilt matches the mean of log X strictly inside the profiled interval, which requires nonconstant data. |
| lnorm | sdlog | upper | failure | accepted | A constant sample at the estimated cut is a zero-carrier support face and the location tilt collapses there. |
| lnorm | sdlog | upper | uniqueness | accepted | The full scalar location tilt is identifiable and positive carrier density makes every estimated cut strict. |
| lnorm | sdlog | both | criterion | accepted | The full location tilt matches the mean of log X strictly inside the profiled interval, which requires nonconstant data. |
| lnorm | sdlog | both | failure | accepted | A constant sample makes the two estimated cuts coincide and narrowing the window gives divergence. |
| lnorm | sdlog | both | uniqueness | accepted | The full scalar location tilt is identifiable and positive carrier density makes every estimated cut strict. |
| nbinom | size | lower | criterion | accepted | The positive-size tilt ends at the finite weighted mean or infinity, while size zero requires a zero atom or feasible logarithmic alias. |
| nbinom | size | lower | failure | accepted | At the tightened lower count, an incompatible ordinary atom is excluded, and the finite rising-factorial carrier is the opposite endpoint where eligible. |
| nbinom | size | lower | uniqueness | accepted | Strict mean matching gives at most one root on a nonsingleton, and the compatible singleton, alias and zero-mass cut rules give the complete joint set. |
| nbinom | size | upper | criterion | accepted | The positive-size tilt ends at the finite weighted mean or infinity, while size zero requires a zero atom or feasible logarithmic alias. |
| nbinom | size | upper | failure | accepted | The rising-factorial carrier is the only excluded face on a compatible nonsingleton with a tightened upper cut. |
| nbinom | size | upper | uniqueness | accepted | Strict mean matching gives at most one root on a nonsingleton, and the compatible singleton, alias and zero-mass cut rules give the complete joint set. |
| nbinom | size | both | criterion | accepted | The positive-size tilt ends at the finite weighted mean or infinity, while size zero requires a zero atom or feasible logarithmic alias. |
| nbinom | size | both | failure | accepted | The rising-factorial carrier is the only excluded face on a compatible nonsingleton with a tightened upper cut. |
| nbinom | size | both | uniqueness | accepted | Strict mean matching gives at most one root on a nonsingleton, and the compatible singleton, alias and zero-mass cut rules give the complete joint set. |
| norm | all free | lower | criterion | accepted | On the window cut at the lower edge of the observed range, testing the linear-tilt face matched at the empirical mean at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| norm | all free | lower | failure | accepted | The single-peaked quadratic directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| norm | all free | lower | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| norm | all free | upper | criterion | accepted | On the window cut at the upper edge of the observed range, testing the linear-tilt face matched at the empirical mean at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| norm | all free | upper | failure | accepted | The single-peaked quadratic directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| norm | all free | upper | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| norm | all free | both | criterion | accepted | On the window cut at both observed extremes, testing the linear-tilt face matched at the empirical mean at the integrable ends exhausts the closure by coercivity on nonconstant data. |
| norm | all free | both | failure | accepted | The single-peaked quadratic directions leave no escape at infinity for distinct observations, and constants give the stated concentration. |
| norm | all free | both | uniqueness | accepted | Strict concavity identifies the natural coefficient vector, and every extension past an estimated cut adds positive mass. |
| norm | mean | lower | criterion | accepted | Tilting squared distance from the held mean gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| norm | mean | lower | failure | accepted | The eligible face is the zero quadratic tilt giving uniform on finite windows, with infinite-tilt collapse only at the projection of the held centre on positive width. |
| norm | mean | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| norm | mean | upper | criterion | accepted | Tilting squared distance from the held mean gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| norm | mean | upper | failure | accepted | The eligible face is the zero quadratic tilt giving uniform on finite windows, with infinite-tilt collapse only at the projection of the held centre on positive width. |
| norm | mean | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| norm | mean | both | criterion | accepted | Tilting squared distance from the held mean gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| norm | mean | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| norm | mean | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| norm | sd | lower | criterion | accepted | The full location tilt matches the mean of X strictly inside the profiled interval, which requires nonconstant data. |
| norm | sd | lower | failure | accepted | A constant sample at the estimated cut is a zero-carrier support face and the location tilt collapses there. |
| norm | sd | lower | uniqueness | accepted | The full scalar location tilt is identifiable and positive carrier density makes every estimated cut strict. |
| norm | sd | upper | criterion | accepted | The full location tilt matches the mean of X strictly inside the profiled interval, which requires nonconstant data. |
| norm | sd | upper | failure | accepted | A constant sample at the estimated cut is a zero-carrier support face and the location tilt collapses there. |
| norm | sd | upper | uniqueness | accepted | The full scalar location tilt is identifiable and positive carrier density makes every estimated cut strict. |
| norm | sd | both | criterion | accepted | The full location tilt matches the mean of X strictly inside the profiled interval, which requires nonconstant data. |
| norm | sd | both | failure | accepted | A constant sample makes the two estimated cuts coincide and narrowing the window gives divergence. |
| norm | sd | both | uniqueness | accepted | The full scalar location tilt is identifiable and positive carrier density makes every estimated cut strict. |
| pareto | all free | lower | criterion | accepted | Profiling the effective lower cut to the minimum leaves the strict scalar log-mean test. |
| pareto | all free | lower | failure | accepted | The only positive-width scalar failures are lower collapse or the excluded log-uniform, and effective width can shrink at a coincident sample. |
| pareto | all free | lower | uniqueness | accepted | Either the nominal lower cut or the free parent endpoint can supply the observed minimum. |
| pareto | all free | upper | criterion | accepted | Profiling the effective lower cut to the minimum leaves the strict scalar log-mean test. |
| pareto | all free | upper | failure | accepted | The only positive-width scalar failures are lower collapse or the excluded log-uniform, and effective width can shrink at a coincident sample. |
| pareto | all free | upper | uniqueness | accepted | With a fixed lower cut below the minimum, only the free parent endpoint can supply the optimal effective cut. |
| pareto | all free | both | criterion | accepted | Profiling the effective lower cut to the minimum leaves the strict scalar log-mean test. |
| pareto | all free | both | failure | accepted | The only positive-width scalar failures are lower collapse or the excluded log-uniform, and effective width can shrink at a coincident sample. |
| pareto | all free | both | uniqueness | accepted | The two lower-cut coordinates can trade off while the estimated upper cut is pinned at the maximum. |
| pareto | a | lower | criterion | accepted | With shape held, profiling to the minimum gives a finite maximum exactly on positive effective width. |
| pareto | a | lower | failure | accepted | The held shape has no coefficient escape except endpoint or cut shrinkage when the optimal effective width is zero. |
| pareto | a | lower | uniqueness | accepted | Either the nominal lower cut or the free parent endpoint can supply the observed minimum. |
| pareto | a | upper | criterion | accepted | With shape held, profiling to the minimum gives a finite maximum exactly on positive effective width. |
| pareto | a | upper | failure | accepted | The held shape has no coefficient escape except endpoint or cut shrinkage when the optimal effective width is zero. |
| pareto | a | upper | uniqueness | accepted | With a fixed lower cut below the minimum, only the free parent endpoint can supply the optimal effective cut. |
| pareto | a | both | criterion | accepted | With shape held, profiling to the minimum gives a finite maximum exactly on positive effective width. |
| pareto | a | both | failure | accepted | The held shape has no coefficient escape except endpoint or cut shrinkage when the optimal effective width is zero. |
| pareto | a | both | uniqueness | accepted | The two lower-cut coordinates can trade off while the estimated upper cut is pinned at the maximum. |
| pareto | b | lower | criterion | accepted | The exponential mean test in log coordinates uses the held endpoint and the resolved nominal window. |
| pareto | b | lower | failure | accepted | The scalar log tilt has just the log-uniform and lower point-mass limits on positive width, with zero width handled by feasibility. |
| pareto | b | lower | uniqueness | accepted | The fitted shape and effective cuts are unique, and a free lower nominal cut is flat exactly when the held support end supplies the minimum. |
| pareto | b | upper | criterion | accepted | The exponential mean test in log coordinates uses the held endpoint and the resolved nominal window. |
| pareto | b | upper | failure | accepted | The scalar log tilt has just the log-uniform and lower point-mass limits on positive width, with zero width handled by feasibility. |
| pareto | b | upper | uniqueness | accepted | With the parent endpoint and nominal lower cut fixed, strict normalization and the scalar score identify the upper cut and shape. |
| pareto | b | both | criterion | accepted | The exponential mean test in log coordinates uses the held endpoint and the resolved nominal window. |
| pareto | b | both | failure | accepted | The scalar log tilt has just the log-uniform and lower point-mass limits on positive width, with zero width handled by feasibility. |
| pareto | b | both | uniqueness | accepted | The fitted shape and effective cuts are unique, and a free lower nominal cut is flat exactly when the held support end supplies the minimum. |
| pois | all free | lower | criterion | accepted | The conditional mean of the positive-rate tilt covers the strict interior, with singleton and admitted lower-atom exceptions. |
| pois | all free | lower | failure | accepted | The only failure on a valid lower-tight window is an excluded bottom atom for a constant positive sample. |
| pois | all free | lower | uniqueness | accepted | Strict mean matching gives at most one root on a nonsingleton, and the compatible singleton, alias and zero-mass cut rules give the complete joint set. |
| pois | all free | upper | criterion | accepted | The conditional mean of the positive-rate tilt covers the strict interior, with singleton and admitted lower-atom exceptions. |
| pois | all free | upper | failure | accepted | The top point mass is the only excluded face on a compatible nonsingleton with a tightened upper cut. |
| pois | all free | upper | uniqueness | accepted | Strict mean matching gives at most one root on a nonsingleton, and the compatible singleton, alias and zero-mass cut rules give the complete joint set. |
| pois | all free | both | criterion | accepted | With both cuts tight a constant gives a compatible singleton and distinct counts give an interior mean. |
| pois | all free | both | failure | accepted | A compatible singleton or an interior mean covers all both-tight Poisson samples. |
| pois | all free | both | uniqueness | accepted | Strict mean matching gives at most one root on a nonsingleton, and the compatible singleton, alias and zero-mass cut rules give the complete joint set. |
| rayleigh | all free | lower | criterion | regraded | The transformed score test requires strictly positive observations because the Jacobian factor vanishes at zero. |
| rayleigh | all free | lower | failure | regraded | The exact failure list includes the zero-likelihood branch before the positive-data collapse and linear-density limits. |
| rayleigh | all free | lower | uniqueness | regraded | The scalar optimum and cut strictness identify one fit on the positive-likelihood branch, while zero data give no positive-likelihood fit. |
| rayleigh | all free | upper | criterion | regraded | The transformed score test requires strictly positive observations because the Jacobian factor vanishes at zero. |
| rayleigh | all free | upper | failure | regraded | The exact failure list includes the zero-likelihood branch before the positive-data collapse and linear-density limits. |
| rayleigh | all free | upper | uniqueness | regraded | The scalar optimum and cut strictness identify one fit on the positive-likelihood branch, while zero data give no positive-likelihood fit. |
| rayleigh | all free | both | criterion | regraded | The transformed score test requires strictly positive observations because the Jacobian factor vanishes at zero. |
| rayleigh | all free | both | failure | regraded | The exact failure list includes the zero-likelihood branch before the positive-data collapse and linear-density limits. |
| rayleigh | all free | both | uniqueness | regraded | The scalar optimum and cut strictness identify one fit on the positive-likelihood branch, while zero data give no positive-likelihood fit. |
| weibull | shape | lower | criterion | accepted | Tilting X to the held shape gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| weibull | shape | lower | failure | accepted | The eligible face is the zero-rate power density on finite windows, with infinite-tilt collapse only at the lower observed extreme on positive width. |
| weibull | shape | lower | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| weibull | shape | upper | criterion | accepted | Tilting X to the held shape gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| weibull | shape | upper | failure | accepted | The eligible face is the zero-rate power density on finite windows, with infinite-tilt collapse only at the fixed effective lower cut on positive width. |
| weibull | shape | upper | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| weibull | shape | both | criterion | accepted | Tilting X to the held shape gives the stated strict mean range after evaluating the carrier and statistic endpoints at the observed cut. |
| weibull | shape | both | failure | accepted | The finite zero-tilt face includes threshold equality, and with both cuts estimated a constant yields shrinking-window divergence. |
| weibull | shape | both | uniqueness | accepted | The score is strictly monotone in the single free tilt and positive density fixes every estimated cut on an attained branch. |
| zeta | all free | lower | criterion | regraded | The separate-domain proof does not compare the looser signed window at lower count one with the tighter parent-domain window. |
| zeta | all free | lower | failure | regraded | The separate-domain proof does not compare the looser signed window at lower count one with the tighter parent-domain window. |
| zeta | all free | lower | uniqueness | regraded | The separate-domain proof does not compare the looser signed window at lower count one with the tighter parent-domain window. |
| zeta | all free | upper | criterion | regraded | The fixed lower count fixes eligibility for the signed extension, so the exact upper-cut transfer can state each exponent domain explicitly. |
| zeta | all free | upper | failure | regraded | The fixed lower count fixes eligibility for the signed extension, so the exact upper-cut transfer can state each exponent domain explicitly. |
| zeta | all free | upper | uniqueness | regraded | The fixed lower count fixes eligibility for the signed extension, so the exact upper-cut transfer can state each exponent domain explicitly. |
| zeta | all free | both | criterion | regraded | The separate-domain proof does not compare the looser signed window at lower count one with the tighter parent-domain window. |
| zeta | all free | both | failure | regraded | The separate-domain proof does not compare the looser signed window at lower count one with the tighter parent-domain window. |
| zeta | all free | both | uniqueness | regraded | The separate-domain proof does not compare the looser signed window at lower count one with the tighter parent-domain window. |

## Proof errors and scope corrections

**Zeta domain changes can reverse the answer.** Replacing a free lower cut by the sample minimum keeps the same exponent domain only within a common-domain model. With lower estimation and fixed upper cut 3, or with both cuts estimated, data `(2,3)` give the tight parent window `[2,3]` and the looser declared signed window `[1,3]`. On the tight parent window with `s>1`, the probability of 3 is strictly below 2/5, so the likelihood increases toward its excluded harmonic supremum 6/25 as `s` decreases to one. On the signed window put `r=-s`, `y=(2/3)^r` and `z=(1/3)^r`. Its likelihood is `y/(1+y+z)^2`. For `r≤0` the probabilities are ordered and the product of the two smallest is at most 1/9. For `0≤r≤2`, `y≤1` and `z≥1/9` give the upper bound 81/361. For `r≥2`, `y≤4/9` gives the upper bound 36/169. Each is below 6/25. If both cuts vary, tightening the upper cut within either exponent domain preserves these bounds, and parent windows with intermediate cuts are dominated by the tight one. The union has no MLE even though the signed submodel has one.

On the same designs, `(2,3,3,3)` gives the tight parent supremum 24/625 by the same monotone score. At `s=-1` the signed window gives 1/24, strictly greater, and its interior log mean guarantees a unique signed exponent maximum. The union therefore attains. These exact examples isolate the missing comparison. I retained only the separate-domain statements with the free lower cut. The fixed-lower rows retain exact directions because upper tightening preserves the signed extension whenever the fixed lower cut permits it. The joint comparison and equality semantics for the union should be stated before those six rows receive full coverage. The current signed implementation also has a finite indexing cap at 2,147,483,647 on the rounded upper count. [Zeta proof](2026-11-27-trunc-est-math-report.md#pareto-endpoint-and-count-exceptions) · [Exact witnesses](2026-11-27-mod-rev5-data/exact_checks.py) · [Zeta implementation](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/truncation.hpp)

**Rayleigh needs a positive-density condition in the atlas text.** For example, with data `(0,0,2)` and tight window `[0,2]`, the mean of the squared observations is 4/3, strictly between zero and the transformed midpoint 2. The likelihood is identically zero because the original density is zero at either zero observation. The source report already gives the positive-likelihood restriction and the zero-likelihood outcome in its common hypotheses. The revised nine rows carry those qualifications into the standalone claims. [Source hypotheses](2026-11-27-trunc-est-math-report.md#joint-profiling-lemma) · [Known-window Rayleigh proof](2026-11-27-trunc-expfam-math-report.md#rayleigh-free-truncateknown)

**The original known-window transfer needs the common-parent premise.** TASK-448 correctly repairs the earlier unconditional transfer. Ordinary tightening is pointwise in every compatible coefficient vector, even with Pareto's moving support. Supremizing and using feasibility of the tight window proves equality of the joint and tight suprema, including coefficient escape. A positive finite supremum forces the mass ratio of any maximizing sequence to tend to one, so its ordinary conditional law and the corresponding tight law have total variation distance tending to zero. Exact equality allows precisely the zero-removed-mass plateaus. Repeated observations impose the same cut constraints as a single observation there. On zero effective width a continuous fit can diverge by approaching through positive-width windows, while a compatible count singleton already has probability one. When no positive-width expansion is possible the conditioning event is invalid. With the Pareto endpoint held, a lower estimate at that endpoint can be padded arbitrarily downward. With it free, the nominal lower cut and the parent endpoint can jointly supply the same effective cut.

For the declared `[1,Inf]` alias, regular coefficients use the ordinary likelihood and obey tightening. The extension permits additional laws that must be considered separately. At NB size zero, positive samples `(1,2)` with fixed lower cut one and `(2,3)` with fixed upper cut infinity have no ordinary positive-likelihood fit anywhere. Their available alias gives the logarithmic family, whose conditional mean increases strictly from one to infinity as the geometric weight runs from zero to one. Each sample consequently has exactly one alias probability and the unique alias window. The same reasoning covers both cuts estimated. Binomial `(1)` with probability held at zero admits every size at least one on the alias, with no ordinary positive-data fit. With size held at two and probability free, a tight singleton attains at every interior probability and the alias adds probability zero. For all-one Poisson, geometric and positive-size NB data, the extension can also add a distinct maximizing coefficient and window pair to an ordinary singleton. These statements assume the joint search includes the alias whenever compatible with the fixed cuts. The binding selects the alias after resolving the cuts. [Original transfer](2026-11-27-trunc-expfam-math-report.md#all-applicable-truncateestimated) · [TASK-448 correction](2026-11-27-trunc-est-math-report.md#correction-to-the-known-window-transfer) · [Binding dispatch](../../../bindings/common/modifiers.hpp)

## Evidence and implementation checks

The two-free continuous proofs use the closed natural-domain likelihood, with compact-direction bounds based on the leading tilt of each family. The relevant tilts have a unique peak or are strictly monotone, so two distinct observations cannot both maximize them, even when both observations are cut points. The finite integrable boundary faces are approachable from the interior. At an edge, matching the tangential statistic and checking the inward derivative, including equality and divergent one-sided moments, gives its global comparison. At a corner both directional signs enter. The gamma corner example follows from strict Jensen applied to the reciprocal over `[1,2]`; the beta signs follow from the symmetry of its carrier and the strict interior increase in `x(1-x)` above its endpoint value. The inverse Gaussian corner integrals give means 2 and 7/12. In each scalar case I checked the free tilt map against the parameter domain and the carrier at each effective endpoint, including the decreasing reciprocal transforms, unbounded lognormal log windows, and the divergent Lévy first moment on an unbounded upper side. [Natural-coordinate proof](2026-11-27-trunc-est-math-report.md#continuous-likelihood-arguments) · [Likelihood closure](../ml-behaviour.tex#lem:closure) · [Geyer on recession](https://arxiv.org/abs/0901.0455)

The count checks use ceiling and floor on the closed nominal cuts and cap the binomial carrier by size. At interior held binomial probability on a bounded nonsingleton, the ratio to the next count grows without bound as size increases. Only a constant at the top can take the top atom as a supremum. A lower-tight constant can instead attain by taking size equal to its count. A retained singleton stays flat even as size tends to infinity, as the earlier review demonstrated. With both parent coordinates free and nonconstant data, bracketing the unique Poisson mean root keeps rescaled odds in a compact positive interval for large sizes. The limiting profile then has the stated Poisson value. An escaping maximizing sequence must asymptotically match that mean because the conditional log likelihood in log odds is strictly concave, with a strict limiting loss at each fixed log-odds displacement. The prior bounded-window polynomial and positive lower-only analytic-tail arguments give finite comparisons or an explicitly flat tail. A finite equality with the reachable face attains. Transcendence excludes finite-to-face equality and a zero first-order gap for rational empirical moments on positive lower-only windows. The ordinary untruncated comparison treats variance equal to the mean as escape. In particular, the exact maximizer-set claims here use integer comparisons with a proved tail and unique scalar roots. For `(1,1,2)` in closed `[1,2]`, all sizes at least two attain at `prob=1/size` because the conditional odds equal the empirical odds 1/2 and attain the global multinomial bound. [First review](2026-11-27-mod-rev1-report.md#proof-errors-and-grading-corrections) · [Finite comparison](2026-11-25-tbinom-math-report.md#a-finite-decision-certificate) · [Positive-binomial result](2026-11-25-tbinom2-math-report.md) · [Hermite–Lindemann](https://arxiv.org/abs/2306.14352) · [Ordinary binomial criterion](https://www.tandfonline.com/doi/abs/10.1080/01621459.1983.10477949)

**Normal margin.** On the dyadic sample in TASK-448, all three bound patterns give `[-1,1]`. With mean held at zero or with both parent coordinates free, symmetry and strict concavity give zero linear tilt. The exact inward gap is 2097151/13194139533312. On the quadratic tilt interval `[-1/100,0]`, the density ratio to uniform exceeds 99/100, so the variance of the squared observation is bounded below by `(99/100)(4/45)` via minimization over the centering constant in the second moment. The sign at the other end of this interval is reversed, which brackets the unique negative root. The exact gain bound per observation is below `1.44e-13`. The same calculation replays the prior rational normal near-face witness and the exact uniform and translated-exponential equality cases. The core source compares the successful interior likelihood against the face with an allowance of `1e-8` per observation. In exact arithmetic this sends the dyadic sample into the comparison branch even though a unique MLE exists. The proof establishes the mathematical gap, and the branch assessment follows from source inspection conditional on a successful search. [TASK-448 sample](2026-11-27-trunc-est-math-report.md#exact-audit-inputs) · [Exact arithmetic](2026-11-27-mod-rev5-data/exact_checks.py) · [Normal core](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_mle.hpp)

On NB data `(1,2,2)` at size two, the finite-window weights at the excluded endpoint are exactly `(2,3)`, giving mean 8/5 below 5/3. The strict comparison certifies the weighted law as the unattained supremum. The generic status names the upper boundary and supplies a uniform payload only at size one. The binomial two-count shortcut keeps a representative while the exact profile has the integer-indexed set above. The zeta resolved-window refusal is within implementation scope when the resolved lower count exceeds one, even where the mathematical signed submodel could attain on a looser admissible window. These are route observations from source inspection. [Count status mapper](../../../univariateML-cpp/include/cppunivariate/distributions/geom/truncation.hpp) · [Binomial core](../../../univariateML-cpp/include/cppunivariate/distributions/binom/tbinom_mle.hpp) · [Zeta core](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/truncation.hpp)

## Rows not joined

Zero proposal keys were omitted from the submission. All 270 have eligible parent and modifier keys. Six zeta rows carry only their scoped claims, and the joint claims beyond that scope remain open. The submitted CSV is ready for the coordinator's join.

## Open by family

| Family | Remaining work |
| --- | --- |
| beta | The reviewed masks have full mathematical coverage. Near the corner, a finite-budget implementation needs verified inward signs and coefficient enclosures. |
| binom | The discrete comparisons describe the full sets. A general positive lower-only integer unimodality theorem or an exact nonconstant lower-only tie remains open, and the two-count set needs a mixed integer–real representation in the binding. Alias availability in a joint estimate needs an interface decision. |
| exp | The free-rate mathematical rows are settled, including the zero support plateau. A general truncation fitting route is outside the current implementation. |
| gamma | The reviewed masks are covered mathematically. Near a corner or equality, a finite-budget implementation needs enclosures for the edge means and the interior root. |
| geom | The scalar theory covers the reviewed rows. The separate alias atom matters if a joint fit allows looser windows, and the interface still uses the resolved-window convention. |
| invgamma | The held-alpha slice is settled. Free alpha with held or free beta needs analysis of the relevant new tilt or joint escape directions. |
| invgauss | The mathematical masks are covered. A finite-budget implementation needs to enclose the face means, including handling the divergent first moment at an unbounded Lévy edge. |
| invweibull | The held-shape slice is settled. A free shape changes the power statistic and needs separate existence and escape analysis. |
| lnorm | The reviewed masks are covered by the log-coordinate normal proof. A general modifier fit would need the transformed checks near the face with the Jacobian included in likelihood comparisons. |
| nbinom | Positive held size and the declared size-zero branch are covered. The size-zero example exposes the joint alias decision. Free-size general windows and a named weighted-limit payload at positive size other than one remain open. |
| norm | The exact face and root tests are settled. The public margin can give unresolved comparisons despite an interior optimum, as shown by the dyadic sample. |
| pareto | The reviewed mathematical masks are settled. A fitting route needs to keep the nominal lower cut and the support endpoint distinct and express their tied set. |
| pois | The ordinary count theory and alias extension are covered. Reporting the complete joint set requires a decision about whether to include the looser atom at one. |
| rayleigh | The positive-density theory and zero-likelihood branch are covered. A general truncation fitting route is outside the current implementation. |
| weibull | The held-shape slice is settled. A free shape requires a separate boundary analysis because the statistic depends on shape. |
| zeta | Decide whether to estimate over the union of domains and whether to extend signed exponents to every finite window. With a free lower cut, a comparison of the tight parent profile and the best signed window at lower count one must retain exact ties and distinguish attained from excluded winners. |

## Validation

All mathematical replays and validators used a single workstation CPU slot. No builds were needed for this review.

| Check | Result |
| --- | --- |
| [TASK-448 replay](2026-11-27-trunc-est-math-data/check.py) | Passed the 270 claims, 90 regime anchors and stored exact and 70-digit results. |
| [Independent exact replay](2026-11-27-mod-rev5-data/exact_checks.py) | Passed the normal gap and equality inequalities, the two zeta comparisons, the inverse Gaussian and beta corner arithmetic, and the count examples. |
| [TASK-439 replay](2026-11-27-trunc-expfam-math-data/check.py) | Passed the two-precision consistency checks. |
| [Prior binomial replay](2026-11-25-tbinom-math-data/check.py) | Passed the interval likelihood signs and the finite-prefix examples. |
| [Positive-binomial interval proof](2026-11-25-tbinom2-math-data/face_interval.py) | Passed all 80,000 directed interval boxes. |
| [Submission validator](2026-11-27-mod-rev5-data/validate.py) and [atlas validator](../../tools/existence_atlas.py) | Passed all keys, held masks, eligibility, provenance, known-window counterparts, and evidence anchors. |
| [Markdown link check](../../tools/check_links.py) | The baseline and final run report the same 47 inherited broken destinations and one existing parser false positive at line 461 of the beta-prime report. This report adds no broken links. |
