# Pearson type IV: literature check — TASK-160

**Provenance and review.** Codex (gpt-6.1-sol, low effort, live web search; 23 searches,
20 fetches) wrote the report below on 2026-11-08. Claude reviewed it the same day.
The type V boundary expansion attributed to Cheng (2011) was checked directly against the
paper's text (§3.1, eq. 11a–11d): the log likelihood is expanded as
L_IV = L_V + M_V δ² + O(δ³) around the inverse-gamma boundary with free threshold, and
M_V > 0 means type IV improves on the best type V. Cheng treats positively skewed or
symmetric data only. The PearsonDS fitter description could not be re-fetched (DNS
failure) and stands as Codex's [read]. Markers: [read] source text inspected,
[abstract] abstract or index only, [cited-by] secondary attribution. Cheng (2017) is
already cited in the [boundary literature report](2026-11-03-boundary-lit-report.md); its
Pearson and Johnson chapter is the part relevant here.

---

Research reconnaissance, 8 November 2026: 23 search queries; 20 fetch attempts, including failures and repeat views. Stopped at the fetch limit. Only `report.md` was written. Evidence markers distinguish inspected text **[read]**, abstracts/index records **[abstract]**, and secondary attribution **[cited-by]**. Numbered references resolve to full bibliographic entries below; unknown fields remain explicit. Negative findings mean “not established by the inspected evidence,” not “never published.”

## 1. Maximum likelihood

**[abstract; 1]** Nagahara (1999) is verified bibliographically. Its abstract describes practical implementation, stock-return fitting and tail probabilities; indexed text recommends ML using the general normalizer. Full text was blocked: its optimizer, initialization, information matrix and exact theoretical guarantees remain unverified.

**[read; 2, §8]** Heinrich recommends numerical minimization of the negative log likelihood, with moment estimates as starts, and supplies normalizer derivatives involving complex digamma functions. This is practical guidance, not an existence/uniqueness theorem or prescribed optimizer.

**[read; 3, §§2.2–3.1]** Cheng uses bounded Nelder–Mead, moment starts when admissible, otherwise starts matching the first three moments under an additional stabilizing constraint. He reports robust numerical behavior but restricts parameters for stability. Crucially, he expands the IV log likelihood around a free-location type V boundary and gives a coefficient whose positive sign implies local improvement into IV. He recommends fitting V first. This supports the proposed vanishing-scale/diverging-skew escape, without certifying global optimality. His bounds must not be mistaken for the full admissible family.

**[read; 2, §8; 3, §2.2]** A source discrepancy matters: Cheng characterizes Heinrich as recommending moments, whereas Heinrich explicitly prefers ML and uses moments for initialization. Cheng correctly identifies that optimizing through four-moment coordinates excludes distributions lacking those moments. Neither source supplies a controlled finite-sample efficiency comparison.

**[abstract; 4]** Cheng's later chapter systematically searches Pearson types using boundary score statistics, identifies III and V as boundary-embedded models, and describes restricted, reasonably robust IV fitting. Full text was inaccessible; precise gamma/normal paths and inference conditions were not checked.

## 2. Computation

**[read; 2, §§2–7]** Heinrich agrees with the supplied density, admissible domain, mode and Student-t specialization. He gives complex-gamma and hypergeometric/product normalizers, an angular CDF integral and hypergeometric CDF representations. His C rejection generator follows Devroye and requires **m > 1**, leaving part of the requested family uncovered. The supplied angular transformation is consistent with his CDF integral; no re-derivation is needed.

**[abstract; 5]** Willink gives a closed-form probability integral and evaluation method, with hypergeometric/incomplete-beta connections. “Closed form” does not imply numerical stability throughout parameter space.

**[read; 6b]** PearsonDS uses `gsl::lngamma_complex` for normalization, otherwise Heinrich's algorithm, documented as slow at large |nu|. CDF evaluation uses quadrature without gsl; with gsl it switches between quadrature and Heinrich/Willink representations. Hypergeometric partial sums escalate from double to double-double arithmetic when cancellation exceeds tolerance. Quantiles use Newton iteration, with tolerance specified as absolute quantile error. Random generation retains Heinrich's m > 1 restriction. These are algorithm descriptions and target tolerances, not verified uniform relative-error bounds for extreme tail probabilities or quantiles. No universal tail-accuracy guarantee was found.

## 3. Software

**[read; 6a, 6c]** PearsonDS 1.3.2 exposes separate moment (`pearsonFitM`) and likelihood (`pearsonFitML`) interfaces: there is no single fitting default to label ML. The latter fits all subclasses with `nlminb` and returns the largest fitted likelihood. Starts use empirical moments adjusted into each subclass's moment region, then support adjustments. Its manual calls the implementation preliminary and slow, without analytic gradients. The manual's assertion that boundary subclasses contain the MLE with probability zero should not be adopted as a boundary-certification result, particularly given Cheng's explicit boundary analysis [3].

**[abstract; 7]** MATLAB provides moment-parameterized Pearson density/CDF/random functions (`pearspdf`, `pearscdf`, `pearsrnd`); the inspected documentation establishes evaluation, not default ML fitting.

**[abstract; 8]** Python lmfit has `Pearson4Model`, a curve-fitting model whose starts set exponent 1.5 and skew 0. Its reported FWHM approximation can err about 20%; that is not a density or tail error. **[abstract; 9]** Mantid provides a Python PearsonIV peak function with location shifted to the mode. Neither inspected record establishes default iid ML fitting. No maintained ROOT or Julia distribution implementation was verified; this is a search gap, not evidence of absence.

## 4. Related work

**[abstract; 10]** Forman–Sørensen classify ergodic Pearson diffusions with linear drift and quadratic squared diffusion coefficient; stationary laws span the Pearson system. They develop martingale/GMM/quasi-likelihood/likelihood inference and report consistency and asymptotic normality for the estimators considered. These dependent-process results cannot simply certify an unrestricted iid IV MLE.

**[read; 11, §5]** Pearson IV is a classical skewed-t alternative, distinct from Jones–Faddy's construction and from the Azzalini-type and two-piece alternatives discussed there. “Skew-t” alone is therefore ambiguous; it is not a unique synonym for IV.

**[cited-by; 12 via 3; abstract; 12]** Parrish jointly selects and fits Pearson members using loss functions including likelihood. Cheng warns that its moment-coordinate approach misses infinite-moment cases. **[read; 6c; abstract; 4]** Whole-system ML is thus existing work, but selecting the best converged subtype fit is not an exhaustive escape classification.

## 5. Applications

**[abstract; 13]** Ion implantation: Ashworth–Oven–Mundin fit amorphous implantation profiles with Pearson curves including IV; they warn that replacing full moments by semi-infinite moments can invalidate fitting. **[abstract; 1]** Finance: Nagahara applies IV to stock returns. **[read; 14, §2]** X-ray diffraction: silicon (111)/(220) peaks are fitted with IV to study widths and area ratios. **[read; 2, §1]** Particle physics: Heinrich motivates IV with asymmetric, heavy-tailed pull distributions. These uses do not themselves establish iid MLE guarantees.

## Gaps relevant to solid ML

Across the inspected evidence [1–6, 10–12], the following remain **unestablished**, rather than disproved:

- Necessary and sufficient sample conditions for a finite global MLE; uniqueness or enumeration of competing stationary points.
- The conjectured tied-location collapse exponent `(2m−1)(n−k)−k`, including equality cases and joint boundary paths.
- Exhaustiveness of escapes: V has explicit local analysis [3], but normal/gamma paths, m ↓ 1/2, divergent location/skew, and interacting rates need a complete compactification.
- A global certificate distinguishing boundary supremum, unbounded likelihood, and optimizer failure.
- A checked full four-parameter Fisher information and inference regimes at symmetric VII and nonregular V/III/normal boundaries. Interior asymptotics must be separated from boundary and infinite-moment behavior.
- Reliable d/p/q/r across the entire domain, with tail-relative error control and random generation for 1/2 < m ≤ 1.

## Bibliography

1. Nagahara, Y. (1999). *The PDF and CF of Pearson type IV distributions and the ML estimation of the parameters*. Statistics & Probability Letters **43**, 251–264. [DOI](https://doi.org/10.1016/S0167-7152(98)00265-X).
2. Heinrich, J. (2004). *A Guide to the Pearson Type IV Distribution*. CDF/MEMO/STATISTICS/PUBLIC/6820, pp. 1–12. [Text](https://citeseerx.ist.psu.edu/document?doi=3d0af8aca37b0cd56cdc65562041431f6968f8eb&repid=rep1&type=pdf).
3. Cheng, R. (2011). *Using Pearson Type IV and Other Cinderella Distributions in Simulation*. Proceedings of the Winter Simulation Conference; volume/pages unknown (inspected §§2.2–3.1, pp. 461–462). [Text](https://informs-sim.org/wsc11papers/040.pdf).
4. Cheng, R. (2017). *The Pearson and Johnson Systems*. In *Non-Standard Parametric Statistical Inference*, Oxford University Press, 173–200. [DOI](https://doi.org/10.1093/oso/9780198505044.003.0009).
5. Willink, R. (2008). *A Closed-form Expression for the Pearson Type IV Distribution Function*. Australian & New Zealand Journal of Statistics **50**, 199–205. [DOI](https://doi.org/10.1111/j.1467-842X.2008.00508.x).
6. Becker, M., Klößner, S.; Heinrich, J. (contributor) (2025). *PearsonDS: Pearson Distribution System*, version 1.3.2; software manual, volume/pages inapplicable. [6a: manual](https://cran.r-project.org/web/packages/PearsonDS/PearsonDS.pdf); [6b: PearsonIV](https://search.r-project.org/CRAN/refmans/PearsonDS/html/PearsonIV.html); [6c: pearsonFitML](https://search.r-project.org/CRAN/refmans/PearsonDS/html/pearsonFitML.html).
7. MathWorks (year unknown). *pearspdf—Pearson probability density function*. MATLAB documentation; volume/pages inapplicable. [URL](https://uk.mathworks.com/help/stats/pearspdf.html).
8. lmfit developers (year unknown). *Built-in Fitting Models*, Pearson4Model. Software documentation; volume/pages inapplicable. [URL](https://lmfit.github.io/lmfit-py/builtin_models.html).
9. Mantid developers (2026). *PearsonIV*. Nightly software documentation; volume/pages inapplicable. [URL](https://mantidproject.github.io/docs-nightly/fitting/fitfunctions/PearsonIV.html).
10. Forman, J. L., Sørensen, M. (2008). *The Pearson Diffusions: A Class of Statistically Tractable Diffusion Processes*. Scandinavian Journal of Statistics **35**, 438–465. [DOI](https://doi.org/10.1111/j.1467-9469.2007.00592.x).
11. Jones, M. C., Faddy, M. J. (2003). *A skew extension of the t-distribution, with applications*. JRSS B **65**, 159–174. [DOI](https://doi.org/10.1111/1467-9868.00378).
12. Parrish, author initials unknown (1983). *On an integrated approach to member selection and parameter estimation for Pearson distributions*. Computational Statistics & Data Analysis **1**, 239–255. [DOI](https://doi.org/10.1016/0167-9473(83)90098-1).
13. Ashworth, D. G., Oven, R., Mundin, B. (1990). *Representation of ion implantation profiles by Pearson frequency distribution curves*. Journal of Physics D: Applied Physics **23**, 870–unknown. [DOI](https://doi.org/10.1088/0022-3727/23/7/018).
14. Authors/year/title/venue/volume/pages unknown. Silicon-film experimental study, §2 (XRD methods). [Inspected institutional PDF](https://publications.tno.nl/publication/34633009/o3HS21/v07008.pdf).
