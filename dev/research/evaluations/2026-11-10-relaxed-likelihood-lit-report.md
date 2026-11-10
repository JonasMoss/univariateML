# Relaxed likelihood, existence and consistency: literature reconnaissance

TASK-173 · 2026-11-10 · branch/worktree `codex/relax-lit` · literature only

**Verdict.** Compactifying an estimation problem, extending its criterion without changing its supremum, and using upper semicontinuity and compactness are established ideas. The closest verified historical lead is **Perlman (1972), pp. 263 and 277**: the latter explicitly credits Bahadur with parametrization by probability laws and a weak-convergence closure. **Hypoconvergence** is explicitly used for likelihood optimization by **Geyer (1994)**; **completion**, **extended/generalized MLE**, and **limiting conditional models** are established for exponential families. I did not locate the exact proposed sample-dependent upper envelope on the weak closure, its proposed “relaxation gap,” or an exhaustive general classification with all the listed categories. These are candidates for a synthesis, not established novelty findings. Several assertions in the proposed framework need qualification before such a synthesis can be claimed: weak closure does not canonically supply density versions; exactness is a statement about all approaches, not one favorable path; and the integral-at-most-one condition requires a strict separation clause for consistency. The requested exact Le Cam–Schwartz theorem and the original sine-family citation remain open.

## Scope, evidence grades and search record

This is research reconnaissance, not a completed formal paper-workflow audit: there is no frozen manuscript inventory or evidence bundle. The user's express search/download instruction authorizes source acquisition. No code, Git writes, board edits, installations or delegation were used. Searches were public bibliographic/claim queries; no unpublished manuscript was uploaded. The search used 20 web queries in five batches, plus approximately twenty page/PDF acquisition attempts (including unsuccessful access and retries). Some multi-query results were dominated by one query, so this budget did not produce equal coverage of all requested authors.

**Verified in source text** means inspected primary text, with a locator below; it does not imply that the source proves the user's stronger formulation. **Cited secondhand** identifies the inspected intermediary and distinguishes its report from original-source verification. **Open** is an unresolved item, not a negative literature finding. Mathematical checks below are explicitly this report's deductions, not either literature grade. Defective/subprobability theory is left to the parallel job; the brief historical reference to it is needed to identify Perlman's passage.

Five PDFs and adjacent `.provenance.json` files are retained under [local/literature](../../../local/literature/). Text extraction and page images used for inspection were temporary. The 1990 Le Cam file is a technical-report version, and Geyer 2009 is arXiv v1; their locators below use those versions rather than silently transferring page numbers to the published articles.

## 1. Is the relaxed-likelihood construction known, and under what names?

### Located precedents

| Source | Exact inspected location and finding | Grade and limit |
|---|---|---|
| Perlman (1972) | Introduction, p. 263: summarizes earlier consistency arguments as extending the log likelihood ratio to a suitable compactification without changing its supremum. P. 277, after Example 4: credits Bahadur with using the law itself as parameter and a weak-convergence closure in measures of mass at most one. | **Verified in source text.** Direct precedent for the compactification viewpoint, not verification of the proposed maximal usc envelope. |
| Perlman (1972) | Theorem 2.4, p. 271: compact-space result using local dominance, almost-sure usc and measurable neighborhood suprema. | **Verified in source text.** A precise consistency mechanism; no integral-of-relaxed-density criterion is stated there. |
| Geyer (1994) | §2.1, Theorem 1, p. 264, especially (8a–b): neighborhood infima of suprema characterize hypoconvergence; theorem also establishes usc of the exact log likelihood under its assumptions. Appendix A.2, pp. 272–273: epi/hypoconvergence and optimization. | **Verified in source text**, including visually inspected scanned pages. Monte Carlo approximations converge to a specified likelihood; this is not the same construction as completing arbitrary likelihoods over weak laws. |
| Csiszár–Matúš (2005) | §1, electronic reprint pp. 1–2: variation, information and reverse-information closures distinguished; generalized ML at a bounded unattained supremum discussed on p. 2. §2.4, pp. 5–6: extensions by face-supported exponential families. | **Verified in source text.** Those closure concepts must not be identified with weak closure. The authors explicitly flag weak closures as harder. |
| Geyer (2009) | arXiv v1 §3.4, Theorem 6, pp. 10–11: limiting densities along recession directions and the limiting conditional model; §3.14.2 treats estimation in the completion. | **Verified in source text.** A structured, model-specific realization of escape to a proper limit family. |
| Barndorff-Nielsen (1978) | Completion is attributed to this book in Geyer (2009), introduction p. 1 and §3.8, p. 13. | **Cited secondhand**, through Geyer. Original book passages not inspected. |
| Rockafellar–Wets (1998) | Geyer (1994), acknowledgements p. 272, explicitly credits the optimization approach to Rockafellar's course using a draft of this book. Publisher confirms book identity. | **Cited secondhand** for the technical connection. Exact book theorem/definition numbers remain open. |

The most useful vocabulary is **upper semicontinuous regularization/envelope**, **closure of the hypograph**, and **hypoconvergence** for maximization. Epi-convergence and Gamma-convergence concern minimization after changing sign. A static usc envelope is not automatically a new Gamma-limit theorem. “Extended MLE,” “generalized MLE” and “completion” have model-specific meanings and are not interchangeable definitions.

### Mathematical checks on the proposed construction

These are deductions, not literature-attribution claims.

1. **Topology needs hypotheses.** For a locally compact, second-countable Hausdorff sample space, its one-point compactification is compact metrizable, and probability laws on it form a weakly compact metrizable space. An arbitrary dominated measurable model does not supply these hypotheses. The sequential formula needs sequential/metric assumptions; otherwise use neighborhoods or nets.
2. **The envelope theorem itself is elementary.** With fixed density versions define
   \[
   F(Q;x)=\inf_{U\ni Q}\sup\{L(\theta;x):P_\theta\in U\}.
   \]
   On a compact metric closure this agrees with the supremum over sequential limsups. It is usc as an extended nonnegative function, has the same global supremum, and attains it, including an infinite supremum. A maximizing sequence plus compactness proves the attainment statement. No inference about a conventional MLE follows just from this relaxed attainment.
3. **It may change the criterion at existing model points.** In general \(F(P_\theta;x)\ge L(\theta;x)\), with strict inequality possible. Weak law convergence is too coarse to preserve point evaluations. Repeated parametrizations and density versions also need fixing: laws alone do not determine pointwise sample likelihoods.
4. **One recovery path is insufficient for exactness.** Locally uniform convergence of densities to \(p_Q\) near the observations *along one sequence* gives \(F(Q;x)\ge L_Q(x)\), not equality. Equality needs an upper bound for every approach, together with a recovery sequence. Full locally uniform convergence as \(P_\theta\to Q\) is sufficient when the limiting likelihood is finite and densities are well defined.
5. **The gap is not universally defined.** A singular law may have no density with respect to the original dominating measure. Even when both likelihoods are defined, \(\infty-\infty\) and \(-\infty-(-\infty)\) are undefined. Define “exact” first by equality of well-defined likelihood values; introduce a logarithmic gap only on an appropriate finite, positive domain.
6. **Sample and observation envelopes differ.** In general the envelope of \(\prod_i p_\theta(x_i)\) is at most \(\prod_i\bar p_Q(x_i)\), with possible strict inequality because individual peaks may require incompatible parameter paths.

### Requested-author coverage and precise open entries

| Requested source | What this search establishes; what is missing |
|---|---|
| Wald (1949) | **Cited secondhand:** Perlman introduction and references, pp. 263, 281; Wald's approximate-estimator result is located secondhand at p. 600, Theorem 2 (Perlman p. 265). **Open:** inspect original pp. 595–601 and verify DOI on publisher/index before assigning its precise hypotheses. |
| Kiefer–Wolfowitz (1956) | **Cited secondhand:** Perlman pp. 263, 269 and 277. DOI/title located, but Euclid access returned an iframe shell. **Open:** original compactification and density assumptions, theorem numbers and pages; no verified attribution of the proposed integral criterion. |
| Bahadur (1958) | **Cited secondhand:** Le Cam technical-report §4, printed pp. 8–13, and references p. 29 identify an inconsistency example. **Open:** original Sankhyā 20, 207–210; whether any oscillatory-density example occurs there. |
| Bahadur (1971) | **Open:** original text not inspected. Do not substitute Perlman's explicit reference to Bahadur **1967**, p. 320, for the requested 1971 source. |
| Le Cam (1953) | **Cited secondhand:** Perlman §5, p. 279, points to pp. 302–304 and Bochner-type dominance rather than compactification. **Open:** original text and pagination/version identity. |
| Le Cam (1970) | **Open:** paper on assumptions for asymptotic normality not inspected; no conclusion about its treatment of envelopes. |
| Le Cam (1990) | **Verified in source text:** technical-report §2, Example 1, printed p. 3, gives normal-mixture likelihood blowup; §4 discusses Bahadur-type inconsistency. Does not establish the exact relaxation construction. |
| Huber (1967) | **Cited secondhand:** Perlman pp. 263, 269 summarizes normalized criteria and semidominance; reference p. 281 identifies the Berkeley paper, pp. 221–233. **Open:** original assumptions and theorem locations. |
| Pfanzagl (1988, 1994) | **Open:** original papers/book not inspected. The search surfaced the 1988 mixture-consistency title but did not resolve the intended 1994 item. Need exact bibliographic identity and full text before assessment. |
| Wang (1985) | **Open:** search identified *Strong Consistency of Approximate Maximum Likelihood Estimators with Applications in Nonparametrics*, but original theorem and DOI were not verified. No claim-support grade is assigned from search snippets. |
| Dupačová–Wets (1988) | **Open:** title/DOI identified, Euclid returned an iframe shell. Need full text for precise epi-convergence theorem and relevance to existence rather than asymptotic stability. |
| van der Vaart (1998), ch. 5 | **Open:** chapter not inspected; no theorem-number attribution from memory. Need compare general argmax consistency and the Wald subsection with this specific weak-law envelope. |

## 2. An exhaustive finite-sample classification?

**Finding: not located; open, not an assertion of absence.** Exponential-family completion provides a substantial classification within a structured class (Geyer 2009, §3 and Csiszár–Matúš 2005, §§2–3; **verified in source text**). Le Cam's technical-report §§2–4 supplies examples of nonexistence, infinite likelihood and inconsistency (**verified in source text**), but is not an exhaustive general taxonomy of weak-boundary outcomes.

The proposed list needs multiple axes rather than disjoint categories. A proper boundary law can be singular relative to the original measure, can carry an infinite envelope, and can tie an interior maximizer. Oscillation describes paths/density behavior, while attainment describes the criterion. “Interior” should usually be replaced by “in the original model”: a model need not have nonempty interior in the space of all laws. Computational budget exhaustion is an algorithmic outcome, not a likelihood geometry class.

A precise formulation would classify the **whole argmax set** by membership in the original model, mass/support and domination properties, envelope value, and agreement with a specified boundary likelihood. It should separately classify maximizing sequences and their cluster laws. **Open:** a source proving exhaustiveness of this combined classification over general dominated models; none of the inspected texts establishes it.

## 3. The integral-of-relaxed-density Wald condition

**Exact historical location: open.** Perlman's Theorem 2.4, p. 271, is a verified neighborhood-supremum/local-dominance result; p. 277 is the verified weak-law closure precedent. Neither inspected passage states the proposed condition as an integral bound on a per-observation relaxed density. Original Kiefer–Wolfowitz text remains necessary to settle that attribution.

For \(g_Q(x)=\inf_{U\ni Q}\sup_{P_\theta\in U}p_\theta(x)\), Jensen gives, when measurable and meaningful,
\[
 E_0\log(g_Q/p_0)\le\log\int_{\{p_0>0\}}g_Q\,d\mu
 \le\log\int g_Q\,d\mu.
\]
But Wald's finite-cover proof needs **strictly negative expected contrast at every competing point**, or a corresponding compact-set separation. The bound \(\int g_Q\le1\) alone gives only a nonpositive contrast. Equality in Jensen is possible when \(g_Q=p_0\), \(P_0\)-almost surely, even at a different weak boundary law. Neighborhood measurability, integrable positive envelopes enabling expectation limits, a precise approximation error, and the topology of consistency must also be specified.

A concrete warning (this report's construction) uses Lebesgue measure on \([0,1]\), true density \(p_0=1\), and, for integers \(k\ge1\),
\[
 p_k(x)=\tfrac12(1+\sin(2\pi kx))+\tfrac{k}{2}\mathbf1_{(0,1/k)}(x).
\]
Each integrates to one. Its weak limit is \(Q=\tfrac12P_0+\tfrac12\delta_0\). At that boundary, the neighborhood envelope is one for almost every irrational \(x\in(0,1)\), so \(\int g_Q=1\) and \(E_0\log(g_Q/p_0)=0\), although \(Q\ne P_0\). Thus the claimed strict separation does not follow from normalization alone. This is a mathematical counterexample to that implication, not a published example located by this search.

**Fatou qualification.** A single path with an actual pointwise density limit has integral at most one. Fatou does not say this for the pointwise **limsup** along a path. Indeed the proposed sine family itself has almost-everywhere limsup two along appropriate diverging parameter sequences. The failure can occur within one oscillatory path, not only by taking a supremum across different paths.

**Open:** whether a published source states the corrected integral bound *plus strict contrast identification* using precisely this weak-law envelope. Do not cite Wald, Kiefer–Wolfowitz or Perlman as having stated it until the relevant original passage is inspected.

## 4. Oscillating densities and vanishing normal spikes

### Sine family

**Original published source: open.** No inspected primary source contains the exact family \(1+\sin(2\pi\theta x)\) on \([-1/2,1/2]\). Perlman's Example 4, pp. 276–277, is a close-looking but substantively different periodic example: its phase shifts with the parameter, its amplitude approaches a limit, and the compactification adds a phase interval. Its approximate MLEs are consistent. It is therefore a useful topology precedent, **verified in source text**, not a citation for the high-frequency nonexistence example.

The claimed sine-family properties can be justified independently. Normalization follows from oddness. The Riemann–Lebesgue lemma gives the uniform weak limit at high frequency. For an absolutely continuous sample, almost surely there is no nonzero integer relation among its coordinates. The continuous torus flow \(t\mapsto(tx_1,\ldots,tx_n)\bmod1\) is then dense; it approaches simultaneous sine peaks, giving supremum \(2^n\). Exact simultaneous peaks require rational ratios of coordinates and have probability zero for \(n\ge2\). A bounded parameter subsequence could attain that supremum by continuity, so maximizing sequences must diverge. At \(x=0\) the envelope is **one**, not two; it is two elsewhere, hence two almost everywhere. These are deductions, not source-verified historical attributions.

Minimum-distance consistency also needs a stated metric and separation argument, not just identifiability. For this family the weak map is continuous and injective for positive finite parameters, with uniform limits as \(\theta\downarrow0\) and \(\theta\to\infty\). For \(\theta_0>0\) those limits differ from \(P_{\theta_0}\), giving separation outside any parameter neighborhood on a compactification; empirical minimum-distance estimates can consequently be consistent. **Open:** original citation, if any, and a published proof for precisely this example.

### Vanishing-spike normal mixture

Le Cam technical-report §2, Example 1, printed p. 3, gives unbounded normal-mixture likelihood by collapsing a component at an observation (**verified in source text**). It uses a fixed positive contamination weight, so it is **not** a source for the requested vanishing-weight Hellinger path. Le Cam himself gives a cautious historical attribution to Kiefer–Wolfowitz; original-priority attribution remains **cited secondhand**.

The requested stronger path follows directly. Write \(q=\varphi(\cdot-\mu)\), \(r_\sigma=\sigma^{-1}\varphi((\cdot-x_1)/\sigma)\), and \(p=(1-w)q+wr_\sigma\). Then
\[
 \|p-q\|_1\le2w,\qquad
 \int(\sqrt p-\sqrt q)^2\le\|p-q\|_1\to0.
\]
At \(x_1\), \(p(x_1)\ge(w/\sigma)\varphi(0)\to\infty\); at every remaining observation \(p(x_i)\ge(1-w)q(x_i)>0\). Thus likelihood diverges while the laws converge in Hellinger distance to the proper normal law. Choosing \(w=\sqrt\sigma\) is explicit. **This proof is this report's deduction. Open:** a published source stating the vanishing-weight Hellinger-convergent path, rather than only normal-mixture blowup. It also shows why convergence along one regular path to the same law cannot establish envelope exactness.

## 5. Le Cam and Schwartz (1960)

**Identity verified against the JSTOR issue index:** *A Necessary and Sufficient Condition for the Existence of Consistent Estimates*, *Annals of Mathematical Statistics* **31**(1), 140–150, JSTOR stable 2237499. DOI 10.1214/aoms/1177705993 was located in an index record; the DOI/Euclid original text could not be retrieved beyond an iframe shell.

**Exact original statement: open.** Missing are the original definitions of the parameter space, its uniform structure, consistency (pointwise/uniform, probability/almost sure), and Theorem 1/Corollary 1 with their assumptions. It would be misleading to supply a purported verbatim or exact theorem from memory.

Two secondhand leads surfaced: Kleijn's *The Le Cam–Schwartz theorem* lecture slides (2018), slide 6, Theorem 6.1, formulate equivalence between consistent tests for two disjoint model subsets and approximation of their indicator by uniformly continuous functions in an induced uniform structure; a Statistica Sinica article at the URL below points to original Corollary 1 and accompanying remarks for necessity of a consistent-estimation condition under additional finite-dimensional/topological assumptions. **Grade: cited secondhand**, and these are retrieval leads, not substitutes for the original statement.

The available evidence does **not** justify saying that Le Cam–Schwartz is a theorem phrased via the weak closure of laws on a one-point compactification. Its topological/uniform-structure formulation must be checked in the original before making that identification. **Open:** fetch pp. 140–150, transcribe the relevant theorem and corollary with definitions, and compare their topology with ordinary weak convergence. This is the highest-priority unresolved source acquisition.

## Bibliography, retained sources and wanted texts

DOIs below are linked explicitly; an unresolved identifier is not silently presented as verified. Publisher/index verification establishes identity only, not theorem support.

### Identity checked; central copies retained where available

- **Perlman, M. D. (1972).** On the Strong Consistency of Approximate Maximum Likelihood Estimators. *Proceedings of the Sixth Berkeley Symposium*, vol. I, 263–281. [Berkeley primary PDF](https://digicoll.lib.berkeley.edu/nanna/record/113187/files/math_s6_v1_article-15.pdf); [publisher contents confirming title/start page](https://www.degruyterbrill.com/document/doi/10.1525/9780520325883-toc/html). No chapter DOI verified; the linked contents DOI is not represented as one. Saved: `1972-perlman-strong-consistency.pdf` (19 pages).
- **Geyer, C. J. (1994).** On the Convergence of Monte Carlo Maximum Likelihood Calculations. *JRSS B* **56**(1), 261–274. DOI [10.1111/j.2517-6161.1994.tb01976.x](https://doi.org/10.1111/j.2517-6161.1994.tb01976.x), [publisher record](https://academic.oup.com/jrsssb/article/56/1/261/7035856), [institutional journal copy](https://www2.stat.duke.edu/~scs/Courses/Stat376/Papers/GibbsFieldEst/GeyerConvergeMCMaxLik1994.pdf). Saved: `1994-geyer-monte-carlo.pdf` (15 PDF pages including cover).
- **Csiszár, I., and Matúš, F. (2005).** Closures of Exponential Families. *Annals of Probability* **33**, 582–600. DOI [10.1214/009117904000000766](https://doi.org/10.1214/009117904000000766), verified in journal reprint and [INIST index](https://pascal-francis.inist.fr/vibad/index.php?action=getRecordDetail&idt=16659587); [arXiv](https://arxiv.org/abs/math/0503653). Saved: `2005-csiszar-matus-closures.pdf` (20 pages; different pagination from journal).
- **Le Cam, L. (1990).** Maximum Likelihood: An Introduction. *International Statistical Review* **58**(2), 153–171. DOI [10.2307/1403464](https://doi.org/10.2307/1403464), [JSTOR publisher/index record](https://www.jstor.org/stable/1403464). Saved: `1990-lecam-maximum-likelihood.pdf` (32 pages), [Berkeley TR 168](https://digicoll.lib.berkeley.edu/record/85823/files/168.pdf), August 1988, revised January 1990. This is the inspected prepublication version, not the 19-page journal PDF.
- **Geyer, C. J. (2009).** Likelihood Inference in Exponential Families and Directions of Recession. *Electronic Journal of Statistics* **3**, 259–289, confirmed on [author's publication page](https://www.stat.umn.edu/geyer/gdor/); [arXiv record](https://arxiv.org/abs/0901.0455). Journal DOI **open/unverified in this search**. Saved: `2009-geyer-directions-recession.pdf` (28 pages, arXiv v1); inspect version before transferring journal locators.
- **Le Cam, L., and Schwartz, L. (1960).** A Necessary and Sufficient Condition for the Existence of Consistent Estimates. *Annals of Mathematical Statistics* **31**, 140–150. [JSTOR stable record](https://www.jstor.org/stable/2237499), [issue index used for identity](https://www.jstor.org/stable/i312801). DOI [10.1214/aoms/1177705993](https://doi.org/10.1214/aoms/1177705993) was found in [index metadata](https://www.mindat.org/reference.php?id=14534982); original full text remains wanted.
- **Rockafellar, R. T., and Wets, R. J.-B. (1998).** *Variational Analysis*. Springer, Grundlehren 317. DOI [10.1007/978-3-642-02431-3](https://doi.org/10.1007/978-3-642-02431-3), verified against publisher book record. Full text/theorem locations wanted; publisher access is restricted.

### Wanted original texts and unresolved metadata

- **Kiefer, J., and Wolfowitz, J. (1956).** Consistency of the Maximum Likelihood Estimator in the Presence of Infinitely Many Incidental Parameters. *Annals of Mathematical Statistics* **27**, 887–906. DOI candidate [10.1214/aoms/1177728066](https://doi.org/10.1214/aoms/1177728066); located in index search, publisher full text inaccessible in this run. Original PDF and publisher DOI confirmation wanted.
- **Dupačová, J., and Wets, R. J.-B. (1988).** Asymptotic Behavior of Statistical Estimators and of Optimal Solutions of Stochastic Problems. *Annals of Statistics* **16**, 1517–1549. DOI [10.1214/aos/1176351052](https://doi.org/10.1214/aos/1176351052), located in the [Czech Digital Mathematics Library reference index](https://www.dml.cz/manakin/handle/10338.dmlcz/135510). Original PDF wanted; Euclid returned an iframe shell, not inspected text.
- **Wald (1949), Bahadur (1958, 1971), Le Cam (1953, 1970), Huber (1967), Pfanzagl (1988, 1994), Wang (1985), van der Vaart (1998), Barndorff-Nielsen (1978):** wanted as specified in the coverage table. DOI verification was not completed; guessing identifiers would defeat the requested provenance standard. Access failure is not proof that these are paywalled.
- Additional high-priority lead: **Csiszár–Matúš (2008), Generalized Maximum Likelihood Estimates for Exponential Families**, *Probability Theory and Related Fields* **141**, 213–246; identified on [Matúš's institutional publication list](https://staff.utia.cas.cz/matus/). Original text and DOI wanted. The 2005 article's introduction points to earlier generalized-MLE work as well.

Secondhand retrieval leads for question 5: [Kleijn slides](https://staff.fnwi.uva.nl/b.j.k.kleijn/milano-apr18.pdf), slide 6; [Statistica Sinica article PDF](https://www3.stat.sinica.edu.tw/statistica/oldpdf/A11n410.pdf), Remark 2.3. These were surfaced as indexed passages rather than fully inspected original-theorem evidence.

The defensible positioning is therefore **a weak-closure/usc-envelope synthesis with explicit exactness and failure diagnostics**, credited to the compactification and completion literature. A claim of a new general existence principle or a new Wald theorem is premature. The open entries above specify the source text or mathematical condition still needed.
