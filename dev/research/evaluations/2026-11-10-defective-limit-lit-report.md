# Complete-data ML at defective limits — TASK-171

**Answer (2026-11-10).** A publicly available, source-verified example exists in **Geyer’s 1990 PhD dissertation, Examples 4.4–4.5**, including a wholly discrete model: for complete i.i.d. observations, the event that every observation is the origin has positive probability under every model member, and its likelihood supremum is approached only at a limit retaining mass 1/2 and losing mass 1/2 to infinity. This is a **correctly specified, finite-sample** counterexample. A dissertation is the verified publication form; **journal publication of these examples remains open**, so this search does not establish a journal-published example. No asymptotic escape-to-infinity example was verified. Le Cam’s published Bahadur construction does select a defective *density* limit asymptotically under correct specification, but the missing mass concentrates at a finite point, so it fails the question’s escaped-mass requirement. [Geyer’s source and publication history](https://www.stat.umn.edu/geyer/thesis/); [Le Cam’s article](https://www.jstor.org/stable/1403464).

## Candidates and evidence

“Verified in source text” refers to the actual version inspected, not merely a search snippet. “Cited secondhand” does not certify a candidate. Source statements and deductions made here are distinguished below.

| Candidate; precise locator | Exact model / escaping mass | Selection; specification | Grade and disposition |
|---|---|---|---|
| **Geyer (1990), §4.2, Example 4.4, equations (4.5)–(4.8)** | On the negative quadrant, dominating measure is area measure plus unit atoms at `(0,0)` and `(0,-1)`. Density is `exp(θ₁x₁+θ₂x₂)/(2+exp(-θ₂))`, with `θ₁θ₂=1`, both positive. As `θ₂→∞`, the quadrant component escapes in its first coordinate; limit is `(1/2)δ₀`. | Source proves unique completed MLE for observation zero. Finite n extension below; correct specification allowed. | **Verified in source text. Qualifies mathematically; dissertation, journal status open.** |
| **Geyer (1990), §4.2, Example 4.5** | Counting measure on `{(0,0),(0,-1)} ∪ {(-k,-l): k,l≥1}`; same density, now `(exp θ₁−1)(exp θ₂−1)=1`. Half the mass escapes along `k→∞` as `θ₂→∞`, `θ₁→0`. | Same strict boundary selection at zero; finite n, correctly specified. | **Verified in source text. Strongest example: entirely discrete, hence no ambiguity about pointwise densities or singular mass.** |
| Geyer (1990), Examples 4.2–4.3 | Predecessors omit one or both atoms. Example 4.3 retains mass 1/2 at zero. | In 4.3 every parameter already gives the same likelihood at zero; defective limits are tied, not required. In 4.2 the relevant boundary data have probability zero. | **Verified in source text. Exclude these predecessors.** |
| **Le Cam (1990), §4, “An Example of Bahadur,” journal pp. 157–159** | On `[0,1]`, `p₀=1`; `p_k=c` outside disjoint intervals `I_k=(a_k,a_{k-1}]`, and `p_k=h` inside, with `∫I_k(h-c)=1-c`, `h(x)=exp(1/x²)`. Pointwise a.e. limit is `c`, but weak limit is `c Unif[0,1]+(1-c)δ₀`. | Source proves estimated index eventually leaves every finite set under every fixed true member. Asymptotic, correctly specified. | **Verified in source text. Exclude: finite-point concentration, escaped mass zero.** Bahadur’s original is only **cited secondhand** here. |
| **Chen (2017), inspected 2016 preprint §2.2.1, p. 8; §2.2.2, p. 9** | Unrestricted mixtures `f(x;G)=∫f(x;θ)dG(θ)` compactified with subdistributions `ρG`. Under the stated continuity/tail conditions, missing mixing mass contributes zero finite-data density. | Explicitly excludes defective maximizers by replacing `ρG` with `G`. Finite-sample exclusion is independent of correct specification. | **Verified in source text. Negative result, not an example.** |
| Wald (1949); Kiefer–Wolfowitz (1956); Pfanzagl (1988); Redner (1981) | General consistency / mixture results discussed by Chen. No exact qualifying model located in original texts. | Correct-specification consistency is not a fixed-n exclusion theorem. | **Cited secondhand**, through Chen §§1–2. Original theorem/example locations remain open. |
| **White (1982), assumptions A2–A3, Theorems 2.1–2.2, pp. 3–4** | General i.i.d. model with **compact** Euclidean parameter set, continuous proper densities and an attained, unique KL minimizer. | Misspecified QMLE converges to that minimizer. These hypotheses do not supply a defective-boundary example in a nonclosed family. | **Verified in source text. Exclude as positive evidence.** Huber (1967) is **cited secondhand**, White pp. 1–2. |
| Lindsay (1983), abstract; Jiang–Zhang (2009), abstract | Mixture likelihood geometry; normal-means empirical Bayes respectively. No source-verified example of optimal leaking mass. | Neither abstract establishes the requested event or asymptotic defective selection. | **Verified in source text, abstract only**; full-text claim remains open. |
| **Whitmore (1979), summary, p. 468** | Wiener job-attachment model leading to defective inverse-Gaussian service times. For first passage to `a>0` with drift `v<0`, diffusion variance `σ²`, finite hitting mass is `exp(2av/σ²)`. This formula is a mathematical characterization here, not a verified formula locator in Whitmore. | The summary establishes use of a defective model, not selection as a limit of proper models with complete i.i.d. data. Observation scheme and likelihood need full text. | **Verified in source text, summary only**; qualifying claim **open**. |
| Cheng–Traylor (1995), summary | Four nonregular estimation problems; abstract identifies infinite spikes among the problems. Exact qualifying defective-limit model not located. | Infinite likelihood / spikes alone do not establish escaped mass. | **Verified in source text, summary only**; full-text claim **open**. |
| Censored Birnbaum–Saunders / defective Gompertz cases supplied in the brief | Already identified censored comparators. | Outside complete-data scope. | **Not independently reverified** in this pass; not counted as positive evidence. |

### Why Geyer answers the mathematical question

Use the **discrete Example 4.5** to avoid all density-version issues. Write `t=θ₂>0` and `θ₁=log(1+1/(exp(t)-1))`. The probabilities are

- `P_t(0,0)=1/(2+exp(-t))`;
- `P_t(0,-1)=exp(-t)/(2+exp(-t))`;
- `P_t(-k,-l)=exp(-kθ₁-lt)/(2+exp(-t))`, `k,l≥1`.

The double geometric sum in the last line equals `1/(2+exp(-t))`, by the parameter constraint. These are the model and normalization in Geyer, not a newly constructed family.

**Deduction extending the source’s one-observation result:** for any fixed n, on `A_n={X₁=⋯=X_n=(0,0)}`,

`L_n(t)=(2+exp(-t))^(-n) ↑ 2^(-n)`.

Every sequence approaching this supremum must have `t→∞`. Under any fixed true `t₀`, `P_t₀(A_n)=(2+exp(-t₀))^(-n)>0`. At every fixed lattice point except the origin the limiting probability is zero; the origin retains probability 1/2. The lattice component has limiting total mass 1/2 and leaves every finite set: its first index diverges while its second index concentrates at one. Thus on the one-point compactification the limit is `(1/2)δ_(0,0)+(1/2)δ_∞`. Its mass signature is **(absolutely continuous 1/2, singular 0, escaped 1/2)** relative to counting measure. Normalizing the defective density would give `δ_(0,0)`, which is unavailable in this constrained model’s closure.

Geyer proves the completed-MLE statement at zero; the fixed-n event, its probability, and the escape interpretation above are deductions from his explicit model. They are not presented as a source theorem about i.i.d. samples. This does not establish asymptotic defective selection: `P_t₀(A_n)→0`.

**Locator caution:** Geyer explicitly warns that his reconstructed PDF has different pagination from the original dissertation; cite example/equation numbers, not original pages. Examples 4.4 and 4.5 occupy printed pages 49–50 of the downloaded reconstruction (PDF leaves 62–63). His publication-history page lists later papers derived from other chapters, but does not establish a journal publication of these Chapter 4 examples.

## Question 3: a general principle?

**No source was verified stating the proposed principle in that generality.** Geyer’s example contradicts “only through misspecification or censoring” for finite samples. The restriction on the parameter curve is essential: a normalization-dominating distribution cannot be substituted within that model.

Chen §2.2.1 gives a narrower, explicit exclusion for unrestricted mixing distributions: renormalization is admissible. White’s compactness and attainment assumptions do not settle KL projections onto defective faces of nonclosed models. Correct-specification population KL separation requires an appropriate consistency/uniformity argument to control sample-dependent maximizing paths; it does not prohibit the positive-probability finite-n event above. No new proof of the background KL facts is claimed.

Le Cam also cautions against interpreting correct specification alone as a universal consistency guarantee, but his particular defective-density example concerns singular concentration, not infinity.

## Question 4: terminology

- **Geyer:** “subprobability density,” “defective,” closure of a generalized exponential family, and completed maximum likelihood. The closure can contain densities of total mass below one for nonconvex natural parameter sets.
- **Chen:** “subdistribution” and “compactification”; his terminology includes convergence of mixing distributions with lost total mass. Mixing-space escape must still be related to observation-space escape.
- **Whitmore:** “defective inverse Gaussian” in a first-passage / service-time model. Defective model membership is a different question from boundary selection within proper models.
- **Not verified as terminology in inspected passages:** “sub-stochastic,” “vague limit,” “loss of mass,” or “mass escaping.” This report uses escape descriptively and distinguishes it from finite-point singular concentration.

## Open entries and wanted texts

1. **Journal-published complete-data example:** find a journal appearance of Geyer Examples 4.4–4.5, or another explicit model satisfying all criteria. Missing evidence is a source-text locator in such a publication. A public dissertation already supplies the mathematical example.
2. **Asymptotic escaped-mass selection:** no verified model/theorem. Le Cam is excluded for finite-point concentration; this is not a proof that no example exists.
3. **Original foundational texts:** Bahadur (1958 and the 1967/1971 references), Kraft–Le Cam (1956), Huber (1967), Pfanzagl (1988/1994), Redner (1981), Lindsay (1995), and van der Vaart (1998), ch. 5 were not fully inspected. Needed: exact example or theorem, full likelihood, and identification of the lost mass. Their absence from this bounded search is not a negative finding.
4. **Wanted, full-text access unresolved:** Whitmore (1979), DOI **10.2307/2982553**; Cheng–Traylor (1995), DOI **10.1111/j.2517-6161.1995.tb02013.x**. Publisher summaries/index records were reachable; no accessible full PDF was retained. Paywall versus technical access failure was not conclusively distinguished.
5. **Chen journal identity:** preprint identity and text verified at arXiv; journal citation is *Statistical Science* 32(1), 47–63 (2017), DOI **10.1214/16-STS578**, but the publisher page yielded an iframe and Crossref returned HTTP 429. The journal DOI therefore remains incompletely independently verified in this pass. Preprint locators are used throughout.

## Bibliography and retained sources

Identity checks below concern bibliographic identity, not claim support. All access dates: 2026-11-10. DOI links are stable URLs; Crossref index records were retrieved directly unless another provider is specified.

- **Geyer, C. J. (1990).** *Likelihood and Exponential Families*. PhD dissertation, University of Washington. No DOI identified. Identity and version history verified on the [author’s institutional source page](https://www.stat.umn.edu/geyer/thesis/); [UW catalogue entry](https://stat.uw.edu/research/theses/phd/likelihood-and-exponential-families); persistent repository identifier [hdl:11299/56330](https://purl.umn.edu/56330) is linked by the author, but its target timed out. Retained: [PDF](../../../local/literature/1990-geyer-likelihood-exponential-families.pdf), 162 leaves, and matching `.provenance.json`.
- **Chen, J. (2016).** *Consistency of the MLE under mixture models*. [arXiv:1607.01251v1](https://arxiv.org/abs/1607.01251v1), DOI [10.48550/arXiv.1607.01251](https://doi.org/10.48550/arXiv.1607.01251). Identity verified at arXiv. Journal relation and unresolved verification recorded above. Retained: [PDF](../../../local/literature/2016-chen-consistency-mixture-models.pdf), 26 leaves, and provenance.
- **Le Cam, L. (1990).** “Maximum Likelihood: An Introduction.” *International Statistical Review* 58(2), 153–171. DOI [10.2307/1403464](https://doi.org/10.2307/1403464); identity verified in the journal scan’s JSTOR cover/index information, [stable record](https://www.jstor.org/stable/1403464). Retained institutional scan: [PDF](../../../local/literature/1990-lecam-maximum-likelihood.pdf), 20 leaves including cover, and provenance.
- **Wald, A. (1949).** “Note on the Consistency of the Maximum Likelihood Estimate.” *Annals of Mathematical Statistics* 20(4), 595–601. DOI [10.1214/aoms/1177729952](https://doi.org/10.1214/aoms/1177729952). Identity verified against [Crossref](https://api.crossref.org/works/10.1214/aoms/1177729952); original text not inspected.
- **Kiefer, J., and Wolfowitz, J. (1956).** “Consistency of the Maximum Likelihood Estimator in the Presence of Infinitely Many Incidental Parameters.” *Annals of Mathematical Statistics* 27(4), 887–906. DOI [10.1214/aoms/1177728066](https://doi.org/10.1214/aoms/1177728066). Identity verified against [Crossref](https://api.crossref.org/works/10.1214/aoms/1177728066); original text not inspected.
- **White, H. (1982).** “Maximum Likelihood Estimation of Misspecified Models.” *Econometrica* 50(1), 1–25. DOI [10.2307/1912526](https://doi.org/10.2307/1912526). Identity verified against the [JSTOR issue index](https://www.jstor.org/stable/i332728); relevant text read in an [institutional scan](https://faculty.utrgv.edu/diego.escobari/teaching/Econ8370/Papers/White%281982%29Econometrica-Corey.pdf). Not retained: background rather than central positive/negative example.
- **Lindsay, B. G. (1983).** “The Geometry of Mixture Likelihoods: A General Theory.” *Annals of Statistics* 11(1), 86–94. DOI [10.1214/aos/1176346059](https://doi.org/10.1214/aos/1176346059). Identity verified against [Crossref](https://api.crossref.org/works/10.1214/aos/1176346059); abstract only inspected.
- **Jiang, W., and Zhang, C.-H. (2009).** “General maximum likelihood empirical Bayes estimation of normal means.” *Annals of Statistics* 37(4), 1647–1684. [arXiv record](https://arxiv.org/abs/0908.1709); [JSTOR issue index](https://www.jstor.org/stable/i30243680). Identity verified against these index records. Journal DOI **10.1214/08-AOS638** was seen secondhand, not independently verified here; use [arXiv DOI](https://doi.org/10.48550/arXiv.0908.1709) for the verified preprint identity.
- **Whitmore, G. A. (1979).** “An Inverse Gaussian Model for Labour Turnover.” *JRSS A (General)* 142(4), 468–478. DOI [10.2307/2982553](https://doi.org/10.2307/2982553); [JSTOR](https://www.jstor.org/stable/2982553). Identity verified against Crossref and journal summary; Crossref records only first page 468, scan summary supplies 468–478. Wanted full text.
- **Cheng, R. C. H., and Traylor, L. (1995).** “Non-Regular Maximum Likelihood Problems.” *JRSS B (Methodological)* 57(1), 3–24. DOI [10.1111/j.2517-6161.1995.tb02013.x](https://doi.org/10.1111/j.2517-6161.1995.tb02013.x). Identity verified against [publisher record/summary](https://academic.oup.com/jrsssb/article/57/1/3/7035899). Wanted full text.

## Search scope and files

Research reconnaissance, not a formal workflow audit. Approximately 20 search queries; roughly two dozen retrieval attempts including failed publisher pages, PDF downloads and bibliographic index checks. Primary journal/index/arXiv and institutional sources were prioritized; broad search results were used to discover leads, not certify claims. Search families covered Chen/mixture compactification, Le Cam/Bahadur, Wald/defective densities, mass escape/subprobabilities, Geyer’s examples and publication history, Whitmore/first passage, White/misspecification, Lindsay and Jiang–Zhang, and Cheng–Traylor. Stopping rule: the requested search budget, with uninspected leads explicitly open. This is not exhaustive literature coverage.

Three central PDFs were retained with SHA-256, byte count, page count, source URL, retrieval timestamp and TASK-171 provenance. No source code, board records, commits or branches were changed.
