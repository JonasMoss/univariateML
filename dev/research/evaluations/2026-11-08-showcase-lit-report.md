# Showcase literature check — TASK-158

Completed 2026-11-08 in `codex/showcase-lit`. This bounded search covered all 13 table rows initially labelled “Not searched” or “Open after search”, plus the requested beta-prime/lognormal comparison. Exactly **40 search queries** were issued, followed by direct source retrievals. No exact published equivalent of the priority threshold or the two exact global ties was located. These are search outcomes, never claims of novelty.

Only Literature cells and the reference list of [showcase.md](../showcase.md) were edited. No mathematical finding, Status, source link, code, default fit or golden changed. The beta-prime/lognormal claim has no row in the supplied showcase; it is assessed here without adding a finding outside the authorized edit surface. No commits, board changes, sub-agents, installs or package tests were made.

## Method and evidence limits

This is a completed research reconnaissance under the requested search budget, not a formal frozen-bundle workflow audit. The audit-literature skill's distinction between identity, support and coverage informed the assessment. Searches used the web search provider; direct article opens, DOI resolution and section retrieval were follow-ups, not additional searches. Primary text was preferred; software documentation is labelled as such. Abstracts and inaccessible articles do not establish a theorem's absence. “Partly known” means a related component is located, not that the whole finding is published.

The required project guidance was read before the search. The previous [literature report](2026-11-07-lit-check-report.md) and [ZTNB source ledger](2026-11-07-ztnb-conjecture-data/literature.json) supplied earlier reading limits, explicitly distinguished below from fresh readings.

## Priority findings

**Fréchet, shape held — not located after search.** [Bücher and Segers (2016), *Maximum likelihood estimation for the Fréchet distribution based on block maxima extracted from a time series*, arXiv:1511.07613v2](https://arxiv.org/pdf/1511.07613), §2.1 Lemma 2.1 (PDF pp. 3–4) and its Appendix A proof (starting PDF p. 14), were read. They prove existence and uniqueness for nonconstant positive data with location fixed and shape/scale free. This does not settle location/scale free with shape fixed, minimum multiplicity, or the equality case. [Ramos, Louzada, Ramos and Dey (2020; online 2019), *The Fréchet distribution: Estimation and application—an overview*, Journal of Statistics and Management Systems 23, 549–578](https://doi.org/10.1080/09720510.2019.1645400), [2018 manuscript](https://arxiv.org/pdf/1801.05327), §2.1 Theorem 2.1 and Appendix A were inspected: again a two-parameter shape/scale problem, not the showcased held-shape threshold.

The suggested nonregular-likelihood leads were pursued: Smith (1985), *Maximum likelihood estimation in a class of nonregular cases*, Biometrika 72, 67–90, [DOI](https://doi.org/10.1093/biomet/72.1.67); Cheng and Iles (1987), *Corrected maximum likelihood in non-regular problems*, JRSS B 49, 95–101, [DOI](https://doi.org/10.1111/j.2517-6161.1987.tb01430.x); Hall and Wang (2005), *Bayesian likelihood methods for estimating the end point of a distribution*, [DOI](https://doi.org/10.1111/j.1467-9868.2005.00523.x), [institutional record](https://hdl.handle.net/1885/85232). Only abstracts/summary or metadata were accessible; no theorem section was read. **Open:** obtain their full texts and compare ordinary likelihood, held coordinates, ties and endpoint treatment. Weibull endpoint singularities or modified likelihoods are not automatically equivalent to this Fréchet result. The exact criterion, uniqueness and finite unattained equality supremum were not located within this search.

**SGED constant-sample scale tie — not located after search.** Schoups and Vrugt (2010), *A formal likelihood function for parameter and predictive inference of hydrologic models with correlated, heteroscedastic, and non-Gaussian errors*, Water Resources Research 46, W10531, [DOI/full text](https://doi.org/10.1029/2009WR008933), Appendix A, paragraphs 51–52 and the text surrounding (A1)–(A10), was read. It constructs the standardized Fernández–Steel skew exponential-power law. However, its stated kurtosis range corresponds to powers at least one and excludes the showcased power 1/2. It gives no constant-sample separated global scale tie. The HTML equations are images; this was a reading of the construction and range discussion, not an independent transcription of every displayed formula. Bottazzi–Secchi's earlier EP working-paper URL timed out in this session. **Open:** full-text comparison in subunit-power skew-GED treatments; neither generic skewness nor density modality settles likelihood non-uniqueness.

**Beta-prime held-shape tie — not located after search.** Tulupyev, Suvorova, Sousa and Zelterman (2013), *Beta prime regression with application to risky behavior frequency screening*, Statistics in Medicine 32, 4044–4056, [DOI](https://doi.org/10.1002/sim.5820), [primary text](https://pmc.ncbi.nlm.nih.gov/articles/PMC3789864/), §2 equation (5) and Appendix Lemma 1/proof, were read after an initial CAPTCHA cleared. Their density fixes the numerator shape at 2. The lemma discusses critical points, not arbitrary held-shape uniqueness or an equal-height global tie. Its conditional score arguments cannot establish the project's continuity-based two-global-maxima result. **Open:** other arbitrary-shape beta-prime/GB2 profile treatments; no matching example located.

**Beta-prime versus GB2 lognormal limit — partly known; exclusion deduced, explicit statement not located.** The [SciPy developers' Beta Prime Distribution guide](https://docs.scipy.org/doc/scipy/tutorial/stats/continuous_betaprime.html), density and moment display, and NIST's *DLMF* §5.15 [equation (5.15.8)](https://dlmf.nist.gov/5.15.E8) were read. Differentiating the beta-prime Mellin transform at zero gives, for deterministic scale s,

$$K(t)=t\log s+\log\Gamma(p+t)+\log\Gamma(q-t)-\log\Gamma(p)-\log\Gamma(q),\qquad K''(0)=\psi'(p)+\psi'(q).$$

Thus log variance tends to zero as both shapes diverge. This also excludes a nondegenerate lognormal weak limit when scale varies: if Z and an independent copy Z' denote the log variables, then Var(Z−Z') tends to zero and E(Z−Z')=0. Chebyshev gives Z−Z' tending to zero in probability; a nondegenerate Gaussian log-limit would instead give a difference with positive variance. This avoids relying on convergence of variances under weak convergence.

For GB2 with power a, log variance is the same numerator divided by a squared. A varying power can preserve spread. McDonald (1984), *Some generalized functions for the size distribution of income*, Econometrica 52, 647–663, [stable bibliographic record](https://ideas.repec.org/a/ecm/emetrp/v52y1984i3p647-63.html), was located, but its original limiting derivation was not accessible. The GB2/lognormal inclusion is standard background; no claim is made to have read McDonald's theorem. **Open:** locate a published explicit exclusion for ordinary scaled beta-prime with both shapes growing, rather than infer it from standard formulas. Standardized log-beta-prime normal limits do not contradict the exclusion: standardization introduces a varying power after exponentiation.

## Other unresolved table rows

Citations in this table refer to the full entries here or in the showcase reference list. Each row records both the inspected location and the missing comparison.

| Finding | Reading and result | Verdict / precise open item |
|---|---|---|
| PIG overdispersion criterion and Bessel-weight inequality | Willmot (1987), *The Poisson-inverse Gaussian distribution as an alternative to the negative binomial*, Scandinavian Actuarial Journal, 113–127, [DOI](https://doi.org/10.1080/03461238.1987.10413823): earlier metadata only. Carlson (2002), *Assessing microdata disclosure risk using the Poisson-inverse Gaussian distribution*, Statistics in Transition 5, 901–925, [institutional PDF](https://www.diva-portal.org/smash/get/diva2%3A666050/FULLTEXT01.pdf), repeatedly timed out. Its search abstract concerns disclosure uniqueness, which must not be confused with uniqueness of ML. | **Not located after search.** Open: read ML sections of these sources and compare strict profile concavity, the polynomial-weight bound and the held-mean criterion. No theorem absence inferred from failed access. |
| ZTNB, probability held | Sampford (1955), *The truncated negative binomial distribution*, Biometrika 42, 58–69, [DOI](https://doi.org/10.1093/biomet/42.1-2.58). Earlier ledger records primary pp. 65–66, §8, on joint endpoint conditions and conjectures. Fresh publisher access was metadata only; the ledger's scan URL also failed. | **Not located after search.** Open: a held-probability size strict-concavity theorem; a conjecture about the joint two-parameter problem does not settle it. |
| ZINB, inflation free, one base parameter held | Haslett et al. (2022), *Modelling excess zeros in count data: a new perspective on modelling approaches*, [DOI](https://doi.org/10.1111/insr.12479), [2021 preprint §3.2, equations (4)–(6)](https://arxiv.org/html/2007.04177v2), read after PDF retrieval failed. Gives zero/nonzero separation. | **Partly known.** Open: exact held-size and held-probability uniqueness with nonnegative-inflation feasibility, including transitions to the base-law face. Separation alone is insufficient. |
| ZINB/ZIP, inflation held, probability multimodality | Ridout, Demétrio and Hinde (1998), *Models for count data with many zeros*, International Biometric Conference, Cape Town, [author PDF](https://www.kent.ac.uk/IMS/personal/msr/webfiles/zip/ibc_fin.pdf), §3 model fitting and §4 inferential aspects read; §1 mechanism was already read in the earlier check. | **Partly known.** Open: fixed-inflation three-root bound and exact crossover/global tie; general structural/sampling-zero ambiguity and numerical fitting are established. |
| One-observation paralogistic bimodality | Delignette Muller and Dutang, *Starting values used in fitdistrplus*, version 1.2-6, [§2.5.5](https://lbbe-software.github.io/fitdistrplus/articles/starting-values.html), likelihood, gradients and initialization discussion read. Author-maintained software documentation, not a journal theorem. | **Not located after search.** Open: a fixed-scale one-observation modality theorem/example in actuarial primary literature. |
| Half-t degrees-of-freedom multimodality | Tsagris, *foldedt reference manual*, [halft.mle and halft1.mle entries](https://mirror.fem-net.de/cran/web/packages/foldedt/refman/foldedt.html), search-extracted entries inspected; direct open timed out. They describe numerical fitting, with a citation to Psarakis and Panaretos (1990), not a mode-count theorem. | **Not located after search.** Open: full original half-t/folded-t ML treatment. On positive data half-t and zero-location t log likelihoods differ by n log 2; such t profile examples would count as equivalents. No published equivalent located. The scanned sample fraction remains numerical. |
| Held-skew skew-t scale modes | Schoups–Vrugt Appendix A supplies related standardization context; the searches found skew-density modality and mixtures, not the specified conditional likelihood. | **Not located after search.** Open: Fernández–Steel t likelihood with mean held (rather than raw mode held), fixed degrees of freedom and skew. Density multimodality is irrelevant to this comparison. |
| Certified near-ZIP finite ZINB maximum | Ridout et al. §§3–4 discusses slow fitting and the ZIP/ZINB score-test connection. | **Not located after search.** Open: an equivalent finite-sample certified near-face counterexample and the numerical face-tolerance failure, not merely general score-test theory. |
| Held-moment endpoint-density penalty | Schoups–Vrugt Appendix A explains standardization but does not establish the specified constrained half-t endpoint penalties. Searches found no equivalent statement. | **Not located after search.** Open: pointwise endpoint likelihood along held-mean/sd paths for the two skew-t constructions. Weak distributional convergence does not answer it. |
| Standardized-t zero-slope three-point collapse | General t/standardized-t searches returned fitting and moment discussions; no matching primary theorem was located or read. Fernández–Steel (1999), Biometrika 86, 153–167, [DOI](https://doi.org/10.1093/biomet/86.1.153), remains earlier abstract-level context only. | **Not located after search.** Open: the exact zero-first-order, positive-second-order seam calculation together with monotone collapse for this sample. No full-text support asserted. |

These open entries are the outcome of the completed bounded search. They identify missing source comparisons, not unfinished computations or a claim that the mathematical results are unpublished.

## Exact search log

Queries are listed in issuance order. Each line counts as one search, including queries batched in the same tool call. General web results and secondary records were discovery aids; only the explicitly identified primary passages/documentation support the assessments above.

```text
01 Frechet distribution maximum likelihood known shape location scale existence uniqueness Cheng Hall Wang
02 three parameter Frechet maximum likelihood existence sample minimum multiplicity
03 skew generalized error distribution maximum likelihood multiple maxima constant sample
04 beta prime distribution maximum likelihood multiple maxima fixed shape lognormal limit
05 "Frechet" "known shape" "location" likelihood
06 "Cheng" "Wang" "likelihood" "threshold" Weibull
07 "skewed generalized error" likelihood uniqueness
08 "generalized beta" "lognormal" "McDonald" 1984
09 "Frechet" "likelihood" "Cheng" estimation three
10 "Weibull" "Hall" "Wang" likelihood three parameter
11 "Frechet" "likelihood" "1/(n-1)"
12 "beta prime" "likelihood" "uniqueness" scale
13 "beta prime" "multiple" "likelihood" maxima
14 "skew" "exponential power" "likelihood" "multiple" maxima
15 "Poisson inverse Gaussian" "likelihood" "unique" maximum
16 "zero truncated negative binomial" "concavity" probability
17 "paralogistic" "likelihood" maxima
18 "half t" "likelihood" "multimodal"
19 "zero inflated negative binomial" "likelihood" "unique" maximum
20 "Poisson-inverse Gaussian" "uniqueness" estimation
21 "Student" "degrees of freedom" "likelihood" "multiple maxima"
22 "skew t" "scale" "multimodal" likelihood Fernandez Steel
23 "skew" "half-t" "limit" likelihood endpoint
24 "standardized t" "likelihood" "kurtosis" maximum
25 "Cheng" "Iles" "1987" likelihood estimation
26 "beta prime regression" "2013" "Bourguignon"
27 "generalized beta" "McDonald" "1984" pdf
28 "Sampford" "1955" truncated negative binomial pdf
29 "Beta Prime Regression with Application" authors doi
30 "Frechet" "known" "shape" "existence" location
31 "beta prime" "lognormal" "trigamma"
32 "zero inflated" "likelihood" "multiple maxima" fixed
33 "Frechet" "maximum likelihood" "threshold" uniqueness shape known
34 "beta prime" "likelihood" "non-uniqueness"
35 "SGED" "maximum likelihood" "multiple"
36 "zero-inflated" "near" "Poisson" "likelihood" numerical Ridout
37 "Modelling excess zeros" Haslett Parnell pdf
38 "half-t" "degrees of freedom" "likelihood" maxima
39 "skew t" "endpoint" "held" likelihood
40 "standardized" "t" "likelihood" "collapse"
```

## Validation

The requested workstation link checker was run through `/tmp/umvl-cpu/run` with temporary/runtime directories under `build/tmp`. It checks local Markdown destinations, not remote URLs or section fragments. The final command finished with exit 1: 550 Markdown files, 3,223 local destinations, six pre-existing broken destinations outside the authorized edit surface. Package builds/tests, no-long-double, JS and site suites are outside this prose-only task's explicitly requested checks.


The six repository-wide findings are:

- `2026-10-31-inference-6-report.md`: two absent scratch screenshots (`desktop.png`, `mobile-evidence.png`).
- `2026-11-03-boundary-lit-report.md`: absent `boundary-points.pdf`.
- `nbinom/README.md`: `1+m/r` parsed as a local link destination.
- `univariateML-cpp/notes/inference.md`: two `/inference/` destinations.

**Open validation issue:** the repository-wide checker cannot be green within this task's two-file edit restriction. No unrelated repair was made. A separate check using the same destination parser passed for both edited documents: 45 showcase destinations and three report destinations, zero broken. The first checker invocation ran before this report existed and therefore also reported its temporarily missing destination; the final invocation above supersedes it.

`git diff --check` passed. A read-only comparison against HEAD passed: exactly 13 Literature cells changed, with every other pre-reference line/cell preserved. Final tracked/untracked scope is the requested showcase and report only. No command remains running.

The required artifact-pane display was attempted with `codex-math-web/bin/show`; the viewer rejected this worktree with HTTP 403 (outside its configured readable roots). The report remains available at the requested path; no viewer settings or other worktrees were changed.
