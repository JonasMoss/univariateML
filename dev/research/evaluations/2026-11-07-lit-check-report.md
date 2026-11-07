# Literature status of the showcase findings — TASK-145

Completed 2026-11-07 against source revision `681d7ac5ce6965db4066b4117c12eff69e75cd0c`, branch `codex/lit-check`. The bounded search covered all 23 rows in showcase sections 2–5 and all four rows in section 6. All 14 original bibliography entries were checked. Only the Literature cells, section 6's existing Literature paragraph, and the reference list in [showcase.md](../showcase.md) were edited; this report is the other authored change. No implementation, findings, proof-status labels, goldens, dossiers, roadmap, board or Git history was changed. No installation or sub-agent was used.

The important attribution changes are that the GPD scalar reduction is already published, two-piece Fisher information is established, and beta-negative-binomial parameter-swap non-identifiability is explicitly documented. Burr existence/uniqueness has direct prior literature and should not be advertised as new without comparing its theorems. The remaining specific counterexamples, certificates and inequalities are candidates for further priority checking, not established novelties.

## Search and evidence standard

This was targeted literature reconnaissance with separate bibliographic identity checks and source comparisons, following the audit-literature skill's distinction between identity, support and coverage. It was not a frozen paper-bundle workflow audit or a systematic review. The user explicitly authorized bibliography edits. No formal workflow runner or model-assisted support contract was used.

The search provider was the web search tool; there were **32 search queries**, followed by direct publisher, institutional-repository, arXiv and DOI opens. Public bibliographic metadata were also requested directly from Crossref's `/works/<doi>` API. Retrieval date was 2026-11-07. Sources included journal articles, author manuscripts, conference papers and software documentation; no publication-year restriction was imposed. Software pages were discovery/context sources, not substitutes for a theorem. The stopping rule was the approximately 30-query budget, direct follow-up on promising sources, and an explicit outcome for every showcase row.

“Read” below means the named theorem statement, equation or passage was inspected, not that every proof in the article was audited. “Abstract” and “metadata” do not establish an exact theorem. “Open” means an identified comparison or source remains missing after this search. Failed PDF access is not evidence that a claim is unsupported. The showcase's mathematical claims and internal numerical certificates were not independently reproved or rerun by this literature job.

## Row-by-row results

Row numbers below count data rows within each section.

| Row | Located prior work and reading depth | Contribution assessment and precise open issue |
|---|---|---|
| 2.1 PIG existence, uniqueness and Bessel inequality | [Willmot (1987)](https://doi.org/10.1080/03461238.1987.10413823): publisher identity and pages verified; full-text request unsuccessful. PIG estimation literature was found, but no matching theorem was read. The separate Bessel-polynomial query did not locate the displayed weighted-variance bound. | **Open.** Obtain the relevant PIG existence/uniqueness results, including Sichel's estimation literature, and compare free-mean and held-mean hypotheses. The compact-coordinate concavity proof, exact held-mean criterion and polynomial bound may add something; this search does not establish priority. |
| 2.2 Kumaraswamy | [Jones (2009)](https://doi.org/10.1016/j.stamet.2008.04.001): indexed publisher likelihood-inference preview gives the conditional shape estimate; direct full text was blocked. | The scalar profiling step is known. **Open:** compare a full theorem for strict profile concavity and existence/uniqueness for every nonconstant interior sample. Censored competing-risk papers also surfaced but were not treated as proofs for the uncensored two-shape problem. |
| 2.3 Burr XII, scale held | [Wingo (1983)](https://doi.org/10.1002/bimj.19830250109) and [Wingo (1993)](https://doi.org/10.1016/0026-2714(93)90126-J): abstracts explicitly cover conditional modality and existence/uniqueness. PDF requests failed. | **Partly known; substantial priority risk.** Compare their exact sample conditions, fixed-scale parameterization and conclusions with the project's strict log-convexity argument. The certificate may add an implementation contract even if uniqueness is classical. No claim that the concavity proof is sharper is justified yet. |
| 2.4 ZTNB, probability held | [Sampford (1955)](https://doi.org/10.1093/biomet/42.1-2.58): identity verified, original theorem inaccessible. Search also found numerical fitting discussions, which do not settle concavity. | **Open:** read a fixed-probability size-profile theorem. Strict concavity could be a useful proof contribution, but the age of truncated-NB ML work makes a novelty claim premature. |
| 2.5 ZINB, inflation free and one base parameter held | Zero-inflation sources were located, including [Ridout, Demétrio and Hinde (1998)](https://www.kent.ac.uk/IMS/personal/msr/webfiles/zip/ibc_fin.pdf). Its opening discussion was read; no matching uniqueness theorem was found. | **Open:** compare positive-data assumptions and whether inflation is constrained to the ordinary nonnegative mixture range. The exact held subsets, rather than zero-inflation modelling itself, are the possible addition. |
| 2.6 GPD joint profile | [Grimshaw (1993)](https://www.stat.cmu.edu/technometrics/90-00/vol-35-02/v3502185.pdf), §2, equation (2.1), root theorem and boundary comparison were read. | **Known reduction.** With Grimshaw's ratio coordinate equal to `-b/M`, its conditional shape is `-bD`; substituting in (2.1) gives the showcased profile. The project adds a seam-safe integral representation and certification, not the scalar reduction. Grimshaw itself credits Davison (1984) for the reduction; that earlier original was not checked. |
| 3.1 GPD interior/uniform global tie | Grimshaw's same section explicitly treats multiple profile roots and the uniform boundary as a competitor. | **Partly known.** The exact constructed equality of two global maxima was not located. Its possible addition is the explicit tie construction, rather than multiple roots or the need for a boundary comparison. **Open:** find or exclude an earlier exact global-tie example in this restricted domain. |
| 3.2 Held-inflation ZINB/ZIP multimodality | Ridout, Demétrio and Hinde (1998), §1, distinguishes structural and sampling zeros. | The mechanism is known. **Open:** no matching fixed-inflation three-root bound or crossover sample was located. Those analytical branch counts and a reproducible equal-height example may add something; general mixture ambiguity alone does not. |
| 3.3 One-observation paralogistic bimodality | The [fitdistrplus authors' starting-values article](https://lbbe-software.github.io/fitdistrplus/articles/starting-values.html), §2.5.5, displays this family's likelihood and derivatives. This is software documentation, not a modality theorem. | **Open:** no corresponding one-observation, fixed-scale bimodality result was located. The certified example is potentially useful evidence against assuming unimodality. Compare actuarial distribution treatments before claiming priority. |
| 3.4 Half-t degrees-of-freedom modes | General Student-t likelihood difficulties were found; no matching half-t profile result was located. | **Open:** search the fixed-endpoint/estimated-scale half-t problem specifically in full-text treatments. The scanned fraction of multimodal samples is this project's numerical observation, not a population theorem. |
| 3.5 Location-scale t interior modes | [Fernández and Steel (1999)](https://academic.oup.com/biomet/article-abstract/86/1/153/255235), abstract read, addresses boundary unboundedness. | That abstract does not establish the two interior degrees-of-freedom modes in the project's restricted parameter domain. **Open:** compare exact free subsets and admissible degrees of freedom with earlier profile examples. The certified global comparisons remain possible example/algorithm contributions. |
| 3.6 Cauchy location roots | [Reeds (1985), Bell Labs author abstract](https://www.nokia.com/bell-labs/publications-and-media/publications/asymptotic-number-of-roots-of-cauchy-location-likelihood-equations/) explicitly discusses multiple local maxima; journal identity checked separately. | **Known.** The project supplies a reproduction, not a new phenomenon. The asymptotic root-count theorem itself was not read. |
| 3.7 BNB labeled maxima and scalar uniqueness | [Ueki (2023), §2.2](https://pmc.ncbi.nlm.nih.gov/articles/PMC10184601/pdf/CJAS_50_2064439.pdf), explicitly states the parameter-swap non-identifiability; passage read. | **Partly known.** Swap symmetry is not new. Attainment, certified diagonal exclusion for the concrete sample, and the scalar single-crossing theorem were not located elsewhere. **Open:** their priority. A bijective change to mean coordinates alone cannot remove the original symmetry; this report does not adopt the source's suggested cure without extra restrictions. |
| 3.8 Held-skew t scale roots | Skew-t distribution literature surfaced, but no matching fixed-skew, fixed-mean, fixed-degrees-of-freedom scale-mode theorem or example was located. | **Open:** compare standardized mean/sd coordinates with the mode/raw-scale coordinates of earlier two-piece t papers. The numerical counterexample may be new evidence; its globality is not upgraded by this search. |
| 4.1 ZINB versus ZIP score | [Ridout, Hinde and Demétrio (2001)](https://doi.org/10.1111/j.0006-341X.2001.00219.x): title, authors and abstract establish the score-test topic; full theorem/formula text was not accessible. | **Partly known.** **Open:** compare nuisance projection, coordinates, normalization and the exact expansion. A local score test supplies no global “if and only if” MLE-existence theorem by itself. The project must retain its separate global comparisons. |
| 4.2 Near-ZIP finite-maximum sample | The preceding search found the testing literature, not this count configuration or historical software failure. | **Open priority for the sample**, with a concrete software-validation contribution already identifiable: a proved finite solution that a historical fitter called a limit. The unresolved location of the maximizer is an existing numerical gap, not something a bibliography search resolves. |
| 4.3 Constrained skew-t endpoint penalties | Search targeted skew limits, held parameters and endpoint likelihoods. No matching penalty theorem was located. | **Open:** inspect full treatments of both Fernández–Steel and Azzalini skew-t closures with held moments/location. The possible addition is the attainable pointwise endpoint likelihood under constraints. Weak convergence to a half-t is a different assertion. |
| 4.4 Standardized-t zero-slope collapse | Search targeted normal-boundary likelihood/kurtosis and degrees-of-freedom profiles; no matching three-point result was found. | **Open:** the exact zero-first-order, positive-second-order sample and monotone collapse comparison may be an explicit counterexample contribution. General t boundary difficulties are insufficient attribution for this statement. |
| 5.1 Two skew-normals at symmetry | [Hallin and Ley (2012)](https://arxiv.org/abs/1207.0282), §1 and §2.1 singularity criterion read; [Rubio and Steel (2014)](https://wrap.warwick.ac.uk/id/eprint/63271/1/WRAP-inference-two-piece-models-Rubio-2014.pdf), Theorems 1–3 and equation (12) read. | Azzalini singularity and nonsingular two-piece information are established. Moving from mode/raw-scale to standardized mean/sd coordinates explains the showcased residual skew information. It is a useful coordinate specialization, not a new regular family. Exact historical priority for that printed constant is **open**. |
| 5.2 GED thresholds | [Bottazzi and Secchi, 4 December 2008 version](https://www.lem.santannapisa.it/WPLem/files/2006-19.pdf), Theorems 2.1–2.2 and information-domain discussion read. [Dachian et al., §1.1 Remark 1](https://arxiv.org/html/1711.03740), also identifies classical generalized-Gaussian cusp work. | The finite-information threshold is established. **Open:** earlier statements of the exact critical `1/16` versus `1/8` constants and the fitted-Hessian result. These must remain separate from DQM and from ordinary expected-Hessian identities. The original Daniels/Prakasa Rao proofs were not checked here. |
| 5.3 Moving joins | Rubio and Steel's theorems cover two-piece information and nonsingularity. Their base-density assumptions must be checked; the result is not an all-procedure regularity theorem. | **Partly known.** **Open:** locate an exact DQM/fitted-Hessian result encompassing the project's full snorm, sstd and Huber scopes. Positive information alone does not prove the fitted-Hessian claim. |
| 5.4 Interior singular sets | Ueki's published BNB symmetry implies equality of the two shape scores on the diagonal. Generalized-beta/Tukey-lambda searches did not locate the stated singular sets. | BNB diagonal rank loss follows from a known symmetry. **Open:** literature priority for genbeta at second shape one and Tukey lambda at one/two, including whether the same location-scale-shape coordinates are free. |
| 5.5 Endpoint coordinates and rates | [Smith (1985)](https://academic.oup.com/biomet/article-abstract/72/1/67/242523), abstract only, includes nuisance parameters. [Bücher and Segers (2017)](https://arxiv.org/html/1601.05702), §1, Proposition 3.2 and Remark 3.4 read, distinguishes regular GEV theory and limitations of earlier proofs. | The general endpoint-versus-nuisance distinction should not be claimed new. **Open:** a theorem-level comparison for the precise GEV/GPD coordinate split and Tukey specialization, especially critical-rate claims. A density/local-experiment calculation is not automatically an MLE limit theorem. |
| 6.1 Ordinary Gaussian rays | [Self and Liang (1987)](https://doi.org/10.1080/01621459.1987.10478472): metadata checked; direct theorem text inaccessible. Related beta-binomial testing papers surfaced. | The boundary-mixture principle is established. **Open:** verification of all family-specific differentiability, localization and integer-size mesh hypotheses. The same mixture alone is not a new result. |
| 6.2 Weibull versus Burr | Burr and boundary-theory searches did not locate the efficient-information constant. | **Open:** exact literature priority for the nuisance-adjusted constant. The mixture follows established geometry once the one-ray experiment is proved. |
| 6.3 BB Poisson corner | The beta-binomial/Poisson boundary query found ordinary overdispersion tests, not this free-size/free-shapes tangent geometry. | **Open:** prior derivation of the half-plane at this corner, including lattice-size localization. The stated chi-square mixture is the standard consequence of a Gaussian half-plane, not the novel part. |
| 6.4 Pareto versus Burr | Smith supplies related nonregular endpoint theory at abstract level; no matching logistic-intensity point-process likelihood supremum was located. | **Open:** compare a full Burr-to-Pareto endpoint-smoothing experiment with fitted endpoint and tail nuisance. A point-process endpoint limit in general does not establish this particular alternative experiment or its LR supremum. |

## Bibliographic verification

Every original entry's **authors, publication year, title, journal, volume and page range** matched the records below, allowing capitalization, journal abbreviation and title punctuation. There was **no substantive wrong entry among the original 14**. Hallin–Ley's title separator was normalized from a colon to the published dash; source links were added throughout.

| Original entry | Verified journal, volume, pages | Identity evidence and reading limit |
|---|---|---|
| Anscombe, F. J. (1950) | Biometrika 37, 358–382 | [Publisher](https://academic.oup.com/biomet/article-abstract/37/3-4/358/176460), DOI metadata. Metadata only for this check. |
| Aragón, J.; Eberly, D.; Eberly, S. (1992) | Statistics & Probability Letters 15, 375–379 | [Publisher](https://www.sciencedirect.com/science/article/pii/016771529290157Z), DOI metadata. Abstract explicitly announces proof of the NB conjecture. |
| Azzalini, A. (1985) | Scandinavian Journal of Statistics 12, 171–178 | [Author's university record](https://www.research.unipd.it/handle/11577/142107?mode=full), all fields. Original theorem not read. No DOI was guessed. |
| Fernández, C.; Steel, M. F. J. (1999) | Biometrika 86, 153–167 | [Publisher](https://academic.oup.com/biomet/article-abstract/86/1/153/255235), abstract and author line. Crossref omits Steel; university metadata gives 153–168. Retained publisher's authors/pages. |
| Grimshaw, S. D. (1993) | Technometrics 35, 185–191 | Original article and DOI metadata; named passages read above. |
| Hallin, M.; Ley, C. (2012) | Bernoulli 18, 747–763 | Article first page and DOI metadata. |
| Levin, B.; Reeds, J. (1977) | Annals of Statistics 5, 79–87 | [Journal archive record](https://www.jstor.org/stable/2958762), DOI `10.1214/aos/1176343741`. Full theorem not read. Its exact NB corollary was not verified merely from this title. |
| Reeds, J. A. (1985) | Annals of Statistics 13, 775–784 | DOI `10.1214/aos/1176349554`, author abstract. A direct guessed URL ending in `9555` failed; the correct DOI was then resolved from the title. |
| Ridout, M.; Hinde, J.; Demétrio, C. G. B. (2001) | Biometrics 57, 219–223 | DOI metadata and abstract. The author order differs from the 1998 conference paper; both were retained correctly. |
| Sampford, M. R. (1955) | Biometrika 42, 58–69 | [Publisher](https://academic.oup.com/biomet/article-abstract/42/1-2/58/241168), DOI metadata. Metadata only. |
| Self, S. G.; Liang, K.-Y. (1987) | Journal of the American Statistical Association 82, 605–610 | DOI metadata verifies all fields. Direct publisher and candidate manuscript fetches unsuccessful; theorem not read. |
| Smith, R. L. (1985) | Biometrika 72, 67–90 | Publisher identity and abstract, DOI metadata. |
| Willmot, G. E. (1987) | Scandinavian Actuarial Journal 1987, 113–127 | Publisher and DOI metadata. Online digitization date 2011 does not replace publication year 1987. |
| Wingo, D. R. (1983) | Biometrical Journal 25, 77–84 | Publisher abstract and DOI metadata. |

Seven sources were added to make the revised Literature entries traceable:

- Bücher–Segers (2017), *Extremes* 20, 839–872: authors/title/year/volume/pages verified through DOI `10.1007/s10687-017-0292-6`.
- Bottazzi–Secchi: the version actually read is dated **4 December 2008**, hosted as LEM Working Paper **2006/19**. It is identified as a working-paper version, not silently given a journal/year/pages from a later publication.
- Jones (2009), *Statistical Methodology* 6, 70–81: all fields verified through DOI `10.1016/j.stamet.2008.04.001`.
- Ridout–Demétrio–Hinde (1998): author-hosted conference paper verifies authors, title, event and year. **Open bibliographic field:** proceedings pagination was not verified and is explicitly omitted/flagged in showcase.
- Rubio–Steel (2014), *Bayesian Analysis* 9, 1–22: all fields verified through DOI `10.1214/13-BA849` and institutional manuscript.
- Ueki (2023 print; 2022 online), *Journal of Applied Statistics* 50, 1650–1663: all fields verified against the printed article and DOI metadata; the different online and print years are explicitly recorded.
- Wingo (1993), *Microelectronics Reliability* 33, 1251–1257: all fields verified through publisher and DOI metadata. There is also a distinct 1993 Wingo paper in *Metrika* 40, 203–210, on progressive censoring; a bare “Wingo 1993” is ambiguous. The showcase now gives the exact selected work.

The NB context paragraph was outside the editable surface and was left unchanged. The Aragón abstract provides direct support for the classical uniqueness statement; metadata verification of the other two NB citations is not a fresh reading of their proofs.

## Search record

These are the exact 32 search strings, in order. Direct opens and DOI metadata requests were follow-up retrievals, not extra search queries.

```text
1. Poisson inverse Gaussian maximum likelihood existence uniqueness overdispersion Willmot 1987
2. Kumaraswamy distribution maximum likelihood uniqueness profile concavity
3. Wingo 1983 1993 Burr XII maximum likelihood existence uniqueness
4. Sampford 1955 truncated negative binomial distribution 42 58 69
5. Grimshaw 1993 generalized Pareto maximum likelihood multiple roots profile 185 191
6. zero inflated negative binomial likelihood uniqueness fixed probability size multiple maxima Ridout Hinde Demetrio 2001
7. paralogistic distribution likelihood multiple maxima shape
8. Student t degrees freedom profile likelihood multimodal Fernandez Steel 1999 half t
9. "beta negative binomial" symmetry likelihood identifiability
10. "skew t" "scale" "multiple" likelihood Fernandez Steel
11. "skew" "half" "likelihood" limit endpoint density held
12. Student t normal boundary likelihood kurtosis degrees freedom maximum likelihood multiple maxima
13. Hallin Ley 2012 skew symmetric Fisher information tale two densities 747 763 Azzalini 1985
14. two piece normal Fisher information orthogonal parameters 3 8 pi regularity Rubio Steel
15. exponential power cusp location estimation shape 1/2 Fisher information logarithmic rate
16. Smith 1985 maximum likelihood estimation class nonregular cases 67 90 endpoint parameters rates
17. "two-piece" "Fisher" "regularity" Steel Rubio
18. "Tukey" "lambda" "singular" information uniform
19. "beta-binomial" "Poisson" "likelihood ratio" boundary
20. "Self" "Liang" "1987" "605" "610" asymptotic
21. Anscombe 1950 sampling theory negative binomial logarithmic series distributions 358 382
22. Aragon Eberly Eberly 1992 existence uniqueness maximum likelihood negative binomial 375 379
23. Levin Reeds 1977 compound multinomial likelihood functions unimodal 79 87
24. Reeds 1985 asymptotic number roots Cauchy location likelihood equations 775 784
25. "A class of distributions which includes the normal ones" Azzalini 1985
26. "two-piece" "Fisher information" "Rubio" "Steel"
27. "Burr" "Weibull" "Pareto" "likelihood ratio" limit
28. "generalized beta" "information" singular "Tukey" lambda
29. "Poisson-inverse Gaussian" Willmot 1987 "113" "127"
30. "half-integer" "Bessel polynomials" variance inequality concavity
31. "zero truncated negative binomial" "concave" likelihood
32. "generalized normal" "1/2" "Fisher information" location
```

Additional direct reading followed the existing infer-moving source ledger: the accessible 2007 [Zhu–Zinde-Walsh manuscript](https://www.mcgill.ca/files/economics/propertiesandestimation.pdf), Proposition 5, requires both power exponents greater than one for its joint asymptotic-normality theorem. This is useful context, not a proof of the project's critical GED constants. Its nearby information-equality wording should not be used to bypass the distinction between score cross-products and an ordinary expected Hessian.

## Validation and completion

All required builds and suites completed before this report was written. The local command waited for shared CPU slots without interruption. The commands were foreground commands, with tool sessions awaited to exit. Scratch files and logs are under ignored `build/scratch/`; no figure or build artifact is needed by this report.

| Final check | Machine and invocation | Result |
|---|---|---|
| C++ development tier | Workstation: `/tmp/umvl-cpu/run4 nice -n 10 just cpp-test`, temporary/runtime directories set to this worktree's `build/tmp` | **106/106 passed**, zero failures; ctest wall time 67.03 s, excluding build and slot wait. |
| R development tier | `/tmp/umvl-cpu/simbox-run just test` | **FAIL 0, WARN 0, SKIP 154, PASS 20983**; test runner elapsed 70.924 s, excluding generation/build and remote-slot wait. Exit 0; no fallback needed. |
| Edit-scope check | Workstation, `/tmp/umvl-cpu/run python3 build/scratch/lit-scope-check.py` | Passed: mask Literature cells and section 6 Literature text, compare remaining pre-reference content with HEAD; assert 23 data rows. |
| Whitespace | `git diff --check` | Passed. |

The workstation reported Linux `7.0.0-42-generic`, GCC `15.2.0` and R `4.6.1`. The supplied simbox runner set the remote process/thread limits and niceness; no separate remote compiler-version probe was performed. Both runners pinned BLAS/OpenMP to one thread; the local runner reserved four slots and selected build parallelism four. Logs: `build/scratch/lit-check-cpp.log` and `build/scratch/lit-check-r.log`.

The first scope-check run had a bookkeeping assertion expecting three lowercase occurrences of “not checked” outside the edited cells; the actual count was two. That scratch assertion was corrected and the final check passed. No package test failed. R skips are reported as skips, not passes. Full-tier, no-long-double, JS and site suites were not required for these prose/reference-only edits: no fitter, d/p/q/r, test infrastructure, binding message, dossier or public signature changed.

The open entries above complete the requested bounded literature assessment; they do not certify novelty. Highest-value remaining source comparisons are the PIG/Bessel theorem, Wingo's exact Burr hypotheses, the Kumaraswamy uniqueness theorem, and the specific half-plane/point-process closure experiments. No needed code change was identified by this task, and none was made. No commit was created.

Artifact display was attempted with the requested `codex-math-web/bin/show` command. The viewer returned HTTP 403 because this worktree is outside its configured readable roots. The report remains at its requested worktree path; viewer configuration and other worktrees were left untouched. This display limitation is separate from the successful package checks.
