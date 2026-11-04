# Literature reconnaissance, 2026-11-04

Scope: identity checks for the references used in this job, and located reading
of accessible primary texts supporting Gaussian cone limits. This is research
reconnaissance, **not** a completed paper-workflows audit. There is no frozen
author bundle/inventory for this research job; no formal workflow outputs were
constructed. The installed audit-literature skill and the workflow's
literature-audit contract were read. The user explicitly requested literature
searches and citation verification. Searches sent public author/title/topic/DOI
metadata to the web search provider; manuscript files and prose were not sent.
No publication PDF or copyrighted source text is retained here.

Search scope: likelihood experiments at Poisson, exponential, Weibull and
Pareto closure laws; integer-size binomial inference; boundary bootstrap and
cone theory. All publication years through 2026. Providers: web search, then
publisher/journal, arXiv and institutional records. Stopping rule: inspect a
primary source for each theoretical mechanism, verify the named historical
binomial paper, search specifically for a pre-existing Burr–Pareto endpoint
experiment, and retain access limitations. No comprehensive coverage or novelty
claim follows from these searches.

## Sources and their permitted uses

| Work; checked identity | Primary source and locator | Access and use |
|---|---|---|
| Self, S. G. and Liang, K.-Y. (1987), *Asymptotic Properties of Maximum Likelihood Estimators and Likelihood Ratio Tests Under Nonstandard Conditions*, JASA 82(398), 605–610. DOI 10.1080/01621459.1987.10478472 | [Institutional full text](https://www.stat.cmu.edu/~brian/763-2013/week06/papers/self-liang-1987.pdf), pp. 605–608, Theorems 1–3, Cases 5–6; p. 610 discussion | Full PDF inspected. Gaussian projection and the two relevant mixtures agree with the elementary calculations here. Their differentiability/positive-information assumptions do not automatically cover BB's singular coordinates or Burr's Pareto face. |
| Drton, M. (2009), *Likelihood ratio tests and singularities*, Annals of Statistics 37(2), 979–1012. DOI 10.1214/07-AOS571 | [Author's arXiv version, v3](https://arxiv.org/pdf/math/0703360), Section 2, Theorem 2.6 and Remarks 2.8–2.9 | Full PDF inspected at these locators. Requires a regular ambient experiment, appropriate set convergence/Chernoff regularity, and consistency. Used to delimit inference from tangent geometry, not as a ready-made theorem for this countable union of BB sheets. |
| Olkin, I., Petkau, A. J. and Zidek, J. V. (1981), *A Comparison of n Estimators for the Binomial Distribution*, JASA 76(375), 637–642. DOI 10.1080/01621459.1981.10477697 | [Publisher abstract/record](https://www.tandfonline.com/servlet/linkout?type=Scholix&url=https%3A%2F%2Fdx.doi.org%2F10.1080%252F01621459.1981.10477697), abstract; [author's publication list](https://www.stat.ubc.ca/~jim/jzall.html) | Identity and abstract verified. Direct ordinary DOI page returned 403. Full text not inspected. The abstract explicitly describes instability for unknown small p and large n. The kappa-rate derivation in this job is independent; it is not attributed to an unread theorem. |
| van der Vaart, A. W. (1998), *Asymptotic Statistics*, Cambridge University Press. DOI 10.1017/CBO9780511802256 | [Publisher, Chapter 7](https://www.cambridge.org/core/books/abs/asymptotic-statistics/local-asymptotic-normality/558B4798D5D70D9B1C3ABC15FAFE3BA7), chapter summary and book record | Identity and chapter summary checked; full book not inspected in this job. Cited as general LAN background, with the actual application derived here. Online chapter date 2012 is not the book's print year 1998. |
| Dacunha-Castelle, D. and Gassiat, É. (1997), *Testing in locally conic models, and application to mixture models*, ESAIM: Probability and Statistics 1, 285–317. DOI 10.1051/ps:1997111 | [Journal archive](https://www.numdam.org/item/PS_1997__1__285_0/), metadata; [publisher record](https://www.cambridge.org/core/journals/esaim-probability-and-statistics/article/abs/testing-in-locally-conic-models-and-application-to-mixture-models/1B08ABE51A8E9A9CFA942E29D5B38ED0), metadata/abstract | Title, authors, issue year, venue, volume, pages and DOI checked. Direct DOI fetch failed, but the publisher record confirms it. Full text inaccessible through this interface. A related conceptual reference only; no theorem from it is invoked. |
| Smith, R. L. (1985), *Maximum likelihood estimation in a class of nonregular cases*, Biometrika 72(1), 67–90. DOI 10.1093/biomet/72.1.67 | [Publisher](https://academic.oup.com/biomet/article-abstract/72/1/67/242523?login=true), abstract | Identity and abstract checked. Describes parameter-dependent support and rate regimes. Does not establish this particular Burr smoothing experiment; no unread theorem is transferred. |
| Andrews, D. W. K. (2000), *Inconsistency of the Bootstrap when a Parameter is on the Boundary of the Parameter Space*, Econometrica 68(2), 399–405. DOI 10.1111/1468-0262.00114 | [Publisher](https://onlinelibrary.wiley.com/doi/10.1111/1468-0262.00114/abstract); [author's 1997 precursor](https://elischolar.library.yale.edu/cowles-discussion-paper-series/1405/), abstract | Published-work identity checked. Precursor abstract states failure for constrained normal-mean ML and discusses percentile overcoverage. Published full text inaccessible here. The report's concrete Gaussian bootstrap argument is independent. Publisher's web date 2003 is not the issue year 2000. |
| Shao, Q. (2004), *Notes on maximum likelihood estimation for the three-parameter Burr XII distribution*, CSDA 45(3), 675–687. DOI 10.1016/S0167-9473(02)00367-5 | [Publisher preview](https://www.sciencedirect.com/science/article/abs/pii/S0167947302003675), title, metadata and abstract | Identity and preview checked. Full text inaccessible. Existence/limiting-family background only; no attribution of the endpoint PPP result or novelty assertion. |
| Shao, Q., Ip, W.-C. and Wong, H. (2004), *Determination of embedded distributions*, CSDA 46(2), 317–334. DOI 10.1016/S0167-9473(03)00171-3 | [Publisher preview](https://www.sciencedirect.com/science/article/abs/pii/S0167947303001713), introduction/abstract | Identity and preview checked. Discusses local log-likelihood discriminants and Burr's embedded Pareto model. This is a different paper from Shao's single-author Burr paper. No full-text theorem inspected. |

## Search record and unresolved coverage

Queries included:

- `Self Liang 1987 asymptotic properties maximum likelihood boundary 605 610 pdf`
- `Olkin Petkau Zidek 1981 comparison estimators N binomial distribution 68 419`
  (the guessed numbers in this discovery query were corrected by the publisher:
  actual volume/pages are 76, 637–642).
- `Drton 2009 likelihood ratio tests singularities tangent cone pdf`
- `Burr Pareto limit likelihood asymptotic inference point process embedded distribution Shao 2004`
- `Testing locally conic models Dacunha Castelle Gassiat 1997 285 317`
- `Andrews 2000 inconsistency bootstrap parameter boundary 68 399 405`
- `Smith 1985 maximum likelihood estimation class nonregular cases 72 67 90`
- `site.cowles.yale.edu Andrews inconsistency bootstrap parameter boundary pdf`
- `site.cambridge.org asymptotic statistics van der Vaart 1998 differentiability quadratic mean local asymptotic normality`
- `"Burr XII" "Shao" "2004" "likelihood"`
- `"Burr" "Pareto" "Poisson process" inference`
- `"On the distribution of the likelihood ratio" "1954" Chernoff`
- `"10.1051/ps:1997111" site.esaim-ps.org`
- `"Testing in locally conic models" site.esaim-ps.org`

Chernoff's historical reference was confirmed in a journal issue listing:
H. Chernoff (1954), *On the Distribution of the Likelihood Ratio*, Annals of
Mathematical Statistics 25(3), 573–578, DOI 10.1214/aoms/1177728725.
[Journal archive](https://www.jstor.org/stable/i312779). Direct DOI access
failed; its full text was not inspected and no additional theorem is attributed
to it. Self–Liang supplies the inspected cone-theory source.

The searches did not locate an inspected primary theorem for the specific
Burr-to-Pareto logistic-intensity experiment or the integer-BB Poisson
half-plane experiment. This is an access/coverage limitation, not evidence of
priority or absence. Ordinary moving-support theorems, Gaussian cone theorems,
and fixed-sample embedded-model slopes address different hypotheses. The new
calculations stand on the derivations, not on analogy to these sources.
