# NB existence and uniqueness: TASK-116.19

The ordinary negative-binomial theorem is **proved in the canonical [NB dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence-and-uniqueness-proof)** without relying on the disputed 1992 proof. For any finite nonempty sample of nonnegative integers with positive mean $m$, a finite joint MLE exists exactly when $v>m$, where $v=n^{-1}\sum_i(x_i-m)^2$. In that case the profile size score has exactly one positive root and it is the unique global maximizer. If $v\le m$, including equality, the profile increases strictly to its unattained Poisson supremum. All-zero samples have the attained, nonunique maximum on $r=0$ or $p=1$ in the package's declared domain.

The dossier expands and checks the convexity argument of **Brøns and Jensen (1988)**. Their complete preprint was obtained and read. No originality claim is made. The accessible evidence identifies older published proofs, but their full texts were not obtained; this job therefore uses the self-contained-proof route in the brief. The exact objection in Wang's inaccessible note remains a precisely bounded historical open item. A concrete defect in the accessible Aragón–Eberly–Eberly preprint was independently located and checked below.

## Source access and publication status

This is research reconnaissance with direct mathematical checking, not a formal paper-workflows bundle audit. There were **22 search queries**, against the brief's approximate budget of 20. The exact queries, scope and stopping rule are in [searches.json](2026-11-07-nb-proof-data/searches.json). [source-access.json](2026-11-07-nb-proof-data/source-access.json) records acquisition URLs, retrieval times, byte counts, SHA-256 digests, reading scope and access failures. Copyrighted PDFs, rasterized pages and challenge responses remain in ignored `build/scratch/nb-proof/`.

| Source | Material actually inspected | Conclusion supported |
|---|---|---|
| [Anscombe (1950)](https://doi.org/10.1093/biomet/37.3-4.358), [scan](https://www1.cmc.edu/pages/faculty/MONeill/Math152/Handouts/Anscombe1.pdf) | Printed p. 367, equation (3.2) and the following paragraph, read from the scan; adjacent pages also inspected | Gives the endpoint-sign argument for existence under overdispersion, but explicitly leaves uniqueness and underdispersed nonexistence unproved. His variance uses divisor $n-1$; his exact condition $(n-1)s^2>nm$ is our $v>m$. |
| [Aragón, Eberly and Eberly, Stanford preprint (1991)](https://purl.stanford.edu/gy645ty0351), [published record (1992)](https://doi.org/10.1016/0167-7152(92)90157-Z) | Entire proof, printed pp. 1–7, PDF pp. 3–9, read from rasterized preprint pages | The preprint's Case 3 graph-ordering proof has the defect identified below. The published text was not acquired or compared line by line with the preprint. |
| [Brøns and Jensen (1988), Preprint 1988 No. 8](https://www.math.ku.dk/bibliotek/arkivet/preprints-fra-ims/1988/preprint_1988_-_no._8_br_ns__hans_and_jensen__s_ren_tolver_-_maximum_likelihood_estimation_in_the---.pdf) | Complete mathematical argument, printed pp. 1–7, PDF pp. 3–9 | A complete working convexity proof. The dossier supplies the intermediate calculus, strictness and endpoint arguments explicitly. This is a university preprint; it is not described here as a verified journal publication. |
| [Levin and Reeds (1977)](https://doi.org/10.1214/aos/1176343741) | Journal identity and bibliographic leads; Euclid PDF, legacy and download routes returned HTML challenges; JSTOR also failed to supply proof text | Classical published reference, not an independently inspected proof in this job. No theorem number or proof detail is invented. |
| [Wang (1996)](https://doi.org/10.1016/0167-7152(94)00259-2) | Indexed publisher abstract and API bibliographic metadata; publisher/PDF routes returned 403, full-text API 401, PDF API 406 | Wang reports a problem with the 1992 proof. The accessible material does not specify the disputed step. |
| [Bandara, Gill and Mitra (2019)](https://doi.org/10.1016/j.spl.2019.01.009) | Indexed publisher introduction and discussion; full proofs not inspected | Points to Simonsen (1976, corrected 1980), and to Levin–Reeds as an alternative. It describes a reversed inequality in Simonsen's original argument and a later correction. This is a lead, not this job's verification of those proofs. |

A further primary bibliography, [Besson and Partrat (1992)](https://www.cambridge.org/core/journals/astin-bulletin-journal-of-the-iaa/article/trend-et-systemes-de-bonusmalus1/5C952A263DE55EE407B905C11E512EC7), lists a Brøns–Jensen contribution in the 1989 ISI Paris proceedings, vol. 1, pp. 168–169. That proceedings text was not obtained. The 1988 preprint cites a 1979 Simonsen correction, whereas the 2019 publisher introduction cites 1980. Those records should not be conflated without reading the corrections. Seifen's thesis, printed p. 81, directs readers from Wang to Levin–Reeds, but is not a substitute for reading either original.

**Published-proof finding:** the bibliographic trail supports the existence of older published treatments. This job does not certify an unread published proof as complete and correct. The dossier's complete proof settles the mathematical dependency independently of that access limitation.

## The preprint defect and Wang's objection

The accessible AEE preprint's Case 3, printed pp. 6–7 (PDF pp. 8–9), fixes the vector of intermediate tail frequencies while integrating a mixed-derivative inequality in the mean. It then uses the fact that the feasible domain at mean zero forces those frequencies to zero to cancel boundary values. That cancellation changes the held coordinates. For $M=3$, write $a=F_2$. Its equation (4), extended to mean zero **with $a$ held**, gives

$$
 u(z,0,a)=-\frac{a}{2(1+z)}.
$$

Thus $a=1/4$, $z_1=1$ and $z_2=2$ give boundary values $-1/16$ and $-1/24$, whose difference is $1/48$, not zero. Integrating at fixed $a$ cannot replace these by values on the different slice $a=0$. The exact rational calculation is reproduced in [check.py](2026-11-07-nb-proof-data/check.py) and [check.json](2026-11-07-nb-proof-data/check.json). This diagnoses a missing argument in the preprint, not a counterexample to the NB theorem. The theorem is established by the separate convexity proof.

**Open — exact attribution to Wang.** Obtain Wang's printed pp. 113–114 and the published AEE pp. 375–379; identify Wang's specific equation or inference, and compare it with the fixed-coordinate boundary defect above. After the recorded publisher, PDF, API and repository attempts, only Wang's abstract and metadata were available. It would be unjustified to call the independently found defect “exactly Wang's objection” without that text. The similarly reported Simonsen inequality problem is a different historical issue and must not be substituted for it.

## Checks of the complete proof

The canonical proof uses the package's success-probability convention, $p(r)=r/(r+m)$, and the total negative profile log likelihood $f=-\ell_{\rm prof}$. Its steps were checked as follows:

1. Differentiation gives $f'=n\log(1+m/r)-\sum_{i,j}(r+j)^{-1}$ and $f''=-nm/[r(r+m)]+\sum_{i,j}(r+j)^{-2}$, with $0\le j<x_i$.
2. The coordinate $b=r\log(1+m/r)/m$ increases from zero to one, with $b'>0$ and $b''=-m/[r(r+m)^2]<0$.
3. The finite-sum telescoping identity in the dossier proves strict convexity of every $T_q=r(r+m)^2\sum_{k=0}^{q-1}(r+k)^{-2}$ for integer $q\ge1$. The split at $\min(q-1,\lfloor m\rfloor)$ covers $m<1$, integer $m$, $q=1$, and empty middle/final sums. The middle factors have strictly positive log curvature; the final factors have an explicitly positive second derivative. At least one term supplies strictness in every case.
4. Consequently $h=f''/b''=n(r+m)-m^{-1}\sum_iT_{x_i}$ is strictly concave and has finite limit $n(v-m)/m$. These facts force $h'>0$, as proved in the dossier.
5. The ratio $G=f'/b'$ is a weighted tail average of $h$, so $G>h$ and $G'>0$. Integrability follows from the explicit $b''$ and boundedness of $h$ on every such tail. This proves strict convexity in $b$ without assuming monotonicity of the raw size score.
6. The limits $G(0+)=-\infty$ and $G(\infty)=n(v-m)/m$ give the complete criterion, including strict negativity at every finite $r$ when $v=m$. Reversing the sign to recover the log-likelihood derivative proves global maximality and unique size, followed by unique probability.
7. The NB mass converges explicitly to the Poisson mass along the profile. Finite zero-law boundary members have zero likelihood when any observation is positive. The all-zero sample is treated separately in the package's extended parameter domain.

This establishes the exact-arithmetic theorem for finite integer samples. It does not upgrade the existing binary64 score evaluations to outward-rounded arithmetic: the implementation's certificate remains conditional on computed comparisons with its documented practical floating-point margin.

## Documentation changes and downstream scope

The NB dossier now cites the inspected Brøns–Jensen argument and contains the full proof. Its front-matter reason and certificate discussion no longer rely on AEE (1992) as proof authority. Levin–Reeds and Wang are identified with their access qualifications. The ZINB dossier links to the NB proof for actual reductions to the ordinary NB likelihood and states that it supplies no general inflated/truncated-profile uniqueness theorem.

The ztnbinom dossier was inspected and left unchanged: it does not cite the disputed ordinary-NB theorem, already declares joint uniqueness unknown, and describes its sample-specific certificate. No new claim is made for joint ZINB, truncated NB or PIG. No production code, default, status, golden, d/p/q/r, generator, binding message or test infrastructure changed. No changes to the settled code are needed to establish the theorem.

## Validation

Tested base revision: `2fb7558010a91dab4d2c4172263cdbdaa99bb8f9`, branch `codex/nb-proof`, with only the documented dossier/evidence changes. [validation.json](2026-11-07-nb-proof-data/validation.json) records commands, machines, final outcomes, source hashes and environment. All commands were foreground jobs and were waited for. Local computation used `/tmp/umvl-cpu/run`; long suites used the supplied simbox wrapper. No simbox fallback was needed.

| Final check | Machine | Result |
|---|---|---|
| `check.py --output …/check.json` through `run` | Workstation | PASS: 219 deterministic samples, 67 finite signed root brackets, 152 Poisson regimes, 10 equality cases; exact AEE boundary calculation passed |
| `just test` through `simbox-run` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20,983 |
| `just cpp-test` through `simbox-run` | simbox | 106/106 dev shards passed |
| `just test-full` through `simbox-run` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43,803 |
| `UMVL_TEST_TIER=full just cpp-test` in the same simbox command | simbox | 129/129 full shards passed |
| `just site-check` through `run` | Workstation | PASS: 99 family pages, MathML and links/fragments, 99 standalone C++17 usage snippets, browser/theory checks |

The first site run found a broken ZINB-to-NB section fragment: family-page rendering omits those section IDs. The dossier link was corrected to the family page and the complete command rerun green. The site check also regenerated an unrelated historical screenshot; that test side effect was restored byte-for-byte from its initially clean tracked version. Compiler warnings in unchanged baseline headers and a shell-profile message appeared in build logs; the R test warning counts above are zero.

The numerical script uses Python 3.14.5, mpmath 1.3.0, 80 decimal digits and a practical comparison margin $10^{-55}$. It enumerates all positive-mean multisets of sizes 1–4 from counts 0–5, adds two stress samples and the pairs $m-k,m+k$ for $k\in\{2,10,100,1000\}$ and $m=k^2-1,k^2,k^2+1$. It checks 29 half-decade sizes from $10^{-6}$ to $10^8$, transformed-score increase, the strict limiting bound, the telescoping identity and curvature for the small-count samples, and signed bisection brackets with relative width below $10^{-35}$ and total-score residual below $10^{-30}$. Its root-work cap is $128\min(\mathrm{iterlim},100000)$, default iterlim 128; exhaustion fails. These finite calculations are sanity checks, not the mathematical proof or universal floating-point certification. No random seed is needed.

The workstation environment was Linux x86_64, R 4.6.1, GNU C++ 15.2.0, Node 22.22.1 and Pandoc 3.7.0.2; site validation reported SymPy 1.14.0 and the installed BH Boost headers. simbox used GNU C++ 13.3.0; its R version was not captured in the suite logs. The wrappers pinned BLAS/OpenMP to one thread and applied nice priority; remote R used four testthat workers. No installs, board edits, commits or publishing were performed.

## Deliverable access

The requested pane command was attempted on this report. The viewer returned HTTP 403 because its configured readable roots omit this worktree (while including the main checkout). No viewer configuration or other checkout was changed. The report and reproducible evidence remain at the worktree paths linked above.
