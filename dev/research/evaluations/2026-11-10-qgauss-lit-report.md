# q-Gaussian / Pearson VII–II: literature check on ML estimation

TASK-184. Sources checked 2026-11-10; final local validation completed
2026-11-11. Worktree base: 99d64df4. This job changes only this report and its
[small evidence folder](2026-11-10-qgauss-lit-data/).

Evidence labels below distinguish **derived** statements (calculations from the
specified density), **read** statements (inspected source text or software),
and **abstract/preview** statements. Bibliographic identity verification does
not establish an inaccessible paper's detailed theorems.

## Findings

1. A direct statistics-side antecedent is Papastathopoulos and Tawn's
   **A generalised Student's t-distribution** (2013), which explicitly extends
   Student t to negative degrees of freedom. “Generalised t” is an overloaded
   name; identify the authors or give the density. [P1]
2. The unrestricted, all-free location–scale–shape likelihood is unbounded.
   There are two independent mechanisms: small-positive-df Student scale
   collapse, and bounded-support endpoint spikes for eta below −1. A numerical
   local optimum is consequently not an unrestricted global MLE.
3. The normal crossing is locally regular in signed inverse df. Its efficient
   shape information, with location and scale unknown, is 3/2 per observation
   in eta and 3/8 in q. There is no skew-normal-at-zero information singularity.
   This is a derivation here, not a theorem attributed to P1.
4. The proposed negative-eta regimes are correct, with an additional critical
   case at eta = −1/3. This is an inference threshold, not an existence
   restriction. Smith's endpoint parameter is alpha = m + 1, giving alpha > 2
   exactly when eta > −1/3 on the bounded branch. [P2; derivation below]
5. Conventional finite q-Gaussians with positive inverse scale cover only
   eta > −1. Uniformity and the endpoint-spiking continuation require care in
   naming and parametrization. Available software supplies component
   distributions and local fitting methods; no global certificate for the
   unified, appropriately restricted free fit was verified.

## 1. Names, domains, and parameter maps

**Derived.** Put z = (x − mu)/s, with s > 0. On its support the normalized
density is

\[
 f(x;\mu,s,\eta)=\frac{C_\eta}{s}
 (1+\eta z^2)^{-(1+\eta)/(2\eta)}.
\]

For eta > 0 the support is the real line and

\[
 C_\eta=\frac{\sqrt{\eta}}{B(1/2,1/(2\eta))}.
\]

For eta < 0 set

\[
 h=\frac{s}{\sqrt{-\eta}},\qquad
 m=-\frac{1+\eta}{2\eta},\qquad
 C_\eta=\frac{\sqrt{-\eta}}{B(1/2,m+1)}.
\]

The support is [mu − h, mu + h]; the density is zero outside it, with endpoint
values interpreted by the library's upper-semicontinuous closure convention.
Every finite negative eta is proper: m > −1/2. At eta = 0 use the normal
density with location mu and standard deviation s. Elsewhere s is a scale,
not generally the ordinary standard deviation; when finite, the variance is
s²/(1 − 2 eta).

| Name / convention | Map to this family |
|---|---|
| Student t, location–scale Student t; symmetric Pearson VII | eta > 0, df nu = 1/eta; the usual t scale is s. In the kernel (1 + ((x − mu)/a)²) to power −M, a = s/sqrt(eta), M = (1 + eta)/(2 eta). |
| Symmetric beta; Pearson II; R-distribution | eta < 0, density proportional to (1 − ((x − mu)/h)²) to power m. For Y = (X − mu + h)/(2h), Y has Beta(m + 1, m + 1). |
| Papastathopoulos–Tawn generalised Student t | Statistics-side negative-df continuation; the signed inverse-df coordinate joins its two normal limits at eta = 0. P1's identity and negative-df scope are verified; its full inference treatment remains inaccessible. |
| Tsallis q-Gaussian, usual convention | Density proportional to [1 + (q − 1) beta (x − mu)²] to power −1/(q − 1), beta > 0, q < 3, with the normal member at q = 1. |

The beta and t names and their location–scale conventions are explicitly
documented by PearsonDS. SciPy calls the bounded symmetric beta law the
R-distribution. [S1, S2, S5] The q-Gaussian convention is given in P5, §2.

**Derived.** The exact q map is

\[
 q=\frac{1+3\eta}{1+\eta},\qquad
 \eta=\frac{q-1}{3-q},\qquad
 \beta=\frac{1+\eta}{2s^2}.
\]

Thus ordinary finite q < 1 corresponds to −1 < eta < 0 and m > 0.
The normal is q = 1. Uniformity at eta = −1 is the q to minus-infinity limit,
not a finite conventional q member. For eta < −1 the algebra gives q > 3
and beta < 0: this proper compact Pearson II continuation is outside the
usual positive-beta q-Gaussian domain. Calling the entire signed-eta family
simply “q-Gaussian” hides this distinction.

The signed-eta bounded branch is also only a subset of the full Pearson II
family: its beta shape exceeds 1/2, whereas general symmetric beta permits
every positive beta shape.

**Read.** “Elliptical Pearson II” and “elliptical Pearson VII” are established
multivariate names. Their radial generators are respectively (1 − u) to power
m on 0 < u < 1, and (1 + u/tau) to power −M, with tau > 0 a radial scale.
In dimension d their integrability
conditions are m > −1 and M > d/2. Wang and Yan, Table 1, p. 105, supplies
these conventions. [P4] A multivariate continuation must account for dimension;
the univariate exponent should not be copied unchanged.

## 2. Existence: what an unrestricted free fit would maximize

The following arguments concern ordinary iid likelihood, the product of the
densities, rather than a q-logarithmic fitting criterion. Assume a finite
nonconstant sample unless stated otherwise.

### Student-side collapse

**Derived proof.** Fix any positive df nu, put mu at an observation of
multiplicity k, and let s decrease to zero. At that observation the density
is proportional to 1/s. At every different observation it is asymptotic to a
positive constant times s to power nu. Therefore

\[
 L(\mu,s,\nu)\asymp s^{\,\nu(n-k)-k}.
\]

For k < n, choosing nu < k/(n − k) makes the likelihood diverge. An unrestricted
free positive df always permits that choice. For distinct observations,
nu < 1/(n − 1), equivalently eta > n − 1, suffices. Constant samples already
have unbounded scale-collapse likelihood with any fixed shape.

Consequently even the conventional q-Gaussian domain, which contains all
positive-df Student distributions, has unbounded all-free likelihood.
Continuation across the normal does not cure this mechanism. Liu and Rubin
also explicitly warn about high likelihood spikes in t fitting (p. 20). [P3]

The library's df-at-least-one collapse restriction is relevant context, but it
is not a complete global proof for a proposed new continuation. Ties, small
samples, and joint limiting paths still require their own analysis.
See the repository's [parameter rules](../../roadmap/2026-10-29-parameter-rules.md).

### Bounded side: confirm the proposed regimes

Write a = mu − h and b = mu + h. Near either endpoint the density is a
positive constant times distance to power m. Smith's notation is distance to
power alpha − 1, so alpha = m + 1. [P2, abstract; derived identification]

| eta | m | Finite-sample / local-inference conclusion | Evidence |
|---|---:|---|---|
| −1/3 < eta < 0 | m > 1 | Finite endpoint information; the Smith regular regime. A fixed-shape location–scale maximum exists for nonconstant data. | Direct endpoint calculation and existence argument below; matches P2's alpha > 2 criterion. |
| eta = −1/3 | m = 1 | Critical case, q = 0: logarithmically divergent endpoint information. A fixed-shape maximum still exists. | Direct calculation. P2 distinguishes alpha = 2 from ordinary root-n theory. |
| −1 < eta < −1/3 | 0 < m < 1 | Nonregular moving endpoints; bounded likelihood. Fixed-shape location–scale maxima exist. | Direct proofs below. P2's abstract describes nonnormal behavior for 1 < alpha < 2. |
| eta = −1 | m = 0 | Uniform. With shape held, the closed-support MLE is the sample interval. | Direct uniform likelihood calculation. |
| eta < −1 | −1/2 < m < 0 | Proper densities with integrable endpoint spikes; likelihood is unbounded with location and scale free. | Explicit endpoint path below. |

**Derived regularity calculation.** For m > 0, an endpoint score has a leading
term proportional to 1/distance. Its squared integral is proportional to

\[
 \int_0^\epsilon d^{m-2}\,{\rm d}d,
\]

which is finite exactly for m > 1. Solving this inequality gives
−1/3 < eta < 0. At m = 1 the divergence is logarithmic. At uniformity, the
interior location score alone misses the moving-support jump; the uniform
model is not regular merely because that interior score vanishes.

**Derived existence and boundedness.** At fixed m > 0 the density is at most
1/[h B(1/2,m + 1)]. Feasibility requires h at least half the positive sample
range r. As h tends to infinity the likelihood tends to zero; with h bounded,
the center stays bounded. If either support endpoint approaches an observation,
the likelihood tends to zero. Continuity on a compact exhaustion therefore
gives an interior location–scale maximizer. This proves existence, not
uniqueness.

Boundedness also holds uniformly while m ranges over (0, 1): the normalizing
constant 1/B(1/2,m + 1) is at most 3/4, giving
L at most [3/(2r)] to power n. An open shape interval can still have an
unattained supremum at an excluded face; boundedness is not attainment.

At m = 0, minimizing b − a subject to containing the sample gives
a = min(x), b = max(x), mu = (a + b)/2 and s = (b − a)/2. This uses closed
support and is a statement with uniform shape held, not a global result over
the unrestricted signed-eta family.

For fixed m < 0, take a = min(x) − epsilon and hold b > max(x) fixed.
The likelihood contains epsilon to a negative power for the minimum
observations, while the other factors have positive finite limits.
It diverges through strictly interior fits as epsilon decreases to zero.
There is no finite-likelihood global MLE to return.

**Conclusion on the roadmap claims.** All four stated regimes are confirmed
with the qualifications above. Add the critical equality case eta = −1/3,
and qualify the q-Gaussian name beyond eta = −1. The threshold −1/3 governs
standard inference; excluding the entire interval below it would discard
bounded-likelihood, fixed-shape estimable models solely for nonregularity.
The detailed joint asymptotic laws with both endpoints and shape unknown
remain an explicit open item below.

## 3. The normal crossing: information is nonsingular

**Derived.** At eta = 0, expansion of the normalized density gives

\[
 \left.\partial_\eta\log f\right|_{\eta=0}
 =\frac{z^4-2z^2-1}{4}.
\]

In particular the normalizing constant contributes −1/4; omitting it would
give a score with nonzero expectation.

At mu = 0, s = 1, use coordinates (mu, log s, eta). Their scores are

\[
 Z,\qquad Z^2-1,\qquad (Z^4-2Z^2-1)/4.
\]

Taking exact standard-normal moments yields

\[
 I=
 \begin{pmatrix}
 1&0&0\\
 0&2&2\\
 0&2&7/2
 \end{pmatrix},
 \qquad \det I=3.
\]

The expected negative pointwise Hessian at the normal member also equals this
matrix. For example,

\[
 \left.\partial_\eta^2\log f\right|_{\eta=0}=Z^4/2-Z^6/3,
\]

whose negative expectation is 7/2. This is a model calculation; no production
covariance procedure or particular fitted estimator's asymptotics is
validated by it.

After projecting out location and scale, the eta score is

\[
 S_{\eta,\mathrm{eff}}=\frac{Z^4-6Z^2+3}{4},\qquad
 I_{\eta,\mathrm{eff}}=\frac32.
\]

Since d eta/d q = 1/2 at q = 1, efficient q-information is 3/8.
The fourth-degree component survives nuisance projection; shape does not
become locally confounded with location or scale.

**Regularity proof sketch, beyond formal differentiation.** In a central
region |z| at most |eta| to power −1/16, the log-density and square-root
density expansions have polynomial remainders controlled by Gaussian moments.
The negative-eta support endpoints are farther away, of order
|eta| to power −1/2. Tail remainders can be made o(eta²) using uniformly
bounded sufficiently high even moments near zero:

\[
 E_\eta Z^{2k}=\frac{(2k-1)!!}
 {\prod_{j=1}^k(1-2j\eta)}.
\]

For each fixed k this formula holds in a sufficiently small two-sided
neighborhood of zero. Splitting the square-root-density integral into those
central and tail regions establishes differentiability in quadratic mean.
Together with the positive-definite matrix, this gives a regular local
normal crossing. It does not establish a global MLE where the likelihood
paths in §2 prove there is none.

If the model is restricted to the Student side eta at least zero, normality
instead becomes a parameter-boundary question. That is distinct from an
information singularity. Apparent flattening in df as df tends to infinity
is also a coordinate issue: signed inverse df has finite positive information.

As an additional check, at the fitted normal location and ML scale,
the total eta score is n times the empirical excess kurtosis divided by four.
The [base-R check](2026-11-10-qgauss-lit-data/check.R) verifies the moment
arithmetic; its [output](2026-11-10-qgauss-lit-data/check-output.txt) records
information 7/2, scale–shape cross-information 2, and efficient information 3/2.
The analytic argument, not this arithmetic check alone, supports regularity.

## 4. Established fitting methods and their guarantees

**Read: P3, §§4–6 and p. 20.** Positive-df Student fitting has EM, ECM and
ECME methods, including unknown df. The normal–gamma mixture gives weights
(nu + 1)/(nu + z²) in one dimension; location and scale receive weighted
updates and df receives a shape update or observed-likelihood maximization.
These are local methods; ECME can improve convergence speed. Monotone
likelihood improvement does not certify a global maximum or rule out the
collapse path in §2. The positive gamma mixing law does not extend this EM
construction to negative df.

**Abstract/preview: P1.** The paper establishes the existence of the
negative-df modelling proposal and presents an application. Its detailed
optimization, constraints and inference guarantees were not verified from
full text. Do not attribute a global free-fit theorem to it on this evidence.

**Read: S3.** PearsonDS uses moment-based starting values, adjusts support,
then numerically optimizes Pearson subclasses with nlminb. The documentation
calls the implementation preliminary and slow, with no analytic gradients.
Its top-level selection ranges over the Pearson system, including asymmetric
models; that result is not automatically a fit constrained to this symmetric
three-parameter family.

For a future implementation, the bounded likelihood can be written directly
in endpoint and exponent coordinates, and the normal member handled exactly.
What is missing is a verified global comparison over a domain that removes
both unbounded mechanisms, including joint escape paths and exceptional
samples. No such certificate was found in the inspected material.

## 5. Software and checked defects

| Software | Available operations / parameter map | Verified limitations |
|---|---|---|
| R stats, through PearsonDS VII [S2] | dt/pt/qt/rt, wrapped by d/p/q/rpearsonVII with df = 1/eta, location = mu, scale = s. | Positive-df branch; no negative-df continuation through these wrappers. |
| R PearsonDS II [S1] | d/p/q/rpearsonII; beta shape a = m + 1, location = mu − h, scale = 2h. | Full symmetric beta is broader than the signed-eta branch. Its unrestricted free-endpoint fit includes unbounded spiking shapes. |
| R PearsonDS fitting [S3] | pearsonFitML performs numerical subclass fits and selects a Pearson type. | Documented preliminary optimizer; no global certificate. Its comment about symmetric subclasses having probability-zero selection concerns the larger Pearson system, not impossibility of symmetric-family estimation. |
| R qGaussian 0.1.8 [S4, P5] | dqgauss, pqgauss, cqgauss (quantiles), rqgauss; sig corresponds to s. qbymc estimates q by a medcouple relation. | qbymc is not joint ML. The normal member is rejected; support and vector-quantile defects are reproduced below. |
| Python SciPy t, norm, rdist [S5] | pdf/cdf/ppf/rvs and generic fit methods. Bounded branch: rdist with c = 2(m + 1) = 1 − 1/eta, loc = mu, scale = h. | rdist permits c > 0; this signed-eta branch uses c > 1. The components do not by themselves supply a unified certified fit. |
| SciPy generic fitting [S6] | Shape/location/scale optimization, with parameter holds. | Documentation explicitly disclaims global optimality and describes finite support penalties. Unrestricted t or spiking rdist likelihoods have the mathematical nonexistence problems proved above. No separate Python defect reproduction was performed. |

**Reproduced source defects / omissions in qGaussian 0.1.8.** The CRAN source
mirror is pinned at revision 1f7d9a66e32808cad24acaef9007c9bb45378a00.
[Provenance and SHA-256 hashes](2026-11-10-qgauss-lit-data/sources.json),
[density source](2026-11-10-qgauss-lit-data/dqgauss.R), and
[quantile source](2026-11-10-qgauss-lit-data/cqgauss.R) are retained.
No package installation was needed.

- dqgauss(2, q = 0) returns 0.1443375672974064, although the support is
  [−sqrt(3), sqrt(3)] and the correct density there is zero. The implementation
  evaluates the power expression beyond support.
- dqgauss(0, q = 1) prints a message and returns a character value. The normal
  member is unsupported rather than evaluated by its limit.
- cqgauss(c(.25, .75), q = 0) fails under R 4.6.1 in its scalar logical
  validation, before any zipfR call. This is a modern-R vector-input failure.

These are source-level reproductions here, not assertions that maintainers
have acknowledged upstream issues. They disqualify those routes as unchecked
oracles for this work. No changes to the repository's settled d/p/q/r code
are proposed by this literature job.

## 6. Citation verification ledger

All identities below were verified through the linked primary publication,
institutional, archive, or official software records. No bibliographic entry
below is carried solely on model memory. Access limitations concern claim
support, not whether the works exist.

| ID | Verified citation / stable identifier | Inspected evidence and limits |
|---|---|---|
| P1 | I. Papastathopoulos and J. A. Tawn (2013), “A generalised Student's t-distribution,” Statistics & Probability Letters 83(1), 70–77. [DOI: 10.1016/j.spl.2012.09.002](https://doi.org/10.1016/j.spl.2012.09.002); [Edinburgh record](https://www.research.ed.ac.uk/en/publications/a-generalised-students-t-distribution/). | Identity verified against publisher and institutional metadata; abstract and publisher introduction preview inspected. Full inference and algorithm claims remain unverified. The [author's publications list](https://wp.lancs.ac.uk/tawn/publications/) has a conflicting abbreviated year/volume entry; the publisher and institutional record support 2013, volume 83. |
| P2 | R. L. Smith (1985), “Maximum likelihood estimation in a class of nonregular cases,” Biometrika 72(1), 67–90. [DOI: 10.1093/biomet/72.1.67](https://doi.org/10.1093/biomet/72.1.67). | Publisher identity and abstract verified. Its alpha > 2, alpha = 2 and 1 < alpha < 2 distinctions were inspected. Full theorem conditions for the present joint two-endpoint problem were not checked. |
| P3 | C. Liu and D. B. Rubin (1995), “ML estimation of the t distribution using EM and its extensions, ECM and ECME,” Statistica Sinica 5(1), 19–39. [Official full text](https://www3.stat.sinica.edu.tw/statistica/oldpdf/A5n12.pdf). | Full text inspected, especially p. 20 and §§4–6. Supports positive-df algorithm descriptions and warnings about likelihood spikes; not a certificate for signed-eta global ML. |
| P4 | X. Wang and J. Yan (2013), “Practical Notes On Multivariate Modeling Based on Elliptical Copulas,” Journal de la société française de statistique 154(1), 102–115. [Numdam record and full text](https://numdam.org/item/JSFS_2013__154_1_102_0/). | Identity verified; Table 1, p. 105 inspected for elliptical II/VII generator conventions. Not used as an iid free-ML existence theorem. |
| P5 | W. S. de Lima and E. L. de Santa Helena (2017), “qGaussian: Tools to Explore Applications of Tsallis Statistics.” [arXiv:1703.06172v1](https://arxiv.org/abs/1703.06172v1). | Preprint identity and full text inspected, §§2–3. Supports q parametrization and software / medcouple descriptions. No peer-reviewed publication status is assumed. |
| S1 | PearsonDS 1.3.1, [Pearson II documentation](https://search.r-project.org/CRAN/refmans/PearsonDS/html/PearsonII.html). | Details formula and beta-wrapper convention inspected. |
| S2 | PearsonDS 1.3.1, [Pearson VII documentation](https://search.r-project.org/CRAN/refmans/PearsonDS/html/PearsonVII.html). | Details formula and t-wrapper convention inspected. |
| S3 | PearsonDS 1.3.1, [pearsonFitML documentation](https://search.r-project.org/CRAN/refmans/PearsonDS/html/pearsonFitML.html). | Description, Details and Note inspected. |
| S4 | qGaussian 0.1.8, [CRAN package record](https://cran.r-project.org/web/packages/qGaussian/index.html), package DOI 10.32614/CRAN.package.qGaussian. | Version and identity verified; pinned source files inspected and tested as above. |
| S5 | SciPy documentation, [rdist](https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.rdist.html) and [t](https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.t.html), documentation reporting v1.18.0 at retrieval. | Distribution notes and method inventories inspected. Version is the documentation version, not a claim about a locally installed Python environment. |
| S6 | SciPy, [rv_continuous.fit](https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.rv_continuous.fit.html). | Notes on penalties and lack of global guarantees inspected. |

## 7. Precise open items

These are limits of this check, not claims that the questions are open in
the world literature.

1. **P1 full-text inference and optimization.** Publisher access was blocked;
   institutional records and repeated title/author searches did not provide
   the full paper. Obtain pp. 70–77 and verify its exact constraints,
   parameter notation, optimizer, and any existence or normal-crossing
   theorems before attributing those results to it. Its identity and
   negative-df scope are verified.
2. **Joint endpoint asymptotics.** The −1/3 information threshold is proved
   here. A checked joint limit law and normalization with both endpoints and
   shape free, particularly at m = 1 and at uniformity, still needs Smith's
   full theorem conditions or an independent proof. Fixed-shape existence
   and the uniform sample-interval calculation do not supply that joint law.
3. **Restricted global free-fit certificate.** The unrestricted problem is
   settled by explicit unbounded paths. A usable restricted model still needs
   exhaustive face/escape analysis and global comparisons, including ties,
   the uniform and normal members, and any Student-df restriction face.
   No inspected source or software documentation supplies that certificate.
   Absence from this bounded search is not evidence of nonexistence of such
   literature.

## 8. Completed validation and scope of changes

All local runs below finished with exit status zero on the workstation.
Build/test computations used the supplied slot wrappers, nice scheduling,
and one BLAS/OpenMP thread per worker.

| Check | Result |
|---|---|
| Base-R evidence check, one reserved slot | Three software behaviors reproduced; Gaussian information arithmetic passed. |
| just regen, one reserved slot with MAKEFLAGS = −j1 | Passed; generated package only. |
| R dev tier, same test_local.R driver as just test, TESTTHAT_CPUS = 1, one reserved slot after regeneration | FAIL 0, WARN 0, SKIP 154, PASS 23031; elapsed 927.596 seconds. Serial execution used an available single slot while the four-slot queue was occupied. |
| just cpp-test, four reserved slots | 106/106 passed; test elapsed 549.84 seconds. |
| just cpp-test-no-long-double, four reserved slots, GNU 15.2.0 | 130/130 passed; test elapsed 574.96 seconds. This includes the required workstation precision-mode run. |
| simbox attempts | Unreachable, exit 75, including a retry of the no-long-double command. Fallback local suites passed; no GCC 13 / simbox pass is claimed. |

No fitting, d/p/q/r, test-infrastructure, public-fitter, dossier, or binding
changes were made. Consequently the conditional R full-tier, JS and site
checks were not triggered. Default fits and goldens were left unchanged.
No commits, board edits, package installations, or publication actions were
performed.

## 9. Search protocol

The search used 15 web queries on 2026-11-10, without a lower publication-year
cutoff, followed by direct primary-source links, official documentation,
institutional records and source-file inspection. It targeted ordinary iid
location–scale–shape ML. q-exponential estimation, generalized Fisher
information, histogram curve fitting and known-scale location-only results
were not treated as solutions to the joint problem. The local research-file
inventory contained no full-text copy of P1 or P2. The stopping rule was the
requested approximately 15-query budget plus checking the resulting sources.

Query strings:

    q Gaussian maximum likelihood estimation q parameter location scale Pearson type II VII
    Smith 1985 maximum likelihood estimation class nonregular cases alpha 2 endpoint Biometrika
    "q-Gaussian" "maximum likelihood" estimation Fisher
    R qGaussian Tsallis distribution dqgauss fitqgauss
    Student t maximum likelihood degrees freedom negative extended Pearson II Fisher information normal
    Papastathopoulos Tawn generalised Student t 2013 pdf
    qGaussian CRAN github dqgauss R source
    Python qGaussian distribution package fit scipy pearson2 rvs
    "Papastathopoulos" "t-distribution" "pdf" 2013 Tawn
    "Student" "maximum likelihood" "degrees of freedom" EM Liu Rubin 1995
    site.search.r-project.org PearsonDS pearsonII pearsonVII pearsonFitML
    "qGaussian" python fitting github
    "elliptical" "Pearson type II" "VII" distribution density
    "generalised Student" "Papastathopoulos" likelihood estimation normal information
    "A generalised Student’s t-distribution" filetype:pdf Papastathopoulos Tawn -site:scribd.com -site:researchgate.net
