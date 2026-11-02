# Existence-8 report — 2026-11-02

Completed TASK-101.6.4.1 for pareto, pareto2, pareto3, pareto4, fpareto and gpd. The six dossiers now state mathematical existence separately from uniqueness and numerical search guarantees. Five matching binding error strings were updated. Fourteen structural hold statuses were corrected from **no-mle** to **not-implemented**, with the exact counterexamples below.

## Tested source and scope

- Worktree: /home/jonas/Files/programming/univariateML.wt/existence-8
- Branch: codex/existence-8
- Base revision: 13d3ad0fed44815f6bc3b6ef677b6d3d87006c59
- Validation completed locally on 2026-11-02, before this report was written.

SHA-256 of the unified source diff for the six README files and five changed common binding headers:

    2334134c27ae3f9fa5f20e552ca8d216cb521f479235a896202b7da554df31ec

A YAML comparison against the base revision verified every protected field, allowing only the fourteen hold-status corrections listed below. Supports, coefficient domains and kinds, held sets, fitting bounds, exemptions, defaults, fit arguments, relations, examples, fit/catalog/inference fields and limit statuses/firmness/numerical flags were preserved. Binding changes are message literals. Numerical fitting code, density code and fixtures were preserved. Existing R test regexes remain valid; the revised advice explicitly retains the unbounded-likelihood consequence.

Proofs use exact arithmetic and the declared endpoint-density versions. Numerical tolerances are stated separately. New existence arguments are direct mathematical arguments. No external existence or uniqueness citation was added, and no web searches were used.

## Family statements and grades

| Family | Statement | Grade and argument |
|---|---|---|
| [pareto](../../../univariateML-cpp/include/cppunivariate/distributions/pareto/README.md) | Both free: unique finite MLE for nonconstant positive data; constant data are unbounded. Held shape permits constant data. Held endpoint gives a unique finite shape unless every observation equals it; an endpoint above an observation gives zero likelihood. | **Proved.** Likelihood increases in endpoint up to the sample minimum. At fixed endpoint its shape part is $n\log\alpha-\alpha\sum_i\log(x_i/b)$, strictly concave with the stated root or unbounded case. No open mathematical case within this positive-sample contract. |
| [pareto2](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md) | All free: unbounded for every sample. Held threshold: exactly the Lomax existence criterion on positive excesses, with a possible exponential supremum; either additional coefficient held gives a unique finite fit. | **Proved existence criterion and conditional uniqueness; unknown joint uniqueness.** Shifting gives Lomax with $\kappa=\alpha,\lambda=1/\sigma$. The unrestricted collapse has exponent $\alpha(n-k)-k$. The numerical profile search is not a global certificate. |
| [pareto3](../../../univariateML-cpp/include/cppunivariate/distributions/pareto3/README.md) | All free: unbounded. Held threshold: unique finite fit for nonconstant positive excesses. Holding shape gives unique scale even for constant data; holding scale gives unique shape except when all excesses equal scale. | **Proved for the supported held-threshold fits.** On log excesses the likelihood is strictly concave in inverse spread and standardized location. Constant-data concentration gives the exceptions. No general uniqueness claim is made for unimplemented free-threshold submodels. |
| [pareto4](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md) | All free: unbounded. Held threshold: Burr existence and boundary conditions transfer, including Weibull/Pareto faces and the separate fixed-scale cases. | **Proved unrestricted singular path and stated conditional results; unknown complete joint criterion and joint uniqueness.** Fixed upper shape gives a concave log-data problem; fixed power gives Lomax on powered excesses. Joint numerical face comparisons remain uncertified. |
| [fpareto](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md) | All free: unbounded. Held threshold: GB2 existence conditions transfer. Both beta shapes held give a unique power/scale fit on nonconstant excesses. Power and scale held give a unique two-beta-shape fit on nonconstant excesses; concentration exceptions remain for constant data. | **Proved singular path and stated conditional results; unknown joint existence criterion and joint uniqueness.** Lower-end power is $\gamma\tau$. The held-beta-shape inner likelihood is strictly concave. With power and scale held, a beta transformation gives a minimal exponential family whose mean sufficient statistic is interior for nonconstant data. Outer basin search and face enumeration remain numerical. |
| [gpd](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md) | All free: unbounded. With threshold held and shape fitted in $[-1,\infty)$, positive excesses admit a finite maximum; any threshold tie makes the joint free-scale/free-shape likelihood unbounded. Fixed-shape scale conditions follow from a monotone score. Fixed scale gives a finite shape maximum, possibly flat. With scale held and threshold free, a finite maximum exists within the fitting shape range, although that API is unimplemented. | **Proved existence statements; unknown joint uniqueness.** Density envelopes and closed-uniform upper semicontinuity prove attainment. The scale score proves the held-shape cases, including the finite unattained equality case. The proofs are expanded below. Numerical profile coverage remains uncertified. |

For Pareto III, put $z_i=\log(x_i-\mu)$, $b=\gamma>0$, and $a=\gamma\log\sigma$. Apart from a data term the log-likelihood is

$$
n\log b-2\sum_i\log\{2\cosh((bz_i-a)/2)\}.
$$

Its second directional derivative is

$$
-\frac{n v_b^2}{b^2}
-\frac12\sum_i\operatorname{sech}^2((bz_i-a)/2)(z_i v_b-v_a)^2<0.
$$

For nonconstant data, profiling location gives likelihood tending to zero at both spread extremes. Holding shape fixes $b$; holding scale imposes the linear restriction $a=b\log\sigma$. These give the conditional uniqueness statements without relying on the numerical optimizer.

## Every hold-status correction

All fourteen changes are **no-mle → not-implemented**. The held sets and runtime rejection policy are preserved. A finite global maximum for one admissible sample and held value disproves a structural claim that no MLE exists for that held set. The new tag does not claim existence for every sample or every held value.

Use $x=(1,2,3)$. In the Pareto-II reductions, the estimated threshold is $\mu=1$.

| Family | Held set | Exact witness |
|---|---|---|
| pareto2 | [shape] | A |
| pareto2 | [scale] | B |
| pareto2 | [shape, scale] | C |
| pareto3 | [shape] | A, with shape $=1$ |
| pareto3 | [shape, scale] | C, with shape $=1$ |
| pareto4 | [shape1, shape2] | A, with both shapes $=1$ |
| pareto4 | [shape2, scale] | B, with shape2 $=1$ |
| pareto4 | [shape1, shape2, scale] | C, with both shapes $=1$ |
| fpareto | [shape1, shape2, shape3] | A, with all three shapes $=1$ |
| fpareto | [shape2, shape3, scale] | B, with shape2 $=$ shape3 $=1$ |
| fpareto | [shape1, shape2, shape3, scale] | C, with all three shapes $=1$ |
| gpd | [xi] | D |
| gpd | [sigma] | E |
| gpd | [sigma, xi] | F |

**A.** Hold Pareto-II shape at one. For every positive scale the likelihood increases in $\mu$, so profile at $\mu=1$. On excesses $(0,1,2)$, the scaled scale score is

$$
2\left\{\frac1{\sigma+1}+\frac2{\sigma+2}\right\}-3.
$$

It decreases strictly from $1$ to $-3$. Its unique root is

$$
\widehat\sigma=\frac{\sqrt{33}-3}{6}>0.
$$

**B.** Hold Pareto-II scale at one. Threshold again profiles to one, and the strictly concave shape likelihood has $\widehat\alpha=3/\log6$.

**C.** Hold Pareto-II shape and scale at one. The remaining threshold likelihood increases strictly up to $\mu=1$, where its value is finite. Pareto III at shape one, Pareto IV at power one, and Feller–Pareto at power and lower beta shape one reduce exactly to the corresponding Pareto-II models. Thus A–C are global maxima in each listed held submodel, rather than finite candidate points in a larger unchecked search.

**D.** Hold GPD shape at zero. Profiling the exponential scale gives $\widehat\sigma=\bar x-\mu$. Its likelihood increases up to $\widehat\mu=1$, where $\widehat\sigma=1$. This is an attained finite global maximum.

**E.** Hold GPD scale at one. The data range is two. Any feasible negative shape satisfies $\xi\ge-1/2>-1$, even in the unrestricted shape domain. Threshold therefore profiles to one. The shape likelihood is continuous and positive on $(-1/2,\infty)$, tends to zero at both ends, and hence attains a finite maximum. This witness does not depend on the fitting floor at $-1$.

**F.** Hold GPD scale at three and shape at minus one. The model is uniform. Every $\mu\in[0,1]$ gives likelihood $3^{-3}$; outside that interval some observation is outside support. This is an attained plateau.

These arguments use the declared closed endpoints and finite endpoint-density versions. In particular, they use the package's R7a convention at the uniform GPD endpoint. The old universal free-threshold reasons were mathematically too strong.

The other **22 no-mle hold records** retain constructive unbounded paths. For Pareto III a free power can be chosen below one. For Pareto IV and Feller–Pareto, a free lower-end power can be made below one; when that power is held but upper shape and scale remain free, the upper-tail collapse exponent can be made arbitrarily small. Pareto II has the same scale-collapse construction. For fully free GPD, choose a positive shape large enough for minimum-tie scale collapse. These constructions work for every nonempty sample and admissible held values in those remaining structural cases.

## GPD existence proofs and qualifications

### Positive excesses with threshold held

Work within $\xi\ge-1$ and write $y_i=x_i-\mu>0$, $M=\max_i y_i$. For one observation the scale score has sign $y-\sigma$, so maximizing over scale gives

$$
f(y;\sigma,\xi)\le \frac{g(\xi)}y,\qquad
g(\xi)=(1+\xi)^{-1-1/\xi},\quad
g(0)=e^{-1},\quad g(-1)=1.
$$

Since $g(\xi)\to0$ as $\xi\to\infty$, shape cannot escape in a maximizing sequence. The uniform member $(\xi,\sigma)=(-1,M)$ supplies the positive reference likelihood $M^{-n}$. Also $f\le1/\sigma$, excluding arbitrarily large scales.

For a bounded shape interval $[-1,C]$, take $C\ge1/2$. At small scale the following bounds tend uniformly to zero for each fixed $y>0$:

$$
\begin{array}{ll}
\xi\le0:
& f\le \sigma^{-1}\exp\{-y/(2\sigma)\},\\
0\le\xi\le1/2:
& f\le \sigma^{-1}(1+y/(2\sigma))^{-2},\\
1/2\le\xi\le C:
& f\le (2/y)(1+y/(2\sigma))^{-1/C}.
\end{array}
$$

For the first bound, support feasibility gives $-\xi\le\sigma/M\le1/2$ once scale is small enough. The other two follow directly from the survival factor and monotonicity of $\log(1+\xi z)/\xi$ for $z>0$. Infeasible parameters have zero likelihood.

Thus maximizing sequences remain in a compact scale/shape set. At an ordinary negative-shape support edge the density tends to zero. At $\xi=-1$, the closed uniform endpoint gives upper semicontinuity: the bound $f\le1/\sigma$ bounds every limiting endpoint factor by its attained uniform value. The joint likelihood therefore attains a finite maximum. No joint uniqueness or global numerical-search certificate follows from this argument.

### Held shape, threshold ties and the equality case

For $\xi>-1$, with some positive excess, the total scale score multiplied by scale is

$$
S(\sigma)=(1+\xi)\sum_i\frac{y_i}{\sigma+\xi y_i}-n.
$$

Its derivative is strictly negative on the feasible interval. For $k$ threshold ties and held $\xi>0$, its limit at zero is $(n-k)/\xi-k$. A negative limit gives unbounded likelihood. At equality, $S(\sigma)<0$ for every positive scale, so the finite scale-zero path limit is the global unattained supremum for this fixed-shape problem. This settles the equality case that the previous prose left qualified.

At $\xi=0$, the root is the mean excess. At $\xi=-1$, likelihood is $\sigma^{-n}$ on $\sigma\ge M$, with maximum at $M>0$. If all excesses vanish, free scale instead gives unbounded likelihood.

### Scale held

With threshold also held, any positive excess excludes infinite shape through the envelope; zero excess factors are the fixed constant $1/\sigma$. The lower feasible shape edge is either the attained uniform member or has zero likelihood. Hence a finite shape maximum exists. All-zero excesses give a flat shape likelihood.

With threshold free, put $R=\max x_i-\min x_i$. If $\sigma\ge R$, a uniform member covers the data and attains the density-product bound $\sigma^{-n}$. If $0<\sigma<R$, feasibility gives $\xi\ge-\sigma/R>-1$; likelihood increases as threshold rises to the minimum. At that threshold the shape likelihood vanishes at both ends of $(-\sigma/R,\infty)$, so a finite maximum exists. Threshold estimation remains outside the API.

## Reader-facing changes and remaining gaps

The dossiers now explain coefficient meanings, held-coefficient effects and numerical boundary comparisons. R calls, warning classes and API conventions are in R notes. Dated diagnostic, portability, precision and seed-subsampling material is in Decision history. Limit paths use the dossier symbols, and detect clauses describe data conditions or numerical comparisons.

The corrected disagreement was the old no-mle label for the fourteen held sets above. No limit status was changed. Pareto's held-endpoint collapse detector now agrees with its existing fixed-[b] qualifier; its existence statement separately covers the fully free constant-data case.

The following gaps remain explicit:

- **Joint theory/search:** Pareto II joint uniqueness, Pareto IV and Feller–Pareto complete joint criteria/uniqueness, and GPD joint uniqueness are not established here. Lomax/Burr/GB2 searches and GPD profile enumeration retain their numerical qualifications. In particular, GPD samples/refines profile roots, and its fixed-scale search uses a log-distance window [-32,32]. A global exhaustion argument or certified likelihood bounds remain missing. The new envelopes establish existence and potential tail bounds, rather than validating that implementation.
- **Availability:** The fourteen free-threshold submodels remain unimplemented by interface policy. Pareto II–IV and Feller–Pareto also require strictly positive excesses for fitting, even though some held submodels have finite density and mathematical maxima at the closed threshold. The new classifications do not add those fitting capabilities.
- **Historical shared analysis:** The free-GPD-threshold paragraph in [ml-behaviour.tex](../ml-behaviour.tex) states universal non-existence across held cases. Closed-support exponential maxima and uniform plateaus disprove that wording. That file was excluded from this job and needs a separate correction.
- **Existing limit messages:** The [Pareto IV binding](../../../bindings/common/distributions/pareto4/fit.hpp) and [Feller–Pareto binding](../../../bindings/common/distributions/fpareto/fit.hpp) say a held-endpoint Pareto fitter is unavailable. The [Pareto binding](../../../bindings/common/distributions/pareto/fit.hpp) already accepts held b. Recommendations need review with the shifted endpoint and pointwise-limit penalty retained: $Z\log2$ for Burr/Pareto IV, and $\tau Z\log2$ for a held opposite beta shape in Feller–Pareto. These non-hold message strings were outside the authorized edits.
- **Other-family help:** The all-family Rd command reports 26 pre-existing diagnostics in man/mlst.Rd, including raw math macros, lost braces and an empty dated section. The st dossier and code generators are unchanged from the base revision. These diagnostics are outside the six-family ownership.

## Validation

All runs used the local workstation. Environment: Ubuntu 26.04.1 LTS, x86_64 Linux, R 4.6.1 (2026-06-24), GNU C++ 15.2.0, OpenBLAS 0.3.32. Slot wrappers used nice level 10 and pinned OpenMP/OpenBLAS/MKL to one thread. Heavy commands reserved all four slots; R full/dev used four workers. Temporary files and logs were under this worktree's build directory.

| Check | Final result |
|---|---|
| just regen | Passed; full/dev subsequently regenerated the final dossier text again. |
| Requested targeted R filter | **FAIL 0, WARN 0, SKIP 7, PASS 7826**. Skips require full tier; full tier below ran without skips. |
| Final Rd check | **No diagnostics for the six owned families.** Only the 26 outside-scope st diagnostics above were printed. |
| Protected-field/message comparison | Passed for all six dossiers; exactly the fourteen listed status exceptions; all declared hold reasons match common binding strings. |
| python3 site/scripts/data.py | Passed; 99 families. Final repeat took 1.47 seconds inside the command, excluding slot wait. |
| just relations | **157 edges, 0 failed**; symbolic and numerical checks retain their distinct meanings. |
| just cpp-test | **106/106 passed**, native dev tier; ctest runtime 49.38 seconds. |
| just test-full | **FAIL 0, WARN 0, SKIP 0, PASS 41705**; runner duration 240.0 seconds. |
| Final just test | **FAIL 0, WARN 0, SKIP 151, PASS 20102**; declared tier skips; process elapsed 110.882 seconds. |
| Endpoint/envelope check below | Passed: three witness templates, eight cross-family log-density-vector comparisons, four uniform thresholds, exponential likelihood, 264 envelope comparisons and 33 equality cases. |
| git diff --check | Passed before report creation; repeated after writing this report. |

The first targeted run had three failures, all missing-argument regex matches for the word unbounded in the revised Pareto III/IV/Feller–Pareto advice. The advice was corrected to state that true consequence explicitly. Its numerical assertions passed. The final targeted run and both full/dev suites above were green. No test regex edit was needed.

Golden regression, conformance, symbolic identities, numerical density comparisons and the mathematical proofs support different claims. The finite-grid check below validates coordinates and endpoint versions on its stated grid; it does not establish a global optimizer certificate.

### Commands

From the worktree root, with build/tmp created:

    export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
    /tmp/umvl-cpu/run4 just regen

From univariateML-r:

    /tmp/umvl-cpu/run4 env UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions Rscript -e "testthat::test_local('.', filter = 'dist-(pareto|pareto2|pareto3|pareto4|fpareto|gpd)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)"
    /tmp/umvl-cpu/run Rscript -e 'for (f in Sys.glob("man/ml*.Rd")) { r <- tools::checkRd(f); r <- r[!grepl("Non-ASCII", r)]; if (length(r)) print(r) }'

From the root:

    /tmp/umvl-cpu/run python3 site/scripts/data.py
    /tmp/umvl-cpu/run just relations
    /tmp/umvl-cpu/run4 just cpp-test
    /tmp/umvl-cpu/run4 just test-full
    /tmp/umvl-cpu/run4 just test

After the dev suite, final metadata comparison, site extraction, Rd checking and the numerical check were run sequentially under one /tmp/umvl-cpu/run slot. Root residual and uniform/exponential likelihood checks use 1e-12 absolute tolerance. Cross-family log-density agreement uses 1e-11. Envelope inequalities and equality at scale y use a practical 1e-10 log-density margin. The grids are deterministic; no random seed is involved.

## Reproducible endpoint and envelope check

After regeneration, save the following as a worktree scratch R file and run it from the root through /tmp/umvl-cpu/run Rscript. This is the exact numerical check used for this report.


```r
pkgload::load_all('univariateML-r', quiet = TRUE, helpers = FALSE)
f <- function(id, ...) do.call(get(paste0('d', id), asNamespace('univariateML')), list(...))
x <- c(1, 2, 3); s <- (sqrt(33) - 3)/6
stopifnot(abs(2*(1/(s+1)+2/(s+2))-3) < 1e-12)
stopifnot(abs(3/(3/log(6))-log(6)) < 1e-12)
for (p in list(c(1,s), c(3/log(6),1), c(1,1))) {
  a <- p[1]; b <- p[2]
  ref <- f('pareto2', x=x, shape=a, scale=b, min=1, log=TRUE)
  stopifnot(all(is.finite(ref)))
  stopifnot(max(abs(ref-f('pareto4',x=x,shape1=a,shape2=1,scale=b,min=1,log=TRUE))) < 1e-11)
  stopifnot(max(abs(ref-f('fpareto',x=x,shape1=a,shape2=1,shape3=1,scale=b,min=1,log=TRUE))) < 1e-11)
  if (a==1) stopifnot(max(abs(ref-f('pareto3',x=x,shape=1,scale=b,min=1,log=TRUE))) < 1e-11)
}
for (m in c(0,.25,.5,1)) stopifnot(abs(sum(f('gpd',x=x,mu=m,sigma=3,xi=-1,log=TRUE))+3*log(3)) < 1e-12)
stopifnot(abs(sum(f('gpd',x=x,mu=1,sigma=1,xi=0,log=TRUE))+3) < 1e-12)
count <- 0L
for (xi in c(-1,-.999,-.9,-.5,-.1,0,1e-8,.1,1,10,100)) for (y in c(.25,1,4)) {
  lg <- if (xi==-1) 0 else if (xi==0) -1 else -(1+1/xi)*log1p(xi)
  for (b in c(.01,.1,.25,.5,1,2,10,y)) {
    actual <- f('gpd',x=y,mu=0,sigma=b,xi=xi,log=TRUE)
    stopifnot(!is.na(actual), actual <= lg-log(y)+1e-10)
    count <- count+1L
  }
  stopifnot(abs(f('gpd',x=y,mu=0,sigma=y,xi=xi,log=TRUE)-(lg-log(y))) < 1e-10)
}
cat('PASS: three Pareto-II witness templates, their supported family reductions, four closed uniform thresholds, and exponential endpoint likelihood.\n')
cat('PASS:',count,'GPD density-envelope comparisons and 33 equality cases; log-density margins 1e-10 (envelope), 1e-11 (reductions).\n')
```

## Artifact delivery

The report file was written at the requested worktree path. The prescribed show command was attempted, but the artifact viewer returned HTTP 403 because this worktree is outside its configured readable roots. Display registration remains unavailable; the source report is complete.
