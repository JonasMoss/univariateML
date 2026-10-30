# Dossier front-matter fields, first batch (2026-10-30)

TASK-101.6.4; tested base `1bd8c44e1d7650aacb864a338a0f9be0f11f2f0d` plus the uncommitted changes in this worktree.

## Schema and consumers

Added optional coefficient `symbol`, `summary`, `classification` plus `classification_reason`, `aliases`, and `key_references`.
Classification remains a string for existing site consumers; its reason is qualitative. Empty alias lists mean no additional verified name is supplied.
Key references are one-based indices (one to three, distinct) into the unchanged string-list reference format. The site resolves them to strings; R retains the full bibliography in order.
The R loader validates these fields and preserves them without using catalog metadata or symbols in help. The quality parser needs no change: API coordinates and `log_density` remain untouched.
`limits.kind: nonexistence` permits an R6 unbounded-likelihood record without a fictitious limit distribution. R wrapper codegen renders these two new records directly.
Intended R-help differences: exp/gamma non-existence notices, verified bibliography corrections/additions, and the edited condition wording/punctuation in the ten dossiers. Other catalog fields do not enter help.
No numerical implementation, default fit, golden, README prose, generated d/p/q/r, board, or Git history was changed. Additional R codegen file used as authorized: `bindings/r/scripts/codegen_wrappers.R`.

## Ten classifications

| Family | Classification | Reason |
|---|---|---|
| beta | classical | A standard two-shape model for proportions and probabilities. |
| binom | classical | A standard sampling model for binary trial counts. |
| exp | classical | A standard waiting-time and reliability model. |
| gamma | classical | A standard model for waiting times and positive skewed data. |
| gb2 | specialist | Used in income-distribution and inequality modelling. |
| gev | specialist | A standard model within extreme-value analysis. |
| hyper | classical | A standard finite-population sampling model. |
| logitnorm | specialist | Used for continuous proportions and logistic-normal compositional models. |
| norm | classical | A standard model for measurement errors and sampling approximations. |
| sstd | specialist | Used for skewed heavy-tailed errors, including financial returns. |

## Bibliographic and name verification

Verified on 2026-10-30 using web searches and publisher/index records (17 search queries; no installations). Authors, publication year, title and venue were checked; online-posting dates were distinguished from issue years.
- Johnson–Kotz–Balakrishnan, Volume 1: [Wiley](https://www.wiley-vch.de/en/areas-interest/mathematics-statistics/continuous-univariate-distributions-978-0-471-58495-7) confirms 1994; corrected exp/norm/gamma from 1995. Volume 2 (beta), 1995: [Wiley](https://www.wiley-vch.de/de/fachgebiete/mathematik-und-statistik/continuous-univariate-distributions-978-0-471-58494-0). No book DOI added for these volumes.
- Johnson–Kemp–Kotz (2005), third edition: [Wiley](https://doi.org/10.1002/0471715816); DOI added to binom/hyper. Only the standard book is selected for hyper; the existing Wang reference remains unmodified.
- Choi–Wette (1969), Technometrics 11(4), 683–690: [publisher](https://doi.org/10.1080/00401706.1969.10490731); DOI added; gamma selects this estimator paper and the standard book.
- Olkin–Petkau–Zidek (1981), JASA 76(375), 637–642: [publisher](https://doi.org/10.1080/01621459.1981.10477697); DOI added; binom selects this and the standard book. Existing Carroll–Lombard and DasGupta–Rubin entries were retained, not newly audited or marked key.
- McDonald (1984), Econometrica 52(3): [issue contents](https://www.jstor.org/stable/i332767) and [RePEc](https://ideas.repec.org/a/ecm/emetrp/v52y1984i3p647-63.html); added original GB2 source and verified DOI 10.2307/1913469. Page range omitted because the records disagree (647–665 versus 647–663).
- Fernández–Steel (1998), JASA 93(441), 359–371: [publisher](https://doi.org/10.1080/01621459.1998.10474117); DOI added. This supports the skewing construction, not a claim that the current optimizer is certified.
- Coles (2001), An Introduction to Statistical Modeling of Extreme Values: [Springer](https://doi.org/10.1007/978-1-4471-3675-0); added standard GEV reference and DOI.
- Aitchison–Shen (1980), Biometrika 67(2), 261–272: [Oxford](https://doi.org/10.1093/biomet/67.2.261); DOI added, logistic-normal alias checked against the title/abstract.
- Names: Gaussian ([NIST](https://csrc.nist.gov/glossary/term/Normal_Distribution)); beta of the first kind ([NIST index](https://nvlpubs.nist.gov/nistpubs/jres/65B/jresv65Bn1p23_A1b.pdf)); negative exponential ([textbook excerpt](https://shop.lehrmittelverlag.lu.ch/annot/564C42696D677C7C393738303338373239373432327C7C504446.pdf?sq=1)); GB2 and skew Student names follow the verified sources above. No historical attribution or extra unverified alias was added.

## Symbols (API name = TeX; all 98 families)

Conventional notation is adapted to the declared coordinates: e.g. gamma uses shape k and scale theta; GB2 uses shape3=p, shape1=q, shape2=a, scale=b. No parameter is renamed numerically.
Related transformed and inverse families share symbols. The mapping is editorial notation, not a claim that every reference uses identical letters; no coordinate transformation is implied.

| Family: symbols | Family: symbols |
|---|---|
| bbinom: `size=n`, `alpha=\alpha`, `beta=\beta` | beta: `shape1=\alpha`, `shape2=\beta` |
| betapr: `shape1=p`, `shape2=q`, `scale=b` | bhatt: `mu=\mu`, `sigma=\sigma`, `a=a` |
| binom: `size=n`, `prob=p` | bnbinom: `size=r`, `alpha=\alpha`, `beta=\beta` |
| burr: `shape1=k`, `shape2=c`, `scale=s` | cauchy: `location=\mu`, `scale=\sigma` |
| cmb: `size=n`, `prob=p`, `nu=\nu` | cmp: `lambda=\lambda`, `nu=\nu` |
| dgamma: `shape=k`, `rate=\lambda` | dlaplace: `location=\mu`, `scale=b` |
| dnorm: `mean=\mu`, `sd=\sigma` | dunif: `min=a`, `max=b` |
| dweibull: `shape1=q`, `shape2=\beta` | emg: `mu=\mu`, `sigma=\sigma`, `lambda=\lambda` |
| exp: `rate=\lambda` | fatigue: `alpha=\alpha`, `beta=\beta`, `mu=\mu` |
| foldnorm: `mu=\mu`, `sigma=\sigma` | fpareto: `shape1=\alpha`, `shape2=\gamma`, `shape3=\tau`, `scale=\sigma`, `min=\mu` |
| frechet: `lambda=\lambda`, `mu=\mu`, `sigma=\sigma` | gamma: `shape=k`, `scale=\theta` |
| gb2: `shape1=q`, `shape2=a`, `shape3=p`, `scale=b` | ged: `mean=\mu`, `sd=\sigma`, `nu=\nu` |
| genbeta: `shape1=p`, `shape2=q`, `shape3=a`, `scale=b` | geom: `prob=p` |
| gev: `mu=\mu`, `sigma=\sigma`, `xi=\xi` | gompertz: `a=a`, `b=b` |
| gpd: `mu=\mu`, `sigma=\sigma`, `xi=\xi` | gumbel: `mu=\mu`, `sigma=\sigma` |
| hcauchy: `mu=\mu`, `sigma=\sigma` | hnorm: `mu=\mu`, `sigma=\sigma` |
| ht: `mu=\mu`, `nu=\nu`, `sigma=\sigma` | huber: `mu=\mu`, `sigma=\sigma`, `epsilon=\epsilon` |
| hyper: `m=K`, `N=N`, `k=n` | invburr: `shape1=k`, `shape2=c`, `scale=s` |
| invexp: `scale=\theta` | invgamma: `alpha=\alpha`, `beta=\beta` |
| invgauss: `mean=\mu`, `shape=\lambda` | invparalogis: `shape=\alpha`, `scale=\theta` |
| invpareto: `shape=\alpha`, `scale=\theta` | invtrgamma: `shape1=k`, `shape2=p`, `scale=a` |
| invweibull: `shape=k`, `scale=\lambda` | kumar: `a=a`, `b=b` |
| laplace: `mu=\mu`, `sigma=b` | lgamma: `shapelog=k`, `ratelog=\lambda` |
| lgser: `theta=\theta` | llogis: `shape=\beta`, `scale=\alpha` |
| lnorm: `meanlog=\mu`, `sdlog=\sigma` | logis: `location=\mu`, `scale=s` |
| logitnorm: `mu=\mu`, `sigma=\sigma` | lomax: `lambda=\lambda`, `kappa=\kappa` |
| lst: `mu=\mu`, `sigma=\sigma`, `df=\nu` | naka: `shape=m`, `scale=\Omega` |
| nbinom: `size=r`, `prob=p` | nhyper: `m=K`, `N=N`, `r=r` |
| norm: `mean=\mu`, `sd=\sigma` | nsbeta: `shape1=\alpha`, `shape2=\beta`, `min=a`, `max=b` |
| paralogis: `shape=\alpha`, `scale=\theta` | pareto: `a=\alpha`, `b=b` |
| pareto2: `shape=\alpha`, `scale=\sigma`, `min=\mu` | pareto3: `shape=\gamma`, `scale=\sigma`, `min=\mu` |
| pareto4: `shape1=\alpha`, `shape2=\gamma`, `scale=\sigma`, `min=\mu` | pig: `mean=\mu`, `shape=\lambda` |
| pois: `lambda=\lambda` | power: `alpha=a`, `beta=b` |
| rayleigh: `sigma=\sigma` | sged: `mean=\mu`, `sd=\sigma`, `nu=\nu`, `xi=\xi` |
| sgomp: `b=b`, `eta=\eta` | skellam: `mu1=\mu_1`, `mu2=\mu_2` |
| slash: `mu=\mu`, `sigma=\sigma` | sn: `xi=\xi`, `omega=\omega`, `alpha=\alpha` |
| snorm: `mean=\mu`, `sd=\sigma`, `xi=\xi` | sstd: `mean=\mu`, `sd=\sigma`, `nu=\nu`, `xi=\xi` |
| std: `mean=\mu`, `sd=\sigma`, `nu=\nu` | tbinom: `prob=p`, `a=a`, `b=b`, `size=n` |
| tlambda: `mu=\mu`, `sigma=\sigma`, `lambda=\lambda` | tnorm: `mean=\mu`, `sd=\sigma`, `a=a`, `b=b` |
| tpois: `lambda=\lambda`, `a=a`, `b=b` | trgamma: `shape1=k`, `shape2=p`, `scale=a` |
| triang: `a=a`, `b=b`, `c=c` | unif: `min=a`, `max=b` |
| weibull: `shape=k`, `scale=\lambda` | yulesimon: `rho=\rho` |
| zeta: `s=s` | zib: `size=n`, `prob=p`, `pi=\pi` |
| zinb: `size=r`, `prob=p`, `pi=\pi` | zip: `lambda=\lambda`, `pi=\pi` |
| zipf: `N=N`, `s=s` | zmbinom: `size=n`, `prob=p`, `p0=p_0` |
| zmgeom: `prob=p`, `p0=p_0` | zmlogarithmic: `prob=p`, `p0=p_0` |
| zmnbinom: `size=r`, `prob=p`, `p0=p_0` | zmpois: `lambda=\lambda`, `p0=p_0` |
| ztbinom: `size=n`, `prob=p` | ztgeom: `prob=p` |
| ztnbinom: `size=r`, `prob=p` | ztpois: `lambda=\lambda` |

## Notices and validation

Notices now use complete conditional sentences, trim terminal punctuation, and substitute coefficient symbols while preserving authored math and code. Held scopes and restriction exemptions remain explicit. They say the fit stops without treating an uncertified numerical boundary as a proof of non-existence.
Exp: all-zero observations with free rate. Gamma: constant observations with both coefficients free. GB2: constant data only when the holds still permit concentration. Binomial now says size is free, covering explicit NULL; normal explicitly requires free sd. Other records preserve their declared paths and scopes.
Exp core gap retained: the all-zero free-rate overload returns nonfinite results with default Ok; the R adapter detects and stops. The new record names the existing Unbounded enum, but this metadata change does not repair core status propagation.
No existence or global-optimality theorem is newly claimed. Site checks establish rendering and snippet compilation; R regression and quality checks establish their documented numerical comparisons, not estimator asymptotics.

- Final `/tmp/umvl-cpu/run4 just site-check`: PASS, 98 MathML pages, resolved links, no external math runtime; all 98 C++17 snippets compile/link. Log: `build/scratch/site-check-final.log`.
- Final `/tmp/umvl-cpu/run4 bash build/scratch/final-checks.sh` includes `NOT_CRAN=true just test`: `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 39040 ]`. Skip: optional `{sn}` unavailable. First full run had the same result. Log: `build/scratch/r-test.log`.
- Same locked script runs `just quality exp norm gamma beta binom hyper gb2 sstd gev logitnorm`: 10/10 pass. Hyper score/Hessian and oracle are skipped (no regular parameters); integer dimensions are excluded from continuous differentiation/optimization. Limits are listed, not exercised by this harness.
- Workloads: exp n=500, norm n=500, gamma n=500, beta n=500, binom n=500, hyper n=500, gb2 n=500, sstd n=500, gev n=500, logitnorm n=500. Existing stored goldens; oracle seed 20261028, eight starts; logLik tolerance `1e-9*(1+abs(logLik))`, standardized score at most `1e-3`, oracle gap at most `1e-6*(1+abs(logLik))`.
- R loader: all 98 accepted; key-reference selections checked. Structural audit: all prose, coefficient contracts, densities, holds, defaults and examples unchanged. `git diff --check` passed.
- All 98 rendered formula annotations checked; detailed HTML read for exp, norm, gamma, beta, binom, hyper, gb2, sstd, gev, logitnorm, burr, genbeta, trgamma, dweibull, zipf, naka. Evidence: `build/scratch/html-inspection.txt`; checks: `build/scratch/inspect_html.py` and `audit_fields.py`.
- Environment: Linux, Python 3.14.5, SymPy 1.14.0, g++ 15.2.0, installed BH headers, Node 22.22.1, R 4.6.1. Commands used shared slot locks, nice priority and single-thread BLAS/OpenMP; temporary files stayed under `build/`. Copied the existing node_modules as requested; no installs.
- Initial Astro callback-await error was fixed before the green runs. The extra HTML audit initially mistook TeX commands for raw API names; its assertion was corrected and the audit passed. Existing roxygen link diagnostics remain (TASK-127); runtime test warnings are zero. No separate C++ numerical suite was requested or run.

Artifact-pane display was attempted with `bin/show`; the viewer returned HTTP 403 because this worktree is outside its configured readable roots. No viewer configuration or other checkout was changed.
