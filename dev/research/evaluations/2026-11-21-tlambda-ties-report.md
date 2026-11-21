# Tukey lambda tie collapse and distinct pairs

TASK-214.17 · Branch `codex/tlambda-ties` · Base `460a0ff2`

| Strict entry | Mathematical outcome | Native | Double-only |
|---|---|---|---|
| `phenomenon:tie-collapse`, class `ties` | Certified nonexistence due to unbounded scale collapse | Pass, `N:unbounded` | Pass, `N:unbounded` |
| `phenomenon:two-point`, class `two` | Unique certified MLE at the uniform member on the observed interval | Pass, `E` | Pass, `E` |

The direct consistency run reported four listed entries passing and zero new findings. [Strict output](2026-11-21-tlambda-ties-data/strict-consistency.txt).

## Strict majority ties

Write $f(x)=g((x-\mu)/\sigma)/\sigma$. At the admissible shape $\lambda=-1$, the standardized quantile is $Q(u)=(1-u)^{-1}-u^{-1}$. As $u\downarrow0$, $Q(u)\sim-1/u$ and $Q'(u)\sim u^{-2}$. Thus $g(z)\sim |z|^{-2}$ on both tails, and $g(0)=1/8$. Set $\mu=c$, the tied value of multiplicity $k$. For each $x_i\ne c$, $f(x_i)\sim\sigma/|x_i-c|^2$. It follows that
$$
L(\sigma)\sim 8^{-k}\left(\prod_{x_i\ne c}|x_i-c|^{-2}\right)\sigma^{n-2k}.
$$
On the recorded sample $n=6, k=4$, the exponent is $-2$ and the constant is about $0.00024576923548$. The divergence proves unboundedness. An independent 85-digit calculation solves the quadratic inverse along this path and checks convergence of $\sigma^2 L$ to the stated constant. [Calculation](2026-11-21-tlambda-ties-data/check.py) and [output](2026-11-21-tlambda-ties-data/high-precision.txt).

At the base commit, `profile.hpp:139` returned `InvalidData` as soon as a running count reached three. The fourth observation in the run was therefore never counted. The post-profile check at base `certificate.hpp:685` counted all four, but its recovery branch handled only equality with half the sample. The public entry point mapped the residual `InvalidData` to `TransformFailure` and then `F:evaluation`. The stop arose before standardization or quantile inversion. It masked an unbounded likelihood.

The free-shape guard now records equality and scans on. Finding $k>n-k$ returns `Unbounded` immediately. The common binding reports that the likelihood is unbounded through scale collapse. [Tie guard](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/profile.hpp#L139) and [binding diagnostic](../../../bindings/common/distributions/tlambda/fit.hpp#L39).

## Distinct pairs

For $x_1<x_2$, let $d=x_2-x_1$, and let $u<v$ be their quantile levels. Write $D(t)=Q'_\lambda(t)=t^{-b}+(1-t)^{-b}$, with $b=1-\lambda\in[0,2]$. Eliminating scale gives
$$
d^2 L=\frac{(Q_\lambda(v)-Q_\lambda(u))^2}{D(u)D(v)}.
$$
For $0<t<1$ set $S=t^{-b-2}+(1-t)^{-b-2}$. Cauchy–Schwarz gives $(D')^2\le b^2 D S$, while $D''=b(b+1)S$. Since $b(b+1)\ge 3b^2/2$, we have $D D''\ge 3(D')^2/2$, which makes $D^{-1/2}$ concave. If $a=D(u)^{-1/2}$ and $c=D(v)^{-1/2}$, then $D(u+(v-u)s)\le ((1-s)a+sc)^{-2}$ for $0\le s\le1$. The integral over $s$ of this upper bound is $1/(ac)$. Therefore $d^2L\le(v-u)^2$. Below $\lambda=1$, finite observations with positive density have interior quantile levels, so the bound is strictly below one. At $\lambda=1$, $D=2$ and equality with one requires $u=0,v=1$. This uniquely determines $\mu=(x_1+x_2)/2$, $\sigma=d/2$, and $\log L=-2\log d$.

The base profile guard reached equality on the second observation even though the pair had no ties. The fallback at base `certificate.hpp:685` treated the pair as a general half-multiplicity case, collected the $\lambda=-1$ collapse witness and uniform incumbent, and returned `CertificateFailure` with `F:evaluation`. The fallback made no global comparison. The proof above establishes that its uniform incumbent was the unique maximizer.

The all-free distinct-pair route now uses the uniform fit directly. It checks support containment and represented scale accuracy, reports the public likelihood, and marks the upper shape restriction. Uniform midpoint computation uses half endpoints when the full range overflows. The dossier now gives the analytic argument in place of the earlier grid search evidence. On the canonical binary64 inputs, 85-digit arithmetic gives $\mu=0.04740228436532295530625447\ldots$, $\sigma=0.99875396402936483797319056\ldots$, and $\log L=-1.38380073528203869935053572\ldots$. [Pair certificate](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/certificate.hpp#L664), [uniform restoration](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/mle.hpp#L20), [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md#L140), and [reference output](2026-11-21-tlambda-ties-data/high-precision.txt).

## Canonical snapshots

Each mode compared 205 input-matched records against the stored November 15 baseline. Four changed in each mode. The 168 estimate-to-estimate records had a maximum relative coefficient move of zero, and the golden record was identical. No other fitter includes the changed headers. [Native diff](2026-11-21-tlambda-ties-data/diff-native.txt), [double-only diff](2026-11-21-tlambda-ties-data/diff-double.txt), [native provenance](2026-11-21-tlambda-ties-data/native.provenance.json), and [double-only provenance](2026-11-21-tlambda-ties-data/double.provenance.json).

| Record | Old | New | Reason |
|---|---|---|---|
| Class `ties` | `F:evaluation` | `N:unbounded` | Four of six observations coincide. |
| Archive `007526`, all free | `F:evaluation` | `N:unbounded` | Four of six observations coincide. |
| Class `two` | `F:evaluation` | Certified `E` | Two distinct observations. |
| Archive `007523`, all free | `F:evaluation` | Certified `E` | Two distinct observations. |

## Tests and reproduction

The two appended C++ doctests exercise the exact class inputs with an iteration limit of one, check outcome kind and reason, and verify zero iterations. The pair uses `mltest::kFitReferenceTol` with `.scale(0)` for the three coefficients and log likelihood, and checks the summed public density. The R test now expects the uniform coefficients and likelihood on `1:2` and checks the scale-collapse diagnostic. [C++ regressions](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/test.cpp#L371) and [R regression](../../../bindings/r/distributions/tlambda/test.R#L41).

All checks ran on the workstation with GCC 15.2 for the native builds and snapshot probes. The targeted recipe used `run4`, `TESTTHAT_PARALLEL=FALSE`, and `MAKEFLAGS=-j4`. [Targeted log summary](2026-11-21-tlambda-ties-data/targeted-results.txt) and [quality and site output](2026-11-21-tlambda-ties-data/quality-and-site.txt).

| Check | Result |
|---|---|
| `just test-changed --no-long-double`, native C++ | 1/1 shard passed |
| Same recipe, double-only C++ | 1/1 shard passed |
| Same recipe, targeted R dev tier | 394 passes, 8 skips, zero failures or warnings |
| Same recipe, `just js-build` and `just js-test` | Build passed, 230/230 tests passed |
| `just quality tlambda` | Passed |
| `just site-check` | Passed |
| `fit-consistency.py` | All four listed entries pass, zero new findings |
| `fit-density.py` | Zero findings on the two snapshots |

To reproduce, run `dev/validation/fit-snapshot.py --family tlambda --jobs 1 --output build/snapshots/tlambda-native` through `/tmp/umvl-cpu/run`. Repeat with `--no-long-double` and output `build/snapshots/tlambda-double`. Pass the two directories in that order to `fit-consistency.py` and `fit-density.py`. Compare each snapshot with the corresponding stored `2026-11-15-fit-snapshot-data/{native,double}.jsonl.gz` using [compare.py](2026-11-21-tlambda-ties-data/compare.py). The supplied `build/scratch/diff_baseline.py` independently agreed after filtering the baseline to Tukey lambda.

## Open

Both reported phenomena are resolved. For samples of more than two observations whose greatest tie multiplicity is exactly half, the retained diagnostic still supplies the collapse face and a uniform incumbent. Settling those cases in general requires a bound on the full free-shape likelihood or a finite maximizer with a verified comparison against the collapse face.
