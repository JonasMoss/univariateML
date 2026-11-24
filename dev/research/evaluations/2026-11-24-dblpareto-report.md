# Double Pareto fitting and limit replay

TASK-235 · `codex/dblpareto` · starting commit `0480604f`

`mldblpareto` fits the double Pareto in C++, R, and JavaScript. GB2 now points to it when the exploratory search selects the double Pareto face. Feller–Pareto points to the same fitter on observations minus the held threshold. All four fresh snapshot modes reproduce the stored outcomes. The 11 selected escape records in each family mode carry public density coefficients, and both strict checks found zero new issues.

The id `dblpareto` was free in the starting canonical names, JavaScript registries, and site sources. The existing Pareto density uses `dpareto`. The new `scale` parameter matches the GB2 and Feller–Pareto convention and locates the kink in observation units. The two log-scale rates preserve the direction labels in the Feller–Pareto payload. The left rate becomes the power shape and the right rate becomes the Pareto shape. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/README.md) · [Canonical names](../../../bindings/r/CANONICAL_NAMES.md)

## Likelihood and existence

Write $z_i=\log x_i$, $c=\log b$, $L=\sum_i(c-z_i)_+$ and $R=\sum_i(z_i-c)_+$. Including the Jacobian, the log likelihood is

$$
\ell=n\{\log\alpha+\log\beta-\log(\alpha+\beta)\}-\alpha L-\beta R-\sum_i z_i.
$$

At fixed $c$, the rate Hessian divided by $n$ is the negative diagonal matrix with entries $1/\alpha^2,1/\beta^2$ plus the all-ones matrix divided by $(\alpha+\beta)^2$. Cauchy–Schwarz bounds the positive quadratic part by $(\alpha^2+\beta^2)/(\alpha+\beta)^2$ times the magnitude of the negative part. The factor is strictly below one, so a stationary rate pair is the unique conditional maximum.

For positive $L,R$, the scores give $\hat\alpha=n/[\sqrt L(\sqrt L+\sqrt R)]$ and $\hat\beta=n/[\sqrt R(\sqrt L+\sqrt R)]$. The profile equals a constant minus $2n\log(\sqrt L+\sqrt R)$. Both distances are affine with nonzero slopes between distinct observations. Their square roots have a strictly concave sum there. Since $-\log$ is decreasing and convex, the profile is strictly convex between consecutive knots. The maximum over that interval is at an endpoint. Beyond either sample extreme the positive distance grows with distance from the sample and the profile worsens.

With one rate free and the other held at $h$, let $t$ be the mean log distance on the free side. The free-rate score gives $t=h/[r(r+h)]$, or

$$
\frac{1}{\hat r}=\frac{t}{2}+\sqrt{\frac{t^2}{4}+\frac{t}{h}}.
$$

Within a sample gap the profiled likelihood is a supremum of affine functions of $c$. The free rate differs at any two distinct interior kinks because $t$ changes strictly there. The maximizing rate at an interior convex combination therefore cannot maximize both endpoint functions, which makes the profile strictly convex. Outside the data range, moving toward the nearest extreme strictly improves the profile. The side with positive distance has its positive rate either held or optimized at each kink. The other distance is zero throughout that exterior range.

With both rates held, the negative log likelihood in $c$ is weighted absolute deviation. Between observations its slope is $\alpha k-\beta(n-k)$ for $k$ observations at or below the left endpoint. The first nonnegative slope identifies a weighted quantile. A zero slope across a gap gives the entire closed interval between its observed endpoints. Ties in the observations can collapse that interval to a single value. With all three coefficients held, the fitter evaluates the density directly.

| Held subset and sample | Outcome in exact arithmetic |
|---|---|
| Free kink, both rates free, nonconstant sample | Compare the finite interior knots with the Pareto and power faces at the extremes. A finite winner is a maximizer even if it ties a face. Two distinct observations give two equal endpoint suprema with no finite maximizer. |
| Free kink, one rate held, nonconstant sample | Compare all knots. The extreme at which the free rate has zero distance supplies the corresponding one-sided limit, with the other rate held. The opposite extreme is a finite candidate. |
| Held kink, at least one rate free | Every free rate with positive distance has the conditional optimum above. If just one free rate has zero distance, it diverges and selects the Pareto or power face, with the other rate fitted or held. |
| Constant sample, including one observation | With both rates free and the kink admissible at the common value, letting both rates grow makes the likelihood unbounded. At a different held kink the held-kink rule applies. With a free kink and one held rate, the best one-sided face has its endpoint at the common value. With both rates held, the free kink equals the common value. |

The implementation sorts physical observations and accumulates positive adjacent log gaps with their counts. This avoids cancellation between large absolute logarithms and preserves observed kinks exactly. The scan takes $O(n\log n)$ time, including sorting. A practical separation margin of $128\epsilon(1+|s_1|+|s_2|)$ in the compared profile coordinates governs certification. Within the margin the fitter retains an accurate finite conditional solution when available and marks its uniqueness undetermined. The fixed-rate interval uses an exact product comparison for the weighted counts. For both rates free, exact products of reflected observations can prove symmetry in log coordinates. When the reflected pair is separated from every other knot and face, both maximizers are returned as enclosed points with exact kinks and relative rate radius $10^{-11}$. [Core fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/mle.hpp) · [C++ cases](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/test.cpp)

## Probability routines and closure

The CDF uses $\beta(x/b)^\alpha/(\alpha+\beta)$ on the left. The survival function uses $\alpha(b/x)^\beta/(\alpha+\beta)$ on the right. The implementation forms the opposite tail in log coordinates and inverts the two explicit branches. The mpmath truth table covers 18 parameter points, 334 density and tail rows, and 792 quantile rows. All quantile rows also passed through the R wrappers, with a largest forward relative difference of $7.14\times10^{-14}$. The frozen R sample has 500 distinct observations. At 50-digit precision its best knot beats the runner-up by $0.0007661645483$ log-likelihood units. The original fit and reference agree within $7.0\times10^{-17}$ in relative coefficient error. [Probability core](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/dpqr.hpp) · [Truth table](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/truth.json) · [Reference evidence](2026-11-24-dblpareto-data/golden-reference.json)

For GB2 let $p=\alpha/a$, $q=\beta/a$, and hold $b$ as $a$ grows. The normalizer satisfies $a/B(p,q)\to\alpha\beta/(\alpha+\beta)$. Also $a^{-1}\log(1+e^{az})\to z_+$ for $z=\log(x/b)$. Substitution gives the two double Pareto powers, including the common density at the kink. Feller–Pareto uses the same identity on excesses, with unit translation Jacobian. Sending the left rate to infinity in the double Pareto gives the Pareto face, and sending the right rate to infinity gives the power face. The limiting masses are one in each case, so pointwise almost-everywhere convergence gives convergence in $L^1$ by Scheffé's lemma. The four edges are declared in the dossiers and rendered on the site. [Independent closure check](2026-11-24-dblpareto-data/check-closure.py) · [Numerical convergence](2026-11-24-dblpareto-data/closure.tsv)

## Escape transport and snapshots

The parent fitters retain their exploratory comparisons and certification flags. The selected payload now has an explicit core model identity. The public density evaluates its point likelihood at the transported coefficients. A nonzero Feller–Pareto threshold travels as `min` in the standard shifted-model payload. A zero threshold uses the base model. The strict checker uses `ddblpareto` for both and subtracts `min` before evaluation when present. This replaces the checker-only `double_pareto` C++ density in `fit-density.py`. Archived names still decode through the public density. The certificate replay rules contain no separate double Pareto reader.

| Example record | Before | After | Independent log likelihood |
|---|---|---|---:|
| GB2 archive `009516` | `unavailable:double-pareto`, `scale,left,right` | `dblpareto`, `scale,left_rate,right_rate` | −7.135794271975 |
| Feller–Pareto archive `001588` | `unavailable:shifted-double-pareto`, `kink,left_rate,right_rate,min` | `dblpareto`, `scale,left_rate,right_rate` | −28.854116794763 |
| Feller–Pareto `threshold-double-pareto`, `min=2` | `unavailable:shifted-double-pareto`, `kink,left_rate,right_rate,min` | `unavailable:shifted-dblpareto`, `scale,left_rate,right_rate,min` | −199.305220095791 |

The 100-digit replay sums the two power-law pieces on the original observation scale after translation. Across the selected records its largest relative difference is $3.24\times10^{-16}$. Re-evaluation through the public density changes the serialized likelihood by at most $9.10\times10^{-13}$ in absolute value. The other changed fields are diagnostic text and transport of the named coefficient witnesses. The physical scale and rate values, core status, outcome kind, and certification flags agree with the stored baselines. [Native replay](2026-11-24-dblpareto-data/native-replay.jsonl) · [Double-only replay](2026-11-24-dblpareto-data/double-replay.jsonl) · [Replay script](2026-11-24-dblpareto-data/check-replay.py)

| Workstation snapshot mode | Fresh records | Changed existing records | Added | Removed |
|---|---:|---:|---:|---:|
| Native | 12,081 | 11 | 28 | 0 |
| Double-only | 12,081 | 11 | 28 | 0 |
| Modifier routes, native | 2,006 | 0 | 0 | 0 |
| Modifier routes, double-only | 2,006 | 0 | 0 | 0 |

Each family mode has six Feller–Pareto and five GB2 payload changes. All additions belong to Double Pareto. Both strict checks ran on the combined family and modifier-route snapshots in each precision. [Native diff](2026-11-24-dblpareto-data/native-diff.txt) · [Double-only diff](2026-11-24-dblpareto-data/double-diff.txt) · [Native route diff](2026-11-24-dblpareto-data/routes-native-diff.txt) · [Double-only route diff](2026-11-24-dblpareto-data/routes-double-diff.txt) · [Density check](2026-11-24-dblpareto-data/fit-density.txt) · [Consistency check](2026-11-24-dblpareto-data/fit-consistency.txt)

## Tests

New tests cover all eight hold subsets, concentration on adjacent large doubles, one-sided likelihoods, constant data and singletons, weighted-quantile intervals and their exact slope signs, reflected maximizing knots, and an unresolved finite–face tie. The R and JavaScript tests replay the named parent escapes through the public density. The R golden tolerance and catalogue count include the new family.

| Check | Machine | Result |
|---|---|---|
| `just golden-reference --write dblpareto`, `just dpqr-truth dblpareto`, `just quality dblpareto` | Workstation | Passed |
| `just test-full` truth regeneration | Workstation | 36/36. Simbox returned 75. |
| `UMVL_TEST_TIER=full just test-changed --no-long-double` | Workstation | Native C++ 181/181, double-only C++ 181/181, R 67,514 pass, 0 warnings, 1 existing Zipf reference-data skip |
| `just js-build` and `just js-test` | Workstation | Build passed and 599/599 tests passed |
| `just site-check` | Workstation | Passed, including 86 pages and compiled C++17 examples |
| `just relations` and Markdown link check | Workstation | 158 relations passed, zero new broken links |
| `fit-density.py` and `fit-consistency.py` | Workstation | Zero new findings |

The first full R run found one stale catalogue-count assertion. The final changed-family runner exercised the entire full R tier after the assertion was updated. The first site check with a built binding found stale inference availability. Regenerating the canonical registry resolved the site check. [Suite output summary](2026-11-24-dblpareto-data/suite-results.txt)

## Open

A general equality test for overlapping non-reflection knot profiles remains open. The exact likelihood criterion covers these ties, and an implementation would need tighter profile comparisons or an equality proof before certifying the full set. The current fixture with a tied finite knot and Pareto face retains its finite solution with `certified = FALSE` and `unique = NA`.

Numerical covariance at a fitted sample kink requires validation of a procedure that accounts for the change in the one-sided location score. It is recorded as unavailable in the dossier.
