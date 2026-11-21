# Near-constant and two-point strict findings

TASK-214.33 · Worktree `codex/strict-33` · Base `8b3b2dba`

## Decisions and strict verdicts

| Family and class | Decision and evidence | Fitter change | Native | Double-only |
|---|---|---|---|---|
| Gompertz, near-constant | Strict concavity and a positive rate score at the cutoff place the initial hazard below binary64. On the snapshot the score bound exceeds 0.9999999999995 and the upper bound on log hazard is −2039.73. | Check the relative spread before standardization or Newton iteration in `gompertz/mle.hpp:269`. | Pass, `F:evaluation` | Pass, `F:evaluation` |
| Kumaraswamy, near-constant | Strict profile concavity places the second shape above binary64. On the snapshot the score bound exceeds 0.9999999999998 and the lower bound on log shape is 1024. | Check the logarithmic spread before the candidate search in `kumar/certificate.hpp:22`. | Pass, `F:evaluation` | Pass, `F:evaluation` |
| Shifted Gompertz, near-constant | A witness excludes every representable shape at every rate. On the snapshot its log likelihood is 104.7631, against a uniform upper bound of 18.0494. This proves the range diagnosis independently of uniqueness. | Compare the witness bound and the representable-shape bound in `sgomp/mle.hpp:139`. | Pass, `F:evaluation` | Pass, `F:evaluation` |
| Generalized gamma, two | The optimized likelihood decreases strictly in gamma shape. The power and Pareto faces tie at a finite unattained supremum. The snapshot exponent is 1.4733099871 and the log likelihood is −1.1181988403. | Replace `min_n` with `two_point`. Return certified unattainment and both named candidates in `gengamma/mle.hpp:50`. | Pass, `N:unattained` | Pass, `N:unattained` |
| Skew Student-t, two | The optimized endpoint half-t product increases strictly with degrees of freedom. The two reflected half-normal faces attain the limiting likelihood. On the snapshot their scale is 0.5991923612 and their log likelihood is −0.4272375149. | Replace `min_n` with `two_point`. Report a certified limit with the right half-normal as the primary payload and both reflected candidates in `sstd/mle.hpp:688`. | Pass, `L` | Pass, `L` |

The three range checks use spread comparisons with explicit slack and avoid computing the out-of-range coefficients. Generalized gamma uses an unattained outcome because both different replacement families are supported equally. Skew Student-t reports a primary half-normal and its reflection, with the same scale and likelihood. The 90-digit script checks the exact binary64 snapshot inputs independently of the C++ fits. The analytic arguments below establish the outcome, and the numerical checks give its scale on the recorded data. [High-precision script](2026-11-21-strict-33-data/check.py), [output](2026-11-21-strict-33-data/high-precision.txt), [strict verdicts](2026-11-21-strict-33-data/strict-consistency.txt).

## Canonical snapshots

Each mode compared 868 records with the stored November 15 baseline. The comparisons agree across modes. Genbeta is the only other fitter that includes a changed family header. Its held-shape branch delegates to Kumaraswamy via `genbeta/mle.hpp:9` and `:208`.

| Family | Compared per mode | Changed per mode | Estimate pairs | Max relative coefficient move |
|---|---:|---:|---:|---:|
| Genbeta | 220 | 0 | 127 | 0 |
| Generalized gamma | 28 | 1 | 2 | 0 |
| Gompertz | 112 | 7 | 86 | 0 |
| Kumaraswamy | 107 | 1 | 86 | 0 |
| Shifted Gompertz | 116 | 9 | 93 | 0 |
| Skew Student-t | 285 | 2 | 100 | 0 |

- Generalized gamma’s canonical pair is the sole change in outcome kind, from numerical failure to certified unattainment with tied power and Pareto faces.
- Gompertz and shifted Gompertz each change the near-constant case and the offset case from nonconvergence to evaluation. The offset cases meet the same relative-spread bounds. The bounds also explain five Gompertz and seven shifted Gompertz archive stops with the same reason change.
- Kumaraswamy changes only the canonical near-constant reason from nonconvergence to evaluation.
- Skew Student-t’s canonical pair and archive pair `006675` both move from a margin-decided half-normal limit to a proved half-normal limit with the reflected candidate attached.

The comparisons check matching case keys and input hashes. [Native summary](2026-11-21-strict-33-data/summary-native.jsonl), [double-only summary](2026-11-21-strict-33-data/summary-double.jsonl), [native diff](2026-11-21-strict-33-data/diff-native.txt), [double-only diff](2026-11-21-strict-33-data/diff-double.txt), [native provenance](2026-11-21-strict-33-data/native.provenance.json), [double-only provenance](2026-11-21-strict-33-data/double.provenance.json).

## Regression tests and suites

The appended doctests exercise these outcomes at an iteration limit of one and verify that they require zero iterations.

| File | Added coverage |
|---|---|
| `gompertz/test.cpp:251` | Exact adjacent-input sample and evaluation reason. |
| `kumar/test.cpp:228` | Exact adjacent-input sample and evaluation reason. |
| `sgomp/test.cpp:140` | Exact adjacent-input sample and evaluation reason. |
| `gengamma/test.cpp:673` | Ordinary and adjacent distinct pairs, certified unattainment, both models, their exponents and the supremum. |
| `sstd/test.cpp:783` | Two distinct pairs including reversed order and translation, certified limit, both endpoints, scale and supremum. |

All runs used the workstation. The post-outage targeted recipe used `TESTTHAT_PARALLEL=FALSE`, `MAKEFLAGS=-j4` and `run4`. The first native run caught a Genbeta regression because the broad Kumaraswamy guard intercepted the test’s budget and witness paths. Restricting the precheck to the binary64 log-resolution regime preserved those paths and resolved the regression. Both snapshot modes and strict consistency were rerun after that fix. The previous quality and site runs covered the same goldens and dossier changes.

| Check | Result |
|---|---|
| `just test-changed --no-long-double`, native C++ | 15/15 |
| `just test-changed --no-long-double`, double-only C++ | 15/15 |
| Targeted R development tier | 4,997 passed, 33 skipped, zero failures or warnings |
| Targeted `just js-build` and `just js-test` | Build passed, 289/289 tests |
| `just quality gompertz kumar sgomp gengamma sstd` | Five passed |
| `just site-check` | Passed |
| `fit-consistency.py`, both final snapshots | Ten listed entries passing, zero new findings |

[Suite results](2026-11-21-strict-33-data/test-results.txt).

## Open

The five listed phenomena are resolved. The Kumaraswamy precheck covers logarithmic spreads below 32 binary64 epsilons relative to the smaller absolute log. Applying the range certificate to wider spreads would require a policy for preserving or replacing the budget and finite-witness diagnostics on Genbeta’s delegated path. The pair `{0.5, 0.500000000001}` in Genbeta’s existing test is a concrete case.

## Likelihood arguments

### Gompertz

Let $m=\min x$, $d=\max x-m$, and $I(b)=\sum_i(\exp(bx_i)-1)/b$. The profiled hazard is $a(b)=n/I(b)$ and the profile is strictly concave because $I(b)$ is the Laplace transform of the sum of the uniform length measures on $[0,x_i]$. The derivative of the profile, multiplied by $b/n$, equals
$$1+b\bar x-b\frac{\sum_i x_i e^{bx_i}}{\sum_i(e^{bx_i}-1)}
\ \ge\ 1-bd-\frac{b\max x}{\operatorname{expm1}(bm)}.$$
At $b_0=2048/m$, the guard ensures $b_0d<1/2$ and the final term is smaller than $2049/\operatorname{expm1}(2048)$. A positive score places the maximum beyond $b_0$. For a distinct positive sample the profile tends to $-\infty$ as $b\to\infty$. The increasing $I$ implies $a(\hat b)<a(b_0)\le b_0/\operatorname{expm1}(2048)$. Even if $m$ is the smallest positive double, this bound is below half that value.

### Kumaraswamy

Write $t_i=-\log x_i$, $t_*=\min t_i$ and $G(a)=\sum_i-\log(1-e^{-at_i})$. The exponential series for $G$ gives $-G'/G\ge t_*$, and at $a_0=1024/t_*$ we have $-a_0G'/n\le 1024/\operatorname{expm1}(1024)$ since $z/\operatorname{expm1}(z)$ decreases. The profile score multiplied by $a_0/n$ is therefore at least
$$1-a_0(\max t_i-t_*)-\frac{1024}{\operatorname{expm1}(1024)}.$$
The guard checks that the logarithmic spread is below $32\epsilon t_*$, where $\epsilon=2^{-52}$. Thus $a_0(\max t_i-t_*)<1024\cdot32\epsilon<1/2$. The positive score and the dossier's strict concavity and existence proof put the maximizer above $a_0$. The increasing profiled shape $n/G(a)$ is already at least $\operatorname{expm1}(1024)$ there by $-\log(1-y)\le y/(1-y)$.

### Shifted Gompertz

For $y=e^{-bx}$, $t=\eta y$, the density divided by $b$ is $(y+t(1-y))e^{-t}\le(1+t)e^{-t}\le1$. When $0\le\eta\le M$ for binary64 maximum $M$, it is also bounded by $(1+M)e^{-bm}$ for $m=\min x$. With $C>\log(1+M)+1$, every density is at most $C/m$. For $bm\le C$ use the first bound. For $bm>C$ use the second and the decrease of $z e^{-z}$ above one.

For $d=\max x-m>0$, take $b=1/d$, $\eta=e^{bm}$. At $z=b(x-m)\in[0,1]$, the witness density divided by $b$ equals
$$e^{-z-e^{-z}}(1+e^{-bm}-e^{-bx})\ge e^{-2}.$$
The guard makes this lower bound at least one log unit per observation higher than the upper bound for all representable $\eta$. The dossier establishes attainment for distinct positive data. The comparison therefore rules out representability at every global maximum without a uniqueness claim.

### Generalized gamma on a distinct pair

For log observations with gap $d>0$ and fixed signed shape $Q\ne0$, set $k=1/Q^2$ and let $s>0$ be half the absolute difference on the log-gamma scale. The two gamma intensities at the location optimum sum to $2k$ and have ratio $e^{2s}$. Both signs of $Q$ thus give the same profiled log likelihood on log data,
$$2\{\log(2s/d)-\log\Gamma(k)+k[\log k-1-\log\cosh s]\}.$$
The unique scale optimum solves $A=s\tanh s=1/k$. Its derivative in $k$, by the envelope identity, is $2[\log k-\psi(k)-\log\cosh s]$.

For $k\le1/2$, $\psi(k+1)>\log(k+1/2)$ and the recurrence give $\log k-\psi(k)<1/k-\log 2\le\log\cosh s$. The digamma inequality follows by integrating
$e^{-(k+1/2)t}[1/t-1/(2\sinh(t/2))]>0$ over $t>0$.

For $k\ge1/2$, $\log k-\psi(k)<A/2+A^2/12$. In the integral for $\log k-\psi(k)$, the kernel is $1/(1-e^{-t})-1/t<1/2+t/12$. This follows from $z\cosh z<(1+z^2/3)\sinh z$ for $z=t/2>0$, by the positive remaining Taylor coefficients. To compare with $\log\cosh s$, differentiate $F(s)=\log\cosh s-A/2-A^2/12$. Its derivative has the sign of $(3-A)\sinh(2s)/(2s)-(3+A)$. Here $A\le2$ implies $s<\sqrt5$. Put $u=s^2$, $P=u(15+u)/(15+6u)$ and $B=1+2u/3+2u^2/15+4u^3/315$. The rational bound $\tanh s\le s(15+s^2)/(15+6s^2)$ follows by comparison for $y'=1-y^2$ at zero. The derivative of the rational function minus $1-y^2$ is $s^6/[9(2s^2+5)^2]$. Thus $A\le P<3$ for $u\le5$. The exponential series gives $\sinh(2s)/(2s)\ge B$, and
$$ (15+6u)[(3-P)B-(3+P)] = \frac{2u^2(945+48u-15u^2-2u^3)}{315}>0 $$
for $0<u\le5$. Hence $F(s)>F(0)=0$ and the derivative in $k$ is negative everywhere.

As $k\downarrow0$, $ks\to1$, $k\Gamma(k)\to1$, and the optimized product on log data approaches $4/(e^2d^2)$, the endpoint exponential product with rate $2/d$. At $Q=0$, the lognormal branch gives the continuous $k\to\infty$ limit, with smaller product $2/(\pi e d^2)$. The two signs give the power and Pareto faces after undoing the log transformation. Their identical Jacobian is $1/(x_1x_2)$.

### Skew Student-t on a distinct pair

At fixed $\nu$, let $l,r>0$ be the two side scales about the mode and let $c_\nu$ denote the ordinary unit-scale Student density at zero. Write $K(z)=(1+z^2/\nu)^{-(\nu+1)/2}$. If the observations straddle the mode, the gap $d$ is $lu+rv$ for standardized nonnegative distances $u,v$. Each density is $2c_\nu K(z)/(l+r)$. Therefore $d^2$ times their product is at most $4c_\nu^2 w^2K(w)$ with $w=\max(u,v)$. If both observations are on the same side, the gap divided by $l+r$ is also at most the larger standardized distance. The bound holds by the same argument. Equality is approached from each side by placing the mode at one endpoint and shrinking the empty side. The maximizing $w^2$ is $2\nu/(\nu-1)$.

The derivative of the log of this optimized half-t product is
$$\psi((\nu+1)/2)-\psi(\nu/2)-\operatorname{atanh}(1/\nu).$$
The digamma difference is $\int_0^\infty e^{-\nu t}[1+\tanh(t/2)]\,dt$. Since $\tanh v\ge v-v^3/3$ for $v\ge0$ (differentiate and use $\tanh v\le v$), the integral is at least $1/\nu+1/(2\nu^2)-1/(4\nu^4)$. The series for $\operatorname{atanh}$ bounds it above by $1/\nu+4/(9\nu^3)$ for $\nu\ge2$. These inequalities make the derivative positive. Taking $\nu\to\infty$ gives the two reflected half-normal limits with scale $d/\sqrt2$ and density product $4/(\pi e d^2)$.
