# Feller–Pareto with held power and threshold — TASK-262

**Proved.** Cases 001625 and 001628 have no finite MLE. Their common likelihood supremum is attained in the inverse transformed-gamma limit at the held power and threshold. Every finite parameter point is strictly below it. The two archive records have identical binary64 observations and holds. The new sample certificate excludes *all* finite stationary points, including in the competing tail regions, using one log-span and one third central log moment.

The [transfer report](2026-11-25-transfer-math-report.md) gives the held-threshold reduction to GB2. Use the [GB2 held-power map](2026-11-08-gb2-cert-report.md) to write the data as scaled beta-prime observations $X_i=(x_i-\mu_0)^a$, with $p=\mathrm{shape3}$, $q=\mathrm{shape1}$, and $s=b^a$ for public scale $b$. Multiplicative centering is optional. Jacobians are parameter independent, so strict likelihood comparisons and attainment transfer. The [earlier Feller–Pareto work](2026-11-12-fpareto-slow-report.md) bounds all-free search work and validates face witnesses. Here the held power is essential.

## Uniform exclusion of finite stationary points

**Proved.** For positive nonconstant $X_i$, put
$$
v_i=\log X_i-\overline{\log X},\qquad
D=\max_i\log X_i-\min_i\log X_i,\qquad
\mu_3=\overline{v_i^3}.
$$
If $D\le 1/100$ and $|\mu_3|>69D^4$, no finite stationary point exists. Positive $\mu_3$ selects inverse gamma, and negative $\mu_3$ selects gamma. Bars denote sample means.

### Score cancellation

At a candidate stationary point let $N=p+q$, $m=p/N$, $h=pq/N$, and $u_i=X_i/(s+X_i)$. The log-scale score is $N(\bar u-m)$, so $\bar u=m$. Define
$$
T_i=\frac{u_i-m}{m(1-m)},\quad
L=\log m-\overline{\log u},\quad
R=\log(1-m)-\overline{\log(1-u)}.
$$
The logistic function $u(y)=(1+\exp(\log s-y))^{-1}$ has derivative $u(1-u)$. Both $\log u$ and $\log(1-u)$ have derivative of magnitude at most one. Since $m$ lies between the extremal $u_i$, on the sample log interval the ratio $u'(y)/[m(1-m)]$ lies in $[e^{-2D},e^{2D}]$. On $0\le D\le 1/100$, this differs from one by at most $3D$. Integrate between each pair of observations and average over the second index. This gives $|T_i-v_i|\le3D^2$, $|T_i|\le2D$, and $\bar T=0$, uniformly over all scales.

Write $H(z)=\log z-\psi(z)$, with $\psi$ the digamma function. The two shape scores set $L=H(p)-H(N)$ and $R=H(q)-H(N)$. The digamma integral gives
$$
H(z)=\frac1{2z}+\int_0^\infty e^{-zt}
\left[\tfrac12\coth(t/2)-1/t\right]\,dt .
$$
The bracket lies in $[0,t/12]$. For $w>0$, expand $w\cosh w-\sinh w$ and $(1+w^2/3)\sinh w-w\cosh w$ in power series. Both have nonnegative coefficients, proving the lower and upper bounds. Consequently, for $0<z<N$,
$$
0\le H(z)-H(N)-\tfrac12(1/z-1/N)
\le \tfrac1{12}(1/z^2-1/N^2).
$$
Thus $L/(1-m)^2$ and $R/m^2$ both have leading term $1/(2h)$. Their respective errors are nonnegative and bounded by $(1-m^2)/(12h^2)$ and $(1-(1-m)^2)/(12h^2)$. The absolute difference is at most $1/(6h^2)$.

On the sample side, $L=-\overline{\log(1+(1-m)T)}$, $R=-\overline{\log(1-mT)}$. The log series after the linear term has absolute remainder at most $z^2$ when $|z|\le2D$. In particular $L/(1-m)^2\le4D^2$, so the shape score forces $h\ge1/(8D^2)$. This controls the digamma error uniformly even when $m$ tends to either endpoint.

In $L/(1-m)^2-R/m^2$, the quadratic terms cancel and the cubic term is $-\overline{T^3}/3$. The log series remainder after the cubic is at most $|z|^4/3$ for $|z|\le2D$. After division by the respective squared weights the combined error is at most $16D^4[(1-m)^2+m^2]/3\le16D^4/3$. Also
$$
|T_i^3-v_i^3|\le |T_i-v_i|(4+2+1)D^2\le21D^4.
$$
At a stationary point the sample and digamma expressions must agree, which forces
$$
|\mu_3|/3\le (7+16/3+64/6)D^4=23D^4.
$$
The strict violation excludes the full positive parameter domain.

### From stationary exclusion to a global face bound

Use the existing [joint beta-prime exterior bounds](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md#joint-exterior-bounds). Along any likelihood level above the better face, the harmonic-shape bounds exclude zero and infinite harmonic shape, and the uniform gamma and reciprocal gamma bounds exclude either shape tending to infinity. Work with the unique scale optimum at each shape pair. Its scale stays between $qX_{\min}/p$ and $qX_{\max}/p$, so every such competitive profiled superlevel is contained in a compact subset of the open parameter domain. A point strictly beating both faces would yield a finite global maximum there, hence a stationary point. A finite point equaling the better face, with no larger value, would also be stationary. Both faces are approachable by positive beta-prime parameters, via one shape tending to infinity with scale adjusted to the gamma rate, or via reciprocation. This proves a strict upper bound by the better face at each finite point and identifies the supremum.

For the face comparison let $g_\pm=\log\overline{e^{\pm v}}$. In the original likelihood units the two profiled gamma likelihoods at the same gamma shape $k$ differ only through $-nk g_\pm$. Therefore the smaller gap yields the strictly higher face optimum. The sign of $g_+-g_-$ is the sign of $\overline{\sinh v}$. Its linear term averages to zero, its cubic term is $\mu_3/6$, and the remaining series has magnitude at most $D^5$ when $D\le1/100$. Under the strict skew condition the cubic dominates, proving the stated orientation without subtracting absolute likelihoods.

## Archive computation

**Enclosed computation.** Both records load from the [fit-audit case archive](2026-11-07-fit-audit-data/cases.json.gz), the input source of the [snapshot reference script](2026-11-21-gb2-acc-data/reference.py). The [checker](2026-11-25-fpareto-math-data/check.py) converts the original binary64 values to mpmath intervals at 110 decimal digits. It uses no scale grid. The [output](2026-11-25-fpareto-math-data/bounds.json) records the exact inputs in hexadecimal and rounds enclosure endpoints outward for display.

| Quantity | Enclosure for both cases |
|---|---:|
| $D$ | $[9.901286977173175959,9.901286977173175960]\times10^{-12}$ |
| $\mu_3$ | $[1.565412635196650184486,1.565412635196650184487]\times10^{-35}$ |
| $69D^4$ | $[6.6315596984780,6.6315596984781]\times10^{-43}$ |
| Inverse face minus ordinary face, total log likelihood | $[2.24777771740817,2.24777771743720]\times10^{-12}$ |

The face shape $k$ satisfies $H(k)=g_-$. The digamma bound above gives $k_0\le k\le k_0+1/6$ at $k_0=1/(2g_-)$ by multiplying the upper score bound by $2k$. More generally for $g_\pm$, the public face scale is $\exp(\overline{\log(x-\mu_0)}\pm(g_\pm-\log k)/a)$. The total face log likelihood is
$$
n\left[\log a-\overline{\log(x-\mu_0)}
+\tfrac12\log(k/(2\pi))-kg_\pm-w(k)\right],
\qquad 0<w(k)<1/(12k),
$$
where $w$ is the Stirling remainder. These formulas produce the following winning face enclosure, with threshold zero and power exactly `0x1.faf47fa66b243p+0`.

| Winning face quantity | Enclosure |
|---|---|
| Shape | [86154065716020863463041.15935451, 86154065716020863463041.32602118] |
| Public scale | [424695594989.537541213582935110168911365044, 424695594989.537541213583349988947719441301] |
| Total log likelihood | [127.81430798409594857218302394555230972959, 127.81430798409594857218303845444390539988] |

The [100-digit GB2 accuracy report](2026-11-21-gb2-acc-report.md) recorded a numerical central-profile deficit of about $1.124\times10^{-12}$ against the inverse face. Its $3.78\times10^{-12}$ number was the half-width on the finite witness, which overlapped the face value. The proof above resolves that comparison even though the strict gap against finite points tends to zero along the winning escape path.

The final Python check passed on the workstation. Reproduce from the worktree root with
```sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-25-fpareto-math-data/check.py
```

## General samples and equality

**Proved decision rules.** Constant positive excesses give the existing unbounded concentration case. For any nonconstant sample the narrow-skew inequality is sufficient for a strict face decision. Outside that condition, a finite feasible witness strictly above both optimized face values proves finite attainment by the compactness argument above. Identifying the maximizer, enclosing its coefficients and ruling out other maxima still require a compact cover. For a face decision outside the skew regime, the existing signed gamma tail bounds, reciprocal bounds and compact stationary/value cover give a second certificate when they close. The cover can eliminate every competitive finite stationary point, or enclose stationary maxima and compare them to the faces. No global unimodality assumption is needed.

For exact face–face equality the condition is $g_+=g_-$, equivalently $\overline{\sinh v}=0$. Reflection symmetry of the centered log sample suffices. Even then the finite region needs a separate certificate. If a finite point ties the better face in exact arithmetic and all points with larger value have been excluded, the finite point is an attained global maximum. A numerical overlap alone leaves that attainment question open. Equality in the skew inequality leaves this particular test undecided. When the leading signed tail coefficient in the beta-prime certificate vanishes, excluding the tail below its own face requires a higher-order signed bound uniform over the profiled shape, or another exclusion proof. A face with a strict gap to the incumbent can still be discarded using the unsigned remainder.

## What this changes for the fitter

Add the skew certificate to the three-free-parameter held-power GB2 route and transport it through fixed-threshold Feller–Pareto. Form log offsets before powering or centering. For narrow data, use $a\log1p((x_i-x_{\mathrm{ref}})/(x_{\mathrm{ref}}-\mu_0))$ with guarded difference arithmetic. Evaluate the skew condition in dimensionless form $\left|\overline{(v_i/D)^3}\right|>69D$, with a lower bound on the magnitude and an upper bound on the right-hand side. Keep the signed comparison certified under the package's arithmetic contract. Use its sign to select `TrGammaBoundary` or `InvTrGammaBoundary` with certified limit metadata. Fit the selected face in excess coordinates with the original power held. For small $v$, compute $g_\pm$ from the mean of $e^{\pm v}-1\mp v$ using a series or stable remainder evaluation followed by `log1p`. This preserves the second-order gap needed for face coefficients.

The regression for both archive records should expect a certified inverse transformed-gamma limit in native and double-only modes. The fixed-scale witness in the existing unresolved path is strictly suboptimal. Where the new test and the general certificate fail to conclude, retain a successful existing search result as uncertified under the fallback rule. Preserve actual search failures and proved stops. A finite certified result needs coefficient enclosures transported to public scale at the requested tolerance.

## Open

The skew test has no conclusion for broad or nearly symmetric log samples. The general beta-prime cover can decide separated cases when its bounds close. A complete elementary sample criterion across vanishing tail coefficients or exact ties between a finite maximum and a face remains open. Production margins for the dimensionless skew test and for the transported face coefficients remain to be validated in native and double-only arithmetic.
