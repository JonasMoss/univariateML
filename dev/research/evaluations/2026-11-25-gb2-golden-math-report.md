# Global maximum for the GB2 golden sample

**TASK-305. Proved conditional on the computed comparisons.** The all-free GB2 likelihood on the 500 stored binary64 observations has a unique global maximum. The replacement cover checks the entire previous finite rectangle, including all 395 formerly unresolved cells and both ratio edges. It leaves zero unresolved cells. Two terminal cells can contain a global maximizer, and every stationary point in either cell lies in the same strictly concave slab.

**Numerical.** In public order the maximizer is approximately `(0.57103303243282106, 1.12322098997071568, 0.71164837657400735, 1.00793655994134824)`. The stored estimates have a conditional maximum componentwise relative error bound of $9.8\times10^{-17}$. Their total log likelihood in the original data units is $-1322.5740862676913966$. [Golden fixture](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/golden.json) · [Replay results](2026-11-25-gb2-golden-math-data/results.json)

## Domain of the comparison

Write $y_i=(\log x_i-\overline{\log x})/s_y$, with $s_y$ the population standard deviation of the logs. All likelihood bounds below are means on this standardized scale. Power $a$ is in standardized units, $p=\text{shape3}$, $q=\text{shape1}$, $r=p/(p+q)$, $k=pq/(p+q)$, and $m$ is the mode in the standardized coordinate. Let $c=am-\operatorname{logit}r$ be the logistic intercept. The joint log likelihood is
$$
 \ell=\log a-\log B(p,q)-p\overline{\operatorname{softplus}(-u_i)}
                          -q\overline{\operatorname{softplus}(u_i)},\qquad
 u_i=ay_i-c .
$$
Equivalently, $\ell=\log a+\log C(k,r)-k\overline{D_r(a(y_i-m))}$, in the chart of the previous analysis.

**Proved using the prior exterior bounds, conditional on their replay.** The witness $w=-1.3958392060055100968$ is the likelihood at the stored estimates. The replay excludes $a\le0.04862370595404910$, $a\ge1144.948581115293$, and both complete gamma faces at level $w-10^{-5}$ or below. This gives finite attainment by the earlier compactness argument. The new finite cover uses the level $w-10^{-15}$, and overlaps the power cutoffs by $2\times10^{-12}$ in each direction. Its initial ten power intervals all span $r\in[0,1]$. Value bounds close the cells abutting the two seams directly. The prior uniform endpoint slab is therefore unnecessary for this finite replay. [Prior GB2 exterior proof](2026-11-25-gbshape-math-report.md#uniform-exterior-bounds) · [Held-power certificate and scope](2026-11-08-gb2-cert-report.md) · [GB2 dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md)

## Finite comparison

The following are exact-arithmetic bounds. The numerical section states the arithmetic used to evaluate them.

**Loss and concentration enclosures.** For each cell $[l,u]\times[r_-,r_+]$, use the prior mixed lower loss at $l$, with $r_+$ on the positive ray and $r_-$ on the negative ray. A supporting tangent over the sample range gives $d_L$ for the optimized mean loss. To get $d_U$, evaluate at a single proposed mode at power $u$ with the opposite ratio endpoint on each ray. Monotonicity on each ray gives $d_L\le d(a,r)\le d_U$, where $d(a,r)$ is the minimized mean loss over $m$. The proposal used for either bound may be inexact.

Let $R(x)$ be the Stirling remainder and $h(x)=-xR'(x)$. In the notation $J(v)=R(1/v)$ of the prior analysis, $h(x)=J'(1/x)/x=\int_0^\infty e^{-t}g(t/x)\,dt$. The positive increasing $g$ in that analysis makes $h$ positive and decreasing. With $P=p+q$, the concentration score is
$$
 \frac{1/2+h(p)+h(q)-h(P)}{k}-d(a,r).
$$
Since $P>p,q$, its numerator exceeds $1/2$. The earlier concentration upper bound then gives $k\in[1/(2d_U),1/d_L]$. At a fixed trial $k$, bound $p=k/(1-r)$, $q=k/r$, and $P=k/[r(1-r)]$ on the ratio interval and evaluate $h$ at the ordered endpoints. A uniformly positive score raises the retained lower endpoint. A uniformly negative score lowers the upper endpoint. These comparisons enclose the concentration maximum because its objective is strictly concave. [Prior loss, height and concavity bounds](2026-11-25-gbshape-math-report.md#two-dimensional-profile-and-cell-upper-bounds)

**Mode enclosure.** At the conditional optimum write $t=am$. The defining equation is $\overline{D'_r(ay_i-t)}=0$. Since $\partial_rD'_r=-(D'_r)^2$ and $D''_r>0$, the root $t$ decreases with $r$. Thus it suffices to bound the two extreme ratio roots over the power interval. At fixed ratio, implicit differentiation gives
$$
 t' = \frac{\overline{D''_r y_i}}{\overline{D''_r}},\qquad
 t''=\frac{\overline{D'''_r(y_i-t')^2}}{\overline{D''_r}} .
$$
All kernel derivatives here are at $ay_i-t$. The bound $|D'''_r|\le D''_r$ and the weighted variance bound give $|t''|\le S^2/4$, where $S=\max y-\min y$. At the midpoint power, validated endpoint scores bracket $t$. If the trial intercept differs from the root by at most $\delta$, each curvature weight changes by a factor within $[e^{-\delta},e^\delta]$. After normalization the factors lie within $[e^{-2\delta},e^{2\delta}]$, so the error in the weighted mean is at most $S(e^{2\delta}-1)$. Taylor's formula now bounds the change in $t$ away from the midpoint. The sample-range bound on $t'$ supplies a second enclosure. The final nuisance box follows from $c=t-\operatorname{logit}r$ and the bounds on $k,p,q$.

**Ratio-restricted asymmetric-Laplace upper.** By the softplus hinge inequality and the integrated trigamma bound for the beta normalizer,
$$
 \ell\le\log(ak)-ak\left[\frac{L(c/a)}{1-r}+\frac{T(c/a)}r\right]
                 +\psi_1(1)pq ,
$$
where $L(v)=\overline{(v-y_i)_+}$ and $T(v)=\overline{(y_i-v)_+}$. Indeed the extra log-normalizer term is $\int_0^p\int_0^q\psi_1(1+x+y)\,dy\,dx\le\psi_1(1)pq$. Maximizing the first two terms over the positive rate $ak$ gives $-1-\log [L/(1-r)+T/r]$. At fixed $r$ the expression inside the log attains its minimum at a data knot by piecewise affinity and the slopes outside the sample. At each knot its optimal ratio is $\sqrt T/(\sqrt L+\sqrt T)$ clipped to the cell. For an interior ratio cell, the correction is at most $\psi_1(1)/(d_L^2 r_-(1-r_+))$. This gives a whole-cell bound uniform in power above $l$.

**Likelihood and score mean-value bounds.** For the nuisance box in $x=(a,c,p,q)$, set $w_i=(1+e^{-u_i})^{-1}$, $v_i=w_i(1-w_i)$ and $z_i=(y_i,-1)^{\mathsf T}$. The Hessian blocks are
$$
 \begin{split}
 H_{a,c}&=\operatorname{diag}(-a^{-2},0)-P\overline{v_i z_i z_i^{\mathsf T}},\\
 H_{(a,c),p}&=\overline{(1-w_i)z_i},\qquad
 H_{(a,c),q}=-\overline{w_i z_i},\\
 H_{p,q}&=\psi_1(P)\mathbf 1\mathbf 1^{\mathsf T}
                         -\operatorname{diag}(\psi_1(p),\psi_1(q)).
 \end{split}
$$
The shape diagonal $\psi_1(p+q)-\psi_1(p)$ increases with $p$ and decreases with $q$ by the signs and monotonicity of $\psi_2$. The other diagonal follows by exchanging shapes. Logistic variance is even in $u_i$ and decreases with $|u_i|$. These facts select endpoints for the interval entries.

For a box midpoint $x_0$, let $G=\nabla\ell$, let $B_i$ bound $|x_i-x_{0i}|$, let $U_{ii}\le0$ upper-bound the diagonal Hessian entry, and let $M_{ij}$ bound the absolute cross entry. Taylor's formula gives
$$
 \ell(x)\le\ell(x_0)
 +\sum_i\max_{0\le d\le B_i}\{|G_i(x_0)|d+U_{ii}d^2/2\}
 +\sum_{i<j}M_{ij}B_iB_j .
$$
The four scalar maxima are clipped quadratic vertices or endpoints. This upper bound applies to every pair of optimized nuisance coefficients in the cell.

For score comparisons let $A_x=\int_0^1 H(x_0+v(x-x_0))\,dv$. Each entry lies in its Hessian interval. Multiplying $G(x_0)+A_x(x-x_0)$ by any fixed matrix $Q$ gives a valid necessary zero condition. The inverse midpoint Hessian supplies $Q$ as a proposal. A signed component rules out joint stationarity. Also, every stationary point obeys
$$
 x=x_0-QG(x_0)+(I-QA_x)(x-x_0).
$$
Intersecting this enclosure with the input box can place all its possible stationary points inside the winning slab. Projecting the contracted shape intervals back onto $p/(p+q)$ gives another exclusion test.

The replay also tests a necessary entropy comparison at larger powers. With $E_i=\operatorname{softplus}(u_i)-u_iw_i$, the four stationary equations imply
$$
 \overline E=\frac{P\psi(1+P)-p\psi(1+p)-q\psi(1+q)}P.
$$
To see this, the location equation gives $\overline w=r$, and the power equation gives $P\overline{u_i(w_i-r)}=1$. Substitute $u_i(w_i-r)=(1-r)\operatorname{softplus}(u_i)+r\operatorname{softplus}(-u_i)-E_i$, use the two shape equations, and apply the digamma recurrence. Integrating the decreasing trigamma in the numerator bounds the right side between $2k\psi_1(1+P)$ and $k[\psi_1(1+p)+\psi_1(1+q)]$. The entropy on the left is even and decreases with $|u_i|$. This check closed zero terminal cells in this replay.

**Slab and global conclusion.** The convex slab is the coordinate box of half-width $0.0005$ around the computed local root in $(a,c,p,q)$. Interval elimination on the negative Hessian gives positive pivot lower bounds $0.19883,0.17479,0.19210,0.00366$ after rounding down. Thus the likelihood is strictly concave on the slab. Every global maximizer is finite, has likelihood at least $w$, optimizes the nuisance parameters, and has joint score zero. The two terminal contractions place every such point in the slab. Strict concavity gives at most one stationary point there. The prior inward radial score test places a stationary point in the radius-$10^{-10}$ ball inside the slab. With the conservative curvature floor $10^{-9}$, its distance from the computed center is bounded by the center's score norm plus $10^{-40}$, divided by that floor. Transport through $a_{\rm public}=a/s_y$ and $\log b_{\rm public}=\overline{\log x}+s_y c/a$ gives the reported relative error bounds. [Prior local inclusion](2026-11-25-gbshape-math-report.md#python-evidence-and-work) · [Results](2026-11-25-gb2-golden-math-data/results.json)

## Arithmetic and measured work

**Numerical.** The final workstation replay used 85-decimal-digit mpmath for standardization, the inherited exterior comparisons and the local inclusion. The finite cover used NumPy long double with a 64-bit significand and extended exponent range. It used a base practical allowance of $2\times10^{-12}$, scaled for likelihoods, interval endpoints and concentration scores. The convex loss tangent includes a term proportional to the absolute row scores before summation, and the matrix-preconditioned comparisons enlarge their allowance by the absolute row norm of $Q$. The smallest accepted value gap before its final allowance exceeded $2.02\times10^{-7}$. The computations are conditional comparisons with practical allowances.

For a positive ray with $z\ge0$, let $v=1-e^{-z}$. The score uses $v/[r+(1-r)e^{-z}]$, and the negative ray uses the reciprocal chart $D_r(z)=D_{1-r}(-z)$. For large positive $z$ and small $r$, evaluate $\log(1+r\operatorname{expm1}z)$ as softplus of $\log r+z+\log v$. Near $r=1$, evaluate $D_r(z)=[z+\log(1-(1-r)v)/(1-r)]/r$ with `log1p` and carry the complement directly. The exact endpoints use their limiting formulas. This avoids subtracting log beta terms at huge shapes. The normalizer instead evaluates its three Stirling remainders, shifting a shape below 16 by gamma recurrence before using six terms. In Binet's arctangent integral, the finite alternating expansion is bounded by its first omitted term. The same geometric remainder bound applies to the derivative integrand $v/(1+v^2)$ for $h$. For trigamma, the integral with $t/(1-e^{-t})=1+t/2+2\sum_{j\ge1}t^2/(t^2+4\pi^2j^2)$ gives the first-omitted-term bound by the same finite geometric expansion. After shifting, the omitted magnitudes are bounded by $16^{-13}/156$, $16^{-13}/12$ and $7\cdot16^{-15}/6$, respectively.

The independent 85-digit arithmetic check passed 165 kernel and score points on 11 ratios and 15 displacements through $\pm8750$, 29 special-function points, and 36 height and concentration-score points. The largest scaled discrepancy was $1.81\times10^{-17}$. These checks sample the numerical error. The proof uses the analytical inequalities above and the replay's guarded comparisons. [Arithmetic checker](2026-11-25-gb2-golden-math-data/check_arithmetic.py) · [Arithmetic results](2026-11-25-gb2-golden-math-data/arithmetic.json)

| Terminal result | Cells |
|---|---:|
| Elementary likelihood bound | 5,067 |
| Ratio-restricted asymmetric-Laplace bound | 91 |
| Concentration tangent likelihood bound | 2,251 |
| Taylor likelihood bound | 6,473 |
| Preconditioned score exclusion | 713 |
| Stationarity confined to winning slab | 2 |
| Unresolved | 0 |

The finite search visited **29,184 cells**, including splits, with maximum subdivision depth 24. The 14,597 leaves satisfy $2(14597)-10=29184$. There were 99 terminal cells touching $r=0$ and 112 touching $r=1$, with narrowest edge width $1/128$ on each side. The two inherited gamma covers used 47 and 49 cells with 37 and 40 distinct power evaluations. The finite ledger recorded 283,126 height-and-score calls, 3,293,976 additional Stirling remainder derivative evaluations for concentration bracketing, 58,821 digamma and 201,161 trigamma evaluations. It recorded 230,020,500 kernel or kernel-score row visits, 127,423,500 kernel-curvature row visits, and 20,452,000 row visits for Hessians, scores and the entropy check. These counts include the nuisance work within each cell. Wall time on one reserved workstation slot was **292.8 seconds**, of which 284.1 seconds were in the finite cover. This is the observed Python workload on 500 rows.

Both final commands exited successfully on the workstation. The cover imports the unchanged exterior and local-root routines from the prior study. [Cover script](2026-11-25-gb2-golden-math-data/cover.py) · [Prior GB2 routines](2026-11-25-gbshape-math-data/gb2_cover.py) · [Prior gamma routines](2026-11-25-gbshape-math-data/gamma_cover.py) · [Full ledger](2026-11-25-gb2-golden-math-data/results.json)

```sh
export TMPDIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
mkdir -p "$TMPDIR"
data=dev/research/evaluations/2026-11-25-gb2-golden-math-data
/tmp/umvl-cpu/run python "$data/check_arithmetic.py"
/tmp/umvl-cpu/run python "$data/cover.py" --digits 85 --binary64
```

## What this changes for the fitter

The all-free route can issue `Ok` with `certified=true` when a valid finite witness clears the exterior bounds, every finite cell closes by value, score or contraction into a common validated slab, and the transported root enclosure meets `tol`. The certificate applies to the golden and, by log-affine equivariance, its exact transformations $x_i\mapsto Cx_i^d$ for $C>0$, $d\ne0$. Power transforms by division by $|d|$, scale by $b\mapsto Cb^d$, and negative $d$ interchanges the two beta shapes. The exact-arithmetic proof also persists in a sufficiently small sample neighborhood. The finitely many strict comparisons persist by continuity, including the exterior tests, and the nondegenerate root persists by the implicit function theorem.

The inequalities apply to nonconstant positive samples with all coefficients free. The power cutoffs, face gaps, cost and existence of a single winning slab depend on the sample. On ordinary data where a comparison or budget prevents certification, use the existing successful search result with `certified=false`. A proved refutation, a proved winning limit or nonexistence, or a failed search follows the existing stops. The golden certificate introduces no shape restriction and requires no fixture change.

At one global evaluation per visited cell, the observed count fits the default allowance of $128\times300=38{,}400$, including the inherited gamma cells and their profile points. Charge local proposal and bracket work separately under the solver cap. Row passes and special-function calls dominate the measured work, so a cell count alone does not size the time budget. In a production port, the knot sums for the ratio-restricted bound can use sorted prefix sums, and the timing budget needs measurement in the actual fitter. [Work-budget policy](../../design/work-budgets.md)

## Open

- **Binary64-only implementation.** The finite replay uses an extended exponent range. The displacement bound is about 8699, and an exact gamma-edge kernel may require $\exp(z)$ there. A portable implementation needs scaled or log-domain positive-ray sums and supporting tangents, or another validated bound whenever an exponential is outside range. The current replay makes no binary64-only performance or arithmetic claim.
- **Other samples and simultaneous winners.** If a gamma, normal or asymmetric-Laplace value ties or beats the finite witness, the strict exterior comparison used here cannot decide that case. It needs a signed uniform comparison against the refitted face or an attained competing witness. For separated finite slabs at overlapping likelihood levels, root isolation still needs a decisive comparison or a proof of equality. Two separated finite local maxima and exact finite global ties for all-free GB2 remain open.
- **Held coefficients and production cost.** Held scale or one held beta shape changes the nuisance equations and the admissible parameter sets, so the free-scale and free-concentration enclosures need corresponding constrained replacements. The golden's work count provides a baseline; an implementation study still needs timings and budget-exhaustion rates on larger samples and on near-ties.
