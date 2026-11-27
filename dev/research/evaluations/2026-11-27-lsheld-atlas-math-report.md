# Held-mask likelihoods for EMG, half-t, Huber, fatigue life, GPD and Feller–Pareto

TASK-407 · 2026-11-27

## Conventions and scope

Take a nonempty finite real sample. Held scales and positive shapes are finite and strictly positive. Compare positive joint likelihoods, or their logs when convenient. At a support endpoint use the declared pointwise density. An invalid held support is outside the claims below. Free-threshold fatigue and the larger free-threshold GPD slices are mathematical models even where the interface marks them unsupported. For half-t the free-shape floor is one only when location is free. GPD uses the fitted shape domain \(\xi\ge-1\) when shape is estimated.

Write \(B\) for the largest boundary envelope, after imposing the holds. In a bounded problem with continuous or upper semicontinuous finite likelihood, if every escaping sequence has limsup at most \(B\), and \(B\) is reachable, a fit exists iff a finite feasible point reaches \(B\). Indeed, a strict improvement confines a maximizing sequence to a compact superlevel set, and if no improvement exists then a point at \(B\) is a maximizer. When all finite points lose strictly, the winning faces determine the supremum and its nonattainment. An included uniform or included shape floor is a finite candidate. An excluded face at the same height as a finite winner does not change the number of finite maximizers. These are exact comparisons in the sense of the [closure lemma](../ml-behaviour.tex#lem:closure) and [tie classification](2026-11-25-ties-math-report.md). Numerical overlap alone supplies no equality proof.

## EMG

Write \(r=\lambda,\ s=\sigma,\ d_i=x_i-\mu,\ k=rs,\ m=\min x_i\). The density is
\[
 f_i=r e^{-r d_i+k^2/2}\Phi(d_i/s-k)
     =\int_0^\infty r e^{-rt}\phi_s(d_i-t)\,dt .
\]
Let \(N(\mu,s)=\prod_i\phi_s(d_i)\), and let \(E(r,\mu)\) be the product of the translated exponential densities, with a factor \(1/2\) per zero residual and zero if there is a negative residual. The five criterion statements agree with the already proved [held-mask Lean theorems](../../lean/UnivariateML/Family/SkewLSEMG.lean#L730), including the moving-location comparison and the held-location corner bounds later in that file.

**Proved boundary and profile facts used below.** The two peak bounds are \(f_i\le \min(r,1/(s\sqrt{2\pi}))\). For \(d>0\), split the convolution at \(d/2\) to get \(f\le r e^{-rd/2}+\phi_s(d/2)\); for \(d<0\), \(f\le\phi_s(d)\). Consequently, when \(s\to0,\ r\to\infty\) with location held and some nonzero residual, the product vanishes: multiply the exponential term by at most \(r^{n-1}\), and the Gaussian term by at most \((s\sqrt{2\pi})^{1-n}\). The same split excludes \(|\mu|\to\infty\) for \(s\) bounded above and \(r\) bounded below in the held-rate and held-scale regimes. For bounded positive \(s\), \(r\to\infty\) is the Gaussian limit, by writing \(t=Y/r\) with \(Y\) unit exponential. For fixed positive \(r\), \(s\to0\) gives the exponential trace from the displayed CDF. These bounds and limits apply on subsequences, and exhaust the corners with positive competitive likelihood. The limiting laws follow from the independent Gaussian and exponential representation.

At each fixed \(k\), the free nuisance coordinates in \(b=1/s,\ u=b\mu\) give the strictly concave objective from the [EMG certificate](2026-11-07-emg-cert-report.md#L80), with log-likelihood curvature
\[
 -n(db)^2/b^2-\sum_i w(k-bx_i+u)(x_i\,db-du)^2,\qquad
 w=-(\log\Phi(-\,\cdot\,))''>0.
\]
The sign follows from the inverse Mills ratio derivative. Each hold is affine in these nuisance coordinates at fixed \(k\); holding the rate fixes \(b=r/k\). At fixed \(k\) the standardized density has exponential decay on both sides, by splitting the convolution or by its CDF formula. Thus positive superlevels keep free \(b\) away from zero and infinity when some residual at the held location is nonzero. In a location-only nuisance fit positive superlevels bound \(u\) at its fixed \(b\). The negative Hessian gives a unique nuisance fit and an analytic profile by the implicit function theorem. When both nuisances are held the profile is analytic directly.

**Proved finite comparison.** As in the [all-free EMG argument](2026-11-25-emg-math-report.md#exact-finite-comparison-and-per-input-certificates), the profile derivative is definable in the o-minimal Pfaffian closure of the real field. The exponential and Gaussian CDF are solutions of the triangular scalar equations for \(e^x\) and \(\Phi'(x)=\phi(x)\). The graph of the unique nuisance root is definable by its score equations, and differentiation is definable by the limit formula. Thus a nonconstant analytic profile has finitely many stationary shapes, including degenerate roots. Each section below shows nonconstancy where needed. Compare the stationary heights with the listed boundary values to decide attainment. Finiteness rules out a continuum of EMG maximizers. A singleton proof still needs a shape comparison. [Speissegger's theorem](https://arxiv.org/abs/math/9710220)

### emg held lambda

**Proved.** The only positive escaping face is the moving exponential. At \(s\to0,\ \mu\to\mu_0\), a minimum tied \(j\) times and \(\mu_0=m\) multiplies the ordinary exponential likelihood by \(\Phi(h)^j\) when \((m-\mu)/s\to h\in[-\infty,\infty]\). If \(\mu_0<m\) there is no penalty, and \(\mu_0>m\) gives zero. The face maximum is \(B=r^n\exp[-r\sum_i(x_i-m)]\). It is reachable at the full endpoint value, for example with \(s=t^2,\ \mu=m-t\). Large scale or escaping location gives zero by the shared bounds. The [Lean moving face and criterion](../../lean/UnivariateML/Family/SkewLSEMG.lean#L1120) prove attainment iff a finite point reaches \(B\). Constant data lose strictly by the exponential peak bound. The location-profiled likelihood tends to zero as \(s\to\infty\), so it is nonconstant. **Uniqueness open.** At each stationary scale the conditional location is unique. In Gaussian precision \(\tau=1/s^2\) the conditional log-likelihood curvature at fixed location is
\[
 \ell_{\tau\tau}=-n/(2\tau^2)+\tfrac14\sum_i\operatorname{Var}[(d_i-T)^2\mid x_i],
\]
where \(T\) has the exponentially weighted Gaussian conditional density from the convolution. Profiling adds \(-\ell_{\tau\mu}^2/\ell_{\mu\mu}\ge0\), so the conditional strict location curvature does not control the sign of the outer curvature. A singleton argument would need to dominate both positive terms on the competitive precision range or order separated scale peaks directly.

### emg held lambda+mu

**Proved.** As \(s\to0\) the boundary product is \(E(r,\mu)\); as \(s\to\infty\) it is zero. Attainment iff a finite scale reaches \(E\) is the [Lean scalar criterion](../../lean/UnivariateML/Family/SkewLSEMG.lean#L730). A negative residual makes \(E=0\), while strictly positive residuals beat \(E\) at sufficiently small positive scale since the Gaussian tail losses are smaller than the gain \(e^{nk^2/2}-1\). Both cases attain. If all residuals vanish, \(e^{k^2/2}\Phi(-k)\) is strictly decreasing for \(k>0\) by the inverse Mills inequality, so the unattained supremum is \((r/2)^n\). In the mixed case with endpoint ties, use the penalized \(E\). The large-scale limit gives nonconstancy and hence a finite stationary list.

**Proved singleton on nonpositive residuals with at least one negative residual.** Write \(D_i=-d_i\ge0\), \(w=\log s\) and change variables in the convolution to \(T=s e^v\). Up to constants the integrand in \(dv\) has log
\[
 v-r e^{w+v}-\tfrac12 D_i^2 e^{-2w}-D_i e^{v-w}-\tfrac12 e^{2v}.
\]
It is jointly concave. Factor out \(\exp[-D_i^2e^{-2w}/2]\), which is independent of \(v\). Prékopa's marginal log-concavity theorem applies to the remaining integrand, and the factored term makes the log density strictly concave in \(w\) whenever \(D_i>0\). The product strictly log-concave in log scale therefore has a unique maximum, which exists by the negative residual.

**Uniqueness open for general attained samples with a positive residual.** In the transformed integrand the mixed term \(d_i e^{v-w}\) then has the opposite sign; the marginal argument fails. The negative-residual result does not address a second winning scale on such samples.

### emg held mu

**Proved.** If every observation equals \(\mu\), fixed \(k>0,\ s\downarrow0\) gives unbounded concentration. Otherwise the only positive ends are \(r\to\infty,\ s\to s_0>0\) with \(N(\mu,s_0)\), and \(s\to0,\ r\to r_0>0\) with \(E(r_0,\mu)\). The small-scale large-rate corner vanishes by the convolution split. Optimize \(N\) at the RMS residual. If all residuals are nonnegative, optimize \(E\) at \(r=n/\sum d_i\), retaining the half penalty per zero. Otherwise \(E=0\). Attainment iff a finite point reaches the larger of these values is the [Lean held-location criterion](../../lean/UnivariateML/Family/SkewLSEMG.lean#L1648).

For the finite comparison, the profile tends to the optimized exponential value as \(k\downarrow0\). The lower bound evaluates at a fixed rate, and the upper bound follows by subsequences from the face list since \(k\downarrow0\) excludes the normal face. If a negative residual exists, the limit is zero. If all residuals are strictly positive, a small \(k\) at the optimizing exponential rate beats the face. If \(j>0\) residuals are zero and the rest strictly positive, the profile is at most \(E_{\max} e^{nk^2/2}[2\Phi(-k)]^j\), strictly below \(E_{\max}\) for small \(k\). Thus the profile is nonconstant in every bounded case. All residuals strictly positive imply attainment because even the unpenalized exponential wins over the optimized normal by the mean–RMS inequality and \(\log(2\pi)>1\). For all residuals nonpositive, the normal wins strictly by pointwise domination of the delayed Gaussian.

**Proved singleton on constant positive residuals.** Write their common value as \(d>0\), put \(z=d/s>0,\ k=rs\), and set \(M(h)=\phi(h)/\Phi(h)\). Ignoring a constant, the one-observation log density is
\[
 \log z+\log k-kz+k^2/2+\log\Phi(z-k).
\]
The two scores at any stationary point imply \(k=z/(z^2-1)\) and \(M(z-k)=1/[z(z^2-1)]\), so \(z>1\). At such a point \(-M'(z-k)=1/z^2\) by the Mills identity. The Hessian determinant is \((z^2-1)(z^2-3)/z^4\), so a maximum has \(z\ge\sqrt3\).

For \(z>1\) define \(F(z)=M(z-z/(z^2-1))-1/[z(z^2-1)]\). At any zero,
\[
 F'(z)=-\frac{z^2-3}{z^2(z^2-1)} .
\]
At \(z=\sqrt3\) the argument of \(M\) is \(\sqrt3/2\), and \(F>0\) since
\[
 M(\sqrt3/2)\ge \frac{e^{-3/8}}{\sqrt{\pi/2}+\sqrt3/2}>\frac1{2\sqrt3}.
\]
The first bound uses \(\Phi(h)\le 1/2+h/\sqrt{2\pi}\), and the second uses \(e^{-3/8}>2/3\) and \(\sqrt{\pi/2}<\sqrt2<5\sqrt3/6\). Above \(\sqrt3\) every zero is a strict downward crossing, so there is at most one. Existence on constant positive residuals was proved above, hence there is exactly one maximizer.

**Uniqueness open for general nonconstant samples.** The two joint scores no longer eliminate the inverse Mills terms to give a single scalar \(F\); the determinant and crossing calculation uses equality of the residuals.

### emg held mu+sigma

**Proved.** The limits at zero and infinite rate are zero and \(N(\mu,s)\). The [Lean scalar criterion](../../lean/UnivariateML/Family/SkewLSEMG.lean#L773) is attainment iff some rate reaches \(N\). The zero-rate limit proves nonconstancy, hence finiteness of the stationary list. All residuals nonpositive imply strict normal victory by the convolution. A positive sum of residuals implies finite attainment by the inward expansion \(\log(F/N)=\sum_i d_i/(r s^2)+O(r^{-2})\) for joint likelihood \(F\). A positive sum is sufficient, and the exact finite/normal equality in the [certificate report](2026-11-07-emg-cert-report.md#L372) can occur with a negative sum. That equality proves finite attainment because its other winner is an excluded face.

**Proved singleton on constant positive residuals.** For their common \(d>0\) let \(g(t)=\phi_s(d-t)\). Integrating by parts in the convolution yields \(f(r)=g(0)+\int_0^\infty e^{-rt}g'(t)\,dt\), so \(f'(r)=-\int_0^\infty e^{-rt}t g'(t)\,dt\). The integrand before the exponential weight has negative sign for \(t<d\) and positive sign for \(t>d\). When \(r\) increases, the ratio of new to old weights strictly decreases in \(t\). Comparing to its value at \(d\) shows that at any zero, the derivative is positive at every smaller rate and negative at every larger rate. The positive residual sum proves attainment, hence the zero is unique. Replicating the residual does not change its maximizing rate.

**Uniqueness open for heterogeneous residuals.** The joint score sums \(f_i'/f_i\), whose denominators and change-of-sign locations vary with the residual. The single Laplace-transform sign change above does not directly control that sum.

### emg held sigma

**Proved.** The only positive escaping face is \(r\to\infty,\ \mu\to\mu_0\) with \(N(\mu_0,s)\). Its maximum is at the sample mean. The [Lean held-scale criterion](../../lean/UnivariateML/Family/SkewLSEMG.lean#L968) compares a finite point with that normal value using weak inequality. Constant data lose strictly by the Gaussian peak bound. The zero-rate limit is uniform in location and proves nonconstancy. At each stationary rate the location is unique. **Uniqueness open.** At fixed location \(\ell_{rr}=-n/r^2+\sum_i\operatorname{Var}(T\mid x_i)\); the profile adds the nonnegative term \(-\ell_{r\mu}^2/\ell_{\mu\mu}\). A monotone-score proof on the competitive region would need to bound their sum strictly below \(n/r^2\), or replace concavity by a comparison between rates. The Gaussian-face expansion only supplies a local direction. A sufficient condition for attainment is empirical variance greater than \(s^2\). Evaluate at \(\mu=\bar x-1/r\) and expand the ratio to the matched normal using the centered exponential delay \((Y-1)/r\). Its variance is \(r^{-2}\), giving the log-likelihood gain \(\sum_i[(x_i-\bar x)^2-s^2]/(2r^2s^4)+O(r^{-3})\).

## Half-t

Use residuals \(d_i=x_i-\mu\ge0\) and write \(C(\nu)=2t_\nu(0)\). The density equals \(C(\nu)s^{-1}(1+d_i^2/(\nu s^2))^{-(\nu+1)/2}\). A free location at every fixed finite positive scale and shape is uniquely the minimum, since raising it strictly increases at least one factor. This includes constant samples and held shapes below one. [Lean endpoint profile](../../lean/UnivariateML/Family/SkewLSHalfT.lean#L66)

For the comparisons, the [normal-end expansion](2026-11-25-halft-math-report.md#finite-root-list-for-general-inputs) supplies a strict eventual sign of the inward score, for either the RMS-profiled scale or a held scale. Its argument uses the smooth gamma-ratio normalizer in \(h=1/\nu\), whose Taylor series has zero radius, added to an analytic data term. Thus some finite-order coefficient is nonzero even if several leading coefficients vanish. An analytic profile with this sign has no accumulation of stationary points at infinity. At a held scale and \(\nu\downarrow0\), the log likelihood is
\[
 \tfrac12[n+j(1+\nu)]\log\nu + A(\nu),\qquad j=\#\{d_i>0\},
\]
with \(A\) analytic near zero, by factoring the positive-residual kernels and writing \(C(\nu)/\sqrt{\nu}\) as an analytic positive gamma ratio. This gives a strictly positive score near zero. At an included floor \(\nu=1\) the held-scale profile extends analytically across the floor. Consequently the applicable stationary list is finite. The first normal-inward coefficient of the held-scale log likelihood is \(\sum_i(q_i^2-2q_i-1)/4\), \(q_i=d_i^2/s^2\). A positive sign proves attainment. If every \(q_i\le1\), the normal face wins strictly since \((1+h)\log(1+hq_i)/h\ge q_i\) and the Student mode is strictly below the normal mode.

### ht held mu

**Proved.** Any endpoint tie gives divergence at \(s\downarrow0\) and a fixed positive \(\nu\) with \(\nu j<n-j\). The exponent of \(s\) in the product is \(\nu j-(n-j)\); at equality the product tends to \(C(\nu)^n\nu^{(\nu+1)j/2}\prod_{d_i>0}d_i^{-(\nu+1)}\). A joint approach to a finite critical shape can multiply this trace by \(\lim \exp[j(\nu-\nu_*)\log s]\), when that limit exists. The global supremum on the tie branch is infinite. To describe the small-shape corner when some residuals are positive, put \(w=s\sqrt{\nu}\). If \(\nu\downarrow0\) and \(w\) is bounded away from zero, the peak \(C(\nu)\sqrt{\nu}/w\) tends to zero. If \(w\to0\), the exact product is \([C(\nu)\sqrt{\nu}]^n w^{\nu j-(n-j)}\prod_{d_i>0}(w^2+d_i^2)^{-(\nu+1)/2}\), which includes path-dependent finite limits as well as vanishing and divergence. At \(\nu\to\infty,\ s\to0\) and \(j>0\), choose \(\nu_0>n/j\) in the kernel bound below to exclude positive likelihood. If \(j=0\) the exact product is just \([C(\nu)/s]^n\) for all paths. See [Lean collapse](../../lean/UnivariateML/Family/SkewLSHalfT.lean#L229).

With every residual positive, the only positive escaping face is the half-normal as \(\nu\to\infty,\ s\to s_0>0\), optimized at the RMS residual. The one-observation envelope obtained by maximizing at \(s=d_i\) tends to zero as \(\nu\downarrow0\), uniformly in scale. Large scale vanishes by boundedness of \(C\). Small scale on compact positive shape intervals has a strictly positive collapse exponent. For \(\nu\to\infty\), fix any \(\nu_0>1\); for \(\nu\ge\nu_0\), \(\nu\log(1+y/\nu)\ge\nu_0\log(1+y/\nu_0)\) for \(y\ge0\). It bounds each small-scale density by \(O(s^{\nu_0-1})\) with positive residual. This exhausts the joint escapes.

There is one conditional scale \(v=s^2\), solving \(n=(1+\nu)\sum_i d_i^2/(d_i^2+\nu v)\) by strict monotonicity in \(v\). Near \(\nu=0\) cancel the factor \(\nu\) to obtain \(n=(1+\nu)v\sum_i (d_i^2+\nu v)^{-1}\). The implicit root extends analytically to the positive harmonic mean of \(d_i^2\). The small-shape expansion above now applies with \(j=n\) and analytic profiled \(v\). Compare the finite stationary list with the half-normal value. Attainment holds iff some stationary shape reaches that value. **Uniqueness open.** There is one scale per shape and a finite list of stationary shapes. For \(u=\nu s^2\), \(y_i=d_i^2\) and \(T(u)=\sum_i y_i/(u+y_i)^2\), the shape curvature from the differentiated scale score is \(n b''(\nu)+n^2/[2(\nu+1)^3u T(u)]\), where \(b'(\nu)=\int_0^\infty e^{-\nu t}/(1+e^{-t})\,dt\). The terms have opposite signs. The [known non-unimodality example with positive residuals](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md#details) rules out a general decreasing profile score; uniqueness requires ordering the competing peak heights. The half-normal first-order test uses the same \(q_i\) at the RMS scale, because the normal scale score there is zero.

### ht held mu+sigma

**Proved.** The small-shape likelihood vanishes even with endpoint ties. The large-shape limit is the half-normal at the held parameters. Attainment holds iff some finite stationary shape reaches its value. The small- and large-shape arguments give a finite list.

**Proved singleton or escape on residuals of one positive magnitude, with optional zeros.** Write \(Q=(d/s)^2>0\) for the common positive squared standardized residual and \(w\) for its sample proportion. The doubled shape score per observation is the Laplace transform in \(\nu\) of
\[
 K(t)=\frac{2}{1+e^{-t}}-(1-w)+w\left[(Q-1)e^{-Qt}-\int_0^Q e^{-ut}\,du\right].
\]
This follows from the digamma difference integral and \(\log(1+Q/\nu)=\int_0^\infty e^{-\nu t}\int_0^Qe^{-ut}\,du\,dt\). For \(Q\le1\) the normal wins by the bound above. For \(Q>1\), \(K(0)=0\), \(K(\infty)=1+w\), and \(e^{Qt}K'(t)\) is strictly increasing: its terms are \(2e^{(Q-1)t}/(1+e^{-t})^2\), the constant \(-wQ(Q-1)\), and \(w\int_0^Q u e^{(Q-u)t}\,du\). If \(K'(0)\ge0\), the score is positive and the normal wins. Otherwise \(K\) crosses once from negative to positive. Increasing \(\nu\) downweights the positive part relative to the negative part, by comparing the exponential weight ratio at the crossing. Thus every score zero is a strict downward crossing; there is exactly one since the score is positive near zero by the small-shape expansion and negative for large \(\nu\) by \(K'(0)<0\). It gives a unique finite maximizer. The condition \(K'(0)<0\) is \(w(Q^2-2Q)>1\).

**Uniqueness open with general residual magnitudes.** The same kernel is an average of terms with different \(Q\). A one-crossing proof requires controlling their different exponential rates, or showing that separated shape maxima have unequal heights. For example \((0,4)\) at \(\mu=0,\ s=1\) attains by the inward coefficient, whereas \((0,1)\) at the same holds loses strictly to the normal face.

### ht held nu+sigma

**Proved.** The closed lower endpoint is compatible at \(\mu=\min x_i\) and strict residual monotonicity proves that this is the unique fit. At \(\mu\to-\infty\) the likelihood vanishes, and approaching the sample minimum gives the attained likelihood including all tied minima. There is no excluded winning face.

### ht held sigma

**Proved.** Profile location to the minimum and take \(\nu\ge1\). If the unprofiled location escapes to \(-\infty\), the kernel bound with \(\nu_0=1\) gives uniform vanishing. Finite limiting locations and infinite shape give the corresponding half-normal, maximized at the sample minimum. At positive finite shape, the closed location contact retains its finite density and the floor is an included candidate. Attainment holds iff the floor or a finite stationary shape reaches the half-normal value. The same finite-root and sufficient sign tests apply with the profiled residuals. **Proved scoped singleton.** For residuals of one positive magnitude with optional zeros, the preceding score proof applies to the restricted interval \(\nu\ge1\). If there is a score zero on \((0,\infty)\), the unique fitted shape is that root or the floor one, whichever is larger. If the score stays positive there is only a normal escape. For a constant sample the matched normal density dominates every finite shape. **Uniqueness open with general residual magnitudes.** A proof must rule out two equal interior peaks and equality of a winning interior peak with the included floor.

## Huber

Write \(c=\epsilon\), \(z_i=(x_i-\mu)/s\),
\[
 \rho_c(z)=z^2/2-(|z|-c)_+^2/2,\quad
 Z(c)=2I(c)+2e^{-c^2/2}/c,\quad I(c)=\int_0^c e^{-t^2/2}\,dt,
 \quad q(c)=2e^{-c^2/2}/[c Z(c)] .
\]
The scale profile at each finite \(c\), when free, is unique, by strict concavity in \(1/s\) of \(n\log(1/s)-\sum\rho_c(z_i)\) in the joint chart \((1/s,\mu/s)\). A free location is in the data hull. Its conditional set is the clipped-score root interval, with a positive-length interval precisely when an even sample's central gap exceeds \(2cs\). These results use the [Lean held-tuning proofs](../../lean/UnivariateML/Family/Huber.lean), including the equality case for the eroded gap.

**Proved face and shape comparisons.** For \(c\) bounded below, \(\rho_c(z)\ge c_0|z|-c_0^2/2\) when \(c\ge c_0>0\); also \(Z(c)\ge\sqrt{2\pi}\). These bounds confine \(s\) and \(\mu\) on positive superlevels when \(\inf_\mu\sum |x_i-\mu|>0\) over admissible locations and scale is free. They also confine free location with held scale. At \(c\to\infty\) the surviving nuisance coordinates give the normal face, including the held coordinates. At \(c\downarrow0\), put \(b=s/c\). The inequalities
\[
 |x_i-\mu|/b-c^2/2\ \le\ \rho_c(z_i)\ \le\ |x_i-\mu|/b,\qquad
 cZ(c)\longrightarrow2
\]
give a Laplace limit at each finite positive \(b\), uniformly in the residuals in the log-density difference. They also bound \(b,\mu\) on positive superlevels for the same nondegenerate free-scale samples. With held \(s\), the density peak tends uniformly to zero at this end. The only positive escaping faces in the bounded cases are therefore normal and, with free scale, Laplace. Both limits have normalized densities, and the envelopes at compatible nuisance values are the corresponding likelihoods. The normal optimizer uses the mean if location is free and the RMS residual if scale is free. The Laplace optimizer uses the median set if location is free and the mean absolute residual for \(b\).

For a finite shape, write \(P(c)\) for the conditional log-likelihood optimum. It is continuously differentiable with
\[
 P'(c)=nq(c)/c-\sum_i (|z_i|-c)_+
\]
evaluated at any conditional maximizer. The derivative follows by differentiating at the maximizer and using local compactness of the optimizing sets. If the location interval has positive length, the sum on the right is the same throughout it, by equal opposite tail counts. In detail, upper and lower difference quotients of the profile are bounded by the partial shape differences at conditional optima for the two endpoint shapes. As the increment tends to zero, limits of these optima are optimal at the starting shape, and their common partial derivative identifies the limit.

The stationary list is finite in all bounded masks. The nuisance score equations after clearing the factor \(1/s\) in the logarithmic derivative are piecewise polynomial in \((c,1/s,\mu/s)\); held coordinates are affine constraints in that chart. They are sufficient by concavity and describe the entire conditional set. Selection of a midpoint in the location interval and the unique inverse scale, or direct use of a held coordinate, gives a semialgebraic graph. Hence \(S(c)=\sum(|z_i|-c)_+\) is semialgebraic. The functions \(Z,q\) are definable in the same Pfaffian closure used for EMG, so the zero set of \(P'\) is a finite union of points and intervals. It has no interval: a semialgebraic \(S\) satisfies a nonzero polynomial relation on some open subinterval of any interval. If \(S=nq/c\) there, analyticity of \(q/c\) continues the polynomial relation to all \(c>0\). A nonzero polynomial relation is impossible for a strictly positive function decaying faster than every inverse power at infinity. Divide the relation by its lowest power of the function and the remaining lowest-degree-in-function coefficient is a nonzero polynomial in \(c\), which cannot decay at infinity. This argument includes kink points of the Huber loss. Compare the stationary heights with the faces for the attainment decision.

### huber held all free

**Proved.** On constant data there is no finite global maximizer by scale collapse at any fixed shape. Otherwise there are finitely many stationary candidate shapes and one location and scale at each shape that can maximize globally. The stationary list and faces are given above. To rule out a finite flat location interval when scale is also free, reuse the [flat-set theorem](2026-11-26-huber-flat-set-report.md#maximizer-sets-and-numerical-decisions). At a flat conditional optimum the scale score forces \(s=cD\), with \(D\) the mean absolute deviation about a median. Along this scale curve the all-tail log-likelihood derivative per observation is \(c-(1-q)/c>0\), including at the transition by differentiability. Indeed \(1-q<c^2\) for \(c\ge1\), and for \(0<c<1\) integration by parts gives \((1-c^2)I(c)<c e^{-c^2/2}\), equivalent to the same inequality. Hence an attained all-free fit has no continuous location multiplicity. **Uniqueness open across distinct shapes.** The all-tail derivative excludes a flat location interval at any winning shape by providing an improving shape and scale direction. It does not order the remaining shape peaks.

### huber held mu

**Proved.** If every residual is zero, the likelihood diverges by scale collapse at fixed shape. Otherwise the two faces and only positive escaping ends are the normal at the RMS and Laplace at the mean absolute residual. Attainment holds iff a stationary shape at its unique conditional scale reaches both. **Uniqueness open in general.** Each candidate shape has a unique conditional scale. For residuals of one nonzero absolute magnitude with optional zeros, write \(w\) for the nonzero fraction. The conditional scale equation gives standardized nonzero magnitude \(1/(wc)\) for \(c<w^{-1/2}\), and \(w^{-1/2}\) above that join. The profile derivative per observation below the join is \(wc-(1-q)/c\), and above it is \(q/c>0\). Since
\[
 \frac{c^2}{1-q}=c^2+cR,\qquad R=e^{-c^2/2}/I(c)\le 1/c,
\]
its derivative is \(2c+(1-c^2)R-cR^2\ge 2c-c^2R>0\). Thus the derivative can only cross upward below the join and there is no finite maximum on this two-level slice. For unequal nonzero magnitudes, several observations can enter the quadratic core at different shapes, and this single-transition sign argument no longer applies. As a sufficient nonattainment case, all nonzero residuals of the same absolute size, with no zero residuals, yield the two-point profile up to powers and rescaling; the normal wins by the [exact two-point analysis](2026-10-29-huber-free-epsilon-report.md#mathematics-proved-from-the-stated-density).

### huber held mu+sigma

**Proved.** There is only the matched normal face; the small-shape likelihood tends to zero. Compare the finite stationary shapes with that face for the attainment decision, retaining exact equality. Equivalently a finite fit exists iff some finite \(c>0\) satisfies
\[
 \tfrac12\sum_i (|z_i|-c)_+^2
 \ \ge\ n\log[Z(c)/\sqrt{2\pi}].
\]
All-zero standardized residuals give a strict normal winner since \(Z(c)>\sqrt{2\pi}\). With every \(|z_i|=10\), the finite witness in the next section beats the normal.

**Proved singleton whenever attained on equal absolute residuals.** If the common absolute standardized residual is \(R>0\), the score per observation is \(c+q/c-R\) on \(c<R\), and \(q/c>0\) on \(c\ge R\). On \(c<R\) its derivative \(1-(\log Z)''\) strictly increases: \(q\) strictly decreases by \(q^{-1}=1+ce^{c^2/2}I\), and each term of \((\log Z)''=q+(2q-q^2)/c^2\) decreases. Thus at most one zero crosses downward and the join cannot be a maximum. An attained fit is unique. **Uniqueness open for heterogeneous residual magnitudes.** Between observed magnitudes the derivative of the score is the fraction of observations in the tails minus \((\log Z)''\). It increases inside each interval and drops at a join. The equal-magnitude proof leaves open ordering the peak heights across joins.

### huber held sigma

**Proved.** The only positive excluded face is normal with location equal to the limiting location. Its optimized value uses the mean. The finite stationary comparison gives attainment iff some candidate reaches this value. At each winning shape the exact conditional set is the clipped-score interval, with a singleton if the interval degenerates. On constant data the normal wins strictly since \(Z(c)>\sqrt{2\pi}\) and the data-point mode bounds every density factor at held scale.

**Exact refutation of universal uniqueness.** For \(x=(-10,10),s=1\), symmetry and convexity give conditional optima with \(\mu=0\). For \(0<c<10\) the profile per observation is \(-\log Z(c)-10c+c^2/2\), and for \(c\ge10\) it is \(-\log Z(c)-50\). At \(c=1/10\) the density exceeds \(e^{-5}\): \(Z(c)\le20.2<e^4\) and the loss is less than one. At \(c\ge1/2\), even at the conditional optimum the loss is at least \(39/8\), and \(Z(c)\ge\sqrt{2\pi}>e^{1/8}\), giving strictly smaller density. On \(0<c<1/2\),
\[
 (\log Z)''=q+(2q-q^2)/c^2>1
\]
because \(q=1/(1+ce^{c^2/2}I)>1/2\). The profile is strictly concave there and tends to \(-\infty\) at zero. There is one winning shape \(c_*\in(0,1/2)\), specified exactly by \(c_*+q(c_*)/c_*=10\). The full global set has that shape and \(\mu\in[-10+c_*,10-c_*]\). This is a dyadic sample and hold. Only the value of the root quoted in the numerical replay is approximate.

## Fatigue life

Let \(m=\min x_i,\ \delta=m-\mu>0,\ d_i=x_i-m,\ j=\#\{d_i=0\}\), \(t_i=\delta+d_i\), \(a=\alpha\), \(b=\beta\), and \(q_i=t_i/b+b/t_i-2\). The endpoint at \(\delta=0\) has zero density at finite \(a,b\). The density for \(\delta>0\) is
\[
 f_i=\frac{\sqrt{q_i+4}}{2a\sqrt{2\pi}\,t_i}e^{-q_i/(2a^2)} .
\]
The existence and face results for these three masks already appear in the [fatigue threshold proof](2026-11-25-fatigue-math-report.md#4-free-threshold). The following adds exact finite comparisons and specifies the equality contact trace at held scale.

### fatigue held alpha

**Proved.** A constant sample is unbounded at \(\delta=b\downarrow0\); any sequence with diverging likelihood requires \(\delta,b\to0\). For a nonconstant sample every escape gives zero. The earlier proof uses the upper bound \(C_a\delta^{-n}\exp[-\sum q_i/(4a^2)]\). A gap \(D>0\) gives \(\sum q_i\ge b/\delta+D/b-2n\ge2\sqrt{D/\delta}-2n\), so it rules out the support edge uniformly in \(b\). A diverging \(\delta\) is excluded by the prefactor, and on compact positive \(\delta\) intervals either scale end makes the exponential tend to zero. For constant samples with \(\delta\to0\) and \(b\) bounded away from zero, \(b/\delta\) also forces zero. Thus attainment holds exactly for nonconstant samples.

The stationary set admits an algebraic decomposition. In coordinates \((\delta,b)\), both log-likelihood derivatives are rational from the density formula; clear the denominators on the positive quadrant. Their common zero set there is semialgebraic. Take one representative per semialgebraically connected component, of which there are finitely many. The value is constant on each component, since any two points can be joined by a piecewise smooth semialgebraic path and the log-likelihood gradient vanishes along it. Every global maximum is on one of these components. Testing the heights of representatives decides which stationary components maximize. On algebraic input, both representatives and their comparisons can in principle be obtained exactly as described below. **Uniqueness open.** Conditional scale can already have two maxima at a fixed threshold for some held shapes, as in the prior fatigue report. The extra threshold score must exclude simultaneous winning branches or prove a tie. The component argument alone does not exclude positive-dimensional stationary components.

### fatigue held alpha+beta

**Proved.** The log likelihood vanishes to \(-\infty\) at both threshold ends by the same bound, using \(b/\delta\) at the lower edge. Every sample attains. For exact comparison, let
\[
 g'(t)=\frac1{t+b}-\frac{3}{2t}-\frac{1/b-b/t^2}{2a^2}.
\]
Take the positive roots of the cleared numerator of \(\sum_i g'(\delta+d_i)\). Its limiting score at \(\delta\to\infty\) is \(-n/(2a^2b)\), so the numerator is a nonzero polynomial. The maximizing threshold lies among those roots. For constant data it is a singleton: rescale \(t=bu\); the numerator equation is \(u^3+(1+a^2)u^2+(3a^2-1)u-1=0\), which has exactly one positive root by Descartes' rule and the opposite signs at zero and infinity.

**Proved singleton for all samples if \(a^2\le1/2\).** Rescale \(t=bu\) and write \(G(u)\) for the resulting one-observation log density, ignoring constants. With \(\lambda=1/(2a^2)\ge1\),
\[
 G''(u)+G'(u)=\frac{u}{(u+1)^2}+\frac{3(1-u)}{2u^2}
       -\lambda\left(1-\frac1{u^2}+\frac2{u^3}\right)<0.
\]
For \(u\ge1\) the first term is at most \(1/4\), the middle is nonpositive, and the bracket is at least \(3/4\), separating \(1\le u\le2\) from \(u\ge2\). For \(0<u<1\), multiplication by \(u^3\) bounds the positive terms by \(1/4+3/8\) and the bracket below by \(u^3-u+2\ge1\). Thus the total score multiplied by \(e^{\delta/b}\), in the dimensionless threshold coordinate, decreases strictly. It starts positive and ends negative by the one-observation score limits and the presence of a minimum observation. Its root is unique.

**Uniqueness open for larger shapes on general nonconstant samples.** The damping bound above uses \(\lambda\ge1\); its sign estimate does not isolate all threshold roots or exclude equal winning heights outside that class.

On algebraic data and algebraic holds, both fixed-shape fatigue comparisons can be performed exactly. Up to common constants, the squared likelihood at a representative is the product of a positive algebraic factor and the exponential of the algebraic number \(-\sum q_i/a^2\). By Hermite–Lindemann, equality of two such values requires equal exponents and then equal factors. Otherwise arbitrary-precision bounds separate the values. This applies to finite roots as well as representatives of a stationary component.

### fatigue held beta

**Proved.** Constant samples are unbounded at \(\delta=b\) with \(a\downarrow0\). For nonconstant samples, \(Q=\bar q>0\) and the unique conditional shape is \(a^2=Q\). The squared profile likelihood is a common positive constant times
\[
 W(\delta)=\frac{\prod_i(t_i+b)^2/t_i^3}{[bQ(\delta)]^n}.
\]
It tends to zero at \(\delta\to\infty\), and on compact positive \(\delta\) intervals the shape ends vanish by the positive lower bound on \(Q\). At the lower threshold edge \(W(\delta)\sim C\delta^{n-3j}\) with \(C>0\). Along the profiled path the shape diverges. In the Birnbaum–Saunders representation \(T=b(aZ/2+\sqrt{1+(aZ/2)^2})^2\), \(Z\) is standard normal. The negative half of \(Z\) then sends \(T\) to zero and the positive half sends it to infinity. The finite contact trace corresponds to a defective distributional limit. The squared likelihood at arbitrary \(a\), divided by the squared profile, is \([u e^{1-u}]^n\) with \(u=Q/a^2\), so all contact penalties are joint and the envelope is reached at \(u=1\). In the constant case, one divergent end requires \(\delta\to b,\ a\to0\); the threshold contact can also be unbounded if \(3j>n\), as the same exact profile shows away from \(\delta=b\). Thus for constant data both mechanisms are available, and any divergent sequence has a subsequence reaching one of them, by the profile bounds at infinity and on compact positive threshold intervals away from \(b\). In the first mechanism the density concentrates at the observations; in the second \(a^2\) can grow on the order of \(1/\delta\).

For nonconstant observations, \(3j>n\) gives an infinite supremum, \(3j<n\) gives attainment, and \(3j=n\) gives attainment iff the cleared numerator of \(W-W_0\), \(W_0=W(0+)\), has a positive root. A root gives a feasible contact with the face value. Any strict improvement also forces such a root by continuity and the zero large-threshold tail. Without a positive root the face strictly wins. At \(b=1\), gaps \((0,1,1)\) strictly beat the critical face and \((0,1/10,1)\) do not reach it, by the exact comparisons in the [prior report](2026-11-25-fatigue-math-report.md#scale-held-shape-free). The stationary numerator of \(W'\) is nonzero since the positive profile vanishes at infinity. The finite stationary list has one conditional \(a=\sqrt Q\) at each root, and rational-input finite and boundary ties reduce to algebraic comparisons.

**Proved singleton on the exact example \(b=1,\ x=(0,1,1)\)** and its translations and positive rescalings with the corresponding held scale. The cleared stationary numerator is a nonzero factor times \(9\delta^4+36\delta^3+19\delta^2-11\delta-4\). It has one positive root by Descartes' rule and opposite signs at the two ends. Since attainment was proved at this example, that root gives the unique maximizer. **Uniqueness open for general samples.** No coefficient sign pattern or rational-score comparison shown here excludes multiple highest roots for arbitrary gaps.

## Generalized Pareto

Use \(m=\min x_i,\ M_x=\max x_i,\ R=M_x-m,\ y_i=x_i-\mu\) and \(s=\sigma\). At \(\xi>-1\) the density decreases strictly with excess on positive support, including the exponential at \(\xi=0\). Negative-shape upper contact has density zero except at the included closed uniform member \(\xi=-1\), which has density \(1/s\) at both ends. For \(\xi\ge-1\) every density factor is at most \(1/s\).

### gpd held mu+sigma

**Proved.** Put \(M=\max y_i\). If \(M=0\), every \(\xi\ge-1\) maximizes at \(s^{-n}\). If \(0<M\le s\), the unique maximizer is the uniform: any strictly positive excess makes a nonuniform density factor strictly less than \(1/s\). If \(M>s\), set \(v_i=y_i/M,\ c=s/M\in(0,1),\ b=\xi/c\). Positive likelihood requires \(b>-1\). The upper support contact as \(b\downarrow-1\) has likelihood zero since its shape tends to \(-c>-1\), and every sequence \(b\to\infty\) gives zero at a positive residual. At an included uniform upper contact when \(M=s\), the envelope is the attained uniform product. In particular approaching \(\xi=-1\) from above at fixed residual \(s\) loses no endpoint factor, since \((\xi+1)\log(1+\xi)\to0\). There is always attainment.

**Proved singleton when \(M>s\).** The scale-held objective from the [GPD certificate](2026-11-05-gpd-cert-report.md) has mean log likelihood, up to \(-\log s\),
\[
 h(b)=-(1/c+b)\overline{\int_0^{v_i}\frac{dt}{1+bt}},\qquad
 h'(b)=\int_0^1 \frac{(t/c-1)w(t)}{(1+bt)^2}\,dt,
 \quad w(t)=n^{-1}\#\{i:v_i\ge t\}.
\]
The integrand identity follows by direct differentiation. Both signed parts have positive mass because \(w(t)\ge 1/n\) for \(t<1\). If \(b_2>b_1>-1\), the ratio of new to old kernel weights \(\big[(1+b_1t)/(1+b_2t)\big]^2\) is strictly decreasing in \(t\). Multiplying \(h'(b_1)\) by the ratio at \(t=c\) gives a strict upper bound for \(h'(b_2)\): the negative part below \(c\) is upweighted relative to this ratio and the positive part above \(c\) is downweighted. Thus a zero is unique and has positive and negative signs on its respective sides. A zero exists at the interior maximum by the vanishing limits. At a zero the derivative is simple, since differentiating once more multiplies the integrand by the strictly decreasing function \(-2t/(1+bt)\); subtract its value at \(c\) to see that \(h''(b)<0\). The exponential point \(b=0\) is regular in this integral. For all-zero excesses, the infinite-shape end has the same product, but it cannot cause nonattainment.

### gpd held sigma

**Proved.** If \(R\le s\), the exact maximizing set consists of the uniform locations \([M_x-s,m]\) at \(\xi=-1\) and, only for a constant sample, all \(\xi>-1\) at \(\mu=m\). That set attains the global bound. If \(R>s\), the uniform is infeasible and strict monotonicity in location profiles to \(\mu=m\). The preceding score theorem with \(M=R\) gives a unique finite shape there. Hence all samples attain. The set is a singleton for \(R\ge s\), including equality of the positive span and the scale. A constant sample gives an exact refutation of universal uniqueness.

For the unprofiled ends, locations going to \(-\infty\) yield zero uniformly in \(\xi\ge-1\). For \(\xi>0\) one can use \(\sup_s f_\xi(y;s)=g(\xi)/y\), where \(g(\xi)=(1+\xi)^{-1-1/\xi}\le1\); at \(\xi\in[-1,0]\) the same bound follows by maximizing over scale, including the uniform at closed contact. At \(\xi\to+\infty\), a positive residual bounded away from zero gives zero, so this end can have positive product only if the sample is constant and \(\mu\to m\). In that case \(s^{-n}(1+v)^{-n}\) is the limit when \(\xi(m-\mu)/s\to v\in[0,\infty]\). At finite shapes greater than \(-1\), upper contact gives zero. At the included uniform support, upper semicontinuity follows from the universal density bound, and its full envelope is realized by the closed uniform. The envelope over strictly nonuniform approaches also reaches that value by approaching from the interior with upper slack of order \(\xi+1\) at a tied contact. More tangential approaches can only lower it. The positive constant-sample traces at infinite shape still attain the supremum at the finite members described above whenever they equal it. The laws along that shape end send their mass to infinity, since the survival probability at any fixed positive excess tends to one. There is no nonattaining branch.

### gpd held xi

**Proved on the fitted branch \(\xi\ge-1\).** At \(\xi=-1\), a nonconstant sample has the unique fit \((\mu,s)=(m,R)\); any compatible uniform needs \(s\ge R\), and equality fixes both endpoints. A constant sample diverges with \(s\downarrow0,\ \mu=m\). For \(\xi>-1\), profile to \(\mu=m\) and put \(j=\#\{x_i=m\}\). If the sample is constant, scale collapse at \(m\) is unbounded. For a nonconstant sample, \(-1<\xi\le0\) always attains; \(\xi>0\) attains iff \(\xi j<n-j\). At each attained nonuniform fit the scale is unique. The scaled scale score is
\[
 (1+\xi)\sum_i\frac{x_i-m}{s+\xi(x_i-m)}-n.
\]
It decreases strictly over positive-density scales, tends to \(-n\) at infinity, and has a positive left-end limit precisely on the indicated nonconstant attained branch. For \(-1<\xi<0\), the left limit is \(+\infty\) at \(s=-\xi R\); for \(\xi=0\) it is \(+\infty\) at zero. At \(\xi>0\) it is \((n-j)/\xi-j\). Equality gives a strictly decreasing scale profile and a positive finite unattained supremum; larger shape gives divergence. The scale equation and exponent are the fixed-threshold dossier formulas with the now-proved optimizing threshold substituted. [Fixed-scale-and-shape conventions and scale score](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md#details)

For exhaustiveness, \(s\to\infty\) gives zero by the density bound, and \(\mu\to-\infty\) gives zero by the bound \(g(\xi)/y_i\). A finite upper contact at a nonuniform negative shape gives zero. At \(\xi>0,\ s\downarrow0\), with \(\mu\to m\), the product is asymptotic to
\[
 s^{(n-j)/\xi-j}\left[\prod_{x_i>m}(\xi(x_i-m))^{-1-1/\xi}\right]
 \left(1+\xi(m-\mu)/s\right)^{-j(1+1/\xi)}.
\]
Thus at equality the full trace requires a threshold shift \(o(s)\); a shift proportional to \(s\) retains an explicit penalty, and larger shifts give zero there. If \(\mu\) stays strictly below \(m\), every excess is positive and the small-scale limit is zero. At nonpositive shape and a nonconstant sample, small scale is either infeasible for \(\xi\le-1\), infeasible for \(-1<\xi<0\), or exponentially suppressed at positive residual for \(\xi=0\). On constant data the same density formula at residual \(m-\mu\) gives the collapse trace exactly, with a zero factor outside support. Thus there is no other maximizing escape.

**Proved distinction below the fitted branch.** If a held \(\xi<-1\) is allowed in the reference model, the upper endpoint can contact \(M_x\) for any \(\mu\le m\) with \(\mu<M_x\), by taking \(s=-\xi(M_x-\mu)>0\). Approaching the contact from inside support makes the likelihood unbounded on finite-density points, even for constant data by choosing \(\mu<m\). Under the declared R7a value \(+\infty\) at upper contact, those pairs are exactly the extended-real maximizers on compatible support. The free-scale fitter rejects this branch. Its precise interior trace, with upper endpoint \(u=\mu-s/\xi>M_x\) and \(\gamma=1+1/\xi\in(0,1)\), is
\[
 (-\xi)^{-n}(u-\mu)^{-n(1-\gamma)}\prod_i(u-x_i)^{-\gamma}.
\]
It goes to zero if \(u\to\infty\), or if \(\mu\to-\infty\) while \(u\) stays away from \(M_x\). At \(u\downarrow M_x\) the displayed joint product covers moving-scale paths, including \(\mu\to-\infty\) and the constant-sample corner \(u-\mu\to0\). In particular the constant sample also diverges with \(\mu=m,\ s\downarrow0\). [GPD endpoint metadata](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md)

## Feller–Pareto

Set \(y_i=\log(x_i-\min)\) for strictly positive excesses. The Jacobian \(\prod_i(x_i-\min)^{-1}\) is fixed. The log-data law is GB2 on the log scale, with \(a=\mathrm{shape2}>0,\ p=\mathrm{shape3}>0,\ q=\mathrm{shape1}>0,\ c=\log(\mathrm{scale})\). Its density is
\[
 g(y)=a\,\exp[ap(y-c)]\,\big[B(p,q)(1+e^{a(y-c)})^{p+q}\big]^{-1}.
\]
The fixed-power archive proof does not address free power. The [GB2 exterior proof](2026-11-25-gbshape-math-report.md#uniform-exterior-bounds) gives the exterior maxima for the unrestricted log sample. The [single-held-shape exterior proof](2026-11-08-gb2-power-data/open-analysis.md#useful-bounds-for-a-future-held-q-free-scale-certificate) also gives eventual strict dominance of the appropriate power or Pareto face. Below is the joint envelope in a common coordinate system, including all limits of moving cutoffs.

**Proved compactification at positive likelihood.** Use the log-density mode \(m_0=c+\log(p/q)/a\), peak \(H\), and tail rates \(A=ap,\ B_r=aq\). Let \(D(t)=\log H-\log g(m_0+t)\). It is convex and nonnegative with
\[
 D'(t)=\frac{A B_r(e^{at}-1)}{B_r+A e^{at}}.
\]
The peak and mode bounds from the GB2 exterior proof follow directly from log concavity: \(1\ge H|y_i-m_0|/(1+D(y_i-m_0))\), by integration on the line segment from the mode to \(y_i\). Consequently the mean log likelihood is at most \(\log H+1-H\overline{|y_i-m_0|}\). For a nonconstant sample at a fixed positive joint likelihood level, \(H\) stays bounded above and below by positive constants, \(m_0\) stays bounded, and every observed loss stays bounded. Further, \(H\le A,B_r\), by integrating the one-sided exponential lower bound on either side of the mode, so neither rate can vanish. Embed the domain in the product of the extended coordinate intervals for \(a,m_0,A,B_r,H,h=aAB_r/(A+B_r)\) and take the closure of its image. The finite original parameters are recovered continuously from the finite positive power and rates and finite mode. The extra \(h\) coordinate distinguishes the small-power normal scales. Subsequence limits in this compactification give the following exhaustive cases.

- If \(a\to0\), let \(h=aAB_r/(A+B_r)\). Uniformly on bounded \(t\), \(D'(t)=ht[1+O(a|t|)]\), including ratios tending to zero or one, by the displayed derivative. Bounded loss at a nonzero distance excludes \(h\to\infty\). If \(h\to0\), integrating \(g/H\) on arbitrarily long fixed intervals contradicts the lower peak bound. Thus \(h\to h_0\in(0,\infty)\) gives the normal with log mean \(m_0\) and variance \(1/h_0\). If a beta shape is held then \(h\le a^2\min(p,q)\) excludes this case.
- If \(a\to a_0\in(0,\infty)\), two finite rates give a finite original point. Two infinite rates make the loss diverge at every fixed nonzero distance, which the sample range excludes. If \(B_r\to\infty\) and \(A\) has finite limit, the loss tends to \((A/a_0)(e^{a_0t}-1-a_0t)\). This is the transformed-gamma face with beta shape \(p=A/a_0\) and log mode \(m_0\). If \(A\to\infty\) with finite \(B_r\) the reflected loss gives inverse transformed gamma with shape \(q=B_r/a_0\). The convergence holds uniformly on compact \(t\) intervals.
- If \(a\to\infty\), for \(t>0\) the reciprocal form \(D'(t)=(1-e^{-at})/(1/B_r+e^{-at}/A)\) tends to \(B_r\) when finite and tends to infinity otherwise. Reflect to get the corresponding left slope. A finite-rate half-line gives linear loss by integration with the bounded slope on that side. On an infinite-rate half-line the loss diverges at each nonzero distance by integrating over a subinterval separated from the mode. Both rates infinite are impossible for a nonconstant sample at positive level. Two finite rates give asymmetric Laplace on the log scale. With one infinite rate the density on the other side is exponential in the log coordinate (a power or Pareto density in excess coordinates), with the cutoff at \(m_0\).

In each admissible limit the normalizing integral of \(e^{-D}\) converges to the integral of the limiting kernel. Indeed \(D\) converges at fixed offsets except possibly at a cutoff, is zero at zero in the mode chart, and is eventually bounded below by a fixed positive number at some fixed offset on each side. Convexity then gives integrable exponential bounds on both tails. This also proves convergence of the laws through the fixed Jacobian. At a hard cutoff all observations tied to it have the same factor. Their joint likelihood is bounded above by the limiting peak per tied observation, since each factor is always at most the peak. At the moving cutoff a particular path multiplies that product by \(\lim \exp[-jD(y_i-m_0)]\) when the limit exists, where \(j\) is the contact multiplicity. The full contact is reachable by keeping the mode at the desired cutoff, or shifting it outward by an amount tending to zero, since the surviving-side loss is bounded by its rate times displacement. This proves the joint envelope there. Outside a hard support a density factor vanishes. At all other surviving face points the density limits above hold also along the moving mode. Conversely take rates fixed, or diverging on the specified side, and set \(c\) by the mode equation to realize each allowed face. For the normal take \(p=q=2h_0/a^2\) in the small-power limit. Thus every positive escaping likelihood is bounded by a listed trace, and the maximal trace at each face point is reachable.

On constant excesses every requested mask is unbounded. Fix both beta shapes at compatible positive values, place the log mode at the common sample point and let \(a\to\infty\). Any sequence with density there diverging has \(H\to\infty\) and \(m_0\) tending to the observation, by the log-concavity inequality above. Such a sequence concentrates there: \(D(\pm t)\ge Ht-1\) at each fixed \(t>0\), and convexity bounds the mass beyond those offsets by integrals of exponentials with slopes tending to infinity. This is a point-mass escape.

**Proved attainment and conditional uniqueness.** For a nonconstant sample the greatest boundary value below is finite by the peak bound and reachability. It is a maximum of the envelope: approximate the supremum by face points and choose finite likelihood witnesses increasingly far out of compact subsets of the original domain. Extract a subsequence as above to reach a face whose envelope is at least this supremum. Decide attainment by comparing with a finite list of heights, as follows.

Use \(r=p/(p+q)\), \(\kappa=pq/(p+q)\), and let \(D_r(z)\) have zero value and derivative at zero and second derivative \(e^z/[1+r(e^z-1)]^2\). Then \(D(t)=\kappa D_r(at)\). For fixed \(a,r\), the unique maximizing mode minimizes the mean loss by strict convexity and is in the sample hull. Where both shapes are free, the unique concentration maximizing \(\log H-\kappa\overline{D_r}\) exists by the [mode and concentration profile](2026-11-25-gbshape-math-report.md#uniform-exterior-bounds). In that argument, \(\log H\) is minus the log integral of \(e^{-\kappa D_r(at)}\), giving a strictly negative second derivative through the variance of \(D_r\). The positive minimum data loss for nonconstant observations and the bound \(H\le a\sqrt{\kappa/(2\pi)}\) give the two coercive ends. If \(q\) is held, \(\kappa=qr\); if \(p\) is held, \(\kappa=p(1-r)\). The nuisance optima are analytic in \(a>0,0<r<1\) by the respective nonzero score derivatives. Write \(P(a,r)\) for the reduced mean log likelihood.

The reduced power score is \(1/a-\kappa\,\overline{(y_i-m_0)D'_r(a(y_i-m_0))}\). The ratio score is \(\partial_r\log H-\kappa\overline{\partial_rD_r}+\kappa'(r)(\partial_\kappa\log H-\overline{D_r})\), with partial derivatives taken at the fitted mode and concentration. The last term vanishes with both beta shapes free by the nuisance score; otherwise \(\kappa'\) is \(q\) or \(-p\) under the two holds.

The stationary set \(P_a=P_r=0\) is definable in the o-minimal expansion of the real exponential field with the positive gamma function established by [van den Dries and Speissegger](https://doi.org/10.1112/s0024611500012648). Indeed the beta normalizer is a gamma quotient; the nuisance optima are definable by their unique score solutions, and implicit derivatives are definable. There are finitely many definably connected stationary components, including isolated points. Each has constant likelihood: join points by a definable piecewise differentiable path and apply the chain rule. One height per component suffices for the attainment comparison. The theorem leaves open how many winning components there are and whether they are isolated points.

A second profiling order proves a unique power and scale at each fixed pair \(p,q\) for any nonconstant sample. Put \(u=ac\). Up to constants the mean log likelihood is
\[
 \log a+p\,\overline{a y_i-u}-(p+q)\,\overline{\log(1+e^{a y_i-u})}.
\]
It is strictly concave in \((a,u)\): \(\log a\) gives strictness in the power direction and the softplus gives strictness in the location direction. The peak is \(O(a)\) as \(a\to0\) with shapes fixed. As \(a\to\infty\) both rates diverge, ruled out at a positive likelihood level by the exterior proof. At bounded positive power, a diverging \(u\) drives all log residuals into a tail. Hence this conditional optimizer exists and is unique. The same proof applies under each single beta-shape hold. Let \(J(p,q)\) be the resulting profile. Its Hessian is the shape Hessian at fixed \(a,u\) plus a positive semidefinite Schur correction from the power and scale adjustment. At fixed \(a,u\) the shape Hessian is the negative Hessian of \(\log B(p,q)\). The signs of the two terms by themselves do not prove concavity of \(J\).

### fpareto held min

**Proved.** For constant positive excesses the supremum is unbounded. Otherwise the complete positive exterior comprises lognormal, the transformed-gamma and inverse-transformed-gamma families with free positive power and limiting shape, asymmetric Laplace on logs, and its one-sided power and Pareto endpoint limits. For the asymmetric Laplace face, optimizing both rates at each mode and then the mode reduces to the sample log knots, including the one-sided extremes, by the square-root sum argument in the [GB2 exterior proof](2026-11-25-gbshape-math-report.md#uniform-exterior-bounds). The finite criterion compares the stationary-component heights with the greatest face value. A strictly higher feasible point suffices for attainment; a finite height equal to the winning face also attains, provided none is higher. **Uniqueness open.** The conditional maxima above imply that distinct global winners must have different beta-shape pairs. On reflection-symmetric log data the two-shape profile satisfies \(J(p,q)=J(q,p)\), and an off-diagonal global maximum would yield two distinct global maximizers. The missing step for that construction is a finite witness strictly above the face heights and every diagonal value \(J(p,p)\); symmetry alone does not supply it.

### fpareto held min+shape1

**Proved.** Here \(q\) is held. Constant excesses have unbounded concentration. For a nonconstant sample the only two positive faces are inverse transformed gamma with shape \(q\), and the upper-cutoff power law in excess coordinates. A finite \(a_0\) has finite right rate \(a_0q\), forcing the reflected gamma orientation at the only possible finite-power escape. Large \(a\) sends the right rate to infinity and leaves a finite positive left rate, producing the power law. Small \(a\) cannot survive at positive likelihood. The cutoff maximizing the power likelihood is at the largest observation in excess coordinates; in logs its rate is \(1/\overline{(y_{\max}-y_i)}\).

The inverse transformed-gamma face can be optimized by a unique scalar solve. In reflected log coordinates \(z_i=-y_i\), its density in a mode chart has loss \(q(e^{a(z_i-m_0)}-1-a(z_i-m_0))\). Optimizing \(m_0\) gives the per-observation likelihood, up to a constant, as \(\log a-q\log\overline{\exp[a(z_i-\bar z)]}\). It is strictly concave, vanishes to \(-\infty\) at both ends and has one maximizer. Attainment is equivalent to a stationary-component height reaching the better of the two fitted faces. The known large-power bound is pointwise strict below the fitted power face for all sufficiently large \(a\); this strengthens the envelope without deciding the other face. [Held-right-shape exterior](2026-11-08-gb2-power-data/open-analysis.md#useful-bounds-for-a-future-held-q-free-scale-certificate)

**Uniqueness open.** At each free \(p\) the conditional power and scale are unique. Proving \(J(p,q)\) strictly concave would require its scalar Schur correction to be smaller than \(\psi_1(p)-\psi_1(p+q)\) in the competitive region, or a different single-crossing or peak-height argument. Reflection exchanges which shape is held and supplies no distinct winner on this slice by itself.

### fpareto held min+shape3

**Proved.** Here \(p\) is held. Reflecting log excesses and interchanging \(p,q\) proves that the two positive faces for nonconstant data are transformed gamma with shape \(p\), and the lower-cutoff Pareto face at the smallest excess. The fitted Pareto log rate is \(1/\overline{(y_i-y_{\min})}\), and the fitted gamma face uses the same strictly concave power solve with \(z_i=y_i,\ q\) in that formula replaced by \(p\). Constant excesses are unbounded. The reflected stationary-height comparison gives the exact attainment criterion. This reflection has unit Jacobian on the log data. The common original-excess Jacobian is restored before comparing physical likelihoods. **Uniqueness open.** At each free \(q\) the conditional power and scale are unique; the missing scalar Schur bound now uses \(\psi_1(q)-\psi_1(p+q)\).

## Results by question

Under uniqueness, “Proved” denotes a singleton theorem throughout the attained branch, and “Refuted” denotes an exact example of multiple global maximizers. “Partial” denotes a scoped singleton or a structural result, with general uniqueness open. The dash marks questions outside the job.

| Family | Held | Criterion | Failure | Uniqueness or complete set |
|---|---|---|---|---|
| EMG | lambda | Proved | Proved | Partial |
| EMG | lambda+mu | Proved | Proved | Partial |
| EMG | mu | Proved | Proved | Partial |
| EMG | mu+sigma | Proved | Proved | Partial |
| EMG | sigma | Proved | Proved | Partial |
| Half-t | mu | Proved | Proved | Partial |
| Half-t | mu+sigma | Proved | Proved | Partial |
| Half-t | nu+sigma | Proved | Proved | Proved, singleton |
| Half-t | sigma | Proved | Proved | Partial |
| Huber | all free | — | — | Partial |
| Huber | mu | Proved | Proved | Partial |
| Huber | mu+sigma | Proved | Proved | Partial |
| Huber | sigma | Proved | Proved | Refuted |
| Fatigue | alpha | Proved | Proved | Partial |
| Fatigue | alpha+beta | Proved | Proved | Partial |
| Fatigue | beta | Proved | Proved | Partial |
| GPD | mu+sigma | Proved | Proved | Refuted on all-zero excesses, singleton otherwise |
| GPD | sigma | Proved | Proved | Refuted below the span condition for a singleton |
| GPD | xi | Proved | Proved | Proved, singleton whenever a finite-likelihood MLE exists |
| Feller–Pareto | min | Proved | Proved | Partial |
| Feller–Pareto | min+shape1 | Proved | Proved | Partial |
| Feller–Pareto | min+shape3 | Proved | Proved | Partial |

## What this changes for the fitter

- **EMG and half-t.** A certified finite root at the top height is a fit even if an excluded face ties it. For an exponential trace preserve the moving or held location: at a held coincident location retain the half-density penalty per tie. Half-t has no scale-collapse diagnostic with scale held. With location held and scale free, a compatible exact endpoint tie and free shape give unboundedness even if the corresponding free-location sample would satisfy the fitted shape-floor rule.
- **Huber.** Compare stationary candidates against the compatible normal and, only if scale is free, Laplace likelihoods. At a certified winning held-scale shape, a positive eroded gap gives the complete closed location interval, with a singleton at gap equality. A free-scale finite optimum has one location at each winning shape. Do not classify a normal or Laplace face as the sole winner from a numerical tie.
- **Fatigue.** An implementation of the unsupported location routes has exact dispatch from the threshold-gap multiplicity. Both nuisance holds permit the positive threshold score polynomial. At held scale and free shape the critical contact is finite and can be checked by a positive-root count of \(W-W_0\). Distinguish an unattained threshold trace from the unbounded collapse at \(\delta=b,\ a\downarrow0\) or at the support edge. On algebraic inputs, the rational profile and the fixed-shape exponential-of-algebraic height comparisons give exact equality predicates.
- **GPD.** At fixed scale, use the closed uniform cases first. When the scale is smaller than the relevant span or maximum excess, bracket the sole simple shape root in \(b>-1\) with the signed score integral. A globally covered multi-root search is unnecessary in that branch. A fixed-shape free-threshold route can use the minimum and the existing unique-scale solve, with equality in the positive-shape collapse test reported as finite unattainment. At the uniform shape use both sample endpoints.
- **Feller–Pareto.** With a beta shape held, the two fitted face heights require only a closed-form rate and one strict scalar power solve. The eventual strict large-power comparison can remove that tail using the existing proof. In all three masks use shape and scale coefficients transported from a validated finite fit, or name a face only after excluding every finite competitor at its value. Stationary-component finiteness is a mathematical set description and supplies no reliable search cap.
- **Fallback.** If an ordinary finite search has passed its local score and accuracy checks and the face or cardinality comparison is unresolved, retain the finite result with its uncertified flag unless a separated bound proves it wrong. Use a proved strict face winner, proved unboundedness, or the corresponding failure of the search itself for a stop.

## Dossier follow-ups

- **EMG.** Record the weak inequality in the five held-mask attainment rules and the moving versus held minimum contact factors. The all-free coexistence examples do not transfer to those held profiles.
- **Half-t.** The applicability of the shape floor follows the free-location mask. Held scale removes the collapse geometry even when the floor remains in force. Record the root-list proof for both fixed-scale free-shape profiles and the held-location RMS profile.
- **Huber.** Add the exact held-scale/free-shape interval example. The joint all-free route has no finite flat location interval by the all-tail scale comparison.
- **Fatigue.** Replace the held-shape blanket statement that an MLE exists by “A finite maximum exists for nonconstant data; constant data have unbounded likelihood as threshold distance and scale tend to zero together.” With threshold free and both other coefficients free, the previously proved spike works for tied minima too. With scale held, include both unbounded mechanisms for constant data and the \(3j=n\) comparison for nonconstant data. Both coefficients held always permit a mathematical threshold MLE.
- **GPD.** State the held-scale singleton result for \(M>s\) and the matching free-threshold claim for \(R>s\). The support branch \(\xi<-1\) has an unbounded interior likelihood and infinite endpoint values under the reference convention, while the fitted branch excludes it. With freely fitted scale at the uniform shape and free threshold, a nonconstant sample has exactly one maximizing pair.
- **Feller–Pareto.** In the held-beta-shape masks the corresponding transformed-gamma face exists at every positive held shape. The Weibull and inverse Weibull are the unit-shape specializations. State the single one-sided large-power face that survives in each mask and the absence of a small-power lognormal when a beta shape is held.

## Open

- **EMG.** The held profiles have finite stationary lists. A practical method that proves a signed tail past the last root for arbitrary held inputs is missing, especially at cancellation in the normal comparison. A universal singleton proof or an exact two-finite-winner example within a particular held mask remains open. The exact held-location-and-scale finite/normal tie does not provide that example.
- **Half-t.** The divergence of the normalizer Taylor series ensures an eventual nonzero coefficient. An effective uniform order bound and a practical remainder sign on the resulting normal collar are missing. A separate ordering or exact equality of two finite root heights for these held masks has not been established.
- **Huber.** Finite stationary-list size and the full conditional location set are proved. The remaining finite equality problem is ordering the heights across disjoint shape roots outside the one-tail example, and comparing a finite root with the normal or Laplace face at an exact equality. A verified algorithm must also handle degenerate stationary roots and joins.
- **Fatigue.** For held shape with both nuisance parameters free, the argument leaves open whether the rational score equations have nontrivial connected components. Bounding their dimension, or proving they reduce to isolated points for every nonconstant sample, requires control of common factors of the two cleared score polynomials on the positive quadrant. In the scalar threshold fits, the polynomial comparison resolves any given algebraic input in principle; a general bound on the number of winning roots outside the proved singleton classes remains open.
- **Feller–Pareto.** The remaining step for an effective general certificate is to isolate the stationary components of the reduced two-dimensional score, prove which are points, and order their heights against the allowed faces. Near the bounded-power gamma seam for a held beta shape, the existing leading inward correction can vanish. A signed remainder uniform over the profiled mode and power is needed there unless a separate strict likelihood gap excludes the seam. No universal singleton or two-finite-winner claim follows from the held-power examples.
- **All six families.** Exact finite-height comparison is a mathematical criterion. Apart from the algebraic fatigue tests and the explicit singleton and flat-set cases, the comparison is generally open as an effective equality predicate on represented inputs.

## Reproduction

The numerical checks used mpmath at 80 decimal digits on the fixed samples and points in [check_examples.py](2026-11-27-lsheld-atlas-math-data/check_examples.py), including three EMG contacts, three half-t normal-side checks, eight GPD score integral comparisons and four large-power Feller–Pareto paths. The Huber shape for the exact counterexample is numerically \(0.1000066286522483083705\). SymPy checked both fatigue examples with exact rational arithmetic, including the absence of a positive root on the nonattaining sample. The singleton checks verify the EMG stationary determinant and crossing derivative, the two fatigue score identities and the exact positive-root count, and the half-t Laplace score at two held-scale workloads. All assertions passed on the workstation through the one-slot wrapper. The computations corroborate the displayed formulas and examples. The proofs above establish the classification.

~~~sh
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-27-lsheld-atlas-math-data/check_examples.py
~~~

The [atlas rows](2026-11-27-lsheld-atlas-math-data/proofs.csv) are for the coordinator’s join. The [row checker](2026-11-27-lsheld-atlas-math-data/check_atlas_rows.py) verifies mask coverage, directions and links.
