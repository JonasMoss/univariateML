# Double Pareto existence, uniqueness and faces

TASK-360 · 2026-11-25 · Base `338f045b`

**Proved.** The eight hold masks reduce to explicit rate optima, weighted quantiles, or a finite comparison of sample knots and one-sided faces. With all coefficients free, exact comparisons settle every sample with up to three distinct observations and every comparison of multiplicative rank at most two. An exact 448-observation binary64 sample has 13 isolated finite maximizers. Exact ties can also occur without reciprocal reflection. With one rate held, each ordered pair of knots has a unique crossing as the held rate varies, including comparisons with the one-sided face.

**Numerical.** The archived native and double-only public rates agree with 100-digit conditional references within $4.01\times10^{-16}$ relative error. Adjacent binary64 inputs straddle a finite/face switch and a finite/finite switch. The switches involve distinct physical kinks even where the change in mean log likelihood is below $10^{-16}$.

## All eight masks

Write $z_i=\log x_i$, $c=\log b$, $L(c)=\overline{(c-z_i)_+}$ and $R(c)=\overline{(z_i-c)_+}$. Here $b$ is the public scale and $\alpha,\beta$ are the left and right rates. The reduced mean log likelihood is
$$
\lambda(c,\alpha,\beta)=\log\frac{\alpha\beta}{\alpha+\beta}-\alpha L(c)-\beta R(c).
$$
The actual mean log likelihood is $\lambda-\bar z$. The [TASK-235 report](2026-11-24-dblpareto-report.md#likelihood-and-existence) proves strict conditional concavity in the rates, the rate formulas, strict profile convexity inside every nonempty sample gap when at least one rate is free, and strict profile improvement from outside the sample range toward the nearest extreme. Those results give the exhaustive candidate set used here. All cases below assume a nonempty sample and positive finite observations and holds. Invalid input gives `InvalidInput`. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/README.md) · [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/mle.hpp)

For two free rates and $L,R>0$, the unique conditional solution is
$$
\alpha=\frac{1}{\sqrt L(\sqrt L+\sqrt R)},\qquad
\beta=\frac{1}{\sqrt R(\sqrt L+\sqrt R)}.
$$
The reduced mean log likelihood at this solution is $-1-\log Q$ with $Q=L+R+2\sqrt{LR}$. At a one-sided face with positive opposite distance the same value formula holds. With one free rate $r$, held rate $h$, and free-side distance $t>0$, the unique conditional rate satisfies $t=h/[r(r+h)]$. The positive inverse rate is $t/2+\sqrt{(t/2)^2+t/h}$. Thus at a fixed kink the only comparisons required to determine existence are $L=0$ and $R=0$, equivalent to $b\le\min x$ and $b\ge\max x$, respectively. A face holds the endpoint and any surviving held rate. If the surviving rate is free, its fitted value is the reciprocal of the positive opposite distance.

| Held coefficients | Exact decision and outcome |
|---|---|
| None | Constant data give `Unbounded`. Otherwise minimize $Q$ over the distinct observed knots, with both extremes counted as faces. Return every interior minimizing knot with its rates. If none minimizes, report the minimizing Pareto or power face, or both at equality. |
| $b$ | $L,R>0$ gives one finite fit. $L=0<R$ gives `ParetoLimit` with rate $1/R$. $R=0<L$ gives `PowerLimit` with rate $1/L$. $L=R=0$ gives `Unbounded`. |
| $\alpha$ | With constant data, `PowerLimit` at the common observation with rate $\alpha$. Otherwise compare the finite knots from the minimum through the penultimate distinct observation with the power face at the maximum. Apply the crossing rule below. |
| $\beta$ | With constant data, `ParetoLimit` at the common observation with rate $\beta$. Otherwise compare the finite knots from the second distinct observation through the maximum with the Pareto face at the minimum. The same crossing rule applies on reversing the log axis. |
| $b,\alpha$ | $R>0$ gives one finite fit, including when $L=0$. $R=0$ gives `PowerLimit` with rate $\alpha$, including a constant sample at $b$. |
| $b,\beta$ | $L>0$ gives one finite fit, including when $R=0$. $L=0$ gives `ParetoLimit` with rate $\beta$, including a constant sample at $b$. |
| $\alpha,\beta$ | A finite fit always exists. For sorted observations $x_{(i)}$, take the first split $k$ with $\alpha k\ge\beta(n-k)$. The answer is $x_{(k)}$ unless equality holds with $x_{(k)}<x_{(k+1)}$, in which case the entire closed interval between them maximizes. A split inside an observation tie collapses to a singleton. |
| $b,\alpha,\beta$ | The held vector is the sole feasible vector and attains its likelihood. |

**Proved equality rule.** When a finite knot and an excluded face share the best value, the finite knot belongs to the MLE set. A face contributes no finite maximizer. Exact finite/finite ties give a discrete set when a rate is free because the profile is strictly convex inside gaps. With both rates held, the slope of negative mean log likelihood in a gap is $[\alpha k-\beta(n-k)]/n$, which proves the interval rule and allows exact product comparisons. For example, $(1,4)$ with both rates one gives exactly $[1,4]$ for $b$. [Tie outcomes](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)

## Exhaustion of the boundary

**Proved.** On the log axis, the density height is at most $m=\min(\alpha,\beta)$. If either rate tends to zero the likelihood tends to zero at every sample, even with a moving kink. If $c$ diverges and the rates stay bounded away from zero, the reduced log likelihood tends to $-\infty$. For instance, above the sample it is bounded by $\log\alpha-\alpha(c-\bar z)$, whose supremum in $\alpha>0$ is $-1-\log(c-\bar z)$. Below the sample the bound uses $\beta$ and $\bar z-c$. When both rates diverge on a nonconstant sample with log range $d>0$, $L+R\ge d/n$ by the two extreme observations. Then $\lambda\le\log m-md/n\to-\infty$. On a constant sample, both free rates can diverge at the common kink exactly when the mask admits that kink, giving the unbounded likelihood.

For a bounded moving kink, pass to a subsequence with $c\to c_*$. If $\alpha\to\infty$ and $\beta\to\beta_*>0$ finite, the only potentially competitive case is $c_*\le\min_i z_i$. The limsup of $\lambda$ is at most $\log\beta_*-\beta_*R(c_*)$. Taking $c=c_*$ reaches this bound. At observations tied to $c_*$, approaching from above may produce an extra penalty from $\alpha(c-c_*)_+$, so the maximizing boundary convention includes the endpoint. This is Pareto with lower endpoint $e^{c_*}$ and shape $\beta_*$. The reflected proof gives the power model. With free endpoint, the Pareto value is strictly improved by raising $c_*$ to $\min_i z_i$ if it is lower, and the power value by lowering its endpoint to $\max_i z_i$ if it is higher. This improvement holds with the surviving rate fixed at any positive value, so it also holds at an optimized positive rate. The constant sample with one held rate is included. These bounds exhaust all escaping parameter subsequences that can compete with a finite likelihood.

For the proper weak closure on $(0,\infty)$, the log variable has law $c+E_\beta-E_\alpha$, with independent exponentials of the indicated rates. This follows by integrating their joint density over either sign of the difference. Tightness on the log line requires rates bounded away from zero, since otherwise the mass of any fixed bounded interval is at most its length times $m\to0$. With both rates bounded away from zero, the offsets are tight, so a diverging kink also prevents tightness. The proper boundary limits are the two one-sided laws and, when both rates diverge, a point mass at the limiting kink. The latter is excluded from the density parameter domain. The one-sided limits with one finite rate also converge in total variation by the fixed-parameter density limit in TASK-235. [Earlier density limits](2026-11-24-dblpareto-report.md#probability-routines-and-closure) · [R7 and R7a](../../roadmap/2026-10-29-parameter-rules.md#rules)

## All-free comparisons and exact ties

**Proved endpoint ordering.** The two face costs are $Q_{\rm Pareto}=\bar z-z_{\min}$ and $Q_{\rm power}=z_{\max}-\bar z$. The sign of the first minus the second is the sign of
$$
\prod_i x_i^2-(x_{\min}x_{\max})^n.
$$
This can be decided exactly on dyadic inputs with integer products and exponents. With precisely two distinct levels and $k$ copies of the lesser one, the costs are $(n-k)d/n$ and $kd/n$. Thus the Pareto face wins for $2k>n$, the power face wins for $2k<n$, and both win for $2k=n$, always with finite attainment excluded. Even with more knots both faces can win without reflection. For the sample with exponents $(-4,-2,1,4)$ in base 2 and counts $(1,1,2,1)$, the endpoint costs in units of $\log 2$ are both $4$, and the finite costs are $(14+4\sqrt6)/5$ and $(11+4\sqrt6)/5$, both greater than $4$.

**Proved three-level rule.** For three distinct observations, let $a,b'>0$ be the successive log gaps and $p,r>0$ the fractions at the first and last levels. The middle knot has $L=pa$ and $R=rb'$. Comparing it with the lower endpoint, its cost is no greater precisely when $(1-2p)a\ge2\sqrt{prab'}$. The reflected comparison uses $(1-2r)b'$. Thus the middle knot is the unique finite MLE exactly when
$$
p<\tfrac12,\quad r<\tfrac12,\quad
4pr b'\le(1-2p)^2 a,\qquad 4pr a\le(1-2r)^2 b'.
$$
Equality in either weak inequality records the corresponding face. If the test fails, use the lower-cost endpoint or both. Each signed test is a rational linear combination of log gaps, so clearing denominators turns it into a product comparison. Both inequalities can hold only if $p+r\le1/2$, as seen by multiplying and taking square roots. At $p+r=1/2$ they hold exactly when $b'/a=r/p$, with equality on both sides. The dyadic sample $(1,2,2,4)$ has the unique finite fit $(b,\alpha,\beta)=(2,2/\log 2,2/\log 2)$ at the value of both faces. The TASK-235 sample $(1/2,1,1,1,1,16)$ lies at equality with the Pareto face alone. In the latter example varying the first observation $x$ changes the comparison sign exactly at $x=1/2$: the finite knot beats Pareto for $x<1/2$ and loses for $x>1/2$. Near the crossing the power face stays strictly worse by the second inequality.

**Proved pair comparator and its exact arithmetic scope.** For two knots, including faces, let
$$
A=L_i+R_i-L_j-R_j,\qquad B=L_iR_i,\qquad D=L_jR_j .
$$
The comparison sign is that of $A+2\sqrt B-2\sqrt D$. Swap the knots if $A<0$. If $A=0$ use the sign of $B-D$. If $A>0$ and $B\ge D$, the sign is positive. Otherwise put $F=4D-A^2-4B$. Squaring the positive sum $A+2\sqrt B$ gives a positive comparison when $F<0$. For $F\ge0$, its sign is the sign of $16A^2B-F^2$, by comparing the two nonnegative sides $4A\sqrt B$ and $F$. These branches handle equality with zero distances.

On rational inputs, each distance is a rational linear combination of the logs of rational observation ratios. A multiplicative basis can be obtained by rational elimination on prime exponent vectors, or only on the four vectors for the distances in a pair. With rank one the costs are rational radicals times a common positive log. With rank two the comparison branches use homogeneous polynomials of degrees one, two and four in two rationally independent real logarithms of algebraic numbers. Their ratio is transcendental by Baker's linear independence theorem as stated in the ties report. Evaluating a nonzero homogeneous polynomial at the two logarithms would force this ratio algebraic if the value were zero. Hence polynomial identity checks decide exact zeros and certified sign refinement terminates on nonidentities. This settles every pair of rank at most two, including any pair from at most three distinct observations. At larger rank a polynomial identity is still sufficient to establish that branch's zero. Schanuel's conjecture would make identity testing exhaustive for zeros by giving algebraic independence of the basis logarithms. [Arithmetic tools](2026-11-25-ties-math-report.md#what-the-transcendence-results-can-decide)

### Complete finite ties of arbitrary size

**Proved construction.** Work in units of $\log 2$, so that an integer log observation $z$ represents $2^z$. Choose $M>0$ rational, distinct rational inner knots in $(-M,M)$, and rational outer knots $-D_-$ and $D_+$ with $D_-,D_+>M$. At the inner knots prescribe the values
$$
G(c)=\frac{M^2+c^2}{2M}
$$
for the mean absolute distance. Join them by chords. Connect the extreme inner knots to the outer points on the graph of $|c|$ and extend on the tails as $|c|$. This is convex with strict increases at each knot. For the right join from the rightmost inner knot $v$, its slope is less than $1$ because $G(v)>v$. At $D_+=M$ the slope would be $(M+v)/(2M)>v/M$. Increasing $D_+$ increases that slope, so the right join exceeds every interior chord slope adjacent to $v$. Reflection proves the left join. This also bounds the left and right join slopes on opposite sides of $v/M$ if there is only one inner knot.

Assign mass equal to half each slope jump. The masses are positive rational and sum to one, since the tail slopes are $-1,1$. The resulting empirical mean absolute distance has the same slopes. Its two tail intercepts are opposite, while the prescribed polygon has zero intercept on both tails. The common additive difference is therefore zero and the empirical log mean is zero. Clear denominators to obtain integer counts. At every inner knot $L+R=G(c)$ and $L-R=c$, so $L=(M+c)^2/(4M)$, $R=(M-c)^2/(4M)$ and $Q=M$ in these log units. At the endpoints the costs are $D_-$ and $D_+$. The earlier exhaustive profiling proof now makes the inner list the complete maximizer set. The public rates at such a knot are $2/[(M+c)\log2]$ and $2/[(M-c)\log2]$.

| Integer exponents of the observations in base 2 | Counts | Complete set of maximizing exponents |
|---|---|---|
| $-5,-1,2,6$ | $9,27,24,4$ | $-1,2$ with no reciprocal reflection |
| $-5,-2,-1,0,1,2,5$ | $4,11,6,6,6,11,4$ | $-2,-1,0,1,2$ |
| $-20,-6,-5,\ldots,5,6,20$ | $50,97$, then $14$ per exponent from $-5$ through $5$, then $97,50$ | All 13 exponents from $-6$ through $6$ |

The first two rows use $M=4$ and the last uses $M=16$. The third has 448 observations. All observation values in the table are exactly representable in binary64. Using an arbitrary finite inner list gives unbounded cardinality over the mathematical domain. The 13-point example exceeds the capacity of the three existing `Points` pieces even if all four slots per piece are used. [Exact sample checks](2026-11-25-dblpareto-math-data/check-profiles.py) · [Set representation](../../../univariateML-cpp/include/cppunivariate/core/maximizer_set.hpp)

## One held rate

**Proved crossing rule.** Hold the left rate at $h>0$, let $t(c)=R(c)$, $u(c)=L(c)$, and put $s=ht$. The profile loss measured from $\log h$ is
$$
H_h(c)=h u(c)+\Phi(ht(c)),\qquad
\Phi(s)=\log(1+y)+\frac{y}{1+y},\qquad
s=\frac{y^2}{1+y},\quad y\ge0 .
$$
Here $y=h/\hat\beta$. The value at $s=0$ is the power face. Differentiating gives $\Phi'(s)=1/y(s)$ for $s>0$, a strictly decreasing positive function. For ordered distinct knots $c_i<c_j$ on a nonconstant sample, $\Delta u=u_j-u_i>0$ and $t_i>t_j\ge0$. Therefore
$$
\frac{H_h(c_j)-H_h(c_i)}h
   =\Delta u-\int_{t_j}^{t_i}\Phi'(h v)\,dv .
$$
The integral is strictly decreasing and continuous in $h$. It tends to infinity as $h\downarrow0$ by restriction to any positive subinterval. It tends to zero as $h\to\infty$ by dominated convergence, since $\Phi'(h_0 v)$ is integrable at zero for fixed $h_0>0$ by its $v^{-1/2}$ behavior. Each ordered pair thus has exactly one positive crossing $h_{ij}$. The higher kink wins that pair below $h_{ij}$, they tie at it, and the lower kink wins above it. The global set of winners moves toward lower knots as $h$ increases. If $f$ denotes the face at the largest knot, let $h_* = \min_{i<f}h_{if}$. Below $h_*$ the face wins strictly and there is no finite MLE. At $h_*$ return the finite knots whose crossings attain that minimum, and record the tied face. Above $h_*$ the face loses and comparing finite costs returns all maximizing knots. One may evaluate the cost differences directly at the supplied $h$. Computing the crossing values is optional. Holding the right rate instead exchanges distances and reverses the knot order, with a Pareto face.

**Proved real crossings on exact dyadic samples.** For $(1,2)$ with the left rate held, put $d=\log 2/2$. The equality is $hd=\Phi(hd)$, with a unique positive solution by the pair theorem. At equality there is one finite MLE with kink $1$, and its value is shared by the power face at $2$. For $(1,2^{[100]},4)$, where the superscript denotes multiplicity, the best two finite knots are $1$ and $2$ at their unique crossing. To prove globality, take $h_0=1/\log 2$. The costs at $1$ and $4$ are $\Phi(1)>\log 2>1/2$ and $1$. At $2$ the cost is $1/102+\Phi(1/102)<1/102+1/4<1/2$, since $y<1/8$ there and $\Phi(s)<2y$ for $s>0$. Its crossing with kink $1$ thus occurs above $h_0$. At this crossing the power face at $4$ still loses to kink $2$ by the pair rule. The equality gives exactly two separated finite maximizers. Both examples specify the held coefficient by an exact real equation. Rounding the hold need not preserve equality.

**Conjecture-dependent exclusion.** Schanuel's conjecture excludes one-held comparisons tying at an algebraic positive $h$ on positive rational data. Let $z$ be the vector of logarithms of a multiplicative basis and $K=\overline{\mathbb Q}(z)$. The entries of $z$ would be algebraically independent. The numbers $y_i,y_j$ are algebraic over $K$, including $y_j=0$ at a face. Equality of costs gives $w=\log[(1+y_i)/(1+y_j)]$ algebraic over $K$. Since $e^w$ is algebraic over $K$ and the $e^{z_k}$ are algebraic, Schanuel forces $w$ into the rational span of $z$. This is impossible. An equality of the algebraic expressions for $w$ and a linear form in $z$ at algebraically independent inputs must hold identically on the analytic branch there. Otherwise its nonzero minimal polynomial has nonzero constant coefficient over the rational function field, and clearing denominators would give a nonzero polynomial vanishing at $z$. On the ray $z\mapsto\epsilon z$, $\epsilon>0$, the positive-root formulas continue analytically as the nonzero distances stay positive. Along this ray $w=(\sqrt{ht_i}-\sqrt{ht_j})\sqrt\epsilon+O(\epsilon)$, with nonzero leading coefficient because $t_i>t_j$. A linear form is $O(\epsilon)$. The unconditional finite-cover and crossing results require no arithmetic conjecture. [Schanuel formulation](2026-11-25-ties-math-report.md#what-the-transcendence-results-can-decide)

## Coefficient and comparison checks

**Numerical.** Both final Python commands exited zero on the workstation through the single-slot runner. The profile script uses exact rational signs and verifies them against direct 80-digit radical costs for all 121 nonconstant size-five log-lattice samples on exponents $0$ through $4$, including 60 three-level samples. It checks the exact fixtures and replays the crossing neighbors at 90 and 130 decimal digits. The finite/face held rate is approximately $5.99194241734625155549$. The finite/finite held rate is approximately $653.299290255926098622$, where the power cost is higher by about $445.715$ in mean log-likelihood units.

| Signed mean log-likelihood gap | Lower binary64 input and gap | Upper binary64 input and gap |
|---|---|---|
| Free rates, finite minus Pareto, vary the first observation of $(x,1,1,1,1,16)$ | `0x1.fffffffffffffp-2`, $+3.5594\times10^{-17}$ | `0x1.0000000000001p-1`, $-7.1187\times10^{-17}$ |
| Held left rate, finite minus power on $(1,2)$ | `0x1.7f7bfc0c811bfp+2`, $-1.7528\times10^{-16}$ | `0x1.7f7bfc0c811c0p+2`, $+2.3169\times10^{-17}$ |
| Held left rate, kink 1 minus kink 2 on $(1,2^{[100]},4)$ | `0x1.46a64f24a29b6p+9`, $-3.0004\times10^{-16}$ | `0x1.46a64f24a29b7p+9`, $+4.4520\times10^{-16}$ |

On the held-rate pair $(1,2)$ the Python replay of the binary64 loss coordinates puts both inputs within the fitter's margin. The finite branch is a strict local maximum near the crossing even when the face wins. Its right derivative in log kink at $1$ is $(\hat\beta-h)/2<0$ since the crossing has $s>1/2$, and its left derivative is positive. The inequality for $s$ follows from $\Phi(1/2)=\log 2+1/2$. The current source prefers the finite witness in an unseparated finite/face comparison. These replays distinguish the fallback from a proof of global attainment. [Profile and crossing checks](2026-11-25-dblpareto-math-data/check-profiles.py)

The coefficient script reads all 28 Double Pareto records in each stored base snapshot mode. Each mode contains four finite fits and 16 limit outputs with public coefficients, plus two unbounded and six invalid rows. The exact binary64 inputs and serialized coefficients are used at 100 digits. Every reported kink equals an observation exactly. The largest relative rate discrepancies are $2.71\times10^{-16}$ in native mode and $4.01\times10^{-16}$ in double-only mode, including surviving rates in the face payloads. The largest scaled likelihood discrepancy at the serialized coefficients is $2.61\times10^{-15}$ in both modes. A separate grid reproduces the binary64 distance and rate formulas in Python with compensated summation via `math.fsum`. It covers raw and adjacent log gaps, exact ties, held kinks away from observations, adjacent doubles at three scales, the full positive binary64 span, and held rates from the smallest subnormal to the largest finite value. The largest relative discrepancy in 704 checks of finite free rates is $3.47\times10^{-16}$. The grid checks conditional coefficients, while the archived results supply actual public coefficients. [Coefficient checks](2026-11-25-dblpareto-math-data/check-coefficients.py) · [Native provenance](2026-11-15-fit-snapshot-data/native.provenance.json) · [Double-only provenance](2026-11-15-fit-snapshot-data/double.provenance.json)

**Proved conditioning at a selected kink.** A free optimal kink is a raw observation, or a raw endpoint of a flat interval, so there is no log-to-physical reconstruction or refit at a rounded kink. For two free rates, relative perturbations of at most $\eta<1$ in the two positive distances perturb each inverse rate by at most $\eta$, by homogeneity and monotonicity of its expression in the distances. For one free rate, $d\log r/d\log t=-(r+h)/(2r+h)$ has absolute value at most one. This separates gap and rate rounding from the global height comparison. Mathematical attainment also permits representational failures in the likelihood. For example, all holds on $x=(1)$ with $b=8$, $\alpha$ the largest binary64 number and $\beta=1$ give a real finite log likelihood below the binary64 range because $\log 8>1$. The current density evaluation then gives `Evaluation`. [Accuracy contract](../../design/accuracy-contract.md) · [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/mle.hpp)

Run the checks sequentially from the worktree root.

```sh
d=dev/research/evaluations/2026-11-25-dblpareto-math-data
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export PYTHONDONTWRITEBYTECODE=1
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run python3 -u "$d/check-profiles.py"
/tmp/umvl-cpu/run python3 -u "$d/check-coefficients.py"
```

## What this changes for the fitter

- Use the direct sample range predicates on held-kink routes and the exact weighted-count sign on the two-held-rate route. Preserve `Unbounded` for constant data with both rates free and the kink admissible at the common value. Preserve the surviving held coefficient and endpoint in one-sided payloads. On two-level all-free samples the count comparison suffices. On any all-free sample an exact product sign orders the two faces if required.
- Use the three-level inequalities and the rank-one or rank-two comparator to establish exact ties within their scope. A minimizing finite knot that ties only excluded faces is a certified singleton. Two strictly winning tied faces mean certified nonattainment with both named witnesses in diagnostics. A strict single face winner gives `ParetoLimit` or `PowerLimit` with the corresponding endpoint and fitted or held shape.
- Return certified `Ok` with the complete set when all finite winners are proved and their public coefficients pass the accuracy check. The weighted-quantile interval fits the existing box. Lists of isolated points need capacity beyond the current fixed arrays. Reciprocal reflection remains a sound sufficient tie test once the other knots and faces have been excluded. Reflection by itself cannot establish completeness on the five- or 13-knot examples.
- When one rate is held, evaluate the exact loss criterion at the supplied hold, with sign refinement for close values. A proved weakly dominating finite witness with unresolved finite cardinality can be certified with `unique = NA` under the ties rule. If only overlapping heights remain, retain the successful checked finite candidate with `certified = FALSE, unique = NA` and the competing bounds. Withdraw that candidate when a proved strict face comparison refutes it. A comparison margin alone proves neither finite attainment nor equality of distant coefficients.
- Check rate enclosures at the exact physical kink and transfer their error to the published binary64 rates. Use the distance sensitivity bounds to allocate the coefficient error budget independently from the height certificate. An accuracy or likelihood representation failure needs a numerical reason, distinct from proved nonexistence. [Tie outcomes](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)

## Open

- General all-free equality beyond the rank-two and polynomial-identity tests needs a proof of nonvanishing for the relevant homogeneous log polynomials or a new exact equality mechanism. Schanuel would suffice for identity completeness. A practical exact comparison path also needs measured work bounds for integer products, basis extraction and sign refinement.
- For one held rate, the real crossing construction and conjecture-dependent exclusion leave unconditional equality on jointly binary64 data and holds open. A dyadic crossing witness or an unconditional obstruction to the displayed algebraic-function/log identity would settle it. Absent an applicable equality proof, adjacent-input signs and a successful fallback do not settle the exact crossing.
- The numerical checks sample the current formulas. Uniform platform bounds for accumulated log gaps and profile comparison margins, and a public complete-set representation for arbitrary isolated winners, remain implementation work.
