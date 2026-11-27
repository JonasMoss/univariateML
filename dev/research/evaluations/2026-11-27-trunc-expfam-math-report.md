# Maximum likelihood under truncation for exponential families

TASK-439, revision 4a4ba7de. Windows are closed and intersected with the parent support. The parent coefficient domain is used unless an extension is declared. Bars denote sample averages.

## General theorem

**Proved.** Let $\nu$ be the image by $T$ of the retained carrier $1_Ah\,d\mu$. Then
$$Z_A(\eta)=\int e^{\eta\cdot t}\,d\nu(t),\quad N_A=\{\eta:Z_A(\eta)<\infty\},\qquad
\ell(\eta)=\eta\cdot\bar T-\log Z_A(\eta)$$
is the log likelihood per observation up to a carrier term. Work with positive finite observation carrier and $T_i$ in the closed convex support $K$ of $\nu$. The normalizer is positive since the event has positive mass at one, hence every, finite parent parameter. Restriction only decreases the integral, so $N_A\supseteq N$. Bounded $T$ on $A$ makes $N_A$ the full space by comparison with the integral at any feasible point.

Hölder proves convexity of the log normalizer, including its finite boundary. Equality along a nontrivial segment holds exactly when the parameter difference is orthogonal to $\operatorname{span}(K-K)$, also the condition for equality of laws by their density ratio. Quotient by these directions to get strict concavity. In the interior, exponential integrability on a neighborhood justifies differentiation and gives $\nabla\ell=\bar T-E_\eta T$, $\nabla^2\ell=-\operatorname{Cov}_\eta T$. A singleton count has no information. Two retained counts admit only one identifiable contrast, their log odds. If both occur the unrestricted finite tilt matches the empirical proportions, and restrictions clip the attainable odds range.

If $\bar T\in\operatorname{ri}K$, $\ell\to-\infty$ uniformly at infinity in the identified coordinates. For each unit direction $u$, choose a bounded positive-carrier set where $u\cdot(T-\bar T)>2\epsilon_u>0$. The same lower bound with $\epsilon_u$ works on a neighborhood of $u$. A finite cover of the sphere gives common positive margins and carrier-mass bounds. Integration gives coercivity. Fatou gives upper semicontinuity of the extended $\ell$, with value $-\infty$ at divergent integrals. Thus a maximizing subsequence converges and the unique finite maximizer belongs to the **full effective domain**, including finite integrable boundary points. For an open $N_A$ the optimum solves the moment equation. Over the interior only, existence means $\bar T\in\nabla\log Z_A(\operatorname{int}N_A)$, which can be a proper subset of $\operatorname{ri}K$.

For $\bar T$ on a proper supporting hyperplane, let $u\cdot(T-\bar T)\le0$ expose the face $F$ at equality. All observations lie on $F$. Tilting any feasible point by $su$, $s>0$, strictly improves the likelihood by decreasing the integral centered at $\bar T$. If $F$ has positive carrier mass, dominated convergence gives total variation convergence to the conditional tilt on $F$. Dropping the off-face mass bounds each full likelihood above by its face likelihood, and the ray recovers that bound. Therefore the full supremum equals the face supremum; the argument can repeat on a smaller face. With zero carrier mass on $F$, the centered integral tends to zero and the likelihood diverges. A weak limit requires a separate argument. A scalar continuous tilt toward a finite endpoint, for example, can collapse to a weak atom. This proves the relative-interior iff over the full effective domain. For the full-family recession framework see [Geyer, Likelihood Inference in Exponential Families and Directions of Recession](https://arxiv.org/abs/0901.0455).

**Exact counterexample over an open parameter domain.** Use carrier $k^{-3}$ on positive integers, parent tilt $\eta<0$, retain the even integers, and observe $(4)$. Then $N_A=(-\infty,0]$. The boundary mean is $2\zeta(2)/\zeta(3)<4$, since $\zeta(2)<2$ by the integral bound and $\zeta(3)>1$. The score is strictly positive throughout $\eta<0$ by monotonicity of the model mean. Although 4 lies in $(2,\infty)$, only the finite integrable boundary law maximizes.

### Convex parent constraints

**Proved.** In identified coordinates let $C$ be convex and $D=\overline C$. A finite point of $D\cap N_A$ is approachable along a segment from a relative-interior point of $C$. Convexity bounds the log normalizer above along the segment and Fatou bounds the limit below. Integrating the minimum of the two densities gives total variation convergence by dominated convergence. For interior data, coercivity gives a unique solution on $D$ and attainment on $C$ iff that point belongs to $C$. At a differentiable finite candidate the necessary and sufficient test is
$$(\bar T-E_\eta T)\cdot(\xi-\eta)\le0\quad(\xi\in D).$$
More generally use nonpositive feasible directional derivatives. Necessity follows by following a segment and sufficiency by concavity. For full-dimensional $C$ any interior optimum also optimizes over $N_A$. With holds one must optimize in their affine span. For instance normal data $(0,2)$ with mean held at zero fit variance 2 in the relative interior of the held ray although the full optimizer $(1,1)$ in mean/variance coordinates is outside it.

A sequence escaping to infinity has a subsequence with normalized direction $u$. Convex combinations with any $z\in D$, at weights $s/\|\eta_j\|$, show $z+su\in D$ by taking limits. The same proof in each nonempty upper likelihood level below the supremum gives a likelihood recession direction. Positive carrier where $u\cdot(T-\bar T)>0$ would contradict the bounded-set integral argument for that sequence. Thus in identified space such directions expose supporting faces. They specify leading tilts, while subleading tilts may still vary on the exposed faces. Finite boundary likelihood and convergence follow from the segment argument; a limit at infinity requires a concentration or conditioning proof and compatibility of the joint sample likelihood. See the [likelihood envelope and closure lemma](../ml-behaviour.tex).

### Scalar and two-coordinate checks

**Proved.** For a scalar tilt $\theta T$, the mean $E_\theta T$ increases strictly unless $T$ is constant. At an infinite parameter end it tends to the corresponding essential extremum, possibly infinite. Compare any two separated statistic levels using their exponential weights and bound the normalizer below on a positive-mass compact subset of the favored level. The unfavored tail first moment is exponentially small relative to that lower bound, using exponential integrability at an interior tilt. The unique root exists exactly between the endpoint mean thresholds. At and beyond a finite threshold the corresponding finite edge wins if its integral is finite. If its integral diverges the likelihood tends to zero on the ordinary scale. At an infinite tilt, equality with the extremum requires every observation to be an extremizer. A discrete endpoint atom then gives likelihood one, and a unique continuous endpoint extremizer gives unbounded likelihood and weak collapse by the preceding face and separated-level arguments.

**Proved.** For each two-coordinate continuous parent below the nonconstant-data likelihood is coercive on the parent-domain closure even if the unrestricted convex-support argument fails. On the admissible unit recession directions the leading exponent is a quadratic with nonpositive quadratic coefficient (normal coordinates), $u\log x-vx$ (gamma), $u\log x+v\log(1-x)$ (beta), or $-ux-v/x$ (inverse Gaussian), with $u,v\ge0$ in the last three cases. Each has at most one maximizing point, by differentiating. Distinct observations cannot all maximize, so some positive-carrier interval has exponent strictly above its empirical average. The finite-cover coercivity proof applies to these directions. Divergent finite edges have extended likelihood $-\infty$. At finite integrable edges, use the unique tangential root and check that the missing inward score is $\le0$. At a corner both inward scores must be $\le0$. The missing-score derivative follows directly from the one-coordinate integrals: a negative unbounded statistic gives the derivative by monotone convergence, and a positive log tail on the gamma shape-zero edge has exponential damping from positive rate. At a gamma corner the interval is compact away from zero. These tests are necessary by coordinate perturbations and sufficient by concavity (or the tangent inequality from Jensen). If no edge passes, the unique maximum is interior.

For constant $c$ the two-free-coordinate cases have unbounded likelihood and weak collapse. Scale a feasible leading tilt with a strict peak at $c$: $-(x-c)^2$, its log-coordinate counterpart, $\log x-x/c$, $c\log x+(1-c)\log(1-x)$, or $-x-c^2/x$, respectively for normal, lognormal, gamma, beta and inverse Gaussian. The zero-carrier face proof gives divergence, including at an included cut. Outside each neighborhood of $c$ there is a strict gap in these tilts, and the separated-level proof gives weak collapse.

## Count windows

Write $L,U$ for retained endpoints, using inward rounding and capping $U$ at a held binomial size. A singleton gives likelihood one for every compatible parameter, hence a flat fit whenever more than one parameter is compatible. Empty events are invalid, and all-held fits evaluate the specified law. Ordinary parent atoms are eligible exactly when they belong to the window. The special positive-window extensions are stated separately.

### pois free truncate:known

**Proved.** Weights $\lambda^k/k!$, natural tilt $\log\lambda$. A unique positive rate exists iff $L<\bar x<U$. The lower endpoint selects the excluded atom at $L$ unless $L=0$ or the declared $[1,\infty)$ extension applies, in which cases zero rate attains. The finite upper endpoint gives an excluded atom as rate tends to infinity. These endpoints follow by dominance of the first or last term and, on unbounded windows, the scalar infinite-tilt argument. The implemented point-mass statuses agree outside the extensions.

### geom free truncate:known

**Proved.** Weights $q^k$, $q=1-p$. The unique interior root requires $L<\bar x<M_0$, where $M_0=(L+U)/2$ on a finite window and $M_0=\infty$ otherwise. Finite sums and the shifted geometric mean $L+q/(1-q)$ give the thresholds. At $L$ the atom attains for $L=0$ or the positive-window extension, and is excluded otherwise. At or above finite $M_0$ the discrete uniform is excluded ($p\downarrow0$). The documented lower and upper statuses agree.

### binom size truncate:known

**Proved.** At fixed size $N$ use weights $\binom Nk[p/(1-p)]^k$. The unique interior root requires $L<\bar x<U$. At either endpoint there is an atom, attained at $p=0$ if $L=0$ or the positive-window extension applies, or at $p=1$ if $U=N$. The remaining atoms are excluded because the parent event has zero mass at that coefficient. The scalar check proves these conditions and the documented distinction agrees.

### binom free truncate:known

**Proved using prior results.** For nonconstant data, each $N\ge M=\max_i x_i$ has just one probability matching the conditional mean. Compare those profiles with the mean-matched Poisson on the same window. The [finite-prefix and analytic-tail theorem](2026-11-25-tbinom-math-report.md#a-finite-decision-certificate) gives a finite decision for bounded or positive lower-only windows. For the full untruncated window use the ordinary-binomial dispersion decision in that report. A winning finite profile attains, including a tie with the face; a strictly winning Poisson face escapes. The exact maximizer set pairs every winning size with its matching probability. Multiple local peaks occur for frequencies $(2358,0,2927,4714,1)$ on $[0,4]$. The sample $(1,1,2)$ on $[1,2]$ has every $N\ge2$ winning at $p=1/N$, also tying the Poisson face. Its conditional odds of 2 against 1 equal $1/2$ for every such pair, attaining the empirical multinomial bound. [Prior examples and exact ties](2026-11-25-tbinom2-math-report.md#exact-size-ties).

For constant $c$, $N=c,p=1$ attains one, and the entire set is the union over sizes of compatible singleton and atom fits from the held-size section. With $0<p<1$ held, on a bounded window increasing size drives successive retained mass ratios to infinity, so the top atom wins without attainment on a nonsingleton window if all observations equal $U$. For all other data the likelihood tends to zero with size: on a bounded window some observation lies below $U$, and on an unbounded window the binomial mass at every fixed count tends to zero while conditioning probability tends to one. A positive finite witness then bounds the winning sizes. Held endpoint probabilities reduce to compatible atoms or extensions. Implemented named Poisson and atom statuses describe these cases, with narrower finite-search certification than the mathematical prefix decision.

### nbinom size truncate:known

**Proved.** For $r>0$ held, weights $(r)_kq^k/k!$ give a unique interior root iff $L<\bar x<M_0$. On a finite window $M_0$ is the mean under normalized weights $(r)_k/k!$; otherwise $M_0=\infty$ since these weights have divergent sum even after deletion of a finite prefix, by the binomial series. Lower atom conventions agree with geometric. At and above finite $M_0$ the excluded weighted law replaces the uniform. At $r=0$ the ordinary parent is the zero atom; the positive-window alias allows the logarithmic law. The documented LowerLimit and UpperLimit agree for the stated fixed-positive-size scope. General free-size windows are refused outside that alias.

### zeta free truncate:known

**Proved.** For $[1,N]$ with $N<\infty$ use tilt $-s$ on $\log k$ with $s\in\mathbb R$. The unique finite root requires $0<\overline{\log x}<\log N$, otherwise an infinite-exponent atom wins at the corresponding endpoint. $s=0$ is attained at the uniform log mean. $N=1$ makes every exponent optimal. This agrees with the implemented signed domain and endpoint limits.

## Continuous windows

Use the effective interval $[a,b]$, positive width, and admissible sample points. Open parent endpoints are excluded. Rayleigh assigns zero density at its closed endpoint zero, so data including zero have zero likelihood for all coefficients and supply no positive-likelihood estimate. This differs from an endpoint with diverging density under collapse.

### norm free truncate:known

**Proved.** Use $(\mu/\sigma^2,-1/(2\sigma^2))$. With nonconstant data fit the integrable zero-quadratic edge by matching $\bar x$ with the linear tilt. The inward score is $E_{\rm edge}X^2-\overline{x^2}$. Attainment is exactly $s_x^2<\operatorname{Var}_{\rm edge}X$. At or above equality the excluded edge is the signed truncated exponential (uniform at zero tilt), the translated exponential on $[a,\infty)$ with mean distance $\bar x-a$, or its reflection on $(-\infty,b]$. On the full line there is no finite integrable edge and nonconstant data attain. Constants collapse. [Earlier face payloads](2026-11-21-tnorm-limit-report.md#coefficient-rule-and-comparison).

### norm mean truncate:known

**Proved.** With $\mu$ held use $T=(x-\mu)^2$ with negative tilt. The unique root requires $\inf T<\bar T<E_{\rm unif}T$ for bounded windows. The lower equality collapses at the projection of $\mu$ onto the interval. At or above the upper threshold the excluded uniform wins. For an unbounded window the upper threshold is infinite: the zero-tilt normalization diverges and the normalized mass of every bounded region vanishes.

### norm sd truncate:known

**Proved.** At held $\sigma$ tilt $x$ against the fixed Gaussian carrier by $\mu/\sigma^2$. The unique root requires $a<\bar x<b$. At either finite endpoint equality means all observations are at that end and location escapes to give unbounded likelihood. Interior constants fit.

### exp free truncate:known

**Proved.** For $Y=X-a$ use negative rate tilt. The unique root requires $0<\bar Y<(b-a)/2$ when $b$ is finite and $\bar Y>0$ otherwise. Equal zero gives lower collapse. At or above the finite upper threshold the uniform wins at excluded zero rate. On an unbounded window the rate is $1/\bar Y$.

### gamma shape truncate:known

**Proved.** For held shape $k$, use negative rate tilt on $x$ with carrier $x^{k-1}dx$ (rate is inverse repository scale). The unique root requires $a<\bar x<M_0$ with $M_0$ the mean under this carrier on finite windows and infinity otherwise. At equality with an admissible $a$ there is collapse; at or above finite $M_0$ the excluded power density wins.

### gamma scale truncate:known

**Proved.** For held rate $v>0$, tilt $\log x$ by $k>0$ against $x^{-1}e^{-vx}dx$. The lower threshold is its normalized log mean if $a>0$, and $-\infty$ otherwise since mass accumulates at zero. The upper threshold is $\log b$ or infinity. The unique root requires strict inequalities. At or below the lower threshold the excluded carrier law wins; at the finite upper threshold there is collapse.

### gamma free truncate:known

**Proved.** With $k,v>0$ use $(\log x,-x)$ against $dx/x$. For nonconstant data test the following finite edges by the two-coordinate check.

- $k=0,v>0$, eligible for $a>0$. Match $\bar x$ in $x^{-1}e^{-vx}$ using the scalar rule, and require $\overline{\log x}\le E\log X$.
- $v=0,k>0$, eligible for $b<\infty$. Match $\overline{\log x}$ in $x^{k-1}$, and require $\bar x\ge EX$.
- $(0,0)$, eligible for $0<a<b<\infty$. The log-uniform law wins iff $\overline{\log x}\le E\log X$ and $\bar x\ge EX$ there.

A winning edge is excluded. Otherwise a unique interior fit exists. Constants collapse. The exact sample $(1,4)$ on $[1,4]$ selects the corner because the log mean agrees and $3/\log4<5/2$, using $\log 2>2/3$ by strict Jensen on $1/x$ over $[1,2]$.

### beta free truncate:known

**Proved.** The shape coordinates multiply $(\log x,\log(1-x))$ against $dx/[x(1-x)]$. For nonconstant data the shape-one-zero edge requires $a>0$, and shape-two-zero requires $b<1$. Match the empirical log statistic of the still-positive shape, and require the empirical mean of the missing statistic to be at most its edge expectation. The densities are proportional to $x^{-1}(1-x)^{\beta-1}$ and $x^{\alpha-1}(1-x)^{-1}$. The corner with both shapes zero requires both ends cut and both inward scores nonpositive. These are the excluded faces. If none wins there is a unique interior root; constants collapse. With one positive shape held, use the scalar rule on the free log statistic. Its finite lower threshold exists precisely when the corresponding singular endpoint is cut, giving the same excluded edge. Uncut, mass accumulates at the singular end and the log mean tends to $-\infty$. At equality with the upper threshold there is collapse at the favored endpoint. Finite roots are unique.

### lnorm free truncate:known

**Proved.** Set $Y=\log X$. The Jacobian is coefficient-independent. All three normal cases apply on the log interval, also for held meanlog or sdlog. The linear-tilt faces transform to power densities $x^{r-1}$ on the appropriate window, including log-uniform when $r=0$. Collapse locations transform back by exponentiation. Attainment and uniqueness carry over.

### invgauss free truncate:known

**Proved.** Use $(u,v)=(\lambda/(2\mu^2),\lambda/2)>0$, statistics $(-x,-1/x)$ and carrier $x^{-3/2}dx$. The nonconstant-data face tests are:

- $u=0,v>0$. Match $\overline{1/x}$ in the truncated Lévy density $x^{-3/2}e^{-v/x}$ and require $\bar x\ge EX$. If $b=\infty$ its infinite $X$ moment rules it out.
- $v=0,u>0$, eligible iff $a>0$. Match $\bar x$ in $x^{-3/2}e^{-ux}$ and require $\overline{1/x}\ge E(1/X)$.
- $(0,0)$, eligible iff $a>0$. Use $x^{-3/2}$ and require both empirical means at least their respective expectations. Infinite $b$ again rules it out.

A winning edge is excluded. Otherwise there is a unique interior fit; constants collapse. With shape held, vary $u$ and require $a<\bar x<E_{u=0}X$, with lower collapse and Lévy boundary as failures. With mean held, the scalar statistic is $T=x/\mu^2+1/x$ with tilt $-v$. Require $\inf T<\bar T<\lim_{v\downarrow0}E_vT$. The lower failure collapses at the projected mean, and the upper failure is the corner carrier on a window cut at both ends. For $a=0$ mass accumulates at zero and the reciprocal moment diverges; for $a>0,b=\infty$ the carrier is integrable but the $X$ moment diverges.

### weibull shape truncate:known

**Proved.** With $k>0$ held, $Y=X^k$ is exponential of rate $\mathrm{scale}^{-k}$. Apply the exp criterion to $[a^k,b^k]$ and $\overline{x^k}$. Failures are lower collapse and the finite-window excluded power density proportional to $x^{k-1}$. The root maps bijectively.

### pareto b truncate:known

**Proved.** With parent endpoint $b_0>0$ held (repository coefficient b), $Y=\log(X/b_0)$ is exponential of rate a. Apply the exp criterion with transformed bounds. The failures are lower collapse and the excluded log-uniform law. With shape held and endpoint free, profile the effective lower endpoint to $x_{\min}$ when it is below the upper bound. Every positive parent endpoint inducing that cut gives the same optimum; if the imposed lower cut already equals $x_{\min}$, these parent endpoints give a flat maximum. With shape also free, the same reduction precedes the scalar fit. If the data are constant at the known finite upper cut, and the imposed lower cut is strictly below it, taking the parent endpoint upward gives unbounded likelihood. The endpoint profiling proof is given below.

### rayleigh free truncate:known

**Proved.** $Y=X^2$ is exponential of rate $1/(2\sigma^2)$. Apply the exp criterion over $[a^2,b^2]$. The zero-rate finite-window face has density proportional to $x$. Lower collapse requires a positive sample endpoint under the dossier convention.

### invgamma alpha truncate:known

**Proved.** With $\alpha$ held, $Y=1/X$ is gamma of that shape and rate beta. Reverse the endpoints and use the held-shape gamma criterion for $\overline{1/x}$. The lower collapse in $Y$ is upper collapse in $X$, and the finite-window power face becomes $x^{-\alpha-1}$. The root is unique.

### invweibull shape truncate:known

**Proved.** With $k$ held, $Y=X^{-k}$ is exponential of rate $\mathrm{scale}^k$. Reverse the endpoints and use the exp criterion for $\overline{x^{-k}}$. The atom is at the upper $X$ endpoint and the finite transformed-window zero-rate face has density proportional to $x^{-k-1}$. The root is unique.

## Estimated bounds

### all applicable truncate:estimated

**Proved.** Fix coefficients and retain every observation. The joint conditional likelihood is the product of the parent densities divided by the window probability to the sample-size power. Moving a free lower bound up to the minimum, or a free upper bound down to the maximum, weakly decreases that probability without changing any numerator, including ties at either cut. Hence the tight window attains the bound profile whenever its probability is positive. Joint maximization reduces to the fixed tight window. A global maximizer using a looser window can tie only when the removed part has zero mass under those fitted coefficients. This describes the entire bound maximizing set, including flat rounding intervals for counts, outside-parent-support padding and degenerate atoms. For a continuous positive-density interval strict inequality holds whenever a looser bound adds positive length in the support. The cut observations still have their usual pointwise densities, with no atoms introduced by estimation. All of the fixed-window criteria and failures apply after profiling, with this separate bound multiplicity.

For positive continuous density at the sample point, if both estimated bounds coincide, or one estimated bound meets the other fixed bound at a constant sample, the tight zero-width event is invalid. When feasible positive-width windows shrink to that point, the integral tends to zero at fixed coefficients, and likelihood diverges. The same proof applies to the free Pareto support endpoint after dropping the endpoint-dependent multiplicative factor from numerator and normalizer. If a positive-width compatible window does not exist the data/design pair is invalid. A discrete coincident pair is a singleton and retains positive mass for compatible coefficients, with likelihood one.

## Zero modifiers

Write $n_0$ for the number of zeros, $n_+$ for the number of positives, $z=n_0/(n_0+n_+)$, $f_0$ for parent mass at zero and $L_+$ for log likelihood of the positive sample under its conditional law. All likelihood comparisons in this section use **total** log likelihood. Counts outside support or holds forcing zero probability at an observation are incompatible. If every coefficient is held this amounts to likelihood evaluation.

### Exact factorization and inflation boundary

**Proved.** The positive-truncation law is the fixed window $[1,\infty)$, enlarged only where declared. For modification the joint log likelihood is $B(w)+L_+$, where
$$ B(w)=n_0\log w+n_+\log(1-w),\qquad w=p_0. $$
This follows by multiplying mass $w$ at zero and $(1-w)$ times the conditional parent mass at each positive count. With free $p_0$ the unique weight is $z$, including the endpoints. A held $p_0$ supplies its Bernoulli term. If there are no positives the positive-component coefficients are unidentified over their entire admissible slice, and $p_0=0$ is incompatible. Thus each positive-truncation criterion, maximizer set and named limit transports to modification with the fitted/held Bernoulli weight attached. With positive observations $p_0=1$ is incompatible.

For inflation the same formula holds with $w=f_0+(1-f_0)\pi$ when $f_0<1$. For fixed parent, free $\pi$ profiles to $w=\max(z,f_0)$ by the sign of $B'$. This yields $\pi=(z-f_0)/(1-f_0)$ when $f_0\le z$, and $\pi=0$ otherwise. In particular with no zeros and free inflation, $\pi=0$ and the fit is the ordinary parent on the positive data. In general a candidate with $\pi=0$ must maximize the ordinary parent over the same holds, since that entire slice remains feasible. For $0<f_0<1$ its inward weight score has the sign of $z-f_0$. This sign is necessary for a boundary maximum. Global comparison is still needed for a nonconvex parent profile. With held inflation simply substitute the given $\pi$ in $w$; held zero gives the ordinary parent.

**Proved stronger reduction for fixed count carrier and free inflation.** Poisson, geometric, positive-size NB and known-size binomial have base weights $h_k e^{\eta k}$. Multiplying the zero weight by $e^\delta$ represents inflation exactly for $\delta\ge0$, after normalization. For nondegenerate parents the map is reversible by the preceding $w$ formula. The joint log likelihood is concave in $(\eta,\delta)$, strictly when zero and at least two distinct positive support counts are available. If positive data fit the conditional root and its parent $f_0\le z$, that root with the fitted $w=z$ attains the factorized upper bound. Otherwise the ordinary parent fit with $\pi=0$ wins: if its $f_0<z$, the unconstrained zero tilt would improve locally and the modified root would be admissible, or a positive endpoint would be required. More directly, in the no-endpoint interior the strictly concave objective is maximized at the ordinary root whenever its inward score is $\le0$. The threshold equivalence follows because the ordinary mean is $(1-f_0)E_+X$, both factors increasing with $\eta$. Endpoint cases are spelled out below. For a fixed parent coefficient only the clipped Bernoulli step remains.

### pois free zero:truncated

**Proved.** Positive mean one gives the attained atom at one at $\lambda=0$. Otherwise solve the unique root $\bar x_+=\lambda/(1-e^{-\lambda})$. Holds restrict this result to evaluation, and a held zero rate only supports ones. There is no excluded winning face on compatible positive samples.

### geom free zero:truncated

**Proved.** Conditional weights give the law of one plus a geometric variable. The unique estimate is $p=1/\bar x_+$, including $p=1$ at all ones by extension. There is no excluded winning face. A held probability is evaluated directly.

### binom free zero:truncated

**Proved using the tbinom reports.** The known-size criterion and atoms are the count-window rule on $[1,\infty)$. For free size and probability, all ones attain at every size $N\ge1$ with $p=0$, or at $N=1$ for every $p$. Any constant $c>1$ attains only at $N=c,p=1$, since other compatible supports contain at least two counts and their allowed atoms do not put mass one at $c$. For nonconstant positives, compare $\overline{X(X-1)}/\bar x_+$ with the mean-matched positive-Poisson rate. A greater value gives strict Poisson escape; a smaller value ensures finite attainment and the finite-prefix algorithm identifies all maximizing sizes. Equality is ruled out as in the earlier reports. The matched probabilities are unique per size. With fixed probability in $(0,1)$ the sequence tends to zero in likelihood and comparing its finite prefix suffices. Held endpoint probabilities restrict the atom sets. [Moment envelope and exact size ties](2026-11-25-tbinom2-math-report.md#the-moment-envelope-for-pure-zero-truncation).

### nbinom free zero:truncated

**Proved.** With $r\ge0$, divide the ordinary unnormalized positive weights by $r$ to get $q^k\prod_{j=1}^{k-1}(r+j)/k!$, valid at zero by continuation. The $r=0$ row is the **attained** logarithmic-series law with weights $q^k/k$. With $q=0$ every $r$ gives the attained atom at one. For a held size and $\bar x_+>1$ the scalar rule gives a unique match in $0<q<1$, since at the lower end the first term dominates, and at the upper end the weight sum diverges (harmonic at $r=0$). With held $0<q<1$ and free size, compare $r=0$ and all positive roots of
$$
 \frac{dL_+}{dr}=\sum_{i:x_i>0}\sum_{j=1}^{x_i-1}\frac1{r+j}
       - n_+\frac{d}{dr}\log\left(\frac{(1-q)^{-r}-1}{r}\right).
$$
There are finitely many: the formula is analytic through zero and strictly negative eventually since the sums tend to zero and the last log derivative tends to $-\log(1-q)>0$. The likelihood tends to zero at infinite size. The maximizing set is exactly the best eligible candidates. In particular with all ones and $p=1/2$ held size zero is the unique maximizer: the positive mass at one strictly decreases with size, since every weight ratio against one increases. With $q=0$ held only ones are compatible and all sizes maximize.

With both free and $\bar x_+>1$, let $q(r)$ be the matched root. The sample criterion is to compare $r=0$ and every positive stationary point of $P(r)=L_+(r,q(r))$ against the mean-matched positive-Poisson face $P(\infty)$. A finite candidate at least as good as the face attains, and the complete maximizing set is the finite set of candidates with the highest value, each with its one matched probability. A strictly better Poisson face is the only excluded limit. To prove the candidate list finite, at zero the divided normalization is analytic, and implicit matching of the mean gives an analytic profile there. At infinity use $s=1/r$, $t=rq$; the positive normalizer before division by $r$ is $(1-st)^{-1/s}-1$, analytic at $(0,t)$ for positive $t$. The matched score has nonzero derivative, so the profile is analytic in $s$ near zero. At first order its coefficient in $s$ is
$$\tfrac12 n_+ \big(\overline{X(X-1)}-E_{\rm ZTP}X(X-1)\big),$$
by differentiating the weights $\prod_{j<k}(1+js)t^k/k!$ and cancelling the derivative of the matched root through its score. The gap is nonzero by the positive-Poisson transcendence argument in the [prior face comparison](2026-11-25-tbinom-math-report.md#poisson-face-comparison-and-equality). Thus the analytic profile is nonconstant and has finitely many stationary points, using analyticity on a compact interval in the coordinate $r/(1+r)$ including both endpoints. This proves the likelihood comparison and gives a signed tail. A positive first-order gap implies finite attainment; the opposite sign alone only orders the tail. The finite criterion does not assume global unimodality. If both coordinates are free and all counts are one, the optimizing ray is $q=0,r\ge0$.

### all free zero:modified

**Proved.** The Bernoulli factorization transports the four preceding criteria, including their held-parent slices and exact ties. The only excluded positive-component faces are the binomial and NB positive-Poisson escapes. They produce modified Poisson with the same Bernoulli weight. All-zero data with any compatible fixed weight or a free weight have the entire admissible positive-component slice as maximizers. Holding the parent coordinates collapses that set to the held point.

### pois free zero:modified

**Proved by factorization.** A compatible Bernoulli weight and the unique positive Poisson fit suffice. There is no excluded positive-component limit. All zeros leave the admissible rate unrestricted when free.

### geom free zero:modified

**Proved by factorization.** A compatible Bernoulli weight and the unique shifted geometric fit suffice. There is no excluded positive-component limit. All zeros leave the admissible probability unrestricted when free.

### binom free zero:modified

**Proved by factorization.** Apply the positive-binomial size and probability results to the positive observations. The only excluded face is the modified Poisson with the same Bernoulli weight. A finite fit inherits the entire size/probability maximizing set, and all zeros leave the positive component unidentified over its admissible slice.

### nbinom free zero:modified

**Proved by factorization.** Apply the positive-NB candidate comparisons to the positive observations. Size zero gives an attained modified logarithmic law; the only excluded face is modified Poisson. The full maximizing set is inherited from the positive component, or its entire admissible slice if there are no positives.

### pois free zero:inflated

**Proved.** The declared ZIP rate domain is $\lambda>0$. For positive observations, free inflation uses the fixed-carrier concavity criterion above, including $\pi=0$ for an infeasible modified root, such as all positive counts equal to one. To check the latter endpoint, if the ordinary Poisson mean is $(1-z)\bar x_+$, then $E_+X>\bar x_+$ there whenever the modified root is inadmissible, so $f_0>z$. The unique fit exists for all compatible samples with positives and free inflation. At held $0<\pi<1$, compare all positive roots of
$$
 \frac{\sum_i x_i}{\lambda}-n_+
 - n_0\frac{(1-\pi)e^{-\lambda}}{\pi+(1-\pi)e^{-\lambda}}=0.
$$
The likelihood tends to zero at both ends for positive samples; this analytic score has finitely many roots, since it is positive near zero and negative near infinity. Cardinality is the number of winning roots. For mixed data write $S=\sum_i x_i$, $u=S/n$, $v=S/n_+$, and $\Delta=v-u$. The roots lie between $u$ and $v$ and satisfy $H(\lambda)=\log(\pi/(1-\pi))$ where
$$H(\lambda)=-\lambda+\log(n\lambda-S)-\log(S-n_+\lambda),\qquad
H'=-1+\frac1{\lambda-u}+\frac1{v-\lambda}.$$
The original score has the opposite sign to $H-\log(\pi/(1-\pi))$. For $\Delta\le4$, $H$ is strictly increasing and the fit is unique. For $\Delta>4$ the derivative has exactly two zeros, with signs $+,-,+$ in order. For levels strictly between the local minimum and local maximum of $H$ there are exactly two local maxima of the likelihood, and their heights determine cardinality. At the lower level the high-rate stationary point is a tangency with strictly lower likelihood than the low-rate maximum (the score is negative between them). At the upper level the reverse holds (the score is positive between the tangency and high-rate maximum). The peaks extend continuously to these levels and their height difference crosses zero exactly once. Indeed its derivative with respect to $\pi$ is strictly positive: the rate scores vanish, the positive-data weight derivatives cancel, and $(1-e^{-\lambda})/(\pi+(1-\pi)e^{-\lambda})$ is strictly increasing in $\lambda$. Thus every mixed histogram with $\Delta>4$ has an exact two-rate maximum for one held inflation. For example 99 zeros and one ten have $\Delta=9.9$, giving the tie in the [prior ZIP analysis](2026-11-07-zip-cert-report.md#nonuniqueness-counterexample). At held $\pi=0$ use the ordinary model restricted to positive rates. At all zeros $\pi=1$ and every positive rate maximize if inflation is free or held at one. With $\pi<1$ held and rate free the only maximizing limit is the zero atom as $\lambda\downarrow0$, agreeing with the documented ZeroLimit. A held rate has just the Bernoulli optimization in the stated admissible domain.

### geom free zero:inflated

**Proved.** For positive samples with both free, the fixed-carrier criterion gives a unique finite fit using the shifted-geometric root if admissible, otherwise the ordinary geometric with $\pi=0$. Its excluded endpoint $q=1$ has zero probability at each observed positive count; $q=0$ also cannot give positives. For held $\pi<1$, the log likelihood as a function of $0<q<1$ is, up to constants,
$$ (\textstyle\sum_i x_i)\log q + n_+\log(1-q)+ n_0\log(1-(1-\pi)q). $$
It is strictly concave, with zero likelihood at both ends, hence a unique root. At all zeros, $q=0$ or $\pi=1$ yields exactly the attained maximizing union, intersected with holds; if both fixed away from these values simply evaluate. With $q$ held and inflation free choose $w=\max(z,1-q)$, unless the parent is already the zero atom, when the weight is unidentified.

### binom size zero:inflated

**Proved.** For $N\ge2$ with positives and free $\pi$, use the fixed-carrier rule for $0<p<1$. The case all positives at $N$ takes $p=1,\pi=z$ and attains the empirical two-atom upper bound. No other parameter gives that bound for $N\ge2$, as other positive counts would have positive mass. For all positives at one, the conditional lower atom is unavailable to inflation for $N\ge2$ because $p=0$ forces $w=1$. The ordinary boundary solution applies in that case. With $\pi<1$ held, compare all interior score roots and the $p=1$ endpoint if compatible. The total likelihood is a polynomial in $p$ (counts are integers), so one may compare exact real roots given rational holds, or use the derivative polynomial on the declared real input. There is no excluded face and the winning candidates give the exact cardinality. At $N=1$ the positive Bernoulli probability is $(1-\pi)p$; the optimal product is $1-z$ if feasible, otherwise its closest feasible endpoint by strict Bernoulli concavity. Every feasible pair producing the optimal product maximizes, with the holds intersected. At $N=0$ only zeros are compatible and the law is flat. For all zeros in general, whenever feasible, the maximizing set is the parent-zero cases ($N=0$ or $p=0$) or $\pi=1$, intersected with holds. If neither can be attained under the holds use the same one-coordinate Bernoulli rule.

### binom free zero:inflated

**Proved.** At each feasible size use the preceding slice rule, compare its value with the best ZIP face under the corresponding $\pi$ hold, and include every winning finite pair (and every maximizing $\pi$ there). A strict ZIP face win is the only excluded positive-sample limit when $p$ can move. To see this, along $N\to\infty$ with positive likelihood, $Np$ stays bounded away from zero and infinity. For each fixed positive observed $k$, the mass is at most $(Np)^k e^{-(N-k)p}/k!$, and also at most $(Np)^k/k!$. Any subsequential positive-rate limit gives ordinary Poisson masses, hence the corresponding ZIP likelihood by continuity, and every such limit is reachable with $p=\lambda/N$. At held $p>0$ the positive-sample likelihood vanishes at infinity and a finite winning prefix suffices. At held $p=0$ positives are incompatible. At free inflation and no zeros use the ordinary binomial criterion. At zeros only, apply the same atom union as for held size. This is an exact comparison including finite-to-face equality. For arbitrary mixed data the comparison is over an infinite sequence unless a tail certificate applies. Multiple finite solutions are possible already in the $N=1$ Bernoulli slice, e.g. sample $(0,1)$ fits every allowed pair with $(1-\pi)p=1/2$ at that size and hence globally by the multinomial bound.

### nbinom size zero:inflated

**Proved.** With $r>0$ held and positives, free inflation uses the fixed-carrier rule. At the conditional all-ones edge $q=0$ the base is all zero, so it cannot supply positives. Both $q$ endpoints give zero likelihood on positive samples and there is a unique finite solution for free $\pi$. At held $\pi<1$ compare interior roots after substituting $w=\pi+(1-\pi)(1-q)^r$. The function is analytic on $(0,1)$, the likelihood vanishes at both ends, and the maximizing roots are finite in number by analyticity on a compact level set above a positive witness. Cardinality is the number of winning roots. The $r=0$ slice is an ordinary zero atom, not the positive-conditioned logarithmic extension. All-zero maximizers use $\pi=1$ or a parent zero atom whenever those are feasible, and otherwise the Bernoulli weight rule.

### nbinom free zero:inflated

**Proved.** For positive samples, use the following candidate/face test. At free $\pi$, include the ordinary parent best fits on the $\pi=0$ slice and all stationary positive-component fits with $0<\pi<1$ given by $f_0<z$ and $w=z$. The latter have zero positive-component scores in the unheld coordinates. At held $\pi<1$, use all simultaneous roots of the unheld parent scores of $B(\pi+(1-\pi)p^r)+L_+$. For one held parent coordinate this is a scalar equation; a held probability in $(0,1)$ gives zero likelihood at infinite size. At an interior candidate with all parent coordinates held simply evaluate. In each case compare the eligible candidates with the corresponding Poisson or ZIP face when both parent coordinates are free. A finite candidate at least as high as the face attains; the complete finite maximizing set consists of the highest candidates, so uniqueness requires exactly one. Strict face victory is an escape. With no zeros and $\pi$ free use the ordinary NB criterion. With only zeros use the union $\pi=1$ or $r=0$ or $p=1$, intersected with holds whenever feasible, and otherwise the weight rule.

For completeness, there are no other positive-height excluded faces. As $r\to0$ each fixed positive parent mass is $O(r)$ uniformly in $p$ by the rising-factorial formula. At bounded positive size the endpoints $p=0,1$ give zero mass at any fixed positive count. At $r\to\infty$ positive-height sequences have $t=r(1-p)$ bounded away from zero and infinity. The fixed-count factor $(r)_k(1-p)^k$ is bounded by a constant times $t^k$, and $p^r\le e^{-t}$, forcing vanishing positive masses at either extreme. At a finite positive limit of $t$, the PMF converges to Poisson by the binomial-series formula; this face is reachable and $\pi$ belongs to a compact interval. Consequently any superlevel strictly above that face is compact, while any finite tie belongs to the stationary or boundary list. The Poisson fit at free $\pi$ may have zero weight, and then the named limit is ordinary Poisson. This comparison proves the criterion without asserting an effective general root isolation or strict separation at equality for the two-dimensional held-inflation score.

## Summary

The table assumes valid observations, positive likelihood, and at least two retained counts or a positive-width interval. Singleton, hold, constant and bound cases are specified in the proofs.

| Parent and coefficients | Attainment test | Excluded maximizing limits |
| --- | --- | --- |
| Poisson; binomial with held size | Interior retained mean or compatible attained atom | Excluded retained endpoint atoms |
| Geometric; NB with held size | Mean between bottom and zero-probability mean, or attained lower atom | Lower atom; finite-window uniform or rising-factorial weights |
| Binomial with free size | Winning finite size | Same-window Poisson; upper atom at held interior probability on a finite window |
| Finite zeta | Interior log mean | Either endpoint atom |
| Normal and lognormal, both free | Nonconstant; variance below matched linear-tilt variance if edge exists | Exponential or power tilt, including uniform or log-uniform; collapse |
| Normal and lognormal, one held | Held-slice moment within the score range | Uniform or log-uniform for held location; projected location or endpoint collapse |
| Exponential and its monotone transforms | Transformed mean between bottom and finite-window midpoint, or above bottom on an unbounded transformed window | Uniform after transformation; endpoint collapse |
| Gamma and inverse gamma, shape held; gamma, scale held | Mean within the scalar score range | Power face or shape-zero gamma carrier; collapse |
| Gamma, beta, inverse Gaussian, both free | Nonconstant; no integrable edge or corner passes inward score tests | Zero-coefficient edge or corner; collapse |
| Zero truncated and modified | Positive-count scalar or size-profile rule; Bernoulli factor in modification | Positive or modified Poisson for free binomial or NB |
| Zero inflated | Clipped Bernoulli and fixed-carrier root, or winning candidate across size/profile slices | ZIP or ordinary Poisson for free size; zero-rate ZIP atom for the stated all-zero hold |

Regular scalar and joint estimates are unique on fixed carriers. Exact coefficient cardinality after atom extensions, bound profiling or a free size comes from the maximizing sets above.

## What this changes for the fitter

**Implementation audit at the recorded revision.** The [interface specification](../../design/modifiers.md) implements count windows for Poisson, geometric, binomial, NB with size held, and signed zeta on finite $[1,N]$, plus all three normal free/held routes. The positive-window alias also includes free-size NB. The zeta dispatch still requires the profiled lower bound to be one and the upper bound to be an integer. For lower-estimated zeta with a minimum above one, the proof works on the resulting finite range by replacing 1 with the sample minimum, although that window fails the current dispatch check. Normal already detects when estimated bounds can shrink onto a constant sample. All four target count families expose each zero modifier. The [initial implementation](2026-11-12-modifiers-report.md#mathematical-and-numerical-evidence), [retirement report](2026-11-12-modifiers-2-report.md#implementation-and-mathematical-evidence), [short-window evaluation](2026-11-22-short-window-report.md#c-placement-and-family-coverage) and [stable profile PMF replay](2026-11-11-ztpois-pmf-report.md#implementation-and-caller-inventory) describe the numerical scopes. The [zero uniqueness inventory](2026-11-22-zero-unique-report.md#private-modifier-routes) covers flat maximizing sets, and the [positive NB candidate fitter](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/ztnbinom_mle.hpp) expressly includes the size-zero logarithmic member.

- **Normal near a face.** The named limits and exact signs in the dossier agree with the proof. The implementation can mark a solved interior point within $10^{-8}$ per observation of a face as an unresolved public comparison even on the exact interior side. For an exact interior example take $\pm57735/100000$ on $[-1,1]$. The difference $\delta=1/3-(57735/100000)^2$ is positive and below $10^{-6}$. By symmetry the linear tilt is zero. On $[-1/100,0]$ the quadratic tilt has density ratio at least $e^{-1/100}$ against uniform, so $\operatorname{Var}(X^2)\ge e^{-1/100}(4/45)>0.08$, using the minimum over centering constants. The root lies in that interval since the score changes by at least $0.0008$. Strong concavity bounds its improvement by $\delta^2/0.16<10^{-8}$. This establishes the gap and implicates the comparison rule by source inspection. A sign enclosure and an interior coefficient enclosure can decide the case. [Normal comparison and outcome mapping](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_mle.hpp).
- **Geometric and NB window statuses.** The lower/upper enum labels agree with the mathematics. The generic outcome overload maps them to NoConvergence, so the route payloads are needed to distinguish proved nonattainment from search failure. The finite upper mean at $p=0$ gives uniform for geometric or NB size one; the general NB face needs the retained window and size. [Count-window outcomes](../../../univariateML-cpp/include/cppunivariate/distributions/geom/truncation.hpp).
- **Binomial and NB zero profiles.** The documented Poisson detectors are sufficient in their stated scope. A complete positive or modified NB comparison includes the attained logarithmic face. At held $p=1/2$ the sample $(1,1)$ gives size zero with mass $1/(2\log2)$ at one. A finite winner tying an excluded face attains. Each exact co-winner belongs in the maximizing set, including the integer-indexed graph in the bounded binomial example.
- **Inflation.** ZipLimit and PoissonLimit correspond to the proved positive-height boundary list. An unresolved comparison or a FaceTie decided at a floating-point margin does not prove exact equality. In free-size slices the ordinary fit on $\pi=0$ still needs a global comparison. With fixed carrier and free inflation the concavity argument supplies a direct certificate; for geometric at held inflation the strict scalar concavity also supplies one.
- **Refusals with proved routes.** Exponential, held-shape Weibull, Rayleigh, held-endpoint Pareto, held-shape inverse gamma and inverse Weibull use transformed scalar checks. Lognormal reuses the three normal routes with a likelihood Jacobian. Gamma, beta and inverse Gaussian use concave interior fits with explicit edge/corner tests (or held-slice scalar checks). For scalar routes enclose the root in the public coefficient and evaluate the conditional normalization and means stably. For joint routes enclose the tangential face solution and its inward score, then the interior root with a covariance bound when the sign requires it. The exact zero sign selects the face by concavity. Direct subtraction of parent CDFs can be inadequate, as in the [short-window report](2026-11-22-short-window-report.md#open).
- **Fallback.** Where global comparison is unresolved, the successful accuracy-checked and unrefuted search remains eligible with certification withdrawn. Proved nonexistence, a separated winning limit, incompatible data or a failed search needs its corresponding outcome.

## Atlas proposal

Use a regime key (family, held, modifier), with an empty modifier for base rows. A held mask uses public coefficient names, sorted and joined by +. The modifier keys are truncate:known, truncate:lower-estimated, truncate:upper-estimated, truncate:both-estimated, zero:truncated, zero:modified, and zero:inflated. Window geometry, support intersection and admitted coefficient extensions belong in the claim or its domain metadata. Implementation status is separate from proof coverage.

Store attainment, failure and uniqueness as separate questions with iff, exact or one-sided directions. A written uniqueness criterion can recover the maximizing set from candidates, with an explicit frontier if comparison is ineffective. Apply the generic estimated-bound rows before selecting a fixed-window parent row. This preserves the distinction between cardinality at the canonical window and cardinality after allowing looser bounds. The Bernoulli factorization similarly applies before retrieving a positive-count parent row. See the [atlas overview](../existence-atlas/overview.md) and the [proof rows](2026-11-27-trunc-expfam-math-data/proofs.csv).

## Numerical checks

**Numerical.** Workstation mpmath runs at 80 and 120 decimal digits replay the non-steep even-count mean, gamma corner signs and normal near-face likelihood gap. They compare mean-two positive NB profiles at $r=0,1,10,100$ and the Poisson face, for one count $M$ and $M-2$ ones with $M=3,10,50$. For $M=3$ the checked heights rise toward Poisson. For $M=10,50$ the logarithmic law exceeds every checked positive-size value and the Poisson face. The finite grid supplies no claim about the overall winning size. The held-probability all-ones logseries value and the inflated binomial weight for 19 zeros, 11 ones and 8 twos at size three are also checked. [Reproducer](2026-11-27-trunc-expfam-math-data/check.py) · [Numerical table](2026-11-27-trunc-expfam-math-data/checks.csv).

From the worktree root, reproduce both local single-slot runs with:

    export TMPDIR="$PWD/build/tmp"
    mkdir -p "$TMPDIR"
    data=dev/research/evaluations/2026-11-27-trunc-expfam-math-data
    /tmp/umvl-cpu/run nice -n 10 python3 -B "$data/check.py" > "$data/checks.csv"
    /tmp/umvl-cpu/run nice -n 10 python3 -B "$data/atlas.py"

The atlas check verifies the base column order with modifier inserted after held, unique claim keys, and the destination anchors of all evidence rows.

## Open

1. **Global free-size inflation certificates.** Mixed binomial samples still need an effective tail comparison for every input. The NB held-inflation profile needs exhaustive two-dimensional stationary-point or rectangle coverage. The positive-height compactification excludes all but ZIP/Poisson. Exact finite ties and finite-to-face equality outside algebraic fixed-window binomial comparisons need additional separation or an equality theorem.
2. **Cardinality shortcuts for free size.** Positive NB has a finite stationary candidate set and its nonzero factorial gap orders the tail. A terminating decision at multiple roots or exactly equal heights and a sample-level classification of those ties remain open. The lower-only binomial unimodality and nonconstant ties remain the specific open questions of the prior binomial reports.
3. **Wider domains.** Adopting full $N_A$ would make some excluded faces attained and add increasing scalar tilts on bounded windows. That requires changing the declared model and conditional evaluators. Free-shape Weibull and inverse Weibull fall outside the fixed-statistic analysis.
