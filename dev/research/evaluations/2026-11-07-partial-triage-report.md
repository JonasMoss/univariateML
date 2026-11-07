# Routes to certify the partial families — TASK-116.18

The best first jobs are ZIP's held-inflation reduction, the remaining scalar
fatigue and shifted-Gompertz problems, generalized beta's held-shape power
problem, and quantitative acceptance checks for concave conditional fits.
The largest shared gains come next from the transformed-gamma pair and the
four binomial relatives. General GB2, free-dispersion beta-binomial size,
and the joint shape/endpoint problems need further boundary mathematics;
replacing their meshes by finer meshes would not certify them.

Source: `681d7ac5ce6965db4066b4117c12eff69e75cd0c`, branch
`codex/partial-triage`. This report completes the requested research and job
plan. No fitter, test, dossier, golden, binding, board or roadmap was changed;
no git write command, installation, delegation, push or publication was used.
The new files are this report and its
[reproduction and evidence directory](2026-11-07-partial-triage-data/).

## Census, notation and meaning of a certificate

Parsing YAML gives **29 partial families**, 67 `yes` and three `no` among
99 families. A literal search for `certified: partial` finds only 25:
bnbinom, frechet, gev and sged quote the value. Sstd, st and tlambda are
`no`, outside this census. Zinb is already `yes`; its existing certificate
contains machinery useful to ZIP. The
[machine inventory](2026-11-07-partial-triage-data/inventory.json) lists every
one of the **251 nonempty free subsets**, its complementary holds and any
matching structural exception. All-held evaluation is excluded from the
certification count.

Below, `F={...}` lists **free** coefficients; every other coefficient is
held. A named collection of subsets is exhaustive unless explicitly narrowed
by held values. Bounds that are estimated first, such as truncation bounds,
are stated separately. A certificate is in exact arithmetic conditional on
computed comparisons, with a stated practical floating-point margin, following
the package's existing standard. No outward-rounded arithmetic is demanded.

Three gaps must not be conflated:

* **Search gap:** a mesh, multistart or local solve can miss a different maximum.
* **Acceptance gap:** a global concavity/monotonicity theorem identifies an
  exact root, but a small computed residual alone does not bound likelihood
  error or locate the returned coefficients within a certified enclosure.
* **Structural stop:** an unsupported subset or proved non-existence is not an
  uncertified successful fit. Some stops are themselves conservative numerical
  decisions and do not establish which face wins.

Existing mathematical reductions below are not a fresh arithmetic audit of
every parent solver. In particular, using an uncertified face fit as a global
upper bound would invalidate a child certificate. Every follow-up must retain
actual holds, original-unit Jacobians, endpoint penalties and all unresolved
competitors. The [stop contract](../../design/structured-stop-details.md),
[work budgets](../../design/work-budgets.md),
[parameter rules](../../roadmap/2026-10-29-parameter-rules.md) and
[validation guide](../../validation/README.md) govern implementation.

## Complete family triage

### Modified and truncated binomial families

**1. ztbinom — `size=N`, `prob=p`.** The outstanding subset is
`F={N,p}`. Fixed `N>1` gives a strictly concave conditional exponential
family in log odds; mean matching includes the point masses at one and `N`.
At `N=1`, `p` is unidentified. Held interior `p` already has an integer-tail
bound; held endpoint probabilities have exact compatibility decisions.
The free-size mesh and non-improvement stop do not exclude unsampled sizes
or prove that the positive-Poisson face wins. Use the retained-support bound
proved below to certify strict finite improvements, followed by complete
enumeration or bounded block exclusion below the cutoff. A signed Poisson-tail
expansion is still needed for a winning-face decision. Do not assume integer
unimodality from an ordinary-binomial theorem.

**2. zmbinom — `N,p,p0`.** The outstanding subsets are
`F={N,p}` and `{N,p,p0}`. Holding or fitting `p0` affects only the
Bernoulli zero-indicator factor; the base problem on positive counts is
exactly ztbinom, with no Jacobian. Thus the preceding certificate transfers
to both subsets. `F={p0}` is analytic; other subsets with `N` or `p` held
use the already settled base routes. No-positive samples and all-one positive
samples have attained, potentially unidentified representatives. Preserve
those cases rather than treating all size divergences as non-existence.

**3. zib — `N,p,pi`.** The outstanding subsets are
`F={N,p}` with inflation held, and `{N,p,pi}`. Fixed-size probability
profiles enumerate their branches, and held interior `p` already permits
integer-tail exclusion. Free inflation reduces to the constrained positive
binomial branch when the base zero probability is at most the empirical
zero fraction, and to ordinary binomial otherwise. This branch constraint
must be retained; the unconstrained ztbinom optimum need not be feasible.
The retained-support bound below also applies directly with inflation held
or free. Reuse ZIP's certified face profiler after job J1, and the fixed-size
ZIB quadratic turning-point machinery. All-zero free-size subsets attain size
zero; this exact result does not require an infinite-size comparison.

**4. tbinom — `prob=p,a,b,size=N`.** Profile every free truncation bound
to its observed extreme first. The outstanding free subsets are exactly
those containing **both `N,p`**, with any subset of `{a,b}` also free:
`{N,p}`, `{N,p,a}`, `{N,p,b}`, `{N,p,a,b}`. This description includes
held infinite upper bounds. Fixed-size conditional probability fitting is
concave; the held-probability integer route is separate and bounded.
For a finite effective upper endpoint `U`, the existing optional pass already
uses the all-size correction in the
[size-search report](2026-10-31-size-search-report.md). It certifies some
strict finite winners. However, failure or excessive cost of that pass
returns the original numerical successful candidate; winning-Poisson stops
also remain numerical. Thus these subsets are not uniformly certified.
Make the pass mandatory for success, use the common retained-support bound
for infinite windows, and replace unresolved face selection by a tie/budget
stop. Include sizes below `U`, where conditioning support changes. Full
untruncated delegation and one/two-point windows deserve their own exact
reductions, not a blanket numerical label.

### Other discrete exponential and mixed families

**5. zip — `lambda,pi`.** Only `F={lambda}` with held
`0<pi<1`, zeros and positive counts has a global-search gap. The free joint
fit and rate-held inflation profile are exact reductions; `pi=0` is Poisson,
no-zero samples give the positive-count mean at held inflation, and `pi=1`
has only compatible all-zero data. The held-inflation score has at most three
roots in an explicit finite interval, as proved below. Better still,
`zinb_global_detail::zip_upper` in
[zinb/global_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/global_certificate.hpp)
already implements the equivalent logarithmic equation with quadratic
turning-point isolation, rate brackets and likelihood bounds. Extract that
machinery into a family-local shared helper rather than importing all of
ZINB into ZIP or maintaining two certificates. Preserve all rate peaks when
likelihood bounds overlap. The helper can refuse nearly multiple turning
points; that is an allowed numerical stop, not permission for a mesh result.

**6. cmp — `lambda,nu`.** All three nonempty free subsets have settled
global exponential-family mathematics; the remaining issue is quantitative
acceptance and moment-tail error propagation, not an unknown landscape.
In natural coordinates `(eta,nu)=(log(lambda),nu)`, the negative Hessian
is the covariance of `(X,-log(X!))`. With mean matched, the dispersion
curvature is the positive Schur complement. Holding either coefficient
restricts to a line. The current shared solver already uses safeguarded
mean matching and a monotone dispersion bracket, not an arbitrary joint
multistart. Zero, geometric and adjacent-point classifications are separate.
Use surviving coordinate brackets plus moment/normalizer truncation bounds
to bound the score and likelihood gap. For an interior root, add coefficient
inclusion; near the geometric boundary certify the KKT score sign rather
than snapping to it solely because its likelihood is within `1e-8*n`.
The settled implementation lives in `special/cmp.hpp`: this job does not
authorize changing it. A follow-up must explicitly authorize such a change
or add a dossier-level acceptance wrapper with independently bounded moments.

**7. cmb — `size=N,prob=p,nu`.** With `N` held, the three free subsets
`{p}`, `{nu}`, `{p,nu}` have concave global theory but the ordinary
`cmb_mle` acceptance uses moment residuals. Add bracket/coefficient bounds
and reuse the entropy dual already used for finite-size solves inside the
free-size certificates. Holding `nu` makes the odds score monotone;
holding odds makes the dispersion score monotone; profiling odds gives the
positive covariance Schur complement. Boundary sufficient statistics require
the existing exact convex-hull analysis. The four subsets containing free
`N` already have conditional accepted-fit bounds: all-free and held-`p`
searches use entropy/dispersion bounds, and held-`nu` or both-held base
coordinates use their respective integer tails. Their unresolved faces and
budgets are stops, not missing certificates on successful interior returns.
The public domain is **`0 <= N <= 10^7`**: exhausting that genuine domain is
different from returning an exploratory size cap. A regional implementation
gap remains for positive constants above five million with interior `p`
held, where `2*x` exceeds the supported design; document it separately from
uncertified success. No new general joint infinite-size closure theorem is
needed merely to strengthen ordinary fixed-design acceptance.

**8. bbinom — `N,alpha,beta`.** The unsupported outstanding subset is
`F={N,alpha,beta}`. Fixed-size free-shape profiles already have tangent/chord
bounds. The three supported free-size subsets `{N}`, `{N,alpha}` and
`{N,beta}` have different tail arguments; do not re-open them as though
reflection transferred the held-beta bound to held-alpha. Their budgets
or unresolved NB faces can stop. The missing joint bound cannot simply
maximize the held-alpha correction: its `alpha^2` term is not uniform.
Start in `(mean,alpha)` coordinates with `beta=N*alpha/mean`, use certified
NB/Poisson face profiles, and split small/compact/large alpha. The CMB-style
saturated-likelihood lower bounds on each observed category's probability
are promising superlevel restrictions. **Open:** explicit bounds on those
three regimes uniform in `N`, including concentration at finite-design
binomial/endpoint faces and joint escapes. Until obtained, enumerating a
large finite prefix is not a route to a successful joint fit. This is
TASK-113's research gap, not a small extension of a scalar solver.

**9. bnbinom — `size=r,alpha,beta`.** The outstanding subsets are the
three two-free-shape sets `{r,alpha}`, `{r,beta}`, `{alpha,beta}`, and
all three free. Every scalar free-shape subset already has a single-crossing
certificate. With alpha held, positive-count likelihood superlevel sets
in `(r,beta)` are compact, as proved in the dossier; exploit that first.
The same dossier gives a diagonal-exclusion example proving multiple
labeled maxima, so never select a diagonal solution merely by symmetry.
Use the scalar certificates to profile one coordinate, preserving both
swap-related maxima when neither swapped coordinate is held. For the two
other pairs and all-free fitting, certify the compatible NB/Poisson faces
and bound their neighborhoods. **Open:** effective compact-superlevel
cutoffs for the alpha-held problem and uniform face remainders as alpha
also varies. The qualitative existence proof is useful but supplies no
numerical rectangle by itself. The
[bnbinom certificate report](2026-11-06-bnbinom-cert-report.md) and its
nonuniqueness reproducer are the existing starting point.

### Positive transformed families

For the next three beta families, use `p=shape1`, `a=shape2`, `q=shape3`
for GB2/Feller–Pareto. Beta prime has `p=shape1,q=shape2`. For generalized
beta below, its different public order is stated explicitly.

**10. betapr — `p,q,b=scale`.** The outstanding subsets are `{p,q,b}`,
`{p,b}` with non-unit held `q`, and `{q,b}` with non-unit held `p`.
Held `p=1` is certified Lomax, held `q=1` certified inverse Pareto,
including further holds and transported diagnostics. At held scale,
`y=x/(b+x)` gives a beta likelihood; with both shapes held, the log-scale
score is strictly decreasing. These cover every other free subset.
For the two non-unit conditional problems, profile the single free beta
shape by its monotone digamma score and bound intervals of log scale by
beta-conjugate chord envelopes. Reciprocal data exchange the two jobs with
Jacobian `-2*sum(log(x))`. Compare against the gamma face retaining `p`,
or inverse-gamma face retaining `q`. **Open:** a uniform exterior remainder
for the coupled scale/shape escape and a signed bound that can select the
face. The three-free-parameter problem additionally needs simultaneous
large-shape/scale paths, including its lognormal corner; a list of two
one-shape limits is not an exhaustive joint certificate.

**11. gb2 — `p,a,q,b`.** Outside exact unit-shape reductions, the
outstanding subsets are
`{p,a}`, `{p,b}`, `{q,a}`, `{q,b}`, `{p,q,a}`, `{p,q,b}`,
`{p,a,b}`, `{q,a,b}`, `{p,a,q,b}`. Equivalently, at least one beta shape
and at least one of power/scale are free. Held `q=1` or `p=1` transfers
the relevant certified Burr/inverse-Burr routes, including their held-scale
two-free-shape cases; do not apply a unit-shape reduction to an estimated
shape merely near one. Both beta shapes held gives strict concavity in
`(a,c=-a*log(b))`; held power and scale gives beta concavity. These also
cover all scalar subsets. Reuse those inner solves, then envelope the one-
or two-dimensional beta-shape profile. **Open:** compact bounds on all
likelihood-relevant outer shapes and quantitative neighborhoods of the
generalized-gamma, reciprocal-gamma, lognormal and power-tail corners.
The dossier explicitly does not exhaust all joint faces. Implement the
non-unit beta-prime conditional job before attempting this larger closure.

**12. fpareto — `p,a,q,b,min`.** With `min` held, its nine outstanding
sets are exactly the preceding nine GB2 sets, on positive excesses.
Translation has unit Jacobian; append held `min` only to finite family
witnesses, leaving face coordinates in excess-data units. The same certified
unit-shape and concave reductions transfer. Every subset containing free
`min`—16 sets—is structurally stopped by the current interface; existence
depends on held shapes, and these are not all universal no-MLE theorems.
For example with all other parameters held and `a*p>1`, the density tends
to zero as the threshold reaches the minimum, and also as it escapes to
minus infinity, giving finite attainment. Threshold estimation therefore
belongs to a separately authorized feature job, not the GB2 certificate
transport. Preserve subtraction/rounded-tie guards in the present delegate.

**13. genbeta — public `p=shape1,q=shape2,a=shape3,b=scale`.** Hold
`b>max(x)` as required. The outstanding current numerical subsets are
`{a}`, `{p,a}`, `{q,a}`, `{p,q,a}`. At fixed power, beta-shape concavity
covers all remaining supported subsets. The first of these is actually
strictly concave **for every held `p,q>0`**, not just `q>=1`; proof below.
Held `p=1` reduces to certified Kumaraswamy on `x/b`, with likelihood shift
`-n*log(b)`. Held `q=1` is the attained power ridge depending only on
`p*a`, already handled analytically. Thus both are useful exact special
routes even though the generic free-power mesh is not a certificate.
For the remaining profiles, use a beta-conjugate chord upper envelope and
the gamma limit of `Y=log(b/x)` at small power, retaining held `q`.
At large power allow the moving power endpoint when `q` is free; do not
compare only with the power law whose endpoint is `b`.
**Open:** uniform small-power/large-shape and moving-endpoint remainders,
including their intersections. All eight sets containing free `b` are
structural stops. When `q` is free or held below one they have the endpoint
pole; with other held values, finite endpoint fits can exist but are not
implemented. An unsupported hold is not a reason to label every such
sample non-existent.

**14. trgamma — `k=shape1,p=shape2,a=scale`.** Only `{k,p}` with scale
held and `{k,p,a}` retain outer-search gaps. Held `p` is a gamma reduction;
held `k` is a strictly concave power/location problem; all scalar subsets
are monotone or concave. Reuse the
[existing profile derivation](2026-10-29-trgamma-derivation.md): its convex
gamma-conjugate function has secant upper bounds, and it supplies an analytic
large-power cutoff. This is substantially more machinery than a bare grid.
The main all-free missing component is the small standardized-power
neighborhood of the lognormal face, especially zero first seam coefficient;
reuse the Student certificate's strategy of signed expansions and bounded
remainders, not its t-specific formulas. At held scale the gamma shape
solves `psi(k)=p*mean(log(x/a))`; secants of the same conjugate give upper
bounds in `p`. Small `p` loses likelihood, while large `p` either loses it
or reaches the fixed-endpoint power face. If observations equal that held
endpoint, the density carries an `exp(-1)` factor for each such observation;
free scale can move slightly to remove it, held scale cannot. This is a
likelihood-neighborhood issue, not a conclusion from weak convergence.
**Open:** quantitative uniform seam remainders and coefficient inclusion for
the retained roots, with actual held-scale endpoint factors.

**15. invtrgamma — same public `k,p,a`.** Its outstanding subsets are
the same two sets, with the reciprocal scale held when appropriate.
Transport the trgamma certificate on `1/x`, map scale back to its reciprocal,
and shift every likelihood bound by `-2*sum(log(x))`. Lognormal stays
lognormal; the power face becomes Pareto. This is one implementation job
with trgamma, not a second theoretical search. Underflow/rounded transformed
ties and unrepresentable reconstructed scales must stop. Its exact scalar
and held-shape routes remain as before.

**16. fatigue — `alpha,beta,mu`.** With threshold held, `{alpha}` is
analytic. `{beta}` with alpha held is **not monotone**, contrary to the
fit note: it can have two global maxima even for a singleton. `{alpha,beta}`
uses a profiled Newton root and a harmonic/arithmetic-mean fallback; it
does not scan a generic joint scale grid as the note suggests. Both have
finite rational-score enumeration routes proved below. This closes the
mathematical route without needing a joint-concavity assertion. For large
samples retain rational interval score/objective bounds rather than expanding
a high-degree polynomial in a poorly conditioned monomial basis. Every set
containing free `mu` is currently structurally stopped: all-free has the
documented threshold pole, and the three partly held threshold problems
remain unimplemented with value-dependent existence. Do not enlarge that
scope in a certification job.

**17. sgomp — `b,eta`.** Only `{b,eta}` has a genuine global-search gap.
`{eta}` has a strictly decreasing score; `{b}` with eta held is strictly
concave, with the exponential formula at eta zero. The latter is currently
searched by a grid/refinement despite that theorem: replace it by a signed
score bracket and quantitative root acceptance. The existing all-free
likelihood envelopes give finite bounds on `b` for nonconstant samples,
and the eta score supplies a finite uniform eta bound on that interval;
proof below. Thus a two-coordinate rectangle or a one-dimensional
shape-profile branch-and-bound is available **now**, with no unknown joint
unimodality theorem required. Include attained `eta=0` and preserve the exact
constant/all-zero divergence cases.

### Location-scale, convolution and robust families

**18. lst — `mu,sigma,df`.** All-free already uses the native certificate;
`F={sigma}` with location and df held has the documented monotone scale
score. The remaining conditional sets are `{mu}`, `{df}`, `{mu,sigma}`,
`{mu,df}`, `{sigma,df}`. With df held at least one, free location/scale
has geodesic convexity; strengthen the current EM residual acceptance with
the nuisance inclusion machinery in `lst/certificate.hpp`. With scale also
held, location can have multiple maxima: its rational score has numerator
degree at most `2*n-1`, with every root in the sample range. Do not transfer
joint geodesic convexity to a horizontal held-scale slice. The three
free-df sets can reuse the shape interval/chord and normal-tail machinery
after enforcing the actual holds. Held location exempts the df lower bound;
df below one loses the geodesic-convexity route, and with a tie at the held
location can permit unbounded collapse as df decreases. At fixed subunit
df with both nuisance coordinates free, polynomial/box enumeration and its
tie-collapse faces are still required. These are TASK-116.13's existing
scope; the all-free certificate must not be advertised as covering it.

**19. std — `mean,sd,nu`.** All-free already certified. The outstanding
conditional sets are `{mean}`, `{nu}`, `{mean,sd}`, `{mean,nu}`,
`{sd,nu}`. Only `{sd}` has the monotone fixed-location/fixed-shape scale
route. With nu held, ordinary scale is `sd*sqrt((nu-2)/nu)`, so the fixed-df
lst methods transfer directly. With nu free and sd held this map is a
constraint, not a free nuisance reparametrization. In particular, nu down
to two forces ordinary scale to zero, and a positive-scale t2 face cannot
be used for that subset. Reuse the native normal/t2/lower-collapse bounds
only after imposing this constraint. The held-mean/free-sd problem can
approach a positive ordinary-scale t2 face and must compare it. Conditional
location roots can again be multiple. Coordinate enclosures must be checked
after mapping back to standard deviation, including near two.

**20. sn — Azzalini `xi,omega,alpha`.** With location free and alpha
free, the implementation already bounds every finite alpha interval and
both tails, including with omega held. Its remaining partial scope is
primarily an acceptance gap in the concave inner solves, not wholesale
uncertified skewness search. With alpha held, `{xi}`, `{omega}` and
`{xi,omega}` are affine restrictions of a strictly concave likelihood in
`(1/omega,xi/omega)`. With xi held and alpha free, `{alpha}` and
`{omega,alpha}` separate exactly under `beta=alpha/omega`; the scale is
analytic and the beta likelihood is strictly concave for mixed residual
signs, monotone for one sign. The current code already uses this reduction.
Add signed root and reconstructed-coordinate enclosures, use the actual
held-location half-face factors, and clarify the note. No new multidimensional
search theorem is missing for those held-location subsets. A held location
exactly at an observed endpoint loses a factor two per tied observation
relative to the free-location closed half-normal face.

**21. snorm — two-piece `mean,sd,xi`.** All-free already enumerates a
degree-at-most-seven mode equation on each observation cell; that is a
global exact-arithmetic candidate comparison conditional on computed roots.
The note's blanket statement that free skewness uses finite scans is wrong
for this all-free route. The outstanding skew-free held-moment sets are
`{xi}`, `{mean,xi}`, `{sd,xi}`. With skewness held, `{mean}`, `{sd}`,
`{mean,sd}` have concave mathematics but the non-unit held-fit code still
uses finite nested scale/location minimizations; replace those by the
concave inverse-scale formulation, not tighter fixed log-scale limits.
Held xi one reduces to normal exactly. For free xi under moment holds,
use mode and the two positive half-scales: held mean is affine, held
variance a positive quadratic constraint, and the Gaussian scores on each
observation cell are rational. Algebraic root/component enumeration on
these constrained curves is the promising complete route; use the existing
all-free root isolator as a starting point, not as an unreviewed high-degree
solver. Include seams and both half-normal endpoints, with the nontrivial
endpoint penalty when both moments are held. The
[skew-theory report](2026-11-07-skew-theory-report.md) provides the analogous
coordinate/endpoint framework; its finite-t formulas cannot be copied unchanged.

**22. ged — `mean,sd,nu`.** The four free-shape sets `{nu}`, `{mean,nu}`,
`{sd,nu}`, `{mean,sd,nu}` retain numerical shape searches and uniform-face
comparisons. Fixed positive nu is settled: convex residual-power minimization
above one, medians at one, and enumeration of observation knots below one;
scale is explicit when free. The symmetric fixed-shape SGED certificate
already supplies this machinery. Profile location/scale and bound every
finite nu interval, including nu one. **Open:** a uniform large-nu likelihood
remainder that also controls optimized/held moments and observation-at-uniform-
endpoint paths, and quantitative root inclusion in free shape. A generic
gamma-ratio density limit is insufficient at those endpoints. Prioritize the
symmetric free-shape problem before the larger SGED profile.

**23. sged — `mean,sd,nu,xi`.** Every free set containing nu or xi—12
sets—retains an uncertified outer shape/skew search. In addition,
`F={sd}` is uncertified for held asymmetric xi and held `0<nu<1`.
Free location with both shapes held, `{mean}` and `{mean,sd}`, is already
certified for every positive held nu; the remaining scale-only cases
(`nu>=1` or `xi=1`) are certified. For asymmetric subunit shape scale-only,
partition inverse scale at every shifted-residual zero and use the coercive
scalar bounds below; this is an inexpensive isolated follow-up. Held nu two
reduces exactly to snorm, and held xi one to GED, but a mathematical identity
is not an automatic certificate unless the implementation dispatches to it.
With fixed general nu and free skew, use the two-half-scale objective and
the actual moment constraints; concavity in a fixed skew does not establish
concavity after profiling skew. With free nu, the uniform and half-GED
corners intersect. **Open:** uniform joint tail bounds for those corners,
their reachable endpoint penalties, and global constrained half-GED profilers.
The [SGED certificate report](2026-11-06-sged-cert-report.md) remains the
canonical record of what is already implemented.

**24. huber — `mu,sigma,epsilon=c`.** The four outstanding sets are
`{c}`, `{mu,c}`, `{sigma,c}`, `{mu,sigma,c}`. Held epsilon gives a
concave location/inverse-scale fit, with possible flat median intervals.
Its existing small-c Laplace envelope and large-c Gaussian-core argument
already bound the relevant outer interval. For free scale the small-c
improvement is at most `n*c^2/2`; with held scale use the uniform density
envelope instead. Above the point where the conditional normal solution
lies in the quadratic core, it solves the inner concave problem and the
normalizer is monotone toward the normal face. On the remaining compact
interval, use interval derivative/curvature bounds and subdivision across
residual joins. For `{c}` alone the residuals and all joins are explicit;
start there, then certify the refitted inner problem for the other three
sets. The present total log-shape score gate is useful but cannot replace
global bounds. A lower-tail value envelope licenses an error bound or a
tie stop, not an exact declaration that Laplace wins.

**25. emg — `mu,sigma,lambda`.** Other than the strictly log-concave
location-only subset `{mu}`, all supported nonempty free subsets retain
outer numerical searches: `{sigma}`, `{lambda}`, `{mu,sigma}`,
`{mu,lambda}`, `{sigma,lambda}`, all three. Even `{mu}` needs a quantitative
root/coordinate acceptance check. Fixed *dimensionless* shape
`k=lambda*sigma` makes location/inverse-scale strictly concave; holding
physical lambda does not hold k. Profile that concave inner problem, impose
public holds as constraints and bound intervals of k. Use the current
normal and translated-exponential faces retaining the actual holds; holding
lambda removes the normal escape, holding sigma removes the exponential
escape. **Open:** uniform k-tail likelihood bounds with optimized nuisance
coordinates, including the `1/2` endpoint density when mu is held exactly
at an observation. Fixed physical rate/shape curves are not affine slices
of the fixed-k concavity theorem. The
[log-concavity derivation](2026-10-31-logconcave-report.md) and
[retained implementation report](2026-10-31-logconcave-4-report.md) should
both be reused; the former's rejected trial is not the current fitter.

**26. bhatt — `mu,sigma,a`.** Other than `{mu}`, all six remaining
nonempty free sets have outer numerical searches, with exact exceptions
when held `a=0` reduces to normal. `{mu}` itself is strictly concave and
needs quantitative root acceptance. Fixed `k=a/sigma` gives the same
concave inner coordinates as EMG, but a held physical a traces a different
curve. The normal case `a=0` is attained, whereas sigma down to zero is an
unattained uniform face. Existing density bounds `f<=1/(2*a)` and
`f<=1/(sqrt(2*pi)*sigma)` bound large scales on every positive likelihood
superlevel set. Held sigma removes the uniform face and guarantees
attainment; held positive a does not establish uniqueness of sigma.
Use compact scale/location subdivision after bounding the remaining
collapse neighborhood. At fixed a and free mu, range at most `2*a` has the
proved uniform winner, including the optimized two-endpoint probability
penalty at equality. At held mu the endpoints instead contribute `1/2`
each. **Open:** quantitative bounds for the small-sigma neighborhood in the
general/free-width and partly held problems. The uniform boundary can be a
local maximum without being global; this excludes a single descent-based
search as a general route.

**27. tnorm — `mean,sd,a,b`.** All 12 subsets containing at least one
free parent parameter have an acceptance gap; there is no unresolved
multimodal global landscape. Every free bound profiles exactly to a sample
extreme. The three nonempty sets consisting only of free bounds require
no numerical optimization after that step. For nonempty resulting support,
natural coordinates `(mean/sd^2,-1/(2*sd^2))` give strict concavity. Holding
mean or sd gives an affine restriction. The dossier already supplies exact
moment-interior, exponential/uniform-boundary and collapse classifications,
including unbounded intervals. Use monotone score brackets for one free
parent coordinate, and two-dimensional root inclusion/concave tangent
upper bounds for both. Carry quadrature error into moment/score comparisons
and distinguish equality at a face from a finite fit. Floating-point
quadrature is not itself disqualifying under the package standard; an
explicit propagated error allowance is what is missing. Coincident profiled
bounds are invalid for a continuous family. Full untruncated normal is an
exact special route and should bypass this machinery.

### Extreme-value families

**28. frechet — `lambda,mu,sigma`.** Every mu-held subset reduces to
inverse Weibull and is already in the certified scope. The four outstanding
free-location sets are `{mu}`, `{mu,lambda}`, `{mu,sigma}` and all three.
`{mu}` actually has a unique score crossing by the ratio monotonicity proved
in its dossier; replace the mesh with that monotone equation. For
`{mu,sigma}` at fixed lambda, the explicit likelihood bounds below confine
the location gap whenever `lambda*(n-k)>k`, with k the minimum multiplicity.
Then certify all stationary candidates by scalar bounds. At equality or
negative exponent, the dossier's global derivative bound already decides
the fixed-shape collapse case. With lambda free, the threshold/spike bound
lambda at least one applies; fixed mu exempts it. Use GEV coordinates for
the all-free route, but retain free-location/free-shape face constraints.
**Open:** uniform joint location-gap/shape neighborhoods for the Gumbel face
and collapse equality. The existing fixed-shape equality theorem does not
settle the shape-free equality problem.

**29. gev — `mu,sigma,xi`.** Generically all seven free subsets retain
numerical scalar/root-discovery routes. Exact exceptions are held xi minus
one (reverse exponential), and location-only at held xi zero (Gumbel).
There is a larger short-proof route: for **held `-1<xi<=0` and sigma**,
the location likelihood is strictly concave, with an explicit finite
individual-mode bracket below. For positive held xi the same bracket
confines every location root, though concavity need not hold. This already
settles the domain needed for a complete scalar subdivision on those
subsets. For negative xi the support endpoint must also be included according
to the declared closure. For held mu or free xi, profile the certified scalar
subproblems and compare xi minus one, zero and one and the applicable collapse
faces. Free xi is compact in `[-1,1]`, but nuisance coordinates are not.
**Open:** effective *uniform* nuisance bounds and face-neighborhood remainders
for free shape, in particular the positive-shape half-tie equality. The
Frechet-to-GEV map preserves the all-free likelihood but not arbitrary
public holds: GEV location is `mu_F+sigma_F` and its scale is
`sigma_F/lambda`. A certificate under a held GEV coefficient cannot silently
use the unconstrained Frechet fit.

## Short proofs and explicit search bounds

These are exact-arithmetic results. The accompanying deterministic checks
verify formulas and examples at 70 decimal digits; those checks are not the
proofs and do not validate a new C++ implementation.

### ZIP: finite bracket and at most three stationary rates

Let n be sample size, z the zero count, m=n-z>0, S=sum(x)>0 and
`a=pi/(1-pi)>0`. With inflation held,

\[
 s(t)=\ell'(t)=S/t-m-\frac{z}{1+ae^t}.
\]

Every root lies in `[S/n,S/m]`: below the first endpoint the score is
positive, above the second it is negative. Moreover

\[
 t^2s'(t)=-S+zH(t),\quad
 H(t)=\frac{t^2ae^t}{(1+ae^t)^2},\quad
 (\log H)''=-2/t^2-2p_t(1-p_t)<0,
 \qquad p_t=\frac{ae^t}{1+ae^t}.
\]

The log derivative of H decreases from positive infinity to minus one;
H has one maximum and tends to zero at both ends. Thus `s'` has at most
two roots, and s at most three. Bracket the H peak and its level crossings,
then isolate s on each monotone piece and compare all maxima. Tangencies
need an inclusion/tie outcome, not a sign-change scan. Alternatively, the
existing ZINB helper rewrites the stationary equation as

\[
 h(t)=-t+\log(nt-S)-\log(S-mt)-\log a=0.
\]

Its derivative vanishes at roots of the explicit quadratic
`n*m*t^2-S*(n+m)*t+S^2+S*(n-m)=0`. The existing implementation isolates
increasing h branches, which correspond to likelihood maxima. This is the
preferred reusable implementation, and the H argument independently explains
why a finite root count is available.

### Fatigue: multiple conditional maxima and rational enumeration

Shift by held threshold and write positive data as x. With alpha fixed,

\[
 \ell'(b)=\sum_i\frac1{x_i+b}-\frac n{2b}
        +\frac{\sum x_i}{2\alpha^2b^2}
        -\frac{\sum x_i^{-1}}{2\alpha^2}.
\]

Multiplication by the positive denominator
`2*alpha^2*b^2*product(x_i+b)` produces a polynomial of degree at most
`n+2`. All positive stationary roots must be compared. Each individual
log-density, in `u=log(b/x)`, has derivative
`tanh(u/2)/2-sinh(u)/alpha^2`. It has roots `u=0` and, if alpha exceeds
two, `b/x=r_-,r_+`, where

\[
 r_\pm=\{\alpha^2-2\pm\alpha\sqrt{\alpha^2-4}\}/2.
\]

The score points inward outside
`[min(x)*r_-,max(x)*r_+]`; for alpha at most two use `r_-=r_+=1`.
This finite bracket also supports a rational bound search without polynomial
expansion. For `x={1}`, `alpha=3`, beta one is a local minimum and
`beta=(7±3*sqrt(5))/2` are two equal global maxima. The checked likelihood
gain is approximately `0.1276873303303866`. This disproves the fit note's
monotone-held-alpha assertion.

With alpha profiled, put arithmetic mean A, harmonic mean H,
`D=product(x_i+b)` and `C=sum_i product_{j!=i}(x_j+b)`.
The score has the sign of

\[
 F(b)=b^2-2Hb+HA+(H-b)\,nD/C.
\]

For nonconstant data `F>0` at and below H. Also `nD/C>=b+H`:
the derivative of the harmonic mean of `x_i+b` is at least one by
Cauchy–Schwarz, and its value at zero is H. Consequently for `b>=A`,
`F(b)<=H*(A+H-2*b)<0`. Every global maximum therefore lies in `[H,A]`.
Multiplying F by positive C gives another degree-at-most-`n+2` polynomial.
The profiled alpha is strictly positive on this interval for nonconstant
data, so roots plus interval endpoints give a complete finite candidate
set. This proves a certification route without assuming root uniqueness.
Constant data instead have the documented unbounded joint fit.

### Generalized beta: held beta shapes give strict power concavity

For `y_i=log(b/x_i)>0`, held p,q give, up to a constant,

\[
 L(a)=n\log a-pa\sum y_i+(q-1)\sum_i\log(1-e^{-ay_i}),
\quad
 L''(a)=-\frac n{a^2}-(q-1)\sum_i
 \frac{y_i^2e^{ay_i}}{(e^{ay_i}-1)^2}.
\]

For q at least one this is negative immediately. For `0<q<1`, use
`t^2*e^t/(e^t-1)^2=(t/(2*sinh(t/2)))^2<1`. Then
`L''(a)<-n*q/a^2<0`. The score tends to positive infinity at zero
and to `-p*sum(y_i)<0` at infinity. There is exactly one finite root
and global maximum, even for constant data below b. Adaptive signed
bracketing and bisection completely replace the power mesh in this subset.

### Retained support: a common all-size upper bound

For N at least K, set
`c_N(k)=sum_{j=0}^{k-1} log(1-j/N)` and
`lambda=N*p/(1-p)`. The binomial kernel is
`lambda^k/k! * exp(c_N(k))`. On any conditioning support whose lower
integer endpoint is L, retain only `L,...,K`, with K at least the largest
observation. The full normalizer is at least
`exp(c_N(K))*sum_{k=L}^K lambda^k/k!`. Therefore

\[
 \ell_N(\lambda)\le T_K(\lambda)+D_{N,K},\qquad
 D_{N,K}=\sum_i\{c_N(x_i)-c_N(K)\}\downarrow0.
\]

Here T_K is the log likelihood of a Poisson kernel conditioned on the
*retained finite support*, with the same data. Its supremum is a scalar
concave mean-matching problem. The bound holds for an originally infinite
upper window as well as a finite one; this extends the finite-window
argument already in the repository. For N at least a proposed cutoff,
both `sup(T_K)` and the monotone correction bound every larger size.
Increasing K approaches the appropriate truncated-Poisson supremum when
that supremum is finite. A strict finite improvement can thus produce a
finite cutoff. All smaller feasible sizes still require exclusion or
evaluation. If the face ties or wins, this unsigned bound alone cannot
certify its superiority.

For ZIB retain `0,...,K`. The base zero mass is at most
`exp(-c_N(K))/Z_K`. Since `exp(-c_N(K))>=1`, its inflated zero mass is
at most `exp(-c_N(K))*(pi+(1-pi)/Z_K)`. Positive masses have their
corresponding `exp(c_N(x)-c_N(K))` bound. The same D thus bounds the
total ZIB likelihood by the finite-support inflated-Poisson likelihood,
with inflation held or maximized as appropriate. At held inflation its
rate score is rational in lambda after multiplying by the positive finite
normalizer; polynomial root isolation is available. This avoids imposing
unproved global unimodality on either size profile. Tightness/rate bounds
as K increases, and signed winning-face comparisons, remain implementation
and analysis work; no new fitter is claimed here.

### Shifted Gompertz: a complete incumbent-based compact search

For a nonconstant nonnegative sample let `m=min(x)`, `M=max(x)>0` and
`G=sum(x)-n*m>0`. The dossier gives, uniformly in eta,

\[
 \ell\le n\log b+n\log n-bG,
 \qquad
 \ell\le n\log b+n\quad\hbox{if }bM\le1.
\]

A finite incumbent B excludes sufficiently large b using the first
decreasing envelope, and excludes
`b<min(1/M,exp(B/n-1))` using the second. Choose strict comparisons
and expand the upper bound only beyond `n/G`, where it is decreasing.
At fixed b, the eta score is
`-sum(exp(-b*x))+sum((1-exp(-b*x))/(1+eta*(1-exp(-b*x))))`.
It is negative when `eta>n/sum(exp(-b*x))`. On the retained rate interval
`b<=b_hi`, this is at most `n*exp(b_hi*m)`. Hence every joint maximizer
is in the explicit rectangle
`b_lo<=b<=b_hi`, `0<=eta<=n*exp(b_hi*m)`. Use log eta or overflow
refusal when necessary; infinity is not a fitted cap. Continuous compact
upper bounds/subdivision now give a complete search.

At held eta, the second derivative in b is

\[
 -n/b^2-\eta\sum_i x_i^2e^{-bx_i}
 -\sum_i\frac{x_i^2\eta(1+\eta)e^{-bx_i}}
 {(1+\eta-\eta e^{-bx_i})^2}<0.
\]

For a sample with a positive observation the score has opposite signs at
zero and infinity, giving a unique finite rate root. All-zero data with
rate free are unbounded. These conditional statements do not assert joint
concavity after eta is profiled.

### Fréchet: fixed-shape location-gap tails

Let `epsilon=min(x)-mu>0`, `g_i=x_i-min(x)`, range R, minimum
multiplicity k and held shape lambda. After profiling scale,

\[
 \ell=n\log(n\lambda)-n
       -n\log\sum_i(g_i+\epsilon)^{-\lambda}
       -(\lambda+1)\sum_i\log(g_i+\epsilon).
\]

With `E=lambda*(n-k)-k`, this is at most

\[
 C+E\log\epsilon,\quad
 C=n\log(n\lambda)-n-n\log k
       -(\lambda+1)\sum_{g_i>0}\log g_i.
\]

When E is positive this excludes a whole small-gap neighborhood using any
finite incumbent. At the other end it is at most
`n*log(lambda)-n+n*lambda*log(epsilon+R)-n*(lambda+1)*log(epsilon)`.
That expression strictly decreases to minus infinity. Thus every potentially
winning fixed-shape profile root lies in an explicitly bracketable compact
gap interval. E zero/negative requires the separate collapse decisions,
not this small-gap exclusion.

### GEV: fixed-scale location curvature and bracket

At held xi not zero and held positive sigma put
`t_i=1+xi*(x_i-mu)/sigma>0`. Direct differentiation gives

\[
 \ell_\mu=\sigma^{-1}\sum_i t_i^{-1}
       \{1+\xi-t_i^{-1/\xi}\},\qquad
 \ell_{\mu\mu}=\frac{1+\xi}{\sigma^2}
       \sum_i t_i^{-2}\{\xi-t_i^{-1/\xi}\}.
\]

The curvature is strictly negative for `-1<xi<0`; the Gumbel limit
xi zero is also strictly concave. For every xi greater than minus one,
each individual score changes sign at
`mu_i=x_i+sigma*(1-(1+xi)^(-xi))/xi`, from positive to negative.
Every stationary location therefore lies between the smallest and largest
of these values, intersected with the support domain. For negative xi,
the finite support boundary must also be considered; strict concavity still
holds in the interior. Xi minus one is the separately attained linear
reverse-exponential case. This yields a scalar global route without claiming
joint GEV concavity.

### SGED: a finite scalar domain for the remaining subunit scale fit

With both shapes and the public mean held, use inverse scale t and the
shifted standardized residuals `a_i*t+d`. Up to known constants its cost is
`F(t)=-n*log(t)+sum_i w_i(t)*abs(a_i*t+d)^nu`, with one of two positive
side weights on each residual-sign cell. Knots `t=-d/a_i>0` are known.
At zero, `F>=-n*log(t)+constant`, excluding a small interval with an
incumbent. If some `a_j!=0`, then beyond `2*abs(d/a_j)`,
`F>=-n*log(t)+C*t^nu+constant`, with
`C=min(side_weights)*(abs(a_j)/2)^nu>0`. This lower envelope increases
once `t^nu>n/(C*nu)` and diverges. It excludes all sufficiently large t.
On the resulting compact interval, termwise minimum/maximum residual
distances give convergent cost bounds, including the finitely many cusp
knots. Branch-and-bound therefore certifies the global value, with ties
or unresolved coefficient inclusion stopped explicitly. All `a_i=0` is
the already classified collapse. No assertion of unimodality below shape
one is needed.

## Ranking and implementation jobs

Effort estimates concern a reviewed implementation **with regression,
independent bounds/face fixtures and dossier updates**, not just writing an
optimizer. S is roughly one or two focused working days, M three to five,
L one to two weeks, and XL an open research dependency rather than a promised
duration. Value includes ordinary defaults, held-subset coverage and reuse;
it is not a measured usage ranking.

| Priority | Families | Value and effort | Concrete job boundary |
|---|---|---|---|
| 1 | zip | High reuse as ZIB/ZINB face; S–M | **J1:** Extract existing ZINB ZIP root certificate, add ZIP acceptance and near-turning-point/tied-mode fixtures. Preserve current free-fit proposal and goldens. |
| 2 | fatigue, sgomp, genbeta | Three short scalar closures; M | **J2:** Held-alpha fatigue rational enumeration, profiled fatigue bracket, held-eta Gompertz root, genbeta power-only monotone root. Use sgomp's compact joint bounds as a separate second increment. |
| 3 | sn | Important skew-t face dependency; M | **J3a:** Quantitative concave-inner and held-location root acceptance; correct the held-scope note. Reuse existing outer certificate. |
| 4 | snorm | All-free proof already exists; M–L | **J3b:** Replace non-unit held-shape nested minimization; then constrained algebraic held-moment/skew enumeration and half-face factors. |
| 5 | lst, std | Core held fits and existing board task; M–L | **J4 / TASK-116.13:** Fixed-shape nuisance inclusion, rational held-scale location enumeration, then actual-hold shape interval/tail certificates. Separate df-below-one work. |
| 6 | trgamma, invtrgamma | Two defaults, reusable profile bounds; L | **J5:** Close small-power normal seam and coefficient inclusion; transport every bound through reciprocal data. Add held-scale endpoint fixtures. |
| 7 | tnorm, cmb, cmp | Mostly acceptance rather than landscape research; M–L | **J6a:** Finite-support concave dual/bracket acceptance for tnorm and fixed-size CMB. **J6b:** CMP moment-error wrapper; authorize shared-special changes explicitly if needed. |
| 8 | ztbinom, zmbinom, tbinom, zib | Four families share size machinery; L | **J7a:** Mandatory finite-winner retained-support bounds and complete finite prefix/block coverage, using J1 for ZIB faces. **J7b:** Signed Poisson tails and winning-face/tie decisions. Do not combine both into an unbounded first job. |
| 9 | huber | One bounded scalar outer search; M–L | **J8:** Fixed nuisance epsilon certificate first, then concave profiled nuisance bounds; retain both normal and Laplace witnesses. |
| 10 | ged, sged | Symmetric route and a scalar low-cost exception; L/XL | **J9a:** SGED asymmetric subunit scale-only compact certificate. **J9b:** GED free-shape/uniform-tail certificate. **J9c:** SGED fixed-shape free-skew constraints, then joint shape/skew corners as research. |
| 11 | frechet, gev | Useful exact scalar wins, difficult joint equality; M/XL | **J10a:** Monotone Frechet location, bounded fixed-shape profile, nonpositive GEV fixed-shape location. **J10b:** Joint free-shape endpoint/Gumbel and half-tie neighborhoods. |
| 12 | betapr, gb2, fpareto | Three heavy-tail families; L/XL | **J11a:** Non-unit beta-prime one-held-shape profiles and gamma tails. **J11b:** GB2 outer-shape superlevel/closure research; transport to Feller–Pareto after parent success. |
| 13 | emg, bhatt | Shared inner geometry but different physical faces; L | **J12a/J12b:** Independent EMG and Bhattacharjee tail-neighborhood jobs sharing the concave-inner acceptance helper. Avoid coupling their unresolved endpoint analyses into one implementation. |
| 14 | bnbinom | Scalar certificates already done; L/XL | **J13a:** Effective compact bounds and all modes with alpha held; **J13b:** Remaining pairs/all-free face neighborhoods. Retain symmetry and singular-diagonal witnesses. |
| 15 | bbinom | Missing structural free-size feature; XL | **J14 / TASK-113:** Uniform joint alpha/mean/size closure bound before implementing the unsupported three-free fit. |

This assigns every partial family once to a job group, while splitting
independent long pieces. Existing TASK-116.11/.13/.16, TASK-113, TASK-115
and the research for TASK-116.14/.17 must be coordinated through the user's
orchestrators; no board record is created here. The
[skew-theory report](2026-11-07-skew-theory-report.md) already plans sstd/st;
those `no` families should not be silently added to this partial-family job.

For every implementation: proposals may retain historical arithmetic and
default goldens, but certificate failure is terminal. Never fall back to a
mesh success after an exhausted certificate. Use solver caps
`min(iterlim,100000)`, certificate caps `128*min(iterlim,100000)` and
enumeration caps `100000*min(iterlim,100000)`, charging nested solves
separately. Coefficient enclosure, likelihood enclosure and root-count
completeness are distinct checks. Unresolved signs, multiple roots, seams,
equal maxima and nearly coincident faces need explicit tie/numerical/budget
outcomes. A positive fixed gap can permit finite certification without
promising termination at an exact tie.

## Metadata corrections and remaining open decisions

No dossier was changed. The concrete corrections for authorized follow-ups are:

1. **Fatigue note is inaccurate:** held alpha does not imply a monotone scale
   solve; the exact singleton counterexample above has two maxima. The joint
   default is a profiled Newton/bracket route, not the held-alpha grid.
2. **Snorm note is inaccurate in scope:** all-free skewness already has the
   algebraic global mode comparison. Restrict its finite-scan qualification
   to the held-moment route, and state the held-shape numerical acceptance gap.
3. **Sn note is too broad about held location:** the code already separates
   scale and `alpha/omega` and uses a monotone root. The remaining issue is
   quantitative acceptance, not missing discovery of separated shape modes.
4. **Tbinom note understates existing scope:** a completed optional finite-
   window pass can certify a strict finite winner. It must also say that
   optional-pass failure returns an uncertified successful proposal.
5. **CMP method/note is misleading:** the production shared fitter uses mean
   matching and monotone dispersion profiling. Calling it multistart or citing
   fixed-design integer tails obscures the actual unresolved acceptance gap.
6. **CMB partial scope should identify ordinary fixed-size residual solves:**
   accepted free-size bounds already exist. Unsupported regional cases and
   conservative unresolved-face stops should be listed separately.
7. **Tnorm's note should name the missing propagated quadrature/root bounds:**
   using floating-point arithmetic alone is not a reason to withhold a
   conditional certificate under the package standard.
8. **Genbeta's power-only theorem and sgomp's held-shape concavity deserve
   explicit scope once used.** Their current generic mesh notes accurately
   describe implementation but leave inexpensive mathematical routes hidden.

No family-wide `partial` label can responsibly be promoted to `yes` solely
from this review. Several are conservative because of acceptance gaps rather
than unknown global geometry; bbinom also has an unsupported structural
combination. None should be downgraded merely because computed comparisons
are not outward-rounded intervals. Structural threshold/endpoint exclusions
in fatigue, Feller–Pareto and genbeta are separate interface decisions:
their value-dependent existence examples do not authorize implementing them
or rewriting R5/R6/A7 in a certification job.

The open mathematics is specifically: signed binomial/Poisson-tail decisions;
uniform joint shape bounds for bbinom and GB2; effective bnbinom cutoffs;
trgamma's refitted normal seam; GED/SGED uniform and half-family corners;
EMG/Bhattacharjee smoothing neighborhoods; and joint extreme-value
shape/endpoint equality. Each is tied above to a failed or nonuniform
candidate argument and a concrete next inequality, rather than a request
for a larger numerical grid. General uniqueness is not required: complete
bounded searches with preserved ties are valid alternatives.

No settled dpqr, truth, generated d/p/q/r or codegen change is required by
these routes. Only CMP may require an explicitly authorized shared-special
moment-error extension; its current seam remains untouched here. New generic
core/binding changes would trigger JS build/test checks in their own jobs;
public-fitter moves and dossier changes trigger site-check there.

## Literature use and its limits

The primary [Noufaily–Jones author manuscript](https://university.open.ac.uk/stem/mathematics-and-statistics/sites/www.open.ac.uk.stem.mathematics-and-statistics/files/files/Onthemaximisation.pdf),
sections 2.2–2.4, supplies uniquely solvable conditional equations and
parameter inequalities for generalized gamma. It explicitly withholds
algorithm-convergence and joint-uniqueness guarantees. It therefore supports
inner reductions, not replacing a global certificate by repeated successful
optimization. The repository's standardized-power conjugate bounds are a
more direct implementation starting point.

The [1983 ordinary-binomial publisher abstract](https://www.tandfonline.com/doi/abs/10.1080/01621459.1983.10477949)
states an ordinary-binomial result; this does not certify the truncated or
inflated profiles. Attempts to retrieve the primary 1994 positive-binomial
paper and its 1996 correction returned HTTP 403. Their full statements and
correction therefore remain unchecked here, as in the existing size-search
report. No integer-profile counterexample or general positive-binomial
unimodality theorem is claimed from them. The proposed retained-support
route does not depend on either. Fatigue's scalar and joint enumeration
routes above are directly derived from the repository density and do not
depend on importing an unchecked classical uniqueness theorem.

## Finished validation and evidence

All research checks and the required dev suites finished successfully before
this report was written. Source/header fingerprints are in the
[manifest](2026-11-07-partial-triage-data/source-manifest.json); the
[check program](2026-11-07-partial-triage-data/check.py) reproduces the
inventory and derivation checks, and
[checks.json](2026-11-07-partial-triage-data/checks.json) records exact grids,
versions, precision and numerical errors. There are no random workloads.
The [validation record](2026-11-07-partial-triage-data/validation.json) links
the complete compressed logs and records the final source/link checks.

| Final command | Machine | Result |
|---|---|---|
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-partial-triage-data/check.py` | Workstation | Passed: 29-family/251-subset census; ZIP root-count identities; fatigue multiple maxima/polynomial identity; 36 generalized-beta curvature cases; retained-support inequalities; GEV curvature and Fréchet exterior inequalities. |
| `/tmp/umvl-cpu/simbox-run just cpp-test` | simbox | 106/106 dev shards passed; ctest elapsed 30.30 s. |
| `/tmp/umvl-cpu/simbox-run just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20983; test runner elapsed 60.169 s. |

Local research used Python 3.14.5, mpmath 1.3.0, PyYAML 6.0.3,
70-digit arithmetic, Ubuntu 26.04.1, with the required single-slot wrapper.
Simbox's wrapper supplied nice/thread limits; no exit-75 fallback was needed.
Its standalone C++ build used GCC 13.3.0; the R build used R 4.6.1 and
`g++-15`. The first CMake system-Boost discovery failed; the Justfile's
existing BH-header fallback then configured and completed successfully.
The build emitted existing range-loop-copy, indentation and inline-declaration
warnings, and R regeneration emitted documentation notices; these are not
test failures. R's 154 skips are declared dev-tier exclusions, not
permitted failures. Compressed complete suite logs and command/environment
details are retained in the evidence directory.

The numerical derivative-identity discrepancies were below `2.4e-66`, tested
against `1e-60`. Fatigue's conditional equal maxima agreed within `1e-60`.
The retained-support and extreme-tail checks used direct inequality tests;
the proof inequalities, rather than those finite grids, justify the routes.
No new fitting code was exercised or certified by this research program.
The dev suites establish regression health of the unchanged source, not
global correctness over the listed free subsets.

Full R/C++, no-long-double, JS and site suites were not required by this
research-only change: no fitting, dpqr, test infrastructure, shared core,
binding, dossier text/front matter or public fitter signature changed.
No golden/reference/truth regeneration was performed. No command started by
this job remained running when the report was written.

The artifact-pane `show` command was attempted after writing the report.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. No viewer configuration or file outside this worktree was
changed to bypass that restriction; the report remains available at its
direct local link.
