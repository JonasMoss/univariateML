# Pathological families for the outcome type (consolidation step 7)

Design note, 2026-11-14. No code. Inputs: [consolidation plan, step 7](../design/consolidation.md),
[decision of 2026-11-12](../roadmap/decisions.md) ("Pathological families use the shared
outcome type; no mixtures for now"), [`core/outcome.hpp`](../../univariateML-cpp/include/cppunivariate/core/outcome.hpp),
[relaxed-likelihood.tex](relaxed-likelihood.tex) (Examples `ex:sine`, `ex:ferguson`).

Evidence labels: **proved here** (argument given below), **lit, verified** (checked in the
source text on 2026-11-14), **lit, secondary** (stated by a secondary source, original not
read), **numerical** (mpmath or binary64 computation, illustrative only), **conjecture**.

## Summary

1. "The sine family" is defined in the repository: [relaxed-likelihood.tex](relaxed-likelihood.tex),
   Example `ex:sine`, $f_\theta(x)=1+\sin(2\pi\theta x)$ on $[-\frac12,\frac12]$. Step 7 and
   the decision refer to it. No published source was found for it, either in the 2026-11-10
   literature report or in five searches today. Its published analogue is Le Cam's dyadic
   example (1990, §4).
2. The plan's "(no maximizer for any sample)" is not true as written. For real data the
   MLE fails to exist for almost every sample with $n\ge2$; with $n=1$ there are countably
   many maximizers. **Every binary64 sample is rational, and on such a sample the
   sine likelihood is periodic in $\theta$, so maximizers exist in exact arithmetic**. The
   period is about $2^{54}$ for data of order one and far larger for tiny data. Under the
   library's exact-input semantics, a fitter for
   the literal sine family cannot honestly return "nonexistence".
3. Fix (recommended): fit the same model observed in the coordinate $x=-\log(u+\frac12)$.
   This is the family `sinexp`, $e^{-x}\{1+\sin(2\pi\theta(e^{-x}-\frac12))\}$ on
   $[0,\infty)$. By Lindemann–Weierstrass plus Kronecker, every sample with at least two
   distinct values has no maximizer: the supremum is finite and not attained. This is
   exact for binary64 inputs and holds for every real sample with two distinct algebraic
   values. Outcome: `Nonexistence`, reason `Unattained`, with no limit model, even
   though the maximizing path converges weakly to a member.
4. Second family (recommended): `ssqnorm`, the signed square of a normal,
   $X=\mu+\sigma Z\lvert Z\rvert$. Its density has a pole at $\mu$, so the likelihood is
   unbounded for every sample whenever $\mu$ is free. Outcome: `Nonexistence`,
   reason `Unbounded`, exact. With $\mu$ held it has a closed-form, certified estimate.
5. Ferguson's example is not a step-7 family. Every fit is an ordinary estimate, so the
   outcome type has nothing to test. Its inconsistency belongs in the paper and in prose,
   and it can become an ordinary family later if a site page is wanted.
6. Decisions for Jonas are collected in [§5](#5-decisions-and-open-questions).

## 1. The sine family

**Definition.** $f_\theta(x)=1+\sin(2\pi\theta x)$ on $[-\frac12,\frac12]$. It is a density
for every real $\theta$, because the sine is odd. Since $f_{-\theta}(x)=f_\theta(-x)$, take
$\Theta=[0,\infty)$; $\theta=0$ is the uniform. Write
$L(\theta)=\prod_i\{1+\sin(2\pi\theta x_i)\}\le2^n$.

- **$n=1$, $x\ne0$** (**proved here**). $L=2$ exactly at $\theta_k=(k+\frac14)/x$ for
  $x>0$, or $\theta_k=(k+\frac34)/\lvert x\rvert$ for $x<0$, with $k=0,1,\dots$. There
  are countably many maximizers. At $x=0$, $L\equiv1$ and every $\theta$ is a maximizer.
- **$n\ge2$, real data in general position** (**proved here**, as in `ex:sine`). If the
  nonzero $x_i$ are linearly independent over $\mathbb Q$, the half-orbit
  $\{\theta x \bmod 1:\theta>T\}$ is dense in the torus for every $T$ (continuous Kronecker
  theorem; Hardy and Wright, ch. XXIII). Hence $\sup L=2^n$. Attaining it needs
  $\theta x_i\equiv\frac14 \pmod 1$ for all $i$, so every ratio $x_i/x_j$ would be
  rational. The supremum is therefore not attained. On $[0,T]$, $L$ is continuous and
  below $2^n$, so maximizing sequences diverge. Rational independence fails only on a
  Lebesgue-null set of samples.
- **Escape.** As $\theta\to\infty$, $P_\theta\Rightarrow U$ (Riemann–Lebesgue), but
  $L(U)=1<2^n$. The relaxed likelihood at $U$ is $2^n$ (excess, row R3 of the regime
  table). There is no limit model to name. "Use `mlunif`" would be wrong, and so would any
  escape detector that only follows where $\theta$ goes.
- **Outcome for real data, $n\ge2$:** `Nonexistence`, reason `Unattained`. For $n=1$:
  `Estimate` with `unique = false`.

### 1.1 The literal sine family on binary64 data

**Lemma** (**proved here**). Let $x_1,\dots,x_n$ be binary64 numbers, not all zero. Write
$x_i=a_i2^e$ with integers $a_i$, let $g=\gcd\lvert a_i\rvert$ over the nonzero $a_i$, and
put $P=2^{-e}/g$. Then $L(\theta+P)=L(\theta)$ for every $\theta$. So $L$ attains its
supremum on $[0,P]$; a maximum at $0$ recurs at $P$. Maximizers exist for every binary64
sample, and the set of maximizers is periodic.

**Numerical** (mpmath, 60 digits):
- For $x=(0.25,0.3)$, $P=2^{54}$, and $L(\theta)=L(\theta+P)$ checked at $\theta=1.2345$.
- A seeded uniform sample of size 5 has $P=2^{53}$.
- On coarse dyadic data the supremum $2^n$ is attained at small $\theta$: $x=(1/16,5/16)$
  gives $L(4)=4$.

On generic binary64 data, the exact maximizer lies far beyond the range where a binary64
$\theta$ resolves the phase. It moves when any datum changes by one ulp, so it is an
artifact of rounding.

Consequences for a fitter of the literal family:

- **S1, exact-input semantics** (as for every other family). For $n\ge2$ the true answer is
  `Estimate`, `unique = false`. Finding it means a global search over a period of about
  $2^{54}$ in $\theta$, which amounts to inhomogeneous simultaneous Diophantine
  approximation. The fitter would end in `Budget` on nearly every input, carrying the best
  witness and the bound $2^n$, and in a certified non-unique estimate on coarse grids.
  This is honest but teaches the wrong lesson.
- **S2, real-sample semantics** for this one family. The claim would be: "for almost every
  real sample that rounds to the input, no maximizer exists, and $\sup L=2^n$". This is a
  theorem for every input with $n\ge2$, because each rounding box has positive volume.
  But it departs from the convention elsewhere: constant normal data and Cauchy
  semicircle ties are decided exactly, although they are null events too.
- **S3, change coordinates** (recommended, §1.2). The statistical model is unchanged; only
  the coordinate in which the data are exact changes. Exact semantics and real-sample
  semantics then agree.

### 1.2 The sine family in the exponential coordinate (`sinexp`)

**Definition.** Let $V=U+\frac12$, where $U$ follows the sine family. Put $X=-\log V$:

$$
f_\theta(x)=e^{-x}\Bigl\{1+\sin\bigl(2\pi\theta(e^{-x}-\tfrac12)\bigr)\Bigr\},\qquad x\in[0,\infty),\ \theta\in[0,\infty).
$$

$\theta=0$ is $\mathrm{Exp}(1)$. Substituting $v=e^{-x}$ shows normalization
(**numerical** check: 1 to 30 digits at $\theta=3.7$). Likelihood ratios equal those of the
sine family at $u_i=e^{-x_i}-\frac12$, so the MLE problem in $\theta$ is literally the same.

**Theorem** (**proved here**). Let $x_1,\dots,x_n\ge0$ be algebraic, including every
binary64 value, with at least two distinct values. Then
$\sup_\theta L(\theta)=2^n e^{-\sum x_i}$, and no $\theta$ attains it.

*Proof.* Let $y_1,\dots,y_m$ be the distinct values, $m\ge2$. By Lindemann–Weierstrass
(Baker 1975, ch. 1), $1,e^{-y_1},\dots,e^{-y_m}$ are linearly independent over the
algebraic numbers when $0,-y_1,\dots,-y_m$ are distinct. If some $y_j=0$, its term is the
1 itself. A relation $\sum_j k_j(e^{-y_j}-\frac12)=0$ with integers $k_j$ rearranges to
$\sum_j k_je^{-y_j}-\frac12\sum_jk_j=0$, which forces every $k_j=0$. So the
$u_j=e^{-y_j}-\frac12$ are rationally independent, and they are nonzero because
$\log 2$ is not algebraic. Then apply the general-position argument of §1, with
multiplicities as exponents. ∎

- **One distinct value** (all observations equal, $n\ge1$). The maximizers are
  $\theta_k=(k+\frac14)/u$ for $u>0$ or $(k+\frac34)/\lvert u\rvert$ for $u<0$, with
  $u=e^{-x}-\frac12\ne0$. This is a countably infinite arithmetic progression.
- **Weak limit.** As $\theta\to\infty$, $P_\theta\Rightarrow\mathrm{Exp}(1)$, the member at
  $\theta=0$. Its likelihood is $2^{-n}$ times the supremum.
- **Numerical illustration.** Sample $(0.25,0.3,1.7)$, $\theta$ grid of step $0.002$:
  the best $\log\prod(1+\sin 2\pi\theta u_i)$ is 1.7069 for $\theta<10^2$, 2.07926 for
  $\theta<10^3$ and 2.079437 for $\theta<10^5$, against $3\log2=2.079442$.

One coefficient only. Adding a rate $\lambda$ makes $u_i=e^{-\lambda x_i}-\frac12$
adjustable. Two free coefficients can hit the peaks exactly for $n\le2$, so the
every-sample statement is lost.

## 2. Other candidates

| candidate | density, support, $\Theta$ | samples without MLE | mechanism, status | outcome | verdict |
|---|---|---|---|---|---|
| sine (`ex:sine`) | §1 | a.e. real sample, $n\ge2$; **no** binary64 sample | excess; proved here | `Unattained` (real data) | motivating example in the `sinexp` dossier |
| `sinexp` | §1.2 | every sample with two distinct values | excess; proved here | `Nonexistence`, `Unattained` | **recommended** |
| Le Cam's dyadic selections (Le Cam 1990, §4) | $1+c(1-e^{-m})\varphi_{s,m}$ on $(0,1]$; $m\ge1$; $s$ a balanced choice of $2^{m-1}$ of the $2^m$ dyadic intervals | every sample: for large $m$, choose $s$ to cover the data | excess; lit, verified ("estimate $m$ by $+\infty$") | `Unattained` | not a library family: $s$ is combinatorial, not a real coefficient |
| dyadic square wave (constructed here) | $1+(1-2^{-m})r_m(x)$ on $[0,1)$, $r_m$ the $m$-th Rademacher function | every binary64 sample (trailing zero digits), a.e. real sample (Borel–Cantelli) | excess; proved here | `Unattained` | no: flip the sign of $r_m$ and every binary64 sample has an MLE; this is a representation artifact |
| open-support uniform | $\theta^{-1}\mathbf 1_{(0,\theta)}$ | every sample | supremum at $\theta=\max x$, excluded only by the density version; folklore (Dudley's 18.466 notes, as summarized by search, not read) | `Estimate` after R7a endpoint closure | no: a version artifact the library already closes |
| `ssqnorm` (signed squared normal) | §4.2; location-scale on $\mathbb R$ | every sample, any $n\ge1$, whenever $\mu$ is free | pole at a member; proved here | `Nonexistence`, `Unbounded` | **recommended** |
| bounded spike (constructed here) | $\sigma^{-1}f((x-\mu)/\sigma)$, $f(z)=\{2(1+\lvert z\rvert)(1+\log(1+\lvert z\rvert))^2\}^{-1}$; closed-form $F$ and $F^{-1}$ | every sample, all-free | $\sigma\to0$ at $\mu=x_1$: $L\asymp\sigma^{-1}(\log 1/\sigma)^{-2(n-1)}\to\infty$; bounded density; proved here | `Unbounded` | alternative to `ssqnorm`: collapse without a pole |
| normal mixtures (Kiefer and Wolfowitz 1956) | free component variances | every sample | spike | `Unbounded` | excluded (no mixtures) |
| three-parameter lognormal (Hill 1963); Weibull or gamma with a location and shape below 1 | thresholds | every sample | atom plus escape, or a pole at the threshold | `Unbounded` | ordinary families with R5 restrictions; pareto3 already refuses a free `min` (`holds: no-mle`) |
| Ferguson (1982) | §3 | none: MLE exists | inconsistency | `Estimate` | §3 |

Which candidate Jonas means: the in-repo sine family, with high confidence. The plan's
wording ("no maximizer for any sample") matches `ex:sine`'s claim, and no other sine
example appears in the repository or in the literature found.

Unsure:
- whether he has a published sine example in mind (none found);
- whether $\theta=0$ belongs to $\Theta$;
- whether he would accept `sinexp` in place of the literal family, or prefer S2.

A variant on $[0,1]$ with $1+\sin(2\pi\theta x)$ is a density only for integer $\theta$.
That version has the same a.e. behaviour (discrete Kronecker) and the same binary64
problem.

## 3. Ferguson's example

$f(x\mid\theta)=\frac{1-\theta}{\delta(\theta)}\bigl(1-\frac{\lvert x-\theta\rvert}{\delta(\theta)}\bigr)^+ +\frac\theta2\mathbf 1_{[-1,1]}(x)$
on $[-1,1]$, $\Theta=[0,1]$, $\delta(\theta)=(1-\theta)\exp\{1-(1-\theta)^{-c}\}$.

- The form is **lit, verified** in Le Cam 1990, pp. 160–161: "everything compact and
  continuous and all of Wald's conditions, except one, are satisfied".
- Ferguson's $c=4$ is **lit, secondary**: Wellner's Stat 582 problem set (2009) states it,
  shows $\hat\theta_n\to1$ a.s. for every $\theta_0$ when $c>2$, and finds a nondegenerate
  limit $\log2+1/(2E)$ of Ferguson's bound at $c=2$.
- The $c=2$ limit matches a triangular truth, where $1-M_n\asymp n^{-1/2}$. For
  $\theta_0>0$, $1-M_n\asymp n^{-1}$, and the repository's sketch puts the threshold at
  $c=1$ (relaxed-likelihood.tex, "Ferguson threshold"). The two are consistent; both are
  sketches beyond Ferguson's own $c=4$. This supports, from a secondary source, the form of
  $\delta$ in open item 2 of relaxed-likelihood.tex. The paper itself was not read.

Belongs in the library? **Not in step 7.** The decision already says its failure is
statistical: on every sample the MLE exists, and a certified fitter returns `Estimate`
with `certified = true`. The outcome type has nothing to represent, and must not grow a
kind for "consistent estimators exist but ML is inconsistent", since no single-sample
outcome can express a property of repeated sampling.

Where it belongs:
- relaxed-likelihood.tex and the ML-difficulties paper (already there);
- a teaching page that contrasts it with `sinexp`: ML returns `Ok` and is wrong
  asymptotically, versus ML refusing correctly on every sample.

If a site page is wanted later, it can be an ordinary family with an `existence` statement,
`inference_numeric: available: false` with the reason, and prose stating the
inconsistency. A certified fitter is feasible but not trivial:
- $\log\delta$ is fine, but $\delta(\theta)$ underflows for $1-\theta<0.2$;
- in binary64 the spike sits at $\theta=x_i$ to within far less than an ulp, so the
  candidates are the data points plus the maxima of the smooth part.

Bank it in `dev/roadmap/` if wanted; no card now.

## 4. Proposed families

Both families have no `<Id>Status` enum (consolidation: "Pathological families use this type and get no
enum of their own"). Their result carries `FitOutcome`, the coefficients and `StopDetail`.
Two schema points follow from that: `limits[].status` names a family enumerator today, so
it needs an outcome form such as `status: nonexistence/unattained`; and the C++ rules
("reports through its `<Id>Status` enum") need an exception note.

### 4.1 `sinexp`: sine family, exponential coordinate

Front matter, fields that matter (a sketch, not final YAML):

```yaml
id: sinexp
name: Sine-modulated exponential
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- {name: freq, symbol: '\theta', role: shape, domain: '[0, inf)', simulate: 3, kind: regular}
log_density: -x + log(1 + sin(2*pi*freq*(exp(-x) - 1/2)))
limits:
- kind: nonexistence
  firmness: proved
  name: oscillation, supremum not attained
  when: '$\theta$ tends to infinity along near-peaks of every observation'
  detect: 'the sample has at least two distinct values'
  status: nonexistence/unattained   # schema change, see above
relations:
  closure_point:
  - name: standard exponential (the member at $\theta=0$)
    path: '$\theta\to\infty$'
    reachability: never
    reason: 'For every sample its likelihood is $2^{-n}$ times the supremum; the weak limit carries no excess.'
fit: {method: closed-form, certified: 'yes', note: 'Nonexistence by Lindemann–Weierstrass and Kronecker; one distinct value: closed-form maximizer set.'}
existence:
  uniqueness: proved      # proved non-unique; the field has no value for that
  statement: >-
    With at least two distinct observations no MLE exists: the supremum
    $2^ne^{-\sum x_i}$ is not attained. With one distinct value the maximizers
    form an infinite arithmetic progression.
  reason: see §1.2 of dev/research/2026-11-14-pathological-families.md
inference_numeric:
- {regime: all, available: false, condition: "freq free", reason: "no MLE for samples with two distinct values; non-unique otherwise",
   classification: "dev/research/2026-11-14-pathological-families.md#12-the-sine-family-in-the-exponential-coordinate-sinexp"}
classification: uncommon
classification_reason: A counterexample for maximum likelihood; no applied use.
group: counterexamples        # new vocabulary value
areas: [teaching]             # new vocabulary value
construction: [transformation]
select: {default: false, reason: "no MLE on almost every sample"}   # new key, §4.3
```

What the fitter returns (hold subsets: `freq` free or held):

| case | outcome | payload |
|---|---|---|
| empty, non-finite, or $x<0$ | `Failure`, `InvalidInput` | |
| held `freq` $<0$ or non-finite | `Failure`, `InvalidArgument` | |
| `freq` held | `Estimate`, certified, unique | logLik at the held value |
| free, one distinct value | `Estimate`, certified, `unique = false` | canonical $\theta_0$ (smallest); maximizer set described as $\theta_0+k/\lvert u\rvert$ |
| free, at least two distinct values | `Nonexistence`, `Unattained` | `loglik_upper` $=n\log2-\sum x_i$ (compensated sum), no limit model, path $\theta\to\infty$ |

The decision needs no likelihood evaluation: the proof is the certificate. A countably
infinite maximizer set is a new non-unique case. The plan lists finite sets and describes
continua, so the rendering of `maximizers` needs a third form (first element and spacing).

d/p/q/r:
- **d.** $2e^{-x}\sin^2_\pi(\theta u+\frac14)$ with $u=e^{-x}-\frac12$. This avoids
  $1+\sin$ cancellation at the density zeros. The phase $\theta u$ is needed mod 2 to full
  relative accuracy, so it needs $e^{-x}$ in double-double: an error below $2^{-100}$
  keeps the phase error under $2^{-60}$ for $\theta\le2^{38}$. Beyond that the core
  reports an evaluation failure, or the dossier states the accuracy limit (a decision).
  Near $x=\log2$, $u=\frac12\operatorname{expm1}(\log2-x)$ needs a double-double $\log2$.
- **p.** Closed form via $v=e^{-x}$:
  $F(x)=-\operatorname{expm1}(-x)+\sin_\pi(\theta v)\sin_\pi(\theta(1-v))/(\pi\theta)$,
  checked against quadrature to 30 digits. The upper tail is computed directly. Use a sinc
  form as $\theta\to0$, and series near $x=0$ and $x\to\infty$ where the density can vanish.
- **q.** Bracketed monotone root. $\lvert F_V(v)-v\rvert\le1/(\pi\theta)$ gives the initial
  bracket. Fall back to bisection at the density zeros.
- **r.** $X=-\log V$, with $V$ by rejection from $U(0,1)$ (acceptance $\frac12$) or by
  inversion. Follow the library's sampler convention.
- **Truth table.** mpmath at 60 digits including large $\theta$; `truth.py` is
  straightforward.

### 4.2 `ssqnorm`: signed squared normal

$X=\mu+\sigma Z\lvert Z\rvert$ with $Z\sim N(0,1)$, so $\lvert X-\mu\rvert\sim\mathrm{Gamma}(\frac12,\text{scale }2\sigma)$:

$$
f(x\mid\mu,\sigma)=\frac{\exp\{-\lvert x-\mu\rvert/(2\sigma)\}}{2\sqrt{2\pi\sigma\lvert x-\mu\rvert}},\qquad
F(x)=\Phi\bigl(\operatorname{sgn}(x-\mu)\sqrt{\lvert x-\mu\rvert/\sigma}\bigr).
$$

Both formulas are checked numerically. Front matter as in §4.1, with:
- params `mu` (location, $\mathbb R$) and `sigma` (scale, $(0,\infty)$);
- `construction: [transformation, location-scale]`;
- `relations: {}` (no `signed-power` transform exists; adding one is optional);
- `fit.method: closed-form`;
- the same `select`, `group` and `areas` values.

| held | outcome | proof (**proved here**) |
|---|---|---|
| none, or `sigma` | `Nonexistence`, `Unbounded`, every sample, $n\ge1$ | as $\mu\to x_1$ the factor $\lvert x_1-\mu\rvert^{-1/2}\to\infty$; the other factors tend to positive limits, or also diverge for ties |
| `mu`, no $x_i=\mu$ | `Estimate`, certified, unique: $\hat\sigma=n^{-1}\sum\lvert x_i-\mu\rvert$ | $\ell=-\frac n2\log\sigma-S/(2\sigma)$ is strictly concave in $1/\sigma$ |
| `mu`, some $x_i=\mu$ | `Nonexistence`, `Unbounded` | the pole sits at a datum for every $\sigma$ |
| both | `Estimate`; if some $x_i=\mu$, `Nonexistence`, `Unbounded` (proposal) | an infinite logLik must not reach `model_select` |
| invalid data or held values | `Failure`, `InvalidInput` / `InvalidArgument` | |

The all-free case must be a fitted outcome, not a `holds: no-mle` refusal as in pareto3.
Otherwise the acceptance test never reaches the outcome type.

d/p/q/r reuse the normal's tail-accurate routines:
- $\log F$ via log-$\Phi$;
- $q=\mu+\sigma z\lvert z\rvert$ with $z=\Phi^{-1}(p)$;
- $r=\mu+\sigma Z\lvert Z\rvert$;
- $d(\mu)=+\infty$;
- $x-\mu$ needs two-sum near $\mu$.

Teaching note (**lit, secondary**): the Fisher information for $\mu$ is infinite. For a
singularity $\lvert x\rvert^{-1/2}$, Ibragimov and Has'minskii (1981, ch. VI) give the rate
$n^{-2}$, so fast estimators exist while ML does not.

### 4.3 Shared plumbing and tests

**`model_select` exclusion.** Today the default is `models = univariateML_models`, and
`check_models` validates against that list. Proposal:
- add a dossier key `select: {default: false, reason}`;
- the loader keeps `univariateML_models` complete and adds a default list without these
  families;
- an explicit `model_select(x, c("sinexp", "exp"))` still works and reports the stop in
  `skipped`.

**C++ tests (`test.cpp`):**
- `sinexp` nonexistence on seeded samples: $n=2$, ties, $x=0$, subnormal $x$, and $x>745$
  (where $e^{-x}$ underflows but the theorem still applies). Assert:
  - `outcome == nonexistence(Unattained)`;
  - `loglik_upper` equals $n\log 2-\sum x_i$ to rounding;
  - no limit model.
- A consistency check: logLik at random and grid $\theta$ never exceeds `loglik_upper`, and
  growing grids approach it. This is numerical, not proof.
- The one-value case: $\theta_0$ and $\theta_1$ both reach the supremum; `unique == false`.
- `ssqnorm`: every hold subset in the table; the closed form against mpmath.
- Truth tables for both families.

**R tests:**
- the condition class is `univariateML_nomle` with reasons `unattained` and `unbounded`;
- the `sinexp` message contains no "use `ml`";
- non-unique fits refuse `vcov`, `confint` and `bootstrapml`;
- `model_select(x)` skips both families by default;
- the explicit `model_select` call returns `exp` with `sinexp` in `skipped`;
- `dml`, `pml`, `qml` and `rml` work.

**Snapshot and strict layer:**
- the fuzzer's every-family cases must record `nonexistence` for every all-free `sinexp`
  sample with two distinct values, and for every `ssqnorm` sample with free $\mu$;
- `--outcomes` must show zero disagreements;
- new strict rule: an `Unattained` stop's `loglik_upper` dominates every witnessed logLik;
- the multistart oracle in `just quality` must not count a finite high point as a
  refutation;
- check whether `golden.json` and `just goldens` can record a stop instead of estimates
  (not checked here).

**R example (both families):** `x <- rml(50, mlsinexp(freq = 3)); try(mlsinexp(x)); mlsinexp(x[1])`.

## 5. Decisions and open questions

1. `sinexp` (S3) in place of the literal sine family? Alternatives: S2 (real-sample
   semantics as a stated exception), or the literal family under S1, which stops on
   `Budget` for nearly every input. Recommendation: S3, with the literal family as the
   motivating text in the `sinexp` dossier.
2. Add `ssqnorm`, or the bounded-spike family instead (collapse without a pole)?
   Recommendation: `ssqnorm`, which has the simplest proofs and reuses the normal routines.
3. Ferguson: no family now; prose and paper only. Bank it in the roadmap if a page is wanted.
4. Schema changes:
   - the `select` key;
   - group `counterexamples` and area `teaching`;
   - `limits[].status` for enum-less families;
   - a third `maximizers` form for infinite discrete sets;
   - an exception note in the C++ rules.
5. All-held `ssqnorm` with a datum at $\mu$: `Nonexistence`/`Unbounded` (proposed), or an
   estimate with infinite logLik.
6. The `sinexp` dpqr accuracy limit at large $\theta$: restrict the claim, or use more
   precision.
7. Open (conjecture): whether the sine family forces unbounded confidence sets for $\theta$
   (relaxed-likelihood.tex, open item 7). The question carries over unchanged to `sinexp`.

## References

- Baker, A. (1975). *Transcendental Number Theory*. Cambridge University Press, ch. 1 (Lindemann–Weierstrass theorem).
- Bahadur, R. R. (1958). Examples of inconsistency of maximum likelihood estimates. *Sankhyā* 20, 207–210.
- Dudley, R. M. (2003). MLEs in exponential families. Lecture notes, MIT 18.466. https://math.mit.edu/~rmd/466/mles-inexpon.pdf (not read; cited from search summary).
- Ferguson, T. S. (1982). An inconsistent maximum likelihood estimate. *Journal of the American Statistical Association* 77(380), 831–834. doi:10.1080/01621459.1982.10477894.
- Ferguson, T. S. (1996). *A Course in Large Sample Theory*. Chapman & Hall, ch. 17.
- Hardy, G. H. and Wright, E. M. (2008). *An Introduction to the Theory of Numbers*, 6th ed. Oxford University Press, ch. XXIII (Kronecker's theorem).
- Hill, B. M. (1963). The three-parameter lognormal distribution and Bayesian analysis of a point-source epidemic. *Journal of the American Statistical Association* 58, 72–84.
- Ibragimov, I. A. and Has'minskii, R. Z. (1981). *Statistical Estimation: Asymptotic Theory*. Springer.
- Kiefer, J. and Wolfowitz, J. (1956). Consistency of the maximum likelihood estimator in the presence of infinitely many incidental parameters. *Annals of Mathematical Statistics* 27(4), 887–906. doi:10.1214/aoms/1177728066.
- Le Cam, L. (1990). Maximum likelihood: an introduction. *International Statistical Review* 58(2), 153–171. doi:10.2307/1403464. Ferguson and the dyadic example on pp. 160–161 (checked in the text).
- Perlman, M. D. (1972). On the strong consistency of approximate maximum likelihood estimators. *Proceedings of the Sixth Berkeley Symposium on Mathematical Statistics and Probability* 1, 263–281.
- Wellner, J. A. (2009). Statistics 582, problem set 1, problem 6. University of Washington. https://sites.stat.washington.edu/jaw/COURSES/580s/582/PROBSETS.09/ps1-09.pdf.

Reproducible checks (mpmath, binary64 inputs read exactly with `fractions.Fraction`):
the period is $P=2^{-e}/g$ computed from `Fraction(x).denominator` and the numerators.
Each of $L(\theta)=L(\theta+P)$, the attained case $(1/16,5/16)$ at $\theta=4$, and both
closed-form CDFs against `mp.quad` takes one line.
