# Near-constant maximizers of betapr, fatigue, genbeta, invparalogis and paralogis (TASK-214.38)

Question: on the strict layer's `near-constant` sample $[c, c, c^+]$ ($c^+$ the next double above $c$,
$c$ from the family's golden sample), what is the maximum likelihood answer, and is it
representable in binary64? Finding group `near-constant-numerical-failure-unsettled`
(`dev/validation/strict-known.json`, TASK-214.33). Model followed:
[`near_constant_maximizers.py`](2026-11-14-strict-rules-data/near_constant_maximizers.py).

Throughout, $\delta = c^+/c - 1 \in [1.1, 2.2]\cdot 10^{-16}$ and $\varepsilon = \log(c^+/c) = \delta + O(\delta^2)$.
All numerics use the exact hexfloat data of the stored records
(`2026-11-15-fit-snapshot-data/{native,double}.jsonl.gz`, key `(family, 'class', 'near-constant')`) in mpmath at 100 to 200 digits.
The scripts and outputs are in [`2026-11-15-near-constant-data/`](2026-11-15-near-constant-data/).

## Summary

| family | held (record) | verdict | outcome the fit should give | outcome now (native = double) |
|---|---|---|---|---|
| paralogis | none | finite maximizer, representable: $\hat\alpha = 1.84\cdot 10^{16}$, $\hat\theta = c(1+2.1\cdot10^{-15})$ | estimate | F:no-convergence (BfgsFailure, shape stops at $1.56\cdot10^{16}$) |
| invparalogis | none | finite maximizer, representable: $\hat\alpha = 2.43\cdot 10^{16}$, $\hat\theta = c(1-1.5\cdot10^{-15})$ | estimate | F:no-convergence (same) |
| fatigue | $\mu = 0$ | finite maximizer, representable up to rounding of $\beta$: $\hat\alpha = 1.0052\cdot10^{-16}$, $\hat\beta = c + (c^+-c)/3$ | estimate | F:no-convergence (SearchFailure; candidate $\alpha$ is 22% too large) |
| genbeta | $\text{scale}=1$ | no finite maximizer: supremum unattained, approached on the log-gamma face ($a\to0$, $p\to\infty$, $pa\to\lambda$) with $q = 8.15\cdot10^{30}$, $\lambda=4.56\cdot10^{31}$ | limit, model `lgamma`, flagged (margin below double resolution) | F:no-convergence (no candidate) |
| betapr | none | no finite maximizer: supremum unattained, approached on the inverse-gamma face ($p\to\infty$) with $q=1.37\cdot10^{32}$ | limit, model `invgamma`, flagged (margin $\delta/3\approx 6\cdot10^{-17}$ in the log likelihood) | F:evaluation (gamma and inverse-gamma candidates unresolved, bounds $\pm10^{22}$) |

None of the five has a coefficient outside binary64. None is a nonexistence (the supremum is finite).
So `F:evaluation` as "unrepresentable maximizer" is wrong for all five; the settled outcomes are an estimate (three families) and a limit (two families).
Section "What the fit cannot do in doubles" lists the real obstruction: the maximizer sits at a resolution below one ulp of a coefficient.

Evidence classes. Derivation = leading-order asymptotics in $\delta$ without a bounded error term (not a rigorous proof). Numerical = 100 to 200 digit
maximization on the actual sample. Dossier facts are quoted as such.

Dependence on $c$: none in any verdict (each re-tested for other $c$, below). Dependence on the holds: the records hold only $\mu=0$ (fatigue)
and scale $=1$ (genbeta); other holds were not examined (see "Not examined").

## paralogis and invparalogis (both coefficients free)

Dossier: constant samples are unbounded; nonconstant samples have a finite maximum (existence proved there, uniqueness not).

### Derivation

paralogis: $\log f = 2\log\alpha + w - \log x - (\alpha+1)\log(1+e^{w})$ with $w = \alpha\log(x/\theta)$. For $x_i = c,c,c(1+\delta)$,
$w_i = w_0 + (0,0,\alpha\delta)$ up to $O(\alpha\delta^2)$. Write $s = \alpha\delta$ and $e^{w_0} = y/\alpha$. For $\alpha\to\infty$,
$w-(\alpha+1)\log(1+e^w) = \log y_i - \log\alpha - y_i + O(1/\alpha)$, so

$$
\ell = 3\log\alpha - 3\log c + \sum_i(\log y_i - y_i),\qquad y_i = y\,(1,1,e^{s}).
$$

Maximizing over $y$ gives $y = 3/(2+e^s)$, and with $\alpha = s/\delta$

$$
\ell(s) = 3\log\frac{s}{\delta} - 3\log c + 3\log 3 - 3\log(2+e^{s}) + s - 3 .
$$

$\ell'(s)=0$ reads $3/s - 3e^s/(2+e^s) + 1 = 0$, root $s^\ast = 2.116363$. So $\hat\alpha = s^\ast/\delta \approx 10^{16}$, and the profile curvature in $\log\alpha$
is $-5.10236$. For $\alpha\delta\gg1$ the third observation forces $y_3 = ye^s$ and the likelihood falls like $-s$; for $\alpha \ll 1/\delta$ it falls like $3\log\alpha$; hence a finite maximizer with $\alpha\asymp 1/\delta$ (consistent with the dossier's existence proof).
$\hat\theta = c\,e^{-w_0/\alpha}$ with $w_0 = \log(y/\alpha) \approx -38$, so $\hat\theta = c(1 + O(40/\alpha))$, about 20 ulp from $c$.

invparalogis: the reciprocal $x\mapsto 1/x$ gives one low and two high values, $y_i = y\,(1,e^s,e^s)$ and
$\ell(s) = 3\log(s/\delta) - 3\log c + 3\log3 - 3\log(1+2e^s) + 2s - 3$, root of $3/s - 6e^s/(1+2e^s) + 2 = 0$: $s^\ast = 3.192688$, profile curvature $-3.60282$.

### Numerical confirmation (`paralogis.py`, `universality.py`; 120 and 100 digits)

| | paralogis | invparalogis |
|---|---|---|
| $\delta$ | $1.14791\cdot10^{-16}$ | $1.31538\cdot10^{-16}$ |
| $\hat\alpha$ | $1.84366348014\cdot10^{16}$ | $2.4271936313\cdot10^{16}$ |
| $\hat\alpha\delta$ (derived $s^\ast$) | 2.11636 (2.116363) | 3.19269 (3.192688) |
| $\hat\theta$ | $0.967167555859281$ | $0.844030540926403$ |
| $\hat\theta/c - 1$ | $+2.10\cdot10^{-15}$ | $-1.54\cdot10^{-15}$ |
| maximum log likelihood | 107.875007191894 | 108.655730334803 |
| profile curvature in $\log\alpha$ | $-5.10235795893$ | $-3.60281673297$ |
| representable | yes (both) | yes (both) |

The profile in $\log_{10}\alpha$ over $[-1, 18]$ increases to a single peak near 16.3 (paralogis 107.87 at 16.25, 94.9 at 17, $-104.8$ at 18); no second maximum.
The asymptotic formulas match the exact maximizer to 10 digits for the record samples and for $c = 1, 1.5, 3.7, 0.2$ ($\hat\alpha$ ratio 1.0000000000), so $\hat\alpha = s^\ast/\delta$ with
$\delta = \mathrm{ulp}(c)/c$, between $0.95\cdot10^{16}$ and $2.7\cdot10^{16}$ for every $c$.

### What the fit gives and should give

Now: BfgsFailure, shape stops at $1.5600926743\cdot10^{16}=\sqrt3\cdot2^{53}$ in both families (below both maximizers), `loglik_upper = inf` from the unresolved certificate; outcome F:no-convergence.
Should: an estimate with $(\hat\alpha,\hat\theta)$; certified only if the certificate closes in this regime (not examined), otherwise uncertified and flagged. See the next section for the logLik caveat.

## fatigue ($\mu = 0$ held)

Dossier: with $\mu$ known, unbounded only when all shifted observations are equal; otherwise a maximum exists (uniqueness unknown); $\hat\beta\in[H,A]$ (harmonic and arithmetic mean of $t = x-\mu$), proved there.

### Derivation (`fatigue.py`)

$\alpha$ profiles out: $\alpha^2 = \overline{t/\beta+\beta/t-2}$. For $t = c(1,1,1+\delta)$ and $\beta = ce^{v}$, $t/\beta + \beta/t - 2 = v_i^2 + O(v_i^4)$ with $v_i = v, v, v-\varepsilon$, so

$$
\hat\alpha^2(v) = \tfrac13\bigl(2v^2 + (v-\varepsilon)^2\bigr),\qquad
\ell(v) = \text{const} - \tfrac32\log\hat\alpha^2(v) + O(\varepsilon).
$$

Minimizing the quadratic: $v = \varepsilon/3$, $\hat\alpha^2 = 2\varepsilon^2/9$. Hence $\hat\alpha = \tfrac{\sqrt2}{3}\varepsilon \approx 0.4714\,\varepsilon \approx 10^{-16}$ and
$\hat\beta = c\,e^{\varepsilon/3}$: one third of the way from $c$ to $c^+$, strictly between two adjacent doubles. The profile is unimodal.

### Numerical confirmation

$\varepsilon = 2.13229050935\cdot10^{-16}$. Exact maximizer: $v/\varepsilon = 0.333333333333333333$, $\hat\alpha/\varepsilon = 0.471404520791032$ ($=\sqrt2/3$ to 15 digits),
$\hat\alpha = 1.0051713857469\cdot10^{-16}$, $\hat\beta = 2.0826862376537029024$ ($(\hat\beta - c)/(c^+-c) = 0.3333333$), maximum log likelihood 104.050819121222.
Hessian in $(\log\alpha,\log\beta)$: diag$(-6,\,-2.97\cdot10^{32})$. A scan over $\log\beta\in[-7,7]$ (relative to $c$) and a fine scan on the scale of $\varepsilon$ find one peak, at $k=7$ of $v = k\varepsilon/20$ (grid point nearest $\varepsilon/3$).
$\hat\alpha$ is representable. $\hat\beta$ is not a double but lies within $7\cdot10^{-17}$ (relative) of $c$, far inside the $10^{-10}$ accuracy contract.

### What the fit gives and should give

Now (SearchFailure): candidate $\alpha = 1.23107849956\cdot10^{-16}$, $\beta = c$, loglik 103.4426. That is the maximizer over the double grid for $\beta$: $\alpha$ conditional on $\beta = c$ is $\varepsilon/\sqrt3$, which is $1.2247\,\hat\alpha$ (22% off).
Should: an estimate $(\hat\alpha,\mathrm{fl}(\hat\beta))$, $\mathrm{fl}(\hat\beta)=c$. The returned $\alpha$ must be $\hat\alpha$, not the conditional maximizer at the rounded $\beta$.

## betapr (all three coefficients free)

Dossier: unbounded when all observations equal; gamma and inverse-gamma faces ($q\to\infty$, resp. $p\to\infty$) are proved limits.

### Derivation and numerics (`betapr.py`, `betapr_faces_c.py`; 200 and 100 digits)

Reduction: for fixed $b$ the score in $b$ is $\sum_i[Nx_i/(x_i+b) - p] = 0$, $N=p+q$, i.e. $\overline{x/(x+b)} = \rho = p/N$. So $(p,q,b)\leftrightarrow(N,b)$ with $\rho=\rho(b)$; profile over $N$ is one-dimensional, then scan $\rho\in(0,1)$ ($\rho\to0$ gamma face, $\rho\to1$ inverse-gamma face).

Concentration forces $p$ or $q$ near $9/(2\delta^2)$. Face maxima: gamma limit (shape $k$ from $\log k-\psi(k)=c_g$, $c_g=\log\bar x-\overline{\log x}$) has $k = 1/(2c_g)(1+\dots) = 1.3678792443\cdot10^{32}$ and loglik $L_g = 108.209702204581697142517828$;
inverse-gamma limit (same on $1/x$ with Jacobian $-2\sum\log x$) has shape $1.3678792443\cdot10^{32}$, scale $8.37289\cdot10^{31}$, loglik $L_i = 108.209702204581697202976851$.
Expansion in $\delta$: $c_g = \delta^2/9 - 8\delta^3/81$, $c_i = \delta^2/9 - 10\delta^3/81$ and $\ell/n = -\tfrac12\log(4\pi c) - \tfrac12 - \overline{\log x} - c/6 + O(c^2)$ on each face, so

$$
L_i - L_g = -\tfrac32\log\frac{c_i}{c_g} = \frac{\delta}{3} + O(\delta^2).
$$

Numerics: $(L_i-L_g)/(\delta/3) = 1.0000000000$ for $c = 0.001, 0.2, 0.612, 1, 1.5, 3.7, 1000$. The sign is the skew of the sample (two low, one high favours the heavier right tail of the inverse gamma), so it holds for every $c$.
Interior profile $\Lambda(\rho)=\max_{N,b}\ell$: $\Lambda(\rho) - L_g = \rho\,(L_i - L_g)$ to 17 digits for $\rho = 10^{-22},\dots,0.5,\dots,1-10^{-22}$ (deviation $< 10^{-17}$ relative, table in `betapr.txt`).
So $\Lambda$ increases strictly to $\rho\to1$ and its supremum is $L_i$, not attained for finite $p$: $p = \rho N\to\infty$ with $q = 1.37\cdot10^{32}$ and $b\to0$. No interior maximizer and no maximizer on the gamma face.
All face coefficients are representable. The fit's own inverse-gamma candidate (shape $1.36\cdot10^{32}$, `0x1.afa067c7ac37ep+106`) is correct.

Proved: the face maxima and $L_i-L_g\approx\delta/3$ (leading order). Numerical: monotonicity in $\rho$, i.e. that no interior point beats the face.

### What the fit gives and should give

Now F:evaluation: candidates gamma, inverse gamma and a capped interior one (shape1 $\approx10^6$), loglik bounds $\pm10^{22}$, nothing separates the faces.
Should: limit `invgamma` ("use `mlinvgamma`"), decided by a margin of $\delta/3 \approx 6\cdot10^{-17}$ in the log likelihood. That is about $5\cdot10^{-19}$ relative to $\ell\approx108$, below the double resolution ($\approx10^{-14}$), so the choice cannot be certified by double evaluation. The honest label is a flagged limit (margin), or a proved one if the leading-order difference above is implemented as an analytic comparison. Choosing `gamma` would be the wrong model.

## genbeta (scale $=1$ held; $p$, $q$, $a$ free)

Dossier: unbounded when all observations are equal and $q$ plus one of $p$, $a$ are free; faces `lgamma` ($p\to\infty$, $a\to0$, $pa\to\lambda$) and `power` ($a\to\infty$) are proved limits.

### Derivation and numerics (`genbeta.py`, `genbeta_limit_h.py`, `genbeta_face.py`; 200 digits)

With $u = x^a$ the density is $a\,u^{p}(1-u)^{q-1}/(xB(p,q))$: for fixed $a$ an exponential family in $(p,q)$, concave, with scores
$\psi(p+q)-\psi(p)+\overline{a\log x}=0$, $\psi(p+q)-\psi(q)+\overline{\log(1-u)}=0$ (solved by multidimensional Newton).
Profile in $a$ (record sample, $\delta=1.327\cdot10^{-16}$): the profile log likelihood is flat to $10^{-16}$ in $a$ at leading order ($\approx 108.2097022045816969$ for all $a\in[10^{-3},50]$, with $\hat p,\hat q\approx10^{30}$ to $10^{34}$), and the order-$\delta$ part decreases:

$$
\frac{\ell(a) - \ell_{\text{face}}}{\delta} = -\kappa(c)\,a^2\,(1+o(1)),\quad \kappa(c)=0.0049620\ (\text{record, from } a=10^{-6}\text{ to }10^{-1}).
$$

The face value is the supremum $\ell_{\text{face}}$: $W=-\log X\sim\text{Gamma}(q,\lambda)$ maximized by the gamma MLE on $w_i=-\log x_i$: $\hat q = 8.1499113\cdot10^{30}$, $\hat\lambda = 4.5624022\cdot10^{31}$, $\ell_{\text{face}} = 108.2097022045816969007368$. Other $c$ ($0.3, 0.5, 0.7, 0.95, 0.999$): $\kappa = 0.0334, 0.0193, 0.0099, 0.00142, 2.8\cdot10^{-5}$, all negative increments. For large $a$ the likelihood is in the gamma regime:
$u_0=c^a\to0$, $q u_i = e^{t_i}$ with $t_i=\nu+a\log x_i$, and $\ell = 3\log s - 3\log\varepsilon + 3[p\psi(p)-p-\log\Gamma(p)] + \text{const}$, $s=a\varepsilon$, $\log p-\psi(p)=\log\frac{2+e^s}{3}-\frac s3$. Then $h(s)-h(0^+) = -s/6+\dots$, strictly decreasing (table in `genbeta_limit_h.txt`); this regime has $q\sim e^{a|\log c|}$ but it is never the maximum. At $a=10^3$ the deficit is $-164.8\,\delta$.
So $\ell$ is strictly below the face for every tested $a>0$, increasing as $a\to0$: the supremum is $\ell_{\text{face}}$, unattained, an escape to `lgamma`.
All face coefficients are representable.

Proved: the large-$a$ reduction and $h'(0)=-1/6$ (leading order; analytic expansion of $h$ above). Numerical: the sign and size of $\kappa(c)$ and the scan in $a$; no analytic derivation of $\kappa$.

### What the fit gives and should give

Now: F:no-convergence, no candidate. Should: limit `lgamma` ("log-gamma on scale / x", `mlgamma` of $-\log x$), flagged: the deficit of any finite-$a$ interior point is $\kappa\delta a^2\lesssim10^{-18}a^2$, below double resolution. (The tie3 preimage selects `power`; this sample selects `lgamma`; the answer depends on the sample, not on $c$.)

## What the fit cannot do in doubles

For paralogis, invparalogis and fatigue the maximizer exists and is representable, but the likelihood varies on the scale of one ulp of the scale coefficient:

- paralogis: the log likelihood in $\log\theta$ has curvature of order $\alpha^2\approx 10^{32}$, so one ulp of $\theta$ changes $w=\alpha\log(x/\theta)$ by about 2. Re-optimizing $\alpha$ for each double $\theta$ near $\hat\theta$ (`paralogis.txt`): the conditional $\hat\alpha(\tilde\theta)$ is $1.874\cdot10^{16}$ at the nearest double ($+1.7\%$ from $\hat\alpha$), $1.99\cdot10^{16}$ one ulp down, $1.77\cdot10^{16}$ one ulp up. $\ell$ at $(\hat\alpha,\mathrm{fl}(\hat\theta))$ is 107.2277, below the supremum 107.8750 by 0.65; at 3 ulp it is off by thousands.
  invparalogis: conditional $\hat\alpha = 2.364\cdot10^{16}$ at the nearest double ($-2.6\%$), $\ell(\hat\alpha,\mathrm{fl}\hat\theta) = 107.6098$ against 108.6557.
- fatigue: $\ell(\hat\alpha, c)=103.3008$ against 104.0508 (loss 0.75); the best $\alpha$ at $\beta=c$ gives 103.4426 (loss $\tfrac32\log\tfrac32=0.608$), at $\beta=c^+$ 102.4029.

Consequence (fact): a fitter that maximizes evaluated doubles, or gates on the score at its returned coefficients, cannot return the maximizer; one that returns $\hat\alpha$ from an offset-coordinate solve (e.g. $\theta = c\,e^{-z/\alpha}$ with $z$ and $\alpha$ in extended precision) can, and each coefficient is then within $10^{-15}$ relative of the maximizer. The recorded `logLik` at the returned doubles is up to 1.05 below the supremum. The memo's remedies for this are a `logLik` at the returned coefficients (honest, lower), or a `conditioning` stop. Which of the two, or `F:evaluation` for lack of an extended-precision path, is a design decision (not decided here).

## Dossier corrections proposed (not applied)

- paralogis and invparalogis, `existence.statement`, append: "Samples whose relative spread is of the order of the binary64 resolution (for example $[c,c,c^+]$) have a finite maximizer with $\hat\alpha\approx s^\ast/\delta$, $\delta$ the relative spread and $s^\ast = 2.1164$ (paralogis) or $3.1927$ (invparalogis) for two-low-one-high, resp. reversed, data; $\hat\alpha$ is about $10^{16}$, still a binary64 number, but $\hat\theta$ is within about 40 ulp of the data and the log likelihood at rounded coefficients differs from the supremum by order 1."
- fatigue, `existence.statement`, append: "For two equal shifted observations and a third larger one by the relative amount $\varepsilon$, $\hat\alpha=(\sqrt2/3)\varepsilon$ and $\hat\beta=t_{(1)}e^{\varepsilon/3}$, a point between $t_{(1)}$ and $t_{(3)}$; for $\varepsilon\approx2^{-52}$ no double represents $\hat\beta$ but its rounding is within the accuracy contract."
- betapr, `limits` (gamma and inverse-gamma entries), add: "For $[c,c,c^+]$ the supremum is the inverse-gamma face; the log likelihood on the segment between the faces is $L_g+\rho(L_i-L_g)$ with $L_i-L_g=(c^+-c)/(3c)$ (leading order, numerical on the segment), below double resolution."
- genbeta, `limits`: the `lgamma` entry should record that near-constant data (two equal values and a third larger by a relative amount at the resolution of binary64) select the log-gamma face (numerical), with $q\sim10^{31}$ to $10^{32}$ and a margin $\kappa\delta a^2$ below double resolution.

## Not examined (hypotheses)

- Other holds: fatigue with $\alpha$ or $\beta$ held; genbeta with shape holds; betapr with $p$ or $q$ held (dossier proves gamma or exp, inverse-gamma or inverse-exp faces for those); paralogis and invparalogis with scale held (dossier: finite maximum for every positive sample). Hypothesis: with $\alpha$ held the fatigue likelihood is bounded away from the degenerate collapse. None was computed.
- The certificates of paralogis (compact-domain) at $\alpha\approx10^{16}$: not run.
- Samples other than $[c,c,c^+]$ (`tie3`, `half-ties`): only their records were read; the tie3 outcomes are unchanged.
