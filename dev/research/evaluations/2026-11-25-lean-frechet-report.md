# Lean evaluation of held-shape Fréchet location–scale likelihood

TASK-303. All four targets are proved for nonempty uncensored samples and every
real held shape $\lambda>0$. The results use the total log-likelihood.
[Lean module](../../lean/UnivariateML/Family/Frechet.lean)

The input `g : Fin n → ℝ` consists of the gaps $g_i=x_i-m$, where
$m=\min_i x_i$. The hypotheses `hg` and `hz` say that these gaps are
nonnegative and at least one is zero. The positive fitted gap $e$ gives
the original location $\mu=m-e$, and $g_i+e=x_i-\mu$. This bijection turns
the positive gap–scale domain into the original feasible location–scale
domain. `minimumCount g` counts the zero gaps, `collapseExponent g lam`
is $\lambda(n-k)-k$, and `powerSum g p e` is $S_p(e)$.

## Source findings and scope

The held-shape score, collapse threshold and finite equality value in the
Fréchet math report and dossier agree with the formal results. The held-shape
branch of `frechet_held_mle` makes the same threshold comparison with a
floating-point fused multiply-add. It uses the historical `InvalidData`
status for the two collapse cases and distinguishes them by the reported
supremum. Lean proves the exact statistical outcomes. Floating-point
comparisons, margins and numerical restoration are outside the theorem
statements. [Math report](2026-11-25-frechet-math-report.md#fréchet-single-holds)
· [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md#details)
· [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp)

The dossier's reference to global shape coverage concerns the free-shape
fit. At held shape, the two ratio limits and strict decrease suffice for
global uniqueness. The ratio limits in Lean also cover the constant sample,
where both limits coincide. No held-shape mathematical discrepancy was found.

## Theorems

The statements below are in `UnivariateML.Frechet`.

### 1. Conditional scale

For every positive gap, $\hat\sigma=(n/S_\lambda(e))^{1/\lambda}$ is positive
and has strictly higher likelihood than any other positive scale. The proof
applies the strict log inequality to the ratio of the candidate and optimal
scale powers.

```lean
theorem heldShape (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hl : 0 < lam) (he : 0 < e) :
    0 < scale g lam e ∧ scale g lam e ^ lam = n / powerSum g lam e ∧
      ∀ s > 0, s ≠ scale g lam e → logLik g lam e s < profile g lam e
```

### 2. Gap score

The derivative with respect to the gap is $S_1(e)$ times
$n\lambda R(e)-(\lambda+1)$, where
$R(e)=S_{\lambda+1}(e)/(S_\lambda(e)S_1(e))$. Since $S_1(e)>0$, this
factorization gives the stated sign.

```lean
theorem hasDerivAt_profile (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hl : 0 < lam) (he : 0 < e) :
    HasDerivAt (profile g lam)
      (powerSum g 1 e * (n * lam * ratio g lam e - (lam + 1))) e
```

### 2. Strict decrease of the ratio

The ratio strictly decreases on the positive half-line if two gaps differ.
For $q_i=1/(g_i+e)$, the mean of $q_i$ with weights $q_i^p$ strictly
increases with $p$. The proof cross-multiplies and symmetrizes the pair
sum. One unequal pair contributes strictly positively, and every pair
contributes nonnegatively. Applying this comparison to the logarithmic
derivative of the ratio gives its negative sign.

```lean
theorem ratio_strictAntiOn (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hl : 0 < lam)
    (hne : ∃ i j, g i ≠ g j) : StrictAntiOn (ratio g lam) (Ioi 0)
```

### 2. Ratio limits

The ratio tends to $1/k$ at the collapse end and $1/n$ at infinity. In
the normalized sums based on $e/(g_i+e)$, the individual terms tend to the
indicator of a zero gap at collapse and to one at infinity.

```lean
theorem ratio_limits (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) (hl : 0 < lam) :
    Tendsto (ratio g lam) (𝓝[>] 0) (𝓝 (1 / (minimumCount g : ℝ))) ∧
      Tendsto (ratio g lam) atTop (𝓝 (1 / (n : ℝ)))
```

### 3. Unique joint maximum above threshold

When $\lambda(n-k)>k$, the ratio limits bracket the unique score zero.
The profile increases before that zero and decreases after it. Combining
the strict profile maximum with the strict conditional scale maximum
gives exactly one finite maximizing location–scale pair.

```lean
theorem heldShape_joint_unique (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hl : 0 < lam) (hE : 0 < collapseExponent g lam) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧
      IsMaxOn (fun p ↦ logLik g lam p.1 p.2) domain p
```

### 3. Unbounded likelihood below threshold

When $\lambda(n-k)<k$, shrinking the gap along the optimal scale path
sends the profile to positive infinity and produces a feasible pair
above every real log-likelihood bound. This includes constant data because
their minimum count is $n$ and their collapse exponent is $-n$.

```lean
theorem heldShape_unbounded (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hl : 0 < lam) (hE : collapseExponent g lam < 0) :
    Tendsto (profile g lam) (𝓝[>] 0) atTop ∧
      ∀ B : ℝ, ∃ p ∈ (domain : Set (ℝ × ℝ)), B < logLik g lam p.1 p.2
```

### 4. Finite unattained supremum at threshold

When $\lambda(n-k)=k$, the profile strictly decreases and converges to
the least upper bound of the joint log-likelihood. Every feasible pair is
strictly below it. The value is

$$
n\log(n\lambda/k)-n-(\lambda+1)\sum_{g_i>0}\log g_i.
$$

The definition `collapseValue` sums over all gaps because Lean's
`Real.log 0 = 0` makes the zero-gap terms vanish there. The proof of the
limit keeps those observations' actual log distances for $e>0$ and
collects their contribution into the coefficient of $\log e$.

```lean
theorem heldShape_collapse_eq (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hl : 0 < lam) (hE : collapseExponent g lam = 0) :
    StrictAntiOn (profile g lam) (Ioi 0) ∧
      Tendsto (profile g lam) (𝓝[>] 0) (𝓝 (collapseValue g lam)) ∧
      IsLUB ((fun p : ℝ × ℝ ↦ logLik g lam p.1 p.2) '' domain) (collapseValue g lam) ∧
      ∀ p ∈ (domain : Set (ℝ × ℝ)), logLik g lam p.1 p.2 < collapseValue g lam
```

## Proof effort

| Target | Relative effort | Main work |
| --- | --- | --- |
| 1 | Short | Isolate the scale power and apply the strict log inequality. |
| 2 | Largest | Formalize the finite weighted-mean comparison, differentiate the log ratio, and normalize the ratio for both endpoint limits. |
| 3 | Moderate | Bracket the score zero, lift the profile maximum to a joint maximum, and use the collapse decomposition to prove divergence below threshold. |
| 4 | Moderate | Separate the divergent logarithm from the finite remainder, then use strict decrease and the one-sided limit to prove the least upper bound and nonattainment. |

## Verification and open items

- On the workstation, `lake env lean UnivariateML/Family/Frechet.lean`
  passed without warnings through the single-slot wrapper.
- On the workstation, `lake build` passed through the same wrapper with
  cached Mathlib. Lake reported `Build completed successfully (6598 jobs).`
  The build emitted two existing line-length warnings in `Weibull.lean`
  at lines 639 and 675. The standalone check covers Fréchet until the
  coordinator adds the library-root import.
- All requested Fréchet proofs are closed without placeholders or added
  axioms. There are no open items within the four targets.

