# Lean formalization of fixed-size beta-binomial existence

**TASK-295 · 2026-11-25**

Lean proves all five required targets. For integer size $m\ge2$ and a
nonempty sample with an interior count, a finite maximum with two free
positive shapes exists exactly when $V>mu(1-u)$. At or below binomial
variance, every finite shape pair is strictly below the best binomial, and
the binomial value is the supremum. For a sample concentrated at one
endpoint, the log-likelihood supremum is zero and every finite value is
strictly below it. [Lean module](../../lean/UnivariateML/Family/BetaBinomial.lean)

## Dossier findings

- **The stated conjecture for sizes three and up is settled.** The dossier
  describes the necessity of overdispersion as conjectural at these sizes.
  Lean proves the later global necessity argument for every $m\ge2$,
  including exact equality. The dossier's criterion of a finite point at
  least as good as the binomial remains equivalent to attainment with an
  interior count. In the exact model any sample satisfying that criterion
  also has a finite point strictly above the binomial. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md),
  [Global necessity proof](2026-11-25-bbinom-math-report.md#global-necessity-proof)
- **The one-trial maximizing ray requires both observed categories.** The
  dossier's description of that ray needs the qualification $0<\bar x<1$.
  The endpoint theorems also apply at size one. An all-zero or all-one
  sample has an unattained point-mass supremum, since a finite positive
  shape pair gives a probability strictly between zero and one.
  [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md),
  [Mathematics report](2026-11-25-bbinom-math-report.md)
- The dossier's phrase “only possible escape” for samples with an interior
  count concerns escapes that can compete for the maximum. The compactness
  proof bounds the log likelihood by a constant plus $\log p$,
  $\log(1-p)$, or $-\log(1+s)$, respectively, in the other boundary
  regimes. The exact criterion concerns mathematical attainment. The
  dossier's floating-point margins concern certification of computed fits.
  [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md#signed-binomial-face-certificate),
  [Compactness argument](2026-10-31-open-existence-report.md#1-beta-binomial-fixed-size-exact-algebraic-decision)

## Proof and effort

| Target | Effort | Argument |
| --- | --- | --- |
| 1. Identity | Short | Expand the centered squares and substitute $u=S/(nm)$. The identity holds for every positive size and sample size. |
| 2. Binomial limit | Moderate | Cancel the log-scale terms to extend the likelihood to $s=0$, then use continuity of the finite sums. A feasible converging path bounds the supremum from below. The binomial optimum at $u$ follows from $\log t\le t-1$. |
| 3. Necessity | Largest | Establish the integral interpolation using Mathlib's Frullani theorem and an explicit integrable majorant. Apply Jensen with weights proportional to the counts and their reflections, then prove the strict kernel scaling inequality via the derivative of the normalized hyperbolic sine. Concavity in the shape argument gives the final bound. |
| 4. Sufficiency | Substantial | Differentiate the extended likelihood at zero, reduce the arithmetic-series sums to $\Delta$, and use the positive derivative to obtain a finite witness. One interior count controls both probability endpoints and the large-$s$ tail. A compact rectangle contains every point that matches or beats the witness, and the binomial face is strictly below the witness. |
| 5. Endpoint cases | Moderate | Bound every finite value strictly below zero using an endpoint factor. Approach zero through the corresponding binomial limits and their open-probability values. The argument works at every positive size. |

The global necessity argument closes. Lean uses ordinary monotonicity and
concavity of the interpolation, with all the strictness needed for the
likelihood comparison supplied by the scaling inequality. For the uniform
tail bound in sufficiency, the proof pairs the shifted numerator factors
with denominator factors and leaves the denominator at index $m-1$.
Since $m-1\ge1$, its logarithm bounds $\log(1+s)$ from below.
[Global necessity proof](2026-11-25-bbinom-math-report.md#global-necessity-proof),
[Sufficiency source](2026-10-31-open-existence-report.md#1-beta-binomial-fixed-size-exact-algebraic-decision)

## Lean statements

The statements below are copied from namespace `UnivariateML.BetaBinomial`.
The sample is `x : Fin n → ℕ`, with support hypotheses where needed.
The positive-shape parameter domain is `Ioi 0 ×ˢ Ioi 0`.

| Declaration | Reading |
| --- | --- |
| `delta_identity` | The signed statistic is $mn^2$ times the excess over binomial variance. |
| `logLik_tendsto` | The path $(p/s,(1-p)/s)$ tends in log likelihood to the binomial at any open probability $p$. |
| `binomial_isMaxOn` | The empirical proportion maximizes the binomial log likelihood over open probabilities. |
| `boundary_score` | At $p=u$, the inverse-concentration derivative at zero is $\Delta/(2nmu(1-u))$. |
| `exists_above_binomial` | Strict overdispersion produces a positive inverse concentration whose value beats the best binomial. |
| `necessity` | At or below binomial variance, every finite pair loses strictly to the best binomial. |
| `binomial_sup_of_nonpos` | In that regime the best binomial is the supremum, which strictness excludes at every finite pair. |
| `sufficiency` | Strict overdispersion and an interior count imply that a positive finite pair attains the global maximum. |
| `exists_max_iff` | In the interior-count regime, finite attainment is equivalent to strict overdispersion. |
| `bestBinomial_le_sup` | The best binomial value is a lower bound for the beta-binomial supremum whenever the empirical proportion is interior. |
| `allZero_sup` | All-zero data have log-likelihood supremum zero, strictly above every finite value. |
| `allSize_sup` | All-size data have the same unattained log-likelihood supremum. |

```lean
variable {m n : ℕ} {x : Fin n → ℕ}

theorem delta_identity (hm : 0 < m) (hn : 0 < n) (x : Fin n → ℕ) :
    delta m x = (m : ℝ) * (n : ℝ)^2 *
      (variance x - m * proportion m x * (1 - proportion m x))

theorem logLik_tendsto (hx : ∀ i, x i ≤ m) {p : ℝ} (hp : 0 < p) (hp' : p < 1) :
    Tendsto (fun s : ℝ ↦ logLik m x (p / s) ((1 - p) / s))
      (𝓝[>] 0) (𝓝 (binomialLogLik m x p))

theorem binomial_isMaxOn (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1) :
    IsMaxOn (binomialLogLik m x) (Ioo 0 1) (proportion m x)

theorem boundary_score (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1) :
    HasDerivAt (faceLogLik m x (proportion m x))
      (delta m x / (2 * n * m * proportion m x * (1 - proportion m x))) 0

theorem exists_above_binomial (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1)
    (hvar : (m : ℝ) * proportion m x * (1 - proportion m x) < variance x) :
    ∃ s > 0, binomialLogLik m x (proportion m x) <
      logLik m x (proportion m x / s) ((1 - proportion m x) / s)

theorem necessity (hm : 2 ≤ m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1)
    (hV : variance x ≤ (m : ℝ) * proportion m x * (1 - proportion m x))
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    logLik m x a b < binomialLogLik m x (proportion m x)

theorem binomial_sup_of_nonpos (hm : 2 ≤ m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1)
    (hV : variance x ≤ (m : ℝ) * proportion m x * (1 - proportion m x)) :
    sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) =
      binomialLogLik m x (proportion m x)

theorem sufficiency (hm : 2 ≤ m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hi : ∃ i, 0 < x i ∧ x i < m)
    (hvar : (m : ℝ) * proportion m x * (1 - proportion m x) < variance x) :
    ∃ q ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q

theorem exists_max_iff (hm : 2 ≤ m) (hn : 0 < n) (hx : ∀ i, x i ≤ m)
    (hi : ∃ i, 0 < x i ∧ x i < m) :
    (∃ q ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q) ↔
      (m : ℝ) * proportion m x * (1 - proportion m x) < variance x

theorem bestBinomial_le_sup (hx : ∀ i, x i ≤ m)
    (hu : 0 < proportion m x) (hu' : proportion m x < 1) :
    binomialLogLik m x (proportion m x) ≤
      sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain)

theorem allZero_sup (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i = 0) :
    sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) = 0 ∧
      ∀ q ∈ domain, logLik m x q.1 q.2 < 0

theorem allSize_sup (hm : 0 < m) (hn : 0 < n) (hx : ∀ i, x i = m) :
    sSup ((fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) '' domain) = 0 ∧
      ∀ q ∈ domain, logLik m x q.1 q.2 < 0
```

## Verification and open scope

- On the workstation, the final `lake env lean UnivariateML/Family/BetaBinomial.lean`
  exited successfully with no diagnostics. The explicit
  `lake build +UnivariateML.Family.BetaBinomial` passed without warnings
  with `Build completed successfully (7207 jobs).`
- The final default `lake build` passed without warnings with
  `Build completed successfully (6594 jobs).` The explicit module target
  covers BetaBinomial while the coordinator manages the library root
  imports. All final runs used the single-slot wrapper at nice priority on the
  workstation, pinned to one CPU, with cached Mathlib.
- The module contains no proof placeholders, custom axioms, or
  `native_decide`. Every required target is proved.
- The optional mixed-endpoint case remains outside the Lean module. Its
  paper proof uses the strict upper bound from the saturated two-category
  likelihood and a vanishing-concentration path to the matching mixture.
  Joint uniqueness for sizes at least three in the overdispersed regime
  remains a mathematical question. [Endpoint proof](2026-10-31-open-existence-report.md#1-beta-binomial-fixed-size-exact-algebraic-decision),
  [Open uniqueness question](2026-11-25-bbinom-math-report.md#open)
