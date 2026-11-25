# Location–scale concavity and joint MLEs

**TASK-313 · 2026-11-25**

Lean verifies the joint-fit results in exact real arithmetic. Logistic,
Gumbel, and normal have a unique finite MLE on nonconstant data. Laplace
has a finite MLE under the same sample condition.

## Dossier comparison

| Dossier | Comparison with Lean |
| --- | --- |
| [Logistic](../../../univariateML-cpp/include/cppunivariate/distributions/logis/README.md) | The joint existence, uniqueness, and unbounded constant-sample likelihood agree. The dossier calls the scaled location $a$ and inverse scale $b$, in the opposite order to the task. Its hyperbolic cosine expression is the same loss since $2\cosh(t/2)=e^{t/2}(1+e^{-t})$. |
| [Gumbel](../../../univariateML-cpp/include/cppunivariate/distributions/gumbel/README.md) | The joint result and constant-sample collapse agree. The proof uses the maxima convention and joint concavity, independent of the dossier’s scale profile. |
| [Normal](../../../univariateML-cpp/include/cppunivariate/distributions/norm/README.md) | Attainment exactly for nonconstant nonempty data and uniqueness when attained agree. The dossier also gives the explicit estimators and uses natural parameters. The Lean proof works in inverse-scale coordinates. |
| [Laplace](../../../univariateML-cpp/include/cppunivariate/distributions/laplace/README.md) | Attainment exactly for nonconstant nonempty data agrees. The dossier gives a unique scale and all sample medians as maximizing locations. Its uniqueness conclusion is conditional on the median interval being a singleton. |

There is no contradiction in the joint-fit claims. The task’s sufficient
hypothesis for strict concavity can be sharpened. For a strictly convex
loss, every **nonempty** sample yields strict concavity, including constant
samples. The empty sample gives a constant zero objective. Nonconstancy is
needed for attainment. On a nonempty constant sample the log likelihood is
unbounded above despite strict concavity for a strictly convex loss.

The generic theorem uses finite real losses and the explicit sufficient
bound $g(t) \ge c|t|-d$ with $c>0$. Normal and Laplace omit additive
normalizing constants. The proofs concern the two free coefficients.
Held-coefficient fits, explicit normal estimates, the exact Laplace
maximizer set, and floating-point implementation behavior are outside
this task. No proof obligations remain open within TASK-313.

## Generic statements

Here $x \colon \mathrm{Fin}\,n \to \mathbb R$. The domain is
$(\mu,\sigma)$ with $\sigma>0$, and the inverse domain is
$(a,b)=(1/\sigma,\mu/\sigma)$ with $a>0$. `Nonconstant` means
that two sample values differ. `IsMaxOn` compares the candidate with
every feasible parameter pair. [Generic module](../../lean/UnivariateML/Generic/LocationScale.lean)

`concave_inverseLogLik` adds the concave log-inverse-scale term and the
negative of a convex loss on an affine residual for each observation.
`strictConcave_inverseLogLik` uses strictness of `Real.log` when the
inverse scales differ. When they agree, distinct parameter pairs have
different residuals at each observation and strict convexity of the
loss supplies strictness. Summing a strict inequality requires $n>0$.

```lean
theorem concave_inverseLogLik {g : ℝ → ℝ} (hg : ConvexOn ℝ univ g)
    (x : Fin n → ℝ) : ConcaveOn ℝ inverseDomain (inverseLogLik g x)

theorem strictConcave_inverseLogLik {g : ℝ → ℝ} (hg : StrictConvexOn ℝ univ g)
    (x : Fin n → ℝ) (hn : 0 < n) :
    StrictConcaveOn ℝ inverseDomain (inverseLogLik g x)
```

`tendsto_inverseLogLik_cocompact` shows that the log likelihood tends to
minus infinity on escape from compact subsets of the feasible parameter
domain, including approach to $a=0$. With $S=\sum_i|ax_i-b|$, two
distinct observations give $S\ge a|x_i-x_j|$, while the tail bound
gives $\ell\le n\log a+nd-cS$. The upper bound for $\log a$
with slope $c|x_i-x_j|/(2n)$ bounds $a$ above on each superlevel set.
The lower bound on $\log a$ bounds $a$ away from zero. The
bound on $S$ then bounds $|b|$ using one residual. These
bounds put the superlevel set in a compact rectangle inside the domain.

```lean
theorem tendsto_inverseLogLik_cocompact {g : ℝ → ℝ} (hg : LinearLowerBound g)
    {x : Fin n → ℝ} (hx : Nonconstant x) :
    Tendsto (fun p : inverseDomain ↦ inverseLogLik g x p) (cocompact inverseDomain) atBot
```

`inverseEquiv` pairs the inverse-scale map with the inverse
$(a,b)\mapsto(b/a,1/a)$.
`inverseLogLik_inverseEquiv` states pointwise equality of the two
objectives under the map. This equality and the bijection transfer
maximizers in both directions.

```lean
def inverseEquiv : domain ≃ inverseDomain

theorem inverseLogLik_inverseEquiv (g : ℝ → ℝ) (x : Fin n → ℝ) (p : domain) :
    inverseLogLik g x (inverseEquiv p) = logLik g x p
```

`exists_mle` uses continuity of the concave objective on the open
inverse-scale domain and the attainment theorem from `Generic.Escape`.
`existsUnique_mle` compares any two attained maxima by strict concavity
and transports equality to location and scale.
`not_bddAbove_logLik_constant` fixes the location at the common
observation $m$ and gives a positive scale with log likelihood above
any proposed bound. This rules out a finite maximizer for each of the
four losses on nonempty constant data.

```lean
theorem exists_mle {g : ℝ → ℝ} (hg : ConvexOn ℝ univ g) (ht : LinearLowerBound g)
    {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃ p ∈ domain, IsMaxOn (logLik g x) domain p

theorem existsUnique_mle {g : ℝ → ℝ} (hg : StrictConvexOn ℝ univ g) (ht : LinearLowerBound g)
    {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik g x) domain p

theorem not_bddAbove_logLik_constant (g : ℝ → ℝ) (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (logLik g (fun _ : Fin n ↦ m) '' domain)
```

## Family statements

`logistic_existsUnique_mle` gives one maximizing location and scale for
each nonconstant logistic sample. The loss has strictly increasing
derivative $1-2/(1+e^t)$ and is bounded below by $|t|$.
`gumbel_existsUnique_mle` gives the same result for the maxima Gumbel
family. Its derivative $1-e^{-t}$ is strictly increasing, and its loss
is also bounded below by $|t|$.
`normal_existsUnique_mle` gives the same result for the normal family.
The quadratic loss has derivative $t$ and is bounded below by
$|t|-1/2$. [Family module](../../lean/UnivariateML/Family/LocScaleInstances.lean)

```lean
theorem logistic_existsUnique_mle {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik logisticLoss x) domain p

theorem gumbel_existsUnique_mle {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik gumbelLoss x) domain p

theorem normal_existsUnique_mle {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik normalLoss x) domain p
```

`laplace_concave` gives concavity in inverse-scale coordinates.
`laplace_exists_mle` gives a maximizing pair for each nonconstant
sample. The absolute-value loss is convex by the triangle inequality
and meets the lower bound with $c=1$ and $d=0$. Its affine pieces
preclude applying the strict-convexity theorem.

```lean
theorem laplace_concave (x : Fin n → ℝ) :
    ConcaveOn ℝ inverseDomain (inverseLogLik laplaceLoss x)

theorem laplace_exists_mle {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃ p ∈ domain, IsMaxOn (logLik laplaceLoss x) domain p
```

## Proof effort

| Target | Relative effort | Main work |
| --- | --- | --- |
| 1. Concavity | Moderate | Affine residuals, the two sources of strictness, and the nonempty finite sum. |
| 2. Coercivity and existence | Highest | Bounds for both coordinates and for distance from the excluded boundary, compactness on the feasible subtype, then continuity and attainment through `Generic.Escape`. |
| 3. Uniqueness | Moderate | The positive-scale bijection, equality of objectives, and transport of `IsMaxOn`. |
| 4. Family applications | Moderate | Derivative monotonicity and elementary tail bounds. The main family inequality was the Gumbel negative tail, using $e^u\ge(1+u/2)^2\ge 2u$ for $u\ge0$. |
| 5. Dossier comparison | Low | Checked the joint-fit claims and the scope of the more detailed family statements. |

## Verification

On the workstation, both standalone `lake env lean` checks passed.
The explicit `lake build +UnivariateML.Family.LocScaleInstances`
built both new modules and finished successfully with 6673 jobs and no
warnings from the new modules. The final plain `lake build` exited
successfully and reported `Build completed successfully (6690 jobs).`
The root build reported existing line-length warnings in
`Weibull.lean` (lines 639 and 675) and a flexible-`simp` warning in
`GPD.lean` (line 260). All commands used the workstation’s single-slot
wrapper and the existing Mathlib cache. The new proofs use no
`sorry`, `admit`, `axiom`, or `native_decide`.
