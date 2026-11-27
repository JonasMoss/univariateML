# Exponential-sum zero counts and four-level separation in Lean

TASK-441 · 2026-11-27

Lean proves the distinct-zero bound for real exponential sums and generalized polynomials, with an identity criterion for merged coefficients. Eleven finite-candidate separation theorems cover every named regime and the additional generalized-beta hold of scale and nonunit upper shape. They compare derivatives in the original observation coordinate on strict support. Each theorem uses four distinct levels, regardless of their multiplicities in the sample. [Generic module](../../lean/UnivariateML/Generic/ExpSum.lean) · [Family module](../../lean/UnivariateML/Family/ExpSumSeparation.lean) · [Atlas rows](../existence-atlas/lean-map.d/lean-expsum.csv)

## Source discrepancy

**Fixed-endpoint generalized beta has a score alias at upper shape one.** The shape report says the four-rate argument separates all finite candidates including the unit-shape member. At endpoint one, upper shape one, the pairs (lower shape, power) `(2, 1)` and `(1, 2)` both give density `2x`. The theorem identifies the full label if either upper shape is nonunit, and identifies the common product of lower shape and power when both upper shapes are one. The later atlas review gives the strict comparison that excludes the unit-shape class from winning when the moving power cutoff is reachable. That comparison needs its own Lean likelihood and path argument. When upper shape is held at one the ridge persists, as covered by the prior AE2 result. [Shape report](2026-11-27-uniq-shape-math-report.md#genbeta-held-scale) · [Atlas review](2026-11-27-atlas-urev2-report.md#proof-errors-and-scope-corrections) · [AE2 report](2026-11-27-lean-ae2-report.md)

## Proof and scope

Rates are indexed by a finite set of reals. Sort a finite set of distinct zeros and choose the required number of consecutive entries. In the induction step, multiply by the reciprocal of one exponential so its coefficient becomes a constant. The derivative gives one zero in each successive interval by Rolle's theorem, with all those zeros distinct. Multiplication by the removed exponential restores the original remaining rate indices. The induction sets each differentiated coefficient to zero, where distinctness of the rates makes its rate-difference factor nonzero. Substitution at a selected zero disposes of the constant. Applying the increasing logarithm to positive zeros gives the real-power corollary.

On the strict-support log chart, the observation derivative, after cancellation of the common reciprocal observation or excess, has form `A + D exp(p t) / (1 + S exp(p t))`. For unequal positive powers, the cleared equation has distinct rates `0, p, q, p + q`. Vanishing coefficients force equal constant terms and zero nonconstant numerators. For equal powers, the two middle terms are combined into one coefficient at `p`. Vanishing at rates `0, p, 2p` identifies the constant and numerator, and identifies the denominator coefficient when the numerator is nonzero. The GB2 chart always has a nonzero numerator and recovers both beta shapes, the power and the scale. Inverse Burr uses its reciprocal-power kernel and a checked equality of derivatives with the positive-power GB2 chart with upper beta shape one.

The source gives a bound with multiplicities. The Lean result uses the permitted finite-set formulation and bounds distinct zeros. It covers every separation in this report. A multiplicity extension would need a weighted Rolle step that carries a zero of order `m` into a zero of order `m − 1` of the derivative, alongside the zeros in the disjoint gaps, and would need to show that multiplication by a nonvanishing exponential preserves the assigned orders. The optional Descartes refinement remains open here. The present induction only tracks the number of rates. A sign-refined induction needs a boundary sign argument to decide when differentiation loses a zero and when it can recover one, as well as a convention that drops zero coefficients before counting changes. [Four-rate argument and face charts](2026-11-27-uniq-shape-math-data/method.md#logistic-scores)

## Effort and remaining incidence by regime

The pair dimension in the table is twice the number of free coordinates of a finite candidate. On the strict four-level stratum, the equal-score incidence set of inequivalent finite pairs is empty in every row. A dimension-based projection for those pairs is therefore unnecessary there, even when the ambient pair dimension exceeds four. The remaining finite-winner step is to verify the hypotheses of the compact-envelope theorem for each open chart. This requires a continuous normalized fixed-count log likelihood, jointly continuous sample derivative, and differentiability in the level coordinates. The derivative weights are the positive counts, which cancel in each coordinate in an equal-gradient comparison. Charts with a contact require their composed height and derivative as the contact level moves. Face assertions still need the score comparisons and smooth comparison charts specified below. Where the intended argument uses a projected incidence bound, it needs the dimension of the allowed competing pair, an account of contact coordinates, and a null projection for analytic score-equality fibers on the strict noncontact domain. [Envelope theorem](2026-11-27-uniq-generic-math-report.md#the-theorem) · [Lean envelope and open-chart API](../../lean/UnivariateML/Generic/UniqueAE.lean) · [Face chart arguments](2026-11-27-uniq-shape-math-data/method.md#comparison-and-incidence)

| Family | Held | Pair dimension | Effort and remaining comparisons |
| --- | --- | ---: | --- |
| GB2 | All free | 8 | Main family proof. Checked the physical derivative, four-rate and coincident-rate algebra, and recovery of the original coefficients. The normal, two transformed gamma, two cutoff, and asymmetric Laplace comparisons need their own charts. At a Laplace join hitting a level, use the moving-knot height and compare scores on the noncontacts. |
| GB2 | `shape1` | 6 | Restricted the upper beta coordinate in the main proof. The inverse transformed gamma has held gamma shape, and the upper cutoff comparison uses the moving contact. |
| GB2 | `shape3` | 6 | Restricted the lower beta coordinate. The transformed gamma has held gamma shape, and the lower cutoff comparison uses the moving contact. |
| Feller–Pareto | `min` | 8 | Reused the main derivative and identification proof with log positive excess. The six GB2 face comparisons transfer on this strict chart. An observation equal to the held minimum needs the endpoint treatment. |
| Feller–Pareto | `min+shape1` | 6 | Restricted the shifted upper beta coordinate. The inverse transformed gamma and upper cutoff require the corresponding shifted comparison charts, with the threshold contact handled separately. |
| Feller–Pareto | `min+shape3` | 6 | Restricted the shifted lower beta coordinate. The transformed gamma and lower cutoff require the corresponding shifted comparison charts, with the threshold contact handled separately. |
| Burr XII | All free | 6 | Set the lower beta shape to one. The Weibull and lower cutoff face comparisons still need to be attached to the smooth likelihood charts. |
| Inverse Burr | All free | 6 | Checked the reciprocal-power kernel transfer in the original observation coordinate, including the parameter-only constant. The inverse Weibull and upper cutoff require the reflected comparison charts. |
| Pareto IV | `min` | 6 | Set the lower beta shape to one in the shifted chart. The Burr face comparisons transfer on positive excesses, and an observed threshold needs its own endpoint analysis. |
| Generalized beta | `scale` | 6 | Checked strict-support denominators and identified the unit-shape rate class. Finite-label uniqueness also needs the strict moving-power comparison against that class. The fixed-endpoint gamma comparison has pair dimension at most five (three finite plus two gamma) and needs the analytic nonidentity and projected null bound on six levels. The moving-power comparison uses the composed height at the maximum and nonunit scores at the noncontacts. |
| Generalized beta | `scale+shape2` | 4 | Restricted the fixed-endpoint theorem to a common nonunit upper shape. The fixed-endpoint, fixed-shape gamma comparison has pair dimension at most three (two finite plus one gamma) and needs the analytic nonidentity and projected null bound on four levels. For upper shape below one the additional fixed-endpoint power score is constant, and above one the inward comparison excludes that face. |

The generic induction took the main zero-count effort. The proof makes the consecutive zero intervals explicit and returns to the original rate set after differentiation. The generalized-polynomial and two identity formulations were short consequences. The family section took most of the remaining work, concentrated in the shared rational comparison, the two derivative charts, and the inverse Burr transfer. The held-coordinate results are direct restrictions. No attempts were needed for the moving-endpoint regimes whose written proofs instead use a pole or tail identification. Attainment, exact tie witnesses, stationary component comparisons, finite/face separation and projected incidence remain outside the checked results.

## Verification

Both modules passed standalone checks on the workstation using `/tmp/umvl-cpu/run nice -n 10 /home/jonas/.elan/bin/lake env lean <file>` from `dev/lean/`. The final `lake build` used the same slot wrapper and niceness with `LEAN_NUM_THREADS=1` and both temporary root imports. It exited zero with `Build completed successfully (8316 jobs).` Existing modules emitted linter warnings. The new modules emitted none in the final build. The temporary imports were removed, and `git diff --exit-code dev/lean/UnivariateML.lean` passed.

## Checked Lean statements

### `UnivariateML.ExpSum.identity_of_card_le`

As many distinct zeros as merged rates force every merged coefficient to vanish. [ExpSum.lean](../../lean/UnivariateML/Generic/ExpSum.lean#L93)

```lean
theorem identity_of_card_le (s z : Finset ℝ) (c : ℝ → ℝ) (h : s.card ≤ z.card)
    (hz : ∀ t ∈ z, ∑ r ∈ s, c r * Real.exp (r * t) = 0) :
    ∀ r ∈ s, c r = 0
```

### `UnivariateML.ExpSum.card_zeros_lt`

A sum with a nonzero coefficient has at most one fewer distinct zeros than rates. [ExpSum.lean](../../lean/UnivariateML/Generic/ExpSum.lean#L101)

```lean
theorem card_zeros_lt (s z : Finset ℝ) (c : ℝ → ℝ)
    (hc : ∃ r ∈ s, c r ≠ 0)
    (hz : ∀ t ∈ z, ∑ r ∈ s, c r * Real.exp (r * t) = 0) :
    z.card < s.card
```

### `UnivariateML.ExpSum.card_positive_zeros_lt`

A generalized polynomial with a nonzero coefficient has at most one fewer distinct positive zeros than exponents. [ExpSum.lean](../../lean/UnivariateML/Generic/ExpSum.lean#L110)

```lean
theorem card_positive_zeros_lt (s z : Finset ℝ) (c : ℝ → ℝ)
    (hc : ∃ r ∈ s, c r ≠ 0) (hp : ∀ x ∈ z, 0 < x)
    (hz : ∀ x ∈ z, ∑ r ∈ s, c r * x ^ r = 0) :
    z.card < s.card
```

### `UnivariateML.ExpSum.identity_of_interval`

Vanishing on a nonempty open interval forces every merged coefficient to vanish. [ExpSum.lean](../../lean/UnivariateML/Generic/ExpSum.lean#L124)

```lean
theorem identity_of_interval (s : Finset ℝ) (c : ℝ → ℝ) {a b : ℝ} (hab : a < b)
    (hz : ∀ t ∈ Set.Ioo a b, ∑ r ∈ s, c r * Real.exp (r * t) = 0) :
    ∀ r ∈ s, c r = 0
```

### `UnivariateML.ExpSumSeparation.genbeta_held_scale_separation`

Four interior levels determine both shapes and the power, except that two unit-upper-shape labels may agree at the common product of lower shape and power. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L199)

```lean
theorem genbeta_held_scale_separation {s l u p l' u' p' : ℝ}
    (hp : 0 < p) (hp' : 0 < p')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x ∧ x < s)
    (h : ∀ x ∈ z, deriv (genbetaKernel s l u p) x = deriv (genbetaKernel s l' u' p') x) :
    (l = l' ∧ u = u' ∧ p = p') ∨ (u = 1 ∧ u' = 1 ∧ l * p = l' * p')
```

### `UnivariateML.ExpSumSeparation.genbeta_held_scale_shape2_separation`

At a held endpoint and a held nonunit upper shape, four interior levels determine lower shape and power. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L245)

```lean
theorem genbeta_held_scale_shape2_separation {s l u p l' p' : ℝ}
    (hu : u ≠ 1) (hp : 0 < p) (hp' : 0 < p')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x ∧ x < s)
    (h : ∀ x ∈ z, deriv (genbetaKernel s l u p) x = deriv (genbetaKernel s l' u p') x) :
    l = l' ∧ p = p'
```

### `UnivariateML.ExpSumSeparation.gb2_separation`

Four positive levels determine upper shape, power, lower shape and scale. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L255)

```lean
theorem gb2_separation {u p l s u' p' l' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l) (hs : 0 < s)
    (hu' : 0 < u') (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel 0 u p l s) x = deriv (gb2Kernel 0 u' p' l' s') x) :
    u = u' ∧ p = p' ∧ l = l' ∧ s = s'
```

### `UnivariateML.ExpSumSeparation.gb2_held_shape1_separation`

At held upper beta shape, four positive levels determine power, lower beta shape and scale. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L265)

```lean
theorem gb2_held_shape1_separation {u p l s p' l' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel 0 u p l s) x = deriv (gb2Kernel 0 u p' l' s') x) :
    p = p' ∧ l = l' ∧ s = s'
```

### `UnivariateML.ExpSumSeparation.gb2_held_shape3_separation`

At held lower beta shape, four positive levels determine upper beta shape, power and scale. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L275)

```lean
theorem gb2_held_shape3_separation {u p l s u' p' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (hu' : 0 < u') (hp' : 0 < p') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel 0 u p l s) x = deriv (gb2Kernel 0 u' p' l s') x) :
    u = u' ∧ p = p' ∧ s = s'
```

### `UnivariateML.ExpSumSeparation.fpareto_held_min_separation`

At a held minimum, four strictly positive excesses determine both beta shapes, power and scale. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L285)

```lean
theorem fpareto_held_min_separation {m u p l s u' p' l' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l) (hs : 0 < s)
    (hu' : 0 < u') (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, m < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel m u p l s) x = deriv (gb2Kernel m u' p' l' s') x) :
    u = u' ∧ p = p' ∧ l = l' ∧ s = s'
```

### `UnivariateML.ExpSumSeparation.fpareto_held_min_shape1_separation`

At held minimum and upper beta shape, four strictly positive excesses determine power, lower beta shape and scale. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L296)

```lean
theorem fpareto_held_min_shape1_separation {m u p l s p' l' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, m < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel m u p l s) x = deriv (gb2Kernel m u p' l' s') x) :
    p = p' ∧ l = l' ∧ s = s'
```

### `UnivariateML.ExpSumSeparation.fpareto_held_min_shape3_separation`

At held minimum and lower beta shape, four strictly positive excesses determine upper beta shape, power and scale. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L307)

```lean
theorem fpareto_held_min_shape3_separation {m u p l s u' p' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (hu' : 0 < u') (hp' : 0 < p') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, m < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel m u p l s) x = deriv (gb2Kernel m u' p' l s') x) :
    u = u' ∧ p = p' ∧ s = s'
```

### `UnivariateML.ExpSumSeparation.burr_separation`

Four positive levels determine Burr XII shape1, power and scale. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L317)

```lean
theorem burr_separation {u p s u' p' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hs : 0 < s)
    (hu' : 0 < u') (hp' : 0 < p') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel 0 u p 1 s) x = deriv (gb2Kernel 0 u' p' 1 s') x) :
    u = u' ∧ p = p' ∧ s = s'
```

### `UnivariateML.ExpSumSeparation.invburr_separation`

Four positive levels determine inverse Burr shape1, power and scale in the reciprocal-power kernel. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L351)

```lean
theorem invburr_separation {p l s p' l' s' : ℝ}
    (hp : 0 < p) (hl : 0 < l) (hs : 0 < s)
    (hp' : 0 < p') (hl' : 0 < l') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, 0 < x)
    (h : ∀ x ∈ z, deriv (invburrKernel l p s) x = deriv (invburrKernel l' p' s') x) :
    l = l' ∧ p = p' ∧ s = s'
```

### `UnivariateML.ExpSumSeparation.pareto4_held_min_separation`

At a held minimum, four strictly positive excesses determine Pareto IV shape1, power and scale. [ExpSumSeparation.lean](../../lean/UnivariateML/Family/ExpSumSeparation.lean#L363)

```lean
theorem pareto4_held_min_separation {m u p s u' p' s' : ℝ}
    (hu : 0 < u) (hp : 0 < p) (hs : 0 < s)
    (hu' : 0 < u') (hp' : 0 < p') (hs' : 0 < s')
    (z : Finset ℝ) (hz : 4 ≤ z.card) (hx : ∀ x ∈ z, m < x)
    (h : ∀ x ∈ z, deriv (gb2Kernel m u p 1 s) x = deriv (gb2Kernel m u' p' 1 s') x) :
    u = u' ∧ p = p' ∧ s = s'
```

