# Lean discrete maximum likelihood results

TASK-316 covers ordinary Poisson, binomial with at least one coefficient held,
geometric on counts starting at zero, and Poisson conditioned on positive counts.
The results are in [Discrete.lean](../../lean/UnivariateML/Family/Discrete.lean).

## Source discrepancies

- **The all-ones zero-truncated Poisson result depends on the parameter domain.**
  On the strictly positive-rate domain the supremum is approached as the
  rate decreases to zero. This is the lower point-mass escape described for a
  nonsingleton positive window in the Poisson dossier and used by the generic
  window fitter on `(0, ∞]`. The dedicated `zero = "truncated"` route accepts
  rate zero and includes a point mass at one. For all-ones data it returns
  rate zero with `Ok`, including when zero is held. The Lean module proves
  both the escape and the attained boundary maximum.
  [Poisson dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pois/README.md),
  [window fitter](../../../univariateML-cpp/include/cppunivariate/distributions/pois/tpois_mle.hpp),
  [zero modifier fitter](../../../univariateML-cpp/include/cppunivariate/distributions/pois/ztpois_mle.hpp),
  [zero modifier mass](../../../univariateML-cpp/include/cppunivariate/distributions/pois/ztpois_dpqr_core.hpp).
- **The binomial fitter's held-probability tie detection misses a nonconstant
  sample.** For counts `(0, 6)` with probability held at `1/2`, Lean proves
  that sizes 7 and 8 are the only maximizers. At size 7 the two observation
  factors for the move to size 8 are `1/2` and `2`, giving an exact product
  of one. The fitter's explicit two-maximizer branch requires constant
  counts. The general held-probability branch returns one size and leaves
  the result's default uniqueness flag set. This is a source-inspection
  finding supported by an exact Lean counterexample.
  [Binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md),
  [binomial fitter](../../../univariateML-cpp/include/cppunivariate/distributions/binom/mle.hpp).

## Scope and proof effort

| Target | Proof work |
| --- | --- |
| Poisson | A strict log-ratio inequality gives global uniqueness in the interior. Direct endpoint comparisons cover all-zero data and held-rate compatibility. |
| Binomial, held size or held pair | Two weighted log-ratio bounds give the global probability estimate, with separate handling of zero weights. The product of binomial masses verifies exactly which held pairs give positive likelihood and when the endpoint likelihood equals one. |
| Binomial, held probability | The adjacent choose identity gives the log-likelihood increment. Above a finite cutoff every log-ratio term is negative. The first nonpositive increment and strict decrease for samples with a positive count give the exact maximizing size set. The all-zero case uses directly negative increments. This proof uses a finite bound in place of the dossier's increment limit. The explicit nonconstant tie is checked separately. |
| Geometric | The probability objective is the two-log-term objective with weights one and the sample mean. Restricting the domain to positive probability preserves its unique maximizer. |
| Zero-truncated Poisson | Differentiation proves strict increase of the conditional mean. The exponential derivative at zero gives the right-hand mean limit and the all-ones likelihood limit. The generic score lemma gives the unique maximizing root in log-rate coordinates. The generic limit bound identifies the open-domain supremum. A separate comparison handles the rate-zero extension and held-rate compatibility. |

The sample uses natural counts, with casts to real numbers for the likelihood
and mean. Here `a` denotes the mean, `s` and `f` denote mean successes and
failures for fixed binomial size, and `m` is the binomial size. The reduced
objectives use per-observation likelihoods up to positive factors independent
of the fitted coefficient. The held-probability binomial proof uses total log
likelihood so the size-dependent choose terms are retained.

All requested mathematical targets are settled. The results concern exact
arithmetic. Floating-point certificate soundness, the general binomial fit
with both coefficients free, general truncation windows, and other zero
modifiers are outside this job. The reported binomial outcome discrepancy
remains in the C++ fitter.

## Lean checks

Both checks ran on the workstation under the one-slot wrapper with low CPU
priority.

- `lake env lean UnivariateML/Family/Discrete.lean` passed with exit code 0
  and no diagnostics. This directly checks the new module.
- `lake build` passed with exit code 0 and reported 7294 jobs. Existing
  modules emitted linter diagnostics. The library root import is reserved
  for the coordinator, so the direct file check supplies validation of
  `Discrete.lean`.

## Public theorem statements

Names below are relative to `UnivariateML.Discrete`. Statements are copied
from the checked file.

### Sample means

**`avg_eq_zero_iff`**

The mean is zero exactly when every count is zero.

```lean
theorem avg_eq_zero_iff {n : ℕ} (x : Fin n → ℕ) :
    avg x = 0 ↔ ∀ i, x i = 0
```

**`avg_le`**

A common upper bound on the counts also bounds their mean.

```lean
theorem avg_le {n m : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) : avg x ≤ m
```

**`avg_eq_of_lowerBound`**

For a nonempty sample, the mean equals a common lower bound exactly when every count equals that bound.

```lean
theorem avg_eq_of_lowerBound {n m : ℕ} (x : Fin n → ℕ) (hn : 0 < n)
    (hx : ∀ i, m ≤ x i) : avg x = m ↔ ∀ i, x i = m
```

### Poisson

**`Poisson.held_positive_iff`**

A held positive rate always gives positive likelihood. A held zero rate gives positive likelihood exactly for an all-zero sample.

```lean
theorem held_positive_iff {a lam : ℝ} (hl : 0 ≤ lam) :
    0 < likelihood a lam ↔ 0 < lam ∨ a = 0
```

**`Poisson.unique_mle`**

The sample mean is the only maximizing rate on the closed nonnegative domain. The all-zero sample therefore attains its maximum at zero.

```lean
theorem unique_mle {a : ℝ} (ha : 0 ≤ a) :
    ∀ lam ∈ Ici (0 : ℝ), IsMaxOn (likelihood a) (Ici 0) lam ↔ lam = a
```

### Binomial

**`Binomial.held_positive_iff`**

At probability zero there must be zero mean successes. At probability one there must be zero mean failures.

```lean
theorem held_positive_iff {s f p : ℝ} :
    0 < likelihood s f p ↔ (p = 0 → s = 0) ∧ (p = 1 → f = 0)
```

**`Binomial.fixed_size_mle`**

For positive total weight, the unique probability is the success weight divided by total weight, including probabilities zero and one.

```lean
theorem fixed_size_mle {s f : ℝ} (hs : 0 ≤ s) (hf : 0 ≤ f) (ht : 0 < s + f) :
    ∀ p ∈ Icc (0 : ℝ) 1, IsMaxOn (likelihood s f) (Icc 0 1) p ↔ p = s / (s + f)
```

**`Binomial.zero_size_mle`**

The reduced binomial likelihood is constant at size zero, so all feasible probabilities maximize it.

```lean
theorem zero_size_mle (p : ℝ) :
    IsMaxOn (likelihood 0 0) (Icc 0 1) p
```

**`Binomial.sample_fixed_size_mle`**

For feasible counts and specified positive integer size, the unique probability is the sample mean divided by size.

```lean
theorem sample_fixed_size_mle {n m : ℕ} (x : Fin n → ℕ) (hm : 0 < m)
    (hx : ∀ i, x i ≤ m) :
    ∀ p ∈ Icc (0 : ℝ) 1,
      IsMaxOn (likelihood (avg x) (m - avg x)) (Icc 0 1) p ↔ p = avg x / m
```

**`Binomial.zero_size_compatible_iff`**

Held size zero is compatible exactly with an all-zero sample.

```lean
theorem zero_size_compatible_iff {n : ℕ} (x : Fin n → ℕ) (p : ℝ) :
    compatible x 0 p ↔ ∀ i, x i = 0
```

**`Binomial.zero_prob_compatible_iff`**

Held probability zero is compatible at every size for all-zero samples.

```lean
theorem zero_prob_compatible_iff {n : ℕ} (x : Fin n → ℕ) (m : ℕ) :
    compatible x m 0 ↔ ∀ i, x i = 0
```

**`Binomial.one_prob_compatible_iff`**

Held probability one is compatible only at the common observed count.

```lean
theorem one_prob_compatible_iff {n : ℕ} (x : Fin n → ℕ) (m : ℕ) :
    compatible x m 1 ↔ ∀ i, x i = m
```

**`Binomial.sample_likelihood_pos_iff`**

For a held pair, positive sample likelihood is equivalent to count support and the appropriate endpoint conditions.

```lean
theorem sample_likelihood_pos_iff {n m : ℕ} (x : Fin n → ℕ) {p : ℝ} (hp : p ∈ Icc 0 1) :
    0 < sampleLikelihood x m p ↔ compatible x m p
```

**`Binomial.endpoint_sample_likelihood`**

A compatible pair with endpoint probability gives sample likelihood one.

```lean
theorem endpoint_sample_likelihood {n m : ℕ} (x : Fin n → ℕ) {p : ℝ}
    (hp : p = 0 ∨ p = 1) (h : compatible x m p) : sampleLikelihood x m p = 1
```

**`Binomial.sizeLogLik_eq_log`**

At an interior probability and feasible size, the size objective is the log of the product of binomial masses.

```lean
theorem sizeLogLik_eq_log {n m : ℕ} (x : Fin n → ℕ) {p : ℝ}
    (hp : p ∈ Ioo 0 1) (hx : ∀ i, x i ≤ m) :
    sizeLogLik x p m = log (sampleLikelihood x m p)
```

**`Binomial.size_increment_eq`**

The increment in total log likelihood from a feasible size to its successor equals the sum of the observation log-ratios.

```lean
theorem size_increment_eq {n m : ℕ} (x : Fin n → ℕ) (p : ℝ) (hx : ∀ i, x i ≤ m) :
    sizeLogLik x p (m + 1) - sizeLogLik x p m = sizeIncrement x p m
```

**`Binomial.held_interior_size_mle`**

A finite maximizing size exists. The only additional maximizer is the successor when the increment at the first maximizing size is exactly zero.

```lean
theorem held_interior_size_mle {n b : ℕ} (x : Fin n → ℕ) (hn : 0 < n)
    {p : ℝ} (hp : p ∈ Ioo 0 1) (hb : ∀ m : ℕ, b ≤ m ↔ ∀ i, x i ≤ m) :
    ∃ r ≥ b, ∀ m ≥ b, IsMaxOn (sizeLogLik x p) (Ici b) m ↔
      m = r ∨ (m = r + 1 ∧ sizeIncrement x p r = 0)
```

**`Binomial.nonconstant_tie`**

Counts zero and six at probability one half give equal log likelihood at sizes seven and eight.

```lean
theorem nonconstant_tie :
    sizeLogLik ![0, 6] (1 / 2) 8 = sizeLogLik ![0, 6] (1 / 2) 7
```

**`Binomial.nonconstant_tie_maximizers`**

Those two sizes are the entire maximizing set.

```lean
theorem nonconstant_tie_maximizers :
    ∀ m ≥ 6, IsMaxOn (sizeLogLik ![0, 6] (1 / 2)) (Ici 6) m ↔ m = 7 ∨ m = 8
```

### Geometric

**`Geometric.held_positive_iff`**

Every interior probability gives positive likelihood. Probability one requires all-zero data.

```lean
theorem held_positive_iff {a p : ℝ} (hp : p ∈ Ioc (0 : ℝ) 1) :
    0 < likelihood a p ↔ p < 1 ∨ a = 0
```

**`Geometric.unique_mle`**

The unique probability is one divided by one plus the mean. For all-zero data this is the included endpoint one.

```lean
theorem unique_mle {a : ℝ} (ha : 0 ≤ a) :
    ∀ p ∈ Ioc (0 : ℝ) 1, IsMaxOn (likelihood a) (Ioc 0 1) p ↔ p = 1 / (1 + a)
```

### Zero-truncated Poisson

**`ZeroTruncatedPoisson.unique_mle`**

For mean above one, the positive root of the conditional-mean equation exists, is unique, and is the only positive-rate maximizer.

```lean
theorem unique_mle {a : ℝ} (ha : 1 < a) :
    (∃! lam, 0 < lam ∧ conditionalMean lam = a) ∧
      ∀ lam > 0, IsMaxOn (logLik a) (Ioi 0) lam ↔ conditionalMean lam = a
```

**`ZeroTruncatedPoisson.all_ones_limit`**

The reduced log likelihood tends to zero as the rate approaches zero from above.

```lean
theorem all_ones_limit : Tendsto (logLik 1) (𝓝[>] 0) (𝓝 0)
```

**`ZeroTruncatedPoisson.all_ones_strict`**

At every positive rate the all-ones reduced log likelihood is strictly below zero.

```lean
theorem all_ones_strict {lam : ℝ} (hl : 0 < lam) : logLik 1 lam < 0
```

**`ZeroTruncatedPoisson.all_ones_supremum`**

Zero is the least upper bound of the all-ones log likelihood, with no positive-rate maximizer.

```lean
theorem all_ones_supremum :
    IsLUB (logLik 1 '' Ioi 0) 0 ∧ ∀ lam > 0, ¬ IsMaxOn (logLik 1) (Ioi 0) lam
```

**`ZeroTruncatedPoisson.held_positive_iff`**

In the extended domain every positive held rate gives positive likelihood. A held zero rate requires mean one, hence all ones for positive counts.

```lean
theorem held_positive_iff {a lam : ℝ} (hl : 0 ≤ lam) :
    0 < likelihood a lam ↔ 0 < lam ∨ a = 1
```

**`ZeroTruncatedPoisson.all_ones_attained`**

The extended model attains its all-ones maximum uniquely at rate zero.

```lean
theorem all_ones_attained :
    ∀ lam ∈ Ici (0 : ℝ), IsMaxOn (likelihood 1) (Ici 0) lam ↔ lam = 0
```

**`ZeroTruncatedPoisson.closed_domain_mle`**

Adding rate zero to the model preserves the unique positive maximizing root when the mean exceeds one.

```lean
theorem closed_domain_mle {a : ℝ} (ha : 1 < a) :
    ∀ lam ∈ Ici (0 : ℝ), IsMaxOn (likelihood a) (Ici 0) lam ↔
      0 < lam ∧ conditionalMean lam = a
```

**`ZeroTruncatedPoisson.sample_exists_iff`**

A nonempty sample of positive counts has a positive-rate maximizer exactly when some count exceeds one.

```lean
theorem sample_exists_iff {n : ℕ} (x : Fin n → ℕ) (hn : 0 < n)
    (hx : ∀ i, 1 ≤ x i) :
    (∃ lam > 0, IsMaxOn (logLik (avg x)) (Ioi 0) lam) ↔
      ∃ i, 1 < x i
```
