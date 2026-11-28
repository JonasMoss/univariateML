# TASK-488 · Zero-modified and zero-inflated count families in Lean

The modules establish sample-level modification results on the Poisson, geometric, fixed-size binomial and fixed-size negative-binomial conditional charts. They give inflation attainment results for those families, the excluded zero atom for held-inflation Poisson and a strict excluded Bernoulli limit for held-probability discrete Weibull on zero/one samples. The other family results use the actual mass formulas on the hold slices with the observed-mass bounds and positive conditional witness supplied. The [atlas fragment](../existence-atlas/modifier-lean-map.d/lean-zero6.csv) records the precise question and hold for review. Its 148 mappings have empty coverage.

## Source discrepancy

The [held-inflation geometric criterion](../existence-atlas/modifier-proofs.csv) calls the likelihood strictly concave in continuation probability. The claim requires **log likelihood**, as used in the companion mathematical proof. With sample `(3)` and inflation held at zero, the likelihood is `q²(1-q)`. At `q = 1/8, 1/4` its average value is `124/4096`, above `117/4096` at the midpoint. The checked held-inflation theorem uses compactness and a positive witness for attainment. The positive-sample companion theorem proves strict decrease of the log score and identifies its unique root.

The other source distinctions matter for scope. Poisson inflation uses strictly positive ordinary rates. Poisson modification includes the zero-rate conditional atom. Negative-binomial modification at size zero uses the logarithmic positive component. Ordinary size-zero inflation has only the original zero atom. Discrete gamma and discrete Weibull parent concavity or profiling arguments do not automatically apply after subtracting the conditional normalizer. The PIG conditional mean and shape comparisons likewise use a different score from the ordinary PIG profile. [Written proofs](2026-11-27-zero-mod-math-report.md) · [Independent review](2026-11-28-atlas-modcov2-report.md)

## Results and open proof obligations by target

The simple conditional objectives suppress positive carrier factors and use the supplied positive mean. The inflation results use the positive count sum, or the sum less the positive sample size for geometric data. All-zero cases have the empty positive sample. The tagged binomial endpoints stand for the atoms at one and at the held size. The tagged negative-binomial lower endpoint stands for probability one.

| Family | Work checked | Open |
| --- | --- | --- |
| Poisson | Applied the closed conditional mean theorem and the included atom at one to free and held modification. Used the existing positive ZIP compactness result, then proved the held-zero strict bound and the rate-down-to-zero path. | Join the sample likelihood to the unique ordinary-versus-conditional free ZIP solution, including its threshold and matching score. The held-rate score theorem compares all roots with ties. Finiteness of the root set and the two-maxima crossing for 99 zeros and one ten remain open. The held-zero path is explicit. Exhaustion as a statement about all escaping rates is open. |
| Geometric | Transferred the reciprocal positive mean through both modification routes. Extended the free inflation result to the included zero atom in effective-zero coordinates. Used a compact continuation interval for held inflation and excluded its right endpoint using positive likelihood. | Pull the free effective-zero result back to the complete raw inflation fiber. The held-weight score has exactly one interior root and identifies the unique positive fit. The explicit vanishing path at the excluded endpoint remains open. |
| Binomial, held size | Applied the existing finite tagged conditional fit and handled the flat size-one component. Used fixed-size compactness for both inflation designs and checked the height-one union and free size-one product curve. | Join the size-zero modification branch and the raw-probability chart to the sample result. The inflation theorem still needs explicit positive compatibility, the fixed-size two-tilt selection and positive uniqueness at size at least two. Held inflation still needs the size-one clipped probability and polynomial stationary-point coverage. |
| Negative binomial, held size | Applied the existing conditional maximizer. Proved the normalizer witness using a geometric comparison for the logarithmic carrier and the real binomial series for positive size. Checked positive inflation attainment including the admitted zero endpoint as a losing candidate, and checked the exact all-zero height-one set separately. | State the matched public probability and exclude every normalized positive endpoint escape in the same sample theorem. Free-inflation positive uniqueness and the finite held-score superlevel root list remain open. |
| Beta-binomial | Reused the finite rising-factorial mass and built a discrete-size slice with independent alpha and beta holds. In particular the original size-zero atom can be passed to the inflation likelihood. The ordinary dispersion condition uses the unconditional objective, so it did not discharge the positive conditional problem. | Derive the size-one flat conditional result and the compatible fixed-size binomial/endpoint-mixture and growing-size NB/Poisson face lists. Transport their actual reaching sequences and rank them jointly with finite candidates. |
| Beta-negative-binomial | Used the existing Gamma-ratio log mass on positive shape slices in the public label order. The transfer evaluates mass zero before conditioning and retains all finite conditional maximizers. The held-inflation theorem transports all three compatible zero-only paths and shows their common height is strict. | Turn the positive-index recurrence argument into a normalized convergence proof, retaining the ordered and swapped numerator paths under the individual holds. Join the conditional NB, logarithmic, Poisson and lower-atom possibilities and the original NB/Poisson comparisons. The ordinary held-shape profiles cannot supply their conditional analogues directly. |
| CMP | Instantiated the normalized exponential weights and the lambda-zero tag. The positive conditional domain excludes the parent zero atom. Checked the shared likelihood factorization on both the geometric boundary and positive-dispersion domain via the supplied mass bounds. | Join existing scalar positive-ray results to the sample likelihood, prove the conditional and parent KKT branch selection, and exhaust the point/adjacent escapes. Include the held-rate Bernoulli limit and the tied free-inflation weights along that face. The concurrent truncation-window job was outside this work. |
| Discretised gamma | Used the parent CDF differences at integers on the shape/rate hold domain. Proved both zero-only inflation paths at a held weight below one, using the parent Gamma CDF limits. The fixed-shape and fixed-rate ordinary profiles were examined for transfer. Their normalizers differ from the positive-component fit. | Prove the conditional shape-zero and concentration paths with hold checks. Prove uniform loss of fixed positive cells on shape-zero paths for inflation, then compare the point and adjacent limits with all finite candidates for each compatible slice. |
| Discrete Weibull | Used the public survival formula. On all-one positives with probability held, proved the conditional probability of one is below one at every finite positive shape and tends to one. Factoring the actual inflated likelihood gives the strict excluded Bernoulli height at every compatible fixed inflation weight. | Extend this to the whole held-probability regime, the free-inflation envelope on that face and held-shape scalar matching. Establish the all-free Pareto and concentration exhaustion and comparisons, including ties and the zero-only held-inflation zero-atom path. |
| PIG | Used the public mean/shape chart, the existing Bessel polynomial and the shape-zero tag. The pre-existing parent variance comparison concerns the ordinary profile. Conditioning changes its zero-cell denominator. | Establish the normalized conditional Sibuya/Poisson equality and all compatible positive-shape and atom paths. Prove the inflated Poisson/Lévy comparison with holds, including the fixed-Bessel-argument inward derivative that removes the Lévy contender. |

The generic comparison theorem assumes the finite critical set contains every maximizer, each supplied boundary height is reachable, and a strict gap gives attainment. It proves the finite comparison and equality sets under those hypotheses. The family-specific critical and escape hypotheses remain open beyond the listed cases. Their explicit inputs are `zeroMass`, `positiveMass` and `conditionalWitness`. The two Weibull path theorems additionally derive the strict bound and its sharpness directly for their sample branch.

## Verification

All 6 new files passed the foreground `lake env lean` checks on the workstation. The foreground aggregate `lake build` with the temporary imports passed at **8502 jobs**. The imports were restored and `git diff --exit-code dev/lean/UnivariateML.lean` passed. `python3 dev/tools/existence_atlas.py --check` passed. The new proofs elaborate without placeholders.

## Lean statements

### UnivariateML.Zero6

**`modified_fit`**. A positive-likelihood modified maximizer is exactly a selected compatible weight and, when positives occur, a parent conditional maximizer. [Zero6.lean line 42](../../lean/UnivariateML/Family/Zero6.lean#L42)

```lean
theorem modified_fit {P : Type*} {D : Set P} {L : P → ℝ} {z k : ℕ}
    (hn : 0 < z+k) (hold : Option ℝ) (hL : ∀ q ∈ D, 0 ≤ L q)
    (hpos : k ≠ 0 → ∃ q ∈ D, 0 < L q)
    {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ weightDomain hold) :
    (IsMaxOn (modified z k L) (D ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k L (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (k ≠ 0 → IsMaxOn L D p)
```

**`pois_modified`**. The modified positive fit is the matched conditional mean, with rate zero admitted exactly on all-one positives. [Zero6.lean line 123](../../lean/UnivariateML/Family/Zero6.lean#L123)

```lean
theorem pois_modified {z k : ℕ} (hn : 0 < z+k) {m : ℝ} (hm : 1 ≤ m)
    (hold : Option ℝ) {p w : ℝ} (hp : p ∈ Ici 0) (hw : w ∈ weightDomain hold) :
    let F := Discrete.ZeroTruncatedPoisson.likelihood m
    (IsMaxOn (modified z k (fun q ↦ F q ^ k)) (Ici 0 ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k (fun q ↦ F q ^ k) (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (k ≠ 0 → (m = 1 ∧ p = 0 ∨ 1 < m ∧ 0 < p ∧
        Discrete.ZeroTruncatedPoisson.conditionalMean p = m))
```

**`geom_modified`**. The modified positive probability is the reciprocal positive mean. [Zero6.lean line 151](../../lean/UnivariateML/Family/Zero6.lean#L151)

```lean
theorem geom_modified {z k : ℕ} (hn : 0 < z+k) {m : ℝ} (hm : 1 ≤ m)
    (hold : Option ℝ) {p w : ℝ} (hp : p ∈ Ioc 0 1) (hw : w ∈ weightDomain hold) :
    let F := Discrete.Geometric.likelihood (m-1)
    (IsMaxOn (modified z k (fun q ↦ F q ^ k)) (Ioc 0 1 ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k (fun q ↦ F q ^ k) (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧ (k ≠ 0 → p = 1/m)
```

**`nbinom_modified`**. The existing unique conditional optimizer carries to either weight design, including the logarithmic component. [Zero6.lean line 175](../../lean/UnivariateML/Family/Zero6.lean#L175)

```lean
theorem nbinom_modified {r : ℝ} (hr : 0 ≤ r) {z k : ℕ} (hn : 0 < z+k)
    {m : ℝ} (hm : 1 ≤ m) (hold : Option ℝ) {p : Option ℝ} {w : ℝ}
    (hp : p ∈ TruncCount.closedLowerDomain (Iio 0)) (hw : w ∈ weightDomain hold) :
    let c := if r = 0 then TruncCount.logSeriesCarrier
      else fun j ↦ TruncCount.negBinomCarrier r (1+j)
    let D := TruncCount.closedLowerDomain (Iio 0)
    let F := TruncCount.closedLowerLikelihood c (m-1)
    (IsMaxOn (modified z k (fun q ↦ F q ^ k)) (D ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k (fun q ↦ F q ^ k) (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (∀ _ : k ≠ 0, p = Classical.choose (TruncCount.nbinom_zt_fit hr hm).exists)
```

**`binom_modified`**. The conditional winner is unique at size at least two. At size one every probability is tied in the positive component. [Zero6.lean line 256](../../lean/UnivariateML/Family/Zero6.lean#L256)

```lean
theorem binom_modified {N z k : ℕ} (hN : 0 < N) (hn : 0 < z+k)
    {m : ℝ} (hm : m ∈ Icc (1 : ℝ) N) (hold : Option ℝ) {p : Sum ℝ Bool} {w : ℝ}
    (hp : p ∈ TruncCount.finiteCountDomain true true) (hw : w ∈ weightDomain hold) :
    let D := TruncCount.finiteCountDomain true true
    let F := TruncCount.finiteCountLikelihood 1 N (fun j ↦ (N.choose j : ℝ)) m
    (IsMaxOn (modified z k (fun q ↦ F q ^ k)) (D ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k (fun q ↦ F q ^ k) (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (∀ _ : k ≠ 0, ∀ h : 1 < N,
        p = Classical.choose (TruncCount.binom_zt_fit h hm).exists)
```


### UnivariateML.Zero6Faces

**`reachable_le`**. A reachable likelihood limit is bounded by the supremum. [Zero6Faces.lean line 22](../../lean/UnivariateML/Family/Zero6Faces.lean#L22)

```lean
theorem reachable_le {P : Type*} {D : Set P} {L : P → ℝ} {s h : ℝ}
    (hs : IsLUB (L '' D) s) {F : Filter P} [F.NeBot]
    (hD : ∀ᶠ p in F, p ∈ D) (hL : Tendsto L F (𝓝 h)) : h ≤ s
```

**`compare`**. With critical coverage, reachability and the strict-gap property, compare finite candidates to the escape height and keep every equality at the global supremum. [Zero6Faces.lean line 30](../../lean/UnivariateML/Family/Zero6Faces.lean#L30)

```lean
theorem compare {P A : Type*} {D C : Set P} {E : Set A} {L : P → ℝ} {H : A → ℝ}
    {s f : ℝ} (hs : IsLUB (L '' D) s) (hf : IsLUB (H '' E) f)
    (reachable : ∀ e ∈ E, H e ≤ s) (hc : C ⊆ D)
    (critical : ∀ p ∈ D, IsMaxOn L D p → p ∈ C)
    (strictGap : f < s → ∃ p ∈ D, IsMaxOn L D p) :
    ((∃ p ∈ D, IsMaxOn L D p) ↔ ∃ p ∈ C, f ≤ L p) ∧
    {p ∈ D | IsMaxOn L D p} = {p ∈ C | L p = s} ∧
    {e ∈ E | s ≤ H e} = {e ∈ E | H e = s}
```

**`modified_tie`**. Positive Bernoulli scaling preserves ordering and ties of positive-component heights. [Zero6Faces.lean line 67](../../lean/UnivariateML/Family/Zero6Faces.lean#L67)

```lean
theorem modified_tie {z k : ℕ} {w a b : ℝ} (h : 0 < bernoulli z k w) :
    bernoulli z k w * a ≤ bernoulli z k w * b ↔ a ≤ b
```

**`inflated_zero_atom`**. The original parent zero atom has zero inflated likelihood whenever there is a positive observation. [Zero6Faces.lean line 73](../../lean/UnivariateML/Family/Zero6Faces.lean#L73)

```lean
theorem inflated_zero_atom {k : ℕ} (hk : 0 < k) {z : ℕ} {x : Fin k → ℕ}
    (hx : ∀ i, 0 < x i) (r : ℝ) :
    Zero6Inflated.likelihood z x (fun (_ : Unit) j ↦ if j = 0 then 1 else 0) ((),r) = 0
```


### UnivariateML.Zero6Families

The six family theorems share this conclusion.

```lean
def SampleClaim {P : Type*} {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    (D : Set P) (mass : P → ℕ → ℝ) (hold : Option ℝ) : Prop :=
  let E := modifiedSlice D mass z k hold
  let L := conditional x mass
  (∀ p ∈ E, ∀ w ∈ Zero6.weightDomain hold,
    (IsMaxOn (Zero6.modified z k L) (E ×ˢ Zero6.weightDomain hold) (p,w) ∧
      0 < Zero6.modified z k L (p,w)) ↔
    w = Zero6.chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (k ≠ 0 → IsMaxOn L E p)) ∧
  (∀ p ∈ positiveSlice D mass, ∀ rho ∈ Icc (0 : ℝ) 1,
    Zero6Inflated.likelihood z x mass (p,rho) =
      bernoulli z k (Zero6Inflated.effective (mass p 0) rho) * L p) ∧
  (k = 0 → ∀ p ∈ D, ∀ rho ∈ Icc (0 : ℝ) 1,
    Zero6Inflated.likelihood z x mass (p,rho) = 1 ↔ mass p 0 = 1 ∨ rho = 1) ∧
  (∀ {F : Filter P} {a : ℝ}, k ≠ 0 → Tendsto L F (𝓝 a) →
    Tendsto (fun p ↦ Zero6.modified z k L (p,Zero6.chosenWeight z k hold)) F
      (𝓝 (bernoulli z k (Zero6.chosenWeight z k hold) * a)))
```

**`bbinom_sample`**. Applies the joined sample transfer to the bbinom masses and exact parent hold domain. The transfer covers the positive factorization, all-zero height-one test and supplied modified path. [Zero6Families.lean line 146](../../lean/UnivariateML/Family/Zero6Families.lean#L146)

```lean
theorem bbinom_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (size : Option ℕ) (alpha beta : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ bbinomSlice size alpha beta, bbinomMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ bbinomSlice size alpha beta, ∀ i, 0 ≤ bbinomMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (bbinomSlice size alpha beta) bbinomMass,
        0 < conditional x bbinomMass p) :
    SampleClaim z x (bbinomSlice size alpha beta) bbinomMass hold
```

**`bnbinom_sample`**. Applies the joined sample transfer to the bnbinom masses and exact parent hold domain. The transfer covers the positive factorization, all-zero height-one test and supplied modified path. [Zero6Families.lean line 158](../../lean/UnivariateML/Family/Zero6Families.lean#L158)

```lean
theorem bnbinom_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (size alpha beta : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ bnbinomSlice size alpha beta, bnbinomMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ bnbinomSlice size alpha beta, ∀ i, 0 ≤ bnbinomMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (bnbinomSlice size alpha beta) bnbinomMass,
        0 < conditional x bnbinomMass p) :
    SampleClaim z x (bnbinomSlice size alpha beta) bnbinomMass hold
```

**`cmp_sample`**. Applies the joined sample transfer to the cmp masses and exact parent hold domain. The transfer covers the positive factorization, all-zero height-one test and supplied modified path. [Zero6Families.lean line 170](../../lean/UnivariateML/Family/Zero6Families.lean#L170)

```lean
theorem cmp_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (rate dispersion : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ cmpSlice rate dispersion, cmpMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ cmpSlice rate dispersion, ∀ i, 0 ≤ cmpMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (cmpSlice rate dispersion) cmpMass,
        0 < conditional x cmpMass p) :
    SampleClaim z x (cmpSlice rate dispersion) cmpMass hold
```

**`dgamma_sample`**. Applies the joined sample transfer to the dgamma masses and exact parent hold domain. The transfer covers the positive factorization, all-zero height-one test and supplied modified path. [Zero6Families.lean line 182](../../lean/UnivariateML/Family/Zero6Families.lean#L182)

```lean
theorem dgamma_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (shape rate : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ dgammaSlice shape rate, dgammaMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ dgammaSlice shape rate, ∀ i, 0 ≤ dgammaMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (dgammaSlice shape rate) dgammaMass,
        0 < conditional x dgammaMass p) :
    SampleClaim z x (dgammaSlice shape rate) dgammaMass hold
```

**`dweibull_sample`**. Applies the joined sample transfer to the dweibull masses and exact parent hold domain. The transfer covers the positive factorization, all-zero height-one test and supplied modified path. [Zero6Families.lean line 194](../../lean/UnivariateML/Family/Zero6Families.lean#L194)

```lean
theorem dweibull_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (shape1 shape2 : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ dweibullSlice shape1 shape2, dweibullMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ dweibullSlice shape1 shape2, ∀ i, 0 ≤ dweibullMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (dweibullSlice shape1 shape2) dweibullMass,
        0 < conditional x dweibullMass p) :
    SampleClaim z x (dweibullSlice shape1 shape2) dweibullMass hold
```

**`pig_sample`**. Applies the joined sample transfer to the pig masses and exact parent hold domain. The transfer covers the positive factorization, all-zero height-one test and supplied modified path. [Zero6Families.lean line 206](../../lean/UnivariateML/Family/Zero6Families.lean#L206)

```lean
theorem pig_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (mean shape : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ pigSlice mean shape, pigMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ pigSlice mean shape, ∀ i, 0 ≤ pigMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (pigSlice mean shape) pigMass,
        0 < conditional x pigMass p) :
    SampleClaim z x (pigSlice mean shape) pigMass hold
```

**`dweibull_held_probability_ones`**. The modified all-one positive component reaches its strict excluded maximum at infinite shape, with the selected zero weight carried along. [Zero6Families.lean line 220](../../lean/UnivariateML/Family/Zero6Families.lean#L220)

```lean
theorem dweibull_held_probability_ones {z k : ℕ} (hk : 0 < k)
    {q w : ℝ} (hq : q ∈ Ioo 0 1) (hw : 0 < bernoulli z k w)
    (x : Fin k → ℕ) (hx : ∀ i, x i = 1) :
    let L := fun b ↦ Zero6.modified z k (conditional x dweibullMass) ((q,b),w)
    let H := bernoulli z k w
    (∀ b > 0, L b < H) ∧ Tendsto L atTop (𝓝 H) ∧ IsLUB (L '' Ioi 0) H
```

**`dweibull_held_probability_inflated_ones`**. The actual inflated zero/one likelihood has a strict excluded Bernoulli supremum at infinite shape for every compatible held weight. [Zero6Families.lean line 264](../../lean/UnivariateML/Family/Zero6Families.lean#L264)

```lean
theorem dweibull_held_probability_inflated_ones {z k : ℕ} (hk : 0 < k)
    {q rho : ℝ} (hq : q ∈ Ioo 0 1) (hr : rho ∈ Ico 0 1)
    (x : Fin k → ℕ) (hx : ∀ i, x i = 1) :
    let L := fun b ↦ Zero6Inflated.likelihood z x dweibullMass ((q,b),rho)
    let H := bernoulli z k (Zero6Inflated.effective (1-q) rho)
    (∀ b > 0, L b < H) ∧ Tendsto L atTop (𝓝 H) ∧ IsLUB (L '' Ioi 0) H
```


### UnivariateML.Zero6Inflated

**`face_limit`**. The original inflated likelihood transports pointwise parent limits and any converging inflation weight. [Zero6Inflated.lean line 35](../../lean/UnivariateML/Family/Zero6Inflated.lean#L35)

```lean
theorem face_limit {P : Type*} {F : Filter P} {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    {mass : P → ℕ → ℝ} {limit : ℕ → ℝ} {rho : P → ℝ} {r : ℝ}
    (h : ∀ j, Tendsto (fun p ↦ mass p j) F (𝓝 (limit j)))
    (hr : Tendsto rho F (𝓝 r)) :
    Tendsto (fun p ↦ likelihood z x mass (p,rho p)) F
      (𝓝 (likelihood z x (fun _ : Unit ↦ limit) ((),r)))
```

**`free_profile`**. A free inflated winner realizes the clipped weight and dominates values on both feasible profile branches. [Zero6Inflated.lean line 48](../../lean/UnivariateML/Family/Zero6Inflated.lean#L48)

```lean
theorem free_profile {P : Type*} {D : Set P} {z k : ℕ} (hn : 0 < z+k)
    {u L : P → ℝ} (hu : ∀ q ∈ D, u q ∈ Ico 0 1)
    (hL : ∀ q ∈ D, 0 < L q)
    {p : P} {r : ℝ} (hp : p ∈ D) (hr : r ∈ Icc 0 1) :
    let v := zeroFraction z k
    let F := fun t : P × ℝ ↦ bernoulli z k (effective (u t.1) t.2) * L t.1
    IsMaxOn F (D ×ˢ Icc 0 1) (p,r) ↔
    effective (u p) r = max (u p) v ∧
      (∀ q ∈ D, u q ≤ v → bernoulli z k v * L q ≤ F (p,r)) ∧
      (∀ q ∈ D, v ≤ u q → bernoulli z k (u q) * L q ≤ F (p,r))
```

**`held_score`**. The scalar chain rule gives the conditional log-score plus the held inflation adjustment. [Zero6Inflated.lean line 107](../../lean/UnivariateML/Family/Zero6Inflated.lean#L107)

```lean
theorem held_score {T u B : ℝ → ℝ} {p t' u' b' rho : ℝ}
    (hT : HasDerivAt T t' p) (hu : HasDerivAt u u' p)
    (hB : HasDerivAt B b' (effective (u p) rho)) :
    HasDerivAt (fun q ↦ T q + B (effective (u q) rho))
      (t' + b' * ((1-rho)*u')) p
```

**`seam_scores`**. At a seam where the Bernoulli derivative vanishes, the two scalar branch derivatives agree. [Zero6Inflated.lean line 116](../../lean/UnivariateML/Family/Zero6Inflated.lean#L116)

```lean
theorem seam_scores {T u B : ℝ → ℝ} {p t' u' v : ℝ} (he : u p = v)
    (hT : HasDerivAt T t' p) (hu : HasDerivAt u u' p)
    (hB : HasDerivAt B 0 v) :
    HasDerivAt (fun q ↦ T q + B v) t' p ∧
      HasDerivAt (fun q ↦ T q + B (u q)) t' p
```

**`pois_held_zeros`**. At held inflation below one the all-zero Poisson height tends to one only at an excluded boundary along the stated path, and every positive rate loses to a smaller one. [Zero6Inflated.lean line 130](../../lean/UnivariateML/Family/Zero6Inflated.lean#L130)

```lean
theorem pois_held_zeros {z : ℕ} (hz : 0 < z) {rho : ℝ} (hr : rho ∈ Icc 0 1) :
    let F := fun r : ℝ ↦ inflatedPoisson z 0 0 (r,rho)
    (∀ r ∈ Ioi 0, IsMaxOn F (Ioi 0) r ↔ rho = 1) ∧
    (rho < 1 → (∀ r ∈ Ioi 0, F r < 1) ∧ Tendsto F (𝓝[>] 0) (𝓝 1))
```

**`pois_inflated`**. A compatible Poisson positive sample attains below inflation one. On all zeros an allowable inflation weight of one is necessary and sufficient. [Zero6Inflated.lean line 170](../../lean/UnivariateML/Family/Zero6Inflated.lean#L170)

```lean
theorem pois_inflated {z k S : ℕ} (hn : 0 < z+k)
    (hS : S = 0 ↔ k = 0) {W : Set ℝ} (hW : IsCompact W)
    (hvalid : W ⊆ Icc 0 1) :
    let D := Ioi (0 : ℝ) ×ˢ W
    let F := inflatedPoisson z k S
    (∃ p ∈ D, IsMaxOn F D p ∧ 0 < F p) ↔
      (k = 0 ∧ (1 : ℝ) ∈ W) ∨ (0 < k ∧ ∃ r ∈ W, r < 1)
```

**`binom_inflated`**. The fixed-size closed interval yields attainment, with the all-zero union and the free size-one product curve characterized. [Zero6Inflated.lean line 222](../../lean/UnivariateML/Family/Zero6Inflated.lean#L222)

```lean
theorem binom_inflated {k : ℕ} (N z : ℕ) (x : Fin k → ℕ)
    (hn : 0 < z+k) {rho : ℝ} (hr : rho ∈ Icc 0 1) :
    (∃ p ∈ Icc (0 : ℝ) 1, IsMaxOn (fun p ↦ inflatedBinom N z x (p,rho)) (Icc 0 1) p) ∧
    (∃ t ∈ (Icc (0 : ℝ) 1 ×ˢ Icc (0 : ℝ) 1),
      IsMaxOn (inflatedBinom N z x) (Icc 0 1 ×ˢ Icc 0 1) t) ∧
    (k = 0 → ∀ p ∈ Icc (0 : ℝ) 1,
      inflatedBinom N z x (p,rho) = 1 ↔ N = 0 ∨ p = 0 ∨ rho = 1) ∧
    ((∀ i, x i = 1) → ∀ p ∈ Icc (0 : ℝ) 1,
      IsMaxOn (inflatedBinom 1 z x) (Icc 0 1 ×ˢ Icc 0 1) (p,rho) ↔
        (1-rho)*p = 1 - zeroFraction z k)
```

**`nbinom_inflated_positive`**. At positive held size a positive witness below inflation one gives a fit in the interior continuation chart. [Zero6Inflated.lean line 252](../../lean/UnivariateML/Family/Zero6Inflated.lean#L252)

```lean
theorem nbinom_inflated_positive {r : ℝ} (hr : 0 < r) {z k S : ℕ}
    (hk : 0 < k) (hS : 0 < S) {W : Set ℝ} (hW : IsCompact W)
    (hvalid : W ⊆ Icc 0 1) {rho : ℝ} (hi : rho ∈ W) (hi' : rho < 1) :
    let D := Ico (0 : ℝ) 1 ×ˢ W
    let F := inflatedNegBinom r z k S
    ∃ p ∈ D, p.1 ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn F D p ∧ 0 < F p
```

**`nbinom_inflated_zeros`**. The all-zero height is one precisely at an included zero atom or inflation one. [Zero6Inflated.lean line 280](../../lean/UnivariateML/Family/Zero6Inflated.lean#L280)

```lean
theorem nbinom_inflated_zeros {r : ℝ} (hr : 0 ≤ r) {z : ℕ} (hz : 0 < z)
    {q rho : ℝ} (hq : q ∈ Ico 0 1) (hi : rho ∈ Icc 0 1) :
    inflatedNegBinom r z 0 0 (q,rho) = 1 ↔ r = 0 ∨ q = 0 ∨ rho = 1
```

**`geom_inflated`**. The effective-zero chart includes the parent atom and has a unique positive fit. Its all-zero winners are exactly effective zero weight one. [Zero6Inflated.lean line 302](../../lean/UnivariateML/Family/Zero6Inflated.lean#L302)

```lean
theorem geom_inflated {z k d : ℕ} (hn : 0 < z+k) (hd : k = 0 → d = 0) :
    let D : Set (ℝ × ℝ) := {t | t.1 ∈ Ico 0 1 ∧ t.2 ∈ Icc (1-t.1) 1}
    let F := inflatedGeom z k d
    (k = 0 → ∀ p ∈ D, IsMaxOn F D p ↔ p.2 = 1) ∧
    (0 < k → ∃! p, p ∈ D ∧ IsMaxOn F D p)
```

**`geom_held`**. The held-weight geometric slice gives a positive fit below inflation one and an exact zero-only maximizing set. [Zero6Inflated.lean line 374](../../lean/UnivariateML/Family/Zero6Inflated.lean#L374)

```lean
theorem geom_held {z k d : ℕ} (hn : 0 < z+k) (hd : k = 0 → d = 0)
    {rho : ℝ} (hr : rho ∈ Icc 0 1) :
    let D := Ico (0 : ℝ) 1
    let F := fun q ↦ inflatedGeom z k d (q,effective (1-q) rho)
    (k = 0 → ∀ q ∈ D, IsMaxOn F D q ↔ q = 0 ∨ rho = 1) ∧
    (0 < k → ((∃ q ∈ D, IsMaxOn F D q ∧ 0 < F q) ↔ rho < 1))
```


### UnivariateML.Zero6Scores

**`geom_held_positive_score`**. The strictly decreasing held score has exactly one interior zero, and maximizing is equivalent to solving it. [Zero6Scores.lean line 61](../../lean/UnivariateML/Family/Zero6Scores.lean#L61)

```lean
theorem geom_held_positive_score {z k d : ℕ} (hk : 0 < k)
    {rho : ℝ} (hr : rho ∈ Ico 0 1) :
    let F := fun q ↦ inflatedGeom z k d (q,effective (1-q) rho)
    (∃! q, q ∈ Ioo 0 1 ∧ score z k d rho q = 0) ∧
      (∀ q ∈ Ico 0 1, IsMaxOn F (Ico 0 1) q ↔
        q ∈ Ioo 0 1 ∧ score z k d rho q = 0)
```

**`pois_held_positive_score`**. Among all held-weight positive-rate score roots, exactly those at the largest likelihood fit. [Zero6Scores.lean line 168](../../lean/UnivariateML/Family/Zero6Scores.lean#L168)

```lean
theorem pois_held_positive_score {z k S : ℕ} (hk : 0 < k) (hS : 0 < S)
    {rho : ℝ} (hr : rho ∈ Ico 0 1) :
    let F := fun r ↦ inflatedPoisson z k S (r,rho)
    (∃ r ∈ Ioi 0, poissonHeldScore z k S rho r = 0) ∧
    (∀ r ∈ Ioi 0, IsMaxOn F (Ioi 0) r ↔
      poissonHeldScore z k S rho r = 0 ∧
      ∀ q ∈ Ioi 0, poissonHeldScore z k S rho q = 0 → F q ≤ F r)
```


### UnivariateML.Zero6ZeroAtoms

**`bnbinom_held_inflation_zero`**. At any compatible hold slice with a free parent coordinate and held inflation below one, the all-zero height one is excluded. All available paths have the same height. [Zero6ZeroAtoms.lean line 34](../../lean/UnivariateML/Family/Zero6ZeroAtoms.lean#L34)

```lean
theorem bnbinom_held_inflation_zero {z : ℕ} (hz : 0 < z) {rho : ℝ} (hrho : rho ∈ Ico 0 1)
    (size alpha beta : Option ℝ) (hfree : size = none ∨ alpha = none ∨ beta = none)
    (hD : (bnbinomSlice size alpha beta).Nonempty) :
    let D := bnbinomSlice size alpha beta
    let L := fun p ↦ likelihood z (fun i : Fin 0 ↦ i.elim0) bnbinomMass (p,rho)
    (∀ p ∈ D, L p < 1) ∧ IsLUB (L '' D) 1 ∧
    ∀ p ∈ D,
      (size = none → Tendsto (fun t ↦ L (t,p.2.1,p.2.2)) (𝓝[>] 0) (𝓝 1)) ∧
      (alpha = none → Tendsto (fun t ↦ L (p.1,t,p.2.2)) atTop (𝓝 1)) ∧
      (beta = none → Tendsto (fun t ↦ L (p.1,p.2.1,t)) (𝓝[>] 0) (𝓝 1))
```

**`dgamma_held_inflation_zero`**. The all-zero held-inflation likelihood lies strictly below one. Shrinking a free shape or raising a free rate reaches one. [Zero6ZeroAtoms.lean line 99](../../lean/UnivariateML/Family/Zero6ZeroAtoms.lean#L99)

```lean
theorem dgamma_held_inflation_zero {z : ℕ} (hz : 0 < z) {rho : ℝ} (hrho : rho ∈ Ico 0 1)
    {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    let L := fun p ↦ likelihood z (fun i : Fin 0 ↦ i.elim0) dgammaMass (p,rho)
    L (a,r) < 1 ∧
    Tendsto (fun b ↦ L (b,r)) (𝓝[>] 0) (𝓝 1) ∧
    Tendsto (fun s ↦ L (a,s)) atTop (𝓝 1)
```
