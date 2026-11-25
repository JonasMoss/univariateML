# Exact likelihood transfers in Lean

TASK-318 · 2026-11-25

Lean proves the transfer of the full maximizer set, existence, uniqueness, boundedness, and finite supremum values under a parameter bijection and an additive data term. The family results cover inverse gamma, inverse Pareto, Pareto II with a held threshold and positive excesses, and Burr with held positive power. [Lean module](../../lean/UnivariateML/Family/Transfers.lean)

## Dossier findings

- **Pareto II threshold ties.** The Details section says fitting requires observations strictly above the held threshold. The dossier's endpoint rule and the fitter allow observations equal to the threshold and handle those samples separately. The transferred existence and uniqueness results use strictly positive excesses, as required by the source Lomax theorems. The pointwise translation identity itself also holds at zero excess. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md#details) · [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/mle.hpp#L20)
- **Exact attainment includes equality with the face.** For inverse Pareto, positive-excess Pareto II, and held-power Burr, the witness test uses `psi ≤ 0`. A finite profile point can tie the optimized face and still yield a mathematical MLE. The inverse Pareto and Pareto II dossiers already note that their numerical face preference can reject a finite fit at equality or with small positive gain. The implementation's comparison margin and Burr's practical certificate are outside the exact theorem. [Inverse Pareto](../../../univariateML-cpp/include/cppunivariate/distributions/invpareto/README.md#r-notes) · [Pareto II](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md#r-notes) · [Burr](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md#details)
- **Joint uniqueness wording.** The dossiers leave joint uniqueness unestablished. The Lomax uniqueness report gives an exact real-sample tie, and its proof transfers to both free residual coefficients on the three routes. Starting at the Lomax sample $y$, take $1/y$, $\mu+y$, or $y^{1/c}$ respectively. Thus a universal uniqueness claim on any of those routes is false. The transferred hold results concern a single free coefficient. The exact tie and the two-support uniqueness result remain separate Lean formalization targets. [Lomax uniqueness report](2026-11-25-lomax-uniq-math-report.md#transfer-and-lean)
- **Inverse gamma.** The exact attainment, constant-sample divergence, and hold statements agree with the dossier. The pair $(a,b)$ uses the public inverse-gamma scale $b$, which is the Gamma source rate. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invgamma/README.md#details)

## Coordinates and scope

| Route | Source data | Source pair for target shape and scale | Additive term |
| --- | --- | --- | --- |
| Inverse gamma | $1/x$ | $(a,b)$ | $-2\sum_i\log x_i/n$ |
| Inverse Pareto | $1/x$ | $(k,s)$ | $-2\sum_i\log x_i$ |
| Pareto II | $x-\mu$ | $(k,1/s)$ | $0$ |
| Burr | $x^c$ | $(k,1/s^c)$ | $\sum_i[\log c+(c-1)\log x_i]$ |

The inverse-gamma objective is per observation to match `Gamma.logLik`. For a nonempty sample the total log likelihood is $n$ times this objective, so it has the same maximizing coefficients and diverges on the same constant-data path. The Lomax transfers use totals. For Burr, the exact proof uses raw powers. With powers normalized by $M=\exp(c\max_i\log x_i)$, the log-Jacobian acquires the additional constant $-n\log M$ used in the dossier. [Transfer principle](2026-11-25-transfer-math-report.md#1-transfers) · [Map and Jacobian table](2026-11-25-lomax-uniq-math-report.md#transfer-and-lean)

The real-valued `sSup` formula assumes a nonempty bounded source image. `bddAbove_iff` handles unbounded likelihoods, and `isLUB_iff` transports any finite least upper bound. The family statements concern exact real arithmetic. Additional cases outside this job include Pareto II with zero excesses or a free threshold, Burr with free power, and floating-point error in transformed inputs or restored coefficients.

## Proof effort

| Target | Relative effort | Proof work |
| --- | --- | --- |
| Generic transfer | Moderate | Restrict the parameter map to the maximizing set and represent the likelihood image by translation. Use the order isomorphism for least upper bounds and reuse `Generic.limit_le_upperBound` for feasible limit paths. |
| Inverse gamma | Moderate | Identify the mean log-Jacobian, preserve sample spread under reciprocation, and transfer the Gamma maximizers on both free and both singly held slices. Translate divergence along the fixed-mean path. |
| Inverse Pareto | Short after sharing the Lomax proofs | The parameter map is the identity in shape and public scale. Transport the finite witness test, both holds, and the face and profiled gap. |
| Pareto II | Moderate | Translate the data on the held threshold, invert the positive scale on the joint and free-scale slices, and transport the zero-Jacobian face and gap. |
| Held-power Burr | Largest among the family routes | Match the dossier density by positive real-power identities and prove the scale-to-rate map and its inverse on the positive half-line. Transport the face path, conditional maxima, and constant-data gap in public scale. |

## Verification

- Workstation, one reserved CPU slot. `lake env lean UnivariateML/Family/Transfers.lean` passed without diagnostics.
- Workstation, one reserved CPU slot. `lake build +UnivariateML.Family.Transfers UnivariateML` completed successfully with **7296 jobs**. The build replayed existing diagnostics in Weibull, GPD, FoldedNormal, and Frechet. The new module built without diagnostics. The explicit target covers the module before its root import is added.
- All four targets are proved. The source contains no proof placeholders.

## Lean statements

In the generic statements, `D` is the target domain, `E` the source domain, `φ` the target-to-source map, `f` and `g` the two objectives, and `c` the additive data term.

### Generic transfer

Namespace `UnivariateML.Transfer`.

**`isMaxOn_iff`**. A feasible point maximizes the target exactly when its image maximizes the source.

```lean
theorem isMaxOn_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c)
    {a : α} (ha : a ∈ D) : IsMaxOn f D a ↔ IsMaxOn g E (φ a)
```

**`bijOn_maximizers`**. The same parameter map bijects the full feasible maximizer sets.

```lean
theorem bijOn_maximizers (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    BijOn φ {a ∈ D | IsMaxOn f D a} {b ∈ E | IsMaxOn g E b}
```

**`exists_max_iff`**. The two feasible maximization problems attain a maximum together.

```lean
theorem exists_max_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    (∃ a ∈ D, IsMaxOn f D a) ↔ (∃ b ∈ E, IsMaxOn g E b)
```

**`existsUnique_max_iff`**. A unique feasible maximizer exists on either side exactly when one exists on the other.

```lean
theorem existsUnique_max_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    (∃! a, a ∈ D ∧ IsMaxOn f D a) ↔ (∃! b, b ∈ E ∧ IsMaxOn g E b)
```

**`image_eq`**. The likelihood values on the target domain form the shifted source value set.

```lean
theorem image_eq (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    f '' D = (fun v ↦ v + c) '' (g '' E)
```

**`isLUB_iff`**. A finite least upper bound is shifted by the additive term.

```lean
theorem isLUB_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) (u : ℝ) :
    IsLUB (f '' D) (u + c) ↔ IsLUB (g '' E) u
```

**`sSup_eq`**. For a nonempty bounded likelihood image, the real supremum is shifted by the additive term.

```lean
theorem sSup_eq (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c)
    (hE : E.Nonempty) (hB : BddAbove (g '' E)) :
    sSup (f '' D) = sSup (g '' E) + c
```

**`attained_iff`**. A value is achieved by a feasible source point exactly when its shifted value is achieved on the target domain.

```lean
theorem attained_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) (u : ℝ) :
    u + c ∈ f '' D ↔ u ∈ g '' E
```

**`bddAbove_iff`**. Boundedness above and unboundedness above are preserved.

```lean
theorem bddAbove_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c) :
    BddAbove (f '' D) ↔ BddAbove (g '' E)
```

**`unattained_sup_iff`**. For a nonempty bounded image, an unattained source supremum gives an unattained target supremum, and conversely.

```lean
theorem unattained_sup_iff (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c)
    (hE : E.Nonempty) (hB : BddAbove (g '' E)) :
    sSup (f '' D) ∉ f '' D ↔ sSup (g '' E) ∉ g '' E
```

**`limit_le_upperBound`**. A feasible source path with finite limiting likelihood supplies a lower bound on every target upper bound after adding the shift.

```lean
theorem limit_le_upperBound {ι : Type*} {F : Filter ι} [F.NeBot] {p : ι → β} {u b : ℝ}
    (hφ : BijOn φ D E) (h : ∀ a ∈ D, f a = g (φ a) + c)
    (hp : ∀ᶠ i in F, p i ∈ E) (hu : Tendsto (fun i ↦ g (p i)) F (𝓝 u))
    (hb : b ∈ upperBounds (f '' D)) : u + c ≤ b
```

### Inverse gamma

Namespace `UnivariateML.Transfer.InvGamma`.

**`logLik_transfer`**. The objective equals the Gamma mean log likelihood on reciprocal observations plus the mean log-Jacobian.

```lean
theorem logLik_transfer (hn : 0 < n) (x : Fin n → ℝ) (a b : ℝ) :
    logLik x a b = Gamma.logLik (reciprocal x) a b + reciprocalJac x / n
```

**`jointMLE`**. A nonempty positive sample with two distinct observations has exactly one finite maximizing shape and scale.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ Gamma.domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) Gamma.domain p
```

**`heldScale_unique`**. A positive held scale leaves exactly one positive maximizing shape, including on constant positive samples.

```lean
theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun k ↦ logLik x k b) (Ioi 0) a
```

**`heldShape_unique`**. A positive held shape leaves exactly one positive maximizing scale, including on constant positive samples.

```lean
theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x a) (Ioi 0) b
```

**`fullyHeld_unique`**. The positive pair supplied by the two holds is the sole feasible maximizer on the singleton slice.

```lean
theorem fullyHeld_unique (x : Fin n → ℝ) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∃! p : ℝ × ℝ, p ∈ Gamma.domain ∩ {(a, b)} ∧
      IsMaxOn (fun q ↦ logLik x q.1 q.2) (Gamma.domain ∩ {(a, b)}) p
```

**`constant_noMLE`**. A constant positive sample has no finite joint MLE.

```lean
theorem constant_noMLE (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    ¬ ∃ p ∈ Gamma.domain,
      IsMaxOn (fun q ↦ logLik (fun _ : Fin n ↦ t) q.1 q.2) Gamma.domain p
```

**`constant_unbounded`**. For a constant positive sample at $t$, the mean log likelihood diverges with shape $a$ along scale $a/(t^{-1})=at$.

```lean
theorem constant_unbounded (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    Tendsto (fun a ↦ logLik (fun _ : Fin n ↦ t) a (a / t⁻¹)) atTop atTop
```

### Inverse Pareto

Namespace `UnivariateML.Transfer.InvPareto`.

**`logLik_transfer`**. The public-coordinate log likelihood is the Lomax objective on reciprocal observations with the coefficient map and additive term in the table.

```lean
theorem logLik_transfer (x : Fin n → ℝ) (k s : ℝ) :
    logLik x k s = Lomax.logLik (reciprocal x) k s + reciprocalJac x
```

**`exists_max_iff`**. A finite maximum exists exactly when the Lomax witness test on reciprocal observations finds a nonpositive value strictly inside its search interval.

```lean
theorem exists_max_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃ p ∈ Lomax.domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Lomax.domain p) ↔
      ∃ u ∈ Ioo 0 (Lomax.uBar (reciprocal x)), Lomax.psi (reciprocal x) u ≤ 0
```

**`exponential_sup_of_no_witness`**. In the absence of a witness, the optimized inverse exponential value is the supremum and strictly exceeds every finite fit.

```lean
theorem exponential_sup_of_no_witness (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (reciprocal x)), Lomax.psi (reciprocal x) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' Lomax.domain) = bestInvExp x ∧
      ∀ p ∈ Lomax.domain, logLik x p.1 p.2 < bestInvExp x
```

**`heldScale_unique`**. Holding a positive scale leaves exactly one maximizing shape. This includes positive constant transformed samples.

```lean
theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) k
```

**`heldShape_unique`**. Holding a positive shape leaves exactly one maximizing scale. This includes positive constant transformed samples.

```lean
theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k) (Ioi 0) s
```

**`constant_face`**. A constant sample in the stated support has the optimized inverse exponential value as its finite, unattained supremum.

```lean
theorem constant_face (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    let x : Fin n → ℝ := fun _ ↦ t
    sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' Lomax.domain) = bestInvExp x ∧
      ∀ p ∈ Lomax.domain, logLik x p.1 p.2 < bestInvExp x
```

**`constant_profile_loss`**. At every finite positive scale, the constant-sample shape profile loses the displayed strictly positive amount per observation to the face.

```lean
theorem constant_profile_loss (hn : 0 < n) {t s : ℝ} (ht : 0 < t) (hs : 0 < s) :
    let x : Fin n → ℝ := fun _ ↦ t
    let z := s * t⁻¹
    (bestInvExp x - logLik x (Lomax.shape (reciprocal x) s) s) / n =
        log ((1 + z) * log (1 + z) / z) ∧ 0 < log ((1 + z) * log (1 + z) / z)
```

**`face_path`**. As shape $k$ tends to infinity along scale $r/k$, the log likelihood tends to the inverse exponential log likelihood indexed by transformed exponential rate $r$.

```lean
theorem face_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {r : ℝ} (hr : 0 < r) :
    Tendsto (fun k : ℝ ↦ logLik x k (r / k)) atTop
      (𝓝 (Lomax.expLogLik (reciprocal x) r + reciprocalJac x))
```

### Pareto II with held threshold

Namespace `UnivariateML.Transfer.ParetoII`.

**`logLik_transfer`**. The public-coordinate log likelihood is the Lomax objective on strictly positive excesses above the held threshold with the coefficient map and additive term in the table.

```lean
theorem logLik_transfer (x : Fin n → ℝ) (μ k s : ℝ) :
    logLik x μ k s = Lomax.logLik (excess x μ) k s⁻¹
```

**`exists_max_iff`**. A finite maximum exists exactly when the Lomax witness test on strictly positive excesses above the held threshold finds a nonpositive value strictly inside its search interval.

```lean
theorem exists_max_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i) :
    (∃ p ∈ Lomax.domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik x μ q.1 q.2) Lomax.domain p) ↔
      ∃ u ∈ Ioo 0 (Lomax.uBar (excess x μ)), Lomax.psi (excess x μ) u ≤ 0
```

**`exponential_sup_of_no_witness`**. In the absence of a witness, the optimized shifted exponential value is the supremum and strictly exceeds every finite fit.

```lean
theorem exponential_sup_of_no_witness (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (excess x μ)), Lomax.psi (excess x μ) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x μ p.1 p.2) '' Lomax.domain) = bestShiftedExp x μ ∧
      ∀ p ∈ Lomax.domain, logLik x μ p.1 p.2 < bestShiftedExp x μ
```

**`heldScale_unique`**. Holding a positive scale leaves exactly one maximizing shape. This includes positive constant transformed samples.

```lean
theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {s : ℝ} (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun a ↦ logLik x μ a s) (Ioi 0) k
```

**`heldShape_unique`**. Holding a positive shape leaves exactly one maximizing scale. This includes positive constant transformed samples.

```lean
theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {k : ℝ} (hk : 0 < k) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x μ k) (Ioi 0) s
```

**`constant_face`**. A constant sample in the stated support has the optimized shifted exponential value as its finite, unattained supremum.

```lean
theorem constant_face (hn : 0 < n) {μ t : ℝ} (ht : μ < t) :
    let x : Fin n → ℝ := fun _ ↦ t
    sSup ((fun p : ℝ × ℝ ↦ logLik x μ p.1 p.2) '' Lomax.domain) = bestShiftedExp x μ ∧
      ∀ p ∈ Lomax.domain, logLik x μ p.1 p.2 < bestShiftedExp x μ
```

**`constant_profile_loss`**. At every finite positive scale, the constant-sample shape profile loses the displayed strictly positive amount per observation to the face.

```lean
theorem constant_profile_loss (hn : 0 < n) {μ t s : ℝ} (ht : μ < t) (hs : 0 < s) :
    let x : Fin n → ℝ := fun _ ↦ t
    let z := s⁻¹ * (t - μ)
    (bestShiftedExp x μ - logLik x μ (Lomax.shape (excess x μ) s⁻¹) s) / n =
        log ((1 + z) * log (1 + z) / z) ∧ 0 < log ((1 + z) * log (1 + z) / z)
```

**`face_path`**. As shape $k$ tends to infinity along scale $k/r$, the log likelihood tends to the shifted exponential log likelihood indexed by transformed exponential rate $r$.

```lean
theorem face_path {x : Fin n → ℝ} {μ r : ℝ} (hx : ∀ i, μ < x i) (hr : 0 < r) :
    Tendsto (fun k : ℝ ↦ logLik x μ k (k / r)) atTop
      (𝓝 (Lomax.expLogLik (excess x μ) r))
```

### Burr with held positive power

Namespace `UnivariateML.Transfer.Burr`.

**`logLik_transfer`**. The public-coordinate log likelihood is the Lomax objective on powered observations with the coefficient map and additive term in the table.

```lean
theorem logLik_transfer {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (c k : ℝ)
    {s : ℝ} (hs : 0 < s) :
    logLik x c k s = Lomax.logLik (powered x c) k (rate c s) + powerJac x c
```

**`exists_max_iff`**. A finite maximum exists exactly when the Lomax witness test on powered observations finds a nonpositive value strictly inside its search interval.

```lean
theorem exists_max_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c) :
    (∃ p ∈ Lomax.domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik x c q.1 q.2) Lomax.domain p) ↔
      ∃ u ∈ Ioo 0 (Lomax.uBar (powered x c)), Lomax.psi (powered x c) u ≤ 0
```

**`exponential_sup_of_no_witness`**. In the absence of a witness, the optimized Weibull at the held power value is the supremum and strictly exceeds every finite fit.

```lean
theorem exponential_sup_of_no_witness (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (powered x c)), Lomax.psi (powered x c) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x c p.1 p.2) '' Lomax.domain) = bestWeibull x c ∧
      ∀ p ∈ Lomax.domain, logLik x c p.1 p.2 < bestWeibull x c
```

**`heldScale_unique`**. Holding a positive scale leaves exactly one maximizing shape. This includes positive constant transformed samples.

```lean
theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun a ↦ logLik x c a s) (Ioi 0) k
```

**`heldShape_unique`**. Holding a positive shape leaves exactly one maximizing scale. This includes positive constant transformed samples.

```lean
theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c k : ℝ} (hc : 0 < c) (hk : 0 < k) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x c k) (Ioi 0) s
```

**`constant_face`**. A constant sample in the stated support has the optimized Weibull at the held power value as its finite, unattained supremum.

```lean
theorem constant_face (hn : 0 < n) {t c : ℝ} (ht : 0 < t) (hc : 0 < c) :
    let x : Fin n → ℝ := fun _ ↦ t
    sSup ((fun p : ℝ × ℝ ↦ logLik x c p.1 p.2) '' Lomax.domain) = bestWeibull x c ∧
      ∀ p ∈ Lomax.domain, logLik x c p.1 p.2 < bestWeibull x c
```

**`constant_profile_loss`**. At every finite positive scale, the constant-sample shape profile loses the displayed strictly positive amount per observation to the face.

```lean
theorem constant_profile_loss (hn : 0 < n) {t c s : ℝ}
    (ht : 0 < t) (_hc : 0 < c) (hs : 0 < s) :
    let x : Fin n → ℝ := fun _ ↦ t
    let z := rate c s * t ^ c
    (bestWeibull x c - logLik x c (Lomax.shape (powered x c) (rate c s)) s) / n =
        log ((1 + z) * log (1 + z) / z) ∧ 0 < log ((1 + z) * log (1 + z) / z)
```

**`face_path`**. As shape $k$ tends to infinity along scale $(k/r)^{1/c}$, the log likelihood tends to the Weibull log likelihood at held power $c$ and scale $r^{-1/c}$.

```lean
theorem face_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c r : ℝ} (hc : 0 < c) (hr : 0 < r) :
    Tendsto (fun k : ℝ ↦ logLik x c k ((k / r) ^ c⁻¹)) atTop
      (𝓝 (Lomax.expLogLik (powered x c) r + powerJac x c))
```
