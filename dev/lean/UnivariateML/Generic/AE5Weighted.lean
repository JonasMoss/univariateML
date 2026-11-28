module

public import UnivariateML.Generic.AE3WeightedChart

set_option linter.style.header false

/-! Fixed-count uniqueness from finite-level identification or analytic incidence.
An open support chart is used for finite candidates. The Lebesgue measure is
on the free levels of the chart, with the multiplicities held fixed. -/

@[expose] public section

open Set MeasureTheory ContinuousLinearMap

namespace UnivariateML.AE5Weighted

variable {n : ℕ} {P : Type*}

/-- Spatial derivative of the scalar log likelihood. -/
noncomputable def score [NormedAddCommGroup P] [NormedSpace ℝ P]
    (f : P × ℝ → ℝ) (p : P) (t : ℝ) : ℝ :=
  fderiv ℝ f (p, t) (0, 1)

private theorem hasScore [NormedAddCommGroup P] [NormedSpace ℝ P]
    {f : P × ℝ → ℝ} {p : P} {t : ℝ} (h : AnalyticAt ℝ f (p, t)) :
    HasDerivAt (fun s ↦ f (p, s)) (score f p t) t :=
  h.differentiableAt.hasFDerivAt.comp_hasDerivAt t
    ((hasDerivAt_const t p).prodMk (hasDerivAt_id t))

/-- The tied samples form a Lebesgue-null set in the coordinates of
each fixed-count stratum. Pointwise identification at distinct levels
supplies separation without an almost-everywhere premise. A continuous
parameter term can include the contribution of pinned observations. -/
theorem measure_ties_eq_zero [NormedAddCommGroup P] [NormedSpace ℝ P]
    [LocallyCompactSpace P] [SecondCountableTopology P]
    {U : Set (P × ℝ)} (hU : IsOpen U) {f d : P × ℝ → ℝ} {c : P → ℝ}
    (hf : ContinuousOn f U) (hd : AnalyticOnNhd ℝ d U)
    (hc : ContinuousOn (fun z : P × ℝ ↦ c z.1) U)
    (hfd : ∀ p t, (p, t) ∈ U → HasDerivAt (fun s ↦ f (p, s)) (d (p, t)) t)
    (k : ℕ) (hident : ∀ p q, ∀ s : Finset ℝ, k ≤ s.card →
      (∀ t ∈ s, (p, t) ∈ U ∧ (q, t) ∈ U ∧ d (p, t) = d (q, t)) → p = q)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    volume {x : Fin n → ℝ | k ≤ (Finset.univ.image x).card ∧
      ∃ p q, p ≠ q ∧
        UniqueAE.Winner (AE3WeightedChart.domain U)
          (AE3WeightedChart.likelihood f c w) x p ∧
        UniqueAE.Winner (AE3WeightedChart.domain U)
          (AE3WeightedChart.likelihood f c w) x q} = 0 := by
  have h := AE3WeightedChart.ae_winning_class hU hf hd.continuousOn hc
    hfd w hw id (fun x ↦ k ≤ (Finset.univ.image x).card) (by
      filter_upwards [] with x hx p q hp hq he
      apply hident p q (Finset.univ.image x) hx
      intro t ht
      obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp ht
      exact ⟨hp.1 i, hq.1 i, he i⟩)
  have hnull : ∀ᵐ x : Fin n → ℝ ∂volume, ¬ (k ≤ (Finset.univ.image x).card ∧
      ∃ p q, p ≠ q ∧
        UniqueAE.Winner (AE3WeightedChart.domain U)
          (AE3WeightedChart.likelihood f c w) x p ∧
        UniqueAE.Winner (AE3WeightedChart.domain U)
          (AE3WeightedChart.likelihood f c w) x q) := by
    filter_upwards [h] with x hx
    rintro ⟨hk, p, q, hne, hp, hq⟩
    exact hne (hx hk p q hp hq)
  simpa only [not_not] using ae_iff.mp hnull

private theorem analyticScore [NormedAddCommGroup P] [NormedSpace ℝ P]
    {U : Set (P × ℝ)} (hU : IsOpen U) {f : P × ℝ → ℝ} (hf : AnalyticOnNhd ℝ f U) :
    AnalyticOnNhd ℝ (fun z ↦ score f z.1 z.2) U := by
  intro z hz
  exact ((ContinuousLinearMap.apply ℝ ℝ ((0, 1) : P × ℝ)).analyticAt _).comp
    (hf.fderiv_of_isOpen hU z hz)

/-- The almost-everywhere form when the scalar log likelihood itself is analytic. -/
theorem ae_unique_of_distinct_scores [NormedAddCommGroup P] [NormedSpace ℝ P]
    [LocallyCompactSpace P] [SecondCountableTopology P]
    {U : Set (P × ℝ)} (hU : IsOpen U) {f : P × ℝ → ℝ}
    (hf : AnalyticOnNhd ℝ f U) (k : ℕ)
    (hident : ∀ p q, ∀ s : Finset ℝ, k ≤ s.card →
      (∀ t ∈ s, (p, t) ∈ U ∧ (q, t) ∈ U ∧ score f p t = score f q t) → p = q)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, k ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (AE3WeightedChart.domain U)
        (AE3WeightedChart.likelihood f (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (AE3WeightedChart.domain U)
        (AE3WeightedChart.likelihood f (fun _ ↦ 0) w) x q → p = q := by
  have h : ∀ᵐ x : Fin n → ℝ ∂volume, ¬ (k ≤ (Finset.univ.image x).card ∧
      ∃ p q, p ≠ q ∧
        UniqueAE.Winner (AE3WeightedChart.domain U)
          (AE3WeightedChart.likelihood f (fun _ ↦ 0) w) x p ∧
        UniqueAE.Winner (AE3WeightedChart.domain U)
          (AE3WeightedChart.likelihood f (fun _ ↦ 0) w) x q) := by
    apply ae_iff.mpr
    simpa only [not_not] using measure_ties_eq_zero (c := fun _ ↦ 0) hU hf.continuousOn
      (analyticScore hU hf) continuousOn_const (fun _ _ hz ↦ hasScore (hf _ hz)) k hident w hw
  filter_upwards [h] with x hx hk p q hp hq
  by_contra hne
  exact hx ⟨hk, p, q, hne, hp, hq⟩

/-- With germ identification, analytic incidence gives the explicit level
threshold (2m+1) in an (m)-dimensional parameter chart. Connectedness
is required on the common observation support of each parameter pair. -/
theorem ae_unique_of_identifiable_germs {m : ℕ} (hn : m + m < n)
    {U : Set ((Fin m → ℝ) × ℝ)} (hU : IsOpen U) {f : (Fin m → ℝ) × ℝ → ℝ}
    (hf : AnalyticOnNhd ℝ f U) {c : (Fin m → ℝ) → ℝ}
    (hc : ContinuousOn (fun z : (Fin m → ℝ) × ℝ ↦ c z.1) U)
    (hconn : ∀ p q, IsPreconnected {t | (p, t) ∈ U ∧ (q, t) ∈ U})
    (hident : ∀ p q, p ≠ q → (∃ t, (p, t) ∈ U ∧ (q, t) ∈ U) →
      ∃ t, (p, t) ∈ U ∧ (q, t) ∈ U ∧ score f p t ≠ score f q t)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p q, UniqueAE.Winner (AE3WeightedChart.domain U)
        (AE3WeightedChart.likelihood f c w) x p →
      UniqueAE.Winner (AE3WeightedChart.domain U)
        (AE3WeightedChart.likelihood f c w) x q → p = q := by
  exact AE3WeightedChart.ae_unique_of_analytic_score hn hU hf.continuousOn
    (analyticScore hU hf) hc (fun _ _ hz ↦ hasScore (hf _ hz))
    hconn hident w hw

/-- Add finitely many analytic heights on a sample chart. Each height is
assumed to differ from the attained finite envelope off a null set. At an
attaining sample, the finite candidate is unique and strictly beats every
competitor that lies below or at its height. -/
theorem ae_unique_with_competitors [NormedAddCommGroup P] [NormedSpace ℝ P]
    [LocallyCompactSpace P] [SecondCountableTopology P] {J : Type*} [Finite J]
    {U : Set (P × ℝ)} (hU : IsOpen U) {f : P × ℝ → ℝ}
    (hf : AnalyticOnNhd ℝ f U) (k : ℕ)
    (hident : ∀ p q, ∀ s : Finset ℝ, k ≤ s.card →
      (∀ t ∈ s, (p, t) ∈ U ∧ (q, t) ∈ U ∧ score f p t = score f q t) → p = q)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (V : Set (Fin n → ℝ)) (height : J → (Fin n → ℝ) → ℝ)
    (_ha : ∀ j, AnalyticOnNhd ℝ (height j) V)
    (hsep : ∀ j, ∀ᵐ x : Fin n → ℝ ∂volume, x ∈ V → ∀ p,
      UniqueAE.Winner (AE3WeightedChart.domain U)
        (AE3WeightedChart.likelihood f (fun _ ↦ 0) w) x p →
      height j x ≠ AE3WeightedChart.likelihood f (fun _ ↦ 0) w p x) :
    ∀ᵐ x : Fin n → ℝ ∂volume, x ∈ V → k ≤ (Finset.univ.image x).card →
      ∀ p, UniqueAE.Winner (AE3WeightedChart.domain U)
        (AE3WeightedChart.likelihood f (fun _ ↦ 0) w) x p →
      (∀ q, UniqueAE.Winner (AE3WeightedChart.domain U)
        (AE3WeightedChart.likelihood f (fun _ ↦ 0) w) x q → q = p) ∧
      ∀ j, height j x ≤ AE3WeightedChart.likelihood f (fun _ ↦ 0) w p x →
        height j x < AE3WeightedChart.likelihood f (fun _ ↦ 0) w p x := by
  filter_upwards [ae_unique_of_distinct_scores hU hf k hident w hw, ae_all_iff.mpr hsep]
    with x hu hs hx hk p hp
  exact ⟨fun q hq ↦ hu hk q p hq hp, fun j hj ↦ lt_of_le_of_ne hj (hs j hx p hp)⟩

end UnivariateML.AE5Weighted

end
