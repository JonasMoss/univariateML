module

public import UnivariateML.Family.PGWEnvelope
public import Mathlib.Analysis.Analytic.IsolatedZeros
public import Mathlib.Analysis.Calculus.ImplicitContDiff

/-!
# Inverse-exponent profiling for PGW

## Deviations

The physical maximizers are in bijection with maximizers of the scalar
profile. The PGW family applications supply the nonsingular nuisance
scores and the nonconstant analytic profiles.
-/

@[expose] public section

open Real Set Filter Topology
open scoped ContDiff

namespace UnivariateML.PGWProfile

noncomputable section

variable {n : ℕ}

def finiteObjective (x : Fin n → ℝ) (p : (ℝ × ℝ) × ℝ) : ℝ :=
  ShapeFacesPGW.logLik x p.1.1 p.1.2 p.2⁻¹

def finiteDomain : Set ((ℝ × ℝ) × ℝ) := Weibull.domain ×ˢ Ioi 0

def bestNuisance (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (h : ℝ) : ℝ × ℝ :=
  if hh : 0 < h then
    (ShapeFacesPGW.held_exponent_mle hn hx (inv_pos.mpr hh) hne).exists.choose
  else (1, 1)

def profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (h : ℝ) : ℝ :=
  finiteObjective x (bestNuisance hn hx hne h, h)

private theorem nuisance_spec (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {h : ℝ} (hh : 0 < h) :
    bestNuisance hn hx hne h ∈ Weibull.domain ∧
    IsMaxOn (fun q ↦ finiteObjective x (q, h)) Weibull.domain (bestNuisance hn hx hne h) ∧
    ∀ q ∈ Weibull.domain, IsMaxOn (fun p ↦ finiteObjective x (p, h)) Weibull.domain q →
      q = bestNuisance hn hx hne h := by
  unfold bestNuisance
  rw [dite_eq_left hh]
  let he := ShapeFacesPGW.held_exponent_mle hn hx (inv_pos.mpr hh) hne
  exact ⟨he.exists.choose_spec.1, he.exists.choose_spec.2,
    fun q hq hm ↦ he.unique ⟨hq, hm⟩ he.exists.choose_spec⟩

/-- Every finite global maximizer comes from exactly one maximizing
inverse exponent, with all equal-height finite winners kept. -/
theorem maximizer_bijection (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    let W := {h ∈ Ioi 0 | IsMaxOn (profile hn hx hne) (Ioi 0) h}
    let P := {p ∈ finiteDomain | IsMaxOn (finiteObjective x) finiteDomain p}
    BijOn (fun h ↦ (bestNuisance hn hx hne h, h)) W P ∧
      ∀ h ∈ W, deriv (profile hn hx hne) h = 0 := by
  intro W P
  have hf (h : ℝ) := nuisance_spec hn hx hne (h := h)
  have hmap : MapsTo (fun h ↦ (bestNuisance hn hx hne h, h)) W P := by
    intro h hh
    refine ⟨⟨(hf h hh.1).1, hh.1⟩, ?_⟩
    intro q hq
    have he := (hf q.2 hq.2).2.1 hq.1
    exact he.trans (hh.2 hq.2)
  refine ⟨⟨hmap, fun _ _ _ _ e ↦ congrArg Prod.snd e, ?_⟩, ?_⟩
  · rintro ⟨q, h⟩ hqh
    have hh : 0 < h := hqh.1.2
    have hq : q ∈ Weibull.domain := hqh.1.1
    have hqmax : IsMaxOn (fun p ↦ finiteObjective x (p, h)) Weibull.domain q := by
      intro p hp
      exact isMaxOn_iff.mp hqh.2 (p, h) ⟨hp, hh⟩
    have hqn := (hf h hh).2.2 q hq hqmax
    refine ⟨h, ⟨hh, ?_⟩, by simp [hqn]⟩
    intro r hr
    have hle := isMaxOn_iff.mp hqh.2 (bestNuisance hn hx hne r, r) ⟨(hf r hr).1, hr⟩
    change profile hn hx hne r ≤ profile hn hx hne h
    simpa only [hqn, profile] using hle
  · intro h hh
    exact (hh.2.isLocalMax (Ioi_mem_nhds hh.1)).deriv_eq_zero

/-- A nonsingular nuisance score produces an analytic branch through the
chosen chart point. Any existing nuisance fit that tends to the point and
solves the same score equation agrees with this branch locally. -/
theorem analytic_nuisance_branch {E : Type*} [NormedAddCommGroup E]
    [NormedSpace ℝ E] [CompleteSpace E] {S : ℝ × E → E} {u : ℝ × E}
    (ha : AnalyticAt ℝ S u)
    (hi : (fderiv ℝ S u ∘L ContinuousLinearMap.inr ℝ ℝ E).IsInvertible) :
    ∃ ψ : ℝ → E, AnalyticAt ℝ ψ u.1 ∧ ψ u.1 = u.2 ∧
      (∀ᶠ h in 𝓝 u.1, S (h, ψ h) = S u) ∧
      (∀ᶠ v in 𝓝 u, S v = S u ↔ ψ v.1 = v.2) := by
  have hc : ContDiffAt ℝ ω S u := ha.contDiffAt
  have hn : (⊤ : ℕ∞ω) ≠ 0 := by simp
  refine ⟨hc.implicitFunction hn hi, (hc.contDiffAt_implicitFunction hn hi).analyticAt,
    hc.implicitFunction_apply_self hn hi, hc.eventually_apply_implicitFunction hn hi,
    hc.eventually_apply_eq_iff_implicitFunction hn hi⟩

/-- An analytic score that is nonzero somewhere on a connected interval
has an empty right collar of stationary points at any endpoint in its
analytic domain, and a finite stationary list on each compact window. -/
theorem analytic_collar_and_finite {S K : Set ℝ} {score : ℝ → ℝ}
    (ha : AnalyticOnNhd ℝ score S) (hs : IsPreconnected S) (h₀ : (0 : ℝ) ∈ S)
    (hne : ∃ t ∈ S, score t ≠ 0) (hK : IsCompact K) (hKS : K ⊆ S) :
    (∃ ε > 0, ∀ t ∈ Ioo 0 ε, score t ≠ 0) ∧ (K ∩ {t | score t = 0}).Finite := by
  obtain h | h := ha.eqOn_zero_or_eventually_ne_zero_of_preconnected hs
  · obtain ⟨t, ht, he⟩ := hne
    exact False.elim (he (h ht))
  · have hc : {t | score t ≠ 0} ∈ codiscreteWithin S := h
    refine ⟨?_, ?_⟩
    · have he : ∀ᶠ t in 𝓝[≠] (0 : ℝ), score t ≠ 0 := by
        by_contra! he
        have hz := ha.eqOn_zero_of_preconnected_of_frequently_eq_zero hs h₀ he
        obtain ⟨t, ht, hne⟩ := hne
        exact hne (hz ht)
      have hle : 𝓝[>] (0 : ℝ) ≤ 𝓝[≠] (0 : ℝ) :=
        nhdsWithin_mono _ (fun _ (ht : 0 < _) ↦ ht.ne')
      exact (nhdsGT_basis (0 : ℝ)).mem_iff.mp (hle he)
    · have hfin := hK.finite_sdiff_of_mem_codiscreteWithin (codiscreteWithin_mono hKS hc)
      convert hfin using 1
      ext t
      simp


/-- A nonvanishing analytic score has one strict sign throughout a
positive interval, so the profile is strictly monotone on that collar. -/
theorem signed_collar {F : ℝ → ℝ} {ε : ℝ} (hε : 0 < ε)
    (ha : AnalyticOnNhd ℝ F (Ici 0))
    (hn : ∀ h ∈ Ioo 0 ε, deriv F h ≠ 0) :
    StrictMonoOn F (Ioo 0 ε) ∨ StrictAntiOn F (Ioo 0 ε) := by
  let I := Ioo (0 : ℝ) ε
  have hsub : I ⊆ Ici 0 := fun _ ht ↦ ht.1.le
  have hcont : ContinuousOn (deriv F) I := ha.deriv.continuousOn.mono hsub
  have hc : IsPreconnected (deriv F '' I) := (convex_Ioo _ _).isPreconnected.image _ hcont
  have hnone : (0 : ℝ) ∉ deriv F '' I := by
    rintro ⟨h, hh, he⟩
    exact hn h hh he
  let h₀ := ε / 2
  have hm : h₀ ∈ I := ⟨by dsimp [h₀]; linarith, by dsimp [h₀]; linarith⟩
  have hm' : deriv F h₀ ∈ deriv F '' I := mem_image_of_mem _ hm
  rcases (hn h₀ hm).lt_or_gt with hneg | hpos
  · right
    apply strictAntiOn_of_deriv_neg (convex_Ioo _ _) (ha.continuousOn.mono hsub)
    intro t ht
    have ht' : t ∈ I := interior_subset ht
    by_contra! he
    exact hnone (hc.Icc_subset hm' (mem_image_of_mem _ ht') ⟨hneg.le, he⟩)
  · left
    apply strictMonoOn_of_deriv_pos (convex_Ioo _ _) (ha.continuousOn.mono hsub)
    intro t ht
    have ht' : t ∈ I := interior_subset ht
    by_contra! he
    exact hnone (hc.Icc_subset (mem_image_of_mem _ ht') hm' ⟨he, hpos.le⟩)

/-- Local analytic extensions through zero and at every positive point
give a common analytic representative on the nonnegative half-line. -/
theorem extend_nonnegative {f : ℝ → ℝ}
    (hlocal : ∀ h ≥ 0, ∃ F : ℝ → ℝ, AnalyticAt ℝ F h ∧ F =ᶠ[𝓝[Ici 0] h] f) :
    ∃ F : ℝ → ℝ, AnalyticOnNhd ℝ F (Ici 0) ∧ EqOn F f (Ici 0) := by
  obtain ⟨F₀, hA₀, he₀⟩ := hlocal 0 le_rfl
  let F := fun h ↦ if 0 ≤ h then f h else F₀ h
  have hF (h : ℝ) (hh : 0 ≤ h) : F h = f h := ite_eq_left hh
  refine ⟨F, ?_, fun h hh ↦ hF h hh⟩
  intro h hh
  rcases hh.eq_or_lt with he | he
  · subst h
    have hg : ∀ᶠ t in 𝓝 (0 : ℝ), t ∈ Ici 0 → F₀ t = f t :=
      eventually_nhdsWithin_iff.mp he₀
    apply (analyticAt_congr ?_).mp hA₀
    filter_upwards [hg] with t ht
    by_cases ht' : 0 ≤ t
    · exact (ht ht').trans (hF t ht').symm
    · simp [F, ht']
  · obtain ⟨G, hG, hGe⟩ := hlocal h he.le
    have hlim : 𝓝[(Ici 0 : Set ℝ)] h = 𝓝 h :=
      nhdsWithin_eq_nhds.mpr
        ((show ∀ᶠ t in 𝓝 h, 0 < t from Ioi_mem_nhds he).mono fun _ ht ↦ ht.le)
    rw [hlim] at hGe
    apply (analyticAt_congr ?_).mp hG
    filter_upwards [hGe, (show ∀ᶠ t in 𝓝 h, 0 < t from Ioi_mem_nhds he)] with t hGt ht
    exact hGt.trans (hF t ht.le).symm


end

end UnivariateML.PGWProfile

end
