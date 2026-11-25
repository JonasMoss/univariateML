module

public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Topology.Order.IntermediateValue

set_option linter.style.header false

/-!
# Maximizing with a decreasing score

Opposite score signs bracket a unique zero on an convex real domain.
Endpoint limits supply the signs. A positive parameter can be optimized
using the score in its log coordinate.
-/

@[expose] public section

open Set Filter Topology

namespace UnivariateML.Generic

/-- A bracketed strictly decreasing derivative has exactly one zero, and
that zero is exactly the set of maximizers on the domain. -/
theorem existsUnique_score_zero_of_signs {f score : ℝ → ℝ} {D : Set ℝ}
    (hconv : Convex ℝ D)
    (hd : ∀ t ∈ D, HasDerivAt f (score t) t) (hc : ContinuousOn score D)
    (hs : StrictAntiOn score D) {a b : ℝ}
    (ha : a ∈ D) (hb : b ∈ D) (hab : a ≤ b)
    (hsa : 0 < score a) (hsb : score b < 0) :
    (∃! t, t ∈ D ∧ score t = 0) ∧
      ∀ t ∈ D, IsMaxOn f D t ↔ score t = 0 := by
  have hsub : Icc a b ⊆ D := hconv.ordConnected.out ha hb
  obtain ⟨r, hr, hz⟩ := intermediate_value_Icc' hab (hc.mono hsub)
    (show (0 : ℝ) ∈ Icc (score b) (score a) from ⟨hsb.le, hsa.le⟩)
  have hrD := hsub hr
  have huniq : ∀ t ∈ D, score t = 0 → t = r :=
    fun t ht hzt ↦ hs.injOn ht hrD (hzt.trans hz.symm)
  have hf : ContinuousOn f D := fun t ht ↦ (hd t ht).continuousAt.continuousWithinAt
  have hinc : StrictMonoOn f (D ∩ Iic r) := by
    apply strictMonoOn_of_deriv_pos (hconv.inter (convex_Iic r)) (hf.mono inter_subset_left)
    intro t ht
    have htD := (interior_subset ht).1
    have htr : t < r := by
      simpa only [interior_Iic, mem_Iio] using (interior_mono inter_subset_right ht :
        t ∈ interior (Iic r))
    rw [(hd t htD).deriv, ← hz]
    exact hs htD hrD htr
  have hdec : StrictAntiOn f (D ∩ Ici r) := by
    apply strictAntiOn_of_deriv_neg (hconv.inter (convex_Ici r)) (hf.mono inter_subset_left)
    intro t ht
    have htD := (interior_subset ht).1
    have hrt : r < t := by
      simpa only [interior_Ici, mem_Ioi] using (interior_mono inter_subset_right ht :
        t ∈ interior (Ici r))
    rw [(hd t htD).deriv, ← hz]
    exact hs hrD htD hrt
  have hstrict : ∀ t ∈ D, t ≠ r → f t < f r := by
    intro t ht hne
    rcases lt_or_gt_of_ne hne with htr | hrt
    · exact hinc ⟨ht, htr.le⟩ ⟨hrD, le_rfl⟩ htr
    · exact hdec ⟨hrD, le_rfl⟩ ⟨ht, hrt.le⟩ hrt
  refine ⟨⟨r, ⟨hrD, hz⟩, fun t ht ↦ huniq t ht.1 ht.2⟩, ?_⟩
  intro t ht
  constructor
  · intro hm
    by_contra hzt
    have hne : t ≠ r := fun he ↦ hzt (he ▸ hz)
    exact (hstrict t ht hne).not_ge (hm hrD)
  · intro hzt
    rw [huniq t ht hzt]
    intro x hx
    by_cases h : x = r
    · simp [h]
    · exact (hstrict x hx h).le

/-- Opposite score limits along nontrivial feasible filters give a maximizing zero. -/
theorem existsUnique_score_zero_of_limits {f score : ℝ → ℝ} {D : Set ℝ}
    {left right : Filter ℝ} [left.NeBot] [right.NeBot]
    (hconv : Convex ℝ D)
    (hd : ∀ t ∈ D, HasDerivAt f (score t) t) (hc : ContinuousOn score D)
    (hs : StrictAntiOn score D)
    (hlD : ∀ᶠ t in left, t ∈ D) (hrD : ∀ᶠ t in right, t ∈ D)
    {u v : ℝ} (hu : 0 < u) (hv : v < 0)
    (hl : Tendsto score left (𝓝 u)) (hr : Tendsto score right (𝓝 v)) :
    (∃! t, t ∈ D ∧ score t = 0) ∧
      ∀ t ∈ D, IsMaxOn f D t ↔ score t = 0 := by
  obtain ⟨a, ha, hsa⟩ := (hlD.and (hl.eventually (Ioi_mem_nhds hu))).exists
  obtain ⟨b, hb, hsb⟩ := (hrD.and (hr.eventually (Iio_mem_nhds hv))).exists
  have hab : a ≤ b := le_of_not_gt (fun h ↦ (hs hb ha h).not_gt (hsb.trans hsa))
  exact existsUnique_score_zero_of_signs hconv hd hc hs ha hb hab hsa hsb

/-- On the real line, opposite finite endpoint limits locate the unique maximizing score zero. -/
theorem existsUnique_score_zero_real {f score : ℝ → ℝ}
    (hd : ∀ t, HasDerivAt f (score t) t) (hc : Continuous score) (hs : StrictAnti score)
    {u v : ℝ} (hu : 0 < u) (hv : v < 0)
    (hl : Tendsto score atBot (𝓝 u)) (hr : Tendsto score atTop (𝓝 v)) :
    (∃! t, score t = 0) ∧ ∀ t, IsMaxOn f univ t ↔ score t = 0 := by
  simpa only [mem_univ, true_and, true_implies] using
    existsUnique_score_zero_of_limits (convex_univ (𝕜 := ℝ))
      (fun t _ ↦ hd t) hc.continuousOn (hs.strictAntiOn univ)
      (Eventually.of_forall mem_univ) (Eventually.of_forall mem_univ) hu hv hl hr

/-- On a finite open interval, use the limits taken from within that interval. -/
theorem existsUnique_score_zero_Ioo {f score : ℝ → ℝ} {a b : ℝ} (hab : a < b)
    (hd : ∀ t ∈ Ioo a b, HasDerivAt f (score t) t) (hc : ContinuousOn score (Ioo a b))
    (hs : StrictAntiOn score (Ioo a b)) {u v : ℝ} (hu : 0 < u) (hv : v < 0)
    (hl : Tendsto score (𝓝[Ioo a b] a) (𝓝 u))
    (hr : Tendsto score (𝓝[Ioo a b] b) (𝓝 v)) :
    (∃! t, t ∈ Ioo a b ∧ score t = 0) ∧
      ∀ t ∈ Ioo a b, IsMaxOn f (Ioo a b) t ↔ score t = 0 := by
  let := left_nhdsWithin_Ioo_neBot hab
  let := right_nhdsWithin_Ioo_neBot hab
  exact existsUnique_score_zero_of_limits (convex_Ioo a b) hd hc hs
    self_mem_nhdsWithin self_mem_nhdsWithin hu hv hl hr

/-- A score in log coordinates identifies the unique positive maximizer. -/
theorem existsUnique_log_score_zero {f score : ℝ → ℝ}
    (hd : ∀ t, HasDerivAt (fun s ↦ f (Real.exp s)) (score t) t)
    (hc : Continuous score) (hs : StrictAnti score)
    {u v : ℝ} (hu : 0 < u) (hv : v < 0)
    (hl : Tendsto score atBot (𝓝 u)) (hr : Tendsto score atTop (𝓝 v)) :
    (∃! x, 0 < x ∧ score (Real.log x) = 0) ∧
      ∀ x > 0, IsMaxOn f (Ioi 0) x ↔ score (Real.log x) = 0 := by
  obtain ⟨⟨t, ht, huniq⟩, hmax⟩ := existsUnique_score_zero_real hd hc hs hu hv hl hr
  refine ⟨⟨Real.exp t, ⟨Real.exp_pos _, by simpa using ht⟩, ?_⟩, ?_⟩
  · intro x ⟨hx, hzx⟩
    rw [← huniq (Real.log x) hzx, Real.exp_log hx]
  · intro x hx
    rw [← hmax (Real.log x)]
    constructor
    · intro hm s _
      change f (Real.exp s) ≤ f (Real.exp (Real.log x))
      rw [Real.exp_log hx]
      exact hm (show Real.exp s ∈ Ioi (0 : ℝ) from Real.exp_pos s)
    · intro hm y hy
      have h := hm (show Real.log y ∈ (univ : Set ℝ) from mem_univ _)
      change f (Real.exp (Real.log y)) ≤ f (Real.exp (Real.log x)) at h
      rwa [Real.exp_log hy, Real.exp_log hx] at h

end UnivariateML.Generic
