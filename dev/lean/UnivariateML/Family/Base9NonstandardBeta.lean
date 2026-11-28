module

public import UnivariateML.Family.Endpoint3NonstandardBeta

/-!
# Nonstandard beta with one shape held

The opposite shape can be fixed at any strictly subunit positive value
along the singular endpoint path. The other endpoint stays fixed.
-/

@[expose] public section

open Filter Set
open scoped Topology

namespace UnivariateML.Base9NonstandardBeta

variable {n : ℕ} {x : Fin n → ℝ} {l r a b : ℝ}

open Endpoint2NonstandardBeta

/-- With the lower shape held and upper shape subunit, there is an
unbounded path at the sample maximum with the lower endpoint fixed. -/
theorem held_shape1_endpoint_spike (hl : ∀ i, l < x i) (hr : ∀ i, x i ≤ r)
    (hmax : ∃ i, x i = r) (_ha : 0 < a) (_hb : 0 < b) (hb1 : b < 1) :
    let R := fun t : ℝ ↦ l + (r - l) / (1 - t)
    (∀ᶠ t in 𝓝[>] 0, ∀ i, x i ∈ Ioo l (R t)) ∧
      Tendsto R (𝓝[>] 0) (𝓝 r) ∧
      Tendsto (fun t ↦ NonstandardBeta.logLik x l (R t) (a, b)) (𝓝[>] 0) atTop := by
  intro R
  obtain ⟨j, hj⟩ := hmax
  have hZ : 0 < r - l := sub_pos.mpr (hj ▸ hl j)
  refine ⟨?_, ?_, ?_⟩
  · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
      mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one)] with t ht ht'
    have hdist : r - l < (r - l) / (1 - t) := by
      apply (lt_div_iff₀ (sub_pos.mpr ht')).mpr
      have htpos : 0 < t := ht
      nlinarith [mul_pos hZ htpos]
    exact fun i ↦ ⟨hl i, (hr i).trans_lt (by dsimp [R]; linarith)⟩
  · have h : ContinuousAt R 0 := by
      dsimp [R]
      fun_prop (disch := norm_num)
    simpa [R] using h.tendsto.mono_left nhdsWithin_le_nhds
  · apply (fixed_shapes_spike (z := fun i ↦ x i - l) (b := a)
      hZ ⟨fun i ↦ (sub_pos.mpr (hl i)).le, Or.inr fun i ↦ sub_pos.mpr (hl i)⟩
      (fun i ↦ sub_le_sub_right (hr i) l) ⟨j, by simp [hj]⟩ hb1).congr'
    exact Eventually.of_forall fun t ↦ by
      unfold widthLogLik NonstandardBeta.logLik
      simp only [R, add_sub_cancel_left, sub_zero,
        show ∀ i, l + (r - l) / (1 - t) - x i = (r - l) / (1 - t) - (x i - l) by
          intro i
          ring]

/-- With the upper shape held and lower shape subunit, the reflected
path diverges at the sample minimum with the upper endpoint fixed. -/
theorem held_shape2_endpoint_spike (hl : ∀ i, l ≤ x i) (hr : ∀ i, x i < r)
    (hmin : ∃ i, x i = l) (ha : 0 < a) (ha1 : a < 1) (hb : 0 < b) :
    let L := fun t : ℝ ↦ -(-r + (r - l) / (1 - t))
    (∀ᶠ t in 𝓝[>] 0, ∀ i, x i ∈ Ioo (L t) r) ∧
      Tendsto L (𝓝[>] 0) (𝓝 l) ∧
      Tendsto (fun t ↦ NonstandardBeta.logLik x (L t) r (a, b)) (𝓝[>] 0) atTop := by
  intro L
  have h := held_shape1_endpoint_spike (x := fun i ↦ -x i) (l := -r) (r := -l)
    (fun i ↦ neg_lt_neg (hr i)) (fun i ↦ neg_le_neg (hl i))
    (hmin.imp fun _ hi ↦ congrArg Neg.neg hi) hb ha ha1
  have hdist : -l - -r = r - l := by ring
  simp only [hdist] at h
  refine ⟨h.1.mono (fun t ht i ↦ ⟨by dsimp [L]; linarith [(ht i).2],
    by linarith [(ht i).1]⟩), by simpa [L] using h.2.1.neg, ?_⟩
  apply h.2.2.congr'
  exact Eventually.of_forall fun t ↦ by
    have hB : Beta.logBeta (b, a) = Beta.logBeta (a, b) := by
      simp [Beta.logBeta, Beta.normalizer, mul_comm, add_comm]
    unfold NonstandardBeta.logLik
    apply Finset.sum_congr rfl
    intro i _
    simp only [L, hB, sub_neg_eq_add]
    ring_nf

end UnivariateML.Base9NonstandardBeta

end
