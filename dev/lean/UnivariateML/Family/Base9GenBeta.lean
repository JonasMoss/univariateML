module

public import UnivariateML.Family.Endpoint3GeneralizedBeta
public import Mathlib.Data.EReal.Basic

/-!
# Generalized beta base regimes

## Deviations

The held-endpoint result covers the constant branch. Nonconstant
attainment and the classification of all constant-data escapes remain open here.
The fixed-shapes result includes the extended likelihood at endpoint contact.
-/

@[expose] public section

open Real Filter Set
open scoped Topology

namespace UnivariateML.Base9GenBeta

open Endpoint2GeneralizedBeta

/-- With endpoint and first beta shape held, a constant interior sample has
unbounded log likelihood and no finite maximizer. -/
theorem held_scale_shape1_constant_no_fit {n : ℕ} (hn : 0 < n)
    {v b p : ℝ} (hv : 0 < v) (hb : v < b) (hp : 0 < p) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun u : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) b p u.1 u.2
    ¬ BddAbove (L '' D) ∧ ¬ ∃ u ∈ D, IsMaxOn L D u := by
  intro D L
  have h : ¬ BddAbove (L '' D) := by
    rintro ⟨B, hB⟩
    have hpath := Endpoint3GeneralizedBeta.held_scale_first_shape_constant_path hn hv hb hp
    obtain ⟨c, hc, hgt⟩ :=
      ((eventually_gt_atTop (0 : ℝ)).and (hpath.eventually_gt_atTop B)).exists
    let q := p * exp (c * log (b / v))
    have hu : (q, c) ∈ D := ⟨mul_pos hp (exp_pos _), hc⟩
    exact hgt.not_ge (hB ⟨(q, c), hu, rfl⟩)
  exact ⟨h, fun ⟨_, _, hm⟩ ↦ h hm.bddAbove⟩

private theorem sum_le_term {n : ℕ} (f : Fin n → ℝ) (i : Fin n)
    (h : ∀ k, f k ≤ 0) : (∑ k, f k) ≤ f i := by
  classical
  rw [← Finset.sum_erase_add _ _ (Finset.mem_univ i)]
  exact add_le_of_nonpos_left (Finset.sum_nonpos fun k _ ↦ h k)

private theorem unit_bounds {x M b c : ℝ} (hx : 0 < x) (hle : x ≤ M)
    (hb : M < b) (hc : 0 < c) : (x / b) ^ c ∈ Ioo (0 : ℝ) 1 := by
  have hbp := hx.trans (hle.trans_lt hb)
  exact ⟨rpow_pos_of_pos (div_pos hx hbp) _,
    by
      simpa using (rpow_lt_rpow (div_nonneg hx.le hbp.le)
        ((div_lt_one hbp).mpr (hle.trans_lt hb)) hc)⟩

/-- When all three shapes are fixed, a subunit upper shape forces every
divergent strict-interior path to contact. For upper shape at least one,
a finite maximum exists and every finite superlevel is confined to a
compact subset of the feasible endpoint domain. -/
theorem fixed_shapes_failure {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℝ} {M p q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (_hq : 0 < q) (hc : 0 < c) :
    let D := if q = 1 then Ici M else Ioi M
    let L := fun b ↦ logLik x b p q c
    let E : ℝ → EReal := fun b ↦
      if M < b ∨ (b = M ∧ q = 1) then (L b : EReal) else if b = M ∧ q < 1 then ⊤ else ⊥
    (∀ b, E b = ⊤ ↔ b = M ∧ q < 1) ∧
    (q < 1 → Tendsto (fun t ↦ L (M / (1 - t) ^ c⁻¹)) (𝓝[>] 0) atTop ∧
      ∀ {ι : Type} {l : Filter ι} {b : ι → ℝ}, (∀ᶠ j in l, b j ∈ D) →
        (Tendsto (L ∘ b) l atTop ↔ Tendsto b l (𝓝 M))) ∧
    (1 ≤ q → (∃ b ∈ D, IsMaxOn L D b) ∧
      ∀ A : ℝ, ∃ K : Set ℝ, IsCompact K ∧ K ⊆ D ∧
        ∀ b ∈ D, A ≤ L b → b ∈ K) := by
  intro D L E
  obtain ⟨i, hi⟩ := heq
  have hM : 0 < M := hi ▸ hx i
  refine ⟨?_, ?_, ?_⟩
  · intro b
    by_cases hb : M < b ∨ (b = M ∧ q = 1)
    · have h : ¬ (b = M ∧ q < 1) := by
        rintro ⟨rfl, hq'⟩
        simp [hq'.ne] at hb
      simp [E, hb, h]
    · by_cases h : b = M ∧ q < 1
      · simp [E, h, h.2.ne]
      · simp [E, hb, h]
  · intro hq'
    refine ⟨endpoint_spike hx hle ⟨i, hi⟩ hq' hc, ?_⟩
    intro ι l b hb
    have hpos : ∀ᶠ j in l, M < b j := by simpa [D, hq'.ne] using hb
    refine ⟨Endpoint3GeneralizedBeta.subunit_divergence_requires_contact hx hle hp hq' hc hpos, ?_⟩
    intro hcontact
    let t := fun j ↦ 1 - (M / b j) ^ c
    have hcont : ContinuousAt (fun b ↦ 1 - (M / b) ^ c) M := by
      fun_prop (disch := positivity)
    have ht : Tendsto t l (𝓝[>] 0) := by
      apply tendsto_nhdsWithin_iff.mpr
      refine ⟨?_, hpos.mono fun j hj ↦ sub_pos.mpr (unit_bounds hM le_rfl hj hc).2⟩
      simpa [t, Function.comp_def, hM.ne'] using hcont.tendsto.comp hcontact
    apply ((endpoint_spike (p := p) hx hle ⟨i, hi⟩ hq' hc).comp ht).congr'
    filter_upwards [hpos] with j hj
    have hr := div_pos hM (hM.trans hj)
    have he : (1 - t j) ^ c⁻¹ = M / b j := by
      rw [show 1 - t j = (M / b j) ^ c by simp [t], ← rpow_mul hr.le,
        mul_inv_cancel₀ hc.ne', rpow_one]
    have he' : M / (M / b j) = b j := div_div_cancel₀ hM.ne'
    simp only [Function.comp_apply, he, he', L]
  · intro hq'
    refine ⟨(fixed_shapes_endpoint_fit hn hx hle ⟨i, hi⟩ hp hc).1.mpr hq', ?_⟩
    let C := ∑ k, (log c - log (x k) - Beta.logBeta (p, q))
    have hsplit (b : ℝ) :
        L b = C + (∑ k, p * log (unitData x b c k)) +
          ∑ k, (q - 1) * log (1 - unitData x b c k) := by
      simp only [L, logLik, Finset.sum_add_distrib, C]
    have hleft (b : ℝ) (hb : M < b) :
        L b ≤ C + p * log (unitData x b c i) := by
      rw [hsplit]
      have h₁ : (∑ k, p * log (unitData x b c k)) ≤ p * log (unitData x b c i) :=
        sum_le_term _ i (fun k ↦ mul_nonpos_of_nonneg_of_nonpos hp.le
          (log_nonpos (unit_bounds (hx k) (hle k) hb hc).1.le
            (unit_bounds (hx k) (hle k) hb hc).2.le))
      have h₂ : (∑ k, (q - 1) * log (1 - unitData x b c k)) ≤ 0 :=
        Finset.sum_nonpos fun k _ ↦ mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hq')
          (log_nonpos (sub_nonneg.mpr (unit_bounds (hx k) (hle k) hb hc).2.le)
            (sub_le_self _ (unit_bounds (hx k) (hle k) hb hc).1.le))
      linarith
    have hright (b : ℝ) (hb : M < b) :
        L b ≤ C + (q - 1) * log (1 - unitData x b c i) := by
      rw [hsplit]
      have h₁ : (∑ k, p * log (unitData x b c k)) ≤ 0 :=
        Finset.sum_nonpos fun k _ ↦ mul_nonpos_of_nonneg_of_nonpos hp.le
          (log_nonpos (unit_bounds (hx k) (hle k) hb hc).1.le
            (unit_bounds (hx k) (hle k) hb hc).2.le)
      have h₂ : (∑ k, (q - 1) * log (1 - unitData x b c k)) ≤
          (q - 1) * log (1 - unitData x b c i) :=
        sum_le_term _ i (fun k ↦ mul_nonpos_of_nonneg_of_nonpos
          (sub_nonneg.mpr hq')
          (log_nonpos (sub_nonneg.mpr (unit_bounds (hx k) (hle k) hb hc).2.le)
            (sub_le_self _ (unit_bounds (hx k) (hle k) hb hc).1.le)))
      linarith
    intro A
    have hinfty : Tendsto L atTop atBot := by
      apply tendsto_atBot_mono' _ ?_ ?_ (f₁ := fun b ↦ C + (p * c) * (log M - log b))
      · filter_upwards [eventually_gt_atTop M] with b hb
        have hbound := hleft b hb
        rw [unitData, hi, log_rpow (div_pos hM (hM.trans hb)),
          log_div hM.ne' (hM.trans hb).ne'] at hbound
        simpa only [mul_assoc] using hbound
      · exact tendsto_atBot_add_const_left _ _
          ((tendsto_atBot_add_const_left _ _ (tendsto_neg_atTop_atBot.comp tendsto_log_atTop)).const_mul_atBot
            (mul_pos hp hc))
    obtain ⟨B, hB⟩ := eventually_atTop.mp (hinfty.eventually_lt_atBot A)
    by_cases hq1 : q = 1
    · refine ⟨Icc M B, isCompact_Icc, ?_, ?_⟩
      · intro b hb
        simpa [D, hq1] using hb.1
      · intro b hb hAb
        exact ⟨by simpa [D, hq1] using hb, le_of_not_ge fun h ↦ (hB b h).not_ge hAb⟩
    · have hqpos : 0 < q - 1 := sub_pos.mpr (lt_of_le_of_ne hq' (Ne.symm hq1))
      have hz : Tendsto (fun b ↦ 1 - unitData x b c i) (𝓝[>] M) (𝓝[>] 0) := by
        have hcont : ContinuousAt (fun b ↦ 1 - unitData x b c i) M := by
          unfold unitData
          fun_prop (disch := positivity)
        apply tendsto_nhdsWithin_iff.mpr
        refine ⟨?_, ?_⟩
        · simpa [unitData, hi, hM.ne'] using hcont.tendsto.mono_left nhdsWithin_le_nhds
        · exact (show ∀ᶠ b in 𝓝[>] M, b ∈ Ioi M from self_mem_nhdsWithin).mono fun b hb ↦
            sub_pos.mpr (unit_bounds (hx i) (hle i) hb hc).2
      have hcontact : Tendsto L (𝓝[>] M) atBot := by
        apply tendsto_atBot_mono' _ ?_ ?_
          (f₁ := fun b ↦ C + (q - 1) * log (1 - unitData x b c i))
        · exact (show ∀ᶠ b in 𝓝[>] M, b ∈ Ioi M from self_mem_nhdsWithin).mono fun b hb ↦ hright b hb
        · exact tendsto_atBot_add_const_left _ _
            ((tendsto_log_nhdsGT_zero.comp hz).const_mul_atBot hqpos)
      obtain ⟨a, ha, ha'⟩ := (mem_nhdsGT_iff_exists_Ioo_subset.mp
        (hcontact.eventually_lt_atBot A))
      refine ⟨Icc a B, isCompact_Icc, ?_, ?_⟩
      · intro b hb
        simpa [D, hq1] using ha.trans_le hb.1
      · intro b hb hAb
        have hb' : M < b := by simpa [D, hq1] using hb
        exact ⟨le_of_not_gt fun h ↦ (ha' ⟨hb', h⟩).not_ge hAb,
          le_of_not_ge fun h ↦ (hB b h).not_ge hAb⟩

end UnivariateML.Base9GenBeta

end
