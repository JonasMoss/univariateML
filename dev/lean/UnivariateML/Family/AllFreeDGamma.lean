module

public import UnivariateML.Family.Discrete3DGamma

set_option linter.style.header false

/-! Discretized gamma's zero-count collapse and strict finite mass bound. -/

@[expose] public section

open Set Filter MeasureTheory ProbabilityTheory Real
open scoped Topology ENNReal
namespace UnivariateML.AllFreeDGamma
noncomputable section

private theorem cdf_zero {a : ℝ} (ha : 0 < a) :
    cdf (gammaMeasure a 1) 0 = 0 := by
  rw [cdf_gammaMeasure_eq_integral ha one_pos, integral_Iic_eq_integral_Iio]
  apply setIntegral_eq_zero_of_forall_eq_zero
  intro y hy
  simp [gammaPDFReal, not_le.mpr (mem_Iio.mp hy)]

/-- With both coefficients free, the all-zero likelihood has supremum one,
which no positive finite parameter pair reaches. -/
theorem all_zero_supremum {n : ℕ} (hn : 0 < n) :
    IsLUB {v | ∃ a > 0, ∃ r > 0,
      Discrete3DGamma.likelihood (fun _ : Fin n ↦ 0) a r = v} 1 ∧
    ∀ a > 0, ∀ r > 0, Discrete3DGamma.likelihood (fun _ : Fin n ↦ 0) a r < 1 := by
  have hv (a r : ℝ) (ha : 0 < a) :
      Discrete3DGamma.likelihood (fun _ : Fin n ↦ 0) a r =
        (cdf (gammaMeasure a 1) r)^n := by
    simp [Discrete3DGamma.likelihood, Discrete3DGamma.cellProb, cdf_zero ha]
  have hstrict (a : ℝ) (ha : 0 < a) (r : ℝ) (hr : 0 < r) :
      Discrete3DGamma.likelihood (fun _ : Fin n ↦ 0) a r < 1 := by
    rw [hv a r ha]
    exact (Discrete3DGamma.all_zero_held_shape ha hn).2 r hr
  refine ⟨⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨a, ha, r, hr, rfl⟩
    exact (hstrict a ha r hr).le
  · intro B hB
    apply le_of_tendsto (Discrete3DGamma.all_zero_held_shape one_pos hn).1
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
    exact hB ⟨1, one_pos, r, hr, hv 1 r one_pos⟩

/-- Two adjacent cells together have strictly less than unit probability
at every finite positive parameter pair, including the cells zero and one. -/
theorem adjacent_mass_lt_one {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (j : ℕ) :
    Discrete3DGamma.cellProb a r j + Discrete3DGamma.cellProb a r (j + 1) < 1 := by
  have hp : 0 < ((j : ℝ) + 2) * r := by positivity
  have h := (Discrete3DGamma.all_zero_held_shape ha (n := 1) Nat.one_pos).2 _ hp
  rw [pow_one] at h
  simp only [Discrete3DGamma.cellProb, Nat.cast_add, Nat.cast_one,
    show (j : ℝ) + 1 + 1 = (j : ℝ) + 2 by ring]
  linarith [cdf_nonneg (gammaMeasure a 1) ((j : ℝ) * r)]

private theorem gamma_integral {a : ℝ} (ha : 0 < a) :
    ∫ y, gammaPDFReal a 1 y = 1 := by
  rw [integral_eq_lintegral_of_nonneg_ae
    (ae_of_all _ (gammaPDFReal_nonneg ha one_pos))
    (measurable_gammaPDFReal a 1).aestronglyMeasurable]
  change (∫⁻ y, gammaPDF a 1 y).toReal = 1
  rw [lintegral_gammaPDF_eq_one ha one_pos, ENNReal.toReal_one]

private theorem gamma_integrable {a : ℝ} (ha : 0 < a) :
    Integrable (gammaPDFReal a 1) := by
  refine ⟨(measurable_gammaPDFReal a 1).aestronglyMeasurable, ?_⟩
  rw [hasFiniteIntegral_iff_ofReal (ae_of_all _ (gammaPDFReal_nonneg ha one_pos))]
  change (∫⁻ y, gammaPDF a 1 y) < ⊤
  rw [lintegral_gammaPDF_eq_one ha one_pos]
  simp

private theorem shift_density {a : ℝ} (ha : 0 < a) :
    (fun y ↦ y * gammaPDFReal a 1 y) =ᵐ[volume]
      fun y ↦ a * gammaPDFReal (a + 1) 1 y := by
  have hzero : ∀ᵐ y ∂(volume : Measure ℝ), y ≠ 0 := by simp [ae_iff, measure_singleton]
  filter_upwards [hzero] with y hy
  rcases lt_or_gt_of_ne hy with hy | hy
  · simp [gammaPDFReal, not_le.mpr hy]
  · have hp := Gamma_pos_of_pos ha
    simp only [gammaPDFReal, ite_eq_left hy.le, one_rpow, one_mul, Real.Gamma_add_one ha.ne']
    rw [show a + 1 - 1 = a by ring, show a = a - 1 + 1 by ring, rpow_add hy, rpow_one]
    field_simp [ha.ne', hp.ne']
    ring

private theorem central_square {a : ℝ} (ha : 0 < a) :
    Integrable (fun y ↦ (y - a)^2 * gammaPDFReal a 1 y) ∧
      ∫ y, (y - a)^2 * gammaPDFReal a 1 y = a := by
  have h1 := shift_density ha
  have h2 : (fun y ↦ y^2 * gammaPDFReal a 1 y) =ᵐ[volume]
      fun y ↦ (a * (a + 1)) * gammaPDFReal (a + 2) 1 y := by
    filter_upwards [h1, shift_density (show 0 < a + 1 by positivity)] with y hy hys
    change y * gammaPDFReal a 1 y = a * gammaPDFReal (a + 1) 1 y at hy
    calc
      _ = y * (y * gammaPDFReal a 1 y) := by ring
      _ = a * (y * gammaPDFReal (a + 1) 1 y) := by rw [hy]; ring
      _ = _ := by rw [hys, show a + 1 + 1 = a + 2 by ring]; ring
  have hi0 := gamma_integrable ha
  have hi1 := ((gamma_integrable (show 0 < a + 1 by positivity)).const_mul a).congr h1.symm
  have hi2 := ((gamma_integrable (show 0 < a + 2 by positivity)).const_mul (a * (a + 1))).congr
    h2.symm
  have he : (fun y ↦ (y - a)^2 * gammaPDFReal a 1 y) =
      fun y ↦ y^2 * gammaPDFReal a 1 y - (2 * a) * (y * gammaPDFReal a 1 y) +
        a^2 * gammaPDFReal a 1 y := by funext y; ring
  refine ⟨he ▸ ((hi2.sub (hi1.const_mul _)).add (hi0.const_mul _)), ?_⟩
  have hsum := integral_add (hi2.sub (hi1.const_mul (2 * a))) (hi0.const_mul (a^2))
  have hsub := integral_sub hi2 (hi1.const_mul (2 * a))
  rw [he]
  simp only [Pi.sub_apply] at hsum hsub
  rw [hsum, hsub, integral_const_mul, integral_const_mul,
    integral_congr_ae h2, integral_congr_ae h1, integral_const_mul, integral_const_mul,
    gamma_integral ha, gamma_integral (show 0 < a + 1 by positivity),
    gamma_integral (show 0 < a + 2 by positivity)]
  ring

private theorem cell_integral {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (j : ℕ) :
    Discrete3DGamma.cellProb a r j =
      ∫ y in Ioc ((j : ℝ) * r) ((j + 1 : ℝ) * r), gammaPDFReal a 1 y := by
  symm
  have hI := gamma_integrable ha
  have h := setIntegral_union (f := gammaPDFReal a 1)
    (s := Iic ((j : ℝ) * r)) (t := Ioc ((j : ℝ) * r) ((j + 1 : ℝ) * r))
    (by rw [Set.disjoint_left]; intro y hy hy'; exact hy'.1.not_ge hy)
    measurableSet_Ioc hI.integrableOn hI.integrableOn
  have hle : (j : ℝ) * r ≤ (j + 1 : ℝ) * r := by nlinarith
  rw [Iic_union_Ioc_eq_Iic hle] at h
  rw [Discrete3DGamma.cellProb, cdf_gammaMeasure_eq_integral ha one_pos,
    cdf_gammaMeasure_eq_integral ha one_pos]
  linarith

private theorem cell_integral_compl {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (j : ℕ) :
    1 - Discrete3DGamma.cellProb a r j =
      ∫ y in (Ioc ((j : ℝ) * r) ((j + 1 : ℝ) * r))ᶜ, gammaPDFReal a 1 y := by
  have hI := gamma_integrable ha
  have hsum := integral_add_compl (μ := volume)
    (s := Ioc ((j : ℝ) * r) ((j + 1 : ℝ) * r)) measurableSet_Ioc hI
  rw [gamma_integral ha, ← cell_integral ha hr] at hsum
  linarith

/-- Keep the latent mean strictly inside a specified cell as shape tends to
infinity. The fitted mass of that cell converges to one. -/
theorem cell_concentration (j : ℕ) {m : ℝ} (hm : (j : ℝ) < m) (hm' : m < j + 1) :
    Tendsto (fun a : ℝ ↦ Discrete3DGamma.cellProb a (a / m) j) atTop (𝓝 1) := by
  have hm0 : 0 < m := (Nat.cast_nonneg j).trans_lt hm
  let e := min (1 - j / m) ((j + 1) / m - 1)
  have he : 0 < e := lt_min (sub_pos.mpr ((div_lt_one hm0).mpr hm))
    (sub_pos.mpr ((one_lt_div hm0).mpr hm'))
  have hbound (a : ℝ) (ha : 0 < a) :
      1 - Discrete3DGamma.cellProb a (a / m) j ≤ (e^2 * a)⁻¹ := by
    rw [cell_integral_compl ha (div_pos ha hm0)]
    let S := (Ioc ((j : ℝ) * (a / m)) ((j + 1 : ℝ) * (a / m)))ᶜ
    let f := fun y ↦ (y - a)^2 * gammaPDFReal a 1 y
    have hS : MeasurableSet S := measurableSet_Ioc.compl
    have hf := central_square ha
    have hfm : (∫ y in S, f y) ≤ a := by
      rw [← hf.2]
      exact setIntegral_le_integral hf.1 (ae_of_all _ fun y ↦ by
        dsimp [f]; positivity [gammaPDFReal_nonneg ha one_pos y])
    have hmon : (∫ y in S, gammaPDFReal a 1 y) ≤ ∫ y in S, ((e * a)^2)⁻¹ * f y := by
      apply integral_mono_ae (gamma_integrable ha).integrableOn (hf.1.const_mul _).integrableOn
      apply (ae_restrict_iff' hS).2
      apply ae_of_all
      intro y hy
      have hc : (e * a)^2 ≤ (y - a)^2 := by
        rcases not_and_or.mp (show ¬ ((j : ℝ) * (a / m) < y ∧ y ≤ (j + 1 : ℝ) * (a / m))
          from hy) with h | h
        · have he' := mul_le_mul_of_nonneg_right (min_le_left (1 - j / m) ((j + 1) / m - 1)) ha.le
          have : e * a ≤ a - y := by linarith only [he', not_lt.mp h,
            show (1 - (j : ℝ) / m) * a = a - j * (a / m) by ring]
          calc
            _ ≤ (a - y)^2 := pow_le_pow_left₀ (mul_pos he ha).le this 2
            _ = _ := by ring
        · have he' := mul_le_mul_of_nonneg_right (min_le_right (1 - j / m) ((j + 1) / m - 1)) ha.le
          have : e * a ≤ y - a := by linarith only [he', (not_le.mp h).le,
            show (((j : ℝ) + 1) / m - 1) * a = (j + 1) * (a / m) - a by ring]
          exact pow_le_pow_left₀ (mul_pos he ha).le this 2
      dsimp [f]
      apply (le_inv_mul_iff₀ (by positivity : 0 < (e * a)^2)).2
      exact mul_le_mul_of_nonneg_right hc (gammaPDFReal_nonneg ha one_pos y)
    rw [integral_const_mul] at hmon
    calc
      _ ≤ ((e * a)^2)⁻¹ * a := hmon.trans (mul_le_mul_of_nonneg_left hfm (by positivity))
      _ = _ := by field_simp
  have ht : Tendsto (fun a : ℝ ↦ (e^2 * a)⁻¹) atTop (𝓝 0) :=
    tendsto_inv_atTop_zero.comp (tendsto_id.const_mul_atTop (sq_pos_of_pos he))
  have hl : Tendsto (fun a : ℝ ↦ 1 - Discrete3DGamma.cellProb a (a / m) j) atTop (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds ht
    · filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
      rw [cell_integral_compl ha (div_pos ha hm0)]
      exact integral_nonneg (gammaPDFReal_nonneg ha one_pos)
    · exact (eventually_gt_atTop (0 : ℝ)).mono fun a ha ↦ hbound a ha
  simpa using hl.const_sub (1 : ℝ)

/-- The all-free supremum for a constant count is one, including when the
common count is positive. No finite shape and rate attain it. -/
theorem constant_supremum {n : ℕ} (hn : 0 < n) (j : ℕ) :
    IsLUB {v | ∃ a > 0, ∃ r > 0,
      Discrete3DGamma.likelihood (fun _ : Fin n ↦ j) a r = v} 1 ∧
    ∀ a > 0, ∀ r > 0, Discrete3DGamma.likelihood (fun _ : Fin n ↦ j) a r < 1 := by
  have hnonneg (a r : ℝ) (hr : 0 < r) (k : ℕ) : 0 ≤ Discrete3DGamma.cellProb a r k := by
    unfold Discrete3DGamma.cellProb
    exact sub_nonneg.mpr (monotone_cdf _ (by nlinarith))
  have hstrict (a : ℝ) (ha : 0 < a) (r : ℝ) (hr : 0 < r) :
      Discrete3DGamma.likelihood (fun _ : Fin n ↦ j) a r < 1 := by
    have hj := adjacent_mass_lt_one ha hr j
    have hj' := hnonneg a r hr (j + 1)
    have hjlt : Discrete3DGamma.cellProb a r j < 1 := by linarith
    simpa [Discrete3DGamma.likelihood] using pow_lt_one₀ (hnonneg a r hr j) hjlt hn.ne'
  let m : ℝ := j + 1 / 2
  have hm : 0 < m := by dsimp [m]; positivity
  have ht := (cell_concentration j (m := m) (by dsimp [m]; linarith)
    (by dsimp [m]; linarith)).pow n
  have ht' : Tendsto (fun a : ℝ ↦ Discrete3DGamma.likelihood (fun _ : Fin n ↦ j) a (a / m))
      atTop (𝓝 1) := by simpa [Discrete3DGamma.likelihood] using ht
  refine ⟨⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨a, ha, r, hr, rfl⟩
    exact (hstrict a ha r hr).le
  · intro B hB
    apply le_of_tendsto ht'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
    exact hB ⟨a, ha, a / m, div_pos ha hm, rfl⟩

/-- For two adjacent cells, a rate chosen to give the prescribed CDF at
their shared endpoint realizes arbitrary positive limiting proportions. -/
theorem adjacent_collapse_path (j : ℕ) {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) :
    ∃ r : ℝ → ℝ, (∀ᶠ a in atTop, 0 < r a) ∧
      Tendsto (fun a ↦ a / r a) atTop (𝓝 ((j : ℝ) + 1)) ∧
      Tendsto (fun a ↦ Discrete3DGamma.cellProb a (r a) j) atTop (𝓝 w) ∧
      Tendsto (fun a ↦ Discrete3DGamma.cellProb a (r a) (j + 1)) atTop (𝓝 (1 - w)) := by
  let lo : ℝ := j + 1 / 2
  let hi : ℝ := (j + 1 : ℕ) + 1 / 2
  have hlo : 0 < lo := by dsimp [lo]; positivity
  have hhi : 0 < hi := by dsimp [hi]; positivity
  have horder : lo < hi := by simp [lo, hi, Nat.cast_add]
  have hleft := cell_concentration j (m := lo) (by dsimp [lo]; linarith)
    (by dsimp [lo]; linarith)
  have hright := cell_concentration (j + 1) (m := hi) (by dsimp [hi]; linarith)
    (by dsimp [hi]; linarith)
  let C (a z : ℝ) := cdf (gammaMeasure a 1) z
  let B : ℝ := (j : ℝ) + 1
  have hB : 0 < B := by dsimp [B]; positivity
  have hbound : ∀ᶠ a in atTop, 0 < a ∧
      C a (B * (a / hi)) < w ∧ w < C a (B * (a / lo)) := by
    filter_upwards [eventually_gt_atTop (0 : ℝ), hleft.eventually (Ioi_mem_nhds hw.2),
      hright.eventually (Ioi_mem_nhds (show 1 - w < 1 by linarith [hw.1]))]
      with a ha hL hR
    change w < Discrete3DGamma.cellProb a (a / lo) j at hL
    change 1 - w < Discrete3DGamma.cellProb a (a / hi) (j+1) at hR
    simp only [Discrete3DGamma.cellProb, Nat.cast_succ] at hL hR
    refine ⟨ha, ?_, ?_⟩
    · dsimp [C, B]
      linarith [cdf_le_one (μ := gammaMeasure a 1) (((j : ℝ) + 1 + 1) * (a / hi))]
    · dsimp [C, B]
      linarith [cdf_nonneg (gammaMeasure a 1) ((j : ℝ) * (a / lo))]
  have hc (a : ℝ) (ha : 0 < a) : ContinuousOn (C a) (Ici 0) := by
    have h (z : ℝ) : ContinuousOn (C a) (Iic z) := by
      simp only [C, cdf_gammaMeasure_eq_integral ha one_pos]
      exact (gamma_integrable ha).integrableOn.continuousOn_Iic_primitive_Iic
    intro z _
    have ht := h (z + 1) z (show z ≤ z + 1 by linarith)
    exact (ht.continuousAt (Iic_mem_nhds (by linarith))).continuousWithinAt
  let good (a r : ℝ) := r ∈ Icc (a / hi) (a / lo) ∧ C a (B * r) = w
  have hex : ∀ᶠ a in atTop, 0 < a ∧ ∃ r, good a r := by
    filter_upwards [hbound] with a ⟨ha, hL, hR⟩
    refine ⟨ha, ?_⟩
    have haorder := div_le_div_of_nonneg_left ha.le hlo horder.le
    have hcont : ContinuousOn (fun r ↦ C a (B * r)) (Icc (a / hi) (a / lo)) :=
      (hc a ha).comp (by fun_prop)
        (fun r hr ↦ mul_nonneg hB.le ((div_pos ha hhi).le.trans hr.1))
    obtain ⟨r, hr, he⟩ := intermediate_value_Icc haorder hcont
      (show w ∈ Icc (C a (B * (a / hi))) (C a (B * (a / lo))) from ⟨hL.le, hR.le⟩)
    exact ⟨r, hr, he⟩
  classical
  let r (a : ℝ) := if h : ∃ r, good a r then Classical.choose h else 1
  have hr : ∀ᶠ a in atTop, 0 < a ∧ good a (r a) := by
    filter_upwards [hex] with a ⟨ha, hx⟩
    exact ⟨ha, by simpa [r, hx] using Classical.choose_spec hx⟩
  have hfa : Tendsto (fun a ↦ C a (j * r a)) atTop (𝓝 0) := by
    have ht := hleft.const_sub 1
    simp only [sub_self] at ht
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds ht
    · exact Eventually.of_forall (fun a ↦ cdf_nonneg _ _)
    · filter_upwards [hr] with a ⟨ha, hr, _⟩
      have hm := monotone_cdf (μ := gammaMeasure a 1)
        (mul_le_mul_of_nonneg_left hr.2 (Nat.cast_nonneg j : (0 : ℝ) ≤ j))
      unfold C
      unfold Discrete3DGamma.cellProb
      linarith [cdf_le_one (μ := gammaMeasure a 1) (((j : ℝ) + 1) * (a / lo))]
  have hfb : Tendsto (fun a ↦ C a ((B + 1) * r a)) atTop (𝓝 1) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' hright tendsto_const_nhds
    · filter_upwards [hr] with a ⟨ha, hr, _⟩
      have hm := monotone_cdf (μ := gammaMeasure a 1)
        (mul_le_mul_of_nonneg_left hr.1 (show 0 ≤ B + 1 by linarith))
      have hn := cdf_nonneg (gammaMeasure a 1) (B * (a / hi))
      simp only [C, B, Discrete3DGamma.cellProb, Nat.cast_succ] at hm hn ⊢
      linarith
    · exact Eventually.of_forall (fun a ↦ cdf_le_one (gammaMeasure a 1) _)
  have hmean : Tendsto (fun a ↦ a / r a) atTop (𝓝 B) := by
    apply tendsto_order.2
    constructor
    · intro t ht
      let m := (max t j + B) / 2
      have hjB : (j : ℝ) < B := by dsimp [B]; linarith
      have hmax : max t (j : ℝ) < B := max_lt ht hjB
      have hmt : t < m := by dsimp [m]; linarith [le_max_left t (j : ℝ)]
      have hmj : (j : ℝ) < m := by dsimp [m]; linarith [le_max_right t (j : ℝ)]
      have hmB : m < B := by dsimp [m]; linarith
      have hm0 : 0 < m := (Nat.cast_nonneg j).trans_lt hmj
      have hC := (cell_concentration j (m := m) hmj hmB).eventually (Ioi_mem_nhds hw.2)
      filter_upwards [hr, hC] with a ⟨ha, hR, hcut⟩ hcell
      have hr0 := (div_pos ha hhi).trans_le hR.1
      have hc' : w < C a (B * (a / m)) := by
        change w < Discrete3DGamma.cellProb a (a / m) j at hcell
        unfold Discrete3DGamma.cellProb at hcell
        dsimp [C, B]
        linarith [cdf_nonneg (gammaMeasure a 1) ((j : ℝ) * (a / m))]
      have hlt : r a < a / m := lt_of_not_ge fun hh ↦ hc'.not_ge
        ((monotone_cdf (μ := gammaMeasure a 1)
          (mul_le_mul_of_nonneg_left hh hB.le)).trans_eq hcut)
      have hprod := mul_lt_mul_of_pos_left hlt hm0
      have he : m * (a / m) = a := by field_simp
      rw [he] at hprod
      exact hmt.trans ((lt_div_iff₀ hr0).2 hprod)
    · intro t ht
      let m := (min t (B + 1) + B) / 2
      have hmin : B < min t (B + 1) := lt_min ht (by linarith)
      have hmB : B < m := by dsimp [m]; linarith
      have hmt : m < t := by dsimp [m]; linarith [min_le_left t (B + 1)]
      have hmU : m < B + 1 := by dsimp [m]; linarith [min_le_right t (B + 1)]
      have hm0 : 0 < m := hB.trans hmB
      have hjm : ((j + 1 : ℕ) : ℝ) < m := by simpa [B, Nat.cast_succ] using hmB
      have hmj : m < ((j + 1 : ℕ) : ℝ) + 1 := by simpa [B, Nat.cast_succ] using hmU
      have hC := (cell_concentration (j + 1) hjm hmj).eventually
        (Ioi_mem_nhds (show 1 - w < 1 by linarith [hw.1]))
      filter_upwards [hr, hC] with a ⟨ha, hR, hcut⟩ hcell
      have hr0 := (div_pos ha hhi).trans_le hR.1
      have hc' : C a (B * (a / m)) < w := by
        change 1 - w < Discrete3DGamma.cellProb a (a / m) (j + 1) at hcell
        simp only [Discrete3DGamma.cellProb, Nat.cast_succ] at hcell
        dsimp [C, B]
        linarith [cdf_le_one (gammaMeasure a 1) (((j : ℝ) + 1 + 1) * (a / m))]
      have hlt : a / m < r a := lt_of_not_ge fun hh ↦ hc'.not_ge
        (hcut.symm.trans_le (monotone_cdf (μ := gammaMeasure a 1)
          (mul_le_mul_of_nonneg_left hh hB.le)))
      have hprod := mul_lt_mul_of_pos_left hlt hm0
      have he : m * (a / m) = a := by field_simp
      rw [he] at hprod
      exact ((div_lt_iff₀ hr0).2 hprod).trans hmt
  refine ⟨r, hr.mono (fun a h ↦ (div_pos h.1 hhi).trans_le h.2.1.1), hmean, ?_, ?_⟩
  · have ht := hfa.const_sub w
    simp only [sub_zero] at ht
    apply ht.congr'
    filter_upwards [hr] with a ⟨_, _, h⟩
    unfold Discrete3DGamma.cellProb
    exact congrArg (fun k ↦ k - C a (j * r a)) h.symm
  · apply (hfb.sub_const w).congr'
    filter_upwards [hr] with a ⟨_, _, h⟩
    simp only [Discrete3DGamma.cellProb, Nat.cast_succ, C, B] at h ⊢
    rw [h]

private theorem cell_nonneg (a : ℝ) {r : ℝ} (hr : 0 < r) (j : ℕ) :
    0 ≤ Discrete3DGamma.cellProb a r j := by
  unfold Discrete3DGamma.cellProb
  exact sub_nonneg.mpr (monotone_cdf _ (by nlinarith))

private theorem saturated_bound {m n : ℕ} (hm : 0 < m) (hn : 0 < n)
    {p q : ℝ} (hp : 0 ≤ p) (hq : 0 ≤ q) (hs : p + q < 1) :
    let w := (m : ℝ) / (m + n)
    p ^ m * q ^ n < w ^ m * (1 - w) ^ n := by
  dsimp only
  let w := (m : ℝ) / (m + n)
  have hm' : 0 < (m : ℝ) := by exact_mod_cast hm
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hw : 0 < w := div_pos hm' (by positivity)
  have hq' : 0 < 1 - w := sub_pos.mpr ((div_lt_one (by positivity)).2 (by linarith))
  change _ < w ^ m * (1 - w) ^ n
  rcases hp.eq_or_lt with rfl | hp
  · simp [hm.ne']; positivity
  rcases hq.eq_or_lt with rfl | hq
  · simp [hn.ne']; positivity
  apply (log_lt_log_iff (by positivity) (by positivity)).mp
  rw [log_mul (pow_ne_zero _ hp.ne') (pow_ne_zero _ hq.ne'),
    log_mul (pow_ne_zero _ hw.ne') (pow_ne_zero _ hq'.ne'),
    log_pow, log_pow, log_pow, log_pow]
  have h1 := log_le_sub_one_of_pos (div_pos hp hw)
  have h2 := log_le_sub_one_of_pos (div_pos hq hq')
  rw [log_div hp.ne' hw.ne'] at h1
  rw [log_div hq.ne' hq'.ne'] at h2
  have hh1 : (m : ℝ) * (p / w - 1) = p * (m + n) - m := by
    dsimp [w]
    field_simp
  have hh2 : (n : ℝ) * (q / (1 - w) - 1) = q * (m + n) - n := by
    have he : 1 - w = (n : ℝ) / (m + n) := by dsimp [w]; field_simp; ring
    rw [he]
    field_simp
  have hneg := mul_neg_of_pos_of_neg (show 0 < (m : ℝ) + n by positivity)
    (show p + q - 1 < 0 by linarith)
  nlinarith only [hh1, hh2, hneg, mul_le_mul_of_nonneg_left h1 hm'.le,
    mul_le_mul_of_nonneg_left h2 hn'.le]

/-- The excluded saturated supremum for counts from two adjacent cells is
the product of the empirical cell proportions, with their multiplicities. -/
theorem adjacent_supremum {n : ℕ} (x : Fin n → ℕ) (j : ℕ)
    (hx : ∀ i, x i = j ∨ x i = j + 1)
    (hl : ∃ i, x i = j) (hu : ∃ i, x i = j + 1) :
    let m := (Finset.univ.filter fun i ↦ x i = j).card
    let k := (Finset.univ.filter fun i ↦ x i ≠ j).card
    let w := (m : ℝ) / (m + k)
    IsLUB {v | ∃ a > 0, ∃ r > 0, Discrete3DGamma.likelihood x a r = v}
      (w ^ m * (1 - w) ^ k) ∧
    ∀ a > 0, ∀ r > 0, Discrete3DGamma.likelihood x a r < w ^ m * (1 - w) ^ k := by
  dsimp only
  let m := (Finset.univ.filter fun i ↦ x i = j).card
  let k := (Finset.univ.filter fun i ↦ x i ≠ j).card
  let w := (m : ℝ) / (m + k)
  have hm : 0 < m := by
    obtain ⟨i, hi⟩ := hl
    exact Finset.card_pos.mpr ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hi⟩⟩
  have hk : 0 < k := by
    obtain ⟨i, hi⟩ := hu
    exact Finset.card_pos.mpr ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, by omega⟩⟩
  have hm' : 0 < (m : ℝ) := by exact_mod_cast hm
  have hk' : 0 < (k : ℝ) := by exact_mod_cast hk
  have hw : w ∈ Ioo (0 : ℝ) 1 :=
    ⟨div_pos hm' (by positivity), (div_lt_one (by positivity)).2 (by linarith)⟩
  have he (a r : ℝ) : Discrete3DGamma.likelihood x a r =
      Discrete3DGamma.cellProb a r j ^ m * Discrete3DGamma.cellProb a r (j + 1) ^ k := by
    unfold Discrete3DGamma.likelihood
    calc
      _ = ∏ i, if x i = j then Discrete3DGamma.cellProb a r j
          else Discrete3DGamma.cellProb a r (j + 1) := by
        apply Finset.prod_congr rfl
        intro i _
        rcases hx i with h | h
        · simp [h]
        · simp [h]
      _ = _ := by rw [Finset.prod_ite]; simp [m, k]
  have hlt (a : ℝ) (ha : 0 < a) (r : ℝ) (hr : 0 < r) :
      Discrete3DGamma.likelihood x a r < w ^ m * (1 - w) ^ k := by
    rw [he]
    exact saturated_bound hm hk (cell_nonneg a hr j) (cell_nonneg a hr (j + 1))
      (adjacent_mass_lt_one ha hr j)
  change IsLUB _ (w ^ m * (1 - w) ^ k) ∧ _
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨a, ha, r, hr, rfl⟩
    exact (hlt a ha r hr).le
  · intro L hL
    obtain ⟨r, hr, _, hLft, hRgt⟩ := adjacent_collapse_path j hw
    have ht : Tendsto (fun a ↦ Discrete3DGamma.likelihood x a (r a)) atTop
        (𝓝 (w ^ m * (1 - w) ^ k)) := by
      simp only [he]
      exact (hLft.pow m).mul (hRgt.pow k)
    apply le_of_tendsto ht
    filter_upwards [eventually_gt_atTop (0 : ℝ), hr] with a ha hra
    exact hL ⟨a, ha, r a, hra, rfl⟩

private theorem density_formula {a y : ℝ} (hy : 0 < y) :
    gammaPDFReal a 1 y = exp (-y) * y ^ (a - 1) / Gamma a := by
  simp [gammaPDFReal, hy.le]
  ring

private theorem cell_small_shape {a r : ℝ} (ha : 0 < a) (ha1 : a ≤ 1) (hr : 0 < r)
    {j : ℕ} (hj : 0 < j) :
    Discrete3DGamma.cellProb a r j ≤ a / ((j : ℝ) * Gamma (a + 1)) := by
  have hj' : 0 < (j : ℝ) := by exact_mod_cast hj
  have hg := Gamma_pos_of_pos ha
  have hG := Gamma_pos_of_pos (show 0 < a + 1 by positivity)
  let c := ((j : ℝ) * r * Gamma a)⁻¹
  rw [cell_integral ha hr]
  have hI : IntegrableOn (fun _ : ℝ ↦ c) (Ioc ((j : ℝ) * r) ((j + 1 : ℝ) * r)) :=
    integrableOn_const (by simp [Real.volume_Ioc])
  calc
    _ ≤ ∫ _ in Ioc ((j : ℝ) * r) ((j + 1 : ℝ) * r), c := by
      apply setIntegral_mono_on (gamma_integrable ha).integrableOn hI measurableSet_Ioc
        (fun y hy ↦ ?_)
      have hpos := (mul_pos hj' hr).trans hy.1
      have hyP : y ^ a ≤ 1 + y := by
        rcases le_or_gt y 1 with h | h
        · linarith [rpow_le_one hpos.le h ha.le]
        · linarith [rpow_le_self_of_one_le h.le ha1]
      have hYe : y ^ a * exp (-y) ≤ 1 := by
        rw [exp_neg, ← div_eq_mul_inv, div_le_one (exp_pos y)]
        exact hyP.trans (by linarith [add_one_le_exp y])
      have he : exp (-y) * y ^ (a - 1) / Gamma a =
          (y ^ a * exp (-y)) / (y * Gamma a) := by
        rw [rpow_sub hpos, rpow_one]
        ring
      rw [density_formula hpos, he]
      calc
        _ ≤ 1 / (y * Gamma a) := div_le_div_of_nonneg_right hYe (by positivity)
        _ ≤ c := by
          rw [one_div]
          exact inv_anti₀ (by positivity) (mul_le_mul_of_nonneg_right hy.1.le hg.le)
    _ = _ := by
      rw [integral_const, measureReal_def]
      simp only [Measure.restrict_apply_univ, Real.volume_Ioc, smul_eq_mul]
      rw [show (j + 1 : ℝ) * r - j * r = r by ring, ENNReal.toReal_ofReal hr.le,
        Real.Gamma_add_one ha.ne']
      dsimp [c]
      field_simp

private theorem cell_variance_bound {a r e : ℝ} (ha : 0 < a) (hr : 0 < r) (he : 0 < e)
    (j : ℕ) (h : ∀ y ∈ Ioc ((j : ℝ) * r) ((j + 1 : ℝ) * r), (e * a)^2 ≤ (y - a)^2) :
    Discrete3DGamma.cellProb a r j ≤ (e^2 * a)⁻¹ := by
  rw [cell_integral ha hr]
  let S := Ioc ((j : ℝ) * r) ((j + 1 : ℝ) * r)
  let f := fun y ↦ (y - a)^2 * gammaPDFReal a 1 y
  have hf := central_square ha
  have hmon : (∫ y in S, gammaPDFReal a 1 y) ≤ ∫ y in S, ((e * a)^2)⁻¹ * f y := by
    apply setIntegral_mono_on (gamma_integrable ha).integrableOn (hf.1.const_mul _).integrableOn
      measurableSet_Ioc
    intro y hy
    apply (le_inv_mul_iff₀ (by positivity : 0 < (e * a)^2)).2
    exact mul_le_mul_of_nonneg_right (h y hy) (gammaPDFReal_nonneg ha one_pos y)
  have hfm : (∫ y in S, f y) ≤ a := by
    rw [← hf.2]
    exact setIntegral_le_integral hf.1 (ae_of_all _ fun y ↦ by
      dsimp [f]; positivity [gammaPDFReal_nonneg ha one_pos y])
  rw [integral_const_mul] at hmon
  calc
    _ ≤ ((e * a)^2)⁻¹ * a := hmon.trans (mul_le_mul_of_nonneg_left hfm (by positivity))
    _ = _ := by field_simp

private theorem cell_pair_large_shape {a r : ℝ} (ha : 0 < a) (hr : 0 < r)
    {j k : ℕ} (hjk : j + 1 < k) :
    let e := ((k : ℝ) - (j + 1)) / (k + (j + 1))
    min (Discrete3DGamma.cellProb a r j) (Discrete3DGamma.cellProb a r k) ≤ (e^2 * a)⁻¹ := by
  dsimp only
  have hjk' : (j : ℝ) + 1 < k := by exact_mod_cast hjk
  let e := ((k : ℝ) - (j + 1)) / (k + (j + 1))
  have he : 0 < e := div_pos (sub_pos.mpr hjk') (by positivity)
  change _ ≤ (e^2 * a)⁻¹
  have hsplit : ((j : ℝ) + 1) * r ≤ (1 - e) * a ∨ (1 + e) * a ≤ k * r := by
    by_contra! hn
    have hratio : (k : ℝ) * (1 - e) = (j + 1) * (1 + e) := by
      dsimp [e]
      field_simp
      ring
    have hpos : (0 : ℝ) < k := (by positivity : 0 < (j : ℝ) + 1).trans hjk'
    nlinarith only [mul_lt_mul_of_pos_left hn.1 hpos,
      mul_lt_mul_of_pos_left hn.2 (show 0 < (j : ℝ) + 1 by positivity),
      congrArg (fun z : ℝ ↦ z * a) hratio]
  rcases hsplit with hj | hk
  · apply (min_le_left _ _).trans (cell_variance_bound ha hr he j ?_)
    intro y hy
    have h : e * a ≤ a - y := by linarith only [hy.2, hj]
    calc
      _ ≤ (a-y)^2 := pow_le_pow_left₀ (mul_pos he ha).le h 2
      _ = _ := by ring
  · apply (min_le_right _ _).trans (cell_variance_bound ha hr he k ?_)
    intro y hy
    have h : e * a ≤ y - a := by linarith only [hy.1, hk]
    exact pow_le_pow_left₀ (mul_pos he ha).le h 2

private theorem gamma_envelope {l h : ℝ} (hl : 0 < l) (hlh : l ≤ h) :
    ∃ C > 0, ∀ a ∈ Icc l h, ∀ᵐ y ∂(volume : Measure ℝ),
      gammaPDFReal a 1 y ≤ C * (gammaPDFReal l 1 y + gammaPDFReal h 1 y) := by
  have hcont : ContinuousOn Gamma (Icc l h) :=
    differentiableOn_Gamma_Ioi.continuousOn.mono (fun _ hx ↦ hl.trans_le hx.1)
  obtain ⟨a₀, h₀, hm⟩ := isCompact_Icc.exists_isMinOn ⟨l, le_rfl, hlh⟩ hcont
  have hmin := Gamma_pos_of_pos (hl.trans_le h₀.1)
  have hhigh := hl.trans_le hlh
  let C := (Gamma l + Gamma h) / Gamma a₀
  refine ⟨C, div_pos (add_pos (Gamma_pos_of_pos hl) (Gamma_pos_of_pos hhigh)) hmin, ?_⟩
  intro a ha
  have ha0 := hl.trans_le ha.1
  have hga := Gamma_pos_of_pos ha0
  have haΓ : Gamma a₀ ≤ Gamma a := isMinOn_iff.mp hm _ ha
  have hzero : ∀ᵐ y ∂(volume : Measure ℝ), y ≠ 0 := by simp [ae_iff, measure_singleton]
  filter_upwards [hzero] with y hy
  rcases lt_or_gt_of_ne hy with hy | hy
  · simp [gammaPDFReal, not_le.mpr hy]
  have hbound {s : ℝ} (hs : 0 < s) (hle : y ^ (a - 1) ≤ y ^ (s - 1))
      (hG : Gamma s ≤ Gamma l + Gamma h) :
      gammaPDFReal a 1 y ≤ C * gammaPDFReal s 1 y := by
    rw [density_formula hy, density_formula hy]
    calc
      _ ≤ exp (-y) * y ^ (s - 1) / Gamma a₀ :=
        div_le_div₀ (by positivity) (mul_le_mul_of_nonneg_left hle (exp_pos _).le) hmin haΓ
      _ ≤ _ := by
        dsimp [C]
        have hgs := Gamma_pos_of_pos hs
        field_simp [hmin, hgs]
        gcongr
  rcases le_or_gt y 1 with hy1 | hy1
  · have hL := hbound hl (rpow_le_rpow_of_exponent_ge hy hy1 (by linarith [ha.1]))
      (by linarith [Gamma_pos_of_pos hhigh])
    exact hL.trans (mul_le_mul_of_nonneg_left
      (le_add_of_nonneg_right (gammaPDFReal_nonneg hhigh one_pos y)) (by dsimp [C]; positivity))
  · have hH := hbound hhigh (rpow_le_rpow_of_exponent_le hy1.le (by linarith [ha.2]))
      (by linarith [Gamma_pos_of_pos hl])
    exact hH.trans (mul_le_mul_of_nonneg_left
      (le_add_of_nonneg_left (gammaPDFReal_nonneg hl one_pos y)) (by dsimp [C]; positivity))

private theorem cdf_joint_continuous {p : ℝ × ℝ} (ha : 0 < p.1) :
    ContinuousAt (fun q : ℝ × ℝ ↦ cdf (gammaMeasure q.1 1) q.2) p := by
  obtain ⟨C, _, hC⟩ := gamma_envelope (show 0 < p.1 / 2 by positivity)
    (show p.1 / 2 ≤ 2 * p.1 by linarith)
  have hpar : ∀ᶠ q : ℝ × ℝ in 𝓝 p, q.1 ∈ Icc (p.1 / 2) (2 * p.1) :=
    continuous_fst.continuousAt (Icc_mem_nhds (by linarith) (by linarith))
  let F (q : ℝ × ℝ) := (Iic q.2).indicator (gammaPDFReal q.1 1)
  have hEq : (fun q : ℝ × ℝ ↦ ∫ y, F q y) =ᶠ[𝓝 p]
      (fun q : ℝ × ℝ ↦ cdf (gammaMeasure q.1 1) q.2) := by
    filter_upwards [hpar] with q hq
    dsimp [F]
    rw [integral_indicator measurableSet_Iic,
      cdf_gammaMeasure_eq_integral ((by positivity : 0 < p.1 / 2).trans_le hq.1) one_pos]
  have hE : (∫ y, F p y) = cdf (gammaMeasure p.1 1) p.2 := by
    dsimp [F]
    rw [integral_indicator measurableSet_Iic, cdf_gammaMeasure_eq_integral ha one_pos]
  change Tendsto _ (𝓝 p) (𝓝 (cdf (gammaMeasure p.1 1) p.2))
  rw [← hE]
  apply Tendsto.congr' hEq
  apply tendsto_integral_filter_of_dominated_convergence
    (fun y ↦ C * (gammaPDFReal (p.1 / 2) 1 y + gammaPDFReal (2 * p.1) 1 y))
  · exact Eventually.of_forall (fun q ↦ ((measurable_gammaPDFReal q.1 1).indicator
      measurableSet_Iic).aestronglyMeasurable)
  · filter_upwards [hpar] with q hq
    have hq' := (show 0 < p.1 / 2 by positivity).trans_le hq.1
    filter_upwards [hC q.1 hq] with y hy
    have hn := gammaPDFReal_nonneg hq' one_pos y
    by_cases h : y ≤ q.2
    · simpa [F, h, Real.norm_eq_abs, abs_of_nonneg hn] using hy
    · have hle : ‖F q y‖ ≤ gammaPDFReal q.1 1 y := by simpa [F, h] using hn
      exact hle.trans hy
  · exact ((gamma_integrable (by positivity : 0 < p.1 / 2)).add
      (gamma_integrable (by positivity : 0 < 2 * p.1))).const_mul C
  · have hy₀ : ∀ᵐ y ∂(volume : Measure ℝ), y ≠ 0 := by simp [ae_iff, measure_singleton]
    have hyp : ∀ᵐ y ∂(volume : Measure ℝ), y ≠ p.2 := by simp [ae_iff, measure_singleton]
    filter_upwards [hy₀, hyp] with y hy hpy
    have hpd : ContinuousAt (fun q : ℝ × ℝ ↦ gammaPDFReal q.1 1 y) p := by
      rcases lt_or_gt_of_ne hy with hy | hy
      · simp [gammaPDFReal, not_le.mpr hy, continuousAt_const]
      · simp_rw [density_formula hy]
        have hs : ContinuousAt (fun q : ℝ × ℝ ↦ y ^ (q.1 - 1)) p :=
          (continuousAt_const_rpow hy.ne').comp (continuous_fst.continuousAt.sub_const 1)
        exact (hs.const_mul (exp (-y))).div
          ((hasDerivAt_Gamma_of_pos ha).continuousAt.comp continuous_fst.continuousAt)
          (Gamma_pos_of_pos ha).ne'
    rcases lt_or_gt_of_ne hpy with h | h
    · have hev : ∀ᶠ q : ℝ × ℝ in 𝓝 p, y < q.2 :=
        continuous_snd.continuousAt (Ioi_mem_nhds h)
      have he : F p y = gammaPDFReal p.1 1 y := by simp [F, h.le]
      rw [he]
      exact hpd.tendsto.congr' (hev.mono (fun q hq ↦ by simp [F, hq.le]))
    · have hev : ∀ᶠ q : ℝ × ℝ in 𝓝 p, q.2 < y :=
        continuous_snd.continuousAt (Iio_mem_nhds h)
      have he : F p y = 0 := by simp [F, h.not_ge]
      rw [he]
      exact tendsto_const_nhds.congr' (hev.mono (fun q hq ↦ by simp [F, hq.not_ge]))

private theorem cdf_envelope {l h : ℝ} (hl : 0 < l) (hlh : l ≤ h) :
    ∃ C > 0, ∀ a ∈ Icc l h, ∀ z,
      cdf (gammaMeasure a 1) z ≤ C *
        (cdf (gammaMeasure l 1) z + cdf (gammaMeasure h 1) z) ∧
      1 - cdf (gammaMeasure a 1) z ≤ C *
        ((1 - cdf (gammaMeasure l 1) z) + (1 - cdf (gammaMeasure h 1) z)) := by
  obtain ⟨C, hC, hbound⟩ := gamma_envelope hl hlh
  refine ⟨C, hC, ?_⟩
  intro a ha z
  have ha0 := hl.trans_le ha.1
  have hh := hl.trans_le hlh
  have hc (a : ℝ) (ha : 0 < a) :
      1 - cdf (gammaMeasure a 1) z = ∫ y in (Iic z)ᶜ, gammaPDFReal a 1 y := by
    have h := integral_add_compl (s := Iic z) measurableSet_Iic (gamma_integrable ha)
    rw [gamma_integral ha, ← cdf_gammaMeasure_eq_integral ha one_pos] at h
    linarith
  rw [hc a ha0, hc l hl, hc h hh, cdf_gammaMeasure_eq_integral ha0 one_pos,
    cdf_gammaMeasure_eq_integral hl one_pos, cdf_gammaMeasure_eq_integral hh one_pos]
  have hs (S : Set ℝ) :
      (∫ y in S, gammaPDFReal a 1 y) ≤ C *
        ((∫ y in S, gammaPDFReal l 1 y) + ∫ y in S, gammaPDFReal h 1 y) := by
    rw [← integral_add (gamma_integrable hl).integrableOn (gamma_integrable hh).integrableOn,
      ← integral_const_mul]
    exact integral_mono_ae (gamma_integrable ha0).integrableOn
      (((gamma_integrable hl).add (gamma_integrable hh)).const_mul C).integrableOn
      (ae_restrict_of_ae (hbound a ha))
  exact ⟨hs _, hs _⟩

private theorem cell_positive {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (j : ℕ) :
    0 < Discrete3DGamma.cellProb a r j := by
  rw [cell_integral ha hr, integral_Ioc_eq_integral_Ioo]
  rw [setIntegral_pos_iff_support_of_nonneg_ae
    (ae_restrict_of_ae (ae_of_all _ (gammaPDFReal_nonneg ha one_pos)))
    (gamma_integrable ha).integrableOn]
  have hsub : Ioo ((j : ℝ) * r) ((j + 1 : ℝ) * r) ⊆
      Function.support (gammaPDFReal a 1) ∩ Ioo ((j : ℝ) * r) ((j + 1 : ℝ) * r) :=
    fun y hy ↦ ⟨(gammaPDFReal_pos ha one_pos ((by positivity : 0 ≤ (j : ℝ) * r).trans_lt
      hy.1)).ne', hy⟩
  exact lt_of_lt_of_le (by simp [Real.volume_Ioo, show (j + 1 : ℝ) * r - j * r = r by ring,
    ENNReal.ofReal_pos.mpr hr]) (measure_mono hsub)

private theorem likelihood_le_cell {n : ℕ} (x : Fin n → ℕ) {a r : ℝ} (hr : 0 < r)
    (i : Fin n) :
    Discrete3DGamma.likelihood x a r ≤ Discrete3DGamma.cellProb a r (x i) := by
  have hle (j : Fin n) : Discrete3DGamma.cellProb a r (x j) ≤ 1 := by
    unfold Discrete3DGamma.cellProb
    linarith [cdf_le_one (gammaMeasure a 1) (((x j : ℝ) + 1) * r),
      cdf_nonneg (gammaMeasure a 1) ((x j : ℝ) * r)]
  unfold Discrete3DGamma.likelihood
  classical
  rw [← Finset.mul_prod_erase _ _ (Finset.mem_univ i)]
  apply mul_le_of_le_one_right (cell_nonneg a hr _)
  exact Finset.prod_le_one₀ (fun j _ ↦ cell_nonneg a hr (x j)) (fun j _ ↦ hle j)

private def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

private theorem likelihood_continuous {n : ℕ} (x : Fin n → ℕ) :
    ContinuousOn (fun p : ℝ × ℝ ↦ Discrete3DGamma.likelihood x p.1 p.2) domain := by
  apply continuousOn_finsetProd
  intro i _ p hp
  let f (y : ℝ) (q : ℝ × ℝ) : ℝ × ℝ := (q.1, y * q.2)
  have h (y : ℝ) : ContinuousAt (f y) p := by
    dsimp [f]
    fun_prop
  have hC (y : ℝ) : ContinuousAt (fun q : ℝ × ℝ ↦
      cdf (gammaMeasure (f y q).1 1) (f y q).2) p :=
    (cdf_joint_continuous (p := f y p) (show 0 < p.1 from hp.1)).comp (h y)
  exact ((hC ((x i : ℝ) + 1)).sub (hC (x i))).continuousWithinAt

private theorem cdf_rate_zero {a c : ℝ} (ha : 0 < a) :
    Tendsto (fun r : ℝ ↦ cdf (gammaMeasure a 1) (c * r)) (𝓝[>] 0) (𝓝 0) := by
  have h : ContinuousAt (fun r : ℝ ↦ (a, c * r)) 0 := by fun_prop
  have hcont : ContinuousAt (fun r : ℝ ↦ cdf (gammaMeasure a 1) (c * r)) 0 :=
    (cdf_joint_continuous (p := (a, c * 0)) ha).comp (f := fun r : ℝ ↦ (a, c * r)) h
  simpa [Function.comp_def, cdf_zero ha] using hcont.tendsto.mono_left
    (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))

/-- Two occupied cells separated by at least one cell force a compact
positive-likelihood superlevel, yielding a finite joint maximizer. -/
theorem separated_attains {n : ℕ} {x : Fin n → ℕ} {i j : Fin n} (hij : x i + 1 < x j) :
    ∃ a > 0, ∃ r > 0, ∀ b > 0, ∀ t > 0,
      Discrete3DGamma.likelihood x b t ≤ Discrete3DGamma.likelihood x a r := by
  let f (p : ℝ × ℝ) := Discrete3DGamma.likelihood x p.1 p.2
  have hpos : 0 < f (1, 1) := Finset.prod_pos (fun i _ ↦ cell_positive one_pos one_pos (x i))
  let W := f (1, 1) / 2
  have hW : 0 < W := by dsimp [W]; linarith
  have hW' : W ≤ f (1, 1) := by dsimp [W]; linarith
  have hj : 0 < x j := by omega
  have hj' : (0 : ℝ) < x j := by exact_mod_cast hj
  have hsmall : Tendsto (fun a : ℝ ↦ a / ((x j : ℝ) * Gamma (a + 1))) (𝓝[>] 0) (𝓝 0) := by
    have ht : Tendsto (fun a : ℝ ↦ a) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
    have hg := (hasDerivAt_Gamma_of_pos (s := 1) one_pos).continuousAt.tendsto.comp
      (show Tendsto (fun a : ℝ ↦ a + 1) (𝓝[>] 0) (𝓝 1) from by simpa using ht.add_const 1)
    simp only [Gamma_one] at hg
    simpa [Function.comp_def, Pi.div_def] using ht.div (hg.const_mul (x j : ℝ))
      (mul_ne_zero hj'.ne' one_ne_zero)
  obtain ⟨L, hL0, hLs⟩ := (mem_nhdsGT_iff_exists_Ioo_subset).mp
    (hsmall.eventually (Iio_mem_nhds hW))
  let l := min L 1
  have hl : 0 < l := lt_min hL0 zero_lt_one
  let e := ((x j : ℝ) - (x i + 1)) / (x j + (x i + 1))
  have he : 0 < e := by
    dsimp [e]
    apply div_pos _ (by positivity)
    apply sub_pos.mpr
    exact_mod_cast hij
  let H := max 1 ((e^2 * W)⁻¹)
  have hlH : l ≤ H := (min_le_right _ _).trans (le_max_left _ _)
  have hshape {p : ℝ × ℝ} (hp : p ∈ domain) (hf : W ≤ f p) : p.1 ∈ Icc l H := by
    have ha : 0 < p.1 := hp.1
    have hr : 0 < p.2 := hp.2
    have hi' := hf.trans (likelihood_le_cell x hr i)
    have hj'' := hf.trans (likelihood_le_cell x hr j)
    constructor
    · by_contra! h
      have hs := hLs (show p.1 ∈ Ioo 0 L from ⟨ha, h.trans_le (min_le_left _ _)⟩)
      exact (not_lt_of_ge (hj''.trans (cell_small_shape ha
        (h.le.trans (min_le_right _ _)) hr hj))) hs
    · have hab : W ≤ (e^2 * p.1)⁻¹ :=
        (le_min hi' hj'').trans (cell_pair_large_shape ha hr hij)
      have hab' : W * (e^2 * p.1) ≤ 1 := by
        apply (le_inv_mul_iff₀' (by positivity : 0 < e^2 * p.1)).mp
        simpa using hab
      apply le_max_of_le_right
      suffices hp : p.1 ≤ (e^2 * W)⁻¹ * 1 by simpa using hp
      apply (le_inv_mul_iff₀' (by positivity)).2
      linarith only [hab', show p.1 * (e^2 * W) = W * (e^2 * p.1) by ring]
  obtain ⟨C, hC, hc⟩ := cdf_envelope hl hlH
  let s : ℝ := (x j : ℝ) + 1
  have h0 : Tendsto (fun r : ℝ ↦ C *
      (cdf (gammaMeasure l 1) (s * r) + cdf (gammaMeasure H 1) (s * r))) (𝓝[>] 0) (𝓝 0) := by
    simpa using ((cdf_rate_zero (c := s) hl).add
      (cdf_rate_zero (c := s) (hl.trans_le hlH))).const_mul C
  obtain ⟨lo, hlo, hlos⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp
    (h0.eventually (Iio_mem_nhds hW))
  let t : ℝ := x j
  have hTail (a : ℝ) : Tendsto (fun r : ℝ ↦ 1 - cdf (gammaMeasure a 1) (t * r)) atTop (𝓝 0) := by
    simpa [Function.comp_def] using
      ((tendsto_cdf_atTop (gammaMeasure a 1)).comp
        (tendsto_id.const_mul_atTop (show 0 < t from hj'))).const_sub 1
  have htop := ((hTail l).add (hTail H)).const_mul C
  simp only [add_zero, mul_zero] at htop
  obtain ⟨hi, hhis⟩ := eventually_atTop.mp (htop.eventually (Iio_mem_nhds hW))
  let K : Set (ℝ × ℝ) := Icc l H ×ˢ Icc lo (max hi 1)
  have hKD : K ⊆ domain := fun _ h ↦ ⟨hl.trans_le h.1.1, hlo.trans_le h.2.1⟩
  have hlevel {p : ℝ × ℝ} (hp : p ∈ domain) (hf : W ≤ f p) : p ∈ K := by
    have hr : 0 < p.2 := hp.2
    have hs := hshape hp hf
    have hL : W ≤ Discrete3DGamma.cellProb p.1 p.2 (x j) :=
      hf.trans (likelihood_le_cell x hr j)
    refine ⟨hs, ?_, ?_⟩
    · by_contra! h
      have hbad := hlos (show p.2 ∈ Ioo 0 lo from ⟨hr, h⟩)
      have hU := (hc p.1 hs (s * p.2)).1
      change _ < W at hbad
      unfold Discrete3DGamma.cellProb at hL
      dsimp [s] at hU hbad
      linarith [cdf_nonneg (gammaMeasure p.1 1) (t * p.2)]
    · by_contra! h
      have hbad := hhis p.2 ((le_max_left _ _).trans h.le)
      have hU := (hc p.1 hs (t * p.2)).2
      change _ < W at hbad
      unfold Discrete3DGamma.cellProb at hL
      dsimp [t] at hU hbad
      linarith [cdf_le_one (gammaMeasure p.1 1) (s * p.2)]
  have h1 : (1, 1) ∈ K := hlevel ⟨one_pos, one_pos⟩ hW'
  obtain ⟨p, hp, hpmax⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨_, h1⟩
    ((likelihood_continuous x).mono hKD)
  have hpD := hKD hp
  refine ⟨p.1, hpD.1, p.2, hpD.2, ?_⟩
  intro a ha r hr
  have hdom : (a, r) ∈ domain := ⟨ha, hr⟩
  by_cases h : W ≤ f (a, r)
  · exact isMaxOn_iff.mp hpmax (a, r) (hlevel hdom h)
  · exact (lt_of_not_ge h).le.trans (hW'.trans (isMaxOn_iff.mp hpmax (1, 1) h1))

/-- A nonempty count sample has a finite all-free fit exactly when its
integer range is at least two. Zero counts and multiplicities are retained. -/
theorem all_free_exists_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    (∃ a > 0, ∃ r > 0, ∀ b > 0, ∀ t > 0,
      Discrete3DGamma.likelihood x b t ≤ Discrete3DGamma.likelihood x a r) ↔
      ∃ i j, x i + 1 < x j := by
  constructor
  · rintro ⟨a, ha, r, hr, hm⟩
    by_contra! h
    obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ x
      ⟨⟨0, hn⟩, Finset.mem_univ _⟩
    have hcells (j : Fin n) : x j = x i ∨ x j = x i + 1 := by
      have hlow := hi j (Finset.mem_univ _)
      have hhigh := h i j
      omega
    have hupper : Discrete3DGamma.likelihood x a r ∈ upperBounds
        {v | ∃ b > 0, ∃ t > 0, Discrete3DGamma.likelihood x b t = v} := by
      rintro _ ⟨b, hb, t, ht, rfl⟩
      exact hm b hb t ht
    by_cases ht : ∃ j, x j = x i + 1
    · obtain ⟨hL, hstrict⟩ := adjacent_supremum x (x i) hcells ⟨i, rfl⟩ ht
      exact (hstrict a ha r hr).not_ge (hL.2 hupper)
    · obtain ⟨v, he⟩ : ∃ v : ℕ, x = fun _ ↦ v :=
        ⟨x i, funext fun j ↦ (hcells j).resolve_right (fun hj ↦ ht ⟨j, hj⟩)⟩
      subst x
      obtain ⟨hL, hstrict⟩ := constant_supremum hn v
      exact (hstrict a ha r hr).not_ge (hL.2 hupper)
  · rintro ⟨i, j, h⟩
    exact separated_attains h

end
end UnivariateML.AllFreeDGamma

end
