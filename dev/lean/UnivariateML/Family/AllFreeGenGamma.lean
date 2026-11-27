module

public import UnivariateML.Family.GenGammaTwoPoint
public import UnivariateML.Generic.Escape

set_option linter.style.header false

/-! Generalized-gamma concentration at the common observation, with location
at the common log observation and any fixed real shape. -/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.AllFreeGenGamma
noncomputable section

/-- With location equal to the common log observation, taking scale to zero
makes log likelihood unbounded for every fixed shape, including zero. -/
theorem constant_concentration {n : ℕ} (hn : 0 < n) (μ Q : ℝ)
    {x : Fin n → ℝ} (hx : ∀ i, x i = exp μ) :
    Tendsto (fun σ : ℝ ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, Q))
      (𝓝[>] 0) atTop := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hlog : Tendsto (fun σ : ℝ ↦ -log σ) (𝓝[>] 0) atTop :=
    tendsto_neg_atBot_atTop.comp tendsto_log_nhdsGT_zero
  have hd (σ : ℝ) (hσ : 0 < σ) :
      GenGammaTwoPoint.logDensity μ σ Q μ =
        -log σ + GenGammaTwoPoint.logDensity μ 1 Q μ := by
    by_cases hQ : Q = 0
    · simp [GenGammaTwoPoint.logDensity, hQ, sub_eq_add_neg]
    · have h := abs_ne_zero.mpr hQ
      simp [GenGammaTwoPoint.logDensity, hQ, log_div h hσ.ne']
      ring
  have hlim := tendsto_atTop_add_const_right _ (GenGammaTwoPoint.logDensity μ 1 Q μ - μ) hlog
  apply hlim.congr'
  filter_upwards [self_mem_nhdsWithin] with σ (hσ : 0 < σ)
  simp [GenGammaTwoPoint.sampleLogLik, hx, hd σ hσ, nsmul_eq_mul]
  field_simp
  ring

/-- For either orientation, the fixed-location path tends to the power or
Pareto log likelihood when all observations are on the supported side.
At zero residual the exponential loss still vanishes. -/
theorem held_location_face_path {n : ℕ} {x : Fin n → ℝ} {μ d : ℝ}
    (hd : 0 < d) (ε : ℝ) (hε : |ε| = 1)
    (hx : ∀ i, ε * (log (x i) - μ) ≤ 0) :
    Tendsto (fun q : ℝ ↦ GenGammaTwoPoint.sampleLogLik x
      (μ, d * sqrt q, ε / sqrt q)) (𝓝[>] 0)
      (𝓝 ((∑ i, (-log d + ε * (log (x i) - μ) / d - log (x i))) / n)) := by
  let y : Fin n → ℝ := fun i ↦ ε * (log (x i) - μ) / d
  have h0 : Tendsto (fun q : ℝ ↦ q) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hqlog : Tendsto (fun q : ℝ ↦ q * log q) (𝓝[>] 0) (𝓝 0) := by
    simpa [mul_comm] using (tendsto_log_mul_rpow_nhdsGT_zero (r := 1) one_pos)
  have hg : Tendsto (fun q : ℝ ↦ log (Real.Gamma (q + 1))) (𝓝[>] 0) (𝓝 0) := by
    have ha := h0.add_const 1
    simp only [zero_add] at ha
    have hc := (Real.hasDerivAt_Gamma_of_pos (s := 1) one_pos).continuousAt.tendsto.comp ha
    simpa [Function.comp_def] using hc.log (by simp)
  unfold GenGammaTwoPoint.sampleLogLik
  apply Tendsto.div_const
  apply tendsto_finsetSum
  intro i _
  have hsmall : Tendsto (fun q : ℝ ↦ q * exp (y i / q)) (𝓝[>] 0) (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds h0
    · filter_upwards [self_mem_nhdsWithin] with q (hq : 0 < q)
      positivity
    · filter_upwards [self_mem_nhdsWithin] with q (hq : 0 < q)
      have hy : y i ≤ 0 := div_nonpos_of_nonpos_of_nonneg (hx i) hd.le
      simpa using mul_le_mul_of_nonneg_left
        (exp_le_one_iff.mpr (div_nonpos_of_nonpos_of_nonneg hy hq.le)) hq.le
  have ht := ((hqlog.sub hg).sub hsmall).const_add (-log d + y i - log (x i))
  simp only [sub_zero, add_zero] at ht
  refine ht.congr' ?_
  filter_upwards [self_mem_nhdsWithin] with q (hq : 0 < q)
  have hs := sqrt_pos.mpr hq
  have habs : |ε / sqrt q| = (sqrt q)⁻¹ := by
    rw [abs_div, hε, abs_of_pos hs, one_div]
  have hε0 : ε ≠ 0 := by intro h; simp [h] at hε
  have hε2 : ε ^ 2 = 1 := by rw [← sq_abs, hε, one_pow]
  have hk : 1 / (ε / sqrt q) ^ 2 = q := by
    rw [div_pow, hε2, sq_sqrt hq.le, div_div_eq_mul_div]
    simp
  have hsprod : sqrt q * (d * sqrt q) = d * q := by
    calc
      _ = d * (sqrt q)^2 := by ring
      _ = _ := by rw [sq_sqrt hq.le]
  have hw : ε / sqrt q / (d * sqrt q) * (log (x i) - μ) = y i / q := by
    dsimp [y]
    rw [div_div, hsprod]
    ring
  have hratio : |ε / sqrt q| / (d * sqrt q) = (d * q)⁻¹ := by
    rw [habs, inv_eq_one_div, div_div,
      hsprod, one_div]
  have hnormal : log (Real.Gamma (q + 1)) = log q + log (Real.Gamma q) := by
    rw [Real.Gamma_add_one hq.ne', log_mul hq.ne' (Gamma_pos_of_pos hq).ne']
  simp only [GenGammaTwoPoint.logDensity, div_ne_zero hε0 hs.ne', ite_false, hk, hw,
    hratio, log_inv, log_mul hd.ne' hq.ne', hnormal]
  field_simp
  ring

private def loss (Q z : ℝ) : ℝ :=
  if Q = 0 then z^2 / 2
  else (1 / Q^2) * (exp (Q * z) - 1 - Q * z)

private theorem loss_convex (Q : ℝ) : ConvexOn ℝ univ (loss Q) := by
  refine ⟨convex_univ, ?_⟩
  intro x _ y _ a b ha hb hab
  change loss Q (a * x + b * y) ≤ a * loss Q x + b * loss Q y
  by_cases h : Q = 0
  · simp only [loss, ite_eq_left h]
    have he : a * x^2 / 2 + b * y^2 / 2 - (a*x+b*y)^2/2 =
        a*b*(x-y)^2/2 := by
      linear_combination -(a*x^2 + b*y^2)/2 * hab
    nlinarith only [he, show 0 ≤ a*b*(x-y)^2/2 by positivity]
  · let k := 1 / Q^2
    have hk : 0 ≤ k := by dsimp [k]; positivity
    have hE := convexOn_exp.2 (mem_univ (Q*x)) (mem_univ (Q*y)) ha hb hab
    change exp (a*(Q*x)+b*(Q*y)) ≤ a*exp (Q*x)+b*exp (Q*y) at hE
    simp only [loss, ite_eq_right h]
    change k * (exp (Q*(a*x+b*y)) - 1 - Q*(a*x+b*y)) ≤
      a * (k*(exp (Q*x)-1-Q*x)) + b * (k*(exp (Q*y)-1-Q*y))
    rw [show Q*(a*x+b*y) = a*(Q*x)+b*(Q*y) by ring]
    nlinarith only [mul_le_mul_of_nonneg_left hE hk,
      congrArg (fun t : ℝ ↦ k*t) hab]

private theorem loss_nonneg (Q z : ℝ) : 0 ≤ loss Q z := by
  by_cases h : Q = 0
  · simp only [loss, ite_eq_left h]
    positivity
  · rw [loss, ite_eq_right h]
    exact mul_nonneg (by positivity) (by linarith [add_one_le_exp (Q*z)])

private theorem loss_linear_minorant (Q : ℝ) :
    ∃ d > 0, ∃ C : ℝ, ∀ z, d * |z| - C ≤ loss Q z := by
  by_cases h : Q = 0
  · refine ⟨1, one_pos, 1, fun z ↦ ?_⟩
    rw [loss, ite_eq_left h, one_mul]
    rcases le_total 0 z with hz | hz
    · rw [abs_of_nonneg hz]
      nlinarith [sq_nonneg (z-1)]
    · rw [abs_of_nonpos hz]
      nlinarith [sq_nonneg (z+1)]
  · let k := 1 / Q^2
    have hk : 0 < k := by dsimp [k]; positivity [sq_pos_of_ne_zero h]
    refine ⟨k * |Q|, mul_pos hk (abs_pos.mpr h), k, fun z ↦ ?_⟩
    rw [loss, ite_eq_right h, mul_assoc, ← abs_mul]
    change k * |Q * z| - k ≤ k * _
    rw [show k * |Q*z| - k = k * (|Q*z| - 1) by ring]
    apply mul_le_mul_of_nonneg_left _ hk.le
    rcases le_total 0 (Q*z) with hz | hz
    · rw [abs_of_nonneg hz]
      nlinarith [quadratic_le_exp_of_nonneg hz, sq_nonneg (Q*z-1)]
    · rw [abs_of_nonpos hz]
      linarith [exp_pos (Q*z)]

/-- At fixed location and fixed real shape, any nonzero log residual gives
a unique positive scale maximizer, including the normal shape. -/
theorem held_location_fixed_shape_scale {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (μ Q : ℝ)
    (hne : ∃ i, log (x i) ≠ μ) :
    ∃! σ, σ ∈ Ioi (0 : ℝ) ∧
      IsMaxOn (fun s ↦ GenGammaTwoPoint.sampleLogLik x (μ, s, Q)) (Ioi 0) σ := by
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  let z : Fin n → ℝ := fun i ↦ log (x i) - μ
  let f : ℝ → ℝ := fun r ↦ (n : ℝ) * log r - ∑ i, loss Q (r * z i)
  let g : ℝ → ℝ := fun s ↦ GenGammaTwoPoint.sampleLogLik x (μ, s, Q)
  let C := GenGammaTwoPoint.logDensity μ 1 Q μ
  have he (r : ℝ) (hr : 0 < r) :
      (n : ℝ) * g r⁻¹ = f r + ((n : ℝ) * C - ∑ i, log (x i)) := by
    have hden (i : Fin n) : GenGammaTwoPoint.logDensity μ r⁻¹ Q (log (x i)) =
        log r + C - loss Q (r * z i) := by
      by_cases hQ : Q = 0
      · simp [GenGammaTwoPoint.logDensity, C, loss, z, hQ, div_inv_eq_mul, log_inv]
        ring
      · simp only [GenGammaTwoPoint.logDensity, C, loss, hQ, ite_false, log_inv, div_one,
          log_one, sub_zero, sub_self, mul_zero, exp_zero, z, div_inv_eq_mul]
        rw [log_mul (abs_ne_zero.mpr hQ) hr.ne']
        ring
    simp only [g, GenGammaTwoPoint.sampleLogLik, mul_div_cancel₀ _ hn'.ne', hden, f,
      Finset.sum_sub_distrib, Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul]
    ring
  have hconc : StrictConcaveOn ℝ (Ioi 0) f := by
    have hlog : StrictConcaveOn ℝ (Ioi 0) (fun r ↦ (n : ℝ) * log r) := by
      refine ⟨convex_Ioi 0, fun r hr s hs hne a b ha hb hab ↦ ?_⟩
      have h := strictConcaveOn_log_Ioi.2 hr hs hne ha hb hab
      simp only [smul_eq_mul] at h ⊢
      nlinarith only [mul_lt_mul_of_pos_left h hn']
    have hsum : ConvexOn ℝ (Ioi 0) (fun r ↦ ∑ i, loss Q (r * z i)) := by
      refine ⟨convex_Ioi 0, fun r _ t _ a b ha hb hab ↦ ?_⟩
      change (∑ i, loss Q ((a*r + b*t) * z i)) ≤
        a * (∑ i, loss Q (r*z i)) + b * ∑ i, loss Q (t*z i)
      rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
      apply Finset.sum_le_sum
      intro i _
      rw [show (a*r + b*t) * z i = a*(r*z i)+b*(t*z i) by ring]
      exact (loss_convex Q).2 (mem_univ _) (mem_univ _) ha hb hab
    exact hlog.sub_convexOn hsum
  obtain ⟨i, hi⟩ := hne
  obtain ⟨d, hd, B, hB⟩ := loss_linear_minorant Q
  let v := d * |z i|
  have hv : 0 < v := mul_pos hd (abs_pos.mpr (sub_ne_zero.mpr hi))
  have hone (r : ℝ) (hr : 0 < r) : f r ≤ (n : ℝ) * log r - v*r + B := by
    have h := Finset.single_le_sum (f := fun i ↦ loss Q (r*z i))
      (fun j _ ↦ loss_nonneg Q (r*z j)) (Finset.mem_univ i)
    have hBi := hB (r*z i)
    rw [abs_mul, abs_of_pos hr] at hBi
    dsimp [f, v]
    nlinarith only [h, hBi]
  have hzero (r : ℝ) : f r ≤ (n : ℝ) * log r := by
    have h := Finset.sum_nonneg (s := Finset.univ) (fun i _ ↦ loss_nonneg Q (r*z i))
    dsimp [f]
    linarith
  have htail : Tendsto (fun r : Ioi (0 : ℝ) ↦ f r) (cocompact (Ioi (0 : ℝ))) atBot := by
    apply tendsto_atBot.2
    intro M
    let lo := exp (M / n)
    let k := v / (2 * (n : ℝ))
    have hk : 0 < k := by dsimp [k]; positivity
    let hi := (B - M - n * (log k + 1)) / (v/2)
    have hD : Icc lo hi ⊆ Ioi (0 : ℝ) := fun _ ht ↦ (exp_pos _).trans_le ht.1
    have hK : IsCompact (Subtype.val ⁻¹' Icc lo hi : Set (Ioi (0 : ℝ))) :=
      Topology.IsEmbedding.subtypeVal.isInducing.isCompact_preimage' isCompact_Icc
        (by simpa only [Subtype.range_coe] using hD)
    have hev : ∀ᶠ r : Ioi (0 : ℝ) in cocompact (Ioi (0 : ℝ)),
        r ∈ (Subtype.val ⁻¹' Icc lo hi : Set (Ioi (0 : ℝ)))ᶜ := hK.compl_mem_cocompact
    apply hev.mono
    intro r hnt
    by_contra! h
    have hr : 0 < (r : ℝ) := r.property
    apply hnt
    change (r : ℝ) ∈ Icc lo hi
    refine ⟨?_, ?_⟩
    · rw [← exp_log hr, exp_le_exp]
      exact (div_le_iff₀ hn').2 (by linarith [hzero r])
    · have hl := log_le_sub_one_of_pos (mul_pos hk hr)
      rw [log_mul hk.ne' hr.ne'] at hl
      have hM := hone r hr
      apply (le_div_iff₀ (half_pos hv)).2
      have he : (n : ℝ) * (k * r) = (v/2) * r := by dsimp [k]; field_simp
      nlinarith only [h, hM, he, mul_le_mul_of_nonneg_left hl hn'.le]
  obtain ⟨r, ⟨hr, hm⟩, hu⟩ :=
    Generic.existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
      (nonempty_Ioi (a := (0 : ℝ))) isOpen_Ioi hconc htail
  have hr' : 0 < r := hr
  have hm' : IsMaxOn g (Ioi 0) r⁻¹ := by
    apply isMaxOn_iff.mpr
    intro s hs
    have hs' : 0 < s := hs
    have hf := isMaxOn_iff.mp hm s⁻¹ (inv_pos.mpr hs')
    have hgs := he s⁻¹ (inv_pos.mpr hs')
    rw [inv_inv] at hgs
    apply (mul_le_mul_iff_right₀ hn').mp
    linarith only [hf, hgs, he r hr']
  refine ⟨r⁻¹, ⟨inv_pos.mpr hr', hm'⟩, ?_⟩
  rintro s ⟨hs, hmS⟩
  have hs' : 0 < s := hs
  have hsmax : IsMaxOn f (Ioi 0) s⁻¹ := by
    apply isMaxOn_iff.mpr
    intro t ht
    have ht' : 0 < t := ht
    have hh := isMaxOn_iff.mp hmS t⁻¹ (inv_pos.mpr ht')
    have heS := he s⁻¹ (inv_pos.mpr hs')
    rw [inv_inv] at heS
    linarith only [heS, he t ht', mul_le_mul_of_nonneg_left hh hn'.le]
  simpa only [inv_inv] using congrArg Inv.inv (hu s⁻¹ ⟨inv_pos.mpr hs', hsmax⟩)

end
end UnivariateML.AllFreeGenGamma

end
