module

public import UnivariateML.Family.Shape2Fatigue
public import UnivariateML.Generic.Compactification

/-! Fatigue life with held scale and free threshold, in gap coordinates. -/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.PGWFatigue

noncomputable section

variable {n : ℕ}

def shifted (g : Fin n → ℝ) (d : ℝ) : Fin n → ℝ := fun i ↦ d + g i

def Q (g : Fin n → ℝ) (b d : ℝ) : ℝ := Fatigue.shapeSq (shifted g d) b

def rationalProfile (g : Fin n → ℝ) (b d : ℝ) : ℝ :=
  (∏ i, (shifted g d i + b) ^ 2 / (shifted g d i) ^ 3) / (b * Q g b d) ^ n

def squaredLik (g : Fin n → ℝ) (b : ℝ) (p : ℝ × ℝ) : ℝ :=
  exp ((2 * n : ℝ) * (Fatigue.logLik (shifted g p.1) p.2 b +
    log 2 + log (2 * π) / 2 + 1 / 2))

private theorem shifted_pos {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {d : ℝ} (hd : 0 < d) (i : Fin n) : 0 < shifted g d i :=
  add_pos_of_pos_of_nonneg hd (hg i)

private theorem q_pos (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hne : ∃ i j, g i ≠ g j) {b d : ℝ} (hb : 0 < b) (hd : 0 < d) :
    0 < Q g b d := by
  obtain ⟨i, j, hij⟩ := hne
  exact Fatigue.shapeSq_pos hn (shifted_pos hg hd)
    ⟨i, j, fun h ↦ hij (add_left_cancel h)⟩ hb

private theorem profile_pos {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {b d : ℝ} (hb : 0 < b) (hd : 0 < d) (hq : 0 < Q g b d) :
    0 < rationalProfile g b d := by
  unfold rationalProfile
  apply div_pos _ (pow_pos (mul_pos hb hq) _)
  exact Finset.prod_pos fun i _ ↦
    div_pos (sq_pos_of_pos (add_pos (shifted_pos hg hd i) hb))
      (pow_pos (shifted_pos hg hd i) _)

/-- The normalized squared likelihood is the rational profile times the
exact joint penalty for shape error. -/
theorem squared_identity (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {b d a : ℝ} (hb : 0 < b) (hd : 0 < d) (ha : 0 < a) (hq : 0 < Q g b d) :
    let u := Q g b d / a ^ 2
    squaredLik g b (d, a) = rationalProfile g b d * (u * exp (1 - u)) ^ n := by
  intro u
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have ht := shifted_pos hg hd
  have hq' : 0 < u := div_pos hq (sq_pos_of_pos ha)
  have hR := profile_pos hg hb hd hq
  rw [← exp_log (mul_pos hR (pow_pos (mul_pos hq' (exp_pos _)) _))]
  unfold squaredLik
  congr 1
  rw [log_mul hR.ne' (pow_pos (mul_pos hq' (exp_pos _)) _).ne', log_pow,
    log_mul hq'.ne' (exp_pos _).ne', log_exp]
  have hl (i : Fin n) : log ((shifted g d i + b) ^ 2 / (shifted g d i) ^ 3) =
      2 * log (shifted g d i + b) - 3 * log (shifted g d i) := by
    rw [log_div (pow_pos (add_pos (ht i) hb) _).ne' (pow_pos (ht i) _).ne',
      log_pow, log_pow]
    norm_num
  have hu : log u = log (Q g b d) - 2 * log a := by
    dsimp [u]
    rw [log_div hq.ne' (sq_pos_of_pos ha).ne', log_pow]
    norm_num
  unfold rationalProfile
  rw [log_div (Finset.prod_pos fun i _ ↦
    div_pos (sq_pos_of_pos (add_pos (ht i) hb)) (pow_pos (ht i) _)).ne'
    (pow_pos (mul_pos hb hq) _).ne', log_prod, log_pow, log_mul hb.ne' hq.ne', hu]
  · simp_rw [hl, Finset.sum_sub_distrib, ← Finset.mul_sum]
    unfold Fatigue.logLik Fatigue.scaleTerm Fatigue.dataTerm Fatigue.avg
    dsimp [u, Q]
    field_simp; ring
  · exact fun i _ ↦ (div_pos (sq_pos_of_pos (add_pos (ht i) hb))
      (pow_pos (ht i) _)).ne'

private theorem profile_equality (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {b d : ℝ} (hb : 0 < b) (hd : 0 < d) (hq : 0 < Q g b d) :
    let a := Fatigue.shape (shifted g d) b
    0 < a ∧ squaredLik g b (d, a) = rationalProfile g b d := by
  intro a
  obtain ⟨ha, he, _⟩ := Fatigue.heldScale_unique (shifted g d) hq
  refine ⟨ha, ?_⟩
  rw [squared_identity hn hg hb hd ha hq]
  change _ * (Q g b d / a ^ 2 * exp (1 - Q g b d / a ^ 2)) ^ n = _
  change a ^ 2 = Q g b d at he
  rw [he, div_self hq.ne']
  simp

/-- The rational profile is an upper envelope for every positive shape,
and the conditional optimum realizes it at each threshold. -/
theorem joint_envelope (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {b d : ℝ} (hb : 0 < b) (hd : 0 < d) (hq : 0 < Q g b d) :
    (∀ a > 0, squaredLik g b (d, a) ≤ rationalProfile g b d) ∧
      ∃ a > 0, squaredLik g b (d, a) = rationalProfile g b d := by
  obtain ⟨ha, he⟩ := profile_equality hn hg hb hd hq
  refine ⟨fun a haa ↦ ?_, ⟨_, ha, he⟩⟩
  rw [← he]
  apply exp_le_exp.mpr
  have hnR : 0 ≤ (2 * n : ℝ) := by positivity
  apply mul_le_mul_of_nonneg_left _ hnR
  have h := (Fatigue.heldScale_unique (shifted g d) hq).2.2
  by_cases hs : a = Fatigue.shape (shifted g d) b
  · simp [hs]
  · have h' := (h a haa hs).le
    unfold Fatigue.profile at h'
    dsimp only
    linarith only [h']

private theorem bQ_eq (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {b d : ℝ} (hb : 0 < b) (hd : 0 < d) :
    b * Q g b d = Fatigue.avg (fun i ↦ (shifted g d i - b) ^ 2 / shifted g d i) := by
  unfold Q
  rw [Fatigue.shapeSq_eq_mean_square hn (shifted_pos hg hd) hb]
  unfold Fatigue.avg
  rw [← mul_div_assoc, Finset.mul_sum]
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  field_simp [hb.ne', (shifted_pos hg hd i).ne']

/-- The profile vanishes at infinite threshold distance. The joint
likelihood is bounded above by the same decaying envelope. -/
theorem infinite_threshold (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {b : ℝ} (hb : 0 < b) : Tendsto (rationalProfile g b) atTop (𝓝 0) := by
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hlimit := ((tendsto_inv_atTop_zero.const_mul (16 : ℝ)).pow n :
    Tendsto (fun d : ℝ ↦ (16 * d⁻¹) ^ n) atTop (𝓝 ((16 * 0) ^ n)))
  simp only [mul_zero, zero_pow hn.ne'] at hlimit
  refine squeeze_zero' ?_ ?_ hlimit
  · filter_upwards [eventually_ge_atTop (max 1 (2 * b))] with d hd
    have hd₁ := (le_max_left _ _).trans hd
    have hdb := (le_max_right _ _).trans hd
    have hd₀ : 0 < d := zero_lt_one.trans_le hd₁
    have hq : 0 < Q g b d := (Fatigue.shapeSq_pos_iff hn (shifted_pos hg hd₀) hb).2
      ⟨⟨0, hn⟩, by have := hg ⟨0, hn⟩; dsimp [shifted]; linarith⟩
    exact (profile_pos hg hb hd₀ hq).le
  · filter_upwards [eventually_ge_atTop (max 1 (2 * b))] with d hd
    have hd₁ := (le_max_left _ _).trans hd
    have hdb := (le_max_right _ _).trans hd
    have hd₀ : 0 < d := zero_lt_one.trans_le hd₁
    have ht (i : Fin n) : d ≤ shifted g d i := le_add_of_nonneg_right (hg i)
    have ht₀ := shifted_pos hg hd₀
    have hQ : d / 4 ≤ b * Q g b d := by
      rw [bQ_eq hn hg hb hd₀, Fatigue.avg, le_div_iff₀ hnR]
      have h := Finset.sum_le_sum (s := (Finset.univ : Finset (Fin n)))
        (f := fun _ ↦ d / 4) (g := fun i ↦
          (shifted g d i - b) ^ 2 / shifted g d i) (fun i _ ↦ by
            rw [le_div_iff₀ (ht₀ i)]
            have h : (shifted g d i / 2) ^ 2 ≤ (shifted g d i - b) ^ 2 := by
              exact pow_le_pow_left₀ (by linarith [ht₀ i]) (by linarith [ht i]) 2
            nlinarith [mul_le_mul_of_nonneg_right (ht i) (ht₀ i).le])
      simpa [mul_comm] using h
    have hprod : (∏ i, (shifted g d i + b) ^ 2 / (shifted g d i) ^ 3) ≤ (4 / d) ^ n := by
      calc
        _ ≤ ∏ _ : Fin n, (4 / d) := by
          apply Finset.prod_le_prod₀ (fun i _ ↦
            div_nonneg (sq_nonneg _) (pow_nonneg (ht₀ i).le _))
          intro i _
          calc
            _ ≤ 4 / shifted g d i := by
              rw [div_le_div_iff₀ (pow_pos (ht₀ i) _) (ht₀ i)]
              have he : (shifted g d i + b) ^ 2 ≤ (2 * shifted g d i) ^ 2 := by
                exact pow_le_pow_left₀ (by linarith [ht₀ i]) (by linarith [ht i]) 2
              nlinarith [mul_le_mul_of_nonneg_right he (ht₀ i).le]
            _ ≤ _ := div_le_div_of_nonneg_left (by norm_num) hd₀ (ht i)
        _ = _ := by simp only [Finset.prod_const, Finset.card_univ, Fintype.card_fin]
    unfold rationalProfile
    calc
      _ ≤ (4 / d) ^ n / (d / 4) ^ n :=
        div_le_div₀ (by positivity) hprod (pow_pos (by positivity) _) (by gcongr)
      _ = (16 * (d⁻¹ * d⁻¹)) ^ n := by rw [← div_pow]; congr 1; field_simp; ring
      _ ≤ _ := by
        gcongr
        exact mul_le_of_le_one_right (inv_nonneg.mpr hd₀.le) (inv_le_one_of_one_le₀ hd₁)

open Classical in
noncomputable def multiplicity (g : Fin n → ℝ) : ℕ :=
  (Finset.univ.filter fun i ↦ g i = 0).card

open Classical in
noncomputable def safeShift (g : Fin n → ℝ) (d : ℝ) (i : Fin n) : ℝ :=
  if g i = 0 then 1 else shifted g d i

open Classical in
noncomputable def scaledResidual (g : Fin n → ℝ) (b d : ℝ) : ℝ :=
  Fatigue.avg (fun i ↦ (shifted g d i - b) ^ 2 *
    (if g i = 0 then 1 else d / shifted g d i))

def edgeFactor (g : Fin n → ℝ) (b d : ℝ) : ℝ :=
  (∏ i, (shifted g d i + b) ^ 2 / (safeShift g d i) ^ 3) / (scaledResidual g b d) ^ n

private theorem scaledResidual_eq (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {b d : ℝ} (hb : 0 < b) (hd : 0 < d) :
    scaledResidual g b d = d * (b * Q g b d) := by
  classical
  rw [bQ_eq hn hg hb hd]
  unfold scaledResidual Fatigue.avg
  rw [← mul_div_assoc, Finset.mul_sum]
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  by_cases he : g i = 0
  · simp only [he, ite_true, mul_one, shifted, add_zero]
    field_simp [hd.ne']
  · simp only [he, ite_false]
    ring

private theorem edge_factorization (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {b d : ℝ} (hb : 0 < b) (hd : 0 < d) :
    rationalProfile g b d = edgeFactor g b d * (d ^ n / d ^ (3 * multiplicity g)) := by
  classical
  let t := shifted g d
  let w := fun i ↦ if g i = 0 then d else 1
  have ht (i : Fin n) : t i = safeShift g d i * w i := by
    by_cases hi : g i = 0 <;> simp [t, w, safeShift, shifted, hi]
  have hw : (∏ i, w i) = d ^ multiplicity g := by
    simp [w, multiplicity, ← Finset.prod_filter]
  have he : (∏ i, (t i + b) ^ 2 / (t i) ^ 3) =
      (∏ i, (t i + b) ^ 2 / (safeShift g d i) ^ 3) / (d ^ (3 * multiplicity g)) := by
    rw [Nat.mul_comm 3 (multiplicity g), pow_mul, ← hw, ← Finset.prod_pow,
      ← Finset.prod_div_distrib]
    apply Finset.prod_congr rfl
    intro i _
    rw [div_div, ← mul_pow, ← ht]
  unfold rationalProfile edgeFactor
  change (∏ i, (t i + b) ^ 2 / (t i) ^ 3) / _ = _
  rw [he, scaledResidual_eq hn hg hb hd]
  let P := ∏ i, (t i + b) ^ 2 / (safeShift g d i) ^ 3
  let R := d ^ (3 * multiplicity g)
  let V := (b * Q g b d) ^ n
  change P / R / V = P / (d * (b * Q g b d)) ^ n * (d ^ n / R)
  rw [mul_pow, div_div, div_mul_div_comm]
  conv_rhs => rw [show d ^ n * V * R = d ^ n * (R * V) by ring, mul_comm P]
  rw [mul_div_mul_left _ _ (pow_ne_zero _ hd.ne')]

private theorem scaledResidual_regular {g : Fin n → ℝ}
    (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    ContinuousAt (scaledResidual g b) 0 ∧ 0 < scaledResidual g b 0 := by
  classical
  have hc (i : Fin n) : Continuous (fun d : ℝ ↦ shifted g d i) :=
    continuous_id.add continuous_const
  have hr : ContinuousAt (scaledResidual g b) 0 := by
    unfold scaledResidual Fatigue.avg
    apply ContinuousAt.div_const
    apply tendsto_finsetSum Finset.univ (fun i _ ↦ ?_)
    apply ((hc i).continuousAt.sub continuousAt_const).pow 2 |>.mul
    by_cases hi : g i = 0 <;> simp only [hi, ite_true, ite_false]
    · exact continuousAt_const
    · exact continuousAt_id.div (hc i).continuousAt (by simpa [shifted] using hi)
  have hr₀ : 0 < scaledResidual g b 0 := by
    obtain ⟨i, hi⟩ := hz
    have hnR : 0 < (n : ℝ) := by exact_mod_cast Fin.pos i
    unfold scaledResidual Fatigue.avg
    apply div_pos _ hnR
    refine Finset.sum_pos' (fun j _ ↦ ?_) ⟨i, Finset.mem_univ _, ?_⟩
    · by_cases he : g j = 0 <;> simp [he, sq_nonneg]
    · simpa [hi, shifted] using sq_pos_of_pos hb
  exact ⟨hr, hr₀⟩

private theorem trace_regular {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    ContinuousAt (edgeFactor g b) 0 ∧ 0 < edgeFactor g b 0 := by
  classical
  have hs (i : Fin n) : 0 < safeShift g 0 i := by
    by_cases hi : g i = 0
    · simp [safeShift, hi]
    · simpa [safeShift, shifted, hi] using lt_of_le_of_ne (hg i) (Ne.symm hi)
  have hc (i : Fin n) : Continuous (fun d : ℝ ↦ shifted g d i) :=
    continuous_id.add continuous_const
  have hsC (i : Fin n) : Continuous (fun d : ℝ ↦ safeShift g d i) := by
    by_cases hi : g i = 0 <;> simp only [safeShift, hi, ite_true, ite_false]
    · exact continuous_const
    · exact hc i
  obtain ⟨hr, hr₀⟩ := scaledResidual_regular hz hb
  unfold edgeFactor
  refine ⟨?_, ?_⟩
  · apply ContinuousAt.div _ (hr.pow n) (pow_pos hr₀ _).ne'
    apply tendsto_finsetProd Finset.univ (fun i _ ↦ ?_)
    exact (((hc i).continuousAt.add continuousAt_const).pow 2).div
      ((hsC i).continuousAt.pow 3) (pow_pos (hs i) _).ne'
  · apply div_pos _ (pow_pos hr₀ _)
    apply Finset.prod_pos
    intro i _
    exact div_pos (sq_pos_of_pos (add_pos_of_nonneg_of_pos
      (by simpa [shifted] using hg i) hb)) (pow_pos (hs i) _)

/-- The conditional shape diverges at threshold contact for every
nonempty sample with at least one minimum observation. -/
theorem threshold_shape_diverges (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    Tendsto (fun d ↦ Fatigue.shape (shifted g d) b) (𝓝[>] 0) atTop := by
  classical
  let R := scaledResidual g b
  obtain ⟨hr, hr₀⟩ := scaledResidual_regular hz hb
  have hQ : Tendsto (Q g b) (𝓝[>] 0) atTop := by
    have ht := ((hr.continuousWithinAt.div_const b).pos_mul_atTop
      (div_pos hr₀ hb) (tendsto_inv_nhdsGT_zero (𝕜 := ℝ)))
    apply ht.congr'
    filter_upwards [self_mem_nhdsWithin] with d (hd : 0 < d)
    change scaledResidual g b d / b * d⁻¹ = Q g b d
    rw [scaledResidual_eq hn hg hb hd]
    field_simp
  exact tendsto_sqrt_atTop.comp hQ

/-- The exact edge exponent is sample size minus three times the number
of minima. Its remaining factor has a positive, finite limit even for
constant samples. -/
theorem threshold_trace (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    0 < edgeFactor g b 0 ∧
      Tendsto (edgeFactor g b) (𝓝[>] 0) (𝓝 (edgeFactor g b 0)) ∧
      ∀ d > 0, rationalProfile g b d =
        edgeFactor g b d * (d ^ n / d ^ (3 * multiplicity g)) := by
  obtain ⟨hc, hp⟩ := trace_regular hg hz hb
  exact ⟨hp, hc.continuousWithinAt, fun _ hd ↦ edge_factorization hn hg hb hd⟩


private theorem profile_continuousAt (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {b d : ℝ} (hb : 0 < b) (hd : 0 < d)
    (hq : 0 < Q g b d) : ContinuousAt (rationalProfile g b) d := by
  have ht := shifted_pos hg hd
  have hc (i : Fin n) : ContinuousAt (fun e : ℝ ↦ shifted g e i) d :=
    continuousAt_id.add continuousAt_const
  have hR : ContinuousAt
      (fun e ↦ Fatigue.avg (fun i ↦ (shifted g e i - b) ^ 2 / shifted g e i)) d := by
    unfold Fatigue.avg
    apply ContinuousAt.div_const
    exact tendsto_finsetSum Finset.univ fun i _ ↦
      ((hc i).sub continuousAt_const |>.pow 2).div (hc i) (ht i).ne'
  have heq := bQ_eq hn hg hb hd
  have hf : ContinuousAt (fun e ↦ (∏ i, (shifted g e i + b) ^ 2 / (shifted g e i) ^ 3) /
      (Fatigue.avg (fun i ↦ (shifted g e i - b) ^ 2 / shifted g e i)) ^ n) d := by
    apply ContinuousAt.div _ (hR.pow n) (by
      change (Fatigue.avg (fun i ↦ (shifted g d i - b) ^ 2 / shifted g d i)) ^ n ≠ 0
      rw [← heq]
      exact (pow_pos (mul_pos hb hq) _).ne')
    exact tendsto_finsetProd Finset.univ fun i _ ↦
      ((hc i).add continuousAt_const |>.pow 2).div ((hc i).pow 3) (pow_pos (ht i) _).ne'
  apply hf.congr
  have h : ∀ᶠ e in 𝓝 d, 0 < e := Ioi_mem_nhds hd
  exact h.mono fun e he ↦ by dsimp only; rw [← bQ_eq hn hg hb he]; rfl

private theorem profile_continuous (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hne : ∃ i j, g i ≠ g j) {b : ℝ} (hb : 0 < b) :
    ContinuousOn (rationalProfile g b) (Ioi 0) :=
  fun _ hd ↦ (profile_continuousAt hn hg hb hd (q_pos hn hg hne hb hd)).continuousWithinAt

private theorem edge_small (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b)
    (hs : 3 * multiplicity g < n) : Tendsto (rationalProfile g b) (𝓝[>] 0) (𝓝 0) := by
  obtain ⟨_, ht, he⟩ := threshold_trace hn hg hz hb
  have hd : Tendsto (fun d : ℝ ↦ d) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have h := ht.mul (hd.pow (n - 3 * multiplicity g))
  rw [zero_pow (Nat.sub_pos_of_lt hs).ne', mul_zero] at h
  apply h.congr'
  filter_upwards [self_mem_nhdsWithin] with d (hd : 0 < d)
  rw [he d hd]
  congr 1
  conv_rhs => arg 1; rw [← Nat.sub_add_cancel hs.le, pow_add]
  exact (mul_div_cancel_right₀ _ (pow_ne_zero _ hd.ne')).symm

private theorem edge_equal (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b)
    (hs : 3 * multiplicity g = n) :
    Tendsto (rationalProfile g b) (𝓝[>] 0) (𝓝 (edgeFactor g b 0)) := by
  obtain ⟨_, ht, he⟩ := threshold_trace hn hg hz hb
  apply ht.congr'
  filter_upwards [self_mem_nhdsWithin] with d (hd : 0 < d)
  rw [he d hd, hs, div_self (pow_ne_zero _ hd.ne'), mul_one]

private theorem edge_large (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b)
    (hs : n < 3 * multiplicity g) :
    Tendsto (rationalProfile g b) (𝓝[>] 0) atTop := by
  obtain ⟨hp, ht, he⟩ := threshold_trace hn hg hz hb
  have hi := (tendsto_pow_atTop (Nat.sub_pos_of_lt hs).ne').comp
    (tendsto_inv_nhdsGT_zero (𝕜 := ℝ))
  apply (ht.pos_mul_atTop hp hi).congr'
  filter_upwards [self_mem_nhdsWithin] with d (hd : 0 < d)
  rw [he d hd]
  congr 1
  rw [Function.comp_apply, inv_pow]
  conv_rhs => arg 2; rw [← Nat.sub_add_cancel hs.le, pow_add]
  rw [div_eq_mul_inv, mul_inv_rev, ← mul_assoc, mul_inv_cancel₀ (pow_ne_zero _ hd.ne'), one_mul]

private theorem max_above_edge {f : ℝ → ℝ} (hc : ContinuousOn f (Ioi 0))
    (hInf : Tendsto f atTop (𝓝 0)) {e d : ℝ}
    (h0 : Tendsto f (𝓝[>] 0) (𝓝 e)) (hd : 0 < d) (hp : 0 < f d) (he : e ≤ f d) :
    ∃ x ∈ Ioi 0, IsMaxOn f (Ioi 0) x := by
  obtain ⟨R, hR⟩ := eventually_atTop.mp (hInf.eventually (Iio_mem_nhds hp))
  let U := max R d
  have hU : 0 < U := hd.trans_le (le_max_right _ _)
  let F := fun t ↦ if t = 0 then e else f t
  have hF : ContinuousOn F (Icc 0 U) := by
    intro t ht
    rcases ht.1.eq_or_lt with h | h
    · subst t
      have h' : Tendsto F (𝓝[>] 0) (𝓝 e) := h0.congr' <|
        (show ∀ᶠ x : ℝ in 𝓝[>] 0, 0 < x from self_mem_nhdsWithin).mono
          fun x hx ↦ by simp [F, hx.ne']
      have hC : ContinuousWithinAt F (Ici 0) 0 := by
        rw [← insert_Ioi, continuousWithinAt_insert_self]
        simpa [ContinuousWithinAt, show F 0 = e by simp [F]] using h'
      exact hC.mono fun _ hx ↦ hx.1
    · have hp' : ∀ᶠ x : ℝ in 𝓝 t, 0 < x := Ioi_mem_nhds h
      have h' : F =ᶠ[𝓝 t] f := hp'.mono fun x hx ↦ by simp [F, hx.ne']
      have hf : ContinuousAt f t := (hc t h).continuousAt (Ioi_mem_nhds h)
      exact (hf.congr h'.symm).continuousWithinAt
  have hde : d ∈ Ioi 0 ∩ Icc 0 U := ⟨hd, hd.le, le_max_right _ _⟩
  obtain ⟨x, hx, hm⟩ := Generic.exists_isMaxOn_of_boundary_le
    (D := Ioi 0 ∩ Icc 0 U) (K := Icc 0 U) (fun _ h ↦ h.2)
    isCompact_Icc hF hde (fun t ht ↦ by
      have h : t = 0 := le_antisymm (le_of_not_gt (fun h ↦ ht.2 ⟨h, ht.1⟩)) ht.1.1
      simpa [F, h, hd.ne'] using he)
  refine ⟨x, hx.1, fun t ht ↦ ?_⟩
  by_cases h : t ≤ U
  · simpa [F, (show 0 < t from ht).ne', (show 0 < x from hx.1).ne'] using
      hm (show t ∈ Ioi 0 ∩ Icc 0 U from ⟨ht, ht.le, h⟩)
  · have hh := hR t ((le_max_left _ _).trans (le_of_not_ge h))
    have hmd : f d ≤ f x := by
      simpa [F, hd.ne', (show 0 < x from hx.1).ne'] using hm hde
    exact hh.le.trans hmd


private theorem exists_shift_max_iff (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hne : ∃ i j, g i ≠ g j) {b : ℝ} (hb : 0 < b) :
    (∃ p ∈ Fatigue.domain, IsMaxOn (fun p : ℝ × ℝ ↦
      Fatigue.logLik (shifted g p.1) p.2 b) Fatigue.domain p) ↔
      ∃ d ∈ Ioi 0, IsMaxOn (rationalProfile g b) (Ioi 0) d := by
  let L := fun p : ℝ × ℝ ↦ Fatigue.logLik (shifted g p.1) p.2 b
  have hnR : 0 < (2 * n : ℝ) := by positivity
  have he (p q : ℝ × ℝ) : squaredLik g b p ≤ squaredLik g b q ↔ L p ≤ L q := by
    rw [squaredLik, squaredLik, exp_le_exp,
      mul_le_mul_iff_right₀ hnR, add_le_add_iff_right, add_le_add_iff_right,
      add_le_add_iff_right]
  constructor
  · rintro ⟨⟨d, a⟩, ⟨hd, ha⟩, hm⟩
    refine ⟨d, hd, fun e he' ↦ ?_⟩
    obtain ⟨_, v, hv, hV⟩ := joint_envelope hn hg hb he' (q_pos hn hg hne hb he')
    change rationalProfile g b e ≤ rationalProfile g b d
    rw [← hV]
    exact ((he _ _).2 (hm (show (e, v) ∈ Fatigue.domain from ⟨he', hv⟩))).trans
      ((joint_envelope hn hg hb hd (q_pos hn hg hne hb hd)).1 a ha)
  · rintro ⟨d, hd, hm⟩
    obtain ⟨_, a, ha, hA⟩ := joint_envelope hn hg hb hd (q_pos hn hg hne hb hd)
    refine ⟨(d, a), ⟨hd, ha⟩, ?_⟩
    rintro ⟨e, v⟩ ⟨he', hv⟩
    apply (he _ _).mp
    rw [hA]
    exact ((joint_envelope hn hg hb he' (q_pos hn hg hne hb he')).1 v hv).trans (hm he')

private theorem constant_edge (hn : 0 < n) {b : ℝ} (hb : 0 < b) :
    Tendsto (rationalProfile (fun _ : Fin n ↦ (0 : ℝ)) b) (𝓝[>] 0) atTop := by
  classical
  apply edge_large hn (fun _ ↦ le_rfl) ⟨⟨0, hn⟩, rfl⟩ hb
  simp [multiplicity]
  omega

private theorem constant_collapse (hn : 0 < n) {b : ℝ} (hb : 0 < b) :
    Tendsto (fun a ↦ Fatigue.logLik (shifted (fun _ : Fin n ↦ (0 : ℝ)) b) a b)
      (𝓝[>] 0) atTop := by
  rw [show shifted (fun _ : Fin n ↦ (0 : ℝ)) b = fun _ ↦ b by
    funext i; exact add_zero b]
  exact Fatigue.constant_unbounded hn hb

set_option maxHeartbeats 800000 in
-- The joined proof elaborates comparisons in the physical and profiled objectives.
/-- The full attainment criterion in the held-scale, free-threshold
regime. Equality compares the rational profile with its limiting trace
at a strictly positive threshold distance. -/
theorem held_beta_criterion (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    (∃ p ∈ Fatigue.domain, IsMaxOn (fun p : ℝ × ℝ ↦
      Fatigue.logLik (shifted g p.1) p.2 b) Fatigue.domain p) ↔
      (∃ i j, g i ≠ g j) ∧
        (3 * multiplicity g < n ∨
          (3 * multiplicity g = n ∧ ∃ d > 0, rationalProfile g b d = edgeFactor g b 0)) := by
  by_cases hne : ∃ i j, g i ≠ g j
  · rw [exists_shift_max_iff hn hg hne hb]
    simp only [hne, true_and]
    let f := rationalProfile g b
    let e := edgeFactor g b 0
    have hc := profile_continuous hn hg hne hb
    have hInf := infinite_threshold hn hg hb
    have hp (d : ℝ) (hd : 0 < d) : 0 < f d :=
      profile_pos hg hb hd (q_pos hn hg hne hb hd)
    rcases lt_trichotomy (3 * multiplicity g) n with hs | hs | hs
    · simp only [hs, true_or, iff_true]
      exact max_above_edge hc hInf (edge_small hn hg hz hb hs) zero_lt_one
        (hp 1 zero_lt_one) (hp 1 zero_lt_one).le
    · simp only [hs, lt_self_iff_false, false_or, true_and]
      have h0 := edge_equal hn hg hz hb hs
      have he : 0 < e := (trace_regular hg hz hb).2
      constructor
      · rintro ⟨d, hd, hm⟩
        have hle : e ≤ f d := le_of_tendsto h0 <|
          (show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).mono fun t ht ↦ hm ht
        rcases hle.eq_or_lt with h | h
        · exact ⟨d, hd, h.symm⟩
        · obtain ⟨R, hR⟩ := eventually_atTop.mp (hInf.eventually (Iio_mem_nhds he))
          let U := max R d
          have hU : d ≤ U := le_max_right _ _
          have hout : f U < e := hR U (le_max_left _ _)
          have hmem : e ∈ Icc (f U) (f d) := ⟨hout.le, h.le⟩
          obtain ⟨x, hx, hxe⟩ := intermediate_value_Icc' hU
            (hc.mono (fun _ hx ↦ hd.trans_le hx.1)) hmem
          exact ⟨x, hd.trans_le hx.1, hxe⟩
      · rintro ⟨d, hd, heq⟩
        exact max_above_edge hc hInf h0 hd (hp d hd) heq.ge
    · have hnmax : ¬ ∃ d ∈ Ioi 0, IsMaxOn f (Ioi 0) d := by
        rintro ⟨d, hd, hm⟩
        obtain ⟨v, hv, hv'⟩ := ((edge_large hn hg hz hb hs).eventually
          (eventually_gt_atTop (f d))).and self_mem_nhdsWithin |>.exists
        exact hv.not_ge (hm hv')
      exact iff_of_false hnmax (by
        rintro (h | ⟨h, _⟩) <;> omega)
  · simp only [hne, false_and, iff_false]
    push Not at hne
    obtain ⟨i, hi⟩ := hz
    have hg₀ : g = fun _ ↦ 0 := funext fun j ↦ (hne j i).trans hi
    subst g
    rintro ⟨p, _, hm⟩
    let L := Fatigue.logLik (shifted (fun _ : Fin n ↦ 0) p.1) p.2 b
    obtain ⟨a, ha, ha'⟩ := ((constant_collapse hn hb).eventually
      (eventually_gt_atTop L)).and self_mem_nhdsWithin |>.exists
    have hle : Fatigue.logLik (shifted (fun _ : Fin n ↦ 0) b) a b ≤ L :=
      isMaxOn_iff.mp hm (b, a) ⟨hb, ha'⟩
    exact ha.not_ge hle


private theorem penalty_atTop (n : ℕ) (hn : 0 < n) :
    Tendsto (fun u : ℝ ↦ (u * exp (1 - u)) ^ n) atTop (𝓝 0) := by
  have h := (tendsto_pow_mul_exp_neg_atTop_nhds_zero 1).const_mul (exp 1)
  have h' : Tendsto (fun u : ℝ ↦ u * exp (1 - u)) atTop (𝓝 0) := by
    rw [mul_zero] at h
    apply h.congr
    intro u
    rw [sub_eq_add_neg, exp_add, pow_one]
    ring
  simpa [zero_pow hn.ne'] using h'.pow n

private theorem residual_tendsto {g : Fin n → ℝ} (hn : 0 < n)
    (hg : ∀ i, 0 ≤ g i) {b r : ℝ} (hb : 0 < b) (hr : 0 < r) :
    Tendsto (Q g b) (𝓝 r) (𝓝 (Q g b r)) := by
  let F := fun d : ℝ ↦ Fatigue.avg (fun i ↦ (shifted g d i - b) ^ 2 / shifted g d i) / b
  have hc (i : Fin n) : ContinuousAt (fun d : ℝ ↦ shifted g d i) r :=
    continuousAt_id.add continuousAt_const
  have hF : ContinuousAt F r := by
    unfold F Fatigue.avg
    apply ContinuousAt.div_const
    apply ContinuousAt.div_const
    exact tendsto_finsetSum Finset.univ fun i _ ↦
      ((hc i).sub continuousAt_const |>.pow 2).div (hc i) (shifted_pos hg hr i).ne'
  have he (d : ℝ) (hd : 0 < d) : F d = Q g b d := by
    dsimp [F]
    rw [← bQ_eq hn hg hb hd, mul_comm, mul_div_cancel_right₀ _ hb.ne']
  rw [← he r hr]
  apply hF.tendsto.congr'
  have hpos : ∀ᶠ d in 𝓝 r, 0 < d := Ioi_mem_nhds hr
  exact hpos.mono he

private theorem interior_shape_escape {ι : Type*} {l : Filter ι}
    (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hne : ∃ i j, g i ≠ g j) {b r : ℝ} (hb : 0 < b) (hr : 0 < r)
    {d a : ι → ℝ} (hd : Tendsto d l (𝓝 r)) (ha : ∀ᶠ t in l, 0 < a t)
    (hlim : Tendsto a l (𝓝 0) ∨ Tendsto a l atTop) :
    Tendsto (fun t ↦ squaredLik g b (d t, a t)) l (𝓝 0) := by
  have hd₀ : ∀ᶠ t in l, 0 < d t := hd.eventually (Ioi_mem_nhds hr)
  have hQ := (residual_tendsto hn hg hb hr).comp hd
  have hQ₀ := q_pos hn hg hne hb hr
  have hF := ((profile_continuous hn hg hne hb r hr).continuousAt
    (Ioi_mem_nhds hr)).tendsto.comp hd
  let u := fun t ↦ Q g b (d t) / a t ^ 2
  have hP : Tendsto (fun t ↦ (u t * exp (1 - u t)) ^ n) l (𝓝 0) := by
    rcases hlim with h | h
    · have hin : Tendsto (fun t ↦ (a t ^ 2)⁻¹) l atTop :=
        (tendsto_inv_nhdsGT_zero (𝕜 := ℝ)).comp
          (tendsto_nhdsWithin_iff.mpr ⟨by simpa using h.pow 2,
            ha.mono fun _ ht ↦ sq_pos_of_pos ht⟩)
      have hu : Tendsto u l atTop := by
        simpa [u, div_eq_mul_inv, Function.comp_def] using hQ.pos_mul_atTop hQ₀ hin
      exact (penalty_atTop n hn).comp hu
    · have hin := tendsto_inv_atTop_zero.comp ((tendsto_pow_atTop (by decide : 2 ≠ 0)).comp h)
      have hu : Tendsto u l (𝓝 0) := by
        simpa [u, div_eq_mul_inv, Function.comp_def] using hQ.mul hin
      simpa [zero_pow hn.ne', Function.comp_def] using
        (hu.mul (continuous_exp.continuousAt.tendsto.comp (hu.const_sub 1))).pow n
  have h := hF.mul hP
  rw [mul_zero] at h
  apply h.congr'
  filter_upwards [hd₀, ha] with t hdt hat
  exact (squared_identity hn hg hb hdt hat (q_pos hn hg hne hb hdt)).symm

private theorem constant_threshold_path (hn : 0 < n) {b : ℝ} (hb : 0 < b) :
    Tendsto (fun d ↦ squaredLik (fun _ : Fin n ↦ 0) b
      (d, Fatigue.shape (shifted (fun _ : Fin n ↦ 0) d) b)) (𝓝[>] 0) atTop := by
  apply (constant_edge hn hb).congr'
  have hlt : ∀ᶠ d : ℝ in 𝓝[>] 0, d < b :=
    nhdsWithin_le_nhds (Iio_mem_nhds hb)
  filter_upwards [self_mem_nhdsWithin, hlt] with d (hd : 0 < d) hdb
  have hq : 0 < Q (fun _ : Fin n ↦ 0) b d :=
    (Fatigue.shapeSq_pos_iff hn (shifted_pos (fun _ ↦ le_rfl) hd) hb).mpr
      ⟨⟨0, hn⟩, by simpa [shifted] using hdb.ne⟩
  exact (profile_equality hn (fun _ ↦ le_rfl) hb hd hq).2.symm


private theorem constant_limit_shape_zero {l : Filter ℕ} (hn : 0 < n) {b : ℝ}
    (hb : 0 < b) {d a : ℕ → ℝ} (hd : Tendsto d l (𝓝 b)) (ha : ∀ᶠ t in l, 0 < a t)
    (hL : Tendsto (fun t ↦ squaredLik (fun _ : Fin n ↦ 0) b (d t, a t)) l atTop) :
    Tendsto a l (𝓝 0) := by
  let g : Fin n → ℝ := fun _ ↦ 0
  let C := fun v : ℝ ↦ Fatigue.scaleTerm (shifted g v) b + Fatigue.dataTerm (shifted g v)
  have hC : ContinuousAt C b := by
    have hs : ContinuousAt (fun v : ℝ ↦ log (v + b)) b :=
      (continuousAt_id.add continuousAt_const).log (by simpa using (add_pos hb hb).ne')
    have ht : ContinuousAt (fun v : ℝ ↦ log v) b := continuousAt_id.log (by simpa using hb.ne')
    have he (v : ℝ) : C v = log (v + b) - log b / 2 -
        (3 * log v / 2) - log 2 - log (2 * π) / 2 := by
      have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
      unfold C Fatigue.scaleTerm Fatigue.dataTerm Fatigue.avg
      simp [shifted, g, hn']
      ring
    rw [funext he]
    exact ((((hs.sub continuousAt_const).sub
      ((continuousAt_const (y := (3 : ℝ))).mul ht |>.div_const 2)).sub
      continuousAt_const).sub continuousAt_const : ContinuousAt
        (fun v : ℝ ↦ log (v + b) - log b / 2 - (3 * log v / 2) -
          log 2 - log (2 * π) / 2) b)
  apply Metric.tendsto_nhds.mpr
  intro ε hε
  let M := exp ((2 * n : ℝ) * (C b + 1 - log ε + log 2 + log (2 * π) / 2 + 1/2))
  have hcb : ∀ᶠ t in l, C (d t) < C b + 1 :=
    (hC.tendsto.comp hd).eventually (Iio_mem_nhds (lt_add_of_pos_right _ zero_lt_one))
  filter_upwards [hd.eventually (Ioi_mem_nhds hb), ha, hcb,
    hL.eventually (eventually_gt_atTop M)] with t hdt hat hct hMt
  rw [Real.dist_eq, sub_zero, abs_of_pos hat]
  by_contra! h
  have hq : 0 ≤ Q g b (d t) := by
    unfold Q
    rw [Fatigue.shapeSq_eq_mean_square hn (shifted_pos (fun _ ↦ le_rfl) hdt) hb]
    unfold Fatigue.avg
    exact div_nonneg (Finset.sum_nonneg fun i _ ↦
      div_nonneg (sq_nonneg _) (mul_pos (shifted_pos (fun _ ↦ le_rfl) hdt i) hb).le)
      (by positivity)
  have hlog := log_le_log hε h
  have hneg : 0 ≤ Q g b (d t) / (2 * a t ^ 2) :=
    div_nonneg hq (by positivity)
  have hh : Fatigue.logLik (shifted g (d t)) (a t) b ≤ C b + 1 - log ε := by
    unfold Fatigue.logLik
    change C (d t) - log (a t) - Q g b (d t) / (2 * a t ^ 2) ≤ _
    linarith
  apply hMt.not_ge
  apply exp_le_exp.mpr
  apply mul_le_mul_of_nonneg_left _ (by positivity)
  dsimp only
  linarith only [hh]

/-- A divergent constant-data sequence has a subsequence at the
threshold edge or at the held-scale collapse with shape tending to zero. -/
theorem constant_divergence_subsequence (hn : 0 < n) {b : ℝ} (hb : 0 < b)
    {d a : ℕ → ℝ} (hd : ∀ᶠ t in atTop, 0 < d t) (ha : ∀ᶠ t in atTop, 0 < a t)
    (hL : Tendsto (fun t ↦ squaredLik (fun _ : Fin n ↦ 0) b (d t, a t)) atTop atTop) :
    ∃ φ : ℕ → ℕ, StrictMono φ ∧
      (Tendsto (d ∘ φ) atTop (𝓝 0) ∨
        (Tendsto (d ∘ φ) atTop (𝓝 b) ∧ Tendsto (a ∘ φ) atTop (𝓝 0))) := by
  let g : Fin n → ℝ := fun _ ↦ 0
  have hg : ∀ i, 0 ≤ g i := fun _ ↦ le_rfl
  have hq {v : ℝ} (hv : 0 < v) (hne : v ≠ b) : 0 < Q g b v :=
    (Fatigue.shapeSq_pos_iff hn (shifted_pos hg hv) hb).mpr
      ⟨⟨0, hn⟩, by simpa [shifted, g] using hne⟩
  have hnd : ¬ Tendsto d atTop atTop := by
    intro h
    have hupper := (infinite_threshold hn hg hb).comp h
    have hgt := h.eventually (eventually_gt_atTop b)
    have hlo := hL.eventually (eventually_gt_atTop 1)
    have hhi := hupper.eventually (Iio_mem_nhds zero_lt_one)
    obtain ⟨t, hdt, hat, hbt, hlot, hhit⟩ :=
      (hd.and (ha.and (hgt.and (hlo.and hhi)))).exists
    exact (hlot.trans_le ((joint_envelope hn hg hb hdt (hq hdt hbt.ne')).1 _ hat)).not_gt hhit
  rw [tendsto_atTop] at hnd
  push Not at hnd
  obtain ⟨B, hB⟩ := hnd
  have hbox : ∃ᶠ t in atTop, d t ∈ Icc 0 B :=
    (hB.and_eventually hd).mono fun _ ⟨hlt, hpos⟩ ↦ ⟨hpos.le, hlt.le⟩
  obtain ⟨r, hr, φ, hφ, h⟩ := isCompact_Icc.tendsto_subseq' hbox
  refine ⟨φ, hφ, ?_⟩
  rcases hr.1.eq_or_lt with he | hr₀
  · exact Or.inl (he ▸ h)
  · have hL' := hL.comp hφ.tendsto_atTop
    have ha' := hφ.tendsto_atTop.eventually ha
    have he : r = b := by
      by_contra! he
      have hupper := (profile_continuousAt hn hg hb hr₀ (hq hr₀ he)).tendsto.comp h
      let A := rationalProfile g b r + 1
      have hhi := hupper.eventually (Iio_mem_nhds (show rationalProfile g b r < A by
        dsimp [A]; linarith))
      have hlo := hL'.eventually (eventually_gt_atTop A)
      have hD : ∀ᶠ t in atTop, d (φ t) ∈ Ioi 0 ∩ {b}ᶜ :=
        h.eventually (inter_mem (Ioi_mem_nhds hr₀) (isClosed_singleton.isOpen_compl.mem_nhds he))
      obtain ⟨t, ⟨hd', hd''⟩, hat, hlt, hgt⟩ := (hD.and (ha'.and (hhi.and hlo))).exists
      exact (hgt.trans_le ((joint_envelope hn hg hb hd' (hq hd' hd'')).1 _ hat)).not_gt hlt
    subst r
    exact Or.inr ⟨h, constant_limit_shape_zero hn hb h ha' hL'⟩


/-- A competitive nonconstant sequence has a subsequence at an interior
point or at threshold contact. Infinity in threshold and the two shape
ends at a regular threshold have zero envelope. -/
theorem competitive_subsequence (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hne : ∃ i j, g i ≠ g j) {b : ℝ} (hb : 0 < b)
    {d a : ℕ → ℝ} {M : ℝ} (hM : 0 < M)
    (hd : ∀ᶠ t in atTop, 0 < d t) (ha : ∀ᶠ t in atTop, 0 < a t)
    (hL : ∀ᶠ t in atTop, M ≤ squaredLik g b (d t, a t)) :
    ∃ φ : ℕ → ℕ, StrictMono φ ∧
      (Tendsto (d ∘ φ) atTop (𝓝 0) ∨
        ∃ d₀ > 0, ∃ a₀ > 0, Tendsto (d ∘ φ) atTop (𝓝 d₀) ∧
          Tendsto (a ∘ φ) atTop (𝓝 a₀)) := by
  have hupper : ∀ᶠ t in atTop, M ≤ rationalProfile g b (d t) := by
    filter_upwards [hd, ha, hL] with t hdt hat ht
    exact ht.trans ((joint_envelope hn hg hb hdt (q_pos hn hg hne hb hdt)).1 _ hat)
  obtain ⟨B, hB⟩ := eventually_atTop.mp
    ((infinite_threshold hn hg hb).eventually (Iio_mem_nhds hM))
  have hbox : ∀ᶠ t in atTop, d t ∈ Icc 0 B := by
    filter_upwards [hd, hupper] with t hdt hlt
    exact ⟨hdt.le, (lt_of_not_ge (fun h ↦ (hB _ h).not_ge hlt)).le⟩
  obtain ⟨d₀, hd₀, φ, hφ, hdlim⟩ := isCompact_Icc.tendsto_subseq' hbox.frequently
  rcases hd₀.1.eq_or_lt with he | he
  · exact ⟨φ, hφ, Or.inl (he ▸ hdlim)⟩
  have hL' := hφ.tendsto_atTop.eventually hL
  have ha' := hφ.tendsto_atTop.eventually ha
  have hnzero : ∀ {l : Filter ℕ} [l.NeBot] {a' d' : ℕ → ℝ},
      Tendsto d' l (𝓝 d₀) → (∀ᶠ t in l, 0 < a' t) →
      (∀ᶠ t in l, M ≤ squaredLik g b (d' t, a' t)) →
      ¬ (Tendsto a' l (𝓝 0) ∨ Tendsto a' l atTop) := by
    intro l _ a' d' da aa LL h
    have hzero := interior_shape_escape hn hg hne hb he da aa h
    exact (not_le_of_gt hM) (le_of_tendsto_of_tendsto tendsto_const_nhds hzero LL)
  have hnI : ¬ Tendsto (a ∘ φ) atTop atTop := fun h ↦ hnzero hdlim ha' hL' (Or.inr h)
  rw [tendsto_atTop] at hnI
  push Not at hnI
  obtain ⟨A, hA⟩ := hnI
  have hA' : ∃ᶠ t in atTop, (a ∘ φ) t ∈ Icc 0 A :=
    (hA.and_eventually ha').mono fun _ ⟨hlt, hpos⟩ ↦ ⟨hpos.le, hlt.le⟩
  obtain ⟨a₀, ha₀, ψ, hψ, halim⟩ := isCompact_Icc.tendsto_subseq' hA'
  have hda := hdlim.comp hψ.tendsto_atTop
  have haa := hψ.tendsto_atTop.eventually ha'
  have hLL := hψ.tendsto_atTop.eventually hL'
  have hp : 0 < a₀ := lt_of_le_of_ne ha₀.1 fun h ↦
    hnzero hda haa hLL (Or.inl (h ▸ halim))
  exact ⟨φ ∘ ψ, hφ.comp hψ, Or.inr ⟨d₀, he, a₀, hp, hda, halim⟩⟩

/-- Held-scale escape envelopes. For nonconstants the only threshold end
with a nonzero envelope is contact, in the equal and supercritical
multiplicity cases. A constant sample has both divergent paths. -/
theorem held_beta_failure (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    (∀ d > 0, rationalProfile g b d =
      edgeFactor g b d * (d ^ n / d ^ (3 * multiplicity g))) ∧
    Tendsto (rationalProfile g b) atTop (𝓝 0) ∧
    Tendsto (fun d ↦ Fatigue.shape (shifted g d) b) (𝓝[>] 0) atTop ∧
    (3 * multiplicity g < n → Tendsto (rationalProfile g b) (𝓝[>] 0) (𝓝 0)) ∧
    (3 * multiplicity g = n → 0 < edgeFactor g b 0 ∧
      Tendsto (rationalProfile g b) (𝓝[>] 0) (𝓝 (edgeFactor g b 0))) ∧
    (n < 3 * multiplicity g → Tendsto (rationalProfile g b) (𝓝[>] 0) atTop) ∧
    ((∃ i j, g i ≠ g j) →
      (∀ d > 0, (∀ a > 0, squaredLik g b (d, a) ≤ rationalProfile g b d) ∧
        ∃ a > 0, squaredLik g b (d, a) = rationalProfile g b d) ∧
      (∀ {ι : Type} {l : Filter ι} {d a : ι → ℝ} {r : ℝ}, 0 < r →
        Tendsto d l (𝓝 r) → (∀ᶠ t in l, 0 < a t) →
        (Tendsto a l (𝓝 0) ∨ Tendsto a l atTop) →
        Tendsto (fun t ↦ squaredLik g b (d t, a t)) l (𝓝 0)) ∧
      ∀ {d a : ℕ → ℝ} {M : ℝ}, 0 < M →
        (∀ᶠ t in atTop, 0 < d t) → (∀ᶠ t in atTop, 0 < a t) →
        (∀ᶠ t in atTop, M ≤ squaredLik g b (d t, a t)) →
        ∃ φ : ℕ → ℕ, StrictMono φ ∧
          (Tendsto (d ∘ φ) atTop (𝓝 0) ∨
            ∃ d₀ > 0, ∃ a₀ > 0, Tendsto (d ∘ φ) atTop (𝓝 d₀) ∧
              Tendsto (a ∘ φ) atTop (𝓝 a₀))) ∧
    ((∀ i, g i = 0) →
      Tendsto (fun a ↦ Fatigue.logLik (shifted g b) a b) (𝓝[>] 0) atTop ∧
      Tendsto (fun d ↦ squaredLik g b (d, Fatigue.shape (shifted g d) b))
        (𝓝[>] 0) atTop ∧
      ∀ {d a : ℕ → ℝ}, (∀ᶠ t in atTop, 0 < d t) → (∀ᶠ t in atTop, 0 < a t) →
        Tendsto (fun t ↦ squaredLik g b (d t, a t)) atTop atTop →
        ∃ φ : ℕ → ℕ, StrictMono φ ∧
          (Tendsto (d ∘ φ) atTop (𝓝 0) ∨
            (Tendsto (d ∘ φ) atTop (𝓝 b) ∧ Tendsto (a ∘ φ) atTop (𝓝 0)))) := by
  refine ⟨fun _ hd ↦ edge_factorization hn hg hb hd,
    infinite_threshold hn hg hb, threshold_shape_diverges hn hg hz hb,
    edge_small hn hg hz hb,
    fun h ↦ ⟨(trace_regular hg hz hb).2, edge_equal hn hg hz hb h⟩,
    edge_large hn hg hz hb, ?_, ?_⟩
  · intro hne
    exact ⟨fun _ hd ↦ joint_envelope hn hg hb hd (q_pos hn hg hne hb hd),
      (fun hr hd ha hlim ↦ interior_shape_escape hn hg hne hb hr hd ha hlim),
      fun hM hd ha hL ↦ competitive_subsequence hn hg hne hb hM hd ha hL⟩
  · intro he
    have h : g = fun _ ↦ 0 := funext he
    subst g
    exact ⟨constant_collapse hn hb, constant_threshold_path hn hb,
      constant_divergence_subsequence hn hb⟩


end

end UnivariateML.PGWFatigue

end
