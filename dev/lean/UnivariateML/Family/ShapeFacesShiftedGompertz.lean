module

public import UnivariateML.Family.Weibull
public import UnivariateML.Generic.Score

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! Shifted Gompertz attainment and the included exponential member.

## Deviations

The local result uses
the strict inward-score condition directly, with a separate implication
from the crossing-rate condition.
-/

@[expose] public section

open Real Set Filter
open scoped Topology
namespace UnivariateML.ShapeFacesShiftedGompertz
noncomputable section

variable {n : ℕ}

def survivalSum (x : Fin n → ℝ) (b : ℝ) : ℝ := ∑ i, exp (-b * x i)

def logLik (x : Fin n → ℝ) (b η : ℝ) : ℝ :=
  (n : ℝ) * log b - b * (∑ i, x i) - η * survivalSum x b +
    ∑ i, log (1 + η * (1 - exp (-b * x i)))

def exponentialRate (x : Fin n → ℝ) : ℝ := (n : ℝ) / ∑ i, x i

theorem held_rate_zero_strict {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {b : ℝ} (hb : 0 < b) (hscore : (n : ℝ) ≤ 2 * survivalSum x b)
    {η : ℝ} (hη : 0 < η) (hn : 0 < n) :
    logLik x b η < logLik x b 0 := by
  have hterm (i : Fin n) :
      log (1 + η * (1 - exp (-b * x i))) ≤ η * (1 - exp (-b * x i)) := by
    have hv : exp (-b * x i) ≤ 1 := exp_le_one_iff.mpr (by nlinarith [hx i])
    have ht : 0 ≤ η * (1 - exp (-b * x i)) := mul_nonneg hη.le (sub_nonneg.mpr hv)
    have h := Real.log_le_sub_one_of_pos (show 0 < 1 + η * (1 - exp (-b * x i)) by linarith)
    linarith
  have hv : 0 < survivalSum x b := by
    apply Finset.sum_pos (fun _ _ ↦ exp_pos _)
    exact ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hstrict : (∑ i, log (1 + η * (1 - exp (-b * x i)))) < η * survivalSum x b := by
    by_cases hxpos : ∃ i, 0 < x i
    · obtain ⟨j, hj⟩ := hxpos
      have ht : 0 < η * (1 - exp (-b * x j)) :=
        mul_pos hη (sub_pos.mpr (exp_lt_one_iff.mpr (by nlinarith)))
      have h := Real.log_lt_sub_one_of_pos
        (show 0 < 1 + η * (1 - exp (-b * x j)) by linarith)
        (show 1 + η * (1 - exp (-b * x j)) ≠ 1 by linarith)
      have hs := Finset.sum_lt_sum (fun i (_ : i ∈ Finset.univ) ↦ hterm i)
        ⟨j, Finset.mem_univ _, by linarith⟩
      rw [← Finset.mul_sum, Finset.sum_sub_distrib] at hs
      simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, mul_one] at hs
      have hbnd := mul_le_mul_of_nonneg_left hscore hη.le
      unfold survivalSum at hbnd ⊢
      linarith
    · have hz (i : Fin n) : x i = 0 := le_antisymm (le_of_not_gt (fun h ↦ hxpos ⟨i, h⟩)) (hx i)
      simp only [hz, mul_zero, exp_zero, sub_self, add_zero, log_one, Finset.sum_const_zero]
      exact mul_pos hη hv
  simp only [logLik, zero_mul, sub_zero, add_zero, log_one, Finset.sum_const_zero]
  linarith


theorem held_rate_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {b : ℝ} (hb : 0 < b) :
    ∃ η ∈ Ici (0 : ℝ), IsMaxOn (logLik x b) (Ici 0) η ∧
      (∀ t ∈ Ici (0 : ℝ), IsMaxOn (logLik x b) (Ici 0) t → t = η) ∧
      (η = 0 ↔ (n : ℝ) ≤ 2 * survivalSum x b) := by
  by_cases h : (n : ℝ) ≤ 2 * survivalSum x b
  · refine ⟨0, (show (0 : ℝ) ≤ 0 from le_rfl), ?_, ?_, by simp [h]⟩
    · intro t ht
      change 0 ≤ t at ht
      obtain he | hp := eq_or_lt_of_le ht
      · subst t
        exact show logLik x b 0 ≤ logLik x b 0 from le_rfl
      · exact (held_rate_zero_strict hx hb h hp hn).le
    · intro t ht hm
      by_contra hne
      exact (held_rate_zero_strict hx hb h (lt_of_le_of_ne ht (Ne.symm hne)) hn).not_ge
        (hm (show (0 : ℝ) ∈ Ici 0 from (show (0 : ℝ) ≤ 0 from le_rfl)))
  let a : Fin n → ℝ := fun i ↦ 1 - exp (-b * x i)
  let V := survivalSum x b
  let score := fun t : ℝ ↦ (∑ i, a i / (1 + t * a i)) - V
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hV : 0 < V := Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have ha (i : Fin n) : 0 ≤ a i :=
    sub_nonneg.mpr (exp_le_one_iff.mpr (by nlinarith [hx i]))
  have hp (i : Fin n) {t : ℝ} (ht : t ∈ Ici 0) : 0 < 1 + t * a i :=
    add_pos_of_pos_of_nonneg zero_lt_one (mul_nonneg ht (ha i))
  have hsumA : ∑ i, a i = (n : ℝ) - V := by
    dsimp [a, V, survivalSum]
    simp [Finset.sum_sub_distrib]
  have hzero : 0 < score 0 := by
    dsimp [score]
    simp only [zero_mul, add_zero, div_one]
    rw [hsumA]
    change ¬ (n : ℝ) ≤ 2 * V at h
    linarith
  have hpos : ∃ j, 0 < a j := by
    by_contra! hz
    have hs := Finset.sum_nonpos (fun i (_ : i ∈ Finset.univ) ↦ hz i)
    change ¬ (n : ℝ) ≤ 2 * V at h
    linarith
  have hanti : StrictAntiOn score (Ici 0) := by
    intro r hr t ht hrt
    apply sub_lt_sub_right
    obtain ⟨j, hj⟩ := hpos
    apply Finset.sum_lt_sum
    · intro i _
      exact div_le_div_of_nonneg_left (ha i) (hp i hr)
        (add_le_add_right (mul_le_mul_of_nonneg_right hrt.le (ha i)) _)
    · exact ⟨j, Finset.mem_univ _, div_lt_div_of_pos_left hj (hp j hr)
        (add_lt_add_right (mul_lt_mul_of_pos_right hrt hj) _)⟩
  have hd (t : ℝ) (ht : t ∈ Ici 0) : HasDerivAt (logLik x b) (score t) t := by
    have hterm (i : Fin n) : HasDerivAt (fun r ↦ log (1 + r * a i))
        (a i / (1 + t * a i)) t := by
      simpa only [one_mul, id_eq] using
        ((((hasDerivAt_id t).mul_const (a i)).const_add 1).log (hp i ht).ne')
    have hd' := ((hasDerivAt_const t ((n : ℝ) * log b - b * (∑ i, x i))).sub
      ((hasDerivAt_id t).mul_const V)).add (HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ hterm i))
    convert hd' using 1
    · rfl
    · dsimp [score]
      ring
  have hcont : ContinuousOn score (Ici 0) := by
    apply ContinuousOn.sub
    · have hc (i : Fin n) : ContinuousOn (fun t ↦ a i / (1 + t * a i)) (Ici 0) :=
        continuousOn_const.div
          ((continuous_const.add (continuous_id.mul_const (a i))).continuousOn)
          (fun _ ht ↦ (hp i ht).ne')
      exact continuousOn_finsetSum Finset.univ (fun i _ ↦ hc i)
    · exact continuousOn_const
  let t := 2 * (n : ℝ) / V
  have ht : 0 < t := by positivity
  have hterm (i : Fin n) : a i / (1 + t * a i) ≤ 1 / t := by
    apply (div_le_div_iff₀ (hp i ht.le) ht).2
    nlinarith
  have hbound : (∑ i, a i / (1 + t * a i)) ≤ V / 2 := by
    calc
      _ ≤ ∑ _ : Fin n, 1 / t := Finset.sum_le_sum (fun i _ ↦ hterm i)
      _ = V / 2 := by
        simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
        dsimp [t]
        field_simp
  have hneg : score t < 0 := by dsimp [score]; linarith
  obtain ⟨⟨r, hr, hu⟩, hm⟩ := Generic.existsUnique_score_zero_of_signs
    (convex_Ici (0 : ℝ)) hd hcont hanti (show (0 : ℝ) ∈ Ici 0 from (show (0 : ℝ) ≤ 0 from le_rfl))
    (show t ∈ Ici 0 from ht.le) ht.le hzero hneg
  have hrne : r ≠ 0 := fun he ↦ hzero.ne' (he ▸ hr.2)
  exact ⟨r, hr.1, (hm r hr.1).2 hr.2,
    fun q hq hqmax ↦ hu q ⟨hq, (hm q hq).1 hqmax⟩, by simp [hrne, h]⟩

theorem exponential_strict {x : Fin n → ℝ} (hn : 0 < n) (hS : 0 < ∑ i, x i)
    {b : ℝ} (hb : 0 < b) (hne : b ≠ exponentialRate x) :
    logLik x b 0 < logLik x (exponentialRate x) 0 := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let b₀ := exponentialRate x
  have hb₀ : 0 < b₀ := div_pos hn' hS
  have hprod : b₀ * (∑ i, x i) = n := div_mul_cancel₀ _ hS.ne'
  have h := Real.log_lt_sub_one_of_pos (div_pos hb hb₀)
    (fun hr ↦ hne ((div_eq_one_iff_eq hb₀.ne').mp hr))
  rw [log_div hb.ne' hb₀.ne'] at h
  have h' := mul_lt_mul_of_pos_left h hn'
  have hright : (n : ℝ) * (b / b₀ - 1) = b * (∑ i, x i) - n := by
    calc
      _ = (n : ℝ) * (b / b₀) - n := by ring
      _ = _ := by rw [← hprod]; field_simp
  rw [hright] at h'
  simp only [logLik, zero_mul, sub_zero, add_zero, log_one, Finset.sum_const_zero]
  change (n : ℝ) * log b - b * (∑ i, x i) < (n : ℝ) * log b₀ - b₀ * (∑ i, x i)
  linarith

theorem all_zero_rate_unbounded (hn : 0 < n) (η : ℝ) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ 0) b η) atTop atTop := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have he (b : ℝ) : logLik (fun _ : Fin n ↦ 0) b η =
      (n : ℝ) * log b + -(η * n) := by simp [logLik, survivalSum, sub_eq_add_neg]
  simp only [he]
  exact tendsto_atTop_add_const_right _ _ (tendsto_log_atTop.const_mul_atTop hn')

theorem exponential_strict_local {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    (hn : 0 < n) (hS : 0 < ∑ i, x i)
    (hscore : (n : ℝ) < 2 * survivalSum x (exponentialRate x)) :
    ∀ᶠ b in 𝓝 (exponentialRate x), ∀ η ≥ 0,
      (b, η) ≠ (exponentialRate x, 0) →
      logLik x b η < logLik x (exponentialRate x) 0 := by
  have hcont : Continuous (fun b ↦ 2 * survivalSum x b) := by
    unfold survivalSum
    fun_prop
  have hb₀ : 0 < exponentialRate x := div_pos (by exact_mod_cast hn) hS
  have hs := hcont.continuousAt (eventually_gt_nhds hscore)
  filter_upwards [eventually_gt_nhds hb₀, hs] with b hb hs
  intro η hη hne
  have hbase : logLik x b 0 ≤ logLik x (exponentialRate x) 0 := by
    obtain rfl | h := eq_or_ne b (exponentialRate x)
    · exact le_rfl
    · exact (exponential_strict hn hS hb h).le
  obtain rfl | h := eq_or_lt_of_le hη
  · exact exponential_strict hn hS hb (fun he ↦ hne (Prod.ext he rfl))
  · exact (held_rate_zero_strict hx hb hs.le h hn).trans_le hbase

theorem exponential_strict_local_below_crossing {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    (hn : 0 < n) (hS : 0 < ∑ i, x i) {β : ℝ}
    (hβ : (n : ℝ) = 2 * survivalSum x β) (hb : exponentialRate x < β) :
    ∀ᶠ b in 𝓝 (exponentialRate x), ∀ η ≥ 0,
      (b, η) ≠ (exponentialRate x, 0) →
      logLik x b η < logLik x (exponentialRate x) 0 := by
  apply exponential_strict_local hx hn hS
  rw [hβ]
  apply mul_lt_mul_of_pos_left _ (by norm_num)
  unfold survivalSum
  apply Finset.sum_lt_sum
  · intro i _
    exact exp_le_exp.mpr (by nlinarith [mul_nonneg (sub_nonneg.mpr hb.le) (hx i)])
  · have hpos : ∃ i, 0 < x i := by
      by_contra! h
      have hs := Finset.sum_nonpos (fun i (_ : i ∈ Finset.univ) ↦ h i)
      exact hS.not_ge hs
    obtain ⟨j, hj⟩ := hpos
    exact ⟨j, Finset.mem_univ _, exp_lt_exp.mpr (by nlinarith [mul_pos (sub_pos.mpr hb) hj])⟩


private def rateCorrection (x η b : ℝ) : ℝ :=
  x * exp (-b * x) * (1 + (1 + η * (1 - exp (-b * x)))⁻¹)

private def rateScore (x : Fin n → ℝ) (η b : ℝ) : ℝ :=
  (n : ℝ) / b - (∑ i, x i) + η * ∑ i, rateCorrection (x i) η b

private theorem rate_den_ge_one {x η b : ℝ} (hx : 0 ≤ x) (hη : 0 ≤ η) (hb : 0 ≤ b) :
    1 ≤ 1 + η * (1 - exp (-b * x)) := by
  have he : exp (-b * x) ≤ 1 := exp_le_one_iff.mpr (by nlinarith [mul_nonneg hb hx])
  linarith [mul_nonneg hη (sub_nonneg.mpr he)]

private theorem rate_hasDerivAt {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {η b : ℝ} (hη : 0 ≤ η) (hb : 0 < b) :
    HasDerivAt (fun t ↦ logLik x t η) (rateScore x η b) b := by
  have hdv (i : Fin n) : HasDerivAt (fun t ↦ exp (-t * x i))
      (-x i * exp (-b * x i)) b := by
    convert (((hasDerivAt_id b).neg.mul_const (x i)).exp) using 1
    · rfl
    · change -x i * exp (-b * x i) = exp (-b * x i) * (-1 * x i)
      ring
  have hterm (i : Fin n) : HasDerivAt
      (fun t ↦ -η * exp (-t * x i) + log (1 + η * (1 - exp (-t * x i))))
      (η * rateCorrection (x i) η b) b := by
    have hp : 0 < 1 + η * (1 - exp (-b * x i)) :=
      zero_lt_one.trans_le (rate_den_ge_one (hx i) hη hb.le)
    convert (((hdv i).const_mul (-η)).add
      (((((hdv i).const_sub 1).const_mul η).const_add 1).log hp.ne')) using 1
    unfold rateCorrection
    ring
  have hf : (fun t ↦ logLik x t η) =
      fun t ↦ (n : ℝ) * log t - t * (∑ i, x i) +
        ∑ i, (-η * exp (-t * x i) + log (1 + η * (1 - exp (-t * x i)))) := by
    funext t
    simp only [logLik, survivalSum, Finset.sum_add_distrib, ← Finset.mul_sum]
    ring
  rw [hf]
  convert (((hasDerivAt_log hb.ne').const_mul (n : ℝ)).sub
    ((hasDerivAt_id b).mul_const (∑ i, x i))).add
    (HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ hterm i) using 1
  · rfl
  · unfold rateScore
    rw [← Finset.mul_sum]
    simp only [div_eq_mul_inv, one_mul]

private theorem correction_bounds {x η b : ℝ} (hx : 0 ≤ x) (hη : 0 ≤ η) (hb : 0 < b) :
    0 ≤ rateCorrection x η b ∧ rateCorrection x η b ≤ 2 / b := by
  have hd := rate_den_ge_one hx hη hb.le
  have hp : 0 < 1 + η * (1 - exp (-b * x)) := zero_lt_one.trans_le hd
  have hi : (1 + η * (1 - exp (-b * x)))⁻¹ ≤ 1 := inv_le_one_of_one_le₀ hd
  have h₁ : x * exp (-b * x) ≤ 1 / b := by
    apply (le_div_iff₀ hb).2
    have h := mul_le_mul_of_nonneg_right (add_one_le_exp (b * x)) (exp_pos (-b * x)).le
    rw [← exp_add, show b * x + -b * x = 0 by ring, exp_zero] at h
    nlinarith [exp_pos (-b * x)]
  refine ⟨mul_nonneg (mul_nonneg hx (exp_pos _).le) (by positivity), ?_⟩
  calc
    _ ≤ x * exp (-b * x) * 2 := by
      apply mul_le_mul_of_nonneg_left _ (mul_nonneg hx (exp_pos _).le)
      linarith
    _ ≤ 1 / b * 2 := mul_le_mul_of_nonneg_right h₁ (by norm_num)
    _ = _ := by ring

private theorem correction_anti {x η s t : ℝ} (hx : 0 ≤ x) (hη : 0 ≤ η)
    (hs : 0 ≤ s) (hst : s ≤ t) : rateCorrection x η t ≤ rateCorrection x η s := by
  have hv : exp (-t * x) ≤ exp (-s * x) :=
    exp_le_exp.mpr (by nlinarith [mul_le_mul_of_nonneg_right hst hx])
  have hd := rate_den_ge_one hx hη hs
  have hd' : 1 + η * (1 - exp (-s * x)) ≤ 1 + η * (1 - exp (-t * x)) := by
    nlinarith [mul_le_mul_of_nonneg_left hv hη]
  have hp : 0 < 1 + η * (1 - exp (-s * x)) := zero_lt_one.trans_le hd
  have hp' : 0 < 1 + η * (1 - exp (-t * x)) := hp.trans_le hd'
  unfold rateCorrection
  apply mul_le_mul (mul_le_mul_of_nonneg_left hv hx) _ (by positivity)
    (mul_nonneg hx (exp_pos _).le)
  exact add_le_add_right (inv_anti₀ hp hd') _

theorem held_shape_rate_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {η : ℝ} (hη : 0 ≤ η) (hS : 0 < ∑ i, x i) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t η) (Ioi 0) b := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let S := ∑ i, x i
  let a := (n : ℝ) / (2 * S)
  let C := (n : ℝ) + η * (n * 2)
  let b := 2 * C / S
  have hC : 0 < C := by dsimp [C]; positivity
  have ha : 0 < a := by dsimp [a]; positivity
  have hb : 0 < b := by dsimp [b]; positivity
  have hsa : 0 < rateScore x η a := by
    have hs : 0 ≤ ∑ i, rateCorrection (x i) η a :=
      Finset.sum_nonneg (fun i _ ↦ (correction_bounds (hx i) hη ha).1)
    have hfrac : (n : ℝ) / a = 2 * S := by dsimp [a]; field_simp
    unfold rateScore
    rw [hfrac]
    change 0 < 2 * S - S + η * ∑ i, rateCorrection (x i) η a
    linarith [mul_nonneg hη hs]
  have hsb : rateScore x η b < 0 := by
    have hs : (∑ i, rateCorrection (x i) η b) ≤ (n : ℝ) * (2 / b) := by
      simpa only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] using
        Finset.sum_le_sum (s := Finset.univ)
          (fun i _ ↦ (correction_bounds (hx i) hη hb).2)
    have hfrac : C / b = S / 2 := by dsimp [b]; field_simp
    have he : (n : ℝ) / b + η * ((n : ℝ) * (2 / b)) = C / b := by dsimp [C]; ring
    unfold rateScore
    calc
      _ ≤ C / b - S := by linarith [mul_le_mul_of_nonneg_left hs hη, he]
      _ < 0 := by rw [hfrac]; linarith
  have hanti : StrictAntiOn (rateScore x η) (Ioi 0) := by
    intro s hs t ht hst
    apply add_lt_add_of_lt_of_le
      (sub_lt_sub_right (div_lt_div_of_pos_left hN hs hst) _)
    exact mul_le_mul_of_nonneg_left (Finset.sum_le_sum
      fun i _ ↦ correction_anti (hx i) hη (show 0 ≤ s from le_of_lt hs) hst.le) hη
  have hcont : ContinuousOn (rateScore x η) (Ioi 0) := by
    have hv (i : Fin n) : Continuous (fun t : ℝ ↦ exp (-t * x i)) :=
      continuous_exp.comp (continuous_id.neg.mul_const _)
    have hd (i : Fin n) : ContinuousOn (fun t ↦ 1 + η * (1 - exp (-t * x i))) (Ioi 0) :=
      (continuous_const.add (continuous_const.mul (continuous_const.sub (hv i)))).continuousOn
    have hr (i : Fin n) : ContinuousOn (rateCorrection (x i) η) (Ioi 0) :=
      (continuous_const.mul (hv i)).continuousOn.mul
        (continuousOn_const.add (hd i |>.inv₀ fun t ht ↦
          (zero_lt_one.trans_le (rate_den_ge_one (hx i) hη (le_of_lt ht))).ne'))
    have hf : ContinuousOn (fun t : ℝ ↦ (n : ℝ) / t) (Ioi 0) :=
      continuousOn_const.div continuousOn_id (fun t ht ↦ (show 0 < t from ht).ne')
    have hs : ContinuousOn (fun t ↦ ∑ i, rateCorrection (x i) η t) (Ioi 0) :=
      continuousOn_finsetSum Finset.univ fun i _ ↦ hr i
    exact (hf.sub continuousOn_const).add (hs.const_mul η)
  have hab : a ≤ b := by
    by_contra! h
    exact (hanti hb ha h).not_gt (hsb.trans hsa)
  obtain ⟨⟨r, hr, hu⟩, hm⟩ := Generic.existsUnique_score_zero_of_signs
    (convex_Ioi (0 : ℝ)) (fun _ ht ↦ rate_hasDerivAt hx hη ht) hcont hanti
    (show a ∈ Ioi 0 from ha) (show b ∈ Ioi 0 from hb) hab hsa hsb
  exact ⟨r, ⟨hr.1, (hm r hr.1).2 hr.2⟩, fun q hq ↦ hu q ⟨hq.1, (hm q hq.1).1 hq.2⟩⟩


def jointDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ici 0

private theorem joint_continuous {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i) :
    ContinuousOn (fun p : ℝ × ℝ ↦ logLik x p.1 p.2) jointDomain := by
  have hv (i : Fin n) : Continuous (fun p : ℝ × ℝ ↦ exp (-p.1 * x i)) :=
    continuous_exp.comp (continuous_fst.neg.mul_const _)
  have hV : Continuous (fun p : ℝ × ℝ ↦ survivalSum x p.1) :=
    continuous_finsetSum _ (fun i _ ↦ hv i)
  have hl (i : Fin n) : ContinuousOn
      (fun p : ℝ × ℝ ↦ log (1 + p.2 * (1 - exp (-p.1 * x i)))) jointDomain := by
    apply (continuous_const.add (continuous_snd.mul (continuous_const.sub (hv i)))).continuousOn.log
    intro p hp
    exact (zero_lt_one.trans_le (rate_den_ge_one (hx i) hp.2
      (show 0 < p.1 from hp.1).le)).ne'
  have hlog : ContinuousOn (fun p : ℝ × ℝ ↦ log p.1) jointDomain :=
    continuous_fst.continuousOn.log (fun p hp ↦ (show 0 < p.1 from hp.1).ne')
  exact (((hlog.const_mul (n : ℝ)).sub (continuous_fst.mul_const _).continuousOn).sub
      (continuous_snd.mul hV).continuousOn).add
    (continuousOn_finsetSum _ (fun i _ ↦ hl i))

private theorem sum_log_le {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {b η : ℝ} (hb : 0 < b) (hη : 0 ≤ η) :
    (∑ i, log (1 + η * (1 - exp (-b * x i)))) ≤ (n : ℝ) * log (1 + η) := by
  have h (i : Fin n) : log (1 + η * (1 - exp (-b * x i))) ≤ log (1 + η) := by
    apply log_le_log (zero_lt_one.trans_le (rate_den_ge_one (hx i) hη hb.le))
    linarith [mul_nonneg hη (exp_pos (-b * x i)).le]
  simpa only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] using
    Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ h i)

private theorem survival_ge {x : Fin n → ℝ} (b : ℝ) (i : Fin n) :
    exp (-b * x i) ≤ survivalSum x b :=
  Finset.single_le_sum (f := fun k ↦ exp (-b * x k))
    (fun _ _ ↦ (exp_pos _).le) (Finset.mem_univ i)

private theorem joint_rate_bound (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 ≤ x i) (j : Fin n) {b η : ℝ} (hb : 0 < b) (hη : 0 ≤ η) :
    logLik x b η ≤ (n : ℝ) * log b - b * ((∑ i, x i) - n * x j) + n * log n := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let V := survivalSum x b
  have hV : 0 < V := (exp_pos (-b * x j)).trans_le (survival_ge b j)
  have hVN : V ≤ n := by
    change survivalSum x b ≤ (n : ℝ)
    have h (i : Fin n) : exp (-b * x i) ≤ 1 :=
      exp_le_one_iff.mpr (by nlinarith [mul_nonneg hb.le (hx i)])
    simpa only [survivalSum, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
      nsmul_eq_mul, mul_one] using Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ h i)
  have hlog := log_le_sub_one_of_pos (show 0 < (1 + η) * (V / n) by positivity)
  rw [log_mul (by positivity) (div_pos hV hN).ne', log_div hV.ne' hN.ne'] at hlog
  have hlin := mul_le_mul_of_nonneg_left hlog hN.le
  have he : (n : ℝ) * ((1 + η) * (V / n)) = (1 + η) * V := by field_simp
  have hlin' : (n : ℝ) * log (1 + η) - η * V ≤ -(n : ℝ) * log V + n * log n := by
    have hh : (n : ℝ) * ((1 + η) * (V / n) - 1) = (1 + η) * V - n := by
      rw [mul_sub, he, mul_one]
    rw [hh] at hlin
    nlinarith only [hlin, hVN]
  have hlogV : -b * x j ≤ log V := by
    rw [← log_exp (-b * x j)]
    exact log_le_log (exp_pos _) (survival_ge b j)
  have hsum := sum_log_le hx hb hη
  unfold logLik
  change _ - η * V + _ ≤ _
  nlinarith only [hsum, hlin', mul_le_mul_of_nonneg_left hlogV hN.le]

private theorem joint_superlevel (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 ≤ x i) (hne : ∃ i j, x i ≠ x j) (M : ℝ) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ jointDomain ∧
      ∀ p ∈ jointDomain, M ≤ logLik x p.1 p.2 → p ∈ K := by
  obtain ⟨j, _, hj⟩ := Finset.exists_min_image Finset.univ x ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hmin (i : Fin n) : x j ≤ x i := hj i (Finset.mem_univ _)
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let G := (∑ i, x i) - (n : ℝ) * x j
  have hG : 0 < G := by
    obtain ⟨l, hl⟩ : ∃ l, x j < x l := by
      by_contra! h
      obtain ⟨i, k, hik⟩ := hne
      exact hik ((le_antisymm (h i) (hmin i)).trans (le_antisymm (h k) (hmin k)).symm)
    have hs := Finset.sum_lt_sum (fun i (_ : i ∈ Finset.univ) ↦ hmin i)
      ⟨l, Finset.mem_univ _, hl⟩
    simpa [G, sub_pos, nsmul_eq_mul] using hs
  let d := G / (2 * n)
  have hd : 0 < d := div_pos hG (by positivity)
  let A := exp ((M - (n : ℝ) * log n) / n)
  let B := max 1 (((n : ℝ) * (log n - log d - 1) - M) / ((n : ℝ) * d))
  let V₀ := exp (-B * x j)
  let e := V₀ / (2 * n)
  have hV₀ : 0 < V₀ := exp_pos _
  have he : 0 < e := div_pos hV₀ (by positivity)
  let H := ((n : ℝ) * (log B + e - 1 - log e) - M) / ((n : ℝ) * e)
  refine ⟨Icc A B ×ˢ Icc 0 H, isCompact_Icc.prod isCompact_Icc, ?_, ?_⟩
  · intro p hp
    exact ⟨(exp_pos _).trans_le hp.1.1, hp.2.1⟩
  · intro p hp hM
    have hb : 0 < p.1 := hp.1
    have hη : 0 ≤ p.2 := hp.2
    have hU := joint_rate_bound hn hx j hb hη
    change logLik x p.1 p.2 ≤ (n : ℝ) * log p.1 - p.1 * G + n * log n at hU
    have hlo : A ≤ p.1 := by
      rw [← exp_log hb]
      apply exp_le_exp.mpr
      apply (div_le_iff₀ hN).2
      nlinarith only [hU, hM, mul_nonneg hb.le hG.le]
    have hhi : p.1 ≤ B := by
      apply le_max_of_le_right
      apply (le_div_iff₀ (mul_pos hN hd)).2
      have hlog := log_le_sub_one_of_pos (mul_pos hd hb)
      rw [log_mul hd.ne' hb.ne'] at hlog
      have hd' : 2 * (n : ℝ) * d = G := by dsimp [d]; field_simp
      nlinarith only [hM, hU, mul_le_mul_of_nonneg_left hlog hN.le,
        congrArg (fun v ↦ p.1 * v) hd']
    have hV : V₀ ≤ survivalSum x p.1 :=
      (exp_le_exp.mpr (by nlinarith [mul_le_mul_of_nonneg_right hhi (hx j)])).trans
        (survival_ge p.1 j)
    have hlog := log_le_sub_one_of_pos (show 0 < e * (1 + p.2) by positivity)
    rw [log_mul he.ne' (by positivity)] at hlog
    have hsum := sum_log_le hx hb hη
    have hR := mul_le_mul_of_nonneg_left (log_le_log hb hhi) hN.le
    have hS : 0 ≤ ∑ i, x i := Finset.sum_nonneg (fun i _ ↦ hx i)
    have he' : 2 * (n : ℝ) * e = V₀ := by dsimp [e]; field_simp
    refine ⟨⟨hlo, hhi⟩, hη, ?_⟩
    apply (le_div_iff₀ (mul_pos hN he)).2
    unfold logLik at hM
    nlinarith only [hM, hsum, hR, mul_nonneg hb.le hS,
      mul_le_mul_of_nonneg_left hlog hN.le, mul_le_mul_of_nonneg_left hV hη,
      congrArg (fun v ↦ p.2 * v) he']

theorem joint_attains (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 ≤ x i) (hne : ∃ i j, x i ≠ x j) :
    ∃ p ∈ jointDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) jointDomain p := by
  have hw : (1, 0) ∈ jointDomain := by norm_num [jointDomain]
  obtain ⟨K, hK, hsub, hlevel⟩ := joint_superlevel hn hx hne (logLik x 1 0)
  have hwK := hlevel (1, 0) hw (by rfl)
  obtain ⟨p, hp, hm⟩ := hK.exists_isMaxOn ⟨(1, 0), hwK⟩
    ((joint_continuous hx).mono hsub)
  change ∀ q ∈ K, logLik x q.1 q.2 ≤ logLik x p.1 p.2 at hm
  refine ⟨p, hsub hp, fun q hq ↦ ?_⟩
  by_cases h : logLik x 1 0 ≤ logLik x q.1 q.2
  · exact hm q (hlevel q hq h)
  · change logLik x q.1 q.2 ≤ logLik x p.1 p.2
    have hc : logLik x 1 0 ≤ logLik x p.1 p.2 := hm (1, 0) hwK
    exact (not_le.mp h).le.trans hc

theorem joint_constant_unbounded (hn : 0 < n) (v : ℝ) (_hv : 0 ≤ v) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ v) b (exp (b * v))) atTop atTop := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  have he (b : ℝ) : exp (b * v) * exp (-b * v) = 1 := by
    rw [← exp_add, show b * v + -b * v = 0 by ring, exp_zero]
  have hlog (b : ℝ) : log (1 + exp (b * v) * (1 - exp (-b * v))) = b * v := by
    rw [mul_sub, mul_one, he]
    have h : 1 + (exp (b * v) - 1) = exp (b * v) := by ring
    rw [h, log_exp]
  have h (b : ℝ) : logLik (fun _ : Fin n ↦ v) b (exp (b * v)) =
      (n : ℝ) * log b + -(n : ℝ) := by
    simp only [logLik, survivalSum, hlog, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul]
    nlinarith only [congrArg (fun z ↦ (n : ℝ) * z) (he b)]
  simp only [h]
  exact tendsto_atTop_add_const_right _ _ (tendsto_log_atTop.const_mul_atTop hN)

end
end UnivariateML.ShapeFacesShiftedGompertz

end
