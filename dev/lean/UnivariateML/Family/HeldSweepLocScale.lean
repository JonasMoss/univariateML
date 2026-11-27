module

public import UnivariateML.Family.LocScaleInstances

import Mathlib.Tactic

set_option linter.style.header false

/-! # Held logistic and Gumbel fits -/

@[expose] public section

open Set Real Filter UnivariateML.LocationScale UnivariateML.LocScaleInstances
open scoped Topology

namespace UnivariateML.HeldSweepLocScale

private theorem logistic_bound (t : ℝ) : |t| ≤ logisticLoss t := by
  unfold logisticLoss
  by_cases ht : 0 ≤ t
  · rw [abs_of_nonneg ht]
    have h := log_nonneg (show 1 ≤ 1 + exp (-t) by linarith only [exp_pos (-t)])
    linarith
  · rw [abs_of_neg (lt_of_not_ge ht)]
    have h := log_le_log (exp_pos (-t)) (show exp (-t) ≤ 1 + exp (-t) by linarith)
    rw [log_exp] at h
    linarith

private theorem gumbel_bound (t : ℝ) : |t| ≤ gumbelLoss t := by
  unfold gumbelLoss
  by_cases ht : 0 ≤ t
  · rw [abs_of_nonneg ht]
    linarith [exp_pos (-t)]
  · rw [abs_of_neg (lt_of_not_ge ht)]
    have h : 1 + -t / 2 ≤ exp (-t / 2) := by linarith [add_one_le_exp (-t / 2)]
    have hs := pow_le_pow_left₀ (show 0 ≤ 1 + -t / 2 by linarith) h 2
    rw [← exp_nat_mul] at hs
    norm_num only [Nat.cast_ofNat] at hs
    rw [show (2 : ℝ) * (-t / 2) = -t by ring] at hs
    nlinarith [sq_nonneg (-t / 2 - 1)]

private theorem logistic_strict : StrictConvexOn ℝ univ logisticLoss := by
  have hd (t : ℝ) : HasDerivAt logisticLoss (1 - 2 / (1 + exp t)) t := by
    have he : HasDerivAt (fun z : ℝ ↦ exp (-z)) (-exp (-t)) t := by
      simpa using ((hasDerivAt_id t).neg.exp)
    have h := (hasDerivAt_id t).add (((he.const_add 1).log (by positivity)).const_mul 2)
    change HasDerivAt logisticLoss (1 + 2 * (-exp (-t) / (1 + exp (-t)))) t at h
    rw [exp_neg] at h
    convert h using 1
    field_simp [(exp_pos t).ne', show 1 + exp t ≠ 0 by positivity,
      show 1 + (exp t)⁻¹ ≠ 0 by positivity]
    ring
  apply StrictMono.strictConvexOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr fun t ↦ (hd t).continuousAt)
  intro t u htu
  rw [(hd t).deriv, (hd u).deriv]
  have h := div_lt_div_of_pos_left (show (0 : ℝ) < 2 by norm_num)
    (show 0 < 1 + exp t by positivity)
    (show 1 + exp t < 1 + exp u by linarith only [exp_lt_exp.mpr htu])
  linarith

private theorem gumbel_strict : StrictConvexOn ℝ univ gumbelLoss := by
  have hd (t : ℝ) : HasDerivAt gumbelLoss (1 - exp (-t)) t := by
    have he : HasDerivAt (fun z : ℝ ↦ exp (-z)) (-exp (-t)) t := by
      simpa using ((hasDerivAt_id t).neg.exp)
    rw [sub_eq_add_neg]
    exact (hasDerivAt_id t).add he
  apply StrictMono.strictConvexOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr fun t ↦ (hd t).continuousAt)
  intro t u htu
  rw [(hd t).deriv, (hd u).deriv]
  exact sub_lt_sub_left (exp_lt_exp.mpr (neg_lt_neg htu)) 1

private theorem cap_of_single {n : ℕ} {g : ℝ → ℝ} (hg : ∀ y, |y| ≤ g y)
    (r : Fin n → ℝ) (i : Fin n) :
    |r i| ≤ ∑ j, g (r j) :=
  (hg _).trans (Finset.single_le_sum
    (fun j _ ↦ (abs_nonneg _).trans (hg (r j))) (Finset.mem_univ i))

private theorem working_fiber_unique {n : ℕ} (hn : 0 < n) {g : ℝ → ℝ}
    (hg : StrictConvexOn ℝ univ g) (x : Fin n → ℝ)
    {D : Set ℝ} (hD : Convex ℝ D) {f : ℝ → ℝ × ℝ}
    (hf : MapsTo f D inverseDomain) (hi : Function.Injective f)
    (haff : ∀ a b u v : ℝ, u + v = 1 → f (u * a + v * b) = u • f a + v • f b)
    (ha : ∃ a ∈ D, IsMaxOn (inverseLogLik g x ∘ f) D a) :
    ∃! a, a ∈ D ∧ IsMaxOn (inverseLogLik g x ∘ f) D a := by
  have hs : StrictConcaveOn ℝ D (inverseLogLik g x ∘ f) := by
    refine ⟨hD, fun a ha b hb hab u v hu hv huv ↦ ?_⟩
    change u * inverseLogLik g x (f a) + v * inverseLogLik g x (f b) <
      inverseLogLik g x (f (u * a + v * b))
    rw [haff a b u v huv]
    exact (strictConcave_inverseLogLik hg x hn).2 (hf ha) (hf hb) (hi.ne hab) hu hv huv
  obtain ⟨a, ha, hm⟩ := ha
  exact ⟨a, ⟨ha, hm⟩, fun b hb ↦ hs.eq_of_isMaxOn hb.2 hm hb.1 ha⟩

private theorem held_scale_working {n : ℕ} (hn : 0 < n) {g : ℝ → ℝ}
    (hcont : Continuous g) (hstrict : StrictConvexOn ℝ univ g)
    (hbound : ∀ y, |y| ≤ g y) (x : Fin n → ℝ) {B : ℝ} (hB : 0 < B) :
    ∃! A, A ∈ (univ : Set ℝ) ∧
      IsMaxOn (fun a ↦ inverseLogLik g x (B, a)) univ A := by
  let f := fun a ↦ inverseLogLik g x (B, a)
  refine working_fiber_unique hn hstrict x convex_univ
    (fun _ _ ↦ ⟨hB, mem_univ _⟩) (fun _ _ h ↦ congrArg Prod.snd h) ?_ ?_
  · intro a b u v huv
    ext <;> simp only [Prod.smul_fst, Prod.smul_snd, Prod.fst_add, Prod.snd_add, smul_eq_mul]
    linear_combination -B * huv
  · let i : Fin n := ⟨0, hn⟩
    let c := B * x i
    let R := max (|c| + 1) ((n : ℝ) * log B - f 0 + |c| + 1)
    have hR : 0 ≤ R := (by positivity : 0 ≤ |c| + 1).trans (le_max_left _ _)
    have hcut (a : ℝ) (ha : R < |a|) : f a < f 0 := by
      have h := cap_of_single hbound (fun j ↦ B * x j - a) i
      have ht := abs_sub_abs_le_abs_sub a c
      have hR' := le_max_right (|c| + 1) ((n : ℝ) * log B - f 0 + |c| + 1)
      change |c - a| ≤ _ at h
      rw [abs_sub_comm a c] at ht
      change inverseLogLik g x (B, a) < f 0
      simp only [inverseLogLik]
      simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
        Fintype.card_fin, nsmul_eq_mul]
      linarith
    have hc : Continuous f := by
      unfold f inverseLogLik
      apply continuous_finsetSum
      intro j _
      change Continuous (fun a : ℝ ↦ log B - g (B * x j - a))
      exact continuous_const.sub (hcont.comp (continuous_const.sub continuous_id))
    obtain ⟨a, ha, hm⟩ := isCompact_Icc.exists_isMaxOn
      ⟨0, neg_nonpos.mpr hR, hR⟩ hc.continuousOn
    refine ⟨a, mem_univ _, fun b _ ↦ ?_⟩
    by_cases hb : b ∈ Icc (-R) R
    · exact hm hb
    · have hlt : R < |b| := lt_of_not_ge (fun h ↦ hb (abs_le.mp h))
      exact (hcut b hlt).le.trans (hm (show 0 ∈ Icc (-R) R from ⟨neg_nonpos.mpr hR, hR⟩))

/-- The held-scale logistic fit has a unique scaled location. -/
theorem logistic_held_scale {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {B : ℝ} (hB : 0 < B) :
    ∃! A, A ∈ (univ : Set ℝ) ∧
      IsMaxOn (fun a ↦ inverseLogLik logisticLoss x (B, a)) univ A :=
  held_scale_working hn (by
    have hc : Continuous (fun x : ℝ ↦ 1 + exp (-x)) := by fun_prop
    exact continuous_id.add ((hc.log (fun x ↦ by positivity)).const_mul 2))
    logistic_strict logistic_bound x hB

/-- The held-scale Gumbel fit has a unique scaled location. -/
theorem gumbel_held_scale {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {B : ℝ} (hB : 0 < B) :
    ∃! A, A ∈ (univ : Set ℝ) ∧
      IsMaxOn (fun a ↦ inverseLogLik gumbelLoss x (B, a)) univ A :=
  held_scale_working hn (by unfold gumbelLoss; fun_prop) gumbel_strict gumbel_bound x hB

private theorem held_location_working {n : ℕ} (hn : 0 < n) {g : ℝ → ℝ}
    (hcont : Continuous g) (hstrict : StrictConvexOn ℝ univ g)
    (hbound : ∀ y, |y| ≤ g y) (x : Fin n → ℝ) (m : ℝ) :
    (∃! B, B ∈ Ioi (0 : ℝ) ∧
      IsMaxOn (fun b ↦ inverseLogLik g x (b, m * b)) (Ioi 0) B) ↔ ∃ i, x i ≠ m := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let f := fun b ↦ inverseLogLik g x (b, m * b)
  have he (b : ℝ) : f b = (n : ℝ) * log b - ∑ i, g (b * (x i - m)) := by
    simp only [f, inverseLogLik, Finset.sum_sub_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    congr 1
    apply Finset.sum_congr rfl
    intro i _
    congr 1
    ring
  constructor
  · rintro ⟨b, ⟨hb, hmax⟩, _⟩
    by_contra! hx
    have hb' : 0 < b := hb
    have h := hmax (show 2 * b ∈ Ioi (0 : ℝ) from mul_pos (by norm_num) hb')
    change f (2 * b) ≤ f b at h
    simp only [he, hx, sub_self, mul_zero] at h
    exact ((mul_lt_mul_of_pos_left (log_lt_log hb' (by linarith)) hn').not_ge
      ((sub_le_sub_iff_right _).mp h))
  · rintro ⟨i, hi⟩
    refine working_fiber_unique hn hstrict x (convex_Ioi 0)
      (fun _ hb ↦ ⟨hb, mem_univ _⟩) (fun _ _ h ↦ congrArg Prod.fst h) ?_ ?_
    · intro a b u v _
      ext <;> simp only [Prod.smul_fst, Prod.smul_snd, Prod.fst_add, Prod.snd_add, smul_eq_mul]
      ring
    · let d := |x i - m|
      have hd : 0 < d := abs_pos.mpr (sub_ne_zero.mpr hi)
      have hcut (b : ℝ) (hb : 0 < b) : f b ≤ (n : ℝ) * log b - d * b := by
        rw [he]
        apply sub_le_sub_left
        have hh := cap_of_single hbound (fun j ↦ b * (x j - m)) i
        simpa only [abs_mul, abs_of_pos hb, d, mul_comm b] using hh
      let a := min 1 (exp (f 1 / (n : ℝ)))
      have ha : 0 < a := lt_min one_pos (exp_pos _)
      have ha1 : a ≤ 1 := min_le_left _ _
      have hlow (b : ℝ) (hb : 0 < b) (hba : b < a) : f b ≤ f 1 := by
        have hh := log_lt_log hb (hba.trans_le (min_le_right 1 (exp (f 1 / n))))
        rw [log_exp] at hh
        have hlog := (lt_div_iff₀ hn').mp hh
        nlinarith only [hcut b hb, hlog, mul_pos hd hb]
      have hl : Tendsto (fun b : ℝ ↦ (n : ℝ) * (log b / b)) atTop (𝓝 0) := by
        have hh : Tendsto (fun b : ℝ ↦ log b / b) atTop (𝓝 0) :=
          (tendsto_pow_log_div_mul_add_atTop (1 : ℝ) 0 1 one_ne_zero).congr (fun b ↦ by simp)
        simpa only [mul_zero] using hh.const_mul (n : ℝ)
      obtain ⟨R, hR⟩ := eventually_atTop.mp
        (hl.eventually (Iio_mem_nhds (show 0 < d / 2 by positivity)))
      let B := max 1 (max R (-(f 1) / (d / 2)))
      have hB : 1 ≤ B := le_max_left _ _
      have hhigh (b : ℝ) (hb : B < b) : f b ≤ f 1 := by
        have hp : 0 < b := one_pos.trans (hB.trans_lt hb)
        have hr : R ≤ b := (le_max_left _ _).trans ((le_max_right _ _).trans hb.le)
        have hlb : -(f 1) / (d / 2) ≤ b :=
          (le_max_right _ _).trans ((le_max_right _ _).trans hb.le)
        have h := mul_lt_mul_of_pos_right (hR b hr) hp
        have hcancel : ((n : ℝ) * (log b / b)) * b = (n : ℝ) * log b := by field_simp
        rw [hcancel] at h
        have hlast := (div_le_iff₀ (show 0 < d / 2 by positivity)).mp hlb
        linarith only [hcut b hp, h, hlast]
      have hc : ContinuousOn f (Icc a B) := by
        rw [show f = (fun b ↦ (n : ℝ) * log b - ∑ i, g (b * (x i - m))) from funext he]
        apply ContinuousOn.sub
        · exact continuousOn_const.mul (continuousOn_log.mono (fun b hb ↦ (ha.trans_le hb.1).ne'))
        · exact (continuous_finsetSum _ (fun i _ ↦ hcont.comp
            (continuous_id.mul continuous_const))).continuousOn
      obtain ⟨b, hb, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1, ha1, hB⟩ hc
      refine ⟨b, ha.trans_le hb.1, fun t ht ↦ ?_⟩
      have href := hm (show 1 ∈ Icc a B from ⟨ha1, hB⟩)
      by_cases hlo : t < a
      · exact (hlow t ht hlo).trans href
      by_cases hhi : B < t
      · exact (hhigh t hhi).trans href
      exact hm (show t ∈ Icc a B from ⟨le_of_not_gt hlo, le_of_not_gt hhi⟩)

/-- With location held, the logistic scale has a unique maximum exactly
when some observation differs from the supplied location. -/
theorem logistic_held_location {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    (∃! B, B ∈ Ioi (0 : ℝ) ∧
      IsMaxOn (fun b ↦ inverseLogLik logisticLoss x (b, m * b)) (Ioi 0) B) ↔
        ∃ i, x i ≠ m :=
  held_location_working hn (by
    have hc : Continuous (fun x : ℝ ↦ 1 + exp (-x)) := by fun_prop
    exact continuous_id.add ((hc.log (fun x ↦ by positivity)).const_mul 2))
    logistic_strict logistic_bound x m

/-- With location held, the Gumbel scale has a unique maximum exactly
when some observation differs from the supplied location. -/
theorem gumbel_held_location {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    (∃! B, B ∈ Ioi (0 : ℝ) ∧
      IsMaxOn (fun b ↦ inverseLogLik gumbelLoss x (b, m * b)) (Ioi 0) B) ↔
        ∃ i, x i ≠ m :=
  held_location_working hn (by unfold gumbelLoss; fun_prop) gumbel_strict gumbel_bound x m

private theorem constant_location_collapse {n : ℕ} (hn : 0 < n) (g : ℝ → ℝ)
    (m : ℝ) : Tendsto (fun b ↦ inverseLogLik g (fun _ : Fin n ↦ m) (b, m * b))
      atTop atTop := by
  have he (b : ℝ) :
      inverseLogLik g (fun _ : Fin n ↦ m) (b, m * b) = (n : ℝ) * log b - (n : ℝ) * g 0 := by
    simp only [inverseLogLik, mul_comm b m, sub_self, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul]
    ring
  simp only [he, sub_eq_add_neg]
  exact tendsto_atTop_add_const_right _ _ (tendsto_log_atTop.const_mul_atTop (by exact_mod_cast hn))

/-- A constant sample at held logistic location has unbounded likelihood as scale shrinks. -/
theorem logistic_held_location_collapse {n : ℕ} (hn : 0 < n) (m : ℝ) :
    Tendsto (fun b ↦ inverseLogLik logisticLoss (fun _ : Fin n ↦ m) (b, m * b)) atTop atTop :=
  constant_location_collapse hn logisticLoss m

/-- A constant sample at held Gumbel location has unbounded likelihood as scale shrinks. -/
theorem gumbel_held_location_collapse {n : ℕ} (hn : 0 < n) (m : ℝ) :
    Tendsto (fun b ↦ inverseLogLik gumbelLoss (fun _ : Fin n ↦ m) (b, m * b)) atTop atTop :=
  constant_location_collapse hn gumbelLoss m

end UnivariateML.HeldSweepLocScale

end
