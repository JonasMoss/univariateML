module

public import UnivariateML.Family.Shape2BetaPrime

set_option linter.style.header false

/-!
GB2 fits with held power and scale or with both beta shapes held.

## Deviations

Both beta shapes free are treated at held scale and power or in the all-free constant
branch. The single-held-beta-shape face comparisons and ties require additional results.
-/

@[expose] public section

open Real Set Filter

namespace UnivariateML.Shape2GB2

open Shape2BetaPrime

noncomputable section

variable {n : ℕ}

/-- The beta shape pair is in lower, upper order (shape3, shape1).
The power is shape2. -/
def logLik (x : Fin n → ℝ) (s b : ℝ) (sh : ℝ × ℝ) : ℝ :=
  ∑ i, (log b - log (x i) - Beta.logBeta sh -
    logisticLoss sh.1 sh.2 (b * log (x i / s)))

private def powered (x : Fin n → ℝ) (s b : ℝ) : Fin n → ℝ :=
  fun i ↦ exp (b * log (x i / s))

private theorem logLik_powered (x : Fin n → ℝ) (s b : ℝ) (sh : ℝ × ℝ) :
    logLik x s b sh = BetaPrime.logLik (powered x s b) 1 sh +
      ∑ i, (log b - log (x i) + log (powered x s b i)) := by
  rw [BetaPrime.logLik, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  simp only [div_one, log_one, sub_zero, powered, log_exp, logisticLoss]
  ring

/-- Positive held scale and power with the upper beta shape held leave
a unique lower beta shape for every nonempty positive sample. -/
theorem held_scale_power_upper_mle (hn : 0 < n) (x : Fin n → ℝ)
    {s b q : ℝ} (_hx : ∀ i, 0 < x i) (_hs : 0 < s) (_hb : 0 < b) (hq : 0 < q) :
    ∃! p, p ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x s b (t, q)) (Ioi 0) p := by
  apply (Transfer.existsUnique_max_iff (g := fun p ↦ BetaPrime.logLik (powered x s b) 1 (p, q))
    (bijOn_id (Ioi (0 : ℝ))) (fun p _ ↦ logLik_powered x s b (p, q))).2
  exact BetaPrime.heldSecond hn one_pos (fun _ ↦ exp_pos _) hq

/-- Positive held scale and power with the lower beta shape held leave
a unique upper beta shape for every nonempty positive sample. -/
theorem held_scale_power_lower_mle (hn : 0 < n) (x : Fin n → ℝ)
    {s b p : ℝ} (_hx : ∀ i, 0 < x i) (_hs : 0 < s) (_hb : 0 < b) (hp : 0 < p) :
    ∃! q, q ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x s b (p, t)) (Ioi 0) q := by
  apply (Transfer.existsUnique_max_iff (g := fun q ↦ BetaPrime.logLik (powered x s b) 1 (p, q))
    (bijOn_id (Ioi (0 : ℝ))) (fun q _ ↦ logLik_powered x s b (p, q))).2
  exact BetaPrime.heldFirst hn one_pos (fun _ ↦ exp_pos _) hp

/-- Held positive scale and power give a unique pair of beta shapes
on nonconstant data. On constant data beta concentration is unbounded. -/
theorem held_scale_power_classification (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {s b : ℝ} (hs : 0 < s) (hb : 0 < b) :
    ((∃ i j, x i ≠ x j) →
      ∃! sh, sh ∈ Beta.domain ∧ IsMaxOn (logLik x s b) Beta.domain sh) ∧
    ((∀ i j, x i = x j) → ¬ BddAbove (logLik x s b '' Beta.domain)) := by
  let y := powered x s b
  have h : ∀ sh ∈ Beta.domain, logLik x s b sh = BetaPrime.logLik y 1 sh +
      ∑ i, (log b - log (x i) + log (y i)) := fun sh _ ↦ logLik_powered x s b sh
  constructor
  · rintro ⟨i, j, hij⟩
    have hey : y i ≠ y j := by
      intro he
      have he' := (mul_right_inj' hb.ne').1 (exp_injective he)
      have ht := log_injOn_pos (div_pos (hx i) hs) (div_pos (hx j) hs) he'
      exact hij ((div_left_inj₀ hs.ne').1 ht)
    exact (Transfer.existsUnique_max_iff (bijOn_id Beta.domain) h).2
      ((BetaPrime.jointMLE_iff hn one_pos (fun _ ↦ exp_pos _)).2 ⟨i, j, hey⟩)
  · intro he hB
    apply BetaPrime.joint_unbounded (x := y) hn one_pos (fun _ ↦ exp_pos _)
    · rintro ⟨i, j, hij⟩
      exact hij (by simp only [y, powered, he i j])
    · obtain ⟨M, hM⟩ := hB
      refine ⟨M - ∑ i, (log b - log (x i) + log (y i)), ?_⟩
      rintro _ ⟨sh, hsh, rfl⟩
      have hle := hM (mem_image_of_mem (logLik x s b) hsh)
      rw [h sh hsh] at hle
      linarith only [hle]

/-- At any two positive beta shape holds, increasing the power at the
common observation as scale gives unbounded likelihood. -/
theorem constant_concentration (hn : 0 < n) {t : ℝ} (ht : 0 < t)
    (sh : ℝ × ℝ) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ t) t b sh) atTop atTop := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have he (b : ℝ) : logLik (fun _ : Fin n ↦ t) t b sh =
      (n : ℝ) * log b + (n : ℝ) * (-log t - Beta.logBeta sh - logisticLoss sh.1 sh.2 0) := by
    simp [logLik, ht.ne', nsmul_eq_mul]
    ring
  simp only [he]
  exact tendsto_atTop_add_const_right _ _ (tendsto_log_atTop.const_mul_atTop hN)

/-- With two positive beta shapes held, the free power and scale have
a unique optimum on every nonconstant positive sample. -/
theorem held_beta_shapes_mle {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    ∃! u : ℝ × ℝ, u ∈ Beta.domain ∧
      IsMaxOn (fun v ↦ logLik x v.2 v.1 (p, q)) Beta.domain u := by
  let y := fun i ↦ log (x i)
  let φ := fun u : ℝ × ℝ ↦ (log u.2, u.1⁻¹)
  have hbij : BijOn φ Beta.domain LocationScale.domain := by
    refine ⟨fun u hu ↦ ⟨mem_univ _, inv_pos.mpr hu.1⟩, ?_, ?_⟩
    · intro u hu v hv h
      exact Prod.ext (inv_injective (congrArg Prod.snd h))
        (log_injOn_pos hu.2 hv.2 (congrArg Prod.fst h))
    · intro v hv
      exact ⟨(v.2⁻¹, exp v.1), ⟨inv_pos.mpr hv.2, exp_pos _⟩,
        Prod.ext (log_exp _) (inv_inv _)⟩
  have he (u : ℝ × ℝ) (hu : u ∈ Beta.domain) :
      logLik x u.2 u.1 (p, q) = LocationScale.logLik (logisticLoss p q) y (φ u) +
        ∑ i, (-log (x i) - Beta.logBeta (p, q)) := by
    have hs : 0 < u.2 := hu.2
    have hz (i : Fin n) : u.1 * log (x i / u.2) = (y i - (φ u).1) / (φ u).2 := by
      simp only [log_div (hx i).ne' hs.ne', y, φ, div_inv_eq_mul, mul_comm]
    simp only [logLik, hz, LocationScale.logLik, φ, log_inv, neg_neg,
      ← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro i _
    ring
  have hy : LocationScale.Nonconstant y := by
    obtain ⟨i, j, hij⟩ := hne
    exact ⟨i, j, fun he ↦ hij (log_injOn_pos (hx i) (hx j) he)⟩
  obtain ⟨hc, hl⟩ := beta_logistic_loss_geometry hp hq
  exact (Transfer.existsUnique_max_iff hbij he).2
    (LocationScale.existsUnique_mle hc
      ⟨min p q, lt_min hp hq, 0, fun z ↦ by simpa only [sub_zero] using hl z⟩ hy)


/-- Every all-free constant positive sample has unbounded likelihood
over the admissible parameter space. -/
theorem all_free_constant_unbounded (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    ¬ BddAbove {v | ∃ s > 0, ∃ b > 0, ∃ sh ∈ Beta.domain,
      logLik (fun _ : Fin n ↦ t) s b sh = v} := by
  rintro ⟨M, hM⟩
  have h := (constant_concentration hn ht (1, 1)).eventually (eventually_gt_atTop M)
  obtain ⟨b, hb, hbM⟩ := ((eventually_gt_atTop (0 : ℝ)).and h).exists
  exact hbM.not_ge (hM ⟨t, ht, b, hb, (1, 1), ⟨one_pos, one_pos⟩, rfl⟩)

end

end UnivariateML.Shape2GB2

end
