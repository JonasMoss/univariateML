module

public import UnivariateML.Family.LocScaleInstances
public import UnivariateML.Family.Transfers
public import Mathlib.Analysis.SpecialFunctions.Pow.Real

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Log-logistic likelihood

The coefficients are shape `β` and scale `α`, in that order. Taking logs
of the observations gives the logistic location–scale likelihood up to
the sum of the log Jacobians.

## Deviations

Held-scale existence requires an observation different from the held
scale, even for a constant sample. The all-equal exception is unbounded.
The results concern exact real arithmetic with a nonempty sample for
each maximization claim. They use strict concavity instead of a Hessian
test and do not analyze floating-point certification.
-/

@[expose] public section

open Set Real
open UnivariateML.LocationScale (Nonconstant)
open UnivariateML.LocScaleInstances (logisticLoss)

namespace UnivariateML.LogLogistic

noncomputable section

variable {n : ℕ}

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

def logDensity (β α t : ℝ) : ℝ :=
  log β - log α + (β - 1) * log (t / α) - 2 * log (1 + (t / α) ^ β)

def logLik (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, logDensity p.1 p.2 (x i)

/-- Map (shape, scale) to logistic (location, scale) on the log data. -/
def toLogistic (p : ℝ × ℝ) : ℝ × ℝ := (log p.2, p.1⁻¹)

/-- Every feasible logistic pair has a unique feasible log-logistic preimage. -/
theorem toLogistic_bijOn : BijOn toLogistic domain LocationScale.domain := by
  refine ⟨fun p hp ↦ ⟨mem_univ _, inv_pos.mpr hp.1⟩, ?_, ?_⟩
  · intro p hp q hq h
    exact Prod.ext (inv_injective (congrArg Prod.snd h))
      (log_injOn_pos hp.2 hq.2 (congrArg Prod.fst h))
  · rintro ⟨μ, σ⟩ hp
    exact ⟨(σ⁻¹, exp μ), ⟨inv_pos.mpr hp.2, exp_pos _⟩,
      Prod.ext (log_exp _) (inv_inv _)⟩

private theorem logisticLoss_symm (t : ℝ) :
    logisticLoss t = -t + 2 * log (1 + exp t) := by
  have he : 1 + exp t = exp t * (1 + exp (-t)) := by
    rw [mul_add, mul_one, ← exp_add, add_neg_cancel, exp_zero, add_comm]
  conv_rhs => rw [he, log_mul (exp_pos _).ne' (by positivity), log_exp]
  unfold logisticLoss
  ring

/-- Taking logs of the observations contributes the data-only log Jacobian. -/
theorem logLik_transfer {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p : ℝ × ℝ} (hp : p ∈ domain) :
    logLik x p =
      LocationScale.logLik logisticLoss (fun i ↦ log (x i)) (toLogistic p) -
        ∑ i, log (x i) := by
  unfold logLik LocationScale.logLik
  rw [← Finset.sum_sub_distrib]
  apply Finset.sum_congr rfl
  intro i _
  have ha : 0 < p.2 := hp.2
  unfold logDensity toLogistic
  simp only [log_inv, div_inv_eq_mul]
  rw [log_div (hx i).ne' ha.ne', ← exp_log (rpow_pos_of_pos (div_pos (hx i) ha) p.1),
    log_rpow (div_pos (hx i) ha), log_div (hx i).ne' ha.ne', logisticLoss_symm]
  rw [mul_comm (log (x i) - log p.2) p.1]
  ring

/-- A nonconstant positive sample has a unique joint MLE. -/
theorem jointMLE {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (hne : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p := by
  apply (Transfer.existsUnique_max_iff toLogistic_bijOn
    (fun p hp ↦ (logLik_transfer hx hp).trans (sub_eq_add_neg _ _))).mpr
  obtain ⟨i, j, hij⟩ := hne
  exact LocScaleInstances.logistic_existsUnique_mle
    ⟨i, j, fun h ↦ hij (log_injOn_pos (hx i) (hx j) h)⟩

/-- At the common observation, a held scale permits unbounded concentration. -/
theorem heldScale_unbounded (hn : 0 < n) {α : ℝ} (ha : 0 < α) :
    ¬ BddAbove ((fun β ↦ logLik (fun _ : Fin n ↦ α) (β, α)) '' Ioi 0) := by
  rintro ⟨u, hu⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let β := exp (u / n + log α + 2 * log 2 + 1)
  have hb : β ∈ Ioi (0 : ℝ) := exp_pos _
  have h := hu (mem_image_of_mem _ hb)
  change logLik (fun _ : Fin n ↦ α) (β, α) ≤ u at h
  simp only [logLik, logDensity, div_self ha.ne', log_one, mul_zero, add_zero, one_rpow,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, β, log_exp] at h
  norm_num at h
  have he : (n : ℝ) * (u / n) = u := by field_simp
  linarith

/-- Free shape and scale give an unbounded likelihood on constant data. -/
theorem constant_unbounded (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    ¬ BddAbove (logLik (fun _ : Fin n ↦ t) '' domain) := by
  intro h
  apply heldScale_unbounded hn ht
  apply h.mono
  rintro _ ⟨β, hb, rfl⟩
  exact ⟨(β, t), ⟨hb, ht⟩, rfl⟩

private theorem logistic_deriv (t : ℝ) :
    HasDerivAt logisticLoss (1 - 2 / (1 + exp t)) t := by
  rw [show logisticLoss = (fun z ↦ -z + 2 * log (1 + exp z)) from
    funext logisticLoss_symm]
  have hh := (hasDerivAt_id t).neg.add
    ((((hasDerivAt_id t).exp.const_add 1).log (by positivity)).const_mul 2)
  change HasDerivAt (fun z : ℝ ↦ -z + 2 * log (1 + exp z))
    (-1 + 2 * (exp t * 1 / (1 + exp t))) t at hh
  convert hh using 1
  field_simp [show 1 + exp t ≠ 0 by positivity]
  ring

private theorem logistic_strict : StrictConvexOn ℝ univ logisticLoss := by
  apply StrictMono.strictConvexOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr fun t ↦ (logistic_deriv t).continuousAt)
  intro x y hxy
  rw [(logistic_deriv x).deriv, (logistic_deriv y).deriv]
  have h := div_lt_div_of_pos_left (show (0 : ℝ) < 2 by norm_num)
    (show 0 < 1 + exp x by positivity)
    (show 1 + exp x < 1 + exp y by linarith only [exp_lt_exp.mpr hxy])
  exact sub_lt_sub_left h 1

private theorem abs_le_logisticLoss (t : ℝ) : |t| ≤ logisticLoss t := by
  rcases le_total 0 t with ht | ht
  · rw [abs_of_nonneg ht, logisticLoss]
    have h := log_nonneg (show 1 ≤ 1 + exp (-t) by linarith [exp_pos (-t)])
    linarith
  · rw [abs_of_nonpos ht, logisticLoss_symm]
    have h := log_nonneg (show 1 ≤ 1 + exp t by linarith [exp_pos t])
    linarith

private theorem existsUnique_of_superlevel {f : ℝ → ℝ} {D : Set ℝ} {a l u : ℝ}
    (hc : StrictConcaveOn ℝ D f) (hf : ContinuousOn f D) (ha : a ∈ D)
    (hsub : Icc l u ⊆ D) (hbox : ∀ z ∈ D, f a ≤ f z → z ∈ Icc l u) :
    ∃! z, z ∈ D ∧ IsMaxOn f D z := by
  have hab := hbox a ha le_rfl
  obtain ⟨m, hm, hmax⟩ :=
    isCompact_Icc.exists_isMaxOn ⟨a, hab⟩ (hf.mono hsub)
  have hmD := hsub hm
  have hmMax : IsMaxOn f D m := by
    intro z hz
    by_cases h : f a ≤ f z
    · exact hmax (hbox z hz h)
    · exact (le_of_not_ge h).trans (hmax hab)
  exact ⟨m, ⟨hmD, hmMax⟩, fun z hz ↦ hc.eq_of_isMaxOn hz.2 hmMax hz.1 hmD⟩

private theorem inverse_slice {f : ℝ → ℝ × ℝ} {D : Set ℝ}
    (hD : Convex ℝ D) (hf : MapsTo f D LocationScale.inverseDomain)
    (hi : InjOn f D)
    (hlin : ∀ (x y a b : ℝ), a + b = 1 → f (a • x + b • y) = a • f x + b • f y)
    (y : Fin n → ℝ) (hn : 0 < n) :
    StrictConcaveOn ℝ D (fun z ↦ LocationScale.inverseLogLik logisticLoss y (f z)) := by
  refine ⟨hD, ?_⟩
  intro x hx z hz hxz a b ha hb hab
  dsimp only
  rw [hlin x z a b hab]
  exact (LocationScale.strictConcave_inverseLogLik logistic_strict y hn).2
    (hf hx) (hf hz) (fun h ↦ hxz (hi hx hz h)) ha hb hab

private theorem obj_upper (y : Fin n → ℝ) (p : ℝ × ℝ) (i : Fin n) :
    LocationScale.inverseLogLik logisticLoss y p ≤ n * log p.1 - |p.1 * y i - p.2| := by
  unfold LocationScale.inverseLogLik
  rw [Finset.sum_sub_distrib]
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  apply sub_le_sub_left
  exact (abs_le_logisticLoss _).trans (Finset.single_le_sum
    (fun j _ ↦ (abs_nonneg _).trans (abs_le_logisticLoss (p.1 * y j - p.2)))
    (Finset.mem_univ i))

private theorem heldShape_logistic (hn : 0 < n) (y : Fin n → ℝ) {β : ℝ} (hb : 0 < β) :
    ∃! μ, μ ∈ univ ∧ IsMaxOn
      (fun μ ↦ LocationScale.inverseLogLik logisticLoss y (β, β * μ)) univ μ := by
  let f := fun μ ↦ LocationScale.inverseLogLik logisticLoss y (β, β * μ)
  have hc : StrictConcaveOn ℝ univ f := by
    apply inverse_slice (f := fun μ ↦ (β, β * μ)) convex_univ
      (fun _ _ ↦ ⟨hb, mem_univ _⟩) ?_ ?_ y hn
    · intro x _ z _ h
      exact mul_left_cancel₀ hb.ne' (congrArg Prod.snd h)
    · intro x z a b hab
      apply Prod.ext
      · change β = a * β + b * β
        rw [← add_mul, hab, one_mul]
      · change β * (a * x + b * z) = a * (β * x) + b * (β * z)
        ring
  let i : Fin n := ⟨0, hn⟩
  let r := (n * log β - f 0) / β
  apply existsUnique_of_superlevel hc (hc.concaveOn.continuousOn isOpen_univ)
    (mem_univ (0 : ℝ)) (l := y i - r) (u := y i + r) (subset_univ _)
  intro z _ hz
  have h := obj_upper y (β, β * z) i
  change f z ≤ n * log β - |β * y i - β * z| at h
  rw [← mul_sub, abs_mul, abs_of_pos hb] at h
  have hr : |y i - z| ≤ r := (le_div_iff₀ hb).mpr (by linarith)
  obtain ⟨hl, hu⟩ := abs_le.mp hr
  exact ⟨by linarith, by linarith⟩

private theorem heldScale_logistic (hn : 0 < n) (y : Fin n → ℝ) {μ : ℝ}
    (hne : ∃ i, y i ≠ μ) :
    ∃! β, β ∈ Ioi (0 : ℝ) ∧ IsMaxOn
      (fun β ↦ LocationScale.inverseLogLik logisticLoss y (β, β * μ)) (Ioi 0) β := by
  let f := fun β ↦ LocationScale.inverseLogLik logisticLoss y (β, β * μ)
  have hc : StrictConcaveOn ℝ (Ioi 0) f := by
    apply inverse_slice (f := fun β ↦ (β, β * μ)) (convex_Ioi 0)
      (fun _ hb ↦ ⟨hb, mem_univ _⟩) ?_ ?_ y hn
    · intro x _ z _ h
      exact congrArg Prod.fst h
    · intro x z a b _
      apply Prod.ext
      · rfl
      · change (a * x + b * z) * μ = a * (x * μ) + b * (z * μ)
        ring
  obtain ⟨i, hi⟩ := hne
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let δ := |y i - μ|
  have hd : 0 < δ := abs_pos.mpr (sub_ne_zero.mpr hi)
  let k := δ / (2 * n)
  have hk : 0 < k := div_pos hd (by positivity)
  have hk' : (n : ℝ) * k = δ / 2 := by dsimp [k]; field_simp
  apply existsUnique_of_superlevel hc (hc.concaveOn.continuousOn isOpen_Ioi)
    (show (1 : ℝ) ∈ Ioi 0 from zero_lt_one)
    (l := exp (f 1 / n)) (u := (- n * (1 + log k) - f 1) / (δ / 2))
    (fun _ hz ↦ (exp_pos _).trans_le hz.1)
  intro β hb hβ
  have hp : 0 < β := hb
  have h := obj_upper y (β, β * μ) i
  change f β ≤ n * log β - |β * y i - β * μ| at h
  rw [← mul_sub, abs_mul, abs_of_pos hp] at h
  change f β ≤ n * log β - β * δ at h
  constructor
  · rw [← exp_log hp, exp_le_exp]
    exact (div_le_iff₀ hn').mpr (by linarith [mul_nonneg hp.le hd.le])
  · apply (le_div_iff₀ (show 0 < δ / 2 by positivity)).mpr
    have hlog := log_le_sub_one_of_pos (mul_pos hk hp)
    rw [log_mul hk.ne' hp.ne'] at hlog
    have hlog' := mul_le_mul_of_nonneg_left hlog hn'.le
    linarith [congrArg (fun v : ℝ ↦ v * β) hk']

private theorem inverse_transfer {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {β α : ℝ} (hb : 0 < β) (ha : 0 < α) :
    logLik x (β, α) =
      LocationScale.inverseLogLik logisticLoss (fun i ↦ log (x i)) (β, β * log α) +
        -(∑ i, log (x i)) := by
  rw [logLik_transfer hx (show (β, α) ∈ domain from ⟨hb, ha⟩), sub_eq_add_neg]
  have he := LocationScale.inverseLogLik_inverseEquiv logisticLoss (fun i ↦ log (x i))
    ⟨(log α, β⁻¹), mem_univ _, inv_pos.mpr hb⟩
  change LocationScale.inverseLogLik logisticLoss _ ((β⁻¹)⁻¹, log α / β⁻¹) = _ at he
  rw [inv_inv, div_inv_eq_mul, mul_comm (log α)] at he
  rw [he]
  rfl

/-- Holding positive shape gives a unique scale fit, including for constant data. -/
theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {β : ℝ} (hb : 0 < β) :
    ∃! α, α ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun α ↦ logLik x (β, α)) (Ioi 0) α := by
  have hlog : BijOn log (Ioi 0) univ :=
    ⟨fun _ _ ↦ mem_univ _, log_injOn_pos, fun μ _ ↦ ⟨exp μ, exp_pos _, log_exp _⟩⟩
  exact (Transfer.existsUnique_max_iff hlog (fun _ ha ↦ inverse_transfer hx hb ha)).mpr
    (heldShape_logistic hn (fun i ↦ log (x i)) hb)

/-- Holding positive scale gives a unique shape fit if a residual is nonzero. -/
theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {α : ℝ} (ha : 0 < α) (hne : ∃ i, x i ≠ α) :
    ∃! β, β ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun β ↦ logLik x (β, α)) (Ioi 0) β := by
  have hid : BijOn id (Ioi (0 : ℝ)) (Ioi 0) :=
    ⟨fun _ h ↦ h, fun _ _ _ _ h ↦ h, fun b hb ↦ ⟨b, hb, rfl⟩⟩
  obtain ⟨i, hi⟩ := hne
  exact (Transfer.existsUnique_max_iff hid (fun _ hb ↦ inverse_transfer hx hb ha)).mpr
    (heldScale_logistic hn (fun i ↦ log (x i)) ⟨i, fun h ↦ hi (log_injOn_pos (hx i) ha h)⟩)

end

end UnivariateML.LogLogistic
