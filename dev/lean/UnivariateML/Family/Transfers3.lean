module

public import UnivariateML.Family.Beta
public import UnivariateML.Family.LogLogistic
public import UnivariateML.Family.Transfers

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Exact transfers for held Pareto III, nonstandard beta, and beta prime fits

The free parameters are unchanged. Shifting the observations has unit Jacobian.
The affine and fractional-linear changes to beta data add a term independent
of both shape parameters.

## Deviations

The results concern nonempty samples in the strict interior of the held support
and exact real arithmetic. Constant-data unboundedness for Pareto III is
scoped to the joint shape and scale fit, or to a free shape with scale held
at the common excess. Free endpoints and free beta-prime scale are outside
the transfer results.
-/

@[expose] public section

open Set Filter Real

namespace UnivariateML

noncomputable section

variable {n : ℕ}

private theorem identity_bijOn {P : Type*} (D : Set P) : BijOn id D D :=
  ⟨fun _ h ↦ h, fun _ _ _ _ h ↦ h, fun p hp ↦ ⟨p, hp, rfl⟩⟩

private theorem nonconstant_map_iff {x : Fin n → ℝ} {f : ℝ → ℝ}
    (hf : InjOn f (range x)) :
    (∃ i j, f (x i) ≠ f (x j)) ↔ ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨i, j, hij⟩
    exact ⟨i, j, fun h ↦ hij (congrArg f h)⟩
  · rintro ⟨i, j, hij⟩
    exact ⟨i, j, fun h ↦ hij (hf (mem_range_self i) (mem_range_self j) h)⟩

private theorem eq_constant (hn : 0 < n) {x : Fin n → ℝ}
    (h : ¬ ∃ i j, x i ≠ x j) : x = fun _ ↦ x ⟨0, hn⟩ :=
  funext fun i ↦ not_ne_iff.mp fun hi ↦ h ⟨i, ⟨0, hn⟩, hi⟩

private theorem no_max_of_unbounded {P : Type*} {D : Set P} {l : P → ℝ}
    (h : ¬ BddAbove (l '' D)) : ¬ ∃ p ∈ D, IsMaxOn l D p := by
  rintro ⟨p, _, hp⟩
  exact h ⟨l p, by rintro _ ⟨q, hq, rfl⟩; exact hp hq⟩

namespace ParetoIII

/-- Excess over the held threshold. -/
def excess (x : Fin n → ℝ) (μ : ℝ) (i : Fin n) : ℝ := x i - μ

def logLik (x : Fin n → ℝ) (μ : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, (log p.1 + log p.2⁻¹ + (p.1 - 1) * log (p.2⁻¹ * (x i - μ)) -
    2 * log (1 + (p.2⁻¹ * (x i - μ)) ^ p.1))

/-- Translation preserves the total log likelihood in shape and scale. -/
theorem logLik_transfer (x : Fin n → ℝ) (μ : ℝ) (p : ℝ × ℝ) :
    logLik x μ p = LogLogistic.logLik (excess x μ) p := by
  simp only [logLik, LogLogistic.logLik, LogLogistic.logDensity, excess, log_inv,
    div_eq_mul_inv, mul_comm p.2⁻¹, sub_eq_add_neg]

/-- Constant excesses allow the free shape to diverge at their common scale. -/
theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove (logLik x μ '' LogLogistic.domain) := by
  have he : excess x μ = fun _ ↦ x ⟨0, hn⟩ - μ := by
    unfold excess
    exact funext fun i ↦ congrArg (· - μ) (congrFun (eq_constant hn h) i)
  simpa only [logLik_transfer, he] using
    (LogLogistic.constant_unbounded hn (sub_pos.mpr (hx ⟨0, hn⟩)))

/-- The two free parameters have one finite maximizer exactly for nonconstant data. -/
theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i) :
    (∃! p, p ∈ LogLogistic.domain ∧ IsMaxOn (logLik x μ) LogLogistic.domain p) ↔
      ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨p, hp, _⟩
    by_contra h
    exact no_max_of_unbounded (joint_unbounded hn hx h) ⟨p, hp.1, hp.2⟩
  · rintro ⟨i, j, hij⟩
    have hne : LocationScale.Nonconstant (excess x μ) :=
      ⟨i, j, fun h ↦ hij (sub_left_inj.mp h)⟩
    rw [funext (logLik_transfer x μ)]
    exact LogLogistic.jointMLE (fun i ↦ sub_pos.mpr (hx i)) hne

/-- A positive held shape gives one finite scale even for constant excesses. -/
theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} {μ γ : ℝ}
    (hx : ∀ i, μ < x i) (hγ : 0 < γ) :
    ∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (fun s ↦ logLik x μ (γ, s)) (Ioi 0) σ := by
  simpa only [logLik_transfer] using
    (LogLogistic.heldShape_unique hn (x := excess x μ) (fun i ↦ sub_pos.mpr (hx i)) hγ)

/-- When every excess equals the held scale, the free-shape likelihood is unbounded. -/
theorem heldScale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ σ : ℝ}
    (hσ : 0 < σ) (h : ∀ i, x i - μ = σ) :
    ¬ BddAbove ((fun γ ↦ logLik x μ (γ, σ)) '' Ioi 0) := by
  have he : excess x μ = fun _ ↦ σ := funext h
  simpa only [logLik_transfer, he] using LogLogistic.heldScale_unbounded hn hσ

/-- A positive held scale gives one finite shape exactly off the concentration case. -/
theorem heldScale_unique_iff (hn : 0 < n) {x : Fin n → ℝ} {μ σ : ℝ}
    (hx : ∀ i, μ < x i) (hσ : 0 < σ) :
    (∃! γ, γ ∈ Ioi 0 ∧ IsMaxOn (fun k ↦ logLik x μ (k, σ)) (Ioi 0) γ) ↔
      ∃ i, x i - μ ≠ σ := by
  constructor
  · rintro ⟨γ, hγ, _⟩
    by_contra! h
    exact no_max_of_unbounded (heldScale_unbounded hn hσ h) ⟨γ, hγ.1, hγ.2⟩
  · intro h
    simpa only [logLik_transfer] using
      (LogLogistic.heldScale_unique hn (x := excess x μ) (fun i ↦ sub_pos.mpr (hx i)) hσ h)

end ParetoIII

private theorem beta_joint_transfer (hn : 0 < n) {x : Fin n → ℝ} {f : ℝ → ℝ}
    (hf : InjOn f (range x)) (hy : ∀ i, f (x i) ∈ Ioo (0 : ℝ) 1)
    {l : (ℝ × ℝ) → ℝ} {c : ℝ}
    (hl : ∀ p ∈ Beta.domain, l p = Beta.logLik (fun i ↦ f (x i)) p + c) :
    (∃! p, p ∈ Beta.domain ∧ IsMaxOn l Beta.domain p) ↔ ∃ i j, x i ≠ x j :=
  (Transfer.existsUnique_max_iff (identity_bijOn Beta.domain) hl).trans
    ((Beta.jointMLE_iff hn hy).trans (nonconstant_map_iff hf))

private theorem beta_constant_transfer (hn : 0 < n) {t : ℝ} (ht : t ∈ Ioo (0 : ℝ) 1)
    {l : (ℝ × ℝ) → ℝ} {c : ℝ}
    (hl : ∀ p ∈ Beta.domain, l p = Beta.logLik (fun _ : Fin n ↦ t) p + c) :
    ¬ BddAbove (l '' Beta.domain) := by
  intro hb
  obtain ⟨M, hM⟩ := (Transfer.bddAbove_iff (identity_bijOn Beta.domain) hl).mp hb
  obtain ⟨s, hs, hlt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
    ((Beta.constant_unbounded hn ht).eventually_gt_atTop M)).exists
  have hp : (s * t, s * (1 - t)) ∈ Beta.domain :=
    ⟨mul_pos hs ht.1, mul_pos hs (sub_pos.mpr ht.2)⟩
  exact hlt.not_ge (hM ⟨_, hp, rfl⟩)

namespace NonstandardBeta

/-- Interior observations in the unit-interval coordinate. -/
def unitData (x : Fin n → ℝ) (a b : ℝ) (i : Fin n) : ℝ := (x i - a) / (b - a)

def logLik (x : Fin n → ℝ) (a b : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, ((p.1 - 1) * log ((x i - a) / (b - a)) +
    (p.2 - 1) * log ((b - x i) / (b - a)) - Beta.logBeta p - log (b - a))

private theorem unit_mem {x : Fin n → ℝ} {a b : ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) (i : Fin n) : unitData x a b i ∈ Ioo (0 : ℝ) 1 := by
  have hw := sub_pos.mpr hab
  exact ⟨div_pos (sub_pos.mpr (hx i).1) hw,
    (div_lt_one hw).mpr (sub_lt_sub_right (hx i).2 a)⟩

/-- The affine map contributes one inverse-width Jacobian per observation. -/
theorem logLik_transfer (x : Fin n → ℝ) {a b : ℝ} (hab : a < b) (p : ℝ × ℝ) :
    logLik x a b p = Beta.logLik (unitData x a b) p + -((n : ℝ) * log (b - a)) := by
  have he (i : Fin n) : (b - x i) / (b - a) = 1 - (x i - a) / (b - a) := by
    field_simp [ne_of_gt (sub_pos.mpr hab)]
    ring
  simp only [logLik, Beta.logLik, unitData, he, Finset.sum_sub_distrib,
    Finset.sum_add_distrib, ← Finset.mul_sum, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  rw [sub_eq_add_neg]

/-- The two shapes have one finite maximizer exactly for nonconstant data. -/
theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℝ} {a b : ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) :
    (∃! p, p ∈ Beta.domain ∧ IsMaxOn (logLik x a b) Beta.domain p) ↔ ∃ i j, x i ≠ x j :=
  beta_joint_transfer (f := fun t ↦ (t - a) / (b - a)) hn
    (by intro u _ v _ h
        exact sub_left_inj.mp ((div_left_inj₀ (sub_pos.mpr hab).ne').mp h))
    (unit_mem hab hx) (fun p _ ↦ logLik_transfer x hab p)

/-- With both shapes free, constant interior observations yield unbounded likelihood. -/
theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ} {a b : ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove (logLik x a b '' Beta.domain) := by
  have he : unitData x a b = fun _ ↦ unitData x a b ⟨0, hn⟩ :=
    funext fun i ↦ congrArg (fun t ↦ (t - a) / (b - a)) (congrFun (eq_constant hn h) i)
  exact beta_constant_transfer hn (unit_mem hab hx ⟨0, hn⟩)
    (fun p _ ↦ (logLik_transfer x hab p).trans (by rw [he]))

theorem heldFirst (hn : 0 < n) {x : Fin n → ℝ} {a b h : ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) (hh : 0 < h) :
    ∃! q, q ∈ Ioi 0 ∧ IsMaxOn (fun z ↦ logLik x a b (h, z)) (Ioi 0) q :=
  (Transfer.existsUnique_max_iff (identity_bijOn (Ioi 0))
    (fun z _ ↦ logLik_transfer x hab (h, z))).mpr (Beta.heldFirst hn (unit_mem hab hx) hh)

theorem heldSecond (hn : 0 < n) {x : Fin n → ℝ} {a b h : ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) (hh : 0 < h) :
    ∃! p, p ∈ Ioi 0 ∧ IsMaxOn (fun z ↦ logLik x a b (z, h)) (Ioi 0) p :=
  (Transfer.existsUnique_max_iff (identity_bijOn (Ioi 0))
    (fun z _ ↦ logLik_transfer x hab (z, h))).mpr (Beta.heldSecond hn (unit_mem hab hx) hh)

end NonstandardBeta

namespace BetaPrime

def unitData (x : Fin n → ℝ) (σ : ℝ) (i : Fin n) : ℝ := x i / (σ + x i)

def logLik (x : Fin n → ℝ) (σ : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, ((p.1 - 1) * log (x i / σ) - log σ -
    (p.1 + p.2) * log (1 + x i / σ) - Beta.logBeta p)

/-- Total log Jacobian of the map from positive data to the unit interval. -/
def logJacobian (x : Fin n → ℝ) (σ : ℝ) : ℝ :=
  ∑ i, log (σ / (σ + x i) ^ 2)

private theorem unit_mem {x : Fin n → ℝ} {σ : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) (i : Fin n) : unitData x σ i ∈ Ioo (0 : ℝ) 1 := by
  have hd := add_pos hσ (hx i)
  exact ⟨div_pos (hx i) hd, (div_lt_one hd).mpr (by linarith)⟩

/-- The beta-prime and transformed beta log likelihoods differ by the log Jacobian. -/
theorem logLik_transfer {x : Fin n → ℝ} {σ : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) (p : ℝ × ℝ) :
    logLik x σ p = Beta.logLik (unitData x σ) p + logJacobian x σ := by
  have he (i : Fin n) :
      (p.1 - 1) * log (x i / σ) - log σ - (p.1 + p.2) * log (1 + x i / σ) -
          Beta.logBeta p =
        (p.1 - 1) * log (unitData x σ i) + (p.2 - 1) * log (1 - unitData x σ i) -
          Beta.logBeta p + log (σ / (σ + x i) ^ 2) := by
    have hd := (add_pos hσ (hx i)).ne'
    have hsum : 1 + x i / σ = (σ + x i) / σ := by field_simp
    have hcomp : 1 - unitData x σ i = σ / (σ + x i) := by
      unfold unitData
      field_simp
      ring
    rw [hsum, hcomp, unitData, log_div (hx i).ne' hσ.ne', log_div hd hσ.ne',
      log_div (hx i).ne' hd, log_div hσ.ne' hd, log_div hσ.ne' (pow_ne_zero _ hd), log_pow]
    ring
  simp only [logLik, he, Beta.logLik, logJacobian, Finset.sum_add_distrib,
    Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]

private theorem unit_injOn {x : Fin n → ℝ} {σ : ℝ} (hσ : 0 < σ) (hx : ∀ i, 0 < x i) :
    InjOn (fun t : ℝ ↦ t / (σ + t)) (range x) := by
  rintro _ ⟨i, rfl⟩ _ ⟨j, rfl⟩ he
  have hm := (div_eq_div_iff (add_pos hσ (hx i)).ne' (add_pos hσ (hx j)).ne').mp he
  exact (mul_right_cancel₀ hσ.ne') (by nlinarith)

theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℝ} {σ : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) :
    (∃! p, p ∈ Beta.domain ∧ IsMaxOn (logLik x σ) Beta.domain p) ↔ ∃ i j, x i ≠ x j :=
  beta_joint_transfer hn (unit_injOn hσ hx) (unit_mem hσ hx)
    (fun p _ ↦ logLik_transfer hσ hx p)

theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ} {σ : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove (logLik x σ '' Beta.domain) := by
  have he : unitData x σ = fun _ ↦ unitData x σ ⟨0, hn⟩ :=
    funext fun i ↦ congrArg (fun t ↦ t / (σ + t)) (congrFun (eq_constant hn h) i)
  exact beta_constant_transfer hn (unit_mem hσ hx ⟨0, hn⟩)
    (fun p _ ↦ (logLik_transfer hσ hx p).trans (by rw [he]))

theorem heldFirst (hn : 0 < n) {x : Fin n → ℝ} {σ h : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) (hh : 0 < h) :
    ∃! q, q ∈ Ioi 0 ∧ IsMaxOn (fun z ↦ logLik x σ (h, z)) (Ioi 0) q :=
  (Transfer.existsUnique_max_iff (identity_bijOn (Ioi 0))
    (fun z _ ↦ logLik_transfer hσ hx (h, z))).mpr (Beta.heldFirst hn (unit_mem hσ hx) hh)

theorem heldSecond (hn : 0 < n) {x : Fin n → ℝ} {σ h : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) (hh : 0 < h) :
    ∃! p, p ∈ Ioi 0 ∧ IsMaxOn (fun z ↦ logLik x σ (z, h)) (Ioi 0) p :=
  (Transfer.existsUnique_max_iff (identity_bijOn (Ioi 0))
    (fun z _ ↦ logLik_transfer hσ hx (z, h))).mpr (Beta.heldSecond hn (unit_mem hσ hx) hh)

end BetaPrime

end
end UnivariateML
