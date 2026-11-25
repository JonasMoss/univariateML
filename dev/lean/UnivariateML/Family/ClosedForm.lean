module

public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.Basic
public import Mathlib.Analysis.Real.Sqrt
public import UnivariateML.Generic.Profile

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Closed-form maximum likelihood

The sample is nonempty. The results concern exact real arithmetic for
uncensored data. Positive scale coefficients are standard deviations for
normal and lognormal, and the Rayleigh scale is in its original units.

## Deviations

The normal dossier's base existence paragraph implicitly assumes a nonempty
sample. The normal fitter rejects empty input, and every fit theorem here
assumes a positive sample size.

The Rayleigh endpoint has zero density for every scale. On nonnegative data,
unique maximization requires every observation to be positive. With a zero
observation there are infinitely many likelihood maximizers, and the fitter
rejects the sample. The one-parameter held cases are evaluations.
-/

@[expose] public section

open Set Real

namespace UnivariateML

noncomputable section

variable {n : ℕ}

private theorem count_pos (hn : 0 < n) : (0 : ℝ) < n := by exact_mod_cast hn

private theorem sum_pos_iff {z : Fin n → ℝ} (hz : ∀ i, 0 ≤ z i) :
    0 < ∑ i, z i ↔ ∃ i, 0 < z i := by
  constructor
  · intro hp
    by_contra! h
    exact hp.not_ge (Finset.sum_nonpos fun i _ ↦ h i)
  · rintro ⟨i, hi⟩
    exact hi.trans_le (Finset.single_le_sum (fun j _ ↦ hz j) (Finset.mem_univ i))

private theorem not_bdd_of_values_above {α : Type*} {D : Set α} {f : α → ℝ}
    (h : ∀ M : ℝ, ∃ p ∈ D, M < f p) : ¬ BddAbove (f '' D) := by
  rintro ⟨M, hM⟩
  obtain ⟨p, hp, hlt⟩ := h M
  exact hlt.not_ge (hM (mem_image_of_mem f hp))

private theorem values_above_of_not_bdd {α : Type*} {D : Set α} {f : α → ℝ}
    (h : ¬ BddAbove (f '' D)) (M : ℝ) : ∃ p ∈ D, M < f p := by
  by_contra! hb
  apply h
  refine ⟨M, ?_⟩
  rintro _ ⟨p, hp, rfl⟩
  exact hb p hp

private theorem max_iff_of_strict_peak {α : Type*} {D : Set α} {f : α → ℝ}
    {p q : α} (hp : p ∈ D) (h : ∀ t ∈ D, t ≠ p → f t < f p) :
    q ∈ D ∧ IsMaxOn f D q ↔ q = p := by
  constructor
  · rintro ⟨hq, hm⟩
    by_contra hne
    exact (h q hq hne).not_ge (hm hp)
  · rintro rfl
    refine ⟨hp, fun t ht ↦ ?_⟩
    by_cases he : t = q
    · simp [he]
    · exact (h t ht he).le

private theorem existsUnique_max_iff {α : Type*} {D : Set α} {f : α → ℝ}
    {P : Prop} {p : α} (hyes : P → ∀ q, q ∈ D ∧ IsMaxOn f D q ↔ q = p)
    (hno : ¬ P → ¬ BddAbove (f '' D)) :
    (∃! q, q ∈ D ∧ IsMaxOn f D q) ↔ P := by
  constructor
  · rintro ⟨q, hq, _⟩
    by_contra hn
    exact hno hn hq.2.bddAbove
  · intro hp
    exact ⟨p, (hyes hp p).mpr rfl, fun q hq ↦ (hyes hp q).mp hq⟩

private def logRate (a b r : ℝ) : ℝ := a * log r - b * r

private theorem logRate_strict_peak {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∀ r > 0, r ≠ a / b → logRate a b r < logRate a b (a / b) := by
  intro r hr hne
  have ht := div_pos ha hb
  have hrne : r / (a / b) ≠ 1 := by
    intro h
    exact hne (by simpa using ((div_eq_iff_mul_eq ht.ne').mp h).symm)
  have h := mul_lt_mul_of_pos_left
    (log_lt_sub_one_of_pos (div_pos hr ht) hrne) ha
  rw [log_div hr.ne' ht.ne'] at h
  have he : a * (r / (a / b) - 1) = b * r - b * (a / b) := by
    field_simp [ha.ne', hb.ne']
  rw [he] at h
  unfold logRate
  linarith

private theorem logRate_zero_above {a : ℝ} (ha : 0 < a) (c M : ℝ) :
    ∃ r > 0, M < c + logRate a 0 r := by
  refine ⟨exp ((M - c) / a + 1), exp_pos _, ?_⟩
  unfold logRate
  rw [log_exp, zero_mul, sub_zero, mul_add, mul_one, mul_div_cancel₀ _ ha.ne']
  linarith

private def precision (s : ℝ) : ℝ := (s ^ 2)⁻¹

private theorem precision_pos {s : ℝ} (hs : 0 < s) : 0 < precision s :=
  inv_pos.mpr (sq_pos_of_pos hs)

private theorem precision_sqrt {t : ℝ} (ht : 0 ≤ t) :
    precision (sqrt t) = t⁻¹ := by simp [precision, sq_sqrt ht]

private theorem precision_inj {s t : ℝ} (hs : 0 < s) (ht : 0 < t)
    (h : precision s = precision t) : s = t := by
  exact (sq_eq_sq₀ hs.le ht.le).mp (inv_injective h)

private theorem scale_strict_peak {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∀ s > 0, s ≠ sqrt (b / a) →
      logRate a b (precision s) < logRate a b (precision (sqrt (b / a))) := by
  have hba := div_pos hb ha
  rw [precision_sqrt hba.le, inv_div]
  intro s hs hne
  apply logRate_strict_peak ha hb _ (precision_pos hs)
  intro he
  apply hne
  apply precision_inj hs (sqrt_pos.mpr hba)
  simpa [precision_sqrt hba.le, inv_div] using he

private theorem scale_zero_above {a : ℝ} (ha : 0 < a) (c M : ℝ) :
    ∃ s > 0, M < c + logRate a 0 (precision s) := by
  obtain ⟨r, hr, h⟩ := logRate_zero_above ha c M
  refine ⟨sqrt r⁻¹, sqrt_pos.mpr (inv_pos.mpr hr), ?_⟩
  rw [precision_sqrt (inv_nonneg.mpr hr.le), inv_inv]
  exact h

private theorem joint_iff_of_fibers {A B : Set ℝ} {L : ℝ → ℝ → ℝ}
    {a b : ℝ} (ha : a ∈ A)
    (hfirst : ∀ y ∈ B, ∀ x, x ∈ A ∧ IsMaxOn (fun t ↦ L t y) A x ↔ x = a)
    (hsecond : ∀ y, y ∈ B ∧ IsMaxOn (L a) B y ↔ y = b)
    (p : ℝ × ℝ) :
    p ∈ A ×ˢ B ∧ IsMaxOn (fun q : ℝ × ℝ ↦ L q.1 q.2) (A ×ˢ B) p ↔
      p = (a, b) := by
  constructor
  · rintro ⟨⟨hx, hy⟩, hm⟩
    have hxa := (hfirst p.2 hy p.1).mp ⟨hx, fun t ht ↦
      hm (show (t, p.2) ∈ A ×ˢ B from ⟨ht, hy⟩)⟩
    have hyb := (hsecond p.2).mp ⟨hy, fun t ht ↦ by
      have h := hm (show (a, t) ∈ A ×ˢ B from ⟨ha, ht⟩)
      change L a t ≤ L p.1 p.2 at h
      rw [hxa] at h
      exact h⟩
    exact Prod.ext hxa hyb
  · rintro rfl
    obtain ⟨hb, hm⟩ := (hsecond b).mpr rfl
    have h := Generic.isMaxOn_prod_of_profile
      (L := fun y x ↦ L x y) (best := fun _ ↦ a)
      (fun y hy ↦ (hfirst y hy a).mpr rfl) hb hm
    exact ⟨⟨ha, hb⟩, fun ⟨x, y⟩ hp ↦
      h.2 (show (y, x) ∈ B ×ˢ A from ⟨hp.2, hp.1⟩)⟩

private theorem average_eq_of_constant {x : Fin n → ℝ} {c : ℝ}
    (hn : 0 < n) (h : ∀ i, x i = c) : (∑ i, x i) / n = c := by
  simp [h, (count_pos hn).ne']

private theorem differs_from_average_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃ i, x i ≠ (∑ j, x j) / n) ↔ ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨i, hi⟩
    by_contra! h
    exact hi (average_eq_of_constant hn (fun j ↦ h j i)).symm
  · rintro ⟨i, j, hij⟩
    by_contra! h
    exact hij ((h i).trans (h j).symm)

namespace Normal

def mean (x : Fin n → ℝ) : ℝ := (∑ i, x i) / n

def residual (x : Fin n → ℝ) (μ : ℝ) : ℝ := ∑ i, (x i - μ) ^ 2

def sd (x : Fin n → ℝ) (μ : ℝ) : ℝ := sqrt (residual x μ / n)

@[no_expose] def logLik (x : Fin n → ℝ) (μ σ : ℝ) : ℝ :=
  -(n : ℝ) / 2 * log (2 * π) + logRate (n / 2) (residual x μ / 2) (precision σ)

def domain : Set (ℝ × ℝ) := univ ×ˢ Ioi 0

/-- The precision expression equals the usual normal log-likelihood at positive scale. -/
theorem logLik_formula (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (_hσ : 0 < σ) :
    logLik x μ σ =
      -(n : ℝ) / 2 * log (2 * π) - n * log σ - residual x μ / (2 * σ ^ 2) := by
  unfold logLik logRate precision
  rw [log_inv, log_pow]
  ring

private theorem residual_pos_iff (x : Fin n → ℝ) (μ : ℝ) :
    0 < residual x μ ↔ ∃ i, x i ≠ μ := by
  rw [residual, sum_pos_iff (fun i ↦ sq_nonneg (x i - μ))]
  simp [sq_pos_iff, sub_ne_zero]

private theorem residual_zero {x : Fin n → ℝ} {μ : ℝ} (h : ∀ i, x i = μ) :
    residual x μ = 0 := by simp [residual, h]

private theorem residual_split (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    residual x μ = residual x (mean x) + n * (μ - mean x) ^ 2 := by
  have hs : ∑ i, (x i - mean x) = 0 := by
    rw [Finset.sum_sub_distrib]
    simp [mean, mul_div_cancel₀ _ (count_pos hn).ne']
  calc
    residual x μ = ∑ i, ((x i - mean x) ^ 2 +
        ((-2 * (μ - mean x)) * (x i - mean x) + (μ - mean x) ^ 2)) := by
      apply Finset.sum_congr rfl
      intro i _
      ring
    _ = _ := by simp [residual, Finset.sum_add_distrib, ← Finset.mul_sum, hs]

/-- With positive standard deviation held, the unique mean maximizer is the sample mean. -/
theorem heldSd (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ) (μ : ℝ) :
    IsMaxOn (fun t ↦ logLik x t σ) univ μ ↔ μ = mean x := by
  simpa only [mem_univ, true_and] using
    (max_iff_of_strict_peak (f := fun t ↦ logLik x t σ) (q := μ) (mem_univ (mean x))
      (fun t _ hne ↦ by
        have h := mul_pos (count_pos hn) (sq_pos_of_ne_zero (sub_ne_zero.mpr hne))
        have hp := precision_pos hσ
        unfold logLik logRate
        rw [residual_split hn x t]
        nlinarith only [mul_pos h hp]))

/-- With mean held and a nonzero residual, the unique standard deviation is the residual RMS. -/
theorem heldMean (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ ↔ σ = sd x μ := by
  have hn' := count_pos hn
  have hQ := (residual_pos_iff x μ).mpr hx
  have he : (residual x μ / 2) / (n / 2) = residual x μ / n := by ring
  apply max_iff_of_strict_peak (sqrt_pos.mpr (div_pos hQ hn'))
  intro s hs hne
  unfold logLik
  apply add_lt_add_right
  unfold sd at hne ⊢
  rw [← he] at hne ⊢
  exact scale_strict_peak (by positivity) (by positivity) s hs hne

/-- With mean held at every observation, the likelihood is unbounded. -/
theorem heldMean_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, x i = μ) :
    ¬ BddAbove (logLik x μ '' Ioi 0) := by
  apply not_bdd_of_values_above
  intro M
  have hn' := count_pos hn
  simpa [logLik, residual_zero hx] using
    scale_zero_above (show (0 : ℝ) < n / 2 by positivity)
      (-(n : ℝ) / 2 * log (2 * π)) M

/-- On nonconstant data the joint maximum is the sample mean and residual RMS. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∃ i j, x i ≠ x j)
    (p : ℝ × ℝ) :
    p ∈ domain ∧ IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p ↔
      p = (mean x, sd x (mean x)) :=
  joint_iff_of_fibers (a := mean x) (mem_univ _)
    (fun _ hs μ ↦ by simpa only [mem_univ, true_and] using heldSd hn x hs μ)
    (heldMean hn (show ∃ i, x i ≠ mean x from (differs_from_average_iff hn x).mpr hx)) p

/-- Constant data give unbounded likelihood along the common mean. -/
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' domain) := by
  have he : ∀ i, x i = mean x := by
    simpa only [mean, not_exists, not_not] using mt (differs_from_average_iff hn x).mp hx
  apply not_bdd_of_values_above
  intro M
  obtain ⟨s, hs, h⟩ := values_above_of_not_bdd (heldMean_unbounded hn he) M
  exact ⟨(mean x, s), ⟨mem_univ _, hs⟩, h⟩

/-- The all-free normal fit exists uniquely exactly for nonconstant data. -/
theorem existsUnique_mle_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃! p : ℝ × ℝ, p ∈ domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p) ↔ ∃ i j, x i ≠ x j :=
  existsUnique_max_iff (jointMLE hn) (constant_unbounded hn)

/-- With mean held, existence and uniqueness are equivalent to a nonzero residual. -/
theorem existsUnique_heldMean_iff (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔ ∃ i, x i ≠ μ :=
  existsUnique_max_iff (heldMean hn) (fun h ↦ heldMean_unbounded hn (by simpa using h))

end Normal

namespace Lognormal

def logSample (x : Fin n → ℝ) : Fin n → ℝ := fun i ↦ log (x i)

def logLik (x : Fin n → ℝ) (μ σ : ℝ) : ℝ :=
  Normal.logLik (logSample x) μ σ - ∑ i, logSample x i

private theorem max_iff_normal {α : Type*} {f : α → ℝ} {D : Set α} {p : α} (c : ℝ) :
    IsMaxOn (fun q ↦ f q - c) D p ↔ IsMaxOn f D p := by
  constructor <;> intro h q hq
  · exact (sub_le_sub_iff_right c).mp (h hq)
  · exact sub_le_sub_right (h hq) c

/-- With log standard deviation held, the free log mean is the mean of the log sample. -/
theorem heldSd (hn : 0 < n) (x : Fin n → ℝ) (_hx : ∀ i, 0 < x i)
    {σ : ℝ} (hσ : 0 < σ) (μ : ℝ) :
    IsMaxOn (fun t ↦ logLik x t σ) univ μ ↔ μ = Normal.mean (logSample x) :=
  (max_iff_normal _).trans (Normal.heldSd hn (logSample x) hσ μ)

/-- With log mean held, any nonzero log residual gives a unique RMS log scale. -/
theorem heldMean (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i) {μ : ℝ}
    (h : ∃ i, log (x i) ≠ μ) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ ↔ σ = Normal.sd (logSample x) μ := by
  have he : IsMaxOn (logLik x μ) (Ioi 0) σ ↔
      IsMaxOn (Normal.logLik (logSample x) μ) (Ioi 0) σ := max_iff_normal _
  rw [he]
  exact Normal.heldMean hn h σ

/-- On positive nonconstant data the fitted coefficients are the normal fit of the logs. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ∃ i j, x i ≠ x j) (p : ℝ × ℝ) :
    p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Normal.domain p ↔
      p = (Normal.mean (logSample x), Normal.sd (logSample x) (Normal.mean (logSample x))) := by
  have he : IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Normal.domain p ↔
      IsMaxOn (fun q : ℝ × ℝ ↦ Normal.logLik (logSample x) q.1 q.2) Normal.domain p :=
    max_iff_normal _
  rw [he]
  obtain ⟨i, j, hij⟩ := h
  exact Normal.jointMLE hn ⟨i, j, fun e ↦ hij (log_injOn_pos (hx i) (hx j) e)⟩ p

/-- When all logs equal the held log mean, the likelihood is unbounded. -/
theorem heldMean_unbounded (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i) {μ : ℝ}
    (h : ∀ i, log (x i) = μ) :
    ¬ BddAbove (logLik x μ '' Ioi 0) := by
  apply not_bdd_of_values_above
  intro M
  obtain ⟨s, hs, hh⟩ := values_above_of_not_bdd
    (Normal.heldMean_unbounded hn h) (M + ∑ i, logSample x i)
  exact ⟨s, hs, lt_sub_iff_add_lt.mpr hh⟩

/-- A positive constant sample gives unbounded likelihood when both coefficients are free. -/
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' Normal.domain) := by
  have he : ¬ ∃ i j, logSample x i ≠ logSample x j := by
    rintro ⟨i, j, hij⟩
    exact h ⟨i, j, fun e ↦ hij (congrArg log e)⟩
  apply not_bdd_of_values_above
  intro M
  obtain ⟨p, hp, hh⟩ := values_above_of_not_bdd
    (Normal.constant_unbounded hn he) (M + ∑ i, logSample x i)
  exact ⟨p, hp, lt_sub_iff_add_lt.mpr hh⟩

/-- The all-free lognormal fit exists uniquely exactly for positive nonconstant data. -/
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃! p : ℝ × ℝ, p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Normal.domain p) ↔ ∃ i j, x i ≠ x j :=
  existsUnique_max_iff (jointMLE hn hx) (constant_unbounded hn hx)

/-- With log mean held, existence and uniqueness are equivalent to a nonzero log residual. -/
theorem existsUnique_heldMean_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (μ : ℝ) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔ ∃ i, log (x i) ≠ μ :=
  existsUnique_max_iff (heldMean hn hx) (fun h ↦ heldMean_unbounded hn hx (by simpa using h))

end Lognormal

namespace Exponential

def rate (x : Fin n → ℝ) : ℝ := n / ∑ i, x i

def logLik (x : Fin n → ℝ) (lam : ℝ) : ℝ := n * log lam - lam * ∑ i, x i

/-- A positive observation in a nonnegative sample gives the unique rate n / sum x. -/
theorem mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    (h : ∃ i, 0 < x i) (r : ℝ) :
    r ∈ Ioi 0 ∧ IsMaxOn (logLik x) (Ioi 0) r ↔ r = rate x := by
  have hS := (sum_pos_iff hx).mpr h
  apply max_iff_of_strict_peak (div_pos (count_pos hn) hS)
  intro t ht hne
  have he (r : ℝ) : logLik x r = logRate n (∑ i, x i) r := by
    unfold logLik logRate
    rw [mul_comm r]
  rw [he, he]
  exact logRate_strict_peak (count_pos hn) hS t ht hne

/-- The all-zero sample has unbounded likelihood as the free rate increases. -/
theorem allZero_unbounded (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i = 0) :
    ¬ BddAbove (logLik x '' Ioi 0) := by
  apply not_bdd_of_values_above
  intro M
  simpa [logLik, logRate, hx] using logRate_zero_above (count_pos hn) 0 M

/-- Holding the only coefficient evaluates a finite real log-likelihood, including at zero data. -/
theorem heldRate (_hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 ≤ x i)
    {r : ℝ} (hr : 0 < r) :
    r ∈ Ioi 0 ∧ IsMaxOn (logLik x) {r} r ∧
      logLik x r = n * log r - r * ∑ i, x i := by
  exact ⟨hr, fun _ ht ↦ (mem_singleton_iff.mp ht).symm ▸ le_rfl, rfl⟩

/-- The exponential fit exists uniquely exactly when a nonnegative sample
contains a positive observation. -/
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i) :
    (∃! r, r ∈ Ioi 0 ∧ IsMaxOn (logLik x) (Ioi 0) r) ↔ ∃ i, 0 < x i := by
  apply existsUnique_max_iff (mle hn hx)
  intro h
  apply allZero_unbounded hn
  intro i
  exact le_antisymm (le_of_not_gt (fun hi ↦ h ⟨i, hi⟩)) (hx i)

end Exponential

namespace InverseGaussian

def mean (x : Fin n → ℝ) : ℝ := (∑ i, x i) / n

def residual (x : Fin n → ℝ) (μ : ℝ) : ℝ :=
  ∑ i, (x i - μ) ^ 2 / (μ ^ 2 * x i)

def shape (x : Fin n → ℝ) (μ : ℝ) : ℝ := n / residual x μ

private def logConstant (x : Fin n → ℝ) : ℝ :=
  -(n : ℝ) / 2 * log (2 * π) - (3 / 2) * ∑ i, log (x i)

@[no_expose] def logLik (x : Fin n → ℝ) (μ lam : ℝ) : ℝ :=
  logConstant x + logRate (n / 2) (residual x μ / 2) lam

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

/-- The shape expression includes the inverse Gaussian normalizing and sample terms. -/
theorem logLik_formula (x : Fin n → ℝ) (μ lam : ℝ) :
    logLik x μ lam = (n : ℝ) / 2 * log lam - (n : ℝ) / 2 * log (2 * π) -
      (3 / 2) * (∑ i, log (x i)) - lam / 2 * residual x μ := by
  unfold logLik logConstant logRate
  ring

private theorem mean_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    0 < mean x := by
  have i : Fin n := ⟨0, hn⟩
  exact div_pos ((sum_pos_iff (fun j ↦ (hx j).le)).mpr ⟨i, hx i⟩) (count_pos hn)

private theorem residual_pos_iff {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {μ : ℝ}
    (hμ : 0 < μ) : 0 < residual x μ ↔ ∃ i, x i ≠ μ := by
  have hd (i : Fin n) : 0 < μ ^ 2 * x i := mul_pos (sq_pos_of_pos hμ) (hx i)
  rw [residual, sum_pos_iff (fun i ↦ div_nonneg (sq_nonneg _) (hd i).le)]
  exact exists_congr fun i ↦ (div_pos_iff_of_pos_right (hd i)).trans
    (sq_pos_iff.trans sub_ne_zero)

private theorem residual_zero {x : Fin n → ℝ} {μ : ℝ} (h : ∀ i, x i = μ) :
    residual x μ = 0 := by simp [residual, h]

private theorem residual_expand {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {μ : ℝ}
    (hμ : 0 < μ) :
    residual x μ = (∑ i, x i) * μ⁻¹ ^ 2 - 2 * (n : ℝ) * μ⁻¹ + ∑ i, (x i)⁻¹ := by
  unfold residual
  calc
    _ = ∑ i, (x i * μ⁻¹ ^ 2 - 2 * μ⁻¹ + (x i)⁻¹) := by
      apply Finset.sum_congr rfl
      intro i _
      field_simp [(hx i).ne', hμ.ne']
      ring
    _ = _ := by
      simp [Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.sum_mul]
      ring

private theorem sum_eq_count_mul_mean (hn : 0 < n) (x : Fin n → ℝ) :
    (∑ i, x i) = (n : ℝ) * mean x := by
  unfold mean
  field_simp [(count_pos hn).ne']

private theorem residual_split (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {μ : ℝ} (hμ : 0 < μ) :
    residual x μ = residual x (mean x) + (∑ i, x i) * (μ⁻¹ - (mean x)⁻¹) ^ 2 := by
  have hm := mean_pos hn hx
  rw [residual_expand hx hμ, residual_expand hx hm, sum_eq_count_mul_mean hn x]
  field_simp [hm.ne', hμ.ne']
  ring

/-- At the fitted mean, inverse shape also equals the mean reciprocal minus the reciprocal mean. -/
theorem inverse_shape_at_mean (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (shape x (mean x))⁻¹ = (∑ i, (x i)⁻¹) / n - (mean x)⁻¹ := by
  have hm := mean_pos hn hx
  rw [shape, inv_div, residual_expand hx hm, sum_eq_count_mul_mean hn x]
  field_simp [(count_pos hn).ne', hm.ne']
  ring

/-- With shape held positive, the sample mean uniquely maximizes the likelihood. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hlam : 0 < lam) (μ : ℝ) :
    μ ∈ Ioi 0 ∧ IsMaxOn (fun t ↦ logLik x t lam) (Ioi 0) μ ↔ μ = mean x := by
  apply max_iff_of_strict_peak (mean_pos hn hx)
  intro t ht hne
  have hS : 0 < ∑ i, x i :=
    (sum_pos_iff fun i ↦ (hx i).le).mpr ⟨⟨0, hn⟩, hx ⟨0, hn⟩⟩
  have hgap := mul_pos hS (sq_pos_of_ne_zero (sub_ne_zero.mpr (fun e ↦ hne (inv_injective e))))
  unfold logLik logRate
  rw [residual_split hn hx ht]
  nlinarith only [mul_pos hgap hlam]

/-- With mean held and a nonzero residual, the unique shape is n divided
by the weighted residual sum. -/
theorem heldMean (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {μ : ℝ} (hμ : 0 < μ) (h : ∃ i, x i ≠ μ) (lam : ℝ) :
    lam ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) lam ↔ lam = shape x μ := by
  have hn' := count_pos hn
  have hQ := (residual_pos_iff hx hμ).mpr h
  have he : (n / 2 : ℝ) / (residual x μ / 2) = shape x μ := by unfold shape; ring
  apply max_iff_of_strict_peak (div_pos hn' hQ)
  intro t ht hne
  unfold logLik
  apply add_lt_add_right
  rw [← he] at hne ⊢
  exact logRate_strict_peak (by positivity) (by positivity) t ht hne

/-- The likelihood is unbounded with mean held at the common observation. -/
theorem heldMean_unbounded (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {μ : ℝ} (_hμ : 0 < μ) (h : ∀ i, x i = μ) :
    ¬ BddAbove (logLik x μ '' Ioi 0) := by
  apply not_bdd_of_values_above
  intro M
  have hn' := count_pos hn
  simpa [logLik, residual_zero h] using
    logRate_zero_above (show (0 : ℝ) < n / 2 by positivity) (logConstant x) M

/-- Nonconstant positive data give the unique joint mean and shape fit. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ∃ i j, x i ≠ x j) (p : ℝ × ℝ) :
    p ∈ domain ∧ IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p ↔
      p = (mean x, shape x (mean x)) :=
  joint_iff_of_fibers (a := mean x) (mean_pos hn hx)
    (fun _ hlam μ ↦ heldShape hn hx hlam μ)
    (heldMean hn hx (mean_pos hn hx) ((differs_from_average_iff hn x).mpr h)) p

/-- With both coefficients free, constant positive data give unbounded likelihood. -/
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' domain) := by
  have he : ∀ i, x i = mean x := by
    simpa only [mean, not_exists, not_not] using mt (differs_from_average_iff hn x).mp h
  apply not_bdd_of_values_above
  intro M
  have hm := mean_pos hn hx
  obtain ⟨s, hs, hh⟩ := values_above_of_not_bdd (heldMean_unbounded hn hx hm he) M
  exact ⟨(mean x, s), ⟨hm, hs⟩, hh⟩

/-- The all-free inverse Gaussian fit exists uniquely exactly for positive nonconstant data. -/
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃! p : ℝ × ℝ, p ∈ domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p) ↔ ∃ i j, x i ≠ x j :=
  existsUnique_max_iff (jointMLE hn hx) (constant_unbounded hn hx)

/-- With positive mean held, existence and uniqueness are equivalent to a nonzero residual. -/
theorem existsUnique_heldMean_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {μ : ℝ} (hμ : 0 < μ) :
    (∃! lam, lam ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) lam) ↔ ∃ i, x i ≠ μ :=
  existsUnique_max_iff (heldMean hn hx hμ)
    (fun h ↦ heldMean_unbounded hn hx hμ (by simpa using h))

end InverseGaussian

namespace Rayleigh

private def sumSquares (x : Fin n → ℝ) : ℝ := ∑ i, x i ^ 2

@[no_expose] def scale (x : Fin n → ℝ) : ℝ := sqrt (sumSquares x / (2 * n))

/-- The product of Rayleigh densities, factored to keep zero observations in the model. -/
@[no_expose] def likelihood (x : Fin n → ℝ) (σ : ℝ) : ℝ :=
  (∏ i, x i) * exp (logRate n (sumSquares x / 2) (precision σ))

/-- The fitted scale is the square root of half the empirical second moment. -/
theorem scale_formula (x : Fin n → ℝ) :
    scale x = sqrt ((∑ i, x i ^ 2) / (2 * n)) := by
  unfold scale sumSquares
  rfl

/-- Every strictly positive sample has the unique scale sqrt(sum of squares / (2n)). -/
theorem mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (likelihood x) (Ioi 0) σ ↔ σ = scale x := by
  have hn' := count_pos hn
  have hS : 0 < sumSquares x :=
    (sum_pos_iff (fun i ↦ sq_nonneg (x i))).mpr ⟨⟨0, hn⟩, sq_pos_of_pos (hx ⟨0, hn⟩)⟩
  have he : (sumSquares x / 2) / n = sumSquares x / (2 * n) := by ring
  apply max_iff_of_strict_peak (show 0 < scale x from
    sqrt_pos.mpr (div_pos hS (mul_pos (by norm_num) hn')))
  intro t ht hne
  unfold likelihood
  apply mul_lt_mul_of_pos_left _ (Finset.prod_pos fun i _ ↦ hx i)
  apply exp_lt_exp.mpr
  unfold scale at hne ⊢
  rw [← he] at hne ⊢
  exact scale_strict_peak hn' (by positivity) t ht hne

/-- A zero observation forces the likelihood to vanish at every positive scale. -/
theorem likelihood_zero {x : Fin n → ℝ} (hx : ∃ i, x i = 0) (σ : ℝ) :
    likelihood x σ = 0 := by
  obtain ⟨i, hi⟩ := hx
  have hp : ∏ j, x j = 0 := Finset.prod_eq_zero (Finset.mem_univ i) hi
  simp [likelihood, hp]

/-- Every positive scale maximizes the all-zero likelihood of a sample containing zero. -/
theorem zero_ties {x : Fin n → ℝ} (hx : ∃ i, x i = 0) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (likelihood x) (Ioi 0) σ ↔ σ ∈ Ioi 0 := by
  exact ⟨And.left, fun hs ↦ ⟨hs, fun t _ ↦ by
    change likelihood x t ≤ likelihood x σ
    rw [likelihood_zero hx t, likelihood_zero hx σ]⟩⟩

/-- With scale held, the raw likelihood evaluation is strictly positive on positive data. -/
theorem heldScale (_hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {σ : ℝ} (hσ : 0 < σ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (likelihood x) {σ} σ ∧ 0 < likelihood x σ ∧
      log (likelihood x σ) = (∑ i, log (x i)) - 2 * n * log σ -
        (∑ i, x i ^ 2) / (2 * σ ^ 2) := by
  have hp : 0 < ∏ i, x i := Finset.prod_pos fun i _ ↦ hx i
  refine ⟨hσ, fun _ ht ↦ (mem_singleton_iff.mp ht).symm ▸ le_rfl,
    mul_pos hp (exp_pos _), ?_⟩
  unfold likelihood logRate precision sumSquares
  rw [log_mul hp.ne' (exp_pos _).ne', log_prod (fun i _ ↦ (hx i).ne'), log_exp,
    log_inv, log_pow]
  ring

/-- Among nonnegative samples, the Rayleigh scale has a unique MLE exactly for positive data. -/
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (likelihood x) (Ioi 0) σ) ↔ ∀ i, 0 < x i := by
  constructor
  · rintro ⟨σ, _, huniq⟩ i
    by_contra hi
    have hz : ∃ j, x j = 0 := ⟨i, le_antisymm (le_of_not_gt hi) (hx i)⟩
    have h₁ := huniq 1 ((zero_ties hz 1).mpr one_pos)
    have h₂ := huniq 2 ((zero_ties hz 2).mpr (by norm_num))
    linarith
  · intro hp
    exact ⟨scale x, (mle hn hp _).mpr rfl, fun t ht ↦ (mle hn hp t).mp ht⟩

end Rayleigh

end
end UnivariateML
