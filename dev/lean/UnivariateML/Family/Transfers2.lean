module

public import UnivariateML.Family.Gamma
public import UnivariateML.Family.ClosedForm
public import UnivariateML.Family.Endpoint

import Mathlib.Analysis.Calculus.Darboux
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Exact likelihood transfers

The sample is nonempty. All likelihood comparisons are in exact real arithmetic.
The Gamma module uses rate and mean log likelihood. Nakagami uses shape and
second moment, and the statements below use total log likelihood. For the
translated half-normal the comparisons use the positive-likelihood domain
where all observations are at or above the endpoint.

## Deviations

Every fit theorem assumes a nonempty sample, as the fitters require. The
logit-normal dossier's fit note points to the loginorm family. The implemented
logit-normal fitter is in the logitnorm family and names collapse Unbounded.
Nakagami's limits metadata records the held-scale collapse. Its existence
paragraph also records the same collapse with both coefficients free.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML

noncomputable section

variable {n : ℕ}

private theorem max_transfer {P Q : Type*} {D : Set P} {E : Set Q}
    {f : P → ℝ} {g : Q → ℝ} {T : P → Q} {a b : ℝ} (ha : 0 < a)
    (hmap : MapsTo T D E) (hsurj : SurjOn T D E)
    (he : ∀ q ∈ D, f q = a * g (T q) + b) {p : P} (hp : p ∈ D) :
    IsMaxOn f D p ↔ IsMaxOn g E (T p) := by
  constructor
  · intro h y hy
    obtain ⟨q, hq, rfl⟩ := hsurj hy
    have hl : f q ≤ f p := h hq
    rw [he q hq, he p hp] at hl
    exact le_of_mul_le_mul_left (le_of_add_le_add_right hl) ha
  · intro h q hq
    change f q ≤ f p
    rw [he q hq, he p hp]
    exact add_le_add_left (mul_le_mul_of_nonneg_left (h (hmap hq)) ha.le) _

private theorem max_add_const {P : Type*} (g : P → ℝ) (b : ℝ) (D : Set P) (p : P) :
    IsMaxOn (fun q ↦ g q + b) D p ↔ IsMaxOn g D p := by
  constructor <;> intro h q hq
  · exact le_of_add_le_add_right (h hq)
  · exact add_le_add_left (h hq) b

private theorem unbounded_add_const {P : Type*} {g : P → ℝ} {D : Set P}
    (h : ¬ BddAbove (g '' D)) (b : ℝ) :
    ¬ BddAbove ((fun q ↦ g q + b) '' D) := by
  rintro ⟨M, hM⟩
  apply h
  refine ⟨M - b, ?_⟩
  rintro _ ⟨q, hq, rfl⟩
  exact le_sub_iff_add_le.mpr (hM ⟨q, hq, rfl⟩)

private theorem no_max_of_unbounded {P : Type*} {f : P → ℝ} {D : Set P}
    (h : ¬ BddAbove (f '' D)) {p : P} (hp : IsMaxOn f D p) : False := by
  apply h
  refine ⟨f p, ?_⟩
  rintro _ ⟨q, hq, rfl⟩
  exact hp hq

private theorem existsUnique_of_fit_eq {P : Type*} {pred : P → Prop} {fit : P}
    (h : ∀ p, pred p ↔ p = fit) : ∃! p, pred p :=
  ⟨fit, (h fit).mpr rfl, fun p hp ↦ (h p).mp hp⟩

namespace LogitNormal

def logit (t : ℝ) : ℝ := log (t / (1 - t))

def logits (x : Fin n → ℝ) : Fin n → ℝ := fun i ↦ logit (x i)

def logLik (x : Fin n → ℝ) (μ σ : ℝ) : ℝ :=
  Normal.logLik (logits x) μ σ + (-∑ i, log (x i * (1 - x i)))

/-- The normal log likelihood on logit data plus the log Jacobian. -/
theorem logLik_logit (x : Fin n → ℝ) (μ σ : ℝ) :
    logLik x μ σ = Normal.logLik (logits x) μ σ - ∑ i, log (x i * (1 - x i)) := by
  simp [logLik, sub_eq_add_neg]

/-- The transferred log likelihood has the dossier's log-density terms. -/
theorem logLik_formula {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    logLik x μ σ = -(n : ℝ) / 2 * log (2 * π) - n * log σ -
      (∑ i, (logit (x i) - μ) ^ 2) / (2 * σ ^ 2) -
      ∑ i, log (x i) - ∑ i, log (1 - x i) := by
  have he (i : Fin n) : log (x i * (1 - x i)) = log (x i) + log (1 - x i) :=
    log_mul (ne_of_gt (hx i).1) (ne_of_gt (sub_pos.mpr (hx i).2))
  rw [logLik_logit, Normal.logLik_formula _ _ hσ]
  simp only [Normal.residual, logits, he, Finset.sum_add_distrib]
  ring

private theorem logit_inj {s t : ℝ} (hs : s ∈ Ioo 0 1) (ht : t ∈ Ioo 0 1)
    (h : logit s = logit t) : s = t := by
  have hs' : 0 < 1 - s := sub_pos.mpr hs.2
  have ht' : 0 < 1 - t := sub_pos.mpr ht.2
  have he := (log_injOn_pos (div_pos hs.1 hs') (div_pos ht.1 ht')) h
  have he' := (div_eq_div_iff hs'.ne' ht'.ne').mp he
  nlinarith

/-- The unique joint fit is the logit mean and its residual RMS. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    (hne : ∃ i j, x i ≠ x j) (p : ℝ × ℝ) :
    p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Normal.domain p ↔
    p = (Normal.mean (logits x), Normal.sd (logits x) (Normal.mean (logits x))) := by
  obtain ⟨i, j, hij⟩ := hne
  unfold logLik
  rw [max_add_const]
  exact Normal.jointMLE hn ⟨i, j, fun h ↦ hij (logit_inj (hx i) (hx j) h)⟩ p

/-- Joint collapse occurs at the common logit. -/
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' Normal.domain) := by
  apply unbounded_add_const (Normal.constant_unbounded hn ?_) _
  rintro ⟨i, j, hij⟩
  exact h ⟨i, j, fun he ↦ hij (congrArg logit he)⟩

/-- The positive held scale leaves the logit mean as the unique maximizer. -/
theorem heldScale (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ) (μ : ℝ) :
    IsMaxOn (fun t ↦ logLik x t σ) univ μ ↔ μ = Normal.mean (logits x) := by
  unfold logLik
  rw [max_add_const]
  exact Normal.heldSd hn _ hσ μ

/-- With location held, existence and uniqueness require a nonzero logit residual. -/
theorem existsUnique_heldLocation_iff (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔ ∃ i, logits x i ≠ μ := by
  rw [← Normal.existsUnique_heldMean_iff hn (logits x) μ]
  exact existsUnique_congr fun σ ↦ and_congr_right
    (fun _ ↦ max_add_const (Normal.logLik (logits x) μ) _ _ σ)

/-- The scale estimate at a held location is the logit residual RMS. -/
theorem heldLocation (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (h : ∃ i, logits x i ≠ μ) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ ↔ σ = Normal.sd (logits x) μ := by
  rw [← Normal.heldMean hn h σ]
  exact and_congr_right (fun _ ↦ max_add_const (Normal.logLik (logits x) μ) _ _ σ)

/-- A joint fit exists uniquely exactly for nonconstant data in the open unit interval. -/
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1) :
    (∃! p : ℝ × ℝ, p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Normal.domain p) ↔ ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨p, hp, _⟩
    by_contra h
    exact no_max_of_unbounded (constant_unbounded hn h) hp.2
  · intro h
    exact existsUnique_of_fit_eq (jointMLE hn hx h)

/-- Holding the common logit as location leaves unbounded likelihood as scale collapses. -/
theorem heldLocation_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (h : ∀ i, logits x i = μ) : ¬ BddAbove (logLik x μ '' Ioi 0) :=
  unbounded_add_const (Normal.heldMean_unbounded hn h) _

end LogitNormal

namespace InverseExponential

def reciprocal (x : Fin n → ℝ) : Fin n → ℝ := fun i ↦ (x i)⁻¹

def scale (x : Fin n → ℝ) : ℝ := n / ∑ i, (x i)⁻¹

def logLik (x : Fin n → ℝ) (θ : ℝ) : ℝ :=
  n * log θ - θ * (∑ i, (x i)⁻¹) - 2 * ∑ i, log (x i)

/-- The reciprocal exponential rate is the inverse-exponential scale. -/
theorem logLik_reciprocal (x : Fin n → ℝ) (θ : ℝ) :
    logLik x θ = Exponential.logLik (reciprocal x) θ - 2 * ∑ i, log (x i) := rfl

/-- Every nonempty positive sample has the harmonic mean as its unique scale maximizer. -/
theorem mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (θ : ℝ) :
    θ ∈ Ioi 0 ∧ IsMaxOn (logLik x) (Ioi 0) θ ↔ θ = scale x := by
  have he : logLik x = fun t ↦ Exponential.logLik (reciprocal x) t + (-(2 * ∑ i, log (x i))) := by
    funext t
    rw [logLik_reciprocal, sub_eq_add_neg]
  rw [he, max_add_const]
  exact Exponential.mle hn (fun i ↦ (inv_pos.mpr (hx i)).le)
    ⟨⟨0, hn⟩, inv_pos.mpr (hx ⟨0, hn⟩)⟩ θ

/-- Holding a positive scale restricts the fit to a single likelihood evaluation. -/
theorem heldScale (x : Fin n → ℝ) {θ : ℝ} (hθ : 0 < θ) :
    Ioi (0 : ℝ) ∩ {θ} = {θ} ∧ IsMaxOn (logLik x) {θ} θ := by
  constructor
  · ext t
    simp +contextual [hθ]
  · intro t ht
    obtain rfl := mem_singleton_iff.mp ht
    exact le_rfl

end InverseExponential

namespace Nakagami

def squared (x : Fin n → ℝ) : Fin n → ℝ := fun i ↦ x i ^ 2

def logDensity (m Ω t : ℝ) : ℝ :=
  log 2 + m * log m - log (Real.Gamma m) - m * log Ω +
    (2 * m - 1) * log t - m * t ^ 2 / Ω

def logLik (x : Fin n → ℝ) (m Ω : ℝ) : ℝ := ∑ i, logDensity m Ω (x i)

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

/-- The gamma shape is m, its scale is Ω/m, and the square Jacobian is 2x. -/
theorem logLik_square (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {m Ω : ℝ} (hm : 0 < m) (hΩ : 0 < Ω) :
    logLik x m Ω = n * Gamma.logLik (squared x) m (m / Ω) + ∑ i, log (2 * x i) := by
  rw [Gamma.logLik_eq_avg hn]
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  rw [Gamma.avg, ← mul_div_assoc, mul_comm (n : ℝ), mul_div_cancel_right₀ _ hn']
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  unfold logDensity Gamma.logDensity squared
  rw [log_mul (by norm_num : (2 : ℝ) ≠ 0) (hx i).ne', log_pow,
    log_div hm.ne' hΩ.ne']
  ring

private def toGamma (p : ℝ × ℝ) : ℝ × ℝ := (p.1, p.1 / p.2)

private theorem gamma_map : MapsTo toGamma domain Gamma.domain :=
  fun _ hp ↦ ⟨hp.1, div_pos hp.1 hp.2⟩

private theorem gamma_inverse {p : ℝ × ℝ} (hp : p ∈ Gamma.domain) :
    toGamma (toGamma p) = p := by
  have h1 : 0 < p.1 := hp.1
  have h2 : 0 < p.2 := hp.2
  refine Prod.ext ?_ ?_
  · rfl
  · change p.1 / (p.1 / p.2) = p.2
    field_simp

private theorem square_inj {u v : ℝ} (hu : 0 < u) (hv : 0 < v)
    (h : u ^ 2 = v ^ 2) : u = v := by
  have hh : (u - v) * (u + v) = 0 := by nlinarith
  exact sub_eq_zero.mp ((mul_eq_zero.mp hh).resolve_right (add_pos hu hv).ne')

/-- The joint Nakagami maximizer transfers from the joint gamma maximizer. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p := by
  have hs : ∃ i j, squared x i ≠ squared x j := by
    obtain ⟨i, j, hij⟩ := hne
    exact ⟨i, j, fun h ↦ hij (square_inj (hx i) (hx j) h)⟩
  obtain ⟨p, hp, hu⟩ := Gamma.jointMLE hn (fun i ↦ sq_pos_of_pos (hx i)) hs
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have ht (q) (hq : q ∈ domain) :
      IsMaxOn (fun r : ℝ × ℝ ↦ logLik x r.1 r.2) domain q ↔
        IsMaxOn (fun r : ℝ × ℝ ↦ Gamma.logLik (squared x) r.1 r.2) Gamma.domain (toGamma q) :=
    max_transfer hn' gamma_map (fun r hr ↦ ⟨toGamma r, gamma_map hr, gamma_inverse hr⟩)
      (fun r hr ↦ logLik_square hn hx hr.1 hr.2) hq
  have hp' : toGamma p ∈ domain := gamma_map hp.1
  refine ⟨toGamma p, ⟨hp', (ht _ hp').mpr ?_⟩, ?_⟩
  · rw [gamma_inverse hp.1]
    exact hp.2
  · rintro q ⟨hq, hmq⟩
    rw [← hu (toGamma q) ⟨gamma_map hq, (ht _ hq).mp hmq⟩, gamma_inverse hq]

/-- Holding shape fixes the unique second-moment estimate, even for constant data. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {m : ℝ} (hm : 0 < m) (Ω : ℝ) :
    Ω ∈ Ioi 0 ∧ IsMaxOn (logLik x m) (Ioi 0) Ω ↔ Ω = Gamma.avg (squared x) := by
  obtain ⟨_, hstrict⟩ := Gamma.heldShape hn (fun i ↦ sq_pos_of_pos (hx i)) hm
  have hpos : 0 < Gamma.avg (squared x) := by
    unfold Gamma.avg
    apply div_pos _ (by exact_mod_cast hn)
    exact Finset.sum_pos (fun i _ ↦ sq_pos_of_pos (hx i)) ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hlt (t : ℝ) (ht : t ∈ Ioi 0) (hne : t ≠ Gamma.avg (squared x)) :
      logLik x m t < logLik x m (Gamma.avg (squared x)) := by
    rw [logLik_square hn hx hm ht, logLik_square hn hx hm hpos]
    apply add_lt_add_left
    apply mul_lt_mul_of_pos_left _ hn'
    apply hstrict _ (div_pos hm ht)
    intro he
    apply hne
    change m / t = m / Gamma.avg (squared x) at he
    have hmul := (div_eq_div_iff (ne_of_gt ht) hpos.ne').mp he
    exact (mul_left_cancel₀ hm.ne' hmul).symm
  constructor
  · rintro ⟨hp, hmax⟩
    by_contra hne
    exact (not_lt_of_ge (hmax hpos)) (hlt Ω hp hne)
  · rintro rfl
    refine ⟨hpos, fun t ht ↦ ?_⟩
    by_cases h : t = Gamma.avg (squared x)
    · subst t
      exact le_rfl
    · exact (hlt t ht h).le

/-- At the common squared value, the log likelihood diverges with shape.
This applies both on the held-scale slice and in the joint domain. -/
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {Ω : ℝ} (hΩ : 0 < Ω) (h : ∀ i, squared x i = Ω) :
    Tendsto (fun m ↦ logLik x m Ω) atTop atTop := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have he : squared x = fun _ ↦ Ω := funext h
  apply Tendsto.congr' (f₁ := fun m ↦
    n * Gamma.logLik (fun _ : Fin n ↦ Ω) m (m / Ω) + ∑ i, log (2 * x i))
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with m hm
    rw [logLik_square hn hx hm hΩ, he]
  · exact tendsto_atTop_add_const_right _ _ ((Gamma.constant_unbounded hn hΩ).const_mul_atTop hn')

/-- The held-second-moment score target is the mean square-ratio deviance. -/
def heldGap (x : Fin n → ℝ) (Ω : ℝ) : ℝ :=
  Gamma.avg (fun i ↦ squared x i / Ω - 1 - log (squared x i / Ω))

private theorem heldGap_formula (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {Ω : ℝ} (hΩ : 0 < Ω) :
    heldGap x Ω = Gamma.avg (squared x) / Ω - 1 - Gamma.avgLog (squared x) + log Ω := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have he (i : Fin n) : log (squared x i / Ω) = log (squared x i) - log Ω :=
    log_div (sq_pos_of_pos (hx i)).ne' hΩ.ne'
  unfold heldGap Gamma.avgLog Gamma.avg
  simp only [he, Finset.sum_sub_distrib, ← Finset.sum_div, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  field_simp
  ring

private theorem heldGap_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {Ω : ℝ} (hΩ : 0 < Ω) (h : ∃ i, squared x i ≠ Ω) : 0 < heldGap x Ω := by
  obtain ⟨j, hj⟩ := h
  unfold heldGap Gamma.avg
  apply div_pos _ (by exact_mod_cast hn)
  apply Finset.sum_pos'
  · intro i _
    exact sub_nonneg.mpr (log_le_sub_one_of_pos (div_pos (sq_pos_of_pos (hx i)) hΩ))
  · refine ⟨j, Finset.mem_univ _, sub_pos.mpr (log_lt_sub_one_of_pos ?_ ?_)⟩
    · exact div_pos (sq_pos_of_pos (hx j)) hΩ
    · simpa [div_eq_iff hΩ.ne'] using hj

private theorem held_deriv (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {Ω : ℝ} (hΩ : 0 < Ω) {m : ℝ} (hm : 0 < m) :
    HasDerivAt (fun k ↦ Gamma.logLik (squared x) k (k / Ω))
      (Gamma.scoreGap m - heldGap x Ω) m := by
  let y := squared x
  have hd := (Gamma.hasDerivAt_profile (x := y) hn (fun i ↦ sq_pos_of_pos (hx i)) hm).add
    ((hasDerivAt_id m).mul_const (Gamma.jensenGap y - heldGap x Ω))
  have he : Gamma.scoreGap m - Gamma.jensenGap y +
      1 * (Gamma.jensenGap y - heldGap x Ω) = Gamma.scoreGap m - heldGap x Ω := by ring
  rw [he] at hd
  apply hd.congr_of_eventuallyEq
  filter_upwards [Ioi_mem_nhds hm] with k hk
  have hk' : 0 < k := hk
  change Gamma.logLik (squared x) k (k / Ω) =
    Gamma.profile y k + k * (Gamma.jensenGap y - heldGap x Ω)
  rw [Gamma.profile_eq (x := y) hn (fun i ↦ sq_pos_of_pos (hx i)) hk',
    heldGap_formula hn hx hΩ,
    Gamma.logLik, log_div hk'.ne' hΩ.ne']
  dsimp [y]
  ring

/-- At held second moment, the unique finite fit solves the gamma gap equation. -/
theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {Ω : ℝ} (hΩ : 0 < Ω) (h : ∃ i, squared x i ≠ Ω) :
    (∃! m, m ∈ Ioi 0 ∧ Gamma.scoreGap m = heldGap x Ω) ∧
    ∀ m ∈ Ioi 0, IsMaxOn (fun k ↦ logLik x k Ω) (Ioi 0) m ↔
      Gamma.scoreGap m = heldGap x Ω := by
  let f : ℝ → ℝ := fun k ↦ Gamma.logLik (squared x) k (k / Ω)
  let g : ℝ → ℝ := fun k ↦ Gamma.scoreGap k - heldGap x Ω
  have hd (k : ℝ) (hk : k ∈ Ioi 0) : HasDerivAt f (g k) k := held_deriv hn hx hΩ hk
  have hg : StrictAntiOn g (Ioi 0) :=
    fun _ ha _ hb hab ↦ sub_lt_sub_right (Gamma.scoreGap_strictAnti ha hb hab) _
  have hgap := heldGap_pos hn hx hΩ h
  obtain ⟨a, ha, hga⟩ := ((show ∀ᶠ t in 𝓝[>] (0 : ℝ), t ∈ Ioi (0 : ℝ) from self_mem_nhdsWithin).and
    (Gamma.scoreGap_tendsto_zero.eventually_gt_atTop (heldGap x Ω))).exists
  obtain ⟨b, hb, hgb⟩ := ((eventually_gt_atTop (0 : ℝ)).and
    (Gamma.scoreGap_tendsto_atTop.eventually (Iio_mem_nhds hgap))).exists
  have hga' : 0 < g a := sub_pos.mpr hga
  have hgb' : g b < 0 := sub_neg.mpr hgb
  have hab : a ≤ b := by
    by_contra! hba
    exact (hg hb ha hba).not_gt (hgb'.trans hga')
  have hsub : Icc a b ⊆ Ioi (0 : ℝ) := fun _ ht ↦ ha.trans_le ht.1
  obtain ⟨m, hm, hz⟩ := exists_hasDerivWithinAt_eq_of_lt_of_gt hab
    (fun k hk ↦ (hd k (hsub hk)).hasDerivWithinAt) hga' hgb'
  have hmD := ha.trans hm.1
  have hroot : Gamma.scoreGap m = heldGap x Ω := sub_eq_zero.mp hz
  refine ⟨⟨m, ⟨hmD, hroot⟩, fun t ht ↦
    hg.injOn ht.1 hmD ((sub_eq_zero.mpr ht.2).trans hz.symm)⟩, ?_⟩
  intro t ht
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have htransfer : IsMaxOn (fun k ↦ logLik x k Ω) (Ioi 0) t ↔ IsMaxOn f (Ioi 0) t :=
    max_transfer (T := id) hn' (fun _ hk ↦ hk) (fun k hk ↦ ⟨k, hk, rfl⟩)
      (fun k hk ↦ logLik_square hn hx hk hΩ) ht
  rw [htransfer]
  constructor
  · intro hmax'
    exact sub_eq_zero.mp ((hmax'.isLocalMax (Ioi_mem_nhds ht)).hasDerivAt_eq_zero (hd t ht))
  · intro hzero
    have ht0 : g t = 0 := sub_eq_zero.mpr hzero
    have hc : ContinuousOn f (Ioi 0) := fun k hk ↦ (hd k hk).continuousAt.continuousWithinAt
    intro q hq
    rcases lt_trichotomy q t with hqt | rfl | htq
    · have hsub' : Icc q t ⊆ Ioi (0 : ℝ) := fun _ hk ↦ hq.trans_le hk.1
      have hmono := strictMonoOn_of_deriv_pos (convex_Icc q t) (hc.mono hsub')
        (fun k hk ↦ by
          rw [interior_Icc] at hk
          have hkD := hq.trans hk.1
          rw [(hd k hkD).deriv, ← ht0]
          exact hg hkD ht hk.2)
      exact (hmono ⟨le_rfl, hqt.le⟩ ⟨hqt.le, le_rfl⟩ hqt).le
    · exact le_rfl
    · have hsub' : Icc t q ⊆ Ioi (0 : ℝ) := fun _ hk ↦ ht.trans_le hk.1
      have hanti := strictAntiOn_of_deriv_neg (convex_Icc t q) (hc.mono hsub')
        (fun k hk ↦ by
          rw [interior_Icc] at hk
          have hkD := ht.trans hk.1
          rw [(hd k hkD).deriv, ← ht0]
          exact hg ht hkD hk.1)
      exact (hanti ⟨le_rfl, htq.le⟩ ⟨htq.le, le_rfl⟩ htq).le

/-- At the joint maximum, the second moment is empirical and the shape
solves the Jensen-gap equation. -/
theorem joint_coefficients (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p : ℝ × ℝ} (hp : p ∈ domain)
    (hm : IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p) :
    p.2 = Gamma.avg (squared x) ∧ Gamma.scoreGap p.1 = Gamma.jensenGap (squared x) := by
  have hscale := (heldShape hn hx hp.1 p.2).mp
    ⟨hp.2, fun Ω hΩ ↦ hm (a := (p.1, Ω)) ⟨hp.1, hΩ⟩⟩
  have hne' : ∃ i, squared x i ≠ p.2 := by
    by_contra! he
    obtain ⟨i, j, hij⟩ := hne
    exact hij (square_inj (hx i) (hx j) ((he i).trans (he j).symm))
  have hscore := ((heldScale hn hx hp.2 hne').2 p.1 hp.1).mp
    (fun m hm' ↦ hm (a := (m, p.2)) ⟨hm', hp.2⟩)
  refine ⟨hscale, hscore.trans ?_⟩
  have hΩ : 0 < p.2 := hp.2
  rw [heldGap_formula hn hx hΩ, Gamma.jensenGap, ← hscale, div_self hΩ.ne']
  ring

/-- Constant positive samples have unbounded joint log likelihood. -/
theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q ↦ logLik x q.1 q.2) '' domain) := by
  let j : Fin n := ⟨0, hn⟩
  have hΩ : 0 < squared x j := sq_pos_of_pos (hx j)
  have he (i : Fin n) : squared x i = squared x j :=
    congrArg (fun u : ℝ ↦ u ^ 2) (not_ne_iff.mp fun hi ↦ h ⟨i, j, hi⟩)
  rintro ⟨M, hM⟩
  obtain ⟨m, hm, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
    ((constant_unbounded hn hx hΩ he).eventually_gt_atTop M)).exists
  exact hgt.not_ge (hM ⟨(m, squared x j), ⟨hm, hΩ⟩, rfl⟩)

/-- Holding both valid coefficients restricts the fit to one likelihood evaluation. -/
theorem bothHeld {m Ω : ℝ} (hm : 0 < m) (hΩ : 0 < Ω) (x : Fin n → ℝ) :
    domain ∩ {(m, Ω)} = {(m, Ω)} ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) {(m, Ω)} (m, Ω) := by
  have hp : (m, Ω) ∈ domain := ⟨hm, hΩ⟩
  constructor
  · ext p
    simp +contextual [hp]
  · intro p hp'
    obtain rfl := mem_singleton_iff.mp hp'
    exact le_rfl

end Nakagami

namespace HalfNormal

def residuals (x : Fin n → ℝ) (μ : ℝ) : Fin n → ℝ := fun i ↦ x i - μ

def scale (x : Fin n → ℝ) (μ : ℝ) : ℝ := sqrt ((∑ i, (x i - μ) ^ 2) / n)

def logLik (x : Fin n → ℝ) (μ σ : ℝ) : ℝ :=
  (n : ℝ) * (log (2 / π) / 2 - log σ) - (∑ i, (x i - μ) ^ 2) / (2 * σ ^ 2)

def domain (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {μ | ∀ i, μ ≤ x i} ×ˢ Ioi 0

/-- Folding the centered normal doubles its density at each supported point,
including the endpoint under the dossier's density convention. -/
theorem logLik_normal (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    logLik x μ σ = Normal.logLik (residuals x μ) 0 σ + n * log 2 := by
  rw [logLik, Normal.logLik_formula _ _ hσ, log_div (by norm_num : (2 : ℝ) ≠ 0) pi_pos.ne',
    log_mul (by norm_num : (2 : ℝ) ≠ 0) pi_pos.ne']
  simp only [Normal.residual, residuals, sub_zero]
  ring

private theorem max_scale_iff (x : Fin n → ℝ) (μ σ : ℝ) (hσ : 0 < σ) :
    IsMaxOn (logLik x μ) (Ioi 0) σ ↔
    IsMaxOn (Normal.logLik (residuals x μ) 0) (Ioi 0) σ :=
  max_transfer (T := id) (a := 1) (b := n * log 2) one_pos
    (fun _ ht ↦ ht) (fun t ht ↦ ⟨t, ht, rfl⟩)
    (fun t ht ↦ by simpa only [one_mul, id_eq] using logLik_normal x μ ht) hσ

/-- At a compatible held endpoint with a positive residual, the fitted scale is the residual RMS. -/
theorem heldLocation (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (_hx : ∀ i, μ ≤ x i) (h : ∃ i, μ < x i) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ ↔ σ = scale x μ := by
  have hp : ∃ i, residuals x μ i ≠ 0 := by
    obtain ⟨i, hi⟩ := h
    exact ⟨i, (sub_pos.mpr hi).ne'⟩
  have he : scale x μ = Normal.sd (residuals x μ) 0 := by
    simp only [scale, Normal.sd, Normal.residual, residuals, sub_zero]
  rw [he, ← Normal.heldMean hn hp σ]
  exact and_congr_right (max_scale_iff x μ σ)

/-- The held-endpoint scale collapses if every observation equals the endpoint. -/
theorem heldLocation_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, x i = μ) : ¬ BddAbove (logLik x μ '' Ioi 0) := by
  have h := Normal.heldMean_unbounded hn (μ := 0) (x := residuals x μ)
    (fun i ↦ sub_eq_zero.mpr (hx i))
  intro hb
  apply unbounded_add_const h (n * log 2)
  obtain ⟨M, hM⟩ := hb
  refine ⟨M, ?_⟩
  rintro _ ⟨σ, hσ, rfl⟩
  change Normal.logLik (residuals x μ) 0 σ + n * log 2 ≤ M
  rw [← logLik_normal x μ hσ]
  exact hM ⟨σ, hσ, rfl⟩

/-- A compatible held endpoint admits a unique scale exactly when a point exceeds it. -/
theorem existsUnique_heldLocation_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ ≤ x i) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔ ∃ i, μ < x i := by
  have h :
      (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔
        ∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (Normal.logLik (residuals x μ) 0) (Ioi 0) σ := by
    exact existsUnique_congr fun σ ↦ and_congr_right (max_scale_iff x μ σ)
  rw [h, Normal.existsUnique_heldMean_iff hn]
  exact exists_congr fun i ↦ by
    change x i - μ ≠ 0 ↔ μ < x i
    rw [sub_ne_zero]
    exact (hx i).lt_iff_ne'.symm

private theorem min_spec (hn : 0 < n) (x : Fin n → ℝ) :
    (∀ i, Endpoint.sampleMin hn x ≤ x i) ∧ ∃ i, x i = Endpoint.sampleMin hn x := by
  unfold Endpoint.sampleMin
  exact ⟨fun i ↦ Finset.min'_le _ _ (Finset.mem_image.mpr ⟨i, Finset.mem_univ _, rfl⟩),
    (Finset.mem_image.mp (Finset.min'_mem _ _)).imp fun _ h ↦ h.2⟩

private theorem endpoint_strict (hn : 0 < n) (x : Fin n → ℝ) {μ ν σ : ℝ}
    (hν : ∀ i, ν ≤ x i) (hμν : μ < ν) (hσ : 0 < σ) : logLik x μ σ < logLik x ν σ := by
  have hs : (∑ i, (x i - ν) ^ 2) < ∑ i, (x i - μ) ^ 2 := by
    apply Finset.sum_lt_sum
    · intro i _
      have h := sub_nonneg.mpr (hν i)
      nlinarith [mul_pos (sub_pos.mpr hμν) (show 0 < (x i - μ) + (x i - ν) by linarith)]
    · refine ⟨⟨0, hn⟩, Finset.mem_univ _, ?_⟩
      have h := sub_nonneg.mpr (hν ⟨0, hn⟩)
      nlinarith [mul_pos (sub_pos.mpr hμν)
        (show 0 < (x ⟨0, hn⟩ - μ) + (x ⟨0, hn⟩ - ν) by linarith)]
  unfold logLik
  exact sub_lt_sub_left (div_lt_div_of_pos_right hs (by positivity)) _

/-- At every positive held scale, the sample minimum uniquely maximizes the likelihood. -/
theorem heldScale (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ) (μ : ℝ) :
    μ ∈ {t | ∀ i, t ≤ x i} ∧
      IsMaxOn (fun t ↦ logLik x t σ) {t | ∀ i, t ≤ x i} μ ↔
    μ = Endpoint.sampleMin hn x := by
  obtain ⟨hx, j, hj⟩ := min_spec hn x
  constructor
  · rintro ⟨hμ, hmax⟩
    have hle : μ ≤ Endpoint.sampleMin hn x := hj ▸ hμ j
    exact hle.eq_of_not_lt fun h ↦ (endpoint_strict hn x hx h hσ).not_ge (hmax hx)
  · rintro rfl
    refine ⟨hx, fun t ht ↦ ?_⟩
    have hle : t ≤ Endpoint.sampleMin hn x := hj ▸ ht j
    rcases hle.lt_or_eq with h | rfl
    · exact (endpoint_strict hn x hx h hσ).le
    · exact le_rfl

/-- The free endpoint and scale are the sample minimum and its residual RMS. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hne : ∃ i j, x i ≠ x j) (p : ℝ × ℝ) :
    p ∈ domain x ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) (domain x) p ↔
    p = (Endpoint.sampleMin hn x, scale x (Endpoint.sampleMin hn x)) := by
  let e := Endpoint.sampleMin hn x
  have hx : ∀ i, e ≤ x i := (min_spec hn x).1
  have h : ∃ i, e < x i := by
    by_contra! he
    obtain ⟨i, j, hij⟩ := hne
    exact hij ((le_antisymm (he i) (hx i)).trans (le_antisymm (he j) (hx j)).symm)
  constructor
  · rintro ⟨hp, hm⟩
    have h1 := (heldScale hn x hp.2 p.1).mp
      ⟨hp.1, fun t ht ↦ hm (a := (t, p.2)) ⟨ht, hp.2⟩⟩
    have h2 := (heldLocation hn hx h p.2).mp
      ⟨hp.2, fun t ht ↦ by
        change logLik x e t ≤ logLik x e p.2
        rw [show e = p.1 from h1.symm]
        exact hm (a := (p.1, t)) ⟨hp.1, ht⟩⟩
    exact Prod.ext h1 h2
  · rintro rfl
    obtain ⟨hs, hmax⟩ := (heldLocation hn hx h (scale x e)).mpr rfl
    refine ⟨⟨hx, hs⟩, fun q hq ↦ ?_⟩
    exact (((heldScale hn x hq.2 e).mpr rfl).2 hq.1).trans (hmax hq.2)

/-- Constant data have unbounded likelihood along the sample-minimum slice. -/
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q ↦ logLik x q.1 q.2) '' domain x) := by
  obtain ⟨hx, j, hj⟩ := min_spec hn x
  have he (i : Fin n) : x i = Endpoint.sampleMin hn x := by
    rw [← hj]
    exact not_ne_iff.mp (fun hi ↦ h ⟨i, j, hi⟩)
  rintro ⟨M, hM⟩
  refine heldLocation_unbounded hn he ⟨M, ?_⟩
  rintro _ ⟨σ, hσ, rfl⟩
  exact hM ⟨(Endpoint.sampleMin hn x, σ), ⟨hx, hσ⟩, rfl⟩

/-- A unique joint fit exists exactly for nonconstant samples. -/
theorem existsUnique_mle_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃! p : ℝ × ℝ, p ∈ domain x ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) (domain x) p) ↔
    ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨p, hp, _⟩
    by_contra h
    exact no_max_of_unbounded (constant_unbounded hn h) hp.2
  · intro h
    exact existsUnique_of_fit_eq (jointMLE hn h)

end HalfNormal

end
end UnivariateML
