module

public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Real
public import Mathlib.Analysis.SpecialFunctions.Gamma.BohrMollerup
public import Mathlib.Analysis.SpecialFunctions.Complex.LogBounds
public import UnivariateML.Generic.Score
public import UnivariateML.Generic.Limit

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Student t likelihood at held shape

The ordinary t scale is used first. For shape above two the standard
deviation is a positive rescaling of this coordinate at held shape.

## Deviations

The strict score statement requires a nonzero residual, as in the dossiers.
With all residuals zero in a nonempty sample, the score is constant and
the negative-exponent collapse proof still applies. The joint result uses
a fixed-location path through an atom and makes no assertion about joint
maximizers at critical equality. All statements concern exact arithmetic.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators

namespace UnivariateML.TDist

noncomputable section

variable {n : ℕ}

def logNormalizer (ν : ℝ) : ℝ :=
  log (Gamma ((ν + 1) / 2)) - log (Gamma (ν / 2)) - log (ν * π) / 2

def logLik (x : Fin n → ℝ) (μ ν a : ℝ) : ℝ :=
  ∑ i, (logNormalizer ν - log a -
    (ν + 1) / 2 * log (1 + (x i - μ)^2 / (ν * a^2)))

def likelihood (x : Fin n → ℝ) (μ ν a : ℝ) : ℝ := exp (logLik x μ ν a)

def multiplicity (x : Fin n → ℝ) (μ : ℝ) : ℕ :=
  (Finset.univ.filter fun i ↦ x i = μ).card

def exponent (x : Fin n → ℝ) (μ ν : ℝ) : ℝ :=
  ν * ((n : ℝ) - multiplicity x μ) - multiplicity x μ

def scaleScore (x : Fin n → ℝ) (μ ν a : ℝ) : ℝ :=
  (∑ i, (ν + 1) * (x i - μ)^2 / (ν * a^2 + (x i - μ)^2)) - n

def collapseLogValue (x : Fin n → ℝ) (μ ν : ℝ) : ℝ :=
  n * logNormalizer ν + (ν + 1) / 2 *
    (((n : ℝ) - multiplicity x μ) * log ν -
      ∑ i ∈ Finset.univ.filter (fun i ↦ x i ≠ μ), log ((x i - μ)^2))

private def off (x : Fin n → ℝ) (μ : ℝ) : Finset (Fin n) :=
  Finset.univ.filter fun i ↦ x i ≠ μ

private theorem mem_off {x : Fin n → ℝ} {μ : ℝ} {i : Fin n} :
    i ∈ off x μ ↔ x i ≠ μ := by simp [off]

private theorem count_off (x : Fin n → ℝ) (μ : ℝ) :
    ((off x μ).card : ℝ) = (n : ℝ) - multiplicity x μ := by
  have h : multiplicity x μ + (off x μ).card = n := by
    unfold multiplicity off
    rw [Finset.card_filter_add_card_filter_not, Finset.card_univ, Fintype.card_fin]
  have h' : (multiplicity x μ : ℝ) + ((off x μ).card : ℝ) = n := by exact_mod_cast h
  linarith

private theorem sum_off (x : Fin n → ℝ) (μ : ℝ) (f : Fin n → ℝ)
    (h : ∀ i, x i = μ → f i = 0) : (∑ i, f i) = ∑ i ∈ off x μ, f i := by
  unfold off
  exact (Finset.sum_filter_of_ne (s := Finset.univ) (p := fun i ↦ x i ≠ μ)
    (fun i _ hi ↦ fun he ↦ hi (h i he))).symm

private theorem denom_pos {ν a : ℝ} (hν : 0 < ν) (ha : 0 < a) (d : ℝ) :
    0 < ν * a^2 + d^2 := by positivity

private theorem log_kernel {ν a : ℝ} (hν : 0 < ν) (ha : 0 < a) (d : ℝ) :
    log (1 + d^2 / (ν * a^2)) = log (ν * a^2 + d^2) - log ν - 2 * log a := by
  have h : (ν * a^2) ≠ 0 := by positivity
  rw [show 1 + d^2 / (ν * a^2) = (ν * a^2 + d^2) / (ν * a^2) by field_simp,
    log_div (denom_pos hν ha d).ne' h, log_mul hν.ne' (by positivity), log_pow]
  ring

/-- The displayed score is the derivative in log-scale coordinates. -/
theorem hasDerivAt_logLik (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν) (t : ℝ) :
    HasDerivAt (fun s ↦ logLik x μ ν (exp s)) (scaleScore x μ ν (exp t)) t := by
  have hterm (i : Fin n) :
      HasDerivAt
        (fun s ↦ logNormalizer ν - log (exp s) -
          (ν + 1) / 2 * log (1 + (x i - μ)^2 / (ν * exp s ^ 2)))
        ((ν + 1) * (x i - μ)^2 / (ν * exp t ^ 2 + (x i - μ)^2) - 1) t := by
    let d := x i - μ
    have hd := (((hasDerivAt_exp t).pow 2).const_mul ν).add_const (d^2)
    have hl := hd.log (denom_pos hν (exp_pos t) d).ne'
    have H := ((hasDerivAt_id t).const_sub (logNormalizer ν)).sub
      (((hl.sub_const (log ν)).sub ((hasDerivAt_id t).const_mul 2)).const_mul ((ν + 1) / 2))
    convert H using 1
    · ext s
      change _ - _ - _ * log (1 + d^2 / (ν * exp s ^ 2)) = _
      rw [log_kernel hν (exp_pos s), log_exp]
      rfl
    · change (ν + 1) * d^2 / (ν * exp t ^ 2 + d^2) - 1 =
        -1 - (ν + 1) / 2 * (ν * (2 * exp t ^ (2 - 1) * exp t) /
          (ν * exp t ^ 2 + d^2) - 2 * 1)
      norm_num
      field_simp [(denom_pos hν (exp_pos t) d).ne']
      ring
  simpa [scaleScore, logLik, Finset.sum_sub_distrib] using
    HasDerivAt.fun_sum (fun i (_ : i ∈ Finset.univ) ↦ hterm i)

/-- One nonzero residual makes the log-scale score strictly decreasing with scale. -/
theorem scaleScore_strictAntiOn (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν)
    (hx : ∃ i, x i ≠ μ) : StrictAntiOn (scaleScore x μ ν) (Ioi 0) := by
  intro a (ha : 0 < a) b (hb : 0 < b) hab
  have hden (i : Fin n) : ν * a^2 + (x i - μ)^2 < ν * b^2 + (x i - μ)^2 := by
    exact add_lt_add_left (mul_lt_mul_of_pos_left
      ((sq_lt_sq₀ ha.le hb.le).2 hab) hν) _
  unfold scaleScore
  apply sub_lt_sub_right
  obtain ⟨i, hi⟩ := hx
  exact Finset.sum_lt_sum (fun j _ ↦ div_le_div_of_nonneg_left (by positivity)
    (denom_pos hν ha _) (hden j).le) ⟨i, Finset.mem_univ _,
      div_lt_div_of_pos_left (mul_pos (by positivity)
        (sq_pos_of_ne_zero (sub_ne_zero.mpr hi))) (denom_pos hν ha _) (hden i)⟩

/-- The score starts at the collapse exponent and ends at minus the sample size. -/
theorem scaleScore_limits (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν) :
    Tendsto (scaleScore x μ ν) (𝓝[>] 0) (𝓝 (exponent x μ ν)) ∧
      Tendsto (scaleScore x μ ν) atTop (𝓝 (-(n : ℝ))) := by
  constructor
  · have h (i : Fin n) (hi : i ∈ off x μ) :
        Tendsto (fun a : ℝ ↦ (ν + 1) * (x i - μ)^2 / (ν * a^2 + (x i - μ)^2))
          (𝓝[>] 0) (𝓝 (ν + 1)) := by
      have hd := sq_pos_of_ne_zero (sub_ne_zero.mpr (mem_off.mp hi))
      have hc : ContinuousAt (fun a : ℝ ↦ (ν + 1) * (x i - μ)^2 /
          (ν * a^2 + (x i - μ)^2)) 0 :=
        continuousAt_const.div (by fun_prop) (by simpa using hd.ne')
      simpa [hd.ne', mul_div_cancel_right₀] using hc.continuousWithinAt (s := Ioi 0) |>.tendsto
    have H := (tendsto_finsetSum (s := off x μ) h).sub
      (tendsto_const_nhds (x := (n : ℝ)))
    simp only [Finset.sum_const, nsmul_eq_mul, count_off] at H
    have he : ((n : ℝ) - multiplicity x μ) * (ν + 1) - n = exponent x μ ν := by
      unfold exponent
      ring
    rw [he] at H
    refine H.congr' (Eventually.of_forall fun a ↦ ?_)
    unfold scaleScore
    rw [sum_off x μ _ (fun i hi ↦ by simp [hi])]
  · have h (i : Fin n) :
        Tendsto (fun a : ℝ ↦ (ν + 1) * (x i - μ)^2 / (ν * a^2 + (x i - μ)^2))
          atTop (𝓝 0) := by
      apply tendsto_const_nhds.div_atTop
      exact tendsto_atTop_add_const_right _ _
        ((tendsto_pow_atTop (by decide : 2 ≠ 0)).const_mul_atTop hν)
    unfold scaleScore
    simpa only [Finset.sum_const_zero, zero_sub] using
      (tendsto_finsetSum (s := Finset.univ) (fun i _ ↦ h i)).sub
        (tendsto_const_nhds (x := (n : ℝ)))

private theorem held_deriv (x : Fin n → ℝ) (μ : ℝ) {ν a : ℝ} (hν : 0 < ν) (ha : 0 < a) :
    HasDerivAt (logLik x μ ν) (scaleScore x μ ν a * a⁻¹) a := by
  have H := (hasDerivAt_logLik x μ hν (log a)).comp a (hasDerivAt_log ha.ne')
  rw [exp_log ha] at H
  refine H.congr_of_eventuallyEq ?_
  filter_upwards [Ioi_mem_nhds ha] with b (hb : 0 < b)
  exact (congrArg (logLik x μ ν) (exp_log hb)).symm

private theorem score_lt_exponent (x : Fin n → ℝ) (μ : ℝ) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) (hx : ∃ i, x i ≠ μ) :
    scaleScore x μ ν a < exponent x μ ν := by
  have hanti := scaleScore_strictAntiOn x μ hν hx
  refine (hanti (show a / 2 ∈ Ioi (0 : ℝ) from half_pos ha) ha (by linarith)).trans_le ?_
  apply ge_of_tendsto (scaleScore_limits x μ hν).1
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
    mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds (half_pos ha))] with b hb hba
  exact (hanti hb (half_pos ha) hba).le

private theorem held_strictAnti (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν)
    (hx : ∃ i, x i ≠ μ) (he : exponent x μ ν ≤ 0) :
    StrictAntiOn (logLik x μ ν) (Ioi 0) := by
  apply strictAntiOn_of_deriv_neg (convex_Ioi 0)
    (fun a ha ↦ (held_deriv x μ hν ha).continuousAt.continuousWithinAt)
  intro a ha
  have ha' := interior_subset ha
  rw [(held_deriv x μ hν ha').deriv]
  exact mul_neg_of_neg_of_pos ((score_lt_exponent x μ hν ha' hx).trans_le he) (inv_pos.mpr ha')

/-- A positive maximizing scale exists exactly at positive collapse exponent.
In that case it is unique and is the unique positive score zero. -/
theorem heldLocation_maximum (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν)
    (hx : ∃ i, x i ≠ μ) :
    ((∃ a ∈ Ioi 0, IsMaxOn (logLik x μ ν) (Ioi 0) a) ↔ 0 < exponent x μ ν) ∧
      (0 < exponent x μ ν →
        (∃! a, 0 < a ∧ scaleScore x μ ν a = 0) ∧
        ∀ a > 0, IsMaxOn (logLik x μ ν) (Ioi 0) a ↔ scaleScore x μ ν a = 0) := by
  have ⟨i, _⟩ := hx
  have hn : (0 : ℝ) < n := by exact_mod_cast (Nat.zero_lt_of_lt i.isLt)
  have H (he : 0 < exponent x μ ν) :=
    Generic.existsUnique_log_score_zero (hasDerivAt_logLik x μ hν)
      (show Continuous (fun t ↦ scaleScore x μ ν (exp t)) by
        unfold scaleScore
        apply Continuous.sub _ continuous_const
        apply continuous_finsetSum
        intro j _
        exact continuous_const.div (by fun_prop) fun t ↦ (denom_pos hν (exp_pos t) _).ne')
      (fun a b hab ↦ scaleScore_strictAntiOn x μ hν hx (exp_pos a) (exp_pos b) (exp_lt_exp.mpr hab))
      he (neg_neg_of_pos hn)
      ((scaleScore_limits x μ hν).1.comp tendsto_exp_atBot_nhdsGT)
      ((scaleScore_limits x μ hν).2.comp tendsto_exp_atTop)
  have R (he : 0 < exponent x μ ν) :
      (∃! a, 0 < a ∧ scaleScore x μ ν a = 0) ∧
        ∀ a > 0, IsMaxOn (logLik x μ ν) (Ioi 0) a ↔ scaleScore x μ ν a = 0 := by
    obtain ⟨⟨a, ⟨ha, hz⟩, hu⟩, hm⟩ := H he
    refine ⟨⟨a, ⟨ha, by simpa [exp_log ha] using hz⟩, ?_⟩, ?_⟩
    · intro b hb
      exact hu b ⟨hb.1, by simpa [exp_log hb.1] using hb.2⟩
    · intro b hb
      simpa [exp_log hb] using hm b hb
  refine ⟨⟨?_, ?_⟩, R⟩
  · rintro ⟨a, (ha : 0 < a), hm⟩
    by_contra! he
    exact (held_strictAnti x μ hν hx he (half_pos ha) ha (by linarith)).not_ge
      (hm (show a / 2 ∈ Ioi (0 : ℝ) from half_pos ha))
  · intro he
    obtain ⟨a, ⟨ha, hz⟩, _⟩ := (R he).1
    exact ⟨a, ha, ((R he).2 a ha).2 hz⟩

private def collapseRemainder (x : Fin n → ℝ) (μ ν a : ℝ) : ℝ :=
  n * logNormalizer ν + (ν + 1) / 2 *
    (((n : ℝ) - multiplicity x μ) * log ν -
      ∑ i ∈ off x μ, log (ν * a^2 + (x i - μ)^2))

private theorem collapse_formula (x : Fin n → ℝ) (μ : ℝ) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    logLik x μ ν a = exponent x μ ν * log a + collapseRemainder x μ ν a := by
  unfold logLik collapseRemainder exponent
  rw [Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  rw [sum_off x μ _ (fun i hi ↦ by simp [hi])]
  simp_rw [log_kernel hν ha]
  rw [Finset.sum_sub_distrib, Finset.sum_sub_distrib, Finset.sum_const, Finset.sum_const,
    nsmul_eq_mul, nsmul_eq_mul, count_off]
  ring

private theorem remainder_limit (x : Fin n → ℝ) (μ ν : ℝ) :
    Tendsto (collapseRemainder x μ ν) (𝓝[>] 0) (𝓝 (collapseLogValue x μ ν)) := by
  have h (i : Fin n) (hi : i ∈ off x μ) :
      Tendsto (fun a : ℝ ↦ log (ν * a^2 + (x i - μ)^2)) (𝓝[>] 0)
        (𝓝 (log ((x i - μ)^2))) := by
    have hc : ContinuousAt (fun a : ℝ ↦ ν * a^2 + (x i - μ)^2) 0 := by fun_prop
    have hne := (sq_pos_of_ne_zero (sub_ne_zero.mpr (mem_off.mp hi))).ne'
    simpa using ((hc.log (by simpa using hne)).continuousWithinAt (s := Ioi 0)).tendsto
  exact ((tendsto_const_nhds.sub (tendsto_finsetSum _ h)).const_mul _).const_add _

/-- At zero scale the likelihood divided by the collapse power has a
finite, strictly positive limit, including in the all-tied case. -/
theorem collapse_power (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν) :
    Tendsto (fun a ↦ likelihood x μ ν a / a ^ exponent x μ ν) (𝓝[>] 0)
      (𝓝 (exp (collapseLogValue x μ ν))) ∧ 0 < exp (collapseLogValue x μ ν) := by
  refine ⟨(continuous_exp.continuousAt.tendsto.comp (remainder_limit x μ ν)).congr' ?_, exp_pos _⟩
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with a (ha : 0 < a)
  rw [likelihood, collapse_formula x μ hν ha, exp_add, rpow_def_of_pos ha, mul_comm (log a),
    mul_div_cancel_left₀ _ (exp_pos _).ne']
  rfl

private theorem equality_limit (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : exponent x μ ν = 0) :
    Tendsto (logLik x μ ν) (𝓝[>] 0) (𝓝 (collapseLogValue x μ ν)) := by
  refine (remainder_limit x μ ν).congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with a ha
  simp [collapse_formula x μ hν ha, he]

/-- At critical equality the finite supremum is unattained. Every eventually
positive scale path whose log likelihood approaches it tends to zero. -/
theorem heldLocation_equality (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν)
    (hx : ∃ i, x i ≠ μ) (he : exponent x μ ν = 0) :
    Tendsto (logLik x μ ν) (𝓝[>] 0) (𝓝 (collapseLogValue x μ ν)) ∧
      sSup (logLik x μ ν '' Ioi 0) = collapseLogValue x μ ν ∧
      (∀ a > 0, logLik x μ ν a < collapseLogValue x μ ν) ∧
      ∀ {ι : Type*} {l : Filter ι} {a : ι → ℝ},
        (∀ᶠ i in l, 0 < a i) →
        Tendsto (fun i ↦ logLik x μ ν (a i)) l (𝓝 (collapseLogValue x μ ν)) →
        Tendsto a l (𝓝[>] 0) := by
  have ht := equality_limit x μ hν he
  have hanti := held_strictAnti x μ hν hx he.le
  have hbound (a : ℝ) (ha : 0 < a) : logLik x μ ν a ≤ collapseLogValue x μ ν := by
    apply ge_of_tendsto ht
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
      mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds ha)] with b hb hba
    exact (hanti hb ha hba).le
  have hlt (a : ℝ) (ha : 0 < a) : logLik x μ ν a < collapseLogValue x μ ν :=
    (hanti (half_pos ha) ha (by linarith)).trans_le (hbound _ (half_pos ha))
  have hub : collapseLogValue x μ ν ∈ upperBounds (logLik x μ ν '' Ioi 0) := by
    rintro _ ⟨a, ha, rfl⟩
    exact hbound a ha
  refine ⟨ht, le_antisymm (csSup_le ⟨_, 1, zero_lt_one, rfl⟩ hub)
    (Generic.limit_le_sSup (l := 𝓝[>] (0 : ℝ)) (p := fun a : ℝ ↦ a)
      (show ∀ᶠ a : ℝ in 𝓝[>] 0, a ∈ Ioi (0 : ℝ) from self_mem_nhdsWithin) ht ⟨_, hub⟩), hlt, ?_⟩
  intro ι l a ha hlim
  apply tendsto_inf.mpr
  refine ⟨Metric.tendsto_nhds.mpr ?_, tendsto_principal.mpr ha⟩
  intro ε hε
  filter_upwards [ha, hlim.eventually (Ioi_mem_nhds (hlt ε hε))] with i hi hval
  rw [Real.dist_eq, sub_zero, abs_of_pos hi]
  exact lt_of_not_ge fun h ↦ hval.not_ge (hanti.antitoneOn hε hi h)

/-- In the likelihood itself the critical supremum is strictly positive,
finite and unattained. -/
theorem heldLocation_likelihood_equality (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (hx : ∃ i, x i ≠ μ) (he : exponent x μ ν = 0) :
    Tendsto (likelihood x μ ν) (𝓝[>] 0) (𝓝 (exp (collapseLogValue x μ ν))) ∧
      sSup (likelihood x μ ν '' Ioi 0) = exp (collapseLogValue x μ ν) ∧
      ∀ a > 0, likelihood x μ ν a < exp (collapseLogValue x μ ν) := by
  have H := heldLocation_equality.{0} x μ hν hx he
  have ht : Tendsto (likelihood x μ ν) (𝓝[>] 0) (𝓝 (exp (collapseLogValue x μ ν))) :=
    continuous_exp.continuousAt.tendsto.comp H.1
  have hlt (a : ℝ) (ha : 0 < a) : likelihood x μ ν a < exp (collapseLogValue x μ ν) :=
    exp_lt_exp.mpr (H.2.2.1 a ha)
  have hub : exp (collapseLogValue x μ ν) ∈ upperBounds (likelihood x μ ν '' Ioi 0) := by
    rintro _ ⟨a, ha, rfl⟩
    exact (hlt a ha).le
  refine ⟨ht, le_antisymm (csSup_le ⟨_, 1, zero_lt_one, rfl⟩ hub) ?_, hlt⟩
  exact Generic.limit_le_sSup (l := 𝓝[>] (0 : ℝ)) (p := fun a : ℝ ↦ a)
    (show ∀ᶠ a : ℝ in 𝓝[>] 0, a ∈ Ioi (0 : ℝ) from self_mem_nhdsWithin) ht ⟨_, hub⟩

/-- When all observations are tied at the location, the exponent is
independent of the shape. -/
theorem exponent_all_tied (x : Fin n → ℝ) (μ ν : ℝ) (hx : ∀ i, x i = μ) :
    exponent x μ ν = -(n : ℝ) := by
  simp [exponent, multiplicity, hx]

/-- A negative exponent forces divergence along the constant-location path. -/
theorem collapse_unbounded (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν)
    (he : exponent x μ ν < 0) : Tendsto (likelihood x μ ν) (𝓝[>] 0) atTop := by
  have hd := (tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg he).atTop_add (remainder_limit x μ ν)
  apply tendsto_exp_atTop.comp (hd.congr' ?_)
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with a ha
  exact (collapse_formula x μ hν ha).symm

/-- A sample atom with a negative collapse exponent makes the joint
location–scale likelihood unbounded at held shape. -/
theorem joint_unbounded (x : Fin n → ℝ) {ν : ℝ} (hν : 0 < ν)
    (h : ∃ i, exponent x (x i) ν < 0) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) '' (univ ×ˢ Ioi 0)) := by
  obtain ⟨i, hi⟩ := h
  rintro ⟨b, hb⟩
  have hpos : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
  obtain ⟨a, ha, hgt⟩ := (hpos.and
    ((collapse_unbounded x (x i) hν hi).eventually (eventually_gt_atTop b))).exists
  exact hgt.not_ge (hb ⟨(x i, a), ⟨mem_univ _, ha⟩, rfl⟩)

/-- The ordinary t scale associated with standard deviation `σ`. -/
def ordinaryScale (ν σ : ℝ) : ℝ := σ * sqrt ((ν - 2) / ν)

def stdLogLik (x : Fin n → ℝ) (μ ν σ : ℝ) : ℝ :=
  ∑ i, (log (Gamma ((ν + 1) / 2)) - log (Gamma (ν / 2)) -
    log ((ν - 2) * π) / 2 - log σ -
    (ν + 1) / 2 * log (1 + (x i - μ)^2 / ((ν - 2) * σ^2)))

private theorem scale_factor_pos {ν : ℝ} (hν : 2 < ν) : 0 < sqrt ((ν - 2) / ν) :=
  sqrt_pos.mpr (div_pos (sub_pos.mpr hν) (by linarith))

/-- At held shape above two, the dossier's standard-deviation formula is
the ordinary t likelihood in the positive rescaled coordinate. -/
theorem stdLogLik_eq (x : Fin n → ℝ) (μ : ℝ) {ν σ : ℝ} (hν : 2 < ν) (hσ : 0 < σ) :
    stdLogLik x μ ν σ = logLik x μ ν (ordinaryScale ν σ) := by
  have hv : 0 < ν := by linarith
  have hm : 0 < ν - 2 := sub_pos.mpr hν
  have hc := scale_factor_pos hν
  have he : ν * ordinaryScale ν σ ^ 2 = (ν - 2) * σ^2 := by
    rw [ordinaryScale, mul_pow, sq_sqrt (by positivity)]
    field_simp
  have hl : log (ordinaryScale ν σ) = log σ + (log (ν - 2) - log ν) / 2 := by
    rw [ordinaryScale, log_mul hσ.ne' hc.ne', log_sqrt (by positivity), log_div hm.ne' hv.ne']
  unfold stdLogLik logLik logNormalizer
  apply Finset.sum_congr rfl
  intro i _
  rw [he, hl, log_mul hm.ne' pi_ne_zero, log_mul hv.ne' pi_ne_zero]
  ring

private theorem ordinaryScale_tendsto {ν : ℝ} (hν : 2 < ν) :
    Tendsto (ordinaryScale ν) (𝓝[>] 0) (𝓝[>] 0) := by
  apply tendsto_inf.mpr
  have h : Continuous (ordinaryScale ν) := continuous_id.mul continuous_const
  have ht := ((h.continuousAt (x := (0 : ℝ))).continuousWithinAt (s := Ioi 0)).tendsto
  refine ⟨by simpa [ordinaryScale] using ht, tendsto_principal.mpr ?_⟩
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with σ hσ
  exact mul_pos hσ (scale_factor_pos hν)

/-- The scale change preserves the full set of positive maximizing scales
and the attainable log-likelihood values. -/
theorem std_reparametrization (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 2 < ν) :
    stdLogLik x μ ν '' Ioi 0 = logLik x μ ν '' Ioi 0 ∧
      ∀ σ > 0, IsMaxOn (stdLogLik x μ ν) (Ioi 0) σ ↔
        IsMaxOn (logLik x μ ν) (Ioi 0) (ordinaryScale ν σ) := by
  let c := sqrt ((ν - 2) / ν)
  have hc : 0 < c := scale_factor_pos hν
  have hpos (a : ℝ) (ha : 0 < a) : 0 < a / c := div_pos ha hc
  have hright (a : ℝ) : ordinaryScale ν (a / c) = a := div_mul_cancel₀ a hc.ne'
  have he : stdLogLik x μ ν '' Ioi 0 = logLik x μ ν '' Ioi 0 := by
    ext y
    constructor
    · rintro ⟨σ, hσ, rfl⟩
      exact ⟨ordinaryScale ν σ, mul_pos hσ hc, (stdLogLik_eq x μ hν hσ).symm⟩
    · rintro ⟨a, ha, rfl⟩
      exact ⟨a / c, hpos a ha, (stdLogLik_eq x μ hν (hpos a ha)).trans
        (congrArg (logLik x μ ν) (hright a))⟩
  refine ⟨he, fun σ hσ ↦ ?_⟩
  constructor
  · intro hm a ha
    have H := hm (show a / c ∈ Ioi (0 : ℝ) from hpos a ha)
    change stdLogLik x μ ν (a / c) ≤ stdLogLik x μ ν σ at H
    rw [stdLogLik_eq x μ hν (hpos a ha), hright, stdLogLik_eq x μ hν hσ] at H
    exact H
  · intro hm τ hτ
    have H := hm (show ordinaryScale ν τ ∈ Ioi (0 : ℝ) from mul_pos hτ hc)
    change stdLogLik x μ ν τ ≤ stdLogLik x μ ν σ
    rwa [stdLogLik_eq x μ hν hτ, stdLogLik_eq x μ hν hσ]

/-- A positive rescaling carries the full score theorem to the std
log-standard-deviation coordinate. -/
theorem std_scaleScore (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 2 < ν)
    (hx : ∃ i, x i ≠ μ) :
    let S := scaleScore x μ ν ∘ ordinaryScale ν
    (∀ t, HasDerivAt (fun u ↦ stdLogLik x μ ν (exp u)) (S (exp t)) t) ∧
      StrictAntiOn S (Ioi 0) ∧
      Tendsto S (𝓝[>] 0) (𝓝 (exponent x μ ν)) ∧
      Tendsto S atTop (𝓝 (-(n : ℝ))) := by
  have hv : 0 < ν := by linarith
  let c := sqrt ((ν - 2) / ν)
  have hc : 0 < c := scale_factor_pos hν
  have hexp (t : ℝ) : exp (t + log c) = ordinaryScale ν (exp t) := by
    rw [exp_add, exp_log hc]
    rfl
  refine ⟨?_, ?_, (scaleScore_limits x μ hv).1.comp (ordinaryScale_tendsto hν),
    (scaleScore_limits x μ hv).2.comp (tendsto_id.atTop_mul_const hc)⟩
  · intro t
    have H := (hasDerivAt_logLik x μ hv (t + log c)).comp t
      ((hasDerivAt_id t).add_const (log c))
    rw [hexp, mul_one] at H
    refine H.congr_of_eventuallyEq (Eventually.of_forall fun u ↦ ?_)
    change stdLogLik x μ ν (exp u) = logLik x μ ν (exp (u + log c))
    rw [hexp, stdLogLik_eq x μ hν (exp_pos u)]
  · intro a (ha : 0 < a) b (hb : 0 < b) hab
    exact scaleScore_strictAntiOn x μ hv hx (mul_pos ha hc) (mul_pos hb hc)
      (mul_lt_mul_of_pos_right hab hc)

/-- The held-location maximum, critical limit, and divergence regimes
in the standard-deviation coordinate. -/
theorem std_heldLocation (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 2 < ν)
    (hx : ∃ i, x i ≠ μ) :
    ((∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (stdLogLik x μ ν) (Ioi 0) σ) ↔ 0 < exponent x μ ν) ∧
      (exponent x μ ν = 0 →
        Tendsto (stdLogLik x μ ν) (𝓝[>] 0) (𝓝 (collapseLogValue x μ ν)) ∧
        sSup (stdLogLik x μ ν '' Ioi 0) = collapseLogValue x μ ν ∧
        ∀ σ > 0, stdLogLik x μ ν σ < collapseLogValue x μ ν) ∧
      (exponent x μ ν < 0 →
        Tendsto (fun σ ↦ exp (stdLogLik x μ ν σ)) (𝓝[>] 0) atTop) := by
  have hv : 0 < ν := by linarith
  obtain ⟨himage, hmax⟩ := std_reparametrization x μ hν
  have hc := scale_factor_pos hν
  have hpos : ∀ᶠ σ : ℝ in 𝓝[>] 0, 0 < σ := self_mem_nhdsWithin
  refine ⟨⟨?_, ?_⟩, ?_, ?_⟩
  · rintro ⟨σ, ⟨hσ, hm⟩, _⟩
    exact (heldLocation_maximum x μ hv hx).1.mp ⟨ordinaryScale ν σ, mul_pos hσ hc,
      (hmax σ hσ).mp hm⟩
  · intro he
    obtain ⟨⟨a, ⟨ha, hz⟩, hu⟩, hm⟩ := (heldLocation_maximum x μ hv hx).2 he
    let c := sqrt ((ν - 2) / ν)
    have hp : 0 < a / c := div_pos ha hc
    have ht : ordinaryScale ν (a / c) = a := div_mul_cancel₀ a hc.ne'
    have hopt : IsMaxOn (logLik x μ ν) (Ioi 0) (ordinaryScale ν (a / c)) := by
      rw [ht]
      exact (hm a ha).mpr hz
    refine ⟨a / c, ⟨hp, (hmax _ hp).mpr hopt⟩, ?_⟩
    rintro σ ⟨hσ, hσm⟩
    have hscale := hu (ordinaryScale ν σ) ⟨mul_pos hσ hc,
      (hm _ (mul_pos hσ hc)).mp ((hmax σ hσ).mp hσm)⟩
    exact (eq_div_iff hc.ne').mpr hscale
  · intro he
    obtain ⟨ht, hs, hl, _⟩ := heldLocation_equality.{0} x μ hv hx he
    refine ⟨(ht.comp (ordinaryScale_tendsto hν)).congr' ?_, by rw [himage, hs], ?_⟩
    · exact hpos.mono fun σ hσ ↦ (stdLogLik_eq x μ hν hσ).symm
    · intro σ hσ
      rw [stdLogLik_eq x μ hν hσ]
      exact hl _ (mul_pos hσ hc)
  · intro he
    exact ((collapse_unbounded x μ hv he).comp (ordinaryScale_tendsto hν)).congr'
      (hpos.mono fun σ hσ ↦ congrArg exp (stdLogLik_eq x μ hν hσ).symm)

/-- A maximizing path at critical equality must also collapse in the
standard-deviation coordinate. -/
theorem std_equality_path {ι : Type*} {l : Filter ι} {σ : ι → ℝ}
    (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 2 < ν) (hx : ∃ i, x i ≠ μ)
    (he : exponent x μ ν = 0) (hσ : ∀ᶠ i in l, 0 < σ i)
    (hlim : Tendsto (fun i ↦ stdLogLik x μ ν (σ i)) l (𝓝 (collapseLogValue x μ ν))) :
    Tendsto σ l (𝓝[>] 0) := by
  have hv : 0 < ν := by linarith
  have hc := scale_factor_pos hν
  have hp : ∀ᶠ i in l, 0 < ordinaryScale ν (σ i) := hσ.mono fun _ hi ↦ mul_pos hi hc
  have ht : Tendsto (fun i ↦ ordinaryScale ν (σ i)) l (𝓝[>] 0) :=
    (heldLocation_equality x μ hv hx he).2.2.2 hp
      (hlim.congr' (hσ.mono fun _ hi ↦ stdLogLik_eq x μ hν hi))
  have hz := ((tendsto_inf.mp ht).1.div_const (sqrt ((ν - 2) / ν)))
  simp only [zero_div] at hz
  exact tendsto_inf.mpr ⟨hz.congr (fun i ↦ mul_div_cancel_right₀ (σ i) hc.ne'),
    tendsto_principal.mpr hσ⟩

/-- A negative exponent at a sample atom also excludes a bounded joint
likelihood in the std parameterization. -/
theorem std_joint_unbounded (x : Fin n → ℝ) {ν : ℝ} (hν : 2 < ν)
    (h : ∃ i, exponent x (x i) ν < 0) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ exp (stdLogLik x p.1 ν p.2)) '' (univ ×ˢ Ioi 0)) := by
  have hv : 0 < ν := by linarith
  have hc := scale_factor_pos hν
  intro ⟨b, hb⟩
  apply joint_unbounded x hv h
  refine ⟨b, ?_⟩
  rintro _ ⟨⟨μ, a⟩, ⟨_, ha⟩, rfl⟩
  let σ := a / sqrt ((ν - 2) / ν)
  have hσ : 0 < σ := div_pos ha hc
  have hr : ordinaryScale ν σ = a := div_mul_cancel₀ a hc.ne'
  have H := hb (mem_image_of_mem (fun p : ℝ × ℝ ↦ exp (stdLogLik x p.1 ν p.2))
    (show (μ, σ) ∈ univ ×ˢ Ioi 0 from ⟨mem_univ _, hσ⟩))
  change exp (stdLogLik x μ ν σ) ≤ b at H
  rwa [stdLogLik_eq x μ hν hσ, hr] at H

private theorem logGamma_step {t : ℝ} (ht : 0 < t) :
    log (Gamma (t + 1)) = log t + log (Gamma t) := by
  rw [Gamma_add_one ht.ne', log_mul ht.ne' (Gamma_pos_of_pos ht).ne']

-- Two midpoint inequalities for log Γ bound the half-step ratio.
private theorem normalizer_bounds {t : ℝ} (ht : 0 < t) :
    (log t - log (t + 1 / 2)) / 2 ≤
      log (Gamma (t + 1 / 2)) - log (Gamma t) - log t / 2 ∧
    log (Gamma (t + 1 / 2)) - log (Gamma t) - log t / 2 ≤ 0 := by
  have midpoint {u : ℝ} (hu : 0 < u) :
      log (Gamma (u + 1 / 2)) ≤ (log (Gamma u) + log (Gamma (u + 1))) / 2 := by
    have H := convexOn_log_Gamma.2 (show u ∈ Ioi 0 from hu)
      (show u + 1 ∈ Ioi 0 from add_pos hu one_pos)
      (show 0 ≤ (1 : ℝ) / 2 by norm_num) (show 0 ≤ (1 : ℝ) / 2 by norm_num)
      (show (1 : ℝ) / 2 + 1 / 2 = 1 by norm_num)
    simp only [Function.comp_apply, smul_eq_mul] at H
    rw [show 1 / 2 * u + 1 / 2 * (u + 1) = u + 1 / 2 by ring] at H
    linarith only [H]
  have hhalf : 0 < t + 1 / 2 := by positivity
  have h₁ := midpoint ht
  have h₂ := midpoint hhalf
  rw [logGamma_step ht] at h₁
  rw [show t + 1 / 2 + 1 / 2 = t + 1 by ring, logGamma_step ht,
    logGamma_step hhalf] at h₂
  exact ⟨by linarith only [h₂], by linarith only [h₁]⟩

private theorem normalizer_limit :
    Tendsto logNormalizer atTop (𝓝 (-(log (2 * π) / 2))) := by
  have hbase : Tendsto (fun t : ℝ ↦ log t - log (t + 1 / 2)) atTop (𝓝 0) := by
    have H := (tendsto_const_nhds (x := (1 / 2 : ℝ))).div_atTop tendsto_id
    have hl := ((tendsto_const_nhds (x := (1 : ℝ))).add H).log (by norm_num)
    simp only [add_zero, log_one] at hl
    have hl' := hl.neg
    rw [neg_zero] at hl'
    refine hl'.congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    have ht' : 0 < t + 1 / 2 := by positivity
    change -log (1 + 1 / 2 / t) = _
    rw [show 1 + 1 / 2 / t = (t + 1 / 2) / t by field_simp, log_div ht'.ne' ht.ne']
    ring
  have hg : Tendsto (fun t : ℝ ↦ log (Gamma (t + 1 / 2)) - log (Gamma t) - log t / 2)
      atTop (𝓝 0) := by
    have hl : Tendsto (fun t : ℝ ↦ (log t - log (t + 1 / 2)) / 2) atTop (𝓝 0) := by
      simpa only [zero_div] using hbase.div_const 2
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' hl tendsto_const_nhds ?_ ?_
    · exact (eventually_gt_atTop (0 : ℝ)).mono fun t ht ↦ (normalizer_bounds ht).1
    · exact (eventually_gt_atTop (0 : ℝ)).mono fun t ht ↦ (normalizer_bounds ht).2
  have ht : Tendsto (fun ν : ℝ ↦ ν / 2) atTop atTop :=
    tendsto_id.atTop_div_const (by norm_num)
  have H := ((hg.comp ht).sub (tendsto_const_nhds (x := log (2 * π) / 2)))
  simp only [zero_sub] at H
  refine H.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with ν hν
  change log (Gamma (ν / 2 + 1 / 2)) - log (Gamma (ν / 2)) -
    log (ν / 2) / 2 - log (2 * π) / 2 = logNormalizer ν
  have hl : log (ν / 2) + log (2 * π) = log (ν * π) := by
    rw [← log_mul (by positivity) (by positivity)]
    congr 1
    ring
  rw [logNormalizer, show ν / 2 + 1 / 2 = (ν + 1) / 2 by ring]
  linarith only [hl]

def density (μ ν a y : ℝ) : ℝ :=
  exp (logNormalizer ν - log a - (ν + 1) / 2 * log (1 + (y - μ)^2 / (ν * a^2)))

def normalDensity (μ a y : ℝ) : ℝ :=
  exp (-(log (2 * π) / 2) - log a - (y - μ)^2 / a^2 / 2)

/-- The ordinary t density converges pointwise to the normal density even
when the ordinary scales vary and tend to a positive limit. -/
theorem density_tendsto_normal (μ y : ℝ) {a : ℝ → ℝ} {s : ℝ}
    (hs : 0 < s) (ha : Tendsto a atTop (𝓝 s)) :
    Tendsto (fun ν ↦ density μ ν (a ν) y) atTop (𝓝 (normalDensity μ s y)) := by
  let q := fun ν : ℝ ↦ (y - μ)^2 / (ν * (a ν)^2)
  have hpos := ha.eventually (Ioi_mem_nhds hs)
  have hq : Tendsto (fun ν ↦ ν * q ν) atTop (𝓝 ((y - μ)^2 / s^2)) := by
    refine (tendsto_const_nhds.div (ha.pow 2) (by positivity)).congr' ?_
    filter_upwards [hpos, eventually_gt_atTop (0 : ℝ)] with ν (h : 0 < a ν) hν
    dsimp [q]
    field_simp
  have hlog := tendsto_mul_log_one_add_of_tendsto hq
  have hr : Tendsto (fun ν : ℝ ↦ (1 + 1 / ν) / 2) atTop (𝓝 (1 / 2)) := by
    simpa using ((tendsto_const_nhds (x := (1 : ℝ))).add
      ((tendsto_const_nhds (x := (1 : ℝ))).div_atTop tendsto_id)).div_const 2
  have hk : Tendsto (fun ν ↦ (ν + 1) / 2 * log (1 + q ν)) atTop
      (𝓝 ((y - μ)^2 / s^2 / 2)) := by
    have H := hr.mul hlog
    rw [show 1 / 2 * ((y - μ)^2 / s^2) = (y - μ)^2 / s^2 / 2 by ring] at H
    refine H.congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with ν hν
    field_simp
  exact continuous_exp.continuousAt.tendsto.comp
    ((normalizer_limit.sub (ha.log hs.ne')).sub hk)

/-- The normal face for the standard Student t at fixed standard deviation. -/
theorem std_density_tendsto_normal (μ y : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun ν ↦ density μ ν (ordinaryScale ν σ) y) atTop (𝓝 (normalDensity μ σ y)) := by
  have h : Tendsto (fun ν : ℝ ↦ (ν - 2) / ν) atTop (𝓝 1) := by
    have H := (tendsto_const_nhds (x := (1 : ℝ))).sub
      ((tendsto_const_nhds (x := (2 : ℝ))).div_atTop tendsto_id)
    simp only [sub_zero] at H
    refine H.congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with ν hν
    change 1 - 2 / ν = (ν - 2) / ν
    field_simp
  apply density_tendsto_normal μ y hσ
  simpa [ordinaryScale] using h.sqrt.const_mul σ

end

end UnivariateML.TDist
