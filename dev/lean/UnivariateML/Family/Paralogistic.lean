module

public import UnivariateML.Family.BurrHeld
public import UnivariateML.Family.Transfers
public import UnivariateML.Generic.Score
public import UnivariateML.Generic.Escape

import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Paralogistic and inverse paralogistic held fits

The two Burr shapes are equal. The fixed-shape score is monotone in the
intercept. For fixed scale, the negative log ratios give a sufficient
curvature bound, and both shape endpoints escape to minus infinity.

## Deviations

The held-scale tie proof gives an unconditional crossing of the two compact
band heights. Global maximality at the crossing still assumes the compact
exclusion check from the source report.
-/

@[expose] public section

open Real Set Filter Topology BigOperators

private theorem id_bij : BijOn id (Ioi (0 : ℝ)) (Ioi 0) :=
  ⟨fun _ h ↦ h, fun _ _ _ _ h ↦ h, fun a ha ↦ ⟨a, ha, rfl⟩⟩

namespace UnivariateML.Paralogistic

noncomputable section

variable {n : ℕ}

open BurrHeld (avg softplus logRatios)

def logLik (x : Fin n → ℝ) (a s : ℝ) : ℝ := BurrHeld.logLik x a a s

private theorem cast_pos (hn : 0 < n) : 0 < (n : ℝ) := by exact_mod_cast hn

private theorem avg_const (hn : 0 < n) (c : ℝ) : avg (fun _ : Fin n ↦ c) = c := by
  simp [avg, (cast_pos hn).ne']

private theorem avg_mono {f g : Fin n → ℝ} (h : ∀ i, f i ≤ g i) : avg f ≤ avg g :=
  div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ h i) (Nat.cast_nonneg n)

private theorem avg_nonneg {f : Fin n → ℝ} (h : ∀ i, 0 ≤ f i) : 0 ≤ avg f :=
  div_nonneg (Finset.sum_nonneg fun i _ ↦ h i) (Nat.cast_nonneg n)

private theorem avg_deriv {f : Fin n → ℝ → ℝ} {d : Fin n → ℝ} {a : ℝ}
    (h : ∀ i, HasDerivAt (f i) (d i) a) :
    HasDerivAt (fun t ↦ avg (fun i ↦ f i t)) (avg d) a :=
  (HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ h i).div_const (n : ℝ)

private theorem avg_tendsto {F : Filter ℝ} {f : ℝ → Fin n → ℝ} {v : Fin n → ℝ}
    (h : ∀ i, Tendsto (fun t ↦ f t i) F (𝓝 (v i))) :
    Tendsto (fun t ↦ avg (f t)) F (𝓝 (avg v)) :=
  (tendsto_finsetSum _ fun i _ ↦ h i).div_const _

private theorem softplus_pos (t : ℝ) : 0 < softplus t :=
  log_pos (by linarith [exp_pos t])

private theorem softplus_shift (t : ℝ) : softplus t = t + softplus (-t) := by
  have he : exp t * (1 + exp (-t)) = 1 + exp t := by
    rw [mul_add, mul_one, ← exp_add, add_neg_cancel, exp_zero, add_comm]
  calc
    softplus t = log (exp t * (1 + exp (-t))) := by rw [he, softplus]
    _ = t + softplus (-t) := by rw [log_mul (exp_pos _).ne' (by positivity), log_exp, softplus]

private theorem softplus_mono : Monotone softplus :=
  fun u v huv ↦ log_le_log (by positivity) (by gcongr)

def sigmoid (t : ℝ) : ℝ := exp t / (1 + exp t)

private theorem softplus_deriv (t : ℝ) : HasDerivAt softplus (sigmoid t) t :=
  ((hasDerivAt_exp t).const_add 1).log (by positivity)

private theorem sigmoid_deriv (t : ℝ) :
    HasDerivAt sigmoid (exp t / (1 + exp t)^2) t := by
  unfold sigmoid
  convert (hasDerivAt_exp t).div ((hasDerivAt_exp t).const_add 1) (by positivity) using 1
  ring

private theorem sigmoid_strictMono : StrictMono sigmoid := by
  apply strictMono_of_deriv_pos
  intro t
  rw [(sigmoid_deriv t).deriv]
  positivity

private theorem sigmoid_atBot : Tendsto sigmoid atBot (𝓝 0) := by
  change Tendsto (fun t ↦ exp t / (1 + exp t)) atBot (𝓝 0)
  simpa [Pi.div_def] using tendsto_exp_atBot.div (tendsto_exp_atBot.const_add 1)
    (by norm_num : (1 : ℝ) + 0 ≠ 0)

private theorem sigmoid_atTop : Tendsto sigmoid atTop (𝓝 1) := by
  have he (t : ℝ) : sigmoid t = 1 / (1 + exp (-t)) := by
    rw [sigmoid, exp_neg]
    have ht := exp_pos t
    field_simp
    ring
  rw [show sigmoid = (fun t ↦ 1 / (1 + exp (-t))) from funext he]
  simpa [Pi.div_def] using (tendsto_const_nhds (x := (1 : ℝ))).div
    ((tendsto_exp_atBot.comp tendsto_neg_atTop_atBot).const_add 1)
    (by norm_num : (1 : ℝ) + 0 ≠ 0)

private def interceptLik (y : Fin n → ℝ) (a b : ℝ) : ℝ :=
  avg (fun i ↦ a * y i + b - (a + 1) * softplus (a * y i + b))

def interceptScore (y : Fin n → ℝ) (a b : ℝ) : ℝ :=
  avg (fun i ↦ 1 - (a + 1) * sigmoid (a * y i + b))

private theorem intercept_deriv (y : Fin n → ℝ) (a b : ℝ) :
    HasDerivAt (interceptLik y a) (interceptScore y a b) b := by
  apply avg_deriv
  intro i
  have h := (hasDerivAt_id b).const_add (a * y i)
  simpa [Pi.sub_def] using h.sub (((softplus_deriv (a * y i + b)).comp b h).const_mul (a + 1))

/-- The intercept score decreases strictly. -/
theorem interceptScore_strictAnti (hn : 0 < n) (y : Fin n → ℝ) {a : ℝ} (ha : 0 < a) :
    StrictAnti (interceptScore y a) := by
  intro b c hbc
  apply div_lt_div_of_pos_right _ (cast_pos hn)
  exact Finset.sum_lt_sum_of_nonempty (Finset.univ_nonempty_iff.mpr ⟨⟨0, hn⟩⟩)
    (fun i _ ↦ sub_lt_sub_left (mul_lt_mul_of_pos_left
      (sigmoid_strictMono (show a * y i + b < a * y i + c by linarith))
      (show 0 < a + 1 by positivity)) 1)

private theorem intercept_max (hn : 0 < n) (y : Fin n → ℝ) {a : ℝ} (ha : 0 < a) :
    (∃! b, interceptScore y a b = 0) ∧
      ∀ b, IsMaxOn (interceptLik y a) univ b ↔ interceptScore y a b = 0 := by
  apply Generic.existsUnique_score_zero_real (intercept_deriv y a)
  · exact continuous_iff_continuousAt.mpr fun b ↦ (avg_deriv fun i ↦
      (((sigmoid_deriv (a * y i + b)).comp b
        ((hasDerivAt_id b).const_add (a * y i))).const_mul (a + 1)).const_sub 1).continuousAt
  · exact interceptScore_strictAnti hn y ha
  · exact zero_lt_one
  · exact neg_neg_of_pos ha
  · have h := avg_tendsto (v := fun _ : Fin n ↦ 1) (F := atBot) (f := fun b i ↦
          1 - (a + 1) * sigmoid (a * y i + b)) fun i ↦ by
        simpa using ((sigmoid_atBot.comp (tendsto_atBot_add_const_left _ _ tendsto_id)).const_mul
          (a + 1)).const_sub 1
    rwa [avg_const hn] at h
  · have h := avg_tendsto (v := fun _ : Fin n ↦ -a) (F := atTop) (f := fun b i ↦
          1 - (a + 1) * sigmoid (a * y i + b)) fun i ↦ by
        simpa using ((sigmoid_atTop.comp (tendsto_atTop_add_const_left _ _ tendsto_id)).const_mul
          (a + 1)).const_sub 1
    rwa [avg_const hn] at h

private theorem logLik_intercept (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (a : ℝ) {s : ℝ} (hs : 0 < s) :
    logLik x a s = interceptLik (fun i ↦ log (x i)) a (-a * log s) +
      (2 * log a - avg (fun i ↦ log (x i))) := by
  unfold logLik BurrHeld.logLik interceptLik
  have h (i : Fin n) : a * log (x i / s) = a * log (x i) + -a * log s := by
    rw [log_div (hx i).ne' hs.ne']
    ring
  simp only [BurrHeld.logDensity, h, avg, Finset.sum_sub_distrib, Finset.sum_add_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, add_div, sub_div]
  field_simp [(cast_pos hn).ne']
  ring

private def scaleToIntercept (a s : ℝ) : ℝ := -a * log s

private theorem scaleToIntercept_bij {a : ℝ} (ha : 0 < a) :
    BijOn (scaleToIntercept a) (Ioi 0) univ := by
  refine ⟨fun _ _ ↦ mem_univ _, ?_, ?_⟩
  · intro s hs t ht h
    have e : log s = log t := mul_left_cancel₀ (neg_ne_zero.mpr ha.ne') h
    exact log_injOn_pos hs ht e
  · intro b _
    refine ⟨exp (b / -a), exp_pos _, ?_⟩
    rw [scaleToIntercept, log_exp]
    field_simp [ha.ne']

/-- Holding shape gives a unique positive scale. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃! s, s ∈ Ioi 0 ∧ IsMaxOn (logLik x a) (Ioi 0) s := by
  obtain ⟨⟨b, hb, hu⟩, hm⟩ := intercept_max hn (fun i ↦ log (x i)) ha
  apply (Transfer.existsUnique_max_iff (scaleToIntercept_bij ha)
    (fun s hs ↦ logLik_intercept hn hx a (show 0 < s from hs))).mpr
  exact ⟨b, ⟨mem_univ _, (hm b).mpr hb⟩, fun t ht ↦ hu t ((hm t).mp ht.2)⟩

/-- A feasible held-shape scale maximizes the likelihood exactly when
its intercept score vanishes. -/
theorem heldShape_iff_score_zero (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a s : ℝ} (ha : 0 < a) (hs : 0 < s) :
    IsMaxOn (logLik x a) (Ioi 0) s ↔
      interceptScore (fun i ↦ log (x i)) a (-a * log s) = 0 :=
  (Transfer.isMaxOn_iff (scaleToIntercept_bij ha)
    (fun t ht ↦ logLik_intercept hn hx a (show 0 < t from ht)) hs).trans
    ((intercept_max hn (fun i ↦ log (x i)) ha).2 _)

/-- On constant data the parent scale is the observation times
`exp (log a / a)`. -/
theorem heldShape_constant (hn : 0 < n) {c a : ℝ} (hc : 0 < c) (ha : 0 < a) :
    IsMaxOn (logLik (fun _ : Fin n ↦ c) a) (Ioi 0) (c * exp (log a / a)) := by
  have hs : 0 < c * exp (log a / a) := by positivity
  apply (Transfer.isMaxOn_iff (scaleToIntercept_bij ha)
    (fun s hs ↦ logLik_intercept hn (fun _ ↦ hc) a (show 0 < s from hs)) hs).mpr
  apply (intercept_max hn (fun _ : Fin n ↦ log c) ha).2 _ |>.mpr
  have hz : a * log c + scaleToIntercept a (c * exp (log a / a)) = -log a := by
    rw [scaleToIntercept, log_mul hc.ne' (exp_pos _).ne', log_exp]
    field_simp [ha.ne']
    ring
  simp only [interceptScore, hz]
  rw [avg_const hn, sigmoid, exp_neg, exp_log ha]
  field_simp
  ring

/-- The held-scale criterion in log ratios, with the data-only term removed. -/
def shapeLik (y : Fin n → ℝ) (a : ℝ) : ℝ :=
  2 * log a - avg (fun i ↦ a * softplus (a * y i) + softplus (-(a * y i)))

private theorem logLik_heldScale (hn : 0 < n) (x : Fin n → ℝ) (a s : ℝ) :
    logLik x a s = shapeLik (logRatios x s) a - avg (fun i ↦ log (x i)) := by
  rw [logLik, BurrHeld.logLik_eq_scale hn]
  simp only [shapeLik, BurrHeld.scaleLik, BurrHeld.G, BurrHeld.R, avg,
    Finset.sum_add_distrib, ← Finset.mul_sum, add_div]
  ring

private theorem shapeLik_upper (y : Fin n → ℝ) {a : ℝ} (ha : 0 < a) :
    shapeLik y a ≤ 2 * log a :=
  sub_le_self _ (avg_nonneg fun i ↦ by
    have := softplus_pos (a * y i)
    have := softplus_pos (-(a * y i))
    positivity)

private theorem linear_penalty (y : ℝ) :
    ∃ d > 0, ∀ a ≥ 1, d * a ≤ a * softplus (a * y) + softplus (-(a * y)) := by
  by_cases hy : y = 0
  · refine ⟨softplus 0, softplus_pos 0, fun a _ ↦ ?_⟩
    simp only [hy, mul_zero, neg_zero]
    linarith [softplus_pos 0]
  refine ⟨|y|, abs_pos.mpr hy, fun a ha ↦ ?_⟩
  have hge (z : ℝ) : z ≤ softplus z := by
    rw [softplus_shift z]
    linarith [softplus_pos (-z)]
  by_cases hs : 0 ≤ y
  · rw [abs_of_nonneg hs, mul_comm y a]
    have hp := hge (a * y)
    have hq := softplus_pos (a * y)
    nlinarith [softplus_pos (-(a * y)), mul_nonneg (sub_nonneg.mpr ha) hq.le]
  · rw [abs_of_neg (lt_of_not_ge hs), neg_mul, mul_comm y a]
    have hp := hge (-(a * y))
    have hq := softplus_pos (a * y)
    nlinarith [mul_nonneg (show 0 ≤ a by linarith) hq.le]

private theorem shapeLik_upper_large (hn : 0 < n) (y : Fin n → ℝ) :
    ∃ δ > 0, ∀ a ≥ 1, shapeLik y a ≤ 2 * log a - δ * a := by
  let j : Fin n := ⟨0, hn⟩
  obtain ⟨d, hd, h⟩ := linear_penalty (y j)
  refine ⟨d / n, div_pos hd (cast_pos hn), fun a ha ↦ ?_⟩
  apply sub_le_sub_left
  rw [div_mul_eq_mul_div]
  apply div_le_div_of_nonneg_right _ (Nat.cast_nonneg n)
  exact (h a ha).trans (Finset.single_le_sum (f := fun i ↦
    a * softplus (a * y i) + softplus (-(a * y i))) (fun i _ ↦ by
      have := softplus_pos (a * y i)
      have := softplus_pos (-(a * y i))
      have : 0 ≤ a := by linarith
      positivity) (Finset.mem_univ j))

private theorem shapeLik_top (hn : 0 < n) (y : Fin n → ℝ) :
    Tendsto (shapeLik y) atTop atBot := by
  obtain ⟨δ, hδ, h⟩ := shapeLik_upper_large hn y
  let B := 4 / δ
  have hB : 0 < B := by dsimp [B]; positivity
  have hrate : 2 / B = δ / 2 := by dsimp [B]; field_simp; norm_num
  have hlin : Tendsto (fun a : ℝ ↦ (2 * log B - 2) + (- (δ / 2)) * a) atTop atBot :=
    tendsto_atBot_add_const_left _ _ ((tendsto_const_mul_atBot_of_neg (by linarith)).mpr tendsto_id)
  apply tendsto_atBot_mono' _ ?_ hlin
  filter_upwards [eventually_ge_atTop (1 : ℝ)] with a ha
  have ha' : 0 < a := by linarith
  have hb := log_le_sub_one_of_pos (div_pos ha' hB)
  rw [log_div ha'.ne' hB.ne'] at hb
  calc
    shapeLik y a ≤ 2 * log a - δ * a := h a ha
    _ ≤ _ := by linarith [congrArg (fun t : ℝ ↦ t * a) hrate,
      show 2 * (a / B) = (2 / B) * a by ring]

private theorem shapeLik_zero (y : Fin n → ℝ) :
    Tendsto (shapeLik y) (𝓝[>] 0) atBot := by
  apply tendsto_atBot_mono' _ ?_ ((tendsto_const_mul_atBot_of_pos (by norm_num : (0 : ℝ) < 2)).mpr
    tendsto_log_nhdsGT_zero)
  filter_upwards [self_mem_nhdsWithin] with a ha
  exact shapeLik_upper y ha

private theorem shapeLik_escape (hn : 0 < n) (y : Fin n → ℝ) :
    Tendsto (fun a : Ioi (0 : ℝ) ↦ shapeLik y a) (cocompact _) atBot := by
  apply tendsto_atBot.2
  intro M
  obtain ⟨R, hR⟩ := eventually_atTop.1 (tendsto_atBot.1 (shapeLik_top hn y) M)
  obtain ⟨ε, hε, hsmall⟩ := (mem_nhdsGT_iff_exists_Ioo_subset.mp
    (tendsto_atBot.1 (shapeLik_zero y) M))
  have hsub : Icc ε R ⊆ Ioi (0 : ℝ) := fun _ ha ↦ hε.trans_le ha.1
  have hK : IsCompact ((Subtype.val : Ioi (0 : ℝ) → ℝ) ⁻¹' Icc ε R) :=
    IsEmbedding.subtypeVal.isInducing.isCompact_preimage' isCompact_Icc
      (by simpa only [Subtype.range_coe] using hsub)
  filter_upwards [hK.compl_mem_cocompact] with a ha
  by_cases h : (a : ℝ) < ε
  · exact hsmall ⟨a.property, h⟩
  · exact hR a (le_of_not_ge fun hle ↦ ha ⟨le_of_not_gt h, hle⟩)

def shapeScore (y : Fin n → ℝ) (a : ℝ) : ℝ :=
  2 / a + avg (fun i ↦ y i - softplus (a * y i) - (a + 1) * (y i * sigmoid (a * y i)))

private def shapeCurvature (y : Fin n → ℝ) (a : ℝ) : ℝ :=
  -2 / a ^ 2 + avg (fun i ↦ -2 * y i * sigmoid (a * y i) -
    (a + 1) * y i ^ 2 * (exp (a * y i) / (1 + exp (a * y i)) ^ 2))

private theorem shape_deriv (y : Fin n → ℝ) {a : ℝ} (ha : 0 < a) :
    HasDerivAt (shapeLik y) (shapeScore y a) a := by
  have h (i : Fin n) : HasDerivAt
      (fun t ↦ t * y i - (t + 1) * softplus (t * y i))
      (y i - softplus (a * y i) - (a + 1) * (y i * sigmoid (a * y i))) a := by
    have hy := (hasDerivAt_id a).mul_const (y i)
    convert hy.sub (((hasDerivAt_id a).add_const 1).mul
      ((softplus_deriv (a * y i)).comp a hy)) using 1
    · rfl
    · simp [id]
      ring
  have he : shapeLik y = fun t : ℝ ↦
      2 * log t + avg (fun i ↦ t * y i - (t + 1) * softplus (t * y i)) := by
    funext t
    simp only [shapeLik, avg, sub_eq_add_neg, ← neg_div, ← Finset.sum_neg_distrib]
    congr 2
    apply Finset.sum_congr rfl
    intro i _
    rw [softplus_shift (t * y i)]
    ring
  rw [he]
  simpa only [shapeScore, div_eq_mul_inv, Pi.add_def] using
    ((hasDerivAt_log ha.ne').const_mul 2).add (avg_deriv h)

private theorem shapeScore_deriv (y : Fin n → ℝ) {a : ℝ} (ha : 0 < a) :
    HasDerivAt (shapeScore y) (shapeCurvature y a) a := by
  have h (i : Fin n) : HasDerivAt
      (fun t ↦ y i - softplus (t * y i) - (t + 1) * (y i * sigmoid (t * y i)))
      (-2 * y i * sigmoid (a * y i) -
        (a + 1) * y i ^ 2 * (exp (a * y i) / (1 + exp (a * y i)) ^ 2)) a := by
    have hy := (hasDerivAt_id a).mul_const (y i)
    have hh := (softplus_deriv (a * y i)).comp a hy
    have hp := (sigmoid_deriv (a * y i)).comp a hy
    convert ((hasDerivAt_const a (y i)).sub hh).sub
      (((hasDerivAt_id a).add_const 1).mul (hp.const_mul (y i))) using 1
    · rfl
    · simp [id]
      ring
  have hinv : HasDerivAt (fun t : ℝ ↦ 2 / t) (-2 / a ^ 2) a := by
    convert (hasDerivAt_inv ha.ne').const_mul 2 using 1 <;> simp [div_eq_mul_inv]
  exact hinv.add (avg_deriv h)

private theorem sq_exp_bound {u : ℝ} (hu : 0 ≤ u) :
    u ^ 2 * exp (-u) ≤ 4 / exp 2 := by
  have h := add_one_le_exp (u / 2 - 1)
  have h' : u / 2 ≤ exp (u / 2 - 1) := by linarith
  have hs : (u / 2) ^ 2 ≤ exp (u / 2 - 1) ^ 2 :=
    pow_le_pow_left₀ (by positivity) h' 2
  rw [← exp_nat_mul, show (2 : ℕ) * (u / 2 - 1) = u - 2 by push_cast; ring,
    exp_sub] at hs
  rw [exp_neg]
  apply (le_div_iff₀ (exp_pos 2)).mpr
  rw [mul_comm (u ^ 2), ← div_eq_inv_mul, div_mul_eq_mul_div]
  apply (div_le_iff₀ (exp_pos u)).mpr
  have hh := (le_div_iff₀ (exp_pos 2)).mp hs
  nlinarith

/-- Average reciprocal magnitude of the negative log ratios. -/
def negativeSpread (y : Fin n → ℝ) : ℝ :=
  avg (fun i ↦ if y i < 0 then (-y i)⁻¹ else 0)

private theorem curvature_term_le {a : ℝ} (ha : 0 < a) (y : ℝ) :
    -2 * y * sigmoid (a * y) -
        (a + 1) * y ^ 2 * (exp (a * y) / (1 + exp (a * y)) ^ 2) ≤
      (8 / (a ^ 2 * exp 2)) * (if y < 0 then (-y)⁻¹ else 0) := by
  have hq : 0 ≤ (a + 1) * y ^ 2 * (exp (a * y) / (1 + exp (a * y)) ^ 2) := by positivity
  by_cases hy : y < 0
  · rw [ite_eq_left hy]
    have hd : 0 < -y := neg_pos.mpr hy
    have hp : sigmoid (a * y) ≤ exp (a * y) :=
      div_le_self (exp_pos _).le (by linarith [exp_pos (a * y)])
    have hb := sq_exp_bound (show 0 ≤ a * -y by positivity)
    rw [show -(a * -y) = a * y by ring] at hb
    have h₂ : -2 * y * exp (a * y) ≤ 8 / (a ^ 2 * exp 2) * (-y)⁻¹ := by
      rw [← div_eq_mul_inv, div_div]
      apply (le_div_iff₀ (by positivity)).mpr
      have hh := (le_div_iff₀ (exp_pos 2)).mp hb
      calc
        -2 * y * exp (a * y) * (a ^ 2 * exp 2 * -y) =
            2 * ((a * -y) ^ 2 * exp (a * y) * exp 2) := by ring
        _ ≤ 8 := by linarith
    exact (sub_le_self _ hq).trans ((mul_le_mul_of_nonneg_left hp (by linarith)).trans h₂)
  · rw [ite_eq_right hy, mul_zero]
    have hy' := le_of_not_gt hy
    have hp : 0 ≤ sigmoid (a * y) := by unfold sigmoid; positivity
    nlinarith [mul_nonneg hy' hp]

private theorem shapeCurvature_neg (y : Fin n → ℝ) (h : negativeSpread y < exp 2 / 4)
    {a : ℝ} (ha : 0 < a) : shapeCurvature y a < 0 := by
  have hp : 0 < a ^ 2 * exp 2 := by positivity
  have hb := avg_mono (fun i : Fin n ↦ curvature_term_le ha (y i))
  have he : avg (fun i ↦ (8 / (a ^ 2 * exp 2)) * (if y i < 0 then (-y i)⁻¹ else 0)) =
      (8 / (a ^ 2 * exp 2)) * negativeSpread y := by
    unfold negativeSpread avg
    rw [← Finset.mul_sum, mul_div_assoc]
  rw [he] at hb
  have hh : (8 / (a ^ 2 * exp 2)) * negativeSpread y < 2 / a ^ 2 := by
    calc
      _ < (8 / (a ^ 2 * exp 2)) * (exp 2 / 4) :=
        mul_lt_mul_of_pos_left h (by positivity)
      _ = _ := by field_simp [ha.ne', (exp_pos 2).ne']; ring
  rw [shapeCurvature, neg_div]
  linarith [hb.trans_lt hh]

private theorem shapeLik_strictConcaveOn {D : Set ℝ} (hD : Convex ℝ D)
    (hpos : D ⊆ Ioi 0) (y : Fin n → ℝ)
    (hneg : ∀ a ∈ D, shapeCurvature y a < 0) : StrictConcaveOn ℝ D (shapeLik y) := by
  apply strictConcaveOn_of_deriv2_neg' hD
  · exact fun a ha ↦ (shape_deriv y (hpos ha)).continuousAt.continuousWithinAt
  intro a ha
  have ha' : 0 < a := hpos ha
  have he : deriv (shapeLik y) =ᶠ[𝓝 a] shapeScore y := by
    filter_upwards [Ioi_mem_nhds ha'] with t ht
    exact (shape_deriv y ht).deriv
  change deriv (deriv (shapeLik y)) a < 0
  rw [he.deriv_eq, (shapeScore_deriv y ha').deriv]
  exact hneg a ha

/-- The report's strict reciprocal-log-ratio bound implies strict concavity. -/
theorem shapeLik_strictConcave (y : Fin n → ℝ) (h : negativeSpread y < exp 2 / 4) :
    StrictConcaveOn ℝ (Ioi 0) (shapeLik y) :=
  shapeLik_strictConcaveOn (convex_Ioi 0) Subset.rfl y
    (fun _ ha ↦ shapeCurvature_neg y h ha)

private theorem heldScale_map (hn : 0 < n) (x : Fin n → ℝ) (s : ℝ) :
    ∀ a ∈ Ioi 0, logLik x a s = shapeLik (logRatios x s) (id a) +
      -avg (fun i ↦ log (x i)) := by
  intro a _
  exact (logLik_heldScale hn x a s).trans (sub_eq_add_neg ..)

/-- Every nonempty positive sample attains its maximum when the scale is held. -/
theorem heldScale_exists (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {s : ℝ} (_hs : 0 < s) :
    ∃ a ∈ Ioi 0, IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a := by
  apply (Transfer.exists_max_iff id_bij (heldScale_map hn x s)).mpr
  exact Generic.exists_isMaxOn_of_tendsto_cocompact ⟨1, zero_lt_one⟩
    (fun a ha ↦ (shape_deriv _ ha).continuousAt.continuousWithinAt) (shapeLik_escape hn _)

/-- The sufficient spread condition implies a unique positive held-scale shape. -/
theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {s : ℝ} (_hs : 0 < s) (h : negativeSpread (logRatios x s) < exp 2 / 4) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a := by
  apply (Transfer.existsUnique_max_iff id_bij (heldScale_map hn x s)).mpr
  exact Generic.existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    ⟨1, zero_lt_one⟩ isOpen_Ioi (shapeLik_strictConcave _ h) (shapeLik_escape hn _)

/-- All observations at or above the held scale satisfy the spread condition. -/
theorem heldScale_above (hn : 0 < n) {x : Fin n → ℝ} {s : ℝ} (hs : 0 < s)
    (h : ∀ i, s ≤ x i) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a := by
  apply heldScale hn (fun i ↦ hs.trans_le (h i)) hs
  have hy (i : Fin n) : ¬ logRatios x s i < 0 :=
    not_lt.mpr (log_nonneg ((le_div_iff₀ hs).mpr (by simpa using h i)))
  simp only [negativeSpread, ite_eq_right (hy _), avg, Finset.sum_const_zero, zero_div]
  positivity

/-- The two log ratios from the report's held-scale construction. -/
def tieLogs (r : ℝ) : Fin 2 → ℝ := ![-1 / 1000, -r]

/-- Maximized criterion over a closed shape band for the two-point sample. -/
def bandHeight (K : Set ℝ) (r : ℝ) : ℝ := sSup (shapeLik (tieLogs r) '' K)

private theorem continuous_bandHeight {K : Set ℝ} (hK : IsCompact K)
    (hK' : K ⊆ Ici 1) : Continuous (bandHeight K) := by
  let f (r a : ℝ) := shapeLik (tieLogs r) (max a 1)
  have hf : Continuous (Function.uncurry f) := by
    unfold f Function.uncurry shapeLik avg tieLogs
    simp only [Fin.sum_univ_two, Matrix.cons_val_zero, Matrix.cons_val_one]
    have ha : Continuous (fun p : ℝ × ℝ ↦ max p.2 1) :=
      continuous_snd.max continuous_const
    have hlog := ha.log (fun p ↦ (zero_lt_one.trans_le (le_max_right p.2 1)).ne')
    have hs : Continuous softplus :=
      (continuous_const.add continuous_exp).log (fun t ↦ (show 0 < 1 + exp t by positivity).ne')
    have h₃ : Continuous (fun p : ℝ × ℝ ↦ max p.2 1 * -p.1) := ha.mul continuous_fst.neg
    have h₄ : Continuous (fun p : ℝ × ℝ ↦ max p.2 1 * (-1 / 1000)) :=
      ha.mul continuous_const
    apply (hlog.const_mul 2).sub
    exact ((((ha.mul (hs.comp h₄)).add (hs.comp h₄.neg)).add
        ((ha.mul (hs.comp h₃)).add (hs.comp h₃.neg))).div_const (2 : ℝ))
  have he : bandHeight K = fun r ↦ sSup (f r '' K) := by
    funext r
    unfold bandHeight
    congr 1
    exact image_congr (fun a ha ↦ (congrArg (shapeLik (tieLogs r))
      (max_eq_left (hK' ha))).symm)
  rw [he]
  exact hK.continuous_sSup hf

private theorem tieLik_eq (r a : ℝ) :
    shapeLik (tieLogs r) a = 2 * log a - a * (1 / 1000 + r) / 2 -
      (a + 1) * (softplus (-(a / 1000)) + softplus (-(a * r))) / 2 := by
  simp only [shapeLik, avg, tieLogs, Fin.sum_univ_two, Matrix.cons_val_zero,
    Matrix.cons_val_one, Nat.cast_ofNat]
  have h₁ : a * (-1 / 1000) = -(a / 1000) := by ring
  rw [h₁, mul_neg, neg_neg, neg_neg, softplus_shift (a / 1000), softplus_shift (a * r)]
  ring

private theorem softplus_tangent (z : ℝ) : log 2 + z / 2 ≤ softplus z := by
  have h : 2 * exp (z / 2) ≤ 1 + exp z := by
    have he : exp z = exp (z / 2) * exp (z / 2) := by
      rw [← exp_add, show z / 2 + z / 2 = z by ring]
    rw [he]
    nlinarith [sq_nonneg (exp (z / 2) - 1)]
  rw [← log_exp (z / 2), ← log_mul (by norm_num : (2 : ℝ) ≠ 0) (exp_pos _).ne']
  exact log_le_log (by positivity) h

private theorem softplus_le_exp (z : ℝ) : softplus z ≤ exp z := by
  unfold softplus
  linarith [log_le_sub_one_of_pos (show 0 < 1 + exp z by positivity)]

private theorem tie_low_left_nonpos {a : ℝ} (ha : a ∈ Icc (2 : ℝ) 4) :
    shapeLik (tieLogs (3 / 1000)) a ≤ 0 := by
  have ha' : 0 < a := (by norm_num : (0 : ℝ) < 2).trans_le ha.1
  have hl := log_le_sub_one_of_pos (show 0 < a / 3 by positivity)
  rw [log_div ha'.ne' (by norm_num : (3 : ℝ) ≠ 0)] at hl
  have hsoft : 34 / 25 ≤ softplus (-(a / 1000)) + softplus (-(a * (3 / 1000))) := by
    linarith [softplus_tangent (-(a / 1000)), softplus_tangent (-(a * (3 / 1000))),
      log_two_gt_d9, ha.2]
  rw [tieLik_eq]
  have hmul := mul_le_mul_of_nonneg_left hsoft (show 0 ≤ a + 1 by positivity)
  linarith [log_three_lt_d9]

private theorem tie_high_left_pos :
    0 < shapeLik (tieLogs (3 / 1000)) 7500 := by
  have hlog := log_le_log (show (0 : ℝ) < 2 ^ 11 * 3 by positivity)
    (by norm_num : (2 : ℝ) ^ 11 * 3 ≤ 7500)
  rw [log_mul (by positivity : (2 : ℝ) ^ 11 ≠ 0) (by norm_num), log_pow] at hlog
  have h₇ : (2.718 : ℝ) ^ 7 ≤ exp 7 := by
    calc
      _ ≤ exp 1 ^ 7 := pow_le_pow_left₀ (by norm_num) (by linarith [exp_one_gt_d9]) 7
      _ = _ := by simpa only [Nat.cast_ofNat] using exp_one_pow 7
  have hhalf := quadratic_le_exp_of_nonneg (by norm_num : (0 : ℝ) ≤ 1 / 2)
  have hbig : (1700 : ℝ) ≤ exp (15 / 2) := by
    rw [show (15 / 2 : ℝ) = 7 + 1 / 2 by norm_num, exp_add]
    refine (show (1700 : ℝ) ≤ 2.718 ^ 7 * (1 + 1 / 2 + (1 / 2) ^ 2 / 2) by norm_num).trans ?_
    exact mul_le_mul h₇ hhalf (by norm_num) (exp_pos _).le
  have h₂₂ : (100000 : ℝ) ≤ exp (45 / 2) := by
    have hh : (2 : ℝ) ^ 22 ≤ exp 22 := by
      calc
        _ ≤ exp 1 ^ 22 := pow_le_pow_left₀ (by norm_num) exp_one_gt_two.le 22
        _ = _ := by simpa only [Nat.cast_ofNat] using exp_one_pow 22
    exact (by norm_num : (100000 : ℝ) ≤ 2 ^ 22).trans
      (hh.trans (exp_le_exp.mpr (by norm_num)))
  have he₁ : exp (-(7500 / 1000 : ℝ)) ≤ (1700 : ℝ)⁻¹ := by
    rw [show (7500 / 1000 : ℝ) = 15 / 2 by norm_num, exp_neg]
    exact (inv_le_inv₀ (exp_pos _) (by norm_num)).mpr hbig
  have he₂ : exp (-(7500 * (3 / 1000) : ℝ)) ≤ (100000 : ℝ)⁻¹ := by
    rw [show (7500 * (3 / 1000) : ℝ) = 45 / 2 by norm_num, exp_neg]
    exact (inv_le_inv₀ (exp_pos _) (by norm_num)).mpr h₂₂
  have hs₁ := (softplus_le_exp _).trans he₁
  have hs₂ := (softplus_le_exp _).trans he₂
  rw [tieLik_eq]
  norm_num1 at hlog hs₁ hs₂ ⊢
  linarith only [hlog, log_two_gt_d9, log_three_gt_d9, hs₁, hs₂]

private theorem tie_high_right_lt {a : ℝ} (ha : a ∈ Icc (6500 : ℝ) 10000) :
    shapeLik (tieLogs (5 / 1000)) a < -7 / 10 := by
  have hl := log_le_log (show 0 < a from (by norm_num : (0 : ℝ) < 6500).trans_le ha.1)
    (ha.2.trans (by norm_num : (10000 : ℝ) ≤ 2 ^ 4 * 3 ^ 6))
  rw [log_mul (by positivity : (2 : ℝ) ^ 4 ≠ 0) (by positivity), log_pow, log_pow] at hl
  rw [tieLik_eq]
  have hp : 0 ≤ (a + 1) *
      (softplus (-(a / 1000)) + softplus (-(a * (5 / 1000)))) := by
    have ha' : 0 ≤ a := (by norm_num : (0 : ℝ) ≤ 6500).trans ha.1
    have := softplus_pos (-(a / 1000))
    have := softplus_pos (-(a * (5 / 1000)))
    positivity
  norm_num1 at hl ⊢
  linarith only [hl, hp, log_two_lt_d9, log_three_lt_d9, ha.1]

private theorem tie_witness_gt {r : ℝ} (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000)) :
    -3 / 5 < shapeLik (tieLogs r) 3 := by
  have h₁ : softplus (-(3 / 1000 : ℝ)) ≤ log 2 := by
    simpa [softplus, show (1 + 1 : ℝ) = 2 by norm_num] using
      (softplus_mono (show -(3 / 1000 : ℝ) ≤ 0 by norm_num))
  have h₂ : softplus (-(3 * r)) ≤ log 2 := by
    simpa [softplus, show (1 + 1 : ℝ) = 2 by norm_num] using
      (softplus_mono (show -(3 * r) ≤ 0 by linarith [hr.1]))
  rw [tieLik_eq]
  linarith [log_two_lt_d9, log_three_gt_d9, hr.2]

private theorem bandHeight_spec {l u : ℝ} (hl : 0 < l) (hlu : l ≤ u) (r : ℝ) :
    ∃ a ∈ Icc l u, bandHeight (Icc l u) r = shapeLik (tieLogs r) a ∧
      ∀ b ∈ Icc l u, shapeLik (tieLogs r) b ≤ shapeLik (tieLogs r) a :=
  isCompact_Icc.exists_sSup_image_eq_and_ge (nonempty_Icc.mpr hlu)
    (fun _ ha ↦ (shape_deriv _ (hl.trans_le ha.1)).continuousAt.continuousWithinAt)

private theorem bandHeight_left :
    bandHeight (Icc 2 4) (3 / 1000) < bandHeight (Icc 6500 10000) (3 / 1000) := by
  obtain ⟨a, ha, hea, _⟩ := bandHeight_spec
    (by norm_num : (0 : ℝ) < 2) (by norm_num : (2 : ℝ) ≤ 4) _
  obtain ⟨b, _, heb, hmb⟩ := bandHeight_spec (by norm_num : (0 : ℝ) < 6500)
    (by norm_num : (6500 : ℝ) ≤ 10000) _
  rw [hea, heb]
  exact (tie_low_left_nonpos ha).trans_lt (tie_high_left_pos.trans_le (hmb 7500 (by norm_num)))

private theorem bandHeight_right :
    bandHeight (Icc 6500 10000) (5 / 1000) < bandHeight (Icc 2 4) (5 / 1000) := by
  obtain ⟨a, _, hea, hma⟩ := bandHeight_spec (by norm_num : (0 : ℝ) < 2)
    (by norm_num : (2 : ℝ) ≤ 4) _
  obtain ⟨b, hb, heb, _⟩ := bandHeight_spec (by norm_num : (0 : ℝ) < 6500)
    (by norm_num : (6500 : ℝ) ≤ 10000) _
  rw [hea, heb]
  exact (tie_high_right_lt hb).trans ((by norm_num : (-7 / 10 : ℝ) < -3 / 5).trans
    ((tie_witness_gt (r := 5 / 1000) (by norm_num)).trans_le (hma 3 (by norm_num))))

private theorem sigmoid_neg (t : ℝ) : sigmoid (-t) = 1 / (1 + exp t) := by
  unfold sigmoid
  rw [exp_neg]
  field_simp [ne_of_gt (exp_pos t)]
  ring

private theorem tie_ratio_deriv (a r : ℝ) :
    HasDerivAt (fun v ↦ shapeLik (tieLogs v) a)
      (a * ((a + 1) * sigmoid (-(a * r)) - 1) / 2) r := by
  have ha := (hasDerivAt_id r).const_mul (-a)
  have h := ((softplus_deriv (-(a * r))).comp r
    (show HasDerivAt (fun v ↦ -(a * v)) (-a) r by
      convert ha using 1
      · funext v
        simp only [id, neg_mul]
      · ring))
  have hd := (((hasDerivAt_id r).const_add (1 / 1000)).const_mul a).div_const 2
  have hp := (((h.const_add (softplus (-(a / 1000)))).const_mul (a + 1)).div_const 2)
  convert ((hd.const_sub (2 * log a)).sub hp) using 1
  · funext v
    exact tieLik_eq v a
  · ring

private theorem tie_value_strictMono {a : ℝ} (ha : a ∈ Icc (2 : ℝ) 4) :
    StrictMonoOn (fun r ↦ shapeLik (tieLogs r) a) (Icc (3 / 1000) (5 / 1000)) := by
  apply strictMonoOn_of_deriv_pos (convex_Icc _ _)
  · exact fun r _ ↦ (tie_ratio_deriv a r).continuousAt.continuousWithinAt
  intro r hr
  have hr' := interior_subset hr
  have ha₀ : 0 < a := (by norm_num : (0 : ℝ) < 2).trans_le ha.1
  have har : a * r ≤ 1 / 50 := calc
    a * r ≤ a * (5 / 1000) := mul_le_mul_of_nonneg_left hr'.2 ha₀.le
    _ ≤ _ := by linarith [ha.2]
  have hp : exp (a * r) < 2 := by
    rw [← exp_log (by norm_num : (0 : ℝ) < 2), exp_lt_exp]
    linarith [log_two_gt_d9]
  rw [(tie_ratio_deriv a r).deriv]
  apply div_pos (mul_pos ha₀ (sub_pos.mpr ?_)) (by norm_num)
  rw [sigmoid_neg, ← mul_div_assoc, mul_one, lt_div_iff₀ (by positivity)]
  linarith [ha.1]

private theorem tie_value_strictAnti {a : ℝ} (ha : a ∈ Icc (6500 : ℝ) 10000) :
    StrictAntiOn (fun r ↦ shapeLik (tieLogs r) a) (Icc (3 / 1000) (5 / 1000)) := by
  apply strictAntiOn_of_deriv_neg (convex_Icc _ _)
  · exact fun r _ ↦ (tie_ratio_deriv a r).continuousAt.continuousWithinAt
  intro r hr
  have hr' := interior_subset hr
  have ha₀ : 0 < a := (by norm_num : (0 : ℝ) < 6500).trans_le ha.1
  have har : 19 ≤ a * r := calc
    (19 : ℝ) ≤ a * (3 / 1000) := by linarith [ha.1]
    _ ≤ _ := mul_le_mul_of_nonneg_left hr'.1 ha₀.le
  have hpow : (2 : ℝ) ^ 19 ≤ exp 19 := calc
    _ ≤ exp 1 ^ 19 := pow_le_pow_left₀ (by norm_num) exp_one_gt_two.le 19
    _ = _ := by simpa only [Nat.cast_ofNat] using exp_one_pow 19
  have hp : a < exp (a * r) := ha.2.trans_lt
    ((by norm_num : (10000 : ℝ) < 2 ^ 19).trans_le (hpow.trans (exp_le_exp.mpr har)))
  rw [(tie_ratio_deriv a r).deriv]
  apply div_neg_of_neg_of_pos (mul_neg_of_pos_of_neg ha₀ (sub_neg.mpr ?_)) (by norm_num)
  rw [sigmoid_neg, ← mul_div_assoc, mul_one, div_lt_iff₀ (by positivity)]
  linarith

private theorem bandHeight_strictMono :
    StrictMonoOn (bandHeight (Icc 2 4)) (Icc (3 / 1000) (5 / 1000)) := by
  intro r hr t ht hrt
  obtain ⟨a, ha, hae, _⟩ := bandHeight_spec
    (by norm_num : (0 : ℝ) < 2) (by norm_num : (2 : ℝ) ≤ 4) r
  obtain ⟨b, _, hbe, hbm⟩ := bandHeight_spec
    (by norm_num : (0 : ℝ) < 2) (by norm_num : (2 : ℝ) ≤ 4) t
  rw [hae, hbe]
  exact (tie_value_strictMono ha hr ht hrt).trans_le (hbm a ha)

private theorem bandHeight_strictAnti :
    StrictAntiOn (bandHeight (Icc 6500 10000)) (Icc (3 / 1000) (5 / 1000)) := by
  intro r hr t ht hrt
  obtain ⟨a, _, hae, ham⟩ := bandHeight_spec (by norm_num : (0 : ℝ) < 6500)
    (by norm_num : (6500 : ℝ) ≤ 10000) r
  obtain ⟨b, hb, hbe, _⟩ := bandHeight_spec (by norm_num : (0 : ℝ) < 6500)
    (by norm_num : (6500 : ℝ) ≤ 10000) t
  rw [hae, hbe]
  exact (tie_value_strictAnti hb hr ht hrt).trans_le (ham b hb)

/-- The two band heights agree at a unique sample parameter strictly between
the source report's endpoints. -/
theorem heldScale_band_crossing :
    ∃! r, r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) ∧
      bandHeight (Icc 2 4) r = bandHeight (Icc 6500 10000) r := by
  have hleft := bandHeight_left
  have hright := bandHeight_right
  have hlow : Icc (2 : ℝ) 4 ⊆ Ici 1 := fun _ h ↦ (by norm_num : (1 : ℝ) ≤ 2).trans h.1
  have hhigh : Icc (6500 : ℝ) 10000 ⊆ Ici 1 :=
    fun _ h ↦ (by norm_num : (1 : ℝ) ≤ 6500).trans h.1
  have hc := (continuous_bandHeight isCompact_Icc hlow).sub
    (continuous_bandHeight isCompact_Icc hhigh)
  obtain ⟨r, hr, hz⟩ := intermediate_value_Icc
    (by norm_num : (3 / 1000 : ℝ) ≤ 5 / 1000) hc.continuousOn
    (show 0 ∈ Icc _ _ from ⟨(sub_neg.mpr hleft).le, (sub_pos.mpr hright).le⟩)
  have he := sub_eq_zero.mp hz
  have hr' : r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) :=
    ⟨hr.1.lt_of_ne (fun h ↦ hleft.ne (h.symm ▸ he)),
      hr.2.lt_of_ne (fun h ↦ hright.ne (h ▸ he.symm))⟩
  refine ⟨r, ⟨hr', he⟩, fun t ht ↦ ?_⟩
  have hmono : StrictMonoOn (fun v ↦ bandHeight (Icc 2 4) v - bandHeight (Icc 6500 10000) v)
      (Icc (3 / 1000) (5 / 1000)) := fun _ hs _ ht hst ↦
    sub_lt_sub (bandHeight_strictMono hs ht hst) (bandHeight_strictAnti hs ht hst)
  apply hmono.injOn (Ioo_subset_Icc_self ht.1) hr
  change bandHeight (Icc 2 4) t - bandHeight (Icc 6500 10000) t =
    bandHeight (Icc 2 4) r - bandHeight (Icc 6500 10000) r
  simp only [ht.2, he, sub_self]

private theorem tie_small_loses {r a : ℝ} (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000))
    (ha : 0 < a) (ha₁ : a ≤ 1) :
    shapeLik (tieLogs r) a < shapeLik (tieLogs r) 3 := by
  have hr₀ : 0 ≤ r := by linarith [hr.1]
  have hmul : a * r ≤ 5 / 1000 := (mul_le_of_le_one_left hr₀ ha₁).trans hr.2
  have hsoft : 13 / 10 ≤ softplus (-(a / 1000)) + softplus (-(a * r)) := by
    linarith [softplus_tangent (-(a / 1000)), softplus_tangent (-(a * r)), log_two_gt_d9]
  have hpen := mul_le_mul_of_nonneg_left hsoft (show 0 ≤ a + 1 by positivity)
  have hdata : 0 ≤ a * (1 / 1000 + r) / 2 := by positivity
  have hlog := log_nonpos ha.le ha₁
  have hupper : shapeLik (tieLogs r) a ≤ -13 / 20 := by
    rw [tieLik_eq]
    nlinarith
  exact hupper.trans_lt ((by norm_num : (-13 / 20 : ℝ) < -3 / 5).trans (tie_witness_gt hr))

private theorem tie_large_loses {r a : ℝ} (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000))
    (ha : 20000 ≤ a) :
    shapeLik (tieLogs r) a < shapeLik (tieLogs r) 3 := by
  have ha' : 0 < a := (by norm_num : (0 : ℝ) < 20000).trans_le ha
  have hl := log_le_sub_one_of_pos (show 0 < a / 20000 by positivity)
  rw [log_div ha'.ne' (by norm_num : (20000 : ℝ) ≠ 0)] at hl
  have hB := log_le_log (by norm_num : (0 : ℝ) < 20000)
    (by norm_num : (20000 : ℝ) ≤ 2 ^ 5 * 3 ^ 6)
  rw [log_mul (by positivity : (2 : ℝ) ^ 5 ≠ 0) (by positivity), log_pow, log_pow] at hB
  have hdata : a * (4 / 1000) ≤ a * (1 / 1000 + r) :=
    mul_le_mul_of_nonneg_left (by linarith [hr.1]) ha'.le
  have hpen : 0 ≤ (a + 1) * (softplus (-(a / 1000)) + softplus (-(a * r))) := by
    have := softplus_pos (-(a / 1000))
    have := softplus_pos (-(a * r))
    positivity
  have hupper : shapeLik (tieLogs r) a ≤ -1 := by
    rw [tieLik_eq]
    norm_num1 at hB
    linarith only [hl, hB, hdata, hpen, ha, log_two_lt_d9, log_three_lt_d9]
  exact hupper.trans_lt ((by norm_num : (-1 : ℝ) < -3 / 5).trans (tie_witness_gt hr))

private theorem tie_log_range {r : ℝ} (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000)) (i : Fin 2) :
    1 / 1000 ≤ -tieLogs r i ∧ -tieLogs r i ≤ 5 / 1000 := by
  fin_cases i
  · norm_num [tieLogs]
  · change 1 / 1000 ≤ -(-r) ∧ -(-r) ≤ 5 / 1000
    rw [neg_neg]
    exact ⟨(by norm_num : (1 / 1000 : ℝ) ≤ 3 / 1000).trans hr.1, hr.2⟩

private theorem curvatureTerm_eq (a d : ℝ) :
    -2 * (-d) * sigmoid (a * -d) -
        (a + 1) * (-d) ^ 2 * (exp (a * -d) / (1 + exp (a * -d)) ^ 2) =
      d * sigmoid (-(a * d)) * (2 - (a + 1) * d * (1 - sigmoid (-(a * d)))) := by
  simp only [mul_neg]
  unfold sigmoid
  have hp : 0 < 1 + exp (-(a * d)) := by positivity
  field_simp [hp.ne']
  ring

private theorem tie_curvature_term_le {a d : ℝ} (ha : 0 < a) (hd : 0 ≤ d) :
    d * sigmoid (-(a * d)) * (2 - (a + 1) * d * (1 - sigmoid (-(a * d)))) ≤ 2 * d := by
  have hp : 0 ≤ sigmoid (-(a * d)) := by unfold sigmoid; positivity
  have hp' : sigmoid (-(a * d)) ≤ 1 := by
    rw [sigmoid_neg]
    exact div_le_self (by norm_num) (by linarith [exp_pos (a * d)])
  have hc : 2 - (a + 1) * d * (1 - sigmoid (-(a * d))) ≤ 2 :=
    sub_le_self _ (mul_nonneg (mul_nonneg (by positivity) hd) (sub_nonneg.mpr hp'))
  calc
    _ ≤ d * sigmoid (-(a * d)) * 2 := mul_le_mul_of_nonneg_left hc (mul_nonneg hd hp)
    _ ≤ _ := by nlinarith [mul_le_mul_of_nonneg_left hp' hd]

private theorem tie_low_concave {r : ℝ} (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000)) :
    StrictConcaveOn ℝ (Icc 2 4) (shapeLik (tieLogs r)) := by
  apply shapeLik_strictConcaveOn (convex_Icc 2 4)
    (fun _ h ↦ (by norm_num : (0 : ℝ) < 2).trans_le h.1)
  intro a ha
  have ha' : 0 < a := (by norm_num : (0 : ℝ) < 2).trans_le ha.1
  have hb := avg_mono (f := fun i : Fin 2 ↦ -2 * tieLogs r i * sigmoid (a * tieLogs r i) -
      (a + 1) * tieLogs r i ^ 2 * (exp (a * tieLogs r i) / (1 + exp (a * tieLogs r i)) ^ 2))
    (g := fun _ ↦ 1 / 100) (fun i ↦ by
      let d := -tieLogs r i
      have hd := tie_log_range hr i
      change 1 / 1000 ≤ d ∧ d ≤ 5 / 1000 at hd
      rw [show tieLogs r i = -d by simp [d], curvatureTerm_eq]
      exact (tie_curvature_term_le ha' (by linarith [hd.1])).trans (by linarith [hd.2]))
  rw [avg_const (n := 2) (by decide)] at hb
  have hs : a ^ 2 ≤ 16 := by
    have h := pow_le_pow_left₀ ha'.le ha.2 2
    norm_num at h
    exact h
  have hc : (1 / 100 : ℝ) < 2 / a ^ 2 :=
    (lt_div_iff₀ (by positivity)).mpr (by nlinarith)
  rw [shapeCurvature, neg_div]
  linarith [hb.trans_lt hc]

private theorem tie_high_concave {r : ℝ} (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000)) :
    StrictConcaveOn ℝ (Icc 6500 10000) (shapeLik (tieLogs r)) := by
  apply shapeLik_strictConcaveOn (convex_Icc 6500 10000)
    (fun _ h ↦ (by norm_num : (0 : ℝ) < 6500).trans_le h.1)
  intro a ha
  have ha' : 0 < a := (by norm_num : (0 : ℝ) < 6500).trans_le ha.1
  have hb := avg_mono (f := fun i : Fin 2 ↦ -2 * tieLogs r i * sigmoid (a * tieLogs r i) -
      (a + 1) * tieLogs r i ^ 2 * (exp (a * tieLogs r i) / (1 + exp (a * tieLogs r i)) ^ 2))
    (g := fun _ ↦ 0) (fun i ↦ by
      let d := -tieLogs r i
      have hd := tie_log_range hr i
      change 1 / 1000 ≤ d ∧ d ≤ 5 / 1000 at hd
      have hd₀ : 0 < d := by linarith [hd.1]
      rw [show tieLogs r i = -d by simp [d], curvatureTerm_eq]
      have hp : 0 ≤ sigmoid (-(a * d)) := by unfold sigmoid; positivity
      have hp' : sigmoid (-(a * d)) ≤ 1 / 2 := by
        rw [sigmoid_neg, one_div_le_one_div (by positivity) (by norm_num : (0 : ℝ) < 2)]
        linarith [one_le_exp (mul_nonneg ha'.le hd₀.le)]
      have h₁ : (6501 : ℝ) * (1 / 1000) ≤ (a + 1) * d :=
        mul_le_mul (by linarith [ha.1]) hd.1 (by norm_num) (by positivity)
      have h₂ := mul_le_mul h₁ (show (1 / 2 : ℝ) ≤ 1 - sigmoid (-(a * d)) by linarith)
        (by norm_num) (show 0 ≤ (a + 1) * d by positivity)
      exact mul_nonpos_of_nonneg_of_nonpos (mul_nonneg hd₀.le hp) (by linarith))
  rw [avg_const (n := 2) (by decide)] at hb
  rw [shapeCurvature, neg_div]
  exact add_neg_of_neg_of_nonpos (neg_neg_of_pos (by positivity)) hb

private theorem tie_max_mem_bands
    {r m : ℝ} (hr : r ∈ Icc (3 / 1000 : ℝ) (5 / 1000))
    (hcover : ∀ a ∈ Icc (1 : ℝ) 20000, a ∉ Icc (2 : ℝ) 4 → a ∉ Icc (6500 : ℝ) 10000 →
      shapeScore (tieLogs r) a = 0 → shapeLik (tieLogs r) a < shapeLik (tieLogs r) 3)
    (hm : 0 < m) (hmm : IsMaxOn (shapeLik (tieLogs r)) (Ioi 0) m) :
    m ∈ Icc (2 : ℝ) 4 ∨ m ∈ Icc (6500 : ℝ) 10000 := by
  have hw := hmm (show (3 : ℝ) ∈ Ioi 0 by norm_num)
  have hscore : shapeScore (tieLogs r) m = 0 :=
    (hmm.isLocalMax (Ioi_mem_nhds hm)).hasDerivAt_eq_zero (shape_deriv _ hm)
  have hml : 1 ≤ m := le_of_not_gt fun h ↦ (tie_small_loses hr hm h.le).not_ge hw
  have hmu : m ≤ 20000 := le_of_not_gt fun h ↦ (tie_large_loses hr h.le).not_ge hw
  by_cases hl : m ∈ Icc (2 : ℝ) 4
  · exact Or.inl hl
  by_cases hh : m ∈ Icc (6500 : ℝ) 10000
  · exact Or.inr hh
  exact ((hcover m ⟨hml, hmu⟩ hl hh hscore).not_ge hw).elim

/-- If the compact cover excludes all stationary competitors outside the
two bands, the report's sample has exactly two global maximizers. -/
theorem heldScale_tie_of_band_bounds
    (hcover : ∀ r ∈ Icc (3 / 1000 : ℝ) (5 / 1000), ∀ a ∈ Icc (1 : ℝ) 20000,
      a ∉ Icc (2 : ℝ) 4 → a ∉ Icc (6500 : ℝ) 10000 →
      shapeScore (tieLogs r) a = 0 → shapeLik (tieLogs r) a < shapeLik (tieLogs r) 3) :
    ∃ r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000),
      ∃ a ∈ Icc (2 : ℝ) 4, ∃ b ∈ Icc (6500 : ℝ) 10000,
        {k ∈ Ioi 0 | IsMaxOn (fun t ↦ logLik (fun i ↦ exp (tieLogs r i)) t 1) (Ioi 0) k} =
          {a, b} := by
  obtain ⟨r, ⟨hr', he⟩, _⟩ := heldScale_band_crossing
  have hr := Ioo_subset_Icc_self hr'
  obtain ⟨a, ha, haeq, ham⟩ := bandHeight_spec
    (by norm_num : (0 : ℝ) < 2) (by norm_num : (2 : ℝ) ≤ 4) r
  obtain ⟨b, hb, hbeq, hbm⟩ := bandHeight_spec
    (by norm_num : (0 : ℝ) < 6500) (by norm_num : (6500 : ℝ) ≤ 10000) r
  have hab : shapeLik (tieLogs r) a = shapeLik (tieLogs r) b :=
    haeq.symm.trans (he.trans hbeq)
  obtain ⟨m, hm, hmm⟩ := Generic.exists_isMaxOn_of_tendsto_cocompact (D := Ioi 0)
    ⟨1, zero_lt_one⟩ (fun _ h ↦ (shape_deriv _ h).continuousAt.continuousWithinAt)
    (shapeLik_escape (n := 2) (by decide) (tieLogs r))
  have hma : IsMaxOn (shapeLik (tieLogs r)) (Ioi 0) a := by
    intro k hk
    apply (hmm hk).trans
    rcases tie_max_mem_bands hr (hcover r hr) hm hmm with hl | hh
    · exact ham m hl
    · exact (hbm m hh).trans_eq hab.symm
  have hrat : logRatios (fun i ↦ exp (tieLogs r i)) 1 = tieLogs r := by
    funext i
    simp [logRatios]
  have hmap := heldScale_map (n := 2) (by decide) (fun i ↦ exp (tieLogs r i)) 1
  rw [hrat] at hmap
  have ha' : 0 < a := (by norm_num : (0 : ℝ) < 2).trans_le ha.1
  have hb' : 0 < b := (by norm_num : (0 : ℝ) < 6500).trans_le hb.1
  refine ⟨r, hr', a, ha, b, hb, ?_⟩
  ext k
  simp only [Set.mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
  constructor
  · rintro ⟨hk, hmk⟩
    have h := (Transfer.isMaxOn_iff id_bij hmap hk).mp hmk
    rcases tie_max_mem_bands hr (hcover r hr) hk h with hl | hh
    · exact Or.inl ((tie_low_concave hr).eq_of_isMaxOn
        (fun t ht ↦ h ((by norm_num : (0 : ℝ) < 2).trans_le ht.1))
        (fun t ht ↦ ham t ht) hl ha)
    · exact Or.inr ((tie_high_concave hr).eq_of_isMaxOn
        (fun t ht ↦ h ((by norm_num : (0 : ℝ) < 6500).trans_le ht.1))
        (fun t ht ↦ hbm t ht) hh hb)
  · rintro (rfl | rfl)
    · exact ⟨ha', (Transfer.isMaxOn_iff id_bij hmap ha').mpr hma⟩
    · exact ⟨hb', (Transfer.isMaxOn_iff id_bij hmap hb').mpr
        (fun _ hk ↦ (hma hk).trans_eq hab)⟩

end

end UnivariateML.Paralogistic

namespace UnivariateML.InvParalogistic

noncomputable section

variable {n : ℕ}

open BurrHeld (avg softplus logRatios)
open Transfer (reciprocal)

def logLik (x : Fin n → ℝ) (a s : ℝ) : ℝ :=
  avg (fun i ↦ log a + log a - log (x i) + a * log (s / x i) -
    (a + 1) * softplus (a * log (s / x i)))

/-- The inverse likelihood equals the parent likelihood at reciprocal data
and scale plus the mean log Jacobian. -/
theorem logLik_reciprocal {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    (a s : ℝ) :
    logLik x a s = Paralogistic.logLik (reciprocal x) a s⁻¹ +
      avg (fun i ↦ -2 * log (x i)) := by
  have hr (i : Fin n) : (x i)⁻¹ / s⁻¹ = s / x i := by
    rw [div_eq_mul_inv, inv_inv, mul_comm, ← div_eq_mul_inv]
  simp only [logLik, Paralogistic.logLik, BurrHeld.logLik, BurrHeld.logDensity,
    reciprocal, hr, log_inv, avg, Finset.sum_add_distrib, Finset.sum_sub_distrib,
    Finset.sum_neg_distrib, ← Finset.mul_sum, add_div, sub_div]
  ring

private theorem inv_bij : BijOn (fun s : ℝ ↦ s⁻¹) (Ioi 0) (Ioi 0) :=
  ⟨fun _ hs ↦ inv_pos.mpr hs, fun _ _ _ _ h ↦ inv_injective h,
    fun s hs ↦ ⟨s⁻¹, inv_pos.mpr hs, inv_inv s⟩⟩

/-- Holding shape gives a unique positive inverse paralogistic scale. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃! s, s ∈ Ioi 0 ∧ IsMaxOn (logLik x a) (Ioi 0) s := by
  apply (Transfer.existsUnique_max_iff inv_bij (fun s _ ↦ logLik_reciprocal hx a s)).mpr
  exact Paralogistic.heldShape hn (fun i ↦ inv_pos.mpr (hx i)) ha

/-- The inverse scale on constant data is the observation times
`exp (-(log a / a))`. -/
theorem heldShape_constant (hn : 0 < n) {c a : ℝ} (hc : 0 < c) (ha : 0 < a) :
    IsMaxOn (logLik (fun _ : Fin n ↦ c) a) (Ioi 0) (c * exp (-(log a / a))) := by
  have hs : 0 < c * exp (-(log a / a)) := by positivity
  apply (Transfer.isMaxOn_iff inv_bij (fun s _ ↦ logLik_reciprocal (fun _ ↦ hc) a s) hs).mpr
  change IsMaxOn (Paralogistic.logLik (fun _ : Fin n ↦ c⁻¹) a) (Ioi 0)
    ((c * exp (-(log a / a)))⁻¹)
  simpa only [mul_inv, exp_neg, inv_inv] using
    Paralogistic.heldShape_constant (n := n) hn (inv_pos.mpr hc) ha

/-- At any fixed positive scale the inverse fit attains its maximum. -/
theorem heldScale_exists (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) :
    ∃ a ∈ Ioi 0, IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a := by
  apply (Transfer.exists_max_iff id_bij
    (g := fun a ↦ Paralogistic.logLik (reciprocal x) a s⁻¹)
    (fun a _ ↦ logLik_reciprocal hx a s)).mpr
  exact Paralogistic.heldScale_exists hn (fun i ↦ inv_pos.mpr (hx i)) (inv_pos.mpr hs)

/-- In the inverse orientation the sufficient condition uses `log (s / x i)`. -/
theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s)
    (h : Paralogistic.negativeSpread (fun i ↦ log (s / x i)) < exp 2 / 4) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a := by
  apply (Transfer.existsUnique_max_iff id_bij
    (g := fun a ↦ Paralogistic.logLik (reciprocal x) a s⁻¹)
    (fun a _ ↦ logLik_reciprocal hx a s)).mpr
  apply Paralogistic.heldScale hn (fun i ↦ inv_pos.mpr (hx i)) (inv_pos.mpr hs)
  convert h using 2
  funext i
  simp [logRatios, div_eq_mul_inv, mul_comm]

/-- All inverse observations at or below the held scale imply uniqueness. -/
theorem heldScale_below (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (h : ∀ i, x i ≤ s) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a := by
  apply (Transfer.existsUnique_max_iff id_bij
    (g := fun a ↦ Paralogistic.logLik (reciprocal x) a s⁻¹)
    (fun a _ ↦ logLik_reciprocal hx a s)).mpr
  exact Paralogistic.heldScale_above hn (inv_pos.mpr hs)
    (fun i ↦ (inv_le_inv₀ hs (hx i)).mpr (h i))

end

end UnivariateML.InvParalogistic
