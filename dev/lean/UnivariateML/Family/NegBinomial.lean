module

public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.SpecialFunctions.Log.NegMulLog
public import Mathlib.Topology.Order.IntermediateValue

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Negative binomial maximum likelihood

Counts are natural numbers. The real log-likelihood is defined for positive
size and interior probabilities. The degenerate distributions have separate
likelihood statements.

## Deviations

The positive-mean statements use the interior parameter space. The all-zero
likelihood includes both degenerate faces. For the free fit we use the
dossier's telescoping decomposition, then show directly that its scaled
profile-score derivative is strictly increasing. Its sign and the limit of
the score suffice to identify the maximizer or the Poisson supremum.
-/

@[expose] public section

open scoped BigOperators Topology
open Set Filter Real

namespace UnivariateML.NegBinomial

variable {n : ℕ}
noncomputable section

def total (x : Fin n → ℕ) : ℝ := ∑ i, (x i : ℝ)

def mean (x : Fin n → ℕ) : ℝ := total x / n

def variance (x : Fin n → ℕ) : ℝ :=
  (∑ i, ((x i : ℝ) - mean x) ^ 2) / n

def constant (x : Fin n → ℕ) : ℝ := ∑ i, log ((x i).factorial : ℝ)

def logLik (x : Fin n → ℕ) (r p : ℝ) : ℝ :=
  (∑ i, ∑ j ∈ Finset.range (x i), log (r + j)) +
    (n : ℝ) * r * log p + total x * log (1 - p) - constant x

def prob (x : Fin n → ℕ) (r : ℝ) : ℝ := r / (r + mean x)

def profile (x : Fin n → ℕ) (r : ℝ) : ℝ := logLik x r (prob x r)

def poisson (x : Fin n → ℕ) : ℝ :=
  total x * log (mean x) - (n : ℝ) * mean x - constant x

def reciprocalSum (x : Fin n → ℕ) (r : ℝ) : ℝ :=
  ∑ i, ∑ j ∈ Finset.range (x i), (r + (j : ℝ))⁻¹

def score (x : Fin n → ℕ) (r : ℝ) : ℝ :=
  reciprocalSum x r - (n : ℝ) * log (1 + mean x / r)

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioo 0 1

private theorem mean_mul (x : Fin n → ℕ) (hn : 0 < n) :
    (n : ℝ) * mean x = total x := by
  unfold mean
  field_simp

private theorem prob_mem {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) : prob x r ∈ Ioo (0 : ℝ) 1 := by
  have hd : 0 < r + mean x := by positivity
  exact ⟨div_pos hr hd, (div_lt_one hd).2 (by linarith)⟩

/-- The conditional probability optimum strictly improves on every other interior probability. -/
theorem heldSize (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) :
    prob x r ∈ Ioo (0 : ℝ) 1 ∧
      ∀ p ∈ Ioo (0 : ℝ) 1, p ≠ prob x r → logLik x r p < profile x r := by
  refine ⟨prob_mem hm hr, ?_⟩
  intro p hp hne
  have hb := prob_mem hm hr
  have h₁ : log (p / prob x r) < p / prob x r - 1 :=
    log_lt_sub_one_of_pos (div_pos hp.1 hb.1) (by
      intro h
      exact hne ((div_eq_iff (ne_of_gt hb.1)).1 h |>.trans (one_mul _)))
  have h₂ : log ((1-p) / (1-prob x r)) ≤ (1-p) / (1-prob x r) - 1 :=
    log_le_sub_one_of_pos (div_pos (by linarith [hp.2]) (by linarith [hb.2]))
  rw [log_div (ne_of_gt hp.1) (ne_of_gt hb.1)] at h₁
  rw [log_div (by linarith [hp.2]) (by linarith [hb.2])] at h₂
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hweight : 0 < (n : ℝ) * r := mul_pos hn' hr
  have htotal : 0 < total x := by rw [← mean_mul x hn]; positivity
  have hCancel : (n : ℝ) * r * (p / prob x r - 1) +
      total x * ((1-p) / (1-prob x r) - 1) = 0 := by
    rw [← mean_mul x hn]
    have hden : r + mean x ≠ 0 := by positivity
    have hc : 1 - prob x r = mean x / (r + mean x) := by
      unfold prob
      field_simp
      ring
    rw [hc]
    unfold prob
    field_simp
    ring
  have ha := mul_lt_mul_of_pos_left h₁ hweight
  have hb' := mul_le_mul_of_nonneg_left h₂ htotal.le
  unfold profile logLik
  linarith only [ha, hb', hCancel]

private theorem logSum_deriv (x : Fin n → ℕ) {r : ℝ} (hr : 0 < r) :
    HasDerivAt (fun s : ℝ ↦ ∑ i, ∑ j ∈ Finset.range (x i), log (s + j))
      (reciprocalSum x r) r := by
  apply HasDerivAt.fun_sum
  intro i _
  apply HasDerivAt.fun_sum
  intro j _
  simpa using ((hasDerivAt_id r).add_const (j : ℝ)).log (by dsimp; positivity)

private theorem logLinear_deriv (m : ℝ) {r : ℝ} (hr : 0 < r) (hm : 0 < m) :
    HasDerivAt (fun t : ℝ ↦ log (1 + m / t)) (-m / (r * (r + m))) r := by
  have hd : r + m ≠ 0 := by positivity
  have harg : 1 + m / r ≠ 0 := by positivity
  convert (((hasDerivAt_const r m).div (hasDerivAt_id r) hr.ne').const_add 1).log harg
    using 1
  · rfl
  change -m / (r * (r+m)) = ((0*r-m*1)/r^2) / (1+m/r)
  field_simp
  ring

private theorem profile_formula {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) :
    profile x r = (∑ i, ∑ j ∈ Finset.range (x i), log (r + j)) -
      (n : ℝ) * r * log (1 + mean x / r) +
      total x * (log (mean x) - log (r + mean x)) - constant x := by
  have hd : 0 < r + mean x := by positivity
  have hp : 1 - prob x r = mean x / (r + mean x) := by
    unfold prob
    field_simp
    ring
  have he : 1 + mean x / r = (r + mean x) / r := by field_simp
  unfold profile logLik
  rw [hp, he, log_div hm.ne' hd.ne', prob, log_div hr.ne' hd.ne', log_div hd.ne' hr.ne']
  ring

/-- Differentiating the attained profile cancels the derivative of the conditional optimum. -/
theorem profile_hasDerivAt (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) : HasDerivAt (profile x) (score x r) r := by
  have hd : 0 < r + mean x := by positivity
  have h := (((logSum_deriv x hr).sub
    (((hasDerivAt_id r).const_mul (n : ℝ)).mul (logLinear_deriv (mean x) hr hm))).add
    (((hasDerivAt_const r (log (mean x))).sub
      (((hasDerivAt_id r).add_const (mean x)).log hd.ne')).const_mul (total x))).sub_const
      (constant x)
  apply HasDerivAt.congr_of_eventuallyEq _ (eventuallyEq_of_mem (Ioi_mem_nhds hr)
    (fun t ht ↦ profile_formula hm ht))
  convert h using 1
  · rfl
  change reciprocalSum x r - (n : ℝ) * log (1 + mean x/r) =
    reciprocalSum x r - ((n : ℝ)*1*log (1+mean x/r) +
      (n : ℝ)*r*(-mean x/(r*(r+mean x)))) + total x*(0-1/(r+mean x))
  rw [← mean_mul x hn]
  field_simp
  ring


private theorem normalizeLogSum (x : Fin n → ℕ) {r : ℝ} (hr : 0 < r) :
    (∑ i, ∑ j ∈ Finset.range (x i), log (r + j)) =
      (∑ i, ∑ j ∈ Finset.range (x i), log (1 + (j : ℝ) / r)) + total x * log r := by
  rw [total, Finset.sum_mul, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  have hconst : (∑ _ ∈ Finset.range (x i), log r) = (x i : ℝ) * log r := by
    simp [nsmul_eq_mul]
  rw [← hconst, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro j _
  have hj : 0 < 1 + (j : ℝ) / r := by positivity
  rw [← log_mul hj.ne' hr.ne']
  congr 1
  field_simp

private theorem normalizedProfile {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) :
    profile x r =
      (∑ i, ∑ j ∈ Finset.range (x i), log (1 + (j : ℝ) / r)) +
        total x * log (mean x) -
        ((n : ℝ) * r + total x) * log (1 + mean x / r) - constant x := by
  rw [profile_formula hm hr, normalizeLogSum x hr]
  have he : 1 + mean x / r = (r + mean x) / r := by field_simp
  have hd : r + mean x ≠ 0 := by positivity
  rw [he, log_div hd hr.ne']
  ring

private theorem log_ratio :
    Tendsto (fun t : ℝ ↦ log (1 + t) / t) (𝓝[≠] 0) (𝓝 1) := by
  have h := (((hasDerivAt_id (0 : ℝ)).const_add 1).log (by norm_num [id])).tendsto_slope_zero
  simpa [id, div_eq_mul_inv, smul_eq_mul, mul_comm] using h

private theorem const_div_tendsto (c : ℝ) :
    Tendsto (fun r : ℝ ↦ c / r) atTop (𝓝 0) := by
  simpa [div_eq_mul_inv] using tendsto_inv_atTop_zero.const_mul c

private theorem logOnePlus_div_tendsto (c : ℝ) :
    Tendsto (fun r : ℝ ↦ log (1 + c / r)) atTop (𝓝 0) := by
  have h := (tendsto_const_nhds (x := (1 : ℝ))).add (const_div_tendsto c)
  have hc : (1 + 0 : ℝ) ≠ 0 := by norm_num
  simpa [Function.comp_def] using (continuousAt_log hc).tendsto.comp h

/-- The profile converges to the log likelihood of the fitted Poisson model. -/
theorem profile_tendsto_poisson {x : Fin n → ℕ} (hm : 0 < mean x) :
    Tendsto (profile x) atTop (𝓝 (poisson x)) := by
  have hs : Tendsto (fun r : ℝ ↦ ∑ i, ∑ j ∈ Finset.range (x i),
      log (1 + (j : ℝ) / r)) atTop (𝓝 0) := by
    simpa using (tendsto_finsetSum Finset.univ fun i _ ↦
      tendsto_finsetSum (Finset.range (x i)) fun j _ ↦ logOnePlus_div_tendsto (j : ℝ))
  have hne : Tendsto (fun r : ℝ ↦ mean x / r) atTop (𝓝[≠] 0) := by
    refine tendsto_nhdsWithin_iff.mpr ⟨const_div_tendsto _, ?_⟩
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
    exact (div_ne_zero hm.ne' hr.ne')
  have hmain : Tendsto (fun r : ℝ ↦ r * log (1 + mean x / r)) atTop (𝓝 (mean x)) := by
    have h := (log_ratio.comp hne).const_mul (mean x)
    rw [mul_one] at h
    apply h.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
    dsimp
    field_simp [hm.ne', hr.ne']
  have h := ((hs.add_const (total x * log (mean x))).sub
    ((hmain.const_mul (n : ℝ)).add ((logOnePlus_div_tendsto (mean x)).const_mul
      (total x)))).sub_const (constant x)
  simp only [zero_add, mul_zero, add_zero] at h
  change Tendsto _ atTop (𝓝 (poisson x)) at h
  apply h.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
  rw [normalizedProfile hm hr]
  ring

/-- A nonempty count sample has positive mean exactly when some count is positive. -/
theorem mean_pos_iff (hn : 0 < n) (x : Fin n → ℕ) :
    0 < mean x ↔ ∃ i, 0 < x i := by
  constructor
  · intro hm
    by_contra! h
    simp [mean, total, fun i ↦ Nat.eq_zero_of_le_zero (h i)] at hm
  · rintro ⟨i, hi⟩
    have hi' : 0 < (x i : ℝ) := by exact_mod_cast hi
    have hsum : (x i : ℝ) ≤ total x := Finset.single_le_sum
      (f := fun j ↦ (x j : ℝ)) (fun j _ ↦ Nat.cast_nonneg (x j)) (Finset.mem_univ i)
    exact div_pos (hi'.trans_le hsum) (by exact_mod_cast hn)

private theorem hasPositive {x : Fin n → ℕ} (hm : 0 < mean x) :
    ∃ i, 0 < x i := by
  have hn : 0 < n := by
    by_contra! h
    have hz := Nat.eq_zero_of_le_zero h
    simp [mean, hz] at hm
  exact (mean_pos_iff hn x).mp hm

private theorem reciprocal_tendsto (x : Fin n → ℕ) :
    Tendsto (reciprocalSum x) atTop (𝓝 0) := by
  have h (j : ℕ) : Tendsto (fun r : ℝ ↦ (r + (j : ℝ))⁻¹) atTop (𝓝 0) :=
    tendsto_inv_atTop_zero.comp (tendsto_atTop_add_const_right atTop (j : ℝ) tendsto_id)
  unfold reciprocalSum
  simpa using (tendsto_finsetSum Finset.univ fun i _ ↦
    tendsto_finsetSum (Finset.range (x i)) fun j _ ↦ h j)

private theorem reciprocal_lower {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) : r⁻¹ ≤ reciprocalSum x r := by
  obtain ⟨i, hi⟩ := hasPositive hm
  have h₁ : r⁻¹ ≤ ∑ j ∈ Finset.range (x i), (r + (j : ℝ))⁻¹ := by
    simpa using (Finset.single_le_sum
      (f := fun j : ℕ ↦ (r + (j : ℝ))⁻¹) (fun j _ ↦ by positivity)
      (Finset.mem_range.mpr hi))
  exact h₁.trans (Finset.single_le_sum
    (f := fun i ↦ ∑ j ∈ Finset.range (x i), (r + (j : ℝ))⁻¹)
    (fun k _ ↦ Finset.sum_nonneg fun j _ ↦ by positivity) (Finset.mem_univ i))

private theorem reciprocal_strictAnti {x : Fin n → ℕ} (hm : 0 < mean x) :
    StrictAntiOn (reciprocalSum x) (Ioi 0) := by
  intro a ha b _ hab
  have ha' : 0 < a := ha
  obtain ⟨i, hi⟩ := hasPositive hm
  apply Finset.sum_lt_sum
  · intro k _
    exact Finset.sum_le_sum fun j _ ↦ le_of_lt (inv_strictAnti₀ (by positivity) (by linarith))
  · refine ⟨i, Finset.mem_univ _, ?_⟩
    apply Finset.sum_lt_sum
    · intro j _
      exact le_of_lt (inv_strictAnti₀ (by positivity) (by linarith))
    · refine ⟨0, Finset.mem_range.mpr hi, ?_⟩
      exact inv_strictAnti₀ (by positivity) (by linarith)

/-- For a positive-mean sample, holding an interior probability leaves a unique size optimum. -/
theorem heldProb (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    {p : ℝ} (hp : p ∈ Ioo (0 : ℝ) 1) :
    ∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t p) (Ioi 0) r := by
  let c : ℝ := (n : ℝ) * log p
  let g : ℝ → ℝ := fun r ↦ reciprocalSum x r + c
  have hc : c < 0 := mul_neg_of_pos_of_neg (by exact_mod_cast hn) (log_neg hp.1 hp.2)
  have hd (r : ℝ) (hr : 0 < r) : HasDerivAt (fun t ↦ logLik x t p) (g r) r := by
    simpa [logLik, g, c, mul_assoc, mul_comm, mul_left_comm, id] using
      (((logSum_deriv x hr).add (((hasDerivAt_id r).const_mul (n : ℝ)).mul_const (log p))).add_const
        (total x * log (1 - p))).sub_const (constant x)
  have hcont : ContinuousOn g (Ioi 0) := by
    intro r hr
    have hr' : 0 < r := hr
    unfold g
    apply ContinuousWithinAt.add _ continuousWithinAt_const
    exact (show ContinuousAt (reciprocalSum x) r from tendsto_finsetSum _ fun i _ ↦
      tendsto_finsetSum _ fun j _ ↦ (continuousAt_id.add continuousAt_const).inv₀
        (show r + (j : ℝ) ≠ 0 by positivity)).continuousWithinAt
  have hanti : StrictAntiOn g (Ioi 0) := fun a ha b hb hab ↦
    add_lt_add_left (reciprocal_strictAnti hm ha hb hab) c
  let a : ℝ := (1 - c)⁻¹
  have ha : 0 < a := inv_pos.mpr (by linarith)
  have hga : 0 < g a := by
    have h := reciprocal_lower hm ha
    simp [a] at h
    dsimp [g]
    linarith
  have hlim : Tendsto g atTop (𝓝 c) := by simpa [g] using (reciprocal_tendsto x).add_const c
  obtain ⟨b, hab, hgb⟩ := ((eventually_gt_atTop a).and
    (hlim.eventually (Iio_mem_nhds hc))).exists
  have hb : 0 < b := ha.trans hab
  obtain ⟨r, hr, hzero⟩ := intermediate_value_Icc' hab.le
    (hcont.mono (fun t ht ↦ ha.trans_le ht.1)) (show (0 : ℝ) ∈ Icc (g b) (g a) from
      ⟨hgb.le, hga.le⟩)
  have hr' : 0 < r := ha.trans_le hr.1
  have hstrict : ∀ t > 0, t ≠ r → logLik x t p < logLik x r p := by
    intro t ht hne
    rcases lt_or_gt_of_ne hne with htr | hrt
    · apply strictMonoOn_of_deriv_pos (convex_Icc t r)
        (fun z hz ↦ (hd z (ht.trans_le hz.1)).continuousAt.continuousWithinAt)
        (fun z hz ↦ ?_) (left_mem_Icc.mpr htr.le) (right_mem_Icc.mpr htr.le) htr
      rw [interior_Icc] at hz
      rw [(hd z (ht.trans hz.1)).deriv, ← hzero]
      exact hanti (ht.trans hz.1) hr' hz.2
    · apply strictAntiOn_of_deriv_neg (convex_Icc r t)
        (fun z hz ↦ (hd z (hr'.trans_le hz.1)).continuousAt.continuousWithinAt)
        (fun z hz ↦ ?_) (left_mem_Icc.mpr hrt.le) (right_mem_Icc.mpr hrt.le) hrt
      rw [interior_Icc] at hz
      rw [(hd z (hr'.trans hz.1)).deriv, ← hzero]
      exact hanti hr' (hr'.trans hz.1) hz.1
  refine ⟨r, ⟨hr', ?_⟩, fun t ht ↦ ?_⟩
  · intro t ht
    by_cases he : t = r
    · simp [he]
    · exact (hstrict t ht he).le
  · by_contra he
    exact (hstrict t ht.1 he).not_ge (ht.2 hr')

/-- For all-zero data the likelihood extended to include both degenerate faces. -/
def zeroLikelihood (n : ℕ) (r p : ℝ) : ℝ := p ^ ((n : ℝ) * r)

theorem zeroLikelihood_eq_one_iff (hn : 0 < n) {r p : ℝ} (hr : 0 ≤ r)
    (hp : 0 < p) (hp' : p ≤ 1) :
    zeroLikelihood n r p ≤ 1 ∧ (zeroLikelihood n r p = 1 ↔ r = 0 ∨ p = 1) := by
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  by_cases h : r = 0 ∨ p = 1
  · rcases h with h | h <;> simp [zeroLikelihood, h]
  · have hr' : 0 < r := lt_of_le_of_ne hr (fun e ↦ h (Or.inl e.symm))
    have hlt : p < 1 := lt_of_le_of_ne hp' (fun e ↦ h (Or.inr e))
    have hs := rpow_lt_one hp.le hlt (mul_pos hn' hr')
    exact ⟨hs.le, iff_of_false hs.ne h⟩


theorem zeroLikelihood_eq_exp_logLik (x : Fin n → ℕ) (hx : ∀ i, x i = 0)
    {r p : ℝ} (hp : 0 < p) :
    zeroLikelihood n r p = exp (logLik x r p) := by
  rw [zeroLikelihood, rpow_def_of_pos hp]
  simp [logLik, total, constant, hx, mul_comm]

-- The linear part removed from each term makes the sum vanish at infinity.
private def centered (m : ℝ) (q : ℕ) (r : ℝ) : ℝ :=
  ∑ j ∈ Finset.range q,
    (r * (r + m)^2 / (r + (j : ℝ))^2 - (r + 2 * (m - j)))

private def middle (m : ℝ) (j : ℕ) (r : ℝ) : ℝ :=
  (j + 1 : ℕ) * (1 + (m - (j + 1 : ℕ)) / (r + (j + 1 : ℕ)))^2 / (r + j)

-- The dossier's telescoping identity after subtracting the linear terms.
private theorem centered_low (m : ℝ) (k : ℕ) {r : ℝ} (hr : 0 < r) :
    centered m (k+1) r =
      ((k : ℝ) + 1) * (m-k)^2 / (r+k) + ∑ j ∈ Finset.range k, middle m j r := by
  induction k with
  | zero =>
    simp [centered]
    field_simp
    ring
  | succ k ih =>
    rw [centered, Finset.sum_range_succ, ← centered, ih, Finset.sum_range_succ]
    simp only [middle, Nat.cast_succ]
    have hk : r + (k : ℝ) ≠ 0 := by positivity
    have hk' : r + ((k : ℝ) + 1) ≠ 0 := by positivity
    have hk'' : r + (k : ℝ) + 1 ≠ 0 := by positivity
    field_simp [hk, hk', hk'']
    ring

private theorem middle_strictAnti (m : ℝ) (j : ℕ) (hj : ((j : ℝ) + 1) ≤ m) :
    StrictAntiOn (middle m j) (Ioi 0) := by
  intro a ha b _ hab
  have ha' : 0 < a := ha
  have hb' := ha'.trans hab
  simp only [middle, Nat.cast_succ, div_eq_mul_inv]
  have h₁ : (b + (j : ℝ) + 1)⁻¹ < (a + (j : ℝ) + 1)⁻¹ :=
    inv_strictAnti₀ (by positivity) (by linarith)
  have h₂ : 0 ≤ m - ((j : ℝ) + 1) := sub_nonneg.mpr hj
  have h₃ : 0 < 1 + (m - ((j : ℝ) + 1)) * (a + ((j : ℝ) + 1))⁻¹ := by positivity
  have hp : 0 ≤ 1 + (m - ((j : ℝ) + 1)) * (b + ((j : ℝ) + 1))⁻¹ := by positivity
  have h₄ : (1 + (m - ((j : ℝ) + 1)) * (b + ((j : ℝ) + 1))⁻¹)^2 ≤
      (1 + (m - ((j : ℝ) + 1)) * (a + ((j : ℝ) + 1))⁻¹)^2 := by
    apply pow_le_pow_left₀ hp _ 2
    exact add_le_add_right (mul_le_mul_of_nonneg_left (by simpa [add_assoc] using h₁.le) h₂) 1
  apply lt_of_le_of_lt (mul_le_mul_of_nonneg_right
    (mul_le_mul_of_nonneg_left h₄ (by positivity)) (by positivity))
  exact mul_lt_mul_of_pos_left (inv_strictAnti₀ (by positivity) (by linarith)) (by positivity)

private theorem tail_strictAnti (m k : ℝ) (hm : 0 < m) (hk : m < k) :
    StrictAntiOn (fun r : ℝ ↦ r * (r + m)^2 / (r + k)^2 - (r + 2 * (m - k))) (Ioi 0) := by
  have hk' : 0 < k := hm.trans hk
  have hd (r : ℝ) (hr : 0 < r) :
      HasDerivAt (fun r : ℝ ↦ r * (r + m)^2 / (r + k)^2 - (r + 2 * (m - k)))
        (-(k-m) * ((3*k-m)*r + k*(k+m)) / (r+k)^3) r := by
    have hden : r+k ≠ 0 := by positivity
    convert ((((hasDerivAt_id r).mul (((hasDerivAt_id r).add_const m).pow 2)).div
      (((hasDerivAt_id r).add_const k).pow 2) (pow_ne_zero 2 hden)).sub
      ((hasDerivAt_id r).add_const (2*(m-k)))) using 1
    · rfl
    change -(k-m)*((3*k-m)*r+k*(k+m))/(r+k)^3 =
      ((1*(r+m)^2+r*(2*(r+m)^(2-1)*1))*(r+k)^2 -
        (r*(r+m)^2)*(2*(r+k)^(2-1)*1))/((r+k)^2)^2-1
    norm_num
    field_simp
    ring
  apply strictAntiOn_of_deriv_neg (convex_Ioi 0)
    (fun r hr ↦ (hd r hr).continuousAt.continuousWithinAt)
  intro r hr
  rw [interior_Ioi] at hr
  rw [(hd r hr).deriv]
  have hr : 0 < r := hr
  have h₁ : 0 < k-m := sub_pos.mpr hk
  have h₂ : 0 < 3*k-m := by linarith
  exact div_neg_of_neg_of_pos (mul_neg_of_neg_of_pos (neg_neg_of_pos h₁) (by positivity))
    (by positivity)

private theorem centered_strictAnti (m : ℝ) (hm : 0 < m) (q : ℕ) (hq : 0 < q) :
    StrictAntiOn (centered m q) (Ioi 0) := by
  induction q with
  | zero => omega
  | succ k ih =>
    by_cases hk : (k : ℝ) ≤ m
    · intro a ha b hb hab
      have ha' : 0 < a := ha
      rw [centered_low m k ha, centered_low m k hb]
      have hr : (b + (k : ℝ))⁻¹ < (a + (k : ℝ))⁻¹ :=
        inv_strictAnti₀ (by positivity) (by linarith)
      have hterm : ((k : ℝ) + 1) * (m-k)^2 / (b+k) ≤
          ((k : ℝ) + 1) * (m-k)^2 / (a+k) := by
        simpa only [div_eq_mul_inv] using mul_le_mul_of_nonneg_left hr.le
          (show 0 ≤ ((k : ℝ) + 1) * (m-k)^2 by positivity)
      by_cases hz : k = 0
      · subst k
        simp only [Nat.cast_zero, sub_zero, zero_add, add_zero, Finset.range_zero,
          Finset.sum_empty, one_mul] at *
        simpa only [div_eq_mul_inv] using mul_lt_mul_of_pos_left hr (sq_pos_of_pos hm)
      · apply add_lt_add_of_le_of_lt hterm
        apply Finset.sum_lt_sum
        · intro j hj
          have hj' : ((j : ℝ) + 1) ≤ k := by
            exact_mod_cast Nat.succ_le_of_lt (Finset.mem_range.mp hj)
          exact (middle_strictAnti m j (hj'.trans hk) ha hb hab).le
        · refine ⟨0, Finset.mem_range.mpr (Nat.pos_of_ne_zero hz), ?_⟩
          apply middle_strictAnti m 0 _ ha hb hab
          have hk' : (1 : ℝ) ≤ k := by exact_mod_cast Nat.pos_of_ne_zero hz
          simpa using hk'.trans hk
    · have hk' : m < (k : ℝ) := lt_of_not_ge hk
      have hkpos : 0 < k := by
        by_contra! h
        simp [Nat.eq_zero_of_le_zero h] at hk'
        linarith
      intro a ha b hb hab
      simp only [centered, Finset.sum_range_succ]
      exact add_lt_add (ih hkpos ha hb hab) (tail_strictAnti m k hm hk' ha hb hab)

private theorem centered_tendsto (m : ℝ) (q : ℕ) :
    Tendsto (centered m q) atTop (𝓝 0) := by
  have h (j : ℕ) : Tendsto (fun r : ℝ ↦
      r * (r + m)^2 / (r + (j : ℝ))^2 - (r + 2*(m-j))) atTop (𝓝 0) := by
    have h₁ := (tendsto_inv_atTop_zero : Tendsto (fun r : ℝ ↦ r⁻¹) atTop (𝓝 0))
    have h₂ := ((tendsto_const_nhds (x := (1 : ℝ))).add (const_div_tendsto (j : ℝ))).pow 2
    have h₃ := (h₁.const_mul (((j : ℝ)-m)*(3*(j : ℝ)-m))).add
      ((h₁.pow 2).const_mul (((j : ℝ)-m)*2*(j : ℝ)^2))
    have hh := h₃.div h₂ (by norm_num)
    simp only [mul_zero, zero_pow (by decide : (2 : ℕ) ≠ 0), add_zero, zero_div,
      one_pow] at hh
    apply hh.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
    have hr' : r ≠ 0 := hr.ne'
    have hd : r + (j : ℝ) ≠ 0 := by positivity
    have hd' : 1 + (j : ℝ) / r = (r + j) / r := by field_simp
    dsimp
    rw [hd']
    field_simp
    ring
  unfold centered
  simpa using tendsto_finsetSum (Finset.range q) (fun j _ ↦ h j)


-- Multiplication by r(r+m)^2 preserves the sign of the profile-score derivative.
private def curvature (x : Fin n → ℕ) (r : ℝ) : ℝ :=
  (n : ℝ) * (variance x - mean x) - ∑ i, centered (mean x) (x i) r

private theorem curvature_strictMono {x : Fin n → ℕ} (hm : 0 < mean x) :
    StrictMonoOn (curvature x) (Ioi 0) := by
  intro a ha b hb hab
  obtain ⟨i, hi⟩ := hasPositive hm
  apply sub_lt_sub_left
  apply Finset.sum_lt_sum
  · intro j _
    by_cases hj : 0 < x j
    · exact (centered_strictAnti _ hm _ hj ha hb hab).le
    · simp [centered, Nat.eq_zero_of_le_zero (Nat.le_of_not_gt hj)]
  · exact ⟨i, Finset.mem_univ _, centered_strictAnti _ hm _ hi ha hb hab⟩

private theorem curvature_limit (x : Fin n → ℕ) :
    Tendsto (curvature x) atTop (𝓝 ((n : ℝ) * (variance x - mean x))) := by
  have h := tendsto_finsetSum Finset.univ (fun i _ ↦ centered_tendsto (mean x) (x i))
  unfold curvature
  simpa using h.const_sub ((n : ℝ) * (variance x - mean x))

private theorem twice_index_sum (q : ℕ) :
    (∑ j ∈ Finset.range q, (2 : ℝ)*j) = (q : ℝ) * ((q : ℝ)-1) := by
  induction q with
  | zero => simp
  | succ q ih =>
    rw [Finset.sum_range_succ, ih, Nat.cast_succ]
    ring

private theorem variance_mul (hn : 0 < n) (x : Fin n → ℕ) :
    (n : ℝ) * variance x = (∑ i, (x i : ℝ)^2) - (n : ℝ) * mean x ^ 2 := by
  have h (i : Fin n) : ((x i : ℝ) - mean x)^2 =
      (x i : ℝ)^2 - (x i : ℝ) * (2 * mean x) + mean x ^ 2 := by ring
  unfold variance
  rw [mul_div_cancel₀ _ (by exact_mod_cast (Nat.ne_of_gt hn)), Finset.sum_congr rfl (fun i _ ↦ h i),
    Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.sum_mul, ← total,
    ← mean_mul x hn]
  simp
  ring

private theorem curvature_formula (hn : 0 < n) (x : Fin n → ℕ) (r : ℝ) :
    curvature x r = (n : ℝ) * mean x * (r + mean x) -
      ∑ i, ∑ j ∈ Finset.range (x i), r * (r + mean x)^2 / (r+(j : ℝ))^2 := by
  unfold curvature centered
  simp only [Finset.sum_sub_distrib]
  have h (i : Fin n) : (∑ j ∈ Finset.range (x i), (r + 2*(mean x-(j : ℝ)))) =
      (x i : ℝ) * (r+2*mean x) - (x i : ℝ) * ((x i : ℝ)-1) := by
    simp_rw [show ∀ j : ℕ, r+2*(mean x-(j : ℝ)) = (r+2*mean x) - 2*j by intro; ring,
      Finset.sum_sub_distrib, twice_index_sum, Finset.sum_const, Finset.card_range, nsmul_eq_mul]
  rw [Finset.sum_congr rfl (fun i _ ↦ h i), Finset.sum_sub_distrib, ← Finset.sum_mul, ← total]
  simp_rw [show ∀ i : Fin n, (x i : ℝ)*((x i : ℝ)-1) = (x i : ℝ)^2 - (x i : ℝ) by intro; ring,
    Finset.sum_sub_distrib]
  rw [← total, mul_sub, variance_mul hn, ← mean_mul x hn]
  ring

private theorem score_hasDerivAt (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) :
    HasDerivAt (score x) (curvature x r / (r * (r+mean x)^2)) r := by
  have hs : HasDerivAt (reciprocalSum x)
      (∑ i, ∑ j ∈ Finset.range (x i), -(1 : ℝ)/(r+(j : ℝ))^2) r := by
    apply HasDerivAt.fun_sum
    intro i _
    apply HasDerivAt.fun_sum
    intro j _
    exact ((hasDerivAt_id r).add_const (j : ℝ)).inv
      (show r + (j : ℝ) ≠ 0 by positivity)
  convert hs.sub ((logLinear_deriv (mean x) hr hm).const_mul (n : ℝ)) using 1
  · rfl
  rw [curvature_formula hn, sub_div]
  have hd : r + mean x ≠ 0 := by positivity
  simp_rw [Finset.sum_div, neg_div, Finset.sum_neg_distrib]
  have he (j : ℕ) : r * (r + mean x)^2 / (r + (j : ℝ))^2 /
      (r * (r + mean x)^2) = 1 / (r + (j : ℝ))^2 := by
    have hj : r + (j : ℝ) ≠ 0 := by positivity
    field_simp
  simp_rw [he]
  field_simp
  ring

private theorem score_limit (x : Fin n → ℕ) :
    Tendsto (score x) atTop (𝓝 0) := by
  unfold score
  simpa using (reciprocal_tendsto x).sub
    ((logOnePlus_div_tendsto (mean x)).const_mul (n : ℝ))

private theorem score_eventually_pos {x : Fin n → ℕ} (hm : 0 < mean x) :
    ∀ᶠ r in 𝓝[>] (0 : ℝ), 0 < r ∧ 0 < score x r := by
  have h₁ : Tendsto (fun r : ℝ ↦ log r * r) (𝓝[>] (0 : ℝ)) (𝓝 0) := by
    have h : Tendsto (fun r : ℝ ↦ r * log r) (𝓝 0) (𝓝 (0 * log 0)) :=
      continuous_mul_log.continuousAt.tendsto
    simpa only [zero_mul, mul_comm] using h.mono_left
      (nhdsWithin_le_nhds : 𝓝[>] (0 : ℝ) ≤ 𝓝 0)
  have h₂ : Tendsto (fun r : ℝ ↦ log (r + mean x) * r) (𝓝[>] (0 : ℝ)) (𝓝 0) := by
    have h := ((continuousAt_id.add continuousAt_const).log
      (show (0 : ℝ) + mean x ≠ 0 by simpa using hm.ne')).mul continuousAt_id
    have ht : Tendsto (fun r : ℝ ↦ log (r + mean x) * r) (𝓝 0)
        (𝓝 (log (0 + mean x) * 0)) := h.tendsto
    simpa only [mul_zero] using ht.mono_left
      (nhdsWithin_le_nhds : 𝓝[>] (0 : ℝ) ≤ 𝓝 0)
  have ht : Tendsto (fun r : ℝ ↦ (n : ℝ) * (log (r+mean x)*r - log r*r))
      (𝓝[>] (0 : ℝ)) (𝓝 0) := by simpa using (h₂.sub h₁).const_mul (n : ℝ)
  filter_upwards [self_mem_nhdsWithin, ht.eventually (Iio_mem_nhds (zero_lt_one' ℝ))] with r hr h
  have hr' : 0 < r := hr
  have hh : (n : ℝ) * (log (r+mean x)*r - log r*r) < 1 := h
  have hd : r+mean x ≠ 0 := by positivity
  have he : 1 + mean x / r = (r+mean x)/r := by field_simp
  have hb := mul_le_mul_of_nonneg_right (reciprocal_lower hm hr') hr'.le
  rw [inv_mul_cancel₀ hr'.ne'] at hb
  refine ⟨hr', pos_of_mul_pos_left (show score x r * r > 0 from ?_) hr'.le⟩
  rw [score, he, log_div hd hr'.ne']
  linarith only [hb, hh]

private theorem curvature_lt_limit {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) : curvature x r < (n : ℝ) * (variance x - mean x) := by
  have hrs : r < r + 1 := lt_add_of_pos_right _ zero_lt_one
  have hs : 0 < r + 1 := hr.trans hrs
  apply lt_of_lt_of_le (curvature_strictMono hm hr hs hrs)
  apply ge_of_tendsto (curvature_limit x)
  filter_upwards [eventually_ge_atTop (r + 1)] with t ht
  exact (curvature_strictMono hm).monotoneOn hs (hs.trans_le ht) ht

private theorem score_neg_of_curvature_nonneg (hn : 0 < n) {x : Fin n → ℕ}
    (hm : 0 < mean x) {r : ℝ} (hr : 0 < r) (hc : 0 ≤ curvature x r) :
    score x r < 0 := by
  have hmono : StrictMonoOn (score x) (Ici r) := by
    apply strictMonoOn_of_deriv_pos (convex_Ici r)
      (fun t ht ↦ (score_hasDerivAt hn hm (hr.trans_le ht)).continuousAt.continuousWithinAt)
    intro t ht
    rw [interior_Ici] at ht
    have hp := hr.trans ht
    rw [(score_hasDerivAt hn hm hp).deriv]
    exact div_pos (hc.trans_lt (curvature_strictMono hm hr hp ht)) (by positivity)
  have hrs : r < r+1 := lt_add_of_pos_right _ zero_lt_one
  apply lt_of_lt_of_le (hmono (mem_Ici.mpr (le_refl r))
    (mem_Ici.mpr hrs.le) hrs)
  apply ge_of_tendsto (score_limit x)
  filter_upwards [eventually_ge_atTop (r+1)] with t ht
  exact hmono.monotoneOn (mem_Ici.mpr hrs.le) (mem_Ici.mpr (hrs.le.trans ht)) ht

private theorem score_anti_to (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hc : curvature x r ≤ 0) :
    StrictAntiOn (score x) (Ioc 0 r) := by
  apply strictAntiOn_of_deriv_neg (convex_Ioc 0 r)
    (fun t ht ↦ (score_hasDerivAt hn hm ht.1).continuousAt.continuousWithinAt)
  intro t ht
  rw [interior_Ioc] at ht
  have hr : 0 < r := ht.1.trans ht.2
  rw [(score_hasDerivAt hn hm ht.1).deriv]
  exact div_neg_of_neg_of_pos ((curvature_strictMono hm ht.1 hr ht.2).trans_le hc)
    (by have := ht.1; positivity)

private theorem score_pos_of_variance_le (hn : 0 < n) {x : Fin n → ℕ}
    (hm : 0 < mean x) (hv : variance x ≤ mean x) {r : ℝ} (hr : 0 < r) :
    0 < score x r := by
  have hc (t : ℝ) (ht : 0 < t) : curvature x t ≤ 0 :=
    (curvature_lt_limit hm ht).le.trans (mul_nonpos_of_nonneg_of_nonpos (by positivity)
      (sub_nonpos.mpr hv))
  have hstep : score x (r+1) < score x r :=
    score_anti_to hn hm (hc (r+1) (by positivity)) ⟨hr, by linarith⟩
      ⟨by positivity, le_rfl⟩ (by linarith)
  apply lt_of_le_of_lt _ hstep
  apply le_of_tendsto (score_limit x)
  filter_upwards [eventually_ge_atTop (r+1)] with t ht
  have ht' : 0 < t := by linarith
  exact (score_anti_to hn hm (hc t ht')).antitoneOn ⟨by positivity, ht⟩ ⟨ht', le_rfl⟩ ht

private theorem score_root (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    (hv : mean x < variance x) :
    ∃ r > 0, (∀ t ∈ Ioo 0 r, 0 < score x t) ∧ (∀ t > r, score x t < 0) := by
  have hlim : 0 < (n : ℝ) * (variance x - mean x) :=
    mul_pos (by exact_mod_cast hn) (sub_pos.mpr hv)
  obtain ⟨b, hb, hcb⟩ := ((eventually_gt_atTop (0 : ℝ)).and
    ((curvature_limit x).eventually (Ioi_mem_nhds hlim))).exists
  have hgb := score_neg_of_curvature_nonneg hn hm hb (le_of_lt hcb)
  obtain ⟨a, ⟨ha, hga⟩, hab⟩ := ((score_eventually_pos hm).and
    (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds hb))).exists
  obtain ⟨r, hr, hz⟩ := intermediate_value_Icc' (le_of_lt hab)
    (fun t ht ↦ (score_hasDerivAt hn hm (ha.trans_le ht.1)).continuousAt.continuousWithinAt)
    (show (0 : ℝ) ∈ Icc (score x b) (score x a) from ⟨hgb.le, hga.le⟩)
  have hr' := ha.trans_le hr.1
  have hc : curvature x r < 0 := by
    by_contra! h
    exact (score_neg_of_curvature_nonneg hn hm hr' h).ne hz
  refine ⟨r, hr', ?_, ?_⟩
  · intro t ht
    rw [← hz]
    exact score_anti_to hn hm hc.le ⟨ht.1, ht.2.le⟩ ⟨hr', le_rfl⟩ ht.2
  · intro t ht
    by_cases h : 0 ≤ curvature x t
    · exact score_neg_of_curvature_nonneg hn hm (hr'.trans ht) h
    · rw [← hz]
      exact score_anti_to hn hm (le_of_not_ge h) ⟨hr', ht.le⟩
        ⟨hr'.trans ht, le_rfl⟩ ht

private theorem logLik_le_profile {x : Fin n → ℕ} (hn : 0 < n) (hm : 0 < mean x)
    {r p : ℝ} (hr : 0 < r) (hp : p ∈ Ioo (0 : ℝ) 1) :
    logLik x r p ≤ profile x r := by
  by_cases h : p = prob x r
  · simp [h, profile]
  · exact ((heldSize hn hm hr).2 p hp h).le

/-- When variance is at most the mean, every feasible pair has log likelihood
strictly below the Poisson limit, which is the supremum of feasible values. -/
theorem poisson_supremum (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    (hv : variance x ≤ mean x) :
    (∀ q ∈ domain, logLik x q.1 q.2 < poisson x) ∧
      sSup ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' domain) = poisson x := by
  have hmprof : StrictMonoOn (profile x) (Ioi 0) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioi 0)
      (fun t ht ↦ (profile_hasDerivAt hn hm ht).continuousAt.continuousWithinAt)
    intro t ht
    rw [interior_Ioi] at ht
    rw [(profile_hasDerivAt hn hm ht).deriv]
    exact score_pos_of_variance_le hn hm hv ht
  have hlt (q : ℝ × ℝ) (hq : q ∈ domain) : logLik x q.1 q.2 < poisson x := by
    have hr : 0 < q.1 := hq.1
    have hs : 0 < q.1+1 := by positivity
    apply lt_of_lt_of_le ((logLik_le_profile hn hm hr hq.2).trans_lt
      (hmprof hr hs (lt_add_of_pos_right _ zero_lt_one)))
    apply ge_of_tendsto (profile_tendsto_poisson hm)
    filter_upwards [eventually_ge_atTop (q.1+1)] with t ht
    exact hmprof.monotoneOn hs (hs.trans_le ht) ht
  refine ⟨hlt, ?_⟩
  let f : ℝ × ℝ → ℝ := fun q ↦ logLik x q.1 q.2
  have hB : BddAbove (f '' domain) := ⟨poisson x, fun _ ⟨q, hq, he⟩ ↦ he ▸ (hlt q hq).le⟩
  have hD : (f '' domain).Nonempty := ⟨f (1, prob x 1), (1, prob x 1),
    ⟨mem_Ioi.mpr (zero_lt_one : (0 : ℝ) < 1), prob_mem hm zero_lt_one⟩, rfl⟩
  apply le_antisymm (csSup_le hD (fun _ ⟨q, hq, he⟩ ↦ he ▸ (hlt q hq).le))
  apply le_of_tendsto (profile_tendsto_poisson hm)
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
  exact le_csSup hB ⟨(r, prob x r), ⟨hr, prob_mem hm hr⟩, rfl⟩

/-- A positive-mean sample has exactly one finite interior joint MLE
if and only if its population-form variance exceeds its mean. -/
theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x) :
    (∃! q : ℝ × ℝ, q ∈ domain ∧
      IsMaxOn (fun q ↦ logLik x q.1 q.2) domain q) ↔ mean x < variance x := by
  constructor
  · rintro ⟨q, ⟨hq, hmax⟩, _⟩
    by_contra! hv
    have h := (poisson_supremum hn hm hv).1 q hq
    let f : ℝ × ℝ → ℝ := fun q ↦ logLik x q.1 q.2
    have hnonempty : (f '' domain).Nonempty := ⟨f q, q, hq, rfl⟩
    have hub : sSup (f '' domain) ≤ f q := by
      apply csSup_le hnonempty
      rintro y ⟨t, ht, rfl⟩
      exact hmax ht
    have he : sSup (f '' domain) = poisson x := (poisson_supremum hn hm hv).2
    exact h.not_ge (he ▸ hub)
  · intro hv
    obtain ⟨r, hr, hleft, hright⟩ := score_root hn hm hv
    have hstrict (t : ℝ) (ht : 0 < t) (hne : t ≠ r) : profile x t < profile x r := by
      rcases lt_or_gt_of_ne hne with htr | hrt
      · apply strictMonoOn_of_deriv_pos (convex_Icc t r)
          (fun z hz ↦ (profile_hasDerivAt hn hm (ht.trans_le hz.1)).continuousAt.continuousWithinAt)
          (fun z hz ↦ ?_) (left_mem_Icc.mpr htr.le) (right_mem_Icc.mpr htr.le) htr
        rw [interior_Icc] at hz
        rw [(profile_hasDerivAt hn hm (ht.trans hz.1)).deriv]
        exact hleft z ⟨ht.trans hz.1, hz.2⟩
      · apply strictAntiOn_of_deriv_neg (convex_Icc r t)
          (fun z hz ↦ (profile_hasDerivAt hn hm (hr.trans_le hz.1)).continuousAt.continuousWithinAt)
          (fun z hz ↦ ?_) (left_mem_Icc.mpr hrt.le) (right_mem_Icc.mpr hrt.le) hrt
        rw [interior_Icc] at hz
        rw [(profile_hasDerivAt hn hm (hr.trans hz.1)).deriv]
        exact hright z hz.1
    let best := (r, prob x r)
    have hb : best ∈ domain := ⟨hr, prob_mem hm hr⟩
    have hlt (q : ℝ × ℝ) (hq : q ∈ domain) (hne : q ≠ best) :
        logLik x q.1 q.2 < logLik x best.1 best.2 := by
      by_cases he : q.1 = r
      · have hp : q.2 ≠ prob x r := fun hp ↦ hne (Prod.ext he hp)
        simpa [he, best, profile] using (heldSize hn hm hr).2 q.2 hq.2 hp
      · exact (logLik_le_profile hn hm hq.1 hq.2).trans_lt (hstrict q.1 hq.1 he)
    refine ⟨best, ⟨hb, ?_⟩, fun q hq ↦ ?_⟩
    · intro q hq
      by_cases he : q = best
      · simp [he]
      · exact (hlt q hq he).le
    · by_contra he
      exact (hlt q hq.1 he).not_ge (hq.2 hb)

end

end UnivariateML.NegBinomial
