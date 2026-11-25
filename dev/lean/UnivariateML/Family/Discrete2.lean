module

public import UnivariateML.Generic.Score
public import UnivariateML.Generic.Limit
public import UnivariateML.Family.Laplace
public import Mathlib.Analysis.PSeries
public import Mathlib.Analysis.Normed.Group.Tannery
public import Mathlib.Analysis.Calculus.SmoothSeries
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false
set_option maxHeartbeats 800000

/-!
# Four discrete likelihoods

The sample is nonempty. Natural numbers encode positive counts and integers
encode the discrete Laplace lattice. The boundary statements concern the
supremum over the declared open parameter domains.

## Deviations

Log-series, zeta and discrete Laplace use log likelihood per observation.
Yule–Simon uses the finite product for the integer-count beta law.
The zeta theorems concern the infinite-support family. For the finite
truncation, the dossier's strict concavity requires at least two support
points, as its details specify. These are exact-real results.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators

noncomputable section

namespace UnivariateML

private theorem boundary_sup {P : Type*} {D : Set P} {f : P → ℝ} {c : ℝ}
    {l : Filter ℝ} [l.NeBot] {p : ℝ → P}
    (hp : ∀ᶠ t in l, p t ∈ D) (hl : Tendsto (fun t ↦ f (p t)) l (𝓝 c))
    (hlt : ∀ q ∈ D, f q < c) :
    sSup (f '' D) = c ∧ ¬ ∃ q ∈ D, IsMaxOn f D q := by
  have hu : c ∈ upperBounds (f '' D) := by
    rintro _ ⟨q, hq, rfl⟩
    exact (hlt q hq).le
  obtain ⟨t, ht⟩ := hp.exists
  refine ⟨le_antisymm (csSup_le ⟨_, mem_image_of_mem f ht⟩ hu)
    (Generic.limit_le_sSup hp hl ⟨c, hu⟩), ?_⟩
  rintro ⟨q, hq, hm⟩
  exact (hlt q hq).not_ge
    (Generic.limit_le_upperBound hp hl (by rintro _ ⟨y, hy, rfl⟩; exact hm hy))

private theorem peak_of_signs {D : Set ℝ} {f g : ℝ → ℝ} {r : ℝ}
    (hD : Convex ℝ D) (hr : r ∈ D)
    (hd : ∀ t ∈ D, HasDerivAt f (g t) t)
    (hl : ∀ t ∈ D, t < r → 0 < g t)
    (hu : ∀ t ∈ D, r < t → g t < 0) :
    ∀ t ∈ D, IsMaxOn f D t ↔ t = r := by
  have hf : ContinuousOn f D := fun t ht ↦ (hd t ht).continuousAt.continuousWithinAt
  have hinc : StrictMonoOn f (D ∩ Iic r) := by
    apply strictMonoOn_of_deriv_pos (hD.inter (convex_Iic r)) (hf.mono inter_subset_left)
    intro t ht
    have htD := (interior_subset ht).1
    rw [(hd t htD).deriv]
    exact hl t htD (by simpa using interior_mono inter_subset_right ht)
  have hdec : StrictAntiOn f (D ∩ Ici r) := by
    apply strictAntiOn_of_deriv_neg (hD.inter (convex_Ici r)) (hf.mono inter_subset_left)
    intro t ht
    have htD := (interior_subset ht).1
    rw [(hd t htD).deriv]
    exact hu t htD (by simpa using interior_mono inter_subset_right ht)
  have hlt (t : ℝ) (ht : t ∈ D) (hne : t ≠ r) : f t < f r := by
    rcases lt_or_gt_of_ne hne with h | h
    · exact hinc ⟨ht, h.le⟩ ⟨hr, le_rfl⟩ h
    · exact hdec ⟨hr, le_rfl⟩ ⟨ht, h.le⟩ h
  intro t ht
  refine ⟨fun hm ↦ by by_contra h; exact (hlt t ht h).not_ge (hm hr), ?_⟩
  rintro rfl y hy
  by_cases h : y = t
  · simp [h]
  · exact (hlt y hy h).le

private theorem sum_nonneg_pos_iff {n : ℕ} {f : Fin n → ℝ} (hf : ∀ i, 0 ≤ f i) :
    0 < ∑ i, f i ↔ ∃ i, 0 < f i := by
  constructor
  · contrapose!
    intro h
    exact Finset.sum_nonpos fun i _ ↦ h i
  · rintro ⟨i, hi⟩
    exact Finset.sum_pos' (fun j _ ↦ hf j) ⟨i, Finset.mem_univ _, hi⟩

namespace YuleSimon

variable {n : ℕ}

/-- The integer-count beta likelihood written as a finite product in logarithms. -/
def logLik (x : Fin n → ℕ) (ρ : ℝ) : ℝ :=
  ∑ i, (log ρ + (∑ j ∈ Finset.range (x i),
    (log ((j : ℝ) + 1) - log (ρ + ((j : ℝ) + 1)))) - log (x i : ℝ))

/-- The score in log shape. -/
def logShapeScore (x : Fin n → ℕ) (t : ℝ) : ℝ :=
  ∑ i, (1 - ∑ j ∈ Finset.range (x i), exp t / (exp t + ((j : ℝ) + 1)))

private theorem hasDerivAt_logShape (x : Fin n → ℕ) (t : ℝ) :
    HasDerivAt (fun u ↦ logLik x (exp u)) (logShapeScore x t) t := by
  have hterm (j : ℕ) : HasDerivAt (fun u ↦ log ((j : ℝ) + 1) - log (exp u + ((j : ℝ) + 1)))
      (-(exp t / (exp t + ((j : ℝ) + 1)))) t := by
    have hpos : 0 < exp t + ((j : ℝ) + 1) := add_pos (exp_pos _) (by positivity)
    simpa [Pi.sub_def, div_eq_inv_mul, mul_comm] using
      (hasDerivAt_const t (log ((j : ℝ) + 1))).sub
        (((hasDerivAt_exp t).add_const ((j : ℝ) + 1)).log hpos.ne')
  unfold logLik logShapeScore
  apply HasDerivAt.fun_sum
  intro i _
  simpa [Pi.add_def, ← Finset.sum_neg_distrib, sub_eq_add_neg] using
    ((hasDerivAt_id t).add (HasDerivAt.fun_sum fun j (_ : j ∈ Finset.range (x i)) ↦ hterm j)).sub_const
      (log (x i : ℝ))

private theorem score_continuous (x : Fin n → ℕ) : Continuous (logShapeScore x) := by
  unfold logShapeScore
  exact continuous_finsetSum _ fun i _ ↦ continuous_const.sub
    (continuous_finsetSum _ fun j _ ↦ continuous_exp.div (continuous_exp.add continuous_const)
      fun t ↦ (add_pos (exp_pos t) (show 0 < ((j : ℝ) + 1) by positivity)).ne')

private theorem score_strictAnti (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i) :
    StrictAnti (logShapeScore x) := by
  intro s t hst
  have hterm (j : ℕ) : exp s / (exp s + ((j : ℝ) + 1)) < exp t / (exp t + ((j : ℝ) + 1)) := by
    have hj : 0 < ((j : ℝ) + 1) := by positivity
    apply (div_lt_div_iff₀ (add_pos (exp_pos _) hj) (add_pos (exp_pos _) hj)).2
    nlinarith [mul_pos (sub_pos.mpr (exp_lt_exp.mpr hst)) hj]
  unfold logShapeScore
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro i _
  apply sub_lt_sub_left
  exact Finset.sum_lt_sum_of_nonempty ⟨0, Finset.mem_range.mpr (hx i)⟩ fun j _ ↦ hterm j

private theorem score_limits (x : Fin n → ℕ) :
    Tendsto (logShapeScore x) atBot (𝓝 (n : ℝ)) ∧
      Tendsto (logShapeScore x) atTop (𝓝 ((n : ℝ) - ∑ i, (x i : ℝ))) := by
  have hleft (j : ℕ) :
      Tendsto (fun t ↦ exp t / (exp t + ((j : ℝ) + 1))) atBot (𝓝 0) := by
    have hj : 0 < ((j : ℝ) + 1) := by positivity
    simpa [Pi.div_def] using tendsto_exp_atBot.div (tendsto_exp_atBot.add_const ((j : ℝ) + 1)) (by simpa using hj.ne')
  have hright (j : ℕ) :
      Tendsto (fun t ↦ exp t / (exp t + ((j : ℝ) + 1))) atTop (𝓝 1) := by
    have ht := ((tendsto_inv_atTop_zero.comp tendsto_exp_atTop).const_mul (((j : ℝ) + 1))).const_add 1
    have ht' := (tendsto_const_nhds (x := (1 : ℝ))).div ht (by norm_num : (1 + (((j : ℝ) + 1) : ℝ) * 0) ≠ 0)
    have ht'' : Tendsto (fun t ↦ 1 / (1 + ((j : ℝ) + 1) * (exp t)⁻¹)) atTop (𝓝 1) := by
      simpa [Pi.div_def, Function.comp_def] using ht'
    apply ht''.congr
    intro t
    have hne := (exp_pos t).ne'
    have hden : 0 < exp t + ((j : ℝ) + 1) := add_pos (exp_pos _) (by positivity)
    field_simp [hne, hden.ne']
  unfold logShapeScore
  constructor
  · simpa [Pi.sub_def, Finset.sum_sub_distrib] using
      tendsto_finsetSum Finset.univ (fun i _ ↦
        tendsto_const_nhds (x := (1 : ℝ)) |>.sub (tendsto_finsetSum (Finset.range (x i)) fun j _ ↦ hleft j))
  · simpa [Pi.sub_def, Finset.sum_sub_distrib] using
      tendsto_finsetSum Finset.univ (fun i _ ↦
        tendsto_const_nhds (x := (1 : ℝ)) |>.sub (tendsto_finsetSum (Finset.range (x i)) fun j _ ↦ hright j))

/-- A count above one gives exactly one score root, which characterizes the MLE. -/
theorem mle (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i)
    (h : ∃ i, 1 < x i) :
    (∃! ρ, 0 < ρ ∧ logShapeScore x (log ρ) = 0) ∧
      ∀ ρ > 0, IsMaxOn (logLik x) (Ioi 0) ρ ↔ logShapeScore x (log ρ) = 0 := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  have hneg : (n : ℝ) - ∑ i, (x i : ℝ) < 0 := by
    obtain ⟨i, hi⟩ := h
    have hsum : (∑ _ : Fin n, (1 : ℝ)) < ∑ j, (x j : ℝ) :=
      Finset.sum_lt_sum (fun j _ ↦ by exact_mod_cast hx j) ⟨i, Finset.mem_univ _, by exact_mod_cast hi⟩
    simpa using sub_neg.mpr hsum
  exact Generic.existsUnique_log_score_zero (hasDerivAt_logShape x) (score_continuous x)
    (score_strictAnti hn hx) hnR hneg (score_limits x).1 (score_limits x).2

private theorem allOnes_formula {x : Fin n → ℕ} (hx : ∀ i, x i = 1) {ρ : ℝ}
    (hρ : 0 < ρ) :
    logLik x ρ = n * log (1 / (1 + ρ⁻¹)) := by
  have h : log ρ - log (ρ + 1) = log (1 / (1 + ρ⁻¹)) := by
    rw [← log_div hρ.ne' (by positivity)]
    congr 1
    field_simp [hρ.ne', show ρ + 1 ≠ 0 by positivity]
  simp only [logLik, hx, Finset.sum_range_one, Nat.cast_zero, Nat.cast_one, zero_add, log_one, zero_sub, sub_zero, ← sub_eq_add_neg, h, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]

/-- In the all-one case the log likelihood tends to zero at infinite shape. -/
theorem allOnes_boundary (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i = 1) :
    Tendsto (logLik x) atTop (𝓝 0) ∧
      sSup (logLik x '' Ioi 0) = 0 ∧ ¬ ∃ ρ ∈ Ioi 0, IsMaxOn (logLik x) (Ioi 0) ρ := by
  have hp : ∀ᶠ ρ : ℝ in atTop, ρ ∈ Ioi (0 : ℝ) := eventually_gt_atTop 0
  have ht : Tendsto (fun ρ : ℝ ↦ 1 + ρ⁻¹) atTop (𝓝 1) := by
    simpa using (tendsto_inv_atTop_zero : Tendsto (fun ρ : ℝ ↦ ρ⁻¹) atTop (𝓝 0)).const_add 1
  have ht' : Tendsto (fun ρ : ℝ ↦ 1 / (1 + ρ⁻¹)) atTop (𝓝 1) := by
    simpa [one_div] using ht.inv₀ one_ne_zero
  have hl : Tendsto (logLik x) atTop (𝓝 0) := by
    have h := ((continuousAt_log one_ne_zero).tendsto.comp ht').const_mul (n : ℝ)
    simpa using h.congr' (hp.mono fun ρ hρ ↦ (allOnes_formula hx hρ).symm)
  refine ⟨hl, boundary_sup hp hl ?_⟩
  intro ρ hρ
  change 0 < ρ at hρ
  rw [allOnes_formula hx hρ]
  apply mul_neg_of_pos_of_neg (by exact_mod_cast hn)
  exact log_neg (by positivity) ((div_lt_one (by positivity)).2 (by linarith [inv_pos.mpr hρ]))

/-- Finite maximization is possible exactly when some count exceeds one. -/
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i) :
    (∃! ρ, ρ ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x) (Ioi 0) ρ) ↔ ∃ i, 1 < x i := by
  by_cases h : ∃ i, 1 < x i
  · have hm := mle hn hx h
    obtain ⟨ρ, hρ, hu⟩ := hm.1
    exact iff_of_true ⟨ρ, ⟨hρ.1, (hm.2 ρ hρ.1).2 hρ.2⟩,
      fun t ht ↦ hu t ⟨ht.1, (hm.2 t ht.1).1 ht.2⟩⟩ h
  · have ha : ∀ i, x i = 1 := fun i ↦ by have := hx i; have := not_exists.mp h i; omega
    exact iff_of_false (fun ⟨ρ, hρ, _⟩ ↦ (allOnes_boundary hn ha).2.2 ⟨ρ, hρ⟩) h

end YuleSimon

namespace DiscreteLaplace

variable {n : ℕ}

/-- The mean absolute deviation at an integer location. -/
def deviation (x : Fin n → ℤ) (μ : ℤ) : ℝ :=
  Laplace.meanAbsDev (fun i ↦ (x i : ℝ)) μ

/-- Log likelihood per observation. -/
def logLik (d b : ℝ) : ℝ := log (1 - b) - log (1 + b) + d * log b

/-- Closed-form scale at positive deviation. -/
def scale (d : ℝ) : ℝ := d / (sqrt (1 + d ^ 2) + 1)

private def numerator (d b : ℝ) : ℝ := d * (1 - b ^ 2) - 2 * b

private theorem hasDerivAt_logLik (d : ℝ) {b : ℝ} (hb : b ∈ Ioo (0 : ℝ) 1) :
    HasDerivAt (logLik d) (numerator d b / (b * (1 - b ^ 2))) b := by
  have hb0 : 0 < b := hb.1
  have hb1 : b < 1 := hb.2
  have h₁ : 1 - b ≠ 0 := by linarith
  have h₂ : 1 + b ≠ 0 := by linarith
  have h₃ : 1 - b ^ 2 ≠ 0 := by nlinarith [mul_pos (sub_pos.mpr hb1) (show 0 < 1 + b by positivity)]
  convert (((((hasDerivAt_id b).const_sub 1).log h₁).sub
    (((hasDerivAt_id b).const_add 1).log h₂)).add
    ((hasDerivAt_log hb0.ne').const_mul d)) using 1
  · rfl
  simp only [id_eq]
  unfold numerator
  field_simp [hb0.ne', h₁, h₂, h₃]
  ring

private theorem scale_spec {d : ℝ} (hd : 0 < d) :
    scale d ∈ Ioo (0 : ℝ) 1 ∧ numerator d (scale d) = 0 := by
  have hs := sq_sqrt (show 0 ≤ 1 + d ^ 2 by positivity)
  have hpos : 0 < sqrt (1 + d ^ 2) + 1 := by positivity
  have hlt : d < sqrt (1 + d ^ 2) + 1 := by
    nlinarith [sqrt_nonneg (1 + d ^ 2)]
  refine ⟨⟨div_pos hd hpos, (div_lt_one hpos).2 hlt⟩, ?_⟩
  unfold numerator scale
  field_simp
  linear_combination d * hs

private theorem numerator_strictAnti {d : ℝ} (hd : 0 ≤ d) :
    StrictAntiOn (numerator d) (Ioo (0 : ℝ) 1) := by
  intro a ha b hb hab
  have ha0 : 0 < a := ha.1
  have hb0 : 0 < b := hb.1
  have hsq : a ^ 2 ≤ b ^ 2 := by gcongr
  unfold numerator
  linarith [mul_nonneg hd (sub_nonneg.mpr hsq)]

/-- Positive deviation gives a unique interior scale, the closed-form root. -/
theorem scale_mle {d : ℝ} (hd : 0 < d) :
    scale d ∈ Ioo (0 : ℝ) 1 ∧
      ∀ b ∈ Ioo (0 : ℝ) 1, IsMaxOn (logLik d) (Ioo 0 1) b ↔ b = scale d := by
  obtain ⟨hr, hz⟩ := scale_spec hd
  have hden (b : ℝ) (hb : b ∈ Ioo (0 : ℝ) 1) : 0 < b * (1 - b ^ 2) := by
    apply mul_pos hb.1
    nlinarith [mul_pos (sub_pos.mpr hb.2) (show 0 < 1 + b from add_pos zero_lt_one hb.1)]
  refine ⟨hr, peak_of_signs (convex_Ioo 0 1) hr (fun _ hb ↦ hasDerivAt_logLik d hb) ?_ ?_⟩
  · intro b hb h
    have hnum := numerator_strictAnti hd.le hb hr h
    rw [hz] at hnum
    exact div_pos hnum (hden b hb)
  · intro b hb h
    have hnum := numerator_strictAnti hd.le hr hb h
    rw [hz] at hnum
    exact div_neg_of_neg_of_pos hnum (hden b hb)

private theorem zero_lt_bound {d b : ℝ} (hd : 0 ≤ d) (hb : b ∈ Ioo (0 : ℝ) 1) :
    logLik d b < 0 := by
  have h₁ := log_neg (sub_pos.mpr hb.2) (show 1 - b < 1 by linarith [hb.1])
  have h₂ := log_pos (show 1 < 1 + b by linarith [hb.1])
  have h₃ := mul_nonpos_of_nonneg_of_nonpos hd (log_neg hb.1 hb.2).le
  unfold logLik
  linarith

/-- Zero deviation gives an unattained log-likelihood supremum at scale zero. -/
theorem zero_boundary :
    Tendsto (logLik 0) (𝓝[>] 0) (𝓝 0) ∧
      sSup (logLik 0 '' Ioo 0 1) = 0 ∧ ¬ ∃ b ∈ Ioo 0 1, IsMaxOn (logLik 0) (Ioo 0 1) b := by
  have h₁ : ContinuousAt (fun b : ℝ ↦ log (1 - b)) 0 :=
    (continuousAt_log (by norm_num : (1 : ℝ) - 0 ≠ 0)).comp
      (continuous_const.sub continuous_id).continuousAt
  have h₂ : ContinuousAt (fun b : ℝ ↦ log (1 + b)) 0 :=
    (continuousAt_log (by norm_num : (1 : ℝ) + 0 ≠ 0)).comp
      (continuous_const.add continuous_id).continuousAt
  have hl : Tendsto (logLik 0) (𝓝[>] 0) (𝓝 0) := by
    have h := (h₁.sub h₂).tendsto.mono_left (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
    simpa only [show logLik 0 = (fun b : ℝ ↦ log (1 - b)) - (fun b ↦ log (1 + b)) by
      funext b; simp [logLik], Pi.sub_apply, sub_zero, add_zero, log_one] using h
  have hp : ∀ᶠ b in 𝓝[>] (0 : ℝ), b ∈ Ioo (0 : ℝ) 1 :=
    (show ∀ᶠ b in 𝓝[>] (0 : ℝ), b ∈ Ioi (0 : ℝ) from self_mem_nhdsWithin).and
      (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one))
  exact ⟨hl, boundary_sup hp hl (fun _ ↦ zero_lt_bound le_rfl)⟩

/-- A held integer location has an interior scale exactly when some residual is nonzero. -/
theorem heldLocation_existsUnique_iff (hn : 0 < n) (x : Fin n → ℤ) (μ : ℤ) :
    (∃! b, b ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn (logLik (deviation x μ)) (Ioo 0 1) b) ↔
       ∃ i, x i ≠ μ := by
  have hz : deviation x μ = 0 ↔ ∀ i, x i = μ := by
    rw [deviation, Laplace.meanAbsDev_zero_iff hn]
    simp
  have hd : 0 ≤ deviation x μ := by
    unfold deviation Laplace.meanAbsDev
    positivity
  by_cases h : ∃ i, x i ≠ μ
  · have hp := lt_of_le_of_ne hd (Ne.symm (fun he ↦ by
      obtain ⟨i, hi⟩ := h
      exact hi (hz.mp he i)))
    obtain ⟨hb, hm⟩ := scale_mle hp
    exact iff_of_true ⟨_, ⟨hb, (hm _ hb).2 rfl⟩, fun b he ↦ (hm b he.1).1 he.2⟩ h
  · have he := hz.mpr (fun i ↦ not_ne_iff.mp (not_exists.mp h i))
    rw [he]
    exact iff_of_false (fun ⟨b, hb, _⟩ ↦ zero_boundary.2.2 ⟨b, hb⟩) h

/-- Mean absolute deviations order the likelihoods in the opposite direction. -/
private theorem scale_order {a d b : ℝ} (hb : b ∈ Ioo (0 : ℝ) 1) :
    logLik a b ≤ logLik d b ↔ d ≤ a := by
  have hl := log_neg hb.1 hb.2
  unfold logLik
  rw [add_le_add_iff_left, mul_le_mul_right_of_neg hl]

private theorem continuousLaplace_order (hn : 0 < n) (x : Fin n → ℝ) (a b : ℝ) :
    Laplace.logLik x b 1 ≤ Laplace.logLik x a 1 ↔
      Laplace.meanAbsDev x a ≤ Laplace.meanAbsDev x b := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  simp only [Laplace.logLik, Laplace.logDensity, mul_one, div_one, Finset.sum_sub_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, Laplace.meanAbsDev,
    sub_le_sub_iff_left, div_le_div_iff_of_pos_right hnR]

/-- For fixed scale, the maximizing integer locations are exactly the integer medians. -/
theorem heldScale (hn : 0 < n) (x : Fin n → ℤ) {b : ℝ} (hb : b ∈ Ioo (0 : ℝ) 1)
    (μ : ℤ) :
    IsMaxOn (fun t ↦ logLik (deviation x t) b) univ μ ↔
      (μ : ℝ) ∈ Laplace.medianSet (fun i ↦ (x i : ℝ)) hn := by
  let y : Fin n → ℝ := fun i ↦ x i
  let j : Fin n := ⟨(n - 1) / 2, by omega⟩
  let m : ℤ := x (Tuple.sort y j)
  have hm : (m : ℝ) = Laplace.lowerMedian y hn := rfl
  have hmedian : ∀ a : ℝ, IsMaxOn (fun t ↦ Laplace.logLik y t 1) univ a ↔
      a ∈ Laplace.medianSet y hn := by
    intro a
    exact Set.ext_iff.mp (Laplace.heldScale hn y (b := 1) zero_lt_one) a
  have hmem : (m : ℝ) ∈ Laplace.medianSet y hn := by
    rw [hm]
    exact ⟨le_rfl, Tuple.monotone_sort y (show j ≤ (⟨n / 2, by omega⟩ : Fin n) by
      change (n - 1) / 2 ≤ n / 2; omega)⟩
  have hmax := (hmedian m).2 hmem
  constructor
  · intro h
    apply (hmedian μ).1
    intro a _
    exact (hmax (mem_univ a)).trans
      ((continuousLaplace_order hn y μ m).2 ((scale_order hb).1 (h (mem_univ m))))
  · intro h a _
    exact (scale_order hb).2
      ((continuousLaplace_order hn y μ a).1 (((hmedian μ).2 h) (mem_univ (a : ℝ))))

/-- The joint domain consists of integer locations and interior scales. -/
def domain : Set (ℤ × ℝ) := univ ×ˢ Ioo 0 1

def jointLogLik (x : Fin n → ℤ) (p : ℤ × ℝ) : ℝ := logLik (deviation x p.1) p.2

private theorem deviation_nonneg (x : Fin n → ℤ) (μ : ℤ) : 0 ≤ deviation x μ := by
  unfold deviation Laplace.meanAbsDev
  positivity

private theorem deviation_zero_iff (hn : 0 < n) (x : Fin n → ℤ) (μ : ℤ) :
    deviation x μ = 0 ↔ ∀ i, x i = μ := by
  rw [deviation, Laplace.meanAbsDev_zero_iff hn]
  simp

/-- For nonconstant data, the integer medians paired with the common scale are all the MLEs. -/
theorem joint (hn : 0 < n) {x : Fin n → ℤ} (hx : ∃ i j, x i ≠ x j) {p : ℤ × ℝ}
    (hp : p ∈ domain) :
    IsMaxOn (jointLogLik x) domain p ↔
      (p.1 : ℝ) ∈ Laplace.medianSet (fun i ↦ (x i : ℝ)) hn ∧
        p.2 = scale (deviation x p.1) := by
  have hd : 0 < deviation x p.1 := by
    refine lt_of_le_of_ne (deviation_nonneg x p.1) (Ne.symm fun hz ↦ ?_)
    obtain ⟨i, j, hij⟩ := hx
    have he := (deviation_zero_iff hn x p.1).1 hz
    exact hij ((he i).trans (he j).symm)
  obtain ⟨_, hm⟩ := scale_mle hd
  constructor
  · intro h
    refine ⟨(heldScale hn x hp.2 p.1).1 (fun a _ ↦ h (show (a, p.2) ∈ domain from
      ⟨mem_univ _, hp.2⟩)), (hm p.2 hp.2).1 ?_⟩
    intro b hb
    exact h (show (p.1, b) ∈ domain from ⟨mem_univ _, hb⟩)
  · rintro ⟨hμ, hb⟩ q hq
    have hloc := (heldScale hn x hq.2 p.1).2 hμ (mem_univ q.1)
    have hscale := (hm p.2 hp.2).2 hb hq.2
    exact hloc.trans hscale

/-- Constant data escape to scale zero at the common location. -/
theorem joint_constant_boundary (hn : 0 < n) {x : Fin n → ℤ} {μ : ℤ} (hx : ∀ i, x i = μ) :
    Tendsto (fun b ↦ jointLogLik x (μ, b)) (𝓝[>] 0) (𝓝 0) ∧
      sSup (jointLogLik x '' domain) = 0 ∧ ¬ ∃ p ∈ domain, IsMaxOn (jointLogLik x) domain p := by
  have hz := (deviation_zero_iff hn x μ).2 hx
  have hl : Tendsto (fun b ↦ jointLogLik x (μ, b)) (𝓝[>] 0) (𝓝 0) := by
    simpa only [jointLogLik, hz] using zero_boundary.1
  have hp : ∀ᶠ b in 𝓝[>] (0 : ℝ), (μ, b) ∈ domain :=
    (show ∀ᶠ b in 𝓝[>] (0 : ℝ), b ∈ Ioi (0 : ℝ) from self_mem_nhdsWithin).and
      (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one)) |>.mono
        (fun _ h ↦ ⟨mem_univ _, h⟩)
  exact ⟨hl, boundary_sup hp hl (fun p h ↦ zero_lt_bound (deviation_nonneg x p.1) h.2)⟩

/-- At a held location with zero residuals, scale zero is the excluded limit. -/
theorem heldLocation_boundary (hn : 0 < n) {x : Fin n → ℤ} {μ : ℤ} (hx : ∀ i, x i = μ) :
    Tendsto (logLik (deviation x μ)) (𝓝[>] 0) (𝓝 0) ∧
      sSup (logLik (deviation x μ) '' Ioo 0 1) = 0 ∧
      ¬ ∃ b ∈ Ioo 0 1, IsMaxOn (logLik (deviation x μ)) (Ioo 0 1) b := by
  rw [(deviation_zero_iff hn x μ).2 hx]
  exact zero_boundary

/-- A joint finite fit exists exactly for nonconstant integer samples. -/
theorem joint_exists_iff (hn : 0 < n) (x : Fin n → ℤ) :
    (∃ p ∈ domain, IsMaxOn (jointLogLik x) domain p) ↔ ∃ i j, x i ≠ x j := by
  let i₀ : Fin n := ⟨0, hn⟩
  by_cases hx : ∃ i j, x i ≠ x j
  · let y : Fin n → ℝ := fun i ↦ x i
    let j : Fin n := ⟨(n - 1) / 2, by omega⟩
    let m : ℤ := x (Tuple.sort y j)
    have hmem : (m : ℝ) ∈ Laplace.medianSet y hn := by
      change Laplace.lowerMedian y hn ∈ Laplace.medianSet y hn
      exact ⟨le_rfl, Tuple.monotone_sort y (show j ≤ (⟨n / 2, by omega⟩ : Fin n) by
        change (n - 1) / 2 ≤ n / 2; omega)⟩
    have hd : 0 < deviation x m := by
      refine lt_of_le_of_ne (deviation_nonneg x m) (Ne.symm fun hz ↦ ?_)
      obtain ⟨i, j, hij⟩ := hx
      have he := (deviation_zero_iff hn x m).1 hz
      exact hij ((he i).trans (he j).symm)
    have hp : (m, scale (deviation x m)) ∈ domain := ⟨mem_univ _, (scale_mle hd).1⟩
    exact iff_of_true ⟨_, hp, (joint hn hx hp).2 ⟨hmem, rfl⟩⟩ hx
  · have he : ∀ i, x i = x i₀ := fun i ↦ not_ne_iff.mp (not_exists.mp (not_exists.mp hx i) i₀)
    exact iff_of_false (joint_constant_boundary hn he).2.2 hx

/-- Integer medians all have the same deviation and hence the same fitted scale. -/
theorem median_scale_eq (hn : 0 < n) {x : Fin n → ℤ} {μ ν : ℤ}
    (hμ : (μ : ℝ) ∈ Laplace.medianSet (fun i ↦ (x i : ℝ)) hn)
    (hν : (ν : ℝ) ∈ Laplace.medianSet (fun i ↦ (x i : ℝ)) hn) :
    scale (deviation x μ) = scale (deviation x ν) :=
  congrArg scale (Laplace.meanAbsDev_eq_of_mem_medianSet hn hμ hν)

/-- A nonconstant sample has a unique joint fit exactly when the central ranks agree. -/
theorem joint_unique_iff (hn : 0 < n) {x : Fin n → ℤ} (hx : ∃ i j, x i ≠ x j) :
    (∃! p, p ∈ domain ∧ IsMaxOn (jointLogLik x) domain p) ↔
      Laplace.lowerMedian (fun i ↦ (x i : ℝ)) hn =
        Laplace.upperMedian (fun i ↦ (x i : ℝ)) hn := by
  let y : Fin n → ℝ := fun i ↦ x i
  let lo : Fin n := ⟨(n - 1) / 2, by omega⟩
  let hi : Fin n := ⟨n / 2, by omega⟩
  let a : ℤ := x (Tuple.sort y lo)
  let b : ℤ := x (Tuple.sort y hi)
  have hab : (a : ℝ) ≤ b := Tuple.monotone_sort y (show lo ≤ hi by
    change (n - 1) / 2 ≤ n / 2; omega)
  have hfit (μ : ℤ) (hμ : (μ : ℝ) ∈ Laplace.medianSet y hn) :
      (μ, scale (deviation x μ)) ∈ domain ∧
        IsMaxOn (jointLogLik x) domain (μ, scale (deviation x μ)) := by
    have hd : 0 < deviation x μ := by
      refine lt_of_le_of_ne (deviation_nonneg x μ) (Ne.symm fun hz ↦ ?_)
      obtain ⟨i, j, hij⟩ := hx
      have he := (deviation_zero_iff hn x μ).1 hz
      exact hij ((he i).trans (he j).symm)
    have hp : (μ, scale (deviation x μ)) ∈ domain := ⟨mem_univ _, (scale_mle hd).1⟩
    exact ⟨hp, (joint hn hx hp).2 ⟨hμ, rfl⟩⟩
  have ha := hfit a ⟨le_rfl, hab⟩
  have hb := hfit b ⟨hab, le_rfl⟩
  constructor
  · rintro ⟨p, _, hu⟩
    have he := congrArg Prod.fst ((hu _ ha).trans (hu _ hb).symm)
    exact congrArg (fun t : ℤ ↦ (t : ℝ)) he
  · intro he
    refine ⟨_, ha, ?_⟩
    rintro p ⟨hp, hm⟩
    obtain ⟨hμ, hs⟩ := (joint hn hx hp).1 hm
    have hμ' : (p.1 : ℝ) = a := le_antisymm (hμ.2.trans_eq he.symm) hμ.1
    have hpa : p.1 = a := by exact_mod_cast hμ'
    exact Prod.ext hpa (hs.trans (congrArg (fun t ↦ scale (deviation x t)) hpa))

end DiscreteLaplace

namespace LogSeries

variable {n : ℕ}

def empiricalMean (x : Fin n → ℕ) : ℝ := (∑ i, (x i : ℝ)) / n

def logLik (x : Fin n → ℕ) (θ : ℝ) : ℝ :=
  empiricalMean x * log θ - log (-log (1 - θ)) -
    (∑ i, log (x i : ℝ)) / n

def modelMean (u : ℝ) : ℝ := (exp u - 1) / u

def coordinate (θ : ℝ) : ℝ := -log (1 - θ)
private def parameter (u : ℝ) : ℝ := 1 - exp (-u)

private theorem parameter_mem {u : ℝ} (hu : 0 < u) : parameter u ∈ Ioo (0 : ℝ) 1 :=
  ⟨sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos hu)), sub_lt_self _ (exp_pos _)⟩

private theorem coordinate_pos {θ : ℝ} (hθ : θ ∈ Ioo (0 : ℝ) 1) : 0 < coordinate θ :=
  neg_pos.mpr (log_neg (sub_pos.mpr hθ.2) (sub_lt_self _ hθ.1))

private theorem coordinate_parameter (u : ℝ) : coordinate (parameter u) = u := by
  simp [coordinate, parameter, log_exp]

private theorem coordinate_strictMono : StrictMonoOn coordinate (Ioo (0 : ℝ) 1) := by
  intro a ha b hb hab
  exact neg_lt_neg (log_lt_log (sub_pos.mpr hb.2) (sub_lt_sub_left hab 1))

/-- The mean in the solver coordinate increases strictly from one to infinity. -/
theorem modelMean_spec :
    StrictMonoOn modelMean (Ioi 0) ∧ ContinuousOn modelMean (Ioi 0) ∧
      Tendsto modelMean (𝓝[>] 0) (𝓝 1) ∧ Tendsto modelMean atTop atTop := by
  refine ⟨?_, ?_, ?_, ?_⟩
  · intro a ha b _ hab
    have ha' : 0 < a := ha
    have hb : 0 < b := ha'.trans hab
    have h := strictConvexOn_exp.2 (mem_univ (0 : ℝ)) (mem_univ b) hb.ne
      (show 0 < 1 - a / b by exact sub_pos.mpr ((div_lt_one hb).2 hab))
      (div_pos ha' hb) (sub_add_cancel 1 (a / b))
    simp only [smul_eq_mul, mul_zero, zero_add, div_mul_cancel₀ _ hb.ne', exp_zero] at h
    apply (div_lt_div_iff₀ ha' hb).2
    have h' := mul_lt_mul_of_pos_right h hb
    field_simp [hb.ne'] at h'
    linarith
  · exact (continuous_exp.sub continuous_const).continuousOn.div continuousOn_id
      (fun u hu ↦ (show 0 < u from hu).ne')
  · unfold modelMean
    simpa [div_eq_inv_mul] using (hasDerivAt_exp 0).tendsto_slope_zero_right
  · unfold modelMean
    simpa [pow_one, sub_eq_add_neg] using
      tendsto_mul_exp_add_div_pow_atTop 1 (-1) 1 (by norm_num)

private theorem modelMean_gt_one {u : ℝ} (hu : 0 < u) : 1 < modelMean u := by
  apply (lt_div_iff₀ hu).2
  have h := log_lt_sub_one_of_pos (exp_pos u) (fun h ↦ hu.ne' (exp_injective (h.trans exp_zero.symm)))
  rw [log_exp] at h
  simpa using h

private theorem derivative (x : Fin n → ℕ) {θ : ℝ} (hθ : θ ∈ Ioo (0 : ℝ) 1) :
    HasDerivAt (logLik x) ((empiricalMean x - modelMean (coordinate θ)) / θ) θ := by
  have hθ0 := hθ.1
  have hθ1 := sub_pos.mpr hθ.2
  have hc := coordinate_pos hθ
  have h := (((hasDerivAt_log hθ0.ne').const_mul (empiricalMean x)).sub
    ((((hasDerivAt_id θ).const_sub 1).log hθ1.ne').neg.log hc.ne')).sub_const
       ((∑ i, log (x i : ℝ)) / n)
  convert h using 1
  · rfl
  simp only [id_eq, Pi.neg_apply, modelMean, coordinate, exp_neg, exp_log hθ1]
  field_simp [hθ0.ne', hθ1.ne', show log (1 - θ) ≠ 0 from neg_ne_zero.mp hc.ne']
  ring

private theorem empiricalMean_gt_one_iff (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i) :
    1 < empiricalMean x ↔ ∃ i, 1 < x i := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  have he : (∑ i, ((x i : ℝ) - 1)) = (∑ i, (x i : ℝ)) - n := by
    simp [Finset.sum_sub_distrib]
  rw [empiricalMean, lt_div_iff₀ hnR, one_mul, ← sub_pos, ← he,
    sum_nonneg_pos_iff (fun i ↦ sub_nonneg.mpr (by exact_mod_cast hx i))]
  exact exists_congr fun i ↦ by rw [sub_pos]; exact_mod_cast Iff.rfl

/-- A sample mean above one gives exactly one mean-matching parameter and MLE. -/
theorem mle {x : Fin n → ℕ} (hx : 1 < empiricalMean x) :
    (∃! θ ∈ Ioo (0 : ℝ) 1, modelMean (coordinate θ) = empiricalMean x) ∧
      ∀ θ ∈ Ioo (0 : ℝ) 1, IsMaxOn (logLik x) (Ioo 0 1) θ ↔
        modelMean (coordinate θ) = empiricalMean x := by
  obtain ⟨hmono, hc, hzero, hinf⟩ := modelMean_spec
  obtain ⟨a, ha, hsa⟩ :=
    ((show ∀ᶠ u in 𝓝[>] (0 : ℝ), u ∈ Ioi (0 : ℝ) from self_mem_nhdsWithin).and
      (hzero.eventually (Iio_mem_nhds hx))).exists
  obtain ⟨b, hb, hsb⟩ := ((eventually_gt_atTop (0 : ℝ)).and
    ((tendsto_atTop.1 hinf (empiricalMean x)))).exists
  have hab : a ≤ b := le_of_not_gt fun h ↦ (hmono hb ha h).not_ge (hsa.le.trans hsb)
  have hsub : Icc a b ⊆ Ioi (0 : ℝ) := fun _ h ↦ ha.trans_le h.1
  obtain ⟨r, hr, hz⟩ := intermediate_value_Icc hab (hc.mono hsub)
    (show empiricalMean x ∈ Icc (modelMean a) (modelMean b) from ⟨hsa.le, hsb⟩)
  have hθ := parameter_mem (hsub hr)
  have hroot : modelMean (coordinate (parameter r)) = empiricalMean x := by
    simpa [coordinate_parameter] using hz
  have huniq (θ : ℝ) (ht : θ ∈ Ioo (0 : ℝ) 1) (he : modelMean (coordinate θ) = empiricalMean x) :
      θ = parameter r := by
    apply coordinate_strictMono.injOn ht hθ
    exact hmono.injOn (coordinate_pos ht) (coordinate_pos hθ) (he.trans hroot.symm)
  refine ⟨⟨_, ⟨hθ, hroot⟩, fun t ht ↦ huniq t ht.1 ht.2⟩, ?_⟩
  have hm := peak_of_signs (convex_Ioo 0 1) hθ (fun t ht ↦ derivative x ht)
    (fun t ht h ↦ div_pos (sub_pos.mpr (hroot ▸
      hmono (coordinate_pos ht) (coordinate_pos hθ) (coordinate_strictMono ht hθ h))) ht.1)
    (fun t ht h ↦ div_neg_of_neg_of_pos (sub_neg.mpr (hroot ▸
      hmono (coordinate_pos hθ) (coordinate_pos ht) (coordinate_strictMono hθ ht h))) ht.1)
  exact fun t ht ↦ (hm t ht).trans ⟨fun h ↦ h.symm ▸ hroot, huniq t ht⟩

private theorem allOnes_formula (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i = 1)
    {θ : ℝ} (hθ : θ ∈ Ioo (0 : ℝ) 1) :
    logLik x θ = -log (coordinate θ / θ) := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  have hm : empiricalMean x = 1 := by simp [empiricalMean, hx, hnR.ne']
  rw [log_div (coordinate_pos hθ).ne' hθ.1.ne']
  simp [logLik, hm, hx, coordinate, neg_sub]

/-- All ones give a log-likelihood supremum of zero as theta decreases to zero. -/
theorem allOnes_boundary (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i = 1) :
    Tendsto (logLik x) (𝓝[>] 0) (𝓝 0) ∧ sSup (logLik x '' Ioo 0 1) = 0 ∧
      ¬ ∃ θ ∈ Ioo 0 1, IsMaxOn (logLik x) (Ioo 0 1) θ := by
  have hp : ∀ᶠ θ in 𝓝[>] (0 : ℝ), θ ∈ Ioo (0 : ℝ) 1 :=
    (show ∀ᶠ θ in 𝓝[>] (0 : ℝ), θ ∈ Ioi (0 : ℝ) from self_mem_nhdsWithin).and
      (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one))
  have hd : HasDerivAt coordinate 1 0 := by
    unfold coordinate
    simpa [Pi.neg_def] using (((hasDerivAt_id (0 : ℝ)).const_sub 1).log (by norm_num)).neg
  have ht : Tendsto (fun θ ↦ coordinate θ / θ) (𝓝[>] 0) (𝓝 1) := by
    simpa [coordinate, div_eq_inv_mul] using hd.tendsto_slope_zero_right
  have hl : Tendsto (logLik x) (𝓝[>] 0) (𝓝 0) := by
    have h := ((continuousAt_log one_ne_zero).tendsto.comp ht).neg
    simpa using h.congr' (hp.mono fun θ hθ ↦ (allOnes_formula hn hx hθ).symm)
  refine ⟨hl, boundary_sup hp hl ?_⟩
  intro θ hθ
  rw [allOnes_formula hn hx hθ]
  apply neg_neg_of_pos
  apply log_pos
  apply (lt_div_iff₀ hθ.1).2
  have h := log_lt_sub_one_of_pos (sub_pos.mpr hθ.2) (by linarith [hθ.1] : 1 - θ ≠ 1)
  unfold coordinate
  linarith

/-- The interior MLE exists and is unique exactly when the empirical mean exceeds one. -/
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i) :
    (∃! θ, θ ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn (logLik x) (Ioo 0 1) θ) ↔ 1 < empiricalMean x := by
  by_cases h : 1 < empiricalMean x
  · obtain ⟨⟨θ, hθ, hu⟩, hm⟩ := mle h
    exact iff_of_true ⟨θ, ⟨hθ.1, (hm θ hθ.1).2 hθ.2⟩,
      fun t ht ↦ hu t ⟨ht.1, (hm t ht.1).1 ht.2⟩⟩ h
  · have he := not_exists.mp (mt (empiricalMean_gt_one_iff hn hx).2 h)
    have ha : ∀ i, x i = 1 := fun i ↦ by have := hx i; have := he i; omega
    exact iff_of_false (fun ⟨θ, hθ, _⟩ ↦ (allOnes_boundary hn ha).2.2 ⟨θ, hθ⟩) h

end LogSeries

namespace Zeta

variable {n : ℕ}

/-- Unnormalized mass of the count `k + 1`. -/
def weight (s : ℝ) (k : ℕ) : ℝ := exp (-s * log ((k : ℝ) + 1))

/-- The normalizer, expressed as a convergent series on `s > 1`. -/
def normalizer (s : ℝ) : ℝ := ∑' k, weight s k

/-- The model mean of the logarithm of the count. -/
def modelMean (s : ℝ) : ℝ :=
  (∑' k : ℕ, log ((k : ℝ) + 1) * weight s k) / normalizer s

def empiricalMeanLog (x : Fin n → ℕ) : ℝ := (∑ i, log (x i : ℝ)) / n

def logLik (x : Fin n → ℕ) (s : ℝ) : ℝ := -s * empiricalMeanLog x - log (normalizer s)

private theorem weight_pos (s : ℝ) (k : ℕ) : 0 < weight s k := exp_pos _

private theorem base_pos (k : ℕ) : 0 < (k : ℝ) + 1 := by positivity
private theorem log_nonneg_base (k : ℕ) : 0 ≤ log ((k : ℝ) + 1) := log_nonneg (le_add_of_nonneg_left (Nat.cast_nonneg k))

private theorem weight_eq (s : ℝ) (k : ℕ) :
    weight s k = 1 / ((k : ℝ) + 1) ^ s := by
  rw [weight, rpow_def_of_pos (base_pos k), one_div, ← exp_neg]
  congr 1
  ring

private theorem weight_summable {s : ℝ} (hs : 1 < s) : Summable (weight s) := by
  exact ((summable_one_div_nat_add_rpow 1 s).2 hs).congr
    (fun k ↦ by rw [weight_eq, abs_of_pos (base_pos k)])

private theorem weight_anti (k : ℕ) : Antitone (fun s ↦ weight s k) := by
  intro a b hab
  exact exp_le_exp.mpr (mul_le_mul_of_nonneg_right (neg_le_neg hab) (log_nonneg_base k))

private theorem normalizer_gt_one {s : ℝ} (hs : 1 < s) : 1 < normalizer s := by
  rw [normalizer, (weight_summable hs).tsum_eq_zero_add]
  have hpos : Summable (fun k ↦ weight s (k + 1)) :=
    (weight_summable hs).comp_injective (i := Nat.succ) Nat.succ_injective
  have hle := hpos.sum_le_tsum {0} (fun k _ ↦ (exp_pos _).le)
  simp only [Finset.sum_singleton] at hle
  have hzero : weight s 0 = 1 := by simp [weight]
  rw [hzero]
  exact lt_add_of_pos_right _ ((exp_pos _).trans_le hle)

/-- The harmonic finite sums force divergence as the exponent decreases to one. -/
theorem normalizer_atOne : Tendsto normalizer (𝓝[>] 1) atTop := by
  refine tendsto_atTop.2 fun M ↦ ?_
  obtain ⟨K, hK⟩ :=
    (tendsto_atTop.1 tendsto_sum_range_one_div_nat_succ_atTop (M + 1)).exists
  have hc : Continuous (fun s : ℝ ↦ ∑ k ∈ Finset.range K, weight s k) :=
    continuous_finsetSum _ fun k _ ↦ continuous_exp.comp (continuous_neg.mul continuous_const)
  have hlt : M < ∑ k ∈ Finset.range K, weight 1 k := by
    simp only [weight_eq, rpow_one]
    exact lt_of_lt_of_le (by linarith) hK
  filter_upwards [self_mem_nhdsWithin (a := (1 : ℝ)) (s := Ioi 1),
    hc.continuousAt.continuousWithinAt (s := Ioi (1 : ℝ)) (Ioi_mem_nhds hlt)] with s hs h
  exact h.le.trans ((weight_summable hs).sum_le_tsum _ fun k _ ↦ (exp_pos _).le)

-- Spend a small part of the power-law exponent to dominate each log power.
private theorem moment_bound {a : ℝ} (ha : 1 < a) (m : ℕ) :
    ∃ u : ℕ → ℝ, Summable u ∧
      ∀ (k : ℕ) (s : ℝ), a ≤ s → log ((k : ℝ) + 1) ^ m * weight s k ≤ u k := by
  let ε := (a - 1) / (2 * (m + 1))
  have hε : 0 < ε := div_pos (sub_pos.mpr ha) (by positivity)
  have hq : 1 < a - ε * m := by
    have h := (div_mul_cancel₀ (a - 1) (show (2 * ((m : ℝ) + 1)) ≠ 0 by positivity))
    change ε * (2 * ((m : ℝ) + 1)) = a - 1 at h
    nlinarith
  refine ⟨fun k ↦ (ε ^ m)⁻¹ * weight (a - ε * m) k,
    (weight_summable hq).mul_left _, ?_⟩
  intro k s hs
  let b : ℝ := (k : ℝ) + 1
  have hb : 0 < b := base_pos k
  have hl : 0 ≤ log b := log_nonneg_base k
  have hlog : log b ≤ b ^ ε / ε := log_le_rpow_div hb.le hε
  calc
    log b ^ m * weight s k ≤ (b ^ ε / ε) ^ m * weight a k :=
      mul_le_mul (pow_le_pow_left₀ hl hlog _) (weight_anti k hs) (exp_pos _).le (by positivity)
    _ = (ε ^ m)⁻¹ * weight (a - ε * m) k := by
      rw [div_pow, rpow_def_of_pos hb, ← exp_nat_mul]
      simp only [weight]
      change exp ((m : ℝ) * (log b * ε)) / ε ^ m * exp (-a * log b) = _
      rw [div_eq_inv_mul, mul_assoc, ← exp_add]
      congr 2
      ring

private theorem hasDerivAt_moment (m : ℕ) {s : ℝ} (hs : 1 < s) :
    HasDerivAt (fun t ↦ ∑' k : ℕ, log ((k : ℝ) + 1) ^ m * weight t k)
      (∑' k : ℕ, -(log ((k : ℝ) + 1) ^ (m + 1) * weight s k)) s := by
  let a := (1 + s) / 2
  have ha : 1 < a := by dsimp [a]; linarith
  have has : a < s := by dsimp [a]; linarith
  obtain ⟨u, hu, hub⟩ := moment_bound ha (m + 1)
  obtain ⟨v, hv, hvb⟩ := moment_bound ha m
  refine hasDerivAt_tsum_of_isPreconnected (g := fun (k : ℕ) t ↦ log ((k : ℝ) + 1) ^ m * weight t k)
    (g' := fun (k : ℕ) t ↦ -(log ((k : ℝ) + 1) ^ (m + 1) * weight t k)) hu isOpen_Ioi
    (convex_Ioi a).isPreconnected ?_ ?_ (y₀ := s) has ?_ has
  · intro k t _
    have h := ((((hasDerivAt_id t).neg).mul_const (log ((k : ℝ) + 1))).exp).const_mul
      (log ((k : ℝ) + 1) ^ m)
    convert h using 1
    · rfl
    · simp [weight, pow_succ]
      ring
  · intro k t ht
    rw [Real.norm_eq_abs, abs_neg, abs_of_nonneg
      (mul_nonneg (pow_nonneg (log_nonneg_base k) _) (weight_pos t k).le)]
    exact hub k t ht.le
  · exact hv.of_nonneg_of_le (fun k ↦ mul_nonneg (pow_nonneg (log_nonneg_base k) _) (exp_pos _).le)
      (fun k ↦ hvb k s has.le)

private theorem moment_summable (m : ℕ) {s : ℝ} (hs : 1 < s) :
    Summable (fun k : ℕ ↦ log ((k : ℝ) + 1) ^ m * weight s k) := by
  obtain ⟨u, hu, hb⟩ := moment_bound hs m
  exact hu.of_nonneg_of_le (fun k ↦ mul_nonneg (pow_nonneg (log_nonneg_base k) _) (exp_pos _).le)
    (fun k ↦ hb k s le_rfl)

private theorem weight_limit (k : ℕ) :
    Tendsto (fun s ↦ weight s k) atTop (𝓝 (if k = 0 then 1 else 0)) := by
  by_cases hk : k = 0
  · subst k
    simp [weight]
  · have hp : 0 < log ((k : ℝ) + 1) := by
      apply log_pos
      have hk' : (0 : ℝ) < k := by exact_mod_cast Nat.pos_of_ne_zero hk
      linarith
    unfold weight
    simpa [hk, Function.comp_def, neg_mul] using
      tendsto_exp_neg_atTop_nhds_zero.comp (tendsto_id.atTop_mul_const hp)

/-- The normalizer tends to one as the exponent increases without bound. -/
theorem normalizer_atTop : Tendsto normalizer atTop (𝓝 1) := by
  unfold normalizer
  have h := tendsto_tsum_of_dominated_convergence (weight_summable (by norm_num : (1 : ℝ) < 2))
    weight_limit ((eventually_ge_atTop (2 : ℝ)).mono fun s hs k ↦ by
      rw [Real.norm_eq_abs, abs_of_pos (weight_pos s k)]
      exact weight_anti k hs)
  simpa [tsum_ite_eq] using h

private theorem hasDerivAt_negLogNormalizer {s : ℝ} (hs : 1 < s) :
    HasDerivAt (fun t ↦ -log (normalizer t)) (modelMean s) s := by
  have hz : HasDerivAt normalizer (-(∑' k : ℕ, log ((k : ℝ) + 1) * weight s k)) s := by
    unfold normalizer
    simpa [pow_one, tsum_neg] using hasDerivAt_moment 0 hs
  have h := (hz.log ((normalizer_gt_one hs).trans' zero_lt_one).ne').neg
  unfold modelMean
  simpa [Pi.neg_def, neg_div] using h

/-- The exponent derivative is the model log mean minus the empirical log mean. -/
theorem derivative (x : Fin n → ℕ) {s : ℝ} (hs : 1 < s) :
    HasDerivAt (logLik x) (modelMean s - empiricalMeanLog x) s := by
  have h := ((hasDerivAt_id s).neg.mul_const (empiricalMeanLog x)).add
    (hasDerivAt_negLogNormalizer hs)
  convert h using 1
  · funext t
    simp only [logLik, Pi.add_apply, id_eq, Pi.neg_apply]
    ring
  · ring

private def moment (m : ℕ) (s : ℝ) : ℝ :=
  ∑' k : ℕ, log ((k : ℝ) + 1) ^ m * weight s k

-- Counts one and two have different logarithms and both carry positive mass.
private theorem nonzero_variance {s : ℝ} (hs : 1 < s) :
    0 < moment 2 s - 2 * modelMean s * moment 1 s + modelMean s ^ 2 * normalizer s := by
  let f := fun k : ℕ ↦ (log ((k : ℝ) + 1) - modelMean s) ^ 2 * weight s k
  let g := fun k : ℕ ↦ log ((k : ℝ) + 1) ^ 2 * weight s k
    - (2 * modelMean s) * (log ((k : ℝ) + 1) ^ 1 * weight s k)
    + modelMean s ^ 2 * weight s k
  have he : f = g := by
    funext k
    dsimp [f, g]
    ring
  have h₂ := moment_summable 2 hs
  have h₁ := (moment_summable 1 hs).mul_left (2 * modelMean s)
  have h₀ := (weight_summable hs).mul_left (modelMean s ^ 2)
  have hg : Summable g := (h₂.sub h₁).add h₀
  have hf : Summable f := he ▸ hg
  have hge (k : ℕ) : 0 ≤ f k := mul_nonneg (sq_nonneg _) (exp_pos _).le
  obtain ⟨k, hk⟩ : ∃ k : ℕ, 0 < f k := by
    by_cases h : modelMean s = 0
    · exact ⟨1, by
        unfold f
        rw [h, sub_zero]
        exact mul_pos (sq_pos_of_pos (log_pos (by norm_num))) (exp_pos _)⟩
    · exact ⟨0, mul_pos (sq_pos_of_ne_zero (by simpa using neg_ne_zero.mpr h)) (exp_pos _)⟩
  have hpos := hk.trans_le (by
    simpa only [Finset.sum_singleton] using hf.sum_le_tsum {k} (fun j _ ↦ hge j))
  rw [he] at hpos
  have hEq : ∑' k, g k = moment 2 s - 2 * modelMean s * moment 1 s +
      modelMean s ^ 2 * normalizer s := by
    have hadd := (h₂.sub h₁).tsum_add h₀
    have hsub := h₂.tsum_sub h₁
    rw [show (∑' k, g k) = _ from hadd, hsub]
    simp [moment, normalizer, tsum_mul_left]
  rwa [hEq] at hpos

private theorem hasDerivAt_modelMean {s : ℝ} (hs : 1 < s) :
    HasDerivAt modelMean
      ((-moment 2 s * normalizer s + moment 1 s ^ 2) / normalizer s ^ 2) s := by
  have hnz := ((normalizer_gt_one hs).trans' zero_lt_one).ne'
  have h₀ : HasDerivAt normalizer (-moment 1 s) s := by
    unfold normalizer moment
    simpa [tsum_neg] using hasDerivAt_moment 0 hs
  have h₁ : HasDerivAt (moment 1) (-moment 2 s) s := by
    unfold moment
    simpa [tsum_neg] using hasDerivAt_moment 1 hs
  convert h₁.div h₀ hnz using 1
  · unfold modelMean
    simp only [moment, pow_one, Pi.div_def]
  · ring

/-- The model log mean, hence the exponent score, is strictly decreasing. -/
theorem modelMean_strictAnti : StrictAntiOn modelMean (Ioi 1) := by
  apply strictAntiOn_of_deriv_neg (convex_Ioi 1)
    (fun s hs ↦ (hasDerivAt_modelMean hs).continuousAt.continuousWithinAt)
  intro s hs
  have hs' := interior_subset hs
  have hz : 0 < normalizer s := (normalizer_gt_one hs').trans' zero_lt_one
  have he : modelMean s * normalizer s = moment 1 s := by
    simp [modelMean, moment, hz.ne']
  have h := mul_pos (nonzero_variance hs') hz
  have hm : 0 < moment 2 s * normalizer s - moment 1 s ^ 2 := by
    convert h using 1
    calc
      moment 2 s * normalizer s - moment 1 s ^ 2 =
          moment 2 s * normalizer s - (modelMean s * normalizer s) ^ 2 := by rw [he]
      _ = (moment 2 s - 2 * modelMean s * moment 1 s +
          modelMean s ^ 2 * normalizer s) * normalizer s := by rw [← he]; ring
  rw [(hasDerivAt_modelMean hs').deriv]
  exact div_neg_of_neg_of_pos (by linarith) (sq_pos_of_pos hz)

private theorem empirical_pos_iff (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i) :
    0 < empiricalMeanLog x ↔ ∃ i, 1 < x i := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  rw [empiricalMeanLog, lt_div_iff₀ hnR, zero_mul,
    sum_nonneg_pos_iff (fun i ↦ log_nonneg (by exact_mod_cast (show 1 ≤ x i from hx i)))]
  exact exists_congr fun i ↦ (log_pos_iff (Nat.cast_nonneg (x i))).trans (by exact_mod_cast Iff.rfl)

/-- The model log mean tends to zero at infinite exponent. -/
theorem modelMean_atTop : Tendsto modelMean atTop (𝓝 0) := by
  have hs : (1 : ℝ) < 2 := by norm_num
  have hsum : Summable (fun k : ℕ ↦ log ((k : ℝ) + 1) * weight 2 k) := by
    simpa only [pow_one] using moment_summable 1 hs
  have ht (k : ℕ) : Tendsto (fun s ↦ log ((k : ℝ) + 1) * weight s k) atTop (𝓝 0) := by
    by_cases hk : k = 0
    · subst k
      simp
    · simpa [hk] using (weight_limit k).const_mul (log ((k : ℝ) + 1))
  have h := tendsto_tsum_of_dominated_convergence hsum ht
    ((eventually_ge_atTop (2 : ℝ)).mono fun s hs k ↦ by
      rw [Real.norm_eq_abs, abs_of_nonneg (mul_nonneg (log_nonneg_base k) (weight_pos s k).le)]
      exact mul_le_mul_of_nonneg_left (weight_anti k hs) (log_nonneg_base k))
  unfold modelMean
  simpa [Pi.div_def] using h.div normalizer_atTop one_ne_zero

/-- The model log mean diverges as the exponent decreases to one. -/
theorem modelMean_atOne : Tendsto modelMean (𝓝[>] 1) atTop := by
  let f : ℝ → ℝ := fun s ↦ -log (normalizer s)
  have hd (s : ℝ) (hs : s ∈ Ioi (1 : ℝ)) : HasDerivAt f (modelMean s) s :=
    hasDerivAt_negLogNormalizer hs
  have hf : ContinuousOn f (Ioi 1) := fun s hs ↦ (hd s hs).continuousAt.continuousWithinAt
  have hL : Tendsto f (𝓝[>] 1) atBot :=
    tendsto_neg_atTop_atBot.comp (tendsto_log_atTop.comp normalizer_atOne)
  refine tendsto_atTop.2 fun M ↦ ?_
  -- A steep secant to a fixed interior point produces an arbitrarily large derivative.
  let B := max M 0
  obtain ⟨a, ha, ha2, hfa⟩ :=
    ((show ∀ᶠ s in 𝓝[>] (1 : ℝ), s ∈ Ioi (1 : ℝ) from self_mem_nhdsWithin).and
      ((show ∀ᶠ s : ℝ in 𝓝[>] 1, s < 2 from
        mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds (by norm_num))).and
        (hL.eventually (eventually_lt_atBot (f 2 - B))))).exists
  have hs : Icc a 2 ⊆ Ioi (1 : ℝ) := fun _ ht ↦ ha.trans_le ht.1
  obtain ⟨u, hu, hdu⟩ := exists_deriv_eq_slope f ha2 (hf.mono hs)
    (fun t ht ↦ (hd t (hs (Ioo_subset_Icc_self ht))).differentiableAt.differentiableWithinAt)
  have hu' := hs (Ioo_subset_Icc_self hu)
  rw [(hd u hu').deriv] at hdu
  have hBu : B < modelMean u := by
    rw [hdu, lt_div_iff₀ (sub_pos.mpr ha2)]
    have hB : 0 ≤ B := le_max_right _ _
    have ha' : 1 < a := ha
    nlinarith [mul_nonneg hB (sub_nonneg.mpr ha'.le)]
  filter_upwards [self_mem_nhdsWithin (a := (1 : ℝ)) (s := Ioi 1),
    mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds (show 1 < u from hu'))] with s h₁ h₂
  exact (le_max_left M 0).trans ((hBu.trans (modelMean_strictAnti h₁ hu' h₂)).le)

/-- With positive empirical log mean, the exact zeta likelihood has one maximizing score root. -/
theorem mle {x : Fin n → ℕ} (hx : 0 < empiricalMeanLog x) :
    (∃! s, 1 < s ∧ modelMean s = empiricalMeanLog x) ∧
      ∀ s > 1, IsMaxOn (logLik x) (Ioi 1) s ↔ modelMean s = empiricalMeanLog x := by
  obtain ⟨a, ha, hsa⟩ :=
    ((show ∀ᶠ s in 𝓝[>] (1 : ℝ), s ∈ Ioi (1 : ℝ) from self_mem_nhdsWithin).and
      ((tendsto_atTop.1 modelMean_atOne (empiricalMeanLog x + 1)))).exists
  obtain ⟨b, hb, hsb⟩ := ((eventually_gt_atTop (1 : ℝ)).and
    (modelMean_atTop.eventually (Iio_mem_nhds hx))).exists
  have hpos : 0 < modelMean a - empiricalMeanLog x := by linarith
  have hneg : modelMean b - empiricalMeanLog x < 0 := sub_neg.mpr hsb
  have hab : a ≤ b := le_of_not_gt fun h ↦
    (modelMean_strictAnti hb ha h).not_gt (hsb.trans (sub_pos.mp hpos))
  simpa only [mem_Ioi, sub_eq_zero] using
    Generic.existsUnique_score_zero_of_signs (convex_Ioi 1) (fun _ hs ↦ derivative x hs)
      (fun _ hs ↦ ((hasDerivAt_modelMean hs).sub_const _).continuousAt.continuousWithinAt)
      (fun _ h₁ _ h₂ h ↦ sub_lt_sub_right (modelMean_strictAnti h₁ h₂ h) _)
      ha hb hab hpos hneg

/-- An all-one sample approaches log likelihood zero only in the infinite-exponent limit. -/
theorem allOnes_boundary {x : Fin n → ℕ} (hx : ∀ i, x i = 1) :
    Tendsto (logLik x) atTop (𝓝 0) ∧ sSup (logLik x '' Ioi 1) = 0 ∧
      ¬ ∃ s ∈ Ioi 1, IsMaxOn (logLik x) (Ioi 1) s := by
  have he : empiricalMeanLog x = 0 := by simp [empiricalMeanLog, hx]
  have hl : Tendsto (logLik x) atTop (𝓝 0) := by
    unfold logLik
    simpa [he, Function.comp_def, Pi.neg_def] using
      ((continuousAt_log one_ne_zero).tendsto.comp normalizer_atTop).neg
  exact ⟨hl, boundary_sup (eventually_gt_atTop (1 : ℝ)) hl fun s hs ↦ by
    simpa [logLik, he] using log_pos (normalizer_gt_one hs)⟩

/-- The finite MLE exists and is unique exactly when the empirical log mean is positive. -/
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i) :
    (∃! s, s ∈ Ioi (1 : ℝ) ∧ IsMaxOn (logLik x) (Ioi 1) s) ↔ 0 < empiricalMeanLog x := by
  by_cases h : 0 < empiricalMeanLog x
  · obtain ⟨⟨s, hs, hu⟩, hm⟩ := mle h
    exact iff_of_true ⟨s, ⟨hs.1, (hm s hs.1).2 hs.2⟩,
      fun t ht ↦ hu t ⟨ht.1, (hm t ht.1).1 ht.2⟩⟩ h
  · have he := not_exists.mp (mt (empirical_pos_iff hn hx).2 h)
    have ha : ∀ i, x i = 1 := fun i ↦ by have := hx i; have := he i; omega
    exact iff_of_false (fun ⟨s, hs, _⟩ ↦ (allOnes_boundary ha).2.2 ⟨s, hs⟩) h

end Zeta
end UnivariateML
