module

public import UnivariateML.Family.FinishHalfT

import Mathlib.Tactic

set_option linter.style.header false

/-! # Half-Student held endpoint, shape, and scale -/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.StudentHalf

noncomputable section

open SkewLSHalfT

private theorem endpoint_strict {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hm : ∀ j, x i ≤ x j) {μ ν s : ℝ} (hμ : μ < x i)
    (hν : 0 < ν) (hs : 0 < s) :
    likelihood x μ ν s < likelihood x (x i) ν s := by
  unfold likelihood TDist.likelihood
  apply mul_lt_mul_of_pos_left _ (by positivity)
  apply exp_lt_exp.mpr
  unfold TDist.logLik
  have hn : Nonempty (Fin n) := ⟨i⟩
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro j _
  have h₁ : 0 ≤ x j-x i := sub_nonneg.mpr (hm j)
  have h₂ : 0 < x j-μ := sub_pos.mpr (hμ.trans_le (hm j))
  have hsq := (sq_lt_sq₀ h₁ h₂.le).mpr (show x j-x i < x j-μ by linarith)
  have hd : 0 < ν*s^2 := by positivity
  have hlog := log_lt_log (show 0 < 1+(x j-x i)^2/(ν*s^2) by positivity)
    (add_lt_add_right ((div_lt_div_iff_of_pos_right hd).mpr hsq) 1)
  exact sub_lt_sub_left (mul_lt_mul_of_pos_left hlog (by positivity)) _

/-- With shape and scale held positive, the sample minimum is the unique
maximizing feasible endpoint, including for constant samples. -/
theorem held_shape_scale_endpoint {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    ∃ i : Fin n, (∀ j, x i ≤ x j) ∧
      ∀ μ, ((∀ j, μ ≤ x j) ∧ IsMaxOn (fun m ↦ likelihood x m ν s) {m | ∀ j, m ≤ x j} μ) ↔
        μ = x i := by
  obtain ⟨i,_,hi⟩ := Finset.exists_min_image Finset.univ x
    ⟨⟨0,hn⟩, Finset.mem_univ _⟩
  have hm (j : Fin n) := hi j (Finset.mem_univ j)
  refine ⟨i,hm,fun μ ↦ ?_⟩
  constructor
  · rintro ⟨hμ, hmax⟩
    rcases lt_or_eq_of_le (hμ i) with h | h
    · exact ((endpoint_strict x hm h hν hs).not_ge (hmax hm)).elim
    · exact h
  · rintro rfl
    refine ⟨hm, fun m (h : ∀ j, m ≤ x j) ↦ ?_⟩
    rcases lt_or_eq_of_le (h i) with hlt | rfl
    · exact (endpoint_strict x hm hlt hν hs).le
    · exact le_rfl

/-- With held positive shape and scale, the likelihood vanishes as the
endpoint tends to minus infinity. The support endpoint itself is feasible. -/
theorem held_shape_scale_escape {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    Tendsto (fun m ↦ likelihood x m ν s) atBot (𝓝 0) ∧
      ∃ i : Fin n, x i ∈ {m | ∀ j, m ≤ x j} ∧ 0 < likelihood x (x i) ν s := by
  have ht (i : Fin n) : Tendsto (fun μ ↦ TDist.density μ ν s (x i)) atBot (𝓝 0) := by
    have hr : Tendsto (fun μ : ℝ ↦ x i - μ) atBot atTop :=
      tendsto_atTop_add_const_left _ _ tendsto_neg_atBot_atTop
    have ht' := (tendsto_pow_atTop (by decide : (2 : ℕ) ≠ 0)).comp hr
    have hl : Tendsto (fun μ : ℝ ↦ log (1 + (x i - μ)^2/(ν*s^2))) atBot atTop :=
      tendsto_log_atTop.comp
        (tendsto_atTop_add_const_left _ _ (ht'.atTop_div_const (by positivity)))
    have hh := tendsto_neg_atTop_atBot.comp (hl.const_mul_atTop (by positivity : 0 < (ν+1)/2))
    exact tendsto_exp_atBot.comp (tendsto_atBot_add_const_left _
      (TDist.logNormalizer ν - log s) hh)
  have hprod := (tendsto_finsetProd Finset.univ fun i _ ↦ ht i).const_mul ((2 : ℝ)^n)
  have hzero : (∏ _ : Fin n, (0 : ℝ)) = 0 := by simp [zero_pow hn.ne']
  rw [hzero, mul_zero] at hprod
  refine ⟨hprod.congr (fun μ ↦ ?_), ?_⟩
  · rw [likelihood, TDist.likelihood, TDist.logLik, exp_sum]
    rfl
  · obtain ⟨i,hm,_⟩ := held_shape_scale_endpoint x hn hν hs
    exact ⟨i,hm, mul_pos (by positivity) (exp_pos _)⟩

/-- At a compatible held endpoint and held positive shape, attainment
is equivalent to a positive collapse exponent. -/
theorem held_endpoint_shape_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {ν : ℝ} (hν : 0 < ν) :
    (∃ s ∈ Ioi 0, IsMaxOn (likelihood x μ ν) (Ioi 0) s) ↔
      0 < TDist.exponent x μ ν := by
  have hcompare (a s : ℝ) :
      likelihood x μ ν a ≤ likelihood x μ ν s ↔ logLik x μ ν a ≤ logLik x μ ν s := by
    have h : 0 < (2 : ℝ)^n := by positivity
    simp [likelihood, TDist.likelihood, logLik, mul_le_mul_iff_right₀ h]
  constructor
  · rintro ⟨s,hs,hm⟩
    by_contra! he
    rcases lt_or_eq_of_le he with hneg | heq
    · have hpos : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
      obtain ⟨a,ha,hgt⟩ := (hpos.and
        ((held_endpoint_collapse x μ hν hneg).eventually_gt_atTop (likelihood x μ ν s))).exists
      exact hgt.not_ge (hm ha)
    · obtain ⟨_,h,hstrict⟩ := held_endpoint_equality hn x μ hν heq
      exact (hstrict s hs).not_ge (h.2 (show likelihood x μ ν s ∈ upperBounds
        (likelihood x μ ν '' Ioi 0) from by rintro _ ⟨a,ha,rfl⟩; exact hm ha))
  · intro he
    obtain ⟨s,⟨hs,hm⟩,_⟩ := (held_endpoint hn x μ hν).mpr he
    exact ⟨s,hs,fun a ha ↦ (hcompare a s).mpr (hm ha)⟩

/-- The fixed-endpoint, fixed-shape failure branches have exact scale paths.
At equality the positive limit is the unattained supremum. -/
theorem held_endpoint_shape_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {ν : ℝ} (hν : 0 < ν) :
    (TDist.exponent x μ ν < 0 → Tendsto (likelihood x μ ν) (𝓝[>] 0) atTop) ∧
    (TDist.exponent x μ ν = 0 →
      Tendsto (likelihood x μ ν) (𝓝[>] 0) (𝓝 (collapseValue x μ ν)) ∧
      IsLUB (likelihood x μ ν '' Ioi 0) (collapseValue x μ ν) ∧
      ∀ a > 0, likelihood x μ ν a < collapseValue x μ ν) :=
  ⟨held_endpoint_collapse x μ hν, held_endpoint_equality hn x μ hν⟩

/-- With shape held and endpoint free, the scale-zero path and the global
supremum are determined by the exponent at the minimum. -/
theorem held_shape_collapse {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) {ν : ℝ} (hν : 0 < ν) :
    (TDist.exponent x (x i) ν < 0 → Tendsto (likelihood x (x i) ν) (𝓝[>] 0) atTop) ∧
    (TDist.exponent x (x i) ν = 0 →
      Tendsto (likelihood x (x i) ν) (𝓝[>] 0) (𝓝 (collapseValue x (x i) ν)) ∧
      IsLUB ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) ''
        {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2}) (collapseValue x (x i) ν) ∧
      ∀ m a : ℝ, (∀ j, m ≤ x j) → 0 < a → likelihood x m ν a < collapseValue x (x i) ν) := by
  refine ⟨held_endpoint_collapse x (x i) hν, fun he ↦ ?_⟩
  exact ⟨(held_endpoint_equality (Nat.zero_lt_of_lt i.isLt) x (x i) hν he).1,
    held_shape_equality x hν hmin he⟩

/-- The all-free shape path at a fixed feasible endpoint and scale stays
in the fitted domain eventually and tends to the matched half-normal. -/
theorem half_normal_feasible_path {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hμ : ∀ j, μ ≤ x j) {s : ℝ} (hs : 0 < s) :
    Tendsto (fun ν ↦ likelihood x μ ν s) atTop
      (𝓝 (∏ i, 2*SkewLSGaussian.φ ((x i-μ)/s)/s)) ∧
      ∀ᶠ ν in atTop, (μ,ν,s) ∈ FinishHalfT.domain x := by
  refine ⟨half_normal_path x μ hs, ?_⟩
  filter_upwards [eventually_ge_atTop (1 : ℝ)] with ν hν
  exact ⟨hμ,hν,hs⟩

/-- With shape held and endpoint free, a positive-scale maximum exists
exactly when the exponent at the sample minimum is strictly positive. -/
theorem held_shape_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n} (hmin : ∀ j, x i ≤ x j)
    {ν : ℝ} (hν : 0 < ν) :
    let D : Set (ℝ × ℝ) := {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2}
    (∃ p ∈ D, IsMaxOn (fun p ↦ likelihood x p.1 ν p.2) D p) ↔
      0 < TDist.exponent x (x i) ν := by
  dsimp
  have hpath := held_shape_collapse x hmin hν
  constructor
  · rintro ⟨p,hp,hm⟩
    by_contra! he
    rcases lt_or_eq_of_le he with hneg | heq
    · have hpos : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
      obtain ⟨a,ha,hgt⟩ := (hpos.and
        ((hpath.1 hneg).eventually_gt_atTop (likelihood x p.1 ν p.2))).exists
      exact hgt.not_ge (hm (show (x i,a) ∈ {q : ℝ × ℝ | (∀ j, q.1 ≤ x j) ∧ 0 < q.2}
        from ⟨hmin,ha⟩))
    · obtain ⟨_,h,hstrict⟩ := hpath.2 heq
      exact (hstrict p.1 p.2 hp.1 hp.2).not_ge (h.2 (show likelihood x p.1 ν p.2 ∈ upperBounds
        ((fun q : ℝ × ℝ ↦ likelihood x q.1 ν q.2) '' {q | (∀ j, q.1 ≤ x j) ∧ 0 < q.2})
        from by rintro _ ⟨q,hq,rfl⟩; exact hm hq))
  · intro he
    obtain ⟨p,⟨hp,hm⟩,_⟩ := (SkewLSHalfT.held_shape x hν hmin).mpr he
    refine ⟨p,hp,fun q hq ↦ ?_⟩
    have h := hm hq
    change TDist.logLik x q.1 ν q.2 + n * log 2 ≤ TDist.logLik x p.1 ν p.2 + n * log 2 at h
    exact mul_le_mul_of_nonneg_left (exp_le_exp.mpr (le_of_add_le_add_right h)) (by positivity)

/-- At a held positive shape, the compatible joint likelihood is
unbounded exactly when the minimum has negative collapse exponent. -/
theorem held_shape_unbounded_iff {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) {ν : ℝ} (hν : 0 < ν) :
    (¬ BddAbove ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2})) ↔ TDist.exponent x (x i) ν < 0 := by
  have hpath := held_shape_collapse x hmin hν
  constructor
  · intro hb
    by_contra! he
    rcases lt_or_eq_of_le he with h | h
    · obtain ⟨p,_,hm⟩ := (held_shape_criterion x hmin hν).mpr h
      refine hb ⟨likelihood x p.1 ν p.2, ?_⟩
      rintro _ ⟨q,hq,rfl⟩
      exact hm hq
    · exact hb ⟨_, (hpath.2 h.symm).2.1.1⟩
  · intro he
    rintro ⟨b,hb⟩
    have hpos : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
    obtain ⟨a,ha,hgt⟩ := (hpos.and ((hpath.1 he).eventually_gt_atTop b)).exists
    exact hgt.not_ge (hb ⟨(x i,a),⟨hmin,ha⟩,rfl⟩)

/-- A strict majority at the minimum supplies a vertical divergent path
through the feasible shape floor of the all-free model. -/
theorem all_free_majority_path {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) (hm : n < 2 * TDist.multiplicity x (x i)) :
    Tendsto (likelihood x (x i) 1) (𝓝[>] 0) atTop ∧
      ∀ᶠ a in 𝓝[>] (0 : ℝ), (x i, (1 : ℝ), a) ∈ FinishHalfT.domain x := by
  have hm' : (n : ℝ) < 2 * TDist.multiplicity x (x i) := by exact_mod_cast hm
  refine ⟨held_endpoint_collapse x (x i) one_pos ?_, ?_⟩
  · unfold TDist.exponent
    linarith
  · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with a ha
    exact ⟨hmin,le_rfl,ha⟩

end
end UnivariateML.StudentHalf
end
