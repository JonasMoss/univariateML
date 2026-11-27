module

public import UnivariateML.Family.AllFreeBetaPrime
public import UnivariateML.Generic.Endpoint3GammaRatio
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Beta-prime gamma faces with varying shape and scale

## Deviations

The held-shape results classify maximizing escape sequences and give
an exact finite point comparison, including equality. Finite stationary
lists and exhaustion with both shapes free remain separate.
-/

@[expose] public section

open Real Set Filter Topology
open scoped BigOperators Topology

namespace UnivariateML.Endpoint3BetaPrime

variable {n : ℕ}

private theorem log_ratio_at_zero :
    Tendsto (fun t : ℝ ↦ log (1 + t) / t) (𝓝[≠] 0) (𝓝 1) := by
  have h : HasDerivAt (fun t : ℝ ↦ log (1 + t)) 1 0 := by
    simpa using ((hasDerivAt_id (0 : ℝ)).const_add 1).log
      (by simp : 1 + id (0 : ℝ) ≠ 0)
  simpa [smul_eq_mul, div_eq_mul_inv, mul_comm] using h.tendsto_slope_zero

/-- With first shape held, every path with diverging second shape and
second shape divided by scale converging to a positive rate has the
corresponding gamma likelihood limit. -/
theorem gamma_of_shape_rate_limit {ι : Type*} {l : Filter ι} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p r : ℝ} (hp : 0 < p) (hr : 0 < r)
    {q s : ι → ℝ} (hq : Tendsto q l atTop) (hs : ∀ᶠ j in l, 0 < s j)
    (hrate : Tendsto (fun j ↦ q j / s j) l (𝓝 r)) :
    Tendsto (fun j ↦ BetaPrime.logLik x (s j) (p, q j)) l
      (𝓝 (∑ i, Gamma.logDensity p r (x i))) := by
  have hqpos := hq.eventually (eventually_gt_atTop (0 : ℝ))
  have hsi : Tendsto (fun j ↦ (s j)⁻¹) l (𝓝 0) := by
    have H := hrate.mul (tendsto_inv_atTop_zero.comp hq)
    rw [mul_zero] at H
    apply H.congr'
    filter_upwards [hqpos] with j hj
    change q j / s j * (q j)⁻¹ = (s j)⁻¹
    rw [div_eq_mul_inv, mul_right_comm, mul_inv_cancel₀ hj.ne', one_mul]
  have hlog := (continuousAt_log hr.ne').tendsto.comp hrate
  have hΓ := (Endpoint3GammaRatio.log_gamma_ratio p hp).comp hq
  unfold BetaPrime.logLik
  apply tendsto_finsetSum
  intro i _
  let t := fun j ↦ (s j)⁻¹ * x i
  have ht : Tendsto t l (𝓝 0) := by simpa [t] using hsi.mul_const (x i)
  have htn : Tendsto t l (𝓝[≠] 0) :=
    tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within _ ht <|
      hs.mono fun j hj ↦ (mul_pos (inv_pos.mpr hj) (hx i)).ne'
  have hn := ((hrate.add (hsi.const_mul p)).mul_const (x i))
  simp only [mul_zero, add_zero] at hn
  have hterm := hn.mul (log_ratio_at_zero.comp htn)
  let C := (p - 1) * log (x i) - log (Real.Gamma p)
  have hlim := ((hlog.const_mul p).const_add C).add hΓ |>.sub hterm
  simp only [mul_one, add_zero] at hlim
  rw [show C + p * log r - r * x i = Gamma.logDensity p r (x i) by
    unfold C Gamma.logDensity
    ring] at hlim
  apply hlim.congr'
  filter_upwards [hqpos, hs] with j hj hj'
  have htj : 0 < t j := mul_pos (inv_pos.mpr hj') (hx i)
  change C + p * log (q j / s j) +
      (log (Real.Gamma (q j + p)) - log (Real.Gamma (q j)) - p * log (q j)) -
      ((q j / s j + p * (s j)⁻¹) * x i) * (log (1 + t j) / t j) =
      (p - 1) * log (x i / s j) - log (s j) -
        (p + q j) * log (1 + x i / s j) - Beta.logBeta (p, q j)
  have hratio : t j = x i / s j := by simp [t, div_eq_mul_inv, mul_comm]
  have hfactor : (q j / s j + p * (s j)⁻¹) * x i = (p + q j) * t j := by
    rw [hratio]
    ring
  have he : (p + q j) * t j * (log (1 + t j) / t j) =
      (p + q j) * log (1 + t j) := by
    field_simp [htj.ne']
  rw [hfactor, he, hratio, log_div hj.ne' hj'.ne', log_div (hx i).ne' hj'.ne']
  unfold Beta.logBeta Beta.normalizer
  dsimp only [Prod.fst, Prod.snd]
  rw [log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hj)).ne'
      (Gamma_pos_of_pos (add_pos hp hj)).ne',
    log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hj).ne', add_comm p (q j)]
  dsimp [C]
  ring

/-- With second shape held, the reciprocal path with diverging first
shape and first shape times scale tending to a positive rate gives
the inverse-gamma likelihood, including the data Jacobian. -/
theorem inverse_gamma_of_shape_rate_limit {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {q r : ℝ} (hq : 0 < q) (hr : 0 < r)
    {p s : ι → ℝ} (hp : Tendsto p l atTop) (hs : ∀ᶠ j in l, 0 < s j)
    (hrate : Tendsto (fun j ↦ p j * s j) l (𝓝 r)) :
    Tendsto (fun j ↦ BetaPrime.logLik x (s j) (p j, q)) l
      (𝓝 ((∑ i, Gamma.logDensity q r (x i)⁻¹) - ∑ i, 2 * log (x i))) := by
  have H := gamma_of_shape_rate_limit (x := fun i ↦ (x i)⁻¹)
    (fun i ↦ inv_pos.mpr (hx i)) hq hr hp
    (hs.mono fun _ hj ↦ inv_pos.mpr hj)
    (by simpa only [div_inv_eq_mul] using hrate)
  apply (H.sub_const (∑ i, 2 * log (x i))).congr'
  filter_upwards [hs] with j hj
  exact (AllFreeBetaPrime.reciprocal_logLik hx hj _ _).symm

private theorem logBeta_formula {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    Beta.logBeta (p, q) =
      log (Real.Gamma p) + log (Real.Gamma q) - log (Real.Gamma (p + q)) := by
  unfold Beta.logBeta Beta.normalizer
  dsimp only [Prod.fst, Prod.snd]
  rw [log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hq)).ne'
      (Gamma_pos_of_pos (add_pos hp hq)).ne',
    log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hq).ne']

private theorem ratio_upper_bound {p ε : ℝ} (hp : 0 < p) (hε : 0 < ε) :
    ∃ U, ∀ q ≥ ε,
      log (Real.Gamma (p + q)) - log (Real.Gamma q) - p * log q ≤ U := by
  let F := fun q : ℝ ↦ log (Real.Gamma (p + q)) - log (Real.Gamma q) - p * log q
  have htop : Tendsto F atTop (𝓝 0) := by
    simpa only [F, add_comm p] using Endpoint3GammaRatio.log_gamma_ratio p hp
  obtain ⟨B, hB⟩ := eventually_atTop.mp
    (htop.eventually (Iio_mem_nhds (show (0 : ℝ) < 1 from one_pos)))
  let K := Icc ε (max ε B)
  have hc : ContinuousOn F K := by
    intro q hq
    have hq₀ := hε.trans_le hq.1
    have hG {u : ℝ} (hu : 0 < u) : ContinuousAt (fun t ↦ log (Real.Gamma t)) u :=
      ((Real.differentiableAt_Gamma (fun m ↦ by
        have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
        linarith)).continuousAt).log (Gamma_pos_of_pos hu).ne'
    exact ((((hG (add_pos hp hq₀)).comp'
      (continuousAt_const.add continuousAt_id)).sub (hG hq₀)).sub
        (continuousAt_const.mul (continuousAt_log hq₀.ne'))).continuousWithinAt
  obtain ⟨q, _, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨ε, le_rfl, le_max_left _ _⟩ hc
  refine ⟨max 1 (F q), fun u hu ↦ ?_⟩
  by_cases h : u ≤ max ε B
  · exact (hm ⟨hu, h⟩).trans (le_max_right _ _)
  · exact (hB u ((le_max_right _ _).trans (le_of_not_ge h))).le.trans (le_max_left _ _)

/-- At any fixed finite likelihood level for a held first shape, the
second shape is bounded below and its ratio to scale lies in a compact
positive interval. This holds for every nonempty positive sample. -/
theorem held_first_superlevel_bounds (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) (M : ℝ) :
    ∃ ε > 0, ∃ lo > 0, ∃ hi > 0, ∀ q > 0, ∀ s > 0,
      M ≤ BetaPrime.logLik x s (p, q) → ε ≤ q ∧ q / s ∈ Icc lo hi := by
  obtain ⟨j, _, hj⟩ := Finset.exists_min_image Finset.univ x
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  let y := x j
  have hy : 0 < y := hx j
  let N : ℝ := n
  have hN : 0 < N := Nat.cast_pos.mpr hn
  let C := ∑ i, -log (x i)
  let G := fun q : ℝ ↦ log (Real.Gamma (p + q)) - log (Real.Gamma q) -
    log (Real.Gamma p)
  have hzero : Tendsto G (𝓝[>] 0) atBot := by
    apply (tendsto_log_nhdsGT_zero.atBot_add
      (Endpoint3GammaRatio.log_gamma_ratio_at_zero hp)).congr
    intro q
    dsimp [G]
    ring
  obtain ⟨ε, hε, hsmall⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp
    (hzero.eventually_lt_atBot ((M - C) / N))
  have hshape (q s : ℝ) (hq : 0 < q) (hs : 0 < s) :
      BetaPrime.logLik x s (p, q) ≤ C + N * G q := by
    have hf (i : Fin n) :
        (p - 1) * log (x i / s) - log s -
          (p + q) * log (1 + x i / s) - Beta.logBeta (p, q) ≤
            -log (x i) + G q := by
      have hu := div_pos (hx i) hs
      have hm := log_le_log hu (le_add_of_nonneg_left zero_le_one)
      have hpos : 0 ≤ log (1 + x i / s) := log_nonneg (le_add_of_nonneg_right hu.le)
      rw [log_div (hx i).ne' hs.ne'] at hm ⊢
      rw [logBeta_formula hp hq]
      dsimp [G]
      nlinarith [mul_le_mul_of_nonneg_left hm hp.le, mul_nonneg hq.le hpos]
    simpa only [BetaPrime.logLik, Finset.sum_add_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, C, N] using
      Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ hf i)
  obtain ⟨U, hU⟩ := ratio_upper_bound hp hε
  let Q := p + ε
  have hQ : 0 < Q := add_pos hp hε
  let V := U - log (Real.Gamma p) + (∑ i, (p - 1) * log (x i)) / N
  let Z := M / N - V
  let lo := exp (Z / p)
  let hi := exp ((-Z - Q * log (y / Q)) / ε)
  refine ⟨ε, hε, lo, exp_pos _, hi, exp_pos _, ?_⟩
  intro q hq s hs hM
  have he : ε ≤ q := by
    by_contra! hh
    have ht := (lt_div_iff₀ hN).mp (hsmall ⟨hq, hh⟩)
    linarith [hM.trans (hshape q s hq hs)]
  let r := q / s
  have hr : 0 < r := div_pos hq hs
  have hlog (i : Fin n) :
      Q * log (1 + r * y / Q) ≤ (p + q) * log (1 + x i / s) := by
    have hratio : r * y / q = y / s := by
      dsimp [r]
      field_simp
    have hbase : log (1 + y / s) ≤ log (1 + x i / s) :=
      log_le_log (by positivity) (add_le_add_right
        (div_le_div_of_nonneg_right (hj i (Finset.mem_univ _)) hs.le) 1)
    have hpos : 0 ≤ log (1 + y / s) :=
      log_nonneg (le_add_of_nonneg_right (div_nonneg hy.le hs.le))
    rcases le_total q Q with h | h
    · have hle : r * y / Q ≤ y / s := hratio ▸
        div_le_div_of_nonneg_left (mul_nonneg hr.le hy.le) hq h
      have hlog := log_le_log (show 0 < 1 + r * y / Q by positivity)
        (add_le_add_right hle 1)
      exact (mul_le_mul_of_nonneg_left hlog hQ.le).trans
        ((mul_le_mul_of_nonneg_right (add_le_add_right he p) hpos).trans
          (mul_le_mul_of_nonneg_left hbase (add_pos hp hq).le))
    · have hb := one_add_mul_self_le_rpow_one_add
        (show -1 ≤ r * y / q by linarith [div_nonneg (mul_nonneg hr.le hy.le) hq.le])
        ((one_le_div hQ).mpr h)
      have hEq : (q / Q) * (r * y / q) = r * y / Q := by field_simp
      rw [hEq, hratio] at hb
      have hl := log_le_log (show 0 < 1 + r * y / Q by positivity) hb
      rw [log_rpow (by positivity : 0 < 1 + y / s)] at hl
      have hm := mul_le_mul_of_nonneg_left hl hQ.le
      have hx' : Q * (q / Q * log (1 + y / s)) = q * log (1 + y / s) := by
        field_simp
      rw [hx'] at hm
      exact hm.trans ((mul_le_mul_of_nonneg_right (le_add_of_nonneg_left hp.le) hpos).trans
        (mul_le_mul_of_nonneg_left hbase (add_pos hp hq).le))
  have hbound_i (i : Fin n) :
      (p - 1) * log (x i / s) - log s -
        (p + q) * log (1 + x i / s) - Beta.logBeta (p, q) ≤
        (p - 1) * log (x i) + U - log (Real.Gamma p) + p * log r -
          Q * log (1 + r * y / Q) := by
    rw [logBeta_formula hp hq, log_div (hx i).ne' hs.ne',
      show log r = log q - log s from log_div hq.ne' hs.ne']
    linarith [hU q he, hlog i]
  have hbound := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ hbound_i i)
  rw [show (∑ i, ((p - 1) * log (x i / s) - log s -
    (p + q) * log (1 + x i / s) - Beta.logBeta (p, q))) = BetaPrime.logLik x s (p, q)
    from rfl] at hbound
  simp only [Finset.sum_add_distrib, Finset.sum_sub_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at hbound
  have hnorm : M / N ≤ V + p * log r - Q * log (1 + r * y / Q) := by
    apply (div_le_iff₀ hN).mpr
    have hh : ((∑ i, (p - 1) * log (x i)) / N) * N =
        ∑ i, (p - 1) * log (x i) := div_mul_cancel₀ _ hN.ne'
    dsimp [V, N] at hh ⊢
    linear_combination (norm := ring_nf) (hM.trans hbound) - hh
    exact le_rfl
  have hL : Z ≤ p * log r - Q * log (1 + r * y / Q) := by
    dsimp [Z]
    linarith
  refine ⟨he, ?_, ?_⟩
  · have hnon : 0 ≤ log (1 + r * y / Q) :=
      log_nonneg (le_add_of_nonneg_right (by positivity))
    have hl := (div_le_iff₀ hp).mpr (by linarith [mul_nonneg hQ.le hnon] : Z ≤ log r * p)
    exact (exp_le_exp.mpr hl).trans_eq (exp_log hr)
  · have hlog : log r + log (y / Q) ≤ log (1 + r * y / Q) := by
      rw [← log_mul hr.ne' (div_pos hy hQ).ne', show r * (y / Q) = r * y / Q by ring]
      exact log_le_log (by positivity) (le_add_of_nonneg_left zero_le_one)
    have hl : log r ≤ (-Z - Q * log (y / Q)) / ε := by
      apply (le_div_iff₀ hε).mpr
      have hh := mul_le_mul_of_nonneg_left hlog hQ.le
      dsimp [Q] at hh hL ⊢
      linarith
    exact (exp_log hr).symm.trans_le (exp_le_exp.mpr hl)

/-- For a maximizing sequence escaping every compact subset of the
positive shape and scale domain at held first shape, the second shape
must diverge and its ratio to scale must converge to the fitted gamma
rate. Consequently the sequence has exactly the fitted gamma value. -/
theorem held_first_maximizing_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) {q s : ℕ → ℝ}
    (hq : ∀ j, 0 < q j) (hs : ∀ j, 0 < s j)
    (hesc : ∀ K : Set (ℝ × ℝ), IsCompact K → K ⊆ Beta.domain →
      ∀ᶠ j in atTop, (q j, s j) ∉ K)
    {M : ℝ} (hM : ∀ u > 0, ∀ v > 0, BetaPrime.logLik x v (p, u) ≤ M)
    (hlim : Tendsto (fun j ↦ BetaPrime.logLik x (s j) (p, q j)) atTop (𝓝 M)) :
    let r := Gamma.rate x p
    Tendsto q atTop atTop ∧ Tendsto (fun j ↦ q j / s j) atTop (𝓝 r) ∧
      M = ∑ i, Gamma.logDensity p r (x i) := by
  intro r
  obtain ⟨ε, hε, lo, hlo, hi, hhi, hbounds⟩ :=
    held_first_superlevel_bounds hn hx hp (M - 1)
  have he : ∀ᶠ j in atTop, ε ≤ q j ∧ q j / s j ∈ Icc lo hi :=
    (hlim.eventually (Ioi_mem_nhds (sub_lt_self _ zero_lt_one))).mono fun j hj ↦
      hbounds _ (hq j) _ (hs j) hj.le
  have hqtop : Tendsto q atTop atTop := by
    apply tendsto_atTop.mpr
    intro B
    let K : Set (ℝ × ℝ) := Icc ε B ×ˢ Icc (ε / hi) (B / lo)
    have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
    have hsub : K ⊆ Beta.domain := fun _ v ↦
      ⟨hε.trans_le v.1.1, (div_pos hε hhi).trans_le v.2.1⟩
    filter_upwards [hesc K hK hsub, he] with j hout hj
    by_contra! hB
    apply hout
    have h₁ := (div_le_iff₀ (hs j)).mp hj.2.2
    have h₂ := (le_div_iff₀ (hs j)).mp hj.2.1
    exact ⟨⟨hj.1, hB.le⟩, (div_le_iff₀ hhi).mpr (by linarith),
      (le_div_iff₀ hlo).mpr (by linarith)⟩
  obtain ⟨hr, hopt⟩ := Gamma.heldShape hn hx hp
  let γ := fun v : ℝ ↦ ∑ i, Gamma.logDensity p v (x i)
  have hfit : γ r ≤ M := by
    have hpos : ∀ᶠ v : ℝ in atTop, 0 < v / r :=
      (eventually_gt_atTop (0 : ℝ)).mono fun _ hv ↦ div_pos hv hr
    have hrate : Tendsto (fun v : ℝ ↦ v / (v / r)) atTop (𝓝 r) := by
      apply tendsto_const_nhds.congr'
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with v hv
      field_simp [hv.ne', hr.ne']
    have h := gamma_of_shape_rate_limit hx hp hr tendsto_id hpos hrate
    exact le_of_tendsto h ((eventually_gt_atTop (0 : ℝ)).mono fun v hv ↦
      hM v hv (v / r) (div_pos hv hr))
  have hmatches (u : ℝ) (hu : 0 < u) (φ : ℕ → ℕ) (hφ : StrictMono φ)
      (ht : Tendsto ((fun j ↦ q j / s j) ∘ φ) atTop (𝓝 u)) : γ u = M ∧ u = r := by
    have hq' := hqtop.comp hφ.tendsto_atTop
    have hs' : ∀ᶠ j in atTop, 0 < (s ∘ φ) j := Eventually.of_forall fun j ↦ hs (φ j)
    have hγ := gamma_of_shape_rate_limit hx hp hu hq' hs' ht
    have him : γ u = M := tendsto_nhds_unique hγ (hlim.comp hφ.tendsto_atTop)
    refine ⟨him, ?_⟩
    by_contra hne
    have hh := hopt u hu hne
    rw [Gamma.logLik_eq_avg hn, show Gamma.profile x p = Gamma.logLik x p r from rfl,
      Gamma.logLik_eq_avg hn] at hh
    have hlt : γ u / n < γ r / n := hh
    have hstrict := (div_lt_div_iff_of_pos_right (Nat.cast_pos.mpr hn : (0 : ℝ) < n)).mp hlt
    exact hstrict.not_ge (him ▸ hfit)
  obtain ⟨u, hu, φ, hφ, hφlim⟩ := isCompact_Icc.tendsto_subseq' (he.mono fun _ hj ↦ hj.2).frequently
  obtain ⟨hγ, hu_eq⟩ := hmatches u (hlo.trans_le hu.1) φ hφ hφlim
  subst u
  refine ⟨hqtop, ?_, hγ.symm⟩
  apply Metric.tendsto_nhds.mpr
  intro δ hδ
  by_contra h
  let K := Icc lo hi ∩ {v : ℝ | δ ≤ dist v r}
  have hK : IsCompact K := isCompact_Icc.inter_right (isClosed_le continuous_const
    (continuous_id.dist continuous_const))
  have hf : ∃ᶠ j in atTop, q j / s j ∈ K :=
    ((not_eventually.mp h).and_eventually he).mono fun _ h ↦
      ⟨h.2.2, le_of_not_gt h.1⟩
  obtain ⟨v, hv, ψ, hψ, hψlim⟩ := hK.tendsto_subseq' hf
  have heq := (hmatches v (hlo.trans_le hv.1.1) ψ hψ hψlim).2
  exact hδ.not_ge (by simpa [heq] using hv.2)

/-- At held second shape, every maximizing escape is the inverse-gamma
face on the reciprocal sample. The first shape diverges and its product
with scale converges to the fitted inverse-gamma parameter. -/
theorem held_second_maximizing_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {q : ℝ} (hq : 0 < q) {p s : ℕ → ℝ}
    (hp : ∀ j, 0 < p j) (hs : ∀ j, 0 < s j)
    (hesc : ∀ K : Set (ℝ × ℝ), IsCompact K → K ⊆ Beta.domain →
      ∀ᶠ j in atTop, (p j, s j) ∉ K)
    {M : ℝ} (hM : ∀ u > 0, ∀ v > 0, BetaPrime.logLik x v (u, q) ≤ M)
    (hlim : Tendsto (fun j ↦ BetaPrime.logLik x (s j) (p j, q)) atTop (𝓝 M)) :
    let r := Gamma.rate (fun i ↦ (x i)⁻¹) q
    Tendsto p atTop atTop ∧ Tendsto (fun j ↦ p j * s j) atTop (𝓝 r) ∧
      M = (∑ i, Gamma.logDensity q r (x i)⁻¹) - ∑ i, 2 * log (x i) := by
  intro r
  let y := fun i ↦ (x i)⁻¹
  let J := ∑ i, 2 * log (x i)
  let invScale : ℝ × ℝ → ℝ × ℝ := fun v ↦ (v.1, v.2⁻¹)
  have hesc' (K : Set (ℝ × ℝ)) (hc : IsCompact K) (hsub : K ⊆ Beta.domain) :
      ∀ᶠ j in atTop, (p j, (s j)⁻¹) ∉ K := by
    have hcont : ContinuousOn invScale K := by
      apply ContinuousOn.prodMk continuous_fst.continuousOn
      exact continuous_snd.continuousOn.inv₀ (fun v hv ↦
        (show 0 < v.2 from (hsub hv).2).ne')
    have hsub' : invScale '' K ⊆ Beta.domain := by
      rintro v ⟨u, hu, rfl⟩
      exact ⟨(hsub hu).1, inv_pos.mpr (hsub hu).2⟩
    filter_upwards [hesc (invScale '' K) (hc.image_of_continuousOn hcont) hsub'] with j hj
    intro h
    exact hj ⟨(p j, (s j)⁻¹), h, by simp [invScale]⟩
  have htransfer {u v : ℝ} (hv : 0 < v) :
      BetaPrime.logLik y v (q, u) = BetaPrime.logLik x v⁻¹ (u, q) + J := by
    rw [AllFreeBetaPrime.reciprocal_logLik hx (inv_pos.mpr hv), inv_inv]
    ring
  have hM' : ∀ u > 0, ∀ v > 0, BetaPrime.logLik y v (q, u) ≤ M + J := by
    intro u hu v hv
    rw [htransfer hv]
    exact add_le_add_left (hM u hu v⁻¹ (inv_pos.mpr hv)) J
  have hlim' : Tendsto (fun j ↦ BetaPrime.logLik y (s j)⁻¹ (q, p j)) atTop
      (𝓝 (M + J)) := by
    apply (hlim.add_const J).congr
    intro j
    rw [htransfer (inv_pos.mpr (hs j)), inv_inv]
  obtain ⟨hptop, hrate, hval⟩ := held_first_maximizing_escape hn
    (fun i ↦ inv_pos.mpr (hx i)) hq hp (fun j ↦ inv_pos.mpr (hs j)) hesc' hM' hlim'
  exact ⟨hptop, by simpa only [div_inv_eq_mul] using hrate, (eq_sub_iff_add_eq).mpr hval⟩

private theorem held_first_continuous {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p : ℝ} (hp : 0 < p) {v : ℝ × ℝ} (hv : v ∈ Beta.domain) :
    ContinuousAt (fun w : ℝ × ℝ ↦ BetaPrime.logLik x w.2 (p, w.1)) v := by
  have hq : 0 < v.1 := hv.1
  have hs : 0 < v.2 := hv.2
  have hG {z : ℝ} (hz : 0 < z) : ContinuousAt (fun y ↦ log (Real.Gamma y)) z :=
    ((Real.differentiableAt_Gamma (fun m ↦ by
      have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
      linarith)).continuousAt).log (Gamma_pos_of_pos hz).ne'
  have harg : ContinuousAt (fun w : ℝ × ℝ ↦ p + w.1) v :=
    continuousAt_const.add continuous_fst.continuousAt
  have hpq := (hG (add_pos hp hq)).comp'
    (f := fun w : ℝ × ℝ ↦ p + w.1) (x := v) harg
  have hβ : ContinuousAt (fun w : ℝ × ℝ ↦ Beta.logBeta (p, w.1)) v := by
    apply ContinuousAt.congr_of_eventuallyEq
      ((continuousAt_const (y := log (Real.Gamma p))).add
        ((hG hq).comp' (f := Prod.fst) (x := v) continuous_fst.continuousAt) |>.sub hpq)
    filter_upwards [continuous_fst.continuousAt.tendsto.eventually (Ioi_mem_nhds hq)] with w hw
    exact logBeta_formula hp hw
  unfold BetaPrime.logLik
  apply tendsto_finsetSum Finset.univ
  intro i _
  have hr : ContinuousAt (fun w : ℝ × ℝ ↦ x i / w.2) v :=
    continuousAt_const.div continuous_snd.continuousAt hs.ne'
  have hh := (continuousAt_const (y := p - 1)).mul (hr.log (div_pos (hx i) hs).ne')
  have hl := (continuousAt_const.add hr).log (add_pos one_pos (div_pos (hx i) hs)).ne'
  exact (((hh.sub (continuous_snd.continuousAt.log hs.ne')).sub
    ((continuousAt_const.add continuous_fst.continuousAt).mul hl)).sub hβ).tendsto

/-- At held first shape, a finite maximum exists if and only if a
finite point reaches the fitted gamma value. This includes finite
equality and all constant samples. The conclusion uses a finite
point as witness and does not require a finite stationary list. -/
theorem held_first_face_comparison (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) :
    let L := fun v : ℝ × ℝ ↦ BetaPrime.logLik x v.2 (p, v.1)
    let G := ∑ i, Gamma.logDensity p (Gamma.rate x p) (x i)
    (∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔
      ∃ v ∈ Beta.domain, G ≤ L v := by
  intro L G
  let r := Gamma.rate x p
  let γ := fun v : ℝ ↦ ∑ i, Gamma.logDensity p v (x i)
  obtain ⟨hr, hopt⟩ := Gamma.heldShape hn hx hp
  have hface : ∀ u > 0, γ u ≤ G := by
    intro u hu
    by_cases he : u = r
    · exact le_of_eq (congrArg γ he)
    · have hh := hopt u hu he
      rw [Gamma.logLik_eq_avg hn, show Gamma.profile x p = Gamma.logLik x p r from rfl,
        Gamma.logLik_eq_avg hn] at hh
      exact ((div_lt_div_iff_of_pos_right (Nat.cast_pos.mpr hn : (0 : ℝ) < n)).mp
        (show γ u / n < γ r / n from hh)).le
  constructor
  · rintro ⟨v, hv, hm⟩
    refine ⟨v, hv, ?_⟩
    have h := AllFreeBetaPrime.gamma_path hx (inv_pos.mpr hr) hp
    have hlim : Tendsto (fun t : ℝ ↦ L (t⁻¹, r⁻¹ / t)) (𝓝[>] 0) (𝓝 G) := by
      simpa only [inv_inv] using h
    exact le_of_tendsto hlim ((show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).mono
      fun t ht ↦ hm (show (t⁻¹, r⁻¹ / t) ∈ Beta.domain from
        ⟨inv_pos.mpr ht, div_pos (inv_pos.mpr hr) ht⟩))
  · rintro ⟨v, hv, hvg⟩
    by_cases h : ∃ w ∈ Beta.domain, G < L w
    · obtain ⟨w, hw, hgw⟩ := h
      let C := L w
      obtain ⟨ε, hε, lo, hlo, hi, hhi, hb⟩ := held_first_superlevel_bounds hn hx hp C
      have hB : ∃ B, ∀ u ∈ Beta.domain, C ≤ L u → u.1 ≤ B := by
        by_contra! hh
        choose u hu huc huB using fun j : ℕ ↦ hh (j : ℝ)
        have hqt : Tendsto (fun j ↦ (u j).1) atTop atTop :=
          tendsto_atTop_mono (fun j ↦ (huB j).le) tendsto_natCast_atTop_atTop
        have ht : ∀ j, (u j).1 / (u j).2 ∈ Icc lo hi :=
          fun j ↦ (hb _ (hu j).1 _ (hu j).2 (huc j)).2
        obtain ⟨v, hv, φ, hφ, hlim⟩ := isCompact_Icc.tendsto_subseq ht
        have hf := gamma_of_shape_rate_limit hx hp (hlo.trans_le hv.1)
          (hqt.comp hφ.tendsto_atTop) (Eventually.of_forall fun j ↦ (hu (φ j)).2) hlim
        have hc : C ≤ γ v := ge_of_tendsto hf (Eventually.of_forall fun j ↦ huc (φ j))
        exact hgw.not_ge (hc.trans (hface v (hlo.trans_le hv.1)))
      obtain ⟨B, hB⟩ := hB
      let K := Icc ε B ×ˢ Icc (ε / hi) (B / lo)
      have hsub : K ⊆ Beta.domain := fun _ h ↦
        ⟨hε.trans_le h.1.1, (div_pos hε hhi).trans_le h.2.1⟩
      have hkeep (u : ℝ × ℝ) (hu : u ∈ Beta.domain) (hC : C ≤ L u) : u ∈ K := by
        have h := hb _ hu.1 _ hu.2 hC
        have h₁ := (div_le_iff₀ (show 0 < u.2 from hu.2)).mp h.2.2
        have h₂ := (le_div_iff₀ (show 0 < u.2 from hu.2)).mp h.2.1
        have hU := hB u hu hC
        exact ⟨⟨h.1, hU⟩, (div_le_iff₀ hhi).mpr (by linarith),
          (le_div_iff₀ hlo).mpr (by linarith)⟩
      have hwK := hkeep w hw le_rfl
      obtain ⟨u, hu, hm⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨w, hwK⟩
        (fun v hv ↦ (held_first_continuous hx hp (hsub hv)).continuousWithinAt)
      refine ⟨u, hsub hu, fun t ht ↦ ?_⟩
      by_cases hC : C ≤ L t
      · exact hm (hkeep t ht hC)
      · exact (lt_of_not_ge hC).le.trans (hm hwK)
    · exact ⟨v, hv, fun w hw ↦ (le_of_not_gt fun hwg ↦ h ⟨w, hw, hwg⟩).trans hvg⟩

/-- At held second shape, finite attainment is equivalent to a
finite point reaching the fitted inverse-gamma value. Equality is
included. The comparison uses the exact reciprocal parameter map. -/
theorem held_second_face_comparison (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {q : ℝ} (hq : 0 < q) :
    let L := fun v : ℝ × ℝ ↦ BetaPrime.logLik x v.2 (v.1, q)
    let r := Gamma.rate (fun i ↦ (x i)⁻¹) q
    let G := (∑ i, Gamma.logDensity q r (x i)⁻¹) - ∑ i, 2 * log (x i)
    (∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔
      ∃ v ∈ Beta.domain, G ≤ L v := by
  intro L r G
  let y := fun i ↦ (x i)⁻¹
  let J := ∑ i, 2 * log (x i)
  let γ := ∑ i, Gamma.logDensity q r (y i)
  let f := fun v : ℝ × ℝ ↦ BetaPrime.logLik y v.2 (q, v.1)
  let φ := fun v : ℝ × ℝ ↦ (v.1, v.2⁻¹)
  have hφ : BijOn φ Beta.domain Beta.domain := by
    refine ⟨fun _ h ↦ ⟨h.1, inv_pos.mpr h.2⟩, ?_, ?_⟩
    · intro v _ w _ h
      have hl := congrArg (fun u : ℝ × ℝ ↦ u.1) h
      have hr := congrArg (fun u : ℝ × ℝ ↦ u.2) h
      exact Prod.ext hl (inv_injective hr)
    · intro v hv
      exact ⟨(v.1, v.2⁻¹), ⟨hv.1, inv_pos.mpr hv.2⟩, Prod.ext rfl (inv_inv _)⟩
  have he (v : ℝ × ℝ) (hv : v ∈ Beta.domain) : L v = f (φ v) + (-J) := by
    change BetaPrime.logLik x v.2 (v.1, q) = f (φ v) + (-J)
    rw [AllFreeBetaPrime.reciprocal_logLik hx hv.2]
    rfl
  have h₁ := Transfer.exists_max_iff hφ he
  have h₂ := held_first_face_comparison hn (x := y) (fun i ↦ inv_pos.mpr (hx i)) hq
  refine h₁.trans (h₂.trans ?_)
  constructor
  · rintro ⟨u, hu, hγ⟩
    obtain ⟨v, hv, rfl⟩ := hφ.surjOn hu
    refine ⟨v, hv, ?_⟩
    rw [he v hv]
    exact sub_le_sub_right hγ J
  · rintro ⟨v, hv, hG⟩
    refine ⟨φ v, hφ.mapsTo hv, ?_⟩
    rw [he v hv] at hG
    change γ - J ≤ f (φ v) - J at hG
    exact (sub_le_sub_iff_right J).mp hG

/-- Both beta-prime shapes diverge on the symmetric fixed-scale ray
at a constant positive observation, and the sample likelihood diverges. -/
theorem constant_both_shapes_path (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    let x : Fin n → ℝ := fun _ ↦ v
    Tendsto (fun t : ℝ ↦ BetaPrime.logLik x v (t / 2, t / 2)) atTop atTop := by
  intro x
  have hhalf : (1 / 2 : ℝ) ∈ Ioo 0 1 := by constructor <;> norm_num
  have h := tendsto_atTop_add_const_right _ (BetaPrime.logJacobian x v)
    (Beta.constant_unbounded (n := n) hn hhalf)
  apply h.congr
  intro t
  rw [BetaPrime.logLik_transfer (x := x) hv (fun _ ↦ hv)]
  have hu : BetaPrime.unitData x v = fun _ ↦ 1 / 2 := by
    funext i
    simp only [BetaPrime.unitData, x]
    field_simp [hv.ne']
    norm_num
  rw [hu]
  have hshape : (t * (1 / 2), t * (1 - 1 / 2)) = (t / 2, t / 2) := by
    congr 1 <;> ring
  simp only [hshape]

end UnivariateML.Endpoint3BetaPrime

end
