module

public import UnivariateML.Family.Fatigue
import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics

set_option linter.style.header false

/-!
Threshold concentration in the unrestricted Birnbaum–Saunders model.

## Deviations

The all-free result includes tied minima. The held-shape collapse theorem covers constant
data. The held-scale classification and the held-shape uniqueness claims need further
proofs.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.Shape2Fatigue

noncomputable section

private theorem threshold_density {r d : ℝ} (hr : 0 < r) (hd : 0 ≤ d) :
    let e := r⁻¹
    let t := d + e ^ 2
    Fatigue.logDensity (sqrt r) e t =
      log (t + e) - 3 * log t / 2 - log 2 - log (2 * π) / 2 -
        (t + e ^ 2 / t - 2 * e) / 2 := by
  dsimp only
  have hs := sqrt_pos.mpr hr
  have ht : 0 < d + (r⁻¹) ^ 2 := add_pos_of_nonneg_of_pos hd (sq_pos_of_pos (inv_pos.mpr hr))
  rw [Fatigue.logDensity, sq_sqrt hr.le, log_inv, log_mul (by norm_num) hs.ne', log_sqrt hr.le]
  field_simp
  ring

private theorem threshold_point_min :
    Tendsto (fun r : ℝ ↦ Fatigue.logDensity (sqrt r) r⁻¹ (0 + (r⁻¹) ^ 2)) atTop atTop := by
  have he : Tendsto (fun r : ℝ ↦ r⁻¹) atTop (𝓝 0) := tendsto_inv_atTop_zero
  have h1 : Tendsto (fun r : ℝ ↦ 1 + r⁻¹) atTop (𝓝 1) := by
    simpa only [add_zero] using he.const_add 1
  have hlog := (continuousAt_log (one_ne_zero : (1 : ℝ) ≠ 0)).tendsto.comp h1
  have hrem := ((hlog.sub_const (log 2 + log (2 * π) / 2)).sub
    (((he.pow 2).add_const 1 |>.sub (he.const_mul 2)).div_const 2))
  have ht := (tendsto_log_atTop.const_mul_atTop (by norm_num : (0 : ℝ) < 2)).atTop_add hrem
  apply ht.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
  have he0 := inv_pos.mpr hr
  rw [threshold_density hr le_rfl]
  have heq : (r⁻¹) ^ 2 + r⁻¹ = r⁻¹ * (1 + r⁻¹) := by ring
  simp only [zero_add, Function.comp_apply]
  rw [heq, log_mul he0.ne' (by positivity), log_inv, log_pow, log_inv,
    div_self (pow_ne_zero 2 he0.ne')]
  push_cast
  ring

private theorem threshold_point_other {d : ℝ} (hd : 0 < d) :
    Tendsto (fun r : ℝ ↦ Fatigue.logDensity (sqrt r) r⁻¹ (d + (r⁻¹) ^ 2)) atTop
      (𝓝 (log d - 3 * log d / 2 - log 2 - log (2 * π) / 2 - d / 2)) := by
  have he : Tendsto (fun r : ℝ ↦ r⁻¹) atTop (𝓝 0) := tendsto_inv_atTop_zero
  have ht : Tendsto (fun r : ℝ ↦ d + (r⁻¹) ^ 2) atTop (𝓝 d) := by
    simpa using (he.pow 2).const_add d
  have hte : Tendsto (fun r : ℝ ↦ d + (r⁻¹) ^ 2 + r⁻¹) atTop (𝓝 d) := by
    simpa only [add_zero] using ht.add he
  have h1 : Tendsto (fun r : ℝ ↦ log (d + (r⁻¹) ^ 2 + r⁻¹)) atTop (𝓝 (log d)) := by
    simpa only [add_zero, Function.comp_def] using
      (continuousAt_log hd.ne').tendsto.comp hte
  have h2 : Tendsto (fun r : ℝ ↦ log (d + (r⁻¹) ^ 2)) atTop (𝓝 (log d)) :=
    (continuousAt_log hd.ne').tendsto.comp ht
  have h3 : Tendsto (fun r : ℝ ↦ (r⁻¹) ^ 2 / (d + (r⁻¹) ^ 2)) atTop (𝓝 0) := by
    change Tendsto ((fun r : ℝ ↦ (r⁻¹) ^ 2) / (fun r : ℝ ↦ d + (r⁻¹) ^ 2)) atTop (𝓝 0)
    simpa only [zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div] using
      (he.pow 2).div ht hd.ne'
  have h := (((h1.sub ((h2.const_mul 3).div_const 2)).sub_const (log 2)).sub_const
    (log (2 * π) / 2)).sub (((ht.add h3).sub (he.const_mul 2)).div_const 2)
  simp only [add_zero, mul_zero, sub_zero] at h
  apply h.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
  exact (threshold_density hr hd.le).symm

variable {n : ℕ}

/-- Along the joint threshold, scale and shape escape, every minimum
observation diverges and all observations above the minimum have finite
log-density limits. -/
theorem all_free_threshold_path {x : Fin n → ℝ} {m : ℝ} (_hm : ∀ i, m ≤ x i) :
    (∀ i, x i = m →
      Tendsto (fun r : ℝ ↦ Fatigue.logDensity (sqrt r) r⁻¹ (x i - (m - (r⁻¹) ^ 2)))
        atTop atTop) ∧
    (∀ i, m < x i →
      Tendsto (fun r : ℝ ↦ Fatigue.logDensity (sqrt r) r⁻¹ (x i - (m - (r⁻¹) ^ 2)))
        atTop (𝓝 (log (x i - m) - 3 * log (x i - m) / 2 -
          log 2 - log (2 * π) / 2 - (x i - m) / 2))) := by
  constructor
  · intro i hi
    exact threshold_point_min.congr fun r ↦ by
      rw [hi, show m - (m - (r⁻¹) ^ 2) = 0 + (r⁻¹) ^ 2 by ring]
  · intro i hi
    exact (threshold_point_other (sub_pos.mpr hi)).congr fun r ↦ by
      rw [show x i - (m - (r⁻¹) ^ 2) = x i - m + (r⁻¹) ^ 2 by ring]

/-- For every nonempty real sample, the unrestricted finite log likelihood
is unbounded above. -/
theorem all_free_unbounded (hn : 0 < n) (x : Fin n → ℝ) :
    ∀ M : ℝ, ∃ (a b μ : ℝ), 0 < a ∧ 0 < b ∧ (∀ i, μ < x i) ∧
      M < Fatigue.logLik (fun i ↦ x i - μ) a b := by
  obtain ⟨i, _, hmin⟩ := Finset.exists_min_image Finset.univ x
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  let m := x i
  have hm (j : Fin n) : m ≤ x j := hmin j (Finset.mem_univ _)
  let L (r : ℝ) (j : Fin n) :=
    Fatigue.logDensity (sqrt r) r⁻¹ (x j - (m - (r⁻¹) ^ 2))
  have hp := all_free_threshold_path hm
  have hbounded (j : Fin n) : ∃ c : ℝ, ∀ᶠ r in atTop, c ≤ L r j := by
    rcases (hm j).eq_or_lt with hj | hj
    · exact ⟨0, tendsto_atTop.1 (hp.1 j hj.symm) 0⟩
    · let c := log (x j - m) - 3 * log (x j - m) / 2 -
        log 2 - log (2 * π) / 2 - (x j - m) / 2
      exact ⟨c - 1, ((hp.2 j hj).eventually (eventually_gt_nhds (by linarith : c - 1 < c))).mono
        fun _ h ↦ h.le⟩
  classical
  choose C hC using hbounded
  intro M
  let s := Finset.univ.erase i
  have hothers : ∀ᶠ r in atTop, ∀ j ∈ s, C j ≤ L r j :=
    (Filter.eventually_all_finset s).2 fun j _ ↦ hC j
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hi := tendsto_atTop.1 (hp.1 i rfl) (M * n + 1 - ∑ j ∈ s, C j)
  obtain ⟨r, hr, hci, hrest⟩ := ((eventually_gt_atTop (0 : ℝ)).and (hi.and hothers)).exists
  have hsum := Finset.sum_le_sum hrest
  have he : (∑ j ∈ s, L r j) + L r i = ∑ j, L r j :=
    Finset.sum_erase_add _ _ (Finset.mem_univ _)
  refine ⟨sqrt r, r⁻¹, m - (r⁻¹) ^ 2, sqrt_pos.mpr hr, inv_pos.mpr hr,
    fun j ↦ (sub_lt_self _ (sq_pos_of_pos (inv_pos.mpr hr))).trans_le (hm j), ?_⟩
  rw [Fatigue.logLik_eq_avg_logDensity hn _ (sqrt_pos.mpr hr), Fatigue.avg]
  change M < (∑ j, L r j) / n
  exact (lt_div_iff₀ hN).2 (by linarith only [hci, hsum, he])

theorem all_free_no_max (hn : 0 < n) (x : Fin n → ℝ) :
    ¬ ∃ p : ℝ × ℝ × ℝ,
      p ∈ {q | 0 < q.1 ∧ 0 < q.2.1 ∧ ∀ i, q.2.2 < x i} ∧
      IsMaxOn (fun q : ℝ × ℝ × ℝ ↦ Fatigue.logLik (fun i ↦ x i - q.2.2) q.1 q.2.1)
        {q | 0 < q.1 ∧ 0 < q.2.1 ∧ ∀ i, q.2.2 < x i} p := by
  rintro ⟨p, _, hp⟩
  obtain ⟨a, b, μ, ha, hb, hμ, hgt⟩ :=
    all_free_unbounded hn x (Fatigue.logLik (fun i ↦ x i - p.2.2) p.1 p.2.1)
  exact hgt.not_ge (isMaxOn_iff.mp hp (a, b, μ) ⟨ha, hb, hμ⟩)


private def boundConstant (a : ℝ) : ℝ :=
  3 / (2 * a ^ 2) - log (2 * a) - log (2 * π) / 2 - (log (2 * a ^ 2)⁻¹ + 1) / 2

private theorem density_bound {a b t : ℝ} (ha : 0 < a) (hb : 0 < b) (ht : 0 < t) :
    Fatigue.logDensity a b t ≤ boundConstant a - log t -
      (t / b + b / t) / (4 * a ^ 2) := by
  let u := t / b + b / t + 2
  let k := (2 * a ^ 2)⁻¹
  have hu : 0 < u := by dsimp [u]; positivity
  have hk : 0 < k := by dsimp [k]; positivity
  have he : u = (t + b) ^ 2 / (t * b) := by dsimp [u]; field_simp; ring
  have hl : log u = 2 * log (t + b) - (log t + log b) := by
    rw [he, log_div (by positivity) (mul_ne_zero ht.ne' hb.ne'),
      log_pow, log_mul ht.ne' hb.ne']
    norm_num
  have h := log_le_sub_one_of_pos (mul_pos hk hu)
  rw [log_mul hk.ne' hu.ne', hl] at h
  unfold Fatigue.logDensity boundConstant
  change _ ≤ 3 / (2 * a ^ 2) - log (2 * a) - log (2 * π) / 2 -
    (log k + 1) / 2 - log t - (t / b + b / t) / (4 * a ^ 2)
  have hform : k * u = (t / b + b / t + 2) / (2 * a ^ 2) := by
    dsimp [k, u]
    ring
  rw [hform] at h
  have hsplit : (t / b + b / t - 2) / (2 * a ^ 2) =
      2 * ((t / b + b / t) / (4 * a ^ 2)) - 1 / (a ^ 2) := by ring
  rw [hsplit]
  have hsmall : (t / b + b / t + 2) / (2 * a ^ 2) =
      2 * ((t / b + b / t) / (4 * a ^ 2)) + 1 / (a ^ 2) := by ring
  rw [hsmall] at h
  have hconst : 3 / (2 * a ^ 2) = (3 / 2) * (1 / (a ^ 2)) := by ring
  rw [hconst]
  linarith only [h]

private def scaleObjective (t : Fin n → ℝ) (a b : ℝ) : ℝ :=
  ∑ i, Fatigue.logDensity a b (t i)

private theorem scale_continuous (t : Fin n → ℝ) (a : ℝ) (ht : ∀ i, 0 < t i) :
    ContinuousOn (scaleObjective t a) (Ioi 0) := by
  unfold scaleObjective
  apply continuousOn_finsetSum
  intro i _
  have hb : ContinuousOn (fun b : ℝ ↦ log b) (Ioi 0) :=
    continuousOn_id.log fun _ h ↦ ne_of_gt h
  have ht' : ContinuousOn (fun b : ℝ ↦ log (t i + b)) (Ioi 0) :=
    (continuousOn_const.add continuousOn_id).log
      (fun b hb ↦ (add_pos (ht i) (show 0 < b from hb)).ne')
  unfold Fatigue.logDensity
  exact (((ht'.sub continuousOn_const).sub (hb.div_const 2)).sub
    continuousOn_const |>.sub continuousOn_const).sub
      ((((continuousOn_const.div continuousOn_id (fun _ h ↦ ne_of_gt h)).add
      (continuousOn_id.div_const (t i))).sub continuousOn_const).div_const (2 * a ^ 2))

/-- At held shape and threshold, the scale-end penalties send the
entire sample log likelihood to minus infinity at both scale ends. -/
theorem held_alpha_mu_scale_ends (hn : 0 < n) {t : Fin n → ℝ}
    (ht : ∀ i, 0 < t i) {a : ℝ} (ha : 0 < a) :
    Tendsto (Fatigue.logLik t a) (𝓝[>] 0) atBot ∧
      Tendsto (Fatigue.logLik t a) atTop atBot := by
  let i : Fin n := ⟨0, hn⟩
  let C := ∑ j, (boundConstant a - log (t j))
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hD : 0 < 4 * a ^ 2 := by positivity
  have hbnd (b : ℝ) (hb : 0 < b) :
      Fatigue.logLik t a b ≤ (C - (t i / b + b / t i) / (4 * a ^ 2)) / n := by
    rw [Fatigue.logLik_eq_avg_logDensity hn _ ha, Fatigue.avg]
    apply (div_le_div_iff_of_pos_right hN).2
    have h := Finset.sum_le_sum (s := Finset.univ) fun j _ ↦ density_bound ha hb (ht j)
    simp only [Finset.sum_sub_distrib] at h
    have hp := Finset.single_le_sum
      (s := Finset.univ) (f := fun j ↦ (t j / b + b / t j) / (4 * a ^ 2))
      (fun j _ ↦ div_nonneg (add_nonneg (div_nonneg (ht j).le hb.le)
        (div_nonneg hb.le (ht j).le)) hD.le) (Finset.mem_univ i)
    simp only [C, Finset.sum_sub_distrib]
    linarith only [h, hp]
  constructor
  · have hf := ((tendsto_inv_nhdsGT_zero.const_mul_atTop (ht i)).atTop_div_const hD)
    have hb := tendsto_neg_atTop_atBot.comp hf
    have hlim := (tendsto_atBot_add_const_right _ C hb).atBot_div_const hN
    apply tendsto_atBot_mono' _ ?_ hlim
    filter_upwards [self_mem_nhdsWithin] with b (hb : 0 < b)
    refine (hbnd b hb).trans (div_le_div_of_nonneg_right ?_ hN.le)
    rw [div_eq_mul_inv (t i) b]
    have h := div_nonneg (div_nonneg hb.le (ht i).le) hD.le
    simp only [Function.comp_apply, add_div]
    linarith
  · have hf := (tendsto_id.atTop_div_const (ht i)).atTop_div_const hD
    have hb := tendsto_neg_atTop_atBot.comp hf
    have hlim := (tendsto_atBot_add_const_right _ C hb).atBot_div_const hN
    apply tendsto_atBot_mono' _ ?_ hlim
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
    refine (hbnd b hb).trans (div_le_div_of_nonneg_right ?_ hN.le)
    have h := div_nonneg (div_nonneg (ht i).le hb.le) hD.le
    simp only [Function.comp_apply, id_eq, add_div]
    linarith

private theorem exists_of_ends {f : ℝ → ℝ} (hc : ContinuousOn f (Ioi 0))
    (h0 : Tendsto f (𝓝[>] 0) atBot) (hInf : Tendsto f atTop atBot) :
    ∃ x ∈ Ioi 0, IsMaxOn f (Ioi 0) x := by
  obtain ⟨l, hl, hlo⟩ := (nhdsGT_basis (0 : ℝ)).mem_iff.mp
    (h0.eventually (eventually_lt_atBot (f 1)))
  obtain ⟨r, hhi⟩ := eventually_atTop.1 (hInf.eventually (eventually_lt_atBot (f 1)))
  let lo := min l 1
  let hi := max r 1
  have hs : Icc lo hi ⊆ Ioi (0 : ℝ) := fun _ h ↦
    (lt_min hl zero_lt_one).trans_le h.1
  have h1 : 1 ∈ Icc lo hi := ⟨min_le_right _ _, le_max_right _ _⟩
  obtain ⟨x, hx, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1, h1⟩ (hc.mono hs)
  refine ⟨x, hs hx, fun y hy ↦ ?_⟩
  by_cases h : y ∈ Icc lo hi
  · exact hm h
  · have hlt : f y < f 1 := by
      rw [mem_Icc, not_and_or] at h
      rcases h with h | h
      · exact hlo ⟨hy, (lt_of_not_ge h).trans_le (min_le_left _ _)⟩
      · exact hhi y ((le_max_left _ _).trans (lt_of_not_ge h).le)
    exact hlt.le.trans (hm h1)

/-- Every nonempty positive excess sample admits a positive scale
maximizer at each positive held shape. -/
theorem held_alpha_mu_attains (hn : 0 < n) {t : Fin n → ℝ}
    (ht : ∀ i, 0 < t i) {a : ℝ} (ha : 0 < a) :
    ∃ b ∈ Ioi 0, IsMaxOn (Fatigue.logLik t a) (Ioi 0) b := by
  have hc : ContinuousOn (Fatigue.logLik t a) (Ioi 0) := by
    rw [show Fatigue.logLik t a = fun b ↦ scaleObjective t a b / (n : ℝ) from
      funext fun b ↦ Fatigue.logLik_eq_avg_logDensity hn t ha b]
    exact (scale_continuous t a ht).div_const (n : ℝ)
  exact exists_of_ends hc (held_alpha_mu_scale_ends hn ht ha).1
    (held_alpha_mu_scale_ends hn ht ha).2


private theorem threshold_continuous (g : Fin n → ℝ) (a : ℝ) {b : ℝ}
    (hb : 0 < b) (hg : ∀ i, 0 ≤ g i) :
    ContinuousOn (fun δ ↦ scaleObjective (fun i ↦ δ + g i) a b) (Ioi 0) := by
  unfold scaleObjective
  apply continuousOn_finsetSum
  intro i _
  have ht : ContinuousOn (fun δ : ℝ ↦ δ + g i) (Ioi 0) :=
    continuousOn_id.add continuousOn_const
  have htpos (δ : ℝ) (hδ : δ ∈ Ioi (0 : ℝ)) : 0 < δ + g i :=
    add_pos_of_pos_of_nonneg hδ (hg i)
  have hlog := ht.log (fun δ hδ ↦ (htpos δ hδ).ne')
  have hlog' := (ht.add continuousOn_const).log
    (fun δ hδ ↦ (add_pos_of_pos_of_nonneg (htpos δ hδ) hb.le).ne')
  unfold Fatigue.logDensity
  exact ((((hlog'.sub ((hlog.const_mul 3).div_const 2)).sub continuousOn_const).sub
    continuousOn_const).sub continuousOn_const).sub
      ((((ht.div_const b).add
        (continuousOn_const.div ht (fun δ hδ ↦ (htpos δ hδ).ne'))).sub
          continuousOn_const).div_const (2 * a ^ 2))

private theorem threshold_bound (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ j, 0 ≤ g j)
    {i : Fin n} (hi : g i = 0) {a b δ : ℝ} (ha : 0 < a) (hb : 0 < b) (hδ : 0 < δ) :
    Fatigue.logLik (fun j ↦ δ + g j) a b ≤
      boundConstant a - log δ - (b / (4 * a ^ 2 * n)) / δ := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hD : 0 < 4 * a ^ 2 := by positivity
  have hT (j : Fin n) : 0 < δ + g j := add_pos_of_pos_of_nonneg hδ (hg j)
  let P := fun j ↦ (δ + g j) / b + b / (δ + g j)
  have hsum := Finset.sum_le_sum (s := Finset.univ) fun j _ ↦
    (density_bound ha hb (hT j)).trans (sub_le_sub_right
      (sub_le_sub_left (log_le_log hδ (le_add_of_nonneg_right (hg j))) _) (P j / (4 * a ^ 2)))
  have hp : b / δ / (4 * a ^ 2) ≤ ∑ j, P j / (4 * a ^ 2) := by
    have h := Finset.single_le_sum (s := Finset.univ)
      (f := fun j ↦ P j / (4 * a ^ 2)) (fun j _ ↦ div_nonneg
        (add_nonneg (div_nonneg (hT j).le hb.le) (div_nonneg hb.le (hT j).le)) hD.le)
      (Finset.mem_univ i)
    have hp' : b / δ / (4 * a ^ 2) ≤ P i / (4 * a ^ 2) := by
      apply div_le_div_of_nonneg_right _ hD.le
      simp only [P, hi, add_zero]
      exact le_add_of_nonneg_left (div_nonneg hδ.le hb.le)
    exact hp'.trans h
  rw [Fatigue.logLik_eq_avg_logDensity hn _ ha, Fatigue.avg]
  apply (div_le_iff₀ hN).2
  simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul] at hsum
  have he : (b / (4 * a ^ 2 * n)) / δ * n = b / δ / (4 * a ^ 2) := by field_simp
  nlinarith only [hsum, hp, he]

/-- At held positive shape and scale, the log likelihood tends to minus
infinity both at contact with the minimum and as the threshold recedes. -/
theorem held_alpha_beta_threshold_ends (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    Tendsto (fun δ ↦ Fatigue.logLik (fun i ↦ δ + g i) a b) (𝓝[>] 0) atBot ∧
      Tendsto (fun δ ↦ Fatigue.logLik (fun i ↦ δ + g i) a b) atTop atBot := by
  obtain ⟨i, hi⟩ := hz
  let C := boundConstant a
  let k := b / (4 * a ^ 2 * n)
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hk : 0 < k := by dsimp [k]; positivity
  have hbnd (δ : ℝ) (hδ : 0 < δ) :
      Fatigue.logLik (fun j ↦ δ + g j) a b ≤ C - log δ - k / δ :=
    threshold_bound hn hg hi ha hb hδ
  constructor
  · have hlog : Tendsto (fun δ : ℝ ↦ -(log δ * δ)) (𝓝[>] 0) (𝓝 0) := by
      have h := tendsto_log_mul_rpow_nhdsGT_zero (r := 1) one_pos
      simpa only [rpow_one, neg_zero] using h.neg
    have hInv := tendsto_inv_nhdsGT_zero.const_mul_atTop (half_pos hk)
    have hneg := tendsto_neg_atTop_atBot.comp hInv
    have hlim := tendsto_atBot_add_const_right _ C hneg
    apply tendsto_atBot_mono' _ ?_ hlim
    filter_upwards [self_mem_nhdsWithin, hlog.eventually (eventually_lt_nhds (half_pos hk))]
      with δ (hδ : 0 < δ) hsmall
    apply (hbnd δ hδ).trans
    have hl : -log δ ≤ k / 2 / δ := (le_div_iff₀ hδ).2 (by linarith only [hsmall])
    dsimp only [Function.comp_apply]
    linarith only [hl, show k / 2 / δ = k / δ / 2 by ring,
      show k / 2 * δ⁻¹ = k / δ / 2 by ring]
  · have hneg := tendsto_neg_atTop_atBot.comp tendsto_log_atTop
    have hlim := tendsto_atBot_add_const_right _ C hneg
    apply tendsto_atBot_mono' _ ?_ hlim
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with δ hδ
    exact (hbnd δ hδ).trans (by
      dsimp only [Function.comp_apply]
      linarith only [div_nonneg hk.le hδ.le])

/-- Every nonempty sample at held positive shape and scale attains
at an interior threshold, expressed as a positive distance from the minimum. -/
theorem held_alpha_beta_attains (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∃ δ ∈ Ioi (0 : ℝ),
      IsMaxOn (fun d ↦ Fatigue.logLik (fun i ↦ d + g i) a b) (Ioi 0) δ := by
  have hc : ContinuousOn (fun δ ↦ Fatigue.logLik (fun i ↦ δ + g i) a b) (Ioi 0) := by
    rw [show (fun δ ↦ Fatigue.logLik (fun i ↦ δ + g i) a b) =
      fun δ ↦ scaleObjective (fun i ↦ δ + g i) a b / (n : ℝ) from
      funext fun δ ↦ Fatigue.logLik_eq_avg_logDensity hn _ ha b]
    exact (threshold_continuous g a hb hg).div_const (n : ℝ)
  exact exists_of_ends hc (held_alpha_beta_threshold_ends hn hg hz ha hb).1
    (held_alpha_beta_threshold_ends hn hg hz ha hb).2

/-- Holding the shape still allows simultaneous threshold and scale
collapse on a constant sample. -/
theorem held_alpha_constant_collapse (hn : 0 < n) {a : ℝ} (ha : 0 < a) :
    Tendsto (fun d : ℝ ↦ Fatigue.logLik (fun _ : Fin n ↦ d) a d)
      (𝓝[>] 0) atTop := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hl := tendsto_neg_atBot_atTop.comp tendsto_log_nhdsGT_zero
  have hlim := tendsto_atTop_add_const_right _ (-log a - log (2 * π) / 2) hl
  apply hlim.congr'
  filter_upwards [self_mem_nhdsWithin] with d (hd : 0 < d)
  rw [Fatigue.logLik_eq_avg_logDensity hn _ ha, Fatigue.avg]
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
    mul_div_cancel_left₀ _ hN, Fatigue.logDensity, div_self hd.ne']
  rw [show d + d = 2 * d by ring, log_mul (by norm_num : (2 : ℝ) ≠ 0) hd.ne',
    log_mul (by norm_num : (2 : ℝ) ≠ 0) ha.ne']
  dsimp only [Function.comp_apply]
  ring

end

end UnivariateML.Shape2Fatigue

end
