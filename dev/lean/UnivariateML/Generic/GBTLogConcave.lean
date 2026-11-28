module

public import Mathlib.Analysis.Convex.Function
public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Algebra.Order.BigOperators.Group.Finset
public import Mathlib.MeasureTheory.Measure.Lebesgue.Basic
public import Mathlib.MeasureTheory.Integral.Bochner.Set

set_option linter.style.header false

/-! A normalization bound for the peak and mode of a log-concave density. -/

@[expose] public section

open Real Set MeasureTheory Filter Topology
namespace UnivariateML.GBTLogConcave

/-- A density with concave log has mass at least its peak divided by
`exp 1` times the distance to a point divided by one plus its log loss.
This provides a pointwise bound uniform in both the peak and the mode. -/
theorem peak_mul_distance_le {f : ℝ → ℝ} {m y : ℝ}
    (hconc : ConcaveOn ℝ univ f) (hm : f y ≤ f m)
    (hi : Integrable (fun z ↦ exp (f z)) volume)
    (hn : (∫ z, exp (f z)) = 1) :
    exp (f m) * |y - m| ≤ exp 1 * (1 + (f m - f y)) := by
  let d := f m - f y
  have hd : 0 ≤ d := sub_nonneg.mpr hm
  have hden : 0 < 1 + d := by linarith
  let t := (1 + d)⁻¹
  have ht : 0 < t := inv_pos.mpr hden
  let b := m + t * (y - m)
  let S := Icc (min m b) (max m b)
  have hlower (z : ℝ) (hz : z ∈ S) : exp (f m - 1) ≤ exp (f z) := by
    by_cases he : y = m
    · have hz' : z = m := by simpa [S, b, he] using hz
      rw [hz']
      exact exp_le_exp.mpr (by linarith)
    let u := (z - m) / (y - m)
    have hub : 0 ≤ u ∧ u ≤ t := by
      rcases lt_or_gt_of_ne he with hy | hy
      · have hb : b ≤ m := by
          dsimp [b]
          linarith [mul_nonpos_of_nonneg_of_nonpos ht.le (sub_nonpos.mpr hy.le)]
        have hz' : b ≤ z ∧ z ≤ m := by simpa [S, min_eq_right hb, max_eq_left hb] using hz
        refine ⟨div_nonneg_of_nonpos (sub_nonpos.mpr hz'.2) (sub_nonpos.mpr hy.le),
          (div_le_iff_of_neg (sub_neg.mpr hy)).mpr ?_⟩
        exact le_sub_iff_add_le.mpr (by simpa [b, add_comm] using hz'.1)
      · have hb : m ≤ b := by
          dsimp [b]
          linarith [mul_nonneg ht.le (sub_nonneg.mpr hy.le)]
        have hz' : m ≤ z ∧ z ≤ b := by simpa [S, min_eq_left hb, max_eq_right hb] using hz
        refine ⟨div_nonneg (sub_nonneg.mpr hz'.1) (sub_nonneg.mpr hy.le),
          (div_le_iff₀ (sub_pos.mpr hy)).mpr ?_⟩
        linarith only [show z ≤ m + t * (y - m) from hz'.2]
    have ht1 : t ≤ 1 := (inv_le_one₀ hden).mpr (by linarith)
    have hz' : (1 - u) * m + u * y = z := by
      dsimp [u]
      field_simp [sub_ne_zero.mpr he]
      ring
    have h := hconc.2 (mem_univ m) (mem_univ y)
      (sub_nonneg.mpr (hub.2.trans ht1)) hub.1 (show 1 - u + u = 1 by ring)
    change (1 - u) * f m + u * f y ≤ f ((1 - u) * m + u * y) at h
    rw [hz'] at h
    have htd : t * d ≤ 1 := by
      dsimp [t]
      rw [← div_eq_inv_mul, div_le_iff₀ hden]
      linarith
    apply exp_le_exp.mpr
    nlinarith only [h, (mul_le_mul_of_nonneg_right hub.2 hd).trans htd,
      show d = f m - f y from rfl]
  have hle := setIntegral_mono_on (μ := volume)
    (integrableOn_const (by simp [S] : volume S ≠ ⊤))
    hi.integrableOn (measurableSet_Icc (a := min m b) (b := max m b)) hlower
  have hmass := setIntegral_le_integral (s := S) hi (ae_of_all _ fun z ↦ (exp_pos (f z)).le)
  rw [hn] at hmass
  have hlen : max m b - min m b = t * |y - m| := by
    rw [max_sub_min_eq_abs]
    rw [show b - m = t * (y - m) by dsimp [b]; ring, abs_mul, abs_of_pos ht]
  rw [setIntegral_const, measureReal_def, volume_Icc,
    ENNReal.toReal_ofReal (sub_nonneg.mpr (le_trans (min_le_left _ _) (le_max_left _ _))),
    hlen, smul_eq_mul, exp_sub] at hle
  have hbound := hle.trans hmass
  dsimp [t] at hbound
  have he := exp_pos (1 : ℝ)
  rw [show (1 + d)⁻¹ * |y - m| * (exp (f m) / exp 1) =
      (exp (f m) * |y - m|) / (exp 1 * (1 + d)) by
        field_simp [he.ne', hden.ne']] at hbound
  simpa using (div_le_iff₀ (mul_pos he hden)).mp hbound

/-- A nonconstant log sample uniformly bounds the peak and mode of any
normalized log-concave density on each mean-log-likelihood superlevel. -/
theorem sample_superlevel {n : ℕ} (hn : 0 < n) (y : Fin n → ℝ)
    (hne : ∃ i j, y i ≠ y j) (c : ℝ) :
    ∃ B M : ℝ, ∀ {f : ℝ → ℝ} {m : ℝ},
      ConcaveOn ℝ univ f → (∀ x, f x ≤ f m) →
      Integrable (fun z ↦ exp (f z)) volume → (∫ z, exp (f z)) = 1 →
      c ≤ (∑ i, f (y i)) / n → exp c ≤ exp (f m) ∧ exp (f m) ≤ B ∧ |m| ≤ M := by
  obtain ⟨i, j, hij⟩ := hne
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let C := exp 1
  let δ := |y i - y j|
  have hδ : 0 < δ := abs_pos.mpr (sub_ne_zero.mpr hij)
  let R (H : ℝ) := 2 * C * ((1 - n * c) / H + n * (log H / H))
  have hlog : Tendsto (fun H : ℝ ↦ log H / H) atTop (𝓝 0) := by
    simpa using tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero
  have hR : Tendsto R atTop (𝓝 0) := by
    simpa [R, div_eq_mul_inv] using
      (((tendsto_inv_atTop_zero.const_mul (1 - n * c)).add (hlog.const_mul (n : ℝ))).const_mul
        (2 * C))
  obtain ⟨B₀, hB₀⟩ := eventually_atTop.mp (hR.eventually (Iio_mem_nhds hδ))
  let B := max B₀ (exp c)
  have hB : 0 < B := (exp_pos c).trans_le (le_max_right _ _)
  refine ⟨B, |y i| + C * (1 + n * (log B - c)) / exp c, ?_⟩
  intro f m hconc hmode hi hnorm hc
  let H := exp (f m)
  have hH : 0 < H := exp_pos _
  have hc' := (le_div_iff₀ hn').mp hc
  have hm : c ≤ f m := by
    have h := Finset.sum_le_sum (s := Finset.univ) (fun l _ ↦ hmode (y l))
    simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at h
    exact (mul_le_mul_iff_right₀ hn').mp (by simpa only [mul_comm] using hc'.trans h)
  have hcl : exp c ≤ H := exp_le_exp.mpr hm
  have hloss (l : Fin n) : f m - f (y l) ≤ n * (f m - c) := by
    calc
      f m - f (y l) ≤ ∑ r, (f m - f (y r)) :=
        Finset.single_le_sum (fun r _ ↦ sub_nonneg.mpr (hmode (y r))) (Finset.mem_univ l)
      _ = n * f m - ∑ r, f (y r) := by
        simp [Finset.sum_sub_distrib, nsmul_eq_mul]
      _ ≤ _ := by nlinarith only [hc']
  have hd (l : Fin n) : H * |y l - m| ≤ C * (1 + n * (log H - c)) := by
    refine (peak_mul_distance_le hconc (hmode (y l)) hi hnorm).trans ?_
    have h : f m - f (y l) ≤ n * (log H - c) := by
      rw [show log H = f m from log_exp _]
      exact hloss l
    exact mul_le_mul_of_nonneg_left (add_le_add_right h 1) (exp_pos _).le
  have hpeak : H ≤ B := by
    by_contra! h
    have hRlt : R H < δ := hB₀ H ((le_max_left _ _).trans h.le)
    have htri : δ ≤ |y i - m| + |y j - m| := by
      dsimp [δ]
      rw [show y i - y j = (y i - m) + (m - y j) by ring]
      simpa only [abs_sub_comm m (y j)] using abs_add_le (y i - m) (m - y j)
    have he : H * δ ≤ 2 * C * (1 + n * (log H - c)) := by
      have h := mul_le_mul_of_nonneg_left htri hH.le
      rw [mul_add] at h
      linarith only [h, hd i, hd j]
    have hr : R H = (2 * C * (1 + n * (log H - c))) / H := by
      dsimp [R]
      ring
    exact hRlt.not_ge (hr ▸ (le_div_iff₀ hH).mpr (by simpa only [mul_comm] using he))
  refine ⟨hcl, hpeak, ?_⟩
  have hd' : |y i - m| ≤ C * (1 + n * (log B - c)) / exp c := by
    refine (le_div_iff₀ (exp_pos c)).mpr ?_
    calc
      |y i - m| * exp c ≤ H * |y i - m| := by
        rw [mul_comm H]
        exact mul_le_mul_of_nonneg_left hcl (abs_nonneg _)
      _ ≤ C * (1 + n * (log H - c)) := hd i
      _ ≤ _ := by
        gcongr
  have h := abs_add_le (y i) (m - y i)
  rw [show y i + (m - y i) = m by ring, abs_sub_comm m] at h
  exact h.trans (add_le_add_right hd' _)

end UnivariateML.GBTLogConcave
end
