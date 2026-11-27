module

public import UnivariateML.Family.AllFreeGenGamma
public import Mathlib.Analysis.Calculus.LocalExtr.Rolle
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.Series
import Mathlib.Analysis.Real.Pi.Bounds
import Mathlib.Analysis.SpecialFunctions.Stirling

set_option linter.style.header false

/-! # Generalized gamma at held log mode

The signed chart uses gamma shape `k` and the positive rate `A = k t`,
where `t` is the absolute log power.

At held location, the rate bounds and the normal-seam continuity
control simultaneous scale and shape changes. The moving-location
path records the contact penalty explicitly.

## Deviations

The signed stationary-point criterion uses every finite stationary
point. Finiteness of the signed lists and the almost-everywhere bridge
are separate. The weighted two-level bound concerns the standardized
signed profile.
-/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.Shape3GenGamma
noncomputable section

private theorem log_density_chart {k A ε μ y : ℝ} (hk : 0 < k) (hA : 0 < A)
    (hε : |ε| = 1) :
    GenGammaTwoPoint.logDensity μ (sqrt k / A) (ε / sqrt k) y =
      log A + k * log k - log (Gamma (k + 1)) +
        (A * (ε * (y - μ)) - k * exp (A * (ε * (y - μ)) / k)) := by
  have hs : 0 < sqrt k := sqrt_pos.mpr hk
  have hε0 : ε ≠ 0 := by
    intro h
    simp [h] at hε
  have hε2 : ε ^ 2 = 1 := by rw [← sq_abs, hε, one_pow]
  have hshape : 1 / (ε / sqrt k) ^ 2 = k := by
    rw [div_pow, hε2, sq_sqrt hk.le, div_div_eq_mul_div]
    simp
  have habs : |ε / sqrt k| / (sqrt k / A) = A / k := by
    rw [abs_div, hε, abs_of_pos hs]
    conv_rhs => rw [← sq_sqrt hk.le]
    field_simp
  have hpower : ε / sqrt k / (sqrt k / A) * (y - μ) =
      A * (ε * (y - μ)) / k := by
    conv_rhs => rw [← sq_sqrt hk.le]
    field_simp
  have hΓ : log (Gamma (k + 1)) = log k + log (Gamma k) := by
    rw [Gamma_add_one hk.ne', log_mul hk.ne' (Gamma_pos_of_pos hk).ne']
  simp only [GenGammaTwoPoint.logDensity, div_ne_zero hε0 hs.ne', ite_false,
    hshape, habs, hpower, log_div hA.ne' hk.ne', hΓ]
  field_simp
  ring

private theorem small_shape_normalization {ι : Type*} {l : Filter ι} {k : ι → ℝ}
    (hk : Tendsto k l (𝓝[>] 0)) :
    Tendsto (fun a ↦ k a * log (k a) - log (Gamma (k a + 1))) l (𝓝 0) := by
  have h0 : Tendsto k l (𝓝 0) := hk.mono_right nhdsWithin_le_nhds
  have h1 : Tendsto (fun a ↦ k a + 1) l (𝓝 1) := by simpa using h0.add_const 1
  have hg := (Real.hasDerivAt_Gamma_of_pos (s := 1) one_pos).continuousAt.tendsto.comp h1
  have hkl : Tendsto (fun a ↦ k a * log (k a)) l (𝓝 0) := by
    simpa [mul_comm, Function.comp_def] using
      (tendsto_log_mul_rpow_nhdsGT_zero (r := 1) one_pos).comp hk
  simpa using hkl.sub (hg.log (by simp))

private theorem exponential_penalty {ι : Type*} {l : Filter ι}
    {k A : ι → ℝ} {a z : ℝ}
    (hk : Tendsto k l (𝓝[>] 0)) (hA : Tendsto A l (𝓝 a)) (ha : 0 < a) :
    (z ≤ 0 → Tendsto (fun j ↦ k j * exp (A j * z / k j)) l (𝓝 0)) ∧
    (0 < z → Tendsto (fun j ↦ k j * exp (A j * z / k j)) l atTop) := by
  have hkpos := hk.eventually self_mem_nhdsWithin
  have hApos := hA.eventually (Ioi_mem_nhds ha)
  constructor
  · intro hz
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds
      (hk.mono_right nhdsWithin_le_nhds) ?_ ?_
    · filter_upwards [hkpos] with j hj
      exact mul_nonneg hj.le (exp_pos _).le
    · filter_upwards [hkpos, hApos] with j hj hAj
      simpa using mul_le_mul_of_nonneg_left
        (exp_le_one_iff.mpr (div_nonpos_of_nonpos_of_nonneg
          (mul_nonpos_of_nonneg_of_nonpos hAj.le hz) hj.le)) hj.le
  · intro hz
    have hc : 0 < a / 2 * z := by positivity
    have ht := (tendsto_inv_nhdsGT_zero.comp hk).const_mul_atTop hc
    have he := (tendsto_exp_div_pow_atTop 1).comp ht
    have hscaled := he.const_mul_atTop hc
    refine tendsto_atTop_mono' l ?_ hscaled
    filter_upwards [hkpos, hA.eventually (Ioi_mem_nhds (show a / 2 < a by linarith))]
      with j hj hAj
    change a / 2 * z * (exp ((a / 2 * z) * (k j)⁻¹) /
      ((a / 2 * z) * (k j)⁻¹) ^ (1 : ℕ)) ≤ k j * exp (A j * z / k j)
    have heq : a / 2 * z * (exp ((a / 2 * z) * (k j)⁻¹) /
      ((a / 2 * z) * (k j)⁻¹) ^ (1 : ℕ)) = k j * exp ((a / 2 * z) * (k j)⁻¹) := by
      field_simp [hc.ne', hj.ne']
    rw [heq]
    gcongr
    rw [← div_eq_mul_inv]
    gcongr

/-- In the small-gamma-shape chart, every convergent positive finite rate
gives the full endpoint trace if supported and vanishing likelihood otherwise. -/
private theorem held_mode_small_shape_limit {ι : Type*} {l : Filter ι}
    {n : ℕ} (hn : 0 < n) (z : Fin n → ℝ)
    {k A : ι → ℝ} {a : ℝ}
    (hk : Tendsto k l (𝓝[>] 0)) (hA : Tendsto A l (𝓝 a)) (ha : 0 < a) :
    let L := fun j ↦ log (A j) + k j * log (k j) - log (Gamma (k j + 1)) +
      (∑ i : Fin n, (A j * z i - k j * exp (A j * z i / k j))) / n
    ((∀ i, z i ≤ 0) → Tendsto L l (𝓝 (log a + (∑ i : Fin n, a * z i) / n))) ∧
    ((∃ i, 0 < z i) → Tendsto L l atBot) := by
  dsimp only
  have hnorm := small_shape_normalization hk
  have hlog := hA.log ha.ne'
  have hlinear : Tendsto (fun j ↦ ∑ i : Fin n, A j * z i) l (𝓝 (∑ i : Fin n, a * z i)) :=
    tendsto_finsetSum Finset.univ fun i _ ↦ hA.mul_const (z i)
  have hsplit (j : ι) : (∑ i : Fin n, (A j * z i - k j * exp (A j * z i / k j))) =
      (∑ i : Fin n, A j * z i) - ∑ i : Fin n, k j * exp (A j * z i / k j) :=
    Finset.sum_sub_distrib _ _
  simp only [hsplit, sub_div]
  have hbase := (hlog.add hnorm).add (hlinear.div_const (n : ℝ))
  have heq (j : ι) :
      log (A j) + k j * log (k j) - log (Gamma (k j + 1)) +
        ((∑ i : Fin n, A j * z i) / n - (∑ i : Fin n, k j * exp (A j * z i / k j)) / n) =
      log (A j) + (k j * log (k j) - log (Gamma (k j + 1))) +
        (∑ i : Fin n, A j * z i) / n - (∑ i : Fin n, k j * exp (A j * z i / k j)) / n := by ring
  simp only [heq]
  constructor
  · intro hz
    have hsum : Tendsto (fun j ↦ ∑ i : Fin n, k j * exp (A j * z i / k j)) l (𝓝 0) := by
      simpa using tendsto_finsetSum Finset.univ fun i _ ↦
        (exponential_penalty (z := z i) hk hA ha).1 (hz i)
    simpa using hbase.sub (hsum.div_const (n : ℝ))
  · rintro ⟨i, hi⟩
    have hsum : Tendsto (fun j ↦ ∑ v : Fin n, k j * exp (A j * z v / k j)) l atTop := by
      refine tendsto_atTop_mono' l ?_ ((exponential_penalty hk hA ha).2 hi)
      filter_upwards [hk.eventually self_mem_nhdsWithin] with j hj
      exact Finset.single_le_sum (fun v _ ↦ mul_nonneg hj.le (exp_pos (A j * z v / k j)).le)
        (Finset.mem_univ i)
    have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
    have hneg := tendsto_neg_atTop_atBot.comp (Filter.Tendsto.atTop_div_const hnR hsum)
    simpa [sub_eq_add_neg] using hbase.add_atBot hneg

/-- Positive finite limiting rates give the power or Pareto trace on the
compatible half-line and zero likelihood off it. Zero residuals are included. -/
theorem held_location_small_shape_limit {ι : Type*} {l : Filter ι}
    {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ ε : ℝ) (hε : |ε| = 1)
    {k A : ι → ℝ} {a : ℝ}
    (hk : Tendsto k l (𝓝[>] 0)) (hA : Tendsto A l (𝓝 a)) (ha : 0 < a) :
    let z := fun i : Fin n ↦ ε * (log (x i) - μ)
    let L := fun j ↦ GenGammaTwoPoint.sampleLogLik x (μ, sqrt (k j) / A j, ε / sqrt (k j))
    ((∀ i, z i ≤ 0) →
      Tendsto L l (𝓝 (log a + (∑ i : Fin n, (a * z i - log (x i))) / n))) ∧
    ((∃ i, 0 < z i) → Tendsto L l atBot) := by
  let z := fun i : Fin n ↦ ε * (log (x i) - μ)
  let M := fun j ↦ log (A j) + k j * log (k j) - log (Gamma (k j + 1)) +
    (∑ i : Fin n, (A j * z i - k j * exp (A j * z i / k j))) / n
  let J := (∑ i : Fin n, log (x i)) / (n : ℝ)
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have he : (fun j ↦ M j - J) =ᶠ[l]
      (fun j ↦ GenGammaTwoPoint.sampleLogLik x (μ, sqrt (k j) / A j, ε / sqrt (k j))) := by
    filter_upwards [hk.eventually self_mem_nhdsWithin, hA.eventually (Ioi_mem_nhds ha)]
      with j hj hAj
    simp only [GenGammaTwoPoint.sampleLogLik, log_density_chart hj hAj hε, M, J, z,
      Finset.sum_sub_distrib, Finset.sum_add_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    field_simp
  obtain ⟨hs, hu⟩ := held_mode_small_shape_limit hn z hk hA ha
  constructor
  · intro hz
    simpa only [Finset.sum_sub_distrib, sub_div, add_sub_assoc] using
      ((hs hz).sub_const J).congr' he
  · intro hz
    refine Filter.Tendsto.congr' he ?_
    simpa only [M, sub_eq_add_neg] using tendsto_atBot_add_const_right _ (-J) (hu hz)

/-- Along either signed moving-mode face, the limiting contact penalty is
the limit of the scaled exponential at the endpoint. A zero penalty gives
the full power or Pareto endpoint density. -/
theorem moving_mode_supported_face {ι : Type*} {l : Filter ι} {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (μ₀ ε : ℝ) (hε : |ε| = 1)
    {k A μ : ι → ℝ} {a c : ℝ}
    (hk : Tendsto k l (𝓝[>] 0)) (hA : Tendsto A l (𝓝 a)) (ha : 0 < a)
    (hμ : Tendsto μ l (𝓝 μ₀))
    (hc : Tendsto (fun j ↦ k j * exp (A j * (ε * (μ₀ - μ j)) / k j)) l (𝓝 c))
    (hs : ∀ i, ε * (log (x i) - μ₀) ≤ 0) :
    Tendsto (fun j ↦ GenGammaTwoPoint.sampleLogLik x (μ j, sqrt (k j) / A j, ε / sqrt (k j))) l
      (𝓝 (log a + (∑ i : Fin n,
        (a * (ε * (log (x i) - μ₀)) - log (x i) -
          (if ε * (log (x i) - μ₀) = 0 then c else 0))) / n)) := by
  let z := fun i : Fin n ↦ ε * (log (x i) - μ₀)
  let Δ := fun j ↦ ε * (μ₀ - μ j)
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hnorm := (hA.log ha.ne').add (small_shape_normalization hk)
  have hpoint (i : Fin n) :
      Tendsto (fun j ↦ A j * (z i + Δ j) - log (x i) -
        k j * exp (A j * (z i + Δ j) / k j)) l
        (𝓝 (a * z i - log (x i) - (if z i = 0 then c else 0))) := by
    have hd : Tendsto Δ l (𝓝 0) := by
      simpa only [Δ, sub_self, mul_zero] using (hμ.const_sub μ₀).const_mul ε
    have hl : Tendsto (fun j ↦ A j * (z i + Δ j) - log (x i)) l
        (𝓝 (a * z i - log (x i))) := by
      simpa only [add_zero] using (hA.mul (hd.const_add (z i))).sub_const (log (x i))
    have hp : Tendsto (fun j ↦ k j * exp (A j * (z i + Δ j) / k j)) l
        (𝓝 (if z i = 0 then c else 0)) := by
      by_cases hi : z i = 0
      · simpa only [hi, zero_add, ite_true, Δ] using hc
      · have hz : z i < 0 := lt_of_le_of_ne (hs i) hi
        have hslope : Tendsto (fun j ↦ A j * z i / k j) l atBot := by
          have hB := (hA.mul_const (z i)).eventually
            (Iio_mem_nhds (show a * z i < a * z i / 2 by nlinarith [mul_neg_of_pos_of_neg ha hz]))
          have hcmp := (tendsto_inv_nhdsGT_zero.comp hk).const_mul_atTop_of_neg
            (div_neg_of_neg_of_pos (mul_neg_of_pos_of_neg ha hz) (by norm_num : (0 : ℝ) < 2))
          refine tendsto_atBot_mono' _ ?_ hcmp
          filter_upwards [hB, hk.eventually self_mem_nhdsWithin] with j hj hjk
          exact mul_le_mul_of_nonneg_right hj.le (inv_pos.mpr hjk).le
        have hzero := (tendsto_exp_atBot.comp hslope).mul hc
        simp only [zero_mul, ite_eq_right hi] at hzero ⊢
        refine hzero.congr fun j ↦ ?_
        change exp (A j * z i / k j) * (k j * exp (A j * Δ j / k j)) = _
        rw [show A j * (z i + Δ j) / k j = A j * z i / k j + A j * Δ j / k j by ring,
          exp_add]
        ring
    exact hl.sub hp
  have hsum := tendsto_finsetSum Finset.univ fun i _ ↦ hpoint i
  have h := hnorm.add (hsum.div_const (n : ℝ))
  simp only [add_zero, z] at h
  refine h.congr' ?_
  filter_upwards [hk.eventually self_mem_nhdsWithin, hA.eventually (Ioi_mem_nhds ha)] with j hj hAj
  have he (i : Fin n) : z i + Δ j = ε * (log (x i) - μ j) := by dsimp [z, Δ]; ring
  simp only [GenGammaTwoPoint.sampleLogLik, log_density_chart hj hAj hε, he,
    Finset.sum_sub_distrib, Finset.sum_add_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, z]
  field_simp
  ring

/-- On any sample with a nonzero residual, competitive small-shape fits
have rates bounded above and bounded away from zero. -/
private theorem held_mode_small_shape_competitive_rates {ι : Type*} {l : Filter ι}
    {n : ℕ} (hn : 0 < n) (z : Fin n → ℝ) (hz : ∃ i, z i ≠ 0)
    {k A : ι → ℝ} (hk : Tendsto k l (𝓝[>] 0))
    (hA : ∀ᶠ j in l, 0 < A j) (b : ℝ)
    (hb : ∀ᶠ j in l, b ≤ log (A j) + k j * log (k j) - log (Gamma (k j + 1)) +
      (∑ i : Fin n, (A j * z i - k j * exp (A j * z i / k j))) / n) :
    ∃ d > 0, ∃ D : ℝ, ∀ᶠ j in l, A j ∈ Icc d D := by
  let c := (∑ i : Fin n, |z i|) / (n : ℝ)
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hc : 0 < c := by
    obtain ⟨i, hi⟩ := hz
    exact div_pos ((abs_pos.mpr hi).trans_le (Finset.single_le_sum
      (fun j _ ↦ abs_nonneg (z j)) (Finset.mem_univ i))) hnR
  refine ⟨exp (b - 1), exp_pos _, (2 - b - log (c / 2)) / (c / 2), ?_⟩
  have hnorm := (small_shape_normalization hk).eventually
    (Iio_mem_nhds (show (0 : ℝ) < 1 by norm_num))
  have htwo : ∀ w : ℝ, 2 * w ≤ exp w := by
    intro w
    have h := add_one_le_exp (w - log 2)
    rw [exp_sub, exp_log (by norm_num : (0 : ℝ) < 2)] at h
    have hl := log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 2)
    linarith
  filter_upwards [hk.eventually self_mem_nhdsWithin, hA, hb, hnorm] with j hj hAj hbj hnj
  have hp (i : Fin n) :
      A j * z i - k j * exp (A j * z i / k j) ≤ -(A j * |z i|) := by
    rcases le_or_gt (z i) 0 with hi | hi
    · rw [abs_of_nonpos hi]
      have := mul_nonneg hj.le (exp_pos (A j * z i / k j)).le
      linarith
    · rw [abs_of_pos hi]
      have h := mul_le_mul_of_nonneg_left (htwo (A j * z i / k j)) hj.le
      have he : k j * (2 * (A j * z i / k j)) = 2 * (A j * z i) := by
        field_simp
      rw [he] at h
      linarith
  have hs : (∑ i : Fin n, (A j * z i - k j * exp (A j * z i / k j))) / n ≤
      -(A j * c) := by
    calc
      _ ≤ (∑ i : Fin n, -(A j * |z i|)) / n :=
        div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ hp i) hnR.le
      _ = _ := by rw [Finset.sum_neg_distrib, ← Finset.mul_sum]; dsimp [c]; ring
  have hr : b ≤ log (A j) + 1 - A j * c := by linarith
  refine ⟨(exp_le_exp.mpr ?_).trans_eq (exp_log hAj), ?_⟩
  · have := mul_nonneg hAj.le hc.le
    linarith
  · have hc2 : 0 < c / 2 := by positivity
    have ht := log_le_sub_one_of_pos (mul_pos hc2 hAj)
    rw [log_mul hc2.ne' hAj.ne'] at ht
    apply (le_div_iff₀ hc2).mpr
    linarith

private theorem fixed_shape_density_bound {σ : ℝ} (hσ : 0 < σ) (Q : ℝ) :
    ∃ c > 0, ∃ C, ∀ μ y, GenGammaTwoPoint.logDensity μ σ Q y ≤ C - c * |y - μ| := by
  by_cases hQ : Q = 0
  · refine ⟨σ⁻¹, inv_pos.mpr hσ, -log σ - log (2 * π) / 2 + 1, ?_⟩
    intro μ y
    rw [GenGammaTwoPoint.logDensity, ite_eq_left hQ]
    have he : σ⁻¹ * |y - μ| = |(y - μ) / σ| := by
      rw [abs_div, abs_of_pos hσ, div_eq_mul_inv, mul_comm]
    rw [he]
    nlinarith only [sq_abs ((y - μ) / σ), sq_nonneg (|(y - μ) / σ| - 1)]
  · let k := 1 / Q ^ 2
    let c := Q / σ
    have hk : 0 < k := div_pos one_pos (sq_pos_of_ne_zero hQ)
    have hc : 0 < |c| := abs_pos.mpr (div_ne_zero hQ hσ.ne')
    refine ⟨k * |c|, mul_pos hk hc, log (|Q| / σ) + (k * log k - k - log (Gamma k)) + k, ?_⟩
    intro μ y
    rw [GenGammaTwoPoint.logDensity, ite_eq_right hQ]
    change log (|Q| / σ) + (k * log k - k - log (Gamma k)) -
        k * (exp (c * (y - μ)) - 1 - c * (y - μ)) ≤ _
    rw [mul_assoc k, ← abs_mul]
    have h (w : ℝ) : |w| - 1 ≤ exp w - 1 - w := by
      rcases le_or_gt w 0 with hw | hw
      · rw [abs_of_nonpos hw]
        linarith [exp_pos w]
      · rw [abs_of_pos hw]
        nlinarith [quadratic_le_exp_of_nonneg hw.le, sq_nonneg (w - 1)]
    linarith only [mul_le_mul_of_nonneg_left (h (c * (y - μ))) hk.le]

/-- With shape and positive scale fixed, mean log likelihood tends to minus
infinity as location leaves compact sets, for every nonempty positive sample. -/
theorem held_shape_scale_location_no_escape {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) (Q : ℝ) :
    Tendsto (fun μ ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, Q)) (cocompact ℝ) atBot := by
  obtain ⟨c, hc, C, hb⟩ := fixed_shape_density_bound hσ Q
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  let B := (∑ i : Fin n, (C + c * |log (x i)| - log (x i))) / (n : ℝ)
  have habs : Tendsto (fun μ : ℝ ↦ |μ|) (cocompact ℝ) atTop := by
    rw [cocompact_eq_atBot_atTop, ← comap_abs_atTop]
    exact tendsto_comap
  refine tendsto_atBot_mono (fun μ ↦ ?_) (tendsto_atBot_add_const_right _ B
    (habs.const_mul_atTop_of_neg (neg_lt_zero.mpr hc)))
  have hp (i : Fin n) : GenGammaTwoPoint.logDensity μ σ Q (log (x i)) - log (x i) ≤
      C + c * |log (x i)| - log (x i) - c * |μ| := by
    have ht : |μ| ≤ |log (x i)| + |log (x i) - μ| := by
      simpa [abs_sub_comm (log (x i)) μ, sub_add_cancel, add_comm] using
        (abs_add_le (μ - log (x i)) (log (x i)))
    linarith only [hb μ (log (x i)), mul_le_mul_of_nonneg_left ht hc.le]
  calc
    _ ≤ (∑ i : Fin n, (C + c * |log (x i)| - log (x i) - c * |μ|)) / n :=
      div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ hp i) hnR.le
    _ = -c * |μ| + B := by
      simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
        Fintype.card_fin, nsmul_eq_mul, B]
      field_simp
      ring

private def loss (Q z : ℝ) : ℝ :=
  if Q = 0 then z ^ 2 / 2 else (exp (Q * z) - 1 - Q * z) / Q ^ 2

private theorem loss_nonneg (Q z : ℝ) : 0 ≤ loss Q z := by
  by_cases h : Q = 0
  · rw [loss, ite_eq_left h]
    positivity
  · rw [loss, ite_eq_right h]
    exact div_nonneg (by linarith [add_one_le_exp (Q * z)]) (sq_nonneg Q)

private theorem loss_convex (Q : ℝ) : ConvexOn ℝ univ (loss Q) := by
  change ConvexOn ℝ univ (fun z ↦ if Q = 0 then z ^ 2 / 2
    else (exp (Q * z) - 1 - Q * z) / Q ^ 2)
  by_cases hQ : Q = 0
  · simp only [ite_eq_left hQ]
    refine ⟨convex_univ, fun x _ y _ a b ha hb hab ↦ ?_⟩
    change (a * x + b * y) ^ 2 / 2 ≤ a * (x ^ 2 / 2) + b * (y ^ 2 / 2)
    have he : a * x ^ 2 + b * y ^ 2 - (a * x + b * y) ^ 2 =
        a * b * (x - y) ^ 2 := by
      linear_combination -(a * x ^ 2 + b * y ^ 2) * hab
    nlinarith only [he, mul_nonneg (mul_nonneg ha hb) (sq_nonneg (x - y))]
  · simp only [ite_eq_right hQ]
    have he : ConvexOn ℝ univ (fun z : ℝ ↦ exp (Q * z)) := by
      refine ⟨convex_univ, fun x _ y _ a b ha hb hab ↦ ?_⟩
      change exp (Q * (a * x + b * y)) ≤ a * exp (Q * x) + b * exp (Q * y)
      rw [show Q * (a * x + b * y) = a * (Q * x) + b * (Q * y) by ring]
      exact convexOn_exp.2 (mem_univ _) (mem_univ _) ha hb hab
    refine ⟨convex_univ, fun x _ y _ a b ha hb hab ↦ ?_⟩
    change _ / Q ^ 2 ≤ a * (_ / Q ^ 2) + b * (_ / Q ^ 2)
    have hh := he.2 (mem_univ x) (mem_univ y) ha hb hab
    change exp (Q * (a * x + b * y)) ≤ a * exp (Q * x) + b * exp (Q * y) at hh
    rw [← mul_div_assoc, ← mul_div_assoc, ← add_div]
    apply div_le_div_of_nonneg_right _ (sq_nonneg Q)
    linear_combination hh + hab

private theorem exp_defect_bound {M w : ℝ} (hM : 0 ≤ M) (hw : |w| ≤ M) :
    exp (-M) * w ^ 2 / 2 ≤ exp w - 1 - w := by
  by_cases hw0 : 0 ≤ w
  · have h := quadratic_le_exp_of_nonneg hw0
    have hE := exp_le_one_iff.mpr (neg_nonpos.mpr hM)
    nlinarith only [h, mul_le_mul_of_nonneg_right hE (sq_nonneg w)]
  · have hv : -w ∈ Icc (0 : ℝ) M := ⟨by linarith, neg_le.mpr (abs_le.mp hw).1⟩
    let f := fun v : ℝ ↦ exp (-v) - 1 + v - exp (-M) * v ^ 2 / 2
    have hd (v : ℝ) : HasDerivAt f (-exp (-v) + 1 - exp (-M) * v) v := by
      refine (((((hasDerivAt_id v).neg.exp).sub_const 1).add (hasDerivAt_id v)).sub
        ((((hasDerivAt_id v).pow 2).const_mul (exp (-M))).div_const 2)).congr_deriv ?_
      simp only [Pi.neg_apply, id_eq]
      ring
    have hD (v : ℝ) (h : v ∈ Icc (0 : ℝ) M) : 0 ≤ -exp (-v) + 1 - exp (-M) * v := by
      have hE := exp_le_exp.mpr (neg_le_neg h.2)
      have h1 := mul_le_mul_of_nonneg_left (add_one_le_exp v) (exp_pos (-v)).le
      have he : exp (-v) * exp v = 1 := by rw [← exp_add, neg_add_cancel, exp_zero]
      rw [he] at h1
      nlinarith only [h1, mul_le_mul_of_nonneg_right hE h.1]
    have hm := monotoneOn_of_deriv_nonneg (convex_Icc (0 : ℝ) M)
      (fun v _ ↦ (hd v).continuousAt.continuousWithinAt)
      (fun v _ ↦ (hd v).differentiableAt.differentiableWithinAt) (fun v hv ↦ by
        rw [(hd v).deriv]
        exact hD v (interior_subset hv))
    have h := hm (show (0 : ℝ) ∈ Icc 0 M from ⟨le_rfl, hM⟩) hv hv.1
    dsimp [f] at h
    simp only [neg_zero, exp_zero, neg_neg, neg_sq] at h
    linarith only [h]

private theorem loss_uniform {Q M : ℝ} (hM : 0 ≤ M) (hQ : |Q| ≤ M) (z : ℝ) :
    exp (-M) / 2 * (|z| - 1) ≤ loss Q z := by
  let c := exp (-M) / 2
  have hc : 0 < c := half_pos (exp_pos _)
  by_cases hz : |z| ≤ 1
  · exact (mul_nonpos_of_nonneg_of_nonpos hc.le (sub_nonpos.mpr hz)).trans (loss_nonneg Q z)
  have hz1 : 1 < |z| := lt_of_not_ge hz
  have hzpos := zero_lt_one.trans hz1
  let v := z / |z|
  have hv : |v| = 1 := by dsimp [v]; rw [abs_div, abs_abs, div_self hzpos.ne']
  have hV : c ≤ loss Q v := by
    by_cases h0 : Q = 0
    · simp only [loss, ite_eq_left h0]
      have he : v ^ 2 = 1 := by rw [← sq_abs, hv, one_pow]
      rw [he]
      exact div_le_div_of_nonneg_right (exp_le_one_iff.mpr (neg_nonpos.mpr hM)) (by norm_num)
    · simp only [loss, ite_eq_right h0]
      apply (le_div_iff₀ (sq_pos_of_ne_zero h0)).2
      have h := exp_defect_bound hM (show |Q * v| ≤ M by rw [abs_mul, hv, mul_one]; exact hQ)
      have he : v ^ 2 = 1 := by rw [← sq_abs, hv, one_pow]
      rw [mul_pow, he, mul_one] at h
      simpa only [c, div_mul_eq_mul_div] using h
  have hα : |z|⁻¹ ≤ 1 := inv_le_one_of_one_le₀ hz1.le
  have hcv := (loss_convex Q).2 (mem_univ z) (mem_univ 0)
    (inv_nonneg.mpr hzpos.le) (sub_nonneg.mpr hα) (by ring : |z|⁻¹ + (1 - |z|⁻¹) = 1)
  change loss Q (|z|⁻¹ * z + (1 - |z|⁻¹) * 0) ≤
    |z|⁻¹ * loss Q z + (1 - |z|⁻¹) * loss Q 0 at hcv
  have h0 : loss Q 0 = 0 := by simp [loss]
  simp only [h0, mul_zero, add_zero] at hcv
  have hh : |z|⁻¹ * z = v := by dsimp [v]; ring
  rw [hh] at hcv
  have hcv' := mul_le_mul_of_nonneg_left (hV.trans hcv) hzpos.le
  rw [← mul_assoc, mul_inv_cancel₀ hzpos.ne', one_mul] at hcv'
  change c * (|z| - 1) ≤ _
  linarith only [hcv', hc]

private theorem log_density_peak {σ : ℝ} (hσ : 0 < σ) (Q μ y : ℝ) :
    GenGammaTwoPoint.logDensity μ σ Q y ≤ 1 - log σ - loss Q ((y - μ) / σ) := by
  by_cases hQ : Q = 0
  · rw [GenGammaTwoPoint.logDensity, ite_eq_left hQ, loss, ite_eq_left hQ]
    have h := log_nonneg (show 1 ≤ 2 * π by linarith [Real.pi_gt_three])
    linarith
  · let k := 1 / Q ^ 2
    have hk : 0 < k := div_pos one_pos (sq_pos_of_ne_zero hQ)
    have hQp : 0 < |Q| := abs_pos.mpr hQ
    have h := GenGammaTwoPoint.profile_lt_face hQp hk
    have hcos : GenGammaTwoPoint.cumulant |Q| ≤ |Q| ^ 2 / 2 := by
      unfold GenGammaTwoPoint.cumulant
      simpa only [log_exp] using log_le_log (cosh_pos _) (cosh_le_exp_half_sq |Q|)
    have he : k * (|Q| ^ 2 / 2) = 1 / 2 := by dsimp [k]; rw [sq_abs]; field_simp
    have hpeak : log |Q| + (k * log k - k - log (Gamma k)) ≤ 1 := by
      unfold GenGammaTwoPoint.profile at h
      linarith only [h, he, mul_le_mul_of_nonneg_left hcos hk.le]
    rw [GenGammaTwoPoint.logDensity, ite_eq_right hQ, loss, ite_eq_right hQ, log_div hQp.ne' hσ.ne']
    change _ + (k * log k - k - log (Gamma k)) - _ ≤ _
    have hz : Q / σ * (y - μ) = Q * ((y - μ) / σ) := by ring
    rw [hz]
    let w := Q * ((y - μ) / σ)
    have hw : (exp w - 1 - w) / Q ^ 2 = k * (exp w - 1 - w) := by
      dsimp [k]
      ring
    change log |Q| - log σ + (k * log k - k - log (Gamma k)) -
      k * (exp w - 1 - w) ≤ 1 - log σ - (exp w - 1 - w) / Q ^ 2
    rw [hw]
    linarith only [hpeak]

/-- In a bounded-shape strip, any competitive scale at a held mode with
a nonzero residual lies in a fixed compact positive interval. This bound
is uniform up to and including Q = 0. -/
private theorem held_mode_bounded_shape_competitive_scales {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) {μ M : ℝ} (hM : 0 ≤ M) (hx : ∃ i, log (x i) ≠ μ) (b : ℝ) :
    ∃ lo hi : ℝ, 0 < lo ∧ ∀ σ > 0, ∀ Q, |Q| ≤ M →
      b ≤ GenGammaTwoPoint.sampleLogLik x (μ, σ, Q) → σ ∈ Icc lo hi := by
  obtain ⟨i, hi⟩ := hx
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  let J := (∑ j : Fin n, log (x j)) / (n : ℝ)
  let c := exp (-M) / 2 / (n : ℝ)
  let d := c * |log (x i) - μ|
  have hc : 0 < c := by dsimp [c]; positivity
  have hd : 0 < d := mul_pos hc (abs_pos.mpr (sub_ne_zero.mpr hi))
  let hiσ := exp (1 - J - b)
  let B := 1 - J + c - log (d / 2) - 1 - b
  let lo := (d / 2) / max 1 B
  have hlo : 0 < lo := div_pos (half_pos hd) (zero_lt_one.trans_le (le_max_left _ _))
  refine ⟨lo, hiσ, hlo, fun σ hσ Q hQ hb ↦ ?_⟩
  have hsum := Finset.sum_le_sum (s := Finset.univ) fun j _ ↦
    sub_le_sub_right (log_density_peak hσ Q μ (log (x j))) (log (x j))
  simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul] at hsum
  have hlb : b ≤ (1 - log σ) - J -
      (∑ j : Fin n, loss Q ((log (x j) - μ) / σ)) / n := by
    simp only [GenGammaTwoPoint.sampleLogLik, Finset.sum_sub_distrib] at hb
    have hl := hb.trans (div_le_div_of_nonneg_right hsum hnR.le)
    rw [sub_div, sub_div, ← mul_sub, mul_comm (n : ℝ) (1 - log σ),
      mul_div_cancel_right₀ _ hnR.ne'] at hl
    dsimp [J]
    linarith only [hl]
  have hsum0 : 0 ≤ ∑ j : Fin n, loss Q ((log (x j) - μ) / σ) :=
    Finset.sum_nonneg fun j _ ↦ loss_nonneg Q _
  have hupper : σ ≤ hiσ := by
    change σ ≤ exp (1 - J - b)
    rw [← exp_log hσ, exp_le_exp]
    linarith only [hlb, div_nonneg hsum0 hnR.le]
  have hsum1 : d / σ - c ≤
      (∑ j : Fin n, loss Q ((log (x j) - μ) / σ)) / n := by
    have he := loss_uniform hM hQ ((log (x i) - μ) / σ)
    rw [abs_div, abs_of_pos hσ] at he
    have ho := Finset.single_le_sum (f := fun j : Fin n ↦ loss Q ((log (x j) - μ) / σ))
      (fun j _ ↦ loss_nonneg Q _) (Finset.mem_univ i)
    apply (le_div_iff₀ hnR).2
    have hc' : c * n = exp (-M) / 2 := by dsimp [c]; field_simp
    have heq : (d / σ - c) * n = exp (-M) / 2 * (|log (x i) - μ| / σ - 1) := by
      dsimp only [d]
      linear_combination (|log (x i) - μ| / σ - 1) * hc'
    rw [heq]
    exact he.trans ho
  have ht := log_le_sub_one_of_pos (div_pos (half_pos hd) hσ)
  rw [log_div (half_pos hd).ne' hσ.ne'] at ht
  have hbound : d / 2 / σ ≤ B := by
    dsimp [B]
    linarith only [ht, hlb, hsum1, show d / 2 / σ = (d / σ) / 2 by ring]
  refine ⟨?_, hupper⟩
  apply (div_le_iff₀ (zero_lt_one.trans_le (le_max_left (1 : ℝ) B))).2
  rw [mul_comm]
  exact (div_le_iff₀ hσ).1 (hbound.trans (le_max_right _ _))

private def modeDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ

private def modeObjective {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) (p : modeDomain) : ℝ :=
  GenGammaTwoPoint.sampleLogLik x (μ, p.val.1, p.val.2)

private def faceValue {n : ℕ} (x : Fin n → ℝ) (μ ε a : ℝ) : ℝ :=
  log a + (∑ i : Fin n, (a * (ε * (log (x i) - μ)) - log (x i))) / n

private theorem mode_escape_ultrafilter {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    (h : ∃ i, log (x i) ≠ μ) {ι : Type*} (u : Ultrafilter ι) (p : ι → modeDomain)
    (hp : Tendsto p (u : Filter ι) (cocompact modeDomain)) (b : ℝ)
    (hcomp : ∀ᶠ j in (u : Filter ι), b ≤ modeObjective x μ (p j)) :
    ∃ ε : ℝ, |ε| = 1 ∧ ∃ a > 0,
      (∀ i, ε * (log (x i) - μ) ≤ 0) ∧
      Tendsto (modeObjective x μ ∘ p) (u : Filter ι) (𝓝 (faceValue x μ ε a)) := by
  let D := modeDomain
  let L := modeObjective x μ
  let F : Filter ι := u
  let σ := fun j ↦ (p j).val.1
  let Q := fun j ↦ (p j).val.2
  have hσ (j : ι) : 0 < σ j := (p j).property.1
  have hQ : Tendsto (fun j ↦ |Q j|) F atTop := by
    apply tendsto_atTop.2
    intro M
    let M' := max M 0
    obtain ⟨lo, hi, hlo, hbound⟩ := held_mode_bounded_shape_competitive_scales
      hn x (le_max_right M 0) h b
    let R := Icc lo hi ×ˢ Icc (-M') M'
    have hsub : R ⊆ D := fun q hq ↦ ⟨hlo.trans_le hq.1.1, mem_univ _⟩
    have hR : IsCompact (Subtype.val ⁻¹' R : Set D) :=
      Topology.IsEmbedding.subtypeVal.isInducing.isCompact_preimage'
        (isCompact_Icc.prod isCompact_Icc) (by simpa only [Subtype.range_coe] using hsub)
    filter_upwards [hp.eventually hR.compl_mem_cocompact, hcomp] with j hout hj
    by_contra! ht
    have ht' : |Q j| ≤ M' := ht.le.trans (le_max_left _ _)
    exact hout ⟨hbound (σ j) (hσ j) (Q j) ht' hj, abs_le.mp ht'⟩
  let k := fun j ↦ (Q j ^ 2)⁻¹
  let A := fun j ↦ (σ j * |Q j|)⁻¹
  have hq0 : ∀ᶠ j in F, 0 < |Q j| :=
    hQ.eventually (eventually_gt_atTop 0)
  have hk : Tendsto k F (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_iff.mpr
    have hh := tendsto_inv_atTop_zero.comp ((tendsto_pow_atTop (by norm_num : (2 : ℕ) ≠ 0)).comp
      hQ)
    exact ⟨by simpa only [Function.comp_def, sq_abs, k] using hh,
      hq0.mono fun j hj ↦ inv_pos.mpr (sq_pos_of_ne_zero (abs_pos.mp hj))⟩
  have hA : ∀ᶠ j in F, 0 < A j := hq0.mono fun j hj ↦ inv_pos.mpr (mul_pos (hσ j) hj)
  have hsign : (∀ᶠ j in F, 0 ≤ Q j) ∨ (∀ᶠ j in F, Q j ≤ 0) :=
    Ultrafilter.eventually_or.mp (Eventually.of_forall fun j ↦ le_total 0 (Q j))
  obtain ⟨ε, hε, hεQ⟩ : ∃ ε : ℝ, |ε| = 1 ∧ (∀ᶠ j in F, Q j = ε * |Q j|) := by
    rcases hsign with hpos | hneg
    · exact ⟨1, abs_one, hpos.mono fun j hj ↦ by rw [one_mul, abs_of_nonneg hj]⟩
    · exact ⟨-1, by simp, hneg.mono fun j hj ↦ by rw [abs_of_nonpos hj]; ring⟩
  have hchart : ∀ᶠ j in F, sqrt (k j) / A j = σ j ∧ ε / sqrt (k j) = Q j := by
    filter_upwards [hq0, hεQ] with j hj hεj
    have hq : Q j ≠ 0 := abs_pos.mp hj
    have hroot : sqrt (k j) = |Q j|⁻¹ := by
      dsimp [k]
      rw [sqrt_inv, sqrt_sq_eq_abs]
    refine ⟨?_, ?_⟩
    · rw [hroot, show A j = (σ j * |Q j|)⁻¹ by rfl]
      field_simp
    · rw [hroot, div_inv_eq_mul]
      exact hεj.symm
  let z := fun i ↦ ε * (log (x i) - μ)
  have hz : ∃ i, z i ≠ 0 := by
    obtain ⟨i, hi⟩ := h
    have he : ε ≠ 0 := by intro h0; simp [h0] at hε
    exact ⟨i, mul_ne_zero he (sub_ne_zero.mpr hi)⟩
  let J := (∑ i : Fin n, log (x i)) / (n : ℝ)
  have hrew : ∀ᶠ j in F, L (p j) =
      log (A j) + k j * log (k j) - log (Gamma (k j + 1)) +
        (∑ i : Fin n, (A j * z i - k j * exp (A j * z i / k j))) / n - J := by
    have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
    filter_upwards [hk.eventually self_mem_nhdsWithin, hA, hchart] with j hj ha he
    change GenGammaTwoPoint.sampleLogLik x (μ, σ j, Q j) = _
    rw [← he.1, ← he.2]
    simp only [GenGammaTwoPoint.sampleLogLik, log_density_chart hj ha hε, J, z,
      Finset.sum_sub_distrib, Finset.sum_add_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    field_simp
  have hb : ∀ᶠ j in F, b + J ≤ log (A j) + k j * log (k j) -
      log (Gamma (k j + 1)) +
        (∑ i : Fin n, (A j * z i - k j * exp (A j * z i / k j))) / n := by
    filter_upwards [hrew, hcomp] with j hj hL
    change b ≤ L (p j) at hL
    linarith only [hj, hL]
  obtain ⟨d, hd, B, hbd⟩ := held_mode_small_shape_competitive_rates hn z hz hk hA
    (b + J) hb
  obtain ⟨a, ha, hlim⟩ := isCompact_Icc.ultrafilter_le_nhds' (u.map A) hbd
  have ha' : 0 < a := hd.trans_le ha.1
  have hAlim : Tendsto A F (𝓝 a) := hlim
  obtain ⟨hsupp, hoff⟩ := held_location_small_shape_limit hn x μ ε hε hk hAlim ha'
  have he : (fun j ↦ GenGammaTwoPoint.sampleLogLik x (μ, sqrt (k j) / A j,
      ε / sqrt (k j))) =ᶠ[F] L ∘ p := by
    filter_upwards [hchart] with j hj
    rw [Function.comp_apply, hj.1, hj.2]
    rfl
  have hsupport : ∀ i, z i ≤ 0 := by
    by_contra! hn'
    have hf := ((hoff hn').congr' he).eventually (eventually_lt_atBot b)
    obtain ⟨j, hj, hj'⟩ := (hf.and hcomp).exists
    exact hj.not_ge hj'
  exact ⟨ε, hε, a, ha', hsupport, (hsupp hsupport).congr' he⟩

/-- At a held mode, all-zero residuals allow concentration. Every finite
log-likelihood limit on any escape with a nonzero residual is a supported
power or Pareto trace at that mode. This includes all zero-residual contacts. -/
theorem held_location_escape_classification {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (hx : ∀ i, 0 < x i) (μ : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : D ↦ GenGammaTwoPoint.sampleLogLik x (μ, p.val.1, p.val.2)
    ((∀ i, log (x i) = μ) → ∀ Q,
      Tendsto (fun σ ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, Q)) (𝓝[>] 0) atTop) ∧
    ((∃ i, log (x i) ≠ μ) → ∀ {ι : Type} {l : Filter ι} [l.NeBot] {p : ι → D} {v : ℝ},
      Tendsto p l (cocompact D) → Tendsto (L ∘ p) l (𝓝 v) →
      ∃ ε : ℝ, |ε| = 1 ∧ ∃ a > 0,
        (∀ i, ε * (log (x i) - μ) ≤ 0) ∧
        v = log a + (∑ i : Fin n, (a * (ε * (log (x i) - μ)) - log (x i))) / n) := by
  intro D L
  constructor
  · intro h Q
    apply AllFreeGenGamma.constant_concentration hn μ Q
    intro i
    rw [← h i, exp_log (hx i)]
  · intro h ι l hl p v hp hv
    let u := Ultrafilter.of l
    have hu : (u : Filter ι) ≤ l := Ultrafilter.of_le l
    have hv' := hv.mono_left hu
    have hB := (hv'.eventually (Ioi_mem_nhds (show v - 1 < v by linarith))).mono
      (fun j hj ↦ le_of_lt hj)
    obtain ⟨ε, hε, a, ha, hs, hlim⟩ := mode_escape_ultrafilter hn x μ h u p
      (hp.mono_left hu) (v - 1) hB
    exact ⟨ε, hε, a, ha, hs, tendsto_nhds_unique hv' hlim⟩

/-- At positive held scale, both signed shape ends lose likelihood
uniformly over all locations. The gamma shape k tends to zero when
the absolute Prentice shape tends to infinity. -/
theorem held_scale_shape_ends {ι : Type*} {l : Filter ι} {n : ℕ}
    (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ)
    {k ε μ : ι → ℝ} (hk : Tendsto k l (𝓝[>] 0))
    (hε : ∀ᶠ j in l, |ε j| = 1) :
    Tendsto (fun j ↦ GenGammaTwoPoint.sampleLogLik x (μ j, σ, ε j / sqrt (k j)))
      l atBot := by
  let J := (∑ i : Fin n, log (x i)) / (n : ℝ)
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hg := (small_shape_normalization hk).sub
    (hk.mono_right nhdsWithin_le_nhds)
  have hroot : Tendsto (fun j ↦ sqrt (k j)) l (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_iff.mpr
    have hc := continuous_sqrt.continuousAt.tendsto.comp
      (hk.mono_right nhdsWithin_le_nhds)
    exact ⟨by simpa only [Function.comp_def, sqrt_zero] using hc,
      (hk.eventually self_mem_nhdsWithin).mono fun j hj ↦ sqrt_pos.mpr hj⟩
  have hlog := tendsto_log_nhdsGT_zero.comp hroot
  have hlim := (hlog.atBot_add hg).atBot_add
    (show Tendsto (fun _ : ι ↦ -log σ - J) l (𝓝 (-log σ - J)) from tendsto_const_nhds)
  refine tendsto_atBot_mono' _ ?_ hlim
  filter_upwards [hk.eventually self_mem_nhdsWithin, hε] with j hj hεj
  let A := sqrt (k j) / σ
  have hs : 0 < sqrt (k j) := sqrt_pos.mpr hj
  have ha : 0 < A := div_pos hs hσ
  have he : sqrt (k j) / A = σ := by dsimp [A]; field_simp
  have hp (i : Fin n) :
      GenGammaTwoPoint.logDensity (μ j) σ (ε j / sqrt (k j)) (log (x i)) - log (x i) ≤
        log A + k j * log (k j) - log (Gamma (k j + 1)) - k j - log (x i) := by
    rw [← he, log_density_chart hj ha hεj]
    let z := ε j * (log (x i) - μ j)
    change _ + (A * z - k j * exp (A * z / k j)) - log (x i) ≤ _
    have h := mul_le_mul_of_nonneg_left (add_one_le_exp (A * z / k j)) hj.le
    have hh : k j * (A * z / k j + 1) = A * z + k j := by field_simp
    rw [hh] at h
    linarith
  calc
    _ ≤ (∑ i : Fin n, (log A + k j * log (k j) -
        log (Gamma (k j + 1)) - k j - log (x i))) / n :=
      div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ hp i) hnR.le
    _ = _ := by
      simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
        nsmul_eq_mul, J, Function.comp_apply, A, log_div hs.ne' hσ.ne']
      field_simp
      ring

private theorem score_deriv (k p A y : ℝ) :
    HasDerivAt (fun t ↦ k * p - A * p * exp (p * t))
      (-(A * p ^ 2) * exp (p * y)) y := by
  refine (((((hasDerivAt_id y).const_mul p).exp).const_mul (A * p)).const_sub
    (k * p)).congr_deriv ?_
  dsimp only [id_eq]
  ring

private def stirlingMain (x : ℝ) : ℝ := (x - 1 / 2) * log x - x

private def stirlingError (x : ℝ) : ℝ := log (Gamma x) - stirlingMain x

private theorem gamma_log_step {x : ℝ} (hx : 0 < x) :
    log (Gamma (x + 1)) = log x + log (Gamma x) := by
  rw [Gamma_add_one hx.ne', log_mul hx.ne' (Gamma_pos_of_pos hx).ne']

private theorem stirling_nat :
    Tendsto (fun n : ℕ ↦ stirlingError n) atTop (𝓝 (log (2 * π) / 2)) := by
  have hs := Stirling.tendsto_stirlingSeq_sqrt_pi.log (sqrt_pos.mpr pi_pos).ne'
  have ht := hs.add_const (log 2 / 2)
  have he : log (sqrt π) + log 2 / 2 = log (2 * π) / 2 := by
    rw [log_sqrt pi_pos.le, log_mul (by norm_num : (2 : ℝ) ≠ 0) pi_pos.ne']
    ring
  rw [he] at ht
  refine ht.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℕ)] with n hn
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hg := gamma_log_step hnR
  rw [Gamma_nat_eq_factorial] at hg
  rw [Stirling.log_stirlingSeq_formula, log_mul (by norm_num : (2 : ℝ) ≠ 0) hnR.ne',
    log_div hnR.ne' (exp_ne_zero _), log_exp]
  dsimp [stirlingError, stirlingMain]
  linarith only [hg]

private theorem stirling_difference {N x : ℝ} (hN : 1 < N)
    (hl : N ≤ x) (hu : x ≤ N + 1) :
    -(1 / (N - 1) + 1 / N) ≤ stirlingError x - stirlingError N ∧
      stirlingError x - stirlingError N ≤ 1 / N := by
  rcases hl.eq_or_lt with rfl | hl
  · have h₁ : 0 ≤ 1 / (N - 1) := by positivity [sub_pos.mpr hN]
    have h₂ : 0 ≤ 1 / N := by positivity [zero_lt_one.trans hN]
    simp only [sub_self]
    constructor <;> linarith
  have hN0 : 0 < N := zero_lt_one.trans hN
  have hx : 0 < x := hN0.trans hl
  let t := x - N
  have ht : 0 < t := sub_pos.mpr hl
  have ht1 : t ≤ 1 := by dsimp [t]; linarith
  have hX : 0 < N - 1 := sub_pos.mpr hN
  have hlo := convexOn_log_Gamma.slope_mono_adjacent
    (show N - 1 ∈ Ioi 0 from hX) (show x ∈ Ioi 0 from hx) (sub_lt_self _ one_pos) hl
  have hg := gamma_log_step hX
  rw [sub_add_cancel] at hg
  simp only [Function.comp_apply, show N - (N - 1) = 1 by ring, div_one] at hlo
  rw [hg, add_sub_cancel_right, ← hg] at hlo
  have hlo' : log (N - 1) * t ≤ log (Gamma x) - log (Gamma N) :=
    (le_div_iff₀ ht).1 hlo
  have hhi : log (Gamma x) - log (Gamma N) ≤ t * log N := by
    have h := convexOn_log_Gamma.2 (show N ∈ Ioi 0 from hN0)
      (show 0 < N + 1 by positivity) (sub_nonneg.mpr ht1) ht.le
      (show 1 - t + t = 1 by ring)
    have hei : (1 - t) * N + t * (N + 1) = x := by dsimp [t]; ring
    simp only [smul_eq_mul, hei, Function.comp_apply, gamma_log_step hN0] at h
    linarith only [h]
  have hlog₁ : log x - log N ≤ t / N := by
    have hh := log_le_sub_one_of_pos (div_pos hx hN0)
    rw [log_div hx.ne' hN0.ne'] at hh
    have he : t / N = x / N - 1 := by dsimp [t]; rw [sub_div, div_self hN0.ne']
    rwa [he]
  have hlog₂ : t / x ≤ log x - log N := by
    have hh := log_le_sub_one_of_pos (div_pos hN0 hx)
    rw [log_div hN0.ne' hx.ne'] at hh
    have he : t / x = 1 - N / x := by dsimp [t]; rw [sub_div, div_self hx.ne']
    rw [he]
    linarith only [hh]
  have hlog₃ : log N - log (N - 1) ≤ 1 / (N - 1) := by
    have hh := log_le_sub_one_of_pos (div_pos hN0 hX)
    rw [log_div hN0.ne' hX.ne'] at hh
    convert hh using 1
    field_simp
    ring
  have hst : stirlingMain x - stirlingMain N =
      t * log N + (x - 1 / 2) * (log x - log N) - t := by dsimp [stirlingMain, t]; ring
  have hlm : t * log N - 1 / N ≤ stirlingMain x - stirlingMain N := by
    have h := mul_le_mul_of_nonneg_left hlog₂ (show 0 ≤ x - 1 / 2 by linarith)
    have he : (x - 1 / 2) * (t / x) = t - t / (2 * x) := by field_simp
    rw [he] at h
    have h' : t / (2 * x) ≤ 1 / N :=
      (div_le_div_of_nonneg_right ht1 (by positivity)).trans
        (div_le_div_of_nonneg_left zero_le_one hN0 (by linarith))
    linarith only [hst, h, h']
  have hum : stirlingMain x - stirlingMain N ≤ t * log N + 1 / N := by
    have h := mul_le_mul_of_nonneg_left hlog₁ (show 0 ≤ x - 1 / 2 by linarith)
    have h' : (x - 1 / 2) * (t / N) ≤ t + 1 / N := by
      have he : ((x - 1 / 2) * (t / N) - t) * N = (t - 1 / 2) * t := by
        field_simp
        dsimp [t]
        ring
      have hs : (t - 1 / 2) * t ≤ 1 := by
        nlinarith only [mul_le_mul_of_nonneg_right (show t - 1 / 2 ≤ 1 by linarith) ht.le, ht1]
      have hs' : (x - 1 / 2) * (t / N) - t ≤ 1 / N :=
        (le_div_iff₀ hN0).2 (he ▸ hs)
      linarith
    linarith only [hst, h, h']
  have hdiff : t * log N - t * log (N - 1) ≤ 1 / (N - 1) := by
    have h := mul_le_mul_of_nonneg_left hlog₃ ht.le
    have h' := mul_le_mul_of_nonneg_right ht1 (one_div_nonneg.mpr hX.le)
    nlinarith only [h, h']
  dsimp [stirlingError]
  constructor <;> linarith only [hlo', hhi, hlm, hum, hdiff]

private theorem stirling_real :
    Tendsto stirlingError atTop (𝓝 (log (2 * π) / 2)) := by
  let N := fun x : ℝ ↦ (⌊x⌋₊ : ℝ)
  have hnat := stirling_nat.comp (tendsto_nat_floor_atTop (α := ℝ))
  have hN : Tendsto N atTop atTop := tendsto_natCast_atTop_atTop.comp tendsto_nat_floor_atTop
  have hi := tendsto_inv_atTop_zero.comp hN
  have hlo := tendsto_inv_atTop_zero.comp (tendsto_atTop_add_const_right _ (-1) hN)
  refine tendsto_of_tendsto_of_tendsto_of_le_of_le'
    (g := fun x ↦ stirlingError (N x) - (1 / (N x - 1) + 1 / N x))
    (h := fun x ↦ stirlingError (N x) + 1 / N x) ?_ ?_ ?_ ?_
  · simpa only [Function.comp_def, one_div, sub_eq_add_neg, neg_zero, add_zero, N] using
      hnat.sub (hlo.add hi)
  · simpa only [Function.comp_def, one_div, add_zero, N] using hnat.add hi
  · filter_upwards [eventually_ge_atTop (0 : ℝ), hN.eventually (eventually_gt_atTop 1)]
      with x hx hNx
    have h := (stirling_difference hNx (Nat.floor_le hx) (Nat.lt_floor_add_one x).le).1
    dsimp [N] at *
    linarith only [h]
  · filter_upwards [eventually_ge_atTop (0 : ℝ), hN.eventually (eventually_gt_atTop 1)]
      with x hx hNx
    have h := (stirling_difference hNx (Nat.floor_le hx) (Nat.lt_floor_add_one x).le).2
    dsimp [N] at *
    linarith only [h]

private def peak (Q : ℝ) : ℝ :=
  if Q = 0 then -log (2 * π) / 2 else -stirlingError (1 / Q ^ 2)

private theorem peak_continuous : Continuous peak := by
  apply continuous_iff_continuousAt.mpr
  intro Q
  by_cases hQ : Q = 0
  · subst Q
    apply continuousAt_iff_punctured_nhds.mpr
    have ht : Tendsto (fun Q : ℝ ↦ Q ^ 2) (𝓝[≠] 0) (𝓝[>] 0) := by
      apply tendsto_nhdsWithin_iff.mpr
      have hc : ContinuousAt (fun t : ℝ ↦ t ^ (2 : ℕ)) 0 := (continuous_id.pow 2).continuousAt
      refine ⟨by simpa only [zero_pow (by decide : (2 : ℕ) ≠ 0)]
        using hc.tendsto.mono_left nhdsWithin_le_nhds, ?_⟩
      apply eventually_mem_nhdsWithin.mono
      intro t ht
      exact sq_pos_of_ne_zero ht
    have hs := (stirling_real.comp (tendsto_inv_nhdsGT_zero.comp ht)).neg
    rw [show peak 0 = -(log (2 * π) / 2) by simp [peak, neg_div]]
    refine hs.congr' ?_
    filter_upwards [eventually_mem_nhdsWithin (a := (0 : ℝ)) (s := {0}ᶜ)] with t ht
    simp [Function.comp_def, peak, show t ≠ 0 from ht, one_div]
  · let k := fun t : ℝ ↦ 1 / t ^ 2
    have hk : 0 < k Q := div_pos one_pos (sq_pos_of_ne_zero hQ)
    have hc : ContinuousAt k Q := continuousAt_const.div (continuousAt_id.pow 2) (pow_ne_zero _ hQ)
    have hG : ContinuousAt (fun t ↦ log (Gamma t)) (k Q) :=
      (Real.hasDerivAt_Gamma_of_pos hk).continuousAt.log (Gamma_pos_of_pos hk).ne'
    have hS : ContinuousAt stirlingError (k Q) :=
      hG.sub (((continuousAt_id.sub continuousAt_const).mul (continuousAt_log hk.ne')).sub continuousAt_id)
    have he : peak =ᶠ[𝓝 Q] (fun t ↦ -stirlingError (k t)) := by
      filter_upwards [isOpen_ne.mem_nhds hQ] with t ht
      rw [peak, ite_eq_right ht]
    exact (hS.comp hc).neg.congr he.symm

private def expDefect (w : ℝ) : ℝ :=
  if w = 0 then 1 / 2 else (exp w - 1 - w) / w ^ 2

private theorem expDefect_continuous : Continuous expDefect := by
  apply continuous_iff_continuousAt.mpr
  intro w
  by_cases hw : w = 0
  · subst w
    let r := fun t : ℝ ↦ exp t - 1 - t - t ^ 2 / 2
    have hr : Asymptotics.IsLittleO (𝓝 (0 : ℝ)) r (fun t ↦ t ^ (2 : ℕ)) := by
      convert exp_sub_sum_range_succ_isLittleO_pow 2 using 1
      funext t
      simp only [r, Finset.sum_range_succ, Finset.sum_range_zero]
      norm_num [Nat.factorial_succ]
      ring
    have h0 := (Asymptotics.isLittleO_iff_tendsto (f := r) (g := fun t ↦ t ^ (2 : ℕ))
      (fun t ht ↦ by have h := sq_eq_zero_iff.mp ht; simp [r, h])).mp hr
    have h1 := (h0.mono_left (nhdsWithin_le_nhds : 𝓝[≠] (0 : ℝ) ≤ 𝓝 0)).add_const (1 / 2)
    apply continuousAt_iff_punctured_nhds.mpr
    rw [expDefect, ite_eq_left rfl]
    simp only [zero_add] at h1
    refine h1.congr' ?_
    filter_upwards [eventually_mem_nhdsWithin (a := (0 : ℝ)) (s := {0}ᶜ)] with t ht
    have ht' : t ≠ 0 := ht
    rw [expDefect, ite_eq_right ht']
    dsimp [r]
    field_simp
    ring
  · have hr : ContinuousAt (fun t : ℝ ↦ (exp t - 1 - t) / t ^ 2) w :=
      (((continuous_exp.continuousAt).sub continuousAt_const).sub continuousAt_id).div
        (continuousAt_id.pow 2) (pow_ne_zero _ hw)
    apply hr.congr
    filter_upwards [isOpen_ne.mem_nhds hw] with t ht
    rw [expDefect, ite_eq_right ht]

private theorem loss_eq (Q z : ℝ) : loss Q z = z ^ 2 * expDefect (Q * z) := by
  by_cases hQ : Q = 0
  · simp [hQ, loss, expDefect, div_eq_mul_inv]
  · by_cases hz : z = 0
    · simp [hz, loss]
    · rw [loss, ite_eq_right hQ, expDefect, ite_eq_right (mul_ne_zero hQ hz)]
      field_simp

private theorem log_density_eq {σ : ℝ} (hσ : 0 < σ) (Q μ y : ℝ) :
    GenGammaTwoPoint.logDensity μ σ Q y = -log σ + peak Q - loss Q ((y - μ) / σ) := by
  by_cases hQ : Q = 0
  · simp only [GenGammaTwoPoint.logDensity, peak, loss, ite_eq_left hQ]
    ring
  · let k := 1 / Q ^ 2
    have hQp := abs_pos.mpr hQ
    have hk : 0 < k := div_pos one_pos (sq_pos_of_ne_zero hQ)
    have hlog : log |Q| = -(log k) / 2 := by
      rw [show k = (|Q| ^ 2)⁻¹ by rw [sq_abs]; exact one_div _, log_inv, log_pow]
      ring
    simp only [GenGammaTwoPoint.logDensity, peak, loss, ite_eq_right hQ,
      log_div hQp.ne' hσ.ne', stirlingError, stirlingMain, hlog]
    rw [show Q / σ * (y - μ) = Q * ((y - μ) / σ) by ring]
    change _ - k * _ = _
    dsimp [k]
    ring

/-- The Prentice log likelihood is continuous in all coefficients on the
positive-scale domain, including through the normal seam. -/
theorem likelihood_continuous {n : ℕ} (x : Fin n → ℝ) :
    ContinuousOn (GenGammaTwoPoint.sampleLogLik x) GenGammaTwoPoint.domain := by
  let D := GenGammaTwoPoint.domain
  unfold GenGammaTwoPoint.sampleLogLik
  apply ContinuousOn.div_const
  apply continuousOn_finsetSum
  intro i _
  let y := log (x i)
  have hμ : ContinuousOn (fun p : ℝ × ℝ × ℝ ↦ p.1) D := continuous_fst.continuousOn
  have hσ : ContinuousOn (fun p : ℝ × ℝ × ℝ ↦ p.2.1) D :=
    (continuous_fst.comp continuous_snd).continuousOn
  have hQ : ContinuousOn (fun p : ℝ × ℝ × ℝ ↦ p.2.2) D :=
    (continuous_snd.comp continuous_snd).continuousOn
  have hz := (continuousOn_const (c := y)).sub hμ |>.div hσ
    (fun p hp ↦ (show 0 < p.2.1 from hp).ne')
  have hr := expDefect_continuous.comp_continuousOn (hQ.mul hz)
  have hg := (((hσ.log (fun p hp ↦ (show 0 < p.2.1 from hp).ne')).neg.add
    (peak_continuous.comp_continuousOn hQ)).sub ((hz.pow 2).mul hr)).sub
      (continuousOn_const (c := y))
  refine hg.congr fun p hp ↦ ?_
  rw [log_density_eq (show 0 < p.2.1 from hp), loss_eq]
  rfl

private theorem peak_even (Q : ℝ) : peak |Q| = peak Q := by
  simp [peak, abs_eq_zero, sq_abs]

private theorem peak_large_shape : Tendsto peak atTop atBot := by
  let k := fun t : ℝ ↦ (t ^ 2)⁻¹
  have hk : Tendsto k atTop (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_iff.mpr
    refine ⟨tendsto_inv_atTop_zero.comp (tendsto_pow_atTop (by decide : (2 : ℕ) ≠ 0)), ?_⟩
    exact (eventually_gt_atTop (0 : ℝ)).mono fun t ht ↦ inv_pos.mpr (sq_pos_of_pos ht)
  let x := fun _ : Fin 1 ↦ (1 : ℝ)
  have h := held_scale_shape_ends (by decide : 0 < 1) x one_pos
    (ε := fun _ ↦ 1) (μ := fun _ ↦ 0) hk (Eventually.of_forall fun _ ↦ abs_one)
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
  have he : 1 / sqrt (k t) = t := by
    dsimp [k]
    rw [sqrt_inv, sqrt_sq_eq_abs, abs_of_pos ht, one_div, inv_inv]
  rw [he]
  simp [GenGammaTwoPoint.sampleLogLik, x, log_density_eq one_pos, loss]

private theorem held_scale_competitive_box {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) (b : ℝ) :
    ∃ C M : ℝ, 0 ≤ C ∧ 0 ≤ M ∧ ∀ μ Q,
      b ≤ GenGammaTwoPoint.sampleLogLik x (μ, σ, Q) →
      |μ| ≤ C ∧ |Q| ≤ M := by
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  let J := (∑ i : Fin n, log (x i)) / (n : ℝ)
  let S := fun μ Q ↦ (∑ i : Fin n, loss Q ((log (x i) - μ) / σ)) / (n : ℝ)
  have hEq (μ Q : ℝ) :
      GenGammaTwoPoint.sampleLogLik x (μ, σ, Q) = -log σ + peak Q - S μ Q - J := by
    simp only [GenGammaTwoPoint.sampleLogLik, log_density_eq hσ,
      Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
      nsmul_eq_mul, sub_div, mul_div_cancel_left₀ _ hnR.ne', J, S]
  obtain ⟨U, hU⟩ := eventually_atTop.1 (peak_large_shape.eventually
    (eventually_lt_atBot (b + log σ + J)))
  let M := max U 0
  let c := exp (-M) / 2
  let y := log (x ⟨0, hn⟩)
  let B := -log σ - J + 1 + c / n - b
  let C := max 0 (B * (n : ℝ) * σ / c + |y|)
  have hc : 0 < c := half_pos (exp_pos _)
  refine ⟨C, M, le_max_left _ _, le_max_right _ _, fun μ Q hL ↦ ?_⟩
  rw [hEq] at hL
  have hS : 0 ≤ S μ Q := by
    exact div_nonneg (Finset.sum_nonneg fun i _ ↦ loss_nonneg Q _) hnR.le
  have hM : |Q| ≤ M := by
    by_contra! h
    have hh := hU _ ((le_max_left _ _).trans h.le)
    rw [peak_even] at hh
    linarith only [hL, hh, hS]
  have hP : peak Q ≤ 1 := by
    have h := log_density_peak (σ := 1) one_pos Q 0 0
    rw [log_density_eq one_pos] at h
    linarith only [h]
  have hsum : (c * (|y - μ| / σ - 1)) / n ≤ S μ Q := by
    apply div_le_div_of_nonneg_right _ hnR.le
    have h := loss_uniform (le_max_right U (0 : ℝ)) hM ((y - μ) / σ)
    rw [abs_div, abs_of_pos hσ] at h
    exact h.trans (Finset.single_le_sum
      (f := fun i : Fin n ↦ loss Q ((log (x i) - μ) / σ))
      (fun i _ ↦ loss_nonneg Q _) (Finset.mem_univ (⟨0, hn⟩ : Fin n)))
  have hT : c * (|y - μ| / σ - 1) ≤ (B - c / n) * n := by
    apply (div_le_iff₀ hnR).1
    dsimp [B]
    linarith only [hL, hP, hsum]
  have he : (B - c / n) * n = B * n - c := by field_simp
  rw [he] at hT
  have hY : |y - μ| ≤ B * n * σ / c := by
    apply (le_div_iff₀ hc).2
    have h := (div_le_iff₀ hσ).1 (show c * |y - μ| / σ ≤ B * n by
      linarith only [hT, show c * (|y - μ| / σ - 1) = c * |y - μ| / σ - c by ring])
    linarith only [h]
  refine ⟨?_, hM⟩
  apply le_trans _ (le_max_right (0 : ℝ) _)
  have ht := abs_add_le (μ - y) y
  rw [sub_add_cancel, abs_sub_comm μ y] at ht
  linarith only [ht, hY]

/-- Every nonempty generalized-gamma sample has a finite fit with
location and positive scale held. The normal shape is included. -/
theorem held_location_scale_attains {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    ∃ Q, IsMaxOn (fun q ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, q)) univ Q := by
  let F := fun q ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, q)
  obtain ⟨_, M, _, hM, hB⟩ := held_scale_competitive_box hn x hσ (F 0)
  have h0 : 0 ∈ Icc (-M) M := ⟨neg_nonpos.mpr hM, hM⟩
  have hCont : ContinuousOn F (Icc (-M) M) :=
    (likelihood_continuous x).comp
      (continuousOn_const.prodMk (continuousOn_const.prodMk continuousOn_id))
      (fun _ _ ↦ hσ)
  obtain ⟨q, hq, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨0, h0⟩ hCont
  refine ⟨q, fun t _ ↦ ?_⟩
  by_cases h : F 0 ≤ F t
  · exact hm (abs_le.mp (hB μ t h).2)
  · exact (le_of_not_ge h).trans (hm h0)

/-- Every nonempty generalized-gamma sample has a finite free-location
and free-shape fit with positive scale held. -/
theorem held_scale_attains {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    ∃ p : ℝ × ℝ, IsMaxOn
      (fun q : ℝ × ℝ ↦ GenGammaTwoPoint.sampleLogLik x (q.1, σ, q.2)) univ p := by
  let F := fun q : ℝ × ℝ ↦ GenGammaTwoPoint.sampleLogLik x (q.1, σ, q.2)
  obtain ⟨C, M, hC, hM, hB⟩ := held_scale_competitive_box hn x hσ (F (0, 0))
  let R := Icc (-C) C ×ˢ Icc (-M) M
  have h0 : ((0, 0) : ℝ × ℝ) ∈ R := ⟨⟨neg_nonpos.mpr hC, hC⟩, neg_nonpos.mpr hM, hM⟩
  have hCont : ContinuousOn F R :=
    (likelihood_continuous x).comp
      (continuous_fst.continuousOn.prodMk (continuousOn_const.prodMk continuous_snd.continuousOn))
      (fun _ _ ↦ hσ)
  obtain ⟨p, hp, hm⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨(0, 0), h0⟩ hCont
  refine ⟨p, fun t _ ↦ ?_⟩
  by_cases h : F (0, 0) ≤ F t
  · obtain ⟨hc, hM'⟩ := hB t.1 t.2 h
    exact hm ⟨abs_le.mp hc, abs_le.mp hM'⟩
  · exact (le_of_not_ge h).trans (hm h0)

private theorem mode_eventually_below {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    (hne : ∃ i, log (x i) ≠ μ) (v : ℝ)
    (h : ∀ ε, |ε| = 1 → ∀ a > 0, (∀ i, ε * (log (x i) - μ) ≤ 0) →
      faceValue x μ ε a < v) :
    ∀ᶠ p in cocompact modeDomain, modeObjective x μ p < v := by
  by_contra H
  let P := {p : modeDomain | v ≤ modeObjective x μ p}
  have hfreq : ∃ᶠ p in cocompact modeDomain, p ∈ P := by
    exact (not_eventually.mp H).mono (fun _ hp ↦ le_of_not_gt hp)
  let F := cocompact modeDomain ⊓ 𝓟 P
  have : F.NeBot := frequently_mem_iff_neBot.mp hfreq
  let u := Ultrafilter.of F
  have hu : (u : Filter modeDomain) ≤ F := Ultrafilter.of_le F
  have hp : Tendsto id (u : Filter modeDomain) (cocompact modeDomain) :=
    tendsto_id.mono_right (hu.trans inf_le_left)
  have hcomp : ∀ᶠ p in (u : Filter modeDomain), v ≤ modeObjective x μ (id p) :=
    le_principal_iff.mp (hu.trans inf_le_right)
  obtain ⟨ε, hε, a, ha, hsupport, hv⟩ := mode_escape_ultrafilter hn x μ hne u id hp v hcomp
  exact (h ε hε a ha hsupport).not_ge (ge_of_tendsto hv hcomp)

private theorem mode_continuous {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) :
    Continuous (modeObjective x μ) := by
  apply continuousOn_univ.mp
  have h1 : Continuous (fun p : modeDomain ↦ p.val.1) := continuous_fst.comp continuous_subtype_val
  have h2 : Continuous (fun p : modeDomain ↦ p.val.2) := continuous_snd.comp continuous_subtype_val
  exact (likelihood_continuous x).comp
    ((continuous_const.prodMk (h1.prodMk h2)).continuousOn)
    (fun p _ ↦ p.property.1)

private theorem face_le_of_mode_max {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    {p : modeDomain} (hm : IsMaxOn (modeObjective x μ) univ p) {ε a : ℝ}
    (hε : |ε| = 1) (ha : 0 < a) (hside : ∀ i, ε * (log (x i) - μ) ≤ 0) :
    faceValue x μ ε a ≤ modeObjective x μ p := by
  have ht := (held_location_small_shape_limit hn x μ ε hε
    (k := id) (A := fun _ : ℝ ↦ a) (tendsto_id : Tendsto id (𝓝[>] (0 : ℝ)) (𝓝[>] (0 : ℝ)))
      tendsto_const_nhds ha).1 hside
  apply le_of_tendsto ht
  filter_upwards [eventually_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with q hq
  exact @hm ⟨(sqrt q / a, ε / sqrt q), div_pos (sqrt_pos.mpr hq) ha, mem_univ _⟩ (mem_univ _)

private theorem mode_attains_of_witness {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    (hne : ∃ i, log (x i) ≠ μ) (p : modeDomain)
    (hw : ∀ ε, |ε| = 1 → ∀ a > 0, (∀ i, ε * (log (x i) - μ) ≤ 0) →
      faceValue x μ ε a ≤ modeObjective x μ p) :
    ∃ q : modeDomain, IsMaxOn (modeObjective x μ) univ q := by
  let L := modeObjective x μ
  by_cases hmax : IsMaxOn L univ p
  · exact ⟨p, hmax⟩
  · have hbetter : ∃ q : modeDomain, L p < L q := by
      by_contra! h
      exact hmax (fun q _ ↦ h q)
    obtain ⟨q, hq⟩ := hbetter
    have hB := mode_eventually_below hn x μ hne (L q)
      (fun ε he a ha hs ↦ (hw ε he a ha hs).trans_lt hq)
    obtain ⟨K, hK, hK'⟩ := mem_cocompact.mp hB
    have hqK : q ∈ K := by
      by_contra h
      exact lt_irrefl (L q) (hK' h)
    obtain ⟨w, hw, hmw⟩ := hK.exists_isMaxOn ⟨q, hqK⟩ (mode_continuous x μ).continuousOn
    refine ⟨w, fun t _ ↦ ?_⟩
    by_cases ht : t ∈ K
    · exact hmw ht
    · exact (hK' ht).le.trans (hmw hqK)

/-- A held-mode fit attains iff a fitted normal or a finite signed
stationary point meets every compatible face value. If all residuals
vanish the likelihood is unbounded, so this case has no finite fit. -/
theorem held_location_attains_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (hx : ∀ i, 0 < x i) (μ : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : ℝ × ℝ ↦ GenGammaTwoPoint.sampleLogLik x (μ, p.1, p.2)
    (∃ p ∈ D, IsMaxOn L D p) ↔
    (∃ i, log (x i) ≠ μ) ∧
    ∃ p ∈ D,
      ((p.2 = 0 ∧ IsMaxOn (fun s ↦ L (s, 0)) (Ioi 0) p.1) ∨
        (p.2 ≠ 0 ∧ deriv (fun s ↦ L (s, p.2)) p.1 = 0 ∧
          deriv (fun q ↦ L (p.1, q)) p.2 = 0)) ∧
      ∀ ε, |ε| = 1 → ∀ a > 0, (∀ i, ε * (log (x i) - μ) ≤ 0) →
        log a + (∑ i : Fin n, (a * (ε * (log (x i) - μ)) - log (x i))) / n ≤ L p := by
  intro D L
  constructor
  · rintro ⟨p, hp, hm⟩
    have hne : ∃ i, log (x i) ≠ μ := by
      by_contra! h
      have ht := (held_location_escape_classification hn x hx μ).1 h p.2
      obtain ⟨s, hs, hval⟩ := (eventually_mem_nhdsWithin.and
        (ht.eventually (eventually_gt_atTop (L p)))).exists
      exact hval.not_ge (hm (show (s, p.2) ∈ D from ⟨hs, mem_univ _⟩))
    have hs : IsMaxOn (fun s ↦ L (s, p.2)) (Ioi 0) p.1 :=
      fun s hs ↦ hm (show (s, p.2) ∈ D from ⟨hs, mem_univ _⟩)
    have hq : IsMaxOn (fun q ↦ L (p.1, q)) univ p.2 :=
      fun q hq ↦ hm (show (p.1, q) ∈ D from ⟨hp.1, hq⟩)
    refine ⟨hne, p, hp, ?_, ?_⟩
    · by_cases h0 : p.2 = 0
      · exact Or.inl ⟨h0, by simpa only [h0] using hs⟩
      · exact Or.inr ⟨h0, (hs.isLocalMax (Ioi_mem_nhds hp.1)).deriv_eq_zero,
          (hq.isLocalMax Filter.univ_mem).deriv_eq_zero⟩
    · intro ε hε a ha hside
      let q : modeDomain := ⟨p, hp⟩
      have hm' : IsMaxOn (modeObjective x μ) univ q := fun w _ ↦ hm w.property
      exact face_le_of_mode_max hn x μ hm' hε ha hside
  · rintro ⟨hne, p, hp, _, hw⟩
    obtain ⟨q, hq⟩ := mode_attains_of_witness hn x μ hne ⟨p, hp⟩ hw
    exact ⟨q.val, q.property, fun t ht ↦ @hq ⟨t, ht⟩ (mem_univ _)⟩

private theorem stirling_step_strict {k : ℝ} (hk : 0 < k) :
    stirlingError (k + 1) < stirlingError k := by
  let f := fun u : ℝ ↦ log (1 + u) - 2 * u / (2 + u)
  have hd (u : ℝ) (hu : 0 ≤ u) :
      HasDerivAt f (u ^ 2 / ((1 + u) * (2 + u) ^ 2)) u := by
    have h1 : 0 < 1 + u := by linarith
    have h2 : 0 < 2 + u := by linarith
    convert (((hasDerivAt_const u (1 : ℝ)).add (hasDerivAt_id u)).log h1.ne').sub
      (((hasDerivAt_id u).const_mul 2).div
        ((hasDerivAt_const u (2 : ℝ)).add (hasDerivAt_id u)) h2.ne') using 1
    · rfl
    · simp only [id_eq, Pi.add_apply]
      field_simp
      ring
  have hmono : StrictMonoOn f (Ici 0) := by
    apply strictMonoOn_of_deriv_pos (convex_Ici 0)
    · exact fun u hu ↦ (hd u hu).continuousAt.continuousWithinAt
    · intro u hu
      rw [interior_Ici] at hu
      have hu' : 0 < u := hu
      rw [(hd u hu'.le).deriv]
      positivity
  have h := hmono (show (0 : ℝ) ∈ Ici 0 from le_rfl) (one_div_pos.mpr hk).le
    (one_div_pos.mpr hk)
  have hr : 2 * (1 / k) / (2 + 1 / k) = (k + 1 / 2)⁻¹ := by
    field_simp
  have hf0 : f 0 = 0 := by simp [f]
  rw [hf0] at h
  have h := sub_pos.mp h
  change 2 * (1 / k) / (2 + 1 / k) < log (1 + 1 / k) at h
  rw [hr, show 1 + 1 / k = (k + 1) / k by field_simp,
    log_div (by positivity) hk.ne'] at h
  have h' : 1 < (k + 1 / 2) * (log (k + 1) - log k) := by
    rw [← one_div] at h
    simpa only [mul_comm] using (div_lt_iff₀ (show 0 < k + 1 / 2 by positivity)).1 h
  unfold stirlingError stirlingMain
  rw [gamma_log_step hk]
  linarith only [h']

private theorem stirling_strict {k : ℝ} (hk : 0 < k) :
    log (2 * π) / 2 < stirlingError k := by
  have hlim := stirling_real.comp
    (tendsto_atTop_add_const_left atTop (k + 1) tendsto_natCast_atTop_atTop)
  have hle (n : ℕ) : stirlingError (k + 1 + n) ≤ stirlingError (k + 1) := by
    induction n with
    | zero => simp
    | succ n ih =>
      rw [Nat.cast_succ, ← add_assoc]
      exact (stirling_step_strict (by positivity)).le.trans ih
  exact (le_of_tendsto hlim (.of_forall hle)).trans_lt (stirling_step_strict hk)

private theorem peak_normal_strict {Q : ℝ} (hQ : Q ≠ 0) : peak Q < peak 0 := by
  simp only [peak, ite_eq_right hQ, ite_true]
  linarith only [stirling_strict (one_div_pos.mpr (sq_pos_of_ne_zero hQ))]

/-- If every observation is at the held log location, fixing a positive
scale leaves the normal shape as the unique maximizer. -/
theorem all_contact_held_location_scale_normal {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) {μ σ : ℝ} (hσ : 0 < σ) (hx : ∀ i, log (x i) = μ) :
    {Q : ℝ | IsMaxOn (fun q ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, q)) univ Q} = {0} := by
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have he (Q : ℝ) :
      GenGammaTwoPoint.sampleLogLik x (μ, σ, Q) = -log σ + peak Q - μ := by
    simp only [GenGammaTwoPoint.sampleLogLik, hx, log_density_eq hσ, sub_self, zero_div,
      show loss Q 0 = 0 by simp [loss], sub_zero, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul, mul_div_cancel_left₀ _ hnR]
  have hlt (Q : ℝ) (hQ : Q ≠ 0) :
      GenGammaTwoPoint.sampleLogLik x (μ, σ, Q) <
        GenGammaTwoPoint.sampleLogLik x (μ, σ, 0) := by
    rw [he, he]
    linarith only [peak_normal_strict hQ]
  ext Q
  simp only [mem_ofPred_eq, mem_singleton_iff]
  constructor
  · intro h
    by_contra hQ
    have hm : IsMaxOn (fun q ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, q)) univ Q := h
    exact (hlt Q hQ).not_ge (hm (mem_univ (0 : ℝ)))
  · intro h
    have hQ : Q = 0 := h
    subst Q
    intro q hq
    by_cases hz : q = 0
    · subst q
      exact le_rfl
    · exact (hlt q hz).le

private theorem paired_loss_ge_normal (Q z : ℝ) :
    2 * loss 0 z ≤ loss Q z + loss Q (-z) := by
  by_cases hQ : Q = 0
  · subst Q
    simp only [loss, ite_true, neg_sq]
    linarith
  · let w := Q * z
    have h (u : ℝ) (hu : 0 ≤ u) : u ^ 2 ≤ sinh u ^ 2 := by
      rcases hu.eq_or_lt with h | h
      · subst u
        simp
      · have h' := self_lt_sinh_iff.mpr h
        exact ((sq_lt_sq₀ h.le (h.trans h').le).mpr h').le
    have hS : (w / 2) ^ 2 ≤ sinh (w / 2) ^ 2 := by
      by_cases hw : 0 ≤ w / 2
      · exact h _ hw
      · simpa only [sinh_neg, neg_sq] using h (-(w / 2)) (neg_nonneg.mpr (le_of_not_ge hw))
    have hC : 1 + w ^ 2 / 2 ≤ cosh w := by
      rw [show w = 2 * (w / 2) by ring, cosh_two_mul, cosh_sq]
      nlinarith only [hS]
    simp only [loss, ite_true, ite_eq_right hQ, ← add_div, mul_neg]
    rw [show 2 * (z ^ 2 / 2) = z ^ 2 by ring]
    apply (le_div_iff₀ (sq_pos_of_ne_zero hQ)).2
    change z ^ 2 * Q ^ 2 ≤ exp w - 1 - w + (exp (-w) - 1 - -w)
    rw [cosh_eq] at hC
    dsimp [w] at *
    nlinarith only [hC]

/-- At a held log mode, sign-symmetric nonzero residuals have a single
maximizing scale and shape pair, and its shape is normal. -/
theorem held_location_symmetric_normal {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    (h : ∃ i, log (x i) ≠ μ)
    (e : Fin n ≃ Fin n) (he : ∀ i, log (x (e i)) - μ = -(log (x i) - μ)) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : ℝ × ℝ ↦ GenGammaTwoPoint.sampleLogLik x (μ, p.1, p.2)
    ∃ s > 0, {p ∈ D | IsMaxOn L D p} = {(s, 0)} := by
  intro D L
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hbetter (s Q : ℝ) (hs : 0 < s) (hq : Q ≠ 0) : L (s, Q) < L (s, 0) := by
    let z := fun i ↦ (log (x i) - μ) / s
    have hz (i : Fin n) : z (e i) = -(z i) := by dsimp [z]; rw [he, neg_div]
    have hsum := Finset.sum_le_sum (s := Finset.univ) fun i _ ↦ paired_loss_ge_normal Q (z i)
    have hneg : (∑ i, loss Q (-(z i))) = ∑ i, loss Q (z i) := by
      simp only [← hz]
      exact Equiv.sum_comp e (fun i ↦ loss Q (z i))
    rw [Finset.sum_add_distrib, ← Finset.mul_sum, hneg] at hsum
    have hloss : (∑ i, loss 0 (z i)) ≤ ∑ i, loss Q (z i) := by linarith only [hsum]
    unfold L GenGammaTwoPoint.sampleLogLik
    apply div_lt_div_of_pos_right _ hnR
    simp only [log_density_eq hs, Finset.sum_sub_distrib, Finset.sum_add_distrib,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    exact (by
      change _ + (n : ℝ) * peak Q - (∑ i, loss Q (z i)) - _ <
        _ + (n : ℝ) * peak 0 - (∑ i, loss 0 (z i)) - _
      linarith only [hloss, mul_lt_mul_of_pos_left (peak_normal_strict hq) hnR])
  obtain ⟨s, ⟨hs, hm⟩, hu⟩ :=
    AllFreeGenGamma.held_location_fixed_shape_scale hn μ 0 h
  have hm' : IsMaxOn L D (s, 0) := by
    intro p hp
    have hh : L (p.1, 0) ≤ L (s, 0) := hm hp.1
    by_cases hQ : p.2 = 0
    · have hEq : p = (p.1, 0) := Prod.ext rfl hQ
      rwa [hEq]
    · exact (hbetter p.1 p.2 hp.1 hQ).le.trans hh
  refine ⟨s, hs, ?_⟩
  ext p
  simp only [mem_ofPred_eq, mem_singleton_iff]
  constructor
  · rintro ⟨hp, hmp⟩
    have hQ : p.2 = 0 := by
      by_contra hq
      exact (hbetter p.1 p.2 hp.1 hq).not_ge (hmp (show (p.1, 0) ∈ D from ⟨hp.1, mem_univ _⟩))
    refine Prod.ext (hu p.1 ⟨hp.1, fun t ht ↦ ?_⟩) hQ
    have ht' : L (t, 0) ≤ L p := hmp (show (t, 0) ∈ D from ⟨ht, mem_univ _⟩)
    have he : p = (p.1, 0) := Prod.ext rfl hQ
    exact ht'.trans_eq (congrArg L he)
  · intro h
    subst p
    exact ⟨⟨hs, mem_univ _⟩, hm'⟩

private theorem weighted_two_level_cumulant_lower {w : ℝ} (hw : 0 ≤ w) (hw1 : w ≤ 1)
    (t : ℝ) :
    2 * min w (1 - w) * GenGammaTwoPoint.cumulant t ≤
      log (w * exp t + (1 - w) * exp (-t)) - (2 * w - 1) * t := by
  unfold GenGammaTwoPoint.cumulant
  by_cases h : w ≤ 1 / 2
  · rw [min_eq_left (by linarith)]
    have hc := strictConcaveOn_log_Ioi.concaveOn.2
      (show cosh t ∈ Ioi 0 from cosh_pos _) (show exp (-t) ∈ Ioi 0 from exp_pos _)
      (show 0 ≤ 2 * w by positivity) (show 0 ≤ 1 - 2 * w by linarith)
      (show 2 * w + (1 - 2 * w) = 1 by ring)
    simp only [smul_eq_mul, log_exp] at hc
    have he : 2 * w * cosh t + (1 - 2 * w) * exp (-t) =
        w * exp t + (1 - w) * exp (-t) := by rw [cosh_eq]; ring
    rw [he] at hc
    linarith only [hc]
  · rw [min_eq_right (by linarith)]
    have hc := strictConcaveOn_log_Ioi.concaveOn.2
      (show cosh t ∈ Ioi 0 from cosh_pos _) (show exp t ∈ Ioi 0 from exp_pos _)
      (show 0 ≤ 2 * (1 - w) by positivity [sub_nonneg.mpr hw1])
      (show 0 ≤ 2 * w - 1 by linarith)
      (show 2 * (1 - w) + (2 * w - 1) = 1 by ring)
    simp only [smul_eq_mul, log_exp] at hc
    have he : 2 * (1 - w) * cosh t + (2 * w - 1) * exp t =
        w * exp t + (1 - w) * exp (-t) := by rw [cosh_eq]; ring
    rw [he] at hc
    linarith only [hc]

private theorem gamma_profile_shape_scaling {k c : ℝ} (hk : 0 < k)
    (hc : 0 < c) (hc1 : c ≤ 1) :
    let B := fun k : ℝ ↦ k * log k - k - log (Gamma k)
    B k ≤ B (c * k) - log c := by
  intro B
  let H := fun k : ℝ ↦ k * log k - k - log (Gamma (k + 1))
  have hD (x : ℝ) (hx : 0 < x) :
      HasDerivAt H (log x - deriv (log ∘ Gamma) (x + 1)) x := by
    have hx1 : 0 < x + 1 := by positivity
    have hg := (Real.hasDerivAt_Gamma_of_pos hx1).log (Gamma_pos_of_pos hx1).ne'
    have hg' := hg.deriv ▸ hg
    have h := (((hasDerivAt_id x).mul (hasDerivAt_log hx.ne')).sub (hasDerivAt_id x)).sub
      (hg'.comp x ((hasDerivAt_id x).add_const 1))
    convert h using 1
    · funext y
      dsimp [H, Function.comp_def, Pi.sub_apply]
    · simp only [Function.comp_def, id_eq, mul_one]
      field_simp
      ring
  have hmono : AntitoneOn H (Ioi 0) := by
    apply antitoneOn_of_deriv_nonpos (convex_Ioi 0) (fun x hx ↦
      (hD x hx).continuousAt.continuousWithinAt)
      (fun x hx ↦ (hD x (interior_subset hx)).differentiableAt.differentiableWithinAt)
    intro x hx
    rw [interior_Ioi] at hx
    rw [(hD x hx).deriv, sub_nonpos]
    have hx0 : 0 < x := hx
    have hx1 : 0 < x + 1 := by positivity
    have hd := ((Real.hasDerivAt_Gamma_of_pos hx1).log (Gamma_pos_of_pos hx1).ne').differentiableAt
    have h := convexOn_log_Gamma.slope_le_deriv hx (show x + 1 ∈ Ioi 0 from hx1) (lt_add_one _) hd
    simpa only [slope_def_field, Function.comp_apply, gamma_log_step hx,
      show x + 1 - x = 1 by ring, div_one, add_sub_cancel_right] using h
  have ha := hmono (mul_pos hc hk) hk (by
    simpa only [one_mul] using mul_le_mul_of_nonneg_right hc1 hk.le)
  simp only [H, gamma_log_step hk, gamma_log_step (mul_pos hc hk), log_mul hc.ne' hk.ne'] at ha
  simp only [B, log_mul hc.ne' hk.ne']
  linarith only [ha]

/-- For two standardized log levels, arbitrary positive count weights
still give a signed profile strictly below the best endpoint value.
The equal-count balanced-pair theorem supplies the strict comparison. -/
theorem weighted_two_level_signed_profile_lt_face {w t k : ℝ}
    (hw : 0 < w) (hw1 : w < 1) (ht : 0 < t) (hk : 0 < k) :
    let c := 2 * min w (1 - w)
    log t + (k * log k - k - log (Gamma k)) -
      k * (log (w * exp t + (1 - w) * exp (-t)) - (2 * w - 1) * t) < -1 - log c := by
  intro c
  have hc : 0 < c := mul_pos (by norm_num) (lt_min hw (sub_pos.mpr hw1))
  have hc1 : c ≤ 1 := by
    dsimp [c]
    by_cases h : w ≤ 1 / 2
    · rw [min_eq_left (by linarith)]
      linarith
    · rw [min_eq_right (by linarith)]
      linarith
  have hC := mul_le_mul_of_nonneg_left (weighted_two_level_cumulant_lower hw.le hw1.le t) hk.le
  have hB := gamma_profile_shape_scaling hk hc hc1
  have h := GenGammaTwoPoint.profile_lt_face ht (mul_pos hc hk)
  dsimp only [GenGammaTwoPoint.profile] at h
  change k * (c * GenGammaTwoPoint.cumulant t) ≤ _ at hC
  linarith only [hB, h, hC, show k * (c * GenGammaTwoPoint.cumulant t) =
    c * k * GenGammaTwoPoint.cumulant t by ring]

/-- Three distinct log observations identify the signed Prentice score
parameters. This is a separation input for almost-everywhere uniqueness. -/
theorem signed_score_separation {k p A k' p' A' x y z : ℝ}
    (hp : p ≠ 0) (hp' : p' ≠ 0) (hA : 0 < A) (hA' : 0 < A')
    (hxy : x < y) (hyz : y < z)
    (h : ∀ t ∈ ({x, y, z} : Set ℝ),
      k * p - A * p * exp (p * t) = k' * p' - A' * p' * exp (p' * t)) :
    p = p' ∧ k = k' ∧ A = A' := by
  let f := fun t : ℝ ↦ (k * p - A * p * exp (p * t)) -
    (k' * p' - A' * p' * exp (p' * t))
  let g := fun t : ℝ ↦ -(A * p ^ 2) * exp (p * t) - (-(A' * p' ^ 2) * exp (p' * t))
  have hd (t : ℝ) : HasDerivAt f (g t) t :=
    (score_deriv k p A t).sub (score_deriv k' p' A' t)
  have hc : Continuous f := continuous_iff_continuousAt.mpr
    fun t ↦ (hd t).continuousAt
  have hz (t : ℝ) (ht : t ∈ ({x, y, z} : Set ℝ)) : f t = 0 := sub_eq_zero.mpr (h t ht)
  have hx0 := hz x (by simp)
  have hy0 := hz y (by simp)
  have hz0 := hz z (by simp)
  obtain ⟨u, hu, hgu⟩ := exists_hasDerivAt_eq_zero hxy hc.continuousOn
    (hx0.trans hy0.symm) (fun t _ ↦ hd t)
  obtain ⟨v, hv, hgv⟩ := exists_hasDerivAt_eq_zero hyz hc.continuousOn
    (hy0.trans hz0.symm) (fun t _ ↦ hd t)
  have hzero (t : ℝ) (ht : g t = 0) :
      (A * p ^ 2) * exp ((p - p') * t) = A' * p' ^ 2 := by
    rw [sub_mul, exp_sub]
    rw [← mul_div_assoc]
    apply (div_eq_iff (exp_ne_zero (p' * t))).2
    dsimp [g] at ht
    linarith only [ht]
  have hc0 : A * p ^ 2 ≠ 0 := mul_ne_zero hA.ne' (pow_ne_zero _ hp)
  have hEq := exp_injective (mul_left_cancel₀ hc0 ((hzero u hgu).trans (hzero v hgv).symm))
  have hpp : p = p' := by
    apply sub_eq_zero.mp
    apply (mul_eq_zero.mp (show (p - p') * (u - v) = 0 by linear_combination hEq)).resolve_right
    exact sub_ne_zero.mpr (ne_of_lt (hu.2.trans hv.1))
  subst p'
  have hAA : A = A' := by
    apply mul_right_cancel₀ (pow_ne_zero 2 hp)
    simpa only [sub_self, zero_mul, exp_zero, mul_one] using hzero u hgu
  subst A'
  have hk : k = k' := by
    apply mul_right_cancel₀ hp
    linarith only [h x (by simp)]
  exact ⟨rfl, hk, rfl⟩

/-- A signed score cannot coincide with a normal score at three distinct
log observations, regardless of the normal intercept and slope. -/
theorem signed_normal_score_separation {k p A c m x y z : ℝ}
    (hp : p ≠ 0) (hA : 0 < A) (hxy : x < y) (hyz : y < z) :
    ¬ ∀ t ∈ ({x, y, z} : Set ℝ), k * p - A * p * exp (p * t) = c + m * t := by
  intro h
  let f := fun t : ℝ ↦ (k * p - A * p * exp (p * t)) - (c + m * t)
  let g := fun t : ℝ ↦ -(A * p ^ 2) * exp (p * t) - m
  have hd (t : ℝ) : HasDerivAt f (g t) t := by
    have hh := (score_deriv k p A t).sub (((hasDerivAt_id t).const_mul m).const_add c)
    convert hh using 1
    · funext v
      rfl
    · simp only [g, mul_one]
  have hc : Continuous f := continuous_iff_continuousAt.mpr fun t ↦ (hd t).continuousAt
  have hz (t : ℝ) (ht : t ∈ ({x, y, z} : Set ℝ)) : f t = 0 := sub_eq_zero.mpr (h t ht)
  have hx0 := hz x (by simp)
  have hy0 := hz y (by simp)
  have hz0 := hz z (by simp)
  obtain ⟨u, hu, hgu⟩ := exists_hasDerivAt_eq_zero hxy hc.continuousOn
    (hx0.trans hy0.symm) (fun t _ ↦ hd t)
  obtain ⟨v, hv, hgv⟩ := exists_hasDerivAt_eq_zero hyz hc.continuousOn
    (hy0.trans hz0.symm) (fun t _ ↦ hd t)
  have he : -(A * p ^ 2) * exp (p * u) = -(A * p ^ 2) * exp (p * v) := by
    dsimp [g] at hgu hgv
    linarith only [hgu, hgv]
  have hne : -(A * p ^ 2) ≠ 0 := neg_ne_zero.mpr (mul_ne_zero hA.ne' (pow_ne_zero _ hp))
  exact (ne_of_lt (hu.2.trans hv.1)) (mul_left_cancel₀ hp
    (exp_injective (mul_left_cancel₀ hne he)))

end
end UnivariateML.Shape3GenGamma
end
