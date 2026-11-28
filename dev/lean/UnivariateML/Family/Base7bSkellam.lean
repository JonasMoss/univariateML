module

public import UnivariateML.Family.Base7Skellam
public import UnivariateML.Family.Discrete4Skellam
import Mathlib.Analysis.SpecialFunctions.Sqrt
import Mathlib.Analysis.Analytic.OfScalars
import Mathlib.Analysis.Analytic.IsolatedZeros

set_option linter.style.header false

/-! The Skellam mean line, its analytic score and its finite candidate set.

The integrating-factor argument uses the normalized count series and
vanishing atom heights to exclude roots in the tail.

## Deviations
No decision algorithm for equality between distinct analytic candidate
heights is asserted.
-/

@[expose] public section

namespace UnivariateML.Base7bSkellam

open Real Set Filter Discrete3Skellam
open scoped Topology

noncomputable section

private def radialTerm (k j : ℕ) (t : ℝ) : ℝ :=
  t^j/(j.factorial : ℝ)/((j+k).factorial : ℝ)

private def radialSeries (k : ℕ) (t : ℝ) : ℝ :=
  ∑' j, radialTerm k j t

private theorem radialTerm_bound (k j : ℕ) {t R : ℝ} (ht : |t| ≤ R) :
    ‖radialTerm k j t‖ ≤ R^j/(j.factorial : ℝ) := by
  have hj : 0 ≤ (j.factorial : ℝ) := by positivity
  have hk : 1 ≤ ((j+k).factorial : ℝ) := by
    exact_mod_cast Nat.factorial_pos (j+k)
  simp only [radialTerm, Real.norm_eq_abs, abs_div, abs_pow,
    abs_of_nonneg hj, abs_of_nonneg (zero_le_one.trans hk)]
  exact (div_le_self (by positivity) hk).trans
    (div_le_div_of_nonneg_right (pow_le_pow_left₀ (abs_nonneg t) ht _) hj)

private theorem radialTerm_summable (k : ℕ) (t : ℝ) :
    Summable (fun j ↦ radialTerm k j t) :=
  Summable.of_norm_bounded (Real.summable_pow_div_factorial |t|)
    (fun j ↦ radialTerm_bound k j le_rfl)

private def radialTermDeriv (k : ℕ) (t : ℝ) : ℕ → ℝ
  | 0 => 0
  | j+1 => radialTerm (k+1) j t

private theorem radialTerm_deriv (k j : ℕ) (t : ℝ) :
    HasDerivAt (radialTerm k j) (radialTermDeriv k t j) t := by
  cases j with
  | zero =>
    have he : radialTerm k 0 = fun _ : ℝ ↦ ((k.factorial : ℝ)⁻¹) := by
      funext y
      simp [radialTerm]
    rw [he, radialTermDeriv]
    exact hasDerivAt_const t _
  | succ j =>
    have h := (((hasDerivAt_id t).pow j.succ).div_const (j.succ.factorial : ℝ)).div_const
      ((j.succ+k).factorial : ℝ)
    convert h using 1
    · rfl
    · rw [radialTermDeriv]
      change t^j/(j.factorial : ℝ)/((j+(k+1)).factorial : ℝ) =
        ((j.succ : ℝ)*t^(j.succ-1)*1)/(j.succ.factorial : ℝ)/((j.succ+k).factorial : ℝ)
      rw [show j.succ+k = j+(k+1) by omega, Nat.factorial_succ, Nat.cast_mul]
      simp only [Nat.succ_sub_one, mul_one]
      field_simp [show (j.succ : ℝ) ≠ 0 by positivity]

private theorem radialSeries_deriv (k : ℕ) (t : ℝ) :
    HasDerivAt (radialSeries k) (radialSeries (k+1) t) t := by
  let R := |t|+1
  let u : ℕ → ℝ := fun j ↦ if j = 0 then 0 else R^(j-1)/((j-1).factorial : ℝ)
  have hu : Summable u := by
    apply (summable_nat_add_iff (f := u) 1).mp
    simpa [u] using Real.summable_pow_div_factorial R
  have hb (j : ℕ) (y : ℝ) (hy : y ∈ Ioo (-R) R) : ‖radialTermDeriv k y j‖ ≤ u j := by
    cases j with
    | zero => simp [radialTermDeriv, u]
    | succ j =>
      have hv : |y| ≤ R := abs_le.mpr ⟨hy.1.le, hy.2.le⟩
      simpa [radialTermDeriv, u] using radialTerm_bound (k+1) j hv
  have ht : t ∈ Ioo (-R) R := by
    dsimp [R]
    exact ⟨by linarith [neg_abs_le t], by linarith [le_abs_self t]⟩
  have h := hasDerivAt_tsum_of_isPreconnected hu isOpen_Ioo
    (convex_Ioo (-R) R).isPreconnected (fun j y _ ↦ radialTerm_deriv k j y)
    hb ht (radialTerm_summable k t) ht
  have hs : Summable (radialTermDeriv k t) := by
    apply (summable_nat_add_iff 1).mp
    exact radialTerm_summable (k+1) t
  have he : (∑' j, radialTermDeriv k t j) = radialSeries (k+1) t := by
    rw [hs.tsum_eq_zero_add]
    exact zero_add _
  rw [he] at h
  exact h

private theorem factorial_ratio (j k : ℕ) (t : ℝ) :
    radialTerm k j t = ((j : ℝ)+(k : ℝ)+1)*radialTerm (k+1) j t := by
  unfold radialTerm
  rw [show j+(k+1) = (j+k)+1 by omega, Nat.factorial_succ, Nat.cast_mul]
  push_cast
  field_simp [show (j : ℝ)+(k : ℝ)+1 ≠ 0 by positivity]

private theorem radialSeries_ode (k : ℕ) (t : ℝ) :
    t*radialSeries (k+2) t+((k : ℝ)+1)*radialSeries (k+1) t = radialSeries k t := by
  let A : ℕ → ℝ := fun j ↦ if j = 0 then 0 else t*radialTerm (k+2) (j-1) t
  have hA : Summable A := by
    apply (summable_nat_add_iff (f := A) 1).mp
    simpa [A] using (radialTerm_summable (k+2) t).mul_left t
  have hAt : (∑' j, A j) = t*radialSeries (k+2) t := by
    rw [hA.tsum_eq_zero_add]
    simp [A, radialSeries, tsum_mul_left]
  rw [← hAt, radialSeries, ← tsum_mul_left,
    ← hA.tsum_add ((radialTerm_summable (k+1) t).mul_left ((k : ℝ)+1))]
  unfold radialSeries
  apply tsum_congr
  intro j
  rw [factorial_ratio j k]
  have he : A j = (j : ℝ)*radialTerm (k+1) j t := by
    cases j with
    | zero => simp [A]
    | succ j =>
      simp only [A, Nat.succ_ne_zero, ↓reduceIte, Nat.succ_sub_one]
      unfold radialTerm
      rw [show j.succ+(k+1) = j+(k+2) by omega, Nat.factorial_succ, Nat.cast_mul, pow_succ]
      field_simp [show (j.succ : ℝ) ≠ 0 by positivity]
  rw [he]
  ring

private theorem mass_nat_factor (k : ℕ) (a b : ℝ) :
    mass (k : ℤ) (a,b) = exp (-(a+b))*a^k*radialSeries k (a*b) := by
  unfold mass radialSeries
  rw [← tsum_mul_left]
  apply tsum_congr
  intro j
  simp only [Int.toNat_natCast, Int.toNat_of_nonpos (show -(k : ℤ) ≤ 0 by omega),
    Nat.add_zero, poissonMass, radialTerm, neg_add, exp_add, mul_pow, pow_add]
  ring

private theorem radialSeries_pos (k : ℕ) {t : ℝ} (ht : 0 ≤ t) : 0 < radialSeries k t := by
  have hn (j : ℕ) : 0 ≤ radialTerm k j t := by unfold radialTerm; positivity
  have h0 : 0 < radialTerm k 0 t := by simp [radialTerm, Nat.factorial_pos]
  exact h0.trans_le ((radialTerm_summable k t).le_tsum 0 fun j _ ↦ hn j)

private theorem mass_reflect_here (k : ℤ) (p : ℝ × ℝ) :
    mass (-k) (p.2,p.1) = mass k p := by
  unfold mass
  apply tsum_congr
  intro j
  simp [mul_comm]

private theorem mass_zero_left {k : ℤ} (hk : 0 < k) (b : ℝ) :
    mass k (0,b) = 0 := by
  unfold mass
  have hpos : 0 < k.toNat := by omega
  have h (j : ℕ) : poissonMass 0 (j+k.toNat) = 0 := by
    simp [poissonMass, zero_pow (show j+k.toNat ≠ 0 by omega)]
  simp [h]

private theorem mass_signed_factor {k : ℕ} {l : ℤ} (hl : l = (k : ℤ) ∨ l = -(k : ℤ))
    (a b : ℝ) :
    mass l (a,b) = (exp (-(a+b))*radialSeries k (a*b))*
      (a^l.toNat*b^(-l).toNat) := by
  rcases hl with rfl | rfl
  · rw [mass_nat_factor]
    rw [Int.toNat_natCast, Int.toNat_of_nonpos (show -(k : ℤ) ≤ 0 by omega), pow_zero]
    ring
  · rw [mass_reflect_here (k : ℤ) (b,a), mass_nat_factor, neg_neg, Int.toNat_natCast,
      Int.toNat_of_nonpos (show -(k : ℤ) ≤ 0 by omega), pow_zero]
    rw [add_comm b a, mul_comm b a]
    ring


private def totalRate (d u : ℝ) := sqrt (d ^ 2 + 4 * u)
private def rates (d u : ℝ) : ℝ × ℝ :=
  ((totalRate d u + d) / 2, (totalRate d u - d) / 2)

private theorem totalRate_sq (d : ℝ) {u : ℝ} (hu : 0 ≤ u) :
    totalRate d u ^ 2 = d ^ 2 + 4 * u := sq_sqrt (by positivity)

private theorem rates_pos (d : ℝ) {u : ℝ} (hu : 0 < u) :
    0 < (rates d u).1 ∧ 0 < (rates d u).2 := by
  have h := totalRate_sq d hu.le
  have hle : 0 ≤ totalRate d u := sqrt_nonneg _
  have hA : -d < totalRate d u := by
    by_contra! hn
    nlinarith [mul_nonneg (sub_nonneg.mpr hn) (show 0 ≤ -d + totalRate d u by linarith)]
  have hB : d < totalRate d u := by
    by_contra! hn
    nlinarith [mul_nonneg (sub_nonneg.mpr hn) (show 0 ≤ d + totalRate d u by linarith)]
  exact ⟨div_pos (by linarith) (by norm_num), div_pos (by linarith) (by norm_num)⟩

private theorem rates_prod (d : ℝ) {u : ℝ} (hu : 0 ≤ u) :
    (rates d u).1 * (rates d u).2 = u := by
  dsimp [rates]
  nlinarith only [totalRate_sq d hu]

private theorem rates_sum (d u : ℝ) : (rates d u).1 + (rates d u).2 = totalRate d u := by
  dsimp [rates]
  ring

private theorem rates_diff (d u : ℝ) : (rates d u).1 - (rates d u).2 = d := by
  dsimp [rates]
  ring

private theorem recover_rates {d a b : ℝ} (ha : 0 < a) (hb : 0 < b) (hd : a - b = d) :
    rates d (a * b) = (a, b) := by
  have H : totalRate d (a * b) = a + b := by
    dsimp [totalRate]
    rw [← hd, show (a - b) ^ 2 + 4 * (a * b) = (a + b) ^ 2 by ring,
      sqrt_sq (by positivity)]
  apply Prod.ext
  · dsimp [rates]; rw [H]; linarith
  · dsimp [rates]; rw [H]; linarith

private theorem totalRate_deriv (d : ℝ) {u : ℝ} (hu : 0 < u) :
    HasDerivAt (totalRate d) (2 / totalRate d u) u := by
  have H := (((hasDerivAt_id' u).const_mul 4).const_add (d ^ 2)).sqrt
    (show d ^ 2 + 4 * u ≠ 0 by positivity)
  convert H using 1
  · rfl
  · dsimp [totalRate]
    ring

private theorem rates_deriv (d : ℝ) {u : ℝ} (hu : 0 < u) :
    HasDerivAt (fun w ↦ (rates d w).1) (1 / totalRate d u) u ∧
    HasDerivAt (fun w ↦ (rates d w).2) (1 / totalRate d u) u := by
  have H := totalRate_deriv d hu
  constructor
  · convert (H.add_const d).div_const 2 using 1
    · rfl
    · ring
  · convert (H.sub_const d).div_const 2 using 1
    · rfl
    · ring

private def flux (k : ℕ) (d u : ℝ) : ℝ :=
  ((k : ℝ) - totalRate d u) * radialSeries k u + 2 * u * radialSeries (k + 1) u

private def fluxWeight (k : ℕ) (d u : ℝ) : ℝ :=
  exp (totalRate d u + (k : ℝ) / 2 * log u +
    d / 2 * (log (rates d u).2 - log (rates d u).1))

private theorem weighted_flux_deriv (k : ℕ) (d : ℝ) {u : ℝ} (hu : 0 < u) :
    HasDerivAt (fun w ↦ fluxWeight k d w * flux k d w)
      ((fluxWeight k d u * radialSeries k u / (2 * u)) *
        ((k : ℝ) ^ 2 - d ^ 2 - 4 * u / totalRate d u)) u := by
  have hH : 0 < totalRate d u := by
    rw [← rates_sum]
    exact add_pos (rates_pos d hu).1 (rates_pos d hu).2
  let a := (rates d u).1
  let b := (rates d u).2
  have ha : 0 < a := (rates_pos d hu).1
  have hb : 0 < b := (rates_pos d hu).2
  have hab : a * b = u := rates_prod d hu.le
  have had : a - b = d := rates_diff d u
  have hflux := (((totalRate_deriv d hu).const_sub (k : ℝ)).mul
    (radialSeries_deriv k u)).add
      (((hasDerivAt_id' u).const_mul 2).mul (radialSeries_deriv (k + 1) u))
  have hweight := (((totalRate_deriv d hu).add
    ((hasDerivAt_log hu.ne').const_mul ((k : ℝ) / 2))).add
      ((((rates_deriv d hu).2.log hb.ne').sub ((rates_deriv d hu).1.log ha.ne')).const_mul
        (d / 2))).exp
  have hw : HasDerivAt (fluxWeight k d)
      (fluxWeight k d u * (((k : ℝ) + totalRate d u) / (2 * u))) u := by
    convert hweight using 1
    · rfl
    · change _ = fluxWeight k d u * (2 / totalRate d u +
        (k : ℝ) / 2 * u⁻¹ + d / 2 * (1 / totalRate d u / b - 1 / totalRate d u / a))
      congr 1
      field_simp [hH.ne', hu.ne', ha.ne', hb.ne']
      linear_combination (totalRate d u ^ 2 - 4 * u) * hab - u * d * had +
        u * totalRate_sq d hu.le
  convert hw.mul hflux using 1
  · rfl
  · dsimp [flux]
    simp only [mul_one, show k + 1 + 1 = k + 2 by omega]
    have H := radialSeries_ode k u
    have E := totalRate_sq d hu.le
    field_simp [hu.ne', hH.ne']
    linear_combination
      -((4 : ℝ) * fluxWeight k d u * totalRate d u * u) * H +
      fluxWeight k d u * radialSeries k u * totalRate d u * E

private theorem flux_root_unique {k : ℕ} {d : ℝ} (hd : d ^ 2 < (k : ℝ) ^ 2)
    {u v : ℝ} (hu : 0 < u) (hv : 0 < v) (hru : flux k d u = 0)
    (hrv : flux k d v = 0) : u = v := by
  let W := fun t ↦ fluxWeight k d t * flux k d t
  let F := fun t ↦ (k : ℝ) ^ 2 - d ^ 2 - 4 * t / totalRate d t
  have hH {t : ℝ} (ht : 0 < t) : 0 < totalRate d t := by
    rw [← rates_sum]
    exact add_pos (rates_pos d ht).1 (rates_pos d ht).2
  have hfc : Continuous (flux k d) := by
    unfold flux
    have H (n : ℕ) : Continuous (radialSeries n) :=
      continuous_iff_continuousAt.mpr fun t ↦ (radialSeries_deriv n t).continuousAt
    have hRate : Continuous (totalRate d) := by unfold totalRate; fun_prop
    exact (continuous_const.sub hRate).mul (H k) |>.add ((continuous_const.mul continuous_id).mul (H _))
  have h0 : 0 < flux k d 0 := by
    have hk : |d| < (k : ℝ) := by
      rw [← sq_abs d] at hd
      exact lt_of_not_ge (fun h ↦ not_lt_of_ge
        (pow_le_pow_left₀ (Nat.cast_nonneg _) h 2) hd)
    have H : totalRate d 0 = |d| := by simp [totalRate, sqrt_sq_eq_abs]
    simp only [flux, H, mul_zero, zero_mul, add_zero]
    exact mul_pos (sub_pos.mpr hk) (radialSeries_pos k le_rfl)
  have near : ∀ᶠ t in 𝓝 (0 : ℝ), 0 < flux k d t :=
    hfc.continuousAt.eventually (Ioi_mem_nhds h0)
  have hfD (t : ℝ) (ht : 0 < t) :
      HasDerivAt F (-(4 * (totalRate d t ^ 2 - 2 * t) / totalRate d t ^ 3)) t := by
    have H := ((((hasDerivAt_id' t).const_mul 4).div
      (totalRate_deriv d ht) (hH ht).ne')).const_sub ((k : ℝ) ^ 2 - d ^ 2)
    convert H using 1
    field_simp [(hH ht).ne']
  have hanti : StrictAntiOn F (Ioi 0) := by
    refine strictAntiOn_of_deriv_neg (convex_Ioi _) (fun t ht ↦
      (hfD t ht).continuousAt.continuousWithinAt) ?_
    intro t ht
    rw [interior_Ioi] at ht
    rw [(hfD t ht).deriv]
    have hp : 0 < totalRate d t ^ 2 - 2 * t := by
      rw [totalRate_sq d ht.le]
      have ht' : 0 < t := ht
      nlinarith only [sq_nonneg d, ht']
    exact neg_neg_of_pos (by have := hH ht; positivity)
  have hneg {r : ℝ} (hr : 0 < r) (hz : flux k d r = 0) : F r < 0 := by
    by_contra! hn
    have H : ∀ᶠ t in 𝓝[>] (0 : ℝ), t ∈ Ioo 0 r ∧ 0 < flux k d t :=
      (show ∀ᶠ t in 𝓝[>] (0 : ℝ), 0 < flux k d t from nhdsWithin_le_nhds near).and
        ((show ∀ᶠ t in 𝓝[>] (0 : ℝ), 0 < t from self_mem_nhdsWithin).and
          (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds hr))) |>.mono
            (fun _ h ↦ ⟨h.2, h.1⟩)
    obtain ⟨t, ht, htpos⟩ := H.exists
    have hmono : StrictMonoOn W (Icc t r) := by
      apply strictMonoOn_of_deriv_pos (convex_Icc _ _)
      · intro w hw
        exact (weighted_flux_deriv k d (ht.1.trans_le hw.1)).continuousAt.continuousWithinAt
      · intro w hw
        rw [interior_Icc] at hw
        have hw0 := ht.1.trans hw.1
        rw [(weighted_flux_deriv k d hw0).deriv]
        have hF : 0 < F w := hn.trans_lt (hanti hw0 hr hw.2)
        have hG := radialSeries_pos k hw0.le
        exact mul_pos (div_pos (mul_pos (exp_pos _) hG) (by positivity)) hF
    have H := hmono (left_mem_Icc.mpr ht.2.le) (right_mem_Icc.mpr ht.2.le) ht.2
    have hpos : 0 < W t := mul_pos (exp_pos _) htpos
    have hzero : W r = 0 := by simp [W, hz]
    linarith
  have hun := hneg hu hru
  have hvn := hneg hv hrv
  have hmono : StrictAntiOn W (Ioi 0 ∩ {w | F w < 0}) := by
    -- On a segment starting at a negative value of the source, the
    -- weighted flux is strictly decreasing.
    intro r hr s hs hrs
    have H : StrictAntiOn W (Icc r s) := by
      apply strictAntiOn_of_deriv_neg (convex_Icc _ _)
      · intro t ht
        exact (weighted_flux_deriv k d (hr.1.trans_le ht.1)).continuousAt.continuousWithinAt
      · intro t ht
        rw [interior_Icc] at ht
        have ht0 := hr.1.trans ht.1
        rw [(weighted_flux_deriv k d ht0).deriv]
        have hF : F t < 0 := (hanti hr.1 ht0 ht.1).trans hr.2
        have hG := radialSeries_pos k ht0.le
        exact mul_neg_of_pos_of_neg (div_pos (mul_pos (exp_pos _) hG) (by positivity)) hF
    exact H (left_mem_Icc.mpr hrs.le) (right_mem_Icc.mpr hrs.le) hrs
  exact hmono.injOn ⟨hu, hun⟩ ⟨hv, hvn⟩ (by simp [W, hru, hrv])

private theorem radialSeries_analytic (k : ℕ) (u : ℝ) :
    AnalyticAt ℝ (radialSeries k) u := by
  let c (n : ℕ) : ℝ := 1 / ((n.factorial : ℝ) * ((n + k).factorial : ℝ))
  let P := FormalMultilinearSeries.ofScalars ℝ c
  have hc (n : ℕ) : 0 < c n := by dsimp [c]; positivity
  have hlim : Tendsto (fun n : ℕ ↦ ‖c (n + 1)‖ / ‖c n‖) atTop (𝓝 0) := by
    have H : Tendsto (fun n : ℕ ↦ ((n : ℝ) + 1)⁻¹) atTop (𝓝 0) :=
      tendsto_inv_atTop_zero.comp (tendsto_atTop_add_const_right _ _ tendsto_natCast_atTop_atTop)
    have H' : Tendsto (fun n : ℕ ↦ ((n : ℝ) + k + 1)⁻¹) atTop (𝓝 0) :=
      tendsto_inv_atTop_zero.comp (tendsto_atTop_add_const_right _ _
        (tendsto_atTop_add_const_right _ _ tendsto_natCast_atTop_atTop))
    have ht := H.mul H'
    rw [zero_mul] at ht
    apply ht.congr
    intro n
    simp only [Real.norm_eq_abs, abs_of_pos (hc _), c]
    rw [show n + 1 + k = n + k + 1 by omega, Nat.factorial_succ, Nat.factorial_succ, Nat.cast_mul,
      Nat.cast_mul, Nat.cast_add, Nat.cast_add, Nat.cast_one]
    field_simp
    push_cast
    rfl
  have hP : P.radius = ⊤ := FormalMultilinearSeries.ofScalars_radius_eq_top_of_tendsto
    ℝ c (Eventually.of_forall fun n ↦ (hc n).ne') hlim
  have HE : P.sum = radialSeries k := by
    change FormalMultilinearSeries.ofScalarsSum c = _
    rw [FormalMultilinearSeries.ofScalarsSum_eq_tsum]
    funext t
    apply tsum_congr
    intro j
    simp only [c, radialTerm, smul_eq_mul]
    ring
  rw [← HE]
  exact P.analyticOnNhd u (by simp [hP])

private theorem totalRate_atTop (d : ℝ) : Tendsto (totalRate d) atTop atTop := by
  apply tendsto_atTop.mpr
  intro b
  filter_upwards [eventually_ge_atTop (max 0 ((b ^ 2 + 1) / 4))] with u hu
  have h₀ : 0 ≤ u := (le_max_left _ _).trans hu
  have h₁ := (le_max_right 0 ((b ^ 2 + 1) / 4)).trans hu
  have he := totalRate_sq d h₀
  have hb : b ^ 2 < totalRate d u ^ 2 := by nlinarith only [he, h₁, sq_nonneg d]
  have hp : 0 ≤ totalRate d u := sqrt_nonneg _
  exact le_of_lt (lt_of_not_ge fun h ↦ hb.not_ge (pow_le_pow_left₀ hp h 2))

private theorem first_rate_atTop (d : ℝ) : Tendsto (fun u ↦ (rates d u).1) atTop atTop := by
  change Tendsto (fun u ↦ (totalRate d u + d) / 2) atTop atTop
  simpa only [div_eq_mul_inv] using
    (tendsto_atTop_add_const_right _ d (totalRate_atTop d)).atTop_mul_const
      (by norm_num : 0 < (2 : ℝ)⁻¹)

private theorem second_rate_atTop (d : ℝ) : Tendsto (fun u ↦ (rates d u).2) atTop atTop := by
  change Tendsto (fun u ↦ (totalRate d u - d) / 2) atTop atTop
  simpa only [div_eq_mul_inv, sub_eq_add_neg] using
    (tendsto_atTop_add_const_right _ (-d) (totalRate_atTop d)).atTop_mul_const
      (by norm_num : 0 < (2 : ℝ)⁻¹)

private theorem positive_atom_profile_vanishes (k : ℕ) (d : ℝ) :
    Tendsto (fun u ↦ mass (k : ℤ) (rates d u)) atTop (𝓝[>] 0) := by
  have hpos : ∀ᶠ u : ℝ in atTop, 0 < mass (k : ℤ) (rates d u) := by
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with u hu
    exact mass_pos _ (rates_pos d hu).1 (rates_pos d hu).2
  refine tendsto_nhdsWithin_iff.mpr ⟨tendsto_order.2 ⟨?_, ?_⟩, hpos⟩
  · intro a ha
    exact hpos.mono fun u hu ↦ ha.trans hu
  · intro ε hε
    obtain ⟨R, hR⟩ := likelihood_uniform_tail (by decide) (fun _ : Fin 1 ↦ (k : ℤ)) hε
    filter_upwards [(first_rate_atTop d).eventually_gt_atTop R, eventually_gt_atTop (0 : ℝ)]
      with u hu hpos
    have hr := rates_pos d hpos
    simpa [likelihood] using hR (rates d u) ⟨hr.1.le, hr.2.le⟩ (Or.inl hu)

private theorem log_rate_ratio_tendsto (d : ℝ) :
    Tendsto (fun u ↦ log (rates d u).1 - log (rates d u).2) atTop (𝓝 0) := by
  have H := ((tendsto_const_nhds (x := d)).div_atTop (second_rate_atTop d)).const_add 1
  rw [add_zero] at H
  have H' : Tendsto (fun u ↦ (rates d u).1 / (rates d u).2) atTop (𝓝 1) := by
    apply H.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with u hu
    have hp := rates_pos d hu
    have he := rates_diff d u
    change 1 + d / (rates d u).2 = (rates d u).1 / (rates d u).2
    apply (eq_div_iff hp.2.ne').mpr
    field_simp [hp.2.ne']
    linarith only [he]
  have HT := (continuousAt_log one_ne_zero).tendsto.comp H'
  rw [log_one] at HT
  apply HT.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with u hu
  have hp := rates_pos d hu
  change log ((rates d u).1 / (rates d u).2) = _
  exact log_div hp.1.ne' hp.2.ne'

private def atomPotential (k : ℕ) (d u : ℝ) : ℝ :=
  -totalRate d u + log (radialSeries k u) + (k : ℝ) / 2 * log u +
    d / 2 * (log (rates d u).1 - log (rates d u).2)

private theorem atomPotential_atBot (k : ℕ) (d : ℝ) :
    Tendsto (atomPotential k d) atTop atBot := by
  have H := tendsto_log_nhdsGT_zero.comp (positive_atom_profile_vanishes k d)
  have hcorr := (log_rate_ratio_tendsto d).const_mul ((d - k) / 2)
  rw [mul_zero] at hcorr
  apply tendsto_atBot.mpr
  intro B
  filter_upwards [H.eventually_le_atBot (B - 1), hcorr.eventually (Iio_mem_nhds zero_lt_one),
    eventually_gt_atTop (0 : ℝ)] with u hu hsmall hpos
  have hp := rates_pos d hpos
  have hG := radialSeries_pos k hpos.le
  have he : atomPotential k d u = log (mass (k : ℤ) (rates d u)) +
      ((d - k) / 2) * (log (rates d u).1 - log (rates d u).2) := by
    rcases h : rates d u with ⟨a, b⟩
    have ha : 0 < a := by simpa [h] using hp.1
    have hb : 0 < b := by simpa [h] using hp.2
    have hp' : a * b = u := by simpa [h] using rates_prod d hpos.le
    have hs : a + b = totalRate d u := by simpa [h] using rates_sum d u
    dsimp [atomPotential]
    rw [h, mass_nat_factor, hs, hp',
      log_mul (mul_pos (exp_pos _) (pow_pos ha _)).ne' hG.ne',
      log_mul (exp_pos _).ne' (pow_pos ha _).ne', log_exp, log_pow]
    have E : log u = log a + log b := by rw [← hp', log_mul ha.ne' hb.ne']
    rw [E]
    ring
  rw [he]
  change log (mass (k : ℤ) (rates d u)) ≤ B - 1 at hu
  change ((d - k) / 2) * (log (rates d u).1 - log (rates d u).2) < 1 at hsmall
  linarith only [hu, hsmall]

private theorem atomPotential_deriv (k : ℕ) (d : ℝ) {u : ℝ} (hu : 0 < u) :
    HasDerivAt (atomPotential k d) (flux k d u / (2 * u * radialSeries k u)) u := by
  let H := totalRate d u
  let a := (rates d u).1
  let b := (rates d u).2
  have hp := rates_pos d hu
  have ha : 0 < a := hp.1
  have hb : 0 < b := hp.2
  have hH : 0 < H := by change 0 < totalRate d u; rw [← rates_sum]; exact add_pos ha hb
  have hG := radialSeries_pos k hu.le
  have Hder := ((((totalRate_deriv d hu).neg.add ((radialSeries_deriv k u).log hG.ne')).add
    ((hasDerivAt_log hu.ne').const_mul ((k : ℝ) / 2))).add
    ((((rates_deriv d hu).1.log ha.ne').sub ((rates_deriv d hu).2.log hb.ne')).const_mul (d / 2)))
  convert Hder using 1
  · rfl
  · change _ = (-(2 / H) + radialSeries (k + 1) u / radialSeries k u) +
      (k : ℝ) / 2 * u⁻¹ + d / 2 * (1 / H / a - 1 / H / b)
    have hterm : 1 / H / a - 1 / H / b = -d / (H * u) := by
      have hab : a * b = u := rates_prod d hu.le
      have hdiff : a - b = d := rates_diff d u
      rw [← hab, ← hdiff]
      field_simp [hH.ne', ha.ne', hb.ne']
      ring
    rw [hterm]
    change (((k : ℝ) - H) * radialSeries k u + 2 * u * radialSeries (k + 1) u) /
        (2 * u * radialSeries k u) = _
    field_simp [hH.ne', hu.ne', hG.ne']
    linear_combination -radialSeries k u * totalRate_sq d hu.le

private theorem flux_eventually_negative (k : ℕ) (d : ℝ) :
    ∀ᶠ u : ℝ in atTop, flux k d u < 0 := by
  let H := totalRate d
  let W := fun t ↦ fluxWeight k d t * flux k d t
  have Hneg : ∀ᶠ u : ℝ in atTop, (k : ℝ) ^ 2 - d ^ 2 - 4 * u / H u < 0 := by
    filter_upwards [(totalRate_atTop d).eventually_gt_atTop ((k : ℝ) ^ 2 + d ^ 2 + 1),
      eventually_gt_atTop (0 : ℝ)] with u hh hu
    have hH : 1 < H u := by
      change (k : ℝ) ^ 2 + d ^ 2 + 1 < H u at hh
      nlinarith only [hh, sq_nonneg (k : ℝ), sq_nonneg d]
    have he : 4 * u / H u = H u - d ^ 2 / H u := by
      field_simp [show H u ≠ 0 by linarith]
      have := totalRate_sq d hu.le
      change H u ^ 2 = d ^ 2 + 4 * u at this
      linarith only [this]
    have hd : d ^ 2 / H u ≤ d ^ 2 :=
      div_le_self (sq_nonneg _) hH.le
    rw [he]
    linarith only [hh, hd, sq_nonneg d]
  obtain ⟨B, hB⟩ := eventually_atTop.mp Hneg
  let C := max B 1
  have hC : 0 < C := zero_lt_one.trans_le (le_max_right _ _)
  have hanti : StrictAntiOn W (Ici C) := by
    apply strictAntiOn_of_deriv_neg (convex_Ici _)
    · intro t ht
      exact (weighted_flux_deriv k d (hC.trans_le ht)).continuousAt.continuousWithinAt
    · intro t ht
      rw [interior_Ici] at ht
      have ht' : 0 < t := hC.trans ht
      rw [(weighted_flux_deriv k d ht').deriv]
      exact mul_neg_of_pos_of_neg
        (div_pos (mul_pos (exp_pos _) (radialSeries_pos k ht'.le)) (by positivity))
        (hB t ((le_max_left _ _).trans ht.le))
  have hsome : ∃ u ≥ C, flux k d u < 0 := by
    by_contra! hn
    have hmono : MonotoneOn (atomPotential k d) (Ici C) := by
      apply monotoneOn_of_deriv_nonneg (convex_Ici _)
      · intro t ht
        exact (atomPotential_deriv k d (hC.trans_le ht)).continuousAt.continuousWithinAt
      · intro t ht
        rw [interior_Ici] at ht
        exact (atomPotential_deriv k d (hC.trans ht)).differentiableAt.differentiableWithinAt
      · intro t ht
        rw [interior_Ici] at ht
        rw [(atomPotential_deriv k d (hC.trans ht)).deriv]
        exact div_nonneg (hn t ht.le)
          (by have := hC.trans ht; have := radialSeries_pos k (hC.trans ht).le; positivity)
    obtain ⟨t, ht, hlt⟩ := ((eventually_ge_atTop C).and
      ((atomPotential_atBot k d).eventually_lt_atBot (atomPotential k d C))).exists
    exact hlt.not_ge (hmono le_rfl ht ht)
  obtain ⟨u, hu, hfu⟩ := hsome
  filter_upwards [eventually_gt_atTop u] with t ht
  have H := hanti hu (hu.trans ht.le) ht
  have hwt : W t < 0 := H.trans (mul_neg_of_pos_of_neg (exp_pos _) hfu)
  exact neg_of_mul_neg_right hwt (exp_pos _).le

private theorem log_mass_as_potential (x : ℤ) (d : ℝ) {u : ℝ} (hu : 0 < u) :
    log (mass x (rates d u)) = atomPotential x.natAbs d u +
      ((x : ℝ) - d) / 2 * (log (rates d u).1 - log (rates d u).2) := by
  have hp := rates_pos d hu
  have hG := radialSeries_pos x.natAbs hu.le
  rcases h : rates d u with ⟨a, b⟩
  have ha : 0 < a := by simpa [h] using hp.1
  have hb : 0 < b := by simpa [h] using hp.2
  have hp' : a * b = u := by simpa [h] using rates_prod d hu.le
  have hs : a + b = totalRate d u := by simpa [h] using rates_sum d u
  have hx : x = (x.natAbs : ℤ) ∨ x = -(x.natAbs : ℤ) := by omega
  have hlogu : log u = log a + log b := by rw [← hp', log_mul ha.ne' hb.ne']
  rw [mass_signed_factor hx, hs, hp',
    log_mul (mul_pos (exp_pos _) hG).ne' (mul_pos (pow_pos ha _) (pow_pos hb _)).ne',
    log_mul (exp_pos _).ne' hG.ne', log_exp,
    log_mul (pow_pos ha _).ne' (pow_pos hb _).ne', log_pow, log_pow]
  unfold atomPotential
  rw [hlogu, h]
  rcases hx with hx | hx
  · have hx0 : (x : ℝ) = x.natAbs := by
      simpa only [Int.cast_natCast] using congrArg (Int.cast (R := ℝ)) hx
    have hn : (-x).toNat = 0 := Int.toNat_of_nonpos (by omega)
    have hp : x.toNat = x.natAbs := by omega
    rw [hx0, hn, hp, Nat.cast_zero]
    ring
  · have hx0 : (x : ℝ) = -(x.natAbs : ℝ) := by
      simpa only [Int.cast_neg, Int.cast_natCast] using congrArg (Int.cast (R := ℝ)) hx
    have hn : x.toNat = 0 := Int.toNat_of_nonpos (by omega)
    have hp : (-x).toNat = x.natAbs := by omega
    rw [hx0, hn, hp, Nat.cast_zero]
    ring

private theorem general_profile_deriv {n : ℕ} (x : Fin n → ℤ)
    {d u : ℝ} (hu : 0 < u) (hd : ∑ i, (x i : ℝ) = (n : ℝ) * d) :
    HasDerivAt (fun v ↦ log (likelihood x (rates d v)))
      ((∑ i, flux (x i).natAbs d u / radialSeries (x i).natAbs u) / (2 * u)) u := by
  have he (v : ℝ) (hv : 0 < v) :
      log (likelihood x (rates d v)) = ∑ i, atomPotential (x i).natAbs d v := by
    have hp := rates_pos d hv
    unfold likelihood
    rw [log_prod (fun i _ ↦ (mass_pos (x i) hp.1 hp.2).ne')]
    simp_rw [log_mass_as_potential _ d hv, Finset.sum_add_distrib, ← Finset.sum_mul,
      div_eq_mul_inv, ← Finset.sum_mul, Finset.sum_sub_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, hd, sub_self, zero_mul, add_zero]
  have H := HasDerivAt.fun_sum (u := Finset.univ)
    (fun i (_ : i ∈ Finset.univ) ↦ atomPotential_deriv (x i).natAbs d hu)
  have hEq : (∑ i, flux (x i).natAbs d u / (2 * u * radialSeries (x i).natAbs u)) =
      (∑ i, flux (x i).natAbs d u / radialSeries (x i).natAbs u) / (2 * u) := by
    simp only [div_eq_mul_inv, Finset.sum_mul, mul_inv_rev]
    apply Finset.sum_congr rfl
    intro i _
    ring
  rw [hEq] at H
  exact H.congr_of_eventuallyEq ((show ∀ᶠ v : ℝ in 𝓝 u, 0 < v from Ioi_mem_nhds hu).mono
    fun v hv ↦ he v hv)

private def meanLine (d t : ℝ) : ℝ × ℝ := (t + max d 0, t + max (-d) 0)

private theorem meanLine_sum (d t : ℝ) : (meanLine d t).1 + (meanLine d t).2 = 2 * t + |d| := by
  rcases le_total 0 d with hd | hd
  · simp [meanLine, max_eq_left hd, max_eq_right (neg_nonpos.mpr hd), abs_of_nonneg hd]
    ring
  · simp [meanLine, max_eq_right hd, max_eq_left (neg_nonneg.mpr hd), abs_of_nonpos hd]
    ring

private theorem meanLine_diff (d t : ℝ) : (meanLine d t).1 - (meanLine d t).2 = d := by
  rcases le_total 0 d with hd | hd
  · simp [meanLine, max_eq_left hd, max_eq_right (neg_nonpos.mpr hd)]
  · simp [meanLine, max_eq_right hd, max_eq_left (neg_nonneg.mpr hd)]

private theorem meanLine_prod (d t : ℝ) : (meanLine d t).1 * (meanLine d t).2 = t * (t + |d|) := by
  rcases le_total 0 d with hd | hd
  · simp [meanLine, max_eq_left hd, max_eq_right (neg_nonpos.mpr hd), abs_of_nonneg hd, mul_comm]
  · simp [meanLine, max_eq_right hd, max_eq_left (neg_nonneg.mpr hd), abs_of_nonpos hd]

private theorem meanLine_domain (d : ℝ) {t : ℝ} (ht : 0 ≤ t) : meanLine d t ∈ domain :=
  ⟨add_nonneg ht (le_max_right _ _), add_nonneg ht (le_max_right _ _)⟩

private theorem meanLine_recover {p : ℝ × ℝ} {d : ℝ} (h : p.1 - p.2 = d) :
    meanLine d (min p.1 p.2) = p := by
  rcases le_total p.1 p.2 with hle | hle
  · have hd : d ≤ 0 := by linarith
    apply Prod.ext
    · simp [meanLine, min_eq_left hle, max_eq_right hd]
    · simp [meanLine, min_eq_left hle, max_eq_left (neg_nonneg.mpr hd)]
      linarith
  · have hd : 0 ≤ d := by linarith
    apply Prod.ext
    · simp [meanLine, min_eq_right hle, max_eq_left hd]
      linarith
    · simp [meanLine, min_eq_right hle, max_eq_right (neg_nonpos.mpr hd)]

private def meanLineScore {n : ℕ} (x : Fin n → ℤ) (d t : ℝ) : ℝ :=
  ∑ i, (((x i).natAbs : ℝ) - (2 * t + |d|) +
    2 * (t * (t + |d|)) * radialSeries ((x i).natAbs + 1) (t * (t + |d|)) /
      radialSeries (x i).natAbs (t * (t + |d|)))

private theorem meanLineScore_analytic {n : ℕ} (x : Fin n → ℤ) (d : ℝ) :
    AnalyticOnNhd ℝ (meanLineScore x d) (Ici 0) := by
  intro t ht
  unfold meanLineScore
  apply Finset.analyticAt_fun_sum
  intro i _
  let u : ℝ → ℝ := fun v ↦ v * (v + |d|)
  have hu : AnalyticAt ℝ u t := by dsimp [u]; fun_prop
  have hU : 0 ≤ u t := mul_nonneg ht (add_nonneg ht (abs_nonneg _))
  have h₁ := (radialSeries_analytic (x i).natAbs (u t)).comp hu
  have h₂ := (radialSeries_analytic ((x i).natAbs + 1) (u t)).comp hu
  have hbase : AnalyticAt ℝ (fun v ↦ ((x i).natAbs : ℝ) - (2 * v + |d|)) t := by fun_prop
  exact hbase.add (((analyticAt_const.mul hu).mul h₂).div h₁ (radialSeries_pos _ hU).ne')

private theorem meanLineScore_eq {n : ℕ} (x : Fin n → ℤ) (d : ℝ) {t : ℝ} (ht : 0 < t) :
    let u := t * (t + |d|)
    meanLineScore x d t = ∑ i, flux (x i).natAbs d u / radialSeries (x i).natAbs u := by
  intro u
  have hp : 0 < (meanLine d t).1 ∧ 0 < (meanLine d t).2 :=
    ⟨add_pos_of_pos_of_nonneg ht (le_max_right _ _),
      add_pos_of_pos_of_nonneg ht (le_max_right _ _)⟩
  have hr := recover_rates hp.1 hp.2 (meanLine_diff d t)
  rw [meanLine_prod] at hr
  have hsum : totalRate d u = 2 * t + |d| := by
    rw [← rates_sum, hr, meanLine_sum]
  unfold meanLineScore
  apply Finset.sum_congr rfl
  intro i _
  unfold flux
  rw [hsum, add_div]
  have hG := radialSeries_pos (x i).natAbs (mul_nonneg ht.le
    (add_nonneg ht.le (abs_nonneg d)))
  rw [mul_div_cancel_right₀ _ hG.ne']

private theorem meanLineScore_tail {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) (d : ℝ) :
    ∀ᶠ t : ℝ in atTop, meanLineScore x d t < 0 := by
  let u : ℝ → ℝ := fun t ↦ t * (t + |d|)
  have hu : Tendsto u atTop atTop := by
    apply tendsto_atTop_mono' _ _ (tendsto_pow_atTop (show (2 : ℕ) ≠ 0 by decide))
    filter_upwards [eventually_ge_atTop (0 : ℝ)] with t ht
    dsimp [u]
    nlinarith only [mul_nonneg ht (abs_nonneg d)]
  have hb : ∀ᶠ t : ℝ in atTop, ∀ i, flux (x i).natAbs d (u t) < 0 :=
    eventually_all.mpr fun i ↦ hu.eventually (flux_eventually_negative (x i).natAbs d)
  filter_upwards [hb, eventually_gt_atTop (0 : ℝ)] with t ht hpos
  rw [meanLineScore_eq x d hpos]
  exact Finset.sum_neg (fun i _ ↦ div_neg_of_neg_of_pos (ht i)
    (radialSeries_pos _ (mul_nonneg hpos.le (add_nonneg hpos.le (abs_nonneg _)))))
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩

private theorem meanLine_hasDerivAt {n : ℕ} (x : Fin n → ℤ) {d t : ℝ} (ht : 0 < t)
    (hd : ∑ i, (x i : ℝ) = (n : ℝ) * d) :
    HasDerivAt (fun t ↦ log (likelihood x (meanLine d t)))
      (meanLineScore x d t / (2 * (t * (t + |d|))) * (2 * t + |d|)) t := by
  let u := fun t ↦ t * (t + |d|)
  have hu : 0 < u t := mul_pos ht (add_pos_of_pos_of_nonneg ht (abs_nonneg _))
  have hdu : HasDerivAt u (2 * t + |d|) t := by
    convert (hasDerivAt_id' t).mul ((hasDerivAt_id' t).add_const |d|) using 1
    ring
  have H := (general_profile_deriv x hu hd).comp t hdu
  rw [← meanLineScore_eq x d ht] at H
  apply H.congr_of_eventuallyEq
  filter_upwards [show ∀ᶠ v : ℝ in 𝓝 t, 0 < v from Ioi_mem_nhds ht] with v hv
  have hp : 0 < (meanLine d v).1 ∧ 0 < (meanLine d v).2 :=
    ⟨add_pos_of_pos_of_nonneg hv (le_max_right _ _),
      add_pos_of_pos_of_nonneg hv (le_max_right _ _)⟩
  have hr := recover_rates hp.1 hp.2 (meanLine_diff d v)
  rw [meanLine_prod] at hr
  change log (likelihood x (meanLine d v)) = log (likelihood x (rates d (u v)))
  rw [hr]

/-- Every nonempty signed histogram has a finite exact candidate set.
The candidates are the positive score zeros on the mean-matching line
and the zero endpoint when its Poisson likelihood is positive. All
highest candidates, including any ties, are retained. -/
theorem finite_mean_line_candidates {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    let d := (∑ i, (x i : ℝ)) / n
    let R := fun t : ℝ ↦ (t + max d 0, t + max (-d) 0)
    let G := fun (k : ℕ) (u : ℝ) ↦
      ∑' j : ℕ, u ^ j / (j.factorial : ℝ) / ((j + k).factorial : ℝ)
    let S := fun t : ℝ ↦ ∑ i, (((x i).natAbs : ℝ) - (2 * t + |d|) +
      2 * (t * (t + |d|)) * G ((x i).natAbs + 1) (t * (t + |d|)) /
        G (x i).natAbs (t * (t + |d|)))
    let P := fun t ↦ likelihood x (R t)
    let C := {t : ℝ | (0 < t ∧ S t = 0) ∨ (t = 0 ∧ 0 < P t)}
    C.Finite ∧ ∀ q, (q ∈ domain ∧ IsMaxOn (likelihood x) domain q) ↔
      ∃ t ∈ C, q = R t ∧ IsMaxOn P C t := by
  intro d R G S P C
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hd : ∑ i, (x i : ℝ) = (n : ℝ) * d := by
    dsimp [d]
    field_simp
  have han : AnalyticOnNhd ℝ S (Ici 0) := meanLineScore_analytic x d
  have htail : ∀ᶠ u : ℝ in atTop, S u < 0 := meanLineScore_tail hn x d
  have hne : ∃ t ∈ Ici (0 : ℝ), S t ≠ 0 := by
    obtain ⟨t, ht, hlt⟩ := ((eventually_ge_atTop (0 : ℝ)).and htail).exists
    exact ⟨t, ht, hlt.ne⟩
  obtain ⟨T, hT⟩ := eventually_atTop.mp htail
  have hdis : ∀ᶠ t in codiscreteWithin (Ici (0 : ℝ)), S t ≠ 0 := by
    rcases han.eqOn_zero_or_eventually_ne_zero_of_preconnected (convex_Ici _).isPreconnected
      with he | h
    · obtain ⟨t, ht, hn⟩ := hne
      exact (hn (he ht)).elim
    · exact h
  have hsub : Icc (0 : ℝ) T ⊆ Ici 0 := fun _ t ↦ t.1
  have hf := (isCompact_Icc (a := (0 : ℝ)) (b := T)).finite_sdiff_of_mem_codiscreteWithin
    (show {t | S t ≠ 0} ∈ codiscreteWithin (Icc 0 T) from codiscreteWithin_mono hsub hdis)
  have hC : C.Finite := by
    apply ((Set.finite_singleton (0 : ℝ)).union hf).subset
    rintro t (⟨ht, hz⟩ | ⟨rfl, _⟩)
    · refine Or.inr ⟨⟨ht.le, ?_⟩, fun hh ↦ hh hz⟩
      exact le_of_not_ge fun h ↦ (hT t h).ne hz
    · exact Or.inl rfl
  have hNecessary (q : ℝ × ℝ) (hq : q ∈ domain) (hm : IsMaxOn (likelihood x) domain q) :
      ∃ t ∈ C, q = R t := by
    let t := min q.1 q.2
    have ht : 0 ≤ t := le_min hq.1 hq.2
    have hr : R t = q := meanLine_recover (FinishSkellam.maximizing_rate_difference hn x q hq hm)
    have hqpos : 0 < likelihood x q :=
      (Finset.prod_pos fun i _ ↦ mass_pos (x i) (p := (1, 1)) one_pos one_pos).trans_le
        (hm (a := (1, 1)) ⟨zero_le_one, zero_le_one⟩)
    have hct : t ∈ C := by
      rcases ht.eq_or_lt with h | h
      · exact Or.inr ⟨h.symm, by change 0 < likelihood x (R t); rwa [hr]⟩
      · refine Or.inl ⟨h, ?_⟩
        have hmx : IsMaxOn (fun v ↦ log (likelihood x (meanLine d v))) (Ioi 0) t := by
          intro v hv
          have hp : 0 < (meanLine d v).1 ∧ 0 < (meanLine d v).2 :=
            ⟨add_pos_of_pos_of_nonneg hv (le_max_right _ _),
              add_pos_of_pos_of_nonneg hv (le_max_right _ _)⟩
          have hP : 0 < likelihood x (meanLine d v) :=
            Finset.prod_pos fun i _ ↦ mass_pos (x i) hp.1 hp.2
          change log (likelihood x (meanLine d v)) ≤ log (likelihood x (R t))
          rw [hr]
          exact log_le_log hP (hm (meanLine_domain d hv.le))
        have hz := (hmx.isLocalMax (Ioi_mem_nhds h)).hasDerivAt_eq_zero
          (meanLine_hasDerivAt x h hd)
        have hcoef : 0 < 2 * t + |d| := by positivity
        have hden : 0 < 2 * (t * (t + |d|)) := by positivity
        exact (div_eq_zero_iff.mp ((mul_eq_zero.mp hz).resolve_right hcoef.ne')).resolve_right
          hden.ne'
    exact ⟨t, hct, hr.symm⟩
  refine ⟨hC, fun q ↦ ⟨?_, ?_⟩⟩
  · rintro ⟨hq, hm⟩
    obtain ⟨t, hct, hqr⟩ := hNecessary q hq hm
    refine ⟨t, hct, hqr, fun v hv ↦ ?_⟩
    have hv0 : 0 ≤ v := hv.elim (fun hv ↦ hv.1.le) (fun hv ↦ hv.1.symm ▸ le_rfl)
    change likelihood x (R v) ≤ likelihood x (R t)
    rw [← hqr]
    exact hm (meanLine_domain d hv0)
  · rintro ⟨t, ht, rfl, hm⟩
    have ht0 : 0 ≤ t := ht.elim (fun ht ↦ ht.1.le) (fun ht ↦ ht.1.symm ▸ le_rfl)
    refine ⟨meanLine_domain d ht0, fun q hq ↦ ?_⟩
    obtain ⟨p, hp, hmp⟩ := joint_attained hn x
    obtain ⟨v, hcv, hvr⟩ := hNecessary p hp hmp
    have HC : likelihood x (R v) ≤ likelihood x (R t) := hm hcv
    rw [← hvr] at HC
    exact (hmp hq).trans HC


/-- Both signs of one positive magnitude yield a unique Skellam winner,
including when the sign counts and hence the fitted rates are unequal. -/
theorem one_magnitude_unique {n k : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (hx : ∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ))
    (hpos : ∃ i, 0 < x i) (hneg : ∃ i, x i < 0) :
    ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p := by
  let d : ℝ := (∑ i, (x i : ℝ)) / n
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hd : ∑ i, (x i : ℝ) = (n : ℝ) * d := by
    dsimp [d]
    field_simp
  have hbox (i : Fin n) : -(k : ℝ) ≤ (x i : ℝ) ∧ (x i : ℝ) ≤ k := by
    rcases hx i with h | h <;> rw [h] <;> push_cast <;> constructor <;> linarith [Nat.cast_nonneg (α := ℝ) k]
  have hlt : d < (k : ℝ) := by
    obtain ⟨i, hi⟩ := hneg
    have h := Finset.sum_lt_sum (s := Finset.univ)
      (fun j _ ↦ (hbox j).2)
      ⟨i, Finset.mem_univ _, (show (x i : ℝ) < 0 by exact_mod_cast hi)
        |>.trans_le (Nat.cast_nonneg _)⟩
    simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, hd] at h
    exact lt_of_mul_lt_mul_left h hn'.le
  have hgt : -(k : ℝ) < d := by
    obtain ⟨i, hi⟩ := hpos
    have H : 0 < (x i : ℝ) := by exact_mod_cast hi
    have h := Finset.sum_lt_sum (s := Finset.univ) (fun j _ ↦ (hbox j).1)
      ⟨i, Finset.mem_univ _, (neg_nonpos.mpr (Nat.cast_nonneg _)).trans_lt H⟩
    simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, hd] at h
    exact lt_of_mul_lt_mul_left h hn'.le
  have hdk : d ^ 2 < (k : ℝ) ^ 2 := by
    rw [← sq_abs d]
    exact pow_lt_pow_left₀ (abs_lt.mpr ⟨hgt, hlt⟩) (abs_nonneg _) (by decide)
  have hrate (p : ℝ × ℝ) (hp : p ∈ domain) (hm : IsMaxOn (likelihood x) domain p) :
      0 < p.1 ∧ 0 < p.2 := by
    have hbase : ((1, 1) : ℝ × ℝ) ∈ domain := ⟨zero_le_one, zero_le_one⟩
    have hL : 0 < likelihood x p := (Finset.prod_pos fun i _ ↦
      mass_pos (x i) (p := (1, 1)) one_pos one_pos).trans_le (hm hbase)
    obtain ⟨a, b⟩ := p
    have ha : 0 ≤ a := hp.1
    have hb : 0 ≤ b := hp.2
    refine ⟨ha.lt_of_ne ?_, hb.lt_of_ne ?_⟩
    · intro e
      subst a
      obtain ⟨i, hi⟩ := hpos
      have H : likelihood x (0, b) = 0 := Finset.prod_eq_zero (Finset.mem_univ i)
        (mass_zero_left hi b)
      linarith
    · intro e
      subst b
      obtain ⟨i, hi⟩ := hneg
      have hz : mass (x i) (a, 0) = 0 := by
        rw [← mass_reflect_here]
        exact mass_zero_left (neg_pos.mpr hi) a
      have H : likelihood x (a, 0) = 0 := Finset.prod_eq_zero (Finset.mem_univ i) hz
      linarith
  have hroot (p : ℝ × ℝ) (hp : p ∈ domain) (hm : IsMaxOn (likelihood x) domain p) :
      0 < p.1 * p.2 ∧ p = rates d (p.1 * p.2) ∧ flux k d (p.1 * p.2) = 0 := by
    have hp' := hrate p hp hm
    have hu := mul_pos hp'.1 hp'.2
    have hr : rates d (p.1 * p.2) = p :=
      recover_rates hp'.1 hp'.2 (FinishSkellam.maximizing_rate_difference hn x p hp hm)
    refine ⟨hu, hr.symm, ?_⟩
    have hMax : IsMaxOn (fun u ↦ log (likelihood x (rates d u))) (Ioi 0) (p.1 * p.2) := by
      intro u hu'
      have H := rates_pos d hu'
      have hL : 0 < likelihood x (rates d u) := Finset.prod_pos fun i _ ↦ mass_pos (x i) H.1 H.2
      change log (likelihood x (rates d u)) ≤ log (likelihood x (rates d (p.1 * p.2)))
      rw [hr]
      exact log_le_log hL (hm (a := rates d u) ⟨H.1.le, H.2.le⟩)
    have hlocal := hMax.isLocalMax (Ioi_mem_nhds hu)
    have hmag (i : Fin n) : (x i).natAbs = k := by
      rcases hx i with h | h <;> simp [h]
    have H := general_profile_deriv x hu hd
    simp only [hmag, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at H
    have he :
        (n : ℝ) * (flux k d (p.1 * p.2) / radialSeries k (p.1 * p.2)) / (2 * (p.1 * p.2)) =
        (n : ℝ) * flux k d (p.1 * p.2) / (2 * (p.1 * p.2) * radialSeries k (p.1 * p.2)) := by
      ring
    rw [he] at H
    have hz := hlocal.hasDerivAt_eq_zero H
    exact (mul_eq_zero.mp ((div_eq_zero_iff.mp hz).resolve_right
      (by positivity [radialSeries_pos k hu.le]))).resolve_left hn'.ne'
  obtain ⟨p, hp, hm⟩ := joint_attained hn x
  refine ⟨p, ⟨hp, hm⟩, ?_⟩
  rintro q ⟨hq, hmq⟩
  have H := hroot p hp hm
  have G := hroot q hq hmq
  rw [G.2.1, flux_root_unique hdk G.1 H.1 G.2.2 H.2.2, ← H.2.1]

private theorem zero_skellam_origin {n : ℕ} (hn : 0 < n) :
    let x : Fin n → ℤ := fun _ ↦ 0
    ((0, 0) : ℝ × ℝ) ∈ domain ∧ IsMaxOn (likelihood x) domain (0, 0) ∧
      ∀ p ∈ domain, IsMaxOn (likelihood x) domain p → p = (0, 0) := by
  intro x
  have hbound (p : ℝ × ℝ) (hp : p ∈ domain) : mass 0 p ≤ 1 := by
    have hp₁ : 0 ≤ p.1 := hp.1
    have hp₂ : 0 ≤ p.2 := hp.2
    have hF : HasSum (poissonMass p.1) 1 :=
      ProbabilityTheory.hasSum_one_poissonMeasure ⟨p.1, hp₁⟩
    have hG : HasSum (poissonMass p.2) 1 :=
      ProbabilityTheory.hasSum_one_poissonMeasure ⟨p.2, hp₂⟩
    have hnon (j : ℕ) : 0 ≤ poissonMass p.1 j * poissonMass p.2 j := by
      unfold poissonMass
      positivity
    have hle (j : ℕ) : poissonMass p.1 j * poissonMass p.2 j ≤ poissonMass p.1 j := by
      apply mul_le_of_le_one_right (by unfold poissonMass; positivity)
      rw [← hG.tsum_eq]
      exact hG.summable.le_tsum j fun i _ ↦ by unfold poissonMass; positivity
    have hs := Summable.of_nonneg_of_le hnon hle hF.summable
    simpa [mass, hF.tsum_eq] using Summable.tsum_le_tsum hle hs hF.summable
  have hmass : mass 0 (0, 0) = 1 := by
    simp only [mass, neg_zero, Int.toNat_zero, Nat.add_zero]
    rw [tsum_eq_single 0]
    · simp [poissonMass]
    · intro j hj
      simp [poissonMass, zero_pow hj]
  have hp₀ : ((0, 0) : ℝ × ℝ) ∈ domain := ⟨le_rfl, le_rfl⟩
  have hm : IsMaxOn (likelihood x) domain (0, 0) := by
    intro p hp
    change (∏ _ : Fin n, mass 0 p) ≤ ∏ _ : Fin n, mass 0 (0, 0)
    simp only [hmass, Finset.prod_const, Finset.card_univ, Fintype.card_fin, one_pow]
    have hnon : 0 ≤ mass 0 p := by
      unfold mass
      apply tsum_nonneg
      intro j
      have ha : 0 ≤ p.1 := hp.1
      have hb : 0 ≤ p.2 := hp.2
      unfold poissonMass
      positivity
    exact pow_le_one₀ hnon (hbound p hp)
  obtain ⟨p, _, hu⟩ := Discrete4Skellam.constant_unique hn 0
  exact ⟨hp₀, hm, fun q hq hmq ↦ (hu q ⟨hq, hmq⟩).trans (hu _ ⟨hp₀, hm⟩).symm⟩

/-- The finite candidate rule holds for every nonempty sample. Constants,
both signs of a single magnitude, and either orientation of the supported
Poisson class have a unique winner. -/
theorem all_free_candidate_uniqueness {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    let d := (∑ i, (x i : ℝ)) / n
    let R := fun t : ℝ ↦ (t + max d 0, t + max (-d) 0)
    let G := fun (k : ℕ) (u : ℝ) ↦
      ∑' j : ℕ, u ^ j / (j.factorial : ℝ) / ((j + k).factorial : ℝ)
    let S := fun t : ℝ ↦ ∑ i, (((x i).natAbs : ℝ) - (2 * t + |d|) +
      2 * (t * (t + |d|)) * G ((x i).natAbs + 1) (t * (t + |d|)) /
        G (x i).natAbs (t * (t + |d|)))
    let P := fun t ↦ likelihood x (R t)
    let C := {t : ℝ | (0 < t ∧ S t = 0) ∨ (t = 0 ∧ 0 < P t)}
    (C.Finite ∧ ∀ q, (q ∈ domain ∧ IsMaxOn (likelihood x) domain q) ↔
      ∃ t ∈ C, q = R t ∧ IsMaxOn P C t) ∧
    ((∃ c, ∀ i, x i = c) → ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p) ∧
    ((∀ i, x i = 0) → ((0, 0) : ℝ × ℝ) ∈ domain ∧
      IsMaxOn (likelihood x) domain (0, 0) ∧
      ∀ p ∈ domain, IsMaxOn (likelihood x) domain p → p = (0, 0)) ∧
    ((∃ k : ℕ, (∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ)) ∧
      (∃ i, 0 < x i) ∧ (∃ i, x i < 0)) →
        ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p) ∧
    (∀ z : Fin n → ℕ, let d₀ : ℝ := (∑ i, (z i : ℝ)) / n
      0 < d₀ →
      ((∀ t ≥ 0, (∑ i, poissonMass t (z i) / ((n : ℝ) * poissonMass d₀ (z i))) ≤ 1) ∨
        (∃ j : ℕ, ∀ i, z i = j ∨ z i = j + 1)) →
      ((x = fun i ↦ (z i : ℤ)) → (d₀, 0) ∈ domain ∧ IsMaxOn (likelihood x) domain (d₀, 0) ∧
        ∀ p ∈ domain, IsMaxOn (likelihood x) domain p → p = (d₀, 0)) ∧
      ((x = fun i ↦ -(z i : ℤ)) → (0, d₀) ∈ domain ∧ IsMaxOn (likelihood x) domain (0, d₀) ∧
        ∀ p ∈ domain, IsMaxOn (likelihood x) domain p → p = (0, d₀))) := by
  intro d R G S P C
  refine ⟨finite_mean_line_candidates hn x, ?_, ?_, ?_, ?_⟩
  · rintro ⟨c, hc⟩
    rw [show x = fun _ ↦ c from funext hc]
    exact Discrete4Skellam.constant_unique hn c
  · intro hz
    rw [show x = fun _ ↦ 0 from funext hz]
    exact zero_skellam_origin hn
  · rintro ⟨k, hx, hp, hm⟩
    exact one_magnitude_unique hn x hx hp hm
  · intro z d₀ hd hs
    have H : ∀ t ≥ 0, (∑ i, poissonMass t (z i) / ((n : ℝ) * poissonMass d₀ (z i))) ≤ 1 := by
      rcases hs with h | ⟨j, hj⟩
      · exact h
      · exact Base7Skellam.adjacent_supporting_inequality hn z hj hd rfl
    constructor
    · intro hx
      rw [hx]
      exact Base7Skellam.nonnegative_supporting_unique hn z hd rfl H
    · intro hx
      rw [hx]
      exact Base7Skellam.nonpositive_supporting_unique hn z hd rfl H


end

end UnivariateML.Base7bSkellam

end
