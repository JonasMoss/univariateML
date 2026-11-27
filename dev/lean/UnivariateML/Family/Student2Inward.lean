module

public import UnivariateML.Family.Student2Half
public import UnivariateML.Family.Student2Normal

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Sample-checkable improvements on the Gaussian face

A one-sided quartic moment condition gives a strict finite Student
witness. The proof keeps an explicit sixth-order remainder.
-/

@[expose] public section

open Set Real Filter Topology

namespace UnivariateML.Student2Inward

private theorem normalizer_atTop :
    Tendsto TDist.logNormalizer atTop (𝓝 (-(log (2*π)/2))) := by
  have h := TDist.density_tendsto_normal 0 0 (s := 1) (a := fun _ ↦ 1)
    zero_lt_one tendsto_const_nhds
  have H : Tendsto (fun q ↦ exp (TDist.logNormalizer q)) atTop (𝓝 (exp (-(log (2*π)/2)))) := by
    simpa [TDist.density,TDist.normalDensity] using h
  simpa only [log_exp] using H.log (exp_pos _).ne'

private theorem normalizer_diff {q : ℝ} (hq : 0 < q) :
    DifferentiableAt ℝ TDist.logNormalizer q := by
  have h := (Student2Half.log_gamma_half_step_derivative hq).differentiableAt
  have hL : DifferentiableAt ℝ (fun v : ℝ ↦ log (v*π)/2) q :=
    ((by fun_prop : DifferentiableAt ℝ (fun v : ℝ ↦ v*π) q).log (by positivity)).div_const 2
  exact h.sub hL

/-- The Student log normalizer lies within one over four times shape
below its Gaussian limit. -/
theorem normalizer_ge_gaussian_sub {q : ℝ} (hq : 0 < q) :
    -(log (2*π)/2)-1/(4*q) ≤ TDist.logNormalizer q := by
  let G := fun q : ℝ ↦ TDist.logNormalizer q+ (1/4)/q
  have hd (v : ℝ) (hv : 0 < v) :
      HasDerivAt G (deriv TDist.logNormalizer v-1/(4*v^2)) v := by
    have h := (normalizer_diff hv).hasDerivAt.add
      ((hasDerivAt_const v (1/4:ℝ)).div (hasDerivAt_id v) hv.ne')
    convert h using 1
    · rfl
    · simp only [id_eq]
      ring
  have hM : AntitoneOn G (Ioi 0) := by
    apply antitoneOn_of_deriv_nonpos (convex_Ioi 0)
    · exact fun v hv ↦ (hd v hv).continuousAt.continuousWithinAt
    · intro v hv
      exact (hd v (interior_subset hv)).differentiableAt.differentiableWithinAt
    · intro v hv
      rw [interior_Ioi] at hv
      rw [(hd v hv).deriv]
      exact sub_nonpos.mpr (Student2Half.student_log_normalizer_score_bounds hv).2
  have h := normalizer_atTop.add (tendsto_inv_atTop_zero.const_mul (1/4:ℝ))
  have hh : Tendsto G atTop (𝓝 (-(log (2*π)/2))) := by
    simpa [G,div_eq_mul_inv] using h
  have H := le_of_tendsto hh ((eventually_ge_atTop q).mono fun v hv ↦ hM hq (hq.trans_le hv) hv)
  unfold G at H
  linarith [show (1/4:ℝ)/q=1/(4*q) by ring]

private theorem log_one_add_upper_cubic {z : ℝ} (hz : 0 ≤ z) :
    log (1+z) ≤ z-z^2/2+z^3/3 := by
  let F := fun z : ℝ ↦ z-z^2/2+z^3/3-log (1+z)
  have hd (t : ℝ) (ht : 0 ≤ t) : HasDerivAt F (t^3/(1+t)) t := by
    have h₁ := (hasDerivAt_id t).sub (((hasDerivAt_id t).pow 2).div_const 2)
    have h₂ := h₁.add (((hasDerivAt_id t).pow 3).div_const 3)
    have h := h₂.sub (((hasDerivAt_id t).const_add 1).log (by positivity : 1+t ≠ 0))
    convert h using 1
    · rfl
    · simp only [id_eq, Nat.cast_ofNat, Nat.reduceSub, pow_one, mul_one]
      field_simp
      ring
  have hm : MonotoneOn F (Ici 0) := by
    apply monotoneOn_of_deriv_nonneg (convex_Ici 0)
    · exact fun t ht ↦ (hd t ht).continuousAt.continuousWithinAt
    · exact fun t ht ↦ (hd t (interior_subset ht)).differentiableAt.differentiableWithinAt
    · intro t ht
      have hh : 0 ≤ t := interior_subset ht
      rw [(hd t hh).deriv]
      positivity
  have h0 : F 0 = 0 := by norm_num [F]
  exact sub_nonneg.mp (h0 ▸ hm (show (0:ℝ)∈Ici 0 from le_rfl) hz hz)

private theorem ordinary_kernel_remainder {v r : ℝ} (hv : 1 ≤ v) :
    (r^4-2*r^2)/(4*v)-r^6/(3*v^2) ≤
      r^2/2-(v+1)/2*log (1+r^2/v) := by
  have hv0 : 0 < v := zero_lt_one.trans_le hv
  let t := r^2/v
  have ht : 0 ≤ t := by dsimp [t]; positivity
  have hL := log_one_add_upper_cubic ht
  have he := mul_le_mul_of_nonneg_left hL (show 0 ≤ (v+1)/2 by positivity)
  have hh : r^2/2-(v+1)/2*(t-t^2/2+t^3/3) =
      (r^4-2*r^2)/(4*v)+r^4/(4*v^2)-(v+1)*r^6/(6*v^3) := by
    unfold t
    field_simp
    ring
  have hz : (v+1)*r^6/(6*v^3) ≤ r^6/(3*v^2) := by
    apply (div_le_iff₀ (by positivity : 0 < 6*v^3)).2
    have hf : r^6/(3*v^2)*(6*v^3) = (2*v)*r^6 := by field_simp; ring
    rw [hf]
    exact mul_le_mul_of_nonneg_right (by linarith) (by positivity)
  change _ ≤ r^2/2-(v+1)/2*log (1+t)
  linarith [show 0 ≤ r^4/(4*v^2) by positivity]

/-- At any held center and ordinary scale, a positive Gaussian inward
coefficient gives a strictly winning finite Student witness above any
prescribed finite shape floor. -/
theorem ordinary_gaussian_inward_witness {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s)
    (hK : 0 < ∑ i, (((x i-m)/s)^4-2*((x i-m)/s)^2-1))
    (floor : ℝ) :
    ∃ v > max floor 1, (∑ i, log (TDist.normalDensity m s (x i))) <
      TDist.logLik x m v s := by
  let r (i : Fin n) := (x i-m)/s
  let K := ∑ i, ((r i)^4-2*(r i)^2-1)
  let T := ∑ i, (r i)^6
  have hK0 : 0 < K := hK
  have hT : 0 ≤ T := Finset.sum_nonneg fun i _ ↦ by positivity
  let v := max (max floor 1) (4*T/(3*K)) + 1
  have hv : max floor 1 < v := by unfold v; linarith [le_max_left (max floor 1) (4*T/(3*K))]
  have hv1 : 1 ≤ v := (le_max_right _ _).trans hv.le
  have hv0 : 0 < v := zero_lt_one.trans_le hv1
  have hr : 4*T/(3*K) < v := by unfold v; linarith [le_max_right (max floor 1) (4*T/(3*K))]
  have hStrict : 0 < K/(4*v)-T/(3*v^2) := by
    have hh := (div_lt_iff₀ (show 0 < 3*K by positivity)).1 hr
    apply sub_pos.mpr
    apply (div_lt_iff₀ (by positivity : 0 < 3*v^2)).2
    have he : K/(4*v)*(3*v^2) = v*(3*K)/4 := by field_simp
    rw [he]
    linarith
  have hterm (i : Fin n) :
      log (TDist.normalDensity m s (x i)) +
        ((r i)^4-2*(r i)^2-1)/(4*v)-(r i)^6/(3*v^2) ≤
      TDist.logNormalizer v-log s-(v+1)/2*log (1+(x i-m)^2/(v*s^2)) := by
    have hh := ordinary_kernel_remainder (r := r i) hv1
    have hNorm := normalizer_ge_gaussian_sub hv0
    have he : (x i-m)^2/(v*s^2) = (r i)^2/v := by unfold r; field_simp
    rw [TDist.normalDensity,log_exp,he]
    have hi : (x i-m)^2/s^2 = (r i)^2 := by unfold r; rw [div_pow]
    rw [hi]
    linarith [show ((r i)^4-2*(r i)^2-1)/(4*v) =
      ((r i)^4-2*(r i)^2)/(4*v)-1/(4*v) by ring]
  have H := Finset.sum_le_sum (s := Finset.univ) fun i _ ↦ hterm i
  rw [Finset.sum_sub_distrib,Finset.sum_add_distrib,← Finset.sum_div,← Finset.sum_div] at H
  change (∑ i, log (TDist.normalDensity m s (x i))) + K/(4*v)-T/(3*v^2) ≤
    TDist.logLik x m v s at H
  exact ⟨v,hv,by linarith⟩


private theorem log_one_add_lower_quadratic {z : ℝ} (hz : 0 ≤ z) :
    z-z^2/2 ≤ log (1+z) := by
  refine le_trans ((le_div_iff₀ (by positivity : 0 < z+2)).2 ?_) (le_log_one_add_of_nonneg hz)
  nlinarith [show 0 ≤ z^2*z by positivity]

private theorem fixed_sd_kernel_remainder {t r : ℝ} (ht : 1 ≤ t) :
    (r^4-6*r^2)/(4*t)-(2/3)*r^6/t^2 ≤
      r^2/2-(t+3)/2*log (1+r^2/t) := by
  have ht0 : 0 < t := zero_lt_one.trans_le ht
  let z := r^2/t
  have hz : 0 ≤ z := by dsimp [z]; positivity
  have he := mul_le_mul_of_nonneg_left (log_one_add_upper_cubic hz)
    (show 0 ≤ (t+3)/2 by positivity)
  have hh : r^2/2-(t+3)/2*(z-z^2/2+z^3/3) =
      (r^4-6*r^2)/(4*t)+3*r^4/(4*t^2)-(t+3)*r^6/(6*t^3) := by
    unfold z
    field_simp
    ring
  have hb : (t+3)*r^6/(6*t^3) ≤ (2/3)*r^6/t^2 := by
    apply (div_le_iff₀ (by positivity : 0 < 6*t^3)).2
    have hf : (2/3)*r^6/t^2*(6*t^3) = (4*t)*r^6 := by field_simp; ring
    rw [hf]
    exact mul_le_mul_of_nonneg_right (by linarith) (by positivity)
  change _ ≤ r^2/2-(t+3)/2*log (1+z)
  linarith [show 0 ≤ 3*r^4/(4*t^2) by positivity]

/-- A positive Gaussian inward coefficient for held standard deviation
gives a strictly better finite Student candidate, above any finite floor. -/
theorem standard_gaussian_inward_witness {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s)
    (hK : 0 < ∑ i, (((x i-m)/s)^4-6*((x i-m)/s)^2+3))
    (floor : ℝ) :
    ∃ v > max floor 2, (∑ i, log (TDist.normalDensity m s (x i))) <
      TDist.stdLogLik x m v s := by
  let r (i : Fin n) := (x i-m)/s
  let K := ∑ i, ((r i)^4-6*(r i)^2+3)
  let T := ∑ i, (1+(2/3)*(r i)^6)
  have hK0 : 0 < K := hK
  let t := max (max floor 1) (4*T/K) + 1
  have ht : max floor 1 < t := by unfold t; linarith [le_max_left (max floor 1) (4*T/K)]
  have ht1 : 1 ≤ t := (le_max_right _ _).trans ht.le
  have ht0 : 0 < t := zero_lt_one.trans_le ht1
  have hB : 4*T/K < t := by unfold t; linarith [le_max_right (max floor 1) (4*T/K)]
  have hv : max floor 2 < t+2 := by
    have h1 : floor < t := (le_max_left _ _).trans_lt ht
    exact max_lt (by linarith) (by linarith)
  have hStrict : 0 < K/(4*t)-T/t^2 := by
    have h := (div_lt_iff₀ hK0).1 hB
    apply sub_pos.mpr
    apply (div_lt_iff₀ (by positivity : 0 < t^2)).2
    have he : K/(4*t)*t^2 = t*K/4 := by field_simp
    rw [he]
    linarith
  have hC : -(log (2*π)/2)+3/(4*t)-1/t^2 ≤
      TDist.logNormalizer (t+2)+(log (t+2)-log t)/2 := by
    have h1 := normalizer_ge_gaussian_sub (show 0 < t+2 by positivity)
    have h2 := log_one_add_lower_quadratic (show 0 ≤ 2/t by positivity)
    have h3 : log (t+2)-log t = log (1+2/t) := by
      rw [← log_div (by positivity) ht0.ne']
      congr 1
      field_simp
    have h4 : 1/(4*(t+2)) ≤ 1/(4*t) := one_div_le_one_div_of_le (by positivity) (by linarith)
    rw [h3]
    have hh : ((2:ℝ)/t)^2/2 = 2/t^2 := by field_simp
    rw [hh] at h2
    rw [show 3/(4*t) = 1/t-1/(4*t) by field_simp; ring]
    have h5 := div_le_div_of_nonneg_right h2 (by norm_num : (0:ℝ) ≤ 2)
    rw [show (((2:ℝ)/t-2/t^2)/2) = 1/t-1/t^2 by ring] at h5
    linarith only [h1,h4,h5]
  have hterm (i : Fin n) :
      log (TDist.normalDensity m s (x i)) +
        ((r i)^4-6*(r i)^2+3)/(4*t)-(1+(2/3)*(r i)^6)/t^2 ≤
      log (Gamma (((t+2)+1)/2)) - log (Gamma ((t+2)/2)) -
        log (((t+2)-2)*π)/2 - log s -
        ((t+2)+1)/2 * log (1+(x i-m)^2/(((t+2)-2)*s^2)) := by
    have hh := fixed_sd_kernel_remainder (r := r i) ht1
    have he : (x i-m)^2/(t*s^2) = (r i)^2/t := by unfold r; field_simp
    have hi : (x i-m)^2/s^2 = (r i)^2 := by unfold r; rw [div_pow]
    have ht2 : t+2-2 = t := by ring
    have hNorm : TDist.logNormalizer (t+2)+(log (t+2)-log t)/2 =
        log (Gamma ((t+2+1)/2))-log (Gamma ((t+2)/2))-log (t*π)/2 := by
      unfold TDist.logNormalizer
      rw [log_mul (by positivity : t+2 ≠ 0) pi_pos.ne',log_mul ht0.ne' pi_pos.ne']
      ring
    rw [TDist.normalDensity,log_exp,hi,ht2,he,← hNorm]
    have hplus : ((t+2)+1)/2 = (t+3)/2 := by ring
    rw [hplus]
    linarith [show ((r i)^4-6*(r i)^2+3)/(4*t) =
      ((r i)^4-6*(r i)^2)/(4*t)+3/(4*t) by ring,
      show (1+(2/3)*(r i)^6)/t^2 = 1/t^2+(2/3)*(r i)^6/t^2 by ring]
  have H := Finset.sum_le_sum (s := Finset.univ) fun i _ ↦ hterm i
  rw [Finset.sum_sub_distrib,Finset.sum_add_distrib,← Finset.sum_div,← Finset.sum_div] at H
  change (∑ i, log (TDist.normalDensity m s (x i))) + K/(4*t)-T/t^2 ≤
    TDist.stdLogLik x m (t+2) s at H
  exact ⟨t+2,hv,by linarith⟩

/-- A two-sided normalizer enclosure with width (1/(24 q^3)).
It is suitable as the terminal enclosure for recurrence descent. -/
theorem normalizer_le_gaussian_cubic {q : ℝ} (hq : 0 < q) :
    TDist.logNormalizer q ≤ -(log (2*π)/2)-1/(4*q)+1/(24*q^3) := by
  let G := fun q : ℝ ↦ TDist.logNormalizer q+(1/4)/q-(1/24)/q^3
  have hd (v : ℝ) (hv : 0 < v) :
      HasDerivAt G (deriv TDist.logNormalizer v-1/(4*v^2)+1/(8*v^4)) v := by
    have h1 := (normalizer_diff hv).hasDerivAt.add
      ((hasDerivAt_const v (1/4:ℝ)).div (hasDerivAt_id v) hv.ne')
    have h2 := (hasDerivAt_const v (1/24:ℝ)).div ((hasDerivAt_id v).pow 3)
      (pow_ne_zero _ hv.ne')
    convert h1.sub h2 using 1
    · rfl
    · simp only [Pi.pow_apply,id_eq,zero_mul,mul_one,zero_sub]
      field_simp
      <;> ring
  have hM : MonotoneOn G (Ioi 0) := by
    apply monotoneOn_of_deriv_nonneg (convex_Ioi 0)
    · exact fun v hv ↦ (hd v hv).continuousAt.continuousWithinAt
    · intro v hv
      exact (hd v (interior_subset hv)).differentiableAt.differentiableWithinAt
    · intro v hv
      rw [interior_Ioi] at hv
      rw [(hd v hv).deriv]
      linarith [Student2Half.student_log_normalizer_score_cubic_lower hv]
  have h := (normalizer_atTop.add (tendsto_inv_atTop_zero.const_mul (1/4:ℝ))).sub
    ((tendsto_inv_atTop_zero.pow 3).const_mul (1/24:ℝ))
  have hh : Tendsto G atTop (𝓝 (-(log (2*π)/2))) := by
    simpa [G,div_eq_mul_inv,inv_pow] using h
  have H := ge_of_tendsto hh ((eventually_ge_atTop q).mono fun v hv ↦ hM hq (hq.trans_le hv) hv)
  unfold G at H
  linarith [show (1/4:ℝ)/q=1/(4*q) by ring,show (1/24:ℝ)/q^3=1/(24*q^3) by ring]

/-- Exact log-normalizer step used to descend from the certified
large-shape enclosure to a prescribed small shape. -/
theorem normalizer_step_two {q : ℝ} (hq : 0 < q) :
    TDist.logNormalizer (q+2) =
      TDist.logNormalizer q+log (1+1/(q*(q+2)))/2 := by
  have hq2 : 0 < q+2 := by positivity
  have h1 : 0 < (q+1)/2 := by positivity
  have h2 : 0 < q/2 := half_pos hq
  have hγ1 := Gamma_pos_of_pos h1
  have hγ2 := Gamma_pos_of_pos h2
  have hpos : 0 < (q+1)/q := by positivity
  have hR : 1+1/(q*(q+2)) = ((q+1)^2)/(q*(q+2)) := by field_simp <;> ring
  unfold TDist.logNormalizer
  rw [show (q+2+1)/2 = (q+1)/2+1 by ring,show (q+2)/2=q/2+1 by ring,
    Gamma_add_one h1.ne',Gamma_add_one h2.ne',
    log_mul h1.ne' hγ1.ne',log_mul h2.ne' hγ2.ne',hR,
    log_div (show (q+1)^2 ≠ 0 by positivity) (mul_ne_zero hq.ne' hq2.ne'),
    log_pow,log_mul hq.ne' hq2.ne',
    log_mul hq.ne' pi_pos.ne',log_mul hq2.ne' pi_pos.ne']
  have ha : log ((q+1)/2) = log (q+1)-log 2 := log_div (by positivity) two_ne_zero
  have hb : log (q/2) = log q-log 2 := log_div hq.ne' two_ne_zero
  rw [ha,hb]
  push_cast
  ring


/-- A finite recurrence descent gives the same analytic enclosure at
any positive shape, with error starting at the shifted shape. -/
theorem normalizer_descended_bounds {q : ℝ} (hq : 0 < q) (j : ℕ) :
    let r : ℝ := q+2*j
    let S := ∑ i ∈ Finset.range j, log (1+1/((q+2*i)*(q+2*i+2)))/2
    (-(log (2*π)/2)-1/(4*r)-S ≤ TDist.logNormalizer q ∧
      TDist.logNormalizer q ≤ -(log (2*π)/2)-1/(4*r)+1/(24*r^3)-S) := by
  let A := TDist.logNormalizer
  let H := fun i : ℕ ↦ log (1+1/((q+2*i)*(q+2*i+2)))/2
  have he (k : ℕ) : A (q+2*k) = A q+∑ i ∈ Finset.range k, H i := by
    induction k with
    | zero => simp
    | succ k ih =>
      rw [Finset.sum_range_succ, ← add_assoc, ← ih, Nat.cast_succ,
        show q+2*((k:ℝ)+1) = q+2*k+2 by ring]
      exact normalizer_step_two (by positivity)
  let r : ℝ := q+2*j
  have hr : 0 < r := by unfold r; positivity
  have hlo := normalizer_ge_gaussian_sub hr
  have hhi := normalizer_le_gaussian_cubic hr
  have hE := he j
  change -(log (2*π)/2)-1/(4*r)-(∑ i ∈ Finset.range j, H i) ≤ A q ∧
    A q ≤ -(log (2*π)/2)-1/(4*r)+1/(24*r^3)-(∑ i ∈ Finset.range j, H i)
  change -(log (2*π)/2)-1/(4*r) ≤ A r at hlo
  change A r ≤ -(log (2*π)/2)-1/(4*r)+1/(24*r^3) at hhi
  change A r = A q+∑ i ∈ Finset.range j, H i at hE
  constructor <;> linarith

/-- Exact two-unit recurrence for the digamma half-step represented as
a Laplace integral. It descends the derivative enclosure in the same
chart as the log normalizer. -/
theorem gamma_half_step_score_recurrence {q : ℝ} (hq : 0 < q) :
    (∫ t in Ioi (0:ℝ), exp (-q*t)/(1+exp (-t))) =
      (∫ t in Ioi (0:ℝ), exp (-(q+2)*t)/(1+exp (-t)))+1/(q*(q+1)) := by
  let J := fun v : ℝ ↦ ∫ t in Ioi (0:ℝ), exp (-v*t)/(1+exp (-t))
  let g := fun v : ℝ ↦ log (Gamma ((v+1)/2))-log (Gamma (v/2))
  let h := fun v : ℝ ↦ log (v+1)-log v
  have he {v : ℝ} (hv : 0 < v) : g (v+2) = g v+h v := by
    unfold g h
    rw [show (v+2+1)/2 = (v+1)/2+1 by ring,show (v+2)/2=v/2+1 by ring,
      Gamma_add_one (by positivity),Gamma_add_one (by positivity),
      log_mul (by positivity : (v+1)/2 ≠ 0)
        (Gamma_pos_of_pos (by positivity : 0 < (v+1)/2)).ne',
      log_mul (half_pos hv).ne' (Gamma_pos_of_pos (half_pos hv)).ne',
      log_div (by positivity : v+1 ≠ 0) two_ne_zero,log_div hv.ne' two_ne_zero]
    ring
  have hL : HasDerivAt (fun v ↦ g (v+2)) (J (q+2)) q := by
    have H := (Student2Half.log_gamma_half_step_derivative (show 0 < q+2 by positivity)).comp q
      ((hasDerivAt_id q).add_const 2)
    change HasDerivAt (fun v ↦ g (v+2)) (J (q+2)*1) q at H
    simpa only [mul_one] using H
  have hD : HasDerivAt h ((q+1)⁻¹-q⁻¹) q := by
    have H := (((hasDerivAt_id q).add_const 1).log (show q+1 ≠ 0 by positivity)).sub
      (hasDerivAt_log hq.ne')
    change HasDerivAt h (1/(q+1)-q⁻¹) q at H
    simpa only [one_div] using H
  have hR : HasDerivAt (fun v ↦ g v+h v) (J q+((q+1)⁻¹-q⁻¹)) q :=
    (Student2Half.log_gamma_half_step_derivative hq).add hD
  have hR' := hR.congr_of_eventuallyEq
    (by filter_upwards [Ioi_mem_nhds hq] with v hv; exact he hv)
  have H := hL.unique hR'
  change J q = J (q+2)+1/(q*(q+1))
  have hId : (q+1)⁻¹-q⁻¹ = -(1/(q*(q+1))) := by field_simp <;> ring
  rw [hId] at H
  linarith


end UnivariateML.Student2Inward
end
