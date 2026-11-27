module

public import UnivariateML.Family.SkewLSSN
public import UnivariateML.Family.ClosedForm
public import UnivariateML.Generic.FinishProfile
import Mathlib.Tactic

set_option linter.style.header false

/-!
# All-free Azzalini skew normal in inverse-scale coordinates

## Deviations

The probability model is the Azzalini skew normal (dossier `sn`). The
stationary criterion quantifies over all finite slants. It does not assert
analyticity, a finite root list, or accuracy of a numerical comparison.
-/

@[expose] public section

open Set Filter Topology Real
open UnivariateML.SkewLSGaussian UnivariateML.SkewLSSN

namespace UnivariateML.FinishSN

noncomputable section

private def gaussianLoss (z : ℝ) := z^2/2

private theorem gaussian_coercive : LocationScale.LinearLowerBound gaussianLoss := by
  refine ⟨1, zero_lt_one, 1, fun z ↦ ?_⟩
  unfold gaussianLoss
  nlinarith only [sq_nonneg (|z|-1), sq_abs z]

private def constant (n : ℕ) : ℝ := n * log (2*φ 0)

private def envelope {n : ℕ} (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  LocationScale.inverseLogLik gaussianLoss x p + constant n

/-- Inverse-scale chart `(α, (b,u)) = (α, (1/ω, ξ/ω))`. -/
@[no_expose] def objective {n : ℕ} (x : Fin n → ℝ) (α : ℝ) (p : ℝ × ℝ) : ℝ :=
  envelope x p + ∑ i, log (Φ (α*(p.1*x i-p.2)))

private theorem phi_relative (z : ℝ) : φ z = φ 0 * exp (-(z^2/2)) := by
  simp [phi_formula]

/-- The inverse-scale objective is exactly the normalized physical log likelihood. -/
theorem objective_transfer {n : ℕ} (x : Fin n → ℝ) (α : ℝ)
    (p : LocationScale.domain) :
    objective x α (LocationScale.inverseEquiv p) = logLik α x p := by
  rw [objective, envelope, LocationScale.inverseLogLik_inverseEquiv]
  have hw : 0 < p.val.2 := p.property.2
  have hc : (∑ _ : Fin n, log (2*φ 0)) = constant n := by
    simp [constant, nsmul_eq_mul]
  have hz (i : Fin n) : (LocationScale.inverseEquiv p).val.1*x i -
      (LocationScale.inverseEquiv p).val.2 = (x i-p.val.1)/p.val.2 := by
    change p.val.2⁻¹*x i-p.val.1/p.val.2 = _
    ring
  simp only [hz, ← hc, LocationScale.logLik, ← Finset.sum_add_distrib, logLik]
  apply Finset.sum_congr rfl
  intro i _
  let z := (x i-p.val.1)/p.val.2
  change -log p.val.2 - gaussianLoss z + log (2*φ 0) + log (Φ (α*z)) =
    log ((2*φ z/p.val.2)*Φ (α*z))
  rw [phi_relative z, show 2 * (φ 0 * exp (-(z^2/2))) = (2*φ 0)*exp (-(z^2/2)) by ring]
  have hC : 0 < 2*φ 0 := mul_pos (by norm_num) (phi_pos _)
  rw [log_mul (div_ne_zero (mul_ne_zero hC.ne' (exp_pos _).ne') hw.ne') (cdf_pos _).ne',
    log_div (mul_ne_zero hC.ne' (exp_pos _).ne') hw.ne',
    log_mul hC.ne' (exp_pos _).ne', log_exp]
  unfold gaussianLoss
  ring

private theorem logs_negative {n : ℕ} (x : Fin n → ℝ) (α : ℝ) (p : ℝ × ℝ) (hn : 0 < n) :
    (∑ i, log (Φ (α*(p.1*x i-p.2)))) < 0 := by
  let : Nonempty (Fin n) := ⟨⟨0,hn⟩⟩
  simpa using Finset.sum_neg (s := Finset.univ)
    (fun i _ ↦ log_neg (cdf_pos _) (cdf_lt_one _)) Finset.univ_nonempty

private theorem objective_le_envelope {n : ℕ} (x : Fin n → ℝ) (α : ℝ) (p : ℝ × ℝ) :
    objective x α p ≤ envelope x p := by
  unfold objective
  exact add_le_of_nonpos_right (Finset.sum_nonpos fun i _ ↦
    log_nonpos (cdf_pos _).le (cdf_lt_one _).le)

/-- Every log-likelihood superlevel confines location and positive scale
to one compact nuisance set, uniformly over all finite slants. -/
theorem nuisance_confinement {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (L : ℝ) : ∃ K : Set LocationScale.inverseDomain, IsCompact K ∧
      ∀ (α : ℝ) (p : LocationScale.inverseDomain), L ≤ objective x α p → p ∈ K := by
  have h := LocationScale.tendsto_inverseLogLik_cocompact gaussian_coercive hx
  have he := h.eventually (eventually_lt_atBot (L-constant n))
  obtain ⟨K,hK,hsub⟩ := Filter.mem_cocompact'.mp he
  refine ⟨K,hK,fun α p hp ↦ hsub ?_⟩
  change ¬ LocationScale.inverseLogLik gaussianLoss x p < L-constant n
  have hh := objective_le_envelope x α p
  unfold envelope at hh
  linarith

/-- The optimized log value of a half-normal face at a compatible sample endpoint. -/
def face {n : ℕ} (x : Fin n → ℝ) (c : ℝ) : ℝ :=
  Normal.logLik x c (Normal.sd x c) + n * log 2

private theorem envelope_normal {n : ℕ} (x : Fin n → ℝ) (p : LocationScale.domain) :
    envelope x (LocationScale.inverseEquiv p) = Normal.logLik x p.val.1 p.val.2 + n*log 2 := by
  have hw : 0 < p.val.2 := p.property.2
  have hc : log (2*φ 0) = log 2 - log (2*π)/2 := by
    rw [log_mul (by norm_num) (phi_pos _).ne']
    rw [phi_formula 0]
    simp only [zero_pow (by decide : 2 ≠ 0), zero_div, neg_zero, exp_zero, mul_one]
    rw [one_div, log_inv, log_sqrt (by positivity : 0 ≤ 2*π)]
    ring
  rw [envelope, LocationScale.inverseLogLik_inverseEquiv, Normal.logLik_formula x _ hw]
  unfold LocationScale.logLik gaussianLoss constant Normal.residual
  have hs : (∑ i, ((x i-p.val.1)/p.val.2)^2/2) =
      (∑ i, (x i-p.val.1)^2)/(2*p.val.2^2) := by
    rw [Finset.sum_div]
    apply Finset.sum_congr rfl
    intro i _
    rw [div_pow]
    ring
  simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, hs, hc]
  ring

private theorem endpoint_scale_max {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (c : ℝ) :
    0 < Normal.sd x c ∧
      ∀ b > 0, envelope x (b,b*c) ≤ face x c := by
  have hn : 0 < n := by obtain ⟨i,_,_⟩ := hx; exact i.isLt.pos
  have hne : ∃ i, x i ≠ c := by
    obtain ⟨i,j,hij⟩ := hx
    by_contra! h
    exact hij ((h i).trans (h j).symm)
  have hm := (Normal.heldMean hn hne (Normal.sd x c)).mpr rfl
  refine ⟨hm.1,fun b hb ↦ ?_⟩
  let p : LocationScale.domain := ⟨(c,b⁻¹),⟨mem_univ _, inv_pos.mpr hb⟩⟩
  have he : (LocationScale.inverseEquiv p).val = (b,b*c) := by
    change ((b⁻¹)⁻¹,c/b⁻¹) = _
    simp [div_inv_eq_mul, mul_comm]
  rw [← he, envelope_normal, face]
  exact add_le_add_left (hm.2 (show b⁻¹ ∈ Ioi 0 from inv_pos.mpr hb)) _

private theorem logcdf_tangent {z : ℝ} (hz : z ≤ 0) :
    log (Φ z) ≤ log (1/2) + (φ 0 / Φ 0)*z := by
  rcases eq_or_lt_of_le hz with rfl | hz
  · simp [cdf_zero]
  have hd : HasDerivAt (fun t ↦ -log (Φ t)) (-(φ 0/Φ 0)) 0 :=
    ((cdf_hasDerivAt 0).log (cdf_pos _).ne').neg
  have h := cdf_strictLogConcave.concaveOn.neg.slope_le_of_hasDerivAt
    (mem_univ z) (mem_univ 0) hz hd
  rw [slope_def_field] at h
  change (-log (Φ 0) - -log (Φ z))/(0-z) ≤ -(φ 0/Φ 0) at h
  have hh := (div_le_iff₀ (sub_pos.mpr hz)).mp h
  rw [cdf_zero] at hh ⊢
  linarith only [hh]

private theorem endpoint_difference {n : ℕ} (x : Fin n → ℝ) (c b t : ℝ) :
    envelope x (b,b*c+t) = envelope x (b,b*c) +
      b*(∑ j, (x j-c))*t-(n : ℝ)*t^2/2 := by
  have he (j : Fin n) :
      log b - gaussianLoss (b*x j-(b*c+t)) =
      (log b - gaussianLoss (b*x j-b*c)) + (b*(x j-c)*t - t^2/2) := by
    unfold gaussianLoss
    ring
  unfold envelope LocationScale.inverseLogLik
  simp only [he, Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.sum_mul, ← Finset.mul_sum]
  ring

/-- A sufficiently large positive slant is strictly below the optimized
right half-normal face, uniformly over all feasible nuisance parameters. -/
theorem right_tail {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    ∃ A > 0, ∀ α ≥ A, ∀ p ∈ LocationScale.inverseDomain, objective x α p < face x (x i) := by
  let c := x i
  let D := ∑ j, (x j-c)
  let R := φ 0/Φ 0
  have hR : 0 < R := div_pos (phi_pos _) (cdf_pos _)
  have hD : 0 ≤ D := Finset.sum_nonneg fun j _ ↦ sub_nonneg.mpr (hi j)
  obtain ⟨K,hK,hconf⟩ := nuisance_confinement hx (face x c)
  obtain ⟨B,hB⟩ := (hK.image (continuous_fst.comp continuous_subtype_val)).bddAbove
  refine ⟨max 1 (B*D/R), (zero_lt_one : (0 : ℝ) < 1).trans_le (le_max_left _ _), ?_⟩
  intro α hα p hp
  by_contra! hP
  have hb : 0 < p.1 := hp.1
  have hbB : p.1 ≤ B := hB ⟨⟨p,hp⟩,hconf α ⟨p,hp⟩ hP,rfl⟩
  have hA : B*D ≤ R*α := by
    simpa only [mul_comm α R] using (div_le_iff₀ hR).mp ((le_max_right _ _).trans hα)
  have hα0 : 0 ≤ α := zero_le_one.trans ((le_max_left _ _).trans hα)
  have hbase := (endpoint_scale_max hx c).2 p.1 hb
  let t := p.2-p.1*c
  have he : envelope x p = envelope x (p.1,p.1*c) + p.1*D*t - (n : ℝ)*t^2/2 := by
    have hp' : (p.1,p.1*c+t) = p := by ext <;> simp [t]
    simpa only [hp'] using endpoint_difference x c p.1 t
  rcases le_or_gt t 0 with ht | ht
  · have hg := mul_nonpos_of_nonneg_of_nonpos (mul_nonneg hb.le hD) ht
    have hneg := logs_negative x α p i.isLt.pos
    unfold objective at hP
    rw [he] at hP
    nlinarith only [hP,hbase,hg,hneg,mul_nonneg (Nat.cast_nonneg n : (0:ℝ) ≤ n) (sq_nonneg t)]
  · have hz : α*(p.1*x i-p.2) = -(α*t) := by dsimp [t,c]; ring
    have hsum : (∑ j, log (Φ (α*(p.1*x j-p.2)))) ≤ log (Φ (-(α*t))) := by
      rw [← Finset.sum_erase_add _ _ (Finset.mem_univ i), ← hz]
      exact add_le_of_nonpos_left (Finset.sum_nonpos fun j _ ↦
        log_nonpos (cdf_pos _).le (cdf_lt_one _).le)
    have htangent := logcdf_tangent (neg_nonpos.mpr (mul_nonneg hα0 ht.le))
    have hgain := mul_le_mul_of_nonneg_right
      ((mul_le_mul_of_nonneg_right hbB hD).trans hA) ht.le
    have hlog := log_neg (by norm_num : (0 : ℝ) < 1/2) (by norm_num : (1/2 : ℝ) < 1)
    unfold objective at hP
    rw [he] at hP
    nlinarith only [hP,hbase,hsum,htangent,hgain,hlog,
      mul_nonneg (Nat.cast_nonneg n : (0:ℝ) ≤ n) (sq_nonneg t)]


private theorem reflect_objective {n : ℕ} (x : Fin n → ℝ) (α b u : ℝ) :
    objective (fun j ↦ -x j) (-α) (b,-u) = objective x α (b,u) := by
  unfold objective envelope LocationScale.inverseLogLik gaussianLoss
  simp only
  have h (j : Fin n) : b * -x j - -u = -(b*x j-u) := by ring
  simp only [h, neg_sq, neg_mul_neg]

private theorem reflect_face {n : ℕ} (x : Fin n → ℝ) (c : ℝ)
    (hx : LocationScale.Nonconstant x) : face (fun j ↦ -x j) (-c) = face x c := by
  have hxr : LocationScale.Nonconstant (fun j ↦ -x j) := by
    obtain ⟨i,j,h⟩ := hx
    exact ⟨i,j,fun he ↦ h (neg_injective he)⟩
  have h (j : Fin n) : (-x j - -c)^2 = (x j-c)^2 := by ring
  have hr : Normal.residual (fun j ↦ -x j) (-c) = Normal.residual x c := by
    simp only [Normal.residual,h]
  have hs : Normal.sd (fun j ↦ -x j) (-c) = Normal.sd x c := by simp only [Normal.sd,hr]
  unfold face
  rw [Normal.logLik_formula _ _ (endpoint_scale_max hxr (-c)).1,
    Normal.logLik_formula _ _ (endpoint_scale_max hx c).1, hs, hr]

/-- A sufficiently negative slant is strictly below the optimized
left half-normal face, uniformly over all feasible nuisance parameters. -/
theorem left_tail {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x j ≤ x i) :
    ∃ A > 0, ∀ α ≤ -A, ∀ p ∈ LocationScale.inverseDomain, objective x α p < face x (x i) := by
  have hxr : LocationScale.Nonconstant (fun j ↦ -x j) := by
    obtain ⟨i,j,h⟩ := hx
    exact ⟨i,j,fun he ↦ h (neg_injective he)⟩
  obtain ⟨A,hA,ht⟩ := right_tail hxr i (fun j ↦ neg_le_neg (hi j))
  refine ⟨A,hA,fun α hα ⟨b,u⟩ hbu ↦ ?_⟩
  have hp : (b,-u) ∈ LocationScale.inverseDomain := ⟨hbu.1,mem_univ _⟩
  simpa only [reflect_objective, reflect_face x (x i) hx] using
    ht (-α) (by linarith) (b,-u) hp

private theorem log_product {n : ℕ} (x : Fin n → ℝ) (α c : ℝ) {s : ℝ}
    (hs : 0 < s) :
    log (∏ j, SkewLSSN.density α c s (x j)) = logLik α x (c,s) := by
  unfold logLik
  apply log_prod
  intro j _
  unfold SkewLSSN.density
  exact (mul_pos (div_pos (mul_pos (by norm_num) (phi_pos _)) hs) (cdf_pos _)).ne'

private theorem envelope_log_product {n : ℕ} (x : Fin n → ℝ) (c : ℝ) {s : ℝ} (hs : 0 < s) :
    0 < halfNormalEnvelope x c s ∧
      log (halfNormalEnvelope x c s) = Normal.logLik x c s + n*log 2 := by
  let p : LocationScale.domain := ⟨(c,s),⟨mem_univ _,hs⟩⟩
  have hden (j : Fin n) : 0 < 2*φ ((x j-c)/s)/s :=
    div_pos (mul_pos (by norm_num) (phi_pos _)) hs
  refine ⟨Finset.prod_pos (fun j _ ↦ hden j), ?_⟩
  rw [← envelope_normal x p, halfNormalEnvelope, log_prod (fun j _ ↦ (hden j).ne'),
    envelope, LocationScale.inverseLogLik_inverseEquiv]
  have hC : 0 < 2*φ 0 := mul_pos (by norm_num) (phi_pos _)
  rw [constant, ← show (∑ _ : Fin n, log (2*φ 0)) = n*log (2*φ 0) by simp [nsmul_eq_mul],
    LocationScale.logLik, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro j _
  let z := (x j-c)/s
  change log (2*φ z/s) = -log s - gaussianLoss z + log (2*φ 0)
  rw [phi_relative z, show 2 * (φ 0 * exp (-(z^2/2))) = (2*φ 0)*exp (-(z^2/2)) by ring,
    log_div (mul_ne_zero hC.ne' (exp_pos _).ne') hs.ne',
    log_mul hC.ne' (exp_pos _).ne', log_exp, gaussianLoss]
  ring

private theorem continuous_objective {n : ℕ} (x : Fin n → ℝ) :
    Continuous (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2) := by
  have hb : Continuous (fun ap : ℝ × LocationScale.inverseDomain ↦ ap.2.val.1) :=
    continuous_fst.comp (continuous_subtype_val.comp continuous_snd)
  have hu : Continuous (fun ap : ℝ × LocationScale.inverseDomain ↦ ap.2.val.2) :=
    continuous_snd.comp (continuous_subtype_val.comp continuous_snd)
  have hz (j : Fin n) : Continuous
      (fun ap : ℝ × LocationScale.inverseDomain ↦ ap.2.val.1*x j-ap.2.val.2) :=
    (hb.mul continuous_const).sub hu
  unfold objective envelope LocationScale.inverseLogLik gaussianLoss
  apply Continuous.add
  · apply Continuous.add _ continuous_const
    exact continuous_finsetSum _ fun j _ ↦
      (hb.log fun ap ↦ (show 0 < ap.2.val.1 from ap.2.property.1).ne').sub
        ((hz j).pow 2 |>.div_const 2)
  · exact continuous_finsetSum _ fun j _ ↦
      (cdf_continuous.comp (continuous_fst.mul (hz j))).log (fun _ ↦ (cdf_pos _).ne')

/-- The full optimized right face is reachable as a moving-location log-likelihood limit. -/
theorem right_face_limit {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    Tendsto (fun t : ℝ ↦ logLik (t^2) x (x i-t⁻¹,Normal.sd x (x i))) atTop
      (𝓝 (face x (x i))) := by
  have hs := (endpoint_scale_max hx (x i)).1
  obtain ⟨hp,he⟩ := envelope_log_product x (x i) hs
  exact (he ▸ ((continuousAt_log hp.ne').tendsto.comp
    (right_moving_half_normal x hs hi))).congr
      (fun t ↦ log_product x (t^2) (x i-t⁻¹) hs)

/-- The full optimized left face is reachable as a moving-location log-likelihood limit. -/
theorem left_face_limit {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x j ≤ x i) :
    Tendsto (fun t : ℝ ↦ logLik (-(t^2)) x (x i+t⁻¹,Normal.sd x (x i))) atTop
      (𝓝 (face x (x i))) := by
  have hs := (endpoint_scale_max hx (x i)).1
  obtain ⟨hp,he⟩ := envelope_log_product x (x i) hs
  exact (he ▸ ((continuousAt_log hp.ne').tendsto.comp
    (left_moving_half_normal x hs hi))).congr
      (fun t ↦ log_product x (-(t^2)) (x i+t⁻¹) hs)

private theorem faces_below_bound {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) {L : ℝ}
    (hL : ∀ (α : ℝ) (p : LocationScale.inverseDomain), objective x α p ≤ L) :
    max (face x (x i)) (face x (x j)) ≤ L := by
  have upper (α : ℝ) (c : ℝ) {s : ℝ} (hs : 0 < s) : logLik α x (c,s) ≤ L := by
    let p : LocationScale.domain := ⟨(c,s),⟨mem_univ _,hs⟩⟩
    rw [← objective_transfer x α p]
    exact hL α (LocationScale.inverseEquiv p)
  exact max_le
    (le_of_tendsto (right_face_limit hx i hi) (Eventually.of_forall fun t ↦
      upper (t^2) (x i-t⁻¹) (endpoint_scale_max hx (x i)).1))
    (le_of_tendsto (left_face_limit hx j hj) (Eventually.of_forall fun t ↦
      upper (-(t^2)) (x j+t⁻¹) (endpoint_scale_max hx (x j)).1))

/-- An all-free fit exists exactly when a finite likelihood reaches both
optimized half-normal faces. This includes equality with an excluded face. -/
theorem all_free_criterion {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ p : ℝ × LocationScale.inverseDomain,
      IsMaxOn (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2) univ p) ↔
      ∃ (α : ℝ) (p : LocationScale.inverseDomain),
        max (face x (x i)) (face x (x j)) ≤ objective x α p := by
  constructor
  · rintro ⟨⟨α,p⟩,hm⟩
    exact ⟨α,p,faces_below_bound hx i j hi hj fun a v ↦ isMaxOn_iff.mp hm (a,v) (mem_univ _)⟩
  · rintro ⟨α,p,hp⟩
    let F := max (face x (x i)) (face x (x j))
    obtain ⟨K,hK,hconf⟩ := nuisance_confinement hx F
    obtain ⟨A,_,hA⟩ := right_tail hx i hi
    obtain ⟨B,_,hB⟩ := left_tail hx j hj
    let box := Icc (-B) A ×ˢ K
    have hcomp : IsCompact box := isCompact_Icc.prod hK
    have hcontain (a : ℝ) (v : LocationScale.inverseDomain) (hv : F ≤ objective x a v) :
        (a,v) ∈ box := by
      refine ⟨⟨?_,?_⟩,hconf a v hv⟩
      · by_contra! h
        exact (hB a h.le v v.property).not_ge ((le_max_right _ _).trans hv)
      · by_contra! h
        exact (hA a h.le v v.property).not_ge ((le_max_left _ _).trans hv)
    have hmem := hcontain α p hp
    obtain ⟨q,hq,hm⟩ := hcomp.exists_isMaxOn ⟨(α,p),hmem⟩
      (continuous_objective x).continuousOn
    refine ⟨q,?_⟩
    rintro ⟨a,v⟩ _
    by_cases hc : F ≤ objective x a v
    · exact hm (hcontain a v hc)
    · exact (le_of_not_ge hc).trans (hp.trans (hm hmem))

/-- If there is no finite fit, the larger optimized face is the supremum. -/
theorem unattained_face_supremum {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j)
    (h : ¬ ∃ p : ℝ × LocationScale.inverseDomain,
      IsMaxOn (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2) univ p) :
    IsLUB (range (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2))
      (max (face x (x i)) (face x (x j))) := by
  constructor
  · rintro _ ⟨⟨α,p⟩,rfl⟩
    apply le_of_lt
    exact lt_of_not_ge fun hv ↦ h ((all_free_criterion hx i j hi hj).mpr ⟨α,p,hv⟩)
  · intro L hL
    exact faces_below_bound hx i j hi hj fun α p ↦ hL ⟨(α,p),rfl⟩

private def skewLoss (α z : ℝ) : ℝ := gaussianLoss z - log (Φ (α*z))

private theorem skewLoss_convex (α : ℝ) : ConvexOn ℝ univ (skewLoss α) := by
  refine ⟨convex_univ,?_⟩
  intro x _ y _ a b ha hb hab
  have h := cdf_strictLogConcave.concaveOn.2 (mem_univ (α*x)) (mem_univ (α*y)) ha hb hab
  simp only [smul_eq_mul] at h ⊢
  rw [← show α*(a*x+b*y) = a*(α*x)+b*(α*y) by ring] at h
  unfold skewLoss gaussianLoss
  have he : a*x^2+b*y^2-(a*x+b*y)^2 = a*b*(x-y)^2 := by
    rw [show b = 1-a by linarith]
    ring
  nlinarith only [he,h,mul_nonneg (mul_nonneg ha hb) (sq_nonneg (x-y))]

private theorem objective_as_loss {n : ℕ} (x : Fin n → ℝ) (α : ℝ) (p : ℝ × ℝ) :
    objective x α p = LocationScale.inverseLogLik (skewLoss α) x p + constant n := by
  unfold objective envelope LocationScale.inverseLogLik skewLoss
  have he (j : Fin n) : log p.1 - (gaussianLoss (p.1*x j-p.2) -
      log (Φ (α*(p.1*x j-p.2)))) =
      (log p.1 - gaussianLoss (p.1*x j-p.2)) + log (Φ (α*(p.1*x j-p.2))) := by ring
  simp only [he, Finset.sum_add_distrib]
  ring

private theorem objective_smooth {n : ℕ} (x : Fin n → ℝ) {α : ℝ} {p : ℝ × ℝ}
    (hp : p ∈ LocationScale.inverseDomain) :
    ContDiffAt ℝ 1 (fun ap : ℝ × (ℝ × ℝ) ↦ objective x ap.1 ap.2) (α,p) := by
  unfold objective envelope LocationScale.inverseLogLik gaussianLoss
  have hb : p.1 ≠ 0 := (show 0 < p.1 from hp.1).ne'
  have hcdf : ContDiff ℝ 1 Φ := by
    rw [contDiff_one_iff_deriv, show deriv Φ = φ from funext fun u ↦ (cdf_hasDerivAt u).deriv]
    exact ⟨fun u ↦ (cdf_hasDerivAt u).differentiableAt,
      continuous_iff_continuousAt.mpr fun u ↦ (phi_hasDerivAt u).continuousAt⟩
  apply ContDiffAt.add
  · apply ContDiffAt.add _ contDiffAt_const
    apply ContDiffAt.sum
    intro j _
    fun_prop
  · apply ContDiffAt.sum
    intro j _
    have hz : ContDiffAt ℝ 1 (fun ap : ℝ × (ℝ × ℝ) ↦ ap.1*(ap.2.1*x j-ap.2.2)) (α,p) := by
      fun_prop
    exact (hcdf.contDiffAt.comp (α,p) hz).log (cdf_pos _).ne'

/-- The maximized log likelihood at a finite slant. -/
def profile {n : ℕ} (x : Fin n → ℝ) (α : ℝ) : ℝ :=
  sSup (objective x α '' LocationScale.inverseDomain)

private theorem profile_max {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) (α : ℝ) :
    ∃ p ∈ LocationScale.inverseDomain, IsMaxOn (objective x α) LocationScale.inverseDomain p ∧
      profile x α = objective x α p ∧
      ∀ q ∈ LocationScale.inverseDomain, q ≠ p → objective x α q < objective x α p := by
  obtain ⟨p,⟨hp,hm⟩,hu⟩ := held_slant hx α
  let v := LocationScale.inverseEquiv ⟨p,hp⟩
  have htrans (q : LocationScale.inverseDomain) :
      objective x α q = logLik α x (LocationScale.inverseEquiv.symm q) := by
    rw [← objective_transfer x α (LocationScale.inverseEquiv.symm q), Equiv.apply_symm_apply]
  have hmax : IsMaxOn (objective x α) LocationScale.inverseDomain v := by
    intro q hq
    change objective x α (⟨q,hq⟩ : LocationScale.inverseDomain) ≤ objective x α v
    rw [htrans,objective_transfer]
    exact hm (LocationScale.inverseEquiv.symm ⟨q,hq⟩).property
  refine ⟨v,v.property,hmax,(hmax.isLUB v.property).csSup_eq (Nonempty.image _ ⟨v,v.property⟩),?_⟩
  intro q hq hne
  have hq' := (LocationScale.inverseEquiv.symm ⟨q,hq⟩).property
  change objective x α (⟨q,hq⟩ : LocationScale.inverseDomain) < objective x α v
  rw [htrans,objective_transfer]
  apply lt_of_le_of_ne (hm hq')
  intro he
  have hh := hu _ ⟨hq',fun y hy ↦ (hm hy).trans_eq he.symm⟩
  exact hne (congrArg Subtype.val (LocationScale.inverseEquiv.symm_apply_eq.mp
    (Subtype.ext hh : LocationScale.inverseEquiv.symm ⟨q,hq⟩ = ⟨p,hp⟩)))

private theorem profile_differentiable {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (α : ℝ) : DifferentiableAt ℝ (profile x) α := by
  choose q hq hmax hval hstrict using profile_max hx
  have hlim : Tendsto q (𝓝 α) (𝓝 (q α)) :=
    FinishProfile.tendsto_optimizer (isOpen_Ioi.prod isOpen_univ) (hq α)
      (fun y hy ↦ by rw [← nhds_prod_eq]; exact (objective_smooth x hy).continuousAt)
      (hstrict α)
      (Eventually.of_forall fun a ↦ by
        have he : objective x a =
            fun p ↦ LocationScale.inverseLogLik (skewLoss a) x p + constant n :=
          funext (objective_as_loss x a)
        rw [he]
        exact (LocationScale.concave_inverseLogLik (skewLoss_convex a) x).add_const _)
      (Eventually.of_forall fun a ↦ ⟨hq a,hmax a (hq α)⟩)
  exact (FinishProfile.hasDerivAt_profile
    ((objective_smooth x (hq α)).hasStrictFDerivAt_fderiv one_ne_zero)
    hlim (hval α) (Eventually.of_forall fun a ↦
      ⟨(hmax a (hq α)).trans_eq (hval a).symm,hval a,
        (hmax α (hq a)).trans_eq (hval α).symm⟩)).differentiableAt

/-- A finite stationary profile value reaching both half-normal faces is
equivalent to an all-free fit. The comparison includes finite equality. -/
theorem stationary_criterion {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ p : ℝ × LocationScale.inverseDomain,
      IsMaxOn (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2) univ p) ↔
      ∃ α : ℝ, HasDerivAt (profile x) 0 α ∧ max (face x (x i)) (face x (x j)) ≤ profile x α := by
  constructor
  · rintro ⟨⟨α,p⟩, hm⟩
    have hmax (a : ℝ) (v : LocationScale.inverseDomain) :
        objective x a v ≤ objective x α p := isMaxOn_iff.mp hm (a,v) (mem_univ _)
    have hval : profile x α = objective x α p := by
      have hp : IsMaxOn (objective x α) LocationScale.inverseDomain p :=
        fun v hv ↦ hmax α ⟨v,hv⟩
      exact (hp.isLUB p.property).csSup_eq (Nonempty.image _ ⟨p,p.property⟩)
    have hm' : IsMaxOn (profile x) univ α := by
      intro a _
      obtain ⟨v,hv,_,he,_⟩ := profile_max hx a
      change profile x a ≤ profile x α
      rw [hval,he]
      exact hmax a ⟨v,hv⟩
    exact ⟨α, (hm'.isLocalMax univ_mem).deriv_eq_zero ▸ (profile_differentiable hx α).hasDerivAt,
      (faces_below_bound hx i j hi hj hmax).trans_eq hval.symm⟩
  · rintro ⟨α,_,hα⟩
    obtain ⟨p,hp,_,he,_⟩ := profile_max hx α
    exact (all_free_criterion hx i j hi hj).mpr ⟨α,⟨p,hp⟩,hα.trans_eq he⟩


private theorem profile_ge_physical {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (α c : ℝ) {s : ℝ} (hs : 0 < s) : logLik α x (c,s) ≤ profile x α := by
  let p : LocationScale.domain := ⟨(c,s),⟨mem_univ _,hs⟩⟩
  obtain ⟨v,_,hm,he,_⟩ := profile_max hx α
  rw [he, ← objective_transfer x α p]
  exact hm (LocationScale.inverseEquiv p).property

/-- In the two slant directions, the maximized likelihood tends to its
corresponding optimized half-normal face. -/
theorem profile_face_limits {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    Tendsto (profile x) atTop (𝓝 (face x (x i))) ∧
      Tendsto (profile x) atBot (𝓝 (face x (x j))) := by
  constructor
  · obtain ⟨A,_,hA⟩ := right_tail hx i hi
    have ht := (right_face_limit hx i hi).comp tendsto_sqrt_atTop
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' ht tendsto_const_nhds ?_ ?_
    · filter_upwards [eventually_ge_atTop (0 : ℝ)] with α hα
      simp only [Function.comp_apply,sq_sqrt hα]
      exact profile_ge_physical hx α _ (endpoint_scale_max hx (x i)).1
    · filter_upwards [eventually_ge_atTop A] with α hα
      obtain ⟨p,hp,_,he,_⟩ := profile_max hx α
      rw [he]
      exact (hA α hα p hp).le
  · obtain ⟨A,_,hA⟩ := left_tail hx j hj
    have hs := tendsto_sqrt_atTop.comp tendsto_neg_atBot_atTop
    have ht := (left_face_limit hx j hj).comp hs
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' ht tendsto_const_nhds ?_ ?_
    · filter_upwards [eventually_le_atBot (0 : ℝ)] with α hα
      simp only [Function.comp_apply,sq_sqrt (neg_nonneg.mpr hα),neg_neg]
      exact profile_ge_physical hx α _ (endpoint_scale_max hx (x j)).1
    · filter_upwards [eventually_le_atBot (-A)] with α hα
      obtain ⟨p,hp,_,he,_⟩ := profile_max hx α
      rw [he]
      exact (hA α hα p hp).le


/-- A nonempty constant sample has unbounded all-free log likelihood. -/
theorem constant_unbounded {n : ℕ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (range (fun ap : ℝ × LocationScale.inverseDomain ↦
      objective (fun _ : Fin n ↦ m) ap.1 ap.2)) := by
  rintro ⟨B,hB⟩
  apply constant_collapse 0 hn m
  refine ⟨B,?_⟩
  rintro _ ⟨p,hp,rfl⟩
  rw [← objective_transfer (fun _ : Fin n ↦ m) 0 (⟨p,hp⟩ : LocationScale.domain)]
  exact hB ⟨(0,LocationScale.inverseEquiv ⟨p,hp⟩),rfl⟩

end

end UnivariateML.FinishSN
