module

public import UnivariateML.Family.Discrete
public import Mathlib.Analysis.SpecialFunctions.Gaussian.GaussianIntegral
public import Mathlib.MeasureTheory.Integral.IntegralEqImproper
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.MeasureTheory.Integral.IntervalIntegral.FundThmCalculus
public import Mathlib.Analysis.SpecialFunctions.ExpDeriv
import Mathlib.Tactic

set_option linter.style.header false

/-! Discretized-normal likelihood with cells `[k,k+1]`.
The endpoint information identity gives strict concavity in the inverse-scale
chart. Gaussian tails yield compact positive likelihood levels at range at
least two. Constant and adjacent samples have exact zero-scale suprema. -/

@[expose] public section

namespace UnivariateML.Discrete3DNorm

open Set Real MeasureTheory Filter
open scoped Topology BigOperators

noncomputable section

def kernel (z : ℝ) : ℝ := exp (-(z^2) / 2)

private def primitive (b : ℝ) : ℝ := ∫ z in 0..b, kernel z

/-- Cell mass, up to the common positive Gaussian normalization. -/
@[no_expose] def mass (k B μ : ℝ) : ℝ :=
  primitive ((k+1-μ)*B) - primitive ((k-μ)*B)

private theorem primitive_deriv (b : ℝ) : HasDerivAt primitive (kernel b) b := by
  have hc : Continuous kernel := by unfold kernel; fun_prop
  exact intervalIntegral.integral_hasDerivAt_right (hc.intervalIntegrable _ _)
    hc.stronglyMeasurable.stronglyMeasurableAtFilter hc.continuousAt

private theorem mass_deriv (k B μ : ℝ) :
    HasDerivAt (mass k B) (B * (kernel ((k-μ)*B) - kernel ((k+1-μ)*B))) μ := by
  have h (c : ℝ) : HasDerivAt (fun u ↦ (c-u)*B) (-B) μ := by
    simpa using (((hasDerivAt_const μ c).sub (hasDerivAt_id μ)).mul_const B)
  convert ((primitive_deriv _).comp μ (h (k+1))).sub
    ((primitive_deriv _).comp μ (h k)) using 1
  · rfl
  · ring

/-- A constant discretized-normal sample with positive held inverse scale
has its unique location optimum at the cell midpoint. -/
theorem midpoint_strict {k B μ : ℝ} (hB : 0 < B) (hμ : μ ≠ k+1/2) :
    mass k B μ < mass k B (k+1/2) := by
  have hc : Continuous (mass k B) := continuous_iff_continuousAt.mpr
    (fun u ↦ (mass_deriv k B u).continuousAt)
  have hsign (u : ℝ) : ((k+1-u)*B)^2 - ((k-u)*B)^2 = (2*(k-u)+1)*B^2 := by ring
  rcases lt_or_gt_of_ne hμ with hl | hr
  · apply (strictMonoOn_of_deriv_pos (convex_Iic (k+1/2)) hc.continuousOn ?_)
      (show μ ∈ Iic (k+1/2) from hl.le) (show k+1/2 ∈ Iic (k+1/2) by simp) hl
    intro u hu
    rw [interior_Iic] at hu
    rw [(mass_deriv k B u).deriv]
    apply mul_pos hB (sub_pos.mpr _)
    unfold kernel
    apply exp_lt_exp.mpr
    have hh := mul_pos (show 0 < 2*(k-u)+1 by have := mem_Iio.mp hu; linarith)
      (sq_pos_of_pos hB)
    linarith [hsign u]
  · apply (strictAntiOn_of_deriv_neg (convex_Ici (k+1/2)) hc.continuousOn ?_)
      (show k+1/2 ∈ Ici (k+1/2) by simp) (show μ ∈ Ici (k+1/2) from hr.le) hr
    intro u hu
    rw [interior_Ici] at hu
    rw [(mass_deriv k B u).deriv]
    apply mul_neg_of_pos_of_neg hB (sub_neg.mpr _)
    unfold kernel
    apply exp_lt_exp.mpr
    have hh := mul_neg_of_neg_of_pos
      (show 2*(k-u)+1 < 0 by have := mem_Ioi.mp hu; linarith) (sq_pos_of_pos hB)
    linarith [hsign u]

/-- If the held mean belongs to the observed cell closure, that cell mass
increases strictly with inverse scale. -/
theorem cell_mass_strictMono_of_mem {k μ : ℝ} (hμ : μ ∈ Icc k (k+1)) :
    StrictMonoOn (fun B ↦ mass k B μ) (Ioi 0) := by
  have hd (B : ℝ) :
      HasDerivAt (fun b ↦ mass k b μ)
        ((k+1-μ) * kernel ((k+1-μ)*B) - (k-μ) * kernel ((k-μ)*B)) B := by
    convert ((primitive_deriv _).comp B ((hasDerivAt_id B).const_mul (k+1-μ))).sub
      ((primitive_deriv _).comp B ((hasDerivAt_id B).const_mul (k-μ))) using 1
    · rfl
    · simp only [mul_one, id_eq]
      ring
  apply strictMonoOn_of_deriv_pos (convex_Ioi _)
    (fun B _ ↦ (hd B).continuousAt.continuousWithinAt)
  intro B _
  rw [(hd B).deriv]
  have hp (z : ℝ) : 0 < kernel z := exp_pos _
  rcases lt_or_eq_of_le hμ.1 with hl | he
  · exact sub_pos.mpr ((mul_neg_of_neg_of_pos (sub_neg.mpr hl) (hp _)).trans_le
      (mul_nonneg (sub_nonneg.mpr hμ.2) (hp _).le))
  · rw [← he]
    ring_nf
    exact hp _

/-- Gaussian mass between standardized endpoints, before normalization. -/
@[no_expose] def intervalMass (a b : ℝ) : ℝ := primitive b - primitive a

private theorem kernel_deriv (z : ℝ) : HasDerivAt kernel (-z*kernel z) z := by
  have h : HasDerivAt (fun t : ℝ ↦ -(t^2)/2) (-z) z := by
    convert (((hasDerivAt_id z).pow 2).neg).div_const (2 : ℝ) using 1
    · rfl
    · norm_num
      ring
  change HasDerivAt (fun t : ℝ ↦ exp (-(t^2)/2)) _ z
  simpa only [kernel, mul_comm] using h.exp

@[fun_prop] private theorem kernel_continuous : Continuous kernel :=
  continuous_iff_continuousAt.mpr fun z ↦ (kernel_deriv z).continuousAt

private theorem primitive_continuous : Continuous primitive :=
  continuous_iff_continuousAt.mpr fun z ↦ (primitive_deriv z).continuousAt

private theorem intervalMass_eq_integral (a b : ℝ) :
    intervalMass a b = ∫ z in a..b, kernel z := by
  rw [intervalMass, primitive, primitive]
  exact (intervalIntegral.integral_eq_sub_of_hasDerivAt
    (fun z _ ↦ primitive_deriv z) (kernel_continuous.intervalIntegrable _ _)).symm

private theorem intervalMass_pos {a b : ℝ} (hab : a < b) : 0 < intervalMass a b := by
  rw [intervalMass_eq_integral]
  exact intervalIntegral.integral_pos hab kernel_continuous.continuousOn
    (fun z _ ↦ (exp_pos _).le) ⟨a, ⟨le_rfl, hab.le⟩, exp_pos _⟩

/-- The log mass derivative for a unit change in the line parameter. -/
def intervalScore (a b va vb : ℝ) : ℝ :=
  (kernel b*vb-kernel a*va)/intervalMass a b

/-- The auxiliary field for the grouped-normal integration by parts. -/
@[no_expose] def directionField (a b va vb z : ℝ) : ℝ :=
  (kernel a*va + intervalScore a b va vb*(primitive z-primitive a))/kernel z

private theorem directionField_left (a b va vb : ℝ) :
    directionField a b va vb a = va := by
  simp [directionField, mul_div_cancel_left₀, show kernel a ≠ 0 from (exp_pos _).ne']

private theorem directionField_right {a b va vb : ℝ} (hab : a < b) :
    directionField a b va vb b = vb := by
  unfold directionField intervalScore
  change (kernel a*va + ((kernel b*vb-kernel a*va)/intervalMass a b)*intervalMass a b)/
    kernel b = vb
  rw [div_mul_cancel₀ _ (intervalMass_pos hab).ne']
  apply (div_eq_iff (show kernel b ≠ 0 from (exp_pos _).ne')).mpr
  ring

private theorem directionField_deriv (a b va vb z : ℝ) :
    HasDerivAt (directionField a b va vb)
      (intervalScore a b va vb + z*directionField a b va vb z) z := by
  let s := intervalScore a b va vb
  have hn := (((primitive_deriv z).sub_const (primitive a)).const_mul s).const_add
    (kernel a*va)
  convert hn.div (kernel_deriv z) (show kernel z ≠ 0 from (exp_pos _).ne') using 1
  · rfl
  · change _ = (s*kernel z*kernel z -
        (kernel a*va+s*(primitive z-primitive a))*(-z*kernel z))/(kernel z)^2
    unfold directionField
    change s+z*((kernel a*va+s*(primitive z-primitive a))/kernel z) = _
    field_simp [show kernel z ≠ 0 from (exp_pos _).ne']
    ring

/-- Integration by parts for one Gaussian cell with affine endpoint velocities.
The directional observed information times the cell mass equals a
weighted integral of two squares. -/
theorem endpoint_information_identity {a b va vb : ℝ} (hab : a < b) :
    let p := intervalMass a b
    let s := intervalScore a b va vb
    let w := directionField a b va vb
    s^2*p + (b*kernel b*vb^2-a*kernel a*va^2) =
      ∫ z in a..b, ((s+z*w z)^2+(w z)^2)*kernel z := by
  let s := intervalScore a b va vb
  let w := directionField a b va vb
  have hw : Continuous w :=
    continuous_iff_continuousAt.mpr fun z ↦ (directionField_deriv a b va vb z).continuousAt
  let g := fun z ↦ ((s+z*w z)^2+(w z)^2)*kernel z
  have hg : Continuous g := by
    dsimp [g]
    fun_prop (disch := assumption)
  let H := fun z ↦ z*kernel z*(w z)^2
  have hH (z : ℝ) : HasDerivAt H (g z-s^2*kernel z) z := by
    have h := ((hasDerivAt_id z).mul (kernel_deriv z)).mul
      ((directionField_deriv a b va vb z).pow 2)
    convert h using 1
    · rfl
    · dsimp [g]
      change ((s+z*w z)^2+(w z)^2)*kernel z-s^2*kernel z =
        (1*kernel z+z*(-z*kernel z))*(w z)^2+
          z*kernel z*((2 : ℝ)*(w z)^(2-1)*(s+z*w z))
      ring
  have he := intervalIntegral.integral_eq_sub_of_hasDerivAt (fun z _ ↦ hH z)
    ((hg.sub (kernel_continuous.const_mul (s^2))).intervalIntegrable a b)
  rw [intervalIntegral.integral_sub (hg.intervalIntegrable _ _)
    ((kernel_continuous.const_mul (s^2)).intervalIntegrable _ _),
    intervalIntegral.integral_const_mul, ← intervalMass_eq_integral] at he
  change s^2*intervalMass a b + (b*kernel b*vb^2-a*kernel a*va^2) =
    ∫ z in a..b, g z
  change _ = b*kernel b*(w b)^2-a*kernel a*(w a)^2 at he
  have ha : w a = va := directionField_left a b va vb
  have hb : w b = vb := directionField_right hab
  rw [ha, hb] at he
  linarith only [he]

/-- The observed information on a Gaussian cell is positive in every
nonzero endpoint direction. -/
theorem endpoint_information_pos {a b va vb : ℝ} (hab : a < b)
    (hne : va ≠ 0 ∨ vb ≠ 0) :
    (intervalScore a b va vb)^2 +
      (b*kernel b*vb^2-a*kernel a*va^2)/intervalMass a b > 0 := by
  let s := intervalScore a b va vb
  let w := directionField a b va vb
  let g := fun z ↦ ((s+z*w z)^2+(w z)^2)*kernel z
  have hw : Continuous w :=
    continuous_iff_continuousAt.mpr fun z ↦ (directionField_deriv a b va vb z).continuousAt
  have hg : Continuous g := by
    dsimp [g]
    fun_prop (disch := assumption)
  have hp (z : ℝ) : 0 < kernel z := exp_pos _
  have hG : 0 < ∫ z in a..b, g z := by
    apply intervalIntegral.integral_pos hab hg.continuousOn
      (fun z _ ↦ mul_nonneg (add_nonneg (sq_nonneg _) (sq_nonneg _)) (hp z).le)
    rcases hne with ha | hb
    · have h : w a = va := directionField_left a b va vb
      exact ⟨a, ⟨le_rfl, hab.le⟩, mul_pos
        (add_pos_of_nonneg_of_pos (sq_nonneg _) (sq_pos_of_ne_zero (h ▸ ha))) (hp a)⟩
    · have h : w b = vb := directionField_right hab
      exact ⟨b, ⟨hab.le, le_rfl⟩, mul_pos
        (add_pos_of_nonneg_of_pos (sq_nonneg _) (sq_pos_of_ne_zero (h ▸ hb))) (hp b)⟩
  have hi := endpoint_information_identity (va := va) (vb := vb) hab
  dsimp only at hi
  change 0 < s^2+(b*kernel b*vb^2-a*kernel a*va^2)/intervalMass a b
  have hp' := intervalMass_pos hab
  apply pos_of_mul_pos_right _ hp'.le
  rw [mul_comm (intervalMass a b), add_mul, div_mul_cancel₀ _ hp'.ne']
  exact hi.symm ▸ hG

private def lineMass (a b va vb t : ℝ) : ℝ :=
  intervalMass (a+va*t) (b+vb*t)

private theorem affine_deriv (a v t : ℝ) :
    HasDerivAt (fun u ↦ a+v*u) v t := by
  simpa using ((hasDerivAt_id t).const_mul v).const_add a

private theorem lineMass_deriv (a b va vb t : ℝ) :
    HasDerivAt (lineMass a b va vb) (kernel (b+vb*t)*vb-kernel (a+va*t)*va) t := by
  change HasDerivAt (fun u ↦ primitive (b+vb*u)-primitive (a+va*u)) _ t
  exact ((primitive_deriv (b+vb*t)).comp t (affine_deriv b vb t)).sub
    ((primitive_deriv (a+va*t)).comp t (affine_deriv a va t))

private theorem lineLog_second (a b va vb t : ℝ) (hab : a+va*t < b+vb*t) :
    deriv (deriv (fun u ↦ log (lineMass a b va vb u))) t =
      -((intervalScore (a+va*t) (b+vb*t) va vb)^2 +
        ((b+vb*t)*kernel (b+vb*t)*vb^2-(a+va*t)*kernel (a+va*t)*va^2) /
          lineMass a b va vb t) := by
  let N := fun u ↦ kernel (b+vb*u)*vb-kernel (a+va*u)*va
  let P := lineMass a b va vb
  have hp : 0 < P t := intervalMass_pos hab
  have hdN := ((((kernel_deriv (b+vb*t)).comp t (affine_deriv b vb t)).mul_const vb).sub
    (((kernel_deriv (a+va*t)).comp t (affine_deriv a va t)).mul_const va))
  have hdP : HasDerivAt P (N t) t := lineMass_deriv a b va vb t
  have hd := hdN.div hdP hp.ne'
  have he : deriv (fun u ↦ log (P u)) =ᶠ[𝓝 t] fun u ↦ N u / P u := by
    filter_upwards [hdP.continuousAt.eventually (Ioi_mem_nhds hp)] with u hu
    exact ((lineMass_deriv a b va vb u).log ((show 0 < P u from hu).ne')).deriv
  change deriv (deriv (fun u ↦ log (P u))) t = _
  rw [he.deriv_eq]
  have hd' : HasDerivAt (fun u ↦ N u/P u)
      (((-(b+vb*t)*kernel (b+vb*t)*vb*vb -
        (-(a+va*t)*kernel (a+va*t)*va*va))*P t - N t*N t)/(P t)^2) t := hd
  rw [hd'.deriv]
  change (((-(b+vb*t)*kernel (b+vb*t)*vb*vb -
        (-(a+va*t)*kernel (a+va*t)*va*va))*P t - N t*N t)/(P t)^2) =
    -((N t/P t)^2+((b+vb*t)*kernel (b+vb*t)*vb^2 -
        (a+va*t)*kernel (a+va*t)*va^2)/P t)
  field_simp [hp.ne']
  ring

/-- Free grouped-normal coordinates `(A,B)=(μ/σ,1/σ)`. -/
def workingDomain : Set (ℝ × ℝ) := univ ×ˢ Ioi 0

/-- A cell log likelihood in the working coordinates, up to normalization. -/
def workingCellLog (k : ℝ) (p : ℝ × ℝ) : ℝ :=
  log (intervalMass (k*p.2-p.1) ((k+1)*p.2-p.1))

/-- Each cell log mass is strictly concave in the working coordinates. -/
theorem workingCellLog_strictConcave (k : ℝ) :
    StrictConcaveOn ℝ workingDomain (workingCellLog k) := by
  refine ⟨convex_univ.prod (convex_Ioi _), ?_⟩
  intro p hp q hq hpq α β hα hβ hsum
  let a := k*p.2-p.1
  let b := (k+1)*p.2-p.1
  let va := (k*q.2-q.1)-a
  let vb := ((k+1)*q.2-q.1)-b
  have hpB : 0 < p.2 := hp.2
  have hqB : 0 < q.2 := hq.2
  have hend (t : ℝ) (ht : t ∈ Icc (0 : ℝ) 1) : a+va*t < b+vb*t := by
    have hdiff : (b+vb*t)-(a+va*t) = (1-t)*p.2+t*q.2 := by
      dsimp [a, b, va, vb]
      ring
    have hpos : 0 < (1-t)*p.2+t*q.2 := by
      rcases eq_or_lt_of_le ht.1 with he | he
      · rw [← he]
        simpa using hpB
      · exact add_pos_of_nonneg_of_pos (mul_nonneg (sub_nonneg.mpr ht.2) hpB.le)
          (mul_pos he hqB)
    linarith only [hdiff, hpos]
  have hne : va ≠ 0 ∨ vb ≠ 0 := by
    by_contra! h
    have heB : p.2 = q.2 := by
      have ha := h.1
      have hb := h.2
      dsimp [va, vb, a, b] at ha hb
      linarith only [ha, hb]
    have heA : p.1 = q.1 := by
      have ha := h.1
      dsimp [va, a] at ha
      rw [heB] at ha
      linarith only [ha]
    exact hpq (Prod.ext heA heB)
  let f := fun t ↦ log (lineMass a b va vb t)
  have hc : StrictConcaveOn ℝ (Icc 0 1) f := by
    apply strictConcaveOn_of_deriv2_neg' (convex_Icc _ _)
    · exact fun t ht ↦ ((lineMass_deriv a b va vb t).log
        (intervalMass_pos (hend t ht)).ne').continuousAt.continuousWithinAt
    · intro t ht
      change deriv (deriv f) t < 0
      dsimp only [f]
      rw [lineLog_second a b va vb t (hend t ht)]
      exact neg_neg_of_pos (endpoint_information_pos (hend t ht) hne)
  have h := hc.2 (show (0 : ℝ) ∈ Icc 0 1 by simp)
    (show (1 : ℝ) ∈ Icc 0 1 by simp) zero_ne_one hα hβ hsum
  simp only [smul_eq_mul, mul_zero, mul_one, zero_add] at h
  have h0 : f 0 = workingCellLog k p := by
    simp [f, lineMass, a, b, workingCellLog]
  have h1 : f 1 = workingCellLog k q := by
    simp [f, lineMass, a, b, va, vb, workingCellLog]
  have hβ' : 1-β = α := by linarith
  have he : f β = workingCellLog k (α • p + β • q) := by
    unfold f lineMass workingCellLog
    congr 2
    · change a+va*β = k*(α*p.2+β*q.2)-(α*p.1+β*q.1)
      rw [← hβ']
      dsimp [va, a]
      ring
    · change b+vb*β = (k+1)*(α*p.2+β*q.2)-(α*p.1+β*q.1)
      rw [← hβ']
      dsimp [vb, b]
      ring
  rwa [h0, h1, he] at h

/-- Sample grouped-normal log likelihood in the working coordinates. -/
def workingLogLik {n : ℕ} (x : Fin n → ℤ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, workingCellLog (x i) p

/-- Summing the cell log likelihoods preserves strict concavity on nonempty data. -/
theorem workingLogLik_strictConcave {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    StrictConcaveOn ℝ workingDomain (workingLogLik x) := by
  refine ⟨convex_univ.prod (convex_Ioi _), ?_⟩
  intro p hp q hq hne α β hα hβ hsum
  simp only [workingLogLik, smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  apply Finset.sum_lt_sum_of_nonempty ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  exact fun i _ ↦ (workingCellLog_strictConcave (x i)).2 hp hq hne hα hβ hsum

private theorem kernel_integrable : Integrable kernel := by
  convert integrable_exp_neg_mul_sq (b := 1/2) (by norm_num) using 1
  funext z
  unfold kernel
  congr 1
  ring

private def primitiveUpper : ℝ := ∫ z in Ioi 0, kernel z
private def primitiveLower : ℝ := -(∫ z in Iic 0, kernel z)

/-- The positive normalization for the Gaussian kernel. -/
def gaussianTotal : ℝ := ∫ z : ℝ, kernel z

private theorem primitiveUpper_sub_lower :
    primitiveUpper - primitiveLower = gaussianTotal := by
  have h := integral_add_compl (f := kernel) (measurableSet_Iic (a := (0 : ℝ))) kernel_integrable
  rw [compl_Iic] at h
  unfold primitiveUpper primitiveLower gaussianTotal
  linarith only [h]

private theorem primitive_atTop : Tendsto primitive atTop (𝓝 primitiveUpper) :=
  intervalIntegral_tendsto_integral_Ioi 0 kernel_integrable.integrableOn tendsto_id

private theorem primitive_atBot : Tendsto primitive atBot (𝓝 primitiveLower) := by
  have h := (intervalIntegral_tendsto_integral_Iic (f := kernel) 0
    kernel_integrable.integrableOn (tendsto_id (x := atBot))).neg
  exact h.congr fun b ↦ (intervalIntegral.integral_symm (f := kernel) b 0).symm

private theorem primitive_strictMono : StrictMono primitive :=
  strictMono_of_deriv_pos (fun z ↦ by rw [(primitive_deriv z).deriv]; exact exp_pos _)

private theorem primitive_bounds (a : ℝ) :
    primitiveLower < primitive a ∧ primitive a < primitiveUpper := by
  have htop (x : ℝ) : primitive x ≤ primitiveUpper :=
    ge_of_tendsto primitive_atTop ((eventually_ge_atTop x).mono fun _ h ↦
      primitive_strictMono.monotone h)
  have hbot (x : ℝ) : primitiveLower ≤ primitive x :=
    le_of_tendsto primitive_atBot ((eventually_le_atBot x).mono fun _ h ↦
      primitive_strictMono.monotone h)
  exact ⟨(hbot (a-1)).trans_lt (primitive_strictMono (by linarith)),
    (primitive_strictMono (show a < a+1 by linarith)).trans_le (htop (a+1))⟩

private theorem gaussianTotal_pos : 0 < gaussianTotal := by
  rw [← primitiveUpper_sub_lower]
  exact sub_pos.mpr ((primitive_bounds 0).1.trans (primitive_bounds 0).2)

/-- Normalized probability of a standardized Gaussian interval. -/
def intervalProb (a b : ℝ) : ℝ := intervalMass a b / gaussianTotal

private theorem intervalProb_pos {a b : ℝ} (hab : a < b) : 0 < intervalProb a b :=
  div_pos (intervalMass_pos hab) gaussianTotal_pos

private theorem intervalProb_lt_one (a b : ℝ) : intervalProb a b < 1 := by
  apply (div_lt_one gaussianTotal_pos).mpr
  rw [← primitiveUpper_sub_lower]
  unfold intervalMass
  linarith only [(primitive_bounds a).1, (primitive_bounds b).2]

private theorem intervalProb_le_width {a b : ℝ} (hab : a ≤ b) :
    intervalProb a b ≤ (b-a)/gaussianTotal := by
  apply div_le_div_of_nonneg_right _ gaussianTotal_pos.le
  rw [intervalMass_eq_integral]
  calc
    (∫ z in a..b, kernel z) ≤ ∫ _ in a..b, (1 : ℝ) :=
      intervalIntegral.integral_mono hab (kernel_continuous.intervalIntegrable _ _)
        (continuous_const.intervalIntegrable _ _) (fun z ↦ exp_le_one_iff.mpr (div_nonpos_of_nonpos_of_nonneg
          (neg_nonpos.mpr (sq_nonneg z)) (by norm_num)))
    _ = b-a := by simp

private theorem intervalProb_add (a b c : ℝ) :
    intervalProb a b + intervalProb b c = intervalProb a c := by
  simp only [intervalProb, intervalMass]
  ring

private theorem intervalProb_continuous : Continuous (fun p : ℝ × ℝ ↦ intervalProb p.1 p.2) :=
  ((primitive_continuous.comp continuous_snd).sub
    (primitive_continuous.comp continuous_fst)).div_const gaussianTotal

private theorem intervalProb_uniform_tails {ε : ℝ} (hε : 0 < ε) :
    ∃ R > 0, ∀ a b : ℝ, (R < a ∨ b < -R) → intervalProb a b < ε := by
  have hE := mul_pos hε gaussianTotal_pos
  have hR₁ : ∀ᶠ a in atTop, primitiveUpper-ε*gaussianTotal < primitive a :=
    primitive_atTop.eventually (Ioi_mem_nhds (by linarith only [hE]))
  have hR₂ : ∀ᶠ b in atBot, primitive b < primitiveLower+ε*gaussianTotal :=
    primitive_atBot.eventually (Iio_mem_nhds (by linarith only [hE]))
  obtain ⟨R₁, h₁⟩ := eventually_atTop.mp hR₁
  obtain ⟨R₂, h₂⟩ := eventually_atBot.mp hR₂
  let R := max 1 (max R₁ (-R₂))
  have hR : 0 < R := zero_lt_one.trans_le (le_max_left _ _)
  have haR : R₁ ≤ R := (le_max_left _ _).trans (le_max_right _ _)
  have hbR : -R₂ ≤ R := (le_max_right _ _).trans (le_max_right _ _)
  refine ⟨R, hR, fun a b h ↦ ?_⟩
  apply (div_lt_iff₀ gaussianTotal_pos).mpr
  unfold intervalMass
  rcases h with ha | hb
  · linarith only [h₁ a (haR.trans ha.le), (primitive_bounds b).2]
  · have hR₂' : -R ≤ R₂ := by linarith only [hbR]
    linarith only [h₂ b (hb.le.trans hR₂'), (primitive_bounds a).1]

/-- One grouped-normal cell probability in the working coordinates. -/
def workingCellProb (k : ℝ) (p : ℝ × ℝ) : ℝ :=
  intervalProb (k*p.2-p.1) ((k+1)*p.2-p.1)

/-- Joint grouped-normal likelihood, on the probability scale. -/
def workingLikelihood {n : ℕ} (x : Fin n → ℤ) (p : ℝ × ℝ) : ℝ :=
  ∏ i, workingCellProb (x i) p

private theorem workingCellProb_pos (k : ℝ) {p : ℝ × ℝ} (hp : p ∈ workingDomain) :
    0 < workingCellProb k p := by
  have h : 0 < p.2 := hp.2
  exact intervalProb_pos (by linarith only [h])

private theorem workingLikelihood_pos {n : ℕ} (x : Fin n → ℤ)
    {p : ℝ × ℝ} (hp : p ∈ workingDomain) : 0 < workingLikelihood x p :=
  Finset.prod_pos (fun i _ ↦ workingCellProb_pos (x i) hp)

private theorem workingLikelihood_continuous {n : ℕ} (x : Fin n → ℤ) :
    Continuous (workingLikelihood x) := by
  have hc (k : ℝ) : Continuous (workingCellProb k) := by
    have h₁ : Continuous (fun p : ℝ × ℝ ↦ k*p.2-p.1) :=
      (continuous_snd.const_mul k).sub continuous_fst
    have h₂ : Continuous (fun p : ℝ × ℝ ↦ (k+1)*p.2-p.1) :=
      (continuous_snd.const_mul (k+1)).sub continuous_fst
    change Continuous ((fun p : ℝ × ℝ ↦ intervalProb p.1 p.2) ∘
      fun p : ℝ × ℝ ↦ (k*p.2-p.1, (k+1)*p.2-p.1))
    exact intervalProb_continuous.comp (h₁.prodMk h₂)
  change Continuous (fun p : ℝ × ℝ ↦ ∏ i : Fin n, workingCellProb (x i) p)
  exact continuous_finsetProd (f := fun i : Fin n ↦ workingCellProb (x i)) Finset.univ
    (fun i _ ↦ hc (x i))

private theorem workingLikelihood_le_cell {n : ℕ} (x : Fin n → ℤ) (i : Fin n)
    {p : ℝ × ℝ} (hp : p ∈ workingDomain) :
    workingLikelihood x p ≤ workingCellProb (x i) p := by
  unfold workingLikelihood
  simpa only [Finset.prod_singleton] using Finset.prod_le_prod_of_subset_of_le_one₀
    (f := fun j : Fin n ↦ workingCellProb (x j) p)
    (Finset.subset_univ {i}) (fun j _ ↦ (workingCellProb_pos (x j) hp).le)
    (fun j _ _ ↦ (intervalProb_lt_one ((x j : ℝ)*p.2-p.1) (((x j : ℝ)+1)*p.2-p.1)).le)

private theorem workingLogLik_eq_log {n : ℕ} (x : Fin n → ℤ)
    {p : ℝ × ℝ} (hp : p ∈ workingDomain) :
    log (workingLikelihood x p) = workingLogLik x p - n*log gaussianTotal := by
  unfold workingLikelihood workingLogLik
  rw [Real.log_prod (fun i _ ↦ (workingCellProb_pos (x i) hp).ne')]
  have hm (i : Fin n) : log (workingCellProb (x i) p) =
      workingCellLog (x i) p - log gaussianTotal := by
    have h : 0 < p.2 := hp.2
    exact log_div (intervalMass_pos (by linarith only [h]) :
      0 < intervalMass ((x i : ℝ)*p.2-p.1) (((x i : ℝ)+1)*p.2-p.1)).ne'
      gaussianTotal_pos.ne'
  simp only [hm, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]

private theorem workingLogLik_max_of_Lik {n : ℕ} {x : Fin n → ℤ} {p : ℝ × ℝ}
    (hp : p ∈ workingDomain) (hmax : IsMaxOn (workingLikelihood x) workingDomain p) :
    IsMaxOn (workingLogLik x) workingDomain p := by
  intro q hq
  have h := log_le_log (workingLikelihood_pos x hq) (hmax hq)
  rw [workingLogLik_eq_log x hq, workingLogLik_eq_log x hp] at h
  exact (sub_le_sub_iff_right _).mp h

-- Every positive likelihood level is contained in a finite rectangle
-- when a pair of observed cells has a positive gap between its closures.
private theorem gap_superlevel_rectangle {n : ℕ} {x : Fin n → ℤ} {i j : Fin n}
    (hgap : x i + 1 < x j) {ε : ℝ} (hε : 0 < ε) :
    ∃ M U : ℝ, ∀ p ∈ workingDomain, ε ≤ workingLikelihood x p →
        p ∈ Icc (-M) M ×ˢ Icc (ε*gaussianTotal) U := by
  obtain ⟨R, hR, ht⟩ := intervalProb_uniform_tails hε
  let g : ℝ := (x j : ℝ)-(x i : ℝ)-1
  have hg : 0 < g := by
    have h : (x i : ℝ)+1 < (x j : ℝ) := by exact_mod_cast hgap
    dsimp [g]
    linarith only [h]
  let U := 2*R/g
  let C := |(x i : ℝ)+1|+|(x j : ℝ)|
  let M := C*U+R
  refine ⟨M, U, fun p hp hL ↦ ?_⟩
  have hB : 0 < p.2 := hp.2
  have hc (k : Fin n) : ε ≤ workingCellProb (x k) p :=
    hL.trans (workingLikelihood_le_cell x k hp)
  have hj : (x j : ℝ)*p.2-p.1 ≤ R := le_of_not_gt fun h ↦
    (not_lt_of_ge (hc j)) (ht _ _ (Or.inl h))
  have hi : -R ≤ ((x i : ℝ)+1)*p.2-p.1 := le_of_not_gt fun h ↦
    (not_lt_of_ge (hc i)) (ht _ _ (Or.inr h))
  have hhigh : p.2 ≤ U := (le_div_iff₀ hg).mpr (by dsimp [g]; nlinarith only [hj, hi])
  have hlow : ε*gaussianTotal ≤ p.2 := by
    have h := (hc i).trans (intervalProb_le_width
      (a := (x i : ℝ)*p.2-p.1) (b := ((x i : ℝ)+1)*p.2-p.1)
      (by linarith only [hB]))
    have h' := (le_div_iff₀ gaussianTotal_pos).mp h
    linarith only [h']
  have hxj : -C ≤ (x j : ℝ) := by
    dsimp [C]
    linarith only [abs_nonneg ((x i : ℝ)+1), neg_abs_le (x j : ℝ)]
  have hxi : (x i : ℝ)+1 ≤ C := by
    dsimp [C]
    linarith only [abs_nonneg (x j : ℝ), le_abs_self ((x i : ℝ)+1)]
  have hC : 0 ≤ C := by dsimp [C]; positivity
  have ha := mul_le_mul_of_nonneg_right hxj hB.le
  have hb := mul_le_mul_of_nonneg_right hxi hB.le
  have hc' := mul_le_mul_of_nonneg_left hhigh hC
  exact ⟨⟨by dsimp [M]; linarith only [hj, ha, hc'],
      by dsimp [M]; linarith only [hi, hb, hc']⟩, hlow, hhigh⟩

/-- A gap between two observed cell closures gives a unique finite
discretized-normal maximum. -/
theorem joint_of_gap {n : ℕ} {x : Fin n → ℤ} {i j : Fin n} (hgap : x i+1 < x j) :
    ∃! p, p ∈ workingDomain ∧ IsMaxOn (workingLikelihood x) workingDomain p := by
  let c : ℝ × ℝ := (0, 1)
  have hc : c ∈ workingDomain := ⟨mem_univ _, show (0 : ℝ) < 1 from zero_lt_one⟩
  let ε := workingLikelihood x c
  have hε : 0 < ε := workingLikelihood_pos x hc
  obtain ⟨M, U, hR⟩ := gap_superlevel_rectangle hgap hε
  let K := Icc (-M) M ×ˢ Icc (ε*gaussianTotal) U
  have hsub : K ⊆ workingDomain := fun p hp ↦ ⟨mem_univ _,
    (mul_pos hε gaussianTotal_pos).trans_le hp.2.1⟩
  have hcK : c ∈ K := hR c hc le_rfl
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  obtain ⟨p, hp, hmax⟩ := hK.exists_isMaxOn ⟨c, hcK⟩
    (workingLikelihood_continuous x).continuousOn
  have hpD := hsub hp
  have hmaxD : IsMaxOn (workingLikelihood x) workingDomain p := by
    intro q hq
    by_cases h : ε ≤ workingLikelihood x q
    · exact hmax (hR q hq h)
    · exact (lt_of_not_ge h).le.trans (hmax hcK)
  have hn : 0 < n := Fin.pos i
  refine ⟨p, ⟨hpD, hmaxD⟩, fun q hq ↦ ?_⟩
  exact (workingLogLik_strictConcave hn x).eq_of_isMaxOn
    (workingLogLik_max_of_Lik hq.1 hq.2) (workingLogLik_max_of_Lik hpD hmaxD) hq.1 hpD

private def gaussianCDF (z : ℝ) : ℝ :=
  (primitive z-primitiveLower)/gaussianTotal

private theorem gaussianCDF_value {q : ℝ} (hq : q ∈ Ioo (0 : ℝ) 1) :
    ∃ z, gaussianCDF z = q := by
  have hc : Continuous gaussianCDF :=
    (primitive_continuous.sub continuous_const).div_const gaussianTotal
  have hbot : Tendsto gaussianCDF atBot (𝓝 0) := by
    unfold gaussianCDF
    simpa using (primitive_atBot.sub_const primitiveLower).div_const gaussianTotal
  have htop : Tendsto gaussianCDF atTop (𝓝 1) := by
    unfold gaussianCDF
    simpa only [primitiveUpper_sub_lower, div_self gaussianTotal_pos.ne'] using
      (primitive_atTop.sub_const primitiveLower).div_const gaussianTotal
  obtain ⟨a, ha⟩ := (hbot.eventually (Iio_mem_nhds hq.1)).exists
  obtain ⟨b, hb, hb'⟩ := (Filter.Eventually.and (eventually_ge_atTop a)
    (htop.eventually (Ioi_mem_nhds hq.2))).exists
  obtain ⟨z, _, hz⟩ := intermediate_value_Icc hb hc.continuousOn ⟨ha.le, hb'.le⟩
  exact ⟨z, hz⟩

private theorem centeredCell_tendsto (k : ℝ) :
    Tendsto (fun B : ℝ ↦ workingCellProb k ((k+1/2)*B,B)) atTop (𝓝 1) := by
  have hleft : Tendsto (fun B : ℝ ↦ k*B-(k+1/2)*B) atTop atBot := by
    have h := tendsto_id.const_mul_atTop_of_neg (show (-1/2 : ℝ) < 0 by norm_num)
    exact h.congr fun B ↦ by change (-1/2 : ℝ)*B = _; ring
  have hright : Tendsto (fun B : ℝ ↦ (k+1)*B-(k+1/2)*B) atTop atTop := by
    have h := tendsto_id.const_mul_atTop (show (1/2 : ℝ) > 0 by norm_num)
    exact h.congr fun B ↦ by change (1/2 : ℝ)*B = _; ring
  unfold workingCellProb intervalProb intervalMass
  simpa only [Function.comp_def, primitiveUpper_sub_lower, div_self gaussianTotal_pos.ne'] using
    ((primitive_atTop.comp hright).sub (primitive_atBot.comp hleft)).div_const gaussianTotal

/-- A nonempty constant sample has the strict point-mass supremum. -/
theorem constant_supremum {n : ℕ} (hn : 0 < n) {x : Fin n → ℤ}
    {k : ℤ} (hx : ∀ i, x i = k) :
    IsLUB (workingLikelihood x '' workingDomain) 1 ∧
      ∀ p ∈ workingDomain, workingLikelihood x p < 1 := by
  let i : Fin n := ⟨0, hn⟩
  have hlt (p : ℝ × ℝ) (hp : p ∈ workingDomain) : workingLikelihood x p < 1 :=
    (workingLikelihood_le_cell x i hp).trans_lt (intervalProb_lt_one _ _)
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hlt p hp).le
  · intro b hb
    let p := fun B : ℝ ↦ (((k : ℝ)+1/2)*B,B)
    have hlim : Tendsto (fun B ↦ workingLikelihood x (p B)) atTop (𝓝 1) := by
      unfold workingLikelihood
      simpa only [hx, Finset.prod_const, Finset.card_univ, Fintype.card_fin, one_pow] using
        (centeredCell_tendsto (k : ℝ)).pow n
    apply le_of_tendsto hlim
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with B hB
    exact hb ⟨p B, ⟨mem_univ _, hB⟩, rfl⟩

/-- Multiplicities of observed cells. -/
def cellCount {n : ℕ} (x : Fin n → ℤ) (k : ℤ) : ℕ :=
  (Finset.univ.filter (fun i ↦ x i = k)).card

private theorem adjacent_product {n : ℕ} {x : Fin n → ℤ} {k : ℤ}
    (hx : ∀ i, x i = k ∨ x i = k+1) (p : ℝ × ℝ) :
    workingLikelihood x p =
      workingCellProb (k : ℝ) p ^ cellCount x k *
        workingCellProb ((k+1 : ℤ) : ℝ) p ^ cellCount x (k+1) := by
  have h (i : Fin n) : workingCellProb (x i : ℝ) p =
      if x i = k then workingCellProb (k : ℝ) p else workingCellProb ((k+1 : ℤ) : ℝ) p := by
    rcases hx i with hi | hi
    · simp [hi]
    · have hne : k+1 ≠ k := by omega
      simp [hi, hne]
  have hF : (Finset.univ.filter (fun i ↦ ¬x i = k)) =
      Finset.univ.filter (fun i ↦ x i = k+1) := by
    ext i
    simp only [Finset.mem_filter, Finset.mem_univ, true_and]
    rcases hx i with hi | hi <;> omega
  unfold workingLikelihood cellCount
  simp_rw [h]
  rw [Finset.prod_ite, hF, Finset.prod_const, Finset.prod_const]

private theorem bernoulli_product_eq (z o : ℕ) {q : ℝ} (hq : q ∈ Ioo (0 : ℝ) 1) :
    Discrete.Binomial.likelihood z o q = q^z*(1-q)^o := by
  have h1 := sub_pos.mpr hq.2
  unfold Discrete.Binomial.likelihood
  rw [ite_eq_right (by
    rintro (⟨h, _⟩ | ⟨h, _⟩)
    · exact hq.1.ne' h
    · exact hq.2.ne h)]
  rw [exp_add, exp_nat_mul, exp_nat_mul, exp_log hq.1, exp_log h1]

private theorem bernoulli_product_le {z o : ℕ} (hz : 0 < z) (ho : 0 < o)
    {q : ℝ} (hq : q ∈ Ioo (0 : ℝ) 1) :
    q^z*(1-q)^o ≤
      ((z : ℝ)/(z+o))^z*((o : ℝ)/(z+o))^o := by
  have hz' : (0 : ℝ) < z := by exact_mod_cast hz
  have ho' : (0 : ℝ) < o := by exact_mod_cast ho
  let q₀ := (z : ℝ)/(z+o)
  have hsum := add_pos hz' ho'
  have hq₀ : q₀ ∈ Ioo (0 : ℝ) 1 :=
    ⟨div_pos hz' hsum, (div_lt_one hsum).mpr (by linarith only [ho'])⟩
  have hc : 1-q₀ = (o : ℝ)/(z+o) := by dsimp [q₀]; field_simp; ring
  have h := (Discrete.Binomial.fixed_size_mle hz'.le ho'.le hsum q₀
    ⟨hq₀.1.le, hq₀.2.le⟩).mpr rfl
  have hle : Discrete.Binomial.likelihood z o q ≤ Discrete.Binomial.likelihood z o q₀ :=
    h ⟨hq.1.le, hq.2.le⟩
  simpa only [bernoulli_product_eq z o hq, bernoulli_product_eq z o hq₀, hc] using hle

private theorem seam_limits (k : ℤ) (z : ℝ) :
    let p := fun B : ℝ ↦ (((k : ℝ)+1)*B-z, B)
    Tendsto (fun B ↦ workingCellProb (k : ℝ) (p B)) atTop (𝓝 (gaussianCDF z)) ∧
      Tendsto (fun B ↦ workingCellProb ((k+1 : ℤ) : ℝ) (p B)) atTop (𝓝 (1-gaussianCDF z)) := by
  let p := fun B : ℝ ↦ (((k : ℝ)+1)*B-z, B)
  have h₁ : Tendsto (fun B : ℝ ↦ z-B) atTop atBot := by
    simpa only [sub_eq_add_neg] using
      tendsto_atBot_add_const_left atTop z (tendsto_neg_atTop_atBot (G := ℝ))
  have h₂ : Tendsto (fun B : ℝ ↦ z+B) atTop atTop :=
    tendsto_atTop_add_const_left atTop z tendsto_id
  have hleft (B : ℝ) : workingCellProb (k : ℝ) (p B) = intervalProb (z-B) z := by
    unfold workingCellProb
    congr 1 <;> dsimp [p] <;> ring
  have hright (B : ℝ) : workingCellProb ((k+1 : ℤ) : ℝ) (p B) = intervalProb z (z+B) := by
    unfold workingCellProb
    push_cast
    congr 1 <;> dsimp [p] <;> ring
  constructor
  · exact ((tendsto_const_nhds (x := primitive z)).sub
      (primitive_atBot.comp h₁) |>.div_const gaussianTotal).congr (fun B ↦ (hleft B).symm)
  · have h : (primitiveUpper-primitive z)/gaussianTotal = 1-gaussianCDF z := by
      unfold gaussianCDF
      have he := primitiveUpper_sub_lower
      field_simp [gaussianTotal_pos.ne']
      linarith only [he]
    rw [← h]
    exact ((primitive_atTop.comp h₂).sub
      (tendsto_const_nhds (x := primitive z)) |>.div_const gaussianTotal).congr
      (fun B ↦ (hright B).symm)

/-- The saturated likelihood when two adjacent cells have the indicated counts. -/
def adjacentHeight (z o : ℕ) : ℝ :=
  ((z : ℝ)/(z+o))^z * ((o : ℝ)/(z+o))^o

/-- A sample occupying exactly two adjacent cells has the strict,
unattained Bernoulli supremum with the empirical proportions. -/
theorem adjacent_supremum {n : ℕ} {x : Fin n → ℤ} {k : ℤ}
    (hx : ∀ i, x i = k ∨ x i = k+1)
    (hz : 0 < cellCount x k) (ho : 0 < cellCount x (k+1)) :
    IsLUB (workingLikelihood x '' workingDomain)
        (adjacentHeight (cellCount x k) (cellCount x (k+1))) ∧
      ∀ p ∈ workingDomain, workingLikelihood x p <
        adjacentHeight (cellCount x k) (cellCount x (k+1)) := by
  let z := cellCount x k
  let o := cellCount x (k+1)
  let h := adjacentHeight z o
  have hlt (p : ℝ × ℝ) (hp : p ∈ workingDomain) : workingLikelihood x p < h := by
    let P := workingCellProb (k : ℝ) p
    let Q := workingCellProb ((k+1 : ℤ) : ℝ) p
    let T := P+Q
    have hP := workingCellProb_pos (k : ℝ) hp
    have hQ := workingCellProb_pos ((k+1 : ℤ) : ℝ) hp
    have hT : 0 < T := add_pos hP hQ
    have hT1 : T < 1 := by
      unfold T P Q workingCellProb
      push_cast
      rw [intervalProb_add]
      exact intervalProb_lt_one _ _
    let q := P/T
    have hq : q ∈ Ioo (0 : ℝ) 1 :=
      ⟨div_pos hP hT, (div_lt_one hT).mpr (lt_add_of_pos_right P hQ)⟩
    have hqP : P = T*q := by dsimp [q]; field_simp [hT.ne']
    have hqQ : Q = T*(1-q) := by
      rw [mul_sub, mul_one, ← hqP]
      dsimp [T]
      ring
    have hpower : T^(z+o) < 1 := pow_lt_one₀ hT.le hT1 (by omega)
    have hprod : 0 < q^z*(1-q)^o :=
      mul_pos (pow_pos hq.1 _) (pow_pos (sub_pos.mpr hq.2) _)
    calc
      workingLikelihood x p = P^z*Q^o := adjacent_product hx p
      _ = T^(z+o)*(q^z*(1-q)^o) := by
        rw [hqP, hqQ, mul_pow, mul_pow, pow_add]
        ring
      _ < q^z*(1-q)^o := by simpa using mul_lt_mul_of_pos_right hpower hprod
      _ ≤ h := bernoulli_product_le hz ho hq
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hlt p hp).le
  · intro b hb
    have hz' : (0 : ℝ) < z := by exact_mod_cast hz
    have ho' : (0 : ℝ) < o := by exact_mod_cast ho
    let q := (z : ℝ)/(z+o)
    have hden := add_pos hz' ho'
    have hq : q ∈ Ioo (0 : ℝ) 1 :=
      ⟨div_pos hz' hden, (div_lt_one hden).mpr (by linarith only [ho'])⟩
    have hq' : 1-q = (o : ℝ)/(z+o) := by dsimp [q]; field_simp; ring
    obtain ⟨τ, hτ⟩ := gaussianCDF_value hq
    let p := fun B : ℝ ↦ (((k : ℝ)+1)*B-τ,B)
    have hlim : Tendsto (fun B ↦ workingLikelihood x (p B)) atTop (𝓝 h) := by
      obtain ⟨hl, hr⟩ := seam_limits k τ
      rw [hτ] at hl hr
      change Tendsto (fun B ↦ workingCellProb (k : ℝ) (p B)) atTop (𝓝 q) at hl
      change Tendsto (fun B ↦ workingCellProb ((k+1 : ℤ) : ℝ) (p B)) atTop (𝓝 (1-q)) at hr
      have hlr := (hl.pow z).mul (hr.pow o)
      rw [hq'] at hlr
      exact hlr.congr (fun B ↦ (adjacent_product hx (p B)).symm)
    apply le_of_tendsto hlim
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with B hB
    exact hb ⟨p B, ⟨mem_univ _, hB⟩, rfl⟩

private theorem no_max_of_strict_sup {n : ℕ} {x : Fin n → ℤ} {u : ℝ}
    (hsup : IsLUB (workingLikelihood x '' workingDomain) u)
    (hlt : ∀ p ∈ workingDomain, workingLikelihood x p < u) :
    ¬ ∃ p ∈ workingDomain, IsMaxOn (workingLikelihood x) workingDomain p := by
  rintro ⟨p, hp, hm⟩
  have hb : workingLikelihood x p ∈ upperBounds (workingLikelihood x '' workingDomain) := by
    rintro _ ⟨q, hq, rfl⟩
    exact hm hq
  exact (not_le_of_gt (hlt p hp)) (hsup.2 hb)

/-- The all-free discretized-normal maximum exists exactly at integer
range at least two. When it exists it is unique. -/
theorem joint_range_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    ((∃ p ∈ workingDomain, IsMaxOn (workingLikelihood x) workingDomain p) ↔
      ∃ i j, x i+1 < x j) ∧
    ((∃ i j, x i+1 < x j) →
      ∃! p, p ∈ workingDomain ∧ IsMaxOn (workingLikelihood x) workingDomain p) := by
  have hfinite (h : ∃ i j, x i+1 < x j) :
      ∃! p, p ∈ workingDomain ∧ IsMaxOn (workingLikelihood x) workingDomain p := by
    obtain ⟨i, j, hij⟩ := h
    exact joint_of_gap hij
  refine ⟨⟨?_, fun h ↦ (hfinite h).exists⟩, hfinite⟩
  intro hmax
  by_contra! h
  obtain ⟨i, _, hi⟩ := Set.exists_min_image (univ : Set (Fin n)) x (Set.toFinite _)
    ⟨⟨0, hn⟩, mem_univ _⟩
  let k := x i
  have hk (j : Fin n) : k ≤ x j := hi j (mem_univ _)
  have hx (j : Fin n) : x j = k ∨ x j = k+1 := by
    have h' := h i j
    change x j ≤ k+1 at h'
    have h₀ := hk j
    omega
  by_cases hconst : ∀ j, x j = k
  · obtain ⟨hsup, hlt⟩ := constant_supremum hn hconst
    exact no_max_of_strict_sup hsup hlt hmax
  · push Not at hconst
    obtain ⟨j, hj⟩ := hconst
    have hj' := (hx j).resolve_left hj
    have hz : 0 < cellCount x k := Finset.card_pos.mpr ⟨i, by simp [k]⟩
    have ho : 0 < cellCount x (k+1) := Finset.card_pos.mpr ⟨j, by simp [hj']⟩
    obtain ⟨hsup, hlt⟩ := adjacent_supremum hx hz ho
    exact no_max_of_strict_sup hsup hlt hmax

/-- The change from ordinary location and positive scale to the working chart. -/
def workingChart (p : ℝ × ℝ) : ℝ × ℝ := (p.1/p.2, p.2⁻¹)


private theorem chart_involution {p : ℝ × ℝ} (hp : p ∈ workingDomain) :
    workingChart (workingChart p) = p := by
  have h : 0 < p.2 := hp.2
  apply Prod.ext
  · change p.1/p.2/(p.2⁻¹) = p.1
    field_simp [h.ne']
  · change (p.2⁻¹)⁻¹ = p.2
    simp

/-- The working chart is bijective on the entire positive-scale domain. -/
theorem workingChart_bijOn : BijOn workingChart workingDomain workingDomain := by
  have hmap : MapsTo workingChart workingDomain workingDomain := fun p hp ↦
    ⟨mem_univ _, show 0 < p.2⁻¹ from inv_pos.mpr hp.2⟩
  refine ⟨hmap, ?_, ?_⟩
  · intro p hp q hq h
    simpa only [chart_involution hp, chart_involution hq] using congrArg workingChart h
  · exact fun p hp ↦ ⟨workingChart p, hmap hp, chart_involution hp⟩

end

end UnivariateML.Discrete3DNorm

end
