module

public import UnivariateML.Family.PGWHeldShape
public import Mathlib.Topology.Algebra.Module.FiniteDimension

/-! Nuisance curvature and analytic all-free profiling for PGW. -/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.PGWJointProfile

noncomputable section

open PGWEnvelope
variable {n : ℕ}

def objective (y : Fin n → ℝ) (q : ℝ × (ℝ × ℝ)) : ℝ :=
  PGWHeldShape.chartObjective y q.2.1 (q.1, q.2.2)

def nuisanceDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ

private theorem objective_analytic (y : Fin n → ℝ) {h : ℝ} {p : ℝ × ℝ}
    (hh : 0 ≤ h) (hp : p ∈ nuisanceDomain) : AnalyticAt ℝ (objective y) (h, p) := by
  let f := fun q : ℝ × (ℝ × ℝ) ↦ (q.2.1, exp q.2.2, q.1)
  have hf : AnalyticAt ℝ f (h, p) := by
    have hs : AnalyticAt ℝ (fun q : ℝ × (ℝ × ℝ) ↦ q.2) (h, p) := analyticAt_snd
    exact (analyticAt_fst.comp hs).prod
      ((analyticAt_rexp.comp (analyticAt_snd.comp hs)).prod analyticAt_fst)
  exact (analytic_chart y (p := (p.1, exp p.2, h)) ⟨hp.1, exp_pos _, hh⟩).comp_of_eq hf rfl

def score (y : Fin n → ℝ) (q : ℝ × (ℝ × ℝ)) : ℝ × ℝ :=
  (fderiv ℝ (objective y) q (0, 1, 0), fderiv ℝ (objective y) q (0, 0, 1))

private theorem score_analytic (y : Fin n → ℝ) {h : ℝ} {p : ℝ × ℝ}
    (hh : 0 ≤ h) (hp : p ∈ nuisanceDomain) : AnalyticAt ℝ (score y) (h, p) := by
  let A : ((ℝ × (ℝ × ℝ)) →L[ℝ] ℝ) →L[ℝ] ℝ := ContinuousLinearMap.apply ℝ ℝ (0, 1, 0)
  let B : ((ℝ × (ℝ × ℝ)) →L[ℝ] ℝ) →L[ℝ] ℝ := ContinuousLinearMap.apply ℝ ℝ (0, 0, 1)
  have ha := (objective_analytic y hh hp).fderiv
  exact ((A.analyticAt _).comp ha).prod ((B.analyticAt _).comp ha)

private theorem path_deriv (h : ℝ) (p w : ℝ × ℝ) :
    HasDerivAt (fun t : ℝ ↦ (h, p + t • w)) (0, w) 0 := by
  simpa using (hasDerivAt_const (0 : ℝ) h).prodMk
    (((hasDerivAt_id (0 : ℝ)).smul_const w).const_add p)

private theorem path_pos {p w : ℝ × ℝ} (hp : p ∈ nuisanceDomain) :
    ∀ᶠ t : ℝ in 𝓝 0, 0 < (p + t • w).1 := by
  have hd : ContinuousAt (fun t : ℝ ↦ p.1 + t * w.1) 0 :=
    (continuousAt_id.mul_const w.1).const_add p.1
  exact (hd.tendsto.eventually (Ioi_mem_nhds (by simpa using (show 0 < p.1 from hp.1))))

private theorem directional_formula (y : Fin n → ℝ) {h K d : ℝ} {p : ℝ × ℝ}
    (hp : p ∈ nuisanceDomain) (hh : 0 ≤ h) {f : ℝ → ℝ}
    (hf : ∀ v, DifferentiableAt ℝ f v)
    (he : ∀ {b : ℝ}, 0 < b → ∀ v, PGWHeldShape.chartObjective y b (h, v) =
      (n : ℝ) * log b + K + ∑ i, f (v + b * y i + d))
    (w : ℝ × ℝ) :
    fderiv ℝ (objective y) (h, p) (0, w) =
      (n : ℝ) / p.1 * w.1 + ∑ i, deriv f (p.2 + p.1 * y i + d) * (w.2 + w.1 * y i) := by
  have hD (a v : ℝ) : HasDerivAt (fun t : ℝ ↦ a + t * v) v 0 := by
    simpa using ((hasDerivAt_id (0 : ℝ)).mul_const v).const_add a
  have hL := ((hD p.1 w.1).log (by simpa using (show 0 < p.1 from hp.1).ne')).const_mul (n : ℝ)
  have hsum : HasDerivAt
      (fun t : ℝ ↦ ∑ i, f (p.2 + t * w.2 + (p.1 + t * w.1) * y i + d))
      (∑ i, deriv f (p.2 + p.1 * y i + d) * (w.2 + w.1 * y i)) 0 := by
    apply HasDerivAt.fun_sum
    intro i _
    have h := (((hD p.2 w.2).add ((hD p.1 w.1).mul_const (y i))).add_const d)
    exact (hf _).hasDerivAt.comp_of_eq 0 h (by simp)
  have hall := (hL.add_const K).add hsum
  have hd : HasDerivAt (fun t : ℝ ↦ objective y (h, p + t • w))
      ((n : ℝ) / p.1 * w.1 + ∑ i, deriv f (p.2 + p.1 * y i + d) * (w.2 + w.1 * y i)) 0 := by
    have hEq : (fun t : ℝ ↦ objective y (h, p + t • w)) =ᶠ[𝓝 0]
        (fun t ↦ (n : ℝ) * log (p.1 + t * w.1) + K +
          ∑ i, f (p.2 + t * w.2 + (p.1 + t * w.1) * y i + d)) := by
      filter_upwards [path_pos (w := w) hp] with t ht
      exact he ht (p + t • w).2
    convert hall.congr_of_eventuallyEq hEq using 1
    simp only [zero_mul, add_zero]
    ring
  have hA := (objective_analytic y hh hp).differentiableAt.hasFDerivAt
  have ht := HasFDerivAt.comp_hasDerivAt_of_eq 0 hA (path_deriv h p w) (by simp)
  exact ht.deriv.symm.trans hd.deriv

private theorem score_formula (y : Fin n → ℝ) {h K d : ℝ} {p : ℝ × ℝ}
    (hp : p ∈ nuisanceDomain) (hh : 0 ≤ h) {f : ℝ → ℝ}
    (hf : ∀ v, DifferentiableAt ℝ f v)
    (he : ∀ {b : ℝ}, 0 < b → ∀ v, PGWHeldShape.chartObjective y b (h, v) =
      (n : ℝ) * log b + K + ∑ i, f (v + b * y i + d)) :
    score y (h, p) =
      ((n : ℝ) / p.1 + ∑ i, deriv f (p.2 + p.1 * y i + d) * y i,
        ∑ i, deriv f (p.2 + p.1 * y i + d)) := by
  apply Prod.ext
  · simpa [score] using directional_formula y hp hh hf he (1, 0)
  · simpa [score] using directional_formula y hp hh hf he (0, 1)

private theorem score_derivative (y : Fin n → ℝ) {h : ℝ} {p : ℝ × ℝ}
    (hp : p ∈ nuisanceDomain) (hh : 0 ≤ h) (w : ℝ × ℝ) :
    ∃ a : Fin n → ℝ, (∀ i, a i < 0) ∧
      HasDerivAt (fun t : ℝ ↦ score y (h, p + t • w))
        (-(n : ℝ) / p.1 ^ 2 * w.1 + ∑ i, a i * (w.2 + w.1 * y i) * y i,
          ∑ i, a i * (w.2 + w.1 * y i)) 0 := by
  obtain ⟨K, d, f, hneg, hf, he⟩ := PGWHeldShape.joint_section_forms y hh
  let z := fun i ↦ p.2 + p.1 * y i + d
  let a := fun i ↦ deriv (deriv f) (z i)
  refine ⟨a, fun i ↦ hneg _, ?_⟩
  have hg (v : ℝ) : DifferentiableAt ℝ (deriv f) v := by
    by_contra h
    exact (hneg v).ne (deriv_zero_of_not_differentiableAt h)
  let u := fun (i : Fin n) (t : ℝ) ↦ deriv f (p.2 + t * w.2 + (p.1 + t * w.1) * y i + d)
  have hD (v q : ℝ) : HasDerivAt (fun t : ℝ ↦ v + t * q) q 0 := by
    simpa using ((hasDerivAt_id (0 : ℝ)).mul_const q).const_add v
  have hu (i : Fin n) : HasDerivAt (u i) (a i * (w.2 + w.1 * y i)) 0 := by
    exact (hg (z i)).hasDerivAt.comp_of_eq 0
      (((hD p.2 w.2).add ((hD p.1 w.1).mul_const (y i))).add_const d) (by simp [z])
  have hsum : HasDerivAt (fun t : ℝ ↦ ∑ i, u i t * y i)
      (∑ i, a i * (w.2 + w.1 * y i) * y i) 0 :=
    HasDerivAt.fun_sum fun i _ ↦ (hu i).mul_const (y i)
  have hsum' : HasDerivAt (fun t : ℝ ↦ ∑ i, u i t)
      (∑ i, a i * (w.2 + w.1 * y i)) 0 := HasDerivAt.fun_sum fun i _ ↦ hu i
  have hinv := (hasDerivAt_const (0 : ℝ) (n : ℝ)).div (hD p.1 w.1)
    (by simpa using (show 0 < p.1 from hp.1).ne')
  have hall := (hinv.add hsum).prodMk hsum'
  convert hall.congr_of_eventuallyEq (by
    filter_upwards [path_pos (w := w) hp] with t ht
    exact score_formula y ⟨ht, mem_univ _⟩ hh hf he) using 1
  simp only [zero_mul, add_zero, zero_sub]
  congr 1
  ring

private def dot (v w : ℝ × ℝ) : ℝ := v.1 * w.1 + v.2 * w.2

private theorem negative_quadratic (hn : 0 < n) (y : Fin n → ℝ) {b : ℝ} (hb : 0 < b)
    (a : Fin n → ℝ) (ha : ∀ i, a i < 0) {w : ℝ × ℝ} (hw : w ≠ 0) :
    dot (-(n : ℝ) / b ^ 2 * w.1 + ∑ i, a i * (w.2 + w.1 * y i) * y i,
      ∑ i, a i * (w.2 + w.1 * y i)) w < 0 := by
  have he : dot (-(n : ℝ) / b ^ 2 * w.1 + ∑ i, a i * (w.2 + w.1 * y i) * y i,
      ∑ i, a i * (w.2 + w.1 * y i)) w =
      -(n : ℝ) / b ^ 2 * w.1 ^ 2 + ∑ i, a i * (w.2 + w.1 * y i) ^ 2 := by
    unfold dot
    simp only [add_mul, Finset.sum_mul]
    rw [add_assoc, ← Finset.sum_add_distrib]
    congr 1
    · ring
    · apply Finset.sum_congr rfl
      intro i _
      ring
  rw [he]
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hcoef : -(n : ℝ) / b ^ 2 < 0 := div_neg_of_neg_of_pos (neg_neg_of_pos hnR) (by positivity)
  have hle (i : Fin n) : a i * (w.2 + w.1 * y i) ^ 2 ≤ 0 :=
    mul_nonpos_of_nonpos_of_nonneg (ha i).le (sq_nonneg _)
  by_cases h : w.1 = 0
  · have h₂ : w.2 ≠ 0 := fun he ↦ hw (Prod.ext h he)
    have hlt : ∑ i, a i * (w.2 + w.1 * y i) ^ 2 < 0 := by
      have hh := Finset.sum_neg (s := Finset.univ) (f := fun i ↦ a i * (w.2 + w.1 * y i) ^ 2)
        (fun i _ ↦ by
          rw [h, zero_mul, add_zero]
          exact mul_neg_of_neg_of_pos (ha i) (sq_pos_of_ne_zero h₂))
        ⟨⟨0, hn⟩, Finset.mem_univ _⟩
      exact hh
    simpa [h] using hlt
  · exact add_neg_of_neg_of_nonpos (mul_neg_of_neg_of_pos hcoef (sq_pos_of_ne_zero h))
      (Finset.sum_nonpos fun i _ ↦ hle i)

private theorem dot_hessian_neg (hn : 0 < n) (y : Fin n → ℝ) {h : ℝ}
    {p : ℝ × ℝ} (hp : p ∈ nuisanceDomain) (hh : 0 ≤ h) {w : ℝ × ℝ} (hw : w ≠ 0) :
    dot (fderiv ℝ (score y) (h, p) (0, w)) w < 0 := by
  obtain ⟨a, ha, hD⟩ := score_derivative y hp hh w
  have hA := (score_analytic y hh hp).differentiableAt.hasFDerivAt
  have ht := HasFDerivAt.comp_hasDerivAt_of_eq 0 hA (path_deriv h p w) (by simp)
  have he := ht.deriv.symm.trans hD.deriv
  change fderiv ℝ (score y) (h, p) (0, w) = _ at he
  rw [he]
  exact negative_quadratic hn y hp.1 a ha hw

/-- The joint nuisance score has a negative-definite derivative, including
at the Gompertz face. This supplies the analytic implicit-function condition. -/
theorem nuisance_hessian_invertible (hn : 0 < n) (y : Fin n → ℝ) {h : ℝ}
    {p : ℝ × ℝ} (hp : p ∈ nuisanceDomain) (hh : 0 ≤ h) :
    (fderiv ℝ (score y) (h, p) ∘L ContinuousLinearMap.inr ℝ ℝ (ℝ × ℝ)).IsInvertible := by
  let A := fderiv ℝ (score y) (h, p) ∘L ContinuousLinearMap.inr ℝ ℝ (ℝ × ℝ)
  have hneg {w : ℝ × ℝ} (hw : w ≠ 0) : dot (A w) w < 0 :=
    dot_hessian_neg hn y hp hh hw
  have hinj : Function.Injective A := by
    intro v w he
    by_contra h
    have hh' := hneg (sub_ne_zero.mpr h)
    rw [map_sub, he, sub_self] at hh'
    simp [dot] at hh'
  let eqv := (LinearEquiv.ofBijective A.toLinearMap
    ⟨hinj, LinearMap.surjective_of_injective hinj⟩).toContinuousLinearEquiv
  exact ⟨eqv, rfl⟩

private theorem score_injective (hn : 0 < n) (y : Fin n → ℝ)
    {h : ℝ} (hh : 0 ≤ h) : InjOn (fun p ↦ score y (h, p)) nuisanceDomain := by
  intro p hp q hq he
  by_contra hne
  let w := q - p
  have hw : w ≠ 0 := sub_ne_zero.mpr (Ne.symm hne)
  let g := fun t : ℝ ↦ dot (score y (h, p + t • w)) w
  have hD (t : ℝ) (ht : t ∈ Icc (0 : ℝ) 1) : (p + t • w) ∈ nuisanceDomain := by
    refine ⟨?_, mem_univ _⟩
    have hp₀ : 0 < p.1 := hp.1
    have hq₀ : 0 < q.1 := hq.1
    change 0 < p.1 + t * (q.1 - p.1)
    by_cases h : t = 0
    · simpa [h] using hp₀
    · have hpos : 0 < t := lt_of_le_of_ne ht.1 (Ne.symm h)
      have he := add_pos_of_nonneg_of_pos
        (mul_nonneg (sub_nonneg.mpr ht.2) hp₀.le) (mul_pos hpos hq₀)
      nlinarith only [he]
  have hd (t : ℝ) (ht : t ∈ Icc (0 : ℝ) 1) :
      HasDerivAt g (dot (fderiv ℝ (score y) (h, p + t • w) (0, w)) w) t := by
    have hP : HasDerivAt (fun s : ℝ ↦ (h, p + s • w)) (0, w) t := by
      simpa using (hasDerivAt_const t h).prodMk
        (((hasDerivAt_id t).smul_const w).const_add p)
    have hS := (score_analytic y hh (hD t ht)).differentiableAt.hasFDerivAt
    have hs := HasFDerivAt.comp_hasDerivAt_of_eq t hS hP rfl
    let D : (ℝ × ℝ) →L[ℝ] ℝ :=
      w.1 • ContinuousLinearMap.fst ℝ ℝ ℝ + w.2 • ContinuousLinearMap.snd ℝ ℝ ℝ
    simpa [g, dot, D, Function.comp_def, mul_comm] using
      (HasFDerivAt.comp_hasDerivAt t D.hasFDerivAt hs)
  have hmono : StrictAntiOn g (Icc 0 1) := by
    apply strictAntiOn_of_deriv_neg (convex_Icc _ _)
    · exact fun t ht ↦ (hd t ht).continuousAt.continuousWithinAt
    · intro t ht
      have ht' : t ∈ Icc (0 : ℝ) 1 := interior_subset ht
      rw [(hd t ht').deriv]
      exact dot_hessian_neg hn y (hD t ht') hh hw
  have hlt := hmono (show 0 ∈ Icc (0 : ℝ) 1 from ⟨le_rfl, zero_le_one⟩)
    (show 1 ∈ Icc (0 : ℝ) 1 from ⟨zero_le_one, le_rfl⟩) zero_lt_one
  have hwq : p + w = q := by dsimp [w]; abel
  simp only [g, zero_smul, add_zero, one_smul, hwq, he, lt_self_iff_false] at hlt

private theorem max_score_zero (y : Fin n → ℝ) {h : ℝ} {p : ℝ × ℝ}
    (hh : 0 ≤ h) (hp : p ∈ nuisanceDomain)
    (hm : IsMaxOn (fun q ↦ objective y (h, q)) nuisanceDomain p) :
    score y (h, p) = 0 := by
  have hz (w : ℝ × ℝ) : fderiv ℝ (objective y) (h, p) (0, w) = 0 := by
    have hA := (objective_analytic y hh hp).differentiableAt.hasFDerivAt
    have hD := HasFDerivAt.comp_hasDerivAt_of_eq 0 hA (path_deriv h p w) (by simp)
    rw [← hD.deriv]
    apply IsLocalMax.deriv_eq_zero
    filter_upwards [path_pos (w := w) hp] with t ht
    have he := isMaxOn_iff.mp hm (p + t • w) ⟨ht, mem_univ _⟩
    simpa [Function.comp_def] using he
  exact Prod.ext (hz (1, 0)) (hz (0, 1))

/-- Section maxima exist at every inverse exponent on nonconstant data,
including the Gompertz face at zero. -/
theorem section_attains {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    {h : ℝ} (hh : 0 ≤ h) :
    ∃ p ∈ nuisanceDomain, IsMaxOn (fun q ↦ objective y (h, q)) nuisanceDomain p := by
  rcases hh.eq_or_lt with he | hh
  · subst h
    obtain ⟨⟨b, r⟩, ⟨hb, hr⟩, hm⟩ := gompertz_face_attains hij
    refine ⟨(b, log r), ⟨hb, mem_univ _⟩, ?_⟩
    intro q hq
    change gompertzProduct y q.1 (exp q.2) ≤ gompertzProduct y b (exp (log r))
    rw [exp_log hr]
    exact isMaxOn_iff.mp hm (q.1, exp q.2) ⟨hq.1, exp_pos _⟩
  · let x := fun l ↦ exp (y l)
    have hx : ∀ l, 0 < x l := fun _ ↦ exp_pos _
    have hne : ∃ s t, x s ≠ x t := ⟨i, j, (exp_lt_exp.mpr hij).ne⟩
    obtain ⟨⟨b, a⟩, ⟨hb, ha⟩, hm⟩ :=
      (ShapeFacesPGW.held_exponent_mle (Fin.pos i) hx (inv_pos.mpr hh) hne).exists
    let v := -b * log a - log h
    have he (q : ℝ × ℝ) (hq : q ∈ nuisanceDomain) :
        objective y (h, q) =
          ShapeFacesPGW.logLik x q.1 (exp (-(log h + q.2) / q.1)) h⁻¹ + ∑ i, log (x i) := by
      unfold objective PGWHeldShape.chartObjective
      dsimp only
      rw [chart_surjective _ hq.1 (exp_pos _) hh,
        ← physical_eq hx hq.1 (inv_pos.mpr hh) (exp_pos (-(log h + q.2) / q.1)),
        log_exp, log_mul hh.ne' (exp_pos _).ne', log_exp]
      simp only [x, log_exp]
    refine ⟨(b, v), ⟨hb, mem_univ _⟩, ?_⟩
    intro q hq
    have hp : (b, v) ∈ nuisanceDomain := ⟨hb, mem_univ _⟩
    have hb₀ : 0 < b := hb
    have hv : -(log h + v) / b = log a := by dsimp [v]; field_simp [hb₀.ne']; ring
    change objective y (h, q) ≤ objective y (h, (b, v))
    rw [he q hq, he _ hp]
    dsimp only
    rw [hv, exp_log ha]
    let a' := exp (-(log h + q.2) / q.1)
    exact add_le_add_left (isMaxOn_iff.mp hm (q.1, a') ⟨hq.1, exp_pos _⟩) _

def nuisance {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j) (h : ℝ) : ℝ × ℝ :=
  if hh : 0 ≤ h then (section_attains hij hh).choose else (1, 0)

def profile {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j) (h : ℝ) : ℝ :=
  objective y (h, nuisance hij h)

private theorem nuisance_spec {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    {h : ℝ} (hh : 0 ≤ h) :
    nuisance hij h ∈ nuisanceDomain ∧
      IsMaxOn (fun p ↦ objective y (h, p)) nuisanceDomain (nuisance hij h) := by
  rw [nuisance, dite_eq_left hh]
  exact (section_attains hij hh).choose_spec

/-- The optimized all-free nuisance section extends analytically locally
at every nonnegative inverse exponent, including zero. -/
theorem analytic_profile_extension {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    {h : ℝ} (hh : 0 ≤ h) :
    ∃ F : ℝ → ℝ, AnalyticAt ℝ F h ∧ F =ᶠ[𝓝[Ici 0] h] profile hij := by
  let p := nuisance hij h
  have hp := (nuisance_spec hij hh).1
  obtain ⟨ψ, hA, hv, hS, _⟩ := PGWProfile.analytic_nuisance_branch
    (score_analytic y hh hp) (nuisance_hessian_invertible (Fin.pos i) y hp hh)
  have hz : score y (h, p) = 0 := max_score_zero y hh hp (nuisance_spec hij hh).2
  let F := fun t ↦ objective y (t, ψ t)
  refine ⟨F, ?_, ?_⟩
  · exact (objective_analytic y hh hp).comp_of_eq (analyticAt_id.prod hA)
      (by simpa using congrArg (fun v ↦ (h, v)) hv)
  · have hψ : ∀ᶠ t in 𝓝 h, ψ t ∈ nuisanceDomain := by
      have hpos : 0 < p.1 := hp.1
      have ht := (continuous_fst.continuousAt.tendsto.comp hA.continuousAt.tendsto)
      rw [hv] at ht
      exact (ht.eventually (Ioi_mem_nhds hpos)).mono fun _ ht ↦ ⟨ht, mem_univ _⟩
    filter_upwards [eventually_nhdsWithin_of_eventually_nhds (hS.and hψ), self_mem_nhdsWithin]
      with t ⟨ht, htp⟩ (ht₀ : 0 ≤ t)
    have hsp := nuisance_spec hij ht₀
    have he := (score_injective (Fin.pos i) y ht₀) htp hsp.1
      (ht.trans (hz.trans (max_score_zero y ht₀ hsp.1 hsp.2).symm))
    simp [F, profile, he]


def fit {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j) (h : ℝ) : ℝ × ℝ × ℝ :=
  let p := nuisance hij h
  (p.1, h⁻¹, -(log h + p.2) / p.1)

def value (y : Fin n → ℝ) (q : ℝ × ℝ × ℝ) : ℝ := logProduct y q.1 q.2.1 q.2.2

private theorem section_value (y : Fin n → ℝ) {h : ℝ} (hh : 0 < h)
    {p : ℝ × ℝ} (hp : p ∈ nuisanceDomain) :
    objective y (h, p) = value y (p.1, h⁻¹, -(log h + p.2) / p.1) := by
  unfold objective PGWHeldShape.chartObjective value
  dsimp only
  rw [chart_surjective y hp.1 (exp_pos _) hh, log_mul hh.ne' (exp_pos _).ne', log_exp]

private theorem fit_value {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    {h : ℝ} (hh : 0 < h) : fit hij h ∈ physicalDomain ∧ value y (fit hij h) = profile hij h := by
  have hp := (nuisance_spec hij hh.le).1
  exact ⟨⟨hp.1, inv_pos.mpr hh, mem_univ _⟩, (section_value y hh hp).symm⟩

private def nuisanceOf (q : ℝ × ℝ × ℝ) : ℝ × ℝ :=
  (q.1, log q.2.1 - q.1 * q.2.2)

private theorem inverse_section {y : Fin n → ℝ} {q : ℝ × ℝ × ℝ} (hq : q ∈ physicalDomain) :
    objective y (q.2.1⁻¹, nuisanceOf q) = value y q := by
  have hc : 0 < q.2.1 := hq.2.1
  unfold objective PGWHeldShape.chartObjective nuisanceOf value
  dsimp only
  rw [sub_eq_add_neg, exp_add, exp_log hc, ← chart_physical y hq.1 hc, neg_mul]

private theorem profile_dominates {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    {q : ℝ × ℝ × ℝ} (hq : q ∈ physicalDomain) :
    value y q ≤ profile hij q.2.1⁻¹ := by
  rw [← inverse_section hq]
  exact isMaxOn_iff.mp (nuisance_spec hij (inv_pos.mpr hq.2.1).le).2 _
    ⟨hq.1, mem_univ _⟩

private theorem profile_face {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ gompertzProduct y q.1 q.2) Weibull.domain p ∧
        profile hij 0 = gompertzProduct y p.1 p.2 := by
  let p := nuisance hij 0
  have hp := nuisance_spec hij (h := 0) le_rfl
  refine ⟨(p.1, exp p.2), ⟨hp.1.1, exp_pos _⟩, ?_, rfl⟩
  intro q hq
  have hq₂ : 0 < q.2 := hq.2
  have hm := isMaxOn_iff.mp hp.2 (q.1, log q.2)
    (show (q.1, log q.2) ∈ nuisanceDomain from ⟨hq.1, mem_univ _⟩)
  change gompertzProduct y q.1 (exp (log q.2)) ≤ gompertzProduct y p.1 (exp p.2) at hm
  rwa [exp_log hq₂] at hm

private theorem max_profile_coordinate {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) {q : ℝ × ℝ × ℝ} (hq : q ∈ physicalDomain)
    (hm : IsMaxOn (value y) physicalDomain q) :
    let h := q.2.1⁻¹
    q = fit hij h ∧ IsMaxOn (profile hij) (Ioi 0) h := by
  intro h
  have hc : 0 < q.2.1 := hq.2.1
  have hh : 0 < h := inv_pos.mpr hc
  have hn := nuisance_spec hij hh.le
  have hN : nuisanceOf q ∈ nuisanceDomain := ⟨hq.1, mem_univ _⟩
  have hqN : IsMaxOn (fun p ↦ objective y (h, p)) nuisanceDomain (nuisanceOf q) := by
    intro p hp
    change objective y (h, p) ≤ objective y (h, nuisanceOf q)
    rw [section_value y hh hp, show objective y (h, nuisanceOf q) = value y q from
      inverse_section hq]
    exact isMaxOn_iff.mp hm _ ⟨hp.1, inv_pos.mpr hh, mem_univ _⟩
  have he := (score_injective (Fin.pos i) y hh.le) hN hn.1
    ((max_score_zero y hh.le hN hqN).trans (max_score_zero y hh.le hn.1 hn.2).symm)
  have hfit : q = fit hij h := by
    unfold fit
    rw [← he]
    have hb : 0 < q.1 := hq.1
    have hμ : -(log h + (nuisanceOf q).2) / (nuisanceOf q).1 = q.2.2 := by
      dsimp [h, nuisanceOf]
      rw [log_inv]
      field_simp [hb.ne']
      ring
    rcases q with ⟨b, c, μ⟩
    dsimp [h, nuisanceOf] at hμ ⊢
    rw [inv_inv, hμ]
  refine ⟨hfit, fun t ht ↦ ?_⟩
  have ht' : 0 < t := ht
  have htV := fit_value hij ht'
  have hhV := fit_value hij hh
  have hl := isMaxOn_iff.mp hm _ htV.1
  rw [htV.2, hfit, hhV.2] at hl
  exact hl

private theorem candidate_list {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    (hmin : ∀ l, y i ≤ y l) (hmax : ∀ l, y l ≤ y j) :
    ∃ (F : ℝ → ℝ) (k ε H : ℝ) (R : Set ℝ),
      AnalyticOnNhd ℝ F (Ici 0) ∧ EqOn F (profile hij) (Ici 0) ∧
      0 < k ∧ IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k ∧
      0 < ε ∧ ε ≤ H ∧ (∀ h ∈ Ioo 0 ε, deriv F h ≠ 0) ∧
      (∀ h > H, F h < thresholdProduct y (y i) k) ∧
      R = Icc ε H ∩ {h | deriv F h = 0} ∧ R.Finite := by
  obtain ⟨F, hF, hEq⟩ := PGWProfile.extend_nonnegative
    (fun _ hh ↦ analytic_profile_extension hij hh)
  obtain ⟨k, hk, hkmax, _⟩ := threshold_face_attains hij hmin
  obtain ⟨H₀, hH₀, hTail⟩ := strict_inverse_exponent_tail hij hmin hmax hkmax
  have hcut (h : ℝ) (hh : H₀ < h) : F h < thresholdProduct y (y i) k := by
    have hh₀ := hH₀.trans hh
    rw [hEq hh₀.le, ← (fit_value hij hh₀).2]
    exact hTail (nuisance_spec hij hh₀.le).1.1 hh
  have hnz : ∃ h ∈ Ici (0 : ℝ), deriv F h ≠ 0 := by
    by_contra! h
    let H₁ := H₀ + 1
    have hH₁ : H₀ < H₁ := by dsimp [H₁]; linarith
    have hconst (t : ℝ) (ht : 0 < t) : F t = F H₁ :=
      isOpen_Ioi.is_const_of_deriv_eq_zero (convex_Ioi (0 : ℝ)).isPreconnected
        (fun x hx ↦ (hF x (le_of_lt hx)).differentiableAt.differentiableWithinAt)
        (fun _ ht' ↦ h _ (le_of_lt ht')) ht (hH₀.trans hH₁)
    have hb (p : ℝ × ℝ × ℝ) (hp : p ∈ physicalDomain) : value y p ≤ F H₁ := by
      have ht := inv_pos.mpr (show 0 < p.2.1 from hp.2.1)
      have he := profile_dominates hij hp
      rwa [← hEq ht.le, hconst _ ht] at he
    have ht := (faces_le_of_upper_bound hmin hb).2 k hk
    exact (hcut H₁ hH₁).not_ge ht
  obtain ⟨⟨ε, hε, hcollar⟩, _⟩ := PGWProfile.analytic_collar_and_finite hF.deriv
    (convex_Ici _).isPreconnected (show (0 : ℝ) ∈ Ici 0 from le_rfl)
    hnz (K := ∅) isCompact_empty (empty_subset _)
  let H := max ε H₀
  have hhR : Icc ε H ⊆ Ici 0 := fun _ hr ↦ hε.le.trans hr.1
  obtain ⟨_, hR⟩ := PGWProfile.analytic_collar_and_finite hF.deriv
    (convex_Ici _).isPreconnected (show (0 : ℝ) ∈ Ici 0 from le_rfl) hnz isCompact_Icc hhR
  exact ⟨F, k, ε, H, _, hF, hEq, hk, hkmax, hε, le_max_left .., hcollar,
    fun h hh ↦ hcut h ((le_max_right ..).trans_lt hh), rfl, hR⟩


private theorem retained_winners {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (hmax : ∀ l, y l ≤ y j)
    {F : ℝ → ℝ} {k ε H : ℝ}
    (hEq : EqOn F (profile hij) (Ici 0)) (hk : 0 < k)
    (hkmax : IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k)
    (hε : 0 < ε) (hcollar : ∀ h ∈ Ioo 0 ε, deriv F h ≠ 0)
    (hTail : ∀ h > H, F h < thresholdProduct y (y i) k) :
    let C := max (F 0) (thresholdProduct y (y i) k)
    let R := Icc ε H ∩ {h | deriv F h = 0}
    let W := {h ∈ R | C ≤ F h ∧ ∀ t ∈ R, F t ≤ F h}
    ((∃ p ∈ physicalDomain, IsMaxOn (value y) physicalDomain p) ↔
      ∃ h ∈ R, C ≤ F h) ∧
    BijOn (fit hij) W {p ∈ physicalDomain | IsMaxOn (value y) physicalDomain p} := by
  intro C R W
  obtain ⟨g, hg, hG, hgF⟩ := profile_face hij
  have hface : F 0 = gompertzProduct y g.1 g.2 :=
    (hEq (show 0 ≤ (0 : ℝ) from le_rfl)).trans hgF
  have hfit {h : ℝ} (hh : 0 < h) :
      fit hij h ∈ physicalDomain ∧ value y (fit hij h) = F h := by
    exact ⟨(fit_value hij hh).1, (fit_value hij hh).2.trans (hEq hh.le).symm⟩
  have hmem {p : ℝ × ℝ × ℝ} (hp : p ∈ physicalDomain)
      (hm : IsMaxOn (value y) physicalDomain p) :
      p.2.1⁻¹ ∈ W ∧ p = fit hij p.2.1⁻¹ := by
    let h := p.2.1⁻¹
    have hh : 0 < h := inv_pos.mpr hp.2.1
    obtain ⟨hf, hmf⟩ := max_profile_coordinate hij hp hm
    have hmF : IsMaxOn F (Ioi 0) h := by
      intro t ht
      change F t ≤ F h
      rw [hEq (show 0 < t from ht).le, hEq hh.le]
      exact hmf ht
    have hLd : value y p = F h := by rw [hf, (hfit hh).2]
    obtain ⟨hGb, hTb⟩ := faces_le_of_upper_bound hmin
      (fun q hq ↦ isMaxOn_iff.mp hm q hq)
    have hC : C ≤ F h := by
      rw [← hLd]
      exact max_le (hface.symm ▸ hGb g.1 hg.1 g.2 hg.2) (hTb k hk)
    have hz : deriv F h = 0 := (hmF.isLocalMax (Ioi_mem_nhds hh)).deriv_eq_zero
    have hlo : ε ≤ h := le_of_not_gt fun ht ↦ (hcollar _ ⟨hh, ht⟩) hz
    have hhi : h ≤ H := le_of_not_gt fun ht ↦ (hTail _ ht).not_ge
      ((le_max_right _ _).trans hC)
    exact ⟨⟨⟨⟨hlo, hhi⟩, hz⟩, hC, fun t ht ↦ hmF (hε.trans_le ht.1.1)⟩, hf⟩
  have hex {h : ℝ} (hr : h ∈ R) (hc : C ≤ F h) :
      ∃ p ∈ physicalDomain, IsMaxOn (value y) physicalDomain p := by
    have hh : 0 < h := hε.trans_le hr.1.1
    have hw := hfit hh
    apply (all_free_attains_iff_witness hij hmin hmax).mpr
    refine ⟨fit hij h, hw.1, ?_, ?_⟩
    · intro b hb r hr
      have hg' := isMaxOn_iff.mp hG (b, r) (show (b, r) ∈ Weibull.domain from ⟨hb, hr⟩)
      change gompertzProduct y b r ≤ value y (fit hij h)
      rw [hw.2]
      exact (hg'.trans_eq hface.symm).trans ((le_max_left _ _).trans hc)
    · intro t ht
      change thresholdProduct y (y i) t ≤ value y (fit hij h)
      rw [hw.2]
      exact (hkmax ht).trans ((le_max_right _ _).trans hc)
  constructor
  · constructor
    · rintro ⟨p, hp, hm⟩
      have hv := (hmem hp hm).1
      exact ⟨_, hv.1, hv.2.1⟩
    · rintro ⟨h, hr, hc⟩
      exact hex hr hc
  · refine ⟨?_, ?_, ?_⟩
    · intro h hh
      have hh₀ := hε.trans_le hh.1.1.1
      have hf := hfit hh₀
      obtain ⟨p, hp, hm⟩ := hex hh.1 hh.2.1
      have hv := hmem hp hm
      have p₀ : 0 < p.2.1⁻¹ := inv_pos.mpr hp.2.1
      have he : value y p ≤ F h := by
        rw [hv.2, (hfit p₀).2]
        exact hh.2.2 _ hv.1.1
      refine ⟨hf.1, fun q hq ↦ ?_⟩
      rw [show value y (fit hij h) = F h from hf.2]
      exact (hm hq).trans he
    · intro h _ t _ he
      have he' : h⁻¹ = t⁻¹ := congrArg (fun q : ℝ × ℝ × ℝ ↦ q.2.1) he
      simpa using congrArg Inv.inv he'
    · intro p hp
      have h := hmem hp.1 hp.2
      exact ⟨p.2.1⁻¹, h.1, h.2.symm⟩

/-- All-free PGW criterion and maximizer enumeration. Constants have no
finite maximizer. For nonconstants the retained finite list determines
attainment and captures every tying finite winner exactly once. -/
theorem all_free_criterion_and_uniqueness (hn : 0 < n) (y : Fin n → ℝ) :
    ((∀ i j, y i = y j) → ¬ ∃ p ∈ physicalDomain, IsMaxOn (value y) physicalDomain p) ∧
    ((∃ i j, y i ≠ y j) →
      ∃ (i j : Fin n) (hij : y i < y j), (∀ l, y i ≤ y l) ∧ (∀ l, y l ≤ y j) ∧
      ∃ (F : ℝ → ℝ) (k ε H : ℝ) (R : Set ℝ),
        AnalyticOnNhd ℝ F (Ici 0) ∧ EqOn F (profile hij) (Ici 0) ∧
        (∃ g ∈ Weibull.domain,
          IsMaxOn (fun p : ℝ × ℝ ↦ gompertzProduct y p.1 p.2) Weibull.domain g ∧
            F 0 = gompertzProduct y g.1 g.2) ∧
        0 < k ∧ IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k ∧
        0 < ε ∧ ε ≤ H ∧ (∀ h ∈ Ioo 0 ε, deriv F h ≠ 0) ∧
        (StrictMonoOn F (Ioo 0 ε) ∨ StrictAntiOn F (Ioo 0 ε)) ∧
        (∀ h > H, F h < thresholdProduct y (y i) k) ∧
        R = Icc ε H ∩ {h | deriv F h = 0} ∧ R.Finite ∧
        let C := max (F 0) (thresholdProduct y (y i) k)
        let W := {h ∈ R | C ≤ F h ∧ ∀ t ∈ R, F t ≤ F h}
        ((∃ p ∈ physicalDomain, IsMaxOn (value y) physicalDomain p) ↔
          ∃ h ∈ R, C ≤ F h) ∧
        BijOn (fit hij) W {p ∈ physicalDomain | IsMaxOn (value y) physicalDomain p}) := by
  constructor
  · intro hconst
    have hb := ((all_free_failure hn y).1 hconst).2
    rintro ⟨p, hp, hm⟩
    apply hb
    refine ⟨value y p, ?_⟩
    intro v hv
    obtain ⟨q, hq, rfl⟩ := hv
    exact hm hq
  · rintro ⟨v, w, hw⟩
    obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ y ⟨v, Finset.mem_univ _⟩
    obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ y ⟨v, Finset.mem_univ _⟩
    have hmin (t : Fin n) := hi t (Finset.mem_univ t)
    have hmax (t : Fin n) := hj t (Finset.mem_univ t)
    have hij : y i < y j := by
      by_contra! h
      exact hw ((le_antisymm (hmax v) (h.trans (hmin v))).trans
        (le_antisymm (hmax w) (h.trans (hmin w))).symm)
    obtain ⟨F, k, ε, H, R, hF, hEq, hk, hkmax, hε, hεH, hcollar, hTail, heR, hR⟩ :=
      candidate_list hij hmin hmax
    obtain ⟨g, hg, hG, hgF⟩ := profile_face hij
    have hface : F 0 = gompertzProduct y g.1 g.2 :=
      (hEq (show 0 ≤ (0 : ℝ) from le_rfl)).trans hgF
    subst R
    exact ⟨i, j, hij, hmin, hmax, F, k, ε, H, _, hF, hEq, ⟨g, hg, hG, hface⟩,
      hk, hkmax, hε, hεH, hcollar, PGWProfile.signed_collar hε hF hcollar, hTail, rfl, hR,
      retained_winners hij hmin hmax hEq hk hkmax hε hcollar hTail⟩


end

end UnivariateML.PGWJointProfile

end
