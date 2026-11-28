module

public import UnivariateML.Family.Trunc3CMP
public import UnivariateML.Family.Trunc3CMPRay
public import Mathlib.Analysis.Convex.Extrema

/-! Finite CMP windows at held rate. The zero-rate branch is represented
by its exact compatibility test, and the positive-rate branch uses signed
dispersion `t = -ν`. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.Trunc5CMP

noncomputable section

private theorem factorial_mono : Monotone CMP.logFactorial := by
  intro a b hab
  apply log_le_log (by exact_mod_cast Nat.factorial_pos a)
  exact_mod_cast Nat.factorial_le hab

private theorem carrier_interior {l u : ℕ} (hlu : l ≤ u)
    (h : CMP.logFactorial l < CMP.logFactorial u) (η : ℝ) :
    Truncation.carrierMean (Finset.Icc l u) CMP.logFactorial (Trunc3CMP.rateCarrier η) ∈
      Ioo (CMP.logFactorial l) (CMP.logFactorial u) := by
  let A := Finset.Icc l u
  let T := CMP.logFactorial
  let w := Trunc3CMP.rateCarrier η
  have hl : l ∈ A := Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩
  have hu : u ∈ A := Finset.mem_Icc.mpr ⟨hlu,le_rfl⟩
  have hp : 0 < ∑ k ∈ A, w k := Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨l,hl⟩
  change T l < (∑ k ∈ A, w k*T k)/(∑ k ∈ A, w k) ∧
    (∑ k ∈ A, w k*T k)/(∑ k ∈ A, w k) < T u
  rw [lt_div_iff₀ hp, div_lt_iff₀ hp, Finset.mul_sum, Finset.mul_sum]
  constructor
  · apply Finset.sum_lt_sum
    · intro k hk
      rw [mul_comm (T l)]
      exact mul_le_mul_of_nonneg_left (factorial_mono (Finset.mem_Icc.mp hk).1) (exp_pos _).le
    · exact ⟨u,hu,by rw [mul_comm (T l)]; exact mul_lt_mul_of_pos_left h (exp_pos _)⟩
  · apply Finset.sum_lt_sum
    · intro k hk
      rw [mul_comm (T u)]
      exact mul_le_mul_of_nonneg_left (factorial_mono (Finset.mem_Icc.mp hk).2) (exp_pos _).le
    · exact ⟨l,hl,by rw [mul_comm (T u)]; exact mul_lt_mul_of_pos_left h (exp_pos _)⟩

private theorem finite_concave {l u : ℕ} (hlu : l ≤ u) (η m : ℝ) :
    ConcaveOn ℝ univ (Truncation.logLik (Finset.Icc l u) CMP.logFactorial
      (Trunc3CMP.rateCarrier η) m) := by
  have H := Truncation.convex_partition (Finset.Icc l u) CMP.logFactorial
    (Trunc3CMP.rateCarrier η) ⟨l,Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩ (fun _ _ ↦ exp_pos _)
  refine ⟨convex_univ, fun x hx y hy a b ha hb hab ↦ ?_⟩
  have hi := H.2 hx hy ha hb hab
  simp only [smul_eq_mul] at hi ⊢
  simp only [Truncation.logLik, RCLike.inner_apply, conj_trivial]
  linarith

private theorem minimum_law {l u : ℕ} (hlu : l ≤ u) (η : ℝ) (j : ℕ)
    (hj : j ∈ Finset.Icc l u) :
    let A := Finset.Icc l u
    let T := CMP.logFactorial
    let h := Trunc3CMP.rateCarrier η
    let F := A.filter (fun k ↦ T k = T l)
    Tendsto (fun t ↦ h j * exp (t*T j) / ∑ k ∈ A, h k * exp (t*T k))
      atBot (𝓝 ((if j ∈ F then h j else 0) / ∑ k ∈ F, h k)) := by
  classical
  intro A T h F
  let g (k : ℕ) := if k ∈ F then h k else 0
  let w (t : ℝ) (k : ℕ) := h k * exp (t*(T k-T l))
  have hlim (k : ℕ) (hk : k ∈ A) :
      Tendsto (fun t ↦ w t k) atBot (𝓝 (g k)) := by
    by_cases he : T k = T l
    · have hm : k ∈ F := Finset.mem_filter.mpr ⟨hk,he⟩
      simp [w,g,hm,he]
    · have hm : k ∉ F := fun h ↦ he (Finset.mem_filter.mp h).2
      have hlt : T l < T k := lt_of_le_of_ne (factorial_mono (Finset.mem_Icc.mp hk).1) (Ne.symm he)
      have H := (tendsto_exp_atBot.comp
        (tendsto_id.atBot_mul_const (sub_pos.mpr hlt))).const_mul (h k)
      simpa [w,g,hm,Function.comp_def] using H
  have hZ := tendsto_finsetSum A hlim
  have hsum : (∑ k ∈ A, g k) = ∑ k ∈ F, h k := by
    rw [Finset.sum_filter]
    apply Finset.sum_congr rfl
    intro k hk
    simp [g,F,hk]
  rw [hsum] at hZ
  have hpos : 0 < ∑ k ∈ F, h k :=
    Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨l,Finset.mem_filter.mpr
      ⟨Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩,rfl⟩⟩
  refine ((hlim j hj).div hZ hpos.ne').congr (fun t ↦ ?_)
  have he (k : ℕ) : w t k = exp (-t*T l) * (h k * exp (t*T k)) := by
    dsimp [w]
    rw [mul_left_comm, ← exp_add]
    congr 2
    ring
  change w t j / (∑ k ∈ A, w t k) = _
  simp only [he, ← Finset.mul_sum]
  rw [mul_div_mul_left _ _ (exp_pos _).ne']

/-- CMP, lambda held, `truncate:known`, criterion, failure and uniqueness
on a finite retained window. The atomic branch has exactly the compatible
zero-rate fiber. In the positive-rate branch a flat window admits every
dispersion in the parent chart, the lower extreme has no optimum, the
open interval below the geometric mean has one interior optimum, and the
geometric threshold and above admit only the zero-dispersion point if
the parent allows it. The last two limits describe the geometric law and
the cells in the minimum-statistic support. -/
theorem held_rate_finite_window {l u n : ℕ} (hlu : l ≤ u) (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∀ i, x i ∈ Finset.Icc l u) (η : ℝ) :
    let A := Finset.Icc l u
    let T := CMP.logFactorial
    let h := Trunc3CMP.rateCarrier η
    let m := (∑ i, T (x i)) / n
    let g := Truncation.carrierMean A T h
    let L := Truncation.logLik A T h m
    let D := Trunc3CMPRay.domain η
    ((0 < ∑ k ∈ A, (if k = 0 then (1 : ℝ) else 0)) ∧
      (∀ i, 0 < (if x i = 0 then (1 : ℝ) else 0)) ↔ l = 0 ∧ ∀ i, x i = 0) ∧
    (T l = T u → ∀ t ∈ D, IsMaxOn L D t) ∧
    (T l < T u →
      ((∃! t, t ∈ D ∧ IsMaxOn L D t) ↔ T l < m ∧ (m < g ∨ η < 0)) ∧
      (m = T l → ∀ t ∈ D, ¬ IsMaxOn L D t) ∧
      (m < g → ∀ t ∈ D, IsMaxOn L D t → t < 0) ∧
      (g ≤ m → ∀ t ∈ D, IsMaxOn L D t ↔ t = 0 ∧ η < 0)) ∧
    (∀ j, Tendsto (fun t : ℝ ↦ h j * exp (t*T j) /
      ∑ k ∈ A, h k * exp (t*T k)) (𝓝[<] 0) (𝓝 (h j / ∑ k ∈ A, h k))) ∧
    (∀ k ∈ A, T k = T l ↔ k = l ∨ (l = 0 ∧ k = 1)) ∧
    (∀ j ∈ A, let F := A.filter (fun k ↦ T k = T l)
      Tendsto (fun t ↦ h j * exp (t*T j) / ∑ k ∈ A, h k * exp (t*T k))
        atBot (𝓝 ((if j ∈ F then h j else 0) / ∑ k ∈ F, h k))) := by
  intro A T h m g L D
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  have hbound : m ∈ Icc (T l) (T u) := by
    change T l ≤ (∑ i, T (x i))/n ∧ (∑ i, T (x i))/n ≤ T u
    rw [le_div_iff₀ hN, div_le_iff₀ hN]
    simp only [← nsmul_eq_mul']
    constructor
    · simpa using Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦
        factorial_mono (Finset.mem_Icc.mp (hx i)).1)
    · simpa using Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦
        factorial_mono (Finset.mem_Icc.mp (hx i)).2)
  refine ⟨Trunc3CMP.zero_rate_compatibility x, ?_, ?_,
    fun j ↦ Trunc3CMP.held_rate_geometric_limit (j := j) hlu η,
    fun _ hk ↦ Trunc3CMP.minimum_factorial_cells hk, minimum_law hlu η⟩
  · intro he t _ s _
    have hm : m = T l := le_antisymm (hbound.2.trans_eq he.symm) hbound.1
    have hc (k : ℕ) (hk : k ∈ A) : T k = m :=
      le_antisymm ((factorial_mono (Finset.mem_Icc.mp hk).2).trans_eq (he.symm.trans hm.symm))
        (hm ▸ factorial_mono (Finset.mem_Icc.mp hk).1)
    have H := Truncation.logLik_shift A T h (s-t) t m
      ⟨l,Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩ (fun _ _ ↦ exp_pos _)
      (fun k hk ↦ by rw [hc k hk])
    rw [add_sub_cancel] at H
    exact H.le
  · intro hlt
    have hg : g ∈ Ioo (T l) (T u) := carrier_interior hlu hlt η
    have hmin : m = T l → ∀ t ∈ D, ¬ IsMaxOn L D t := by
      intro he
      simpa only [L,m,he,D,Trunc3CMPRay.domain,T,h,A] using
        Trunc3CMP.held_rate_minimum_failure hlu η hlt
    have hedge : g ≤ m → ∀ t ∈ D, IsMaxOn L D t ↔ t = 0 ∧ η < 0 :=
      Trunc3CMP.held_rate_geometric hlu η hlt
    have hincl : Iio (0 : ℝ) ⊆ D := by
      intro t ht
      unfold D Trunc3CMPRay.domain
      split_ifs
      · exact ht.le
      · exact ht
    have hWin (hm₁ : T l < m) (hm₂ : m < g) :
        ∃! t, t ∈ D ∧ IsMaxOn L D t := by
      have H := (Trunc3CMP.held_rate_interior hlu η hlt
        ⟨hm₁,hm₂.trans hg.2⟩).mpr hm₂
      obtain ⟨t,⟨ht,hmax⟩,_⟩ := H
      have hlocal : IsLocalMax L t := hmax.isLocalMax (Iio_mem_nhds ht)
      have hglobal : IsMaxOn L univ t :=
        IsMaxOn.of_isLocalMaxOn_of_concaveOn (mem_univ t) (hlocal.isLocalMaxOn univ) (finite_concave hlu η m)
      refine ⟨t,⟨hincl ht,fun s _ ↦ hglobal (mem_univ s)⟩,?_⟩
      rintro s ⟨hs,hsmax⟩
      have he : L s = L t := le_antisymm (hglobal (mem_univ s)) (hsmax (hincl ht))
      have hother : IsMaxOn L univ s := fun q _ ↦ (hglobal (mem_univ q)).trans_eq he.symm
      have Hb := Truncation.maximizers_constant_direction A T h
        ⟨l,Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩ (fun _ _ ↦ exp_pos _) hother hglobal
        l (Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩) u (Finset.mem_Icc.mpr ⟨hlu,le_rfl⟩)
      change T l*(s-t) = T u*(s-t) at Hb
      have Hzero : (T u - T l)*(s-t) = 0 := by linarith
      exact sub_eq_zero.mp ((mul_eq_zero.mp Hzero).resolve_left (sub_ne_zero.mpr hlt.ne'))
    refine ⟨?_,hmin,?_,hedge⟩
    · constructor
      · rintro ⟨t,⟨ht,htmax⟩,_⟩
        have hm₁ := lt_of_le_of_ne hbound.1 (fun he ↦ hmin he.symm t ht htmax)
        refine ⟨hm₁,?_⟩
        by_cases hg' : m < g
        · exact Or.inl hg'
        · exact Or.inr ((hedge (le_of_not_gt hg') t ht).mp htmax).2
      · rintro ⟨hm₁,hm₂ | hη⟩
        · exact hWin hm₁ hm₂
        · rcases lt_or_ge m g with hm₂ | hm₂
          · exact hWin hm₁ hm₂
          · have hz : 0 ∈ D := by simp [D,Trunc3CMPRay.domain,hη]
            exact ⟨0,⟨hz,(hedge hm₂ 0 hz).mpr ⟨rfl,hη⟩⟩,
              fun t ht ↦ ((hedge hm₂ t ht.1).mp ht.2).1⟩
    · intro hm₂ t ht htmax
      have hm₁ := lt_of_le_of_ne hbound.1 (fun he ↦ hmin he.symm t ht htmax)
      obtain ⟨s,⟨hs,_⟩,huniq⟩ := hWin hm₁ hm₂
      have H := (Trunc3CMP.held_rate_interior hlu η hlt ⟨hm₁,hm₂.trans hg.2⟩).mpr hm₂
      obtain ⟨v,⟨hv,hvmax⟩,_⟩ := H
      have hvglobal : IsMaxOn L univ v :=
        IsMaxOn.of_isLocalMaxOn_of_concaveOn (mem_univ v)
          ((hvmax.isLocalMax (Iio_mem_nhds hv)).isLocalMaxOn univ) (finite_concave hlu η m)
      have hv' := huniq v ⟨hincl hv, fun q _ ↦ hvglobal (mem_univ q)⟩
      exact (huniq t ⟨ht,htmax⟩).trans hv'.symm ▸ hv

/-- Finite-window inverse likelihood per observation, expressed as a
centered partition in signed-dispersion coordinates. -/
def finiteJointPartition (l u : ℕ) (m f : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ k ∈ Finset.Icc l u, exp (p.1*((k : ℝ)-m)+p.2*(CMP.logFactorial k-f))

/-- CMP, all free, `truncate:known`, criterion and failure for the
mean-matched geometric candidate. The inward factorial score decides
whether this chart gives the best height. A negative log rate admits
the candidate. At a nonnegative log rate the same height is approached
from negative signed dispersion. On three or more cells every other
finite point is strictly worse when the geometric score passes. -/
theorem finite_geometric_comparison {l u : ℕ} (hlu : l+2 ≤ u) (m f η : ℝ)
    (hmean : ∑ k ∈ Finset.Icc l u, exp (η*((k:ℝ)-m)) * ((k:ℝ)-m) = 0) :
    let Z := finiteJointPartition l u m f
    let S := ∑ k ∈ Finset.Icc l u, exp (η*((k:ℝ)-m)) * (CMP.logFactorial k-f)
    let D := {p : ℝ × ℝ | p.2 < 0 ∨ p.2 = 0 ∧ p.1 < 0}
    (IsMinOn Z {p | p.2 ≤ 0} (η,0) ↔ S ≤ 0) ∧
    (S ≤ 0 → (∀ q : ℝ × ℝ, q.2 ≤ 0 → q ≠ (η,0) → Z (η,0) < Z q) ∧
      IsGLB (Z '' D) (Z (η,0)) ∧
      (∀ q ∈ D, IsMinOn Z D q ↔ q = (η,0) ∧ η < 0)) := by
  intro Z S D
  let A := Finset.Icc l u
  let w (k : ℕ) := exp (η*((k:ℝ)-m))
  let Δ (q : ℝ × ℝ) (k : ℕ) :=
    (q.1-η)*((k:ℝ)-m)+q.2*(CMP.logFactorial k-f)
  have hrepr (q : ℝ × ℝ) : Z q = ∑ k ∈ A, w k * exp (Δ q k) := by
    apply Finset.sum_congr rfl
    intro k _
    rw [← exp_add]
    unfold Δ
    congr 1
    ring
  have hbase : Z (η,0) = ∑ k ∈ A, w k := by
    rw [hrepr]
    simp [Δ]
  have hlin (q : ℝ × ℝ) : (∑ k ∈ A, w k * (Δ q k + 1)) = Z (η,0) + q.2*S := by
    have he (k : ℕ) : w k * (Δ q k+1) = w k + (q.1-η)*(w k*((k:ℝ)-m)) +
        q.2*(w k*(CMP.logFactorial k-f)) := by unfold Δ; ring
    simp only [he,Finset.sum_add_distrib, ← Finset.mul_sum, ← hbase]
    change _ + (q.1-η)*(∑ k ∈ A, exp (η*((k:ℝ)-m))*((k:ℝ)-m)) + q.2*S = _
    rw [hmean,mul_zero,add_zero]
  have hid (q : ℝ × ℝ) (hq : q ≠ (η,0)) : ∃ k ∈ A, Δ q k ≠ 0 := by
    by_contra! H
    have h₀ := H l (Finset.mem_Icc.mpr ⟨le_rfl,by omega⟩)
    have h₁ := H (l+1) (Finset.mem_Icc.mpr ⟨by omega,by omega⟩)
    have h₂ := H (l+2) (Finset.mem_Icc.mpr ⟨by omega,hlu⟩)
    have hs₁ : CMP.logFactorial (l+1) = CMP.logFactorial l + log ((l:ℝ)+1) := by
      simp only [CMP.logFactorial,Nat.factorial_succ,Nat.cast_mul,Nat.cast_succ]
      rw [log_mul (by positivity) (by positivity), add_comm]
    have hs₂ : CMP.logFactorial (l+2) = CMP.logFactorial (l+1) + log ((l:ℝ)+2) := by
      change CMP.logFactorial (l+1+1) = _
      simp only [CMP.logFactorial,Nat.factorial_succ (l+1),Nat.cast_mul,Nat.cast_succ]
      rw [log_mul (by positivity) (by positivity),add_comm]
      norm_num [add_assoc]
    simp only [Δ,Nat.cast_add,Nat.cast_one,Nat.cast_ofNat,hs₁,hs₂] at h₀ h₁ h₂
    have hz : q.2 * (log ((l:ℝ)+2)-log ((l:ℝ)+1)) = 0 := by
      linear_combination h₂-2*h₁+h₀
    have hlog : log ((l:ℝ)+1) < log ((l:ℝ)+2) :=
      log_lt_log (by positivity) (by linarith)
    have hv := (mul_eq_zero.mp hz).resolve_right (sub_ne_zero.mpr hlog.ne')
    have hη : q.1 = η := by rw [hv] at h₀ h₁; linarith only [h₀,h₁]
    exact hq (Prod.ext hη hv)
  have hlt (hS : S ≤ 0) (q : ℝ × ℝ) (hq : q.2 ≤ 0) (hne : q ≠ (η,0)) :
      Z (η,0) < Z q := by
    obtain ⟨k,hk,hΔ⟩ := hid q hne
    have H : (∑ k ∈ A, w k * (Δ q k+1)) < ∑ k ∈ A, w k * exp (Δ q k) :=
      Finset.sum_lt_sum
        (fun j _ ↦ mul_le_mul_of_nonneg_left (add_one_le_exp _) (exp_pos _).le)
        ⟨k,hk,mul_lt_mul_of_pos_left (add_one_lt_exp hΔ) (exp_pos _)⟩
    rw [hlin,← hrepr] at H
    exact (le_add_of_nonneg_right (mul_nonneg_of_nonpos_of_nonpos hq hS)).trans_lt H
  have hweak (hS : S ≤ 0) (q : ℝ × ℝ) (hq : q.2 ≤ 0) : Z (η,0) ≤ Z q := by
    by_cases he : q = (η,0)
    · rw [he]
    · exact (hlt hS q hq he).le
  have hderiv : HasDerivAt (fun t ↦ Z (η,t)) S 0 := by
    unfold Z finiteJointPartition
    apply HasDerivAt.fun_sum
    intro k _
    convert (((hasDerivAt_id (0 : ℝ)).mul_const (CMP.logFactorial k-f)).const_add
      (η*((k:ℝ)-m))).exp using 1 <;>
    simp
  constructor
  · constructor
    · intro H
      apply le_of_tendsto hderiv.tendsto_slope_zero_left
      filter_upwards [show ∀ᶠ t : ℝ in 𝓝[<] 0, t < 0 from self_mem_nhdsWithin] with t ht
      simpa only [smul_eq_mul, zero_add, div_eq_mul_inv, mul_comm] using
        div_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr (H (a := (η,t)) ht.le)) ht.le
    · exact fun hS q hq ↦ hweak hS q hq
  · intro hS
    have hdom {p : ℝ × ℝ} (hp : p ∈ D) : p.2 ≤ 0 :=
      hp.elim (fun h ↦ h.le) (fun h ↦ h.1.le)
    have hstep : Tendsto (fun t ↦ Z (η,t)) (𝓝[<] 0) (𝓝 (Z (η,0))) :=
      hderiv.continuousAt.tendsto.mono_left nhdsWithin_le_nhds
    have hglb : IsGLB (Z '' D) (Z (η,0)) := by
      refine ⟨?_,?_⟩
      · rintro _ ⟨q,hq,rfl⟩
        exact hweak hS q (hdom hq)
      · intro r hr
        apply ge_of_tendsto hstep
        filter_upwards [show ∀ᶠ t : ℝ in 𝓝[<] 0, t < 0 from self_mem_nhdsWithin] with t ht
        exact hr ⟨(η,t),Or.inl ht,rfl⟩
    refine ⟨hlt hS,hglb,fun q hq ↦ ?_⟩
    constructor
    · intro H
      have he : q = (η,0) := by
        by_contra he
        exact (hlt hS q (hdom hq) he).not_ge
          (hglb.2 (by rintro _ ⟨v,hv,rfl⟩; exact H hv))
      refine ⟨he,?_⟩
      subst q
      exact hq.elim (fun h ↦ (lt_irrefl 0 h).elim) And.right
    · rintro ⟨rfl,_⟩ v hv
      exact hweak hS v (hdom hv)

end

end UnivariateML.Trunc5CMP

end
