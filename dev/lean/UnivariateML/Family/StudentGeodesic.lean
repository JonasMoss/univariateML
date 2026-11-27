module

public import UnivariateML.Family.TDist

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Uniqueness at fixed Student shape

The upper half-plane midpoint is written in coordinates. The two
Busemann midpoint inequalities follow from identities that express
their slack as a square.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.StudentGeodesic

noncomputable section

private def halfPlaneKernel (x m s : ℝ) : ℝ := ((x-m)^2+s^2)/s

private theorem kernel_pos (x m : ℝ) {s : ℝ} (hs : 0 < s) :
    0 < halfPlaneKernel x m s := by
  unfold halfPlaneKernel
  positivity

private theorem log_midpoint {z u v : ℝ} (hz : 0 < z) (hu : 0 < u) (hv : 0 < v) :
    (z^2 ≤ u*v → log z ≤ (log u + log v)/2) ∧
    (z^2 < u*v → log z < (log u + log v)/2) := by
  constructor
  · intro h
    have hh := log_le_log (sq_pos_of_pos hz) h
    rw [log_pow, log_mul hu.ne' hv.ne'] at hh
    push_cast at hh
    linarith
  · intro h
    have hh := log_lt_log (sq_pos_of_pos hz) h
    rw [log_pow, log_mul hu.ne' hv.ne'] at hh
    push_cast at hh
    linarith

private theorem midpoint_bounds {u v a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∃ m s : ℝ, 0 < s ∧
      log s⁻¹ ≤ (log a⁻¹ + log b⁻¹)/2 ∧
      (u ≠ v → log s⁻¹ < (log a⁻¹ + log b⁻¹)/2) ∧
      (∀ x, log (halfPlaneKernel x m s) ≤
        (log (halfPlaneKernel x u a) + log (halfPlaneKernel x v b))/2) ∧
      ∀ x, u = v → a ≠ b → x ≠ u →
        log (halfPlaneKernel x m s) <
          (log (halfPlaneKernel x u a) + log (halfPlaneKernel x v b))/2 := by
  let K := sqrt (((u-v)^2+(a+b)^2)/(a*b))
  have hab : 0 < a+b := by positivity
  have hK : 0 < K := by
    dsimp [K]
    exact sqrt_pos.mpr (div_pos (add_pos_of_nonneg_of_pos (sq_nonneg _) (sq_pos_of_pos hab))
      (mul_pos ha hb))
  have hK2 : K^2 * a * b = (u-v)^2+(a+b)^2 := by
    unfold K
    rw [sq_sqrt (by positivity)]
    field_simp
  let m := (u*b+v*a)/(a+b)
  let s := K*a*b/(a+b)
  have hs : 0 < s := by dsimp [s]; positivity
  have hInf : s⁻¹^2 + (u-v)^2/(K*a*b)^2 = a⁻¹*b⁻¹ := by
    dsimp [s]
    field_simp [ha.ne', hb.ne', hab.ne', hK.ne']
    linear_combination - hK2
  have hI := log_midpoint (inv_pos.mpr hs) (inv_pos.mpr ha) (inv_pos.mpr hb)
  have hKernel (x : ℝ) :
      halfPlaneKernel x m s = (halfPlaneKernel x u a + halfPlaneKernel x v b)/K := by
    dsimp [halfPlaneKernel, m, s]
    field_simp [ha.ne', hb.ne', hab.ne', hK.ne']
    linear_combination a*b*hK2
  have hId (x : ℝ) :
      halfPlaneKernel x m s ^ 2 +
        (((x-u)*((x-v)^2+b^2)-(x-v)*((x-u)^2+a^2))/(K*a*b))^2 =
      halfPlaneKernel x u a * halfPlaneKernel x v b := by
    rw [hKernel]
    unfold halfPlaneKernel
    field_simp [ha.ne', hb.ne', hK.ne']
    linear_combination - ((x-u)^2+a^2)*((x-v)^2+b^2)*hK2
  refine ⟨m, s, hs, hI.1 (by linarith [show 0 ≤ (u-v)^2/(K*a*b)^2 by positivity]),
    ?_, ?_, ?_⟩
  · intro huv
    apply hI.2
    have h : 0 < (u-v)^2/(K*a*b)^2 :=
      div_pos (sq_pos_of_ne_zero (sub_ne_zero.mpr huv)) (by positivity)
    linarith
  · intro x
    apply (log_midpoint (kernel_pos x m hs) (kernel_pos x u ha) (kernel_pos x v hb)).1
    linarith [hId x, sq_nonneg
      (((x-u)*((x-v)^2+b^2)-(x-v)*((x-u)^2+a^2))/(K*a*b))]
  · intro x huv hneq hx
    have hsquares : b^2 ≠ a^2 := by
      intro he
      apply hneq
      exact (sq_eq_sq₀ ha.le hb.le).mp he.symm
    have hd :
        (x-u)*((x-v)^2+b^2)-(x-v)*((x-u)^2+a^2) ≠ 0 := by
      rw [← huv, show (x-u)*((x-u)^2+b^2)-(x-u)*((x-u)^2+a^2) =
        (x-u)*(b^2-a^2) by ring]
      exact mul_ne_zero (sub_ne_zero.mpr hx) (sub_ne_zero.mpr hsquares)
    apply (log_midpoint (kernel_pos x m hs) (kernel_pos x u ha) (kernel_pos x v hb)).2
    linarith [hId x, sq_pos_of_ne_zero (div_ne_zero hd (by positivity : K*a*b ≠ 0))]

private theorem logLik_in_kernel {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    TDist.logLik x m ν (s/sqrt ν) =
      n*(TDist.logNormalizer ν + log (sqrt ν)) -
        (ν+1)/2 * (∑ i, log (halfPlaneKernel (x i) m s)) -
        n*(ν-1)/2 * log s⁻¹ := by
  have hsq : sqrt ν ^ 2 = ν := sq_sqrt hν.le
  have hνs : 0 < sqrt ν := sqrt_pos.mpr hν
  have hScale : ν * (s / sqrt ν)^2 = s^2 := by
    rw [div_pow, hsq]
    field_simp [hν.ne']
  have hAux (r : ℝ) : log (1+(r-m)^2/s^2) = log (halfPlaneKernel r m s) - log s := by
    rw [show 1+(r-m)^2/s^2 = halfPlaneKernel r m s / s by
      unfold halfPlaneKernel; field_simp [hs.ne']; ring,
      log_div (kernel_pos r m hs).ne' hs.ne']
  unfold TDist.logLik
  simp_rw [hScale, hAux, log_div hs.ne' hνs.ne', log_inv]
  simp only [mul_sub, sub_sub_eq_add_sub, Finset.sum_sub_distrib, Finset.sum_add_distrib,
    ← Finset.mul_sum, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  ring

private theorem constant_no_max {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ}
    (hν : 0 < ν) (m : ℝ) (hx : ∀ i, x i = m)
    {s : ℝ} (_hs : 0 < s) :
    ¬ IsMaxOn (fun p : ℝ × ℝ ↦ TDist.logLik x p.1 ν p.2) (univ ×ˢ Ioi 0) (m,s) := by
  have he : TDist.exponent x m ν < 0 := by
    rw [TDist.exponent_all_tied x m ν hx]
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    linarith
  intro h
  have hpos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
  obtain ⟨t, ht, hh⟩ := (hpos.and
    ((TDist.collapse_unbounded x m hν he).eventually_gt_atTop (TDist.likelihood x m ν s))).exists
  exact hh.not_ge (exp_le_exp.mpr (h (show (m,t) ∈ univ ×ˢ Ioi 0 from ⟨mem_univ _, ht⟩)))

/-- With ordinary scale and location free at held df above one, any two
maximizers coincide. The proof compares their hyperbolic midpoint. -/
theorem lst_held_df_unique {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 1 < ν)
    {p q : ℝ × ℝ} (hp : 0 < p.2) (hq : 0 < q.2)
    (hP : IsMaxOn (fun z : ℝ × ℝ ↦ TDist.logLik x z.1 ν z.2) (univ ×ˢ Ioi 0) p)
    (hQ : IsMaxOn (fun z : ℝ × ℝ ↦ TDist.logLik x z.1 ν z.2) (univ ×ˢ Ioi 0) q) :
    p = q := by
  have hv : 0 < ν := by linarith
  have hr : 0 < sqrt ν := sqrt_pos.mpr hv
  let a := p.2 * sqrt ν
  let b := q.2 * sqrt ν
  have ha : 0 < a := mul_pos hp hr
  have hb : 0 < b := mul_pos hq hr
  obtain ⟨m,s,hs,hI,hIs,hK,hKs⟩ := midpoint_bounds (u := p.1) (v := q.1) ha hb
  have hAbove : TDist.logLik x m ν (s/sqrt ν) ≤
      TDist.logLik x p.1 ν p.2 := hP (show (m,s/sqrt ν) ∈ univ ×ˢ Ioi 0 from
        ⟨mem_univ _, div_pos hs hr⟩)
  have hEqual : TDist.logLik x p.1 ν p.2 = TDist.logLik x q.1 ν q.2 :=
    le_antisymm (hQ (show p ∈ univ ×ˢ Ioi 0 from ⟨mem_univ _, hp⟩))
      (hP (show q ∈ univ ×ˢ Ioi 0 from ⟨mem_univ _, hq⟩))
  have hA : a/sqrt ν = p.2 := mul_div_cancel_right₀ _ hr.ne'
  have hB : b/sqrt ν = q.2 := mul_div_cancel_right₀ _ hr.ne'
  have hSum : ∑ i, log (halfPlaneKernel (x i) m s) ≤
      ((∑ i, log (halfPlaneKernel (x i) p.1 a)) +
        (∑ i, log (halfPlaneKernel (x i) q.1 b)))/2 := by
    rw [← Finset.sum_add_distrib, Finset.sum_div]
    exact Finset.sum_le_sum fun i _ ↦ hK (x i)
  have hConclude (h : log s⁻¹ < (log a⁻¹ + log b⁻¹)/2 ∨
      (∑ i, log (halfPlaneKernel (x i) m s)) <
        ((∑ i, log (halfPlaneKernel (x i) p.1 a)) +
          (∑ i, log (halfPlaneKernel (x i) q.1 b)))/2) : False := by
    rw [← hA] at hAbove
    rw [← hA, ← hB] at hEqual
    rw [logLik_in_kernel x m hv hs, logLik_in_kernel x p.1 hv ha] at hAbove
    rw [logLik_in_kernel x p.1 hv ha, logLik_in_kernel x q.1 hv hb] at hEqual
    have hw : 0 < (ν+1)/2 := by positivity
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    have hz : 0 < n*(ν-1)/2 := by positivity
    rcases h with h | h
    · have h₁ := mul_le_mul_of_nonneg_left hSum hw.le
      have h₂ := mul_lt_mul_of_pos_left h hz
      nlinarith only [hAbove, hEqual, h₁, h₂]
    · have h₁ := mul_lt_mul_of_pos_left h hw
      have h₂ := mul_le_mul_of_nonneg_left hI hz.le
      nlinarith only [hAbove, hEqual, h₁, h₂]
  have hloc : p.1 = q.1 := by
    by_contra h
    exact hConclude (Or.inl (hIs h))
  apply Prod.ext hloc
  by_contra hscale
  have hab : a ≠ b := by
    intro he
    apply hscale
    exact mul_right_cancel₀ hr.ne' he
  have hx : ∃ i, x i ≠ p.1 := by
    by_contra! hh
    exact constant_no_max x hn hv p.1 hh hp hP
  obtain ⟨i,hi⟩ := hx
  apply hConclude
  right
  rw [← Finset.sum_add_distrib, Finset.sum_div]
  exact Finset.sum_lt_sum (fun j _ ↦ hK (x j))
    ⟨i, Finset.mem_univ _, hKs (x i) hloc hab hi⟩

/-- At held df above two, positive standard deviation and location have
at most one joint maximizer. -/
theorem std_held_df_unique {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ}
    (hν : 2 < ν) {p q : ℝ × ℝ} (hp : 0 < p.2) (hq : 0 < q.2)
    (hP : IsMaxOn (fun z : ℝ × ℝ ↦ TDist.stdLogLik x z.1 ν z.2) (univ ×ˢ Ioi 0) p)
    (hQ : IsMaxOn (fun z : ℝ × ℝ ↦ TDist.stdLogLik x z.1 ν z.2) (univ ×ˢ Ioi 0) q) :
    p = q := by
  let c := sqrt ((ν-2)/ν)
  have hc : 0 < c := sqrt_pos.mpr (div_pos (by linarith) (by linarith))
  let f : ℝ × ℝ → ℝ × ℝ := fun z ↦ (z.1, z.2*c)
  have hf (z : ℝ × ℝ) (hz : 0 < z.2) :
      IsMaxOn (fun y : ℝ × ℝ ↦ TDist.stdLogLik x y.1 ν y.2) (univ ×ˢ Ioi 0) z →
      IsMaxOn (fun y : ℝ × ℝ ↦ TDist.logLik x y.1 ν y.2) (univ ×ˢ Ioi 0) (f z) := by
    intro hm y hy
    have hys : 0 < y.2/c := div_pos hy.2 hc
    have h := hm (show (y.1, y.2/c) ∈ univ ×ˢ Ioi 0 from ⟨mem_univ _, hys⟩)
    change TDist.stdLogLik x y.1 ν (y.2/c) ≤ TDist.stdLogLik x z.1 ν z.2 at h
    rw [TDist.stdLogLik_eq x y.1 hν hys, TDist.stdLogLik_eq x z.1 hν hz] at h
    change TDist.logLik x y.1 ν (y.2/c*c) ≤ TDist.logLik x z.1 ν (z.2*c) at h
    simpa [f, div_mul_cancel₀ _ hc.ne'] using h
  have h := lst_held_df_unique x hn (by linarith : 1 < ν) (p := f p) (q := f q)
    (mul_pos hp hc) (mul_pos hq hc) (hf p hp hP) (hf q hq hQ)
  have hfst := congrArg Prod.fst h
  have hsnd := congrArg Prod.snd h
  change p.2*c = q.2*c at hsnd
  exact Prod.ext hfst (mul_right_cancel₀ hc.ne' hsnd)

end
end UnivariateML.StudentGeodesic
end
