module

public import UnivariateML.Generic.UniqueAE
public import UnivariateML.Family.LocScaleHuber

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Huber with held location and scale

The spatial derivative is continuous through both Huber joins. A finite
winning cutoff has a strict tail, which distinguishes it from every larger
cutoff at the same location and scale.

## Deviations

The theorems compare attained finite cutoffs on freely varying level charts.
The normal and Laplace faces and the open-set interval construction are separate.
-/

@[expose] public section

open Set Filter MeasureTheory Real ContinuousLinearMap
open UnivariateML.Huber UnivariateML.LocScaleHuber

namespace UnivariateML.AE1Huber

noncomputable section

variable {n : ℕ}

/-- Weighted normalized Huber likelihood at a fixed location and scale. -/
def logLik (w : Fin n → ℕ) (m s c : ℝ) (x : Fin n → ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (-log (s * normalizer c) - rho c ((x i - m) / s))

private def sampleDeriv (w : Fin n → ℕ) (m s c : ℝ) (x : Fin n → ℝ) :
    (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ i, ((w i : ℝ) * (-clip c ((x i - m) / s) / s)) • proj i

private theorem rho_derivative {c : ℝ} (hc : 0 ≤ c) (z : ℝ) :
    HasDerivAt (rho c) (clip c z) z := by
  have hl (v : ℝ) : clip c v ∈ Icc (-c) c :=
    ⟨le_max_left _ _, max_le (by linarith) (min_le_left _ _)⟩
  have support {a : ℝ} (ha : a ∈ Icc (-c) c) (t : ℝ) :
      a * t - a ^ 2 / 2 + (a - clip c t)^2 / 2 ≤ rho c t := by
    have ha₁ := ha.1
    have ha₂ := ha.2
    unfold rho clip
    rcases le_or_gt t (-c) with h | h
    · rw [min_eq_right (show t ≤ c by linarith), max_eq_left h]
      nlinarith [mul_nonneg (show 0 ≤ a + c by linarith) (show 0 ≤ -c - t by linarith)]
    · rcases le_or_gt c t with h' | h'
      · rw [min_eq_left h', max_eq_right (by linarith : -c ≤ c)]
        nlinarith [mul_nonneg (sub_nonneg.mpr ha₂) (sub_nonneg.mpr h')]
      · rw [min_eq_right h'.le, max_eq_right h.le]
        ring_nf
        exact le_rfl
  rw [hasDerivAt_iff_isLittleO]
  apply Asymptotics.IsBigO.trans_isLittleO _ (Asymptotics.isLittleO_pow_sub_sub z
    (show 1 < 2 by decide))
  refine Asymptotics.IsBigO.of_norm_le (fun t ↦ ?_)
  simp only [smul_eq_mul, Real.norm_eq_abs]
  have h₁ := support (hl z) t
  have h₂ := support (hl t) z
  have hnonneg : 0 ≤ rho c t - rho c z - (t - z) * clip c z := by
    unfold rho at *
    nlinarith [sq_nonneg (clip c z - clip c t)]
  rw [abs_of_nonneg hnonneg, sq_abs]
  unfold rho at *
  nlinarith [sq_nonneg (t - z - (clip c t - clip c z)), sq_nonneg (t - z)]

private theorem normalizer_positive {c : ℝ} (hc : 0 < c) : 0 < normalizer c := by
  unfold normalizer
  have hi : 0 ≤ ∫ t in (0 : ℝ)..c, exp (-(t^2/2)) :=
    intervalIntegral.integral_nonneg_of_forall hc.le (fun t ↦ (exp_pos _).le)
  positivity

private theorem normalizer_continuous : ContinuousOn normalizer (Ioi 0) := by
  let f (t : ℝ) := exp (-(t^2/2))
  have hf : Continuous f := by unfold f; fun_prop
  have hi : Continuous (fun c : ℝ ↦ ∫ t in (0 : ℝ)..c, f t) :=
    intervalIntegral.continuous_primitive (fun a b ↦ hf.intervalIntegrable a b) 0
  exact (hi.continuousOn.const_mul 2).add ((hf.continuousOn.const_mul 2).div
    continuousOn_id (fun _ hc ↦ (mem_Ioi.mp hc).ne'))

private theorem cutoff_derivative (w : Fin n → ℕ) (m : ℝ) {s c : ℝ}
    (hs : 0 < s) (hc : 0 < c) (x : Fin n → ℝ) :
    HasDerivAt (fun t ↦ logLik w m s t x)
      (∑ i, (w i : ℝ) * (tailShare c / c - max (|(x i - m) / s| - c) 0)) c := by
  unfold logLik
  apply HasDerivAt.fun_sum
  intro i _
  have h := held_location_scale_hasDerivAt (n := 1) (fun _ ↦ x i) m hs hc
  simpa [logLikelihood] using h.const_mul (w i : ℝ)

private theorem strict_tail (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 0 < n)
    (m : ℝ) {s c : ℝ} (hs : 0 < s) (hc : 0 < c) (x : Fin n → ℝ)
    (hmax : IsMaxOn (fun t ↦ logLik w m s t x) (Ioi 0) c) :
    ∃ i, c < |(x i - m) / s| := by
  by_contra! ht
  have hlocal := hmax.isLocalMax (Ioi_mem_nhds hc)
  have hz := hlocal.hasDerivAt_eq_zero (cutoff_derivative w m hs hc x)
  have hp : 0 < tailShare c / c := by
    have := normalizer_positive hc
    unfold tailShare
    positivity
  have he : ∀ i : Fin n, 0 < (w i : ℝ) * (tailShare c / c - max (|(x i - m) / s| - c) 0) := by
    intro i
    rw [max_eq_right (sub_nonpos.mpr (ht i)), sub_zero]
    exact mul_pos (by exact_mod_cast hw i) hp
  let _ : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  have hpos := Finset.sum_pos (fun i _ ↦ he i) (Finset.univ_nonempty (α := Fin n))
  exact hpos.ne' hz

private theorem clip_ne_of_tail {c d z : ℝ} (hc : 0 < c) (hd : c < d)
    (hz : c < |z|) : clip c z ≠ clip d z := by
  unfold clip
  rcases lt_or_ge z 0 with hs | hs
  · rw [abs_of_neg hs] at hz
    have h₁ : min c z = z := min_eq_right (by linarith)
    have h₂ : min d z = z := min_eq_right (by linarith)
    rw [h₁, h₂, max_eq_left (by linarith : z ≤ -c)]
    exact (max_lt (by linarith) (by linarith)).ne'
  · rw [abs_of_nonneg hs] at hz
    rw [min_eq_left hz.le, max_eq_right (by linarith : -c ≤ c)]
    have h : c < min d z := lt_min hd hz
    rw [max_eq_right (by linarith : -d ≤ min d z)]
    exact h.ne

private theorem chart {P : Type*} [TopologicalSpace P] [LocallyCompactSpace P]
    [SecondCountableTopology P] (w : Fin n → ℕ) {s : ℝ} (hs : 0 < s)
    (C M : P → ℝ) (hC : Continuous C) (hM : Continuous M)
    (A : Set P) (hA : IsOpen A) (hcA : ∀ p ∈ A, 0 < C p) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ A, ∀ q ∈ A,
      IsMaxOn (fun r ↦ logLik w (M r) s (C r) x) A p →
      IsMaxOn (fun r ↦ logLik w (M r) s (C r) x) A q →
      sampleDeriv w (M p) s (C p) x = sampleDeriv w (M q) s (C q) x := by
  let S : Set (P × (Fin n → ℝ)) := A ×ˢ univ
  have hZ := (normalizer_continuous.comp (hC.comp continuous_fst).continuousOn
    (fun (z : P × (Fin n → ℝ)) (hz : z ∈ S) ↦ hcA z.1 hz.1)).const_mul s
  have hlog := hZ.log (fun z hz ↦ (mul_pos hs (normalizer_positive (hcA z.1 hz.1))).ne')
  have hL : ContinuousOn (fun z : P × (Fin n → ℝ) ↦ logLik w (M z.1) s (C z.1) z.2) S := by
    unfold logLik
    apply continuousOn_finsetSum Finset.univ
    intro i _
    have hR : Continuous (fun z : P × (Fin n → ℝ) ↦
        rho (C z.1) ((z.2 i - M z.1) / s)) := by
      unfold rho clip
      fun_prop
    exact (hlog.neg.sub hR.continuousOn).const_mul _
  have hD : Continuous (fun z : P × (Fin n → ℝ) ↦ sampleDeriv w (M z.1) s (C z.1) z.2) := by
    unfold sampleDeriv
    apply continuous_finsetSum Finset.univ
    intro i _
    unfold clip
    fun_prop
  have hd (p : P) (x : Fin n → ℝ) (hx : (p, x) ∈ S) :
      HasFDerivAt (logLik w (M p) s (C p)) (sampleDeriv w (M p) s (C p) x) x := by
    unfold logLik sampleDeriv
    apply HasFDerivAt.fun_sum
    intro i _
    let f : (Fin n → ℝ) →L[ℝ] ℝ := proj i
    have hc : 0 < C p := hcA p hx.1
    have h := (rho_derivative hc.le ((x i - M p) / s)).comp (x i)
      (((hasDerivAt_id (x i)).sub_const (M p)).div_const s)
    have h' := ((h.const_sub (-log (s * normalizer (C p)))).const_mul (w i : ℝ))
      |>.comp_hasFDerivAt x (f.hasFDerivAt (x := x))
    simpa only [Function.comp_apply, Function.comp_def, id_eq, f, proj_apply,
      mul_one_div, neg_div] using h'
  filter_upwards [UniqueAE.ae_winners_derivative (μ := volume) (S := S)
    (hA.prod isOpen_univ) hL hD.continuousOn hd] with x hx c hc d hd' hmc hmd
  exact hx c d ⟨⟨hc, mem_univ _⟩, fun _ ht ↦ hmc ht.1⟩
    ⟨⟨hd', mem_univ _⟩, fun _ ht ↦ hmd ht.1⟩

private theorem clips_eq (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) {s : ℝ} (hs : 0 < s)
    {m v c d : ℝ} {x : Fin n → ℝ}
    (he : sampleDeriv w m s c x = sampleDeriv w v s d x) (i : Fin n) :
    clip c ((x i - m) / s) = clip d ((x i - v) / s) := by
  have eval (m' r : ℝ) :
      sampleDeriv w m' s r x (Pi.single i 1) = (w i : ℝ) * (-clip r ((x i - m') / s) / s) := by
    simp only [sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
    rw [Finset.sum_eq_single i]
    · simp
    · intro j _ hj
      simp [Pi.single_eq_of_ne hj]
    · simp
  have h := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single i 1)) he
  rw [eval, eval] at h
  have hw' : (w i : ℝ) ≠ 0 := by exact_mod_cast (hw i).ne'
  exact neg_inj.mp ((div_left_inj₀ hs.ne').mp (mul_left_cancel₀ hw' h))

/-- At held location and scale, finite Huber cutoffs are almost everywhere
unique on every nonempty fixed-count level chart. -/
theorem held_mu_sigma_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 0 < n)
    (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ c d : ℝ, 0 < c → 0 < d →
      IsMaxOn (fun t ↦ logLik w m s t x) (Ioi 0) c →
      IsMaxOn (fun t ↦ logLik w m s t x) (Ioi 0) d → c = d := by
  have hchart := chart w hs id (fun _ : ℝ ↦ m) continuous_id continuous_const
    (Ioi 0) isOpen_Ioi (fun _ hp ↦ hp)
  filter_upwards [hchart] with x hx c d hc hd hmc hmd
  have he := hx c hc d hd hmc hmd
  have sep (a b : ℝ) (ha : 0 < a) (hab : a < b)
      (hma : IsMaxOn (fun t ↦ logLik w m s t x) (Ioi 0) a)
      (he : sampleDeriv w m s a x = sampleDeriv w m s b x) : False := by
    obtain ⟨i, hi⟩ := strict_tail w hw hn m hs ha x hma
    exact clip_ne_of_tail ha hab hi (clips_eq w hw hs he i)
  exact le_antisymm (le_of_not_gt fun h ↦ sep d c hd h hmd he.symm)
    (le_of_not_gt fun h ↦ sep c d hc h hmc he)

private theorem clip_bounds {c : ℝ} (hc : 0 < c) (z : ℝ) :
    -c ≤ clip c z ∧ clip c z ≤ c :=
  ⟨le_max_left _ _, max_le (by linarith) (min_le_left _ _)⟩

private theorem strict_clip {c z : ℝ} (hl : -c < clip c z) (hr : clip c z < c) :
    clip c z = z := by
  have h₁ : min c z < c := (le_max_right _ _).trans_lt hr
  have h₂ : -c < min c z := by
    by_contra! h
    rw [clip, max_eq_left h] at hl
    exact (lt_irrefl _ hl)
  unfold clip
  rcases min_choice c z with h | h
  · rw [h] at h₁
    exact (lt_irrefl _ h₁).elim
  · rw [h] at h₂ ⊢
    exact max_eq_right h₂.le

private theorem cutoff_separation (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 3 ≤ n)
    {m v s c d : ℝ} (hs : 0 < s) (hc : 0 < c) (hd : c < d)
    {x : Fin n → ℝ} (hx : Function.Injective x)
    (hmc : IsMaxOn (fun a ↦ logLik w m s a x) (Ioi 0) c)
    (he : ∀ i, clip c ((x i - m) / s) = clip d ((x i - v) / s)) : False := by
  have he' (i : Fin n) : clip c ((x i - m) / s) = (x i - v) / s := by
    have h := clip_bounds hc ((x i - m) / s)
    rw [he i] at h
    exact (he i).trans (strict_clip (by linarith [h.1]) (by linarith [h.2]))
  by_cases hcore : ∃ i, -c < clip c ((x i - m) / s) ∧ clip c ((x i - m) / s) < c
  · obtain ⟨i, hi, hj⟩ := hcore
    have hh := (strict_clip hi hj).symm.trans (he' i)
    have hmv : m = v := by
      have := (div_left_inj₀ hs.ne').mp hh
      linarith
    subst v
    obtain ⟨i, hi⟩ := strict_tail w hw (by omega) m hs hc x hmc
    exact clip_ne_of_tail hc hd hi (he i)
  · push Not at hcore
    have hsign (i : Fin n) : (x i - v) / s = -c ∨ (x i - v) / s = c := by
      rw [← he' i]
      rcases le_or_gt (clip c ((x i - m) / s)) (-c) with h | h
      · exact Or.inl (h.antisymm (clip_bounds hc _).1)
      · exact Or.inr ((clip_bounds hc _).2.antisymm (hcore i h))
    let i : Fin n := ⟨0, by omega⟩
    let j : Fin n := ⟨1, by omega⟩
    let k : Fin n := ⟨2, by omega⟩
    have hij : i ≠ j := by simp [i, j]
    have hik : i ≠ k := by simp [i, k]
    have hjk : j ≠ k := by simp [j, k]
    have hne (a b : Fin n) (hab : a ≠ b) : (x a - v) / s ≠ (x b - v) / s := by
      intro h
      have := (div_left_inj₀ hs.ne').mp h
      exact hab (hx (by linarith))
    rcases hsign i with ha | ha <;> rcases hsign j with hb | hb <;>
      rcases hsign k with hh | hh <;> first
      | exact hne i j hij (ha.trans hb.symm)
      | exact hne i k hik (ha.trans hh.symm)
      | exact hne j k hjk (hb.trans hh.symm)

/-- The held-scale chart uses pairs (cutoff, location). -/
def scaleDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ

/-- A finite joint winner in the held-scale chart. -/
def scaleWinner (w : Fin n → ℕ) (s : ℝ) (x : Fin n → ℝ) (p : ℝ × ℝ) : Prop :=
  p ∈ scaleDomain ∧ IsMaxOn (fun r : ℝ × ℝ ↦ logLik w r.2 s r.1 x) scaleDomain p

private theorem scale_scores_agree (w : Fin n → ℕ) {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : ℝ × ℝ,
      scaleWinner w s x p → scaleWinner w s x q →
      sampleDeriv w p.2 s p.1 x = sampleDeriv w q.2 s q.1 x := by
  have hchart := chart (P := ℝ × ℝ) w hs Prod.fst Prod.snd continuous_fst continuous_snd
    scaleDomain (isOpen_Ioi.prod isOpen_univ) (fun _ hp ↦ hp.1)
  filter_upwards [hchart] with x hx p q hp hq
  exact hx p hp.1 q hq.1 hp.2 hq.2

private theorem fixed_center_max {w : Fin n → ℕ} {s : ℝ} {x : Fin n → ℝ}
    {p : ℝ × ℝ} (hp : scaleWinner w s x p) :
    IsMaxOn (fun r ↦ logLik w p.2 s r x) (Ioi 0) p.1 := by
  intro c hc
  have h : (c, p.2) ∈ scaleDomain := ⟨hc, mem_univ p.2⟩
  have hmax := (isMaxOn_iff.mp hp.2) (c, p.2) h
  change logLik w p.2 s c x ≤ logLik w p.2 s p.1 x
  exact hmax

private theorem scale_separation {w : Fin n → ℕ} (hw : ∀ i, 0 < w i) (hn : 3 ≤ n)
    {s : ℝ} (hs : 0 < s) {x : Fin n → ℝ} (hx : Function.Injective x)
    {p q : ℝ × ℝ} (hp : scaleWinner w s x p) (hq : scaleWinner w s x q)
    (he : sampleDeriv w p.2 s p.1 x = sampleDeriv w q.2 s q.1 x) : p.1 = q.1 := by
  have hcp : 0 < p.1 := hp.1.1
  have hcq : 0 < q.1 := hq.1.1
  by_contra hne
  rcases lt_or_gt_of_ne hne with h | h
  · exact cutoff_separation w hw hn hs hcp h hx (fixed_center_max hp) (clips_eq w hw hs he)
  · exact cutoff_separation w hw hn hs hcq h hx (fixed_center_max hq) (clips_eq w hw hs he.symm)

/-- At held scale, three distinct levels almost everywhere determine
a unique winning cutoff. There can be several locations at that cutoff. -/
theorem held_sigma_ae_unique_cutoff (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 3 ≤ n)
    {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p q : ℝ × ℝ, scaleWinner w s x p → scaleWinner w s x q → p.1 = q.1 := by
  filter_upwards [scale_scores_agree w hs] with x hxe hx p q hp hq
  exact scale_separation hw hn hs hx hp hq (hxe p q hp hq)

end
end UnivariateML.AE1Huber

end
