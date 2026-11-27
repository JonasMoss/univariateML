module

public import UnivariateML.Family.SkewLSHalfT
public import UnivariateML.Generic.FinishProfile
import Mathlib.Tactic

set_option linter.style.header false

/-!
# All-free translated half-t attainment on the fitted shape domain

## Deviations

The profile and face comparison use products of densities. The candidate
condition includes the eligible floor and the interior score zeros. It
does not count the zeros or isolate them numerically.
-/

@[expose] public section

open Set Filter Topology Real
open UnivariateML.SkewLSHalfT

namespace UnivariateML.FinishHalfT

noncomputable section

private def kernel {n : ℕ} (x : Fin n → ℝ) (m ν v : ℝ) : ℝ :=
  ∑ i, (-v - (ν+1)/2 * log (1+(x i-m)^2/(ν*exp v^2)))

private def normalizer (n : ℕ) (ν : ℝ) : ℝ := n * TDist.logNormalizer ν

private theorem kernel_eq {n : ℕ} (x : Fin n → ℝ) (m ν v : ℝ) :
    TDist.logLik x m ν (exp v) = kernel x m ν v + normalizer n ν := by
  unfold TDist.logLik kernel normalizer
  simp only [log_exp, show ∀ a b c : ℝ, a-b-c = (-b-c)+a by intros; ring,
    Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]

private theorem kernel_smooth {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {ν v : ℝ}
    (hν : 0 < ν) : ContDiffAt ℝ 1 (fun p : ℝ × ℝ ↦ kernel x m p.1 p.2) (ν,v) := by
  unfold kernel
  apply ContDiffAt.sum
  intro i _
  have hd : ContDiffAt ℝ 1 (fun p : ℝ × ℝ ↦ p.1 * exp p.2^2) (ν,v) := by fun_prop
  have hp : 0 < ν*exp v^2 := by positivity
  have ha := (contDiffAt_const (c := (x i-m)^2)).div hd hp.ne'
  have hb := (contDiffAt_const (c := (1 : ℝ))).add ha
  have hlog := hb.log (show 1+(x i-m)^2/(ν*exp v^2) ≠ 0 by positivity)
  exact ContDiffAt.sub (by fun_prop) (ContDiffAt.mul (by fun_prop) hlog)

private theorem kernel_derivative {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {ν : ℝ}
    (hν : 0 < ν) (v : ℝ) :
    HasDerivAt (kernel x m ν) (TDist.scaleScore x m ν (exp v)) v := by
  have h := (TDist.hasDerivAt_logLik x m hν v).sub_const (normalizer n ν)
  convert h using 1
  ext u
  rw [kernel_eq, add_sub_cancel_right]

private theorem kernel_concave {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {ν : ℝ}
    (hν : 0 < ν) (hx : ∃ i, x i ≠ m) : ConcaveOn ℝ univ (kernel x m ν) := by
  apply StrictConcaveOn.concaveOn
  apply StrictAnti.strictConcaveOn_univ_of_deriv
  · exact continuous_iff_continuousAt.mpr fun v ↦ (kernel_derivative x m hν v).continuousAt
  · intro a b hab
    rw [(kernel_derivative x m hν a).deriv, (kernel_derivative x m hν b).deriv]
    exact TDist.scaleScore_strictAntiOn x m hν hx (exp_pos _) (exp_pos _) (exp_lt_exp.mpr hab)

private theorem normalizer_differentiable (n : ℕ) {ν : ℝ} (hν : 0 < ν) :
    DifferentiableAt ℝ (normalizer n) ν := by
  have hg {a : ℝ} (ha : 0 < a) : DifferentiableAt ℝ (fun a ↦ log (Gamma a)) a :=
    (differentiableAt_Gamma fun k ↦ ne_of_gt
      ((neg_nonpos.mpr (Nat.cast_nonneg k)).trans_lt ha)).log (Gamma_pos_of_pos ha).ne'
  have ha : DifferentiableAt ℝ (fun a : ℝ ↦ (a+1)/2) ν := by fun_prop
  have hb : DifferentiableAt ℝ (fun a : ℝ ↦ a/2) ν := by fun_prop
  have hc : DifferentiableAt ℝ (fun a : ℝ ↦ log (a*π)/2) ν := by
    apply DifferentiableAt.div_const
    exact (differentiableAt_id.mul_const π).log (mul_pos hν pi_pos).ne'
  have hga : DifferentiableAt ℝ (fun a : ℝ ↦ log (Gamma ((a+1)/2))) ν :=
    (hg (show 0 < (ν + 1) / 2 by positivity)).fun_comp'
      (f := fun a : ℝ ↦ (a + 1) / 2) (g := fun a ↦ log (Gamma a)) ν ha
  have hgb : DifferentiableAt ℝ (fun a : ℝ ↦ log (Gamma (a/2))) ν :=
    (hg (half_pos hν)).fun_comp' (f := fun a : ℝ ↦ a/2) (g := fun a ↦ log (Gamma a)) ν hb
  unfold normalizer TDist.logNormalizer
  exact (differentiableAt_const (c := (n : ℝ))).mul ((hga.sub hgb).sub hc)

private theorem likelihood_eq {n : ℕ} (x : Fin n → ℝ) (m ν s : ℝ) :
    likelihood x m ν s = exp (TDist.logLik x m ν s + n * log 2) := by
  rw [likelihood, TDist.likelihood, exp_add, exp_nat_mul, exp_log (by norm_num : (0 : ℝ) < 2)]
  ring

private theorem scale_profile_differentiable {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    (hx : ∃ i, x i ≠ m) {ν : ℝ} (hν : 0 < ν) (he : 0 < TDist.exponent x m ν) :
    DifferentiableAt ℝ (scaleProfile x m) ν := by
  have hchoose (a : ℝ) : ∃ v : ℝ, 0 < a → 0 < TDist.exponent x m a →
      IsMaxOn (kernel x m a) univ v ∧
        (∀ u, u ≠ v → kernel x m a u < kernel x m a v) ∧
        scaleProfile x m a = exp (kernel x m a v + normalizer n a + n*log 2) := by
    by_cases h : 0 < a ∧ 0 < TDist.exponent x m a
    · obtain ⟨s,⟨hs,hzero⟩,hu⟩ := ((TDist.heldLocation_maximum x m h.1 hx).2 h.2).1
      have hsmax := (((TDist.heldLocation_maximum x m h.1 hx).2 h.2).2 s hs).mpr hzero
      have htrans (u : ℝ) : TDist.logLik x m a (exp u) = kernel x m a u + normalizer n a :=
        kernel_eq x m a u
      have hm : IsMaxOn (kernel x m a) univ (log s) := by
        intro u _
        have hh := hsmax (show exp u ∈ Ioi 0 from exp_pos u)
        change TDist.logLik x m a (exp u) ≤ TDist.logLik x m a s at hh
        rw [← exp_log hs, htrans, htrans] at hh
        exact le_of_add_le_add_right hh
      refine ⟨log s, fun _ _ ↦ ⟨hm, ?_, ?_⟩⟩
      · intro u hne
        apply lt_of_le_of_ne (hm (mem_univ u))
        intro heq
        have hu' : IsMaxOn (kernel x m a) univ u := fun t ht ↦ (hm ht).trans_eq heq.symm
        have hz := (hu'.isLocalMax univ_mem).deriv_eq_zero
        rw [(kernel_derivative x m h.1 u).deriv] at hz
        exact hne (exp_injective ((hu _ ⟨exp_pos u,hz⟩).trans (exp_log hs).symm))
      · have hL : IsMaxOn (likelihood x m a) (Ioi 0) s := by
          intro u hu
          change likelihood x m a u ≤ likelihood x m a s
          rw [likelihood_eq, likelihood_eq]
          exact exp_le_exp.mpr (add_le_add_left (hsmax hu) _)
        have hmax : IsLUB (likelihood x m a '' Ioi 0) (likelihood x m a s) := hL.isLUB hs
        rw [scaleProfile, hmax.csSup_eq (Nonempty.image _ ⟨s,hs⟩)]
        conv_lhs => rw [← exp_log hs]
        rw [likelihood_eq, htrans]
    · exact ⟨0, fun ha hE ↦ (h ⟨ha,hE⟩).elim⟩
  choose q hq using hchoose
  have hne : ∀ᶠ a in 𝓝 ν, 0 < a ∧ 0 < TDist.exponent x m a := by
    have hc : Continuous (TDist.exponent x m) := by unfold TDist.exponent; fun_prop
    exact (eventually_gt_nhds hν).and (hc.continuousAt.tendsto.eventually (eventually_gt_nhds he))
  have hq0 := hq ν hν he
  have hlim : Tendsto q (𝓝 ν) (𝓝 (q ν)) :=
    FinishProfile.tendsto_optimizer isOpen_univ (mem_univ (q ν))
      (fun y _ ↦ by rw [← nhds_prod_eq]; exact (kernel_smooth x m hν (v := y)).continuousAt)
      (fun y _ ↦ hq0.2.1 y)
      (hne.mono fun a ha ↦ kernel_concave x m ha.1 hx)
      (hne.mono fun a ha ↦ ⟨mem_univ _, (hq a ha.1 ha.2).1 (mem_univ _)⟩)
  let P (a : ℝ) := kernel x m a (q a)
  have hP : DifferentiableAt ℝ P ν :=
    (FinishProfile.hasDerivAt_profile
      ((kernel_smooth x m hν (v := q ν)).hasStrictFDerivAt_fderiv one_ne_zero)
      hlim rfl (hne.mono fun a ha ↦
        ⟨(hq a ha.1 ha.2).1 (mem_univ _), rfl, hq0.1 (mem_univ _)⟩)).differentiableAt
  have hd := ((hP.add (normalizer_differentiable n hν)).add
    (differentiableAt_const (c := (n : ℝ)*log 2))).exp
  exact hd.congr_of_eventuallyEq (hne.mono fun a ha ↦ (hq a ha.1 ha.2).2.2)

/-- The fitted shape floor and the compatible translated support. -/
def domain {n : ℕ} (x : Fin n → ℝ) : Set (ℝ × (ℝ × ℝ)) :=
  {p | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2.1 ∧ 0 < p.2.2}

private theorem nonconstant_of_count {n : ℕ} {x : Fin n → ℝ} {i : Fin n}
    (h : 2 * TDist.multiplicity x (x i) ≤ n) :
    ∃ j, x j ≠ x i := by
  by_contra! hc
  have he : TDist.multiplicity x (x i) = n := by simp [TDist.multiplicity, hc]
  have hn : 0 < n := i.isLt.pos
  omega

private theorem eligible_or_collapse {n : ℕ} {x : Fin n → ℝ} {i : Fin n}
    (h : 2 * TDist.multiplicity x (x i) ≤ n) {ν : ℝ} (hν : 1 ≤ ν) :
    0 < TDist.exponent x (x i) ν ∨ ν = 1 ∧ TDist.exponent x (x i) 1 = 0 := by
  let k : ℝ := TDist.multiplicity x (x i)
  have hk : 2*k ≤ n := by dsimp [k]; exact_mod_cast h
  have hn : (0 : ℝ) < n := by exact_mod_cast i.isLt.pos
  have hn' : 0 < (n : ℝ)-k := by linarith
  rcases eq_or_lt_of_le hν with hν | hν
  · subst ν
    have hE : 0 ≤ TDist.exponent x (x i) 1 := by
      change 0 ≤ 1*((n : ℝ)-k)-k
      linarith
    exact hE.eq_or_lt.elim (fun he ↦ Or.inr ⟨rfl, he.symm⟩) Or.inl
  · left
    change 0 < ν*((n : ℝ)-k)-k
    linarith only [hk, mul_lt_mul_of_pos_right hν hn']

private theorem profile_bound {n : ℕ} {x : Fin n → ℝ} {i : Fin n}
    (h : 2 * TDist.multiplicity x (x i) ≤ n) {ν : ℝ} (hν : 1 ≤ ν) :
    IsLUB (likelihood x (x i) ν '' Ioi 0) (scaleProfile x (x i) ν) := by
  have hne := Nonempty.image (likelihood x (x i) ν) ((nonempty_Ioi : (Ioi (0 : ℝ)).Nonempty))
  refine isLUB_csSup hne ?_
  rcases eligible_or_collapse h hν with he | ⟨rfl,he⟩
  · obtain ⟨s,⟨hs,hm⟩,_⟩ := (held_endpoint i.isLt.pos x (x i) (zero_lt_one.trans_le hν)).mpr he
    refine ⟨likelihood x (x i) ν s, ?_⟩
    rintro _ ⟨u,hu,rfl⟩
    rw [likelihood_eq, likelihood_eq]
    exact exp_le_exp.mpr (add_le_add_left (le_of_add_le_add_right (hm hu)) _)
  · exact ⟨_, (held_endpoint_equality i.isLt.pos x (x i) zero_lt_one he).2.1.1⟩

private theorem profile_bounded {n : ℕ} {x : Fin n → ℝ} {i : Fin n}
    (h : 2 * TDist.multiplicity x (x i) ≤ n) :
    BddAbove (scaleProfile x (x i) '' Ici 1) := by
  have hx := nonconstant_of_count h
  have hcont : ContinuousOn (scaleProfile x (x i)) (Ici 1) := by
    intro ν hν
    rcases eligible_or_collapse h hν with he | ⟨rfl,he⟩
    · exact (scale_profile_continuous x (x i) hx
        (zero_lt_one.trans_le hν) he).continuousWithinAt
    · apply continuousWithinAt_Ioi_iff_Ici.mp
      change Tendsto (scaleProfile x (x i)) (𝓝[>] (1 : ℝ))
        (𝓝 (scaleProfile x (x i) 1))
      have hv := (held_endpoint_equality i.isLt.pos x (x i) zero_lt_one he).2.1
      have heq : scaleProfile x (x i) 1 = collapseValue x (x i) 1 :=
        hv.csSup_eq (Nonempty.image _ (nonempty_Ioi : (Ioi (0 : ℝ)).Nonempty))
      rw [heq]
      exact profiled_half_tie_limit x (x i) hx he
  have ht := profiled_half_normal_limit x (x i) hx
  obtain ⟨R,hR⟩ := eventually_atTop.mp
    (ht.eventually (eventually_lt_nhds (lt_add_one (normalFace x (x i)))))
  obtain ⟨B,hB⟩ := ((isCompact_Icc (a := (1 : ℝ)) (b := R)).image_of_continuousOn
    (hcont.mono (fun _ hy ↦ hy.1))).bddAbove
  refine ⟨max B (normalFace x (x i)+1), ?_⟩
  rintro _ ⟨ν,hν,rfl⟩
  rcases le_total ν R with ht | ht
  · exact (hB (mem_image_of_mem _ ⟨hν,ht⟩)).trans (le_max_left _ _)
  · exact (hR ν ht).le.trans (le_max_right _ _)

/-- Strict majority at the minimum is exactly the unbounded branch. -/
theorem all_free_unbounded_iff {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) :
    (¬ BddAbove ((fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2) '' domain x)) ↔
      n < 2 * TDist.multiplicity x (x i) := by
  refine ⟨fun h ↦ ?_, joint_majority_unbounded x hi⟩
  by_contra! hc
  obtain ⟨B,hB⟩ := profile_bounded hc
  apply h
  refine ⟨B,?_⟩
  rintro _ ⟨⟨μ,ν,s⟩,⟨hμ,hν,hs⟩,rfl⟩
  have hupper : B ∈ upperBounds (likelihood x (x i) ν '' Ioi 0) :=
    fun _ hu ↦ ((profile_bound hc hν).1 hu).trans
      (hB (mem_image_of_mem _ hν))
  rw [← free_endpoint_profile_upperBounds x (zero_lt_one.trans_le hν) hi] at hupper
  exact hupper ⟨(μ,s), ⟨hμ,hs⟩, rfl⟩

/-- Finite attainment requires an eligible floor or a stationary interior
shape reaching the half-normal and scale-zero face values. -/
theorem all_free_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) :
    (∃ p ∈ domain x, IsMaxOn (fun p ↦ likelihood x p.1 p.2.1 p.2.2) (domain x) p) ↔
      2 * TDist.multiplicity x (x i) ≤ n ∧
      ∃ ν ≥ (1 : ℝ), 0 < TDist.exponent x (x i) ν ∧
        (ν = 1 ∨ HasDerivAt (scaleProfile x (x i)) 0 ν) ∧
        max (normalFace x (x i)) (collapseFace x (x i)) ≤ scaleProfile x (x i) ν := by
  constructor
  · rintro ⟨⟨μ,ν,s⟩,⟨hμ,hν,hs⟩,hm⟩
    have hb := hm.bddAbove
    have hc : 2 * TDist.multiplicity x (x i) ≤ n :=
      le_of_not_gt fun h ↦ ((all_free_unbounded_iff x hi).mpr h) hb
    have hx := nonconstant_of_count hc
    have hv : likelihood x (x i) ν s = likelihood x μ ν s := by
      apply le_antisymm (isMaxOn_iff.mp hm (x i,ν,s) ⟨hi,hν,hs⟩)
      rw [likelihood_eq, likelihood_eq, exp_le_exp, add_le_add_iff_right]
      unfold TDist.logLik
      apply Finset.sum_le_sum
      intro j _
      have hsq : (x j-x i)^2 ≤ (x j-μ)^2 := by
        have hf : 0 ≤ x j-x i := sub_nonneg.mpr (hi j)
        exact pow_le_pow_left₀ hf (by linarith [hμ i]) _
      have hν' := zero_lt_one.trans_le hν
      have hd : 0 < ν*s^2 := by positivity
      have hp : 0 < 1+(x j-x i)^2/(ν*s^2) := by positivity
      have hl := log_le_log hp (add_le_add_right (div_le_div_of_nonneg_right hsq hd.le) 1)
      have hh := mul_le_mul_of_nonneg_left hl (show 0 ≤ (ν+1)/2 by positivity)
      linarith
    have hm' : IsMaxOn (fun p ↦ likelihood x p.1 p.2.1 p.2.2) (domain x) (x i,ν,s) := by
      intro p hp
      exact (hm hp).trans_eq hv.symm
    have hscale : IsMaxOn (TDist.logLik x (x i) ν) (Ioi 0) s := by
      intro a ha
      have he := isMaxOn_iff.mp hm' (x i,ν,a) ⟨hi,hν,ha⟩
      change likelihood x (x i) ν a ≤ likelihood x (x i) ν s at he
      rw [likelihood_eq, likelihood_eq, exp_le_exp, add_le_add_iff_right] at he
      exact he
    have he := (TDist.heldLocation_maximum x (x i) (zero_lt_one.trans_le hν) hx).1.mp
      ⟨s,hs,hscale⟩
    have htrace (u : ℝ) (hu : 1 ≤ u) : scaleProfile x (x i) u ≤ likelihood x (x i) ν s :=
      (profile_bound hc hu).2 (by
        rintro _ ⟨a,ha,rfl⟩
        exact isMaxOn_iff.mp hm' (x i,u,a) ⟨hi,hu,ha⟩)
    have hval : scaleProfile x (x i) ν = likelihood x (x i) ν s :=
      le_antisymm (htrace ν hν) ((profile_bound hc hν).1 (mem_image_of_mem _ hs))
    refine ⟨hc,ν,hν,he,?_,?_⟩
    · rcases eq_or_lt_of_le hν with hn | hn
      · exact Or.inl hn.symm
      · have hlocal : IsLocalMax (scaleProfile x (x i)) ν :=
          Filter.mem_of_superset (Ici_mem_nhds hn) (fun u hu ↦ (htrace u hu).trans_eq hval.symm)
        exact Or.inr (hlocal.deriv_eq_zero ▸
          (scale_profile_differentiable x (x i) hx (zero_lt_one.trans hn) he).hasDerivAt)
    · rw [hval]
      apply max_le
      · exact le_of_tendsto (profiled_half_normal_limit x (x i) hx)
          ((eventually_ge_atTop 1).mono htrace)
      · by_cases h : TDist.exponent x (x i) 1 = 0
        · have hv := (held_endpoint_equality i.isLt.pos x (x i) zero_lt_one h).2.1
          have ht := (profile_bound hc (ν := 1) le_rfl).2 hv.1
          have hl := hv.2 (profile_bound hc (ν := 1) le_rfl).1
          simpa [collapseFace, h, le_antisymm ht hl] using htrace 1 le_rfl
        · simpa [collapseFace,h] using (show 0 ≤ likelihood x (x i) ν s by
            rw [likelihood_eq]; positivity)
  · rintro ⟨hc,ν,hν,he,_,hw⟩
    exact (joint_attainment_criterion x hi hc).mpr ⟨ν,hν,he,hw⟩


/-- In the bounded branch with no finite fit, the larger excluded face
is the exact likelihood supremum. -/
theorem unattained_face_supremum {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) (hc : 2 * TDist.multiplicity x (x i) ≤ n)
    (h : ¬ ∃ p ∈ domain x,
      IsMaxOn (fun p ↦ likelihood x p.1 p.2.1 p.2.2) (domain x) p) :
    IsLUB ((fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2) '' domain x)
      (max (normalFace x (x i)) (collapseFace x (x i))) := by
  let H := normalFace x (x i)
  let C := collapseFace x (x i)
  let F := max H C
  have hfloor (he : TDist.exponent x (x i) 1 = 0) : scaleProfile x (x i) 1 = C := by
    rw [show C = collapseValue x (x i) 1 by simp [C,collapseFace,he]]
    have hv := (held_endpoint_equality i.isLt.pos x (x i) zero_lt_one he).2.1
    exact hv.csSup_eq (Nonempty.image _ (nonempty_Ioi : (Ioi (0 : ℝ)).Nonempty))
  have hx := nonconstant_of_count hc
  constructor
  · rintro _ ⟨⟨μ,ν,s⟩,⟨hμ,hν,hs⟩,rfl⟩
    have hp : scaleProfile x (x i) ν ≤ F := by
      rcases eligible_or_collapse hc hν with he | ⟨rfl,he⟩
      · apply le_of_not_gt
        intro hb
        exact h ((joint_attainment_criterion x hi hc).mpr ⟨ν,hν,he,hb.le⟩)
      · exact (hfloor he).trans_le (le_max_right _ _)
    have hupper : F ∈ upperBounds (likelihood x (x i) ν '' Ioi 0) :=
      fun _ hu ↦ ((profile_bound hc hν).1 hu).trans hp
    rw [← free_endpoint_profile_upperBounds x (zero_lt_one.trans_le hν) hi] at hupper
    exact hupper ⟨(μ,s),⟨hμ,hs⟩,rfl⟩
  · intro B hB
    have htrace (ν : ℝ) (hν : 1 ≤ ν) : scaleProfile x (x i) ν ≤ B := by
      apply (profile_bound hc hν).2
      rintro _ ⟨s,hs,rfl⟩
      exact hB ⟨(x i,ν,s),⟨hi,hν,hs⟩,rfl⟩
    refine max_le (le_of_tendsto (profiled_half_normal_limit x (x i) hx)
      ((eventually_ge_atTop 1).mono htrace)) ?_
    by_cases he : TDist.exponent x (x i) 1 = 0
    · exact (hfloor he).symm.trans_le (htrace 1 le_rfl)
    · have h : 0 ≤ likelihood x (x i) 1 1 := by rw [likelihood_eq]; positivity
      simpa [C,collapseFace,he] using h.trans (hB ⟨(x i,1,1),⟨hi,le_rfl,zero_lt_one⟩,rfl⟩)

end

end UnivariateML.FinishHalfT
