module

public import UnivariateML.Family.TwoPieceStudent
public import UnivariateML.Family.StudentFaces
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! # Critical collapse at a supporting Student sample extreme -/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TwoPiece2StudentExtreme

open TwoPieceStudent

variable {n : ℕ}

private theorem broadWidth_comparison (x : Fin n → ℝ) (y c ν w : ℝ) {u W : ℝ}
    (hν : 0 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) (hu : 0 < u)
    (hW : w ≤ W) (hW' : 1-w ≤ W) :
    let a := W / (sqrt ν*u)
    let f := logAlongAtom x y ν w ((y-c)*u) u
    let g := TDist.logLik x c ν a + n * log (2*W)
    f ≤ g ∧
    ((∀ i, x i = c ∨ side w ((x i-c)*u) = W) → f = g) := by
  intro a f g
  have hWp : 0 < W := hw.1.trans_le hW
  have hq : 0 < sqrt ν := sqrt_pos.2 hν
  let b := W / (sqrt ν*u)
  have hbs : ν*b^2 = (W/u)^2 := by
    dsimp [b]
    rw [div_pow, mul_pow, sq_sqrt hν.le, div_pow]
    field_simp [hν.ne',hu.ne']
  have hnrm : log b = log W - log ν / 2 - log u := by
    have hlog : log (sqrt ν) = log ν / 2 := log_sqrt hν.le
    dsimp [b]
    rw [log_div hWp.ne' (mul_pos hq hu).ne', log_mul hq.ne' hu.ne', hlog]
    ring
  have hnorm' : (n : ℝ)*(logPeak ν + log u) =
      n * (TDist.logNormalizer ν - log b) + n*log (2*W) := by
    rw [hnrm, log_mul (by norm_num : (2 : ℝ) ≠ 0) hWp.ne']
    unfold logPeak
    ring
  have hk (i : Fin n) :
      log (1+(x i-c)^2 / (ν*b^2)) =
        log (1+(((x i-c)*u)/W)^2) := by
    rw [hbs, div_pow, div_pow]
    congr 1
    field_simp [hWp.ne',hu.ne']
  have he (i : Fin n) : (x i-y)*u+(y-c)*u = (x i-c)*u := by ring
  have hg : TDist.logLik x c ν b + n*log (2*W) =
      (n : ℝ)*(logPeak ν + log u) -
        (ν+1)/2 * ∑ i, log (1+(((x i-c)*u)/W)^2) := by
    unfold TDist.logLik
    simp only [hk, Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    rw [hnorm']
    ring
  have hf : f = (n : ℝ)*(logPeak ν + log u) -
      (ν+1)/2 * ∑ i, log (1+(((x i-c)*u)/side w ((x i-c)*u))^2) := by
    simp only [f, logAlongAtom, he]
  have hK (i : Fin n) :
      log (1+(((x i-c)*u)/W)^2) ≤
        log (1+(((x i-c)*u)/side w ((x i-c)*u))^2) := by
    let r := (x i-c)*u
    let s := side w r
    have hside : 0 < s ∧ s ≤ W := by
      unfold s side
      split
      · exact ⟨hw.1,hW⟩
      · exact ⟨sub_pos.mpr hw.2,hW'⟩
    apply log_le_log (by positivity)
    rw [div_pow, div_pow]
    exact add_le_add_right
      (div_le_div_of_nonneg_left (sq_nonneg _) (pow_pos hside.1 _)
        (pow_le_pow_left₀ hside.1.le hside.2 _)) 1
  have hfac : 0 ≤ (ν+1)/2 := by positivity
  have hle : f ≤ TDist.logLik x c ν b + n * log (2*W) := by
    rw [hf,hg]
    exact sub_le_sub_left (mul_le_mul_of_nonneg_left (Finset.sum_le_sum fun i _ ↦ hK i) hfac) _
  have heq (hs : ∀ i, x i = c ∨ side w ((x i-c)*u) = W) :
      f = TDist.logLik x c ν b + n*log (2*W) := by
    rw [hf,hg]
    congr 2
    apply Finset.sum_congr rfl
    intro i _
    rcases hs i with h | h
    · simp [h]
    · rw [h]
  exact ⟨hle,heq⟩

/-- At a critical sample extreme with fixed skew, if the broad side
contains the sample, the collapse height is the joint supremum over
all modes and positive scales. This includes the symmetric case and
both orientations. -/
theorem criticalExtreme_fixedSkew (hn : 0 < n) (x : Fin n → ℝ) (y ν w : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) (hE : TDist.exponent x y ν = 0)
    (left : Bool)
    (hs : if left then (∀ i, x i ≤ y) ∧ 1-w ≤ w else (∀ i, y ≤ x i) ∧ w ≤ 1-w) :
    let W := if left then w else 1-w
    let C := collapseLogHeight x y ν w 0
    let V := {L : ℝ | ∃ c u : ℝ, 0 < u ∧ L = logAlongAtom x y ν w ((y-c)*u) u}
    C = TDist.collapseLogValue x y ν + n*log (2*W) ∧
    IsLUB V C ∧ ∀ L ∈ V, L < C := by
  intro W C V
  have hw₁ : w ≤ W := by cases left <;> simp_all [W]
  have hw₂ : 1-w ≤ W := by cases left <;> simp_all [W]
  have hW : 0 < W := hw.1.trans_le hw₁
  have hν' : 0 < ν := by linarith
  have hq := sqrt_pos.2 hν'
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hne : ∃ i, x i ≠ y := by
    by_contra! he
    rw [TDist.exponent_all_tied x y ν he] at hE
    linarith
  have hL := ((TDist.heldLocation_equality.{0} x y hν' hne hE).1)
  have ha : Tendsto (fun u : ℝ ↦ W / (sqrt ν*u)) atTop (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_iff.mpr
    refine ⟨?_, ?_⟩
    · have hh := tendsto_inv_atTop_zero.const_mul (W/sqrt ν)
      simpa only [mul_zero, div_eq_mul_inv, mul_inv_rev, mul_assoc, mul_left_comm, mul_comm, zero_mul] using hh
    · exact (eventually_gt_atTop (0 : ℝ)).mono
        (fun _ hu ↦ div_pos hW (mul_pos hq hu))
  have hpath : Tendsto (logAlongAtom x y ν w 0) atTop (𝓝 C) :=
    ((collapse_cases x y ν w 0 hw).2.2 hE).1
  have hmatch :
      Tendsto (logAlongAtom x y ν w 0) atTop
        (𝓝 (TDist.collapseLogValue x y ν + n*log (2*W))) := by
    apply ((hL.comp ha).add_const _).congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with u hu
    have he : ∀ i, x i = y ∨ side w ((x i-y)*u) = W := by
      intro i
      by_cases hi : x i = y
      · exact Or.inl hi
      · right
        cases left
        · simp only [Bool.false_eq] at hs
          have hr := mul_pos (sub_pos.mpr (lt_of_le_of_ne (hs.1 i) (Ne.symm hi))) hu
          simp [side, hr.not_gt, W]
        · simp only [ite_true] at hs
          have hr := mul_neg_of_neg_of_pos (sub_neg.mpr (lt_of_le_of_ne (hs.1 i) hi)) hu
          simp [side, hr, W]
    simpa only [Function.comp_def, sub_self, zero_mul] using
      ((broadWidth_comparison x y y ν w hν' hw hu hw₁ hw₂).2 he).symm
  have hC := tendsto_nhds_unique hpath hmatch
  have hlt (L : ℝ) (hL : L ∈ V) : L < C := by
    obtain ⟨c,u,hu,rfl⟩ := hL
    rw [hC]
    have hb := div_pos hW (mul_pos hq hu)
    have hstrict := (StudentFaces.lst_critical_joint x hn (by linarith : 1 < ν) hE).2 c _ hb
    exact (broadWidth_comparison x y c ν w hν' hw hu hw₁ hw₂).1.trans_lt
      (add_lt_add_left (exp_lt_exp.mp hstrict) _)
  refine ⟨hC, ⟨fun L hL ↦ (hlt L hL).le, ?_⟩, hlt⟩
  intro L hL
  apply le_of_tendsto hpath
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with u hu
  exact hL ⟨y,u,hu,by rw [sub_self, zero_mul]⟩

/-- At held shape and a critical extreme, the unattained all-skew
supremum assigns the limiting width to the side supporting the sample. -/
theorem criticalExtreme_freeSkew (hn : 0 < n) (x : Fin n → ℝ) (y ν : ℝ)
    (hν : 2 < ν) (hE : TDist.exponent x y ν = 0) (left : Bool)
    (hs : if left then ∀ i, x i ≤ y else ∀ i, y ≤ x i) :
    let C := TDist.collapseLogValue x y ν + n * log 2
    let V := {L : ℝ | ∃ c u w : ℝ, 0 < u ∧ w ∈ Ioo 0 1 ∧
      L = logAlongAtom x y ν w ((y-c)*u) u}
    IsLUB V C ∧ ∀ L ∈ V, L < C := by
  intro C V
  have hν' : 0 < ν := by linarith
  have hq := sqrt_pos.2 hν'
  have hlt (L : ℝ) (hL : L ∈ V) : L < C := by
    obtain ⟨c,u,w,hu,hw,rfl⟩ := hL
    have hcomp := (broadWidth_comparison x y c ν w hν' hw hu
      (W := 1) hw.2.le (sub_le_self 1 hw.1.le)).1
    have ha : 0 < 1 / (sqrt ν*u) := one_div_pos.mpr (mul_pos hq hu)
    have hstrict := (StudentFaces.lst_critical_joint x hn (by linarith : 1 < ν) hE).2 c _ ha
    simp only [one_div, mul_one] at *
    exact hcomp.trans_lt (add_lt_add_left (exp_lt_exp.mp hstrict) _)
  refine ⟨⟨fun L hL ↦ (hlt L hL).le, ?_⟩, hlt⟩
  intro L hL
  let A := Ioo (0 : ℝ) (1/2)
  let g (v : ℝ) := TDist.collapseLogValue x y ν + n * log (2*(1-v))
  let : (𝓝[A] (0 : ℝ)).NeBot :=
    mem_closure_iff_nhdsWithin_neBot.mp (by
      change (0 : ℝ) ∈ closure (Ioo (0 : ℝ) (1/2))
      rw [closure_Ioo (by norm_num : (0 : ℝ) ≠ 1/2)]
      norm_num)
  have hcont : ContinuousAt g 0 := by
    unfold g
    apply continuousAt_const.add
    apply continuousAt_const.mul
    apply ContinuousAt.log
    · fun_prop
    · norm_num
  have hlim : Tendsto g (𝓝[A] 0) (𝓝 C) := by
    simpa only [g, C, sub_zero, mul_one] using (hcont.continuousWithinAt (s := A)).tendsto
  apply le_of_tendsto hlim
  filter_upwards [self_mem_nhdsWithin (s := A)] with v hv
  let w := if left then 1-v else v
  have hw : w ∈ Ioo (0 : ℝ) 1 := by
    have hv₁ : 0 < v := hv.1
    have hv₂ : v < 1/2 := hv.2
    cases left
    · exact ⟨hv₁, hv₂.trans (by norm_num)⟩
    · exact ⟨by dsimp [w]; linarith, sub_lt_self _ hv₁⟩
  have hs' : if left then (∀ i, x i ≤ y) ∧ 1-w ≤ w else
      (∀ i, y ≤ x i) ∧ w ≤ 1-w := by
    have hv₂ : v < 1/2 := hv.2
    cases left <;> dsimp [w] at * <;> exact ⟨hs,by linarith⟩
  have hW : (if left then w else 1-w) = 1-v := by cases left <;> simp [w]
  obtain ⟨he, hsup, _⟩ := criticalExtreme_fixedSkew hn x y ν w hν hw hE left hs'
  rw [hW] at he
  change TDist.collapseLogValue x y ν + n * log (2*(1-v)) ≤ L
  rw [← he]
  apply hsup.2
  rintro M ⟨c,u,hu,rfl⟩
  exact hL ⟨c,u,w,hu,hw,rfl⟩

end UnivariateML.TwoPiece2StudentExtreme

end
