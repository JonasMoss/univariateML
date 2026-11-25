module

public import UnivariateML.Generic.Escape
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Location–scale likelihoods with convex negative log densities

The inverse-scale coordinates are `(a,b) = (1/σ, μ/σ)`. Strict concavity
uses only a nonempty sample. Distinct observations are needed for the
coercivity argument, which uses a positive linear lower bound on the loss.
The loss can omit an additive normalization constant.

## Deviations

The loss is finite everywhere and the linear lower bound is an explicit
hypothesis. Density normalization plays no role in the maximization.
-/

@[expose] public section

open Set Filter Real

namespace UnivariateML.LocationScale

noncomputable section

variable {n : ℕ}

def inverseDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ

def domain : Set (ℝ × ℝ) := univ ×ˢ Ioi 0

def inverseLogLik (g : ℝ → ℝ) (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, (log p.1 - g (p.1 * x i - p.2))

def logLik (g : ℝ → ℝ) (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, (-log p.2 - g ((x i - p.1) / p.2))

def Nonconstant (x : Fin n → ℝ) : Prop := ∃ i j, x i ≠ x j

def LinearLowerBound (g : ℝ → ℝ) : Prop :=
  ∃ c > 0, ∃ d, ∀ t, c * |t| - d ≤ g t

private theorem convex_inverseDomain : Convex ℝ inverseDomain :=
  (convex_Ioi 0).prod convex_univ

private theorem mix_residual (p q : ℝ × ℝ) (u v z : ℝ) :
    (u • p + v • q).1 * z - (u • p + v • q).2 =
      u • (p.1 * z - p.2) + v • (q.1 * z - q.2) := by
  simp [smul_eq_mul]
  ring

/-- Each observation contributes a concave function in inverse-scale coordinates. -/
theorem concave_inverseLogLik {g : ℝ → ℝ} (hg : ConvexOn ℝ univ g)
    (x : Fin n → ℝ) : ConcaveOn ℝ inverseDomain (inverseLogLik g x) := by
  refine ⟨convex_inverseDomain, ?_⟩
  intro p hp q hq u v hu hv huv
  have hl := strictConcaveOn_log_Ioi.concaveOn.2 hp.1 hq.1 hu hv huv
  simp only [inverseLogLik, smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  apply Finset.sum_le_sum
  intro i _
  have hg' := hg.2 (mem_univ (p.1 * x i - p.2)) (mem_univ (q.1 * x i - q.2)) hu hv huv
  rw [mix_residual]
  change u * log p.1 + v * log q.1 ≤ log (u • p + v • q).1 at hl
  simp only [smul_eq_mul] at hg'
  simp only [smul_eq_mul]
  linarith

/-- A strictly convex loss gives strict concavity for every nonempty sample. -/
theorem strictConcave_inverseLogLik {g : ℝ → ℝ} (hg : StrictConvexOn ℝ univ g)
    (x : Fin n → ℝ) (hn : 0 < n) :
    StrictConcaveOn ℝ inverseDomain (inverseLogLik g x) := by
  refine ⟨convex_inverseDomain, ?_⟩
  intro p hp q hq hpq u v hu hv huv
  simp only [inverseLogLik, smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  have hterm (i : Fin n) :
      u * (log p.1 - g (p.1 * x i - p.2)) + v * (log q.1 - g (q.1 * x i - q.2)) <
        log (u • p + v • q).1 - g ((u • p + v • q).1 * x i - (u • p + v • q).2) := by
    rw [mix_residual]
    simp only [smul_eq_mul]
    by_cases ha : p.1 = q.1
    · have hb : p.2 ≠ q.2 := fun hb ↦ hpq (Prod.ext ha hb)
      have hr : p.1 * x i - p.2 ≠ q.1 * x i - q.2 := by
        rw [ha]
        exact fun h ↦ hb (by linarith)
      have hl := strictConcaveOn_log_Ioi.concaveOn.2 hp.1 hq.1 hu.le hv.le huv
      have hstrict := hg.2 (mem_univ _) (mem_univ _) hr hu hv huv
      change u * log p.1 + v * log q.1 ≤ log (u • p + v • q).1 at hl
      simp only [smul_eq_mul] at hstrict
      linarith
    · have hl := strictConcaveOn_log_Ioi.2 hp.1 hq.1 ha hu hv huv
      have hweak := hg.convexOn.2 (mem_univ (p.1 * x i - p.2))
        (mem_univ (q.1 * x i - q.2)) hu.le hv.le huv
      change u * log p.1 + v * log q.1 < log (u • p + v • q).1 at hl
      simp only [smul_eq_mul] at hweak
      linarith
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  exact Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty (fun i _ ↦ hterm i)

private theorem inverseDomain_nonempty : inverseDomain.Nonempty :=
  ⟨(1, 0), ⟨zero_lt_one, mem_univ _⟩⟩

private theorem inverseDomain_open : IsOpen inverseDomain :=
  isOpen_Ioi.prod isOpen_univ

/-- Nonconstant data and a positive linear lower bound force the likelihood
to minus infinity along every escape from compact subsets. -/
theorem tendsto_inverseLogLik_cocompact {g : ℝ → ℝ} (hg : LinearLowerBound g)
    {x : Fin n → ℝ} (hx : Nonconstant x) :
    Tendsto (fun p : inverseDomain ↦ inverseLogLik g x p) (cocompact inverseDomain) atBot := by
  obtain ⟨c, hc, d, hg⟩ := hg
  obtain ⟨i, j, hij⟩ := hx
  have hn : (0 : ℝ) < n := by exact_mod_cast (lt_of_le_of_lt (Nat.zero_le i.val) i.isLt)
  let δ := |x i - x j|
  have hδ : 0 < δ := abs_pos.mpr (sub_ne_zero.mpr hij)
  let k := c * δ / (2 * n)
  have hk : 0 < k := div_pos (mul_pos hc hδ) (by positivity)
  have hk_eq : (n : ℝ) * k = c * δ / 2 := by
    dsimp [k]
    field_simp
  -- A superlevel set fits inside this rectangle in the positive-scale chart.
  apply tendsto_atBot.mpr
  intro level
  let A := (n * d - n * log k - n - level) / (c * δ / 2)
  let B := (n * (A - 1) + n * d - level) / c
  let lo := exp ((level - n * d) / n)
  let hi := A * |x i| + B
  let box := Icc lo A ×ˢ Icc (-hi) hi
  have hlo : 0 < lo := exp_pos _
  have hbox : box ⊆ inverseDomain :=
    fun _ hp ↦ ⟨hlo.trans_le hp.1.1, mem_univ _⟩
  have hcompact : IsCompact (Subtype.val ⁻¹' box : Set inverseDomain) :=
    Topology.IsInducing.subtypeVal.isCompact_preimage'
      (isCompact_Icc.prod isCompact_Icc : IsCompact box) (by simpa using hbox)
  refine (Filter.mem_cocompact.mpr ⟨_, hcompact, ?_⟩)
  intro p hp
  change inverseLogLik g x p ≤ level
  by_contra! hlevel
  change (p : ℝ × ℝ) ∉ box at hp
  apply hp
  let a := (p : ℝ × ℝ).1
  let b := (p : ℝ × ℝ).2
  have ha : 0 < a := p.property.1
  let r (l : Fin n) := a * x l - b
  let S := ∑ l, |r l|
  have hS : 0 ≤ S := Finset.sum_nonneg (fun _ _ ↦ abs_nonneg _)
  have hiS : |r i| ≤ S :=
    Finset.single_le_sum (f := fun l ↦ |r l|) (fun _ _ ↦ abs_nonneg _) (Finset.mem_univ i)
  have hpair : |r i| + |r j| ≤ S := by
    have hne : i ≠ j := fun h ↦ hij (congrArg x h)
    have hj : |r j| ≤ ∑ l ∈ Finset.univ.erase i, |r l| :=
      Finset.single_le_sum (f := fun l ↦ |r l|) (fun _ _ ↦ abs_nonneg _)
        (Finset.mem_erase.mpr ⟨hne.symm, Finset.mem_univ _⟩)
    have he := Finset.sum_erase_add (s := Finset.univ) (f := fun l ↦ |r l|)
      (Finset.mem_univ i)
    change _ = S at he
    linarith
  have hspread : a * δ ≤ S := by
    have he : a * (x i - x j) = r i - r j := by dsimp [r]; ring
    calc
      a * δ = |r i - r j| := by rw [← he, abs_mul, abs_of_pos ha]
      _ ≤ |r i| + |r j| := abs_sub _ _
      _ ≤ S := hpair
  have hupper : inverseLogLik g x p ≤ n * log a + n * d - c * S := by
    calc
      _ ≤ ∑ l : Fin n, (log a + d - c * |r l|) :=
        Finset.sum_le_sum (fun l _ ↦ by
          change log a - g (r l) ≤ _
          linarith [hg (r l)])
      _ = _ := by simp [S, Finset.sum_sub_distrib, Finset.mul_sum]
  have hlog : log a ≤ k * a - 1 - log k := by
    have h := log_le_sub_one_of_pos (mul_pos hk ha)
    rw [log_mul hk.ne' ha.ne'] at h
    linarith
  have haA : a ≤ A := by
    apply (le_div_iff₀ (show 0 < c * δ / 2 by positivity)).mpr
    have h := mul_le_mul_of_nonneg_left hlog hn.le
    have hs := mul_le_mul_of_nonneg_left hspread hc.le
    linarith only [hupper, hlevel, hs, h, congrArg (fun y : ℝ ↦ y * a) hk_eq]
  have hloA : lo ≤ a := by
    rw [← exp_log ha]
    apply exp_le_exp.mpr
    apply (div_le_iff₀ hn).mpr
    linarith [mul_nonneg hc.le hS]
  have hSB : S ≤ B := by
    apply (le_div_iff₀ hc).mpr
    have h := mul_le_mul_of_nonneg_left ((log_le_sub_one_of_pos ha).trans
      (sub_le_sub_right haA 1)) hn.le
    linarith
  have hb : |b| ≤ hi := calc
    |b| = |a * x i - r i| := by congr 1; dsimp [r]; ring
    _ ≤ |a * x i| + |r i| := abs_sub _ _
    _ ≤ hi := by
      rw [abs_mul, abs_of_pos ha]
      exact add_le_add (mul_le_mul_of_nonneg_right haA (abs_nonneg _)) (hiS.trans hSB)
  exact ⟨⟨hloA, haA⟩, abs_le.mp hb⟩

/-- The inverse-scale and original parameter domains are in bijection. -/
def inverseEquiv : domain ≃ inverseDomain where
  toFun p := ⟨(p.val.2⁻¹, p.val.1 / p.val.2), ⟨inv_pos.mpr p.property.2, mem_univ _⟩⟩
  invFun p := ⟨(p.val.2 / p.val.1, p.val.1⁻¹), ⟨mem_univ _, inv_pos.mpr p.property.1⟩⟩
  left_inv p := by
    have hp : 0 < p.val.2 := p.property.2
    apply Subtype.ext
    simp [hp.ne']
  right_inv p := by
    have hp : 0 < p.val.1 := p.property.1
    apply Subtype.ext
    simp [hp.ne']

/-- The bijection preserves log likelihood values. -/
theorem inverseLogLik_inverseEquiv (g : ℝ → ℝ) (x : Fin n → ℝ) (p : domain) :
    inverseLogLik g x (inverseEquiv p) = logLik g x p := by
  unfold inverseLogLik logLik
  apply Finset.sum_congr rfl
  intro i _
  change log p.val.2⁻¹ - g (p.val.2⁻¹ * x i - p.val.1 / p.val.2) =
    -log p.val.2 - g ((x i - p.val.1) / p.val.2)
  rw [log_inv]
  congr 2
  ring

private theorem max_iff {g : ℝ → ℝ} {x : Fin n → ℝ} (p : domain) :
    IsMaxOn (inverseLogLik g x) inverseDomain (inverseEquiv p) ↔
      IsMaxOn (logLik g x) domain p := by
  constructor
  · intro h q hq
    have hm := h (inverseEquiv ⟨q, hq⟩).property
    change inverseLogLik g x (inverseEquiv ⟨q, hq⟩) ≤
      inverseLogLik g x (inverseEquiv p) at hm
    change logLik g x q ≤ logLik g x p
    simpa only [inverseLogLik_inverseEquiv] using hm
  · intro h q hq
    obtain ⟨r, hr⟩ := inverseEquiv.surjective ⟨q, hq⟩
    have he : (inverseEquiv r : ℝ × ℝ) = q := congrArg Subtype.val hr
    change inverseLogLik g x q ≤ inverseLogLik g x (inverseEquiv p)
    rw [← he, inverseLogLik_inverseEquiv, inverseLogLik_inverseEquiv]
    exact h r.property

/-- Convex loss with a positive linear lower bound gives an attained MLE. -/
theorem exists_mle {g : ℝ → ℝ} (hg : ConvexOn ℝ univ g) (ht : LinearLowerBound g)
    {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃ p ∈ domain, IsMaxOn (logLik g x) domain p := by
  obtain ⟨q, hq, hm⟩ := UnivariateML.Generic.exists_isMaxOn_of_tendsto_cocompact
    inverseDomain_nonempty ((concave_inverseLogLik hg x).continuousOn inverseDomain_open)
    (tendsto_inverseLogLik_cocompact ht hx)
  obtain ⟨p, hp⟩ := inverseEquiv.surjective ⟨q, hq⟩
  exact ⟨p, p.property, (max_iff p).mp (by simpa only [hp] using hm)⟩

/-- Strict convexity gives a unique MLE under the same coercivity hypotheses. -/
theorem existsUnique_mle {g : ℝ → ℝ} (hg : StrictConvexOn ℝ univ g) (ht : LinearLowerBound g)
    {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik g x) domain p := by
  have hn : 0 < n := by
    obtain ⟨i, _, _⟩ := hx
    exact (Nat.zero_le i.val).trans_lt i.isLt
  obtain ⟨p, hp, hm⟩ := exists_mle hg.convexOn ht hx
  refine ⟨p, ⟨hp, hm⟩, ?_⟩
  intro q hq
  have he := (strictConcave_inverseLogLik hg x hn).eq_of_isMaxOn
    ((max_iff ⟨q, hq.1⟩).mpr hq.2) ((max_iff ⟨p, hp⟩).mpr hm)
    (inverseEquiv ⟨q, hq.1⟩).property (inverseEquiv ⟨p, hp⟩).property
  exact congrArg Subtype.val (inverseEquiv.injective (Subtype.ext he))

/-- At a common observation, shrinking scale makes the log likelihood unbounded above. -/
theorem not_bddAbove_logLik_constant (g : ℝ → ℝ) (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (logLik g (fun _ : Fin n ↦ m) '' domain) := by
  rintro ⟨level, hlevel⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let s := exp (-(level / n + g 0 + 1))
  have hm : (m, s) ∈ domain := ⟨mem_univ _, exp_pos _⟩
  have h := hlevel (mem_image_of_mem _ hm)
  change logLik g (fun _ : Fin n ↦ m) (m, s) ≤ level at h
  simp only [logLik, s, log_exp, sub_self, zero_div, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul] at h
  have he : (n : ℝ) * (level / n) = level := by field_simp
  linarith

end

end UnivariateML.LocationScale
