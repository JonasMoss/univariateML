module

public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Topology.Order.Compact

import UnivariateML.Generic.Escape
import UnivariateML.Generic.Limit
import UnivariateML.Generic.Profile
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Burr XII with the first shape or scale held

The log likelihood is averaged over a nonempty sample. The normalized
form omits the data-only Jacobian term, the negative mean of the log data.

## Deviations

The results concern exact real arithmetic. They use log observations for the
first-shape hold and log ratios for the scale hold. The endpoint value is the
normalized mean from the November 25 derivation. The dossier uses the total
physical log likelihood, including the Jacobian. The endpoint theorem also
covers constant samples strictly above the held scale.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.BurrHeld

noncomputable section

variable {n : ℕ}

def avg (z : Fin n → ℝ) : ℝ := (∑ i, z i) / n

def softplus (t : ℝ) : ℝ := log (1 + exp t)

def logRatios (x : Fin n → ℝ) (s : ℝ) : Fin n → ℝ := fun i ↦ log (x i / s)

def logDensity (k c s x : ℝ) : ℝ :=
  log k + log c - log x + c * log (x / s) - (k + 1) * softplus (c * log (x / s))

def firstDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ

def shapeDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

def firstLik (y : Fin n → ℝ) (k : ℝ) (p : ℝ × ℝ) : ℝ :=
  log k + log p.1 - avg (fun i ↦ softplus (-(p.1 * y i + p.2)) +
    k * softplus (p.1 * y i + p.2))

def G (z : Fin n → ℝ) (c : ℝ) : ℝ := avg (fun i ↦ softplus (c * z i))

def R (z : Fin n → ℝ) (c : ℝ) : ℝ := avg (fun i ↦ softplus (-(c * z i)))

def scaleLik (z : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  log p.2 + log p.1 - p.2 * G z p.1 - R z p.1

def bestK (z : Fin n → ℝ) (c : ℝ) : ℝ := (G z c)⁻¹

def scaleProfile (z : Fin n → ℝ) (c : ℝ) : ℝ :=
  -1 - log (G z c / c) - R z c

private theorem cast_pos (hn : 0 < n) : 0 < (n : ℝ) := by exact_mod_cast hn

private theorem avg_mono {u v : Fin n → ℝ} (h : ∀ i, u i ≤ v i) : avg u ≤ avg v :=
  div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ h i) (Nat.cast_nonneg _)

private theorem avg_const (hn : 0 < n) (a : ℝ) : avg (fun _ : Fin n ↦ a) = a := by
  simp [avg, mul_comm, (cast_pos hn).ne']

private theorem avg_add (a b : Fin n → ℝ) :
    avg (fun i ↦ a i + b i) = avg a + avg b := by simp [avg, Finset.sum_add_distrib, add_div]

private theorem avg_mul (a : ℝ) (b : Fin n → ℝ) :
    avg (fun i ↦ a * b i) = a * avg b := by rw [avg, ← Finset.mul_sum, mul_div_assoc, avg]

private theorem avg_pos (hn : 0 < n) {a : Fin n → ℝ} (ha : ∀ i, 0 < a i) :
    0 < avg a := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  exact div_pos (Finset.sum_pos (fun i _ ↦ ha i) Finset.univ_nonempty) (cast_pos hn)

private theorem avg_one_le (hn : 0 < n) {a : Fin n → ℝ} (ha : ∀ i, 0 ≤ a i)
    (i : Fin n) : a i / n ≤ avg a :=
  div_le_div_of_nonneg_right (Finset.single_le_sum (fun j _ ↦ ha j) (Finset.mem_univ i))
    (cast_pos hn).le

private theorem softplus_pos (t : ℝ) : 0 < softplus t :=
  log_pos (by linarith [exp_pos t])

private theorem softplus_shift (t : ℝ) : softplus t = t + softplus (-t) := by
  have he : exp t * (1 + exp (-t)) = 1 + exp t := by
    rw [mul_add, mul_one, ← exp_add, add_neg_cancel, exp_zero, add_comm]
  calc
    softplus t = log (exp t * (1 + exp (-t))) := by rw [he, softplus]
    _ = t + softplus (-t) := by rw [log_mul (exp_pos _).ne' (by positivity), log_exp, softplus]

private theorem le_softplus (t : ℝ) : t ≤ softplus t := by
  rw [softplus_shift t]
  linarith [softplus_pos (-t)]

private theorem softplus_le_exp (t : ℝ) : softplus t ≤ exp t := by
  unfold softplus
  linarith [log_le_sub_one_of_pos (show 0 < 1 + exp t by positivity)]

private theorem continuous_softplus : Continuous softplus :=
  (continuous_const.add continuous_exp).log (fun t ↦ (show 0 < 1 + exp t by positivity).ne')

private theorem softplus_deriv (t : ℝ) :
    HasDerivAt softplus (exp t / (1 + exp t)) t :=
  ((hasDerivAt_exp t).const_add 1).log (by positivity)

private theorem logistic_deriv (t : ℝ) :
    HasDerivAt (fun u : ℝ ↦ exp u / (1 + exp u)) (exp t / (1 + exp t)^2) t := by
  convert (hasDerivAt_exp t).div ((hasDerivAt_exp t).const_add 1) (by positivity) using 1
  ring

private theorem strictConvex_softplus : StrictConvexOn ℝ univ softplus := by
  apply strictConvexOn_of_deriv2_pos' convex_univ continuous_softplus.continuousOn
  intro t _
  have he : deriv softplus = fun u ↦ exp u / (1 + exp u) :=
    funext fun u ↦ (softplus_deriv u).deriv
  change 0 < deriv (deriv softplus) t
  rw [he, (logistic_deriv t).deriv]
  positivity

private theorem mix (a b : ℝ) (p q : ℝ × ℝ) (y : ℝ) :
    (a • p + b • q).1 * y + (a • p + b • q).2 =
      a * (p.1 * y + p.2) + b * (q.1 * y + q.2) := by
  change (a * p.1 + b * q.1) * y + (a * p.2 + b * q.2) = _
  ring

private theorem penalty_convex {k a b u v : ℝ} (hk : 0 < k)
    (ha : 0 < a) (hb : 0 < b) (hab : a + b = 1) :
    softplus (-(a * u + b * v)) + k * softplus (a * u + b * v) ≤
      a * (softplus (-u) + k * softplus u) + b * (softplus (-v) + k * softplus v) := by
  have h₁ := strictConvex_softplus.convexOn.2 (mem_univ (-u)) (mem_univ (-v)) ha.le hb.le hab
  have h₂ := strictConvex_softplus.convexOn.2 (mem_univ u) (mem_univ v) ha.le hb.le hab
  simp only [smul_eq_mul] at h₁ h₂
  rw [show -(a * u + b * v) = a * -u + b * -v by ring]
  nlinarith [mul_le_mul_of_nonneg_left h₂ hk.le]

private theorem penalty_strict {k a b u v : ℝ} (hk : 0 < k)
    (ha : 0 < a) (hb : 0 < b) (hab : a + b = 1) (huv : u ≠ v) :
    softplus (-(a * u + b * v)) + k * softplus (a * u + b * v) <
      a * (softplus (-u) + k * softplus u) + b * (softplus (-v) + k * softplus v) := by
  have h₁ := strictConvex_softplus.convexOn.2 (mem_univ (-u)) (mem_univ (-v)) ha.le hb.le hab
  have h₂ := strictConvex_softplus.2 (mem_univ u) (mem_univ v) huv ha hb hab
  simp only [smul_eq_mul] at h₁ h₂
  rw [show -(a * u + b * v) = a * -u + b * -v by ring]
  nlinarith [mul_lt_mul_of_pos_left h₂ hk]

/-- Distinct log observations make the fixed-first-shape likelihood strictly concave. -/
theorem first_strictConcave (hn : 0 < n) {y : Fin n → ℝ} {k : ℝ} (hk : 0 < k)
    (hvar : ∃ i j, y i ≠ y j) : StrictConcaveOn ℝ firstDomain (firstLik y k) := by
  refine ⟨(convex_Ioi _).prod convex_univ, ?_⟩
  intro p hp q hq hpq a b ha hb hab
  obtain ⟨i, j, hij⟩ := hvar
  have hex : ∃ i, p.1 * y i + p.2 ≠ q.1 * y i + q.2 := by
    by_contra! h
    have hz : (p.1 - q.1) * (y i - y j) = 0 := by
      calc
        _ = (p.1 * y i + p.2 - (q.1 * y i + q.2)) -
            (p.1 * y j + p.2 - (q.1 * y j + q.2)) := by ring
        _ = 0 := by simp [h i, h j]
    have he := sub_eq_zero.mp ((mul_eq_zero.mp hz).resolve_right (sub_ne_zero.mpr hij))
    exact hpq (Prod.ext he (by simpa [he] using h i))
  obtain ⟨i, hi⟩ := hex
  have hlog := strictConcaveOn_log_Ioi.concaveOn.2 hp.1 hq.1 ha.le hb.le hab
  change a * log p.1 + b * log q.1 ≤ log (a * p.1 + b * q.1) at hlog
  let P (r : ℝ × ℝ) (i : Fin n) := softplus (-(r.1 * y i + r.2)) + k * softplus (r.1 * y i + r.2)
  have hsum : avg (P (a • p + b • q)) < avg (fun i ↦ a * P p i + b * P q i) := by
    apply div_lt_div_of_pos_right _ (cast_pos hn)
    apply Finset.sum_lt_sum (fun j _ ↦ ?_) ⟨i, Finset.mem_univ _, ?_⟩
    · unfold P
      rw [mix]
      exact penalty_strict hk ha hb hab hi
    · unfold P
      rw [mix]
      exact penalty_convex hk ha hb hab
  have havg : avg (fun i ↦ a * P p i + b * P q i) = a * avg (P p) + b * avg (P q) := by
    rw [avg_add, avg_mul, avg_mul]
  rw [havg] at hsum
  change a * (log k + log p.1 - avg (P p)) + b * (log k + log q.1 - avg (P q)) <
    log k + log (a * p.1 + b * q.1) - avg (P (a • p + b • q))
  nlinarith [congrArg (fun x ↦ x * log k) hab]

private theorem first_penalty_ge {k t : ℝ} (hk : 0 < k) :
    min 1 k * |t| ≤ softplus (-t) + k * softplus t := by
  rcases le_total 0 t with ht | ht
  · rw [abs_of_nonneg ht]
    nlinarith [mul_le_mul_of_nonneg_right (min_le_right 1 k) ht,
      mul_le_mul_of_nonneg_left (le_softplus t) hk.le, softplus_pos (-t)]
  · rw [abs_of_nonpos ht]
    nlinarith [mul_le_mul_of_nonneg_right (min_le_left 1 k) (neg_nonneg.mpr ht),
      le_softplus (-t), mul_nonneg hk.le (softplus_pos t).le]

private theorem first_point_bound (hn : 0 < n) {k : ℝ} (hk : 0 < k)
    (y : Fin n → ℝ) (p : ℝ × ℝ) (i : Fin n) :
    min 1 k / n * |p.1 * y i + p.2| ≤ log k + log p.1 - firstLik y k p := by
  unfold firstLik
  rw [show ∀ A B : ℝ, A - (A - B) = B by intros; ring]
  rw [div_mul_eq_mul_div]
  exact (div_le_div_of_nonneg_right (first_penalty_ge hk) (cast_pos hn).le).trans
    (avg_one_le hn (fun j ↦ add_nonneg (softplus_pos (-(p.1 * y j + p.2))).le
      (mul_nonneg hk.le (softplus_pos (p.1 * y j + p.2)).le)) i)

/-- Every likelihood superlevel is contained in a compact box inside the parameter domain. -/
private theorem first_superlevel (hn : 0 < n) {k : ℝ} (hk : 0 < k) {y : Fin n → ℝ}
    (hvar : ∃ i j, y i ≠ y j) (M : ℝ) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ firstDomain ∧
      ∀ p ∈ firstDomain, M ≤ firstLik y k p → p ∈ K := by
  obtain ⟨i, j, hij⟩ := hvar
  let m : ℝ := min 1 k / n
  have hm : 0 < m := div_pos (lt_min (by norm_num) hk) (cast_pos hn)
  let d := m * |y i - y j| / 4
  have hd : 0 < d := by
    dsimp [d]
    exact div_pos (mul_pos hm (abs_pos.mpr (sub_ne_zero.mpr hij))) (by norm_num)
  let C := (log k - M - log d - 1) / d
  let B := (log k + log (max C (exp (M - log k))) - M) / m +
    |max C (exp (M - log k))| * |y i|
  refine ⟨Icc (exp (M - log k)) (max C (exp (M - log k))) ×ˢ Icc (-B) B,
    isCompact_Icc.prod isCompact_Icc, ?_, ?_⟩
  · intro p hp
    exact ⟨(exp_pos _).trans_le hp.1.1, mem_univ _⟩
  · intro p hp hM
    have hc : 0 < p.1 := hp.1
    have hi := first_point_bound hn hk y p i
    have hj := first_point_bound hn hk y p j
    change m * _ ≤ _ at hi hj
    have he := mul_nonneg hm.le (abs_nonneg (p.1 * y i + p.2))
    have hlo : exp (M - log k) ≤ p.1 :=
      by rw [← exp_log hc]; exact exp_le_exp.mpr (by linarith)
    have hdiff := abs_sub (p.1 * y i + p.2) (p.1 * y j + p.2)
    rw [show p.1 * y i + p.2 - (p.1 * y j + p.2) = p.1 * (y i - y j) by ring,
      abs_mul, abs_of_pos hc] at hdiff
    have hlin := mul_le_mul_of_nonneg_left hdiff hm.le
    have hlog := log_le_sub_one_of_pos (mul_pos hd hc)
    rw [log_mul hd.ne' hc.ne'] at hlog
    have hhi : p.1 ≤ C := by
      apply (le_div_iff₀ hd).2
      dsimp [d] at hd ⊢
      nlinarith [show d * p.1 = m * (p.1 * |y i - y j|) / 4 by dsimp [d]; ring]
    have hcB : p.1 ≤ max C (exp (M - log k)) := hhi.trans (le_max_left _ _)
    have hlogB := log_le_log hc hcB
    have hbi : |p.1 * y i + p.2| ≤
        (log k + log (max C (exp (M - log k))) - M) / m := by
      apply (le_div_iff₀ hm).2
      nlinarith
    have hb := abs_sub (p.1 * y i + p.2) (p.1 * y i)
    rw [add_sub_cancel_left, abs_mul, abs_of_pos hc] at hb
    have hbB : |p.2| ≤ B := hb.trans (add_le_add hbi
      (mul_le_mul_of_nonneg_right (hcB.trans (le_abs_self _)) (abs_nonneg _)))
    exact ⟨⟨hlo, hcB⟩, abs_le.mp hbB⟩

private theorem continuousOn_first (y : Fin n → ℝ) (k : ℝ) :
    ContinuousOn (firstLik y k) firstDomain := by
  have h (i : Fin n) : Continuous (fun p : ℝ × ℝ ↦ p.1 * y i + p.2) :=
    (continuous_fst.mul continuous_const).add continuous_snd
  have hP : Continuous (fun p : ℝ × ℝ ↦ avg (fun i ↦
      softplus (-(p.1 * y i + p.2)) + k * softplus (p.1 * y i + p.2))) := by
    unfold avg
    exact (continuous_finsetSum _ fun i _ ↦ (continuous_softplus.comp (h i).neg).add
      (continuous_const.mul (continuous_softplus.comp (h i)))).div_const _
  have hl : ContinuousOn (fun p : ℝ × ℝ ↦ log p.1) firstDomain :=
    continuous_fst.continuousOn.log fun p hp ↦ (show 0 < p.1 from hp.1).ne'
  exact (continuousOn_const.add hl).sub hP.continuousOn

private theorem escape_of_superlevels {X : Type*} [TopologicalSpace X]
    {D : Set X} {L : X → ℝ}
    (h : ∀ M, ∃ K : Set X, IsCompact K ∧ K ⊆ D ∧
      ∀ p ∈ D, M ≤ L p → p ∈ K) :
    Tendsto (fun p : D ↦ L p) (cocompact D) atBot := by
  apply tendsto_atBot.2
  intro M
  obtain ⟨K, hK, hKD, hlevel⟩ := h M
  have hK' : IsCompact ((Subtype.val : D → X) ⁻¹' K) :=
    IsEmbedding.subtypeVal.isInducing.isCompact_preimage' hK
      (by simpa only [Subtype.range_coe] using hKD)
  have he : ∀ᶠ p : D in cocompact D, p ∈ ((Subtype.val : D → X) ⁻¹' K)ᶜ :=
    hK'.compl_mem_cocompact
  exact he.mono fun p hp ↦ (lt_of_not_ge fun hM ↦ hp (hlevel p p.property hM)).le

/-- The fixed-first-shape likelihood tends to minus infinity on escape from
compact subsets of its feasible parameter domain. -/
theorem first_coercive (hn : 0 < n) {k : ℝ} (hk : 0 < k) {y : Fin n → ℝ}
    (hvar : ∃ i j, y i ≠ y j) :
    Tendsto (fun p : firstDomain ↦ firstLik y k p) (cocompact firstDomain) atBot :=
  escape_of_superlevels (first_superlevel hn hk hvar)

/-- At fixed first shape the two free coefficients have one finite maximizer. -/
theorem first_existsUnique (hn : 0 < n) {k : ℝ} (hk : 0 < k) {y : Fin n → ℝ}
    (hvar : ∃ i j, y i ≠ y j) :
    ∃! p, p ∈ firstDomain ∧ IsMaxOn (firstLik y k) firstDomain p :=
  Generic.existsUnique_isMaxOn_of_strictConcaveOn_of_tendsto_cocompact
    ⟨(1, 0), by norm_num [firstDomain]⟩ (continuousOn_first y k)
    (first_strictConcave hn hk hvar) (first_coercive hn hk hvar)

/-- For constant log observations the path `b = -c * y₀` has unbounded likelihood. -/
theorem first_constant_unbounded (hn : 0 < n) (k y₀ : ℝ) :
    ¬ BddAbove (firstLik (fun _ : Fin n ↦ y₀) k '' firstDomain) := by
  rintro ⟨M, hM⟩
  let d := softplus 0 + k * softplus 0
  let c := exp (M + 1 - log k + d)
  have hb : firstLik (fun _ : Fin n ↦ y₀) k (c, -(c * y₀)) ≤ M :=
    hM (mem_image_of_mem _ ⟨exp_pos _, mem_univ _⟩)
  change log k + log c - avg (fun _ : Fin n ↦
    softplus (-(c * y₀ + -(c * y₀))) + k * softplus (c * y₀ + -(c * y₀))) ≤ M at hb
  simp only [add_neg_cancel, neg_zero, avg_const hn] at hb
  change log k + log (exp (M + 1 - log k + d)) - d ≤ M at hb
  rw [log_exp] at hb
  linarith

private theorem G_pos (hn : 0 < n) (z : Fin n → ℝ) (c : ℝ) : 0 < G z c :=
  avg_pos hn (fun _ ↦ softplus_pos _)

private theorem R_pos (hn : 0 < n) (z : Fin n → ℝ) (c : ℝ) : 0 < R z c :=
  avg_pos hn (fun _ ↦ softplus_pos _)

private theorem G_shift (z : Fin n → ℝ) (c : ℝ) : G z c = c * avg z + R z c := by
  unfold G R
  simp_rw [softplus_shift (c * z _)]
  rw [avg_add, avg_mul]

private theorem profile_at (hn : 0 < n) (z : Fin n → ℝ) {c : ℝ} (hc : 0 < c) :
    scaleLik z (c, bestK z c) = scaleProfile z c := by
  unfold scaleLik bestK scaleProfile
  dsimp
  rw [log_inv, log_div (G_pos hn z c).ne' hc.ne', inv_mul_cancel₀ (G_pos hn z c).ne']
  ring

/-- For each positive power the first shape has a unique maximizing value. -/
theorem scale_bestK (hn : 0 < n) (z : Fin n → ℝ) {c : ℝ} (hc : 0 < c) :
    bestK z c ∈ Ioi (0 : ℝ) ∧
      IsMaxOn (fun k ↦ scaleLik z (c, k)) (Ioi 0) (bestK z c) ∧
      ∀ k ∈ Ioi (0 : ℝ), IsMaxOn (fun u ↦ scaleLik z (c, u)) (Ioi 0) k → k = bestK z c := by
  have hg := G_pos hn z c
  have hbest : 0 < bestK z c := inv_pos.mpr hg
  have hstrict (k : ℝ) (hk : 0 < k) (he : k ≠ bestK z c) :
      scaleLik z (c, k) < scaleLik z (c, bestK z c) := by
    rw [profile_at hn z hc]
    have hh : k * G z c ≠ 1 := by
      intro h
      apply he
      unfold bestK
      rw [← one_div, eq_div_iff hg.ne']
      exact h
    have h := log_lt_sub_one_of_pos (mul_pos hk hg) hh
    rw [log_mul hk.ne' hg.ne'] at h
    unfold scaleLik scaleProfile
    dsimp
    rw [log_div hg.ne' hc.ne']
    linarith
  refine ⟨hbest, ?_, ?_⟩
  · intro k hk
    by_cases he : k = bestK z c
    · subst k
      exact le_rfl
    · exact (hstrict k hk he).le
  · intro k hk hm
    by_contra he
    exact (hstrict k hk he).not_ge (hm hbest)

private theorem scale_le_profile (hn : 0 < n) (z : Fin n → ℝ) {p : ℝ × ℝ}
    (hp : p ∈ shapeDomain) : scaleLik z p ≤ scaleProfile z p.1 := by
  rw [← profile_at hn z hp.1]
  have hg := G_pos hn z p.1
  have hk : 0 < p.2 := hp.2
  have h := log_le_sub_one_of_pos (mul_pos hk hg)
  rw [log_mul hk.ne' hg.ne'] at h
  rcases p with ⟨c, k⟩
  unfold scaleLik bestK
  dsimp at hg h ⊢
  rw [log_inv, inv_mul_cancel₀ hg.ne']
  linarith

private theorem tendsto_avg {ι : Type*} {l : Filter ι} {f : ι → Fin n → ℝ} {z : Fin n → ℝ}
    (h : ∀ i, Tendsto (fun a ↦ f a i) l (𝓝 (z i))) :
    Tendsto (fun a ↦ avg (f a)) l (𝓝 (avg z)) :=
  (tendsto_finsetSum _ fun i _ ↦ h i).div_const _

def zeroCount (z : Fin n → ℝ) : ℕ :=
  (Finset.univ.filter fun i ↦ z i = 0).card

def paretoValue (z : Fin n → ℝ) : ℝ :=
  -1 - log (avg z) - ((zeroCount z : ℝ) / n) * log 2

private theorem zero_avg (z : Fin n → ℝ) :
    avg (fun i ↦ if z i = 0 then log 2 else 0) = ((zeroCount z : ℝ) / n) * log 2 := by
  classical
  unfold avg zeroCount
  rw [← Finset.sum_filter, Finset.sum_const, nsmul_eq_mul]
  ring

private theorem R_limit {z : Fin n → ℝ} (hz : ∀ i, 0 ≤ z i) :
    Tendsto (R z) atTop (𝓝 (((zeroCount z : ℝ) / n) * log 2)) := by
  rw [← zero_avg]
  apply tendsto_avg
  intro i
  by_cases he : z i = 0
  · norm_num [he, softplus]
  · have hi : 0 < z i := lt_of_le_of_ne (hz i) (Ne.symm he)
    have ht : Tendsto (fun c : ℝ ↦ -(c * z i)) atTop atBot :=
      tendsto_neg_atTop_atBot.comp ((tendsto_id.const_mul_atTop hi).congr fun c ↦ mul_comm (z i) c)
    have hexp := tendsto_exp_atBot.comp ht
    have he₁ : Tendsto (fun c ↦ 1 + exp (-(c * z i))) atTop (𝓝 1) := by
      simpa only [add_zero, Function.comp_apply] using tendsto_const_nhds.add hexp (b := 0) (a := 1)
    have hlog := (continuousAt_log (show (1 : ℝ) ≠ 0 by norm_num)).tendsto.comp he₁
    simpa [softplus, he, Function.comp_def] using hlog

private theorem nonneg_avg_pos (hn : 0 < n) {z : Fin n → ℝ}
    (hz : ∀ i, 0 ≤ z i) (hpos : ∃ i, 0 < z i) : 0 < avg z := by
  obtain ⟨i, hi⟩ := hpos
  exact (div_pos hi (cast_pos hn)).trans_le (avg_one_le hn hz i)

/-- The profiled path converges to the normalized Pareto value, including ties at scale. -/
theorem scale_pareto_limit (hn : 0 < n) {z : Fin n → ℝ}
    (hz : ∀ i, 0 ≤ z i) (hpos : ∃ i, 0 < z i) :
    Tendsto (scaleProfile z) atTop (𝓝 (paretoValue z)) := by
  have hr := R_limit hz
  have hratio : Tendsto (fun c ↦ G z c / c) atTop (𝓝 (avg z)) := by
    have hd := hr.div_atTop tendsto_id
    have ht : Tendsto (fun c ↦ avg z + R z c / c) atTop (𝓝 (avg z)) := by
      simpa only [add_zero, id_eq] using (tendsto_const_nhds (x := avg z)).add hd
    apply ht.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with c hc
    rw [G_shift, add_div, mul_div_cancel_left₀ _ hc.ne']
  exact (tendsto_const_nhds.sub
    ((continuousAt_log (nonneg_avg_pos hn hz hpos).ne').tendsto.comp hratio)).sub hr

private theorem scale_below_pareto (hn : 0 < n) {z : Fin n → ℝ}
    (hz : ∀ i, 0 ≤ z i) (hpos : ∃ i, 0 < z i) {c : ℝ} (hc : 0 < c) :
    scaleProfile z c < paretoValue z := by
  have hG : avg z < G z c / c := by
    rw [G_shift, add_div, mul_div_cancel_left₀ _ hc.ne']
    linarith [div_pos (R_pos hn z c) hc]
  have hR : ((zeroCount z : ℝ) / n) * log 2 ≤ R z c := by
    rw [← zero_avg]
    apply avg_mono
    intro i
    by_cases he : z i = 0
    · norm_num [he, softplus]
    · simpa [he] using (softplus_pos (-(c * z i))).le
  unfold scaleProfile paretoValue
  linarith [(log_lt_log (nonneg_avg_pos hn hz hpos) hG)]

/-- The finite two-shape likelihoods lie strictly below their Pareto supremum. -/
theorem scale_pareto_supremum (hn : 0 < n) {z : Fin n → ℝ}
    (hz : ∀ i, 0 ≤ z i) (hpos : ∃ i, 0 < z i) :
    IsLUB (scaleLik z '' shapeDomain) (paretoValue z) ∧
      ∀ p ∈ shapeDomain, scaleLik z p < paretoValue z := by
  have hlt : ∀ p ∈ shapeDomain, scaleLik z p < paretoValue z :=
    fun p hp ↦ (scale_le_profile hn z hp).trans_lt (scale_below_pareto hn hz hpos hp.1)
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hlt p hp).le
  · intro M hM
    refine Generic.limit_le_upperBound (l := atTop) (p := fun c ↦ (c, bestK z c)) ?_ ?_ hM
    · filter_upwards [eventually_gt_atTop (0 : ℝ)] with c hc
      exact ⟨hc, inv_pos.mpr (G_pos hn z c)⟩
    · refine (scale_pareto_limit hn hz hpos).congr' ?_
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with c hc
      exact (profile_at hn z hc).symm

theorem scale_pareto_no_max (hn : 0 < n) {z : Fin n → ℝ}
    (hz : ∀ i, 0 ≤ z i) (hpos : ∃ i, 0 < z i) :
    ¬ ∃ p ∈ shapeDomain, IsMaxOn (scaleLik z) shapeDomain p := by
  rintro ⟨p, hp, hm⟩
  obtain ⟨hlub, hlt⟩ := scale_pareto_supremum hn hz hpos
  apply (hlt p hp).not_ge
  apply hlub.2
  rintro _ ⟨q, hq, rfl⟩
  exact hm hq


private def logAdd (u v : ℝ) : ℝ := log (exp u + exp v)

private theorem logAdd_eq (u v : ℝ) : logAdd u v = v + softplus (u - v) := by
  have he : exp v * (1 + exp (u - v)) = exp u + exp v := by
    rw [mul_add, mul_one, ← exp_add, show v + (u - v) = u by ring, add_comm]
  rw [logAdd, ← he, log_mul (exp_pos _).ne' (by positivity), log_exp, softplus]

private theorem logAdd_mix {a b : ℝ} (ha : 0 < a) (hb : 0 < b) (hab : a + b = 1)
    (u v w t : ℝ) :
    logAdd (a * u + b * v) (a * w + b * t) ≤ a * logAdd u w + b * logAdd v t := by
  simp_rw [logAdd_eq]
  rw [show a * u + b * v - (a * w + b * t) = a * (u - w) + b * (v - t) by ring]
  have he := strictConvex_softplus.convexOn.2 (mem_univ (u - w)) (mem_univ (v - t))
    ha.le hb.le hab
  simp only [smul_eq_mul] at he
  linarith

private theorem logAdd_lt {u v w t : ℝ} (h₁ : u < v) (h₂ : w ≤ t) :
    logAdd u w < logAdd v t := by
  unfold logAdd
  exact log_lt_log (by positivity) (add_lt_add_of_lt_of_le (exp_lt_exp.mpr h₁) (exp_le_exp.mpr h₂))

private theorem logAdd_log {u v : ℝ} (hu : 0 < u) (hv : 0 < v) :
    logAdd (log u) (log v) = log (u + v) := by simp [logAdd, exp_log hu, exp_log hv]

private theorem strictLogConvex_add {f g : ℝ → ℝ}
    (hf : ∀ x > 0, 0 < f x) (hg : ∀ x > 0, 0 < g x)
    (hfc : StrictConvexOn ℝ (Ioi 0) (fun x ↦ log (f x)))
    (hgc : ConvexOn ℝ (Ioi 0) (fun x ↦ log (g x))) :
    StrictConvexOn ℝ (Ioi 0) (fun x ↦ log (f x + g x)) := by
  refine ⟨convex_Ioi _, ?_⟩
  intro x hx y hy hxy a b ha hb hab
  have hm : a * x + b * y ∈ Ioi (0 : ℝ) :=
    (convex_Ioi (0 : ℝ)) hx hy ha.le hb.le hab
  dsimp only [smul_eq_mul]
  rw [← logAdd_log (hf _ hm) (hg _ hm), ← logAdd_log (hf _ hx) (hg _ hx),
    ← logAdd_log (hf _ hy) (hg _ hy)]
  exact (logAdd_lt (hfc.2 hx hy hxy ha hb hab)
    (hgc.2 hx hy ha.le hb.le hab)).trans_le (logAdd_mix ha hb hab _ _ _ _)

private theorem strictLogConvex_sum {ι : Type*} (S : Finset ι) (hS : S.Nonempty)
    (f : ι → ℝ → ℝ) (hf : ∀ i ∈ S, ∀ x > 0, 0 < f i x)
    (hc : ∀ i ∈ S, StrictConvexOn ℝ (Ioi 0) (fun x ↦ log (f i x))) :
    StrictConvexOn ℝ (Ioi 0) (fun x ↦ log (∑ i ∈ S, f i x)) := by
  classical
  induction S using Finset.induction_on with
  | empty => simp at hS
  | @insert i S hi ih =>
    simp only [Finset.sum_insert hi]
    by_cases h : S.Nonempty
    · exact strictLogConvex_add (hf i (Finset.mem_insert_self _ _))
        (fun x hx ↦ Finset.sum_pos (fun j hj ↦ hf j (Finset.mem_insert_of_mem hj) x hx) h)
        (hc i (Finset.mem_insert_self _ _))
        (ih h (fun j hj ↦ hf j (Finset.mem_insert_of_mem hj))
          (fun j hj ↦ hc j (Finset.mem_insert_of_mem hj))).convexOn
    · rw [Finset.not_nonempty_iff_eq_empty.mp h]
      simpa using hc i (Finset.mem_insert_self _ _)

private def A (a c : ℝ) : ℝ := softplus (-(a * c))
private def P (a c : ℝ) : ℝ := exp (-(a * c)) / (1 + exp (-(a * c)))
private def Q (a c : ℝ) : ℝ := P a c / A a c

private theorem A_deriv (a c : ℝ) : HasDerivAt (A a) (-a * P a c) c := by
  have h := (softplus_deriv (-(a * c))).comp c (((hasDerivAt_id c).const_mul a).neg)
  change HasDerivAt (A a) (P a c * -(a * 1)) c at h
  convert h using 1
  ring

private theorem P_deriv (a c : ℝ) : HasDerivAt (P a) (-a * P a c * (1 - P a c)) c := by
  have h := (logistic_deriv (-(a * c))).comp c (((hasDerivAt_id c).const_mul a).neg)
  change HasDerivAt (P a) (exp (-(a * c)) / (1 + exp (-(a * c)))^2 * -(a * 1)) c at h
  convert h using 1
  unfold P
  field_simp
  ring

private theorem Q_deriv (a c : ℝ) :
    HasDerivAt (Q a) (a * Q a c * (Q a c - (1 - P a c))) c := by
  have h := (P_deriv a c).div (A_deriv a c) (softplus_pos _).ne'
  change HasDerivAt (Q a)
    ((-a * P a c * (1 - P a c) * A a c - P a c * (-a * P a c)) / A a c ^ 2) c at h
  convert h using 1
  unfold Q
  field_simp [(show 0 < A a c from softplus_pos _).ne']
  ring

private theorem logistic_le_softplus (t : ℝ) : exp t / (1 + exp t) ≤ softplus t := by
  have h := le_log_one_add_of_nonneg (exp_pos t).le
  apply le_trans _ h
  apply (div_le_div_iff₀ (by positivity) (by positivity)).2
  nlinarith [sq_nonneg (exp t)]

private theorem Q_bounds (a c : ℝ) : 0 ≤ Q a c ∧ Q a c ≤ 1 := by
  have hP : 0 < P a c := by unfold P; positivity
  have hA : 0 < A a c := softplus_pos _
  refine ⟨(div_pos hP hA).le, (div_le_one hA).2 ?_⟩
  exact logistic_le_softplus (-(a * c))

private theorem sq_lt_exp {u : ℝ} (hu : 0 ≤ u) : u ^ 2 < exp u := by
  have h := quadratic_le_exp_of_nonneg (show 0 ≤ u / 2 by linarith)
  have h' : u < exp (u / 2) := by nlinarith [sq_nonneg (u / 2 - 1)]
  rw [show u = u / 2 + u / 2 by ring, exp_add]
  nlinarith [mul_pos (sub_pos.mpr h') (show 0 < exp (u / 2) + u by positivity)]

private theorem strictLogConvex_A {a : ℝ} (ha : 0 ≤ a) :
    StrictConvexOn ℝ (Ioi 0) (fun c ↦ log (A a c / c)) := by
  let D (c : ℝ) := -a * Q a c - c⁻¹
  have hd (c : ℝ) (hc : 0 < c) :
      HasDerivAt (fun x ↦ log (A a x / x)) (D c) c := by
    have h := ((A_deriv a c).log (softplus_pos _).ne').sub (hasDerivAt_log hc.ne')
    change HasDerivAt (fun x ↦ log (A a x) - log x)
      ((-a * P a c) / A a c - c⁻¹) c at h
    have he : (fun x ↦ log (A a x) - log x) =ᶠ[𝓝 c] fun x ↦ log (A a x / x) := by
      filter_upwards [Ioi_mem_nhds hc] with x hx
      exact (log_div (softplus_pos _).ne' (show 0 < x from hx).ne').symm
    convert h.congr_of_eventuallyEq he.symm using 1
    unfold D Q
    ring
  have hdd (c : ℝ) (hc : 0 < c) :
      HasDerivAt D (-a ^ 2 * Q a c * (Q a c - (1 - P a c)) + 1 / c ^ 2) c := by
    have h := ((Q_deriv a c).const_mul (-a)).sub ((hasDerivAt_id c).inv hc.ne')
    change HasDerivAt D
      (-a * (a * Q a c * (Q a c - (1 - P a c))) - (-1 / c ^ 2)) c at h
    convert h using 1
    ring
  apply strictConvexOn_of_deriv2_pos' (convex_Ioi _)
  · exact fun c hc ↦ (hd c hc).continuousAt.continuousWithinAt
  intro c hc
  have hco : 0 < c := hc
  have he : deriv (fun x ↦ log (A a x / x)) =ᶠ[𝓝 c] D := by
    filter_upwards [Ioi_mem_nhds hco] with x hx using (hd x hx).deriv
  change 0 < deriv (deriv (fun x ↦ log (A a x / x))) c
  rw [he.deriv_eq, (hdd c hco).deriv]
  obtain ⟨hQ₀, hQ₁⟩ := Q_bounds a c
  have hP₀ : 0 ≤ P a c := by unfold P; positivity
  have hP₁ : P a c ≤ exp (-(a * c)) := by
    unfold P
    exact div_le_self (exp_pos _).le (by linarith [exp_pos (-(a * c))])
  have hQP : Q a c * (Q a c - (1 - P a c)) ≤ exp (-(a * c)) := calc
    _ ≤ Q a c * P a c := mul_le_mul_of_nonneg_left (by linarith) hQ₀
    _ ≤ 1 * P a c := mul_le_mul_of_nonneg_right hQ₁ hP₀
    _ ≤ _ := by simpa using hP₁
  have hsq := sq_lt_exp (mul_nonneg ha hco.le)
  have hE : a ^ 2 * exp (-(a * c)) < 1 / c ^ 2 := by
    rw [lt_div_iff₀ (sq_pos_of_pos hco), exp_neg, ← div_eq_mul_inv]
    rw [show a ^ 2 / exp (a * c) * c ^ 2 = (a * c)^2 / exp (a * c) by ring]
    exact (div_lt_one (exp_pos _)).2 hsq
  linarith [mul_le_mul_of_nonneg_left hQP (sq_nonneg a)]

private theorem strictLogConvex_summand (z : ℝ) :
    StrictConvexOn ℝ (Ioi 0) (fun c ↦ log (softplus (c * z) / c)) := by
  rcases le_or_gt z 0 with hz | hz
  · simpa [A, mul_comm] using strictLogConvex_A (neg_nonneg.mpr hz)
  · have hh := strictLogConvex_add
      (f := fun c ↦ A z c / c) (g := fun _ ↦ z)
      (fun c hc ↦ div_pos (softplus_pos _) hc) (fun _ _ ↦ hz)
      (strictLogConvex_A hz.le) (convexOn_const (log z) (convex_Ioi 0))
    refine hh.congr ?_
    intro c hc
    change log (A z c / c + z) = log (softplus (c * z) / c)
    rw [softplus_shift (c * z), add_div, mul_comm c z, mul_div_cancel_right₀ _ (ne_of_gt hc), A,
      add_comm]

private theorem strictConvex_log_G_div (hn : 0 < n) (z : Fin n → ℝ) :
    StrictConvexOn ℝ (Ioi 0) (fun c ↦ log (G z c / c)) := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  have hh := strictLogConvex_sum Finset.univ Finset.univ_nonempty
    (fun i : Fin n ↦ fun c ↦ softplus (c * z i) / c)
    (fun i _ c hc ↦ div_pos (softplus_pos _) hc)
    (fun i _ ↦ strictLogConvex_summand (z i))
  have he := hh.add_convexOn (convexOn_const (-log (n : ℝ)) (convex_Ioi (0 : ℝ)))
  refine he.congr ?_
  intro c hc
  have hs : 0 < ∑ i : Fin n, softplus (c * z i) / c :=
    Finset.sum_pos (fun _ _ ↦ div_pos (softplus_pos _) hc) Finset.univ_nonempty
  change log (∑ i : Fin n, softplus (c * z i) / c) + -log (n : ℝ) =
    log (G z c / c)
  rw [← sub_eq_add_neg, ← log_div hs.ne' (cast_pos hn).ne', G, avg]
  congr 1
  simp only [div_eq_mul_inv, ← Finset.sum_mul]
  ring

/-- At held scale the first-shape profile is strictly concave in the positive power. -/
theorem scale_strictConcave (hn : 0 < n) (z : Fin n → ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (scaleProfile z) := by
  refine ⟨convex_Ioi _, ?_⟩
  intro x hx y hy hxy a b ha hb hab
  have h := (strictConvex_log_G_div hn z).2 hx hy hxy ha hb hab
  simp only [smul_eq_mul] at h
  have hr : R z (a * x + b * y) ≤ a * R z x + b * R z y := by
    unfold R
    rw [← avg_mul, ← avg_mul, ← avg_add]
    apply avg_mono
    intro i
    have h := strictConvex_softplus.convexOn.2 (mem_univ (-(x * z i)))
      (mem_univ (-(y * z i))) ha.le hb.le hab
    simpa only [smul_eq_mul, add_mul, mul_assoc, neg_add, mul_neg] using h
  change a * scaleProfile z x + b * scaleProfile z y < scaleProfile z (a * x + b * y)
  unfold scaleProfile
  linarith


private theorem softplus_mono : Monotone softplus := by
  intro x y h
  exact log_le_log (by positivity) (add_le_add_right (exp_le_exp.mpr h) 1)

private theorem softplus_exp_lower (t : ℝ) : exp (min t 0) / 2 ≤ softplus t := by
  refine le_trans ?_ (softplus_mono (min_le_left t 0))
  refine le_trans ?_ (logistic_le_softplus (min t 0))
  have he : exp (min t 0) ≤ 1 := by rw [← exp_zero]; exact exp_le_exp.mpr (min_le_right t 0)
  apply div_le_div_of_nonneg_left (exp_pos _).le (by positivity)
  linarith

private theorem avg_strict_of_one (hn : 0 < n) {u v : Fin n → ℝ}
    (h : ∀ i, u i ≤ v i) (h' : ∃ i, u i < v i) : avg u < avg v := by
  obtain ⟨i, hi⟩ := h'
  exact div_lt_div_of_pos_right
    (Finset.sum_lt_sum (fun i _ ↦ h i) ⟨i, Finset.mem_univ _, hi⟩) (cast_pos hn)

private theorem scale_escape_bound (hn : 0 < n) {z : Fin n → ℝ}
    (hvar : ∃ i j, z i ≠ z j) (hneg : ∃ i, z i < 0) :
    ∃ a > 0, ∀ c > 0, scaleProfile z c ≤ log c - 1 - log (1 / (2 * n)) - a * c := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ z Finset.univ_nonempty
  have hmax : ∀ i, z i ≤ z j := fun i ↦ hj i (Finset.mem_univ i)
  let w := fun i ↦ max (-(z i)) 0
  let a := avg w + min (z j) 0
  have ha : 0 < a := by
    rcases le_or_gt 0 (z j) with hj₀ | hj₀
    · rw [show a = avg w by dsimp [a]; rw [min_eq_right hj₀, add_zero]]
      exact nonneg_avg_pos hn (fun i ↦ le_max_right _ _) (by
        obtain ⟨i, hi⟩ := hneg
        exact ⟨i, (neg_pos.mpr hi).trans_le (le_max_left _ _)⟩)
    · have hmin : ∀ i, -(z j) ≤ w i :=
        fun i ↦ (neg_le_neg (hmax i)).trans (le_max_left _ _)
      have hstrict : ∃ i, -(z j) < w i := by
        obtain ⟨i, k, hik⟩ := hvar
        by_cases he : z i = z j
        · exact ⟨k, (neg_lt_neg (lt_of_le_of_ne (hmax k)
            (fun h ↦ hik (he.trans h.symm)))).trans_le (le_max_left _ _)⟩
        · exact ⟨i, (neg_lt_neg (lt_of_le_of_ne (hmax i) he)).trans_le (le_max_left _ _)⟩
      have h := avg_strict_of_one hn hmin hstrict
      rw [avg_const hn] at h
      dsimp [a]
      rw [min_eq_left hj₀.le]
      linarith
  refine ⟨a, ha, fun c hc ↦ ?_⟩
  have hR : c * avg w ≤ R z c := by
    rw [← avg_mul]
    apply avg_mono
    intro i
    dsimp [w]
    rw [mul_max_of_nonneg _ _ hc.le, mul_zero, mul_neg]
    exact max_le (le_softplus _) (softplus_pos _).le
  have hG : exp (c * min (z j) 0) * (1 / (2 * n)) ≤ G z c := by
    have h := softplus_exp_lower (c * z j)
    rw [← mul_zero c, ← mul_min_of_nonneg _ _ hc.le] at h
    have hh := (div_le_div_of_nonneg_right h (cast_pos hn).le).trans
      (avg_one_le hn (fun i ↦ (softplus_pos (c * z i)).le) j)
    unfold G
    convert hh using 1
    ring
  have hd : 0 < (1 / (2 * n) : ℝ) := div_pos one_pos (mul_pos (by norm_num) (cast_pos hn))
  have hlog := log_le_log (mul_pos (exp_pos _) hd) hG
  rw [log_mul (exp_pos _).ne' hd.ne', log_exp] at hlog
  unfold scaleProfile
  rw [log_div (G_pos hn z c).ne' hc.ne']
  dsimp [a]
  linarith [mul_add c (avg w) (min (z j) 0)]

private theorem scale_superlevel (hn : 0 < n) {z : Fin n → ℝ}
    (hvar : ∃ i j, z i ≠ z j) (hneg : ∃ i, z i < 0) (M : ℝ) :
    ∃ K : Set ℝ, IsCompact K ∧ K ⊆ Ioi 0 ∧
      ∀ c > 0, M ≤ scaleProfile z c → c ∈ K := by
  obtain ⟨a, ha, h⟩ := scale_escape_bound hn hvar hneg
  let D := log (1 / (2 * n) : ℝ)
  let lo := exp (M + 1 + D)
  let hi := (-2 - D - log (a / 2) - M) / (a / 2)
  refine ⟨Icc lo hi, isCompact_Icc, fun c hc ↦ (exp_pos _).trans_le hc.1, ?_⟩
  intro c hc hm
  have hb := h c hc
  change scaleProfile z c ≤ log c - 1 - D - a * c at hb
  have hac := mul_pos ha hc
  refine ⟨?_, ?_⟩
  · change exp (M + 1 + D) ≤ c
    rw [← exp_log hc, exp_le_exp]
    linarith
  · have hh : 0 < a / 2 := by positivity
    have hl := log_le_sub_one_of_pos (mul_pos hh hc)
    rw [log_mul hh.ne' hc.ne'] at hl
    apply (le_div_iff₀ hh).2
    linarith

/-- The held-scale profile is coercive when the data vary and some lie below scale. -/
theorem scale_coercive (hn : 0 < n) {z : Fin n → ℝ}
    (hvar : ∃ i j, z i ≠ z j) (hneg : ∃ i, z i < 0) :
    Tendsto (fun c : Ioi (0 : ℝ) ↦ scaleProfile z c) (cocompact (Ioi (0 : ℝ))) atBot :=
  escape_of_superlevels (scale_superlevel hn hvar hneg)

/-- Nonconstant data with an observation below scale have a unique profile maximizer. -/
theorem scale_profile_existsUnique (hn : 0 < n) {z : Fin n → ℝ}
    (hvar : ∃ i j, z i ≠ z j) (hneg : ∃ i, z i < 0) :
    ∃! c, c ∈ Ioi 0 ∧ IsMaxOn (scaleProfile z) (Ioi 0) c :=
  Generic.existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    ⟨1, one_pos⟩ isOpen_Ioi (scale_strictConcave hn z) (scale_coercive hn hvar hneg)

/-- The maximizing profile point and its optimal first shape give the unique joint fit. -/
theorem scale_existsUnique (hn : 0 < n) {z : Fin n → ℝ}
    (hvar : ∃ i j, z i ≠ z j) (hneg : ∃ i, z i < 0) :
    ∃! p, p ∈ shapeDomain ∧ IsMaxOn (scaleLik z) shapeDomain p := by
  obtain ⟨c, ⟨hc, hm⟩, huniq⟩ := scale_profile_existsUnique hn hvar hneg
  have hb (t : ℝ) : 0 < bestK z t := inv_pos.mpr (G_pos hn z t)
  have hm' : IsMaxOn (fun t ↦ scaleLik z (t, bestK z t)) (Ioi 0) c := by
    intro t ht
    change scaleLik z (t, bestK z t) ≤ scaleLik z (c, bestK z c)
    rw [profile_at hn z ht, profile_at hn z hc]
    exact hm ht
  have ⟨hp, hmax⟩ := Generic.isMaxOn_prod_of_profile
    (fun t ht ↦ ⟨(scale_bestK hn z ht).1, (scale_bestK hn z ht).2.1⟩) hc hm'
  refine ⟨_, ⟨hp, hmax⟩, ?_⟩
  intro p hp'
  have hpc : p.1 ∈ Ioi (0 : ℝ) := hp'.1.1
  have hmp : IsMaxOn (scaleProfile z) (Ioi 0) p.1 := by
    intro t ht
    change scaleProfile z t ≤ scaleProfile z p.1
    rw [← profile_at hn z ht]
    exact (hp'.2 (show (t, bestK z t) ∈ shapeDomain from ⟨ht, hb t⟩)).trans
      (scale_le_profile hn z hp'.1)
  have he := huniq p.1 ⟨hpc, hmp⟩
  obtain ⟨_, _, hu⟩ := scale_bestK hn z hpc
  exact Prod.ext he ((hu p.2 hp'.1.2 (fun k hk ↦ hp'.2 ⟨hpc, hk⟩)).trans
    (congrArg (bestK z) he))


/-- The held-scale constant sample at or below scale has unbounded likelihood. -/
theorem scale_constant_unbounded (hn : 0 < n) {z₀ : ℝ} (hz₀ : z₀ ≤ 0) :
    ¬ BddAbove (scaleLik (fun _ : Fin n ↦ z₀) '' shapeDomain) := by
  rintro ⟨M, hM⟩
  let c := exp (M + 3)
  have hc : 0 < c := exp_pos _
  let z : Fin n → ℝ := fun _ ↦ z₀
  have h := hM (mem_image_of_mem (scaleLik z)
    (show (c, bestK z c) ∈ shapeDomain from ⟨hc, inv_pos.mpr (G_pos hn z c)⟩))
  rw [profile_at hn z hc] at h
  have hg : G z c = softplus (c * z₀) := by
    change avg (fun _ : Fin n ↦ softplus (c * z₀)) = _
    exact avg_const hn _
  have hr : R z c = softplus (c * z₀) - c * z₀ := by
    change avg (fun _ : Fin n ↦ softplus (-(c * z₀))) = _
    rw [avg_const hn, softplus_shift (c * z₀)]
    ring
  have ht : softplus (c * z₀) ≤ 1 :=
    (softplus_le_exp _).trans (exp_le_one_iff.mpr (mul_nonpos_of_nonneg_of_nonpos hc.le hz₀))
  have hl := log_le_log (softplus_pos _) (softplus_le_exp (c * z₀))
  rw [log_exp] at hl
  unfold scaleProfile at h
  rw [hg, hr, log_div (softplus_pos _).ne' hc.ne'] at h
  change -1 - (log (softplus (c * z₀)) - log (exp (M + 3))) -
    (softplus (c * z₀) - c * z₀) ≤ M at h
  rw [log_exp] at h
  linarith

private theorem avg_sub (a b : Fin n → ℝ) :
    avg (fun i ↦ a i - b i) = avg a - avg b := by simp [avg, Finset.sum_sub_distrib, sub_div]

def logLik (x : Fin n → ℝ) (k c s : ℝ) : ℝ := avg (fun i ↦ logDensity k c s (x i))

/-- Holding the scale fixes the log ratios and leaves a data-only Jacobian term. -/
theorem logLik_eq_scale (hn : 0 < n) (x : Fin n → ℝ) (k c s : ℝ) :
    logLik x k c s = scaleLik (logRatios x s) (c, k) - avg (fun i ↦ log (x i)) := by
  have h (i : Fin n) :
      logDensity k c s (x i) =
        (log k + log c - log (x i)) -
          (k * softplus (c * logRatios x s i) + softplus (-(c * logRatios x s i))) := by
    have ht := softplus_shift (c * logRatios x s i)
    change log k + log c - log (x i) + c * logRatios x s i -
      (k + 1) * softplus (c * logRatios x s i) = _
    linarith
  unfold logLik
  simp_rw [h]
  rw [avg_sub, avg_sub, avg_const hn, avg_add, avg_mul]
  unfold scaleLik G R
  dsimp
  ring

/-- The change of variable `b = -c * log s` preserves the held-first-shape likelihood
up to the mean log observation. -/
theorem logLik_eq_first (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (k c : ℝ) :
    logLik x k c s = firstLik (fun i ↦ log (x i)) k (c, -(c * log s)) -
      avg (fun i ↦ log (x i)) := by
  have h (i : Fin n) : c * logRatios x s i = c * log (x i) + -(c * log s) := by
    rw [logRatios, log_div (hx i).ne' hs.ne']
    ring
  rw [logLik_eq_scale hn]
  unfold firstLik scaleLik G R
  dsimp
  simp_rw [h, avg_add, avg_mul]
  ring

/-- For positive power every real intercept comes from a unique positive scale. -/
theorem first_intercept_iff {c s b : ℝ} (hc : 0 < c) (hs : 0 < s) :
    -(c * log s) = b ↔ s = exp (-b / c) := by
  constructor
  · intro h
    rw [← exp_log hs]
    congr 1
    apply (eq_div_iff hc.ne').2
    linarith
  · intro h
    rw [h, log_exp]
    field_simp

end
end UnivariateML.BurrHeld
