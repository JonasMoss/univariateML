module

public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Data.Finset.Max
public import Mathlib.Order.Filter.Extr

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Endpoint likelihoods

Uniform uses closed support at both endpoints. Pareto uses `[b, ∞)` and
power uses `(0, a]`. The moving endpoints must be included for the
all-free and held-shape estimates to be attained. The open-support
variants below assign density zero at the excluded boundary.

## Deviations

These are exact-real, uncensored likelihoods for nonempty samples.
The Pareto and power data are strictly positive. Options encode the
held masks. The common Pareto/power pair is ordered (endpoint, shape),
so Pareto's coordinates are `(b, α)` and power's are `(a, b)`.
Both-held compatible cases are likelihood evaluations on singletons.
The uniform collapse theorem requires support compatibility explicitly.
For Pareto and power the unbounded case covers a free shape at the common
observation in both endpoint masks, matching the dossiers' existence prose.
-/

@[expose] public section

open Set Real

namespace UnivariateML.Endpoint

noncomputable section

variable {n : ℕ}

/-- The minimum of a nonempty sample. -/
def sampleMin (hn : 0 < n) (x : Fin n → ℝ) : ℝ :=
  letI : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  (Finset.univ.image x).min' (Finset.univ_nonempty.image x)

/-- The maximum of a nonempty sample. -/
def sampleMax (hn : 0 < n) (x : Fin n → ℝ) : ℝ :=
  letI : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  (Finset.univ.image x).max' (Finset.univ_nonempty.image x)

private theorem min_spec (hn : 0 < n) (x : Fin n → ℝ) :
    (∀ i, sampleMin hn x ≤ x i) ∧ ∃ i, x i = sampleMin hn x := by
  exact ⟨fun i ↦ Finset.min'_le _ _ (Finset.mem_image.mpr ⟨i, Finset.mem_univ _, rfl⟩),
    (Finset.mem_image.mp (Finset.min'_mem _ _)).imp fun _ h ↦ h.2⟩

private theorem max_spec (hn : 0 < n) (x : Fin n → ℝ) :
    (∀ i, x i ≤ sampleMax hn x) ∧ ∃ i, x i = sampleMax hn x := by
  exact ⟨fun i ↦ Finset.le_max' _ _ (Finset.mem_image.mpr ⟨i, Finset.mem_univ _, rfl⟩),
    (Finset.mem_image.mp (Finset.max'_mem _ _)).imp fun _ h ↦ h.2⟩

private theorem extrema_lt_iff (hn : 0 < n) (x : Fin n → ℝ) :
    sampleMin hn x < sampleMax hn x ↔ ∃ i j, x i ≠ x j := by
  obtain ⟨i, hi⟩ := (min_spec hn x).2
  obtain ⟨j, hj⟩ := (max_spec hn x).2
  refine ⟨fun h ↦ ⟨i, j, by rw [hi, hj]; exact h.ne⟩, ?_⟩
  rintro ⟨i, j, hij⟩
  rcases lt_or_gt_of_ne hij with h | h
  · exact ((min_spec hn x).1 i).trans_lt (h.trans_le ((max_spec hn x).1 j))
  · exact ((min_spec hn x).1 j).trans_lt (h.trans_le ((max_spec hn x).1 i))

/-- Convert a log-likelihood on its support to a likelihood with zero outside. -/
def likelihoodOn {P : Type*} (S : Set P) (f : P → ℝ) (p : P) : ℝ := by
  classical
  exact if p ∈ S then exp (f p) else 0

private theorem strict_max {P : Type*} {D S : Set P} {f : P → ℝ} {p : P}
    (hp : p ∈ D) (hs : p ∈ S)
    (h : ∀ q ∈ D, q ∈ S → q ≠ p → f q < f p) :
    (p ∈ D ∧ IsMaxOn (likelihoodOn S f) D p) ∧
      ∃! q, q ∈ D ∧ IsMaxOn (likelihoodOn S f) D q := by
  have hlt : ∀ q ∈ D, q ≠ p → likelihoodOn S f q < likelihoodOn S f p := by
    intro q hq hne
    by_cases hqs : q ∈ S
    · simpa [likelihoodOn, hs, hqs] using h q hq hqs hne
    · simp [likelihoodOn, hs, hqs, exp_pos]
  have hm : IsMaxOn (likelihoodOn S f) D p := by
    intro q hq
    by_cases hqp : q = p
    · simp [hqp]
    · exact (hlt q hq hqp).le
  exact ⟨⟨hp, hm⟩, ⟨p, ⟨hp, hm⟩, fun q hq ↦ by
    by_contra hne
    exact (hlt q hq.1 hne).not_ge (hq.2 hp)⟩⟩

private theorem unbounded_of_log {P : Type*} {D S : Set P} {f : P → ℝ}
    (h : ∀ M : ℝ, ∃ p ∈ D, p ∈ S ∧ M < f p) :
    ¬ BddAbove (likelihoodOn S f '' D) := by
  rintro ⟨M, hM⟩
  obtain ⟨p, hp, hs, hlt⟩ := h (log (max M 1))
  have hpos : 0 < max M 1 := lt_of_lt_of_le zero_lt_one (le_max_right _ _)
  have he := exp_lt_exp.mpr hlt
  rw [exp_log hpos] at he
  have hb := hM (mem_image_of_mem (likelihoodOn S f) hp)
  rw [likelihoodOn, ite_eq_left hs] at hb
  exact (he.trans_le hb).not_ge (le_max_left _ _)

namespace Uniform

/-- Valid endpoints with the requested holds. -/
def domain (lower upper : Option ℝ) : Set (ℝ × ℝ) :=
  {p | p.1 < p.2 ∧ lower.getD p.1 = p.1 ∧ upper.getD p.2 = p.2}

/-- All observations lie between the endpoints. -/
def support (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {p | ∀ i, p.1 ≤ x i ∧ x i ≤ p.2}

def logLik (n : ℕ) (p : ℝ × ℝ) : ℝ :=
  -(n : ℝ) * log (p.2 - p.1)

def likelihood (x : Fin n → ℝ) : ℝ × ℝ → ℝ :=
  likelihoodOn (support x) (logLik n)

/-- Each free endpoint is replaced by the corresponding sample extreme. -/
def fit (hn : 0 < n) (x : Fin n → ℝ) (lower upper : Option ℝ) : ℝ × ℝ :=
  (lower.getD (sampleMin hn x), upper.getD (sampleMax hn x))

private theorem width_order (hn : 0 < n) {x : Fin n → ℝ} {lower upper : Option ℝ}
    {q : ℝ × ℝ} (hq : q ∈ domain lower upper) (hs : q ∈ support x) :
    q.1 ≤ (fit hn x lower upper).1 ∧ (fit hn x lower upper).2 ≤ q.2 := by
  obtain ⟨i, hi⟩ := (min_spec hn x).2
  obtain ⟨j, hj⟩ := (max_spec hn x).2
  constructor
  · cases lower with
    | none => exact (hs i).1.trans_eq hi
    | some a => exact (show a = q.1 from hq.2.1).ge
  · cases upper with
    | none => exact hj.symm.trans_le (hs j).2
    | some b => exact (show b = q.2 from hq.2.2).le

/-- A compatible positive-width candidate is the unique MLE for its hold mask. -/
theorem mle (hn : 0 < n) {x : Fin n → ℝ} {lower upper : Option ℝ}
    (hwidth : (fit hn x lower upper).1 < (fit hn x lower upper).2)
    (hs : fit hn x lower upper ∈ support x) :
    (fit hn x lower upper ∈ domain lower upper ∧
      IsMaxOn (likelihood x) (domain lower upper) (fit hn x lower upper)) ∧
      ∃! p, p ∈ domain lower upper ∧ IsMaxOn (likelihood x) (domain lower upper) p := by
  have hp : fit hn x lower upper ∈ domain lower upper := by
    refine ⟨hwidth, ?_, ?_⟩
    · cases lower <;> rfl
    · cases upper <;> rfl
  refine strict_max hp hs ?_
  intro q hq hqs hne
  obtain ⟨ha, hb⟩ := width_order hn hq hqs
  have hqwidth : q.1 < q.2 := hq.1
  have hw : (fit hn x lower upper).2 - (fit hn x lower upper).1 < q.2 - q.1 := by
    by_contra! h
    exact hne (Prod.ext (by linarith) (by linarith))
  have hlog := log_lt_log (sub_pos.mpr hwidth) hw
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  exact mul_lt_mul_of_neg_left hlog (neg_neg_of_pos hN)

/-- In the all-free mask, nonconstant data give the minimum-maximum fit. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ domain none none ∧ IsMaxOn (likelihood x) (domain none none) p :=
  (mle hn (lower := none) (upper := none) ((extrema_lt_iff hn x).mpr hne)
    (fun i ↦ ⟨(min_spec hn x).1 i, (max_spec hn x).1 i⟩)).2

/-- With the lower endpoint held, the upper endpoint is the sample maximum. -/
theorem heldLower (hn : 0 < n) {x : Fin n → ℝ} {a : ℝ}
    (hs : a ≤ sampleMin hn x) (hw : a < sampleMax hn x) :
    ∃! p, p ∈ domain (some a) none ∧
      IsMaxOn (likelihood x) (domain (some a) none) p :=
  (mle hn (lower := some a) (upper := none) hw
    (fun i ↦ ⟨hs.trans ((min_spec hn x).1 i), (max_spec hn x).1 i⟩)).2

/-- With the upper endpoint held, the lower endpoint is the sample minimum. -/
theorem heldUpper (hn : 0 < n) {x : Fin n → ℝ} {b : ℝ}
    (hs : sampleMax hn x ≤ b) (hw : sampleMin hn x < b) :
    ∃! p, p ∈ domain none (some b) ∧
      IsMaxOn (likelihood x) (domain none (some b)) p :=
  (mle hn (lower := none) (upper := some b) hw
    (fun i ↦ ⟨(min_spec hn x).1 i, ((max_spec hn x).1 i).trans hs⟩)).2

/-- Holding both valid endpoints leaves only likelihood evaluation at that interval. -/
theorem bothHeld {a b : ℝ} (h : a < b) : domain (some a) (some b) = {(a, b)} := by
  ext p
  change (p.1 < p.2 ∧ a = p.1 ∧ b = p.2) ↔ p = (a, b)
  constructor
  · intro hp
    exact Prod.ext hp.2.1.symm hp.2.2.symm
  · rintro rfl
    exact ⟨h, rfl, rfl⟩

/-- An observation outside a held endpoint makes every feasible likelihood zero. -/
theorem incompatible_zero {x : Fin n → ℝ} {lower upper : Option ℝ}
    (h : (∃ a i, lower = some a ∧ x i < a) ∨ (∃ b i, upper = some b ∧ b < x i))
    {p : ℝ × ℝ} (hp : p ∈ domain lower upper) : likelihood x p = 0 := by
  apply ite_eq_right
  intro hs
  rcases h with ⟨a, i, he, hi⟩ | ⟨b, i, he, hi⟩
  · have ha : a = p.1 := by simpa [he] using hp.2.1
    rw [ha] at hi
    exact hi.not_ge (hs i).1
  · have hb : b = p.2 := by simpa [he] using hp.2.2
    rw [hb] at hi
    exact hi.not_ge (hs i).2

/-- A compatible zero-width candidate collapses if at least one endpoint can move. -/
theorem collapse_unbounded (hn : 0 < n) {x : Fin n → ℝ} {lower upper : Option ℝ}
    (hfree : lower = none ∨ upper = none)
    (heq : (fit hn x lower upper).1 = (fit hn x lower upper).2)
    (hs : fit hn x lower upper ∈ support x) :
    ¬ BddAbove (likelihood x '' domain lower upper) := by
  apply unbounded_of_log
  intro M
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let c := (fit hn x lower upper).1
  let w := exp (-(M + 1) / n)
  have hw : 0 < w := exp_pos _
  have hx (i : Fin n) : x i = c := le_antisymm ((hs i).2.trans_eq heq.symm) (hs i).1
  have hlog : M < -(n : ℝ) * log w := by
    dsimp only [w]
    rw [log_exp, ← mul_div_assoc, neg_mul_neg, mul_div_cancel_left₀ _ hN.ne']
    linarith
  rcases hfree with h | h
  · refine ⟨(c - w, c), ⟨by dsimp; linarith, by simp [h], ?_⟩, ?_, ?_⟩
    · rw [show c = (fit hn x lower upper).2 from heq]
      cases upper <;> rfl
    · intro i
      rw [hx i]
      exact ⟨by linarith, le_rfl⟩
    · simpa [logLik, sub_sub_cancel] using hlog
  · refine ⟨(c, c + w), ⟨by dsimp; linarith, ?_, by simp [h]⟩, ?_, ?_⟩
    · cases lower <;> rfl
    · intro i
      rw [hx i]
      exact ⟨le_rfl, by linarith⟩
    · simpa [logLik] using hlog

/-- Constant samples are unbounded when both endpoints are free. -/
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} {c : ℝ} (hx : ∀ i, x i = c) :
    ¬ BddAbove (likelihood x '' domain none none) := by
  obtain ⟨i, hi⟩ := (min_spec hn x).2
  obtain ⟨j, hj⟩ := (max_spec hn x).2
  exact collapse_unbounded hn (Or.inl rfl)
    (hi.symm.trans ((hx i).trans ((hx j).symm.trans hj)))
    (fun k ↦ ⟨(min_spec hn x).1 k, (max_spec hn x).1 k⟩)

/-- Hypothetical uniform likelihood with both endpoints excluded. -/
def openLikelihood (x : Fin n → ℝ) : ℝ × ℝ → ℝ :=
  likelihoodOn {p | ∀ i, p.1 < x i ∧ x i < p.2} (logLik n)

/-- An open uniform interval cannot maximize the likelihood with both endpoints free. -/
theorem open_no_mle (hn : 0 < n) (x : Fin n → ℝ) :
    ¬ ∃ p, p ∈ domain none none ∧ IsMaxOn (openLikelihood x) (domain none none) p := by
  rintro ⟨p, hp, hm⟩
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  obtain ⟨i, hi⟩ := (min_spec hn x).2
  let S : Set (ℝ × ℝ) := {p | ∀ i, p.1 < x i ∧ x i < p.2}
  change IsMaxOn (likelihoodOn S (logLik n)) (domain none none) p at hm
  by_cases hs : p ∈ S
  · let q := ((p.1 + sampleMin hn x) / 2, p.2)
    have hmin := (hs i).1.trans_eq hi
    have hqS : q ∈ S := fun j ↦
      ⟨by dsimp [q]; linarith [(min_spec hn x).1 j], (hs j).2⟩
    have hq : q ∈ domain none none := ⟨(hqS i).1.trans (hqS i).2, rfl, rfl⟩
    have hwidth : q.2 - q.1 < p.2 - p.1 := by dsimp [q]; linarith
    have hl := log_lt_log (sub_pos.mpr hq.1) hwidth
    have hlt : logLik n p < logLik n q := mul_lt_mul_of_neg_left hl (neg_neg_of_pos hN)
    exact (show likelihoodOn S (logLik n) p < likelihoodOn S (logLik n) q by
      simpa [likelihoodOn, hs, hqS] using hlt).not_ge (hm hq)
  · let q := (sampleMin hn x - 1, sampleMax hn x + 1)
    have hqS : q ∈ S := fun j ↦
      ⟨by dsimp [q]; linarith [(min_spec hn x).1 j],
       by dsimp [q]; linarith [(max_spec hn x).1 j]⟩
    have hq : q ∈ domain none none := ⟨(hqS i).1.trans (hqS i).2, rfl, rfl⟩
    exact (show likelihoodOn S (logLik n) p < likelihoodOn S (logLik n) q by
      simp [likelihoodOn, hs, hqS, exp_pos]).not_ge (hm hq)

end Uniform

namespace ParetoPower

/-- The choice of lower-endpoint Pareto or upper-endpoint power law. -/
inductive Family
  | pareto
  | power
  deriving DecidableEq

/-- Orient both families so that every sample value is at or above the endpoint. -/
def Family.orient (family : Family) (t : ℝ) : ℝ :=
  match family with
  | .pareto => t
  | .power => -t

/-- The best free endpoint is the sample minimum for Pareto and maximum for power. -/
def Family.extreme (family : Family) (hn : 0 < n) (x : Fin n → ℝ) : ℝ :=
  match family with
  | .pareto => sampleMin hn x
  | .power => sampleMax hn x

/-- Positive parameters with the requested holds, in (endpoint, shape) order. -/
def domain (endpoint shape : Option ℝ) : Set (ℝ × ℝ) :=
  {p | 0 < p.1 ∧ 0 < p.2 ∧ endpoint.getD p.1 = p.1 ∧ shape.getD p.2 = p.2}

/-- The entire sample lies in the support, with the moving endpoint included. -/
def support (family : Family) (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {p | ∀ i, 0 < x i ∧ family.orient p.1 ≤ family.orient (x i)}

/-- The sum of log ratios: observation/endpoint for Pareto, endpoint/observation for power. -/
def gap (family : Family) (x : Fin n → ℝ) (e : ℝ) : ℝ :=
  ∑ i, (family.orient (log (x i)) - family.orient (log e))

/-- The Pareto gap in the dossier's ratio form. -/
theorem pareto_gap {x : Fin n → ℝ} {b : ℝ} (hx : ∀ i, 0 < x i) (hb : 0 < b) :
    gap .pareto x b = ∑ i, log (x i / b) := by
  unfold gap
  exact Finset.sum_congr rfl fun i _ ↦ (log_div (hx i).ne' hb.ne').symm

/-- The power gap in the dossier's ratio form. -/
theorem power_gap {x : Fin n → ℝ} {a : ℝ} (hx : ∀ i, 0 < x i) (ha : 0 < a) :
    gap .power x a = ∑ i, log (a / x i) := by
  unfold gap
  refine Finset.sum_congr rfl fun i _ ↦ ?_
  rw [log_div ha.ne' (hx i).ne']
  dsimp [Family.orient]
  ring

def logLik (family : Family) (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  n * log p.2 - p.2 * gap family x p.1 - ∑ i, log (x i)

def likelihood (family : Family) (x : Fin n → ℝ) : ℝ × ℝ → ℝ :=
  likelihoodOn (support family x) (logLik family x)

/-- Use the sample extreme when the endpoint is free, and `n / gap` when shape is free. -/
def fit (family : Family) (hn : 0 < n) (x : Fin n → ℝ)
    (endpoint shape : Option ℝ) : ℝ × ℝ :=
  let e := endpoint.getD (family.extreme hn x)
  (e, shape.getD (n / gap family x e))

private theorem extreme_spec (family : Family) (hn : 0 < n) (x : Fin n → ℝ) :
    (∀ i, family.orient (family.extreme hn x) ≤ family.orient (x i)) ∧
      ∃ i, x i = family.extreme hn x := by
  cases family
  · exact min_spec hn x
  · simpa [Family.extreme, Family.orient] using max_spec hn x

private theorem orient_log_lt (family : Family) {e t : ℝ} (he : 0 < e) (ht : 0 < t)
    (h : family.orient e < family.orient t) :
    family.orient (log e) < family.orient (log t) := by
  cases family with
  | pareto => exact log_lt_log he h
  | power => exact neg_lt_neg (log_lt_log ht (neg_lt_neg_iff.mp h))

private theorem orient_inj (family : Family) : Function.Injective family.orient := by
  cases family <;> simp [Family.orient, Function.Injective]

private theorem orient_log_le (family : Family) {e t : ℝ} (he : 0 < e) (ht : 0 < t)
    (h : family.orient e ≤ family.orient t) :
    family.orient (log e) ≤ family.orient (log t) := by
  rcases h.lt_or_eq with hlt | heq
  · exact (orient_log_lt family he ht hlt).le
  · rw [orient_inj family heq]

/-- For a compatible positive endpoint, the gap is positive exactly when some observation
differs from the endpoint. -/
theorem gap_pos_iff (family : Family) {x : Fin n → ℝ} {e a : ℝ} (he : 0 < e)
    (hs : (e, a) ∈ support family x) : 0 < gap family x e ↔ ∃ i, x i ≠ e := by
  classical
  have hnon (i : Fin n) :
      0 ≤ family.orient (log (x i)) - family.orient (log e) :=
    sub_nonneg.mpr (orient_log_le family he (hs i).1 (hs i).2)
  constructor
  · contrapose!
    intro h
    simp [gap, h]
  · rintro ⟨i, hi⟩
    have hlt := lt_of_le_of_ne (hs i).2 (fun h ↦ hi (orient_inj family h.symm))
    exact Finset.sum_pos' (fun j _ ↦ hnon j) ⟨i, Finset.mem_univ _,
      sub_pos.mpr (orient_log_lt family he (hs i).1 hlt)⟩

private theorem shape_strict {N S a : ℝ} (hN : 0 < N) (hS : 0 < S) (ha : 0 < a)
    (hne : a ≠ N / S) :
    N * log a - a * S < N * log (N / S) - (N / S) * S := by
  have hopt : 0 < N / S := div_pos hN hS
  have hratio : a / (N / S) ≠ 1 := by
    intro h
    apply hne
    exact (div_eq_one_iff_eq hopt.ne').mp h
  have h := log_lt_sub_one_of_pos (div_pos ha hopt) hratio
  rw [log_div ha.ne' hopt.ne', div_div_eq_mul_div] at h
  have h' := mul_lt_mul_of_pos_left h hN
  rw [mul_sub _ (a * S / N) 1, mul_div_cancel₀ _ hN.ne', mul_one] at h'
  rw [div_mul_cancel₀ _ hS.ne']
  linarith

private theorem endpoint_strict (family : Family) (hn : 0 < n) {x : Fin n → ℝ}
    {e t a : ℝ} (he : 0 < e) (ht : 0 < t) (ha : 0 < a)
    (het : family.orient e < family.orient t) :
    logLik family x (e, a) < logLik family x (t, a) := by
  have hlog := orient_log_lt family he ht het
  have hgap : gap family x t < gap family x e := by
    simp only [gap, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul]
    have hN : (0 : ℝ) < n := by exact_mod_cast hn
    linarith [mul_lt_mul_of_pos_left hlog hN]
  exact sub_lt_sub_right (sub_lt_sub_left (mul_lt_mul_of_pos_left hgap ha) _) _

/-- Under any hold mask, a compatible candidate with positive endpoint and admissible
shape is the unique MLE. A free shape requires a positive gap. -/
theorem mle (family : Family) (hn : 0 < n) {x : Fin n → ℝ} {endpoint shape : Option ℝ}
    (he : 0 < (fit family hn x endpoint shape).1)
    (hs : fit family hn x endpoint shape ∈ support family x)
    (ha : match shape with
      | some a => 0 < a
      | none => 0 < gap family x (fit family hn x endpoint shape).1) :
    (fit family hn x endpoint shape ∈ domain endpoint shape ∧
      IsMaxOn (likelihood family x) (domain endpoint shape)
        (fit family hn x endpoint shape)) ∧
      ∃! p, p ∈ domain endpoint shape ∧
        IsMaxOn (likelihood family x) (domain endpoint shape) p := by
  let p := fit family hn x endpoint shape
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hp : p ∈ domain endpoint shape := by
    refine ⟨he, ?_, ?_, ?_⟩
    · cases shape with
      | none => exact div_pos hN ha
      | some _ => exact ha
    · cases endpoint <;> rfl
    · cases shape <;> rfl
  refine strict_max hp hs ?_
  intro q hq hqs hne
  have hqE : 0 < q.1 := hq.1
  have hqA : 0 < q.2 := hq.2.1
  have hE : family.orient q.1 ≤ family.orient p.1 := by
    cases endpoint with
    | none =>
      obtain ⟨i, hi⟩ := (extreme_spec family hn x).2
      change family.orient q.1 ≤ family.orient (family.extreme hn x)
      simpa only [hi] using (hqs i).2
    | some e =>
      have hqe : e = q.1 := hq.2.2.1
      change family.orient q.1 ≤ family.orient e
      rw [hqe]
  have shape_comparison (h : q.2 ≠ p.2) :
      logLik family x (p.1, q.2) < logLik family x p := by
    cases shape with
    | some a => exact (h (show a = q.2 from hq.2.2.2).symm).elim
    | none => exact sub_lt_sub_right (shape_strict hN ha hqA h) _
  have hshape : logLik family x (p.1, q.2) ≤ logLik family x p := by
    by_cases h : q.2 = p.2
    · rw [h, Prod.eta]
    · exact (shape_comparison h).le
  rcases hE.lt_or_eq with hlt | heq
  · exact (endpoint_strict family hn hqE he hqA hlt).trans_le hshape
  · have h := orient_inj family heq
    exact (show logLik family x (p.1, q.2) = logLik family x q by rw [← h, Prod.eta]) ▸
      shape_comparison (fun ha' ↦ hne (Prod.ext h ha'))

/-- With both parameters free, either family has a unique MLE on nonconstant data. -/
theorem jointMLE (family : Family) (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ domain none none ∧ IsMaxOn (likelihood family x) (domain none none) p := by
  let p := fit family hn x none none
  have hs : p ∈ support family x := fun i ↦ ⟨hx i, (extreme_spec family hn x).1 i⟩
  obtain ⟨i, hi⟩ := (extreme_spec family hn x).2
  have he : 0 < p.1 := (hx i).trans_eq hi
  have hg := (gap_pos_iff family he hs).mpr (by
    by_contra! h
    obtain ⟨j, k, hjk⟩ := hne
    exact hjk ((h j).trans (h k).symm))
  exact (mle family hn he hs hg).2

/-- Holding shape leaves the sample extreme as the unique endpoint fit, including constant data. -/
theorem heldShape (family : Family) (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃! p, p ∈ domain none (some a) ∧
      IsMaxOn (likelihood family x) (domain none (some a)) p := by
  obtain ⟨i, hi⟩ := (extreme_spec family hn x).2
  exact (mle family hn (endpoint := none) (shape := some a) ((hx i).trans_eq hi)
    (fun j ↦ ⟨hx j, (extreme_spec family hn x).1 j⟩) ha).2

/-- Holding a compatible endpoint gives the unique shape fit unless every observation equals it. -/
theorem heldEndpoint (family : Family) (hn : 0 < n) {x : Fin n → ℝ} {e : ℝ}
    (he : 0 < e) (hs : (e, 0) ∈ support family x) (hne : ∃ i, x i ≠ e) :
    ∃! p, p ∈ domain (some e) none ∧
      IsMaxOn (likelihood family x) (domain (some e) none) p := by
  have hs' : fit family hn x (some e) none ∈ support family x := hs
  exact (mle family hn (endpoint := some e) (shape := none) he hs'
    ((gap_pos_iff family he hs').mpr hne)).2

/-- Holding both positive coefficients leaves only likelihood evaluation at that pair. -/
theorem bothHeld {e a : ℝ} (he : 0 < e) (ha : 0 < a) :
    domain (some e) (some a) = {(e, a)} := by
  ext p
  change (0 < p.1 ∧ 0 < p.2 ∧ e = p.1 ∧ a = p.2) ↔ p = (e, a)
  constructor
  · intro hp
    exact Prod.ext hp.2.2.1.symm hp.2.2.2.symm
  · rintro rfl
    exact ⟨he, ha, rfl, rfl⟩

/-- A held endpoint on the wrong side of any observation forces zero likelihood. -/
theorem incompatible_zero (family : Family) {x : Fin n → ℝ} {e : ℝ} {shape : Option ℝ}
    (h : ∃ i, family.orient (x i) < family.orient e) {p : ℝ × ℝ}
    (hp : p ∈ domain (some e) shape) : likelihood family x p = 0 := by
  apply ite_eq_right
  intro hs
  obtain ⟨i, hi⟩ := h
  have he : e = p.1 := hp.2.2.1
  rw [he] at hi
  exact hi.not_ge (hs i).2

/-- For a free shape, a compatible endpoint equal to every observation gives unbounded
likelihood along that endpoint. This covers held endpoints and the all-free fit. -/
theorem constant_unbounded (family : Family) (hn : 0 < n) {x : Fin n → ℝ}
    {endpoint : Option ℝ} {c : ℝ} (hc : 0 < c) (he : endpoint.getD c = c)
    (hx : ∀ i, x i = c) :
    ¬ BddAbove (likelihood family x '' domain endpoint none) := by
  apply unbounded_of_log
  intro M
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let t := exp ((M + (∑ i, log (x i)) + 1) / n)
  refine ⟨(c, t), ⟨hc, exp_pos _, he, rfl⟩, ?_, ?_⟩
  · intro i
    rw [hx i]
    exact ⟨hc, le_rfl⟩
  · simp only [logLik, gap, hx, sub_self, Finset.sum_const_zero, mul_zero,
      sub_zero, t, log_exp, mul_div_cancel₀ _ hN.ne']
    have hsum : (∑ i, log (x i)) = ∑ _i : Fin n, log c := by simp [hx]
    linarith

/-- Hypothetical Pareto or power likelihood with the moving endpoint excluded. -/
def openLikelihood (family : Family) (x : Fin n → ℝ) : ℝ × ℝ → ℝ :=
  likelihoodOn {p | ∀ i, 0 < x i ∧ family.orient p.1 < family.orient (x i)}
    (logLik family x)

private theorem orient_mid (family : Family) (u v : ℝ) :
    family.orient ((u + v) / 2) = (family.orient u + family.orient v) / 2 := by
  cases family with
  | pareto => rfl
  | power =>
    dsimp [Family.orient]
    ring

/-- Excluding the moving endpoint destroys attainment when the endpoint is free. -/
theorem open_no_mle (family : Family) (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {shape : Option ℝ} :
    ¬ ∃ p, p ∈ domain none shape ∧ IsMaxOn (openLikelihood family x) (domain none shape) p := by
  rintro ⟨p, hp, hm⟩
  let e := family.extreme hn x
  obtain ⟨i, hi⟩ := (extreme_spec family hn x).2
  have he : 0 < e := (hx i).trans_eq hi
  let S : Set (ℝ × ℝ) := {q | ∀ i, 0 < x i ∧ family.orient q.1 < family.orient (x i)}
  change IsMaxOn (likelihoodOn S (logLik family x)) (domain none shape) p at hm
  by_cases hs : p ∈ S
  · let q := ((p.1 + e) / 2, p.2)
    have hpe : family.orient p.1 < family.orient e := by
      simpa only [e, ← hi] using (hs i).2
    have hmid : family.orient p.1 < family.orient q.1 ∧
        family.orient q.1 < family.orient e := by
      change _ < family.orient ((p.1 + e) / 2) ∧ _
      simp only [q, orient_mid]
      constructor <;> linarith
    have hqS : q ∈ S := fun j ↦ ⟨hx j, hmid.2.trans_le ((extreme_spec family hn x).1 j)⟩
    have hq : q ∈ domain none shape :=
      ⟨by dsimp [q]; linarith [hp.1], hp.2.1, rfl, hp.2.2.2⟩
    have hl : logLik family x p < logLik family x q := by
      simpa only [q, Prod.eta] using endpoint_strict family hn hp.1 hq.1 hp.2.1 hmid.1
    exact (show likelihoodOn S (logLik family x) p < likelihoodOn S (logLik family x) q by
      simpa [likelihoodOn, hs, hqS] using hl).not_ge (hm hq)
  · obtain ⟨t, ht, hte⟩ : ∃ t > 0, family.orient t < family.orient e := by
      cases family with
      | pareto => exact ⟨e / 2, by linarith, by change e / 2 < e; linarith⟩
      | power => exact ⟨e + 1, by linarith, by change -(e + 1) < -e; linarith⟩
    let q := (t, p.2)
    have hq : q ∈ domain none shape := ⟨ht, hp.2.1, rfl, hp.2.2.2⟩
    have hqS : q ∈ S := fun j ↦ ⟨hx j, hte.trans_le ((extreme_spec family hn x).1 j)⟩
    exact (show likelihoodOn S (logLik family x) p < likelihoodOn S (logLik family x) q by
      simp [likelihoodOn, hs, hqS, exp_pos]).not_ge (hm hq)

end ParetoPower

end

end UnivariateML.Endpoint
