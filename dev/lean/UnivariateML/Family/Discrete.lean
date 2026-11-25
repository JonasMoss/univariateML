module

public import UnivariateML.Generic.Score
public import UnivariateML.Generic.Limit
public import Mathlib.Analysis.Calculus.Deriv.Slope
public import Mathlib.Analysis.SpecialFunctions.ExpDeriv
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Discrete likelihoods

The objectives are per observation and omit parameter-independent constants.
The sample is encoded by natural counts; the reduced objectives use their real mean.
At incompatible endpoints the reduced likelihood is zero.

## Deviations

Zero-truncated Poisson has two domains in the library. The strictly positive
rate model has an unattained all-ones limit. The dedicated zero modifier
includes rate zero as a point mass at one.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Discrete

noncomputable section

/-- Arithmetic mean of the counts. -/
def avg {n : ℕ} (x : Fin n → ℕ) : ℝ := (∑ i, (x i : ℝ)) / n

theorem avg_eq_zero_iff {n : ℕ} (x : Fin n → ℕ) :
    avg x = 0 ↔ ∀ i, x i = 0 := by
  cases n with
  | zero => simp [avg]
  | succ n =>
    have hn : ((n + 1 : ℕ) : ℝ) ≠ 0 := by positivity
    simp only [avg, div_eq_zero_iff, hn, or_false]
    rw [Finset.sum_eq_zero_iff_of_nonneg (fun i _ ↦ Nat.cast_nonneg (x i))]
    simp

theorem avg_le {n m : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) : avg x ≤ m := by
  cases n with
  | zero => simp [avg]
  | succ n =>
    apply (div_le_iff₀ (by positivity : 0 < ((n + 1 : ℕ) : ℝ))).2
    simpa [mul_comm] using
      (Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦
        (Nat.cast_le.mpr (hx i) : (x i : ℝ) ≤ m)))

theorem avg_eq_of_lowerBound {n m : ℕ} (x : Fin n → ℕ) (hn : 0 < n)
    (hx : ∀ i, m ≤ x i) : avg x = m ↔ ∀ i, x i = m := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hs : (∑ i, ((x i : ℝ) - m)) = (∑ i, (x i : ℝ)) - (n : ℝ) * m := by
    simp [Finset.sum_sub_distrib]
  have hz := Finset.sum_eq_zero_iff_of_nonneg
    (s := (Finset.univ : Finset (Fin n)))
    (fun i _ ↦ sub_nonneg.mpr (Nat.cast_le.mpr (hx i) : (m : ℝ) ≤ x i))
  rw [avg, div_eq_iff hn'.ne']
  rw [hs, sub_eq_zero, mul_comm (n : ℝ) (m : ℝ)] at hz
  simpa [sub_eq_zero] using hz

private theorem le_avg {n m : ℕ} (x : Fin n → ℕ) (hn : 0 < n)
    (hx : ∀ i, m ≤ x i) : (m : ℝ) ≤ avg x := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  apply (le_div_iff₀ hn').mpr
  simpa [mul_comm] using
    (Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦
      (Nat.cast_le.mpr (hx i) : (m : ℝ) ≤ x i)))


private theorem log_ratio_lt {u v : ℝ} (hu : 0 < u) (hv : 0 < v) (hne : u ≠ v) :
    log u - log v < u / v - 1 := by
  rw [← log_div hu.ne' hv.ne']
  exact log_lt_sub_one_of_pos (div_pos hu hv) (by intro h; exact hne ((div_eq_one_iff_eq hv.ne').mp h))

namespace Poisson

def likelihood (a lam : ℝ) : ℝ :=
  if lam = 0 then (if a = 0 then 1 else 0) else exp (a * log lam - lam)

theorem held_positive_iff {a lam : ℝ} (hl : 0 ≤ lam) :
    0 < likelihood a lam ↔ 0 < lam ∨ a = 0 := by
  rcases hl.eq_or_lt with h | h
  · subst lam
    by_cases ha : a = 0 <;> simp [likelihood, ha]
  · simp [likelihood, h.ne', h, exp_pos]

theorem unique_mle {a : ℝ} (ha : 0 ≤ a) :
    ∀ lam ∈ Ici (0 : ℝ), IsMaxOn (likelihood a) (Ici 0) lam ↔ lam = a := by
  have hstrict : ∀ lam : ℝ, 0 ≤ lam → lam ≠ a → likelihood a lam < likelihood a a := by
    intro lam hl hn
    rcases ha.eq_or_lt with h | h
    · subst a
      have hp : 0 < lam := lt_of_le_of_ne hl (Ne.symm hn)
      simpa [likelihood, hn] using (exp_lt_exp.mpr (show -lam < 0 by linarith))
    · rcases hl.eq_or_lt with hz | hl
      · subst lam
        simp [likelihood, h.ne', exp_pos]
      · simp only [likelihood, ite_eq_right hl.ne', ite_eq_right h.ne', exp_lt_exp]
        have hh := mul_lt_mul_of_pos_left (log_ratio_lt hl h hn) h
        have he : a * (lam / a - 1) = lam - a := by field_simp
        rw [he] at hh
        linarith
  intro lam hl
  constructor
  · intro hm
    by_contra hn
    exact (hstrict lam hl hn).not_ge (hm ha)
  · rintro rfl y hy
    by_cases h : y = lam
    · simp [h]
    · exact (hstrict y hy h).le

end Poisson

namespace Binomial

/-- Two log terms, with zero exponents contributing zero even at a boundary. -/
def likelihood (s f p : ℝ) : ℝ :=
  if (p = 0 ∧ s ≠ 0) ∨ (p = 1 ∧ f ≠ 0) then 0
  else exp (s * log p + f * log (1 - p))

theorem held_positive_iff {s f p : ℝ} :
    0 < likelihood s f p ↔ (p = 0 → s = 0) ∧ (p = 1 → f = 0) := by
  unfold likelihood
  split_ifs with h <;> simp only [lt_self_iff_false, exp_pos] <;> tauto

theorem fixed_size_mle {s f : ℝ} (hs : 0 ≤ s) (hf : 0 ≤ f) (ht : 0 < s + f) :
    ∀ p ∈ Icc (0 : ℝ) 1, IsMaxOn (likelihood s f) (Icc 0 1) p ↔ p = s / (s + f) := by
  let q := s / (s + f)
  have hq : q ∈ Icc (0 : ℝ) 1 :=
    ⟨div_nonneg hs ht.le, (div_le_one ht).2 (by linarith)⟩
  have hcomp : 1 - q = f / (s + f) := by dsimp [q]; field_simp; ring
  have hstrict : ∀ p ∈ Icc (0 : ℝ) 1, p ≠ q → likelihood s f p < likelihood s f q := by
    intro p hp hn
    by_cases hb : (p = 0 ∧ s ≠ 0) ∨ (p = 1 ∧ f ≠ 0)
    · rw [likelihood, ite_eq_left hb]
      apply held_positive_iff.mpr
      constructor
      · intro h
        exact (div_eq_zero_iff.mp h).resolve_right ht.ne'
      · intro h
        apply (div_eq_zero_iff.mp (show f / (s + f) = 0 by rw [← hcomp, h]; ring)).resolve_right ht.ne'
    have hbq : ¬ ((q = 0 ∧ s ≠ 0) ∨ (q = 1 ∧ f ≠ 0)) := by
      rintro (⟨h, hs'⟩ | ⟨h, hf'⟩)
      · exact hs' ((div_eq_zero_iff.mp h).resolve_right ht.ne')
      · exact hf' ((div_eq_zero_iff.mp (show f / (s + f) = 0 by rw [← hcomp, h]; ring)).resolve_right ht.ne')
    simp only [likelihood, ite_eq_right hb, ite_eq_right hbq, exp_lt_exp]
    have term : ∀ (w u v : ℝ), 0 ≤ w → 0 ≤ u → u ≠ v →
        (u = 0 → w = 0) → v = w / (s + f) →
        w * (log u - log v) ≤ (s + f) * (u - v) ∧
          (0 < w → w * (log u - log v) < (s + f) * (u - v)) := by
      intro w u v hw hu huv huz hv
      rcases hw.eq_or_lt with h | h
      · have hv' : v = 0 := by rw [hv, ← h, zero_div]
        rw [← h, hv']
        simp [mul_nonneg ht.le hu]
      · have hu' : 0 < u := lt_of_le_of_ne hu (fun e ↦ h.ne' (huz e.symm))
        have hv' : 0 < v := hv ▸ div_pos h ht
        have hh := mul_lt_mul_of_pos_left (log_ratio_lt hu' hv' huv) h
        have he : w * (u / v - 1) = (s + f) * (u - v) := by
          rw [hv]; field_simp
        rw [he] at hh
        exact ⟨hh.le, fun _ ↦ hh⟩
    have h₁ := term s p q hs hp.1 hn (by aesop) rfl
    have h₂ := term f (1 - p) (1 - q) hf (by linarith [hp.2])
      (by intro h; apply hn; linarith) (by intro h; by_contra h'; exact hb (.inr ⟨by linarith, h'⟩)) hcomp
    by_cases h : 0 < s
    · linarith [h₁.2 h, h₂.1]
    · have h' : 0 < f := by linarith
      linarith [h₁.1, h₂.2 h']
  intro p hp
  constructor
  · intro hm
    by_contra hn
    exact (hstrict p hp hn).not_ge (hm hq)
  · rintro rfl y hy
    by_cases h : y = q
    · change likelihood s f y ≤ likelihood s f q
      rw [h]
    · exact (hstrict y hy h).le

theorem zero_size_mle (p : ℝ) :
    IsMaxOn (likelihood 0 0) (Icc 0 1) p := by
  intro y _
  simp [likelihood]

/-- Sample formulation of the held positive size estimate. -/
theorem sample_fixed_size_mle {n m : ℕ} (x : Fin n → ℕ) (hm : 0 < m)
    (hx : ∀ i, x i ≤ m) :
    ∀ p ∈ Icc (0 : ℝ) 1,
      IsMaxOn (likelihood (avg x) (m - avg x)) (Icc 0 1) p ↔ p = avg x / m := by
  have ha : 0 ≤ avg x := by unfold avg; positivity
  have ht : avg x + ((m : ℝ) - avg x) = m := by ring
  have hm' : (0 : ℝ) < m := by exact_mod_cast hm
  have hsum : 0 < avg x + ((m : ℝ) - avg x) := by simpa only [ht] using hm'
  simpa only [ht] using fixed_size_mle ha (sub_nonneg.mpr (avg_le x hx)) hsum

/-- Positive likelihood of a held pair, including both degenerate probabilities. -/
def compatible {n : ℕ} (x : Fin n → ℕ) (m : ℕ) (p : ℝ) : Prop :=
  (∀ i, x i ≤ m) ∧ (p = 0 → ∀ i, x i = 0) ∧ (p = 1 → ∀ i, x i = m)

theorem zero_size_compatible_iff {n : ℕ} (x : Fin n → ℕ) (p : ℝ) :
    compatible x 0 p ↔ ∀ i, x i = 0 := by
  simp [compatible]
  tauto

theorem zero_prob_compatible_iff {n : ℕ} (x : Fin n → ℕ) (m : ℕ) :
    compatible x m 0 ↔ ∀ i, x i = 0 := by
  constructor
  · exact fun h ↦ h.2.1 rfl
  · intro h
    simp [compatible, h]

theorem one_prob_compatible_iff {n : ℕ} (x : Fin n → ℕ) (m : ℕ) :
    compatible x m 1 ↔ ∀ i, x i = m := by
  constructor
  · exact fun h ↦ h.2.2 rfl
  · intro h
    simp [compatible, h]

/-- Log likelihood for varying size at a held interior probability. -/
def sizeLogLik {n : ℕ} (x : Fin n → ℕ) (p : ℝ) (m : ℕ) : ℝ :=
  ∑ i, (log (m.choose (x i)) + (x i : ℝ) * log p +
    ((m : ℝ) - x i) * log (1 - p))

def sizeIncrement {n : ℕ} (x : Fin n → ℕ) (p : ℝ) (m : ℕ) : ℝ :=
  ∑ i, (log (1 - p) - log (1 - (x i : ℝ) / ((m : ℝ) + 1)))

/-- Product of the binomial masses, including the degenerate endpoints. -/
def sampleLikelihood {n : ℕ} (x : Fin n → ℕ) (m : ℕ) (p : ℝ) : ℝ :=
  ∏ i, (m.choose (x i) : ℝ) * p ^ (x i) * (1 - p) ^ (m - x i)

theorem sample_likelihood_pos_iff {n m : ℕ} (x : Fin n → ℕ) {p : ℝ} (hp : p ∈ Icc 0 1) :
    0 < sampleLikelihood x m p ↔ compatible x m p := by
  have hp₀ : 0 ≤ p := hp.1
  have hp₁ : 0 ≤ 1 - p := sub_nonneg.mpr hp.2
  have factor_nonneg (i : Fin n) :
      0 ≤ (m.choose (x i) : ℝ) * p ^ (x i) * (1 - p) ^ (m - x i) := by positivity
  constructor
  · intro h
    have hf : ∀ i, 0 < (m.choose (x i) : ℝ) * p ^ (x i) * (1 - p) ^ (m - x i) := by
      intro i
      refine lt_of_le_of_ne (factor_nonneg i) (fun hi ↦ ?_)
      have he : sampleLikelihood x m p = 0 := Finset.prod_eq_zero (Finset.mem_univ i) hi.symm
      exact h.ne' he
    have hs : ∀ i, x i ≤ m := by
      intro i
      by_contra hi
      have hh := hf i
      rw [Nat.choose_eq_zero_of_lt (by omega), Nat.cast_zero, zero_mul, zero_mul] at hh
      exact (lt_irrefl _ hh)
    refine ⟨hs, ?_, ?_⟩
    · intro h₀ i
      by_contra hi
      have hh := hf i
      rw [h₀, zero_pow hi, mul_zero, zero_mul] at hh
      exact (lt_irrefl _ hh)
    · intro h₁ i
      by_contra hi
      have hk : m - x i ≠ 0 := by have := hs i; omega
      have hh := hf i
      rw [h₁, sub_self, zero_pow hk, mul_zero] at hh
      exact (lt_irrefl _ hh)
  · rintro ⟨hs, h₀, h₁⟩
    apply Finset.prod_pos
    intro i _
    have hc : 0 < (m.choose (x i) : ℝ) := by exact_mod_cast Nat.choose_pos (hs i)
    have ha : 0 < p ^ (x i) := by
      rcases hp₀.eq_or_lt with h | h
      · simp [h₀ h.symm i]
      · exact pow_pos h _
    have hb : 0 < (1 - p) ^ (m - x i) := by
      rcases hp₁.eq_or_lt with h | h
      · have he : p = 1 := by linarith
        simp [h₁ he i]
      · exact pow_pos h _
    exact mul_pos (mul_pos hc ha) hb

theorem endpoint_sample_likelihood {n m : ℕ} (x : Fin n → ℕ) {p : ℝ}
    (hp : p = 0 ∨ p = 1) (h : compatible x m p) : sampleLikelihood x m p = 1 := by
  apply Finset.prod_eq_one
  intro i _
  rcases hp with rfl | rfl
  · simp [h.2.1 rfl i]
  · simp [h.2.2 rfl i]

theorem sizeLogLik_eq_log {n m : ℕ} (x : Fin n → ℕ) {p : ℝ}
    (hp : p ∈ Ioo 0 1) (hx : ∀ i, x i ≤ m) :
    sizeLogLik x p m = log (sampleLikelihood x m p) := by
  have hp₀ : 0 < p := hp.1
  have hp₁ : 0 < 1 - p := sub_pos.mpr hp.2
  have hc (i : Fin n) : 0 < (m.choose (x i) : ℝ) := by exact_mod_cast Nat.choose_pos (hx i)
  unfold sampleLikelihood sizeLogLik
  rw [log_prod (fun i _ ↦ (mul_pos (mul_pos (hc i) (pow_pos hp₀ _)) (pow_pos hp₁ _)).ne')]
  apply Finset.sum_congr rfl
  intro i _
  rw [log_mul (mul_pos (hc i) (pow_pos hp₀ _)).ne' (pow_pos hp₁ _).ne',
    log_mul (hc i).ne' (pow_pos hp₀ _).ne', log_pow, log_pow, Nat.cast_sub (hx i)]

private theorem one_sub_ratio_pos {k m : ℕ} (hk : k ≤ m) :
    0 < 1 - (k : ℝ) / ((m : ℝ) + 1) := by
  have hm : 0 < (m : ℝ) + 1 := by positivity
  rw [sub_pos, div_lt_one hm]
  have h : (k : ℝ) ≤ m := by exact_mod_cast hk
  linarith

theorem size_increment_eq {n m : ℕ} (x : Fin n → ℕ) (p : ℝ) (hx : ∀ i, x i ≤ m) :
    sizeLogLik x p (m + 1) - sizeLogLik x p m = sizeIncrement x p m := by
  unfold sizeLogLik sizeIncrement
  rw [← Finset.sum_sub_distrib]
  apply Finset.sum_congr rfl
  intro i _
  let k := x i
  have hk : k ≤ m := hx i
  have hm : 0 < (m : ℝ) + 1 := by positivity
  have h0 : 0 < (m.choose k : ℝ) := by exact_mod_cast Nat.choose_pos hk
  have h1 : 0 < ((m + 1).choose k : ℝ) := by exact_mod_cast Nat.choose_pos (by omega : k ≤ m + 1)
  have hchoose : (m.choose k : ℝ) * ((m : ℝ) + 1) =
      ((m + 1).choose k : ℝ) * ((m : ℝ) + 1 - k) := by
    have h := congrArg (fun t : ℕ ↦ (t : ℝ)) (Nat.choose_mul_succ_eq m k)
    simpa [Nat.cast_sub (show k ≤ m + 1 by omega)] using h
  have hr : 1 - (k : ℝ) / ((m : ℝ) + 1) = (m.choose k : ℝ) / ((m + 1).choose k : ℝ) := by
    apply (eq_div_iff h1.ne').2
    apply (eq_of_sub_eq_zero)
    field_simp
    linear_combination -hchoose
  change log ((m + 1).choose k) + (k : ℝ) * log p +
      (((m + 1 : ℕ) : ℝ) - k) * log (1 - p) -
    (log (m.choose k) + (k : ℝ) * log p + ((m : ℝ) - k) * log (1 - p)) = _
  rw [hr, log_div h0.ne' h1.ne']
  push_cast
  ring

private theorem increment_lt {n u v : ℕ} (x : Fin n → ℕ) (p : ℝ)
    (hx : ∀ i, x i ≤ u) (huv : u < v) (hpos : ∃ i, 0 < x i) :
    sizeIncrement x p v < sizeIncrement x p u := by
  obtain ⟨j, hj⟩ := hpos
  apply Finset.sum_lt_sum
  · intro i _
    apply sub_le_sub_left
    apply log_le_log (one_sub_ratio_pos (hx i))
    have h : (u : ℝ) ≤ v := by exact_mod_cast huv.le
    exact sub_le_sub_left (div_le_div_of_nonneg_left (by positivity) (by positivity) (by linarith)) _
  · refine ⟨j, Finset.mem_univ _, ?_⟩
    apply sub_lt_sub_left
    apply log_lt_log (one_sub_ratio_pos (hx j))
    have h : (u : ℝ) < v := by exact_mod_cast huv
    exact sub_lt_sub_left (div_lt_div_of_pos_left (by exact_mod_cast hj) (by positivity) (by linarith)) _

private theorem exists_negative_increment {n : ℕ} (x : Fin n → ℕ) (hn : 0 < n)
    {p : ℝ} (hp : p ∈ Ioo 0 1) (b : ℕ) :
    ∃ m ≥ b, sizeIncrement x p m < 0 := by
  let B : ℝ := ∑ i, (x i : ℝ)
  obtain ⟨m, hm⟩ := exists_nat_gt (B / p + b)
  have hB : 0 ≤ B := by dsimp [B]; positivity
  have hBp : 0 ≤ B / p := div_nonneg hB hp.1.le
  have hmb : b ≤ m := by exact_mod_cast (show (b : ℝ) ≤ m by linarith)
  have hmp : B < p * ((m : ℝ) + 1) := by
    have h := (div_lt_iff₀ hp.1).mp (show B / p < (m : ℝ) + 1 by linarith)
    linarith
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  refine ⟨m, hmb, Finset.sum_neg (fun i _ ↦ ?_) Finset.univ_nonempty⟩
  have hi : (x i : ℝ) ≤ B := Finset.single_le_sum
    (fun j (_ : j ∈ Finset.univ) ↦ Nat.cast_nonneg (x j)) (Finset.mem_univ i)
  apply sub_neg.mpr
  apply log_lt_log (by linarith [hp.2])
  have hdiv : (x i : ℝ) / ((m : ℝ) + 1) < p :=
    (div_lt_iff₀ (by positivity)).mpr (by linarith)
  linarith

/-- A held interior probability has a finite maximizing size, with at most an adjacent tie. -/
theorem held_interior_size_mle {n b : ℕ} (x : Fin n → ℕ) (hn : 0 < n)
    {p : ℝ} (hp : p ∈ Ioo 0 1) (hb : ∀ m : ℕ, b ≤ m ↔ ∀ i, x i ≤ m) :
    ∃ r ≥ b, ∀ m ≥ b, IsMaxOn (sizeLogLik x p) (Ici b) m ↔
      m = r ∨ (m = r + 1 ∧ sizeIncrement x p r = 0) := by
  classical
  obtain ⟨m, hm, hneg⟩ := exists_negative_increment x hn hp b
  have hex : ∃ r, b ≤ r ∧ sizeIncrement x p r ≤ 0 := ⟨m, hm, hneg.le⟩
  let r := Nat.find hex
  have hr : b ≤ r ∧ sizeIncrement x p r ≤ 0 := Nat.find_spec hex
  have hleft : ∀ k ≥ b, k < r → sizeLogLik x p k < sizeLogLik x p (k + 1) := by
    intro k hk hkr
    have h := Nat.find_min hex hkr
    have hpos : 0 < sizeIncrement x p k := by
      exact lt_of_not_ge (fun he ↦ h ⟨hk, he⟩)
    rw [← size_increment_eq x p ((hb k).mp hk)] at hpos
    exact sub_pos.mp hpos
  have hright : ∀ k > r, sizeLogLik x p (k + 1) < sizeLogLik x p k := by
    intro k hk
    have hneg' : sizeIncrement x p k < 0 := by
      by_cases hx : ∃ i, 0 < x i
      · exact (increment_lt x p ((hb r).mp hr.1) hk hx).trans_le hr.2
      · push Not at hx
        have hzero : ∀ i, x i = 0 := fun i ↦ Nat.eq_zero_of_le_zero (hx i)
        simpa [sizeIncrement, hzero] using
          (mul_neg_of_pos_of_neg (by exact_mod_cast hn : (0 : ℝ) < n)
            (log_neg (by linarith [hp.2]) (by linarith [hp.1])))
    rw [← size_increment_eq x p ((hb k).mp (hr.1.trans hk.le))] at hneg'
    exact sub_neg.mp hneg'
  have hstep := size_increment_eq x p ((hb r).mp hr.1)
  have htail : AntitoneOn (sizeLogLik x p) (Ici (r + 1)) :=
    antitoneOn_nat_Ici_of_succ_le fun k hk ↦ (hright k (by omega)).le
  have hbefore : ∀ k ≥ b, k < r → sizeLogLik x p k < sizeLogLik x p r := by
    intro k hk hkr
    have aux : ∀ j, k + 1 ≤ j → j ≤ r → sizeLogLik x p k < sizeLogLik x p j := by
      intro j hj
      induction j, hj using Nat.le_induction with
      | base => exact fun _ ↦ hleft k hk hkr
      | succ j hj ih => exact fun hjr ↦ (ih (by omega)).trans (hleft j (by omega) (by omega))
    exact aux r hkr le_rfl
  have hmax : IsMaxOn (sizeLogLik x p) (Ici b) r := by
    intro k hk
    rcases lt_trichotomy k r with h | rfl | h
    · exact (hbefore k hk h).le
    · exact le_rfl
    · have hh := htail (show r + 1 ∈ Ici (r + 1) from mem_Ici.mpr le_rfl)
        (show k ∈ Ici (r + 1) from h) h
      exact hh.trans (by linarith [hr.2])
  refine ⟨r, hr.1, fun k hk ↦ ⟨?_, ?_⟩⟩
  · intro hm
    have he : sizeLogLik x p r ≤ sizeLogLik x p k := hm hr.1
    rcases lt_trichotomy k r with h | h | h
    · exact ((hbefore k hk h).not_ge he).elim
    · exact Or.inl h
    · have hkr : k = r + 1 := by
        by_contra hn'
        have hk' : r + 2 ≤ k := by omega
        have hh := htail (show r + 2 ∈ Ici (r + 1) by exact Nat.le_succ _)
          (mem_Ici.mpr (show r + 1 ≤ k by omega)) hk'
        have hnext := hright (r + 1) (by omega)
        linarith [hmax (show r + 1 ∈ Ici b from hr.1.trans (Nat.le_succ _))]
      exact Or.inr ⟨hkr, by rw [hkr] at he; linarith [hr.2]⟩
  · rintro (rfl | ⟨rfl, hz⟩)
    · exact hmax
    · intro k hk
      have he : sizeLogLik x p k ≤ sizeLogLik x p r := hmax hk
      change sizeLogLik x p k ≤ sizeLogLik x p (r + 1)
      linarith

/-- A nonconstant sample can have an exact adjacent tie at a binary64 probability. -/
theorem nonconstant_tie :
    sizeLogLik ![0, 6] (1 / 2) 8 = sizeLogLik ![0, 6] (1 / 2) 7 := by
  let x : Fin 2 → ℕ := ![0, 6]
  have hx : ∀ i, x i ≤ 7 := by intro i; fin_cases i <;> decide
  have hstep := size_increment_eq x (1 / 2) hx
  have hz : sizeIncrement x (1 / 2) 7 = 0 := by
    unfold sizeIncrement
    rw [Fin.sum_univ_two]
    change log (1 - 1 / 2) - log (1 - (0 : ℕ) / (↑(7 : ℕ) + 1)) +
      (log (1 - 1 / 2) - log (1 - (6 : ℕ) / (↑(7 : ℕ) + 1))) = 0
    have hlog : log (1 / 4 : ℝ) = log (1 / 2) + log (1 / 2) := by
      rw [← log_mul (by norm_num : (1 / 2 : ℝ) ≠ 0) (by norm_num)]
      norm_num
    norm_num
    linarith
  exact sub_eq_zero.mp (hstep.trans hz)

theorem nonconstant_tie_maximizers :
    ∀ m ≥ 6, IsMaxOn (sizeLogLik ![0, 6] (1 / 2)) (Ici 6) m ↔ m = 7 ∨ m = 8 := by
  let x : Fin 2 → ℕ := ![0, 6]
  have htie : sizeLogLik x (1 / 2) 8 = sizeLogLik x (1 / 2) 7 := nonconstant_tie
  have hb (m : ℕ) : 6 ≤ m ↔ ∀ i, x i ≤ m := by
    constructor
    · intro hm i
      fin_cases i
      · exact Nat.zero_le _
      · exact hm
    · exact fun h ↦ h 1
  have hx : ∃ i, 0 < x i := ⟨1, by decide⟩
  have hz : sizeIncrement x (1 / 2) 7 = 0 := by
    rw [← size_increment_eq x _ ((hb 7).mp (by decide)), htie, sub_self]
  have h₆ : sizeLogLik x (1 / 2) 6 < sizeLogLik x (1 / 2) 7 := by
    have h := increment_lt x (1 / 2) ((hb 6).mp le_rfl) (show 6 < 7 by decide) hx
    rw [hz, ← size_increment_eq x _ ((hb 6).mp le_rfl)] at h
    exact sub_pos.mp h
  have htail : ∀ k ≥ 8, sizeLogLik x (1 / 2) (k + 1) < sizeLogLik x (1 / 2) k := by
    intro k hk
    have h := increment_lt x (1 / 2) ((hb 7).mp (by decide)) (show 7 < k by omega) hx
    rw [hz, ← size_increment_eq x _ ((hb k).mp (by omega))] at h
    exact sub_neg.mp h
  have hanti : AntitoneOn (sizeLogLik x (1 / 2)) (Ici 8) :=
    antitoneOn_nat_Ici_of_succ_le fun k hk ↦ (htail k hk).le
  have hbig : ∀ k ≥ 9, sizeLogLik x (1 / 2) k < sizeLogLik x (1 / 2) 8 := by
    intro k hk
    exact (hanti (mem_Ici.mpr (by decide : 8 ≤ 9))
      (mem_Ici.mpr (show 8 ≤ k by omega)) hk).trans_lt (htail 8 le_rfl)
  have hmax : IsMaxOn (sizeLogLik x (1 / 2)) (Ici 6) 8 := by
    intro k hk
    have hk' : 6 ≤ k := hk
    change sizeLogLik x (1 / 2) k ≤ sizeLogLik x (1 / 2) 8
    rcases (show k = 6 ∨ k = 7 ∨ k = 8 ∨ 9 ≤ k by omega) with rfl | rfl | rfl | h
    · rw [htie]
      exact h₆.le
    · rw [htie]
    · exact le_rfl
    · exact (hbig k h).le
  intro m hm
  change IsMaxOn (sizeLogLik x (1 / 2)) (Ici 6) m ↔ _
  constructor
  · intro h
    have hh : sizeLogLik x (1 / 2) 8 ≤ sizeLogLik x (1 / 2) m :=
      h (mem_Ici.mpr (by decide))
    rcases (show m = 6 ∨ m = 7 ∨ m = 8 ∨ 9 ≤ m by omega) with rfl | rfl | rfl | he
    · rw [htie] at hh
      exact (h₆.not_ge hh).elim
    · exact Or.inl rfl
    · exact Or.inr rfl
    · exact ((hbig m he).not_ge hh).elim
  · rintro (rfl | rfl)
    · intro k hk
      change sizeLogLik x (1 / 2) k ≤ sizeLogLik x (1 / 2) 7
      rw [← htie]
      exact hmax hk
    · exact hmax

end Binomial

namespace Geometric

def likelihood (a p : ℝ) : ℝ := Binomial.likelihood 1 a p

theorem held_positive_iff {a p : ℝ} (hp : p ∈ Ioc (0 : ℝ) 1) :
    0 < likelihood a p ↔ p < 1 ∨ a = 0 := by
  rw [likelihood, Binomial.held_positive_iff]
  have h₀ := hp.1
  have h₁ := hp.2
  grind

theorem unique_mle {a : ℝ} (ha : 0 ≤ a) :
    ∀ p ∈ Ioc (0 : ℝ) 1, IsMaxOn (likelihood a) (Ioc 0 1) p ↔ p = 1 / (1 + a) := by
  intro p hp
  have ht : 0 < 1 + a := by linarith
  let q : ℝ := 1 / (1 + a)
  have hq : q ∈ Ioc (0 : ℝ) 1 := ⟨div_pos one_pos ht, (div_le_one ht).mpr (by linarith)⟩
  have hm : IsMaxOn (likelihood a) (Icc 0 1) q :=
    (Binomial.fixed_size_mle zero_le_one ha ht q ⟨hq.1.le, hq.2⟩).mpr rfl
  have restrict (y : ℝ) (hy : y ∈ Ioc 0 1) : y ∈ Icc (0 : ℝ) 1 := ⟨hy.1.le, hy.2⟩
  constructor
  · intro hpmax
    apply (Binomial.fixed_size_mle zero_le_one ha ht p (restrict p hp)).mp
    intro y hy
    rcases hy.1.eq_or_lt with h | h
    · have hpos : 0 < likelihood a q := by
        apply (held_positive_iff hq).mpr
        by_cases ha' : a = 0
        · exact Or.inr ha'
        · exact Or.inl ((div_lt_one ht).mpr (by linarith [lt_of_le_of_ne ha (Ne.symm ha')]))
      have hle : likelihood a q ≤ likelihood a p := hpmax hq
      change likelihood a y ≤ likelihood a p
      rw [← h]
      simpa [likelihood, Binomial.likelihood] using (hpos.le.trans hle)
    · exact hpmax ⟨h, hy.2⟩
  · rintro rfl y hy
    exact hm (restrict y hy)

end Geometric

namespace ZeroTruncatedPoisson

def conditionalMean (lam : ℝ) : ℝ := lam / (1 - exp (-lam))

def logLik (a lam : ℝ) : ℝ := a * log lam - log (exp lam - 1)

private theorem denominator_pos {lam : ℝ} (hl : 0 < lam) : 0 < 1 - exp (-lam) := by
  rw [sub_pos, exp_lt_one_iff]
  linarith

private theorem exp_sub_one_pos {lam : ℝ} (hl : 0 < lam) : 0 < exp lam - 1 := by
  rw [sub_pos, one_lt_exp_iff]
  exact hl

private theorem mean_eq {lam : ℝ} (hl : 0 < lam) :
    conditionalMean lam = lam * exp lam / (exp lam - 1) := by
  rw [conditionalMean, exp_neg]
  field_simp [(denominator_pos hl).ne', (exp_sub_one_pos hl).ne', (exp_pos lam).ne']

private theorem mean_strictMono : StrictMonoOn conditionalMean (Ioi (0 : ℝ)) := by
  apply strictMonoOn_of_deriv_pos (convex_Ioi 0)
  · exact continuous_id.continuousOn.div (continuous_const.sub (continuous_exp.comp continuous_neg)).continuousOn
      (fun t ht ↦ (denominator_pos ht).ne')
  · intro t ht
    have hp : 0 < t := interior_subset ht
    have he : 1 + t < exp t := by simpa [add_comm] using add_one_lt_exp hp.ne'
    have hd := (hasDerivAt_const t (1 : ℝ)).sub (((hasDerivAt_id t).neg).exp)
    have hdiv : HasDerivAt conditionalMean _ t := (hasDerivAt_id t).div hd (denominator_pos hp).ne'
    rw [hdiv.deriv]
    apply div_pos _ (sq_pos_of_pos (denominator_pos hp))
    change 0 < 1 * (1 - exp (-t)) - t * (0 - exp (-t) * -1)
    have h := mul_lt_mul_of_pos_right he (exp_pos (-t))
    rw [← exp_add, add_neg_cancel, exp_zero] at h
    nlinarith

private theorem ratio_limit :
    Tendsto (fun lam : ℝ ↦ (exp lam - 1) / lam) (𝓝[>] 0) (𝓝 1) := by
  simpa [div_eq_inv_mul] using (hasDerivAt_exp 0).tendsto_slope_zero_right

private theorem mean_limit :
    Tendsto conditionalMean (𝓝[>] 0) (𝓝 1) := by
  have hExp : Tendsto exp (𝓝[>] (0 : ℝ)) (𝓝 1) := by
    simpa only [exp_zero] using (continuous_exp.continuousAt (x := (0 : ℝ))).tendsto.mono_left
      (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
  have h := hExp.div ratio_limit one_ne_zero
  rw [div_one] at h
  have he : ∀ᶠ t : ℝ in 𝓝[>] 0, t ∈ Ioi (0 : ℝ) := self_mem_nhdsWithin
  refine h.congr' (he.mono fun t ht ↦ ?_)
  change exp t / ((exp t - 1) / t) = conditionalMean t
  rw [mean_eq ht]
  field_simp [(exp_sub_one_pos ht).ne', (show 0 < t from ht).ne']

private theorem log_coordinate_deriv (a t : ℝ) :
    HasDerivAt (fun s ↦ logLik a (exp s)) (a - conditionalMean (exp t)) t := by
  have hp := exp_pos t
  have hd := (((hasDerivAt_exp t).exp).sub_const 1).log (exp_sub_one_pos hp).ne'
  have h := ((hasDerivAt_id t).const_mul a).sub hd
  rw [mean_eq hp]
  simpa [logLik, log_exp, id, mul_comm, Pi.sub_def] using h

/-- The positive-rate model has exactly one maximizing rate above mean one. -/
theorem unique_mle {a : ℝ} (ha : 1 < a) :
    (∃! lam, 0 < lam ∧ conditionalMean lam = a) ∧
      ∀ lam > 0, IsMaxOn (logLik a) (Ioi 0) lam ↔ conditionalMean lam = a := by
  let score : ℝ → ℝ := fun t ↦ a - conditionalMean (exp t)
  have hlimit := mean_limit.eventually (Iio_mem_nhds ha)
  have he : ∀ᶠ t : ℝ in 𝓝[>] 0, t ∈ Ioi (0 : ℝ) := self_mem_nhdsWithin
  obtain ⟨lo, hlo, hmean⟩ := (he.and hlimit).exists
  have hlo' : 0 < lo := hlo
  let hi := max lo (a + 1)
  have hhi : 0 < hi := hlo'.trans_le (le_max_left _ _)
  have hhigh : a < conditionalMean hi := by
    have hle : hi ≤ conditionalMean hi := by
      apply (le_div_iff₀ (denominator_pos hhi)).2
      nlinarith [mul_pos hhi (exp_pos (-hi))]
    exact (show a < a + 1 by linarith).trans_le ((le_max_right _ _).trans hle)
  have hc : Continuous score := by
    unfold score conditionalMean
    exact continuous_const.sub (continuous_exp.div
      (continuous_const.sub (continuous_exp.comp continuous_exp.neg))
      (fun t ↦ (denominator_pos (exp_pos t)).ne'))
  have hs : StrictAnti score := fun u v huv ↦ sub_lt_sub_left
    (mean_strictMono (exp_pos u) (exp_pos v) (exp_lt_exp.mpr huv)) a
  obtain ⟨⟨r, ⟨_, hr⟩, huniq⟩, hm⟩ :=
    Generic.existsUnique_score_zero_of_signs (convex_univ (𝕜 := ℝ))
      (fun t _ ↦ log_coordinate_deriv a t) hc.continuousOn (hs.strictAntiOn univ)
      (mem_univ (log lo)) (mem_univ (log hi)) (log_le_log hlo' (le_max_left _ _))
      (show 0 < score (log lo) by dsimp [score]; rw [exp_log hlo']; exact sub_pos.mpr hmean)
      (show score (log hi) < 0 by dsimp [score]; rw [exp_log hhi]; exact sub_neg.mpr hhigh)
  refine ⟨⟨exp r, ⟨exp_pos r, (sub_eq_zero.mp hr).symm⟩, ?_⟩, ?_⟩
  · intro lam ⟨hl, he⟩
    have ht := huniq (log lam) ⟨mem_univ _, by
      rw [exp_log hl, he, sub_self]⟩
    rw [← ht, exp_log hl]
  · intro lam hl
    have he := hm (log lam) (mem_univ _)
    have hz : score (log lam) = 0 ↔ conditionalMean lam = a := by
      dsimp [score]
      rw [exp_log hl, sub_eq_zero, eq_comm]
    rw [← hz, ← he]
    constructor
    · intro h t _
      change logLik a (exp t) ≤ logLik a (exp (log lam))
      rw [exp_log hl]
      exact h (exp_pos t)
    · intro h t ht
      have h' := h (mem_univ (log t))
      change logLik a (exp (log t)) ≤ logLik a (exp (log lam)) at h'
      change logLik a t ≤ logLik a lam
      rwa [exp_log ht, exp_log hl] at h'

theorem all_ones_limit : Tendsto (logLik 1) (𝓝[>] 0) (𝓝 0) := by
  have h : Tendsto (fun t : ℝ ↦ -log ((exp t - 1) / t)) (𝓝[>] 0) (𝓝 0) := by
    simpa using ((continuousAt_log one_ne_zero).tendsto.comp ratio_limit).neg
  have he : ∀ᶠ t : ℝ in 𝓝[>] 0, t ∈ Ioi (0 : ℝ) := self_mem_nhdsWithin
  refine h.congr' (he.mono fun t ht ↦ ?_)
  have hp : 0 < t := ht
  rw [log_div (exp_sub_one_pos hp).ne' hp.ne']
  unfold logLik
  ring

theorem all_ones_strict {lam : ℝ} (hl : 0 < lam) : logLik 1 lam < 0 := by
  have he : lam + 1 < exp lam := add_one_lt_exp hl.ne'
  have h := log_lt_log hl (show lam < exp lam - 1 by linarith)
  unfold logLik
  linarith

theorem all_ones_supremum :
    IsLUB (logLik 1 '' Ioi 0) 0 ∧ ∀ lam > 0, ¬ IsMaxOn (logLik 1) (Ioi 0) lam := by
  have he : ∀ᶠ t : ℝ in 𝓝[>] 0, id t ∈ Ioi (0 : ℝ) := self_mem_nhdsWithin
  refine ⟨⟨?_, ?_⟩, ?_⟩
  · rintro _ ⟨lam, hl, rfl⟩
    exact (all_ones_strict hl).le
  · intro u hu
    exact Generic.limit_le_upperBound (p := id) he all_ones_limit hu
  · intro lam hl hm
    have hu : logLik 1 lam ∈ upperBounds (logLik 1 '' Ioi 0) := by
      rintro _ ⟨t, ht, rfl⟩
      exact hm ht
    exact (all_ones_strict hl).not_ge
      (Generic.limit_le_upperBound (p := id) he all_ones_limit hu)

/-- The zero modifier includes the point mass at one at rate zero. -/
def likelihood (a lam : ℝ) : ℝ :=
  if lam = 0 then (if a = 1 then 1 else 0) else exp (logLik a lam)

theorem held_positive_iff {a lam : ℝ} (hl : 0 ≤ lam) :
    0 < likelihood a lam ↔ 0 < lam ∨ a = 1 := by
  rcases hl.eq_or_lt with h | h
  · subst lam
    by_cases ha : a = 1 <;> simp [likelihood, ha]
  · simp [likelihood, h.ne', h, exp_pos]

theorem all_ones_attained :
    ∀ lam ∈ Ici (0 : ℝ), IsMaxOn (likelihood 1) (Ici 0) lam ↔ lam = 0 := by
  intro lam hl
  constructor
  · intro hm
    by_contra h
    have hp : 0 < lam := lt_of_le_of_ne hl (Ne.symm h)
    have hs := all_ones_strict hp
    have hh := hm (show (0 : ℝ) ∈ Ici 0 from le_rfl)
    simp [likelihood, h] at hh
    exact hs.not_ge hh
  · rintro rfl y hy
    rcases hy.eq_or_lt with h | h
    · simp [← h]
    · simpa [likelihood, h.ne'] using (exp_le_one_iff.mpr (all_ones_strict h).le)

/-- In the library's closed domain, positive-mean excess still gives the same unique root. -/
theorem closed_domain_mle {a : ℝ} (ha : 1 < a) :
    ∀ lam ∈ Ici (0 : ℝ), IsMaxOn (likelihood a) (Ici 0) lam ↔
      0 < lam ∧ conditionalMean lam = a := by
  obtain ⟨⟨r, ⟨hr, _⟩, _⟩, hmax⟩ := unique_mle ha
  intro lam hl
  have h₀ : likelihood a 0 = 0 := by simp [likelihood, ha.ne']
  have hpos : ∀ t > 0, 0 < likelihood a t := fun t ht ↦ by
    simp [likelihood, ht.ne', exp_pos]
  constructor
  · intro hm
    have hp : 0 < lam := by
      rcases hl.eq_or_lt with h | h
      · subst lam
        exact ((hpos r hr).not_ge (h₀ ▸ hm hr.le)).elim
      · exact h
    refine ⟨hp, (hmax lam hp).mp ?_⟩
    intro t ht
    have h := hm (show t ∈ Ici (0 : ℝ) from ht.le)
    simpa [likelihood, ht.ne', hp.ne'] using h
  · rintro ⟨hp, hr'⟩ t ht
    rcases ht.eq_or_lt with h | h
    · subst t
      change likelihood a 0 ≤ likelihood a lam
      rw [h₀]
      exact (hpos lam hp).le
    · have hh := ((hmax lam hp).mpr hr') h
      simpa [likelihood, h.ne', hp.ne'] using (exp_le_exp.mpr hh)

theorem sample_exists_iff {n : ℕ} (x : Fin n → ℕ) (hn : 0 < n)
    (hx : ∀ i, 1 ≤ x i) :
    (∃ lam > 0, IsMaxOn (logLik (avg x)) (Ioi 0) lam) ↔
      ∃ i, 1 < x i := by
  have ha : 1 ≤ avg x := by simpa using le_avg x hn hx
  have he : avg x = 1 ↔ ∀ i, x i = 1 := by simpa using avg_eq_of_lowerBound x hn hx
  constructor
  · rintro ⟨lam, hl, hm⟩
    by_contra h
    push Not at h
    have h₁ := he.mpr fun i ↦ le_antisymm (h i) (hx i)
    rw [h₁] at hm
    exact all_ones_supremum.2 lam hl hm
  · rintro ⟨i, hi⟩
    have h : 1 < avg x := lt_of_le_of_ne ha (fun hz ↦ hi.ne' (he.mp hz.symm i))
    obtain ⟨⟨lam, ⟨hl, hz⟩, _⟩, hm⟩ := unique_mle h
    exact ⟨lam, hl, (hm lam hl).mpr hz⟩

end ZeroTruncatedPoisson

end

end UnivariateML.Discrete
