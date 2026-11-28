module

public import UnivariateML.Family.Zero6
public import UnivariateML.Family.Zero6Inflated
public import UnivariateML.Family.Discrete4BBinom
public import UnivariateML.Family.Discrete3BNBinom
public import UnivariateML.Family.Discrete3DGamma
public import UnivariateML.Family.DiscreteWeibull
public import UnivariateML.Family.Discrete3PIG
public import UnivariateML.Family.CMP

/-! Zero-modifier likelihoods on the six parent charts.
Deviations: the positive conditional witness and nonnegativity of the
observed parent masses enter explicitly. These transfers do not classify
family escape directions or establish that the conditional fit attains. -/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.Zero6Families

open Truncation

noncomputable section

def matchesHold {P : Type*} (hold : Option P) (v : P) : Prop :=
  ∀ w ∈ hold, v = w

def bbinomSlice (size : Option ℕ) (alpha beta : Option ℝ) :
    Set (ℕ × ℝ × ℝ) :=
  {p | 0 < p.2.1 ∧ 0 < p.2.2 ∧ matchesHold size p.1 ∧
    matchesHold alpha p.2.1 ∧ matchesHold beta p.2.2}

def bbinomMass (p : ℕ × ℝ × ℝ) (j : ℕ) : ℝ :=
  Discrete4BBinom.mass p.1 j p.2.1 p.2.2

def bnbinomSlice (size alpha beta : Option ℝ) : Set (ℝ × ℝ × ℝ) :=
  {p | 0 < p.1 ∧ 0 < p.2.1 ∧ 0 < p.2.2 ∧ matchesHold size p.1 ∧
    matchesHold alpha p.2.1 ∧ matchesHold beta p.2.2}

def bnbinomMass (p : ℝ × ℝ × ℝ) (j : ℕ) : ℝ :=
  exp (Discrete3BNBinom.logMass j p.1 p.2.1 p.2.2)

def cmpSlice (rate dispersion : Option ℝ) : Set (ℝ × ℝ) :=
  {p | 0 ≤ p.1 ∧ 0 ≤ p.2 ∧ (0 < p.2 ∨ p.1 < 1) ∧
    matchesHold rate p.1 ∧ matchesHold dispersion p.2}

def cmpMass (p : ℝ × ℝ) (j : ℕ) : ℝ :=
  if p.1 = 0 then (if j = 0 then 1 else 0)
  else CMP.weight (log p.1) p.2 j / ∑' k, CMP.weight (log p.1) p.2 k

def dgammaSlice (shape rate : Option ℝ) : Set (ℝ × ℝ) :=
  {p | 0 < p.1 ∧ 0 < p.2 ∧ matchesHold shape p.1 ∧ matchesHold rate p.2}

def dgammaMass (p : ℝ × ℝ) (j : ℕ) : ℝ :=
  Discrete3DGamma.cellProb p.1 p.2 j

def dweibullSlice (shape1 shape2 : Option ℝ) : Set (ℝ × ℝ) :=
  {p | p.1 ∈ Ioo 0 1 ∧ 0 < p.2 ∧ matchesHold shape1 p.1 ∧ matchesHold shape2 p.2}

def dweibullMass (p : ℝ × ℝ) (j : ℕ) : ℝ :=
  if j = 0 then 1-p.1 else p.1 ^ ((j : ℝ)^p.2) - p.1 ^ (((j+1 : ℕ) : ℝ)^p.2)

def pigSlice (mean shape : Option ℝ) : Set (ℝ × ℝ) :=
  {p | 0 < p.1 ∧ 0 ≤ p.2 ∧ matchesHold mean p.1 ∧ matchesHold shape p.2}

def pigMass (p : ℝ × ℝ) (j : ℕ) : ℝ :=
  if p.2 = 0 then (if j = 0 then 1 else 0) else
    let t := p.2 / p.1
    let b := sqrt (2*p.2 + t^2)
    exp (t-b) * (p.2/b)^j *
      (Discrete3PIG.besselPolynomial (max j 1)).eval (1/b) / j.factorial

def positiveSlice {P : Type*} (D : Set P) (mass : P → ℕ → ℝ) : Set P :=
  {p ∈ D | mass p 0 < 1}

def conditional {P : Type*} {k : ℕ} (x : Fin k → ℕ) (mass : P → ℕ → ℝ) (p : P) : ℝ :=
  ∏ i, mass p (x i) / (1-mass p 0)

def modifiedSlice {P : Type*} (D : Set P) (mass : P → ℕ → ℝ)
    (z k : ℕ) (hold : Option ℝ) : Set P :=
  if k = 0 ∧ Zero6.chosenWeight z k hold = 1 then D else positiveSlice D mass

/-- Joint transfer statement for one sample and one parent hold slice.
The final clause transfers a limiting conditional likelihood along any
supplied path at the selected zero weight. -/
def SampleClaim {P : Type*} {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    (D : Set P) (mass : P → ℕ → ℝ) (hold : Option ℝ) : Prop :=
  let E := modifiedSlice D mass z k hold
  let L := conditional x mass
  (∀ p ∈ E, ∀ w ∈ Zero6.weightDomain hold,
    (IsMaxOn (Zero6.modified z k L) (E ×ˢ Zero6.weightDomain hold) (p,w) ∧
      0 < Zero6.modified z k L (p,w)) ↔
    w = Zero6.chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (k ≠ 0 → IsMaxOn L E p)) ∧
  (∀ p ∈ positiveSlice D mass, ∀ rho ∈ Icc (0 : ℝ) 1,
    Zero6Inflated.likelihood z x mass (p,rho) =
      bernoulli z k (Zero6Inflated.effective (mass p 0) rho) * L p) ∧
  (k = 0 → ∀ p ∈ D, ∀ rho ∈ Icc (0 : ℝ) 1,
    Zero6Inflated.likelihood z x mass (p,rho) = 1 ↔ mass p 0 = 1 ∨ rho = 1) ∧
  (∀ {F : Filter P} {a : ℝ}, k ≠ 0 → Tendsto L F (𝓝 a) →
    Tendsto (fun p ↦ Zero6.modified z k L (p,Zero6.chosenWeight z k hold)) F
      (𝓝 (bernoulli z k (Zero6.chosenWeight z k hold) * a)))

private theorem sample_transfer {P : Type*} {z k : ℕ} (hn : 0 < z+k)
    {x : Fin k → ℕ} {D : Set P} {mass : P → ℕ → ℝ} {hold : Option ℝ}
    (zeroMass : ∀ p ∈ D, mass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ D, ∀ i, 0 ≤ mass p (x i))
    (conditionalWitness : k ≠ 0 → ∃ p ∈ positiveSlice D mass, 0 < conditional x mass p) :
    SampleClaim z x D mass hold := by
  unfold SampleClaim
  refine ⟨?_,?_,?_,?_⟩
  · intro p hp w hw
    apply Zero6.modified_fit hn hold _ _ hp hw
    · intro q hq
      by_cases hk : k = 0
      · subst k
        simp [conditional]
      · have H : q ∈ positiveSlice D mass := by
          simpa [modifiedSlice, hk] using hq
        exact Finset.prod_nonneg fun i _ ↦
          div_nonneg (positiveMass q H.1 i) (sub_nonneg.mpr H.2.le)
    · intro hk
      simpa [modifiedSlice, hk] using conditionalWitness hk
  · intro p hp rho hr
    have he : Zero6Inflated.effective (mass p 0) rho = mass p 0 + (1-mass p 0)*rho := by
      unfold Zero6Inflated.effective
      ring
    simpa only [Zero6Inflated.likelihood, conditional, he] using
      Trunc3Zero.inflated_factorization z x (mass p 0) rho hp.2 (mass p)
  · intro hk p hp rho hr
    subst k
    have hz : 0 < z := by omega
    simpa only [Zero6Inflated.likelihood, Zero6Inflated.effective, Finset.univ_eq_empty,
      Finset.prod_empty, mul_one] using Trunc3Zero.inflated_zero hz (zeroMass p hp) hr
  · intro F a hk ha
    simpa only [Zero6.modified, ite_eq_right hk] using
      (Trunc3Zero.modified_face_limit tendsto_const_nhds ha :
        Tendsto (fun p ↦ bernoulli z k (Zero6.chosenWeight z k hold) * conditional x mass p)
          F (𝓝 (bernoulli z k (Zero6.chosenWeight z k hold) * a)))

/-- Bbinom, all free, alpha, beta, size, alpha+size and beta+size, zero:modified criterion and
failure transfer and zero:inflated criterion transfer. -/
theorem bbinom_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (size : Option ℕ) (alpha beta : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ bbinomSlice size alpha beta, bbinomMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ bbinomSlice size alpha beta, ∀ i, 0 ≤ bbinomMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (bbinomSlice size alpha beta) bbinomMass,
        0 < conditional x bbinomMass p) :
    SampleClaim z x (bbinomSlice size alpha beta) bbinomMass hold :=
  sample_transfer hn zeroMass positiveMass conditionalWitness

/-- Bnbinom, all eight parent masks, zero:modified criterion and
failure transfer and zero:inflated criterion transfer. -/
theorem bnbinom_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (size alpha beta : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ bnbinomSlice size alpha beta, bnbinomMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ bnbinomSlice size alpha beta, ∀ i, 0 ≤ bnbinomMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (bnbinomSlice size alpha beta) bnbinomMass,
        0 < conditional x bnbinomMass p) :
    SampleClaim z x (bnbinomSlice size alpha beta) bnbinomMass hold :=
  sample_transfer hn zeroMass positiveMass conditionalWitness

/-- Cmp, all free, lambda, nu and lambda+nu, zero:modified criterion and
failure transfer and zero:inflated criterion transfer. -/
theorem cmp_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (rate dispersion : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ cmpSlice rate dispersion, cmpMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ cmpSlice rate dispersion, ∀ i, 0 ≤ cmpMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (cmpSlice rate dispersion) cmpMass,
        0 < conditional x cmpMass p) :
    SampleClaim z x (cmpSlice rate dispersion) cmpMass hold :=
  sample_transfer hn zeroMass positiveMass conditionalWitness

/-- Dgamma, all free, shape, rate and rate+shape, zero:modified criterion and
failure transfer and zero:inflated criterion transfer. -/
theorem dgamma_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (shape rate : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ dgammaSlice shape rate, dgammaMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ dgammaSlice shape rate, ∀ i, 0 ≤ dgammaMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (dgammaSlice shape rate) dgammaMass,
        0 < conditional x dgammaMass p) :
    SampleClaim z x (dgammaSlice shape rate) dgammaMass hold :=
  sample_transfer hn zeroMass positiveMass conditionalWitness

/-- Dweibull, all free, shape1, shape2 and shape1+shape2, zero:modified criterion and
failure transfer and zero:inflated criterion transfer. -/
theorem dweibull_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (shape1 shape2 : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ dweibullSlice shape1 shape2, dweibullMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ dweibullSlice shape1 shape2, ∀ i, 0 ≤ dweibullMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (dweibullSlice shape1 shape2) dweibullMass,
        0 < conditional x dweibullMass p) :
    SampleClaim z x (dweibullSlice shape1 shape2) dweibullMass hold :=
  sample_transfer hn zeroMass positiveMass conditionalWitness

/-- Pig, all free, mean, shape and mean+shape, zero:modified criterion and
failure transfer and zero:inflated criterion transfer. -/
theorem pig_sample {z k : ℕ} (hn : 0 < z+k) (x : Fin k → ℕ)
    (mean shape : Option ℝ) (hold : Option ℝ)
    (zeroMass : ∀ p ∈ pigSlice mean shape, pigMass p 0 ∈ Icc 0 1)
    (positiveMass : ∀ p ∈ pigSlice mean shape, ∀ i, 0 ≤ pigMass p (x i))
    (conditionalWitness : k ≠ 0 →
      ∃ p ∈ positiveSlice (pigSlice mean shape) pigMass,
        0 < conditional x pigMass p) :
    SampleClaim z x (pigSlice mean shape) pigMass hold :=
  sample_transfer hn zeroMass positiveMass conditionalWitness

/-- Dweibull, shape1 held, zero:modified failure on an all-ones
positive component. The excluded positive atom at one is reached
as shape2 tends to infinity. The modified height uses the chosen
zero probability, even when the sample also contains zeros. -/
theorem dweibull_held_probability_ones {z k : ℕ} (hk : 0 < k)
    {q w : ℝ} (hq : q ∈ Ioo 0 1) (hw : 0 < bernoulli z k w)
    (x : Fin k → ℕ) (hx : ∀ i, x i = 1) :
    let L := fun b ↦ Zero6.modified z k (conditional x dweibullMass) ((q,b),w)
    let H := bernoulli z k w
    (∀ b > 0, L b < H) ∧ Tendsto L atTop (𝓝 H) ∧ IsLUB (L '' Ioi 0) H := by
  dsimp only
  let A := fun b : ℝ ↦ (q-q^((2 : ℝ)^b))/q
  have ha (b : ℝ) : conditional x dweibullMass (q,b) = A b ^ k := by
    have h0 : dweibullMass (q,b) 0 = 1-q := by simp [dweibullMass]
    have h1 : dweibullMass (q,b) 1 = q-q^((2 : ℝ)^b) := by
      norm_num [dweibullMass]
    simp [conditional,hx,h0,h1,A,div_pow]
  let L := fun b : ℝ ↦ Zero6.modified z k (conditional x dweibullMass) ((q,b),w)
  have he (b : ℝ) : L b = bernoulli z k w * A b ^ k := by
    simp [L,Zero6.modified,hk.ne',ha]
  have hlt (b : ℝ) (hb : 0 < b) : L b < bernoulli z k w := by
    have hA : A b ∈ Ico (0 : ℝ) 1 := by
      unfold A
      have hq2 : q^((2 : ℝ)^b) ≤ q := by
        conv_rhs => rw [← rpow_one q]
        exact rpow_le_rpow_of_exponent_ge hq.1 hq.2.le (one_le_rpow (by norm_num) hb.le)
      exact ⟨div_nonneg (sub_nonneg.mpr hq2) hq.1.le,
        (div_lt_one hq.1).mpr (sub_lt_self _ (rpow_pos_of_pos hq.1 _))⟩
    rw [he]
    exact mul_lt_of_lt_one_right hw (pow_lt_one₀ hA.1 hA.2 hk.ne')
  have hA : Tendsto A atTop (𝓝 1) := by
    have hP := (tendsto_rpow_atTop_of_base_lt_one q (by linarith [hq.1]) hq.2).comp
      (tendsto_rpow_atTop_of_base_gt_one 2 (by norm_num))
    have h := (tendsto_const_nhds (x := q)).sub hP |>.div_const q
    simpa [A, Function.comp_def, hq.1.ne'] using h
  have hlim : Tendsto L atTop (𝓝 (bernoulli z k w)) := by
    rw [show L = (fun b ↦ bernoulli z k w * A b ^ k) from funext he]
    simpa only [one_pow,mul_one] using (hA.pow k).const_mul (bernoulli z k w)
  refine ⟨hlt,hlim,⟨?_,?_⟩⟩
  · rintro _ ⟨b,hb,rfl⟩
    exact (hlt b hb).le
  · intro a ha
    exact le_of_tendsto hlim ((eventually_gt_atTop (0 : ℝ)).mono
      fun b hb ↦ ha ⟨b,hb,rfl⟩)

/-- Dweibull, shape1 held, zero:inflated criterion on samples whose
positive counts are all one. Every compatible held inflation weight
has a strict excluded Bernoulli face on zero and one. -/
theorem dweibull_held_probability_inflated_ones {z k : ℕ} (hk : 0 < k)
    {q rho : ℝ} (hq : q ∈ Ioo 0 1) (hr : rho ∈ Ico 0 1)
    (x : Fin k → ℕ) (hx : ∀ i, x i = 1) :
    let L := fun b ↦ Zero6Inflated.likelihood z x dweibullMass ((q,b),rho)
    let H := bernoulli z k (Zero6Inflated.effective (1-q) rho)
    (∀ b > 0, L b < H) ∧ Tendsto L atTop (𝓝 H) ∧ IsLUB (L '' Ioi 0) H := by
  let w := Zero6Inflated.effective (1-q) rho
  have hw0 : 0 < w :=
    add_pos_of_nonneg_of_pos hr.1 (mul_pos (sub_pos.mpr hr.2) (sub_pos.mpr hq.2))
  have hw1 : 0 < 1-w := by
    have he : 1-w = (1-rho)*q := by unfold w Zero6Inflated.effective; ring
    rw [he]
    exact mul_pos (sub_pos.mpr hr.2) hq.1
  have hb := mul_pos (pow_pos hw0 z) (pow_pos hw1 k)
  have H := dweibull_held_probability_ones hk hq hb x hx
  have he (b : ℝ) :
      Zero6Inflated.likelihood z x dweibullMass ((q,b),rho) =
        Zero6.modified z k (conditional x dweibullMass) ((q,b),w) := by
    have hu : dweibullMass (q,b) 0 = 1-q := by simp [dweibullMass]
    have hU : dweibullMass (q,b) 0 < 1 := by rw [hu]; linarith [hq.1]
    have hW : dweibullMass (q,b) 0 + (1-dweibullMass (q,b) 0)*rho = w := by
      rw [hu]
      unfold w Zero6Inflated.effective
      ring
    have hF := Trunc3Zero.inflated_factorization z x
      (dweibullMass (q,b) 0) rho hU (dweibullMass (q,b))
    rw [hW] at hF
    simpa only [Zero6Inflated.likelihood, hu, ← show
      Zero6Inflated.effective (1-q) rho = (1-q)+(1-(1-q))*rho by
        unfold Zero6Inflated.effective; ring, Zero6.modified, ite_eq_right hk.ne',
      conditional] using hF
  dsimp only
  simp_rw [he]
  exact H

end

end UnivariateML.Zero6Families

end
