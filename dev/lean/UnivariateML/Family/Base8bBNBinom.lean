module

public import UnivariateML.Family.Base8BNBinom
public import UnivariateML.Family.FinishBNBinom
public import UnivariateML.Family.NegBinomial
public import UnivariateML.Generic.Base8bGammaRatio
import Mathlib.Analysis.SpecialFunctions.Gamma.Beta

/-!
# Beta-negative-binomial fixed symmetric holds

## Deviations

The fixed-hold escaping-sequence statements identify the fitted face at
every fixed count. The attainment comparison uses both coordinate score equations to
specify stationary conditional profile ordinates. For binary data at held alpha
the diagonal improvement excludes off-diagonal winners. The diagonal singleton,
golden diagonal exclusion, joint three-shape escapes, and integer-hold
set-elimination algorithms remain open.
-/

set_option linter.style.header false

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators

namespace UnivariateML.Base8bBNBinom

open Discrete3BNBinom

private noncomputable def rise (k : ℕ) (z : ℝ) :=
  ∏ j ∈ Finset.range k, (z + j)

private theorem rise_pos (k : ℕ) {z : ℝ} (hz : 0 < z) : 0 < rise k z :=
  Finset.prod_pos fun _ _ ↦ by positivity

private theorem gamma_nat_add (k : ℕ) {z : ℝ} (hz : 0 < z) :
    Gamma (z + k) = Gamma z * rise k z := by
  induction k with
  | zero => simp [rise]
  | succ k ih =>
    rw [Nat.cast_succ, ← add_assoc, Gamma_add_one (by positivity : z + (k : ℝ) ≠ 0),
      ih]
    unfold rise
    rw [Finset.prod_range_succ]
    ring

/-- Beta-negative-binomial mass in the zero-cell and count-ratio chart. -/
noncomputable def gammaMass (k : ℕ) (r a b : ℝ) : ℝ :=
  (Gamma (a+b) * Gamma (a+r) / (Gamma a * Gamma (a+r+b))) *
    ((∏ j ∈ Finset.range k, ((r+j)*(b+j) / ((a+r+b+j)*((j : ℝ)+1)))))

private theorem count_product (k : ℕ) (r a b : ℝ) :
    gammaMass k r a b =
      (Gamma (a+b) * Gamma (a+r) / (Gamma a * Gamma (a+r+b))) *
        (rise k r * rise k b / (rise k (a+r+b) * k.factorial)) := by
  rw [gammaMass]
  congr 1
  simp only [Finset.prod_div_distrib, Finset.prod_mul_distrib]
  unfold rise
  congr 2
  induction k with
  | zero => simp
  | succ k ih =>
    rw [Finset.prod_range_succ, Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ, ih, mul_comm]

/-- At either symmetric positive integer hold, every count mass reduces
to the same explicit rational expression in the two free shapes. -/
theorem integer_held_shape_count_mass {r a : ℝ} (hr : 0 < r) (ha : 0 < a)
    {b : ℕ} (k : ℕ) :
    gammaMass k r a b =
      ((∏ j ∈ Finset.range b, (a + j)) / (∏ j ∈ Finset.range b, (a+r+j))) *
        (((∏ j ∈ Finset.range k, (r+j)) * (∏ j ∈ Finset.range k, ((b : ℝ)+j))) /
          ((∏ j ∈ Finset.range k, (a+r+(b : ℝ)+j)) * k.factorial)) ∧
    gammaMass k (b : ℝ) a r = gammaMass k r a (b : ℝ) := by
  have H := count_product k r a (b : ℝ)
  constructor
  · rw [H, gamma_nat_add b ha, gamma_nat_add b (add_pos ha hr)]
    have h₁ := (Gamma_pos_of_pos ha).ne'
    have h₂ := (Gamma_pos_of_pos (add_pos ha hr)).ne'
    have h₃ := (rise_pos b (add_pos ha hr)).ne'
    have he : Gamma a * rise b a * Gamma (a+r) /
        (Gamma a * (Gamma (a+r) * rise b (a+r))) = rise b a / rise b (a+r) := by
      field_simp
    rw [he]
    rfl
  · rw [count_product k (b : ℝ) a r, H, add_right_comm a (b : ℝ) r, mul_comm (Gamma (a+r))]
    ring

private noncomputable def gammaRatio (b a : ℝ) := Gamma (a+b) / Gamma a

private theorem gammaMass_swap (k : ℕ) (r a b : ℝ) :
    gammaMass k b a r = gammaMass k r a b := by
  simp only [gammaMass, add_right_comm a b r, mul_comm (Gamma (a+r)), mul_comm (a := b + _)]

private theorem zero_gammaRatio {a r b : ℝ} (ha : 0 < a) (hr : 0 < r) (hb : 0 < b) :
    Gamma (a+b) * Gamma (a+r) / (Gamma a * Gamma (a+r+b)) =
      gammaRatio b a / gammaRatio b (a+r) := by
  unfold gammaRatio
  field_simp [(Gamma_pos_of_pos ha).ne', (Gamma_pos_of_pos (add_pos ha hr)).ne',
    (Gamma_pos_of_pos (show 0 < a+r+b by positivity)).ne']

/-- At fixed positive beta, if the two free shapes have diverging total
and a limiting mixing proportion, all fixed counts converge. Endpoint
proportions give the zero atom or total loss of mass at finite counts. -/
theorem held_beta_growing_total {ι : Type*} {l : Filter ι} {a r : ι → ℝ} {b p : ℝ}
    (hb : 0 < b) (hp : p ∈ Icc 0 1)
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < r t)
    (hT : Tendsto (fun t ↦ a t + r t) l atTop)
    (hP : Tendsto (fun t ↦ a t / (a t+r t)) l (𝓝 p)) (k : ℕ) :
    Tendsto (fun t ↦ gammaMass k (r t) (a t) b) l
      (𝓝 (p^b * (∏ j ∈ Finset.range k, ((1-p)*(b+j)/((j : ℝ)+1))))) := by
  let T (t : ι) := a t+r t
  have hz : Tendsto (fun t ↦ gammaRatio b (a t) / gammaRatio b (a t+r t)) l (𝓝 (p^b)) :=
    Base8bGammaRatio.scaled_limit hb hp.1 (hpos.mono fun _ h ↦ h.1) hT hP
  have hQ : Tendsto (fun t ↦ r t / T t) l (𝓝 (1-p)) := by
    apply ((tendsto_const_nhds (x := (1 : ℝ))).sub hP).congr'
    filter_upwards [hpos] with t ht
    change 1 - a t / T t = r t / T t
    have hD : 0 < T t := add_pos ht.1 ht.2
    field_simp [hD.ne']
    dsimp [T]
    ring
  have hi := tendsto_inv_atTop_zero.comp hT
  have hF (j : ℕ) : Tendsto (fun t ↦ ((r t+j)*(b+j)) /
      ((a t+r t+b+j)*((j : ℝ)+1))) l (𝓝 ((1-p)*(b+j)/((j : ℝ)+1))) := by
    have hN : Tendsto (fun t ↦ (r t/T t + (j : ℝ)*(T t)⁻¹)*(b+j)) l
        (𝓝 ((1-p)*(b+j))) := by simpa using (hQ.add (hi.const_mul (j : ℝ))).mul_const _
    have hD : Tendsto (fun t ↦ (1+(b+j)*(T t)⁻¹)*((j : ℝ)+1)) l (𝓝 ((j : ℝ)+1)) := by
      simpa using ((hi.const_mul (b+j)).const_add 1).mul_const ((j : ℝ)+1)
    apply (hN.div hD (by positivity)).congr'
    filter_upwards [hpos] with t ht
    have hT' : 0 < T t := add_pos ht.1 ht.2
    have hh : 0 < a t+r t+b+(j : ℝ) := by have := ht.1; have := ht.2; positivity
    have he : 1+(b+j)*(T t)⁻¹ = (a t+r t+b+j)/T t := by
      field_simp [hT'.ne']
      dsimp [T]
      ring
    change ((r t/T t + (j : ℝ)*(T t)⁻¹)*(b+j)) /
      ((1+(b+j)*(T t)⁻¹)*((j : ℝ)+1)) = _
    rw [he]
    field_simp [hT'.ne', hh.ne']
  have H := hz.mul (tendsto_finsetProd (Finset.range k) (fun j _ ↦ hF j))
  apply H.congr'
  filter_upwards [hpos] with t ht
  simp only [gammaMass, zero_gammaRatio ht.1 ht.2 hb]

private noncomputable def gammaRatioRegular (b a : ℝ) := Gamma (a+b) / Gamma (a+1)

private theorem regular_pos {b a : ℝ} (hb : 0 < b) (ha : 0 ≤ a) :
    0 < gammaRatioRegular b a :=
  div_pos (Gamma_pos_of_pos (by positivity)) (Gamma_pos_of_pos (by positivity))

private theorem continuous_regular {b a : ℝ} (hb : 0 < b) (ha : 0 ≤ a) :
    ContinuousAt (gammaRatioRegular b) a := by
  have hG {t : ℝ} (ht : 0 < t) : ContinuousAt Gamma t :=
    (Real.differentiableAt_Gamma (fun m ↦ by linarith [Nat.cast_nonneg (α := ℝ) m])).continuousAt
  have h₁ : 0 < a+b := by positivity
  have h₂ : 0 < a+1 := by positivity
  have hadd (v : ℝ) : ContinuousAt (fun x : ℝ ↦ x+v) a := by fun_prop
  have hG₁ : ContinuousAt (fun x : ℝ ↦ Gamma (x+b)) a := (hG h₁).comp (f := fun x : ℝ ↦ x+b) (x := a) (hadd b)
  have hG₂ : ContinuousAt (fun x : ℝ ↦ Gamma (x+1)) a := (hG h₂).comp (f := fun x : ℝ ↦ x+1) (x := a) (hadd 1)
  exact hG₁.div hG₂ (Gamma_pos_of_pos h₂).ne'

private theorem zero_regular {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    Gamma (a+b) * Gamma (a+r) / (Gamma a * Gamma (a+r+b)) =
      (a/(a+r)) * (gammaRatioRegular b a / gammaRatioRegular b (a+r)) := by
  rw [zero_gammaRatio ha hr hb]
  unfold gammaRatio gammaRatioRegular
  rw [Gamma_add_one ha.ne', Gamma_add_one (add_pos ha hr).ne']
  field_simp [(Gamma_pos_of_pos ha).ne', (Gamma_pos_of_pos (add_pos ha hr)).ne',
    (Gamma_pos_of_pos (show 0 < a+r+b by positivity)).ne', ha.ne', (add_pos ha hr).ne']

/-- At fixed beta the bounded-total boundary has no positive count mass.
At total zero the remaining zero mass is the limiting alpha fraction.
At a positive finite total the endpoint fractions give mass zero or one. -/
theorem held_beta_bounded_boundary {ι : Type*} {l : Filter ι} {a r : ι → ℝ}
    {b p T : ℝ} (hb : 0 < b) (hp : p ∈ Icc 0 1) (hT : 0 ≤ T)
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < r t)
    (hTotal : Tendsto (fun t ↦ a t+r t) l (𝓝 T))
    (hP : Tendsto (fun t ↦ a t/(a t+r t)) l (𝓝 p))
    (hBoundary : T = 0 ∨ p = 0 ∨ p = 1) (k : ℕ) :
    Tendsto (fun t ↦ gammaMass k (r t) (a t) b) l (𝓝 (if k = 0 then p else 0)) := by
  have ha : Tendsto a l (𝓝 (p*T)) := by
    apply (hP.mul hTotal).congr'
    filter_upwards [hpos] with t ht
    exact div_mul_cancel₀ _ (add_pos ht.1 ht.2).ne'
  have hr : Tendsto r l (𝓝 ((1-p)*T)) := by
    convert (hTotal.sub ha) using 1
    · funext t
      ring
    · ring_nf
  have hR : Tendsto (fun t ↦ gammaRatioRegular b (a t)) l
      (𝓝 (gammaRatioRegular b (p*T))) :=
    (continuous_regular hb (mul_nonneg hp.1 hT)).tendsto.comp ha
  have hD : Tendsto (fun t ↦ gammaRatioRegular b (a t+r t)) l (𝓝 (gammaRatioRegular b T)) :=
    (continuous_regular hb hT).tendsto.comp hTotal
  have hz := hP.mul (hR.div hD (regular_pos hb hT).ne')
  have hF (j : ℕ) :
      Tendsto (fun t ↦ ((r t+j)*(b+j)) / ((a t+r t+b+j)*((j : ℝ)+1))) l
        (𝓝 ((((1-p)*T+j)*(b+j)) / ((T+b+j)*((j : ℝ)+1)))) := by
    have hnum' := (hr.add_const (j : ℝ)).mul_const (b+j)
    have hden := ((hTotal.add_const b).add_const (j : ℝ)).mul_const ((j : ℝ)+1)
    have hd : 0 < (T+b+j)*((j : ℝ)+1) := by positivity
    exact hnum'.div hden hd.ne'
  have H := hz.mul (tendsto_finsetProd (Finset.range k) (fun j _ ↦ hF j))
  have eq : (p*(gammaRatioRegular b (p*T)/gammaRatioRegular b T)) *
      (∏ j ∈ Finset.range k, (((1-p)*T+j)*(b+j))/((T+b+j)*((j : ℝ)+1))) =
        if k = 0 then p else 0 := by
    obtain h | h | h := hBoundary
    · subst T
      by_cases hk : k = 0
      · simp [hk, (regular_pos hb (le_refl 0)).ne']
      · have hh := Finset.prod_eq_zero (f := fun j : ℕ ↦ (((1-p)*(0 : ℝ)+j)*(b+j)) /
            ((0+b+j)*((j : ℝ)+1)))
          (Finset.mem_range.mpr (Nat.pos_of_ne_zero hk)) (by simp)
        rw [hh, mul_zero, ite_eq_right hk]
    · simp [h]
    · subst p
      by_cases hk : k = 0
      · simp [hk, (regular_pos hb hT).ne']
      · have hh := Finset.prod_eq_zero (f := fun j : ℕ ↦ (((1-(1 : ℝ))*T+j)*(b+j)) /
            ((T+b+j)*((j : ℝ)+1)))
          (Finset.mem_range.mpr (Nat.pos_of_ne_zero hk)) (by simp)
        rw [hh, mul_zero, ite_eq_right hk]
  rw [eq] at H
  apply H.congr'
  filter_upwards [hpos] with t ht
  simp only [gammaMass, zero_regular ht.2 ht.1 hb, Pi.div_apply]

/-- Fixed-size negative-binomial mass, including its limiting endpoint traces. -/
noncomputable def faceMass (k : ℕ) (b p : ℝ) :=
  p^b * (∏ j ∈ Finset.range k, ((1-p)*(b+j)/((j : ℝ)+1)))

private theorem held_beta_escape_subsequence {a r : ℕ → ℝ} {b : ℝ} (hb : 0 < b)
    (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M)) :
    ∃ φ : ℕ → ℕ, StrictMono φ ∧ ∃ p ∈ Icc (0 : ℝ) 1,
      (∀ k, Tendsto (fun j ↦ gammaMass k (r (φ j)) (a (φ j)) b)
        atTop (𝓝 (faceMass k b p))) ∨
      (∀ k, Tendsto (fun j ↦ gammaMass k (r (φ j)) (a (φ j)) b)
        atTop (𝓝 (if k = 0 then p else 0))) := by
  let T (j : ℕ) := a j+r j
  let P (j : ℕ) := a j/T j
  let U (j : ℕ) := (T j+1)⁻¹
  let C := Icc (0 : ℝ) 1
  have hT j : 0 < T j := add_pos (hpos j).1 (hpos j).2
  have hU j : U j ∈ C := ⟨inv_nonneg.mpr (add_pos (hT j) one_pos).le,
    inv_le_one_of_one_le₀ (by linarith [hT j])⟩
  have hP j : P j ∈ C :=
    ⟨div_nonneg (hpos j).1.le (hT j).le, (div_le_one (hT j)).mpr
      (le_add_of_nonneg_right (hpos j).2.le)⟩
  obtain ⟨⟨u,p⟩, ⟨hu,hp⟩, φ, hφ, hlim⟩ :=
    (isCompact_Icc.prod isCompact_Icc).tendsto_subseq
      (x := fun j ↦ (U j,P j)) (fun j ↦ ⟨hU j,hP j⟩ : ∀ j, (U j,P j) ∈ C ×ˢ C)
  have hUL : Tendsto (fun j ↦ U (φ j)) atTop (𝓝 u) := by simpa only [Function.comp_def] using (continuous_fst.continuousAt.tendsto.comp hlim)
  have hPL : Tendsto (fun j ↦ P (φ j)) atTop (𝓝 p) := by simpa only [Function.comp_def] using (continuous_snd.continuousAt.tendsto.comp hlim)
  have hpos' : ∀ᶠ j in atTop, 0 < a (φ j) ∧ 0 < r (φ j) :=
    Eventually.of_forall fun j ↦ hpos (φ j)
  have hid (j : ℕ) : (U (φ j))⁻¹ - 1 = T (φ j) := by dsimp [U]; simp
  refine ⟨φ,hφ,p,hp,?_⟩
  by_cases hu0 : u = 0
  · left
    have hU0 : Tendsto (fun j ↦ U (φ j)) atTop (𝓝[>] (0 : ℝ)) :=
      tendsto_nhdsWithin_iff.mpr ⟨hu0 ▸ hUL, Eventually.of_forall fun j ↦ by
        change 0 < U (φ j)
        exact inv_pos.mpr (add_pos (hT _) one_pos)⟩
    have hl : Tendsto (fun j ↦ T (φ j)) atTop atTop := by
      have h := tendsto_atTop_add_const_right atTop (-1 : ℝ) hU0.inv_tendsto_nhdsGT_zero
      simpa only [Pi.inv_apply, ← sub_eq_add_neg, hid] using h
    exact fun k ↦ held_beta_growing_total hb hp hpos' hl hPL k
  · right
    let v := u⁻¹-1
    have hu' : 0 < u := lt_of_le_of_ne hu.1 (Ne.symm hu0)
    have hv : 0 ≤ v := by
      dsimp [v]
      exact sub_nonneg.mpr ((one_le_inv₀ hu').mpr hu.2)
    have hV : Tendsto (fun j ↦ T (φ j)) atTop (𝓝 v) := by
      simpa only [hid] using (hUL.inv₀ hu0).sub_const 1
    have hboundary : v = 0 ∨ p = 0 ∨ p = 1 := by
      by_contra! H
      have hv' : 0 < v := lt_of_le_of_ne hv H.1.symm
      have hp' : p ∈ Ioo (0 : ℝ) 1 :=
        ⟨lt_of_le_of_ne hp.1 H.2.1.symm, lt_of_le_of_ne hp.2 H.2.2⟩
      have ha : Tendsto (fun j ↦ a (φ j)) atTop (𝓝 (p*v)) := by
        apply (hPL.mul hV).congr
        intro j
        exact div_mul_cancel₀ _ (hT (φ j)).ne'
      have hr : Tendsto (fun j ↦ r (φ j)) atTop (𝓝 ((1-p)*v)) := by
        convert hV.sub ha using 1
        · funext j
          dsimp [T]; ring
        · ring_nf
      let δ := min (p*v) ((1-p)*v)/2
      let M := max (p*v) ((1-p)*v)+1
      have hδ : 0 < δ := div_pos (lt_min (mul_pos hp'.1 hv') (mul_pos (sub_pos.mpr hp'.2) hv')) (by norm_num)
      have hA : Icc δ M ∈ 𝓝 (p*v) := Icc_mem_nhds
        (by
          have h := min_le_left (p*v) ((1-p)*v)
          dsimp [δ]
          linarith [mul_pos hp'.1 hv'])
        (by dsimp [M]; linarith [le_max_left (p*v) ((1-p)*v)])
      have hR : Icc δ M ∈ 𝓝 ((1-p)*v) := Icc_mem_nhds
        (by
          have h := min_le_right (p*v) ((1-p)*v)
          dsimp [δ]
          linarith [mul_pos (sub_pos.mpr hp'.2) hv'])
        (by dsimp [M]; linarith [le_max_right (p*v) ((1-p)*v)])
      have out := hφ.tendsto_atTop.eventually (he δ M hδ)
      have H' : ∀ᶠ j : ℕ in atTop, False := by
        filter_upwards [ha.eventually hA, hr.eventually hR, out] with j hj hk hl
        exact hl ⟨hj,hk⟩
      obtain ⟨J, hJ⟩ := eventually_atTop.mp H'
      exact hJ J le_rfl
    exact fun k ↦ held_beta_bounded_boundary hb hp hv hpos' hV hPL hboundary k

private theorem face_path {b p : ℝ} (hb : 0 < b) (hp : p ∈ Ioo 0 1) (k : ℕ) :
    Tendsto (fun t : ℝ ↦ gammaMass k ((1-p)*t) (p*t) b) atTop (𝓝 (faceMass k b p)) := by
  have hs : 0 < 1-p := sub_pos.mpr hp.2
  have hpos : ∀ᶠ t : ℝ in atTop, 0 < p*t ∧ 0 < (1-p)*t :=
    (eventually_gt_atTop 0).mono fun _ h ↦ ⟨mul_pos hp.1 h, mul_pos hs h⟩
  have he (t : ℝ) : p*t+(1-p)*t = t := by ring
  have hP : Tendsto (fun t : ℝ ↦ p*t/(p*t+(1-p)*t)) atTop (𝓝 p) := by
    apply tendsto_const_nhds.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    rw [he, mul_div_cancel_right₀ _ ht.ne']
  exact held_beta_growing_total hb ⟨hp.1.le,hp.2.le⟩ hpos
    (by simp only [he]; exact tendsto_id) hP k

private theorem faceMass_pos {b p : ℝ} (hb : 0 < b) (hp : p ∈ Ioo 0 1) (k : ℕ) :
    0 < faceMass k b p := by
  unfold faceMass
  refine mul_pos (rpow_pos_of_pos hp.1 _) (Finset.prod_pos fun (j : ℕ) _ ↦ ?_)
  have := sub_pos.mpr hp.2
  positivity

private theorem face_log {n : ℕ} (x : Fin n → ℕ) {b p : ℝ}
    (hb : 0 < b) (hp : p ∈ Ioo 0 1) :
    log (∏ i, faceMass (x i) b p) = NegBinomial.logLik x b p := by
  have hq : 0 < 1-p := sub_pos.mpr hp.2
  have eq (k : ℕ) :
      log (faceMass k b p) = b * log p + (k : ℝ)*log (1-p) +
        (∑ j ∈ Finset.range k, log (b+j)) - log (k.factorial : ℝ) := by
    unfold faceMass
    rw [log_mul (rpow_pos_of_pos hp.1 _).ne' (Finset.prod_pos fun (j : ℕ) _ ↦
        (show 0 < ((1-p)*(b+j)/((j : ℝ)+1)) by positivity)).ne',
      log_rpow hp.1, log_prod (fun (j : ℕ) _ ↦
        (show 0 < ((1-p)*(b+j)/((j : ℝ)+1)) by positivity).ne')]
    have H (j : ℕ) :
        log ((1-p)*(b+j)/((j : ℝ)+1)) =
          log (1-p) + log (b+j) - log ((j : ℝ)+1) := by
      rw [log_div (by positivity) (by positivity), log_mul hq.ne' (by positivity)]
    have hfact : (∑ j ∈ Finset.range k, log ((j : ℝ)+1)) = log (k.factorial : ℝ) := by
      induction k with
      | zero => simp
      | succ k ih =>
        rw [Finset.sum_range_succ, ih, Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ,
          log_mul (by positivity) (by positivity), add_comm]
    simp only [H, Finset.sum_sub_distrib, Finset.sum_add_distrib, Finset.sum_const,
      Finset.card_range, nsmul_eq_mul, hfact]
    ring
  rw [log_prod (fun i _ ↦ (faceMass_pos hb hp (x i)).ne')]
  simp only [eq, Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.sum_mul]
  unfold NegBinomial.logLik NegBinomial.total NegBinomial.constant
  ring

/-- At a fixed positive beta, any escaping sequence approaching a
global upper likelihood value on positive-count data has a subsequence
converging countwise to the fitted negative binomial. This includes
a boundary height tied by a finite maximizer. -/
theorem held_beta_positive_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b)
    {a r : ℕ → ℝ} (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    {H : ℝ} (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) r a b) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) (r j) (a j) b) atTop (𝓝 H)) :
    let p := NegBinomial.prob x b
    H = (∏ i, faceMass (x i) b p) ∧
      ∃ φ : ℕ → ℕ, StrictMono φ ∧ ∀ k,
        Tendsto (fun j ↦ gammaMass k (r (φ j)) (a (φ j)) b) atTop
          (𝓝 (faceMass k b p)) := by
  intro p
  have hp : p ∈ Ioo (0 : ℝ) 1 :=
    (NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr hx) hb).1
  have hposH : 0 < ∏ i, faceMass (x i) b p :=
    Finset.prod_pos fun i _ ↦ faceMass_pos hb hp (x i)
  have hFace : (∏ i, faceMass (x i) b p) ≤ H := by
    apply le_of_tendsto (tendsto_finsetProd Finset.univ
      (fun i (_ : i ∈ (Finset.univ : Finset (Fin n))) ↦ face_path hb hp (x i)))
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    exact hH _ (mul_pos (sub_pos.mpr hp.2) ht) _ (mul_pos hp.1 ht)
  have hHpos := hposH.trans_le hFace
  obtain ⟨φ,hφ,q,hq,h⟩ := held_beta_escape_subsequence hb hpos he
  have hL := hlim.comp hφ.tendsto_atTop
  obtain ⟨i, hi⟩ := hx
  rcases h with h | h
  · have hEq : H = ∏ i, faceMass (x i) b q :=
      tendsto_nhds_unique hL (tendsto_finsetProd Finset.univ fun i _ ↦ h (x i))
    have hq' : q ∈ Ioo 0 1 := by
      refine ⟨lt_of_le_of_ne hq.1 ?_, lt_of_le_of_ne hq.2 ?_⟩
      · intro heq
        have hall : faceMass (x i) b q = 0 := by
          simp [faceMass, ← heq, zero_rpow hb.ne']
        exact hHpos.ne' (hEq.trans (Finset.prod_eq_zero (Finset.mem_univ i) hall))
      · intro heq
        have hz : (∏ j ∈ Finset.range (x i), ((1-q)*(b+j)/((j : ℝ)+1))) = 0 :=
          Finset.prod_eq_zero (Finset.mem_range.mpr hi) (by simp [heq])
        have hall : faceMass (x i) b q = 0 := by rw [faceMass, hz, mul_zero]
        exact hHpos.ne' (hEq.trans (Finset.prod_eq_zero (Finset.mem_univ i) hall))
    have eq : q = p := by
      by_contra hne
      have hh := (NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr ⟨i,hi⟩) hb).2
        q hq' hne
      rw [NegBinomial.profile, ← face_log x hb hq', ← face_log x hb hp] at hh
      have hlt := (log_lt_log_iff hHpos hposH).mp (by simpa only [← hEq] using hh)
      exact (not_lt_of_ge hFace) hlt
    subst q
    exact ⟨hEq, φ, hφ, h⟩
  · have heq : H = ∏ j : Fin n, (if x j = 0 then q else 0) :=
      tendsto_nhds_unique hL (tendsto_finsetProd Finset.univ fun j _ ↦ h (x j))
    have hz : (∏ j : Fin n, (if x j = 0 then q else 0)) = 0 :=
      Finset.prod_eq_zero (Finset.mem_univ i) (by simp [hi.ne'])
    exact (hHpos.ne' (heq.trans hz)).elim

/-- The symmetric held-size version has the same optimized NB face. -/
theorem held_size_positive_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b)
    {a r : ℕ → ℝ} (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    {H : ℝ} (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) b a r) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) b (a j) (r j)) atTop (𝓝 H)) :
    let p := NegBinomial.prob x b
    H = (∏ i, faceMass (x i) b p) ∧
      ∃ φ : ℕ → ℕ, StrictMono φ ∧ ∀ k,
        Tendsto (fun j ↦ gammaMass k b (a (φ j)) (r (φ j))) atTop
          (𝓝 (faceMass k b p)) := by
  simp only [gammaMass_swap _ _ _ b] at hH hlim ⊢
  exact held_beta_positive_maximizing_escape hn x hx hb hpos he hH hlim

private theorem gammaMass_pos (k : ℕ) {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    0 < gammaMass k r a b := by
  rw [count_product]
  exact mul_pos
    (div_pos (mul_pos (Gamma_pos_of_pos (add_pos ha hb)) (Gamma_pos_of_pos (add_pos ha hr)))
      (mul_pos (Gamma_pos_of_pos ha) (Gamma_pos_of_pos (by positivity))))
    (div_pos (mul_pos (rise_pos _ hr) (rise_pos _ hb)) (mul_pos (rise_pos _ (by positivity))
      (by positivity)))

private theorem log_gammaMass (k : ℕ) {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    log (gammaMass k r a b) =
      log (Gamma (a+r)) + log (Gamma (a+b)) - log (Gamma a) -
        log (Gamma (a+r+b+(k : ℝ))) +
        (log (Gamma (r+(k : ℝ))) - log (Gamma r) + log (Gamma (b+(k : ℝ))) -
          log (Gamma b) - log (k.factorial : ℝ)) := by
  have hG {z : ℝ} (hz : 0 < z) : Gamma z ≠ 0 := (Gamma_pos_of_pos hz).ne'
  have hR {m : ℕ} {z : ℝ} (hz : 0 < z) : rise m z ≠ 0 := (rise_pos _ hz).ne'
  have hT : 0 < a+r+b := by positivity
  have hfact : (k.factorial : ℝ) ≠ 0 := by positivity
  rw [count_product, log_mul (div_ne_zero (mul_ne_zero (hG (add_pos ha hb)) (hG (add_pos ha hr)))
      (mul_ne_zero (hG ha) (hG hT))) (div_ne_zero (mul_ne_zero (hR hr) (hR hb))
        (mul_ne_zero (hR hT) hfact)),
    log_div (mul_ne_zero (hG (add_pos ha hb)) (hG (add_pos ha hr))) (mul_ne_zero (hG ha) (hG hT)),
    log_div (mul_ne_zero (hR hr) (hR hb)) (mul_ne_zero (hR hT) hfact),
    log_mul (hG (add_pos ha hb)) (hG (add_pos ha hr)), log_mul (hG ha) (hG hT),
    log_mul (hR hr) (hR hb), log_mul (hR hT) hfact,
    gamma_nat_add k hr, gamma_nat_add k hb, gamma_nat_add k hT,
    log_mul (hG hr) (hR hr), log_mul (hG hb) (hR hb), log_mul (hG hT) (hR hT)]
  ring

/-- The product of the gamma-chart masses gives the existing exposed
average log objective, with the usual sample-size factor. -/
theorem gamma_likelihood_eq_exp_averageLogMass {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    (∏ i, gammaMass (x i) r a b) =
      exp ((n : ℝ) * FinishBNBinom.averageLogMass x r a b) := by
  have hpos : 0 < ∏ i, gammaMass (x i) r a b :=
    Finset.prod_pos fun i _ ↦ gammaMass_pos (x i) hr ha hb
  apply (log_injOn_pos hpos (exp_pos _))
  rw [log_exp, log_prod (fun i _ ↦ (gammaMass_pos (x i) hr ha hb).ne')]
  unfold FinishBNBinom.averageLogMass
  rw [mul_comm (n : ℝ), div_mul_cancel₀ _ (Nat.cast_pos.mpr hn).ne']
  exact Finset.sum_congr rfl fun i _ ↦ log_gammaMass (x i) hr ha hb

private theorem zero_path {b a : ℝ} (hb : 0 < b) (ha : 0 < a) (k : ℕ) :
    Tendsto (fun r : ℝ ↦ gammaMass k r a b) (𝓝[>] 0)
      (𝓝 (if k = 0 then 1 else 0)) := by
  have hp : ∀ᶠ r : ℝ in 𝓝[>] 0, 0 < a ∧ 0 < r :=
    (show ∀ᶠ r : ℝ in 𝓝[>] 0, r ∈ Ioi (0 : ℝ) from self_mem_nhdsWithin).mono
      (fun _ h ↦ ⟨ha,h⟩)
  have hT : Tendsto (fun r : ℝ ↦ a+r) (𝓝[>] 0) (𝓝 a) := by
    simpa only [add_zero] using
      (show ContinuousAt (fun r : ℝ ↦ a+r) 0 by fun_prop).tendsto.mono_left nhdsWithin_le_nhds
  have hP : Tendsto (fun r : ℝ ↦ a/(a+r)) (𝓝[>] 0) (𝓝 1) := by
    have H := (tendsto_const_nhds (x := a)).div hT ha.ne'
    rw [div_self ha.ne'] at H
    exact H
  exact held_beta_bounded_boundary hb ⟨zero_le_one,le_rfl⟩ ha.le hp hT hP (Or.inr (Or.inr rfl)) k

private theorem zero_escaping {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) {b H : ℝ} (hb : 0 < b) {a r : ℕ → ℝ}
    (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) r a b) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) (r j) (a j) b) atTop (𝓝 H)) :
    H = 1 ∧ ∃ φ : ℕ → ℕ, StrictMono φ ∧ ∀ k,
      Tendsto (fun j ↦ gammaMass k (r (φ j)) (a (φ j)) b) atTop
        (𝓝 (if k = 0 then 1 else 0)) := by
  have hf : 1 ≤ H := by
    have HP := tendsto_finsetProd Finset.univ
      (fun i (_ : i ∈ (Finset.univ : Finset (Fin n))) ↦ zero_path hb one_pos (x i))
    simp only [hx, ite_true, Finset.prod_const_one] at HP
    apply le_of_tendsto HP
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    simpa only [hx] using hH t ht 1 one_pos
  obtain ⟨φ,hφ,p,hp,h⟩ := held_beta_escape_subsequence hb hpos he
  have hL := hlim.comp hφ.tendsto_atTop
  have hstrict {q : ℝ} (hq : 0 ≤ q) (hq' : q < 1) : q^n < 1 := by
    exact pow_lt_one₀ hq hq' hn.ne'
  rcases h with h | h
  · have hEq : H = (p^b)^n := by
      have H' := tendsto_nhds_unique hL (tendsto_finsetProd Finset.univ fun i _ ↦ h (x i))
      simpa only [hx, faceMass, Finset.range_zero, Finset.prod_empty, mul_one,
        Finset.prod_const, Finset.card_univ, Fintype.card_fin] using H'
    have hp1 : p = 1 := by
      by_contra hh
      have hp' := lt_of_le_of_ne hp.2 hh
      have hz := (rpow_lt_one_iff' hp.1 hb).mpr hp'
      exact (hstrict (rpow_nonneg hp.1 _) hz).not_ge (hEq ▸ hf)
    subst p
    refine ⟨by simpa using hEq,φ,hφ,fun k ↦ ?_⟩
    by_cases hk : k = 0
    · subst k
      simpa [faceMass] using h 0
    · have hz : (∏ j ∈ Finset.range k, ((1-(1 : ℝ))*(b+j)/((j : ℝ)+1))) = 0 :=
        Finset.prod_eq_zero (Finset.mem_range.mpr (Nat.pos_of_ne_zero hk)) (by simp)
      simpa only [faceMass, hz, mul_zero, ite_eq_right hk] using h k
  · have hEq : H = p^n := by
      have H' := tendsto_nhds_unique hL (tendsto_finsetProd Finset.univ fun i _ ↦ h (x i))
      simpa only [hx, ite_true, Finset.prod_const, Finset.card_univ, Fintype.card_fin] using H'
    have hp1 : p = 1 := by
      by_contra hh
      exact (hstrict hp.1 (lt_of_le_of_ne hp.2 hh)).not_ge (hEq ▸ hf)
    subst p
    exact ⟨by simpa using hEq, φ, hφ,h⟩

private theorem held_beta_escaping_subsequence {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b)
    {a r : ℕ → ℝ} (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    {H : ℝ} (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) r a b) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) (r j) (a j) b) atTop (𝓝 H)) :
    let p := NegBinomial.prob x b
    let m := fun k ↦ if (∀ i, x i = 0) then (if k = 0 then 1 else 0) else faceMass k b p
    H = (∏ i, m (x i)) ∧ ∃ φ : ℕ → ℕ, StrictMono φ ∧ ∀ k,
      Tendsto (fun j ↦ gammaMass k (r (φ j)) (a (φ j)) b) atTop (𝓝 (m k)) := by
  classical
  intro p m
  by_cases hx : ∀ i, x i = 0
  · obtain ⟨hH', φ, hφ, h⟩ := zero_escaping hn x hx hb hpos he hH hlim
    exact ⟨by simp [m,hx,hH'], φ,hφ,by simpa [m,hx] using h⟩
  · have hx' : ∃ i, 0 < x i := by
      obtain ⟨i, hi⟩ := not_forall.mp hx
      exact ⟨i,Nat.pos_of_ne_zero hi⟩
    simpa only [m, ite_eq_right hx] using
      held_beta_positive_maximizing_escape hn x hx' hb hpos he hH hlim

/-- The fitted escape laws at held beta, including all-zero data and
the case where a finite fit ties the optimized negative-binomial face. -/
theorem held_beta_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b)
    {a r : ℕ → ℝ} (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    {H : ℝ} (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) r a b) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) (r j) (a j) b) atTop (𝓝 H)) :
    let p := NegBinomial.prob x b
    let m := fun k ↦ if (∀ i, x i = 0) then (if k = 0 then 1 else 0) else faceMass k b p
    H = (∏ i, m (x i)) ∧ ∀ k,
      Tendsto (fun j ↦ gammaMass k (r j) (a j) b) atTop (𝓝 (m k)) := by
  classical
  intro p m
  refine ⟨(held_beta_escaping_subsequence hn x hb hpos he hH hlim).1, fun k ↦ ?_⟩
  rw [Metric.tendsto_nhds]
  intro ε hε
  by_contra hbad
  obtain ⟨φ,hφ,hout⟩ := extraction_of_frequently_atTop
    (show ∃ᶠ j in atTop, ¬ dist (gammaMass k (r j) (a j) b) (m k) < ε by
      simpa only [Filter.Frequently, not_not] using hbad)
  obtain ⟨_,ψ,hψ,hlim'⟩ := held_beta_escaping_subsequence hn x hb
    (fun j ↦ hpos (φ j)) (fun δ M hδ ↦ hφ.tendsto_atTop.eventually (he δ M hδ))
    hH (hlim.comp hφ.tendsto_atTop)
  have hnear := (Metric.tendsto_nhds.mp (hlim' k) ε hε).exists
  obtain ⟨j,hj⟩ := hnear
  exact hout (ψ j) hj

/-- The fitted escape laws at held size, in the same count chart. -/
theorem held_size_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b)
    {a r : ℕ → ℝ} (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    {H : ℝ} (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) b a r) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) b (a j) (r j)) atTop (𝓝 H)) :
    let p := NegBinomial.prob x b
    let m := fun k ↦ if (∀ i, x i = 0) then (if k = 0 then 1 else 0) else faceMass k b p
    H = (∏ i, m (x i)) ∧ ∀ k,
      Tendsto (fun j ↦ gammaMass k b (a j) (r j)) atTop (𝓝 (m k)) := by
  classical
  simp only [gammaMass_swap _ _ _ b] at hH hlim ⊢
  exact held_beta_maximizing_escape hn x hb hpos he hH hlim

private noncomputable def likelihood {n : ℕ} (x : Fin n → ℕ) (b : ℝ) (q : ℝ × ℝ) :=
  ∏ i, gammaMass (x i) q.1 q.2 b

private theorem face_le_optimum {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b) {p : ℝ} (hp : p ∈ Icc 0 1) :
    ∏ i, faceMass (x i) b p ≤ ∏ i, faceMass (x i) b (NegBinomial.prob x b) := by
  have h := NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr hx) hb
  have hp₀ := h.1
  have hpos : 0 < ∏ i, faceMass (x i) b (NegBinomial.prob x b) :=
    Finset.prod_pos fun i _ ↦ faceMass_pos hb hp₀ (x i)
  by_cases hq : p ∈ Ioo 0 1
  · by_cases hq' : p = NegBinomial.prob x b
    · simp [hq']
    · have H := (h.2 p hq hq').le
      rw [NegBinomial.profile, ← face_log x hb hq, ← face_log x hb hp₀] at H
      exact (log_le_log_iff (Finset.prod_pos fun i _ ↦ faceMass_pos hb hq (x i)) hpos).mp H
  · obtain ⟨i, hi⟩ := hx
    have hzero : faceMass (x i) b p = 0 := by
      have hh : p = 0 ∨ p = 1 := by
        by_contra! he
        exact hq ⟨lt_of_le_of_ne hp.1 he.1.symm, lt_of_le_of_ne hp.2 he.2⟩
      rcases hh with rfl | rfl
      · simp [faceMass, zero_rpow hb.ne']
      · have hz : (∏ j ∈ Finset.range (x i), ((1-(1 : ℝ))*(b+j)/((j : ℝ)+1))) = 0 :=
          Finset.prod_eq_zero (Finset.mem_range.mpr hi) (by simp)
        rw [faceMass, hz, mul_zero]
    rw [Finset.prod_eq_zero (Finset.mem_univ i) hzero]
    exact hpos.le

private theorem exteriors_below {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b) {v : ℝ}
    (hv : (∏ i, faceMass (x i) b (NegBinomial.prob x b)) < v) :
    ∃ δ M : ℝ, 0 < δ ∧ ∀ r > 0, ∀ a > 0, v ≤ likelihood x b (r,a) →
      r ∈ Icc δ M ∧ a ∈ Icc δ M := by
  classical
  by_contra! h
  have H (j : ℕ) : ∃ r > 0, ∃ a > 0, v ≤ likelihood x b (r,a) ∧
      ¬ (r ∈ Icc (1/((j : ℝ)+1)) ((j : ℝ)+1) ∧
        a ∈ Icc (1/((j : ℝ)+1)) ((j : ℝ)+1)) := by
    obtain ⟨r, hr, a, ha, hv', hh⟩ := h (1/((j : ℝ)+1)) ((j : ℝ)+1) (by positivity)
    exact ⟨r,hr,a,ha,hv',fun h ↦ hh h.1 h.2⟩
  choose r hr a ha hv' hrect using H
  have hescape (δ M : ℝ) (hδ : 0 < δ) :
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M) := by
    have hTop : Tendsto (fun j : ℕ ↦ (j : ℝ)+1) atTop atTop :=
      tendsto_atTop_add_const_right _ _ tendsto_natCast_atTop_atTop
    have hinv : Tendsto (fun j : ℕ ↦ 1/((j : ℝ)+1)) atTop (𝓝 (0 : ℝ)) := by
      simpa only [one_div, Function.comp_def] using tendsto_inv_atTop_zero.comp hTop
    filter_upwards [hinv.eventually_lt_const hδ, hTop.eventually_gt_atTop M] with j hj₁ hj₂ hj₃
    exact hrect j ⟨⟨hj₁.le.trans hj₃.2.1,hj₃.2.2.trans hj₂.le⟩,
      ⟨hj₁.le.trans hj₃.1.1,hj₃.1.2.trans hj₂.le⟩⟩
  obtain ⟨φ, _, q, hq, h⟩ := held_beta_escape_subsequence hb (fun j ↦ ⟨ha j,hr j⟩) hescape
  rcases h with h | h
  · have hbound : v ≤ ∏ i, faceMass (x i) b q := by
      apply ge_of_tendsto (tendsto_finsetProd Finset.univ fun i _ ↦ h (x i))
      exact Eventually.of_forall fun j ↦ hv' (φ j)
    exact hv.not_ge (hbound.trans (face_le_optimum hn x hx hb hq))
  · obtain ⟨i, hi⟩ := hx
    have hz : (∏ j : Fin n, (if x j = 0 then q else 0)) = 0 :=
      Finset.prod_eq_zero (Finset.mem_univ i) (by simp [hi.ne'])
    have hlim := tendsto_finsetProd Finset.univ (fun j (_ : j ∈ Finset.univ) ↦ h (x j))
    rw [hz] at hlim
    have hh : v ≤ 0 := ge_of_tendsto hlim (Eventually.of_forall fun j ↦ hv' (φ j))
    have hp := (NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr ⟨i,hi⟩) hb).1
    exact hv.not_ge (hh.trans (Finset.prod_nonneg fun i _ ↦ (faceMass_pos hb hp (x i)).le))

private theorem likelihood_continuous {n : ℕ} (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b)
    {q : ℝ × ℝ} (hr : 0 < q.1) (ha : 0 < q.2) :
    ContinuousAt (likelihood x b) q := by
  have hG {f : ℝ × ℝ → ℝ} (h : ContinuousAt f q) (hp : 0 < f q) :
      ContinuousAt (fun p ↦ Gamma (f p)) q :=
    ((Real.differentiableAt_Gamma
      (fun m ↦ by linarith [Nat.cast_nonneg (α := ℝ) m])).continuousAt
      ).comp (f := f) (x := q) h
  unfold likelihood gammaMass
  apply tendsto_finsetProd
  intro i _
  apply ContinuousAt.mul
  · apply ContinuousAt.div
    · exact (hG (f := fun p ↦ p.2+b) (by fun_prop) (by positivity)).mul
        (hG (f := fun p ↦ p.2+p.1) (by fun_prop) (by positivity))
    · exact (hG (f := fun p ↦ p.2) (by fun_prop) ha).mul
        (hG (f := fun p ↦ p.2+p.1+b) (by fun_prop) (by positivity))
    · exact (mul_pos (Gamma_pos_of_pos ha)
        (Gamma_pos_of_pos (show 0 < q.2+q.1+b by positivity))).ne'
  · apply tendsto_finsetProd
    intro j _
    apply ContinuousAt.div
    · fun_prop
    · fun_prop
    · positivity

/-- With fixed positive beta and positive-count data, attainment is
equivalent to some finite likelihood reaching the optimized NB face.
This comparison retains finite equality cases. -/
theorem held_beta_face_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ ∏ i, gammaMass (x i) q.1 q.2 b
    let F := ∏ i, faceMass (x i) b (NegBinomial.prob x b)
    (∃ q ∈ D, IsMaxOn L D q) ↔ ∃ q ∈ D, F ≤ L q := by
  intro D L F
  have hF : ∀ H, (∀ q ∈ D, L q ≤ H) → F ≤ H := by
    intro H hH
    have hp := (NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr hx) hb).1
    apply le_of_tendsto (tendsto_finsetProd Finset.univ
      (fun i (_ : i ∈ (Finset.univ : Finset (Fin n))) ↦ face_path hb hp (x i)))
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    exact hH ((1-NegBinomial.prob x b)*t,NegBinomial.prob x b*t)
      ⟨mul_pos (sub_pos.mpr hp.2) ht, mul_pos hp.1 ht⟩
  constructor
  · rintro ⟨q, hq, hmax⟩
    exact ⟨q,hq,hF _ (fun _ hr ↦ hmax hr)⟩
  · rintro ⟨q, hq, hqF⟩
    by_cases hw : ∃ q ∈ D, F < L q
    · obtain ⟨w, hwD, hwF⟩ := hw
      obtain ⟨δ,M,hδ,hRect⟩ := exteriors_below hn x hx hb hwF
      let K := Icc δ M ×ˢ Icc δ M
      have hwK : w ∈ K := hRect _ hwD.1 _ hwD.2 le_rfl
      have hKD : K ⊆ D := fun z hz ↦ ⟨hδ.trans_le hz.1.1,hδ.trans_le hz.2.1⟩
      have hc : ContinuousOn L K := fun z hz ↦
        (likelihood_continuous x hb (hKD hz).1 (hKD hz).2).continuousWithinAt
      obtain ⟨z,hz,hm⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨w,hwK⟩ hc
      refine ⟨z,hKD hz,fun t ht ↦ ?_⟩
      by_cases h : t ∈ K
      · exact hm h
      · have h' : L t < L w := lt_of_not_ge (fun hh ↦ h (hRect _ ht.1 _ ht.2 hh))
        exact h'.le.trans (hm hwK)
    · refine ⟨q,hq,fun t ht ↦ ?_⟩
      exact (le_of_not_gt (fun hh ↦ hw ⟨t,ht,hh⟩)).trans hqF

/-- The same finite-versus-face comparison with size held. -/
theorem held_size_face_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ ∏ i, gammaMass (x i) b q.2 q.1
    let F := ∏ i, faceMass (x i) b (NegBinomial.prob x b)
    (∃ q ∈ D, IsMaxOn L D q) ↔ ∃ q ∈ D, F ≤ L q := by
  simpa only [gammaMass_swap _ _ _ b] using held_beta_face_comparison hn x hx hb

private theorem gamma_product_limit {a r b : ℝ} (ha : 0 < a) (hr : 0 < r) (hb : 0 < b) :
    Tendsto (fun n : ℕ ↦ ∏ j ∈ Finset.range (n+1),
      (1 + r*b/((a+(j : ℝ))*(a+r+b+(j : ℝ))))) atTop
      (𝓝 (Gamma a * Gamma (a+r+b) / (Gamma (a+r)*Gamma (a+b)))) := by
  have hd : 0 < Gamma (a+r)*Gamma (a+b) :=
    mul_pos (Gamma_pos_of_pos (by positivity)) (Gamma_pos_of_pos (by positivity))
  have H := ((GammaSeq_tendsto_Gamma a).mul (GammaSeq_tendsto_Gamma (a+r+b))).div
    ((GammaSeq_tendsto_Gamma (a+r)).mul (GammaSeq_tendsto_Gamma (a+b))) hd.ne'
  apply H.congr'
  filter_upwards [eventually_gt_atTop (0 : ℕ)] with n hn
  have hn' : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hprod (z : ℝ) (hz : 0 < z) :
      0 < ∏ j ∈ Finset.range (n+1), (z+(j : ℝ)) :=
    Finset.prod_pos fun j _ ↦ by positivity
  have he (j : ℕ) :
      1 + r*b/((a+j)*(a+r+b+j)) =
        ((a+r+j)*(a+b+j))/((a+j)*(a+r+b+j)) := by
    field_simp
    ring
  simp only [Pi.div_apply]
  simp_rw [he, Finset.prod_div_distrib, Finset.prod_mul_distrib, GammaSeq]
  have hp : (n : ℝ)^a * (n : ℝ)^(a+r+b) = (n : ℝ)^(a+r) * (n : ℝ)^(a+b) := by
    rw [← rpow_add hn', ← rpow_add hn']
    congr 1
    ring
  have ha' := (hprod a ha).ne'
  have hT := (hprod (a+r+b) (by positivity)).ne'
  have har := (hprod (a+r) (by positivity)).ne'
  have hab := (hprod (a+b) (by positivity)).ne'
  field_simp [ha', hT, har, hab]
  exact hp

private theorem gammaMass_zero_lt_one {a r b : ℝ} (ha : 0 < a) (hr : 0 < r) (hb : 0 < b) :
    gammaMass 0 r a b < 1 := by
  let f (j : ℕ) := 1 + r*b/((a+(j : ℝ))*(a+r+b+(j : ℝ)))
  have hf (j : ℕ) : 1 < f j := by dsimp [f]; exact lt_add_of_pos_right _ (by positivity)
  have hG : 0 < Gamma a * Gamma (a+r+b) :=
    mul_pos (Gamma_pos_of_pos ha) (Gamma_pos_of_pos (by positivity))
  have hG' : 0 < Gamma (a+r) * Gamma (a+b) :=
    mul_pos (Gamma_pos_of_pos (by positivity)) (Gamma_pos_of_pos (by positivity))
  have hle : f 0 ≤ Gamma a * Gamma (a+r+b) / (Gamma (a+r)*Gamma (a+b)) := by
    apply ge_of_tendsto (gamma_product_limit ha hr hb)
    apply Eventually.of_forall
    intro n
    change f 0 ≤ ∏ j ∈ Finset.range (n+1), f j
    rw [← Finset.prod_erase_mul (Finset.range (n+1)) f (Finset.mem_range.mpr (Nat.succ_pos n))]
    exact le_mul_of_one_le_left (zero_le_one.trans (hf 0).le)
      (Finset.one_le_prod₀ (fun j _ ↦ (hf j).le))
  have h := (hf 0).trans_le hle
  have h' : Gamma (a+r)*Gamma (a+b) < Gamma a * Gamma (a+r+b) :=
    (one_lt_div hG').mp h
  simpa [gammaMass, mul_comm (Gamma (a+b))] using (div_lt_one hG).mpr h'

private theorem zero_has_no_maximum {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) {b : ℝ} (hb : 0 < b) :
    ¬ ∃ q ∈ Ioi (0 : ℝ) ×ˢ Ioi 0,
      IsMaxOn (likelihood x b) (Ioi 0 ×ˢ Ioi 0) q := by
  rintro ⟨q,hq,hm⟩
  have hlt : likelihood x b q < 1 := by
    have h₀ := (gammaMass_pos 0 hq.1 hq.2 hb).le
    have h₁ := gammaMass_zero_lt_one hq.2 hq.1 hb
    simpa only [likelihood, hx, Finset.prod_const, Finset.card_univ, Fintype.card_fin]
      using pow_lt_one₀ h₀ h₁ hn.ne'
  have H := tendsto_finsetProd Finset.univ
    (fun i (_ : i ∈ (Finset.univ : Finset (Fin n))) ↦ zero_path hb one_pos (x i))
  simp only [hx, ite_true, Finset.prod_const_one] at H
  apply hlt.not_ge
  apply le_of_tendsto H
  filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
  have hu : likelihood x b (t,1) ≤ likelihood x b q := hm (a := (t,1)) ⟨ht,one_pos⟩
  simpa only [likelihood, hx, Prod.fst, Prod.snd] using hu

private theorem log_likelihood_order {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {b : ℝ} (hb : 0 < b) {q s : ℝ × ℝ}
    (hq : q ∈ Ioi (0 : ℝ) ×ˢ Ioi 0) (hs : s ∈ Ioi (0 : ℝ) ×ˢ Ioi 0) :
    likelihood x b q ≤ likelihood x b s ↔
      FinishBNBinom.averageLogMass x q.1 q.2 b ≤ FinishBNBinom.averageLogMass x s.1 s.2 b := by
  unfold likelihood
  rw [gamma_likelihood_eq_exp_averageLogMass hn x hq.1 hq.2 hb,
    gamma_likelihood_eq_exp_averageLogMass hn x hs.1 hs.2 hb, exp_le_exp]
  exact mul_le_mul_iff_of_pos_left (Nat.cast_pos.mpr hn)

/-- At held beta, attainment is equivalent to a stationary conditional
size profile point reaching the fitted NB face. The stationarity tests
are the two coordinate scores, one of which identifies the conditional
optimum. All-zero data have no finite maximum. -/
theorem held_beta_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ ∏ i, gammaMass (x i) q.1 q.2 b
    let F := ∏ i, faceMass (x i) b (NegBinomial.prob x b)
    let A := FinishBNBinom.averageLogMass x
    (∃ q ∈ D, IsMaxOn L D q) ↔
      (∃ i, 0 < x i) ∧ ∃ q ∈ D,
      HasDerivAt (fun r ↦ A r q.2 b) 0 q.1 ∧
      HasDerivAt (fun a ↦ A q.1 a b) 0 q.2 ∧ F ≤ L q := by
  intro D L F A
  constructor
  · rintro ⟨q,hq,hm⟩
    have hx : ∃ i, 0 < x i := by
      by_contra! h
      exact zero_has_no_maximum hn x (fun i ↦ by have := h i; omega) hb ⟨q,hq,hm⟩
    have rMax : IsMaxOn (fun r ↦ A r q.2 b) (Ioi 0) q.1 := by
      intro r hr
      exact (log_likelihood_order hn x hb (q := (r,q.2)) ⟨hr,hq.2⟩ hq).mp
        (hm (a := (r,q.2)) ⟨hr,hq.2⟩)
    have aMax : IsMaxOn (fun a ↦ A q.1 a b) (Ioi 0) q.2 := by
      intro a ha
      exact (log_likelihood_order hn x hb (q := (q.1,a)) ⟨hq.1,ha⟩ hq).mp
        (hm (a := (q.1,a)) ⟨hq.1,ha⟩)
    obtain ⟨r,_,hr,_,_,hR⟩ := FinishBNBinom.size_score hn hx hq.2 hb
    obtain ⟨a,_,ha,_,_,hA⟩ := FinishBNBinom.alpha_score hn hx hq.1 hb
    have hqR : q.1 = r := (hR q.1 hq.1).mp rMax
    have hqA : q.2 = a := (hA q.2 hq.2).mp aMax
    have hF : F ≤ L q := by
      have hp := (NegBinomial.heldSize hn ((NegBinomial.mean_pos_iff hn x).mpr hx) hb).1
      apply le_of_tendsto (tendsto_finsetProd Finset.univ
        (fun i (_ : i ∈ (Finset.univ : Finset (Fin n))) ↦ face_path hb hp (x i)))
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
      exact hm (a := ((1-NegBinomial.prob x b)*t,NegBinomial.prob x b*t))
        ⟨mul_pos (sub_pos.mpr hp.2) ht,mul_pos hp.1 ht⟩
    exact ⟨hx,q,hq,hqR ▸ hr,hqA ▸ ha,hF⟩
  · rintro ⟨hx,q,hq,_,_,hF⟩
    exact (held_beta_face_comparison hn x hx hb).mpr ⟨q,hq,hF⟩

private theorem average_log_swap {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    FinishBNBinom.averageLogMass x b a r = FinishBNBinom.averageLogMass x r a b := by
  apply mul_left_cancel₀ (Nat.cast_pos.mpr hn).ne'
  apply exp_injective
  rw [← gamma_likelihood_eq_exp_averageLogMass hn x hb ha hr,
    ← gamma_likelihood_eq_exp_averageLogMass hn x hr ha hb]
  simp only [gammaMass_swap _ _ _ b]

/-- At held size, the two coordinate scores and the optimized NB
likelihood threshold give the symmetric attainment criterion. -/
theorem held_size_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ ∏ i, gammaMass (x i) b q.2 q.1
    let F := ∏ i, faceMass (x i) b (NegBinomial.prob x b)
    let A := FinishBNBinom.averageLogMass x
    (∃ q ∈ D, IsMaxOn L D q) ↔
      (∃ i, 0 < x i) ∧ ∃ q ∈ D,
      HasDerivAt (fun r ↦ A b q.2 r) 0 q.1 ∧
      HasDerivAt (fun a ↦ A b a q.1) 0 q.2 ∧ F ≤ L q := by
  intro D L F A
  have hL : L = fun q : ℝ × ℝ ↦ ∏ i, gammaMass (x i) q.1 q.2 b :=
    funext fun q ↦ by simp only [gammaMass_swap _ _ _ b, L]
  have H := held_beta_criterion hn x hb
  dsimp only at H
  rw [hL, H]
  have hDeriv (q : ℝ × ℝ) (hq : q ∈ D) :
      ((fun r ↦ A b q.2 r) =ᶠ[𝓝 q.1] (fun r ↦ A r q.2 b)) ∧
        ((fun a ↦ A b a q.1) =ᶠ[𝓝 q.2] (fun a ↦ A q.1 a b)) := by
    constructor
    · filter_upwards [Ioi_mem_nhds hq.1] with r hr
      exact average_log_swap hn x hr hq.2 hb
    · filter_upwards [Ioi_mem_nhds hq.2] with a ha
      exact average_log_swap hn x hq.1 ha hb
  constructor
  · rintro ⟨hx,q,hq,hr,ha,hF⟩
    exact ⟨hx,q,hq,hr.congr_of_eventuallyEq (hDeriv q hq).1,
      ha.congr_of_eventuallyEq (hDeriv q hq).2,hF⟩
  · rintro ⟨hx,q,hq,hr,ha,hF⟩
    exact ⟨hx,q,hq,hr.congr_of_eventuallyEq (hDeriv q hq).1.symm,
      ha.congr_of_eventuallyEq (hDeriv q hq).2.symm,hF⟩

private theorem zero_mass_strict_at_fixed_ratio {a r b t : ℝ}
    (ha : 0 < a) (hr : 0 < r) (hb : 0 < b) (ht : 0 < t)
    (hs : 2*t < r+b) (hell : t*t/(a+t+t) = r*b/(a+r+b)) :
    gammaMass 0 r a b < gammaMass 0 t a t := by
  let ell := r*b/(a+r+b)
  let f (r b : ℝ) (j : ℕ) :=
    1+r*b/((a+(j : ℝ))*(a+r+b+(j : ℝ)))
  have hellpos : 0 < ell := by dsimp [ell]; positivity
  have hden (u v : ℝ) (hu : 0 < u) (hv : 0 < v) (j : ℕ) :
      0 < a+u+v+(j : ℝ) := by positivity
  have hprod (u v : ℝ) (hu : 0 < u) (hv : 0 < v) (j : ℕ) :
      0 < f u v j := by dsimp [f]; positivity
  have form (u v : ℝ) (hu : 0 < u) (hv : 0 < v)
      (h : u*v/(a+u+v) = ell) (j : ℕ) :
      f u v j = 1+(ell/(a+(j : ℝ)))*((a+u+v)/(a+u+v+(j : ℝ))) := by
    have hu' : u*v = ell*(a+u+v) := (div_eq_iff (by positivity)).mp h
    dsimp [f]
    rw [hu']
    field_simp [(show 0 < a+(j : ℝ) by positivity).ne', (hden u v hu hv j).ne']
  have hratio (j : ℕ) :
      (a+t+t)/(a+t+t+(j : ℝ)) ≤ (a+r+b)/(a+r+b+(j : ℝ)) := by
    apply (div_le_div_iff₀ (hden t t ht ht j) (hden r b hr hb j)).mpr
    have hh : 0 ≤ ((r+b)-2*t)*(j : ℝ) := mul_nonneg (sub_nonneg.mpr hs.le) (Nat.cast_nonneg _)
    nlinarith only [hh]
  have hstrict :
      (a+t+t)/(a+t+t+(1 : ℝ)) < (a+r+b)/(a+r+b+(1 : ℝ)) := by
    apply (div_lt_div_iff₀ (by positivity) (by positivity)).mpr
    nlinarith only [hs]
  have hall (j : ℕ) : f t t j ≤ f r b j := by
    rw [form t t ht ht hell,form r b hr hb rfl]
    exact add_le_add le_rfl (mul_le_mul_of_nonneg_left (hratio j) (by positivity))
  have h1 : f t t 1 < f r b 1 := by
    rw [form t t ht ht hell,form r b hr hb rfl]
    norm_cast
    exact add_lt_add_of_le_of_lt le_rfl (mul_lt_mul_of_pos_left hstrict (by positivity))
  let c := f r b 1 / f t t 1
  have hc : 1 < c := (one_lt_div (hprod t t ht ht 1)).mpr h1
  have hL (u v : ℝ) (hu : 0 < u) (hv : 0 < v) :
      Tendsto (fun n ↦ ∏ j ∈ Finset.range (n+1), f u v j) atTop
        (𝓝 ((gammaMass 0 u a v)⁻¹)) := by
    simpa [gammaMass,inv_div,mul_comm (Gamma (a+v))] using gamma_product_limit ha hu hv
  have hlim : c*(gammaMass 0 t a t)⁻¹ ≤ (gammaMass 0 r a b)⁻¹ := by
    apply le_of_tendsto_of_tendsto ((hL t t ht ht).const_mul c) (hL r b hr hb)
    filter_upwards [eventually_gt_atTop (0 : ℕ)] with n hn
    have hj : 1 ∈ Finset.range (n+1) := Finset.mem_range.mpr (by omega)
    rw [← Finset.prod_erase_mul _ (f t t) hj, ← Finset.prod_erase_mul _ (f r b) hj,
      mul_left_comm,show c*f t t 1 = f r b 1 by dsimp [c]; field_simp [(hprod t t ht ht 1).ne']]
    exact mul_le_mul_of_nonneg_right
      (Finset.prod_le_prod₀ (fun j _ ↦ (hprod t t ht ht j).le) (fun j _ ↦ hall j))
      (hprod r b hr hb 1).le
  have hG := gammaMass_pos 0 ht ha ht
  have hG' := gammaMass_pos 0 hr ha hb
  apply (inv_lt_inv₀ hG hG').mp
  exact (lt_mul_of_one_lt_left (inv_pos.mpr hG) hc).trans_le hlim

/-- In the held-alpha binary problem, every off-diagonal point is
strictly improved by a diagonal point with the same first count ratio. -/
theorem held_alpha_binary_diagonal {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ 1) {a r b : ℝ} (ha : 0 < a) (hr : 0 < r) (hb : 0 < b)
    (hne : r ≠ b) :
    ∃ t > 0, t*t/(a+t+t) = r*b/(a+r+b) ∧
      (∏ i, gammaMass (x i) r a b) < ∏ i, gammaMass (x i) t a t := by
  let ell := r*b/(a+r+b)
  have hell : 0 < ell := by dsimp [ell]; positivity
  let t := ell+Real.sqrt (ell^2+ell*a)
  have ht : 0 < t := add_pos_of_pos_of_nonneg hell (Real.sqrt_nonneg _)
  have hTS : t*t = ell*(a+t+t) := by
    have hS := Real.sq_sqrt (show 0 ≤ ell^2+ell*a by positivity)
    dsimp [t]
    nlinarith only [hS]
  have hT := (div_eq_iff (show a+t+t ≠ 0 by positivity)).mpr hTS
  have hS : 2*t < r+b := by
    have hprod : r*b = ell*(a+r+b) :=
      (div_eq_iff (show a+r+b ≠ 0 by positivity)).mp rfl
    have hsq := sq_pos_of_ne_zero (sub_ne_zero.mpr hne)
    have hh : 0 < (r+b-2*t)*(r+b+2*t-4*ell) := by
      nlinarith only [hprod,hTS,hsq]
    have hpart : 0 < r+b+2*t-4*ell := by
      have hell' : 4*ell < r+b := by
        dsimp [ell]
        have H : 4*(r*b/(a+r+b)) = (4*r*b)/(a+r+b) := by ring
        rw [H]
        apply (div_lt_iff₀ (by positivity)).mpr
        nlinarith only [hsq, mul_pos ha (add_pos hr hb)]
      linarith only [hell',ht]
    exact sub_pos.mp (pos_of_mul_pos_left hh hpart.le)
  refine ⟨t,ht,hT,?_⟩
  have h0 := zero_mass_strict_at_fixed_ratio ha hr hb ht hS hT
  have h1 : gammaMass 1 r a b < gammaMass 1 t a t := by
    have he (u v : ℝ) : gammaMass 1 u a v =
        gammaMass 0 u a v * (u*v/(a+u+v)) := by simp [gammaMass]
    rw [he,he,hT]
    exact mul_lt_mul_of_pos_right h0 hell
  exact Finset.prod_lt_prod_of_nonempty₀ (s := (Finset.univ : Finset (Fin n)))
    (fun i _ ↦ gammaMass_pos (x i) hr ha hb) (fun i _ ↦ by
      obtain h | h : x i = 0 ∨ x i = 1 := by have := hx i; omega
      · simpa [h] using h0
      · simpa [h] using h1) ⟨⟨0,hn⟩,Finset.mem_univ _⟩

end UnivariateML.Base8bBNBinom

end
