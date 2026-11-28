module

public import UnivariateML.Family.TruncCount
public import UnivariateML.Family.TruncZero
public import UnivariateML.Generic.TruncEstimated

/-! Fixed and estimated windows for regular count families.

## Deviations

The core criterion uses effective integer endpoints. The nominal-window theorem
computes them by rounding and clips the binomial upper endpoint to the held size.
Nominal lower cuts are real and upper cuts can be infinite. Atom tags denote the
corresponding public coefficient. The positive alias is treated separately from
ordinary conditioning. Boundary convergence is stated pointwise in the count. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.Trunc2Known

noncomputable section

open TruncCount


/-- The reciprocal positive mean is the exact public geometric coefficient. -/
theorem geom_zt_reciprocal {m p : ℝ} (hm : 1 ≤ m) :
    (p ∈ Ioc (0 : ℝ) 1 ∧ IsMaxOn (Discrete.Geometric.likelihood (m-1)) (Ioc 0 1) p) ↔
      p = 1 / m := by
  have he : 1 + (m-1) = m := by ring
  constructor
  · rintro ⟨hp, hmax⟩
    simpa only [he] using (Discrete.Geometric.unique_mle (sub_nonneg.mpr hm) p hp).mp hmax
  · rintro rfl
    have hp : 1 / m ∈ Ioc (0 : ℝ) 1 :=
      ⟨one_div_pos.mpr (by linarith), (div_le_one (by linarith)).mpr hm⟩
    exact ⟨hp, (Discrete.Geometric.unique_mle (sub_nonneg.mpr hm) _ hp).mpr (by rw [he])⟩

/-- Finite Poisson windows have one maximizing tilt or admitted atom exactly
when the mean is interior or the mean is zero at the lower cut. -/
private theorem poisson_finite_known {l u : ℕ} (hlu : l < u) {m : ℝ}
    (hm : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let D := finiteCountDomain (l == 0) false
    (∃! p, p ∈ D ∧ IsMaxOn (finiteCountLikelihood l u (poissonCarrier 0) m) D p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ l = 0) := by
  simpa using poisson_bounded_attainment hlu hm false

/-- Finite binomial windows have one maximizing tilt or admitted endpoint
exactly when the sample mean is interior or matches a parent atom. -/
private theorem binom_finite_known {l u N : ℕ} (hlu : l < u) (huN : u ≤ N) {m : ℝ}
    (hm : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let D := finiteCountDomain (l == 0) (u == N)
    (∃! p, p ∈ D ∧ IsMaxOn (finiteCountLikelihood l u (fun k ↦ (N.choose k : ℝ)) m) D p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ l = 0) ∨ (m = u ∧ u = N) := by
  simpa using binom_bounded_attainment hlu huN hm false

/-- The size-zero NB positive-window alias has one maximizing tilt or
the admitted probability-one atom, using the logarithmic carrier. -/
theorem nbinom_zero_alias {m : ℝ} (hm : 1 ≤ m) :
    ∃! p, p ∈ closedLowerDomain (Iio 0) ∧
      IsMaxOn (closedLowerLikelihood logSeriesCarrier (m-1)) (closedLowerDomain (Iio 0)) p := by
  simpa using nbinom_zt_fit (r := 0) le_rfl hm

/-- A finite negative tilt with a separately tagged lower atom. -/
def negativeFinite (l u : ℕ) (c : ℕ → ℝ) (m : ℝ) : Option ℝ → ℝ
  | none => if m = l then (c l)⁻¹ else 0
  | some t => exp (finiteLogLik l u c m t)

def lowerDomain (D : Set ℝ) (atom : Prop) : Set (Option ℝ) :=
  {p | match p with
    | none => atom
    | some t => t ∈ D}

private theorem add_lower_atom {D : Set ℝ} {F : Option ℝ → ℝ}
    (hD : D.Nonempty) (hpos : ∀ t ∈ D, 0 < F (some t)) {admitted bottom eligible : Prop}
    (hzero : ¬ bottom → F none = 0)
    (hstrict : bottom → ∀ t ∈ D, F (some t) < F none)
    (hfail : bottom → ∀ t ∈ D, ¬ IsMaxOn (fun s ↦ F (some s)) D t)
    (hfit : ¬ bottom →
      ((∃ t ∈ D, IsMaxOn (fun s ↦ F (some s)) D t) ↔ eligible))
    (huniq : ¬ bottom → ∀ t ∈ D, ∀ u ∈ D,
      IsMaxOn (fun s ↦ F (some s)) D t → IsMaxOn (fun s ↦ F (some s)) D u → t = u) :
    let A := lowerDomain D admitted
    ((∃ p ∈ A, IsMaxOn F A p) ↔ eligible ∧ ¬ bottom ∨ bottom ∧ admitted) ∧
    ∀ p ∈ A, ∀ q ∈ A, IsMaxOn F A p → IsMaxOn F A q → p = q := by
  dsimp only
  let A := lowerDomain D admitted
  have restrict {t : ℝ} (ht : IsMaxOn F A (some t)) :
      IsMaxOn (fun s ↦ F (some s)) D t := fun _ hu ↦ ht hu
  by_cases hb : bottom
  · have onlyAtom (p : Option ℝ) (hp : p ∈ A) (hm : IsMaxOn F A p) : p = none := by
      cases p with
      | none => rfl
      | some t => exact (hfail hb t hp (restrict hm)).elim
    refine ⟨?_, fun p hp q hq hpm hqm ↦ (onlyAtom p hp hpm).trans (onlyAtom q hq hqm).symm⟩
    constructor
    · rintro ⟨p,hp,hm⟩
      rw [onlyAtom p hp hm] at hp
      exact Or.inr ⟨hb,hp⟩
    · rintro (⟨_,hn⟩ | ⟨_,ha⟩)
      · exact (hn hb).elim
      refine ⟨none,ha,?_⟩
      rintro (_ | t) ht
      · exact le_rfl
      · exact (hstrict hb t ht).le
  · obtain ⟨t₀, ht₀⟩ := hD
    have notAtom : ¬ IsMaxOn F A none := by
      intro H
      exact (hpos t₀ ht₀).not_ge ((hzero hb) ▸ H (show some t₀ ∈ A from ht₀))
    constructor
    · constructor
      · rintro ⟨(_ | t), ht, hmax⟩
        · exact (notAtom hmax).elim
        exact Or.inl ⟨(hfit hb).mp ⟨t,ht,restrict hmax⟩,hb⟩
      · rintro (⟨he,_⟩ | ⟨hb',_⟩)
        · obtain ⟨t,ht,hmax⟩ := (hfit hb).mpr he
          refine ⟨some t,ht,?_⟩
          rintro (_ | u) hu
          · change F none ≤ F (some t)
            rw [hzero hb]
            exact (hpos t ht).le
          · exact hmax hu
        · exact (hb hb').elim
    · rintro (_ | t) ht (_ | u) hu htm hum
      · exact rfl
      · exact (notAtom htm).elim
      · exact (notAtom hum).elim
      · exact congrArg some (huniq hb t ht u hu (restrict htm) (restrict hum))

private theorem increasing_normalizer {l u : ℕ} (hlu : l < u) {c : ℕ → ℝ}
    (hc : ∀ k ∈ Finset.Icc l u, 0 < c k) :
    let Z := fun t : ℝ ↦ ∑ k ∈ Finset.Icc l u, c k * exp (t * ((k : ℝ) - l))
    (∀ t, c l < Z t) ∧ StrictMono Z := by
  dsimp only
  have hl := Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩
  have hu := Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩
  constructor
  · intro t
    let f (k : ℕ) := c k * exp (t * ((k : ℝ) - l))
    have h : f l + f u ≤ ∑ k ∈ Finset.Icc l u, f k := by
      have hsub : ({l,u} : Finset ℕ) ⊆ Finset.Icc l u := by
        simp [Finset.insert_subset_iff,hl,hu]
      simpa [hlu.ne] using Finset.sum_le_sum_of_subset_of_nonneg hsub (f := f)
        (fun k hk _ ↦ (mul_pos (hc k hk) (exp_pos _)).le)
    have hlt := (lt_add_of_pos_right (f l) (mul_pos (hc u hu) (exp_pos _))).trans_le h
    simpa [f] using hlt
  · intro s t hst
    refine Finset.sum_lt_sum (fun k hk ↦ ?_) ⟨u,hu,?_⟩
    · have hkl : (l : ℝ) ≤ k := by exact_mod_cast (Finset.mem_Icc.mp hk).1
      exact mul_le_mul_of_nonneg_left (exp_le_exp.mpr
        (mul_le_mul_of_nonneg_right hst.le (sub_nonneg.mpr hkl))) (hc k hk).le
    · have hlu' : (l : ℝ) < u := by exact_mod_cast hlu
      exact mul_lt_mul_of_pos_left (exp_lt_exp.mpr
        (mul_lt_mul_of_pos_right hst (sub_pos.mpr hlu'))) (hc u hu)

private theorem negative_bottom_eval {l u : ℕ} (hlu : l < u) {c : ℕ → ℝ}
    (hc : ∀ k ∈ Finset.Icc l u, 0 < c k) (t : ℝ) :
    negativeFinite l u c l (some t) =
      (∑ k ∈ Finset.Icc l u, c k * exp (t * ((k : ℝ) - l)))⁻¹ := by
  have hl := Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩
  have hp := Finset.sum_pos (fun k hk ↦ mul_pos (hc k hk) (exp_pos ((k : ℝ) * t))) ⟨l,hl⟩
  change exp ((l : ℝ) * t - log (∑ k ∈ Finset.Icc l u, c k * exp ((k : ℝ) * t))) = _
  rw [exp_sub, exp_log hp, ← inv_div]
  congr 1
  rw [Finset.sum_div]
  apply Finset.sum_congr rfl
  intro k _
  rw [mul_div_assoc, ← exp_sub]
  congr 2
  ring

private theorem carrierMean_lt_upper {l u : ℕ} (hlu : l < u) {c : ℕ → ℝ}
    (hc : ∀ k ∈ Finset.Icc l u, 0 < c k) :
    Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c < u := by
  have hl := Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩
  have hs := Finset.sum_pos hc ⟨l,hl⟩
  apply (div_lt_iff₀ hs).mpr
  rw [Finset.mul_sum]
  refine Finset.sum_lt_sum (fun k hk ↦ ?_) ⟨l, hl, ?_⟩
  · have h : (k : ℝ) ≤ u := by exact_mod_cast (Finset.mem_Icc.mp hk).2
    simpa [mul_comm] using mul_le_mul_of_nonneg_right h (hc k hk).le
  · have h : (l : ℝ) < u := by exact_mod_cast hlu
    simpa [mul_comm] using mul_lt_mul_of_pos_right h (hc l hl)

private theorem negative_finite_known {l u : ℕ} (hlu : l < u) {c : ℕ → ℝ}
    (hc : ∀ k ∈ Finset.Icc l u, 0 < c k) {m : ℝ} (hm : m ∈ Icc (l : ℝ) (u : ℝ))
    (admitted : Prop) :
    let D := lowerDomain (Iio 0) admitted
    let F := negativeFinite l u c m
    let threshold := Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c
    ((∃ p ∈ D, IsMaxOn F D p) ↔ (l : ℝ) < m ∧ m < threshold ∨ m = l ∧ admitted) ∧
      ∀ p ∈ D, ∀ q ∈ D, IsMaxOn F D p → IsMaxOn F D q → p = q := by
  dsimp only
  let T := Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c
  let G := fun t ↦ finiteLogLik l u c m t
  have hconvert (t : ℝ) :
      IsMaxOn (fun s ↦ negativeFinite l u c m (some s)) (Iio 0) t ↔ IsMaxOn G (Iio 0) t := by
    constructor <;> intro H s hs
    · exact exp_le_exp.mp (H hs)
    · exact exp_le_exp.mpr (H hs)
  have hfit (hb : m ≠ l) :
      (∃! t, t ∈ Iio 0 ∧ IsMaxOn G (Iio 0) t) ↔ m < T := by
    have hleft : (l : ℝ) < m := lt_of_le_of_ne hm.1 (Ne.symm hb)
    have hl := Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩
    have hu := Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩
    have hlu' : (l : ℝ) < u := by exact_mod_cast hlu
    rcases hm.2.lt_or_eq with hr | hr
    · exact Truncation.scalar_negative_existsUnique_iff hc hl hu hlu'
        (fun k hk ↦ ⟨by exact_mod_cast (Finset.mem_Icc.mp hk).1,
                       by exact_mod_cast (Finset.mem_Icc.mp hk).2⟩) ⟨hleft,hr⟩
    · have hge : T ≤ m := hr ▸ (carrierMean_lt_upper hlu hc).le
      have H := (Truncation.scalar_negative_upper_failure hc hl hu hlu' hge).2
      exact iff_of_false (fun ⟨t,⟨ht,hmax⟩,_⟩ ↦ H t ht hmax) (not_lt_of_ge hge)
  have hinterior (hb : m ≠ l) (t : ℝ) (ht : t ∈ Iio 0) (hmax : IsMaxOn G (Iio 0) t) :
      m < T := by
    by_contra! hge
    have hl := Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩
    have hu := Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩
    have hlu' : (l : ℝ) < u := by exact_mod_cast hlu
    exact (Truncation.scalar_negative_upper_failure hc hl hu hlu' hge).2 t ht hmax
  have H := add_lower_atom (D := Iio 0) (F := negativeFinite l u c m)
    (admitted := admitted) (bottom := m = l) (eligible := m < T)
    ⟨-1, by norm_num⟩ (fun _ _ ↦ exp_pos _)
    (fun hb ↦ ite_eq_right hb) ?_ ?_ ?_ ?_
  · refine ⟨H.1.trans ?_, H.2⟩
    have Hlt : m ≠ l ↔ (l : ℝ) < m := ⟨fun h ↦ lt_of_le_of_ne hm.1 (Ne.symm h),
      fun h ↦ h.ne'⟩
    change m < T ∧ (m ≠ l) ∨ m = l ∧ admitted ↔ _
    rw [Hlt]
    exact or_congr and_comm Iff.rfl
  · intro hb t _
    subst m
    rw [negative_bottom_eval hlu hc]
    change _ < (if (l : ℝ) = l then (c l)⁻¹ else 0)
    rw [ite_eq_left rfl]
    apply (inv_lt_inv₀ _ _).mpr ((increasing_normalizer hlu hc).1 t)
    · exact (hc l (Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩)).trans
        ((increasing_normalizer hlu hc).1 t)
    · exact hc l (Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩)
  · intro hb t ht H
    subst m
    have ht' : t - 1 ∈ Iio (0 : ℝ) := by
      change t < 0 at ht
      change t - 1 < 0
      linarith
    have he := H ht'
    simp only [negative_bottom_eval hlu hc] at he
    have hpos (s : ℝ) : 0 < ∑ k ∈ Finset.Icc l u, c k * exp (s * ((k : ℝ) - l)) :=
      (hc l (Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩)).trans ((increasing_normalizer hlu hc).1 s)
    exact ((increasing_normalizer hlu hc).2 (sub_lt_self t zero_lt_one)).not_ge
      ((inv_le_inv₀ (hpos (t-1)) (hpos t)).mp he)
  · intro hb
    constructor
    · rintro ⟨t,ht,hmax⟩
      exact hinterior hb t ht ((hconvert t).mp hmax)
    · intro he
      obtain ⟨t,⟨ht,hmax⟩,_⟩ := (hfit hb).mpr he
      exact ⟨t,ht,(hconvert t).mpr hmax⟩
  · intro hb t ht u hu htm hum
    have htm' := (hconvert t).mp htm
    obtain ⟨v,_,hv⟩ := (hfit hb).mpr (hinterior hb t ht htm')
    exact (hv t ⟨ht,htm'⟩).trans (hv u ⟨hu,(hconvert u).mp hum⟩).symm


private theorem nb_pos {r : ℝ} (hr : 0 < r) (k : ℕ) : 0 < negBinomCarrier r k := by
  unfold negBinomCarrier
  have H := Ring.descPochhammer_eq_factorial_smul_choose (r + (k : ℝ) - 1) k
  rw [nsmul_eq_mul] at H
  have hp : 0 < (descPochhammer ℤ k).smeval (r + (k : ℝ) - 1) := by
    rw [Polynomial.descPochhammer_smeval_eq_ascPochhammer,
      Polynomial.ascPochhammer_smeval_cast, Polynomial.ascPochhammer_smeval_eq_eval,
      show r + (k : ℝ) - 1 - k + 1 = r by ring]
    exact ascPochhammer_pos k r hr
  exact pos_of_mul_pos_right (H ▸ hp) (by positivity)


private theorem ray_known {D : Set ℝ} (hD : D.Nonempty) {c : ℕ → ℝ}
    (hc : ∀ k, 0 < c k) {m : ℝ} (hm : 0 ≤ m)
    (hs : ∀ t ∈ D, Summable (fun k : ℕ ↦ c k * exp t ^ k))
    (hfit : (∃! t, t ∈ D ∧ IsMinOn (centeredSum c m) D t) ↔ 0 < m)
    (hfail : m = 0 → ∀ t ∈ D, ¬ IsMinOn (centeredSum c m) D t)
    (admitted : Prop) :
    let A := lowerDomain D admitted
    let F := closedLowerLikelihood c m
    ((∃ p ∈ A, IsMaxOn F A p) ↔ 0 < m ∨ m = 0 ∧ admitted) ∧
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn F A p → IsMaxOn F A q → p = q := by
  dsimp only
  have hs' (t : ℝ) (ht : t ∈ D) :
      Summable (fun k : ℕ ↦ c k * exp (t * ((k : ℝ) - m))) := by
    convert (hs t ht).mul_right (exp (-(t*m))) using 1
    ext k
    rw [show t * ((k : ℝ) - m) = (k : ℝ)*t + -(t*m) by ring, exp_add, exp_nat_mul, mul_assoc]
  have hpos (t : ℝ) (ht : t ∈ D) : 0 < centeredSum c m t :=
    (mul_pos (hc 0) (exp_pos _)).trans_le
      ((hs' t ht).le_tsum 0 (fun k _ ↦ (mul_pos (hc k) (exp_pos _)).le))
  have hconvert (t : ℝ) (ht : t ∈ D) :
      IsMaxOn (fun u ↦ closedLowerLikelihood c m (some u)) D t ↔
        IsMinOn (centeredSum c m) D t :=
    ⟨fun H u hu ↦ (inv_le_inv₀ (hpos u hu) (hpos t ht)).mp (H hu),
     fun H u hu ↦ (inv_le_inv₀ (hpos u hu) (hpos t ht)).mpr (H hu)⟩
  have H := add_lower_atom (F := closedLowerLikelihood c m) hD
    (fun t ht ↦ inv_pos.mpr (hpos t ht)) (admitted := admitted) (bottom := m = 0)
    (eligible := 0 < m) (fun hb ↦ ite_eq_right hb) ?_ ?_ ?_ ?_
  · refine ⟨H.1.trans (or_congr ⟨fun h ↦ h.1, fun h ↦ ⟨h,h.ne'⟩⟩ Iff.rfl), H.2⟩
  · intro hb t ht
    have H := (hs' t ht).sum_le_tsum ({0,1} : Finset ℕ)
      (fun k _ ↦ (mul_pos (hc k) (exp_pos _)).le)
    have H' : c 0 + c 1 * exp t ≤ centeredSum c m t := by
      simpa [centeredSum, hb] using H
    change (centeredSum c m t)⁻¹ < (if m = 0 then (c 0)⁻¹ else 0)
    rw [ite_eq_left hb]
    exact (inv_lt_inv₀ (hpos t ht) (hc 0)).mpr
      ((lt_add_of_pos_right _ (mul_pos (hc 1) (exp_pos _))).trans_le H')
  · intro hb t ht H
    exact hfail hb t ht ((hconvert t ht).mp H)
  · intro hb
    constructor
    · intro _
      exact lt_of_le_of_ne hm (Ne.symm hb)
    · intro he
      obtain ⟨t,⟨ht,hmin⟩,_⟩ := hfit.mpr he
      exact ⟨t,ht,(hconvert t ht).mpr hmin⟩
  · intro hb t ht u hu htm hum
    obtain ⟨v,_,hv⟩ := hfit.mpr (lt_of_le_of_ne hm (Ne.symm hb))
    exact (hv t ⟨ht,(hconvert t ht).mp htm⟩).trans (hv u ⟨hu,(hconvert u hu).mp hum⟩).symm

private theorem poisson_summable (l : ℕ) (t : ℝ) :
    Summable (fun k : ℕ ↦ poissonCarrier l k * exp t ^ k) := by
  have h := (summable_nat_add_iff l).mpr (Real.summable_pow_div_factorial (exp t))
  convert h.mul_left ((exp t ^ l)⁻¹) using 1
  ext k
  rw [poissonCarrier, Nat.add_comm l k, pow_add]
  field_simp

private theorem geom_summable {t : ℝ} (ht : t < 0) :
    Summable (fun k : ℕ ↦ (1 : ℝ) * exp t ^ k) := by
  simpa using summable_geometric_of_lt_one (exp_pos t).le (exp_lt_one_iff.mpr ht)

private theorem nbinom_summable (l : ℕ) (r : ℝ) {t : ℝ} (ht : t < 0) :
    Summable (fun k : ℕ ↦ negBinomCarrier r (l+k) * exp t ^ k) := by
  have hball : exp t ∈ Metric.eball (0 : ℝ) 1 := by
    rw [← ENNReal.ofReal_one, Metric.eball_ofReal, Metric.mem_ball, Real.dist_eq, sub_zero,
      abs_of_pos (exp_pos t)]
    exact exp_lt_one_iff.mpr ht
  have h : Summable (fun k : ℕ ↦ negBinomCarrier r k * exp t ^ k) := by
    simpa [negBinomCarrier, FormalMultilinearSeries.ofScalars, smul_eq_mul] using
      ((Real.one_div_one_sub_rpow_hasFPowerSeriesOnBall_zero r).hasSum_sub hball).summable
  convert ((summable_nat_add_iff l).mpr h).mul_left ((exp t ^ l)⁻¹) using 1
  ext k
  rw [Nat.add_comm l k, pow_add]
  field_simp

/-- A Poisson ray has a unique fit above the lower cut, and includes the
zero-rate atom only at cut zero or at the declared positive alias. -/
private theorem poisson_ray_known (l : ℕ) {m : ℝ} (hm : (l : ℝ) ≤ m)
    (positiveAlias : Prop) (_ha : positiveAlias → l = 1) :
    let D := lowerDomain univ (l = 0 ∨ positiveAlias)
    let F := closedLowerLikelihood (poissonCarrier l) (m-l)
    ((∃ p ∈ D, IsMaxOn F D p) ↔ (l : ℝ) < m ∨ m = l ∧ (l = 0 ∨ positiveAlias)) ∧
     ∀ p ∈ D, ∀ q ∈ D, IsMaxOn F D p → IsMaxOn F D q → p = q := by
  have H := ray_known (c := poissonCarrier l) (D := univ) (m := m-l) univ_nonempty
    (fun _ ↦ one_div_pos.mpr (by positivity)) (sub_nonneg.mpr hm)
    (fun t _ ↦ poisson_summable l t)
    (by simpa using poisson_lower_fit l hm)
    (fun h t _ ↦ by rw [h]; exact poisson_lower_failure l t) (l = 0 ∨ positiveAlias)
  simpa only [sub_pos, sub_eq_zero] using H

/-- A geometric ray has a unique fit above the lower cut, with the
probability-one atom at cut zero or at the declared positive alias. -/
private theorem geom_ray_known (l : ℕ) {m : ℝ} (hm : (l : ℝ) ≤ m)
    (positiveAlias : Prop) (_ha : positiveAlias → l = 1) :
    let D := lowerDomain (Iio 0) (l = 0 ∨ positiveAlias)
    let F := closedLowerLikelihood (fun _ ↦ 1) (m-l)
    ((∃ p ∈ D, IsMaxOn F D p) ↔ (l : ℝ) < m ∨ m = l ∧ (l = 0 ∨ positiveAlias)) ∧
     ∀ p ∈ D, ∀ q ∈ D, IsMaxOn F D p → IsMaxOn F D q → p = q := by
  have H := ray_known (c := fun _ ↦ 1) (D := Iio 0) (m := m-l) ⟨-1,by norm_num⟩
    (fun _ ↦ one_pos) (sub_nonneg.mpr hm) (fun _ h ↦ geom_summable h)
    (by simpa only [mem_Iio, sub_pos] using geom_lower_fit l hm)
    (fun h _ ht ↦ by rw [h]; exact geom_lower_failure ht) (l = 0 ∨ positiveAlias)
  simpa only [sub_pos, sub_eq_zero] using H

/-- Positive-size NB on a ray has the same lower-atom criterion as the
geometric law and no finite upper threshold. -/
private theorem nbinom_ray_known {r : ℝ} (hr : 0 < r) (l : ℕ) {m : ℝ} (hm : (l : ℝ) ≤ m)
    (positiveAlias : Prop) (_ha : positiveAlias → l = 1) :
    let D := lowerDomain (Iio 0) (l = 0 ∨ positiveAlias)
    let F := closedLowerLikelihood (fun k ↦ negBinomCarrier r (l+k)) (m-l)
    ((∃ p ∈ D, IsMaxOn F D p) ↔ (l : ℝ) < m ∨ m = l ∧ (l = 0 ∨ positiveAlias)) ∧
     ∀ p ∈ D, ∀ q ∈ D, IsMaxOn F D p → IsMaxOn F D q → p = q := by
  have H := ray_known (c := fun k ↦ negBinomCarrier r (l+k)) (D := Iio 0) (m := m-l)
    ⟨-1,by norm_num⟩ (fun k ↦ nb_pos hr (l+k)) (sub_nonneg.mpr hm)
    (fun _ h ↦ nbinom_summable l r h)
    (by simpa only [mem_Iio, sub_pos] using nbinom_lower_fit hr l hm)
    (fun h _ ht ↦ by rw [h]; exact nbinom_lower_failure hr l ht) (l = 0 ∨ positiveAlias)
  simpa only [sub_pos, sub_eq_zero] using H

open TruncEstimated

/-- Unnormalized ordinary count weights in the natural chart. The right
tags stand for the lower and upper parent atoms. -/
def countWeights (c : ℕ → ℝ) (topCount : ℕ) : Sum ℝ Bool → ℕ → ℝ
  | .inl t => fun k ↦ c k * exp t ^ k
  | .inr false => fun k ↦ if k = 0 then 1 else 0
  | .inr true => fun k ↦ if k = topCount then 1 else 0

/-- Public coefficient associated to a Poisson chart point. -/
def poissonRate : Sum ℝ Bool → ℝ
  | .inl t => exp t
  | .inr _ => 0

/-- Public coefficient associated to a geometric or NB chart point. -/
def negativeProb : Sum ℝ Bool → ℝ
  | .inl t => 1 - exp t
  | .inr _ => 1

/-- Public probability associated to a binomial chart point. -/
def binomProb : Sum ℝ Bool → ℝ
  | .inl t => exp t / (1 + exp t)
  | .inr false => 0
  | .inr true => 1

def negativeParent : Set (Sum ℝ Bool) :=
  {p | match p with
    | .inl t => t < 0
    | .inr b => b = false}

private theorem weights_summable {D : Set (Sum ℝ Bool)} {c : ℕ → ℝ} {N : ℕ}
    (hs : ∀ t, Sum.inl t ∈ D → Summable (countWeights c N (Sum.inl t))) :
    ∀ p ∈ D, Summable (countWeights c N p) := by
  rintro (t | (_ | _)) hp
  · exact hs t hp
  · change Summable (fun k : ℕ ↦ if k = 0 then (1 : ℝ) else 0)
    exact summable_of_ne_finset_zero (s := {0}) (by simp)
  · change Summable (fun k : ℕ ↦ if k = N then (1 : ℝ) else 0)
    exact summable_of_ne_finset_zero (s := {N}) (by simp)

private theorem binom_summable (N : ℕ) (t : ℝ) :
    Summable (countWeights (fun k ↦ (N.choose k : ℝ)) N (Sum.inl t)) := by
  apply summable_of_ne_finset_zero (s := Finset.range (N+1))
  intro k hk
  have h : N < k := by have hnot := mt Finset.mem_range.mpr hk; omega
  simp [countWeights, Nat.choose_eq_zero_of_lt h]

/-- Nominal cuts for the declared positive-truncation extension. -/
def positiveWindow : Window := (1, ⊤)

/-- Exact maximizer-set extension for the extra atom at one.
The ordinary candidate objective is bounded by one. At a compatible alias,
ordinary ties survive precisely at height one. -/
private theorem atom_alias_join {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    {c : ℕ → ℝ} {N : ℕ} {D : Set (Sum ℝ Bool)}
    (hc : ∀ k, 0 ≤ c k)
    (hs : ∀ t, Sum.inl t ∈ D → Summable (countWeights c N (Sum.inl t)))
    (d : Design) (fixed : Window) :
    let f := countWeights c N
    let C := candidates x f D d fixed
    let a : Sum ℝ Bool × Window := (Sum.inr false, positiveWindow)
    let eligible := allows d fixed positiveWindow ∧ ∀ i, x i = 1
    let A := {q | eligible ∧ q = a}
    let L := fun q ↦ if q = a then 1 else likelihood x (f q.1) (cells q.2)
    (∀ q ∈ C, IsMaxOn L (C ∪ A) q ↔
      IsMaxOn L C q ∧ (eligible → L q = 1)) ∧
    (∀ q ∈ A, IsMaxOn L (C ∪ A) q) := by
  classical
  dsimp only
  let f := countWeights c N
  let C := candidates x f D d fixed
  let a : Sum ℝ Bool × Window := (Sum.inr false, positiveWindow)
  let eligible := allows d fixed positiveWindow ∧ ∀ i, x i = 1
  let A := {q | eligible ∧ q = a}
  let L := fun q ↦ if q = a then 1 else likelihood x (f q.1) (cells q.2)
  have hb (q : Sum ℝ Bool × Window) (hq : q ∈ C ∪ A) : L q ≤ 1 := by
    by_cases he : q = a
    · simp [L,he]
    · rw [show L q = likelihood x (f q.1) (cells q.2) by simp [L,he]]
      rcases hq with hq | hq
      · refine likelihood_le_one hn ?_ (weights_summable hs _ hq.1) hq.2.2
        intro k
        rcases q.1 with t | (_ | _)
        · exact mul_nonneg (hc k) (pow_nonneg (exp_pos t).le _)
        · exact ite_nonneg (by norm_num) le_rfl
        · exact ite_nonneg (by norm_num) le_rfl
      · exact (he hq.2).elim
  change (∀ q ∈ C, IsMaxOn L (C ∪ A) q ↔ IsMaxOn L C q ∧ (eligible → L q = 1)) ∧ _
  constructor
  · intro q hq
    constructor
    · intro H
      refine ⟨fun p hp ↦ H (Or.inl hp), fun he ↦ le_antisymm (hb q (Or.inl hq)) ?_⟩
      simpa [L] using H (show a ∈ C ∪ A from Or.inr ⟨he,rfl⟩)
    · rintro ⟨H,he⟩ p (hp | hp)
      · exact H hp
      · exact (hb p (Or.inr hp)).trans_eq (he hp.1).symm
  · intro q hq p hp
    change L p ≤ L q
    have he : q = a := hq.2
    exact (hb p hp).trans_eq (by simp [L, he])

/-- Ordinary size-zero NB fits exactly all-zero data with zero retained.
Every allowed coefficient has the same atom likelihood. -/
theorem nbinom_zero_ordinary {P : Type*} {D : Set P} {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (d : Design) (fixed : Window) (p : P) (w : Window) :
    let f := fun (_ : P) (k : ℕ) ↦ if k = 0 then (1 : ℝ) else 0
    (p,w) ∈ candidates x f D d fixed ↔
      p ∈ D ∧ allows d fixed w ∧ 0 ∈ cells w ∧ ∀ i, x i = 0 := by
  dsimp only
  constructor
  · intro h
    have hz (i : Fin n) : x i = 0 := by
      have H := (h.2.2 i).2
      change 0 < (if x i = 0 then (1 : ℝ) else 0) at H
      split at H <;> simp_all
    have hw := (h.2.2 ⟨0,hn⟩).1
    exact ⟨h.1,h.2.1,hz ⟨0,hn⟩ ▸ hw,hz⟩
  · rintro ⟨hp,hw,hretain,hz⟩
    exact ⟨hp,hw,fun i ↦ by simp [hz,hretain]⟩

/-- Size-zero NB has exactly one fit on the positive alias when that
nominal window is allowed. This statement uses the shifted logarithmic likelihood. -/
private theorem nbinom_zero_estimated_alias {m : ℝ} (hm : 1 ≤ m) (d : Design) (fixed : Window) :
    let D := closedLowerDomain (Iio 0)
    let C : Set (Option ℝ × Window) :=
      {q | q.1 ∈ D ∧ allows d fixed positiveWindow ∧ q.2 = positiveWindow}
    let L := fun q : Option ℝ × Window ↦ closedLowerLikelihood logSeriesCarrier (m-1) q.1
    (∃! q, q ∈ C ∧ IsMaxOn L C q) ↔ allows d fixed positiveWindow := by
  dsimp only
  constructor
  · rintro ⟨q,⟨hq,_⟩,_⟩
    exact hq.2.1
  · intro ha
    obtain ⟨p,⟨hp,hpmax⟩,hu⟩ := nbinom_zero_alias hm
    refine ⟨(p,positiveWindow), ⟨⟨hp,ha,rfl⟩,fun q hq ↦ hpmax hq.1⟩,?_⟩
    rintro ⟨q,w⟩ ⟨⟨hq,_,hw⟩,hqmax⟩
    exact Prod.ext (hu q ⟨hq,fun r hr ↦ hqmax (show (r,positiveWindow) ∈ _ from ⟨hr,ha,rfl⟩)⟩) hw

private theorem product_tilt {n : ℕ} {x : Fin n → ℕ} {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) (c : ℕ → ℝ) (t : ℝ) :
    (∏ i, c (x i) * exp t ^ x i) = (∏ i, c (x i)) * exp (m*t) ^ n := by
  rw [Finset.prod_mul_distrib]
  congr 1
  simp only [← exp_nat_mul, ← Real.exp_sum, ← Finset.sum_mul, hm]
  congr 1
  ring

/-- On a finite retained window the ordinary sample likelihood is the
carrier product times the nth power of its scalar objective. -/
private theorem finite_likelihood_chart {n l u N : ℕ} {x : Fin n → ℕ} {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) {c : ℕ → ℝ}
    (hc : ∀ k ∈ Finset.Icc l u, 0 < c k) (hlu : l ≤ u) (t : ℝ) :
    likelihood x (countWeights c N (Sum.inl t)) (Icc l u) =
      (∏ i, c (x i)) * exp (finiteLogLik l u c m t) ^ n := by
  have hp := Finset.sum_pos (fun k hk ↦ mul_pos (hc k hk) (exp_pos ((k : ℝ) * t)))
    ⟨l,Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩
  have hz : mass (countWeights c N (Sum.inl t)) (Icc l u) =
      ∑ k ∈ Finset.Icc l u, c k * exp ((k : ℝ)*t) := by
    unfold mass
    rw [sum_eq_tsum_indicator]
    congr 1
    funext k
    have he : (Finset.Icc l u : Set ℕ) = Icc l u := by ext j; simp
    rw [he]
    simp only [countWeights, exp_nat_mul]
  change (∏ i, c (x i) * exp t ^ x i) / mass _ _ ^ n =
    (∏ i, c (x i)) * exp (m*t - log (∑ k ∈ Finset.Icc l u, c k * exp ((k : ℝ)*t))) ^ n
  rw [hz, product_tilt hm, exp_sub, exp_log hp, div_pow, mul_div_assoc]

/-- The ordinary ray likelihood has the corresponding centered-sum
representation. The parent carrier is indexed by the original count. -/
private theorem ray_likelihood_chart {n l : ℕ} {x : Fin n → ℕ} {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) (c : ℕ → ℝ) (t : ℝ) :
    likelihood x (countWeights c 0 (Sum.inl t)) (Ici l) =
      (∏ i, c (x i)) *
        (closedLowerLikelihood (fun k ↦ c (l+k)) (m-l) (some t)) ^ n := by
  let e : ℕ → Ici l := fun k ↦ ⟨l+k, by simp⟩
  have he : Function.Bijective e := by
    constructor
    · intro a b hab
      have H := congrArg Subtype.val hab
      simpa [e] using H
    · intro ⟨k,hk⟩
      exact ⟨k-l, Subtype.ext (by change l ≤ k at hk; change l + (k-l) = k; omega)⟩
  have hz : mass (countWeights c 0 (Sum.inl t)) (Ici l) =
      exp (m*t) * centeredSum (fun k ↦ c (l+k)) (m-l) t := by
    unfold mass centeredSum
    rw [← tsum_subtype, ← (Equiv.ofBijective e he).tsum_eq, ← tsum_mul_left]
    congr 1
    funext k
    change c (l+k) * exp t ^ (l+k) = exp (m*t) * (c (l+k) * exp (t * ((k : ℝ)-(m-l))))
    rw [← exp_nat_mul, mul_left_comm, ← exp_add]
    congr 2
    push_cast
    ring
  change (∏ i, c (x i) * exp t ^ x i) / mass _ _ ^ n = _
  rw [hz, product_tilt hm, closedLowerLikelihood, mul_pow]
  rw [mul_div_assoc, div_mul_eq_div_div, div_self (pow_pos (exp_pos _) _).ne', one_div, inv_pow]

private theorem finite_unique_of_exists {l u : ℕ} (hlu : l < u) {c : ℕ → ℝ}
    (hc : ∀ k ∈ Finset.Icc l u, 0 < c k) {m : ℝ} {lower upper : Bool} :
    let D := finiteCountDomain lower upper
    let F := finiteCountLikelihood l u c m
    (∃ p ∈ D, IsMaxOn F D p) → ∃! p, p ∈ D ∧ IsMaxOn F D p := by
  dsimp only
  let D := finiteCountDomain lower upper
  let F := finiteCountLikelihood l u c m
  have hl := Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩
  have hu := Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩
  have hlu' : (l : ℝ) < u := by exact_mod_cast hlu
  have hin (s : ℝ) : Sum.inl s ∈ D := trivial
  have tilt_max {t : ℝ} (ht : IsMaxOn F D (Sum.inl t)) :
      IsMaxOn (finiteLogLik l u c m) univ t := fun s _ ↦ exp_le_exp.mp (ht (hin s))
  have tilt_unique (t s : ℝ) (ht : IsMaxOn (finiteLogLik l u c m) univ t)
      (hs : IsMaxOn (finiteLogLik l u c m) univ s) : t = s := by
    have H := Truncation.maximizers_constant_direction (Finset.Icc l u)
      (fun k ↦ (k : ℝ)) c ⟨l,hl⟩ hc ht hs l hl u hu
    change (l : ℝ) * (t-s) = (u : ℝ) * (t-s) at H
    exact sub_eq_zero.mp ((mul_eq_zero.mp (show ((u : ℝ)-l)*(t-s)=0 by linarith)).resolve_left
      (sub_pos.mpr hlu').ne')
  have no_atom_and_tilt (b : Bool) (hbm : IsMaxOn F D (Sum.inr b))
      (t : ℝ) (htm : IsMaxOn F D (Sum.inl t)) : False := by
    have hpos : 0 < F (Sum.inr b) := (exp_pos (finiteLogLik l u c m t)).trans_le (hbm (hin t))
    have hedge : m = l ∨ m = u := by
      cases b
      · exact Or.inl (by by_contra h; simp [F,finiteCountLikelihood,h] at hpos)
      · exact Or.inr (by by_contra h; simp [F,finiteCountLikelihood,h] at hpos)
    have H : ∃! s, IsMaxOn (finiteLogLik l u c m) univ s :=
      ⟨t, tilt_max htm, fun s hs ↦ tilt_unique s t hs (tilt_max htm)⟩
    have hmean := (Truncation.scalar_existsUnique_iff hc hl hu hlu'
      (fun k hk ↦ ⟨by exact_mod_cast (Finset.mem_Icc.mp hk).1,
                      by exact_mod_cast (Finset.mem_Icc.mp hk).2⟩) m).mp H
    rcases hedge with h | h
    · exact hmean.1.ne' h
    · exact hmean.2.ne h
  rintro ⟨p,hp,hpm⟩
  refine ⟨p,⟨hp,hpm⟩,?_⟩
  rintro q ⟨hq,hqm⟩
  rcases q with t | b
  · rcases p with s | b
    · exact congrArg Sum.inl (tilt_unique t s (tilt_max hqm) (tilt_max hpm))
    · exact (no_atom_and_tilt b hpm t hqm).elim
  · rcases p with t | a
    · exact (no_atom_and_tilt b hqm t hpm).elim
    · have ha : 0 < F (Sum.inr a) := (exp_pos (finiteLogLik l u c m 0)).trans_le (hpm (hin 0))
      have hb : 0 < F (Sum.inr b) := (exp_pos (finiteLogLik l u c m 0)).trans_le (hqm (hin 0))
      congr 1
      cases a <;> cases b
      · rfl
      · have h₁ : m = l := by by_contra h; simp [F,finiteCountLikelihood,h] at ha
        have h₂ : m = u := by by_contra h; simp [F,finiteCountLikelihood,h] at hb
        exact (hlu'.ne (h₁.symm.trans h₂)).elim
      · have h₁ : m = l := by by_contra h; simp [F,finiteCountLikelihood,h] at hb
        have h₂ : m = u := by by_contra h; simp [F,finiteCountLikelihood,h] at ha
        exact (hlu'.ne (h₁.symm.trans h₂)).elim
      · rfl

private theorem mean_eq_bottom {n l : ℕ} {x : Fin n → ℕ} {m : ℝ}
    (hn : 0 < n) (hm : ∑ i, (x i : ℝ) = n * m) (hx : ∀ i, l ≤ x i) :
    m = l ↔ ∀ i, x i = l := by
  have hp : (0 : ℝ) < n := by exact_mod_cast hn
  constructor
  · intro he
    subst m
    intro i
    by_contra hi
    have H : (∑ _ : Fin n, (l : ℝ)) < ∑ j, (x j : ℝ) :=
      Finset.sum_lt_sum (fun j _ ↦ by exact_mod_cast hx j)
        ⟨i,Finset.mem_univ i,by exact_mod_cast lt_of_le_of_ne (hx i) (Ne.symm hi)⟩
    simp [hm] at H
  · intro he
    have H : (n : ℝ) * l = n * m := by simpa [he] using hm
    exact (mul_left_cancel₀ hp.ne' H).symm

private theorem mean_eq_top {n u : ℕ} {x : Fin n → ℕ} {m : ℝ}
    (hn : 0 < n) (hm : ∑ i, (x i : ℝ) = n * m) (hx : ∀ i, x i ≤ u) :
    m = u ↔ ∀ i, x i = u := by
  have hp : (0 : ℝ) < n := by exact_mod_cast hn
  constructor
  · intro he
    subst m
    intro i
    by_contra hi
    have H : (∑ j, (x j : ℝ)) < ∑ _ : Fin n, (u : ℝ) :=
      Finset.sum_lt_sum (fun j _ ↦ by exact_mod_cast hx j)
        ⟨i,Finset.mem_univ i,by exact_mod_cast lt_of_le_of_ne (hx i) hi⟩
    simp [hm] at H
  · intro he
    have H : (n : ℝ) * u = n * m := by simpa [he] using hm
    exact (mul_left_cancel₀ hp.ne' H).symm

private theorem atom_likelihood {n j : ℕ} {x : Fin n → ℕ} {A : Set ℕ} (hj : j ∈ A)
    (hx : ∀ i, x i = j) :
    likelihood x (fun k ↦ if k = j then (1 : ℝ) else 0) A = 1 := by
  classical
  let f := fun k ↦ if k = j then (1 : ℝ) else 0
  have he : A.indicator f = ({j} : Set ℕ).indicator f := by
    funext k
    by_cases h : k = j
    · subst k
      simp [hj]
    · by_cases hk : k ∈ A <;> simp [hk,h,f]
  unfold likelihood mass
  change _ / (∑' k, A.indicator f k)^n = _
  rw [he]
  exact likelihood_singleton (f := f) hx (show 0 < f j by simp [f])

private theorem carrier_power_atom {n j : ℕ} {x : Fin n → ℕ} (c : ℕ → ℝ)
    (hx : ∀ i, x i = j) (hc : 0 < c j) :
    (∏ i, c (x i)) * (c j)⁻¹ ^ n = 1 := by
  simp [hx, hc.ne']

private theorem positive_chart_max {P : Type*} {E D : Set P} {F L : P → ℝ} {C : ℝ}
    (hn : 0 < n) (hC : 0 < C) (hED : E ⊆ D)
    (hval : ∀ p ∈ E, 0 < F p ∧ L p = C * F p ^ n)
    (houtside : ∀ p ∈ D \ E, F p = 0) (hE : E.Nonempty) :
    ∀ p, (p ∈ E ∧ IsMaxOn L E p) ↔ p ∈ D ∧ IsMaxOn F D p := by
  intro p
  have hcomp (q : P) (hq : q ∈ E) (p : P) (hp : p ∈ E) :
      L q ≤ L p ↔ F q ≤ F p := by
    rw [(hval q hq).2,(hval p hp).2,mul_le_mul_iff_right₀ hC]
    exact ⟨fun h ↦ le_of_not_gt (fun hh ↦
      (pow_lt_pow_left₀ hh (hval p hp).1.le hn.ne').not_ge h),
      fun h ↦ pow_le_pow_left₀ (hval q hq).1.le h _⟩
  constructor
  · rintro ⟨hp,hpm⟩
    refine ⟨hED hp,fun q hq ↦ ?_⟩
    by_cases hq' : q ∈ E
    · exact (hcomp q hq' p hp).mp (hpm hq')
    · change F q ≤ F p
      rw [houtside q ⟨hq,hq'⟩]
      exact (hval p hp).1.le
  · rintro ⟨hp,hpm⟩
    have hp' : p ∈ E := by
      by_contra he
      obtain ⟨r,hr⟩ := hE
      exact (hval r hr).1.not_ge (houtside p ⟨hp,he⟩ ▸ hpm (hED hr))
    exact ⟨hp',fun q hq ↦ (hcomp q hq p hp').mpr (hpm (hED hq))⟩

private theorem finite_sample_transfer {n l u N : ℕ} (hn : 0 < n) (hlu : l ≤ u)
    {x : Fin n → ℕ} (hx : ∀ i, x i ∈ Icc l u) {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) {c : ℕ → ℝ}
    (hc : ∀ k ∈ Finset.Icc l u, 0 < c k)
    {parent : Set (Sum ℝ Bool)} {lower upper : Bool}
    (hint : ∃ t : ℝ, Sum.inl t ∈ parent)
    (hl : Sum.inr false ∈ parent → (lower = true ↔ l = 0))
    (hu : Sum.inr true ∈ parent → (upper = true ↔ u = N))
    (hcap : Sum.inr true ∈ parent → u ≤ N) :
    let f := countWeights c N
    let E := {p ∈ parent | ∀ i, 0 < f p (x i)}
    let D := parent ∩ finiteCountDomain lower upper
    let F := finiteCountLikelihood l u c m
    let L := fun p ↦ likelihood x (f p) (Icc l u)
    ∀ p, (p ∈ E ∧ IsMaxOn L E p) ↔ p ∈ D ∧ IsMaxOn F D p := by
  dsimp only
  let f := countWeights c N
  let E := {p ∈ parent | ∀ i, 0 < f p (x i)}
  let D := parent ∩ finiteCountDomain lower upper
  let F := finiteCountLikelihood l u c m
  let L := fun p ↦ likelihood x (f p) (Icc l u)
  let C := ∏ i, c (x i)
  have hpos (i : Fin n) : 0 < c (x i) := hc _ (Finset.mem_Icc.mpr (hx i))
  have hC : 0 < C := Finset.prod_pos (fun i _ ↦ hpos i)
  have bot := mean_eq_bottom hn hm (fun i ↦ (hx i).1)
  have top := mean_eq_top hn hm (fun i ↦ (hx i).2)
  have hE (t : ℝ) (ht : Sum.inl t ∈ parent) : Sum.inl t ∈ E :=
    ⟨ht, fun i ↦ mul_pos (hpos i) (pow_pos (exp_pos t) _)⟩
  have bot_compat (hp : Sum.inr false ∈ parent) :
      Sum.inr false ∈ E ↔ lower = true ∧ m = l := by
    constructor
    · intro H
      have he (i : Fin n) : x i = 0 := by
        have h := H.2 i
        by_contra he
        simp [f,countWeights,he] at h
      have hl' : l = 0 := by have := (hx ⟨0,hn⟩).1; rw [he] at this; omega
      exact ⟨(hl hp).mpr hl', bot.mpr (fun i ↦ (he i).trans hl'.symm)⟩
    · rintro ⟨ha,he⟩
      refine ⟨hp,fun i ↦ ?_⟩
      simp [f,countWeights,bot.mp he i,(hl hp).mp ha]
  have top_compat (hp : Sum.inr true ∈ parent) :
      Sum.inr true ∈ E ↔ upper = true ∧ m = u := by
    constructor
    · intro H
      have he (i : Fin n) : x i = N := by
        have h := H.2 i
        by_contra he
        simp [f,countWeights,he] at h
      have hu' : u = N := by
        have Hm := (hx ⟨0,hn⟩).2
        rw [he] at Hm
        exact le_antisymm (hcap hp) Hm
      exact ⟨(hu hp).mpr hu', top.mpr (fun i ↦ (he i).trans hu'.symm)⟩
    · rintro ⟨ha,he⟩
      refine ⟨hp,fun i ↦ ?_⟩
      simp [f,countWeights,top.mp he i,(hu hp).mp ha]
  change ∀ p, (p ∈ E ∧ IsMaxOn L E p) ↔ p ∈ D ∧ IsMaxOn F D p
  have hEn : E.Nonempty := by
    obtain ⟨t,ht⟩ := hint
    exact ⟨Sum.inl t,hE t ht⟩
  refine positive_chart_max hn hC ?_ ?_ ?_ hEn
  · rintro (t | (_ | _)) hp
    · exact ⟨hp.1,trivial⟩
    · exact ⟨hp.1,((bot_compat hp.1).mp hp).1⟩
    · exact ⟨hp.1,((top_compat hp.1).mp hp).1⟩
  · rintro (t | (_ | _)) hp
    · exact ⟨exp_pos _,finite_likelihood_chart hm hc hlu t⟩
    · obtain ⟨ha,he⟩ := (bot_compat hp.1).mp hp
      have hzero := (hl hp.1).mp ha
      have hconst := bot.mp he
      have hmem : l ∈ Icc l u := ⟨le_rfl,hlu⟩
      have hcL := hc l (Finset.mem_Icc.mpr hmem)
      have hL : L (Sum.inr false) = 1 := by
        simpa only [L,f,countWeights,← hzero] using atom_likelihood hmem hconst
      simp only [F,finiteCountLikelihood,he,hL]
      exact ⟨inv_pos.mpr hcL,(carrier_power_atom c hconst hcL).symm⟩
    · obtain ⟨ha,he⟩ := (top_compat hp.1).mp hp
      have hlast := (hu hp.1).mp ha
      have hconst := top.mp he
      have hmem : u ∈ Icc l u := ⟨hlu,le_rfl⟩
      have hcU := hc u (Finset.mem_Icc.mpr hmem)
      have hL : L (Sum.inr true) = 1 := by
        simpa only [L,f,countWeights,← hlast] using atom_likelihood hmem hconst
      simp only [F,finiteCountLikelihood,he,hL]
      exact ⟨inv_pos.mpr hcU,(carrier_power_atom c hconst hcU).symm⟩
  · rintro (t | (_ | _)) hp
    · exact (hp.2 (hE t hp.1.1)).elim
    · have he : m ≠ l := fun he ↦ hp.2 ((bot_compat hp.1.1).mpr ⟨hp.1.2,he⟩)
      simp [F,finiteCountLikelihood,he]
    · have he : m ≠ u := fun he ↦ hp.2 ((top_compat hp.1.1).mpr ⟨hp.1.2,he⟩)
      simp [F,finiteCountLikelihood,he]

private theorem admissible_intersection {lower upper : Bool} {D : Set (Sum ℝ Bool)}
    (h : finiteCountDomain lower upper ⊆ D) :
    D ∩ finiteCountDomain lower upper = finiteCountDomain lower upper :=
  inter_eq_right.mpr h

/-- Known finite Poisson windows, on the sample likelihood used in the
ordinary estimated-cut theorem. -/
private theorem poisson_finite_sample {n l u : ℕ} (hn : 0 < n) (hlu : l < u)
    {x : Fin n → ℕ} (hx : ∀ i, x i ∈ Icc l u) {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) (hb : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let f := countWeights (poissonCarrier 0) 0
    let E := {p ∈ finiteCountDomain true false | ∀ i, 0 < f p (x i)}
    let L := fun p ↦ likelihood x (f p) (Icc l u)
    ((∃ p ∈ E, IsMaxOn L E p) ↔ m ∈ Ioo (l : ℝ) (u : ℝ) ∨ m = l ∧ l = 0) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ m ∈ Ioo (l : ℝ) (u : ℝ) ∨ m = l ∧ l = 0) := by
  dsimp only
  have hc : ∀ k ∈ Finset.Icc l u, 0 < poissonCarrier 0 k :=
    fun _ _ ↦ one_div_pos.mpr (by positivity)
  have H := finite_sample_transfer (N := 0) hn hlu.le hx hm hc
    (parent := finiteCountDomain true false) (lower := l == 0) (upper := false)
    ⟨0,trivial⟩ (fun _ ↦ by simp) (fun h ↦ False.elim (by exact Bool.noConfusion h))
    (fun h ↦ False.elim (by exact Bool.noConfusion h))
  dsimp only at H
  rw [admissible_intersection (by rintro (_ | (_ | _)) _ <;> trivial)] at H
  have hfit := poisson_finite_known hlu hb
  dsimp only at hfit
  refine ⟨?_, (existsUnique_congr H).trans hfit⟩
  change (∃ _, _ ∧ _) ↔ _
  rw [exists_congr H]
  exact ⟨fun h ↦ hfit.mp (finite_unique_of_exists hlu hc h),
    fun h ↦ by obtain ⟨p,hp,_⟩ := hfit.mpr h; exact ⟨p,hp⟩⟩

/-- Known held-size binomial windows on the ordinary sample likelihood. Use
the effective upper bound after clipping to size, including for nominal rays. -/
private theorem binom_finite_sample {n l u N : ℕ} (hn : 0 < n) (hlu : l < u) (huN : u ≤ N)
    {x : Fin n → ℕ} (hx : ∀ i, x i ∈ Icc l u) {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) (hb : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let f := countWeights (fun k ↦ (N.choose k : ℝ)) N
    let E := {p ∈ finiteCountDomain true true | ∀ i, 0 < f p (x i)}
    let L := fun p ↦ likelihood x (f p) (Icc l u)
    let fits := m ∈ Ioo (l : ℝ) (u : ℝ) ∨ m = l ∧ l = 0 ∨ m = u ∧ u = N
    ((∃ p ∈ E, IsMaxOn L E p) ↔ fits) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ fits) := by
  dsimp only
  have hc : ∀ k ∈ Finset.Icc l u, 0 < (N.choose k : ℝ) := fun k hk ↦ by
    exact_mod_cast Nat.choose_pos ((Finset.mem_Icc.mp hk).2.trans huN)
  have H := finite_sample_transfer (N := N) hn hlu.le hx hm hc
    (parent := finiteCountDomain true true) (lower := l == 0) (upper := u == N)
    ⟨0,trivial⟩ (fun _ ↦ by simp) (fun _ ↦ by simp) (fun _ ↦ huN)
  dsimp only at H
  rw [admissible_intersection (by rintro (_ | (_ | _)) _ <;> trivial)] at H
  have hfit := binom_finite_known hlu huN hb
  dsimp only at hfit
  refine ⟨?_, (existsUnique_congr H).trans hfit⟩
  change (∃ _, _ ∧ _) ↔ _
  rw [exists_congr H]
  exact ⟨fun h ↦ hfit.mp (finite_unique_of_exists hlu hc h),
    fun h ↦ by obtain ⟨p,hp,_⟩ := hfit.mpr h; exact ⟨p,hp⟩⟩

/-- For a held-size binomial, the nominal window can be clipped to
`Iic N` without changing the likelihood when all observations are retained. -/
private theorem binom_window_clip {N n : ℕ} {x : Fin n → ℕ} {A : Set ℕ}
    (p : Sum ℝ Bool) :
    let f := countWeights (fun k ↦ (N.choose k : ℝ)) N p
    likelihood x f A = likelihood x f (A ∩ Iic N) := by
  classical
  dsimp only
  let f := countWeights (fun k ↦ (N.choose k : ℝ)) N p
  have hz (k : ℕ) (hk : N < k) : f k = 0 := by
    rcases p with t | (_ | _)
    · simp [f,countWeights,Nat.choose_eq_zero_of_lt hk]
    · simp [f,countWeights,show k ≠ 0 by omega]
    · simp [f,countWeights,hk.ne']
  have he : A.indicator f = (A ∩ Iic N).indicator f := by
    funext k
    by_cases hk : k ≤ N
    · by_cases ha : k ∈ A <;> simp [ha,hk]
    · by_cases ha : k ∈ A <;> simp [ha,hk,hz k (lt_of_not_ge hk)]
  change likelihood x f A = likelihood x f (A ∩ Iic N)
  simp only [likelihood,mass,he]

private def lowerTag : Option ℝ → Sum ℝ Bool
  | none => Sum.inr false
  | some t => Sum.inl t

private theorem lowerTag_injective : Function.Injective lowerTag := by
  rintro (_ | a) (_ | b) h <;> simp_all [lowerTag]

private theorem tagged_max {E : Set (Sum ℝ Bool)} {A : Set (Option ℝ)}
    {L : Sum ℝ Bool → ℝ} {F : Option ℝ → ℝ}
    (h : ∀ p, (p ∈ E ∧ IsMaxOn L E p) ↔ ∃ q ∈ A, IsMaxOn F A q ∧ lowerTag q = p) :
    ((∃ p ∈ E, IsMaxOn L E p) ↔ ∃ q ∈ A, IsMaxOn F A q) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ ∃! q, q ∈ A ∧ IsMaxOn F A q) := by
  constructor
  · constructor
    · rintro ⟨p,hp,hpm⟩
      obtain ⟨q,hq,hqm,_⟩ := (h p).mp ⟨hp,hpm⟩
      exact ⟨q,hq,hqm⟩
    · rintro ⟨q,hq,hqm⟩
      exact ⟨lowerTag q,(h _).mpr ⟨q,hq,hqm,rfl⟩⟩
  · constructor
    · rintro ⟨p,hp,hu⟩
      obtain ⟨q,hq,hqm,he⟩ := (h p).mp hp
      exact ⟨q,⟨hq,hqm⟩,fun r ⟨hr,hrm⟩ ↦ lowerTag_injective
        ((hu _ ((h _).mpr ⟨r,hr,hrm,rfl⟩)).trans he.symm)⟩
    · rintro ⟨q,⟨hq,hqm⟩,hu⟩
      refine ⟨lowerTag q,(h _).mpr ⟨q,hq,hqm,rfl⟩,fun p hp ↦ ?_⟩
      obtain ⟨r,hr,hrm,he⟩ := (h p).mp hp
      exact he.symm.trans (congrArg lowerTag (hu r ⟨hr,hrm⟩))

private theorem lowerTag_max {D : Set ℝ} (admitAtom : Prop) {L : Sum ℝ Bool → ℝ}
    {A : Set (Sum ℝ Bool)}
    (hA : ∀ p, p ∈ A ↔ ∃ q ∈ lowerDomain D admitAtom, lowerTag q = p) :
    ∀ p, (p ∈ A ∧ IsMaxOn L A p) ↔
      ∃ q ∈ lowerDomain D admitAtom, IsMaxOn (L ∘ lowerTag) (lowerDomain D admitAtom) q ∧
        lowerTag q = p := by
  intro p
  constructor
  · rintro ⟨hp,hpm⟩
    obtain ⟨q,hq,rfl⟩ := (hA p).mp hp
    exact ⟨q,hq,fun v hv ↦ hpm ((hA _).mpr ⟨v,hv,rfl⟩),rfl⟩
  · rintro ⟨q,hq,hqm,rfl⟩
    refine ⟨(hA _).mpr ⟨q,hq,rfl⟩,fun p hp ↦ ?_⟩
    obtain ⟨v,hv,rfl⟩ := (hA p).mp hp
    exact hqm hv

private theorem unique_from_pairwise {P : Type*} {E : Set P} {F : P → ℝ} {B : Prop}
    (H : ((∃ p ∈ E, IsMaxOn F E p) ↔ B) ∧
      ∀ p ∈ E, ∀ q ∈ E, IsMaxOn F E p → IsMaxOn F E q → p = q) :
    (∃! p, p ∈ E ∧ IsMaxOn F E p) ↔ B := by
  exact ⟨fun ⟨p,hp,_⟩ ↦ H.1.mp ⟨p,hp⟩,fun h ↦ by
    obtain ⟨p,hp,hm⟩ := H.1.mpr h
    exact ⟨p,⟨hp,hm⟩,fun q ⟨hq,hqm⟩ ↦ H.2 q hq p hp hqm hm⟩⟩

private theorem negative_finite_sample {n l u : ℕ} (hn : 0 < n) (hlu : l < u)
    {x : Fin n → ℕ} (hx : ∀ i, x i ∈ Icc l u) {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) (hb : m ∈ Icc (l : ℝ) (u : ℝ))
    {c : ℕ → ℝ} (hc : ∀ k ∈ Finset.Icc l u, 0 < c k) :
    let f := countWeights c 0
    let E := {p ∈ negativeParent | ∀ i, 0 < f p (x i)}
    let L := fun p ↦ likelihood x (f p) (Icc l u)
    let fits := (l : ℝ) < m ∧
      m < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c ∨ m = l ∧ l = 0
    ((∃ p ∈ E, IsMaxOn L E p) ↔ fits) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ fits) := by
  dsimp only
  have H := finite_sample_transfer (N := 0) hn hlu.le hx hm hc
    (parent := negativeParent) (lower := l == 0) (upper := false)
    ⟨-1,by change (-1 : ℝ) < 0; norm_num⟩ (fun _ ↦ by simp)
    (fun h ↦ by change true = false at h; contradiction)
    (fun h ↦ by change true = false at h; contradiction)
  have hA : ∀ p, p ∈ negativeParent ∩ finiteCountDomain (l == 0) false ↔
      ∃ q ∈ lowerDomain (Iio 0) (l = 0), lowerTag q = p := by
    rintro (t | (_ | _))
    · constructor
      · intro h
        exact ⟨some t,h.1,rfl⟩
      · rintro ⟨(_ | q),hq,he⟩
        · cases he
        · cases he
          exact ⟨hq,trivial⟩
    · constructor
      · intro h
        exact ⟨none,by change l = 0; simpa [finiteCountDomain] using h.2,rfl⟩
      · rintro ⟨(_ | q),hq,he⟩
        · exact ⟨rfl,by simpa [finiteCountDomain] using (show l = 0 from hq)⟩
        · cases he
    · constructor
      · intro h
        have h' : true = false := h.1
        contradiction
      · rintro ⟨(_ | q),hq,he⟩ <;> cases he
  have he : finiteCountLikelihood l u c m ∘ lowerTag = negativeFinite l u c m := by
    funext p
    cases p <;> rfl
  have h := lowerTag_max (L := finiteCountLikelihood l u c m) (l = 0) hA
  rw [he] at h
  have ht := tagged_max fun p ↦ (H p).trans (h p)
  have hf := negative_finite_known hlu hc hb (l = 0)
  exact ⟨ht.1.trans hf.1,ht.2.trans (unique_from_pairwise hf)⟩

/-- The geometric finite-window criterion and uniqueness for the sample
objective used by extremal-cut profiling. -/
private theorem geom_finite_sample {n l u : ℕ} (hn : 0 < n) (hlu : l < u)
    {x : Fin n → ℕ} (hx : ∀ i, x i ∈ Icc l u) {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) (hb : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let f := countWeights (fun _ ↦ 1) 0
    let E := {p ∈ negativeParent | ∀ i, 0 < f p (x i)}
    let L := fun p ↦ likelihood x (f p) (Icc l u)
    let fits := (l : ℝ) < m ∧
      m < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) ∨ m = l ∧ l = 0
    ((∃ p ∈ E, IsMaxOn L E p) ↔ fits) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ fits) :=
  negative_finite_sample hn hlu hx hm hb (fun _ _ ↦ one_pos)

/-- The positive-size NB finite-window criterion and uniqueness for the
sample objective used by extremal-cut profiling. -/
private theorem nbinom_finite_sample {r : ℝ} (hr : 0 < r) {n l u : ℕ} (hn : 0 < n) (hlu : l < u)
    {x : Fin n → ℕ} (hx : ∀ i, x i ∈ Icc l u) {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) (hb : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let f := countWeights (negBinomCarrier r) 0
    let E := {p ∈ negativeParent | ∀ i, 0 < f p (x i)}
    let L := fun p ↦ likelihood x (f p) (Icc l u)
    let fits := (l : ℝ) < m ∧
      m < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (negBinomCarrier r) ∨
      m = l ∧ l = 0
    ((∃ p ∈ E, IsMaxOn L E p) ↔ fits) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ fits) :=
  negative_finite_sample hn hlu hx hm hb (fun k _ ↦ nb_pos hr k)

private theorem ray_sample {n l : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    (hx : ∀ i, l ≤ x i) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    {c : ℕ → ℝ} (hc : ∀ k, 0 < c k) {B : Set ℝ} (hB : B.Nonempty)
    {parent : Set (Sum ℝ Bool)}
    (hparent : ∀ p, p ∈ parent ↔ ∃ q ∈ lowerDomain B True, lowerTag q = p)
    (hpos : ∀ t ∈ B, 0 < centeredSum (fun k ↦ c (l+k)) (m-l) t) :
    let E := {p ∈ parent | ∀ i, 0 < countWeights c 0 p (x i)}
    let L := fun p ↦ likelihood x (countWeights c 0 p) (Ici l)
    let A := lowerDomain B (l = 0)
    let F := closedLowerLikelihood (fun k ↦ c (l+k)) (m-l)
    ((∃ p ∈ E, IsMaxOn L E p) ↔ ∃ q ∈ A, IsMaxOn F A q) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ ∃! q, q ∈ A ∧ IsMaxOn F A q) := by
  dsimp only
  let E := {p ∈ parent | ∀ i, 0 < countWeights c 0 p (x i)}
  let L := fun p ↦ likelihood x (countWeights c 0 p) (Ici l)
  let A := lowerDomain B (l = 0)
  let F := closedLowerLikelihood (fun k ↦ c (l+k)) (m-l)
  let G := fun p : Sum ℝ Bool ↦ match p with
    | .inl t => F (some t)
    | .inr _ => F none
  let D := lowerTag '' A
  have bot := mean_eq_bottom hn hm hx
  have hint (t : ℝ) : Sum.inl t ∈ parent ↔ t ∈ B := by
    rw [hparent]
    constructor
    · rintro ⟨(_ | s),hs,he⟩
      · cases he
      · cases he
        exact hs
    · intro h
      exact ⟨some t,h,rfl⟩
  have hin (t : ℝ) (ht : t ∈ B) : Sum.inl t ∈ E :=
    ⟨(hint t).mpr ht,fun i ↦ mul_pos (hc _) (pow_pos (exp_pos t) _)⟩
  have hbot : Sum.inr false ∈ E ↔ l = 0 ∧ m = l := by
    constructor
    · intro H
      have he (i : Fin n) : x i = 0 := by
        have H' := H.2 i
        by_contra h
        simp [countWeights,h] at H'
      have hz : l = 0 := by have H' := hx ⟨0,hn⟩; rw [he] at H'; omega
      exact ⟨hz,bot.mpr (fun i ↦ (he i).trans hz.symm)⟩
    · rintro ⟨hz,he⟩
      exact ⟨(hparent _).mpr ⟨none,trivial,rfl⟩,fun i ↦ by simp [countWeights,bot.mp he i,hz]⟩
  have htop : Sum.inr true ∉ parent := by
    rw [hparent]
    rintro ⟨(_ | t),_,he⟩ <;> cases he
  have hEn : E.Nonempty := by obtain ⟨t,ht⟩ := hB; exact ⟨_,hin t ht⟩
  have H := positive_chart_max (E := E) (D := D) (F := G) (L := L)
    (C := ∏ i, c (x i)) hn (Finset.prod_pos (fun _ _ ↦ hc _)) ?_ ?_ ?_ hEn
  · refine tagged_max fun p ↦ (H p).trans ?_
    have he : G ∘ lowerTag = F := by funext q; cases q <;> rfl
    simpa only [he] using lowerTag_max (D := B) (A := D) (L := G) (l = 0)
      (fun _ ↦ Iff.rfl) p
  · rintro (t | (_ | _)) hp
    · exact ⟨some t,(hint t).mp hp.1,rfl⟩
    · exact ⟨none,(hbot.mp hp).1,rfl⟩
    · exact (htop hp.1).elim
  · rintro (t | (_ | _)) hp
    · exact ⟨inv_pos.mpr (hpos t ((hint t).mp hp.1)),ray_likelihood_chart hm c t⟩
    · obtain ⟨hz,he⟩ := hbot.mp hp
      have hconst := bot.mp he
      have hL : L (Sum.inr false) = 1 := by
        simpa only [L,countWeights,← hz] using atom_likelihood (mem_Ici.mpr le_rfl) hconst
      change 0 < F none ∧ L (Sum.inr false) = _
      simpa [G,F,closedLowerLikelihood,he,hL] using And.intro (inv_pos.mpr (hc l))
        (carrier_power_atom c hconst (hc l)).symm
    · exact (htop hp.1).elim
  · rintro p ⟨⟨q,hq,rfl⟩,hnE⟩
    cases q with
    | none =>
      have hb' : m ≠ l := fun hb' ↦ hnE (hbot.mpr ⟨hq,hb'⟩)
      simp [G,F,lowerTag,closedLowerLikelihood,sub_ne_zero.mpr hb']
    | some t =>
      exact (hnE (hin t hq)).elim

private theorem centered_positive {c : ℕ → ℝ} (hc : ∀ k, 0 < c k) {m t : ℝ}
    (hs : Summable (fun k ↦ c k * exp t ^ k)) : 0 < centeredSum c m t := by
  have hs' : Summable (fun k : ℕ ↦ c k * exp (t * ((k : ℝ) - m))) := by
    convert hs.mul_right (exp (-(t*m))) using 1
    ext k
    rw [show t * ((k : ℝ) - m) = (k : ℝ)*t + -(t*m) by ring, exp_add, exp_nat_mul,mul_assoc]
  exact (mul_pos (hc 0) (exp_pos _)).trans_le
    (hs'.le_tsum 0 (fun k _ ↦ (mul_pos (hc k) (exp_pos _)).le))

private theorem full_parent_tags (p : Sum ℝ Bool) :
    p ∈ finiteCountDomain true false ↔ ∃ q ∈ lowerDomain univ True, lowerTag q = p := by
  rcases p with t | (_ | _)
  · exact ⟨fun _ ↦ ⟨some t,trivial,rfl⟩,fun _ ↦ trivial⟩
  · exact ⟨fun _ ↦ ⟨none,trivial,rfl⟩,fun _ ↦ rfl⟩
  · constructor
    · intro h
      cases h
    · rintro ⟨(_ | t),_,he⟩ <;> cases he

private theorem negative_parent_tags (p : Sum ℝ Bool) :
    p ∈ negativeParent ↔ ∃ q ∈ lowerDomain (Iio 0) True, lowerTag q = p := by
  rcases p with t | (_ | _)
  · constructor
    · intro h
      exact ⟨some t,h,rfl⟩
    · rintro ⟨(_ | s),hs,he⟩
      · cases he
      · cases he
        exact hs
  · exact ⟨fun _ ↦ ⟨none,trivial,rfl⟩,fun _ ↦ rfl⟩
  · constructor
    · intro h
      cases h
    · rintro ⟨(_ | t),_,he⟩ <;> cases he

/-- Ordinary Poisson ray criterion and uniqueness in the sample objective. -/
private theorem poisson_ray_sample {n l : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, l ≤ x i)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) (hb : (l : ℝ) ≤ m) :
    let f := countWeights (poissonCarrier 0) 0
    let E := {p ∈ finiteCountDomain true false | ∀ i, 0 < f p (x i)}
    let L := fun p ↦ likelihood x (f p) (Ici l)
    let fits := (l : ℝ) < m ∨ m = l ∧ l = 0
    ((∃ p ∈ E, IsMaxOn L E p) ↔ fits) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ fits) := by
  have hp : ∀ k, 0 < poissonCarrier 0 k := fun _ ↦ one_div_pos.mpr (by positivity)
  have H := ray_sample hn hx hm hp univ_nonempty full_parent_tags
    (fun t _ ↦ centered_positive (fun k ↦ hp (l+k)) (by
      simpa [poissonCarrier] using poisson_summable l t))
  have hf := poisson_ray_known l hb False False.elim
  simp only [or_false, poissonCarrier, Nat.zero_add] at hf H
  exact ⟨H.1.trans hf.1,H.2.trans (unique_from_pairwise hf)⟩

/-- Ordinary geometric ray criterion and uniqueness in the sample objective. -/
private theorem geom_ray_sample {n l : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, l ≤ x i)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) (hb : (l : ℝ) ≤ m) :
    let f := countWeights (fun _ ↦ 1) 0
    let E := {p ∈ negativeParent | ∀ i, 0 < f p (x i)}
    let L := fun p ↦ likelihood x (f p) (Ici l)
    let fits := (l : ℝ) < m ∨ m = l ∧ l = 0
    ((∃ p ∈ E, IsMaxOn L E p) ↔ fits) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ fits) := by
  have H := ray_sample hn hx hm (c := fun _ ↦ 1) (fun _ ↦ one_pos) ⟨-1,by norm_num⟩
    negative_parent_tags (fun _ ht ↦ centered_positive (fun _ ↦ one_pos) (geom_summable ht))
  have hf := geom_ray_known l hb False False.elim
  simp only [or_false] at hf
  exact ⟨H.1.trans hf.1,H.2.trans (unique_from_pairwise hf)⟩

/-- Ordinary positive-size NB ray criterion and uniqueness in the sample objective. -/
private theorem nbinom_ray_sample {r : ℝ} (hr : 0 < r) {n l : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    (hx : ∀ i, l ≤ x i) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) (hb : (l : ℝ) ≤ m) :
    let f := countWeights (negBinomCarrier r) 0
    let E := {p ∈ negativeParent | ∀ i, 0 < f p (x i)}
    let L := fun p ↦ likelihood x (f p) (Ici l)
    let fits := (l : ℝ) < m ∨ m = l ∧ l = 0
    ((∃ p ∈ E, IsMaxOn L E p) ↔ fits) ∧
    ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ fits) := by
  have H := ray_sample hn hx hm (nb_pos hr) ⟨-1,by norm_num⟩ negative_parent_tags
    (fun _ ht ↦ centered_positive (fun k ↦ nb_pos hr (l+k)) (nbinom_summable l r ht))
  have hf := nbinom_ray_known hr l hb False False.elim
  simp only [or_false] at hf
  exact ⟨H.1.trans hf.1,H.2.trans (unique_from_pairwise hf)⟩

/-- The four regular count families in their one-free-coefficient regimes. -/
inductive RegularCount
  | poisson
  | geometric
  | binomial (size : ℕ)
  | negBinomial (size : ℝ)

def RegularCount.carrier : RegularCount → ℕ → ℝ
  | .poisson => poissonCarrier 0
  | .geometric => fun _ ↦ 1
  | .binomial N => fun k ↦ N.choose k
  | .negBinomial r => negBinomCarrier r

def RegularCount.topCount : RegularCount → ℕ
  | .binomial N => N
  | _ => 0

def RegularCount.parent : RegularCount → Set (Sum ℝ Bool)
  | .poisson => finiteCountDomain true false
  | .binomial _ => finiteCountDomain true true
  | _ => negativeParent

def RegularCount.weights (s : RegularCount) := countWeights s.carrier s.topCount

/-- A finite effective count interval or a lower-only ray. -/
inductive CountCut
  | finite (l u : ℕ)
  | ray (l : ℕ)

def CountCut.kept : CountCut → Set ℕ
  | .finite l u => Icc l u
  | .ray l => Ici l

def CountCut.nonsingleton : CountCut → Prop
  | .finite l u => l < u
  | .ray _ => True

def RegularCount.valid (s : RegularCount) (K : CountCut) : Prop :=
  match s, K with
  | .binomial N, .finite l u => l ≤ u ∧ u ≤ N
  | .binomial _, .ray _ => False
  | .negBinomial r, .finite l u => 0 < r ∧ l ≤ u
  | .negBinomial r, .ray _ => 0 < r
  | _, .finite l u => l ≤ u
  | _, .ray _ => True

def RegularCount.attains (s : RegularCount) (K : CountCut) (m : ℝ) : Prop :=
  match K with
  | .ray l => (l : ℝ) < m ∨ m = l ∧ l = 0
  | .finite l u => l = u ∨
      match s with
      | .poisson => m ∈ Ioo (l : ℝ) (u : ℝ) ∨ m = l ∧ l = 0
      | .binomial N => m ∈ Ioo (l : ℝ) (u : ℝ) ∨ m = l ∧ l = 0 ∨ m = u ∧ u = N
      | _ => (l : ℝ) < m ∧
        m < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) s.carrier ∨ m = l ∧ l = 0

/-- Effective retained cells, with held-size clipping on the binomial. -/
def RegularCount.effective (s : RegularCount) (A : Set ℕ) : Set ℕ :=
  match s with
  | .binomial N => A ∩ Iic N
  | _ => A

private theorem count_clip (s : RegularCount) {n : ℕ} (x : Fin n → ℕ) (A : Set ℕ) (p : Sum ℝ Bool) :
    likelihood x (s.weights p) A = likelihood x (s.weights p) (s.effective A) := by
  cases s
  · rfl
  · rfl
  · exact binom_window_clip p
  · rfl

private theorem mean_bounds {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {m : ℝ}
    (hm : ∑ i, (x i : ℝ) = n * m) :
    (∀ l : ℕ, (∀ i, l ≤ x i) → (l : ℝ) ≤ m) ∧
    (∀ u : ℕ, (∀ i, x i ≤ u) → m ≤ (u : ℝ)) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  constructor
  · intro l hx
    apply (mul_le_mul_iff_right₀ hn').mp
    have h : (∑ _ : Fin n, (l : ℝ)) ≤ ∑ i, (x i : ℝ) :=
      Finset.sum_le_sum (fun i _ ↦ by exact_mod_cast hx i)
    simpa [hm, mul_comm] using h
  · intro u hx
    apply (mul_le_mul_iff_right₀ hn').mp
    have h : (∑ i, (x i : ℝ)) ≤ ∑ _ : Fin n, (u : ℝ) :=
      Finset.sum_le_sum (fun i _ ↦ by exact_mod_cast hx i)
    simpa [hm, mul_comm] using h

private theorem singleton_result {n l : ℕ} {x : Fin n → ℕ} (hx : ∀ i, x i ∈ Icc l l)
    {parent : Set (Sum ℝ Bool)} {c : ℕ → ℝ} {N : ℕ}
    (hc : 0 < c l) (ht : ∃ t, Sum.inl t ∈ parent) :
    let E := {p ∈ parent | ∀ i, 0 < countWeights c N p (x i)}
    let L := fun p ↦ likelihood x (countWeights c N p) (Icc l l)
    (∃ p ∈ E, IsMaxOn L E p) ∧ ∀ p ∈ E, L p = 1 := by
  dsimp only
  have hconst (i : Fin n) : x i = l := le_antisymm (hx i).2 (hx i).1
  have hval (p : Sum ℝ Bool) (hp : ∀ i, 0 < countWeights c N p (x i))
      (hn : Nonempty (Fin n)) : likelihood x (countWeights c N p) (Icc l l) = 1 := by
    obtain ⟨i⟩ := hn
    rw [Icc_self]
    exact likelihood_singleton hconst (hconst i ▸ hp i)
  have hval' (p : Sum ℝ Bool) (hp : ∀ i, 0 < countWeights c N p (x i)) :
      likelihood x (countWeights c N p) (Icc l l) = 1 := by
    by_cases hi : Nonempty (Fin n)
    · exact hval p hp hi
    · have hzero : n = 0 := by cases n <;> simp_all
      subst n
      simp [likelihood]
  obtain ⟨t,ht⟩ := ht
  refine ⟨⟨Sum.inl t,⟨ht,fun i ↦ hconst i ▸ mul_pos hc (pow_pos (exp_pos t) l)⟩,?_⟩,
    fun p hp ↦ hval' p hp.2⟩
  intro p hp
  change likelihood x (countWeights c N p) (Icc l l) ≤
    likelihood x (countWeights c N (Sum.inl t)) (Icc l l)
  rw [hval' p hp.2, hval' _ (fun i ↦ hconst i ▸ mul_pos hc (pow_pos (exp_pos t) l))]

private theorem joined_finite {P : Type*} {E : Set P} {F : P → ℝ}
    {l u : ℕ} (hle : l ≤ u) (B : Prop)
    (hflat : l = u → (∃ p ∈ E, IsMaxOn F E p) ∧ ∀ p ∈ E, F p = 1)
    (hfit : l < u → (((∃ p ∈ E, IsMaxOn F E p) ↔ B) ∧
      ((∃! p, p ∈ E ∧ IsMaxOn F E p) ↔ B))) :
    (((∃ p ∈ E, IsMaxOn F E p) ↔ l = u ∨ B) ∧
      (l < u → ((∃! p, p ∈ E ∧ IsMaxOn F E p) ↔ l = u ∨ B)) ∧
      (l = u → ∀ p ∈ E, F p = 1)) := by
  rcases hle.eq_or_lt with he | ht
  · exact ⟨iff_of_true (hflat he).1 (Or.inl he),
      fun h ↦ (h.ne he).elim, fun _ ↦ (hflat he).2⟩
  · exact ⟨(hfit ht).1.trans (by simp [ht.ne]),fun _ ↦ (hfit ht).2.trans (by simp [ht.ne]),
      fun h ↦ (ht.ne h).elim⟩

/-- Joined ordinary criterion and uniqueness on finite windows, singletons
and rays, with binomial clipping and the positive-size NB restriction. -/
theorem regular_known (s : RegularCount) (K : CountCut) (hv : s.valid K)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i ∈ K.kept)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
    let L := fun p ↦ likelihood x (s.weights p) K.kept
    ((∃ p ∈ E, IsMaxOn L E p) ↔ s.attains K m) ∧
      (K.nonsingleton → ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ s.attains K m)) ∧
      (¬ K.nonsingleton → ∀ p ∈ E, L p = 1) := by
  have H := mean_bounds hn hm
  cases K with
  | ray l =>
    have hb := H.1 l hx
    cases s with
    | poisson =>
      have h := poisson_ray_sample hn hx hm hb
      exact ⟨h.1,fun _ ↦ h.2,fun h ↦ (h trivial).elim⟩
    | geometric =>
      have h := geom_ray_sample hn hx hm hb
      exact ⟨h.1,fun _ ↦ h.2,fun h ↦ (h trivial).elim⟩
    | binomial N => exact False.elim hv
    | negBinomial r =>
      have h := nbinom_ray_sample (show 0 < r from hv) hn hx hm hb
      exact ⟨h.1,fun _ ↦ h.2,fun h ↦ (h trivial).elim⟩
  | finite l u =>
    have hx' : ∀ i, x i ∈ Icc l u := hx
    have hb : m ∈ Icc (l : ℝ) (u : ℝ) :=
      ⟨H.1 l (fun i ↦ (hx' i).1), H.2 u (fun i ↦ (hx' i).2)⟩
    cases s with
    | poisson =>
      have h := joined_finite (show l ≤ u from hv) _
        (fun he ↦ by
          subst u
          exact singleton_result hx' (c := poissonCarrier 0) (parent := finiteCountDomain true false)
            (N := 0) (by unfold poissonCarrier; positivity) ⟨0,trivial⟩)
        (fun h ↦ poisson_finite_sample hn h hx' hm hb)
      exact ⟨h.1,h.2.1,fun hle ↦ h.2.2 (le_antisymm hv (le_of_not_gt hle))⟩
    | geometric =>
      have h := joined_finite (show l ≤ u from hv) _
        (fun he ↦ by
          subst u
          exact singleton_result hx' (c := fun _ ↦ 1) (parent := negativeParent)
            (N := 0) one_pos ⟨-1,by change (-1 : ℝ) < 0; norm_num⟩)
        (fun h ↦ geom_finite_sample hn h hx' hm hb)
      exact ⟨h.1,h.2.1,fun hle ↦ h.2.2 (le_antisymm hv (le_of_not_gt hle))⟩
    | binomial N =>
      have hv' : l ≤ u ∧ u ≤ N := hv
      have h := joined_finite hv'.1 _
        (fun he ↦ by
          subst u
          exact singleton_result hx' (c := fun k ↦ (N.choose k : ℝ))
            (parent := finiteCountDomain true true) (N := N)
            (by exact_mod_cast Nat.choose_pos hv'.2) ⟨0,trivial⟩)
        (fun h ↦ binom_finite_sample hn h hv'.2 hx' hm hb)
      exact ⟨h.1,h.2.1,fun hle ↦ h.2.2 (le_antisymm hv'.1 (le_of_not_gt hle))⟩
    | negBinomial r =>
      have hv' : 0 < r ∧ l ≤ u := hv
      have h := joined_finite hv'.2 _
        (fun he ↦ by
          subst u
          exact singleton_result hx' (c := negBinomCarrier r) (parent := negativeParent)
            (N := 0) (nb_pos hv'.1 l) ⟨-1,by change (-1 : ℝ) < 0; norm_num⟩)
        (fun h ↦ nbinom_finite_sample hv'.1 hn h hx' hm hb)
      exact ⟨h.1,h.2.1,fun hle ↦ h.2.2 (le_antisymm hv'.2 (le_of_not_gt hle))⟩

private theorem regular_weights_nonneg (s : RegularCount)
    (hr : ∀ r, s = .negBinomial r → 0 < r) : ∀ p ∈ s.parent, ∀ k, 0 ≤ s.weights p k := by
  have hc (k : ℕ) : 0 ≤ s.carrier k := by
    cases s with
    | poisson => change 0 ≤ poissonCarrier 0 k; unfold poissonCarrier; positivity
    | geometric => exact zero_le_one
    | binomial N => exact Nat.cast_nonneg _
    | negBinomial r => exact (nb_pos (hr r rfl) k).le
  rintro (t | (_ | _)) _ k
  · exact mul_nonneg (hc k) (pow_nonneg (exp_pos t).le _)
  · exact ite_nonneg (by norm_num) le_rfl
  · exact ite_nonneg (by norm_num) le_rfl

private theorem regular_interior_summable (s : RegularCount) :
    ∀ t, Sum.inl t ∈ s.parent → Summable (s.weights (Sum.inl t)) := by
  intro t ht
  cases s with
  | poisson => exact poisson_summable 0 t
  | geometric => exact geom_summable ht
  | binomial N => exact binom_summable N t
  | negBinomial r => simpa [RegularCount.weights,RegularCount.carrier,RegularCount.topCount,
      countWeights] using nbinom_summable 0 r (t := t) ht

/-- Joint ordinary estimated cuts, in any of the three design regimes.
At the tightened nominal window, use the effective cut after support clipping.
The set of joint maxima is exactly the tight fit and its zero-mass fibres. -/
private theorem regular_estimated (s : RegularCount) (K : CountCut) (hv : s.valid K)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    {d : Design} {fixed w : Window} (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w)
    (hK : s.effective (cells (tight d fixed lo hi)) = K.kept)
    (hxK : ∀ i, x i ∈ K.kept) :
    let T := tight d fixed lo hi
    let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
    let C := candidates x s.weights s.parent d fixed
    let L := fun p : Sum ℝ Bool × Window ↦ likelihood x (s.weights p.1) (cells p.2)
    let G := fun p ↦ likelihood x (s.weights p) K.kept
    sSup (L '' C) = sSup (G '' E) ∧
      (∀ q ∈ C, IsMaxOn L C q ↔ IsMaxOn G E q.1 ∧
        ∀ k ∈ cells q.2 \ cells T, s.weights q.1 k = 0) ∧
      ((∃ q ∈ C, IsMaxOn L C q) ↔ s.attains K m) ∧
      (K.nonsingleton → ((∃! p, p ∈ E ∧ IsMaxOn G E p) ↔ s.attains K m)) ∧
      (¬ K.nonsingleton → ∀ p ∈ E, G p = 1) := by
  have hp : ∀ r, s = .negBinomial r → 0 < r := by
    rintro r rfl
    cases K
    · exact hv.1
    · exact hv
  have HS : ∀ p ∈ s.parent, Summable (s.weights p) :=
    weights_summable (regular_interior_summable s)
  have H := profile hn hlo hhi hb (regular_weights_nonneg s hp) HS hw hx
  have hG : (fun p ↦ likelihood x (s.weights p) (cells (tight d fixed lo hi))) =
      fun p ↦ likelihood x (s.weights p) K.kept := by
    funext p
    rw [count_clip s, hK]
  dsimp only at H ⊢
  rw [hG] at H
  have hk := regular_known s K hv hn hxK hm
  exact ⟨H.1,H.2.1,H.2.2.trans hk.1,hk.2.1,hk.2.2⟩

/-- In all four regular count families, the declared positive alias adds
one candidate at likelihood one exactly for all-ones data and an allowed
nominal alias. The ordinary tight-window winners survive when they also
reach one. This applies to fixed windows and to each estimated-cut design. -/
private theorem regular_with_alias (s : RegularCount) (K : CountCut) (hv : s.valid K)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    {d : Design} {fixed w : Window} (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w)
    (hK : s.effective (cells (tight d fixed lo hi)) = K.kept)
    (hxK : ∀ i, x i ∈ K.kept) :
    let T := tight d fixed lo hi
    let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
    let C := candidates x s.weights s.parent d fixed
    let G := fun p ↦ likelihood x (s.weights p) K.kept
    let a : Sum ℝ Bool × Window := (Sum.inr false, positiveWindow)
    let eligible := allows d fixed positiveWindow ∧ ∀ i, x i = 1
    let A := {q | eligible ∧ q = a}
    let L := fun q ↦ if q = a then 1 else likelihood x (s.weights q.1) (cells q.2)
    ((∃ q ∈ C ∪ A, IsMaxOn L (C ∪ A) q) ↔ s.attains K m ∨ eligible) ∧
      ∀ q, (q ∈ C ∪ A ∧ IsMaxOn L (C ∪ A) q) ↔
        q ∈ A ∨ (q ∈ C ∧ IsMaxOn G E q.1 ∧
          (∀ k ∈ cells q.2 \ cells T, s.weights q.1 k = 0) ∧ (eligible → G q.1 = 1)) := by
  classical
  dsimp only
  let T := tight d fixed lo hi
  let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
  let C := candidates x s.weights s.parent d fixed
  let G := fun p ↦ likelihood x (s.weights p) K.kept
  let a : Sum ℝ Bool × Window := (Sum.inr false, positiveWindow)
  let eligible := allows d fixed positiveWindow ∧ ∀ i, x i = 1
  let A := {q | eligible ∧ q = a}
  let F := fun q : Sum ℝ Bool × Window ↦ likelihood x (s.weights q.1) (cells q.2)
  let L := fun q ↦ if q = a then 1 else F q
  have hr : ∀ r, s = .negBinomial r → 0 < r := by
    rintro r rfl
    cases K
    · exact hv.1
    · exact hv
  have hc : ∀ k, 0 ≤ s.carrier k := by
    intro k
    cases s with
    | poisson => change 0 ≤ poissonCarrier 0 k; unfold poissonCarrier; positivity
    | geometric => exact zero_le_one
    | binomial N => exact Nat.cast_nonneg _
    | negBinomial r => exact (nb_pos (hr r rfl) k).le
  have haC : a ∉ C := by
    intro H
    have h := H.2.2 ⟨0,hn⟩
    have hb : x ⟨0,hn⟩ = 0 := by
      by_contra he
      have hpos := h.2
      change 0 < (if x ⟨0,hn⟩ = 0 then (1 : ℝ) else 0) at hpos
      simp [he] at hpos
    have hwin := h.1
    change (1 : ℝ) ≤ (x ⟨0,hn⟩ : ℕ) ∧ _ at hwin
    norm_num [hb] at hwin
  have hLF (q : Sum ℝ Bool × Window) (hq : q ∈ C) : L q = F q := by
    have hne : q ≠ a := fun h ↦ haC (h ▸ hq)
    simp [L,hne]
  have hOrd (q : Sum ℝ Bool × Window) (hq : q ∈ C) :
      IsMaxOn L C q ↔ IsMaxOn F C q := by
    constructor <;> intro H p hp
    · have he := H hp
      change L p ≤ L q at he
      rwa [hLF p hp,hLF q hq] at he
    · change L p ≤ L q
      rw [hLF p hp,hLF q hq]
      exact H hp
  have hAlias := atom_alias_join (N := s.topCount) (D := s.parent) hn hc
    (regular_interior_summable s) d fixed (x := x)
  change (∀ q ∈ C, IsMaxOn L (C ∪ A) q ↔ IsMaxOn L C q ∧ (eligible → L q = 1)) ∧
    (∀ q ∈ A, IsMaxOn L (C ∪ A) q) at hAlias
  have hFit := regular_estimated s K hv hn hlo hhi hb hm hw hx hK hxK
  change sSup (F '' C) = sSup (G '' E) ∧
    (∀ q ∈ C, IsMaxOn F C q ↔ IsMaxOn G E q.1 ∧
      ∀ k ∈ cells q.2 \ cells T, s.weights q.1 k = 0) ∧
    ((∃ q ∈ C, IsMaxOn F C q) ↔ s.attains K m) ∧ _ at hFit
  have hT := tight_cells hlo hhi hb hw hx
  have hval (q : Sum ℝ Bool × Window) (hq : q ∈ C)
      (hz : ∀ k ∈ cells q.2 \ cells T, s.weights q.1 k = 0) : F q = G q.1 := by
    have hqT := tight_cells hlo hhi hb hq.2.1 (fun i ↦ (hq.2.2 i).1)
    have H := likelihood_shrink hn (regular_weights_nonneg s hr _ hq.1)
      (weights_summable (regular_interior_summable s) _ hq.1)
      (fun i ↦ (hq.2.2 i).2) hqT.2.2 hT.2.1
    exact (H.2.2.mpr hz).trans ((count_clip s x (cells T) q.1).trans
      (congrArg (likelihood x (s.weights q.1)) hK))
  have hC (q : Sum ℝ Bool × Window) (hq : q ∈ C) :
      IsMaxOn L (C ∪ A) q ↔ IsMaxOn G E q.1 ∧
        (∀ k ∈ cells q.2 \ cells T, s.weights q.1 k = 0) ∧ (eligible → G q.1 = 1) := by
    rw [hAlias.1 q hq, hOrd q hq, hFit.2.1 q hq, hLF q hq]
    constructor
    · rintro ⟨⟨hG,hz⟩,he⟩
      exact ⟨hG,hz,fun h ↦ (hval q hq hz).symm.trans (he h)⟩
    · rintro ⟨hG,hz,he⟩
      exact ⟨⟨hG,hz⟩,fun h ↦ (hval q hq hz).trans (he h)⟩
  constructor
  · constructor
    · rintro ⟨q,(hq | hq),hqm⟩
      · exact Or.inl (hFit.2.2.1.mp ⟨q,hq,(hOrd q hq).mp ((hAlias.1 q hq).mp hqm).1⟩)
      · exact Or.inr hq.1
    · rintro (hf | ha)
      · by_cases he : eligible
        · exact ⟨a,Or.inr ⟨he,rfl⟩,hAlias.2 a ⟨he,rfl⟩⟩
        · obtain ⟨q,hq,hqm⟩ := hFit.2.2.1.mpr hf
          exact ⟨q,Or.inl hq,(hAlias.1 q hq).mpr ⟨(hOrd q hq).mpr hqm,fun hh ↦ (he hh).elim⟩⟩
      · exact ⟨a,Or.inr ⟨ha,rfl⟩,hAlias.2 a ⟨ha,rfl⟩⟩
  · intro q
    constructor
    · rintro ⟨(hq | hq),hqm⟩
      · exact Or.inr ⟨hq,(hC q hq).mp hqm⟩
      · exact Or.inl hq
    · rintro (hq | ⟨hq,hqm⟩)
      · exact ⟨Or.inr hq,hAlias.2 q hq⟩
      · exact ⟨Or.inl hq,(hC q hq).mpr hqm⟩

def RegularCount.coefficient : RegularCount → (Sum ℝ Bool → ℝ)
  | .poisson => poissonRate
  | .binomial _ => binomProb
  | _ => negativeProb

def RegularCount.coefficientDomain : RegularCount → Set ℝ
  | .poisson => Ici 0
  | .binomial _ => Icc 0 1
  | _ => Ioc 0 1

private theorem poisson_chart : BijOn poissonRate (finiteCountDomain true false) (Ici 0) := by
  refine ⟨?_,?_,?_⟩
  · rintro (t | (_ | _)) h
    · exact (exp_pos t).le
    · exact le_rfl
    · cases h
  · rintro (t | (_ | _)) ht (u | (_ | _)) hu h
    all_goals try contradiction
    · exact congrArg Sum.inl (exp_injective h)
    · exact ((exp_pos t).ne' h).elim
    · exact ((exp_pos u).ne' h.symm).elim
    · rfl
  · intro p hp
    rcases (show 0 ≤ p from hp).eq_or_lt with h | h
    · exact ⟨Sum.inr false,rfl,h⟩
    · exact ⟨Sum.inl (log p),trivial,exp_log h⟩

private theorem negative_chart : BijOn negativeProb negativeParent (Ioc 0 1) := by
  refine ⟨?_,?_,?_⟩
  · rintro (t | (_ | _)) h
    · exact ⟨sub_pos.mpr (exp_lt_one_iff.mpr h),sub_le_self _ (exp_pos _).le⟩
    · exact ⟨zero_lt_one,le_rfl⟩
    · cases h
  · rintro (t | (_ | _)) ht (u | (_ | _)) hu h
    all_goals try contradiction
    · exact congrArg Sum.inl (exp_injective (sub_right_inj.mp h))
    · change 1 - exp t = 1 at h
      linarith [exp_pos t]
    · change 1 = 1 - exp u at h
      linarith [exp_pos u]
    · rfl
  · intro p hp
    rcases hp.2.lt_or_eq with h | h
    · have hq : 0 < 1-p := sub_pos.mpr h
      refine ⟨Sum.inl (log (1-p)),?_,?_⟩
      · exact log_neg hq (by linarith [hp.1])
      · change 1 - exp (log (1-p)) = p
        rw [exp_log hq]
        ring
    · exact ⟨Sum.inr false,rfl,h.symm⟩

private theorem binom_chart : BijOn binomProb (finiteCountDomain true true) (Icc 0 1) := by
  have interior (t : ℝ) : binomProb (Sum.inl t) ∈ Ioo 0 1 := by
    have hp := exp_pos t
    change 0 < exp t / (1 + exp t) ∧ exp t / (1 + exp t) < 1
    exact ⟨div_pos hp (by positivity),(div_lt_one (by positivity)).mpr (by linarith)⟩
  refine ⟨?_,?_,?_⟩
  · rintro (t | (_ | _)) _
    · exact ⟨(interior t).1.le,(interior t).2.le⟩
    · exact ⟨le_rfl,zero_le_one⟩
    · exact ⟨zero_le_one,le_rfl⟩
  · rintro (t | (_ | _)) ht (u | (_ | _)) hu h
    · change exp t / (1 + exp t) = exp u / (1 + exp u) at h
      have H := (div_eq_div_iff (show 1 + exp t ≠ 0 by positivity)
        (show 1 + exp u ≠ 0 by positivity)).mp h
      exact congrArg Sum.inl (exp_injective (by linarith [H]))
    · exact ((interior t).1.ne' h).elim
    · exact ((interior t).2.ne h).elim
    · exact ((interior u).1.ne' h.symm).elim
    · rfl
    · exact (zero_ne_one h).elim
    · exact ((interior u).2.ne h.symm).elim
    · exact (one_ne_zero h).elim
    · rfl
  · intro p hp
    rcases hp.1.eq_or_lt with h | h
    · exact ⟨Sum.inr false,rfl,h⟩
    rcases hp.2.lt_or_eq with h' | h'
    · have hq : 0 < p / (1-p) := div_pos h (sub_pos.mpr h')
      refine ⟨Sum.inl (log (p / (1-p))),trivial,?_⟩
      change exp (log (p / (1-p))) / (1 + exp (log (p / (1-p)))) = p
      rw [exp_log hq]
      apply (div_eq_iff (by linarith : (1 + p/(1-p)) ≠ 0)).mpr
      field_simp
      ring
    · exact ⟨Sum.inr true,rfl,h'.symm⟩

/-- Chart points identify exactly one public rate or probability,
including all admitted parent endpoint coefficients. -/
theorem regular_coefficient_chart (s : RegularCount) :
    BijOn s.coefficient s.parent s.coefficientDomain := by
  cases s
  · exact poisson_chart
  · exact negative_chart
  · exact binom_chart
  · exact negative_chart

/-- Resolve real nominal cuts to the retained lattice. The binomial cap
turns even a nominal ray into a finite effective window. -/
def RegularCount.resolve (s : RegularCount) (w : Window) : CountCut :=
  let l := ⌈w.1⌉₊
  w.2.recTopCoe
    (match s with
      | .binomial N => .finite l N
      | _ => .ray l)
    (fun b ↦ .finite l (match s with
      | .binomial N => min ⌊b⌋₊ N
      | _ => ⌊b⌋₊))

private theorem nominal_cells {w : Window} (hw : (cells w).Nonempty) :
    cells w = (RegularCount.poisson.resolve w).kept := by
  rcases w with ⟨a,b⟩
  induction b using WithTop.recTopCoe with
  | top =>
    ext k
    simp [cells,RegularCount.resolve,CountCut.kept,Nat.ceil_le]
  | coe b =>
    obtain ⟨k,hk⟩ := hw
    have h : (k : ℝ) ≤ b := WithTop.coe_le_coe.mp hk.2
    have hb := (Nat.cast_nonneg k).trans h
    ext j
    simp [cells,RegularCount.resolve,CountCut.kept,Nat.ceil_le,Nat.le_floor_iff hb]
    intro _
    exact WithTop.coe_le_coe

/-- The effective window is computed by rounding and, for binomial, clipping. -/
private theorem regular_resolve (s : RegularCount) {w : Window} (hw : (cells w).Nonempty) :
    s.effective (cells w) = (s.resolve w).kept := by
  rw [nominal_cells hw]
  rcases w with ⟨a,b⟩
  induction b using WithTop.recTopCoe <;> cases s <;>
    ext k <;> simp [RegularCount.effective,RegularCount.resolve,CountCut.kept,le_min_iff,and_assoc]

private theorem resolve_valid (s : RegularCount) {w : Window}
    (hw : (s.resolve w).kept.Nonempty) (hr : ∀ r, s = .negBinomial r → 0 < r) :
    s.valid (s.resolve w) := by
  rcases w with ⟨a,b⟩
  have H {l u : ℕ} (h : (Icc l u).Nonempty) : l ≤ u := by
    obtain ⟨k,hk⟩ := h
    exact hk.1.trans hk.2
  induction b using WithTop.recTopCoe with
  | top =>
    cases s
    · trivial
    · trivial
    · exact ⟨H hw,le_rfl⟩
    · exact hr _ rfl
  | coe b =>
    cases s
    · exact H hw
    · exact H hw
    · exact ⟨H hw,min_le_right ..⟩
    · exact ⟨hr _ rfl,H hw⟩

/-- Failure cases indexed by the ordinary winning boundary law. The
first clause is a lower atom, the second is an upper atom for positive
tilts or the zero-tilt carrier for negative tilts. -/
def RegularCount.escapes (s : RegularCount) (K : CountCut) (m : ℝ) : Prop :=
  match K with
  | .ray l => m = l ∧ l ≠ 0
  | .finite l u => l < u ∧ (m = l ∧ l ≠ 0 ∨
      match s with
      | .poisson => m = u
      | .binomial N => m = u ∧ u ≠ N
      | _ => Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) s.carrier ≤ m)

private theorem carrierMean_gt_lower {l u : ℕ} (hlu : l < u) {c : ℕ → ℝ}
    (hc : ∀ k ∈ Finset.Icc l u, 0 < c k) :
    (l : ℝ) < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c := by
  have hu := Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩
  apply (lt_div_iff₀ (Finset.sum_pos hc ⟨u,hu⟩)).mpr
  rw [Finset.mul_sum]
  refine Finset.sum_lt_sum (fun k hk ↦ ?_) ⟨u,hu,?_⟩
  · have h : (l : ℝ) ≤ k := by exact_mod_cast (Finset.mem_Icc.mp hk).1
    simpa [mul_comm] using mul_le_mul_of_nonneg_right h (hc k hk).le
  · have h : (l : ℝ) < u := by exact_mod_cast hlu
    simpa [mul_comm] using mul_lt_mul_of_pos_right h (hc u hu)

private theorem negative_failure_test {l u : ℕ} (hlt : l < u) {c : ℕ → ℝ}
    (hc : ∀ k ∈ Finset.Icc l u, 0 < c k) {m : ℝ} (hm : (l : ℝ) ≤ m) :
    (¬ ((l : ℝ) < m ∧ m < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c ∨
      m = l ∧ l = 0)) ↔ m = l ∧ l ≠ 0 ∨
        Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c ≤ m := by
  rcases hm.eq_or_lt with h | h
  · subst m
    simp [not_le.mpr (carrierMean_gt_lower hlt hc)]
  · simp [h,h.ne']

private theorem failure_test (s : RegularCount) (K : CountCut) (hv : s.valid K)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i ∈ K.kept)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    (¬ s.attains K m) ↔ s.escapes K m := by
  have H := mean_bounds hn hm
  cases K with
  | ray l =>
    have hl := H.1 l hx
    change (¬ ((l : ℝ) < m ∨ m = l ∧ l = 0)) ↔ m = l ∧ l ≠ 0
    rcases hl.eq_or_lt with h | h
    · subst m
      simp
    · simp [h,h.ne']
  | finite l u =>
    have hx' : ∀ i, x i ∈ Icc l u := hx
    have hl := H.1 l (fun i ↦ (hx' i).1)
    have hu := H.2 u (fun i ↦ (hx' i).2)
    have hle : l ≤ u := (hx' ⟨0,hn⟩).1.trans (hx' ⟨0,hn⟩).2
    rcases hle.eq_or_lt with he | he
    · subst u
      cases s <;> simp [RegularCount.attains,RegularCount.escapes]
    · cases s with
      | poisson =>
        have hlt : (l : ℝ) < u := by exact_mod_cast he
        simp only [RegularCount.attains, RegularCount.escapes, he, he.ne, false_or, true_and]
        rcases hl.eq_or_lt with h | h
        · subst m
          simp [mem_Ioo,hlt,hlt.ne]
        · rcases hu.lt_or_eq with h' | h'
          · simp [mem_Ioo,h,h',h.ne',h'.ne]
          · subst m
            simp [mem_Ioo,hlt,hlt.ne']
      | binomial N =>
        have hlt : (l : ℝ) < u := by exact_mod_cast he
        simp only [RegularCount.attains, RegularCount.escapes, he, he.ne, false_or, true_and]
        rcases hl.eq_or_lt with h | h
        · subst m
          simp [mem_Ioo,hlt,hlt.ne]
        · rcases hu.lt_or_eq with h' | h'
          · simp [mem_Ioo,h,h',h.ne',h'.ne]
          · subst m
            simp [mem_Ioo,hlt,hlt.ne']
      | geometric =>
        simpa only [RegularCount.attains, RegularCount.escapes, RegularCount.carrier, he, he.ne,
          false_or, true_and] using
          negative_failure_test he (c := fun _ ↦ 1) (fun _ _ ↦ one_pos) hl
      | negBinomial r =>
        have hr : 0 < r ∧ l ≤ u := hv
        simpa only [RegularCount.attains, RegularCount.escapes, RegularCount.carrier, he, he.ne,
          false_or, true_and] using
          negative_failure_test he (fun k _ ↦ nb_pos hr.1 k) hl

/-- Failure of positive-likelihood attainment on an ordinary known
compatible window is exactly the corresponding excluded boundary case. -/
theorem regular_known_failure (s : RegularCount) (K : CountCut) (hv : s.valid K)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i ∈ K.kept)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
    let L := fun p ↦ likelihood x (s.weights p) K.kept
    (¬ ∃ p ∈ E, IsMaxOn L E p) ↔ s.escapes K m :=
  (not_congr (regular_known s K hv hn hx hm).1).trans (failure_test s K hv hn hx hm)

/-- Attainment and the complete set of ordinary or alias maxima from the
actual nominal cuts, including fixed-cut designs. For a binomial, sample
compatibility includes every observation being at most the held size. -/
theorem regular_window_fit (s : RegularCount)
    (hr : ∀ r, s = .negBinomial r → 0 < r)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    {d : Design} {fixed w : Window} (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w)
    (hN : ∀ N, s = .binomial N → hi ≤ N) :
    let T := tight d fixed lo hi
    let K := s.resolve T
    let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
    let C := candidates x s.weights s.parent d fixed
    let G := fun p ↦ likelihood x (s.weights p) K.kept
    let a : Sum ℝ Bool × Window := (Sum.inr false, positiveWindow)
    let eligible := allows d fixed positiveWindow ∧ ∀ i, x i = 1
    let A := {q | eligible ∧ q = a}
    let L := fun q ↦ if q = a then 1 else likelihood x (s.weights q.1) (cells q.2)
    ((∃ q ∈ C ∪ A, IsMaxOn L (C ∪ A) q) ↔ s.attains K m ∨ eligible) ∧
      (∀ q, (q ∈ C ∪ A ∧ IsMaxOn L (C ∪ A) q) ↔
        q ∈ A ∨ (q ∈ C ∧ IsMaxOn G E q.1 ∧
          (∀ k ∈ cells q.2 \ cells T, s.weights q.1 k = 0) ∧ (eligible → G q.1 = 1))) ∧
      (K.nonsingleton → ((∃! p, p ∈ E ∧ IsMaxOn G E p) ↔ s.attains K m)) ∧
      (¬ K.nonsingleton → ∀ p ∈ E, G p = 1) ∧
      ((¬ ∃ q ∈ C ∪ A, IsMaxOn L (C ∪ A) q) ↔ s.escapes K m ∧ ¬ eligible) := by
  classical
  let T := tight d fixed lo hi
  let K := s.resolve T
  have hT := (tight_cells hlo hhi hb hw hx).2.1
  have he : s.effective (cells T) = K.kept := regular_resolve s ⟨_,hT ⟨0,hn⟩⟩
  have hxK (i : Fin n) : x i ∈ K.kept := by
    rw [← he]
    cases s with
    | binomial N => exact ⟨hT i,(hb i).2.trans (hN N rfl)⟩
    | poisson => exact hT i
    | geometric => exact hT i
    | negBinomial r => exact hT i
  have hv : s.valid K := resolve_valid s ⟨_,hxK ⟨0,hn⟩⟩ hr
  have H := regular_with_alias s K hv hn hlo hhi hb hm hw hx he hxK
  have hk := regular_known s K hv hn hxK hm
  refine ⟨H.1,H.2,hk.2.1,hk.2.2,?_⟩
  exact (not_congr H.1).trans (not_or.trans
    (and_congr (failure_test s K hv hn hxK hm) Iff.rfl))

/-- Held NB size zero on fixed or estimated nominal windows. Ordinary
all-zero fits are flat over probability and every allowed window retaining
zero. Positive data can fit only on the exact logarithmic alias. -/
theorem nbinom_zero_window_fit {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) (d : Design) (fixed : Window) :
    let D := closedLowerDomain (Iio 0)
    let C : Set (Option ℝ × Window) :=
      {q | q.1 ∈ D ∧ allows d fixed q.2 ∧ 0 ∈ cells q.2 ∧ ∀ i, x i = 0}
    let A : Set (Option ℝ × Window) :=
      {q | q.1 ∈ D ∧ allows d fixed positiveWindow ∧ q.2 = positiveWindow}
    let P := ∀ i, 1 ≤ x i
    let W := C ∪ {q ∈ A | P}
    let F := fun q : Option ℝ × Window ↦ closedLowerLikelihood logSeriesCarrier (m-1) q.1
    let L := fun q ↦ if q.2 = positiveWindow then F q else 1
    (∀ q, (q ∈ W ∧ IsMaxOn L W q) ↔
      q ∈ C ∨ (P ∧ q ∈ A ∧ IsMaxOn F A q)) ∧
    ((∃ q ∈ W, IsMaxOn L W q) ↔
      (∀ i, x i = 0) ∧ (∃ w, allows d fixed w ∧ 0 ∈ cells w) ∨
      P ∧ allows d fixed positiveWindow) ∧
    (P → ((∃! q, q ∈ W ∧ IsMaxOn L W q) ↔ allows d fixed positiveWindow)) := by
  classical
  dsimp only
  let D := closedLowerDomain (Iio 0)
  let C : Set (Option ℝ × Window) :=
    {q | q.1 ∈ D ∧ allows d fixed q.2 ∧ 0 ∈ cells q.2 ∧ ∀ i, x i = 0}
  let A : Set (Option ℝ × Window) :=
    {q | q.1 ∈ D ∧ allows d fixed positiveWindow ∧ q.2 = positiveWindow}
  let P := ∀ i, 1 ≤ x i
  let W := C ∪ {q ∈ A | P}
  let F := fun q : Option ℝ × Window ↦ closedLowerLikelihood logSeriesCarrier (m-1) q.1
  let L := fun q : Option ℝ × Window ↦ if q.2 = positiveWindow then F q else 1
  have zero_excludes (q : Option ℝ × Window) (hq : q ∈ C) : ¬ P := by
    intro h
    have hz := hq.2.2.2 ⟨0,hn⟩
    have hp := h ⟨0,hn⟩
    omega
  have hzero (q : Option ℝ × Window) (hq : q ∈ C) : L q = 1 := by
    have hne : q.2 ≠ positiveWindow := by
      intro he
      have H := hq.2.2.1
      rw [he] at H
      norm_num [cells,positiveWindow] at H
    simp [L,hne]
  have hpositive (q : Option ℝ × Window) (hq : q ∈ A) : L q = F q := by
    have he : q.2 = positiveWindow := hq.2.2
    simp [L,he]
  have positive_window (h : P) : W = A := by
    ext q
    constructor
    · rintro (hq | hq)
      · exact (zero_excludes q hq h).elim
      · exact hq.1
    · intro hq
      exact Or.inr ⟨hq,h⟩
  have no_positive (h : ¬ P) : W = C := by
    ext q
    constructor
    · rintro (hq | hq)
      · exact hq
      · exact (h hq.2).elim
    · exact Or.inl
  have hordinary (q : Option ℝ × Window) (hq : q ∈ C) : IsMaxOn L C q := by
    intro p hp
    change L p ≤ L q
    rw [hzero p hp,hzero q hq]
  have hmax (q : Option ℝ × Window) (hq : q ∈ A) : IsMaxOn L A q ↔ IsMaxOn F A q := by
    constructor <;> intro H p hp
    · have H' := H hp
      change L p ≤ L q at H'
      rwa [hpositive p hp,hpositive q hq] at H'
    · change L p ≤ L q
      rw [hpositive p hp,hpositive q hq]
      exact H hp
  have huniq (h : P) :
      (∃! q, q ∈ A ∧ IsMaxOn F A q) ↔ allows d fixed positiveWindow :=
    nbinom_zero_estimated_alias (by simpa using (mean_bounds hn hm).1 1 h) d fixed
  change (∀ q, (q ∈ W ∧ IsMaxOn L W q) ↔ q ∈ C ∨ P ∧ q ∈ A ∧ IsMaxOn F A q) ∧
    ((∃ q ∈ W, IsMaxOn L W q) ↔ _) ∧
      (P → ((∃! q, q ∈ W ∧ IsMaxOn L W q) ↔ allows d fixed positiveWindow))
  by_cases h : P
  · rw [positive_window h]
    refine ⟨?_,?_,?_⟩
    · intro q
      constructor
      · rintro ⟨hq,hqm⟩
        exact Or.inr ⟨h,hq,(hmax q hq).mp hqm⟩
      · rintro (hq | ⟨_,hq,hqm⟩)
        · exact (zero_excludes q hq h).elim
        · exact ⟨hq,(hmax q hq).mpr hqm⟩
    · constructor
      · rintro ⟨q,hq,hqm⟩
        exact Or.inr ⟨h,hq.2.1⟩
      · rintro (⟨hz,hw⟩ | ⟨_,ha⟩)
        · have HP := h ⟨0,hn⟩
          have HZ := hz ⟨0,hn⟩
          omega
        · obtain ⟨q,⟨hq,hqm⟩,_⟩ := (huniq h).mpr ha
          exact ⟨q,hq,(hmax q hq).mpr hqm⟩
    · intro _
      exact (existsUnique_congr (fun q ↦ and_congr_right (hmax q))).trans (huniq h)
  · rw [no_positive h]
    refine ⟨?_,?_,fun hn ↦ (h hn).elim⟩
    · intro q
      constructor
      · exact fun H ↦ Or.inl H.1
      · rintro (hq | ⟨hh,_,_⟩)
        · exact ⟨hq,hordinary q hq⟩
        · exact (h hh).elim
    · constructor
      · rintro ⟨q,hq,_⟩
        exact Or.inl ⟨hq.2.2.2,q.2,hq.2.1,hq.2.2.1⟩
      · rintro (⟨hz,w,hw,h0⟩ | ⟨hh,_⟩)
        · have hq : (none,w) ∈ C := ⟨by simp [D,closedLowerDomain],hw,h0,hz⟩
          exact ⟨(none,w),hq,hordinary (none,w) hq⟩
        · exact (h hh).elim

private theorem finite_carrier_pos (s : RegularCount) {l u : ℕ} (hv : s.valid (.finite l u)) :
    ∀ k ∈ Finset.Icc l u, 0 < s.carrier k := by
  intro k hk
  cases s with
  | poisson => change 0 < poissonCarrier 0 k; unfold poissonCarrier; positivity
  | geometric => exact one_pos
  | binomial N =>
    have hN : u ≤ N := hv.2
    change 0 < (N.choose k : ℝ)
    exact_mod_cast Nat.choose_pos ((Finset.mem_Icc.mp hk).2.trans hN)
  | negBinomial r => exact nb_pos hv.1 k

/-- Pointwise limits of the finite conditional masses at the named failure
faces. At the finite negative-tilt upper face the log likelihood strictly
increases to the excluded value when the mean reaches the carrier threshold. -/
theorem regular_finite_faces (s : RegularCount) {l u j : ℕ} (hlu : l < u)
    (hv : s.valid (.finite l u)) (hj : j ∈ Finset.Icc l u) (m : ℝ) :
    let c := s.carrier
    let M := finiteCountMass l u c j
    Tendsto M atBot (𝓝 (if j = l then 1 else 0)) ∧
      match s with
      | .poisson | .binomial _ => Tendsto M atTop (𝓝 (if j = u then 1 else 0))
      | _ => Tendsto M (𝓝[<] 0) (𝓝 (c j / ∑ k ∈ Finset.Icc l u, c k)) ∧
        (Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c ≤ m →
          StrictMonoOn (finiteLogLik l u c m) (Iic 0)) := by
  have hc := finite_carrier_pos s hv
  have hl := Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩
  have hu := Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩
  have hlu' : (l : ℝ) < u := by exact_mod_cast hlu
  refine ⟨finite_lower_atom hlu.le hj (hc l hl),?_⟩
  cases s
  · exact finite_upper_atom hlu.le hj (hc u hu)
  · exact ⟨finite_zero_tilt_limit hlu.le hc,fun h ↦
      (Truncation.scalar_negative_upper_failure hc hl hu hlu' h).1⟩
  · exact finite_upper_atom hlu.le hj (hc u hu)
  · exact ⟨finite_zero_tilt_limit hlu.le hc,fun h ↦
      (Truncation.scalar_negative_upper_failure hc hl hu hlu' h).1⟩

/-- The ray's lower failure face is the first retained count. Counts in
the conditional mass function are indexed relative to the lower cut. -/
theorem regular_ray_face (s : RegularCount) (l j : ℕ) (hv : s.valid (.ray l)) :
    let c := fun k ↦ s.carrier (l+k)
    Tendsto (fun t : ℝ ↦ c j * exp t ^ j / (∑' k : ℕ, c k * exp t ^ k)) atBot
      (𝓝 (if j = 0 then 1 else 0)) := by
  cases s with
  | poisson =>
    simpa only [RegularCount.carrier,poissonCarrier,Nat.zero_add] using poisson_lower_atom l j
  | geometric =>
    simpa only [RegularCount.carrier,one_mul] using geom_lower_atom j
  | binomial N => exact False.elim hv
  | negBinomial r => exact nbinom_lower_atom hv l j

/-- On a finite count window the geometric zero-tilt boundary mean is
the midpoint of the retained integer extremes. -/
theorem geom_carrier_midpoint {l u : ℕ} (hlu : l ≤ u) :
    Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) =
      ((l : ℝ) + u) / 2 := by
  let A := Finset.Icc l u
  have ha : A.Nonempty := ⟨l,Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩
  have hc : (0 : ℝ) < A.card := by exact_mod_cast ha.card_pos
  have hmirror (k : ℕ) (hk : k ∈ A) : l+u-k ∈ A := by
    have H := Finset.mem_Icc.mp hk
    exact Finset.mem_Icc.mpr ⟨by omega,by omega⟩
  have he : (∑ k ∈ A, ((l+u-k : ℕ) : ℝ)) = ∑ k ∈ A, (k : ℝ) := by
    refine Finset.sum_bij (fun k _ ↦ l+u-k) hmirror ?_ ?_ (fun _ _ ↦ rfl)
    · intro i hi j hj hij
      have Hi := Finset.mem_Icc.mp hi
      have Hj := Finset.mem_Icc.mp hj
      omega
    · intro k hk
      refine ⟨l+u-k,hmirror k hk,?_⟩
      have H := Finset.mem_Icc.mp hk
      omega
  have hs : (∑ k ∈ A, (k : ℝ)) + ∑ k ∈ A, ((l+u-k : ℕ) : ℝ) =
      (A.card : ℝ) * ((l : ℝ) + u) := by
    rw [← Finset.sum_add_distrib]
    calc
      _ = ∑ _ ∈ A, ((l : ℝ) + u) := by
        apply Finset.sum_congr rfl
        intro k hk
        have H := Finset.mem_Icc.mp hk
        rw [Nat.cast_sub (show k ≤ l+u by omega),Nat.cast_add]
        ring
      _ = _ := by simp only [Finset.sum_const,nsmul_eq_mul]
  change (∑ k ∈ A, 1 * (k : ℝ)) / (∑ _ ∈ A, (1 : ℝ)) = _
  simp only [one_mul,Finset.sum_const,nsmul_eq_mul,mul_one]
  apply (div_eq_iff hc.ne').mpr
  rw [he] at hs
  linarith [hs, mul_comm (A.card : ℝ) ((l : ℝ)+u)]

/-- The size-zero logarithmic alias selects the probability-one atom
exactly at unit positive mean. -/
theorem nbinom_zero_alias_atom {m : ℝ} (_hm : 1 ≤ m) :
    IsMaxOn (closedLowerLikelihood logSeriesCarrier (m-1))
      (closedLowerDomain (Iio 0)) none ↔ m = 1 := by
  have hc (k : ℕ) : 0 < logSeriesCarrier k := one_div_pos.mpr (by positivity)
  have hs : ∀ t < 0, Summable (fun k ↦ logSeriesCarrier k * exp t ^ k) := by
    intro t ht
    apply (summable_geometric_of_lt_one (exp_pos t).le (exp_lt_one_iff.mpr ht)).of_nonneg_of_le
    · intro k
      exact mul_nonneg (hc k).le (pow_nonneg (exp_pos t).le _)
    · intro k
      have H : logSeriesCarrier k ≤ 1 := by
        unfold logSeriesCarrier
        exact (div_le_one (by positivity)).mpr (by linarith [Nat.cast_nonneg (α := ℝ) k])
      simpa only [one_mul] using mul_le_mul_of_nonneg_right H (pow_nonneg (exp_pos t).le k)
  constructor
  · intro H
    by_contra hn
    have hp : 0 < closedLowerLikelihood logSeriesCarrier (m-1) (some (-1)) :=
      inv_pos.mpr (centered_positive hc (hs _ (by norm_num)))
    have hx : some (-1 : ℝ) ∈ closedLowerDomain (Iio 0) := by simp [closedLowerDomain]
    have hb := H hx
    change closedLowerLikelihood logSeriesCarrier (m-1) (some (-1)) ≤ _ at hb
    have hz : closedLowerLikelihood logSeriesCarrier (m-1) none = 0 := by
      simp [closedLowerLikelihood,sub_ne_zero.mpr hn]
    exact hp.not_ge (hz ▸ hb)
  · intro he
    subst m
    obtain ⟨p,⟨hp,hpm⟩,huniq⟩ := nbinom_zero_alias (m := 1) le_rfl
    have he : p = none := by
      cases p with
      | none => rfl
      | some t =>
        have ht : t < 0 := hp t rfl
        have hn : (none : Option ℝ) ∈ closedLowerDomain (Iio 0) := by simp [closedLowerDomain]
        have H := (hs t ht).sum_le_tsum ({0,1} : Finset ℕ)
          (fun k _ ↦ mul_nonneg (hc k).le (pow_nonneg (exp_pos t).le _))
        have hsum : logSeriesCarrier 0 + logSeriesCarrier 1 * exp t ≤
            centeredSum logSeriesCarrier 0 t := by
          simpa [centeredSum,mul_comm t,exp_nat_mul] using H
        have hlt := (lt_add_of_pos_right _ (mul_pos (hc 1) (exp_pos t))).trans_le hsum
        have H' : closedLowerLikelihood logSeriesCarrier (1-1) (some t) <
            closedLowerLikelihood logSeriesCarrier (1-1) none := by
          simpa [closedLowerLikelihood] using (inv_lt_inv₀ ((hc 0).trans hlt) (hc 0)).mpr hlt
        exact (H'.not_ge (hpm hn)).elim
    exact he ▸ hpm

private theorem log_series_objective (m : ℝ) {t : ℝ} (ht : t < 0) :
    closedLowerLikelihood logSeriesCarrier (m-1) (some t) =
      exp (m*t - log (-log (1-exp t))) := by
  have he := exp_pos t
  have hq := exp_lt_one_iff.mpr ht
  have hZ : 0 < -log (1-exp t) := neg_pos.mpr (log_neg (sub_pos.mpr hq) (by linarith))
  have H := hasSum_pow_div_log_of_abs_lt_one (show |exp t| < 1 by simpa [abs_of_pos he] using hq)
  have hterm (k : ℕ) : logSeriesCarrier k * exp (t * ((k : ℝ) - (m-1))) =
      exp (-(m*t)) * (exp t ^ (k+1) / ((k : ℝ)+1)) := by
    unfold logSeriesCarrier
    rw [← exp_nat_mul, ← mul_div_assoc, ← exp_add]
    have harg : t * ((k : ℝ) - (m-1)) = -(m*t) + ((k+1 : ℕ) : ℝ)*t := by push_cast; ring
    rw [harg, one_div_mul_eq_div]
  change (∑' k : ℕ, logSeriesCarrier k * exp (t * ((k : ℝ) - (m-1))))⁻¹ = _
  simp only [hterm,tsum_mul_left]
  have hsum : (∑' k : ℕ, exp t ^ (k+1) / ((k : ℝ)+1)) = -log (1-exp t) := by
    simpa only [Nat.cast_add,Nat.cast_one] using H.tsum_eq
  rw [hsum,exp_sub,exp_log hZ,mul_inv,exp_neg,inv_inv,div_eq_mul_inv]

/-- At an interior size-zero alias maximizer the logarithmic model mean
matches the sample mean. The public probability is `1 - exp t`. -/
theorem nbinom_zero_alias_match {m t : ℝ} (ht : t < 0)
    (hmax : IsMaxOn (closedLowerLikelihood logSeriesCarrier (m-1))
      (closedLowerDomain (Iio 0)) (some t)) :
    m = exp t / ((1-exp t) * (-log (1-exp t))) := by
  let F := fun s : ℝ ↦ m*s - log (-log (1-exp s))
  have hmax' : IsMaxOn F (Iio 0) t := by
    intro s hs
    have H := hmax (show some s ∈ closedLowerDomain (Iio 0) from by
      simpa [closedLowerDomain] using hs)
    change closedLowerLikelihood logSeriesCarrier (m-1) (some s) ≤
      closedLowerLikelihood logSeriesCarrier (m-1) (some t) at H
    rw [log_series_objective m hs,log_series_objective m ht] at H
    exact exp_le_exp.mp H
  have hq : 0 < 1-exp t := sub_pos.mpr (exp_lt_one_iff.mpr ht)
  have hZ : 0 < -log (1-exp t) :=
    neg_pos.mpr (log_neg hq (by linarith [exp_pos t]))
  have hD : HasDerivAt F (m - exp t / ((1-exp t) * (-log (1-exp t)))) t := by
    have H := ((hasDerivAt_id t).const_mul m).sub
      ((((hasDerivAt_exp t).const_sub 1).log hq.ne').neg.log hZ.ne')
    convert H using 1
    · rfl
    · simp only [mul_one, neg_div, neg_neg, div_div, Pi.neg_apply]
  exact sub_eq_zero.mp (hD.deriv ▸ (hmax'.isLocalMax (Iio_mem_nhds ht)).deriv_eq_zero)

end

end UnivariateML.Trunc2Known

end
