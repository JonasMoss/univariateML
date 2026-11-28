module

public import UnivariateML.Generic.Truncation
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics
public import Mathlib.Analysis.SpecialFunctions.Pow.Continuity
public import Mathlib.Analysis.Normed.Algebra.Exponential

/-! Zero-modifier applications and the fixed-probability CMB counterexample.

The Poisson positive objective omits the product of inverse factorials,
which is independent of the fitted rate. The CMB example uses rationally
specified real masses on the dispersion-one binomial slice. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.TruncZero

noncomputable section

open Truncation

theorem modified_all_zero {P : Type*} {D : Set P} {z : ℕ} (hz : 0 < z)
    {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ Icc 0 1) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z 0 t.2) (D ×ˢ Icc 0 1) (p, w) ↔ w = 1 := by
  have h := modified_max_iff (k := 0) (L := fun (_ : P) ↦ 1)
    (by simpa using hz) (fun _ _ ↦ zero_le_one) ⟨p, hp, one_pos⟩ hp hw
  have hconst : IsMaxOn (fun (_ : P) ↦ (1 : ℝ)) D p := by
    intro q hq
    exact le_rfl
  simpa [hconst, show (z : ℝ) ≠ 0 by exact_mod_cast hz.ne'] using h

open Discrete.ZeroTruncatedPoisson in
/-- Mean matching on the positive sample, with the declared zero-rate
atom at one, gives a unique zero-truncated Poisson fit. -/
theorem poisson_zt_fit {m : ℝ} (hm : 1 ≤ m) :
    ∃! rate, rate ∈ Ici (0 : ℝ) ∧ IsMaxOn (likelihood m) (Ici 0) rate := by
  rcases hm.eq_or_lt with he | h
  · rw [← he]
    exact ⟨0, ⟨le_rfl, (all_ones_attained 0 le_rfl).mpr rfl⟩,
      fun t ⟨ht, htmax⟩ ↦ (all_ones_attained t ht).mp htmax⟩
  · obtain ⟨⟨r, ⟨hr, hmean⟩, huniq⟩, _⟩ := unique_mle h
    exact ⟨r, ⟨hr.le, (closed_domain_mle h r hr.le).mpr ⟨hr, hmean⟩⟩,
      fun t ⟨ht, htmax⟩ ↦ huniq t ((closed_domain_mle h t ht).mp htmax)⟩

open Discrete.Geometric in
/-- The positive-geometric optimum is the reciprocal positive mean,
including the all-ones atom at probability one. -/
theorem geom_zt_fit {m : ℝ} (hm : 1 ≤ m) :
    ∃! p, p ∈ Ioc (0 : ℝ) 1 ∧ IsMaxOn (likelihood (m - 1)) (Ioc 0 1) p := by
  have hd : 0 < 1 + (m-1) := by linarith
  have hp : 1 / (1 + (m-1)) ∈ Ioc (0 : ℝ) 1 :=
    ⟨one_div_pos.mpr hd, (div_le_one hd).mpr (by linarith)⟩
  exact ⟨_, ⟨hp, (unique_mle (sub_nonneg.mpr hm) _ hp).mpr rfl⟩,
    fun p ⟨hp, hmax⟩ ↦ (unique_mle (sub_nonneg.mpr hm) p hp).mp hmax⟩

/-- With positives, zero-modified Poisson has a unique joint fit on its
closed rate and zero-weight domain. -/
theorem poisson_zm_fit {z k : ℕ} (hk : 0 < k) {m : ℝ} (hm : 1 ≤ m) :
    ∃! t : ℝ × ℝ, t ∈ Ici 0 ×ˢ Icc 0 1 ∧ IsMaxOn
      (fun t : ℝ × ℝ ↦ bernoulli z k t.2 * Discrete.ZeroTruncatedPoisson.likelihood m t.1 ^ k)
      (Ici 0 ×ˢ Icc 0 1) t := by
  open Discrete.ZeroTruncatedPoisson in
  have hpos : ∀ rate > 0, 0 < likelihood m rate := fun rate hr ↦
    (held_positive_iff hr.le).mpr (Or.inl hr)
  apply modified_unique hk _ ⟨1, zero_le_one, hpos 1 one_pos⟩ (poisson_zt_fit hm)
  intro rate hr
  rcases eq_or_lt_of_le hr with h | h
  · subst rate
    by_cases h : m = 1 <;> simp [Discrete.ZeroTruncatedPoisson.likelihood, h]
  · exact (hpos rate h).le

/-- With positives, zero-modified geometric has a unique joint fit. -/
theorem geom_zm_fit {z k : ℕ} (hk : 0 < k) {m : ℝ} (hm : 1 ≤ m) :
    ∃! t : ℝ × ℝ, t ∈ Ioc 0 1 ×ˢ Icc 0 1 ∧ IsMaxOn
      (fun t : ℝ × ℝ ↦ bernoulli z k t.2 * Discrete.Geometric.likelihood (m-1) t.1 ^ k)
      (Ioc 0 1 ×ˢ Icc 0 1) t := by
  have hp : (1/2 : ℝ) ∈ Ioc 0 1 := ⟨by norm_num, by norm_num⟩
  apply modified_unique hk _ ⟨1/2, hp, (Discrete.Geometric.held_positive_iff hp).mpr
    (Or.inl (by norm_num))⟩ (geom_zt_fit hm)
  intro p _
  unfold Discrete.Geometric.likelihood Discrete.Binomial.likelihood
  split_ifs
  · exact le_rfl
  · exact (exp_pos _).le

/-- The positive geometric likelihood in continuation-probability coordinates.
Here `d` is the sum of positive counts minus the number of positives. -/
def inflatedGeom (z k d : ℕ) (t : ℝ × ℝ) : ℝ :=
  bernoulli z k t.2 * bernoulli d k t.1

/-- The nondegenerate positive-data chart for free geometric inflation. -/
def inflatedGeomDomain : Set (ℝ × ℝ) :=
  {t | t.1 ∈ Ioo 0 1 ∧ t.2 ∈ Icc (1-t.1) 1}

private theorem geom_at_parent (z k d : ℕ) (q : ℝ) :
    inflatedGeom z k d (q, 1-q) = bernoulli (d+k) (z+k) q := by
  have H : 1 - (1 - q) = q := by ring
  simp only [inflatedGeom, bernoulli, H, pow_add]
  ring

private theorem geom_profile_max_iff {z k d : ℕ} (hk : 0 < k)
    {q w : ℝ} (hq : q ∈ Ioo 0 1) (hw : w ∈ Icc (1-q) 1) :
    IsMaxOn (fun v ↦ inflatedGeom z k d (q,v)) (Icc (1-q) 1) w ↔
      w = max (1-q) (zeroFraction z k) := by
  have h : 0 < bernoulli d k q :=
    mul_pos (pow_pos hq.1 _) (pow_pos (sub_pos.mpr hq.2) _)
  rw [← bernoulli_clip (show 0 < z+k by omega)
    (show 1-q ∈ Icc (0 : ℝ) 1 from ⟨sub_nonneg.mpr hq.2.le, by linarith [hq.1]⟩) hw]
  constructor <;> intro H v hv
  · exact (mul_le_mul_iff_left₀ h).mp
      (show bernoulli z k v * bernoulli d k q ≤ bernoulli z k w * bernoulli d k q from H hv)
  · change bernoulli z k v * bernoulli d k q ≤ bernoulli z k w * bernoulli d k q
    exact (mul_le_mul_iff_left₀ h).mpr (H hv)

private theorem geom_thresholds (z k d : ℕ) (hk : 0 < k) :
    let a := 1 - zeroFraction z k
    (a ≤ zeroFraction d k ↔ a ≤ zeroFraction (d+k) (z+k)) ∧
    (zeroFraction d k < a ↔ zeroFraction (d+k) (z+k) < a) := by
  dsimp only
  have hK : (0 : ℝ) < k := by exact_mod_cast hk
  have hZ : 0 ≤ (z : ℝ) := Nat.cast_nonneg _
  have hD : 0 ≤ (d : ℝ) := Nat.cast_nonneg _
  have h₁ : (0 : ℝ) < z + k := by linarith
  have h₂ : (0 : ℝ) < d + k := by linarith
  have h₃ : (0 : ℝ) < d + k + (z + k) := by linarith
  have he : 1 - zeroFraction z k = (k : ℝ) / (z+k) := by
    unfold zeroFraction
    field_simp
    ring
  rw [he]
  simp only [zeroFraction, Nat.cast_add]
  rw [div_le_div_iff₀ h₁ h₂, div_le_div_iff₀ h₁ h₃,
    div_lt_div_iff₀ h₂ h₁, div_lt_div_iff₀ h₃ h₁]
  constructor <;> constructor <;> intro H <;> nlinarith

/-- With positives, free geometric inflation fits the positive
conditional solution if feasible, and otherwise fits the ordinary
parent with no inflation. The seam belongs to both descriptions. -/
theorem geom_zi_fit {z k d : ℕ} (hk : 0 < k) :
    let a := 1 - zeroFraction z k
    let q := if a ≤ zeroFraction d k then zeroFraction d k
      else zeroFraction (d+k) (z+k)
    let w := max (1-q) (zeroFraction z k)
    (q,w) ∈ inflatedGeomDomain ∧ IsMaxOn (inflatedGeom z k d) inflatedGeomDomain (q,w) ∧
      ∀ t ∈ inflatedGeomDomain, IsMaxOn (inflatedGeom z k d) inflatedGeomDomain t → t = (q,w) := by
  dsimp only
  let a := 1 - zeroFraction z k
  let qc := zeroFraction d k
  let qb := zeroFraction (d+k) (z+k)
  let zf := zeroFraction z k
  let prof (u : ℝ) := inflatedGeom z k d (u,max (1-u) zf)
  have hkR : (0 : ℝ) < k := by exact_mod_cast hk
  have hzR : 0 ≤ (z : ℝ) := Nat.cast_nonneg _
  have hdR : 0 ≤ (d : ℝ) := Nat.cast_nonneg _
  have hz : zf ∈ Icc (0 : ℝ) 1 := by
    constructor
    · exact div_nonneg hzR (by linarith)
    · exact (div_le_one (show (0 : ℝ) < z + k by linarith)).mpr (by linarith)
  have ha : 0 < a := by
    apply sub_pos.mpr
    exact (div_lt_one (show (0 : ℝ) < z + k by linarith)).mpr (by linarith)
  have haq : a ≤ 1 := by dsimp [a, zf]; linarith [hz.1]
  have hqc : qc ∈ Icc (0 : ℝ) 1 := by
    exact ⟨div_nonneg hdR (by linarith),
      (div_le_one (show (0 : ℝ) < d + k by linarith)).mpr (by linarith)⟩
  have hc1 : qc < 1 := (div_lt_one (show (0 : ℝ) < d + k by linarith)).mpr (by linarith)
  have hqb : qb ∈ Ioo (0 : ℝ) 1 := by
    simp only [qb, zeroFraction, Nat.cast_add]
    exact ⟨div_pos (by linarith) (by linarith),
      (div_lt_one (by linarith)).mpr (by linarith)⟩
  have hW {u : ℝ} (hu : u ∈ Ioo 0 1) : max (1-u) zf ∈ Icc (1-u) 1 :=
    ⟨le_max_left _ _, max_le (by linarith [hu.1]) hz.2⟩
  have hLeft {u : ℝ} (hu : u ≤ a) : prof u = bernoulli (d+k) (z+k) u := by
    have h : zf ≤ 1-u := by dsimp [a, zf] at hu ⊢; linarith
    dsimp [prof]
    rw [max_eq_left h, geom_at_parent]
  have hRight {u : ℝ} (hu : a ≤ u) : prof u = bernoulli z k zf * bernoulli d k u := by
    have h : 1-u ≤ zf := by dsimp [a, zf] at hu ⊢; linarith
    dsimp [prof]
    rw [max_eq_right h]
    rfl
  have hB : IsMaxOn (bernoulli (d+k) (z+k)) (Icc 0 1) qb :=
    (bernoulli_max_iff (by omega) ⟨hqb.1.le,hqb.2.le⟩).mpr rfl
  have hC : IsMaxOn (bernoulli d k) (Icc 0 1) qc :=
    (bernoulli_max_iff (by omega) hqc).mpr rfl
  have hBpos : 0 < bernoulli z k zf := by
    have H := (bernoulli_max_iff (show 0 < z+k by omega) hz).mpr rfl
    exact (show 0 < bernoulli z k (1/2) from by
      unfold bernoulli
      positivity).trans_le (H (show (1/2 : ℝ) ∈ Icc 0 1 from by norm_num))
  have hstrictB {u : ℝ} (hu : u ∈ Icc 0 1) (hne : u ≠ qb) :
      bernoulli (d+k) (z+k) u < bernoulli (d+k) (z+k) qb := by
    refine lt_of_le_of_ne (hB hu) (fun he ↦ hne ?_)
    apply (bernoulli_max_iff (by omega) hu).mp
    intro v hv
    change bernoulli (d+k) (z+k) v ≤ bernoulli (d+k) (z+k) u
    rw [he]
    exact hB hv
  have hstrictC {u : ℝ} (hu : u ∈ Icc 0 1) (hne : u ≠ qc) :
      bernoulli d k u < bernoulli d k qc := by
    refine lt_of_le_of_ne (hC hu) (fun he ↦ hne ?_)
    apply (bernoulli_max_iff (by omega) hu).mp
    intro v hv
    change bernoulli d k v ≤ bernoulli d k u
    rw [he]
    exact hC hv
  have hfinish {q : ℝ} (hq : q ∈ Ioo 0 1)
      (hprof : ∀ u ∈ Ioo (0 : ℝ) 1, u ≠ q → prof u < prof q) :
      (q,max (1-q) zf) ∈ inflatedGeomDomain ∧
      IsMaxOn (inflatedGeom z k d) inflatedGeomDomain (q,max (1-q) zf) ∧
      ∀ t ∈ inflatedGeomDomain, IsMaxOn (inflatedGeom z k d) inflatedGeomDomain t →
        t = (q,max (1-q) zf) := by
    have H : (q,max (1-q) zf) ∈ inflatedGeomDomain := ⟨hq, hW hq⟩
    refine ⟨H, ?_, ?_⟩
    · rintro ⟨u,v⟩ ⟨hu,hv⟩
      have hvmax := (geom_profile_max_iff (d := d) hk hu (hW hu)).mpr rfl
      exact (hvmax hv).trans (if he : u = q then he ▸ le_rfl else (hprof u hu he).le)
    · rintro ⟨u,v⟩ ⟨hu,hv⟩ hmax
      have hvmax := (geom_profile_max_iff (d := d) hk hu (hW hu)).mpr rfl
      have hbound : prof q ≤ prof u := (hmax H).trans (hvmax hv)
      have he : u = q := by
        by_contra hne
        exact (hprof u hu hne).not_ge hbound
      have hw : IsMaxOn (fun w ↦ inflatedGeom z k d (u,w)) (Icc (1-u) 1) v :=
        fun w hw ↦ hmax (a := (u,w)) ⟨hu,hw⟩
      exact Prod.ext he ((geom_profile_max_iff (d := d) hk hu hv).mp hw |>.trans (by rw [he]))
  let q := if a ≤ qc then qc else qb
  change (q, max (1-q) zf) ∈ inflatedGeomDomain ∧
    IsMaxOn (inflatedGeom z k d) inflatedGeomDomain (q,max (1-q) zf) ∧
    ∀ t ∈ inflatedGeomDomain, IsMaxOn (inflatedGeom z k d) inflatedGeomDomain t →
      t = (q,max (1-q) zf)
  by_cases h : a ≤ qc
  · simp only [q, ite_eq_left h]
    have hb : a ≤ qb := (geom_thresholds z k d hk).1.mp h
    apply hfinish ⟨ha.trans_le h,hc1⟩
    intro u hu hne
    by_cases hlu : a ≤ u
    · rw [hRight hlu, hRight h]
      exact mul_lt_mul_of_pos_left (hstrictC ⟨hu.1.le,hu.2.le⟩ hne) hBpos
    · have hlu' : u < a := lt_of_not_ge hlu
      have hinc := bernoulli_strictMono (show 0 < (d+k)+(z+k) by omega)
      calc
        prof u < prof a := by
          rw [hLeft hlu'.le, hLeft le_rfl]
          exact hinc ⟨hu.1.le,hlu'.le.trans hb⟩ ⟨ha.le,hb⟩ hlu'
        _ ≤ prof qc := by
          rw [hRight le_rfl, hRight h]
          exact mul_le_mul_of_nonneg_left (hC (show a ∈ Icc 0 1 from ⟨ha.le,haq⟩)) hBpos.le
  · simp only [q, ite_eq_right h]
    have hc : qc < a := lt_of_not_ge h
    have hb : qb < a := (geom_thresholds z k d hk).2.mp hc
    apply hfinish hqb
    intro u hu hne
    by_cases hlu : u ≤ a
    · rw [hLeft hlu, hLeft hb.le]
      exact hstrictB ⟨hu.1.le,hu.2.le⟩ hne
    · have hlu' : a < u := lt_of_not_ge hlu
      have hdec := bernoulli_strictAnti (show 0 < d+k by omega)
      calc
        prof u < prof a := by
          rw [hRight hlu'.le, hRight le_rfl]
          exact mul_lt_mul_of_pos_left (hdec ⟨hc.le,haq⟩ ⟨hc.le.trans hlu'.le,hu.2.le⟩ hlu') hBpos
        _ ≤ prof qb := by
          rw [hLeft le_rfl, hLeft hb.le]
          exact hB (show a ∈ Icc 0 1 from ⟨ha.le,haq⟩)

/-- Inflated binomial likelihood at fixed size, with the zeros counted
separately from the strictly positive observations. -/
def inflatedBinom {k : ℕ} (N z : ℕ) (x : Fin k → ℕ) (t : ℝ × ℝ) : ℝ :=
  (t.2 + (1-t.2)*(1-t.1)^N)^z * (1-t.2)^k *
    ∏ i, (N.choose (x i) : ℝ) * t.1^(x i) * (1-t.1)^(N-x i)

/-- Both coefficients of the fixed-size inflated binomial range over
closed unit intervals, so its likelihood attains its maximum. -/
theorem binom_zi_attainment {k : ℕ} (N z : ℕ) (x : Fin k → ℕ) :
    ∃ t ∈ (Icc (0 : ℝ) 1 ×ˢ Icc (0 : ℝ) 1),
      IsMaxOn (inflatedBinom N z x) (Icc 0 1 ×ˢ Icc 0 1) t := by
  have H : Continuous (inflatedBinom N z x) := by unfold inflatedBinom; fun_prop
  exact (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨(0,0), by norm_num⟩ H.continuousOn

/-- With inflation held, the fixed-size binomial likelihood attains
on the closed probability interval. -/
theorem binom_zi_held_inflation_attainment {k : ℕ} (N z : ℕ) (x : Fin k → ℕ) (rho : ℝ) :
    ∃ p ∈ Icc (0 : ℝ) 1, IsMaxOn (fun p ↦ inflatedBinom N z x (p,rho)) (Icc 0 1) p := by
  have H : Continuous (fun p ↦ inflatedBinom N z x (p,rho)) := by
    unfold inflatedBinom
    fun_prop
  exact isCompact_Icc.exists_isMaxOn ⟨0, by norm_num⟩ H.continuousOn

/-- At binomial size one the full inflated maximizing set is the
Bernoulli product curve. -/
theorem binom_zi_size_one {z k : ℕ} (hn : 0 < z+k) {x : Fin k → ℕ} (hx : ∀ i, x i = 1)
    {p rho : ℝ} (hp : p ∈ Icc 0 1) (hrho : rho ∈ Icc 0 1) :
    IsMaxOn (inflatedBinom 1 z x) (Icc 0 1 ×ˢ Icc 0 1) (p,rho) ↔
      (1-rho)*p = 1 - zeroFraction z k := by
  let w (u v : ℝ) := 1 - (1-v)*u
  have he (u v : ℝ) : inflatedBinom 1 z x (u,v) = bernoulli z k (w u v) := by
    have h₁ : v + (1-v)*(1-u) = w u v := by dsimp [w]; ring
    have h₂ : 1 - w u v = (1-v)*u := by dsimp [w]; ring
    simp [inflatedBinom, hx, bernoulli, h₁, h₂, mul_pow, mul_assoc]
  have hw (u v : ℝ) (hu : u ∈ Icc 0 1) (hv : v ∈ Icc 0 1) : w u v ∈ Icc 0 1 := by
    have hV : 0 ≤ 1-v := sub_nonneg.mpr hv.2
    constructor
    · exact sub_nonneg.mpr ((mul_le_mul_of_nonneg_left hu.2 hV).trans (by linarith [hv.1]))
    · exact sub_le_self _ (mul_nonneg hV hu.1)
  have hTransfer :
      IsMaxOn (inflatedBinom 1 z x) (Icc 0 1 ×ˢ Icc 0 1) (p,rho) ↔
        IsMaxOn (bernoulli z k) (Icc 0 1) (w p rho) := by
    constructor
    · intro H u hu
      have H' : inflatedBinom 1 z x (1-u,0) ≤ inflatedBinom 1 z x (p,rho) :=
        H (a := (1-u,0)) ⟨⟨sub_nonneg.mpr hu.2, sub_le_self _ hu.1⟩, by norm_num⟩
      rw [he,he, show w (1-u) 0 = u by dsimp [w]; ring] at H'
      exact H'
    · rintro H ⟨u,v⟩ ⟨hu,hv⟩
      change inflatedBinom 1 z x (u,v) ≤ inflatedBinom 1 z x (p,rho)
      rw [he,he]
      exact H (hw u v hu hv)
  rw [hTransfer, bernoulli_max_iff hn (hw p rho hp hrho)]
  change 1 - (1-rho)*p = zeroFraction z k ↔ _
  constructor <;> intro H <;> linarith

/-- With only zeros, a free inflated zero mass is optimal exactly
when the parent is already a zero atom or inflation is one. -/
theorem inflated_all_zero {z : ℕ} (hz : 0 < z) {f rho : ℝ}
    (hf : f ∈ Icc (0 : ℝ) 1) (hrho : rho ∈ Icc (0 : ℝ) 1) :
    (rho + (1-rho)*f)^z = 1 ↔ f = 1 ∨ rho = 1 := by
  have he : rho + (1-rho)*f = 1 - (1-rho)*(1-f) := by ring
  constructor
  · intro H
    by_contra! h
    have hlt : rho + (1-rho)*f < 1 := by
      rw [he]
      exact sub_lt_self _ (mul_pos (sub_pos.mpr (lt_of_le_of_ne hrho.2 h.2))
        (sub_pos.mpr (lt_of_le_of_ne hf.2 h.1)))
    have hnn : 0 ≤ rho + (1-rho)*f :=
      add_nonneg hrho.1 (mul_nonneg (sub_nonneg.mpr hrho.2) hf.1)
    exact (pow_lt_one₀ hnn hlt hz.ne').ne H
  · rintro (rfl | rfl) <;> ring_nf

/-- ZIP likelihood with parameter-independent factorials suppressed.
`S` is the sum of the positive observations. -/
def inflatedPoisson (z k S : ℕ) (t : ℝ × ℝ) : ℝ :=
  (t.2 + (1-t.2)*exp (-t.1))^z * (1-t.2)^k * (t.1^S * exp (-(k : ℝ)*t.1))

private theorem poisson_zi_envelope (z k S : ℕ) {rate rho : ℝ} (hr : 0 < rate)
    (hi : rho ∈ Icc 0 1) :
    inflatedPoisson z k S (rate,rho) ≤ rate^S * exp (-(k : ℝ)*rate) := by
  have hW₀ : 0 ≤ rho + (1-rho)*exp (-rate) :=
    add_nonneg hi.1 (mul_nonneg (sub_nonneg.mpr hi.2) (exp_pos _).le)
  have hW₁ : rho + (1-rho)*exp (-rate) ≤ 1 := by
    have H := mul_le_mul_of_nonneg_left ((exp_lt_one_iff.mpr (neg_lt_zero.mpr hr)).le)
      (sub_nonneg.mpr hi.2)
    linarith
  unfold inflatedPoisson
  exact mul_le_of_le_one_left (mul_nonneg (pow_nonneg hr.le _) (exp_pos _).le)
    (mul_le_of_le_one_left (pow_nonneg (sub_nonneg.mpr hi.2) _) (pow_le_one₀ hW₀ hW₁) |>.trans
      (pow_le_one₀ (sub_nonneg.mpr hi.2) (by linarith [hi.1])))

/-- With positives, a ZIP fit with free inflation attains at a finite
positive rate. The same compactness argument applies at any held
inflation strictly below one. -/
theorem poisson_zi_positive_attainment {z k S : ℕ} (hk : 0 < k) (hS : 0 < S)
    {W : Set ℝ} (hW : IsCompact W) (hvalid : W ⊆ Icc 0 1)
    {rho : ℝ} (hrho : rho ∈ W) (hrho' : rho < 1) :
    ∃ t ∈ (Ioi (0 : ℝ) ×ˢ W),
      IsMaxOn (inflatedPoisson z k S) (Ioi 0 ×ˢ W) t := by
  let L := inflatedPoisson z k S
  let b (t : ℝ) := t^S * exp (-(k : ℝ)*t)
  let v := L (1,rho)
  have hv : 0 < v := by
    have hi := hvalid hrho
    have hP : 0 < rho + (1-rho)*exp (-(1 : ℝ)) :=
      add_pos_of_nonneg_of_pos hi.1 (mul_pos (sub_pos.mpr hrho') (exp_pos _))
    exact mul_pos (mul_pos (pow_pos hP _) (pow_pos (sub_pos.mpr hrho') _))
      (mul_pos (pow_pos one_pos _) (exp_pos _))
  have hLeft : Tendsto b (𝓝[>] 0) (𝓝 0) := by
    have H : Continuous b := by unfold b; fun_prop
    simpa [b, zero_pow hS.ne'] using (H.continuousAt (x := 0)).tendsto.mono_left
      (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
  have hRight : Tendsto b atTop (𝓝 0) := by
    have hp : (0 : ℝ) < k := by exact_mod_cast hk
    have H := tendsto_rpow_mul_exp_neg_mul_atTop_nhds_zero (S : ℝ) (k : ℝ) hp
    simpa [b, Real.rpow_natCast, neg_mul] using H
  have hlo : {t | b t < v} ∈ 𝓝[>] (0 : ℝ) := hLeft.eventually (Iio_mem_nhds hv)
  obtain ⟨a, ha, ha'⟩ := (mem_nhdsGT_iff_exists_Ioo_subset.mp hlo)
  obtain ⟨d, hd⟩ := eventually_atTop.mp (hRight.eventually (Iio_mem_nhds hv))
  let lo := min a 1
  let hi := max d 1
  let D := Icc lo hi ×ˢ W
  have hD : D.Nonempty := ⟨(1,rho), ⟨⟨min_le_right _ _,le_max_right _ _⟩,hrho⟩⟩
  have hL : Continuous L := by unfold L inflatedPoisson; fun_prop
  obtain ⟨t, ht, hmax⟩ := (isCompact_Icc.prod hW).exists_isMaxOn hD hL.continuousOn
  have ht0 : 0 < t.1 := (lt_min ha zero_lt_one).trans_le ht.1.1
  have hvmax : v ≤ L t := hmax (a := (1,rho)) ⟨⟨min_le_right _ _,le_max_right _ _⟩,hrho⟩
  refine ⟨t, ⟨ht0,ht.2⟩, ?_⟩
  intro u hu
  change L u ≤ L t
  by_cases h : u.1 ∈ Icc lo hi
  · exact hmax ⟨h,hu.2⟩
  · have hBound := poisson_zi_envelope z k S hu.1 (hvalid hu.2)
    have hlt : b u.1 < v := by
      by_cases he : u.1 < lo
      · exact ha' ⟨hu.1,he.trans_le (min_le_left _ _)⟩
      · exact hd _ (le_max_left _ _ |>.trans (lt_of_not_ge (fun hHi ↦
          h ⟨le_of_not_gt he,hHi⟩)).le)
    exact hBound.trans (hlt.le.trans hvmax)

/-- The ordinary Poisson carrier normalized at a positive rate. -/
def poissonMass (rate : ℝ) (j : ℕ) : ℝ := exp (-rate) * (rate^j / (j.factorial : ℝ))

private theorem poissonSeries (rate : ℝ) :
    HasSum (fun j : ℕ ↦ rate^j / (j.factorial : ℝ)) (exp rate) := by
  rw [Real.exp_eq_exp_ℝ]
  exact NormedSpace.expSeries_div_hasSum_exp rate

private theorem poissonMass_sum (rate : ℝ) : HasSum (poissonMass rate) 1 := by
  have h := (poissonSeries rate).mul_left (exp (-rate))
  change HasSum (poissonMass rate) (exp (-rate) * exp rate) at h
  rwa [← exp_add, neg_add_cancel, exp_zero] at h

private theorem poissonMass_mean (rate : ℝ) :
    HasSum (fun j : ℕ ↦ poissonMass rate j * j) rate := by
  have hShift : HasSum (fun j : ℕ ↦ poissonMass rate (j+1) * (↑(j+1) : ℝ)) rate := by
    convert (poissonMass_sum rate).mul_left rate using 1
    · ext j
      simp only [poissonMass, pow_succ, Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ]
      field_simp
    · ring
  rw [← hasSum_nat_add_iff' 1]
  simpa [Finset.sum_range_one] using hShift

/-- For ZIP, the ordinary mean rate with zero inflation is globally
optimal when its zero mass dominates the observed zero fraction.
The likelihood comparison is expressed in feasible log-tilt increments. -/
theorem poisson_zi_boundary {rate z : ℝ} (hr : 0 < rate) (hz : z ≤ exp (-rate))
    (a d : ℝ) (hd : 0 ≤ d) :
    a*rate + z*d ≤ log (∑' j : ℕ, poissonMass rate j * exp (a*j + if j = 0 then d else 0)) := by
  have h0 : poissonMass rate 0 = exp (-rate) := by simp [poissonMass]
  refine fixed_carrier_inflation_boundary (fun _ ↦ by unfold poissonMass; positivity)
    (h0 ▸ exp_pos _) (poissonMass_sum rate) (poissonMass_mean rate) (h0 ▸ hz) a d hd ?_
  apply (summable_nat_add_iff 1).mp
  apply ((summable_nat_add_iff 1).mpr (poissonSeries (rate * exp a)).summable
    |>.mul_left (exp (-rate))).congr
  intro j
  simp only [poissonMass, Nat.add_one_ne_zero, ite_false, add_zero, mul_pow]
  rw [mul_comm a, exp_nat_mul]
  ring

/-- Positive-size negative-binomial inflation in continuation-probability
coordinates, with positive carrier constants suppressed. -/
def inflatedNegBinom (r : ℝ) (z k S : ℕ) (t : ℝ × ℝ) : ℝ :=
  (t.2 + (1-t.2)*(1-t.1)^r)^z * (1-t.2)^k * t.1^S * ((1-t.1)^r)^k

/-- At positive held NB size, compatible positive data attain in the
interior probability domain, both with inflation free and with any
compatible fixed inflation. -/
theorem nbinom_zi_positive_attainment {r : ℝ} (hr : 0 < r) {z k S : ℕ}
    (hk : 0 < k) (hS : 0 < S) {W : Set ℝ} (hW : IsCompact W)
    (hvalid : W ⊆ Icc 0 1) {rho : ℝ} (hi : rho ∈ W) (hi' : rho < 1) :
    ∃ t ∈ (Ioo (0 : ℝ) 1 ×ˢ W),
      IsMaxOn (inflatedNegBinom r z k S) (Ioo 0 1 ×ˢ W) t := by
  let L := inflatedNegBinom r z k S
  have hP : Continuous (fun q : ℝ ↦ (1-q)^r) :=
    (Real.continuous_rpow_const hr.le).comp (continuous_const.sub continuous_id)
  have hProd : Continuous (fun t : ℝ × ℝ ↦ (1-t.1)^r) := hP.comp continuous_fst
  have hL : Continuous L := by unfold L inflatedNegBinom; fun_prop
  have hmid : ((1/2 : ℝ),rho) ∈ Icc (0 : ℝ) 1 ×ˢ W :=
    ⟨⟨by norm_num,by norm_num⟩,hi⟩
  obtain ⟨t, ht, hmax⟩ := (isCompact_Icc.prod hW).exists_isMaxOn ⟨_, hmid⟩ hL.continuousOn
  have hW' : 0 < rho + (1-rho)*(1-(1/2 : ℝ))^r :=
    add_pos_of_nonneg_of_pos (hvalid hi).1
      (mul_pos (sub_pos.mpr hi') (rpow_pos_of_pos (by norm_num) _))
  have hpos : 0 < L ((1/2 : ℝ),rho) :=
    mul_pos (mul_pos (mul_pos (pow_pos hW' _) (pow_pos (sub_pos.mpr hi') _))
      (pow_pos (by norm_num) _)) (pow_pos (rpow_pos_of_pos (by norm_num) _) _)
  have hV : 0 < L t := hpos.trans_le (hmax hmid)
  have ht' : t.1 ∈ Ioo (0 : ℝ) 1 := by
    constructor
    · rcases eq_or_lt_of_le ht.1.1 with h | h
      · have H : L t = 0 := by simp [L, inflatedNegBinom, ← h, zero_pow hS.ne']
        exact (hV.ne' H).elim
      · exact h
    · rcases eq_or_lt_of_le ht.1.2 with h | h
      · have H : L t = 0 := by
          simp [L, inflatedNegBinom, h, zero_rpow hr.ne', zero_pow hk.ne']
        exact (hV.ne' H).elim
      · exact h
  exact ⟨t, ⟨ht',ht.2⟩, fun u hu ↦ hmax ⟨⟨hu.1.1.le,hu.1.2.le⟩,hu.2⟩⟩

def cmbFailureProb : ℝ := 183/500
def cmbSuccessProb : ℝ := 317/500

/-- Binomial mass at two on the CMB dispersion-one slice of size `n+2`. -/
def cmbMassTwo (n : ℕ) : ℝ := ((n+2 : ℝ) * (n+1) / 2) * cmbSuccessProb^2 * cmbFailureProb^n

/-- Positive-conditional binomial mass at two. -/
def cmbConditionalTwo (n : ℕ) : ℝ := cmbMassTwo n / (1 - cmbFailureProb^(n+2))

private theorem q_pos : 0 < cmbFailureProb := by norm_num [cmbFailureProb]
private theorem q_lt : cmbFailureProb < 1 := by norm_num [cmbFailureProb]
private theorem qpow_lt_one (n : ℕ) : cmbFailureProb^(n+2) < 1 :=
  pow_lt_one₀ q_pos.le q_lt (by omega)

private theorem cmb_mass_pos (n : ℕ) : 0 < cmbMassTwo n := by
  unfold cmbMassTwo cmbSuccessProb cmbFailureProb
  positivity

private theorem cmb_tail (n : ℕ) (hn : 1 ≤ n) :
    cmbConditionalTwo (n+1) < cmbConditionalTwo n := by
  have hN : (1 : ℝ) ≤ n := by exact_mod_cast hn
  have hNum : cmbMassTwo (n+1) < cmbMassTwo n := by
    have H : ((n : ℝ) + 3) * cmbFailureProb < (n : ℝ) + 1 := by dsimp [cmbFailureProb]; linarith
    have K := mul_lt_mul_of_pos_right H
      (show 0 < ((n : ℝ)+2) / 2 * cmbSuccessProb^2 * cmbFailureProb^n by unfold cmbSuccessProb cmbFailureProb; positivity)
    simp only [cmbMassTwo, Nat.cast_add, Nat.cast_one, pow_succ cmbFailureProb]
    convert K using 1 <;> ring
  have hDen : 1 - cmbFailureProb^(n+2) ≤ 1 - cmbFailureProb^(n+1+2) := by
    rw [show n+1+2 = (n+2)+1 by omega, pow_succ cmbFailureProb (n+2)]
    linarith [mul_le_mul_of_nonneg_left q_lt.le (pow_nonneg q_pos.le (n+2))]
  unfold cmbConditionalTwo
  exact (div_lt_div_of_pos_right hNum (sub_pos.mpr (qpow_lt_one (n+1)))).trans_le
    (div_le_div_of_nonneg_left (cmb_mass_pos n).le (sub_pos.mpr (qpow_lt_one n)) hDen)

private theorem cmb_tail_le (n : ℕ) (hn : 1 ≤ n) :
    cmbConditionalTwo n ≤ cmbConditionalTwo 1 := by
  induction n with
  | zero => omega
  | succ n ih =>
    by_cases h : n = 0
    · subst n
      exact le_rfl
    · exact (cmb_tail n (by omega)).le.trans (ih (by omega))

/-- At held probability 317/500 and dispersion one, the zero-modified
conditional winner is size two and its parent zero mass is infeasible.
A size-three fit with positive inflation beats every zero-inflation fit
on the same held slice for one zero and seven twos. -/
theorem cmb_inflation_counterexample :
    (∀ n : ℕ, n ≠ 0 → cmbConditionalTwo n < cmbConditionalTwo 0) ∧
      (1/8 : ℝ) < cmbFailureProb^2 ∧ cmbFailureProb^3 < (1/8 : ℝ) ∧
      (∀ n : ℕ, cmbFailureProb^(n+2) * cmbMassTwo n ^ 7 <
        bernoulli 1 7 (1/8) * cmbConditionalTwo 1 ^ 7) := by
  have hb : cmbConditionalTwo 1 < cmbConditionalTwo 0 := by
    norm_num [cmbConditionalTwo, cmbMassTwo, cmbFailureProb, cmbSuccessProb]
  have hC : ∀ n, n ≠ 0 → cmbConditionalTwo n < cmbConditionalTwo 0 :=
    fun n hn ↦ (cmb_tail_le n (by omega)).trans_lt hb
  refine ⟨hC, by norm_num [cmbFailureProb], by norm_num [cmbFailureProb], ?_⟩
  intro n
  by_cases hn : n = 0
  · subst n
    norm_num [bernoulli, cmbConditionalTwo, cmbMassTwo, cmbFailureProb, cmbSuccessProb]
  · have hW : cmbFailureProb^(n+2) ∈ Icc (0 : ℝ) 1 := ⟨pow_nonneg q_pos.le _, (qpow_lt_one n).le⟩
    have hopt : IsMaxOn (bernoulli 1 7) (Icc 0 1) (1/8) :=
      (bernoulli_max_iff (by decide) ⟨by norm_num, by norm_num⟩).mpr (by norm_num)
    have hB : bernoulli 1 7 (cmbFailureProb^(n+2)) ≤ bernoulli 1 7 (1/8) := hopt hW
    have hStrict : bernoulli 1 7 (cmbFailureProb^(n+2)) < bernoulli 1 7 (1/8) := by
      refine lt_of_le_of_ne hB (fun he ↦ ?_)
      have hm : IsMaxOn (bernoulli 1 7) (Icc 0 1) (cmbFailureProb^(n+2)) := by
        intro v hv
        change bernoulli 1 7 v ≤ bernoulli 1 7 (cmbFailureProb^(n+2))
        rw [he]
        exact hopt hv
      have hq := (bernoulli_max_iff (by decide) hW).mp hm
      have hn3 : 3 ≤ n+2 := by omega
      have hle : cmbFailureProb^(n+2) ≤ cmbFailureProb^3 := pow_le_pow_of_le_one q_pos.le q_lt.le hn3
      norm_num [cmbFailureProb] at hq hle
      linarith
    have hpos (i : ℕ) : 0 < cmbConditionalTwo i :=
      div_pos (cmb_mass_pos i) (sub_pos.mpr (qpow_lt_one i))
    have hc := pow_le_pow_left₀ (hpos n).le (cmb_tail_le n (by omega)) 7
    calc
      cmbFailureProb^(n+2) * cmbMassTwo n ^ 7 =
          bernoulli 1 7 (cmbFailureProb^(n+2)) * cmbConditionalTwo n ^ 7 := by
        unfold bernoulli cmbConditionalTwo
        rw [pow_one, div_pow]
        field_simp [(sub_pos.mpr (qpow_lt_one n)).ne']
      _ < bernoulli 1 7 (1/8) * cmbConditionalTwo 1 ^ 7 :=
        mul_lt_mul_of_lt_of_le_of_pos_of_nonneg hStrict hc (pow_pos (hpos n) _)
          (by norm_num [bernoulli])

/-- The full constrained CMB likelihood of the counterexample is
maximized uniquely at size three and empirical zero weight one eighth. -/
theorem cmb_inflated_unique :
    let D : Set (ℕ × ℝ) := {t | t.2 ∈ Icc (cmbFailureProb^(t.1+2)) 1}
    let L (t : ℕ × ℝ) := bernoulli 1 7 t.2 * cmbConditionalTwo t.1 ^ 7
    (1, (1/8 : ℝ)) ∈ D ∧
      ∀ t ∈ D, t ≠ (1, (1/8 : ℝ)) → L t < L (1, (1/8 : ℝ)) := by
  dsimp only
  have hC := cmb_inflation_counterexample
  refine ⟨⟨hC.2.2.1.le, by norm_num⟩, ?_⟩
  rintro ⟨n,w⟩ hw hne
  change w ∈ Icc (cmbFailureProb^(n+2)) 1 at hw
  change bernoulli 1 7 w * cmbConditionalTwo n ^ 7 <
    bernoulli 1 7 (1/8) * cmbConditionalTwo 1 ^ 7
  have hpos (i : ℕ) : 0 < cmbConditionalTwo i :=
    div_pos (cmb_mass_pos i) (sub_pos.mpr (qpow_lt_one i))
  have hnzero : 0 < (1 : ℕ) + 7 := by decide
  have hf : zeroFraction 1 7 = (1/8 : ℝ) := by norm_num [zeroFraction]
  by_cases hn : n = 0
  · subst n
    have hb : bernoulli 1 7 w ≤ bernoulli 1 7 (cmbFailureProb^2) :=
      ((bernoulli_clip hnzero ⟨pow_nonneg q_pos.le _, (qpow_lt_one 0).le⟩
        ⟨le_rfl,(qpow_lt_one 0).le⟩).mpr (by rw [hf, max_eq_left hC.2.1.le])) hw
    apply (mul_le_mul_of_nonneg_right hb (pow_pos (hpos 0) 7).le).trans_lt
    convert hC.2.2.2 0 using 1
    norm_num [bernoulli, cmbConditionalTwo, cmbMassTwo, cmbSuccessProb, cmbFailureProb]
  · have hw' : w ∈ Icc (0 : ℝ) 1 := ⟨(pow_nonneg q_pos.le _).trans hw.1, hw.2⟩
    have hopt : IsMaxOn (bernoulli 1 7) (Icc 0 1) (1/8) :=
      (bernoulli_max_iff hnzero ⟨by norm_num, by norm_num⟩).mpr (by norm_num)
    have hb : bernoulli 1 7 w ≤ bernoulli 1 7 (1/8) := hopt hw'
    by_cases h1 : n = 1
    · subst n
      have hBw : bernoulli 1 7 w < bernoulli 1 7 (1/8) := by
        refine lt_of_le_of_ne hb fun he ↦ hne ?_
        have hmax : IsMaxOn (bernoulli 1 7) (Icc 0 1) w := by
          intro v hv
          change bernoulli 1 7 v ≤ bernoulli 1 7 w
          rw [he]
          exact hopt hv
        have hEq := (bernoulli_max_iff hnzero hw').mp hmax
        exact Prod.ext rfl (hEq.trans hf)
      exact mul_lt_mul_of_pos_right hBw (pow_pos (hpos 1) 7)
    · have h' : cmbConditionalTwo n < cmbConditionalTwo 1 := by
        obtain ⟨j, rfl⟩ := Nat.exists_eq_succ_of_ne_zero hn
        exact (cmb_tail j (by omega)).trans_le (cmb_tail_le j (by omega))
      exact mul_lt_mul_of_le_of_lt_of_nonneg_of_pos hb
        (pow_lt_pow_left₀ h' (hpos n).le (by decide : (7 : ℕ) ≠ 0))
        (pow_pos (hpos n) 7).le (by norm_num [bernoulli])

end

end UnivariateML.TruncZero

end
