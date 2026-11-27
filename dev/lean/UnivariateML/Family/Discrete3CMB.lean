module

public import UnivariateML.Family.CMP
public import UnivariateML.Family.Discrete
public import UnivariateML.Generic.ExpFamily
import Mathlib.Tactic

set_option linter.style.header false

/-! Fixed-size Conway–Maxwell–binomial likelihood.
Supporting lines to the log-binomial statistic expose the fixed-design
faces. The scalar mean map covers the interior fitted-probability chart
at held dispersion. The capped comparison between different sizes is
separate. -/

@[expose] public section

namespace UnivariateML.Discrete3CMB

open Set Real Filter
open scoped Topology BigOperators

noncomputable section

def height (N : ℕ) (k : Fin (N+1)) : ℝ := log (N.choose k : ℝ)

def weight (N : ℕ) (η ν : ℝ) (k : Fin (N+1)) : ℝ :=
  exp (η * k.val + ν * height N k)

def rayMass {K : Type*} [Fintype K] (base slope : K → ℝ) (t : ℝ) (k : K) : ℝ :=
  exp (base k + t * slope k) / ∑ j, exp (base j + t * slope j)

/-- A normalized finite exponential ray converges to its weights conditioned
on the cells of maximal slope. -/
theorem exposed_mass {K : Type*} [Fintype K] [DecidableEq K]
    (base slope : K → ℝ) (F : Finset K) (k : K)
    (hk : k ∈ F) (hzero : ∀ j ∈ F, slope j = 0)
    (hneg : ∀ j ∉ F, slope j < 0) :
    Tendsto (fun t ↦ rayMass base slope t k) atTop
      (𝓝 (exp (base k) / ∑ j ∈ F, exp (base j))) := by
  classical
  have hlim (j : K) : Tendsto (fun t : ℝ ↦ exp (base j + t * slope j)) atTop
      (𝓝 (if j ∈ F then exp (base j) else 0)) := by
    by_cases hj : j ∈ F
    · simp [hj, hzero j hj]
    · have h : Tendsto (fun t : ℝ ↦ base j + t * slope j) atTop atBot :=
        tendsto_atBot_add_const_left atTop _ (tendsto_id.atTop_mul_const_of_neg (hneg j hj))
      simpa only [ite_eq_right hj, Function.comp_def] using tendsto_exp_atBot.comp h
  have hs := tendsto_finsetSum Finset.univ (fun j _ ↦ hlim j)
  have hF : 0 < ∑ j ∈ F, exp (base j) :=
    Finset.sum_pos (fun j _ ↦ exp_pos (base j)) ⟨k, hk⟩
  simp only [← Finset.sum_filter, Finset.filter_mem_eq_inter, Finset.univ_inter] at hs
  have hkLim : Tendsto (fun t : ℝ ↦ exp (base k + t * slope k)) atTop (𝓝 (exp (base k))) := by
    simpa [hk] using hlim k
  exact hkLim.div hs hF.ne'

/-- The face product is the least upper bound along the ray, and every
finite point on the ray lies strictly below it if a cell is excluded. -/
theorem exposed_supremum {K : Type*} [Fintype K] [DecidableEq K] {n : ℕ}
    (hn : 0 < n) (base slope : K → ℝ) (F : Finset K) (x : Fin n → K)
    (hx : ∀ i, x i ∈ F) (hzero : ∀ j ∈ F, slope j = 0)
    (hneg : ∀ j ∉ F, slope j < 0) (hout : ∃ j, j ∉ F) :
    let u := ∏ i, exp (base (x i)) / ∑ j ∈ F, exp (base j)
    IsLUB (range (fun t ↦ ∏ i, rayMass base slope t (x i))) u ∧
      ∀ t, (∏ i, rayMass base slope t (x i)) < u := by
  classical
  let u := ∏ i, exp (base (x i)) / ∑ j ∈ F, exp (base j)
  let f := fun t ↦ ∏ i, rayMass base slope t (x i)
  have lim : Tendsto f atTop (𝓝 u) :=
    tendsto_finsetProd _ (fun i _ ↦ exposed_mass base slope F (x i) (hx i) hzero hneg)
  let i₀ : Fin n := ⟨0, hn⟩
  have hF : 0 < ∑ j ∈ F, exp (base j) :=
    Finset.sum_pos (fun j _ ↦ exp_pos (base j)) ⟨x i₀, hx i₀⟩
  have hlt (t : ℝ) : f t < u := by
    obtain ⟨j, hj⟩ := hout
    let w := fun k ↦ exp (base k + t * slope k)
    have heq : ∑ k ∈ F, w k = ∑ k ∈ F, exp (base k) := by
      apply Finset.sum_congr rfl
      intro k hk
      simp [w, hzero k hk]
    have hsum : (∑ k ∈ F, w k) < ∑ k, w k := by
      rw [← Finset.sum_sdiff (Finset.subset_univ F)]
      have : 0 < ∑ k ∈ Finset.univ \ F, w k :=
        Finset.sum_pos (fun k _ ↦ exp_pos _) ⟨j, by simp [hj]⟩
      linarith
    rw [heq] at hsum
    apply Finset.prod_lt_prod_of_nonempty₀
    · intro i _
      exact div_pos (exp_pos _) (hF.trans hsum)
    · intro i _
      unfold rayMass
      simp only [hzero _ (hx i), mul_zero, add_zero]
      exact div_lt_div_of_pos_left (exp_pos _) hF hsum
    · exact ⟨i₀, Finset.mem_univ _⟩
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨t, rfl⟩
    exact (hlt t).le
  · intro b hb
    exact le_of_tendsto lim (Eventually.of_forall fun t ↦ hb ⟨t, rfl⟩)

/-- The fixed-size CMB likelihood in the interior probability chart. -/
def likelihood {N n : ℕ} (x : Fin n → Fin (N+1)) (η ν : ℝ) : ℝ :=
  ∏ i, weight N η ν (x i) / ∑ j, weight N η ν j

/-- At held interior probability, an endpoint-only sample has an unattained
supremum with normalized weights proportional to `1` and `exp (N*η)`. -/
theorem endpoint_supremum {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (η : ℝ) (x : Fin n → Fin (N+1))
    (hx : ∀ i, x i = 0 ∨ x i = Fin.last N) :
    let u := ∏ i, exp (η * (x i).val) / (1 + exp (η * N))
    IsLUB (range (likelihood x η)) u ∧ ∀ ν, likelihood x η ν < u := by
  classical
  let F : Finset (Fin (N+1)) := {0, Fin.last N}
  let b := fun k : Fin (N+1) ↦ η * (k.val : ℝ)
  have hmem (k : Fin (N+1)) : k ∈ F ↔ k = 0 ∨ k = Fin.last N := by simp [F]
  have hn0 : (0 : Fin (N+1)) ≠ Fin.last N := by
    intro h
    have he := congrArg Fin.val h
    change 0 = N at he
    omega
  have hzero : ∀ k ∈ F, -height N k = 0 := by
    intro k hk
    rcases (hmem k).mp hk with rfl | rfl <;> simp [height]
  have hneg : ∀ k ∉ F, -height N k < 0 := by
    intro k hk
    have hchoose : 1 < N.choose k.val := by
      have hp := Nat.choose_pos (Nat.le_of_lt_succ k.isLt)
      by_contra! h
      apply hk
      apply (hmem k).mpr
      rcases Nat.choose_eq_one_iff.mp (show N.choose k.val = 1 by omega) with hz | he
      · exact Or.inl (Fin.ext hz)
      · exact Or.inr (Fin.ext he.symm)
    exact neg_neg_of_pos (log_pos (by exact_mod_cast hchoose))
  let k₁ : Fin (N+1) := ⟨1, by omega⟩
  have hout : ∃ k, k ∉ F := by
    refine ⟨k₁, fun h ↦ ?_⟩
    rcases (hmem k₁).mp h with h | h
    · have he := congrArg Fin.val h
      change 1 = 0 at he
      omega
    · have he := congrArg Fin.val h
      change 1 = N at he
      omega
  have hden : (∑ j ∈ F, exp (b j)) = 1 + exp (η * N) := by
    simp [F, b, Finset.sum_insert, hn0]
  have h := exposed_supremum hn b (fun k ↦ -height N k) F x
    (fun i ↦ (hmem _).mpr (hx i)) hzero hneg hout
  dsimp only at h
  rw [hden] at h
  have he (t : ℝ) : (∏ i, rayMass b (fun k ↦ -height N k) t (x i)) =
      likelihood x η (-t) := by
    simp [likelihood, rayMass, b, weight, mul_neg, neg_mul]
  have hr : range (fun t ↦ ∏ i, rayMass b (fun k ↦ -height N k) t (x i)) =
      range (likelihood x η) := by
    ext y
    constructor
    · rintro ⟨t, rfl⟩
      exact ⟨-t, (he t).symm⟩
    · rintro ⟨ν, rfl⟩
      exact ⟨-ν, by simpa only [neg_neg] using he (-ν)⟩
  rw [hr] at h
  refine ⟨h.1, fun ν ↦ ?_⟩
  simpa only [he, neg_neg] using h.2 (-ν)

/-- The fixed-dispersion average log likelihood after dropping its data-only term. -/
def heldDispersionLogLik (N : ℕ) (ν m : ℝ) : ℝ → ℝ :=
  ExpFamily.logLik (fun k : Fin (N+1) ↦ (k.val : ℝ))
    (fun k ↦ exp (ν * height N k)) m

/-- For a positive fixed size and an interior empirical mean, the held-
dispersion CMB has one finite log-odds optimum. -/
theorem held_dispersion_interior {N : ℕ} (hN : 0 < N) (ν m : ℝ)
    (hm : m ∈ Ioo (0 : ℝ) N) :
    ∃! η, IsMaxOn (heldDispersionLogLik N ν m) univ η := by
  let T := fun k : Fin (N+1) ↦ (k.val : ℝ)
  have hN' : (0 : ℝ) < N := by exact_mod_cast hN
  have hull : convexHull ℝ (range T) = Icc (0 : ℝ) (N : ℝ) := by
    apply le_antisymm
    · apply convexHull_min (fun y hy ↦ ?_) (convex_Icc _ _)
      obtain ⟨k, rfl⟩ := hy
      exact ⟨Nat.cast_nonneg _, by change (k.val : ℝ) ≤ (N : ℝ); exact_mod_cast Nat.le_of_lt_succ k.isLt⟩
    · rw [← segment_eq_Icc hN'.le, ← convexHull_pair]
      apply convexHull_mono
      intro y hy
      rcases mem_insert_iff.mp hy with rfl | hy
      · exact ⟨0, by simp [T]⟩
      · obtain rfl := mem_singleton_iff.mp hy
        exact ⟨Fin.last N, rfl⟩
  have hspan : affineSpan ℝ (range T) = ⊤ := by
    rw [← affineSpan_convexHull, hull, ← (convex_Icc _ _).interior_nonempty_iff_affineSpan_eq_top]
    rw [interior_Icc, nonempty_Ioo]
    exact hN'
  apply (ExpFamily.mle_existsUnique_iff T (fun k ↦ exp_pos (ν * height N k)) hspan m).mpr
  apply interior_subset_intrinsicInterior
  rwa [hull, interior_Icc]

private theorem log_choose_factorial {N k : ℕ} (hk : k ≤ N) :
    log (N.choose k : ℝ) =
      CMP.logFactorial N - CMP.logFactorial k - CMP.logFactorial (N-k) := by
  have h := congrArg (fun z : ℕ ↦ log (z : ℝ)) (Nat.choose_mul_factorial_mul_factorial hk)
  push_cast at h
  rw [log_mul (by have := Nat.choose_pos hk; positivity) (by positivity),
    log_mul (by have := Nat.choose_pos hk; positivity) (by positivity)] at h
  unfold CMP.logFactorial
  linarith only [h]

private def edgeSlope (N c : ℕ) : ℝ := log ((N-c : ℕ) : ℝ)-log ((c : ℝ)+1)

private def edgeExposure (N c : ℕ) (k : Fin (N+1)) : ℝ :=
  height N k-log (N.choose c : ℝ)-edgeSlope N c*((k.val : ℝ)-c)

private theorem edgeExposure_sign {N c : ℕ} (hc : c < N) (k : Fin (N+1)) :
    edgeExposure N c k ≤ 0 ∧
      (edgeExposure N c k = 0 ↔ k.val = c ∨ k.val = c+1) := by
  let d := N-(c+1)
  have hk : k.val ≤ N := Nat.le_of_lt_succ k.isLt
  have hc' : c ≤ N := hc.le
  have hd : d+1 = N-c := by dsimp [d]; omega
  have hcast_d : (d : ℝ)+1 = ((N-c : ℕ) : ℝ) := by exact_mod_cast hd
  have hkdiff : ((N-k.val : ℕ) : ℝ)-(d : ℝ) = 1-((k.val : ℝ)-c) := by
    dsimp [d]
    rw [Nat.cast_sub hk, Nat.cast_sub (by omega : c+1 ≤ N), Nat.cast_add, Nat.cast_one]
    ring
  let L := fun j : ℕ ↦ CMP.logFactorial c+log ((c : ℝ)+1)*((j : ℝ)-c)
  let R := fun j : ℕ ↦ CMP.logFactorial d+log ((d : ℝ)+1)*((j : ℝ)-d)
  have hL : L k.val ≤ CMP.logFactorial k.val := (CMP.adjacent_supporting_line c k.val).1
  have hR : R (N-k.val) ≤ CMP.logFactorial (N-k.val) :=
    (CMP.adjacent_supporting_line d (N-k.val)).1
  have hRc : R (N-c) = CMP.logFactorial (N-c) :=
    (CMP.adjacent_supporting_line d (N-c)).2.mpr (Or.inr hd.symm)
  have hsum : edgeExposure N c k =
      (L k.val-CMP.logFactorial k.val)+(R (N-k.val)-CMP.logFactorial (N-k.val)) := by
    unfold edgeExposure height edgeSlope
    rw [log_choose_factorial hk, log_choose_factorial hc', ← hRc]
    dsimp [L, R]
    rw [hkdiff, hcast_d]
    have hs : (((N-c : ℕ) : ℝ)-(d : ℝ)) = 1 := by linarith only [hcast_d]
    rw [hs]
    ring
  refine ⟨by linarith only [hsum, hL, hR], ?_⟩
  constructor
  · intro h
    have he : L k.val = CMP.logFactorial k.val := by linarith only [hsum,h,hL,hR]
    exact (CMP.adjacent_supporting_line c k.val).2.mp he
  · intro h
    have heL : L k.val = CMP.logFactorial k.val :=
      (CMP.adjacent_supporting_line c k.val).2.mpr h
    have heR : R (N-k.val) = CMP.logFactorial (N-k.val) := by
      apply (CMP.adjacent_supporting_line d (N-k.val)).2.mpr
      dsimp [d]
      rcases h with h | h <;> omega
    rw [hsum, heL, heR, sub_self, sub_self, add_zero]

private theorem edgeExposure_neg {N c : ℕ} (hc : c < N) {k : Fin (N+1)}
    (hk : ¬(k.val = c ∨ k.val = c+1)) : edgeExposure N c k < 0 :=
  lt_of_le_of_ne (edgeExposure_sign hc k).1 (fun h ↦ hk ((edgeExposure_sign hc k).2.mp h))

private theorem edgeExposure_affine (N c : ℕ) (k : Fin (N+1)) :
    edgeExposure N c k = (-edgeSlope N c)*(k.val : ℝ)+1*height N k+
      (edgeSlope N c*c-log (N.choose c : ℝ)) := by
  unfold edgeExposure
  ring

private theorem rayMass_affine {N : ℕ} (η ν a b c t : ℝ) (k : Fin (N+1)) :
    rayMass (fun j : Fin (N+1) ↦ η*(j.val : ℝ)+ν*height N j)
        (fun j ↦ a*(j.val : ℝ)+b*height N j+c) t k =
      weight N (η+t*a) (ν+t*b) k / ∑ j, weight N (η+t*a) (ν+t*b) j := by
  have he (j : Fin (N+1)) : exp (η*(j.val : ℝ)+ν*height N j+
        t*(a*(j.val : ℝ)+b*height N j+c)) =
      exp (t*c)*weight N (η+t*a) (ν+t*b) j := by
    unfold weight
    rw [← exp_add]
    congr 1
    ring
  simp only [rayMass, he, ← Finset.mul_sum]
  exact mul_div_mul_left _ _ (exp_pos _).ne'

private theorem face_limit {N n : ℕ} (x : Fin n → Fin (N+1))
    (η ν a b c : ℝ) (F : Finset (Fin (N+1))) (hx : ∀ i, x i ∈ F)
    (hzero : ∀ j ∈ F, a*(j.val : ℝ)+b*height N j+c = 0)
    (hneg : ∀ j ∉ F, a*(j.val : ℝ)+b*height N j+c < 0) :
    Tendsto (fun t ↦ likelihood x (η+t*a) (ν+t*b)) atTop
      (𝓝 (∏ i, weight N η ν (x i)/∑ j ∈ F, weight N η ν j)) := by
  have h := tendsto_finsetProd (Finset.univ : Finset (Fin n)) (fun i _ ↦
    exposed_mass (fun j ↦ η*(j.val : ℝ)+ν*height N j)
      (fun j ↦ a*(j.val : ℝ)+b*height N j+c) F (x i) (hx i) hzero hneg)
  simpa only [rayMass_affine, likelihood, weight] using h

private theorem weight_sum_pos (N : ℕ) (η ν : ℝ) :
    0 < ∑ j, weight N η ν j :=
  Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨0, Finset.mem_univ _⟩

private theorem conditional_weight_lt_one {N : ℕ} (η ν : ℝ)
    (F : Finset (Fin (N+1))) (hout : ∃ j, j ∉ F) :
    (∑ j ∈ F, weight N η ν j)/(∑ j, weight N η ν j) < 1 := by
  apply (div_lt_one (weight_sum_pos _ _ _)).mpr
  obtain ⟨j, hj⟩ := hout
  have h : 0 < ∑ i ∈ Finset.univ \ F, weight N η ν i :=
    Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨j, by simp [hj]⟩
  rw [← Finset.sum_sdiff (Finset.subset_univ F)]
  linarith only [h]

private theorem face_from_path {N n : ℕ} {x : Fin n → Fin (N+1)} {u : ℝ}
    (hstrict : ∀ p : ℝ × ℝ, likelihood x p.1 p.2 < u)
    (path : ℝ → ℝ × ℝ)
    (hlim : Tendsto (fun t ↦ likelihood x (path t).1 (path t).2) atTop (𝓝 u)) :
    IsLUB (range (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2)) u ∧
      ∀ p : ℝ × ℝ, likelihood x p.1 p.2 < u := by
  refine ⟨⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨p, rfl⟩
    exact (hstrict p).le
  · intro z hz
    exact le_of_tendsto hlim (Eventually.of_forall fun t ↦ hz ⟨path t,rfl⟩)

/-- Number of observations at a given fixed-design count. -/
def multiplicity {N n : ℕ} (x : Fin n → Fin (N+1)) (k : Fin (N+1)) : ℕ :=
  (Finset.univ.filter (fun i ↦ x i = k)).card

/-- Saturated likelihood of a two-cell histogram. -/
def pairHeight (z o : ℕ) : ℝ :=
  ((z : ℝ)/(z+o))^z*((o : ℝ)/(z+o))^o

private theorem product_pair {K : Type*} [DecidableEq K] {n : ℕ}
    (x : Fin n → K) {l u : K} (hne : l ≠ u) (hx : ∀ i, x i = l ∨ x i = u) (f : K → ℝ) :
    (∏ i, f (x i)) = f l ^ (Finset.univ.filter (fun i ↦ x i = l)).card *
      f u ^ (Finset.univ.filter (fun i ↦ x i = u)).card := by
  have h (i : Fin n) : f (x i) = if x i = l then f l else f u := by
    rcases hx i with hi | hi
    · simp [hi]
    · simp [hi, hne.symm]
  have hf : (Finset.univ.filter (fun i ↦ ¬x i = l)) =
      Finset.univ.filter (fun i ↦ x i = u) := by
    ext i
    simp only [Finset.mem_filter, Finset.mem_univ, true_and]
    rcases hx i with hi | hi
    · simp [hi, hne]
    · simp [hi, hne.symm]
  simp_rw [h]
  rw [Finset.prod_ite, hf, Finset.prod_const, Finset.prod_const]

private theorem pair_height_bound {z o : ℕ} (hz : 0 < z) (ho : 0 < o)
    {p q : ℝ} (hp : 0 < p) (hq : 0 < q) (ht' : p+q < 1) :
    p^z*q^o < pairHeight z o := by
  let t := p+q
  let r := p/t
  have ht : 0 < t := add_pos hp hq
  have hr : r ∈ Ioo (0 : ℝ) 1 :=
    ⟨div_pos hp ht, (div_lt_one ht).mpr (lt_add_of_pos_right p hq)⟩
  have hz' : (0 : ℝ) < z := by exact_mod_cast hz
  have ho' : (0 : ℝ) < o := by exact_mod_cast ho
  have hzo := add_pos hz' ho'
  let r₀ := (z : ℝ)/(z+o)
  have hr₀ : r₀ ∈ Ioo (0 : ℝ) 1 :=
    ⟨div_pos hz' hzo, (div_lt_one hzo).mpr (by linarith only [ho'])⟩
  have he (s : ℝ) (hs : s ∈ Ioo (0 : ℝ) 1) :
      Discrete.Binomial.likelihood z o s = s^z*(1-s)^o := by
    unfold Discrete.Binomial.likelihood
    rw [ite_eq_right (by
      rintro (⟨h, _⟩ | ⟨h, _⟩)
      · exact hs.1.ne' h
      · exact hs.2.ne h),
      exp_add, exp_nat_mul, exp_nat_mul, exp_log hs.1, exp_log (sub_pos.mpr hs.2)]
  have hmax := (Discrete.Binomial.fixed_size_mle hz'.le ho'.le hzo r₀
    ⟨hr₀.1.le, hr₀.2.le⟩).mpr rfl
  have hval : Discrete.Binomial.likelihood z o r ≤ Discrete.Binomial.likelihood z o r₀ :=
    hmax ⟨hr.1.le, hr.2.le⟩
  rw [he r hr, he r₀ hr₀] at hval
  have hc : 1-r₀ = (o : ℝ)/(z+o) := by dsimp [r₀]; field_simp; ring
  have hpr : p = t*r := by dsimp [r]; field_simp [ht.ne']
  have hqr : q = t*(1-r) := by rw [mul_sub, mul_one, ← hpr]; dsimp [t]; ring
  calc
    p^z*q^o = t^(z+o)*(r^z*(1-r)^o) := by rw [hpr, hqr, mul_pow, mul_pow, pow_add]; ring
    _ < r^z*(1-r)^o := by
      have h := pow_lt_one₀ ht.le ht' (show z+o ≠ 0 by omega)
      simpa using mul_lt_mul_of_pos_right h (mul_pos (pow_pos hr.1 _) (pow_pos (sub_pos.mpr hr.2) _))
    _ ≤ pairHeight z o := by simpa only [pairHeight, hc] using hval

private theorem pair_face {N n : ℕ} (x : Fin n → Fin (N+1))
    {l u : Fin (N+1)} (hlu : l.val < u.val) (hx : ∀ i, x i = l ∨ x i = u)
    (hz : 0 < multiplicity x l) (ho : 0 < multiplicity x u)
    (hproper : ∃ k : Fin (N+1), k ≠ l ∧ k ≠ u)
    (a b c : ℝ)
    (hzero : ∀ k ∈ ({l,u} : Finset (Fin (N+1))), a*(k.val : ℝ)+b*height N k+c = 0)
    (hneg : ∀ k ∉ ({l,u} : Finset (Fin (N+1))), a*(k.val : ℝ)+b*height N k+c < 0) :
    IsLUB (range (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2))
        (pairHeight (multiplicity x l) (multiplicity x u)) ∧
      ∀ p : ℝ × ℝ, likelihood x p.1 p.2 < pairHeight (multiplicity x l) (multiplicity x u) := by
  let z := multiplicity x l
  let o := multiplicity x u
  let F : Finset (Fin (N+1)) := {l,u}
  have hne : l ≠ u := fun h ↦ hlu.ne (congrArg Fin.val h)
  have hp : ∃ k, k ∉ F := by
    obtain ⟨k, h₁, h₂⟩ := hproper
    exact ⟨k, by simp [F, h₁, h₂]⟩
  have hxF (i : Fin n) : x i ∈ F := by simpa [F] using hx i
  have hstrict (p : ℝ × ℝ) : likelihood x p.1 p.2 < pairHeight z o := by
    let f := fun k ↦ weight N p.1 p.2 k / ∑ j, weight N p.1 p.2 j
    have hf (k : Fin (N+1)) : 0 < f k := div_pos (exp_pos _) (weight_sum_pos _ _ _)
    have hsum : f l+f u < 1 := by
      have h := conditional_weight_lt_one p.1 p.2 F hp
      rw [show F = insert l ({u} : Finset (Fin (N+1))) from rfl,
        Finset.sum_insert (by simp [hne]), Finset.sum_singleton, add_div] at h
      exact h
    unfold likelihood
    rw [product_pair x hne hx f]
    exact pair_height_bound hz ho (hf l) (hf u) hsum
  have hz' : (0 : ℝ) < z := by exact_mod_cast hz
  have ho' : (0 : ℝ) < o := by exact_mod_cast ho
  have hden := add_pos hz' ho'
  let δ := (u.val : ℝ)-(l.val : ℝ)
  have hδ : 0 < δ := sub_pos.mpr (by exact_mod_cast hlu)
  let η := log ((o : ℝ)/z)/δ
  have hW (k : Fin (N+1)) : weight N η 0 k = exp (η*(k.val : ℝ)) := by simp [weight]
  have he : exp (η*(u.val : ℝ)) = exp (η*(l.val : ℝ))*((o : ℝ)/z) := by
    have heq : η*(u.val : ℝ) = η*(l.val : ℝ)+log ((o : ℝ)/z) := by
      dsimp [η, δ]
      have hd : (u.val : ℝ)-(l.val : ℝ) ≠ 0 := hδ.ne'
      field_simp [hd]
      ring
    rw [heq, exp_add, exp_log (div_pos ho' hz')]
  let D : ℝ := ∑ j ∈ F, weight N η 0 j
  have hD : D = exp (η*(l.val : ℝ))+exp (η*(u.val : ℝ)) := by
    simp [D, F, Finset.sum_insert, hne, hW]
  have hl : weight N η 0 l / D = (z : ℝ)/(z+o) := by
    rw [hW, hD, he]
    have hf : 0 < exp (η*(l.val : ℝ))+exp (η*(l.val : ℝ))*((o : ℝ)/z) := by positivity
    field_simp [hz'.ne', hden.ne', hf.ne']
  have hu : weight N η 0 u / D = (o : ℝ)/(z+o) := by
    rw [hW, hD, he]
    have hf : 0 < exp (η*(l.val : ℝ))+exp (η*(l.val : ℝ))*((o : ℝ)/z) := by positivity
    field_simp [hz'.ne', hden.ne', hf.ne']
  have hlim := face_limit x η 0 a b c F hxF hzero hneg
  have heq : (∏ i, weight N η 0 (x i)/D) = pairHeight z o := by
    rw [product_pair x hne hx (fun k ↦ weight N η 0 k / D), hl, hu]
    rfl
  change Tendsto (fun t ↦ likelihood x (η+t*a) (0+t*b)) atTop
    (𝓝 (∏ i, weight N η 0 (x i)/D)) at hlim
  rw [heq] at hlim
  exact face_from_path hstrict (fun t ↦ (η+t*a,0+t*b)) hlim

private theorem pair_proper {N : ℕ} (hN : 2 ≤ N) (l u : Fin (N+1)) :
    ∃ k : Fin (N+1), k ≠ l ∧ k ≠ u := by
  classical
  by_contra! h
  have he : (Finset.univ : Finset (Fin (N+1))) = {l,u} := by
    ext k
    have hk := h k
    simp only [Finset.mem_univ, Finset.mem_insert, Finset.mem_singleton, true_iff]
    exact or_iff_not_imp_left.mpr hk
  have hc : (Finset.univ : Finset (Fin (N+1))).card ≤ 2 := by
    rw [he]
    exact Finset.card_le_two
  simp at hc
  omega

/-- With size held and both regular coefficients free, a sample occupying
two adjacent cells has the strict saturated two-point supremum. -/
theorem adjacent_supremum {N n : ℕ} (hN : 2 ≤ N) (x : Fin n → Fin (N+1))
    {l u : Fin (N+1)} (hlu : u.val = l.val+1)
    (hx : ∀ i, x i = l ∨ x i = u)
    (hz : 0 < multiplicity x l) (ho : 0 < multiplicity x u) :
    IsLUB (range (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2))
        (pairHeight (multiplicity x l) (multiplicity x u)) ∧
      ∀ p : ℝ × ℝ, likelihood x p.1 p.2 < pairHeight (multiplicity x l) (multiplicity x u) := by
  have hc : l.val < N := by have := u.isLt; omega
  apply pair_face x (by omega : l.val < u.val) hx hz ho (pair_proper hN l u)
    (-edgeSlope N l.val) 1 (edgeSlope N l.val*l.val-log (N.choose l.val : ℝ))
  · intro k hk
    rw [← edgeExposure_affine]
    apply (edgeExposure_sign hc k).2.mpr
    rcases (show k = l ∨ k = u from by simpa using hk) with rfl | rfl
    · exact Or.inl rfl
    · exact Or.inr hlu
  · intro k hk
    rw [← edgeExposure_affine]
    apply edgeExposure_neg hc
    intro h
    apply hk
    rcases h with h | h
    · have h' : k = l := Fin.ext h
      simp [h']
    · have h' : k = u := Fin.ext (h.trans hlu.symm)
      simp [h']

private theorem height_endpoint (N : ℕ) :
    height N 0 = 0 ∧ height N (Fin.last N) = 0 := by simp [height]

private theorem height_pos_of_nonendpoint {N : ℕ} {k : Fin (N+1)}
    (hk₀ : k ≠ 0) (hkN : k ≠ Fin.last N) : 0 < height N k := by
  have hpos := Nat.choose_pos (Nat.le_of_lt_succ k.isLt)
  have hchoose : 1 < N.choose k.val := by
    by_contra! h
    rcases Nat.choose_eq_one_iff.mp (show N.choose k.val = 1 by omega) with h₀ | hN
    · exact hk₀ (Fin.ext h₀)
    · exact hkN (Fin.ext hN.symm)
  exact log_pos (by exact_mod_cast hchoose)

/-- With size held and both regular coefficients free, both occupied
endpoints give an unattained saturated endpoint law. -/
theorem endpoints_free_supremum {N n : ℕ} (hN : 2 ≤ N) (x : Fin n → Fin (N+1))
    (hx : ∀ i, x i = 0 ∨ x i = Fin.last N)
    (hz : 0 < multiplicity x 0) (ho : 0 < multiplicity x (Fin.last N)) :
    IsLUB (range (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2))
        (pairHeight (multiplicity x 0) (multiplicity x (Fin.last N))) ∧
      ∀ p : ℝ × ℝ, likelihood x p.1 p.2 <
        pairHeight (multiplicity x 0) (multiplicity x (Fin.last N)) := by
  apply pair_face x (show (0 : Fin (N+1)).val < (Fin.last N).val from by
    change 0 < N
    omega) hx hz ho (pair_proper hN 0 (Fin.last N)) 0 (-1) 0
  · intro k hk
    rcases (show k = 0 ∨ k = Fin.last N from by simpa using hk) with rfl | rfl
    · simp [(height_endpoint N).1]
    · simp [(height_endpoint N).2]
  · intro k hk
    have h₀ : k ≠ 0 := by rintro rfl; exact hk (by simp)
    have hN' : k ≠ Fin.last N := by rintro rfl; exact hk (by simp)
    simpa using neg_neg_of_pos (height_pos_of_nonendpoint h₀ hN')

private def pointSlope (N c : ℕ) : ℝ := -(edgeSlope N (c-1)+edgeSlope N c)/2

private def pointIntercept (N c : ℕ) : ℝ :=
  ((edgeSlope N (c-1))*((c-1 : ℕ) : ℝ)-log (N.choose (c-1) : ℝ)+
    edgeSlope N c*c-log (N.choose c : ℝ))/2

private theorem pointExposure_eq {N c : ℕ} {k : Fin (N+1)} :
    pointSlope N c*(k.val : ℝ)+height N k+pointIntercept N c =
      (edgeExposure N (c-1) k+edgeExposure N c k)/2 := by
  rw [edgeExposure_affine, edgeExposure_affine]
  unfold pointSlope pointIntercept
  ring

private theorem pointExposure_sign {N c : ℕ} (hc₀ : 0 < c) (hcN : c < N) (k : Fin (N+1)) :
    pointSlope N c*(k.val : ℝ)+height N k+pointIntercept N c ≤ 0 ∧
      (pointSlope N c*(k.val : ℝ)+height N k+pointIntercept N c = 0 ↔ k.val = c) := by
  have hc : c-1 < N := by omega
  obtain ⟨hle, hL⟩ := edgeExposure_sign hc k
  obtain ⟨hre, hR⟩ := edgeExposure_sign hcN k
  rw [pointExposure_eq]
  refine ⟨by linarith only [hle, hre], ?_⟩
  constructor
  · intro h
    have hl := hL.mp (show edgeExposure N (c-1) k = 0 by linarith only [hle, hre, h])
    have hr := hR.mp (show edgeExposure N c k = 0 by linarith only [hle, hre, h])
    omega
  · intro h
    rw [hL.mpr (Or.inr (by omega)), hR.mpr (Or.inl h)]
    norm_num

/-- An interior constant sample at held size has the strict, unattained
point-mass supremum when both regular coefficients are free. -/
theorem constant_interior_supremum {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N+1))
    {k : Fin (N+1)} (hk₀ : 0 < k.val) (hkN : k.val < N) (hx : ∀ i, x i = k) :
    IsLUB (range (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2)) 1 ∧
      ∀ p : ℝ × ℝ, likelihood x p.1 p.2 < 1 := by
  let F : Finset (Fin (N+1)) := {k}
  let a := pointSlope N k.val
  let c := pointIntercept N k.val
  have hstrict (p : ℝ × ℝ) : likelihood x p.1 p.2 < 1 := by
    have hp : ∃ j, j ∉ F := by
      refine ⟨0, ?_⟩
      intro h
      have he : (0 : Fin (N+1)) = k := by simpa [F] using h
      have ht := congrArg Fin.val he
      change 0 = k.val at ht
      omega
    have hmass := conditional_weight_lt_one p.1 p.2 F hp
    have hmass' : weight N p.1 p.2 k/(∑ j, weight N p.1 p.2 j) < 1 := by
      simpa [F] using hmass
    have hp₀ : 0 ≤ weight N p.1 p.2 k / (∑ j, weight N p.1 p.2 j) :=
      div_nonneg (show 0 ≤ weight N p.1 p.2 k from (exp_pos _).le)
        (weight_sum_pos N p.1 p.2).le
    unfold likelihood
    simp only [hx, Finset.prod_const, Finset.card_univ, Fintype.card_fin]
    exact pow_lt_one₀ hp₀ hmass' hn.ne'
  have hzero (j : Fin (N+1)) (hj : j ∈ F) :
      a*(j.val : ℝ)+1*height N j+c = 0 := by
    have he : j = k := by simpa [F] using hj
    simpa only [one_mul] using (pointExposure_sign hk₀ hkN j).2.mpr (congrArg Fin.val he)
  have hneg (j : Fin (N+1)) (hj : j ∉ F) :
      a*(j.val : ℝ)+1*height N j+c < 0 := by
    simpa only [one_mul] using lt_of_le_of_ne (pointExposure_sign hk₀ hkN j).1
      (fun he ↦ hj (by
        rw [show j = k from Fin.ext ((pointExposure_sign hk₀ hkN j).2.mp he)]
        simp [F]))
  have hlim := face_limit x 0 0 a 1 c F (fun i ↦ by simp [F, hx i]) hzero hneg
  have hlim' : Tendsto (fun t ↦ likelihood x (t*a) t) atTop (𝓝 1) := by
    simpa [F, hx, weight] using hlim
  exact face_from_path hstrict (fun t ↦ (t*a,t)) hlim'

private theorem held_height_face {N n : ℕ} (hn : 0 < n) (η H : ℝ)
    (x : Fin n → Fin (N+1)) (F : Finset (Fin (N+1)))
    (hx : ∀ i, x i ∈ F) (hout : ∃ k, k ∉ F)
    (hzero : ∀ k ∈ F, height N k = H)
    (hneg : ∀ k ∉ F, height N k < H) :
    let u := ∏ i, exp (η*(x i).val)/(∑ j ∈ F, exp (η*j.val))
    IsLUB (range (likelihood x η)) u ∧ ∀ ν, likelihood x η ν < u := by
  let base := fun k : Fin (N+1) ↦ η*(k.val : ℝ)+0*height N k
  let slope := fun k : Fin (N+1) ↦ (0 : ℝ)*(k.val : ℝ)+1*height N k+(-H)
  have h := exposed_supremum hn base slope F x hx
    (fun k hk ↦ by simp [slope, hzero k hk])
    (fun k hk ↦ by simpa [slope] using sub_neg.mpr (hneg k hk)) hout
  have he (t : ℝ) : (∏ i, rayMass base slope t (x i)) = likelihood x η t := by
    simp only [base, slope, rayMass_affine, mul_zero, add_zero, mul_one, zero_add, likelihood]
  have hr : range (fun t ↦ ∏ i, rayMass base slope t (x i)) = range (likelihood x η) := by
    rw [show (fun t ↦ ∏ i, rayMass base slope t (x i)) = likelihood x η from funext he]
  let v : ℝ := ∏ i, exp (base (x i))/(∑ j ∈ F, exp (base j))
  let u : ℝ := ∏ i, exp (η*(x i).val)/(∑ j ∈ F, exp (η*j.val))
  have hvu : v = u := by simp only [v, u, base, zero_mul, add_zero]
  have hs : IsLUB (range (fun t ↦ ∏ i, rayMass base slope t (x i))) v := h.1
  rw [hr, hvu] at hs
  refine ⟨hs, fun t ↦ ?_⟩
  have ht : (∏ i, rayMass base slope t (x i)) < v := h.2 t
  rwa [he, hvu] at ht

/-- At an odd held size and held interior probability, any sample on the
central pair has the exact unattained central two-point supremum. This
includes either singleton of the pair. -/
theorem odd_center_supremum {N n c : ℕ} (hN : N = 2*c+1) (hc : 0 < c)
    (hn : 0 < n) (η : ℝ) (x : Fin n → Fin (N+1))
    (hx : ∀ i, (x i).val = c ∨ (x i).val = c+1) :
    let u := ∏ i, exp (η*(x i).val)/(exp (η*c)+exp (η*(c+1 : ℕ)))
    IsLUB (range (likelihood x η)) u ∧ ∀ ν, likelihood x η ν < u := by
  let l : Fin (N+1) := ⟨c, by omega⟩
  let u : Fin (N+1) := ⟨c+1, by omega⟩
  let F : Finset (Fin (N+1)) := {l,u}
  have hne : l ≠ u := by
    intro h
    have hh := congrArg Fin.val h
    change c = c+1 at hh
    omega
  have hF (k : Fin (N+1)) : k ∈ F ↔ k.val = c ∨ k.val = c+1 := by
    have hl : k = l ↔ k.val = c := Fin.ext_iff
    have hu : k = u ↔ k.val = c+1 := Fin.ext_iff
    simp only [F, Finset.mem_insert, Finset.mem_singleton, hl, hu]
  have h₀ : edgeSlope N c = 0 := by
    have he : N-c = c+1 := by omega
    simp [edgeSlope, he, Nat.cast_add]
  have he (k : Fin (N+1)) : edgeExposure N c k = height N k-height N l := by
    simp [edgeExposure, h₀, l, height]
  have hproper : ∃ k, k ∉ F := by
    obtain ⟨k, h₁, h₂⟩ := pair_proper (show 2 ≤ N by omega) l u
    exact ⟨k, by simp [F, h₁, h₂]⟩
  have hzero (k : Fin (N+1)) (hk : k ∈ F) : height N k = height N l := by
    have h := (edgeExposure_sign (show c < N by omega) k).2.mpr ((hF k).mp hk)
    rw [he, sub_eq_zero] at h
    exact h
  have hneg (k : Fin (N+1)) (hk : k ∉ F) : height N k < height N l := by
    have h := edgeExposure_neg (show c < N by omega) (fun h ↦ hk ((hF k).mpr h))
    rw [he] at h
    exact sub_neg.mp h
  have h := held_height_face hn η (height N l) x F (fun i ↦ (hF _).mpr (hx i))
    hproper hzero hneg
  have hden : (∑ j ∈ F, exp (η*j.val)) = exp (η*c)+exp (η*(c+1 : ℕ)) := by
    simp [F, Finset.sum_insert, hne, l, u]
  dsimp only at h
  rwa [hden] at h

/-- At an even positive held size and held interior probability, the central
constant sample has the strict, unattained point-mass supremum. -/
theorem even_center_supremum {N n c : ℕ} (hN : N = 2*c) (hc : 0 < c)
    (hn : 0 < n) (η : ℝ) (x : Fin n → Fin (N+1)) (hx : ∀ i, (x i).val = c) :
    IsLUB (range (likelihood x η)) 1 ∧ ∀ ν, likelihood x η ν < 1 := by
  let l : Fin (N+1) := ⟨c, by omega⟩
  let F : Finset (Fin (N+1)) := {l}
  have hx' (i : Fin n) : x i = l := Fin.ext (hx i)
  have hproper : ∃ k, k ∉ F := by
    refine ⟨0, ?_⟩
    intro h
    have he : (0 : Fin (N+1)) = l := by simpa [F] using h
    have he' := congrArg Fin.val he
    change 0 = c at he'
    omega
  have hs : pointSlope N c = 0 := by
    have hL : N-(c-1) = c+1 := by omega
    have hR : N-c = c := by omega
    have he : (((c-1 : ℕ) : ℝ)+1) = c := by
      have h : c-1+1 = c := by omega
      exact_mod_cast h
    simp only [pointSlope, edgeSlope, hL, hR, he, Nat.cast_add, Nat.cast_one]
    ring
  have hi : pointIntercept N c = -height N l := by
    have h := (pointExposure_sign hc (show c < N by omega) l).2.mpr rfl
    rw [hs, zero_mul, zero_add] at h
    linarith only [h]
  have hzero (k : Fin (N+1)) (hk : k ∈ F) : height N k = height N l := by
    have he : k = l := by simpa [F] using hk
    rw [he]
  have hneg (k : Fin (N+1)) (hk : k ∉ F) : height N k < height N l := by
    obtain ⟨hle, hiff⟩ := pointExposure_sign hc (show c < N by omega) k
    have hlt : pointSlope N c*(k.val : ℝ)+height N k+pointIntercept N c < 0 := by
      apply lt_of_le_of_ne hle
      intro he
      apply hk
      have hkl : k = l := Fin.ext (hiff.mp he)
      simp [F, hkl]
    rw [hs, zero_mul, zero_add, hi] at hlt
    linarith only [hlt]
  have h := held_height_face hn η (height N l) x F (fun i ↦ by simp [F, hx' i])
    hproper hzero hneg
  simpa [F, hx', exp_pos] using h

/-- CMB in the closed probability chart. At the two endpoints the law is
the corresponding atom. -/
def closedLikelihood {N n : ℕ} (x : Fin n → Fin (N+1)) (p ν : ℝ) : ℝ :=
  if p = 0 then (if ∀ i, x i = 0 then 1 else 0)
  else if p = 1 then (if ∀ i, x i = Fin.last N then 1 else 0)
  else likelihood x (log (p/(1-p))) ν

/-- Size zero is the point mass at zero on the entire closed probability chart. -/
theorem size_zero_likelihood {n : ℕ} (x : Fin n → Fin 1) (p ν : ℝ) :
    closedLikelihood x p ν = 1 := by
  have hx (i : Fin n) : x i = 0 := Subsingleton.elim _ _
  have hl (η : ℝ) : likelihood x η ν = 1 := by
    have hd : (∑ j : Fin 1, weight 0 η ν j) = weight 0 η ν 0 :=
      Fin.sum_univ_one _
    change (∏ i, weight 0 η ν (x i)/(∑ j : Fin 1, weight 0 η ν j)) = 1
    simp only [hd, hx, div_self (show (weight 0 η ν 0) ≠ 0 from (exp_pos _).ne'),
      Finset.prod_const_one]
  simp [closedLikelihood, hx, hl]

private theorem binary_cell (k : Fin 2) : k = 0 ∨ k = 1 := by
  have h : k.val = 0 ∨ k.val = 1 := by have := k.isLt; omega
  rcases h with h | h
  · exact Or.inl (Fin.ext h)
  · exact Or.inr (Fin.ext h)

private theorem multiplicity_zero_iff {N n : ℕ} (x : Fin n → Fin (N+1))
    (k : Fin (N+1)) : multiplicity x k = 0 ↔ ∀ i, x i ≠ k := by
  simp [multiplicity, Finset.card_eq_zero, Finset.filter_eq_empty_iff]

private theorem binary_odds_likelihood {n : ℕ} (x : Fin n → Fin 2) {p : ℝ}
    (hp : p ∈ Ioo (0 : ℝ) 1) (ν : ℝ) :
    likelihood x (log (p/(1-p))) ν =
      p^multiplicity x 1 * (1-p)^multiplicity x 0 := by
  let η := log (p/(1-p))
  have h₁ := sub_pos.mpr hp.2
  have h0 : weight 1 η ν 0 = 1 := by simp [weight, height]
  have h1 : weight 1 η ν 1 = exp η := by
    simp [weight, height]
  have hden : (∑ k : Fin 2, weight 1 η ν k) = 1+exp η := by
    rw [Fin.sum_univ_two, h0, h1]
  let f := fun k : Fin 2 ↦ weight 1 η ν k/(∑ j : Fin 2, weight 1 η ν j)
  have hp0 : f 0 = 1-p := by
    dsimp [f]
    rw [h0, hden, show exp η = p/(1-p) from exp_log (div_pos hp.1 h₁)]
    have hd : 1+p/(1-p) ≠ 0 := (add_pos one_pos (div_pos hp.1 h₁)).ne'
    field_simp [h₁.ne', hd]
    ring
  have hp1 : f 1 = p := by
    dsimp [f]
    rw [h1, hden, show exp η = p/(1-p) from exp_log (div_pos hp.1 h₁)]
    have hd : 1+p/(1-p) ≠ 0 := (add_pos one_pos (div_pos hp.1 h₁)).ne'
    field_simp [h₁.ne', hd]
    ring
  change (∏ i, f (x i)) = _
  rw [product_pair x (show (0 : Fin 2) ≠ 1 by decide) (fun i ↦ binary_cell (x i)) f,
    hp0, hp1]
  exact mul_comm _ _

/-- At size one the CMB likelihood is Bernoulli for every dispersion,
including the compatible endpoint atoms. -/
theorem bernoulli_likelihood {n : ℕ} (x : Fin n → Fin 2) {p : ℝ}
    (hp : p ∈ Icc (0 : ℝ) 1) (ν : ℝ) :
    closedLikelihood x p ν = p^multiplicity x 1 * (1-p)^multiplicity x 0 := by
  rcases eq_or_lt_of_le hp.1 with h₀ | h₀
  · rw [← h₀]
    by_cases hx : ∀ i, x i = 0
    · have hz : multiplicity x 1 = 0 := (multiplicity_zero_iff x 1).mpr
        (fun i ↦ by rw [hx i]; decide)
      simp [closedLikelihood, hx, hz]
    · have hz : multiplicity x 1 ≠ 0 := by
        intro he
        apply hx
        intro i
        exact (binary_cell (x i)).resolve_right ((multiplicity_zero_iff x 1).mp he i)
      simp [closedLikelihood, hx, zero_pow hz]
  rcases eq_or_lt_of_le hp.2 with h₁ | h₁
  · rw [h₁]
    have he : (Fin.last 1 : Fin 2) = 1 := rfl
    by_cases hx : ∀ i, x i = 1
    · have hz : multiplicity x 0 = 0 := (multiplicity_zero_iff x 0).mpr
        (fun i ↦ by rw [hx i]; decide)
      simp [closedLikelihood, he, hx, hz]
    · have hz : multiplicity x 0 ≠ 0 := by
        intro hh
        apply hx
        intro i
        exact (binary_cell (x i)).resolve_left ((multiplicity_zero_iff x 0).mp hh i)
      simp [closedLikelihood, he, hx, zero_pow hz]
  · unfold closedLikelihood
    rw [ite_eq_right h₀.ne', ite_eq_right h₁.ne]
    exact binary_odds_likelihood x ⟨h₀,h₁⟩ ν

private theorem bernoulli_binomial_eq (z o : ℕ) {p : ℝ} (hp : p ∈ Icc (0 : ℝ) 1) :
    Discrete.Binomial.likelihood o z p = p^o*(1-p)^z := by
  rcases eq_or_lt_of_le hp.1 with h₀ | h₀
  · rw [← h₀]
    by_cases h : o = 0
    · simp [Discrete.Binomial.likelihood, h]
    · simp [Discrete.Binomial.likelihood, h, zero_pow h]
  rcases eq_or_lt_of_le hp.2 with h₁ | h₁
  · rw [h₁]
    by_cases h : z = 0
    · simp [Discrete.Binomial.likelihood, h]
    · simp [Discrete.Binomial.likelihood, h, zero_pow h]
  · unfold Discrete.Binomial.likelihood
    rw [ite_eq_right (by
      rintro (⟨h, _⟩ | ⟨h, _⟩)
      · exact h₀.ne' h
      · exact h₁.ne h),
      exp_add, exp_nat_mul, exp_nat_mul, exp_log h₀, exp_log (sub_pos.mpr h₁)]

/-- At size one the unique fitted probability is the empirical success
fraction, with arbitrary held dispersion. -/
theorem bernoulli_fit {n : ℕ} (hn : 0 < n) (x : Fin n → Fin 2)
    (ν : ℝ) {p : ℝ} (hp : p ∈ Icc (0 : ℝ) 1) :
    IsMaxOn (fun q ↦ closedLikelihood x q ν) (Icc 0 1) p ↔
      p = (multiplicity x 1 : ℝ)/(multiplicity x 1 + (multiplicity x 0 : ℝ)) := by
  let z := multiplicity x 0
  let o := multiplicity x 1
  have hsum : 0 < o+z := by
    let i : Fin n := ⟨0, hn⟩
    rcases binary_cell (x i) with hi | hi
    · have h : 0 < z := Finset.card_pos.mpr ⟨i, by simp [hi]⟩
      omega
    · have h : 0 < o := Finset.card_pos.mpr ⟨i, by simp [hi]⟩
      omega
  have he (q : ℝ) (hq : q ∈ Icc (0 : ℝ) 1) :
      closedLikelihood x q ν = Discrete.Binomial.likelihood o z q := by
    rw [bernoulli_likelihood x hq, bernoulli_binomial_eq z o hq]
  have hmax := Discrete.Binomial.fixed_size_mle (Nat.cast_nonneg o) (Nat.cast_nonneg z)
    (show (0 : ℝ) < (o : ℝ)+(z : ℝ) by exact_mod_cast hsum) p hp
  have htransfer : IsMaxOn (fun q ↦ closedLikelihood x q ν) (Icc 0 1) p ↔
      IsMaxOn (Discrete.Binomial.likelihood o z) (Icc 0 1) p := by
    constructor
    · intro h q hq
      have hh : closedLikelihood x q ν ≤ closedLikelihood x p ν := h hq
      rwa [he q hq, he p hp] at hh
    · intro h q hq
      change closedLikelihood x q ν ≤ closedLikelihood x p ν
      rw [he q hq, he p hp]
      exact h hq
  exact htransfer.trans hmax

private def designMoment (N d : ℕ) (ν η : ℝ) : ℝ :=
  ∑ k : Fin (N+1), (k.val : ℝ)^d*weight N η ν k

/-- Fixed-size CMB mean as a function of log odds at held dispersion. -/
@[no_expose] def modelMean (N : ℕ) (ν η : ℝ) : ℝ :=
  designMoment N 1 ν η / designMoment N 0 ν η

private theorem designMoment_zero_pos (N : ℕ) (ν η : ℝ) :
    0 < designMoment N 0 ν η := by
  unfold designMoment
  simp only [pow_zero, one_mul]
  exact weight_sum_pos _ _ _

private theorem designMoment_deriv (N d : ℕ) (ν η : ℝ) :
    HasDerivAt (designMoment N d ν) (designMoment N (d+1) ν η) η := by
  unfold designMoment
  apply HasDerivAt.fun_sum
  intro k _
  have h := (((hasDerivAt_id η).mul_const (k.val : ℝ)).add_const
    (ν*height N k)).exp.const_mul ((k.val : ℝ)^d)
  convert h using 1
  · rfl
  · simp only [weight, pow_succ, id_eq, one_mul]
    ring

/-- The mean increases strictly in log odds at every positive fixed size
and every real held dispersion. -/
theorem modelMean_strictMono {N : ℕ} (hN : 0 < N) (ν : ℝ) :
    StrictMono (modelMean N ν) := by
  apply strictMono_of_deriv_pos
  intro η
  let m := modelMean N ν η
  have hZ := designMoment_zero_pos N ν η
  have hM : 0 < designMoment N 1 ν η := by
    apply Finset.sum_pos' (fun k _ ↦ mul_nonneg (pow_nonneg (Nat.cast_nonneg _) _) (exp_pos _).le)
    refine ⟨Fin.last N, Finset.mem_univ _, ?_⟩
    have h : (0 : ℝ) < N := by exact_mod_cast hN
    simp only [Fin.val_last, pow_one]
    exact mul_pos h (exp_pos _)
  have hm : 0 < m := div_pos hM hZ
  let g := fun k : Fin (N+1) ↦ ((k.val : ℝ)-m)^2*weight N η ν k
  have hg : 0 < ∑ k, g k := by
    apply Finset.sum_pos' (fun k _ ↦ mul_nonneg (sq_nonneg _) (exp_pos _).le)
    refine ⟨0, Finset.mem_univ _, ?_⟩
    change 0 < (((0 : Fin (N+1)).val : ℝ)-m)^2*weight N η ν 0
    simp only [Fin.val_zero, Nat.cast_zero]
    exact mul_pos (by rw [zero_sub, neg_sq]; exact sq_pos_of_pos hm) (exp_pos _)
  have he : (∑ k, g k) =
      designMoment N 2 ν η-2*m*designMoment N 1 ν η+m^2*designMoment N 0 ν η := by
    unfold g designMoment
    simp_rw [sub_sq, add_mul, sub_mul, pow_one, pow_zero, one_mul,
      Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.mul_sum]
    congr 1
    congr 1
    rw [Finset.mul_sum]
    apply Finset.sum_congr rfl
    intro k _
    ring
  have hh := (designMoment_deriv N 1 ν η).div (designMoment_deriv N 0 ν η) hZ.ne'
  have hd : HasDerivAt (modelMean N ν)
      ((designMoment N 2 ν η*designMoment N 0 ν η-
        (designMoment N 1 ν η)^2)/(designMoment N 0 ν η)^2) η := by
    convert hh using 1
    · rfl
    · rw [pow_two]
  rw [hd.deriv]
  apply div_pos _ (sq_pos_of_pos hZ)
  have hc : designMoment N 0 ν η*(∑ k, g k) =
      designMoment N 2 ν η*designMoment N 0 ν η-(designMoment N 1 ν η)^2 := by
    rw [he]
    dsimp [m, modelMean]
    field_simp [hZ.ne']
    ring
  rw [← hc]
  exact mul_pos hZ hg

private def centeredMoment {N : ℕ} (d : ℕ) (ν η : ℝ) (c : Fin (N+1)) : ℝ :=
  ∑ k : Fin (N+1), (k.val : ℝ)^d*exp (η*((k.val : ℝ)-c.val)+ν*height N k)

private theorem designMoment_centered {N : ℕ} (d : ℕ) (ν η : ℝ) (c : Fin (N+1)) :
    designMoment N d ν η = exp (η*c.val)*centeredMoment d ν η c := by
  unfold designMoment centeredMoment
  rw [Finset.mul_sum]
  apply Finset.sum_congr rfl
  intro k _
  unfold weight
  rw [mul_comm (exp _), mul_assoc, ← exp_add]
  congr 2
  ring

private theorem centeredMoment_limit {N : ℕ} (d : ℕ) (ν : ℝ) (c : Fin (N+1))
    (hc : height N c = 0) {l : Filter ℝ}
    (ht : ∀ k : Fin (N+1), k ≠ c → Tendsto (fun η ↦ η*((k.val : ℝ)-c.val)) l atBot) :
    Tendsto (fun η ↦ centeredMoment d ν η c) l (𝓝 ((c.val : ℝ)^d)) := by
  classical
  have h (k : Fin (N+1)) :
      Tendsto (fun η ↦ (k.val : ℝ)^d*exp (η*((k.val : ℝ)-c.val)+ν*height N k)) l
        (𝓝 (if k = c then (c.val : ℝ)^d else 0)) := by
    by_cases he : k = c
    · subst k
      simpa only [hc, ite_eq_left rfl, ite_true, sub_self, mul_zero, zero_add, exp_zero, mul_one] using
        (tendsto_const_nhds (x := ((c.val : ℝ)^d)) : Tendsto (fun _ : ℝ ↦ (c.val : ℝ)^d) l (𝓝 ((c.val : ℝ)^d)))
    · have h' := tendsto_atBot_add_const_right l (ν*height N k) (ht k he)
      simpa only [Function.comp_def, ite_eq_right he, mul_zero] using
        (tendsto_exp_atBot.comp h').const_mul ((k.val : ℝ)^d)
  simpa only [centeredMoment, Finset.sum_ite_eq', Finset.mem_univ, ite_true] using
    tendsto_finsetSum (Finset.univ : Finset (Fin (N+1))) (fun k _ ↦ h k)

private theorem modelMean_endpoint {N : ℕ} (ν : ℝ) (c : Fin (N+1))
    (hc : height N c = 0) {l : Filter ℝ}
    (ht : ∀ k : Fin (N+1), k ≠ c → Tendsto (fun η ↦ η*((k.val : ℝ)-c.val)) l atBot) :
    Tendsto (modelMean N ν) l (𝓝 (c.val : ℝ)) := by
  have h := (centeredMoment_limit 1 ν c hc ht).div
    (centeredMoment_limit 0 ν c hc ht) (by norm_num)
  have he (η : ℝ) : centeredMoment 1 ν η c / centeredMoment 0 ν η c = modelMean N ν η := by
    unfold modelMean
    rw [designMoment_centered 1 ν η c, designMoment_centered 0 ν η c,
      mul_div_mul_left _ _ (exp_pos _).ne']
  simpa only [pow_one, pow_zero, div_one] using h.congr he

/-- At fixed size and dispersion, the mean tends to zero and to the size
at the two ends of the log-odds chart. -/
theorem modelMean_limits (N : ℕ) (ν : ℝ) :
    Tendsto (modelMean N ν) atBot (𝓝 0) ∧ Tendsto (modelMean N ν) atTop (𝓝 (N : ℝ)) := by
  constructor
  · suffices h : Tendsto (modelMean N ν) atBot (𝓝 (((0 : Fin (N+1)).val : ℝ))) by
      simpa only [Fin.val_zero, Nat.cast_zero] using h
    apply modelMean_endpoint ν (0 : Fin (N+1)) (height_endpoint N).1
    intro k hk
    have hp : 0 < k.val := Nat.pos_of_ne_zero (fun h ↦ hk (Fin.ext h))
    exact tendsto_id.atBot_mul_const (sub_pos.mpr (by exact_mod_cast hp))
  · apply modelMean_endpoint ν (Fin.last N) (height_endpoint N).2
    intro k hk
    have hp : k.val < N := lt_of_le_of_ne (Nat.le_of_lt_succ k.isLt)
      (fun h ↦ hk (Fin.ext h))
    exact tendsto_id.atTop_mul_const_of_neg (sub_neg.mpr (by exact_mod_cast hp))

end

end UnivariateML.Discrete3CMB

end
