module

public import UnivariateML.Family.Discrete3CMB
public import Mathlib.Analysis.InnerProductSpace.PiL2
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Exposed likelihood rays of the fixed-size Conway–Maxwell–binomial law

The ray directions below are computed from the actual log-binomial
statistic. They expose the adjacent edge, an interior vertex, or the
endpoint edge. Base weights remain arbitrary finite CMB weights.

## Deviations

The hull theorem uses the empirical moment pair directly, and the closed
criterion treats held sizes at least two. The earlier size-zero and Bernoulli
theorems cover the lower sizes. The ray suprema are along the stated paths;
the earlier saturated face theorems compare with the whole finite chart.
-/

@[expose] public section

open Set Real Filter
open scoped Topology BigOperators
open UnivariateML.Discrete3CMB

namespace UnivariateML.FinishCMB

noncomputable section

/-- The adjacent increment of the fixed-size log-binomial statistic. -/
def edgeSlope (N c : ℕ) : ℝ := log ((N-c : ℕ) : ℝ)-log ((c : ℝ)+1)

/-- The negative average of the two adjacent increments at an interior cell. -/
def pointSlope (N c : ℕ) : ℝ := -(edgeSlope N (c-1)+edgeSlope N c)/2

/-- The finite fixed-design mass in the canonical coordinates. -/
def mass (N : ℕ) (η ν : ℝ) (k : Fin (N + 1)) : ℝ :=
  weight N η ν k / ∑ j, weight N η ν j

/-- The base mass conditioned on the indicated set of cells. -/
def conditionalMass {N : ℕ} (η ν : ℝ) (F : Finset (Fin (N + 1))) (k : Fin (N + 1)) : ℝ :=
  weight N η ν k / ∑ j ∈ F, weight N η ν j

private theorem log_choose_factorial {N k : ℕ} (hk : k ≤ N) :
    log (N.choose k : ℝ) =
      CMP.logFactorial N - CMP.logFactorial k - CMP.logFactorial (N-k) := by
  have h := congrArg (fun z : ℕ ↦ log (z : ℝ)) (Nat.choose_mul_factorial_mul_factorial hk)
  push_cast at h
  rw [log_mul (by have := Nat.choose_pos hk; positivity) (by positivity),
    log_mul (by have := Nat.choose_pos hk; positivity) (by positivity)] at h
  unfold CMP.logFactorial
  linarith only [h]

private def edgeExposure (N c : ℕ) (k : Fin (N + 1)) : ℝ :=
  height N k-log (N.choose c : ℝ)-edgeSlope N c*((k.val : ℝ)-c)

private theorem edgeExposure_sign {N c : ℕ} (hc : c < N) (k : Fin (N + 1)) :
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

private theorem edgeExposure_neg {N c : ℕ} (hc : c < N) {k : Fin (N + 1)}
    (hk : ¬(k.val = c ∨ k.val = c+1)) : edgeExposure N c k < 0 :=
  lt_of_le_of_ne (edgeExposure_sign hc k).1 (fun h ↦ hk ((edgeExposure_sign hc k).2.mp h))

private theorem edgeExposure_affine (N c : ℕ) (k : Fin (N + 1)) :
    edgeExposure N c k = (-edgeSlope N c)*(k.val : ℝ)+1*height N k+
      (edgeSlope N c*c-log (N.choose c : ℝ)) := by
  unfold edgeExposure
  ring

private theorem rayMass_affine {N : ℕ} (η ν a b c t : ℝ) (k : Fin (N + 1)) :
    rayMass (fun j : Fin (N + 1) ↦ η*(j.val : ℝ)+ν*height N j)
        (fun j ↦ a*(j.val : ℝ)+b*height N j+c) t k =
      weight N (η+t*a) (ν+t*b) k / ∑ j, weight N (η+t*a) (ν+t*b) j := by
  have he (j : Fin (N + 1)) : exp (η*(j.val : ℝ)+ν*height N j+
        t*(a*(j.val : ℝ)+b*height N j+c)) =
      exp (t*c)*weight N (η+t*a) (ν+t*b) j := by
    unfold weight
    rw [← exp_add]
    congr 1
    ring
  simp only [rayMass, he, ← Finset.mul_sum]
  exact mul_div_mul_left _ _ (exp_pos _).ne'

private theorem height_endpoint (N : ℕ) :
    height N 0 = 0 ∧ height N (Fin.last N) = 0 := by simp [height]

private theorem height_pos_of_nonendpoint {N : ℕ} {k : Fin (N + 1)}
    (hk₀ : k ≠ 0) (hkN : k ≠ Fin.last N) : 0 < height N k := by
  have hpos := Nat.choose_pos (Nat.le_of_lt_succ k.isLt)
  have hchoose : 1 < N.choose k.val := by
    by_contra! h
    rcases Nat.choose_eq_one_iff.mp (show N.choose k.val = 1 by omega) with h₀ | hN
    · exact hk₀ (Fin.ext h₀)
    · exact hkN (Fin.ext hN.symm)
  exact log_pos (by exact_mod_cast hchoose)

private def pointIntercept (N c : ℕ) : ℝ :=
  ((edgeSlope N (c-1))*((c-1 : ℕ) : ℝ)-log (N.choose (c-1) : ℝ)+
    edgeSlope N c*c-log (N.choose c : ℝ))/2

private theorem pointExposure_eq {N c : ℕ} {k : Fin (N + 1)} :
    pointSlope N c*(k.val : ℝ)+height N k+pointIntercept N c =
      (edgeExposure N (c-1) k+edgeExposure N c k)/2 := by
  rw [edgeExposure_affine, edgeExposure_affine]
  unfold pointSlope pointIntercept
  ring

private theorem pointExposure_sign {N c : ℕ} (hc₀ : 0 < c) (hcN : c < N) (k : Fin (N + 1)) :
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

private theorem exposed_family {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N + 1))
    (η ν a b c : ℝ) (F : Finset (Fin (N + 1))) (hx : ∀ i, x i ∈ F)
    (hzero : ∀ k ∈ F, a * (k.val : ℝ) + b * height N k + c = 0)
    (hneg : ∀ k ∉ F, a * (k.val : ℝ) + b * height N k + c < 0)
    (hout : ∃ k, k ∉ F) :
    (∀ k ∈ F, Tendsto (fun t ↦ mass N (η+t*a) (ν+t*b) k) atTop
      (𝓝 (conditionalMass η ν F k))) ∧
    let U := ∏ i, conditionalMass η ν F (x i)
    IsLUB (range (fun t ↦ likelihood x (η+t*a) (ν+t*b))) U ∧
      ∀ t, likelihood x (η+t*a) (ν+t*b) < U := by
  let f (k : Fin (N + 1)) := η*(k.val : ℝ)+ν*height N k
  let s (k : Fin (N + 1)) := a * (k.val : ℝ) + b * height N k + c
  have hr (t : ℝ) (k : Fin (N + 1)) :
      rayMass f s t k = mass N (η+t*a) (ν+t*b) k :=
    rayMass_affine η ν a b c t k
  refine ⟨fun k hk ↦ ?_,?_⟩
  · simpa only [hr, conditionalMass, weight, f] using exposed_mass f s F k hk hzero hneg
  · simpa only [hr, conditionalMass, weight, f, likelihood, mass] using
      exposed_supremum hn f s F x hx hzero hneg hout

private theorem outside_pair {N : ℕ} (hN : 2 ≤ N) (l u : Fin (N + 1)) :
    ∃ k : Fin (N + 1), k ∉ ({l,u} : Finset (Fin (N + 1))) := by
  by_contra! h
  let F : Finset (Fin (N + 1)) := {l,u}
  have he : (Finset.univ : Finset (Fin (N + 1))) = F :=
    Finset.Subset.antisymm (fun k _ ↦ h k) (Finset.subset_univ _)
  have hcard : F.card ≤ 2 := by
    dsimp [F]
    by_cases hl : l = u
    · simp [hl]
    · rw [Finset.card_insert_of_notMem (show l ∉ ({u} : Finset (Fin (N + 1))) by simpa using hl),
        Finset.card_singleton]
  rw [← he, Finset.card_univ, Fintype.card_fin] at hcard
  omega

/-- The adjacent log-binomial edge gives an actual CMB ray with increasing
dispersion and compensating log odds. Its likelihood supremum on the ray
is the product of the base masses conditioned on the two cells. -/
theorem adjacent_ray {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n) (x : Fin n → Fin (N + 1))
    {l u : Fin (N + 1)} (hlu : u.val = l.val + 1) (hx : ∀ i, x i = l ∨ x i = u)
    (η ν : ℝ) :
    let F : Finset (Fin (N + 1)) := {l,u}
    let a := edgeSlope N l.val
    (∀ k ∈ F, Tendsto (fun t ↦ mass N (η-t*a) (ν+t) k) atTop
      (𝓝 (conditionalMass η ν F k))) ∧
    let U := ∏ i, conditionalMass η ν F (x i)
    IsLUB (range (fun t ↦ likelihood x (η-t*a) (ν+t))) U ∧
      ∀ t, likelihood x (η-t*a) (ν+t) < U := by
  dsimp only
  let F : Finset (Fin (N + 1)) := {l,u}
  let a := edgeSlope N l.val
  let c := a*l.val-log (N.choose l.val : ℝ)
  have hc : l.val < N := by have := u.isLt; omega
  have he (k : Fin (N + 1)) :
      -a*(k.val : ℝ)+1*height N k+c = edgeExposure N l.val k :=
    (edgeExposure_affine N l.val k).symm
  have hv (k : Fin (N + 1)) : k ∈ F ↔ k.val = l.val ∨ k.val = l.val + 1 := by
    simp only [F, Finset.mem_insert, Finset.mem_singleton, Fin.ext_iff, hlu]
  have h := exposed_family hn x η ν (-a) 1 c F (fun i ↦ by simpa [F] using hx i)
    (fun k hk ↦ (he k).trans ((edgeExposure_sign hc k).2.mpr ((hv k).mp hk)))
    (fun k hk ↦ (he k).trans_lt (edgeExposure_neg hc (fun hh ↦ hk ((hv k).mpr hh))))
    (outside_pair hN l u)
  simpa only [mul_neg, ← sub_eq_add_neg, mul_one] using h

/-- An interior cell is exposed alone by the midpoint of the neighboring
edge directions. The resulting finite CMB ray has unattained supremum one
for a nonempty constant sample. -/
theorem point_ray {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N + 1))
    {k : Fin (N + 1)} (hk₀ : 0 < k.val) (hkN : k.val < N) (hx : ∀ i, x i = k)
    (η ν : ℝ) :
    let a := pointSlope N k.val
    Tendsto (fun t ↦ mass N (η+t*a) (ν+t) k) atTop (𝓝 1) ∧
    IsLUB (range (fun t ↦ likelihood x (η+t*a) (ν+t))) 1 ∧
      ∀ t, likelihood x (η+t*a) (ν+t) < 1 := by
  let F : Finset (Fin (N + 1)) := {k}
  let a := pointSlope N k.val
  let c := pointIntercept N k.val
  have hp (j : Fin (N + 1)) :
      a*(j.val : ℝ)+1*height N j+c = 0 ↔ j ∈ F := by
    simpa only [one_mul, F, Finset.mem_singleton, Fin.ext_iff] using
      (pointExposure_sign hk₀ hkN j).2
  have hn' (j : Fin (N + 1)) (hj : j ∉ F) : a*(j.val : ℝ)+1*height N j+c < 0 :=
    lt_of_le_of_ne (by simpa only [one_mul] using (pointExposure_sign hk₀ hkN j).1)
      (fun h ↦ hj ((hp j).mp h))
  have hx' (i : Fin n) : x i ∈ F := by simp [F,hx i]
  have hout : ∃ j, j ∉ F := by
    refine ⟨0,?_⟩
    simp only [F, Finset.mem_singleton, Fin.ext_iff, Fin.val_zero]
    exact hk₀.ne
  have h := exposed_family hn x η ν a 1 c F hx'
    (fun j hj ↦ (hp j).mpr hj) hn' hout
  have hw : weight N η ν k ≠ 0 := (exp_pos _).ne'
  have he : conditionalMass η ν F k = 1 := by simp [conditionalMass,F,hw]
  simpa only [mul_one,he,hx,Finset.prod_const_one] using
    And.intro (h.1 k (Finset.mem_singleton_self k)) h.2

/-- At fixed log odds, decreasing dispersion exposes exactly the two
endpoints. Along the actual ray the limiting masses are the base masses
conditioned on those endpoints. Their product is the strict
unattained least upper bound along the ray. -/
theorem endpoints_ray {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N + 1)) (hx : ∀ i, x i = 0 ∨ x i = Fin.last N) (η ν : ℝ) :
    let F : Finset (Fin (N + 1)) := {0,Fin.last N}
    (∀ k ∈ F, Tendsto (fun t ↦ mass N η (ν-t) k) atTop (𝓝 (conditionalMass η ν F k))) ∧
    let U := ∏ i, conditionalMass η ν F (x i)
    IsLUB (range (fun t ↦ likelihood x η (ν-t))) U ∧
      ∀ t, likelihood x η (ν-t) < U := by
  let F : Finset (Fin (N + 1)) := {0,Fin.last N}
  have hz (k : Fin (N + 1)) (hk : k ∈ F) : 0*(k.val : ℝ) + (-1)*height N k+0 = 0 := by
    rcases (show k = 0 ∨ k = Fin.last N from by simpa [F] using hk) with rfl | rfl
    · simp [(height_endpoint N).1]
    · simp [(height_endpoint N).2]
  have hn' (k : Fin (N + 1)) (hk : k ∉ F) : 0*(k.val : ℝ) + (-1)*height N k+0 < 0 := by
    have hl : k ≠ 0 := fun h ↦ hk (by simp [F,h])
    have hu : k ≠ Fin.last N := fun h ↦ hk (by simp [F,h])
    simpa using neg_neg_of_pos (height_pos_of_nonendpoint hl hu)
  simpa only [mul_zero,add_zero,mul_neg, mul_one, ← sub_eq_add_neg] using
    exposed_family hn x η ν 0 (-1) 0 F (fun i ↦ by simpa [F] using hx i)
      hz hn' (outside_pair hN 0 (Fin.last N))


open scoped RealInnerProductSpace

/-- Sufficient statistic of the fixed-size CMB exponential family. -/
def statistic (N : ℕ) (k : Fin (N + 1)) : EuclideanSpace ℝ (Fin 2) :=
  WithLp.toLp 2 ![(k.val : ℝ),height N k]

/-- The empirical moment vector for the two finite canonical coefficients. -/
def empirical {N n : ℕ} (x : Fin n → Fin (N + 1)) : EuclideanSpace ℝ (Fin 2) :=
  (n : ℝ)⁻¹ • ∑ i, statistic N (x i)

private theorem statistic_span {N : ℕ} (hN : 2 ≤ N) :
    affineSpan ℝ (range (statistic N)) = ⊤ := by
  let S := affineSpan ℝ (range (statistic N))
  let v : EuclideanSpace ℝ (Fin 2) := WithLp.toLp 2 ![(N : ℝ),0]
  let w : EuclideanSpace ℝ (Fin 2) := WithLp.toLp 2 ![1,log (N : ℝ)]
  have hzero : (0 : EuclideanSpace ℝ (Fin 2)) ∈ S := by
    apply mem_affineSpan ℝ
    refine ⟨0,?_⟩
    ext j
    fin_cases j <;> simp [statistic,(height_endpoint N).1]
  have hv : v ∈ S := by
    apply mem_affineSpan ℝ
    refine ⟨Fin.last N,?_⟩
    ext j
    fin_cases j <;> simp [v,statistic,(height_endpoint N).2]
  have hw : w ∈ S := by
    apply mem_affineSpan ℝ
    refine ⟨⟨1,by omega⟩,?_⟩
    ext j
    fin_cases j <;> simp [w,statistic,height,Nat.choose_one_right]
  have hV : v ∈ S.direction := by
    simpa using AffineSubspace.vsub_mem_direction hv hzero
  have hW : w ∈ S.direction := by
    simpa using AffineSubspace.vsub_mem_direction hw hzero
  apply top_unique
  intro q _
  have hN' : (0 : ℝ) < N := by exact_mod_cast (show 0 < N by omega)
  have hlog : 0 < log (N : ℝ) := log_pos (by exact_mod_cast (show 1 < N by omega))
  let β := q 1 / log (N : ℝ)
  let α := (q 0-β)/(N : ℝ)
  have he : α • v + β • w = q := by
    ext j
    fin_cases j
    · change α*(N : ℝ)+β*1 = q 0
      dsimp [α]
      rw [div_mul_cancel₀ _ hN'.ne']
      ring
    · change α*0+β*log (N : ℝ) = q 1
      dsimp [β]
      rw [mul_zero,zero_add,div_mul_cancel₀ _ hlog.ne']
  have hq := S.direction.add_mem (S.direction.smul_mem α hV) (S.direction.smul_mem β hW)
  rw [he] at hq
  simpa using AffineSubspace.vadd_mem_of_mem_direction hq hzero

private theorem statistic_inner {N : ℕ} (θ : EuclideanSpace ℝ (Fin 2))
    (k : Fin (N + 1)) :
    ⟪θ, statistic N k⟫ = θ 0*(k.val : ℝ)+θ 1*height N k := by
  simp [statistic,PiLp.inner_apply,Fin.sum_univ_two,mul_comm]

private theorem likelihood_log {N n : ℕ} (x : Fin n → Fin (N + 1))
    (hn : 0 < n) (θ : EuclideanSpace ℝ (Fin 2)) :
    0 < likelihood x (θ 0) (θ 1) ∧
      log (likelihood x (θ 0) (θ 1)) =
        n * ExpFamily.logLik (statistic N) (fun _ ↦ 1) (empirical x) θ := by
  let Z := ∑ k, weight N (θ 0) (θ 1) k
  have hz : 0 < Z := Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨0,Finset.mem_univ _⟩
  refine ⟨Finset.prod_pos (fun _ _ ↦ div_pos (exp_pos _) hz),?_⟩
  unfold likelihood
  change log (∏ i, weight N (θ 0) (θ 1) (x i)/Z) = _
  rw [log_prod (fun i _ ↦ div_ne_zero (show weight N (θ 0) (θ 1) (x i) ≠ 0 from
    (exp_pos _).ne') hz.ne')]
  have he (i : Fin n) :
      log (weight N (θ 0) (θ 1) (x i)/Z) = ⟪θ,statistic N (x i)⟫-log Z := by
    rw [log_div (show weight N (θ 0) (θ 1) (x i) ≠ 0 from (exp_pos _).ne') hz.ne',
      weight,log_exp,statistic_inner]
  change (∑ i, log (weight N (θ 0) (θ 1) (x i)/Z)) = _
  simp only [he,Finset.sum_sub_distrib,Finset.sum_const,Finset.card_univ,Fintype.card_fin,
    nsmul_eq_mul,ExpFamily.logLik,ExpFamily.logPartition,one_mul,statistic_inner]
  have hpart : (∑ k, exp (θ 0*(k.val : ℝ)+θ 1*height N k)) = Z := rfl
  rw [hpart]
  have hv : ⟪θ, empirical x⟫ = (n : ℝ)⁻¹ * ∑ i, ⟪θ,statistic N (x i)⟫ := by
    simp [empirical,real_inner_smul_right,inner_sum]
  rw [hv]
  simp_rw [statistic_inner]
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  field_simp [hn']

private theorem canonical_max_iff {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N + 1))
    (θ : EuclideanSpace ℝ (Fin 2)) :
    IsMaxOn (fun θ ↦ likelihood x (θ 0) (θ 1)) univ θ ↔
      IsMaxOn (ExpFamily.logLik (statistic N) (fun _ ↦ 1) (empirical x)) univ θ := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  simp only [isMaxOn_iff]
  apply forall_congr'
  intro a
  apply imp_congr_right
  intro _
  rw [← log_le_log_iff (likelihood_log x hn a).1 (likelihood_log x hn θ).1,
    (likelihood_log x hn a).2, (likelihood_log x hn θ).2, mul_le_mul_iff_of_pos_left hn']

/-- At sizes at least two, a unique finite canonical fit exists exactly
when the empirical moment lies in the interior of the fixed-size hull. -/
theorem held_size_open_hull {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N + 1)) :
    (∃! θ : EuclideanSpace ℝ (Fin 2),
      IsMaxOn (fun θ ↦ likelihood x (θ 0) (θ 1)) univ θ) ↔
      empirical x ∈ interior (convexHull ℝ (range (statistic N))) := by
  simp_rw [canonical_max_iff hn x]
  have hs := statistic_span hN
  rw [ExpFamily.mle_existsUnique_iff _ (fun _ ↦ zero_lt_one) hs]
  rw [← affineSpan_convexHull] at hs
  constructor
  · rintro ⟨a,ha,haeq⟩
    rw [← haeq]
    let K := convexHull ℝ (range (statistic N))
    have ho : IsOpen (affineSpan ℝ K : Set (EuclideanSpace ℝ (Fin 2))) := by
      rw [hs, AffineSubspace.top_coe]
      exact isOpen_univ
    let coe : affineSpan ℝ K → EuclideanSpace ℝ (Fin 2) := Subtype.val
    refine interior_maximal (s := K) (t := coe '' interior (coe ⁻¹' K)) ?_ ?_ ?_
    · rintro _ ⟨b,hb,rfl⟩
      exact (interior_subset hb : b ∈ coe ⁻¹' K)
    · exact ho.isOpenMap_subtype_val _ isOpen_interior
    · exact mem_image_of_mem _ ha
  · exact fun hm ↦ interior_subset_intrinsicInterior hm

private def probability (η : ℝ) : ℝ := exp η/(1+exp η)

private theorem probability_interior (η : ℝ) : probability η ∈ Ioo (0 : ℝ) 1 :=
  ⟨div_pos (exp_pos _) (add_pos one_pos (exp_pos _)),
    (div_lt_one (show 0 < 1+exp η by positivity)).mpr (by linarith)⟩

private theorem canonical_probability (η : ℝ) :
    log (probability η / (1-probability η)) = η := by
  have hp : 0 < 1+exp η := by positivity
  unfold probability
  rw [show exp η/(1+exp η)/(1-exp η/(1+exp η)) = exp η by field_simp [hp.ne']; ring]
  exact log_exp _

private theorem finite_likelihood_bounds {N n : ℕ} (hN : 0 < N) (hn : 0 < n)
    (x : Fin n → Fin (N + 1)) (η ν : ℝ) : 0 < likelihood x η ν ∧ likelihood x η ν < 1 := by
  let : Nonempty (Fin n) := ⟨⟨0,hn⟩⟩
  let Z := ∑ j, weight N η ν j
  have hz : 0 < Z := Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨0,Finset.mem_univ _⟩
  have hW (j : Fin (N + 1)) : 0 < weight N η ν j := exp_pos _
  refine ⟨Finset.prod_pos (fun i _ ↦ div_pos (hW (x i)) hz),?_⟩
  have hs (j : Fin (N + 1)) : weight N η ν j < Z := by
    have hn' : Nontrivial (Fin (N + 1)) := ⟨⟨0,Fin.last N,fun h ↦ by
      have he := congrArg Fin.val h
      change 0 = N at he
      omega⟩⟩
    obtain ⟨k,hk⟩ := exists_ne j
    unfold Z
    rw [← Finset.sum_erase_add _ _ (Finset.mem_univ j)]
    have hpos : 0 < ∑ a ∈ Finset.univ.erase j, weight N η ν a :=
      Finset.sum_pos (fun a _ ↦ hW a) ⟨k,Finset.mem_erase.mpr ⟨hk,Finset.mem_univ _⟩⟩
    linarith
  unfold likelihood
  simpa only [Finset.prod_const_one] using Finset.prod_lt_prod_of_nonempty₀
    (fun i _ ↦ div_pos (hW (x i)) hz)
    (fun i _ ↦ (div_lt_one hz).mpr (hs (x i))) (Finset.univ_nonempty (α := Fin n))

private theorem closed_at_interior {N n : ℕ} (x : Fin n → Fin (N + 1)) {p : ℝ}
    (hp : p ∈ Ioo (0 : ℝ) 1) (ν : ℝ) :
    closedLikelihood x p ν = likelihood x (log (p/(1-p))) ν := by
  simp [closedLikelihood,hp.1.ne',hp.2.ne]

/-- At held size at least two, the closed probability domain attains a
maximum exactly at an endpoint-constant sample or an interior moment pair. -/
theorem held_size_closed_criterion {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N + 1)) :
    (∃ p ∈ Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ),
      IsMaxOn (fun p : ℝ × ℝ ↦ closedLikelihood x p.1 p.2) (Icc 0 1 ×ˢ univ) p) ↔
      (∀ i, x i = 0) ∨ (∀ i, x i = Fin.last N) ∨
        empirical x ∈ interior (convexHull ℝ (range (statistic N))) := by
  have hN' : 0 < N := by omega
  let D := Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ)
  let L (p : ℝ × ℝ) := closedLikelihood x p.1 p.2
  have hfinite (θ : EuclideanSpace ℝ (Fin 2)) :
      L (probability (θ 0),θ 1) = likelihood x (θ 0) (θ 1) := by
    rw [show L (probability (θ 0),θ 1) = closedLikelihood x (probability (θ 0)) (θ 1) from rfl,
      closed_at_interior x (probability_interior _), canonical_probability]
  have hcases (p : ℝ) (hp : p ∈ Icc (0 : ℝ) 1) :
      p = 0 ∨ p = 1 ∨ p ∈ Ioo (0 : ℝ) 1 := by
    rcases eq_or_lt_of_le hp.1 with h|h
    · exact Or.inl h.symm
    · rcases eq_or_lt_of_le hp.2 with h'|h'
      · exact Or.inr (Or.inl h')
      · exact Or.inr (Or.inr ⟨h,h'⟩)
  have hupper : ∀ p ∈ D, L p ≤ 1 := by
    rintro ⟨p,ν⟩ hp
    rcases hcases p hp.1 with rfl|rfl|hi
    · by_cases hz : ∀ i, x i = 0 <;> simp [L,closedLikelihood,hz]
    · by_cases hl : ∀ i, x i = Fin.last N <;> simp [L,closedLikelihood,hl]
    · change closedLikelihood x p ν ≤ 1
      rw [closed_at_interior x hi]
      exact (finite_likelihood_bounds hN' hn x _ ν).2.le
  constructor
  · rintro ⟨⟨p,ν⟩,hp,hm⟩
    by_cases hzero : ∀ i, x i = 0
    · exact Or.inl hzero
    by_cases hlast : ∀ i, x i = Fin.last N
    · exact Or.inr (Or.inl hlast)
    have hbad₀ : L (0,ν) = 0 := by simp [L,closedLikelihood,hzero]
    have hbad₁ : L (1,ν) = 0 := by simp [L,closedLikelihood,hlast]
    let θ₀ : EuclideanSpace ℝ (Fin 2) := 0
    have ht : (probability (θ₀ 0), θ₀ 1) ∈ D :=
      ⟨Ioo_subset_Icc_self (probability_interior _),mem_univ _⟩
    have hpos : 0 < L (p,ν) := by
      have h := (finite_likelihood_bounds hN' hn x (θ₀ 0) (θ₀ 1)).1
      rw [← hfinite θ₀] at h
      exact h.trans_le (isMaxOn_iff.mp hm _ ht)
    have hpi : p ∈ Ioo (0 : ℝ) 1 := (hcases p hp.1).resolve_left
      (fun h ↦ by subst p; rw [hbad₀] at hpos; exact (lt_irrefl _ hpos))
      |>.resolve_left (fun h ↦ by subst p; rw [hbad₁] at hpos; exact (lt_irrefl _ hpos))
    let θ : EuclideanSpace ℝ (Fin 2) := WithLp.toLp 2 ![log (p/(1-p)),ν]
    have he : L (p,ν) = likelihood x (θ 0) (θ 1) := closed_at_interior x hpi ν
    have hmax : IsMaxOn (fun a : EuclideanSpace ℝ (Fin 2) ↦ likelihood x (a 0) (a 1)) univ θ :=
      fun a _ ↦ (hfinite a).symm.trans_le ((isMaxOn_iff.mp hm _
        ⟨Ioo_subset_Icc_self (probability_interior _),mem_univ _⟩).trans_eq he)
    apply Or.inr ∘ Or.inr
    apply (held_size_open_hull hN hn x).mp
    exact ⟨θ,hmax,fun a ha ↦ ExpFamily.unique_mle _ (fun _ ↦ zero_lt_one) (statistic_span hN)
      ((canonical_max_iff hn x a).mp ha) ((canonical_max_iff hn x θ).mp hmax)⟩
  · rintro (hzero|hlast|hull)
    · exact ⟨(0,0),⟨by norm_num,mem_univ _⟩,fun a ha ↦ (hupper a ha).trans_eq
        (by simp [closedLikelihood,hzero])⟩
    · exact ⟨(1,0),⟨by norm_num,mem_univ _⟩,fun a ha ↦ (hupper a ha).trans_eq
        (by simp [closedLikelihood,hlast])⟩
    · by_cases hzero : ∀ i, x i = 0
      · exact ⟨(0,0),⟨by norm_num,mem_univ _⟩,fun a ha ↦ (hupper a ha).trans_eq
          (by simp [closedLikelihood,hzero])⟩
      by_cases hlast : ∀ i, x i = Fin.last N
      · exact ⟨(1,0),⟨by norm_num,mem_univ _⟩,fun a ha ↦ (hupper a ha).trans_eq
          (by simp [closedLikelihood,hlast])⟩
      obtain ⟨θ,hθ,_⟩ := (held_size_open_hull hN hn x).mpr hull
      refine ⟨(probability (θ 0),θ 1),
        ⟨Ioo_subset_Icc_self (probability_interior _),mem_univ _⟩,?_⟩
      rintro ⟨p,ν⟩ hp
      change L (p,ν) ≤ L (probability (θ 0),θ 1)
      rw [hfinite]
      rcases hcases p hp.1 with rfl|rfl|hi
      · simpa [L,closedLikelihood,hzero] using (finite_likelihood_bounds hN' hn x (θ 0) (θ 1)).1.le
      · simpa [L,closedLikelihood,hlast] using (finite_likelihood_bounds hN' hn x (θ 0) (θ 1)).1.le
      · change closedLikelihood x p ν ≤ _
        rw [closed_at_interior x hi]
        exact isMaxOn_iff.mp hθ (WithLp.toLp 2 ![log (p/(1-p)),ν]) (mem_univ _)

end

end UnivariateML.FinishCMB
