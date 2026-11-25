# Balanced two-point generalized gamma has no finite MLE

TASK-339. For any two distinct positive observations occurring equally often, the all-free Prentice generalized-gamma mean log likelihood has a finite, unattained supremum. In standardized log coordinates it is $-1$. Both the power and Pareto paths approach it. With raw observations $a<b$, put $c=(\log a+\log b)/2$ and $d=(\log b-\log a)/2$. The raw mean log-likelihood supremum is $-1-\log d-c$. [Lean module](../../lean/UnivariateML/Family/GenGammaTwoPoint.lean)

## Source proof gap and scope

**The balanced-case paragraph leaves the finite $Q=0$ seam implicit.** Its displayed inequalities cover positive absolute log-power on the two reflected sides. The lognormal seam belongs to the finite Prentice domain. Lean bounds its entire likelihood surface by $-(1+\log(2\pi))/2<-1$, with equality in the weak bound at standardized location zero and scale one. With this additional case, the source's nonattainment conclusion holds across the full finite domain. [Source report](2026-11-25-gbshape-math-report.md#generalized-gamma)

The source applies $k<1/K(t)$ at a shape optimizer. Lean makes the large-power estimate pointwise by proving that the location-profiled objective is nonincreasing in $k$ above $1/K(t)$ and comparing with the cutoff. This also covers equality at the cutoff. The dossier's separate assertion that the likelihood optimized over log-power decreases strictly in gamma shape remains open in this formalization. The checked-out fitter already has a certified `TiedPowerPareto` shortcut for a literal distinct pair and maps that status to unattained nonexistence. Its explicit shortcut checks `x.size() == 2`. The theorem covers equal replication of any positive multiplicity. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/README.md) · [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/mle.hpp)

## Profile and proof

The standardized log observations are $z=-1,1$ with equal weights. Set $t=|Q|/\sigma$ and $k=Q^{-2}$ for $Q\ne0$. Their mean exponential on either orientation is $\cosh t$, so $K(t)=\log\cosh t$. The likelihood after maximizing over the standardized location $\mu$ is

$$
P(t,k)=\log t+k\log k-k-\log\Gamma(k)-kK(t).
$$

At an arbitrary location the mean log likelihood is $P(t,k)+k(u+1-e^u)$, with $u=K(t)-(Q/\sigma)\mu$. The location optimum is obtained by setting $u=0$.

- For $0<t\le 3$, the decreasing second derivative of $K$ gives $K(t)/t^2\ge K(3)/9$. The identity $K(t)=t-\log 2+\log(1+e^{-2t})$ bounds $K(3)/9$ below by $(3-\log 2)/9$. Lean uses the Mathlib bounds $\log 2<7/10$, $e<11/4$ and $\pi>3$, and rational arithmetic gives $K(3)/9>e/(4\pi)$. Stirling and $\log v\le v-1$ at $v=2kK(t)$ yield $P(t,k)\le\log t-\tfrac12\log(4\pi eK(t))<-1$. The real-shape Stirling bound follows from the integer bound, the Gamma recurrence, and convexity of $\log\Gamma$ at large positive arguments.
- For $t\ge3$, $K(t)\ge t-\log2>2$. For $k\le 1/K(t)$, the Gamma recurrence and the tangent to $\log\Gamma$ at one bound the excess over $-1$ by $\log(tk)+1-tk+k[\log(2k)+\gamma_E-1]$. The first three terms sum to at most zero, and the bracket is strictly negative by $2k<1$ and Mathlib's $\gamma_E<2/3$. Above the cutoff, the shape derivative is $1/k+\log k-\psi(k+1)-K(t)\le0$. The log-Gamma secant from $k$ to $k+1$ gives $\log k\le\psi(k+1)$.
- On the power and Pareto paths take $Q=\pm\sqrt t$, $\sigma=1/\sqrt t$ and $\mu=\pm K(t)/t$. Then $k=1/t$, the location loss vanishes, $kK(t)\to1$, and $P(t,1/t)\to-1$. The standardized locations approach the respective endpoints $1$ and $-1$, while $\sigma|Q|=1$. In raw log coordinates, the same paths have scale $d/\sqrt t$ and locations $c\pm dK(t)/t$, approaching $\log b$ for power and $\log a$ for Pareto. Both raw likelihood limits are $-1-\log d-c$, and the face exponent is $1/d=2/\log(b/a)$.

## Lean theorem statements

All names are in `UnivariateML.GenGammaTwoPoint`. The domain consists of real triples $(\mu,\sigma,Q)$ with $\sigma>0$. The equivalence with `Fin 2 × Fin m` enumerates $m$ occurrences of each observation in arbitrary order.

### `profile_face_limit`

The common location-profiled likelihood tends to the face value along gamma shape $1/t$.

```lean
theorem profile_face_limit :
    Tendsto (fun t : ℝ ↦ profile t (1 / t)) atTop (𝓝 (-1))
```

### `profile_lt_face`

The location profile is strictly below the face at every positive, finite power and gamma shape.

```lean
theorem profile_lt_face {t k : ℝ} (ht : 0 < t) (hk : 0 < k) :
    profile t k < -1
```

### `logLik_eq_profile`

For a nonzero Prentice shape, departure from the optimal location contributes the explicit exponential loss.

```lean
theorem logLik_eq_profile {μ σ Q : ℝ} (hσ : 0 < σ) (hQ : Q ≠ 0) :
    let t := |Q| / σ
    let k := 1 / Q ^ 2
    let u := cumulant t - Q / σ * μ
    logLik (μ, σ, Q) = profile t k + k * (u + 1 - exp u)
```

### `location_profile`

At any fixed nonzero Prentice shape and positive scale, the profile is the attained maximum over location.

```lean
theorem location_profile {σ Q : ℝ} (hσ : 0 < σ) (hQ : Q ≠ 0) :
    IsGreatest (Set.range (fun μ ↦ logLik (μ, σ, Q))) (profile (|Q| / σ) (1 / Q ^ 2))
```

### `logLik_lt_face`

Every finite parameter in the full Prentice domain, including the lognormal seam, is strictly below the common face.

```lean
theorem logLik_lt_face {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) : logLik p < -1
```

### `power_limit`

The positive-shape power path approaches the common likelihood supremum.

```lean
theorem power_limit : Tendsto (logLik ∘ powerPath) atTop (𝓝 (-1))
```

### `pareto_limit`

The negative-shape Pareto path approaches the same supremum.

```lean
theorem pareto_limit : Tendsto (logLik ∘ paretoPath) atTop (𝓝 (-1))
```

### `balanced_two_point_noMLE`

The standardized supremum is minus one, and no finite standardized parameter is a global maximizer.

```lean
theorem balanced_two_point_noMLE :
    sSup (logLik '' domain) = -1 ∧ ¬ ∃ p ∈ domain, IsMaxOn logLik domain p
```

### `standardize_pair`

Centering the two logs and dividing by their positive half-gap gives exactly minus one and one.

```lean
theorem standardize_pair {a b : ℝ} (ha : 0 < a) (hab : a < b) :
    let m := (log a + log b) / 2
    let d := (log b - log a) / 2
    (log a - m) / d = -1 ∧ (log b - m) / d = 1
```

### `power_coordinates`

The standardized power location tends to one, its signed shape tends to positive infinity, and the scale–absolute-shape product stays at one.

```lean
theorem power_coordinates :
    Tendsto (fun t ↦ (powerPath t).1) atTop (𝓝 1) ∧
    Tendsto (fun t ↦ (powerPath t).2.2) atTop atTop ∧
    ∀ t > 0, (powerPath t).2.1 * |(powerPath t).2.2| = 1
```

### `pareto_coordinates`

The standardized Pareto location tends to minus one, its signed shape tends to negative infinity, and the scale–absolute-shape product stays at one.

```lean
theorem pareto_coordinates :
    Tendsto (fun t ↦ (paretoPath t).1) atTop (𝓝 (-1)) ∧
    Tendsto (fun t ↦ (paretoPath t).2.2) atTop atBot ∧
    ∀ t > 0, (paretoPath t).2.1 * |(paretoPath t).2.2| = 1
```

### `balanced_mean_eq`

The standardized mean is unchanged by equal replication and permuting observations.

```lean
theorem balanced_mean_eq {n m : ℕ} (hm : 0 < m) (e : Fin n ≃ Fin 2 × Fin m)
    (p : ℝ × ℝ × ℝ) :
    (∑ i : Fin n, logDensity p.1 p.2.1 p.2.2
      (if (e i).1 = 0 then -1 else 1)) / n = logLik p
```

### `balanced_sample_transfer`

The original positive-data likelihood reduces to the standardized likelihood with the log-scale and log-Jacobian shift.

```lean
theorem balanced_sample_transfer {n m : ℕ} (hm : 0 < m) (e : Fin n ≃ Fin 2 × Fin m)
    {x : Fin n → ℝ} {a b : ℝ} (ha : 0 < a) (hab : a < b)
    (hx : ∀ i, x i = if (e i).1 = 0 then a else b)
    {μ σ Q : ℝ} (hσ : 0 < σ) :
    let c := (log a + log b) / 2
    let d := (log b - log a) / 2
    (∑ i : Fin n, (logDensity μ σ Q (log (x i)) - log (x i))) / n =
      logLik ((μ - c) / d, σ / d, Q) - log d - c
```

### `balanced_sample_noMLE`

Any equally replicated positive pair has the stated raw supremum, every finite parameter is strictly below it, and there is no finite MLE.

```lean
theorem balanced_sample_noMLE {n m : ℕ} (hm : 0 < m) (e : Fin n ≃ Fin 2 × Fin m)
    {x : Fin n → ℝ} {a b : ℝ} (ha : 0 < a) (hab : a < b)
    (hx : ∀ i, x i = if (e i).1 = 0 then a else b) :
    let c := (log a + log b) / 2
    let d := (log b - log a) / 2
    sSup (sampleLogLik x '' domain) = -1 - log d - c ∧
      (∀ p ∈ domain, sampleLogLik x p < -1 - log d - c) ∧
      ¬ ∃ p ∈ domain, IsMaxOn (sampleLogLik x) domain p
```

## Verification and effort

- Workstation, through `/tmp/umvl-cpu/run` at reduced priority. The final `lake env lean UnivariateML/Family/GenGammaTwoPoint.lean` exited zero with empty diagnostics.
- Workstation, through the same slot lock. The final `lake build UnivariateML +UnivariateML.Family.GenGammaTwoPoint` exited zero with `Build completed successfully (7416 jobs)`. Lake built the new module without warnings. It replayed pre-existing warnings from other modules. The explicit module target includes this proof in the build while the coordinator's root import is pending.
- Target 1 used the standardized density algebra, location profiling and balanced sample transfer. Target 2 took most of the proof work, especially lifting integer Stirling to real shapes, bounding the cutoff, and checking the two paths and the seam. Target 3 used direct comparison with the source proof, dossier and fitter. Total elapsed time to the final green build was about 22 minutes, including the initial library build and shared CPU-slot waits.
- All three targets are closed. The new module contains no proof placeholders or additional axioms.
