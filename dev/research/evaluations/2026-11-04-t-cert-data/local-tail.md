# A sharper terminal interval from a normal nuisance ball

This improves the constants in [signed-tail.md](signed-tail.md). Standardize
the exact data to normal mean zero and MLE scale one. Let
`theta=(mu,t=log(s))`, and use the Euclidean ball of radius r at zero.
For each `xi in [0,h]`, the negative likelihood Hessian has entries

```
H_mm = (1+xi) exp(-2t) sum (1-xi z^2)/(1+xi z^2)^2,
H_mt = 2(1+xi) exp(-t) sum z/(1+xi z^2)^2,
H_tt = 2(1+xi) sum z^2/(1+xi z^2)^2,
z=(x-mu) exp(-t).
```

The script encloses z on the larger rectangle `|mu|,|t|<=r`. It bounds
each denominator between 1 and `(1+h max(z^2))^2`, requires
`h max(z^2)<1`, encloses the signed sum in H_mt before taking its absolute
value, and uses the two Gershgorin lower bounds to obtain `H>=lambda I`.
These elementary inequalities bound all intermediate shapes and nuisances;
they do not rely on evaluating a Hessian at one numerical root.

At theta zero, the total negative score vanishes at xi zero. Its norm for
positive xi is at most `xi G`, where

```
G = hypot(sum |x(1-x^2)|, sum |x^2(1-x^2)|).
```

This follows by subtracting the exact normal score from the rational t
score and using denominators `1+xi x^2>=1`. If `h G < lambda r`, radial
outward derivatives of the negative likelihood on the ball boundary are
strictly positive. Its minimum over the closed ball is interior and solves
the nuisance score equations. Fixed-shape geodesic convexity identifies it
as the global nuisance optimum; positive local curvature makes it unique.
This argument avoids relying on a Euclidean Hessian outside the ball.

Strong convexity bounds the improvement over theta zero by
`xi^2 G^2/(2lambda)`. The center likelihood inequality from signed-tail.md
therefore gives the uniform signed profile bound

```
P(1/xi)-P_normal <= A xi + B xi^2,
B = sum x^4/4 + n h/24 + G^2/(2lambda).
```

A negative A and `h B<=-A/2` strictly exclude every `nu>=1/h`. For any
strictly negative A, sufficiently small r makes the normal Hessian lower
bound positive, and sufficiently small h satisfies both strict conditions.
The script tries three radii and 100 halvings each; failure is a budget
outcome. Its computed interval endpoints currently use ordinary binary64
arithmetic without propagated practical error allowances; these are
research comparisons rather than an approved production certificate.

This is a mathematical improvement over the global nuisance rectangle,
with usable terminal intervals on ordinary normal samples. It still does
not prove a normal global winner: all smaller shapes and the lower faces
must be excluded. Nor does it enclose a finite joint stationary root. The
main branch-and-bound driver deliberately retains its earlier unsigned-tail
refusal rather than silently treating this additional research calculation
as a complete normal-winner implementation.
