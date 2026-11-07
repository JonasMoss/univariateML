# Strict shape-profile concavity

This proof uses only finite half-integer Bessel polynomials. All assertions
in this file are exact-arithmetic statements, independently of the numerical
curvature scan.

For a count k>=2 put m=k-1, c=k(k-1), and let E and V be the mean and
variance of J under weights a_j z^(-j) from Q_m(1/z). The polynomial ODE is

y^2 Q'' + 2(y+1) Q' - c Q = 0.

Consequently

    dE/dlog(z) = -V,
    V = c-(2z+1)E-E^2.

We claim

    V < b(E) = E*(k^2-E^2)/(k^2+E^2).                 (1)

Let f(z)=V-b(E). At any zero of f, direct differentiation of the two
identities above gives

    z*f'(z) = 2*E*k*(E+k)^2*(E^3+E^2*k-E*k^2+k^3)/(E^2+k^2)^3 > 0.

Indeed 0<E<k-1<k, and the last polynomial is
E^3+E^2*k+k^2*(k-E)>0. Every zero would therefore cross from negative to
positive as z increases. But the finite polynomial expansion at infinity is

    E=c/(2z)-c/(2z^2)+O(z^-3),
    V=c/(2z)-c/z^2+O(z^-3),
    b(E)=E+O(z^-3),

so f(z)=-c/(2z^2)+O(z^-3)<0 eventually. A nonnegative region would need a
crossing back to negative, which is impossible. This proves (1).

Now hold mu>0 and write

    w=(sqrt(1+2mu^2/lambda)-1)/(sqrt(1+2mu^2/lambda)+1),  0<w<1,
    z=mu*(1-w^2)/(2w),
    log p_k=-mu*(1-w)+k*log(mu*(1-w)/(1+w))+log Q_{k-1}(1/z)-log(k!).

The scaled second derivative of log p_k is

    (1-w^2)^2 * (log p_k)''
      = -4*k*w+2*E*(3+w^2)+(V-E)*(1+w^2)^2/w^2.

Apply (1), let r=E/k, and divide by k. The resulting strict upper bound
factors as

    2*(r-w)^2*(r*w^2-r-2*w)/(w^2*(1+r^2)) <= 0.

The inequality is strict before the final weak upper bound, including r=w,
because (1) is strict. For k=1, Q_0=1 and the scaled second derivative is
-4w<0. For k=0 the log mass is linear in w. Thus the sample log likelihood
is strictly concave on 0<w<1 whenever at least one count is positive.

At w=0 its one-sided derivative is

    n*mu-2*S+sum(k*(k-1))/mu.

At w=1 the likelihood tends to minus infinity for any positive count, and
the derivative tends to minus infinity. A finite maximum exists and is
unique iff the derivative at zero is strictly positive; otherwise the
unique supremum is the unattained Poisson face. With mu=xbar this sign is
exactly the population-variance-minus-mean sign. The existing fixed-z mean
reduction therefore proves uniqueness of the joint finite maximizer too.

For held mu the condition is

    population_variance + (xbar-mu)^2 > xbar.

Equality gives the Poisson supremum, not a finite tie. All-zero data with
free shape attain the zero mass at lambda=0; mean is unidentified if free.
Positive held shape has the separate unique mean maximum proved in the
dossier. These cases exhaust all supported free subsets.
