# All-free zero-quadratic gamma seam

This analysis supplies a precise obstruction to deciding a gamma face from
its quadratic coefficient. It does not certify the all-free fitter.

Normalize mean(v)=1 and put g=mean(log(v)). The gamma-face fitted shape k
solves log(k)-psi(k)=-g. Its profiled quadratic coefficient is

    C2=(k-1)*(mean(v^2)-(k+1)/k)/24.

The independent 80-digit `closure-audit.py` gives examples with C2 positive,
negative and zero, and solves the two conditional beta scores at h=.01,.001.
These are numerical stationary checks, not global comparisons.

There is an exact zero-coefficient sample with a **positive quartic gain**.
Let t=sqrt(1-exp(-2*EulerGamma)) and v=(1-t,1+t). Then mean(v)=1 and
g=-EulerGamma, so the gamma face has k=1 and value -1. C2=0 exactly.
At every power h, p=1/h,q=1 is the attained exponential member, also
with normalized value -1. The conditional p score vanishes there.

Expand the q score at this member, using the digamma asymptotic and
log((1-exp(-z))/z)=-z/2+z^2/24-z^4/2880+O(z^6):

    Sq=psi(1/h+1)-psi(1)-r(h)
      = (mean(v^2)-2)*h^2/24+O(h^4).

Here mean(v^2)-2=-exp(-2*EulerGamma), strictly negative. After eliminating
p, the q information tends to I=trigamma(1)-1=pi^2/6-1>0. The implicit
function theorem applied to the gamma-rate/q parametrization gives a
conditional maximum near q=1 with gain

    P(h)-(-1)
      = exp(-4*EulerGamma)*h^4/(1152*(pi^2/6-1))+o(h^4)>0

for sufficiently small positive h. Thus the gamma-face quadratic test is
exactly zero even though finite parameters improve on that face. The
numerical ratios in `closure-audit.json` converge to this quartic coefficient.
This example does not assert that the gamma and moving-power faces tie:
the moving-power value must still be compared independently.

For k away from 1, a negative C2 alone still gives only a local statement.
A certified gamma winner needs a signed, uniform remainder after both
conditional shapes have been enclosed, including paths with shapes diverging
as h tends to zero. At C2=0 the needed order increases. A compact global
search cannot replace those unclosed-tail comparisons.

The existing unsigned small-h bound and signed moving-endpoint large-h bound
reduce the remaining task, but are insufficient by themselves. A complete
all-free implementation still needs (i) a validated conditional two-shape
beta solve with conjugate upper bounds, (ii) compact coverage and joint
coefficient inclusion, (iii) a signed uniform gamma-face neighborhood with
an explicit degenerate-seam outcome, and (iv) comparison of every retained
finite candidate and both faces with margins. The current mesh supplies
none of these coverage guarantees. Its default numerical output was retained
under the instruction to preserve default fits and goldens. The family
certificate flag therefore stays partial.
