"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,a,b=t['mean'],t['sd'],t['a'],t['b']
    def interval(l,h):
        # Midpoint integration error is O(((h-l)*(1+|l|+|h|))^2).
        # This branch resolves intervals much narrower than mp tail precision.
        if (h-l)*(1+abs(l)+abs(h)) < mp.mpf('1e-35'):
            return mp.log(h-l)-mp.log(2*mp.pi)/2-((l+h)/2)**2/2
        if l>=0:
            u,v=normal(l)[1],normal(h)[1]
            return u+mp.log(-mp.expm1(v-u)) if v!=-mp.inf else u
        if h<=0:
            u,v=normal(h)[0],normal(l)[0]
            return u+mp.log(-mp.expm1(v-u)) if v!=-mp.inf else u
        return mp.log((mp.erf(h/mp.sqrt(2))-mp.erf(l/mp.sqrt(2)))/2)
    def physical_interval(l,h):
        if mp.isfinite(l) and mp.isfinite(h):
            width=(h-l)/s
            start=(l-m)/s
            if width*(1+abs(start)+abs(width))<mp.mpf('1e-35'):
                # Keep the physical width before subtracting a huge mean.
                # Midpoint relative error is below 1e-70 in this branch.
                return mp.log(width)-mp.log(2*mp.pi)/2-start*start/2-start*width/2-width*width/8
        return interval((l-m)/s,(h-m)/s)
    z=physical_interval(a,b)
    def tails(x):
        l=physical_interval(a,x)-z
        h=physical_interval(x,b)-z
        return pair(l) if l<h else pair(h,False)
    out=Reference(tails,a,b)
    out.log_density=lambda x:-(x-m)**2/(2*s*s)-mp.log(s*mp.sqrt(2*mp.pi))-z
    if abs(m)/s>1e5:
        width=s*s/max(abs(a-m) if mp.isfinite(a) else 0,abs(b-m) if mp.isfinite(b) else 0)
        center=a if m<a else b
        direction=1 if m<a else -1
        end=min(mp.mpf(120),(b-a)/width)
        out.normalization_integrand=lambda y:mp.exp(out.log_density(center+direction*width*y))*width
        out.normalization_domain=sorted(set([mp.mpf(0),end]+[mp.mpf(v) for v in (1,10) if v<end]))
    return out

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'mean': -1e+16, 'sd': 100000000.0, 'a': 0, 'b': mp.inf}, {'mean': 1e+16, 'sd': 100000000.0, 'a': -mp.inf, 'b': 0}, {'mean': 1e+16, 'sd': 100000000.0, 'a': 0, 'b': 1}, {'sd': 100000000.0, 'a': 0, 'b': 1}]
