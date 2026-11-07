"""Symbolic endpoint factors in side fraction p=a/(a+b).
Any smooth rational reparametrization with nonzero derivative and positive
scale factors at the endpoint preserves these vanishing orders. This checks
the orders used by the ellipse implementation; g represents its regular
normalization-score term. Right factors follow by reflection.
"""
import sympy as s
p,m,k,n,L,Q,R,U,g=s.symbols('p m k n L Q R U g')
a=p;b=1-p;w=b-a;ab=a*b;v=ab**2
h=k*b*b+(n-k)*a*a;j=L*b*b+R*a*a;z=Q*b*b+U*a*a
c=z;e=m*w*j;f=m*m*w*w*h
def dif(z):return s.diff(z,p)*ab-2*z*s.diff(ab,p)
h0=dif(f)+g*ab**3;h1=2*dif(e);h2=dif(c)
l=c*h1-e*h2;q=c*h0+n*v*h2
resultant=s.Poly(s.expand(c*q*q-e*q*l-n*v*l*l),p)
loss=z*h-j*j;den=v*h
meanfree=s.Poly(s.expand(g*den**2+s.diff(loss,p)*den-loss*s.diff(den,p)),p)
held=s.Poly(s.expand(g*ab**3+dif(c+2*e+f)),p)
def order(poly,substitution):
    pp=s.Poly(s.expand(poly.as_expr().subs(substitution)),p)
    return min(power[0] for power,coefficient in pp.terms() if coefficient!=0)
checks=[('held sd, free mean, nonconstant left',meanfree,{},1),
        ('held sd, constant left',meanfree,{Q:L*L/k},4),
        ('held sd, empty left',meanfree,{k:0,L:0,Q:0},8),
        ('held mean, constant left',resultant,{Q:L*L/k},3),
        ('held mean, zero left',resultant,{Q:0,L:0},6),
        ('held mean, empty left',resultant,{Q:0,L:0,k:0},12),
        ('both moments held, empty left',held,{Q:0,L:0,k:0},3)]
for name,poly,substitution,expected in checks:
    actual=order(poly,substitution)
    assert actual>=expected,(name,actual,expected)
    print(name,actual)
