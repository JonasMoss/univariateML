# Refines the upper log-concavity endpoint for lambda>2 (matches 8/3 to 50 digits; numerical, not a proof).
import mpmath as mp
mp.mp.dps = 50
def D(u,l): return u**(l-1)+(1-u)**(l-1)
def sec(u,l): return mp.diff(lambda t: 1/D(t,l), u, 2)
def maxsec(l):
    us=mp.linspace(mp.mpf('1e-6'),mp.mpf('0.5'),2000)
    vals=[sec(u,l) for u in us]; i=max(range(len(us)),key=lambda j: vals[j])
    a=us[max(i-1,0)]; b=us[min(i+1,len(us)-1)]
    um=mp.findroot(lambda t: mp.diff(lambda s: sec(s,l), t), (a+b)/2)
    return um, sec(um,l)
for d in ['-1e-6','0','1e-6']:
    l=mp.mpf(8)/3+mp.mpf(d); um,s=maxsec(l); print(d, mp.nstr(um,10), mp.nstr(s,6))
l0=mp.findroot(lambda l: maxsec(l)[1], mp.mpf(8)/3)
print('threshold', mp.nstr(l0,20), 'minus 8/3:', mp.nstr(l0-mp.mpf(8)/3,5))
