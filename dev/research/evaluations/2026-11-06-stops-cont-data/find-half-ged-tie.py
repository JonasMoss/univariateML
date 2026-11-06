"""Independent shape-1/2 conditional-profile seam: x=(-1,z), mean=0."""
import math

def comparison(z):
    def score(u):
        return -2/u + .5*(-1/math.sqrt(6-u)+z/math.sqrt(6+z*u))
    lo=0.0
    root=None
    for i in range(1,1000):
        hi=6*i/1000
        if score(hi)>0:
            for _ in range(100):
                mid=(lo+hi)/2
                if score(mid)>0: hi=mid
                else: lo=mid
            root=(lo+hi)/2
            break
        lo=hi
    endpoint=2*math.log(3)-math.sqrt(6+6*z)
    if root is None:
        return None,endpoint,None
    interior=2*math.log(.5*root)-math.sqrt(6-root)-math.sqrt(6+z*root)
    return interior,endpoint,root

lo,hi=2.0,100.0
for _ in range(80):
    z=(lo+hi)/2
    first,last,u=comparison(z)
    if first is not None and first>last: hi=z
    else: lo=z
z=(lo+hi)/2
print(format(z,'.17g'),comparison(z))
