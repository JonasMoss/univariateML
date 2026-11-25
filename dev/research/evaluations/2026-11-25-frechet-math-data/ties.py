"""Interval certificates for exact (implicitly specified) sample ties."""
from fractions import Fraction as F
import math
import mpmath as mp
from mpmath import iv
from replay import held_point

iv.dps = 40


def interval(a,b=None):
    def point(q):
        q=F(q)
        return iv.mpf(q.numerator)/q.denominator
    if b is None:
        return point(a)
    return iv.mpf([point(a).a,point(b).b])


def tpoint(b,z):
    if b==0:
        return z
    return iv.ln(1+b*z)/b


def free_interval(x,p,a,b,y):
    z=[g*iv.exp(-y) for g in x]
    a,b=interval(a),interval(b)
    shape=iv.mpf([a.a,b.b])
    t=[iv.mpf([tpoint(b,g.a).a,tpoint(a,g.b).b]) for g in z]
    r=[g/(1+shape*g) for g in z]
    w=[q*iv.exp(-v) for q,v in zip(p,t)]
    total=sum(w)
    value=-y-(1+shape)*sum(q*v for q,v in zip(p,t))-iv.ln(total)-1
    score=-1+(1+shape)*sum(q*v for q,v in zip(p,r))-sum(q*v for q,v in zip(w,r))/total
    # t_b=-integral_0^z s/(1+b*s)^2 ds; the integral grows in z
    # and decreases in b. Use the closed form at the two corners.
    def db(b,z):
        if b==0:
            return -z*z/2
        return (z/(1+b*z)-tpoint(b,z))/b
    dt=[iv.mpf([db(a,g.b).a,db(b,g.a).b]) for g in z]
    directional=(-sum(q*v for q,v in zip(p,t))
                 -(1+shape)*sum(q*v for q,v in zip(p,dt))
                 +sum(q*v for q,v in zip(w,dt))/total)
    return value,score,directional


class Family:
    def __init__(self, free, counts=None):
        self.free=free
        self.xmiddle=F(1) if free else F(1,20)
        if counts is None:
            counts=[1,1,1] if free else [3,4,1]
        self.p=[F(k,sum(counts)) for k in counts]
        self.search=(-6,2) if free else (-2,0)

    def point(self,a,b,c,d,y):
        x=[interval(0),interval(self.xmiddle),interval(c,d)]
        p=list(map(interval,self.p))
        if self.free:
            return free_interval(x,p,a,b,y)
        return held_point(x,p,interval(a,b),y,iv)

    def root(self,a,b,c,d):
        shape, data = float((a+b)/2),float((c+d)/2)
        x = [0.,float(self.xmiddle),data]
        p = list(map(float,self.p))
        def approximate_score(y):
            if self.free:
                z = [g*math.exp(-y) for g in x]
                t = z if shape==0 else [math.log1p(shape*g)/shape for g in z]
                r = [g/(1+shape*g) for g in z]
                w = [q*math.exp(-v) for q,v in zip(p,t)]
                return -1+(1+shape)*sum(q*v for q,v in zip(p,r))-sum(q*v for q,v in zip(w,r))/sum(w)
            e = math.exp(y/shape)
            r = [e/(g+e) for g in x]
            h = [math.exp(-shape*math.log(g+e)) for g in x]
            return sum(q*v*(w-1-1/shape) for q,v,w in zip(p,r,h))
        # Binary64 proposes a center. Only interval signs restrict the root.
        lo,hi=self.search
        for _ in range(25):
            mid=(lo+hi)/2
            if approximate_score(mid)>0:
                lo=mid
            else:
                hi=mid
        y=(lo+hi)/2
        width=1e-7
        for _ in range(26):
            lo,hi=iv.mpf(y-width),iv.mpf(y+width)
            if self.point(a,b,c,d,lo)[1]>0 and self.point(a,b,c,d,hi)[1]<0:
                return iv.mpf([lo,hi]),iv.mpf(y)
            width*=2
        return None,iv.mpf(y)

    def upper(self,a,b,c,d):
        y,mid=self.root(a,b,c,d)
        if y is None:
            return None
        value,_,score=self.point(a,b,c,d,y)
        return value,score

    def lower(self,a,c,d):
        _,y=self.root(a,a,c,d)
        return self.point(a,a,c,d,y)[0].a

    def exclude(self,a,b,d,witness):
        level=self.lower(witness,d,d)
        stack=[(a,b,0)]
        visited=0
        while stack:
            lo,hi,depth=stack.pop()
            result=self.upper(lo,hi,d,d)
            visited+=1
            if result is not None:
                val,score=result
                if val < level:
                    continue
                if score>0 or score<0:
                    edge=hi if score>0 else lo
                    result_edge=self.upper(edge,edge,d,d)
                    if result_edge is not None and result_edge[0]<level:
                        continue
            assert depth<24, (lo,hi,d,witness)
            mid=(lo+hi)/2
            stack.extend([(lo,mid,depth+1),(mid,hi,depth+1)])
        return visited

    def barrier(self,split,witness,lo,hi):
        stack=[(lo,hi,0)]
        visited=0
        while stack:
            c,d,depth=stack.pop()
            upper=self.upper(split,split,c,d)
            visited+=1
            if upper is not None and upper[0] < self.lower(witness,c,d):
                continue
            assert depth<20
            mid=(c+d)/2
            stack.extend([(c,mid,depth+1),(mid,d,depth+1)])
        return visited

    def seam_positive(self,lo,hi):
        stack=[(lo,hi,0)]
        while stack:
            c,d,depth=stack.pop()
            upper=self.upper(F(0),F(0),c,d)
            if upper is not None and upper[1]>0:
                continue
            assert depth<20
            mid=(c+d)/2
            stack.extend([(c,mid,depth+1),(mid,d,depth+1)])


    def face_cover(self,lo,hi):
        # Prove that only xi=0 and xi=1 can win for this data segment.
        stack=[(F(0),F('.1'),lo,hi,-1,0),
               (F('.1'),F('.9'),lo,hi,0,0),
               (F('.9'),F(1),lo,hi,1,0)]
        visited=0
        while stack:
            a,b,c,d,sign,depth=stack.pop()
            upper=self.upper(a,b,c,d)
            visited+=1
            if upper is not None:
                if sign and sign*upper[1]>0:
                    continue
                if not sign and upper[0] < max(self.lower(F(0),c,d),self.lower(F(1),c,d)):
                    continue
            assert depth<25
            if b-a>=d-c:
                m=(a+b)/2
                stack.extend([(a,m,c,d,sign,depth+1),(m,b,c,d,sign,depth+1)])
            else:
                m=(c+d)/2
                stack.extend([(a,b,c,m,sign,depth+1),(a,b,m,d,sign,depth+1)])
        return visited


def endpoints():
    # Enclose each unique Gumbel root using a high precision proposal,
    # then compare both endpoint values with interval arithmetic.
    for p,sign in ((F(2,5),-1),(F(49,100),1),(F(1,3),-1)):
        v=mp.mpf(p.numerator)/p.denominator
        q=1-v
        score=lambda t: 1/t-q+q*mp.exp(-t)/(v+q*mp.exp(-t))
        t=mp.findroot(score,(2,3))
        lo,hi=[iv.mpf(mp.nstr(t+j*mp.mpf('1e-20'),35)) for j in (-1,1)]
        v=interval(p)
        q=1-v
        def score(t):
            return 1/t-q+q*iv.exp(-t)/(v+q*iv.exp(-t))
        assert score(lo)>0>score(hi)
        t=iv.mpf([lo,hi])
        g=iv.ln(t)-q*t-iv.ln(v+q*iv.exp(-t))-1
        h=v*(1-2*v)/(iv.sqrt(v*q)+v)
        t=iv.ln((1+h)/h)
        f=iv.ln(iv.exp(t)-1)-2*q*t-iv.ln(v+q*iv.exp(-t))-1
        assert sign*(f-g)>0
    print('Two-point endpoint signs verified.',flush=True)


if __name__=='__main__':
    endpoints()
    held=Family(False)
    a,b,split,lo,hi=map(F,('1','10','1.2','39','40'))
    # For A>=10 the upper bound log(A+1)-(A+1)*log(39)/8
    # decreases and is below a feasible boundary likelihood for all data.
    assert 1/interval(11)-iv.ln(interval(39))/8 < 0
    assert iv.ln(interval(11))-11*iv.ln(interval(39))/8 < held.lower(a,lo,hi)
    # The exclusion test uses the previously proved unique nuisance root.
    print('Held low, high, barrier cells:',
          held.exclude(a,split,lo,F('1.575')),
          held.exclude(split,b,hi,a),
          held.barrier(split,a,lo,hi),flush=True)
    free=Family(True)
    a,b,split,lo,hi=map(F,('0','1','.65','2.6','2.7'))
    free.seam_positive(lo,hi)
    print('Free low, high, barrier cells:',
          free.exclude(split,b,lo,F('.053')),
          free.exclude(a,split,hi,b),
          free.barrier(split,b,lo,hi),flush=True)
    face=Family(True, [4,5,1])
    lo,hi=F('1.4'),F('1.5')
    for r,sign in ((lo,-1),(hi,1)):
        left=face.upper(F(0),F(0),r,r)[0]
        right=face.upper(F(1),F(1),r,r)[0]
        assert sign*(right-left)>0
    print('Finite/Gumbel face cover cells:',face.face_cover(lo,hi),flush=True)
    print('Exact global tie certificates passed.',flush=True)
