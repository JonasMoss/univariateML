#!/usr/bin/env python3
import collections,json,math,pathlib
root=pathlib.Path(__file__).resolve().parent;repo=root.parents[3]
rows=[]
for family in ['gev','frechet']:
    g=json.loads((repo/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
    x=sorted(g['data']);n=len(x);b=g['logLik'];B=math.exp(-b/n)
    k=max(collections.Counter(x).values())
    spacing=min(v-u for u,v in zip(x,x[1:]) if v>u)
    def lower(j,d):
        return min(math.log(d/4),(b-(n-j)*(math.log(16)-2*math.log(d)))/(n-2*j))
    original=lower(k,spacing)
    conservative=max(lower(j,min(x[i+j]-x[i] for i in range(n-j))) for j in range(k,(n-1)//2+1))
    def lower(j,d):
        return min(math.log(d/2),(b-(n-j)*(math.log(4)-2*math.log(d)))/(n-2*j))
    candidates=[(lower(j,min(x[i+j]-x[i] for i in range(n-j))),j) for j in range(k,(n-1)//2+1)]
    logS,j=max(candidates);S=math.exp(logS)
    Z=(x[-1]-x[0]+B)/S;T=(x[-1]-x[0]+3*B)/S
    # r=1/2. Compute the Gumbel error constant in log coordinates.
    aa=math.log(2*Z);bb=2*math.log(Z)+2*Z+math.log1p(math.exp(-2*Z))
    logC=max(aa,bb)+math.log1p(math.exp(-abs(aa-bb)))
    D=2*math.log(max(1,T))+2*max(math.exp(-1),T*T*math.log(max(1,T)))+T
    rows.append(dict(family=family,n=n,largest_multiplicity=k,minimum_spacing=spacing,
        incumbent=b,scale_upper=B,original_scale_lower=math.exp(original),
        conservative_optimized_scale_lower=math.exp(conservative),strengthened_scale_lower=S,selected_near_count=j,Z=Z,T=T,
        log_gumbel_error_constant=logC,reverse_error_constant=D,
        log_shape_slab_for_one_loglik_unit=-math.log(n)-logC))
print(json.dumps(rows,indent=2))
