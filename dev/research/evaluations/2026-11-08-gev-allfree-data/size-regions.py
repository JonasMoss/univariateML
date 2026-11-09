import json,pathlib,mpmath as mp
mp.mp.dps=50
root=pathlib.Path(__file__).parent
source=json.loads((root/'region-check.json').read_text())
rows=[]
for row in source['rows']:
    original=list(map(mp.mpf,json.loads(pathlib.Path(f"univariateML-cpp/include/cppunivariate/distributions/{row['data']}/golden.json").read_text())['data']))
    m=min(original);M=max(original);unit=M-m
    positive=row['branch']=='frechet'
    d=[(x-m)/unit if positive else (M-x)/unit for x in original]
    def scale(e,a):
        power=-a if positive else a
        return (mp.fsum((e+x)**power for x in d)/len(d))**(1/power)
    shapes=list(map(mp.mpf,row['shapes']));gaps=list(map(mp.mpf,row['gaps']))
    for A,B,eA,eB in zip(shapes,shapes[1:],gaps,gaps[1:]):
        if positive:
            ml=scale(eA,B)-eA;mh=scale(eB,A)-eB
            sl=scale(eA,B)/B;sh=scale(eB,A)/A
            xl,xh=1/B,1/A
        else:
            ml=1-(scale(eA,B)-eA);mh=1-(scale(eB,A)-eB)
            sl=scale(eA,A)/B;sh=scale(eB,B)/A
            xl,xh=-1/A,-1/B
        radius=max(abs(ml),abs(mh),abs(1-ml),abs(1-mh))/sl
        rows.append(dict(data=row['data'],branch=row['branch'],xi=[str(xl),str(xh)],mu=[str(m+unit*ml),str(m+unit*mh)],sigma=[str(unit*sl),str(unit*sh)],standardized_radius=str(radius)))
print(json.dumps(dict(precision=mp.mp.dps,scope='Numerical sizes of the proved corner formulas evaluated at point roots; not outward-rounded root enclosures or a global certificate',rows=rows),indent=2))
