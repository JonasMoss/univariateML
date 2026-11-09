"""Independent numerical checks of the proved shape-dependent nuisance boxes."""
import mpmath as mp,json,pathlib
mp.mp.dps=50
rows=[]
for family in ['gev','frechet']:
    data=json.loads(pathlib.Path(f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())['data']
    x=list(map(mp.mpf,data));m=min(x);M=max(x);unit=M-m
    for branch in ['frechet','weibull']:
        d=[(v-m)/unit if branch=='frechet' else (M-v)/unit for v in x]
        n=len(d)
        def root(a):
            def score(u):
                e=mp.exp(u)
                D=[e+v for v in d]
                S=lambda power:mp.fsum(v**power for v in D)
                if branch=='frechet':return mp.log(n*S(-a-1)/(S(-a)*S(-1)))-mp.log1p(1/a)
                return mp.log(n*S(a-1)/(S(a)*S(-1)))-mp.log1p(-1/a)
            lo,hi=mp.mpf(-20),mp.mpf(20)
            for _ in range(40):
                mid=(lo+hi)/2;s=score(mid)
                if (s>0)==(branch=='frechet'):lo=mid
                else:hi=mid
            u=mp.findroot(score,(lo,hi),solver='secant')
            return mp.exp(u)
        def scale(e,a):
            power=-a if branch=='frechet' else a
            return (mp.fsum((e+v)**power for v in d)/n)**(1/power)
        shapes=list(map(mp.mpf,['1.1','1.5','2','4','10']))
        gaps=[root(a) for a in shapes]
        assert all(v<w for v,w in zip(gaps,gaps[1:]))
        checks=0
        for A,B,eA,eB in zip(shapes,shapes[1:],gaps,gaps[1:]):
            a=(A+B)/2;e=root(a);s=scale(e,a);q=s-e
            if branch=='frechet':
                qlo=scale(eA,B)-eA;qhi=scale(eB,A)-eB
                slo=scale(eA,B)/B;shi=scale(eB,A)/A
            else:
                qlo=scale(eB,A)-eB;qhi=scale(eA,B)-eA
                slo=scale(eA,A)/B;shi=scale(eB,B)/A
            assert qlo<=q<=qhi and slo<=s/a<=shi
            checks+=1
        rows.append(dict(data=family,branch=branch,shapes=list(map(str,shapes)),gaps=list(map(str,gaps)),corner_checks=checks))
print(json.dumps(dict(precision=mp.mp.dps,scope='numerical cross-check of algebra, not a global fitted certificate',rows=rows),indent=2))
