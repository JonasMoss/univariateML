"""Focused independent checks before the final all-row consumer measurement."""
import hashlib,json,math
import mpmath as mp
import audit
from quality.truth_reference import gamma

def main():
    with mp.workdps(160):
        rate=mp.mpf('1e8')
        poisson_body=float(gamma(rate,rate)[1])
        mode=float(-rate+rate*mp.log(rate)-mp.loggamma(rate+1))
        nb_mass=mp.exp(mp.loggamma(2*rate-1)-2*mp.loggamma(rate)-(2*rate-1)*mp.log(2))
        nb_body=float(mp.log(mp.mpf('.5')-nb_mass))
    # job: id, d/p/q mode, input, lower, log, parameter vector.
    cases=[
      (('pareto4',0,999.9976974175579,True,True,[1.,1e8,1000.,0.]),-218.7455815369823,'log'),
      (('nbinom',1,138155105234.,True,True,[1.,5e-9]),-9.999999962748696e-301,'relative'),
      (('zip',2,-0.6931471805599453,True,True,[1e8,.5]),99916056.,'exact'),
      (('zip',2,-0.6931471805599453,False,True,[1e8,.5]),0.,'exact'),
      (('zip',2,.5,True,False,[1e8,.5]),0.,'exact'),
      (('zip',2,-1.,True,False,[1e8,.5]),'NaN','nan'),
      (('zmpois',0,1e8,True,True,[1e8,.5]),-10.822426086574135,'log'),
      (('zmpois',2,-0.6931471805599453,True,True,[1e8,.5]),99916056.,'exact'),
      (('zmnbinom',2,-0.6931471805599453,True,True,[1e8,.5,.5]),[99881303.,99881304.],'jump'),
      (('zinb',2,-0.6931471805599453,True,True,[1e8,.5,.5]),[99881303.,99881304.],'jump'),
      (('ztpois',1,99999999.,True,True,[1e8]),poisson_body,'log'),
      (('ztpois',0,1e8,True,True,[1e8]),mode,'log'),
      (('ztnbinom',1,99999998.,True,True,[1e8,.5]),nb_body,'log'),
      (('cmp',1,99629758.,True,False,[1e8,1.]),1.0030148691227151e-300,'relative'),
      (('pig',1,0.,True,True,[1.,1e-8]),-0.0001414113565908629,'log'),
      (('yulesimon',1,33.,True,False,[1e-8]),4.088798134072771e-8,'relative'),
      (('tnorm',1,1.3862943611198904,True,True,[-1e16,1e8,0.,'Inf']),-0.28768207245178096,'log'),
      (('tnorm',1,-690.7755278981898,True,True,[1e16,1e8,'-Inf',0.]),-690.7755278982137,'log'),
      (('tnorm',2,-690.7755278982137,False,True,[-1e16,1e8,0.,'Inf']),690.7755278981898,'log'),
      (('nhyper',1,99999997.99999999,False,False,[5.,1e8,3.]),6.000000180000012e-23,'relative'),
      (('tnorm',1,1e100,False,True,[-1e16,1e8,0.,'Inf']),-5e183,'log'),
    ]
    records=[];failed=0
    for name in ('probe-fixed','probe-double-fixed'):
        audit.EXE=audit.WORK/name
        actual=audit.evaluate([c[0] for c in cases])
        for (job,expected,kind),got in zip(cases,actual):
            if kind=='nan':ok=math.isnan(got)
            elif kind=='jump':ok=got in expected
            elif kind=='exact':ok=got==expected
            elif kind=='relative':ok=math.isfinite(got) and abs(got/expected-1)<1e-10
            else:ok=math.isfinite(got) and abs(got-expected)<1e-12*max(1,abs(expected))
            failed+=not ok
            records.append(dict(binary=name,binary_sha256=hashlib.sha256(audit.EXE.read_bytes()).hexdigest(),
                                job=job,actual=got if math.isfinite(got) else str(got),expected=expected,passed=ok))
    (audit.HERE/'fixed-smoke.json').write_text(json.dumps(records,indent=2,allow_nan=False)+'\n')
    print('Focused checks:',len(records),'failures:',failed,flush=True)
    return int(failed>0)

if __name__=='__main__':raise SystemExit(main())
