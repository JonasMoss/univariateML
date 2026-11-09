#!/usr/bin/env python3
"""All-domain, normalizer-free rejection sampler from an open uniform stream.
Python random is only a reproducible stand-in for R's uniforms. No package code.
"""
import mpmath as mp
import random, json, pathlib, hashlib
mp.mp.dps=80
HERE=pathlib.Path(__file__).resolve().parent
rng=random.Random(161)
def uniform():
    while True:
        x=rng.random()
        if 0<x<1:return mp.mpf(x)
def draw(m,nu):
    D=mp.sqrt(2*m);r=nu/(2*m);k=r/mp.sqrt(1+r*r)
    M=min(m,mp.mpf(1));D0=mp.sqrt(2*M);a0=2*M-1
    proposals=0
    while proposals<100000:
        proposals+=1
        v=uniform();logmag=mp.log(D0)+mp.log(mp.expm1(-mp.log(v)/a0))
        u=mp.exp(logmag)*(1 if uniform()<mp.mpf('.5') else -1)
        q=1-2*k*u/D+(u/D)**2
        delta=mp.atan2(u*mp.sqrt(1-k*k)/D,1-k*u/D)
        logratio=-m*mp.log(q)-nu*delta
        logaccept=logratio-2*M+2*M*mp.log1p(abs(u)/D0)
        assert logaccept < mp.mpf('1e-60'), (m,nu,u,logaccept)
        if mp.log(uniform())<=logaccept:
            # alpha=1,lambda=0: x = -r + u sqrt(1+r^2)/D
            x=-r+u*mp.sqrt(1+r*r)/D
            return x,proposals,logaccept,abs((1+abs(u)/D0)**(-a0)-v)
    raise ArithmeticError('research rejection budget')
def run():
    rows=[]
    for ms,vs in [('.5001','0'),('.7','40'),('1','-1000'),('10','3'),('10000','1e8')]:
        m,v=map(mp.mpf,(ms,vs));draws=[draw(m,v) for _ in range(64)]
        a=2*m-1;D=mp.sqrt(2*m);r=v/(2*m)
        A=(2*m-2)*mp.log(2)-mp.log(mp.pi)+2*mp.re(mp.loggamma(m+1j*v/2))-mp.loggamma(a)
        h0=mp.exp(A-m*mp.log1p(r*r)+v*mp.atan(r))*mp.sqrt(1+r*r)/D
        M=min(m,mp.mpf(1));R=2*mp.sqrt(2*M)*h0*mp.exp(2*M)/(2*M-1)
        assert R>=1-mp.mpf('1e-60')
        err=max(d[3] for d in draws);assert err<mp.mpf('1e-65')
        rows.append(dict(m=ms,nu=vs,accepted=64,proposals=sum(d[1] for d in draws),
                         theoretical_mean_proposals=mp.nstr(R,18),
                         largest_logaccept=mp.nstr(max(d[2] for d in draws),12),
                         proposal_inverse_error=mp.nstr(err,12),
                         sample_log_absolute_values=[mp.nstr(mp.log(abs(d[0])),12) for d in draws[:4]]))
    out=dict(seed=161,decimal_digits=80,rows=rows,passed=320,
             script_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest(),
             interpretation='Envelope and proposal-transform checks; acceptance counts are numerical, not a distributional proof.')
    (HERE/'sampler.json').write_text(json.dumps(out,indent=2)+'\n')
    print(json.dumps({'sampler_draws':320,'proposals':sum(r['proposals'] for r in rows)}))
if __name__=='__main__':run()
