"""Independent high-precision cell quadrature for the stopped gamma witness.
Run through run. No incomplete-gamma implementation is used by this check.
"""
import json,subprocess,sys
from pathlib import Path
import mpmath as mp
p=Path(__file__).resolve().parent
mp.mp.dps=60
w=[r for r in json.loads((p/'witnesses.json').read_text()) if r['index']==997][0]
a,r=map(mp.mpf,w['candidates'][0]['coef'])
ll=mp.mpf(0);score=mp.mpf(0);derivative=mp.mpf(0)
for x in [1000000,1000001,1000003,1000008,1000011]:
 mid=mp.mpf(x)+mp.mpf('.5')
 def kernel(u):return mp.exp((a-1)*mp.log1p((u-mp.mpf('.5'))/mid)-r*(u-mp.mpf('.5')))
 z=mp.quad(kernel,[0,1]);m=mp.quad(lambda u:u*kernel(u),[0,1])/z
 variance=mp.quad(lambda u:(u-m)**2*kernel(u),[0,1])/z
 ll+=(a-1)*mp.log(r*mid)-r*mid-mp.loggamma(a)+mp.log(r)+mp.log(z)
 score+=a-r*(x+m);derivative+=-r*(x+m)+r*r*variance
score/=5;derivative/=5
row=dict(method='60-digit scaled cell quadrature',loglik=str(ll),mean_log_rate_score=str(score),mean_log_rate_derivative=str(derivative),reported_loglik_lower=w['candidates'][0]['loglik_lower'],reported_loglik_upper=w['candidates'][0]['loglik_upper'],score_roundoff=w['rate_score_roundoff'],tolerance_times_information=str(mp.mpf('1e-10')*(-derivative)))
row['likelihood_in_witness_interval']=w['candidates'][0]['loglik_lower']<=ll<=w['candidates'][0]['loglik_upper']
if len(sys.argv)>1:
 measured=json.loads(subprocess.check_output([sys.argv[1]],input=f"{w['failed_shape']:.17g} {w['failed_rate']:.17g}\n",text=True))
 row['computed_score']=measured['score'];row['computed_derivative']=measured['derivative']
 row['absolute_score_error']=str(abs(mp.mpf(measured['score'])-score))
 assert abs(mp.mpf(measured['score'])-score)<=w['rate_score_roundoff']
output=Path(sys.argv[2]) if len(sys.argv)>2 else p/'precision-check.json'
output.write_text(json.dumps(row,indent=2)+'\n');print(json.dumps(row,indent=2))
assert row['likelihood_in_witness_interval']
