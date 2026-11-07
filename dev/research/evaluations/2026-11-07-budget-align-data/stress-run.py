import json,subprocess,time,os
from pathlib import Path
base=Path(__file__).resolve().parent
families='bbinom betapr bnbinom burr cauchy cmb dgamma foldnorm fpareto gb2 gpd invburr invparalogis invpareto kumar lomax lst nbinom paralogis pareto2 pareto4 slash skellam sn std trgamma invtrgamma zinb zmnbinom ztnbinom zib ztbinom'.split()
families=os.environ.get('STRESS_FAMILIES',' '.join(families)).split()
limit=int(os.environ.get('STRESS_SECONDS','30'))
binary=os.environ.get('STRESS_BINARY','build/scratch/budget-stress')
for f in families:
 family_limit=int(os.environ.get('STRESS_CMB_SECONDS',str(limit))) if f=='cmb' else limit
 for mode in range(4):
  start=time.monotonic()
  try:
   p=subprocess.run([binary,f,str(mode)],text=True,capture_output=True,timeout=family_limit)
   result=dict(family=f,mode=mode,elapsed=time.monotonic()-start,returncode=p.returncode,output=p.stdout.strip(),stderr=p.stderr.strip())
  except subprocess.TimeoutExpired:
   result=dict(family=f,mode=mode,elapsed=time.monotonic()-start,censored=True,limit_seconds=family_limit)
  print(json.dumps(result),flush=True)
