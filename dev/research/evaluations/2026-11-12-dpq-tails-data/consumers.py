"""Replay all active independent truth rows with the existing all-row checker."""
import importlib.util,json,sys
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3]
p=root/'dev/research/evaluations/2026-11-11-oracle-gaps-data/audit.py'
spec=importlib.util.spec_from_file_location('prior_audit',p);a=importlib.util.module_from_spec(spec);spec.loader.exec_module(a)
ids=['bbinom','beta','betapr','dgamma','fpareto','gb2','lgamma','lnorm','pareto4']
ads=a.adapters();a.adapters=lambda:{k:ads[k] for k in ids};a.HERE=here
a.WORK=root/'build/scratch/dpq-tails-consumers';a.WORK.mkdir(parents=True,exist_ok=True);a.EXE=a.WORK/'probe'
mode=sys.argv[1]
sys.argv=['audit.py','build','after']+(['no-long-double'] if mode=='double' else [])
a.build();a.EXE=a.WORK/('probe-double' if mode=='double' else 'probe')
sys.argv=['audit.py','check',mode,'--active-only'];a.check(mode)

assert json.loads((here/(mode+'-summary.json')).read_text())['active_failures']==0
