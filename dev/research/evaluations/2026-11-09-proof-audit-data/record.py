#!/usr/bin/env python3
"""Freeze source identities, environment, and completed foreground-suite logs."""
import gzip
import hashlib
import json
import platform
import subprocess
from pathlib import Path

here=Path(__file__).resolve().parent
root=here.parents[3]
sources=[root/'AGENTS.md',root/'dev/research/showcase.md',root/'dev/validation/diagnostics.md',root/'dev/roadmap/2026-10-29-parameter-rules.md',root/'dev/design/work-budgets.md',root/'univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md']
sources += [root/f'univariateML-cpp/include/cppunivariate/distributions/{name}/README.md' for name in ['pig','frechet','nbinom','sged','betapr','bbinom']]
sources += [root/f'dev/research/evaluations/{name}-report.md' for name in ['2026-11-06-pig-cert','2026-11-07-evt-scalar','2026-11-07-nb-proof','2026-11-07-sged-subunit','2026-11-07-betapr-prof','2026-11-08-betapr-joint','2026-11-07-bbinom-face']]
sources += [root/'univariateML-cpp/include/cppunivariate/distributions/betapr/certificate.hpp',root/'dev/research/evaluations/2026-11-06-pig-cert-data/concavity-proof.md',root/'dev/research/evaluations/2026-10-29-pig/analysis.md',root/'dev/research/evaluations/2026-11-07-betapr-prof-data/counterexample-refined.json']
digest=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
commands={'revision':['git','rev-parse','HEAD'],'compiler':['c++','--version'],'R':['R','--version'],'just':['just','--version'],'cmake':['cmake','--version']}
env={k:subprocess.check_output(v,text=True).splitlines()[0] for k,v in commands.items()}
env.update(python=platform.python_version(),platform=platform.platform())
logs={}
for p in sorted((root/'build/scratch/proof-audit').glob('*.log')):
    target=here/(p.name+'.gz')
    target.write_bytes(gzip.compress(p.read_bytes(),mtime=0))
    logs[target.name]={'uncompressed_sha256':digest(p),'bytes':p.stat().st_size}
aee=root/'build/scratch/proof-audit/aee.pdf'
out={'environment':env,'sources':{str(p.relative_to(root)):digest(p) for p in sources},'logs':logs,'AEE':{'url':'https://stacks.stanford.edu/file/druid:gy645ty0351/gy645ty0351_OLK-NSF-282.pdf','sha256':digest(aee),'bytes':aee.stat().st_size,'inspected':'PDF pages 5, 8, 9; printed pages 3, 6, 7. Equation (4) and Case 3 cancellation read directly from page images.','retention':'Copyrighted PDF and page images only in ignored build/scratch/proof-audit; not part of the deliverable.'}}
out['numerical_results']={p.name:digest(p) for p in [here/'results.json',here/'boundaries.json']}
out['audit_scripts']={p.name:digest(p) for pattern in ['*.py','*.sh','*.R'] for p in sorted(here.glob(pattern))}
(here/'provenance.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(env,indent=2))
