"""Single-slot site validation and final numerical evidence after ST reruns."""
import os, subprocess
import study
H=study.H
os.environ.update(EMCC_CORES='1',BINARYEN_CORES='1',GOMAXPROCS='1')
def run(command,env=None):
 print('LATE ST EVIDENCE',command,flush=True)
 subprocess.run(command,check=True,env=env)
run(['just','site-check'])
base=dict(os.environ);base.pop('DPQ_SKEW_BINARY',None)
run(['python3',str(H/'st-small-cdf.py'),'before'],dict(base,DPQ_SKEW_BINARY='probe-baseline'))
run(['python3',str(H/'seal-current.py')],base)
run(['python3',str(H/'st-small-cdf.py'),'after'],base)
run(['python3',str(H/'st-normal-face.py'),'after'],base)
for script in ['errors.py','preserve.py']:
 run(['python3',str(H/script)],base)
run(['node','--v8-pool-size=1',str(H/'js-consumer.mjs')],base)
run(['node','--v8-pool-size=1',str(H/'st-extra-js.mjs')],base)
print('All final ST reruns and numerical evidence green',flush=True)
