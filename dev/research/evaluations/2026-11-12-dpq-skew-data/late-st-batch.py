"""Family-local ST reruns; the caller reserves four slots for this batch."""
import os, subprocess, sys, shutil
from pathlib import Path
H=Path(__file__).resolve().parent
commands=[
 ('native ST build',['cmake','--build','univariateML-cpp/build','--target','distribution_tests','-j4'],{}),
 ('native ST shard',['ctest','--test-dir','univariateML-cpp/build','--output-on-failure','-R','^distribution_st$','-j1'],{'UMVL_TEST_TIER':'full'}),
 ('double ST build',['cmake','--build','build/cpp-no-long-double','--target','distribution_tests','-j4'],{}),
 ('double ST shard',['ctest','--test-dir','build/cpp-no-long-double','--output-on-failure','-R','^distribution_st$','-j1'],{'UMVL_TEST_TIER':'full'}),
 ('R ST full tier',['just','test'],{'UMVL_TEST_TIER':'full','UMVL_TEST_FAMILIES':'st'}),
 ('full WASM rebuild',['just','js-build'],{}),
 ('JS ST and static regressions',['just','js-test'],{'UMVL_TEST_FAMILIES':'st'}),
]
for label,command,extra in commands:
 print('LATE ST CHECK',label,flush=True)
 code=subprocess.call(command,env=dict(os.environ,**extra))
 if code:sys.exit(code)
shutil.copyfile('build/scratch/truth-results.json',H/'js-st-truth-results.json')
