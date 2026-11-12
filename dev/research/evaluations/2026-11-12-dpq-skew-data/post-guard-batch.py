"""Computing batch: run under one four-slot reservation, without nested locks."""
import subprocess, sys, os
for recipe in ['cpp-test', 'cpp-test-no-long-double', 'test']:
    env = os.environ.copy()
    if recipe == 'cpp-test':
        env['UMVL_TEST_TIER'] = 'full'
    print('FINAL GUARD CHECK', recipe, 'tier', env.get('UMVL_TEST_TIER', 'default'), flush=True)
    code = subprocess.call(['just', recipe], env=env)
    if code:
        sys.exit(code)
