#!/usr/bin/env python3
"""Rebuild the original accuracy marker inventory without archiving headers."""
import json
from pathlib import Path
import re
import subprocess
BASE = '0019c6b3'
ROOT = Path(__file__).resolve().parents[4]
paths = subprocess.check_output(['git','ls-tree','-r','--name-only',BASE],cwd=ROOT,text=True).splitlines()
records = []
for path in paths:
    if not re.match(r'(univariateML-cpp/include/cppunivariate/distributions/[^/]+/[^/]+\.hpp|bindings/common/distributions/[^/]+/fit.hpp)$',path):
        continue
    source = subprocess.check_output(['git','show',f'{BASE}:{path}'],cwd=ROOT,text=True)
    for line_no,line in enumerate(source.splitlines(),1):
        if re.search(r'accuracy_failure|AccuracyFailure|!accurate|accurate\s*=.*(?:polish|accuracy|gamma_fit)|!.*(?:local_accuracy|::(?:nb_|pair_)?polish|transport_accuracy|fallback_accuracy)',line):
            records.append({'path':path,'line':line_no,'source':line.strip()})
output = Path(__file__).with_name('inventory.json')
output.write_text(json.dumps({'baseline':BASE,'sites':records},indent=2)+'\n')
print(f'{len(records)} source sites; {output}')
