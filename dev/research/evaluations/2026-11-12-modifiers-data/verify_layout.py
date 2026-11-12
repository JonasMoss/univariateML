#!/usr/bin/env python3
"""Prove that the extracted legacy helpers differ only in include paths."""
from pathlib import Path
import subprocess
ROOT=Path(__file__).resolve().parents[4]
PREFIX='univariateML-cpp/include/cppunivariate/'
BASELINE='bb460787'
for old,new in [('distributions/ztpois/profile.hpp','core/zero_modified.hpp'),
                ('distributions/zmpois/profile.hpp','core/zero_modified_fit.hpp')]:
    text=subprocess.check_output(['git','show',BASELINE+':'+PREFIX+old],cwd=ROOT,text=True)
    text=text.replace('"../../core/','"').replace('"../../special/','"../special/')
    for id in ['lgser','binom','ztpois','ztgeom','ztnbinom','ztbinom']:
        text=text.replace('"../'+id+'/','"../distributions/'+id+'/')
    assert text==(ROOT/PREFIX/new).read_text(),new
print('Both extractions match baseline '+BASELINE+' after include-path substitution only.')
