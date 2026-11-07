"""Finite counterexample to the unscaled extremum-coordinate products.
The appended C++ regression checks the repaired library range directly.
"""
import json, math, pathlib
c,shift=1e155,8e153
h=math.hypot(c,shift)
old=[c*shift/(c+h),-c*(c+h)/shift]
scaled=[c*((shift/h)/(1+c/h)),-c*((1+c/h)/(shift/h))]
def value(z):
    t=z/c
    return t*(t-shift/c)/(1+t*t)
assert all(math.isinf(x) for x in old)
assert all(math.isfinite(x) for x in scaled)
old_positive_upper,old_positive_lower=0.,0.
probe=value(4e153)
assert probe<old_positive_lower
result=dict(c=c,shift=shift,unscaled_roots=[str(x) for x in old],scaled_roots=scaled,
            old_positive_range=[0,0],positive_probe=probe,
            corrected_positive_lower=value(scaled[0]),negative_probe=value(-2.5e156),
            corrected_negative_upper=value(scaled[1]))
pathlib.Path(__file__).with_suffix('.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result))
