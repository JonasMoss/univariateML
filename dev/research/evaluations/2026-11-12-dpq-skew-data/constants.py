"""Reproduce three-word constants; no reference table values are rewritten."""
from pathlib import Path
import mpmath as m
m.mp.dps=100
p=Path('univariateML-cpp/include/cppunivariate/special/three_word.hpp')
def words(v):
 a=float(v);v-=a;b=float(v);v-=b;c=float(v);return '{'+','.join(map(repr,(a,b,c)))+'}'
import io
h=io.StringIO()
if True:
 for name,v in [('log_two',m.log(2)),('log_sqrt_two_pi',m.log(2*m.pi)/2),('log_pi',m.log(m.pi))]:h.write(f'inline Number {name}() {{return {words(v)};}}\n')
 h.write('inline Number stirling(Number x) {\n const std::array<Number,20> c{{\n')
 h.write(',\n'.join(words(m.bernpoly(2*k,0)/(2*k*(2*k-1))) for k in range(1,21)))
 h.write('\n }};\n const auto t=divide({1},x),t2=multiply(t,t);auto v=c.back();\n for(int i=18;i>=0;--i)v=add(c[i],multiply(t2,v));\n return multiply(t,v);\n}\n')

assert h.getvalue() in p.read_text(), "three-word constants differ from 100-digit expansion"
print("23 three-word constants match independent 100-digit expansions")
