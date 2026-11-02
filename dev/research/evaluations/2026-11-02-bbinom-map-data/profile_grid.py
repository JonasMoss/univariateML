#!/usr/bin/env python3
import subprocess, pathlib
p=pathlib.Path(__file__).resolve().parent
samples=[[0,1,2],[1,1,5],[0,1,2,4],[16,18,22,25,27],[16,18,22,25,28],[1,0,3,0,1,1,0,0],[1,0,3,0,1,1,0,1]]
lines=[]
for x in samples:
 for n in list(range(max(x),151))+[250,500,1000]: lines.append(f"{len(x)} {n} "+' '.join(map(str,x)))
r=subprocess.run(['build/scratch/bbinom-map-check'],input='\n'.join(lines)+'\n',text=True,check=True,capture_output=True)
(p/'profile-grid.csv').write_text('h,N,data,status,loglik,alpha,beta\n'+''.join(f"{src.split()[0]},{out.split()[0]},{'-'.join(src.split()[2:])},"+','.join(out.split()[1:])+'\n' for src,out in zip(lines,r.stdout.splitlines())))
assert len(r.stdout.splitlines())==len(lines)
print('fixed-size profiles',len(lines))
