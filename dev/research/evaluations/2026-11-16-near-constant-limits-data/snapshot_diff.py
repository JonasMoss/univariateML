"""Diff of two fit snapshots for a set of families: python3 snapshot_diff.py OLD NEW fam1,fam2 (run from the repository root)."""
import sys,json
sys.path.insert(0,'dev/validation')
import strict_common as s
fams=set(sys.argv[3].split(','))
old=s.load(sys.argv[1]); new=s.load(sys.argv[2])
old={k:v for k,v in old.items() if k[0] in fams}; new={k:v for k,v in new.items() if k[0] in fams}
print('cases old',len(old),'new',len(new),'only old',len(set(old)-set(new)),'only new',len(set(new)-set(old)))
n=0
for k in sorted(set(old)&set(new)):
    a,b=old[k]['result'],new[k]['result']
    f=[x for x in sorted(set(a)|set(b)) if a.get(x)!=b.get(x) and x not in('message',)]
    if f:
        n+=1
        print(k,'|',a['outcome'].get('kind'),a.get('model'),a.get('certified'),'->',b['outcome'].get('kind'),b.get('model'),b.get('certified'),'| fields',f)
print('changed',n)
