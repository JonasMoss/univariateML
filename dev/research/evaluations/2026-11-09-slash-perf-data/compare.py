#!/usr/bin/env python3
"""Check every paired result and count; select median timing rows per subset.
Run from any directory after run_benchmarks.py output is in paired-simbox.csv.
The first two columns there are variant and repeat; benchmark.py documents
remaining columns. Selected before/after CSVs keep the raw benchmark schema.
"""
import csv
import pathlib
import statistics

p=pathlib.Path(__file__).parent
raw=list(csv.reader((p/'paired-simbox.csv').open()))
assert len(raw) in (64,88),len(raw)
for sample in ('golden500','golden10000','cauchy500','cauchy10000'):
    variants={}
    for variant in ('baseline','after'):
        rows=[r[2:] for r in raw if r[0]==variant and pathlib.Path(r[2]).stem==sample]
        repeats=3 if sample.endswith('500') else 1
        expected=4*repeats if len(raw)==64 or sample.endswith('500') else 10
        assert len(rows)==expected,(sample,variant,len(rows))
        variants[variant]=rows
    selected={v:[] for v in variants}
    for mask in range(4):
        groups={v:[r for r in rows if int(r[1])==mask] for v,rows in variants.items()}
        repetitions=3 if sample.endswith('500') or (len(raw)==88 and mask>0) else 1
        assert all(len(rows)==repetitions for rows in groups.values())
        signature=groups['baseline'][0][3:13]
        depth=groups['baseline'][0][16]
        for v,rows in groups.items():
            for row in rows:
                assert row[3:13]==signature,(sample,mask,v,row,signature)
                assert row[16]==depth,(sample,mask,v,row[16],depth)
            selected[v].append(sorted(rows,key=lambda row:float(row[2]))[len(rows)//2])
        b=statistics.median(float(r[2]) for r in groups['baseline'])
        a=statistics.median(float(r[2]) for r in groups['after'])
        print(sample,'mask',mask,f'{b:.6g} -> {a:.6g}s',f'{b/a:.3f}x','exact results/counts/depth')
    for v,rows in selected.items():
        with (p/f'{v}-{sample}.csv').open('w') as f:
            csv.writer(f).writerows(rows)
print(f'All {len(raw)//2} paired fits match exactly, including every timing repetition ({len(raw)} rows).')
