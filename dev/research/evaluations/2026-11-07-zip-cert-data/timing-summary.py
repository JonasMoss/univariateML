import json,subprocess,statistics,sys
samples={name:[] for name in ['before','after']}
for _ in range(7):
    for name,binary in zip(samples,sys.argv[1:]):
        samples[name].append([float(line.split()[1]) for line in subprocess.check_output([binary],text=True).splitlines()])
print(json.dumps(dict(workload='99 zeros and one count ten; 10000 fits per cell; 7 alternating runs; microseconds/fit',pi=['free',.001,.1,.9],samples=samples,medians={name:[statistics.median(row[i] for row in runs) for i in range(4)] for name,runs in samples.items()}),indent=2))
