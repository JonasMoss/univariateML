"""Try near-binomial frequency tables with an unusually small positive face score."""
import json
from pathlib import Path
import subprocess

root = Path("build/snapshots/render-fu")
tables = []
for n in [37, 51, 91, 127, 151, 211, 257, 401, 509, 809, 1009, 2003, 5003]:
    best = []
    for total in range(n, 3*n//2):
        moment_floor = (2*total*total)//(3*n) + total
        sq = moment_floor+1
        sq += (total-sq)%2
        delta = 3*n*sq-2*total*total-3*n*total
        pairs = (sq-total)//2
        prob = total/(3*n)
        tops = round(n*prob**3)
        counts = [n-total+pairs-tops, total-2*pairs+3*tops, pairs-3*tops, tops]
        if min(counts)>0:
            best.append((delta, counts))
    tables.extend(sorted(best)[:2])
inputs = [
    f"1 {sum(counts)} 3 0 0 " + " ".join(str(i) for i, count in enumerate(counts) for _ in range(count))
    for _,counts in tables
]
for tag in ("before-native", "after-native", "before-double", "after-double"):
    path = root/tag/"build/bbinom"
    with subprocess.Popen([str(path)], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True) as p:
        for (delta, counts),data in zip(tables,inputs):
            p.stdin.write(data+"\n")
            p.stdin.flush()
            row = json.loads(p.stdout.readline())
            print(tag, delta, counts, row["core_status"], row["outcome"],
                  len(row["candidates"]), row["estimates"], flush=True)
        p.stdin.close()
        assert p.wait()==0
