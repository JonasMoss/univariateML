import sys, json
sys.path.insert(0,'dev/validation')
import strict_common as sc
D='dev/research/evaluations/2026-11-15-fit-snapshot-data/'
fams='burr emg expweibull fpareto gengamma huber invburr lst pareto4 sn std trgamma triang'.split()
for mode in ('native','double'):
    rows=sc.load(D+mode+'.jsonl.gz')
    for f in fams:
        for c in ('tie3','near-constant'):
            r=rows[(f,'class',c)]; x=r['result']
            est={k:sc.number(v) for k,v in x.get('estimates',{}).items()}
            lim={k:sc.number(v) for k,v in x.get('limit_estimates',{}).items()}
            print(mode,f,c,'held',r['held'],'data',r['data'],'kind',x['kind'],'model',x.get('model'),'reason',x.get('reason'),'core',x.get('core_status'),'est',est,'lim',lim,'msg',x.get('message'),'ll',x.get('logLik'))
