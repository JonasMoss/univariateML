import {readFileSync} from 'node:fs';
import create from '../../../../build/js/univariateML.mjs';
const m=await create();
const cases=readFileSync(new URL('./sstd-full-tier-cases.jsonl',import.meta.url),'utf8').trim().split('\n').map(v=>JSON.parse(v));
for(const entry of cases.filter(c=>[5007,'replication'].includes(c.seed))) {
 const x=entry.seed==='replication'?Array.from({length:100},()=>entry.data).flat():entry.data;
 const r=m.fitsstd(new Float64Array(x),{},{});
 console.log(JSON.stringify({id:String(entry.seed),n:x.length,error:r.error||'',estimates:r.estimates}));
}
