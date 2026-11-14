// Requires the workstation's `just js-build` artifact; records existing base-model payloads.
import create from '../../../../build/js/univariateML.mjs';
import {writeFileSync} from 'node:fs';
const m=await create();
const cases=[
 {family:'fpareto',data:Array(8).fill(4),held:{min:2,shape2:2,shape3:2}},
 {family:'pareto2',data:Array.from({length:10},(_,i)=>i+3),held:{min:2}}
];
const rows=cases.map(c=>{
 const r=m['fit'+c.family](Float64Array.from(c.data),c.held,{});
 return {...c,kind:r.kind,model:r.model,estimates:r.estimates,limit_loglik:r.limit_loglik};
});
writeFileSync(new URL('./shifted-limit-payloads.json',import.meta.url),JSON.stringify(rows,null,2)+'\n');
