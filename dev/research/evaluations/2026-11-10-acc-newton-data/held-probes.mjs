import {readFileSync} from 'node:fs';
import create from '../../../../bindings/js/index.mjs';
const api=await create(),cases=JSON.parse(readFileSync(new URL('./held-cases.json',import.meta.url)));
for(const c of cases) {
 const x=c.data.map(v=>v*(c.unit==='denorm'?Number.MIN_VALUE:1)),held={...c.held};
 for(const name of c.unit_params??[])held[name]*=Number.MIN_VALUE;
 try {const r=api[c.family].fit(x,held);console.log(JSON.stringify({id:c.id,estimates:r.estimates,error:'',kind:''}));}
 catch(e){console.log(JSON.stringify({id:c.id,estimates:{},error:e.message,kind:e.kind??'error'}));}
}
