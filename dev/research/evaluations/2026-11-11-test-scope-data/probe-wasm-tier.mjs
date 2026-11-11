// Isolated performance probe; the unchanged 615 tests remain the correctness gate.
import create from '../../../../build/js/univariateML.mjs';
import {readFileSync} from 'node:fs';
const cases=JSON.parse(readFileSync(new URL('../../../../build/js/generated/fit-cases.json',import.meta.url)),(_,v)=>v==='Inf'||v==='Infinity'?Infinity:v==='-Inf'||v==='-Infinity'?-Infinity:v==='NaN'?NaN:v);
const c=cases.find(c=>c.id==='tlambda');
const start=performance.now();
const m=await create();
console.log('module_seconds',(performance.now()-start)/1000);
const names=Object.keys(c.g.estimates),data=new Float64Array(c.g.data);
for(let mask=0;mask<1<<names.length;mask++) {
 const held=Object.fromEntries(names.filter((_,i)=>mask>>i&1).map(k=>[k,c.g.estimates[k]]));
 const before=performance.now(),r=m.fittlambda(data,held,c.control);
 console.log(JSON.stringify({mask,seconds:(performance.now()-before)/1000,status:r.status,error:r.error}));
}
