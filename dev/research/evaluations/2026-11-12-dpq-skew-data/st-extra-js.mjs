import {readFileSync,writeFileSync} from 'node:fs';
import create from '../../../../build/js/univariateML.mjs';
const m=await create();
const small=JSON.parse(readFileSync(new URL('st-small-cdf-after.json',import.meta.url),'utf8'));
const normal=JSON.parse(readFileSync(new URL('st-normal-face-after.json',import.meta.url),'utf8'));
const results=[];
for(const r of [small,...normal]){
 const [f,op,x,lo,lg,p]=r.job;
 const actual=m.pst(new Float64Array([x]),p,true,false)[0];
 const error=Math.abs(actual/r.reference-1);
 results.push({job:r.job,reference:r.reference,actual,error});
 if(!(error<1e-10))throw Error(`ST CDF error ${error}`);
}
const q=m.qst(new Float64Array([small.reference]),small.job[5],true,false)[0];
const qe=Math.abs(q/small.job[2]-1);
results.push({function:'qst',reference:small.job[2],actual:q,error:qe});
if(!(qe<1e-9))throw Error(`ST quantile error ${qe}`);
writeFileSync(new URL('st-extra-js.json',import.meta.url),JSON.stringify({checks:results.length,failures:[],results},null,2)+'\n');
console.log('Supplementary ST WASM checks green',results.length);
