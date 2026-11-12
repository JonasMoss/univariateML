import {readFileSync,writeFileSync} from 'node:fs';
import create from '../../../../build/js/univariateML.mjs';
const m=await create();
const rows=JSON.parse(readFileSync(new URL('consumer-jobs.json',import.meta.url),'utf8'),(_,v)=>['Infinity','Inf'].includes(v)?Infinity:['-Infinity','-Inf'].includes(v)?-Infinity:v==='NaN'?NaN:v);
let failures=[],recovered={};let worst=0;
for(const r of rows){const [f,op,x,lo,lg,p]=r.job;const a=m[['d','p','q'][op]+f](new Float64Array([x]),p,!!lo,!!lg)[0],b=r.expected;
const relative=op===2||(op===1&&!lg),e=a===b?0:Number.isFinite(a)&&Number.isFinite(b)?Math.abs(a-b)/(relative&&b?Math.abs(b):Math.max(1,Math.abs(b))):Infinity;
if(e>(op===2?1e-9:1e-10))failures.push({...r,actual:a,error:e});else {worst=Math.max(worst,e);if(r.was_nan)recovered[f]=(recovered[f]??0)+1;}}
const result={checks:rows.length,failures,recovered_witnesses:recovered,worst_pass:worst};
writeFileSync(new URL('final-js.json',import.meta.url),JSON.stringify(result,(_,v)=>typeof v==='number'&&!Number.isFinite(v)?String(v):v,2)+'\n');
console.log(JSON.stringify({checks:rows.length,failures:failures.length,recovered_witnesses:recovered,worst_pass:worst}));
if(failures.length)process.exitCode=1;
