// Capture every default WASM fit on the frozen data, independently of assertions.
import {readFileSync} from 'node:fs';
import create from '../../../../build/js/univariateML.mjs';
const m=await create();
const cases=JSON.parse(readFileSync(new URL('../../../../build/js/generated/fit-cases.json',import.meta.url)),(_,v)=>v==='Inf'?Infinity:v==='-Inf'?-Infinity:v);
for(const c of cases.filter(c=>'lomax kumar weibull gumbel fatigue cmb pig foldnorm'.split(' ').includes(c.id))) {
 try {
  const r=m['fit'+c.id](new Float64Array(c.g.data),c.held,c.control);
  console.log(JSON.stringify({id:c.id,estimates:r.estimates,logLik:r.logLik,error:r.error||''},(_,v)=>v===Infinity?'Inf':v===-Infinity?'-Inf':v));
 } catch(e) {console.log(JSON.stringify({id:c.id,error:String(e)}));}
}
