import {readFileSync} from 'node:fs';
import create from '../../../../build/js/umvl-t-preflight.mjs';
const api=await create();
const read=name=>JSON.parse(readFileSync(new URL(name,import.meta.url),'utf8'));
const selected=new Set(read('spot-indices.json'));
const controls={std:'gradtol',lst:'gradtol',sstd:'gradtol',sged:'gradtol',st:'reltol',slash:'reltol'};
for(const c of read('cases.json'))if(selected.has(c.index)){
 try{
  const r=api['fit'+c.family](new Float64Array(c.data),c.held,{[controls[c.family]]:1e-10});
  console.log(JSON.stringify({index:c.index,error:r.error||'',estimates:r.estimates||{},certified:r.certified??null}));
 }catch(e){console.log(JSON.stringify({index:c.index,error:e.message,estimates:{}}));}
}
