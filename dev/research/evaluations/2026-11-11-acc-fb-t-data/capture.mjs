import {readFileSync} from 'node:fs';
import create from '../../../../bindings/js/index.mjs';
const api=await create();
const data=name=>JSON.parse(readFileSync(new URL(name,import.meta.url),'utf8'));
const selected=new Set(data('spot-indices.json'));
for(const c of data('cases.json'))if(selected.has(c.index)){
 try{
  const r=api[c.family].fit(new Float64Array(c.data),c.held,{[({std:"gradtol",lst:"gradtol",sstd:"gradtol",sged:"gradtol",st:"reltol",slash:"reltol"})[c.family]]:1e-10});
  console.log(JSON.stringify({index:c.index,error:'',estimates:r.estimates,certified:r.certified??null}));
 }catch(e){console.log(JSON.stringify({index:c.index,error:e.message,estimates:{}}));}
}
