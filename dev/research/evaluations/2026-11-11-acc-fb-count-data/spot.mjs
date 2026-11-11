import fs from 'node:fs';
import create from '../../../../bindings/js/index.mjs';
const P=new URL('.',import.meta.url);
const cases=JSON.parse(fs.readFileSync(new URL('cases.json',P)));
const api=await create();
const rows=[];
for(const i of [927,90,209,317,733,1068,629,1076,1078,1245,1247,1250,1252,1253,1254,1255,1257,1258,1259,1260,1261,1263,1264,1265,1266]) {
 const c=cases[i];const f=api[c.family].fit(c.data,c.held,{[c.family==='bnbinom'?'gradtol':'reltol']:c.tol??1e-10,iterlim:c.maxit??(c.family==='bbinom'?500:200)});
 rows.push({index:i,family:c.family,...f});
}
fs.writeFileSync(new URL('wasm-spots.json',P),JSON.stringify(rows,null,2)+'\n');
