import {readFileSync} from 'node:fs';
const {default:create}=await import(new URL('../../../../build/js/univariateML.mjs',import.meta.url));
const m=await create();
const cases=JSON.parse(readFileSync(new URL('./spot-cases.json',import.meta.url)));
for(const c of cases) {
 try {
  const r=m['fit'+c.family](new Float64Array(c.data),c.held,c.control);
  console.log(JSON.stringify({id:c.index,family:c.family,...r}));
 } catch(e) {console.log(JSON.stringify({id:c.index,family:c.family,error:String(e)}));}
}
