const {default:create}=await import(process.env.UMVL_CAPTURE_MODULE || new URL('../../../../build/js/univariateML.mjs',import.meta.url));
const m=await create(),x=new Float64Array(10000000).fill(.999999);
const c=-Math.log(x[0]),reference=(2-c+Math.sqrt(c*c+4))/(2*c);
const r=m.fitgenbeta(x,{scale:1,shape2:2,shape3:1},{});
const relative_error=Math.abs(r.estimates.shape1/reference-1);
console.log(JSON.stringify({n:x.length,x:x[0],status:r.status,shape1:r.estimates.shape1,reference,relative_error,error:r.error||''}));
if(r.error||r.status!=='Ok'||!Number.isFinite(relative_error)||relative_error>1e-10)process.exitCode=1;
