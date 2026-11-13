// Test-only coordinate maps for retained modifier regression assertions.
const routes={tbinom:['binom'],tnorm:['norm'],tpois:['pois'],zipf:['zeta'],
  zib:['binom','inflated'],zinb:['nbinom','inflated'],zip:['pois','inflated'],
  zmbinom:['binom','modified'],zmgeom:['geom','modified'],zmlogarithmic:['lgser','modified'],
  zmnbinom:['nbinom','modified'],zmpois:['pois','modified'],ztbinom:['binom','truncated'],
  ztgeom:['geom','truncated'],ztnbinom:['nbinom','truncated'],ztpois:['pois','truncated']};
export function modifierRoute(api,id) {
  if(!routes[id])return api[id];
  const [base,zero]=routes[id];
  const options=(h,x)=>zero?{zero}: {truncate:id==='zipf'?[1,h.N??NaN]:
    id==='tnorm'?[h.a??NaN,h.b??NaN]:[h.a==null?NaN:Math.max(0,Math.floor(h.a)+1),h.b==null?NaN:Math.floor(h.b)]};
  const names={tbinom:['prob','a','b','size'],tnorm:['mean','sd','a','b'],tpois:['lambda','a','b'],zipf:['N','s'],
    zib:['size','prob','pi'],zinb:['size','prob','pi'],zip:['lambda','pi'],zmbinom:['size','prob','p0'],
    zmgeom:['prob','p0'],zmlogarithmic:['prob','p0'],zmnbinom:['size','prob','p0'],zmpois:['lambda','p0'],
    ztbinom:['size','prob'],ztgeom:['prob'],ztnbinom:['size','prob'],ztpois:['lambda']}[id];
  const holds=h=>Object.fromEntries(Object.entries(h).filter(([k])=>!['a','b','N'].includes(k)).map(([k,v])=>[id==='zmlogarithmic'&&k==='prob'?'theta':k,v]));
  const out={fit(x,h={},c={}) {return api[base].fit(x,holds(h),{...c,...options(h,x)});}};
  for(const k of ['d','p','q','r'])out[k]=(x,p,flags={})=>{
    const h=Object.fromEntries(names.map((name,i)=>[name,p[i]]));
    const values=holds(h);const params=Object.values(values);
    if(id==='tbinom')params.splice(0,params.length,h.size,h.prob);
    return api[base][k](x,params,{...flags,...options(h)});
  };
  return out;
}
