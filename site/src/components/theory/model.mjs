// Plot grids are evaluations of existing held fits, never optimizers or MLE claims.
export const specs = {
 gamma: {title:'Gamma', coefficient:'shape', lo:.15, hi:15, params:[2,1], explanation:'Shape is held; scale is refitted at every point.'},
 lomax: {title:'Lomax', coefficient:'kappa', lo:.1, hi:10000, limit:'exp', explanation:'Kappa grows while lambda × kappa tends to the fitted exponential rate.'},
 std: {title:'Student t', coefficient:'nu', lo:2.01, hi:10000, limit:'norm', explanation:'Nu tends to infinity; mean and standard deviation are refitted. This parameterization requires nu > 2.'},
 nbinom: {title:'Negative binomial', coefficient:'size', lo:.1, hi:10000, limit:'pois', explanation:'Size grows and prob tends to one, preserving the sample mean.'},
 burr: {title:'Burr → Pareto endpoint', coefficient:'shape2', lo:1, hi:10000, limit:'pareto', held:{scale:1}, limitHeld:{b:1}, explanation:'Scale is held at 1 throughout. Shape2 grows and shape1 × shape2 tends to the Pareto shape; the limit has lower endpoint 1. This is a conditional escape.'},
};
export function preset(id) {
 if(id==='gamma') return [0.3,0.5,0.8,1.1,1.3,1.6,1.8,2.2,2.8,3.6,4.1,5.2];
 if(id==='lomax') return Array.from({length:40},(_,i)=>(i+.5)/20);
 if(id==='std') return Array.from({length:40},(_,i)=>(i-19.5)/20);
 if(id==='nbinom') return [1,2,3,2,1,2,3,2,1,2,3,2];
 return Array.from({length:40},(_,i)=>Math.pow(1-(i+.5)/40,-.5));
}
export function parseSample(text) {
 const parts=text.trim().split(/[\s,;]+/);
 if(!text.trim() || parts.length<3 || parts.length>500) throw Error('Enter 3–500 finite observations, separated by spaces or commas.');
 const data=parts.map(Number);
 if(data.some(x=>!Number.isFinite(x))) throw Error('Every observation must be a finite number.');
 return data;
}
let browserPromise;
export function browserApi() {
 const url='/binding/index.mjs';
 return browserPromise??=import(/* @vite-ignore */url).then(m=>m.default());
}
const attempt=fn=>{try{
 const fit=fn();
 if(fit.status!=='Ok'||!Number.isFinite(fit.logLik)||Object.values(fit.estimates).some(v=>!Number.isFinite(v)))throw Error('Fit returned a nonfinite likelihood or coefficient, or a failure status.');
 return {fit};
}catch(e){return {error:e.message};}};
export function profile(api,id,sample) {
 const s=specs[id],x=new Float64Array(sample),free=attempt(()=>api[id].fit(x,s.held||{}));
 const limit=s.limit?attempt(()=>api[s.limit].fit(x,s.limitHeld||{})):null;
 const points=Array.from({length:65},(_,i)=>{
  const value=Math.exp(Math.log(s.lo)+i/64*Math.log(s.hi/s.lo));
  const r=attempt(()=>api[id].fit(x,{...s.held,[s.coefficient]:value}));
  return {x:value,y:r.fit?.logLik??null,error:r.error};
 });
 return {id,sample:[...sample],free,limit,points};
}
const f=n=>Number(n).toFixed(2), esc=s=>String(s).replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
// Interior ticks use decimal 1, 2, 5 steps; log ticks use the same mantissas.
export function niceTicks(lo,hi,log=false) {
 if(log){const out=[];for(let k=Math.floor(Math.log10(lo));k<=Math.ceil(Math.log10(hi));k++)for(const m of [1,2,5]){const v=m*10**k;if(v>=lo*(1-1e-12)&&v<=hi*(1+1e-12))out.push(v);}return out.filter((v,i)=>out.length<=9||i%2===0);}
 const raw=(hi-lo)/5,p=10**Math.floor(Math.log10(raw)),step=[1,2,5,10].find(m=>m*p>=raw)*p;
 return Array.from({length:Math.max(0,Math.floor(hi/step+1e-10)-Math.ceil(lo/step-1e-10)+1)},(_,i)=>Number(((Math.ceil(lo/step-1e-10)+i)*step).toPrecision(12)));
}
const tickLabel=n=>String(Number(n.toPrecision(6)));
export function profileSvg(result) {
 const {id,points,free,limit}=result,s=specs[id],vals=points.map(p=>p.y).filter(Number.isFinite);
 if(!vals.length) return '<p role="status">No successful held fits on this grid. See diagnostics below.</p>';
 const ref=limit?.fit?.logLik, mle=free.fit?.logLik;
 const top=Math.max(...vals,...[ref,mle].filter(Number.isFinite)),bottom=Math.min(...vals),pad=Math.max(.1,(top-bottom)*.07);
 const ylo=bottom-pad,yhi=top+pad,X=x=>92+528*Math.log(x/s.lo)/Math.log(s.hi/s.lo),Y=y=>260-225*(y-ylo)/(yhi-ylo);
 let d='',gap=true;for(const p of points){if(!Number.isFinite(p.y)){gap=true;continue;}d+=`${gap?'M':'L'}${f(X(p.x))},${f(Y(p.y))} `;gap=false;}
 let svg=`<svg data-profile="${id}" viewBox="0 0 660 325" role="img" aria-label="${esc(s.title)} profile log-likelihood, logarithmic coefficient axis"><title>${esc(s.title)} profile log-likelihood</title><rect width="660" height="325" fill="#fffdf8"/>`;
 for(const y of niceTicks(ylo,yhi))svg+=`<path d="M92 ${f(Y(y))}H620" stroke="#e2e1d8"/><text x="84" y="${f(Y(y)+4)}" text-anchor="end">${tickLabel(y)}</text>`;
 for(const x of niceTicks(s.lo,s.hi,true))svg+=`<text x="${f(X(x))}" y="282" text-anchor="middle">${tickLabel(x)}</text>`;
 if(Number.isFinite(ref))svg+=`<path data-limit-line d="M92 ${f(Y(ref))}H620" stroke="#a65330" stroke-width="2" stroke-dasharray="7 5"/>`;
 svg+=`<path data-profile-curve d="${d}" fill="none" stroke="#23645e" stroke-width="3"/>`;
 const hat=free.fit?.estimates[s.coefficient];if(Number.isFinite(mle)&&hat>=s.lo&&hat<=s.hi)svg+=`<circle data-mle cx="${f(X(hat))}" cy="${f(Y(mle))}" r="5" fill="#262b2b"/>`;
 return svg+`<text x="356" y="313" text-anchor="middle">${({kappa:"κ",nu:"ν",shape:"k",shape2:"k₂",size:"size"})[s.coefficient]} (log scale)</text><text x="18" y="148" transform="rotate(-90 18 148)" text-anchor="middle">log-likelihood</text></svg>`;
}
export function faces(api,sample,heldMean=0,heldSd=1) {
 const x=new Float64Array(sample),free=api.norm.fit(x),meanFit=api.norm.fit(x,{mean:heldMean}),sdFit=api.norm.fit(x,{sd:heldSd});
 const m=free.estimates.mean,s=free.estimates.sd;
 const xmin=Math.min(m-2.5*s,heldMean-.3*s),xmax=Math.max(m+2.5*s,heldMean+.3*s),ymin=Math.min(.3*s,.8*heldSd),ymax=Math.max(2.5*s,1.2*heldSd,1.2*meanFit.estimates.sd);
 const nx=65,ny=55,grid=Array.from({length:ny},(_,j)=>Array.from({length:nx},(_,i)=>Array.from(api.norm.d(x,[xmin+(xmax-xmin)*i/(nx-1),ymin+(ymax-ymin)*j/(ny-1)],{log:true})).reduce((a,b)=>a+b,0)));
 return {sample:[...sample],heldMean,heldSd,free,meanFit,sdFit,xmin,xmax,ymin,ymax,nx,ny,grid};
}
export const faceSample=[-1.1,-.2,.1,.4,.8,1,1.2,1.6,2,2.4,2.8,3.1];
export function facesSvg(r) {
 const X=x=>92+528*(x-r.xmin)/(r.xmax-r.xmin),Y=y=>260-225*(y-r.ymin)/(r.ymax-r.ymin);
 let svg='<svg data-faces viewBox="0 0 660 325" role="img" aria-label="Normal log-likelihood contours with held mean and held standard deviation faces"><title>Normal likelihood contours and held fits</title><rect width="660" height="325" fill="#fffdf8"/>';
 // Marching squares; disjoint segments avoid interpolating across grid cells.
 for(const drop of [.5,2,5,10,20]){const level=r.free.logLik-drop;let d='';
  for(let j=0;j<r.ny-1;j++)for(let i=0;i<r.nx-1;i++){
   const corners=[[i,j],[i+1,j],[i+1,j+1],[i,j+1]],hits=[];
   for(let k=0;k<4;k++){const a=corners[k],b=corners[(k+1)%4],va=r.grid[a[1]][a[0]],vb=r.grid[b[1]][b[0]];
    if((va<level)!==(vb<level)){const t=(level-va)/(vb-va);hits.push([92+528*(a[0]+t*(b[0]-a[0]))/(r.nx-1),260-225*(a[1]+t*(b[1]-a[1]))/(r.ny-1)]);}}
   for(let k=0;k+1<hits.length;k+=2)d+=`M${f(hits[k][0])},${f(hits[k][1])}L${f(hits[k+1][0])},${f(hits[k+1][1])}`;
  }svg+=`<path data-contour="${drop}" d="${d}" fill="none" stroke="#23645e" stroke-opacity="${1-drop/30}" stroke-width="1.5"/>`;
 }
 svg+=`<path data-mean-face d="M${f(X(r.heldMean))} 35V260" stroke="#a65330" stroke-dasharray="6 4" stroke-width="2"/><path data-sd-face d="M92 ${f(Y(r.heldSd))}H620" stroke="#6553a0" stroke-dasharray="6 4" stroke-width="2"/>`;
 for(const [name,fit,color] of [['free',r.free,'#262b2b'],['mean held',r.meanFit,'#a65330'],['sd held',r.sdFit,'#6553a0']])svg+=`<circle data-fit="${name}" cx="${f(X(fit.estimates.mean))}" cy="${f(Y(fit.estimates.sd))}" r="5" fill="${color}"><title>${name}: mean ${f(fit.estimates.mean)}, sd ${f(fit.estimates.sd)}</title></circle>`;
 for(const x of niceTicks(r.xmin,r.xmax))svg+=`<text x="${f(X(x))}" y="282" text-anchor="middle">${tickLabel(x)}</text>`;
 for(const y of niceTicks(r.ymin,r.ymax))svg+=`<text x="84" y="${f(Y(y)+4)}" text-anchor="end">${tickLabel(y)}</text>`;
 return svg+'<text x="356" y="313" text-anchor="middle">mean</text><text x="18" y="148" transform="rotate(-90 18 148)" text-anchor="middle">standard deviation</text></svg>';
}

export function conditional(api) {
 const sample=[.2,.4,.6,.8,1,1.2,1.4,1.6],x=new Float64Array(sample),free=attempt(()=>api.gompertz.fit(x));
 const held=[.2,1,2].map(a=>({a,...attempt(()=>api.gompertz.fit(x,{a}))}));
 const xmax=Math.max(2,...[free,...held].filter(r=>r.fit).map(r=>1.15*r.fit.estimates.b));
 for(const h of held)h.points=Array.from({length:81},(_,i)=>{
  const b=xmax*i/80;
  return {x:b,y:b===0?sample.length*Math.log(h.a)-h.a*sample.reduce((s,v)=>s+v,0):Array.from(api.gompertz.d(x,[h.a,b],{log:true})).reduce((s,v)=>s+v,0)};
 });
 return {sample,free,held,xmax};
}
export function conditionalSvg(r) {
 const lo=Math.min(...r.held.flatMap(h=>h.points.map(p=>p.y))),hi=r.free.fit.logLik+1;
 const X=x=>92+528*x/r.xmax,Y=y=>260-225*(y-lo)/(hi-lo),colors=['#23645e','#6553a0','#a65330'];
 let svg='<svg data-conditional viewBox="0 0 660 325" role="img" aria-label="Gompertz held-rate likelihoods; the rate 2 face escapes to b equals zero"><title>Gompertz held-rate faces</title><rect width="660" height="325" fill="#fffdf8"/>';
 for(const y of niceTicks(lo,hi))svg+=`<path d="M92 ${f(Y(y))}H620" stroke="#e2e1d8"/><text x="84" y="${f(Y(y)+4)}" text-anchor="end">${tickLabel(y)}</text>`;
 for(const x of niceTicks(0,r.xmax))svg+=`<text x="${f(X(x))}" y="282" text-anchor="middle">${tickLabel(x)}</text>`;
 r.held.forEach((h,i)=>{svg+=`<path d="${h.points.map((p,j)=>`${j?'L':'M'}${f(X(p.x))},${f(Y(p.y))}`).join(' ')}" fill="none" stroke="${colors[i]}" stroke-width="2"/>`;const p=h.fit?{x:h.fit.estimates.b,y:h.fit.logLik}:h.points[0];svg+=`<circle data-held="${h.a}" cx="${f(X(p.x))}" cy="${f(Y(p.y))}" r="5" fill="${h.fit?colors[i]:'#fffdf8'}" stroke="${colors[i]}" stroke-width="2"/>`;});
 const fit=r.free.fit;svg+=`<circle data-free cx="${f(X(fit.estimates.b))}" cy="${f(Y(fit.logLik))}" r="5" fill="#262b2b"/>`;
 return svg+'<text x="356" y="313" text-anchor="middle">b</text><text x="18" y="148" transform="rotate(-90 18 148)" text-anchor="middle">log-likelihood</text></svg>';
}
