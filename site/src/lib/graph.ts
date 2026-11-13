// Pure layout: sorted inputs, integer coordinates, no browser measurement or randomness.
export const kinds = {special_case_of:'Special case', transform_of:'Transform', truncation_of:'Truncation', modification_of:'Modification', convolution_of:'Convolution', mixture_of:'Mixture', discretisation_of:'Discretisation', limit:'Limit'};
export const colors = ['#23645e','#854d74','#8a531e','#365e9d','#7b5928','#974337','#526c28','#66589a','#237285','#9a4160','#536634','#69594c','#335b70'];
export function graphData(families) {
 const nodes=[...families].sort((a,b)=>a.id.localeCompare(b.id,'en'));
 const groups=[...new Set(nodes.map(f=>f.group))].sort();
 const edges=nodes.flatMap(f=>[
  ...Object.entries(f.relations||{}).filter(([kind])=>kind!=='closure_point').flatMap(([kind,es])=>es.flatMap((e,i)=>[...new Set(kind==='convolution_of'?e.components.map(c=>c.family):kind==='mixture_of'?[e.family,e.mixing.family]:[e.family])].map((target,j)=>({id:`${f.id}-${kind}-${i}-${j}`,source:f.id,target,kind,conditional:!!e.when,note:JSON.stringify(e,(key,value)=>key==='kernel'?undefined:value)})))),
  ...(f.limits||[]).flatMap((e,i)=>e.model?[{id:`${f.id}-limit-${i}`,source:f.id,target:e.model,kind:'limit',note:e.when}]:[])
 ]);
 return {nodes:nodes.map(({id,name,group})=>({id,name,group})),groups,edges};
}
const esc=s=>String(s).replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
export function atlasSvg(data,group,limits=false) {
 const families=data.nodes.filter(f=>f.group===group), color=colors[data.groups.indexOf(group)%colors.length];
 let y=30; const body=[];
 for(const f of families){
  const edges=data.edges.filter(e=>e.source===f.id&&(e.kind==='limit')===limits);
  if(limits&&!edges.length)continue;
  const top=y;
  const name=f.name;
  body.push(`<a href="/distributions/${f.id}/" ${limits?'':`data-node="${f.id}"`} data-family="${f.id}" aria-label="${esc(f.name)}"><title>${esc(f.name)} (${f.id})</title><rect x="12" y="${top-19}" width="326" height="39" rx="4" fill="${color}" fill-opacity=".08" stroke="${color}"/><text x="23" y="${top+2}" fill="${color}">${esc(name.length>38?name.slice(0,name.lastIndexOf(' ',38)):name)}${name.length>38?`<tspan x="23" dy="15">${esc(name.slice(name.lastIndexOf(' ',38)+1))}</tspan>`:''}</text></a>`);
  y+=48;
  for(const e of edges){
   const target=data.nodes.find(n=>n.id===e.target);
   const split=target.name.length>35?target.name.lastIndexOf(' ',35):-1;
   const targetLabel=split>0?`${esc(target.name.slice(0,split))}<tspan x="59" dy="16">${esc(target.name.slice(split+1))}</tspan>`:esc(target.name);
   body.push(`<g data-edge="${e.id}" data-source="${e.source}" data-target="${e.target}" data-kind="${e.kind}"><title>${esc(e.note)}</title><path d="M 27 ${top+21} V ${y-4} H 46" fill="none" stroke="${color}" stroke-width="1.5" stroke-dasharray="${edgeDash(e.kind,e.conditional)}" marker-end="url(#arrow-${data.groups.indexOf(group)}-${limits})"/><a href="/distributions/${e.target}/" data-family="${e.target}"><text x="59" y="${y}" fill="#263b38">${targetLabel}</text><text x="59" y="${y+(split>0?33:17)}" font-size="11" fill="#626761">${esc(kinds[e.kind])} · ${esc(e.target)}</text></a></g>`);
   y+=split>0?59:43;
  }
  if(!edges.length){body.push(`<text x="25" y="${y+1}" fill="#71756d" font-size="12">No declared ${limits?'named limits':'relations'} →</text>`);y+=32;}
  y=Math.max(y,top+43)+12;
 }
 return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 360 ${y}" width="360" height="${y}" role="group" aria-label="${esc(group)}: ${limits?'limit models':'relations'}, arrows point to parents or limit models"><defs><marker id="arrow-${data.groups.indexOf(group)}-${limits}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="${color}"/></marker></defs>${body.join('')}</svg>`;
}

export const edgeDash = (kind,conditional=false) => conditional?'1 4':({convolution_of:'10 3',mixture_of:'4 2 1 2',discretisation_of:'3 3',transform_of:'6 3',truncation_of:'1 3',modification_of:'8 3 2 3',limit:'2 4'}[kind]||'');
// Discrete, bounded optimisation. Fixed slots make label separation an invariant.
export function mapLayout(data) {
 const relations=data.edges.filter(e=>e.kind!=='limit');
 const slots=Array.from({length:16},(_,i)=>({x:10+(i%4)*247,y:10+Math.floor(i/4)*195}));
 const assignment=data.groups.map((_,i)=>i).concat([-1,-1,-1]);
 const groupOf=new Map(data.nodes.map(n=>[n.id,data.groups.indexOf(n.group)]));
 const counts=data.groups.map(g=>data.nodes.filter(n=>n.group===g).length);
 function regionPlacement(){const regions=[];
  for(let col=0;col<4;col++){const gs=assignment.filter((g,i)=>g>=0&&i%4===col);const total=gs.reduce((v,g)=>v+counts[g]+3,0),minimum=g=>Math.ceil(counts[g]/2)*24+70,minimumTotal=gs.reduce((v,g)=>v+minimum(g),0);let y=10;
   for(const g of gs){const height=Math.round(minimum(g)+Math.max(0,780-minimumTotal)*(counts[g]+3)/total);regions[g]={x:10+col*247,y,group:data.groups[g],color:colors[g],width:237,height:height-10};y+=height;}
  }return regions;
 }
 function groupCost(){const where=regionPlacement();return where.reduce((v,r)=>v+Math.max(0,r.y+r.height-790)*100000,0)+relations.reduce((v,e)=>{const a=where[groupOf.get(e.source)],b=where[groupOf.get(e.target)];return v+Math.hypot(a.x-b.x,a.y+a.height/2-b.y-b.height/2);},0);}
 let cost=groupCost();
 for(let pass=0;pass<20;pass++)for(let a=0;a<16;a++)for(let b=a+1;b<16;b++){
  [assignment[a],assignment[b]]=[assignment[b],assignment[a]];const next=groupCost();
  if(next<cost-1e-8)cost=next;else [assignment[a],assignment[b]]=[assignment[b],assignment[a]];
 }
 const regions=regionPlacement();
 const positions={};
 for(const r of regions){const nodes=data.nodes.filter(n=>n.group===r.group),rows=Math.ceil(nodes.length/2),step=Math.min(38,(r.height-70)/Math.max(1,rows-1));nodes.forEach((n,i)=>{positions[n.id]={x:r.x+61+(i%2)*112,y:r.y+53+Math.floor(i/2)*step};});}
 function nodeCost(){return relations.reduce((v,e)=>{const a=positions[e.source],b=positions[e.target];return v+Math.hypot(a.x-b.x,a.y-b.y);},0);}
 cost=nodeCost();
 for(let pass=0;pass<12;pass++)for(const r of regions){const ns=data.nodes.filter(n=>n.group===r.group);for(let a=0;a<ns.length;a++)for(let b=a+1;b<ns.length;b++){
  const x=ns[a].id,y=ns[b].id;[positions[x],positions[y]]=[positions[y],positions[x]];const next=nodeCost();
  if(next<cost-1e-8)cost=next;else [positions[x],positions[y]]=[positions[y],positions[x]];
 }}
 // Final label pass: round, then project any colliding label onto the next row.
 // With the present slot spacing this is normally a no-op, but is independent
 // of the objective and protects future changes to slot construction.
 for(const r of regions){const placed=[];
  for(const n of data.nodes.filter(n=>n.group===r.group)){
   const p=positions[n.id];p.x=Math.round(p.x);p.y=Math.round(p.y);
   for(let pass=0;pass<32;pass++){
    if(!placed.some(q=>Math.abs(q.x-p.x)<94&&Math.abs(q.y-p.y)<22))break;
    p.y+=22;
   }
   if(p.y>r.y+r.height-8)throw new Error(`Label layout overflow: ${n.id}`);
   placed.push(p);
  }
 }
 return {regions,positions};
}
export function mapSvg(data) {
 const {regions,positions}=mapLayout(data),out=[],headers=[];
 out.push(`<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1000 800" width="1000" height="800" role="group" aria-label="Overview of all ${data.nodes.length} families and ${data.groups.length} groups" data-family-map=""><defs><marker id="map-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" orient="auto"><path d="M0 0 L10 5 L0 10z" fill="context-stroke"/></marker></defs>`);
 for(const r of regions){const words=r.group.split(' '),lines=[''];for(const w of words){if((lines.at(-1)+' '+w).trim().length>28)lines.push(w);else lines[lines.length-1]=(lines.at(-1)+' '+w).trim();}
 out.push(`<g data-region="${esc(r.group)}"><rect x="${r.x}" y="${r.y}" width="${r.width}" height="${r.height}" rx="20" fill="${r.color}" fill-opacity=".065" stroke="${r.color}" stroke-opacity=".3"/></g>`);
 headers.push(`<g data-region-label="${esc(r.group)}"><rect x="${r.x+8}" y="${r.y+6}" width="${r.width-16}" height="${lines.length*14+5}" rx="6" fill="#fffdf8" fill-opacity=".9"/><text x="${r.x+12}" y="${r.y+19}" font-size="12" font-weight="600" fill="${r.color}">${lines.map((l,i)=>`<tspan x="${r.x+12}" dy="${i?14:0}">${esc(l)}</tspan>`).join('')}</text></g>`);}

 for(const e of [...data.edges].sort((a,b)=>(a.kind==='limit'?0:1)-(b.kind==='limit'?0:1))){const a=positions[e.source],b=positions[e.target];const dx=b.x-a.x,dy=b.y-a.y,len=Math.hypot(dx,dy)||1;
 const trim=()=>Math.min(46/Math.max(Math.abs(dx/len),.01),11/Math.max(Math.abs(dy/len),.01),Math.max(.5,len/2-.5));
 const t=trim(),u=trim();let x1=Math.round(a.x+dx/len*t),y1=Math.round(a.y+dy/len*t),x2=Math.round(b.x-dx/len*u),y2=Math.round(b.y-dy/len*u);
 const peers=data.edges.filter(o=>o.source===e.source&&o.target===e.target),offset=(peers.indexOf(e)-(peers.length-1)/2)*10;
 let cx=Math.round((a.x+b.x)/2-dy/len*offset),cy=Math.round((a.y+b.y)/2+dx/len*offset);
 const source=data.nodes.find(n=>n.id===e.source),target=data.nodes.find(n=>n.id===e.target);
 if(source.group===target.group&&a.x===b.x&&e.source!==e.target){const r=regions[data.groups.indexOf(source.group)],left=a.x<r.x+118,sign=left?-1:1;x1=a.x+sign*46;x2=b.x+sign*46;y1=a.y;y2=b.y;cx=left?r.x+2:r.x+r.width-2;cy=Math.round((a.y+b.y)/2);}
 let gutterRoute='';
 const sr=regions[data.groups.indexOf(source.group)],tr=regions[data.groups.indexOf(target.group)];
 if(source.group!==target.group&&sr.x===tr.x){const lane=sr.x+117+offset/3;x1=a.x+(a.x<lane?46:-46);x2=b.x+(b.x<lane?46:-46);gutterRoute=`M${x1} ${a.y} C${Math.round(lane)} ${a.y} ${Math.round(lane)} ${b.y} ${x2} ${b.y}`;}
 const route=gutterRoute||(e.source===e.target?`M${a.x+40} ${a.y-10} C${a.x+65} ${a.y-35} ${a.x+65} ${a.y+35} ${a.x+40} ${a.y+10}`:`M${x1} ${y1} Q${cx} ${cy} ${x2} ${y2}`);
 out.push(`<g data-map-edge="${e.id}" data-source="${e.source}" data-target="${e.target}" data-kind="${e.kind}"${e.kind==='limit'?' style="display:none"':''}><title>${esc(e.note)}</title><path d="${route}" stroke="${e.kind==='transform_of'?'#426c8c':'#53685f'}" stroke-width="${e.kind==='limit'?'.7':'1.2'}" opacity="${e.kind==='limit'?'.2':'.65'}" stroke-dasharray="${edgeDash(e.kind,e.conditional)}" fill="none" marker-end="url(#map-arrow)"/></g>`);}
 out.push(...headers);
 for(const n of data.nodes){const p=positions[n.id],color=colors[data.groups.indexOf(n.group)],hub=['norm','gamma','beta','binom','pois'].includes(n.id);out.push(`<a data-map-node="${n.id}" data-family="${n.id}" href="/distributions/${n.id}/" aria-label="${esc(n.name)} (${n.id})"><title>${esc(n.name)} · ${esc(n.group)}. Select for neighbourhood; modified click opens dossier.</title><rect x="${p.x-45}" y="${p.y-10}" width="90" height="21" rx="5" fill="#fffdf8" stroke="${color}" stroke-opacity="${hub?'.9':'.25'}"/><text x="${p.x}" y="${p.y+4}" text-anchor="middle" font-size="12.5" font-weight="${hub?'700':'400'}" fill="${hub?'#203d35':color}">${n.id}</text></a>`);}
 out.push('</svg>');return out.join('');
}
