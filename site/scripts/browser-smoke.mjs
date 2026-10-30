import { chromium } from 'playwright-core';
import {createServer} from 'node:http';
import {readFileSync,writeFileSync,existsSync,readdirSync,mkdirSync,statSync} from 'node:fs';
import path from 'node:path';
import os from 'node:os';
const root=path.resolve(import.meta.dirname,'../..'),dist=path.join(root,'site/dist');
const scratch=path.join(root,'build/scratch/site-browser');mkdirSync(scratch,{recursive:true});
const cache=path.join(os.homedir(),'.cache/ms-playwright');
const executable=process.env.BROWSER_EXECUTABLE || (existsSync(cache) ? readdirSync(cache).filter(n=>/^chromium_headless_shell-/.test(n)).sort().reverse().map(n=>path.join(cache,n,'chrome-headless-shell-linux64/chrome-headless-shell')).find(existsSync) : undefined);
if(!executable) throw new Error('Set BROWSER_EXECUTABLE to an existing Chromium; this check never downloads a browser.');
const types={'.html':'text/html','.js':'text/javascript','.mjs':'text/javascript','.css':'text/css','.svg':'image/svg+xml','.wasm':'application/wasm'};
const requests=[];
const server=createServer((req,res)=>{let file=path.join(dist,decodeURIComponent(new URL(req.url,'http://localhost').pathname));if(!file.startsWith(dist)){res.writeHead(403);res.end();return;}if(existsSync(file)&&statSync(file).isDirectory())file=path.join(file,'index.html');if(!existsSync(file)){res.writeHead(404);res.end();return;}res.setHeader('Content-Type',types[path.extname(file)]||'application/octet-stream');res.end(readFileSync(file));});
await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
let browser;
try {
 const base=`http://127.0.0.1:${server.address().port}`;
 browser=await chromium.launch({executablePath:executable,headless:true,env:{...process.env,XDG_CONFIG_HOME:path.join(scratch,'config')},args:['--no-sandbox','--disable-dev-shm-usage']});
 const page=await browser.newPage({viewport:{width:1200,height:1000}});
 const errors=[],bindingRequests=[],localRequests=[];page.on('pageerror',e=>errors.push(e.message));page.on('request',r=>{if(r.url().startsWith(base))localRequests.push(new URL(r.url()).pathname);if(r.url().includes('/binding/'))bindingRequests.push(r.url());if(!r.url().startsWith(base)&&!r.url().startsWith('data:'))requests.push(r.url());});
 await page.setViewportSize({width:1200,height:200});
 await page.goto(base+'/distributions/gamma/');
 await page.waitForTimeout(200);

 const initialRequests=[...new Set(localRequests)];
 const hasExplorer=await page.locator('.explorer').count()>0;
 if(hasExplorer){
  if(bindingRequests.length)throw new Error('Binding requested before explorer intersection');
  await page.locator('.explorer').scrollIntoViewIfNeeded();
  await page.locator('svg[aria-label="Density plot"] .density').waitFor();
  const loadedRequests=[...new Set(localRequests)];
  const bytes=urls=>urls.reduce((sum,url)=>{let file=path.join(dist,url);if(statSync(file).isDirectory())file=path.join(file,'index.html');return sum+statSync(file).size;},0);
  const lazyRequests=loadedRequests.filter(url=>!initialRequests.includes(url));
  writeFileSync(path.join(scratch,'request-weights.json'),JSON.stringify({initialRequests,lazyRequests,initialBytes:bytes(initialRequests),lazyBytes:bytes(lazyRequests)},null,2));
  const curve=page.locator('svg[aria-label="Density plot"] .density');const before=await curve.getAttribute('d');
  await page.locator('#slider-shape').fill('1');
  await page.waitForFunction(old=>document.querySelector('svg[aria-label="Density plot"] .density').getAttribute('d')!==old,before);
  await page.locator('#slider-shape').press('ArrowRight');
  await page.getByLabel('Hold shape when fitting',{exact:true}).check();
  await page.getByRole('button',{name:'Simulate and fit',exact:true}).click();
  await page.locator('.fit-status').waitFor();
  if(!await page.locator('.explorer table').innerText().then(t=>t.includes('Estimate')&&t.includes('(held)')))throw new Error('Fit estimates missing');
  await page.locator('#observations').fill('2 2 2 2');
  await page.getByRole('button',{name:'Fit and rank models'}).click();
  await page.waitForFunction(()=>document.querySelector('.ranking').textContent.includes('unbounded'));
  const diagnostic=await page.locator('.ranking p').filter({hasText:/^gamma:/}).innerText();
  if(!diagnostic.includes('unbounded')||diagnostic.includes('AIC'))throw new Error('Gamma failure became an estimate');
  await page.locator('#observations').fill('0.2 0.5 0.8 1.1 1.7 2.4 3.1 5');
  await page.getByRole('button',{name:'Fit and rank models'}).click();
  await page.waitForFunction(()=>document.querySelector('.ranking').textContent.includes('AIC'));
  const aics=await page.locator('.ranking p').evaluateAll(ps=>ps.map(p=>p.textContent.match(/AIC ([^;]+)/)?.[1]).filter(Boolean).map(Number));
  if(aics.length<2||aics.some((v,i)=>i&&v<aics[i-1]))throw new Error('AIC ranking is missing or unordered');
  await page.setViewportSize({width:1200,height:1000});
  await page.locator('.explorer').screenshot({path:path.join(scratch,'explorer-gamma.png')});
 }
 await page.selectOption('#language','cpp');
 if(await page.locator('.usage-panel[data-language=r]').isVisible())throw new Error('R panel remained visible');
 await page.check('#detail');
 if(!await page.locator('details.implementation').evaluateAll(ds=>ds.every(d=>d.open)))throw new Error('Implementation did not open');
 await page.goto(base+'/distributions/exp/');
 if(await page.locator('#language').inputValue()!=='cpp')throw new Error('Language preference did not persist');
 await page.goto(base+'/distributions/gamma/');await page.screenshot({path:path.join(scratch,'gamma-desktop.png'),fullPage:true});
 await page.setViewportSize({width:390,height:844});await page.goto(base+'/distributions/gb2/');
 if(hasExplorer){await page.locator('.explorer').scrollIntoViewIfNeeded();await page.locator('svg[aria-label="Density plot"]').waitFor();}
 if(!await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth))throw new Error('Mobile horizontal overflow');
 await page.screenshot({path:path.join(scratch,'gb2-mobile.png'),fullPage:true});
 if(!await page.locator('math[display=block], pre').evaluateAll(nodes=>nodes.every(n=>{if(n.scrollWidth<=n.clientWidth)return true;n.scrollLeft=20;return n.scrollLeft>0;})))throw new Error('Wide math/code cannot scroll');
 for(const id of ['exp','gamma']){
  await page.goto(base+`/distributions/${id}/`);
  if(hasExplorer){await page.locator('.explorer').scrollIntoViewIfNeeded();await page.locator('svg[aria-label="Density plot"]').waitFor();}
  if(!await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth))throw new Error(`${id}: mobile horizontal overflow`);
 }

 await page.goto(base+'/distributions/');await page.locator('astro-island:not([ssr])').waitFor();
 await page.locator('input[type=search]').fill('gamma');await page.waitForFunction(()=>document.querySelectorAll('.cards li').length>0&&document.querySelectorAll('.cards li').length<98);
 await page.locator('input[type=search]').fill('');
 await page.locator('.filters select').nth(1).selectOption('integer');await page.waitForFunction(()=>[...document.querySelectorAll('.cards li .count')].every(n=>n.textContent.includes('Discrete')));
 await page.goto(base+'/search/');await page.locator('.pagefind-ui__search-input').fill('gamma');await page.locator('.pagefind-ui__result').first().waitFor();
 const nojs=await browser.newContext({javaScriptEnabled:false});const plain=await nojs.newPage();await plain.goto(base+'/distributions/exp/');if(await plain.locator('.usage-panel:visible').count()!==4)throw new Error('No-JS usage panels hidden');await nojs.close();
 if(errors.length||requests.length)throw new Error(JSON.stringify({errors,externalRequests:requests}));
 console.log(`PASS Chromium ${browser.version()}: explorer slider/simulation/diagnostics/AIC (when available), persistent language, implementation switch, Svelte name/measure filters, Pagefind results, no-JS panels, mobile overflow; no page errors or external requests.`);
} finally {if(browser)await browser.close();await new Promise(resolve=>server.close(resolve));}
