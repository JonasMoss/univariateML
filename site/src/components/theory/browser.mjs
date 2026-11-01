import assert from 'node:assert/strict';
import {chromium} from 'playwright-core';
import {createServer} from 'node:http';
import {readFileSync,existsSync,readdirSync,mkdirSync,statSync,writeFileSync} from 'node:fs';
import path from 'node:path';
import os from 'node:os';
const root=path.resolve(import.meta.dirname,'../../../..'),dist=path.join(root,'site/dist'),scratch=path.join(root,'build/scratch/theory-viz');
mkdirSync(scratch,{recursive:true});
const cache=path.join(os.homedir(),'.cache/ms-playwright');
const executable=process.env.BROWSER_EXECUTABLE||readdirSync(cache).filter(n=>/^chromium_headless_shell-/.test(n)).sort().reverse().map(n=>path.join(cache,n,'chrome-headless-shell-linux64/chrome-headless-shell')).find(existsSync);
const types={'.html':'text/html','.js':'text/javascript','.mjs':'text/javascript','.wasm':'application/wasm','.css':'text/css','.svg':'image/svg+xml'};
const server=createServer((req,res)=>{let file=path.join(dist,decodeURIComponent(new URL(req.url,'http://localhost').pathname));if(existsSync(file)&&statSync(file).isDirectory())file=path.join(file,'index.html');if(!file.startsWith(dist)||!existsSync(file)){res.writeHead(404);res.end();return;}res.setHeader('Content-Type',types[path.extname(file)]||'application/octet-stream');res.end(readFileSync(file));});
await new Promise(r=>server.listen(0,'127.0.0.1',r));let browser;
try{
 browser=await chromium.launch({executablePath:executable,headless:true,args:['--no-sandbox','--disable-dev-shm-usage']});
 const page=await browser.newPage({viewport:{width:1280,height:1050}}),errors=[];
 page.on('pageerror',e=>errors.push(e.message));
 const url=`http://127.0.0.1:${server.address().port}/theory/`;
 await page.goto(url);const profiles=page.locator('#profiles'),faces=page.locator('#faces');
 await profiles.getByRole('button',{name:'Fit pasted data'}).waitFor();
 await page.waitForFunction(()=>!document.querySelector('#profiles fieldset').disabled);
 assert.equal(await page.locator('svg[data-profile]').count(),5);assert.equal(await page.locator('svg[data-faces]').count(),1);
 for(const [width,height,label] of [[1280,1050,'desktop'],[390,844,'mobile']]){
  await page.setViewportSize({width,height});
  for(const section of ['profiles','escapes','faces']){
   await page.locator('#'+section).evaluate(el=>el.scrollIntoView({block:'start'}));
   await page.screenshot({path:path.join(scratch,`${label}-${section}.png`)});
   await page.locator('#'+section+' .figure').first().screenshot({path:path.join(scratch,`${label}-${section}-figure.png`)});
   assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),label+' overflow '+section);
  }
 }
 for(const id of ['lomax','std','nbinom','burr']){
  const card=page.locator(`[data-escape=${id}]`);
  await card.getByRole('button',{name:'Recompute preset'}).scrollIntoViewIfNeeded();
  await card.getByRole('button',{name:'Recompute preset'}).click();
  await page.waitForFunction(id=>document.querySelector(`[data-escape=${id}] .theory-view`).getAttribute('aria-busy')==='false',id);
  assert.equal(await card.locator('svg[data-profile]').count(),1);
 }
 await profiles.locator('select').selectOption('lomax');await profiles.locator('svg[data-profile=lomax]').waitFor();assert.match(await profiles.textContent(),/mlexp/);
 await profiles.locator('select').selectOption('nbinom');await profiles.locator('svg[data-profile=nbinom]').waitFor();assert.match(await profiles.textContent(),/mlpois/);
 await profiles.locator('select').selectOption('gamma');await profiles.locator('svg[data-profile=gamma]').waitFor();
 await profiles.locator('textarea').fill('1,2,3,4,5,6');await profiles.getByRole('button',{name:'Fit pasted data'}).click();await page.waitForFunction(()=>document.querySelector('#profiles .diagnostic').textContent.includes('6 observations'));
 const pasted=await profiles.locator('svg').getAttribute('aria-label');
 await profiles.locator('textarea').fill('bad');await profiles.getByRole('button',{name:'Fit pasted data'}).click();await profiles.getByRole('alert').waitFor();assert.equal(await profiles.locator('svg').getAttribute('aria-label'),pasted);
 await profiles.getByRole('button',{name:'Simulate 60 & fit'}).click();await page.waitForFunction(()=>document.querySelector('#profiles .diagnostic').textContent.includes('60 observations'));
 const simulated=await profiles.locator('textarea').inputValue();await profiles.getByRole('button',{name:'Simulate 60 & fit'}).click();await page.waitForFunction(()=>document.querySelector('#profiles .theory-view').getAttribute('aria-busy')==='false');assert.equal(await profiles.locator('textarea').inputValue(),simulated,'seed reproducibility');
 await profiles.getByRole('button',{name:'Reset preset'}).click();await page.waitForFunction(()=>document.querySelector('#profiles .diagnostic').textContent.includes('12 observations'));
 await faces.getByLabel('Held mean',{exact:true}).fill('0.5');await faces.getByLabel('Held sd',{exact:true}).fill('1.5');await faces.getByRole('button',{name:'Fit pasted data'}).click();await page.waitForFunction(()=>document.querySelector('#faces tbody tr:nth-child(2) td:nth-child(2)').textContent==='0.500000');
 await faces.getByLabel('Held sd',{exact:true}).fill('-1');await faces.getByRole('button',{name:'Fit pasted data'}).click();await faces.getByRole('alert').waitFor();
 const plain=await browser.newContext({javaScriptEnabled:false,viewport:{width:390,height:844}}),p=await plain.newPage();await p.goto(url);
 assert.equal(await p.locator('svg[data-profile]').count(),5);assert.equal(await p.locator('svg[data-faces]').count(),1);assert.equal(await p.locator('fieldset:disabled').count(),2);
 assert.equal(await p.locator('[data-escape] code').count(),4);await p.locator('#profiles details summary').click();assert.equal(await p.locator('#profiles tbody tr').count(),65);
 await p.screenshot({path:path.join(scratch,'mobile-no-js.png')});await plain.close();assert.deepEqual(errors,[]);
 writeFileSync(path.join(scratch,'browser.json'),JSON.stringify({checks:['desktop/mobile each section','no horizontal overflow','hydration','four gallery recomputations','family changes','paste','invalid data retains figure','seed reproducibility','reset','held faces change','invalid held sd','six static figures and four messages without JS','no page errors'],screenshots:13},null,2)+'\n');
 console.log('PASS theory browser: desktop/mobile, live profiles and held faces, seeded data, errors, no-JS figures and diagnostics.');
}finally{await browser?.close();await new Promise(r=>server.close(r));}
