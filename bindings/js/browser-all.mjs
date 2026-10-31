// Uses the already-installed site browser tooling; never downloads a browser.
import {chromium} from '../../site/node_modules/playwright-core/index.mjs';
import {createServer} from 'node:http';
import {readFileSync,writeFileSync,existsSync,readdirSync,statSync} from 'node:fs';
import path from 'node:path';
import os from 'node:os';
const root=process.cwd(),dist=path.join(root,'site/dist');
const cache=path.join(os.homedir(),'.cache/ms-playwright');
const executable=process.env.BROWSER_EXECUTABLE || readdirSync(cache).filter(n=>/^chromium_headless_shell-/.test(n)).sort().reverse().map(n=>path.join(cache,n,'chrome-headless-shell-linux64/chrome-headless-shell')).find(existsSync);
const types={'.html':'text/html','.js':'text/javascript','.mjs':'text/javascript','.css':'text/css','.wasm':'application/wasm'};
const server=createServer((req,res)=>{
 let file=path.join(dist,decodeURIComponent(new URL(req.url,'http://localhost').pathname));
 if(!file.startsWith(dist+path.sep)){res.writeHead(403);res.end();return;}
 if(existsSync(file)&&statSync(file).isDirectory())file=path.join(file,'index.html');
 if(!existsSync(file)){res.writeHead(404);res.end();return;}
 res.setHeader('Content-Type',types[path.extname(file)]||'application/octet-stream');res.end(readFileSync(file));
});
await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
let browser;
const results=[];
let rankingAudit={ok:false};
try {
 browser=await chromium.launch({executablePath:executable,headless:true,env:{...process.env,XDG_CONFIG_HOME:path.join(root,'build/scratch/browser-all-config')},args:['--no-sandbox','--disable-dev-shm-usage']});
 const page=await browser.newPage();
 const catalog=JSON.parse(readFileSync('site/src/data/families.json','utf8'));
 for(const family of catalog) {
  const errors=[];const listener=e=>errors.push(e.message);page.on('pageerror',listener);
  try {
   await page.goto(`http://127.0.0.1:${server.address().port}/distributions/${family.id}/`);
   await page.locator('.explorer').scrollIntoViewIfNeeded();
   await page.waitForFunction(()=>{const e=document.querySelector('.explorer');return e&&(!e.textContent.includes('Loading local calculation module')||e.querySelector('svg'));},{},{timeout:30000});
   await page.locator('svg[aria-label="CDF plot"]').waitFor({timeout:10000});
   for(const p of family.params.filter(p=>['design','threshold'].includes(p.kind))) {
    if(await page.locator(`#slider-${p.name}`).count())throw Error(`Required coefficient ${p.name} has a slider`);
   }
   const cdf=await page.locator('svg[aria-label="CDF plot"] .density').getAttribute('d');
   if(family.support.type==='integer'&&!cdf.includes('H'))throw Error('Discrete CDF has no steps');
   await page.getByLabel('Observations n').fill('30');
   await page.getByRole('button',{name:'Simulate and fit',exact:true}).click();
   await page.waitForFunction(()=>{
    const e=document.querySelector('.explorer');
    return !e.textContent.includes('Fitting…') && (e.querySelector('.fit-status') || e.querySelector('[role="alert"]'));
   },{},{timeout:120000});
   const fitStatus=await page.locator('.fit-status').allTextContents();
   const diagnostics=await page.locator('.explorer [role="alert"]').allTextContents();
   if(!fitStatus.length&&!diagnostics.some(s=>s.trim()))throw Error('Fit produced neither a result nor a diagnostic');
   if(errors.length)throw Error(errors.join('; '));
   results.push({family:family.id,ok:true,discrete:family.support.type==='integer',fitStatus,diagnostics});
  } catch(e) {results.push({family:family.id,ok:false,error:e.message,alerts:await page.locator('.explorer [role="alert"]').allTextContents()});}
  page.off('pageerror',listener);
 }
 await page.goto(`http://127.0.0.1:${server.address().port}/distributions/exp/`);
 await page.locator('.explorer').scrollIntoViewIfNeeded();
 await page.waitForFunction(()=>!document.querySelector('.explorer').textContent.includes('Loading local calculation module'));
 await page.locator('#observations').fill('0.2 0.5 1 1.8 2.7 3.4 4.1 5.3');
 await page.getByRole('button',{name:'Fit and rank models',exact:true}).click();
 await page.waitForFunction(n=>document.querySelectorAll('.ranking p').length===n,catalog.length,{timeout:300000});
 const rows=await page.locator('.ranking p').allTextContents();
 if(rows.some(s=>s.includes('Fitting is not yet available')))throw Error('Ranking omits a shared fitter');
 const uniform=rows.find(s=>s.startsWith('unif:'));
 if(!uniform||!uniform.includes('AIC'))throw Error('Ranking rejected the admissible fitted uniform support');
 rankingAudit={ok:true,families:rows.length,rows};
} finally {
 if(browser)await browser.close();await new Promise(resolve=>server.close(resolve));
 writeFileSync('build/scratch/browser-all.json',JSON.stringify(results,null,2));
 writeFileSync('build/scratch/browser-ranking.json',JSON.stringify(rankingAudit,null,2));
}
const failures=results.filter(r=>!r.ok);
console.log(JSON.stringify({pages:results.length,failures,ranking:{ok:rankingAudit.ok,families:rankingAudit.families}},null,2));
if(failures.length)process.exitCode=1;
