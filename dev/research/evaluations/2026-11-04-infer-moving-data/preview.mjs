// Offline smoke check for the optional ignored HTML preview; no installation.
import {createRequire} from 'node:module';
import {readFileSync, writeFileSync, readdirSync, existsSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import os from 'node:os';
const here=path.dirname(fileURLToPath(import.meta.url));
const root=path.resolve(here,'../../../..');
const require=createRequire(import.meta.url);
const {chromium}=require(path.join(root,'site/node_modules/playwright-core'));
const cache=path.join(os.homedir(),'.cache/ms-playwright');
const executable=readdirSync(cache).filter(n=>/^chromium_headless_shell-/.test(n)).sort().reverse()
  .map(n=>path.join(cache,n,'chrome-headless-shell-linux64/chrome-headless-shell')).find(existsSync);
const browser=await chromium.launch({executablePath:executable,headless:true,
  args:['--no-sandbox','--disable-dev-shm-usage','--renderer-process-limit=1','--num-raster-threads=1']});
try {
  const context=await browser.newContext({offline:true,viewport:{width:1280,height:900}});
  const page=await context.newPage();
  const errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  await page.goto('file://'+path.join(root,'build/scratch/2026-11-04-infer-moving-report.html'));
  const inspection=await page.evaluate(()=>({
    mathElements:document.querySelectorAll('math').length,
    mathErrors:document.querySelectorAll('merror').length,
    scripts:document.querySelectorAll('script').length,
    equations:[...document.querySelectorAll('math')].map(x=>({
      width:x.getBoundingClientRect().width,height:x.getBoundingClientRect().height,
      text:x.textContent.slice(0,75)})),
    headings:[...document.querySelectorAll('h2')].map(x=>x.id)
  }));
  if(inspection.mathElements!==28 || inspection.mathErrors || inspection.scripts || errors.length)
    throw new Error(JSON.stringify({inspection,errors}));
  if(inspection.equations.some(x=>x.width<=0 || x.height<=0)) throw new Error('invisible math');
  for(const anchor of ['tukey-lambda','ged','gpd-and-gev','two-piece-normal-snorm','huber'])
    if(!inspection.headings.includes(anchor)) throw new Error('missing anchor '+anchor);
  await page.locator('#an-endpoint').scrollIntoViewIfNeeded();
  await page.screenshot({path:path.join(root,'build/scratch/infer-moving-preview.png')});
  const result={offline:true,passed:true,mathElements:28,mathErrors:0,scripts:0,pageErrors:errors,
    browserVersion:browser.version(),equations:inspection.equations,
    scope:'static MathML preview only; not a package site check'};
  writeFileSync(path.join(here,'preview-validation.json'),JSON.stringify(result,null,2)+'\n');
  console.log(JSON.stringify({offline:true,passed:true,mathElements:28,mathErrors:0,scripts:0}));
} finally {
  await browser.close();
}
