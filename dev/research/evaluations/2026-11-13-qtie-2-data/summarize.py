import csv,json,re,pathlib,collections
D=pathlib.Path(__file__).parent
allrows=[]
for interface in ('cpp','r'):
 for stage in ('before','after'):
  path=D/f'{interface}-{stage}.csv'
  if not path.exists():continue
  groups=collections.defaultdict(lambda:collections.Counter())
  errors=[]
  for row in csv.DictReader(path.open()):
   if row['lower']=='NA':errors.append(row);continue
   ident=row['id']
   ident=re.sub(r"-window-\[(\d+), (?:'([^']+)'|(\d+))\]",lambda m:'-window-'+m[1]+'-'+(m[2] or m[3]),ident)
   lower=row['lower'] in ('1','TRUE');log=row['log'] in ('1','TRUE');mode=('lower' if lower else 'upper')+('-log' if log else '-linear')
   key=(ident,mode);target=groups[key]
   for key2 in ('n','up','earlier','invalid','crossing_failure'):target[key2]+=int(row[key2])
   if row.get('error'):errors.append(row)
  for (ident,mode),values in sorted(groups.items()):allrows.append(dict(interface=interface,stage=stage,configuration=ident,mode=mode,**values,raw_misses=values['up']+values['earlier']+values['invalid']))
  (D/f'{interface}-{stage}-exclusions.json').write_text(json.dumps(errors,indent=2)+'\n')
with (D/'counts.csv').open('w') as f:
 writer=csv.DictWriter(f,fieldnames=['interface','stage','configuration','mode','n','up','earlier','invalid','crossing_failure','raw_misses']);writer.writeheader();writer.writerows(allrows)
summary={}
for interface in ('cpp','r'):
 for stage in ('before','after'):
  rows=[r for r in allrows if r['interface']==interface and r['stage']==stage]
  summary[interface+'-'+stage]={key:sum(r[key] for r in rows) for key in ('n','up','earlier','invalid','crossing_failure','raw_misses')}
(D/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
