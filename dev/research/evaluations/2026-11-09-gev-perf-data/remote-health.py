"""Diagnose simbox without acquiring or changing shared slots.
Writes and removes one unique directory inside this job's own remote scratch.
"""
import datetime,json,pathlib,subprocess
folder=pathlib.Path(__file__).resolve().parent
ssh=['timeout','20','ssh','-F',str(pathlib.Path.home()/'.ssh/config'),'-o','BatchMode=yes','-o','ConnectTimeout=8','simbox']
probe='''probe="$HOME/umvl-wt/gev-perf/build/scratch/gev-perf-health-$$"
nice -n 10 /usr/bin/mkdir "$probe"
rc=$?
if [[ "$rc" == 0 ]]; then nice -n 10 /usr/bin/rmdir "$probe"; fi
exit "$rc"
'''
state='''print('python_running',flush=True)
import os,time
root=os.path.expanduser('~/umvl-wt')
try:
 for name in os.listdir(root):
  if name.startswith('.slot'):
   st=os.stat(root+'/'+name);print('slot',name,'age_minutes',(time.time()-st.st_mtime)/60,flush=True)
except OSError as e:print('slot_metadata_error',repr(e),flush=True)
rows=[]
for name in os.listdir('/proc'):
 if not name.isdigit():continue
 path='/proc/'+name
 try:
  if os.stat(path).st_uid!=os.getuid():continue
  comm=open(path+'/comm').read().strip();stat=open(path+'/stat').read();f=stat[stat.rfind(')')+2:].split()
  rows.append(((int(f[11])+int(f[12]))/os.sysconf('SC_CLK_TCK'),name,comm,f[0]))
 except (OSError,ValueError,IndexError):pass
for row in sorted(rows,reverse=True)[:15]:print('cpu_seconds_pid_name_state',row,flush=True)
'''
now=datetime.datetime.now(datetime.timezone.utc)
result={'at_utc':now.isoformat(),'checks':{}}
for name,command,script in [('own_scratch_directory','nice -n 10 /bin/bash -s',probe),('slots_and_processes','nice -n 10 python3 -',state)]:
 r=subprocess.run(ssh+[command],input=script,text=True,capture_output=True)
 result['checks'][name]={'returncode':r.returncode,'stdout':r.stdout,'stderr':r.stderr}
path=folder/('remote-health-'+now.strftime('%Y%m%d-%H%M%S')+'.json')
path.write_text(json.dumps(result,indent=2)+'\n')
print(path)
print(json.dumps(result,indent=2))
