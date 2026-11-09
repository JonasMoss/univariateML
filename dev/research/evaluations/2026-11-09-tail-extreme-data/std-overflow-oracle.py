import json,math,sys,mpmath as m
m.mp.dps=360
nu=m.mpf(sys.float_info.max)
c=m.loggamma((nu+1)/2)-m.loggamma(nu/2)-m.log(m.pi)/2
rows=[]
for xf in [math.nextafter(math.sqrt(sys.float_info.max),math.inf),1.4e154]:
 x=m.mpf(xf)
 rows.append(dict(x=xf,log_std=str(c-m.log(nu-2)/2-(nu+1)/2*m.log1p(x*x/(nu-2))),log_lst=str(c-m.log(nu)/2-(nu+1)/2*m.log1p(x*x/nu))))
print(json.dumps(rows,indent=2))
