"""Pin this foreground validation command and its children to one allowed CPU."""
import os,sys
os.sched_setaffinity(0,{min(os.sched_getaffinity(0))})
for key in ['GOMAXPROCS','RAYON_NUM_THREADS','UV_THREADPOOL_SIZE','OPENBLAS_NUM_THREADS','OMP_NUM_THREADS','MKL_NUM_THREADS']:
    os.environ[key]='1'
os.execvp(sys.argv[1],sys.argv[1:])
