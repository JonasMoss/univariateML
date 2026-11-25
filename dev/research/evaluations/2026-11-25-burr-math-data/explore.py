"""Numerical continuation reconnaissance for competing finite modes."""
import numpy as np

from search import burr, sigmoid


def main():
    # Perturb both repeated lower observations together and the penultimate
    # observation independently, keeping the ends and the fourth fixed.
    rows = np.array([[0,a,a,.625,b,1] for a in np.linspace(.17,.35,31)
                     for b in np.linspace(.66,.91,31)])
    k = np.array([.2,.22,.24,.28,.34,.43,.55,.75,1,1.5,2.5,4,6,8,12,18,32,64,128])
    status = []
    for i in range(0,len(rows),32):
        y = rows[i:i+32]
        values, score, _ = burr(y,np.full(y.shape,1/6),k)
        maxima = (score[:,:-1]>0)&(score[:,1:]<0)
        for j in np.where(maxima.sum(1)>=2)[0]:
            ix = np.where(maxima[j])[0]
            heights = np.maximum(values[j,ix],values[j,ix+1])
            P = -np.log(y[j].mean())-1
            status.append((min(heights)-max(P,values[j,-1]),*y[j],*heights, P, values[j,-1]))
    print('all-free paired grid, two-mode patterns',len(status))
    for row in sorted(status,reverse=True)[:8]:
        print(row)
    # Held-power four-observation kernel and a dense three-coordinate scan.
    t = np.linspace(-35,6,821)
    rows = np.array([[0,a,b,d] for d in np.linspace(6,25,20)
                     for a in np.linspace(.45*d,.95*d,17)
                     for b in np.linspace(a,d,17)])
    status = []
    for i in range(0,len(rows),48):
        y = rows[i:i+48]
        z = y[:,None,:]+t[None,:,None]
        G = np.logaddexp(0,z).mean(2)
        Q = sigmoid(-z).mean(2)
        score = G*Q-(1-Q)
        gaps = t-np.log(G)-G+np.log(np.exp(y).mean(1))[:,None]
        maxima = (score[:,:-1]>1e-12)&(score[:,1:]<-1e-12)
        for j in np.where(maxima.sum(1)>=2)[0]:
            ix = np.where(maxima[j])[0]
            heights = np.maximum(gaps[j,ix],gaps[j,ix+1])
            if min(heights)>0:
                status.append((abs(heights[0]-heights[1]),*y[j],*t[ix],*heights))
    print('held-power paired grid, two positive modes',len(status))
    for row in sorted(status)[:8]:
        print(row)


if __name__ == '__main__':
    main()
