"""Try simpler rational radii and smaller integer weights on the nominated pattern."""
from exact_modes import F, score
from search_modes import signs

base_tracks = [[F(i,1000) for i in (46,794,49,149,327)],
               [F(i,100) for i in (5,79,5,15,33)],
               [F(i,100) for i in (4,80,5,15,33)]]
for base in base_tracks:
    for factor, amplitude in [(10,3),(30,5),(100,10),(300,20),(1000,30),
                              (3000,50),(10000,100),(30000,200),(100000,300),
                              (300000,500),(1000000,700),(10000000,1000)]:
        r = base + [F(amplitude)]
        w = [factor*i for i in (3,40,40,48,31)]+[1]
        g = signs(r,w)
        pattern = ''.join('+' if y>0 else '-' for y in g)
        filtered = ''.join(c for i,c in enumerate(pattern) if i==0 or pattern[i-1]!=c)
        if '+-+-' in filtered:
            print(r,w,pattern)
            break
