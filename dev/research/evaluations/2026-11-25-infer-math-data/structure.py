"""80-digit checks of two singular charts (analytic proofs in report)."""
from pathlib import Path
import csv
import mpmath as mp

mp.mp.dps = 80
rows = []
for s in map(mp.mpf, ('0.2', '1', '5')):
    for a in map(mp.mpf, ('0.3', '2', '10')):
        def ratio(x, s, a, t):
            return ((x+s)**2-t)/((x+1)*(x+a+2*s))
        derivative_rows = [
            [mp.diff(lambda v: mp.log(ratio(x, v, a, 0)), s),
             mp.diff(lambda v: mp.log(ratio(x, s, v, 0)), a),
             mp.diff(lambda v: mp.log(ratio(x, s, a, v)), 0)]
            for x in range(3)]
        det = mp.det(mp.matrix(derivative_rows))
        assert det != 0
        rows.append(('bnbinom', str(s), str(a), mp.nstr(det, 20)))
for rate in map(mp.mpf, ('0.5', '2', '8')):
    for a in map(mp.mpf, ('0.1', '1', '10')):
        p = rate/a
        for y in map(mp.mpf, ('0.01', '0.3', '2', '15')):
            def ll(q):
                return mp.log(a) - rate*y + (q-1)*mp.log(-mp.expm1(-a*y)) - mp.log(mp.beta(p,q))
            assert abs(ll(1)-(mp.log(rate)-rate*y)) < mp.mpf('1e-75')
            score = mp.diff(ll, 1)
            assert abs(score-(mp.digamma(p+1)-mp.digamma(1)+mp.log(-mp.expm1(-a*y)))) < mp.mpf('1e-75')
            rows.append(('genbeta', str(rate), f'{a};{y}', mp.nstr(score, 20)))
with Path(__file__).with_name('structure.csv').open('w', newline='') as output:
    w = csv.writer(output)
    w.writerow(('family','first_grid_coordinate','remaining_grid_coordinates','determinant_or_score'))
    w.writerows(rows)
print(f'{len(rows)} structural checks passed at 80 digits')
