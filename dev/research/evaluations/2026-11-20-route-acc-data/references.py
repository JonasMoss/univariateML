"""Independent high-precision roots for the modifier-route tests."""
import mpmath as mp

mp.mp.dps = 70
rate = mp.findroot(lambda l: l / -mp.expm1(-l) - 3, (2, 3))
theta = mp.findroot(lambda t: -t / ((1-t)*mp.log1p(-t)) - 3, (.8, .9))
def conditional_mean(weights):
    return sum(k*w for k, w in enumerate(weights, 1)) / sum(weights)

window_rate = mp.findroot(
    lambda l: conditional_mean([l**k/mp.factorial(k) for k in range(1, 5)]) - mp.mpf(9)/4, (2, 3))
window_prob = mp.findroot(
    lambda p: conditional_mean([mp.binomial(7, k)*(p/(1-p))**k for k in range(1, 5)]) - mp.mpf(9)/4, (.25, .4))
def normal_variance(sd):
    w = lambda x: mp.exp(-(x/sd)**2/2)
    return mp.quad(lambda x: x*x*w(x), [0, 2]) / mp.quad(w, [0, 2])

sd = mp.findroot(lambda sd: normal_variance(sd) - mp.mpf(5)/8, (.7, .9))
shape = mp.findroot(lambda k: 2/k + mp.log(2) -
    2*sum(x**k*mp.log(x) for x in [1, 2, 3])/sum(x**k for x in [1, 2, 3]), (1, 3))
scale = (sum(x**shape for x in [1, 2, 3])/2)**(1/shape)
zip_held_rate = mp.findroot(lambda l: -l+mp.log(2)+mp.log(l-mp.mpf(3)/2)-mp.log(3-l)
    -mp.log(mp.mpf(1)/3)+mp.log(mp.mpf(2)/3), (mp.mpf('2.5'), mp.mpf('2.8')))
for name in ('rate', 'theta', 'window_rate', 'window_prob', 'sd', 'shape', 'scale', 'zip_held_rate'):
    print(name, mp.nstr(globals()[name], 50))
print('pi', mp.nstr(1-mp.mpf('.5')/-mp.expm1(-rate), 50))
