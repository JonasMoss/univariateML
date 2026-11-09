# Pearson system geometry: numerical and symbolic checks

Checks behind the geometry notes in the
[Pearson system roadmap entry](../../../roadmap/2026-11-08-pearson-system.md#geometry-of-the-system-working-notes-2026-11-09)
(TASK-166). Standalone scripts; they do not use the library. Written and
run 2026-11-09 on master at `dcf0832a`, Python 3.14.5, mpmath 1.3.0,
sympy 1.14.0, one core under `nice -n 10`. Each `.log` is the full output.

Hellinger convention: $H^2(f,g)=\int(\sqrt f-\sqrt g)^2$, range $[0,2]$.

| script | claim checked | result |
|---|---|---|
| `typev_face.py` | Types IV and VI share one expansion through type V: $\log f=\log f_V+\delta\,(s(y)-\mathbb E_Vs)+O(\delta^2)$ with $\delta=1/\nu^2$ (IV, $\alpha=\beta/\nu$) and $\delta=-\gamma^2/\beta^2$ (VI, roots $\lambda\pm\gamma$), $s=-m\beta^2/y^2+\beta^3/(3y^3)$, $y=\lambda-x$, $\mathbb E_Vs=(2m-1)(2m)(1-m)/3$. Normalised densities, 60 digits. | For $m\in\{1.7,0.8\}$, $\beta=1.3$, three points, $\delta\in\{10^{-3},5\cdot10^{-4},2.5\cdot10^{-4}\}$: the residual after the first-order term halves with $\delta$, and its $\delta^2$ coefficient agrees on the two sides within about 1.5% (the next order). First order only; not a DQM proof. |
| `heavy_tail_end.py` | Type VII with $d=2m-1\to0$: location information $d(d+1)/((d+3)\alpha^2)$; $H^2\to0$ like $d$ at a fixed location gap; $H^2=2d\log(\Delta/\alpha)+O(d)$ for large gaps. | Small gap ($\Delta=10^{-3}$): $H^2/(\Delta^2/4)$ equals the information to 6 digits at $d=0.5,0.1,0.01$. Gap $\Delta=\alpha$: $H^2/d=0.081,0.077,0.077$ at $d=0.1,0.01,0.001$. At $d=10^{-3}$, $H^2/d-2\log\Delta=-3.01,-4.05,-4.53,-4.66$ for $\Delta=10,10^2,10^4,10^6$ (remaining drift is the $d\log^2\Delta$ term). |
| `endpoint_modulus.py` | Free endpoint with exponent $p$ (gamma, shape $p+1$, moving threshold): $H^2(h)\asymp h^{p+1}$ for $p<1$; $\tfrac14h^2(\log(1/h)+O(1))$ at $p=1$; $Ih^2/4$ with $I=1/(p-1)$ for $p>1$. The threshold curve in $L^2$ behaves like fractional Brownian motion with Hurst index $s=\min((p+1)/2,1)$: chords over $[0,h]$ and $[2h,3h]$ have cosine $(1+3^{2s}-2^{2s+1})/2$. 50 digits, $h=10^{-2},\dots,10^{-7}$. | Log-slopes 0.5000, 1.0000, 1.4998, 1.94 (rising slowly to 2), 1.9998 for $p=-0.5,0,0.5,1,1.5$. Chord cosines at $h=10^{-7}$: $-0.0482$ (predicted $-0.048$), $-0.0000$ (0), $+0.2695$ (0.270), $+0.875$ (rising to 1), $+0.9997$ (1). $p=1$: $H^2=h^2(\log(1/h)+1.31)/4$. $p=1.5$: $H^2/h^2\to0.49991$ (target 0.5). |
| `spectral_check.py` | Spectral form of the endpoint geometry: $\varphi(h)=\frac1\pi\int(1-\cos\omega h)\lvert\hat g\rvert^2d\omega$ and $I=\frac2\pi\int\omega^2\lvert\hat g\rvert^2d\omega$ for $g=\sqrt f$, gamma with closed-form $\lvert\hat g\rvert^2\sim\lvert\omega\rvert^{-(p+2)}$. Reuses `H2()` from `endpoint_modulus.py`. | $p=0.5$: spectral and direct $\varphi$ agree to 8 digits at $h=0.1,0.01$; $p=1.5$: to 10 digits; spectral $I=2.0000000000$ (target $1/(p-1)=2$). |
| `algebra_checks.py` | In signed skewness the type III and type V curves are $\beta_2=3+k\gamma_1^2$ with $k=3/2$ and $15/8$ (tangent at the normal); the type III quadratics form the tangent line $c_2=0$ to the double-root conic $c_1^2=4c_0c_2$ at the normal; Lomax $[0:\alpha+1:0:\lambda:1]\to[0:r:0:1:0]$ (exponential) as $\alpha/\lambda\to r$. | All assertions pass. |

Reproduce: `nice -n 10 python3 <script>` in this directory (seconds to a
few minutes each).
