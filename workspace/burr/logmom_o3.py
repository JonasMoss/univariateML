def Finv(p, *, eps=2.0**-53):
    if not 0.0 < p < 1.0:
        raise ValueError("p must be in (0,1)")
    # Constants ---------------------------------------
    C  = mp.polygamma(1, 1)      # trigamma(1)
    A  = mp.polygamma(2, 1)      # tetragamma(1)
    K  = 2 - A / C**1.5
    # Fast start --------------------------------------
    if p < 0.08:
        u  = u0_left(p, A, C, K)
    elif p > 0.92:
        u  = u0_right(p, A, C, K)
    else:
        u  = u0_mid(p)
    # One guarded Halley iteration --------------------
    for _ in range(2):           # 1‑2 steps are enough
        t   = mp.e**u
        D   = mp.polygamma(1, t)
        B   = mp.polygamma(2, t)
        F   = (2 - (A - B)/(C + D)**1.5) / K
        if abs(F - p) < 5*eps*max(p,1-p):
            return u             # done
        # derivative from (5):
        dB  = mp.polygamma(3, t)*t
        dD  = B*t
        G1  = 1.5*(A - B)
        G2  = G1 - dB
        Fp  = -(G1*dB + G2*dD) / (K*(C+D)**2.5)
        # second derivative for Halley
        Fpp = ...                # algebra or automatic diff
        delta = (F - p)/Fp
        u   -= 2*delta / (1 - Fpp*delta/Fp/3)   # cubic Halley
    # Fallback bracketing (should be unused)
    return mp.findroot(lambda x: F_of_u(x)-p, [u-1,u+1])
