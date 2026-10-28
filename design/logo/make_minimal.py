"""Minimal neon hex-sticker candidates for univariateML (round 2).

Only a pipe, big lettering, and smoke puffs that are normal densities with
growing variance as they rise -- the smoke diffuses. Canvas is the usual
hexSticker 518 x 600 px.

    python3 make_minimal.py   # writes logo_{D,E,F}_src.svg next to this file
"""
import math
import os

W, H = 518, 600
CX, CY = 259, 300
HERE = os.path.dirname(os.path.abspath(__file__))


def hexpts(r, cx=CX, cy=CY):
    dx = r * math.cos(math.pi / 6)
    pts = [(cx, cy - r), (cx + dx, cy - r / 2), (cx + dx, cy + r / 2),
           (cx, cy + r), (cx - dx, cy + r / 2), (cx - dx, cy - r / 2)]
    return " ".join(f"{x:.2f},{y:.2f}" for x, y in pts)


def bell_pts(mu, sigma, base, amp, k=3.3, n=120):
    xs = [mu - k * sigma + 2 * k * sigma * i / (n - 1) for i in range(n)]
    return [(x, base - amp * math.exp(-0.5 * ((x - mu) / sigma) ** 2)) for x in xs]


def poly(pts):
    return f"M{pts[0][0]:.2f},{pts[0][1]:.2f}" + "".join(f" L{x:.2f},{y:.2f}" for x, y in pts[1:])


def mix(c1, c2, t):
    a = [int(c1[i:i + 2], 16) for i in (1, 3, 5)]
    b = [int(c2[i:i + 2], 16) for i in (1, 3, 5)]
    return "#" + "".join(f"{round(x + (y - x) * t):02x}" for x, y in zip(a, b))


OUTER, INNER, TUBE = hexpts(298), hexpts(282), hexpts(270)

DEFS = f"""
  <clipPath id="hexclip"><polygon points="{INNER}"/></clipPath>
  <radialGradient id="bg" cx="259" cy="280" r="330" gradientUnits="userSpaceOnUse">
    <stop offset="0" stop-color="#1c1830"/>
    <stop offset="0.6" stop-color="#110f1a"/>
    <stop offset="1" stop-color="#0a090f"/>
  </radialGradient>
  <linearGradient id="tube" gradientUnits="userSpaceOnUse" x1="60" y1="40" x2="460" y2="560">
    <stop offset="0" stop-color="#a855f7"/>
    <stop offset="1" stop-color="#3b82f6"/>
  </linearGradient>
  <filter id="neon" x="-50%" y="-50%" width="200%" height="200%">
    <feGaussianBlur in="SourceGraphic" stdDeviation="2.5" result="b1"/>
    <feGaussianBlur in="SourceGraphic" stdDeviation="9" result="b2"/>
    <feMerge><feMergeNode in="b2"/><feMergeNode in="b2"/><feMergeNode in="b1"/><feMergeNode in="SourceGraphic"/></feMerge>
  </filter>
  <filter id="neonSoft" x="-20%" y="-20%" width="140%" height="140%">
    <feGaussianBlur in="SourceGraphic" stdDeviation="5" result="b"/>
    <feMerge><feMergeNode in="b"/><feMergeNode in="SourceGraphic"/></feMerge>
  </filter>
"""


def frame_open():
    return f"""<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">
<defs>{DEFS}</defs>
<polygon points="{OUTER}" fill="#23212d"/>
<polygon points="{INNER}" fill="url(#bg)"/>
<g clip-path="url(#hexclip)">
<polygon points="{TUBE}" fill="none" stroke="url(#tube)" stroke-width="4" filter="url(#neonSoft)"/>
<polygon points="{TUBE}" fill="none" stroke="#ede9fe" stroke-opacity="0.55" stroke-width="1.2"/>
"""


FRAME_CLOSE = "</g>\n</svg>\n"


def neon_path(d, glow, core, w=6.5, extra=""):
    return (f'<path d="{d}" fill="none" stroke="{glow}" stroke-width="{w}" stroke-linecap="round" '
            f'stroke-linejoin="round" filter="url(#neon)" {extra}/>\n'
            f'<path d="{d}" fill="none" stroke="{core}" stroke-width="{w * 0.38:.2f}" stroke-linecap="round" '
            f'stroke-linejoin="round" {extra}/>\n')


def pipe(bx, ry, glow="#94a3b8", core="#f1f5f9"):
    """Bent pipe in neon outline; (bx, ry) is the centre of the bowl rim."""
    bowl = (f"M{bx - 25},{ry} L{bx + 25},{ry} L{bx + 22},{ry + 40} "
            f"Q{bx},{ry + 64} {bx - 22},{ry + 40} Z")
    stem = (f"M{bx - 23},{ry + 22} C{bx - 52},{ry + 27} {bx - 84},{ry + 22} {bx - 116},{ry + 7} "
            f"M{bx - 21},{ry + 38} C{bx - 54},{ry + 43} {bx - 88},{ry + 38} {bx - 121},{ry + 21} "
            f"M{bx - 116},{ry + 7} L{bx - 121},{ry + 21} "
            f"M{bx - 117},{ry + 10} L{bx - 150},{ry} Q{bx - 156},{ry + 4} {bx - 152},{ry + 9} L{bx - 120},{ry + 19}")
    return neon_path(bowl + " " + stem, glow, core, w=6)


def smoke_ridgeline(mu, bases, sigmas, area, c_lo, c_hi, occlude=True, fade=False):
    """Normal densities with constant area (so peaks flatten as sigma grows).

    Drawn back (top) to front (bottom); each front puff hides what is behind it.
    """
    out = ""
    n = len(bases)
    for i in reversed(range(n)):
        amp = area / sigmas[i]
        pts = bell_pts(mu, sigmas[i], bases[i], amp)
        t = i / (n - 1)
        glow = mix(c_lo, c_hi, t)
        core = mix("#e0e7ff", "#fae8ff", t)
        op = f'stroke-opacity="{1 - 0.35 * t:.2f}"' if fade else ""
        if occlude:
            out += f'<path d="{poly(pts)} Z" fill="url(#bg)"/>\n'
        out += neon_path(poly(pts), glow, core, w=6, extra=op)
    return out


def title_line(y, size, parts):
    """parts: list of (text, glow, core). One neon line of Comfortaa Bold."""
    s = ""
    for glow_pass in (True, False):
        spans = ""
        for txt, glow, core in parts:
            if glow_pass:
                spans += f'<tspan fill="{glow}" stroke="{glow}" stroke-width="3">{txt}</tspan>'
            else:
                spans += f'<tspan fill="{core}">{txt}</tspan>'
        f = 'filter="url(#neon)"' if glow_pass else ""
        s += (f'<text x="259" y="{y}" text-anchor="middle" font-family="Comfortaa" font-weight="700" '
              f'font-size="{size}" {f}>{spans}</text>\n')
    return s


PURPLE = ("#a855f7", "#fdf4ff")
BLUE = ("#3b82f6", "#eff6ff")


def candidate_d():
    """Joy-Division-style ridgeline of five puffs, one-line title."""
    bx, ry = 268, 350
    s = frame_open()
    s += smoke_ridgeline(bx, [334, 288, 242, 196, 150], [11 * 1.4 ** i for i in range(5)],
                         area=1050, c_lo="#3b82f6", c_hi="#c026d3")
    s += pipe(bx, ry)
    s += title_line(462, 48, [("univariate",) + PURPLE, ("ML",) + BLUE])
    return s + FRAME_CLOSE


def candidate_e():
    """Four separate puffs, no overlap, fading as they rise; one-line title."""
    bx, ry = 268, 350
    s = frame_open()
    bases = [330, 268, 206, 144]
    sigmas = [12, 19, 29, 40]
    amps = [46, 42, 38, 34]
    n = len(bases)
    for i in range(n):
        t = i / (n - 1)
        pts = bell_pts(bx, sigmas[i], bases[i], amps[i])
        op = f'stroke-opacity="{1 - 0.4 * t:.2f}"'
        s += neon_path(poly(pts), mix("#3b82f6", "#c026d3", t), mix("#e0e7ff", "#fae8ff", t), w=6, extra=op)
    s += pipe(bx, ry)
    s += title_line(462, 48, [("univariate",) + PURPLE, ("ML",) + BLUE])
    return s + FRAME_CLOSE


def candidate_f():
    """Ridgeline smoke, title split over two lines with a huge ML."""
    bx, ry = 264, 286
    s = frame_open()
    s += smoke_ridgeline(bx, [270, 230, 190, 150, 110], [10 * 1.4 ** i for i in range(5)],
                         area=900, c_lo="#3b82f6", c_hi="#c026d3")
    s += pipe(bx, ry)
    s += title_line(416, 54, [("univariate",) + PURPLE])
    s += title_line(506, 92, [("ML",) + BLUE])
    return s + FRAME_CLOSE


if __name__ == "__main__":
    for name, fn in (("D", candidate_d), ("E", candidate_e), ("F", candidate_f)):
        with open(os.path.join(HERE, f"logo_{name}_src.svg"), "w") as fh:
            fh.write(fn())
    print("ok")
