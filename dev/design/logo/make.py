"""Generate three hex-sticker logo candidates for univariateML.

Canvas follows the hexSticker convention: 518 x 600 px, pointy-top hexagon.
"""
import math
import os
import random

W, H = 518, 600
CX, CY = 259, 300


def hexpts(r, cx=CX, cy=CY):
    dx = r * math.cos(math.pi / 6)
    pts = [(cx, cy - r), (cx + dx, cy - r / 2), (cx + dx, cy + r / 2),
           (cx, cy + r), (cx - dx, cy + r / 2), (cx - dx, cy - r / 2)]
    return " ".join(f"{x:.2f},{y:.2f}" for x, y in pts)


def bell(x0, x1, mu, sigma, base, amp, n=90, reverse=False):
    xs = [x0 + (x1 - x0) * i / (n - 1) for i in range(n)]
    if reverse:
        xs = xs[::-1]
    pts = [(x, base - amp * math.exp(-0.5 * ((x - mu) / sigma) ** 2)) for x in xs]
    return pts


def poly(pts, start="M"):
    head = f"{start}{pts[0][0]:.2f},{pts[0][1]:.2f}"
    return head + "".join(f" L{x:.2f},{y:.2f}" for x, y in pts[1:])


OUTER, INNER, LINE = hexpts(298), hexpts(282), hexpts(274)

COMMON_DEFS = f"""
  <clipPath id="hexclip"><polygon points="{INNER}"/></clipPath>
  <linearGradient id="border" gradientUnits="userSpaceOnUse" x1="40" y1="60" x2="480" y2="560">
    <stop offset="0" stop-color="#a78bfa"/>
    <stop offset="0.5" stop-color="#7c3aed"/>
    <stop offset="1" stop-color="#2563eb"/>
  </linearGradient>
  <linearGradient id="mlgrad" gradientUnits="userSpaceOnUse" x1="330" y1="0" x2="420" y2="0">
    <stop offset="0" stop-color="#c4b5fd"/>
    <stop offset="1" stop-color="#60a5fa"/>
  </linearGradient>
"""


def frame_open(extra_defs, bg_fill):
    return f"""<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
     width="{W}" height="{H}" viewBox="0 0 {W} {H}">
<defs>{COMMON_DEFS}{extra_defs}
</defs>
<polygon points="{OUTER}" fill="url(#border)"/>
<polygon points="{INNER}" fill="{bg_fill}"/>
<g clip-path="url(#hexclip)">
"""


def frame_close(line_color="#ffffff", line_opacity=0.10):
    return f"""</g>
<polygon points="{LINE}" fill="none" stroke="{line_color}" stroke-opacity="{line_opacity}" stroke-width="2"/>
</svg>
"""


def title(y=492, size=44, fill="#ece9f5", ml_fill="url(#mlgrad)", family="Inter",
          weight=700, shadow="#0b0a12"):
    return f"""<text x="259" y="{y}" text-anchor="middle" font-family="{family}" font-weight="{weight}"
      font-size="{size}" fill="{fill}" stroke="{shadow}" stroke-width="7" stroke-linejoin="round"
      paint-order="stroke" letter-spacing="0.5">univariate<tspan font-weight="900" fill="{ml_fill}">ML</tspan></text>
"""


# ---------------------------------------------------------------------------
# Candidate A: portrait. Fisher, front view, pipe smoke curls into a
# likelihood curve whose maximum sits right above his bald head.
# ---------------------------------------------------------------------------
def candidate_a():
    ink = "#14121c"
    defs = """
  <radialGradient id="bgA" cx="259" cy="230" r="330" gradientUnits="userSpaceOnUse">
    <stop offset="0" stop-color="#2b2547"/>
    <stop offset="0.55" stop-color="#16131f"/>
    <stop offset="1" stop-color="#08080d"/>
  </radialGradient>
  <radialGradient id="skin" cx="215" cy="245" r="150" gradientUnits="userSpaceOnUse">
    <stop offset="0" stop-color="#f4f2fa"/>
    <stop offset="0.6" stop-color="#d9d5e6"/>
    <stop offset="1" stop-color="#aaa4c0"/>
  </radialGradient>
  <radialGradient id="beard" cx="225" cy="350" r="110" gradientUnits="userSpaceOnUse">
    <stop offset="0" stop-color="#ffffff"/>
    <stop offset="1" stop-color="#bdb8cf"/>
  </radialGradient>
  <radialGradient id="lens" cx="0.4" cy="0.35" r="0.7">
    <stop offset="0" stop-color="#e0e7ff" stop-opacity="0.55"/>
    <stop offset="1" stop-color="#818cf8" stop-opacity="0.35"/>
  </radialGradient>
  <linearGradient id="bowl" x1="0" y1="0" x2="1" y2="0">
    <stop offset="0" stop-color="#4a4658"/>
    <stop offset="0.45" stop-color="#25232f"/>
    <stop offset="1" stop-color="#0e0d14"/>
  </linearGradient>
  <radialGradient id="ember" cx="0.5" cy="0.5" r="0.5">
    <stop offset="0" stop-color="#fdf4ff"/>
    <stop offset="0.45" stop-color="#e879f9"/>
    <stop offset="1" stop-color="#7c3aed"/>
  </radialGradient>
  <linearGradient id="curveA" gradientUnits="userSpaceOnUse" x1="60" y1="0" x2="440" y2="0">
    <stop offset="0" stop-color="#60a5fa" stop-opacity="0.2"/>
    <stop offset="0.25" stop-color="#818cf8"/>
    <stop offset="0.6" stop-color="#c084fc"/>
    <stop offset="1" stop-color="#a78bfa"/>
  </linearGradient>
  <linearGradient id="smokeA" gradientUnits="userSpaceOnUse" x1="0" y1="340" x2="0" y2="195">
    <stop offset="0" stop-color="#9ca3af" stop-opacity="0.15"/>
    <stop offset="0.5" stop-color="#a5b4fc" stop-opacity="0.6"/>
    <stop offset="1" stop-color="#c084fc" stop-opacity="0.95"/>
  </linearGradient>
  <linearGradient id="areaA" gradientUnits="userSpaceOnUse" x1="0" y1="70" x2="0" y2="200">
    <stop offset="0" stop-color="#a855f7" stop-opacity="0.35"/>
    <stop offset="1" stop-color="#3b82f6" stop-opacity="0.0"/>
  </linearGradient>
  <filter id="blur3" x="-30%" y="-30%" width="160%" height="160%"><feGaussianBlur stdDeviation="3.5"/></filter>
  <filter id="glowA" x="-100%" y="-100%" width="300%" height="300%">
    <feGaussianBlur in="SourceGraphic" stdDeviation="4" result="b"/>
    <feMerge><feMergeNode in="b"/><feMergeNode in="SourceGraphic"/></feMerge>
  </filter>
"""
    s = frame_open(defs, "url(#bgA)")

    # likelihood curve (smoke) ------------------------------------------------
    mu, sig, base, amp = 240, 56, 198, 128
    pts = bell(62, 428, mu, sig, base, amp, reverse=True)  # right -> left
    area = poly(bell(62, 428, mu, sig, base, amp)) + f" L428,{base} L62,{base} Z"
    s += f'<path d="{area}" fill="url(#areaA)"/>\n'
    smoke_d = ("M347,338 C333,318 366,304 356,286 C346,268 380,262 400,250 "
               "C424,236 452,222 446,208 C442,199 434,197.9 428,197.9")
    s += f'<path d="{smoke_d}" fill="none" stroke="#c7d2fe" stroke-opacity="0.18" stroke-width="18" stroke-linecap="round" filter="url(#blur3)"/>\n'
    s += f'<path d="{smoke_d}" fill="none" stroke="url(#smokeA)" stroke-width="5" stroke-linecap="round"/>\n'
    # extra faint wisps
    s += '<path d="M352,330 C372,316 352,300 372,286 C388,275 382,262 396,256" fill="none" stroke="#a5b4fc" stroke-opacity="0.25" stroke-width="3" stroke-linecap="round"/>\n'
    s += '<path d="M340,326 C326,312 342,298 332,286" fill="none" stroke="#a5b4fc" stroke-opacity="0.2" stroke-width="3" stroke-linecap="round"/>\n'
    s += f'<path d="{poly(pts)}" fill="none" stroke="url(#curveA)" stroke-width="6" stroke-linecap="round" stroke-linejoin="round"/>\n'
    # MLE marker: dashed drop line onto his head
    s += f'<line x1="{mu}" y1="{base - amp + 10}" x2="{mu}" y2="176" stroke="#ddd6fe" stroke-width="2.5" stroke-dasharray="5 6" stroke-linecap="round"/>\n'
    s += f'<circle cx="{mu}" cy="{base - amp}" r="7" fill="#f5f3ff" filter="url(#glowA)"/>\n'
    s += (f'<text x="{mu + 14}" y="{base - amp - 8}" font-family="STIX Two Text" font-style="italic" '
          f'font-size="26" fill="#ddd6fe">θ</text>'
          f'<path d="M{mu + 16},{base - amp - 30} l5,-5 l5,5" fill="none" stroke="#ddd6fe" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>\n')

    # jacket ----------------------------------------------------------------
    s += f"""<path d="M70,600 L70,478 C76,440 130,414 192,402 L288,402 C350,414 404,440 410,478 L410,600 Z"
      fill="#1e1c2a" stroke="#4b4760" stroke-width="3"/>
<path d="M200,402 L240,458 L280,402 Z" fill="#ebe9f3" stroke="{ink}" stroke-width="2.5" stroke-linejoin="round"/>
<path d="M192,402 L240,470 L214,420 Z M288,402 L240,470 L266,420 Z" fill="#2c2a3b" stroke="#4b4760" stroke-width="2"/>
"""
    # ears, head -----------------------------------------------------------
    s += f"""<ellipse cx="157" cy="302" rx="15" ry="24" fill="url(#skin)" stroke="{ink}" stroke-width="3.5"/>
<ellipse cx="323" cy="302" rx="15" ry="24" fill="url(#skin)" stroke="{ink}" stroke-width="3.5"/>
<path d="M155,304 C150,296 152,290 158,288" fill="none" stroke="#8d88a3" stroke-width="2.5" stroke-linecap="round"/>
<path d="M325,304 C330,296 328,290 322,288" fill="none" stroke="#8d88a3" stroke-width="2.5" stroke-linecap="round"/>
<ellipse cx="240" cy="290" rx="84" ry="112" fill="url(#skin)" stroke="{ink}" stroke-width="4"/>
<ellipse cx="210" cy="207" rx="20" ry="9" transform="rotate(-28 210 207)" fill="#ffffff" fill-opacity="0.75"/>
<path d="M198,232 Q240,222 282,232" fill="none" stroke="#8d88a3" stroke-width="2.5" stroke-linecap="round"/>
<path d="M210,246 Q240,239 270,246" fill="none" stroke="#8d88a3" stroke-width="2.5" stroke-linecap="round"/>
"""
    # side hair tufts (mirror about x=240)
    tuft = (f'<path d="M180,236 C164,230 148,242 154,256 C140,262 142,280 154,284 '
            f'C142,294 148,312 162,312 C166,300 170,292 176,284 C170,270 172,252 180,236 Z" '
            f'fill="#f6f5fa" stroke="{ink}" stroke-width="3" stroke-linejoin="round"/>')
    s += tuft + "\n"
    s += f'<g transform="translate(480,0) scale(-1,1)">{tuft}</g>\n'
    # eyebrows
    brow = (f'<path d="M176,266 C182,248 210,242 230,256 C220,254 214,256 206,258 '
            f'C198,258 186,262 176,266 Z" fill="#f6f5fa" stroke="{ink}" stroke-width="2.5" stroke-linejoin="round"/>')
    s += brow + f'<g transform="translate(480,0) scale(-1,1)">{brow}</g>\n'
    # beard + nose + pipe + mustache --------------------------------------
    s += f"""<path d="M160,300 C158,350 176,396 204,422 C218,434 230,442 240,448 C250,442 262,434 276,422
      C304,396 322,350 320,300 C312,322 300,330 288,334 C276,338 262,336 252,340 L228,340
      C218,336 204,338 192,334 C180,330 168,322 160,300 Z" fill="url(#beard)" stroke="{ink}" stroke-width="3.5" stroke-linejoin="round"/>
<g fill="none" stroke="#b3aec6" stroke-width="2.2" stroke-linecap="round">
  <path d="M182,360 C186,378 194,392 204,402"/><path d="M200,370 C204,388 212,402 222,414"/>
  <path d="M298,360 C294,378 286,392 276,402"/><path d="M280,374 C276,390 268,404 258,414"/>
  <path d="M232,392 C234,408 237,420 240,432"/><path d="M176,334 C176,346 178,356 182,364"/>
  <path d="M304,334 C304,346 302,356 298,364"/>
</g>
<path d="M232,300 C228,314 220,326 226,335 C232,344 248,344 254,335 C260,326 252,314 248,300"
      fill="#d6d1e4" stroke="{ink}" stroke-width="3" stroke-linejoin="round"/>
<ellipse cx="234" cy="326" rx="4" ry="3" fill="#ffffff" fill-opacity="0.8"/>
<path d="M252,366 C276,372 300,380 334,384" fill="none" stroke="#0d0c12" stroke-width="10" stroke-linecap="round"/>
<path d="M256,363 C278,368 300,375 330,379" fill="none" stroke="#5a566b" stroke-width="2" stroke-linecap="round"/>
<path d="M327,343 L367,343 C367,370 365,389 357,397 C350,404 338,404 333,397 C327,389 325,370 327,343 Z"
      fill="url(#bowl)" stroke="{ink}" stroke-width="3" stroke-linejoin="round"/>
<path d="M333,352 C332,368 333,382 337,390" fill="none" stroke="#7a7590" stroke-width="2" stroke-linecap="round"/>
<ellipse cx="347" cy="343" rx="20" ry="5.5" fill="#0a0a0f" stroke="{ink}" stroke-width="2"/>
<ellipse cx="347" cy="342.5" rx="14" ry="3.6" fill="url(#ember)" filter="url(#glowA)"/>
<path d="M240,338 C224,333 204,339 197,352 C193,362 199,371 206,367 C213,357 226,353 240,357
      C254,353 267,357 274,367 C281,371 287,362 283,352 C276,339 256,333 240,338 Z"
      fill="#f6f5fa" stroke="{ink}" stroke-width="3" stroke-linejoin="round"/>
<path d="M216,348 C224,345 232,346 238,349 M264,348 C256,345 248,346 242,349" fill="none" stroke="#b3aec6" stroke-width="2" stroke-linecap="round"/>
"""
    # glasses (drawn last so they sit on top) ------------------------------
    for gx in (206, 274):
        s += f"""<circle cx="{gx}" cy="292" r="29" fill="url(#lens)"/>
<circle cx="{gx}" cy="292" r="20" fill="none" stroke="#ffffff" stroke-opacity="0.28" stroke-width="1.6"/>
<circle cx="{gx}" cy="292" r="12" fill="none" stroke="#ffffff" stroke-opacity="0.22" stroke-width="1.4"/>
<circle cx="{gx + 3}" cy="288" r="4.5" fill="{ink}"/>
<circle cx="{gx}" cy="292" r="29" fill="none" stroke="{ink}" stroke-width="7"/>
<path d="M{gx - 19},282 A21,21 0 0 1 {gx - 6},272" fill="none" stroke="#ffffff" stroke-opacity="0.85" stroke-width="3" stroke-linecap="round"/>
"""
    s += f"""<path d="M235,290 Q240,281 245,290" fill="none" stroke="{ink}" stroke-width="6" stroke-linecap="round"/>
<path d="M177,288 L157,283 M303,288 L323,283" fill="none" stroke="{ink}" stroke-width="5" stroke-linecap="round"/>
"""
    s += title()
    s += frame_close()
    return s


# ---------------------------------------------------------------------------
# Candidate B: "gone fishing". Fisher in a rowboat at night has hooked the
# maximum of a likelihood curve rising out of the sea.
# ---------------------------------------------------------------------------
def candidate_b():
    ink = "#12111a"
    defs = """
  <linearGradient id="skyB" gradientUnits="userSpaceOnUse" x1="0" y1="0" x2="0" y2="390">
    <stop offset="0" stop-color="#0b0a16"/>
    <stop offset="0.7" stop-color="#1d1840"/>
    <stop offset="1" stop-color="#2c2560"/>
  </linearGradient>
  <linearGradient id="seaB" gradientUnits="userSpaceOnUse" x1="0" y1="388" x2="0" y2="600">
    <stop offset="0" stop-color="#1e3a8a"/>
    <stop offset="0.35" stop-color="#14204d"/>
    <stop offset="1" stop-color="#080a18"/>
  </linearGradient>
  <linearGradient id="humpB" gradientUnits="userSpaceOnUse" x1="0" y1="215" x2="0" y2="390">
    <stop offset="0" stop-color="#a855f7"/>
    <stop offset="1" stop-color="#3b82f6"/>
  </linearGradient>
  <radialGradient id="skinB" cx="0.4" cy="0.35" r="0.7">
    <stop offset="0" stop-color="#f4f2fa"/>
    <stop offset="1" stop-color="#b7b1cc"/>
  </radialGradient>
  <radialGradient id="emberB" cx="0.5" cy="0.5" r="0.5">
    <stop offset="0" stop-color="#fdf4ff"/>
    <stop offset="0.5" stop-color="#e879f9"/>
    <stop offset="1" stop-color="#7c3aed"/>
  </radialGradient>
  <filter id="glowB" x="-100%" y="-100%" width="300%" height="300%">
    <feGaussianBlur in="SourceGraphic" stdDeviation="5" result="b"/>
    <feMerge><feMergeNode in="b"/><feMergeNode in="SourceGraphic"/></feMerge>
  </filter>
  <filter id="softB" x="-30%" y="-30%" width="160%" height="160%"><feGaussianBlur stdDeviation="2.5"/></filter>
"""
    s = frame_open(defs, "url(#skyB)")
    rnd = random.Random(3)
    for _ in range(38):
        x, y = rnd.uniform(20, 500), rnd.uniform(20, 330)
        r = rnd.choice([1.0, 1.2, 1.5, 2.0])
        s += f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r}" fill="#e0e7ff" fill-opacity="{rnd.uniform(0.35, 0.9):.2f}"/>\n'
    # moon (crescent)
    s += ('<mask id="moonMask"><rect width="518" height="600" fill="#fff"/>'
          '<circle cx="154" cy="108" r="27" fill="#000"/></mask>'
          '<g mask="url(#moonMask)"><circle cx="140" cy="118" r="30" fill="#e5e3f0" filter="url(#glowB)"/></g>\n')

    # sea
    s += '<rect x="0" y="388" width="518" height="212" fill="url(#seaB)"/>\n'

    # likelihood hump rising from the sea + reflection
    mu, sig, base, amp = 398, 40, 390, 172
    hump = bell(292, 512, mu, sig, base, amp)
    hump_d = poly(hump) + f" L512,{base} L292,{base} Z"
    refl = [(x, base + (base - y) * 0.45) for x, y in hump]
    refl_d = poly(refl) + f" L512,{base} L292,{base} Z"
    s += f'<path d="{refl_d}" fill="#8b5cf6" fill-opacity="0.18" filter="url(#softB)"/>\n'
    s += f'<path d="{hump_d}" fill="url(#humpB)" fill-opacity="0.9"/>\n'
    s += f'<path d="{poly(hump)}" fill="none" stroke="#e9d5ff" stroke-width="4" stroke-linecap="round"/>\n'
    s += f'<line x1="{mu}" y1="{base - amp + 14}" x2="{mu}" y2="{base}" stroke="#f5f3ff" stroke-opacity="0.75" stroke-width="2.5" stroke-dasharray="5 6"/>\n'
    # waves
    for y0, op in ((392, 0.9), (410, 0.55), (430, 0.35)):
        d = f"M0,{y0}"
        for k in range(14):
            x = k * 40
            d += f" q10,-7 20,0 t20,0"
        s += f'<path d="{d}" fill="none" stroke="#60a5fa" stroke-opacity="{op}" stroke-width="3" stroke-linecap="round"/>\n'

    # boat
    s += f"""<path d="M42,362 L292,362 C284,394 262,414 230,416 L104,416 C74,412 52,392 42,362 Z"
      fill="#3d3a4f" stroke="{ink}" stroke-width="4" stroke-linejoin="round"/>
<path d="M52,384 C120,388 230,388 282,384" fill="none" stroke="{ink}" stroke-opacity="0.5" stroke-width="2.5"/>
<path d="M70,402 C130,405 220,405 268,401" fill="none" stroke="{ink}" stroke-opacity="0.5" stroke-width="2.5"/>
<rect x="38" y="356" width="258" height="10" rx="5" fill="#57536b" stroke="{ink}" stroke-width="3"/>
<text x="95" y="400" font-family="STIX Two Text" font-style="italic" font-size="22" fill="#c4b5fd">H<tspan font-size="14" dy="5">0</tspan></text>
"""
    # Fisher, profile facing right --------------------------------------
    s += f"""<path d="M140,362 L138,300 C140,282 158,270 178,270 C198,270 212,282 216,300 L222,362 Z"
      fill="#4b5563" stroke="{ink}" stroke-width="3.5" stroke-linejoin="round"/>
<path d="M150,300 L152,360 M206,300 L212,360" stroke="#374151" stroke-width="2"/>
<path d="M192,292 C206,306 218,316 234,318" fill="none" stroke="{ink}" stroke-width="18" stroke-linecap="round"/>
<path d="M192,292 C206,306 218,316 234,318" fill="none" stroke="#4b5563" stroke-width="12" stroke-linecap="round"/>
"""
    # rod + line + hook (rod passes through hand at ~(235,316))
    tip = (356, 150)
    s += f'<path d="M204,348 Q310,250 {tip[0]},{tip[1]}" fill="none" stroke="#1f1d29" stroke-width="5" stroke-linecap="round"/>\n'
    s += f'<path d="M204,348 Q310,250 {tip[0]},{tip[1]}" fill="none" stroke="#9ca3af" stroke-width="1.6" stroke-linecap="round"/>\n'
    s += f'<circle cx="222" cy="331" r="7" fill="#57536b" stroke="{ink}" stroke-width="2.5"/>\n'
    peak = (mu, base - amp)
    s += f'<path d="M{tip[0]},{tip[1]} L{peak[0]},{peak[1] - 8}" stroke="#e5e7eb" stroke-width="1.6"/>\n'
    s += f'<path d="M{peak[0]},{peak[1] - 9} L{peak[0]},{peak[1] + 4} a5,5 0 0 1 -10,0" fill="none" stroke="#e5e7eb" stroke-width="2.2" stroke-linecap="round"/>\n'
    s += f'<circle cx="{peak[0]}" cy="{peak[1]}" r="5" fill="#f5f3ff" filter="url(#glowB)"/>\n'
    s += f'<circle cx="235" cy="317" r="9" fill="url(#skinB)" stroke="{ink}" stroke-width="3"/>\n'
    # collar + bow tie
    s += f'<path d="M168,272 L182,288 L196,272" fill="#e5e3ee" stroke="{ink}" stroke-width="2.5" stroke-linejoin="round"/>\n'
    # head
    s += f"""<path d="M140,236 C138,200 160,184 184,184 C210,184 226,202 226,226 C226,236 224,246 222,252
      C220,262 214,268 204,272 L168,272 C152,266 141,254 140,236 Z"
      fill="url(#skinB)" stroke="{ink}" stroke-width="3.5" stroke-linejoin="round"/>
<ellipse cx="178" cy="198" rx="13" ry="6" transform="rotate(-20 178 198)" fill="#fff" fill-opacity="0.7"/>
<path d="M150,214 C138,218 134,232 140,240 C132,248 136,262 148,262 C152,252 156,240 160,232 C158,224 156,218 150,214 Z"
      fill="#f6f5fa" stroke="{ink}" stroke-width="2.5" stroke-linejoin="round"/>
<ellipse cx="166" cy="236" rx="8" ry="12" fill="url(#skinB)" stroke="{ink}" stroke-width="2.5"/>
<path d="M164,254 C170,276 190,292 214,294 C222,288 226,276 224,262 C214,262 204,258 196,250 C186,256 174,258 164,254 Z"
      fill="#f3f2f8" stroke="{ink}" stroke-width="3" stroke-linejoin="round"/>
<path d="M180,266 C188,276 198,282 210,284 M196,262 C202,270 210,274 218,276" fill="none" stroke="#b3aec6" stroke-width="1.8" stroke-linecap="round"/>
"""
    # pipe (held in mouth, bowl forward)
    s += f"""<path d="M218,258 C230,262 240,264 250,262" fill="none" stroke="#0d0c12" stroke-width="7" stroke-linecap="round"/>
<path d="M244,238 L266,238 C266,254 264,264 258,268 C253,271 248,269 246,264 C243,258 243,248 244,238 Z"
      fill="#27242f" stroke="{ink}" stroke-width="2.5" stroke-linejoin="round"/>
<ellipse cx="255" cy="238" rx="11" ry="3" fill="url(#emberB)" filter="url(#glowB)"/>
"""
    # nose + mustache + glasses + brow on top
    s += f"""<circle cx="228" cy="242" r="10" fill="#d6d1e4" stroke="{ink}" stroke-width="3"/>
<path d="M206,254 C214,250 224,250 230,256 C224,262 212,262 204,260 Z" fill="#f6f5fa" stroke="{ink}" stroke-width="2.5" stroke-linejoin="round"/>
<path d="M172,226 L200,224" stroke="{ink}" stroke-width="3.5" stroke-linecap="round"/>
<circle cx="208" cy="226" r="12" fill="#c7d2fe" fill-opacity="0.45" stroke="{ink}" stroke-width="4.5"/>
<circle cx="208" cy="226" r="6.5" fill="none" stroke="#fff" stroke-opacity="0.35" stroke-width="1.3"/>
<circle cx="211" cy="225" r="3" fill="{ink}"/>
<path d="M196,210 C204,202 216,202 224,210 C214,208 206,210 196,214 Z" fill="#f6f5fa" stroke="{ink}" stroke-width="2.2" stroke-linejoin="round"/>
"""
    # smoke curling up from the bowl, ending in a small bell curve
    s += ('<path d="M255,234 C246,216 266,204 256,188 C247,174 268,162 262,146" fill="none" stroke="#c7d2fe" '
          'stroke-opacity="0.2" stroke-width="14" stroke-linecap="round" filter="url(#softB)"/>\n')
    s += ('<path d="M255,234 C246,216 266,204 256,188 C247,174 268,162 262,146" fill="none" stroke="#c4b5fd" '
          'stroke-opacity="0.75" stroke-width="3.5" stroke-linecap="round"/>\n')
    for (rx, ry, x, y, op) in ((9, 5, 258, 132, 0.7), (13, 7, 252, 108, 0.5), (17, 9, 260, 80, 0.32)):
        s += f'<ellipse cx="{x}" cy="{y}" rx="{rx}" ry="{ry}" fill="none" stroke="#c4b5fd" stroke-opacity="{op}" stroke-width="4"/>\n'

    s += title(fill="#eef2ff", shadow="#070a18")
    s += frame_close()
    return s


# ---------------------------------------------------------------------------
# Candidate C: neon bar sign. Line-art Fisher in blue neon, pipe and the
# smoke-likelihood in purple neon, "est. 1922" (Fisher's ML paper).
# ---------------------------------------------------------------------------
def candidate_c():
    defs = """
  <pattern id="bricks" width="64" height="32" patternUnits="userSpaceOnUse">
    <rect width="64" height="32" fill="#101017"/>
    <path d="M0,0.5 H64 M0,16.5 H64 M0.5,0 V16 M32.5,16 V32" stroke="#1d1d28" stroke-width="2"/>
  </pattern>
  <radialGradient id="vignette" cx="259" cy="300" r="320" gradientUnits="userSpaceOnUse">
    <stop offset="0" stop-color="#1b1530" stop-opacity="0.35"/>
    <stop offset="1" stop-color="#000000" stop-opacity="0.8"/>
  </radialGradient>
  <filter id="neon" x="-50%" y="-50%" width="200%" height="200%">
    <feGaussianBlur in="SourceGraphic" stdDeviation="3" result="b1"/>
    <feGaussianBlur in="SourceGraphic" stdDeviation="10" result="b2"/>
    <feMerge><feMergeNode in="b2"/><feMergeNode in="b2"/><feMergeNode in="b1"/><feMergeNode in="SourceGraphic"/></feMerge>
  </filter>
"""
    fisher = """<g id="fisherArt" fill="none" stroke-linecap="round" stroke-linejoin="round">
  <path d="M160,306 C150,226 186,180 228,180 C270,180 306,226 296,306"/>
  <path d="M190,206 Q200,196 214,192"/>
  <path d="M161,292 C146,290 145,318 162,320"/>
  <path d="M295,292 C310,290 311,318 294,320"/>
  <circle cx="199" cy="290" r="25"/>
  <circle cx="257" cy="290" r="25"/>
  <path d="M224,289 Q228,281 232,289"/>
  <path d="M176,258 C188,248 206,248 218,256"/>
  <path d="M238,256 C250,248 268,248 280,258"/>
  <path d="M224,312 C218,326 222,336 230,336 C238,336 240,328 236,320"/>
  <path d="M194,350 C206,336 222,338 228,346 C234,338 250,336 262,350"/>
  <path d="M164,322 C164,352 172,380 186,400 C190,410 198,412 200,404 C206,418 216,424 220,416 C224,432 232,432 236,416 C240,424 250,418 256,404 C258,412 266,410 270,400 C284,380 292,352 292,322"/>
  <path d="M184,368 C188,380 194,388 200,392"/>
  <path d="M272,368 C268,380 262,388 256,392"/>
  <path d="M228,370 L228,396"/>
  <path d="M168,262 C150,262 146,278 156,284 M168,248 C152,240 142,252 148,262"/>
  <path d="M288,262 C306,262 310,278 300,284 M288,248 C304,240 314,252 308,262"/>
</g>"""
    pipe = """<g id="pipeArt" fill="none" stroke-linecap="round" stroke-linejoin="round">
  <path d="M244,360 L300,374"/>
  <path d="M298,342 L334,342 L330,384 Q316,396 302,384 Z"/>
</g>"""
    mu, sig, base, amp = 398, 32, 228, 100
    curve = poly(bell(320, 490, mu, sig, base, amp))
    smoke = (f"M316,336 C306,316 336,306 328,290 C320,274 342,262 336,248 "
             f"C332,238 326,232 320,{base}")
    smoke_art = f"""<g id="smokeArt" fill="none" stroke-linecap="round" stroke-linejoin="round">
  <path d="{smoke}"/>
  <path d="{curve}"/>
  <path d="M{mu},{base - amp + 12} L{mu},{base}" stroke-dasharray="7 9"/>
</g>"""
    defs += fisher + pipe + smoke_art
    s = frame_open(defs, "url(#bricks)")
    s += '<rect x="0" y="0" width="518" height="600" fill="url(#vignette)"/>\n'

    def neon(ref, glow, core, w):
        return (f'<use xlink:href="#{ref}" stroke="{glow}" stroke-width="{w}" filter="url(#neon)"/>\n'
                f'<use xlink:href="#{ref}" stroke="{core}" stroke-width="{w * 0.38:.1f}"/>\n')

    s += neon("fisherArt", "#3b82f6", "#dbeafe", 7)
    s += neon("pipeArt", "#a855f7", "#f3e8ff", 7)
    s += neon("smokeArt", "#c026d3", "#fae8ff", 6)
    s += f'<circle cx="{mu}" cy="{base - amp}" r="6" fill="#fdf4ff" filter="url(#neon)"/>\n'

    # neon title (Comfortaa reads like bent glass tubing)
    for glow, core in ((True, False), (False, True)):
        f = 'filter="url(#neon)"' if glow else ''
        sw = 3 if glow else 0
        s += (f'<text x="259" y="486" text-anchor="middle" font-family="Comfortaa" font-weight="700" '
              f'font-size="43" {f}>'
              f'<tspan fill="{"#a855f7" if glow else "#fdf4ff"}" stroke="#a855f7" stroke-width="{sw}">univariate</tspan>'
              f'<tspan fill="{"#3b82f6" if glow else "#eff6ff"}" stroke="#3b82f6" stroke-width="{sw}">ML</tspan></text>\n')
    small = ('<text x="259" y="{y}" text-anchor="middle" font-family="Inter" font-weight="600" '
             'font-size="15" letter-spacing="5" fill="none" stroke="{c}" stroke-width="{w}" {f}>EST. 1922</text>\n')
    s += small.format(y=524, c="#3b82f6", w=2.2, f='filter="url(#neon)"')
    s += small.format(y=524, c="#dbeafe", w=0.9, f='')
    s += frame_close("#a78bfa", 0.35)
    return s


if __name__ == "__main__":
    for name, fn in (("A", candidate_a), ("B", candidate_b), ("C", candidate_c)):
        with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), f"logo_{name}_src.svg"), "w") as fh:
            fh.write(fn())
    print("ok")
