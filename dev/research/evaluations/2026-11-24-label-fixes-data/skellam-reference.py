#!/usr/bin/env python3
"""Compute independent 80-digit face scores and nearby stationary points."""

import json

import mpmath as mp

mp.mp.dps = 80


def likelihood(counts, a, b):
    q = a * b
    return sum(
        multiplicity * (
            -a - b + k * mp.log(a) - mp.loggamma(k + 1)
            + mp.log(mp.hyp0f1(k + 1, q))
        )
        for k, multiplicity in counts.items()
    )


def profile_case(counts, seed=None):
    n = sum(counts.values())
    mean = mp.mpf(sum(k * multiplicity for k, multiplicity in counts.items())) / n
    profile = lambda t: likelihood(counts, mean + t, t)
    score = mean * sum(mp.mpf(count) / (k + 1) for k, count in counts.items()) / n - 1
    row = {
        "counts": counts,
        "face_rate": mp.nstr(mean, 30),
        "face_inward_score_per_observation": mp.nstr(score, 30),
    }
    if seed:
        t = mp.findroot(lambda t: mp.diff(profile, t), mp.mpf(seed))
        row.update({
            "stationary_smaller_rate": mp.nstr(t, 30),
            "stationary_gain_per_observation": mp.nstr((profile(t) - profile(0)) / n, 30),
        })
    return row


strict = profile_case({1: 1, 2: 1})
stationary = profile_case({0: 1, 3: 2})
inside_margin = profile_case({0: 17, 4: 161}, "0.00006")
outside_margin = profile_case({0: 15, 4: 142}, "0.0002")
negative_score = profile_case({0: 19, 20: 1})
negative_score["interior_witness_rates"] = [1.5, 0.5]
negative_score["interior_witness_gain_per_observation"] = mp.nstr((
    likelihood({0: 19, 20: 1}, mp.mpf("1.5"), mp.mpf("0.5"))
    - likelihood({0: 19, 20: 1}, mp.mpf(1), mp.mpf(0))
) / 20, 30)

# Use the exact supplied binary64 value for the held rate.
held_rate = mp.mpf(1.00005)
held = lambda t: likelihood({0: 1}, held_rate, t)
held_root = mp.findroot(lambda t: mp.diff(held, t), mp.mpf("0.0001"))
print(json.dumps({
    "precision_decimal_digits": mp.mp.dps,
    "joint_strict_face": strict,
    "joint_stationary_face": stationary,
    "joint_inside_margin": inside_margin,
    "joint_outside_margin": outside_margin,
    "joint_negative_score_refuted": negative_score,
    "held_rate": {
        "value": mp.nstr(held_rate, 30),
        "face_inward_score_per_observation": mp.nstr(held_rate - 1, 30),
        "stationary_free_rate": mp.nstr(held_root, 30),
        "stationary_gain_per_observation": mp.nstr(held(held_root) - held(0), 30),
    },
}, indent=2))
