"""Enclose the skew certificate and both face fits on exact archive inputs."""
from fractions import Fraction
import gzip
import json
from pathlib import Path

from mpmath import iv

iv.dps = 110
HERE = Path(__file__).resolve().parent
ARCHIVE = HERE.parent / "2026-11-07-fit-audit-data/cases.json.gz"
IDS = (1625, 1628)


def enclosing_decimal(value, places):
    """Round each exact dyadic endpoint outwards for the JSON transcript."""
    def endpoint(raw):
        sign, mantissa, exponent, _ = raw
        return (-1)**sign * mantissa * Fraction(2)**exponent

    scale = 10**places
    lower, upper = (endpoint(raw)*scale for raw in value._mpi_)
    lower = lower.numerator // lower.denominator
    upper = -((-upper.numerator) // upper.denominator)
    return [f"{k // scale}.{k % scale:0{places}d}" if k >= 0 else
            f"-{(-k) // scale}.{(-k) % scale:0{places}d}"
            for k in (lower, upper)]


def check(case):
    n = len(case["data"])
    power = iv.mpf(case["held"]["shape2"])
    threshold = iv.mpf(case["held"]["min"])
    excess = [iv.mpf(value)-threshold for value in case["data"]]
    assert all(value.a > 0 for value in excess)
    logs = [iv.ln(value) for value in excess]
    mean = sum(logs)/n
    centered = [power*(value-mean) for value in logs]
    span = power*(iv.ln(max(excess))-iv.ln(min(excess)))
    third = sum(value**3 for value in centered)/n
    bound = 69*span**4
    assert 0 < span.a and span.b <= iv.mpf(1)/100
    assert third.a > bound.b

    def face(sign):
        gap = iv.ln(sum(iv.exp(sign*value) for value in centered)/n)
        assert gap.a > 0
        k0 = 1/(2*gap)
        shape = k0 + iv.mpf([0, 1])/6
        scale = iv.exp(mean + sign*(gap-iv.ln(shape))/power)
        remainder = iv.mpf([0, 1])/(12*shape)
        ll = n*(iv.ln(power)-mean + iv.ln(shape/(2*iv.pi))/2
                -shape*gap-remainder)
        return shape, scale, ll

    ordinary = face(1)
    inverse = face(-1)
    difference = inverse[2]-ordinary[2]
    assert difference.a > 0
    record = dict(case=f"{case['index']:06}", n=n,
                  data_binary64=[value.hex() for value in case["data"]],
                  power_binary64=case["held"]["shape2"].hex(),
                  threshold=case["held"]["min"],
                  log_span=enclosing_decimal(span, 30),
                  third_central_log_moment=enclosing_decimal(third, 56),
                  third_moment_error_bound=enclosing_decimal(bound, 56),
                  inverse_minus_ordinary=enclosing_decimal(difference, 38))
    for name, (shape, scale, ll) in (("trgamma", ordinary), ("invtrgamma", inverse)):
        record[name] = dict(shape=enclosing_decimal(shape, 8),
                            scale=enclosing_decimal(scale, 30),
                            logLik=enclosing_decimal(ll, 38))
    return record


def main():
    archive = json.loads(gzip.decompress(ARCHIVE.read_bytes()))
    cases = {case["index"]: case for case in archive if case["index"] in IDS}
    assert set(cases) == set(IDS)
    for case in cases.values():
        assert case["family"] == "fpareto" and set(case["held"]) == {"shape2", "min"}
    assert cases[1625]["data"] == cases[1628]["data"]
    assert cases[1625]["held"] == cases[1628]["held"]
    print(json.dumps(dict(interval_decimal_digits=iv.dps,
                          cases=[check(cases[index]) for index in IDS]), indent=2))


if __name__ == "__main__":
    main()
