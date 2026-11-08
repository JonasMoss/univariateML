#!/usr/bin/env python3
"""Independent decimal check of the binary64 log-half input (100 digits).

For size=10, prob=1, pi=1/2, the exact CDF is 1/2 on 0,...,9.
Thus any lower-tail target above 1/2 has quantile 10.
"""
from decimal import Decimal, localcontext
import json

value = -0.69314718055994530942
with localcontext() as ctx:
    ctx.prec = 100
    exact_input = Decimal.from_float(value)
    target = exact_input.exp()
    assert target > Decimal('0.5')
    print(json.dumps({'input_hex': value.hex(), 'exact_input': str(exact_input),
                      'exp_input_100_digits': str(target),
                      'above_one_half': True, 'expected_quantile': 10}, indent=2))
