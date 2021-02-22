def all(seq):
    for elem in seq:
        if not elem:
            return False
    return True

def any(seq):
    for elem in seq:
        if elem:
            return True
    return False

def list(seq):
    result = []
    for elem in seq:
        result.insert(elem)
    return reversed(result)

def map(fn, seq):
    result = []
    for elem in seq:
        result.insert(fn(elem))
    return reversed(result)

def max(seq):
    if len(seq) == 0:
        raise "ValueError: max() arg is an empty sequence"
    m = seq[0]
    for item in seq:
        if item > m:
            m = item
    return m

def min(seq):
    if len(seq) == 0:
        raise "ValueError: min() arg is an empty sequence"
    m = seq[0]
    for item in seq:
        if item < m:
            m = item
    return m

def reduce(fn, seq, base):
    if len(seq) == 0:
        raise "TypeError: reduce() of empty sequence with no initial value"
    for item in seq:
        base = fn(base, item)
    return base

def sum(seq):
    s = 0
    for elem in seq:
        s = s + elem
    return s

