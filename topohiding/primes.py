import secrets

def max_pow_2(n):
    i = 0
    while n % (2 ** i) != 0:
        i += 1
    return i

# def miller_rabin_p(n, r, d):


def miller_rabin(k, n):
    if k == 1:
        return False
    elif k in (2, 3):
        return True
    elif k % 2 == 0:
        return False
    else:
        r = max_pow_2(n)
        d = n // (2 ** r)
        for _ in range(k):
            a = 2 + secrets.randbelow(n - 3)
            x = pow(a, d, n)
            if x in (1, n - 1):
                continue

        return all(miller_rabin_p(n, r, d) for _ in range(k))
