import secrets

__all__ = ['CyclicGroup', 'AdditiveCyclicGroup', 'MultiplicativeCyclicGroup']

class CyclicGroup:
    @property
    def order(self):
        raise NotImplementedError()

    def operation(self, x, y):
        raise NotImplementedError()

    @property
    def identity(self):
        raise NotImplementedError()

    def inverse(self, x):
        raise NotImplementedError()

    @property
    def generator(self):
        raise NotImplementedError()

    def power(self, x, n):
        if n == 0:
            return self.identity
        else:
            xp = self.power(x, n // 2)
            xp2 = self.operation(xp, xp)
            if n % 2 == 0:
                return xp2
            else:
                return self.operation(x, xp2)

class AdditiveCyclicGroup(CyclicGroup):
    def __init__(self, n):
        self._n = n

    @property
    def order(self):
        return self._n

    def operation(self, x, y):
        return (x + y) % self._n

    @property
    def identity(self):
        return 0

    def inverse(self, x):
        return (-x) % self._n

    @property
    def generator(self):
        return 1

def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        d, x, y = egcd(b % a, a)
        return (d, y - x * (b // a), x)

def find_generator(q):
    while True:
        g = 3 + secrets.randbelow(2 * q - 2)
        if pow(g, 2, 2*q + 1) != 1 and pow(g, q, 2*q + 1) != 1:
            return g

class MultiplicativeCyclicGroup(CyclicGroup):
    def __init__(self, q, g):
        self._q = q
        self._p = 2*q + 1
        self._g = g

    @property
    def order(self):
        return 2 * self._q

    def operation(self, x, y):
        return (x * y) % self._p

    @property
    def identity(self):
        return 1

    def inverse(self, a):
        d, x, _ = egcd(a, self._p)
        if d == 1:
            return x % self._p
        else:
            raise ValueError()

    @property
    def generator(self):
        return self._g

    @classmethod
    def random_of_size(cls, n):
        pass
