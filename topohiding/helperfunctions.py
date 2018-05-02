import random
import itertools

def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)

def modinv(a, m):
    g, x, y = egcd(a, m)
    if g != 1:
        raise Exception('modular inverse does not exist')
    else:
        return x % m

def find_generator(q):
    while True:
        g = random.randrange(3, 2 * q - 1)
        if pow(g, 2, 2*q + 1) != 1 and pow(g, q, 2*q + 1) != 1:
            return g

class HPKCR(object):
    def __init__(self,g,q):
        self.g = g        # Generator of group
        self.p = 2*q+1    # 2*q + 1 is a prime
        self.q = q        # q is a prime

    #Generate a key given randomness
    def key_gen(self):
        x = random.randint(1,2*self.q)
        sk = x
        pk = pow(self.g,x,self.p)
        return (pk,sk)

    def group(self, a, b):
        return (a * b) % self.p

    #Encrypt a mesage with the given public key and randomness.
    def enc(self,m,pk):
        y = random.randint(1,2*self.q)
        s = pow(pk,y,self.p)
        c1 = pow(self.g,y,self.p)
        c2 = (m*s) % self.p
        return (c1, c2)

    #Decrypt a message with the given secret key.
    def dec(self,c,sk):
        c1,c2 = c
        s = pow(c1,sk,self.p)
        m = c2 * modinv(s,self.p) % self.p
        return m

    #Randomization function for ElGamal
    def rand(self,c,pk):
        r = random.randint(1,2*self.q-1)
        c1,c2 = c
        return c1*(pow(self.g,r,self.p)) % self.p, pow(pk,r,self.p)*c2 % self.p

    #Adding Layers
    def add_layer(self,c,sk):
        c1,c2 = c
        return (c1, c2*pow(c1,sk,self.p)%self.p)

    #Removing Layers
    def del_layer(self,c,sk):
        c1,c2 = c
        return (c1, c2*(modinv(c1**sk,self.p)) % self.p )

    #Homomorphic Multiplication
    def hmult(self,c,cc):
        c1, c2 = c
        cc1, cc2 = cc
        return (c1*cc1)%self.p, (c2*cc2)%self.p

    #Homomorphic OR
    def hom_or(self,c,cc,pk):
        r1 = random.randint(1,2*self.q-1)
        r2 = random.randint(1,2*self.q-1)
        for i in range(r1):
            c = self.hmult(c,c)
        for i in range(r2):
            cc = self.hmult(cc,cc)
        return self.rand(self.hmult(c,cc),pk)

    def embed_msg(self, m):
        assert m < self.p - 1
        return m + 1

    def unembed_msg(self, m):
        return (m - 1) % self.p

def testLayers(obj,c,sk):
    a = obj.add_layer(c,sk)
    b = obj.del_layer(a,sk)
    return b == c

def testEnc(obj,m):
    pk,sk = obj.key_gen()
    return m == obj.dec(obj.enc(m,pk),sk)

class FakeHPKCR(object):
    def __init__(self):
        self.ctr = itertools.count()

    #Generate a key given randomness
    def key_gen(self):
        keynum = next(self.ctr)
        return (f"PK({keynum})", f"SK({keynum})")

    def group(self, a, b):
        return f"Group({a}, {b})"

    #Encrypt a mesage with the given public key and randomness.
    def enc(self,m,pk):
        return f"Enc({m}, {pk})"

    #Decrypt a message with the given secret key.
    def dec(self,c,sk):
        return f"Dec({c}, {sk})"

    #Randomization function for ElGamal
    def rand(self,c,pk):
        return f"Rand({c}, {pk})"

    #Adding Layers
    def add_layer(self,c,sk):
        return f"AddLayer({c}, {sk})"

    #Removing Layers
    def del_layer(self,c,sk):
        return f"DelLayer({c}, {sk})"

    #Homomorphic Multiplication
    def hmult(self,c,cc):
        return f"HMult({c}, {cc})"

    #Homomorphic OR
    def hom_or(self,c,cc,pk):
        return f"HomOR({c}, {cc}, {pk})"

    def embed_msg(self, m):
        return f"Embed({m})"

    def unembed_msg(self, m):
        return f"Unembed({m})"
