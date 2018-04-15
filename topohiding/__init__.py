from random import SystemRandom


class Permutation(object):
    def __init__(self, perm):
        self._perm = perm

    def forward(self, i):
        return self._perm[i]

    def inverse(self, i):
        return self._perm.index(i)

    @classmethod
    def gen(cls, d):
        r = SystemRandom()
        perm = list(range(d))
        for i in range(d - 1):
            j = r.randrange(i, n)
            perm[i], perm[j] = perm[j], perm[i]
        return cls(perm)


class TopoHiding(object):
    def __init__(self, hpkcr, kappa, n, di, bit):
        self.hpkcr = hpkcr
        self.di
        self.bit = bit
        self.n_rounds = 8 * kappa * n^3
        self.key_pairs = [[hpkcr.key_gen() for _ in range(d)] for _ in range(self.n_rounds)]
        self.permutations = [Permutation.gen(self.di) for _ in range(self.n_rounds - 1)]

    def do_round(self, i, msgs):
        """Given a round number and the messages from the other nodes, return the list
        of messages to send out to the connected nodes.
        """
        if i == 0:
            return [(self.hpkcr.enc(pk, self.hpkcr.embed_bit(self.bit))) for (pk, _) in self.key_pairs[0]]
        elif i < self.n_rounds:
            perm = self.permutations[i-1]
            key_pairs = self.key_pairs[i]
            out = [None] * self.di
            for d, (c, k) in enumerate(msgs):
                dp = perm[d]
                pk_1, sk_1 = key_pairs[dp]
                k_1 = self.hpkcr.group(k, pk_1)
                c_hat_1 = self.hpkcr.add_layer(c, sk_1)
                bit_enc = self.hpkcr.enc(k_1, self.hpkcr.embed_bit(self.bit))
                c_1 = self.hpkcr.hom_or(bit_enc, c_hat_1)
                out[dp] = (c_1, k_1)
            return out
        elif i == self.n_rounds:
            out = []
            for d, (c, k) in enumerate(msgs):
                bit_enc = self.hpkcr.enc(k_1, self.hpkcr.embed_bit(self.bit))
                e = self.hpkcr.hom_or(c, bit_enc)
                out.append(e)
        return ["hello world"]*self.public_keys
