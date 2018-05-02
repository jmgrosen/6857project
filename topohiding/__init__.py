import ast
from random import SystemRandom


class Permutation(object):
    def __init__(self, perm):
        self._perm = perm

    def forward(self, i):
        return self._perm[i]

    def inverse(self, i):
        return self._perm.index(i)

    def __repr__(self):
        return 'Permutation({})'.format(self._perm)

    @classmethod
    def gen(cls, d):
        r = SystemRandom()
        perm = list(range(d))
        for i in range(d - 1):
            j = r.randrange(i, d)
            perm[i], perm[j] = perm[j], perm[i]
        return cls(perm)


class TopoHiding(object):
    def __init__(self, hpkcr, kappa, n, di, bit):
        self.hpkcr = hpkcr
        self.di = di
        self.bit = bit
        self.n_rounds = 8 * kappa * n^3
        # self.n_rounds = 2
        self.key_pairs = [[hpkcr.key_gen() for _ in range(self.di)] for _ in range(self.n_rounds)]
        # print("self.key_pairs =", self.key_pairs)
        self.permutations = [Permutation.gen(self.di) for _ in range(self.n_rounds - 1)]
        # self.permutations = [Permutation(range(self.di)) for _ in range(self.n_rounds - 1)]

    def do_round(self, i, msgs):
        """Given a round number and the messages from the other nodes, return the list
        of messages to send out to the connected nodes.
        """
        # print(msgs)
        msgs = [ast.literal_eval(m) for m in msgs] if msgs is not None else None
        if i == 0:
            # print("using key pair 0")
            return [repr((self.hpkcr.enc(self.hpkcr.embed_msg(self.bit), pk), pk)) for (pk, _) in self.key_pairs[0]]
        elif i < self.n_rounds:
            # print("using perm", i - 1)
            perm = self.permutations[i-1]
            # print("using key pair", i)
            key_pairs = self.key_pairs[i]
            out = [None] * self.di
            for d, (c, k) in enumerate(msgs):
                dp = perm.forward(d)
                pk_1, sk_1 = key_pairs[dp]
                k_1 = self.hpkcr.group(k, pk_1)
                c_hat_1 = self.hpkcr.add_layer(c, sk_1)
                bit_enc = self.hpkcr.enc(self.hpkcr.embed_msg(self.bit), k_1)
                c_1 = self.hpkcr.hom_or(bit_enc, c_hat_1, k_1)
                out[dp] = repr((c_1, k_1))
            return out
        elif i == self.n_rounds:
            out = []
            for d, (c, k) in enumerate(msgs):
                bit_enc = self.hpkcr.enc(self.hpkcr.embed_msg(self.bit), k)
                e = self.hpkcr.hom_or(c, bit_enc, k)
                out.append(repr(e))
            return out
        elif i < 2*self.n_rounds:
            # print("using perm", self.n_rounds - i)
            perm = self.permutations[self.n_rounds - i]
            # print("using key pair", self.n_rounds - i)
            key_pairs = self.key_pairs[self.n_rounds - i]
            out = [None] * self.di
            out_e = [None] * self.di
            for d, e in enumerate(msgs):
                dp = perm.inverse(d)
                e_1 = self.hpkcr.del_layer(e, key_pairs[dp][1])
                out_e[dp] = e_1
                out[dp] = repr(e_1)
            return out
        elif i == 2*self.n_rounds:
            # print("using key pair 0")
            key_pairs = self.key_pairs[0]
            out = []
            for d, e in enumerate(msgs):
                out.append(self.hpkcr.dec(e, key_pairs[d][1]))
            return out
        else:
            raise ValueError("i is out of bounds")
