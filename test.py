from topohiding.helperfunctions import FakeHPKCR, HPKCR, find_generator
from topohiding import TopoHiding
import random
import networkx
import matplotlib.pyplot as plt

test_graph = networkx.Graph()
for i in range(3):
    test_graph.add_node(i, bit=0)
# test_graph.node[1]['bit'] = 1
test_graph.add_edges_from([
    (0, 1),
    (1, 2),
    (0, 2),
    # (2, 3),
    # (3, 4),
    # (0, 4),
    # (1, 3),
])
networkx.draw(test_graph, with_labels=True)
plt.savefig("test_graph.png")
plt.show()
print("shown!")

q = 1559
g = find_generator(q)

hpkcr = HPKCR(g, q)
# hpkcr = FakeHPKCR()

kappa = 2

def n_rounds(n, kappa):
    return 16 * kappa * n**3 + 1
    # return 4 * 2 + 1

def from_graph(g):
    n = len(g)
    g = g.copy()
    for i in g.nodes():
        bit = g.node[i]['bit']
        deg = g.degree(i)
        # print(f"deg = {deg}")
        g.node[i]['th'] = TopoHiding(hpkcr, kappa, n, deg, bit)
    return g

def n_to_list(d):
    if d is None:
        return None
    return [x for (_, x) in sorted(d.items())]

def list_to_n(g, i, l):
    return [l[j] for j in sorted(g.neighbors(i))]

def run_graph(g):
    prev = {i: None for i in g.nodes()}
    new = {i: {j: None for j in g.neighbors(i)} for i in g.nodes()}
    ret = {i: None for i in g.nodes()}
    for r in range(n_rounds(len(g), kappa)):
        for i in g.nodes():
            neigh = sorted(g.neighbors(i))
            t = g.node[i]['th']
            # print(prev)
            out = t.do_round(r, n_to_list(prev[i]))
            if r == n_rounds(len(g), kappa) - 1:
                ret[i] = out
            else:
                for j, x in zip(neigh, out):
                    new[j][i] = x
        prev = new
        new = {i: {j: None for j in g.neighbors(i)} for i in g.nodes()}
    return ret

simple = networkx.Graph()
simple.add_node(0, bit=1)
simple.add_node(1, bit=1)
simple.add_edge(0, 1)
simple = from_graph(simple)
print(run_graph(simple))

test_graph = from_graph(test_graph)
print(run_graph(test_graph))


# n = 2
# bit1 = 0
# node1 = TopoHiding(hpkcr, kappa, n, 1, bit1)
# bit2 = 0
# node2 = TopoHiding(hpkcr, kappa, n, 1, bit2)

# prev1 = node1.do_round(0, None)
# prev2 = node2.do_round(0, None)
# print(prev1)
# print(prev2)
# print()

# for i in range(1, 2 * node1.n_rounds + 1):
#     print(f"ROUND {i}:")
#     # print()

#     print("NODE 1:")
#     cur1 = node1.do_round(i, prev2)
#     print("OUT 1:", cur1)
#     # print()

#     print("NODE 2:")
#     cur2 = node2.do_round(i, prev1)
#     print("OUT 2:", cur2)
#     # print()

#     prev1, prev2 = cur1, cur2

# print()
# print("FINAL ANSWER FOR 1:", any(hpkcr.unembed_msg(x) for x in prev1))
# print("FINAL ANSWER FOR 2:", any(hpkcr.unembed_msg(x) for x in prev2))





# def test_hpkcr(h, n):
#     keys = [h.key_gen() for _ in range(n)]
#     msg = random.randrange(h.p - 1)
#     pk0, sk0 = h.key_gen()
#     ct = h.enc(h.embed_msg(msg), pk0)
#     for _, sk in keys:
#         ct = h.add_layer(ct, sk)
#     for _, sk in keys[::-1]:
#         ct = h.del_layer(ct, sk)
#     pt = h.unembed_msg(h.dec(ct, sk0))
#     if pt != msg:
#         print(pt, msg)

# random.seed()

# # for i in range(100):
# #     test_hpkcr(hpkcr, i)
