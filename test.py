from topohiding.helperfunctions import HPKCR, find_generator
from topohiding import TopoHiding

q = 11
g = find_generator(q)

hpkcr = HPKCR(g, q)

kappa = 5
n = 2

bit1 = 0
node1 = TopoHiding(hpkcr, kappa, n, 1, 0)
bit2 = 0
node2 = TopoHiding(hpkcr, kappa, n, 1, 0)

prev1 = node1.do_round(0, None)
prev2 = node2.do_round(0, None)
print(prev1, prev2)

for i in range(1, 2 * node1.n_rounds):
    cur1 = node1.do_round(i, prev2)
    cur2 = node2.do_round(i, prev1)
    prev1, prev2 = cur1, cur2
    print(prev1, prev2)
