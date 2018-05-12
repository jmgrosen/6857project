import argparse
import socket
import topohiding
from topohiding.helperfunctions import FakeHPKCR, HPKCR, find_generator
import struct
import base64
import os

# Test Commands: 
# python3.6 cli.py -k 5 -v 1 -p 60002 -b 2 -i foo1 -n 127.0.0.1:60001
# python3.6 cli.py -k 5 -v 1 -p 60001 -b 2 -i foo0 -n 127.0.0.1:60002 
#
def receive_exact(s, n):
	res = s.recv(n)
	while(len(res)<n):
		res+=  s.recv(n-len(res))
	return res

def receive_string(s):
	size = receive_exact(s, 4)
	size = struct.unpack("I", size)[0]
	res = receive_exact(s,size).decode("utf-8") 
	return res

def transmit_string(s, str_tx):
	tx_string = str_tx.encode('utf-8')
	size = struct.pack("I", len(tx_string))
	s.send(size+tx_string)


#parse list of neighbors in IP:Port form, argument for OR opperation 

parser = argparse.ArgumentParser()
parser.add_argument('-n', '--nodes', nargs='*', action='append', type=str, required=True) 
parser.add_argument('-i', '--id', type=str, required=False, default=base64.b64encode(os.urandom(16)).decode('utf-8'))
parser.add_argument('-b', '--bound', type=int, required=True) #upper bound on total number of neighbors 
parser.add_argument('-p', '--port', type=int, required=True)
parser.add_argument('-v', '--value', type=int, required=True)
parser.add_argument('-k', '--kappa', type=int, required=True) 
args = parser.parse_args()

node_hostnames = []
node_ports = []

node_addr = []
node_connections = []
for node_info in args.nodes[0]:
	hostname,port = node_info.split(':')
	node_hostnames.append(hostname)
	node_ports.append(int(port))

print(node_hostnames)
print(node_ports)
print(args.nodes)
print(len(args.nodes[0]))
#rx socket 
serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
serversocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
serversocket.bind(('0.0.0.0', args.port))
serversocket.listen(len(args.nodes[0]))
print("Value: "+ str(args.value))
input("Waiting for other clients to come online. Press any key to continue.")

#create tx sockets 
tx_sockets_tmp=[]
rx_sockets={}
tx_sockets={}
for index in range(len(args.nodes[0])):
	clientsocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	clientsocket.connect((node_hostnames[index], node_ports[index]))
	transmit_string(clientsocket, args.id)
	tx_sockets_tmp.append(clientsocket)


#accept rx sockets and reply to name 
for index in range(len(args.nodes[0])):
	connection, address = serversocket.accept()
	client_name = receive_string(connection)
	transmit_string(connection, args.id)
	rx_sockets[client_name] = connection

#check for replies and restablish tx_socket / name mapping
for tx_socket in tx_sockets_tmp:
	client_name = receive_string(tx_socket)
	tx_sockets[client_name] = tx_socket

node_names = list(rx_sockets.keys())

#init topohiding class
q = 1559
g = 2597
#g = find_generator(q)

hpkcr = HPKCR(g, q)
topo = topohiding.TopoHiding(hpkcr, args.kappa, args.bound, len(args.nodes[0]), args.value)

#do first round
tx_messages = topo.do_round(0, None)
rx_messages = ['']*len(tx_messages)

print(node_names)
for round_number in range(1, 2 * topo.n_rounds + 1):
	print(round_number)

	#send message to tx_sockets
	for index in range(len(node_names)):
		print(tx_messages[index])
		transmit_string(tx_sockets[node_names[index]], tx_messages[index])

	#receive message from rx_sockets
	for index in range(len(node_names)):
		rx_messages[index] = receive_string(rx_sockets[node_names[index]])

	#compute next round 
	tx_messages = topo.do_round(round_number, rx_messages)

print("FINAL ANSWER:", tx_messages)




