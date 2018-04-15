import argparse
import socket
import topohiding
from topohiding.helperfunctions import HPKCR
import struct 

def receive_exact(s, n):
	res = s.recv(n)
	while(len(res)<n):
		res+=  s.recv(n-len(res))
	return res

def receive_string(s):
	size = receive_exact(s, 4)
	size = struct.unpack("I", size)[0]
	res = receive_exact(s,size)
	return res

def transmit_string(s, str_tx):
	size = struct.pack("I", len(str_tx))
	s.send(size+"str_tx".encode('utf-8'))


#parse list of neighbors in IP:Port form, argument for OR opperation 

parser = argparse.ArgumentParser()
parser.add_argument('-n', '--nodes', nargs='*', action='append', type=str, required=True) 
parser.add_argument('-i', '--id', type=str, required=True)
parser.add_argument('-b', '--bound', type=int, required=True) #upper bound on total number of neighbors 
parser.add_argument('-p', '--port', type=int, required=True)
parser.add_argument('-v', '--value', type=int, required=True)
parser.add_argument('-k', '--kappa', type=int, required=True) 
args = parser.parse_args()

node_hostnames = []
node_ports = []

node_addr = []
node_connections = []

for node_info in args.nodes:
	hostname,port = node_info.split(':')
	node_hostnames.append(hostname)
	node_ports.append(int(port))

#rx socket 
serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
serversocket.bind(('0.0.0.0', args.port))
serversocket.listen(len(args.nodes))

input("Waiting for other clients to come online. Press any key to continue.")

#create tx sockets 
tx_sockets_tmp=[]
rx_sockets={}
tx_sockets={}
for index in range(len(args.nodes)):
	clientsocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	clientsocket.connect((node_hostnames[index], node_ports[index]))
	transmit_string(clientsocket, args.id)
	tx_sockets_tmp.append(clientsocket)


#accept rx sockets and reply to name 
for index in range(len(args.nodes)):
	connection, address = serversocket.accept()
	client_name = receive_string(connection)
	transmit_string(connection, args.id)
	rx_sockets[client_name] = connection

#check for replies and restablish tx_socket / name mapping
for tx_socket in tx_sockets_tmp:
	client_name = receive_string(tx_socket)
	tx_sockets[client_name] = tx_socket

node_names = rx_sockets.keys()

#init topohiding class
topo = topohiding.TopoHiding(None, args.kappa, args.bound, len(args.nodes), args.value)

#do first round
tx_messages = topo.do_round(0, None)
rx_messages = ['']*len(tx_messages)

for round_number in range(1,topo.n_rounds*2):
	#send message to tx_sockets
	for index in len(node_names):
		transmit_string(tx_sockets[node_names[index]], tx_messages[index])

	#receive message from rx_sockets
	for index in len(node_names):
		rx_messages[index] = receive_string(rx_sockets[node_names[index]])

	#compute next round 
	tx_messages = do_round(round_number, rx_messages)

print(tx_messages)




