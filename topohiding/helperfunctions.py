import random

#Generate a key given randomness 
def keyGen(g,q,p):
	x = random.randint(1,q-1)
	sk = x
	pk = pow(g,x,p)
	return (pk,sk)

#Encrypt a mesage with the given public key and randomness.
def Enc(m,g,q,pk,p):
	if m < 0 or m > 1:
		return "Invalid message"
	y = random.randint(1,q-1)
	s = pow(pk,y,p)
	m =  pow(g,m,p) 
	c1 = pow(g,y,p)
	c2 = (m*s) % p
	return (c1, c2)

#Decrypt a message with the given secret key.
def Dec(c,g,q,sk,p):
	c1,c2 = c
	s = pow(c1,sk,p)
	m = c2 / s
	if m == g:
		return 1
	else:
		return 0

#Randomization function for ElGamal 
def Rand(c,pk,r,g):
	c1,c2 = c
	return c1*(g**r), (pk**r)*c2 

#Adding Layers 
def AddLayer(c,sk):
	c1,c2 = c
	return (c1, c2*(c1**sk))

#Removing Layers 
def DelLayer(c,sk):
	c1,c2 = c
	return (c1, c2/(c1**sk))

#Homomorphic Multiplication
def HMult(c,cc):
	c1, c2 = c
	cc1, cc2 = cc
	return c1*cc1, c2*cc2





def testLayers(c,sk):
	return DelLayer(AddLayer(c,sk),sk) == c

def testEnc(g,q,m,p):
	pk,sk = keyGen(g,q,p)
	return m == Dec(Enc(m,g,q,pk,p),g,q,sk,p)



