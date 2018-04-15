import random


class hpkcr(object):
	def __init__(self,g,p,q):
		self.g = g
		self.p = p
		self.q = q


	#Generate a key given randomness 
	def keyGen(self):
		x = random.randint(1,self.q-1)
		sk = x
		pk = pow(self.g,x,self.p)
		return (pk,sk)

	#Encrypt a mesage with the given public key and randomness.
	def Enc(self,m,pk):
		if m < 0 or m > 1:
			return "Invalid message"
		y = random.randint(1,self.q-1)
		s = pow(pk,y,self.p)
		m =  pow(self.g,m,self.p) 
		c1 = pow(self.g,y,self.p)
		c2 = (m*s) % self.p
		return (c1, c2)

	#Decrypt a message with the given secret key.
	def Dec(self,c,sk):
		c1,c2 = c
		s = pow(c1,sk,self.p)
		m = c2 * self.modinv(s,self.p) % self.p
		if m == self.g:
			return 1
		else:
			return 0

	#Randomization function for ElGamal 
	def Rand(self,c,pk,r):
		c1,c2 = c
		return c1*(pow(self.g,r,self.p)) % self.p, pow(pk,r,self.p)*c2 % self.p

	#Adding Layers 
	def AddLayer(self,c,sk):
		c1,c2 = c
		return (c1, c2*pow(c1,sk,self.p)%self.p)

	#Removing Layers 
	def DelLayer(self,c,sk):
		c1,c2 = c
		return (c1, c2*(self.modinv(c1**sk,self.p)) % self.p )

	#Homomorphic Multiplication
	def HMult(self,c,cc):
		c1, c2 = c
		cc1, cc2 = cc
		return (c1*cc1)%self.p, (c2*cc2)%self.p

	#Homomorphic OR
	def HOR(self,c,cc,pk,r):
		r1,r2 = r
		for i in range(r1):
			c = self.HMult(c,c)
		for i in range(r2):
			cc = self.HMult(cc,cc)
		return self.Rand(self.HMult(c,cc),pk,r1)

	def egcd(self,a, b):
	    if a == 0:
	        return (b, 0, 1)
	    else:
	        g, y, x = self.egcd(b % a, a)
	        return (g, x - (b // a) * y, y)

	def modinv(self,a, m):
	    g, x, y = self.egcd(a, m)
	    if g != 1:
	        raise Exception('modular inverse does not exist')
	    else:
	        return x % m

def testLayers(obj,c,sk):
	a = obj.AddLayer(c,sk)
	b = obj.DelLayer(a,sk)
	return b == c

def testEnc(obj,m):
	pk,sk = obj.keyGen()
	return m == obj.Dec(obj.Enc(m,pk),sk)
