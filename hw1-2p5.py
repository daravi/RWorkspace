from decimal import Decimal

x = [0.8,0.64,0.86,0.80,0.74,0.76,0.77,0.84,0.85,0.75]
y = [0.2,0.16,0.07,0.09,0.18,0.13,0.17,0.14,0.21,0.08]
pd = 0.15
# b = [True,False,True,False,True,True,True,False,False,True]
b = [True,True,True,False,False,True,False,False,False,False]
# b = [False,False,False,True,True,False,True,True,True,True]
pdi = 1
pd_t = 1
for i in range(len(x)):
	if b[i]:
		pd_t = x[i]*pd / (x[i]*pd+y[i]*(1-pd))
	else:
		pd_t = (1-x[i])*pd / ((1-x[i])*pd+(1-y[i])*(1-pd))
	pdi *= pd_t
	print('%.2E' % Decimal(pdi))

print('%.2E' % Decimal(pdi))