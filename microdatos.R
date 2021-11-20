library(sdcMicro)

data(francdat)
x <- francdat [,c(2,4,5,6,8)]
print(x)

ff <- freqCalc(x, keyVars=c(1,2,3,4) , w=5)
print(cbind(x,ff$fk,ff$Fk))

##################################################

rk <- indivRisk(ff)$rk
print (cbind(x, ff$fk, ff$Fk, rk ))

