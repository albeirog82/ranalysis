library(sdcMicro)

data(francdat)
x <- francdat [,c(2,4,5,6,8)]
print(x)

ff <- freqCalc(x, keyVars=c(1,2,3,4) , w=5)
print(cbind(x,ff$fk,ff$Fk))

##################################################

rk <- indivRisk(ff)$rk
print (cbind(x, ff$fk, ff$Fk, rk ))



#### La observación de mayor riesgo es la septima con un 40.23% y se da por que la key 1 es la que tiene el valor "mas extraño"


x[,1] <- globalRecode(x[ ,1] , breaks=c(0 ,1 ,2 ,3,6) ,
                      labels=c(1 ,2 ,3 ,4))
print(x)
ff <- freqCalc(x, keyVars=c(1,2,3,4) , w=5)
print(cbind(x,ff$fk,ff$Fk))
rk <- indivRisk( ff )$rk
print (cbind(x, ff$fk , ff$Fk , rk ))


### Actividad 3: No se observa reduccion del riesgo


localsupx <- kAnon(x, keyVars=1:4, k=2)

plot(localsupx)
print(localsupx$xAnon)

newX <- cbind(localsupx$xAnon, x$w)
newff <- freqCalc(newX, keyVars=c(1,2,3,4) , w=5)
print(cbind(newX,newff$fk,newff$Fk))
newrk <- indivRisk( newff )$rk
print (cbind(newX, newff$fk , newff$Fk , newrk ))


### Actividad 4: Se redujo el riesgo para la muestra 4, 5, 6,, 7. Las filas que fueron suprimidas 


localFrame <- read.table('C:\\Andes\\ingsegpriv\\ranalysis\\data4agr.txt', header=TRUE)
localFrame$condicion <- as.factor(localFrame$condicion)
localFrameagregado <- groupAndRename(localFrame, var = "condicion",
                                    before=c("neumonia","gripa"), after = c("respiratoria") )
ffagr <- freqCalc(localFrameagregado, keyVars=c(3,4))
print(ffagr$fk)



