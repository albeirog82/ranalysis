options(max.print=1000000)
library(sdcMicro)

datos = read.table("C://Andes//ingsegpriv//ranalysis//dataset-statisticdisclosure-original.csv",sep=";",header=TRUE)


# print(datos)

categoricas <- datos [,c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 34)]

head(categoricas, 10)

ff <- freqCalc(categoricas, keyVars=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28) , w=29)
# print(cbind(categoricas,ff$fk,ff$Fk))


rk <- indivRisk(ff)$rk
# print (cbind(categoricas, ff$fk, ff$Fk, rk ))


## Vamos a recodificar mjob y fjob (8 y 9 en categoricas)

categoricas[,8] <- globalRecode(categoricas[ ,8] , breaks=c(0 ,2 ,4 ,6) ,
                      labels=c(1 ,2 ,3 ))
# print(categoricas)

categoricas[,9] <- globalRecode(categoricas[ ,9] , breaks=c(0 ,2 ,4 ,6) ,
                                labels=c(1 ,2 ,3 ))
# print(categoricas)

ff <- freqCalc(categoricas, keyVars=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28) , w=29)
# print(cbind(categoricas,ff$fk,ff$Fk))


rk <- indivRisk(ff)$rk
print (cbind(categoricas, ff$fk, ff$Fk, rk ))


# supresion local 

#localsupx <- kAnon(categoricas, keyVars=1:28, k=2)
#plot(localsupx)
# print(localsupx$xAnon)

newX <- cbind(localsupx$xAnon, categoricas$w)
newff <- freqCalc(newX, keyVars=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28) , w=29)
# print(cbind(newX,newff$fk,newff$Fk))
newrk <- indivRisk( newff )$rk
# print (cbind(newX, newff$fk , newff$Fk , newrk ))

numericos <- datos[,c(3,30,31,32,33)]

print(numericos)

result <- addNoise(numericos,method="additive")
#print(result$xm)

resultAgg <- microaggregation(numericos, method = "single",aggr = 3)
# print(resultAgg$mx)

print(dUtility(obj=numericos, xm=result$xm, method="IL1"))
print(dRisk(obj=numericos, xm=result$xm))



