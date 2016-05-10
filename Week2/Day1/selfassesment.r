#Starting at 10am
#Euclid
#1:
library(ggplot2)
trials = function(k){
  x = runif(k)#distribution from 0 to 1
  y = runif(k, max = x)
  return(y)
}
qplot(trials(1000000), bins = 30)

#2:
t = trials(1000000)
binT = cut(t, 100)
qplot(binT)
meanT = c()
for(i in 1:length(binT)){
  meanT = c(meanT,mean(binT[i]))
}
qplot(meanT)

#3
formulaMean = c()
for(i in 1:100){
  formulaMean = c(formulaMean, ((i/100.0)-1)/(log(i/100.0)))
}
qplot(formulaMean)

#Part2: DataAnalysis
#1
install.packages("psych")
library("psych")
help(msq)
df = msq
#2 #TODO: eliminate for loop
fractionNA = c()
for(i in 1:length(df)){
  isNAVec = is.na(unlist(df[i]))
  isNAVec = isNAVec[isNAVec == TRUE]
  fractionNA = c(fractionNA,length(isNAVec)/length(df[[i]]))
}
names(fractionNA) = names(df)
fractionNA
fractionNA["kindly"]
#3
includedCols = names(df)
includedCols = includedCols[match("active",includedCols):match("scornful",includedCols)]
includedCols = c(includedCols, "Extraversion", "Neuroticism")
print(length(includedCols))
newDF= df[includedCols]
newDF
#4
noNaNewDF = newDF
for(i in 1:length(newDF)){
  meanVal = mean(noNaNewDF[[i]][is.na(noNaNewDF[[i]])==FALSE])
  naVec = is.na(noNaNewDF[[i]])==TRUE
  #print(newDF[[i]][naVec])
  noNaNewDF[[i]][naVec] = meanVal
}
noNaNewDF
#5
#histograms:
(ggplot(noNaNewDF, aes(x=noNaNewDF$"Extraversion"))
+geom_histogram())
(ggplot(noNaNewDF, aes(x=noNaNewDF$"Neuroticism"))
+geom_histogram())

#density:
(ggplot(noNaNewDF, aes(x=noNaNewDF$"Extraversion"))
+geom_density())
(ggplot(noNaNewDF, aes(x=noNaNewDF$"Neuroticism"))
+geom_density())

(ggplot(noNaNewDF, aes(x=noNaNewDF$"Extraversion", y=noNaNewDF$"Neuroticism"))
+geom_point()
+geom_smooth(method = "loess"))

#6:
ExtraActiveModel = c()
NeuroActiveModel = c()
exNoNaNewDF = noNaNewDF[-noNaNewDF$"Extraversion"]
exNoNaNewDF = noNaNewDF[-noNaNewDF$"Neuroticism"]
for(i in 1:length(exNoNaNewDF)){
#for(i in 1:1){
    ExtraActiveModel = c(ExtraActiveModel, (lm(noNaNewDF$"Extraversion" ~ exNoNaNewDF[[i]])))
    NeuroActiveModel = c(NeuroActiveModel, (lm(noNaNewDF$"Neuroticism" ~ exNoNaNewDF[[i]])))
}
#7:
length(ExtraActiveModel)
coefVec = coef(ExtraActiveModel)
EAC = coef(ExtraActiveModel)
EAC
print(EAC)
NAC = coef(NeuroActiveModel)
print(NAC)