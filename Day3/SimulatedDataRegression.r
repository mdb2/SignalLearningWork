#getting familiar with rnorm:
x = rnorm(10000,0,1)
plot(x)

#getSamples function
library(ggplot2)
getSamples = function(a, n){
  x = rnorm(n,0,1)
  b = sqrt(1-(a^2))
  error = rnorm(n,0,b)
  y = a*x+error
  return(data.frame(x,y))
}

ggplot(getSamples(.9,100), aes(x, y)) + geom_point() + geom_smooth(method = "lm")#<super cool!

#estimateSlopes
#how to find coefficient: coef(lm(y ~ x, getSamples(.9,100)))
estimateSlopes = function(a, n, numTrials = 500){
  coefs = c()
  for(i in 1:numTrials){
    coefs = c(coefs,coef(lm(y ~ x, getSamples(a,n)))[2])
  }
  return(data.frame(coefs))
}
estimationDF = estimateSlopes(.9,100)
(ggplot(estimationDF, aes(x=estimationDF))
+geom_histogram())
#Based   on   your reading of the Quora answers above, speculate on why the values might be normally distributed.
#seems relevant: inputting n = 1000 with numTrials = 500 is not the same as n = 100, numTrials = 5000
sdFinder  = function (nNums, aNums){
  dfSD = data.frame()
  percentCount = 0
  total = length(nNums)*length(aNums)
  for(i in nNums){
    rowEst = c()
    for(j in aNums)
    {
      percentCount = percentCount + 1
      a = j*.1
      n = i
      estimationDF = estimateSlopes(a,n)
      print(paste("Percent complete:", 100*(percentCount)/(total)))
      rowEst = c(rowEst,sd(unlist(estimationDF)))
      #rownames(dfSD[j]) = paste("n:",j)
    }
    dfSD = rbind(dfSD,rowEst)
  }
  rownames(dfSD) = nNums
  colnames(dfSD) = aNums
  return(dfSD)
}
dfSD = sdFinder(c(100,500,2500,10000), c(1:9)*.1)
dfSD
#for given a convergence with increasing n
dfSD2 = sdFinder(seq(from = 100, to = 10000, by = 200), c(.1))
plot(rownames(dfSD2), unlist(dfSD2))

#p-values
estimateSlopesWithPVals = function(a, n, numTrials = 1000){
  coefs = c()
  pval = c()
  estSummary = c()
  for(i in 1:numTrials){
    estimationModel = lm(y ~ x, getSamples(a,n))
    estSummary = c(estSummary,summary(estimationModel)$coefficients)
    coefs = c(coefs,coef(estimationModel)[2])
    pval = c(pval,summary(estimationModel)$coefficients[2,4])
  }
  df = data.frame(coefs, pval)
  colnames(df) = c("slope","pValues")
  return(df)
}
estimationDF = estimateSlopesWithPVals(.9,500, numTrials = 10000)
plot(estimationDF$slope,estimationDF$pValues)
median(estimationDF$pValues)
lessZero = estimationDF$slope[estimationDF$slope < 0]
print(length(lessZero)/length(estimationDF[,2]))