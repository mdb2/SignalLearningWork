## 1.6
set.seed(50)
xVec <- sample(0:999, 250, replace=TRUE)
yVec <- sample(0:999, 250, replace=TRUE)
#xVec = c(1:999)
#yVec = c(1:999)
ySumVec = yVec[2:length(yVec)]
xSumVec = xVec[1:length(xVec)-1]
sumVec = xSumVec+ySumVec
sumVec

yTrigVec = yVec[2:length(yVec)]
xTrigVec = xVec[1:length(xVec)-1]
trigVec = sin(yTrigVec)/cos(xTrigVec)
trigVec

xStuffVec = c()
for(i in 3:length(xVec)){
  xStuffVec = c(xStuffVec, xVec[i-2]+2*xVec[i-1]-xVec[i])
}
xStuffVec

finalSum = function(n){
  totalSum = 0
  for(i in 1:n-1){
    totalSum = totalSum+exp(-xVec[i+1])/(xVec[n]+10)
  }
  return(totalSum)
}
finalSum(249)

##1.7

##vector->vector
##picks out numbers greater than 700
newVec = c()
for (i in yVec){
  if (i>600){
    newVec=c(newVec,i)
  }
}
newVec

newVec = c()
for (n in 1:length(yVec)){
  if (yVec[n]>600){
    newVec=c(newVec,xVec[n])
  }
}
newVec

sqrtmean = function(n,m){
  if (n>m){
    (n-m)^(1/2)
  } else 
    (m-n)^(1/2)
}

m = mean(xVec)
newVec = c()
for (i in xVec){
  newVec = c(newVec,sqrtmean(i,m))
}
newVec

high = max(yVec)
counter = 0
for (i in yVec){
  if (high-i<=200){
    counter = counter+1
  }
}
counter

counter2 = 0
for (i in xVec){
  if (i%%2==0){
    counter2 = counter2+1
  }
}
counter2

orderedY = order(yVec)
xVec[orderedY]
  
  a = yVec[seq(1,length(yVec), by=3)]
  a
  
tmpFn = function(inVec){
  outVec = c()
  for(i in 3:length(inVec)){
    outVec = c(outVec,(inVec[i]+inVec[i-1]+inVec[i-2])/3)
  }
  return(outVec)
}
a = c(1:5,6:1)
b = tmpFn(a)
b

##3.9
quadmap = function (start,rho,niter){
  xVec = c(start)
  n = niter
  for (i in 2:niter){
    xlast = xVec[i-1]
    new = rho*xlast*(1-xlast)
    xVec = c(xVec,new)
  }
  return (xVec)
}
quadmap(0.3,2,200)