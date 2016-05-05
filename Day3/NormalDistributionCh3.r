#Normal Density Curve
x = seq(-5,5,.1)
normalDist = dnorm(x, 0, 1.5)
plot(normalDist)

#Normal Cumulative Distro
pnorm(310, 527, 105)

#Tucson monsoon
1-(pnorm(4,5.89,2.23)+pnorm(7,5.89,2.23,lower.tail=FALSE))

#Quantiles for the Normal Distro
400+(400-qnorm(.1,400,83))
#probably better way: qnorm(0.9,400,83) or qnorm(0.1,400,83,lower.tail=FALSE)

#Compute first quartile position
qnorm(.25,0,1)

#Math SAT
pnorm(600,500,100)

