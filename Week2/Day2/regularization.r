set.seed(1);j = 50; a = 0.25
x = rnorm(j)
error = sqrt(1 - a^2)*rnorm(j)
y = a*x + error # true model

plot(rnorm(100))
x

summary(lm(y ~ x - 1))
# output of linear regression
# y = aEst * x 

cost = function(x, y, aEst, lambda, p) {
  # SSE + lambda * |aEst|
  return(sum((y - aEst*x)^2) + lambda*abs(aEst)^p)
}
cost(x, y, 0.22, 1, 1)

lambdas = sapply(-8:1, function(x) 2^x)
alphas = seq(-0.1, 0.3, 0.001)

grid = expand.grid(lambdas, alphas)

costL1 = numeric(nrow(grid))
costL2 = numeric(nrow(grid))
for (i in 1:nrow(grid)) {
  costL1[i] = cost(x,y,grid[i,2],grid[i,1],1)
  costL2[i] = cost(x,y,grid[i,2],grid[i,1],2)
}

grid = cbind(grid, costL1, costL2)
colnames(grid) = c("lambda","alpha","costL1","costL2")
# filter the grid for each lambda, you can use lapply
library("ggplot2")
library("Rmisc")
plotLambda = function(l){#something in here is wrong
  currentData = filter(grid, lambda==l)#THIS IS SUSPECT
  g = ggplot(currentData) + geom_point(aes(x=alpha,y=costL1))
  return(g)
}
lambdaPlots = lapply(lambdas,plotLambda)
lambdaPlot1 = plotLambda(lambdas[1])
lambdaPlot2 = plotLambda(lambdas[2])
# doesn't work
multiplot(lambdaPlot1, lambdaPlot2)
multiplot(plotList = lambdaPlots, cols = 2)

multiplot(lambdaPlots[[1]], lambdaPlots[2], lambdaPlots[3], cols=1)

library(ggplot2)

# This example uses the ChickWeight dataset, which comes with ggplot2
# First plot
p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
  geom_line() +
  ggtitle("Growth curve for individual chicks")

# Second plot
p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
  geom_point(alpha=.3) +
  geom_smooth(alpha=.2, size=1) +
  ggtitle("Fitted growth curve per diet")

# Third plot
p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
  geom_density() +
  ggtitle("Final weight, by diet")

# Fourth plot
p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
  geom_histogram(colour="black", binwidth=50) +
  facet_grid(Diet ~ .) +
  ggtitle("Final weight, by diet") +
  theme(legend.position="none")  
plotsStuff = c(p1, p2, p3, p4)
multiplot(plotsStuff, cols=2)

multiplot(lambdaPlots[1])

# works
multiplot(p1, p2, p3, p4)
# doesn't work
multiplot(unlist(list(p1,p2,p3,p4)))

# Doesn't work
multiplot(lambdaPlots[1], lambdaPlots[2])
