library("ggplot2")
library("Rmisc")
install.packages("glmnet")
library("glmnet")

set.seed(1);j = 50; a = 0.25
x = rnorm(j)
error = sqrt(1 - a^2)*rnorm(j)
y = a*x + error # true model

plot(rnorm(100))
x

summary(lm(y ~ x - 1))

cost = function(x, y, aEst, lambda, p) {
  RMSE = (sqrt(sum((y - aEst*x)^2) / length(y)))
  penalty = lambda*abs(aEst)^p
  return(RMSE+penalty)
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

plotLambda1 = function(l){
  currentData = dplyr::filter(grid, lambda==l)
  g = ggplot(currentData) + geom_point(aes(x=alpha,y=costL1))
  return(g)
}

plotLambda2 = function(l){
  currentData = dplyr::filter(grid, lambda==l)
  g = ggplot(currentData) + geom_point(aes(x=alpha,y=costL2))
  return(g)
}
lambdaPlots = lapply(lambdas,plotLambda1) 
do.call("multiplot", c(lambdaPlots, list(cols=2)))


lambdaPlots = lapply(lambdas,plotLambda2) 
do.call("multiplot", c(lambdaPlots, list(cols=2)))

# Speed Dating

df = read.csv("speedDatingSimple.csv")

maleDF = dplyr::filter(df, gender == 1)
maleActWithAttr = dplyr::select(maleDF, attr_o, sports:yoga)
maleActivities = dplyr::select(maleDF, sports:yoga)

model = lm(attr_o~., maleActWithAttr)
stepFit = step(model, formula(model), direction="backward")
#Coefficients:
# (Intercept)       sports     exercise       dining       gaming  
# 4.30971      0.10413      0.07661      0.08111     -0.05843  
# clubbing           tv      theater     concerts        music  
# 0.04484     -0.05505     -0.08660     -0.09152      0.15611  

# L1
l1fit = glmnet(scale(maleActivities), maleDF$attr_o, alpha = 1)

# L2
l2fit = glmnet(scale(maleActivities), maleDF$attr_o, alpha = 0)
l2fit

minLambda = function(fit, new_data) {
   predictions = predict(fit, new_data, fit$lambda)
   allRMSE = c()
   for(i in 1:ncol(predictions)){
     RMSE = sqrt(sum((predictions[,i]-new_data)^2)/length(new_data))
     allRMSE = c(allRMSE,RMSE)
   }
   minRMSE = min(allRMSE)
   minIndex = (match(allRMSE, minRMSE, nomatch = FALSE))
   bestLambda = fit$lambda[minIndex == 1]
   return(list(bestLambda,minRMSE))
}
l1LambdaRMSE = minLambda(l1fit, scale(maleActivities))
l2LambdaRMSE = minLambda(l2fit, scale(maleActivities))

predictedAttr = predict(stepFit, maleActivities)
n = length(maleDF$attr_o)
stepRMSE = sqrt(sum((predictedAttr - maleDF$attr_o)^2)/n)

# L1 RMSE: 6.005926
# L2 RMSE: 6.006989
# Step: 1.059922

coef(l1fit, l1LambdaRMSE[[1]]) 
# L1
# sports      0.13004897
# exercise    0.04122008
# Everything else was 0

coef(l2fit, l2LambdaRMSE[[1]]) 

# L2
# sports       1.082993e-02
# tvsports     1.988003e-03
# exercise     9.330258e-03
# dining       4.162723e-03
# museums     -2.028907e-03
# art         -1.122557e-03
# hiking       8.176147e-05
# gaming      -4.396238e-03
# clubbing     4.566820e-03
# reading     -2.205844e-03
# tv          -6.216509e-03
# theater     -5.957667e-03
# movies      -2.510495e-03
# concerts    -2.160358e-03
# music        4.755852e-03
# shopping     1.223012e-03
# yoga         1.587023e-03

stepFit
# (Intercept)       sports     exercise       dining       gaming  
# 4.30971      0.10413      0.07661      0.08111     -0.05843  
# clubbing           tv      theater     concerts        music  
# 0.04484     -0.05505     -0.08660     -0.09152      0.15611  

# Sports and Exercise both appear in L1 and Step
