install.packages("readr")
install.packages("gridExtra")
library(readr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(glmnet)
library(caret)
df = read_csv("training.csv")
n = names(df)
bands = select(df, starts_with("m"))
targets = select(df, Ca:Sand)
others = select(df, BSAN:Depth)

### UTILITY FUNCTIONS ##########################################################

# RMSE for (actual, predicted)
rmse = function(x, y) sqrt(mean((x-y)^2))

biggestCors = function(name,
                       df1 = bands, 
                       df2 = targets, 
                       sign = 1, number = 5){
  cors = cor(df1, df2)
  corDF = as.data.frame(cors)
  corDF = corDF[order(-sign*corDF[name]),]
  head(corDF[name], number)
}


print("negative correlations")
lapply(names(targets), function(name){biggestCors(name, sign = -1)})

print("positive correlations")
lapply(names(targets), function(name){biggestCors(name, sign = 1)})

#Plot wave numbers
bandNames = names(bands)
waveNums = as.numeric(sapply(bandNames, function(x){
  as.numeric(substr(x, 2, 10))
}))
qplot(waveNums)



#Plot correlations with targets against wave numbers
cors = cor(bands, targets)
corDF = as.data.frame(cors)
plots = lapply(names(targets), function(target){qplot(x = waveNums, 
                                                      y = corDF[[target]] ,
                                                      geom = "point", 
                                                      xlab = "Wave Number",
                                                      ylab = names(corDF[target]))})
plots[[1]]
do.call(grid.arrange, plots)


#Plot coefficients of L^2 regularization along with correlations
m = cv.glmnet(scale(bands),targets$Ca, family ="gaussian", alpha = 0)
ndf = data.frame(waveNums, corrs = cor(bands,targets$Ca)[,1],coefficients = coef(m, m$lambda.min)[-1,1])
ggplot(ndf) + geom_point(aes(x = waveNums, y = corrs, colour="#000099")) + geom_point(aes(x = waveNums, y = 50*coefficients, colour="#CC0000", alpha = 0.1))

# Experiment with different values of alpha
alphas = c(1, 0.1, 0.05, 0.01, 0.001, 0)

createPlots = function(a) {
  m = cv.glmnet(scale(bands),targets$Ca, family ="gaussian", alpha = a)
  ndf = data.frame(waveNums, corrs = cor(bands,targets$Ca)[,1],coefficients = coef(m, m$lambda.min)[-1,1])
  ggplot(ndf) + geom_point(aes(x = waveNums, y = corrs, colour="#000099")) + geom_point(aes(x = waveNums, y = 50*coefficients, colour="#CC0000", alpha = 0.1))
}

plots = lapply(alphas, createPlots)
do.call("multiplot", c(plots, list(cols=2)))

testDf = read_csv("sorted_test.csv")
bands_test = select(df, starts_with("m"))
bands_test_scaled = scale(bands_test)

# Calculate RMSEs for various values of alpha and lambda
makePredictions = function(target) {
  print(target)
  bands_scaled = scale(bands)
  result = cv.glmnet(bands_scaled, targets[[target]])
  lRange = range(result$lambda)
  minRange = lRange[1]
  maxRange = lRange[2]
  
  param_grid = expand.grid(.alpha=1:10*0.05, .lambda=seq(minRange, maxRange, length.out=10))
  control = trainControl(method="repeatedcv", number=10, repeats=3, verboseIter=TRUE)
  
  # As before, I won't wrap it in a function, I'll just do it directly
  caret_fit = train(x=bands_scaled, y=targets[[target]], method="glmnet", tuneGrid=param_grid, trControl=control)
  
  # Shows RMSE
  caret_fit # For alpha = 0.2, lambda = 0.00644
  
  # Make Predictions
  hyperAlpha = caret_fit$bestTune[[1]]
  hyperLambda = caret_fit$bestTune[[2]]
  
  hyperFit = glmnet(bands_scaled, targets[[target]], alpha = hyperAlpha)
  # TODO: Put in test data
  hyperRMSE = rmse(predictions, targets[[target]])
  predictions = predict(hyperFit, bands_test_scaled, hyperLambda)
  return(predictions)
}

resultList = lapply(names(targets), makePredictions)
my_solution = data.frame(PIDN = testDf$PIDN, Ca = resultList[1], P = resultList[2], pH = resultList[3], 
                         SOC = resultList[4], Sand = resultList[5])
# Check that your data frame has 728 entries
nrow(my_solution)

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution,file="my_solution.csv", row.names = FALSE)
