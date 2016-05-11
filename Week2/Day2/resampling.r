# Exercise about over-fitting
# Let k = max exponent number, you need n = k + 1 data points 
# fit perfectly

y = a0 + a1x
y = a0 + a1x

# RNG exercise
set.seed(5)
runif(5) # Repeat running these two lines over and over again, get same results

df = read.csv("speedDatingSimple.csv")

library(dplyr)
femaleDF = dplyr::filter(df, gender == 0)
maleDF = dplyr::filter(df, gender == 1)

# Based on our stereotypes, we think 0 is female and 1 is male.
summary(maleDF)
summary(femaleDF)

dim(femaleDF)
femaleActivities = select(femaleDF, attr_o, sports:yoga)

attrModel = lm(femaleDF$attr_o ~.,femaleActivities)
summary(attrModel)
#only listing variables with p-values less than 0.1
#exercise seems by far the most important of the activities in determining attraction (having positive correlation)
#positively correlated: clubbing, exercise, sports
#negatively correlated: tvsports


#6.4% of the variance is explained by this model (its not that great)

splitAndPredict = function(df, doTest = TRUE) {
  # Get a vector of shuffle row indices
  sampleVec = sample(1:nrow(df))
  shuffled = df[order(sampleVec),]
  
  # Use that shuffled row indices to sort the data frame (order)
  train = shuffled[c(TRUE, FALSE),]
  test = shuffled[c(FALSE, TRUE),]
  
  model = lm(attr_o ~.,df)
  if(doTest) {
    predictions = predict(model, test)
  } else {
    predictions = predict(model, train)
  }
  #standardError1 = sd(predictions)/sqrt(nrow(test))
  # sd(predictions-test)/sqrt(nrow(test))
  return(cor(predictions, test$attr_o)^2)
  #return(predictions)
}

splitAndPredict(femaleActivities)

# Run 100 times
testVec = c(1:100)
trainVec = c(1:100)
for (i in 1:100) {
  testVec[i] = splitAndPredict(femaleActivities)
  trainVec[i] = splitAndPredict(femaleActivities)
}
# plot(vec)
# hist(vec)
mean(testVec)
mean(trainVec)
sd(trainVec)/sqrt(100)
sd(testVec)/sqrt(100)

#n fold cross validation

nCrossValidation = function(df, n){
  sampleVec = sample(1:nrow(df))
  shuffled = df[order(sampleVec),]
  cutOff = floor((nrow(shuffled)/n))
  beginX = 1
  endX = cutOff 
  predictions = c()
  for(i in 1:n){
    if(i==n)
      endX = nrow(shuffled)
    logiVec = rep(c(FALSE),nrow(df))
    logiVec[beginX:endX] = c(TRUE)
    test = shuffled[logiVec,]
    train = shuffled[(!logiVec),]
    
    model = lm(attr_o ~.,train)
    # accumulate set of all of predictions
    predictions = c(predictions, predict(model, test))
    beginX = beginX+cutOff
    endX = endX+cutOff
  }
  # print(length(predictions))
  # print(dim(shuffled))
  return(cor(predictions, shuffled$attr_o)^2)
}
twoFoldR2 = c()
tenFoldR2 = c()
for(i in 1:100){
  twoFoldR2 = c(twoFoldR2,nCrossValidation(femaleActivities, 2))
  tenFoldR2 = c(tenFoldR2,nCrossValidation(femaleActivities, 10))
}
mean(twoFoldR2)
mean(tenFoldR2)
sd(twoFoldR2)/sqrt(100)
sd(tenFoldR2)/sqrt(100)
hist(twoFoldR2) # Skewed right
hist(tenFoldR2) #

# split once < 2-Fold Cross Validation < 10-Fold Cross Validation
set.seed(1)
splitAndPredict(femaleActivities) # 0.07413794
nCrossValidation(femaleActivities, 2) # 0.01769103
nCrossValidation(femaleActivities, 10) # 0.03240629

backwardStep = function(df) {
  
  rSquaredVec = c()
  numFeaturesRemovedVec = c()
  numFeaturesRemoved = 0
  while (length(colnames(df)) > 1) {
    # Calc R^2
    rSquared = nCrossValidation(df, 10)
    
    # Fit model
    model = lm(attr_o ~.,df)
    
    rSquaredVec = c(rSquaredVec, rSquared)
    numFeaturesRemovedVec = c(numFeaturesRemovedVec, numFeaturesRemoved)
    
    # Feature elimination
    sumDF = as.data.frame(summary(model)$coefficients)
    sumDF = cbind(sumDF, rownames(sumDF))
    maxRow = filter(sumDF, sumDF[4] == max(sumDF[4]))
    colIndex = match(maxRow[[5]], colnames(df))
    
    # Eliminate column
    df = select(df, -colIndex)
    
    numFeaturesRemoved = numFeaturesRemoved + 1
    # names(df)
  }
  return(data.frame(rSquaredVec, numFeaturesRemovedVec))
}
data = backwardStep(femaleActivities)

# Not a completely smooth plot, but R^2 increases as 
# features are removed and then sharply drops when you've
# removed 16 variables
plot(data$numFeaturesRemovedVec, data$rSquaredVec)

backStepOfficial = function(df) {
  responseVars = c("attr_o", "sinc_o","intel_o","fun_o","amb_o")
  finalModels = list()
  for(i in responseVars){
    activities = select(df, one_of(i), sports:yoga)
    f = paste(i,"~.")
    model = lm(f, activities)
    s = step(model, formula(model), direction="backward")
    finalModels[length(finalModels)+1] = s
  }
  return(finalModels)
}

## Females
finalModels = backStepOfficial(femaleDF)

# attr_o: 
# + correlation: sports, exercise, clubbing, yoga
# - tvsports
finalModels[1]

# sinc_o
# + museums, hiking
# - tvsports, dining, reading
finalModels[2]

# intel_o
# + concerts
# - tvsports, theater, music, shopping
finalModels[3]

# fun_o
# + exercise, clubbing, yoga
# - tvsports, music
finalModels[4]

# amb_o
# + sports, dining
# - museums, music
finalModels[5]

## Male
finalModels = backStepOfficial(maleDF)

# attr_o: 
# + correlation: sports, exercise, dining, clubbing, music
# - gaming, tv, theater, concerts
finalModels[1]

# sinc_o
# + sports, hiking, music
# - concerts
finalModels[2]

# intel_o
# + sports, dining, hiking, music
# - tv, concerts
finalModels[3]

# fun_o
# + sports, dining, art, music
# - museums, tv, concerts
finalModels[4]

# amb_o
# + exercise, dining, hiking, clubbing, music, yoga
# - gaming, concerts
finalModels[5]

