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
  standardError1 = sd(predictions)/sqrt(nrow(test))
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
