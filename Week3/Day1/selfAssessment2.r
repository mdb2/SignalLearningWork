#Started at 10:02, took lunch break at 12:00 resumed at 12:45 end at 1:18, 3 hours spent
library(psych)

set.seed(1)
#generates alphas and lambdas:
alphaIterOver = seq(0,1,length.out = 11)
lambdaIterOver = 10^seq(1,-2,length.out = 50)
df = msq
noNADF = df#this hogs memory for programmer neuroticism/convenience
#remove NAs replace with column mean:
for(i in 1:ncol(noNADF)){
  noNADF[[i]][is.na(noNADF[[i]])] = mean(noNADF[[i]], na.rm = TRUE)
}

####### Work on Extraversion, Neuroticism dataset ##########
# set seed for imputation
set.seed(1)

# generates alphas and lambdas:
alphaIterOver = seq(0,1,length.out = 11)
lambdaIterOver = 10^seq(1,-2,length.out = 50)

# set up full dataframe
df = msq

### set second dataframe which will reliably have no NA values
noNADF = df      # this hogs memory for programmer neuroticism/convenience

## remove NAs replace with column mean:
for(i in 1:ncol(noNADF)){
  noNADF[[i]][is.na(noNADF[[i]])] = mean(noNADF[[i]], na.rm = TRUE)
}




#get features, and get targets extraversion and neuroticism:
features = select(noNADF, active:scornful)
extraTarget = noNADF$Extraversion
neuroTarget = noNADF$Extraversion
#rmse helper function:
rmse = function(x, y){
  return(sqrt(mean((x-y)^2)))
}

# generate folds:
numFolds = 10
sampleVec = sample(nrow(features))
shuffledFeatures = features[sampleVec,]
shuffledExtra = extraFeature[sampleVec,]
shuffledNeuro = neuroFeature[sampleVec,]

#preallocate:
trainFeatList = vector("list", numFolds)
trainExtraList = vector("list", numFolds)
trainNeuroList = vector("list", numFolds)
testFeatList = vector("list", numFolds)

3456789012
for(i in 1:numFolds){
  trainRowLogiVec = ((i-1):(numFolds+i-2))%%(numFolds)<=numFolds-2
  print(trainRowLogiVec)
  # trainFeatList[[i]] = shuffledDF[trainRowLogiVec,]
  # trainExtraList[[i]] = shuffledExtra[trainRowLogiVec]
  # trainNeuroList[[i]] = shuffledNeuro[trainRowLogiVec]
  # testFeatList[[i]] = shuffledDF[!trainRowLogiVec,]
}

#create data frame to store the results of computations:
storeComp = data.frame()
storeCompNames = c("Alpha", "Lambda", "CVRMSE-Extra", "CVRMSE-Neuro")

# alphaIterOver = seq(0,1,length.out = 11)
# lambdaIterOver = 10^seq(1,-2,length.out = 50)
library(glmnet)
rmseExtraTot = vector("list", length(alphaIterOver)*length(lambdaIterOver))
rmseNeuroTot = vector("list", length(alphaIterOver)*length(lambdaIterOver))

for(i in 1:length(alphaIterOver)){
  print(alphaIterOver[i])
  fitListExtra = vector("list", numFolds)
  fitListNeuro = vector("list", numFolds)
  trainScales = vector("list", numFolds)
  for(j in 1:numFolds){
    trainScale = scale(trainFeatList[[j]])
    trainScales[[j]] =  scale(trainFeatList[[j]])
    fitListExtra[[j]] = glmnet(trainScale,trainExtraList[[j]],alpha = alphaIterOver[[i]],lambda=lambdaIterOver)
    fitListNeuro[[j]] = glmnet(trainScale,trainNeuroList[[j]],alpha = alphaIterOver[[i]],lambda=lambdaIterOver)
  }
  rmseListExtra = vector("list", length(lambdaIterOver))
  rmseListNeuro = vector("list", length(lambdaIterOver))
  for(i2 in 1:length(lambdaIterOver)){
    print(lambdaIterOver[i2])
    predictListExtra = vector("list", numFolds)
    predictListNeuro = vector("list", numFolds)
    for(j in 1:numFolds){
      scalingCenter = attr(trainScales[[j]], "scaled:center")
      scalingScale = attr(trainScales[[j]], "scaled:scale")
      predictListExtra[[j]]  = predict(fitListExtra[[j]], scale(testFeatList[[j]],scalingCenter,scalingScale), lambdaIterOver[[i]])
      predictListNeuro[[j]]  = predict(fitListNeuro[[j]], scale(testFeatList[[j]],scalingCenter,scalingScale), lambdaIterOver[[i]])
    }
    rmseExtra[[i2]] = rmse(unlist(predictListExtra), shuffledExtra)
    rmseNeuro[[i2]] = rmse(unlist(predictListNeuro), shuffledNeuro)
  }
  rmseExtraTot[[i]] = rmseExtra
  rmseNeuroTot[[i]] = rmseNeuro
}
storeComp = rbind(rep(alphaIterOver,length(lambdaIterOver)), rep(lambdaIterOver,length(alphaIterOver)), rmseExtra, rmseNeuro)
names(storeComp) = storeCompNames
