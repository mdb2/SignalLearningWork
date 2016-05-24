originalData = read.csv("speedDating.csv")
df = originalData
commonCareer = table(df$career_c)[order(table(df$career_c), decreasing = TRUE)][1:4]
# library(dplyr)
top4DF = dplyr::filter(df, career_c %in% as.numeric(names(commonCareer)))
top4Obs = select(top4DF, attr_o:amb_o, sports:yoga)
top4Res = select(top4DF, career_c)
# library(glmnet)
model = glmnet(scale(top4Obs), as.matrix(top4Res), family = "multinomial", lambda = 0)

coefs = coef(model)

for(i in 1:length(coefs)){
  tmpName = rownames(coefs[[i]])
  coefs[[i]] = coefs[[i]][2:length(coefs[[i]])]
  names(coefs[[i]]) = tmpName[2:length(tmpName)]
}
corrCoefs = as.matrix(do.call(cbind, coefs))
# library(corrplot)
corrplot(corrCoefs,is.corr = FALSE)

dfObs = select(df, attr_o:amb_o, sports:yoga)
predictions = predict(model, scale(as.matrix(dfObs)), s = 0)
predMat = as.matrix(as.data.frame(predictions))
# library(psych)
pcaComps = prcomp(predMat, scale = TRUE)
# pcaLoad = pcaComps$rotation

corrplot(as.matrix(pcaComps$rotation), is.corr = FALSE)

pcaComps[1]$rotation

probabilities = function(preds, rownum){
  justRow = preds[rownum,]
  norm = sum(exp(justRow))
  justRow = exp(justRow)/norm
  return(justRow)
}

print(probabilities(as.matrix(as.data.frame(predictions)),551))
