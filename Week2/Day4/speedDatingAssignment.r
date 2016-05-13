originalData = read.csv("speedDating.csv")
df = originalData
df$gender = factor(df$gender)
genAct = select(df, gender, sports:yoga) #contains gender (as factor) and activities
model = glm(gender ~ . , family = "binomial", genAct)
prediction = predict(model)
#??to get out the actual probabilities: exp(prediction)/(1 + prediction)
install.packages("pROC")
library(pROC)
actual = df$gender

r = roc(actual,probs)
plot(r)