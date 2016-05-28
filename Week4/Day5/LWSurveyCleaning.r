LWSurveyDF = read.csv("2016_lw_survey_public_release_3.csv", na.strings = c("NA","N/A",""))
df = LWSurveyDF

df = df[(df$ResearchConsent == "Yes")&(!is.na(df$Age))&(!is.na(df$Depression)),]
numResponses = sapply(df,function(d)length(unique(d)))
hasUnique = (numResponses != 1)
df = df[hasUnique]
# names = colnames(df)
# hasOtherWriteComment = c(grep("other",names, ignore.case = TRUE),grep("comment",names, ignore.case = TRUE),grep("write",names, ignore.case = TRUE))
# df = df[-hasOtherWriteComment]
dropWith = function(dropDF, s){
  dropNames = names(dropDF)
  dropCol = c()
  for(i in s){
    dropCol = c(dropCol,grep(i,dropNames))
  }
  dropCol = unique(dropCol)
  return(dropDF[-dropCol])
}

df = dropWith(df, c("other","comment","Write","Calibration","CharityDonations","Peak", "PhilosophyIssuesNow", "CommunityIssuesNow", "SuccessorPhilosophy"))

#Find other write ins:
#method 1:
otherWriteIn = sapply(df,function(d)((length(unique(d))>20)&!is.numeric(d)))
colnames(df)[otherWriteIn]

#method 2:
otherWriteIn = sapply(df,function(d)(length(unique(d))/(sum(!is.na(d)))))
plot(otherWriteIn)
colnames(df)[otherWriteIn>.8]

#same results except the second method includes RejoinConditions.4 and .5
df = df[otherWriteIn<.8]

for(i in 1:ncol(df)){
  if(is.factor(df[[i]])){
    df[[i]] = addNA(df[[i]])
  }
}

logiNumerics = sapply(df, function(d)is.numeric(d))

numericDF = df[logiNumerics] # use thhis to see all the numerics in df to manually see what is and is not reasonable
# str(numericDF)

summary(df$Age)
df[(df$Age<0)|(df$Age>116),"Age"] = NA
summary(df$IQ)
df[((df$IQ<60)|(df$IQ>200))&(!is.na(df$IQ)),"IQ"] = NA
summary(df$IQAge)
#three people put a year instead of an age, could fix it for them, but decided to set as NA anyway
df[((df$IQAge<4)|(df$IQAge>80)|(df$Age<df$IQAge))&(!is.na(df$IQAge)),"IQAge"] = NA
# summary(df$SAT)
# test = df
# for(i in 1:nrow(test)){
#   if(is.na(test$SAT2)){
#     test[[i]]$SAT2 = test[[i]]$SAT*(24/16)
#   }
# }

# plot(test$IQ, test$SAT2)
# very little/no correlation of IQ to SAT
summary(df$SAT)
df[((df$SAT>1600))&(!is.na(df$SAT)),"SAT"] = NA
summary(df$SAT2)
#SAT2 seems fine
summary(df$ACT)
#ACT seems fine
summary(df$MIRIMission)
#fine
summary(df$MIRIEffectiveness)
#fine
summary(df$PoliticalInterest)
#fine
# df[((df$ProbabilityQuestions.12<4)|(df$ProbabilityQuestions.12>80)|(df$Age<df$IQAge))&(!is.na(df$IQAge)),"IQAge"]

for(i in 1:12){
  colStrings = paste("ProbabilityQuestions.",i,".", sep = "")
  df[((df[[colStrings]]<0)|(df[[colStrings]]>100))&(!is.na(df[[colStrings]])),colStrings] = NA
}
summary(df$SingularityYear)
plot(df$SingularityYear)
df[((df$SingularityYear<2017)|(df$SingularityYear>100000))&(!is.na(df$SingularityYear)),"SingularityYear"] = NA
# test = df[df$SingularityYear < 2200,"SingularityYear"]
# hist((test-2017), breaks = 30)
summary(df$UnemploymentYear)
plot(df$UnemploymentYear)
df[((df$UnemploymentYear<2017)|(df$UnemploymentYear>3000))&(!is.na(df$UnemploymentYear)),"UnemploymentYear"] = NA
summary(df$Income)
df[(df$Income<0)&(!is.na(df$Income)),"Income"] = NA
summary(df$Income)
# hist(log10(df$Income), breaks = 30)
summary(df$IncomeCharityPortion)
#fine
summary(df$XriskCharity)
#hmm... someone donated 80k to X risk with an income of 30k. With just them the mean would be ~80, with everyone its 153.4, so they are a significant proportion

df$Income = log(df$Income+1)
df$IncomeCharityPortion = log(df$IncomeCharityPortion+1)
df$XriskCharity = log(df$XriskCharity+1)

#replace all "Yes" with 1, all "No" with 0
for(i in 1:ncol(df)){
  levels(df[,i])[match("Yes",levels(df[[i]]))] = 1
  levels(df[,i])[match("No",levels(df[[i]]))] = 0
}

levels(df$Depression)[match("Yes, I was formally diagnosed by a doctor or other mental health professional",levels(df$Depression))] = 2
levels(df$OCD)[match("Yes, I was formally diagnosed by a doctor or other mental health professional",levels(df$OCD))] = 2
levels(df$ASD)[match("Yes, I was formally diagnosed by a doctor or other mental health professional",levels(df$ASD))] = 2
levels(df$ADHD)[match("Yes, I was formally diagnosed by a doctor or other mental health professional",levels(df$ADHD))] = 2
levels(df$BipolarDisorder)[match("Yes, I was formally diagnosed by a doctor or other mental health professional",levels(df$BipolarDisorder))] = 2
levels(df$AnxietyDisorder)[match("Yes, I was formally diagnosed by a doctor or other mental health professional",levels(df$AnxietyDisorder))] = 2
levels(df$BPD)[match("Yes, I was formally diagnosed by a doctor or other mental health professional",levels(df$BPD))] = 2
levels(df$Schizophrenia)[match("Yes, I was formally diagnosed by a doctor or other mental health professional",levels(df$Schizophrenia))] = 2


levels(df$Depression)[match("Not formally, but I personally believe I have (or had) it",levels(df$Depression))] = 1
levels(df$OCD)[match("Not formally, but I personally believe I have (or had) it",levels(df$OCD))] = 1
levels(df$ASD)[match("Not formally, but I personally believe I have (or had) it",levels(df$ASD))] = 1
levels(df$ADHD)[match("Not formally, but I personally believe I have (or had) it",levels(df$ADHD))] = 1
levels(df$BipolarDisorder)[match("Not formally, but I personally believe I have (or had) it",levels(df$BipolarDisorder))] = 1
levels(df$AnxietyDisorder)[match("Not formally, but I personally believe I have (or had) it",levels(df$AnxietyDisorder))] = 1
levels(df$BPD)[match("Not formally, but I personally believe I have (or had) it",levels(df$BPD))] = 1
levels(df$Schizophrenia)[match("Not formally, but I personally believe I have (or had) it",levels(df$Schizophrenia))] = 1

for(i in 1:8){
  colStrings = paste("SuccessorCommunity.",i,".", sep = "")
  levels(df[[colStrings]])[match("Less",levels(df[[colStrings]]))] = 0
  levels(df[[colStrings]])[match("Same",levels(df[[colStrings]]))] = 1
  levels(df[[colStrings]])[match("More",levels(df[[colStrings]]))] = 2
}

for(i in 1:9){
  colStrings = paste("BlogsRead.",i,".", sep = "")
  levels(df[[colStrings]])[match("Never Heard Of It",levels(df[[colStrings]]))] = 0
  levels(df[[colStrings]])[match("Never",levels(df[[colStrings]]))] = 1
  levels(df[[colStrings]])[match("Almost Never",levels(df[[colStrings]]))] = 2
  levels(df[[colStrings]])[match("Rarely",levels(df[[colStrings]]))] = 3
  levels(df[[colStrings]])[match("Sometimes",levels(df[[colStrings]]))] = 4
  levels(df[[colStrings]])[match("Regular Reader",levels(df[[colStrings]]))] = 5
  
  colStrings = paste("BlogsRead2.",i,".", sep = "")
  levels(df[[colStrings]])[match("Never Heard Of It",levels(df[[colStrings]]))] = 0
  levels(df[[colStrings]])[match("Never",levels(df[[colStrings]]))] = 1
  levels(df[[colStrings]])[match("Almost Never",levels(df[[colStrings]]))] = 2
  levels(df[[colStrings]])[match("Rarely",levels(df[[colStrings]]))] = 3
  levels(df[[colStrings]])[match("Sometimes",levels(df[[colStrings]]))] = 4
  levels(df[[colStrings]])[match("Regular Reader",levels(df[[colStrings]]))] = 5
  
  colStrings = paste("StoriesRead.",i,".", sep = "")
  levels(df[[colStrings]])[match("Never Heard Of It",levels(df[[colStrings]]))] = 0
  levels(df[[colStrings]])[match("Never",levels(df[[colStrings]]))] = 1
  levels(df[[colStrings]])[match("Partially And Abandoned",levels(df[[colStrings]]))] = 2
  levels(df[[colStrings]])[match("Partially And Intend To Finish",levels(df[[colStrings]]))] = 3
  levels(df[[colStrings]])[match("Whole Thing",levels(df[[colStrings]]))] = 4
  
  colStrings = paste("StoriesRead2.",i,".", sep = "")
  levels(df[[colStrings]])[match("Never Heard Of It",levels(df[[colStrings]]))] = 0
  levels(df[[colStrings]])[match("Never",levels(df[[colStrings]]))] = 1
  levels(df[[colStrings]])[match("Partially And Abandoned",levels(df[[colStrings]]))] = 2
  levels(df[[colStrings]])[match("Partially And Intend To Finish",levels(df[[colStrings]]))] = 3
  levels(df[[colStrings]])[match("Whole Thing",levels(df[[colStrings]]))] = 4
}

SQNames = colnames(df)[grepl("SQ001", colnames(df))]

# find levels for SQ001:
for(i in 1:length(SQNames)){
  print(paste(SQNames[[i]], ":", sep = ""))
  print(levels(df[[SQNames[[i]]]]))
}
#based on above, assign numbers:
correspondNum = rep(1:5,7)
names(correspondNum) = c("Pro-Life","Lean Pro-Life","No strong opinion","Lean Pro-Choice","Pro-Choice",
                         "Should be more restricted","Lean more restricted","No strong opinion","Lean more open","Should be more open",
                         "Should be lower","Lean towards lower","No strong opinion","Lean towards higher","Should be higher",
                         "Should be lower or eliminated","Lean towards lower or eliminated","No strong opinion","Lean towards higher","Should be higher",
                         "Very unfavorable", "Unfavorable", "No strong opinion", "Favorable", "Very favorable",
                         "Strongly oppose","Oppose","No strong opinion","Support","Strongly support",
                         "Strongly doubt", "Doubt", "No strong opinion", "Believe", "Strongly believe" 
)
for(i in 1:length(SQNames)){
  for(j in 1:length(correspondNum)){
    levels(df[[SQNames[[i]]]])[match(names(correspondNum[j]),levels(df[[SQNames[[i]]]]))] = correspondNum[[j]]
  }
}

# for(i in 1:length(SQNames)){
#   print(paste(SQNames[[i]], ":", sep = ""))
#   print(levels(df[[SQNames[[i]]]]))
# }
# 
# print(df$AbortionLaws.SQ001.)

levels(df$Vegetarian)[match("Yes, I restrict meat some other way (pescetarian, flexitarian, try to only eat ethically sourced meat)",levels(df$Vegetarian))] = 1
levels(df$Vegetarian)[match("Yes, I am vegetarian",levels(df$Vegetarian))] = 2
levels(df$Vegetarian)[match("Yes, I am vegan",levels(df$Vegetarian))] = 3

# was interested in vegetarianism vs. abortion in Lw
# plot(df$Vegetarian,df$AbortionLaws.SQ001.)
# testVeg = df$Vegetarian
# testAbort = df$AbortionLaws.SQ001.
# testVeg = factor(testVeg,levels(testVeg)[c(1,4,3,2)])
# testAbort = factor(testAbort,levels(testAbort)[c(4,2,3,1,5)])
# plot(testVeg,testAbort, xlab = "Vegetarianism", ylab = "Abortion")

# find levels for Genetic:
GenNames = colnames(df)[grepl("Genetic", colnames(df))]
for(i in 1:length(GenNames)){
  print(paste(GenNames[[i]], ":", sep = ""))
  print(levels(df[[GenNames[[i]]]]))
}
correspondNum = c(1:5,2,2:3)
names(correspondNum) = c("Mostly Negative","Negative","No strong opinion","Positive","Mostly Positive",
                         "Depends on the disease",
                         "Depends on the strength of the improvements","Maybe a little"
)

for(i in 1:length(GenNames)){
  for(j in 1:length(correspondNum)){
    levels(df[[GenNames[[i]]]])[match(names(correspondNum[j]),levels(df[[GenNames[[i]]]]))] = correspondNum[[j]]
  }
}
levels(df[["GeneticTreament"]])[match("0",levels(df[["GeneticTreament:"]]))] = 1
levels(df[["GeneticImprovement"]])[match("0",levels(df[["GeneticTreament:"]]))] = 1
levels(df[["GeneticCosmetic"]])[match("0",levels(df[["GeneticTreament:"]]))] = 1
levels(df[["GeneticTreament"]])[match("1",levels(df[["GeneticTreament:"]]))] = 3
levels(df[["GeneticImprovement"]])[match("1",levels(df[["GeneticTreament:"]]))] = 4
levels(df[["GeneticCosmetic"]])[match("1",levels(df[["GeneticTreament:"]]))] = 4


for(i in 1:ncol(df)){
  f = df[[i]]
  if(is.factor(f)){
    if(all(!is.na(as.numeric(levels(f))[f])|is.na((levels(f))[f]))){
      df[[i]] = as.numeric(levels(f))[f]
    }
  }
}

save(df, file="CleanDFData.RData")
# f = df[["OCD"]]
# all(!is.na(as.numeric(levels(f))[f])|is.na((levels(f))[f]))
test[["OCD"]] = as.numeric(levels(f))[f]