load("CleanDFData.RData")
cleanDF = df
logiNumerics = sapply(cleanDF, function(d)is.numeric(d))
logiNumerics$Age = FALSE
logiNumerics$Income = FALSE
logiNumerics$IncomeCharityPortion = FALSE
logiNumerics$XriskCharity = FALSE
logiNumerics = unlist(logiNumerics)
numericDF = cleanDF[logiNumerics]

numCor = cor(numericDF, use = "pairwise.complete.obs.")