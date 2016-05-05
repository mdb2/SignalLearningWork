#e1
df = mtcars
attributes(df)

attributes(df)$names = c("asd","asd","asd","asd","asd","asd","asd","asd","asd","asd","asd")

attributes(df)$row.names = c(1:32)

attributes(df)$class = "Double"
  
df

#e2 col name doubler
nameDoubler = function(df){
  newNames = paste(attributes(df)$names,attributes(df)$names, sep="")
  attr(df, "names") =newNames
  return(df)
}
df = nameDoubler(mtcars)
df

#e3 factors
exFac = factor(c(1,2,1,3,4))
exFac[6] = 10#error
levels(exFac)[6] = 7
exFac
#e4 combine
exFac2 = factor(c(1,2,1))
levels(exFac)[6] = 8
exFac3 = c(exFac, exFac2)
exFac3
#can combine, but not much point

#e5 
f1 = factor(letters)
f2 = rev(factor(letters))
f3 = factor(letters, levels = rev(letters))
#f1 and f2 are basically the same, just that the input data is reversed, and f3 is also similar but the display of the levels are reversed
f1
f2
f3

#e6 exclude "NA" and "-"
fruit = c("apple","grapefruit","NA","apple","apple","-","grapefruit","durian")
facFr = factor(fruit,exclude=c("NA","-"))
facFr

#e7 include NA
includeNA = function(inChrVec){
  return(factor(fruit,exclude=c()))
}
fruit = c("apple","grapefruit","NA",NA,"apple","apple","-","grapefruit","durian")
facFr = includeNA(fruit)
facFr
#e8 function that converts  the first floor(ncol(df)) columns into factors
colNameToFactor = function(df){
  inCNames = colnames(df)
  inCNames = inCNames[1:floor(ncol(df))]
  return(factor(inCNames))
}
df = mtcars
print(colNameToFactor(df))

#e9 convert uniques to factors
uniqueToFactor = function(df){
  for(i in 1:ncol(df)){
    currentCol = df[i]
    uniqueCol = unlist(unique(currentCol))
    if(length(uniqueCol)<=5){
      if(i==11){
      #print(typeof(currentCol))
      #print(class(currentCol))
      print(uniqueCol)
      print(length(uniqueCol))
      print("ADSADS")
      print(factor(unlist(currentCol)))}
      df[i] = factor(unlist(currentCol))
    }
  }
  return(df)
}
df = mtcars
df = uniqueToFactor(df)
df
str(df)

#e10 replace NA's with most common value of the column
#t = table(factor(c(1,2,3,1)))

mostCommon = function(table){
  element = 1
  amount = 0
  for(i in 1:length(table)){
    if(table[i]>amount){
      amount = table[i]
      element = i
    }
  }
  return(element)
}

freqReplace = function(table){
  total = sum(table)
  freqTable = table/total
  for(i in 2:length(freqTable)){
        freqTable[i] = freqTable[i]+freqTable[i-1]
  }
  print(freqTable)
  set.seed(1)
  r = runif(1)
  for(i in 1:(length(freqTable)-1)){
    if(r<freqTable[i]){
      return(i)
    }
  }
  return(length(freqTable))
}

replaceNA = function(df, type = "Common"){
  for(i in 1:ncol(df)){
    if(is.factor(df[[i]])){
      for(j in 1:nrow(df)){
        if(is.na(df[[j,i]])){
          colTable = table(df[[i]])
          if(type == "Common")
          df[[j,i]] = names(colTable)[mostCommon(colTable)]
          else if(type == "Frequency")
            df[[j,i]] = names(colTable)[freqReplace(colTable)]
            else
              print("Type not recognized")
        }
      }
    }
  }
  return(df)
}
# NA'd and factored mtcars:
df = mtcars
df = uniqueToFactor(df)
df[2,4] = NA
df[2,2] = NA
is.na(df[2,4])
newDF = replaceNA(df,"Frequency")
df
newDF

#e11 binary indicator variables
comFunc = function(df){
  #so we got df and some of the columns are factors, some aren't
  #now the columns that are factors and do stuff
  #so we got a bunch of factors we are working with
  #So, now what this whole thing is doing is creating new factor columns based on the current factor column
  #in each of the original factors generate a new factor for every level except the first.
  #in each generated factor, make it 
  #For each actual value set it to 1 if it equals to the first level, 0 otherwise
  nameDF = c()
  delCols = c()
  numOfCol = ncol(df)
  for(i in 1:numOfCol){
    if(is.factor(df[[i]])){
      delCols = c(delCols,i)
      colLevel = levels(df[[i]])
      for(j in 2:length(colLevel)){
        newCol = rep(0,length(df[[i]]))
        newCol[df[[i]]==colLevel[j]] = 1
        print(colLevel)
        nameDF = paste(colnames(df)[i],(colLevel)[j],sep="_")
        df = cbind(df,newCol)
        colnames(df)[ncol(df)] = nameDF
      }
    }
  }
  df[delCols]=NULL
  return(df)
}
df = mtcars[1:10,]
for(n in c("cyl","am","carb")){
  df[[n]] = factor(df[[n]])
}
comDF = comFunc(df)
comDF

#e12 load
path = "NICE TRY, SPY"
path = "/home/anon/Documents/DataScience/Exercises/Day3/"
load(paste(path,"time.dat",sep = ""))