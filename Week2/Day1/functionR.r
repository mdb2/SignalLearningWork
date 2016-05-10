#1
lapply(1:10, function(x) 2*x)

#2
mtClass = lapply(mtcars, class)
mtClass = unlist(mtClass)
mtClass

#3
normalized = data.frame(lapply(mtcars, function(x) {
  stdev = sd(x)
  xmean = mean(x)
  
  return ((x - xmean)/stdev)
}))

str(normalized)

#4
normalize = function(x) {
  print(class(x))
  if(is.numeric(x)){
    stdev = sd(x)
    xmean = mean(x)
    return ((x - xmean)/stdev)
  }else{
    return(x)
  }
}
df = data.frame(matrix(1:100, nrow=10))
df[1:5] = lapply(df[1:5], as.character)
dfClass = lapply(df, normalize)
dfClass

#5
my_apply = function(x, func) {
  returnval = x
  for (i in 1:length(returnval)) {
    temp = func(as.vector(x[[i]]))
    returnval[[i]] = as.list(temp)
    
  }
  return(as.data.frame(returnval))
}
str(my_apply(df, normalize))


#6
elementLoop = function(df){
  prevCol = df[1]
  count = 2
  for(col in df[2:length(df)]){
    tmp = col
    col = col - prevCol
    df[count] = col
    prevCol = tmp
    count = count + 1
  }
  return(df)
}

indexLoop = function(df){
  prevCol = df[1]
  for(i in length(df):2){
    df[i]=df[i]-df[i-1]
  }
  return(df)
}


df = data.frame(matrix(1:100,nrow=10))
elementDF = elementLoop(df)
elementDF
indexDF = indexLoop(df)
indexDF

#7

L = lapply(1:5, function(x) sample(c(1:4, NA)))
a_function = function(x) {
  sapply(x, mean, na.rm=TRUE)
}
a_function(L)

#8

addNToName = function(df){
  nameVec = c()
  for(i in 1:ncol(df)){
    nameVec = c(nameVec,paste(names(df[i]),"_",i,sep = ""))
  }
  names(df) = nameVec
  return(df)
}
df = data.frame(matrix(1:100,nrow=10))
df
df = addNToName(df)
df

#9
# sub_letter = function(df){
#   cols = colnames(df)
#   for(i in 1:length(cols)){
#     colnames(df)[i] = paste(gsub(" ", ".", colnames(df)[i]),"_mod", sep = "")
#   }
#   return(df)
# }
# exampleDF = data.frame(1,2,3)
# names(exampleDF) = c("adsf sdf", "a a", "q.q")
# sub_letter(exampleDF)

sub_sub_letter = function(inputCol){
  return(paste(gsub(" ", ".",inputCol),"_mod", sep = ""))

}
exampleDF = data.frame(1,2,3)
names(exampleDF) = c("adsf sdf", "a a", "q.q")
nameVec = names(exampleDF)
newNameVec = sapply(nameVec, sub_sub_letter)
newNameVec

#10

sum(sapply(10:100, function(i) {
  (i^3) + (4 * (i^2))
}))

sum(sapply(1:25, function(i) {
  ((2^i)/i) + ((3^i)/i^2)
}))

sapply(seq(3,6,.1), function(x) exp(x) * cos(x))
cos(3)
exp(1)
