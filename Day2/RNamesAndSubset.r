nameVec = c(name1 = 1, name2 = 2, name3= 3)
print(nameVec)

x = 1:3
names(x) = c("First Name", "Name Deux", "Tree")
print(x)

nameVecMulti = c(name2 = 1, name2 = 2, name3= 3)
print (nameVecMulti)

names(1:3)
names(nameVec)

nuller = function(n){
  return ("namer")
}

names(nameVec)=c(3,4,4)
print (nameVec)
nameVec["4"]


x = list(a = TRUE, b=24L)
print(x)
typeof(x)

df =data.frame(matrix(1:100, nrow=10, ncol=10))
typeof(df)
class(df)
rownames(df)
colnames(df)
names(df)

data.frame(c(1:4,-2),c(4:7,11))

##40 by 10
new_df = df
for (i in 1:3){
  new_df = cbind(new_df, df)
}
dim(new_df)

##10 by 20

dim(rbind(df,df))

n_df = do.call(cbind,rep(df,10))
dim(n_df)

c_v = c("fghj","ghj", "yuio", "edfbn")
c_n = c(4:7)
data.frame(c_v, c_n)
##unlist gives a vector of chars
##as.vector gives a vector of lists
y = list(TRUE, 2, 4L, 3.6, "THJM", "SDFSDF")
unlist(y)
z = as.vector(y)
class(z[1])
class(as.vector(y))
typeof(c(1:3))
class(c(1:3))

d_df = df[-c(1:10)]
d_df
r_df = df[,-c(1:10)]
r_df

x = 1:5
x[c(3,1)]
x[-2]
x[c(3,3,3)]
x[c(1.5, 2.7)]
x[c(-1.5,3)]
x[c(-1,0)]
x[c(0)]
x[0]
x[7]
x[c(NA,2)]
x[-c(3,1)]

x[c(-3,-1)]
names(x) = c("n1","n2","n3","n4","n5")

x["n"]
x[-c("1")]

y = list(1,2,3,4,5)
names(y) = c("n1","n2","n3","n4","n5")
typeof(y[3])
df
df[7:10]
df[,8:10]
df[c("X8","X9"),"X8"]
df[8:9,8]
names(df[1:10,])
subThis = (df[c("X8"),c("X8","X9")])
subThis[1,2]
subThis[2,]
subThis[,"X8"]

df[2]==df[,2]
typeof(df[2])
typeof(df[2,2])

x = 1:5
x[1:2] = c(10,11)
x

x = 1:10
x[c(FALSE, TRUE)] = 100
x

str(mtcars)
mtcars[1:11]
mtcars[1:12]


fruit = c(a="apple", b="banana", x = NA)
x = c("a", "b", "a", "a", "b", "x", "b", "a")
fruit[x]

## Order Columns
df.order = function(df){
  name_vec = names(df)
  o_vec = order(name_vec)
  df[o_vec]
}

df.order(mtcars)

##
?sample
df.rand = function(df, rows=FALSE){
  c_len = length(df)
  c_name_vec = colnames(df)
  c_r_vec = sample(c_name_vec)
  if (rows == TRUE){
    r_name_vec = rownames(df[])
    r_len = length (r_name_vec)
    r_r_vec = sample(r_name_vec)
    return (df[r_r_vec,c_r_vec])
  } else {
    return (df[c_r_vec])
  }
}
df.rand(mtcars, rows=TRUE)

df.dubious = function(df,k){
  c_name_vec = colnames(df)
  c_r_vec = sample(c_name_vec,k,replace=TRUE)
  return (df[c_r_vec])
}

df.dubious(mtcars,20)


##Continuous selector
df.cont = function(df,m){
  l=nrow(df)
  r = sample(l-m,1)
  return (df[r:(r+m),])
}
df.cont(mtcars,5)

##Take out cols
df.col.del = function(df,colname){
  df_col = colnames(df)
  boo_vec = c()
  for (i in df_col){
    if (i == colname){
      boo_vec = c(boo_vec, FALSE)
    } else {
      boo_vec = c(boo_vec, TRUE)
    }
  }
  df[boo_vec]
}


#repeeatedCars = 
df.col.del(mtcars, "mpg")