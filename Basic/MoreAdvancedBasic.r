x = runif(1)
if (x < 0.5) {
  print(x)
} else {
  print('big')
}
r = rnorm(10)
r
for (i in r){
  if (i < 0.5) {
    print(i)
  } else {
    print('big')
  }
}

iterVector = c(10:100)
sumVector = c()
for (j in iterVector){
  sumVector = c(sumVector, j^3+4*(j^2))
}
sumVector
sum(sumVector)

iterVector = c(1:25)
sumVector = c()
for (j in iterVector){
  sumVector = c(sumVector, ((2^j)/(j))+(3^j)/(j^2))
}
sumVector
sum(sumVector)


x=1:10
##time for tail
install.packages("tictoc")
library("tictoc")
tic()
tail(x,5)
exectime <- toc()
exectime <- exectime$toc - exectime$tic
exectime


## create list of e^x cos(x)
vec=(30:60)/10
vecVals=c()
for (i in vec){
  j = exp(i)*cos(i)
  vecVals=c(vecVals,j)
}
vecVals


#vector of pairs
pairvec = c()
for (i in 1:20){
  for (j in i:20){
    pairvec=cbind(pairvec,c(i,j))
  }
}
pairvec

## function test
f.bad <- function(x, y) {
  z1 = 2*x + y
  z2 <- x + 2*y
  z3 <- 2*x + 2*y
  z4 <- x/y
}
f.bad(1,2)


collatz = function(n){
  if(n>0){
  if(n%%2==0){
    return(n/2)
  } else{
    return(3*n+1)
  }
  }else{
    print("WHYYYYYYY??? Your number isn't positive!")
  }
}

##Collatz
iterationsVector = c()
for(i in 1:1000){
  n = i
  iterations = 0
  while(n != 1 & n != 2 & n != 4){
    n = collatz(n)
    iterations = iterations + 1
  }
  iterationsVector = c(iterationsVector, iterations)
  print(iterations)
}

hist(iterationsVector, breaks = 1000)

##
cubesum = function(n){
  marker = n^(1/3)
  markerL = ceiling(marker)
  pairs = c()
  for (i in 1:markerL){
    for (j in i:markerL){
      if (i^3 + j^3 == n){
        pairs=cbind(pairs,c(i,j))
      }
    }
  }
  return (pairs)
}

##smallest number with two cubes
iter=1
while (length(cubesum(iter))<4){
  iter=iter+1
}

print 

tic()
## int -> int
## gives the nth fibonnaci number
fib = function(n){
  if (n<=0){
    return ("Error")
  } else if (n<=2) {
    return (1)
  } else {
    return(fib(n-1)+fib(n-2))
  }
}
fib(30)

exectime = toc()
exectime = exectime$toc - exectime$tic
exectime

## int, int -> vector of logicals
## 
fib_test = function(n,k){
  div_vec=c()
  for (i in 1:n){
    if (fib(i)%%k==0){
      div_vec = c(div_vec,TRUE)
    } else {
      div_vec = c(div_vec, FALSE)
    }
  }
  return (div_vec)
}

fib_test(25,6)

install.packages("tictoc")
library("tictoc")
tic()
rnorm(1000,0,1)
exectime <- toc()
exectime <- exectime$toc - exectime$tic

## Collatz with stored result
global_collatz=c()



## Fibonacchi with stored result
install.packages("tictoc")
library("tictoc")
tic()
rnorm(1000,0,1)
exectime <- toc()
exectime <- exectime$toc - exectime$tic

## Collatz with stored result
global_collatz=c()



## Fibonacchi with stored result

x=1:10
##time for tail
install.packages("tictoc")
library("tictoc")
tic()
tail(x,5)
exectime <- toc()
exectime <- exectime$toc - exectime$tic
exectime

tic()
global_fib = c(1,1)

fib = function(n){
  k = length(global_fib)
  if (k >= n){
    return (global_fib[n])
  } else{
    j = global_fib[k]+global_fib[k-1]
    print (global_fib)
    print (j)
    global_fib <<- c(global_fib,j)
    print (global_fib)
    fib(n)
  }
}

fib(30)

exectime = toc()
exectime = exectime$toc - exectime$tic
exectime