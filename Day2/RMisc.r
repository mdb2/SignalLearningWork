nesting_depth = function(listNest){
  unlistedNest = listNest
  count = 0
  while(TRUE){
    unlistedNest = unlist(unlistedNest, recursive = FALSE)
    unNested = TRUE
    count = count + 1
    for(i in unlistedNest)
    {
      if(typeof(i) == "list"){
        unNested = FALSE
      }
    }
    if(unNested){
      return(count)
    }
  }
}
nesting_depth(list(1, list(2, 3), list(4, 5)))

##all dominos
all_domino = function(n){
  dom_list = NULL
  for (i in 0:n){
    for (j in i:n){
      new_domino = list(c(i,j))
      dom_list = append(dom_list, new_domino)
    }
  }
  return (dom_list)
}

all_domino(10)

##is cicrle?
is_circle = function(L){
  flat = unlist(L)
  len = length(flat)
  if (flat[1] != flat[len]){
    return (FALSE)
  } else {
    iter = c(2:(len-2))
    range = iter[c(TRUE,FALSE)]
    for (i in range){
      if (flat[i] != flat[i+1]){
        return (FALSE)
      }
     }
  }
  return (TRUE)
}

test_circle = list(list(1, 2), list(2, 3), list(3, 1))
is_circle(test_circle)

## Regular Expressions

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

all_letter = function(df){
  freq_vec = c()
  cols = colnames(df)
  merger = ""
  for (j in cols){
    merger = paste(merger,j)
  }
  for (i in 1:26){
    a = substr(alphabet,i,i)
    text = unlist(strsplit(merger,""))
    l = length(grep(a,text,ignore.case=TRUE))
    freq_vec[i] = l
  }
  return (freq_vec)
}

all_letter(mtcars)

##gsub
sub_letter = function(df){
  cols = colnames(df)
  for(i in 1:length(cols)){
    colnames(df)[i] = paste(gsub(" ", ".", colnames(df)[i]),"_mod", sep = "")
  }
  return(df)
}
exampleDF = data.frame(1,2,3)
names(exampleDF) = c("adsf sdf", "a a", "q.q")
sub_letter(exampleDF)

#removal

take_4 = function(df){
  len = ncol(df)
  for (i in 1:len){
    t = nchar(colnames(df)[i])
    col = colnames(df)[i]
    colnames(df)[i]= substr(col,1,(t-4))
  }
  return (df)
}
exampleDF = data.frame(1,2,3)
names(exampleDF) = c("adsf sdf", "a a", "q.q")
sub_letter(exampleDF)
take_4(exampleDF)

## Row Join
row_join = function(df){
  rows = as.list(rownames(df))
  do.call(paste,c(rows, sep="_"))
}
row_join(mtcars)

goDirection = function(df, coords, direction){
  tmpCoords = coords
  if(direction == "RIGHT"){
    tmpCoords[1] = tmpCoords[1]+1
    if(tmpCoords[1]>=ncol(df) | df[tmpCoords[1],tmpCoords[2]] == "BEENHERE"){
      return(c(df,coords,"DOWN",FALSE))
    }else{
      df[tmpCoords[1],tmpCoords[2]] == "BEENHERE"
      return(c(df,coords,"RIGHT",TRUE))
    }
  }else if(direction == "DOWN"){
    tmpCoords[2] = tmpCoords[2]+1
    if(tmpCoords[2]>=nrow(df) | df[tmpCoords[1],tmpCoords[2]] == "BEENHERE"){
      return(c(df,coords,"LEFT",FALSE))
    }else{
      df[tmpCoords[1],tmpCoords[2]] == "BEENHERE"
      return(c(df,coords,"DOWN",TRUE))
    }
  }else if(direction == "LEFT"){
    tmpCoords[1] = tmpCoords[1]-1
    if(tmpCoords[1]>=ncol(df) | df[tmpCoords[1],tmpCoords[2]] == "BEENHERE"){
      return(c(df,coords,"UP",FALSE))
    }else{
      df[tmpCoords[1],tmpCoords[2]] == "BEENHERE"
      return(c(df,coords,"LEFT",TRUE))
    }
  }else if(direction == "UP"){
    tmpCoords[2] = tmpCoords[2]-1
    if(tmpCoords[2]>=nrow(df) | df[tmpCoords[1],tmpCoords[2]] == "BEENHERE"){
      return(c(df,coords,"RIGHT",FALSE))
    }else{
      df[tmpCoords[1],tmpCoords[2]] == "BEENHERE"
      return(c(df,coords,"UP",TRUE))
    }
  }
}
spiral = function(df){
  spiralDF = df
  coords = c(1,1)
  direction = "RIGHT"
  continue = TRUE
  while(continue){
    for(i in 1:4){
      tmp = goDirection(df, coords, direction)
      df = tmp[1]
      coords = tmp[2]
      direction = tmp[3]
      if(tmp[4]){
        output = c(output,df[coords[1],coords[2]])
        break()
      }else if(i == 4){
        continue = FALSE
      }
    }
  }
  return(df)
}

exampleDF = data.frame(matrix(1:9,nrow = 3))
spiral(exampleDF)