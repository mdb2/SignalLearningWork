install.packages("HistData")
install.packages("dplyr")
library("HistData")
library("dplyr")
df =  GaltonFamilies
View(df)
mean(df$father)/mean(df$mother) #why mother is multiplied by 1.08
names(df)

##Coerce gender into numeric
## males are assigned 1; females are assigned 0
gendernumeral = c()
for (i in df$gender){
  if (i == "male"){
    gendernumeral=(c(gendernumeral,1))
  } else {
    gendernumeral=(c(gendernumeral,0))
  }
}
gendernumeral = as.numeric(gendernumeral)
gendernumeral

## add gendernumeral to df
df$GenderNumeral = gendernumeral
names(df)
View(df)

##Testing gender effect on child height
#male height
male_average=mean(df$childHeight[df$GenderNumeral==1])
female_average=mean(df$childHeight[df$GenderNumeral==0])
male_average/female_average

male_average/mean(df$father)
female_average/mean(df$mother)

##Aggregate by family
arrange(df, family)
View(df)

##Children Plotting Against Parents
family_count = max(as.numeric((df$family)))
average_child_vec = c()
mother_vec = c()
father_vec = c()
mid_vec = c()
for (i in 1:family_count){
  temp = filter(df, family==i)
  a_child = mean(as.numeric(temp$child))
  average_child_vec = c(average_child_vec,a_child)
  mother_vec = c(mother_vec, temp$mother)
  father_vec = c(father_vec, temp$father)
}
plot(average_child_vec, mother_vec)
