### UN INFANT MORTALITY DATA ###

# Write code here to load packages and data
install.packages("car")
install.packages("ggplot2")
install.packages("GGally")
library("car")
library("ggplot2")
library("GGally")
df=UN
# Write code here to calculate correlations
View(df)
raw = cor(df, use="complete.obs")
round(raw*100)

cor2 = function(dataframe){
  return(round(cor(dataframe, use="complete.obs")*100))
}

# Write code here to make a new dataframe with incomplete rows omitted
df2 = na.omit(df)

# Write code here to examine the distribution of the data
ggpairs(df)
# Write code here to take the log transformation of the data
ldf = log(df2)
# Write code here to examine the distribution of the log-transformed data
ggpairs(ldf)
# Calculate linear fit of infant mortality vs. GDP
linear_fit = lm(infant.mortality ~ gdp, df)
summary(linear_fit)
# Calculate linear fit of log(infant mortality) vs. log(GDP)
loglog_fit = lm(infant.mortality ~ gdp, ldf)

# Plot the linear fit of infant mortality vs. GDP
ggplot(df2, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")

# Plot of linear fit residuals
qplot(df2$gdp, linear_fit$residuals)

# Plot of linear fit residuals after log transformation of GDP and infant mortality
qplot(df2$gdp, df2$infant.mortality - exp(fitted(loglog_fit)))

#Interludes
names(summary(linear_fit))
