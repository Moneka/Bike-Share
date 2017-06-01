rm(list = ls())

# ================================================================
# Author: Moneka Bommasani
# Date: November 16, 2016
# Purpose: Project - Regression Analysis on Bike Sharing System
# ================================================================

# **********************************************************************************************

# 1) Load the libraries #

library(ISLR)
library(ggplot2)
library(reshape2)

# **********************************************************************************************

# 2) Import the data #

biker <- read.csv("C:/Users/Moneka Bommasani/Desktop/BikeSharing//hour.csv")

attach(biker) #Attach the dataset

# **********************************************************************************************

# 3) Descriptive Statistics #

nrow(biker) #Rows
ncol(biker) #Columns
dim(biker) #Dimensions
table(is.na(biker)) #To check for any missing values
is.integer(biker) #To check if all the value are integers
names(biker) #Headers of the dataset
str(biker) #To get the feel for data

# Range of few variables
range(biker$instant)
range(biker$temp)
range(biker$atemp)
range(biker$hum)
range(biker$windspeed)
range(biker$casual)
range(biker$registered)
range(biker$cnt)

summary(biker) #Summary of Whole data

# Setting the plot size
par(mfrow=c(1,2))

# Converting discrete variables into factors
season=as.factor(season)
weathersit=as.factor(weathersit)
holiday=as.factor(holiday)
workingday=as.factor(workingday)

# Proportions for Catogorical Variables
prop.table(table(season))
prop.table(table(weathersit))
prop.table(table(workingday))
prop.table(table(holiday))

# Number of Occurances
table(weathersit)
table(holiday)

# **********************************************************************************************

# 4) Exploratory Data Analysis (EDA) #

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# a) Distributions ( Normality Check )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Catogorical Variables
hist(biker$season, col= "green")
qqnorm(biker$season, col = "blue")
qqline(biker$season,col="red")
hist(biker$holiday, col ="green")
qqnorm(biker$holiday, col="blue")
qqline(biker$holiday,col="red")
hist(biker$workingday, col="green")
qqnorm(biker$weekday,col="blue")
qqline(biker$weekday,col="red")
hist(biker$weathersit,col="green")
qqnorm(biker$weathersit,col="blue")
qqline(biker$weathersit,col="red")

# Numerical Variables
hist(temp,col="green")
qqnorm(temp,col="blue")
qqline(temp,col="red")
hist(atemp,col="green")
qqnorm(atemp,col="blue")
qqline(atemp,col="red")
hist(hum,col="green")
qqnorm(hum,col="blue")
qqline(hum,col="red")
hist(windspeed,col="green")
qqnorm(windspeed,col="blue")
qqline(windspeed,col="red")

# Over Time
hist(yr,col="green")
qqnorm(yr,col="blue")
qqline(yr,col="red")
hist(hr,col="green")
qqnorm(hr,col="blue")
qqline(hr,col="red")
hist(mnth,col="green")
qqnorm(mnth,col="blue")
qqline(mnth,col="red")
hist(biker$weekday,col="green")
qqnorm(biker$weekday,col="blue")
qqline(biker$weekday,col="red")

# The Bike Count
hist(casual,col="green")
qqnorm(casual,col="blue")
qqline(casual,col="red")
hist(registered,col="green")
qqnorm(registered,col="blue")
qqline(registered,col="red")
hist(cnt,col="green")
qqnorm(cnt,col="blue")
qqline(cnt,col="red")

# The Bike Count - Log
hist(log(casual+1),col="green")
hist(log(registered+1),col="green")
hist(log(cnt),col="green")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# b) Bike Usage vs Time

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Setting the plot size
par(mfrow=c(2,2))
par(mar = c(1,1,1,1))

# Hourly
# For Total users
boxplot(biker$cnt~biker$hr,xlab="Hour",ylab="Count of Users who rented the bike",col="orange")
# For registered Users
boxplot(biker$registered~biker$hr,xlab="Hour",ylab="Count of Registered Users who rented the bike",col="green")
# For Casual Users
boxplot(biker$casual~biker$hr,xlab="Hour",ylab="Count of Casual Users who rented the bike",col="blue")
# To treat outliers
boxplot(log(biker$cnt)~biker$hr,xlab="Hour",ylab="Count of Users who rented the bike(Log)",col="orange")

# Daily
date=substr(biker$dteday,1,10)
days <- weekdays(as.Date(date))
boxplot(biker$cnt~days,xlab="Day",ylab="Count of Users who rented the bike",col="orange")
boxplot(biker$registered~days,xlab="Day",ylab="Count of Registered Users who rented the bike",col="green")
boxplot(biker$casual~days,xlab="Day",ylab="Count of Casual Users who rented the bike",col="blue")
boxplot(log(biker$cnt)~days,xlab="Day",ylab="Count of Users who rented the bike(Log)",col="orange")

# Monthly
boxplot(biker$cnt~biker$mnth,xlab="Month",ylab="Count of Users who rented the bike",col="orange")
boxplot(biker$registered~biker$mnth,xlab="Month",ylab="Count of Registered Users who rented the bike",col="green")
boxplot(biker$casual~biker$mnth,xlab="Month",ylab="Count of Casual Users who rented the bike",col="blue")
boxplot(log(biker$cnt)~biker$mnth,xlab="Month",ylab="Count of Users who rented the bike(Log)",col="orange")

# Yearly
boxplot(biker$cnt~biker$yr,xlab="Year",ylab="Count of Users who rented the bike",col="orange")
boxplot(biker$registered~biker$yr,xlab="Year",ylab="Count of Registered Users who rented the bike",col="blue")
boxplot(biker$casual~biker$yr,xlab="Year",ylab="Count of Casual Users who rented the bike",col="green")
boxplot(log(biker$cnt)~biker$yr,xlab="Year",ylab="Count of Users who rented the bike(Log)",col="orange")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# c) Bike Usage vs Categorical variables

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Season

boxplot(biker$cnt~biker$season,xlab="Season",ylab="Count of Users who rented the bike",col="orange")
boxplot(biker$registered~biker$season,xlab="Season",ylab="Count of Registered Users who rented the bike",col="blue")
boxplot(biker$casual~biker$season,xlab="Season",ylab="Count of Casual Users who rented the bike",col="green")
boxplot(log(biker$cnt)~biker$season,xlab="Season",ylab="Count of Users who rented the bike(Log)",col="orange")

# Weather

boxplot(biker$cnt~biker$weathersit,xlab="Weather",ylab="Count of Users who rented the bike",col="orange")
boxplot(biker$registered~biker$weathersit,xlab="Weather",ylab="Count of Registered Users who rented the bike",col="blue")
boxplot(biker$casual~biker$weathersit,xlab="Weather",ylab="Count of Casual Users who rented the bike",col="green")
boxplot(log(biker$cnt)~biker$weathersit,xlab="Weather",ylab="Count of Users who rented the bike(Log)",col="orange")

# Holiday

boxplot(biker$cnt~biker$holiday,xlab="Holiday",ylab="Count of Users who rented the bike",col="orange")
boxplot(biker$registered~biker$holiday,xlab="Holiday",ylab="Count of Registered Users who rented the bike",col="blue")
boxplot(biker$casual~biker$holiday,xlab="Holiday",ylab="Count of Casual Users who rented the bike",col="green")
boxplot(log(biker$cnt)~biker$holiday,xlab="Holiday",ylab="Count of Users who rented the bike(Log)",col="orange")

# Working day

boxplot(biker$cnt~biker$workingday,xlab="Working Day",ylab="Count of Users who rented the bike",col="orange")
boxplot(biker$registered~biker$workingday,xlab="Working Day",ylab="Count of Registered Users who rented the bike",col="blue")
boxplot(biker$casual~biker$workingday,xlab="Working Day",ylab="Count of Casual Users who rented the bike",col="green")
boxplot(log(biker$cnt)~biker$workingday,xlab="Working Day",ylab="Count of Users who rented the bike(Log)",col="orange")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# d) Bike Usage vs Numerical Variables(Linear Models)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Temperature

# Temperature vs season 
boxplot(biker$temp ~ biker$season,
        data = biker,
        xlab = "Season",
        ylab = "Temperature",
        main = "Temperature by Season",
        col = "skyblue")

m1 <- lm(biker$cnt ~ biker$temp) #Linear model with temp

#Summary of the model
summary(m1) 
coef(m1)
names(m1)
print(m1)
confint(m1)

#Plotting the model
plot(biker$cnt ~ biker$temp, col="orange" , xlab="Temp", ylab="Count")
abline(m1, lwd=3, col="red")
plot(m1)

# Feels like Temperature

m2 <- lm(biker$cnt ~ biker$atemp) #Linear model with atemp

#Summary of the model
summary(m2) 
coef(m2)
names(m2)
print(m2)
confint(m2)

plot(biker$cnt ~ biker$atemp, col="orange", xlab="Feels like", ylab="Count")
abline(m2, lwd=3, col="red")
plot(m2)

#Feels like temp vs count scatterplot with regression line differentiating both the years
#It shows that influence of temperature for 2011 is more significant than for 2012

# blank plot
plot(x = 1,
     xlab = "Feels like Temperature",
     ylab = "Number of Rents",
     xlim = c(0,1),
     ylim = c(0,1000),
     main = "Temperature vs. Count")

# draw points for 2011 year
points(x = biker$atemp[biker$yr == 0],
       y = biker$cnt[biker$yr == 0],
       pch = 16,
       col = "red",
       cex = 0.5
)
# draw points for 2012 year
points(x = biker$atemp[biker$yr == 1],
       y = biker$cnt[biker$yr == 1],
       pch = 16,
       col = "darkgreen",
       cex = 0.5
)

# add regression lines for two ears
abline(lm(biker$cnt~biker$atemp, subset = biker$yr == 0),
       col = "darkgreen",
       lwd = 3)

abline(lm(biker$cnt~biker$atemp, subset = biker$yr == 1),
       col = "red",
       lwd = 3)

# add legend
legend("topleft",
       legend = c(2011, 2012),
       col = c("darkgreen","red"),
       pch = c(16, 16),
       bg = "white",
       cex = 1
)

# Humidity

m3 <- lm(biker$cnt ~ biker$hum) #Linear model with humidity

#Summary of the model
summary(m3) 
coef(m3)
names(m3)
print(m3)
confint(m3)

plot(biker$cnt ~ biker$hum, col="orange", xlab="Humidity", ylab="Count")
abline(m3, lwd=3, col="red")
plot(m3)

# Windspeed

m4 <- lm(biker$cnt ~ biker$windspeed) #Linear model with windspeed

#Summary of the model
summary(m4) 
coef(m4)
names(m4)
print(m4)
confint(m4)

plot(biker$cnt ~ biker$windspeed, col="orange", xlab="Windspeed", ylab="Count")
abline(m4, lwd=3, col="red")
plot(m4)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Correlation Matrix

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# With all variables
cor.matrix <- cor(biker[c(3:17)])
round(cor.matrix,2)
cor.matrix <- melt(cor.matrix, varnames = c("x","y"))
ggplot(data = cor.matrix, aes(x, y, fill = "value")) + geom_tile(aes(fill = value), colour="white") + scale_fill_gradient(low = "white", high = "steelblue")

# Temperature, Windspeed, Humidity, Year, Hour, Month and Weather situation
sub=data.frame(biker$cnt,biker$hr,biker$yr, biker$mnth, biker$weathersit,biker$temp,biker$hum,biker$windspeed)
subcor <- cor(sub)
cor(sub)
cor.matrix <- melt(subcor, varnames = c("x","y"))
ggplot(data = cor.matrix, aes(x, y, fill = "value")) + geom_tile(aes(fill = value), colour="white") + scale_fill_gradient(low = "white", high = "steelblue")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Correlation Matrix - LOG of dependent Variable

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

biker$logcnt <- log(biker$cnt)
range(biker$logcnt) 

# With all variables
cor.matrix2 <- cor(biker[c(3:18)])
round(cor.matrix2,2)
cor.matrix2 <- melt(cor.matrix2, varnames = c("x","y"))
ggplot(data = cor.matrix2, aes(x, y, fill = "value")) + geom_tile(aes(fill = value), colour="white") + scale_fill_gradient(low = "white", high = "steelblue")

# Temperature, Windspeed, Humidity, Year, Hour, Season, working day and Weather situation
sub2=data.frame(biker$logcnt,biker$hr,biker$yr, biker$season, biker$weathersit,biker$temp,biker$hum,biker$windspeed, biker$workingday)
subcor2 <- cor(sub2)
cor(sub2)
cor.matrix2 <- melt(subcor2, varnames = c("x","y"))
ggplot(data = cor.matrix2, aes(x, y, fill = "value")) + geom_tile(aes(fill = value), colour="white") + scale_fill_gradient(low = "white", high = "steelblue")

# **********************************************************************************************

# 5) Fitting the Model #

# Multiple Linear Regression

# With all variables
mall <- lm(biker$cnt ~ biker$season + biker$weathersit + biker$temp + biker$atemp + biker$windspeed + biker$hum +biker$yr + biker$mnth + biker$hr + biker$holiday + biker$weekday + biker$workingday )
summary(mall)
plot(mall)

# With all variables - LOG of Dependant Variable
mall2 <- lm(biker$logcnt ~ biker$season + biker$weathersit + biker$temp + biker$atemp + biker$windspeed + biker$hum +biker$yr + biker$mnth + biker$hr + biker$holiday + biker$weekday + biker$workingday )
summary(mall2)
plot(mall2)

# Till p value is less than or equal to 0.05
redm <- lm(biker$logcnt ~ biker$season  + biker$atemp + biker$windspeed + biker$hum +biker$yr  + biker$hr + biker$holiday + biker$weekday + biker$workingday)
summary(redm)
plot(redm)

# According to correlation matrix
reg_model <- lm(biker$logcnt ~ biker$season + biker$yr + biker$hr + biker$weathersit +biker$temp +biker$hum +biker$windspeed + biker$workingday)
summary(reg_model)
print(reg_model)
confint(reg_model)
plot(reg_model)

# Comparing the models

# null hypothesis is beta 1= beta 2 = ... = 0 (Both models are same or different/ Useful or not)
anova(redm,mall2) #cannot reject null hypothesis some variables do not contribute
anova(reg_model, redm) #reject null hypothesis that variables collectively have no effect on count

# Residuals for the best model

# Standard Residuals
residuals = rstandard(redm)
plot(residuals, col="red")
abline(0,0)
abline(2,0)
abline(-2,0)
qqnorm(residuals, ylab="standardised residuals", xlab="Normal Scores",
       main="Residuals - Normality Plot", col="red")
qqline(residuals)

# **********************************************************************************************

# 6) Predicting #

biker$logpredict <- predict(redm, biker)
plot(biker$logpredict,biker$logcnt, xlab = "Predicted Count Log", ylab = "Original Count Log")

biker$predictcnt <- exp(predictions)
plot(biker$predictcnt, biker$cnt, xlab = "Predicted Count", ylab = "Original Count")

View(biker)
