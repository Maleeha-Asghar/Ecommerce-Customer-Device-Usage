#---------Importing data--------- 
data <-read.csv("Ecommerce Customers",sep = ",", header = T, stringsAsFactors = F)
data
#View(data)
 
#---------Data Analysis---------
head(data)
str(data)
summary(data)

#Checking for Null values
is.na(data)   
any(is.na(data))

#---------Data Visualization---------
installed.packages("ggplot2")
installed.packages("tidyverse")
library(tidyverse)

#Checking Correlation
#Time on Website vs Yearly Amount Spent
scatter.plot1<-ggplot(data, aes(Time.on.Website,Yearly.Amount.Spent)) + 
  theme_light() +
  geom_point(color = "blue") +
  labs(x= "Time spent on Website", 
       y= "Yearly Amount Spent",
       title= "Time Spent on Website against Yearly Amount Spent")
scatter.plot1  

#Average Session Length vs Yearly Amount Spent
scatter.plot2<-ggplot(data, aes(Avg..Session.Length,Yearly.Amount.Spent)) + 
  theme_light() +
  geom_point(color = "blue") +
  labs(x= "Average Session Length", 
       y= "Yearly Amount Spent",
       title= "Average Session Length against Yearly Amount Spent")
scatter.plot2  

#Length of Membership vs Yearly Amount Spent
scatter.plot3<-ggplot(data, aes(Length.of.Membership,Yearly.Amount.Spent)) + 
  theme_light() +
  geom_point(color = "blue") +
  labs(x= "Length of Membership", 
       y= "Yearly Amount Spent",
       title= "Length of Membership against Yearly Amount Spent")
scatter.plot3

#Pair plot
pairplot <-pairs(data[c("Avg..Session.Length","Time.on.App","Time.on.Website","Length.of.Membership","Yearly.Amount.Spent")],
                 col = "blue", pch = 16, 
                 main= "Pairplot of all Continuous Variables")
pairplot

#Length of Membership and Yearly Amount Spent are Correlated

#Checking if variable(Length of Membership) is normally distributed
#hist(data$Length.of.Membership)
hist <-ggplot(data,aes(x=Length.of.Membership)) +
       geom_histogram(color = "white", fill ="blue", binwidth = 0.5) +
       ggtitle("Histogram of Length of Membership") 
hist

#boxplot(data$Length.of.Membership)
 box <- ggplot(data,aes(x= Length.of.Membership)) +
   geom_boxplot(fill ="blue",outlier.colour ="red",outlier.shape =4) +
   labs(title="BoxPlot of Length of Membership") +
   theme_light() 
 box

 #---------Fitting Linear Regression Model----------
 attach(data)
 lm.fit <- lm(Yearly.Amount.Spent~Length.of.Membership)

 summary(lm.fit)
 
 plot(Yearly.Amount.Spent~Length.of.Membership)
 abline(lm.fit,col= "red")
 
 #---------Residual Analysis-----------
 #Checking if residuals are normally distributed
 #Residuals-Distance between points and regression line
 
 #Distribution of Data divided into different quantiles
 qqnorm(residuals(lm.fit))
 qqline(residuals(lm.fit), col = "red")
 
 #Histogram
 hist(residuals(lm.fit))
 
 #Shapiro-Wilk Normality Test
 shapiro.test(residuals(lm.fit))
 #p-value greater than 0.5 - we do not reject h0(Null Hypothesis)
 
 
 #---------Train-Test Split----------
 
 set.seed(1)
 row.number <- sample(1:nrow(data), 0.8*nrow(data))
 
 train <- data[row.number,]
 test <- data[-row.number,]
 
 #---------Evaluation of Model----------
 #---------Fitting linear model on train set----------
lm.fit.train <- lm(Yearly.Amount.Spent~Length.of.Membership, data=train)
summary(lm.fit.train)

#----------Prediction on Test set----------
prediction <-predict(lm.fit.train,newdata = test)
error <- prediction - test$Yearly.Amount.Spent
 
#----------Root Mean Square Error----------
rmse <- sqrt(mean(error^2))

#Mean absolute percentage error
 mape <- mean(abs(error/test$Yearly.Amount.Spent))
 
 c(RMSE=rmse,mape=mape,R2=summary(lm.fit.train)$r.squared)
 
#---------Multiple Regression Model-----------
 
 attach(data)
 lm.fit1 <- lm(Yearly.Amount.Spent~Avg..Session.Length + Time.on.App + Time.on.Website + Length.of.Membership)
 summary(lm.fit1)
 
 #---------Evaluation of Multiple Regression Model----------

 set.seed(1)
 row.number <- sample(1:nrow(data), 0.8*nrow(data))
 
 train <- data[row.number,]
 test <- data[-row.number,]
 multi.lm.fit.train <- lm(Yearly.Amount.Spent~Avg..Session.Length +
                            Time.on.App + 
                            Time.on.Website +
                            Length.of.Membership, 
                          data=train)
 
 summary(multi.lm.fit.train)
 
 
 #-------Prediction---------
 prediction.multi.train<- predict(multi.lm.fit.train, newdata = test)
 err0.8 <- prediction.multi.train  - test$Yearly.Amount.Spent
 rmse <- sqrt(mean(err0.8^2))
 mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))
 
 c(RMSE=rmse,mape=mape,R2=summary(multi.lm.fit.train)$r.squared)
 
 
 
 #---RESULTS ARE MUCH BETTER WITH MULTIPLE REGRESSION ANALYSIS---
 
 
 
 
 
 
 
 