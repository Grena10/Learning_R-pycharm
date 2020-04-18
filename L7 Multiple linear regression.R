# Title     : Multiple linear regression
# Objective : Multiple linear regression functions
# Created by: Elena
# Created on: 4/18/2020
library (MASS)
data(Boston)
attach(Boston)
lm.fit =lm(medv~lstat +age,data = Boston)
lm.fit =lm(medv~.,data = Boston )
summary(lm.fit)
summary(lm.fit)$r.sq #R2
summary(lm.fit)$sigma #RSE
vif(lm.fit)
library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age ,data= Boston)
summary(lm.fit1)
summary(lm.fit1)$r.sq #R2
lm.fit1=update(lm.fit1, ~.-indus)
summary(lm.fit1)
summary(lm.fit1)$r.sq #R2