# Title     : Qualitative Predictors
# Objective : Exercises for qualitative predictors
# Created by: Elena S
# Created on: 4/18/2020
rm(list=ls())
library (ISLR)
data(Carseats)
attach(Carseats)
fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
contrasts (ShelveLoc)#coding for qualitative variables