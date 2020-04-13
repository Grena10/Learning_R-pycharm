# Title     : Basic commands
# Objective : Learn basic commands
# Created by: grena
# Created on: 4/11/2020

x<-c(1,2,3)
x
y = c(3,5,6)
y
length(x)
length(y)
ls()
rm(list = ls())
x=matrix(data = c(1,2,3,4), 2,2)
y=matrix(c(5,6,7,8),2,2,byrow = TRUE)
s = sqrt(x)
x2 = x^2
dis1 = rnorm(50)
dis1[1]
dis2 = dis1 +rnorm(50,50,0.1)
dis2
set.seed(3)
"We can set seed set for random numbers to check the correctness of calculations"
y = rnorm(100)
mean(y)
var(y)
sd(y)