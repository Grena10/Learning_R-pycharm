# Title     : Lesson 5 more Graphs
# Objective : Lesson 5
# Created by: grena
# Created on: 4/13/2020

Auto=read.table ("Auto.data",header=T,na.strings="?")
names(Auto)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
#attach - tells R to use column names only
plot(cylinders, mpg)
cylinders = as.factor(cylinders)
#convert cylinders to qualititative data, boxplot will be default graph
plot(cylinders,mpg)
plot(cylinders, mpg, col = "yellow",varwidth = T, xlab = "cylinders",
  ylab = "MPG")

hist(mpg)
hist(mpg, col=2, breaks=15)

pairs(Auto)
#scatterplot for every pair of variables.
pairs(~mpg + horsepower + weight, Auto)

plot(horsepower, mpg)
identify(horsepower, mpg, name)

summary(Auto)
summary(horsepower)