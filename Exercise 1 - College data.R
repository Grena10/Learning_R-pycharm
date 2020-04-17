# Title     : Exercise: college data
# Objective : Analyse college data exercise
# Created by: grena
# Created on: 4/14/2020
college = read.csv("College.csv")
rownames(college)=college[,1]
college = college[,-1]
summary(college)
pairs(college[,1:5])
attach(college)
pairs(~Private + Top10perc + Top25perc,college[1:10,])
Private = as.factor(Private)
plot(Private,Outstate,xlab="Private", ylab="Outstate",varwidth = T)
Elite=rep("No",nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college, Elite)
plot(college$Elite, college$Outstate,xlab="Elite", ylab="Outstate")
hist(college$Top10perc)
names(college)
hist(college$Accept,col=4, breaks=20)
rm(list=ls())
#working with Auto dataset

Auto=read.table ("Auto.data",header=T,na.strings="?")
Auto=na.omit(Auto)
names(Auto)
range(Auto$mpg)
range(Auto$year)
range(Auto$horsepower)
A = summary(Auto)
sd(Auto$mpg)
sd(Auto$horsepower)
Test = Auto[-10:-85,]
sd(Test$mpg)
sd(Test$horsepower)
attach(Auto)
hist(mpg,col=4, breaks=20)
plot(mpg[1:100],horsepower[1:100],xlab="mpg", ylab="horsepower",
main="mpg vs horsepower")
cylinders = as.factor(cylinders)
plot(cylinders,mpg,xlab="mpg", ylab="cylinder",
main="mpg vs cylinders")
y_name = "year"
year = as.factor(year)
plot(year,mpg,ylab="mpg", xlab=y_name,
main=c("mpg vs",y_name))

attach(Boston)
plot(age, rm, xlab="age", ylab="room",
main="age vs room")

plot( medv,indus, xlab="median price", ylab="non-retails",
main="medv vs proportion of non-retails")
chas = as.factor(chas)
plot(chas,crim, xlab="non-retails", ylab="crime",
main="non-retails vs crime")
names(Boston)
plot(ptratio,crim, xlab="tax", ylab="crime",
main="tax vs crime")
A = data.frame(Boston$chas != 1)
dim(A)
median(Boston$ptratio)
hist(rm,breaks=5)
rm=as.numeric(rm)
B = data.frame(Boston$rm > 8)
length(Boston$rm >7)
mean(Boston$rm)
dems = Boston[Boston$rm > 7 ,]