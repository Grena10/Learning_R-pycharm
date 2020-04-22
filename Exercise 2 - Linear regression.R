# Title     : Linear regression
# Objective : Linear regression exercises
# Created by: grena
# Created on: 4/20/2020

LoadLibraries <- function() {
  library (ISLR)
  library (MASS)
  print("libraries loaded")
}
LoadLibraries()

Auto=read.table ("Auto.data",header=T,na.strings="?")
fix(Auto)
names(Auto)
attach(Auto)
ompleterecords <- na.omit(datacollected)
Auto_nona = na.omit(Auto)
attach(Auto_nona)
lm.fit =lm(mpg~horsepower,data = Auto)
summary(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(horsepower=98),interval = "prediction")
predict(lm.fit,data.frame(horsepower=98),interval = "confidence")
plot(horsepower,mpg)
abline(lm.fit,lwd=2,col="red")
abline(41,-0.15,lwd=2)
plot(lm.fit)
which.max(hatvalues(lm.fit))
#multiple regression
pairs(Auto)
lm_all.fit =lm(mpg~.,data = Auto)
summary(lm_all.fit)
Auto_reduced = Auto[ , !(names(Auto) %in% 'name')]
Auto_reduced = na.omit(Auto_reduced)
a = cor(Auto_reduced)
lm_all.fit=lm(mpg~.,data = Auto_reduced)
summary(lm_all.fit)
par (mfrow =c(2 ,2))
plot(lm_all.fit)
which.max(hatvalues(lm_all.fit))
names(Auto)
lm_all1.fit=lm(mpg~. + cylinders*horsepower,data = Auto_reduced)
summary(lm_all1.fit)
lm_all1.fit=lm(mpg~. - horsepower + weight*displacement,
               data = Auto_reduced)
summary(lm_all1.fit)

#carseats dataset
fix(Carseats)
names(Carseats)
attach(Carseats)
lm_fit=lm(Sales~Price + Urban + US, data=Carseats)
summary(lm_fit)
contrasts(Urban)
lm_fit=lm(Sales~Price + US, data=Carseats)
summary(lm_fit)
confint(lm_fit)
plot(lm_fit)
plot(predict (lm_fit ), rstudent (lm_fit ))

#t-statistic investigation
rm(list=ls())
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
plot(x,y)
par (mfrow =c(1 ,1))
lm.fit=lm(y~x+0)
summary(lm.fit)
plot(x,y)
abline(lm.fit)

lmx.fit=lm(x~y)
summary(lmx.fit)
plot(y,x)
abline(lmx.fit)

n = length(x)
t = sqrt(n - 1)*(x%*%y)/sqrt(sum(x^2)*sum(y^2) - (x%*%y)^2)

x=rnorm(100)
y=x

lmx.fit=lm(x~y+0)
summary(lmx.fit)

#exercise 13
set.seed(1)
x = rnorm(100)
eps = rnorm(100, sd = sqrt(0.25))
y = -1 + 0.5*x + eps
length(y)
lm.fit=lm(y~x)
summary(lm.fit)
plot(x,y)
abline(lm.fit,col="yellow")
abline(-1,0.5,col="blue")
legend("topleft", c("Least square", "Regression"),
       col = c("yellow", "blue"), lty = c(1, 2))
lm.fit2 = lm(y~x+I(x^2))
