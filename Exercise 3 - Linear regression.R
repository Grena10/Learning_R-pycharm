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
summary(lm.fit2)
plot(x,y)
abline()
#less noise
set.seed(1)
x = rnorm(100)
eps = rnorm(100, sd = sqrt(0.01))
y = -1 + 0.5*x + eps
length(y)
lm.fit=lm(y~x)
summary(lm.fit)
plot(x,y)
abline(lm.fit,col="yellow")
abline(-1,0.5,col="blue")
legend("topleft", c("Least square", "Regression"),
       col = c("yellow", "blue"), lty = c(1, 1))
confint(lm.fit)

#more noise
set.seed(1)
x = rnorm(100)
plot(1:100, x)
eps = rnorm(100, sd = sqrt(0.5))
y = -1 + 0.5*x + eps
length(y)
lm.fit=lm(y~x)
summary(lm.fit)
plot(x,y)
abline(lm.fit,col="yellow")
abline(-1,0.5,col="blue")
legend("topleft", c("Least square", "Regression"),
       col = c("yellow", "blue"), lty = c(1, 1))

confint(lm.fit)

#collinearity problem
set.seed(1)
x1=runif(100)
plot(1:100, x1)
x2 =0.5 * x1 + rnorm (100) /10
y = 2 + 2 * x1 +0.3 * x2 + rnorm (100)
plot(x1,x2)
lm.fit=lm(y~x1+x2)
summary(lm.fit)
plot(predict (lm.fit ), rstudent (lm.fit ))
par (mfrow =c(2 ,2))
plot(lm.fit)

lmx1.fit=lm(y~x1)
summary(lmx1.fit)
plot(predict (lmx1.fit ), rstudent (lmx1.fit ))
plot(lmx1.fit)

lmx2.fit=lm(y~x2)
summary(lmx2.fit)
plot(predict (lmx2.fit ), rstudent (lmx2.fit ))
plot(lmx2.fit)

x1=c(x1 , 0.1)
x2=c(x2 , 0.8)
y=c(y ,6)

plot(x1,y)

#using Boston data for multiple linear regression
library (MASS)
data(Boston)
attach(Boston)
names(Boston)
lm.fit = lm(crim~zn)
summary(lm.fit)

lm.fit = lm(crim~indus)
summary(lm.fit)

lm.fit = lm(crim~chas)
summary(lm.fit)

lm.fit = lm(crim~nox)
summary(lm.fit)

lm.fit = lm(crim~rm)
lm.fit2 = lm(crim~rm+I(rm^2)+I(rm^3))
summary(lm.fit2)

lm.fit = lm(crim~age)
summary(lm.fit)

lm.fit = lm(crim~dis)
summary(lm.fit)

lm.fit = lm(crim~rad)
summary(lm.fit)
plot(rad,crim)

lm.fit = lm(crim~tax)
summary(lm.fit)
lm.fit2 = lm(crim~tax+I(tax^2)+I(tax^3))
summary(lm.fit2)

lm.fit = lm(crim~ptratio)
summary(lm.fit)
par (mfrow =c(1 ,1))
plot(crim,ptratio)

lm.fit = lm(crim~black)
summary(lm.fit)

lm.fit = lm(crim~lstat)
summary(lm.fit)


lm.fit = lm(crim~medv)
summary(lm.fit)

lm.fit = lm(crim~.,data = Boston)
summary(lm.fit)

coef_sin = c(-0.07, 0.51,-1.89, 31.24, -2.7, 0.1,
              -1.55, 0.61, 0.03,1.15, -0.03, 0.54, -0.36)
coef_mul = c(0.45,-0.06,-0.749134, -10, -.43, 0,
             -0.98,0.58,0,-0.27,0,0.12,-0.19)
plot(coef_mul,coef_sin)