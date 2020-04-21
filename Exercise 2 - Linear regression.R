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