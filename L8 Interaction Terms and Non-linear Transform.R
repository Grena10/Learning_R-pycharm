# Title     : Interaction Terms
# Objective : Multiple linear regression with interactoin terms and nonlinear
# Created by: Elena
# Created on: 4/18/2020

library (MASS)
data(Boston)
attach(Boston)

summary(lm(medv~lstat*age, data = Boston))

lm.fit2=lm(medv~lstat +I(lstat^2))
summary(lm.fit2)
plot(medv,lstat)
abline(lm.fit2,lwd=2)
lm.fit=lm(medv~lstat, data = Boston)
anova (lm.fit ,lm.fit2)
#anova() function performs a hypothesis test comparing the two models.
par(mfrow =c(2 ,2))# divide screen to have a grid 2x2
plot(lm.fit2)
plot(medv,lstat)
abline (lm2.fit,lwd=2)

lm.fit5=lm(medv~poly(lstat ,5))
summary(lm.fit5)
summary(lm.fit2)

anova (lm.fit5 ,lm.fit2)

summary(lm(medv~log(rm),data= Boston))
lm_log = lm(medv~log(rm))
plot(lm_log)