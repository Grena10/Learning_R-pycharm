# Title     : Classification exercises
# Objective : Complete classification exercises
# Created by: grena
# Created on: 4/26/2020
library (ISLR )
names ( Smarket )
dim(Smarket)
summary(Smarket)
fix(Smarket)
pairs(Smarket)
#correlation matrix
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
glm.fits=glm(Direction~Lag1+ Lag2+ Lag3+Lag4+ Lag5+Volume,
  data=Smarket, family = binomial )
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs = predict (glm.fits,type ="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs > 0.5] = "Up"
#confusion matrix
table(glm.pred,Direction)
mean(glm.pred == Direction)
(507+145)/1250
k = Direction
train =(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]
glm.fits=glm(Direction ~ Lag1 + Lag2 + Lag3+Lag4+Lag5+
    +Volume,data=Smarket,family = binomial,subset = train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)
plot(Lag1,Direction)
glm.fits=glm(Direction~Lag1 + Lag2,data = Smarket,
             family=binomial, subset =train)
predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),
      Lag2=c(1.1,-0.8)),type="response")
#lda
library(MASS)
lda.fit=lda(Direction~ Lag1 + Lag2,data = Smarket,
        subset = train)
lda.fit
plot(lda.fit)


