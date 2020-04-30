# Title     : Classification exercises
# Objective : Complete classification exercises
# Created by: grena
# Created on: 4/26/2020
library (ISLR )
names (Smarket)
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
lda.pred = predict(lda.fit,Smarket.2005)
lda.pred
names(lda.pred)
#I was looking to understand if given data is normalised
plot(Lag1)
pnorm(Lag1)
install.packages("ggpubr")
library("dplyr")
library("ggpubr")
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)
sum(lda.pred$posterior[,1] > 0.5) # predicted down
sum(lda.pred$posterior[,2] > 0.5) # predicted up
lda.pred$posterior [1:5 ,1]
lda.class[1:5]
sum(lda.pred$posterior[,1] > 0.6)
max(lda.pred$posterior[,1])

qda.fit=qda(Direction ~ Lag1 + Lag2, data = Smarket,
            subset = train)
qda.fit
qda.pred = predict(qda.fit,Smarket.2005)
qda.pred
names(qda.pred)
qda.class = qda.pred$class
table(qda.class,Direction.2005)
mean(qda.class == Direction.2005)
#KNN
rm(list=ls())
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
k_list <- 1:10
for (k in k_list) {
knn.pred=knn(train.X,test.X,train.Direction,k)
table(knn.pred,Direction.2005)
print(mean(knn.pred == Direction.2005))
}
rm(list=ls())
dim(Caravan)
attach(Caravan)
summary(Purchase)
fix(Caravan)
Yes = (Purchase == "Yes")
Sale = Caravan[Yes,]
Smarket.2005=Smarket[!train,]
train =(Year<2005)
length(Sale[,1])/length(Purchase)
length(Purchase)
length(Sale[,9])
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(standardized.X[ ,1])
test = 1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
k_list <- 1:10
for (k in k_list) {
knn.pred = knn(train.X,test.X,train.Y,k)
mean(knn.pred != test.Y)#error rate
cat('this k:', k)
cat(table(knn.pred,test.Y))
}

knn.pred = knn(train.X,test.X,train.Y,k=4)
mean(knn.pred != test.Y)#error rate
table(knn.pred,test.Y)

glm.fits=glm(Purchase~.,data=Caravan, family=binomial,
            subset = -test)
glm.pred=predict(glm.fits,Caravan[test,],type='response')
glm.list = rep("No",1000)
glm.list[glm.pred>0.5] = "Yes"
table(glm.list,test.Y)

glm.list[glm.pred>0.25] = "Yes"
table(glm.list,test.Y)

