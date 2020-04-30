# Title     : Classification
# Objective : Complete exercises
# Created by: grena
# Created on: 4/28/2020
exp(-0.5)/(1+exp(-0.5))
log(1)
si = 6
(2*pi+si^2)

library (ISLR )
names(Weekly)
dim(Weekly)
summary(Weekly)
fix(Weekly)
pairs(Weekly)
attach(Weekly)
Year = Weekly$Year
head(Weekly)
cor(Weekly[,-9])
plot(Volume)
new_weekly = na.omit(Weekly)
glm.fit=glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit,type ="response")
glm.pred=rep("Down",1089)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred,Direction)
mean(glm.pred == Direction)
type(Year)
print(class(Year))
train <- (Year < 2009)
plot(Year)
Weekly.2009 = Weekly[!train,]
Direction.2009 = Direction[!train]
glm2.fits=glm(Direction ~  Lag2, data = Weekly, family=binomial,
    subset = train)
summary(glm2.fits)
glm2.probs=predict(glm2.fits,Weekly.2009,type="response")
length(glm2.probs)
glm2.pred=rep("Down",104)
glm2.pred[glm2.probs > 0.5] = "Up"
table(glm2.pred,Direction.2009)
mean(glm2.pred == Direction.2009)

#lda
library(MASS)
lda.fit=lda(Direction ~ Lag2,data = Weekly,
        subset = train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit,Weekly.2009)
lda.pred
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2009)
mean(lda.class == Direction.2009)

#lda
library(MASS)
lda.fit=lda(Direction~ Lag1 + Lag2,data = Weekly,
        subset = train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit,Weekly.2009)
names(lda.pred)
lda.class=lda.pred$class

table(lda.class,Direction.2009)
mean(lda.class == Direction.2009)

#qda
qda.fit=qda(Direction ~  Lag2, data = Weekly,
            subset = train)
qda.fit
qda.pred = predict(qda.fit,Weekly.2009)
names(qda.pred)
qda.class = qda.pred$class
table(qda.class,Direction.2009)
mean(qda.class == Direction.2009)
plot(qda.fit)

#KNN
#rm(list=ls())
library(class)
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
hist(train.X)
hist(test.X)
train.Direction=Direction[train]
dim(train.X)
dim(test.X)
length(train.Direction)
set.seed(1)
#k_list <- 1:10
#for (k in k_list) {

knn.pred=knn(train.X,test.X,train.Direction,k=20)
table(knn.pred,Direction.2009)
mean(knn.pred == Direction.2009)
#}

#Auto dataset
rm(list=ls())
Auto=read.table ("Auto.data",header=T,na.strings="?")
Auto=na.omit(Auto)
names(Auto)
attach(Auto)
mpg1=rep(1,length(mpg))
mpg1[mpg<median(mpg)]=0
Auto = data.frame(Auto,mpg1)
pairs(Auto)
cor(Auto[, -9])

boxplot(weight ~ mpg1, data = Auto, main = "Weight vs mpg01")
boxplot(cylinders ~ mpg1, data = Auto, main = "Number of cylinders vs mpg01")
boxplot(displacement ~ mpg1, data = Auto, main = "displacement vs mpg01")
boxplot(horsepower ~ mpg1, data = Auto, main = "Horsepower vs mpg01")

# Random sample indexes
train_index <- sort(sample(1:nrow(Auto), 0.8 * nrow(Auto)))
test_index <- sort(setdiff(1:nrow(Auto), train_index))
X_train <- Auto[train_index, -10]
y_train <- Auto[train_index, 10]

X_test <- Auto[test_index, -10]
y_test <- Auto[test_index, 10]

#rm(mpg1.test)
library(MASS)
lda.fit=lda(mpg1 ~ cylinders + displacement +
  horsepower + weight, data = Auto,
        subset = train_index)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit,X_test)
names(lda.pred)
lda.class=lda.pred$class

table(lda.class,y_test)
lda.error = mean(lda.class != y_test)


#qda
qda.fit=qda(mpg1 ~ cylinders + displacement +
        horsepower + weight, data = Auto,
        subset = train_index)
qda.fit
qda.pred = predict(qda.fit,X_test)
names(qda.pred)
qda.class = qda.pred$class
table(qda.class,y_test)
qda.error = mean(qda.class != y_test)

#logistics regression
glm.fit=glm(mpg1 ~  displacement +
        horsepower + weight, data = Auto, subset = train_index, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,X_test,type="response")
glm.pred=rep(0,length(y_test))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred,y_test)
log.error = mean(glm.pred != y_test)

#KNN
#rm(list=ls())
library(class)
train.X = cbind(cylinders, displacement, horsepower, weight)[train_index, ]
test.X = cbind(cylinders, displacement, horsepower, weight)[test_index, ]
train.y=mpg1[train_index]
test.y=mpg1[test_index]
set.seed(1)
knn.pred=knn(train.X,test.X,train.y,k=10)
table(knn.pred,test.y)
mean(knn.pred != test.y)


###functions
Power <- function() {
   cat(2^3)
}
Power2 <- function(x,a) {
   cat(x^a)
}
Power3 <- function(x,a) {
   return(x^a)
}

PlotPower = function(x,a) {
  plot(x,Power3(x,a), xlab="x axis", ylab="y axis",
  main='y = x^a')
}
Power()
Power2(3,8)

x=1:10

plot(x,Power3(x,2), log = 'y',xlab="x axis", ylab="y axis",
  main='y = x^2')

PlotPower(1:10,8)

#Boston data - crime vs suburb
attach(Boston)
names(Boston)
pairs(Boston)
hist(crim)
crim1 = rep(0,length(crim))
crim1[crim > median(crim)] = 1
Boston = data.frame(Boston,crim1)
data = Boston
train_index <- sort(sample(1:nrow(data), 0.8 * nrow(data)))
test_index <- sort(setdiff(1:nrow(data), train_index))
X_train <- data[train_index, -15]
y_train <- data[train_index, 15]

X_test <- data[test_index, -15]
y_test <- data[test_index, 15]

cor(data[, -9])

library(MASS)
lda.fit=lda(crim1 ~ nox + indus +
  age + tax, data = data,
        subset = train_index)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit,X_test)
names(lda.pred)
lda.class=lda.pred$class

table(lda.class,y_test)
lda.error = mean(lda.class != y_test)

#logistics regression
glm.fit=glm(crim1 ~ nox + indus +
  age + tax, data = data, subset = train_index, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,X_test,type="response")
glm.pred=rep(0,length(y_test))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred,y_test)
log.error = mean(glm.pred != y_test)

#KNN
#rm(list=ls())
library(class)
train.X = cbind(nox, indus, age, tax)[train_index, ]
test.X = cbind(nox, indus, age, tax)[test_index, ]
train.y=crim1[train_index]
test.y=crim1[test_index]
set.seed(1)
knn.pred=knn(train.X,test.X,train.y,k=5)
table(knn.pred,test.y)
mean(knn.pred != test.y)
