# Title     : International Airline activities
# Objective : Analyse data using learned ML methods
# Created by: Elena Shulyak
# Created on: 5/1/2020
LoadLibraries <- function() {
  library (ISLR)
  library (MASS)
  print("libraries loaded")
}
LoadLibraries()
mydata = read.csv("data/international_airline_activity_opfltsseats.csv")
attach(mydata)
dim(mydata)
summary(mydata)
names(mydata)
fix(mydata)
dim(mydata)
mydata <- na.omit(mydata)
data = mydata
col.y = 13
col.x = 1
train_index <- sort(sample(1:nrow(data), 0.8 * nrow(data)))
test_index <- sort(setdiff(1:nrow(data), train_index))
X_train <- data[train_index, col.x]
y_train <- data[train_index, col.y]

X_test <- data[test_index, col.x]
y_test <- data[test_index, col.y]


lm.fit =lm(y_train ~ X_train,data = data)
summary(lm.fit)
confint(lm.fit)
predict(lm.fit,X_test,interval = "prediction")
#predict(lm.fit,data.frame(X_test=43900),interval = "confidence")
plot(X_train,y_train)
abline(lm.fit,lwd=2,col="red")

#classification
data = data.frame(mydata[Year>2014,])
detach(mydata)
attach(data)
names(data)
names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
asia = rep(0,length(data[,1]))
length(asia)
dim(data)
asia[Service_Region == "SE Asia" | Service_Region == "NE Asia"] = 1
data = data.frame(data,asia)


All_Flights_st = scale(All_Flights)
Max_Seats_st = scale(Max_Seats)
new_data = data.frame(All_Flights_st,Max_Seats_st,asia)

data = new_data
col.y = 3
col.x = -col.y
train_index = (Year<2018)
test_index = !train_index
X_train <- data[train_index, col.x]
y_train <- data[train_index, col.y]

X_test <- data[test_index, col.x]
y_test <- data[test_index, col.y]

library(MASS)
lda.fit=lda(asia ~ All_Flights_st + Max_Seats_st , data = data, subset = train_index)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit,X_test)
names(lda.pred)
lda.class=lda.pred$class

table(lda.class,y_test)
lda.error = mean(lda.class != y_test)

#logistics regression
glm.fit=glm(asia ~ All_Flights_st + Max_Seats_st, data = data, subset = train_index, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,X_test,type="response")
glm.pred=rep(0,length(y_test))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred,y_test)
log.error = mean(glm.pred != y_test)

#KNN
library(class)
train.X = cbind(All_Flights_st, Max_Seats_st)[train_index, ]
test.X = cbind(All_Flights_st, Max_Seats_st)[test_index, ]
train.y=asia[train_index]
test.y=asia[test_index]
set.seed(1)
knn.pred=knn(train.X,test.X,train.y,k=3)
table(knn.pred,test.y)
mean(knn.pred != test.y)