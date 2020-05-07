library(ISLR)
attach(Default)
set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family=binomial)
summary(glm.fit)
names(Default)
data=Default
y_test_col = 1

train_index <- sort(sample(1:nrow(data), 0.5 * nrow(data)))
test_index <- sort(setdiff(1:nrow(data), train_index))

X_train <- data[train_index, -y_test_col]
y_train <- data[train_index, y_test_col]

X_test <- data[test_index, -y_test_col]
y_test <- data[test_index, y_test_col]

glm.fit = glm(default ~ income + balance, data = Default, family=binomial,
              subset = train_index)
summary(glm.fit)
glm.probs=predict(glm.fit,X_test,type="response")
glm.pred=rep(0,length(y_test))
glm.pred[glm.probs > 0.5] = 1
tbl=table(glm.pred,y_test)
(tbl[1,2]+tbl[2,1])/sum(tbl)
a = mean(glm.pred != data$default[test_index])#don't know why a=1?


boot.fn=function(data,index){
  return(coef(glm(default ~ income + balance, data=data,
                 family=binomial, subset = index)))
}
library(boot)
boot.fn(data,1:5000)
#boot strap method
boot(data,boot.fn,R =1000)

#logistics regression

library(ISLR)
names(Weekly)
summary(Weekly)
attach(Weekly)
data = Weekly
train_index <- (2:length(Weekly))
test_index <- 1
X_train <- data[train_index, -9]
y_train <- data[train_index, 9]

X_test <- data[test_index, -9]
y_test <- data[test_index, 9]

glm.fit=glm(Direction~Lag1 + Lag2, data = Weekly, family=binomial,
            subset = train_index)
summary(glm.fit)
glm.probs=predict(glm.fit,X_test,type="response")
glm.pred = 0
glm.pred[glm.probs > 0.5] = 1


n = dim(data)[1]
err <- rep(0,n)
for (i in 1:n){
  train_index <-  1:n
  train_index <- train_index[-i]
  test_index <- i
  X_train <- data[train_index, -9]
  y_train <- data[train_index, 9]
  
  X_test <- data[test_index, -9]
  y_test <- data[test_index, 9]
  glm.fit=glm(Direction~Lag1 + Lag2, data = Weekly, family=binomial,
              subset = train_index)
  summary(glm.fit)
  cat(i)
  glm.probs=predict(glm.fit,X_test,type="response")
  glm.pred = 0
  glm.pred[glm.probs > 0.5] = 1
  if (glm.pred == 0)
    err[i] <- 1
}
mean(err)

  
#cross-validation
library(boot)
set.seed(1)
x = rnorm(100)
y=x - 2*x^2 + rnorm(100)
plot(x,y)

set.seed(8)
x <-  rnorm(100)
y <- x - 2*x^2 + rnorm(100)
df <-  as.data.frame(cbind(x,y))

cv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(y ~ poly(x,i),data = df)
  cv.error[i] = cv.glm(df,glm.fit)$delta[1]
}
cv.error

#Boston library
library(MASS)
attach(Boston)
names(Boston)
dim_Boston <- dim(Boston)
pacman::p_load(pacman, psych) 
mean(medv)
sd(mean)
describe(medv)
n = dim(Boston)[1]
sd(medv)
sd(medv)/sqrt(n)
se(medv)
describe(medv)

index <- 1:10
df = Boston[index,]
hist(medv)
curve(dnorm(x, mean = mean(medv), sd = sd(medv))*4000,0,50,
      col = "red",  # Color of curve
      lwd = 2,          # Line width of 2 pixels
      add = T) 


boot.fn=function(data,index){
  df = data[index,]
  return(describe(df$medv)$se)
}

boot.mean.fn=function(data,index){
  df = data[index,]
  return(mean(df$medv))
}

boot.mean.fn(Boston,1:n)

k <- boot(Boston,boot.mean.fn,1000)
mean.m <- k$t0
?boot
sd(k$t)
se.m <- sd(k$t)

mean.m - 2*se.m
mean.m + 2*se.m

t.test(Boston$medv)
median(Boston$medv)

boot.med.fn=function(data,index){
  df = data[index,]
  return(median(df$medv))
}

boot.med.fn(Boston,1:n)
k <- boot(Boston,boot.med.fn,1000)
boot(Boston,boot.med.fn,1000)
quantile(medv, c(0.1))

boot.fn01=function(data,index){
  df = data[index,]
  #browser
  return (quantile(df$medv, c(0.1)))
}

boot(Boston,boot.fn01,1000)
