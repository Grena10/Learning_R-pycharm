library(ISLR)
Auto=read.table ("Auto.data",header=T,na.strings="?")
Auto=na.omit(Auto)
attach (Auto)
set.seed(2)
train=sample(392,196)
lm.fit =lm(mpg ~ horsepower,data=Auto, subset = train )
mean ((mpg - predict (lm.fit ,Auto))[- train ]^2)
lm.fit2 = lm(mpg ~ poly(horsepower ,2) ,data=Auto , subset=train)
mean((mpg - predict (lm.fit2 ,Auto ))[- train ]^2)
lm.fit3 = lm(mpg ~ poly(horsepower ,3) ,data=Auto , subset=train)
mean((mpg - predict (lm.fit3, Auto ))[- train ]^2) 
glm.fit =glm(mpg ~ horsepower, data= Auto)
coef(glm.fit)
lm.fit =lm(mpg ~ horsepower, data= Auto)
coef(lm.fit)

#Leave one out cross validation

library(boot)
glm.fit =glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto,glm.fit)
cv.err$delta # cross-validation estimate for the test error.


cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
plot(cv.error, type="l",col = "blue", 
     lwd = 2,
     main = "MSE calculated by LOOCV",
     xlab = "polynom",
     ylab = "MSE")


set.seed(17)
cv.error.10 = rep(0 ,10)
for (i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower,i),data = Auto)
  cv.error.10[i]= cv.glm (Auto ,glm.fit, K=10)$delta[1]
}
cv.error.10

plot(cv.error, type="l",col = "red", 
     lwd = 2,
     main = "MSE calculated by k-Fold",
     xlab = "polynom",
     ylab = "MSE")


LoadLibraries <- function() {
  library (ISLR)
  library (MASS)
  print("libraries loaded")
}
LoadLibraries()

library(ISLR)
alfa.fn = function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alfa.fn(Portfolio,1:100)

set.seed(1)
alfa.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio, alfa.fn ,R =1000)

data(Portfolio)
fix(Portfolio)

boot.fn=function(data,index){
  return(coef(lm(mpg~horsepower, data=data, subset = index)))
}

boot.fn2=function(data,index){
  return(coef(lm(mpg~horsepower + I(horsepower^2)
                , data=data, subset = index)))
}

boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto ,sample (392 ,392 , replace =T))

boot.fn(Auto ,sample (392 ,392 , replace =T))

boot(Auto,boot.fn,1000)

boot(Auto,boot.fn2,1000)


summary(lm(mpg ~ horsepower +I(horsepower^2) ,data= Auto))$coef

boot_prob=function(n){
  plot(n,(1-(1-1/n)^n),type="l")
}

boot_prob(1:10000)

store=rep(NA, 10000)

for (i in 1:10000) {
  store [i]= sum(sample(1:100, rep = TRUE) == 4)>0
}

mean(store)

x <- 1:10
sample(x[x >  8]) # length 2
sample(x[x >  9]) # oops -- length 10!
sample(x[x > 10]) # length 0

a=sample(1:10, rep = TRUE)
a==4


# Clear packages
#detach("package:Auto", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L