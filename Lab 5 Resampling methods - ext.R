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
# Clear packages
#detach("package:Auto", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L