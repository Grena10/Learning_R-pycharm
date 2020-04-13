# Title     : Loading Data
# Objective : Read Data file
# Created by: grena
# Created on: 4/13/2020

Auto=read.table ("Auto.data",header=T,na.strings="?")
fix(Auto)
dim(Auto)
Auto[1:4,4:9]
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
