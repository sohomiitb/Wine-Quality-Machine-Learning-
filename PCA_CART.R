library(clValid)
library(MASS)
library(rpart)
library(tree)
## Red wine Data input
wine=read.csv("C:/Users/Sohom Mondal/Dropbox/Statistics/statProject_wine quality/winequality-red.csv")
##training predictor wine data
tr_data=cbind(wine[1:1066,1:8],wine[1:1066,10:11])
##testing data
test_data=cbind(wine[-(1:1066),1:8],wine[-(1:1066),10:11])

## k-means clusternig for PH from testing data 
pH_data=matrix(wine[,9])
pH=pH_data[1:1066,]
set.seed(3)
k1=(kmeans(pH_data[1:1066,],3, nstart=50))
k2=matrix(k1$centers)

## Pricipal Component Analysis
tr_wpc<-princomp(tr_data,scores=T,cor=T) ## using correlation matrix
test_wpc<-princomp(test_data,scores=T,cor=T) ## using correlation matrix
## calculaing percentage of total Varience 
per_var_tr<-round((cumsum((tr_wpc$sdev)^2) / sum(tr_wpc$sdev^2)) * 100, digits = 2)
per_var_test<-round((cumsum((test_wpc$sdev)^2) / sum(test_wpc$sdev^2)) * 100, digits = 2)
## Plot of % of total Varience Vs Principle components
par(mfrow=c(2,1))
barplot(per_var_tr,main="Percentage of Total Varience Vs Principle component",xlab="Principal Component of Tranning data",ylab="Cumulative % of Explained Variance")
barplot(per_var_test,xlab="Principal Component of testing data", ylab="Cumulative % of Explained Variance")
da=data.frame(cbind(tr_wpc$scores[,1:8]))
da1=data.frame(cbind(test_wpc$scores[,1:8]))
## Regression Trees fiiting and predcition  
tree.fit <- tree(pH~da[,1]+da[,2]+da[,3]+da[,4]+da[,5]+da[,6]+da[,7]+da[,8],data=da)
plot(tree.fit)
text(tree.fit,cex=0.75)
tree.pred=predict(tree.fit,da1)
mean((tree.pred-da1)^2)
