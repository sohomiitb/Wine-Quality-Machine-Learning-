#lda method
rw=read.csv("winequality-red.csv",header=T,sep=";")
library(MASS)
rw0=scale(rw[,-12])
quality=rw[,12]
new.rw=cbind(rw0,quality)
rw.data=data.frame(new.rw)
cv.lda=function(v,formula,data,cl){
require(MASS)
grps=cut(1:nrow(data),v,labels=FALSE)[sample(1:nrow(data))]
pred=lapply(1:v,function(i,formula,data){
test=which(grps==i)
z=lda(formula,data=data[-test,])
predict(z,data[test,])
},formula,data)
ct = unlist(lapply(pred,function(pp)pp$class))
table(ct,cl[order(grps)])
}
tt=cv.lda(10,quality~.,rw.data,rw.data$quality)
tt
error=sum(tt[row(tt)!=col(tt)])/sum(tt)
error

####white wine
ww=read.csv("winequality-white.csv",header=T,sep=";")
ww0=scale(ww[,-12])
quality0=ww[,12]
new.ww=cbind(ww0,quality0)
ww.data=data.frame(new.ww)
tt0=cv.lda(10,quality0~.,ww.data,ww.data$quality0)
tt0
error0=sum(tt0[row(tt0)!=col(tt0)])/sum(tt0)
error0



#knn method
rw=read.csv("winequality-red.csv",header=T,sep=";")
library(class)
rw0=scale(rw[,-12])
quality=rw[,12]
new.rw=cbind(rw0,quality)
rw.data=data.frame(new.rw)
cv.knn = function(v,data,cl,k){
require(class)
set.seed(17)
r=cut(1:nrow(data),v,labels=FALSE)[sample(1:nrow(data))]
pred = lapply(1:v,function(i,data,cl,k){
test = which(r == i)
pcl = knn(data[-test,],data[test,],cl[-test],k=k)
},data,cl,k)
ct = unlist(pred)
table(ct,cl[order(r)])
 }
 tt1=cv.knn(10,rw.data,rw.data$quality,1)
sum(tt1[row(tt1) != col(tt1)]) / sum(tt1)

##### Finding the best k
cv.error=rep(0,20)
for (i in 1:20){
tti=cv.knn(10,rw.data,rw.data$quality,i)
cv.error[i]=sum(tti[row(tti) != col(tti)]) / sum(tti)
}
cv.error
p=1:20
plot(p,cv.error,type="b")

#### white wine
ww=read.csv("winequality-white.csv",header=T,sep=";")
ww0=scale(ww[,-12])
quality0=ww[,12]
new.ww=cbind(ww0,quality0)
ww.data=data.frame(new.ww)
tt2=cv.knn(10,ww.data,ww.data$quality0,1)
sum(tt2[row(tt2) != col(tt2)]) / sum(tt2)
cv.error0=rep(0,20)
for (i in 1:20){
tt2i=cv.knn(10,ww.data,ww.data$quality0,i)
cv.error0[i]=sum(tt2i[row(tt2i)!=col(tt2i)])/sum(tt2i)
}
cv.error0
p0=1:20
plot(p0,cv.error0,type="b")


######ridge regression (mulinomial logistic)
rw=read.csv("winequality-red.csv",header=T,sep=";")
x=model.matrix(quality~.,rw)[,-1]
y=rw$quality
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid,family="multinomial")
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test =(-train)
y.test=y[test]
cv.out=cv.glmnet(x[train,],y[train],alpha=0,family ="multinomial")
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
p.y=predict(ridge.mod,s=bestlam,newx=x[test,],type="class")
table(p.y,y.test)
mean(p.y==y.test)

####### white wine
ww=read.csv("winequality-white.csv",header=T,sep=";")
x0=model.matrix(quality~.,ww)[,-1]
y0=ww$quality
ridge.mod0=glmnet(x0,y0,alpha=0,lambda=grid,family="multinomial")
set.seed(1)
train=sample(1:nrow(x0),nrow(x0)/2)
test =(-train)
y0.test=y0[test]
cv.out0=cv.glmnet(x0[train,],y0[train],alpha=0,family ="multinomial")
plot(cv.out0)
bestlam0=cv.out0$lambda.min
bestlam0
ridge.pred0=predict(ridge.mod0,s=bestlam0,newx=x0[test,])
p.y0=predict(ridge.mod0,s=bestlam0,newx=x0[test,],type="class")
table(p.y0,y0.test)
mean(p.y0==y0.test)



