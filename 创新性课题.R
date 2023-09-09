library(ggfortify)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(GGally)
library(MASS)
library(lattice)
library(caret)
library(car)
library(scatterplot3d)
setwd('/Users/gaomengxiao/Desktop')
data=read.csv(file='data.csv',header=TRUE)
attach(data)
sum(data$state==0,na.rm = FALSE)
sum(data$state==1,na.rm = FALSE)
head(data)
cor(data)
hist(data$height)
#绘制scatterplot matrix
c1=data.frame(data[,-7])
str(c1)
c1$Group = 1
c1$Group = as.factor(c1$Group)
ggpairs(c1,ggplot2::aes(color = Group))
#连续性变量统计
subset1=subset(data,data$state==0)
summary(subset1)
subset2=subset(data,data$state==1)
summary(subset2)
summary(data)
#数据的标准化
data$height_scaled=scale(data$height)
data$angle_scaled=scale(data$angle)
data$weight_scaled=scale(data$weight)
data$cohesion_scaled=scale(data$cohesion)
data$friction_angle_scaled=scale(data$friction_angle)
data$pore_water_pressure_scaled=scale(data$pore_water_pressure)
#逻辑回归分析
log1=glm(data$state~data$height_scaled+data$angle_scaled+data$weight_scaled+data$cohesion_scaled
         +data$friction_angle_scaled+data$pore_water_pressure_scaled)
summary(log1)
anova(log1,test="Chisq")
vif(log1)
#stepwise regression
step_byf=function(x,y,F_in,F_out,alpha_in,alpha_out){
  ####数据处理
  x=as.matrix(x);y=as.matrix(y)
  p=ncol(x)
  n=length(y)
  #### step 1 ####
  fm0=lm(y~1)
  r=c()
  for (i in 1:p) {
    fm=lm(y~x[,i])
    r0=summary(fm)$r.squared
    r=c(r,r0)
  }
  pp=which.max(r)
  t=summary(lm(y~x[,pp]))$coefficients[2,3]
  p_use=c()
  if(abs(t)>=qf(alpha_in,F_in,n-1,lower.tail = F)){p_use=c(p_use,pp)}
  out=c()
  #### function #######
  use_function=function(x,in_index,out_index){
    p_use=in_index;out=out_index
    ppp=length(p_use)
    x_in=x[,p_use]
    cor=c()
    for (i in 1:p) {
      cor1=cor(lm(y~x_in)$residuals,lm(x[,i]~x_in)$residuals)
      cor=c(cor,cor1)
    }
    #####假定剔除的变量不会再进入模型
    no_use=c(p_use,out)
    cor[no_use]=0
    pp=which.max(abs(cor))
    
    #### t 统计量
    t=summary(lm(y~x[,c(p_use,pp)]))$coefficients[-1,3]
    #### 判断
    
    t_in=sqrt(qf(alpha_in,F_in,n-ppp-2,lower.tail = F))
    t_out=sqrt(qf(alpha_out,F_in,n-ppp-2,lower.tail = F))
    in_index=which(abs(t[1:ppp])>=t_out)
    out_index1=which(abs(t[1:ppp])<t_out)
    out_model=p_use[out_index1]
    out_index2=c()
    if(abs(t[ppp+1])>=t_in) 
    {p_use=c(p_use[in_index],pp)
    }else out_index2=pp
    return(list(use_index=p_use,nouse_index=c(out_index,out_model,out_index2)))
  }
  #### step 2 #####
  out_p=use_function(x,p_use,out)$nouse_index
  in_p=use_function(x,p_use,out)$use_index
  #### 循环 ######
  repeat{
    a=use_function(x,in_p,out_p)
    in_p=a$use_index
    out_p=a$nouse_index
    index=c(in_p,out_p)
    if(length(index)==p)break()
  }
  fm=lm(y~x[,in_p])
  return(fm)
}
fm0=step_byf(data[8:13],data$state,1,1,0.05,0.05)
summary(fm0)
vif(fm0)
anova(fm0,test="Chisq")
#直观图像
colours<-c("#F08080","#000080")
colours<-colours[as.factor(data$state)]
plot(data$height_scaled~data$pore_water_pressure_scaled,col=colours,pch=20)
plot(data$height,col=colours,pch=20)
plot(data$pore_water_pressure,col=colours,pch=20)
#逻辑回归分析的错判率
data1=data.frame(data$height_scaled,data$angle_scaled,data$weight_scaled,data$cohesion_scaled,data$friction_angle_scaled,data$pore_water_pressure_scaled,data$state)
head(data1)
folds <- createFolds(y=data1$data.state,k=5)
length(folds)
for(i in 1:5){
  train_cv <- data1[-folds[[i]],]
  test_cv <- data1[folds[[i]],]
  pre <- glm(data.state~data.height_scaled+data.angle_scaled+data.weight_scaled+data.cohesion_scaled
             +data.friction_angle_scaled+data.pore_water_pressure_scaled,family=binomial(link = "logit"),data = train_cv)
  real <- test_cv$data.state
  predict.pre <- predict(pre,type='response',newdata=test_cv)
  predict =ifelse(predict.pre>0.5,1,0)
  test_cv$predict = predict
  res <- data.frame(real,predict)
  n = nrow(test_cv) 
  true_value=as.numeric(test_cv[,"data.state"])
  predict_value=as.numeric(test_cv[,"predict"])
  tablecv=table(true_value,predict_value)
  print(tablecv)}   
#混淆矩阵可视化
plot_confusion=function(cm){
  as.table(cm) %>%
    as_tibble() %>%
    mutate(pred=factor(pred),
           true=factor(true,rev(levels(pred)))) %>%
    ggplot(aes(pred,true,fill=n))+geom_tile()+geom_text(aes(label=n))+
    scale_fill_gradientn(colors=rev(hcl.colors(10,"Blues")),breaks=seq(0,10,2))+
    coord_fixed()+theme_minimal() 
}
cells <- c(36,50,26,94)
rnames <- c("0", "1")
cnames <- c("0", "1")
tabcv3<- matrix(data = cells, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(true=rnames, pred=cnames))
print(tabcv3)
cm3=confusionMatrix(tabcv3)
plot_confusion(cm3)
#逐步回归分析的错判率
data2=data.frame(data$height_scaled,data$pore_water_pressure_scaled,data$state)
head(data2)
folds <- createFolds(y=data1$data.state,k=5)
length(folds)
for(i in 1:5){
  train_cv <- data2[-folds[[i]],]
  test_cv <- data2[folds[[i]],]
  pre <- glm(data.state~data.height_scaled+data.pore_water_pressure_scaled,family=binomial(link = "logit"),data = train_cv)
  real <- test_cv$data.state
  predict.pre <- predict(pre,type='response',newdata=test_cv)
  predict =ifelse(predict.pre>0.5,1,0)
  test_cv$predict = predict
  res <- data.frame(real,predict)
  n = nrow(test_cv) 
  true_value=as.numeric(test_cv[,"data.state"])
  predict_value=as.numeric(test_cv[,"predict"])
  tablecv=table(true_value,predict_value)
  print(tablecv)} 
cells <- c(35,51,31,89)
rnames <- c("0", "1")
cnames <- c("0", "1")
tabcv2<- matrix(data = cells, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(true=rnames, pred=cnames))
print(tabcv2)
cm2=confusionMatrix(tabcv2)
plot_confusion(cm2)
#主成分分析
#手动计算结果与使用prcomp函数结果相差负号，实际无影响
n<-nrow(data)
data3=as.matrix(data[8:13])
print(data3)
mx<-diag(1,n)-matrix(1,n,n)/n
covA<-t(data3)%*%mx%*%data3/(n-1);covA 
spec<-eigen(covA);spec
spec.values=spec$values
component.num<-c(1,2,3,4,5,6)
datapca=data.frame(spec.values,component.num)
ggplot(datapca,aes(component.num,spec.values))+geom_point(col="#458B74",size=2)+geom_line(aes(component.num,spec.values))
data.pca1<-prcomp(data[1:6],scale=T,rank=2,retx=T) 
data.pca2<-prcomp(data[1:6],scale=T,rank=3,retx=T)


#分类效果图像
data$state=as.factor(data$state)
autoplot(data.pca1,data = data[7:13],col= 'state',size=4,
         loadings =T,loadings.label = TRUE,
         frame = TRUE,frame.type='norm',
         label = FALSE)+  theme_classic()
autoplot(data.pca2,data = data[7:13],col= 'state',size=4,
         loadings =T,loadings.label = TRUE,
         frame = TRUE,frame.type='norm',
         label = FALSE)+  theme_classic()
data4=data.frame(data.pca2$x)
plot(data.pca$x[,1]~data.pca$x[,2],col=colours,pch=20)
scatterplot3d(data4,pch=20,color=colours)
#错判率统计与混淆矩阵可视化
L1=lda(data$state~data.pca1$x, data, CV=TRUE)
tabcv1=table(pred=L1$class, true=data$state)
tabcv1
cm1=confusionMatrix(tabcv1)
plot_confusion(cm1)
L2=lda(data$state~data.pca2$x, data, CV=TRUE)
tabcv4=table(pred=L2$class, true=data$state)
tabcv4
cm4=confusionMatrix(tabcv4)
plot_confusion(cm4)
x1=mean(data.pca1$x[,1])
x2=mean(data.pca1$x[,2])
dis=array(data=NA,dim=206)
for(i in 1:206){
dis[i]=(data.pca1$x[i,1]-x1)*(data.pca1$x[i,1]-x1)+(data.pca1$x[i,2]-x2)*(data.pca1$x[i,2]-x2)}
plot(dis,col=colours,pch=20)
mean(dis)
subset1=subset(dis,data$state==0)
summary(subset1)
subset2=subset(dis,data$state==1)
summary(subset2)
sum(dis>2.9&data$state==0,na.rm = FALSE)
sum(dis<2.9&data$state==0,na.rm = FALSE)
sum(dis>2.9&data$state==1,na.rm = FALSE)
sum(dis<2.9&data$state==1,na.rm = FALSE)
cells <- c(60,26,36,84)
rnames <- c("0", "1")
cnames <- c("0", "1")
tabcv5<- matrix(data = cells, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(true=rnames, pred=cnames))
print(tabcv5)
cm5=confusionMatrix(tabcv5)
plot_confusion(cm5)
var1=tan(pi*data$friction_angle/180)/tan(pi*data$angle/180)
var2=data$weight*data$height*data$cohesion/(sin(pi*data$angle/180)*sin(pi*data$angle/180))
var3=data$pore_water_pressure
var4=tan(pi*data$friction_angle/180)*data$pore_water_pressure/tan(pi*data$angle/180)
print(var2[3])
for(i in 1:206){
  if(var2[i]>100000000)
    print(i)
}
plot(var2,col=colours,pch=20)
plot(var2_sub,col=colours,pch=20)
var1_sub=subset(var1,var2<300000)
var2_sub=subset(var2,var2<300000)
var3_sub=subset(data$pore_water_pressure,var2<300000)
data_sub=subset(data,var2<300000)
data6=data.frame(var1_sub,var2_sub,var3_sub,data_sub$state)
log3=glm(data_sub$state~var1_sub+var2_sub+var3_sub,family=binomial(link = "logit"),data=data6)
summary(log3)
data6=data.frame(var1_sub,var2_sub,var3_sub,data_sub$state)
folds <- createFolds(y=data6$data_sub.state,k=5)
length(folds)
for(i in 1:5){
  train_cv <- data6[-folds[[i]],]
  test_cv <- data6[folds[[i]],]
  pre <- glm(data_sub.state~var1_sub+var2_sub+var3_sub,family=binomial(link = "logit"),data = train_cv)
  real <- test_cv$data_sub.state
  predict.pre <- predict(pre,type='response',newdata=test_cv)
  predict =ifelse(predict.pre>0.5,1,0)
  test_cv$predict = predict
  res <- data.frame(real,predict)
  n = nrow(test_cv) 
  true_value=as.numeric(test_cv[,"data_sub.state"])
  predict_value=as.numeric(test_cv[,"predict"])
  tablecv=table(true_value,predict_value)
  print(tablecv)} 
cells <- c(42,16,21,43)
rnames <- c("0", "1")
cnames <- c("0", "1")
tabcv6<- matrix(data = cells, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(true=rnames, pred=cnames))
print(tabcv6)
cm6=confusionMatrix(tabcv6)
plot_confusion(cm6)