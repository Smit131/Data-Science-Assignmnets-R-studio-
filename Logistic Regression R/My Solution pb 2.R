
myMatrix<-read.csv(file.choose())
mymm<-myMatrix

#converting affairs in into 1 or 0 where 1 is yes affair and no affair resp. 
mymm$affairs<- ifelse(mymm$affairs>0,1,0)

mymm$gender<-(factor(as.numeric(mymm$gender)-1))
mymm$children<-(factor(as.numeric(mymm$gender)-1))
mymm$affairs<-(factor(as.numeric(mymm$affairs)))

sum(is.na(mymm))

summary(mymm)
str(mymm)
attach(mymm)
logit <- glm(affairs~.,family="binomial",data=mymm)
summary(logit)

logit1 <- glm(affairs~.-gender-children-education-occupation ,family="binomial",data=mymm)
summary(logit1)

#model with least AIC is better model
#here improvised model gave better result so we will go with model logit1
prob <- predict(logit1,type=c("response"),mymm)
prob

#confusion mat
conf<-table(prob>0.5,mymm$affairs) 
table(mymm$affairs)
conf

#accuracy
Accuracy<-sum(diag(conf)/sum(conf))
Accuracy

#ROCR
#install.packages("ROCR")
library('ROCR')
rocrpred<-prediction(prob,mymm$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
rocr_cutoff<-round(rocr_cutoff,2)

#from the table we can see best accuracy is for cut off of 0.39
conf<-table(prob>0.39,mymm$affairs)

Accuracy<-sum(diag(conf)/sum(conf))
Accuracy