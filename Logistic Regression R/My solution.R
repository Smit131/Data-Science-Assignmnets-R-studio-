#logistic regression assignment 1

myMatrix <- read.table(file.choose(), sep=";",header=TRUE)
mym <- myMatrix
library('mlr')
sum(is.na(myMatrix))
mymm<-createDummyFeatures(mym, cols = c("job","marital","education","contact","month","poutcome"))


mymm$default<-(factor(as.numeric(mymm$default)-1))
mymm$housing<-(factor(as.numeric(mymm$housing)-1))
mymm$y<-(factor(as.numeric(mymm$y)-1))
mymm$loan<-(factor(as.numeric(mymm$loan)-1))

#making the new dummy variable in factor form using loop
for(i in 12:49)
{
  mymm[,i]<-factor(mymm[,i])
}


summary(mymm)
str(mymm)
attach(mymm)
logit <- glm(y~.,family="binomial",data=mymm)
summary(logit)

#null deviace must be higher than Residual deviance
#dropping the non sih=gnificant collumns

logit1 <- glm(y~.-default-pdays-previous-job.admin.-job.unknown-marital.divorced-job.blue.collar-job.entrepreneur-job.housemaid-job.management-job.self.employed-job.services-job.technician-job.unemployed-marital.single-education.secondary-education.tertiary-education.unknown-contact.unknown-month.dec-month.oct-month.sep-poutcome.failure-poutcome.unknown,family = "binomial",data=mymm )
#logit1 <- glm(y~.-pdays-default,family = "binomial",data=mymm )
summary(logit1)

#model with least AIC is better model
prob <- predict(logit,type=c("response"),mymm)
prob

#confusion mat
conf<-table(prob>0.5,mymm$y) 
table(mymm$y)
conf


#accuracy
Accuracy<-sum(diag(conf)/sum(conf))
Accuracy

#ROCR
#install.packages("ROCR")
library('ROCR')
rocrpred<-prediction(prob,mymm$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
rocr_cutoff<-round(rocr_cutoff,2)

#from the table we can see best accuracy is for cut off of 0.39
conf<-table(prob>0.39,mymm$y) 

Accuracy<-sum(diag(conf)/sum(conf))
Accuracy
