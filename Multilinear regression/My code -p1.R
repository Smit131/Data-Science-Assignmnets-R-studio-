# MultiLinear Reg prob 1
st<- read.csv(file.choose())
#install.packages('mlr')
# Package to create dummy variables instantly
library('mlr')
stfinale<-createDummyFeatures(st, cols = "State")
stf<-stfinale[,-4]

#install.packages("GGally")
library("GGally")
#install.packages("stringi")
cor(stf)
ggpairs(stf)

#install.packages("corpcor")
library('corpcor')
cor2pcor(cor(stf))
summary(stf)
#Marketng and R.D. have near to strong corelation 
attach(stf)
model.stf <- lm(Profit~.,data=stfinale)
summary(model.stf)
#here we can see, which variable to eliminate if its convinient 
model.stf2 <- lm(Profit~ stfinale$R.D.Spend+stfinale$Administration+stfinale$Marketing,data=stfinale)
summary(model.stf2)

library(car)
vif(model.stf)
vif(model.stf2)
# gives degree of collinearity between the variables
#coolinearity values are low thus no need to delete any variable
avPlots(model.stf)
# which variable is contributing least towards prediction
#thus no one has plot with zero slope....thus no need to delete.
#model is great in itself.

influence.measures(model.stf)
influenceIndexPlot(model.stf,id.n=3)
influencePlot(model.stf,id.n=3)
stfin<-stfinale[-c(50,49),]


model.stf3 <- lm(Profit~., data=stfin)
summary(model.stf3)
sqrt(mean(model.stf3$residuals**2))

model.stf3_1 <- lm (Profit ~ sqrt(R.D.Spend)+ sqrt(Administration)+sqrt(Marketing.Spend) + sqrt(State.California)
                      +sqrt(State.Florida)+ sqrt(State.New.York),data=stfin)
summary(model.stf3_1)
sqrt(mean(model.stf3_1$residuals**2))

model.stf3_2 <- lm (Profit ~ (R.D.Spend)**2 + (Administration)**2 + (Marketing.Spend)**2 + (State.California)**2
                   +(State.Florida)**2 + sqrt(State.New.York)**2,data=stfin)
summary(model.stf3_2)
sqrt(mean(model.stf3_2$residuals**2))


#model.stf33 <- lm (Profit ~ log(R.D.Spend)+ log(Administration)+log(Marketing.Spend) + log(State.California)+log(State.Florida)+ (State.New.York),data=stfin)
#there are zeroes present in the data thus log can't be applied

model.stf3_3 <- lm (log(Profit) ~ .,data=stfin)
summary(model.stf3_3)
model.stf3_3_pred <- (exp(model.stf3_3$fitted.values))
model.stf3_3_err<- stfin$Profit - model.stf3_3_pred
model.stf3_3_rmse <- sqrt(mean(model.stf3_3_err^2))
model.stf3_3_rmse

model.stf34 <- lm (Profit ~ 1/(R.D.Spend)+ 1/(Administration)+ 1/(Marketing.Spend) + 1/(State.California)
                   +1/(State.Florida)+ 1/(State.New.York),data=stfin)
summary(model.stf34)

