# MultiLinear Reg prob 2
tc<- read.csv(file.choose())

tcp <- tc[,c(3,4,7,9,13,14,16,17,18)]




#install.packages("GGally")
library("GGally")
#install.packages("stringi")
cor(tcp)
ggpairs(tcp)

#install.packages("corpcor")
library('corpcor')
cor2pcor(cor(tcp))

#No strong corelation 
model.tcp <- lm(Price~.,data=tcp)
summary(model.tcp)


#here we can see, which variable to eliminate if its convinient 
library(car)
vif(model.tcp)
# gives degree of collinearity between the variables
#coolinearity values are low thus no need to delete any variable
avPlots(model.tcp)
# which variable is contributing least towards prediction
#thus no one has plot with zero slope....thus no need to delete.
#model is great in itself.

influence.measures(model.tcp)
influenceIndexPlot(model.tcp,id.n=3)
influencePlot(model.tcp,id.n=3)
tcpfin<-tcp[-c(81,222),]


model.tcpfin1 <- lm(Price~., data=tcpfin)
summary(model.tcpfin1)
sqrt(mean(model.tcpfin1$residuals**2))

model.tcpfin2 <- lm (Price ~ sqrt(Age_08_04)+ sqrt(KM)+sqrt(HP) + sqrt(cc)
                     +sqrt(Doors)+ sqrt(Gears)+sqrt(Quarterly_Tax)+sqrt(Weight),data=tcpfin)
summary(model.tcpfin2)
sqrt(mean(model.tcpfin2$residuals**2))

model.tcpfin3 <- lm (Price ~ (Age_08_04)**2+ (KM)**2+(HP)**2 + (cc)**2
                     +(Doors)**2+ (Gears)**2+(Quarterly_Tax)**2+(Weight)**2,data=tcpfin)

summary(model.tcpfin3)
sqrt(mean(model.tcpfin3$residuals**2))


#there are zeroes present in the data thus log can't be applied

model.tcpfin4 <- lm (log(Price) ~ .,data=tcpfin)
summary(model.tcpfin4)
model.tcpfin4_pred <- (exp(model.tcpfin4$fitted.values))
model.tcpfin4_err<- tcpfin$Price - model.tcpfin4_pred
model.tcpfin4_rmse <- sqrt(mean(model.tcpfin4_err^2))
model.tcpfin4_rmse



