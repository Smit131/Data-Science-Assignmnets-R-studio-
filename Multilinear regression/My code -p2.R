# MultiLinear Reg prob 2
pc<- read.csv(file.choose())
ppc<-pc
ppc$cd<-as.numeric(factor(as.numeric(pc$cd)-1))
ppc$premium<-as.numeric(factor(as.numeric(pc$premium)-1))
ppc$multi<-as.numeric(factor(as.numeric(pc$multi)-1))

ppc <-ppc[,-1]


#install.packages("GGally")
library("GGally")
#install.packages("stringi")
cor(ppc)
ggpairs(ppc)

#install.packages("corpcor")
library('corpcor')
cor2pcor(cor(ppc))

#No strong corelation 
model.ppc <- lm(price~.,data=ppc)
summary(model.ppc)


#here we can see, which variable to eliminate if its convinient 
library(car)
vif(model.ppc)
# no model has high value so all are contributing 
# gives degree of collinearity between the variables
#coolinearity values are low thus no need to delete any variable
avPlots(model.ppc)
# gives the contribution rate -> decide by th value of slope

influence.measures(model.ppc)
influenceIndexPlot(model.ppc,id.n=3)
influencePlot(model.ppc,id.n=3)
ppcfin<-ppc[-c(1441,1701),]


model.ppcfin1 <- lm(price~., data=ppcfin)
summary(model.ppcfin1)
sqrt(mean(model.ppcfin1$residuals**2))

model.ppcfin2 <- lm (price ~ sqrt(speed)+ sqrt(hd)+sqrt(ram) + sqrt(screen)
                    +sqrt(cd)+ sqrt(multi)+sqrt(premium)+sqrt(ads)+sqrt(trend),data=ppcfin)
summary(model.ppcfin2)
sqrt(mean(model.ppcfin2$residuals**2))

model.ppcfin3 <- lm (price ~ (speed)**2+ (hd)**2+(ram)**2 + (screen)**2
                     +(cd)**2+ (multi)**2+(premium)**2+(ads)**2+(trend)**2,data=ppcfin)

summary(model.ppcfin3)
sqrt(mean(model.ppcfin3$residuals**2))


#there are zeroes present in the data thus log can't be applied

model.ppcfin4 <- lm (log(price) ~ .,data=ppcfin)
summary(model.ppcfin4)
model.ppcfin4_pred <- (exp(model.ppcfin4$fitted.values))
model.ppcfin4_err<- ppcfin$price - model.ppcfin4_pred
model.ppcfin4_rmse <- sqrt(mean(model.ppcfin4_err^2))
model.ppcfin4_rmse



