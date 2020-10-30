#Linear Regression 
#problem 1
cal<- read.csv(file.choose())
install.packages("lattice")
library("lattice")
cor(cal$Weight.gained..grams.,cal$Calories.Consumed)
sum(is.na(cal))
hist(cal$Weight.gained..grams.)
hist(cal$Calories.Consumed)

qqnorm(cal$Weight.gained..grams.)
qqline(cal$Weight.gained..grams.)

qqnorm(cal$Calories.Consumed)
qqline(cal$Calories.Consumed)

# both are normally distributed
y<-cal$Weight.gained..grams.
x<-cal$Calories.Consumed

plot(x,y)
cor(x,y) #0.946991

plot(sqrt(x),y)
cor(sqrt(x),y)

plot(x,sqrt(y))
cor(x,sqrt(y)) #0.9559736
cor(sqrt(x),sqrt(y))

plot(log(x),y)
cor(log(x),y)

plot(exp(x),y)
cor(exp(x),y)
cor(y,exp(x))

plot(1/x,y)
cor(1/x,y)
cor(x,1/y)
cor(1/x,1/y)

plot(x^2,y)
cor(x^2,y) #0.9710636
cor(x^3,y) #0.971167
cor(x^4,y) #0.9534202
cor(x^3,sqrt(y))

plot(x^1/3,y)
cor(x^1/3,y)

cor(x,sqrt(y))

cor(sqrt(x),y^2)


#Base Model

Modelb <- lm(y~x)
summary(Modelb)

Modelb_pred <- (Modelb$fitted.values)
Modelb_err <- y - Modelb_pred

RMSE_Modelb <- sqrt(mean(Modelb_err^2))
RMSE_Modelb


#Improved model test 1

Model1<- lm(y~x^3)
summary(Model1)

Model1_pred <- (Model1$fitted.values)
Model1_err <- y - Model1_pred

RMSE_Model1 <- sqrt(mean(Model1_err^2))
RMSE_Model1

#improved model test 2

Model2<- lm(sqrt(y)~x)
summary(Model2)

Model2_pred <- (Model2$fitted.values^2)
Model2_err <- y - Model2_pred

RMSE_Model2 <- sqrt(mean(Model2_err^2))
RMSE_Model2


#problem 2
dt <- read.csv(file.choose())

cor(dt$Delivery.Time,dt$Sorting.Time)
y<- dt$Delivery.Time
x<-dt$Sorting.Time
sum(is.na(dt))
hist(x)
hist(y)

qqnorm(x)
qqline(x)

qqnorm(y)
qqline(y)

plot(x,y)

cor(x,y)

cor(sqrt(x),y)  
cor(x,sqrt(y)) 
cor(sqrt(x),sqrt(y)) 

cor(log(x),y)
cor(x,log(y))
cor(log(x),log(y))

cor(exp(x),y)
cor(y,exp(x))
cor(exp(x),exp(y))

cor(1/x,y)
cor(x,1/y)
cor(1/x,1/y) 


cor(x^2,y)
cor(x,y^2)
cor(x^2,y^2)

cor(x^3,y) 
cor(x^4,y) 
cor(x^3,sqrt(y))


cor(x^1/3,y)
cor(x,y^1/3)
cor(x^1/3,y^1/3)

cor(x,sqrt(y))
cor(sqrt(x),y^2)

#Base Model

Modelb <- lm(y~x)
summary(Modelb)

Modelb_pred <- (Modelb$fitted.values)
Modelb_err <- y - Modelb_pred

RMSE_Modelb <- sqrt(mean(Modelb_err^2))
RMSE_Modelb


#Improved model test 1

Model1<- lm(log(y)~log(x))
summary(Model1)

Model1_pred <- (exp(Model1$fitted.values))
Model1_err <- y - Model1_pred

RMSE_Model1 <- sqrt(mean(Model1_err^2))
RMSE_Model1

#improved model test 2

Model2<- lm(1/y  ~  1/x)
summary(Model2)
plot(1/x,1/y)
plot(x,y)
Model2_pred <- (1/Model2$fitted.values)
Model2_err <- y - Model2_pred

RMSE_Model2 <- sqrt(mean(Model2_err^2))
RMSE_Model2

#improved model test 2

Model3<- lm(sqrt(y)  ~  sqrt(x))
summary(Model3)

Model3_pred <- (Model3$fitted.values^2)
Model3_err <- y - Model3_pred

RMSE_Model3 <- sqrt(mean(Model3_err^2))
RMSE_Model3


#problem 3

cr<-read.csv(file.choose())
x<- cr$Salary_hike
y<-cr$Churn_out_rate

sum(is.na(dt))
hist(x)
hist(y)

qqnorm(x)
qqline(x)

qqnorm(y)
qqline(y)

plot(x,y)

cor(x,y)

cor(sqrt(x),y)  
cor(x,sqrt(y)) 
cor(sqrt(x),sqrt(y)) 

cor(log(x),y)
cor(x,log(y))
cor(log(x),log(y))

cor(exp(x),y)
cor(y,exp(x))
cor(exp(x),exp(y))

cor(1/x,y)
cor(x,1/y)
cor(1/x,1/y) 


cor(x^2,y)
cor(x,y^2)
cor(x^2,y^2)

cor(x^3,y) 
cor(x^4,y) 
cor(x^3,sqrt(y))


cor(x^1/3,y)
cor(x,y^1/3)
cor(x^1/3,y^1/3)

cor(x,sqrt(y))
cor(sqrt(x),y^2)

#Base Model

Modelb <- lm(y~x)
summary(Modelb)

Modelb_pred <- (Modelb$fitted.values)
Modelb_err <- y - Modelb_pred

RMSE_Modelb <- sqrt(mean(Modelb_err^2))
RMSE_Modelb

#Base Model

Modelb <- lm(y~x)
summary(Modelb)

Modelb_pred <- (Modelb$fitted.values)
Modelb_err <- y - Modelb_pred

RMSE_Modelb <- sqrt(mean(Modelb_err^2))
RMSE_Modelb


#Improved model test 1

Model1<- lm(log(y)~log(x))
summary(Model1)

Model1_pred <- (exp(Model1$fitted.values))
Model1_err <- y - Model1_pred

RMSE_Model1 <- sqrt(mean(Model1_err^2))
RMSE_Model1

#improved model test 2

Model2<- lm(1/y  ~  x)
summary(Model2)

Model2_pred <- (1/Model2$fitted.values)
Model2_err <- y - Model2_pred

RMSE_Model2 <- sqrt(mean(Model2_err^2))
RMSE_Model2

#problem 4

sh<- read.csv(file.choose())
x<-sh$YearsExperience
y<-sh$Salary

sum(is.na(dt))
hist(x)
hist(y)

qqnorm(x)
qqline(x)

qqnorm(y)
qqline(y)

plot(x,y)

cor(x,y)

cor(sqrt(x),y)  
cor(x,sqrt(y)) 
cor(sqrt(x),sqrt(y)) 

cor(log(x),y)
cor(x,log(y))
cor(log(x),log(y))

cor(exp(x),y)
cor(y,exp(x))
cor(exp(x),exp(y))

cor(1/x,y)
cor(x,1/y)
cor(1/x,1/y) 


cor(x^2,y)
cor(x,y^2)
cor(x^2,y^2)

cor(x^3,y) 
cor(x^4,y) 
cor(x^3,sqrt(y))


cor(x^1/3,y)
cor(x,y^1/3)
cor(x^1/3,y^1/3)

cor(x,sqrt(y))
cor(sqrt(x),y^2)

#Base Model

Modelb <- lm(y~x)
summary(Modelb)

Modelb_pred <- (Modelb$fitted.values)
Modelb_err <- y - Modelb_pred

RMSE_Modelb <- sqrt(mean(Modelb_err^2))
RMSE_Modelb

#Improved model test 1

Model1<- lm(sqrt(y)~x)
summary(Model1)

Model1_pred <- (Model1$fitted.values)
Model1_err <- y - Model1_pred

RMSE_Model1 <- sqrt(mean(Model1_err^2))
RMSE_Model1

plot(x,y)