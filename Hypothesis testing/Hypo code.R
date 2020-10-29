#Hypothesis testing
cutlet<- read.csv(choose.files())
qqnorm(cutlet$Unit.A)
qqplot(cutlet$Unit.A)
#getting this error what des it means->
#Error in sort(y) : argument "y" is missing, with no default

install.packages("nortest")
library("nortest")
ad.test(cutlet$Unit.A)
ad.test(cutlet$Unit.B)


install.packages("BSDA")
library("BSDA")
var.test(cutlet$Unit.A,cutlet$Unit.B)
t.test(cutlet$Unit.A,cutlet$Unit.B,alternative = "two.sided",var.equal = TRUE,conf.level = 0.95)
?t.test


#Problem 2
tat<-read.csv(file.choose())
stack_tat <-stack(tat)

ad.test(stack_tat$values)

install.packages("car")
library("car")

#to check variances of the stacked data
leveneTest(stack_tat$values~stack_tat$ind,data=stack_tat)

#One way ANOVA test ->

anova_res <- aov(values~ind, data=stack_tat)
summary(anova_res)

#problem 3

library(readxl)
sales<- read_excel(file.choose())
sales2<-read_excel(file.choose())
attach(sales)
summary(sales)
table(Region,Males)
table(sales2$...1,sales2$East)
?table
chisq.test(sales$Region,sales$Males,sales$Females)



#problem 4

orders<- read.csv(file.choose())
attach(orders)
x<-as.vector(orders$Phillippines)
y<-as.vector(orders$Indonesia)
w<-as.vector(orders$Malta)
z<-as.vector(orders$India)
order_stack <- stack(orders)
combined_orders<-data.frame(x,y,w,z,stringsAsFactors=FALSE)
stacked_orders<-stack(combined_orders) 
table(stacked_orders)


chisq.test(table(stacked_orders))



#problem 5

percen<- read.csv(file.choose())
x<-as.vector(percen$Weekdays)
y<-as.vector(percen$Weekend)
combined_percen<-data.frame(x,y,stringsAsFactors=FALSE)
stacked_percen <- stack(combined_percen)
table(stacked_percen)
prop.test(x=c(287,233),n=c(400,400),conf.level = 0.95,correct = TRUE,alternative = "two.sided")
prop.test(x=c(287,233),n=c(400,400),conf.level = 0.95,correct = TRUE,alternative = "greater")
