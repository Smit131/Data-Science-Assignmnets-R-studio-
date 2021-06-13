#my code clustering -> k means

library("xlsx")
inp1<- read.xlsx(file.choose(),1)
mydata<-scale(inp1[,-1] )

#install.packages("kselection")
library(kselection)
k<-kselection(mydata,parallel = TRUE,k_threshold = 0.95,max_centers = 30)
k

twss=NULL
for (i in 1:30)
{
  twss[i]=sum(kmeans(mydata,i)$tot.withinss)
  
}
warning()
windows()
plot(1:30,twss,type = "b",xlab = "number of clusters",ylab = "within group sum of square")
title(sub="K-means clustering screenplot")

setwd("C:/MY THING$/Data Science/Assignment 7 - Clustering")

#from the plot we can see good clusters can be formed at k = 4 and  k=6

fit<-kmeans(mydata,4)
sum(fit$tot.withinss)
sum(kmeans(mydata,4)$tot.withinss)
str(fit)
fit$cluster
final2<-data.frame(fit$cluster,inp1)
write.csv(final2, file="final2.csv",row.names = F)
aggregate(final2[,-1],by = list(final2$fit.cluster),mean)

fit1<-kmeans(mydata,6)
final3<- data.frame(fit1$cluster,inp1)
write.csv(final3, file="final3.csv",row.names = F)
aggregate(final3[,-1],by = list(final3$fit1.cluster),mean)



