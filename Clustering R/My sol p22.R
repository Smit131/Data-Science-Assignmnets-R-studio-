#my code clustering -> k means
#prob 2

inp1<- read.csv(file.choose())
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

windows()
plot(1:30,twss,type = "b",xlab = "number of clusters",ylab = "within group sum of square")
title(sub="K-means clustering screenplot")


#from the plot we can see good clusters can be formed at k = 5 is best

fit<-kmeans(mydata,5)
sum(fit$tot.withinss)
sum(kmeans(mydata,5)$tot.withinss)
str(fit)
fit$cluster
final2<-data.frame(fit$cluster,inp1)
write.csv(final2, file="final2crime.csv",row.names = F)

aggregate(final2[,-2],by = list(final2$fit.cluster),mean)


