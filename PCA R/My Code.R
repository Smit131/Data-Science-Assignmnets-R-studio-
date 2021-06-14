inp<-read.csv(file.choose())
dat<-inp[,-1]
attach(inp)
x<-cor(inp)

pcaObj<- princomp(dat,cor=TRUE,scores = TRUE,covmat = NULL)

str(pcaObj)
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
pcaObj$loadings

#following is done to resolve the error->Error in plot.new() : figure margins too large
par("mar")
par(mar=c(1,1,1,1))

windows()
plot(pcaObj)

windows()
plot(pcaObj$scores[,1],pcaObj$scores[,4])

cor(pcaObj$scores)


inp1<-cbind(dat,pcaObj$scores[,1:4])
clus_dat<-inp1[,14:17]

norm_clus<-scale(clus_dat)

#to determine clusters we will use the kselection
library(kselection)
k<-kselection(norm_clus,parallel = TRUE,k_threshold = 0.95,max_centers = 20)
k

#lets use the graphical Method to determine the clusters better

twss=NULL
for (i in 1:30)
{
  twss[i]=sum(kmeans(norm_clus,i)$tot.withinss)
  
}

windows()
plot(1:30,twss,type = "b",xlab = "number of clusters",ylab = "within group sum of square")
title(sub="K-means clustering screenplot")
#the first elbow is at 5 and next is at 8 
#thus we will go for k =5

d<-dist(norm_clus,method="euclidean")
fit<-hclust(d,method="complete")

windows()
plot(fit,hang=-1)
rect.hclust(fit,k=5,border="red")
rect.hclust(fit,k=8,border="blue")
#from the dedogram we can see the clusters are better for value of 8 than 5

#using Heirarchical method to find clusters
groups<- cutree(fit,k=8)

member<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(member,dat)

View(final)

write.csv(final, file="finale.csv",row.names = F)
getwd()
setwd("C:/MY THING$/Data Science/Assn 8 - PCA")

aggregate(final[,-1],by = list(final$member),mean)

#Non herachical clusters

fit1<-kmeans(norm_clus,8)
final2<- data.frame(fit1$cluster,dat)
write.csv(final2, file="final2.csv",row.names = F)
aggregate(final[,-1],by = list(final$member),mean)
