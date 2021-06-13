#problem 1
#to find Optimum number of clusters
# Loading
library("xlsx")
inp<- read.xlsx(file.choose(),1)
ninp<-scale(inp[,2:12])

d<-dist(ninp,method="euclidean")
fit<-hclust(d,method="complete")

str(fit)

fit$order
fit$height
plot(fit,hang=-1)

rect.hclust(fit,k=10,border="red")

rect.hclust(fit,k=100,border="red")

rect.hclust(fit,k=350,border="red")

#from the dendogram we can see it is difficult todraw any inference on the data

groups<- cutree(fit,k=10)
#groups<- cutree(fit,k=350)

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(inp, membership)

View(final)

write.csv(final, file="final.csv",row.names = F)
getwd()

#now to get the mean values of the parameters in all the groups 
aggregate(final[,-1],by = list(final$membership),mean)

