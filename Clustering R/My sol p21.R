#clustering prob2


#to find Optimum number of clusters


inp<- read.csv(file.choose())
ninp<-scale(inp[,-1])

d<-dist(ninp,method="euclidean")
fit<-hclust(d,method="complete")

str(fit)

fit$order
fit$height
plot(fit,hang=-1)

rect.hclust(fit,k=9,border="blue")

rect.hclust(fit,k=6,border="red")



#from the dendogram we can see it is difficult todraw any inference on the data

groups<- cutree(fit,k=6)
#groups<- cutree(fit,k=350)

membership<-as.matrix(groups) # groups or cluster numbers
finalcrime <- data.frame(membership,inp)

View(finalcrime)

write.csv(finalcrime, file="finalcrime.csv",row.names = F)
getwd()

#now to get the mean values of the parameters in all the groups 
aggregate(finalcrime[,-c(1,2)],by = list(finalcrime$membership),mean)

