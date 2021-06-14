#KNN zoo prob

#KNN algos p2 
# predicting glass
# Read the dataset
zoo1 <- read.csv(file.choose())
View(zoo1)
#First colum in dataset is id which is not required so we will be taking out
zoo<-zoo1[-1]


table(zoo$type)

#factorizing the type
zoo$Type <- factor(zoo$type)

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(zoo$type))*100,1)

#Create a function to normalize the data
# norm <- function(x){ 
#   return((x-min(x))/(max(x)-min(x)))
# }

train_index <- sample(1:nrow(zoo), 0.7 * nrow(zoo))
test_index <- setdiff(1:nrow(zoo), train_index)

train<- zoo[train_index,-10]
train_label<-zoo[train_index,"Type"]

test<-zoo[test_index,-10]
test_label<-zoo[test_index,"Type"]



train_n<-train[1:17]
test_n<-test[1:17]




# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_zoo_pred <- knn(train=train_n,test=train_n,cl=train_label,k=i)
  train_acc <- c(train_acc,mean(train_zoo_pred==train_label))
  t_a<-c()
  test_zoo_pred <- knn(train = train_n, test = test_n, cl = train_label, k=i)
  test_acc <- c(test_acc,mean(test_zoo_pred==test_label))
}


# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
# Plotting 2 different graphs on same co-ordinate axis
#install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#from the graph max accuracy can be acheved at k = 4

zoo_pred_te <- knn(train = train_n, test = test_n, cl = train_label, k=4)
accuracy_test<- sum(zoo_pred_te==test_label)/31
zoo_pred_tr <- knn(train = train_n, test = train_n, cl = train_label, k=4)
accuracy_train<- sum(zoo_pred_tr==train_label)/70
accuracy_test
accuracy_train
