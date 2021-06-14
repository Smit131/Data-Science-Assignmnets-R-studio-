#KNN algos p1 
# predicting glass
# Read the dataset
glass <- read.csv(file.choose())
View(glass)
#First colum in dataset is id which is not required so we will be taking out
#glass <- glass[-1]


table(glass$Type)

# Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M. We also replacing these two entery with Benign and Malignat
glass$Type <- factor(glass$Type)

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(glass$Type))*100,1)

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

#we need to take random data splitting because the given data has output in sequence which will prove -
#if we use sequential splitting
train_index <- sample(1:nrow(glass), 0.7 * nrow(glass))
test_index <- setdiff(1:nrow(glass), train_index)

train<- glass[train_index,-10]
train_label<-glass[train_index,"Type"]

test<-glass[test_index,-10]
test_label<-glass[test_index,"Type"]

#Apply the normalization function

train_n<-as.data.frame(lapply(train[1:9], norm))
test_n<-as.data.frame(lapply(test[1:9], norm))




# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_glass_pred <- knn(train=train_n,test=train_n,cl=train_label,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==train_label))
  t_a<-c()
  test_glass_pred <- knn(train = train_n, test = test_n, cl = train_label, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==test_label))
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

#from the graph max accuracy can be acheved at k = 50

glass_pred_te <- knn(train = train_n, test = test_n, cl = train_label, k=50)
accuracy_test<- sum(glass_pred_te==test_label)/65
glass_pred_tr <- knn(train = train_n, test = train_n, cl = train_label, k=50)
accuracy_train<- sum(glass_pred_tr==train_label)/149
accuracy_test
accuracy_train

# model seems to be underfitting this problem can only be solved if we have high quantity and quality of observation 
