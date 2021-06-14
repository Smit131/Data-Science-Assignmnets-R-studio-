
#Installing and loading the libraries
#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)

#book Book.Rating data
Book_rate_data <- read.csv("G:\\Data Science\\Assn 10 - Recommender System\\book (3).csv")

#metadata about the variable
str(Book_rate_data)


#Book.Rating distribution
hist(Book_rate_data$Book.Rating)
colnames(Book_rate_data)[1]<- "userId"
colnames(Book_rate_data)[3]<- "rating"

#the datatype should be real Book.Rating Matrix in order to build recommendation engine
Book_rate_data_matrix <- as(Book_rate_data, 'realRatingMatrix')
summary(Book_rate_data)


#Popularity based 
?Recommender
book_recomm_model1 <- Recommender(Book_rate_data_matrix, method="POPULAR")
summary(book_recomm_model1)


?Recommender
?`Recommender,ratingMatrix-method`


#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, Book_rate_data_matrix[103:104], n=2)
as(recommended_items1, "list")
?as

## Popularity model recommends the same books for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

book_recomm_model2 <- Recommender(Book_rate_data_matrix, method="UBCF")
summary(book_recomm_model2)
#Predictions for two users 
sum(is.na(Book_rate_data_matrix[1]))
recommended_items2 <- predict(book_recomm_model2, Book_rate_data_matrix[100:101],n=2)

?predict
as(recommended_items2, "list")
