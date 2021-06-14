#Association rule for data 1
#install.packages("arulesViz")
#install.packages("arules")

library(arules)
groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(groceries,topN=20)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))

library(arulesViz)

groceries_rules<-apriori(groceries,parameter = list(support = 0.006,confidence = 0.05,minlen=3))



plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")
