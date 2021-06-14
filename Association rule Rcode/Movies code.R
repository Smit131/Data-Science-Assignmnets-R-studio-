my_mov<-read.csv(file.choose())
mov<-as.matrix(my_mov)

library(arules)
# the below function CONVERTS DUMMY VARIABLE TABLE INTO A  TRANSACTION ...THANK GOD
mov_tran <- as(mov, "transactions")
inspect(mov_tran)
class(mov_tran)


# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(mov_tran,topN=20)
mov_tran_rules<-apriori(mov_tran,parameter = list(support = 0.002,confidence = 0.05,minlen=3))

library(arulesViz)


mov_tran_rules<-apriori(mov_tran,parameter = list(support = 0.09,confidence = 0.05,minlen=4))

plot(mov_tran_rules,method = "scatterplot")
plot(mov_tran_rules,method = "grouped")
plot(mov_tran_rules,method = "graph")
#plot(mov_tran_rules[1],method = "mosaic")