bk<-read.csv(file.choose())
bk_1<-as.matrix(bk)

library(arules)
# the below function CONVERTS DUMMY VARIABLE TABLE INTO A  TRANSACTION ...THANK GOD
bk_tran <- as(bk_1, "transactions")
inspect(bk_tran)
class(bk_tran)


# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(bk_tran,topN=20)
bk_tran_rules<-apriori(bk_tran,parameter = list(support = 0.002,confidence = 0.05,minlen=3))

library(arulesViz)


#increase the support ; decrease the confidence  ; and increase the minlen to reduse th
bk_tran_rules<-apriori(bk_tran,parameter = list(support = 0.0835,confidence = 0.05,minlen=4))

plot(bk_tran_rules,method = "scatterplot")
plot(bk_tran_rules,method = "grouped")
plot(bk_tran_rules,method = "graph")
plot(bk_tran_rules,method = "mosaic")

