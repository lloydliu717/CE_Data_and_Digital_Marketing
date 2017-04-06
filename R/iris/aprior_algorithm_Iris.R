library(arules)
library(arulesViz)


setwd("/Users/irisapo/Documents/Boston/Courses/ma676/JSM/diary15/")

## take the log transformation for visuzlization


expd = readRDS("rds/expd.rds")
count.X = table(expd$NEWID, expd$UCC) 

# count.X = read.csv("../diary15/rds/countX.csv")
sample.X = (count.X > 0 ) *1   # set to be binary data

XX = t(sample.X) %*% sample.X  # item by item 
library(pheatmap)

Bi.X = as.data.frame(XX)


#Bi.X = as.data.frame(sample.X)

pheatmap(Bi.X, cluster_cols = T, cluster_rows = T)

pheatmap(Bi.X, cluster_cols = T, cluster_rows = T, kmeans_k = 3)



# 

for(i in colnames(Bi.X)){
  Bi.X[,i] <- as.factor(Bi.X[,i])
}



colSums(sample.ucc)

X.trans <- as(sample.ucc, "transactions")

rules = apriori(X.trans, parameter = list(support=0.05, confidence=0.8) )

rules    # 2014019 rules 
str(rules)

inspect(head(sort(rules, by="lift"),10))  # show 10 rules 


rules <- sort(rules, by="confidence", decreasing=TRUE)


# deliminate repeated relus    But can be too slow !!!
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]



###  
raed.ucc = read.csv("/Users/irisapo/Downloads/countX_3.23.csv", skip = 1)





