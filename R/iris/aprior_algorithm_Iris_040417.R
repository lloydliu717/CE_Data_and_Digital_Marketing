library(arules)
library(arulesViz)



## code from Lloyd : 
product = read.csv("/Users/Irisapo/Downloads/lloyd/product_placement_2015_AB_2.csv",header = T)
rownames(product) = product$X
product$X = NULL
### set to be binary data
product[product>0] = 1   

sample.X <- as.matrix(product)   # transform to be matrix 
  

XX = t(sample.X) %*% sample.X  # item by item 

## log-transformation is just for heatmap 
XX.log = log(XX+1)

Bi.X.log <- as.data.frame(XX.log)


library(pheatmap)

pheatmap(Bi.X.log)

## heatmap is done 




##  start apriori algorithm 
Bi.X = as.data.frame(XX)


for(i in colnames(Bi.X)){     # 1,0 need to be facctor 
  Bi.X[,i] <- as.factor(Bi.X[,i])
}

sample.ucc <- Bi.X

X.trans <- as(sample.ucc, "transactions")    # transform to "transactions" object for running apriori algorithm

rules = apriori(X.trans, parameter = list(support=0.1, confidence=0.83) )  # get the rules from apriori algorithm

rules   # display the number of rules we get 
str(rules)   # for fun ....

inspect(head(sort(rules, by="lift"),10))  # show 10 rules (sorted first)


rules <- sort(rules, by="confidence", decreasing=TRUE)   # sort rules by confidence(high relationship among products)




# deliminate repeated relus   But can be too slow !!!  # this part has never run successfully on my laptop because of the RAM. Have a try.
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]








