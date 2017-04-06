setwd("~/Documents/MSSP/SP2017/MA676/project/3:2017")

count.X = read.csv("grocery.csv", skip=1)
sample.X = (count.X > 0)*1 # set to be binary data
XX = t(sample.X) %*% sample.X # item by item
bi.X = as.data.frame(XX)


library(pheatmap)
pheatmap(bi.X, cluster_cols = T, cluster_rows = T)
pheatmap(bi.X, cluster_cols = T, cluster_rows = T, kmeans_k = 3)


View(bi.X)


## try bigger csv
count.X.2 = read.csv("countX_3.23.csv", skip=1)
sample.X.2 = (count.X.2 > 0)*1 # set to be binary data
XX.2 = t(sample.X.2) %*% sample.X.2 # item by item
bi.X.2 = as.data.frame(XX.2)

pheatmap(bi.X.2, cluster_cols = T, cluster_rows = T, kmeans_k = 3)


# log transforming [natural] for exp data

