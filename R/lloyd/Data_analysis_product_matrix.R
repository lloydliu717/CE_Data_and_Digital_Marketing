tb = read.csv("../step2data/product_placement_2015_AB_2.csv",header = T)
row.names(tb) = tb$X
tb$X = NULL

library(ggplot2)
library(pheatmap)
#######
tb[tb>0] = 1

product_counts = colSums(tb)

tb = tb[,which(product_counts > 1000)]

XX = cor(tb)

pheatmap(XX)
