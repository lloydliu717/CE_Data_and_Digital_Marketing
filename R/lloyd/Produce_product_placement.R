setwd("/Users/liuzichun/Documents/BU/2017SPRING/MA676/GSS_Data_Challenge")

expen1 = read.csv("DATA/diary15/expd151.csv", header = T, comment.char = '#')
expen2 = read.csv("DATA/diary15/expd152.csv", header = T, comment.char = '#')
expen3 = read.csv("DATA/diary15/expd153.csv", header = T, comment.char = '#')
expen4 = read.csv("DATA/diary15/expd154.csv", header = T, comment.char = '#')

expen2015 = rbind(expen1,expen2,expen3,expen4)
max(expen2015$NEWID)

###### I found the NEWID is not same as the description in dictionary, digits problem


library(data.table)
library(tidyr)
library(dplyr)
tmp = as.data.table(expen2015[,c(1,6)])
require(stringi)
rm(expen1,expen2,expen3,expen4,expen2015)

tmp[,CUID := stri_sub(tmp$NEWID,from=1,to=-2)]
tmp[,Period := stri_sub(tmp$NEWID,from=-1,to=-1)]
tmp[,buy := 1]
tmp[,NEWID := NULL]

total = tmp[,list(buy = sum(buy)),by=list(CUID,UCC)]

id = unique(total$CUID)


category = as.factor(unique(tmp$UCC))

tb = as.data.frame(matrix(nrow = length(id), ncol = length(category)))


names(tb) = category

rownames(tb) = id


for(cuid in id){
  index = which(rownames(tb)==cuid)
  tmprow = subset(total, subset = CUID == cuid) %>% spread(UCC,buy)
  a = match(names(tb), names(tmprow))
  index1 = which(!is.na(a))
  tmprow = as.matrix(tmprow)
  for(j in index1){
    tb[index, j] = tmprow[,a[j]]
#    print(j)
  }
  print(index)
}

tb[is.na(tb)] = 0

tb = data.matrix(tb)
tb = as.data.frame(tb)

write.csv(tb,file = "product_placement_2015.csv")


