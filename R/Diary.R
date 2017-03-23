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

period1 = subset(tmp, subset = Period == 1)
period2 = subset(tmp, subset = Period == 2)

tmp = tmp[,list(buy = sum(buy)),by=list(CUID,UCC)]

tmp1 = period1[,list(buy = sum(buy)),by=list(CUID,UCC)]
tmp2 = period2[,list(buy = sum(buy)),by=list(CUID,UCC)]


id = unique(tmp$CUID)

id1 = unique(tmp1$CUID)
id2 = unique(tmp2$CUID)


category = as.factor(unique(tmp$UCC))

table1 = as.data.frame(matrix(nrow = length(id1), ncol = length(category)))
table2 = as.data.frame(matrix(nrow = length(id2), ncol = length(category)))


names(table1) = category
names(table2) = category

rownames(table1) = id1
rownames(table2) = id2


for(cuid in id2){
  index = which(rownames(table2)==cuid)
  tmprow = subset(tmp2, subset = CUID == cuid) %>% spread(UCC,buy)
  a = match(names(table2), names(tmprow))
  index1 = which(!is.na(a))
  tmprow = as.matrix(tmprow)
  for(j in index1){
    table2[index, j] = tmprow[,a[j]]
#    print(j)
  }
  print(index)
}

table2[is.na(table2)] = 0
table2 = data.matrix(table2)
table2 = as.data.frame(table2)


## follow part do not work
ucc = read.csv("DATA/ucc_existed.csv",stringsAsFactors = F)

category = as.numeric(as.character(category))
category1 = NULL
for(i in 1:length(category)){
  if(sum(ucc$UCC==category[i])==0){
    category1[i] = category[i]
  }
  else {
    category1[i] = ucc$PRODUCT[ucc$UCC==category[i]]
  }
  print(i)
}

names(table2) = category1
write.csv(table2,file = "product_placement_2015_period2.csv")
