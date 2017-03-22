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

tmp = tmp[,list(buy = sum(buy)),by=list(CUID,UCC)]

id = unique(tmp$CUID)

category = as.factor(unique(tmp$UCC))
table1 = as.data.frame(matrix(nrow = 5785, ncol = 488))
names(table1) = category
rownames(table1) = id



for(cuid in id){
  index = which(rownames(table1)==cuid)
  tmprow = subset(tmp, subset = CUID == cuid) %>% spread(UCC,buy)
  a = match(names(table1), names(tmprow))
  index1 = which(!is.na(a))
  tmprow = as.matrix(tmprow)
  for(j in index1){
    table1[index, j] = tmprow[,a[j]]
    print(j)
  }
  print(index)
}

table1[is.na(table1)] = 0


## follow part do not work
ucc = readLines("DATA/csxdstub.txt")
ucc = as.data.frame(ucc)
ucc = as.character(ucc)
for(i in 1:nrow(ucc)){
  print(i)
  ucc$ucc[i] = as.character(ucc$ucc[i])
}

t = sub(pattern = "  ",replacement = ",",t)
t = as.character(ucc$ucc[1])
