setwd("~/Documents/BU/2017SPRING/MA676/GSS_Data_Challenge/R")
library(data.table)
library(ggplot2)
library(stringi)
library(tidyr)
library(magrittr)
library(plyr)
################
##fmli in 2015
fm1 = read.csv("../DATA/intrvw15/intrvw15/fmli151x.csv",header = T,stringsAsFactors = F)
fm2 = read.csv("../DATA/intrvw15/intrvw15/fmli152.csv",header = T,stringsAsFactors = F)
fm3 = read.csv("../DATA/intrvw15/intrvw15/fmli153.csv",header = T,stringsAsFactors = F)
fm4 = read.csv("../DATA/intrvw15/intrvw15/fmli154.csv",header = T,stringsAsFactors = F)
fm5 = read.csv("../DATA/intrvw15/intrvw15/fmli161.csv",header = T,stringsAsFactors = F)

fm5$BIRTHYR = NULL
fm5$REFGEN = NULL

identical(names(fm1),names(fm2))

tmp = NULL
for(i in 1:length(names(fm1))){
  tmp = c(tmp,identical(names(fm1)[i],names(fm2)[i]))
}
which(tmp==F)#805 806 name is different

tttt = cbind(names(fm1),names(fm2),names(fm3),names(fm4),names(fm5))

names(fm1) = names(fm2)

rm(tmp,tttt)


fm = rbind(fm1,fm2,fm3,fm4,fm5)
var.names = names(fm)
CUID.2015 = unique(fm$CUID)

length(CUID.2015)##Total CU number in 2015




################
##NTAXI in 2015
tax1 = read.csv("../DATA/intrvw15/intrvw15/ntaxi151x.csv")
tax2 = read.csv("../DATA/intrvw15/intrvw15/ntaxi152.csv")
tax3 = read.csv("../DATA/intrvw15/intrvw15/ntaxi153.csv")
tax4 = read.csv("../DATA/intrvw15/intrvw15/ntaxi154.csv")
tax5 = read.csv("../DATA/intrvw15/intrvw15/ntaxi161.csv")

for(i in 1:5){
  text.tmp = paste("newid",i,sep = "")
  text.tmp = paste(text.tmp," = tax",i,"$NEWID",sep = "")
  eval(parse(text = text.tmp))
}
id777 =  c(newid1,newid2,newid3,newid4,newid5)
require(stringi)
id777 = as.integer(stri_sub(id777,from=1,to=-2))
id777 = unique(id777)
identical(CUID.2015,id777)
#####TRUE Thus, I make sure that the CU are fixed within one year's data
rm(i,id777,newid1,newid2,newid3,newid4,newid5,tmp,text.tmp)
rm(tax1,tax2,tax3,tax4,tax5)


tmp = fm$INTERI
table(tmp)
which(fm$INTERI == 1)[1]
fm$CUID[5000]
person1 = subset(fm,subset = CUID == fm$CUID[which(fm$INTERI == 1)[1]])
fm = as.data.table(fm)
fm.unique = fm[,list(INTERI = first(INTERI),
                     N=.N,
                     AGE_REF = median(AGE_REF),
                     FAM_SIZE = mean(FAM_SIZE),
                     BLS_URBN = mean(BLS_URBN),
                     NO.EARNR = mean(NO_EARNR),
                     NO.AUTO = mean(NUM_AUTO),
                     NO.VEH = mean(VEHQ),
                     PERS.LT18 = mean(PERSLT18),
                     PERS.OT64 = mean(PERSOT64),
                     INCome.cLASS = mean(INCLASS),
                     EDUC_REF = last(EDUC_REF),
                     HIGH_EDU = last(HIGH_EDU),
                     Income.bftax = mean(FINCBTAX),
                     TOTEXPCQ = mean(TOTEXPCQ),
                     TRANSCQ = mean(TRANSCQ),
                     STATE = first(STATE)
                     ),by=list(CUID)]


ggplot(fm.unique) + geom_histogram(aes(AGE_REF,fill=..count..),bins = 72) + ggtitle("Histogram on Age of reference person in CU in 2015")

ggplot(fm.unique) + geom_histogram(aes(FAM_SIZE, fill=..count..),bins = 21) + ggtitle("Histogram on Family Size of CU in 2015")
ggplot(fm.unique) + geom_bar(aes(BLS_URBN,fill = ..count..)) + ggtitle("Histogram on living place of CU in 2015")
ggplot(fm.unique) + geom_histogram(aes(NO.VEH,fill = ..count..),bins = 11) + ggtitle("Histogram of Vehicles in CU in 2015")
ggplot(fm.unique) + geom_point(aes(NO.AUTO,NO.VEH))
ggplot(fm.unique) + geom_histogram(aes(EDUC_REF,fill = ..count..),bins = 16) + ggtitle("Education level of reference person in CU in 2015")
ggplot(fm.unique) + geom_histogram(aes(HIGH_EDU,fill = ..count..),bins = 16) + ggtitle("Highest education level in CU in 2015")
ggplot(fm.unique) + geom_histogram(aes(Income.bftax,fill = ..count..),bins = 100) + ggtitle("Income in CU in 2015")
ggplot(fm.unique) + geom_histogram(aes(TOTEXPCQ,fill = ..count..),bins = 50) + ggtitle("Mean quaterly expenditures of CU in 2015")
ggplot(fm.unique) + geom_histogram(aes(TRANSCQ,fill = ..count..),bins = 50) + ggtitle("Mean quaterly transportation expenditures of CU in 2015")



#Where are the cars
veh = subset(fm.unique,subset = NO.VEH>0)

statelist = read.csv("statelist",stringsAsFactors = F,header = F)
names(statelist) = "V1"
statelist %<>% separate(V1, into = paste("V",1:4,sep = ""),sep = " ")
statelist$V2 = paste(statelist$V2,statelist$V3,statelist$V4,sep = " ")
statelist$V2 = gsub("NA","",statelist$V2)
statelist$V2 = gsub("  ","",statelist$V2)
statelist$V3 = NULL
statelist$V4 = NULL
statelist$V1 = as.integer(statelist$V1)
statelist$V2[which(statelist$V2=="Massachuse")] = "massachusetts"

statelist$V2 = tolower(statelist$V2)

for(i in 1: nrow(veh)){
  if(is.na(veh$STATE[i])){
    veh$STATE[i] = NA
  }
  else if(sum(statelist$V1 == veh$STATE[i])==0){
    veh$STATE[i] = NA
  } 
  else{
    k=which(statelist$V1 == veh$STATE[i])
    veh$STATE[i] = statelist$V2[k]
  }
  if(i %% 20 == 0) {
    if(i %% 100 == 0) {
      if(i %% 500 == 0) {
        cat("| ");
        if(i %% 1000 == 0) {
          cat(i/1000);
          cat("k");
        }
        cat("\n");
      } else cat(":")
    } else cat("·")
  }
}

veh = veh[-which(is.na(veh$STATE)),]

veh.tmp = veh[,list(N=.N),by=list(STATE)]

statemap = map_data("state")

statemap = merge(x = statemap,y = veh.tmp,by.x = "region",by.y = "STATE",all.x = T)

statemap = arrange(statemap,group,order)


ggplot(statemap,aes(x=long,y=lat,group=group)) + 
  geom_polygon(linetype = 1, size = 0.1, colour = "lightgrey",aes(fill = N)) + 
  expand_limits(x = statemap$long, y = statemap$lat) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  coord_map( "polyconic") + ggtitle("Distribution of CU which have vehicles in 2015")

unique(statemap$region)

veh$STATE

range(fm.unique$HIGH_EDU)
table(fm.unique$BLS_URBN)

person1$CARTKNPQ

#int1 = subset(fm,subset = INTERI == 1)


