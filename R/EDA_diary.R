
  setwd("/Users/liuzichun/Documents/BU/2017SPRING/MA676/GSS_Data_Challenge")


#

dtbd.1 = read.csv("DATA/diary15/dtbd151.csv")   # "NEWID" "UCC" ,     length(unique(NEWID) ):  2985
dtid.1 = read.csv("DATA/diary15/dtid151.csv")   # "NEWID" "UCC" ,     length(unique(NEWID) ):  2976<
expd.1 = read.csv("DATA/diary15/expd151.csv")   # "NEWID" "UCC" ,     length(unique(NEWID) ):  2725<
fmld.1 = read.csv("DATA/diary15/fmld151.csv")   # "NEWID" ----- ,     length(unique(NEWID) ):  2985
memd.1 = read.csv("DATA/diary15/memd151.csv")   # "NEWID" ----- ,     length(unique(NEWID) ):  2985


memd.1.part = memd.1[,100:168]

fmld.1.part.1 = fmld.1[,100:199]
fmld.1.part.2 = fmld.1[,200:299]
fmld.1.part.3 = fmld.1[,300:387]


dtbd.2 = read.csv("DATA/diary15/dtbd152.csv")   # "NEWID" "UCC" ,     length(unique(NEWID) ):  3009
dtbd.3 = read.csv("DATA/diary15/dtbd153.csv")   # "NEWID" "UCC" ,     length(unique(NEWID) ):  2920
dtbd.4 = read.csv("DATA/diary15/dtbd154.csv")   # "NEWID" "UCC" ,     length(unique(NEWID) ):  2927

library(data.table)
dtbd.12 = rbindlist(list(dtbd.1, dtbd.2))
dtbd.34 = rbindlist(list(dtbd.3, dtbd.4))
dtbd = rbindlist(list(dtbd.12, dtbd.34))
saveRDS(dtbd, "DATA/dtbd.rds")


# bind files 

expd.2 = read.csv("DATA/diary15/expd152.csv")   # "NEWID" "UCC" ,     length(unique(NEWID) ):  2760
expd.3 = read.csv("DATA/diary15/expd153.csv")   # "NEWID" "UCC" ,     length(unique(NEWID) ):  2645
expd.4 = read.csv("DATA/diary15/expd154.csv")   # "NEWID" "UCC" ,     length(unique(NEWID) ):  2645

length(unique(expd.4$NEWID))

expd.12 = rbindlist(list(expd.1, expd.2))
expd.34 = rbindlist(list(expd.3, expd.4))
expd = rbindlist(list(expd.12, expd.34))
saveRDS(expd, "DATA/expd.rds")



fmld.2 = read.csv("DATA/diary15/fmld152.csv")   # "NEWID" ----- ,     length(unique(NEWID) ):  3009
fmld.3 = read.csv("DATA/diary15/fmld153.csv")   # "NEWID" ----- ,     length(unique(NEWID) ):  2920
fmld.4 = read.csv("DATA/diary15/fmld154.csv")   # "NEWID" ----- ,     length(unique(NEWID) ):  2927

length(unique(fmld.4$NEWID))

fmld.12 = rbindlist(list(fmld.1, fmld.2))
fmld.34 = rbindlist(list(fmld.3, fmld.4))
fmld = rbindlist(list(fmld.12, fmld.34))
saveRDS(fmld, "DATA/fmld.rds")

memd.2 = read.csv("DATA/diary15/memd152.csv")   # "NEWID" ----- ,     length(unique(NEWID) ):  3009
memd.3 = read.csv("DATA/diary15/memd153.csv")   # "NEWID" ----- ,     length(unique(NEWID) ):  2920
memd.4 = read.csv("DATA/diary15/memd154.csv")   # "NEWID" ----- ,     length(unique(NEWID) ):  2927

length(unique(memd.4$NEWID))

memd.12 = rbindlist(list(memd.1, memd.2))
memd.34 = rbindlist(list(memd.3, memd.4))
memd = rbindlist(list(memd.12, memd.34))
saveRDS(memd, "DATA/memd.rds")


### EDA


expd = readRDS("DATA/expd.rds")
# expd file:  there is not any id has expediture info for all 12 months, but each id has at least expediture info for one month.

class(expd)
library(data.table)
expd.x = expd[,list(tCOST=sum(COST), nGIFT=sum(GIFT==1), N=.N, nMONTH=length(unique(EXPNMO)), nUCC=length(unique(UCC))),
              by=list(NEWID)]    #  length( unique(NEWID) ) : 10775
# total cost ,   # gift , #purchase, #month, #UCC,  for each id
expd.x$aveCost = expd.x$tCOST / expd.x$nMONTH    # monthly cost 

plot(density(expd.x$aveCost))   # [average monthly cost] is extremely right skewed, also boxplot can tell this. 
summary(expd.x$aveCost)

barplot(table(expd.x$nUCC))    # [#UCC] is also right skewed, 
# how about related to these two together : [average monthly cost] &&  [#UCC] 

expd.x.s = expd.x[expd.x$nUCC<80, ]
expd.x.l = expd.x[expd.x$nUCC>=80, ]
plot(density(expd.x.s$aveCost)) 
plot(density(expd.x.l$aveCost)) 

# seems like smaller [#UCC] cannot gaurantee the smaller [average monthly cost]

# fmld file:
fmld = readRDS("DATA/fmld.rds")

col.name = c("NEWID", "EARNCOMP", "NO_EARNR", "POPSIZE", "PSU","REF_RACE", "HIGH_EDU",
             "MARITAL1", "EDUC_REF", "FAM_SIZE",
             "EMPLTYP1","EMPLTYP2","AGE_REF","AGE2", "OCCEXPNX", "OTHRECX", "FINCBEFX", 
             "LUMPB","FJSSDEDM", "FPVTXM", "FRRXM", 
             "OCCULIS1","OCCULIS2","WHYNWRK1","WHYNWRK2", 
             "REC_FS","FS_MTHI", "FS_AMTXM", "FREEMLX","JGRCFDMV", "JGROCYMV",
             "DESCRIP", "TYPOWND", "VEHQ",
             "CHILDAGE",
             "FOODTOT","FOODHOME", "FOODAWAY", "ALCBEV", "SMOKSUPP", "PET_FOOD","PERSPROD","PERSSERV","DRUGSUPP","HOUSKEEP"
             )
class(fmld)

fmld.sub = fmld[,names(fmld) %in% col.name,with = F]

#write.csv(fmld.sub, "DATA/diary15/rds/fmld_sub.csv", na="NA", row.names = F)


fmld.sub$JGROCYMV = as.numeric( as.character(fmld.sub$JGROCYMV) )
fmld.sub$JGRCFDMV = as.numeric( as.character(fmld.sub$JGRCFDMV) )  #778 (out of 11841) is NA

ratio.GrocyFood = fmld.sub$JGROCYMV / fmld.sub$JGRCFDMV  # 1552 is NA, 19 is NaN. (out of 11841)
library(IDPmisc)
b = NaRV.omit(ratio.GrocyFood)
summary(b)
boxplot(log(b))
sum(log(b)>0.6404); sum(log(b)<=0.6404); 
barplot(table( log(b) ))

##  income vs. compositions of earners
  library(ggplot2)
  table(fmld.sub$EARNCOMP)
  ggplot(fmld.sub) + aes(x = factor(EARNCOMP), y=FINCBEFX ) + geom_boxplot()
  
  ggplot(fmld.sub) + aes(x = factor(EARNCOMP), y=sqrt(FINCBEFX) ) + geom_boxplot()
  
  # seems like: the more people working, the higher the median income. But the highest income is in the CU unit with (type 1)only the REF-per working or (type 2)REF-per and the spouse working 
    # type  in EARNCOMP
     #   1 Reference person only
     #   2 Reference person and spouse
     #   3 Reference person, spouse, and others
     #   4 Reference person and others
     #   5 Spouse only
     #   6 Spouse and others
     #   7 Others only
     #   8 No earners

##  income vs. family size
  ggplot(fmld.sub) + aes(x = factor(FAM_SIZE), y=FINCBEFX ) + geom_boxplot()
  # seems like: the median income goes up until the family size reaches 4, the highest income are also from the small family size(1, 2). 

## so then is there any relationship/pattern between (compositions of earners) and (family size)?
  table(fmld.sub$EARNCOMP, fmld.sub$FAM_SIZE)
  #  what should I do now ? for this 


## income vs. children age
  ggplot(fmld.sub) + aes(x = factor(CHILDAGE), y=FINCBEFX ) + geom_boxplot()
  # Only obvious difference is for CU units with NO child and the other having child(ren). 
  # CU units having no child are either having low meadian income or extremely high income 

## income vs. education level 
  ggplot(fmld.sub) + aes(x = factor(HIGH_EDU), y=FINCBEFX ) + geom_boxplot()
  # this makes sense, higher the education level, higher the median income. But still, many outliers --> how to deal with them ?
  ggplot(fmld.sub) + aes(x = factor(HIGH_EDU), y=sqrt(FINCBEFX) ) + geom_boxplot()

  ggplot(fmld.sub) + aes(x = factor(EDUC_REF), y=FINCBEFX ) + geom_boxplot()
  # seems like the reference person's education contains roughly the same info with the highest education for a CU unit. This make sense.
  # So we can use either one of the two variables. 
  
## income vs. w/o Food stamps ( Food Stamps are for poor people)
  ggplot(fmld.sub) + aes(x = factor(REC_FS), y=FINCBEFX ) + geom_boxplot()
  table(fmld.sub$REC_FS)
  # interesting: people w/ fs can have income such high at 200,000 ??!  Question1: ??
  
## income vs. w/o contribution to the Social Security by all CU members
  table(fmld.sub$FJSSDEDM>0)
  plot(fmld.sub$FJSSDEDM, fmld.sub$FINCBEFX , data=fmld.sub[fmld.sub$FJSSDEDM>0, ])
  # it's weird there is no positive realtionship Question2: ??
  
  
  
# memd file:
memd = readRDS("DATA/diary15/rds/memd.rds")
  
 col.memd.name = c("NEWID","MEMBNO","AGE","")

  
  
  
  
  
  
  
  
