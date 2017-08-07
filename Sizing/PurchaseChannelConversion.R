# Determines the amount of customers who purchased only through retail, only through online, or if there was a crossover and which way.

library(plyr)

# Read in, subset, and clean data
reton <- read.delim("Channel.txt", sep = "|")
re <- reton[ , c("flg_2nChnl_R", "flg_3nChnl_R", "flg_4nChnl_R", "flg_5nChnl_R" , "flg_6nChnl_R")]
on <- reton[ , c("flg_2nChnl_O", "flg_3nChnl_O", "flg_4nChnl_O", "flg_5nChnl_O" , "flg_6nChnl_O")]

#NA's can be 0 in this case because a lack of a purchase is still no purchase for this problem
re[is.na(re)] <- 0
on[is.na(on)] <- 0

#Check Totals
datare <- rowSums(re)
dataon <- rowSums(on)

#Set to dataframe
frame <- as.data.frame(datare)
frame2 <- as.data.frame(dataon)
dfnew2 <- data.frame(reton$flg_IPChnl_O, reton$flg_IPChnl_R, frame$datare, frame2$dataon)

OnltoRe <- with(dfnew2, ifelse(dfnew2$reton.flg_IPChnl_O == 1 & dfnew2$frame.datare > 0 , 1, 0))
Online <- with(dfnew2, ifelse(dfnew2$reton.flg_IPChnl_O == 1 & dfnew2$frame2.dataon > 0 & dfnew2$frame.datare == 0 , 1, 0))
RetoOnl <- with(dfnew2, ifelse(dfnew2$reton.flg_IPChnl_R == 1 & dfnew2$frame2.dataon > 0 , 1, 0))
Retail <- with(dfnew2, ifelse(dfnew2$reton.flg_IPChnl_R == 1 & dfnew2$frame.datare > 0 & dfnew2$frame2.dataon == 0 , 1, 0))
Onlinefirst <- with(dfnew2, ifelse(dfnew2$reton.flg_IPChnl_O == 1 & dfnew2$frame.datare == 0 & dfnew2$frame2.dataon == 0 , 1, 0))
Retailfirst <- with(dfnew2, ifelse(dfnew2$reton.flg_IPChnl_R == 1 & dfnew2$frame.datare == 0 & dfnew2$frame2.dataon == 0 , 1, 0))

# Count The occurences of True results for each case
OtoR = 0
for (i in OnltoRe){
  if (i == 1)
      OtoR <- OtoR + 1
                    
}

onlyO = 0
for (i in Online){
  if (i == 1)
    onlyO <- onlyO + 1
  
}

RtoO = 0
for (i in RetoOnl){
  if (i == 1)
      RtoO <- RtoO + 1
}

onlyR = 0
for (i in Retail){
  if (i == 1)
    onlyR <- onlyR + 1
}

online = 0
for (i in Onlinefirst){
  if (i == 1)
    online <- online + 1
}

retail = 0
for (i in Retailfirst){
  if (i == 1)
    retail <- retail + 1
}

print(online)
print(retail)
print(OtoR)
print(onlyO)
print(RtoO)
print(onlyR)
