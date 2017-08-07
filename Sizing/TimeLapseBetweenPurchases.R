library(plyr)
library(dplyr)
library(zoo)

#Read Data and empty values
retonlauderptl <- read.delim("PurchaseHistory.txt", sep = "|")
re <- retonlauderptl[, c("flg_IPAxis_category", "flg_2nAxis_category", "flg_3nAxis_category", "flg_4nAxis_category", "flg_5nAxis_category", "flg_6nAxis_category", "IPDate", "TrxDate_2n", "TrxDate_3n", "TrxDate_4n", "TrxDate_5n", "TrxDate_6n")]
re[re == ""] <- NA

#Overall Time Lapse between Purchases
x1 <- 12 * as.numeric((as.yearmon(re$TrxDate_2n) - as.yearmon(re$IPDate)))
x2 <- 12 * as.numeric((as.yearmon(re$TrxDate_3n) - as.yearmon(re$TrxDate_2n)))
x3 <- 12 * as.numeric((as.yearmon(re$TrxDate_4n) - as.yearmon(re$TrxDate_3n)))
x4 <- 12 * as.numeric((as.yearmon(re$TrxDate_5n) - as.yearmon(re$TrxDate_4n)))
x5 <- 12 * as.numeric((as.yearmon(re$TrxDate_6n) - as.yearmon(re$TrxDate_5n)))
dif1 <- mean(x1, na.rm = TRUE)
dif2 <- mean(x2, na.rm = TRUE)
dif3 <- mean(x3, na.rm = TRUE)
dif4 <- mean(x4, na.rm = TRUE)
dif5 <- mean(x5, na.rm = TRUE)
differences <- c(dif1, dif2, dif3, dif4, dif5)
y <- c("2nd-IP", "3rd-2nd", "4th-3rd", "5th-4th", "6th-5th")

#Category Specific Time Lapse between Purchases
categorydiff1 <- with(re, ifelse(re$flg_IPAxis_category == 1 , 12 * as.numeric((as.yearmon(re$TrxDate_2n) - as.yearmon(re$IPDate))), NA))
categorydiff2 <- with(re, ifelse(re$flg_IPAxis_category == 1 , 12 * as.numeric((as.yearmon(re$TrxDate_3n) - as.yearmon(re$TrxDate_2n))), NA))
categorydiff3 <- with(re, ifelse(re$flg_IPAxis_category == 1 , 12 * as.numeric((as.yearmon(re$TrxDate_4n) - as.yearmon(re$TrxDate_3n))), NA))
categorydiff4 <- with(re, ifelse(re$flg_IPAxis_category == 1 , 12 * as.numeric((as.yearmon(re$TrxDate_5n) - as.yearmon(re$TrxDate_4n))), NA))
categorydiff5 <- with(re, ifelse(re$flg_IPAxis_category == 1 , 12 * as.numeric((as.yearmon(re$TrxDate_6n) - as.yearmon(re$TrxDate_5n))), NA))
avg_category_diff1 <- mean(categorydiff1, na.rm =TRUE)
avg_category_diff2 <- mean(categorydiff2, na.rm =TRUE)
avg_category_diff3 <- mean(categorydiff3, na.rm =TRUE)
avg_category_diff4 <- mean(categorydiff4, na.rm =TRUE)
avg_category_diff5 <- mean(categorydiff5, na.rm =TRUE)
categorydiffdata <- c(avg_category_diff1, avg_category_diff2, avg_category_diff3, avg_category_diff4, avg_category_diff5)

#Data Frame
difdata <- data.frame(Overall = differences, category_Diff = categorydiffdata)
write.csv(difdata, file = 'TimeLapse.csv')
