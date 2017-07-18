library(plyr)

#import data and filter by region
dat <- read.delim("sampledata.txt", sep = "|")
regionOne <- dat[dat$REGION == "NORTH", ]
regionTwo <- dat[dat$REGION == "SOUTH", ]

#make a binary matrix with all combinations of 0 and 1 for four variables without repeat
expand <- expand.grid(0:1, 0:1, 0:1, 0:1) #could also be T and F

#Use an apply function to assign each category (product) to being True or False (1 or 0)
#Sums every occurence in the dataframe when said condition is true
#This way one can see how many customers (each row) are buying say cat 1 with JUST cat 2, just cat 1, 2, and 3, just cat 2 and 3, etc.
#Project only had 4 overall categories but with more would add vars.
#Could also look into changing cats into a matrix for col nums and looping each number with each instance of T/F
NORTHlist <- apply(expand, 1, function(row) {
  Var1 <- row["Var1"]
  Var2 <- row["Var2"]
  Var3 <- row["Var3"]
  Var4 <- row["Var4"]
  sum(with(regionOne, ifelse(regionOne$flg_IPAxis_cat1 == Var1 & regionOne$flg_IPAxis_cat2 == Var2 & regionOne$flg_IPAxis_cat3 == Var3 & regionOne$flg_IPAxis_cat4 == Var4, 1, 0)))
})
NORTHiterations <- data.frame(matrix(unlist(NORTHlist)))
SOUTHlist <- apply(expand, 1, function(row) {
  Var1 <- row["Var1"]
  Var2 <- row["Var2"]
  Var3 <- row["Var3"]
  Var4 <- row["Var4"]
  sum(with(regionTwo, ifelse(regionTwo$flg_IPAxis_cat1 == Var1 & regionTwo$flg_IPAxis_cat2 == Var2 & regionTwo$flg_IPAxis_cat3 == Var3 & regionTwo$flg_IPAxis_cat4 == Var4, 1, 0)))
})
SOUTHiterations <- data.frame(matrix(unlist(SOUTHlist)))
TOTALlist <- apply(expand, 1, function(row) {
  Var1 <- row["Var1"]
  Var2 <- row["Var2"]
  Var3 <- row["Var3"]
  Var4 <- row["Var4"]
  sum(with(regionTwo, ifelse(regionTwo$flg_IPAxis_cat1 == Var1 & regionTwo$flg_IPAxis_cat2 == Var2 & regionTwo$flg_IPAxis_cat3 == Var3 & regionTwo$flg_IPAxis_cat4 == Var4, 1, 0)))
})
TOTALiterations <- data.frame(matrix(unlist(TOTALlist)))
