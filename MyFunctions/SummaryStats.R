# Summary Statistics Function

#From sum.R, as I wanted to use my own function
my_sum <- function(x) {
  mysum <- 0
  for (i in x) {
    mysum <- mysum + i
  }
  return(mysum)
}

# Custom maximum function
my_max <- function(x) {
  maxvalue <- -1/0.00000000000000000000000000000001 #approaching negative infinity
  for (i in x) {
    if (i <= maxvalue)
      next
    else (maxvalue <- i)
      
  }
  return(maxvalue)
}

# Custom min function
my_min <- function(x) {
  minvalue <- 10000000000000000000000
  for (i in x) {
    if (i >= minvalue)
      next
    else (minvalue <- i)
    
  }
  return(minvalue)
}


# Mean
my_mean <- function(x) {
  numer <- my_sum(x)
  denom <- length(x)
  mymean <- numer/denom
  return(mymean)
}

# Median
my_median <- function(x){
  
  }
# Mode
my_mode <- function(x){
    mymode = as.numeric(names(which(table(x) == max(table(x)))))
    return(mymode)
  }

# Range
my_range <- function(x){
  myrange <- my_max(x) - my_min(x)
  return(myrange)
}
