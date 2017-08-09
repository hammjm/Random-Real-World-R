# Summary Statistics Function
my_summary <- function(x) {
  sumstat <- data.frame(Min = my_min(x), Max = my_max(x), Mean = my_mean(x), 
                        Median = my_median(x), Mode = my_mode(x), Range = my_range(x), 
                        Variance = my_variance(x), StandardDev = my_stdev(x))
  return(sumstat)
  }

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
  sorted <- sort(x, decreasing = FALSE)
  length_sort <- length(sorted)
  if (length_sort %% 2 == 0)
    mymedian <- sorted[length_sort/2]
  else (mymedian <- (sorted[((length_sort)/2) + 0.5] + (sorted[((length_sort)/2) - 0.5]))/2)
  return(mymedian)
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

#Variance
my_variance <- function(x){
  m <- my_mean(x)
  numerator <- 0
  for (i in x){
    numerator <- ((i - m)^2) + numerator
  }
  return(numerator/(length(x)-1))
}

#Standard Dev
my_stdev <- function(x){
  stdev <- my_variance(x)^0.5
}
