# Practicing functions by making personal sum function in R

my_sum <- function(x) {
  mysum <- 0
  for (i in x) {
    mysum <- mysum + i
  }
  print(mysum)
}
