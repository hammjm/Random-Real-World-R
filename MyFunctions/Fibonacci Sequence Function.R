#-------------------------------------------------------#
# Fibonacci Sequence Function
# Created by: John Hamm on 8/8/2017
# Motivation: Wanted to make another recursive function.
#-------------------------------------------------------#

# Input which number in the sequence you want to find

fib <- function(x){
  if (x == 1) return(0)
  else if (x == 2) return(1)
  else (fib(x-1) + fib(x-2))
}
