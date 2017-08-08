#---------------------------------------------------------------------------#
# Permutation & Combination Recursive Function
# Created by: John Hamm on 08/08/2017
# Motivation: Needed to permutate each instance
#             in a column of a dataset for a project
#             (package permute doesn't have a broad one)
#             Decided to add combination one too just in case
#             
#----------------------------------------------------------------------------#

# A recursive solution to permutating a number

permutation <- function(x, na.rm = TRUE) {
  if (x == 0) return(1)
  else if (!is.numeric(x)) return('Please input a number!')
  else return (x * permutation(x-1))
}

# A recursive solution for combinations

combination <- function(n, r) {
  if (r == 0 | r == n) return(1)
  else if (!is.numeric(r) | !is.numeric(n)) return('Please input a number!')
  else return (combination(n-1, r-1) + combination(n-1, r))
}
