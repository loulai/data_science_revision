#-- my standard deviation
mySd <- function(nums) {
  vec <- na.omit(nums)
  sqrt(mean((vec - mean(vec)) ^ 2))
}


#-- standard deviation (n-1)
rsd <- function(nums){
  vec <- na.omit(nums)
  sqrt(sum((vec - mean(vec)) ^ 2)/(length(vec)-1))
}