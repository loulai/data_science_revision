#-- my standard deviation
mysd <- function(nums)
  sqrt(mean((nums - mean(nums)) ^ 2))

#-- standard deviation (n-1)
rsd <- function(nums)
  sqrt(sum((nums - mean(nums)) ^ 2)/(length(nums)-1))