library("microbenchmark")

df = data.frame(v=1:4, name=letters[1:4])

microbenchmark(df[3,2], df[3,"name"], df$name[3])

##----- cumulative summing exercises

x = 1:100

# 1: with a for loop
cs_for = function(x){
  for(i in x){
    if(i == 1){
      xc = x[i]
    } else {
      xc = c(xc, sum(x[1:i]))
    }
  }
  xc
}

# 2: apply
cs_apply = function(x){
  sapply(x, function(x) sum(1:x))
}

cs_dplyr = function(x){
  sum(x)
}

# 3: cumsum

microbenchmark(cs_for(x), cs_apply(x), cs_dplyr(x))

microbenchmark(sum(x), cs_dplyr(x),neval=220)

system.time(
  for(i in 1:500000){
    df[3,2]
  }
)

weird = function(x){
  for(i in 1:x){
    df[3,2]
  }
}