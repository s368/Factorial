###########################################################################
# 4 functions: loop, reduce, func(i.e. recursion), mem (i.e. memoization).
###########################################################################
#loop
Factorial_loop <- function(n) {
  stopifnot(n >= 0)
  
  result <- 1
  if(n == 0)
    return(result)
    
    for(i in 1:n)
  {
    result <- result * i
  }
  
  return(result)
}

#reduce
Factorial_reduce <- function(n){
  stopifnot(n >= 0)

  result <- 1
  if(n == 0)
    return(result)

  Reduce(function(a,b){a*b},1:n) 
}

# recursion
Factorial_func <- function(n){
  stopifnot(n >= 0)

  result <- 1
  if(n == 0)
    return(result)
  
  result<-Factorial_func(n-1)*n
  return(result)
}

#memoization
fac_tbl <- c(1, rep(NA, 24))
#
Factorial_mem <- function(n){
  stopifnot(n >= 0)

  result <- 1
  if(n == 0)
    return(result)
  
  if(!is.na(fac_tbl[n])){
    return(fac_tbl[n])
  } else {
    fac_tbl[n - 1] <<- Factorial_mem(n - 1)
    fac_tbl[n] <<- fac_tbl[n-1]*n
    return(fac_tbl[n])
  }  
}

###########################################################################
# Execution time comparison: produce 'factorial_output.txt' output.
###########################################################################

library(purrr)
library(microbenchmark)
library(tidyr)
library(magrittr)
library(dplyr)

#loop
fac_loop_data <- map(1:10, function(x){microbenchmark(Factorial_loop(x), times = 100)$time})
names(fac_loop_data) <- paste0(letters[1:10], 1:10)
#names(fac_loop_data) <- paste0("factorial(", 1:10,")")
fac_loop_data <- as.data.frame(fac_loop_data)

#fac_loop_data <- 
fac_loop_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))

#reduce
fac_reduce_data <- map(1:10, function(x){microbenchmark(Factorial_reduce(x))$time})
names(fac_reduce_data) <- paste0(letters[1:10], 1:10)
#names(fac_reduce_data) <- paste0("factorial(", 1:10,")")
fac_reduce_data <- as.data.frame(fac_reduce_data)

fac_reduce_data <- fac_reduce_data %>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))

#recursion
fac_rec_data <- map(1:10, function(x){microbenchmark(Factorial_func(x))$time})
names(fac_rec_data) <- paste0(letters[1:10], 1:10)
#names(fac_rec_data) <- paste0("factorial(", 1:10,")")
fac_rec_data <- as.data.frame(fac_rec_data)

fac_rec_data <- fac_rec_data %>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))

#mem
fac_mem_data <- map(1:10, function(x){microbenchmark(Factorial_mem(x))$time})
names(fac_mem_data) <- paste0(letters[1:10], 1:10)
#names(fac_mem_data) <- paste0("factorial(", 1:10,")")
fac_mem_data <- as.data.frame(fac_mem_data)

fac_mem_data <- fac_mem_data %>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))

final_df <- left_join(fac_mem_data,fac_loop_data,by=c("num")) %>% 
  left_join(fac_reduce_data,by=c("num")) %>% 
    left_join(fac_rec_data,by=c("num")) %>%
      rename(mem=med_time.x,loop=med_time.y,reduce=med_time.x.x,func=med_time.y.y) %>%
        select(-num)

print(as.data.frame(final_df))
###########################################################################
# END
###########################################################################
