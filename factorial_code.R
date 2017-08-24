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

Factorial_reduce <- function(n){
  stopifnot(n >= 0)

  result <- 1
  if(n == 0)
    return(result)

  Reduce(function(a,b){a*b},1:n) 
}

Factorial_func <- function(n){
  stopifnot(n >= 0)

  result <- 1
  if(n == 0)
    return(result)
  
  result<-Factorial_func(n-1)*n
  return(result)
}

fac_tbl <- c(1, rep(NA, 24))

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

min_y <- min(fac_loop_data$med_time,fac_reduce_data$med_time,fac_rec_data$med_time,fac_mem_data$med_time)
max_y <- max(fac_loop_data$med_time,fac_reduce_data$med_time,fac_rec_data$med_time,fac_mem_data$med_time)

plot(1:10, fac_loop_data$med_time, xlab = "Factorial Number", ylab = "Median Time (Nanoseconds)",
     pch = 18, bty = "n", xaxt = "n", yaxt = "n", ylim = c(min_y,max_y))
axis(1, at = 1:10)
axis(2, at = seq(0, max_y, by = max_y/10.))
points(1:10 + .1, fac_reduce_data$med_time, col = "blue", pch = 18)
points(1:10 + .1, fac_rec_data$med_time, col = "red", pch = 18)
points(1:10 + .1, fac_mem_data$med_time, col = "yellow", pch = 18)
legend(1, (max_y-min_y)*2./3., c("Loop", "Reduce","Recursion","Mem"), pch = 18, 
       col = c("black", "blue","red","yellow"), bty = "n", cex = 1, y.intersp = 1.5)

final_df <- left_join(fac_mem_data,fac_loop_data,by=c("num")) %>% 
  left_join(fac_reduce_data,by=c("num")) %>% 
    left_join(fac_rec_data,by=c("num"))
final_df
