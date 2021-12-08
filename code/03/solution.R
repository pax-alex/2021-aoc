library(dplyr)
library(readr)
library(tidyr)
library(purrr)

input <- read_delim('./code/03/input.txt',
                    delim = '\n',
                    col_names = "binary_nums")

test <- read_delim('./code/03/test.txt',
                   delim = '\n',
                   col_names = "binary_nums")

## Solution to part 1


solution_1 <- vector(mode = "list",
                     ## 12 bit numbers
                     length = 12)

for(i in 1:12) {
  solution_1[[i]] <- unlist(lapply(input$binary_nums, substr, start = i, stop = i))
}

helper <- function(x) {
  x <- as.numeric(x)
  check <- 1000 - sum(x)
  if(check < 500) {
    return(0)
  } else if (check > 500) {
    return(1)
  } else return(NA)
  
}

bin_to_dec <- function(x) {
  
  places <- length(x)
  
  for(i in 1:places) {
    x[i] <- (2 ^ (places - i)) * x[i]
  }
  
  sum(x)
  
}

gamma_binary <- unlist(lapply(solution_1, helper))
epsilon_binary <- as.numeric(as.logical(gamma_binary - 1))

bin_to_dec(gamma_binary) * bin_to_dec(epsilon_binary)

## Solution to part 2

oxy_check <- input

for(i in 1:12) {
  
  temp <- oxy_check %>%
    mutate(nth_bit = as.numeric(substr(binary_nums, i, i)))
  
  zeros <- nrow(temp %>% filter(nth_bit == 0))
  ones  <- nrow(temp %>% filter(nth_bit == 1))
  
  if(ones > zeros) {
    
    oxy_check <- temp %>% filter(nth_bit == 1)
  
    ## If there's a tie keep 1's in nth bit (per instructions)
  } else if(ones == nrow(oxy_check) / 2) {
    oxy_check <- temp %>% filter(nth_bit == 1)
  } else {
    oxy_check <- temp %>% filter(nth_bit == 0)
  }
  
  if(nrow(oxy_check) == 1) {
    break()
  }
  
}

co2_check <- input

for(i in 1:12) {
  
  temp <- co2_check %>%
    mutate(nth_bit = as.numeric(substr(binary_nums, i, i)))
  
  zeros <- nrow(temp %>% filter(nth_bit == 0))
  ones  <- nrow(temp %>% filter(nth_bit == 1))
  
  if(ones > zeros) {
    
    co2_check <- temp %>% filter(nth_bit == 0)
    
    ## If there's a tie keep 0's in nth bit (per instructions)
  } else if(ones == nrow(temp) / 2) {
    co2_check <- temp %>% filter(nth_bit == 0)
  } else {
    co2_check <- temp %>% filter(nth_bit == 1)
  }

    if(nrow(co2_check) == 1) {
        break()
    }
  
}

oxygen_rating <- as.numeric(unlist(lapply(1:12, function(x, y) substr(y, x, x), y = oxy_check$binary_nums)))
co2_rating <- as.numeric(unlist(lapply(1:12, function(x, y) substr(y, x, x), y = co2_check$binary_nums)))

bin_to_dec(oxygen_rating) * bin_to_dec(co2_rating)
