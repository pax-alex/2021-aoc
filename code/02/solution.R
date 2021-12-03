library(dplyr)
library(readr)
library(tidyr)

input <- read_delim('./code/02/input.txt',
                    delim = '\n',
                    col_names = FALSE)

## Solution to part 1

solution_1 <- input %>%
                rename(input = X1) %>%
                separate(col = input,
                         into = c("direction", "size"),
                         sep = " ") %>%
                mutate(size = as.numeric(size),
                       size = ifelse(direction == "up", -size, size),
                       direction_type = ifelse(direction == "forward", "x", "y"))
                

solution_1 %>%
  group_by(direction_type) %>%
  summarize(sum(size))

## From output above, this is the product of x * y required for the solution.
1957 * 955


## Solution to part 2

solution_2 <- input %>%
                rename(input = X1) %>%
                separate(col = input,
                         into = c("direction", "size"),
                         sep = " ") %>%
                mutate(size = as.numeric(size))
                

  # initialize position
position <- list(x = 0,
                 y = 0,
                 aim = 0)

for(i in 1:nrow(solution_2)) {
  
  if(solution_2$direction[i] == "forward") {
    position$x <- position$x + solution_2$size[i]
    position$y <- position$y  + (position$aim * solution_2$size[i])
  } else if(solution_2$direction[i] == "down") {
    position$aim <- position$aim + solution_2$size[i]
  } else {
    position$aim <- position$aim - solution_2$size[i]
  }
  
}


# this is what the solution asks for: depth times horizontal position
position$x * position$y



