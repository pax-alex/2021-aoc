library(dplyr)
library(readr)

## Solution to Part 1

input <- read_delim('./code/01/01-input.txt',
                    delim = '\n',
                    col_names = FALSE)

solution_1 <- input %>%
              rename(depth = X1) %>%
              mutate(depth_change = depth - lag(depth, 1),
                     direction_change = ifelse(depth_change > 0, "increased", "decreased"))

solution_1 %>%
  group_by(direction_change) %>%
  summarize(n())

## Solution to Part 2

solution_2 <- input %>%
                rename(depth = X1) %>%
                mutate(rolling_depth = depth + lead(depth, 1) + lead(depth, 2),
                       rolling_depth_change = rolling_depth - lag(rolling_depth, 1),
                       direction_change = ifelse(rolling_depth_change > 0, "increased",
                                                 ifelse(rolling_depth_change == 0, "no_change",
                                                        "decreased")))

solution_2 %>%
  group_by(direction_change) %>%
  summarize(n())
