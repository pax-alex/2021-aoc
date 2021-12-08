library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

input_draw <- read_lines("./code/04/bingo_draw_input.txt") %>%
                strsplit(",") %>%
                unlist() %>%
                as.numeric()


input_board <- read_delim("./code/04/bingo_board_input.txt",
                          delim = "\\t",
                          col_names = "row_vals")

input_board <-  input_board %>%
  mutate(row_vals = trimws(row_vals),
         neat_rows = purrr::map(row_vals, str_split, pattern = "\\s", simplify = TRUE),
         neat_rows = purrr::map(neat_rows, function(x) x[which(x != "")]),
         neat_rows = purrr::map(neat_rows, as.numeric))

clean_board <- vector(mode = "list", length = nrow(input_board) / 5)

for(i in 1:length(clean_board)) {
  
  row_range <- (i * 5 - 4):(i * 5)
  
  clean_board[[i]] <- input_board[row_range, ]
  clean_board[[i]] <- clean_board[[i]]$neat_rows %>% unlist() %>% matrix(5, 5, byrow = TRUE)
  
}




## Solution to part 1


truth_board <- clean_board %>%
  lapply(function(x) x * 0)

winner <- 0
draw_index <- 1

board_win_check <- function(board) {
  ## x is a 5x5 truth board of only 0's and 1's
  
  rows <- sapply(1:5, function(x, y) sum(y[x,]), y = board)
  cols <- sapply(1:5, function(x, y) sum(y[,x]), y = board)
  
  any(c(rows,cols) == 5)
  
}

while(winner == 0) {
  
  draw <- input_draw[draw_index]
  for(i in 1:length(clean_board)) {
    if(any(clean_board[[i]] == draw)) {
      match_index <- which(clean_board[[i]] == draw)
      
      ## flip truth from 0 to 1 where the bingo number matched
      truth_board[[i]][match_index] <- 1
    }
    
    if(board_win_check(truth_board[[i]])) {
      winner <- i
      break
    }
    
  }
  
  draw_index <- draw_index + 1
  
}

win_board <- clean_board[[winner]]

sum(win_board[!truth_board[[winner]]]) * draw

## Solution to part 2

truth_board_2 <- clean_board %>%
  lapply(function(x) x * 0)

winners <- as.numeric(c())
win_draw <- as.numeric(c())

# only need to search across boards that haven't won yet
#  track that with this var.
board_index <- 1:length(clean_board)

for(i in 1:length(input_draw)) {
  
  draw <- input_draw[i]
  
  for(j in board_index) {
    if(any(clean_board[[j]] == draw)) {
      match_index <- which(clean_board[[j]] == draw)
      
      ## flip truth from 0 to 1 where the bingo number matched
      truth_board_2[[j]][match_index] <- 1
    }
    
    if(board_win_check(truth_board_2[[j]])) {
      winners <- c(winners, j)
      win_draw <- c(win_draw, draw)
      board_index <- board_index[which(board_index != j)]
      print(j)
      print("\n")
      print(truth_board_2[[j]])
    }
    
  }
  
}

win_board <- clean_board[[winners[length(winners)]]]

sum(win_board[!truth_board_2[[winners[length(winners)]]]]) * win_draw[length(win_draw)]
