library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

input <- read_lines("./code/05/input.txt")

## Solution to part 1
solution_1 <- input %>%
              ## Lots of parsing to convert input text into a 4-column matrix
              str_split(pattern = "->") %>%
              unlist() %>%
              trimws() %>%
              str_split(pattern = ",") %>%
              unlist() %>%
              as.numeric() %>%
              matrix(nrow = length(input),
                     ncol = 4,
                     byrow = TRUE)

max_x <- max(solution_1[, c(1,3)])
max_y <- max(solution_1[, c(2,4)])


match_check <- function(x) {
  
  out <- "no_match"
  
  if(length(unique(x)) == 1) {
    out <- "single_point"
  } else if(x[1] == x[3]) {
    out <- "x_match"
  } else if(x[2] == x[4]) {
    out <- "y_match"
  } else if(length(x[1]:x[3]) == length(x[2]:x[4])) {
    ## added this piece of logic for solution 2.
    ## list of integers between x1 and x2 needs to 
    ##  be the same length as list of integers between
    ##  y1 and y2 to me 45-degree diagonal condition.
    out <- "diag_match"
  }
  
  return(out)
}

## bad form, but we're going to grow this list iteratively
solution_points <- data.frame(x = as.numeric(c()),
                              y = as.numeric(c()),
                              row_index = as.numeric(c()))

for(i in 1:nrow(solution_1)) {
  
  match_type <- match_check(solution_1[i, ])
  
  if(match_type == "x_match") {
    x1 <- solution_1[i, 1]
    x2 <- solution_1[i, 3]
    y1 <- solution_1[i, 2]
    y2 <- solution_1[i, 4]
    y_coords <- y1:y2
    x_coords <- rep(x1, length(y_coords))
    
    
    
    solution_points <- rbind(solution_points,
                              data.frame(x = x_coords,
                                         y = y_coords,
                                         row_index = i))
    
  } else if(match_type == "y_match") {
    x1 <- solution_1[i, 1]
    x2 <- solution_1[i, 3]
    y1 <- solution_1[i, 2]
    y2 <- solution_1[i, 4]
    x_coords <- x1:x2
    y_coords <- rep(y1, length(x_coords))
    
    solution_points <- rbind(solution_points,
                             data.frame(x = x_coords,
                                        y = y_coords,
                                        row_index = i))
    
  }


}

  ## Add 1 to each coordinate since the origin (0, 0) corresponds
  ##  to matrix entry [1, 1]
  solution_points$x <- solution_points$x + 1
  solution_points$y <- solution_points$y + 1
  
vent_diagram <- matrix(data = 0,
                       nrow = max_y + 1,
                       ncol = max_x + 1)

for(i in 1:nrow(solution_points)) {
  
  x <- solution_points$x[i]
  y <- solution_points$y[i]
  
  ## y comes first as it represents rows
  vent_diagram[y, x] <- vent_diagram[y, x] + 1
  
}

vent_diagram

sum(vent_diagram >= 2)

## Solution to part 2

solution_2 <- solution_1

solution_2_points <- data.frame(x = as.numeric(c()),
                                y = as.numeric(c()),
                                row_index = as.numeric(c()),
                                match_type = as.character(c()))

for(i in 1:nrow(solution_2)) {
  
  match_type <- match_check(solution_2[i, ])
  
  if(match_type == "x_match") {
    x1 <- solution_2[i, 1]
    x2 <- solution_2[i, 3]
    y1 <- solution_2[i, 2]
    y2 <- solution_2[i, 4]
    y_coords <- y1:y2
    x_coords <- rep(x1, length(y_coords))
    
    
    
    solution_2_points <- rbind(solution_2_points,
                             data.frame(x = x_coords,
                                        y = y_coords,
                                        row_index = i,
                                        match_type = match_type))
    
  } else if(match_type == "y_match") {
    x1 <- solution_2[i, 1]
    x2 <- solution_2[i, 3]
    y1 <- solution_2[i, 2]
    y2 <- solution_2[i, 4]
    x_coords <- x1:x2
    y_coords <- rep(y1, length(x_coords))
    
    solution_2_points <- rbind(solution_2_points,
                             data.frame(x = x_coords,
                                        y = y_coords,
                                        row_index = i,
                                        match_type = match_type))
    
  } else if(match_type == "diag_match") {
    x1 <- solution_2[i, 1]
    x2 <- solution_2[i, 3]
    y1 <- solution_2[i, 2]
    y2 <- solution_2[i, 4]
    x_coords <- x1:x2
    y_coords <- y1:y2
    
    solution_2_points <- rbind(solution_2_points,
                               data.frame(x = x_coords,
                                          y = y_coords,
                                          row_index = i,
                                          match_type = match_type))
  }
  
  
}

## Add 1 to each coordinate since the origin (0, 0) corresponds
##  to matrix entry [1, 1]
solution_2_points$x <- solution_2_points$x + 1
solution_2_points$y <- solution_2_points$y + 1

vent_diagram_2 <- matrix(data = 0,
                       nrow = max_y + 1,
                       ncol = max_x + 1)

for(i in 1:nrow(solution_2_points)) {
  
  x <- solution_2_points$x[i]
  y <- solution_2_points$y[i]
  
  ## y comes first as it represents rows
  vent_diagram_2[y, x] <- vent_diagram_2[y, x] + 1
  
}

vent_diagram_2

sum(vent_diagram_2 >= 2)

