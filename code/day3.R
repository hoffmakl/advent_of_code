# Day 3:
library(tidyverse)
v <- read_lines(here::here("data/day3.txt"))

tictoc::tic()

last_loc <- str_count(v[1])

count_trees <- function(right, down){
  rows_to_check <- seq(1 + down, length(v), by=down)
  count <- 0
  loc <- 1
  for (r in rows_to_check){
    loc <- loc + right
    if (loc > last_loc){
      loc <- loc - last_loc
    }
    check_row <- v[r]
    value <- substr(check_row, loc, loc)
    if (value == "#") {
      count <- count + 1
    }
  }
  return(count)
}

# puzzle 1: traverse 1 down, 3 over and count the "#"s
count_trees(3,1)

# puzzle 2: traverse a list of moves, then multiply the number of trees
rights <- list(1,3,5,7,1)
downs <- list(1,1,1,1,2)
map2(rights, downs, ~count_trees(.x,.y)) %>%
  reduce(`*`)

tictoc::toc()