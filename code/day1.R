# Day 1: if the sum of 2 or 3 numbers equals 2020, return the product

library(purrr)

v <- readxl::read_xlsx(here::here("data/day1.xlsx"), col_names=F)[[1]]

check_sum <- function(idx, target){
  if (sum(v[idx]) == target){
    return(prod(v[idx]))
  }
}

target_prod <- function(v, target, k){
  search <- t(combn(1:length(v), k))
  map(1:nrow(search), ~check_sum(search[.x,], target)) %>%
    flatten()
}

target_prod(v, 2020, 2) # challenge 1
target_prod(v, 2020, 3) # challenge 2


