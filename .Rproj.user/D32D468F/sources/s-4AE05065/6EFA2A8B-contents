library(purrr)

v <- readxl::read_xlsx(here::here("day1/day1_numbers.xlsx"), col_names=F)[[1]]

check_sum <- function(idx){
  if (sum(v[idx]) == 2020){
    return(prod(v[idx]))
  }
}

target_prod <- function(v, target, k){
  search <- t(combn(1:length(v), k))
  map(1:nrow(search), ~check_sum(search[.x,])) %>%
    flatten()
}

target_prod(v, 2020, 2)
target_prod(v, 2020, 3)


