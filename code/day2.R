# Day 2: password keys

library(tidyverse)

v <- readxl::read_xlsx(here::here("data/day2.xlsx"), col_names=F) %>%
  rename(p = ...1)

tictoc::tic()

df <- # clean vector into df
  v %>%
  separate(p, into = c("key","pw"), sep = ": ") %>%
  separate(key, into = c("min","max"), sep = "-") %>%
  separate(max, into = c("max", "letter"), sep = " ") %>%
  mutate(pw_n = str_extract_all(pw, letter),
         min = as.numeric(min),
         max = as.numeric(max))

# puzzle 1 - passwords with min <= N special letters <= max
df$n <- map(df$pw_n, ~length(.x)) %>% unlist()
df$valid <- ifelse(df$n >= df$min & df$n <= df$max, T, F)
sum(df$valid)

# puzzle 2 - passwords with special letter at either the min or the max location
df2 <- 
  df %>%
  mutate(locations = str_locate_all(pw, letter)) %>%
  mutate(id = row_number()) %>%
  unnest(cols = c(locations)) %>%
  filter(locations[,1] == min | locations[,1] == max) %>%
  add_count(id) %>%
  filter(nn == 1)
nrow(df2)

tictoc::toc()
