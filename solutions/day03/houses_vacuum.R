library(tidyverse)
theme_set(theme_bw())

input_orig = read_file("solutions/day03/input") |> 
  str_split_1("")

# Part I
tibble(instr = input_orig) |> 
  mutate(dx = case_match(instr,
                         c(">", "<") ~  0,
                         "v"         ~  1,
                         "^"         ~ -1),
         dy = case_match(instr,
                         c("v", "^") ~  0,
                         ">"         ~  1,
                         "<"         ~ -1)) |> 
  mutate(x = cumsum(dx),
         y = cumsum(dy)) |> 
  distinct(x, y) |> 
  nrow() + 1 # add 1 for (0,0)

# Part II
tibble(instr = input_orig, who = rep(c("santa", "robo"), length(input_orig)/2)) |> 
  mutate(dx = case_match(instr,
                         c(">", "<") ~  0,
                         "v"         ~  1,
                         "^"         ~ -1),
         dy = case_match(instr,
                         c("v", "^") ~  0,
                         ">"         ~  1,
                         "<"         ~ -1)) |> 
  group_by(who) |> 
  mutate(x = cumsum(dx),
         y = cumsum(dy)) |> 
  ungroup() |> 
  distinct(x, y) |> 
  nrow() # revisited (0,0)