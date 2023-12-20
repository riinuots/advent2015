library(tidyverse)

input_orig = read_csv("solutions/day05/input", col_names = "string")

# Part I
vowels = "a|e|i|o|u"
twoletters = paste0(letters, letters) |> paste(collapse = "|")
not = "ab|cd|pq|xy"

input_orig |> 
  filter(str_count(string, vowels) > 2) |> 
  filter(str_detect(string, twoletters)) |> 
  filter(! str_detect(string, not)) |> 
  nrow()

# Part II
threeway = paste0(letters, ".", letters) |> paste(collapse = "|")
pairs = crossing(l1 = letters, l2 = letters) |> 
  mutate(l = paste0(l1, l2)) |> 
  pull(l)

input_orig |> 
  filter(map_lgl(string, ~any(str_count(.x, pairs) > 1))) |> 
  filter(str_detect(string, threeway)) |> 
  nrow()
