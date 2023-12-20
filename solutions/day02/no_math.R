library(tidyverse)

input_orig = read_delim("solutions/day02/input", delim = "x", col_names = c("l", "w", "h"))

# Part I 
input_orig |> 
  mutate(lw = l*w,
         wh = w*h,
         lh = l*h) |> 
  rowwise() |> 
  mutate(min_side = min(lw, wh, lh)) |> 
  mutate(paper = 2*lw + 2*wh + 2*lh + min_side) |> 
  ungroup() |> 
  summarise(sum(paper))

# Part II
input_orig |> 
  rowwise() |> 
  mutate(ribbon = 2*l + 2*w + 2*h - 2*max(l, w, h) + l*w*h) |> 
  ungroup() |> 
  summarise(sum(ribbon))
