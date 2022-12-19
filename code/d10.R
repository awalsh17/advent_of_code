# --- Day 10 ---
library(dplyr)

real <- read.fwf("input/input10.txt",widths = c(4,4)) # I read it in wrong!
dummy <- read.fwf("input/dummy10.txt",widths = c(4,4)) 

# Find the signal strength *DURING* the 20th, 60th, 100th, 140th, 180th, and 220th cycles. 
# What is the sum of these six signal strengths?

# mutliply cycle by X to get strength
solve1 <- function(input){
  table <- input %>% 
    add_row(V1 = "start", V2 = 1, .before = 1) %>%
    mutate(num = ifelse(V1 == "addx", 2, 1),
           V2 = ifelse(is.na(V2), 0, V2),
           cycle = cumsum(num) - 1,
           x = cumsum(V2))
  # fill in the missing values
  table2 <- table %>% 
    tidyr::uncount(num) %>% 
    mutate(cycle = 1:n())
  result <- table2 %>% 
    mutate(sel_cycle  = cycle %in% c(20, 60, 100, 140, 180, 220),
           current_value = ifelse(V1 == "addx", lag(x, n = 1L), x)) %>% 
    mutate(score = cycle * current_value) %>% 
    filter(sel_cycle) %>%
    summarise(final = sum(score, na.rm = TRUE))
  return(result)
}
solve1(real) 

# more reading comprehension - what am I supposed to do?
# lit pixels? 0 - 39 pixels in 6 rows
# if cycle has sprite in that position, then it is lit
solve2 <- function(input) {
  table <- input %>% 
    add_row(V1 = "start", V2 = 1, .before = 1) %>%
    mutate(num = ifelse(V1 == "addx", 2, 1),
           V2 = ifelse(is.na(V2), 0, V2),
           cycle = cumsum(num) - 1,
           x = cumsum(V2))
  # fill in the missing values
  table2 <- table %>% 
    tidyr::uncount(num) %>% 
    mutate(cycle = 1:n())
  result <- table2 %>% 
    mutate(
      current_value = ifelse(V1 == "addx", lag(x, n = 1L), x),
      x_pos = (cycle-1) %% 40,
      y_pos = rev(c(rep(1,40), rep(2, 40), rep(3, 40), rep(4, 40), rep(5, 40), rep(6,40), 7)),
      lit = abs(current_value - x_pos) < 2
    )
  result <- rbind(result[241, ], result[2:240, ]) # slightly wrong ok
  # make a picture
  picture <- result %>% ggplot(aes(x = x_pos, y = y_pos, fill = lit)) + 
    geom_tile() + scale_fill_manual(values = c("white", "black"))
  return(picture)
}
solve2(real) # I LOVE WHEN IT SPELLS SOMETHING!
