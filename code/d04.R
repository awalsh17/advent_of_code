# --- Day 4: Camp Cleanup ---

real <- read.csv("input/input04.txt", header = FALSE, col.names = c("elf1","elf2"))
dummy<- read.csv("input/dummy04.txt", header = FALSE, col.names = c("elf1","elf2"))

# In how many assignment pairs does one range fully contain the other?
solve1 <- function(input){
  input %>% 
    tidyr::separate(elf1, into = c("e1", "e1b"), sep = "-") %>% 
    tidyr::separate(elf2, into = c("e2", "e2b"), sep = "-") %>% 
    # lol - they were characters!!
    mutate_all(as.numeric) %>% 
    mutate(test = case_when(
      (e1 >= e2) & (e1b <= e2b) ~ "bad",
      (e2 >= e1) & (e2b <= e1b) ~ "bad",
      TRUE ~ "good"
    )) %>% 
    summarise(answer = sum(test == "bad", na.rm = TRUE))
}
solve1(real) # 297 is too low, 567 is high

solve2 <- function(input) {
  input %>% 
    tidyr::separate(elf1, into = c("e1", "e1b"), sep = "-") %>% 
    tidyr::separate(elf2, into = c("e2", "e2b"), sep = "-") %>% 
    # lol - they were characters!!
    mutate_all(as.numeric) %>% 
    # now ANY overlap
    mutate(test = case_when(
      (e1 >= e2) & (e1 <= e2b) ~ "bad",
      (e2 <= e1b) & (e2 >= e1) ~ "bad",
      TRUE ~ "good"
    )) %>% 
    summarise(answer = sum(test == "bad", na.rm = TRUE))
}
solve2(real) # 915 too high

# took 1-2 tries on each because easier to guess than check my work!
