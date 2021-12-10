# Day 10 in R


# Input ----
input <- readLines(here::here('input/day10/input.txt'))
test_input <- readLines(here::here('input/day10/test_input.txt'))

# Part 1 instructions ----
# Sum the scores - it took me a long time to read these!

# Solve ----

score_line <- function(line){
  line <- strsplit(line,"")[[1]]
  open <- c()
  close <- c()
  invalid <- NA
  for (i in line){
    if (i %in% c("(","[","{","<")) {
      open <- factor(c(as.character(open),i), levels = c("(","[","{","<") )
    }
    if (i %in% c(")","]","}",">")) {
      close <- factor(c(as.character(close),i), levels = c(")","]","}",">"))

      if (as.numeric(close[length(close)]) == as.numeric(open[length(open)])) {
        open <- open[-length(open)]
      } else {
        invalid <- close[length(close)]
        break
      }
    }
  }
  score_map <- c(3, 57, 1197, 25137)
  return(score_map[as.numeric(invalid)])
}

solve1 <- function(input){
  scores <- c()
  for (line in input){
    scores <- c(scores, score_line(line))
  }
  sum(na.omit(scores))
}

solve1(input)

# Part 2 ----
# Now, discard the corrupted lines. The remaining lines are incomplete.
# Find the completion string for each incomplete line, score the completion strings, and sort the scores. What is the middle score?

# Solve ----
complete_lines <- function(line){
  line <- strsplit(line,"")[[1]]
  open <- c()
  close <- c()
  invalid <- NA
  for (i in line){
    if (i %in% c("(","[","{","<")) {
      open <- factor(c(as.character(open),i), levels = c("(","[","{","<") )
    }
    if (i %in% c(")","]","}",">")) {
      close <- factor(c(as.character(close),i), levels = c(")","]","}",">"))

      if (as.numeric(close[length(close)]) == as.numeric(open[length(open)])) {
        open <- open[-length(open)]
      } else {
        invalid <- close[length(close)]
        break
      }
    }
  }
  # reverse , numeric, then close
  if (is.na(invalid)){
    completion <- c(")","]","}",">")[as.numeric(rev(open))]
    return(completion)
  } else{
    return(NA)
  }

}
complete_lines(test_input[3])

score_completions <- function(completions){
  score <- 0
  completions <- as.numeric(factor(completions, levels = c(")","]","}",">")))
  for (i in completions) {
    score <- score*5 + i
  }
  score
}

solve2 <- function(input){
  scores <-c()
  for (line in input){
    scores <- c(scores, score_completions(complete_lines(line)))
  }
 return(median(scores, na.rm = TRUE))
}

solve2(input)

# There are a bunch of ways this could have been done. I chose this weird factor way.
