# --- Day 11: Monkey in the Middle ---

real <- readLines("input/input11.txt")
dummy <- readLines("input/dummy11.txt")

# parsing....
# seems like an eval(parse()) situation
parse_input <- function(input) {
  positions = seq_len(length(input)) %% 7
  monkeys <- stringr::str_extract(input[positions == 1], "[0-9]")
  starting <- sapply(stringr::str_extract_all(input[positions == 2], "[0-9][0-9]"), as.numeric)
  operation <- stringr::str_extract(input[positions == 3], "[\\*+] [old0-9]+")
  operation <- sub("old", "worry", operation)
  test <- stringr::str_extract(input[positions == 4], "[0-9]+")
  if_true <- stringr::str_extract(input[positions == 5], "[0-9]+")
  if_false <- stringr::str_extract(input[positions == 6], "[0-9]+")
  tibble::tibble(monkeys = as.numeric(monkeys), 
       starting = starting, 
       operation = operation,
       test = as.numeric(test),
       if_true = as.numeric(if_true),
       if_false = as.numeric(if_false))
}

do_round <- function(monkey, all_monkeys, divide = TRUE) {
  
  # lowest item (dont need)
  all_items <- sort(unlist(monkey$starting))
  
  # while loop - for monkey - add here
  while (length(all_items) > 0) {
    # remove from monkey
    worry <- all_items[1]
    all_items <- all_items[-1]
    all_monkeys$counter[monkey$monkeys + 1] <- all_monkeys$counter[monkey$monkeys + 1] + 1
    # do operation
    worry <- eval(parse(text = paste(worry, monkey$operation))) 
    # divide by 3
    if (divide) worry <- floor(worry / 3)
    # test # throw
    if ((worry %% monkey$test) == 0) {
      all_monkeys$starting[[(monkey$if_true + 1)]] <- 
        c(all_monkeys$starting[[(monkey$if_true + 1)]], worry)
    } else {
      all_monkeys$starting[[(monkey$if_false + 1)]] <- 
        c(all_monkeys$starting[[(monkey$if_false + 1)]], worry)
    }
    all_monkeys$starting[[monkey$monkeys + 1]] <- all_items # need?
  }
  return(all_monkeys)
}


# don't track, just # of items inspected
# after 20 rounds, find top 2 monkeys - multiple # items

solve1 <- function(input){
  test <- parse_input(input)
  test$counter <- 0

  n_monkeys <- nrow(test)
  for (round in 1:20) {
    for (i in 1:n_monkeys) {
      test = do_round(test[i, ], test)
    }
  }
  values <- sort(test$counter, decreasing = TRUE)
  values[1] * values[2]
}
solve1(real)

# do not divide by 3
# 10000 rounds
# stop tracking values - they are going to get too large - 
# just the number of items and whether they are divisible by the values
# there has to be some %% trick here I am missing

do_round2 <- function(monkey, all_monkeys) {
  # lowest item (slow)
  all_items <- sort(unlist(monkey$starting))
  
  # while loop - for monkey - add here
  while (length(all_items) > 0) {
    # remove from monkey
    worry <- all_items[1]
    all_items <- all_items[-1]
    all_monkeys$counter[monkey$monkeys + 1] <- all_monkeys$counter[monkey$monkeys + 1] + 1
    # do operation
    worry <- eval(parse(text = paste(worry, monkey$operation))) 
    # divide by product of tests - this is the math trick. stupid
    worry <- worry %% Reduce(`*`, all_monkeys$test)
    # test # throw
    if ((worry %% monkey$test) == 0) {
      all_monkeys$starting[[(monkey$if_true + 1)]] <- 
        c(all_monkeys$starting[[(monkey$if_true + 1)]], worry)
    } else {
      all_monkeys$starting[[(monkey$if_false + 1)]] <- 
        c(all_monkeys$starting[[(monkey$if_false + 1)]], worry)
    }
    all_monkeys$starting[[monkey$monkeys + 1]] <- all_items # need?
  }
  return(all_monkeys)
}

solve2 <- function(input) {
  test <- parse_input(input)
  test$counter <- 0
  
  n_monkeys <- nrow(test)
  for (round in 1:10000) {
    for (i in 1:n_monkeys) {
      test = do_round2(test[i, ], test)
    }
  }
  values <- sort(test$counter, decreasing = TRUE)
  values[1] * values[2]
  # return(test)
}
solve2(real)
# there are a bunch of ways I could fix this (unnest, better vectorize)
