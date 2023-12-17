# day 16

# get data and parse it
source("R/get_input.R")
# It is another day to choose if we want to make a matrix or long format
input <- get_input(16)
start <- Reduce(rbind, (lapply(input, \(x) strsplit(x, "")[[1]])) )
test <- readLines("2023/inputs/input16test")
test_start <- Reduce(rbind, (lapply(test, \(x) strsplit(x, "")[[1]])) )
# part 1 -----

map_move <- list("R" = c(0,1), "L" = c(0,-1), "U" = c(-1,0), "D" = c(1,0))
map_mirror1 <- list("R" = "D", "L" = "U", "U" = "L", "D" = "R")
map_mirror2 <- list("R" = "U", "L" = "D", "U" = "R", "D" = "L")

run_path <- function(input, start_state) {
 # past_places <- data.frame(row = integer(), col = integer(), dir = character(),
 #                           value = character())
 # track past with a simple string
 past_places <- character()
 # Start with a data.frame of all active things
 n_row = nrow(input)
 n_col = ncol(input)
 current_state <- start_state #
 
 moving <- TRUE
 while (moving == TRUE) {
  # for (x in 1:50) {
  if (all(with(current_state, paste(row,col,dir,value, sep = ".")) %in% past_places)) {
   moving <- FALSE
   print("stop")
  }
  for (i in 1:nrow(current_state)) {
   
   if (paste(current_state[i,], collapse = ".") %in% past_places) {
    # done because you have already been there
    next
   }
   # past_places <- rbind(past_places, current_state[i,])
   past_places <- c(past_places, paste(current_state[i,], collapse = "."))
   
   if ((current_state$row[i] == 1 & current_state$dir[i] == "U" & 
        current_state$value[i] %in% c(".", "|")) | 
       (current_state$col[i] == 1 & current_state$dir[i] == "L" & 
        current_state$value[i] %in% c(".", "-")) | 
       (current_state$row[i] == n_row & current_state$dir[i] == "D" &
        current_state$value[i] %in% c(".", "|")) |
       (current_state$col[i] == n_col & current_state$dir[i] == "R") &
       current_state$value[i] %in% c(".", "-")) {
    # done because you are off map
    next
   }
   # move because you are a legit place
   
   if (current_state[i,"value"] == "\\") {
    # don't go out of bounds
    if ((current_state$dir[i] == "R" & current_state$row[i] == n_row) |
        (current_state$dir[i] == "L" & current_state$row[i] == 1) |
        (current_state$dir[i] == "U" & current_state$col[i] == 1) |
        (current_state$dir[i] == "D" & current_state$col[i] == n_col)) {
     print("out")
    } else {
     current_state$dir[i] <- map_mirror1[[current_state$dir[i]]]
     current_state[i,1:2] <- current_state[i,1:2] + map_move[[current_state$dir[i]]]
     current_state[i,"value"] <- input[current_state[i,1], current_state[i,2]]
    }
    
   } else if (current_state[i,"value"] == "/") {
    # don't go out of bounds
    if ((current_state$dir[i] == "L" & current_state$row[i] == n_row) |
        (current_state$dir[i] == "R" & current_state$row[i] == 1) |
        (current_state$dir[i] == "D" & current_state$col[i] == 1) |
        (current_state$dir[i] == "U" & current_state$col[i] == n_col)) {
     print("out")
    } else {
     current_state$dir[i] <- map_mirror2[[current_state$dir[i]]]
     current_state[i,1:2] <- current_state[i,1:2] + map_move[[current_state$dir[i]]]
     current_state[i,"value"] <- input[current_state[i,1], current_state[i,2]]
    }
   } else if (current_state[i,"value"] == "-" & 
              current_state[i,"dir"] %in% c("L", "R")) {
    current_state[i,1:2] <- current_state[i,1:2] + map_move[[current_state$dir[i]]]
    current_state[i,"value"] <- input[current_state[i,1], current_state[i,2]]
   } else if (current_state[i,"value"] == "|" & 
              current_state[i,"dir"] %in% c("D", "U")) {
    current_state[i,1:2] <- current_state[i,1:2] + map_move[[current_state$dir[i]]]
    current_state[i,"value"] <- input[current_state[i,1], current_state[i,2]]
   } else if (current_state[i,"value"] == ".") {
    current_state[i,1:2] <- current_state[i,1:2] + map_move[[current_state$dir[i]]]
    current_state[i,"value"] <- input[current_state[i,1], current_state[i,2]]
   } else {
    
    if (current_state[i,"value"] == "|") {
     # one goes up
     if (current_state[i, 1] > 1) {
      current_state[i, "dir"] <- "U"
      current_state[i, 1:2] <- current_state[i,1:2] + map_move[["U"]]
      current_state[i,"value"] <- input[current_state[i,1], current_state[i,2]]
     } else {
      current_state[i, "dir"] <- "U"
     }
     # new one goes down
     if (current_state[i, 1] < n_row) {
      new_row <- nrow(current_state) + 1
      current_state[new_row, "dir"] <- "D"
      current_state[new_row, 1:2] <- current_state[i,1:2] + map_move[["D"]]
      current_state[new_row,"value"] <- 
       input[current_state[new_row,1], current_state[new_row,2]]
     } 
     
    } else {
     # one goes left
     if (current_state[i, 2] > 1) {
      current_state[i, "dir"] <- "L"
      current_state[i, 1:2] <- current_state[i,1:2] + map_move[["L"]]
      current_state[i,"value"] <- input[current_state[i,1], current_state[i,2]]
     } else {
      current_state[i, "dir"] <- "L"
     }
     # new one goes right
     if (current_state[i, 2] < n_col) {
      new_row <- nrow(current_state) + 1
      current_state[new_row, "dir"] <- "R"
      current_state[new_row, 1:2] <- current_state[i,1:2] + map_move[["R"]]
      current_state[new_row,"value"] <- 
       input[current_state[new_row,1], current_state[new_row,2]]
     }
    }
   }
  }
 }
 # return(list(current_state, past_places)) # debug
 return(past_places)
}

part_one_start <- data.frame(row = 1, col = 1, dir = "R", value = test_start[1,1])
test_result = run_path(test_start, part_one_start)
length(unique(
 sapply(test_result, 
        \(x) paste(strsplit(x, "\\.")[[1]][1:2], collapse = "."))
)) # 46 is the example answer

part_one_start <- data.frame(row = 1, col = 1, dir = "R", value = start[1,1])
real_result <- run_path(start, part_one_start)
length(unique(
 sapply(real_result, 
        \(x) paste(strsplit(x, "\\.")[[1]][1:2], collapse = "."))
)) #7939 is correct

# part 2 ------
# use foreach/doparallel to run all the options
library(foreach)
doParallel::registerDoParallel(cores=12)

all_options <- data.frame(row = c(rep(1, 110), rep(110, 110), 1:110, 1:110),
                          col = c(1:110, 1:110, rep(1, 110), rep(110, 110))
)
all_options$value <- apply(all_options, 1, \(x) start[x[1],x[2]])
all_options$dir <- c(rep("D", 110), rep("U", 110), rep("R", 110), rep("L", 110))
all_options <- all_options[, c("row", "col", "dir", "value")]

results <- foreach(x = 1:nrow(all_options)) %dopar% {
 real_result <- run_path(start, all_options[x,])
 max_result <- length(unique(
  sapply(real_result, 
         \(x) paste(strsplit(x, "\\.")[[1]][1:2], collapse = "."))
 ))
 return(max_result)
}
max(unlist(results))


# deep thoughts -----
# This one just seemed like another slog with no fun tricks? 
# I am sure that my function could be significantly streamlined.
# the slow part in part 1 is the %in% lookup - so that could be replaced
# for part two, instead of speeding up part 1, I just ran in parallel