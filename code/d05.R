# --- Day 5: Supply Stacks ---

# ah the parsing
real <- read.table("input/input05.txt", skip = 10)
real_crates <- read.fwf("input/input05.txt", widths = rep(4, 9), nrows = 8, header = F)
real_crates <- as.list(real_crates)
real_crates <- lapply(real_crates, stringr::str_extract, pattern = "[A-Z]")
real_crates <- lapply(real_crates, na.omit)


dummy <- read.table("input/dummy05.txt", skip = 5)
dummy_crates <- read.fwf("input/dummy05.txt", widths = c(4,4,4), nrows = 3, header = F)
dummy_crates <- as.list(dummy_crates)
dummy_crates <- lapply(dummy_crates, stringr::str_extract, pattern = "[A-Z]")
dummy_crates <- lapply(dummy_crates, na.omit)

# After the rearrangement procedure completes, what crate ends up on top of each stack?
# First thoughts: this one sucks.
  
solve1 <- function(input1, input2){
  # first for loop of aoc!
  for (line in 1:nrow(input1)) {
    moving <- input2[[input1$V4[line]]][1:input1$V2[line]]
    # add
    input2[[input1$V6[line]]] <- c(rev(moving) ,input2[[input1$V6[line]]] )
    # remove
    input2[[input1$V4[line]]] <- input2[[input1$V4[line]]][input1$V2[line]+1:length(input2[[input1$V4[line]]])]
  
  }
  paste(lapply(input2, function(x) x[1]), collapse = "")
  # return(input2)
}
solve1(dummy, dummy_crates)
solve1(real, real_crates)

solve2 <- function(input) {
  for (line in 1:nrow(input1)) {
    moving <- input2[[input1$V4[line]]][1:input1$V2[line]]
    # add
    input2[[input1$V6[line]]] <- c(moving ,input2[[input1$V6[line]]] )
    # remove
    input2[[input1$V4[line]]] <- input2[[input1$V4[line]]][input1$V2[line]+1:length(input2[[input1$V4[line]]])]
    
  }
  paste(lapply(input2, function(x) x[1]), collapse = "")
  # return(input2)
}