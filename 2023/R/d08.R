# day 8

# read in with get_input function
source("R/get_input.R")
input <- get_input()

instructions <- strsplit(input[1], "")[[1]]
maps <- input[3:length(input)]
names(maps) <- sapply(maps, \(x) substr(x, 1, 3))

test_instructions <- c("L", "L", "R")
test_maps <- c(
 "AAA = (BBB, BBB)",
 "BBB = (AAA, ZZZ)",
 "ZZZ = (ZZZ, ZZZ)"
)
names(test_maps) <- sapply(test_maps, \(x) substr(x, 1, 3))

solve1 <- function(instructions, maps) {

 steps <- 0
 current_place <- "AAA"
 # hack, just concatenate instructions to itself to avoid while
 instructions <- rep(instructions, 100)
 for (i in instructions) {
  if (current_place == "ZZZ") { 
   break
  }
  left <- substr(maps[current_place], 8, 10)
  right <- substr(maps[current_place], 13, 15) 
  if (i == "R") {
   current_place <- right
  } else {
   current_place <- left
  }
  steps <- steps + 1
 }
 return(steps) 

}
solve1(test_instructions, test_maps)
solve1(instructions, maps)

# Simultaneously start on every node that ends with A. How many steps does it take before you're only on nodes that end with Z?

# tried to write my own lcm function - it is too slow

lcm <- function(x, y) {
 greater <- max(x, y)
 while(TRUE) {
  if( all(greater %% c(x, y) == 0) ) {
   lcm <- greater
   break
  }
  greater <- greater + 1
 }
 return(lcm)
}

solve2 <- function(instructions, maps) {
 steps <- 0
 current_place <- names(maps)[substr(names(maps), 3,3) == "A"]
 ending_places <- names(maps)[substr(names(maps), 3,3) == "Z"]
 min_steps <- rep(0, length(current_place))
 # hack, just concatenate instructions to itself to avoid while
 instructions <- rep(instructions, 1000)
 for (i in instructions) {
  if (all(min_steps > 0 )) {
   break
  }
  for (j in 1:length(current_place)) {
   if (current_place[j] %in% ending_places) {
    if (min_steps[j] == 0) min_steps[j] <- steps
    next
   }
   left <- substr(maps[current_place[j]], 8, 10)
   right <- substr(maps[current_place[j]], 13, 15) 
   if (i == "R") {
    current_place[j] <- right
   } else {
    current_place[j] <- left
   }
  }
  steps <- steps + 1
 }
 print(lcm(min_steps))
 return(min_steps) 
}

test_instructions <- c("L", "R")
test_maps <- c(
 "11A = (11B, XXX)",
 "11B = (XXX, 11Z)",
 "11Z = (11B, XXX)",
 "22A = (22B, XXX)",
 "22B = (22C, 22C)",
 "22C = (22Z, 22Z)",
 "22Z = (22B, 22B)",
 "XXX = (XXX, XXX)"
)
names(test_maps) <- sapply(test_maps, \(x) substr(x, 1, 3))

options(scipen = 999)
solve2(test_instructions, test_maps)
solve2(instructions, maps) 

Reduce(lcm, c(21409, 11653, 19241, 12737, 14363, 15989))

# I read part 2 wrong for a while, but then got the example
# part 2 would be crazy slow to brute force, so you need a trick
# the trick is that each start -> end while just loop at some interval
# you need to get the smallest number they all multiply to
# I would love a solution where I did not have to use pracma::Lcm
