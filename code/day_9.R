# Day 9 in R


# Input ----
input <- read.fwf(here::here('input/day9/input.txt'),widths=rep(1,100))
test_input <- read.fwf(here::here('input/day9/test_input.txt'),widths=rep(1,10))

# Part 1 instructions ----

# Solve ----

# get neighbors, compare to value
solve1 <- function(input){
score <- 0
for (i in 1:nrow(input)){
  for (j in 1:ncol(input)){
    neighbors_min <- min(unlist(c(input[i-1,j],
                                  input[i+1,j],
                                  input[i,j-1],
                                  input[i,j+1])),
                         na.rm = T)
    if (input[i,j] < neighbors_min){
      score <- score + input[i,j] + 1
    }
  }
}
score
}
solve1(input)
# Part 2 ----
# What do you get if you multiply together the sizes of the three largest basins?
# Solve ----

# counter for size of basin
# stop when all new neighbors are 9
find_neighbors <- function(input, i, j){
  neighbors <- matrix(c(i-1,j,i+1,j,i,j-1,i,j+1),4,2,byrow = T)

  values <- apply(neighbors,1, function(x) input[x[1],x[2]])
  values <- lapply(values, function(x) ifelse(is.null(x),9,x))
  #get rid of nines
  neighbors <- na.omit(neighbors[values!=9,])
  return(list(neighbors, unlist(values)))
}
solve2 <- function(input){
  size <- list() # one size for each basin
  for (i in 1:nrow(input)){
    for (j in 1:ncol(input)){
      neighbors <- find_neighbors(input, i, j )
      if (input[i,j] < min(neighbors[[2]],na.rm = T)){ # new basin
        basin <- rbind(c(i,j), neighbors[[1]])
        stop_cond <- TRUE
        while (stop_cond){
          old_basin <- basin
          new_spots <- basin
          for (eachrow in 1:nrow(basin)){
            new_spots <- unique(rbind(new_spots,
              find_neighbors(input, basin[eachrow,1], basin[eachrow,2])[[1]]))
          }
          if (nrow(new_spots)>nrow(old_basin)){
            basin <- new_spots
          } else{
            stop_cond <- FALSE
          }
        }
        size[[paste0(i,j)]] <- nrow(basin)
      }
    }
  }

  answer = sort(unlist(size),decreasing = T)[1:3]
  answer[1]*answer[2]*answer[3]
}
solve2(input)

# after I did this, I thought of a couple ways to seriousy clean it up
# but I wont bother
