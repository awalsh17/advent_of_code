# day 5

# read in very simply an then parsed to list of matrices
test <- scan("inputs/input5test", what = "character")
input <- scan("inputs/input5.txt", what = "character")

# What is the lowest location number that corresponds to any of the initial seed numbers?

# process maps helper function that takes the input and makes a list with each
# map as a matrix
process_maps <- function(input, map_locations) {
  map <- list(
    matrix(
      as.numeric(input[(map_locations[1] + 1):(map_locations[2] - 2)]),
      ncol = 3, byrow = TRUE),
    matrix(
      as.numeric(input[(map_locations[2] + 1):(map_locations[3] - 2)]),
      ncol = 3, byrow = TRUE),
    matrix(
      as.numeric(input[(map_locations[3] + 1):(map_locations[4] - 2)]),
      ncol = 3, byrow = TRUE),
    matrix(
      as.numeric(input[(map_locations[4] + 1):(map_locations[5] - 2)]),
      ncol = 3, byrow = TRUE),
    matrix(
      as.numeric(input[(map_locations[5] + 1):(map_locations[6] - 2)]),
      ncol = 3, byrow = TRUE),
    matrix(
      as.numeric(input[(map_locations[6] + 1):(map_locations[7] - 2)]),
      ncol = 3, byrow = TRUE),
    matrix(
      as.numeric(input[(map_locations[7] + 1):length(input)]),
      ncol = 3, byrow = TRUE)
  )
  map <- lapply(map, \(x) {cbind(x, x[,1] - x[,2])}) # 4: change from input
  map <- lapply(map, \(x) {cbind(x, x[,2] + x[,3] - 1)}) # 5: max in the range
  return(map)
}

solve1 <- function(input) {
  # get the various inputs
  map_locations <- grep("map:", input)
  seeds <- as.numeric(input[2:(map_locations[1] - 2)])
  map <- process_maps(input, map_locations)
  
  final_values <- c()
  for (i in seeds) {
    final_value_i <- i
    for (j in 1:length(map)) {
      if (final_value_i < min(map[[j]][,2]) | final_value_i > max(map[[j]][,5])) {
        next
      } else {
        row_i <- map[[j]][,2] <= final_value_i & map[[j]][,5] >= final_value_i
        if (any(row_i)) {
          final_value_i <- final_value_i + map[[j]][row_i, 4]
        }
      }
    }
    print(final_value_i)
    final_values <- c(final_values, final_value_i)
  }
  min(final_values)
}
solve1(input)

# Now the seeds are ranges....

solve2 <- function(input) {
  # get the various inputs
  map_locations <- grep("map:", input)
  seeds <- as.numeric(input[2:(map_locations[1] - 2)])
  # now expand to all the seeds - col1 is start, col2 is end
  seed_range <- matrix(seeds, ncol = 2, byrow = T)
  seed_range[,2] <- seed_range[,2] + seed_range[,1] - 1
  
  map <- process_maps(input, map_locations)
  
  for (j in 1:length(map)) {
    new_ranges <- matrix(nrow = 0, ncol = 2)

    for (i in 1:nrow(seed_range)) {

      start_value_i <- seed_range[i, 1]
      stop_value_i <- seed_range[i, 2]
      # check the overlap with each row of the map
      row_i_start <- (map[[j]][,2] <= start_value_i & map[[j]][,5] >= start_value_i) 
      row_i_stop <- (map[[j]][,2] <= stop_value_i & map[[j]][,5] >= stop_value_i) 
      row_i <- row_i_start & row_i_stop
      # check the overlap of rows with the seed range 
      if (any(row_i)) {
        # range is all in one row, update all seed range to new values
        start_value_i <- start_value_i + map[[j]][row_i, 4]
        stop_value_i <- stop_value_i + map[[j]][row_i, 4]
        new_ranges <- rbind(new_ranges, c(start_value_i, stop_value_i))
        
      } else if (any(row_i_start) & any(row_i_stop)) {
        # split into two or three
        updated_ranges <- matrix(c(start_value_i, map[[j]][row_i_start, 5],
                                   map[[j]][row_i_stop, 2], stop_value_i,
                                   map[[j]][row_i_start, 5] + 1, map[[j]][row_i_stop, 2] - 1), 
                                 ncol = 2, byrow = T)
        # update values 
        updated_ranges[1, ] <- updated_ranges[1, ] + map[[j]][row_i_start, 4]
        updated_ranges[2, ] <- updated_ranges[2, ] + map[[j]][row_i_stop, 4]
        if (updated_ranges[3, 1] > updated_ranges[3, 2]) {
          updated_ranges <- updated_ranges[-3, ]
        } 
        new_ranges <- rbind(new_ranges, updated_ranges)
      } else  if (any(row_i_start)){
        # split into two
        updated_ranges <- matrix(c(start_value_i, map[[j]][row_i_start, 5],
                                   map[[j]][row_i_start, 5] +1, stop_value_i), 
                                 ncol = 2, byrow = T)
        # update values 
        updated_ranges[1, ] <- updated_ranges[1, ] + map[[j]][row_i_start, 4]
        new_ranges <- rbind(new_ranges, updated_ranges)
        
      } else if (any(row_i_stop)) {
        # split into two
        updated_ranges <- matrix(c(start_value_i, map[[j]][row_i_stop, 2] -1,
                                   map[[j]][row_i_stop, 2], stop_value_i), 
                                 ncol = 2, byrow = T)
        # update values 
        updated_ranges[2, ] <- updated_ranges[2, ] + map[[j]][row_i_stop, 4]
        new_ranges <- rbind(new_ranges, updated_ranges)
      } else {
        # range is not in any row, so just add it to the new_ranges matrix
        new_ranges <- rbind(new_ranges, c(start_value_i, stop_value_i))
      }
    }
    seed_range <- new_ranges
  }
  min(seed_range)
}
solve2(input) 

# I did not try to be clever or short for part 1, just ran it. 
# could use foreach to parallelize searches - but then for part 2...
# cannot brute force all seeds in part two, so consider the ranges
# You could have 5 scenarios:
# seed range entirely in a map row - easy, one range, apply change to values
# seed range is entirely outside of map row - easy, one range, no change to values
# seed range overlaps start of map row - split into two ranges, apply changes
# seed range overlaps end of map row - split into two ranges, apply changes
# seed range has map row in the middle - split into three ranges, apply changes
# this approach was very fast
