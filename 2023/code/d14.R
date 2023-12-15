# day 14

# get data and parse it
source("R/get_input.R")
input <- get_input(14)
test <- readLines("2023/inputs/input14test")

# part 1 -----
# Tilt the platform so that the rounded rocks all roll north. Afterward, 
# what is the total load on the north support beams?
solve1 <- function(input) {
  start <- Reduce(rbind, (lapply(input, \(x) strsplit(x, "")[[1]])) )
  tilt_north <- function(grid) {
    # get the indices of the rocks and flat and open
    rocks <- which(grid == "O", arr.ind = TRUE)
    flats <- which(grid == "#", arr.ind = TRUE)
    open <- which(grid == ".", arr.ind = TRUE) 
    # get the indices of northmost most for each rock (lowest row in that col )
    for (i in seq_len(nrow(rocks))) {
      # get the indices of the rocks and flat and open
      rocks <- which(grid == "O", arr.ind = TRUE)
      flats <- which(grid == "#", arr.ind = TRUE)
      open <- which(grid == ".", arr.ind = TRUE) 
      # move each rock
      if (rocks[i, 1] == 1) { next }
      current_col <- rocks[i, 2]
      current_row <- rocks[i, 1]
      all_moves <- open[open[,2] == current_col & open[,1] < current_row, ,drop = F]
      if (nrow(all_moves) == 0) { next }
      # remove anything from all_moves that is blocked by a flat
      highest_flat <- flats[flats[,2] == current_col & flats[,1] < current_row, , drop = F]
      highest_flat <- highest_flat[nrow(highest_flat), , drop = F]
      if (nrow(highest_flat) > 0) {
        all_moves <- all_moves[all_moves[, 1] > highest_flat[1,1], , drop = F]
      }
      # move the rock
      if (nrow(all_moves) > 0) {
        best_move <- all_moves[1, ]
        grid[current_row, current_col] <- "."
        grid[best_move[1], best_move[2]] <- "O"
      }
    }
    return(grid)
  }
  end = as.data.frame(tilt_north(start))
  # now get the score
  rock_counts <- rowSums(end == "O")
  rock_weights <- rev(1:length(rock_counts))
  sum(rock_counts * rock_weights)
}
solve1(input)

# part 2 ------
# Run the spin cycle for 1000000000 cycles. Afterward, what is the total 
# load on the north support beams?

# deep thoughts -----
# part one was not bad and then part two is a bit annoying to have to rotate 
# but it seems doable