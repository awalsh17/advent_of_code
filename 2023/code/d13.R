# day 13

# get data and parse it
# this is another set of grids that we need to parse
input <- readLines("2023/inputs/input13.txt")
test <- readLines("2023/inputs/input13test")

parse_input <- function(input) {
  grids <- list()
  grid_i <- 1
  this_grid <- data.frame()
  for (i in input) {
    if (i == "") {
      grids[[grid_i]] <- this_grid
      grid_i <- grid_i + 1
      this_grid <- data.frame()
    } else {
      this_grid <- rbind(this_grid, strsplit(i, "")[[1]])
    }
  }
  grids[[grid_i]] <- this_grid
  return(grids)
}

parse_input(test)
# part 1 -----
all_grids <- parse_input(input)
find_symmetric <- function(grid) {
  grid = unname(grid)
  grid = as.data.frame(grid)
  for (i in 1:(ncol(grid)-1)) {
    right <- grid[, (i+1):(min(ncol(grid), i+i)), drop = F]
    left <- rev(grid[, max(1, i - ncol(right) + 1):(i), drop = F])
    symm <- all.equal(left, right, check.names = FALSE)
    if (isTRUE(symm)) { return(i) }
  }
  return(FALSE)
}
scores <- 0
for (i in all_grids) {
  sym_cols <- find_symmetric(i)
  sym_rows <- find_symmetric(t(i))
  if (sym_cols != FALSE) {
    scores <- scores + sym_cols
  } else {
    scores <- scores + 100 * sym_rows
  }
}
scores

# part 2 ------
# fix the smudge - find a case with only one different - that is new value
find_symmetric_smudge <- function(grid) {
  grid = unname(grid)
  grid = as.data.frame(grid)
  for (i in 1:(ncol(grid)-1)) {
    right <- grid[, (i+1):(min(ncol(grid), i+i)), drop = F]
    left <- rev(grid[, max(1, i - ncol(right) + 1):(i), drop = F])
    symm <- all.equal(left, right, check.names = FALSE)
    if (length(symm) == 1 & grepl(" 1 string mismatch", symm[1])) { return(i) }
  }
  return(FALSE)
}
all_grids <- parse_input(input)
scores <- 0
for (i in all_grids) {
  sym_cols <- find_symmetric_smudge(i)
  sym_rows <- find_symmetric_smudge(t(i))
  if (sym_cols != FALSE) {
    scores <- scores + sym_cols
    print("col")
  } else {
    scores <- scores + 100 * sym_rows
    print("row")
  }
}
scores 

# deep thoughts -----
# part one took me longer than I thought it would to get the indices
# part two looked easy but I didnt get it right at first thanks to "11 string", etc.