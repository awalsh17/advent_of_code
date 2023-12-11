# day 9

# get data
source("R/get_input.R")
input <- get_input()
# make this a long format with x, y, and value.
# then we convert this to an edge list and use igraph to find the components
grid_in <- data.frame(
 row = rep(1:length(input), each = length(input)),
 col = rep(1:length(input), length(input)),
 value = strsplit(paste(input, collapse = ""), split = "")[[1]]
) 

# now merge with all N, S, E, W to get the edges?
target_nodes <- tibble::tibble(
 value = c("F","F","-","-","J","J","|","|","L","L","7","7"),
 row_add = c(0, 1,  1, -1,  0, -1,  0,  0, -1,  0,  0,  1),
 col_add = c(1, 0,  0,  0, -1,  0,  1, -1,  0,  1, -1,  0),
 choices = list(c("-","7","S","J"), c("S","|","J","L"))
)

grid_in <- merge(grid_in, target_nodes, all.x = TRUE, by.x = "value", by.y = "value")
grid_in$row_end <- grid_in$row + grid_in$row_add
grid_in$col_end <- grid_in$col + grid_in$col_add
grid_in <- dplyr::left_join(grid_in, 
                            dplyr::select(grid_in, row_end = row, col_end = col, 
                                            target = value),
                            by = c("row_end", "col_end"))
grid_in <- dplyr::filter(unique(grid_in), !is.na(target))

# filter any non-edges
fair_edges <- c()

 # Deal with S and .
solve1 <- function() {
 
}

solve1()

solve2 <- function() {
 
}
solve2()