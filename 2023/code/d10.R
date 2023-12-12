# day 10
library(igraph)
library(dplyr)

# get data
source("R/get_input.R")
input <- get_input(day = 10)
# make this a long format with x, y, and value.
# then we convert this to an edge list and use igraph to find the components
grid_in <- data.frame(
 row = rep(1:length(input), each = length(input)),
 col = rep(1:length(input), length(input)),
 value = strsplit(paste(input, collapse = ""), split = "")[[1]]
) 

# now merge with all N, S, E, W to get the edges
target_nodes <- tibble::tibble(
  value = c("F","F","|","|","J","J","-","-","L","L","7","7"),
  row_add = c(0, 1,  1, -1,  0, -1,  0,  0, -1,  0,  0,  1),
  col_add = c(1, 0,  0,  0, -1,  0,  1, -1,  0,  1, -1,  0)
)

grid_in <- merge(grid_in, target_nodes, all.x = TRUE, by.x = "value", by.y = "value")
grid_in$row_end <- grid_in$row + grid_in$row_add
grid_in$col_end <- grid_in$col + grid_in$col_add
grid_in <- dplyr::left_join(grid_in, 
                            dplyr::select(grid_in, row_end = row, col_end = col, 
                                            value_end = value),
                            by = c("row_end", "col_end"))
grid_in <- dplyr::filter(unique(grid_in), !is.na(value_end))
# hm - I thought I would have edges here, but I dont really. Make row/col the edges?
grid_in$source <- paste(grid_in$row, grid_in$col, sep = ".")
grid_in$target <- paste(grid_in$row_end, grid_in$col_end, sep = ".")

# make a graph?
grid_in <- dplyr::filter(grid_in, value_end != "S")
g <- graph_from_data_frame(grid_in[, c("source", "target")], directed = TRUE)

# part 1 -----

# get the shortest path from S to next to S?
# "24.115" to "22.115"
myloop = all_shortest_paths(g, from = "24.115", to = "22.115")

# 13845 nodes - steps to the farthest point is 6923

# part 2 -----
# How many tiles are enclosed by the loop?
# I remembered some ideas from past years on calculating
# stuff inside or outside by tracking the sign by rows.
# You start "outside" and then if you hit a loop tile, flip to inside, and 
# then flip back to outside when you hit the next loop tile.
loop_nodes <- data.frame(row.col = names(myloop$res[[1]]),
                         type = "loop")
loop_nodes <- rbind(loop_nodes, data.frame(row.col = "23.115", type = "loop"))
grid_two <- data.frame(
  row = rep(1:length(input), each = length(input)),
  col = rep(1:length(input), length(input)),
  value = strsplit(paste(input, collapse = ""), split = "")[[1]]
) |> 
  dplyr::mutate(row.col = paste(row, col, sep = ".")) |> 
  dplyr::left_join(loop_nodes, by = "row.col") |> 
  dplyr::mutate(type = ifelse(is.na(type), "non", type)) |> 
  dplyr::mutate(sign = -1) |> 
  dplyr::mutate(type = if_else(value == "S", "loop", type)) |> 
  # convert non vertical to non
  dplyr::mutate(type2 = ifelse((type == "loop" & value == "-"), "non", type)) |> 
  arrange(type2, row, col) |> 
  dplyr::mutate(type2 = ifelse((type2 == "loop" & value == "7" & lag(value) == "L"), "non", type2)) |> 
  arrange(type2, row, col) |> 
  dplyr::mutate(type2 = ifelse((type2 == "loop" & value == "J" & lag(value) == "F"), "non", type2 )) |> 
  arrange(row, col)

# now we need to go through the loop and flip the sign
sign <- c()
for (i in 1:length(input)) { # rows
  for (j in 1:length(input)) {
    if (j == 1 & grid_two$type2[grid_two$row == i & grid_two$col == j] == "loop") {
      sign <- c(sign, 1)
    } else if (j == 1 & grid_two$type2[grid_two$row == i & grid_two$col == j] != "loop") {
      sign <- c(sign, -1)
    } else if (grid_two$type2[grid_two$row == i & grid_two$col == j] == "loop") {
      sign = c(sign, sign[((i - 1) * length(input)) + j - 1] * -1)
    } else {
      sign = c(sign, sign[((i - 1) * length(input)) + j - 1])
    }
  }
}

grid_two$sign <- sign
count(grid_two, type, sign) # 1308 is too high

# Thoughts: I new this one was a problem for igraph, I did not bother coding
# my own algorithm there. This solution is not general as I coded in my start node
# I also messed up part 2 by not having the S as a loop node. Then I was stuck 
# again for a while and had to go bit by bit to figure it out.

# for fun, plot!
grid_two |> ggplot(aes(y = row, x = col, color = type, shape = type)) + geom_point()
