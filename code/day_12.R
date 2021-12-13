# Day 12 in R

library(igraph)
# Input ----
input <- read.table(here::here("input/day12/input.txt"), sep = "-", header = F)
test_input <- read.table(here::here("input/day12/test_input.txt"), sep = "-", header = F)

# Part 1 thoughts ----
# The power n of an adj matrix is all the n-length walks
# but how to prune for the walks with small digits based on their weight?
# we also cannot repeat the start/end nodes

# Solve ----

solve1 <- function(input) {
  thegraph <- igraph::graph_from_edgelist(as.matrix(input), directed = F)
  theadj <- as.matrix(igraph::as_adj(thegraph))

  # nodes
  all_nodes <- rownames(theadj)
  anchors <- grep("start|end", all_nodes)
  small_nodes <- all_nodes[setdiff(grep("^[a-z]", all_nodes), anchors)]

  # ok - BRUTE FORCE
  paths <- list(c("start"))
  complete_paths <- list()
  counter <- 0
  # step 1
  moves <- colnames(theadj)[theadj["start", ] == 1]
  # do step
  # for each path, get all possible next steps
  while (length(paths) > 0) {
    for (j in seq_len(length(paths))) {
      pathi <- paths[[j]]
      paths <- paths[-j]
      current_cave <- pathi[length(pathi)]
      visited_small <- intersect(small_nodes, pathi)
      if (current_cave == "end") {
        counter <- counter + 1
        complete_paths[[counter]] <- pathi
        break
      } else {
        moves <- colnames(theadj)[theadj[current_cave, ] == 1]
        moves <- setdiff(moves, c(visited_small, "start"))
        if (length(moves) < 1) {
          break
        }
        # create new paths
        for (i in moves) {
          paths <- c(paths, list(c(pathi, i)))
        }
      }
    }
  }
  return(counter)
}

solve1(input)

# Part 2 ----
# now ONE small cave can be visited twice.
# I read this wrong and then almost gave up

# Solve ----
make_step <- function(pathi, theadj) {
  # nodes
  all_nodes <- rownames(theadj)
  anchors <- grep("start|end", all_nodes)
  small_nodes <- all_nodes[setdiff(grep("^[a-z]", all_nodes), anchors)]
  # do
  new_paths <- list()
  current_cave <- pathi[length(pathi)]
  moves <- colnames(theadj)[theadj[current_cave, ] == 1]
  moves <- setdiff(moves, "start")
  # now you count the small nodes
  # if any == 2, then no more small nodes
  visited_small <- intersect(small_nodes, pathi)
  too_many_small <- any(table(pathi[pathi %in% small_nodes]) > 1)
  if (too_many_small) {
    moves <- setdiff(moves, visited_small)
  }
  if (length(moves) > 0) {
    # create new paths
    for (i in moves) {
      new_paths <- unique(c(new_paths, list(c(pathi, i))))
    }
  }
  return(new_paths)
}

get_paths <- function(theadj) {
  paths <- list(c("start"))
  complete_paths <- list()
  # do step
  # for each path, get all possible next steps
  repeat {
    if (length(paths) == 0) {
      break
    }
    new_paths <- list()
    for (i in 1:length(paths)) {
      pathi <- paths[[i]]
      if (pathi[length(pathi)] == "end") {
        complete_paths <- c(complete_paths, list(pathi))
      } else {
        new_paths <- c(new_paths, make_step(pathi, theadj))
      }
    }
    paths <- new_paths
  }
  return(complete_paths)
}

solve2 <- function(input){
  thegraph <- igraph::graph_from_edgelist(as.matrix(input), directed = F)
  theadj <- as.matrix(igraph::as_adj(thegraph))
  length(get_paths(theadj))
}

solve2(test_input)


# incorrect things
all_paths <- igraph::all_simple_paths(thegraph, from = anchors[1], to = anchors[2])

# get_all_walks - cannot repeat start/end either
# They will always start with start neighbors and end with end neighbors
all_walks <- sapply(2:10, function(x) matrixcalc::matrix.power(theadj, x)["start", "end"])
