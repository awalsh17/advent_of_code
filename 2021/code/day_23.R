# Day 23 in R
library(dplyr)
library(collections) # for priority_queue
library(digest) # for hash

# Input ----
input <-
"#############
 #...........#
 ###D#B#A#C###
   #C#A#D#B#
   #########"

# answer is 15538 (I can figure that out)

# Part 1 instructions ----
# What is the least energy required to organize the amphipods?
# Solve ----

# The state can be a vector of all places (19)
# or remove the hallway spots they cannot be in? (15)
state <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 2, 1, 1, 4, 3, 2)

costs <- c(1, 10, 100, 1000)

# create a graph of all the spaces
# 1 2 3  4 5 6 7  8 9 10 11
#     12   14  16   18
#     13   15  17   19

edges <- matrix(c(1,2, 2,3, 3,12, 12,13,
                  3,4, 4,5, 5,14, 14,15,
                  5,6, 6,7, 7,16, 16,17,
                  7,8, 8,9, 9,18, 18,19,
                  9,10, 10,11),
                ncol = 2, byrow=T)
gamegraph <- igraph::graph_from_edgelist(edges, directed = FALSE)
plot(gamegraph) # cute
# create a table with start, end, visited
paths <- data.frame(goal = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             1, 1, 2, 2, 3, 3, 4, 4),
                    e = 1:19) %>%
  tidyr::crossing(s = 1:19) %>%
  rowwise() %>%
  # some moves are not "useful" - never get the winning game
  filter(s != e,
         !((s %in% 1:11) & (e %in% 1:11)),
         !(s == 12 & e == 13),
         !(s == 14 & e == 15),
         !(s == 16 & e == 17),
         !(s == 18 & e == 19),
         !(e == 12 & s == 13),
         !(e == 14 & s == 15),
         !(e == 16 & s == 17),
         !(e == 18 & s == 19),
         !s %in% c(3,5,7,9),
         !e %in% c(3,5,7,9)) %>%
  mutate(v = list(unlist(igraph::shortest_paths(gamegraph, s, e)$vpath)),
         v = list(setdiff(v, s)),
         nsteps = length(v)) %>%
  ungroup()

check_won <- function(state, p2 = FALSE){ # T/F did I win
  res <- FALSE
  if (p2) {
    win <-   c(rep(0,11), 1, 1, 1, 1, 2, 2, 2, 2,
               3, 3, 3, 3, 4, 4, 4, 4)
    if (all(state[1:27] == win[1:27])) res <- TRUE
  } else {
    win <-   c(rep(0,11), 1, 1, 2, 2, 3, 3, 4, 4)
    if (all(state[1:19] == win[1:19])) res <- TRUE
  }
  return(res)
}
check_wrong <- function(state) { # return indices of places that need to be moved
  halls <- c(1:11)[state[1:11] != 0]
  # needs to be correct - cannot be zero (empty)
  deep_rooms <- c(13, 15, 17, 19)[state[c(13, 15, 17, 19)] != c(1, 2, 3, 4) &
    state[c(13, 15, 17, 19)] != c(0, 0, 0, 0)]
  rooms <- c(
    c(12, 14, 16, 18)[state[c(12, 14, 16, 18)] != c(1, 2, 3, 4) &
      state[c(12, 14, 16, 18)] != c(0, 0, 0, 0)],
    c(12, 14, 16, 18)[state[c(13, 15, 17, 19)] != c(1, 2, 3, 4) &
      state[c(12, 14, 16, 18)] != c(0, 0, 0, 0)]
  )
  return(unique(c(halls, deep_rooms, rooms)))
}

# so given a state, you can find all the legal moves through filtering
# can further restrict moves that are never optimal
# don't move into first room spot if second is wrong
get_moves <- function(state, p2=FALSE) {
  if (!p2){
    occupied <- c(1:19)[state != 0]
    to_move <- check_wrong(state)
    bad_destination_room <- c(12, 14, 16, 18)[state[c(13, 15, 17, 19)] !=
                                                c(1, 2, 3, 4)]
    moves <-
      paths[(paths$s %in% to_move) & (!paths$e %in% bad_destination_room), ] %>%
      merge(., data.frame(now = state, s = 1:19), by.x = "s",
            by.y = "s", all.x = TRUE)
  } else {
    occupied <- c(1:27)[state != 0]
    to_move <- check_wrong(state, p2 = TRUE)
    bad_destination_room <- c(c(12, 16, 20, 24)[state[c(13, 17, 21, 25)] !=
                                                c(1, 2, 3, 4) |
                                                state[c(14, 18, 22, 26)] !=
                                                c(1, 2, 3, 4) |
                                                state[c(15, 19, 23, 27)] !=
                                                c(1, 2, 3, 4)
                                                ],
                              c(13, 17, 21, 25)[state[c(14, 18, 22, 26)] !=
                                                  c(1, 2, 3, 4) |
                                                  state[c(15, 19, 23, 27)] !=
                                                  c(1, 2, 3, 4)
                              ],
                              c(14, 18, 22, 26)[state[c(15, 19, 23, 27)] !=
                                                  c(1, 2, 3, 4)
                              ]
    )
    moves <-
      paths[(paths$s %in% to_move) & (!paths$e %in% bad_destination_room), ] %>%
      merge(., data.frame(now = state, s = 1:27), by.x = "s",
            by.y = "s", all.x = TRUE)
  }

  # blocked_path <- sapply(moves$v, function(x) any(occupied %in% x))
  blocked_path <- vapply(moves$v, function(x) any(occupied %in% x),
                         FUN.VALUE = logical(1))

  moves[!blocked_path & ((moves$goal == moves$now) | (moves$goal == 0)),]
}


# need to improve this
heuristic <- function(state,
                      p2 = FALSE){ # needs to be the lowest score to goal
  # the cost to move directly to correct room
  in_hall <- state[1:11] != 0
  halls <- sum(
    abs(c(3,5,7,9)[state[1:11][in_hall]] - c(1:11)[in_hall]) *
      costs[state[1:11][in_hall]]
  )
  # pieces in wrong rooms - cost to move 4 (min to move to right room)
  if (!p2){
    wrong <- state[c(13, 15, 17, 19)][state[c(13, 15, 17, 19)] != c(1, 2, 3, 4) &
                                        state[c(13, 15, 17, 19)] != c(0, 0, 0, 0)]
    wrong <- c(wrong,
               state[c(12, 14, 16, 18)][state[c(12, 14, 16, 18)] != c(1, 2, 3, 4) &
                 state[c(12, 14, 16, 18)] != c(0, 0, 0, 0) |
                 state[c(13, 15, 17, 19)] != c(1, 2, 3, 4) &
                 state[c(12, 14, 16, 18)] != c(0, 0, 0, 0)]
    )
    closed <- sum(4*costs[wrong])
  } else {
    wrong <-
      c(state[c(15, 19, 23, 27)][state[c(15, 19, 23, 27)] != c(1, 2, 3, 4) &
                               state[c(15, 19, 23, 27)] != c(0, 0, 0, 0)],
        state[c(14, 18, 22, 26)][state[c(14, 18, 22, 26)] != c(1, 2, 3, 4) &
                                   state[c(14, 18, 22, 26)] != c(0, 0, 0, 0)],
        state[c(13, 17, 21, 25)][state[c(13, 17, 21, 25)] != c(1, 2, 3, 4) &
                                   state[c(13, 17, 21, 25)] != c(0, 0, 0, 0)],
        state[c(12, 16, 20, 24)][state[c(12, 16, 20, 24)] != c(1, 2, 3, 4) &
                                   state[c(12, 16, 20, 24)] != c(0, 0, 0, 0)]
    )
    closed <- sum(4*costs[wrong])
  }
  halls + closed
}

get_min <- function(state, p2=FALSE) { # find low cost with priority queue
  state_id <- digest(state)
  # cost_v <- setNames(c(0), state_id) #named vector
  cost_v <- vector("list", 10000)
  cost_v[[state_id]] <- c(0)
  pq <- priority_queue(items = list(state), 0)
  win_cost <- c()
  # for (x in 1:10) {
  # repeat { #
  while(pq$size()) {
    cur_state <- pq$pop()
    cur_id <- digest(cur_state)
    # if winner - return 0
    if (check_won(cur_state, p2)) {
      win_cost <- c(win_cost, cost_v[cur_id])
      print(paste("winner", win_cost)) # debug
      return(win_cost)
    }
    # get all next moves
    new_moves <- get_moves(cur_state, p2)
    # if no moves - nothing should happen
    if (nrow(new_moves) > 0) {
    # else - update and run get_min
    for (k in 1:nrow(new_moves)) {
      update_state <- cur_state
      update_state[new_moves$e[k]] <- cur_state[new_moves$s[k]]
      update_state[new_moves$s[k]] <- 0
      # update_id <- paste(update_state[1:19], collapse = "")
      update_id <- digest(update_state)
      new_cost <-
        (costs[cur_state[new_moves$s[k]]] * new_moves$nsteps[k]) +
        (cost_v[[cur_id]])

      if (is.null(cost_v[[update_id]])) {
        cost_v[[update_id]] <- new_cost
        priority <- new_cost + heuristic(update_state, p2)
        pq <- pq$push(update_state, -priority)
      } else if (new_cost < cost_v[[update_id]]) {
        cost_v[[update_id]] <- new_cost
        priority <- new_cost + heuristic(update_state, p2)
        pq <- pq$push(update_state, -priority)
      }
    }
    }
  }
  return(win_cost)
}

state <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 2, 1, 1, 4, 3, 2) # real 15538
state <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 3, 4, 2, 3, 4, 1) # 12521
state <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 3, 2, 2, 3, 4, 4) # easy

get_min(state) # ok, but too slow
profvis::profvis(get_min(state) )

# Part 2 ----
# now the input is
#############
#...........#
###D#B#A#C###
  #D#C#B#A#
  #D#B#A#C#
  #C#A#D#B#
  #########
# Solve ----
# I will need to update all the functions. oops.

# need a new paths data.frame
edges <- matrix(c(1,2, 2,3, 3,12, 12,13, 13,14, 14,15,
                  3,4, 4,5, 5,16, 16,17, 17,18, 18,19,
                  5,6, 6,7, 7,20, 20,21, 21,22, 22,23,
                  7,8, 8,9, 9,24, 24,25, 25,26, 26,27,
                  9,10, 10,11),
                ncol = 2, byrow=T)
gamegraph <- igraph::graph_from_edgelist(edges, directed = FALSE)
plot(gamegraph) # cute
# create a table with start, end, visited
paths <- data.frame(goal = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             1, 1, 1, 1,
                             2, 2, 2, 2,
                             3, 3, 3, 3,
                             4, 4, 4, 4),
                    e = 1:27) %>%
  tidyr::crossing(s = 1:27) %>%
  rowwise() %>%
  # some moves are not "useful" - never get the winning game
  filter(s != e,
         !((s %in% 1:11) & (e %in% 1:11)),
         !((s %in% 12:15) & (e %in% 12:15)),
         !((s %in% 16:19) & (e %in% 16:19)),
         !((s %in% 20:23) & (e %in% 20:23)),
         !((s %in% 24:27) & (e %in% 24:27)),
         !s %in% c(3,5,7,9),
         !e %in% c(3,5,7,9)) %>%
  mutate(v = list(unlist(igraph::shortest_paths(gamegraph, s, e)$vpath)),
         v = list(setdiff(v, s)),
         nsteps = length(v)) %>%
  ungroup()

# update get_moves, heuristic, check_wrong, check_won
check_wrong <- function(state, p2 = FALSE) {
  # return indices of places that need to be moved
  halls <- c(1:11)[state[1:11] != 0]
  # should we check if their room is open? (will be handled in get_moves)
  if (p2) {
    # get first occupied in each room
    rooms <- na.omit(c(c(12:15)[state[12:15] != 0][1],
                  c(16:19)[state[16:19] != 0][1],
                  c(20:23)[state[20:23] != 0][1],
                  c(24:27)[state[24:27] != 0][1]))
  } else {
    # needs to be correct - cannot be zero (empty)
    rooms <- c(13, 15, 17, 19)[state[c(13, 15, 17, 19)] != c(1, 2, 3, 4) &
                                      state[c(13, 15, 17, 19)] != c(0, 0, 0, 0)]
    rooms <- c(rooms,
      c(12, 14, 16, 18)[state[c(12, 14, 16, 18)] != c(1, 2, 3, 4) &
                          state[c(12, 14, 16, 18)] != c(0, 0, 0, 0)],
      c(12, 14, 16, 18)[state[c(13, 15, 17, 19)] != c(1, 2, 3, 4) &
                          state[c(12, 14, 16, 18)] != c(0, 0, 0, 0)]
    )
  }
  return(c(halls, rooms))
}

state <- c(0,0,0,0,0,0,0,0,0,0,0,
           4,4,4,3,
           2,3,2,1,
           1,2,1,4,
           3,1,3,2)
get_min(state, p2=TRUE) # this worked! no idea how long it took
