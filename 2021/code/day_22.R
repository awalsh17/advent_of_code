# Day 22 in R
library(dplyr)
options(scipen = 999999)
# Input ----
input <- read.table(here::here("input/day22/input.txt"),
  col.names = c("dir", "xyz"),
  sep = " " ) %>%
  dplyr::mutate(xyz = gsub("x=|y=|z=", "", xyz)) %>%
  tidyr::separate(xyz,
    into = c("x1", "x2", "y1", "y2", "z1", "z2"),
    convert = TRUE,
    sep = ",|\\.\\.")


test <- read.table(here::here("input/day22/test2.txt"),
  col.names = c("dir", "xyz"),
  sep = " ") %>%
  dplyr::mutate(xyz = gsub("x=|y=|z=", "", xyz)) %>%
  tidyr::separate(xyz,
    into = c("x1", "x2", "y1", "y2", "z1", "z2"),
    convert = TRUE,
    sep = ",|\\.\\.")

# Part 1 instructions ----
# how many cubes are on?
# Solve ----

# parse input to data.frame

# initialize cubes
cubes <- as.data.frame(
  expand.grid(x = c(-50:50), y = c(-50:50), z = c(-50:50)))
cubes$state <- 0

# run steps
for (i in seq_len(nrow(input))) {
  r <- input[i, ]
  cubes <- cubes %>%
    dplyr::mutate(
      inrange =
        between(x, r$x1, r$x2) &
        between(y, r$y1, r$y2) &
        between(z, r$z1, r$z2),
      state = case_when(
        r$dir == "on" & inrange ~ 1,
        r$dir == "off" & inrange ~ 0,
        TRUE ~ state
      )
    ) %>%
    select(x, y, z, state)
}
# sum
sum(cubes$state)
part1 <- 590467

# this solution is too slow. I know we will need to fix in part 2

# Part 2 ----
# do the whole thing
# Solve ----

# its basically 200,000 cube - too big
# just keep track of each ON cube - also too big
# I will need to actually find overlap
# will need cube - cube volume - on/off
# you could optimize this

volume <- function(v) { # get volume
  v <- as.numeric(v)
  (abs(v[2] - v[1]) + 1) * (abs(v[4] - v[3]) + 1) * (abs(v[6] - v[5]) + 1)
}
check_intersect <- function(v1, v2) { # T/F intersect
  # New idea: what about to check if cube entirely inside other cube?
  # true if intersect and true if 1 inside the other
  (v1[2] >= v2[1]) & (v1[1] <= v2[2]) &
    (v1[4] >= v2[3]) & (v1[3] <= v2[4]) &
    (v1[6] >= v2[5]) & (v1[5] <= v2[6])
}
split_cubes <- function(v1, v2) { # Return data.frame with all cubes
  # new idea: just return all cube 2 whole and split cube 1
  overlap <- data.frame(
    dir = v1$dir,
    x1 = max(v1$x1, v2$x1),
    x2 = min(v1$x2, v2$x2),
    y1 = max(v1$y1, v2$y1),
    y2 = min(v1$y2, v2$y2),
    z1 = max(v1$z1, v2$z1),
    z2 = min(v1$z2, v2$z2)
  )
  # if v2 completely covers v1 - just return v2
  if ((v2$x1 < overlap$x1) & (v2$x2 > overlap$x2) &
    (v2$y1 < overlap$y1) & (v2$y2 > overlap$y2) &
    (v2$z1 < overlap$z1) & (v2$z2 > overlap$z2)
  ) {
    split1 <- v2
  } else {
    xv <- sort(c(v2$x1, v2$x2, v1$x1, v1$x2))
    yv <- sort(c(v2$y1, v2$y2, v1$y1, v1$y2))
    zv <- sort(c(v2$z1, v2$z2, v1$z1, v1$z2))
    xlist <- list(c(xv[2], xv[3]))
    if (v1$x1 < v2$x1) xlist <- c(xlist, list(c(xv[1], xv[2] - 1)))
    if (v1$x2 > v2$x2) xlist <- c(xlist, list(c(xv[3] + 1, xv[4])))
    ylist <- list(c(yv[2], yv[3]))
    if (v1$y1 < v2$y1) ylist <- c(ylist, list(c(yv[1], yv[2] - 1)))
    if (v1$y2 > v2$y2) ylist <- c(ylist, list(c(yv[3] + 1, yv[4])))
    zlist <- list(c(zv[2], zv[3]))
    if (v1$z1 < v2$z1) zlist <- c(zlist, list(c(zv[1], zv[2] - 1)))
    if (v1$z2 > v2$z2) zlist <- c(zlist, list(c(zv[3] + 1, zv[4])))

    split1 <- expand.grid(xlist, ylist, zlist)
    split1 <- as.data.frame(t(apply(split1, 1, unlist)))
    colnames(split1) <- c("x1", "x2", "y1", "y2", "z1", "z2")
    split1$dir <- v1$dir
    # remove overlap row
    split1 <- anti_join(split1, overlap,
      by = c("x1", "x2", "y1", "y2", "z1", "z2", "dir")
    )
  }
  new_cubes <- unique(rbind(split1, v2))
  return(new_cubes)
}
# Test helper functions
fake_data <- data.frame(
  dir = c("on", "off"),
  x1 = c(1, 2),
  x2 = c(10, 4),
  y1 = c(1, 2),
  y2 = c(10, 3),
  z1 = c(1, 3),
  z2 = c(10, 6)
)
fake_data <- data.frame(
  dir = c("on", "off"),
  x1 = c(1, 3),
  x2 = c(3, 6),
  y1 = c(1, 2),
  y2 = c(2, 3),
  z1 = c(1, 3),
  z2 = c(3, 6)
)
check_intersect(fake_data[1, -1], fake_data[2, -1]) # one inside the other
check_intersect(fake_data[2, -1], fake_data[1, -1])

split_cubes(fake_data[2, ], fake_data[1, ]) # just the outside
split_cubes(fake_data[1, ], fake_data[2, ]) # everything


# Run the steps
do_step <- function(all_cubes, step, input) {
  new_cube <- input[step, ]
  # check if cube overlaps
  overlaps <- c()
  for (j in seq_len(nrow(all_cubes))) {
    overlaps <- rbind(
      overlaps,
      check_intersect(all_cubes[j, -1], new_cube[, -1])
    )
  }
  if (new_cube$dir == "on" & !any(overlaps)) { # add to all_cubes
    all_cubes <- unique(rbind(all_cubes, new_cube))
  } else if (any(overlaps)) { # split
    for (k in which(overlaps)) {
      splits <- split_cubes(all_cubes[k, ], new_cube) %>%
        filter(dir == "on")
      all_cubes <- rbind(all_cubes, splits)
    }
    all_cubes <- unique(all_cubes[-which(overlaps), ])
  }
  rownames(all_cubes) <- 1:nrow(all_cubes)
  return(all_cubes)
}

solve2 <- function(input) {
  all_cubes <- input[1, ]
  for (i in 2:nrow(input)) { # check all the cubes
    all_cubes <- do_step(all_cubes, i, input)
  }
  all_cubes$volume <- apply(all_cubes[, -1], 1, volume)
  sum(all_cubes$volume)
}

# get the answer - it works!
solve2(input)
