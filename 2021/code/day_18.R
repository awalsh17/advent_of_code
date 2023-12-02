# Day 18 in R

# Input ----
input <- readLines(here::here("input/day18/input.txt"))
test <- readLines(here::here("input/day18/test_2.txt"))

# Part 1 instructions ----
# Add up all of the snailfish numbers from the homework assignment in
# the order they appear. What is the magnitude of the final sum

# Solve ----
# keep as string? # json? I looked into 3 options:
# nested list, graph object, data.tree object, and named vector

# parse into nested list - I then unlist these to named vector
parse_item <- function(item) {
  eval(parse(
    text = stringr::str_replace_all(
      stringr::str_replace_all(
        stringr::str_replace_all(item, "\\[", "list(x="),
        ",", ",y="
      ),
      "\\]", "\\)"
    )
  ))
}

# split
splitn <- function(number) {
  ex <- which(number >= 10)[1]
  new_pair <- c(floor(number[ex] / 2), ceiling(number[ex] / 2))
  names(new_pair) <- c(
    paste0("x.", names(number[ex])),
    paste0(names(number[ex]), ".y")
  )
  if (ex == length(number)) {
    c(number[1:(ex - 1)], new_pair)
  } else if (ex == 1) {
    c(new_pair, number[(ex + 1):length(number)])
  } else {
    c(number[1:(ex - 1)], new_pair, number[(ex + 1):length(number)])
  }
}

# explode
explode <- function(number) {
  ex <- which(get_depth(number) == 5)[1:2]
  new_number <- number
  if (ex[1] > 1) {
    new_number[ex[1] - 1] <- number[ex[1] - 1] + number[ex[1]]
  }
  if (ex[2] != length(number)) {
    new_number[ex[2] + 1] <- number[ex[2] + 1] + number[ex[2]]
  }
  # replace pair with 0
  new_number[ex[1]] <- 0
  names(new_number)[ex[1]] <- substr(names(new_number)[ex[1]], 1,
                                     (nchar(names(new_number)[ex[1]]) - 2))
  new_number <- new_number[-c(ex[2])]
  new_number
}

# reduce
get_depth <- function(number) {
  stringr::str_count(names(number), "x|y")
}

reduce_number <- function(number) {
  if (any(get_depth(number) >= 5)) {
    # explode the first one
    number <- explode(number)
    number <- reduce_number(number)
  }
  # split
  else if (any(number >= 10)) {
    number <- splitn(number)
    number <- reduce_number(number)
  }
  return(number)
}

add_two <- function(n1, n2) {
  names(n1) <- paste0("x.", names(n1))
  names(n2) <- paste0(names(n2), ".y")
  # c(n1, n2)
  reduce_number(c(n1, n2))
}

# calc magnitude
calc_mag <- function(number) {
  while (length(number) > 1) {
    max_depth <- max(get_depth(number))
    ex <- which(get_depth(number) == max_depth)
    # go from max_depth to min_depth
    for (pair in 1:(length(ex) / 2)) {
      i <- ex[pair:(pair + 1)]
      newn <- 3 * number[i[1]] + 2 * number[i[2]]
      number[i[1]] <- newn
      number <- number[-i[2]]
      names(number)[i[1]] <- sub("x.|y.", "", names(number)[i[1]])
    }
  }
  return(number)
}

# solve whole thing
solve1 <- function(input) {
  input <- lapply(input, parse_item)
  input <- lapply(input, unlist)
  calc_mag(Reduce(add_two, input))
}

solve1(input)


# Part 2 ----
# What is the largest magnitude of any sum of two different
# snailfish numbers from the homework assignment?
# Solve ----

# test all pairwise combinations
# calc magnitude - rewrote this bc one for part 1 was wrong!!
calc_mag <- function(number) {
  while (length(number) > 1) {
    max_depth <- max(get_depth(number))
    ex <- which(get_depth(number) == max_depth)
    dont_touch <- which(get_depth(number) != max_depth)
    # go from max_depth to min_depth
    new_number <- number
    for (x in 1:(length(ex) / 2)) {
      i <- ex[((x - 1) * 2 + 1):((x - 1) * 2 + 2)]
      newn <- 3 * number[i[1]] + 2 * number[i[2]]
      new_number[i[1]] <- newn
      names(new_number)[i[1]] <- sub("x.|y.", "", names(new_number)[i[1]])
    }
    number <- new_number[c(ex[c(T, F)], dont_touch)]
  }
  return(number)
}

solve2 <- function(input) {
  input <- lapply(input, parse_item)
  input <- lapply(input, unlist)
  n <- length(input)
  max_n <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        mag <- calc_mag(add_two(input[[i]], input[[j]]))
        if (mag > max_n) max_n <- mag
      }
    }
  }
  max_n
}

test <- readLines(here::here("input/day18/test_input.txt"))
solve2(input)
