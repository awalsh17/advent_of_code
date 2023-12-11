# day 6

# read 6
parse_input <- function(path) { 
  res <- readLines(path)
  res <- as.numeric(unlist(stringr::str_extract_all(res, "[0-9]+")))
  matrix(res, nrow = 2, byrow = TRUE)
}
test <- parse_input("inputs/input6test")
input <- parse_input("inputs/input6.txt")

# Determine the number of ways you could beat the record in each race.
# What do you get if you multiply these numbers together?

solve1 <- function(input) {
  # the formula is distance = (time - hold) * hold
  # we need to solve for all integers between the intersect with the input record distance
  # use the quadratic formula
  limit_right <- ceiling(
    apply(input, 2, \(x) (x[1] + sqrt(x[1]^2 - 4 * x[2])) / (2))
  )
  limit_left <- floor(
    apply(input, 2, \(x) (x[1] - sqrt(x[1]^2 - 4 * x[2])) / (2))
  )
  prod(limit_right - limit_left - 1)
}
solve1(input)

# How many ways can you beat the record in this one much longer race?

parse_input <- function(path) { 
  res <- readLines(path)
  res <- gsub(" ", "", res)
  res <- as.numeric(stringr::str_extract_all(res, "[0-9]+"))
  matrix(res, nrow = 2, byrow = TRUE)
}
test <- parse_input("inputs/input6test")
input <- parse_input("inputs/input6.txt")

# can re-use the same part 1
solve1(input)

# hooray! the hard part here was parsing and I bet this could be done in one line...
