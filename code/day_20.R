# Day 20 in R

# Input ----
instr <- strsplit(readLines(here::here("input/day20/input.txt"),
                            n = 1), "")[[1]]
input <- read.fwf(here::here("input/day20/input.txt"),
  widths = rep(1, 100), skip = 2L,
  comment.char = "")

tinstr <- strsplit(readLines(here::here("input/day20/test.txt"),
                             n = 1), "")[[1]]
test <- read.fwf("input/day20/test.txt",
                 widths = rep(1, 5),
                 skip = 2L,
                 comment.char = "")

# Part 1 instructions ----
# apply the image enhancement algorithm twice, being careful
# to account for the infinite size of the images.
# How many pixels are lit in the resulting image?

# Solve ----
convert_binary <- function(input) {
  ifelse(input == "#", 1, 0)
}
convert_binary(test)
convert_binary(instr)


do_step <- function(image, instr, inf_state = 0) {
  # add 2 border
  ndim <- nrow(image)
  new_image <- (matrix(inf_state, ndim + 4, ndim + 4))
  new_image[3:(2 + ndim), 3:(2 + ndim)] <- image
  image <- new_image
  # enhance!
  for (i in 2:(nrow(new_image) - 1)) {
    for (j in 2:(nrow(new_image) - 1)) {
      kernel <- image[(i - 1):(i + 1), (j - 1):(j + 1)]
      bin_string <- paste(t(kernel), collapse = "")
      new_pixel <- instr[strtoi(bin_string, base = 2L) + 1] # 1-index
      new_image[i, j] <- new_pixel
    }
  }
  # return
  return(new_image[2:(nrow(new_image) - 1), 2:(nrow(new_image) - 1)])
}
# test
sum(do_step(do_step(convert_binary(test), convert_binary(tinstr)),
            convert_binary(tinstr)))
# real 5819
sum(do_step(do_step(convert_binary(input), convert_binary(instr)),
            convert_binary(instr), 1))


# Part 2 ----
# algorithm 50 times. How many pixels are lit in the resulting image?

# Solve ----
solve2 <- function(input, instr, steps = 50) {
  input <- convert_binary(input)
  instr <- convert_binary(instr)
  zero_color <- (instr[1] == 1)
  inf_state <- 0
  for (x in 1:steps) {
    input <- do_step(input, instr, inf_state)
    if (zero_color) inf_state <- ifelse(inf_state == 1, 0, 1)
  }
  return(sum(input))
}

solve2(test, tinstr)
# this is slow, I am probably not efficient
solve2(input, instr)
