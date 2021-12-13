# Day 13 in R

# Input ----
parse_input <- function(input_file) {
  input <- read.csv(here::here(input_file),
                    header = F,
                    col.names = c("x", "y"))
  instr <- grepl("fold", input$x)
  coords <- input[!instr, ]
  coords$x <- as.numeric(coords$x)
  instr <- input[instr, ]
  instr <- tidyr::separate(instr, x,
                           into = c("dir", "val"),
                           sep = "=", convert = TRUE)
  return(list(coords, instr))
}

input <- parse_input("input/day13/input.txt")
test_input <- parse_input("input/day13/test_input.txt")

# Part 1 instructions ----
# How many dots are visible after completing just the first fold instruction on your transparent paper?
# Solve ----
make_fold <- function(coords, direction) {
  if (direction$dir == "fold along y") {
    dirsplit <- "y"
  } else {
    dirsplit <- "x"
  }
  if (dirsplit == "y") {
    coords$y <- ifelse(coords$y < direction$val, coords$y,
      direction$val - (coords$y - direction$val)
    )
  } else {
    coords$x <- ifelse(coords$x > direction$val, coords$x,
      direction$val + (direction$val - coords$x)
    )
    # reset the coords to (0,0)
    coords$x <- coords$x - direction$val - 1
  }
  return(coords)
}

solve1 <- function(input) {
  nrow(unique(make_fold(input[[1]], input[[2]][1, ])))
}

solve1(input)

# Part 2 ----
# What code do you use to activate the infrared thermal imaging camera system?
# Solve ----

solve2 <- function(input) {
  mycode <- input[[1]]
  for (i in seq_len(nrow(input[[2]]))) {
    mycode <- unique(make_fold(mycode, input[[2]][i, ]))
  }
  # now plot it
  # it is upside down and backwards
  mycode$newy <- (max(mycode$y) - mycode$y)/(max(mycode$y))
  mycode$newx <- (max(mycode$x) - mycode$x)/(max(mycode$x))
  plot(mycode$newx, mycode$newy)
  return(mycode)
}

# I suppose you could write code to translate each part
# to a digit, but I just looked at it.

solve2(input)
