# Day 24 in R
library(dplyr)
# Part 1 instructions ----
# find the largest valid fourteen-digit model number that contains no 0 digits.
# What is the largest model number accepted by MONAD?

# Input ----
input <- read.fwf(here::here("input/day24/input.txt"),
  widths = c(4, 2, 3),
  header = F, col.names = c("abrv", "a", "b")
)
input$abrv <- trimws(input$abrv)
input$a <- trimws(input$a)
input$b <- trimws(input$b)

# reviewed the instructions - only need three lines that differ
constants <- data.frame(
  div1 = input$b[input$abrv == "div"],
  add2 = as.numeric(input$b[input$abrv == "add" &
    input$a == "x" &
    input$b != "z"]),
  add5 = as.numeric(input$b[input$abrv == "add" &
    input$a == "y" &
    input$b != "w"][c(F, F, T)])
)

# Solve ----

# new function to parse abbreviation into operations
inp <- function(x, y) assign(x, y)
eql <- function(x, y) ifelse(x == y, 1, 0)
div <- function(x, y) trunc(x / y)
what_op <- function(abrv) {
  i <- match(abrv, c("inp", "add", "mul", "div", "mod", "eql"))
  c("inp", "+", "prod", "div", "%%", "eql")[i]
}


# not sure what to do - this calcs final z for a vector
check_number <- function(number) {
  w <- x <- y <- z <- 0
  for (i in 1:nrow(input)) {
    operation <- what_op(input[i, 1])
    if (operation == "inp") {
      assign(input[i, 2], number[1])
      number <- number[-1]
    } else {
      assign(input[i, 2], do.call(
        operation,
        list(
          eval(parse(text = input[i, 2])),
          eval(parse(text = input[i, 3]))
        )
      ))
    }
  }
  return(z)
}
# test
check_number(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3))
check_number(c(9, 1, 2, 9, 7, 3, 9, 5, 9, 1, 9, 9, 9, 3))

# only really need this simplified function
calc_z <- function(w0, z0, step) {
  if (constants$div1[step] == "1") { # these always grow
    res <- z0 * 26 + w0 + constants$add5[step]
  } else { # "26" can shrink
    mod2 <- ((z0 %% 26) + constants$add2[step])
    res <- ifelse(w0 == mod2,
      trunc(z0 / 26),
      w0 + constants$add5[step] +
        trunc(z0 / 26) * 26)
  }
  return(res)
}

# write function to find all the numbers
# just store the z vals, and min/max w to get them
# step 1:14, current is data.frame with min_num, max_num, z
find_next <- function(step, current) {
  # if div 1 - grows
  # if div 26 - need to shrink
  res <- current %>%
    tidyr::crossing(w = 1:9) %>%
    dplyr::mutate(
      old_z = z,
      z = calc_z(w, z, step),
      min_num = 10 * min_num + w,
      max_num = 10 * max_num + w
    )
  # can filter out the naughty z here
  if (constants$div1[step] == "26") {
    res <- filter(res, old_z > 10 * z)
  }
  res %>%
    dplyr::group_by(z) %>%
    dplyr::summarise(
      min_num = min(min_num),
      max_num = max(max_num),
      .groups = "drop")
}

start <- data.frame(min_num = 0, max_num = 0, z = 0)
# will run reasonably fast (a second)
for (s in 1:14) {
  start <- find_next(s, start)
}
winners <- start %>% filter(z == 0)
options(scipen = 9999999)
winners$max_num
# 91297395919993 answer

# Part 2 ----
# Go the other way
# Solve ----
winners$min_num
# 71131151917891 answer
