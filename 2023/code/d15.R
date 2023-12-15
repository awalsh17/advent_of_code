# day 15

# get data and parse it
source("R/get_input.R")
input <- get_input("15")
input <- (strsplit(input, ",")[[1]])
test = (strsplit("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7", ",")[[1]])
# part 1 -----
run_step <- function(x, y) {
  ((x + y) * 17) %% 256
}
run_algo <- function(input) {
  ascii <- sapply(input, utf8ToInt)
  new <- sapply(ascii, \(x) Reduce(run_step, c(0, x)))
  return(sum(new))
}
run_algo(input)

# part 2 ------


# deep thoughts -----
# hm. part one was easy.
# just reading part 2 was too much for me. so long