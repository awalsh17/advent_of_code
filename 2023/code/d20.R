# day 20

# get the data -----
source("R/get_input.R")
input <- get_input(20)

parse_input <- function(lines) {
 b_line <- grep("broadcaster", lines)
 list(
  stringr::str_extract_all(sub("broadcaster -> ", "", lines[b_line]), "[a-z][a-z]")[[1]],
  data.frame(type = substr(lines[-b_line], 1,1),
             input = substr(lines[-b_line], 2,3),
             output = stringr::str_extract(lines[-b_line], "(?<=\\->) .*"),
             send = 1
  )
 )
}
input_list <- parse_input(input)

# -1 is low, +1 is high
bleeps <- rep(-1, length(input_list[[1]]))
modules <- input_list[[1]]
for (i in 1:10) {
 next_modules <- c()
 next_bleeps <- c()
 for (sig in 1:length(bleeps)) {
  c_line <- input_list[[2]][input_list[[2]]$input == modules[sig], ]
  if (c_line$type == "%") {
   # send a pulse depending on the "send" column
   if (bleeps[sig] == -1) { 
    # update the send column
    input_list[[2]][input_list[[2]]$input == modules[sig], "send"] <- -1 * input_list[[2]][input_list[[2]]$input == modules[sig], "send"]
   }
   # add new values for next_modules, next_bleeps
   next_modules <- c(next_modules, strsplit(c_line$output, ", ")[[1]])
   next_bleeps <- c(next_bleeps, 
                    rep(input_list[[2]][input_list[[2]]$input == modules[sig], "send"], length(next_modules)))
  } else { # line type is &
   
  }
 }
 bleeps <- next_bleeps
 modules <- next_modules
}
# part 1 ------

# part 2 ------

# deep thoughts ------
# The puzzle text was extremely long.
# Maybe the input can be a graph and then look for something in there?

