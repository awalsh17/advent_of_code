# day 19 

# get the data -----
source("R/get_input.R")
input <- get_input(19)
empty_line <- which(input == "")
workflows <- input[1:(empty_line-1)]
names(workflows) <- sapply(workflows, \(x) strsplit(x,  "\\{|\\}|,")[[1]][1])
rules <- input[(empty_line+1):length(input)]
rules <- paste("list", sub("\\}", "\\)", sub("\\{", "\\(", rules)))

# part 1 ------
# turn the workflows into functions? 
convert_to_function <- function(line) {
 values <- strsplit(line, "\\{|\\}|,")[[1]]
 name <- values[1] 
 n_tests <- length(values) - 1
 tests <- values[-1]
 tests <- tests[-length(tests)]
 final_condition <- values[length(values)]
 function_text <-  paste0("if(", sub(":", ") { return('", tests), "')}")
 function_text <- paste(paste(function_text, collapse = ";"), 
                        paste0("; return('", final_condition, "')"))
 # function_text <- paste0("function(x,a,s,m) {", function_text, "}")
 return(function_text)
}
# example output
convert_to_function(workflows[2])
# example evaluation
eval(parse(text = convert_to_function(workflows[2])), 
     envir = list(a = 10, m = 10, x = 10))
# example with real input
eval(parse(text = convert_to_function(workflows[2])), 
     envir = eval(parse(text = rules[1])))

# now make a recursive function to run this for each rule
get_answer <- function(name, rule) {
 answer <- eval(parse(text = convert_to_function(workflows[name])), 
                envir = eval(parse(text = rule)))
 if (answer %in% c("R", "A")) return(answer)
 return(get_answer(answer, rule))
}

# this seems to work!
accepted <- (sapply(rules, \(x) get_answer("in", x)) == "A")
# 62 are accepted
accepted <- rules[accepted]
stringr::str_extract_all(accepted, "[0-9]+") |> unlist() |> as.numeric() |> sum()
# 287054

# part 2 ------
# How many distinct combinations of ratings will be accepted by the Elves' workflows?
# can I brute force this? there are smarter searches I could do to come up with this...
# is it possible to work backwards?
4000 * 4000 * 4000 * 4000

# deep thoughts ------
# Lots of eval(parse()) and I am not sure if that is a good idea
 