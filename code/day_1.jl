# Trying Julia in VScode
# I have no idea what I am doing
input = parse.(Int, readlines("input/day1/input.txt"))

# part 1
sum(i>0 for i in diff(input))

# part 2
sum(sum(input[i:i+2])<sum(input[i+1:i+3]) for i in 1:(length(input)-3))
