# day 2 in Julia

# part 1
# I basically only know what I learned in day 1
input = readlines("input/day2/input.txt")
h = (i[9:end] for i in input if i[1:3]=="for")
d = (i[6:end] for i in input if i[1:3]=="dow")
u = (i[4:end] for i in input if i[1:3]=="up ")

# answer
sum(parse.(Int,h)) * (sum(parse.(Int, d)) - sum(parse.(Int, u)))

# part 2
# tracking [h, a, d]
# baby's first function
function update_position(line, pos)
    dir = split.(line)[1]
    n = parse(Int,split.(line)[2])
    if dir == "down"
        pos[2] = pos[2]+n
    end
    if dir == "up"
        pos[2] = pos[2]-n
    end
    if dir == "forward" 
        pos[1] = pos[1]+n
        pos[3] = pos[3]+ (n*pos[2])
    end
    return pos
end

pos = [0,0,0]
for i in input # global scope what?!? no idea
    global pos = update_position(i, pos)
end
# answer
pos[1]*pos[3]