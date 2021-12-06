# day 6 in Julia

input = parse.(Int, split(readlines("input/day6/input.txt")[1,],","))

function solve1(input, n)
    counts_dict = Dict(1 => sum(input.==1), 2 => sum(input.==2), 3 => sum(input.==3),
    4 => sum(input.==4),5 => sum(input.==5),6 => sum(input.==6),7 => sum(input.==7),
    8 => sum(input.==8),0 => sum(input.==0))
    for round in 1:n
        new_fish = counts_dict[0]
        for i in 0:7
            counts_dict[i] = counts_dict[i+1]
        end
        counts_dict[6] = counts_dict[6] + new_fish
        counts_dict[8] = new_fish
    end
    sum(counts_dict.vals)
end

solve1(input, 80)

# global scope issues or copy issues
# I got the right answer but have to call the function fresh
solve1(input, 256)
