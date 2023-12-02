# day 10 in Julia

# I wanted to use push! and pop!
input = [split(i, "") for i in readlines("input/day10/input.txt")]

flip_dict = Dict("(" => ")", "[" => "]", "{" => "}", "<" => ">")
# return the invalid thing or the end
function do_line(line)
    opens = String[]
    answer = String[]
    for i in line
        
        if i in ["(","[","{","<"]
            push!(opens, i) 
        else
            if flip_dict[opens[end]] == i
                pop!(opens)
            else 
                answer = i
                break
            end
        end 
    end
    if length(answer) > 0
        return answer
    else
        return opens
    end
end

function score(res)
    part1_scores = Dict(")"=>3, "]" =>57, "}"=>1197, ">"=>25137)
    if length(res) == 1
        part1_scores[res]
    else
        0
    end
end

# part 1 answer
sum([score(do_line(x)) for x in input])

# part 2 answer - need to update scoring function 
function score2(res)
    part2_scores = Dict("("=>1, "[" =>2, "{"=>3, "<"=>4)
    score = 0 
    if length(res) > 1
        for i in res
            score = score * 5 + part2_scores[i]
        end
    else
        score = missing
    end
    return score
end

using Statistics
# part 2 answer
median(skipmissing([score2(do_line(x)) for x in input]))
