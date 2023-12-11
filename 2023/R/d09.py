# day 9 lets do python

import numpy as np
# this needs some recursion

def get_next_row(row):

 row[0] = list(np.diff(row[0]))
 row[1].append(row[0][-1])
 
 if not all(v == 0 for v in row[0]):
  return get_next_row(row)
 return row

# read in input and parse
with open("2023/inputs/input9.txt") as f:
 puzzle_input = [[int(i) for i in line.split(" ")] for line in f.read().splitlines()]
 
# part 1 --------------------
# run in loop to get answer
all_answers = []
for entry in puzzle_input:
 test = [entry, [entry[-1]]]
 result = get_next_row(test)
 result[1].reverse()
 all_answers.append(np.cumsum(result[1])[-1])


sum(all_answers) # 1731106378

# part 2 -----------------
# now add to the left

test = [[10, 13, 16, 21, 30, 45], [45]]

# run in loop to get answer
all_answers = []
for entry in puzzle_input:
 entry.reverse()
 test = [entry, [entry[-1]]]
 result = get_next_row(test)
 result[1].reverse()
 all_answers.append(np.cumsum(result[1])[-1])


sum(all_answers) # 1087
