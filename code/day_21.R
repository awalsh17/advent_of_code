# Day 21 in R
library(memoise)

# Input ----
input <- c(2, 5)

# Part 1 instructions ----
# what do you get if you multiply the score of the
# losing player by the number of times the die was
# rolled during the game?
# Solve ----

rolls <- c(100, 1:100)

play_game <- function(score,
                      input,
                      turn = 1,
                      roll = 0,
                      win_score = 21) {
  die_roll <- sum(rolls[((roll + 1) %% 100 + 1):((roll + 2) %% 100 + 2)])
  if (turn == 1) {
    input[1] <- c(10, 1:9)[(input[1] + die_roll) %% 10 + 1]
    score[1] <- score[1] + input[1]
  } else {
    input[2] <- c(10, 1:9)[(input[2] + die_roll) %% 10 + 1]
    score[2] <- score[2] + input[2]
  }
  roll <- roll + 3
  turn <- ifelse(turn == 1, 2, 1)
  while (score[1] < win_score & score[2] < win_score) {
    new_res <- Recall(score, input, turn, roll, win_score)
    score <- new_res$score
    roll <- new_res$roll
  }
  # print(roll)
  return(list(score = score, roll = roll))
}

rolls <- c(100, 1:100)

play_game(c(0, 0), c(4, 8), roll = 0)

mf <- memoise(play_game)
mf(c(0, 0), c(4, 8), roll = 0, win_score = 1000)


# Part 2 ----
# Using your given starting positions,
# determine every possible outcome.
# Find the player that wins in more universes;
# in how many universes does that player win?
# Solve ----

# get all the possible 3 rolls up to 25
# this is a problem because that is too many combs
# to keep in memory


# each turn is 3^3  = 27 possibilities

# 3^n universes for n rolls
# min rolls to win: 15 - 14348907
# max rolls to win: 33 - 5.559061e+15


# start with player 1.
# If score is 20
# then they always win in 27 universes

# each turn - player gets between 1 and 10 points. but it depends on the
# roll

# there are only 7 possible roll sums - so calculate and weight based
# on the frequency of the sums

options(stringsAsFactors = F)

roll_totals <- data.frame(table(rowSums(expand.grid(rep(list(c(1, 2, 3)), 3)))))
roll_totals$new_val <- as.numeric(as.character(roll_totals$Var1))

# just store the number of times each player wins
play_game2 <- function(score,
                       input,
                       turn = 1,
                       win_score = 21,
                       wins = c(0, 0)) {

  # do all 7 possible rolls
  weight <- roll_totals$Freq
  if (turn == 1) {
    roll_totals$score <- c(10, 1:9)[(roll_totals$new_val + input[1]) %% 10 + 1]
    input_v <- roll_totals$score
    score_v <- score[1] + roll_totals$score
    scores_mat <- matrix(c(score_v, rep(score[2], 7)), nrow = 7)
    input_mat <- matrix(c(input_v, rep(input[2], 7)), nrow = 7)
  } else {
    roll_totals$score <- c(10, 1:9)[(roll_totals$new_val + input[2]) %% 10 + 1]
    input_v <- roll_totals$score
    score_v <- score[2] + roll_totals$score
    scores_mat <- matrix(c(rep(score[1], 7), score_v), nrow = 7)
    input_mat <- matrix(c(rep(input[1], 7), input_v), nrow = 7)
  }
  # add wins for any >= win_score
  wins[turn] <- sum(weight[score_v >= win_score])
  not_wins <- score_v < win_score
  turn <- ifelse(turn == 1, 2, 1)
  if (any(not_wins)) {
    for (i in c(1:7)[not_wins]) {
      # use new coords depending on turn
      new_res <- Recall(scores_mat[i, ], input_mat[i, ], turn, win_score, wins = c(0, 0))
      wins <- wins + new_res$wins * weight[i]
    }
  }
  return(list(score = score, input = input, turn = turn, wins = wins))
}

mf2 <- memoise(play_game2)
options(scipen = 999999)
mf2(c(0, 0), c(4, 8), win_score = 21, wins = c(0, 0))
mf2(c(0, 0), c(2, 5), win_score = 21, wins = c(0, 0))
