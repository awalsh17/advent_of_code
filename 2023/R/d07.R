# day 7

# read
parse_input <- function(path) {
  read.table(path, sep = " ", header = FALSE, stringsAsFactors = FALSE)
}
test <- parse_input("inputs/input7test")
input <- parse_input("inputs/input7.txt")

# woof - this is a lot of reading to understand the poker rules

solve1 <- function(input) {
  # determine the type of hand by counting each value
  counts <- apply(input, 1, \(x) as.data.frame(table(strsplit(x, "")[[1]])))
  two_pair <- sapply(counts, \(x) if (length(x$Freq) == 3) {
    any(x$Freq == 2L)
  } else {
    FALSE
  })
  full_house <- sapply(counts, \(x) if (length(x$Freq) == 2) {
    any(x$Freq == 3L)
  } else {
    FALSE
  })
  max_counts <- sapply(counts, \(x) max(x$Freq))
  # give high scores to best
  input$score <- max_counts
  input$score[two_pair] <- 2.5
  input$score[full_house] <- 3.5
  # now break the ties
  # R will sort 0-9 and the A-Z, so we can use that to our advantage
  # We can replace the A to T in our data with E, D, C, B, A
  input$new <- sapply(input$V1, \(x) {
    x <- gsub("A", "E", x)
    x <- gsub("K", "D", x)
    x <- gsub("Q", "C", x)
    x <- gsub("J", "B", x)
    x <- gsub("T", "A", x)
    x
  })
  input <- input[order(input$score, input$new), ]
  input$rank <- 1:nrow(input)
  sum(input$V2 * input$rank)
}
solve1(input)

# J are now jokers...

solve2 <- function(input) {
  # Need to rank each hand (1 is low, n is high) and then mult by bid and sum
  counts <- apply(input, 1, \(x) as.data.frame(table(strsplit(x, "")[[1]])))
  # figure out how to change the data to have jokers wild
  counts <- lapply(
    counts,
    \(x) {
      if (("J" %in% x$Var1) & nrow(x) > 1) {
        y <- x[x$Var1 != "J", ]
        y$Freq[which.max(y$Freq)] <- y$Freq[which.max(y$Freq)] + x$Freq[x$Var1 == "J"]
        y
      } else {
        x
      }
    }
  )
  two_pair <- sapply(counts, \(x) if (length(x$Freq) == 3) {
    any(x$Freq == 2L)
  } else {
    FALSE
  })
  full_house <- sapply(counts, \(x) if (length(x$Freq) == 2) {
    any(x$Freq == 3L)
  } else {
    FALSE
  })
  max_counts <- sapply(counts, \(x) max(x$Freq))
  # give high scores to best
  input$score <- max_counts
  input$score[two_pair] <- 2.5
  input$score[full_house] <- 3.5
  # now break the ties
  # make the J a 1
  input$new <- sapply(input$V1, \(x) {
    x <- gsub("A", "E", x)
    x <- gsub("K", "D", x)
    x <- gsub("Q", "C", x)
    x <- gsub("J", "1", x)
    x <- gsub("T", "A", x)
    x
  })
  input <- input[order(input$score, input$new), ]
  input$rank <- 1:nrow(input)
  sum(input$V2 * input$rank)
}
solve2(test)
solve2(input) # 251135960

# working with strings in R is not my favorite, so I am sure other languages do better?
# tricky bit in part 2 is to remember that there can be five of a kind with all J
