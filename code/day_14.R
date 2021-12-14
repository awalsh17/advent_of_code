# Day 14 in R

# Input ----
parse_input <- function(input_file) {
  input <- list()
  input$template <- scan(here::here(input_file),
    what = "character",
    sep = "", nlines = 1
  )
  input$instr <- read.fwf(here::here(input_file), widths = c(2, 4, 1), skip = 2)
  input
}

input <- parse_input("input/day14/input.txt")
test_input <- parse_input("input/day14/test_input.txt")

# Part 1 instructions ----
# do ten
# What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

# can either vector and add or make data.frame and do repeated joins
# Solve ----

do_step <- function(polymer, instr) {
  new_polymer <- ""
  polymer <- strsplit(polymer, "")[[1]]
  for (i in seq_len(length(polymer) - 1)) {
    chunk <- paste0(polymer[i], polymer[i + 1])
    add_row <- instr[instr$V1 == chunk, ]

    if (nrow(add_row) > 0) {
      new_polymer <- paste0(
        substr(new_polymer, 1, nchar(new_polymer) - 1),
        substr(add_row$V1, 1, 1),
        add_row$V3, substr(add_row$V1, 2, 2)
      )
    } else {
      new_polymer <- paste0(substr(new_polymer, 1, nchar(new_polymer) - 1), chunk)
    }
  }
  new_polymer
}

solve1 <- function(input, nsteps = 10) {
  polymer <- input$template
  for (i in 1:nsteps) {
    polymer <- do_step(polymer, input$instr)
  }
  # count all letters
  counts <- sapply(LETTERS, function(x) stringr::str_count(polymer, x))
  max(counts) - min(counts[counts > 0])
  # polymer
}

solve1(test_input)

solve1(input)

# Part 2 ----
# 40 steps.
# ok, need to rewrite
# Solve ----
# can track the number of each 2-based chunk.
# each step, we add the number of new chunks.
count_chunks <- function(polymer, instr) {
  instr$counts <- stringr::str_count(polymer, instr$V1)
  instr
}

do_step <- function(chunk_count) {
  current_chunks <- chunk_count[chunk_count$counts > 0, ]
  chunk_count$counts <- 0
  for (i in 1:nrow(current_chunks)) {
    first_new <- paste0(substr(current_chunks[i, "V1"], 1, 1), current_chunks[i, "V3"])
    chunk_count[chunk_count$V1 == first_new, "counts"] <-
      chunk_count[chunk_count$V1 == first_new, "counts"] + current_chunks[i, "counts"]
    sec_new <- paste0(current_chunks[i, "V3"], substr(current_chunks[i, "V1"], 2, 2))
    chunk_count[chunk_count$V1 == sec_new, "counts"] <-
      chunk_count[chunk_count$V1 == sec_new, "counts"] + current_chunks[i, "counts"]
  }
  chunk_count
}

library(dplyr)

solve2 <- function(input, nsteps = 40) {
  chunk_count <- count_chunks(input$template, input$instr)
  for (i in 1:nsteps) {
    chunk_count <- do_step(chunk_count)
  }
  # count all letters
  map_letters <- chunk_count %>%
    tidyr::separate_rows(V1, sep = "") %>%
    filter(V1 != "") %>%
    group_by(V1) %>%
    summarise(totals = ceiling(sum(counts) / 2))
  max(map_letters$totals) - min(map_letters$totals[map_letters$totals > 0])
}

options(scipen = 99999)

solve2(input, 40)

# super ugly code, but works, not going to bother making it nice.
