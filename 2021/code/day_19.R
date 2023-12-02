# Day 19 in R
library(dplyr)

# Input ----
test <- read.table("input/day19/test.txt", sep = "\n", col.names = "data") %>%
  mutate(scanner = ifelse(grepl("scanner", data),
    stringr::str_extract(data, "\\d|\\d"),
    NA_integer_
  )) %>%
  tidyr::fill(scanner, .direction = "down") %>%
  filter(!grepl("scanner", data)) %>%
  tidyr::separate(data, into = c("x", "y", "z"), sep = ",", convert = TRUE)

input <- read.table("input/day19/input.txt", sep = "\n", col.names = "data") %>%
  mutate(scanner = ifelse(grepl("scanner", data),
    stringr::str_extract(data, "\\d\\d|\\d"),
    NA_integer_
  )) %>%
  tidyr::fill(scanner, .direction = "down") %>%
  filter(!grepl("scanner", data)) %>%
  tidyr::separate(data, into = c("x", "y", "z"), sep = ",", convert = TRUE)


# Part 1 instructions ----
# Assemble the full map of beacons. How many beacons are there?
# THE SCANNERS CAN HAVE DIFFERENT N BEACONS
# Solve ----

# get the matches between two scanners
find_matches <- function(input, s1, s2) {
  # try getting a set of distances between all beacons for each scanner?
  scanner0 <- input %>% filter(scanner == s1)
  n0 <- nrow(scanner0)
  dist0 <- as.matrix(dist(scanner0[, 1:3])) %>%
    reshape2::melt(varnames = c("id1", "id2")) %>%
    filter(id2 > id1)
  scanner1 <- input %>% filter(scanner == s2)
  n1 <- nrow(scanner1)
  dist1 <- as.matrix(dist(scanner1[, 1:3])) %>%
    reshape2::melt(varnames = c("id1", "id2")) %>%
    filter(id2 > id1)

  # find the matching distances
  m01 <- merge(dist0, dist1, by = "value")
  # pair up the "id"/row for each
  res <- m01 %>%
    tidyr::pivot_longer(id1.x:id2.x, names_to = "names_1", values_to = "first_id") %>%
    tidyr::pivot_longer(id1.y:id2.y, names_to = "names_2", values_to = "second_id") %>%
    add_count(first_id) %>%
    filter(n > 2) %>%
    group_by(first_id) %>%
    summarise(matched = which.max(tabulate(second_id))) %>%
    full_join(data.frame(first_id = seq_len(n0)), by = "first_id")

  names(res) <- c(paste0("s", s1), paste0("s", s2))
  res
}
# tests
find_matches(test, 0, 1)
find_matches(test, 4, 0)
find_matches(input, 11, 17)
full_join(find_matches(input, 15, 11), find_matches(input, 15, 17))
# to get the full list of beacons - need to remove the overlapping beacons
# for part one, dont need to even translate them?, just count

solve1 <- function(input) {
  max_scan <- max(as.integer(input$scanner))
  all_check <- list()
  for (i in 0:max_scan) {
    all_tables <- lapply(setdiff(0:max_scan, i), function(x) find_matches(input, i, x))
    all_check[[i + 1]] <- Reduce(
      function(x, y) unique(suppressMessages(full_join(x, y))),
      all_tables
    )
  }
  all_beacons <- Reduce(
    function(x, y) unique(suppressMessages(full_join(x, y))),
    all_check
  )
  # nrow(all_beacons)
  all_beacons
}

testd <- solve1(test)
inputm <- solve1(input) # 472 # SLOW


# Part 2 ----
# What is the largest Manhattan distance between any two scanners?
# Solve ----


# Getting all the rotation matrices sucks
# I could not figure this out - did manually
# should be 24: https://stackoverflow.com/questions/16452383/how-to-get-all-24-rotations-of-a-3-dimensional-array
rots <- list(
  matrix(c(0, 0, -1, 0, 1, 0, 1, 0, 0), 3, 3),
  matrix(c(0, 0, 1, 0, -1, 0, 1, 0, 0), 3, 3),
  matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3),
  matrix(c(0, -1, 0, 0, 0, -1, 1, 0, 0), 3, 3),
  matrix(c(0, 0, 1, 0, 1, 0, -1, 0, 0), 3, 3),
  matrix(c(0, 0, -1, 0, -1, 0, -1, 0, 0), 3, 3),
  matrix(c(0, -1, 0, 0, 0, 1, -1, 0, 0), 3, 3),
  matrix(c(0, 1, 0, 0, 0, -1, -1, 0, 0), 3, 3),
  matrix(c(0, 0, 1, 1, 0, 0, 0, 1, 0), 3, 3),
  matrix(c(0, 0, -1, -1, 0, 0, 0, 1, 0), 3, 3),
  matrix(c(-1, 0, 0, 0, 0, 1, 0, 1, 0), 3, 3),
  matrix(c(1, 0, 0, 0, 0, -1, 0, 1, 0), 3, 3),
  matrix(c(0, 0, -1, 1, 0, 0, 0, -1, 0), 3, 3),
  matrix(c(0, 0, 1, -1, 0, 0, 0, -1, 0), 3, 3),
  matrix(c(1, 0, 0, 0, 0, 1, 0, -1, 0), 3, 3),
  matrix(c(-1, 0, 0, 0, 0, -1, 0, -1, 0), 3, 3),
  matrix(c(0, -1, 0, 1, 0, 0, 0, 0, 1), 3, 3),
  matrix(c(0, 1, 0, -1, 0, 0, 0, 0, 1), 3, 3),
  matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3),
  matrix(c(-1, 0, 0, 0, -1, 0, 0, 0, 1), 3, 3),
  matrix(c(0, 1, 0, 1, 0, 0, 0, 0, -1), 3, 3),
  matrix(c(0, -1, 0, -1, 0, 0, 0, 0, -1), 3, 3),
  matrix(c(-1, 0, 0, 0, 1, 0, 0, 0, -1), 3, 3),
  matrix(c(1, 0, 0, 0, -1, 0, 0, 0, -1), 3, 3)
)


get_scanner_pos <- function(input_map, input, s1, s2, shift=NULL, rotate=NULL){
  label1 = paste0("s", s1)
  label2 = paste0("s", s2)
  matched_coord <- input_map %>% select(all_of(c(label1, label2)))
  matched_coord <- matched_coord[complete.cases(matched_coord),]
  mat0 <- as.matrix(input %>% filter(scanner==s1) %>%
                      select(x,y,z))[matched_coord[,1,TRUE],]
  if (!is.null(shift)) {
    mat0 <- mat0 %*% rotate + matrix(shift, nrow = nrow(mat0), 3, byrow = TRUE)
  }
  mat1 <- as.matrix(input %>% filter(scanner==s2) %>%
                      select(x,y,z))[matched_coord[,2,TRUE],]

  all_rotated <- lapply(rots, function(x) unique(mat0 - (mat1 %*% x)))
  # all_rotated
  if (any(sapply(all_rotated, nrow)==1)) {
    good_rotated <- all_rotated[sapply(all_rotated, nrow)==1][[1]]
    return(list(new_post = good_rotated,
                rotation = rots[sapply(all_rotated, nrow)==1][[1]]))
  } else {
    return(NULL)
  }

}

res1 <- get_scanner_pos(testd, test, 0,1)
test_answer <- rbind(
  c(0,0,0),
get_scanner_pos(testd, test, 0,1)$new_post,
get_scanner_pos(testd, test, 0,2)$new_post,
get_scanner_pos(testd, test, 1,3, shift = res1$new_post, rotate = res1$rotation)$new_post,
get_scanner_pos(testd, test, 0,4)$new_post
)

# calc manhattan distances
max(dist(test_answer, method = "manhattan"))


# make all beacons relative to scanner 0
completed <- c()
final_res <- list()
base <- c(0)
final_res[[0+1]] <- list(new_post = matrix(c(0,0,0),nrow=1),
                         rotation = diag(c(1,1,1)))
while(length(completed) < 38) {
# for (x in 1:6) {
  new_base <- c()
  for (i in base){
    filtered_map <- inputm[!is.na(inputm[paste0("s",i)]),]
    filtered_map <- filtered_map[, colSums(!is.na(filtered_map))> 1] %>%
      janitor::remove_empty(which = "cols")

    nextup <- setdiff(as.numeric(sub("s", "",
                                     colnames(filtered_map))),
                      c(completed, i))

    if (length(nextup) > 0) {
    for (j in nextup) {
      print(paste("j",j))
      final_res[[j+1]] <- get_scanner_pos(inputm,
                                          input, i, j,
                                          shift = final_res[[i+1]]$new_post,
                                          rotate = final_res[[i+1]]$rotation)
    }
    }
    completed <- unique(c(completed, nextup, i))
    new_base <- c(new_base, nextup)
  }
  base <- new_base
}


all_pos <- bind_rows(lapply(final_res, function(x) as.data.frame(x$new_post)))

# calc manhattan distances
max(dist((all_pos), method = "manhattan")) # 12092
