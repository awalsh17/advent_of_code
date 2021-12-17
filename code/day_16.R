# Day 16 in R

# Input ----
input <- scan(here::here("input/day16/input.txt"), what = "character")

test <- "8A004A801A8002F478" # 16
test <- "620080001611562C8802118E34" # 12
test <- "C0015000016115A2E0802F182340" # 23
test <- "A0016C880162017C3686B18A3D4780" # 31
# Part 1 instructions ----
# what do you get if you add up the version numbers in all packets?
# Solve ----

# write a recursive function?

parse_packets <- function(bv, version) {
  # read first bit - version
  version_new <- as.integer(BMS::bin2hex(bv[1:3]))
  print(paste("new version:", version_new))
  # read type
  typeid <- as.integer(BMS::bin2hex(bv[4:6]))

  if (typeid == 4) {
    # literal value
    print("literal")
    lit_length <- get_lit_len(bv)
    return(list(
      v = version + version_new,
      len = lit_length
    ))
  }

  ltid <- bv[7]
  # n_sub <- 0
  # len_sub <- 0
  print(paste("ltid:", ltid))
  if (ltid == 1) {
    end <- 18
    n_sub <- strtoi(BMS::bin2hex(bv[8:end]), base = 16)
    len_sub <- Inf
    # print(paste("n_sub:",n_sub))
  } else if (ltid == 0) {
    end <- 22
    len_sub <- strtoi(BMS::bin2hex(bv[8:end]), base = 16) + end
    n_sub <- Inf
    # print(paste("len_sub:",len_sub))
  }
  # recursion on the sub packets
  n <- 1
  v_sum <- list()
  repeat {
    newres <- parse_packets(bv[-seq_len(end)], version)
    end <- end + newres$len
    # print(paste("end:",end))
    v_sum[[n]] <- newres$v
    n <- n + 1
    if (end == len_sub | n > n_sub) break
  }
  return(list(
    v = sum(unlist(v_sum)) + version_new,
    len = end
  ))
}


get_lit_len <- function(bv) {
  # you can get the length of a literal
  # by splitting into 4-mers and finding first with 0
  literal <- bv[7:length(bv)]
  count <- 1
  while (literal[1] == 1) {
    literal <- literal[-c(1:5)]
    count <- count + 1
  }
  # len is 5* count + 6 (headers)
  return(5 * count + 6)
}

# get_lit_len(c(1,1,0,1,0,0,  1,0,1,1,1, 1,1,1,1,0, 0,0,1,0,1, 0,0,0))
# get_lit_len(c(1,1,0,1,0,0,  0,0,1,1,1, 0,1,1,1,0, 0,0,1,0,1, 0,0,0))
# get_lit_len(c(1,1,0,1,0,0,  1,0,1,1,1, 1,1,1,1,0, 1,0,1,0,1, 0,0,0,0,0))

# make binary vector
# This is correct
bv <- BMS::hex2bin(input)
parse_packets(bv, 0)

# Part 2 ----
# What do you get if you evaluate the expression represented by your hexadecimal-encoded BITS transmission?
# Solve ----

# new function to parse codes into operations
what_op <- function(typeid) {
  # DID NOT know most these :(
  c("sum", "prod", "min", "max", "sum", ">", "<", "==")[typeid + 1]
}

bin_to_int <- function(binary) { # strtoi hint I needed - borrowed this
  powers <- rev(seq_along(binary)) - 1
  sum(binary * 2^powers)
}

# change literal function to return value
get_lit_len <- function(bv) {
  # you can get the length of a literal
  # by splitting into 4-mers and finding first with 0
  literal <- bv[7:length(bv)]
  count <- 1
  binary <- literal[c(2:5)]
  while (literal[1] == 1) {
    literal <- literal[-c(1:5)]
    count <- count + 1
    binary <- c(binary, literal[c(2:5)])
  }
  # len is 5* count + 6 (headers)
  return(list(
    len = 5 * count + 6,
    value = bin_to_int(binary), # strtoi(BMS::bin2hex(binary), base = 16),
    bin = binary
  ))
}

parse_packets <- function(bv, version) {
  # read first bit - version
  version_new <- as.integer(BMS::bin2hex(bv[1:3]))
  # print(paste("new version:", version_new))
  # read type
  typeid <- as.integer(BMS::bin2hex(bv[4:6]))

  if (typeid == 4) {
    # literal value
    # print("literal")
    lit_length <- get_lit_len(bv)
    if (is.na(lit_length$value)) {
      print("lit NA")
    }
    return(list(
      v = version + version_new,
      value = lit_length$value,
      len = lit_length$len
    ))
  } else {
    ltid <- bv[7]
    # print(paste("ltid:", ltid))
    if (ltid == 1) {
      end <- 18
      n_sub <- bin_to_int(bv[8:end]) # strtoi(BMS::bin2hex(bv[8:end]), base = 16)
      len_sub <- Inf
    } else if (ltid == 0) {
      end <- 22
      len_sub <- bin_to_int(bv[8:end]) + end # strtoi(BMS::bin2hex(bv[8:end]), base = 16) + end
      n_sub <- Inf
    }
    # recursion on the sub packets
    n <- 1
    v_sum <- list()
    v_vals <- list()
    repeat {
      newres <- parse_packets(bv[-seq_len(end)], version)
      end <- end + newres$len
      v_sum[[n]] <- newres$v
      v_vals[[n]] <- newres$value
      n <- n + 1
      if (end == len_sub | n > n_sub) break
    }
    return(list(
      v = sum(unlist(v_sum)) + version_new,
      typeid = typeid,
      value = as.numeric(do.call(
        what_op(typeid),
        v_vals
      )),
      len = end
    ))
  }
}
options(scipen = 99999)

bv <- BMS::hex2bin(input)
parse_packets(bv, 0)


# 2910423935 is low
# 2910843711 is low
