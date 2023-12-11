# Noah's Rug/ Hanukkah of Data
# https://hanukkah.bluebird.sh/5784/

library(dplyr)
# after solving day 0, we can work with the password-locked data
all_files <- list.files(path = "~/Downloads/5784/", pattern = "*.csv", full.names = TRUE)
all_data <- lapply(all_files, read.csv)
names(all_data) <- c("customers", "order_items", "orders", "products")

lapply(all_data, dim)


# for day 1, we work with the customers data ----
customers <- all_data[[1]]

customers <- mutate(customers,
                    last_name = stringr::str_replace(name, "\\sJr\\.$|\\sIII$|\\sII$|\\sIV$|\\sV$", ""),
                    last_name = stringr::str_extract(last_name, "[\\w\\.]+$"),
                    number = sapply(last_name,  phonenumber::letterToNumber),
                    correct_number = number == gsub("-", "", phone)
                    )

# This was the answer!
filter(customers, correct_number == TRUE)

# for day 2, need to merge in all the data ----
jp_customers <- customers |> 
  mutate(initials = sapply(stringr::str_extract_all(name, "[A-Z]"), paste, collapse = "")) |> 
  filter(initials == "JP")

jp_merged <- inner_join(jp_customers, all_data[[3]], by = "customerid")

# here is our answer!
all_data[[4]] |> 
  filter(desc == "Rug Cleaner") |> 
  left_join(all_data[[2]], by = "sku") |> 
  inner_join(jp_merged, by = "orderid") |> 
  filter(lubridate::year(ordered) == 2017)

# day 3? --------

# day 4? --------


