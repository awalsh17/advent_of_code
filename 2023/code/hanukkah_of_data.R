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

# he was a Cancer born in the year of the Rabbit, so maybe he was able to clean it.
# spider hat - in same neighborhood as cleaner and has subway
# male - was shopping around 2017 or 2018

# so need to search for customers born in c(1987, 1999) and with a cancer sign (June 21 - July 22)

suspects <- all_data$customers |> 
  filter(lubridate::year(birthdate) %in% c(1987, 1999),
         lubridate::month(birthdate) %in% c(6, 7),
         (lubridate::month(birthdate) == 6 & lubridate::day(birthdate) > 20) |
           (lubridate::month(birthdate) == 7 & lubridate::day(birthdate) <23)) |> 
  tidyr::separate(citystatezip, into = c("city", "statezip"), sep = ",") |> 
  filter(grepl("NY", statezip))

names_of_suspects <- unique(suspects$name)
selected_names <- c("Nick Campbell", "Robert Morton", "Derrick Willis",  
                    "Adrian Brown", "Peter Wilcox", "Todd Simmons", "Sam Greene")
suspects |> left_join(all_data$orders, by = "customerid") |> 
  inner_join(all_data$order_items, by = "orderid") |> 
  inner_join(all_data$products, by = "sku") |> 
  # filter by names
  filter(name %in% selected_names) |> 
  filter(desc == "Rug Cleaner") 

# picked the person who bought rug cleaner - that was correct

# day 4 --------
# now we are looking for a woman that bought pastries before 5 am. 
# it would be a few weeks after the last person bought rug cleaner - 2017-07-22

# I dont have a list of pastries so lets filter by order date
suspects <- all_data$customers |> 
  # tidyr::separate(citystatezip, into = c("city", "statezip"), sep = ",") |> 
  # filter(grepl("NY", statezip)) |> 
  inner_join(all_data$orders, by = "customerid") |> 
  filter(lubridate::hour(shipped) <= 5)
  
suspects |> 
  inner_join(all_data$order_items, by = "orderid") |> 
  inner_join(all_data$products, by = "sku") |> 
  filter(grepl("BKY", sku)) |> View()

unique(suspects$name)

# should we pick the people that also bought rug cleaner? bike kit?
# my first guess was wrong - even thought she bought rug cleaner
# I needed someone that had ordered more than ONE pastry

# day 5 -----

# staten island, woman, 10-11 Senior Cats, had to have ordered something pre 2018

cat_products <- all_data$products |> 
  filter(grepl("Senior Cat", desc))


suspects <- all_data$customers |> 
  filter(grepl("Staten Island", citystatezip)) |> 
  inner_join(all_data$orders, by = "customerid") |> 
  inner_join(all_data$order_items, by = "orderid") |> 
  inner_join(cat_products, by = "sku")

# who had the most?
count(suspects, name, phone, sort = T)

# picked the top one and it was correct

# day 6 ------
