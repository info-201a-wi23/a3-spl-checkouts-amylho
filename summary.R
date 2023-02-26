library("dplyr")
library("stringr")
library("ggplot2")

# Download and unzip one or more of the SPL datasets and load here from a file path

ten_df <- read.csv("~/Documents/INFO201/a3-spl-checkouts-amylho/2017-2023-10-Checkouts-SPL-Data 2.csv", stringsAsFactors = FALSE)

ten_df <- ten_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
ten_df$date <- as.Date(ten_df$date, format = "%Y-%m-%d")
options(dplyr.summarise.inform = FALSE)



# What is the sum total checkouts of "The Hobbit" every year?
hobbit_checkout <- ten_df %>%
  group_by(Title, CheckoutYear) %>%
  filter(Title %in% c("The Hobbit")) %>%
  summarize(total_hobbit = sum(Checkouts))

# Filter author name
author_df <- ten_df %>% 
  filter(str_detect(Creator, "Tolkien")) %>% 
  filter(str_detect(Creator, "J.R.R.")) 

# What are the number of monthly checkouts by Author J.R.R. Tolkien (The Hobbit)
tolkien_checkouts_month <- author_df %>% 
  filter(Title %in% c("The Hobbit", "The Silmarillion", "The Lord of the Rings: One Volume", "The Lord of the Rings, the Fellowship of the Ring","The Fellowship of the Ring: Being the First Part of The Lord of the Rings")) %>%
  group_by(Title, date) %>% 
  summarize(monthly_checkouts = sum(Checkouts))

# Calculate total number of fiction checkouts
total_fiction <- ten_df %>%
  filter(Subjects %in% c("Fiction")) %>%
  group_by(Subjects) %>%
  summarize(sum_fiction = sum(Checkouts))

# Calculate total number of nonfiction checkouts
total_nonfiction <- ten_df %>%
  filter(Subjects %in% c("Nonfiction")) %>%
  group_by(Subjects) %>%
  summarize(sum_nonfiction = sum(Checkouts))

  
# What is the average number of checkouts?
avg_checkouts <- ten_df %>%
 select(Checkouts) %>%
  summarize(Checkouts = mean(Checkouts)) 
    

# Calculate the total number of checkouts by item type over the years
top_checkouts_item <- ten_df %>%
   group_by(MaterialType, Checkouts) %>%
  summarize(sum_each_material = sum(Checkouts))



# Material type checkouts filtered
material_checkouts_filtered <- top_checkouts_item %>%
  filter(MaterialType %in% c("AUDIOBOOK", "BOOK", "EBOOK", "SOUNDDISC", "VIDEODISC")) 

# Total book checkouts
total_book_checkout <- ten_df %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts))

# Total Ebook checkouts
total_ebook_checkout <- ten_df %>%
  filter(MaterialType %in% c("EBOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts))

# What is the year with the least checkouts for books?
least_book_checkout <- ten_df %>%
    filter(MaterialType %in% c("BOOK")) %>%
    group_by(CheckoutYear) %>%
    summarize(Checkouts = sum(Checkouts)) %>%
    filter(Checkouts == min(Checkouts)) 
  

# What is the year with the most checkouts for books?
most_book_checkout <- ten_df %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts))  %>%
  filter(Checkouts == max(Checkouts))


# Total book checkouts every year from 2017-2023
total_book_each_year<- ten_df %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(sum_book_checkouts = sum(Checkouts)) 
  

# What are the total checked out books in 2017?
total_book_2017 <- ten_df %>%
  filter(CheckoutYear %in% c("2017")) %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(sum_2017_checkouts = sum(Checkouts)) 


# What are the total checked out books in 2018?   
total_book_2018 <- ten_df %>%
  filter(CheckoutYear %in% c("2018")) %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(sum_2018_checkouts = sum(Checkouts)) 

# What are the total checked out books in 2019?   
total_book_2019 <- ten_df %>%
  filter(CheckoutYear %in% c("2019")) %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(sum_2019_checkouts = sum(Checkouts)) 

# What are the total checked out books in 2020?   
total_book_2020 <- ten_df %>%
  filter(CheckoutYear %in% c("2020")) %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(sum_2020_checkouts = sum(Checkouts)) 

# What are the total checked out books in 2021?   
total_book_2021 <- ten_df %>%
  filter(CheckoutYear %in% c("2021")) %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(sum_2021_checkouts = sum(Checkouts)) 

# What are the total checked out books in 2022?   
total_book_2022 <- ten_df %>%
  filter(CheckoutYear %in% c("2022")) %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(sum_2022_checkouts = sum(Checkouts)) 

# What are the total checked out books in 2023?   
total_book_2023 <- ten_df %>%
  filter(CheckoutYear %in% c("2023")) %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(sum_2023_checkouts = sum(Checkouts)) 


