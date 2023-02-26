# First Chart
library("ggplot2")
library("dplyr")
library("plotly")
library("knitr")
library("tidyverse")


# Download and unzip one or more of the SPL datasets and load here from a file path

ten_df <- read.csv("~/Documents/INFO201/a3-spl-checkouts-amylho/2017-2023-10-Checkouts-SPL-Data 2.csv", stringsAsFactors = FALSE)
ten_df <- ten_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
ten_df$date <- as.Date(ten_df$date, format = "%Y-%m-%d")
# Drop summarise groups
options(dplyr.summarise.inform = FALSE)


# Filter for Author
author_df <- ten_df %>% filter(str_detect(Creator, "Tolkien")) %>% 
  filter(str_detect(Creator, "J.R.R."))


# Tolkien Checkouts Per month
tolkien_checkouts_month <- author_df %>% 
  filter(Title %in% c("The Hobbit", "The Silmarillion", "The Lord of the Rings: One Volume", "The Lord of the Rings, the Fellowship of the Ring","The Fellowship of the Ring: Being the First Part of The Lord of the Rings")) %>%
  group_by(Title, date) %>% 
  summarize(monthly_checkouts = sum(Checkouts))


# First Chart Representing Monthly Checkouts by J.R.R. Tolkien of Various Books

chart_1 <- ggplot(tolkien_checkouts_month) +
  geom_line(aes(x = as.Date(date), y = monthly_checkouts, color = Title)) +
  labs(title = "Monthly Checkouts of J.R.R. Tolkien Books",
       x = "Years", y = "Number of Checkouts",
       color = "Title") +
  scale_color_discrete(labels = function(x) str_wrap(x, width = 2)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 5)) 
        ggplotly(chart_1, tooltip = "text")
  
  



