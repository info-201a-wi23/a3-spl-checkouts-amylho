# Chart 2
library("dplyr")
library("stringr")
library("ggplot2")
library("plotly")

# Load in Data
ten_df <- read.csv("~/Documents/INFO201/a3-spl-checkouts-amylho/2017-2023-10-Checkouts-SPL-Data 3.csv", stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)
# Fiction versus Nonfiction
"Fiction" <- ten_df$Subjects[str_detect(ten_df$Subjects, "fiction")]
"Nonfiction" <-  ten_df$Subjects[str_detect(ten_df$Subjects, "nonfiction")]

# Filter the two genres
two_genres <- ten_df %>%
  filter(Subjects %in% c("Fiction", "Nonfiction")) %>%
  group_by(Subjects, CheckoutYear) %>%
  summarize(genre_checkout = sum(Checkouts))



# Fiction versus Nonfiction checkouts 
chart_2 <- ggplot(two_genres) +
  geom_line(aes(x = CheckoutYear, y = genre_checkout, color = Subjects)) +
  labs(title = "Fiction versus Nonfiction Checkouts",
       x = "Years", y = "Quantity of Checkouts",
       color = "Subjects") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 8)) 
ggplotly(chart_2, tooltip = "text")
