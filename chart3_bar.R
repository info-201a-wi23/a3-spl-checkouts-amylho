# Chart 3
library("dplyr")
library("stringr")
library("ggplot2")
library("plotly")


# Download and unzip one or more of the SPL datasets and load here from a file path

ten_df <- read.csv("~/Documents/INFO201/a3-spl-checkouts-amylho/2017-2023-10-Checkouts-SPL-Data 2.csv", stringsAsFactors = FALSE)

# Calculate the total number of checkouts by item type over the years
top_checkouts_item <- ten_df %>%
  group_by(MaterialType, Checkouts) %>%
  summarize(sum_each_material = sum(Checkouts))

# Total checkouts item types over the years 
material_checkouts_filtered <- top_checkouts_item %>%
  filter(MaterialType %in% c("AUDIOBOOK", "BOOK", "EBOOK", "SOUNDDISC", "VIDEODISC"))

# Create a horizontal bar plot to represent 
chart_3 <- ggplot(data = material_checkouts_filtered, aes(x = Checkouts, y = reorder(MaterialType,+Checkouts))) +
  geom_col(width = 0.7, aes(fill = MaterialType)) + 
  labs(title = "Checkouts by Item Type From 2017 to 2023", 
       x = "Total Checkouts", y = "Checkout Item Type", 
       fill = "Material Type") +
  scale_x_continuous(limits = c(0, 180000)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 5)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size=8),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=8))
  ggplotly(chart_3, tooltip = "text")
