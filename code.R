#Load The Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

#Load The Data
vi <- read.csv(
  "data/vi.txt",
  header = FALSE, sep = ";",
  encoding = "UTF-8")

colnames(vi) <- c("name", "page", "timestamp", "categories")

#Change timestamp into Date
vi <- vi %>%
mutate(timestamp = as.numeric(timestamp)) %>%
mutate(date = as_datetime(timestamp)) %>%
select(-timestamp)

#Create a line for each categories of change
vi <- vi %>%
mutate(
  categories = substring(
    categories,
    first = 2,
    last = nchar(categories) - 1
  )
) %>%
mutate(categories = gsub("'", "", categories)) %>%
mutate(categories = strsplit(categories, ",")) %>%
unnest(categories)

#n_edit by categories
vi_edit <- vi %>%
group_by(categories) %>%
tally() %>%
mutate(n = n / sum(n), type = "edit")


#n_editor by categories
vi_editor <- vi %>% select(categories, name) %>%  unique()
vi_editor <- vi_editor %>%
group_by(categories) %>%
tally() %>%
mutate(n = n / sum(n), type = "editor")

#n_pages by categories
vi_pages <- vi %>%
select(categories, page) %>%
unique()
vi_pages <- vi_pages %>%
group_by(categories) %>%
tally() %>%
mutate(n = n / sum(n), type = "pages")

cb_palette <- c("#000000",
"#E69F00",
"#56B4E9",
"#009E73",
"#F0E442",
"#0072B2",
"#D55E00",
"#CC79A7",
"#000000",
"#E69F00",
"#56B4E9",
"#009E73",
"#F0E442",
"#0072B2")


vi_plot <- rbind(vi_edit, rbind(vi_editor, vi_pages))

ggplot(vi_plot, aes(fill = categories, y = n, x = type)) +
  geom_bar(position = "fill", stat = "identity", colour = "Black") +
  scale_fill_manual(values = cb_palette)