library(tidyverse)
library(lubridate)
library(scales)
library(googlesheets4)
library(stats)
library(stats4)
library(survey)
library(srvyr, warn.conflicts = FALSE)
library(sjPlot)
library(dplyr)
library(gridExtra)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

rm(list=ls())
# bring in sheet ----------------------------------------------------------
cons_raw <-googlesheets4::read_sheet(sheet = "Version 06", "https://docs.google.com/spreadsheets/d/1HLBK3GrPWgRImRZniRT-XrdaUMbFFqKc7ewzcA2ymlg") %>% 
  as.data.frame()

########################################################
# clean data and set data types
########################################################

cons <- cons_raw %>% 
  rename(name = 1, data = 5, type = 19, size = 20) %>% # rename by index
  mutate(id = row_number()) %>% 
  select(id, name, data, type, size) %>% 
  filter(!name == "NA")
 
#saveRDS(cons, "cons_cleaned.rds")

########################################################
# visualise
########################################################

my_palette <- c("#d77e5e", "#a4b792", "#3d5919", "#e6e7e2")

cons$type <- factor(cons$type, levels = c("Private", "Group", "Community", "Proposed or no data"))

pie <- ggplot(subset(cons, !is.na(type)), aes(x = "", fill = type)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar("y") +
  scale_fill_manual(values = my_palette) +
  labs(title = "Conservancy Types") +  
  theme(
    panel.background = element_blank(),  
    panel.grid = element_blank(),        
    axis.text = element_blank(),        
    axis.title = element_blank()       
  ) +
  guides(fill = guide_legend(title = "Types of Conservancies")) +  # Set legend title
  geom_text(stat = 'count', aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

  #theme_sjplot()
print(pie)

# Create a stacked bar chart showing the total counts of all data
stacked <- ggplot(cons, aes(x = "", fill = type)) +
  geom_bar(width = 1, position = "stack") +
  scale_fill_manual(values = my_palette) +
  labs(title = "Total Conservancy Types (including proposed or no data)") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
grid.arrange(pie, stacked, ncol = 2)


# Now combined them but removed NAs
cons <- cons %>% mutate(type = ifelse(is.na(type), "Proposed or no data", type))

pie <- ggplot(filter(cons, type != "Proposed or no data"), aes(x = "", fill = type)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar("y") +
  scale_fill_manual(values = my_palette) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  geom_text(stat = 'count', aes(label = after_stat(count)), position = position_stack(vjust = 0.5))

# Create a stacked bar chart showing the total counts of all data
stacked <- ggplot(cons, aes(x = "", fill = type)) +
  geom_bar(width = 1, position = "stack") +
  scale_fill_manual(values = my_palette) +
  labs(title = "All conservancies (w/ proposed)") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  guides(fill = guide_legend(title = "Conservancy Types"))

grid.arrange(pie, stacked, ncol = 2)
#ggsave(filename = here::here("images", "cons_types.png"))

  