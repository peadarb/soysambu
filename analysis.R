library(tidyverse)
library(lubridate)
library(openxlsx)
library(scales)
library(googlesheets4)
library(stats)
library(stats4)
library(survey)
library(srvyr, warn.conflicts = FALSE)
library(sjPlot)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

######################################################################################################################
####### import cleaned household survey data with wealth index
######################################################################################################################

#test with api data
#data(api)
rm(list=ls())

#hhs_wealth <- readRDS("hhs_cleaned_wealth.rds")
hhs_wealth <- readRDS("hhs_cleaned.rds")
head(hhs_wealth)

#Sustain EA colour pallette
my_palette <- c("#202C39", "#d77e5e", "#3d5919", "#e6e7e2", "#381D2A","#a4b792",  "#000000","#202C39", "#d77e5e")

# Set survey design
dclus2 <- hhs_wealth %>%
  as_survey_design(c(dnum, snum), fpc = c(fpc1, fpc2))

# Explanation of the terms above
# dnum is the ID of the village
# snum is the ID of the survey 
# FPC1 is the finite population correction of the total number of potential villages to sample
# FPC2 is the finite population correction of the total number of potential households in the cluster

############################################################################################################################################
############################################################################################################################################
# attempt at looping through - sort of working
############################################################################################################################################
############################################################################################################################################

variables <- c("gender", "age", "hh_head_born")

for (variable in variables) {
  by_location <- dclus2 %>% 
    group_by_at(vars(locat, {{variable}})) %>%  
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE),
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())
    )
  
  ggplot(by_location, aes(x = locat, y = proportion, group = {{variable}}, fill = {{variable}})) +
    geom_bar(aes_string(fill = {{variable}}), stat = "identity", position = position_dodge(preserve = "single"), width = 0.8) +
    geom_errorbar(
          data = by_location,
          aes(
            ymax = ifelse(proportion_upp > 1, 1, proportion_upp),
            ymin = ifelse(proportion_low < 0, 0, proportion_low)
          ),
          position = position_dodge(preserve = "single", width = 0.95),
          width = 0.1
        ) +
      labs(title = variable, x = "Location", y = "Proportion of Households") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_sjplot() +
    theme(legend.position = c(0.95, 0.95))
  
  ggsave(filename = here::here("images", paste0(variable, ".png")))
}

############################################################################################################################################
############################################################################################################################################
# gender
############################################################################################################################################
############################################################################################################################################
by_location <- dclus2 %>% 
  group_by(locat, gender) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) 

ggplot(by_location, aes(x = locat, y = n, group = gender, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Gender of respondents", x = "Location", y = "Number of Households") +
  theme_sjplot() + 
  theme(legend.position = c(0.95, 0.95))

ggsave(filename = here::here("images", "gender_respondents.png"))

############################################################################################################################################
############################################################################################################################################
# born in area
############################################################################################################################################
############################################################################################################################################
by_location <- dclus2 %>% 
  group_by(locat, hh_head_born) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) 

ggplot(by_location, aes(x=locat, y=proportion, group = hh_head_born, fill = hh_head_born)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill=guide_legend(title=NULL)) +
  labs(title="hh_head_born", x="Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.80,0.950))

ggsave(filename = here::here("images", "hh_head_born.png"))

############################################################################################################################################
############################################################################################################################################
# ppl in household
############################################################################################################################################
############################################################################################################################################
by_location <- dclus2 %>% 
  mutate(hh_total_numb_cat = cut(hh_total_numb, c(1, 2, 5, 10, 100), include.lowest = TRUE,
                           labels = c("<2", "2-5", "5-10", ">10"))) %>%
  group_by(locat, hh_total_numb_cat) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) 

ggplot(by_location, aes(x = locat, y = n, group = hh_total_numb_cat, fill = hh_total_numb_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "hh_total_numb_respondents", x = "Location", y = "Number of Households") +
  theme_sjplot() + 
  theme(legend.position = c(0.95, 0.95))

ggsave(filename = here::here("images", "hh_total_numb_respondents.png"))

############################################################################################################################################
############################################################################################################################################
# age
############################################################################################################################################
############################################################################################################################################
by_location <- dclus2 %>% 
  mutate(age_cat = cut(age, c(18, 25, 35, 55, 100), include.lowest = TRUE,
                       labels = c("18-25", "25-35", "35-55", ">55"))) %>% 
  group_by(locat, age_cat) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) 

ggplot(by_location, aes(x=locat, y=n, group = age_cat, fill = age_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  #geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
  #              position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill=guide_legend(title=NULL)) +
  labs(title="age categories of respondents", x="Location", y = "Number of Households") +
  #scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.90,0.950))

ggsave(filename = here::here("images", "age_respondents.png"))


############################################################################################################################################
############################################################################################################################################
# children in household
############################################################################################################################################
############################################################################################################################################
by_location <- dclus2 %>% 
  mutate(numb_child_cat = cut(numb_child, c(1, 2, 4, 6, 8), include.lowest = TRUE,
                              labels = c("1-2", "3-4", "5-6", "7 or more"))) %>%
  group_by(locat, numb_child_cat) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(numb_child_cat)

ggplot(by_location, aes(x = locat, y = proportion, group = numb_child_cat, fill = numb_child_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "numb_child_cat_proportions", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.85, 0.85))

ggsave(filename = here::here("images", "numb_child_proportions.png"))

############################################################################################################################################
############################################################################################################################################
# Principal livelihood
############################################################################################################################################
############################################################################################################################################

dclus2 <- dclus2 %>% 
  mutate(   
    livelihood_activity1 = case_when(
      livelihood_activity1 == "Cultivation" ~ "Cultivation",
      livelihood_activity1 == "Kibarua or short-term employment (includes working for someone as bodaboda driver or herder)" ~ "Kibarua",
      livelihood_activity1 == "Own business" ~ "Other",
      livelihood_activity1 == "Food aid" ~ "Other",
      livelihood_activity1 == "Tourism related employment" ~ "Other",
      livelihood_activity1 == "Permanent Employment" ~ "Other",
      livelihood_activity1 == "Livestock and related products" ~ "Livestock",
      livelihood_activity1 == "I do not want to answer" ~ NA))

by_location <- dclus2 %>% 
  group_by(locat, livelihood_activity1) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(livelihood_activity1)

ggplot(by_location, aes(x = locat, y = proportion, group = livelihood_activity1, fill = livelihood_activity1)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "livelihood_activity1", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.93, 0.85))

ggsave(filename = here::here("images", "principal_livelihood.png"))

############################################################################################################################################
############################################################################################################################################
# Skip meal
############################################################################################################################################
############################################################################################################################################

dclus2 <- dclus2 %>% 
  mutate(   
    skip_meal = case_when(
      skip_meal == "Some days in every month" ~ "Every month",
      skip_meal == "Only a few days in the worst months" ~ "Few days in worst months",
      skip_meal == "Never" ~ "Never",
      skip_meal == "I do not want to answer" ~ NA))

by_location <- dclus2 %>% 
  group_by(locat, skip_meal) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(skip_meal)

ggplot(by_location, aes(x = locat, y = proportion, group = skip_meal, fill = skip_meal)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "skip_meal", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.83, 0.85))

ggsave(filename = here::here("images", "skip_meal.png"))

############################################################################################################################################
############################################################################################################################################
# housing material
############################################################################################################################################
############################################################################################################################################
by_location <- dclus2 %>% 
  group_by(locat, wall) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) #%>% 
  drop_na(wall)

ggplot(by_location, aes(x = locat, y = proportion, group = wall, fill = wall)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "wall", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.5, 0.85))

ggsave(filename = here::here("images", "wall.png"))

############################################################################################################################################
############################################################################################################################################
# influence over decision making
############################################################################################################################################
############################################################################################################################################

by_location <- dclus2 %>% 
  group_by(locat, influence_level) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
filter(!influence_level == "I do not want to answer") %>%
  mutate(influence_level = fct_relevel(influence_level, "High", "Medium", "Low", "None"))

ggplot(by_location, aes(x = locat, y = proportion, group = influence_level, fill = influence_level)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "influence_level", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.5, 0.85))

ggsave(filename = here::here("images", "influence_level.png"))

############################################################################################################################################
############################################################################################################################################
# secure feelings
############################################################################################################################################
############################################################################################################################################

by_location <- dclus2 %>% 
  group_by(locat, secure_feelings) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  filter(!secure_feelings == "I do not want to answer") %>%
  mutate(secure_feelings = fct_relevel(secure_feelings, "Very secure", "Secure", "Insecure", "Very Insecure"))

ggplot(by_location, aes(x = locat, y = proportion, group = secure_feelings, fill = secure_feelings)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "secure_feelings", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.5, 0.85))

ggsave(filename = here::here("images", "secure_feelings.png"))

############################################################################################################################################
############################################################################################################################################
# Wellbeing score
############################################################################################################################################
############################################################################################################################################
by_location <- dclus2 %>% 
  mutate(note_life_cat = cut(note_life, c(1, 3, 5, 8, 10), include.lowest = TRUE, labels = c("Very Bad", "Bad", "Good", "Very Good")
  )) %>% 
  group_by(locat, note_life_cat) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  drop_na(note_life_cat)

ggplot(by_location, aes(x = locat, y = proportion, group = note_life_cat, fill = note_life_cat)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "note_life_cat", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.5, 0.85))

ggsave(filename = here::here("images", "wellbeing_cat.png"))

############################################################################################################################################
############################################################################################################################################
# Wellbeing score
############################################################################################################################################
############################################################################################################################################

by_location <- dclus2 %>% 
  group_by(locat, change_wellbeing) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  filter(!change_wellbeing == "I do not want to answer") 

ggplot(by_location, aes(x = locat, y = proportion, group = change_wellbeing, fill = change_wellbeing)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "change_wellbeing over 5 years", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.75, 0.85))

ggsave(filename = here::here("images", "change_wellbeing_5 years.png"))

############################################################################################################################################
############################################################################################################################################
# Negative impact
############################################################################################################################################
############################################################################################################################################


# List of variables
variables <- c(
  "transmn_diseases",
  "conflict_wild",
  "compensation_wild",
  "access_restrictn",
  "priority_employ",
  "involved_projects",
  "soys_appreciate",
  "access_graze",
  "tree_cutting"
)

# Create a data frame to store the results for all variables
all_by_location <- data.frame()

# Loop through each variable
for (variable in variables) {
  by_location <- dclus2 %>% 
    group_by_at(vars({{variable}})) %>% 
    summarise(
      proportion = survey_mean(vartype = "ci", na.rm = TRUE),
      total = survey_total(vartype = "ci", na.rm = TRUE),
      n = unweighted(n())
    ) %>% 
    filter(!{{variable}} == "I do not want to answer") %>% 
    drop_na({{variable}}) %>% 
    mutate({{variable}} := fct_relevel({{variable}}, "High", "Medium", "Low", "Zero"))
  
  # Bind the results to the all_by_location data frame
  all_by_location <- bind_rows(all_by_location, by_location)
}

# Create a Likert-style bar chart function
likert_bar_chart <- function(data, variable) {
  # Filter data for the specified variable
  variable_data <- data %>%
    filter(variable == !!variable)
  
  # Set the palette
  my_palette <- c("High" = "#FF0000", "Medium" = "#FFA500", "Low" = "#FFFF00", "Zero" = "#00FF00")
  
  # Create the Likert-style bar chart
  ggplot(variable_data, aes(x = factor(n), y = proportion, group = factor(n), fill = factor(n))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
    geom_errorbar(aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp),
                      ymin = ifelse(proportion_low < 0, 0, proportion_low)),
                  position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
    scale_fill_manual(values = my_palette) +
    guides(fill = guide_legend(title = NULL)) +
    labs(title = paste("Likert-style Bar Chart -", variable),
         x = "Number of Responses",
         y = "Proportion of Households") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_sjplot() + 
    theme(legend.position = c(0.75, 0.85))
}

# Create Likert-style bar charts for each variable
all_by_location %>%
  filter(!is.na(variable)) %>%
  pull(variable) %>%
  unique() %>%
  walk(~ likert_bar_chart(all_by_location, .))

For each negative impact: % reporting impact as high or medium significance
transmn_diseases
conflict_wild
compensation_wild
access_restrictn
priority_employ
involved_projects
soys_appreciate
access_graze
tree_cutting

by_location <- dclus2 %>% 
  group_by(transmn_diseases) %>% 
  summarise(
    proportion = survey_mean(vartype = "ci", na.rm = TRUE),
    total = survey_total(vartype = "ci", na.rm = TRUE),
    n = unweighted(n())
  ) %>% 
  filter(!transmn_diseases == "I do not want to answer") %>% 
    drop_na(transmn_diseases) %>% 
  mutate(transmn_diseases = fct_relevel(transmn_diseases, "High", "Medium", "Low", "Zero"))

ggplot(by_location, aes(x = transmn_diseases, y = proportion, group = transmn_diseases, fill = transmn_diseases)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=by_location, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  scale_fill_manual(values = my_palette) +  
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "transmn_diseases", x = "Location", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position = c(0.75, 0.85))

7 For each positive impact: % reporting impact as high or medium significance
8.1
Overall impact on wellbeing: % reporting overall impact as positive, neutral
or negative
8.2
Contribution of PA/CA to wellbeing: % reporting increase, no change
or decrease
9.1 Crop damage by wildlife: % reporting damage in the last year
9.2 Livestock damaged by wildlife: % reporting damage in the last year
9.3 Problem animals: % reporting each different type of problem animal
9.4 Attribution: % believing problem animal spends all/most of its time within the PA


For cross-tabulation by wellbeing status, use the food security variable (per cent
                                                                          skipping meals), taking people responding ‘never’ as having higher wellbeing and the other
three categories (merged into one category) as lower wellbeing (or poorer). If for some reason
the food security variable does not provide reliable results, use one of the asset indicators.

