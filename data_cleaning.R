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
library(leaflet)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

######################################################################################################################
####### run data cleaning code for household survey
######################################################################################################################
#  -99 indicates don't know and these are converted to NA in continuous or "Don't know" in categorical   #######

rm(list=ls())
# # bring in sheet ----------------------------------------------------------
# gs4_deauth()
# hhs<-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1FA5UUiUtW6oHuHmQV6DGJs07dS8tWvm3XWQ_mME5MMI" , sheet = 1) %>%
#  as.data.frame()
# hhs_codes <-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1FA5UUiUtW6oHuHmQV6DGJs07dS8tWvm3XWQ_mME5MMI" , sheet = 4) %>%
#  as.data.frame() %>%
#  select(column_titles)
# 
# # transpose the column of codes to replace the header row
# new_header <- as.character(hhs_codes$column_titles)
# colnames(hhs) <- new_header
# 
# saveRDS(hhs, "hhs_raw.rds")


########################################################
# set all data types correctly before importation
########################################################
hhs <- readRDS("hhs_raw.rds")

hhs$start <- lubridate::ymd(hhs$start) # convert to date
hhs$end <- lubridate::ymd(hhs$end) # convert to date time

hhs_agreed <- hhs %>% 
  filter(agreed == "Yes") %>% 
  select(-name_respondent) %>%
  mutate(
    village = case_when(
      village == "Soysambu Area" ~ "Soysambu North",
      village == "Jolai1,2, Sleeping Warrior gate, Jolai gate" ~ "Soysambu South",
      .default = as.character(village)))%>% 
  mutate(
    locat = case_when(
      locat == "OlJorai Location" ~ "OlJorai",
      locat == "Kiptangwanyi Location" ~ "Kiptangwanyi",
      locat == "Mbaruk Location" ~ "Mbaruk",
      locat == "Soysambu" ~ "Soysambu"
    ))%>% 
  mutate(
    dnum = case_when(
      village == "Jogoo" ~ 4,
      village == "Kelelwa" ~ 24,
      village == "Muranga" ~ 11,
      village == "Kapkures" ~ 26,
      village == "Ngatta" ~ 38,
      village == "Leleshwa" ~ 20,
      village == "Oldubey" ~ 23, 
      village == "Pema" ~ 13, 
      village == "Kiambogo" ~ 15,
      village == "Central Utut" ~ 25,
      village == "Kiwanja Ndege Mkulima" ~ 14,
      village == "Echeraria" ~ 21,
      village == "Kapedo" ~ 29,
      village == "Soysambu North" ~ 41,
      village == "Mbaruk" ~ 22,
      village == "Soysambu South" ~ 43,
      village == "Mololine" ~ 17, 
      village == "Kampi Turkana" ~ 39)) %>% 
  mutate(snum = row_number()) %>% 
  mutate(fpc1 = 44) %>% 
  mutate(   
    fpc2 = case_when(
    village == "Jogoo" ~ 500,
    village == "Kelelwa" ~ 282,
    village == "Muranga" ~ 90,
    village == "Kapkures" ~ 274,
    village == "Ngatta" ~ 250,
    village == "Leleshwa" ~ 80,
    village == "Oldubey" ~ 260, 
    village == "Pema" ~ 200, 
    village == "Kiambogo" ~ 70,
    village == "Central Utut" ~ 178,
    village == "Kiwanja Ndege Mkulima" ~ 5000,
    village == "Echeraria" ~ 300,
    village == "Kapedo" ~ 189,
    village == "Soysambu North" ~ 183,
    village == "Mbaruk" ~ 500,
    village == "Soysambu South" ~ 50,
    village == "Mololine" ~ 80, 
    village == "Kampi Turkana" ~ 117)) %>% 
  mutate(stype = locat) %>% 
# mutate(
#    pw = fpc2/9, 10 or 11 depending on how many were sampled) # no need to do as can be infered. 
 
  mutate(cattle_tlu = cattle*0.71) %>% #based on Grandin 1988
  mutate(sheep_tlu = sheep*0.17) %>% #based on Grandin 1988
  mutate(goat_tlu = goat*0.17) %>% #based on Grandin 1988
  mutate(total_tlu = cattle_tlu+sheep_tlu+goat_tlu) %>% 
  mutate(crop_acre = if_else(is.na(crop_acre), 0, crop_acre)) %>% #there were no -99s here 
  mutate(tlu_per_person = total_tlu/hh_total_numb) %>% 
#  mutate(graze_cons = fct_recode(graz_hhcons, "1" = "Always", "1" = "Often","1" = "Sometimes", "1" = "Rarely", "0" = "Never")) %>%  # graze in cons area yes or no 
#  mutate(land_size_fct = fct_recode(land_size, "1" = "Less than 10 acres", "2" = "Between 10 and 20 acres", "3" = "Between 20 and 30 acres", 
#                                    "4" = "Over 30 acres", NULL = "I do not want to answer")) %>%  # graze in cons area yes or no 
#  mutate(land_size_fct = fct_inseq(land_size_fct)) %>% 
  mutate(livelihood_activity1 = as.factor(livelihood_activity1)) %>% 
  mutate(livelihood_activity2 = as.factor(livelihood_activity2)) %>% 
  mutate(livelihood_activity3 = as.factor(livelihood_activity3)) %>% 
  mutate(overall_impact = case_when(
    overall_impact == "It has increased our wellbeing" ~ "Increased our wellbeing",
    overall_impact == "It has not increased or decreased in wellbeing" ~ "Neutral",
    overall_impact == "It has reduced our wellbeing" ~ "Reduced our wellbeing",
    overall_impact == "It has slightly increased our wellbeing" ~ "Increased our wellbeing",
    overall_impact == "It has slightly reduced our wellbeing" ~ "Reduced our wellbeing"
  )) %>% 
  mutate(contribute_wellbeing = case_when(
    contribute_wellbeing == "It has increased our wellbeing" ~ "Increased our wellbeing",
    contribute_wellbeing == "It has not increased or decreased in wellbeing" ~ "Neutral",
    contribute_wellbeing == "It has reduced our wellbeing" ~ "Reduced our wellbeing",
    contribute_wellbeing == "It has slightly increased our wellbeing" ~ "Increased our wellbeing",
    contribute_wellbeing == "It has slightly reduced our wellbeing" ~ "Reduced our wellbeing"
  ))%>% 
  mutate(community_rep = case_when(
    community_rep == "Yes" ~ "Agree",
    community_rep == "No" ~ "Disagree",
    community_rep == "Dont Know" ~ "Dont Know",
    community_rep == "I do not want to answer" ~ "I do not want to answer"
  )) %>% 
  rename("elec" = "main-elec")

saveRDS(hhs_agreed, "hhs_cleaned.rds")


########################################################################################################################################
#########################################################################################################################################
# can end data cleaning here
######################################################################################################################################### 
########################################################################################################################################


#######################################################################################################################
###### constructing wealth index #####
#######################################################################################################################
#### GOT TO HERE
hhs2 <- readRDS("hhs_cleaned.rds")
# 1. exploratory data analysis of variables to potentially use in PCA ######

hhs_pca_eda <- hhs2 %>% 
  select(dnum, snum, fpc1, fpc2, stype, inter_phon, norm_phon, radio, torch,  tv, elec, gen, 
         solar_elec, piki, vehicle, table, sofa, pit_lat, tank, mpesa,skip_meal, wall,fuel,
         total_tlu, crop_acre, "_index") %>% 
  mutate(inter_phon = ifelse(inter_phon == "Yes", 1, 0)) %>% 
  mutate(norm_phon = ifelse(norm_phon == "Yes", 1, 0)) %>% 
  mutate(radio = ifelse(radio == "Yes", 1, 0)) %>% 
  mutate(torch = ifelse(torch == "Yes", 1, 0)) %>% 
  mutate(tv = ifelse(tv == "Yes", 1, 0)) %>% 
  mutate(elec = ifelse(elec == "Yes", 1, 0)) %>% 
  mutate(gen = ifelse(gen == "Yes", 1, 0)) %>% 
  mutate(solar_elec = ifelse(solar_elec == "Yes", 1, 0)) %>% 
  mutate(piki = ifelse(piki == "Yes", 1, 0)) %>% 
  mutate(vehicle = ifelse(vehicle == "Yes", 1, 0)) %>% 
  mutate(table = ifelse(table == "Yes", 1, 0)) %>% 
  mutate(sofa = ifelse(sofa == "Yes", 1, 0)) %>% 
  mutate(pit_lat = ifelse(pit_lat == "Yes", 1, 0)) %>% 
  mutate(tank = ifelse(tank == "Yes", 1, 0)) %>% 
  mutate(mpesa = ifelse(mpesa == "Yes", 1, 0)) %>% 
  mutate(gas_fuel = ifelse(fuel == "Gas", 1, 0)) %>% 
  mutate(skip_meal = ifelse(skip_meal == "Never", 1, 0)) %>% 
  mutate(brick_cement_wall = ifelse(wall == "Bricks (or mud bricks) with Cement (Durable)", 1, 0)) %>% 
  mutate(crop_acre= ifelse(crop_acre > 1, 1, 0)) %>% 
  mutate(total_tlu= ifelse(total_tlu > 5, 1, 0)) %>% 
  select(!c(fuel, wall))

# 2. Select all the subset of variables to be used to construct the PCA and omit all NA
# this one contains all the ones possible including some continuous variables
#removed tlu as this excluded too many people
hhs_pca_eda_subset_all <- hhs_pca_eda %>% 
  select(!c(dnum, snum, fpc1, fpc2, stype, total_tlu)) %>% 
  na.omit()

### for all, checked all scaling and centering and both scaling and centering are needed - means it is a correlation matrix
hhs_pca_all <- prcomp(hhs_pca_eda_subset_all, center = TRUE, scale = TRUE)
summary(hhs_pca_all)

#boxplot of all the variables with red cross at mean 
plot <- hhs_pca_eda_subset_all 

#### for binary variables, checked all scaling and centering and only centering needed - means it is a covariance matrix 
hhs_pca_binary <- prcomp(plot, center = TRUE, scale = FALSE)
summary(hhs_pca_binary)

# construct index of principal components
index_all = hhs_pca_binary$x[,1]
nlab<-c(1,2,3,4,5)

# append the index, and the wealth quintiles from all (with tlu and crop area) onto the full hhs dataframe
hhs_pca_eda_subset_all <- hhs_pca_eda_subset_all %>% 
  mutate(quintiles = as.factor(cut(index_all, breaks=5, labels=nlab))) %>% 
  mutate(wealth_pca = index_all) 

hhs_wealth <- full_join(hhs2, hhs_pca_eda_subset_all, by = "_index")

saveRDS(hhs_wealth, "hhs_cleaned_wealth.rds")
