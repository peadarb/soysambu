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
  mutate(livelihood_activity3 = as.factor(livelihood_activity3)) 

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
  select(snum, int_phon, norm_phon, radio, torch, tv, elec, gen, sola, piki, car, table, sofa, lat, mpesa, bank, fuel, 
         roof, wall, land_size, cow_now_tlu, sheep_now_tlu, goat_now_tlu, total_now_tlu, crop_acre, more_conservancies) %>% 
  mutate(int_phon = ifelse(int_phon == "Yes", 1, 0)) %>% 
  mutate(norm_phon = ifelse(norm_phon == "Yes", 1, 0)) %>% 
  mutate(radio = ifelse(radio == "Yes", 1, 0)) %>% 
  mutate(torch = ifelse(torch == "Yes", 1, 0)) %>% 
  mutate(tv = ifelse(tv == "Yes", 1, 0)) %>% 
  mutate(elec = ifelse(elec == "Yes", 1, 0)) %>% 
  mutate(gen = ifelse(gen == "Yes", 1, 0)) %>% 
  mutate(sola = ifelse(sola == "Yes", 1, 0)) %>% 
  mutate(piki = ifelse(piki == "Yes", 1, 0)) %>% 
  mutate(car = ifelse(car == "Yes", 1, 0)) %>% 
  mutate(table = ifelse(table == "Yes", 1, 0)) %>% 
  mutate(sofa = ifelse(sofa == "Yes", 1, 0)) %>% 
  mutate(lat = ifelse(lat == "Yes", 1, 0)) %>% 
  mutate(mpesa = ifelse(mpesa == "Yes", 1, 0)) %>% 
  mutate(bank = ifelse(bank == "Yes", 1, 0)) %>% 
  mutate(gas_fuel = ifelse(fuel == "Gas", 1, 0)) %>% 
  mutate(cement_brick_iron_roof = ifelse(roof == "Corrugated Iron" | roof == "Cement/Bricks", 1, 0)) %>% 
  mutate(brick_cement_wall = ifelse(wall == "Bricks (or mud bricks) with Cement (Durable)", 1, 0)) %>% 
  mutate(large_land_size = ifelse(land_size == "Over 30 acres", 1, 0)) %>% 
  mutate(more_conservancies_binary= ifelse(more_conservancies > 1, 1, 0)) %>% 
  select(!c(fuel, roof, wall, land_size))
#change outliers to NA for total TLU and crop area
#hhs_pca_eda$total_tlu[hhs_pca_eda$total_tlu > quantile(hhs_pca_eda$total_tlu, 0.99, na.rm = T)] <- NA
#hhs_pca_eda$crop_acre[hhs_pca_eda$crop_acre > quantile(hhs_pca_eda$crop_acre, 0.99, na.rm = T)] <- NA

# 2. Select all the subset of variables to be used to construct the PCA and omit all NA
# this one contains all the ones possible including some continuous variables
# could remove large land size as this has a number of NAs
hhs_pca_eda_subset_all <- hhs_pca_eda %>% 
  select(!c(more_conservancies, large_land_size, total_now_tlu, cow_now_tlu, sheep_now_tlu, goat_now_tlu, crop_acre, more_conservancies_binary)) %>% 
  na.omit()

### for all, checked all scaling and centering and both scaling and centering are needed - means it is a correlation matrix
hhs_pca_all <- prcomp(hhs_pca_eda_subset_all, center = TRUE, scale = TRUE)
summary(hhs_pca_all)


#boxplot of all the variables with red cross at mean 
plot <- hhs_pca_eda_subset_all %>% 
  select(-id)

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

hhs_wealth <- full_join(hhs2, hhs_pca_eda_subset_all, by = "id")

#write_csv(hhs, "C:/Users/peada/Documents/PhD/Research/4_data/2_analysis/hhsurvey/1_raw/hhs_cleaned.csv")
saveRDS(hhs_wealth, "hhs_cleaned_wealth.rds")
