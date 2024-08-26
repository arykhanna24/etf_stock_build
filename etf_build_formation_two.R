# Goal: To add the following list of columns: cumulative return and annualized return for 5,10,15,and 20 years.
# Name: Aryan Khanna


# Restarting --------------------------------------------------------------
library(rstudioapi )
executeCommand("restartR")
rm(list=ls()) 

# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(stringr)
library(glue)
library(purrr)
library(readr)
library(lubridate)
library(broom)
library(tibble)
library(tidyquant)
library(tidyverse)
library(vroom)

# setting working directory
setwd("/Users/aryan/Documents/etf_analysis/datasets")

# Code --------------------------------------------------------------------

#Loading in build part one and making sure that all rows have 2023 data 
etf_build <- vroom("dataset_yearly_returns_only.csv") %>%
  rename_with(.cols = "1999":"2024",~str_c("return_",.x)) %>% 
  select(-return_2024) %>% 
  mutate(flag_2023 = if_else(!is.na(return_2023),1,0)) %>% 
  filter(flag_2023 == 1) %>% 
  select(-flag_2023)

# Adding inception year and flags for duration
etf_build_two <- etf_build %>% 
  mutate(inception_year = year(initial_date),
         count_year_returns = 2023-inception_year+1) %>% 
  mutate(
    flag_yrs_5 = if_else(count_year_returns > 5,1,0),      
    flag_yrs_10 = if_else(count_year_returns > 10,1,0),
    flag_yrs_15 =  if_else(count_year_returns > 15,1,0),
    flag_yrs_20 = if_else(count_year_returns > 20,1,0)  # Note we take > required number of years to avoid the inclusion of partial years.     
      
  ) # check another way to make sure no mistake here!!

# Calculating annualized goemetric mean returns for 5,10,15,20 yrs
etf_build_three <- etf_build_two %>% 
  mutate(
    return_5_yrs = if_else(flag_yrs_5 == 1,
                           ((1+return_2023)*(1+return_2022)*(1+return_2021)*(1+return_2020)*(1+return_2019))^(1/(5))-1,NA),
    return_10_yrs = if_else(flag_yrs_10 == 1,
                            ((1+return_2023)*(1+return_2022)*(1+return_2021)*(1+return_2020)*(1+return_2019)*(1+return_2018)*(1+return_2017)*(1+return_2016)*(1+return_2015)*(1+return_2014))^(1/(10))-1,NA),
return_15_yrs = if_else(flag_yrs_15 == 1,
                        ((1+return_2023)*(1+return_2022)*(1+return_2021)*(1+return_2020)*(1+return_2019)*(1+return_2018)*(1+return_2017)*(1+return_2016)*(1+return_2015)*(1+return_2014)*(1+return_2013)*(1+return_2012)*(1+return_2011)*(1+return_2010)*(1+return_2009))^(1/(15))-1,NA),
    return_20_yrs = if_else(flag_yrs_20 == 1,
                            ((1+return_2023)*(1+return_2022)*(1+return_2021)*(1+return_2020)*(1+return_2019)*(1+return_2018)*(1+return_2017)*(1+return_2016)*(1+return_2015)*(1+return_2014)*(1+return_2013)*(1+return_2012)*(1+return_2011)*(1+return_2010)*(1+return_2009)*(1+return_2008)*(1+return_2007)*(1+return_2006)*(1+return_2005)*(1+return_2004))^(1/(20))-1,NA)
  )




#export
write_csv(etf_build_three,"dataset_yearly_returns_only_two.csv")

#scratch
investigation <- etf_build_three %>% 
  arrange(desc(return_5_yrs)) %>% 
  mutate(rank_5_yrs = row_number()) %>% 
  arrange(desc(return_10_yrs)) %>% 
  mutate(rank_10_yrs = row_number()) %>% 
  arrange(desc(return_15_yrs)) %>% 
  mutate(rank_15_yrs = row_number()) %>% 
  arrange(desc(return_20_yrs)) %>% 
  mutate(rank_20_yrs = row_number()) %>% 
  mutate(sum_ranks = (4*rank_5_yrs) + (3*rank_10_yrs) + (2*rank_15_yrs) + rank_20_yrs) %>% 
  arrange(sum_ranks) %>% 
  mutate(overall_rank = row_number())
  
  
