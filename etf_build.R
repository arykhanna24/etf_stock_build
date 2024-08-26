# Goal: To Pull yearly returns and calculate compounded returns for the list of US equity ETFs pulled from NASDAQ
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

# setting working directory
setwd("/Users/aryan/Documents/etf_analysis/datasets")


# Code --------------------------------------------------------------------

#import 
etf_data <- read.csv("/Users/aryan/Documents/etf_analysis/import/nasdaq_etf_screener_1705267390747.csv") %>% 
  clean_names() %>% 
  transmute(symbol,
            name) %>% 
  slice(1:1405)

get_yearly_returns <- function(symbol,name){
  
  print(symbol)
  
  start_date <- "1999-12-31"
  

    

      price_etf <- tq_get(symbol, from = start_date,end_date = Sys.Date(), periodicity = "daily")
  
    
  
  price_etf <- tq_get(symbol, from = start_date,end_date = Sys.Date(), periodicity = "daily")
  
  earliest_date <- price_etf %>% 
    slice(1) %>% 
    select(date) %>% 
    pull()
  
  returns_etf <- price_etf %>% 
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "yearly",
                 col_rename = glue("{symbol}"))
  
  returns_etf_modified<- returns_etf %>% 
    transmute(
      year = year(date),
       !!sym(symbol)
    ) %>% 
    column_to_rownames(var = "year") %>% 
    t() %>% 
    as.data.frame() 
  
  returns_etf_modified[1,"fund_name"] = name
  returns_etf_modified[1,"initial_date"] = earliest_date
  
  returns_etf_modified <- returns_etf_modified %>% 
    rownames_to_column("ticker") %>% 
    select(ticker,fund_name,initial_date,everything())
    
  
  return(returns_etf_modified)
  
}

final <- pmap_dfr(etf_data,get_yearly_returns)

final <- final %>% 
  select(ticker,fund_name,initial_date,"1999","2000","2001","2002","2003","2004","2005","2006",everything())

write_csv(final,"dataset_yearly_returns_only.csv")


  





