# Goal: To Pull yearly returns and calculate compounded returns for the list of US stocks pulled from NASDAQ
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
setwd("/Users/aryan/Documents/stock_analysis/datasets")


# Code --------------------------------------------------------------------

#import 
stock_data <- read.csv("/Users/aryan/Documents/etf_analysis/import/nasdaq_screener_1705895340252.csv") %>%
  clean_names() %>% 
  filter(country == "United States") %>% 
  transmute(symbol,
            name) 

list_loop <- stock_data %>% 
  select(symbol) %>% 
  pull()

#list_loop <- c("ACIC","ABR^D","ACIW")

final <- data.frame()

for (symbol in list_loop){
  
  print(symbol)
  
  name <- stock_data %>% 
    rename(sym = symbol) %>% 
    filter(sym == symbol) %>% 
    select(name) %>% 
    pull()
  
  start_date <- "1999-12-31"
  
  price_stock <- try(tq_get(symbol, from = start_date,end_date = Sys.Date(), periodicity = "daily"),silent = T)
  
  if("logical" %in% class(price_stock)|"try-error" %in% class(price_stock)) next
    
    earliest_date <- price_stock %>% 
      slice(1) %>% 
      select(date) %>% 
      pull()
    
    returns_stock <- price_stock %>% 
      tq_transmute(select = adjusted,
                   mutate_fun = periodReturn,
                   period = "yearly",
                   col_rename = glue("{symbol}"))
    
    returns_stock_modified<- returns_stock %>% 
      transmute(
        year = year(date),
        !!sym(symbol)
      ) %>% 
      column_to_rownames(var = "year") %>% 
      t() %>% 
      as.data.frame() 
    
    returns_stock_modified[1,"fund_name"] = name
    returns_stock_modified[1,"initial_date"] = earliest_date
    
    returns_stock_modified <- returns_stock_modified %>% 
      rownames_to_column("ticker") %>% 
      select(ticker,fund_name,initial_date,everything())
    
    print(returns_stock_modified %>% nrow())
    
    final <- final %>% 
      bind_rows(returns_stock_modified)
    
    
    #return(returns_stock_modified)
  
  
  
}
  


final <- final %>% 
  select(ticker,fund_name,initial_date,"1999","2000","2001","2002","2003","2004","2005","2006",everything())

write_csv(final,"stock_dataset_yearly_returns_only.csv")


