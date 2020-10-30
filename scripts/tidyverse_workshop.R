library(tidyverse)

#read the file in using read_csv()
#define the file name
filesol <- "data_raw/Human-development-index.csv"

#import the file
sol <- read_csv(filesol) %>% 
  janitor::clean_names()

#tidy the data
sol <- sol %>% 
  pivot_longer(names_to= "year",
               values_to = "HDI",
               cols = -c(hdi_rank_2018, country))
#check for values that are Na
#and remove them 
is.na(sol)
hdi_no_na <- na.omit(sol)

