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

#the data frame in its tidied form does
#not need to be saved unless its taken
#a very long time to tidy and you want 
#to create a whole new script to process 
#the data in its tidied format

#summarise the data
hdi_summary <- hdi_no_na %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(HDI))

#add summary columns
hdi_summary <- hdi_no_na %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(HDI),
            n = length(HDI))

#add columns for standard deviation and
#standard error to the hdi_summary
hdi_summary <- hdi_no_na %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(HDI),
            n = length(HDI),
            st_dev_index = sd(HDI),
            st_error_index = st_dev_index/sqrt(n))
#there is no function for standard error
#so we need to use se = sd/sqrt(n)

#filter to get the ten countries with the
#lowest mean HDI
hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_index) < 11)

hdi_summary_low

#plot these data
hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_index)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_index - st_error_index,
                    ymax = mean_index + st_error_index)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()