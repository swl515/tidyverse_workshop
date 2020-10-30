library(tidyverse)

###############################
####       HDI       ##########
###############################


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

#take the sol to the plot above using
#the pipe so all the intermediate steps are not
#necessary
sol_raw <- read_csv(filesol) %>% 
  janitor::clean_names()

hdi <- sol_raw %>% 
  pivot_longer(names_to= "year",
               values_to = "HDI",
               cols = -c(hdi_rank_2018, country)) %>% 
  na.omit(sol) %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(HDI),
            n = length(HDI),
            st_dev_index = sd(HDI),
            st_error_index = st_dev_index/sqrt(n)) %>% 
  filter(rank(mean_index) < 11) %>% 
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
hdi


###############################
####### BUOY ##################
###############################

#use the readLines() function
#to view the first few lines
#of the data frame and help us decide
#how to read it in
file <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/"
readLines(file, n = 4)

#The first line gives the 
#column name, the second line 
#gives units and the data 
#themselves begin on line 3. 
#We can read them in with:

?read_table()
buoy44025 <- read_table(file, 
                        col_names = FALSE,
                        skip = 2)

#use scan to read in appropriate
#lines and then tidy the results
#and names the columns measure_units()

#read in the variable names from the first line, 
#removing the hash
measure <- scan(file,
                nlines = 1,
                what = character()) %>% 
  str_remove("#")
#this is because the first variable name is
#YY

#read in the units from the second line, 
#removing the hash and replacing the 
#/ with _per_ as / is a special character
units <- scan(file, 
              skip = 1,
              nlines = 1, 
              what = character()) %>% 
  str_remove("#") %>% 
  str_replace("/", "_per_")
#skip =1, skips the first row to 
#do the second row.
# replace the / with _per_ 
#this stops the WSPD (etc etc) variable
#units being m/s and to m_per_s

#paste the variable name and its units
#together for the column names
names(buoy44025) <- paste(measure, units, sep = "_")
#we've added in our units and measure columns
#and separate them by a _
#so we get YY_yr (which means years in years etc)
