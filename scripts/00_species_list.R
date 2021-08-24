pacman::p_load(tidyverse, cowplot, funk, janitor, install=FALSE)

## load trade data from Seafish
## filter to 2019 and drop rows without volume data
vol<-clean_names(read.csv('data/Trade_Migrated Data.csv')) %>% 
		filter(year == 2019 & kg > 0)

## split into UK export and import
export<-vol %>% filter(destination != 'United Kingdom') %>% 
			filter(origin == "United Kingdom" & flow == 'Export') %>%
			select(destination,origin_destination_continent, species, species_group, kg, value, average_price_tonne) 

import<-vol %>% filter(destination == 'United Kingdom' & flow == "Import") %>% 
			select(origin_destination,origin_destination_continent, species, species_group, kg, value, average_price_tonne) 


## get landing data ???