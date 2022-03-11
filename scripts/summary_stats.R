

## prop UK production by volume
read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, 
         class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  group_by(class) %>% 
  summarise(sum(prop_tot))

read.csv('data/UK_GHG_nutrient_catch.csv') %>% filter(common_name == 'Atlantic mackerel')
read.csv('data/UK_GHG_nutrient_catch.csv') %>% filter(common_name == 'Atlantic salmon')

all<-read.csv('data/UK_GHG_nutrient_catch.csv')
271378.8 / sum(all$tot[all$farmed_wild == 'Farmed']) * 100

## top 5 species
read.csv('data/UK_GHG_nutrient_catch.csv') %>% filter(uk_name %in% c('Mackerel', 'Cod', "Salmon", 'Tuna, skipjack', 'Haddock')) %>% 
summarise(mean(mid), mean(nut_score))

## big range
load('data/nutrient_ghg_species.rds')
all %>% 
    filter(str_detect(common_name, c('tiger|mussel'))) %>% data.frame()
all %>% 
    filter(str_detect(common_name, c('carp|bass'))) %>% data.frame()



## stock status
stock<-read.csv('data/ices/stock_data_timeries.csv') %>% 
      filter(Year >= 1990 & Year < 2020) %>% 
      mutate(F_stat = ifelse(FishingPressure > FMSY, 'Over', 'Under'),
            B_stat = ifelse(StockSize > Blim, 'Over', 'Under'),
            F_stat = ifelse(is.na(F_stat), 'Unknown', F_stat),
            B_stat = ifelse(is.na(B_stat), 'Unknown', B_stat))

stock$common_name<-nut$common_name[match(stock$SpeciesName, nut$scientific_name)]
stock %>% filter(common_name == 'Atlantic herring') %>% count(F_stat)
80 / (80 + 60 +129)

stock %>% count(F_stat)
(1923) / (1923 + 3770 + 624)
(3770) / (1923 + 3770 + 624)

n_distinct(stock$FishStock)
unique(stock$SpeciesName[stock$SpeciesName %in% nut$scientific_name])


## look at GFG categories
readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% 
    filter(str_detect(common_name, 'Mack*|erring')) %>% data.frame()

readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% 
    filter(str_detect(common_name, 'Salmon, Atlantic')) %>% data.frame()

readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% rename(scientific_name = latin_name) %>% 
    filter(! total_score %in% c('Unknown', 'Under Review', 'Default Red Rating', 'FIP Improver')) %>% 
    group_by(common_name, scientific_name, farmed_wild) %>% 
    mutate(total_score = as.numeric(total_score),
          farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
          scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                                      'Theragra chalcogramma' = 'Gadus chalcogrammus'),
          id = paste(farmed_wild, scientific_name, sep='_')) %>% 
    group_by(farmed_wild) %>% 
    ## rescale ratings between 0-1, but inverse for farmed
    mutate(total_score = ifelse(farmed_wild == 'Farmed', rescale(total_score, to = c(0,1)),rescale(total_score, to = c(1,0))))  %>% 
    summarise(n_distinct(id))

## count GFG records
gg<-readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% rename(scientific_name = latin_name) %>% 
    filter(! total_score %in% c('Unknown', 'Under Review', 'Default Red Rating', 'FIP Improver')) %>% 
    group_by(common_name, scientific_name, farmed_wild) %>% 
    mutate(total_score = as.numeric(total_score),
          farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
          scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                                      'Theragra chalcogramma' = 'Gadus chalcogrammus'),
          id = paste(farmed_wild, scientific_name, sep='_')) %>% 
    filter(id %in% nut$id) %>% 
    group_by(farmed_wild) %>% 
    ## rescale ratings between 0-1, but inverse for farmed
    mutate(total_score = ifelse(farmed_wild == 'Farmed', rescale(total_score, to = c(0,1)),rescale(total_score, to = c(1,0)))) 

gg %>% group_by(common_name) %>% tally()
gg %>% group_by(farmed_wild) %>% tally()
