

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

>>>>>>> c40ee679ed2757c93f99403e6d779ee5b027e73e
