

## prop UK production by volume
read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, 
         class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  group_by(class) %>% 
  summarise(sum(prop_tot))

read.csv('data/UK_GHG_nutrient_catch.csv') %>% filter(common_name == 'Atlantic mackerel')

## top 5 species
read.csv('data/UK_GHG_nutrient_catch.csv') %>% filter(uk_name %in% c('Mackerel', 'Cod', "Salmon", 'Tuna, skipjack', 'Haddock')) %>% 
summarise(mean(mid), mean(nut_score))

## big range
load('data/nutrient_ghg_species.rds')
all %>% 
    filter(str_detect(common_name, c('tiger|mussel'))) %>% data.frame()
all %>% 
    filter(str_detect(common_name, c('carp|bass'))) %>% data.frame()
