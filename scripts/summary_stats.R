

## prop UK production by volume
read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, 
         class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  group_by(class) %>% 
  summarise(sum(prop_tot))

read.csv('data/UK_GHG_nutrient_catch.csv') %>% filter(common_name == 'Atlantic mackerel')